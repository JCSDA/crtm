!
! FitStats_netCDF_IO
!
! Module containing routines to read and write FitStats netCDF 
! format files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 10-Apr-2007
!                       paul.vandelst@noaa.gov
!

MODULE FitStats_netCDF_IO


  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds     , ONLY: Long, Double
  USE Message_Handler, ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                             Display_Message
  USE String_Utility,  ONLY: StrClean
  USE FitStats_Define, ONLY: MAX_PREDICTORS, &
                             FitStats_type, &
                             Associated_FitStats, &
                             Destroy_FitStats, &
                             Allocate_FitStats, &
                             CheckRelease_FitStats, &
                             Info_FitStats
  USE netcdf
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Inquire_FitStats_netCDF
  PUBLIC :: Write_FitStats_netCDF
  PUBLIC :: Read_FitStats_netCDF


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Module RCS Id string
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  ! Keyword set value
  INTEGER,      PARAMETER :: SET = 1
  ! Message string length
  INTEGER,      PARAMETER :: ML = 512
  ! Literal constants
  REAL(Double), PARAMETER :: ZERO = 0.0_Double
  REAL(Double), PARAMETER :: ONE  = 1.0_Double

  ! Global attribute names. Case sensitive
  CHARACTER(*), PARAMETER :: TITLE_GATTNAME            = 'title' 
  CHARACTER(*), PARAMETER :: HISTORY_GATTNAME          = 'history'
  CHARACTER(*), PARAMETER :: COMMENT_GATTNAME          = 'comment'
  CHARACTER(*), PARAMETER :: ID_TAG_GATTNAME           = 'id_tag' 
  CHARACTER(*), PARAMETER :: RELEASE_GATTNAME          = 'Release'
  CHARACTER(*), PARAMETER :: VERSION_GATTNAME          = 'Version'
  CHARACTER(*), PARAMETER :: SENSOR_ID_GATTNAME        = 'Sensor_Id'
  CHARACTER(*), PARAMETER :: WMO_SATELLITE_ID_GATTNAME = 'WMO_Satellite_Id'
  CHARACTER(*), PARAMETER :: WMO_SENSOR_ID_GATTNAME    = 'WMO_Sensor_Id'

  ! Dimension names
  CHARACTER(*), PARAMETER :: PREDICTOR_DIMNAME = 'Max_n_Predictors'
  CHARACTER(*), PARAMETER :: CHANNEL_DIMNAME   = 'n_Channels'

  ! Variable names
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_VARNAME = 'Sensor_Channel'
  CHARACTER(*), PARAMETER :: ORDER_VARNAME          = 'Order'
  CHARACTER(*), PARAMETER :: N_PREDICTORS_VARNAME   = 'n_Predictors'
  CHARACTER(*), PARAMETER :: PREDICTOR_IDX_VARNAME  = 'Predictor_Idx'
  CHARACTER(*), PARAMETER :: FREQUENCY_VARNAME      = 'Frequency'
  CHARACTER(*), PARAMETER :: FIT_RESIDUAL_VARNAME   = 'Fit_Residual'
  CHARACTER(*), PARAMETER :: TB_BIAS_VARNAME        = 'Tb_BIAS'
  CHARACTER(*), PARAMETER :: TB_SDEV_VARNAME        = 'Tb_SDEV'
  CHARACTER(*), PARAMETER :: TB_RMS_VARNAME         = 'Tb_RMS'
  CHARACTER(*), PARAMETER :: TB_MAX_VARNAME         = 'Tb_MAX'
  CHARACTER(*), PARAMETER :: TAU_BIAS_VARNAME       = 'Tau_BIAS'
  CHARACTER(*), PARAMETER :: TAU_SDEV_VARNAME       = 'Tau_SDEV'
  CHARACTER(*), PARAMETER :: TAU_RMS_VARNAME        = 'Tau_RMS'
  CHARACTER(*), PARAMETER :: TAU_MAX_VARNAME        = 'Tau_MAX'
  CHARACTER(*), PARAMETER :: TAU_MAX_BIAS_VARNAME   = 'Tau_Max_BIAS'
  CHARACTER(*), PARAMETER :: TAU_MAX_SDEV_VARNAME   = 'Tau_Max_SDEV'
  CHARACTER(*), PARAMETER :: TAU_MAX_RMS_VARNAME    = 'Tau_Max_RMS'
  CHARACTER(*), PARAMETER :: MAX_PRED_TERM_VARNAME  = 'Max_Pred_Term'

  ! Variable long name attribute.
  CHARACTER(*), PARAMETER :: LONGNAME_ATTNAME = 'long_name'

  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_LONGNAME   = 'Sensor Channel'
  CHARACTER(*), PARAMETER :: ORDER_LONGNAME            = 'Polynomial Order'
  CHARACTER(*), PARAMETER :: N_PREDICTORS_LONGNAME     = 'Number of predictors'
  CHARACTER(*), PARAMETER :: PREDICTOR_IDX_LONGNAME    = 'Predictor Index'
  CHARACTER(*), PARAMETER :: FREQUENCY_LONGNAME        = 'Frequency'
  CHARACTER(*), PARAMETER :: FIT_RESIDUAL_LONGNAME     = 'Fit Residual'
  CHARACTER(*), PARAMETER :: TB_BIAS_LONGNAME          = 'Brightness temperature residual BIAS'
  CHARACTER(*), PARAMETER :: TB_SDEV_LONGNAME          = 'Brightness temperature residual SDEV'
  CHARACTER(*), PARAMETER :: TB_RMS_LONGNAME           = 'Brightness temperature residual RMS'
  CHARACTER(*), PARAMETER :: TB_MAX_LONGNAME           = 'Brightness temperature residual MAX'
  CHARACTER(*), PARAMETER :: TAU_BIAS_LONGNAME         = 'Transmittance residual BIAS'
  CHARACTER(*), PARAMETER :: TAU_SDEV_LONGNAME         = 'Transmittance residual SDEV'
  CHARACTER(*), PARAMETER :: TAU_RMS_LONGNAME          = 'Transmittance residual RMS'
  CHARACTER(*), PARAMETER :: TAU_MAX_LONGNAME          = 'Transmittance residual MAX'
  CHARACTER(*), PARAMETER :: TAU_MAX_BIAS_LONGNAME     = 'Total transmittance residual maximum BIAS'
  CHARACTER(*), PARAMETER :: TAU_MAX_SDEV_LONGNAME     = 'Total transmittance residual maximum SDEV'
  CHARACTER(*), PARAMETER :: TAU_MAX_RMS_LONGNAME      = 'Total transmittance residual maximum RMS'
  CHARACTER(*), PARAMETER :: MAX_PRED_TERM_LONGNAME    = '(Max. Predicted term)/(Max. predictand)'


  ! Variable description attribute.
  CHARACTER(*), PARAMETER :: DESCRIPTION_ATTNAME = 'description'

  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_DESCRIPTION   = 'List of sensor channel numbers'
  CHARACTER(*), PARAMETER :: ORDER_DESCRIPTION            = 'Polynomial Order of absorption coefficient fit'
  CHARACTER(*), PARAMETER :: N_PREDICTORS_DESCRIPTION     = 'Number of predictors used in fit'
  CHARACTER(*), PARAMETER :: PREDICTOR_IDX_DESCRIPTION    = 'Index of selected predictors'
  CHARACTER(*), PARAMETER :: FREQUENCY_DESCRIPTION        = 'Sensor channel centre frequency'
  CHARACTER(*), PARAMETER :: FIT_RESIDUAL_DESCRIPTION     = 'RMS error between the logarithm of absorption '//&
  'coefficients and regression coefficients times the predictors.'
  CHARACTER(*), PARAMETER :: TB_BIAS_DESCRIPTION          = 'Brightness temperature residual BIAS'
  CHARACTER(*), PARAMETER :: TB_SDEV_DESCRIPTION          = 'Brightness temperature residual SDEV'
  CHARACTER(*), PARAMETER :: TB_RMS_DESCRIPTION           = 'Brightness temperature residual RMS'
  CHARACTER(*), PARAMETER :: TB_MAX_DESCRIPTION           = 'Brightness temperature residual MAX'
  CHARACTER(*), PARAMETER :: TAU_BIAS_DESCRIPTION         = 'Transmittance residual BIAS'
  CHARACTER(*), PARAMETER :: TAU_SDEV_DESCRIPTION         = 'Transmittance residual SDEV'
  CHARACTER(*), PARAMETER :: TAU_RMS_DESCRIPTION          = 'Transmittance residual RMS'
  CHARACTER(*), PARAMETER :: TAU_MAX_DESCRIPTION          = 'Transmittance residual MAX'
  CHARACTER(*), PARAMETER :: TAU_MAX_BIAS_DESCRIPTION     = 'Transmittance residual maximum BIAS'
  CHARACTER(*), PARAMETER :: TAU_MAX_SDEV_DESCRIPTION     = 'Transmittance residual maximum SDEV'
  CHARACTER(*), PARAMETER :: TAU_MAX_RMS_DESCRIPTION      = 'Transmittance residual maximum RMS'
  CHARACTER(*), PARAMETER :: MAX_PRED_TERM_DESCRIPTION    = 'Ratio of maximum of predicted term and maximum '//&
  'predictand log(absorption coefficient) for all layers, profiles, angles; used to watch sensitivity.'

  ! Variable units attribute.
  CHARACTER(*), PARAMETER :: UNITS_ATTNAME = 'units'
  
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_UNITS = 'N/A'
  CHARACTER(*), PARAMETER :: ORDER_UNITS          = 'N/A'
  CHARACTER(*), PARAMETER :: N_PREDICTORS_UNITS   = 'N/A'
  CHARACTER(*), PARAMETER :: PREDICTOR_IDX_UNITS  = 'N/A'
  CHARACTER(*), PARAMETER :: FREQUENCY_UNITS      = 'Inverse centimetres (cm^-1)'
  CHARACTER(*), PARAMETER :: FIT_RESIDUAL_UNITS   = 'Unitless'
  CHARACTER(*), PARAMETER :: TB_BIAS_UNITS        = 'Kelvin (K)'
  CHARACTER(*), PARAMETER :: TB_SDEV_UNITS        = 'Kelvin (K)'
  CHARACTER(*), PARAMETER :: TB_RMS_UNITS         = 'Kelvin (K)'
  CHARACTER(*), PARAMETER :: TB_MAX_UNITS         = 'Kelvin (K)'
  CHARACTER(*), PARAMETER :: TAU_BIAS_UNITS       = 'Unitless'
  CHARACTER(*), PARAMETER :: TAU_SDEV_UNITS       = 'Unitless'
  CHARACTER(*), PARAMETER :: TAU_RMS_UNITS        = 'Unitless'
  CHARACTER(*), PARAMETER :: TAU_MAX_UNITS        = 'Unitless'
  CHARACTER(*), PARAMETER :: TAU_MAX_BIAS_UNITS   = 'Unitless'
  CHARACTER(*), PARAMETER :: TAU_MAX_SDEV_UNITS   = 'Unitless'
  CHARACTER(*), PARAMETER :: TAU_MAX_RMS_UNITS    = 'Unitless'
  CHARACTER(*), PARAMETER :: MAX_PRED_TERM_UNITS  = 'Unitless'


  ! Variable _FillValue attribute.
  CHARACTER(*), PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'
  
  INTEGER(Long), PARAMETER :: SENSOR_CHANNEL_FILLVALUE = 0
  INTEGER(Long), PARAMETER :: ORDER_FILLVALUE          = 0
  INTEGER(Long), PARAMETER :: N_PREDICTORS_FILLVALUE   = 0
  INTEGER(Long), PARAMETER :: PREDICTOR_IDX_FILLVALUE  = 0
  REAL(Double) , PARAMETER :: FREQUENCY_FILLVALUE      = ZERO
  REAL(Double) , PARAMETER :: FIT_RESIDUAL_FILLVALUE   = ZERO
  REAL(Double) , PARAMETER :: TB_BIAS_FILLVALUE        = ZERO
  REAL(Double) , PARAMETER :: TB_SDEV_FILLVALUE        = ZERO
  REAL(Double) , PARAMETER :: TB_RMS_FILLVALUE         = ZERO
  REAL(Double) , PARAMETER :: TB_MAX_FILLVALUE         = ZERO
  REAL(Double) , PARAMETER :: TAU_BIAS_FILLVALUE       = ZERO
  REAL(Double) , PARAMETER :: TAU_SDEV_FILLVALUE       = ZERO
  REAL(Double) , PARAMETER :: TAU_RMS_FILLVALUE        = ZERO
  REAL(Double) , PARAMETER :: TAU_MAX_FILLVALUE        = ZERO
  REAL(Double) , PARAMETER :: TAU_MAX_BIAS_FILLVALUE   = ZERO
  REAL(Double) , PARAMETER :: TAU_MAX_SDEV_FILLVALUE   = ZERO
  REAL(Double) , PARAMETER :: TAU_MAX_RMS_FILLVALUE    = ZERO
  REAL(Double) , PARAMETER :: MAX_PRED_TERM_FILLVALUE  = ZERO

  ! Variable types
  INTEGER, PARAMETER :: SENSOR_CHANNEL_TYPE = NF90_INT
  INTEGER, PARAMETER :: ORDER_TYPE          = NF90_INT
  INTEGER, PARAMETER :: N_PREDICTORS_TYPE   = NF90_INT
  INTEGER, PARAMETER :: PREDICTOR_IDX_TYPE  = NF90_INT
  INTEGER, PARAMETER :: FREQUENCY_TYPE      = NF90_DOUBLE
  INTEGER, PARAMETER :: FIT_RESIDUAL_TYPE   = NF90_DOUBLE
  INTEGER, PARAMETER :: TB_BIAS_TYPE        = NF90_DOUBLE
  INTEGER, PARAMETER :: TB_SDEV_TYPE        = NF90_DOUBLE
  INTEGER, PARAMETER :: TB_RMS_TYPE         = NF90_DOUBLE
  INTEGER, PARAMETER :: TB_MAX_TYPE         = NF90_DOUBLE
  INTEGER, PARAMETER :: TAU_BIAS_TYPE       = NF90_DOUBLE
  INTEGER, PARAMETER :: TAU_SDEV_TYPE       = NF90_DOUBLE
  INTEGER, PARAMETER :: TAU_RMS_TYPE        = NF90_DOUBLE
  INTEGER, PARAMETER :: TAU_MAX_TYPE        = NF90_DOUBLE
  INTEGER, PARAMETER :: TAU_MAX_BIAS_TYPE   = NF90_DOUBLE
  INTEGER, PARAMETER :: TAU_MAX_SDEV_TYPE   = NF90_DOUBLE
  INTEGER, PARAMETER :: TAU_MAX_RMS_TYPE    = NF90_DOUBLE
  INTEGER, PARAMETER :: MAX_PRED_TERM_TYPE  = NF90_DOUBLE


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Inquire_FitStats_netCDF
!
! PURPOSE:
!       Function to inquire a netCDF FitStats format file to obtain the
!       dimensions and global attributes.
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_FitStats_netCDF( NC_Filename                       , &
!                                               n_Channels       =n_Channels      , &
!                                               Release          =Release         , &
!                                               Version          =Version         , &
!                                               Sensor_Id        =Sensor_Id       , &
!                                               WMO_Satellite_Id =WMO_Satellite_Id, &
!                                               WMO_Sensor_Id    =WMO_Sensor_Id   , &
!                                               ID_Tag           =ID_Tag          , &
!                                               Title            =Title           , &
!                                               History          =History         , &
!                                               Comment          =Comment         , &
!                                               RCS_Id           =RCS_Id          , &
!                                               Message_Log      =Message_Log       )
!
! INPUT ARGUMENTS:
!       NC_Filename:        Character string specifying the name of the
!                           FitStats netCDF format data file to inquire.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:        Character string specifying a filename in which any
!                           msgs will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output msgs to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Channels:         The number of spectral channels for the sensor.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:            The release number of the netCDF FitStats file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:            The version number of the netCDF FitStats file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Sensor_Id:          Character string sensor/platform identifier.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WMO_Satellite_Id:   The WMO code used to identify satellite platforms.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WMO_Sensor_Id:      The WMO code used to identify sensors.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       ID_Tag:             Character string written into the ID_TAG global
!                           attribute field of the netCDF FitStats file.
!                           Identifies the dependent profile set.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:              Character string written into the TITLE global
!                           attribute field of the netCDF FitStats file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the netCDF FitStats file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF FitStats file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the netCDF file inquiry was successful.
!                        == FAILURE an error occurred reading any of the requested
!                                   dimension or variable data.
!                        == WARNING - an error occurred reading any of the requested
!                                     global file attributes, or
!                                   - an error occurred closing the netCDF file.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Inquire_FitStats_netCDF( NC_Filename     , &  ! Input
                                    n_Channels      , &  ! Optional output
                                    Release         , &  ! Optional output
                                    Version         , &  ! Optional output
                                    Sensor_Id       , &  ! Optional output
                                    WMO_Satellite_Id, &  ! Optional output
                                    WMO_Sensor_Id   , &  ! Optional output
                                    ID_Tag          , &  ! Optional output
                                    Title           , &  ! Optional output
                                    History         , &  ! Optional output
                                    Comment         , &  ! Optional output
                                    RCS_Id          , &  ! Revision control
                                    Message_Log     ) &  ! Error messaging
                                  RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Channels  
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release         
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Sensor_Id       
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Satellite_Id
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Sensor_Id   
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: ID_Tag
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Inquire_FitStats_netCDF'
    ! Function variables
    CHARACTER(ML) :: msg
    INTEGER :: NC_FileID
    INTEGER :: NF90_STATUS
    INTEGER :: DimId, n
    
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID


    ! Open the file
    ! -------------
    NF90_Status = NF90_OPEN( NC_Filename,NF90_NOWRITE,NC_FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error opening '//TRIM(NC_Filename)//' for read access - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Get the channel dimension
    ! -------------------------
    ! Get the dimension id
    NF90_Status = NF90_INQ_DIMID( NC_FileId,CHANNEL_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//CHANNEL_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! Get the dimension value
    NF90_Status = NF90_INQUIRE_DIMENSION( NC_FileId,DimId,Len=n )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//CHANNEL_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Get the global attributes
    ! -------------------------
    Error_Status = ReadGAtts( NC_Filename                       , &
                              NC_FileID                         , &
                              Release          =Release         , &
                              Version          =Version         , &
                              Sensor_Id        =Sensor_Id       , &
                              WMO_Satellite_Id =WMO_Satellite_Id, &
                              WMO_Sensor_Id    =WMO_Sensor_Id   , &
                              ID_Tag           =ID_Tag          , &
                              Title            =Title           , &
                              History          =History         , &
                              Comment          =Comment         , &
                              Message_Log      =Message_Log       )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error reading global attributes from '//TRIM(NC_Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Close the file
    ! --------------
    NF90_Status = NF90_CLOSE( NC_FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error closing input file - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Set the dimension return value
    ! ------------------------------
    IF ( PRESENT(n_Channels) ) n_Channels = n
    
  CONTAINS
  
    SUBROUTINE Inquire_CleanUp( Close_File )
      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File ) THEN
          NF90_Status = NF90_CLOSE( NC_FileId )
          IF ( NF90_Status /= NF90_NOERR ) &
            msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
                  TRIM(NF90_STRERROR( NF90_Status ))
        END IF
      END IF
      ! Set error status and print error msg
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION Inquire_FitStats_netCDF


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Write_FitStats_netCDF
!
! PURPOSE:
!       Function to write FitStats data to a netCDF format FitStats file.
!
! CALLING SEQUENCE:
!       Error_Status = Write_FitStats_netCDF( NC_Filename            , &
!                                             FitStats               , &
!                                             Quiet      =Quiet      , &
!                                             ID_Tag     =ID_Tag     , &
!                                             Title      =Title      , &
!                                             History    =History    , &
!                                             Comment    =Comment    , &
!                                             RCS_Id     =RCS_Id     , &
!                                             Message_Log=Message_Log  )
!
! INPUT ARGUMENTS:
!       NC_Filename:  Character string specifying the name of the netCDF
!                     format FitStats data file to write data into.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       FitStats:     Structure containing the FitStats data
!                     to write to file.
!                     UNITS:      N/A
!                     TYPE:       TYPE(FitStats_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:        Set this keyword to suppress information msgs being
!                     printed to standard output (or the msg log file if
!                     the Message_Log optional argument is used.) By default,
!                     information msgs are printed.
!                     If QUIET = 0, information msgs are OUTPUT.
!                        QUIET = 1, information msgs are SUPPRESSED.
!                     UNITS:      N/A
!                     TYPE:       Integer
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       ID_Tag:       Character string written into the ID_TAG global
!                     attribute field of the netCDF FitStats file.
!                     Identifies the dependent profile set.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:        Character string written into the TITLE global
!                     attribute field of the netCDF FitStats file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:      Character string written into the HISTORY global
!                     attribute field of the netCDF FitStats file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:      Character string written into the COMMENT global
!                     attribute field of the netCDF FitStats file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:  Character string specifying a filename in which any
!                     msgs will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output msgs to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the netCDF data write was successful
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Write_FitStats_netCDF( NC_Filename , &  ! Input
                                  FitStats    , &  ! Input
                                  Quiet       , &  ! Optional input
                                  ID_Tag      , &  ! Optional input
                                  Title       , &  ! Optional input
                                  History     , &  ! Optional input
                                  Comment     , &  ! Optional input
                                  RCS_Id      , &  ! Revision control
                                  Message_Log ) &  ! Error messaging
                                RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    TYPE(FitStats_type)   , INTENT(IN)  :: FitStats
    INTEGER     , OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: ID_Tag
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title  
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_FitStats_netCDF'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
    INTEGER :: NC_FileID
    INTEGER :: NF90_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! Output informational msgs....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT(Quiet) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF

    ! Check structure association
    IF ( .NOT. Associated_FitStats( FitStats ) ) THEN
      msg = 'Some or all INPUT FitStats pointer members are NOT associated.'
      CALL Write_Cleanup(); RETURN
    END IF


    ! Create the output data file
    ! ---------------------------
    Error_Status = CreateFile( NC_Filename                                , &  ! Input
                               FitStats%n_Channels                        , &  ! Input
                               NC_FileID                                  , &  ! Output
                               Version          =FitStats%Version         , &  ! Optional input
                               Sensor_Id        =FitStats%Sensor_Id       , &  ! Optional input
                               WMO_Satellite_Id =FitStats%WMO_Satellite_Id, &  ! Optional input
                               WMO_Sensor_Id    =FitStats%WMO_Sensor_Id   , &  ! Optional input
                               ID_Tag           =ID_Tag                   , &  ! Optional input
                               Title            =Title                    , &  ! Optional input
                               History          =History                  , &  ! Optional input
                               Comment          =Comment                  , &  ! Optional input
                               Message_Log      =Message_Log                )  ! Error messaging
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error creating output file '//TRIM(NC_Filename)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the FitStats data
    ! ------------------------
    Error_Status = WriteVar( NC_Filename            , &
                             NC_FileID              , &
                             FitStats               , &
                             Message_Log=Message_Log  )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error writing FitStats variables to output file '//TRIM(NC_Filename)
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    

    ! Close the file
    ! --------------
    NF90_Status = NF90_CLOSE( NC_FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error closing input file - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Output an info msg
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_FitStats( FitStats, msg )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(NC_Filename)//'; '//TRIM(msg), &
                            INFORMATION, &
                            Message_Log=Message_Log )
    END IF

  CONTAINS
  
    SUBROUTINE Write_CleanUp( Close_File )
      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File ) THEN
          NF90_Status = NF90_CLOSE( NC_FileId )
          IF ( NF90_Status /= NF90_NOERR ) &
            msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
                  TRIM(NF90_STRERROR( NF90_Status ))
        END IF
      END IF
      ! Set error status and print error msg
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
    END SUBROUTINE Write_CleanUp

  END FUNCTION Write_FitStats_netCDF


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Read_FitStats_netCDF
!
! PURPOSE:
!       Function to read data from a netCDF format FitStats file.
!
! CALLING SEQUENCE:
!     Error_Status = Read_FitStats_netCDF( NC_Filename            , &
!                                          FitStats               , &
!                                          Quiet      =Quiet      , &
!                                          ID_Tag     =ID_Tag     , &
!                                          Title      =Title      , &
!                                          History    =History    , &
!                                          Comment    =Comment    , &
!                                          RCS_Id     =RCS_Id     , &
!                                          Message_Log=Message_Log  )
!
! INPUT ARGUMENTS:
!       NC_Filename:  Character string specifying the name of the
!                     netCDF format FitStats data file to read.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       FitStats:     Structure to contain the FitStats data
!                     read from file.
!                     UNITS:      N/A
!                     TYPE:       TYPE(FitStats_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:        Set this keyword to suppress information msgs being
!                     printed to standard output (or the msg log file if
!                     the Message_Log optional argument is used.) By default,
!                     information msgs are printed.
!                     If QUIET = 0, information msgs are OUTPUT.
!                        QUIET = 1, information msgs are SUPPRESSED.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:  Character string specifying a filename in which any
!                     msgs will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output msgs to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       ID_Tag:       Character string written into the ID_TAG global
!                     attribute field of the netCDF FitStats file.
!                     Identifies the dependent profile set.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:        Character string written into the TITLE global
!                     attribute field of the netCDF FitStats file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:      Character string written into the HISTORY global
!                     attribute field of the netCDF FitStats file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:      Character string written into the COMMENT global
!                     attribute field of the netCDF FitStats file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the netCDF data read was successful.
!                        == FAILURE an unrecoverable error occurred.
!                        == WARNING an error occurred closing the netCDF
!                                   input file after a successful read.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! COMMENTS:
!       If specified as the output data type, the INTENT on the output FitStats
!       structure argument is IN OUT rather than just OUT. This is necessary
!       because the argument may be defined on input. To prevent memory leaks,
!       the IN OUT INTENT is a must.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Read_FitStats_netCDF( NC_Filename, &  ! Input
                                 FitStats   , &  ! Output
                                 Quiet      , &  ! Optional input
                                 ID_Tag     , &  ! Optional output
                                 Title      , &  ! Optional output
                                 History    , &  ! Optional output
                                 Comment    , &  ! Optional output
                                 RCS_Id     , &  ! Revision control
                                 Message_Log) &  ! Error messaging
                               RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)     :: NC_Filename
    TYPE(FitStats_type)   , INTENT(IN OUT) :: FitStats
    INTEGER,      OPTIONAL, INTENT(IN)     :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: ID_Tag
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: Title  
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_FitStats_netCDF'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
    INTEGER :: NC_FileID
    INTEGER :: NF90_Status
    INTEGER :: n_Channels  

    
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! Output informational msgs....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF

    
    ! Allocate the structure for the netCDF read
    ! ------------------------------------------
    ! Read the dimension values
    Error_Status = Inquire_FitStats_netCDF( NC_Filename, &
                                            n_Channels=n_Channels, &
                                            Message_Log=Message_Log) 
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error obtaining FitStats dimensions from '//TRIM(NC_Filename)
      CALL Read_Cleanup(); RETURN
    END IF
    ! Allocate the structure
    Error_Status = Allocate_FitStats( n_Channels,FitStats,Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error occurred allocating FitStats structure.'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Open the netCDF file for reading
    ! --------------------------------
    NF90_Status = NF90_OPEN( NC_Filename,NF90_NOWRITE,NC_FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error opening '//TRIM(NC_Filename)//' for read access - '//&
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the global attributes
    ! --------------------------
    Error_Status = ReadGAtts( NC_Filename                               , &
                              NC_FileID                                 , &
                              Release         =FitStats%Release         , &
                              Version         =FitStats%Version         , &
                              Sensor_Id       =FitStats%Sensor_Id       , &
                              WMO_Satellite_Id=FitStats%WMO_Satellite_Id, &
                              WMO_Sensor_Id   =FitStats%WMO_Sensor_Id   , &
                              ID_Tag          =ID_Tag                   , &
                              Title           =Title                    , &
                              History         =History                  , &
                              Comment         =Comment                  , &
                              Message_Log     =Message_Log                )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error reading global attribute from '//TRIM(NC_Filename)
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF

    ! Check the release
    Error_Status = CheckRelease_FitStats( FitStats,Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'FitStats Release check failed for '//TRIM(NC_Filename)
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    

    ! Read the FitStats data
    ! ------------------------
    Error_Status = ReadVar( NC_Filename            , &
                            NC_FileID              , &
                            FitStats               , &
                            Message_Log=Message_Log  )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error reading FitStats variables from '//TRIM(NC_Filename)
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Close the file
    ! --------------
    NF90_Status = NF90_CLOSE( NC_FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error closing input file - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF


    ! Output an info msg
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_FitStats( FitStats, msg )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(NC_Filename)//'; '//TRIM(msg), &
                            INFORMATION, &
                            Message_Log=Message_Log )
    END IF

  CONTAINS
  
    SUBROUTINE Read_CleanUp( Close_File )
       LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File ) THEN
          NF90_Status = NF90_CLOSE( NC_FileId )
          IF ( NF90_Status /= NF90_NOERR ) &
            msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
                  TRIM(NF90_STRERROR( NF90_Status ))
        END IF
      END IF
      ! Destroy the structure if necessary
      IF ( Associated_FitStats( FitStats ) ) THEN
        Error_Status = Destroy_FitStats(FitStats, Message_Log=Message_Log)
        IF ( Error_Status /= SUCCESS ) &
          msg = TRIM(msg)//'; Error destroying FitStats during error cleanup.'
      END IF
      ! Set error status and print error msg
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
    END SUBROUTINE Read_CleanUp

  END FUNCTION Read_FitStats_netCDF


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!------------------------------------------------------------------------------
!
! NAME:
!       WriteGAtts
!
! PURPOSE:
!       Function to write the global attributes to a netCDF FitStats
!       data file.
!
! CALLING SEQUENCE:
!       Error_Status = WriteGAtts( NC_Filename                      , &
!                                  NC_FileID                        , &
!                                  Version         =Version         , &
!                                  Sensor_Id       =Sensor_Id       , &
!                                  WMO_Satellite_Id=WMO_Satellite_Id, &
!                                  WMO_Sensor_Id   =WMO_Sensor_Id   , &
!                                  ID_Tag          =ID_Tag          , &
!                                  Title           =Title           , &
!                                  History         =History         , &
!                                  Comment         =Comment         , &
!                                  Message_Log     =Message_Log       )
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF FitStats format data file to create.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:        NetCDF file ID number returned from the
!                         Open_FitStats_netCDF() function.
!                         UNITS:      N/A
!                         TYPE:       Integer
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Version:          The version number of the netCDF FitStats file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Sensor_Id:        Character string sensor/platform identifier.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       WMO_Satellite_Id: The WMO code used to identify satellite platforms.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       WMO_Sensor_Id:    The WMO code used to identify sensors.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       ID_Tag:           Character string written into the ID_TAG global
!                         attribute field of the netCDF FitStats file.
!                         Should contain a short tag used to identify the
!                         dependent profile set.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF FitStats file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF FitStats file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF FitStats file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:      Character string specifying a filename in which
!                         any msgs will be logged. If not specified,
!                         or if an error occurs opening the log file, the
!                         default action is to output msgs to standard
!                         output.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error status.
!                         The error codes are defined in the Message_Handler module.
!                         If == SUCCESS the global attribute write was successful.
!                            == FAILURE an error occurred.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION WriteGAtts( NC_Filename     , &  ! Input
                       NC_FileID       , &  ! Input
                       Version         , &  ! Optional input
                       Sensor_Id       , &  ! Optional input
                       WMO_Satellite_Id, &  ! Optional input
                       WMO_Sensor_Id   , &  ! Optional input
                       ID_Tag          , &  ! Optional input
                       Title           , &  ! Optional input
                       History         , &  ! Optional input
                       Comment         , &  ! Optional input
                       Message_Log     ) &  ! Error messaging
                     RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: NC_Filename
    INTEGER     ,           INTENT(IN) :: NC_FileID
    INTEGER     , OPTIONAL, INTENT(IN) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Sensor_Id       
    INTEGER     , OPTIONAL, INTENT(IN) :: WMO_Satellite_Id
    INTEGER     , OPTIONAL, INTENT(IN) :: WMO_Sensor_Id   
    CHARACTER(*), OPTIONAL, INTENT(IN) :: ID_Tag
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'WriteGAtts'
    CHARACTER(*), PARAMETER :: WRITE_MODULE_HISTORY_GATTNAME   = 'write_module_history' 
    CHARACTER(*), PARAMETER :: CREATION_DATE_AND_TIME_GATTNAME = 'creation_date_and_time' 
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: GAttName
    CHARACTER(8)  :: cdate
    CHARACTER(10) :: ctime
    CHARACTER(5)  :: czone
    INTEGER :: Ver
    INTEGER :: NF90_Status
    TYPE(FitStats_type) :: FitStats_Default

    ! Set up
    ! ------
    Error_Status = SUCCESS
    msg = ' '


    ! Mandatory global attributes
    ! ---------------------------
    ! Software ID
    GAttName = WRITE_MODULE_HISTORY_GATTNAME
    NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                MODULE_RCS_ID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF
    
    ! Creation date
    CALL DATE_AND_TIME( cdate, ctime, czone )
    GAttName = CREATION_DATE_AND_TIME_GATTNAME
    NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                cdate(1:4)//'/'//cdate(5:6)//'/'//cdate(7:8)//', '// &
                                ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//' '// &
                                czone//'UTC' )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF

    ! The Release
    GAttName = RELEASE_GATTNAME
    NF90_Status = NF90_PUT_ATT( NC_FileId, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                FitStats_Default%Release )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF


    ! Optional global attributes
    ! --------------------------
    ! The Version
    IF ( PRESENT(Version) ) THEN
      Ver = Version
    ELSE
      Ver = FitStats_Default%Version
    END IF
    GAttName = VERSION_GATTNAME
    NF90_Status = NF90_PUT_ATT( NC_FileId, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                Ver )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF

    ! The Sensor_Id
    IF ( PRESENT(Sensor_Id) ) THEN
      GAttName = SENSOR_ID_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  Sensor_Id )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The WMO_Satellite_Id
    IF ( PRESENT(WMO_Satellite_Id) ) THEN
      GAttName = WMO_SATELLITE_ID_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  WMO_Satellite_Id )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The WMO_Sensor_Id
    IF ( PRESENT(WMO_Sensor_Id) ) THEN
      GAttName = WMO_SENSOR_ID_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  WMO_Sensor_Id )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The ID_Tag
    IF ( PRESENT(ID_Tag) ) THEN
      GAttName = ID_TAG_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  ID_Tag )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The Title
    IF ( PRESENT(Title) ) THEN
      GAttName = TITLE_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  Title )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The History
    IF ( PRESENT(History) ) THEN
      GAttName = HISTORY_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  History )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The Comment
    IF ( PRESENT(Comment) ) THEN
      GAttName = COMMENT_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  Comment )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

  CONTAINS
  
    SUBROUTINE WriteGAtts_CleanUp()
      ! Close file
      NF90_Status = NF90_CLOSE( NC_FileID )
      IF ( NF90_Status /= NF90_NOERR ) &
        msg = '; Error closing input file during error cleanup - '//&
                  TRIM(NF90_STRERROR( NF90_Status ) )
      ! Set error status and print error msg
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM(GAttName)//' attribute to '//&
                            TRIM(NC_Filename)//' - '// &
                            TRIM(NF90_STRERROR( NF90_Status ) )//TRIM(msg), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE WriteGAtts_CleanUp
    
  END FUNCTION WriteGAtts


!------------------------------------------------------------------------------
!
! NAME:
!       ReadGAtts
!
! PURPOSE:
!       Function to read the global attributes from a netCDF FitStats
!       data file.
!
! CALLING SEQUENCE:
!       Error_Status = ReadGAtts( NC_Filename                      , &
!                                 NC_FileID                        , &
!                                 Release         =Release         , &
!                                 Version         =Version         , &
!                                 Sensor_Id       =Sensor_Id       , &
!                                 WMO_Satellite_Id=WMO_Satellite_Id, &
!                                 WMO_Sensor_Id   =WMO_Sensor_Id   , &
!                                 ID_Tag          =ID_Tag          , &
!                                 Title           =Title           , &
!                                 History         =History         , &
!                                 Comment         =Comment         , &
!                                 Message_Log     =Message_Log       )
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF FitStats format data file to read from.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:        NetCDF file ID number.
!                         function.
!                         UNITS:      N/A
!                         TYPE:       Integer
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:      Character string specifying a filename in which
!                         any msgs will be logged. If not specified,
!                         or if an error occurs opening the log file, the
!                         default action is to output msgs to standard
!                         output.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Release:          The release number of the netCDF FitStats file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Version:          The version number of the netCDF FitStats file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Sensor_Id:        Character string sensor/platform identifier.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WMO_Satellite_Id: The WMO code used to identify satellite platforms.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WMO_Sensor_Id:    The WMO code used to identify sensors.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       ID_Tag:           Character string written into the ID_TAG global
!                         attribute field of the netCDF FitStats file.
!                         Should contain a short tag used to identify the
!                         dependent profile set.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF FitStats file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF FitStats file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF FitStats file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error status.
!                         The error codes are defined in the Message_Handler module.
!                         If == SUCCESS the global attribute read was successful.
!                            == FAILURE an error occurred.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION ReadGAtts( NC_Filename     , &  ! Input
                      NC_FileID       , &  ! Input
                      Release         , &  ! Optional output
                      Version         , &  ! Optional output
                      Sensor_Id       , &  ! Optional output
                      WMO_Satellite_Id, &  ! Optional output
                      WMO_Sensor_Id   , &  ! Optional output
                      ID_Tag          , &  ! Optional output
                      Title           , &  ! Optional output
                      History         , &  ! Optional output
                      Comment         , &  ! Optional output
                      Message_Log     ) &  ! Error messaging
                    RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER,                INTENT(IN)  :: NC_FileID
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release         
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Sensor_Id       
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Satellite_Id
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Sensor_Id   
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: ID_Tag
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ReadGAtts'
    ! Local variables
    CHARACTER(256)  :: GAttName
    CHARACTER(5000) :: GAttString
    INTEGER :: Rel
    INTEGER :: NF90_Status
    TYPE(FitStats_type) :: FitStats_Default

    ! Set up
    ! ------
    Error_Status = SUCCESS


    ! The mandatory GAtts for checking
    ! --------------------------------
    ! The Release
    GAttName = RELEASE_GATTNAME
    NF90_Status = NF90_GET_ATT( NC_FileId, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                Rel )
    IF ( NF90_Status /= NF90_NOERR .OR. Rel /= FitStats_Default%Release) THEN
      CALL ReadGAtts_Cleanup(); RETURN
    END IF
    IF ( PRESENT(Release) ) Release = FitStats_Default%Release


    ! The optional GAtts
    ! ------------------
    ! The Version
    IF ( PRESENT(Version) ) THEN
      GAttName = VERSION_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  Version )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The Sensor_Id
    IF ( PRESENT(Sensor_Id) ) THEN
      GAttString = ' '; Sensor_Id = ' '
      GAttName = SENSOR_ID_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL StrClean( GAttString )
      Sensor_Id = GAttString(1:MIN( LEN(Sensor_Id), LEN_TRIM(GAttString) ))
    END IF

    ! The WMO_Satellite_Id
    IF ( PRESENT(WMO_Satellite_Id) ) THEN
      GAttName = WMO_SATELLITE_ID_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  WMO_Satellite_Id )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The WMO_Sensor_Id
    IF ( PRESENT(WMO_Sensor_Id) ) THEN
      GAttName = WMO_SENSOR_ID_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  WMO_Sensor_Id )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The ID_Tag
    IF ( PRESENT(ID_Tag) ) THEN
      GAttString = ' '; ID_Tag = ' '
      GAttName = ID_TAG_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL StrClean( GAttString )
      ID_Tag = GAttString(1:MIN( LEN(ID_Tag), LEN_TRIM(GAttString) ))
    END IF

    ! The Title
    IF ( PRESENT(Title) ) THEN
      GAttString = ' '; Title = ' '
      GAttName = TITLE_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL StrClean( GAttString )
      Title = GAttString(1:MIN( LEN(Title), LEN_TRIM(GAttString) ))
    END IF

    ! The History
    IF ( PRESENT(History) ) THEN
      GAttString = ' '; History = ' '
      GAttName = HISTORY_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL StrClean( GAttString )
      History = GAttString(1:MIN( LEN(History), LEN_TRIM(GAttString) ))
    END IF

    ! The Comment
    IF ( PRESENT(Comment) ) THEN
      GAttString = ' '; Comment = ' '
      GAttName = COMMENT_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL StrClean( GAttString )
      Comment = GAttString(1:MIN( LEN(Comment), LEN_TRIM(GAttString) ))
    END IF

  CONTAINS
  
    SUBROUTINE ReadGAtts_CleanUp()
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//TRIM(GAttName)//&
                            ' attribute from '//TRIM(NC_Filename)//' - '// &
                            TRIM(NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE ReadGAtts_CleanUp

  END FUNCTION ReadGAtts


!------------------------------------------------------------------------------
!
! NAME:
!       DefineVar
!
! PURPOSE:
!       Function to define the FitStats variables in an output
!       netCDF file.
!
! CALLING SEQUENCE:
!       Error_Status = DefineVar( NC_Filename            , &
!                                 NC_FileID              , &
!                                 n_Predictors_DimID     , &
!                                 n_Channels_DimID       , &
!                                 RCS_Id     =RCS_Id     , &
!                                 Message_Log=Message_Log  )
!
! INPUT ARGUMENTS
!       NC_Filename:        Character string specifying the name of the
!                           already created netCDF FitStats format file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:          NetCDF file ID number of the file in which
!                           the variables are to be defned.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Predictors_DimID: NetCDF dimension ID of the number of predictors
!                           (n_Predictors).
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Channels_DimID:   NetCDF dimension ID of the number of sensor
!                           channels (n_Channels).
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS
!       Message_Log:        Character string specifying a filename in which any
!                           msgs will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output msgs to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!------------------------------------------------------------------------------

  FUNCTION DefineVar( NC_Filename       , &  ! Input
                      NC_FileID         , &  ! Input
                      n_Predictors_DimID, &  ! Input
                      n_Channels_DimID  , &  ! Input
                      RCS_Id            , &  ! Revision control
                      Message_Log       ) &  ! Error messaging
                    RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER     ,           INTENT(IN)  :: NC_FileID
    INTEGER     ,           INTENT(IN)  :: n_Predictors_DimID
    INTEGER     ,           INTENT(IN)  :: n_Channels_DimID  
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'DefineVar'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: NF90_Status(4)
    INTEGER :: varID
                               
    ! Set up
    ! ------
    Error_Status = SUCCESS                                      
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID


    ! Begin all the variable definitions
    ! ----------------------------------
    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,SENSOR_CHANNEL_VARNAME,SENSOR_CHANNEL_TYPE, &
                                   dimIDs=(/n_Channels_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//SENSOR_CHANNEL_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,SENSOR_CHANNEL_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,SENSOR_CHANNEL_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,SENSOR_CHANNEL_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,SENSOR_CHANNEL_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//SENSOR_CHANNEL_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,ORDER_VARNAME,ORDER_TYPE, &
                                   dimIDs=(/n_Channels_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//ORDER_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,ORDER_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,ORDER_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,ORDER_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,ORDER_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//ORDER_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,N_PREDICTORS_VARNAME,N_PREDICTORS_TYPE, &
                                   dimIDs=(/n_Channels_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//N_PREDICTORS_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,N_PREDICTORS_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,N_PREDICTORS_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,N_PREDICTORS_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,N_PREDICTORS_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//N_PREDICTORS_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,PREDICTOR_IDX_VARNAME,PREDICTOR_IDX_TYPE, &
                                   dimIDs=(/n_Predictors_DimID,n_Channels_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//PREDICTOR_IDX_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,PREDICTOR_IDX_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,PREDICTOR_IDX_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,PREDICTOR_IDX_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,PREDICTOR_IDX_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//PREDICTOR_IDX_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,FREQUENCY_VARNAME,FREQUENCY_TYPE, &
                                   dimIDs=(/n_Channels_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//FREQUENCY_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,FREQUENCY_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,FREQUENCY_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,FREQUENCY_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,FREQUENCY_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//FREQUENCY_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,FIT_RESIDUAL_VARNAME,FIT_RESIDUAL_TYPE, &
                                   dimIDs=(/n_Channels_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//FIT_RESIDUAL_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,FIT_RESIDUAL_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,FIT_RESIDUAL_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,FIT_RESIDUAL_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,FIT_RESIDUAL_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//FIT_RESIDUAL_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,TB_BIAS_VARNAME,TB_BIAS_TYPE, &
                                   dimIDs=(/n_Channels_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//TB_BIAS_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,TB_BIAS_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,TB_BIAS_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,TB_BIAS_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,TB_BIAS_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//TB_BIAS_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,TB_SDEV_VARNAME,TB_SDEV_TYPE, &
                                   dimIDs=(/n_Channels_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//TB_SDEV_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,TB_SDEV_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,TB_SDEV_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,TB_SDEV_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,TB_SDEV_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//TB_SDEV_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,TB_RMS_VARNAME,TB_RMS_TYPE, &
                                   dimIDs=(/n_Channels_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//TB_RMS_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,TB_RMS_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,TB_RMS_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,TB_RMS_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,TB_RMS_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//TB_RMS_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,TB_MAX_VARNAME,TB_MAX_TYPE, &
                                   dimIDs=(/n_Channels_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//TB_MAX_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,TB_MAX_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,TB_MAX_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,TB_MAX_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,TB_MAX_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//TB_MAX_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,TAU_BIAS_VARNAME,TAU_BIAS_TYPE, &
                                   dimIDs=(/n_Channels_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//TAU_BIAS_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,TAU_BIAS_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,TAU_BIAS_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,TAU_BIAS_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,TAU_BIAS_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//TAU_BIAS_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,TAU_SDEV_VARNAME,TAU_SDEV_TYPE, &
                                   dimIDs=(/n_Channels_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//TAU_SDEV_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,TAU_SDEV_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,TAU_SDEV_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,TAU_SDEV_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,TAU_SDEV_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//TAU_SDEV_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,TAU_RMS_VARNAME,TAU_RMS_TYPE, &
                                   dimIDs=(/n_Channels_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//TAU_RMS_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,TAU_RMS_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,TAU_RMS_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,TAU_RMS_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,TAU_RMS_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//TAU_RMS_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,TAU_MAX_VARNAME,TAU_MAX_TYPE, &
                                   dimIDs=(/n_Channels_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//TAU_MAX_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,TAU_MAX_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,TAU_MAX_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,TAU_MAX_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,TAU_MAX_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//TAU_MAX_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,TAU_MAX_BIAS_VARNAME,TAU_MAX_BIAS_TYPE, &
                                   dimIDs=(/n_Channels_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//TAU_MAX_BIAS_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,TAU_MAX_BIAS_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,TAU_MAX_BIAS_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,TAU_MAX_BIAS_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,TAU_MAX_BIAS_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//TAU_MAX_BIAS_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,TAU_MAX_SDEV_VARNAME,TAU_MAX_SDEV_TYPE, &
                                   dimIDs=(/n_Channels_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//TAU_MAX_SDEV_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,TAU_MAX_SDEV_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,TAU_MAX_SDEV_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,TAU_MAX_SDEV_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,TAU_MAX_SDEV_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//TAU_MAX_SDEV_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,TAU_MAX_RMS_VARNAME,TAU_MAX_RMS_TYPE, &
                                   dimIDs=(/n_Channels_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//TAU_MAX_RMS_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,TAU_MAX_RMS_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,TAU_MAX_RMS_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,TAU_MAX_RMS_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,TAU_MAX_RMS_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//TAU_MAX_RMS_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,MAX_PRED_TERM_VARNAME,MAX_PRED_TERM_TYPE, &
                                   dimIDs=(/n_Channels_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//MAX_PRED_TERM_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,MAX_PRED_TERM_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,MAX_PRED_TERM_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,MAX_PRED_TERM_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,MAX_PRED_TERM_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//MAX_PRED_TERM_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE DefineVar_CleanUp()
      ! Close file
      NF90_Status(1) = NF90_CLOSE( NC_FileID )
      IF ( NF90_Status(1) /= NF90_NOERR ) &
        msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
              TRIM(NF90_STRERROR( NF90_Status(1) ) )
      ! Set error status and print error msg
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
    END SUBROUTINE DefineVar_CleanUp

  END FUNCTION DefineVar


!------------------------------------------------------------------------------
!
! NAME:
!       WriteVar
!
! PURPOSE:
!       Function to write the FitStats variables in an output
!       netCDF file in which they have been defined.
!
! CALLING SEQUENCE:
!       Error_Status = WriteVar( NC_Filename            , &
!                                NC_FileID              , &
!                                FitStats               , &
!                                RCS_Id     =RCS_Id     , &
!                                Message_Log=Message_Log  )
!
! INPUT ARGUMENTS
!       NC_Filename:        Character string specifying the name of the
!                           already created netCDF FitStats format file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:          NetCDF file ID number of the file in which
!                           the variables are to be written.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       FitStats:           Structure containing the data to write to file.
!                           UNITS:      N/A
!                           TYPE:       TYPE(FitStats_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS
!       Message_Log:        Character string specifying a filename in which any
!                           msgs will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output msgs to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! SIDE EFFECTS:
!       If an error occurs, the netCDF file is closed.
!
!------------------------------------------------------------------------------

  FUNCTION WriteVar( NC_Filename, &  ! Input
                     NC_FileID  , &  ! Input
                     FitStats   , &  ! Input
                     RCS_Id     , &  ! Revision control
                     Message_Log) &  ! Error messaging
                   RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)  :: NC_Filename
    INTEGER               , INTENT(IN)  :: NC_FileID
    TYPE(FitStats_type)   , INTENT(IN)  :: FitStats
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'WriteVar'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: NF90_Status
    INTEGER :: VarId
                               
    ! Set up
    ! ------
    Error_Status = SUCCESS                                      
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID


    ! Write the variable data
    ! -----------------------
    ! The Sensor_Channel
    NF90_Status = NF90_INQ_VARID( NC_FileId,SENSOR_CHANNEL_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//SENSOR_CHANNEL_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,FitStats%Sensor_Channel )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//SENSOR_CHANNEL_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Order
    NF90_Status = NF90_INQ_VARID( NC_FileId,ORDER_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//ORDER_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,FitStats%Order )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//ORDER_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The n_Predictors
    NF90_Status = NF90_INQ_VARID( NC_FileId,N_PREDICTORS_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//N_PREDICTORS_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,FitStats%n_Predictors )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//N_PREDICTORS_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Predictor_Idx
    NF90_Status = NF90_INQ_VARID( NC_FileId,PREDICTOR_IDX_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//PREDICTOR_IDX_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,FitStats%Predictor_Idx )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//PREDICTOR_IDX_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Frequency
    NF90_Status = NF90_INQ_VARID( NC_FileId,FREQUENCY_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//FREQUENCY_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,FitStats%Frequency )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//FREQUENCY_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Fit_Residual
    NF90_Status = NF90_INQ_VARID( NC_FileId,FIT_RESIDUAL_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//FIT_RESIDUAL_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,FitStats%Fit_Residual )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//FIT_RESIDUAL_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Tb_BIAS
    NF90_Status = NF90_INQ_VARID( NC_FileId,TB_BIAS_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TB_BIAS_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,FitStats%Tb_BIAS )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//TB_BIAS_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Tb_SDEV
    NF90_Status = NF90_INQ_VARID( NC_FileId,TB_SDEV_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TB_SDEV_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,FitStats%Tb_SDEV )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//TB_SDEV_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Tb_RMS
    NF90_Status = NF90_INQ_VARID( NC_FileId,TB_RMS_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TB_RMS_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,FitStats%Tb_RMS )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//TB_RMS_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Tb_MAX
    NF90_Status = NF90_INQ_VARID( NC_FileId,TB_MAX_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TB_MAX_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,FitStats%Tb_MAX )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//TB_MAX_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Tau_BIAS
    NF90_Status = NF90_INQ_VARID( NC_FileId,TAU_BIAS_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TAU_BIAS_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,FitStats%Tau_BIAS )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//TAU_BIAS_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Tau_SDEV
    NF90_Status = NF90_INQ_VARID( NC_FileId,TAU_SDEV_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TAU_SDEV_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,FitStats%Tau_SDEV )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//TAU_SDEV_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Tau_RMS
    NF90_Status = NF90_INQ_VARID( NC_FileId,TAU_RMS_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TAU_RMS_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,FitStats%Tau_RMS )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//TAU_RMS_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Tau_MAX
    NF90_Status = NF90_INQ_VARID( NC_FileId,TAU_MAX_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TAU_MAX_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,FitStats%Tau_MAX )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//TAU_MAX_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Tau_Max_BIAS
    NF90_Status = NF90_INQ_VARID( NC_FileId,TAU_MAX_BIAS_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TAU_MAX_BIAS_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,FitStats%Tau_Max_BIAS )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//TAU_MAX_BIAS_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Tau_Max_SDEV
    NF90_Status = NF90_INQ_VARID( NC_FileId,TAU_MAX_SDEV_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TAU_MAX_SDEV_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,FitStats%Tau_Max_SDEV )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//TAU_MAX_SDEV_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Tau_Max_RMS
    NF90_Status = NF90_INQ_VARID( NC_FileId,TAU_MAX_RMS_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TAU_MAX_RMS_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,FitStats%Tau_Max_RMS )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//TAU_MAX_RMS_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Max_Pred_Term
    NF90_Status = NF90_INQ_VARID( NC_FileId,MAX_PRED_TERM_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//MAX_PRED_TERM_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,FitStats%Max_Pred_Term )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//MAX_PRED_TERM_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    
  CONTAINS
  
    SUBROUTINE WriteVar_CleanUp()
      ! Close file
      NF90_Status = NF90_CLOSE( NC_FileID )
      IF ( NF90_Status /= NF90_NOERR ) &
        msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
              TRIM(NF90_STRERROR( NF90_Status ) )
      ! Set error status and print error msg
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
    END SUBROUTINE WriteVar_CleanUp

  END FUNCTION WriteVar


!------------------------------------------------------------------------------
!
! NAME:
!       ReadVar
!
! PURPOSE:
!       Function to read the FitStats variables from any input
!       netCDF file in which they have been defined.
!
! CALLING SEQUENCE:
!       Error_Status = ReadVar( NC_Filename            , &
!                               NC_FileID              , &
!                               FitStats               , &
!                               RCS_Id     =RCS_Id     , &
!                               Message_Log=Message_Log  )
!
! INPUT ARGUMENTS
!       NC_Filename:        Character string specifying the name of the
!                           already created netCDF FitStats format file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:          NetCDF file ID number of the file in which
!                           the variables are to be written.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       FitStats:           Structure containing the data that was read
!                           from file.
!                           UNITS:      N/A
!                           TYPE:       TYPE(FitStats_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS
!       Message_Log:        Character string specifying a filename in which any
!                           msgs will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output msgs to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! SIDE EFFECTS:
!       If an error occurs, the netCDF file is closed.
!
! COMMENTS:
!       The INTENT on the output FitStats argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION ReadVar( NC_Filename, &  ! Input
                    NC_FileID  , &  ! Input
                    FitStats   , &  ! Output
                    RCS_Id     , &  ! Revision control
                    Message_Log) &  ! Error messaging
                  RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)     :: NC_Filename
    INTEGER               , INTENT(IN)     :: NC_FileID
    TYPE(FitStats_type)   , INTENT(IN OUT) :: FitStats
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ReadVar'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: NF90_Status
    INTEGER :: VarId
                               
    ! Set up
    ! ------
    Error_Status = SUCCESS                                      
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID


    ! Read the variable data
    ! ----------------------
    ! The Sensor_Channel
    NF90_Status = NF90_INQ_VARID( NC_FileId,SENSOR_CHANNEL_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//SENSOR_CHANNEL_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,FitStats%Sensor_Channel )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//SENSOR_CHANNEL_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Order
    NF90_Status = NF90_INQ_VARID( NC_FileId,ORDER_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//ORDER_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,FitStats%Order )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//ORDER_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The n_Predictors
    NF90_Status = NF90_INQ_VARID( NC_FileId,N_PREDICTORS_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//N_PREDICTORS_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,FitStats%n_Predictors )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//N_PREDICTORS_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Predictor_Idx
    NF90_Status = NF90_INQ_VARID( NC_FileId,PREDICTOR_IDX_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//PREDICTOR_IDX_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,FitStats%Predictor_Idx )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//PREDICTOR_IDX_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Frequency
    NF90_Status = NF90_INQ_VARID( NC_FileId,FREQUENCY_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//FREQUENCY_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,FitStats%Frequency )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//FREQUENCY_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Fit_Residual
    NF90_Status = NF90_INQ_VARID( NC_FileId,FIT_RESIDUAL_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//FIT_RESIDUAL_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,FitStats%Fit_Residual )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//FIT_RESIDUAL_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Tb_BIAS
    NF90_Status = NF90_INQ_VARID( NC_FileId,TB_BIAS_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TB_BIAS_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,FitStats%Tb_BIAS )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//TB_BIAS_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Tb_SDEV
    NF90_Status = NF90_INQ_VARID( NC_FileId,TB_SDEV_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TB_SDEV_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,FitStats%Tb_SDEV )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//TB_SDEV_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Tb_RMS
    NF90_Status = NF90_INQ_VARID( NC_FileId,TB_RMS_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TB_RMS_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,FitStats%Tb_RMS )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//TB_RMS_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Tb_MAX
    NF90_Status = NF90_INQ_VARID( NC_FileId,TB_MAX_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TB_MAX_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,FitStats%Tb_MAX )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//TB_MAX_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Tau_BIAS
    NF90_Status = NF90_INQ_VARID( NC_FileId,TAU_BIAS_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TAU_BIAS_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,FitStats%Tau_BIAS )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//TAU_BIAS_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Tau_SDEV
    NF90_Status = NF90_INQ_VARID( NC_FileId,TAU_SDEV_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TAU_SDEV_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,FitStats%Tau_SDEV )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//TAU_SDEV_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Tau_RMS
    NF90_Status = NF90_INQ_VARID( NC_FileId,TAU_RMS_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TAU_RMS_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,FitStats%Tau_RMS )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//TAU_RMS_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Tau_MAX
    NF90_Status = NF90_INQ_VARID( NC_FileId,TAU_MAX_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TAU_MAX_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,FitStats%Tau_MAX )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//TAU_MAX_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Tau_Max_BIAS
    NF90_Status = NF90_INQ_VARID( NC_FileId,TAU_MAX_BIAS_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TAU_MAX_BIAS_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,FitStats%Tau_Max_BIAS )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//TAU_MAX_BIAS_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Tau_Max_SDEV
    NF90_Status = NF90_INQ_VARID( NC_FileId,TAU_MAX_SDEV_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TAU_MAX_SDEV_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,FitStats%Tau_Max_SDEV )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//TAU_MAX_SDEV_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Tau_Max_RMS
    NF90_Status = NF90_INQ_VARID( NC_FileId,TAU_MAX_RMS_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TAU_MAX_RMS_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,FitStats%Tau_Max_RMS )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//TAU_MAX_RMS_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Max_Pred_Term
    NF90_Status = NF90_INQ_VARID( NC_FileId,MAX_PRED_TERM_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//MAX_PRED_TERM_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,FitStats%Max_Pred_Term )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//MAX_PRED_TERM_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    
  CONTAINS
  
    SUBROUTINE ReadVar_CleanUp()
      ! Close file
      NF90_Status = NF90_CLOSE( NC_FileID )
      IF ( NF90_Status /= NF90_NOERR ) &
        msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
              TRIM(NF90_STRERROR( NF90_Status ) )
      ! Set error status and print error msg
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
    END SUBROUTINE ReadVar_CleanUp

  END FUNCTION ReadVar


!------------------------------------------------------------------------------
!
! NAME:
!       CreateFile
!
! PURPOSE:
!       Function to create a netCDF FitStats data file for writing.
!
! CALLING SEQUENCE:
!       Error_Status = CreateFile( NC_Filename                      , &
!                                  n_Channels                       , &
!                                  NC_FileID                        , &
!                                  Version         =Version         , &
!                                  Sensor_Id       =Sensor_Id       , &
!                                  WMO_Satellite_Id=WMO_Satellite_Id, &
!                                  WMO_Sensor_Id   =WMO_Sensor_Id   , &
!                                  ID_Tag          =ID_Tag          , &
!                                  Title           =Title           , &
!                                  History         =History         , &
!                                  Comment         =Comment         , &
!                                  Message_Log     =Message_Log       )
!
! INPUT ARGUMENTS:
!       NC_Filename:        Character string specifying the name of the
!                           netCDF FitStats format data file to create.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Channels:         Number of sensor channels.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       NC_FileID:          NetCDF file ID number to be used for subsequent
!                           writing to the output file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Version:            The version number of the netCDF FitStats file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Sensor_Id:          Character string sensor/platform identifier.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       WMO_Satellite_Id:   The WMO code used to identify satellite platforms.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       WMO_Sensor_Id:      The WMO code used to identify sensors.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       ID_Tag:             Character string written into the ID_TAG global
!                           attribute field of the netCDF FitStats file.
!                           Should contain a short tag used to identify the
!                           dependent profile set.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:              Character string written into the TITLE global
!                           attribute field of the netCDF FitStats file.
!                           Should contain a succinct description of what
!                           is in the netCDF datafile.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the netCDF FitStats file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF FitStats file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:        Character string specifying a filename in which
!                           any msgs will be logged. If not specified,
!                           or if an error occurs opening the log file, the
!                           default action is to output msgs to standard
!                           output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.  
!                           The error codes are defined in the Message_Handler module. 
!                           If == SUCCESS the netCDF file creation was successful.     
!                              == FAILURE an unrecoverable error occurred.             
!                              == WARNING - an error occurred writing any of the       
!                                           supplied global attributes.                
!                                         - an error occurred closing the netCDF file. 
!                           UNITS:      N/A                                            
!                           TYPE:       INTEGER                                        
!                           DIMENSION:  Scalar                                         
!
!------------------------------------------------------------------------------

  FUNCTION CreateFile( NC_Filename     , &  ! Input
                       n_Channels      , &  ! Input
                       NC_FileID       , &  ! Output
                       Version         , &  ! Optional input
                       Sensor_Id       , &  ! Optional input
                       WMO_Satellite_Id, &  ! Optional input
                       WMO_Sensor_Id   , &  ! Optional input
                       ID_Tag          , &  ! Optional input
                       Title           , &  ! Optional input
                       History         , &  ! Optional input
                       Comment         , &  ! Optional input
                       Message_Log     ) &  ! Error messaging
                     RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)  :: NC_Filename
    INTEGER               , INTENT(IN)  :: n_Channels  
    INTEGER               , INTENT(OUT) :: NC_FileID
    INTEGER     , OPTIONAL, INTENT(IN)  :: Version         
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Sensor_Id       
    INTEGER     , OPTIONAL, INTENT(IN)  :: WMO_Satellite_Id
    INTEGER     , OPTIONAL, INTENT(IN)  :: WMO_Sensor_Id   
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: ID_Tag
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CreateFile'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: NF90_Status
    INTEGER :: n_Predictors_DimID
    INTEGER :: n_Channels_DimID
    

    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Check input
    IF ( n_Channels < 1 ) THEN
      msg = 'Invalid dimension input detected.'
      CALL Create_Cleanup(); RETURN
    END IF


    ! Create the data file
    ! --------------------
    NF90_Status = NF90_CREATE( NC_Filename,NF90_CLOBBER,NC_FileID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error creating '//TRIM(NC_Filename)//' - '//&
                TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF


    ! Define the dimensions
    ! ---------------------
    ! The number of predictors
    NF90_Status = NF90_DEF_DIM( NC_FileID,PREDICTOR_DIMNAME,MAX_PREDICTORS,n_Predictors_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//PREDICTOR_DIMNAME//' dimension in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF

    ! The number of spectral channels
    NF90_Status = NF90_DEF_DIM( NC_FileID,CHANNEL_DIMNAME,n_Channels,n_Channels_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//CHANNEL_DIMNAME//' dimension in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Write the global attributes
    ! ---------------------------
    Error_Status = WriteGAtts( NC_Filename                      , &
                               NC_FileID                        , &
                               Version         =Version         , &
                               Sensor_Id       =Sensor_Id       , &
                               WMO_Satellite_Id=WMO_Satellite_Id, &
                               WMO_Sensor_Id   =WMO_Sensor_Id   , &
                               ID_Tag          =ID_Tag          , &
                               Title           =Title           , &
                               History         =History         , &
                               Comment         =Comment         , &
                               Message_Log     =Message_Log       )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error writing global attributes to '//TRIM(NC_Filename)
      CALL Create_Cleanup(); RETURN
    END IF


    ! Define the FitStats variables
    ! ------------------------------
    Error_Status = DefineVar( NC_Filename            , &
                              NC_FileID              , &
                              n_Predictors_DimID     , &
                              n_Channels_DimID       , &
                              Message_Log=Message_Log  )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error defining variables in '//TRIM(NC_Filename)
      CALL Create_Cleanup(); RETURN
    END IF
                                             

    ! Take netCDF file out of define mode
    ! -----------------------------------
    NF90_Status = NF90_ENDDEF( NC_FileID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error taking '//TRIM(NC_Filename)//' out of define mode.'
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE Create_CleanUp( Close_File )
      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File ) THEN
          NF90_Status = NF90_CLOSE( NC_FileID )
          IF ( NF90_Status /= NF90_NOERR ) &
            msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
                      TRIM(NF90_STRERROR( NF90_Status ) )
        END IF
      END IF
      ! Set error status and print error msg
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
    END SUBROUTINE Create_CleanUp

  END FUNCTION CreateFile

END MODULE FitStats_netCDF_IO
