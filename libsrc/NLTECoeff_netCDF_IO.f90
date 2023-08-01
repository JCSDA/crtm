!
! NLTECoeff_netCDF_IO
!
! Module containing routines to read and write NLTECoeff netCDF 
! format files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 20-Jan-2011
!                       paul.vandelst@noaa.gov
!

MODULE NLTECoeff_netCDF_IO


  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds      , ONLY: Long, Double
  USE Message_Handler , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE File_Utility    , ONLY: File_Exists
  USE String_Utility  , ONLY: StrClean
  USE NLTECoeff_Define, ONLY: NLTECoeff_type        , &
                              NLTECoeff_Associated  , &
                              NLTECoeff_Destroy     , &
                              NLTECoeff_Create      , &
                              NLTECoeff_Inspect     , &
                              NLTECoeff_ValidRelease, &
                              NLTECoeff_Info        
  USE netcdf
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Procedures
  PUBLIC :: NLTECoeff_netCDF_InquireFile
  PUBLIC :: NLTECoeff_netCDF_ReadFile
  PUBLIC :: NLTECoeff_netCDF_WriteFile
  PUBLIC :: NLTECoeff_netCDF_IOVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Module version
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
    '$Id: NLTECoeff_netCDF_IO.f90 13518 2011-04-22 17:25:42Z paul.vandelst@noaa.gov $'
  ! Default msg string length
  INTEGER, PARAMETER :: ML = 1024
  ! Literal constants
  REAL(Double), PARAMETER :: ZERO = 0.0_Double

  ! Global attribute names.
  CHARACTER(*), PARAMETER :: RELEASE_GATTNAME          = 'Release'
  CHARACTER(*), PARAMETER :: VERSION_GATTNAME          = 'Version'
  CHARACTER(*), PARAMETER :: TITLE_GATTNAME            = 'Title' 
  CHARACTER(*), PARAMETER :: HISTORY_GATTNAME          = 'History'
  CHARACTER(*), PARAMETER :: COMMENT_GATTNAME          = 'Comment'
  CHARACTER(*), PARAMETER :: SENSOR_ID_GATTNAME        = 'Sensor_Id'
  CHARACTER(*), PARAMETER :: WMO_SATELLITE_ID_GATTNAME = 'WMO_Satellite_Id'
  CHARACTER(*), PARAMETER :: WMO_SENSOR_ID_GATTNAME    = 'WMO_Sensor_Id'
  
  ! Dimension names
  CHARACTER(*), PARAMETER :: PREDICTOR_DIMNAME    = 'n_Predictors'    
  CHARACTER(*), PARAMETER :: SENSOR_ANGLE_DIMNAME = 'n_Sensor_Angles' 
  CHARACTER(*), PARAMETER :: SOLAR_ANGLE_DIMNAME  = 'n_Solar_Angles'  
  CHARACTER(*), PARAMETER :: NLTE_CHANNEL_DIMNAME = 'n_NLTE_Channels' 
  CHARACTER(*), PARAMETER :: CHANNEL_DIMNAME      = 'n_Channels'
  CHARACTER(*), PARAMETER :: LAYER_DIMNAME        = 'n_Layers'

  ! Variable names
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_VARNAME  = 'Sensor_Channel'
  CHARACTER(*), PARAMETER :: UPPER_PLEVEL_VARNAME    = 'Upper_Plevel'
  CHARACTER(*), PARAMETER :: LOWER_PLEVEL_VARNAME    = 'Lower_Plevel'
  CHARACTER(*), PARAMETER :: MIN_TM_VARNAME          = 'Min_Tm'
  CHARACTER(*), PARAMETER :: MAX_TM_VARNAME          = 'Max_Tm'
  CHARACTER(*), PARAMETER :: MEAN_TM_VARNAME         = 'Mean_Tm'
  CHARACTER(*), PARAMETER :: SENSOR_ANGLE_VARNAME    = 'Secant_Sensor_Zenith'
  CHARACTER(*), PARAMETER :: SOLAR_ANGLE_VARNAME     = 'Secant_Solar_Zenith'
  CHARACTER(*), PARAMETER :: NLTE_CHANNEL_VARNAME    = 'NLTE_Channel'
  CHARACTER(*), PARAMETER :: C_INDEX_VARNAME         = 'C_Index'
  CHARACTER(*), PARAMETER :: C_VARNAME               = 'C'

  ! Variable long name attribute.
  CHARACTER(*), PARAMETER :: LONGNAME_ATTNAME = 'long_name'

  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_LONGNAME  = 'Sensor Channel'
  CHARACTER(*), PARAMETER :: UPPER_PLEVEL_LONGNAME    = 'Upper Pressure Levels'
  CHARACTER(*), PARAMETER :: LOWER_PLEVEL_LONGNAME    = 'Lower Pressure Levels'
  CHARACTER(*), PARAMETER :: MIN_TM_LONGNAME          = 'Minimum Layer Temperature'
  CHARACTER(*), PARAMETER :: MAX_TM_LONGNAME          = 'Maximum Layer Temperature'
  CHARACTER(*), PARAMETER :: MEAN_TM_LONGNAME         = 'Mean Layer Temperature'
  CHARACTER(*), PARAMETER :: SENSOR_ANGLE_LONGNAME    = 'Secant Sensor Zenith Angle'
  CHARACTER(*), PARAMETER :: SOLAR_ANGLE_LONGNAME     = 'Secant Solar Zenith Angle'
  CHARACTER(*), PARAMETER :: NLTE_CHANNEL_LONGNAME    = 'NLTE Channel'
  CHARACTER(*), PARAMETER :: C_INDEX_LONGNAME         = 'NLTE Channel Index'
  CHARACTER(*), PARAMETER :: C_LONGNAME               = 'NLTE Correction Coefficients'
  
  ! Variable description attribute.
  CHARACTER(*), PARAMETER :: DESCRIPTION_ATTNAME = 'description'

  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_DESCRIPTION  = 'List of sensor channel numbers'
  CHARACTER(*), PARAMETER :: UPPER_PLEVEL_DESCRIPTION    = 'Upper pressure levels used for computing mean layer temperatures'
  CHARACTER(*), PARAMETER :: LOWER_PLEVEL_DESCRIPTION    = 'Lower pressure levels used for computing mean layer temperatures'
  CHARACTER(*), PARAMETER :: MIN_TM_DESCRIPTION          = 'Minimum layer temperatures used as the temperature predictor limits'
  CHARACTER(*), PARAMETER :: MAX_TM_DESCRIPTION          = 'Maximum layer temperatures used as the temperature predictor limits'
  CHARACTER(*), PARAMETER :: MEAN_TM_DESCRIPTION         = 'Mean layer temperatures used as the temperature predictor limits'
  CHARACTER(*), PARAMETER :: SENSOR_ANGLE_DESCRIPTION    = 'Secant of the Sensor Zenith Angle'
  CHARACTER(*), PARAMETER :: SOLAR_ANGLE_DESCRIPTION     = 'Secant of the Solar Zenith Angle'
  CHARACTER(*), PARAMETER :: NLTE_CHANNEL_DESCRIPTION    = 'List of NLTE-affected channel numbers'
  CHARACTER(*), PARAMETER :: C_INDEX_DESCRIPTION         = 'Coefficient array index of NLTE-affected channel'
  CHARACTER(*), PARAMETER :: C_DESCRIPTION               = 'Coefficients used in the NLTE radianec correction algorithm'


  ! Variable units attribute.
  CHARACTER(*), PARAMETER :: UNITS_ATTNAME = 'units'
  
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_UNITS  = 'N/A'
  CHARACTER(*), PARAMETER :: UPPER_PLEVEL_UNITS    = 'hectoPascals (hPa)'
  CHARACTER(*), PARAMETER :: LOWER_PLEVEL_UNITS    = 'hectoPascals (hPa)'
  CHARACTER(*), PARAMETER :: MIN_TM_UNITS          = 'Kelvin (K)'
  CHARACTER(*), PARAMETER :: MAX_TM_UNITS          = 'Kelvin (K)'
  CHARACTER(*), PARAMETER :: MEAN_TM_UNITS         = 'Kelvin (K)'
  CHARACTER(*), PARAMETER :: SENSOR_ANGLE_UNITS    = 'N/A'
  CHARACTER(*), PARAMETER :: SOLAR_ANGLE_UNITS     = 'N/A'
  CHARACTER(*), PARAMETER :: NLTE_CHANNEL_UNITS    = 'N/A'
  CHARACTER(*), PARAMETER :: C_INDEX_UNITS         = 'N/A'
  CHARACTER(*), PARAMETER :: C_UNITS               = 'N/A'


  ! Variable _FillValue attribute.
  CHARACTER(*), PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'
  
  INTEGER(Long), PARAMETER :: SENSOR_CHANNEL_FILLVALUE  = 0_Long
  REAL(Double) , PARAMETER :: UPPER_PLEVEL_FILLVALUE    = 0.0_Double
  REAL(Double) , PARAMETER :: LOWER_PLEVEL_FILLVALUE    = 0.0_Double
  REAL(Double) , PARAMETER :: MIN_TM_FILLVALUE          = 0.0_Double
  REAL(Double) , PARAMETER :: MAX_TM_FILLVALUE          = 0.0_Double
  REAL(Double) , PARAMETER :: MEAN_TM_FILLVALUE         = 0.0_Double
  REAL(Double) , PARAMETER :: SENSOR_ANGLE_FILLVALUE    = 0.0_Double
  REAL(Double) , PARAMETER :: SOLAR_ANGLE_FILLVALUE     = 0.0_Double
  INTEGER(Long), PARAMETER :: NLTE_CHANNEL_FILLVALUE    = 0_Long
  INTEGER(Long), PARAMETER :: C_INDEX_FILLVALUE         = 0_Long
  REAL(Double) , PARAMETER :: C_FILLVALUE               = 0.0_Double


  ! Variable types
  INTEGER, PARAMETER :: SENSOR_CHANNEL_TYPE  = NF90_INT
  INTEGER, PARAMETER :: UPPER_PLEVEL_TYPE    = NF90_DOUBLE
  INTEGER, PARAMETER :: LOWER_PLEVEL_TYPE    = NF90_DOUBLE
  INTEGER, PARAMETER :: MIN_TM_TYPE          = NF90_DOUBLE
  INTEGER, PARAMETER :: MAX_TM_TYPE          = NF90_DOUBLE
  INTEGER, PARAMETER :: MEAN_TM_TYPE         = NF90_DOUBLE
  INTEGER, PARAMETER :: SENSOR_ANGLE_TYPE    = NF90_DOUBLE
  INTEGER, PARAMETER :: SOLAR_ANGLE_TYPE     = NF90_DOUBLE
  INTEGER, PARAMETER :: NLTE_CHANNEL_TYPE    = NF90_INT
  INTEGER, PARAMETER :: C_INDEX_TYPE         = NF90_INT
  INTEGER, PARAMETER :: C_TYPE               = NF90_DOUBLE


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
!       NLTECoeff_netCDF_InquireFile
!
! PURPOSE:
!       Function to inquire NLTECoeff object netCDF format files.
!
! CALLING SEQUENCE:
!       Error_Status = NLTECoeff_netCDF_InquireFile( &
!                        Filename, &
!                        n_Predictors     = n_Predictors    , &
!                        n_Sensor_Angles  = n_Sensor_Angles , &
!                        n_Solar_Angles   = n_Solar_Angles  , &
!                        n_NLTE_Channels  = n_NLTE_Channels , &
!                        n_Channels       = n_Channels      , &
!                        Release          = Release         , &
!                        Version          = Version         , &
!                        Sensor_Id        = Sensor_Id       , &
!                        WMO_Satellite_Id = WMO_Satellite_Id, &
!                        WMO_Sensor_Id    = WMO_Sensor_Id   , &
!                        Title            = Title           , &
!                        History          = History         , &
!                        Comment          = Comment           )
!
! INPUTS:
!       Filename:           Character string specifying the name of the
!                           NLTECoeff data file to inquire.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_Predictors:       The number of predictor functions used in generating
!                           the NLTE correction coefficients.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Sensor_Angles:    Number of sensor zenith angles.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Solar_Angles:     Number of solar zenith angles.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_NLTE_Channels:    Number of NLTE channels for the sensor.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Channels:         Total number of sensor channels.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:            The release number of the NLTECoeff file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:            The version number of the NLTECoeff file.
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
!       Title:              Character string written into the TITLE global
!                           attribute field of the NLTECoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the NLTECoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the NLTECoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           Message_Handler module.
!                           If == SUCCESS the file inquiry was successful
!                              == FAILURE an error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION NLTECoeff_netCDF_InquireFile( &
    Filename        , &  ! Input
    n_Predictors    , &  ! Optional output
    n_Sensor_Angles , &  ! Optional output
    n_Solar_Angles  , &  ! Optional output
    n_NLTE_Channels , &  ! Optional output  
    n_Channels      , &  ! Optional output  
    Release         , &  ! Optional Output
    Version         , &  ! Optional Output
    Sensor_Id       , &  ! Optional Output
    WMO_Satellite_Id, &  ! Optional Output
    WMO_Sensor_Id   , &  ! Optional Output
    Title           , &  ! Optional output
    History         , &  ! Optional output
    Comment         ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Predictors
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Sensor_Angles
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Solar_Angles
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_NLTE_Channels
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Channels
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Sensor_Id       
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Satellite_Id
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Sensor_Id   
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title           
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment         
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'NLTECoeff_InquireFile(netCDF)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: close_file
    INTEGER :: nf90_status
    INTEGER :: fileid
    INTEGER :: dimid
    TYPE(NLTECoeff_type) :: nltecoeff
    
    ! Set up
    err_stat = SUCCESS
    close_file = .FALSE.


    ! Open the file
    nf90_status = NF90_OPEN( Filename,NF90_NOWRITE,fileid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error opening '//TRIM(Filename)//' for read access - '// &
            TRIM(NF90_STRERROR( nf90_status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    close_file = .TRUE.


    ! Get the dimensions
    ! ...n_Predictors dimension
    nf90_status = NF90_INQ_DIMID( FileId,PREDICTOR_DIMNAME,DimId )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//PREDICTOR_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( nf90_status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    nf90_status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=nltecoeff%n_Predictors )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//PREDICTOR_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( nf90_status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...n_Sensor_Angles dimension
    nf90_status = NF90_INQ_DIMID( FileId,SENSOR_ANGLE_DIMNAME,DimId )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//SENSOR_ANGLE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( nf90_status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    nf90_status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=nltecoeff%n_Sensor_Angles )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//SENSOR_ANGLE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( nf90_status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...n_Solar_Angles dimension
    nf90_status = NF90_INQ_DIMID( FileId,SOLAR_ANGLE_DIMNAME,DimId )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//SOLAR_ANGLE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( nf90_status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    nf90_status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=nltecoeff%n_Solar_Angles )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//SOLAR_ANGLE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( nf90_status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...n_NLTE_Channels dimension
    nf90_status = NF90_INQ_DIMID( FileId,NLTE_CHANNEL_DIMNAME,DimId )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//NLTE_CHANNEL_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( nf90_status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    nf90_status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=nltecoeff%n_NLTE_Channels )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//NLTE_CHANNEL_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( nf90_status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...n_Channels dimension
    nf90_status = NF90_INQ_DIMID( FileId,CHANNEL_DIMNAME,DimId )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//CHANNEL_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( nf90_status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    nf90_status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=nltecoeff%n_Channels )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//CHANNEL_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( nf90_status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
  
  
    ! Get the global attributes
    err_stat = ReadGAtts( Filename, &
                          fileid  , &
                          Release          = Release         , &
                          Version          = Version         , &
                          Sensor_Id        = Sensor_Id       , &
                          WMO_Satellite_Id = WMO_Satellite_Id, &
                          WMO_Sensor_Id    = WMO_Sensor_Id   , &
                          Title            = Title           , &
                          History          = History         , &
                          Comment          = Comment           )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attributes from '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Close the file
    nf90_status = NF90_CLOSE( fileid )
    close_file = .FALSE.
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error closing input file - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Set the return values
    IF ( PRESENT(n_Predictors    ) ) n_Predictors     = nltecoeff%n_Predictors
    IF ( PRESENT(n_Sensor_Angles ) ) n_Sensor_Angles  = nltecoeff%n_Sensor_Angles
    IF ( PRESENT(n_Solar_Angles  ) ) n_Solar_Angles   = nltecoeff%n_Solar_Angles
    IF ( PRESENT(n_NLTE_Channels ) ) n_NLTE_Channels  = nltecoeff%n_NLTE_Channels
    IF ( PRESENT(n_Channels      ) ) n_Channels       = nltecoeff%n_Channels

  CONTAINS
 
    SUBROUTINE Inquire_CleanUp()
      IF ( close_file ) THEN
        nf90_status = NF90_CLOSE( fileid )
        IF ( nf90_status /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup.'
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION NLTECoeff_netCDF_InquireFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTECoeff_netCDF_WriteFile
!
! PURPOSE:
!       Function to write NLTECoeff object files in netCDF format.
!
! CALLING SEQUENCE:
!       Error_Status = NLTECoeff_netCDF_WriteFile( &
!                        Filename         , &
!                        NLTECoeff        , &
!                        Quiet   = Quiet  , &
!                        Title   = Title  , &
!                        History = History, &
!                        Comment = Comment  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       NLTECoeff data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       NLTECoeff:      NLTECoeff object containing the NLTE correction
!                       coefficient data.
!                       UNITS:      N/A
!                       TYPE:       NLTECoeff_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Quiet:          Set this logical argument to suppress INFORMATION
!                       messages being printed to stdout
!                       If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                          == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:          Character string written into the TITLE global
!                       attribute field of the NLTECoeff file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the NLTECoeff file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the NLTECoeff file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the data write was successful
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION NLTECoeff_netCDF_WriteFile( &
    Filename , &  ! Input
    NLTECoeff, &  ! Input
    Quiet    , &  ! Optional input
    Title    , &  ! Optional input
    History  , &  ! Optional input
    Comment  ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: Filename
    TYPE(NLTECoeff_type),   INTENT(IN) :: NLTECoeff
    LOGICAL,      OPTIONAL, INTENT(IN) :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'NLTECoeff_WriteFile(netCDF)'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: close_file
    LOGICAL :: noisy
    INTEGER :: nf90_status
    INTEGER :: fileid
    INTEGER :: varid

    ! Set up
    err_stat = SUCCESS
    close_file = .FALSE.
    ! ...Check structure pointer association status
    IF ( .NOT. NLTECoeff_Associated( NLTECoeff ) ) THEN
      msg = 'NLTECoeff structure is empty. Nothing to do!'
      CALL Write_CleanUp(); RETURN
    END IF
    ! ...Check if release is valid
    IF ( .NOT. NLTECoeff_ValidRelease( NLTECoeff ) ) THEN
      msg = 'NLTECoeff Release check failed.'
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet


    ! Create the output file
    err_stat = CreateFile( &
                 Filename                                     , &  ! Input
                 NLTECoeff%n_Predictors                       , &  ! Input
                 NLTECoeff%n_Sensor_Angles                    , &  ! Input
                 NLTECoeff%n_Solar_Angles                     , &  ! Input
                 NLTECoeff%n_NLTE_Channels                    , &  ! Input
                 NLTECoeff%n_Channels                         , &  ! Input
                 NLTECoeff%n_Layers                           , &  ! Input
                 fileid                                       , &  ! Output
                 Version          = NLTECoeff%Version         , &  ! Optional input
                 Sensor_Id        = NLTECoeff%Sensor_Id       , &  ! Optional input
                 WMO_Satellite_Id = NLTECoeff%WMO_Satellite_Id, &  ! Optional input
                 WMO_Sensor_Id    = NLTECoeff%WMO_Sensor_Id   , &  ! Optional input
                 Title            = Title                     , &  ! Optional input
                 History          = History                   , &  ! Optional input
                 Comment          = Comment                     )  ! Optional input
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error creating output file '//TRIM(Filename)
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    close_file = .TRUE.


    ! Write the data items
    ! ...Sensor_Channel variable
    nf90_status = NF90_INQ_varid( fileid,SENSOR_CHANNEL_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//SENSOR_CHANNEL_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Write_Cleanup(); RETURN
    END IF
    nf90_status = NF90_PUT_VAR( fileid,varid,NLTECoeff%Sensor_Channel )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error writing '//SENSOR_CHANNEL_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Upper_Plevel variable
    nf90_status = NF90_INQ_varid( fileid,UPPER_PLEVEL_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//UPPER_PLEVEL_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Write_Cleanup(); RETURN
    END IF
    nf90_status = NF90_PUT_VAR( fileid,varid,NLTECoeff%Upper_Plevel )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error writing '//UPPER_PLEVEL_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Lower_Plevel variable
    nf90_status = NF90_INQ_varid( fileid,LOWER_PLEVEL_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//LOWER_PLEVEL_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Write_Cleanup(); RETURN
    END IF
    nf90_status = NF90_PUT_VAR( fileid,varid,NLTECoeff%Lower_Plevel )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error writing '//LOWER_PLEVEL_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Min_Tm variable
    nf90_status = NF90_INQ_varid( fileid,MIN_TM_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//MIN_TM_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Write_Cleanup(); RETURN
    END IF
    nf90_status = NF90_PUT_VAR( fileid,varid,NLTECoeff%Min_Tm )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error writing '//MIN_TM_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Max_Tm variable
    nf90_status = NF90_INQ_varid( fileid,MAX_TM_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//MAX_TM_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Write_Cleanup(); RETURN
    END IF
    nf90_status = NF90_PUT_VAR( fileid,varid,NLTECoeff%Max_Tm )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error writing '//MAX_TM_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Mean_Tm variable
    nf90_status = NF90_INQ_varid( fileid,MEAN_TM_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//MEAN_TM_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Write_Cleanup(); RETURN
    END IF
    nf90_status = NF90_PUT_VAR( fileid,varid,NLTECoeff%Mean_Tm )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error writing '//MEAN_TM_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Secant_Sensor_Zenith variable
    nf90_status = NF90_INQ_varid( fileid,SENSOR_ANGLE_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//SENSOR_ANGLE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Write_Cleanup(); RETURN
    END IF
    nf90_status = NF90_PUT_VAR( fileid,varid,NLTECoeff%Secant_Sensor_Zenith )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error writing '//SENSOR_ANGLE_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Secant_Solar_Zenith variable
    nf90_status = NF90_INQ_varid( fileid,SOLAR_ANGLE_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//SOLAR_ANGLE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Write_Cleanup(); RETURN
    END IF
    nf90_status = NF90_PUT_VAR( fileid,varid,NLTECoeff%Secant_Solar_Zenith )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error writing '//SOLAR_ANGLE_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...NLTE_Channel variable
    nf90_status = NF90_INQ_varid( fileid,NLTE_CHANNEL_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//NLTE_CHANNEL_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Write_Cleanup(); RETURN
    END IF
    nf90_status = NF90_PUT_VAR( fileid,varid,NLTECoeff%NLTE_Channel )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error writing '//NLTE_CHANNEL_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...C_Index variable
    nf90_status = NF90_INQ_varid( fileid,C_INDEX_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//C_INDEX_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Write_Cleanup(); RETURN
    END IF
    nf90_status = NF90_PUT_VAR( fileid,varid,NLTECoeff%C_Index )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error writing '//C_INDEX_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...C variable
    nf90_status = NF90_INQ_varid( fileid,C_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//C_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Write_Cleanup(); RETURN
    END IF
    nf90_status = NF90_PUT_VAR( fileid,varid,NLTECoeff%C )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error writing '//C_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Close the file
    nf90_status = NF90_CLOSE( fileid )
    close_file = .FALSE.
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error closing output file - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Output an info message
    IF ( noisy ) THEN
      CALL NLTECoeff_Info( NLTECoeff, msg )
      CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
    END IF

  CONTAINS

    SUBROUTINE Write_CleanUp()
      IF ( close_file ) THEN
        nf90_status = NF90_CLOSE( fileid )
        IF ( nf90_status /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing output file during error cleanup - '//&
                TRIM(NF90_STRERROR( nf90_status ))
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Write_CleanUp

  END FUNCTION NLTECoeff_netCDF_WriteFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTECoeff_netCDF_ReadFile
!
! PURPOSE:
!       Function to read NLTECoeff object files in netCDF format.
!
! CALLING SEQUENCE:
!       Error_Status = NLTECoeff_netCDF_ReadFile( &
!                        Filename         , &
!                        NLTECoeff        , &
!                        Quiet   = Quiet  , &
!                        Title   = Title  , &
!                        History = History, &
!                        Comment = Comment  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       NLTECoeff data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       NLTECoeff:      NLTECoeff object containing the NLTE correction
!                       coefficient data.
!                       UNITS:      N/A
!                       TYPE:       NLTECoeff_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Quiet:          Set this logical argument to suppress INFORMATION
!                       messages being printed to stdout
!                       If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                          == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUTS:
!       Title:          Character string written into the TITLE global
!                       attribute field of the NLTECoeff file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the NLTECoeff file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the NLTECoeff file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the data write was successful
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION NLTECoeff_netCDF_ReadFile( &
    Filename , &  ! Input
    NLTECoeff, &  ! Output
    Quiet    , &  ! Optional input
    Title    , &  ! Optional output
    History  , &  ! Optional output
    Comment  ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    TYPE(NLTECoeff_type),   INTENT(OUT) :: NLTECoeff
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'NLTECoeff_ReadFile(netCDF)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: close_file
    LOGICAL :: noisy
    INTEGER :: nf90_status
    INTEGER :: fileid
    INTEGER :: n_predictors  
    INTEGER :: n_sensor_angles
    INTEGER :: n_solar_angles
    INTEGER :: n_nlte_channels
    INTEGER :: n_channels
    INTEGER :: varid


    ! Set up
    err_stat = SUCCESS
    close_file = .FALSE.
    ! ...Check that the file exists
    IF ( .NOT. File_Exists(Filename) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet


    ! Inquire the file to get the dimensions
    err_stat = NLTECoeff_netCDF_InquireFile( &
                 Filename, &
                 n_Predictors    = n_predictors   , &
                 n_Sensor_Angles = n_sensor_angles, &
                 n_Solar_Angles  = n_solar_angles , &
                 n_NLTE_Channels = n_nlte_channels, &
                 n_Channels      = n_channels       )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error obtaining NLTECoeff dimensions from '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Allocate the output structure
    CALL NLTECoeff_Create( &
           NLTECoeff, &
           n_predictors   , &
           n_sensor_angles, &
           n_solar_angles , &
           n_nlte_channels, &
           n_channels       )
    IF ( .NOT. NLTECoeff_Associated(NLTECoeff) ) THEN
      msg = 'Error allocating output NLTECoeff'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Open the file for reading
    nf90_status = NF90_OPEN( Filename,NF90_NOWRITE,fileid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error opening '//TRIM(Filename)//' for read access - '//&
            TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    close_file = .TRUE.


    ! Read the global attributes
    err_stat = ReadGAtts( &
                 Filename, &
                 fileid  , &
                 Release          = NLTECoeff%Release         , &
                 Version          = NLTECoeff%Version         , &
                 Sensor_Id        = NLTECoeff%Sensor_Id       , &
                 WMO_Satellite_Id = NLTECoeff%WMO_Satellite_Id, &
                 WMO_Sensor_Id    = NLTECoeff%WMO_Sensor_Id   , &
                 Title            = Title                     , &
                 History          = History                   , &
                 Comment          = Comment                     )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attribute from '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check if release is valid
    IF ( .NOT. NLTECoeff_ValidRelease( NLTECoeff ) ) THEN
      msg = 'NLTECoeff Release check failed.'
      CALL Read_Cleanup(); RETURN
    END IF

    
    ! Read the NLTECoeff data
    ! ...Sensor_Channel variable
    nf90_status = NF90_INQ_varid( fileid,SENSOR_CHANNEL_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//SENSOR_CHANNEL_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,NLTECoeff%Sensor_Channel )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//SENSOR_CHANNEL_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Upper_Plevel variable
    nf90_status = NF90_INQ_varid( fileid,UPPER_PLEVEL_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//UPPER_PLEVEL_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,NLTECoeff%Upper_Plevel )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//UPPER_PLEVEL_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Lower_Plevel variable
    nf90_status = NF90_INQ_varid( fileid,LOWER_PLEVEL_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//LOWER_PLEVEL_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,NLTECoeff%Lower_Plevel )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//LOWER_PLEVEL_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Min_Tm variable
    nf90_status = NF90_INQ_varid( fileid,MIN_TM_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//MIN_TM_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,NLTECoeff%Min_Tm )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//MIN_TM_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Max_Tm variable
    nf90_status = NF90_INQ_varid( fileid,MAX_TM_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//MAX_TM_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,NLTECoeff%Max_Tm )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//MAX_TM_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Mean_Tm variable
    nf90_status = NF90_INQ_varid( fileid,MEAN_TM_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//MEAN_TM_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,NLTECoeff%Mean_Tm )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//MEAN_TM_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Secant_Sensor_Zenith variable
    nf90_status = NF90_INQ_varid( fileid,SENSOR_ANGLE_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//SENSOR_ANGLE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,NLTECoeff%Secant_Sensor_Zenith )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//SENSOR_ANGLE_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Secant_Solar_Zenith variable
    nf90_status = NF90_INQ_varid( fileid,SOLAR_ANGLE_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//SOLAR_ANGLE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,NLTECoeff%Secant_Solar_Zenith )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//SOLAR_ANGLE_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...NLTE_Channel variable
    nf90_status = NF90_INQ_varid( fileid,NLTE_CHANNEL_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//NLTE_CHANNEL_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,NLTECoeff%NLTE_Channel )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//NLTE_CHANNEL_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...C_Index variable
    nf90_status = NF90_INQ_varid( fileid,C_INDEX_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//C_INDEX_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,NLTECoeff%C_Index )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//C_INDEX_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...C variable
    nf90_status = NF90_INQ_varid( fileid,C_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//C_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,NLTECoeff%C )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//C_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF


    ! Set the logical flag based on the C_Index values
    WHERE( NLTECoeff%C_Index > 0 ) NLTECoeff%Is_NLTE_Channel = .TRUE.
    

    ! Close the file
    nf90_status = NF90_CLOSE( fileid ); CLOSE_FILE = .FALSE.
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error closing output file - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF


    ! Output an info message
    IF ( noisy ) THEN
      CALL NLTECoeff_Info( NLTECoeff, msg )
      CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
    END IF

  CONTAINS
 
    SUBROUTINE Read_CleanUp()
      IF ( close_file ) THEN
        nf90_status = NF90_CLOSE( fileid )
        IF ( nf90_status /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup- '//&
                TRIM(NF90_STRERROR( nf90_status ))
      END IF
      CALL NLTECoeff_Destroy( NLTECoeff )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Read_CleanUp
    
  END FUNCTION NLTECoeff_netCDF_ReadFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTECoeff_netCDF_IOVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL NLTECoeff_netCDF_IOVersion( Id )
!
! OUTPUT ARGUMENTS:
!       Id:            Character string containing the version Id information
!                      for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE NLTECoeff_netCDF_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE NLTECoeff_netCDF_IOVersion


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  ! Function to write the global attributes to a NLTECoeff data file.

  FUNCTION WriteGAtts( &
    Filename        , &  ! Input
    FileId          , &  ! Input
    Version         , &  ! Optional input
    Sensor_Id       , &  ! Optional input
    WMO_Satellite_Id, &  ! Optional input
    WMO_Sensor_Id   , &  ! Optional input
    Title           , &  ! Optional input
    History         , &  ! Optional input
    Comment         ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: Filename
    INTEGER     ,           INTENT(IN) :: FileId
    INTEGER     , OPTIONAL, INTENT(IN) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Sensor_Id       
    INTEGER     , OPTIONAL, INTENT(IN) :: WMO_Satellite_Id
    INTEGER     , OPTIONAL, INTENT(IN) :: WMO_Sensor_Id   
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'NLTECoeff_WriteGAtts(netCDF)'
    CHARACTER(*), PARAMETER :: WRITE_MODULE_HISTORY_GATTNAME   = 'write_module_history'
    CHARACTER(*), PARAMETER :: CREATION_DATE_AND_TIME_GATTNAME = 'creation_date_and_time'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: gattname
    CHARACTER(8)  :: cdate
    CHARACTER(10) :: ctime
    CHARACTER(5)  :: czone
    INTEGER :: ver
    INTEGER :: nf90_status
    TYPE(NLTECoeff_type) :: nltecoeff

    ! Set up
    err_stat = SUCCESS
    msg = ' '

    ! Mandatory global attributes
    ! ...Software ID
    gattname = WRITE_MODULE_HISTORY_GATTNAME
    nf90_status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(gattname),MODULE_VERSION_ID )
    IF ( nf90_status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF
    ! ...Creation date
    CALL DATE_AND_TIME( cdate, ctime, czone )
    gattname = CREATION_DATE_AND_TIME_GATTNAME
    nf90_status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(gattname), &
                                cdate(1:4)//'/'//cdate(5:6)//'/'//cdate(7:8)//', '// &
                                ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//' '// &
                                czone//'UTC' )
    IF ( nf90_status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF
    ! ...The Release
    gattname = RELEASE_GATTNAME
    nf90_status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(gattname),NLTECoeff%Release )
    IF ( nf90_status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF


    ! Optional global attributes
    ! ...The Version
    IF ( PRESENT(Version) ) THEN
      ver = Version
    ELSE
      ver = nltecoeff%Version
    END IF
    gattname = VERSION_GATTNAME
    nf90_status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(gattname),Ver )
    IF ( nf90_status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF
    ! ...The Sensor_Id
    IF ( PRESENT(Sensor_Id) ) THEN
      gattname = SENSOR_ID_GATTNAME
      nf90_status = NF90_PUT_ATT( FileID,NF90_GLOBAL,TRIM(gattname),Sensor_Id )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The WMO_Satellite_Id
    IF ( PRESENT(WMO_Satellite_Id) ) THEN
      gattname = WMO_SATELLITE_ID_GATTNAME
      nf90_status = NF90_PUT_ATT( FileID,NF90_GLOBAL,TRIM(gattname),WMO_Satellite_Id )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The WMO_Sensor_Id
    IF ( PRESENT(WMO_Sensor_Id) ) THEN
      gattname = WMO_SENSOR_ID_GATTNAME
      nf90_status = NF90_PUT_ATT( FileID,NF90_GLOBAL,TRIM(gattname),WMO_Sensor_Id )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The title
    IF ( PRESENT(title) ) THEN
      gattname = TITLE_GATTNAME
      nf90_status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(gattname),title )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The history
    IF ( PRESENT(history) ) THEN
      gattname = HISTORY_GATTNAME
      nf90_status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(gattname),history )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The comment
    IF ( PRESENT(comment) ) THEN
      gattname = COMMENT_GATTNAME
      nf90_status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(gattname),comment )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    
 CONTAINS
  
    SUBROUTINE WriteGAtts_CleanUp()
      nf90_status = NF90_CLOSE( FileId )
      IF ( nf90_status /= NF90_NOERR ) &
        msg = '; Error closing input file during error cleanup - '//&
              TRIM(NF90_STRERROR( nf90_status ) )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM(gattname)//' attribute to '//&
                            TRIM(Filename)//' - '// &
                            TRIM(NF90_STRERROR( nf90_status ) )//TRIM(msg), &
                            err_stat )
    END SUBROUTINE WriteGAtts_CleanUp
    
  END FUNCTION WriteGAtts


  ! Function to read the global attributes from a NLTECoeff data file.

  FUNCTION ReadGAtts( &
    Filename        , &  ! Input
    FileId          , &  ! Input
    Release         , &  ! Optional output
    Version         , &  ! Optional output
    Sensor_Id       , &  ! Optional output
    WMO_Satellite_Id, &  ! Optional output
    WMO_Sensor_Id   , &  ! Optional output
    Title           , &  ! Optional output
    History         , &  ! Optional output
    Comment         ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     ,           INTENT(IN)  :: FileId
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release        
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Sensor_Id       
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Satellite_Id
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Sensor_Id   
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'NLTECoeff_ReadGAtts(netCDF)'
    ! Local variables
    CHARACTER(ML)   :: msg
    CHARACTER(256)  :: gattname
    CHARACTER(5000) :: gattstring
    INTEGER :: nf90_status
    
    ! Set up
    err_stat = SUCCESS

    ! The global attributes
    ! ...The Release
    IF ( PRESENT(Release) ) THEN
      gattname = RELEASE_GATTNAME
      nf90_status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(gattname),Release )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The Version
    IF ( PRESENT(Version) ) THEN
      gattname = VERSION_GATTNAME
      nf90_status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(gattname),Version )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The Sensor_Id
    IF ( PRESENT(Sensor_Id) ) THEN
      gattname = SENSOR_ID_GATTNAME; gattstring = ''
      nf90_status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(gattname),gattstring )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL StrClean( gattstring )
      Sensor_Id = gattstring(1:MIN(LEN(Sensor_Id), LEN_TRIM(gattstring)))
    END IF
    ! The WMO_Satellite_Id
    IF ( PRESENT(WMO_Satellite_Id) ) THEN
      gattname = WMO_SATELLITE_ID_GATTNAME
      nf90_status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(gattname),WMO_Satellite_Id )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The WMO_Sensor_Id
    IF ( PRESENT(WMO_Sensor_Id) ) THEN
      gattname = WMO_SENSOR_ID_GATTNAME
      nf90_status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(gattname),WMO_Sensor_Id )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The title
    IF ( PRESENT(title) ) THEN
      gattname = TITLE_GATTNAME; gattstring = ''
      nf90_status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(gattname),gattstring )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF        
      CALL StrClean( gattstring )
      title = gattstring(1:MIN(LEN(title), LEN_TRIM(gattstring)))
    END IF
    ! ...The history
    IF ( PRESENT(history) ) THEN
      gattname = HISTORY_GATTNAME; gattstring = ''
      nf90_status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(gattname),gattstring )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF        
      CALL StrClean( gattstring )
      history = gattstring(1:MIN(LEN(history), LEN_TRIM(gattstring)))
    END IF
    ! ...The comment
    IF ( PRESENT(comment) ) THEN
      gattname = COMMENT_GATTNAME; gattstring = ''
      nf90_status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(gattname),gattstring )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF        
      CALL StrClean( gattstring )
      comment = gattstring(1:MIN(LEN(comment), LEN_TRIM(gattstring)))
    END IF

  CONTAINS

    SUBROUTINE ReadGAtts_CleanUp()
      err_stat = FAILURE
      msg = 'Error reading '//TRIM(gattname)//' attribute from '//TRIM(Filename)//' - '// &
            TRIM(NF90_STRERROR( nf90_status ) )
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE ReadGAtts_CleanUp

  END FUNCTION ReadGAtts


  ! Function to create a NLTECoeff file for writing

  FUNCTION CreateFile( &
    Filename        , &  ! Input
    n_Predictors    , &  ! Input
    n_Sensor_Angles , &  ! Input
    n_Solar_Angles  , &  ! Input
    n_NLTE_Channels , &  ! Input
    n_Channels      , &  ! Input
    n_Layers        , &  ! Input
    FileId          , &  ! Output
    Version         , &  ! Optional input
    Sensor_Id       , &  ! Optional input
    WMO_Satellite_Id, &  ! Optional input
    WMO_Sensor_Id   , &  ! Optional input
    Title           , &  ! Optional input
    History         , &  ! Optional input
    Comment         ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     ,           INTENT(IN)  :: n_Predictors   
    INTEGER     ,           INTENT(IN)  :: n_Sensor_Angles
    INTEGER     ,           INTENT(IN)  :: n_Solar_Angles 
    INTEGER     ,           INTENT(IN)  :: n_NLTE_Channels
    INTEGER     ,           INTENT(IN)  :: n_Channels     
    INTEGER     ,           INTENT(IN)  :: n_Layers       
    INTEGER     ,           INTENT(OUT) :: FileId
    INTEGER     , OPTIONAL, INTENT(IN)  :: Version         
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Sensor_Id       
    INTEGER     , OPTIONAL, INTENT(IN)  :: WMO_Satellite_Id         
    INTEGER     , OPTIONAL, INTENT(IN)  :: WMO_Sensor_Id            
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'NLTECoeff_CreateFile(netCDF)'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: close_file
    INTEGER :: nf90_status
    INTEGER :: n_predictors_dimid   
    INTEGER :: n_sensor_angles_dimid
    INTEGER :: n_solar_angles_dimid 
    INTEGER :: n_nlte_channels_dimid
    INTEGER :: n_channels_dimid     
    INTEGER :: n_layers_dimid 
    INTEGER :: varid
    INTEGER :: put_status(4)
    
    ! Setup
    err_stat = SUCCESS
    close_file = .FALSE.


    ! Create the data file
    nf90_status = NF90_CREATE( Filename,NF90_CLOBBER,FileId )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error creating '//TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    close_file = .TRUE.


    ! Define the dimensions
    ! ...Number of predictors used in NLTE correction algorithm
    nf90_status = NF90_DEF_DIM( FileID,PREDICTOR_DIMNAME,n_predictors,n_predictors_dimid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//PREDICTOR_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Number of sensor zenith angles
    nf90_status = NF90_DEF_DIM( FileID,SENSOR_ANGLE_DIMNAME,n_sensor_angles,n_sensor_angles_dimid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//SENSOR_ANGLE_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Number of solar zenith angles
    nf90_status = NF90_DEF_DIM( FileID,SOLAR_ANGLE_DIMNAME,n_solar_angles,n_solar_angles_dimid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//SOLAR_ANGLE_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Number of NLTE channels for a sensor
    nf90_status = NF90_DEF_DIM( FileID,NLTE_CHANNEL_DIMNAME,n_nlte_channels,n_nlte_channels_dimid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//NLTE_CHANNEL_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Total number of channels for the sensor
    nf90_status = NF90_DEF_DIM( FileID,CHANNEL_DIMNAME,n_channels,n_channels_dimid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//CHANNEL_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Number of layers for which mean temperatures are computed
    nf90_status = NF90_DEF_DIM( FileID,LAYER_DIMNAME,n_layers,n_layers_dimid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//LAYER_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF


    ! Write the global attributes
    err_stat = WriteGAtts( &
                 Filename, &
                 FileId  , &
                 Version          = Version         , &
                 Sensor_Id        = Sensor_Id       , &
                 WMO_Satellite_Id = WMO_Satellite_Id, &
                 WMO_Sensor_Id    = WMO_Sensor_Id   , &
                 Title            = Title           , &
                 History          = History         , &
                 Comment          = Comment           )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing global attribute to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF


    ! Define the variables
    ! ...Sensor_Channel variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                SENSOR_CHANNEL_VARNAME, &
                                SENSOR_CHANNEL_TYPE, &
                                dimIDs=(/n_channels_dimid/), &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//SENSOR_CHANNEL_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,SENSOR_CHANNEL_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,SENSOR_CHANNEL_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,SENSOR_CHANNEL_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,SENSOR_CHANNEL_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//SENSOR_CHANNEL_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Upper_Plevel variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                UPPER_PLEVEL_VARNAME, &
                                UPPER_PLEVEL_TYPE, &
                                dimIDs=(/n_layers_dimid/), &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//UPPER_PLEVEL_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,UPPER_PLEVEL_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,UPPER_PLEVEL_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,UPPER_PLEVEL_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,UPPER_PLEVEL_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//UPPER_PLEVEL_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Lower_Plevel variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                LOWER_PLEVEL_VARNAME, &
                                LOWER_PLEVEL_TYPE, &
                                dimIDs=(/n_layers_dimid/), &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//LOWER_PLEVEL_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,LOWER_PLEVEL_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,LOWER_PLEVEL_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,LOWER_PLEVEL_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,LOWER_PLEVEL_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//LOWER_PLEVEL_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Min_Tm variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                MIN_TM_VARNAME, &
                                MIN_TM_TYPE, &
                                dimIDs=(/n_layers_dimid/), &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//MIN_TM_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,MIN_TM_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,MIN_TM_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,MIN_TM_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,MIN_TM_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//MIN_TM_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Max_Tm variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                MAX_TM_VARNAME, &
                                MAX_TM_TYPE, &
                                dimIDs=(/n_layers_dimid/), &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//MAX_TM_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,MAX_TM_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,MAX_TM_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,MAX_TM_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,MAX_TM_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//MAX_TM_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Mean_Tm variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                MEAN_TM_VARNAME, &
                                MEAN_TM_TYPE, &
                                dimIDs=(/n_layers_dimid/), &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//MEAN_TM_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,MEAN_TM_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,MEAN_TM_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,MEAN_TM_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,MEAN_TM_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//MEAN_TM_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Secant_Sensor_Zenith variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                SENSOR_ANGLE_VARNAME, &
                                SENSOR_ANGLE_TYPE, &
                                dimIDs=(/n_sensor_angles_dimid/), &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//SENSOR_ANGLE_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,SENSOR_ANGLE_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,SENSOR_ANGLE_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,SENSOR_ANGLE_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,SENSOR_ANGLE_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//SENSOR_ANGLE_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Secant_Solar_Zenith variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                SOLAR_ANGLE_VARNAME, &
                                SOLAR_ANGLE_TYPE, &
                                dimIDs=(/n_solar_angles_dimid/), &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//SOLAR_ANGLE_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,SOLAR_ANGLE_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,SOLAR_ANGLE_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,SOLAR_ANGLE_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,SOLAR_ANGLE_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//SOLAR_ANGLE_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...NLTE_Channel variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                NLTE_CHANNEL_VARNAME, &
                                NLTE_CHANNEL_TYPE, &
                                dimIDs=(/n_nlte_channels_dimid/), &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//NLTE_CHANNEL_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,NLTE_CHANNEL_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,NLTE_CHANNEL_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,NLTE_CHANNEL_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,NLTE_CHANNEL_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//NLTE_CHANNEL_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...C_Index variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                C_INDEX_VARNAME, &
                                C_INDEX_TYPE, &
                                dimIDs=(/n_channels_dimid/), &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//C_INDEX_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,C_INDEX_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,C_INDEX_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,C_INDEX_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,C_INDEX_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//C_INDEX_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...C variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                C_VARNAME, &
                                C_TYPE, &
                                dimIDs=(/n_predictors_dimid   ,&
                                         n_sensor_angles_dimid,&
                                         n_solar_angles_dimid ,&
                                         n_nlte_channels_dimid/), &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//C_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,C_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,C_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,C_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,C_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//C_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF


    ! Take netCDF file out of define mode
    nf90_status = NF90_ENDDEF( FileId )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error taking file '//TRIM(Filename)// &
            ' out of define mode - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF

  CONTAINS
 
    SUBROUTINE Create_CleanUp()
      IF ( close_file ) THEN
        nf90_status = NF90_CLOSE( FileID )
        IF ( nf90_status /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
                TRIM(NF90_STRERROR( nf90_status ))
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Create_CleanUp
    
  END FUNCTION CreateFile

END MODULE NLTECoeff_netCDF_IO
