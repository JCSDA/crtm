!
! SpcCoeff_netCDF_IO
!
! Module containing routines to read and write netCDF format
! SpcCoeff files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 17-Dec-2002
!                       paul.vandelst@ssec.wisc.edu
!

MODULE SpcCoeff_netCDF_IO_old

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds       , ONLY: Long, Double
  USE Message_Handler  , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                               Display_Message
  USE File_Utility     , ONLY: File_Exists
  USE String_Utility   , ONLY: StrClean
  USE SpcCoeff_Define_old  , ONLY: INVALID_WMO_SATELLITE_ID, &
                               INVALID_WMO_SENSOR_ID   , &
                               INVALID_SENSOR          , &
                               INVALID_POLARIZATION    , &
                               SpcCoeff_type           , &
                               Associated_SpcCoeff     , &
                               Destroy_SpcCoeff        , &
                               Allocate_SpcCoeff       , &
                               CheckRelease_SpcCoeff   , &
                               Info_SpcCoeff
  USE AntCorr_netCDF_IO_old, ONLY: DefineVar_AntCorr_netCDF, &
                               WriteVar_AntCorr_netCDF , &
                               ReadVar_AntCorr_netCDF
  USE netcdf
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Inquire_SpcCoeff_netCDF
  PUBLIC :: Write_SpcCoeff_netCDF
  PUBLIC :: Read_SpcCoeff_netCDF


  ! -----------------
  ! Module parameters
  ! -----------------

  ! Module RCS Id string
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  ! Literal constants
  REAL(Double), PARAMETER :: ZERO = 0.0_Double
  REAL(Double), PARAMETER :: ONE  = 1.0_Double
  ! Keyword set value
  INTEGER, PARAMETER :: SET = 1
  ! Default msg string length
  INTEGER, PARAMETER :: ML = 1024

  ! Global attribute names. Case sensitive
  CHARACTER(*), PARAMETER :: TITLE_GATTNAME            = 'title' 
  CHARACTER(*), PARAMETER :: HISTORY_GATTNAME          = 'history'
  CHARACTER(*), PARAMETER :: COMMENT_GATTNAME          = 'comment'
  CHARACTER(*), PARAMETER :: RELEASE_GATTNAME          = 'Release'
  CHARACTER(*), PARAMETER :: VERSION_GATTNAME          = 'Version'
  CHARACTER(*), PARAMETER :: SENSOR_ID_GATTNAME        = 'Sensor_Id'
  CHARACTER(*), PARAMETER :: WMO_SATELLITE_ID_GATTNAME = 'WMO_Satellite_Id'
  CHARACTER(*), PARAMETER :: WMO_SENSOR_ID_GATTNAME    = 'WMO_Sensor_Id'
  CHARACTER(*), PARAMETER :: AC_RELEASE_GATTNAME       = 'AC_Release'
  CHARACTER(*), PARAMETER :: AC_VERSION_GATTNAME       = 'AC_Version'

  ! Dimension names
  CHARACTER(*), PARAMETER :: CHANNEL_DIMNAME = 'n_Channels'
  CHARACTER(*), PARAMETER :: FOV_DIMNAME     = 'n_FOVs'     ! Only used if antenna correction data present

  ! Variable names. Case sensitive.
  CHARACTER(*), PARAMETER :: SENSOR_TYPE_VARNAME      = 'Sensor_Type'
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_VARNAME   = 'Sensor_Channel'
  CHARACTER(*), PARAMETER :: POLARIZATION_VARNAME     = 'Polarization'
  CHARACTER(*), PARAMETER :: CHANNEL_FLAG_VARNAME     = 'Channel_Flag'
  CHARACTER(*), PARAMETER :: FREQUENCY_VARNAME        = 'Frequency'
  CHARACTER(*), PARAMETER :: WAVENUMBER_VARNAME       = 'Wavenumber'
  CHARACTER(*), PARAMETER :: PLANCK_C1_VARNAME        = 'Planck_C1'
  CHARACTER(*), PARAMETER :: PLANCK_C2_VARNAME        = 'Planck_C2'
  CHARACTER(*), PARAMETER :: BAND_C1_VARNAME          = 'Band_C1'
  CHARACTER(*), PARAMETER :: BAND_C2_VARNAME          = 'Band_C2'
  CHARACTER(*), PARAMETER :: CBR_VARNAME              = 'Cosmic_Background_Radiance'
  CHARACTER(*), PARAMETER :: SOLAR_IRRADIANCE_VARNAME = 'Solar_Irradiance'

  ! Description attribute.
  CHARACTER(*), PARAMETER :: DESCRIPTION_ATTNAME = 'description'

  CHARACTER(*), PARAMETER :: SENSOR_TYPE_DESCRIPTION      = 'Sensor type to identify uW, IR, VIS, UV, etc sensor channels'
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_DESCRIPTION   = 'List of sensor channel numbers'
  CHARACTER(*), PARAMETER :: POLARIZATION_DESCRIPTION     = 'Polarization type flag.'
  CHARACTER(*), PARAMETER :: CHANNEL_FLAG_DESCRIPTION     = 'Bit position flags for channels'
  CHARACTER(*), PARAMETER :: FREQUENCY_DESCRIPTION        = 'Channel central frequency, f'
  CHARACTER(*), PARAMETER :: WAVENUMBER_DESCRIPTION       = 'Channel central wavenumber, v'
  CHARACTER(*), PARAMETER :: PLANCK_C1_DESCRIPTION        = 'First Planck coefficient, c1.v^3'
  CHARACTER(*), PARAMETER :: PLANCK_C2_DESCRIPTION        = 'Second Planck coefficient, c2.v'
  CHARACTER(*), PARAMETER :: BAND_C1_DESCRIPTION          = 'Polychromatic band correction offset'
  CHARACTER(*), PARAMETER :: BAND_C2_DESCRIPTION          = 'Polychromatic band correction slope'
  CHARACTER(*), PARAMETER :: CBR_DESCRIPTION              = 'Planck radiance for the cosmic background temperature'
  CHARACTER(*), PARAMETER :: SOLAR_IRRADIANCE_DESCRIPTION = 'TOA solar irradiance using Kurucz spectrum'


  ! Long name attribute.
  CHARACTER(*), PARAMETER :: LONGNAME_ATTNAME = 'long_name'

  CHARACTER(*), PARAMETER :: SENSOR_TYPE_LONGNAME      = 'Sensor Type'
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_LONGNAME   = 'Sensor Channel'
  CHARACTER(*), PARAMETER :: POLARIZATION_LONGNAME     = 'Polarization type flag'
  CHARACTER(*), PARAMETER :: CHANNEL_FLAG_LONGNAME     = 'Channel flag'
  CHARACTER(*), PARAMETER :: FREQUENCY_LONGNAME        = 'Frequency'
  CHARACTER(*), PARAMETER :: WAVENUMBER_LONGNAME       = 'Wavenumber'
  CHARACTER(*), PARAMETER :: PLANCK_C1_LONGNAME        = 'Planck C1'
  CHARACTER(*), PARAMETER :: PLANCK_C2_LONGNAME        = 'Planck C2'
  CHARACTER(*), PARAMETER :: BAND_C1_LONGNAME          = 'Band C1'
  CHARACTER(*), PARAMETER :: BAND_C2_LONGNAME          = 'Band C2'
  CHARACTER(*), PARAMETER :: CBR_LONGNAME              = 'Cosmic Background Radiance'
  CHARACTER(*), PARAMETER :: SOLAR_IRRADIANCE_LONGNAME = 'Kurucz Solar Irradiance'



  ! Variable units attribute.
  CHARACTER(*), PARAMETER :: UNITS_ATTNAME = 'units'

  CHARACTER(*), PARAMETER :: SENSOR_TYPE_UNITS      = 'N/A'
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_UNITS   = 'N/A'
  CHARACTER(*), PARAMETER :: POLARIZATION_UNITS     = 'N/A'
  CHARACTER(*), PARAMETER :: CHANNEL_FLAG_UNITS     = 'N/A'
  CHARACTER(*), PARAMETER :: FREQUENCY_UNITS        = 'Gigahertz (GHz)'
  CHARACTER(*), PARAMETER :: WAVENUMBER_UNITS       = 'Inverse centimetres (cm^-1)'
  CHARACTER(*), PARAMETER :: PLANCK_C1_UNITS        = 'mW/(m^2.sr.cm^-1)'
  CHARACTER(*), PARAMETER :: PLANCK_C2_UNITS        = 'Kelvin (K)'
  CHARACTER(*), PARAMETER :: BAND_C1_UNITS          = 'Kelvin (K)'
  CHARACTER(*), PARAMETER :: BAND_C2_UNITS          = 'K/K'
  CHARACTER(*), PARAMETER :: CBR_UNITS              = 'mW/(m^2.sr.cm^-1)'
  CHARACTER(*), PARAMETER :: SOLAR_IRRADIANCE_UNITS = 'mW/(m^2.cm^-1)'

  ! Variable _FillValue attribute.
  CHARACTER(*), PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'

  INTEGER(Long), PARAMETER :: SENSOR_TYPE_FILLVALUE      = INVALID_SENSOR
  INTEGER(Long), PARAMETER :: SENSOR_CHANNEL_FILLVALUE   = 0
  INTEGER(Long), PARAMETER :: POLARIZATION_FILLVALUE     = INVALID_POLARIZATION        
  INTEGER(Long), PARAMETER :: CHANNEL_FLAG_FILLVALUE     = 0
  REAL(Double),  PARAMETER :: FREQUENCY_FILLVALUE        = ZERO
  REAL(Double),  PARAMETER :: WAVENUMBER_FILLVALUE       = ZERO
  REAL(Double),  PARAMETER :: PLANCK_C1_FILLVALUE        = ZERO
  REAL(Double),  PARAMETER :: PLANCK_C2_FILLVALUE        = ZERO
  REAL(Double),  PARAMETER :: BAND_C1_FILLVALUE          = ZERO
  REAL(Double),  PARAMETER :: BAND_C2_FILLVALUE          = ZERO
  REAL(Double),  PARAMETER :: CBR_FILLVALUE              = ZERO
  REAL(Double),  PARAMETER :: SOLAR_IRRADIANCE_FILLVALUE = ZERO

  ! Variable netCDF datatypes
  INTEGER, PARAMETER :: SENSOR_TYPE_TYPE      = NF90_INT
  INTEGER, PARAMETER :: SENSOR_CHANNEL_TYPE   = NF90_INT
  INTEGER, PARAMETER :: POLARIZATION_TYPE     = NF90_INT
  INTEGER, PARAMETER :: CHANNEL_FLAG_TYPE     = NF90_INT
  INTEGER, PARAMETER :: FREQUENCY_TYPE        = NF90_DOUBLE
  INTEGER, PARAMETER :: WAVENUMBER_TYPE       = NF90_DOUBLE
  INTEGER, PARAMETER :: PLANCK_C1_TYPE        = NF90_DOUBLE
  INTEGER, PARAMETER :: PLANCK_C2_TYPE        = NF90_DOUBLE
  INTEGER, PARAMETER :: BAND_C1_TYPE          = NF90_DOUBLE
  INTEGER, PARAMETER :: BAND_C2_TYPE          = NF90_DOUBLE
  INTEGER, PARAMETER :: CBR_TYPE              = NF90_DOUBLE
  INTEGER, PARAMETER :: SOLAR_IRRADIANCE_TYPE = NF90_DOUBLE


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
!       Inquire_SpcCoeff_netCDF
!
! PURPOSE:
!       Function to inquire a netCDF SpcCoeff format file to obtain the
!       dimensions and global attributes.
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_SpcCoeff_netCDF( NC_Filename                      , &
!                                               n_Channels      =n_Channels      , &
!                                               n_FOVs          =n_FOVs          , &
!                                               Release         =Release         , &
!                                               Version         =Version         , &
!                                               Sensor_Id       =Sensor_Id       , &
!                                               WMO_Satellite_Id=WMO_Satellite_Id, &
!                                               WMO_Sensor_Id   =WMO_Sensor_Id   , &
!                                               AC_Release      =AC_Release      , &
!                                               AC_Version      =AC_Version      , &
!                                               Title           =Title           , &
!                                               History         =History         , &
!                                               Comment         =Comment         , &
!                                               RCS_Id          =RCS_Id          , &
!                                               Message_Log     =Message_Log       )
!
! INPUT ARGUMENTS:
!       NC_Filename:        Character string specifying the name of the netCDF
!                           format SpcCoeff data file to inquire.
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
!       n_FOVs:             The number of fields-of-view for the sensor.
!                           If specified and == 0, then this dimension is
!                           not present in the netCDF SpcCoeff file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:            The release number of the netCDF SpcCoeff file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:            The version number of the netCDF SpcCoeff file.
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
!       AC_Release:         The release number of the netCDF AntCorr file from
!                           which the antenna correction data, if present, was
!                           obtained
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       AC_Version:         The version number of the netCDF AntCorr file from
!                           which the antenna correction data, if present, was
!                           obtained
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:              Character string written into the TITLE global
!                           attribute field of the netCDF SpcCoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the netCDF SpcCoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF SpcCoeff file.
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
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           Message_Handler module.
!                           If == SUCCESS the netCDF file inquiry was successful
!                              == FAILURE an error occurred reading any of the
!                                         requested data
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Inquire_SpcCoeff_netCDF( NC_Filename     , &  ! Input
                                    n_Channels      , &  ! Optional output
                                    n_FOVs          , &  ! Optional output
                                    Release         , &  ! Optional output
                                    Version         , &  ! Optional output
                                    Sensor_Id       , &  ! Optional output
                                    WMO_Satellite_Id, &  ! Optional output
                                    WMO_Sensor_Id   , &  ! Optional output
                                    AC_Release      , &  ! Optional output
                                    AC_Version      , &  ! Optional output
                                    Title           , &  ! Optional output
                                    History         , &  ! Optional output
                                    Comment         , &  ! Optional output
                                    RCS_Id          , &  ! Revision control
                                    Message_Log     ) &  ! Error messaging
                                  RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Channels
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_FOVs    
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release         
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Sensor_Id       
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Satellite_Id
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Sensor_Id   
    INTEGER     , OPTIONAL, INTENT(OUT) :: AC_Release         
    INTEGER     , OPTIONAL, INTENT(OUT) :: AC_Version         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Inquire_SpcCoeff_netCDF'
    ! Function variables
    CHARACTER(ML) :: msg
    INTEGER :: NF90_Status
    INTEGER :: NC_FileID
    INTEGER :: n_Dims, DimId
    TYPE(SpcCoeff_type) :: SpcCoeff
    
    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID


    ! Open the file
    NF90_Status = NF90_OPEN( NC_Filename,NF90_NOWRITE,NC_FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error opening '//TRIM(NC_Filename)//' for read access - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Get the dimensions
    ! ..How many are there?
    NF90_Status = NF90_INQUIRE( NC_FileID,nDimensions=n_Dims )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error obtaining dimension information from '//TRIM(NC_Filename)//' - '//&
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..The number of spectral channels
    NF90_Status = NF90_INQ_DIMID( NC_FileId,CHANNEL_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//CHANNEL_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( NC_FileId,DimId,Len=SpcCoeff%n_Channels )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//CHANNEL_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..The number of sensor fields-of-view
    IF ( n_Dims > 1 ) THEN
      NF90_Status = NF90_INQ_DIMID( NC_FileId,FOV_DIMNAME,DimId )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error inquiring dimension ID for '//FOV_DIMNAME//' - '// &
              TRIM(NF90_STRERROR( NF90_Status ))
        CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
      END IF
      NF90_Status = NF90_INQUIRE_DIMENSION( NC_FileId,DimId,Len=SpcCoeff%AC%n_FOVs )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error reading dimension value for '//FOV_DIMNAME//' - '// &
              TRIM(NF90_STRERROR( NF90_Status ))
        CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
      END IF
    END IF


    ! Get the global attributes
    Error_Status = ReadGAtts( NC_Filename                               , &
                              NC_FileID                                 , &
                              Release         =SpcCoeff%Release         , &
                              Version         =SpcCoeff%Version         , &
                              Sensor_Id       =SpcCoeff%Sensor_Id       , &
                              WMO_Satellite_Id=SpcCoeff%WMO_Satellite_Id, &
                              WMO_Sensor_Id   =SpcCoeff%WMO_Sensor_Id   , &
                              AC_Release      =SpcCoeff%AC%Release      , &
                              AC_Version      =SpcCoeff%AC%Version      , &
                              Title           =Title                    , &
                              History         =History                  , &
                              Comment         =Comment                  , &
                              Message_Log     =Message_Log                )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error reading global attribute from '//TRIM(NC_Filename)
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Close the file
    NF90_Status = NF90_CLOSE( NC_FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error closing input file - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Set the return values
    IF ( PRESENT(n_Channels      ) ) n_Channels       = SpcCoeff%n_Channels
    IF ( PRESENT(n_FOVs          ) ) n_FOVs           = SpcCoeff%AC%n_FOVs
    IF ( PRESENT(Release         ) ) Release          = SpcCoeff%Release
    IF ( PRESENT(Version         ) ) Version          = SpcCoeff%Version
    IF ( PRESENT(Sensor_Id       ) ) Sensor_Id        = SpcCoeff%Sensor_Id
    IF ( PRESENT(WMO_Satellite_Id) ) WMO_Satellite_Id = SpcCoeff%WMO_Satellite_Id
    IF ( PRESENT(WMO_Sensor_Id   ) ) WMO_Sensor_Id    = SpcCoeff%WMO_Sensor_Id   
    IF ( PRESENT(AC_Release      ) ) AC_Release       = SpcCoeff%AC%Release
    IF ( PRESENT(AC_Version      ) ) AC_Version       = SpcCoeff%AC%Version

  CONTAINS
  
    SUBROUTINE Inquire_CleanUp( Close_File )
      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File ) THEN
          NF90_Status = NF90_CLOSE( NC_FileId )
          IF ( NF90_Status /= NF90_NOERR ) &
            msg = TRIM(msg)//'; Error closing input file during error cleanup.'
        END IF
      END IF
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION Inquire_SpcCoeff_netCDF


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Write_SpcCoeff_netCDF
!
! PURPOSE:
!       Function to write SpcCoeff data to a netCDF format SpcCoeff file.
!
! CALLING SEQUENCE:
!       Error_Status = Write_SpcCoeff_netCDF( NC_Filename            , &
!                                             SpcCoeff               , &
!                                             Quiet      =Quiet      , &
!                                             Title      =Title      , &
!                                             History    =History    , &
!                                             Comment    =Comment    , &
!                                             RCS_Id     =RCS_Id     , &
!                                             Message_Log=Message_Log  )
!
! INPUT ARGUMENTS:
!       NC_Filename:  Character string specifying the name of the netCDF
!                     format SpcCoeff data file to write data into.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       SpcCoeff:     Structure containing the spectral coefficient data
!                     to write to file.
!                     UNITS:      N/A
!                     TYPE:       TYPE(SpcCoeff_type)
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
!       Title:        Character string written into the TITLE global
!                     attribute field of the netCDF SpcCoeff file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:      Character string written into the HISTORY global
!                     attribute field of the netCDF SpcCoeff file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:      Character string written into the COMMENT global
!                     attribute field of the netCDF SpcCoeff file.
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

  FUNCTION Write_SpcCoeff_netCDF( NC_Filename , &  ! Input
                                  SpcCoeff    , &  ! Input
                                  Quiet       , &  ! Optional input
                                  Title       , &  ! Optional input
                                  History     , &  ! Optional input
                                  Comment     , &  ! Optional input
                                  RCS_Id      , &  ! Revision control
                                  Message_Log ) &  ! Error messaging
                                RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    TYPE(SpcCoeff_type)   , INTENT(IN)  :: SpcCoeff
    INTEGER     , OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title  
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_SpcCoeff_netCDF'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
    INTEGER :: NC_FileId
    INTEGER :: NF90_Status
    INTEGER :: AC_Release, AC_Version
    INTEGER :: VarId

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    Noisy = .TRUE.
    IF ( PRESENT(Quiet) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF

    IF ( .NOT. Associated_SpcCoeff( SpcCoeff ) ) THEN
      msg = 'Some or all INPUT SpcCoeff pointer members are NOT associated.'
      CALL Write_Cleanup(); RETURN
    END IF

    Error_Status = CheckRelease_SpcCoeff( SpcCoeff, Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'SpcCoeff Release check failed.'
      CALL Write_Cleanup(); RETURN
    END IF


    ! Set the antenna correction GAtts
    IF ( SpcCoeff%AC_Present ) THEN
      AC_Release = SpcCoeff%AC%Release
      AC_Version = SpcCoeff%AC%Version
    ELSE
      AC_Release = -1
      AC_Version = -1
    END IF
    

    ! Create the output data file
    Error_Status = CreateFile( NC_Filename                               , &  ! Input
                               SpcCoeff%n_Channels                       , &  ! Input
                               NC_FileId                                 , &  ! Output
                               n_FOVs          =SpcCoeff%AC%n_FOVs       , &  ! Optional input
                               Version         =SpcCoeff%Version         , &  ! Optional input
                               Sensor_Id       =SpcCoeff%Sensor_Id       , &  ! Optional input
                               WMO_Satellite_Id=SpcCoeff%WMO_Satellite_Id, &  ! Optional input
                               WMO_Sensor_Id   =SpcCoeff%WMO_Sensor_Id   , &  ! Optional input
                               AC_Release      =AC_Release               , &  ! Optional input
                               AC_Version      =AC_Version               , &  ! Optional input
                               Title           =Title                    , &  ! Optional input
                               History         =History                  , &  ! Optional input
                               Comment         =Comment                  , &  ! Optional input
                               Message_Log     =Message_Log                )  ! Error messaging
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error creating output file '//TRIM(NC_Filename)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the SpcCoeff data
    ! ..Sensor type to identify uW, IR, VIS, UV, etc sensor channels
    NF90_Status = NF90_INQ_VARID( NC_FileId,SENSOR_TYPE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//SENSOR_TYPE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,SpcCoeff%Sensor_Type )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//SENSOR_TYPE_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..List of sensor channel numbers
    NF90_Status = NF90_INQ_VARID( NC_FileId,SENSOR_CHANNEL_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//SENSOR_CHANNEL_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,SpcCoeff%Sensor_Channel )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//SENSOR_CHANNEL_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..Polarization type flag.
    NF90_Status = NF90_INQ_VARID( NC_FileId,POLARIZATION_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//POLARIZATION_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,SpcCoeff%Polarization )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//POLARIZATION_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..Bit position flags for channels
    NF90_Status = NF90_INQ_VARID( NC_FileId,CHANNEL_FLAG_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//CHANNEL_FLAG_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,SpcCoeff%Channel_Flag )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//CHANNEL_FLAG_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..Channel central frequency, f
    NF90_Status = NF90_INQ_VARID( NC_FileId,FREQUENCY_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//FREQUENCY_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,SpcCoeff%Frequency )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//FREQUENCY_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..Channel central wavenumber, v
    NF90_Status = NF90_INQ_VARID( NC_FileId,WAVENUMBER_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//WAVENUMBER_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,SpcCoeff%Wavenumber )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//WAVENUMBER_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..First Planck coefficient, c1.v^3
    NF90_Status = NF90_INQ_VARID( NC_FileId,PLANCK_C1_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//PLANCK_C1_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,SpcCoeff%Planck_C1 )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//PLANCK_C1_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..Second Planck coefficient, c2.v
    NF90_Status = NF90_INQ_VARID( NC_FileId,PLANCK_C2_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//PLANCK_C2_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,SpcCoeff%Planck_C2 )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//PLANCK_C2_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..Polychromatic band correction offset
    NF90_Status = NF90_INQ_VARID( NC_FileId,BAND_C1_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//BAND_C1_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,SpcCoeff%Band_C1 )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//BAND_C1_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..Polychromatic band correction slope
    NF90_Status = NF90_INQ_VARID( NC_FileId,BAND_C2_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//BAND_C2_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,SpcCoeff%Band_C2 )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//BAND_C2_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..Planck radiance for the cosmic background temperature
    NF90_Status = NF90_INQ_VARID( NC_FileId,CBR_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//CBR_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,SpcCoeff%Cosmic_Background_Radiance )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//CBR_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..TOA solar irradiance using Kurucz spectrum
    NF90_Status = NF90_INQ_VARID( NC_FileId,SOLAR_IRRADIANCE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//SOLAR_IRRADIANCE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,SpcCoeff%Solar_Irradiance )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//SOLAR_IRRADIANCE_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Write the antenna correction data
    IF ( SpcCoeff%AC_Present ) THEN
      Error_Status = WriteVar_AntCorr_netCDF( NC_Filename            , &
                                              NC_FileID              , &
                                              SpcCoeff%AC            , &
                                              Message_Log=Message_Log  )
      IF ( Error_Status /= SUCCESS ) THEN
        msg = 'Error writing AC variables to '//TRIM(NC_Filename)
        CALL Write_Cleanup(); RETURN
      END IF
    END IF


    ! Close the file
    NF90_Status = NF90_CLOSE( NC_FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error closing output file - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Output an info message
    IF ( Noisy ) THEN
      CALL Info_SpcCoeff( SpcCoeff, msg )
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
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
    END SUBROUTINE Write_CleanUp

  END FUNCTION Write_SpcCoeff_netCDF


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Read_SpcCoeff_netCDF
!
! PURPOSE:
!       Function to read data from a netCDF format SpcCoeff file.
!
! CALLING SEQUENCE:
!       Error_Status = Read_SpcCoeff_netCDF( NC_Filename            , &
!                                            SpcCoeff               , &
!                                            Quiet      =Quiet      , &
!                                            Title      =Title      , &
!                                            History    =History    , &
!                                            Comment    =Comment    , &
!                                            RCS_Id     =RCS_Id     , &
!                                            Message_Log=Message_Log  )
!
! INPUT ARGUMENTS:
!       NC_Filename:     Character string specifying the name of the netCDF
!                        format SpcCoeff data file to read.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       SpcCoeff:        Structure to contain the spectral coefficient data read
!                        from the file.
!                        UNITS:      N/A
!                        TYPE:       SpcCoeff_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:           Set this keyword to suppress information msgs being
!                        printed to standard output (or the msg log file if
!                        the Message_Log optional argument is used.) By default,
!                        information msgs are printed.
!                        If QUIET = 0, information msgs are OUTPUT.
!                           QUIET = 1, information msgs are SUPPRESSED.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:     Character string specifying a filename in which any
!                        msgs will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output msgs to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Title:           Character string written into the TITLE global
!                        attribute field of the netCDF SpcCoeff file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       History:         Character string written into the HISTORY global
!                        attribute field of the netCDF SpcCoeff file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Comment:         Character string written into the COMMENT global
!                        attribute field of the netCDF SpcCoeff file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       RCS_Id:          Character string containing the Revision Control
!                        System Id field for the module.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the netCDF file read was successful
!                           == FAILURE an unrecoverable read error occurred.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output SpcCoeff argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined on
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Read_SpcCoeff_netCDF( NC_Filename, &  ! Input
                                 SpcCoeff   , &  ! Output
                                 Quiet      , &  ! Optional input
                                 Title      , &  ! Optional output
                                 History    , &  ! Optional output
                                 Comment    , &  ! Optional output
                                 RCS_Id     , &  ! Revision control
                                 Message_Log) &  ! Error messaging
                               RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)     :: NC_Filename
    TYPE(SpcCoeff_type)   , INTENT(IN OUT) :: SpcCoeff
    INTEGER,      OPTIONAL, INTENT(IN)     :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: Title  
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_SpcCoeff_netCDF'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
    INTEGER :: NF90_Status
    INTEGER :: NC_FileID
    INTEGER :: n_Channels, n_FOVs
    INTEGER :: VarId


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


    ! Allocate the structure for the netCDF read
    ! ..Read the dimension values
    Error_Status = Inquire_SpcCoeff_netCDF( NC_Filename            , &
                                            n_Channels =n_Channels , &
                                            n_FOVs     =n_FOVs     , &
                                            Message_Log=Message_Log  )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error obtaining SpcCoeff dimensions from '//TRIM(NC_Filename)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ..Allocate the structure
    Error_Status = Allocate_SpcCoeff( n_Channels             , &  ! Input
                                      SpcCoeff               , &  ! Output
                                      n_FOVs     =n_FOVs     , &  ! Optional Input
                                      Message_Log=Message_Log  )  ! Error messaging
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error occurred allocating SpcCoeff structure.'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Open the netCDF file for reading
    NF90_Status = NF90_OPEN( NC_Filename,NF90_NOWRITE,NC_FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error opening '//TRIM(NC_Filename)//' for read access - '//&
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read and assign the remaining global attributes
    Error_Status = ReadGAtts( NC_Filename                               , &
                              NC_FileID                                 , &
                              Release         =SpcCoeff%Release         , &
                              Version         =SpcCoeff%Version         , &
                              Sensor_Id       =SpcCoeff%Sensor_Id       , &
                              WMO_Satellite_Id=SpcCoeff%WMO_Satellite_Id, &
                              WMO_Sensor_Id   =SpcCoeff%WMO_Sensor_Id   , &
                              AC_Release      =SpcCoeff%AC%Release      , &
                              AC_Version      =SpcCoeff%AC%Version      , &
                              Title           =Title                    , &
                              History         =History                  , &
                              Comment         =Comment                  , &
                              Message_Log     =Message_Log                )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error reading global attribute from '//TRIM(NC_Filename)
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    SpcCoeff%AC%Sensor_Id        = SpcCoeff%Sensor_Id       
    SpcCoeff%AC%WMO_Satellite_Id = SpcCoeff%WMO_Satellite_Id
    SpcCoeff%AC%WMO_Sensor_Id    = SpcCoeff%WMO_Sensor_Id   


    ! Check the release
    Error_Status = CheckRelease_SpcCoeff( SpcCoeff,Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'SpcCoeff Release check failed for '//TRIM(NC_Filename)
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Read the spectral coefficient data
    ! ..Sensor type to identify uW, IR, VIS, UV, etc sensor channels
    NF90_Status = NF90_INQ_VARID( NC_FileId,SENSOR_TYPE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//SENSOR_TYPE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,SpcCoeff%Sensor_Type )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//SENSOR_TYPE_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..List of sensor channel numbers
    NF90_Status = NF90_INQ_VARID( NC_FileId,SENSOR_CHANNEL_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//SENSOR_CHANNEL_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,SpcCoeff%Sensor_Channel )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//SENSOR_CHANNEL_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..Polarization type flag.
    NF90_Status = NF90_INQ_VARID( NC_FileId,POLARIZATION_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//POLARIZATION_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,SpcCoeff%Polarization )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//POLARIZATION_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..Bit position flags for channels
    NF90_Status = NF90_INQ_VARID( NC_FileId,CHANNEL_FLAG_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//CHANNEL_FLAG_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,SpcCoeff%Channel_Flag )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//CHANNEL_FLAG_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..Channel central frequency, f
    NF90_Status = NF90_INQ_VARID( NC_FileId,FREQUENCY_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//FREQUENCY_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,SpcCoeff%Frequency )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//FREQUENCY_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..Channel central wavenumber, v
    NF90_Status = NF90_INQ_VARID( NC_FileId,WAVENUMBER_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//WAVENUMBER_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,SpcCoeff%Wavenumber )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//WAVENUMBER_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..First Planck coefficient, c1.v^3
    NF90_Status = NF90_INQ_VARID( NC_FileId,PLANCK_C1_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//PLANCK_C1_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,SpcCoeff%Planck_C1 )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//PLANCK_C1_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..Second Planck coefficient, c2.v
    NF90_Status = NF90_INQ_VARID( NC_FileId,PLANCK_C2_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//PLANCK_C2_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,SpcCoeff%Planck_C2 )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//PLANCK_C2_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..Polychromatic band correction offset
    NF90_Status = NF90_INQ_VARID( NC_FileId,BAND_C1_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//BAND_C1_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,SpcCoeff%Band_C1 )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//BAND_C1_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..Polychromatic band correction slope
    NF90_Status = NF90_INQ_VARID( NC_FileId,BAND_C2_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//BAND_C2_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,SpcCoeff%Band_C2 )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//BAND_C2_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..Planck radiance for the cosmic background temperature
    NF90_Status = NF90_INQ_VARID( NC_FileId,CBR_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//CBR_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,SpcCoeff%Cosmic_Background_Radiance )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//CBR_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..TOA solar irradiance using Kurucz spectrum
    NF90_Status = NF90_INQ_VARID( NC_FileId,SOLAR_IRRADIANCE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//SOLAR_IRRADIANCE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,SpcCoeff%Solar_Irradiance )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//SOLAR_IRRADIANCE_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Read the antenna correction data if required
    IF ( SpcCoeff%AC_Present ) THEN
      Error_Status = ReadVar_AntCorr_netCDF( NC_Filename            , &
                                             NC_FileID              , &
                                             SpcCoeff%AC            , &
                                             Message_Log=Message_Log  )
      IF ( Error_Status /= SUCCESS ) THEN
        msg = 'Error reading AC variables from '//TRIM(NC_Filename)
        CALL Read_Cleanup(); RETURN
      END IF
    END IF


    ! Close the file
    NF90_Status = NF90_CLOSE( NC_FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error closing input file - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF


    ! Output an info message
    IF ( Noisy ) THEN
      CALL Info_SpcCoeff( SpcCoeff, msg )
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
            msg = TRIM(msg)//'; Error closing input file during error cleanup- '//&
                  TRIM(NF90_STRERROR( NF90_Status ))
        END IF
      END IF
      ! Destroy the structure if necessary
      IF ( Associated_SpcCoeff( SpcCoeff ) ) THEN
        Error_Status = Destroy_SpcCoeff( SpcCoeff, Message_Log=Message_Log )
        IF ( Error_Status /= SUCCESS ) &
          msg = TRIM(msg)//'; Error destroying SpcCoeff structure during error cleanup.'
      END IF
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
    END SUBROUTINE Read_CleanUp

  END FUNCTION Read_SpcCoeff_netCDF


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
!       Write_SpcCoeff_GAtts
!
! PURPOSE:
!       Function to write the global attributes to a netCDF SpcCoeff data file.
!
! CALLING SEQUENCE:
!       Error_Status = WriteGAtts( NC_Filename                      , &
!                                  NC_FileID                        , &
!                                  Release         =Release         , &
!                                  Version         =Version         , &
!                                  Sensor_Id       =Sensor_Id       , &
!                                  WMO_Satellite_Id=WMO_Satellite_Id, &
!                                  WMO_Sensor_Id   =WMO_Sensor_Id   , &
!                                  AC_Release      =AC_Release      , &
!                                  AC_Version      =AC_Version      , &
!                                  Title           =Title           , &
!                                  History         =History         , &
!                                  Comment         =Comment         , &
!                                  Message_Log     =Message_Log       )
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF SpcCoeff format data file to write to.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:        NetCDF file ID number.
!                         function.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Release:          The release number of the netCDF SpcCoeff file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Version:          The version number of the netCDF SpcCoeff file.
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
!       AC_Release:       The release number of the netCDF AntCorr file from
!                         which the antenna correction data, if present, was
!                         obtained
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       AC_Version:       The version number of the netCDF AntCorr file from
!                         which the antenna correction data, if present, was
!                         obtained
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF SpcCoeff file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF SpcCoeff file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF SpcCoeff file.
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
!                         If == SUCCESS the global attribute write was successful
!                            == FAILURE an error occurred writing the supplied
!                                       global attribute(s).
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
                       AC_Release      , &  ! Optional input
                       AC_Version      , &  ! Optional input
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
    INTEGER     , OPTIONAL, INTENT(IN) :: AC_Release         
    INTEGER     , OPTIONAL, INTENT(IN) :: AC_Version         
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
    TYPE(SpcCoeff_type) :: SpcCoeff_Default

    ! Set up
    Error_Status = SUCCESS
    msg = ' '

    ! Mandatory global attributes
    ! ..Software ID
    GAttName = WRITE_MODULE_HISTORY_GATTNAME
    NF90_Status = NF90_PUT_ATT( NC_FileID,NF90_GLOBAL,TRIM(GAttName),MODULE_RCS_ID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF
    ! ..Creation date
    CALL DATE_AND_TIME( cdate, ctime, czone )
    GAttName = CREATION_DATE_AND_TIME_GATTNAME
    NF90_Status = NF90_PUT_ATT( NC_FileID,NF90_GLOBAL,TRIM(GAttName), &
                                cdate(1:4)//'/'//cdate(5:6)//'/'//cdate(7:8)//', '// &
                                ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//' '// &
                                czone//'UTC' )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF
    ! ..The Release
    GAttName = RELEASE_GATTNAME
    NF90_Status = NF90_PUT_ATT( NC_FileID,NF90_GLOBAL,TRIM(GAttName),SpcCoeff_Default%Release )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF


    ! Optional global attributes
    ! ..The Version
    IF ( PRESENT(Version) ) THEN
      Ver = Version
    ELSE
      Ver = SpcCoeff_Default%Version
    END IF
    GAttName = VERSION_GATTNAME
    NF90_Status = NF90_PUT_ATT( NC_FileID,NF90_GLOBAL,TRIM(GAttName),Ver )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF
    ! ..The Sensor_Id
    IF ( PRESENT(Sensor_Id) ) THEN
      GAttName = SENSOR_ID_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID,NF90_GLOBAL,TRIM(GAttName),Sensor_Id )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ..The WMO_Satellite_Id
    IF ( PRESENT(WMO_Satellite_Id) ) THEN
      GAttName = WMO_SATELLITE_ID_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID,NF90_GLOBAL,TRIM(GAttName),WMO_Satellite_Id )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ..The WMO_Sensor_Id
    IF ( PRESENT(WMO_Sensor_Id) ) THEN
      GAttName = WMO_SENSOR_ID_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID,NF90_GLOBAL,TRIM(GAttName),WMO_Sensor_Id )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ..The antenna correction Release
    IF ( PRESENT(AC_Release) ) THEN
      GAttName = AC_RELEASE_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID,NF90_GLOBAL,TRIM(GAttName),AC_Release )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ..The antenna correction Version
    IF ( PRESENT(AC_Version) ) THEN
      GAttName = AC_VERSION_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID,NF90_GLOBAL,TRIM(GAttName),AC_Version )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ..The Title
    IF ( PRESENT(Title) ) THEN
      GAttName = TITLE_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID,NF90_GLOBAL,TRIM(GAttName),TRIM(Title) )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ..The History
    IF ( PRESENT(History) ) THEN
      GAttName = HISTORY_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID,NF90_GLOBAL,TRIM(GAttName),TRIM(History) )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ..The Comment
    IF ( PRESENT(Comment) ) THEN
      GAttName = COMMENT_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID,NF90_GLOBAL,TRIM(GAttName),TRIM(Comment) )
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
      ! Set error status and print error message
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
!       Function to read the global attributes from a netCDF SpcCoeff
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
!                                 AC_Release      =AC_Release      , &
!                                 AC_Version      =AC_Version      , &
!                                 Title           =Title           , &
!                                 History         =History         , &
!                                 Comment         =Comment         , &
!                                 Message_Log     =Message_Log       )
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF SpcCoeff format data file to read from.
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
!       Release:          The release number of the netCDF SpcCoeff file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Version:          The version number of the netCDF SpcCoeff file.
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
!       AC_Release:       The release number of the netCDF AntCorr file from
!                         which the antenna correction data, if present, was
!                         obtained
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       AC_Version:       The version number of the netCDF AntCorr file from
!                         which the antenna correction data, if present, was
!                         obtained
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF SpcCoeff file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF SpcCoeff file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF SpcCoeff file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the global attribute read was successful.
!                        == WARNING an error occurred reading the requested
!                           global attributes.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION ReadGAtts( NC_Filename     , &  ! Input
                      NC_FileID       , &  ! Input
                      Release         , &  ! Optional output
                      Version         , &  ! Optional output
                      Sensor_Id       , &  ! Optional output
                      WMO_Satellite_Id, &  ! Optional output
                      WMO_Sensor_Id   , &  ! Optional output
                      AC_Release      , &  ! Optional output
                      AC_Version      , &  ! Optional output
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
    INTEGER     , OPTIONAL, INTENT(OUT) :: AC_Release         
    INTEGER     , OPTIONAL, INTENT(OUT) :: AC_Version         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ReadGAtts'
    ! Local variables
    CHARACTER(ML)   :: GAttName
    CHARACTER(5000) :: GAttString
    INTEGER :: Rel
    INTEGER :: NF90_Status
    TYPE(SpcCoeff_type) :: SpcCoeff_Default

    ! Set up
    Error_Status = SUCCESS

    ! The mandatory GAtts for checking
    ! ..The Release
    GAttName = RELEASE_GATTNAME
    NF90_Status = NF90_GET_ATT( NC_FileID,NF90_GLOBAL,TRIM(GAttName),Rel )
    IF ( NF90_Status /= NF90_NOERR .OR. Rel /= SpcCoeff_Default%Release) THEN
      CALL ReadGAtts_Cleanup(); RETURN
    END IF
    IF ( PRESENT(Release) ) Release = SpcCoeff_Default%Release


    ! The optional GAtts
    ! ..The Version
    IF ( PRESENT(Version) ) THEN
      GAttName = VERSION_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID,NF90_GLOBAL,TRIM(GAttName),Version )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ..The Sensor_Id
    IF ( PRESENT(Sensor_Id) ) THEN
      GAttString = ' '; Sensor_Id = ' '
      GAttName = SENSOR_ID_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID,NF90_GLOBAL,TRIM(GAttName),GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL StrClean( GAttString )
      Sensor_Id = GAttString(1:MIN( LEN(Sensor_Id), LEN_TRIM(GAttString) ))
    END IF
    ! ..The WMO_Satellite_Id
    IF ( PRESENT(WMO_Satellite_Id) ) THEN
      GAttName = WMO_SATELLITE_ID_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID,NF90_GLOBAL,TRIM(GAttName),WMO_Satellite_Id )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ..The WMO_Sensor_Id
    IF ( PRESENT(WMO_Sensor_Id) ) THEN
      GAttName = WMO_SENSOR_ID_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID,NF90_GLOBAL,TRIM(GAttName),WMO_Sensor_Id )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ..The antenna correction Release
    IF ( PRESENT(AC_Release) ) THEN
      GAttName = AC_RELEASE_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID,NF90_GLOBAL,TRIM(GAttName),AC_Release )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ..The antenna correction Version
    IF ( PRESENT(AC_Version) ) THEN
      GAttName = AC_VERSION_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID,NF90_GLOBAL,TRIM(GAttName),AC_Version )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF
    !.. The Title
    IF ( PRESENT(Title) ) THEN
      GAttString = ' '; Title = ' '
      GAttName = TITLE_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID,NF90_GLOBAL,TRIM(GAttName),GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL StrClean( GAttString )
      Title = GAttString(1:MIN( LEN(Title), LEN_TRIM(GAttString) ))
    END IF
    ! ..The History
    IF ( PRESENT(History) ) THEN
      GAttString = ' '; History = ' '
      GAttName = HISTORY_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID,NF90_GLOBAL,TRIM(GAttName),GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL StrClean( GAttString )
      History = GAttString(1:MIN( LEN(History), LEN_TRIM(GAttString) ))
    END IF
    ! ..The Comment
    IF ( PRESENT(Comment) ) THEN
      GAttString = ' '; Comment = ' '
      GAttName = COMMENT_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID,NF90_GLOBAL,TRIM(GAttName),GAttString )
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
!       CreateFile
!
! PURPOSE:
!       Function to create the netCDF format SpcCoeff file
!
! CALLING SEQUENCE:
!       Error_Status = CreateFile( NC_Filename                      , &
!                                  n_Channels                       , &
!                                  NC_FileID                        , &
!                                  n_FOVs,         =n_FOVs          , &
!                                  Version         =Version         , &
!                                  Sensor_Id       =Sensor_Id       , &
!                                  WMO_Satellite_Id=WMO_Satellite_Id, &
!                                  WMO_Sensor_Id   =WMO_Sensor_Id   , &
!                                  AC_Release      =AC_Release      , &
!                                  AC_Version      =AC_Version      , &
!                                  Title           =Title           , &
!                                  History         =History         , &
!                                  Comment         =Comment         , &
!                                  Message_Log     =Message_Log       )
!
! INPUT ARGUMENTS:
!       NC_Filename:        Character string specifying the name of the
!                           netCDF SpcCoeff format data file to write to.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Channels:         The number of sensor channels dimension
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
!       n_FOVs:             The number of fields-of-view dimension for
!                           antenna correction data
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Version:            The version number of the netCDF SpcCoeff file.
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
!       AC_Release:         The release number of the netCDF AntCorr file from
!                           which the antenna correction data, if present, was
!                           obtained
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       AC_Version:         The version number of the netCDF AntCorr file from
!                           which the antenna correction data, if present, was
!                           obtained
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:              Character string written into the TITLE global
!                           attribute field of the netCDF SpcCoeff file.
!                           Should contain a succinct description of what
!                           is in the netCDF datafile.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the netCDF SpcCoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF SpcCoeff file.
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
!                           UNITS:      N/A                                            
!                           TYPE:       INTEGER                                        
!                           DIMENSION:  Scalar                                         
!
!------------------------------------------------------------------------------

  FUNCTION CreateFile( NC_Filename     , &  ! Input
                       n_Channels      , &  ! Input
                       NC_FileID       , &  ! Output
                       n_FOVs          , &  ! Optional input
                       Version         , &  ! Optional input
                       Sensor_Id       , &  ! Optional input
                       WMO_Satellite_Id, &  ! Optional input
                       WMO_Sensor_Id   , &  ! Optional input
                       AC_Release      , &  ! Optional input
                       AC_Version      , &  ! Optional input
                       Title           , &  ! Optional input
                       History         , &  ! Optional input
                       Comment         , &  ! Optional input
                       Message_Log     ) &  ! Error messaging
                     RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)  :: NC_Filename
    INTEGER               , INTENT(IN)  :: n_Channels
    INTEGER               , INTENT(OUT) :: NC_FileID
    INTEGER     , OPTIONAL, INTENT(IN)  :: n_FOVs
    INTEGER     , OPTIONAL, INTENT(IN)  :: Version         
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Sensor_Id       
    INTEGER     , OPTIONAL, INTENT(IN)  :: WMO_Satellite_Id
    INTEGER     , OPTIONAL, INTENT(IN)  :: WMO_Sensor_Id   
    INTEGER     , OPTIONAL, INTENT(IN)  :: AC_Release         
    INTEGER     , OPTIONAL, INTENT(IN)  :: AC_Version         
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
    LOGICAL :: AC_Present
    INTEGER :: NF90_Status
    INTEGER :: n_FOVs_DimID
    INTEGER :: n_Channels_DimID
    INTEGER :: VarID
    INTEGER :: Put_Status(4)
    
    ! Set up
    Error_Status = SUCCESS

    IF ( n_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid channel dimension input.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    
    AC_Present = .FALSE.
    IF ( PRESENT(n_FOVs) ) THEN
      IF ( n_FOVs > 0 ) AC_Present = .TRUE.
    END IF


    ! Create the data file
    NF90_Status = NF90_CREATE( NC_Filename,NF90_CLOBBER,NC_FileID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error creating '//TRIM(NC_Filename)//' - '//&
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF


    ! Define the dimensions
    ! ..The number of spectral channels
    NF90_Status = NF90_DEF_DIM( NC_FileID,CHANNEL_DIMNAME,n_Channels,n_Channels_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//CHANNEL_DIMNAME//' dimension in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..The number of fields-of-view
    IF ( AC_Present ) THEN
      NF90_Status = NF90_DEF_DIM( NC_FileID,FOV_DIMNAME,n_FOVs,n_FOVs_DimID )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error defining '//FOV_DIMNAME//' dimension in '//&
                  TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Create_Cleanup(Close_File=.TRUE.); RETURN
      END IF
    END IF


    ! Write the global attributes
    Error_Status = WriteGAtts( NC_Filename                      , &
                               NC_FileID                        , &
                               Version         =Version         , &
                               Sensor_Id       =Sensor_Id       , &
                               WMO_Satellite_Id=WMO_Satellite_Id, &
                               WMO_Sensor_Id   =WMO_Sensor_Id   , &
                               AC_Release      =AC_Release      , &
                               AC_Version      =AC_Version      , &
                               Title           =Title           , &
                               History         =History         , &
                               Comment         =Comment         , &
                               Message_Log     =Message_Log       )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error writing global attributes to '//TRIM(NC_Filename)
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Define the variables
    ! ..Sensor type to identify uW, IR, VIS, UV, etc sensor channels
    NF90_Status = NF90_DEF_VAR( NC_FileID,SENSOR_TYPE_VARNAME,SENSOR_TYPE_TYPE, &
                                varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//SENSOR_TYPE_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME   ,SENSOR_TYPE_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,SENSOR_TYPE_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME      ,SENSOR_TYPE_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME  ,SENSOR_TYPE_FILLVALUE )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//SENSOR_TYPE_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..List of sensor channel numbers
    NF90_Status = NF90_DEF_VAR( NC_FileID,SENSOR_CHANNEL_VARNAME,SENSOR_CHANNEL_TYPE, &
                                dimIDs=(/n_Channels_DimID/),varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//SENSOR_CHANNEL_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME   ,SENSOR_CHANNEL_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,SENSOR_CHANNEL_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME      ,SENSOR_CHANNEL_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME  ,SENSOR_CHANNEL_FILLVALUE )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//SENSOR_CHANNEL_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..Polarization type flag.
    NF90_Status = NF90_DEF_VAR( NC_FileID,POLARIZATION_VARNAME,POLARIZATION_TYPE, &
                                dimIDs=(/n_Channels_DimID/),varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//POLARIZATION_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME   ,POLARIZATION_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,POLARIZATION_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME      ,POLARIZATION_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME  ,POLARIZATION_FILLVALUE )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//POLARIZATION_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..Bit position flags for channels
    NF90_Status = NF90_DEF_VAR( NC_FileID,CHANNEL_FLAG_VARNAME,CHANNEL_FLAG_TYPE, &
                                dimIDs=(/n_Channels_DimID/),varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//CHANNEL_FLAG_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME   ,CHANNEL_FLAG_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,CHANNEL_FLAG_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME      ,CHANNEL_FLAG_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME  ,CHANNEL_FLAG_FILLVALUE )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//CHANNEL_FLAG_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..Channel central frequency, f
    NF90_Status = NF90_DEF_VAR( NC_FileID,FREQUENCY_VARNAME,FREQUENCY_TYPE, &
                                dimIDs=(/n_Channels_DimID/),varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//FREQUENCY_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME   ,FREQUENCY_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,FREQUENCY_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME      ,FREQUENCY_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME  ,FREQUENCY_FILLVALUE )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//FREQUENCY_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..Channel central wavenumber, v
    NF90_Status = NF90_DEF_VAR( NC_FileID,WAVENUMBER_VARNAME,WAVENUMBER_TYPE, &
                                dimIDs=(/n_Channels_DimID/),varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//WAVENUMBER_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME   ,WAVENUMBER_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,WAVENUMBER_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME      ,WAVENUMBER_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME  ,WAVENUMBER_FILLVALUE )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//WAVENUMBER_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..First Planck coefficient, c1.v^3
    NF90_Status = NF90_DEF_VAR( NC_FileID,PLANCK_C1_VARNAME,PLANCK_C1_TYPE, &
                                dimIDs=(/n_Channels_DimID/),varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//PLANCK_C1_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME   ,PLANCK_C1_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,PLANCK_C1_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME      ,PLANCK_C1_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME  ,PLANCK_C1_FILLVALUE )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//PLANCK_C1_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..Second Planck coefficient, c2.v
    NF90_Status = NF90_DEF_VAR( NC_FileID,PLANCK_C2_VARNAME,PLANCK_C2_TYPE, &
                                dimIDs=(/n_Channels_DimID/),varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//PLANCK_C2_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME   ,PLANCK_C2_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,PLANCK_C2_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME      ,PLANCK_C2_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME  ,PLANCK_C2_FILLVALUE )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//PLANCK_C2_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..Polychromatic band correction offset
    NF90_Status = NF90_DEF_VAR( NC_FileID,BAND_C1_VARNAME,BAND_C1_TYPE, &
                                dimIDs=(/n_Channels_DimID/),varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//BAND_C1_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME   ,BAND_C1_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,BAND_C1_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME      ,BAND_C1_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME  ,BAND_C1_FILLVALUE )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//BAND_C1_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..Polychromatic band correction slope
    NF90_Status = NF90_DEF_VAR( NC_FileID,BAND_C2_VARNAME,BAND_C2_TYPE, &
                                dimIDs=(/n_Channels_DimID/),varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//BAND_C2_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME   ,BAND_C2_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,BAND_C2_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME      ,BAND_C2_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME  ,BAND_C2_FILLVALUE )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//BAND_C2_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..Planck radiance for the cosmic background temperature
    NF90_Status = NF90_DEF_VAR( NC_FileID,CBR_VARNAME,CBR_TYPE, &
                                dimIDs=(/n_Channels_DimID/),varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//CBR_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME   ,CBR_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,CBR_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME      ,CBR_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME  ,CBR_FILLVALUE )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//CBR_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ..TOA solar irradiance using Kurucz spectrum
    NF90_Status = NF90_DEF_VAR( NC_FileID,SOLAR_IRRADIANCE_VARNAME,SOLAR_IRRADIANCE_TYPE, &
                                dimIDs=(/n_Channels_DimID/),varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//SOLAR_IRRADIANCE_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME   ,SOLAR_IRRADIANCE_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,SOLAR_IRRADIANCE_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME      ,SOLAR_IRRADIANCE_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME  ,SOLAR_IRRADIANCE_FILLVALUE )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//SOLAR_IRRADIANCE_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Define the antenna correction variables if necessary
    IF ( AC_Present ) THEN
      Error_Status = DefineVar_AntCorr_netCDF( NC_Filename            , &
                                               NC_FileID              , &
                                               n_FOVs_DimID           , &
                                               n_Channels_DimID       , &
                                               Message_Log=Message_Log  )
      IF ( Error_Status /= SUCCESS ) THEN
        msg = 'Error defining AntCorr variables in '//TRIM(NC_Filename)
        CALL Create_Cleanup(Close_File=.TRUE.); RETURN
      END IF
    END IF
    

    ! Take netCDF file out of define mode
    NF90_Status = NF90_ENDDEF( NC_FileID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error taking '//TRIM(NC_Filename)//' out of define mode.'
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE Create_CleanUp(Close_File)
      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
      ! Close file
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File ) THEN
          NF90_Status = NF90_CLOSE( NC_FileID )
          IF ( NF90_Status /= NF90_NOERR ) &
            msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
                  TRIM(NF90_STRERROR( NF90_Status ))
        END IF
      END IF
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
    END SUBROUTINE Create_CleanUp

  END FUNCTION CreateFile

END MODULE SpcCoeff_netCDF_IO_old
