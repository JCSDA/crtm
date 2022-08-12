!
! SpcCoeff_netCDF_IO
!
! Module containing routines to read and write SpcCoeff netCDF
! format files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 17-Dec-2002
!                       paul.vandelst@noaa.gov
!

MODULE SpcCoeff_netCDF_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds     , ONLY: Long, Double
  USE Message_Handler, ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE File_Utility   , ONLY: File_Exists
  USE String_Utility , ONLY: StrClean
  USE SpcCoeff_Define, ONLY: SpcCoeff_type          , &
                             SpcCoeff_Associated    , &
                             SpcCoeff_Destroy       , &
                             SpcCoeff_Create        , &
                             SpcCoeff_Inspect       , &
                             SpcCoeff_ValidRelease  , &
                             SpcCoeff_Info          
                            
  USE netcdf
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Procedures
  PUBLIC :: SpcCoeff_netCDF_InquireFile
  PUBLIC :: SpcCoeff_netCDF_ReadFile
  PUBLIC :: SpcCoeff_netCDF_WriteFile
  PUBLIC :: SpcCoeff_netCDF_IOVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
    '$Id: SpcCoeff_netCDF_IO.f90 13519 2021-01-29 19:34:34Z patrick.stegmann@noaa.gov $'
  ! Default message string length
  INTEGER, PARAMETER :: ML = 1024
  ! Literal constants
  REAL(Double), PARAMETER :: ZERO = 0.0_Double
  REAL(Double), PARAMETER :: ONE  = 1.0_Double


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
  CHARACTER(*), PARAMETER :: CHANNEL_DIMNAME = 'n_Channels'


  ! Variable names.
  CHARACTER(*), PARAMETER :: SENSOR_TYPE_VARNAME      = 'Sensor_Type'
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_VARNAME   = 'Sensor_Channel'
  CHARACTER(*), PARAMETER :: POLARIZATION_VARNAME     = 'Polarization'
  CHARACTER(*), PARAMETER :: POLANGLE_VARNAME         = 'Polarization_Angle'
  CHARACTER(*), PARAMETER :: CHANNEL_FLAG_VARNAME     = 'Channel_Flag'
  CHARACTER(*), PARAMETER :: FREQUENCY_VARNAME        = 'Frequency'
  CHARACTER(*), PARAMETER :: WAVENUMBER_VARNAME       = 'Wavenumber'
  CHARACTER(*), PARAMETER :: PLANCK_C1_VARNAME        = 'Planck_C1'
  CHARACTER(*), PARAMETER :: PLANCK_C2_VARNAME        = 'Planck_C2'
  CHARACTER(*), PARAMETER :: BAND_C1_VARNAME          = 'Band_C1'
  CHARACTER(*), PARAMETER :: BAND_C2_VARNAME          = 'Band_C2'
  CHARACTER(*), PARAMETER :: CBR_VARNAME              = 'Cosmic_Background_Radiance'
  CHARACTER(*), PARAMETER :: SOLAR_IRRADIANCE_VARNAME = 'Solar_Irradiance'


  ! Variable long name attribute.
  CHARACTER(*), PARAMETER :: LONGNAME_ATTNAME = 'long_name'

  CHARACTER(*), PARAMETER :: SENSOR_TYPE_LONGNAME      = 'Sensor Type'
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_LONGNAME   = 'Sensor Channel'
  CHARACTER(*), PARAMETER :: POLARIZATION_LONGNAME     = 'Polarization type flag'
  CHARACTER(*), PARAMETER :: POLANGLE_LONGNAME         = 'Polarization Angle'
  CHARACTER(*), PARAMETER :: CHANNEL_FLAG_LONGNAME     = 'Channel flag'
  CHARACTER(*), PARAMETER :: FREQUENCY_LONGNAME        = 'Frequency'
  CHARACTER(*), PARAMETER :: WAVENUMBER_LONGNAME       = 'Wavenumber'
  CHARACTER(*), PARAMETER :: PLANCK_C1_LONGNAME        = 'Planck C1'
  CHARACTER(*), PARAMETER :: PLANCK_C2_LONGNAME        = 'Planck C2'
  CHARACTER(*), PARAMETER :: BAND_C1_LONGNAME          = 'Band C1'
  CHARACTER(*), PARAMETER :: BAND_C2_LONGNAME          = 'Band C2'
  CHARACTER(*), PARAMETER :: CBR_LONGNAME              = 'Cosmic Background Radiance'
  CHARACTER(*), PARAMETER :: SOLAR_IRRADIANCE_LONGNAME = 'Kurucz Solar Irradiance'


  ! Variable description attribute.
  CHARACTER(*), PARAMETER :: DESCRIPTION_ATTNAME = 'description'

  CHARACTER(*), PARAMETER :: SENSOR_TYPE_DESCRIPTION      = 'Sensor type to identify uW, IR, VIS, UV, etc sensor channels'
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_DESCRIPTION   = 'List of sensor channel numbers'
  CHARACTER(*), PARAMETER :: POLARIZATION_DESCRIPTION     = 'Polarization type flag.'
  CHARACTER(*), PARAMETER :: POLANGLE_DESCRIPTION         = 'Polarization angle offset'
  CHARACTER(*), PARAMETER :: CHANNEL_FLAG_DESCRIPTION     = 'Bit position flags for channels'
  CHARACTER(*), PARAMETER :: FREQUENCY_DESCRIPTION        = 'Channel central frequency, f'
  CHARACTER(*), PARAMETER :: WAVENUMBER_DESCRIPTION       = 'Channel central wavenumber, v'
  CHARACTER(*), PARAMETER :: PLANCK_C1_DESCRIPTION        = 'First Planck coefficient, c1.v^3'
  CHARACTER(*), PARAMETER :: PLANCK_C2_DESCRIPTION        = 'Second Planck coefficient, c2.v'
  CHARACTER(*), PARAMETER :: BAND_C1_DESCRIPTION          = 'Polychromatic band correction offset'
  CHARACTER(*), PARAMETER :: BAND_C2_DESCRIPTION          = 'Polychromatic band correction slope'
  CHARACTER(*), PARAMETER :: CBR_DESCRIPTION              = 'Planck radiance for the cosmic background temperature'
  CHARACTER(*), PARAMETER :: SOLAR_IRRADIANCE_DESCRIPTION = 'TOA solar irradiance using Kurucz spectrum'


  ! Variable units attribute.
  CHARACTER(*), PARAMETER :: UNITS_ATTNAME = 'units'

  CHARACTER(*), PARAMETER :: SENSOR_TYPE_UNITS      = 'N/A'
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_UNITS   = 'N/A'
  CHARACTER(*), PARAMETER :: POLARIZATION_UNITS     = 'N/A'
  CHARACTER(*), PARAMETER :: POLANGLE_UNITS         = 'degrees (^o)'
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

  INTEGER(Long), PARAMETER :: SENSOR_TYPE_FILLVALUE      = 0
  INTEGER(Long), PARAMETER :: SENSOR_CHANNEL_FILLVALUE   = 0
  INTEGER(Long), PARAMETER :: POLARIZATION_FILLVALUE     = 0
  REAL(Double), PARAMETER :: POLANGLE_FILLVALUE         = ZERO
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
  INTEGER, PARAMETER :: POLANGLE_TYPE         = NF90_DOUBLE
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
!       SpcCoeff_netCDF_InquireFile
!
! PURPOSE:
!       Function to inquire SpcCoeff object netCDF format files.
!
! CALLING SEQUENCE:
!       Error_Status = SpcCoeff_netCDF_InquireFile( &
!                        Filename, &
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
!                           SpcCoeff data file to inquire.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_Channels:         Total number of sensor channels.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:            The release number of the SpcCoeff file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:            The version number of the SpcCoeff file.
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
!                           attribute field of the SpcCoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the SpcCoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the SpcCoeff file.
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

  FUNCTION SpcCoeff_netCDF_InquireFile( &
    Filename        , &  ! Input
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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'SpcCoeff_InquireFile(netCDF)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: close_file
    INTEGER :: nf90_status
    INTEGER :: fileid
    INTEGER :: dimid
    TYPE(SpcCoeff_type) :: spccoeff
    
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
    ! ...n_Channels dimension
    nf90_status = NF90_INQ_DIMID( FileId,CHANNEL_DIMNAME,DimId )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//CHANNEL_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( nf90_status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    nf90_status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=spccoeff%n_Channels )
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
    IF ( PRESENT(n_Channels) ) n_Channels = spccoeff%n_Channels

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

  END FUNCTION SpcCoeff_netCDF_InquireFile



!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_netCDF_WriteFile
!
! PURPOSE:
!       Function to write SpcCoeff object files in netCDF format.
!
! CALLING SEQUENCE:
!       Error_Status = SpcCoeff_netCDF_WriteFile( &
!                        Filename         , &
!                        SpcCoeff         , &
!                        Quiet   = Quiet  , &
!                        Title   = Title  , &
!                        History = History, &
!                        Comment = Comment  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       SpcCoeff data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       SpcCoeff:       SpcCoeff object containing the spectral
!                       coefficient data.
!                       UNITS:      N/A
!                       TYPE:       SpcCoeff_type
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
!                       attribute field of the SpcCoeff file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the SpcCoeff file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the SpcCoeff file.
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

  FUNCTION SpcCoeff_netCDF_WriteFile( &
    Filename, &  ! Input
    SpcCoeff, &  ! Input
    Quiet   , &  ! Optional input
    Title   , &  ! Optional input
    History , &  ! Optional input
    Comment ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: Filename
    TYPE(SpcCoeff_type),    INTENT(IN) :: SpcCoeff
    LOGICAL,      OPTIONAL, INTENT(IN) :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'SpcCoeff_WriteFile(netCDF)'
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
    IF ( .NOT. SpcCoeff_Associated( SpcCoeff ) ) THEN
      msg = 'SpcCoeff structure is empty. Nothing to do!'
      CALL Write_CleanUp(); RETURN
    END IF
    ! ...Check if release is valid
    IF ( .NOT. SpcCoeff_ValidRelease( SpcCoeff ) ) THEN
      msg = 'SpcCoeff Release check failed.'
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet


    ! Create the output file
    err_stat = CreateFile( &
                 Filename                                    , &  ! Input
                 SpcCoeff%n_Channels                         , &  ! Input
                 fileid                                      , &  ! Output
                 Version          = SpcCoeff%Version         , &  ! Optional input
                 Sensor_Id        = SpcCoeff%Sensor_Id       , &  ! Optional input
                 WMO_Satellite_Id = SpcCoeff%WMO_Satellite_Id, &  ! Optional input
                 WMO_Sensor_Id    = SpcCoeff%WMO_Sensor_Id   , &  ! Optional input
                 Title            = Title                    , &  ! Optional input
                 History          = History                  , &  ! Optional input
                 Comment          = Comment                    )  ! Optional input
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error creating output file '//TRIM(Filename)
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    close_file = .TRUE.


    ! Write the data items
    ! ...Sensor_Type variable
    NF90_Status = NF90_INQ_VARID( FileId,SENSOR_TYPE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//SENSOR_TYPE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,SpcCoeff%Sensor_Type )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//SENSOR_TYPE_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Sensor_Channel variable
    NF90_Status = NF90_INQ_VARID( FileId,SENSOR_CHANNEL_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//SENSOR_CHANNEL_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,SpcCoeff%Sensor_Channel )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//SENSOR_CHANNEL_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Polarization variable
    NF90_Status = NF90_INQ_VARID( FileId,POLARIZATION_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//POLARIZATION_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,SpcCoeff%Polarization )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//POLARIZATION_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Polarization angle variable
    IF( SpcCoeff%Version > 2 ) THEN
      NF90_Status = NF90_INQ_VARID( FileId,POLANGLE_VARNAME,VarId )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for '//POLANGLE_VARNAME//&
              ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Write_Cleanup(); RETURN
      END IF
      NF90_Status = NF90_PUT_VAR( FileId,VarID,SpcCoeff%PolAngle )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error writing '//POLANGLE_VARNAME//' to '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Write_Cleanup(); RETURN
      END IF
    END IF
    ! ...Channel_Flag variable
    NF90_Status = NF90_INQ_VARID( FileId,CHANNEL_FLAG_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//CHANNEL_FLAG_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,SpcCoeff%Channel_Flag )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//CHANNEL_FLAG_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Frequency variable
    NF90_Status = NF90_INQ_VARID( FileId,FREQUENCY_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//FREQUENCY_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,SpcCoeff%Frequency )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//FREQUENCY_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Wavenumber variable
    NF90_Status = NF90_INQ_VARID( FileId,WAVENUMBER_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//WAVENUMBER_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,SpcCoeff%Wavenumber )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//WAVENUMBER_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Planck_C1 variable
    NF90_Status = NF90_INQ_VARID( FileId,PLANCK_C1_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//PLANCK_C1_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,SpcCoeff%Planck_C1 )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//PLANCK_C1_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Planck_C2 variable
    NF90_Status = NF90_INQ_VARID( FileId,PLANCK_C2_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//PLANCK_C2_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,SpcCoeff%Planck_C2 )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//PLANCK_C2_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Band_C1 variable
    NF90_Status = NF90_INQ_VARID( FileId,BAND_C1_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//BAND_C1_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,SpcCoeff%Band_C1 )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//BAND_C1_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Band_C2 variable
    NF90_Status = NF90_INQ_VARID( FileId,BAND_C2_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//BAND_C2_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,SpcCoeff%Band_C2 )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//BAND_C2_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Cosmic_Background_Radiance variable
    NF90_Status = NF90_INQ_VARID( FileId,CBR_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//CBR_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,SpcCoeff%Cosmic_Background_Radiance )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//CBR_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Solar_Irradiance variable
    NF90_Status = NF90_INQ_VARID( FileId,SOLAR_IRRADIANCE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//SOLAR_IRRADIANCE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,SpcCoeff%Solar_Irradiance )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//SOLAR_IRRADIANCE_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
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
      CALL SpcCoeff_Info( SpcCoeff, msg )
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

  END FUNCTION SpcCoeff_netCDF_WriteFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_netCDF_ReadFile
!
! PURPOSE:
!       Function to read SpcCoeff object files in netCDF format.
!
! CALLING SEQUENCE:
!       Error_Status = SpcCoeff_netCDF_ReadFile( &
!                        Filename         , &
!                        SpcCoeff         , &
!                        Quiet   = Quiet  , &
!                        Title   = Title  , &
!                        History = History, &
!                        Comment = Comment  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       SpcCoeff data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       SpcCoeff:       SpcCoeff object containing the spectral
!                       coefficient data.
!                       UNITS:      N/A
!                       TYPE:       SpcCoeff_type
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
!                       attribute field of the SpcCoeff file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the SpcCoeff file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the SpcCoeff file.
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

  FUNCTION SpcCoeff_netCDF_ReadFile( &
    Filename, &  ! Input
    SpcCoeff, &  ! Output
    Quiet   , &  ! Optional input
    Title   , &  ! Optional output
    History , &  ! Optional output
    Comment ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    TYPE(SpcCoeff_type),    INTENT(OUT) :: SpcCoeff
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'SpcCoeff_ReadFile(netCDF)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: close_file
    LOGICAL :: noisy
    INTEGER :: nf90_status
    INTEGER :: fileid
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
    err_stat = SpcCoeff_netCDF_InquireFile( &
                 Filename, &
                 n_Channels = n_channels  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error obtaining SpcCoeff dimensions from '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Allocate the output structure
    CALL SpcCoeff_Create( SpcCoeff, n_channels )
    IF ( .NOT. SpcCoeff_Associated(SpcCoeff) ) THEN
      msg = 'Error allocating output SpcCoeff'
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
                 Release          = SpcCoeff%Release         , &
                 Version          = SpcCoeff%Version         , &
                 Sensor_Id        = SpcCoeff%Sensor_Id       , &
                 WMO_Satellite_Id = SpcCoeff%WMO_Satellite_Id, &
                 WMO_Sensor_Id    = SpcCoeff%WMO_Sensor_Id   , &
                 Title            = Title                    , &
                 History          = History                  , &
                 Comment          = Comment                    )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attribute from '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check if release is valid
    IF ( .NOT. SpcCoeff_ValidRelease( SpcCoeff ) ) THEN
      msg = 'SpcCoeff Release check failed.'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the SpcCoeff data
    ! ...Sensor_Type variable
    nf90_status = NF90_INQ_VARID( fileid,SENSOR_TYPE_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//SENSOR_TYPE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,SpcCoeff%Sensor_Type )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//SENSOR_TYPE_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Sensor_Channel variable
    nf90_status = NF90_INQ_VARID( fileid,SENSOR_CHANNEL_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//SENSOR_CHANNEL_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,SpcCoeff%Sensor_Channel )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//SENSOR_CHANNEL_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Polarization variable
    nf90_status = NF90_INQ_VARID( fileid,POLARIZATION_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//POLARIZATION_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,SpcCoeff%Polarization )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//POLARIZATION_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Polarization angle variable
    IF ( SpcCoeff%Version > 2 ) THEN
      nf90_status = NF90_INQ_VARID( fileid,POLANGLE_VARNAME,varid )
      IF ( nf90_status /= NF90_NOERR ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for '//POLANGLE_VARNAME//&
              ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
        CALL Read_Cleanup(); RETURN
      END IF
      nf90_status = NF90_GET_VAR( fileid,varid,SpcCoeff%PolAngle )
      IF ( nf90_status /= NF90_NOERR ) THEN
        msg = 'Error reading '//POLANGLE_VARNAME//' from '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( nf90_status ))
        CALL Read_Cleanup(); RETURN
      END IF
    END IF
    ! ...Channel_Flag variable
    nf90_status = NF90_INQ_VARID( fileid,CHANNEL_FLAG_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//CHANNEL_FLAG_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,SpcCoeff%Channel_Flag )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//CHANNEL_FLAG_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Frequency variable
    nf90_status = NF90_INQ_VARID( fileid,FREQUENCY_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//FREQUENCY_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,SpcCoeff%Frequency )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//FREQUENCY_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Wavenumber variable
    nf90_status = NF90_INQ_VARID( fileid,WAVENUMBER_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//WAVENUMBER_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,SpcCoeff%Wavenumber )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//WAVENUMBER_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Planck_C1 variable
    nf90_status = NF90_INQ_VARID( fileid,PLANCK_C1_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//PLANCK_C1_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,SpcCoeff%Planck_C1 )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//PLANCK_C1_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Planck_C2 variable
    nf90_status = NF90_INQ_VARID( fileid,PLANCK_C2_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//PLANCK_C2_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,SpcCoeff%Planck_C2 )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//PLANCK_C2_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Band_C1 variable
    nf90_status = NF90_INQ_VARID( fileid,BAND_C1_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//BAND_C1_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,SpcCoeff%Band_C1 )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//BAND_C1_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Band_C2 variable
    nf90_status = NF90_INQ_VARID( fileid,BAND_C2_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//BAND_C2_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,SpcCoeff%Band_C2 )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//BAND_C2_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Cosmic_Background_Radiance variable
    nf90_status = NF90_INQ_VARID( fileid,CBR_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//CBR_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,SpcCoeff%Cosmic_Background_Radiance )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//CBR_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Solar_Irradiance variable
    nf90_status = NF90_INQ_VARID( fileid,SOLAR_IRRADIANCE_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//SOLAR_IRRADIANCE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,SpcCoeff%Solar_Irradiance )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//SOLAR_IRRADIANCE_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF


    ! Close the file
    nf90_status = NF90_CLOSE( fileid ); CLOSE_FILE = .FALSE.
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error closing output file - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF


    ! Output an info message
    IF ( noisy ) THEN
      CALL SpcCoeff_Info( SpcCoeff, msg )
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
      CALL SpcCoeff_Destroy( SpcCoeff )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Read_CleanUp
    
  END FUNCTION SpcCoeff_netCDF_ReadFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_netCDF_IOVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL SpcCoeff_netCDF_IOVersion( Id )
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

  SUBROUTINE SpcCoeff_netCDF_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE SpcCoeff_netCDF_IOVersion


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  ! Function to write the global attributes to a SpcCoeff data file.

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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'SpcCoeff_WriteGAtts(netCDF)'
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
    TYPE(SpcCoeff_type) :: SpcCoeff

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
    nf90_status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(gattname),SpcCoeff%Release )
    IF ( nf90_status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF


    ! Optional global attributes
    ! ...The Version
    IF ( PRESENT(Version) ) THEN
      ver = Version
    ELSE
      ver = SpcCoeff%Version
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
      nf90_status = NF90_PUT_ATT( FileID,NF90_GLOBAL,TRIM(gattname),title )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The history
    IF ( PRESENT(history) ) THEN
      gattname = HISTORY_GATTNAME
      nf90_status = NF90_PUT_ATT( FileID,NF90_GLOBAL,TRIM(gattname),history )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The comment
    IF ( PRESENT(comment) ) THEN
      gattname = COMMENT_GATTNAME
      nf90_status = NF90_PUT_ATT( FileID,NF90_GLOBAL,TRIM(gattname),comment )
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


  ! Function to read the global attributes from a SpcCoeff data file.

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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'SpcCoeff_ReadGAtts(netCDF)'
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
    ! ...The WMO_Satellite_Id
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
    ! ...The Title
    IF ( PRESENT(Title) ) THEN
      gattname = TITLE_GATTNAME; gattstring = ''
      nf90_status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(gattname),gattstring )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF         
      CALL StrClean( gattstring )
      Title = gattstring(1:MIN(LEN(Title), LEN_TRIM(gattstring)))
    END IF
    ! ...The History
    IF ( PRESENT(History) ) THEN
      gattname = HISTORY_GATTNAME; gattstring = ''
      nf90_status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(gattname),gattstring )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF         
      CALL StrClean( gattstring )
      History = gattstring(1:MIN(LEN(History), LEN_TRIM(gattstring)))
    END IF
    ! ...The Comment
    IF ( PRESENT(Comment) ) THEN
      gattname = COMMENT_GATTNAME; gattstring = ''
      nf90_status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(gattname),gattstring )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF         
      CALL StrClean( gattstring )
      Comment = gattstring(1:MIN(LEN(Comment), LEN_TRIM(gattstring)))
    END IF

  CONTAINS

    SUBROUTINE ReadGAtts_CleanUp()
      err_stat = FAILURE
      msg = 'Error reading '//TRIM(gattname)//' attribute from '//TRIM(Filename)//' - '// &
            TRIM(NF90_STRERROR( nf90_status ) )
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE ReadGAtts_CleanUp

  END FUNCTION ReadGAtts


  ! Function to create a SpcCoeff file for writing

  FUNCTION CreateFile( &
    Filename        , &  ! Input
    n_Channels      , &  ! Input
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
    INTEGER     ,           INTENT(IN)  :: n_Channels
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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'SpcCoeff_CreateFile(netCDF)'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: close_file
    INTEGER :: nf90_status
    INTEGER :: n_channels_dimid
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
    ! ...Total number of channels for the sensor
    nf90_status = NF90_DEF_DIM( FileID,CHANNEL_DIMNAME,n_Channels,n_Channels_dimid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//CHANNEL_DIMNAME//' dimension in '//&
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
    ! ...Sensor_Type variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                SENSOR_TYPE_VARNAME, &
                                SENSOR_TYPE_TYPE, &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//SENSOR_TYPE_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,SENSOR_TYPE_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,SENSOR_TYPE_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,SENSOR_TYPE_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,SENSOR_TYPE_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//SENSOR_TYPE_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
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
    ! ...Polarization variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                POLARIZATION_VARNAME, &
                                POLARIZATION_TYPE, &
                                dimIDs=(/n_channels_dimid/), &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//POLARIZATION_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,POLARIZATION_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,POLARIZATION_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,POLARIZATION_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,POLARIZATION_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//POLARIZATION_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Polarization angle variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                POLANGLE_VARNAME, &
                                POLANGLE_TYPE, &
                                dimIDs=(/n_channels_dimid/), &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//POLANGLE_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,POLANGLE_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,POLANGLE_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,POLANGLE_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,POLANGLE_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//POLANGLE_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Channel_Flag variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                CHANNEL_FLAG_VARNAME, &
                                CHANNEL_FLAG_TYPE, &
                                dimIDs=(/n_channels_dimid/), &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//CHANNEL_FLAG_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,CHANNEL_FLAG_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,CHANNEL_FLAG_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,CHANNEL_FLAG_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,CHANNEL_FLAG_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//CHANNEL_FLAG_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Frequency variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                FREQUENCY_VARNAME, &
                                FREQUENCY_TYPE, &
                                dimIDs=(/n_channels_dimid/), &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//FREQUENCY_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,FREQUENCY_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,FREQUENCY_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,FREQUENCY_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,FREQUENCY_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//FREQUENCY_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Wavenumber variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                WAVENUMBER_VARNAME, &
                                WAVENUMBER_TYPE, &
                                dimIDs=(/n_channels_dimid/), &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//WAVENUMBER_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,WAVENUMBER_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,WAVENUMBER_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,WAVENUMBER_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,WAVENUMBER_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//WAVENUMBER_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Planck_C1 variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                PLANCK_C1_VARNAME, &
                                PLANCK_C1_TYPE, &
                                dimIDs=(/n_channels_dimid/), &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//PLANCK_C1_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,PLANCK_C1_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,PLANCK_C1_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,PLANCK_C1_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,PLANCK_C1_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//PLANCK_C1_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Planck_C2 variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                PLANCK_C2_VARNAME, &
                                PLANCK_C2_TYPE, &
                                dimIDs=(/n_channels_dimid/), &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//PLANCK_C2_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,PLANCK_C2_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,PLANCK_C2_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,PLANCK_C2_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,PLANCK_C2_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//PLANCK_C2_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Band_C1 variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                BAND_C1_VARNAME, &
                                BAND_C1_TYPE, &
                                dimIDs=(/n_channels_dimid/), &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//BAND_C1_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,BAND_C1_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,BAND_C1_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,BAND_C1_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,BAND_C1_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//BAND_C1_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Band_C2 variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                BAND_C2_VARNAME, &
                                BAND_C2_TYPE, &
                                dimIDs=(/n_channels_dimid/), &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//BAND_C2_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,BAND_C2_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,BAND_C2_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,BAND_C2_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,BAND_C2_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//BAND_C2_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Cosmic_Background_Radiance variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                CBR_VARNAME, &
                                CBR_TYPE, &
                                dimIDs=(/n_channels_dimid/), &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//CBR_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,CBR_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,CBR_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,CBR_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,CBR_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//CBR_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Solar_Irradiance variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                SOLAR_IRRADIANCE_VARNAME, &
                                SOLAR_IRRADIANCE_TYPE, &
                                dimIDs=(/n_channels_dimid/), &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//SOLAR_IRRADIANCE_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,SOLAR_IRRADIANCE_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,SOLAR_IRRADIANCE_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,SOLAR_IRRADIANCE_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,SOLAR_IRRADIANCE_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//SOLAR_IRRADIANCE_VARNAME//' variable attributes to '//TRIM(Filename)
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

END MODULE SpcCoeff_netCDF_IO
