!
! ACCoeff_netCDF_IO
!
! Module containing routines to read and write ACCoeff netCDF 
! format files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 10-Apr-2007
!                       paul.vandelst@noaa.gov
!

MODULE ACCoeff_netCDF_IO


  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds     , ONLY: Long, Double
  USE Message_Handler, ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE File_Utility   , ONLY: File_Exists
  USE String_Utility , ONLY: StrClean
  USE ACCoeff_Define , ONLY: ACCoeff_type          , &
                             ACCoeff_Associated    , &
                             ACCoeff_Destroy       , &
                             ACCoeff_Create        , &
                             ACCoeff_Inspect       , &
                             ACCoeff_ValidRelease  , &
                             ACCoeff_Info          , &
                             ACCoeff_DefineVersion
  USE netcdf
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Procedures
  PUBLIC :: ACCoeff_netCDF_InquireFile
  PUBLIC :: ACCoeff_netCDF_ReadFile
  PUBLIC :: ACCoeff_netCDF_WriteFile
  PUBLIC :: ACCoeff_netCDF_IOVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Default message string length
  INTEGER, PARAMETER :: ML = 1024
  ! Literal constants
  REAL(Double), PARAMETER :: ZERO = 0.0_Double
  REAL(Double), PARAMETER :: ONE  = 1.0_Double

  ! Global attribute names. Case sensitive
  CHARACTER(*), PARAMETER :: RELEASE_GATTNAME          = 'Release'
  CHARACTER(*), PARAMETER :: VERSION_GATTNAME          = 'Version'
  CHARACTER(*), PARAMETER :: TITLE_GATTNAME            = 'Title' 
  CHARACTER(*), PARAMETER :: HISTORY_GATTNAME          = 'History'
  CHARACTER(*), PARAMETER :: COMMENT_GATTNAME          = 'Comment'
  CHARACTER(*), PARAMETER :: SENSOR_ID_GATTNAME        = 'Sensor_Id'
  CHARACTER(*), PARAMETER :: WMO_SATELLITE_ID_GATTNAME = 'WMO_Satellite_Id'
  CHARACTER(*), PARAMETER :: WMO_SENSOR_ID_GATTNAME    = 'WMO_Sensor_Id'

  ! Dimension names
  CHARACTER(*), PARAMETER :: FOV_DIMNAME     = 'n_FOVs'
  CHARACTER(*), PARAMETER :: CHANNEL_DIMNAME = 'n_Channels'

  ! Variable names
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_VARNAME   = 'Sensor_Channel'
  CHARACTER(*), PARAMETER :: A_EARTH_VARNAME          = 'A_earth'
  CHARACTER(*), PARAMETER :: A_SPACE_VARNAME          = 'A_space'
  CHARACTER(*), PARAMETER :: A_PLATFORM_VARNAME       = 'A_platform'

  ! Variable long name attribute.
  CHARACTER(*), PARAMETER :: LONGNAME_ATTNAME = 'long_name'

  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_LONGNAME = 'Sensor Channel'
  CHARACTER(*), PARAMETER :: A_EARTH_LONGNAME        = 'A(earth)'
  CHARACTER(*), PARAMETER :: A_SPACE_LONGNAME        = 'A(space)'
  CHARACTER(*), PARAMETER :: A_PLATFORM_LONGNAME     = 'A(platform)'

 
  ! Variable description attribute.
  CHARACTER(*), PARAMETER :: DESCRIPTION_ATTNAME = 'description'

  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_DESCRIPTION   = 'List of sensor channel numbers'
  CHARACTER(*), PARAMETER :: A_EARTH_DESCRIPTION          = 'Antenna efficiency for earth view'
  CHARACTER(*), PARAMETER :: A_SPACE_DESCRIPTION          = 'Antenna efficiency for cold space view'
  CHARACTER(*), PARAMETER :: A_PLATFORM_DESCRIPTION       = 'Antenna efficiency for satellite platform view'

  ! Variable units attribute.
  CHARACTER(*), PARAMETER :: UNITS_ATTNAME = 'units'
  
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_UNITS   = 'N/A'
  CHARACTER(*), PARAMETER :: A_EARTH_UNITS          = 'N/A'
  CHARACTER(*), PARAMETER :: A_SPACE_UNITS          = 'N/A'
  CHARACTER(*), PARAMETER :: A_PLATFORM_UNITS       = 'N/A'


  ! Variable _FillValue attribute.
  CHARACTER(*), PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'
  
  INTEGER(Long), PARAMETER :: SENSOR_CHANNEL_FILLVALUE   = 0_Long
  REAL(Double) , PARAMETER :: A_EARTH_FILLVALUE          = 1.0_Double
  REAL(Double) , PARAMETER :: A_SPACE_FILLVALUE          = 0.0_Double
  REAL(Double) , PARAMETER :: A_PLATFORM_FILLVALUE       = 0.0_Double


  ! Variable types
  INTEGER, PARAMETER :: SENSOR_CHANNEL_TYPE   = NF90_INT
  INTEGER, PARAMETER :: A_EARTH_TYPE          = NF90_DOUBLE
  INTEGER, PARAMETER :: A_SPACE_TYPE          = NF90_DOUBLE
  INTEGER, PARAMETER :: A_PLATFORM_TYPE       = NF90_DOUBLE


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
!       ACCoeff_netCDF_InquireFile
!
! PURPOSE:
!       Function to inquire ACCoeff object netCDF format files.
!
! CALLING SEQUENCE:
!       Error_Status = ACCoeff_netCDF_InquireFile( &
!                        Filename                           , &
!                        n_FOVs           = n_FOVs          , &
!                        n_Channels       = n_Channels      , &
!                        Release          = Release         , &
!                        Version          = Version         , &
!                        Sensor_Id        = Sensor_Id       , &
!                        WMO_Satellite_Id = WMO_Satellite_Id, &
!                        WMO_Sensor_Id    = WMO_Sensor_Id     )
!                        Title            = Title           , &
!                        History          = History         , &
!                        Comment          = Comment           )
!
! INPUTS:
!       Filename:           Character string specifying the name of the
!                           ACCoeff data file to inquire.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_FOVs:             Number of sensor fields-of-view (FOVs).
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Channels:         Number of sensor channels.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:            The release number of the ACCoeff file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:            The version number of the ACCoeff file.
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
!                           attribute field of the ACCoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the ACCoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the ACCoeff file.
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

  FUNCTION ACCoeff_netCDF_InquireFile( &
    Filename        , &  ! Input
    n_FOVs          , &  ! Optional output  
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
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_FOVs
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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ACCoeff_InquireFile(netCDF)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: close_file
    INTEGER :: nf90_status
    INTEGER :: fileid
    INTEGER :: dimid
    TYPE(ACCoeff_type) :: ACCoeff
    
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
    ! ...n_FOVs dimension
    NF90_Status = NF90_INQ_DIMID( FileId,FOV_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//FOV_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=accoeff%n_FOVs )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//FOV_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...n_Channels dimension
    NF90_Status = NF90_INQ_DIMID( FileId,CHANNEL_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//CHANNEL_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=accoeff%n_Channels )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//CHANNEL_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
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
    IF ( PRESENT(n_FOVs    ) ) n_FOVs     = ACCoeff%n_FOVs
    IF ( PRESENT(n_Channels) ) n_Channels = ACCoeff%n_Channels

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

  END FUNCTION ACCoeff_netCDF_InquireFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ACCoeff_netCDF_WriteFile
!
! PURPOSE:
!       Function to write ACCoeff object files in netCDF format.
!
! CALLING SEQUENCE:
!       Error_Status = ACCoeff_netCDF_WriteFile( &
!                        Filename         , &
!                        ACCoeff          , &
!                        Quiet   = Quiet  , &
!                        Title   = Title  , &
!                        History = History, &
!                        Comment = Comment  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       ACCoeff data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       ACCoeff:        ACCoeff object containing the antenna correction
!                       coefficient data.
!                       UNITS:      N/A
!                       TYPE:       ACCoeff_type
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
!                       attribute field of the ACCoeff file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the ACCoeff file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the ACCoeff file.
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

  FUNCTION ACCoeff_netCDF_WriteFile( &
    Filename, &  ! Input
    ACCoeff , &  ! Input
    Quiet   , &  ! Optional input
    Title   , &  ! Optional input
    History , &  ! Optional input
    Comment ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: Filename
    TYPE(ACCoeff_type),     INTENT(IN) :: ACCoeff
    LOGICAL,      OPTIONAL, INTENT(IN) :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ACCoeff_WriteFile(netCDF)'
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
    IF ( .NOT. ACCoeff_Associated( ACCoeff ) ) THEN
      msg = 'ACCoeff structure is empty. Nothing to do!'
      CALL Write_CleanUp(); RETURN
    END IF
    ! ...Check if release is valid
    IF ( .NOT. ACCoeff_ValidRelease( ACCoeff ) ) THEN
      msg = 'ACCoeff Release check failed.'
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet


    ! Create the output file
    err_stat = CreateFile( &
                 Filename                                   , &  ! Input
                 ACCoeff%n_FOVs                             , &  ! Input
                 ACCoeff%n_Channels                         , &  ! Input
                 fileid                                     , &  ! Output
                 Version          = ACCoeff%Version         , &  ! Optional input
                 Sensor_Id        = ACCoeff%Sensor_Id       , &  ! Optional input
                 WMO_Satellite_Id = ACCoeff%WMO_Satellite_Id, &  ! Optional input
                 WMO_Sensor_Id    = ACCoeff%WMO_Sensor_Id   , &  ! Optional input
                 Title            = Title                   , &  ! Optional input
                 History          = History                 , &  ! Optional input
                 Comment          = Comment                   )  ! Optional input
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error creating output file '//TRIM(Filename)
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    close_file = .TRUE.


    ! Write the data items
    ! ...Sensor_Channel variable
    NF90_Status = NF90_INQ_VARID( FileId,SENSOR_CHANNEL_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//SENSOR_CHANNEL_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,ACcoeff%Sensor_Channel )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//SENSOR_CHANNEL_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...A_earth variable
    NF90_Status = NF90_INQ_VARID( FileId,A_EARTH_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//A_EARTH_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,ACcoeff%A_earth )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//A_EARTH_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...A_space variable
    NF90_Status = NF90_INQ_VARID( FileId,A_SPACE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//A_SPACE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,ACcoeff%A_space )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//A_SPACE_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...A_platform variable
    NF90_Status = NF90_INQ_VARID( FileId,A_PLATFORM_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//A_PLATFORM_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,ACcoeff%A_platform )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//A_PLATFORM_VARNAME//' to '//TRIM(Filename)//&
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
      CALL ACCoeff_Info( ACCoeff, msg )
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

  END FUNCTION ACCoeff_netCDF_WriteFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ACCoeff_netCDF_ReadFile
!
! PURPOSE:
!       Function to read ACCoeff object files in netCDF format.
!
! CALLING SEQUENCE:
!       Error_Status = ACCoeff_netCDF_ReadFile( &
!                        Filename         , &
!                        ACCoeff          , &
!                        Quiet   = Quiet  , &
!                        Title   = Title  , &
!                        History = History, &
!                        Comment = Comment  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       ACCoeff data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       ACCoeff:        ACCoeff object containing the antenna correction
!                       coefficient data.
!                       UNITS:      N/A
!                       TYPE:       ACCoeff_type
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
!                       attribute field of the ACCoeff file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the ACCoeff file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the ACCoeff file.
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

  FUNCTION ACCoeff_netCDF_ReadFile( &
    Filename, &  ! Input
    ACCoeff , &  ! Output
    Quiet   , &  ! Optional input
    Title   , &  ! Optional output
    History , &  ! Optional output
    Comment ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    TYPE(ACCoeff_type),     INTENT(OUT) :: ACCoeff
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ACCoeff_ReadFile(netCDF)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: close_file
    LOGICAL :: noisy
    INTEGER :: nf90_status
    INTEGER :: fileid
    INTEGER :: n_fovs    
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
    err_stat = ACCoeff_netCDF_InquireFile( &
                 Filename, &
                 n_FOVs     = n_fovs    , &
                 n_Channels = n_channels  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error obtaining ACCoeff dimensions from '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Allocate the output structure
    CALL ACCoeff_Create( &
           ACCoeff, &
           n_fovs    , &
           n_channels  )
    IF ( .NOT. ACCoeff_Associated(ACCoeff) ) THEN
      msg = 'Error allocating output ACCoeff'
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
                 Release          = ACCoeff%Release         , &
                 Version          = ACCoeff%Version         , &
                 Sensor_Id        = ACCoeff%Sensor_Id       , &
                 WMO_Satellite_Id = ACCoeff%WMO_Satellite_Id, &
                 WMO_Sensor_Id    = ACCoeff%WMO_Sensor_Id   , &
                 Title            = Title                     , &
                 History          = History                   , &
                 Comment          = Comment                     )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attribute from '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check if release is valid
    IF ( .NOT. ACCoeff_ValidRelease( ACCoeff ) ) THEN
      msg = 'ACCoeff Release check failed.'
      CALL Read_Cleanup(); RETURN
    END IF

    
    ! Read the ACCoeff data
    ! ...Sensor_Channel variable
    nf90_status = NF90_INQ_VARID( fileid,SENSOR_CHANNEL_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//SENSOR_CHANNEL_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,ACcoeff%Sensor_Channel )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//SENSOR_CHANNEL_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...A_earth variable
    nf90_status = NF90_INQ_VARID( fileid,A_EARTH_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//A_EARTH_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,ACcoeff%A_earth )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//A_EARTH_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...A_space variable
    nf90_status = NF90_INQ_VARID( fileid,A_SPACE_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//A_SPACE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,ACcoeff%A_space )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//A_SPACE_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...A_platform variable
    nf90_status = NF90_INQ_VARID( fileid,A_PLATFORM_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//A_PLATFORM_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,ACcoeff%A_platform )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//A_PLATFORM_VARNAME//' from '//TRIM(Filename)//&
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
      CALL ACCoeff_Info( ACCoeff, msg )
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
      CALL ACCoeff_Destroy( ACCoeff )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Read_CleanUp
    
  END FUNCTION ACCoeff_netCDF_ReadFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ACCoeff_netCDF_IOVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL ACCoeff_netCDF_IOVersion( Id )
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

  SUBROUTINE ACCoeff_netCDF_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE ACCoeff_netCDF_IOVersion



!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  ! Function to write the global attributes to a ACCoeff data file.

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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ACCoeff_WriteGAtts(netCDF)'
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
    TYPE(ACCoeff_type) :: ACCoeff

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
    nf90_status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(gattname),ACCoeff%Release )
    IF ( nf90_status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF


    ! Optional global attributes
    ! ...The Version
    IF ( PRESENT(Version) ) THEN
      ver = Version
    ELSE
      ver = ACCoeff%Version
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


  ! Function to read the global attributes from a ACCoeff data file.

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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ACCoeff_ReadGAtts(netCDF)'
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


  ! Function to create a ACCoeff file for writing

  FUNCTION CreateFile( &
    Filename        , &  ! Input
    n_FOVs          , &  ! Input
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
    INTEGER     ,           INTENT(IN)  :: n_FOVs    
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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ACCoeff_CreateFile(netCDF)'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: close_file
    INTEGER :: nf90_status
    INTEGER :: n_fovs_dimid
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
    ! ...Number of fields of view for the sensor
    nf90_status = NF90_DEF_DIM( FileID,FOV_DIMNAME,n_FOVs,n_fovs_dimid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//FOV_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Total number of channels for the sensor
    nf90_status = NF90_DEF_DIM( FileID,CHANNEL_DIMNAME,n_Channels,n_channels_dimid )
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
    ! ...A_earth variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                A_EARTH_VARNAME, &
                                A_EARTH_TYPE, &
                                dimIDs=(/n_fovs_dimid,n_channels_dimid/), &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//A_EARTH_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,A_EARTH_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,A_EARTH_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,A_EARTH_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,A_EARTH_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//A_EARTH_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...A_space variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                A_SPACE_VARNAME, &
                                A_SPACE_TYPE, &
                                dimIDs=(/n_fovs_dimid,n_channels_dimid/), &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//A_SPACE_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,A_SPACE_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,A_SPACE_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,A_SPACE_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,A_SPACE_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//A_SPACE_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...A_platform variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                A_PLATFORM_VARNAME, &
                                A_PLATFORM_TYPE, &
                                dimIDs=(/n_fovs_dimid,n_channels_dimid/), &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//A_PLATFORM_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,A_PLATFORM_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,A_PLATFORM_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,A_PLATFORM_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,A_PLATFORM_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//A_PLATFORM_VARNAME//' variable attributes to '//TRIM(Filename)
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

END MODULE ACCoeff_netCDF_IO
