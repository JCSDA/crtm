!
! AtmProfile_netCDF_IO
!
! Module containing routines to read and write AtmProfile netCDF 
! format files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 08-Jul-2002
!                       paul.vandelst@noaa.gov
!

MODULE AtmProfile_netCDF_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds     ,   ONLY: Long, Double
  USE File_Utility   ,   ONLY: File_Exists
  USE Message_Handler,   ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE String_Utility ,   ONLY: StrClean
  USE AtmProfile_Define, ONLY: AtmProfile_type               , &
                               AtmProfile_Associated         , &
                               AtmProfile_Destroy            , &
                               AtmProfile_Create             , &
                               AtmProfile_Inspect            , &
                               AtmProfile_ValidRelease       , &
                               AtmProfile_Info               , &
                               AtmProfile_DefineVersion      , &
                               AtmProfile_Absorber_Name      , &
                               AtmProfile_Absorber_Units_Name, &
                               AtmProfile_Absorber_Units_LBL
  USE netcdf
  ! Disable implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Procedures
  PUBLIC :: AtmProfile_netCDF_InquireFile
  PUBLIC :: AtmProfile_netCDF_ReadFile
  PUBLIC :: AtmProfile_netCDF_ReadGroup
  PUBLIC :: AtmProfile_netCDF_WriteFile
  PUBLIC :: AtmProfile_netCDF_WriteGroup
  PUBLIC :: AtmProfile_netCDF_IOVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Module Version Id
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Default message string length
  INTEGER, PARAMETER :: ML = 512
  ! Literal constants
  REAL(Double), PARAMETER :: ZERO = 0.0_Double
  REAL(Double), PARAMETER :: ONE  = 1.0_Double
  ! Extra parameters not in netCDF(?)
  INTEGER, PARAMETER :: MAX_N_GROUPS = 8096

  ! Global attribute names. Case sensitive
  CHARACTER(*), PARAMETER :: RELEASE_GATTNAME     = 'Release'
  CHARACTER(*), PARAMETER :: VERSION_GATTNAME     = 'Version'
  CHARACTER(*), PARAMETER :: TITLE_GATTNAME       = 'Title' 
  CHARACTER(*), PARAMETER :: HISTORY_GATTNAME     = 'History'
  CHARACTER(*), PARAMETER :: COMMENT_GATTNAME     = 'Comment'
  CHARACTER(*), PARAMETER :: PROFILE_SET_GATTNAME = 'Profile_Set'
  ! Dimension names
  CHARACTER(*), PARAMETER :: LEVEL_DIMNAME       = 'n_Levels'
  CHARACTER(*), PARAMETER :: LAYER_DIMNAME       = 'n_Layers'
  CHARACTER(*), PARAMETER :: ABSORBER_DIMNAME    = 'n_Absorbers'
  CHARACTER(*), PARAMETER :: PROFILE_DIMNAME     = 'n_Profiles'
  CHARACTER(*), PARAMETER :: DESCRIPTION_DIMNAME = 'pl_strlen'
  
  ! Variable names
  CHARACTER(*), PARAMETER :: PROFILE_VARNAME             = 'Profile_Number'
  CHARACTER(*), PARAMETER :: DESCRIPTION_VARNAME         = 'Profile_Description'
  CHARACTER(*), PARAMETER :: CLIMATOLOGY_MODEL_VARNAME   = 'Climatology_Model'
  CHARACTER(*), PARAMETER :: YEAR_VARNAME                = 'Year'
  CHARACTER(*), PARAMETER :: MONTH_VARNAME               = 'Month'
  CHARACTER(*), PARAMETER :: DAY_VARNAME                 = 'Day'
  CHARACTER(*), PARAMETER :: HOUR_VARNAME                = 'Hour'
  CHARACTER(*), PARAMETER :: LATITUDE_VARNAME            = 'Latitude'
  CHARACTER(*), PARAMETER :: LONGITUDE_VARNAME           = 'Longitude'
  CHARACTER(*), PARAMETER :: SURFACE_ALTITUDE_VARNAME    = 'Surface_Altitude'
  CHARACTER(*), PARAMETER :: ABSORBER_ID_VARNAME         = 'Absorber_Id'
  CHARACTER(*), PARAMETER :: ABSORBER_UNITS_ID_VARNAME   = 'Absorber_Units_Id'
  CHARACTER(*), PARAMETER :: LEVEL_PRESSURE_VARNAME      = 'Level_Pressure'
  CHARACTER(*), PARAMETER :: LEVEL_TEMPERATURE_VARNAME   = 'Level_Temperature'
  CHARACTER(*), PARAMETER :: LEVEL_ABSORBER_VARNAME      = 'Level_Absorber'
  CHARACTER(*), PARAMETER :: LEVEL_ALTITUDE_VARNAME      = 'Level_Altitude'
  CHARACTER(*), PARAMETER :: LAYER_PRESSURE_VARNAME      = 'Layer_Pressure'
  CHARACTER(*), PARAMETER :: LAYER_TEMPERATURE_VARNAME   = 'Layer_Temperature'
  CHARACTER(*), PARAMETER :: LAYER_ABSORBER_VARNAME      = 'Layer_Absorber'
  CHARACTER(*), PARAMETER :: LAYER_DELTA_Z_VARNAME       = 'Layer_Delta_Z'

  ! Variable long name attribute.
  CHARACTER(*), PARAMETER :: LONGNAME_ATTNAME = 'long_name'

  CHARACTER(*), PARAMETER :: PROFILE_LONGNAME             = 'Profile Number'
  CHARACTER(*), PARAMETER :: DESCRIPTION_LONGNAME         = 'Profile Description'
  CHARACTER(*), PARAMETER :: CLIMATOLOGY_MODEL_LONGNAME   = 'Climatology Model'
  CHARACTER(*), PARAMETER :: YEAR_LONGNAME                = 'Year'
  CHARACTER(*), PARAMETER :: MONTH_LONGNAME               = 'Month'
  CHARACTER(*), PARAMETER :: DAY_LONGNAME                 = 'Day'
  CHARACTER(*), PARAMETER :: HOUR_LONGNAME                = 'Hour'  
  CHARACTER(*), PARAMETER :: LATITUDE_LONGNAME            = 'Latitude'
  CHARACTER(*), PARAMETER :: LONGITUDE_LONGNAME           = 'Longitude'
  CHARACTER(*), PARAMETER :: SURFACE_ALTITUDE_LONGNAME    = 'Surface Altitude'
  CHARACTER(*), PARAMETER :: ABSORBER_ID_LONGNAME         = 'Absorber ID'
  CHARACTER(*), PARAMETER :: ABSORBER_UNITS_ID_LONGNAME   = 'Absorber Units ID'
  CHARACTER(*), PARAMETER :: LEVEL_PRESSURE_LONGNAME      = 'Level pressure'
  CHARACTER(*), PARAMETER :: LEVEL_TEMPERATURE_LONGNAME   = 'Level temperature'
  CHARACTER(*), PARAMETER :: LEVEL_ABSORBER_LONGNAME      = 'Level absorber'
  CHARACTER(*), PARAMETER :: LEVEL_ALTITUDE_LONGNAME      = 'Level altitude'
  CHARACTER(*), PARAMETER :: LAYER_PRESSURE_LONGNAME      = 'Layer pressure'
  CHARACTER(*), PARAMETER :: LAYER_TEMPERATURE_LONGNAME   = 'Layer temperature'
  CHARACTER(*), PARAMETER :: LAYER_ABSORBER_LONGNAME      = 'Layer absorber'
  CHARACTER(*), PARAMETER :: LAYER_DELTA_Z_LONGNAME       = 'Layer thickness'

  ! Variable description attribute.
  CHARACTER(*), PARAMETER :: DESCRIPTION_ATTNAME = 'description'

  CHARACTER(*), PARAMETER :: PROFILE_DESCRIPTION             = 'The number of the profile in the dataset'
  CHARACTER(*), PARAMETER :: DESCRIPTION_DESCRIPTION         = 'Description of atmospheric profile and modification'
  CHARACTER(*), PARAMETER :: CLIMATOLOGY_MODEL_DESCRIPTION   = 'Climatology model associated with profile date/time/location.'
  CHARACTER(*), PARAMETER :: YEAR_DESCRIPTION                = 'Year associated with profile date'
  CHARACTER(*), PARAMETER :: MONTH_DESCRIPTION               = 'Month of year associated with profile date'
  CHARACTER(*), PARAMETER :: DAY_DESCRIPTION                 = 'Day of month associated with profile date'
  CHARACTER(*), PARAMETER :: HOUR_DESCRIPTION                = 'Hour of day associated with profile time'
  CHARACTER(*), PARAMETER :: LATITUDE_DESCRIPTION            = 'Latitude of profile location'
  CHARACTER(*), PARAMETER :: LONGITUDE_DESCRIPTION           = 'Longitude of profile location'
  CHARACTER(*), PARAMETER :: SURFACE_ALTITUDE_DESCRIPTION    = 'Surface altitude of profile'
  CHARACTER(*), PARAMETER :: ABSORBER_ID_DESCRIPTION         = 'HITRAN/LBLRTM/MonoRTM absorber ID for atmospheric absorbers'
  CHARACTER(*), PARAMETER :: ABSORBER_UNITS_ID_DESCRIPTION   = 'LBLRTM/MonoRTM absorber units ID'
  CHARACTER(*), PARAMETER :: LEVEL_PRESSURE_DESCRIPTION      = 'Level pressure'
  CHARACTER(*), PARAMETER :: LEVEL_TEMPERATURE_DESCRIPTION   = 'Level temperature'
  CHARACTER(*), PARAMETER :: LEVEL_ABSORBER_DESCRIPTION      = 'Level absorber amount'
  CHARACTER(*), PARAMETER :: LEVEL_ALTITUDE_DESCRIPTION      = 'Level geopotential altitude'
  CHARACTER(*), PARAMETER :: LAYER_PRESSURE_DESCRIPTION      = 'Average layer pressure'
  CHARACTER(*), PARAMETER :: LAYER_TEMPERATURE_DESCRIPTION   = 'Average layer temperature'
  CHARACTER(*), PARAMETER :: LAYER_ABSORBER_DESCRIPTION      = 'Average layer absorber amount'
  CHARACTER(*), PARAMETER :: LAYER_DELTA_Z_DESCRIPTION       = 'Layer thickness'

  ! Variable units attribute.
  CHARACTER(*), PARAMETER :: UNITS_ATTNAME = 'units'

  CHARACTER(*), PARAMETER :: PROFILE_UNITS             = 'N/A'
  CHARACTER(*), PARAMETER :: DESCRIPTION_UNITS         = 'N/A'
  CHARACTER(*), PARAMETER :: CLIMATOLOGY_MODEL_UNITS   = 'N/A'
  CHARACTER(*), PARAMETER :: YEAR_UNITS                = 'Year (C.E.)'
  CHARACTER(*), PARAMETER :: MONTH_UNITS               = 'Month of year'
  CHARACTER(*), PARAMETER :: DAY_UNITS                 = 'Day of month'
  CHARACTER(*), PARAMETER :: HOUR_UNITS                = 'Hour of day (24H)'
  CHARACTER(*), PARAMETER :: LATITUDE_UNITS            = 'degress North (-90->+90)'
  CHARACTER(*), PARAMETER :: LONGITUDE_UNITS           = 'degress East (0->360)'
  CHARACTER(*), PARAMETER :: SURFACE_ALTITUDE_UNITS    = 'metres (m)'
  CHARACTER(*), PARAMETER :: ABSORBER_ID_UNITS         = 'N/A'
  CHARACTER(*), PARAMETER :: ABSORBER_UNITS_ID_UNITS   = 'N/A'
  CHARACTER(*), PARAMETER :: LEVEL_PRESSURE_UNITS      = 'hectoPascals (hPa)'
  CHARACTER(*), PARAMETER :: LEVEL_TEMPERATURE_UNITS   = 'Kelvin (K)'
  CHARACTER(*), PARAMETER :: LEVEL_ABSORBER_UNITS      = 'Variable (see Absorber_Units_Id)'
  CHARACTER(*), PARAMETER :: LEVEL_ALTITUDE_UNITS      = 'metres (m)'
  CHARACTER(*), PARAMETER :: LAYER_PRESSURE_UNITS      = 'hectoPascals (hPa)'
  CHARACTER(*), PARAMETER :: LAYER_TEMPERATURE_UNITS   = 'Kelvin (K)'
  CHARACTER(*), PARAMETER :: LAYER_ABSORBER_UNITS      = 'Variable (see Absorber_Units_Id)'
  CHARACTER(*), PARAMETER :: LAYER_DELTA_Z_UNITS       = 'metres (m)'

  ! Variable fill value attribute
  CHARACTER(*), PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'

  INTEGER(Long), PARAMETER :: PROFILE_FILLVALUE              = 0
  CHARACTER(*) , PARAMETER :: DESCRIPTION_FILLVALUE          = NF90_FILL_CHAR
  INTEGER(Long), PARAMETER :: CLIMATOLOGY_MODEL_FILLVALUE    = 0
  INTEGER(Long), PARAMETER :: YEAR_FILLVALUE                 = 0
  INTEGER(Long), PARAMETER :: MONTH_FILLVALUE                = 0
  INTEGER(Long), PARAMETER :: DAY_FILLVALUE                  = 0
  INTEGER(Long), PARAMETER :: HOUR_FILLVALUE                 = 0
  REAL(Double) , PARAMETER :: LATITUDE_FILLVALUE             = -999.0_Double
  REAL(Double) , PARAMETER :: LONGITUDE_FILLVALUE            = -999.0_Double
  REAL(Double) , PARAMETER :: SURFACE_ALTITUDE_FILLVALUE     = -999.0_Double
  INTEGER(Long), PARAMETER :: ABSORBER_ID_FILLVALUE          = 0
  INTEGER(Long), PARAMETER :: ABSORBER_UNITS_ID_FILLVALUE    = 0
  REAL(Double) , PARAMETER :: LEVEL_PRESSURE_FILLVALUE       = ZERO
  REAL(Double) , PARAMETER :: LEVEL_TEMPERATURE_FILLVALUE    = ZERO
  REAL(Double) , PARAMETER :: LEVEL_ABSORBER_FILLVALUE       = ZERO
  REAL(Double) , PARAMETER :: LEVEL_ALTITUDE_FILLVALUE       = ZERO
  REAL(Double) , PARAMETER :: LAYER_PRESSURE_FILLVALUE       = ZERO
  REAL(Double) , PARAMETER :: LAYER_TEMPERATURE_FILLVALUE    = ZERO
  REAL(Double) , PARAMETER :: LAYER_ABSORBER_FILLVALUE       = ZERO
  REAL(Double) , PARAMETER :: LAYER_DELTA_Z_FILLVALUE        = ZERO

  ! Variable  datatypes
  INTEGER, PARAMETER :: PROFILE_TYPE              = NF90_INT
  INTEGER, PARAMETER :: DESCRIPTION_TYPE          = NF90_CHAR
  INTEGER, PARAMETER :: CLIMATOLOGY_MODEL_TYPE    = NF90_INT
  INTEGER, PARAMETER :: YEAR_TYPE                 = NF90_INT
  INTEGER, PARAMETER :: MONTH_TYPE                = NF90_INT
  INTEGER, PARAMETER :: DAY_TYPE                  = NF90_INT
  INTEGER, PARAMETER :: HOUR_TYPE                 = NF90_INT
  INTEGER, PARAMETER :: LATITUDE_TYPE             = NF90_DOUBLE
  INTEGER, PARAMETER :: LONGITUDE_TYPE            = NF90_DOUBLE
  INTEGER, PARAMETER :: SURFACE_ALTITUDE_TYPE     = NF90_DOUBLE
  INTEGER, PARAMETER :: ABSORBER_ID_TYPE          = NF90_INT
  INTEGER, PARAMETER :: ABSORBER_UNITS_ID_TYPE    = NF90_INT
  INTEGER, PARAMETER :: LEVEL_PRESSURE_TYPE       = NF90_DOUBLE
  INTEGER, PARAMETER :: LEVEL_TEMPERATURE_TYPE    = NF90_DOUBLE
  INTEGER, PARAMETER :: LEVEL_ABSORBER_TYPE       = NF90_DOUBLE
  INTEGER, PARAMETER :: LEVEL_ALTITUDE_TYPE       = NF90_DOUBLE
  INTEGER, PARAMETER :: LAYER_PRESSURE_TYPE       = NF90_DOUBLE
  INTEGER, PARAMETER :: LAYER_TEMPERATURE_TYPE    = NF90_DOUBLE
  INTEGER, PARAMETER :: LAYER_ABSORBER_TYPE       = NF90_DOUBLE
  INTEGER, PARAMETER :: LAYER_DELTA_Z_TYPE        = NF90_DOUBLE


CONTAINS


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PUBLIC MODULE ROUTINES ##                        ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AtmProfile_netCDF_InquireFile
!
! PURPOSE:
!       Function to inquire AtmProfile object netCDF format files.
!
! CALLING SEQUENCE:
!       Error_Status = AtmProfile_netCDF_InquireFile( &
!                        Filename                 , &
!                        n_Profiles  = n_Profiles , &
!                        Release     = Release    , &
!                        Title       = Title      , &
!                        History     = History    , &
!                        Comment     = Comment    , &
!                        Profile_Set = Profile_Set  )
!
! INPUTS:
!       Filename:           Character string specifying the name of the
!                           AtmProfile data file to inquire.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_Profiles:         The number of profiles contained in the AtmProfile
!                           file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:            The release number of the AtmProfile file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:              Character string written into the TITLE global
!                           attribute field of the AtmProfile file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the AtmProfile file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the AtmProfile file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Profile_Set:        Character string written into the PROFILE_SET global
!                           attribute field of the netCDF AtmProfile file.
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

  FUNCTION AtmProfile_netCDF_InquireFile( &
    Filename   , &  ! Input
    n_Profiles , &  ! Optional output
    Release    , &  ! Optional Output
    Title      , &  ! Optional output
    History    , &  ! Optional output
    Comment    , &  ! Optional output
    Profile_Set) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Profiles
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Profile_Set
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'AtmProfile_netCDF_IO::InquireFile'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: close_file
    INTEGER :: nf90_stat
    INTEGER :: fileid
    INTEGER :: groupid(MAX_N_GROUPS)
    INTEGER :: n_groups

    ! Set up
    err_stat = SUCCESS
    close_file = .FALSE.


    ! Open the file
    nf90_stat = NF90_OPEN( Filename,NF90_NOWRITE,fileid )
    IF ( nf90_stat /= NF90_NOERR ) THEN
      msg = 'Error opening '//TRIM(Filename)//' for read access - '// &
            TRIM(NF90_STRERROR( nf90_stat ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    close_file = .TRUE.


    ! Get the number of profiles
    nf90_stat = NF90_INQ_GRPS( fileid,n_groups,groupid )
    IF ( nf90_stat /= NF90_NOERR ) THEN
      msg = 'Error inquiring group IDs in '//TRIM(Filename)//' - '// &
            TRIM(NF90_STRERROR( nf90_stat ))
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Get the global attributes
    err_stat = ReadGAtts( &
      fileid  , &
      Release     = Release    , &
      Title       = Title      , &
      History     = History    , &
      Comment     = Comment    , &
      Profile_Set = Profile_Set  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attributes from '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Close the file
    nf90_stat = NF90_CLOSE( fileid )
    close_file = .FALSE.
    IF ( nf90_stat /= NF90_NOERR ) THEN
      msg = 'Error closing input file - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Set the return values
    IF ( PRESENT(n_Profiles) ) n_Profiles = n_groups

  CONTAINS

    SUBROUTINE Inquire_CleanUp()
      IF ( close_file ) THEN
        nf90_stat = NF90_CLOSE( fileid )
        IF ( nf90_stat /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup.'
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION AtmProfile_netCDF_InquireFile



!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AtmProfile_netCDF_WriteFile
!
! PURPOSE:
!       Function to write AtmProfile object files in netCDF format.
!
! CALLING SEQUENCE:
!       Error_Status = AtmProfile_netCDF_WriteFile( &
!                        AtmProfile, &
!                        Filename  , &
!                        Quiet       = Quiet      , &
!                        Clobber     = Clobber    , &
!                        Title       = Title      , &
!                        History     = History    , &
!                        Comment     = Comment    , &
!                        Profile_Set = Profile_Set  )
!
! OBJECTS:
!       AtmProfile:     AtmProfile object array containing the data to write
!                       to file.
!                       UNITS:      N/A
!                       TYPE:       AtmProfile_type
!                       DIMENSION:  Rank-1
!                       ATTRIBUTES: INTENT(IN)
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       AtmProfile data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
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
!       Clobber:        Set this logical argument to overwrite an existing filename
!                       If == .FALSE., an existing file is NOT overwritten, and the
!                                      function returns with an error [DEFAULT].
!                          == .TRUE.,  an existing file is overwritten with the new data.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:          Character string written into the TITLE global
!                       attribute field of the AtmProfile file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the AtmProfile file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the AtmProfile file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Profile_Set:    Character string written into the PROFILE_SET global
!                       attribute field of the netCDF AtmProfile file.
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

  FUNCTION AtmProfile_netCDF_WriteFile( &
    AtmProfile , &  ! Input
    Filename   , &  ! Input
    Quiet      , &  ! Optional input
    Clobber    , &  ! Optional input
    Title      , &  ! Optional input
    History    , &  ! Optional input
    Comment    , &  ! Optional input
    Profile_Set, &  ! Optional input
    Debug      ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(AtmProfile_type),  INTENT(IN) :: AtmProfile(:)
    CHARACTER(*),           INTENT(IN) :: Filename
    LOGICAL,      OPTIONAL, INTENT(IN) :: Quiet
    LOGICAL,      OPTIONAL, INTENT(IN) :: Clobber
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Profile_Set
    LOGICAL,      OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'AtmProfile_netCDF_IO::WriteFile'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: noisy
    LOGICAL :: no_clobber
    LOGICAL :: new_file
    INTEGER :: nf90_stat
    INTEGER :: clobber_flag
    INTEGER :: fileid
    INTEGER :: n
    TYPE(AtmProfile_type) :: dummy

    ! Set up
    err_stat = SUCCESS
    ! ...Check structure
    IF ( .NOT. ALL(AtmProfile_Associated( AtmProfile )) ) THEN
      msg = 'AtmProfile structure is empty. Nothing to do!'
      CALL Write_CleanUp(); RETURN
    END IF
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Check Clobber argument
    no_clobber = .TRUE.
    IF ( PRESENT(Clobber) ) no_clobber = .NOT. Clobber
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug ) noisy = .TRUE.
    END IF


    ! Open the file
    new_file = .TRUE.
    ! ...Set the clobber flag
    IF ( no_clobber ) THEN
      clobber_flag = NF90_NOCLOBBER
    ELSE
      clobber_flag = NF90_CLOBBER
    END IF
    ! ...Create the file
    nf90_stat = NF90_CREATE( &
      Filename, &
      clobber_flag+NF90_NETCDF4, &
      fileid )
    IF ( nf90_stat /= NF90_NOERR ) THEN
      ! Was creation failure due to exisitng file?
      IF ( nf90_stat == NF90_EEXIST ) THEN
        ! ...Yes, so just open it
        nf90_stat = NF90_OPEN( &
          Filename  , &
          NF90_WRITE, &  ! Test with NF90_SHARE?
          fileid      )
        IF ( nf90_stat /= NF90_NOERR ) THEN
          msg = 'Error opening existing file, '//TRIM(Filename)//', for write access - '//&
                TRIM(NF90_STRERROR( nf90_stat ))
          CALL Write_Cleanup(); RETURN
        END IF
        new_file = .FALSE.
      ELSE
        ! ...No, so toss an error
        msg = 'Error creating '//TRIM(Filename)//' - '//&
                TRIM(NF90_STRERROR( nf90_stat ))
        CALL Write_Cleanup(); RETURN
      END IF
    END IF


    ! Write the global attributes
    IF ( new_file ) THEN
      err_stat = WriteGAtts( &
        fileid, &
        Release     = dummy%Release, &
        Title       = Title      , &
        History     = History    , &
        Comment     = Comment    , &
        Profile_Set = Profile_Set  )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error writing global attribute to '//TRIM(Filename)
        CALL Write_Cleanup(); RETURN
      END IF
    END IF


    ! Write each entry as a separate group
    Profile_loop: DO n = 1, SIZE(AtmProfile)

      IF ( .NOT. AtmProfile_Associated( AtmProfile(n) ) ) CYCLE Profile_loop

      err_stat = AtmProfile_netCDF_WriteGroup( &
        AtmProfile(n), &
        fileid, &
        Quiet = Quiet, &
        Debug = Debug  )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error writing group to '//TRIM(Filename)
        CALL Write_Cleanup(); RETURN
      END IF

    END DO Profile_loop


    ! Close the file
    nf90_stat = NF90_CLOSE( fileid )
    IF ( nf90_stat /= NF90_NOERR ) THEN
      msg = 'Error closing output file - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Write_Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE Write_CleanUp()
      nf90_stat = NF90_CLOSE( fileid )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Write_CleanUp

  END FUNCTION AtmProfile_netCDF_WriteFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AtmProfile_netCDF_WriteGroup
!
! PURPOSE:
!       Function to write a AtmProfile object group to a netCDF file.
!
! CALLING SEQUENCE:
!       Error_Status = AtmProfile_netCDF_WriteGroup( &
!                        AtmProfile           , &
!                        FileId               , &
!                        GroupName = GroupName, &
!                        Quiet     = Quiet      )
!
! OBJECTS:
!       AtmProfile:     AtmProfile object containing the group data to write
!                       to file.
!                       UNITS:      N/A
!                       TYPE:       AtmProfile_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! INPUTS:
!       FileId:         The netCDF Id for the file to contain the AtmProfile
!                       group.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       GroupName:      The name of the group to write to file. If not specified
!                       the default group name is "atmprofile-X" where "X" is the
!                       profile number associated with the AtmProfile object.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
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
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the group data write was successful
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION AtmProfile_netCDF_WriteGroup( &
    AtmProfile, &  ! Input
    FileId    , &  ! Input
    GroupName , &  ! Optional input
    Quiet     , &  ! Optional input
    Debug     ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(AtmProfile_type),  INTENT(IN) :: AtmProfile
    INTEGER,                INTENT(IN) :: FileId
    CHARACTER(*), OPTIONAL, INTENT(IN) :: GroupName
    LOGICAL,      OPTIONAL, INTENT(IN) :: Quiet
    LOGICAL,      OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'AtmProfile_netCDF_IO::WriteGroup'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: group_name
    LOGICAL :: noisy
    INTEGER :: nf90_stat
    INTEGER :: groupid
    INTEGER :: n_levels_dimid
    INTEGER :: n_layers_dimid
    INTEGER :: n_absorbers_dimid
    INTEGER :: pl_strlen_dimid

    ! Setup
    err_stat = SUCCESS
    ! ...Check structure
    IF ( .NOT. (AtmProfile_Associated( AtmProfile )) ) THEN
      msg = 'AtmProfile structure is empty. Nothing to do!'
      CALL Write_CleanUp(); RETURN
    END IF
    IF ( .NOT. AtmProfile_ValidRelease( AtmProfile ) ) THEN
      msg = 'AtmProfile Release check failed.'
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Check GroupName argument, defining default.
    WRITE(group_name,'("atmprofile-",i0)') AtmProfile%Profile
    IF ( PRESENT(GroupName) ) THEN
      group_name = ADJUSTL(GroupName)
    END IF
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug ) noisy = .TRUE.
    END IF


    ! Create a new group for the AtmProfile data
    nf90_stat = NF90_DEF_GRP( &
      fileid, &
      group_name, &
      groupid )
    IF ( nf90_stat /= NF90_NOERR ) THEN
      msg = 'Error creating '//TRIM(group_name)//' group - '//&
            ' - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the group attributes
    err_stat = WriteGAtts( &
        groupid, &
        Version = AtmProfile%Version )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing Version attribute for the '//TRIM(group_name)//&
            ' group - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Define the dimensions for the group
    err_stat = DefineDimensions( &
      AtmProfile       , &
      groupid          , &
      n_levels_dimid   , &
      n_layers_dimid   , &
      n_absorbers_dimid, &
      pl_strlen_dimid  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error defining dimensions for the '//TRIM(group_name)//&
            ' group - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Define the variables for the group
    err_stat = DefineVariables( &
      groupid          , &
      n_levels_dimid   , &
      n_layers_dimid   , &
      n_absorbers_dimid, &
      pl_strlen_dimid  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error defining variables for the '//TRIM(group_name)//&
            ' group - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Take netCDF file out of define mode
    nf90_stat = NF90_ENDDEF( fileid )
    IF ( nf90_stat /= NF90_NOERR ) THEN
      msg = 'Error taking file out of define mode to write the '//&
            TRIM(group_name)//' group - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the variables for the group
    err_stat = WriteVariables( AtmProfile, groupid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing variables for the '//TRIM(group_name)//&
            ' group - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Put netCDF file back into define mode
    nf90_stat = NF90_REDEF( fileid )
    IF ( nf90_stat /= NF90_NOERR ) THEN
      msg = 'Error putting file back into define mode after writing the '//&
            TRIM(group_name)//' group - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Output an info message
    IF ( noisy ) THEN
      CALL AtmProfile_Info( AtmProfile,msg )
      CALL Display_Message( ROUTINE_NAME,msg,INFORMATION )
    END IF

  CONTAINS

    SUBROUTINE Write_CleanUp()
      nf90_stat = NF90_CLOSE( fileid )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Write_CleanUp

  END FUNCTION AtmProfile_netCDF_WriteGroup


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AtmProfile_netCDF_ReadFile
!
! PURPOSE:
!       Function to read AtmProfile object files in netCDF format.
!
! CALLING SEQUENCE:
!       Error_Status = AtmProfile_netCDF_ReadFile( &
!                        AtmProfile, &
!                        Filename  , &
!                        Quiet       = Quiet      , &
!                        Title       = Title      , &
!                        History     = History    , &
!                        Comment     = Comment    , &
!                        Profile_Set = Profile_Set  )
!
! OBJECTS:
!       AtmProfile:     AtmProfile object array to contain the data read
!                       from file.
!                       UNITS:      N/A
!                       TYPE:       AtmProfile_type
!                       DIMENSION:  Rank-1
!                       ATTRIBUTES: INTENT(OUT), ALLOCATABLE
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       AtmProfile data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
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
! OPTIONAL OUTPUTS:
!       Title:          Character string written into the TITLE global
!                       attribute field of the AtmProfile file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the AtmProfile file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the AtmProfile file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Profile_Set:    Character string written into the PROFILE_SET global
!                       attribute field of the AtmProfile file.
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

  FUNCTION AtmProfile_netCDF_ReadFile( &
    AtmProfile , &  ! Output
    Filename   , &  ! Input
    Quiet      , &  ! Optional input
    Title      , &  ! Optional output
    History    , &  ! Optional output
    Comment    , &  ! Optional output
    Profile_Set, &  ! Optional output
    Debug      ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(AtmProfile_type), ALLOCATABLE, INTENT(OUT) :: AtmProfile(:)
    CHARACTER(*),                       INTENT(IN)  :: Filename
    LOGICAL,                  OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*),             OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*),             OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*),             OPTIONAL, INTENT(OUT) :: Comment
    CHARACTER(*),             OPTIONAL, INTENT(OUT) :: Profile_Set
    LOGICAL,                  OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'AtmProfile_netCDF_IO::ReadFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: groupname
    LOGICAL :: close_file
    LOGICAL :: noisy
    INTEGER :: alloc_stat
    INTEGER :: nf90_stat
    INTEGER :: fileid
    INTEGER :: n, n_profiles
    INTEGER, ALLOCATABLE :: groupid(:)
    TYPE(AtmProfile_type) :: dummy


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


    ! Inquire the file to get the number of profiles
    err_stat = AtmProfile_netCDF_InquireFile( &
                 Filename, &
                 n_Profiles = n_profiles )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error obtaining profile count from '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Allocate the output and group id structure
    ALLOCATE( AtmProfile(n_profiles), groupid(n_profiles), &
              STAT=alloc_stat )
    IF ( alloc_stat /= 0 ) THEN
      msg = 'Error allocating arrays'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Open the file for reading
    nf90_stat = NF90_OPEN( Filename,NF90_NOWRITE,fileid )
    IF ( nf90_stat /= NF90_NOERR ) THEN
      msg = 'Error opening '//TRIM(Filename)//' for read access - '//&
            TRIM(NF90_STRERROR( nf90_stat ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    close_file = .TRUE.


    ! Read the global attributes
    err_stat = ReadGAtts( &
      fileid  , &
      Release     = dummy%Release, &
      Title       = Title  , &
      History     = History, &
      Comment     = Comment, &
      Profile_Set = Profile_Set )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attribute from '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check if release is valid
    IF ( .NOT. AtmProfile_ValidRelease( dummy ) ) THEN
      msg = 'Release check failed.'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Get the list of groups and their id
    nf90_stat = NF90_INQ_GRPS( fileid, n, groupid )
    IF ( nf90_stat /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for group ids - '//&
            TRIM(NF90_STRERROR( nf90_stat ))
      CALL Read_Cleanup(); RETURN
    END IF


    ! Begin main profile loop
    Profile_loop: DO n = 1, n_profiles

      ! Get the current group's name
      nf90_stat = NF90_INQ_GRPNAME( groupid(n), groupname )
      IF ( nf90_stat /= NF90_NOERR ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for the current group name - '//&
              TRIM(NF90_STRERROR( nf90_stat ))
        CALL Read_Cleanup(); RETURN
      END IF

      ! Read the current group's data
      err_stat = AtmProfile_netCDF_ReadGroup( &
        AtmProfile(n), &
        fileid    , &
        GroupName = groupname, &
        Quiet     = Quiet    , &
        Debug     = Debug      )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error reading '//TRIM(groupname)//' group from '//TRIM(Filename)
        CALL Read_Cleanup(); RETURN
      END IF

    END DO Profile_loop


    ! Close the file
    nf90_stat = NF90_CLOSE( fileid ); close_file = .FALSE.
    IF ( nf90_stat /= NF90_NOERR ) THEN
      msg = 'Error closing output file - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Read_Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE Read_CleanUp()
      IF ( close_file ) THEN
        nf90_stat = NF90_CLOSE( fileid )
        IF ( nf90_stat /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup- '//&
                TRIM(NF90_STRERROR( nf90_stat ))
      END IF
      IF ( ALLOCATED(AtmProfile) ) THEN
        CALL AtmProfile_Destroy( AtmProfile )
        DEALLOCATE(AtmProfile)
      END IF
      IF ( ALLOCATED(groupid) ) DEALLOCATE(groupid)
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Read_CleanUp

  END FUNCTION AtmProfile_netCDF_ReadFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AtmProfile_netCDF_ReadGroup
!
! PURPOSE:
!       Function to read a AtmProfile object group from a netCDF file.
!
! CALLING SEQUENCE:
!       Error_Status = AtmProfile_netCDF_ReadGroup( &
!                        AtmProfile           , &
!                        FileId               , &
!                        GroupName = GroupName, &
!                        Quiet     = Quiet      )
!
! OBJECTS:
!       AtmProfile:     AtmProfile object to contain the group data read
!                       from file.
!                       UNITS:      N/A
!                       TYPE:       AtmProfile_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       FileId:         The netCDF Id for the file containing the AtmProfile
!                       group.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       GroupName:      The name of the group to read from file. If not specified
!                       the default group name is 'atmprofile'
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
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
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the group data read was successful
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION AtmProfile_netCDF_ReadGroup( &
    AtmProfile, &  ! Output
    FileId    , &  ! Input
    GroupName , &  ! Optional input
    Quiet     , &  ! Optional input
    Debug     ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(AtmProfile_type),  INTENT(OUT) :: AtmProfile
    INTEGER,                INTENT(IN)  :: FileId
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: GroupName
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Quiet
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'AtmProfile_netCDF_IO::ReadGroup'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: group_name
    LOGICAL :: noisy
    INTEGER :: nf90_stat
    INTEGER :: groupid
    INTEGER :: n_levels   
    INTEGER :: n_layers   
    INTEGER :: n_absorbers
    INTEGER :: pl_strlen  

    ! Setup
    err_stat = SUCCESS
    ! ...Check GroupName argument, defining default.
    group_name = 'atmprofile'
    IF ( PRESENT(GroupName) ) THEN
      group_name = ADJUSTL(GroupName)
    END IF
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug ) noisy = .TRUE.
    END IF


    ! Get the group id
    nf90_stat = NF90_INQ_GRP_NCID(fileid, group_name, groupid)
    IF ( nf90_stat /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(group_name)//' group for its group id - '//&
            TRIM(NF90_STRERROR( nf90_stat ))
      CALL Read_Cleanup(); RETURN
    END IF


    ! Get the group dimensions
    err_stat = ReadDimensions( &
      GroupId    , &
      n_levels   , &
      n_layers   , &
      n_absorbers, &
      pl_strlen    )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading dimensions for the '//TRIM(group_name)//&
            ' group - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Read_Cleanup(); RETURN
    END IF


    ! Allocate the current AtmProfile object
    CALL AtmProfile_Create( &
      AtmProfile , &
      n_layers   , &
      n_absorbers  )
    IF ( .NOT. AtmProfile_Associated(AtmProfile) ) THEN
      msg = 'Error allocating output AtmProfile for group '//TRIM(group_name)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the group attributes
    err_stat = ReadGAtts( &
        groupid, &
        Version = AtmProfile%Version )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading Version attribute for the '//TRIM(group_name)//&
            ' group - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the variables for the group
    err_stat = ReadVariables( AtmProfile, groupid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading variables for the '//TRIM(group_name)//&
            ' group - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Read_Cleanup(); RETURN
    END IF


    ! Convert absorber id info
    CALL AtmProfile_Absorber_Name(AtmProfile)
    CALL AtmProfile_Absorber_Units_Name(AtmProfile)
    CALL AtmProfile_Absorber_Units_LBL(AtmProfile)
    
    
    ! Output an info message
    IF ( noisy ) THEN
      CALL AtmProfile_Info( AtmProfile,msg )
      CALL Display_Message( ROUTINE_NAME,msg,INFORMATION )
    END IF

  CONTAINS

    SUBROUTINE Read_CleanUp()
      CALL AtmProfile_Destroy(AtmProfile)
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Read_CleanUp

  END FUNCTION AtmProfile_netCDF_ReadGroup


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AtmProfile_netCDF_IOVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL AtmProfile_netCDF_IOVersion( Id )
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

  SUBROUTINE AtmProfile_netCDF_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE AtmProfile_netCDF_IOVersion


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  INCLUDE 'AtmProfile_netCDF_IO.inc'

END MODULE AtmProfile_netCDF_IO
