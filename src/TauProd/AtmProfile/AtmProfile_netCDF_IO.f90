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
  USE AtmProfile_Define, ONLY: ATMPROFILE_N_ABSORBERS        , &
                               ATMPROFILE_N_ABSORBER_UNITS   , &
                               ATMPROFILE_ABSORBER_UNITS_ID  , &
                               ATMPROFILE_ABSORBER_UNITS_NAME, &
                               ATMPROFILE_ABSORBER_UNITS_CHAR, &
                               AtmProfile_type         , &
                               AtmProfile_Associated   , &
                               AtmProfile_Destroy      , &
                               AtmProfile_Create       , &
                               AtmProfile_Inspect      , &
                               AtmProfile_ValidRelease , &
                               AtmProfile_Info         , &
                               AtmProfile_DefineVersion
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
  PUBLIC :: AtmProfile_netCDF_WriteFile
  PUBLIC :: AtmProfile_netCDF_IOVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Module Version Id
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
    '$Id$'
  ! Default message string length
  INTEGER,      PARAMETER :: ML    = 512
  ! Literal constants
  REAL(Double), PARAMETER :: ZERO = 0.0_Double
  REAL(Double), PARAMETER :: ONE  = 1.0_Double

  ! Global attribute names. Case sensitive
  CHARACTER(*), PARAMETER :: RELEASE_GATTNAME        = 'Release'
  CHARACTER(*), PARAMETER :: VERSION_GATTNAME        = 'Version'
  CHARACTER(*), PARAMETER :: TITLE_GATTNAME          = 'Title' 
  CHARACTER(*), PARAMETER :: HISTORY_GATTNAME        = 'History'
  CHARACTER(*), PARAMETER :: COMMENT_GATTNAME        = 'Comment'
  CHARACTER(*), PARAMETER :: PROFILE_SET_ID_GATTNAME = 'Profile_Set_Id'
  ! Dimension names
  CHARACTER(*), PARAMETER :: LEVEL_DIMNAME          = 'n_levels'
  CHARACTER(*), PARAMETER :: LAYER_DIMNAME          = 'n_layers'
  CHARACTER(*), PARAMETER :: ABSORBER_DIMNAME       = 'n_absorbers'
  CHARACTER(*), PARAMETER :: PROFILE_DIMNAME        = 'n_profiles'
  CHARACTER(*), PARAMETER :: DESCRIPTION_DIMNAME    = 'pdsl'
  
  ! Variable names
  CHARACTER(*), PARAMETER :: DESCRIPTION_VARNAME         = 'profile_description'
  CHARACTER(*), PARAMETER :: CLIMATOLOGY_MODEL_VARNAME   = 'climatology_model'
  CHARACTER(*), PARAMETER :: YEAR_VARNAME                = 'year'
  CHARACTER(*), PARAMETER :: MONTH_VARNAME               = 'month'
  CHARACTER(*), PARAMETER :: DAY_VARNAME                 = 'day'
  CHARACTER(*), PARAMETER :: HOUR_VARNAME                = 'hour'
  CHARACTER(*), PARAMETER :: LATITUDE_VARNAME            = 'latitude'
  CHARACTER(*), PARAMETER :: LONGITUDE_VARNAME           = 'longitude'
  CHARACTER(*), PARAMETER :: SURFACE_ALTITUDE_VARNAME    = 'surface_altitude'
  CHARACTER(*), PARAMETER :: ABSORBER_ID_VARNAME         = 'absorber_id'
  CHARACTER(*), PARAMETER :: ABSORBER_UNITS_ID_VARNAME   = 'absorber_units_id'
  CHARACTER(*), PARAMETER :: LEVEL_PRESSURE_VARNAME      = 'level_pressure'
  CHARACTER(*), PARAMETER :: LEVEL_TEMPERATURE_VARNAME   = 'level_temperature'
  CHARACTER(*), PARAMETER :: LEVEL_ABSORBER_VARNAME      = 'level_absorber'
  CHARACTER(*), PARAMETER :: LEVEL_ALTITUDE_VARNAME      = 'level_altitude'
  CHARACTER(*), PARAMETER :: LAYER_PRESSURE_VARNAME      = 'layer_pressure'
  CHARACTER(*), PARAMETER :: LAYER_TEMPERATURE_VARNAME   = 'layer_temperature'
  CHARACTER(*), PARAMETER :: LAYER_ABSORBER_VARNAME      = 'layer_absorber'
  CHARACTER(*), PARAMETER :: LAYER_DELTA_Z_VARNAME       = 'layer_delta_z'

  ! Variable long name attribute.
  CHARACTER(*), PARAMETER :: LONGNAME_ATTNAME = 'long_name'

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


  ! --------------------
  ! Structure defnitions
  ! --------------------
  TYPE :: DimId_type
    INTEGER :: Level    = -1
    INTEGER :: Layer    = -1
    INTEGER :: Absorber = -1
    INTEGER :: Profile  = -1
    INTEGER :: PL       = -1
    INTEGER :: NL       = -1
  END TYPE DimId_type

  TYPE :: VarId_type
    INTEGER :: Description         = -1
    INTEGER :: Climatology_Model   = -1
    INTEGER :: Year                = -1
    INTEGER :: Month               = -1
    INTEGER :: Day                 = -1
    INTEGER :: Hour                = -1
    INTEGER :: Latitude            = -1
    INTEGER :: Longitude           = -1
    INTEGER :: Surface_Altitude    = -1
    INTEGER :: Absorber_Id         = -1
    INTEGER :: Absorber_Units_Id   = -1
    INTEGER :: Absorber_Units_Name = -1
    INTEGER :: Level_Pressure      = -1
    INTEGER :: Level_Temperature   = -1
    INTEGER :: Level_Absorber      = -1
    INTEGER :: Level_Altitude      = -1
    INTEGER :: Layer_Pressure      = -1
    INTEGER :: Layer_Temperature   = -1
    INTEGER :: Layer_Absorber      = -1
    INTEGER :: Layer_Delta_Z       = -1
  END TYPE VarId_type

  TYPE :: FileInfo_type
    INTEGER :: Id = -1
    TYPE(DimId_type) :: DimId
    TYPE(VarId_type) :: VarId
  END TYPE FileInfo_type


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
!                        Filename                       , &
!                        n_Layers       = n_Layers      , &
!                        n_Absorbers    = n_Absorbers   , &
!                        n_Profiles     = n_Profiles    , &
!                        Release        = Release       , &
!                        Version        = Version       , &
!                        Title          = Title         , &
!                        History        = History       , &
!                        Comment        = Comment       , &
!                        Profile_Set_Id = Profile_Set_Id  )
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
!       n_Layers:           The number of atmospheric layers dimension of the
!                           atmospheric profile data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Absorbers:        The number of molecular absorbers dimension of the
!                           atmospheric profile data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Profiles:         The number of profiles contained in the netCDF
!                           dataset.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:            The release number of the netCDF AtmProfile file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:            The version number of the netCDF AtmProfile file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:              Character string written into the TITLE global
!                           attribute field of the netCDF AtmProfile file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the netCDF AtmProfile file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF AtmProfile file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Profile_Set_Id:     Character string written into the PROFILE_SET_ID global
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
    Filename      , &  ! Input
    n_Layers      , &  ! Optional output
    n_Absorbers   , &  ! Optional output
    n_Profiles    , &  ! Optional output
    Release       , &  ! Optional output
    Version       , &  ! Optional output
    Title         , &  ! Optional output
    History       , &  ! Optional output
    Comment       , &  ! Optional output
    Profile_Set_Id) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Layers
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Absorbers
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Profiles
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release         
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Profile_Set_Id
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'AtmProfile_netCDF_InquireFile'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Close_File
    INTEGER :: NF90_Status
    INTEGER :: FileId
    INTEGER :: DimId
    INTEGER :: m
    TYPE(AtmProfile_type) :: AtmProfile
    
    ! Set up
    err_stat = SUCCESS
    Close_File = .FALSE.


    ! Open the file
    NF90_Status = NF90_OPEN( Filename,NF90_NOWRITE,FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error opening '//TRIM(Filename)//' for read access - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    Close_File = .TRUE.


    ! Get the dimensions
    ! ...the layer dimension
    NF90_Status = NF90_INQ_DIMID( FileId,LAYER_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//LAYER_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=AtmProfile%n_Layers )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//LAYER_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...the absorber dimension
    NF90_Status = NF90_INQ_DIMID( FileId,ABSORBER_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//ABSORBER_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=AtmProfile%n_Absorbers )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//ABSORBER_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...the profile dimension
    NF90_Status = NF90_INQ_DIMID( FileId,PROFILE_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//PROFILE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=m )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//PROFILE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Get the global attributes
    err_stat = ReadGAtts( Filename, &
                          FileId  , &
                          Release        = Release       , &
                          Version        = Version       , &
                          Title          = Title         , &
                          History        = History       , &
                          Comment        = Comment       , &
                          Profile_Set_Id = Profile_Set_Id  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attributes from '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Close the file
    NF90_Status = NF90_CLOSE( FileId )
    Close_File = .FALSE.
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error closing input file - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    

    ! Set the return values
    IF ( PRESENT(n_Layers   ) ) n_Layers    = AtmProfile%n_Layers
    IF ( PRESENT(n_Absorbers) ) n_Absorbers = AtmProfile%n_Absorbers
    IF ( PRESENT(n_Profiles ) ) n_Profiles  = m

  CONTAINS
  
    SUBROUTINE Inquire_CleanUp()
      IF ( Close_File ) THEN
        NF90_Status = NF90_CLOSE( FileId )
        IF ( NF90_Status /= NF90_NOERR ) &
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
!       Function to write an array of AtmProfile objects to netCDF file.
!
! CALLING SEQUENCE:
!       Error_Status = AtmProfile_netCDF_WriteFile( &
!                        Filename  , &
!                        AtmProfile, &
!                        Quiet          = Quiet         , &
!                        Title          = Title         , &
!                        History        = History       , &
!                        Comment        = Comment       , &
!                        Profile_Set_Id = Profile_Set_Id  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       AtmProfile data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       AtmProfile:     Array of objects, each element of which contains
!                       atmospheric profile data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(AtmProfile_type)
!                       DIMENSION:  Rank-1
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
!       Profile_Set_Id: Character string written into the PROFILE_SET_ID global
!                       attribute field of the netCDF AtmProfile file.
!                       Identifies the profile set.
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

  FUNCTION AtmProfile_netCDF_WriteFile( &
    Filename      , &  ! Input
    AtmProfile    , &  ! Input
    Quiet         , &  ! Optional input
    Title         , &  ! Optional input
    History       , &  ! Optional input
    Comment       , &  ! Optional input
    Profile_Set_Id) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: Filename
    TYPE(AtmProfile_type),  INTENT(IN) :: AtmProfile(:)
    LOGICAL,      OPTIONAL, INTENT(IN) :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Profile_Set_Id
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'AtmProfile_netCDF_WriteFile'
    ! Local variables
    TYPE(FileInfo_type) :: FileInfo
    CHARACTER(ML) :: msg
    LOGICAL :: Close_File
    LOGICAL :: Noisy
    INTEGER :: NF90_Status
    INTEGER :: m, j
    INTEGER :: n_Profiles
    INTEGER :: Profile
    
    ! Set up
    err_stat = SUCCESS
    Close_File = .FALSE.
    ! ...Check structure status
    IF ( ANY( .NOT. AtmProfile_Associated( AtmProfile )) ) THEN
      msg = 'Empty AtmProfile objects found in array!'
      CALL Write_CleanUp(); RETURN
    END IF
    
    !----put this in separate procedure
    ! ...Check structures are consistent
    n_Profiles = SIZE(AtmProfile)
    DO m = 1, n_Profiles
      ! ...Check release
      IF ( .NOT. AtmProfile_ValidRelease( AtmProfile(m) ) ) THEN
        WRITE( msg,'("AtmProfile Release check failed for element #",i0)' ) m
        CALL Write_Cleanup(); RETURN
      END IF
      ! ...Check versions and absorber info match
      IF ( m > 1 ) THEN
        IF ( AtmProfile(m)%Version /= AtmProfile(1)%Version ) THEN
          WRITE( msg,'("AtmProfile Version diffferent for element #",i0)' ) m
          CALL Write_Cleanup(); RETURN
        END IF
        IF ( AtmProfile(m)%n_Layers /= AtmProfile(1)%n_Layers ) THEN
          WRITE( msg,'("AtmProfile n_Layers diffferent for element #",i0)' ) m
          CALL Write_Cleanup(); RETURN
        END IF
        IF ( AtmProfile(m)%n_Absorbers /= AtmProfile(1)%n_Absorbers ) THEN
          WRITE( msg,'("AtmProfile n_Absorbers diffferent for element #",i0)' ) m
          CALL Write_Cleanup(); RETURN
        END IF
        DO j = 1, AtmProfile(1)%n_Absorbers
          IF ( AtmProfile(m)%Absorber_Id(j) /= AtmProfile(1)%Absorber_Id(j) ) THEN
            WRITE( msg,'("AtmProfile absorber #",i0," id is diffferent for element #",i0)' ) j, m
            CALL Write_Cleanup(); RETURN
          END IF
          IF ( AtmProfile(m)%Absorber_Units_Id(j) /= AtmProfile(1)%Absorber_Units_Id(j) ) THEN
            WRITE( msg,'("AtmProfile absorber #",i0," units id is diffferent for element #",i0)' ) j, m
            CALL Write_Cleanup(); RETURN
          END IF
        END DO
      END IF
    END DO
    !----
    
    ! ...Check Quiet argument
    Noisy = .TRUE.
    IF ( PRESENT(Quiet) ) Noisy = .NOT. Quiet


    ! Open the output file
    IF ( File_Exists(Filename) ) THEN
      ! ...Get the file info for an existing file
      err_stat = GetInfo( Filename, FileInfo )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error retrieving '//TRIM(Filename)//' information'
        CALL Write_CleanUp(); RETURN
      END IF
      ! ...Open existing file to append data
      NF90_Status = NF90_OPEN( Filename,NF90_WRITE,FileInfo%Id )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error opening exisiting file '//TRIM(Filename)//' for write access - '//&
              TRIM(NF90_STRERROR( NF90_Status ))
        CALL Write_Cleanup(); RETURN
      END IF
    ELSE
      ! ...Create new output file
      err_stat = CreateFile( &
                   Filename                              , &  ! Input
                   AtmProfile(1)%n_Layers                , &  ! Input
                   AtmProfile(1)%n_Absorbers             , &  ! Input
                   FileInfo                              , &  ! Output
                   Version        = AtmProfile(1)%Version, &  ! Optional input
                   Title          = Title                , &  ! Optional input
                   History        = History              , &  ! Optional input
                   Comment        = Comment              , &  ! Optional input
                   Profile_Set_Id = Profile_Set_Id         )  ! Optional input
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error creating output file '//TRIM(Filename)
        CALL Write_Cleanup(); RETURN
      END IF
    END IF
    ! ...Close the file if any error from here on
    Close_File = .TRUE.
    
    
    ! Write the profile INDEPENDENT data
    ! ...Absorber_ID variable
    NF90_Status = NF90_PUT_VAR( &
                    FileInfo%Id, &
                    FileInfo%VarID%Absorber_ID, &
                    AtmProfile(1)%Absorber_ID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//ABSORBER_ID_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Absorber_Units_ID variable
    NF90_Status = NF90_PUT_VAR( &
                    FileInfo%Id, &
                    FileInfo%VarID%Absorber_Units_ID, &
                    AtmProfile(1)%Absorber_Units_ID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//ABSORBER_UNITS_ID_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the profile DEPENDENT data    
    DO m = 1, n_Profiles

      ! Assign the profile index to write
      Profile = AtmProfile(m)%Profile
      
      
      ! Write the data
      ! ...Description variable
      NF90_Status = NF90_PUT_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Description, &
                      AtmProfile(m)%Description, &
                      start=(/1,Profile/)   )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error writing '//DESCRIPTION_VARNAME//' to '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Write_Cleanup(); RETURN
      END IF
      ! ...Climatology_Model variable
      NF90_Status = NF90_PUT_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Climatology_Model, &
                      AtmProfile(m)%Climatology_Model, &
                      start=(/Profile/)     )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error writing '//CLIMATOLOGY_MODEL_VARNAME//' to '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Write_Cleanup(); RETURN
      END IF
      ! ...Year variable
      NF90_Status = NF90_PUT_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Year, &
                      AtmProfile(m)%Year, &
                      start=(/Profile/)     )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error writing '//YEAR_VARNAME//' to '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Write_Cleanup(); RETURN
      END IF
      ! ...Month variable
      NF90_Status = NF90_PUT_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Month, &
                      AtmProfile(m)%Month, &
                      start=(/Profile/)     )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error writing '//MONTH_VARNAME//' to '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Write_Cleanup(); RETURN
      END IF
      ! ...Day variable
      NF90_Status = NF90_PUT_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Day, &
                      AtmProfile(m)%Day, &
                      start=(/Profile/)     )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error writing '//DAY_VARNAME//' to '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Write_Cleanup(); RETURN
      END IF
      ! ...Hour variable
      NF90_Status = NF90_PUT_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Hour, &
                      AtmProfile(m)%Hour, &
                      start=(/Profile/)     )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error writing '//HOUR_VARNAME//' to '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Write_Cleanup(); RETURN
      END IF
      ! ...Latitude variable
      NF90_Status = NF90_PUT_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Latitude, &
                      AtmProfile(m)%Latitude, &
                      start=(/Profile/)     )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error writing '//LATITUDE_VARNAME//' to '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Write_Cleanup(); RETURN
      END IF
      ! ...Longitude variable
      NF90_Status = NF90_PUT_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Longitude, &
                      AtmProfile(m)%Longitude, &
                      start=(/Profile/)     )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error writing '//LONGITUDE_VARNAME//' to '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Write_Cleanup(); RETURN
      END IF
      ! ...Surface_Altitude variable
      NF90_Status = NF90_PUT_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Surface_Altitude, &
                      AtmProfile(m)%Surface_Altitude, &
                      start=(/Profile/)     )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error writing '//SURFACE_ALTITUDE_VARNAME//' to '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Write_Cleanup(); RETURN
      END IF
      ! ...Level_Pressure variable
      NF90_Status = NF90_PUT_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Level_Pressure, &
                      AtmProfile(m)%Level_Pressure, &
                      start=(/1,Profile/)   )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error writing '//LEVEL_PRESSURE_VARNAME//' to '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Write_Cleanup(); RETURN
      END IF
      ! ...Level_Temperature variable
      NF90_Status = NF90_PUT_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Level_Temperature, &
                      AtmProfile(m)%Level_Temperature, &
                      start=(/1,Profile/)   )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error writing '//LEVEL_TEMPERATURE_VARNAME//' to '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Write_Cleanup(); RETURN
      END IF
      ! ...Level_Absorber variable
      NF90_Status = NF90_PUT_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Level_Absorber, &
                      AtmProfile(m)%Level_Absorber, &
                      start=(/1,1,Profile/) )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error writing '//LEVEL_ABSORBER_VARNAME//' to '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Write_Cleanup(); RETURN
      END IF
      ! ...Level_Altitude variable
      NF90_Status = NF90_PUT_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Level_Altitude, &
                      AtmProfile(m)%Level_Altitude, &
                      start=(/1,Profile/)   )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error writing '//LEVEL_ALTITUDE_VARNAME//' to '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Write_Cleanup(); RETURN
      END IF
      ! ...Layer_Pressure variable
      NF90_Status = NF90_PUT_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Layer_Pressure, &
                      AtmProfile(m)%Layer_Pressure, &
                      start=(/1,Profile/)   )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error writing '//LAYER_PRESSURE_VARNAME//' to '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Write_Cleanup(); RETURN
      END IF
      ! ...Layer_Temperature variable
      NF90_Status = NF90_PUT_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Layer_Temperature, &
                      AtmProfile(m)%Layer_Temperature, &
                      start=(/1,Profile/)   )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error writing '//LAYER_TEMPERATURE_VARNAME//' to '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Write_Cleanup(); RETURN
      END IF
      ! ...Layer_Absorber variable
      NF90_Status = NF90_PUT_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Layer_Absorber, &
                      AtmProfile(m)%Layer_Absorber, &
                      start=(/1,1,Profile/) )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error writing '//LAYER_ABSORBER_VARNAME//' to '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Write_Cleanup(); RETURN
      END IF
      ! ...Layer_Delta_Z variable
      NF90_Status = NF90_PUT_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Layer_Delta_Z, &
                      AtmProfile(m)%Layer_Delta_Z, &
                      start=(/1,Profile/)   )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error writing '//LAYER_DELTA_Z_VARNAME//' to '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Write_Cleanup(); RETURN
      END IF

      
      ! Output an info message
      IF ( Noisy ) THEN
        CALL AtmProfile_Info( AtmProfile(m), msg )
        CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
      END IF

    END DO


    ! Close the file
    NF90_Status = NF90_CLOSE( FileInfo%Id )
    Close_File = .FALSE.
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error closing output file - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE Write_CleanUp()
      IF ( Close_File ) THEN
        NF90_Status = NF90_CLOSE( FileInfo%Id )
        IF ( NF90_Status /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing output file during error cleanup - '//&
                TRIM(NF90_STRERROR( NF90_Status ))
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Write_CleanUp

  END FUNCTION AtmProfile_netCDF_WriteFile


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
!                        Filename  , &
!                        AtmProfile, &
!                        Quiet          = Quiet         , &
!                        Reverse        = Reverse       , &
!                        Profile_List   = Profile_List  , &
!                        Title          = Title         , &
!                        History        = History       , &
!                        Comment        = Comment       , &
!                        Profile_Set_Id = Profile_Set_Id  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       AtmProfile data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       AtmProfile:     Array of objects, each element of which contains
!                       atmospheric profile data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(AtmProfile_type)
!                       DIMENSION:  Rank-1
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
!       Reverse:        Set this logical keyword to reverse the order of the
!                       profile data arrays in the K index (vertical) dimension.
!                       If REVERSE = .FALSE., arrays are returned as they are
!                                       stored in the netCDF input file (DEFAULT)
!                          REVERSE = .TRUE., arrays are returned in reverse order
!                                       to how they are stored in the input
!                                       netCDF file.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Profile_List:   Index array specifying the list of profiles to be
!                       read from the file. If not specified, the indices 
!                       of the profile read are:
!                         1, 2, 3, ..., MIN(SIZE(AtmProfile),n_Profiles)
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Rank-1
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUTS:
!       Profile_Set_Id: Character string written into the PROFILE_SET_ID global
!                       attribute field of the netCDF AtmProfile file.
!                       Identifies the dependent profile set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:          Character string written into the TITLE global
!                       attribute field of the netCDF AtmProfile file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the netCDF AtmProfile file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the netCDF AtmProfile file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the data read was successful
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION AtmProfile_netCDF_ReadFile( &
    Filename      , &  ! Input
    AtmProfile    , &  ! Output
    Quiet         , &  ! Optional input
    Reverse       , &  ! Optional input
    Profile_List  , &  ! Optional input
    Title         , &  ! Optional output
    History       , &  ! Optional output
    Comment       , &  ! Optional output
    Profile_Set_Id) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename   
    TYPE(AtmProfile_type),  INTENT(OUT) :: AtmProfile(:)
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Quiet
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Reverse
    INTEGER,      OPTIONAL, INTENT(IN)  :: Profile_List(:)
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title  
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Profile_Set_Id
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'AtmProfile_netCDF_ReadFile'
    ! Function variables
    TYPE(FileInfo_type) :: FileInfo
    INTEGER :: Local_Profile_List(SIZE(AtmProfile))
    CHARACTER(ML) :: msg
    LOGICAL :: Close_File
    LOGICAL :: Noisy
    LOGICAL :: ReverseProfile
    INTEGER :: NF90_Status
    INTEGER :: k, j, m
    INTEGER :: n_File_Profiles
    INTEGER :: n_Profiles, n_Absorbers, n_Layers
    INTEGER :: Profile
    
    ! Set up
    err_stat = SUCCESS
    CALL AtmProfile_Destroy(AtmProfile)
    Close_File = .FALSE.
    ! ...Check that the file exists
    IF ( .NOT. File_Exists(Filename) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check Quiet argument
    Noisy = .TRUE.
    IF ( PRESENT(Quiet) ) Noisy = .NOT. Quiet
    ! ...Check Reverse argument
    ReverseProfile = .FALSE.
    IF ( PRESENT(Reverse) ) ReverseProfile = Reverse

    
    ! Inquire the file to get the dimensions
    err_stat = AtmProfile_netCDF_InquireFile( &
                 Filename, &
                 n_Layers    = n_Layers       , &
                 n_Absorbers = n_Absorbers    , &
                 n_Profiles  = n_File_Profiles  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error obtaining AtmProfile dimensions from '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Check Profile_List argument
    IF ( PRESENT(Profile_List) ) THEN
      n_Profiles = MIN(SIZE(Profile_List),SIZE(AtmProfile))
      Local_Profile_List(1:n_Profiles) = Profile_List(1:n_Profiles)
    ELSE
      n_Profiles = MIN(n_File_Profiles,SIZE(AtmProfile))
      Local_Profile_List = (/(m,m=1,n_Profiles)/)
    ENDIF
    ! ...Check the resulting profile count
    IF ( n_Profiles == 0 ) THEN
      msg = 'Number of profiles to read is zero!'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Allocate the structure array
    CALL AtmProfile_Create( AtmProfile(1:n_Profiles) , &
                            n_Layers   , &
                            n_Absorbers  )
    IF ( ANY(.NOT. AtmProfile_Associated( AtmProfile(1:n_Profiles) ) ) ) THEN
      msg = 'Error allocating AtmProfile structures'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Get the file info
    err_stat = GetInfo( Filename, FileInfo )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error retrieving '//TRIM(Filename)//' information'
      CALL Read_CleanUp(); RETURN
    END IF
    
    
    ! Open the file for reading
    NF90_Status = NF90_OPEN( Filename,NF90_NOWRITE,FileInfo%Id )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error opening '//TRIM(Filename)//' for read access - '//&
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    Close_File = .TRUE.

    
    ! Read the global attributes
    err_stat = ReadGAtts( Filename   , &
                          FileInfo%Id, &
                          Release        = AtmProfile(1)%Release, &
                          Version        = AtmProfile(1)%Version, &
                          Title          = Title             , & 
                          History        = History           , & 
                          Comment        = Comment           , & 
                          Profile_Set_Id = Profile_Set_Id      ) 
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attribute from '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check if release is valid
    IF ( .NOT. AtmProfile_ValidRelease( AtmProfile(1) ) ) THEN
      msg = 'AtmProfile Release check failed.'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Replicate version to remaining objects
    AtmProfile%Version = AtmProfile(1)%Version
    
    
    ! Read the profile data
    DO m = 1, n_Profiles

      ! Assign the profile index to read
      Profile = Local_Profile_List(m)
      
    
      ! Read the AtmProfile data
      ! ...Description variable
      NF90_Status = NF90_GET_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Description, &
                      AtmProfile(m)%Description, &
                      start=(/1,Profile/)   )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error reading '//DESCRIPTION_VARNAME//' from '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Read_Cleanup(); RETURN
      END IF
      CALL StrClean( AtmProfile(m)%Description )
      ! ...Climatology_Model variable
      NF90_Status = NF90_GET_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Climatology_Model, &
                      AtmProfile(m)%Climatology_Model, &
                      start=(/Profile/)     )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error reading '//CLIMATOLOGY_MODEL_VARNAME//' from '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Read_Cleanup(); RETURN
      END IF
      ! ...Year variable
      NF90_Status = NF90_GET_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Year, &
                      AtmProfile(m)%Year, &
                      start=(/Profile/)     )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error reading '//YEAR_VARNAME//' from '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Read_Cleanup(); RETURN
      END IF
      ! ...Month variable
      NF90_Status = NF90_GET_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Month, &
                      AtmProfile(m)%Month, &
                      start=(/Profile/)     )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error reading '//MONTH_VARNAME//' from '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Read_Cleanup(); RETURN
      END IF
      ! ...Day variable
      NF90_Status = NF90_GET_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Day, &
                      AtmProfile(m)%Day, &
                      start=(/Profile/)     )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error reading '//DAY_VARNAME//' from '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Read_Cleanup(); RETURN
      END IF
      ! ...Hour variable
      NF90_Status = NF90_GET_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Hour, &
                      AtmProfile(m)%Hour, &
                      start=(/Profile/)     )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error reading '//HOUR_VARNAME//' from '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Read_Cleanup(); RETURN
      END IF
      ! ...Latitude variable
      NF90_Status = NF90_GET_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Latitude, &
                      AtmProfile(m)%Latitude, &
                      start=(/Profile/)     )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error reading '//LATITUDE_VARNAME//' from '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Read_Cleanup(); RETURN
      END IF
      ! ...Longitude variable
      NF90_Status = NF90_GET_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Longitude, &
                      AtmProfile(m)%Longitude, &
                      start=(/Profile/)     )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error reading '//LONGITUDE_VARNAME//' from '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Read_Cleanup(); RETURN
      END IF
      ! ...Surface_Altitude variable
      NF90_Status = NF90_GET_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Surface_Altitude, &
                      AtmProfile(m)%Surface_Altitude, &
                      start=(/Profile/)     )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error reading '//SURFACE_ALTITUDE_VARNAME//' from '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Read_Cleanup(); RETURN
      END IF
      ! ...Absorber_ID variable
      NF90_Status = NF90_GET_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Absorber_ID, &
                      AtmProfile(m)%Absorber_ID )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error reading '//ABSORBER_ID_VARNAME//' from '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Read_Cleanup(); RETURN
      END IF
      ! ...Absorber_Units_ID variable
      NF90_Status = NF90_GET_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Absorber_Units_ID, &
                      AtmProfile(m)%Absorber_Units_ID )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error reading '//ABSORBER_UNITS_ID_VARNAME//' from '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Read_Cleanup(); RETURN
      END IF
      ! ...Level_Pressure variable
      NF90_Status = NF90_GET_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Level_Pressure, &
                      AtmProfile(m)%Level_Pressure, &
                      start=(/1,Profile/)   )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error reading '//LEVEL_PRESSURE_VARNAME//' from '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Read_Cleanup(); RETURN
      END IF
      ! ...Level_Temperature variable
      NF90_Status = NF90_GET_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Level_Temperature, &
                      AtmProfile(m)%Level_Temperature, &
                      start=(/1,Profile/)   )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error reading '//LEVEL_TEMPERATURE_VARNAME//' from '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Read_Cleanup(); RETURN
      END IF
      ! ...Level_Absorber variable
      NF90_Status = NF90_GET_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Level_Absorber, &
                      AtmProfile(m)%Level_Absorber, &
                      start=(/1,1,Profile/) )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error reading '//LEVEL_ABSORBER_VARNAME//' from '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Read_Cleanup(); RETURN
      END IF
      ! ...Level_Altitude variable
      NF90_Status = NF90_GET_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Level_Altitude, &
                      AtmProfile(m)%Level_Altitude, &
                      start=(/1,Profile/)   )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error reading '//LEVEL_ALTITUDE_VARNAME//' from '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Read_Cleanup(); RETURN
      END IF
      ! ...Layer_Pressure variable
      NF90_Status = NF90_GET_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Layer_Pressure, &
                      AtmProfile(m)%Layer_Pressure, &
                      start=(/1,Profile/)   )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error reading '//LAYER_PRESSURE_VARNAME//' from '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Read_Cleanup(); RETURN
      END IF
      ! ...Layer_Temperature variable
      NF90_Status = NF90_GET_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Layer_Temperature, &
                      AtmProfile(m)%Layer_Temperature, &
                      start=(/1,Profile/)   )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error reading '//LAYER_TEMPERATURE_VARNAME//' from '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Read_Cleanup(); RETURN
      END IF
      ! ...Layer_Absorber variable
      NF90_Status = NF90_GET_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Layer_Absorber, &
                      AtmProfile(m)%Layer_Absorber, &
                      start=(/1,1,Profile/) )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error reading '//LAYER_ABSORBER_VARNAME//' from '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Read_Cleanup(); RETURN
      END IF
      ! ...Layer_Delta_Z variable
      NF90_Status = NF90_GET_VAR( &
                      FileInfo%Id, &
                      FileInfo%VarID%Layer_Delta_Z, &
                      AtmProfile(m)%Layer_Delta_Z, &
                      start=(/1,Profile/)   )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error reading '//LAYER_DELTA_Z_VARNAME//' from '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Read_Cleanup(); RETURN
      END IF


      ! Finish up with the data structure
      ! ...Fill the other Absorber_Units structure members
      DO j = 1, AtmProfile(m)%n_Absorbers
        AtmProfile(m)%Absorber_Units_Name(j) = ATMPROFILE_ABSORBER_UNITS_NAME(AtmProfile(m)%Absorber_Units_ID(j))
        AtmProfile(m)%Absorber_Units_LBL(j)  = ATMPROFILE_ABSORBER_UNITS_CHAR(AtmProfile(m)%Absorber_Units_ID(j))
      END DO
      ! ...Reverse the profile data direction
      IF ( ReverseProfile ) THEN
        k = AtmProfile(m)%n_Layers
        ! ...Level data
        AtmProfile(m)%Level_Pressure(0:k)    = AtmProfile(m)%Level_Pressure(k:0:-1)
        AtmProfile(m)%Level_Temperature(0:k) = AtmProfile(m)%Level_Temperature(k:0:-1)
        AtmProfile(m)%Level_Absorber(0:k,:)  = AtmProfile(m)%Level_Absorber(k:0:-1,:)
        AtmProfile(m)%Level_Altitude(0:k)    = AtmProfile(m)%Level_Altitude(k:0:-1)
        ! ...Layer data
        AtmProfile(m)%Layer_Pressure(1:k)    = AtmProfile(m)%Layer_Pressure(k:1:-1)
        AtmProfile(m)%Layer_Temperature(1:k) = AtmProfile(m)%Layer_Temperature(k:1:-1)
        AtmProfile(m)%Layer_Absorber(1:k,:)  = AtmProfile(m)%Layer_Absorber(k:1:-1,:)
        AtmProfile(m)%Layer_Delta_Z(1:k)     = AtmProfile(m)%Layer_Delta_Z(k:1:-1)
      END IF


      ! Output an info message
      IF ( Noisy ) THEN
        CALL AtmProfile_Info( AtmProfile(m), msg )
        CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
      END IF

    END DO


    ! Close the file
    NF90_Status = NF90_CLOSE( FileInfo%Id )
    Close_File = .FALSE.
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error closing input file - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE Read_CleanUp()
      IF ( Close_File ) THEN
        NF90_Status = NF90_CLOSE( FileInfo%Id )
        IF ( NF90_Status /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup- '//&
                TRIM(NF90_STRERROR( NF90_Status ))
      END IF
      CALL AtmProfile_Destroy( AtmProfile )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Read_CleanUp

  END FUNCTION AtmProfile_netCDF_ReadFile


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


!--------------------------------------------------------------------------------
!
! NAME:
!       WriteGAtts
!
! PURPOSE:
!       Function to write the global attributes to a netCDF AtmProfile
!       data file.
!
! CALLING SEQUENCE:
!       err_stat = WriteGAtts( Filename                  , &  ! Input
!                                  FileId                    , &  ! Input
!                                  Version       =Version       , &  ! Optional input
!                                  Title         =Title         , &  ! Optional input
!                                  History       =History       , &  ! Optional input
!                                  Comment       =Comment       , &  ! Optional input
!                                  Profile_Set_Id=Profile_Set_Id, &  ! Optional input
!                                  Message_Log   =Message_Log     )  ! Error messaging
!
! INPUTS:
!       Filename:      Character string specifying the name of the
!                         netCDF AtmProfile format data file to create.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       FileId:        NetCDF file ID number returned from the
!                         Open_ or Create_AtmProfile_netCDF() function.
!                         UNITS:      N/A
!                         TYPE:       Integer
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!
! OPTIONAL INPUTS:
!       Version:          The version number of the netCDF AtmProfile file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL

!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF AtmProfile file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF AtmProfile file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF AtmProfile file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Profile_Set_Id:   Character string written into the PROFILE_SET_ID global
!                         attribute field of the netCDF AtmProfile file.
!                         Should contain a short tag used to identify the
!                         profile set.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:      Character string specifying a filename in which
!                         any Messages will be logged. If not specified,
!                         or if an error occurs opening the log file, the
!                         default action is to output Messages to standard
!                         output.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       err_stat: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the global attribute write was successful.
!                        == WARNING an error occurred writing the supplied
!                           global attributes.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION WriteGAtts( Filename   , &  ! Input
                       FileId     , &  ! Input
                       Version       , &  ! Optional input
                       Title         , &  ! Optional input
                       History       , &  ! Optional input
                       Comment       , &  ! Optional input
                       Profile_Set_Id, &  ! Optional input
                       Message_Log   ) &  ! Error messaging
                     RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: Filename
    INTEGER     ,           INTENT(IN) :: FileId
    INTEGER     , OPTIONAL, INTENT(IN) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Profile_Set_Id
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_AtmProfile_GAtts'
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
    TYPE(AtmProfile_type) :: AtmProfile_Default

    ! Set up
    ! ------
    err_stat = SUCCESS
    msg = ' '

    ! Mandatory global attributes
    ! ---------------------------
    ! Software ID
    GAttName = WRITE_MODULE_HISTORY_GATTNAME
    NF90_Status = NF90_PUT_ATT( FileId, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                MODULE_VERSION_ID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF
    
    ! Creation date
    CALL DATE_AND_TIME( cdate, ctime, czone )
    GAttName = CREATION_DATE_AND_TIME_GATTNAME
    NF90_Status = NF90_PUT_ATT( FileId, &
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
    NF90_Status = NF90_PUT_ATT( FileId, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                AtmProfile_Default%Release )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF


    ! Optional global attributes
    ! --------------------------
    ! The Version
    IF ( PRESENT(Version) ) THEN
      Ver = Version
    ELSE
      Ver = AtmProfile_Default%Version
    END IF
    GAttName = VERSION_GATTNAME
    NF90_Status = NF90_PUT_ATT( FileId, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                Ver )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF

    ! The Profile_Set_Id
    IF ( PRESENT(Profile_Set_Id) ) THEN
      GAttName = PROFILE_SET_ID_GATTNAME
      NF90_Status = NF90_PUT_ATT( FileId, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  Profile_Set_Id )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The Title
    IF ( PRESENT(Title) ) THEN
      GAttName = TITLE_GATTNAME
      NF90_Status = NF90_PUT_ATT( FileId, &
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
      NF90_Status = NF90_PUT_ATT( FileId, &
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
      NF90_Status = NF90_PUT_ATT( FileId, &
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
      NF90_Status = NF90_CLOSE( FileId )
      IF ( NF90_Status /= NF90_NOERR ) &
        msg = '; Error closing input file during error cleanup - '//&
                  TRIM(NF90_STRERROR( NF90_Status ) )
      ! Set error status and print error msg
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM(GAttName)//' attribute to '//&
                            TRIM(Filename)//' - '// &
                            TRIM(NF90_STRERROR( NF90_Status ) )//TRIM(msg), &
                            err_stat, &
                            Message_Log=Message_Log )
    END SUBROUTINE WriteGAtts_CleanUp
    
  END FUNCTION WriteGAtts


!------------------------------------------------------------------------------
!
! NAME:
!       ReadGAtts
!
! PURPOSE:
!       Function to read the global attributes from a netCDF AtmProfile
!       data file.
!
! CALLING SEQUENCE:
!       err_stat = ReadGAtts( Filename                  , &  ! Input
!                                 FileId                    , &  ! Input
!                                 Release       =Release       , &  ! Optional output
!                                 Version       =Version       , &  ! Optional output
!                                 Profile_Set_Id=Profile_Set_Id, &  ! Optional output
!                                 Title         =Title         , &  ! Optional output
!                                 History       =History       , &  ! Optional output
!                                 Comment       =Comment       , &  ! Optional output
!                                 Message_Log   =Message_Log     )  ! Error messaging
!
! INPUTS:
!       Filename:      Character string specifying the name of the
!                         netCDF AtmProfile format data file to read from.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       FileId:        NetCDF file ID number.
!                         function.
!                         UNITS:      N/A
!                         TYPE:       Integer
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
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
! OPTIONAL OUTPUTS:
!       Release:          The release number of the netCDF AtmProfile file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Version:          The version number of the netCDF AtmProfile file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Profile_Set_Id:   Character string written into the PROFILE_SET_ID global
!                         attribute field of the netCDF AtmProfile file.
!                         Should contain a short tag used to identify the
!                         dependent profile set.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF AtmProfile file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF AtmProfile file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF AtmProfile file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       err_stat:     The return value is an integer defining the error status.
!                         The error codes are defined in the Message_Handler module.
!                         If == SUCCESS the global attribute read was successful.
!                            == FAILURE an error occurred.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION ReadGAtts( Filename     , &  ! Input
                      FileId       , &  ! Input
                      Release         , &  ! Optional output
                      Version         , &  ! Optional output
                      Profile_Set_Id  , &  ! Optional output
                      Title           , &  ! Optional output
                      History         , &  ! Optional output
                      Comment         , &  ! Optional output
                      Message_Log     ) &  ! Error messaging
                    RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER,                INTENT(IN)  :: FileId
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release         
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Profile_Set_Id
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ReadGAtts'
    ! Local variables
    CHARACTER(256)  :: GAttName
    CHARACTER(5000) :: GAttString
    INTEGER :: Rel
    INTEGER :: NF90_Status
    TYPE(AtmProfile_type) :: AtmProfile_Default

    ! Set up
    ! ------
    err_stat = SUCCESS


    ! The mandatory GAtts for checking
    ! --------------------------------
    ! The Release
    IF ( PRESENT(Release) ) THEN
      GAttName = RELEASE_GATTNAME
      NF90_Status = NF90_GET_ATT( FileId, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  Rel )
      IF ( NF90_Status /= NF90_NOERR .OR. Rel /= AtmProfile_Default%Release) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      Release = AtmProfile_Default%Release
    END IF

    ! The optional GAtts
    ! ------------------
    ! The Version
    IF ( PRESENT(Version) ) THEN
      GAttName = VERSION_GATTNAME
      NF90_Status = NF90_GET_ATT( FileId, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  Version )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The Profile_Set_Id
    IF ( PRESENT(Profile_Set_Id) ) THEN
      GAttString = ' '; Profile_Set_Id = ' '
      GAttName = PROFILE_SET_ID_GATTNAME
      NF90_Status = NF90_GET_ATT( FileId, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL StrClean( GAttString )
      Profile_Set_Id = GAttString(1:MIN( LEN(Profile_Set_Id), LEN_TRIM(GAttString) ))
    END IF

    ! The Title
    IF ( PRESENT(Title) ) THEN
      GAttString = ' '; Title = ' '
      GAttName = TITLE_GATTNAME
      NF90_Status = NF90_GET_ATT( FileId, &
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
      NF90_Status = NF90_GET_ATT( FileId, &
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
      NF90_Status = NF90_GET_ATT( FileId, &
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
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//TRIM(GAttName)//&
                            ' attribute from '//TRIM(Filename)//' - '// &
                            TRIM(NF90_STRERROR( NF90_Status ) ), &
                            err_stat, &
                            Message_Log=Message_Log )
    END SUBROUTINE ReadGAtts_CleanUp

  END FUNCTION ReadGAtts


!------------------------------------------------------------------------------
!
! NAME:
!       CreateFile
!
! PURPOSE:
!       Function to create a AtmProfile file for writing.
!
! CALLING SEQUENCE:
!       err_stat = CreateFile( &
!                    Filename                       , &  ! Input
!                    n_Layers                       , &  ! Input
!                    n_Absorbers                    , &  ! Input
!                    FileInfo                       , &  ! Output
!                    Version        = Version       , &  ! Optional input
!                    Title          = Title         , &  ! Optional input
!                    History        = History       , &  ! Optional input
!                    Comment        = Comment       , &  ! Optional input
!                    Profile_Set_Id = Profile_Set_Id  )  ! Optional input
!
! INPUTS:
!       Filename:           Character string specifying the name of the
!                           AtmProfile file to create.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Layers:           Number of profile layers.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Absorbers:        Number of profile absorbers.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       FileInfo:           Structure containing the created file information.
!                           UNITS:      N/A
!                           TYPE:       TYPE(FileInfo_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Version:            The version number of the netCDF AtmProfile file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Profile_Set_Id:     Character string written into the PROFILE_SET_ID global
!                           attribute field of the netCDF AtmProfile file.
!                           Should contain a short tag used to identify the
!                           dependent profile set.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:              Character string written into the TITLE global
!                           attribute field of the netCDF AtmProfile file.
!                           Should contain a succinct description of what
!                           is in the netCDF datafile.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the netCDF AtmProfile file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF AtmProfile file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Profile_Set_Id:     Character string written into the PROFILE_SET_ID global
!                           attribute field of the netCDF AtmProfile file.
!                           Should contain a short tag used to identify the
!                           dependent profile set.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       err_stat:           The return value is an integer defining the error status.  
!                           The error codes are defined in the Message_Handler module. 
!                           If == SUCCESS the file creation was successful.     
!                              == FAILURE an unrecoverable error occurred.             
!                           UNITS:      N/A                                            
!                           TYPE:       INTEGER                                        
!                           DIMENSION:  Scalar                                         
!
!------------------------------------------------------------------------------
  
  FUNCTION CreateFile( &
    Filename      , &  ! Input
    n_Layers      , &  ! Input
    n_Absorbers   , &  ! Input
    FileInfo      , &  ! Output
    Version       , &  ! Optional input
    Title         , &  ! Optional input
    History       , &  ! Optional input
    Comment       , &  ! Optional input
    Profile_Set_Id) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)  :: Filename
    INTEGER               , INTENT(IN)  :: n_Layers     
    INTEGER               , INTENT(IN)  :: n_Absorbers  
    TYPE(FileInfo_type)   , INTENT(OUT) :: FileInfo
    INTEGER     , OPTIONAL, INTENT(IN)  :: Version
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Profile_Set_Id
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CreateFile'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: Close_File
    INTEGER :: NF90_Status(4)
    TYPE(AtmProfile_type) :: ap


     ! Setup
    err_stat = SUCCESS
    Close_File = .FALSE.
    ! ...Check input
    IF ( n_Layers    < 1 .OR. &
         n_Absorbers < 1      ) THEN
      msg = 'Invalid dimension input detected.'
      CALL Create_Cleanup(); RETURN
    END IF


    ! Create the data file
    NF90_Status(1) = NF90_CREATE( Filename,NF90_CLOBBER,FileInfo%Id )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error creating '//TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    Close_File = .TRUE.


    ! Define the dimensions
    ! ...Number of levels
    NF90_Status(1) = NF90_DEF_DIM( FileInfo%Id,LEVEL_DIMNAME,n_Layers+1,FileInfo%DimId%Level )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//LEVEL_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Number of layers
    NF90_Status(1) = NF90_DEF_DIM( FileInfo%Id,LAYER_DIMNAME,n_Layers,FileInfo%DimId%Layer )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//LAYER_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Number of absorbers
    NF90_Status(1) = NF90_DEF_DIM( FileInfo%Id,ABSORBER_DIMNAME,n_Absorbers,FileInfo%DimId%Absorber )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//ABSORBER_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Profile description string length
    NF90_Status(1) = NF90_DEF_DIM( FileInfo%Id,DESCRIPTION_DIMNAME,LEN(ap%Description),FileInfo%DimId%PL )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//DESCRIPTION_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Number of profiles (unlimited)
    NF90_Status(1) = NF90_DEF_DIM( FileInfo%Id,PROFILE_DIMNAME,NF90_UNLIMITED,FileInfo%DimId%Profile )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//PROFILE_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(); RETURN
    END IF


    ! Write the global attributes
    err_stat = WriteGAtts( Filename   , &
                           FileInfo%Id, &
                           Version        = Version       , &
                           Title          = Title         , &
                           History        = History       , &
                           Comment        = Comment       , &
                           Profile_Set_Id = Profile_Set_Id  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing global attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF


    ! Define the variables
    ! ...The Description
    NF90_Status(1) = NF90_DEF_VAR( FileInfo%Id,DESCRIPTION_VARNAME,DESCRIPTION_TYPE, &
                                   dimIDs=(/FileInfo%DimId%PL,FileInfo%DimId%Profile/), &
                                   varId=FileInfo%VarId%Description )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//DESCRIPTION_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Description,&
                                   LONGNAME_ATTNAME,DESCRIPTION_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Description,&
                                   DESCRIPTION_ATTNAME,DESCRIPTION_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Description,&
                                   UNITS_ATTNAME,DESCRIPTION_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Description,&
                                   FILLVALUE_ATTNAME,DESCRIPTION_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//DESCRIPTION_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...The Climatology_Model
    NF90_Status(1) = NF90_DEF_VAR( FileInfo%Id,CLIMATOLOGY_MODEL_VARNAME,CLIMATOLOGY_MODEL_TYPE, &
                                   dimIDs=(/FileInfo%DimId%Profile/),&
                                   varId=FileInfo%VarId%Climatology_Model )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//CLIMATOLOGY_MODEL_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Climatology_Model,&
                                   LONGNAME_ATTNAME,CLIMATOLOGY_MODEL_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Climatology_Model,&
                                   DESCRIPTION_ATTNAME,CLIMATOLOGY_MODEL_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Climatology_Model,&
                                   UNITS_ATTNAME,CLIMATOLOGY_MODEL_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Climatology_Model,&
                                   FILLVALUE_ATTNAME,CLIMATOLOGY_MODEL_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//CLIMATOLOGY_MODEL_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...The year
    NF90_Status(1) = NF90_DEF_VAR( FileInfo%Id,YEAR_VARNAME,YEAR_TYPE, &
                                   dimIDs=(/FileInfo%DimId%Profile/),&
                                   varId=FileInfo%VarId%Year )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//YEAR_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Year,&
                                   LONGNAME_ATTNAME,YEAR_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Year,&
                                   DESCRIPTION_ATTNAME,YEAR_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Year,&
                                   UNITS_ATTNAME,YEAR_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Year,&
                                   FILLVALUE_ATTNAME,YEAR_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//YEAR_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...The Month
    NF90_Status(1) = NF90_DEF_VAR( FileInfo%Id,MONTH_VARNAME,MONTH_TYPE, &
                                   dimIDs=(/FileInfo%DimId%Profile/),&
                                   varId=FileInfo%VarId%Month )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//MONTH_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Month,&
                                   LONGNAME_ATTNAME,MONTH_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Month,&
                                   DESCRIPTION_ATTNAME,MONTH_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Month,&
                                   UNITS_ATTNAME,MONTH_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Month,&
                                   FILLVALUE_ATTNAME,MONTH_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//MONTH_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...The Day
    NF90_Status(1) = NF90_DEF_VAR( FileInfo%Id,DAY_VARNAME,DAY_TYPE, &
                                   dimIDs=(/FileInfo%DimId%Profile/),&
                                   varId=FileInfo%VarId%Day )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//DAY_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Day,&
                                   LONGNAME_ATTNAME,DAY_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Day,&
                                   DESCRIPTION_ATTNAME,DAY_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Day,&
                                   UNITS_ATTNAME,DAY_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Day,&
                                   FILLVALUE_ATTNAME,DAY_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//DAY_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...The Hour
    NF90_Status(1) = NF90_DEF_VAR( FileInfo%Id,HOUR_VARNAME,HOUR_TYPE,&
                                   dimIDs=(/FileInfo%DimId%Profile/),&
                                   varId=FileInfo%VarId%Hour )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//HOUR_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Hour,&
                                   LONGNAME_ATTNAME,HOUR_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Hour,&
                                   DESCRIPTION_ATTNAME,HOUR_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Hour,&
                                   UNITS_ATTNAME,HOUR_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Hour,&
                                   FILLVALUE_ATTNAME,HOUR_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//HOUR_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...The Latitude
    NF90_Status(1) = NF90_DEF_VAR( FileInfo%Id,LATITUDE_VARNAME,LATITUDE_TYPE, &
                                   dimIDs=(/FileInfo%DimId%Profile/),&
                                   varId=FileInfo%VarId%Latitude )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//LATITUDE_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Latitude,&
                                   LONGNAME_ATTNAME,LATITUDE_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Latitude,&
                                   DESCRIPTION_ATTNAME,LATITUDE_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Latitude,&
                                   UNITS_ATTNAME,LATITUDE_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Latitude,&
                                   FILLVALUE_ATTNAME,LATITUDE_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//LATITUDE_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...The Longitude
    NF90_Status(1) = NF90_DEF_VAR( FileInfo%Id,LONGITUDE_VARNAME,LONGITUDE_TYPE, &
                                   dimIDs=(/FileInfo%DimId%Profile/),&
                                   varId=FileInfo%VarId%Longitude )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//LONGITUDE_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Longitude,&
                                   LONGNAME_ATTNAME,LONGITUDE_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Longitude,&
                                   DESCRIPTION_ATTNAME,LONGITUDE_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Longitude,&
                                   UNITS_ATTNAME,LONGITUDE_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Longitude,&
                                   FILLVALUE_ATTNAME,LONGITUDE_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//LONGITUDE_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...The Surface_Altitude
    NF90_Status(1) = NF90_DEF_VAR( FileInfo%Id,SURFACE_ALTITUDE_VARNAME,SURFACE_ALTITUDE_TYPE, &
                                   dimIDs=(/FileInfo%DimId%Profile/),&
                                   varId=FileInfo%VarId%Surface_Altitude )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//SURFACE_ALTITUDE_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Surface_Altitude,&
                                   LONGNAME_ATTNAME,SURFACE_ALTITUDE_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Surface_Altitude,&
                                   DESCRIPTION_ATTNAME,SURFACE_ALTITUDE_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Surface_Altitude,&
                                   UNITS_ATTNAME,SURFACE_ALTITUDE_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Surface_Altitude,&
                                   FILLVALUE_ATTNAME,SURFACE_ALTITUDE_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//SURFACE_ALTITUDE_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...The Absorber_Id
    NF90_Status(1) = NF90_DEF_VAR( FileInfo%Id,ABSORBER_ID_VARNAME,ABSORBER_ID_TYPE, &
                                   dimIDs=(/FileInfo%DimId%Absorber/),&
                                   varId=FileInfo%VarId%Absorber_Id )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//ABSORBER_ID_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Absorber_Id,&
                                   LONGNAME_ATTNAME,ABSORBER_ID_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Absorber_Id,&
                                   DESCRIPTION_ATTNAME,ABSORBER_ID_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Absorber_Id,&
                                   UNITS_ATTNAME,ABSORBER_ID_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Absorber_Id,&
                                   FILLVALUE_ATTNAME,ABSORBER_ID_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//ABSORBER_ID_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...The Absorber_Units_Id
    NF90_Status(1) = NF90_DEF_VAR( FileInfo%Id,ABSORBER_UNITS_ID_VARNAME,ABSORBER_UNITS_ID_TYPE, &
                                   dimIDs=(/FileInfo%DimId%Absorber/),&
                                   varId=FileInfo%VarId%Absorber_Units_Id )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//ABSORBER_UNITS_ID_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Absorber_Units_Id,&
                                   LONGNAME_ATTNAME,ABSORBER_UNITS_ID_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Absorber_Units_Id,&
                                   DESCRIPTION_ATTNAME,ABSORBER_UNITS_ID_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Absorber_Units_Id,&
                                   UNITS_ATTNAME,ABSORBER_UNITS_ID_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Absorber_Units_Id,&
                                   FILLVALUE_ATTNAME,ABSORBER_UNITS_ID_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//ABSORBER_UNITS_ID_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...The Level_Pressure
    NF90_Status(1) = NF90_DEF_VAR( FileInfo%Id,LEVEL_PRESSURE_VARNAME,LEVEL_PRESSURE_TYPE, &
                                   dimIDs=(/FileInfo%DimId%Level,FileInfo%DimId%Profile/),&
                                   varId=FileInfo%VarId%Level_Pressure )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//LEVEL_PRESSURE_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Level_Pressure,&
                                   LONGNAME_ATTNAME,LEVEL_PRESSURE_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Level_Pressure,&
                                   DESCRIPTION_ATTNAME,LEVEL_PRESSURE_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Level_Pressure,&
                                   UNITS_ATTNAME,LEVEL_PRESSURE_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Level_Pressure,&
                                   FILLVALUE_ATTNAME,LEVEL_PRESSURE_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//LEVEL_PRESSURE_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...The Level_Temperature
    NF90_Status(1) = NF90_DEF_VAR( FileInfo%Id,LEVEL_TEMPERATURE_VARNAME,LEVEL_TEMPERATURE_TYPE, &
                                   dimIDs=(/FileInfo%DimId%Level,FileInfo%DimId%Profile/),&
                                   varId=FileInfo%VarId%Level_Temperature )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//LEVEL_TEMPERATURE_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Level_Temperature,&
                                   LONGNAME_ATTNAME,LEVEL_TEMPERATURE_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Level_Temperature,&
                                   DESCRIPTION_ATTNAME,LEVEL_TEMPERATURE_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Level_Temperature,&
                                   UNITS_ATTNAME,LEVEL_TEMPERATURE_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Level_Temperature,&
                                   FILLVALUE_ATTNAME,LEVEL_TEMPERATURE_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//LEVEL_TEMPERATURE_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...The Level_Absorber
    NF90_Status(1) = NF90_DEF_VAR( FileInfo%Id,LEVEL_ABSORBER_VARNAME,LEVEL_ABSORBER_TYPE, &
                                   dimIDs=(/FileInfo%DimId%Level,FileInfo%DimId%Absorber,FileInfo%DimId%Profile/),&
                                   varId=FileInfo%VarId%Level_Absorber )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//LEVEL_ABSORBER_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Level_Absorber,&
                                   LONGNAME_ATTNAME,LEVEL_ABSORBER_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Level_Absorber,&
                                   DESCRIPTION_ATTNAME,LEVEL_ABSORBER_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Level_Absorber,&
                                   UNITS_ATTNAME,LEVEL_ABSORBER_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Level_Absorber,&
                                   FILLVALUE_ATTNAME,LEVEL_ABSORBER_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//LEVEL_ABSORBER_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...The Level_Altitude
    NF90_Status(1) = NF90_DEF_VAR( FileInfo%Id,LEVEL_ALTITUDE_VARNAME,LEVEL_ALTITUDE_TYPE, &
                                   dimIDs=(/FileInfo%DimId%Level,FileInfo%DimId%Profile/),&
                                   varId=FileInfo%VarId%Level_Altitude )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//LEVEL_ALTITUDE_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Level_Altitude,&
                                   LONGNAME_ATTNAME,LEVEL_ALTITUDE_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Level_Altitude,&
                                   DESCRIPTION_ATTNAME,LEVEL_ALTITUDE_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Level_Altitude,&
                                   UNITS_ATTNAME,LEVEL_ALTITUDE_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Level_Altitude,&
                                   FILLVALUE_ATTNAME,LEVEL_ALTITUDE_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//LEVEL_ALTITUDE_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...The Layer_Pressure
    NF90_Status(1) = NF90_DEF_VAR( FileInfo%Id,LAYER_PRESSURE_VARNAME,LAYER_PRESSURE_TYPE, &
                                   dimIDs=(/FileInfo%DimId%Layer,FileInfo%DimId%Profile/),&
                                   varId=FileInfo%VarId%Layer_Pressure )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//LAYER_PRESSURE_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Layer_Pressure,&
                                   LONGNAME_ATTNAME,LAYER_PRESSURE_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Layer_Pressure,&
                                   DESCRIPTION_ATTNAME,LAYER_PRESSURE_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Layer_Pressure,&
                                   UNITS_ATTNAME,LAYER_PRESSURE_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Layer_Pressure,&
                                   FILLVALUE_ATTNAME,LAYER_PRESSURE_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//LAYER_PRESSURE_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...The Layer_Temperature
    NF90_Status(1) = NF90_DEF_VAR( FileInfo%Id,LAYER_TEMPERATURE_VARNAME,LAYER_TEMPERATURE_TYPE, &
                                   dimIDs=(/FileInfo%DimId%Layer,FileInfo%DimId%Profile/),&
                                   varId=FileInfo%VarId%Layer_Temperature )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//LAYER_TEMPERATURE_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Layer_Temperature,&
                                   LONGNAME_ATTNAME,LAYER_TEMPERATURE_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Layer_Temperature,&
                                   DESCRIPTION_ATTNAME,LAYER_TEMPERATURE_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Layer_Temperature,&
                                   UNITS_ATTNAME,LAYER_TEMPERATURE_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Layer_Temperature,&
                                   FILLVALUE_ATTNAME,LAYER_TEMPERATURE_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//LAYER_TEMPERATURE_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...The Layer_Absorber
    NF90_Status(1) = NF90_DEF_VAR( FileInfo%Id,LAYER_ABSORBER_VARNAME,LAYER_ABSORBER_TYPE, &
                                   dimIDs=(/FileInfo%DimId%Layer,FileInfo%DimId%Absorber,FileInfo%DimId%Profile/),&
                                   varId=FileInfo%VarId%Layer_Absorber )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//LAYER_ABSORBER_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Layer_Absorber,&
                                   LONGNAME_ATTNAME,LAYER_ABSORBER_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Layer_Absorber,&
                                   DESCRIPTION_ATTNAME,LAYER_ABSORBER_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Layer_Absorber,&
                                   UNITS_ATTNAME,LAYER_ABSORBER_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Layer_Absorber,&
                                   FILLVALUE_ATTNAME,LAYER_ABSORBER_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//LAYER_ABSORBER_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...The Layer_Delta_Z
    NF90_Status(1) = NF90_DEF_VAR( FileInfo%Id,LAYER_DELTA_Z_VARNAME,LAYER_DELTA_Z_TYPE, &
                                   dimIDs=(/FileInfo%DimId%Layer,FileInfo%DimId%Profile/),&
                                   varId=FileInfo%VarId%Layer_Delta_Z )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//LAYER_DELTA_Z_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Layer_Delta_Z,&
                                   LONGNAME_ATTNAME,LAYER_DELTA_Z_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Layer_Delta_Z,&
                                   DESCRIPTION_ATTNAME,LAYER_DELTA_Z_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Layer_Delta_Z,&
                                   UNITS_ATTNAME,LAYER_DELTA_Z_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( FileInfo%Id,FileInfo%VarId%Layer_Delta_Z,&
                                   FILLVALUE_ATTNAME,LAYER_DELTA_Z_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//LAYER_DELTA_Z_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    
                                             
    ! Take netCDF file out of define mode
    NF90_Status(1) = NF90_ENDDEF( FileInfo%Id )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error taking file '//TRIM(Filename)// &
            ' out of define mode - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Create_Cleanup(); RETURN
    END IF

  CONTAINS
 
    SUBROUTINE Create_CleanUp()
      IF ( Close_File ) THEN
        NF90_Status(1) = NF90_CLOSE( FileInfo%Id )
        IF ( NF90_Status(1) /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
                TRIM(NF90_STRERROR( NF90_Status(1) ))
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Create_CleanUp
    
  END FUNCTION CreateFile



  FUNCTION GetInfo( &
    Filename, &
    FileInfo) &
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),        INTENT(IN)  :: Filename
    TYPE(FileInfo_type), INTENT(OUT) :: FileInfo
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'AtmProfile_netCDF_GetInfo'
    ! Variables
    CHARACTER(ML) :: msg
    LOGICAL :: Close_File
    INTEGER :: NF90_Status
    ! Set up
    err_stat = SUCCESS
    Close_File = .FALSE.


    ! Open the file
    NF90_Status = NF90_OPEN( Filename,NF90_NOWRITE,FileInfo%Id )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error opening '//TRIM(Filename)//' for read access - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL GetInfo_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    Close_File = .TRUE.


    ! Get the dimension Ids
    ! ...Level dimension
    NF90_Status = NF90_INQ_DIMID( &
                    FileInfo%Id, &
                    LEVEL_DIMNAME, &
                    FileInfo%DimId%Level )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//LEVEL_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL GetInfo_Cleanup(); RETURN
    END IF
    ! ...Layer dimension
    NF90_Status = NF90_INQ_DIMID( &
                    FileInfo%Id, &
                    LAYER_DIMNAME, &
                    FileInfo%DimId%Layer )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//LAYER_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL GetInfo_Cleanup(); RETURN
    END IF
    ! ...Absorber dimension
    NF90_Status = NF90_INQ_DIMID( &
                    FileInfo%Id, &
                    ABSORBER_DIMNAME, &
                    FileInfo%DimId%Absorber )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//ABSORBER_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL GetInfo_Cleanup(); RETURN
    END IF
    ! ...Profile dimension
    NF90_Status = NF90_INQ_DIMID( &
                    FileInfo%Id, &
                    PROFILE_DIMNAME, &
                    FileInfo%DimId%Profile )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//PROFILE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL GetInfo_Cleanup(); RETURN
    END IF
    ! ...PL dimension
    NF90_Status = NF90_INQ_DIMID( &
                    FileInfo%Id, &
                    DESCRIPTION_DIMNAME, &
                    FileInfo%DimId%PL )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//DESCRIPTION_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL GetInfo_Cleanup(); RETURN
    END IF


    ! Get the variable Ids
    ! ...Description Id
    NF90_Status = NF90_INQ_VARID( &
                    FileInfo%Id, &
                    DESCRIPTION_VARNAME, &
                    FileInfo%VarId%Description )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//DESCRIPTION_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL GetInfo_Cleanup(); RETURN
    END IF
    ! ...Climatology_Model Id
    NF90_Status = NF90_INQ_VARID( &
                    FileInfo%Id, &
                    CLIMATOLOGY_MODEL_VARNAME, &
                    FileInfo%VarId%Climatology_Model )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//CLIMATOLOGY_MODEL_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL GetInfo_Cleanup(); RETURN
    END IF
    ! ...Year Id
    NF90_Status = NF90_INQ_VARID( &
                    FileInfo%Id, &
                    YEAR_VARNAME, &
                    FileInfo%VarId%Year )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//YEAR_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL GetInfo_Cleanup(); RETURN
    END IF
    ! ...Month Id
    NF90_Status = NF90_INQ_VARID( &
                    FileInfo%Id, &
                    MONTH_VARNAME, &
                    FileInfo%VarId%Month )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//MONTH_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL GetInfo_Cleanup(); RETURN
    END IF
    ! ...Day Id
    NF90_Status = NF90_INQ_VARID( &
                    FileInfo%Id, &
                    DAY_VARNAME, &
                    FileInfo%VarId%Day )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//DAY_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL GetInfo_Cleanup(); RETURN
    END IF
    ! ...Hour Id
    NF90_Status = NF90_INQ_VARID( &
                    FileInfo%Id, &
                    HOUR_VARNAME, &
                    FileInfo%VarId%Hour )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//HOUR_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL GetInfo_Cleanup(); RETURN
    END IF
    ! ...Latitude Id
    NF90_Status = NF90_INQ_VARID( &
                    FileInfo%Id, &
                    LATITUDE_VARNAME, &
                    FileInfo%VarId%Latitude )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//LATITUDE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL GetInfo_Cleanup(); RETURN
    END IF
    ! ...Longitude Id
    NF90_Status = NF90_INQ_VARID( &
                    FileInfo%Id, &
                    LONGITUDE_VARNAME, &
                    FileInfo%VarId%Longitude )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//LONGITUDE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL GetInfo_Cleanup(); RETURN
    END IF
    ! ...Surface_Altitude Id
    NF90_Status = NF90_INQ_VARID( &
                    FileInfo%Id, &
                    SURFACE_ALTITUDE_VARNAME, &
                    FileInfo%VarId%Surface_Altitude )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//SURFACE_ALTITUDE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL GetInfo_Cleanup(); RETURN
    END IF
    ! ...Absorber_Id Id
    NF90_Status = NF90_INQ_VARID( &
                    FileInfo%Id, &
                    ABSORBER_ID_VARNAME, &
                    FileInfo%VarId%Absorber_Id )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//ABSORBER_ID_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL GetInfo_Cleanup(); RETURN
    END IF
    ! ...Absorber_Units_Id Id
    NF90_Status = NF90_INQ_VARID( &
                    FileInfo%Id, &
                    ABSORBER_UNITS_ID_VARNAME, &
                    FileInfo%VarId%Absorber_Units_Id )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//ABSORBER_UNITS_ID_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL GetInfo_Cleanup(); RETURN
    END IF
    ! ...Level_Pressure Id
    NF90_Status = NF90_INQ_VARID( &
                    FileInfo%Id, &
                    LEVEL_PRESSURE_VARNAME, &
                    FileInfo%VarId%Level_Pressure )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//LEVEL_PRESSURE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL GetInfo_Cleanup(); RETURN
    END IF
    ! ...Level_Temperature Id
    NF90_Status = NF90_INQ_VARID( &
                    FileInfo%Id, &
                    LEVEL_TEMPERATURE_VARNAME, &
                    FileInfo%VarId%Level_Temperature )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//LEVEL_TEMPERATURE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL GetInfo_Cleanup(); RETURN
    END IF
    ! ...Level_Absorber Id
    NF90_Status = NF90_INQ_VARID( &
                    FileInfo%Id, &
                    LEVEL_ABSORBER_VARNAME, &
                    FileInfo%VarId%Level_Absorber )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//LEVEL_ABSORBER_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL GetInfo_Cleanup(); RETURN
    END IF
    ! ...Level_Altitude Id
    NF90_Status = NF90_INQ_VARID( &
                    FileInfo%Id, &
                    LEVEL_ALTITUDE_VARNAME, &
                    FileInfo%VarId%Level_Altitude )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//LEVEL_ALTITUDE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL GetInfo_Cleanup(); RETURN
    END IF
    ! ...Layer_Pressure Id
    NF90_Status = NF90_INQ_VARID( &
                    FileInfo%Id, &
                    LAYER_PRESSURE_VARNAME, &
                    FileInfo%VarId%Layer_Pressure )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//LAYER_PRESSURE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL GetInfo_Cleanup(); RETURN
    END IF
    ! ...Layer_Temperature Id
    NF90_Status = NF90_INQ_VARID( &
                    FileInfo%Id, &
                    LAYER_TEMPERATURE_VARNAME, &
                    FileInfo%VarId%Layer_Temperature )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//LAYER_TEMPERATURE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL GetInfo_Cleanup(); RETURN
    END IF
    ! ...Layer_Absorber Id
    NF90_Status = NF90_INQ_VARID( &
                    FileInfo%Id, &
                    LAYER_ABSORBER_VARNAME, &
                    FileInfo%VarId%Layer_Absorber )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//LAYER_ABSORBER_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL GetInfo_Cleanup(); RETURN
    END IF
    ! ...Layer_Delta_Z Id
    NF90_Status = NF90_INQ_VARID( &
                    FileInfo%Id, &
                    LAYER_DELTA_Z_VARNAME, &
                    FileInfo%VarId%Layer_Delta_Z )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//LAYER_DELTA_Z_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL GetInfo_Cleanup(); RETURN
    END IF
    
    
    ! Close the file
    NF90_Status = NF90_CLOSE( FileInfo%Id )
    Close_File = .FALSE.
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error closing input file - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL GetInfo_Cleanup(); RETURN
    END IF
    
  CONTAINS
  
    SUBROUTINE GetInfo_CleanUp()
      IF ( Close_File ) THEN
        NF90_Status = NF90_CLOSE( FileInfo%Id )
        IF ( NF90_Status /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup.'
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE GetInfo_CleanUp

  END FUNCTION GetInfo


END MODULE AtmProfile_netCDF_IO
