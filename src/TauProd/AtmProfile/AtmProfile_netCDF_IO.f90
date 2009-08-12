!
! AtmProfile_netCDF_IO
!
! Module containing routines to read and write AtmProfile netCDF 
! format files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 08-Jul-2002
!                       paul.vandelst@noaa.gov
!

MODULE AtmProfile_netCDF_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds     ,   ONLY: Long, Double
  USE Message_Handler,   ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                               Display_Message
  USE String_Utility,    ONLY: StrClean
  USE AtmProfile_Define, ONLY: ATMPROFILE_ABSORBER_UNITS_NAME, &
                               ATMPROFILE_ABSORBER_UNITS_CHAR, &
                               ATMPROFILE_FP_INVALID, &
                               AtmProfile_type, &
                               Associated_AtmProfile, &
                               Destroy_AtmProfile, &
                               Allocate_AtmProfile, &
                               CheckRelease_AtmProfile, &
                               Info_AtmProfile
  USE netcdf
  ! Disable implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Inquire_AtmProfile_netCDF
  PUBLIC :: Read_AtmProfile_netCDF
  PUBLIC :: Write_AtmProfile_netCDF

  ! -----------------
  ! Module parameters
  ! -----------------
  ! Module RCS Id string
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
    '$Id$'
  ! Keyword set value
  INTEGER,      PARAMETER :: SET = 1
  ! String lengths
  INTEGER,      PARAMETER :: ML    = 512
  INTEGER,      PARAMETER :: PDSL  = 512
  INTEGER,      PARAMETER :: AUNSL =  32
  ! Literal constants
  REAL(Double), PARAMETER :: ZERO = 0.0_Double
  REAL(Double), PARAMETER :: ONE  = 1.0_Double

  ! Global attribute names. Case sensitive
  CHARACTER(*), PARAMETER :: TITLE_GATTNAME          = 'title' 
  CHARACTER(*), PARAMETER :: HISTORY_GATTNAME        = 'history'
  CHARACTER(*), PARAMETER :: COMMENT_GATTNAME        = 'comment'
  CHARACTER(*), PARAMETER :: RELEASE_GATTNAME        = 'Release'
  CHARACTER(*), PARAMETER :: VERSION_GATTNAME        = 'Version'
  CHARACTER(*), PARAMETER :: PROFILE_SET_ID_GATTNAME = 'Profile_Set_Id'
  ! Dimension names
  CHARACTER(*), PARAMETER :: LEVEL_DIMNAME          = 'n_levels'
  CHARACTER(*), PARAMETER :: LAYER_DIMNAME          = 'n_layers'
  CHARACTER(*), PARAMETER :: ABSORBER_DIMNAME       = 'n_absorbers'
  CHARACTER(*), PARAMETER :: PROFILE_DIMNAME        = 'n_profiles'
  CHARACTER(*), PARAMETER :: DESCRIPTION_DIMNAME    = 'pdsl'
  CHARACTER(*), PARAMETER :: ABSORBER_UNITS_DIMNAME = 'aunsl'
  
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
  CHARACTER(*), PARAMETER :: ABSORBER_UNITS_NAME_VARNAME = 'absorber_units_name'
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
  CHARACTER(*), PARAMETER :: ABSORBER_UNITS_NAME_LONGNAME = 'Absorber Units Name'
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
  CHARACTER(*), PARAMETER :: YEAR_DESCRIPTION                = 'Year'
  CHARACTER(*), PARAMETER :: MONTH_DESCRIPTION               = 'Month'
  CHARACTER(*), PARAMETER :: HOUR_DESCRIPTION                = 'Hour'
  CHARACTER(*), PARAMETER :: DAY_DESCRIPTION                 = 'Day'
  CHARACTER(*), PARAMETER :: LATITUDE_DESCRIPTION            = 'Latitude of profile location'
  CHARACTER(*), PARAMETER :: LONGITUDE_DESCRIPTION           = 'Longitude of profile location'
  CHARACTER(*), PARAMETER :: SURFACE_ALTITUDE_DESCRIPTION    = 'Surface altitude of profile'
  CHARACTER(*), PARAMETER :: ABSORBER_ID_DESCRIPTION         = 'HITRAN/LBLRTM absorber ID number for atmospheric absorbers'
  CHARACTER(*), PARAMETER :: ABSORBER_UNITS_ID_DESCRIPTION   = 'LBLRTM/MonoRTM absorber units ID number'
  CHARACTER(*), PARAMETER :: ABSORBER_UNITS_NAME_DESCRIPTION = 'Absorber Units Name'
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
  CHARACTER(*), PARAMETER :: HOUR_UNITS                = 'Hour of day'
  CHARACTER(*), PARAMETER :: LATITUDE_UNITS            = 'degress North (-90->+90)'
  CHARACTER(*), PARAMETER :: LONGITUDE_UNITS           = 'degress East (0->360)'
  CHARACTER(*), PARAMETER :: SURFACE_ALTITUDE_UNITS    = 'metres (m)'
  CHARACTER(*), PARAMETER :: ABSORBER_ID_UNITS         = 'N/A'
  CHARACTER(*), PARAMETER :: ABSORBER_UNITS_ID_UNITS   = 'N/A'
  CHARACTER(*), PARAMETER :: ABSORBER_UNITS_NAME_UNITS = 'N/A'
  CHARACTER(*), PARAMETER :: LEVEL_PRESSURE_UNITS      = 'hectoPascals (hPa)'
  CHARACTER(*), PARAMETER :: LEVEL_TEMPERATURE_UNITS   = 'Kelvin (K)'
  CHARACTER(*), PARAMETER :: LEVEL_ABSORBER_UNITS      = 'Variable (see Absorber_Units_Name)'
  CHARACTER(*), PARAMETER :: LEVEL_ALTITUDE_UNITS      = 'metres (m)'
  CHARACTER(*), PARAMETER :: LAYER_PRESSURE_UNITS      = 'hectoPascals (hPa)'
  CHARACTER(*), PARAMETER :: LAYER_TEMPERATURE_UNITS   = 'Kelvin (K)'
  CHARACTER(*), PARAMETER :: LAYER_ABSORBER_UNITS      = 'Variable (see Absorber_Units_Name)'
  CHARACTER(*), PARAMETER :: LAYER_DELTA_Z_UNITS       = 'metres (m)'

  ! Variable fill value attribute
  CHARACTER(*), PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'

  CHARACTER(*) , PARAMETER :: DESCRIPTION_FILLVALUE          = NF90_FILL_CHAR
  INTEGER(Long), PARAMETER :: CLIMATOLOGY_MODEL_FILLVALUE    = 0
  INTEGER(Long), PARAMETER :: YEAR_FILLVALUE                 = 0
  INTEGER(Long), PARAMETER :: MONTH_FILLVALUE                = 0
  INTEGER(Long), PARAMETER :: DAY_FILLVALUE                  = 0
  INTEGER(Long), PARAMETER :: HOUR_FILLVALUE                 = 0
  REAL(Double) , PARAMETER :: LATITUDE_FILLVALUE             = ATMPROFILE_FP_INVALID
  REAL(Double) , PARAMETER :: LONGITUDE_FILLVALUE            = ATMPROFILE_FP_INVALID
  REAL(Double) , PARAMETER :: SURFACE_ALTITUDE_FILLVALUE     = ATMPROFILE_FP_INVALID
  INTEGER(Long), PARAMETER :: ABSORBER_ID_FILLVALUE          = 0
  INTEGER(Long), PARAMETER :: ABSORBER_UNITS_ID_FILLVALUE    = 0
  CHARACTER(*) , PARAMETER :: ABSORBER_UNITS_NAME_FILLVALUE  = NF90_FILL_CHAR
  REAL(Double) , PARAMETER :: LEVEL_PRESSURE_FILLVALUE       = ATMPROFILE_FP_INVALID
  REAL(Double) , PARAMETER :: LEVEL_TEMPERATURE_FILLVALUE    = ATMPROFILE_FP_INVALID
  REAL(Double) , PARAMETER :: LEVEL_ABSORBER_FILLVALUE       = ATMPROFILE_FP_INVALID
  REAL(Double) , PARAMETER :: LEVEL_ALTITUDE_FILLVALUE       = ATMPROFILE_FP_INVALID
  REAL(Double) , PARAMETER :: LAYER_PRESSURE_FILLVALUE       = ATMPROFILE_FP_INVALID
  REAL(Double) , PARAMETER :: LAYER_TEMPERATURE_FILLVALUE    = ATMPROFILE_FP_INVALID
  REAL(Double) , PARAMETER :: LAYER_ABSORBER_FILLVALUE       = ATMPROFILE_FP_INVALID
  REAL(Double) , PARAMETER :: LAYER_DELTA_Z_FILLVALUE        = ATMPROFILE_FP_INVALID

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
  INTEGER, PARAMETER :: ABSORBER_UNITS_NAME_TYPE  = NF90_CHAR
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
!       Inquire_AtmProfile_netCDF
!
! PURPOSE:
!       Function to inquire a netCDF AtmProfile format file to obtain the
!       dimensions and global attributes.
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_AtmProfile_netCDF( &
!         NC_Filename                    , &  ! Input
!         n_Layers       = n_Layers      , &  ! Optional output
!         n_Absorbers    = n_Absorbers   , &  ! Optional output
!         n_Profiles     = n_Profiles    , &  ! Optional output
!         Release        = Release       , &  ! Optional output
!         Version        = Version       , &  ! Optional output
!         Profile_Set_Id = Profile_Set_Id, &  ! Optional output
!         Title          = Title         , &  ! Optional output
!         History        = History       , &  ! Optional output
!         Comment        = Comment       , &  ! Optional output
!         RCS_Id         = RCS_Id        , &  ! Revision control
!         Message_Log    = Message_Log     )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:        Character string specifying the name of the netCDF
!                           format AtmProfile data file to inquire.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:        Character string specifying a filename in which any
!                           Messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output Messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
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
!       Profile_Set_Id:     Character string written into the PROFILE_SET_ID global
!                           attribute field of the netCDF AtmProfile file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
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
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the netCDF file inquiry was successful.
!                        == FAILURE - an error occurred opening the netCDF file, or
!                                   - an error occurred reading any of the requested
!                                     dimension or variable data.
!                        == WARNING - an error occurred reading any of the requested
!                                     global file attributes, or
!                                   - an error occurred closing the netCDF file.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Inquire_AtmProfile_netCDF( &
    NC_Filename   , &  ! Input
    n_Layers      , &  ! Optional output
    n_Absorbers   , &  ! Optional output
    n_Profiles    , &  ! Optional output
    Release       , &  ! Optional output
    Version       , &  ! Optional output
    Profile_Set_Id, &  ! Optional output
    Title         , &  ! Optional output
    History       , &  ! Optional output
    Comment       , &  ! Optional output
    RCS_Id        , &  ! Revision control
    Message_Log   ) &  ! Error messaging
  RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Layers
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Absorbers
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Profiles
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release         
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Profile_Set_Id
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Inquire_AtmProfile_netCDF'
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

    ! Get the layer dimension
    ! -----------------------
    ! Get the dimension id
    NF90_Status = NF90_INQ_DIMID( NC_FileId,LAYER_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//LAYER_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! Get the dimension value
    NF90_Status = NF90_INQUIRE_DIMENSION( NC_FileId,DimId,Len=n )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//LAYER_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! Keep it if necessary
    IF ( PRESENT(n_Layers) ) n_Layers = n

    ! Get the absorber dimension
    ! --------------------------
    ! Get the dimension id
    NF90_Status = NF90_INQ_DIMID( NC_FileId,ABSORBER_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//ABSORBER_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! Get the dimension value
    NF90_Status = NF90_INQUIRE_DIMENSION( NC_FileId,DimId,Len=n )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//ABSORBER_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! Keep it if necessary
    IF ( PRESENT(n_Absorbers) ) n_Absorbers = n

    ! Get the profile dimension
    ! -------------------------
    ! Get the dimension id
    NF90_Status = NF90_INQ_DIMID( NC_FileId,PROFILE_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//PROFILE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! Get the dimension value
    NF90_Status = NF90_INQUIRE_DIMENSION( NC_FileId,DimId,Len=n )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//PROFILE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! Keep it if necessary
    IF ( PRESENT(n_Profiles) ) n_Profiles = n

    ! Get the global attributes
    ! -------------------------
    Error_Status = ReadGAtts( NC_Filename                  , &
                              NC_FileID                    , &
                              Release       =Release       , &
                              Version       =Version       , &
                              Profile_Set_Id=Profile_Set_Id, &
                              Title         =Title         , &
                              History       =History       , &
                              Comment       =Comment       , &
                              Message_Log   =Message_Log     )
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

  END FUNCTION Inquire_AtmProfile_netCDF


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Write_AtmProfile_netCDF
!
! PURPOSE:
!       Function to write AtmProfile data to a netCDF format AtmProfile file.
!
! CALLING SEQUENCE:
!       Error_Status = Write_AtmProfile_netCDF( &
!         NC_Filename                    , &  ! Input
!         AtmProfile                     , &  ! Input
!         Profile_Set    = Profile_Set   , &  ! Optional input
!         Quiet          = Quiet         , &  ! Optional input
!         Profile_Set_Id = Profile_Set_Id, &  ! Optional input
!         Title          = Title         , &  ! Optional input
!         History        = History       , &  ! Optional input
!         Comment        = Comment       , &  ! Optional input
!         RCS_Id         = RCS_Id        , &  ! Revision control
!         Message_Log    = Message_Log     )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:    Character string specifying the name of the netCDF
!                       format AtmProfile data file to write data into.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       AtmProfile:     Structure containing the AtmProfile data
!                       to write to file.
!                       UNITS:      N/A
!                       TYPE:       TYPE(AtmProfile_type)
!                       DIMENSION:  Rank-1
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!
!       Profile_Set:    Integer array specifying the profiles to be written
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Rank-1
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Quiet:          Set this keyword to suppress information msgs being
!                       printed to standard output (or the msg log file if
!                       the Message_Log optional argument is used.) By default,
!                       information msgs are printed.
!                       If QUIET = 0, information msgs are OUTPUT.
!                          QUIET = 1, information msgs are SUPPRESSED.
!                       UNITS:      N/A
!                       TYPE:       Integer
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Profile_Set_Id: Character string written into the PROFILE_SET_ID global
!                       attribute field of the netCDF AtmProfile file.
!                       Identifies the dependent profile set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:          Character string written into the TITLE global
!                       attribute field of the netCDF AtmProfile file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the netCDF AtmProfile file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the netCDF AtmProfile file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:    Character string specifying a filename in which any
!                       msgs will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output msgs to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:         Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the netCDF data write was successful
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Write_AtmProfile_netCDF( NC_Filename   , &  ! Input
                                    AtmProfile    , &  ! Input
                                    Profile_Set   , &  ! Optional input
                                    Quiet         , &  ! Optional input
                                    Profile_Set_Id, &  ! Optional input
                                    Title         , &  ! Optional input
                                    History       , &  ! Optional input
                                    Comment       , &  ! Optional input
                                    RCS_Id        , &  ! Revision control
                                    Message_Log   ) &  ! Error messaging
                                  RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)     :: NC_Filename
    TYPE(AtmProfile_type) , INTENT(IN OUT) :: AtmProfile(:)
    INTEGER,      OPTIONAL, INTENT(IN)     :: Profile_Set(:)
    INTEGER,      OPTIONAL, INTENT(IN)     :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Profile_Set_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Title  
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: History
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_AtmProfile_netCDF'
    ! Local variables
    INTEGER , DIMENSION(SIZE(AtmProfile)) :: Local_Profile_Set
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
    INTEGER :: NC_FileID
    INTEGER :: NF90_Status
    INTEGER :: m
    INTEGER :: n_Profiles
    
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

    ! Create the output data file
    ! ---------------------------
    Error_Status = CreateFile( NC_Filename                           , &  ! Input
                               AtmProfile(1)%n_Layers                , &  ! Input
                               AtmProfile(1)%n_Absorbers             , &  ! Input
                               NC_FileID                             , &  ! Output
                               Version        = AtmProfile(1)%Version, &  ! Optional input
                               Profile_Set_Id = Profile_Set_Id       , &  ! Optional input
                               Title          = Title                , &  ! Optional input
                               History        = History              , &  ! Optional input
                               Comment        = Comment              , &  ! Optional input
                               Message_Log    = Message_Log            )  ! Error messaging
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error creating output file '//TRIM(NC_Filename)
      CALL Write_Cleanup(); RETURN
    END IF    
    
    ! Assign the number of profiles to write
    n_Profiles = SIZE(AtmProfile)
    
    ! Assign the set of profiles to be read
    IF ( PRESENT(Profile_Set) ) THEN
      Local_Profile_Set=Profile_Set
    ELSE
      Local_Profile_Set=(/(m,m=1,n_Profiles)/)
    ENDIF
    
    ! Check that SIZE(AtmProfile) = SIZE(Local_Profile_Set)
    IF ( SIZE(AtmProfile) /= SIZE(Local_Profile_Set) ) THEN
      CALL Display_Message(  ROUTINE_NAME, &
                             'SIZE of AtmProfile inconsistent with profile set '//&
                             TRIM(NC_Filename), &
                             FAILURE )
      STOP
    END IF
    
    DO m = 1, n_Profiles
    
      ! Check structure association
      IF ( .NOT. Associated_AtmProfile( AtmProfile(m) ) ) THEN
        msg = 'Some or all INPUT AtmProfile pointer members are NOT associated.'
        CALL Write_Cleanup(); RETURN
      END IF

      ! Write the AtmProfile data
      ! -------------------------
      Error_Status = WriteVar( NC_Filename             , &
                               NC_FileID               , &
                               Local_Profile_Set(m)    , &
                               AtmProfile(m)           , &
                               Message_Log=Message_Log   )
      IF ( Error_Status /= SUCCESS ) THEN
        msg = 'Error writing AtmProfile variables to output file '//TRIM(NC_Filename)
        CALL Write_Cleanup(Close_File=.TRUE.); RETURN
      END IF
      
      ! Output an info msg
      ! ----------------------
      IF ( Noisy ) THEN
        CALL Info_AtmProfile( AtmProfile(m), msg )
        CALL Display_Message( ROUTINE_NAME, &
                              'FILE: '//TRIM(NC_Filename)//'; '//TRIM(msg), &
                              INFORMATION, &
                              Message_Log=Message_Log )
      END IF

    END DO

    ! Close the file
    ! --------------
    NF90_Status = NF90_CLOSE( NC_FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error closing input file - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
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

  END FUNCTION Write_AtmProfile_netCDF


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Read_AtmProfile_netCDF
!
! PURPOSE:
!       Function to read data from a netCDF format AtmProfile file.
!
! CALLING SEQUENCE:
!     Error_Status = Read_AtmProfile_netCDF( &
!       NC_Filename                    , &  ! Input
!       AtmProfile                     , &  ! Output
!       Profile_Set    = Profile_Set   , &  ! Optional Input 
!       Quiet          = Quiet         , &  ! Optional input
!       Reverse        = Reverse       , &  ! Optional input
!       Profile_Set_Id = Profile_Set_Id, &  ! Optional output
!       Title          = Title         , &  ! Optional output
!       History        = History       , &  ! Optional output
!       Comment        = Comment       , &  ! Optional output
!       RCS_Id         = RCS_Id        , &  ! Revision control
!       Message_Log    = Message_Log     )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:    Character string specifying the name of the
!                       netCDF format AtmProfile data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       AtmProfile:     Structure to contain the AtmProfile data
!                       read from file.
!                       UNITS:      N/A
!                       TYPE:       TYPE(AtmProfile_type)
!                       DIMENSION:  Rank-1
!                       ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:          Set this keyword to suppress information messages being
!                       printed to standard output (or the msg log file if
!                       the Message_Log optional argument is used.) By default,
!                       information messages are printed.
!                       If QUIET = 0, information messages are OUTPUT.
!                          QUIET = 1, information messages are SUPPRESSED.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Reverse:        Set this keyword to reverse the order of the profile data
!                       arrays in the K index (vertical) dimension.
!                       If REVERSE = 0, arrays are returned as they are stored in
!                                       the netCDF input file (DEFAULT)
!                          REVERSE = 1, arrays are returned in reverse order to how
!                                       they are stored in the netCDF input file.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:    Character string specifying a filename in which any
!                       msgs will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output msgs to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!
!       Profile_Set:    Integer array specifying the profiles to be read                  
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Rank-1
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
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
!       RCS_Id:         Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT(OUT)
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
!       If specified as the output data type, the INTENT on the output AtmProfile
!       structure argument is IN OUT rather than just OUT. This is necessary
!       because the argument may be defined on input. To prevent memory leaks,
!       the IN OUT INTENT is a must.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Read_AtmProfile_netCDF( NC_Filename   , &  ! Input
                                   AtmProfile    , &  ! Output
                                   Profile_Set   , &  ! Optional Input
                                   Quiet         , &  ! Optional input
                                   Reverse       , &  ! Optional input
                                   Profile_Set_Id, &  ! Optional output
                                   Title         , &  ! Optional output
                                   History       , &  ! Optional output
                                   Comment       , &  ! Optional output
                                   RCS_Id        , &  ! Revision control
                                   Message_Log   ) &  ! Error messaging
                                 RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)     :: NC_Filename   
    TYPE(AtmProfile_type),  INTENT(IN OUT) :: AtmProfile(:)
    INTEGER,      OPTIONAL, INTENT(IN)     :: Profile_Set(:)
    INTEGER,      OPTIONAL, INTENT(IN)     :: Quiet
    INTEGER,      OPTIONAL, INTENT(IN)     :: Reverse
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: Profile_Set_Id
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: Title  
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_AtmProfile_netCDF'
    ! Function variables
    INTEGER , DIMENSION(SIZE(AtmProfile)) :: Local_Profile_Set
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy, ReverseProfile
    INTEGER :: NC_FileID
    INTEGER :: NF90_Status
    INTEGER :: k, j, m
    INTEGER :: n_File_Profiles
    INTEGER :: n_Allocated_Profiles
    INTEGER :: n_Profiles, n_Absorbers, n_Layers
    
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

    ! Do NOT reverse profile....
    ReverseProfile = .FALSE.
    ! ....unless the REVERSE keyword is set.
    IF ( PRESENT(Reverse) ) THEN
      IF ( Reverse == SET ) ReverseProfile = .TRUE.
    END IF
    
    ! Inquire the dimensions for the file
    Error_Status = Inquire_AtmProfile_netCDF( NC_Filename,                    &
                                              n_Layers = n_Layers ,           &
                                              n_Absorbers = n_Absorbers,      &
                                              n_Profiles  = n_File_Profiles   )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message(  ROUTINE_NAME, &
                             'Error inquiring the netCDF AtmProfile file '//&
                             TRIM(NC_Filename), &
                             FAILURE )
      STOP
    END IF    
    
    ! Assign n_Profiles based on input 
    n_Allocated_Profiles = SIZE(AtmProfile)
    IF ( n_Allocated_Profiles <= n_File_Profiles) THEN
      n_Profiles = n_Allocated_Profiles
    ELSE 
      n_Profiles = n_File_Profiles
    ENDIF
    
    ! Assign the set of profiles to read
    IF ( PRESENT(Profile_Set) ) THEN
      Local_Profile_Set=Profile_Set
    ELSE
      Local_Profile_Set=(/(m,m=1,n_Profiles)/)
    ENDIF
    
    ! Check that SIZE(AtmProfile) = SIZE(Local_Profile_Set)
    IF ( SIZE(AtmProfile) /= SIZE(Local_Profile_Set) ) THEN
      CALL Display_Message(  ROUTINE_NAME, &
                             'SIZE of AtmProfile inconsistent with profile set '//&
                             TRIM(NC_Filename), &
                             FAILURE )
      STOP
    END IF    
    
    DO m = 1, n_Profiles
    
      ! Allocate the structure
      Error_Status = Allocate_AtmProfile( n_Layers,n_Absorbers,                 &
                                          AtmProfile(m),Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        msg = 'Error occurred allocating AtmProfile structure.'
        CALL Read_Cleanup(AtmProfile(m)); RETURN
      END IF

      ! Open the netCDF file for reading
      ! --------------------------------
      NF90_Status = NF90_OPEN( NC_Filename,NF90_NOWRITE,NC_FileId )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error opening '//TRIM(NC_Filename)//' for read access - '//&
              TRIM(NF90_STRERROR( NF90_Status ))
        CALL Read_Cleanup(AtmProfile(m),Destroy_Structure=.TRUE.); RETURN
      END IF

      ! Read the global attributes
      ! --------------------------
      Error_Status = ReadGAtts( NC_Filename                    , &
                                NC_FileID                      , &
!                                Release        = AtmProfile%Release, &
!                                Version        = AtmProfile%Version, &
                                Profile_Set_Id = Profile_Set_Id, &
                                Title          = Title         , &
                                History        = History       , &
                                Comment        = Comment       , &
                                Message_Log    = Message_Log     )
      IF ( Error_Status /= SUCCESS ) THEN
        msg = 'Error reading global attribute from '//TRIM(NC_Filename)
        CALL Read_Cleanup(AtmProfile(m),Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
      END IF

      ! Check the release
      Error_Status = CheckRelease_AtmProfile( AtmProfile(m),Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        msg = 'AtmProfile Release check failed for '//TRIM(NC_Filename)
        CALL Read_Cleanup(AtmProfile(m),Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
      END IF
    
      ! Read the AtmProfile data
      ! ------------------------
      Error_Status = ReadVar( NC_Filename               , &
                              NC_FileID                 , &
                              Local_Profile_Set(m)      , &
                              AtmProfile(m)             , &
                              Message_Log=Message_Log     )
      IF ( Error_Status /= SUCCESS ) THEN
        msg = 'Error reading AtmProfile variables from '//TRIM(NC_Filename)
        CALL Read_Cleanup(AtmProfile(m),Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
      END IF


      ! Close the file
      ! --------------
      NF90_Status = NF90_CLOSE( NC_FileId )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error closing input file - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Read_Cleanup(AtmProfile(m),Destroy_Structure=.TRUE.); RETURN
      END IF

      ! Finish up with the data structure
      ! ---------------------------------
      ! Fill the other Absorber_Units structure members
      DO j = 1, AtmProfile(m)%n_Absorbers
        AtmProfile(m)%Absorber_Units_Name(j) = ATMPROFILE_ABSORBER_UNITS_NAME(AtmProfile(m)%Absorber_Units_ID(j))
        AtmProfile(m)%Absorber_Units_LBL(j) = ATMPROFILE_ABSORBER_UNITS_CHAR(AtmProfile(m)%Absorber_Units_ID(j))
      END DO
      ! Reverse the profile data direction if required
      IF ( ReverseProfile ) THEN
        ! Level data
        k = AtmProfile(m)%n_Levels
        AtmProfile(m)%Level_Pressure(1:k)    = AtmProfile(m)%Level_Pressure(k:1:-1)
        AtmProfile(m)%Level_Temperature(1:k) = AtmProfile(m)%Level_Temperature(k:1:-1)
        AtmProfile(m)%Level_Absorber(1:k,:)  = AtmProfile(m)%Level_Absorber(k:1:-1,:)
        AtmProfile(m)%Level_Altitude(1:k)    = AtmProfile(m)%Level_Altitude(k:1:-1)
        ! Layer data
        k = AtmProfile(m)%n_Layers
        AtmProfile(m)%Layer_Pressure(1:k)    = AtmProfile(m)%Layer_Pressure(k:1:-1)
        AtmProfile(m)%Layer_Temperature(1:k) = AtmProfile(m)%Layer_Temperature(k:1:-1)
        AtmProfile(m)%Layer_Absorber(1:k,:)  = AtmProfile(m)%Layer_Absorber(k:1:-1,:)
        AtmProfile(m)%Layer_Delta_Z(1:k)     = AtmProfile(m)%Layer_Delta_Z(k:1:-1)
      END IF

      ! Output an info message
      ! ----------------------
      IF ( Noisy ) THEN
        CALL Info_AtmProfile( AtmProfile(m), msg )
        CALL Display_Message( ROUTINE_NAME, &
                              'FILE: '//TRIM(NC_Filename)//'; '//TRIM(msg), &
                              INFORMATION, &
                              Message_Log=Message_Log )
      END IF
   
    END DO

  CONTAINS
  
    SUBROUTINE Read_CleanUp( AtmProfile, Close_File, Destroy_Structure )
      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
      LOGICAL, OPTIONAL, INTENT(IN) :: Destroy_Structure
      TYPE(AtmProfile_type) :: AtmProfile
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
      IF ( PRESENT(Destroy_Structure) ) THEN                                        
        IF ( Destroy_Structure ) THEN                                               
          Error_Status = Destroy_AtmProfile(AtmProfile, Message_Log=Message_Log)    
          IF ( Error_Status /= SUCCESS ) &                                          
            msg = TRIM(msg)//'; Error destroying AtmProfile during error cleanup.'  
        END IF                                                                      
      END IF                                                                        
      
      ! Set error status and print error msg
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
    END SUBROUTINE Read_CleanUp

  END FUNCTION Read_AtmProfile_netCDF


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
!       Error_Status = WriteGAtts( NC_Filename                  , &  ! Input
!                                  NC_FileID                    , &  ! Input
!                                  Version       =Version       , &  ! Optional input
!                                  Title         =Title         , &  ! Optional input
!                                  History       =History       , &  ! Optional input
!                                  Comment       =Comment       , &  ! Optional input
!                                  Profile_Set_Id=Profile_Set_Id, &  ! Optional input
!                                  Message_Log   =Message_Log     )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF AtmProfile format data file to create.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:        NetCDF file ID number returned from the
!                         Open_ or Create_AtmProfile_netCDF() function.
!                         UNITS:      N/A
!                         TYPE:       Integer
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!
! OPTIONAL INPUT ARGUMENTS:
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
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the global attribute write was successful.
!                        == WARNING an error occurred writing the supplied
!                           global attributes.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION WriteGAtts( NC_Filename   , &  ! Input
                       NC_FileID     , &  ! Input
                       Version       , &  ! Optional input
                       Title         , &  ! Optional input
                       History       , &  ! Optional input
                       Comment       , &  ! Optional input
                       Profile_Set_Id, &  ! Optional input
                       Message_Log   ) &  ! Error messaging
                     RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: NC_Filename
    INTEGER     ,           INTENT(IN) :: NC_FileID
    INTEGER     , OPTIONAL, INTENT(IN) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Profile_Set_Id
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
    ! Function result
    INTEGER :: Error_Status
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
    NF90_Status = NF90_PUT_ATT( NC_FileId, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                Ver )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF

    ! The Profile_Set_Id
    IF ( PRESENT(Profile_Set_Id) ) THEN
      GAttName = PROFILE_SET_ID_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
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
!       Function to read the global attributes from a netCDF AtmProfile
!       data file.
!
! CALLING SEQUENCE:
!       Error_Status = ReadGAtts( NC_Filename                  , &  ! Input
!                                 NC_FileID                    , &  ! Input
!                                 Release       =Release       , &  ! Optional output
!                                 Version       =Version       , &  ! Optional output
!                                 Profile_Set_Id=Profile_Set_Id, &  ! Optional output
!                                 Title         =Title         , &  ! Optional output
!                                 History       =History       , &  ! Optional output
!                                 Comment       =Comment       , &  ! Optional output
!                                 Message_Log   =Message_Log     )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF AtmProfile format data file to read from.
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
                      Profile_Set_Id  , &  ! Optional output
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
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Profile_Set_Id
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
    TYPE(AtmProfile_type) :: AtmProfile_Default

    ! Set up
    ! ------
    Error_Status = SUCCESS


    ! The mandatory GAtts for checking
    ! --------------------------------
    ! The Release
    IF ( PRESENT(Release) ) THEN
      GAttName = RELEASE_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileId, &
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
      NF90_Status = NF90_GET_ATT( NC_FileID, &
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
      NF90_Status = NF90_GET_ATT( NC_FileID, &
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
!       Function to define the AtmProfile variables in an output
!       netCDF file.
!
! CALLING SEQUENCE:
!       Error_Status = DefineVar( NC_Filename            , &  ! Input
!                                 NC_FileID              , &  ! Input
!                                 Level_DimID            , &  ! Input
!                                 Layer_DimID            , &  ! Input
!                                 Absorber_DimID         , &  ! Input
!                                 Profile_DimID          , &  ! Input
!                                 PL_DimID               , &  ! Input
!                                 AUL_DimID              , &  ! Input
!                                 Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS
!       NC_Filename:        Character string specifying the name of the
!                           already created netCDF AtmProfile format file.
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
!       Level_DimID:        NetCDF dimension ID of the number of levels
!                           (n_Levels).
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Layer_DimID:        NetCDF dimension ID of the number of layers
!                           (n_Layers).
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Absorber_DimID:     NetCDF dimension ID of the number of absorbers
!                           (n_Absorbers).
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Profile_DimID:      NetCDF dimension ID of the number of profiles
!                           (n_Profiles).
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       PL_DimID:           NetCDF dimension ID for the string length of
!                           the profile description.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       AUL_DimID:          NetCDF dimension ID for the string length of
!                           the absorber units description.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN) 
!
! OPTIONAL INPUT ARGUMENTS
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!------------------------------------------------------------------------------

  FUNCTION DefineVar( NC_Filename   , &  ! Input
                      NC_FileID     , &  ! Input
                      Level_DimID   , &  ! Input
                      Layer_DimID   , &  ! Input
                      Absorber_DimID, &  ! Input
                      Profile_DimID , &  ! Input
                      PL_DimID      , &  ! Input
                      AUL_DimID     , &  ! Input
                      Message_Log   ) &  ! Error messaging
                    RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER     ,           INTENT(IN)  :: NC_FileID
    INTEGER     ,           INTENT(IN)  :: Level_DimID   
    INTEGER     ,           INTENT(IN)  :: Layer_DimID   
    INTEGER     ,           INTENT(IN)  :: Absorber_DimID
    INTEGER     ,           INTENT(IN)  :: Profile_DimID 
    INTEGER     ,           INTENT(IN)  :: PL_DimID
    INTEGER     ,           INTENT(IN)  :: AUL_DimID 
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

    ! Begin all the variable definitions
    ! ----------------------------------
    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,DESCRIPTION_VARNAME,DESCRIPTION_TYPE, &
                                   dimIDs=(/PL_DimID,Profile_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//DESCRIPTION_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,DESCRIPTION_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,DESCRIPTION_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,DESCRIPTION_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,DESCRIPTION_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//DESCRIPTION_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,CLIMATOLOGY_MODEL_VARNAME,CLIMATOLOGY_MODEL_TYPE, &
                                   dimIDs=(/Profile_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//CLIMATOLOGY_MODEL_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,CLIMATOLOGY_MODEL_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,CLIMATOLOGY_MODEL_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,CLIMATOLOGY_MODEL_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,CLIMATOLOGY_MODEL_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//CLIMATOLOGY_MODEL_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF
    
    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,YEAR_VARNAME,YEAR_TYPE, &
                                   dimIDs=(/Profile_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//YEAR_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,YEAR_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,YEAR_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,YEAR_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,YEAR_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//YEAR_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF
    
    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,MONTH_VARNAME,MONTH_TYPE, &
                                   dimIDs=(/Profile_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//MONTH_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,MONTH_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,MONTH_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,MONTH_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,MONTH_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//MONTH_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF
    
    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,DAY_VARNAME,DAY_TYPE, &
                                   dimIDs=(/Profile_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//DAY_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,DAY_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,DAY_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,DAY_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,DAY_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//DAY_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF
    
    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,HOUR_VARNAME,HOUR_TYPE,    &
                                   dimIDs=(/Profile_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//HOUR_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,HOUR_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,HOUR_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,HOUR_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,HOUR_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//HOUR_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,LATITUDE_VARNAME,LATITUDE_TYPE, &
                                   dimIDs=(/Profile_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//LATITUDE_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,LATITUDE_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,LATITUDE_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,LATITUDE_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,LATITUDE_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//LATITUDE_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,LONGITUDE_VARNAME,LONGITUDE_TYPE, &
                                   dimIDs=(/Profile_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//LONGITUDE_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,LONGITUDE_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,LONGITUDE_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,LONGITUDE_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,LONGITUDE_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//LONGITUDE_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,SURFACE_ALTITUDE_VARNAME,SURFACE_ALTITUDE_TYPE, &
                                   dimIDs=(/Profile_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//SURFACE_ALTITUDE_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,SURFACE_ALTITUDE_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,SURFACE_ALTITUDE_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,SURFACE_ALTITUDE_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,SURFACE_ALTITUDE_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//SURFACE_ALTITUDE_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,ABSORBER_ID_VARNAME,ABSORBER_ID_TYPE, &
                                   dimIDs=(/Absorber_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//ABSORBER_ID_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,ABSORBER_ID_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,ABSORBER_ID_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,ABSORBER_ID_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,ABSORBER_ID_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//ABSORBER_ID_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,ABSORBER_UNITS_ID_VARNAME,ABSORBER_UNITS_ID_TYPE, &
                                   dimIDs=(/Absorber_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//ABSORBER_UNITS_ID_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,ABSORBER_UNITS_ID_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,ABSORBER_UNITS_ID_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,ABSORBER_UNITS_ID_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,ABSORBER_UNITS_ID_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//ABSORBER_UNITS_ID_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF
    
    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,ABSORBER_UNITS_NAME_VARNAME,ABSORBER_UNITS_NAME_TYPE, &
                                   dimIDs=(/AUL_DimID,Absorber_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//ABSORBER_UNITS_NAME_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,ABSORBER_UNITS_NAME_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,ABSORBER_UNITS_NAME_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,ABSORBER_UNITS_NAME_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,ABSORBER_UNITS_NAME_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//ABSORBER_UNITS_NAME_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,LEVEL_PRESSURE_VARNAME,LEVEL_PRESSURE_TYPE, &
                                   dimIDs=(/Level_DimID,Profile_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//LEVEL_PRESSURE_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,LEVEL_PRESSURE_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,LEVEL_PRESSURE_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,LEVEL_PRESSURE_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,LEVEL_PRESSURE_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//LEVEL_PRESSURE_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,LEVEL_TEMPERATURE_VARNAME,LEVEL_TEMPERATURE_TYPE, &
                                   dimIDs=(/Level_DimID,Profile_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//LEVEL_TEMPERATURE_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,LEVEL_TEMPERATURE_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,LEVEL_TEMPERATURE_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,LEVEL_TEMPERATURE_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,LEVEL_TEMPERATURE_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//LEVEL_TEMPERATURE_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,LEVEL_ABSORBER_VARNAME,LEVEL_ABSORBER_TYPE, &
                                   dimIDs=(/Level_DimID,Absorber_DimID,Profile_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//LEVEL_ABSORBER_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,LEVEL_ABSORBER_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,LEVEL_ABSORBER_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,LEVEL_ABSORBER_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,LEVEL_ABSORBER_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//LEVEL_ABSORBER_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,LEVEL_ALTITUDE_VARNAME,LEVEL_ALTITUDE_TYPE, &
                                   dimIDs=(/Level_DimID,Profile_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//LEVEL_ALTITUDE_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,LEVEL_ALTITUDE_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,LEVEL_ALTITUDE_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,LEVEL_ALTITUDE_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,LEVEL_ALTITUDE_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//LEVEL_ALTITUDE_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,LAYER_PRESSURE_VARNAME,LAYER_PRESSURE_TYPE, &
                                   dimIDs=(/Layer_DimID,Profile_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//LAYER_PRESSURE_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,LAYER_PRESSURE_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,LAYER_PRESSURE_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,LAYER_PRESSURE_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,LAYER_PRESSURE_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//LAYER_PRESSURE_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,LAYER_TEMPERATURE_VARNAME,LAYER_TEMPERATURE_TYPE, &
                                   dimIDs=(/Layer_DimID,Profile_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//LAYER_TEMPERATURE_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,LAYER_TEMPERATURE_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,LAYER_TEMPERATURE_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,LAYER_TEMPERATURE_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,LAYER_TEMPERATURE_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//LAYER_TEMPERATURE_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,LAYER_ABSORBER_VARNAME,LAYER_ABSORBER_TYPE, &
                                   dimIDs=(/Layer_DimID,Absorber_DimID,Profile_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//LAYER_ABSORBER_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,LAYER_ABSORBER_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,LAYER_ABSORBER_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,LAYER_ABSORBER_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,LAYER_ABSORBER_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//LAYER_ABSORBER_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,LAYER_DELTA_Z_VARNAME,LAYER_DELTA_Z_TYPE, &
                                   dimIDs=(/Layer_DimID,Profile_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//LAYER_DELTA_Z_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,LAYER_DELTA_Z_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,LAYER_DELTA_Z_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,LAYER_DELTA_Z_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,LAYER_DELTA_Z_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//LAYER_DELTA_Z_VARNAME//' variable attributes to '//TRIM(NC_Filename)
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
!       Function to write the AtmProfile variables in an output
!       netCDF file in which they have been defined.
!
! CALLING SEQUENCE:
!       Error_Status = WriteVar( NC_Filename            , &  ! Input
!                                NC_FileID              , &  ! Input
!                                Profile                , &  ! Input
!                                AtmProfile             , &  ! Input
!                                RCS_Id     =RCS_Id     , &  ! Revision control
!                                Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS
!       NC_Filename:        Character string specifying the name of the
!                           already created netCDF AtmProfile format file.
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
!       Profile:            Profile number to be written
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       AtmProfile:         Structure containing the data to write to file.
!                           UNITS:      N/A
!                           TYPE:       TYPE(AtmProfile_type)
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
                     Profile    , &  ! Input
                     AtmProfile , &  ! Input
                     RCS_Id     , &  ! Revision control
                     Message_Log) &  ! Error messaging
                   RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)  :: NC_Filename
    INTEGER               , INTENT(IN)  :: NC_FileID
    INTEGER               , INTENT(IN)  :: Profile
    TYPE(AtmProfile_type) , INTENT(IN)  :: AtmProfile
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
    ! The Absorber_ID
    NF90_Status = NF90_INQ_VARID( NC_FileId,ABSORBER_ID_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//ABSORBER_ID_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,AtmProfile%Absorber_ID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//ABSORBER_ID_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Absorber_Units_ID
    NF90_Status = NF90_INQ_VARID( NC_FileId,ABSORBER_UNITS_ID_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//ABSORBER_UNITS_ID_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,AtmProfile%Absorber_Units_ID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//ABSORBER_UNITS_ID_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Absorber_Units_Name
    NF90_Status = NF90_INQ_VARID( NC_FileId,ABSORBER_UNITS_NAME_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//ABSORBER_UNITS_NAME_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,AtmProfile%Absorber_Units_Name )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//ABSORBER_UNITS_NAME_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Description
    NF90_Status = NF90_INQ_VARID( NC_FileId,DESCRIPTION_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//DESCRIPTION_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,AtmProfile%Description,&
                                start=(/1,Profile/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//DESCRIPTION_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Climatology_Model
    NF90_Status = NF90_INQ_VARID( NC_FileId,CLIMATOLOGY_MODEL_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//CLIMATOLOGY_MODEL_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,AtmProfile%Climatology_Model,&
                                start=(/Profile/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//CLIMATOLOGY_MODEL_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Year
    NF90_Status = NF90_INQ_VARID( NC_FileId,YEAR_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//YEAR_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,AtmProfile%Year,&
                                start=(/Profile/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//YEAR_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Month
    NF90_Status = NF90_INQ_VARID( NC_FileId,MONTH_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//MONTH_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,AtmProfile%Month,&
                                start=(/Profile/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//MONTH_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Day
    NF90_Status = NF90_INQ_VARID( NC_FileId,DAY_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//DAY_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,AtmProfile%Day,&
                                start=(/Profile/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//DAY_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Hour
    NF90_Status = NF90_INQ_VARID( NC_FileId,HOUR_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//HOUR_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,AtmProfile%Hour,&
                                start=(/Profile/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//HOUR_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Latitude
    NF90_Status = NF90_INQ_VARID( NC_FileId,LATITUDE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//LATITUDE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,AtmProfile%Latitude,&
                                start=(/Profile/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//LATITUDE_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Longitude
    NF90_Status = NF90_INQ_VARID( NC_FileId,LONGITUDE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//LONGITUDE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,AtmProfile%Longitude,&
                                start=(/Profile/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//LONGITUDE_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Surface_Altitude
    NF90_Status = NF90_INQ_VARID( NC_FileId,SURFACE_ALTITUDE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//SURFACE_ALTITUDE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,AtmProfile%Surface_Altitude,&
                                start=(/Profile/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//SURFACE_ALTITUDE_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Level_Pressure
    NF90_Status = NF90_INQ_VARID( NC_FileId,LEVEL_PRESSURE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//LEVEL_PRESSURE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,AtmProfile%Level_Pressure,&
                                start=(/1,Profile/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//LEVEL_PRESSURE_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Level_Temperature
    NF90_Status = NF90_INQ_VARID( NC_FileId,LEVEL_TEMPERATURE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//LEVEL_TEMPERATURE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,AtmProfile%Level_Temperature,&
                                start=(/1,Profile/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//LEVEL_TEMPERATURE_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Level_Absorber
    NF90_Status = NF90_INQ_VARID( NC_FileId,LEVEL_ABSORBER_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//LEVEL_ABSORBER_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,AtmProfile%Level_Absorber,&
                                start=(/1,1,Profile/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//LEVEL_ABSORBER_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Level_Altitude
    NF90_Status = NF90_INQ_VARID( NC_FileId,LEVEL_ALTITUDE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//LEVEL_ALTITUDE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,AtmProfile%Level_Altitude,&
                                start=(/1,Profile/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//LEVEL_ALTITUDE_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Layer_Pressure
    NF90_Status = NF90_INQ_VARID( NC_FileId,LAYER_PRESSURE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//LAYER_PRESSURE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,AtmProfile%Layer_Pressure,&
                                start=(/1,Profile/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//LAYER_PRESSURE_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Layer_Temperature
    NF90_Status = NF90_INQ_VARID( NC_FileId,LAYER_TEMPERATURE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//LAYER_TEMPERATURE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,AtmProfile%Layer_Temperature,&
                                start=(/1,Profile/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//LAYER_TEMPERATURE_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Layer_Absorber
    NF90_Status = NF90_INQ_VARID( NC_FileId,LAYER_ABSORBER_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//LAYER_ABSORBER_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,AtmProfile%Layer_Absorber,&
                                start=(/1,1,Profile/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//LAYER_ABSORBER_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Layer_Delta_Z
    NF90_Status = NF90_INQ_VARID( NC_FileId,LAYER_DELTA_Z_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//LAYER_DELTA_Z_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,AtmProfile%Layer_Delta_Z,&
                                start=(/1,Profile/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//LAYER_DELTA_Z_VARNAME//' to '//TRIM(NC_Filename)//&
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
!       Function to read the AtmProfile variables from any input
!       netCDF file in which they have been defined.
!
! CALLING SEQUENCE:
!       Error_Status = ReadVar( NC_Filename            , &  ! Input
!                               NC_FileID              , &  ! Input
!                               Profile                , &  ! Input
!                               AtmProfile             , &  ! Output
!                               RCS_Id     =RCS_Id     , &  ! Revision control
!                               Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS
!       NC_Filename:        Character string specifying the name of the
!                           already created netCDF AtmProfile format file.
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
!       Profile:            Profile number to be read
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)       
!
! OUTPUT ARGUMENTS:
!       AtmProfile:         Structure containing the data that was read
!                           from file.
!                           UNITS:      N/A
!                           TYPE:       TYPE(AtmProfile_type)
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
!       The INTENT on the output AtmProfile argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION ReadVar( NC_Filename, &  ! Input
                    NC_FileID  , &  ! Input
                    Profile    , &  ! Input
                    AtmProfile , &  ! Output
                    RCS_Id     , &  ! Revision control
                    Message_Log) &  ! Error messaging
                  RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)     :: NC_Filename
    INTEGER               , INTENT(IN)     :: NC_FileID
    INTEGER               , INTENT(IN)     :: Profile
    TYPE(AtmProfile_type) , INTENT(IN OUT) :: AtmProfile
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
    ! The Absorber_ID
    NF90_Status = NF90_INQ_VARID( NC_FileId,ABSORBER_ID_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//ABSORBER_ID_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,AtmProfile%Absorber_ID,&
    start=(/1/),count=(/AtmProfile%n_Absorbers/))
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//ABSORBER_ID_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Absorber_Units_ID
    NF90_Status = NF90_INQ_VARID( NC_FileId,ABSORBER_UNITS_ID_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//ABSORBER_UNITS_ID_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,AtmProfile%Absorber_Units_ID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//ABSORBER_UNITS_ID_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Description
    NF90_Status = NF90_INQ_VARID( NC_FileId,DESCRIPTION_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//DESCRIPTION_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,AtmProfile%Description,&
                                start=(/1,Profile/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//DESCRIPTION_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Climatology_Model
    NF90_Status = NF90_INQ_VARID( NC_FileId,CLIMATOLOGY_MODEL_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//CLIMATOLOGY_MODEL_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,AtmProfile%Climatology_Model,&
                                start=(/Profile/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//CLIMATOLOGY_MODEL_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Year
    NF90_Status = NF90_INQ_VARID( NC_FileId,YEAR_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//YEAR_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,AtmProfile%Year,&
                                start=(/Profile/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//YEAR_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Month
    NF90_Status = NF90_INQ_VARID( NC_FileId,MONTH_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//MONTH_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,AtmProfile%Month,&
                                start=(/Profile/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//YEAR_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Day
    NF90_Status = NF90_INQ_VARID( NC_FileId,DAY_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//DAY_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,AtmProfile%Day,&
                                start=(/Profile/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//DAY_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Hour
    NF90_Status = NF90_INQ_VARID( NC_FileId,HOUR_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//HOUR_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,AtmProfile%Hour,&
                                start=(/Profile/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//HOUR_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Latitude
    NF90_Status = NF90_INQ_VARID( NC_FileId,LATITUDE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//LATITUDE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,AtmProfile%Latitude,&
                                start=(/Profile/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//LATITUDE_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Longitude
    NF90_Status = NF90_INQ_VARID( NC_FileId,LONGITUDE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//LONGITUDE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,AtmProfile%Longitude,&
                                start=(/Profile/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//LONGITUDE_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Surface_Altitude
    NF90_Status = NF90_INQ_VARID( NC_FileId,SURFACE_ALTITUDE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//SURFACE_ALTITUDE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,AtmProfile%Surface_Altitude,&
                                start=(/Profile/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//SURFACE_ALTITUDE_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Level_Pressure
    NF90_Status = NF90_INQ_VARID( NC_FileId,LEVEL_PRESSURE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//LEVEL_PRESSURE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,AtmProfile%Level_Pressure, &
                                start=(/ 1,Profile /) ) 
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//LEVEL_PRESSURE_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Level_Temperature
    NF90_Status = NF90_INQ_VARID( NC_FileId,LEVEL_TEMPERATURE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//LEVEL_TEMPERATURE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,AtmProfile%Level_Temperature, &
                                start=(/ 1,Profile /) )  
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//LEVEL_TEMPERATURE_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Level_Absorber
    NF90_Status = NF90_INQ_VARID( NC_FileId,LEVEL_ABSORBER_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//LEVEL_ABSORBER_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,AtmProfile%Level_Absorber,&
                                start=(/1,1,Profile/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//LEVEL_ABSORBER_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Level_Altitude
    NF90_Status = NF90_INQ_VARID( NC_FileId,LEVEL_ALTITUDE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//LEVEL_ALTITUDE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,AtmProfile%Level_Altitude,&
                                start=(/1,Profile/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//LEVEL_ALTITUDE_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Layer_Pressure
    NF90_Status = NF90_INQ_VARID( NC_FileId,LAYER_PRESSURE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//LAYER_PRESSURE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,AtmProfile%Layer_Pressure,&
                                start=(/1,Profile/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//LAYER_PRESSURE_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Layer_Temperature
    NF90_Status = NF90_INQ_VARID( NC_FileId,LAYER_TEMPERATURE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//LAYER_TEMPERATURE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,AtmProfile%Layer_Temperature,&
                                start=(/1,Profile/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//LAYER_TEMPERATURE_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Layer_Absorber
    NF90_Status = NF90_INQ_VARID( NC_FileId,LAYER_ABSORBER_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//LAYER_ABSORBER_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,AtmProfile%Layer_Absorber,&
                                start=(/1,1,Profile/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//LAYER_ABSORBER_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Layer_Delta_Z
    NF90_Status = NF90_INQ_VARID( NC_FileId,LAYER_DELTA_Z_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//LAYER_DELTA_Z_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,AtmProfile%Layer_Delta_Z,&
                                start=(/1,Profile/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//LAYER_DELTA_Z_VARNAME//' from '//TRIM(NC_Filename)//&
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
!       Function to create a netCDF AtmProfile data file for writing.
!
! CALLING SEQUENCE:
!       Error_Status = CreateFile( NC_Filename                    , &  ! Input
!                                  n_Layers                       , &  ! Input
!                                  n_Absorbers                    , &  ! Input
!                                  NC_FileID                      , &  ! Output
!                                  Version        = Version       , &  ! Optional input
!                                  Profile_Set_Id = Profile_Set_Id, &  ! Optional input
!                                  Title          = Title         , &  ! Optional input
!                                  History        = History       , &  ! Optional input
!                                  Comment        = Comment       , &  ! Optional input
!                                  Message_Log    = Message_Log     )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:        Character string specifying the name of the
!                           netCDF AtmProfile format data file to create.
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
! OUTPUT ARGUMENTS:
!       NC_FileID:          NetCDF file ID number to be used for subsequent
!                           writing to the output file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT ARGUMENTS:
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

  FUNCTION CreateFile( NC_Filename   , &  ! Input
                       n_Layers      , &  ! Input
                       n_Absorbers   , &  ! Input
                       NC_FileID     , &  ! Output
                       Version       , &  ! Optional input
                       Profile_Set_Id, &  ! Optional input
                       Title         , &  ! Optional input
                       History       , &  ! Optional input
                       Comment       , &  ! Optional input
                       Message_Log   ) &  ! Error messaging
                     RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)  :: NC_Filename
    INTEGER               , INTENT(IN)  :: n_Layers     
    INTEGER               , INTENT(IN)  :: n_Absorbers  
    INTEGER               , INTENT(OUT) :: NC_FileID
    INTEGER     , OPTIONAL, INTENT(IN)  :: Version         
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Profile_Set_Id
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
    INTEGER :: n_Levels
    INTEGER :: Level_DimID
    INTEGER :: Layer_DimID
    INTEGER :: Absorber_DimID
    INTEGER :: Profile_DimID
    INTEGER :: PL_DimID
    INTEGER :: AUL_DimID
    TYPE(AtmProfile_type) :: Dummy
    
    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Check input
    IF ( n_Layers    < 1 .OR. &
         n_Absorbers < 1      ) THEN
      msg = 'Invalid dimension input detected.'
      CALL Create_Cleanup(); RETURN
    END IF
    n_Levels = n_Layers+1

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
    NF90_Status = NF90_DEF_DIM( NC_FileID,LEVEL_DIMNAME,n_Levels,Level_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//LEVEL_DIMNAME//' dimension in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    
    NF90_Status = NF90_DEF_DIM( NC_FileID,LAYER_DIMNAME,n_Layers,Layer_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//LAYER_DIMNAME//' dimension in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    
    NF90_Status = NF90_DEF_DIM( NC_FileID,ABSORBER_DIMNAME,n_Absorbers,Absorber_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//ABSORBER_DIMNAME//' dimension in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    
    NF90_Status = NF90_DEF_DIM( NC_FileID,PROFILE_DIMNAME,NF90_UNLIMITED,Profile_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//PROFILE_DIMNAME//' dimension in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    
    NF90_Status = NF90_DEF_DIM( NC_FileID,DESCRIPTION_DIMNAME,PDSL,PL_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//DESCRIPTION_DIMNAME//' dimension in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    
    NF90_Status = NF90_DEF_DIM( NC_FileID,ABSORBER_UNITS_DIMNAME,AUNSL,AUL_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//ABSORBER_UNITS_DIMNAME//' dimension in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF

    ! Write the global attributes
    ! ---------------------------
    Error_Status = WriteGAtts( NC_Filename                    , &
                               NC_FileID                      , &
                               Version        = Version       , &
                               Profile_Set_Id = Profile_Set_Id, &
                               Title          = Title         , &
                               History        = History       , &
                               Comment        = Comment       , &
                               Message_Log    = Message_Log     )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error writing global attributes to '//TRIM(NC_Filename)
      CALL Create_Cleanup(); RETURN
    END IF

    ! Define the AtmProfile variables
    ! -------------------------------
    Error_Status = DefineVar( NC_Filename            , &  ! Input
                              NC_FileID              , &  ! Input
                              Level_DimID            , &  ! Input
                              Layer_DimID            , &  ! Input
                              Absorber_DimID         , &  ! Input
                              Profile_DimID          , &  ! Input
                              PL_DimID               , &  ! Input
                              AUL_DimID              , &  ! Input
                              Message_Log=Message_Log  )  ! Error messaging
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

END MODULE AtmProfile_netCDF_IO
