!
! TauProfile_netCDF_IO
!
! Module containing routines to read and write TauProfile netCDF 
! format files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Apr-2002
!                       paul.vandelst@ssec.wisc.edu
!

MODULE TauProfile_netCDF_IO


  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds       , ONLY: fp
  USE Message_Handler  , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                               Display_Message
  USE TauProfile_Define, ONLY: TauProfile_type, &
                               Associated_TauProfile, &
                               Destroy_TauProfile, &
                               Allocate_TauProfile, &
                               CheckRelease_TauProfile, &
                               Info_TauProfile
  USE netcdf
  USE netCDF_Utility   ,  Open_TauProfile_netCDF =>  Open_netCDF, &
                         Close_TauProfile_netCDF => Close_netCDF
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Create_TauProfile_netCDF
  PUBLIC :: Modify_TauProfile_GAtts
  PUBLIC :: Inquire_TauProfile_netCDF
  PUBLIC :: Write_TauProfile_netCDF
  PUBLIC :: Read_TauProfile_netCDF
  PUBLIC :: Write_GeometricAngle_netCDF
  PUBLIC :: Read_GeometricAngle_netCDF


  ! ---------------------
  ! Overloaded procedures
  ! ---------------------
  INTERFACE Write_TauProfile_netCDF
    MODULE PROCEDURE Write_TauArray_rank1
    MODULE PROCEDURE Write_TauArray_rank2
    MODULE PROCEDURE Write_TauArray_rank3
    MODULE PROCEDURE Write_TauArray_rank4
    MODULE PROCEDURE Write_TauArray_rank5
    MODULE PROCEDURE Write_TauProfile_type
  END INTERFACE Write_TauProfile_netCDF

  INTERFACE Read_TauProfile_netCDF
    MODULE PROCEDURE Read_TauArray_rank1
    MODULE PROCEDURE Read_TauArray_rank2
    MODULE PROCEDURE Read_TauArray_rank3
    MODULE PROCEDURE Read_TauArray_rank4
    MODULE PROCEDURE Read_TauArray_rank5
    MODULE PROCEDURE Read_TauProfile_type
  END INTERFACE Read_TauProfile_netCDF

  INTERFACE Get_Index
    MODULE PROCEDURE Get_Integer_Index
    MODULE PROCEDURE Get_Float_Index
  END INTERFACE Get_Index


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Module RCS Id string
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  ! Keyword set value
  INTEGER,      PARAMETER :: SET = 1
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  ! Maximum angle secant value
  REAL(fp), PARAMETER :: ANGLE_LIMIT = 3.0_fp

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
  CHARACTER(*), PARAMETER :: LEVEL_DIMNAME        = 'n_levels'
  CHARACTER(*), PARAMETER :: LAYER_DIMNAME        = 'n_layers'
  CHARACTER(*), PARAMETER :: CHANNEL_DIMNAME      = 'n_Channels'
  CHARACTER(*), PARAMETER :: ANGLE_DIMNAME        = 'n_Angles'
  CHARACTER(*), PARAMETER :: PROFILE_DIMNAME      = 'n_Profiles'
  CHARACTER(*), PARAMETER :: MOLECULE_SET_DIMNAME = 'n_Molecule_Sets'

  ! Variable names
  CHARACTER(*), PARAMETER :: LEVEL_PRESSURE_VARNAME    = 'Level_Pressure'
  CHARACTER(*), PARAMETER :: CHANNEL_LIST_VARNAME      = 'Channel_list'
  CHARACTER(*), PARAMETER :: ANGLE_LIST_VARNAME        = 'Angle_list'
  CHARACTER(*), PARAMETER :: PROFILE_LIST_VARNAME      = 'Profile_list'
  CHARACTER(*), PARAMETER :: MOLECULE_SET_LIST_VARNAME = 'Molecule_Set_list'
  CHARACTER(*), PARAMETER :: TRANSMITTANCE_VARNAME     = 'transmittance'
  CHARACTER(*), PARAMETER :: GEOMETRIC_ANGLE_VARNAME   = 'Geometric_Angle'

  ! Variable long name attribute.
  CHARACTER(*), PARAMETER :: LONGNAME_ATTNAME = 'long_name'

  CHARACTER(*), PARAMETER :: LEVEL_PRESSURE_LONGNAME    = 'Level pressure'
  CHARACTER(*), PARAMETER :: CHANNEL_LIST_LONGNAME      = 'Sensor channel'
  CHARACTER(*), PARAMETER :: ANGLE_LIST_LONGNAME        = 'Zenith angle'
  CHARACTER(*), PARAMETER :: PROFILE_LIST_LONGNAME      = 'Profile number'
  CHARACTER(*), PARAMETER :: MOLECULE_SET_LIST_LONGNAME = 'Molecular species index'
  CHARACTER(*), PARAMETER :: TRANSMITTANCE_LONGNAME     = 'Instrument transmittance'
  CHARACTER(*), PARAMETER :: GEOMETRIC_ANGLE_LONGNAME   = 'Profile geometric angle'

  ! Variable description attribute.
  CHARACTER(*), PARAMETER :: DESCRIPTION_ATTNAME = 'description'

  CHARACTER(*), PARAMETER :: LEVEL_PRESSURE_DESC    = 'Atmospheric level pressures.'
  CHARACTER(*), PARAMETER :: CHANNEL_LIST_DESC      = 'List of sensor channel numbers.'
  CHARACTER(*), PARAMETER :: ANGLE_LIST_DESC        = 'List of the secant of the zenith angles.'
  CHARACTER(*), PARAMETER :: PROFILE_LIST_DESC      = 'List of atmospheric profile set indices.'
  CHARACTER(*), PARAMETER :: MOLECULE_SET_LIST_DESC = 'List of molecular species set indices.'
  CHARACTER(*), PARAMETER :: TRANSMITTANCE_DESC     = 'Instrument resolution channel transmittances'
  CHARACTER(*), PARAMETER :: GEOMETRIC_ANGLE_DESC   = 'Atmospheric profile geometric angles'

  ! Variable units attribute.
  CHARACTER(*), PARAMETER :: UNITS_ATTNAME = 'units'
  
  CHARACTER(*), PARAMETER :: LEVEL_PRESSURE_UNITS    = 'hectoPascals (hPa)'
  CHARACTER(*), PARAMETER :: CHANNEL_LIST_UNITS      = 'N/A'
  CHARACTER(*), PARAMETER :: ANGLE_LIST_UNITS        = 'None'
  CHARACTER(*), PARAMETER :: PROFILE_LIST_UNITS      = 'N/A'
  CHARACTER(*), PARAMETER :: MOLECULE_SET_LIST_UNITS = 'N/A'
  CHARACTER(*), PARAMETER :: TRANSMITTANCE_UNITS     = 'None'
  CHARACTER(*), PARAMETER :: GEOMETRIC_ANGLE_UNITS   = 'Degree'

  ! Variable _FillValue attribute.
  CHARACTER(*), PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'
  
  REAL(fp)    , PARAMETER :: FP_FILLVALUE                = -1.0_fp
  INTEGER     , PARAMETER :: IP_FILLVALUE                = -1
  REAL(fp)    , PARAMETER :: LEVEL_PRESSURE_FILLVALUE    = FP_FILLVALUE
  INTEGER     , PARAMETER :: CHANNEL_LIST_FILLVALUE      = IP_FILLVALUE 
  REAL(fp)    , PARAMETER :: ANGLE_LIST_FILLVALUE        = FP_FILLVALUE
  INTEGER     , PARAMETER :: PROFILE_LIST_FILLVALUE      = IP_FILLVALUE 
  INTEGER     , PARAMETER :: MOLECULE_SET_LIST_FILLVALUE = IP_FILLVALUE 
  REAL(fp)    , PARAMETER :: TRANSMITTANCE_FILLVALUE     = FP_FILLVALUE
  REAL(fp)    , PARAMETER :: GEOMETRIC_ANGLE_FILLVALUE   = FP_FILLVALUE

  ! Variable types
  INTEGER, PARAMETER :: LEVEL_PRESSURE_TYPE    = NF90_DOUBLE
  INTEGER, PARAMETER :: CHANNEL_LIST_TYPE      = NF90_INT
  INTEGER, PARAMETER :: ANGLE_LIST_TYPE        = NF90_DOUBLE
  INTEGER, PARAMETER :: PROFILE_LIST_TYPE      = NF90_INT
  INTEGER, PARAMETER :: MOLECULE_SET_LIST_TYPE = NF90_INT
  INTEGER, PARAMETER :: TRANSMITTANCE_TYPE     = NF90_DOUBLE
  INTEGER, PARAMETER :: GEOMETRIC_ANGLE_TYPE   = NF90_DOUBLE


CONTAINS


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  ! ==============================================
  ! Private functions to return an index location.
  ! ==============================================

  FUNCTION Get_Integer_Index( List, Value ) RESULT( Idx )
    INTEGER, INTENT(IN) :: List(:)
    INTEGER, INTENT(IN) :: Value
    INTEGER :: Idx
    Idx = MINLOC( ABS( List-Value ), DIM=1 )
    IF ( ( List(Idx)-Value ) /= 0 ) Idx = -1
  END FUNCTION Get_Integer_Index

  FUNCTION Get_Float_Index( List, Value ) RESULT( Idx )
    REAL(fp), INTENT(IN) :: List(:)
    REAL(fp), INTENT(IN) :: Value
    INTEGER :: Idx
    REAL(fp), PARAMETER :: TOLERANCE = EPSILON( 1.0_fp )
    Idx = MINLOC( ABS( List-Value ), DIM=1 )
    IF ( ABS( List(Idx)-Value ) > TOLERANCE ) Idx = -1
  END FUNCTION Get_Float_Index


!------------------------------------------------------------------------------
!
! NAME:
!       Write_TauProfile_GAtts
!
! PURPOSE:
!       Function to write the global attributes to a netCDF TauProfile
!       data file.
!
! CALLING SEQUENCE:
!       Error_Status = Write_TauProfile_GAtts( NC_Filename                      , &  ! Input
!                                              NC_FileID                        , &  ! Input
!                                              Release         =Release         , &  ! Optional input
!                                              Version         =Version         , &  ! Optional input
!                                              Sensor_Id       =Sensor_Id       , &  ! Optional input
!                                              WMO_Satellite_Id=WMO_Satellite_Id, &  ! Optional input
!                                              WMO_Sensor_Id   =WMO_Sensor_Id   , &  ! Optional input
!                                              ID_Tag          =ID_Tag          , &  ! Optional input
!                                              Title           =Title           , &  ! Optional input
!                                              History         =History         , &  ! Optional input
!                                              Comment         =Comment         , &  ! Optional input
!                                              Message_Log     =Message_Log       )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF TauProfile format data file to create.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:        NetCDF file ID number returned from the
!                         Open_TauProfile_netCDF() function.
!                         UNITS:      N/A
!                         TYPE:       Integer
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Release:          The release number of the netCDF TauProfile file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Version:          The version number of the netCDF TauProfile file.
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
!                         attribute field of the netCDF TauProfile file.
!                         Should contain a short tag used to identify the
!                         Profile set.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF TauProfile file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF TauProfile file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF TauProfile file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:      Character string specifying a filename in which
!                         any messages will be logged. If not specified,
!                         or if an error occurs opening the log file, the
!                         default action is to output messages to standard
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
!                            == FAILURE an error occurred writing the supplied
!                               global attributes.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION Write_TauProfile_GAtts( NC_Filename     , &  ! Input
                                   NC_FileID       , &  ! Input
                                   Release         , &  ! Optional input
                                   Version         , &  ! Optional input
                                   Sensor_Id       , &  ! Optional input
                                   WMO_Satellite_Id, &  ! Optional input
                                   WMO_Sensor_Id   , &  ! Optional input
                                   ID_Tag          , &  ! Optional input
                                   Title           , &  ! Optional input
                                   History         , &  ! Optional input
                                   Comment         , &  ! Optional input
                                   Message_Log     ) &  ! Error messaging
                                 RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: NC_Filename
    INTEGER     ,           INTENT(IN) :: NC_FileID
    INTEGER     , OPTIONAL, INTENT(IN) :: Release         
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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_TauProfile_GAtts'
    CHARACTER(*), PARAMETER :: WRITE_MODULE_HISTORY_GATTNAME   = 'write_module_history' 
    CHARACTER(*), PARAMETER :: CREATION_DATE_AND_TIME_GATTNAME = 'creation_date_and_time' 
    ! Local variables
    CHARACTER(256) :: GAttName
    CHARACTER(8)  :: cdate
    CHARACTER(10) :: ctime
    CHARACTER(5)  :: czone
    INTEGER :: NF90_Status

    ! Set up
    Error_Status = SUCCESS

    ! Software ID
    GAttName = WRITE_MODULE_HISTORY_GATTNAME
    NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                MODULE_RCS_ID )
    IF ( NF90_Status /= NF90_NOERR ) GOTO 1000
    

    ! Creation date
    CALL DATE_AND_TIME( cdate, ctime, czone )
    GAttName = CREATION_DATE_AND_TIME_GATTNAME
    NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                cdate(1:4)//'/'//cdate(5:6)//'/'//cdate(7:8)//', '// &
                                ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//' '// &
                                czone//'UTC' )
    IF ( NF90_Status /= NF90_NOERR ) GOTO 1000

    ! The Release
    IF ( PRESENT(Release) ) THEN
      GAttName = RELEASE_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  Release )
      IF ( NF90_Status /= NF90_NOERR ) GOTO 1000
    END IF

    ! The Version
    IF ( PRESENT(Version) ) THEN
      GAttName = VERSION_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  Version )
      IF ( NF90_Status /= NF90_NOERR ) GOTO 1000
    END IF

    ! The Sensor_Id
    IF ( PRESENT(Sensor_Id) ) THEN
      GAttName = SENSOR_ID_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  Sensor_Id )
      IF ( NF90_Status /= NF90_NOERR ) GOTO 1000
    END IF

    ! The WMO_Satellite_Id
    IF ( PRESENT(WMO_Satellite_Id) ) THEN
      GAttName = WMO_SATELLITE_ID_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  WMO_Satellite_Id )
      IF ( NF90_Status /= NF90_NOERR ) GOTO 1000
    END IF

    ! The WMO_Sensor_Id
    IF ( PRESENT(WMO_Sensor_Id) ) THEN
      GAttName = WMO_SENSOR_ID_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  WMO_Sensor_Id )
      IF ( NF90_Status /= NF90_NOERR ) GOTO 1000
    END IF

    ! The ID_Tag
    IF ( PRESENT(ID_Tag) ) THEN
      GAttName = ID_TAG_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  ID_Tag )
      IF ( NF90_Status /= NF90_NOERR ) GOTO 1000
    END IF

    ! The Title
    IF ( PRESENT(Title) ) THEN
      GAttName = TITLE_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  Title )
      IF ( NF90_Status /= NF90_NOERR ) GOTO 1000
    END IF

    ! The History
    IF ( PRESENT(History) ) THEN
      GAttName = HISTORY_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  History )
      IF ( NF90_Status /= NF90_NOERR ) GOTO 1000
    END IF

    ! The Comment
    IF ( PRESENT(Comment) ) THEN
      GAttName = COMMENT_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  Comment )
      IF ( NF90_Status /= NF90_NOERR ) GOTO 1000
    END IF

    !=====
    RETURN
    !=====
    
    ! Clean up after error
    1000 CONTINUE
    NF90_Status = NF90_CLOSE( NC_FileID )
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          'Error writing '//TRIM(GAttName)//' attribute to '//&
                          TRIM(NC_Filename)//' - '// &
                          TRIM(NF90_STRERROR( NF90_Status ) ), &
                          Error_Status, &
                          Message_Log=Message_Log )
    
  END FUNCTION Write_TauProfile_GAtts


!------------------------------------------------------------------------------
!
! NAME:
!       Read_TauProfile_GAtts
!
! PURPOSE:
!       Function to read the global attributes from a netCDF TauProfile
!       data file.
!
! CALLING SEQUENCE:
!       Error_Status = Read_TauProfile_GAtts( NC_Filename                      , &  ! Input
!                                             NC_FileID                        , &  ! Input
!                                             Release         =Release         , &  ! Optional output
!                                             Version         =Version         , &  ! Optional output
!                                             Sensor_Id       =Sensor_Id       , &  ! Optional output
!                                             WMO_Satellite_Id=WMO_Satellite_Id, &  ! Optional output
!                                             WMO_Sensor_Id   =WMO_Sensor_Id   , &  ! Optional output
!                                             ID_Tag          =ID_Tag          , &  ! Optional output
!                                             Title           =Title           , &  ! Optional output
!                                             History         =History         , &  ! Optional output
!                                             Comment         =Comment         , &  ! Optional output
!                                             Message_Log     =Message_Log       )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF TauProfile format data file to read from.
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
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:      Character string specifying a filename in which
!                         any messages will be logged. If not specified,
!                         or if an error occurs opening the log file, the
!                         default action is to output messages to standard
!                         output.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Release:          The release number of the netCDF TauProfile file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Version:          The version number of the netCDF TauProfile file.
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
!                         attribute field of the netCDF TauProfile file.
!                         Should contain a short tag used to identify the
!                         Profile set.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF TauProfile file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF TauProfile file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF TauProfile file.
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

  FUNCTION Read_TauProfile_GAtts( NC_Filename     , &  ! Input
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
                                RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER     ,           INTENT(IN)  :: NC_FileID
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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_TauProfile_GAtts'
    ! Local variables
    CHARACTER(256)  :: GAttName
    CHARACTER(5000) :: GAttString
    INTEGER :: NF90_Status

    ! Set up
    Error_Status = SUCCESS

    ! The Release
    IF ( PRESENT(Release) ) THEN
      GAttName = RELEASE_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  Release )
      IF ( NF90_Status /= NF90_NOERR ) GOTO 1000
    END IF

    ! The Version
    IF ( PRESENT(Version) ) THEN
      GAttName = VERSION_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  Version )
      IF ( NF90_Status /= NF90_NOERR ) GOTO 1000
    END IF

    ! The Sensor_Id
    IF ( PRESENT(Sensor_Id) ) THEN
      GAttString = ' '; Sensor_Id = ' '
      GAttName = SENSOR_ID_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  GAttString )
      IF ( NF90_Status /= NF90_NOERR ) GOTO 1000
      CALL Remove_NULL_Characters( GAttString )
      Sensor_Id = GAttString(1:MIN( LEN(Sensor_Id), LEN_TRIM(GAttString) ))
    END IF

    ! The WMO_Satellite_Id
    IF ( PRESENT(WMO_Satellite_Id) ) THEN
      GAttName = WMO_SATELLITE_ID_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  WMO_Satellite_Id )
      IF ( NF90_Status /= NF90_NOERR ) GOTO 1000
    END IF

    ! The WMO_Sensor_Id
    IF ( PRESENT(WMO_Sensor_Id) ) THEN
      GAttName = WMO_SENSOR_ID_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  WMO_Sensor_Id )
      IF ( NF90_Status /= NF90_NOERR ) GOTO 1000
    END IF

    ! The ID_Tag
    IF ( PRESENT(ID_Tag) ) THEN
      GAttString = ' '; ID_Tag = ' '
      GAttName = ID_TAG_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  GAttString )
      IF ( NF90_Status /= NF90_NOERR ) GOTO 1000
      CALL Remove_NULL_Characters( GAttString )
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
      IF ( NF90_Status /= NF90_NOERR ) GOTO 1000
      CALL Remove_NULL_Characters( GAttString )
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
      IF ( NF90_Status /= NF90_NOERR ) GOTO 1000
      CALL Remove_NULL_Characters( GAttString )
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
      IF ( NF90_Status /= NF90_NOERR ) GOTO 1000
      CALL Remove_NULL_Characters( GAttString )
      Comment = GAttString(1:MIN( LEN(Comment), LEN_TRIM(GAttString) ))
    END IF

    !=====
    RETURN
    !=====
    
    ! Clean up after error
    1000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          'Error reading '//TRIM(GAttName)//&
                          ' attribute from '//TRIM(NC_Filename), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Read_TauProfile_GAtts


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!
! NAME:
!       Create_TauProfile_netCDF
!
! PURPOSE:
!       Function to create a netCDF TauProfile data file for writing.
!
! CALLING SEQUENCE:
!       Error_Status = Create_TauProfile_netCDF( NC_Filename                      , &  ! Input
!                                                Level_Pressure                   , &  ! Input
!                                                Channel_List                     , &  ! Input
!                                                Angle_List                       , &  ! Input
!                                                Profile_List                     , &  ! Input
!                                                Molecule_Set_List                , &  ! Input
!                                                Release         =Release         , &  ! Optional input
!                                                Version         =Version         , &  ! Optional input
!                                                Sensor_ID       =Sensor_ID       , &  ! Optional Input
!                                                WMO_Satellite_ID=WMO_Satellite_ID, &  ! Optional Input
!                                                WMO_Sensor_ID   =WMO_Sensor_ID   , &  ! Optional Input
!                                                ID_Tag          =ID_Tag          , &  ! Optional input
!                                                Title           =Title           , &  ! Optional input
!                                                History         =History         , &  ! Optional input
!                                                Comment         =Comment         , &  ! Optional input
!                                                RCS_Id          =RCS_Id          , &  ! Revision control
!                                                Message_Log     =Message_Log       )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:        Character string specifying the name of the
!                           netCDF TauProfile format data file to create.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Level_Pressure:     The level pressures defining the layers.
!                           The N_LAYERS dimension is derived from
!                           this array.
!                           UNITS:      N/A
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Rank-1, n_Layers+1
!                           ATTRIBUTES: INTENT(IN)
!
!       Channel_List:       The list of Channel numbers to be written
!                           to the TauProfile file. The N_CHANNELS
!                           dimension is derived from this array.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Rank-1, n_Channels
!                           ATTRIBUTES: INTENT(IN)
!
!       Angle_List:         The list of secant(zenith Angle) values to
!                           be written to the TauProfile file. The 
!                           N_ANGLES dimension is derived from this array.
!                           UNITS:      N/A
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Rank-1, n_Angles
!                           ATTRIBUTES: INTENT(IN)
!
!       Profile_List:       The list of Profile numbers to be written
!                           to the TauProfile file. The N_PROFILES
!                           dimension is derived from this array.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Rank-1, n_Profiles
!                           ATTRIBUTES: INTENT(IN)
!
!       Molecule_Set_List:  The list of molecular absorber OR absorber set
!                           ID values. The N_MOLECULE_SETS dimension is
!                           derived from this array.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Rank-1, n_Molecule_Sets
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Release:            The release number of the netCDF TauProfile file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Version:            The version number of the netCDF TauProfile file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Sensor_ID:          A character string describing the sensor and
!                           platform used to construct sensor specific
!                           filenames. Examples of sensor ids are
!                             hirs3_n17
!                             ssmis_f16
!                             imgr_g11
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       WMO_Satellite_ID:   The WMO code for identifying satellite
!                           platforms. Taken from the WMO common
!                           code tables at:
!                             http://www.wmo.ch/web/ddbs/Code-tables.html
!                           The Satellite ID is from Common Code
!                           table C-5, or code table 0 01 007 in BUFR
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       WMO_Sensor_ID:      The WMO code for identifying a satelite
!                           sensor. Taken from the WMO common
!                           code tables at:
!                             http://www.wmo.ch/web/ddbs/Code-tables.html
!                           The Sensor ID is from Common Code
!                           table C-8, or code table 0 02 019 in BUFR
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       ID_Tag:             Character string written into the ID_TAG global
!                           attribute field of the netCDF TauProfile file.
!                           Should contain a short tag used to identify the
!                           Profile set.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:              Character string written into the TITLE global
!                           attribute field of the netCDF TauProfile file.
!                           Should contain a succinct description of what
!                           is in the netCDF datafile.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the netCDF TauProfile file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF TauProfile file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:        Character string specifying a filename in which
!                           any messages will be logged. If not specified,
!                           or if an error occurs opening the log file, the
!                           default action is to output messages to standard
!                           output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:      The return value is an integer defining the error status.  
!                          The error codes are defined in the Message_Handler module. 
!                          If == SUCCESS the netCDF file creation was successful.     
!                             == FAILURE an unrecoverable error occurred.             
!                             == WARNING - an error occurred writing any of the       
!                                          supplied global attributes.                
!                                        - an error occurred closing the netCDF file. 
!                          UNITS:      N/A                                            
!                          TYPE:       INTEGER                                        
!                          DIMENSION:  Scalar                                         
!
!------------------------------------------------------------------------------

  FUNCTION Create_TauProfile_netCDF( NC_Filename      , &  ! Input
                                     Level_Pressure   , &  ! Input
                                     Channel_List     , &  ! Input
                                     Angle_List       , &  ! Input
                                     Profile_List     , &  ! Input
                                     Molecule_Set_List, &  ! Input
                                     Release          , &  ! Optional input
                                     Version          , &  ! Optional input
                                     Sensor_ID        , &  ! Optional Input
                                     WMO_Satellite_ID , &  ! Optional Input
                                     WMO_Sensor_ID    , &  ! Optional Input
                                     ID_Tag           , &  ! Optional input
                                     Title            , &  ! Optional input
                                     History          , &  ! Optional input
                                     Comment          , &  ! Optional input
                                     RCS_Id           , &  ! Revision control
                                     Message_Log      ) &  ! Error messaging
                                   RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)  :: NC_Filename
    REAL(fp)              , INTENT(IN)  :: Level_Pressure(0:)
    INTEGER               , INTENT(IN)  :: Channel_List(:)
    REAL(fp)              , INTENT(IN)  :: Angle_List(:)
    INTEGER               , INTENT(IN)  :: Profile_List(:)
    INTEGER               , INTENT(IN)  :: Molecule_Set_List(:)
    INTEGER     , OPTIONAL, INTENT(IN)  :: Release         
    INTEGER     , OPTIONAL, INTENT(IN)  :: Version         
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Sensor_ID
    INTEGER     , OPTIONAL, INTENT(IN)  :: WMO_Satellite_ID
    INTEGER     , OPTIONAL, INTENT(IN)  :: WMO_Sensor_ID
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: ID_Tag
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Create_TauProfile_netCDF'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: NC_FileID
    INTEGER :: NF90_Status
    INTEGER :: Put_Status(4)
    INTEGER :: Close_Status
    INTEGER :: StrLen_DimID
    INTEGER :: n_Layers,        Layer_DimID
    INTEGER :: n_Levels,        Level_DimID
    INTEGER :: n_Channels,      Channel_DimID
    INTEGER :: n_Angles,        Angle_DimID
    INTEGER :: n_Profiles,      Profile_DimID
    INTEGER :: n_Molecule_Sets, Molecule_Set_DimID
    INTEGER :: VarID
    TYPE(TauProfile_type) :: TPdummy

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! Check level pressure
    n_Levels = SIZE( Level_Pressure )
    n_Layers = n_Levels - 1
    IF ( n_Layers < 1 ) THEN
      Message = 'LEVEL_PRESSURE array must have at least 2-elements.'
      GOTO 2000
    END IF
    IF ( ANY( Level_Pressure < ZERO ) ) THEN
      Message = 'Invalid LEVEL_PRESSURE value found.'
      GOTO 2000
    END IF

    ! Check channel input
    n_Channels = SIZE( Channel_List )
    IF ( n_Channels < 1 ) THEN
      Message = 'CHANNEL_LIST array must be non-zero size.'
      GOTO 2000
    END IF
    IF ( ANY( Channel_List < 1 ) ) THEN
      Message = 'Invalid CHANNEL_LIST value found.'
      GOTO 2000
    END IF

    !  Check Angle input
    n_Angles = SIZE( Angle_List )
    IF ( n_Angles < 1 ) THEN
      Message = 'ANGLE_LIST array must be non-zero size.'
      GOTO 2000
    END IF
    IF ( ANY( ABS(Angle_List) > ANGLE_LIMIT ) ) THEN
      Message = 'Invalid ANGLE_LIST value found.'
      GOTO 2000
    END IF

    ! Check Profile input
    n_Profiles = SIZE( Profile_List )
    IF ( n_Profiles < 1 ) THEN
      Message = 'PROFILE_LIST array must be non-zero size.'
      GOTO 2000
    END IF
    IF ( ANY( Profile_List < 1 ) ) THEN
      Message = 'Invalid PROFILE_LIST value found.'
      GOTO 2000
    END IF

    ! Check molecule set input
    n_Molecule_Sets = SIZE( Molecule_Set_List )
    IF ( n_Molecule_Sets < 1 ) THEN
      Message = 'MOLECULE_SET_LIST array must be non-zero size.'
      GOTO 2000
    END IF
    IF ( ANY( Molecule_Set_List < 1 ) ) THEN
      Message = 'Invalid MOLECULE_SET_LIST value found.'
      GOTO 2000
    END IF


    ! Create the data file
    ! --------------------
    NF90_Status = NF90_CREATE( NC_Filename, &
                               NF90_CLOBBER, &
                               NC_FileID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error creating '//TRIM(NC_Filename)//' - '//&
                TRIM(NF90_STRERROR( NF90_Status ))
      GOTO 2000
    END IF


    ! Define the dimensions
    ! ---------------------
    ! The number of levels
    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                LEVEL_DIMNAME, n_Levels, Level_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//LEVEL_DIMNAME//' dimension in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      GOTO 1000
    END IF

    ! The number of layers
    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                LAYER_DIMNAME, n_Layers, Layer_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//LAYER_DIMNAME//' dimension in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      GOTO 1000
    END IF

    ! The number of channels
    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                CHANNEL_DIMNAME, n_Channels, Channel_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//CHANNEL_DIMNAME//' dimension in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      GOTO 1000
    END IF

    ! The number of angles
    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                ANGLE_DIMNAME, n_Angles, Angle_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//ANGLE_DIMNAME//' dimension in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      GOTO 1000
    END IF

    ! The number of profiles
    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                PROFILE_DIMNAME, n_Profiles, Profile_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//PROFILE_DIMNAME//' dimension in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      GOTO 1000
    END IF

    ! The number of molecule sets
    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                MOLECULE_SET_DIMNAME, NF90_UNLIMITED, Molecule_Set_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//MOLECULE_SET_DIMNAME//' dimension in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      GOTO 1000
    END IF


    ! Write the global attributes
    ! ---------------------------
    Error_Status = Write_TauProfile_GAtts( NC_Filename                      , &
                                           NC_FileID                        , &
                                           Release         =Release         , &
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
      Message = 'Error writing global attributes to '//TRIM(NC_Filename)
      GOTO 1000
    END IF


    ! Define the variables
    ! --------------------
    ! The level pressure
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                LEVEL_PRESSURE_VARNAME, &
                                LEVEL_PRESSURE_TYPE, &
                                dimIDs=(/Level_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//LEVEL_PRESSURE_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      GOTO 1000
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  LEVEL_PRESSURE_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  LEVEL_PRESSURE_DESC )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  LEVEL_PRESSURE_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  LEVEL_PRESSURE_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//LEVEL_PRESSURE_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      GOTO 1000
    END IF

    ! The channel list
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                CHANNEL_LIST_VARNAME, &
                                CHANNEL_LIST_TYPE, &
                                dimIDs=(/Channel_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//CHANNEL_LIST_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      GOTO 1000
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  CHANNEL_LIST_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  CHANNEL_LIST_DESC )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  CHANNEL_LIST_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  CHANNEL_LIST_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//CHANNEL_LIST_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      GOTO 1000
    END IF

    ! The angle list
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                ANGLE_LIST_VARNAME, &
                                ANGLE_LIST_TYPE, &
                                dimIDs=(/Angle_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//ANGLE_LIST_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      GOTO 1000
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  ANGLE_LIST_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  ANGLE_LIST_DESC )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  ANGLE_LIST_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  ANGLE_LIST_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//ANGLE_LIST_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      GOTO 1000
    END IF

    ! The profile set list
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                PROFILE_LIST_VARNAME, &
                                PROFILE_LIST_TYPE, &
                                dimIDs=(/Profile_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//PROFILE_LIST_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      GOTO 1000
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  PROFILE_LIST_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  PROFILE_LIST_DESC )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  PROFILE_LIST_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  PROFILE_LIST_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//PROFILE_LIST_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      GOTO 1000
    END IF

    ! The molecule set list
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                MOLECULE_SET_LIST_VARNAME, &
                                MOLECULE_SET_LIST_TYPE, &
                                dimIDs=(/Molecule_Set_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//MOLECULE_SET_LIST_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      GOTO 1000
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  MOLECULE_SET_LIST_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  MOLECULE_SET_LIST_DESC )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  MOLECULE_SET_LIST_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  MOLECULE_SET_LIST_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//MOLECULE_SET_LIST_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      GOTO 1000
    END IF

    ! The geometic angle
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                GEOMETRIC_ANGLE_VARNAME, &
                                GEOMETRIC_ANGLE_TYPE, &
                                dimIDs=(/ Layer_DimID       , &
                                          Angle_DimID       , &
                                          Profile_DimID /)   , &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//GEOMETRIC_ANGLE_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      GOTO 1000
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  GEOMETRIC_ANGLE_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  GEOMETRIC_ANGLE_DESC )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  GEOMETRIC_ANGLE_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  GEOMETRIC_ANGLE_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//GEOMETRIC_ANGLE_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      GOTO 1000
    END IF

    ! The transmittance
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                TRANSMITTANCE_VARNAME, &
                                TRANSMITTANCE_TYPE, &
                                dimIDs=(/ Layer_DimID       , &
                                          Channel_DimID     , &
                                          Angle_DimID       , &
                                          Profile_DimID     , &
                                          Molecule_Set_DimID /), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//TRANSMITTANCE_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      GOTO 1000
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  TRANSMITTANCE_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  TRANSMITTANCE_DESC )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  TRANSMITTANCE_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  TRANSMITTANCE_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//TRANSMITTANCE_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      GOTO 1000
    END IF


    ! Take netCDF file out of define mode
    ! -----------------------------------
    NF90_Status = NF90_ENDDEF( NC_FileID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error taking '//TRIM(NC_Filename)//' out of define mode'
      GOTO 1000
    END IF


    ! Write the "dimension" data
    ! --------------------------
    ! Level pressure
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        LEVEL_PRESSURE_VARNAME, &
                                        Level_Pressure )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//LEVEL_PRESSURE_VARNAME//' to '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    ! Channel list
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        CHANNEL_LIST_VARNAME, &
                                        Channel_List )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//CHANNEL_LIST_VARNAME//' to '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    ! Angle list
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        ANGLE_LIST_VARNAME, &
                                        Angle_List )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//ANGLE_LIST_VARNAME//' to '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    ! Profile list
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        PROFILE_LIST_VARNAME, &
                                        Profile_List )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//PROFILE_LIST_VARNAME//' to '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    ! Molecule set list
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        MOLECULE_SET_LIST_VARNAME, &
                                        Molecule_Set_List )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//MOLECULE_SET_LIST_VARNAME//' to '//TRIM(NC_Filename)
      GOTO 1000
    END IF


    ! Close the file
    ! --------------
    Close_Status = Close_TauProfile_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauProfile data file '// &
                            TRIM(NC_Filename), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END IF

    !=====
    RETURN
    !=====
    
    ! Clean up after error
    1000 CONTINUE
    NF90_Status = NF90_CLOSE( NC_FileID )
    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Create_TauProfile_netCDF


!------------------------------------------------------------------------------
!
! NAME:
!       Modify_TauProfile_GAtts
!
! PURPOSE:
!       Function to modify the global attributes in an existing netCDF format
!       TauProfile data file.
!
! CALLING SEQUENCE:
!       Error_Status = Modify_TauProfile_GAtts( NC_Filename                      , &  ! Input
!                                               Release         =Release         , &  ! Optional input
!                                               Version         =Version         , &  ! Optional input
!                                               Sensor_ID       =Sensor_ID       , &  ! Optional Input
!                                               WMO_Satellite_ID=WMO_Satellite_ID, &  ! Optional Input
!                                               WMO_Sensor_ID   =WMO_Sensor_ID   , &  ! Optional Input
!                                               ID_Tag          =ID_Tag          , &  ! Optional Input
!                                               Title           =Title           , &  ! Optional Input
!                                               History         =History         , &  ! Optional Input
!                                               Comment         =Comment         , &  ! Optional Input
!                                               RCS_Id          =RCS_Id          , &  ! Revision control
!                                               Message_Log     =Message_Log       )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         TauProfile netCDF format data file of which the
!                         global attributes are to be modifed.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Release:          The release number of the netCDF TauProfile file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Version:          The version number of the netCDF TauProfile file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Sensor_ID:        A character string describing the sensor and
!                         platform used to construct sensor specific
!                         filenames. Examples of sensor ids are
!                           hirs3_n17
!                           ssmis_f16
!                           imgr_g11
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       WMO_Satellite_ID: The WMO code for identifying satellite
!                         platforms. Taken from the WMO common
!                         code tables at:
!                           http://www.wmo.ch/web/ddbs/Code-tables.html
!                         The Satellite ID is from Common Code
!                         table C-5, or code table 0 01 007 in BUFR
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       WMO_Sensor_ID:    The WMO code for identifying a satelite
!                         sensor. Taken from the WMO common
!                         code tables at:
!                           http://www.wmo.ch/web/ddbs/Code-tables.html
!                         The Sensor ID is from Common Code
!                         table C-8, or code table 0 02 019 in BUFR
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       ID_Tag:           Character string to write into the ID_TAG global
!                         attribute field of the netCDF TauProfile file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:            Character string to write into the TITLE global
!                         attribute field of the netCDF TauProfile file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:          Character string to write into the HISTORY global
!                         attribute field of the netCDF TauProfile file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:          Character string to write into the COMMENT global
!                         attribute field of the netCDF TauProfile file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:      Character string specifying a filename in which any
!                         messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output messages to standard output.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:           Character string containing the Revision Control
!                         System Id field for the module.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error status.
!                         The error codes are defined in the Message_Handler module.
!                         If == SUCCESS the netCDF file attribute write was successful.
!                            == WARNING an error occurred closing the netCDF file.
!                                       Because this is attrribute data, any I/O
!                                       errors are not flagged as fatal.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION Modify_TauProfile_GAtts( NC_Filename     , &  ! Input
                                    Release         , &  ! Optional input
                                    Version         , &  ! Optional input
                                    Sensor_ID       , &  ! Optional Input
                                    WMO_Satellite_ID, &  ! Optional Input
                                    WMO_Sensor_ID   , &  ! Optional Input
                                    ID_Tag          , &  ! Optional Input
                                    Title           , &  ! Optional Input
                                    History         , &  ! Optional Input
                                    Comment         , &  ! Optional Input
                                    RCS_Id          , &  ! Revision control
                                    Message_Log     ) &  ! Error messaging
                                  RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER     , OPTIONAL, INTENT(IN)  :: Release         
    INTEGER     , OPTIONAL, INTENT(IN)  :: Version         
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Sensor_ID
    INTEGER     , OPTIONAL, INTENT(IN)  :: WMO_Satellite_ID
    INTEGER     , OPTIONAL, INTENT(IN)  :: WMO_Sensor_ID
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: ID_Tag
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Modify_TauProfile_GAtts'
    ! Function variables
    CHARACTER(256) :: Message
    INTEGER :: NC_FileID
    INTEGER :: NF90_Status
    INTEGER :: Close_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! Open the file
    ! -------------
    Error_Status = Open_TauProfile_netCDF( TRIM(NC_Filename), &
                                           NC_FileID, &
                                           Mode = 'READWRITE' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF TauProfile data file '//TRIM(NC_Filename)
      GOTO 2000
    END IF


    ! Put the file in define mode
    ! ---------------------------
    NF90_Status = NF90_REDEF( NC_FileID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error putting '//TRIM(NC_Filename)//' in DEFINE mode - '// &
                 TRIM( NF90_STRERROR( NF90_Status ) )
      GOTO 1000
    END IF


    ! Write the global attributes
    ! ---------------------------
    Error_Status = Write_TauProfile_GAtts( NC_Filename                      , &
                                           NC_FileID                        , &
                                           Release         =Release         , &
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
      Message = 'Error writing global attribute to '//TRIM(NC_Filename)
      GOTO 1000
    END IF


    ! Close the file
    ! --------------
    Close_Status = Close_TauProfile_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauProfile data file '// &
                            TRIM(NC_Filename), &
                            WARNING, &
                            Message_Log=Message_Log )
    END IF

    !=====
    RETURN
    !=====
    
    ! Clean up after an error
    ! -----------------------
    1000 CONTINUE
    NF90_Status = NF90_CLOSE( NC_FileID )
    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Modify_TauProfile_GAtts


!------------------------------------------------------------------------------
!
! NAME:
!       Inquire_TauProfile_netCDF
!
! PURPOSE:
!       Function to inquire a netCDF TauProfile format file to obtain the number of
!       Channels and the Channel list.
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_TauProfile_netCDF( NC_Filename                        , &  ! Input
!                                                 n_Layers         =n_Layers         , &  ! Optional output
!                                                 n_Channels       =n_Channels       , &  ! Optional output
!                                                 n_Angles         =n_Angles         , &  ! Optional output
!                                                 n_Profiles       =n_Profiles       , &  ! Optional output
!                                                 n_Molecule_Sets  =n_Molecule_Sets  , &  ! Optional output
!                                                 Release          =Release          , &  ! Optional output
!                                                 Version          =Version          , &  ! Optional output
!                                                 Sensor_ID        =Sensor_ID        , &  ! Optional output
!                                                 WMO_Satellite_ID =WMO_Satellite_ID , &  ! Optional output
!                                                 WMO_Sensor_ID    =WMO_Sensor_ID    , &  ! Optional output
!                                                 Level_Pressure   =Level_Pressure   , &  ! Optional output
!                                                 Channel_List     =Channel_List     , &  ! Optional output
!                                                 Angle_List       =Angle_List       , &  ! Optional output
!                                                 Profile_List     =Profile_List     , &  ! Optional output
!                                                 Molecule_Set_List=Molecule_Set_List, &  ! Optional output
!                                                 ID_Tag           =ID_Tag           , &  ! Optional output
!                                                 Title            =Title            , &  ! Optional output
!                                                 History          =History          , &  ! Optional output
!                                                 Comment          =Comment          , &  ! Optional output
!                                                 RCS_Id           =RCS_Id           , &  ! Revision control
!                                                 Message_Log      =Message_Log        )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:        Character string specifying the name of the
!                           TauProfile netCDF format data file to inquire.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Layers:           The number of atmospheric layers dimension of the
!                           transmittance data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Channels:         The number of channels dimension of the
!                           transmittance data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Angles:           The number of zenith angles dimension of the
!                           transmittance data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Profiles:         The number of profiles dimension of the
!                           transmittance data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Molecule_Sets:    The number of molecule sets dimension of the
!                           transmittance data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:            The release number of the netCDF TauProfile file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:            The version number of the netCDF TauProfile file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Sensor_ID:          A character string identifying the sensor and
!                           satellite platform used to contruct filenames.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WMO_Satellite_ID:   The WMO code for identifying satellite
!                           platforms. Taken from the WMO common
!                           code tables at:
!                             http://www.wmo.ch/web/ddbs/Code-tables.html
!                           The Satellite ID is from Common Code
!                           table C-5, or code table 0 01 007 in BUFR
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WMO_Sensor_ID:      The WMO code for identifying a satelite
!                           sensor. Taken from the WMO common
!                           code tables at:
!                             http://www.wmo.ch/web/ddbs/Code-tables.html
!                           The Sensor ID is from Common Code
!                           table C-8, or code table 0 02 019 in BUFR
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Level_Pressure:     The level pressures defining the layers.
!                           The N_LAYERS dimension is derived from
!                           this array.
!                           UNITS:      N/A
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Rank-1, n_Layers+1
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Channel_List:       The list of channel numbers present in the netCDF
!                           TauProfile file. The list may not necessarily
!                           start at 1 or contain contiguous values.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Rank-1, n_Channels
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Angle_List:         The list of the secant of the zenith angles for
!                           the TauProfile data.
!                           UNITS:      N/A
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Rank-1, n_Angles
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Profile_List:       The list of profile numbers present in the netCDF
!                           TauProfile file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Rank-1, n_Profiles
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Molecule_Set_List:  The list of ID numbers for the molecule sets
!                           used in the generation of the TauProfile
!                           data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Rank-1, n_Molecule_Sets
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       ID_Tag:             Character string written into the ID_TAG global
!                           attribute field of the netCDF TauProfile file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:              Character string written into the TITLE global
!                           attribute field of the netCDF TauProfile file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the netCDF TauProfile file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF TauProfile file.
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
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the Message_Handler module.
!                           If == SUCCESS the netCDF file inquiry was successful.
!                              == FAILURE an error occurred reading any of the requested
!                                         dimension or variable data.
!                              == WARNING - an error occurred reading any of the requested
!                                           global file attributes, or
!                                         - an error occurred closing the netCDF file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! RESTRICTIONS:
!       To successfully return any of the CHANNEL, ANGLE, PROFILE, or
!       MOLECULE_SET list arrays, the dummy arguments must have at least
!       the same size as the dataset in the netCDF file.
!
!------------------------------------------------------------------------------

  FUNCTION Inquire_TauProfile_netCDF( NC_Filename      , &  ! Input
                                      n_Layers         , &  ! Optional output
                                      n_Channels       , &  ! Optional output
                                      n_Angles         , &  ! Optional output
                                      n_Profiles       , &  ! Optional output
                                      n_Molecule_Sets  , &  ! Optional output
                                      Release          , &  ! Optional output
                                      Version          , &  ! Optional output
                                      Sensor_ID        , &  ! Optional output
                                      WMO_Satellite_ID , &  ! Optional output
                                      WMO_Sensor_ID    , &  ! Optional output
                                      Level_Pressure   , &  ! Optional output
                                      Channel_List     , &  ! Optional output
                                      Angle_List       , &  ! Optional output
                                      Profile_List     , &  ! Optional output
                                      Molecule_Set_List, &  ! Optional output
                                      ID_Tag           , &  ! Optional output
                                      Title            , &  ! Optional output
                                      History          , &  ! Optional output
                                      Comment          , &  ! Optional output
                                      RCS_Id           , &  ! Revision control
                                      Message_Log      ) &  ! Error messaging
                                    RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Layers
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Channels
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Angles
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Profiles
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Molecule_Sets
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release         
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Sensor_ID   
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Satellite_ID 
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Sensor_ID
    REAL(fp)    , OPTIONAL, INTENT(OUT) :: Level_Pressure(:)
    INTEGER     , OPTIONAL, INTENT(OUT) :: Channel_List(:)
    REAL(fp)    , OPTIONAL, INTENT(OUT) :: Angle_List(:)
    INTEGER     , OPTIONAL, INTENT(OUT) :: Profile_List(:)
    INTEGER     , OPTIONAL, INTENT(OUT) :: Molecule_Set_List(:)
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: ID_Tag
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Inquire_TauProfile_netCDF'
    ! Function variables
    CHARACTER(1000) :: Message
    INTEGER :: NC_FileID
    INTEGER :: Close_Status
    INTEGER :: k, l, m, i, j
    INTEGER :: n_Levels

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! Open the file
    ! -------------
    Error_Status = Open_TauProfile_netCDF( TRIM(NC_Filename), &
                                           NC_FileID, &
                                           Mode = 'READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF TauProfile data file '//&
                TRIM(NC_Filename)
      GOTO 2000
    END IF


    ! Get the dimensions
    ! ------------------
    ! The number of layers
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         LAYER_DIMNAME, &
                                         k, &
                                         Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//LAYER_DIMNAME//&
                ' dimension from '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    ! The number of Channels
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         CHANNEL_DIMNAME, &
                                         l, &
                                         Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//CHANNEL_DIMNAME//&
                ' dimension from '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    ! The number of Angles
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         ANGLE_DIMNAME, &
                                         i, &
                                         Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//ANGLE_DIMNAME//&
                ' dimension from '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    ! The number of Profiles
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         PROFILE_DIMNAME, &
                                         m, &
                                         Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//PROFILE_DIMNAME//&
                ' dimension from '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    ! The number of molecule sets
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         MOLECULE_SET_DIMNAME, &
                                         j, &
                                         Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//MOLECULE_SET_DIMNAME//&
                ' dimension from '//TRIM(NC_Filename)
      GOTO 1000
    END IF

    ! Set the dimension return values
    IF ( PRESENT( n_Layers        ) ) n_Layers        = k
    IF ( PRESENT( n_Channels      ) ) n_Channels      = l
    IF ( PRESENT( n_Angles        ) ) n_Angles        = i
    IF ( PRESENT( n_Profiles      ) ) n_Profiles      = m
    IF ( PRESENT( n_Molecule_Sets ) ) n_Molecule_Sets = j


    ! Get the dimension list data
    ! ---------------------------
    ! The level pressure
    IF ( PRESENT( Level_Pressure ) ) THEN
    
      ! Set the number of levels
      n_Levels = k + 1
      
      ! Check the dummy argument size
      IF ( SIZE( Level_Pressure ) < n_Levels ) THEN
        Message = 'LEVEL_PRESSURE array too small to hold data.'
        GOTO 1000
      END IF
      
      ! Initialise the entire array (in case it's bigger
      ! than the number of elements in the file)
      Level_Pressure = LEVEL_PRESSURE_FILLVALUE
      
      ! Get the data
      Error_Status = Get_netCDF_Variable( NC_FileID, &
                                          LEVEL_PRESSURE_VARNAME, &
                                          Level_Pressure(1:n_Levels) )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading '//LEVEL_PRESSURE_VARNAME//&
                  ' data from '//TRIM(NC_Filename)
        GOTO 1000
      END IF
    END IF
    
    ! The Channel list
    IF ( PRESENT( Channel_List ) ) THEN
    
      ! Check the dummy argument size
      IF ( SIZE( Channel_List ) < l ) THEN
        Message = 'CHANNEL_LIST array too small to hold data.'
        GOTO 1000
      END IF

      ! Initialise the entire array (in case it's bigger
      ! than the number of elements in the file)
      Channel_List = CHANNEL_LIST_FILLVALUE

      ! Get the data
      Error_Status = Get_netCDF_Variable( NC_FileID, &
                                          CHANNEL_LIST_VARNAME, &
                                          Channel_List(1:l) )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading '//CHANNEL_LIST_VARNAME//&
                  ' data from '//TRIM(NC_Filename)
        GOTO 1000
      END IF
    END IF

    ! The Angle list
    IF ( PRESENT( Angle_List ) ) THEN

      ! Check the dummy argument size
      IF ( SIZE( Angle_List ) < i ) THEN
        Message = 'ANGLE_LIST array too small to hold data.'
        GOTO 1000
      END IF

      ! Initialise the entire array (in case it's bigger
      ! than the number of elements in the file)
      Angle_List = ANGLE_LIST_FILLVALUE

      ! Get the data
      Error_Status = Get_netCDF_Variable( NC_FileID, &
                                          ANGLE_LIST_VARNAME, &
                                          Angle_List(1:i) )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading '//ANGLE_LIST_VARNAME//&
                  ' data from '//TRIM(NC_Filename)
        GOTO 1000
      END IF
    END IF

    ! The Profile list
    IF ( PRESENT( Profile_List ) ) THEN

      ! Check the dummy argument size
      IF ( SIZE( Profile_List ) < m ) THEN
        Message = 'PROFILE_LIST array too small to hold data.'
        GOTO 1000
      END IF

      ! Initialise the entire array (in case it's bigger
      ! than the number of elements in the file)
      Profile_List = PROFILE_LIST_FILLVALUE

      ! Get the data
      Error_Status = Get_netCDF_Variable( NC_FileID, &
                                          PROFILE_LIST_VARNAME, &
                                          Profile_List(1:m) )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading '//PROFILE_LIST_VARNAME//&
                  ' data from '//TRIM(NC_Filename)
        GOTO 1000
      END IF
    END IF

    ! The Molecule_Set list
    IF ( PRESENT( Molecule_Set_List ) ) THEN

      ! Check the dummy argument size
      IF ( SIZE( Molecule_Set_List ) < j ) THEN
        Message = 'MOLECULE_SET_LIST array too small to hold data.'
        GOTO 1000
      END IF

      ! Initialise the entire array (in case it's bigger
      ! than the number of elements in the file)
      Molecule_Set_List = MOLECULE_SET_LIST_FILLVALUE

      ! Get the data
      Error_Status = Get_netCDF_Variable( NC_FileID, &
                                          MOLECULE_SET_LIST_VARNAME, &
                                          Molecule_Set_List(1:j) )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading '//MOLECULE_SET_LIST_VARNAME//&
                  ' data from '//TRIM(NC_Filename)
        GOTO 1000
      END IF
    END IF


    ! Get the global attributes
    ! -------------------------
    Error_Status = Read_TauProfile_GAtts( TRIM(NC_Filename)                 , &
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
      Message = 'Error reading global attribute from '//TRIM(NC_Filename)
      GOTO 1000
    END IF


    ! Close the file
    ! --------------
    Close_Status = Close_TauProfile_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauProfile data file '// &
                            TRIM(NC_Filename), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

    !=====
    RETURN
    !=====

    ! Clean up after an error
    1000 CONTINUE
    Close_Status = Close_TauProfile_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing input file during error cleanup.'
    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Inquire_TauProfile_netCDF


!------------------------------------------------------------------------------
!
! NAME:
!       Write_TauProfile_netCDF
!
! PURPOSE:
!       Function to write TauProfile data to a netCDF format TauProfile
!       file. Note that the file should already have been created via the
!       Create_TauProfile_netCDF() function.
!
! CALLING SEQUENCE:
!
!   To write a K x 1 transmittance profile vector:
!   ----------------------------------------------
!
!     Error_Status = Write_TauProfile_netCDF( NC_Filename , &  ! Input
!                                             Tau         , &  ! Input, Rank-1 K
!                                             Channel     , &  ! Input, L
!                                             Angle       , &  ! Input, I
!                                             Profile     , &  ! Input, M
!                                             Molecule_Set, &  ! Input, J
!                                             Quiet      =Quiet      , &  ! Optional input
!                                             RCS_Id     =RCS_Id     , &  ! Revision control
!                                             Message_Log=Message_Log  )  ! Error messaging
!
!
!   To write a K x L (n_Channels) transmittance profile array:
!   ----------------------------------------------------------
!
!     Error_Status = Write_TauProfile_netCDF( NC_Filename , &  ! Input
!                                             Tau         , &  ! Input, Rank-2 K x L
!                                             Angle       , &  ! Input, I
!                                             Profile     , &  ! Input, M
!                                             Molecule_Set, &  ! Input, J
!                                             Quiet      =Quiet      , &  ! Optional input
!                                             RCS_Id     =RCS_Id     , &  ! Revision control
!                                             Message_Log=Message_Log  )  ! Error messaging
!
!
!   To write a K x L x I (n_Angles) transmittance profile array:
!   ------------------------------------------------------------
!
!     Error_Status = Write_TauProfile_netCDF( NC_Filename , &  ! Input
!                                             Tau         , &  ! Input, Rank-3 K x L x I
!                                             Profile     , &  ! Input, M
!                                             Molecule_Set, &  ! Input, J
!                                             Quiet      =Quiet      , &  ! Optional input
!                                             RCS_Id     =RCS_Id     , &  ! Revision control
!                                             Message_Log=Message_Log  )  ! Error messaging
!
!
!   To write a K x L x I x M (n_Profiles) transmittance profile array:
!   ------------------------------------------------------------------
!
!     Error_Status = Write_TauProfile_netCDF( NC_Filename , &  ! Input
!                                             Tau         , &  ! Input, Rank-4 K x L x I x M
!                                             Molecule_Set, &  ! Input, J
!                                             Quiet      =Quiet      , &  ! Optional input
!                                             RCS_Id     =RCS_Id     , &  ! Revision control
!                                             Message_Log=Message_Log  )  ! Error messaging
!
!
!   To write a K x L x I x M x J (n_Molecule_Sets) transmittance profile array:
!   ---------------------------------------------------------------------------
!
!     Error_Status = Write_TauProfile_netCDF( NC_Filename, &  ! Input
!                                             Tau        , &  ! Input, Rank-5 K x L x I x M x J
!                                             Quiet      =Quiet      , &  ! Optional input
!                                             RCS_Id     =RCS_Id     , &  ! Revision control
!                                             Message_Log=Message_Log  )  ! Error messaging
!
!
!   To write the entire transmittance array in the TauProfile structure:
!   --------------------------------------------------------------------
!
!     Error_Status = Write_TauProfile_netCDF( NC_Filename, &  ! Input
!                                             TauProfile , &  ! Input, TYPE( TauProfile_type)
!                                             Quiet      =Quiet      , &  ! Optional input
!                                             RCS_Id     =RCS_Id     , &  ! Revision control
!                                             Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:     Character string specifying the name of the netCDF
!                        format TauProfile data file to write data into.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Tau:             Transmittance data array. The dimensions of the
!                        specified transmittance array must match those
!                        defined in the netCDF data file.
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1, 2, 3, 4, or 5 array
!                        ATTRIBUTES: INTENT(IN)
!         OR
!       TauProfile:      Structure containing the transmittance data to write to file.
!                        UNITS:      N/A
!                        TYPE:       TauProfile_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Channel:         Channel number for the input transmittance Profile.
!                        ONLY TO BE USED WITH RANK-1 TAU ARRAY INPUT.
!                        UNITS:      N/A
!                        TYPE:       Integer
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Angle:           Angle value for the input transmittance Profiles.
!                        ONLY TO BE USED WITH RANK-1 AND 2 TAU ARRAY INPUT.
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Profile:         Profile number for the input transmittance Profiles.
!                        ONLY TO BE USED WITH RANK-1, 2, AND 3 TAU ARRAY
!                        INPUT.
!                        UNITS:      N/A
!                        TYPE:       Integer
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Molecule_Set:    Molecule set ID number for the input transmittance
!                        Profiles. ONLY TO BE USED WITH RANK-1, 2, 3, AND 4
!                        TAU ARRAY INPUT.
!                        UNITS:      N/A
!                        TYPE:       Integer
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:           Set this keyword to suppress information messages being
!                        printed to standard output (or the message log file if
!                        the MESSAGE_LOG optional argument is used.) By default,
!                        information messages are printed.
!                        If QUIET = 0, information messages are OUTPUT.
!                           QUIET = 1, information messages are SUPPRESSED.
!                        UNITS:      N/A
!                        TYPE:       Integer
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:          Character string containing the Revision Control
!                        System Id field for the module.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT(OUT)
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
!------------------------------------------------------------------------------

  FUNCTION Write_TauArray_rank1( NC_Filename , &  ! Input
                                 Tau         , &  ! Input
                                 Channel     , &  ! Input
                                 Angle       , &  ! Input
                                 Profile     , &  ! Input
                                 Molecule_Set, &  ! Input
                                 Geometric_Angle, & ! Optional input
                                 Quiet       , &  ! Optional input
                                 RCS_Id      , &  ! Revision control
                                 Message_Log ) &  ! Error messaging
                               RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    REAL(fp)    ,           INTENT(IN)  :: Tau(:)
    INTEGER     ,           INTENT(IN)  :: Channel
    REAL(fp)    ,           INTENT(IN)  :: Angle
    INTEGER     ,           INTENT(IN)  :: Profile
    INTEGER     ,           INTENT(IN)  :: Molecule_Set
    REAL(fp)    , OPTIONAL, INTENT(IN)  :: Geometric_Angle(:)
    INTEGER     , OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_TauProfile_netCDF(rank1)'
    ! Local variables
    CHARACTER(1000) :: Message
    LOGICAL :: Noisy
    INTEGER :: NC_FileID
    INTEGER :: Allocate_Status
    INTEGER :: Close_Status
    INTEGER , ALLOCATABLE :: Channel_List(:)
    REAL(fp), ALLOCATABLE :: Angle_List(:)
    INTEGER , ALLOCATABLE :: Profile_List(:)
    INTEGER , ALLOCATABLE :: Molecule_Set_List(:)
    INTEGER :: Channel_Index
    INTEGER :: Angle_Index
    INTEGER :: Profile_Index
    INTEGER :: Molecule_Set_Index
    INTEGER :: k, l, i, m, j

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF


    ! Read the dimension values
    ! -------------------------
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              n_Layers       =k, &
                                              n_Channels     =l, &
                                              n_Angles       =i, &
                                              n_Profiles     =m, &
                                              n_Molecule_Sets=j, &
                                              Message_Log    =Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining TauProfile dimensions from '//&
                TRIM(NC_Filename)
      GOTO 3000
    END IF

    ! Check the TauProfile dimension values
    IF ( SIZE(Tau, DIM=1) /= k ) THEN
      Message = 'Tau N_LAYERS array size different from netCDF definition.'
      GOTO 3000
    END IF


    ! Allocate the index list arrays
    ! ------------------------------
    ALLOCATE( Channel_List(l), Angle_List(i), &
              Profile_List(m), Molecule_Set_List(j), &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '( "Error allocating index list arrays. STAT = ", i0 )' ) &
                      Allocate_Status
      GOTO 3000
    END IF


    ! Fill the index list arrays
    ! --------------------------
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename                        , &
                                              Channel_List     =Channel_List     , &
                                              Angle_List       =Angle_List       , &
                                              Profile_List     =Profile_List     , &
                                              Molecule_Set_List=Molecule_Set_List, &
                                              Message_Log      =Message_Log        )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining index list data from '//&
                TRIM(NC_Filename)
      GOTO 2000
    END IF


    ! Determine the index values for the input Tau data
    ! -------------------------------------------------
    Channel_Index      = Get_Index( Channel_List, Channel )
    Angle_Index        = Get_Index( Angle_List, Angle )
    Profile_Index      = Get_Index( Profile_List, Profile )
    Molecule_Set_Index = Get_Index( Molecule_Set_List, Molecule_Set )


    ! Check the index list values
    ! ---------------------------
    IF ( Channel_Index < 1 ) THEN
      WRITE( Message, '( "Invalid CHANNEL_LIST array index value, ", i0, &
                        &" for channel #", i0 )' ) &
                      Channel_Index, Channel
      GOTO 2000
    END IF
    IF ( Angle_Index < 1 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Invalid ANGLE_LIST array index value, ", i0, &
                        &" for angle ", f5.2 )' ) &
                      Angle_Index, Angle
      GOTO 2000
    END IF
    IF ( Profile_Index < 1 ) THEN
      WRITE( Message, '( "Invalid PROFILE_LIST array index value, ", i0, &
                        &" for molecule set #", i0 )' ) &
                      Profile_Index, Profile
      GOTO 2000
    END IF
    IF ( Molecule_Set_Index < 1 ) THEN
      Molecule_Set_Index = j + 1  ! New molecule set
    END IF


    ! Open the file
    ! -------------
    Error_Status = Open_TauProfile_netCDF( TRIM(NC_Filename), &
                                           NC_FileID, &
                                           Mode = 'READWRITE' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF TauProfile data file '//&
                TRIM(NC_Filename)
      GOTO 2000
    END IF


    ! Write the molecule list data
    ! ----------------------------
    IF ( Molecule_Set_Index > j ) THEN
      Error_Status = Put_netCDF_Variable( NC_FileID, &
                                          MOLECULE_SET_LIST_VARNAME, &
                                          Molecule_Set, &
                                          START=(/Molecule_Set_Index/) )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing molecule set value ", i0, &
                          &" to ", a, "." )' ) &
                        Molecule_Set, &
                        TRIM(NC_Filename)
        GOTO 1000
      END IF
    END IF

    ! Write the Geometric angle data
    ! ----------------------------
    IF ( PRESENT(Geometric_Angle) ) THEN 
       ! Check the TauProfile dimension values
      IF ( SIZE( Geometric_Angle, DIM=1) /= k ) THEN
        Message = 'Geometric_Angle N_LAYERS array size different from netCDF definition.'
        GOTO 3000
      END IF

      Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                          GEOMETRIC_ANGLE_VARNAME, &
                                          Geometric_Angle, &
                                          START=(/1,Angle_Index, &
                                                    Profile_Index/),&
                                          COUNT=(/k,1,1/) )
      IF ( Error_Status/= SUCCESS ) THEN
        WRITE( Message, '( "Error writing Geometric_Angle vector for ", &
                          &", Angle secant ", f5.2, ", and Profile ", i0, &
                          &", to ", a, "." )' ) &
                        Angle, Profile, TRIM(NC_Filename)
        GOTO 1000
      END IF
    
    ENDIF
    ! Write the transmittance data
    ! ----------------------------
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        TRANSMITTANCE_VARNAME, &
                                        Tau, &
                                        START=(/1,Channel_Index, &
                                                  Angle_Index, &
                                                  Profile_Index,&
                                                  Molecule_Set_Index/), &
                                        COUNT=(/k,1,1,1,1/) )
    IF ( Error_Status/= SUCCESS ) THEN
      WRITE( Message, '( "Error writing Tau vector for Channel ", i0, &
                        &", Angle secant ", f5.2, ", Profile ", i0, &
                        &", and molecule set ", i0, " to ", a, "." )' ) &
                      Channel, Angle, Profile, Molecule_Set, &
                      TRIM(NC_Filename)
      GOTO 1000
    END IF


    ! Deallocate the list arrays
    ! --------------------------
    DEALLOCATE(Channel_List, Angle_List, Profile_List, Molecule_Set_List, &
               STAT=Allocate_Status)
    IF ( Allocate_Status /= 0 ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating list arrays.', &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! Close the file
    ! --------------
    Close_Status = Close_TauProfile_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauProfile data file '// &
                            TRIM(NC_Filename), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      WRITE(Message,'("Tau: ", &
                     &"N_LAYERS=",i0,2x,&
                     &"for channel ",i0,&
                     &", sec(angle) ",f4.2,&
                     &", profile ",i0,&
                     &" and molecule set ",i0 )' ) &
                     k,Channel,Angle,Profile,Molecule_Set
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(NC_Filename)//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

    !=====
    RETURN
    !=====
    
    ! Clean up after an error
    ! -----------------------
    1000 CONTINUE
    Close_Status = Close_TauProfile_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing output file during error cleanup.'
    2000 CONTINUE
    DEALLOCATE(Channel_List, Angle_List, Profile_List, Molecule_Set_List, &
               STAT=Allocate_Status)
    IF ( Allocate_Status /= 0 ) &
      Message = TRIM(Message)//&
                '; Error deallocating list data arrays during error cleanup.'
    3000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Write_TauArray_rank1


  FUNCTION Write_TauArray_rank2( NC_Filename , &  ! Input
                                 Tau         , &  ! Input
                                 Angle       , &  ! Input
                                 Profile     , &  ! Input
                                 Molecule_Set, &  ! Input
                                 Quiet       , &  ! Optional input
                                 RCS_Id      , &  ! Revision control
                                 Message_Log ) &  ! Error messaging
                               RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    REAL(fp)    ,           INTENT(IN)  :: Tau(:,:)
    REAL(fp)    ,           INTENT(IN)  :: Angle
    INTEGER     ,           INTENT(IN)  :: Profile
    INTEGER     ,           INTENT(IN)  :: Molecule_Set
    INTEGER     , OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_TauProfile_netCDF(rank2)'
    ! Local variables
    CHARACTER(1000) :: Message
    LOGICAL :: Noisy
    INTEGER :: NC_FileID
    INTEGER :: Allocate_Status
    INTEGER :: Close_Status
    REAL(fp), ALLOCATABLE :: Angle_List(:)
    INTEGER , ALLOCATABLE :: Profile_List(:)
    INTEGER , ALLOCATABLE :: Molecule_Set_List(:)
    INTEGER :: Angle_Index
    INTEGER :: Profile_Index
    INTEGER :: Molecule_Set_Index
    INTEGER :: k, l, i, m, j

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF


    ! Read the dimension values
    ! -------------------------
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              n_Layers       =k, &
                                              n_Channels     =l, &
                                              n_Angles       =i, &
                                              n_Profiles     =m, &
                                              n_Molecule_Sets=j, &
                                              Message_Log    =Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining TauProfile dimensions from '//&
                TRIM(NC_Filename)
      GOTO 3000
    END IF

    ! Check the TauProfile dimension values
    IF ( SIZE(Tau, DIM=1) /= k .OR. &
         SIZE(Tau, DIM=2) /= l      ) THEN
      Message = 'Tau N_LAYERS x N_CHANNELS '//&
                'array size different from netCDF definition.'
      GOTO 3000
    END IF


    ! Allocate the index list arrays
    ! ------------------------------
    ALLOCATE( Angle_List( i ), &
              Profile_List( m ), &
              Molecule_Set_List( j ), &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '( "Error allocating index list arrays. STAT = ", i0 )' ) &
                      Allocate_Status
      GOTO 3000
    END IF


    ! Fill the index list arrays
    ! --------------------------
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              Angle_List       =Angle_List       , &
                                              Profile_List     =Profile_List     , &
                                              Molecule_Set_List=Molecule_Set_List, &
                                              Message_Log      =Message_Log        )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining index list data from '//&
                TRIM(NC_Filename)
      GOTO 2000
    END IF


    ! Determine the index values for the input Tau data
    ! -------------------------------------------------
    Angle_Index        = Get_Index( Angle_List, Angle )
    Profile_Index      = Get_Index( Profile_List, Profile )
    Molecule_Set_Index = Get_Index( Molecule_Set_List, Molecule_Set )


    ! Check the index list values
    ! ---------------------------
    IF ( Angle_Index < 1 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Invalid ANGLE_LIST array index value, ", i0, &
                        &" for angle ", f5.2 )' ) &
                      Angle_Index, Angle
      GOTO 2000
    END IF
    IF ( Profile_Index < 1 ) THEN
      WRITE( Message, '( "Invalid PROFILE_LIST array index value, ", i0, &
                        &" for molecule set #", i0 )' ) &
                      Profile_Index, Profile
      GOTO 2000
    END IF
    IF ( Molecule_Set_Index < 1 ) THEN
      Molecule_Set_Index = j + 1  ! New molecule set
    END IF


    ! Open the file
    ! -------------
    Error_Status = Open_TauProfile_netCDF( TRIM(NC_Filename), &
                                           NC_FileID, &
                                           Mode = 'READWRITE' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF TauProfile data file '//&
                TRIM(NC_Filename)
      GOTO 2000
    END IF


    ! Write the molecule list data
    ! ----------------------------
    IF ( Molecule_Set_Index > j ) THEN
      Error_Status = Put_netCDF_Variable( NC_FileID, &
                                          MOLECULE_SET_LIST_VARNAME, &
                                          Molecule_Set, &
                                          START=(/Molecule_Set_Index/) )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing molecule set value ", i0, &
                          &" to ", a, "." )' ) &
                        Molecule_Set, &
                        TRIM(NC_Filename)
        GOTO 1000
      END IF
    END IF
 
    ! Write the transmittance data
    ! ----------------------------
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        TRANSMITTANCE_VARNAME, &
                                        Tau, &
                                        START=(/1,1,Angle_Index, &
                                                    Profile_Index,&
                                                    Molecule_Set_Index/), &
                                        COUNT=(/k,l,1,1,1/) )
    IF ( Error_Status/= SUCCESS ) THEN
      WRITE( Message, '( "Error writing Tau array for angle secant ", f5.2, &
                        &", profile ", i0, ", and molecule set ", i0, " to ", a, "." )' ) &
                      Angle, Profile, Molecule_Set, &
                      TRIM(NC_Filename)
      GOTO 1000
    END IF


    ! Deallocate the list arrays
    ! --------------------------
    DEALLOCATE(Angle_List, Profile_List, Molecule_Set_List, STAT=Allocate_Status)
    IF ( Allocate_Status /= 0 ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating list arrays.', &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! Close the file
    ! --------------
    Close_Status = Close_TauProfile_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauProfile data file '// &
                            TRIM(NC_Filename), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      WRITE(Message,'("Tau: ", &
                     &"N_LAYERS=",i0,2x,&
                     &"N_CHANNELS=",i0,2x,&
                     &"for sec(angle) ",f4.2,&
                     &", profile ",i0," and molecule set ",i0 )' ) &
                     k,l,Angle,Profile,Molecule_Set
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(NC_Filename)//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

    !=====
    RETURN
    !=====
    
    ! Clean up after an error
    ! -----------------------
    1000 CONTINUE
    Close_Status = Close_TauProfile_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing output file during error cleanup.'
    2000 CONTINUE
    DEALLOCATE(Angle_List, Profile_List, Molecule_Set_List, &
               STAT=Allocate_Status)
    IF ( Allocate_Status /= 0 ) &
      Message = TRIM(Message)//&
                '; Error deallocating list data arrays during error cleanup.'
    3000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Write_TauArray_rank2


  FUNCTION Write_TauArray_rank3( NC_Filename , &  ! Input
                                 Tau         , &  ! Input
                                 Profile     , &  ! Input
                                 Molecule_Set, &  ! Input
                                 Geometric_Angle, & ! Optional input
                                 Quiet       , &  ! Optional input
                                 RCS_Id      , &  ! Revision control
                                 Message_Log ) &  ! Error messaging
                               RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    REAL(fp)    ,           INTENT(IN)  :: Tau(:,:,:)
    INTEGER     ,           INTENT(IN)  :: Profile
    INTEGER     ,           INTENT(IN)  :: Molecule_Set
    REAL(fp)    , OPTIONAL, INTENT(IN)  :: Geometric_Angle(:,:)
    INTEGER     , OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_TauProfile_netCDF(rank3)'
    ! Local variables
    CHARACTER(1000) :: Message
    LOGICAL :: Noisy
    INTEGER :: NC_FileID
    INTEGER :: Allocate_Status
    INTEGER :: Close_Status
    INTEGER, ALLOCATABLE :: Profile_List(:)
    INTEGER, ALLOCATABLE :: Molecule_Set_List(:)
    INTEGER :: Profile_Index
    INTEGER :: Molecule_Set_Index
    INTEGER :: k, l, i, m, j

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF


    ! Read the dimension values
    ! -------------------------
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              n_Layers       =k, &
                                              n_Channels     =l, &
                                              n_Angles       =i, &
                                              n_Profiles     =m, &
                                              n_Molecule_Sets=j, &
                                              Message_Log    =Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining TauProfile dimensions from '//&
                TRIM(NC_Filename)
      GOTO 3000
    END IF

    ! Check the TauProfile dimension values
    IF ( SIZE(Tau, DIM=1) /= k .OR. &
         SIZE(Tau, DIM=2) /= l .OR. &
         SIZE(Tau, DIM=3) /= i      ) THEN
      Message = 'Tau N_LAYERS x N_CHANNELS x N_ANGLES '//&
                'array size different from netCDF definition.'
      GOTO 3000
    END IF


    ! Allocate the index list arrays
    ! ------------------------------
    ALLOCATE( Profile_List( m ), &
              Molecule_Set_List( j ), &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '( "Error allocating index list arrays. STAT = ", i0 )' ) &
                      Allocate_Status
      GOTO 3000
    END IF


    ! Fill the index list arrays
    ! --------------------------
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename                        , &
                                              Profile_List     =Profile_List     , &
                                              Molecule_Set_List=Molecule_Set_List, &
                                              Message_Log      =Message_Log        )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining index list data from '//&
                TRIM(NC_Filename)
      GOTO 2000
    END IF


    ! Determine the index values for the input Tau data
    ! -------------------------------------------------
    Profile_Index      = Get_Index( Profile_List, Profile )
    Molecule_Set_Index = Get_Index( Molecule_Set_List, Molecule_Set )


    ! Check the index list values
    ! ---------------------------
    IF ( Profile_Index < 1 ) THEN
      WRITE( Message, '( "Invalid PROFILE_LIST array index value, ", i0, &
                        &" for molecule set #", i0 )' ) &
                      Profile_Index, Profile
      GOTO 2000
    END IF
    IF ( Molecule_Set_Index < 1 ) THEN
      Molecule_Set_Index = j + 1  ! New molecule set
    END IF


    ! Open the file
    ! -------------
    Error_Status = Open_TauProfile_netCDF( TRIM(NC_Filename), &
                                           NC_FileID, &
                                           Mode = 'READWRITE' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF TauProfile data file '//&
                TRIM(NC_Filename)
      GOTO 2000
    END IF


    ! Write the molecule list data
    ! ----------------------------
    IF ( Molecule_Set_Index > j ) THEN
      Error_Status = Put_netCDF_Variable( NC_FileID, &
                                          MOLECULE_SET_LIST_VARNAME, &
                                          Molecule_Set, &
                                          START=(/Molecule_Set_Index/) )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing molecule set value ", i0, &
                          &" to ", a, "." )' ) &
                        Molecule_Set, &
                        TRIM(NC_Filename)
        GOTO 1000
      END IF
    END IF

    ! Write the Geometric angle data
    ! ----------------------------
    IF ( PRESENT(Geometric_Angle) ) THEN 
       ! Check the TauProfile dimension values
      IF ( SIZE( Geometric_Angle, DIM=1) /= k .OR. &
           SIZE( Geometric_Angle, DIM=2) /= i      ) THEN 
        Message = 'Geometric_Angle N_LAYERS X N_ANGLES array size different from netCDF definition.'
        GOTO 3000
      END IF

      Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                          GEOMETRIC_ANGLE_VARNAME, &
                                          Geometric_Angle, &
                                          START=(/1,1,Profile_Index/),&
                                          COUNT=(/k,i,1/) )
      IF ( Error_Status/= SUCCESS ) THEN
        WRITE( Message, '( "Error writing Geometric_Angle vector for ", &
                          &" Profile ", i0, &
                          &", to ", a, "." )' ) &
                          Profile, TRIM(NC_Filename)
        GOTO 1000
      END IF
    
    ENDIF

    ! Write the transmittance data
    ! ----------------------------
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        TRANSMITTANCE_VARNAME, &
                                        Tau, &
                                        START=(/1,1,1,Profile_Index,&
                                                      Molecule_Set_Index/), &
                                        COUNT=(/k,l,i,1,1/) )
    IF ( Error_Status/= SUCCESS ) THEN
      WRITE( Message, '( "Error reading Tau array for profile ", i0, &
                        &", and molecule set ", i0, " from ", a, "." )' ) &
                      Profile, Molecule_Set, &
                      TRIM(NC_Filename)
      GOTO 1000
    END IF


    ! Deallocate the list arrays
    ! --------------------------
    DEALLOCATE(Profile_List, Molecule_Set_List, STAT=Allocate_Status)
    IF ( Allocate_Status /= 0 ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating list arrays.', &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! Close the file
    ! --------------
    Close_Status = Close_TauProfile_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauProfile data file '// &
                            TRIM(NC_Filename), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      WRITE(Message,'("Tau: ", &
                     &"N_LAYERS=",i0,2x,&
                     &"N_CHANNELS=",i0,2x,&
                     &"N_ANGLES=",i0,2x,&
                     &"for profile ",i0," and molecule set ",i0 )' ) &
                     k,l,i,Profile,Molecule_Set
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(NC_Filename)//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

    !=====
    RETURN
    !=====
    
    ! Clean up after an error
    ! -----------------------
    1000 CONTINUE
    Close_Status = Close_TauProfile_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing output file during error cleanup.'
    2000 CONTINUE
    DEALLOCATE(Profile_List, Molecule_Set_List, &
               STAT=Allocate_Status)
    IF ( Allocate_Status /= 0 ) &
      Message = TRIM(Message)//&
                '; Error deallocating list data arrays during error cleanup.'
    3000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Write_TauArray_rank3


  FUNCTION Write_TauArray_rank4( NC_Filename , &  ! Input
                                 Tau         , &  ! Input
                                 Molecule_Set, &  ! Input
                                 Geometric_Angle, & ! Optional input
                                 Quiet       , &  ! Optional input
                                 RCS_Id      , &  ! Revision control
                                 Message_Log ) &  ! Error messaging
                               RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    REAL(fp)    ,           INTENT(IN)  :: Tau(:,:,:,:)
    INTEGER     ,           INTENT(IN)  :: Molecule_Set
    INTEGER     , OPTIONAL, INTENT(IN)  :: Quiet
    REAL(fp)    , OPTIONAL, INTENT(IN)  :: Geometric_Angle(:,:,:)
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_TauProfile_netCDF(rank4)'
    ! Local variables
    CHARACTER(1000) :: Message
    LOGICAL :: Noisy
    INTEGER :: NC_FileID
    INTEGER :: Allocate_Status
    INTEGER :: Close_Status
    INTEGER, ALLOCATABLE :: Molecule_Set_List(:)
    INTEGER :: Molecule_Set_Index
    INTEGER :: k, l, i, m, j

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF


    ! Read the dimension values
    ! -------------------------
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              n_Layers       =k, &
                                              n_Channels     =l, &
                                              n_Angles       =i, &
                                              n_Profiles     =m, &
                                              n_Molecule_Sets=j, &
                                              Message_Log    =Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining TauProfile dimensions from '//&
                TRIM(NC_Filename)
      GOTO 3000
    END IF

    ! Check the TauProfile dimension values
    IF ( SIZE(Tau, DIM=1) /= k .OR. &
         SIZE(Tau, DIM=2) /= l .OR. &
         SIZE(Tau, DIM=3) /= i .OR. &
         SIZE(Tau, DIM=4) /= m      ) THEN
      Message = 'Tau N_LAYERS x N_CHANNELS x N_ANGLES x N_PROFILES '//&
                'array size different from netCDF definition.'
      GOTO 3000
    END IF


    ! Allocate the index list arrays
    ! ------------------------------
    ALLOCATE( Molecule_Set_List( j ), &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '( "Error allocating index list arrays. STAT = ", i0 )' ) &
                      Allocate_Status
      GOTO 3000
    END IF


    ! Fill the index list arrays
    ! --------------------------
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              Molecule_Set_List=Molecule_Set_List, &
                                              Message_Log      =Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining index list data from '//&
                TRIM(NC_Filename)
      GOTO 2000
    END IF


    ! Determine the index values for the input Tau data
    ! -------------------------------------------------
    Molecule_Set_Index = Get_Index( Molecule_Set_List, Molecule_Set )


    ! Check the index list value
    ! --------------------------
    IF ( Molecule_Set_Index < 1 ) THEN
      Molecule_Set_Index = j + 1  ! New molecule set
    END IF


    ! Open the file
    ! -------------
    Error_Status = Open_TauProfile_netCDF( TRIM(NC_Filename), &
                                           NC_FileID, &
                                           Mode = 'READWRITE' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF TauProfile data file '//&
                TRIM(NC_Filename)
      GOTO 2000
    END IF


    ! Write the molecule list data
    ! ----------------------------
    IF ( Molecule_Set_Index > j ) THEN
      Error_Status = Put_netCDF_Variable( NC_FileID, &
                                          MOLECULE_SET_LIST_VARNAME, &
                                          Molecule_Set, &
                                          START=(/Molecule_Set_Index/) )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing molecule set value ", i0, &
                          &" to ", a, "." )' ) &
                        Molecule_Set, &
                        TRIM(NC_Filename)
        GOTO 1000
      END IF
    END IF

    ! Write the Geometric angle data
    ! ----------------------------
    IF ( PRESENT(Geometric_Angle) ) THEN 
       ! Check the TauProfile dimension values
      IF ( SIZE( Geometric_Angle, DIM=1) /= k .OR. &
           SIZE( Geometric_Angle, DIM=2) /= i .OR. &
           SIZE( Geometric_Angle, DIM=3) /= m      ) THEN 
        Message = 'Geometric_Angle N_LAYERS x N_ANGLES x N_PROFILES array size different from netCDF definition.'
        GOTO 3000
      END IF

      Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                          GEOMETRIC_ANGLE_VARNAME, &
                                          Geometric_Angle, &
                                          START=(/1,1,1/),&
                                          COUNT=(/k,i,m/) )
      IF ( Error_Status/= SUCCESS ) THEN
        WRITE( Message, '( "Error writing Geometric_Angle vector for to ", a, "." )' ) &
                          TRIM(NC_Filename)
        GOTO 1000
      END IF
    
    ENDIF

    ! Write the transmittance data
    ! ----------------------------
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        TRANSMITTANCE_VARNAME, &
                                        Tau, &
                                        START=(/1,1,1,1,Molecule_Set_Index/), &
                                        COUNT=(/k,l,i,m,1/) )
    IF ( Error_Status/= SUCCESS ) THEN
      WRITE( Message, '( "Error writing Tau array for molecule set ", i0, &
                        &" to ", a, "." )' ) &
                      Molecule_Set, &
                      TRIM(NC_Filename)
      GOTO 1000
    END IF


    ! Deallocate the list array
    ! -------------------------
    DEALLOCATE(Molecule_Set_List, STAT=Allocate_Status)
    IF ( Allocate_Status /= 0 ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating list arrays.', &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! Close the file
    ! --------------
    Close_Status = Close_TauProfile_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauProfile data file '// &
                            TRIM(NC_Filename), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      WRITE(Message,'("Tau: ", &
                     &"N_LAYERS=",i0,2x,&
                     &"N_CHANNELS=",i0,2x,&
                     &"N_ANGLES=",i0,2x,&
                     &"N_PROFILES=",i0,2x,&
                     &"for molecule set ",i0 )' ) k,l,i,m,Molecule_Set
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(NC_Filename)//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

    !=====
    RETURN
    !=====
    
    ! Clean up after an error
    ! -----------------------
    1000 CONTINUE
    Close_Status = Close_TauProfile_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing output file during error cleanup.'
    2000 CONTINUE
    DEALLOCATE(Molecule_Set_List, STAT=Allocate_Status)
    IF ( Allocate_Status /= 0 ) &
      Message = TRIM(Message)//&
                '; Error deallocating list data arrays during error cleanup.'
    3000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Write_TauArray_rank4


  FUNCTION Write_TauArray_rank5( NC_Filename , &  ! Input
                                 Tau         , &  ! Input
                                 Quiet       , &  ! Optional input
                                 RCS_Id      , &  ! Revision control
                                 Message_Log ) &  ! Error messaging
                               RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    REAL(fp)    ,           INTENT(IN)  :: Tau(:,:,:,:,:)
    INTEGER     , OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_TauProfile_netCDF(rank5)'
    ! Local variables
    CHARACTER(1000) :: Message
    LOGICAL :: Noisy
    INTEGER :: NC_FileID
    INTEGER :: Close_Status
    INTEGER :: k, l, i, m, j

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF


    ! Read the dimension values
    ! -------------------------
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              n_Layers       =k, &
                                              n_Channels     =l, &
                                              n_Angles       =i, &
                                              n_Profiles     =m, &
                                              n_Molecule_Sets=j, &
                                              Message_Log    =Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining TauProfile dimensions from '//&
                TRIM(NC_Filename)
      GOTO 2000
    END IF

    ! Check the TauProfile dimension values
    IF ( SIZE(Tau, DIM=1) /= k .OR. &
         SIZE(Tau, DIM=2) /= l .OR. &
         SIZE(Tau, DIM=3) /= i .OR. &
         SIZE(Tau, DIM=4) /= m .OR. &
         SIZE(Tau, DIM=5) /= j      ) THEN
      Message = 'Tau N_LAYERS x N_CHANNELS x N_ANGLES x N_PROFILES '//&
                'x N_MOLCULE_SETS array size different from netCDF definition.'
      GOTO 2000
    END IF


    ! Open the file
    ! -------------
    Error_Status = Open_TauProfile_netCDF( TRIM(NC_Filename), &
                                           NC_FileID, &
                                           Mode = 'READWRITE' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF TauProfile data file '//&
                TRIM(NC_Filename)
      GOTO 2000
    END IF


    ! Write the transmittance data
    ! ----------------------------
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        TRANSMITTANCE_VARNAME, &
                                        Tau )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error writing Tau array to ", a, "." )' ) &
                      TRIM(NC_Filename)
      GOTO 1000
    END IF


    ! Close the file
    ! --------------
    Close_Status = Close_TauProfile_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauProfile data file '// &
                            TRIM(NC_Filename), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      WRITE(Message,'("Tau: ", &
                     &"N_LAYERS=",i0,2x,&
                     &"N_CHANNELS=",i0,2x,&
                     &"N_ANGLES=",i0,2x,&
                     &"N_PROFILES=",i0,2x,&
                     &"N_MOLECULE_SETS=",i0 )' ) k,l,i,m,j
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(NC_Filename)//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

    !=====
    RETURN
    !=====
    
    ! Clean up after an error
    ! -----------------------
    1000 CONTINUE
    Close_Status = Close_TauProfile_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing output file during error cleanup.'
    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Write_TauArray_rank5


  FUNCTION Write_TauProfile_type( NC_Filename , &  ! Input
                                  TauProfile  , &  ! Input
                                  Profile_Angle, &   ! Optional input
                                  Quiet       , &  ! Optional input
                                  RCS_Id      , &  ! Revision control
                                  Message_Log ) &  ! Error messaging
                                RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    TYPE(TauProfile_type) , INTENT(IN)  :: TauProfile
    INTEGER     , OPTIONAL, INTENT(IN)     :: Profile_Angle
    INTEGER     , OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_TauProfile_netCDF(structure)'
    ! Local variables
    CHARACTER(1000) :: Message
    LOGICAL :: Noisy
    LOGICAL :: Write_Profile_Angle
    INTEGER :: NC_FileID
    INTEGER :: Close_Status
    INTEGER :: k, l, i, m, j

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF

    Write_Profile_Angle = .FALSE.
    ! ....unless the Profile_Angle keyword is set.
    IF ( PRESENT( Profile_Angle ) ) THEN
      IF ( Profile_Angle == 1 ) Write_Profile_Angle = .TRUE.
    END IF

    ! Check structure association
    IF ( .NOT. Associated_TauProfile( TauProfile ) ) THEN
      Message = 'Some or all INPUT TauProfile pointer members are NOT associated.'
      GOTO 2000
    END IF


    ! Read the dimension values
    ! -------------------------
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              n_Layers       =k, &
                                              n_Channels     =l, &
                                              n_Angles       =i, &
                                              n_Profiles     =m, &
                                              n_Molecule_Sets=j, &
                                              Message_Log    =Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining TauProfile dimensions from '//TRIM(NC_Filename)
      GOTO 2000
    END IF

    ! Check the dimension values with the TauProfile structure
    IF ( TauProfile%n_Layers        /= k .OR. &
         TauProfile%n_Channels      /= l .OR. &
         TauProfile%n_Angles        /= i .OR. &
         TauProfile%n_Profiles      /= m .OR. &
         TauProfile%n_Molecule_Sets /= j      ) THEN
      Message = 'TauProfile dimensions different from netCDF definitions.'
      GOTO 2000
    END IF


    ! Open the file
    ! -------------
    Error_Status = Open_TauProfile_netCDF( TRIM(NC_Filename), &
                                           NC_FileID, &
                                           Mode = 'READWRITE' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF TauProfile data file '//TRIM(NC_Filename)
      GOTO 2000
    END IF

    ! Write the Geometric angle data
    ! ----------------------------
    IF ( Write_Profile_Angle ) THEN
      Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                          GEOMETRIC_ANGLE_VARNAME, &
                                          TauProfile%Geometric_Angle)
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing Geometric_Angle array to ", a, "." )' ) &
                        TRIM(NC_Filename)
        GOTO 1000
      END IF
    END IF

    ! Write the transmittance data
    ! ----------------------------
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        TRANSMITTANCE_VARNAME, &
                                        TauProfile%Tau )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error writing Tau array to ", a, "." )' ) &
                      TRIM(NC_Filename)
      GOTO 1000
    END IF


    ! Close the file
    ! --------------
    Close_Status = Close_TauProfile_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauProfile data file '// &
                            TRIM(NC_Filename), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_TauProfile( TauProfile, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(NC_Filename)//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

    !=====
    RETURN
    !=====

    ! Clean up after an error
    1000 CONTINUE
    Close_Status = Close_TauProfile_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing output file during error cleanup.'
    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Write_TauProfile_type


  FUNCTION Write_GeometricAngle_netCDF( NC_Filename , &  ! Input
                                  Geometric_Angle   , &  ! Input
                                  Quiet             , &  ! Optional input
                                  RCS_Id            , &  ! Revision control
                                  Message_Log )       &  ! Error messaging
                                RESULT ( Error_Status )

    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    REAL(fp)    ,           INTENT(IN)  :: Geometric_Angle(:,:,:)
    INTEGER     , OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_GeometricAngle_netCDF'
    ! Local variables
    CHARACTER(1000) :: Message
    LOGICAL :: Noisy
    INTEGER :: NC_FileID
    INTEGER :: Close_Status
    INTEGER :: k, i, m

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF


    ! Read the dimension values
    ! -------------------------
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              n_Layers       =k, &
                                              n_Angles       =i, &
                                              n_Profiles     =m, &
                                              Message_Log    =Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining TauProfile dimensions from '//&
                TRIM(NC_Filename)
      GOTO 2000
    END IF

    ! Check the TauProfile dimension values
    IF ( SIZE(Geometric_Angle, DIM=1) /= k .OR. &
         SIZE(Geometric_Angle, DIM=2) /= i .OR. &
         SIZE(Geometric_Angle, DIM=3) /= m      ) THEN
      Message = 'Geometric_Angle N_LAYERS x N_ANGLES x N_PROFILES '//&
                'array size different from netCDF definition.'
      GOTO 2000
    END IF


    ! Open the file
    ! -------------
    Error_Status = Open_TauProfile_netCDF( TRIM(NC_Filename), &
                                           NC_FileID, &
                                           Mode = 'READWRITE' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF TauProfile data file '//&
                TRIM(NC_Filename)
      GOTO 2000
    END IF


    ! Write the transmittance data
    ! ----------------------------
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        GEOMETRIC_ANGLE_VARNAME, &
                                        Geometric_Angle )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error writing  Geometric_Angle array to ", a, "." )' ) &
                      TRIM(NC_Filename)
      GOTO 1000
    END IF


    ! Close the file
    ! --------------
    Close_Status = Close_TauProfile_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauProfile data file '// &
                            TRIM(NC_Filename), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      WRITE(Message,'("Geometric_Angle: ", &
                     &"N_LAYERS=",i0,2x,&
                     &"N_ANGLES=",i0,2x,&
                     &"N_PROFILES=",i0 )' ) k,i,m
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(NC_Filename)//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

    !=====
    RETURN
    !=====
    
    ! Clean up after an error
    ! -----------------------
    1000 CONTINUE
    Close_Status = Close_TauProfile_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing output file during error cleanup.'
    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )
 
  END FUNCTION Write_GeometricAngle_netCDF 

  FUNCTION Read_GeometricAngle_netCDF( NC_Filename, &  ! Input
                                Geometric_Angle   , &  ! Output
                                Quiet             , &  ! Optional input
                                RCS_Id            , &  ! Revision control
                                Message_Log)        &  ! Error messaging
                              RESULT ( Error_Status )
    ! Arguments               
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    REAL(fp)    ,           INTENT(OUT) :: Geometric_Angle(:,:,:)
    INTEGER     , OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_GeometricAngle_netCDF'
    ! Local variables
    CHARACTER(1000) :: Message
    LOGICAL :: Noisy
    INTEGER :: NC_FileID
    INTEGER :: Close_Status
    INTEGER :: k, i, m

    ! Set up
    ! ------ 
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF


    ! Read the dimension values
    ! -------------------------
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              n_Layers       =k, &
                                              n_Angles       =i, &
                                              n_Profiles     =m, &
                                              Message_Log    =Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining TauProfile dimensions from '//&
                TRIM(NC_Filename)
      GOTO 2000
    END IF

    ! Check the TauProfile dimension values
    IF ( SIZE(Geometric_Angle, DIM=1) /= k .OR. &
         SIZE(Geometric_Angle, DIM=2) /= i .OR. &
         SIZE(Geometric_Angle, DIM=3) /= m      ) THEN
      Message = 'Geometric_Angle N_LAYERS x N_ANGLES x N_PROFILES '//&
                'array size different from netCDF definition.'
      GOTO 2000
    END IF
 

    ! Open the file
    ! -------------            
    Error_Status = Open_TauProfile_netCDF( TRIM(NC_Filename), &
                                           NC_FileID, &
                                           Mode = 'READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF TauProfile data file '//&
                TRIM(NC_Filename)
      GOTO 2000
    END IF


    ! Read the transmittances
    ! -----------------------  
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        GEOMETRIC_ANGLE_VARNAME, &
                                        Geometric_Angle )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error writing  Geometric_Angle array to ", a, "." )' ) &
                      TRIM(NC_Filename)
      GOTO 1000
    END IF
 

    ! Close the file
    ! --------------           
    Close_Status = Close_TauProfile_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauProfile data file '// &
                            TRIM(NC_Filename), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      WRITE(Message,'("Geometric_Angle: ", &
                     &"N_LAYERS=",i0,2x,&
                     &"N_ANGLES=",i0,2x,&
                     &"N_PROFILES=",i0 )' ) k,i,m
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(NC_Filename)//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF
 
    !=====
    RETURN
    !=====

    ! Clean up after an error
    1000 CONTINUE
    Close_Status = Close_TauProfile_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing input file during error cleanup.'
    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Read_GeometricAngle_netCDF

!------------------------------------------------------------------------------
!
! NAME:
!       Read_TauProfile_netCDF
!
! PURPOSE:
!       Function to read data from a netCDF format TauProfile file.
!
! CALLING SEQUENCE:
!
!   To read a K x 1 transmittance profile vector:
!   ---------------------------------------------
!
!     Error_Status = Read_TauProfile_netCDF( NC_Filename,  &  ! Input
!                                            Channel,      &  ! Input, L
!                                            Angle,        &  ! Input, I
!                                            Profile,      &  ! Input, M
!                                            Molecule_Set, &  ! Input, J
!                                            Tau,          &  ! Output, Rank-1 K
!                                            Quiet       = Quiet,      &  ! Optional input
!                                            RCS_Id      = RCS_Id,     &  ! Revision control
!                                            Message_Log = Message_Log )  ! Error messaging
!
!
!   To read a K x L (n_Channels) transmittance profile array:
!   ---------------------------------------------------------
!
!     Error_Status = Read_TauProfile_netCDF( NC_Filename,  &  ! Input
!                                            Angle,        &  ! Input, I
!                                            Profile,      &  ! Input, M
!                                            Molecule_Set, &  ! Input, J
!                                            Tau,          &  ! Output, Rank-2 K x L
!                                            Quiet       = Quiet,      &  ! Optional input
!                                            RCS_Id      = RCS_Id,     &  ! Revision control
!                                            Message_Log = Message_Log )  ! Error messaging
!
!
!   To read a K x L x I (n_Angles) transmittance profile array:
!   -----------------------------------------------------------
!
!     Error_Status = Read_TauProfile_netCDF( NC_Filename,  &  ! Input
!                                            Profile,      &  ! Input, M
!                                            Molecule_Set, &  ! Input, J
!                                            Tau,          &  ! Output, Rank-3 K x L x I
!                                            Quiet       = Quiet,      &  ! Optional input
!                                            RCS_Id      = RCS_Id,     &  ! Revision control
!                                            Message_Log = Message_Log )  ! Error messaging
!
!
!   To read a K x L x I x M (n_Profiles) transmittance profile array:
!   -----------------------------------------------------------------
!
!     Error_Status = Read_TauProfile_netCDF( NC_Filename,  &  ! Input
!                                            Molecule_Set, &  ! Input, J
!                                            Tau,          &  ! Output, Rank-4 K x L x I x M
!                                            Quiet       = Quiet,      &  ! Optional input
!                                            RCS_Id      = RCS_Id,     &  ! Revision control
!                                            Message_Log = Message_Log )  ! Error messaging
!
!
!   To read a K x L x I x M x J (n_Molecule_Sets) transmittance profile array:
!   --------------------------------------------------------------------------
!
!     Error_Status = Read_TauProfile_netCDF( NC_Filename,  &  ! Input
!                                            Tau,          &  ! Output, Rank-5 K x L x I x M x J
!                                            Quiet       = Quiet,      &  ! Optional input
!                                            RCS_Id      = RCS_Id,     &  ! Revision control
!                                            Message_Log = Message_Log )  ! Error messaging
!
!
!   To read the entire transmittance array in the TauProfile structure:
!   -------------------------------------------------------------------
!
!     Error_Status = Read_TauProfile_netCDF( NC_Filename,  &  ! Input
!                                            TauProfile,   &  ! Output, TYPE( TauProfile_type)
!                                            Quiet       = Quiet,      &  ! Optional input
!                                            RCS_Id      = RCS_Id,     &  ! Revision control
!                                            Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:     Character string specifying the name of the
!                        netCDF format TauProfile data file to read.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Channel:         Channel number for the output transmittance Profile.
!                        Only to be used for rank-1 Tau array output.
!                        UNITS:      N/A
!                        TYPE:       Integer
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Angle:           Angle value for the output transmittance Profiles.
!                        Only to be used for rank-1 or 2 Tau array output.
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Profile:         Atmospheric Profile number for the output transmittance
!                        Profiles. Only to be used for rank-1, 2, or 3 Tau array
!                        output.
!                        UNITS:      N/A
!                        TYPE:       Integer
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Molecule_Set:    Molecule set ID number for the output transmittance
!                        Profiles. Only to be used for rank-1, 2, 3, or 4
!                        Tau array output.
!                        UNITS:      N/A
!                        TYPE:       Integer
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:           Set this keyword to suppress information messages being
!                        printed to standard output (or the message log file if
!                        the MESSAGE_LOG optional argument is used.) By default,
!                        information messages are printed.
!                        If QUIET = 0, information messages are OUTPUT.
!                           QUIET = 1, information messages are SUPPRESSED.
!                        UNITS:      N/A
!                        TYPE:       Integer
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Tau:             Transmittance data array. The dimensions of the 
!                        transmittance array on input must match those
!                        defined in the netCDF data file. The appropriate
!                        combinations of Channel, Angle, Profile, and/or
!                        Molecule_Set values must be specified as input 
!                        for Tau array output of less than rank-5.
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1, 2, 3, 4, or 5 array
!                        ATTRIBUTES: INTENT(OUT)
!         OR
!       TauProfile:      Structure to contain the transmittance data read
!                        from file.
!                        UNITS:      N/A
!                        TYPE:       TauProfile_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT)
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:          Character string containing the Revision Control
!                        System Id field for the module.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT(OUT)
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
!       If specified as the output data type, the INTENT on the output TauProfile
!       structure argument is IN OUT rather than just OUT. This is necessary
!       because the argument may be defined on input. To prevent memory leaks,
!       the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Read_TauArray_rank1( NC_Filename , &  ! Input
                                Channel     , &  ! Input
                                Angle       , &  ! Input
                                Profile     , &  ! Input
                                Molecule_Set, &  ! Input
                                Tau         , &  ! Output
                                Geometric_Angle, & ! Optional ouput
                                Quiet       , &  ! Optional input
                                RCS_Id      , &  ! Revision control
                                Message_Log ) &  ! Error messaging
                              RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER     ,           INTENT(IN)  :: Channel
    REAL(fp)    ,           INTENT(IN)  :: Angle
    INTEGER     ,           INTENT(IN)  :: Profile
    INTEGER     ,           INTENT(IN)  :: Molecule_Set
    REAL(fp)    ,           INTENT(OUT) :: Tau(:)
    REAL(fp)    , OPTIONAL, INTENT(OUT) :: Geometric_Angle(:)
    INTEGER     , OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_TauProfile_netCDF(rank1)'
    ! Local variables
    CHARACTER(1000) :: Message
    LOGICAL :: Noisy
    INTEGER :: NC_FileID
    INTEGER :: Allocate_Status
    INTEGER :: Close_Status
    INTEGER,  ALLOCATABLE :: Channel_List(:)
    REAL(fp), ALLOCATABLE :: Angle_List(:)
    INTEGER,  ALLOCATABLE :: Profile_List(:)
    INTEGER,  ALLOCATABLE :: Molecule_Set_List(:)
    INTEGER :: Channel_Index
    INTEGER :: Angle_Index
    INTEGER :: Profile_Index
    INTEGER :: Molecule_Set_Index
    INTEGER :: k, l, i, m, j

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF


    ! Read the dimension values
    ! -------------------------
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              n_Layers       =k, &
                                              n_Channels     =l, &
                                              n_Angles       =i, &
                                              n_Profiles     =m, &
                                              n_Molecule_Sets=j, &
                                              Message_Log    =Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining TauProfile dimensions from '//&
                TRIM(NC_Filename)
      GOTO 3000
    END IF

    ! Check the TauProfile dimension value
    IF ( SIZE(Tau) /= k ) THEN
      Message = 'Tau N_LAYERS array size different from netCDF definition.'
      GOTO 3000
    END IF

    ! Allocate the index list arrays
    ! ------------------------------
    ALLOCATE( Channel_List( l ), &
              Angle_List( i ), &
              Profile_List( m ), &
              Molecule_Set_List( j ), &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '( "Error allocating index list arrays. STAT = ", i0 )' ) &
                      Allocate_Status
      GOTO 3000
    END IF


    ! Fill the index list arrays
    ! --------------------------
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename                        , &
                                              Channel_List     =Channel_List     , &
                                              Angle_List       =Angle_List       , &
                                              Profile_List     =Profile_List     , &
                                              Molecule_Set_List=Molecule_Set_List, &
                                              Message_Log      =Message_Log        )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining index list data from '//&
                TRIM(NC_Filename)
      GOTO 2000
    END IF


    ! Determine the index values for the input Tau data
    ! -------------------------------------------------
    Channel_Index      = Get_Index( Channel_List, Channel )
    Angle_Index        = Get_Index( Angle_List, Angle )
    Profile_Index      = Get_Index( Profile_List, Profile )
    Molecule_Set_Index = Get_Index( Molecule_Set_List, Molecule_Set )


    ! Check the index list value
    ! --------------------------
    IF ( Channel_Index < 1 ) THEN
      WRITE( Message, '( "Invalid CHANNEL_LIST array index value, ", i0, &
                        &" for channel #", i0 )' ) &
                      Channel_Index, Channel
      GOTO 2000
    END IF
    IF ( Angle_Index < 1 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Invalid ANGLE_LIST array index value, ", i0, &
                        &" for angle ", f5.2 )' ) &
                      Angle_Index, Angle
      GOTO 2000
    END IF
    IF ( Profile_Index < 1 ) THEN
      WRITE( Message, '( "Invalid PROFILE_LIST array index value, ", i0, &
                        &" for profile ", i0 )' ) &
                      Profile_Index, Profile
      GOTO 2000
    END IF
    IF ( Molecule_Set_Index < 1 ) THEN
      WRITE( Message, '( "Invalid MOLECULE_SET_LIST array index value, ", i0, &
                        &" for molecule set #", i0 )' ) &
                      Molecule_Set_Index, Molecule_Set
      GOTO 2000
    END IF


    ! Open the file
    ! -------------
    Error_Status = Open_TauProfile_netCDF( TRIM(NC_Filename), &
                                           NC_FileID, &
                                           Mode = 'READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF TauProfile data file '//&
                TRIM(NC_Filename)
      GOTO 2000
    END IF

    ! Read the Geometric angle data
    ! ----------------------------
    IF ( PRESENT(Geometric_Angle) ) THEN 
       ! Check the TauProfile dimension values
      IF ( SIZE( Geometric_Angle, DIM=1) /= k ) THEN
        Message = 'Geometric_Angle N_LAYERS array size different from netCDF definition.'
        GOTO 3000
      END IF

      Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                          GEOMETRIC_ANGLE_VARNAME, &
                                          Geometric_Angle, &
                                          START=(/1,Angle_Index, &
                                                    Profile_Index/),&
                                          COUNT=(/k,1,1/) )
      IF ( Error_Status/= SUCCESS ) THEN
        WRITE( Message, '( "Error reading Geometric_Angle vector for ", &
                          &", Angle secant ", f5.2, ", and Profile ", i0, &
                          &", to ", a, "." )' ) &
                        Angle, Profile, TRIM(NC_Filename)
        GOTO 1000
      END IF
    
    ENDIF
 
    ! Read the transmittance data
    ! ---------------------------
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        TRANSMITTANCE_VARNAME, &
                                        Tau, &
                                        START=(/1,Channel_Index     , &
                                                  Angle_Index       , &
                                                  Profile_Index     , &
                                                  Molecule_Set_Index/), &
                                        COUNT=(/k,1,1,1,1/) )

    IF ( Error_Status/= SUCCESS ) THEN
      WRITE( Message, '( "Error reading Tau vector for Channel ", i0, &
                        &", Angle secant ", f5.2, ", Profile ", i0, &
                        &", and molecule set ", i0, " from ", a, "." )' ) &
                      Channel, Angle, Profile, Molecule_Set, &
                      TRIM(NC_Filename)
      GOTO 1000
    END IF


    ! Deallocate the list array
    ! -------------------------
    DEALLOCATE(Channel_List, Angle_List, Profile_List, Molecule_Set_List, &
               STAT=Allocate_Status)
    IF ( Allocate_Status /= 0 ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating list arrays.', &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! Close the file
    ! --------------
    Close_Status = Close_TauProfile_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauProfile data file '// &
                            TRIM(NC_Filename), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      WRITE(Message,'("Tau: ", &
                     &"N_LAYERS=",i0,2x,&
                     &"for channel ",i0,&
                     &", sec(angle) ",f4.2,&
                     &", profile ",i0,&
                     &" and molecule set ",i0 )' ) &
                     k,Channel,Angle,Profile,Molecule_Set
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(NC_Filename)//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

    !=====
    RETURN
    !=====

    ! Clean up after an error
    1000 CONTINUE
    Close_Status = Close_TauProfile_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing input file during error cleanup.'
    2000 CONTINUE
    DEALLOCATE(Channel_List, Angle_List, Profile_List, Molecule_Set_List, &
               STAT=Allocate_Status)
    IF ( Allocate_Status /= 0 ) &
      Message = TRIM(Message)//&
                '; Error deallocating list data arrays during error cleanup.'
    3000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Read_TauArray_rank1


  FUNCTION Read_TauArray_rank2( NC_Filename , &  ! Input
                                Angle       , &  ! Input
                                Profile     , &  ! Input
                                Molecule_Set, &  ! Input
                                Tau         , &  ! Output
                                Quiet       , &  ! Optional input
                                RCS_Id      , &  ! Revision control
                                Message_Log ) &  ! Error messaging
                              RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    REAL(fp)    ,           INTENT(IN)  :: Angle
    INTEGER     ,           INTENT(IN)  :: Profile
    INTEGER     ,           INTENT(IN)  :: Molecule_Set
    REAL(fp)    ,           INTENT(OUT) :: Tau(:,:)
    INTEGER     , OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_TauProfile_netCDF(rank2)'
    ! Local variables
    CHARACTER(1000) :: Message
    LOGICAL :: Noisy
    INTEGER :: NC_FileID
    INTEGER :: Allocate_Status
    INTEGER :: Close_Status
    REAL(fp), ALLOCATABLE :: Angle_List(:)
    INTEGER,  ALLOCATABLE :: Profile_List(:)
    INTEGER,  ALLOCATABLE :: Molecule_Set_List(:)
    INTEGER :: Angle_Index
    INTEGER :: Profile_Index
    INTEGER :: Molecule_Set_Index
    INTEGER :: k, l, i, m, j

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF


    ! Read the dimension values
    ! -------------------------
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              n_Layers       =k, &
                                              n_Channels     =l, &
                                              n_Angles       =i, &
                                              n_Profiles     =m, &
                                              n_Molecule_Sets=j, &
                                              Message_Log    =Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining TauProfile dimensions from '//&
                TRIM(NC_Filename)
      GOTO 3000
    END IF

    ! Check the TauProfile dimension value
    IF ( SIZE(Tau, DIM=1) /= k .OR. &
         SIZE(Tau, DIM=2) /= l      ) THEN
      Message = 'Tau N_LAYERS x N_CHANNELS '//&
                'array size different from netCDF definition.'
      GOTO 3000
    END IF


    ! Allocate the index list arrays
    ! ------------------------------
    ALLOCATE( Angle_List( i ), &
              Profile_List( m ), &
              Molecule_Set_List( j ), &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '( "Error allocating index list arrays. STAT = ", i0 )' ) &
                      Allocate_Status
      GOTO 3000
    END IF


    ! Fill the index list arrays
    ! --------------------------
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename                        , &
                                              Angle_List       =Angle_List       , &
                                              Profile_List     =Profile_List     , &
                                              Molecule_Set_List=Molecule_Set_List, &
                                              Message_Log      = Message_Log       )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining index list data from '//&
                TRIM(NC_Filename)
      GOTO 2000
    END IF


    ! Determine the index values for the input Tau data
    ! -------------------------------------------------
    Angle_Index        = Get_Index( Angle_List, Angle )
    Profile_Index      = Get_Index( Profile_List, Profile )
    Molecule_Set_Index = Get_Index( Molecule_Set_List, Molecule_Set )


    ! Check the index list value
    ! --------------------------
    IF ( Angle_Index < 1 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Invalid ANGLE_LIST array index value, ", i0, &
                        &" for angle ", f5.2 )' ) &
                      Angle_Index, Angle
      GOTO 2000
    END IF
    IF ( Profile_Index < 1 ) THEN
      WRITE( Message, '( "Invalid PROFILE_LIST array index value, ", i0, &
                        &" for profile ", i0 )' ) &
                      Profile_Index, Profile
      GOTO 2000
    END IF
    IF ( Molecule_Set_Index < 1 ) THEN
      WRITE( Message, '( "Invalid MOLECULE_SET_LIST array index value, ", i0, &
                        &" for molecule set #", i0 )' ) &
                      Molecule_Set_Index, Molecule_Set
      GOTO 2000
    END IF


    ! Open the file
    ! -------------
    Error_Status = Open_TauProfile_netCDF( TRIM(NC_Filename), &
                                           NC_FileID, &
                                           Mode = 'READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF TauProfile data file '//&
                TRIM(NC_Filename)
      GOTO 2000
    END IF


    ! Read the transmittance data
    ! ---------------------------
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        TRANSMITTANCE_VARNAME, &
                                        Tau, &
                                        START=(/1,1,Angle_Index, &
                                                    Profile_Index, &
                                                    Molecule_Set_Index/), &
                                        COUNT=(/k,l,1,1,1/) )
    IF ( Error_Status/= SUCCESS ) THEN
      WRITE( Message, '( "Error reading Tau array for angle secant ", f5.2, &
                        &", profile ", i0, ", and molecule set ", i0, " from ", a, "." )' ) &
                      Angle, Profile, Molecule_Set, &
                      TRIM(NC_Filename)
      GOTO 1000
    END IF


    ! Deallocate the list array
    ! -------------------------
    DEALLOCATE(Angle_List, Profile_List, Molecule_Set_List, STAT=Allocate_Status)
    IF ( Allocate_Status /= 0 ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating list arrays.', &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! Close the file
    ! --------------
    Close_Status = Close_TauProfile_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauProfile data file '// &
                            TRIM(NC_Filename), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      WRITE(Message,'("Tau: ", &
                     &"N_LAYERS=",i0,2x,&
                     &"N_CHANNELS=",i0,2x,&
                     &"for sec(angle) ",f4.2,&
                     &", profile ",i0," and molecule set ",i0 )' ) &
                     k,l,Angle,Profile,Molecule_Set
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(NC_Filename)//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

    !=====
    RETURN
    !=====

    ! Clean up after an error
    1000 CONTINUE
    Close_Status = Close_TauProfile_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing input file during error cleanup.'
    2000 CONTINUE
    DEALLOCATE(Angle_List, Profile_List, Molecule_Set_List, STAT=Allocate_Status)
    IF ( Allocate_Status /= 0 ) &
      Message = TRIM(Message)//&
                '; Error deallocating list data arrays during error cleanup.'
    3000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Read_TauArray_rank2


  FUNCTION Read_TauArray_rank3( NC_Filename , &  ! Input
                                Profile     , &  ! Input
                                Molecule_Set, &  ! Input
                                Tau         , &  ! Output
                                Geometric_Angle, & ! Optional output
                                Quiet       , &  ! Optional input
                                RCS_Id      , &  ! Revision control
                                Message_Log ) &  ! Error messaging
                              RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER     ,           INTENT(IN)  :: Profile
    INTEGER     ,           INTENT(IN)  :: Molecule_Set
    REAL(fp)    ,           INTENT(OUT) :: Tau(:,:,:)
    REAL(fp)    , OPTIONAL, INTENT(OUT) :: Geometric_Angle(:,:)
    INTEGER     , OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_TauProfile_netCDF(rank3)'
    ! Local variables
    CHARACTER(1000) :: Message
    LOGICAL :: Noisy
    INTEGER :: NC_FileID
    INTEGER :: Allocate_Status
    INTEGER :: Close_Status
    INTEGER, ALLOCATABLE :: Profile_List(:)
    INTEGER, ALLOCATABLE :: Molecule_Set_List(:)
    INTEGER :: Profile_Index
    INTEGER :: Molecule_Set_Index
    INTEGER :: k, l, i, m, j

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF


    ! Read the dimension values
    ! -------------------------
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              n_Layers       =k, &
                                              n_Channels     =l, &
                                              n_Angles       =i, &
                                              n_Profiles     =m, &
                                              n_Molecule_Sets=j, &
                                              Message_Log    =Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining TauProfile dimensions from '//&
                TRIM(NC_Filename)
      GOTO 3000
    END IF

    ! Check the TauProfile dimension value
    IF ( SIZE(Tau, DIM=1) /= k .OR. &
         SIZE(Tau, DIM=2) /= l .OR. &
         SIZE(Tau, DIM=3) /= i      ) THEN
      Message = 'Tau N_LAYERS x N_CHANNELS x N_ANGLES '//&
                'array size different from netCDF definition.'
      GOTO 3000
    END IF


    ! Allocate the index list arrays
    ! ------------------------------
    ALLOCATE( Profile_List( m ), &
              Molecule_Set_List( j ), &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '( "Error allocating index list arrays. STAT = ", i0 )' ) &
                      Allocate_Status
      GOTO 3000
    END IF


    ! Fill the index list arrays
    ! --------------------------
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename                        , &
                                              Profile_List     =Profile_List     , &
                                              Molecule_Set_List=Molecule_Set_List, &
                                              Message_Log      =Message_Log        )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining index list data from '//&
                TRIM(NC_Filename)
      GOTO 2000
    END IF


    ! Determine the index values for the input Tau data
    ! -------------------------------------------------
    Profile_Index      = Get_Index( Profile_List, Profile )
    Molecule_Set_Index = Get_Index( Molecule_Set_List, Molecule_Set )


    ! Check the index list value
    ! --------------------------
    IF ( Profile_Index < 1 ) THEN
      WRITE( Message, '( "Invalid PROFILE_LIST array index value, ", i0, &
                        &" for profile ", i0 )' ) &
                      Profile_Index, Profile
      GOTO 2000
    END IF
    IF ( Molecule_Set_Index < 1 ) THEN
      WRITE( Message, '( "Invalid MOLECULE_SET_LIST array index value, ", i0, &
                        &" for molecule set #", i0 )' ) &
                      Molecule_Set_Index, Molecule_Set
      GOTO 2000
    END IF


    ! Open the file
    ! -------------
    Error_Status = Open_TauProfile_netCDF( TRIM(NC_Filename), &
                                           NC_FileID, &
                                           Mode = 'READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF TauProfile data file '//&
                TRIM(NC_Filename)
      GOTO 2000
    END IF

    ! Read the Geometric angle data
    ! ----------------------------
    IF ( PRESENT(Geometric_Angle) ) THEN 
       ! Check the TauProfile dimension values
      IF ( SIZE( Geometric_Angle, DIM=1) /= k .OR. &
           SIZE( Geometric_Angle, DIM=2) /= i      ) THEN 
        Message = 'Geometric_Angle N_LAYERS X N_ANGLES array size different from netCDF definition.'
        GOTO 3000
      END IF

      Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                          GEOMETRIC_ANGLE_VARNAME, &
                                          Geometric_Angle, &
                                          START=(/1,1,Profile_Index/),&
                                          COUNT=(/k,i,1/) )
      IF ( Error_Status/= SUCCESS ) THEN
        WRITE( Message, '( "Error reading Geometric_Angle vector for ", &
                          &" Profile ", i0, &
                          &", to ", a, "." )' ) &
                          Profile, TRIM(NC_Filename)
        GOTO 1000
      END IF
    
    ENDIF

    ! Read the transmittance data
    ! ---------------------------
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        TRANSMITTANCE_VARNAME, &
                                        Tau, &
                                        START=(/1,1,1,Profile_Index, &
                                                      Molecule_Set_Index/), &
                                        COUNT=(/k,l,i,1,1/) )
    IF ( Error_Status/= SUCCESS ) THEN
      WRITE( Message, '( "Error reading Tau array for profile ", i0, &
                        &", and molecule set ", i0, " from ", a, "." )' ) &
                      Profile, Molecule_Set, &
                      TRIM(NC_Filename)
      GOTO 1000
    END IF


    ! Deallocate the list array
    ! -------------------------
    DEALLOCATE(Profile_List, Molecule_Set_List, STAT=Allocate_Status)
    IF ( Allocate_Status /= 0 ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating list arrays.', &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! Close the file
    ! --------------
    Close_Status = Close_TauProfile_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauProfile data file '// &
                            TRIM(NC_Filename), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      WRITE(Message,'("Tau: ", &
                     &"N_LAYERS=",i0,2x,&
                     &"N_CHANNELS=",i0,2x,&
                     &"N_ANGLES=",i0,2x,&
                     &"for profile ",i0," and molecule set ",i0 )' ) &
                     k,l,i,Profile,Molecule_Set
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(NC_Filename)//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

    !=====
    RETURN
    !=====

    ! Clean up after an error
    1000 CONTINUE
    Close_Status = Close_TauProfile_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing input file during error cleanup.'
    2000 CONTINUE
    DEALLOCATE(Profile_List, Molecule_Set_List, STAT=Allocate_Status)
    IF ( Allocate_Status /= 0 ) &
      Message = TRIM(Message)//&
                '; Error deallocating list data arrays during error cleanup.'
    3000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Read_TauArray_rank3


  FUNCTION Read_TauArray_rank4( NC_Filename , &  ! Input
                                Molecule_Set, &  ! Input
                                Tau         , &  ! Output
                                Geometric_Angle, & ! Optional output
                                Quiet       , &  ! Optional input
                                RCS_Id      , &  ! Revision control
                                Message_Log ) &  ! Error messaging
                              RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER     ,           INTENT(IN)  :: Molecule_Set
    REAL(fp)    ,           INTENT(OUT) :: Tau(:,:,:,:)
    REAL(fp)    , OPTIONAL, INTENT(OUT) :: Geometric_Angle(:,:,:)
    INTEGER     , OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_TauProfile_netCDF(rank4)'
    ! Local variables
    CHARACTER(1000) :: Message
    LOGICAL :: Noisy
    INTEGER :: NC_FileID
    INTEGER :: Allocate_Status
    INTEGER :: Close_Status
    INTEGER, ALLOCATABLE :: Molecule_Set_List(:)
    INTEGER :: Molecule_Set_Index
    INTEGER :: k, l, i, m, j

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF


    ! Read the dimension values
    ! -------------------------
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              n_Layers       =k, &
                                              n_Channels     =l, &
                                              n_Angles       =i, &
                                              n_Profiles     =m, &
                                              n_Molecule_Sets=j, &
                                              Message_Log    =Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining TauProfile dimensions from '//&
                TRIM(NC_Filename)
      GOTO 3000
    END IF

    ! Check the TauProfile dimension value
    IF ( SIZE(Tau, DIM=1) /= k .OR. &
         SIZE(Tau, DIM=2) /= l .OR. &
         SIZE(Tau, DIM=3) /= i .OR. &
         SIZE(Tau, DIM=4) /= m      ) THEN
      Message = 'Tau N_LAYERS x N_CHANNELS x N_ANGLES x N_PROFILES '//&
                'array size different from netCDF definition.'
      GOTO 3000
    END IF


    ! Allocate the index list arrays
    ! ------------------------------
    ALLOCATE( Molecule_Set_List( j ), &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '( "Error allocating index list arrays. STAT = ", i0 )' ) &
                      Allocate_Status
      GOTO 3000
    END IF


    ! Fill the index list arrays
    ! --------------------------
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              Molecule_Set_List=Molecule_Set_List, &
                                              Message_Log      =Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining index list data from '//&
                TRIM(NC_Filename)
      GOTO 2000
    END IF


    ! Determine the index values for the input Tau data
    ! -------------------------------------------------
    Molecule_Set_Index = Get_Index( Molecule_Set_List, Molecule_Set )


    ! Check the index list value
    ! --------------------------
    IF ( Molecule_Set_Index < 1 ) THEN
      WRITE( Message, '( "Invalid MOLECULE_SET_LIST array index value, ", i0, &
                        &" for molecule set #", i0 )' ) &
                      Molecule_Set_Index, Molecule_Set
      GOTO 2000
    END IF


    ! Open the file
    ! -------------
    Error_Status = Open_TauProfile_netCDF( TRIM(NC_Filename), &
                                           NC_FileID, &
                                           Mode = 'READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF TauProfile data file '//&
                TRIM(NC_Filename)
      GOTO 2000
    END IF

    ! Read the Geometric angle data
    ! ----------------------------
    IF ( PRESENT(Geometric_Angle) ) THEN 
       ! Check the TauProfile dimension values
      IF ( SIZE( Geometric_Angle, DIM=1) /= k .OR. &
           SIZE( Geometric_Angle, DIM=2) /= i .OR. &
           SIZE( Geometric_Angle, DIM=3) /= m      ) THEN 
        Message = 'Geometric_Angle N_LAYERS x N_ANGLES x N_PROFILES array size different from netCDF definition.'
        GOTO 3000
      END IF

      Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                          GEOMETRIC_ANGLE_VARNAME, &
                                          Geometric_Angle, &
                                          START=(/1,1,1/),&
                                          COUNT=(/k,i,m/) )
      IF ( Error_Status/= SUCCESS ) THEN
        WRITE( Message, '( "Error reading Geometric_Angle vector for to ", a, "." )' ) &
                          TRIM(NC_Filename)
        GOTO 1000
      END IF
    
    ENDIF


    ! Read the transmittance data
    ! ---------------------------
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        TRANSMITTANCE_VARNAME, &
                                        Tau, &
                                        START=(/1,1,1,1,Molecule_Set_Index/), &
                                        COUNT=(/k,l,i,m,1/) )

    IF ( Error_Status/= SUCCESS ) THEN
      WRITE( Message, '( "Error reading Tau array for molecule set ", i0, &
                        &" from ", a, "." )' ) &
                      Molecule_Set, &
                      TRIM(NC_Filename)
      GOTO 1000
    END IF


    ! Deallocate the list array
    ! -------------------------
    DEALLOCATE(Molecule_Set_List, STAT=Allocate_Status)
    IF ( Allocate_Status /= 0 ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating list arrays.', &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! Close the file
    ! --------------
    Close_Status = Close_TauProfile_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauProfile data file '// &
                            TRIM(NC_Filename), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      WRITE(Message,'("Tau: ", &
                     &"N_LAYERS=",i0,2x,&
                     &"N_CHANNELS=",i0,2x,&
                     &"N_ANGLES=",i0,2x,&
                     &"N_PROFILES=",i0,2x,&
                     &"for molecule set ",i0 )' ) k,l,i,m,Molecule_Set
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(NC_Filename)//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

    !=====
    RETURN
    !=====

    ! Clean up after an error
    1000 CONTINUE
    Close_Status = Close_TauProfile_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing input file during error cleanup.'
    2000 CONTINUE
    DEALLOCATE(Molecule_Set_List, STAT=Allocate_Status)
    IF ( Allocate_Status /= 0 ) &
      Message = TRIM(Message)//&
                '; Error deallocating list data arrays during error cleanup.'
    3000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Read_TauArray_rank4


  FUNCTION Read_TauArray_rank5( NC_Filename, &  ! Input
                                Tau        , &  ! Output
                                Quiet      , &  ! Optional input
                                RCS_Id     , &  ! Revision control
                                Message_Log) &  ! Error messaging
                              RESULT ( Error_Status )
    ! Arguments               
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    REAL(fp)    ,           INTENT(OUT) :: Tau(:,:,:,:,:)
    INTEGER     , OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_TauProfile_netCDF(rank5)'
    ! Local variables
    CHARACTER(1000) :: Message
    LOGICAL :: Noisy
    INTEGER :: NC_FileID
    INTEGER :: Close_Status
    INTEGER :: k, l, i, m, j

    ! Set up
    ! ------ 
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF


    ! Read the dimension values
    ! -------------------------
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              n_Layers       =k, &
                                              n_Channels     =l, &
                                              n_Angles       =i, &
                                              n_Profiles     =m, &
                                              n_Molecule_Sets=j, &
                                              Message_Log    =Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining TauProfile dimensions from '//&
                TRIM(NC_Filename)
      GOTO 2000
    END IF

    ! Check the TauProfile dimension values
    IF ( SIZE(Tau, DIM=1) /= k .OR. &
         SIZE(Tau, DIM=2) /= l .OR. &
         SIZE(Tau, DIM=3) /= i .OR. &
         SIZE(Tau, DIM=4) /= m .OR. &
         SIZE(Tau, DIM=5) /= j      ) THEN
      Message = 'Tau N_LAYERS x N_CHANNELS x N_ANGLES x N_PROFILES '//&
                'x N_MOLCULE_SETS array size different from netCDF definition.'
      GOTO 2000
    END IF


    ! Open the file
    ! -------------            
    Error_Status = Open_TauProfile_netCDF( TRIM(NC_Filename), &
                                           NC_FileID, &
                                           Mode = 'READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF TauProfile data file '//&
                TRIM(NC_Filename)
      GOTO 2000
    END IF


    ! Read the transmittances
    ! -----------------------  
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        TRANSMITTANCE_VARNAME, &
                                        Tau )
    IF ( Error_Status/= SUCCESS ) THEN
      WRITE( Message, '( "Error reading Tau array from ", a, "." )' ) &
                      TRIM(NC_Filename)
      GOTO 1000
    END IF


    ! Close the file
    ! --------------           
    Close_Status = Close_TauProfile_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauProfile data file '// &
                            TRIM(NC_Filename), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! Output an info message
    ! ----------------------   
    IF ( Noisy ) THEN
      WRITE(Message,'("Tau: ", &
                     &"N_LAYERS=",i0,2x,&
                     &"N_CHANNELS=",i0,2x,&
                     &"N_ANGLES=",i0,2x,&
                     &"N_PROFILES=",i0,2x,&
                     &"N_MOLECULE_SETS=",i0 )' ) k,l,i,m,j
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(NC_Filename)//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

    !=====
    RETURN
    !=====

    ! Clean up after an error
    1000 CONTINUE
    Close_Status = Close_TauProfile_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing input file during error cleanup.'
    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Read_TauArray_rank5


  FUNCTION Read_TauProfile_type( NC_Filename, &   ! Input
                                 TauProfile , &   ! Output
                                 Profile_Angle, &   ! Optional input
                                 Quiet      , &   ! Optional input
                                 RCS_Id     , &   ! Revision contorl
                                 Message_Log) &   ! Error messaging
                               RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)     :: NC_Filename
    TYPE(TauProfile_type) , INTENT(IN OUT) :: TauProfile
    INTEGER     , OPTIONAL, INTENT(IN)     :: Profile_Angle
    INTEGER     , OPTIONAL, INTENT(IN)     :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_TauProfile_netCDF(Structure)'
    ! Function variables
    CHARACTER(1000) :: Message
    LOGICAL :: Noisy
    LOGICAL :: Read_Profile_Angle
    INTEGER :: NC_FileID
    INTEGER :: Destroy_Status
    INTEGER :: Close_Status
    INTEGER :: k, l, i, m, j

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF

    Read_Profile_Angle = .FALSE.
    ! ....unless the Profile_Angle keyword is set.
    IF ( PRESENT( Profile_Angle ) ) THEN
      IF ( Profile_Angle == 1 ) Read_Profile_Angle = .TRUE.
    END IF

    ! Read the dimension values
    ! -------------------------
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              n_Layers       =k, &
                                              n_Channels     =l, &
                                              n_Angles       =i, &
                                              n_Profiles     =m, &
                                              n_Molecule_Sets=j, &
                                              Message_Log    =Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining TauProfile dimensions from '//&
                TRIM(NC_Filename)
      GOTO 3000
    END IF


    ! Allocate the structure
    ! ----------------------
    Error_Status = Allocate_TauProfile( k, &  ! n_Layers
                                        l, &  ! n_Channels
                                        i, &  ! n_Angles
                                        m, &  ! n_Profiles
                                        j, &  ! n_Molecule_Sets
                                        TauProfile, &
                                        Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error occurred allocating TauProfile structure.'
      GOTO 3000
    END IF


    ! Read the global attributes
    ! --------------------------
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename                                 , &
                                              Release         =TauProfile%Release         , &
                                              Version         =TauProfile%Version         , &
                                              Sensor_ID       =TauProfile%Sensor_ID       , &  
                                              WMO_Satellite_ID=TauProfile%WMO_Satellite_ID, &
                                              WMO_Sensor_ID   =TauProfile%WMO_Sensor_ID   , &   
                                              Message_Log     =Message_Log                  )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining TauProfile global attributes from '//&
                TRIM(NC_Filename)
      GOTO 2000
    END IF
    
    ! Check the release
    Error_Status = CheckRelease_TauProfile( TauProfile, &
                                            Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'TauProfile Release check failed for '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    
    
    ! Fill the dimension descriptor arrays
    ! ------------------------------------
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename                                , &
                                              Level_Pressure   =TauProfile%Level_Pressure, &
                                              Channel_List     =TauProfile%Channel       , &
                                              Angle_List       =TauProfile%Angle         , &
                                              Profile_List     =TauProfile%Profile       , &
                                              Molecule_Set_List=TauProfile%Molecule_Set  , &
                                              Message_Log      =Message_Log                )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining TauProfile list arrays from '//&
                TRIM(NC_Filename)
      GOTO 2000
    END IF


    ! Open the file
    ! -------------
    Error_Status = Open_TauProfile_netCDF( TRIM(NC_Filename), &
                                         NC_FileID, &
                                         Mode = 'READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF TauProfile data file '//&
                TRIM(NC_Filename)
      GOTO 2000
    END IF

    ! Read the Geometric angle data
    ! ----------------------------
    IF ( Read_Profile_Angle ) THEN
      Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                          GEOMETRIC_ANGLE_VARNAME, &
                                          TauProfile%Geometric_Angle)
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error reading Geometric_Angle array to ", a, "." )' ) &
                        TRIM(NC_Filename)
        GOTO 1000
      END IF
    END IF

    ! Read all the transmittance data
    ! -------------------------------
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        TRANSMITTANCE_VARNAME, &
                                        TauProfile%Tau )
    IF ( Error_Status/= SUCCESS ) THEN
      WRITE( Message, '( "Error reading Tau array from ", a, "." )' ) &
                      TRIM(NC_Filename)
      GOTO 1000
    END IF


    ! Close the file
    ! --------------
    Close_Status = Close_TauProfile_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauProfile data file '// &
                            TRIM(NC_Filename), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_TauProfile( TauProfile, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(NC_Filename)//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

    !=====
    RETURN
    !=====
    
    ! Clean up after an error
    ! -----------------------
    1000 CONTINUE
    Close_Status = Close_TauProfile_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing input file during error cleanup.'
    2000 CONTINUE
    Destroy_Status = Destroy_TauProfile(TauProfile, Message_Log=Message_Log)
    IF ( Destroy_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error destroying TauProfile during error cleanup.'
    3000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Read_TauProfile_type

END MODULE TauProfile_netCDF_IO
