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
                               Information_TauProfile
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
    MODULE PROCEDURE Get_Double_Index
  END INTERFACE Get_Index


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Module RCS Id string
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
    '$Id$'

  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp

  ! Maximum angle secant value
  REAL(fp), PARAMETER :: ANGLE_LIMIT = 3.0_fp

  ! Global attribute names. Case sensitive
  CHARACTER(*), PARAMETER :: TITLE_GATTNAME         = 'title' 
  CHARACTER(*), PARAMETER :: HISTORY_GATTNAME       = 'history' 
  CHARACTER(*), PARAMETER :: SENSOR_NAME_GATTNAME   = 'sensor_name' 
  CHARACTER(*), PARAMETER :: PLATFORM_NAME_GATTNAME = 'platform_name' 
  CHARACTER(*), PARAMETER :: COMMENT_GATTNAME       = 'comment' 
  CHARACTER(*), PARAMETER :: ID_TAG_GATTNAME        = 'id_tag' 

  ! Dimension names
  CHARACTER(*), PARAMETER :: LEVEL_DIMNAME        = 'n_levels'
  CHARACTER(*), PARAMETER :: LAYER_DIMNAME        = 'n_layers'
  CHARACTER(*), PARAMETER :: CHANNEL_DIMNAME      = 'n_Channels'
  CHARACTER(*), PARAMETER :: ANGLE_DIMNAME        = 'n_Angles'
  CHARACTER(*), PARAMETER :: PROFILE_DIMNAME      = 'n_Profiles'
  CHARACTER(*), PARAMETER :: MOLECULE_SET_DIMNAME = 'n_Molecule_Sets'
  CHARACTER(*), PARAMETER :: STRLEN_DIMNAME       = 'sensor_id_strlen'

  ! Variable names
  CHARACTER(*), PARAMETER :: SENSOR_ID_VARNAME         = 'Sensor_ID'
  CHARACTER(*), PARAMETER :: WMO_SATELLITE_ID_VARNAME  = 'WMO_Satellite_ID'
  CHARACTER(*), PARAMETER :: WMO_SENSOR_ID_VARNAME     = 'WMO_Sensor_ID'
  CHARACTER(*), PARAMETER :: LEVEL_PRESSURE_VARNAME    = 'Level_Pressure'
  CHARACTER(*), PARAMETER :: CHANNEL_LIST_VARNAME      = 'Channel_list'
  CHARACTER(*), PARAMETER :: ANGLE_LIST_VARNAME        = 'Angle_list'
  CHARACTER(*), PARAMETER :: PROFILE_LIST_VARNAME      = 'Profile_list'
  CHARACTER(*), PARAMETER :: MOLECULE_SET_LIST_VARNAME = 'Molecule_Set_list'
  CHARACTER(*), PARAMETER :: TRANSMITTANCE_VARNAME     = 'transmittance'

  ! Variable long name attribute.
  CHARACTER(*), PARAMETER :: LONGNAME_ATTNAME = 'long_name'

  CHARACTER(*), PARAMETER :: SENSOR_ID_LONGNAME         = 'Sensor Identifier'
  CHARACTER(*), PARAMETER :: WMO_SATELLITE_ID_LONGNAME  = 'WMO satellite ID'
  CHARACTER(*), PARAMETER :: WMO_SENSOR_ID_LONGNAME     = 'WMO sensor ID'
  CHARACTER(*), PARAMETER :: LEVEL_PRESSURE_LONGNAME    = 'Level pressure'
  CHARACTER(*), PARAMETER :: CHANNEL_LIST_LONGNAME      = 'Sensor channel'
  CHARACTER(*), PARAMETER :: ANGLE_LIST_LONGNAME        = 'Zenith angle'
  CHARACTER(*), PARAMETER :: PROFILE_LIST_LONGNAME      = 'Profile number'
  CHARACTER(*), PARAMETER :: MOLECULE_SET_LIST_LONGNAME = 'Molecular species index'
  CHARACTER(*), PARAMETER :: TRANSMITTANCE_LONGNAME     = 'Instrument transmittance'

  ! Variable description attribute.
  CHARACTER(*), PARAMETER :: DESCRIPTION_ATTNAME = 'description'

  CHARACTER(*), PARAMETER :: SENSOR_ID_DESC         = &
    'Character string identifying the sensor and satellite platform'
  CHARACTER(*), PARAMETER :: WMO_SATELLITE_ID_DESC  = &
    'WMO code for identifying satellite platforms (1023 == none available)'
  CHARACTER(*), PARAMETER :: WMO_SENSOR_ID_DESC     = &
    'WMO code for identifying a satellite sensor (2047 == none available)'
  CHARACTER(*), PARAMETER :: LEVEL_PRESSURE_DESC    = &
    'Atmospheric level pressures defining the layers for which the transmittances were calculated.'
  CHARACTER(*), PARAMETER :: CHANNEL_LIST_DESC      = &
    'List of sensor channel numbers associated with the TauProfile data.'
  CHARACTER(*), PARAMETER :: ANGLE_LIST_DESC        = &
    'List of the secant of the zenith angles.'
  CHARACTER(*), PARAMETER :: PROFILE_LIST_DESC      = &
    'List of atmospheric Profile set indices used in generating the transmittance data.'
  CHARACTER(*), PARAMETER :: MOLECULE_SET_LIST_DESC = &
    'List of molecular species set indices used in generating the transmittance data.'
  CHARACTER(*), PARAMETER :: TRANSMITTANCE_DESC     = &
    'Instrument resolution channel transmittance profiles'

  ! Variable units attribute.
  CHARACTER(*), PARAMETER :: UNITS_ATTNAME = 'units'
  
  CHARACTER(*), PARAMETER :: SENSOR_ID_UNITS         = 'N/A'
  CHARACTER(*), PARAMETER :: WMO_SATELLITE_ID_UNITS  = 'N/A'
  CHARACTER(*), PARAMETER :: WMO_SENSOR_ID_UNITS     = 'N/A'
  CHARACTER(*), PARAMETER :: LEVEL_PRESSURE_UNITS    = 'hectoPascals (hPa)'
  CHARACTER(*), PARAMETER :: CHANNEL_LIST_UNITS      = 'N/A'
  CHARACTER(*), PARAMETER :: ANGLE_LIST_UNITS        = 'None'
  CHARACTER(*), PARAMETER :: PROFILE_LIST_UNITS      = 'N/A'
  CHARACTER(*), PARAMETER :: MOLECULE_SET_LIST_UNITS = 'N/A'
  CHARACTER(*), PARAMETER :: TRANSMITTANCE_UNITS     = 'None'

  ! Variable _FillValue attribute.
  CHARACTER(*), PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'
  
  CHARACTER(*), PARAMETER :: SENSOR_ID_FILLVALUE         = ' '
  INTEGER     , PARAMETER :: WMO_SATELLITE_ID_FILLVALUE  = 1023
  INTEGER     , PARAMETER :: WMO_SENSOR_ID_FILLVALUE     = 2047
  REAL(fp)    , PARAMETER :: FP_FILLVALUE                = -1.0_fp
  INTEGER     , PARAMETER :: IP_FILLVALUE                = -1
  REAL(fp)    , PARAMETER :: LEVEL_PRESSURE_FILLVALUE    = FP_FILLVALUE
  INTEGER     , PARAMETER :: CHANNEL_LIST_FILLVALUE      = IP_FILLVALUE 
  REAL(fp)    , PARAMETER :: ANGLE_LIST_FILLVALUE        = FP_FILLVALUE
  INTEGER     , PARAMETER :: PROFILE_LIST_FILLVALUE      = IP_FILLVALUE 
  INTEGER     , PARAMETER :: MOLECULE_SET_LIST_FILLVALUE = IP_FILLVALUE 
  REAL(fp)    , PARAMETER :: TRANSMITTANCE_FILLVALUE     = FP_FILLVALUE

  ! Variable types
  INTEGER, PARAMETER :: SENSOR_ID_TYPE         = NF90_CHAR
  INTEGER, PARAMETER :: WMO_SATELLITE_ID_TYPE  = NF90_INT
  INTEGER, PARAMETER :: WMO_SENSOR_ID_TYPE     = NF90_INT
  INTEGER, PARAMETER :: LEVEL_PRESSURE_TYPE    = NF90_DOUBLE
  INTEGER, PARAMETER :: CHANNEL_LIST_TYPE      = NF90_INT
  INTEGER, PARAMETER :: ANGLE_LIST_TYPE        = NF90_DOUBLE
  INTEGER, PARAMETER :: PROFILE_LIST_TYPE      = NF90_INT
  INTEGER, PARAMETER :: MOLECULE_SET_LIST_TYPE = NF90_INT
  INTEGER, PARAMETER :: TRANSMITTANCE_TYPE     = NF90_DOUBLE


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
    INTEGER, DIMENSION(:), INTENT(IN) :: List
    INTEGER,               INTENT(IN) :: Value
    INTEGER :: Idx
    Idx = MINLOC( ABS( List-Value ), DIM = 1 )
    IF ( ( List(Idx)-Value ) /= 0 ) Idx = -1
  END FUNCTION Get_Integer_Index

  FUNCTION Get_Double_Index( List, Value ) RESULT( Idx )
    REAL(fp), DIMENSION(:), INTENT(IN) :: List
    REAL(fp),                 INTENT(IN) :: Value
    INTEGER :: Idx
    REAL(fp), PARAMETER :: TOLERANCE = EPSILON( 1.0_fp )
    Idx = MINLOC( ABS( List-Value ), DIM = 1 )
    IF ( ABS( List(Idx)-Value ) > TOLERANCE ) Idx = -1
  END FUNCTION Get_Double_Index


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
!       Error_Status = Write_TauProfile_GAtts( NC_Filename                , &  ! Input
!                                              NC_FileID                  , &  ! Input
!                                              ID_Tag       =ID_Tag       , &  ! Optional input
!                                              Title        =Title        , &  ! Optional input
!                                              History      =History      , &  ! Optional input
!                                              Sensor_Name  =Sensor_Name  , &  ! Optional input
!                                              Platform_Name=Platform_Name, &  ! Optional input
!                                              Comment      =Comment      , &  ! Optional input
!                                              Message_Log  =Message_Log    )  ! Error messaging
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
!       Sensor_Name:      Character string written into the SENSOR_NAME
!                         global attribute field of the netCDF TauProfile
!                         file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Platform_Name:    Character string written into the PLATFORM_NAME
!                         global attribute field of the netCDF TauProfile
!                         file.
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
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Apr-2002
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Write_TauProfile_GAtts( NC_Filename  , &  ! Input
                                   NC_FileID    , &  ! Input
                                   ID_Tag       , &  ! Optional input
                                   Title        , &  ! Optional input
                                   History      , &  ! Optional input
                                   Sensor_Name  , &  ! Optional input
                                   Platform_Name, &  ! Optional input
                                   Comment      , &  ! Optional input
                                   Message_Log  ) &  ! Error messaging
                                 RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: NC_Filename
    INTEGER     ,           INTENT(IN) :: NC_FileID
    CHARACTER(*), OPTIONAL, INTENT(IN) :: ID_Tag
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Sensor_Name
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Platform_Name
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_TauProfile_GAtts'
    CHARACTER(*), PARAMETER :: WRITE_MODULE_HISTORY_GATTNAME   = 'write_module_history' 
    CHARACTER(*), PARAMETER :: CREATION_DATE_AND_TIME_GATTNAME = 'creation_date_and_time' 
    INTEGER     , PARAMETER :: NPUTGATTS = 8
    ! Local variables
    INTEGER :: Put_Status(NPUTGATTS), n
    CHARACTER(8)  :: cdate
    CHARACTER(10) :: ctime
    CHARACTER(5)  :: czone

    ! Set up
    Error_Status = SUCCESS
    Put_Status   = SUCCESS
    n = 0

    ! Software ID
    n = n + 1
    Put_Status(n) = Put_GAttString(WRITE_MODULE_HISTORY_GATTNAME, &
                                   MODULE_RCS_ID, &
                                   Message_Log=Message_Log )

    ! Creation date
    CALL DATE_AND_TIME( cdate, ctime, czone )
    n = n + 1
    Put_Status(n) = Put_GAttString(CREATION_DATE_AND_TIME_GATTNAME, &
                                   cdate(1:4)//'/'//cdate(5:6)//'/'//cdate(7:8)//', '// &
                                   ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//' '// &
                                   czone//'UTC', &
                                   Message_Log=Message_Log )

    ! The Title
    n = n + 1
    IF ( PRESENT( Title ) ) THEN
      Put_Status(n) = Put_GAttString(TITLE_GATTNAME, Title, &
                                     Message_Log=Message_Log )
    END IF

    ! The History
    n = n + 1
    IF ( PRESENT( History ) ) THEN
      Put_Status(n) = Put_GAttString(HISTORY_GATTNAME, History, &
                                     Message_Log=Message_Log )
    END IF

    ! The Sensor_Name
    n = n + 1
    IF ( PRESENT( Sensor_Name ) ) THEN
      Put_Status(n) = Put_GAttString(SENSOR_NAME_GATTNAME, Sensor_Name, &
                                     Message_Log=Message_Log )
    END IF

    ! The Platform_Name
    n = n + 1
    IF ( PRESENT( Platform_Name ) ) THEN
      Put_Status(n) = Put_GAttString(PLATFORM_NAME_GATTNAME, Platform_Name, &
                                     Message_Log=Message_Log )
    END IF

    ! The Comment
    n = n + 1
    IF ( PRESENT( Comment ) ) THEN
      Put_Status(n) = Put_GAttString(COMMENT_GATTNAME, Comment, &
                                     Message_Log=Message_Log )
    END IF

    ! The ID_Tag
    n = n + 1
    IF ( PRESENT( ID_Tag ) ) THEN
      Put_Status(n) = Put_GAttString(ID_TAG_GATTNAME, ID_Tag, &
                                     Message_Log=Message_Log )
    END IF

    ! Check for any errors
    IF ( ANY( Put_Status /= SUCCESS ) ) Error_Status = WARNING

  CONTAINS

    FUNCTION Put_GAttString(GAttName, GAttString, Message_Log) RESULT(Error_Status)
      CHARACTER(*),           INTENT(IN) :: GAttName
      CHARACTER(*),           INTENT(IN) :: GAttString
      CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
      INTEGER :: Error_Status
      INTEGER :: NF90_Status
      Error_Status = SUCCESS
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  TRIM(GAttString) )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//TRIM(GAttName)//' attribute to '//&
                              TRIM( NC_Filename )//' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END FUNCTION Put_GAttString

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
!       Error_Status = Read_TauProfile_GAtts( NC_Filename                , &  ! Input
!                                             NC_FileID                  , &  ! Input
!                                             ID_Tag       =ID_Tag       , &  ! Optional output
!                                             Title        =Title        , &  ! Optional output
!                                             History      =History      , &  ! Optional output
!                                             Sensor_Name  =Sensor_Name  , &  ! Optional output
!                                             Platform_Name=Platform_Name, &  ! Optional output
!                                             Comment      =Comment      , &  ! Optional output
!                                             Message_Log  =Message_Log    )  ! Error messaging
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
!       Sensor_Name:      Character string written into the SENSOR_NAME
!                         global attribute field of the netCDF TauProfile
!                         file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Platform_Name:    Character string written into the PLATFORM_NAME
!                         global attribute field of the netCDF TauProfile
!                         file.
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
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the global attribute read was successful.
!                        == WARNING an error occurred reading the requested
!                           global attributes.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       If a FAILURE error occurs, the netCDF file is closed.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Apr-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Read_TauProfile_GAtts( NC_Filename  , &  ! Input
                                  NC_FileID    , &  ! Input
                                  ID_Tag       , &  ! Optional output
                                  Title        , &  ! Optional output
                                  History      , &  ! Optional output
                                  Sensor_Name  , &  ! Optional output
                                  Platform_Name, &  ! Optional output
                                  Comment      , &  ! Optional output
                                  Message_Log  ) &  ! Error messaging
                                RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER,                  INTENT(IN)  :: NC_FileID
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: ID_Tag
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Sensor_Name
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Platform_Name
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_TauProfile_GAtts'
    INTEGER     , PARAMETER :: NGETGATTS = 6
    ! Local variables
    INTEGER :: Get_Status(NGETGATTS), n

    ! Set up
    Error_Status = SUCCESS
    Get_Status   = SUCCESS
    n = 0

    ! The Title
    n = n + 1
    IF ( PRESENT( Title ) ) THEN
      Get_Status(n) = Get_GAttString(TITLE_GATTNAME, Title, &
                                     Message_Log=Message_Log )
    END IF

    ! The History
    n = n + 1
    IF ( PRESENT( History ) ) THEN
      Get_Status(n) = Get_GAttString(HISTORY_GATTNAME, History, &
                                     Message_Log=Message_Log )
    END IF

    ! The Sensor_Name
    n = n + 1
    IF ( PRESENT( Sensor_Name ) ) THEN
      Get_Status(n) = Get_GAttString(SENSOR_NAME_GATTNAME, Sensor_Name, &
                                     Message_Log=Message_Log )
    END IF

    ! The Platform_Name
    n = n + 1
    IF ( PRESENT( Platform_Name ) ) THEN
      Get_Status(n) = Get_GAttString(PLATFORM_NAME_GATTNAME, Platform_Name, &
                                     Message_Log=Message_Log )
    END IF

    ! The Comment
    n = n + 1
    IF ( PRESENT( Comment ) ) THEN
      Get_Status(n) = Get_GAttString(COMMENT_GATTNAME, Comment, &
                                     Message_Log=Message_Log )
    END IF

    ! The ID_Tag
    n = n + 1
    IF ( PRESENT( ID_TAG ) ) THEN
      Get_Status(n) = Get_GAttString(ID_TAG_GATTNAME, ID_Tag, &
                                     Message_Log=Message_Log )
    END IF

    ! Check for any errors
    IF ( ANY( Get_Status /= SUCCESS ) ) Error_Status = WARNING

  CONTAINS

    FUNCTION Get_GAttString(GAttName, GAttString, Message_Log) RESULT(Error_Status)
      CHARACTER(*),           INTENT(IN)  :: GAttName
      CHARACTER(*),           INTENT(OUT) :: GAttString
      CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
      INTEGER :: Error_Status
      CHARACTER(5000) :: LongString
      GAttString = ' '
      LongString = ' '
      Error_Status = Get_netCDF_Attribute( NC_FileID, &
                                           TRIM(GAttName), &
                                           LongString, &
                                           Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//TRIM(GAttName)//&
                              ' attribute from '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
      CALL Remove_NULL_Characters( LongString )
      GAttString = LongString(1:MIN( LEN(GAttString), LEN_TRIM(LongString) ))
    END FUNCTION Get_GAttString

  END FUNCTION Read_TauProfile_GAtts


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!S+
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
!                                                Sensor_ID       =Sensor_ID       , &  ! Optional Input
!                                                WMO_Satellite_ID=WMO_Satellite_ID, &  ! Optional Input
!                                                WMO_Sensor_ID   =WMO_Sensor_ID   , &  ! Optional Input
!                                                ID_Tag          =ID_Tag          , &  ! Optional input
!                                                Title           =Title           , &  ! Optional input
!                                                History         =History         , &  ! Optional input
!                                                Sensor_Name     =Sensor_Name     , &  ! Optional input
!                                                Platform_Name   =Platform_Name   , &  ! Optional input
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
!       Sensor_Name:        Character string written into the SENSOR_NAME
!                           global attribute field of the netCDF TauProfile
!                           file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Platform_Name:      Character string written into the PLATFORM_NAME
!                           global attribute field of the netCDF TauProfile
!                           file.
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
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Apr-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Create_TauProfile_netCDF( NC_Filename      , &  ! Input
                                     Level_Pressure   , &  ! Input
                                     Channel_List     , &  ! Input
                                     Angle_List       , &  ! Input
                                     Profile_List     , &  ! Input
                                     Molecule_Set_List, &  ! Input
                                     Sensor_ID        , &  ! Optional Input
                                     WMO_Satellite_ID , &  ! Optional Input
                                     WMO_Sensor_ID    , &  ! Optional Input
                                     ID_Tag           , &  ! Optional input
                                     Title            , &  ! Optional input
                                     History          , &  ! Optional input
                                     Sensor_Name      , &  ! Optional input
                                     Platform_Name    , &  ! Optional input
                                     Comment          , &  ! Optional input
                                     RCS_Id           , &  ! Revision control
                                     Message_Log      ) &  ! Error messaging
                                   RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)  :: NC_Filename
    REAL(fp)              , INTENT(IN)  :: Level_Pressure(:)
    INTEGER               , INTENT(IN)  :: Channel_List(:)
    REAL(fp)              , INTENT(IN)  :: Angle_List(:)
    INTEGER               , INTENT(IN)  :: Profile_List(:)
    INTEGER               , INTENT(IN)  :: Molecule_Set_List(:)
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Sensor_ID
    INTEGER     , OPTIONAL, INTENT(IN)  :: WMO_Satellite_ID
    INTEGER     , OPTIONAL, INTENT(IN)  :: WMO_Sensor_ID
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: ID_Tag
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Sensor_Name
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Platform_Name
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Create_TauProfile_netCDF'
    ! Local variables
    INTEGER :: NC_FileID
    INTEGER :: NF90_Status
    INTEGER :: Status1, Status2, Status3
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
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! Check input
    !
    ! The level pressure input
    n_Levels = SIZE( Level_Pressure )
    n_Layers = n_Levels - 1
    IF ( n_Layers < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'LEVEL_PRESSURE array must have at least 2-elements.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
    IF ( ANY( Level_Pressure < ZERO ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid LEVEL_PRESSURE value found.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! The Channel input
    n_Channels = SIZE( Channel_List )
    IF ( n_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'CHANNEL_LIST array must be non-zero size.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
    IF ( ANY( Channel_List < 1 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid CHANNEL_LIST value found.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! The Angle input
    n_Angles = SIZE( Angle_List )
    IF ( n_Angles < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'ANGLE_LIST array must be non-zero size.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
    IF ( ANY( ABS(Angle_List) > ANGLE_LIMIT ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid ANGLE_LIST value found.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! The Profile input
    n_Profiles = SIZE( Profile_List )
    IF ( n_Profiles < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'PROFILE_LIST array must be non-zero size.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
    IF ( ANY( Profile_List < 1 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid PROFILE_LIST value found.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! The molecule set input
    n_Molecule_Sets = SIZE( Molecule_Set_List )
    IF ( n_Molecule_Sets < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'MOLECULE_SET_LIST array must be non-zero size.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
    IF ( ANY( Molecule_Set_List < 1 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid MOLECULE_SET_LIST value found.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! Create the data file
    NF90_Status = NF90_CREATE( NC_Filename, &
                               NF90_CLOBBER, &
                               NC_FileID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error creating '//TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! Define the dimensions
    !
    ! The Sensor_Id string length
    Error_Status = Def_Dim(STRLEN_DIMNAME, LEN(TPDummy%Sensor_ID), StrLen_DimID )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! The number of levels
    Error_Status = Def_Dim(LEVEL_DIMNAME, n_Levels, Level_DimID )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! The number of layers
    Error_Status = Def_Dim(LAYER_DIMNAME, n_Layers, Layer_DimID )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! The number of Channels
    Error_Status = Def_Dim(CHANNEL_DIMNAME, n_Channels, Channel_DimID )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! The number of Angles
    Error_Status = Def_Dim(ANGLE_DIMNAME, n_Angles, Angle_DimID )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! The number of Profiles
    Error_Status = Def_Dim(PROFILE_DIMNAME, n_Profiles, Profile_DimID )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! The number of molecule sets
    Error_Status = Def_Dim(MOLECULE_SET_DIMNAME, NF90_UNLIMITED, Molecule_Set_DimID )
    IF ( Error_Status /= SUCCESS ) RETURN


    ! Write the global attributes
    Error_Status = Write_TauProfile_GAtts( NC_Filename                , &
                                           NC_FileID                  , &
                                           ID_Tag       =ID_Tag       , &
                                           Title        =Title        , &
                                           History      =History      , &
                                           Sensor_Name  =Sensor_Name  , &
                                           Platform_Name=Platform_Name, &
                                           Comment      =Comment      , &
                                           Message_Log  =Message_Log    )
    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing global attributes to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! Define the variables
    !
    ! Define the sensor ID
    Error_Status = Def_Var(SENSOR_ID_VARNAME  , &
                           SENSOR_ID_TYPE     , &
                           SENSOR_ID_LONGNAME , &
                           SENSOR_ID_DESC     , &
                           SENSOR_ID_UNITS    , &
                           DimIDs=(/StrLen_DimID/) )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Define the WMO satellite ID
    Error_Status = Def_Var(WMO_SATELLITE_ID_VARNAME  , &
                           WMO_SATELLITE_ID_TYPE     , &
                           WMO_SATELLITE_ID_LONGNAME , &
                           WMO_SATELLITE_ID_DESC     , &
                           WMO_SATELLITE_ID_UNITS      )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Define the WMO sensor ID
    Error_Status = Def_Var(WMO_SENSOR_ID_VARNAME  , &
                           WMO_SENSOR_ID_TYPE     , &
                           WMO_SENSOR_ID_LONGNAME , &
                           WMO_SENSOR_ID_DESC     , &
                           WMO_SENSOR_ID_UNITS      )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Define the level pressure
    Error_Status = Def_Var(LEVEL_PRESSURE_VARNAME  , &
                           LEVEL_PRESSURE_TYPE     , &
                           LEVEL_PRESSURE_LONGNAME , &
                           LEVEL_PRESSURE_DESC     , &
                           LEVEL_PRESSURE_UNITS    , &
                           DimIDs=(/Level_DimID/)    )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Define the channel list
    Error_Status = Def_Var(CHANNEL_LIST_VARNAME  , &
                           CHANNEL_LIST_TYPE     , &
                           CHANNEL_LIST_LONGNAME , &
                           CHANNEL_LIST_DESC     , &
                           CHANNEL_LIST_UNITS    , &
                           DimIDs=(/Channel_DimID/) )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Define the angle list
    Error_Status = Def_Var(ANGLE_LIST_VARNAME  , &
                           ANGLE_LIST_TYPE     , &
                           ANGLE_LIST_LONGNAME , &
                           ANGLE_LIST_DESC     , &
                           ANGLE_LIST_UNITS    , &
                           DimIDs=(/Angle_DimID/) )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Define the profile list
    Error_Status = Def_Var(PROFILE_LIST_VARNAME  , &
                           PROFILE_LIST_TYPE     , &
                           PROFILE_LIST_LONGNAME , &
                           PROFILE_LIST_DESC     , &
                           PROFILE_LIST_UNITS    , &
                           DimIDs=(/Profile_DimID/) )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Define the molecule set list
    Error_Status = Def_Var(MOLECULE_SET_LIST_VARNAME  , &
                           MOLECULE_SET_LIST_TYPE     , &
                           MOLECULE_SET_LIST_LONGNAME , &
                           MOLECULE_SET_LIST_DESC     , &
                           MOLECULE_SET_LIST_UNITS    , &
                           DimIDs=(/Molecule_Set_DimID/) )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Define the transmittance
    Error_Status = Def_Var(TRANSMITTANCE_VARNAME  , &
                           TRANSMITTANCE_TYPE     , &
                           TRANSMITTANCE_LONGNAME , &
                           TRANSMITTANCE_DESC     , &
                           TRANSMITTANCE_UNITS    , &
                           DimIDs=(/ Layer_DimID       , &
                                     Channel_DimID     , &
                                     Angle_DimID       , &
                                     Profile_DimID     , &
                                     Molecule_Set_DimID /) )
    IF ( Error_Status /= SUCCESS ) RETURN


    ! Take netCDF file out of define mode
    NF90_Status = NF90_ENDDEF( NC_FileID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error taking file '//TRIM( NC_Filename )// &
                            ' out of define mode - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! Write the supplied variable data
    !
    ! Sensor ID
    IF ( PRESENT( Sensor_ID ) ) THEN
      Error_Status = Put_netCDF_Variable( NC_FileID, &
                                          SENSOR_ID_VARNAME, &
                                          Sensor_ID )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//SENSOR_ID_VARNAME//&
                              ' to '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF
    END IF

    ! WMO Satellite ID
    IF ( PRESENT( WMO_Satellite_ID ) ) THEN
      Error_Status = Put_netCDF_Variable( NC_FileID, &
                                          WMO_SATELLITE_ID_VARNAME, &
                                          WMO_Satellite_ID )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//WMO_SATELLITE_ID_VARNAME//&
                              ' to '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF
    END IF

    ! WMO Sensor ID
    IF ( PRESENT( WMO_Sensor_ID ) ) THEN
      Error_Status = Put_netCDF_Variable( NC_FileID, &
                                          WMO_SENSOR_ID_VARNAME, &
                                          WMO_Sensor_ID )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//WMO_SENSOR_ID_VARNAME//&
                              ' to '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF
    END IF

    ! Level pressure
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        LEVEL_PRESSURE_VARNAME, &
                                        Level_Pressure )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//LEVEL_PRESSURE_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! Channel list
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        CHANNEL_LIST_VARNAME, &
                                        Channel_List )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//CHANNEL_LIST_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! Angle list
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        ANGLE_LIST_VARNAME, &
                                        Angle_List )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//ANGLE_LIST_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! Profile list
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        PROFILE_LIST_VARNAME, &
                                        Profile_List )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//PROFILE_LIST_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! Molecule set list
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        MOLECULE_SET_LIST_VARNAME, &
                                        Molecule_Set_List )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//MOLECULE_SET_LIST_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! Close the file
    Close_Status = Close_TauProfile_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauProfile data file '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


  CONTAINS


    FUNCTION Def_Dim(DimName, DimSize, DimID) RESULT(Error_Status)
      CHARACTER(*), INTENT(IN)  :: DimName
      INTEGER,      INTENT(IN)  :: DimSize
      INTEGER,      INTENT(OUT) :: DimID
      INTEGER :: Error_Status
      INTEGER :: NF90_Status
      Error_Status = SUCCESS
      NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                  TRIM(DimName), &
                                  DimSize, &
                                  DimID )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error defining the '//TRIM(DimName)//' dimension in '// &
                              TRIM( NC_Filename )//' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
      END IF
    END FUNCTION Def_Dim


    FUNCTION Def_Var( VarName    , &
                      VarType    , &
                      LongName   , &
                      Description, &
                      Units      , &
                      DimIds     ) &
                    RESULT(Error_Status)
      ! Arguments
      CHARACTER(*),           INTENT(IN) :: VarName
      INTEGER     ,           INTENT(IN) :: VarType
      CHARACTER(*),           INTENT(IN) :: LongName
      CHARACTER(*),           INTENT(IN) :: Description
      CHARACTER(*),           INTENT(IN) :: Units
      INTEGER     , OPTIONAL, INTENT(IN) :: DimIDs(:)
      ! Function result
      INTEGER :: Error_Status
      ! Local parameters
      INTEGER, PARAMETER :: NATTS=4
      ! Loocal variables
      INTEGER :: NF90_Status
      INTEGER :: Put_Status(NATTS)

      ! Set up
      Error_Status = SUCCESS 
            
      ! Define the variable
      NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                  VarName  , &
                                  VarType  , &
                                  dimIDs=DimIDs, &
                                  varID =VarID )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error defining '//VarName//' variable in '// &
                              TRIM( NC_Filename )//' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF
  
      ! Write some attributes
      Put_Status(1) = Put_netCDF_Attribute( NC_FileID, &
                                            LONGNAME_ATTNAME, &
                                            LongName, &
                                            Variable_Name=VarName )
      Put_Status(2) = Put_netCDF_Attribute( NC_FileID, &
                                            DESCRIPTION_ATTNAME, &
                                            Description, &
                                            Variable_Name=VarName )
      Put_Status(3) = Put_netCDF_Attribute( NC_FileID, &
                                            UNITS_ATTNAME, &
                                            Units, &
                                            Variable_Name=VarName )
      ! The following yukness is because
      ! I don't want to overload.
      SELECT CASE(VarName)
        CASE (LEVEL_PRESSURE_VARNAME,&
              ANGLE_LIST_VARNAME, &
              TRANSMITTANCE_VARNAME)
          Put_Status(4) = Put_netCDF_Attribute( NC_FileID, &
                                                FILLVALUE_ATTNAME, &
                                                FP_FILLVALUE, &
                                                Variable_Name=VarName )
        CASE (CHANNEL_LIST_VARNAME, &
              PROFILE_LIST_VARNAME, &
              MOLECULE_SET_LIST_VARNAME)
          Put_Status(4) = Put_netCDF_Attribute( NC_FileID, &
                                                FILLVALUE_ATTNAME, &
                                                IP_FILLVALUE, &
                                                Variable_Name=VarName )
        CASE (SENSOR_ID_VARNAME)
          Put_Status(4) = SUCCESS
!          Put_Status(4) = Put_netCDF_Attribute( NC_FileID, &
!                                                FILLVALUE_ATTNAME, &
!                                                SENSOR_ID_FILLVALUE, &
!                                                Variable_Name=VarName )
        CASE (WMO_SATELLITE_ID_VARNAME)
          Put_Status(4) = Put_netCDF_Attribute( NC_FileID, &
                                                FILLVALUE_ATTNAME, &
                                                WMO_SATELLITE_ID_FILLVALUE, &
                                                Variable_Name=VarName )
        CASE (WMO_SENSOR_ID_VARNAME)
          Put_Status(4) = Put_netCDF_Attribute( NC_FileID, &
                                                FILLVALUE_ATTNAME, &
                                                WMO_SENSOR_ID_FILLVALUE, &
                                                Variable_Name=VarName )
      END SELECT
      
      ! Check attribute write errors
      IF ( ANY(Put_Status /= SUCCESS) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//VarName//&
                              ' variable attributes to '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF
    END FUNCTION Def_Var

  END FUNCTION Create_TauProfile_netCDF


!------------------------------------------------------------------------------
!S+
! NAME:
!       Modify_TauProfile_GAtts
!
! PURPOSE:
!       Function to modify the global attributes in an existing netCDF format
!       TauProfile data file.
!
! CALLING SEQUENCE:
!       Error_Status = Modify_TauProfile_GAtts( NC_Filename                  , &  ! Input
!                                               ID_Tag        = ID_Tag       , &  ! Optional Input
!                                               Title         = Title        , &  ! Optional Input
!                                               History       = History      , &  ! Optional Input
!                                               Sensor_Name   = Sensor_Name  , &  ! Optional Input
!                                               Platform_Name = Platform_Name, &  ! Optional Input
!                                               Comment       = Comment      , &  ! Optional Input
!                                               RCS_Id        = RCS_Id       , &  ! Revision control
!                                               Message_Log   = Message_Log    )  ! Error messaging
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
!       Sensor_Name:      Character string to write into the SENSOR_NAME global
!                         attribute field of the netCDF TauProfile file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Platform_Name:    Character string to write into the PLATFORM_NAME global
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
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Jul-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Modify_TauProfile_GAtts( NC_Filename  , &  ! Input
                                    ID_Tag       , &  ! Optional Input
                                    Title        , &  ! Optional Input
                                    History      , &  ! Optional Input
                                    Sensor_Name  , &  ! Optional Input
                                    Platform_Name, &  ! Optional Input
                                    Comment      , &  ! Optional Input
                                    RCS_Id       , &  ! Revision control
                                    Message_Log  ) &  ! Error messaging
                                  RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: ID_Tag
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Sensor_Name
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Platform_Name
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Modify_TauProfile_GAtts'
    ! Function variables
    INTEGER :: NC_FileID
    INTEGER :: NF90_Status
    INTEGER :: Close_Status

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! Open the file
    Error_Status = Open_TauProfile_netCDF( TRIM( NC_Filename ), &
                                           NC_FileID, &
                                           Mode = 'READWRITE' )
    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening netCDF TauProfile data file '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! Put the file in define mode
    NF90_Status = NF90_REDEF( NC_FileID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error putting '//TRIM( NC_Filename )//' in DEFINE mode - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! Write the global attributes
    Error_Status = Write_TauProfile_GAtts( TRIM( NC_Filename )        , &
                                           NC_FileID                  , &
                                           ID_Tag       =ID_Tag       , &
                                           Title        =Title        , &
                                           History      =History      , &
                                           Sensor_Name  =Sensor_Name  , &
                                           Platform_Name=Platform_Name, &
                                           Comment      =Comment      , &
                                           Message_Log  =Message_Log    )
    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing global attribute to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! Close the file
    Close_Status = Close_TauProfile_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauProfile data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

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
!                                                 Sensor_Name      =Sensor_Name      , &  ! Optional output
!                                                 Platform_Name    =Platform_Name    , &  ! Optional output
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
!       Sensor_Name:        Character string written into the SENSOR_NAME global
!                           attribute field of the netCDF TauProfile file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Platform_Name:      Character string written into the PLATFORM_NAME global
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
! RESTRICTIONS:
!       To successfully return any of the CHANNEL, ANGLE, PROFILE, or
!       MOLECULE_SET list arrays, the dummy arguments must have the same size
!       as the dataset in the netCDF file. Thus, two calls to this routine are
!       required. First, the various dimensions should be read and used
!       either to allocate the required data array of the correct size, or
!       to subset an existing array in the call.
!
!------------------------------------------------------------------------------

  FUNCTION Inquire_TauProfile_netCDF( NC_Filename      , &  ! Input
                                      n_Layers         , &  ! Optional output
                                      n_Channels       , &  ! Optional output
                                      n_Angles         , &  ! Optional output
                                      n_Profiles       , &  ! Optional output
                                      n_Molecule_Sets  , &  ! Optional output
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
                                      Sensor_Name      , &  ! Optional output
                                      Platform_Name    , &  ! Optional output
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
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Sensor_Name
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Platform_Name
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
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Open the file
    Error_Status = Open_TauProfile_netCDF( TRIM( NC_Filename ), &
                                           NC_FileID, &
                                           Mode = 'READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF TauProfile data file '//&
                TRIM( NC_Filename )
      GOTO 2000
    END IF

    ! Get the dimensions
    !
    ! The number of layers
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         LAYER_DIMNAME, &
                                         k, &
                                         Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//LAYER_DIMNAME//&
                ' dimension from '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The number of Channels
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         CHANNEL_DIMNAME, &
                                         l, &
                                         Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//CHANNEL_DIMNAME//&
                ' dimension from '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The number of Angles
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         ANGLE_DIMNAME, &
                                         i, &
                                         Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//ANGLE_DIMNAME//&
                ' dimension from '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The number of Profiles
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         PROFILE_DIMNAME, &
                                         m, &
                                         Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//PROFILE_DIMNAME//&
                ' dimension from '//TRIM( NC_Filename )
      GOTO 1000
    END IF
    ! The number of molecule sets
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         MOLECULE_SET_DIMNAME, &
                                         j, &
                                         Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//MOLECULE_SET_DIMNAME//&
                ' dimension from '//TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! Set the dimension return values
    IF ( PRESENT( n_Layers        ) ) n_Layers        = k
    IF ( PRESENT( n_Channels      ) ) n_Channels      = l
    IF ( PRESENT( n_Angles        ) ) n_Angles        = i
    IF ( PRESENT( n_Profiles      ) ) n_Profiles      = m
    IF ( PRESENT( n_Molecule_Sets ) ) n_Molecule_Sets = j

    ! Get the sensor Ids
    !
    ! The Sensor ID
    IF ( PRESENT( Sensor_ID ) ) THEN
      Error_Status = Get_netCDF_Variable( NC_FileID, &
                                          SENSOR_ID_VARNAME, &
                                          Sensor_ID )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading '//SENSOR_ID_VARNAME//&
                  ' data from '//TRIM( NC_Filename )
        GOTO 1000
      END IF
    END IF
    ! The WMO satellite ID
    IF ( PRESENT( WMO_Satellite_ID ) ) THEN
      Error_Status = Get_netCDF_Variable( NC_FileID, &
                                          WMO_SATELLITE_ID_VARNAME, &
                                          WMO_Satellite_ID )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading '//WMO_SATELLITE_ID_VARNAME//&
                  ' data from '//TRIM( NC_Filename )
        GOTO 1000
      END IF
    END IF
    ! The WMO Sensor ID
    IF ( PRESENT( WMO_Sensor_ID ) ) THEN
      Error_Status = Get_netCDF_Variable( NC_FileID, &
                                          WMO_SENSOR_ID_VARNAME, &
                                          WMO_Sensor_ID )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading '//WMO_SENSOR_ID_VARNAME//&
                  ' data from '//TRIM( NC_Filename )
        GOTO 1000
      END IF
    END IF

    ! Get the dimension list data
    !
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
                  ' data from '//TRIM( NC_Filename )
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
                  ' data from '//TRIM( NC_Filename )
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
                  ' data from '//TRIM( NC_Filename )
        GOTO 1000
      END IF
    END IF
    ! The Profile list
    ! The Angle list
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
                  ' data from '//TRIM( NC_Filename )
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
                  ' data from '//TRIM( NC_Filename )
        GOTO 1000
      END IF
    END IF

    ! Get the global attributes
    Error_Status = Read_TauProfile_GAtts( TRIM( NC_Filename )        , &
                                          NC_FileID                  , &
                                          ID_Tag       =ID_Tag       , &
                                          Title        =Title        , &
                                          History      =History      , &
                                          Sensor_Name  =Sensor_Name  , &
                                          Platform_Name=Platform_Name, &
                                          Comment      =Comment      , &
                                          Message_Log  =Message_Log    )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading global attribute from '//TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! Close the file
    Close_Status = Close_TauProfile_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauProfile data file '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

    RETURN


    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

    1000 CONTINUE
    Close_Status = Close_TauProfile_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing input file during error cleanup.'

    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
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
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF

    ! Read the dimension values
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              n_Layers        = k, &
                                              n_Channels      = l, &
                                              n_Angles        = i, &
                                              n_Profiles      = m, &
                                              n_Molecule_Sets = j, &
                                              Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining TauProfile dimensions from '//&
                TRIM( NC_Filename )
      GOTO 3000
    END IF

    ! Check the TauProfile dimension values
    IF ( SIZE(Tau, DIM=1) /= k ) THEN
      Message = 'Tau N_LAYERS array size different from netCDF definition.'
      GOTO 3000
    END IF

    ! Allocate the index list arrays
    ALLOCATE( Channel_List(l), Angle_List(i), &
              Profile_List(m), Molecule_Set_List(j), &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '( "Error allocating index list arrays. STAT = ", i0 )' ) &
                      Allocate_Status
      GOTO 3000
    END IF

    ! Fill the index list arrays
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename                        , &
                                              Channel_List     =Channel_List     , &
                                              Angle_List       =Angle_List       , &
                                              Profile_List     =Profile_List     , &
                                              Molecule_Set_List=Molecule_Set_List, &
                                              Message_Log      =Message_Log        )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining index list data from '//&
                TRIM( NC_Filename )
      GOTO 2000
    END IF

    ! Now determine the index values for the input Tau data
    Channel_Index      = Get_Index( Channel_List, Channel )
    Angle_Index        = Get_Index( Angle_List, Angle )
    Profile_Index      = Get_Index( Profile_List, Profile )
    Molecule_Set_Index = Get_Index( Molecule_Set_List, Molecule_Set )

    ! Check the index list values
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
    Error_Status = Open_TauProfile_netCDF( TRIM( NC_Filename ), &
                                           NC_FileID, &
                                           Mode = 'READWRITE' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF TauProfile data file '//&
                TRIM( NC_Filename )
      GOTO 2000
    END IF

    ! Write the molecule list data
    IF ( Molecule_Set_Index > j ) THEN
      Error_Status = Put_netCDF_Variable( NC_FileID, &
                                          MOLECULE_SET_LIST_VARNAME, &
                                          Molecule_Set, &
                                          START = (/ Molecule_Set_Index /) )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing molecule set value ", i0, &
                          &" to ", a, "." )' ) &
                        Molecule_Set, &
                        TRIM( NC_Filename )
        GOTO 1000
      END IF
    END IF

    ! Write the transmittance data
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
                      TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! Deallocate the list arrays
    DEALLOCATE(Channel_List, Angle_List, Profile_List, Molecule_Set_List, &
               STAT=Allocate_Status)
    IF ( Allocate_Status /= 0 ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating list arrays.', &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    ! Close the file
    Close_Status = Close_TauProfile_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauProfile data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    ! Output an info message
    IF ( Noisy ) THEN
      WRITE(Message,'("Tau: ", &
                     &"N_LAYERS=",i0,2x,&
                     &"for channel ",i0,&
                     &", sec(angle) ",f4.2,&
                     &", profile ",i0,&
                     &" and molecule set ",i0 )' ) &
                     k,Channel,Angle,Profile,Molecule_Set
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( NC_Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

    RETURN



    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

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
                          TRIM( Message ), &
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
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF

    ! Read the dimension values
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              n_Layers        = k, &
                                              n_Channels      = l, &
                                              n_Angles        = i, &
                                              n_Profiles      = m, &
                                              n_Molecule_Sets = j, &
                                              Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining TauProfile dimensions from '//&
                TRIM( NC_Filename )
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
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              Angle_List       =Angle_List       , &
                                              Profile_List     =Profile_List     , &
                                              Molecule_Set_List=Molecule_Set_List, &
                                              Message_Log      =Message_Log        )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining index list data from '//&
                TRIM( NC_Filename )
      GOTO 2000
    END IF

    ! Now determine the index values for the input Tau data
    Angle_Index        = Get_Index( Angle_List, Angle )
    Profile_Index      = Get_Index( Profile_List, Profile )
    Molecule_Set_Index = Get_Index( Molecule_Set_List, Molecule_Set )

    IF ( Angle_Index < 1 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Invalid ANGLE_LIST array index value, ", i0, &
                        &" for angle ", f5.2 )' ) &
                      Angle_Index, Angle
      GOTO 2000
    END IF
    ! Check the index list values
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
    Error_Status = Open_TauProfile_netCDF( TRIM( NC_Filename ), &
                                           NC_FileID, &
                                           Mode = 'READWRITE' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF TauProfile data file '//&
                TRIM( NC_Filename )
      GOTO 2000
    END IF

    ! Write the molecule list data
    IF ( Molecule_Set_Index > j ) THEN
      Error_Status = Put_netCDF_Variable( NC_FileID, &
                                          MOLECULE_SET_LIST_VARNAME, &
                                          Molecule_Set, &
                                          START = (/ Molecule_Set_Index /) )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing molecule set value ", i0, &
                          &" to ", a, "." )' ) &
                        Molecule_Set, &
                        TRIM( NC_Filename )
        GOTO 1000
      END IF
    END IF

    ! Write the transmittance data
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
                      TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! Deallocate the list arrays
    DEALLOCATE(Angle_List, Profile_List, Molecule_Set_List, STAT=Allocate_Status)
    IF ( Allocate_Status /= 0 ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating list arrays.', &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    ! Close the file
    Close_Status = Close_TauProfile_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauProfile data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    ! Output an info message
    IF ( Noisy ) THEN
      WRITE(Message,'("Tau: ", &
                     &"N_LAYERS=",i0,2x,&
                     &"N_CHANNELS=",i0,2x,&
                     &"for sec(angle) ",f4.2,&
                     &", profile ",i0," and molecule set ",i0 )' ) &
                     k,l,Angle,Profile,Molecule_Set
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( NC_Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

    RETURN



    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

    1000 CONTINUE
    Close_Status = Close_TauProfile_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing output file during error cleanup.'

    2000 CONTINUE
    DEALLOCATE(Angle_List, Profile_List, Molecule_Set_List, STAT=Allocate_Status)
    IF ( Allocate_Status /= 0 ) &
      Message = TRIM(Message)//&
                '; Error deallocating list data arrays during error cleanup.'

    3000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Write_TauArray_rank2


  FUNCTION Write_TauArray_rank3( NC_Filename , &  ! Input
                                 Tau         , &  ! Input
                                 Profile     , &  ! Input
                                 Molecule_Set, &  ! Input
                                 Quiet       , &  ! Optional input
                                 RCS_Id      , &  ! Revision control
                                 Message_Log ) &  ! Error messaging
                               RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    REAL(fp)    ,           INTENT(IN)  :: Tau(:,:,:)
    INTEGER     ,           INTENT(IN)  :: Profile
    INTEGER     ,           INTENT(IN)  :: Molecule_Set
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
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF

    ! Read the dimension values
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              n_Layers        = k, &
                                              n_Channels      = l, &
                                              n_Angles        = i, &
                                              n_Profiles      = m, &
                                              n_Molecule_Sets = j, &
                                              Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining TauProfile dimensions from '//&
                TRIM( NC_Filename )
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
    ALLOCATE( Profile_List( m ), &
              Molecule_Set_List( j ), &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '( "Error allocating index list arrays. STAT = ", i0 )' ) &
                      Allocate_Status
      GOTO 3000
    END IF

    ! Fill the index list arrays
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              Profile_List = Profile_List, &
                                              Molecule_Set_List = Molecule_Set_List, &
                                              Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining index list data from '//&
                TRIM( NC_Filename )
      GOTO 2000
    END IF

    ! Now determine the index values for the input Tau data
    Profile_Index      = Get_Index( Profile_List, Profile )
    Molecule_Set_Index = Get_Index( Molecule_Set_List, Molecule_Set )

    ! Check the index list values
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
    Error_Status = Open_TauProfile_netCDF( TRIM( NC_Filename ), &
                                           NC_FileID, &
                                           Mode = 'READWRITE' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF TauProfile data file '//&
                TRIM( NC_Filename )
      GOTO 2000
    END IF

    ! Write the molecule list data
    IF ( Molecule_Set_Index > j ) THEN
      Error_Status = Put_netCDF_Variable( NC_FileID, &
                                          MOLECULE_SET_LIST_VARNAME, &
                                          Molecule_Set, &
                                          START = (/ Molecule_Set_Index /) )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing molecule set value ", i0, &
                          &" to ", a, "." )' ) &
                        Molecule_Set, &
                        TRIM( NC_Filename )
        GOTO 1000
      END IF
    END IF

    ! Write the transmittance data
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
                      TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! Deallocate the list arrays
    DEALLOCATE(Profile_List, Molecule_Set_List, STAT=Allocate_Status)
    IF ( Allocate_Status /= 0 ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating list arrays.', &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    ! Close the file
    Close_Status = Close_TauProfile_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauProfile data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    ! Output an info message
    IF ( Noisy ) THEN
      WRITE(Message,'("Tau: ", &
                     &"N_LAYERS=",i0,2x,&
                     &"N_CHANNELS=",i0,2x,&
                     &"N_ANGLES=",i0,2x,&
                     &"for profile ",i0," and molecule set ",i0 )' ) &
                     k,l,i,Profile,Molecule_Set
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( NC_Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

    RETURN



    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

    1000 CONTINUE
    Close_Status = Close_TauProfile_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing output file during error cleanup.'

    2000 CONTINUE
    DEALLOCATE(Profile_List, Molecule_Set_List, STAT=Allocate_Status)
    IF ( Allocate_Status /= 0 ) &
      Message = TRIM(Message)//&
                '; Error deallocating list data arrays during error cleanup.'

    3000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Write_TauArray_rank3


  FUNCTION Write_TauArray_rank4( NC_Filename , &  ! Input
                                 Tau         , &  ! Input
                                 Molecule_Set, &  ! Input
                                 Quiet       , &  ! Optional input
                                 RCS_Id      , &  ! Revision control
                                 Message_Log ) &  ! Error messaging
                               RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    REAL(fp)    ,           INTENT(IN)  :: Tau(:,:,:,:)
    INTEGER     ,           INTENT(IN)  :: Molecule_Set
    INTEGER     , OPTIONAL, INTENT(IN)  :: Quiet
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
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF

    ! Read the dimension values
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              n_Layers        = k, &
                                              n_Channels      = l, &
                                              n_Angles        = i, &
                                              n_Profiles      = m, &
                                              n_Molecule_Sets = j, &
                                              Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining TauProfile dimensions from '//&
                TRIM( NC_Filename )
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
    ALLOCATE( Molecule_Set_List( j ), &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '( "Error allocating index list arrays. STAT = ", i0 )' ) &
                      Allocate_Status
      GOTO 3000
    END IF

    ! Fill the index list arrays
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              Molecule_Set_List = Molecule_Set_List, &
                                              Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining index list data from '//&
                TRIM( NC_Filename )
      GOTO 2000
    END IF

    ! Now determine the index values for the input Tau data
    Molecule_Set_Index = Get_Index( Molecule_Set_List, Molecule_Set )

    ! Check the index list value
    IF ( Molecule_Set_Index < 1 ) THEN
      Molecule_Set_Index = j + 1  ! New molecule set
    END IF

    ! Open the file
    Error_Status = Open_TauProfile_netCDF( TRIM( NC_Filename ), &
                                           NC_FileID, &
                                           Mode = 'READWRITE' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF TauProfile data file '//&
                TRIM( NC_Filename )
      GOTO 2000
    END IF

    ! Write the molecule list data
    IF ( Molecule_Set_Index > j ) THEN
      Error_Status = Put_netCDF_Variable( NC_FileID, &
                                          MOLECULE_SET_LIST_VARNAME, &
                                          Molecule_Set, &
                                          START = (/ Molecule_Set_Index /) )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing molecule set value ", i0, &
                          &" to ", a, "." )' ) &
                        Molecule_Set, &
                        TRIM( NC_Filename )
        GOTO 1000
      END IF
    END IF

    ! Write the transmittance data
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        TRANSMITTANCE_VARNAME, &
                                        Tau, &
                                        START=(/1,1,1,1,Molecule_Set_Index/), &
                                        COUNT=(/k,l,i,m,1/) )
    IF ( Error_Status/= SUCCESS ) THEN
      WRITE( Message, '( "Error writing Tau array for molecule set ", i0, &
                        &" to ", a, "." )' ) &
                      Molecule_Set, &
                      TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! Deallocate the list array
    DEALLOCATE(Molecule_Set_List, STAT=Allocate_Status)
    IF ( Allocate_Status /= 0 ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating list arrays.', &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    ! Close the file
    Close_Status = Close_TauProfile_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauProfile data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    ! Output an info message
    IF ( Noisy ) THEN
      WRITE(Message,'("Tau: ", &
                     &"N_LAYERS=",i0,2x,&
                     &"N_CHANNELS=",i0,2x,&
                     &"N_ANGLES=",i0,2x,&
                     &"N_PROFILES=",i0,2x,&
                     &"for molecule set ",i0 )' ) k,l,i,m,Molecule_Set
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( NC_Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

    RETURN



    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

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
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Write_TauArray_rank4


  FUNCTION Write_TauArray_rank5( NC_Filename,  &  ! Input
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
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF

    ! Read the dimension values
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              n_Layers        = k, &
                                              n_Channels      = l, &
                                              n_Angles        = i, &
                                              n_Profiles      = m, &
                                              n_Molecule_Sets = j, &
                                              Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining TauProfile dimensions from '//&
                TRIM( NC_Filename )
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
    Error_Status = Open_TauProfile_netCDF( TRIM( NC_Filename ), &
                                           NC_FileID, &
                                           Mode = 'READWRITE' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF TauProfile data file '//&
                TRIM( NC_Filename )
      GOTO 2000
    END IF

    ! Write the transmittance data
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        TRANSMITTANCE_VARNAME, &
                                        Tau )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error writing Tau array to ", a, "." )' ) &
                      TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! Close the file
    Close_Status = Close_TauProfile_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauProfile data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    ! Output an info message
    IF ( Noisy ) THEN
      WRITE(Message,'("Tau: ", &
                     &"N_LAYERS=",i0,2x,&
                     &"N_CHANNELS=",i0,2x,&
                     &"N_ANGLES=",i0,2x,&
                     &"N_PROFILES=",i0,2x,&
                     &"N_MOLECULE_SETS=",i0 )' ) k,l,i,m,j
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( NC_Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

    RETURN


    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

    1000 CONTINUE
    Close_Status = Close_TauProfile_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing output file during error cleanup.'

    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Write_TauArray_rank5


  FUNCTION Write_TauProfile_type( NC_Filename , &  ! Input
                                  TauProfile  , &  ! Input
                                  Quiet       , &  ! Optional input
                                  RCS_Id      , &  ! Revision control
                                  Message_Log ) &  ! Error messaging
                                RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    TYPE(TauProfile_type) , INTENT(IN)  :: TauProfile
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
    INTEGER :: NC_FileID
    INTEGER :: Close_Status
    INTEGER :: k, l, i, m, j

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF

    ! Check structure association
    IF ( .NOT. Associated_TauProfile( TauProfile ) ) THEN
      Message = 'Some or all INPUT TauProfile pointer members are NOT associated.'
      GOTO 2000
    END IF

    ! Read the dimension values
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              n_Layers        = k, &
                                              n_Channels      = l, &
                                              n_Angles        = i, &
                                              n_Profiles      = m, &
                                              n_Molecule_Sets = j, &
                                              Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining TauProfile dimensions from '//TRIM( NC_Filename )
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
    Error_Status = Open_TauProfile_netCDF( TRIM( NC_Filename ), &
                                           NC_FileID, &
                                           Mode = 'READWRITE' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF TauProfile data file '//TRIM( NC_Filename )
      GOTO 2000
    END IF

    ! Write the transmittance data
    Error_Status = Put_netCDF_Variable( NC_FileID,   &
                                        TRANSMITTANCE_VARNAME, &
                                        TauProfile%Tau )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error writing Tau array to ", a, "." )' ) &
                      TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! Close the file
    Close_Status = Close_TauProfile_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauProfile data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    ! Output an info message
    IF ( Noisy ) THEN
      CALL Information_TauProfile( TauProfile, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( NC_Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

    RETURN


    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

    1000 CONTINUE
    Close_Status = Close_TauProfile_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing output file during error cleanup.'

    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Write_TauProfile_type





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

  FUNCTION Read_TauArray_rank1( NC_Filename,  &  ! Input
                                Channel,      &  ! Input
                                Angle,        &  ! Input
                                Profile,      &  ! Input
                                Molecule_Set, &  ! Input
                                Tau,          &  ! Output
                                Quiet,        &  ! Optional input
                                RCS_Id,       &  ! Revision control
                                Message_Log ) &  ! Error messaging
                              RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER,                INTENT(IN)  :: Channel
    REAL(fp),               INTENT(IN)  :: Angle
    INTEGER,                INTENT(IN)  :: Profile
    INTEGER,                INTENT(IN)  :: Molecule_Set
    REAL(fp), DIMENSION(:), INTENT(OUT) :: Tau
    INTEGER,      OPTIONAL, INTENT(IN)  :: Quiet
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
    INTEGER,  DIMENSION(:), ALLOCATABLE :: Channel_List
    REAL(fp), DIMENSION(:), ALLOCATABLE :: Angle_List
    INTEGER,  DIMENSION(:), ALLOCATABLE :: Profile_List
    INTEGER,  DIMENSION(:), ALLOCATABLE :: Molecule_Set_List
    INTEGER :: Channel_Index
    INTEGER :: Angle_Index
    INTEGER :: Profile_Index
    INTEGER :: Molecule_Set_Index
    INTEGER :: k, l, i, m, j

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF

    ! Read the dimension values
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              n_Layers        = k, &
                                              n_Channels      = l, &
                                              n_Angles        = i, &
                                              n_Profiles      = m, &
                                              n_Molecule_Sets = j, &
                                              Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining TauProfile dimensions from '//&
                TRIM( NC_Filename )
      GOTO 3000
    END IF

    ! Check the TauProfile dimension value
    IF ( SIZE(Tau) /= k ) THEN
      Message = 'Tau N_LAYERS array size different from netCDF definition.'
      GOTO 3000
    END IF

    ! Allocate the index list arrays
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
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              Channel_List      = Channel_List, &
                                              Angle_List        = Angle_List, &
                                              Profile_List      = Profile_List, &
                                              Molecule_Set_List = Molecule_Set_List, &
                                              Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining index list data from '//&
                TRIM( NC_Filename )
      GOTO 2000
    END IF

    ! Now determine the index values for the input Tau data
    Channel_Index      = Get_Index( Channel_List, Channel )
    Angle_Index        = Get_Index( Angle_List, Angle )
    Profile_Index      = Get_Index( Profile_List, Profile )
    Molecule_Set_Index = Get_Index( Molecule_Set_List, Molecule_Set )

    ! Check the index list value
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
    Error_Status = Open_TauProfile_netCDF( TRIM( NC_Filename ), &
                                           NC_FileID, &
                                           Mode = 'READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF TauProfile data file '//&
                TRIM( NC_Filename )
      GOTO 2000
    END IF

    ! Read the transmittance data
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        TRANSMITTANCE_VARNAME, &
                                        Tau, &
                                        START=(/1,Channel_Index, &
                                                  Angle_Index, &
                                                  Profile_Index, &
                                                  Molecule_Set_Index/), &
                                        COUNT=(/k,1,1,1,1/) )

    IF ( Error_Status/= SUCCESS ) THEN
      WRITE( Message, '( "Error reading Tau vector for Channel ", i0, &
                        &", Angle secant ", f5.2, ", Profile ", i0, &
                        &", and molecule set ", i0, " from ", a, "." )' ) &
                      Channel, Angle, Profile, Molecule_Set, &
                      TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! Deallocate the list array
    DEALLOCATE(Channel_List, Angle_List, Profile_List, Molecule_Set_List, &
               STAT=Allocate_Status)
    IF ( Allocate_Status /= 0 ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating list arrays.', &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    ! Close the file
    Close_Status = Close_TauProfile_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauProfile data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    ! Output an info message
    IF ( Noisy ) THEN
      WRITE(Message,'("Tau: ", &
                     &"N_LAYERS=",i0,2x,&
                     &"for channel ",i0,&
                     &", sec(angle) ",f4.2,&
                     &", profile ",i0,&
                     &" and molecule set ",i0 )' ) &
                     k,Channel,Angle,Profile,Molecule_Set
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( NC_Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

    RETURN



    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

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
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Read_TauArray_rank1


  FUNCTION Read_TauArray_rank2( NC_Filename,  &  ! Input
                                Angle,        &  ! Input
                                Profile,      &  ! Input
                                Molecule_Set, &  ! Input
                                Tau,          &  ! Output
                                Quiet,        &  ! Optional input
                                RCS_Id,       &  ! Revision control
                                Message_Log ) &  ! Error messaging
                              RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),             INTENT(IN)  :: NC_Filename
    REAL(fp),                 INTENT(IN)  :: Angle
    INTEGER,                  INTENT(IN)  :: Profile
    INTEGER,                  INTENT(IN)  :: Molecule_Set
    REAL(fp), DIMENSION(:,:), INTENT(OUT) :: Tau
    INTEGER,        OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*),   OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*),   OPTIONAL, INTENT(IN)  :: Message_Log
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
    REAL(fp), DIMENSION(:), ALLOCATABLE :: Angle_List
    INTEGER,  DIMENSION(:), ALLOCATABLE :: Profile_List
    INTEGER,  DIMENSION(:), ALLOCATABLE :: Molecule_Set_List
    INTEGER :: Angle_Index
    INTEGER :: Profile_Index
    INTEGER :: Molecule_Set_Index
    INTEGER :: k, l, i, m, j

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF

    ! Read the dimension values
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              n_Layers        = k, &
                                              n_Channels      = l, &
                                              n_Angles        = i, &
                                              n_Profiles      = m, &
                                              n_Molecule_Sets = j, &
                                              Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining TauProfile dimensions from '//&
                TRIM( NC_Filename )
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
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              Angle_List        = Angle_List, &
                                              Profile_List      = Profile_List, &
                                              Molecule_Set_List = Molecule_Set_List, &
                                              Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining index list data from '//&
                TRIM( NC_Filename )
      GOTO 2000
    END IF

    ! Now determine the index values for the input Tau data
    Angle_Index        = Get_Index( Angle_List, Angle )
    Profile_Index      = Get_Index( Profile_List, Profile )
    Molecule_Set_Index = Get_Index( Molecule_Set_List, Molecule_Set )

    ! Check the index list value
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
    Error_Status = Open_TauProfile_netCDF( TRIM( NC_Filename ), &
                                           NC_FileID, &
                                           Mode = 'READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF TauProfile data file '//&
                TRIM( NC_Filename )
      GOTO 2000
    END IF

    ! Read the transmittance data
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
                      TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! Deallocate the list array
    DEALLOCATE(Angle_List, Profile_List, Molecule_Set_List, STAT=Allocate_Status)
    IF ( Allocate_Status /= 0 ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating list arrays.', &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    ! Close the file
    Close_Status = Close_TauProfile_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauProfile data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    ! Output an info message
    IF ( Noisy ) THEN
      WRITE(Message,'("Tau: ", &
                     &"N_LAYERS=",i0,2x,&
                     &"N_CHANNELS=",i0,2x,&
                     &"for sec(angle) ",f4.2,&
                     &", profile ",i0," and molecule set ",i0 )' ) &
                     k,l,Angle,Profile,Molecule_Set
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( NC_Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

    RETURN


    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

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
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Read_TauArray_rank2


  FUNCTION Read_TauArray_rank3( NC_Filename,  &  ! Input
                                Profile,      &  ! Input
                                Molecule_Set, &  ! Input
                                Tau,          &  ! Output
                                Quiet,        &  ! Optional input
                                RCS_Id,       &  ! Revision control
                                Message_Log ) &  ! Error messaging
                              RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),               INTENT(IN)  :: NC_Filename
    INTEGER,                    INTENT(IN)  :: Profile
    INTEGER,                    INTENT(IN)  :: Molecule_Set
    REAL(fp), DIMENSION(:,:,:), INTENT(OUT) :: Tau
    INTEGER,          OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*),     OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*),     OPTIONAL, INTENT(IN)  :: Message_Log
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
    INTEGER, DIMENSION(:), ALLOCATABLE :: Profile_List
    INTEGER, DIMENSION(:), ALLOCATABLE :: Molecule_Set_List
    INTEGER :: Profile_Index
    INTEGER :: Molecule_Set_Index
    INTEGER :: k, l, i, m, j

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF

    ! Read the dimension values
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              n_Layers        = k, &
                                              n_Channels      = l, &
                                              n_Angles        = i, &
                                              n_Profiles      = m, &
                                              n_Molecule_Sets = j, &
                                              Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining TauProfile dimensions from '//&
                TRIM( NC_Filename )
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
    ALLOCATE( Profile_List( m ), &
              Molecule_Set_List( j ), &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '( "Error allocating index list arrays. STAT = ", i0 )' ) &
                      Allocate_Status
      GOTO 3000
    END IF

    ! Fill the index list arrays
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              Profile_List      = Profile_List, &
                                              Molecule_Set_List = Molecule_Set_List, &
                                              Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining index list data from '//&
                TRIM( NC_Filename )
      GOTO 2000
    END IF

    ! Now determine the index values for the input Tau data
    Profile_Index      = Get_Index( Profile_List, Profile )
    Molecule_Set_Index = Get_Index( Molecule_Set_List, Molecule_Set )

    ! Check the index list value
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
    Error_Status = Open_TauProfile_netCDF( TRIM( NC_Filename ), &
                                           NC_FileID, &
                                           Mode = 'READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF TauProfile data file '//&
                TRIM( NC_Filename )
      GOTO 2000
    END IF

    ! Read the transmittance data
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
                      TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! Deallocate the list array
    DEALLOCATE(Profile_List, Molecule_Set_List, STAT=Allocate_Status)
    IF ( Allocate_Status /= 0 ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating list arrays.', &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    ! Close the file
    Close_Status = Close_TauProfile_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauProfile data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    ! Output an info message
    IF ( Noisy ) THEN
      WRITE(Message,'("Tau: ", &
                     &"N_LAYERS=",i0,2x,&
                     &"N_CHANNELS=",i0,2x,&
                     &"N_ANGLES=",i0,2x,&
                     &"for profile ",i0," and molecule set ",i0 )' ) &
                     k,l,i,Profile,Molecule_Set
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( NC_Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

    RETURN


    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

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
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Read_TauArray_rank3


  FUNCTION Read_TauArray_rank4( NC_Filename,  &  ! Input
                                Molecule_Set, &  ! Input
                                Tau,          &  ! Output
                                Quiet,        &  ! Optional input
                                RCS_Id,       &  ! Revision control
                                Message_Log ) &  ! Error messaging
                              RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),                 INTENT(IN)  :: NC_Filename
    INTEGER,                      INTENT(IN)  :: Molecule_Set
    REAL(fp), DIMENSION(:,:,:,:), INTENT(OUT) :: Tau
    INTEGER,            OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*),       OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*),       OPTIONAL, INTENT(IN)  :: Message_Log
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
    INTEGER, DIMENSION(:), ALLOCATABLE :: Molecule_Set_List
    INTEGER :: Molecule_Set_Index
    INTEGER :: k, l, i, m, j

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF

    ! Read the dimension values
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              n_Layers        = k, &
                                              n_Channels      = l, &
                                              n_Angles        = i, &
                                              n_Profiles      = m, &
                                              n_Molecule_Sets = j, &
                                              Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining TauProfile dimensions from '//&
                TRIM( NC_Filename )
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
    ALLOCATE( Molecule_Set_List( j ), &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '( "Error allocating index list arrays. STAT = ", i0 )' ) &
                      Allocate_Status
      GOTO 3000
    END IF

    ! Fill the index list arrays
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              Molecule_Set_List = Molecule_Set_List, &
                                              Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining index list data from '//&
                TRIM( NC_Filename )
      GOTO 2000
    END IF

    ! Now determine the index values for the input Tau data
    Molecule_Set_Index = Get_Index( Molecule_Set_List, Molecule_Set )

    ! Check the index list value
    IF ( Molecule_Set_Index < 1 ) THEN
      WRITE( Message, '( "Invalid MOLECULE_SET_LIST array index value, ", i0, &
                        &" for molecule set #", i0 )' ) &
                      Molecule_Set_Index, Molecule_Set
      GOTO 2000
    END IF

    ! Open the file
    Error_Status = Open_TauProfile_netCDF( TRIM( NC_Filename ), &
                                           NC_FileID, &
                                           Mode = 'READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF TauProfile data file '//&
                TRIM( NC_Filename )
      GOTO 2000
    END IF

    ! Read the transmittance data
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        TRANSMITTANCE_VARNAME, &
                                        Tau, &
                                        START=(/1,1,1,1,Molecule_Set_Index/), &
                                        COUNT=(/k,l,i,m,1/) )

    IF ( Error_Status/= SUCCESS ) THEN
      WRITE( Message, '( "Error reading Tau array for molecule set ", i0, &
                        &" from ", a, "." )' ) &
                      Molecule_Set, &
                      TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! Deallocate the list array
    DEALLOCATE(Molecule_Set_List, STAT=Allocate_Status)
    IF ( Allocate_Status /= 0 ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating list arrays.', &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    ! Close the file
    Close_Status = Close_TauProfile_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauProfile data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    ! Output an info message
    IF ( Noisy ) THEN
      WRITE(Message,'("Tau: ", &
                     &"N_LAYERS=",i0,2x,&
                     &"N_CHANNELS=",i0,2x,&
                     &"N_ANGLES=",i0,2x,&
                     &"N_PROFILES=",i0,2x,&
                     &"for molecule set ",i0 )' ) k,l,i,m,Molecule_Set
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( NC_Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

    RETURN


    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

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
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Read_TauArray_rank4


  FUNCTION Read_TauArray_rank5( NC_Filename,  &  ! Input
                                Tau,          &  ! Output
                                Quiet,        &  ! Optional input
                                RCS_Id,       &  ! Revision control
                                Message_Log ) &  ! Error messaging
                              RESULT ( Error_Status )
    CHARACTER(*),                   INTENT(IN)  :: NC_Filename
    REAL(fp), DIMENSION(:,:,:,:,:), INTENT(OUT) :: Tau
    INTEGER,              OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*),         OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*),         OPTIONAL, INTENT(IN)  :: Message_Log
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
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF

    ! Read the dimension values
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              n_Layers        = k, &
                                              n_Channels      = l, &
                                              n_Angles        = i, &
                                              n_Profiles      = m, &
                                              n_Molecule_Sets = j, &
                                              Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining TauProfile dimensions from '//&
                TRIM( NC_Filename )
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
    Error_Status = Open_TauProfile_netCDF( TRIM( NC_Filename ), &
                                           NC_FileID, &
                                           Mode = 'READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF TauProfile data file '//&
                TRIM( NC_Filename )
      GOTO 2000
    END IF

    ! Read the transmittances
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        TRANSMITTANCE_VARNAME, &
                                        Tau )
    IF ( Error_Status/= SUCCESS ) THEN
      WRITE( Message, '( "Error reading Tau array from ", a, "." )' ) &
                      TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! Close the file
    Close_Status = Close_TauProfile_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauProfile data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    ! Output an info message
    IF ( Noisy ) THEN
      WRITE(Message,'("Tau: ", &
                     &"N_LAYERS=",i0,2x,&
                     &"N_CHANNELS=",i0,2x,&
                     &"N_ANGLES=",i0,2x,&
                     &"N_PROFILES=",i0,2x,&
                     &"N_MOLECULE_SETS=",i0 )' ) k,l,i,m,j
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( NC_Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

    RETURN



    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

    1000 CONTINUE
    Close_Status = Close_TauProfile_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing input file during error cleanup.'

    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Read_TauArray_rank5


  FUNCTION Read_TauProfile_type( NC_Filename,  &   ! Input
                                 TauProfile,   &   ! Output
                                 Quiet,        &   ! Optional input
                                 RCS_Id,       &   ! Revision contorl
                                 Message_Log ) &   ! Error messaging
                               RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)     :: NC_Filename
    TYPE(TauProfile_type),  INTENT(IN OUT) :: TauProfile
    INTEGER,      OPTIONAL, INTENT(IN)     :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_TauProfile_netCDF(Structure)'
    ! Function variables
    CHARACTER(1000) :: Message
    LOGICAL :: Noisy
    INTEGER :: NC_FileID
    INTEGER :: Destroy_Status
    INTEGER :: Close_Status
    INTEGER :: k, l, i, m, j

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF

    ! Read the dimension values
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              n_Layers        = k, &
                                              n_Channels      = l, &
                                              n_Angles        = i, &
                                              n_Profiles      = m, &
                                              n_Molecule_Sets = j, &
                                              Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining TauProfile dimensions from '//&
                TRIM( NC_Filename )
      GOTO 3000
    END IF

    ! Allocate the structure
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

    ! Fill the dimension descriptor arrays
    Error_Status = Inquire_TauProfile_netCDF( NC_Filename, &
                                              Level_Pressure    = TauProfile%Level_Pressure, &
                                              Channel_List      = TauProfile%Channel, &
                                              Angle_List        = TauProfile%Angle, &
                                              Profile_List      = TauProfile%Profile, &
                                              Molecule_Set_List = TauProfile%Molecule_Set, &
                                              Sensor_ID         = TauProfile%Sensor_ID, &  
                                              WMO_Satellite_ID  = TauProfile%WMO_Satellite_ID, &
                                              WMO_Sensor_ID     = TauProfile%WMO_Sensor_ID, &   
                                              Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining TauProfile list arrays from '//&
                TRIM( NC_Filename )
      GOTO 2000
    END IF

    ! Open the file
    Error_Status = Open_TauProfile_netCDF( TRIM( NC_Filename ), &
                                         NC_FileID, &
                                         Mode = 'READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF TauProfile data file '//&
                TRIM( NC_Filename )
      GOTO 2000
    END IF

    ! Read all the transmittance data
    Error_Status = Get_netCDF_Variable( NC_FileID,   &
                                        TRANSMITTANCE_VARNAME, &
                                        TauProfile%Tau )
    IF ( Error_Status/= SUCCESS ) THEN
      WRITE( Message, '( "Error reading Tau array from ", a, "." )' ) &
                      TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! Close the file
    Close_Status = Close_TauProfile_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauProfile data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    ! Output an info message
    IF ( Noisy ) THEN
      CALL Information_TauProfile( TauProfile, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( NC_Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

    RETURN


    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

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
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Read_TauProfile_type

END MODULE TauProfile_netCDF_IO
