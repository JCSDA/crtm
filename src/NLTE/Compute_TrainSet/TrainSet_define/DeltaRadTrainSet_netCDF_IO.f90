!
! DeltaRadTrainSet_netCDF_IO
!
! Module containing routines to read and write TauProfile netCDF 
! format files.
!
!
! CREATION HISTORY:
!       Written by:     Y. Han, June 17, 2010
!

MODULE DeltaRadTrainSet_netCDF_IO


  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds       , ONLY: fp
  USE Message_Handler  , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                               Display_Message
  USE DeltaRad_TrainSet_Define, ONLY: TrainSet_type, &
                               TrainSet_Associated, &
                               TrainSet_Create
  USE netcdf
  USE netCDF_Utility   ,  Open_TrainSet_netCDF =>  Open_netCDF, &
                         Close_TrainSet_netCDF => Close_netCDF
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Inquire_TrainSet_netCDF
  PUBLIC :: Write_TrainSet_netCDF
  PUBLIC :: Read_TrainSet_netCDF


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
  CHARACTER(*), PARAMETER :: CHANNEL_DIMNAME      = 'n_Channels'
  CHARACTER(*), PARAMETER :: SENSOR_ANGLE_DIMNAME = 'n_Sensor_Angles'
  CHARACTER(*), PARAMETER :: SUN_ANGLE_DIMNAME    = 'n_Sun_Angles'
  CHARACTER(*), PARAMETER :: PROFILE_DIMNAME      = 'n_Profiles'
  CHARACTER(*), PARAMETER :: LAYER_DIMNAME        = 'n_layers'
  CHARACTER(*), PARAMETER :: LEVEL_DIMNAME        = 'n_levels'

  ! Variable names
  CHARACTER(*), PARAMETER :: RADIANCE_NLTE_VARNAME     = 'NLTE_radiance'
  CHARACTER(*), PARAMETER :: RADIANCE_LTE_VARNAME      = 'LTE_radiance'
  CHARACTER(*), PARAMETER :: SENSOR_ANGLE_VARNAME      = 'Sensor_zenith_angle'
  CHARACTER(*), PARAMETER :: SUN_ANGLE_VARNAME         = 'Sun_zenith_angle'
  CHARACTER(*), PARAMETER :: LEVEL_PRESSURE_VARNAME    = 'Level_pressure'
  CHARACTER(*), PARAMETER :: LEVEL_TEMPERATURE_VARNAME = 'Level_temperature'
  CHARACTER(*), PARAMETER :: LEVEL_CO2_VARNAME         = 'Level_CO2'
  CHARACTER(*), PARAMETER :: CHANNEL_VARNAME           = 'Channel_index'

  ! Variable long name attribute.
  CHARACTER(*), PARAMETER :: LONGNAME_ATTNAME = 'long_name'

  CHARACTER(*), PARAMETER :: RADIANCE_NLTE_LONGNAME     = 'NLTE radiance'
  CHARACTER(*), PARAMETER :: RADIANCE_LTE_LONGNAME      = 'LTE radiance'
  CHARACTER(*), PARAMETER :: SENSOR_ANGLE_LONGNAME      = 'Sensor zenith angle'
  CHARACTER(*), PARAMETER :: SUN_ANGLE_LONGNAME         = 'Sun zenith angle'
  CHARACTER(*), PARAMETER :: LEVEL_PRESSURE_LONGNAME    = 'Level pressure'
  CHARACTER(*), PARAMETER :: LEVEL_TEMPERATURE_LONGNAME = 'Level temperature'
  CHARACTER(*), PARAMETER :: LEVEL_CO2_LONGNAME         = 'Level CO2'
  CHARACTER(*), PARAMETER :: CHANNEL_LONGNAME           = 'Channel index'

  ! Variable description attribute.
  CHARACTER(*), PARAMETER :: DESCRIPTION_ATTNAME = 'description'

  CHARACTER(*), PARAMETER :: RADIANCE_NLTE_DESC     = 'NLTE radiance spectrum'
  CHARACTER(*), PARAMETER :: RADIANCE_LTE_DESC      = 'LTE radiance spectrum'
  CHARACTER(*), PARAMETER :: SENSOR_ANGLE_DESC      = 'List of sSensor zenith angle'
  CHARACTER(*), PARAMETER :: SUN_ANGLE_DESC         = 'List of Sun zenith angle'
  CHARACTER(*), PARAMETER :: LEVEL_PRESSURE_DESC    = 'Atmospheric level pressure profile'
  CHARACTER(*), PARAMETER :: LEVEL_TEMPERATURE_DESC = 'Atmospheric level temperature profile'
  CHARACTER(*), PARAMETER :: LEVEL_CO2_DESC         = 'Atmospheric level CO2 profile'
  CHARACTER(*), PARAMETER :: CHANNEL_DESC           = 'list of channel index'

  ! Variable units attribute.
  CHARACTER(*), PARAMETER :: UNITS_ATTNAME = 'units'

  CHARACTER(*), PARAMETER :: RADIANCE_NLTE_UNITS     = 'W / (cm^2 sr cm^-1)'
  CHARACTER(*), PARAMETER :: RADIANCE_LTE_UNITS      = 'W / (cm^2 sr cm^-1)'
  CHARACTER(*), PARAMETER :: SENSOR_ANGLE_UNITS      = 'Degree'
  CHARACTER(*), PARAMETER :: SUN_ANGLE_UNITS         = 'Degree'
  CHARACTER(*), PARAMETER :: LEVEL_PRESSURE_UNITS    = 'hectoPascals (hPa)'
  CHARACTER(*), PARAMETER :: LEVEL_TEMPERATURE_UNITS = 'K'
  CHARACTER(*), PARAMETER :: LEVEL_CO2_UNITS         = 'PPMV'
  CHARACTER(*), PARAMETER :: CHANNEL_UNITS           = 'None'
  
  ! Variable _FillValue attribute.
  CHARACTER(*), PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'
  
  REAL(fp)    , PARAMETER :: FP_FILLVALUE                = -1.0_fp
  INTEGER     , PARAMETER :: IP_FILLVALUE                = -1
  REAL(fp)    , PARAMETER :: RADIANCE_NLTE_FILLVALUE     = FP_FILLVALUE
  REAL(fp)    , PARAMETER :: RADIANCE_LTE_FILLVALUE      = FP_FILLVALUE
  REAL(fp)    , PARAMETER :: SENSOR_ANGLE_FILLVALUE      = FP_FILLVALUE
  REAL(fp)    , PARAMETER :: SUN_ANGLE_FILLVALUE         = FP_FILLVALUE
  REAL(fp)    , PARAMETER :: LEVEL_PRESSURE_FILLVALUE    = FP_FILLVALUE
  REAL(fp)    , PARAMETER :: LEVEL_TEMPERATURE_FILLVALUE = FP_FILLVALUE
  REAL(fp)    , PARAMETER :: LEVEL_CO2_FILLVALUE         = FP_FILLVALUE
  INTEGER     , PARAMETER :: CHANNEL_FILLVALUE           = IP_FILLVALUE

  ! Variable types
  INTEGER   , PARAMETER :: RADIANCE_NLTE_TYPE     = NF90_DOUBLE
  INTEGER   , PARAMETER :: RADIANCE_LTE_TYPE      = NF90_DOUBLE
  INTEGER   , PARAMETER :: SENSOR_ANGLE_TYPE      = NF90_DOUBLE
  INTEGER   , PARAMETER :: SUN_ANGLE_TYPE         = NF90_DOUBLE
  INTEGER   , PARAMETER :: LEVEL_PRESSURE_TYPE    = NF90_DOUBLE
  INTEGER   , PARAMETER :: LEVEL_TEMPERATURE_TYPE = NF90_DOUBLE
  INTEGER   , PARAMETER :: LEVEL_CO2_TYPE         = NF90_DOUBLE
  INTEGER   , PARAMETER :: CHANNEL_TYPE           = NF90_INT


CONTAINS


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
!       Write_TrainSet_GAtts
!
! PURPOSE:
!       Function to write the global attributes to a netCDF TrainSet
!       data file.
!
! CALLING SEQUENCE:
!       Error_Status = Write_TrainSet_GAtts( NC_Filename                      , &  ! Input
!                                            NC_FileID                        , &  ! Input            
!                                            Release         =Release         , &  ! Optional input   
!                                            Version         =Version         , &  ! Optional input   
!                                            Sensor_Id       =Sensor_Id       , &  ! Optional input   
!                                            WMO_Satellite_Id=WMO_Satellite_Id, &  ! Optional input   
!                                            WMO_Sensor_Id   =WMO_Sensor_Id   , &  ! Optional input   
!                                            ID_Tag          =ID_Tag          , &  ! Optional input   
!                                            Title           =Title           , &  ! Optional input   
!                                            History         =History         , &  ! Optional input   
!                                            Comment         =Comment         , &  ! Optional input   
!                                            Message_Log     =Message_Log       )  ! Error messaging  
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

  FUNCTION Write_TrainSet_GAtts( NC_Filename     , &  ! Input
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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_TrainSet_GAtts'
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
    
  END FUNCTION Write_TrainSet_GAtts


!------------------------------------------------------------------------------
!
! NAME:
!       Read_TrainSet_GAtts
!
! PURPOSE:
!       Function to read the global attributes from a netCDF TrainSet
!       data file.
!
! CALLING SEQUENCE:
!       Error_Status = Read_TrainSet_GAtts( NC_Filename                      , &  ! Input
!                                           NC_FileID                        , &  ! Input            
!                                           Release         =Release         , &  ! Optional output  
!                                           Version         =Version         , &  ! Optional output  
!                                           Sensor_Id       =Sensor_Id       , &  ! Optional output  
!                                           WMO_Satellite_Id=WMO_Satellite_Id, &  ! Optional output  
!                                           WMO_Sensor_Id   =WMO_Sensor_Id   , &  ! Optional output  
!                                           ID_Tag          =ID_Tag          , &  ! Optional output  
!                                           Title           =Title           , &  ! Optional output  
!                                           History         =History         , &  ! Optional output  
!                                           Comment         =Comment         , &  ! Optional output  
!                                           Message_Log     =Message_Log       )  ! Error messaging  
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

  FUNCTION Read_TrainSet_GAtts( NC_Filename     , &  ! Input
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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_TrainSet_GAtts'
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

  END FUNCTION Read_TrainSet_GAtts


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
!       Write_TrainSet_netCDF
!
! PURPOSE:
!       Function to Write a netCDF TrainSet data file .
!
! CALLING SEQUENCE:
!       Error_Status = Write_TrainSet_netCDF(    NC_Filename                      , &  ! Input
!                                                Radiance_nlte                    , &  ! Input
!                                                Radiance_lte                     , &  ! Input
!                                                Sensor_Angle                     , &  ! Input
!                                                Sun_Angle                        , &  ! Input
!                                                Level_Pressure                   , &  ! Input
!                                                Level_Temperature                , &  ! Input
!                                                Level_CO2                        , &  ! Input
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
!       Radiance_nlte:      The nlte radiance
!                           UNITS:      see units specificantion in the module variable section
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Rank-4
!                           ATTRIBUTES: INTENT(IN)
!
!       Radiance_nlte:      The lte radiance
!                           UNITS:      see units specificantion in the module variable section
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Rank-4
!                           ATTRIBUTES: INTENT(IN)
!
!       Sensor_Angle:       The sensor zenith angle
!                           UNITS:      Degree
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Rank-1, n_Channels
!                           ATTRIBUTES: INTENT(IN)
!
!       Sun_Angle:          The sun zenith angle
!                           UNITS:      Degree
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Rank-1, n_Channels
!                           ATTRIBUTES: INTENT(IN)
!
!       Level_Pressure:     The level pressure profile
!                           dimension is derived from this array.
!                           UNITS:      mb
!                           TYPE:       fp
!                           DIMENSION:  Rank-2, n_levels x n_Profiles
!                           ATTRIBUTES: INTENT(IN)
!
!       Level_Temperature:  The level temperature profile
!                           UNITS:      K
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Rank-2, n_levels x n_Profiles
!                           ATTRIBUTES: INTENT(IN)
!
!       Level_CO2:          The level CO2 profile
!                           UNITS:      mmpv
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Rank-2, n_levels x n_Profiles
!                           ATTRIBUTES: INTENT(IN)
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

  FUNCTION Write_TrainSet_netCDF(    NC_Filename      , &  ! Input
                                     Radiance_nlte    , &  ! Input
                                     Radiance_lte     , &  ! Input
                                     Sensor_angle     , &  ! Input
                                     Sun_angle        , &  ! Input
                                     Level_Pressure   , &  ! Input
                                     Level_Temperature, &  ! Input
                                     Level_CO2        , &  ! Input
                                     Channel          , &  ! Input
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
    REAL(fp)              , INTENT(IN)  :: Radiance_nlte(:,:,:,:)
    REAL(fp)              , INTENT(IN)  :: Radiance_lte(:,:,:)
    REAL(fp)              , INTENT(IN)  :: Sensor_Angle(:)
    REAL(fp)              , INTENT(IN)  :: Sun_Angle(:)
    REAL(fp)              , INTENT(IN)  :: Level_Pressure(0:, :)
    REAL(fp)              , INTENT(IN)  :: Level_Temperature(0:, :)
    REAL(fp)              , INTENT(IN)  :: Level_CO2(0:, :)
    INTEGER               , INTENT(IN)  :: Channel(:)
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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_TrainSet_netCDF'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: NC_FileID
    INTEGER :: NF90_Status
    INTEGER :: Put_Status(4)
    INTEGER :: Close_Status
    INTEGER :: StrLen_DimID
    INTEGER :: n_Channels,      Channel_DimID
    INTEGER :: n_Sensor_Angles, Sensor_Angle_DimID
    INTEGER :: n_Sun_Angles,    Sun_Angle_DimID
    INTEGER :: n_Layers,        Layer_DimID
    INTEGER :: n_Levels,        Level_DimID
    INTEGER :: n_Profiles,      Profile_DimID
    INTEGER :: VarID

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


   ! get channel dimension
    n_Channels = SIZE( Channel )
    IF ( n_Channels < 1 ) THEN
      Message = 'CHANNEL index array must be non-zero size.'
      GOTO 2000
    END IF

    !  Sensor Angle dimension
    n_Sensor_Angles = SIZE( Sensor_Angle )
    IF ( n_Sensor_Angles < 1 ) THEN
      Message = 'Sensor_Angle array must be non-zero size.'
      GOTO 2000
    END IF

    !  Sun Angle input
    n_Sun_Angles = SIZE( Sun_Angle )
    IF ( n_Sun_Angles < 1 ) THEN
      Message = 'Sun_Angle array must be non-zero size.'
      GOTO 2000
    END IF

    ! Get layer dimension
    n_Levels = SIZE( Level_Pressure, DIM=1 )
    n_Layers = n_Levels - 1
    IF ( n_Layers < 1 ) THEN
      Message = 'LEVEL_PRESSURE array must have at least 2-elements.'
      GOTO 2000
    END IF

    ! profile dimension
    n_Profiles = SIZE( Level_Pressure, DIM=2 )
    IF ( n_Profiles < 1 ) THEN
      Message = 'PROFILE_LIST array must be non-zero size.'
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

    ! The number of sensor angles
    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                SENSOR_ANGLE_DIMNAME, n_Sensor_Angles, Sensor_Angle_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//SENSOR_ANGLE_DIMNAME//' dimension in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      GOTO 1000
    END IF

    ! The number of sun angles
    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                SUN_ANGLE_DIMNAME, n_Sun_Angles, Sun_Angle_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//SUN_ANGLE_DIMNAME//' dimension in '//&
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

    ! The number of profiles
    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                PROFILE_DIMNAME, n_Profiles, Profile_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//PROFILE_DIMNAME//' dimension in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      GOTO 1000
    END IF


    ! Write the global attributes
    ! ---------------------------
    Error_Status = Write_TrainSet_GAtts(   NC_Filename                      , &
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

    ! The radiance_nlte set
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                RADIANCE_NLTE_VARNAME, &
                                RADIANCE_NLTE_TYPE, &
                                dimIDs=(/Channel_DimID, SENSOR_ANGLE_DimID, SUN_ANGLE_DimID, Profile_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//RADIANCE_NLTE_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      GOTO 1000
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  RADIANCE_NLTE_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  RADIANCE_NLTE_DESC )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  RADIANCE_NLTE_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  RADIANCE_NLTE_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//RADIANCE_NLTE_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      GOTO 1000
    END IF

    ! The radiance_lte set
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                RADIANCE_LTE_VARNAME, &
                                RADIANCE_LTE_TYPE, &
                                dimIDs=(/Channel_DimID, SENSOR_ANGLE_DimID, Profile_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//RADIANCE_LTE_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      GOTO 1000
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  RADIANCE_LTE_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  RADIANCE_LTE_DESC )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  RADIANCE_LTE_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  RADIANCE_LTE_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//RADIANCE_LTE_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      GOTO 1000
    END IF

    ! The sensor angle set
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                SENSOR_ANGLE_VARNAME, &
                                SENSOR_ANGLE_TYPE, &
                                dimIDs=(/Sensor_Angle_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//SENSOR_ANGLE_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      GOTO 1000
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  SENSOR_ANGLE_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  SENSOR_ANGLE_DESC )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  SENSOR_ANGLE_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  SENSOR_ANGLE_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//SENSOR_ANGLE_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      GOTO 1000
    END IF

    ! The sun angle set
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                SUN_ANGLE_VARNAME, &
                                SUN_ANGLE_TYPE, &
                                dimIDs=(/Sun_Angle_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//SUN_ANGLE_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      GOTO 1000
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  SUN_ANGLE_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  SUN_ANGLE_DESC )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  SUN_ANGLE_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  SUN_ANGLE_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//SUN_ANGLE_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      GOTO 1000
    END IF

    ! The level pressure
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                LEVEL_PRESSURE_VARNAME, &
                                LEVEL_PRESSURE_TYPE, &
                                dimIDs=(/Level_DimID, Profile_DimID/), &
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

    ! The level pressure
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                LEVEL_TEMPERATURE_VARNAME, &
                                LEVEL_TEMPERATURE_TYPE, &
                                dimIDs=(/Level_DimID, Profile_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//LEVEL_TEMPERATURE_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      GOTO 1000
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  LEVEL_TEMPERATURE_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  LEVEL_TEMPERATURE_DESC )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  LEVEL_TEMPERATURE_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  LEVEL_TEMPERATURE_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//LEVEL_TEMPERATURE_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      GOTO 1000
    END IF

    ! The level CO2
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                LEVEL_CO2_VARNAME, &
                                LEVEL_CO2_TYPE, &
                                dimIDs=(/Level_DimID, Profile_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//LEVEL_CO2_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      GOTO 1000
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  LEVEL_CO2_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  LEVEL_CO2_DESC )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  LEVEL_CO2_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  LEVEL_CO2_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//LEVEL_CO2_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      GOTO 1000
    END IF

    ! The channel set
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                CHANNEL_VARNAME, &
                                CHANNEL_TYPE, &
                                dimIDs=(/Channel_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//CHANNEL_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      GOTO 1000
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  CHANNEL_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  CHANNEL_DESC )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  CHANNEL_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  CHANNEL_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//CHANNEL_VARNAME//&
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


    ! Write the data
    ! --------------------------
    ! Radiance nlte
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        RADIANCE_NLTE_VARNAME, &
                                        Radiance_nlte )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//RADIANCE_NLTE_VARNAME//' to '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    ! Radiance lte
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        RADIANCE_LTE_VARNAME, &
                                        Radiance_lte )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//RADIANCE_LTE_VARNAME//' to '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    ! Sensor Angle
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        SENSOR_ANGLE_VARNAME, &
                                        Sensor_Angle )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//SENSOR_ANGLE_VARNAME//' to '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    ! Sun Angle
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        SUN_ANGLE_VARNAME, &
                                        Sun_Angle )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//SUN_ANGLE_VARNAME//' to '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    ! Level pressure
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        LEVEL_PRESSURE_VARNAME, &
                                        Level_Pressure )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//LEVEL_PRESSURE_VARNAME//' to '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    ! Temperature
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        LEVEL_TEMPERATURE_VARNAME, &
                                        Level_Temperature )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//LEVEL_TEMPERATURE_VARNAME//' to '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    ! CO2
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        LEVEL_CO2_VARNAME, &
                                        Level_CO2 )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//LEVEL_CO2_VARNAME//' to '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    ! Channel
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        CHANNEL_VARNAME, &
                                        Channel )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//CHANNEL_VARNAME//' to '//TRIM(NC_Filename)
      GOTO 1000
    END IF


    ! Close the file
    ! --------------
    Close_Status = Close_TrainSet_netCDF( NC_FileID )
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

  END FUNCTION Write_TrainSet_netCDF


!------------------------------------------------------------------------------
!
! NAME:
!       Inquire_TrainSet_netCDF
!
! PURPOSE:
!       Function to inquire a netCDF TrainSet dimesional variables
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_TrainSet_netCDF( NC_Filename                        , &  ! Input
!                                               n_Channels       = n_Channels      , &  ! Optional output     
!                                               n_Sensor_Angles  = n_Sensor_Angles , &  ! Optional output                   
!                                               n_Sun_Angles     = n_Sun_Angles    , &  ! Optional output                    
!                                               n_Layers         = n_Layers        , &  ! Optional output                   
!                                               n_Profiles       = n_Profiles      , &  ! Optional output                     
!                                               Release          =Release          , &  ! Optional output   
!                                               Version          =Version          , &  ! Optional output   
!                                               Sensor_ID        =Sensor_ID        , &  ! Optional output   
!                                               WMO_Satellite_ID =WMO_Satellite_ID , &  ! Optional output   
!                                               WMO_Sensor_ID    =WMO_Sensor_ID    , &  ! Optional output   
!                                               Level_Pressure   =Level_Pressure   , &  ! Optional output   
!                                               Channel_List     =Channel_List     , &  ! Optional output   
!                                               Angle_List       =Angle_List       , &  ! Optional output   
!                                               Profile_List     =Profile_List     , &  ! Optional output   
!                                               Molecule_Set_List=Molecule_Set_List, &  ! Optional output   
!                                               ID_Tag           =ID_Tag           , &  ! Optional output   
!                                               Title            =Title            , &  ! Optional output   
!                                               History          =History          , &  ! Optional output   
!                                               Comment          =Comment          , &  ! Optional output   
!                                               RCS_Id           =RCS_Id           , &  ! Revision control  
!                                               Message_Log      =Message_Log        )  ! Error messaging   
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
!       n_Channels:         The number of channels dimension 
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Sensor_Angle:     The number of sensor zenith angle dimension 
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Sun_Angle:        The number of sun zenith angle dimension 
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Layers:           The number of atmospheric layers dimension 
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!
!       n_Profiles:         The number of profile dimension
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
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

  FUNCTION Inquire_TrainSet_netCDF( NC_Filename        , &  ! Input
                                    n_Channels       , &  ! Optional output   
                                    n_Sensor_Angles  , &  ! Optional output   
                                    n_Sun_Angles     , &  ! Optional output   
                                    n_Layers         , &  ! Optional output   
                                    n_Profiles       , &  ! Optional output   
                                    Release          , &  ! Optional output   
                                    Version          , &  ! Optional output   
                                    Sensor_ID        , &  ! Optional output   
                                    WMO_Satellite_ID , &  ! Optional output   
                                    WMO_Sensor_ID    , &  ! Optional output   
                                    ID_Tag           , &  ! Optional output   
                                    Title            , &  ! Optional output   
                                    History          , &  ! Optional output   
                                    Comment          , &  ! Optional output   
                                    RCS_Id           , &  ! Revision control  
                                    Message_Log      ) &  ! Error messaging   
                                    RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Channels
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Sensor_Angles
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Sun_Angles
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Layers
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Profiles
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release         
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Sensor_ID   
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Satellite_ID 
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Sensor_ID
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: ID_Tag
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Inquire_TrainSet_netCDF'
    ! Function variables
    CHARACTER(1000) :: Message
    INTEGER :: NC_FileID
    INTEGER :: Close_Status
    INTEGER :: k, l, m, j, isen, isun

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! Open the file
    ! -------------
    Error_Status = Open_TrainSet_netCDF( TRIM(NC_Filename), &
                                           NC_FileID, &
                                           Mode = 'READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF TauProfile data file '//&
                TRIM(NC_Filename)
      GOTO 2000
    END IF


    ! Get the dimensions
    ! ------------------
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
    ! The number of Sensor Angles
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         SENSOR_ANGLE_DIMNAME, &
                                         isen, &
                                         Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//SENSOR_ANGLE_DIMNAME//&
                ' dimension from '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    ! The number of Sun Angles
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         SUN_ANGLE_DIMNAME, &
                                         isun, &
                                         Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//SUN_ANGLE_DIMNAME//&
                ' dimension from '//TRIM(NC_Filename)
      GOTO 1000
    END IF
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

    ! Set the dimension return values
    IF ( PRESENT( n_Channels      ) ) n_Channels      = l
    IF ( PRESENT( n_Sensor_Angles ) ) n_Sensor_Angles = isen
    IF ( PRESENT( n_Sun_Angles    ) ) n_Sun_Angles    = isun
    IF ( PRESENT( n_Layers        ) ) n_Layers        = k
    IF ( PRESENT( n_Profiles      ) ) n_Profiles      = m


    ! Get the global attributes
    ! -------------------------
    Error_Status = Read_TrainSet_GAtts(   TRIM(NC_Filename)                 , &
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
    Close_Status = Close_TrainSet_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TrainSet data file '// &
                            TRIM(NC_Filename), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

    !=====
    RETURN
    !=====

    ! Clean up after an error
    1000 CONTINUE
    Close_Status = Close_TrainSet_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing input file during error cleanup.'
    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Inquire_TrainSet_netCDF


!------------------------------------------------------------------------------
!
! NAME:
!       Read_TrainSet_netCDF
!
! PURPOSE:
!       Function to read data from a netCDF format TauProfile file.
!
! CALLING SEQUENCE:
!
!
!     Error_Status = Read_TrainSete_netCDF( NC_Filename,  &  ! Input
!                                            TrainSet,     &  ! Output, TYPE( TrainSet_type)
!                                            Quiet       = Quiet,      &  ! Optional input
!                                            RCS_Id      = RCS_Id,     &  ! Revision control
!                                            Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:     Character string specifying the name of the
!                        netCDF format TrainSet data file to read.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       TrainSet:        Structure to contain the transmittance data read
!                        from file.
!                        UNITS:      N/A
!                        TYPE:       TrainSet_type
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

  FUNCTION Read_TrainSet_netCDF( NC_Filename, &   ! Input
                                 TrainSet   , &   ! Output
                                 Quiet      , &   ! Optional input
                                 RCS_Id     , &   ! Revision contorl
                                 Message_Log) &   ! Error messaging
                               RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)     :: NC_Filename
    TYPE(TrainSet_type) , INTENT(IN OUT)   :: TrainSet
    INTEGER     , OPTIONAL, INTENT(IN)     :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_TrainSet_netCDF(Structure)'
    ! Function variables
    CHARACTER(1000) :: Message
    LOGICAL :: Noisy
    INTEGER :: NC_FileID
    INTEGER :: Destroy_Status
    INTEGER :: Close_Status
    INTEGER :: k, l, m, j, isen, isun

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
    Error_Status = Inquire_TrainSet_netCDF( NC_Filename, &
                                            n_Channels     =l, &           
                                            n_Sensor_Angles=isen, &        
                                            n_Sun_Angles   =isun, &        
                                            n_Layers       =k, &           
                                            n_Profiles     =m, &           
                                            Message_Log    =Message_Log )  
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining TauProfile dimensions from '//&
                TRIM(NC_Filename)
      GOTO 2000
    END IF

    ! Allocate the structure
    ! ----------------------
    CALL TrainSet_Create( TrainSet, &
                          l, &
                          isen, &
                          isun, &
                          k, &
                          m )
    IF( .NOT. TrainSet_Associated( TrainSet ) )THEN
      Message = 'Error occurred allocating TrainSet structure.'
      GOTO 2000
    END IF

    ! Read the global attributes
    ! --------------------------
    Error_Status = Inquire_TrainSet_netCDF( NC_Filename                                 , &
                                              Release         =TrainSet%Release         , &
                                              Version         =TrainSet%Version         , &
                                              Sensor_ID       =TrainSet%Sensor_ID       , &  
                                              WMO_Satellite_ID=TrainSet%WMO_Satellite_ID, &
                                              WMO_Sensor_ID   =TrainSet%WMO_Sensor_ID   , &   
                                              Message_Log     =Message_Log                  )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining TauProfile global attributes from '//&
                TRIM(NC_Filename)
      GOTO 2000
    END IF
        
    ! Open the file
    ! -------------
    Error_Status = Open_TrainSet_netCDF( TRIM(NC_Filename), &
                                         NC_FileID, &
                                         Mode = 'READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF TrainSet data file '//&
                TRIM(NC_Filename)
      GOTO 2000
    END IF

    ! Read the data
    ! ----------------------------
    Error_Status = Get_netCDF_Variable( NC_FileID,   &            
                                        RADIANCE_NLTE_VARNAME, &  
                                        TrainSet%Radiance_nlte)   
    IF ( Error_Status /= SUCCESS ) THEN                                            
      WRITE( Message, '( "Error reading Radiance_nlte array to ", a, "." )' ) &  
                      TRIM(NC_Filename)                                            
      GOTO 1000                                                                    
    END IF                                                                         

    Error_Status = Get_netCDF_Variable( NC_FileID,   &            
                                        RADIANCE_LTE_VARNAME, &  
                                        TrainSet%Radiance_lte)   
    IF ( Error_Status /= SUCCESS ) THEN                                            
      WRITE( Message, '( "Error reading Radiance_lte array to ", a, "." )' ) &  
                      TRIM(NC_Filename)                                            
      GOTO 1000                                                                    
    END IF                                                                         

    Error_Status = Get_netCDF_Variable( NC_FileID,   &            
                                        SENSOR_ANGLE_VARNAME, &  
                                        TrainSet%Sensor_Angle)   
    IF ( Error_Status /= SUCCESS ) THEN                                            
      WRITE( Message, '( "Error reading Sensor_Angle array to ", a, "." )' ) &  
                      TRIM(NC_Filename)                                            
      GOTO 1000                                                                    
    END IF         

    Error_Status = Get_netCDF_Variable( NC_FileID,   &            
                                        SUN_ANGLE_VARNAME, &  
                                        TrainSet%Sun_Angle)   
    IF ( Error_Status /= SUCCESS ) THEN                                            
      WRITE( Message, '( "Error reading Sun_Angle array to ", a, "." )' ) &  
                      TRIM(NC_Filename)                                            
      GOTO 1000                                                                    
    END IF         
                                                                    
    Error_Status = Get_netCDF_Variable( NC_FileID,   &            
                                        LEVEL_PRESSURE_VARNAME, &  
                                        TrainSet%Level_Pressure)   
    IF ( Error_Status /= SUCCESS ) THEN                                            
      WRITE( Message, '( "Error reading Level_Pressure array to ", a, "." )' ) &  
                      TRIM(NC_Filename)                                            
      GOTO 1000                                                                    
    END IF                                                                         

    Error_Status = Get_netCDF_Variable( NC_FileID,   &            
                                        LEVEL_TEMPERATURE_VARNAME, &  
                                        TrainSet%Level_Temperature)   
    IF ( Error_Status /= SUCCESS ) THEN                                            
      WRITE( Message, '( "Error reading Level_Temperature array to ", a, "." )' ) &  
                      TRIM(NC_Filename)                                            
      GOTO 1000                                                                    
    END IF                                                                         

    Error_Status = Get_netCDF_Variable( NC_FileID,   &            
                                        LEVEL_CO2_VARNAME, &  
                                        TrainSet%Level_CO2)   
    IF ( Error_Status /= SUCCESS ) THEN                                            
      WRITE( Message, '( "Error reading Level_CO2 array to ", a, "." )' ) &  
                      TRIM(NC_Filename)                                            
      GOTO 1000                                                                    
    END IF                                                                         

    Error_Status = Get_netCDF_Variable( NC_FileID,   &            
                                        CHANNEL_VARNAME, &  
                                        TrainSet%Channel)   
    IF ( Error_Status /= SUCCESS ) THEN                                            
      WRITE( Message, '( "Error reading Channel array to ", a, "." )' ) &  
                      TRIM(NC_Filename)                                            
      GOTO 1000                                                                    
    END IF        

    ! Close the file
    ! --------------
    Close_Status = Close_TrainSet_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TrainSet data file '// &
                            TRIM(NC_Filename), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    ! Output an info message
    ! ----------------------
!    IF ( Noisy ) THEN
!      CALL Display_Message( ROUTINE_NAME, &
!                            'FILE: '//TRIM(NC_Filename)//'; '//TRIM(Message), &
!                            INFORMATION, &
!                            Message_Log = Message_Log )
!    END IF

    !=====
    RETURN
    !=====
    
    ! Clean up after an error
    ! -----------------------
    1000 CONTINUE
    Close_Status = Close_TrainSet_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing input file during error cleanup.'
    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Read_TrainSet_netCDF

END MODULE DeltaRadTrainSet_netCDF_IO
