!
! ZTauCoeff_netCDF_IO
!
! Module containing routines to read and write ZTauCoeff netCDF 
! format files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 10-Apr-2007
!                       paul.vandelst@ssec.wisc.edu
!

MODULE ZTauCoeff_netCDF_IO


  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds      , ONLY: Long, Double
  USE Message_Handler , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                              Display_Message
  USE ZTauCoeff_Define, ONLY: ZTauCoeff_type, &
                              Associated_ZTauCoeff, &
                              Destroy_ZTauCoeff, &
                              Allocate_ZTauCoeff, &
                              CheckRelease_ZTauCoeff, &
                              Info_ZTauCoeff
  USE netcdf
  USE netCDF_Utility  , Open_ZTauCoeff_netCDF =>  Open_netCDF, &
                        Close_ZTauCoeff_netCDF => Close_netCDF
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Inquire_ZTauCoeff_netCDF
  PUBLIC :: Write_ZTauCoeff_netCDF
  PUBLIC :: Read_ZTauCoeff_netCDF


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Module RCS Id string
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
    '$Id$'
  ! Keyword set value
  INTEGER,      PARAMETER :: SET = 1
  ! Literal constants
  REAL(Double), PARAMETER :: ZERO = 0.0_Double
  REAL(Double), PARAMETER :: ONE  = 1.0_Double

  ! Global attribute names. Case sensitive
  CHARACTER(*), PARAMETER :: TITLE_GATTNAME            = 'title' 
  CHARACTER(*), PARAMETER :: HISTORY_GATTNAME          = 'history'
  CHARACTER(*), PARAMETER :: COMMENT_GATTNAME          = 'comment'
  CHARACTER(*), PARAMETER :: RELEASE_GATTNAME          = 'Release'
  CHARACTER(*), PARAMETER :: VERSION_GATTNAME          = 'Version'
  CHARACTER(*), PARAMETER :: SENSOR_ID_GATTNAME        = 'Sensor_Id'
  CHARACTER(*), PARAMETER :: WMO_SATELLITE_ID_GATTNAME = 'WMO_Satellite_Id'
  CHARACTER(*), PARAMETER :: WMO_SENSOR_ID_GATTNAME    = 'WMO_Sensor_Id'

  ! Dimension names
  CHARACTER(*), PARAMETER :: PREDICTOR_DIMNAME = 'n_Predictors'
  CHARACTER(*), PARAMETER :: LEVEL_DIMNAME     = 'n_Levels'
  CHARACTER(*), PARAMETER :: LAYER_DIMNAME     = 'n_Layers'
  CHARACTER(*), PARAMETER :: CHANNEL_DIMNAME   = 'n_Channels'

  ! Variable names
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_VARNAME = 'Sensor_Channel'
  CHARACTER(*), PARAMETER :: LEVEL_ALTITUDE_VARNAME = 'Level_Altitude'
  CHARACTER(*), PARAMETER :: LEVEL_PRESSURE_VARNAME = 'Level_Pressure'
  CHARACTER(*), PARAMETER :: PRESSURE_VARNAME       = 'Pressure'
  CHARACTER(*), PARAMETER :: CHANNELINDEX_VARNAME   = 'ChannelIndex'
  CHARACTER(*), PARAMETER :: PREDICTORINDEX_VARNAME = 'PredictorIndex'
  CHARACTER(*), PARAMETER :: SECANT_ZENITH_VARNAME  = 'Secant_Zenith'
  CHARACTER(*), PARAMETER :: C_VARNAME              = 'C'

  ! Variable long name attribute.
  CHARACTER(*), PARAMETER :: LONGNAME_ATTNAME = 'long_name'

  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_LONGNAME = 'Sensor Channel'
  CHARACTER(*), PARAMETER :: LEVEL_ALTITUDE_LONGNAME = 'Level Altitude'
  CHARACTER(*), PARAMETER :: LEVEL_PRESSURE_LONGNAME = 'Level Pressure'
  CHARACTER(*), PARAMETER :: PRESSURE_LONGNAME       = 'Layer Pressure'
  CHARACTER(*), PARAMETER :: CHANNELINDEX_LONGNAME   = 'Channel Index'
  CHARACTER(*), PARAMETER :: PREDICTORINDEX_LONGNAME = 'Predictor Index'
  CHARACTER(*), PARAMETER :: SECANT_ZENITH_LONGNAME  = 'Secant(Zenith Angle)'
  CHARACTER(*), PARAMETER :: C_LONGNAME              = 'Coefficients'

  ! Variable description attribute.
  CHARACTER(*), PARAMETER :: DESCRIPTION_ATTNAME = 'description'

  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_DESCRIPTION = 'List of Zeeman-affected channels for the sensor'
  CHARACTER(*), PARAMETER :: LEVEL_ALTITUDE_DESCRIPTION = 'Reference level altitudes'
  CHARACTER(*), PARAMETER :: LEVEL_PRESSURE_DESCRIPTION = 'Reference level pressures'
  CHARACTER(*), PARAMETER :: PRESSURE_DESCRIPTION       = 'Reference layer pressures'
  CHARACTER(*), PARAMETER :: CHANNELINDEX_DESCRIPTION   = 'Channel index of Zeeman-affected channels for the sensor'
  CHARACTER(*), PARAMETER :: PREDICTORINDEX_DESCRIPTION = 'Index list of predictors to use in the absorption calculation.'
  CHARACTER(*), PARAMETER :: SECANT_ZENITH_DESCRIPTION  = 'Secant of the zenith angles at the weighting function peak levels'
  CHARACTER(*), PARAMETER :: C_DESCRIPTION              = 'Zeeman absorption model coefficients'

  ! Variable units attribute.
  CHARACTER(*), PARAMETER :: UNITS_ATTNAME = 'units'
  
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_UNITS = 'N/A'
  CHARACTER(*), PARAMETER :: LEVEL_ALTITUDE_UNITS = 'Metres (m)'
  CHARACTER(*), PARAMETER :: LEVEL_PRESSURE_UNITS = 'hectoPascals (hPa)'
  CHARACTER(*), PARAMETER :: PRESSURE_UNITS       = 'hectoPascals (hPa)'
  CHARACTER(*), PARAMETER :: CHANNELINDEX_UNITS   = 'N/A'
  CHARACTER(*), PARAMETER :: PREDICTORINDEX_UNITS = 'N/A'
  CHARACTER(*), PARAMETER :: SECANT_ZENITH_UNITS  = 'N/A'
  CHARACTER(*), PARAMETER :: C_UNITS              = 'Variable'

  ! Variable _FillValue attribute.
  CHARACTER(*), PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'
  
  INTEGER(Long), PARAMETER :: SENSOR_CHANNEL_FILLVALUE = 0
  REAL(Double) , PARAMETER :: LEVEL_ALTITUDE_FILLVALUE = ZERO
  REAL(Double) , PARAMETER :: LEVEL_PRESSURE_FILLVALUE = ZERO
  REAL(Double) , PARAMETER :: PRESSURE_FILLVALUE       = ZERO
  INTEGER(Long), PARAMETER :: CHANNELINDEX_FILLVALUE   = 0
  INTEGER(Long), PARAMETER :: PREDICTORINDEX_FILLVALUE = 0
  REAL(Double) , PARAMETER :: SECANT_ZENITH_FILLVALUE  = -ONE
  REAL(Double) , PARAMETER :: C_FILLVALUE              = ZERO

  ! Variable types
  INTEGER, PARAMETER :: SENSOR_CHANNEL_TYPE = NF90_INT
  INTEGER, PARAMETER :: LEVEL_ALTITUDE_TYPE = NF90_DOUBLE
  INTEGER, PARAMETER :: LEVEL_PRESSURE_TYPE = NF90_DOUBLE
  INTEGER, PARAMETER :: PRESSURE_TYPE       = NF90_DOUBLE
  INTEGER, PARAMETER :: CHANNELINDEX_TYPE   = NF90_INT
  INTEGER, PARAMETER :: PREDICTORINDEX_TYPE = NF90_INT
  INTEGER, PARAMETER :: SECANT_ZENITH_TYPE  = NF90_DOUBLE
  INTEGER, PARAMETER :: C_TYPE              = NF90_DOUBLE


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
!       Write_ZTauCoeff_GAtts
!
! PURPOSE:
!       Function to write the global attributes to a netCDF ZTauCoeff
!       data file.
!
! CALLING SEQUENCE:
!       Error_Status = Write_ZTauCoeff_GAtts( NC_Filename                      , &  ! Input
!                                             NC_FileID                        , &  ! Input
!                                             Release         =Release         , &  ! Optional input
!                                             Version         =Version         , &  ! Optional input
!                                             Sensor_Id       =Sensor_Id       , &  ! Optional input
!                                             WMO_Satellite_Id=WMO_Satellite_Id, &  ! Optional input
!                                             WMO_Sensor_Id   =WMO_Sensor_Id   , &  ! Optional input
!                                             Title           =Title           , &  ! Optional input
!                                             History         =History         , &  ! Optional input
!                                             Comment         =Comment         , &  ! Optional input
!                                             Message_Log     =Message_Log       )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF ZTauCoeff format data file to create.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:        NetCDF file ID number returned from the
!                         Open_ZTauCoeff_netCDF() function.
!                         UNITS:      N/A
!                         TYPE:       Integer
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Release:          The release number of the netCDF ZTauCoeff file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Version:          The version number of the netCDF ZTauCoeff file.
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
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF ZTauCoeff file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF ZTauCoeff file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF ZTauCoeff file.
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
!                            == FAILURE an error occurred.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION Write_ZTauCoeff_GAtts( NC_Filename     , &  ! Input
                                  NC_FileID       , &  ! Input
                                  Release         , &  ! Optional input
                                  Version         , &  ! Optional input
                                  Sensor_Id       , &  ! Optional input
                                  WMO_Satellite_Id, &  ! Optional input
                                  WMO_Sensor_Id   , &  ! Optional input
                                  Title           , &  ! Optional input
                                  History         , &  ! Optional input
                                  Comment         , &  ! Optional input
                                  Message_Log     ) &  ! Error messaging
                                RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: NC_Filename
    INTEGER     ,           INTENT(IN) :: NC_FileID
    INTEGER     , OPTIONAL, INTENT(IN) :: Release         
    INTEGER     , OPTIONAL, INTENT(IN) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Sensor_Id       
    INTEGER     , OPTIONAL, INTENT(IN) :: WMO_Satellite_Id
    INTEGER     , OPTIONAL, INTENT(IN) :: WMO_Sensor_Id   
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_ZTauCoeff_GAtts'
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
    
  END FUNCTION Write_ZTauCoeff_GAtts


!------------------------------------------------------------------------------
!
! NAME:
!       Read_ZTauCoeff_GAtts
!
! PURPOSE:
!       Function to read the global attributes from a netCDF ZTauCoeff
!       data file.
!
! CALLING SEQUENCE:
!       Error_Status = Read_ZTauCoeff_GAtts( NC_Filename                      , &  ! Input
!                                            NC_FileID                        , &  ! Input
!                                            Release         =Release         , &  ! Optional output
!                                            Version         =Version         , &  ! Optional output
!                                            Sensor_Id       =Sensor_Id       , &  ! Optional output
!                                            WMO_Satellite_Id=WMO_Satellite_Id, &  ! Optional output
!                                            WMO_Sensor_Id   =WMO_Sensor_Id   , &  ! Optional output
!                                            Title           =Title           , &  ! Optional output
!                                            History         =History         , &  ! Optional output
!                                            Comment         =Comment         , &  ! Optional output
!                                            Message_Log     =Message_Log       )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF ZTauCoeff format data file to read from.
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
!       Release:          The release number of the netCDF ZTauCoeff file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Version:          The version number of the netCDF ZTauCoeff file.
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
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF ZTauCoeff file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF ZTauCoeff file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF ZTauCoeff file.
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

  FUNCTION Read_ZTauCoeff_GAtts( NC_Filename     , &  ! Input
                                 NC_FileID       , &  ! Input
                                 Release         , &  ! Optional output
                                 Version         , &  ! Optional output
                                 Sensor_Id       , &  ! Optional output
                                 WMO_Satellite_Id, &  ! Optional output
                                 WMO_Sensor_Id   , &  ! Optional output
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
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_ZTauCoeff_GAtts'
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

  END FUNCTION Read_ZTauCoeff_GAtts


!------------------------------------------------------------------------------
!
! NAME:
!       DefineVar_ZTauCoeff_netCDF
!
! PURPOSE:
!       Function to define the antenna correction variables in any
!       output netCDF file.
!
! CALLING SEQUENCE:
!       Error_Status = DefineVar_ZTauCoeff_netCDF( NC_Filename            , &  ! Input
!                                                  NC_FileID              , &  ! Input
!                                                  n_Predictors_DimID     , &  ! Input
!                                                  n_Levels_DimID         , &  ! Input
!                                                  n_Layers_DimID         , &  ! Input
!                                                  n_Channels_DimID       , &  ! Input
!                                                  RCS_Id     =RCS_Id     , &  ! Revision control
!                                                  Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS
!       NC_Filename:        Character string specifying the name of the
!                           already created netCDF ZTauCoeff format file.
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
!       n_Predictors_DimID: NetCDF dimension ID of the number of predictors
!                           plus one (since all the predictor arrays are
!                           are dimensioned (0:n_Predictors).
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Levels_DimID:     NetCDF dimension ID of the number of
!                           atmospheric levels.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Layers_DimID:     NetCDF dimension ID of the number of
!                           atmospheric layers.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Channels_DimID:   NetCDF dimension ID of the number of sensor
!                           channels (n_Channels).
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
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!------------------------------------------------------------------------------

  FUNCTION DefineVar_ZTauCoeff_netCDF( NC_Filename       , &  ! Input
                                       NC_FileID         , &  ! Input
                                       n_Predictors_DimID, &  ! Input
                                       n_Levels_DimID    , &  ! Input
                                       n_Layers_DimID    , &  ! Input
                                       n_Channels_DimID  , &  ! Input
                                       RCS_Id            , &  ! Revision control
                                       Message_Log       ) &  ! Error messaging
                                     RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER     ,           INTENT(IN)  :: NC_FileID
    INTEGER     ,           INTENT(IN)  :: n_Predictors_DimID
    INTEGER     ,           INTENT(IN)  :: n_Levels_DimID    
    INTEGER     ,           INTENT(IN)  :: n_Layers_DimID    
    INTEGER     ,           INTENT(IN)  :: n_Channels_DimID  
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'DefineVar_ZTauCoeff_netCDF'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: NF90_Status
    INTEGER :: varID
    INTEGER :: Put_Status(4)
                               
    ! Set up
    ! ------
    Error_Status = SUCCESS                                      
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! Define the sensor channels
    ! --------------------------
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                SENSOR_CHANNEL_VARNAME, &
                                SENSOR_CHANNEL_TYPE, &
                                dimIDs=(/n_Channels_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//SENSOR_CHANNEL_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      GOTO 1000
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  SENSOR_CHANNEL_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  SENSOR_CHANNEL_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  SENSOR_CHANNEL_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  SENSOR_CHANNEL_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//SENSOR_CHANNEL_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      GOTO 1000
    END IF


    ! Define the reference altitude/pressures
    ! ---------------------------------------
    ! Level altitude
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                LEVEL_ALTITUDE_VARNAME, &
                                LEVEL_ALTITUDE_TYPE, &
                                dimIDs=(/n_Levels_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//LEVEL_ALTITUDE_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      GOTO 1000
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  LEVEL_ALTITUDE_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  LEVEL_ALTITUDE_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  LEVEL_ALTITUDE_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  LEVEL_ALTITUDE_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//LEVEL_ALTITUDE_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      GOTO 1000
    END IF

    ! Level pressure
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                LEVEL_PRESSURE_VARNAME, &
                                LEVEL_PRESSURE_TYPE, &
                                dimIDs=(/n_Levels_DimID/), &
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
                                  LEVEL_PRESSURE_DESCRIPTION )
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

    ! Layer pressure
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                PRESSURE_VARNAME, &
                                PRESSURE_TYPE, &
                                dimIDs=(/n_Layers_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//PRESSURE_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      GOTO 1000
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  PRESSURE_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  PRESSURE_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  PRESSURE_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  PRESSURE_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//PRESSURE_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      GOTO 1000
    END IF


    ! Define the indexing arrays
    ! --------------------------
    ! Channel index
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                CHANNELINDEX_VARNAME, &
                                CHANNELINDEX_TYPE, &
                                dimIDs=(/n_Channels_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//CHANNELINDEX_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      GOTO 1000
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  CHANNELINDEX_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  CHANNELINDEX_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  CHANNELINDEX_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  CHANNELINDEX_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//CHANNELINDEX_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      GOTO 1000
    END IF

    ! Predictor index
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                PREDICTORINDEX_VARNAME, &
                                PREDICTORINDEX_TYPE, &
                                dimIDs=(/n_Predictors_DimID, n_Channels_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//PREDICTORINDEX_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      GOTO 1000
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  PREDICTORINDEX_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  PREDICTORINDEX_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  PREDICTORINDEX_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  PREDICTORINDEX_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//PREDICTORINDEX_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      GOTO 1000
    END IF


    ! Define the angle array
    ! ----------------------
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                SECANT_ZENITH_VARNAME, &
                                SECANT_ZENITH_TYPE, &
                                dimIDs=(/n_Channels_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//SECANT_ZENITH_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      GOTO 1000
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  SECANT_ZENITH_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  SECANT_ZENITH_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  SECANT_ZENITH_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  SECANT_ZENITH_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//SECANT_ZENITH_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      GOTO 1000
    END IF


    ! Define the coefficient data
    ! ---------------------------
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                C_VARNAME, &
                                C_TYPE, &
                                dimIDs=(/n_Predictors_DimID, &
                                         n_Layers_DimID    , &
                                         n_Channels_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//C_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      GOTO 1000
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  C_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  C_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  C_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  C_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//C_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      GOTO 1000
    END IF

    !=====
    RETURN
    !=====
    
    ! Clean up after error
    1000 CONTINUE
    NF90_Status = NF90_CLOSE( NC_FileID )
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )
                          
  END FUNCTION DefineVar_ZTauCoeff_netCDF


  FUNCTION WriteVar_ZTauCoeff_netCDF( NC_Filename, &
                                      NC_FileID  , &
                                      ZTauCoeff  , &
                                      RCS_Id     , &
                                      Message_Log) &
                                  RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)  :: NC_Filename
    INTEGER               , INTENT(IN)  :: NC_FileID
    TYPE(ZTauCoeff_type)  , INTENT(IN)  :: ZTauCoeff
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'WriteVar_ZTauCoeff_netCDF'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: NF90_Status
                               
    ! Set up
    ! ------
    Error_Status = SUCCESS                                      
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! Write the sensor channel data
    ! -----------------------------
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        SENSOR_CHANNEL_VARNAME, &
                                        ZTauCoeff%Sensor_Channel )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//SENSOR_CHANNEL_VARNAME//' to '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    
    
    ! Write the reference profile data
    ! --------------------------------
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        LEVEL_ALTITUDE_VARNAME, &
                                        ZTauCoeff%Level_Altitude )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//LEVEL_ALTITUDE_VARNAME//' to '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        LEVEL_PRESSURE_VARNAME, &
                                        ZTauCoeff%Level_Pressure )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//LEVEL_PRESSURE_VARNAME//' to '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        PRESSURE_VARNAME, &
                                        ZTauCoeff%Pressure )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//PRESSURE_VARNAME//' to '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    
    
    ! Write the indexing data
    ! -----------------------
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        CHANNELINDEX_VARNAME, &
                                        ZTauCoeff%ChannelIndex )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//CHANNELINDEX_VARNAME//' to '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        PREDICTORINDEX_VARNAME, &
                                        ZTauCoeff%PredictorIndex )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//PREDICTORINDEX_VARNAME//' to '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    
    
    ! Write the angle data
    ! --------------------
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        SECANT_ZENITH_VARNAME, &
                                        ZTauCoeff%Secant_Zenith )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//SECANT_ZENITH_VARNAME//' to '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    
    
    ! Write the coefficient data
    ! --------------------------
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        C_VARNAME, &
                                        ZTauCoeff%C )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//C_VARNAME//' to '//TRIM(NC_Filename)
      GOTO 1000
    END IF

    !=====
    RETURN
    !=====
    
    ! Clean up after an error
    ! -----------------------
    1000 CONTINUE
    NF90_Status = NF90_CLOSE( NC_FileID )
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION WriteVar_ZTauCoeff_netCDF


  FUNCTION ReadVar_ZTauCoeff_netCDF( NC_Filename, &
                                     NC_FileID  , &
                                     ZTauCoeff  , &
                                     RCS_Id     , &
                                     Message_Log) &
                                 RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)     :: NC_Filename
    INTEGER               , INTENT(IN)     :: NC_FileID
    TYPE(ZTauCoeff_type)  , INTENT(IN OUT) :: ZTauCoeff
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ReadVar_ZTauCoeff_netCDF'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: NF90_Status
                               
    ! Set up
    ! ------
    Error_Status = SUCCESS                                      
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! Read the sensor channel data
    ! -----------------------------
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        SENSOR_CHANNEL_VARNAME, &
                                        ZTauCoeff%Sensor_Channel )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//SENSOR_CHANNEL_VARNAME//' from '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    
    
    ! Read the reference profile data
    ! --------------------------------
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        LEVEL_ALTITUDE_VARNAME, &
                                        ZTauCoeff%Level_Altitude )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//LEVEL_ALTITUDE_VARNAME//' from '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        LEVEL_PRESSURE_VARNAME, &
                                        ZTauCoeff%Level_Pressure )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//LEVEL_PRESSURE_VARNAME//' from '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        PRESSURE_VARNAME, &
                                        ZTauCoeff%Pressure )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//PRESSURE_VARNAME//' from '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    
    
    ! Read the indexing data
    ! -----------------------
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        CHANNELINDEX_VARNAME, &
                                        ZTauCoeff%ChannelIndex )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//CHANNELINDEX_VARNAME//' from '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        PREDICTORINDEX_VARNAME, &
                                        ZTauCoeff%PredictorIndex )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//PREDICTORINDEX_VARNAME//' from '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    
    
    ! Read the angle data
    ! --------------------
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        SECANT_ZENITH_VARNAME, &
                                        ZTauCoeff%Secant_Zenith )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//SECANT_ZENITH_VARNAME//' from '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    
    
    ! Read the coefficient data
    ! --------------------------
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        C_VARNAME, &
                                        ZTauCoeff%C )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//C_VARNAME//' from '//TRIM(NC_Filename)
      GOTO 1000
    END IF

    !=====
    RETURN
    !=====
    
    ! Clean up after an error
    ! -----------------------
    1000 CONTINUE
    NF90_Status = NF90_CLOSE( NC_FileID )
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION ReadVar_ZTauCoeff_netCDF


!------------------------------------------------------------------------------
!
! NAME:
!       Create_ZTauCoeff_netCDF
!
! PURPOSE:
!       Function to create a netCDF ZTauCoeff data file for writing.
!
! CALLING SEQUENCE:
!       Error_Status = Create_ZTauCoeff_netCDF( NC_Filename                      , &  ! Input
!                                               n_Predictors                     , &  ! Input
!                                               n_Layers                         , &  ! Input
!                                               n_Channels                       , &  ! Input
!                                               NC_FileID                        , &  ! Output
!                                               Release         =Release         , &  ! Optional input
!                                               Version         =Version         , &  ! Optional input
!                                               Sensor_Id       =Sensor_Id       , &  ! Optional input
!                                               WMO_Satellite_Id=WMO_Satellite_Id, &  ! Optional input
!                                               WMO_Sensor_Id   =WMO_Sensor_Id   , &  ! Optional input
!                                               Title           =Title           , &  ! Optional input
!                                               History         =History         , &  ! Optional input
!                                               Comment         =Comment         , &  ! Optional input
!                                               Message_Log     =Message_Log       )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:        Character string specifying the name of the
!                           netCDF ZTauCoeff format data file to create.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Predictors:       Number of predictors.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Layers:           Number of atmospheric layers.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Channels:         Number of sensor channels.
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
!       Release:            The release number of the netCDF ZTauCoeff file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Version:            The version number of the netCDF ZTauCoeff file.
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
!       Title:              Character string written into the TITLE global
!                           attribute field of the netCDF ZTauCoeff file.
!                           Should contain a succinct description of what
!                           is in the netCDF datafile.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the netCDF ZTauCoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF ZTauCoeff file.
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

  FUNCTION Create_ZTauCoeff_netCDF( NC_Filename     , &  ! Input
                                    n_Predictors    , &  ! Input
                                    n_Layers        , &  ! Input
                                    n_Channels      , &  ! Input
                                    NC_FileID       , &  ! Output
                                    Release         , &  ! Optional input
                                    Version         , &  ! Optional input
                                    Sensor_Id       , &  ! Optional input
                                    WMO_Satellite_Id, &  ! Optional input
                                    WMO_Sensor_Id   , &  ! Optional input
                                    Title           , &  ! Optional input
                                    History         , &  ! Optional input
                                    Comment         , &  ! Optional input
                                    Message_Log     ) &  ! Error messaging
                                  RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)  :: NC_Filename
    INTEGER               , INTENT(IN)  :: n_Predictors
    INTEGER               , INTENT(IN)  :: n_Layers    
    INTEGER               , INTENT(IN)  :: n_Channels  
    INTEGER               , INTENT(OUT) :: NC_FileID
    INTEGER     , OPTIONAL, INTENT(IN)  :: Release         
    INTEGER     , OPTIONAL, INTENT(IN)  :: Version         
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Sensor_Id       
    INTEGER     , OPTIONAL, INTENT(IN)  :: WMO_Satellite_Id
    INTEGER     , OPTIONAL, INTENT(IN)  :: WMO_Sensor_Id   
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Create_ZTauCoeff_netCDF'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: NF90_Status
    INTEGER :: n_Predictors_DimID
    INTEGER :: n_Levels_DimID
    INTEGER :: n_Layers_DimID
    INTEGER :: n_Channels_DimID
    INTEGER :: VarID
    

    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Check input
    IF ( n_Predictors < 1 .OR. &
         n_Layers     < 1 .OR. &
         n_Channels   < 1      ) THEN
      Message = 'Invalid dimension input detected.'
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
    ! The number of predictors (+1 since arrays dimensioned 0:n_Predictors)
    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                PREDICTOR_DIMNAME, n_Predictors+1, n_Predictors_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//PREDICTOR_DIMNAME//' dimension in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      GOTO 1000
    END IF

    ! The number of levels
    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                LEVEL_DIMNAME, n_Layers+1, n_Levels_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//LEVEL_DIMNAME//' dimension in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      GOTO 1000
    END IF

    ! The number of layers
    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                LAYER_DIMNAME, n_Layers, n_Layers_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//LAYER_DIMNAME//' dimension in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      GOTO 1000
    END IF

    ! The number of spectral channels
    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                CHANNEL_DIMNAME, n_Channels, n_Channels_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//CHANNEL_DIMNAME//' dimension in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      GOTO 1000
    END IF

    ! Write the global attributes
    ! ---------------------------
    Error_Status = Write_ZTauCoeff_GAtts( NC_Filename                      , &
                                          NC_FileID                        , &
                                          Release         =Release         , &
                                          Version         =Version         , &
                                          Sensor_Id       =Sensor_Id       , &
                                          WMO_Satellite_Id=WMO_Satellite_Id, &
                                          WMO_Sensor_Id   =WMO_Sensor_Id   , &
                                          Title           =Title           , &
                                          History         =History         , &
                                          Comment         =Comment         , &
                                          Message_Log     =Message_Log       )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing global attributes to '//TRIM(NC_Filename)
      GOTO 2000
    END IF


    ! Define the antenna correction variables
    ! ---------------------------------------
    Error_Status = DefineVar_ZTauCoeff_netCDF( NC_Filename            , &
                                               NC_FileID              , &
                                               n_Predictors_DimID     , &
                                               n_Levels_DimID         , &
                                               n_Layers_DimID         , &
                                               n_Channels_DimID       , &
                                               Message_Log=Message_Log  )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error defining variables in '//TRIM(NC_Filename)
      GOTO 2000
    END IF
                                             

    ! Take netCDF file out of define mode
    ! -----------------------------------
    NF90_Status = NF90_ENDDEF( NC_FileID )
    IF ( NF90_Status /= NF90_NOERR ) GOTO 1000

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

  END FUNCTION Create_ZTauCoeff_netCDF


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
!       Inquire_ZTauCoeff_netCDF
!
! PURPOSE:
!       Function to inquire a netCDF ZTauCoeff format file to obtain the
!       dimensions and global attributes.
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_ZTauCoeff_netCDF( NC_Filename                       , &  ! Input
!                                                n_Predictors     =n_Predictors    , &  ! Optional output
!                                                n_Layers         =n_Layers        , &  ! Optional output
!                                                n_Channels       =n_Channels      , &  ! Optional output
!                                                Release          =Release         , &  ! Optional output
!                                                Version          =Version         , &  ! Optional output
!                                                Sensor_Id        =Sensor_Id       , &  ! Optional output
!                                                WMO_Satellite_Id =WMO_Satellite_Id, &  ! Optional output
!                                                WMO_Sensor_Id    =WMO_Sensor_Id   , &  ! Optional output
!                                                Title            =Title           , &  ! Optional output
!                                                History          =History         , &  ! Optional output
!                                                Comment          =Comment         , &  ! Optional output
!                                                RCS_Id           =RCS_Id          , &  ! Revision control
!                                                Message_Log      =Message_Log       )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:        Character string specifying the name of the
!                           ZTauCoeff netCDF format data file to inquire.
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
!       n_Predictors:       Number of predictors.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Layers:           Number of atmospheric layers.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Channels:         Number of sensor channels.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:            The release number of the netCDF ZTauCoeff file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:            The version number of the netCDF ZTauCoeff file.
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
!                           attribute field of the netCDF ZTauCoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the netCDF ZTauCoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF ZTauCoeff file.
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
!------------------------------------------------------------------------------

  FUNCTION Inquire_ZTauCoeff_netCDF( NC_Filename     , &  ! Input
                                     n_Predictors    , &  ! Optional output
                                     n_Layers        , &  ! Optional output
                                     n_Channels      , &  ! Optional output
                                     Release         , &  ! Optional output
                                     Version         , &  ! Optional output
                                     Sensor_Id       , &  ! Optional output
                                     WMO_Satellite_Id, &  ! Optional output
                                     WMO_Sensor_Id   , &  ! Optional output
                                     Title           , &  ! Optional output
                                     History         , &  ! Optional output
                                     Comment         , &  ! Optional output
                                     RCS_Id          , &  ! Revision control
                                     Message_Log     ) &  ! Error messaging
                                   RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Predictors
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Layers    
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Channels  
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release         
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Sensor_Id       
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Satellite_Id
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Sensor_Id   
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Inquire_ZTauCoeff_netCDF'
    ! Function variables
    CHARACTER(1000) :: Message
    INTEGER :: NC_FileID
    INTEGER :: Close_Status
    INTEGER :: n_File_Predictors
    INTEGER :: n_File_Layers    
    INTEGER :: n_File_Channels  
    
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! Open the file
    ! -------------
    Error_Status = Open_ZTauCoeff_netCDF( TRIM(NC_Filename), &
                                          NC_FileID, &
                                          Mode = 'READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF ZTauCoeff data file '//&
                TRIM(NC_Filename)
      GOTO 2000
    END IF


    ! Get the dimensions
    ! ------------------
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         PREDICTOR_DIMNAME, &
                                         n_File_Predictors, &
                                         Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//PREDICTOR_DIMNAME//&
                ' dimension from '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         LAYER_DIMNAME, &
                                         n_File_Layers, &
                                         Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//LAYER_DIMNAME//&
                ' dimension from '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         CHANNEL_DIMNAME, &
                                         n_File_Channels, &
                                         Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//CHANNEL_DIMNAME//&
                ' dimension from '//TRIM(NC_Filename)
      GOTO 1000
    END IF


    ! Set the return values
    ! ---------------------
    IF ( PRESENT( n_Predictors ) ) n_Predictors = n_File_Predictors-1
    IF ( PRESENT( n_Layers     ) ) n_Layers     = n_File_Layers    
    IF ( PRESENT( n_Channels   ) ) n_Channels   = n_File_Channels  
    

    ! Get the global attributes
    ! -------------------------
    Error_Status = Read_ZTauCoeff_GAtts( NC_Filename                       , &
                                         NC_FileID                         , &
                                         Release          =Release         , &
                                         Version          =Version         , &
                                         Sensor_Id        =Sensor_Id       , &
                                         WMO_Satellite_Id =WMO_Satellite_Id, &
                                         WMO_Sensor_Id    =WMO_Sensor_Id   , &
                                         Title            =Title           , &
                                         History          =History         , &
                                         Comment          =Comment         , &
                                         Message_Log      =Message_Log       )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading global attribute from '//TRIM(NC_Filename)
      GOTO 2000
    END IF


    ! Close the file
    ! --------------
    Close_Status = Close_ZTauCoeff_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF ZTauCoeff data file '// &
                            TRIM(NC_Filename), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

    !=====
    RETURN
    !=====

    ! Clean up after an error
    1000 CONTINUE
    Close_Status = Close_ZTauCoeff_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing input file during error cleanup.'
    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Inquire_ZTauCoeff_netCDF


!------------------------------------------------------------------------------
!
! NAME:
!       Write_ZTauCoeff_netCDF
!
! PURPOSE:
!       Function to write ZTauCoeff data to a netCDF format ZTauCoeff file.
!
! CALLING SEQUENCE:
!       Error_Status = Write_ZTauCoeff_netCDF( NC_Filename            , &  ! Input
!                                              ZTauCoeff                , &  ! Input
!                                              Quiet      =Quiet      , &  ! Optional input
!                                              Title      =Title      , &  ! Optional input
!                                              History    =History    , &  ! Optional input
!                                              Comment    =Comment    , &  ! Optional input
!                                              RCS_Id     =RCS_Id     , &  ! Revision control
!                                              Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:  Character string specifying the name of the netCDF
!                     format ZTauCoeff data file to write data into.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       ZTauCoeff:    Structure containing the antenna correction data
!                     to write to file.
!                     UNITS:      N/A
!                     TYPE:       TYPE(ZTauCoeff_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:        Set this keyword to suppress information messages being
!                     printed to standard output (or the message log file if
!                     the MESSAGE_LOG optional argument is used.) By default,
!                     information messages are printed.
!                     If QUIET = 0, information messages are OUTPUT.
!                        QUIET = 1, information messages are SUPPRESSED.
!                     UNITS:      N/A
!                     TYPE:       Integer
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:        Character string written into the TITLE global
!                     attribute field of the netCDF ZTauCoeff file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:      Character string written into the HISTORY global
!                     attribute field of the netCDF ZTauCoeff file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:      Character string written into the COMMENT global
!                     attribute field of the netCDF ZTauCoeff file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
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
!------------------------------------------------------------------------------

  FUNCTION Write_ZTauCoeff_netCDF( NC_Filename , &  ! Input
                                   ZTauCoeff     , &  ! Input
                                   Quiet       , &  ! Optional input
                                   Title       , &  ! Optional input
                                   History     , &  ! Optional input
                                   Comment     , &  ! Optional input
                                   RCS_Id      , &  ! Revision control
                                   Message_Log ) &  ! Error messaging
                                 RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    TYPE(ZTauCoeff_type)  , INTENT(IN)  :: ZTauCoeff
    INTEGER     , OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title  
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_ZTauCoeff_netCDF'
    ! Local variables
    CHARACTER(1000) :: Message
    LOGICAL :: Noisy
    INTEGER :: NC_FileID
    INTEGER :: Close_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF

    ! Check structure association
    IF ( .NOT. Associated_ZTauCoeff( ZTauCoeff ) ) THEN
      Message = 'Some or all INPUT ZTauCoeff pointer members are NOT associated.'
      GOTO 2000
    END IF


    ! Create the output data file
    ! ---------------------------
    Error_Status = Create_ZTauCoeff_netCDF( NC_Filename                                 , &  ! Input
                                            ZTauCoeff%n_Predictors                      , &  ! Input
                                            ZTauCoeff%n_Layers                          , &  ! Input
                                            ZTauCoeff%n_Channels                        , &  ! Input
                                            NC_FileID                                   , &  ! Output
                                            Release          =ZTauCoeff%Release         , &  ! Optional input
                                            Version          =ZTauCoeff%Version         , &  ! Optional input
                                            Sensor_Id        =ZTauCoeff%Sensor_Id       , &  ! Optional input
                                            WMO_Satellite_Id =ZTauCoeff%WMO_Satellite_Id, &  ! Optional input
                                            WMO_Sensor_Id    =ZTauCoeff%WMO_Sensor_Id   , &  ! Optional input
                                            Title            =Title                     , &  ! Optional input
                                            History          =History                   , &  ! Optional input
                                            Comment          =Comment                   , &  ! Optional input
                                            Message_Log      =Message_Log                 )  ! Error messaging
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating output file '//TRIM(NC_Filename)
      GOTO 2000
    END IF


    ! Write the antenna correction data
    ! ---------------------------------
    Error_Status = WriteVar_ZTauCoeff_netCDF( NC_Filename            , &
                                              NC_FileID              , &
                                              ZTauCoeff              , &
                                              Message_Log=Message_Log)
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing AC variables to output file '//TRIM(NC_Filename)
      GOTO 2000
    END IF
    

    ! Close the file
    ! --------------
    Close_Status = Close_ZTauCoeff_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF ZTauCoeff data file '// &
                            TRIM(NC_Filename), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_ZTauCoeff( ZTauCoeff, Message )
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
    Close_Status = Close_ZTauCoeff_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing output file during error cleanup.'
    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Write_ZTauCoeff_netCDF


!------------------------------------------------------------------------------
!
! NAME:
!       Read_ZTauCoeff_netCDF
!
! PURPOSE:
!       Function to read data from a netCDF format ZTauCoeff file.
!
! CALLING SEQUENCE:
!     Error_Status = Read_ZTauCoeff_netCDF( NC_Filename            , &  ! Input
!                                           ZTauCoeff                , &  ! Output
!                                           Quiet      =Quiet      , &  ! Optional input
!                                           Title      =Title      , &  ! Optional output
!                                           History    =History    , &  ! Optional output
!                                           Comment    =Comment    , &  ! Optional output
!                                           RCS_Id     =RCS_Id     , &  ! Revision control
!                                           Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:  Character string specifying the name of the
!                     netCDF format ZTauCoeff data file to read.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       ZTauCoeff:    Structure to contain the antenna correction data
!                     read from file.
!                     UNITS:      N/A
!                     TYPE:       TYPE(ZTauCoeff_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:        Set this keyword to suppress information messages being
!                     printed to standard output (or the message log file if
!                     the MESSAGE_LOG optional argument is used.) By default,
!                     information messages are printed.
!                     If QUIET = 0, information messages are OUTPUT.
!                        QUIET = 1, information messages are SUPPRESSED.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Title:        Character string written into the TITLE global
!                     attribute field of the netCDF ZTauCoeff file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:      Character string written into the HISTORY global
!                     attribute field of the netCDF ZTauCoeff file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:      Character string written into the COMMENT global
!                     attribute field of the netCDF ZTauCoeff file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
!
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
!                     If == SUCCESS the netCDF data read was successful.
!                        == FAILURE an unrecoverable error occurred.
!                        == WARNING an error occurred closing the netCDF
!                                   input file after a successful read.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! COMMENTS:
!       If specified as the output data type, the INTENT on the output ZTauCoeff
!       structure argument is IN OUT rather than just OUT. This is necessary
!       because the argument may be defined on input. To prevent memory leaks,
!       the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Read_ZTauCoeff_netCDF( NC_Filename, &  ! Input
                                  ZTauCoeff    , &  ! Output
                                  Quiet      , &  ! Optional input
                                  Title      , &  ! Optional output
                                  History    , &  ! Optional output
                                  Comment    , &  ! Optional output
                                  RCS_Id     , &  ! Revision control
                                  Message_Log) &  ! Error messaging
                                RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)     :: NC_Filename
    TYPE(ZTauCoeff_type)  , INTENT(IN OUT) :: ZTauCoeff
    INTEGER,      OPTIONAL, INTENT(IN)     :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: Title  
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_ZTauCoeff_netCDF'
    ! Function variables
    CHARACTER(1000) :: Message
    LOGICAL :: Noisy
    INTEGER :: NC_FileID
    INTEGER :: Destroy_Status
    INTEGER :: Close_Status
    INTEGER :: n_Predictors
    INTEGER :: n_Layers    
    INTEGER :: n_Channels  

    
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF

    
    ! Allocate the structure for the netCDF read
    ! ------------------------------------------
    ! Read the dimension values
    Error_Status = Inquire_ZTauCoeff_netCDF( NC_Filename              , &
                                             n_Predictors=n_Predictors, &
                                             n_Layers    =n_Layers    , &
                                             n_Channels  =n_Channels  , &
                                             Title       =Title       , & 
                                             History     =History     , & 
                                             Comment     =Comment     , & 
                                             Message_Log =Message_Log   ) 
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining ZTauCoeff dimensions from '//TRIM(NC_Filename)
      GOTO 3000
    END IF

    ! Allocate the structure
    Error_Status = Allocate_ZTauCoeff( n_Predictors, &
                                       n_Layers    , &
                                       n_Channels  , &
                                       ZTauCoeff   , &
                                       Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error occurred allocating ZTauCoeff structure.'
      GOTO 3000
    END IF


    ! Open the netCDF file for reading
    ! --------------------------------
    Error_Status = Open_ZTauCoeff_netCDF( TRIM(NC_Filename), &
                                          NC_FileID, &
                                          Mode='READ' )

    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF ZTauCoeff data file '//TRIM(NC_Filename)
      GOTO 2000
    END IF


    ! Read the remaining global attributes
    ! ------------------------------------
    Error_Status = Read_ZTauCoeff_GAtts( NC_Filename                                , &
                                         NC_FileID                                  , &
                                         Release         =ZTauCoeff%Release         , &
                                         Version         =ZTauCoeff%Version         , &
                                         Sensor_Id       =ZTauCoeff%Sensor_Id       , &
                                         WMO_Satellite_Id=ZTauCoeff%WMO_Satellite_Id, &
                                         WMO_Sensor_Id   =ZTauCoeff%WMO_Sensor_Id   , &
                                         Message_Log     =Message_Log               )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading global attribute from '//TRIM(NC_Filename)
      GOTO 1000
    END IF

    ! Check the release
    Error_Status = CheckRelease_ZTauCoeff( ZTauCoeff, &
                                           Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'ZTauCoeff Release check failed for '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    

    ! Read the antenna correction data
    ! ---------------------------------
    Error_Status = ReadVar_ZTauCoeff_netCDF( NC_Filename            , &
                                             NC_FileID              , &
                                             ZTauCoeff              , &
                                             Message_Log=Message_Log)
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading AC variables from '//TRIM(NC_Filename)
      GOTO 2000
    END IF


    ! Close the file
    ! --------------
    Close_Status = Close_ZTauCoeff_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF ZTauCoeff data file '// &
                            TRIM(NC_Filename), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_ZTauCoeff( ZTauCoeff, Message )
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
    Close_Status = Close_ZTauCoeff_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing input file during error cleanup.'

    2000 CONTINUE
    Destroy_Status = Destroy_ZTauCoeff(ZTauCoeff, Message_Log=Message_Log)
    IF ( Destroy_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error destroying ZTauCoeff during error cleanup.'

    3000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Read_ZTauCoeff_netCDF

END MODULE ZTauCoeff_netCDF_IO
