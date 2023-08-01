!
! TauCoeff_netCDF_IO
!
! Module containing routines to create, inquire, read and write netCDF
! format TauCoeff files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 30-Dec-2002
!                       paul.vandelst@ssec.wisc.edu
!

MODULE TauCoeff_netCDF_IO

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Message_Handler,     ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, Display_Message
  USE TauCoeff_Define,     ONLY: TauCoeff_Type, &
                                 Associated_TauCoeff, &
                                 Allocate_TauCoeff, &
                                 Destroy_TauCoeff, &
                                 Check_TauCoeff_Release, &
                                 Count_TauCoeff_Sensors, &
                                 Info_TauCoeff 
  USE netcdf
  USE netCDF_Utility,  Open_TauCoeff_netCDF =>  Open_netCDF, &
                      Close_TauCoeff_netCDF => Close_netCDF
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Inquire_TauCoeff_netCDF
  PUBLIC :: Write_TauCoeff_netCDF
  PUBLIC :: Read_TauCoeff_netCDF


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Module RCS Id string
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &

  ! Global attribute names. Case sensitive
  CHARACTER(*), PARAMETER :: TITLE_GATTNAME         = 'title' 
  CHARACTER(*), PARAMETER :: HISTORY_GATTNAME       = 'history' 
  CHARACTER(*), PARAMETER :: SENSOR_NAME_GATTNAME   = 'sensor_name' 
  CHARACTER(*), PARAMETER :: PLATFORM_NAME_GATTNAME = 'platform_name' 
  CHARACTER(*), PARAMETER :: COMMENT_GATTNAME       = 'comment' 
  CHARACTER(*), PARAMETER :: ID_TAG_GATTNAME        = 'id_tag' 

  ! Dimension names
  CHARACTER(*), PARAMETER :: ORDER_DIMNAME      = 'n_Orders'
  CHARACTER(*), PARAMETER :: PREDICTOR_DIMNAME  = 'n_Predictors'
  CHARACTER(*), PARAMETER :: CHANNEL_DIMNAME    = 'n_Channels'
  CHARACTER(*), PARAMETER :: ABSORBER_DIMNAME   = 'n_Absorbers'
  CHARACTER(*), PARAMETER :: STRLEN_DIMNAME     = 'sdsl'

  ! Variable names. Case sensitive.
  CHARACTER(*), PARAMETER :: RELEASE_VARNAME           = 'Release'
  CHARACTER(*), PARAMETER :: VERSION_VARNAME           = 'Version'
  CHARACTER(*), PARAMETER :: SENSOR_DESCRIPTOR_VARNAME = 'Sensor_Descriptor'
  CHARACTER(*), PARAMETER :: NCEP_SENSOR_ID_VARNAME    = 'NCEP_Sensor_ID'
  CHARACTER(*), PARAMETER :: WMO_SATELLITE_ID_VARNAME  = 'WMO_Satellite_ID'
  CHARACTER(*), PARAMETER :: WMO_SENSOR_ID_VARNAME     = 'WMO_Sensor_ID'
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_VARNAME    = 'Sensor_Channel'
  CHARACTER(*), PARAMETER :: ABSORBER_ID_VARNAME       = 'Absorber_ID'
  CHARACTER(*), PARAMETER :: ALPHA_VARNAME             = 'Alpha'
  CHARACTER(*), PARAMETER :: ALPHA_C1_VARNAME          = 'Alpha_C1'
  CHARACTER(*), PARAMETER :: ALPHA_C2_VARNAME          = 'Alpha_C2'
  CHARACTER(*), PARAMETER :: ORDER_INDEX_VARNAME       = 'Order_Index'
  CHARACTER(*), PARAMETER :: PREDICTOR_INDEX_VARNAME   = 'Predictor_Index'
  CHARACTER(*), PARAMETER :: TAU_COEFFICIENTS_VARNAME  = 'Tau_Coefficients'

  ! Variable long name attribute.
  CHARACTER(*), PARAMETER :: LONGNAME_ATTNAME = 'long_name'

  CHARACTER(*), PARAMETER :: RELEASE_LONGNAME              = &
'Release number of TauCoeff data file'
  CHARACTER(*), PARAMETER :: VERSION_LONGNAME              = &
'Version number of TauCoeff data file'
  CHARACTER(*), PARAMETER :: SENSOR_DESCRIPTOR_LONGNAME    = &
'Short text string containing the sensor/satellite description'
  CHARACTER(*), PARAMETER :: NCEP_SENSOR_ID_LONGNAME       = &
'ID used at NOAA/NCEP/EMC to identify a satellite/sensor (-1 == none available)'
  CHARACTER(*), PARAMETER :: WMO_SATELLITE_ID_LONGNAME     = &
'WMO code for identifying satellite platforms (1023 == none available)'
  CHARACTER(*), PARAMETER :: WMO_SENSOR_ID_LONGNAME        = &
'WMO code for identifying a satellite sensor (2047 == none available)'
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_LONGNAME       = &
'List of sensor channel numbers associated with the TauCoeff data'
  CHARACTER(*), PARAMETER :: ABSORBER_ID_LONGNAME      = &
'List of absorber ID values for distinguishing the absorber type.'
  CHARACTER(*), PARAMETER :: ALPHA_LONGNAME            = &
'Alpha values used to generate the absorber space levels.'
  CHARACTER(*), PARAMETER :: ALPHA_C1_LONGNAME         = &
'First constant (slope) used in defining the Alpha to absorber space equation.'
  CHARACTER(*), PARAMETER :: ALPHA_C2_LONGNAME         = &
'Second constant (offset) used in defining the Alpha to absorber space equation.'
  CHARACTER(*), PARAMETER :: ORDER_INDEX_LONGNAME  = &
'Index list of polynomial orders to use in the gas absorption calculation.'
  CHARACTER(*), PARAMETER :: PREDICTOR_INDEX_LONGNAME  = &
'Index list of predictors to use in the gas absorption calculation.'
  CHARACTER(*), PARAMETER :: TAU_COEFFICIENTS_LONGNAME = &
'Regression model gas absorption coefficients.'

  ! Variable units attribute.
  CHARACTER(*), PARAMETER :: UNITS_ATTNAME = 'units'

  CHARACTER(*), PARAMETER :: RELEASE_UNITS          = 'N/A'
  CHARACTER(*), PARAMETER :: VERSION_UNITS          = 'N/A'
  CHARACTER(*), PARAMETER :: NCEP_SENSOR_ID_UNITS   = 'N/A'
  CHARACTER(*), PARAMETER :: WMO_SATELLITE_ID_UNITS = 'N/A'
  CHARACTER(*), PARAMETER :: WMO_SENSOR_ID_UNITS    = 'N/A'
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_UNITS   = 'N/A'
  CHARACTER(*), PARAMETER :: ABSORBER_ID_UNITS      = 'N/A'
  CHARACTER(*), PARAMETER :: ALPHA_UNITS            = 'Absorber dependent.'
  CHARACTER(*), PARAMETER :: ALPHA_C1_UNITS         = 'Absorber dependent.'
  CHARACTER(*), PARAMETER :: ALPHA_C2_UNITS         = 'Absorber dependent.'
  CHARACTER(*), PARAMETER :: ORDER_INDEX_UNITS      = 'N/A'
  CHARACTER(*), PARAMETER :: PREDICTOR_INDEX_UNITS  = 'N/A'
  CHARACTER(*), PARAMETER :: TAU_COEFFICIENTS_UNITS = 'Absorber and predictor dependent.'

  ! Variable netCDF datatypes
  INTEGER, PARAMETER :: RELEASE_TYPE           = NF90_INT
  INTEGER, PARAMETER :: VERSION_TYPE           = NF90_INT
  INTEGER, PARAMETER :: SENSOR_DESCRIPTOR_TYPE = NF90_CHAR
  INTEGER, PARAMETER :: NCEP_SENSOR_ID_TYPE    = NF90_INT
  INTEGER, PARAMETER :: WMO_SATELLITE_ID_TYPE  = NF90_INT
  INTEGER, PARAMETER :: WMO_SENSOR_ID_TYPE     = NF90_INT
  INTEGER, PARAMETER :: SENSOR_CHANNEL_TYPE    = NF90_INT
  INTEGER, PARAMETER :: ABSORBER_ID_TYPE       = NF90_INT
  INTEGER, PARAMETER :: ALPHA_TYPE             = NF90_DOUBLE
  INTEGER, PARAMETER :: ALPHA_C1_TYPE          = NF90_DOUBLE
  INTEGER, PARAMETER :: ALPHA_C2_TYPE          = NF90_DOUBLE
  INTEGER, PARAMETER :: ORDER_INDEX_TYPE       = NF90_INT
  INTEGER, PARAMETER :: PREDICTOR_INDEX_TYPE   = NF90_INT
  INTEGER, PARAMETER :: TAU_COEFFICIENTS_TYPE  = NF90_DOUBLE


CONTAINS


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
!       Write_TauCoeff_GAtts
!
! PURPOSE:
!       Function to write the global attributes to a netCDF TauCoeff data file.
!
! CALLING SEQUENCE:
!       Error_Status = Write_TauCoeff_GAtts( NC_Filename,                   &  ! Input
!                                            NC_FileID,                     &  ! Input
!                                            Title         = Title,         &  ! Optional input
!                                            History       = History,       &  ! Optional input
!                                            Sensor_Name   = Sensor_Name,   &  ! Optional input
!                                            Platform_Name = Platform_Name, &  ! Optional input
!                                            Comment       = Comment,       &  ! Optional input
!                                            ID_Tag        = ID_Tag,        &  ! Optional input
!                                            Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF TauCoeff format data file to create.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:        NetCDF file ID number returned from the
!                         Open_ or Create_TauCoeff_netCDF() function.
!                         UNITS:      N/A
!                         TYPE:       Integer
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF TauCoeff file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF TauCoeff file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Sensor_Name:      Character string written into the SENSOR_NAME
!                         global attribute field of the netCDF TauCoeff
!                         file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Platform_Name:    Character string written into the PLATFORM_NAME
!                         global attribute field of the netCDF TauCoeff
!                         file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF TauCoeff file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       ID_Tag:           Character string written into the ID_TAG global
!                         attribute field of the netCDF TauCoeff file.
!                         Should contain a short tag used to identify the
!                         dependent profile set used to generate the 
!                         coefficient data.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Message_Log:      Character string specifying a filename in which
!                         any messages will be logged. If not specified,
!                         or if an error occurs opening the log file, the
!                         default action is to output messages to standard
!                         output.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(IN)
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
! SIDE EFFECTS:
!       If a FAILURE error occurs, the netCDF file is closed.
!
! COMMENTS:
!       The netCDF file remains in DEFINE mode upon exiting this function.
!
!--------------------------------------------------------------------------------

  FUNCTION Write_TauCoeff_GAtts( NC_Filename,   &  ! Input
                                 NC_FileID,     &  ! Input
                                 Title,         &  ! Optional input
                                 History,       &  ! Optional input
                                 Sensor_Name,   &  ! Optional input
                                 Platform_Name, &  ! Optional input
                                 Comment,       &  ! Optional input
                                 ID_Tag,        &  ! Optional input
                                 Message_Log )  &  ! Error messaging
                               RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: NC_Filename
    INTEGER,                INTENT(IN) :: NC_FileID
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Sensor_Name
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Platform_Name
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN) :: ID_Tag
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_TauCoeff_GAtts'
    CHARACTER(*), PARAMETER :: WRITE_MODULE_HISTORY_GATTNAME   = 'write_module_history' 
    CHARACTER(*), PARAMETER :: CREATION_DATE_AND_TIME_GATTNAME = 'creation_date_and_time' 
    INTEGER, PARAMETER :: nPutGAtts = 8
    ! Local variables
    INTEGER :: Put_Status(nPutGAtts), n
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

    ! The TITLE
    n = n + 1
    IF ( PRESENT( Title ) ) THEN
      Put_Status(n) = Put_GAttString(TITLE_GATTNAME, Title, &
                                     Message_Log=Message_Log )
    END IF

    ! The HISTORY
    n = n + 1
    IF ( PRESENT( History ) ) THEN
      Put_Status(n) = Put_GAttString(HISTORY_GATTNAME, History, &
                                     Message_Log=Message_Log )
    END IF

    ! The SENSOR_NAME
    n = n + 1
    IF ( PRESENT( Sensor_Name ) ) THEN
      Put_Status(n) = Put_GAttString(SENSOR_NAME_GATTNAME, Sensor_Name, &
                                     Message_Log=Message_Log )
    END IF

    ! The PLATFORM_NAME
    n = n + 1
    IF ( PRESENT( Platform_Name ) ) THEN
      Put_Status(n) = Put_GAttString(PLATFORM_NAME_GATTNAME, Platform_Name, &
                                     Message_Log=Message_Log )
    END IF

    ! The COMMENT
    n = n + 1
    IF ( PRESENT( Comment ) ) THEN
      Put_Status(n) = Put_GAttString(COMMENT_GATTNAME, Comment, &
                                     Message_Log=Message_Log )
    END IF

    ! The ID_TAG
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

  END FUNCTION Write_TauCoeff_GAtts


!--------------------------------------------------------------------------------
!
! NAME:
!       Read_TauCoeff_GAtts
!
! PURPOSE:
!       Function to read the global attributes from a netCDF TauCoeff data file.
!
! CALLING SEQUENCE:
!       Error_Status = Read_TauCoeff_GAtts( NC_Filename,                   &  ! Input
!                                           NC_FileID,                     &  ! Input
!                                           Title         = Title,         &  ! Optional output
!                                           History       = History,       &  ! Optional output
!                                           Sensor_Name   = Sensor_Name,   &  ! Optional output
!                                           Platform_Name = Platform_Name, &  ! Optional output
!                                           Comment       = Comment,       &  ! Optional output
!                                           ID_Tag        = ID_Tag,        &  ! Optional output
!                                           Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF TauCoeff format data file to create.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:        NetCDF file ID number returned from the
!                         Open_ or Create_TauCoeff_netCDF() function.
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
!                         ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF TauCoeff file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF TauCoeff file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Sensor_Name:      Character string written into the SENSOR_NAME
!                         global attribute field of the netCDF TauCoeff
!                         file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Platform_Name:    Character string written into the PLATFORM_NAME
!                         global attribute field of the netCDF TauCoeff
!                         file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF TauCoeff file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       ID_Tag:           Character string written into the ID_TAG global
!                         attribute field of the netCDF TauCoeff file.
!                         Should contain a short tag used to identify the
!                         dependent profile set used to generate the 
!                         coefficient data.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error status.
!                         The error codes are defined in the Message_Handler module.
!                         If == SUCCESS the global attribute read was successful
!                            == FAILURE an error occurred reading the requested
!                                       global attribute(s).
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION Read_TauCoeff_GAtts( NC_Filename,   &  ! Input
                                NC_FileID,     &  ! Input
                                Title,         &  ! Optional output
                                History,       &  ! Optional output
                                Sensor_Name,   &  ! Optional output
                                Platform_Name, &  ! Optional output
                                Comment,       &  ! Optional output
                                ID_Tag,        &  ! Optional output
                                Message_Log )  &  ! Error messaging
                              RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER,                INTENT(IN)  :: NC_FileID
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Sensor_Name
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Platform_Name
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: ID_Tag
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_TauCoeff_GAtts'
    INTEGER, PARAMETER :: nGetGAtts = 6
    ! Local variables
    INTEGER :: Get_Status(nGetGAtts), n

    ! Set up
    Error_Status = SUCCESS
    Get_Status   = SUCCESS
    n = 0

    ! The TITLE
    n = n + 1
    IF ( PRESENT( Title ) ) THEN
      Get_Status(n) = Get_GAttString(TITLE_GATTNAME, Title, &
                                     Message_Log=Message_Log )
    END IF

    ! The HISTORY
    n = n + 1
    IF ( PRESENT( History ) ) THEN
      Get_Status(n) = Get_GAttString(HISTORY_GATTNAME, History, &
                                     Message_Log=Message_Log )
    END IF

    ! The SENSOR_NAME
    n = n + 1
    IF ( PRESENT( Sensor_Name ) ) THEN
      Get_Status(n) = Get_GAttString(SENSOR_NAME_GATTNAME, Sensor_Name, &
                                     Message_Log=Message_Log )
    END IF

    ! The PLATFORM_NAME
    n = n + 1
    IF ( PRESENT( Platform_Name ) ) THEN
      Get_Status(n) = Get_GAttString(PLATFORM_NAME_GATTNAME, Platform_Name, &
                                     Message_Log=Message_Log )
    END IF

    ! The COMMENT
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
      CHARACTER( 10000 ) :: LongString
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

  END FUNCTION Read_TauCoeff_GAtts


!--------------------------------------------------------------------------------
!
! NAME:
!       Create_TauCoeff_netCDF
!
! PURPOSE:
!       Function to create a netCDF TauCoeff data file for writing.
!
! CALLING SEQUENCE:
!       Error_Status = Create_TauCoeff_netCDF( NC_Filename,                   &  ! Input
!                                              n_Orders,                      &  ! Input
!                                              n_Predictors,                  &  ! Input
!                                              n_Absorbers,                   &  ! Input
!                                              n_Channels,                    &  ! Input
!                                              NC_FileID,                     &  ! Output
!                                              Title         = Title,         &  ! Optional input
!                                              History       = History,       &  ! Optional input
!                                              Sensor_Name   = Sensor_Name,   &  ! Optional input
!                                              Platform_Name = Platform_Name, &  ! Optional input
!                                              Comment       = Comment,       &  ! Optional input
!                                              ID_Tag        = ID_Tag,        &  ! Optional input
!                                              Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:        Character string specifying the name of the
!                           netCDF TauCoeff format data file to create.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Orders:           The maximum order of polynomial used to reconstruct 
!                           the gas absorption coefficients.
!                           NOTE: The data arrays using this dimension value are
!                                 dimensioned as 0:n_Orders, where the
!                                 0'th term is the offset. Therefore the actual
!                                 number of array elements along this dimension
!                                 is n_Orders+1
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Predictors:       The number of predictor functions used in generating
!                           the TauCoeff data.
!                           NOTE: The data arrays using this dimension value are
!                                 dimensioned as 0:n_Predictors, where the 0'th
!                                 term is the offset. Therefore the actual number
!                                 of array elements along this dimension is
!                                 n_Predictors+1
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Absorbers:        The number of absorbers dimension of the TauCoeff data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Channels:         The number of channels dimension of the TauCoeff data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Title:              Character string written into the TITLE global
!                           attribute field of the netCDF TauCoeff file.
!                           Should contain a succinct description of what
!                           is in the netCDF datafile.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the netCDF TauCoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Sensor_Name:        Character string written into the SENSOR_NAME
!                           global attribute field of the netCDF TauCoeff
!                           file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Platform_Name:      Character string written into the PLATFORM_NAME
!                           global attribute field of the netCDF TauCoeff
!                           file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF TauCoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       ID_Tag:             Character string written into the ID_TAG global
!                           attribute field of the netCDF TauCoeff file.
!                           Should contain a short tag used to identify the
!                           dependent profile set used to generate the 
!                           coefficient data.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Message_Log:        Character string specifying a filename in which
!                           any messages will be logged. If not specified,
!                           or if an error occurs opening the log file, the
!                           default action is to output messages to standard
!                           output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       NC_FileID:          NetCDF file ID number to be used for subsequent
!                           writing to the output file.
!                           UNITS:      N/A
!                           TYPE:       Integer
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the Message_Handler module.
!                           If == SUCCESS the netCDF file creation was successful.
!                              == FAILURE an unrecoverable error occurred.
!                              == WARNING an error occurred writing any of the requested
!                                         global file attributes.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       - If the output data file already exists, it is overwritten.
!       - The created netCDF file is in DATA mode upon exiting this function.
!       - If a FAILURE error occurs, the created netCDF file is closed.
!
!--------------------------------------------------------------------------------

  FUNCTION Create_TauCoeff_netCDF( NC_Filename,   &  ! Input
                                   n_Orders,      &  ! Input
                                   n_Predictors,  &  ! Input
                                   n_Absorbers,   &  ! Input
                                   n_Channels,    &  ! Input
                                   NC_FileID,     &  ! Output
                                   Title,         &  ! Optional input
                                   History,       &  ! Optional input
                                   Sensor_Name,   &  ! Optional input
                                   Platform_Name, &  ! Optional input
                                   Comment,       &  ! Optional input
                                   ID_Tag,        &  ! Optional input
                                   Message_Log )  &  ! Error messaging
                                 RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER,                INTENT(IN)  :: n_Orders
    INTEGER,                INTENT(IN)  :: n_Predictors
    INTEGER,                INTENT(IN)  :: n_Absorbers
    INTEGER,                INTENT(IN)  :: n_Channels
    INTEGER,                INTENT(OUT) :: NC_FileID
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Sensor_Name
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Platform_Name
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: ID_Tag
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Create_TauCoeff_netCDF'
    ! Local variables
    INTEGER :: NF90_Status
    INTEGER :: NF90_Status1, NF90_Status2
    INTEGER :: Order_dimID
    INTEGER :: Predictor_dimID
    INTEGER :: Absorber_dimID
    INTEGER :: Channel_dimID
    INTEGER :: StrLen_dimID
    INTEGER :: varID
    TYPE(TauCoeff_type) :: TCdummy

    ! Set up
    Error_Status = SUCCESS

    ! Check dimensions
    IF ( n_Orders     < 1 .OR. &
         n_Predictors < 1 .OR. &
         n_Absorbers  < 1 .OR. &
         n_Channels   < 1      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'All dimensions must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Create the data file
    NF90_Status = NF90_CREATE( NC_Filename,  &
                               NF90_CLOBBER, &
                               NC_FileID     )
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
    ! The maximum polynomial order. Note that the defined
    ! dimension value is "n + 1" as the array elements for
    ! this dimension ranges from 0 -> n.
    Error_Status = Def_Dim(ORDER_DIMNAME, n_Orders+1, Order_DimID )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! The number of predictors. Note that the defined
    ! dimension value is "n + 1" as the array elements for
    ! this dimension ranges from 0 -> n.
    Error_Status = Def_Dim(PREDICTOR_DIMNAME, n_Predictors+1, Predictor_DimID )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! The number of sensor channels
    Error_Status = Def_Dim(CHANNEL_DIMNAME, n_Channels, Channel_DimID )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! The number of absorbers
    Error_Status = Def_Dim(ABSORBER_DIMNAME, n_Absorbers, Absorber_DimID )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! The Sensor_Descriptor string length
    Error_Status = Def_Dim(STRLEN_DIMNAME, TCDummy%StrLen, StrLen_DimID )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Write the global attributes
    Error_Status = Write_TauCoeff_GAtts( TRIM( NC_Filename ), &
                                         NC_FileID, &
                                         Title         = Title, &
                                         History       = History, &
                                         Sensor_Name   = Sensor_Name, &
                                         Platform_Name = Platform_Name, &
                                         Comment       = Comment, &
                                         ID_Tag        = ID_Tag, &
                                         Message_Log = Message_Log )
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
    ! Define the release variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                RELEASE_VARNAME, &
                                RELEASE_TYPE, &
                                varid = varID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//RELEASE_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF
    NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                varID, &
                                LONGNAME_ATTNAME, &
                                RELEASE_LONGNAME )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//RELEASE_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! Define the version variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                VERSION_VARNAME, &
                                VERSION_TYPE, &
                                varid = varID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//VERSION_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF
    NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                varID, &
                                LONGNAME_ATTNAME, &
                                VERSION_LONGNAME )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//VERSION_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! Define the Sensor descriptor variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                SENSOR_DESCRIPTOR_VARNAME, &
                                SENSOR_DESCRIPTOR_TYPE, &
                                dimids = (/ StrLen_DimID, Channel_DimID /), &
                                varid  = VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//SENSOR_DESCRIPTOR_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF
    NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                VarID, &
                                LONGNAME_ATTNAME, &
                                SENSOR_DESCRIPTOR_LONGNAME )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//SENSOR_DESCRIPTOR_VARNAME//' variable attributes to '// &
                            TRIM( NC_fileNAME )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! Define the NCEP Sensor ID variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                NCEP_SENSOR_ID_VARNAME, &
                                NCEP_SENSOR_ID_TYPE, &
                                dimids = Channel_DimID, &
                                varid  = VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//NCEP_SENSOR_ID_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF
    NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                VarID, &
                                LONGNAME_ATTNAME, &
                                NCEP_SENSOR_ID_LONGNAME )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//NCEP_SENSOR_ID_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! Define the WMO satellite ID variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                WMO_SATELLITE_ID_VARNAME, &
                                WMO_SATELLITE_ID_TYPE, &
                                dimids = Channel_DimID, &
                                varid  = VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//WMO_SATELLITE_ID_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF
    NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                VarID, &
                                LONGNAME_ATTNAME, &
                                WMO_SATELLITE_ID_LONGNAME )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//WMO_SATELLITE_ID_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! Define the WMO Sensor ID variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                WMO_SENSOR_ID_VARNAME, &
                                WMO_SENSOR_ID_TYPE, &
                                dimids = Channel_DimID, &
                                varid  = VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//WMO_SENSOR_ID_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF
    NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                VarID, &
                                LONGNAME_ATTNAME, &
                                WMO_SENSOR_ID_LONGNAME )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//WMO_SENSOR_ID_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! Define the Sensor channel list variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                SENSOR_CHANNEL_VARNAME, &
                                SENSOR_CHANNEL_TYPE, &
                                dimids = Channel_DimID, &
                                varid  = VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//SENSOR_CHANNEL_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF
    NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                VarID, &
                                LONGNAME_ATTNAME, &
                                SENSOR_CHANNEL_LONGNAME )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//SENSOR_CHANNEL_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! Define the Absorber ID variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                ABSORBER_ID_VARNAME, &
                                ABSORBER_ID_TYPE, &
                                dimids = Absorber_dimID, &
                                varid  = varID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//ABSORBER_ID_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF
    NF90_Status1 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 LONGNAME_ATTNAME, &
                                 ABSORBER_ID_LONGNAME )
    NF90_Status2 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 UNITS_ATTNAME, &
                                 ABSORBER_ID_UNITS )
    IF ( NF90_Status1 /= NF90_NOERR .OR. &
         NF90_Status2 /= NF90_NOERR      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//ABSORBER_ID_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! Define the Alpha value variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                ALPHA_VARNAME, &
                                ALPHA_TYPE, &
                                dimids = Absorber_dimID, &
                                varid  = varID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//ALPHA_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF
    NF90_Status1 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 LONGNAME_ATTNAME, &
                                 ALPHA_LONGNAME )

    NF90_Status2 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 UNITS_ATTNAME, &
                                 ALPHA_UNITS )
    IF ( NF90_Status1 /= NF90_NOERR .OR. &
         NF90_Status2 /= NF90_NOERR      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//ALPHA_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! Define the Alpha C1 value variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                ALPHA_C1_VARNAME, &
                                ALPHA_C1_TYPE, &
                                dimids = Absorber_dimID, &
                                varid  = varID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//ALPHA_C1_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF
    NF90_Status1 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 LONGNAME_ATTNAME, &
                                 ALPHA_C1_LONGNAME )
    NF90_Status2 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 UNITS_ATTNAME, &
                                 ALPHA_C1_UNITS )
    IF ( NF90_Status1 /= NF90_NOERR .OR. &
         NF90_Status2 /= NF90_NOERR      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//ALPHA_C1_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! Define the Alpha C2 value variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                ALPHA_C2_VARNAME, &
                                ALPHA_C2_TYPE, &
                                dimids = Absorber_dimID, &
                                varid  = varID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//ALPHA_C2_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF
    NF90_Status1 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 LONGNAME_ATTNAME, &
                                 ALPHA_C2_LONGNAME )
    NF90_Status2 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 UNITS_ATTNAME, &
                                 ALPHA_C2_UNITS )
    IF ( NF90_Status1 /= NF90_NOERR .OR. &
         NF90_Status2 /= NF90_NOERR      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//ALPHA_C2_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! Define the Order indices variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                ORDER_INDEX_VARNAME, &
                                ORDER_INDEX_TYPE, &
                                dimids = (/ Predictor_dimID, &
                                            Absorber_dimID, &
                                            Channel_dimID /), &
                                varid  = varID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//ORDER_INDEX_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF
    NF90_Status1 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 LONGNAME_ATTNAME, &
                                 ORDER_INDEX_LONGNAME )
    NF90_Status2 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 UNITS_ATTNAME, &
                                 ORDER_INDEX_UNITS )
    IF ( NF90_Status1 /= NF90_NOERR .OR. &
         NF90_Status2 /= NF90_NOERR      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//ORDER_INDEX_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! Define the Predictor indices variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                PREDICTOR_INDEX_VARNAME, &
                                PREDICTOR_INDEX_TYPE, &
                                dimids = (/ Predictor_dimID, &
                                            Absorber_dimID, &
                                            Channel_dimID /), &
                                varid  = varID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//PREDICTOR_INDEX_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF
    NF90_Status1 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 LONGNAME_ATTNAME, &
                                 PREDICTOR_INDEX_LONGNAME )

    NF90_Status2 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 UNITS_ATTNAME, &
                                 PREDICTOR_INDEX_UNITS )
    IF ( NF90_Status1 /= NF90_NOERR .OR. &
         NF90_Status2 /= NF90_NOERR      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//PREDICTOR_INDEX_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! Define the Gas absorption coefficients variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                TAU_COEFFICIENTS_VARNAME, &
                                TAU_COEFFICIENTS_TYPE, &
                                dimids = (/ Order_dimID, &
                                            Predictor_dimID, &
                                            Absorber_dimID, &
                                            Channel_dimID /), &
                                varid  = varID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//TAU_COEFFICIENTS_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF
    NF90_Status1 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 LONGNAME_ATTNAME, &
                                 TAU_COEFFICIENTS_LONGNAME )
    NF90_Status2 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 UNITS_ATTNAME, &
                                 TAU_COEFFICIENTS_UNITS )
    IF ( NF90_Status1 /= NF90_NOERR .OR. &
         NF90_Status2 /= NF90_NOERR      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TAU_COEFFICIENTS_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! Take the netcdf file out of define mode
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

  END FUNCTION Create_TauCoeff_netCDF


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!
! NAME:
!       Inquire_TauCoeff_netCDF
!
! PURPOSE:
!       Function to inquire a netCDF TauCoeff format file to obtain the
!       dimension values and global attributes.
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_TauCoeff_netCDF( NC_Filename,                   &  ! Input
!                                               n_Orders      = n_Orders,      &  ! Optional output
!                                               n_Predictors  = n_Predictors,  &  ! Optional output
!                                               n_Absorbers   = n_Absorbers,   &  ! Optional output
!                                               n_Channels    = n_Channels,    &  ! Optional output
!                                               Release       = Release,       &  ! Optional Output
!                                               Version       = Version,       &  ! Optional Output
!                                               Title         = Title,         &  ! Optional output
!                                               History       = History,       &  ! Optional output
!                                               Sensor_Name   = Sensor_Name,   &  ! Optional output
!                                               Platform_Name = Platform_Name, &  ! Optional output
!                                               Comment       = Comment,       &  ! Optional output
!                                               ID_Tag        = ID_Tag,        &  ! Optional output
!                                               RCS_Id        = RCS_Id,        &  ! Revision control
!                                               Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the netCDF TauCoeff
!                         format data file. Used only for message output.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:      Character string specifying a filename in which any
!                         messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output messages to standard output.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Orders:           The maximum polynomial order used in reconstructing
!                           the gas absorption coefficients.
!                           NOTE: The data arrays using this dimension value are
!                                 dimensioned as 0:n_Orders, where the
!                                 0'th term is the offset. Therefore the actual
!                                 number of array elements along this dimension
!                                 is n_Orders+1
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       n_Predictors:       The number of predictor functions used in generating
!                           the TauCoeff data.
!                           NOTE: The data arrays using this dimension value are
!                                 dimensioned as 0:n_Predictors, where the 0'th
!                                 term is the offset. Therefore the actual number
!                                 of array elements along this dimension is
!                                 n_Predictors+1
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       n_Absorbers:        The number of absorbers dimension of the TauCoeff data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       n_Channels:         The number of channels dimension of the TauCoeff data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Release:            The TauCoeff data/file release number. Used to check
!                           for data/software mismatch.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Version:            The TauCoeff data/file version number. Used for
!                           purposes only in identifying the dataset for
!                           a particular release.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Title:              Character string written into the TITLE global
!                           attribute field of the netCDF TauCoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the netCDF TauCoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Sensor_Name:        Character string written into the SENSOR_NAME global
!                           attribute field of the netCDF TauCoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Platform_Name:      Character string written into the PLATFORM_NAME global
!                           attribute field of the netCDF TauCoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF TauCoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       ID_Tag:             Character string written into the ID_TAG global
!                           attribute field of the netCDF TauCoeff file.
!                           Should contain a short tag used to identify the
!                           dependent profile set used to generate the 
!                           coefficient data.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the Message_Handler module.
!                           If == SUCCESS the netCDF file inquiry was successful
!                              == FAILURE an error occurred reading any of the
!                                         requested data.
!                              == WARNING an error occurred closing the netCDF
!                                         file after a successful read.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION Inquire_TauCoeff_netCDF( NC_Filename,   &  ! Input
                                    n_Orders,      &  ! Optional output
                                    n_Predictors,  &  ! Optional output
                                    n_Absorbers,   &  ! Optional output
                                    n_Channels,    &  ! Optional output
                                    Release,       &  ! Optional output
                                    Version,       &  ! Optional output
                                    Title,         &  ! Optional output
                                    History,       &  ! Optional output
                                    Sensor_Name,   &  ! Optional output
                                    Platform_Name, &  ! Optional output
                                    Comment,       &  ! Optional output
                                    ID_Tag,        &  ! Optional output
                                    RCS_Id,        &  ! Revision control
                                    Message_Log )  &  ! Error messaging
                                  RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Orders
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Predictors
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Absorbers
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Channels
    INTEGER,      OPTIONAL, INTENT(OUT) :: Release
    INTEGER,      OPTIONAL, INTENT(OUT) :: Version
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Sensor_Name
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Platform_Name
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: ID_Tag
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Inquire_TauCoeff_netCDF'
    ! Function variables
    CHARACTER(256) :: Message
    INTEGER :: Close_Status
    INTEGER :: NC_FileID
    INTEGER :: iOrder
    INTEGER :: iPredictor

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Open the file
    Error_Status = Open_TauCoeff_netCDF( TRIM( NC_Filename ), &
                                         NC_FileID, &
                                         Mode = 'READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF TauCoeff data file '//TRIM( NC_Filename )
      GOTO 2000
    END IF

    ! Get the dimensions
    !
    ! The maximum polynomial order
    IF ( PRESENT( n_Orders ) ) THEN
      Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                           ORDER_DIMNAME, &
                                           iOrder, &
                                           Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error obtaining '//ORDER_DIMNAME//' dimension from '//&
                  TRIM( NC_Filename )
        GOTO 1000
      END IF
      ! Subtract one to account for the 0'th term.
      n_Orders = iOrder - 1
    END IF

    ! The number of predictors
    IF ( PRESENT( n_Predictors ) ) THEN
      Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                           PREDICTOR_DIMNAME, &
                                           iPredictor, &
                                           Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error obtaining '//PREDICTOR_DIMNAME//' dimension from '//&
                  TRIM( NC_Filename )
        GOTO 1000
      END IF
      ! Subtract one to account for the 0'th term.
      n_Predictors = iPredictor - 1
    END IF

    ! The number of absorbers
    IF ( PRESENT( n_Absorbers ) ) THEN
      Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                           ABSORBER_DIMNAME, &
                                           n_Absorbers, &
                                           Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error obtaining '//ABSORBER_DIMNAME//' dimension from '//&
                  TRIM( NC_Filename )
        GOTO 1000
      END IF
    END IF

    ! The number of channels
    IF ( PRESENT( n_Channels ) ) THEN
      Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                           CHANNEL_DIMNAME, &
                                           n_Channels, &
                                           Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error obtaining '//CHANNEL_DIMNAME//' dimension from '//&
                  TRIM( NC_Filename )
        GOTO 1000
      END IF
    END IF


    ! Get release/version information
    !
    ! File release
    IF ( PRESENT( Release ) ) THEN
      Error_Status = Get_netCDF_Variable( NC_FileID, &
                                          RELEASE_VARNAME, &
                                          Release, &
                                          Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error obtaining '//RELEASE_VARNAME//' value from '//&
                  TRIM( NC_Filename )
        GOTO 1000
      END IF
    END IF

    ! File version
    IF ( PRESENT( Version ) ) THEN
      Error_Status = Get_netCDF_Variable( NC_FileID, &
                                          VERSION_VARNAME, &
                                          Version, &
                                          Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error obtaining '//VERSION_VARNAME//' value from '//&
                  TRIM( NC_Filename )
        GOTO 1000
      END IF
    END IF

    ! Get the global attributes
    Error_Status = Read_TauCoeff_GAtts( TRIM( NC_Filename ), &
                                        NC_FileID, &
                                        Title         = Title, &
                                        History       = History, &
                                        Sensor_Name   = Sensor_Name, &
                                        Platform_Name = Platform_Name, &
                                        Comment       = Comment, &
                                        ID_Tag        = ID_Tag, &
                                        Message_Log   = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading global attribute from '// &
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! Close the file
    Close_Status = Close_TauCoeff_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauCoeff data file '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

    RETURN


    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

    1000 CONTINUE
    Close_Status = Close_TauCoeff_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing input file during error cleanup.'

    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Inquire_TauCoeff_netCDF


!--------------------------------------------------------------------------------
!
! NAME:
!       Write_TauCoeff_netCDF
!
! PURPOSE:
!       Function to write TauCoeff data to a netCDF format TauCoeff file.
!
! CALLING SEQUENCE:
!         Error_Status = Write_TauCoeff_netCDF( NC_Filename,                   &  ! Input
!                                               TauCoeff,                      &  ! Input
!                                               Title         = Title,         &  ! Optional input
!                                               History       = History,       &  ! Optional input
!                                               Sensor_Name   = Sensor_Name,   &  ! Optional input
!                                               Platform_Name = Platform_Name, &  ! Optional input
!                                               Comment       = Comment,       &  ! Optional input
!                                               ID_Tag        = ID_Tag,        &  ! Optional input
!                                               Quiet         = Quiet,         &  ! Optional input
!                                               RCS_Id        = RCS_Id,        &  ! Revision control
!                                               Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:     Character string specifying the name of the output
!                        netCDF TauCoeff format data file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       TauCoeff:        Structure to write to file.
!                        UNITS:      N/A
!                        TYPE:       TauCoeff_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Title:           Character string written into the TITLE global
!                        attribute field of the netCDF TauCoeff file.
!                        Should contain a succinct description of what
!                        is in the netCDF datafile.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       History:         Character string written into the HISTORY global
!                        attribute field of the netCDF TauCoeff file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Sensor_Name:     Character string written into the SENSOR_NAME
!                        global attribute field of the netCDF TauCoeff
!                        file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Platform_Name:   Character string written into the PLATFORM_NAME
!                        global attribute field of the netCDF TauCoeff
!                        file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Comment:         Character string written into the COMMENT global
!                        attribute field of the netCDF TauCoeff file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       ID_Tag:          Character string written into the ID_TAG global
!                        attribute field of the netCDF TauCoeff file.
!                        Should contain a short tag used to identify the
!                        dependent profile set used to generate the 
!                        coefficient data.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Quiet:           Set this keyword to suppress information messages being
!                        printed to standard output (or the message log file if
!                        the MESSAGE_LOG optional argument is used.) By default,
!                        information messages are printed.
!                        If QUIET = 0, information messages are OUTPUT.
!                           QUIET = 1, information messages are SUPPRESSED.
!                        UNITS:      N/A
!                        TYPE:       Integer
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT(IN)
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
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the netCDF file write was successful
!                           == FAILURE - the input TauCoeff structure contains
!                                        unassociated pointer members, or
!                                      - a unrecoverable write error occurred.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       If the output file exists, it is overwritten.
!
!--------------------------------------------------------------------------------

  FUNCTION Write_TauCoeff_netCDF( NC_Filename,   &  ! Input
                                  TauCoeff,      &  ! Input
                                  Title,         &  ! Optional input
                                  History,       &  ! Optional input
                                  Sensor_Name,   &  ! Optional input
                                  Platform_Name, &  ! Optional input
                                  Comment,       &  ! Optional input
                                  ID_Tag,        &  ! Optional input
                                  Quiet,         &  ! Optional input
                                  RCS_Id,        &  ! Revision control
                                  Message_Log )  &  ! Error messaging
                                  RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    TYPE(TauCoeff_type),    INTENT(IN)  :: TauCoeff
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Sensor_Name
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Platform_Name
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: ID_Tag
    INTEGER,      OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_TauCoeff_netCDF'
    ! Local variables
    CHARACTER(256) :: Message
    LOGICAL :: Noisy
    INTEGER :: NC_FileID
    INTEGER :: Close_Status

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
    IF ( .NOT. Associated_TauCoeff( TauCoeff ) ) THEN
      Message = 'Some or all INPUT TauCoeff pointer members are NOT associated.'
      GOTO 2000
    END IF

    ! Check structure release
    Error_Status = Check_TauCoeff_Release( TauCoeff, &
                                           Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'TauCoeff Release check failed.'
      GOTO 2000
    END IF

    ! Create the output file
    Error_Status = Create_TauCoeff_netCDF( NC_Filename, &
                                           TauCoeff%n_Orders, &
                                           TauCoeff%n_Predictors, &
                                           TauCoeff%n_Absorbers, &
                                           TauCoeff%n_Channels, &
                                           NC_FileID, &
                                           Title         = Title, &
                                           History       = History, &
                                           Sensor_Name   = Sensor_Name, &
                                           Platform_Name = Platform_Name, &
                                           Comment       = Comment, &
                                           ID_Tag        = ID_Tag, &
                                           Message_Log   = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating output netCDF TauCoeff file '//&
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! Write the TauCoeff data
    !
    ! The Release number
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        RELEASE_VARNAME, &
                                        TauCoeff%Release )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//RELEASE_VARNAME//' number to '// &
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The Version number
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        VERSION_VARNAME, &
                                        TauCoeff%Version )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//VERSION_VARNAME//' number to '// &
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The sensor descriptor
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        SENSOR_DESCRIPTOR_VARNAME, &
                                        TauCoeff%Sensor_Descriptor )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//SENSOR_DESCRIPTOR_VARNAME//' to '// &
                TRIM( NC_fileNAME )
      GOTO 1000
    END IF

    ! The NCEP_SENSOR_ID
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        NCEP_SENSOR_ID_VARNAME, &
                                        TauCoeff%NCEP_Sensor_ID )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//NCEP_SENSOR_ID_VARNAME//' to '// &
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The WMO_SATELLITE_ID
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        WMO_SATELLITE_ID_VARNAME, &
                                        TauCoeff%WMO_Satellite_ID )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//WMO_SATELLITE_ID_VARNAME//' to '// &
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The WMO_SENSOR_ID
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        WMO_SENSOR_ID_VARNAME, &
                                        TauCoeff%WMO_Sensor_ID )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//WMO_SENSOR_ID_VARNAME//' to '// &
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The channel list
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        SENSOR_CHANNEL_VARNAME, &
                                        TauCoeff%Sensor_Channel )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//SENSOR_CHANNEL_VARNAME//' to '// &
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The absorber ID
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        ABSORBER_ID_VARNAME, &
                                        TauCoeff%Absorber_ID )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//ABSORBER_ID_VARNAME//' to '//&
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The Alpha value
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        ALPHA_VARNAME, &
                                        TauCoeff%Alpha )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//ALPHA_VARNAME//' to '//&
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The Alpha_C1 value
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        ALPHA_C1_VARNAME, &
                                        TauCoeff%Alpha_C1 )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//ALPHA_C1_VARNAME//' to '//&
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The Alpha_C2 value
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        ALPHA_C2_VARNAME, &
                                        TauCoeff%Alpha_C2 )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//ALPHA_C2_VARNAME//' to '//&
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The polynomial order indices
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        ORDER_INDEX_VARNAME, &
                                        TauCoeff%Order_Index )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//ORDER_INDEX_VARNAME//' to '//&
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The predictor indices
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        PREDICTOR_INDEX_VARNAME, &
                                        TauCoeff%Predictor_Index )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//PREDICTOR_INDEX_VARNAME//' to '//&
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The gas absorption coefficients
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        TAU_COEFFICIENTS_VARNAME, &
                                        TauCoeff%C )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//TAU_COEFFICIENTS_VARNAME//' to '//&
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! Close the file
    Close_Status = Close_TauCoeff_netCDF( NC_FileID )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauCoeff data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    ! Outut an info message
    IF ( Noisy ) THEN
      CALL Info_TauCoeff( TauCoeff, message )
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
    Close_Status = Close_TauCoeff_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing output file during error cleanup.'

    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )

  END FUNCTION Write_TauCoeff_netCDF


!--------------------------------------------------------------------------------
!
! NAME:
!       Read_TauCoeff_netCDF
!
! PURPOSE:
!       Function to read data from a netCDF format TauCoeff file.
!
! CALLING SEQUENCE:
!         Error_Status = Read_TauCoeff_netCDF( NC_Filename,                   &  ! Input
!                                              TauCoeff,                      &  ! Output
!                                              Quiet         = Quiet,         &  ! Optional input
!                                              Title         = Title,         &  ! Optional output
!                                              History       = History,       &  ! Optional output
!                                              Sensor_Name   = Sensor_Name,   &  ! Optional output
!                                              Platform_Name = Platform_Name, &  ! Optional output
!                                              Comment       = Comment,       &  ! Optional output
!                                              ID_Tag        = ID_Tag,        &  ! Optional output
!                                              RCS_Id        = RCS_Id,        &  ! Revision control
!                                              Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:     Character string specifying the name of the netCDF TauCoeff
!                        format data file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
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
!                        ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       TauCoeff:        Structure to contain the gas absorption coefficient
!                        data read from the file.
!                        UNITS:      N/A
!                        TYPE:       TauCoeff_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Title:           Character string written into the TITLE global
!                        attribute field of the netCDF TauCoeff file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       History:         Character string written into the HISTORY global
!                        attribute field of the netCDF TauCoeff file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Sensor_Name:     Character string written into the SENSOR_NAME global
!                        attribute field of the netCDF TauCoeff file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Platform_Name:   Character string written into the PLATFORM_NAME global
!                        attribute field of the netCDF TauCoeff file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Comment:         Character string written into the COMMENT global
!                        attribute field of the netCDF TauCoeff file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       ID_Tag:          Character string written into the ID_TAG global
!                        attribute field of the netCDF TauCoeff file.
!                        Should contain a short tag used to identify the
!                        dependent profile set used to generate the 
!                        coefficient data.
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
! SIDE EFFECTS:
!       If the TauCoeff argument is defined upon input, it is redefined (or
!       reinitialised) at output.
!
! COMMENTS:
!       Note the INTENT on the output TauCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION Read_TauCoeff_netCDF( NC_Filename,   &  ! Input
                                 TauCoeff,      &  ! Output
                                 Quiet,         &  ! Optional input
                                 Title,         &  ! Optional output
                                 History,       &  ! Optional output
                                 Sensor_Name,   &  ! Optional output
                                 Platform_Name, &  ! Optional output
                                 Comment,       &  ! Optional output
                                 ID_Tag,        &  ! Optional output
                                 RCS_Id,        &  ! Revision control
                                 Message_Log )  &  ! Error messaging
                               RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)     :: NC_Filename
    TYPE(TauCoeff_type),    INTENT(IN OUT) :: TauCoeff
    INTEGER,      OPTIONAL, INTENT(IN)     :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: Sensor_Name
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: Platform_Name
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: ID_Tag
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_TauCoeff_netCDF'
    ! Function variables
    CHARACTER(1000)  :: Message
    LOGICAL :: Noisy
    INTEGER :: Destroy_Status
    INTEGER :: Close_Status
    INTEGER :: NC_FileID
    INTEGER :: n_Orders
    INTEGER :: n_Predictors
    INTEGER :: n_Absorbers
    INTEGER :: n_Channels

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF

    ! Inquire the file
    Error_Status = Inquire_TauCoeff_netCDF( TRIM( NC_Filename ), &
                                            n_Orders      = n_Orders, &
                                            n_Predictors  = n_Predictors, &
                                            n_Absorbers   = n_Absorbers, &
                                            n_Channels    = n_Channels, &
                                            Release       = TauCoeff%Release, &
                                            Version       = TauCoeff%Version, &
                                            Title         = Title, &
                                            History       = History, &
                                            Sensor_Name   = Sensor_Name, &
                                            Platform_Name = Platform_Name, &
                                            Comment       = Comment, &
                                            ID_Tag        = ID_Tag, &
                                            Message_Log   = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining TauCoeff dimensions/attributes from '//&
                TRIM( NC_Filename )
      GOTO 2000
    END IF

    ! Check the release
    Error_Status = Check_TauCoeff_Release( TauCoeff, &
                                           Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'TauCoeff Release check failed for '//TRIM( NC_Filename )
      GOTO 2000
    END IF

    ! Allocate the output structure
    Error_Status = Allocate_TauCoeff( n_Orders, &
                                      n_Predictors, &
                                      n_Absorbers, &
                                      n_Channels, &
                                      TauCoeff, &
                                      Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error occurred allocating TauCoeff structure.'
      GOTO 2000
    END IF

    ! Open the file for reading
    Error_Status = Open_TauCoeff_netCDF( TRIM( NC_Filename ), &
                                         NC_FileID, &
                                         Mode = 'READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF TauCoeff data file '//&
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! Read the TauCoeff data
    !
    ! The sensor descriptor
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        SENSOR_DESCRIPTOR_VARNAME, &
                                        TauCoeff%Sensor_Descriptor )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//SENSOR_DESCRIPTOR_VARNAME//' from '// &
                TRIM( NC_fileNAME )
      GOTO 1000
    END IF

    ! The NCEP_SENSOR_ID
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        NCEP_SENSOR_ID_VARNAME, &
                                        TauCoeff%NCEP_Sensor_ID )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//NCEP_SENSOR_ID_VARNAME//' from '// &
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The WMO_SATELLITE_ID
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        WMO_SATELLITE_ID_VARNAME, &
                                        TauCoeff%WMO_Satellite_ID )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//WMO_SATELLITE_ID_VARNAME//' from '// &
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The WMO_SENSOR_ID
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        WMO_SENSOR_ID_VARNAME, &
                                        TauCoeff%WMO_Sensor_ID )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//WMO_SENSOR_ID_VARNAME//' from '// &
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The channel list
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        SENSOR_CHANNEL_VARNAME, &
                                        TauCoeff%Sensor_Channel )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//SENSOR_CHANNEL_VARNAME//' from '// &
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The absorber ID variable
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        ABSORBER_ID_VARNAME, &
                                        TauCoeff%Absorber_ID )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//ABSORBER_ID_VARNAME//' from '//&
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The Alpha value
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        ALPHA_VARNAME, &
                                        TauCoeff%Alpha )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//ALPHA_VARNAME//' from '//&
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The Alpha_C1 value
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        ALPHA_C1_VARNAME, &
                                        TauCoeff%Alpha_C1 )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//ALPHA_C1_VARNAME//' from '//&
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The Alpha_C2 value
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        ALPHA_C2_VARNAME, &
                                        TauCoeff%Alpha_C2 )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//ALPHA_C2_VARNAME//' from '//&
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The order indices
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        ORDER_INDEX_VARNAME, &
                                        TauCoeff%ORDER_Index )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//ORDER_INDEX_VARNAME//' from '//&
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The predictor indices
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        PREDICTOR_INDEX_VARNAME, &
                                        TauCoeff%Predictor_Index )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//PREDICTOR_INDEX_VARNAME//' from '//&
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The gas absorption coefficients
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        TAU_COEFFICIENTS_VARNAME, &
                                        TauCoeff%C )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//TAU_COEFFICIENTS_VARNAME//' from '//&
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! Close the file
    Close_Status = Close_TauCoeff_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF TauCoeff data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    ! Count the number of sensors
    CALL Count_TauCoeff_Sensors( TauCoeff )

    ! Output an info message
    IF ( Noisy ) THEN
      CALL Info_TauCoeff( TauCoeff, Message )
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
    Destroy_Status = Destroy_TauCoeff(TauCoeff, Message_Log=Message_Log)
    IF ( Destroy_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error destroying TauCoeff during error cleanup.'
    Close_Status = Close_TauCoeff_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing input file during error cleanup.'

    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Read_TauCoeff_netCDF

END MODULE TauCoeff_netCDF_IO
