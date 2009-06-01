
! Module continaing routines for netCDF I/O 
! of RadDiag_Stats structures

MODULE RadDiag_Stats_netCDF_IO


  ! ----------
  ! Module use
  ! ----------
  USE Type_Kinds, ONLY: sp=>Single
  USE File_Utility
  USE Message_Handler
  USE RadDiag_Stats_Define
  USE netcdf
  USE netCDF_Utility,  Open_RadDiag_Stats_netCDF =>  Open_netCDF, &
                      Close_RadDiag_Stats_netCDF => Close_netCDF


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Parameters inherited from the 
  ! RadDiag_Stats_Define module
  PUBLIC :: invalidFOV
  PUBLIC :: nVariables
  PUBLIC :: iBC   
  PUBLIC :: iNBC  
  PUBLIC :: iScan 
  PUBLIC :: iConst
  PUBLIC :: iAngle
  PUBLIC :: iLpsR 
  PUBLIC :: iLpsR2
  PUBLIC :: iCLW
  ! RadDiag_Stats structure data type definition
  ! inherited from the RadDiag_Stats_Define module
  PUBLIC :: RadDiag_Stats_type
  ! RadDiag_Stats structure routines inherited
  ! from the RadDiag_Stats_Define module
  PUBLIC :: Associated_RadDiag_Stats
  PUBLIC :: Destroy_RadDiag_Stats
  PUBLIC :: Allocate_RadDiag_Stats
  ! Routines in this module
  PUBLIC :: Inquire_RadDiag_Stats_netCDF
  PUBLIC :: Read_RadDiag_Stats_netCDF
  PUBLIC :: Write_RadDiag_Stats_netCDF


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id$'

  INTEGER, PRIVATE, PARAMETER :: UNSET = 0
  INTEGER, PRIVATE, PARAMETER ::   SET = 1

  ! Global attribute names. Case sensitive
  CHARACTER(*), PRIVATE, PARAMETER :: TITLE_GATTNAME         = 'title' 
  CHARACTER(*), PRIVATE, PARAMETER :: HISTORY_GATTNAME       = 'history' 
  CHARACTER(*), PRIVATE, PARAMETER :: SENSOR_NAME_GATTNAME   = 'sensor_name' 
  CHARACTER(*), PRIVATE, PARAMETER :: PLATFORM_NAME_GATTNAME = 'platform_name' 
  CHARACTER(*), PRIVATE, PARAMETER :: COMMENT_GATTNAME       = 'comment' 

  ! Dimension names. Case sensitive
  CHARACTER(*), PRIVATE, PARAMETER :: PREDICTOR_DIMNAME    = 'nPredictors'
  CHARACTER(*), PRIVATE, PARAMETER :: CHANNEL_DIMNAME      = 'nChannels'
  CHARACTER(*), PRIVATE, PARAMETER :: FOV_DIMNAME          = 'nFOVs'
  CHARACTER(*), PRIVATE, PARAMETER :: TIME_DIMNAME         = 'nTimes'
  CHARACTER(*), PRIVATE, PARAMETER :: VARIABLENAME_DIMNAME = 'nVariables'
  CHARACTER(*), PRIVATE, PARAMETER :: STRLEN_DIMNAME       = 'StrLen'

  ! Variable names. Case sensitive.
  CHARACTER(*), PRIVATE, PARAMETER :: AIRMASSCOEFF_VARNAME  = 'AirMassCoefficients'
  CHARACTER(*), PRIVATE, PARAMETER :: VARIABLENAME_VARNAME  = 'VariableNames'
  CHARACTER(*), PRIVATE, PARAMETER :: CHANNEL_VARNAME       = 'Channel'
  CHARACTER(*), PRIVATE, PARAMETER :: FOV_VARNAME           = 'FOV'
  CHARACTER(*), PRIVATE, PARAMETER :: SCAN_DATA_VARNAME     = 'scan_Data'
  CHARACTER(*), PRIVATE, PARAMETER :: SCAN_NSAMPLES_VARNAME = 'scan_nSamples'
  CHARACTER(*), PRIVATE, PARAMETER :: DATETIME_VARNAME      = 'DateTime'
  CHARACTER(*), PRIVATE, PARAMETER :: TIME_DATA_VARNAME     = 'time_Data'
  CHARACTER(*), PRIVATE, PARAMETER :: TIME_NSAMPLES_VARNAME = 'time_nSamples'

  ! Variable description attribute.
  CHARACTER(*), PRIVATE, PARAMETER :: DESCRIPTION_ATTNAME = 'description'
  CHARACTER(*), PRIVATE, PARAMETER :: AIRMASSCOEFF_DESCRIPTION  = 'The air mass bias correction predictor coefficients'
  CHARACTER(*), PRIVATE, PARAMETER :: VARIABLENAME_DESCRIPTION  = 'List of the names of the data variables'
  CHARACTER(*), PRIVATE, PARAMETER :: CHANNEL_DESCRIPTION       = 'Sensor channel number (not necessarily contiguous)'
  CHARACTER(*), PRIVATE, PARAMETER :: FOV_DESCRIPTION           = 'FOV scan positions used in scan average'
  CHARACTER(*), PRIVATE, PARAMETER :: SCAN_DATA_DESCRIPTION     = 'Scan averaged Obs-Calc differences and bias correction terms'
  CHARACTER(*), PRIVATE, PARAMETER :: SCAN_NSAMPLES_DESCRIPTION = 'Number of valid samples used in scan average'
  CHARACTER(*), PRIVATE, PARAMETER :: DATETIME_DESCRIPTION      = 'Dates/Times used in time average'
  CHARACTER(*), PRIVATE, PARAMETER :: TIME_DATA_DESCRIPTION     = 'Time averaged Obs-Calc differences and bias correction terms'
  CHARACTER(*), PRIVATE, PARAMETER :: TIME_NSAMPLES_DESCRIPTION = 'Number of valid samples used in time average'

  ! Variable long name attribute.
  CHARACTER(*), PRIVATE, PARAMETER :: LONGNAME_ATTNAME = 'long_name'
  CHARACTER(*), PRIVATE, PARAMETER :: AIRMASSCOEFF_LONGNAME  = 'Air mass bias coefficients'
  CHARACTER(*), PRIVATE, PARAMETER :: VARIABLENAME_LONGNAME  = 'Data variable names'
  CHARACTER(*), PRIVATE, PARAMETER :: CHANNEL_LONGNAME       = 'Channel number'
  CHARACTER(*), PRIVATE, PARAMETER :: FOV_LONGNAME           = 'FOV scan positions'
  CHARACTER(*), PRIVATE, PARAMETER :: SCAN_DATA_LONGNAME     = 'Scan averaged data'
  CHARACTER(*), PRIVATE, PARAMETER :: SCAN_NSAMPLES_LONGNAME = 'Number of valid samples'
  CHARACTER(*), PRIVATE, PARAMETER :: DATETIME_LONGNAME      = 'Date/Time'
  CHARACTER(*), PRIVATE, PARAMETER :: TIME_DATA_LONGNAME     = 'Time averaged data'
  CHARACTER(*), PRIVATE, PARAMETER :: TIME_NSAMPLES_LONGNAME = 'Number of valid samples'

  ! Variable units attribute.
  CHARACTER(*), PRIVATE, PARAMETER :: UNITS_ATTNAME = 'units'
  CHARACTER(*), PRIVATE, PARAMETER :: AIRMASSCOEFF_UNITS  = 'Variable'
  CHARACTER(*), PRIVATE, PARAMETER :: VARIABLENAME_UNITS  = 'N/A'
  CHARACTER(*), PRIVATE, PARAMETER :: CHANNEL_UNITS       = 'N/A'
  CHARACTER(*), PRIVATE, PARAMETER :: FOV_UNITS           = 'N/A'
  CHARACTER(*), PRIVATE, PARAMETER :: SCAN_DATA_UNITS     = 'Kelvin'
  CHARACTER(*), PRIVATE, PARAMETER :: SCAN_NSAMPLES_UNITS = 'N/A'
  CHARACTER(*), PRIVATE, PARAMETER :: DATETIME_UNITS      = 'N/A'
  CHARACTER(*), PRIVATE, PARAMETER :: TIME_DATA_UNITS     = 'Kelvin'
  CHARACTER(*), PRIVATE, PARAMETER :: TIME_NSAMPLES_UNITS = 'N/A'

  ! Variable _FillValue attribute.
  CHARACTER(*), PRIVATE, PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'
  INTEGER,      PRIVATE, PARAMETER :: IP_FILLVALUE = -1
  REAL(sp),     PRIVATE, PARAMETER :: FP_FILLVALUE = 0.0_sp
  REAL(sp),     PRIVATE, PARAMETER :: AIRMASSCOEFF_FILLVALUE  = FP_FILLVALUE
  INTEGER,      PRIVATE, PARAMETER :: CHANNEL_FILLVALUE       = IP_FILLVALUE
  INTEGER,      PRIVATE, PARAMETER :: FOV_FILLVALUE           = IP_FILLVALUE
  REAL(sp),     PRIVATE, PARAMETER :: SCAN_DATA_FILLVALUE     = FP_FILLVALUE
  INTEGER,      PRIVATE, PARAMETER :: SCAN_NSAMPLES_FILLVALUE = IP_FILLVALUE
  INTEGER,      PRIVATE, PARAMETER :: DATETIME_FILLVALUE      = IP_FILLVALUE
  REAL(sp),     PRIVATE, PARAMETER :: TIME_DATA_FILLVALUE     = FP_FILLVALUE
  INTEGER,      PRIVATE, PARAMETER :: TIME_NSAMPLES_FILLVALUE = IP_FILLVALUE

  ! Variable netCDF datatypes
  INTEGER, PRIVATE, PARAMETER :: AIRMASSCOEFF_TYPE  = NF90_FLOAT
  INTEGER, PRIVATE, PARAMETER :: VARIABLENAME_TYPE  = NF90_CHAR
  INTEGER, PRIVATE, PARAMETER :: CHANNEL_TYPE       = NF90_INT
  INTEGER, PRIVATE, PARAMETER :: FOV_TYPE           = NF90_INT
  INTEGER, PRIVATE, PARAMETER :: SCAN_DATA_TYPE     = NF90_FLOAT
  INTEGER, PRIVATE, PARAMETER :: SCAN_NSAMPLES_TYPE = NF90_INT
  INTEGER, PRIVATE, PARAMETER :: DATETIME_TYPE      = NF90_INT
  INTEGER, PRIVATE, PARAMETER :: TIME_DATA_TYPE     = NF90_FLOAT
  INTEGER, PRIVATE, PARAMETER :: TIME_NSAMPLES_TYPE = NF90_INT


  ! ---------
  ! Overloads
  ! ---------

  INTERFACE Put_VarAtts
    MODULE PROCEDURE Put_VarAttsInt
    MODULE PROCEDURE Put_VarAttsReal
  END INTERFACE Put_VarAtts

  INTERFACE Def_Var
    MODULE PROCEDURE Def_VarScalar
    MODULE PROCEDURE Def_VarArray
  END INTERFACE Def_Var


CONTAINS


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################



  ! Function to write the string variable attributes
  ! to a netCDF RadDiag_Stats data file
  !
  FUNCTION Put_VarAttStrings(NC_FileID, NC_Filename, &
                             VarName, &
                             DescriptionAtt, LongNameAtt, UnitsAtt, &
                             Message_Log) &
                             RESULT(Error_Status)
    INTEGER,      INTENT(IN)           :: NC_FileID
    CHARACTER(*), INTENT(IN)           :: NC_Filename
    CHARACTER(*), INTENT(IN)           :: VarName
    CHARACTER(*), INTENT(IN)           :: DescriptionAtt
    CHARACTER(*), INTENT(IN)           :: LongNameAtt
    CHARACTER(*), INTENT(IN)           :: UnitsAtt
    CHARACTER(*), INTENT(IN), OPTIONAL :: Message_Log
    INTEGER :: Error_Status
    CHARACTER(*), PARAMETER :: ROUTINE_NAME='Put_VarAttsStrings'
    INTEGER :: Put_Status(3)
    INTEGER :: NF90_Status
    Error_Status = SUCCESS
    Put_Status(1) = Put_netCDF_Attribute(NC_FileID, &
                                         DESCRIPTION_ATTNAME, &
                                         TRIM(DescriptionAtt), &
                                         Variable_Name = TRIM(VarName))
    Put_Status(2) = Put_netCDF_Attribute(NC_FileID, &
                                         LONGNAME_ATTNAME, &
                                         TRIM(LongNameAtt), &
                                         Variable_Name = TRIM(VarName))
    Put_Status(3) = Put_netCDF_Attribute(NC_FileID, &
                                         UNITS_ATTNAME, &
                                         TRIM(UnitsAtt), &
                                         Variable_Name = TRIM(VarName))
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM(VarName)//' variable attributes to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
    END IF
  END FUNCTION Put_VarAttStrings



  ! Function to write variable attributes to a netCDF
  ! RadDiag_Stats data file for an INTEGER FillValue attribute.
  !
  FUNCTION Put_VarAttsInt(NC_FileID, NC_Filename, &
                          VarName, &
                          DescriptionAtt, LongNameAtt, UnitsAtt, FillValueAtt, &
                          Message_Log) &
                          RESULT(Error_Status)
    INTEGER,      INTENT(IN)           :: NC_FileID
    CHARACTER(*), INTENT(IN)           :: NC_Filename
    CHARACTER(*), INTENT(IN)           :: VarName
    CHARACTER(*), INTENT(IN)           :: DescriptionAtt
    CHARACTER(*), INTENT(IN)           :: LongNameAtt
    CHARACTER(*), INTENT(IN)           :: UnitsAtt
    INTEGER,      INTENT(IN)           :: FillValueAtt
    CHARACTER(*), INTENT(IN), OPTIONAL :: Message_Log
    INTEGER :: Error_Status
    CHARACTER(*), PARAMETER :: ROUTINE_NAME='Put_VarAttsInt'
    INTEGER :: NF90_Status

    ! Write string attributes
    Error_Status = Put_VarAttStrings(NC_FileID, NC_Filename, &
                                     VarName, &
                                     DescriptionAtt, LongNameAtt, UnitsAtt, &
                                     Message_Log=Message_Log)
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Write FillValue attribute
    Error_Status = Put_netCDF_Attribute(NC_FileID, &
                                        FILLVALUE_ATTNAME, &
                                        FillValueAtt, &
                                        Variable_Name = TRIM(VarName))
    IF ( Error_Status  /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM(VarName)//' variable attributes to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
    END IF
  END FUNCTION Put_VarAttsInt



  ! Function to write variable attributes to a netCDF
  ! RadDiag_Stats data file for a REAL FillValue attribute.
  !
  FUNCTION Put_VarAttsReal(NC_FileID, NC_Filename, &
                           VarName, &
                           DescriptionAtt, LongNameAtt, UnitsAtt, FillValueAtt, &
                           Message_Log) &
                           RESULT(Error_Status)
    INTEGER,      INTENT(IN)           :: NC_FileID
    CHARACTER(*), INTENT(IN)           :: NC_Filename
    CHARACTER(*), INTENT(IN)           :: VarName
    CHARACTER(*), INTENT(IN)           :: DescriptionAtt
    CHARACTER(*), INTENT(IN)           :: LongNameAtt
    CHARACTER(*), INTENT(IN)           :: UnitsAtt
    REAL(sp),     INTENT(IN)           :: FillValueAtt
    CHARACTER(*), INTENT(IN), OPTIONAL :: Message_Log
    INTEGER :: Error_Status
    CHARACTER(*), PARAMETER :: ROUTINE_NAME='Put_VarAttsReal'
    INTEGER :: NF90_Status

    ! Write string attributes
    Error_Status = Put_VarAttStrings(NC_FileID, NC_Filename, &
                                     VarName, &
                                     DescriptionAtt, LongNameAtt, UnitsAtt)
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Write FillValue attribute
    Error_Status = Put_netCDF_Attribute(NC_FileID, &
                                        FILLVALUE_ATTNAME, &
                                        FillValueAtt, &
                                        Variable_Name = TRIM(VarName))
    IF ( Error_Status  /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM(VarName)//' variable attributes to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
    END IF
  END FUNCTION Put_VarAttsReal



  ! Function to define a scalar variable
  !
  FUNCTION Def_VarScalar(NC_FileID, NC_Filename, & 
                         VarName, VarType, VarID, &
                         Message_Log) &
                         RESULT(Error_Status)
    INTEGER,      INTENT(IN)           :: NC_FileID
    CHARACTER(*), INTENT(IN)           :: NC_Filename
    CHARACTER(*), INTENT(IN)           :: VarName
    INTEGER,      INTENT(IN)           :: VarType
    INTEGER,      INTENT(OUT)          :: VarID
    CHARACTER(*), INTENT(IN), OPTIONAL :: Message_Log
    INTEGER :: Error_Status
    CHARACTER(*), PARAMETER :: ROUTINE_NAME='Def_VarScalar'
    INTEGER :: NF90_Status
    Error_Status = SUCCESS
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                TRIM(VarName), &
                                VarType, &
                                varid = VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//TRIM(VarName)//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
    END IF
  END FUNCTION Def_VarScalar



  ! Function to define an array variable
  !
  FUNCTION Def_VarArray(NC_FileID, NC_Filename, & 
                        VarName, VarType, VarDimID, VarID, &
                        Message_Log) &
                        RESULT(Error_Status)
    INTEGER,      INTENT(IN)           :: NC_FileID
    CHARACTER(*), INTENT(IN)           :: NC_Filename
    CHARACTER(*), INTENT(IN)           :: VarName
    INTEGER,      INTENT(IN)           :: VarType
    INTEGER,      INTENT(IN)           :: VarDimID(:)
    INTEGER,      INTENT(OUT)          :: VarID
    CHARACTER(*), INTENT(IN), OPTIONAL :: Message_Log
    INTEGER :: Error_Status
    CHARACTER(*), PARAMETER :: ROUTINE_NAME='Def_VarArray'
    INTEGER :: NF90_Status
    Error_Status = SUCCESS
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                TRIM(VarName), &
                                VarType, &
                                dimids = VarDimID, &
                                varid = VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//TRIM(VarName)//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
    END IF
  END FUNCTION Def_VarArray



  ! Function to write the global attributes to a netCDF
  ! RadDiag_Stats data file.

  FUNCTION Write_RadDiag_Stats_GAtts( NC_Filename,   &  ! Input
                                      NC_FileID,     &  ! Input
                                      Title,         &  ! Optional input
                                      History,       &  ! Optional input
                                      Sensor_Name,   &  ! Optional input
                                      Platform_Name, &  ! Optional input
                                      Comment,       &  ! Optional input
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
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log

    ! Function result
    INTEGER :: Error_Status

    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_RadDiag_Stats_GAtts'
    CHARACTER(*), PARAMETER :: WRITE_MODULE_HISTORY_GATTNAME   = 'write_module_history'
    CHARACTER(*), PARAMETER :: CREATION_DATE_AND_TIME_GATTNAME = 'creation_date_and_time'
    INTEGER, PARAMETER :: nPutGAtts = 7

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
                                   Message_Log = Message_Log )

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

    ! Check for any errors
    IF ( ANY( Put_Status /= SUCCESS ) ) Error_Status = WARNING

  CONTAINS

    FUNCTION Put_GAttString(GAttName, GAttString, Message_Log) RESULT(Error_Status)
      CHARACTER(*),             INTENT(IN) :: GAttName
      CHARACTER(*),             INTENT(IN) :: GAttString
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

  END FUNCTION Write_RadDiag_Stats_GAtts


  ! Function to read the global attributes from
  ! a netCDF RadDiag_Stats data file.

  FUNCTION Read_RadDiag_Stats_GAtts( NC_Filename,   &  ! Input
                                     NC_FileID,     &  ! Input
                                     Title,         &  ! Optional output
                                     History,       &  ! Optional output
                                     Sensor_Name,   &  ! Optional output
                                     Platform_Name, &  ! Optional output
                                     Comment,       &  ! Optional output
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
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log

    ! Function result
    INTEGER :: Error_Status

    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_RadDiag_Stats_GAtts'
    INTEGER, PARAMETER :: nGetGAtts = 5

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

    ! Check for any errors
    IF ( ANY( Get_Status /= SUCCESS ) ) Error_Status = WARNING

  CONTAINS

    FUNCTION Get_GAttString(GAttName, GAttString, Message_Log) RESULT(Error_Status)
      CHARACTER(*),           INTENT(IN)   :: GAttName
      CHARACTER(*),           INTENT(OUT)  :: GAttString
      CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
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
      GAttString = LongString(1:MIN( LEN( GAttString ), LEN_TRIM( LongString ) ))
    END FUNCTION Get_GAttString

  END FUNCTION Read_RadDiag_Stats_GAtts



  ! Function to create a netCDF format
  ! RadDiag_Stats file for writing

  FUNCTION Create_RadDiag_Stats_netCDF( NC_Filename,   &  ! Input
                                        nPredictors,   &  ! Input
                                        nChannels,     &  ! Input
                                        nFOVs,         &  ! Input
                                        nTimes,        &  ! Input
                                        nVariables,    &  ! Input
                                        NC_FileID,     &  ! Output
                                        Title,         &  ! Optional input
                                        History,       &  ! Optional input
                                        Sensor_Name,   &  ! Optional input
                                        Platform_Name, &  ! Optional input
                                        Comment,       &  ! Optional input
                                        Message_Log )  &  ! Error messaging
                                      RESULT ( Error_Status )

    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER,                INTENT(IN)  :: nPredictors    ! I
    INTEGER,                INTENT(IN)  :: nChannels      ! L
    INTEGER,                INTENT(IN)  :: nFOVs          ! Is
    INTEGER,                INTENT(IN)  :: nTimes         ! It
    INTEGER,                INTENT(IN)  :: nVariables     ! N
    INTEGER,                INTENT(OUT) :: NC_FileID
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title         
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History       
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Sensor_Name   
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Platform_Name 
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment       
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log

    ! Function result
    INTEGER :: Error_Status

    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Create_RadDiag_Stats_netCDF'

    ! Local variables
    INTEGER :: NF90_Status
    INTEGER :: nPredictors_DimID
    INTEGER :: nChannels_DimID
    INTEGER :: nFOVs_DimID
    INTEGER :: nTimes_DimID
    INTEGER :: nVariables_DimID
    INTEGER :: StrLen_DimID
    INTEGER :: varID
    TYPE( RadDiag_Stats_type ) :: RadDiag_Stats_Dummy


    ! ---------------------------
    ! Create the netCDF data file
    ! ---------------------------
    NF90_Status = NF90_CREATE( TRIM( NC_Filename ), &
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


    ! ---------------------
    ! Define the dimensions
    ! ---------------------
    ! The number of Predictors
    Error_Status = Def_Dim(PREDICTOR_DIMNAME, nPredictors, nPredictors_DimID )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! The number of channels
    Error_Status = Def_Dim(CHANNEL_DIMNAME, nChannels, nChannels_DimID )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! The number of FOVs
    Error_Status = Def_Dim(FOV_DIMNAME, nFOVs, nFOVs_DimID )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! The number of timess
    Error_Status = Def_Dim(TIME_DIMNAME, nTimes, nTimes_DimID )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! The number of variables
    Error_Status = Def_Dim(VARIABLENAME_DIMNAME, nVariables, nVariables_DimID )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! The stringlength
    Error_Status = Def_Dim(STRLEN_DIMNAME, RadDiag_Stats_Dummy%StrLen, StrLen_DimID )
    IF ( Error_Status /= SUCCESS ) RETURN


    ! ---------------------------
    ! Write the global attributes
    ! ---------------------------
    Error_Status = Write_RadDiag_Stats_GAtts( TRIM( NC_Filename ), &
                                              NC_FileID, &
                                              Title         = Title, &
                                              History       = History, &
                                              Sensor_Name   = Sensor_Name, &
                                              Platform_Name = Platform_Name, &
                                              Comment       = Comment, &
                                              Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing global attribute to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! -----------------------------------------
    ! Define the variables and write attributes
    ! -----------------------------------------
    ! AirMassCoeff array
    IF ( Def_Var(NC_FileID, NC_Filename, &
                 AIRMASSCOEFF_VARNAME, AIRMASSCOEFF_TYPE, &
                 (/nPredictors_DimID, nChannels_DimID, nTimes_DimID/), VarID ) /= SUCCESS ) RETURN

    IF ( Put_VarAtts(NC_FileID, NC_Filename, &
                     AIRMASSCOEFF_VARNAME, &
                     AIRMASSCOEFF_DESCRIPTION, AIRMASSCOEFF_LONGNAME, &
                     AIRMASSCOEFF_UNITS, AIRMASSCOEFF_FILLVALUE) /= SUCCESS ) RETURN

    ! Channel array
    IF ( Def_Var(NC_FileID, NC_Filename, &
                 CHANNEL_VARNAME, CHANNEL_TYPE, &
                 (/nChannels_DimID/), VarID ) /= SUCCESS ) RETURN

    IF ( Put_VarAtts(NC_FileID, NC_Filename, &
                     CHANNEL_VARNAME, &
                     CHANNEL_DESCRIPTION, CHANNEL_LONGNAME, &
                     CHANNEL_UNITS, CHANNEL_FILLVALUE) /= SUCCESS ) RETURN

    ! FOV array
    IF ( Def_Var(NC_FileID, NC_Filename, &
                 FOV_VARNAME, FOV_TYPE, &
                 (/nFOVs_DimID/), VarID ) /= SUCCESS ) RETURN

    IF ( Put_VarAtts(NC_FileID, NC_Filename, &
                     FOV_VARNAME, &
                     FOV_DESCRIPTION, FOV_LONGNAME, &
                     FOV_UNITS, FOV_FILLVALUE) /= SUCCESS ) RETURN

    ! SCAN_DATA array
    IF ( Def_Var(NC_FileID, NC_Filename, &
                 SCAN_DATA_VARNAME, SCAN_DATA_TYPE, &
                 (/nVariables_DimID,nChannels_DimID,nFOVs_DimID/), VarID ) /= SUCCESS ) RETURN

    IF ( Put_VarAtts(NC_FileID, NC_Filename, &
                     SCAN_DATA_VARNAME, &
                     SCAN_DATA_DESCRIPTION, SCAN_DATA_LONGNAME, &
                     SCAN_DATA_UNITS, SCAN_DATA_FILLVALUE) /= SUCCESS ) RETURN

    ! SCAN_NSAMPLES array
    IF ( Def_Var(NC_FileID, NC_Filename, &
                 SCAN_NSAMPLES_VARNAME, SCAN_NSAMPLES_TYPE, &
                 (/nChannels_DimID,nFOVs_DimID/), VarID ) /= SUCCESS ) RETURN

    IF ( Put_VarAtts(NC_FileID, NC_Filename, &
                     SCAN_NSAMPLES_VARNAME, &
                     SCAN_NSAMPLES_DESCRIPTION, SCAN_NSAMPLES_LONGNAME, &
                     SCAN_NSAMPLES_UNITS, SCAN_NSAMPLES_FILLVALUE) /= SUCCESS ) RETURN

    ! DATETIME array
    IF ( Def_Var(NC_FileID, NC_Filename, &
                 DATETIME_VARNAME, DATETIME_TYPE, &
                 (/nTimes_DimID/), VarID ) /= SUCCESS ) RETURN

    IF ( Put_VarAtts(NC_FileID, NC_Filename, &
                     DATETIME_VARNAME, &
                     DATETIME_DESCRIPTION, DATETIME_LONGNAME, &
                     DATETIME_UNITS, DATETIME_FILLVALUE) /= SUCCESS ) RETURN

    ! TIME_DATA array
    IF ( Def_Var(NC_FileID, NC_Filename, &
                 TIME_DATA_VARNAME, TIME_DATA_TYPE, &
                 (/nVariables_DimID,nChannels_DimID,nTimes_DimID/), VarID ) /= SUCCESS ) RETURN

    IF ( Put_VarAtts(NC_FileID, NC_Filename, &
                     TIME_DATA_VARNAME, &
                     TIME_DATA_DESCRIPTION, TIME_DATA_LONGNAME, &
                     TIME_DATA_UNITS, TIME_DATA_FILLVALUE) /= SUCCESS ) RETURN

    ! TIME_NSAMPLES array
    IF ( Def_Var(NC_FileID, NC_Filename, &
                 TIME_NSAMPLES_VARNAME, TIME_NSAMPLES_TYPE, &
                 (/nChannels_DimID,nTimes_DimID/), VarID ) /= SUCCESS ) RETURN

    IF ( Put_VarAtts(NC_FileID, NC_Filename, &
                     TIME_NSAMPLES_VARNAME, &
                     TIME_NSAMPLES_DESCRIPTION, TIME_NSAMPLES_LONGNAME, &
                     TIME_NSAMPLES_UNITS, TIME_NSAMPLES_FILLVALUE) /= SUCCESS ) RETURN

    ! VARIABLENAME array
    IF ( Def_Var(NC_FileID, NC_Filename, &
                 VARIABLENAME_VARNAME, VARIABLENAME_TYPE, &
                 (/StrLen_DimID, nVariables_DimID/), VarID ) /= SUCCESS ) RETURN

    IF ( Put_VarAttStrings(NC_FileID, NC_Filename,       &
                           VARIABLENAME_VARNAME,     &
                           VARIABLENAME_DESCRIPTION, &
                           VARIABLENAME_LONGNAME,    &
                           VARIABLENAME_UNITS        ) /= SUCCESS ) RETURN


    ! -----------------------------------
    ! Take netCDF file out of define mode
    ! -----------------------------------
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

  END FUNCTION Create_RadDiag_Stats_netCDF





!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

  ! Function to inquire a netCDF RadDiag_Stats format file to obtain the 
  ! dimensions and global attributes.
  !
  FUNCTION Inquire_RadDiag_Stats_netCDF( NC_Filename,   &  ! Input
                                         nPredictors,   &  ! Optional output
                                         nChannels,     &  ! Optional output
                                         nFOVs,         &  ! Optional output
                                         nTimes,        &  ! Optional output
                                         nVariables,    &  ! Optional output
                                         Title,         &  ! Optional output
                                         History,       &  ! Optional output
                                         Sensor_Name,   &  ! Optional output
                                         Platform_Name, &  ! Optional output
                                         Comment,       &  ! Optional output
                                         RCS_Id,        &  ! Version control
                                         Message_Log )  &  ! Error messaging
                                       RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER,      OPTIONAL, INTENT(OUT) :: nPredictors
    INTEGER,      OPTIONAL, INTENT(OUT) :: nChannels
    INTEGER,      OPTIONAL, INTENT(OUT) :: nFOVs
    INTEGER,      OPTIONAL, INTENT(OUT) :: nTimes
    INTEGER,      OPTIONAL, INTENT(OUT) :: nVariables
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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Inquire_RadDiag_Stats_netCDF'

    ! Function variables
    INTEGER :: NF90_Status
    INTEGER :: Close_Status
    INTEGER :: NC_FileID
    INTEGER :: File_DataType
    INTEGER :: nK_Status


    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Open the netCDF file
    Error_Status = Open_RadDiag_Stats_netCDF( TRIM( NC_Filename ), &
                                              NC_FileID, &
                                              Mode = 'READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening netCDF RadDiag_Stats data file '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ------------------
    ! Get the dimensions
    ! ------------------
    ! The Predictor dimension
    IF ( PRESENT( nPredictors ) ) THEN
      Error_Status = Get_Dim( PREDICTOR_DIMNAME, nPredictors, Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) RETURN
    END IF

    ! The channel dimension
    IF ( PRESENT( nChannels ) ) THEN
      Error_Status = Get_Dim( CHANNEL_DIMNAME, nChannels, Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) RETURN
    END IF

    ! The FOV dimension
    IF ( PRESENT( nFOVs ) ) THEN
      Error_Status = Get_Dim( FOV_DIMNAME, nFOVs, Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) RETURN
    END IF

    ! The time dimension
    IF ( PRESENT( nTimes ) ) THEN
      Error_Status = Get_Dim( TIME_DIMNAME, nTimes, Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) RETURN
    END IF

    ! The number of variables dimension
    IF ( PRESENT( nVariables ) ) THEN
      Error_Status = Get_Dim( VARIABLENAME_DIMNAME, nVariables, Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) RETURN
    END IF

    ! ------------------------
    ! Get theglobal attributes
    ! ------------------------
    Error_Status = Read_RadDiag_Stats_GAtts( TRIM( NC_Filename ),           &
                                             NC_FileID,                     &
                                             Title         = Title,         &
                                             History       = History,       &
                                             Sensor_Name   = Sensor_Name,   &
                                             Platform_Name = Platform_Name, &
                                             Comment       = Comment,       &
                                             Message_Log   = Message_Log    )
    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading global attributes from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! --------------
    ! Close the file
    ! --------------
    Close_Status = Close_RadDiag_Stats_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF RadDiag_Stats data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

  CONTAINS

    FUNCTION Get_Dim(DimName, DimValue, Message_Log) RESULT(Error_Status)
      CHARACTER(*),           INTENT(IN)  :: DimName
      INTEGER,                INTENT(OUT) :: DimValue
      CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
      INTEGER :: Error_Status
      Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                           TRIM(DimName), &
                                           DimValue, &
                                           Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error obtaining '//TRIM(DimName)//' dimension from '//&
                              TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF

    END FUNCTION Get_Dim

  END FUNCTION Inquire_RadDiag_Stats_netCDF



  ! Function to write RadDiag_Stats data to a
  ! netCDF format RadDiag_Stats file.
  !
  FUNCTION Write_RadDiag_Stats_netCDF( NC_Filename,   &  ! Input
                                       RadDiag_Stats, &  ! Input
                                       Title,         &  ! Optional input
                                       History,       &  ! Optional input
                                       Sensor_Name,   &  ! Optional input
                                       Platform_Name, &  ! Optional input
                                       Comment,       &  ! Optional input
                                       RCS_Id,        &  ! Version control
                                       Message_Log )  &  ! Error messaging
                                     RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),               INTENT(IN)  :: NC_Filename
    TYPE( RadDiag_Stats_type ), INTENT(IN)  :: RadDiag_Stats
    CHARACTER(*),     OPTIONAL, INTENT(IN)  :: Title
    CHARACTER(*),     OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*),     OPTIONAL, INTENT(IN)  :: Sensor_Name
    CHARACTER(*),     OPTIONAL, INTENT(IN)  :: Platform_Name
    CHARACTER(*),     OPTIONAL, INTENT(IN)  :: Comment
    CHARACTER(*),     OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*),     OPTIONAL, INTENT(IN)  :: Message_Log

    ! Function result
    INTEGER :: Error_Status

    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_RadDiag_Stats_netCDF'

    ! Local variables
    CHARACTER( 256 ) :: Message
    CHARACTER( 256 ) :: VarName
    INTEGER :: NF90_Status
    INTEGER :: Close_Status
    INTEGER :: NC_FileID
    INTEGER :: i, l, is, it, n

    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check structure pointer association status
    IF ( .NOT. Associated_RadDiag_Stats( RadDiag_Stats ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT RadDiag_Stats pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ----------------------
    ! Create the netCDF file
    ! ----------------------
    Error_Status = Create_RadDiag_Stats_netCDF( TRIM( NC_Filename ),           &  ! Input
                                                RadDiag_Stats%nPredictors,     &  ! Input
                                                RadDiag_Stats%nChannels,       &  ! Input
                                                RadDiag_Stats%nFOVs,           &  ! Input
                                                RadDiag_Stats%nTimes,          &  ! Input
                                                RadDiag_Stats%nVariables,      &  ! Input
                                                NC_FileID,                     &  ! Output
                                                Title         = Title,         &  ! Optional input
                                                History       = History,       &  ! Optional input
                                                Sensor_Name   = Sensor_Name,   &  ! Optional input
                                                Platform_Name = Platform_Name, &  ! Optional input
                                                Comment       = Comment,       &  ! Optional input
                                                Message_Log   = Message_Log    )  ! Error messaging
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error creating netCDF RadDiag_Stats data file '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------------
    ! Write the data items
    ! --------------------
    i  = RadDiag_Stats%nPredictors
    l  = RadDiag_Stats%nChannels
    is = RadDiag_Stats%nFOVs
    it = RadDiag_Stats%nTimes
    n  = RadDiag_Stats%nVariables

    ! The AirMassCoefficient array
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        AIRMASSCOEFF_VARNAME, &
                                        RadDiag_Stats%AirMassCoefficients(:i,:l,:it) )
    IF ( Error_Status /= SUCCESS ) THEN
      VarName = AIRMASSCOEFF_VARNAME
      GOTO 1000
    END IF

    ! The Channel array
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        CHANNEL_VARNAME, &
                                        RadDiag_Stats%Channel(:l) )
    IF ( Error_Status /= SUCCESS ) THEN
      VarName = CHANNEL_VARNAME
      GOTO 1000
    END IF

    ! The FOV array
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        FOV_VARNAME, &
                                        RadDiag_Stats%FOV(:is) )
    IF ( Error_Status /= SUCCESS ) THEN
      VarName = FOV_VARNAME
      GOTO 1000
    END IF

    ! The scan_Data array
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        SCAN_DATA_VARNAME, &
                                        RadDiag_Stats%scan_Data(:n,:l,:is) )
    IF ( Error_Status /= SUCCESS ) THEN
      VarName = SCAN_DATA_VARNAME
      GOTO 1000
    END IF

    ! The scan_nSamples array
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        SCAN_NSAMPLES_VARNAME, &
                                        RadDiag_Stats%scan_nSamples(:l,:is) )
    IF ( Error_Status /= SUCCESS ) THEN
      VarName = SCAN_NSAMPLES_VARNAME
      GOTO 1000
    END IF

    ! The DateTime array
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        DATETIME_VARNAME, &
                                        RadDiag_Stats%DateTime(:it) )
    IF ( Error_Status /= SUCCESS ) THEN
      VarName = DATETIME_VARNAME
      GOTO 1000
    END IF

    ! The time_Data array
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        TIME_DATA_VARNAME, &
                                        RadDiag_Stats%time_Data(:n,:l,:it) )
    IF ( Error_Status /= SUCCESS ) THEN
      VarName = TIME_DATA_VARNAME
      GOTO 1000
    END IF

    ! The time_nSamples array
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        TIME_NSAMPLES_VARNAME, &
                                        RadDiag_Stats%time_nSamples(:l,:it) )
    IF ( Error_Status /= SUCCESS ) THEN
      VarName = TIME_NSAMPLES_VARNAME
      GOTO 1000
    END IF

    ! The VariableNames array
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        VARIABLENAME_VARNAME, &
                                        RadDiag_Stats%VariableNames(:n) )
    IF ( Error_Status /= SUCCESS ) THEN
      VarName = VARIABLENAME_VARNAME
      GOTO 1000
    END IF


    ! --------------
    ! Close the file
    ! --------------
    Close_Status = Close_RadDiag_Stats_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF RadDiag_Stats data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    RETURN


    ! --------------
    ! Process errors
    ! --------------
    1000 CONTINUE
    CALL Display_Message( ROUTINE_NAME, &
                          'Error writing '//TRIM(VarName)//' to '// &
                          TRIM( NC_Filename ), &
                          Error_Status, &
                          Message_Log = Message_Log )
    NF90_Status = NF90_CLOSE( NC_FileID )

  END FUNCTION Write_RadDiag_Stats_netCDF



  ! Function to read RadDiag_Stats data from a
  ! netCDF format RadDiag_Stats file.
  !
  FUNCTION Read_RadDiag_Stats_netCDF( NC_Filename,   &  ! Input
                                      RadDiag_Stats, &  ! Output
                                      Title,         &  ! Optional output
                                      History,       &  ! Optional output
                                      Sensor_Name,   &  ! Optional output
                                      Platform_Name, &  ! Optional output
                                      Comment,       &  ! Optional output
                                      RCS_Id,        &  ! Revision control
                                      Message_Log )  &  ! Error messaging
                                    RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),               INTENT(IN)     :: NC_Filename
    TYPE( RadDiag_Stats_type ), INTENT(IN OUT) :: RadDiag_Stats
    CHARACTER(*),     OPTIONAL, INTENT(OUT)    :: Title
    CHARACTER(*),     OPTIONAL, INTENT(OUT)    :: History
    CHARACTER(*),     OPTIONAL, INTENT(OUT)    :: Sensor_Name
    CHARACTER(*),     OPTIONAL, INTENT(OUT)    :: Platform_Name
    CHARACTER(*),     OPTIONAL, INTENT(OUT)    :: Comment
    CHARACTER(*),     OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),     OPTIONAL, INTENT(IN)     :: Message_Log

    ! Function result
    INTEGER :: Error_Status

    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_RadDiag_Stats_netCDF'

    ! Function variables
    CHARACTER( 256 ) :: Message
    CHARACTER( 256 ) :: VarName
    INTEGER :: NF90_Status
    INTEGER :: Close_Status
    INTEGER :: NC_FileID
    INTEGER :: nPredictors   
    INTEGER :: nChannels   
    INTEGER :: nFOVs       
    INTEGER :: nTimes      
    INTEGER :: nVariables


    ! ------
    ! Set up
    ! ------

    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Inquire the file
    Error_Status = Inquire_RadDiag_Stats_netCDF( TRIM( NC_Filename ),           &
                                                 nPredictors   = nPredictors,   &
                                                 nChannels     = nChannels,     &
                                                 nFOVs         = nFOVs,         &
                                                 nTimes        = nTimes,        &
                                                 nVariables    = nVariables,    &
                                                 Title         = Title,         &
                                                 History       = History,       &
                                                 Sensor_Name   = Sensor_Name,   &
                                                 Platform_Name = Platform_Name, &
                                                 Comment       = Comment,       &
                                                 Message_Log   = Message_Log    )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining RadDiag_Stats dimension/attributes from '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Allocate the output structure
    Error_Status = Allocate_RadDiag_Stats( nPredictors,   &
                                           nChannels,     &
                                           nFOVs,         &
                                           nTimes,        &
                                           nVariables,    &
                                           RadDiag_Stats, &
                                           Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error occurred allocating RadDiag_Stats structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! Open the file for reading
    Error_Status = Open_RadDiag_Stats_netCDF( TRIM( NC_Filename ), &
                                              NC_FileID, &
                                              Mode = 'READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening netCDF RadDiag_Stats data file '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    ! ---------------------------
    ! Read the RadDiag_Stats data
    ! ---------------------------
    ! The Predictor array
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        AIRMASSCOEFF_VARNAME, &
                                        RadDiag_Stats%AirMassCoefficients )
    IF ( Error_Status /= SUCCESS ) THEN
      VarName = AIRMASSCOEFF_VARNAME
      GOTO 1000
    END IF

    ! The Channel array
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        CHANNEL_VARNAME, &
                                        RadDiag_Stats%Channel )
    IF ( Error_Status /= SUCCESS ) THEN
      VarName = CHANNEL_VARNAME
      GOTO 1000
    END IF

    ! The FOV array
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        FOV_VARNAME, &
                                        RadDiag_Stats%FOV )
    IF ( Error_Status /= SUCCESS ) THEN
      VarName = FOV_VARNAME
      GOTO 1000
    END IF

    ! The scan_Data array
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        SCAN_DATA_VARNAME, &
                                        RadDiag_Stats%scan_Data )
    IF ( Error_Status /= SUCCESS ) THEN
      VarName = SCAN_DATA_VARNAME
      GOTO 1000
    END IF

    ! The scan_nSamples array
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        SCAN_NSAMPLES_VARNAME, &
                                        RadDiag_Stats%scan_nSamples )
    IF ( Error_Status /= SUCCESS ) THEN
      VarName = SCAN_NSAMPLES_VARNAME
      GOTO 1000
    END IF

    ! The DateTime array
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        DATETIME_VARNAME, &
                                        RadDiag_Stats%DateTime )
    IF ( Error_Status /= SUCCESS ) THEN
      VarName = DATETIME_VARNAME
      GOTO 1000
    END IF

    ! The time_Data array
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        TIME_DATA_VARNAME, &
                                        RadDiag_Stats%time_Data )
    IF ( Error_Status /= SUCCESS ) THEN
      VarName = TIME_DATA_VARNAME
      GOTO 1000
    END IF

    ! The time_nSamples array
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        TIME_NSAMPLES_VARNAME, &
                                        RadDiag_Stats%time_nSamples )
    IF ( Error_Status /= SUCCESS ) THEN
      VarName = TIME_NSAMPLES_VARNAME
      GOTO 1000
    END IF

    ! The VariableNames array
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        VARIABLENAME_VARNAME, &
                                        RadDiag_Stats%VariableNames )
    IF ( Error_Status /= SUCCESS ) THEN
      VarName = VARIABLENAME_VARNAME
      GOTO 1000
    END IF


    ! --------------
    ! Close the file
    ! --------------
    Close_Status = Close_RadDiag_Stats_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF RadDiag_Stats data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! --------------
    ! Process errors
    ! --------------
    1000 CONTINUE
    CALL Display_Message( ROUTINE_NAME, &
                          'Error reading '//TRIM(VarName)//' from '// &
                          TRIM( NC_Filename ), &
                          Error_Status, &
                          Message_Log = Message_Log )
    NF90_Status = NF90_CLOSE( NC_FileID )

  END FUNCTION Read_RadDiag_Stats_netCDF

END MODULE RadDiag_Stats_netCDF_IO
