!
! EmisCoeff_netCDF_IO
!
! Module containing routines to create, inquire, read and write netCDF
! format EmisCoeff files.
!       
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 14-Jun-2005
!                       paul.vandelst@ssec.wisc.edu
!

MODULE EmisCoeff_netCDF_IO

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds,          ONLY: fp=>fp_kind
  USE Message_Handler,     ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, Display_Message
  USE EmisCoeff_Define,    ONLY: EmisCoeff_Type, &
                                 SPECTRAL_EMISCOEFF_TYPE, &
                                 SENSOR_EMISCOEFF_TYPE, &
                                 Associated_EmisCoeff, &
                                 Allocate_EmisCoeff, &
                                 Destroy_EmisCoeff, &
                                 Check_EmisCoeff_Release, &
                                 Info_EmisCoeff 
  USE netcdf
  USE netCDF_Utility,  Open_EmisCoeff_netCDF =>  Open_netCDF, &
                      Close_EmisCoeff_netCDF => Close_netCDF
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Inquire_EmisCoeff_netCDF
  PUBLIC :: Write_EmisCoeff_netCDF
  PUBLIC :: Read_EmisCoeff_netCDF


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Module RCS Id string
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &

  ! Global attribute names. Case sensitive
  CHARACTER(*), PARAMETER :: TITLE_GATTNAME   = 'title' 
  CHARACTER(*), PARAMETER :: HISTORY_GATTNAME = 'history' 
  CHARACTER(*), PARAMETER :: COMMENT_GATTNAME = 'comment' 

  ! Dimension names
  CHARACTER(*), PARAMETER :: ANGLE_DIMNAME      = 'n_Angles'
  CHARACTER(*), PARAMETER :: FREQUENCY_DIMNAME  = 'n_Frequencies'
  CHARACTER(*), PARAMETER :: WIND_SPEED_DIMNAME = 'n_Wind_Speeds'

  ! Variable names. Case sensitive.
  CHARACTER(*), PARAMETER :: RELEASE_VARNAME    = 'Release'
  CHARACTER(*), PARAMETER :: VERSION_VARNAME    = 'Version'
  CHARACTER(*), PARAMETER :: DATA_TYPE_VARNAME  = 'Data_Type'
  CHARACTER(*), PARAMETER :: ANGLE_VARNAME      = 'Angle'
  CHARACTER(*), PARAMETER :: FREQUENCY_VARNAME  = 'Frequency'
  CHARACTER(*), PARAMETER :: WIND_SPEED_VARNAME = 'Wind_Speed'
  CHARACTER(*), PARAMETER :: EMISSIVITY_VARNAME = 'Emissivity'

  ! Variable description attribute.
  CHARACTER(*), PARAMETER :: DESCRIPTION_ATTNAME = 'description'

  CHARACTER(*), PARAMETER :: RELEASE_DESCRIPTION    = &
    'Release number of Spectral EmisCoeff data file'
  CHARACTER(*), PARAMETER :: VERSION_DESCRIPTION    = &
    'Version number of Spectral EmisCoeff data file'
  CHARACTER(*), PARAMETER :: DATA_TYPE_DESCRIPTION  = &
    'Flag to indicate if this EmisCoeff data is for the spectral or sensor emissivity model'
  CHARACTER(*), PARAMETER :: ANGLE_DESCRIPTION      = &
    'Angle (Z) dimension values for emissivity data.'
  CHARACTER(*), PARAMETER :: FREQUENCY_DESCRIPTION  = &
    'Frequency (F) dimension values for emissivity data.'
  CHARACTER(*), PARAMETER :: WIND_SPEED_DESCRIPTION = &
    'Wind speed (V) dimension values for emissivity data.'
  CHARACTER(*), PARAMETER :: EMISSIVITY_DESCRIPTION = &
    'Spectral sea surface emissivity data.'

  ! Variable long name attribute.
  CHARACTER(*), PARAMETER :: LONGNAME_ATTNAME = 'long_name'

  CHARACTER(*), PARAMETER :: ANGLE_LONGNAME      = 'Angle' 
  CHARACTER(*), PARAMETER :: FREQUENCY_LONGNAME  = 'Frequency'
  CHARACTER(*), PARAMETER :: WIND_SPEED_LONGNAME = 'Wind Speed' 
  CHARACTER(*), PARAMETER :: EMISSIVITY_LONGNAME = 'Emissivity' 

  ! Variable units attribute.
  CHARACTER(*), PARAMETER :: UNITS_ATTNAME = 'units'

  CHARACTER(*), PARAMETER :: ANGLE_UNITS      = 'degrees from vertical'
  CHARACTER(*), PARAMETER :: FREQUENCY_UNITS  = 'Inverse centimetres (cm^-1)'
  CHARACTER(*), PARAMETER :: WIND_SPEED_UNITS = 'metres per second (m.s^-1)'
  CHARACTER(*), PARAMETER :: EMISSIVITY_UNITS = 'None.'

  ! Variable _FillValue attribute.
  CHARACTER(*), PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'

  INTEGER,      PARAMETER :: IP_FILLVALUE = -1
  REAL(fp),     PARAMETER :: FP_FILLVALUE = -999.0_fp

  INTEGER,      PARAMETER :: RELEASE_FILLVALUE    = IP_FILLVALUE
  INTEGER,      PARAMETER :: VERSION_FILLVALUE    = IP_FILLVALUE
  INTEGER,      PARAMETER :: DATA_TYPE_FILLVALUE  = IP_FILLVALUE
  REAL(fp),     PARAMETER :: ANGLE_FILLVALUE      = FP_FILLVALUE
  REAL(fp),     PARAMETER :: FREQUENCY_FILLVALUE  = FP_FILLVALUE
  REAL(fp),     PARAMETER :: WIND_SPEED_FILLVALUE = FP_FILLVALUE
  REAL(fp),     PARAMETER :: EMISSIVITY_FILLVALUE = FP_FILLVALUE
  REAL(fp),     PARAMETER :: DERIVATIVE_FILLVALUE = FP_FILLVALUE

  ! Variable netCDF datatypes
  INTEGER, PARAMETER :: RELEASE_TYPE    = NF90_INT
  INTEGER, PARAMETER :: VERSION_TYPE    = NF90_INT
  INTEGER, PARAMETER :: DATA_TYPE_TYPE  = NF90_INT
  INTEGER, PARAMETER :: ANGLE_TYPE      = NF90_DOUBLE
  INTEGER, PARAMETER :: FREQUENCY_TYPE  = NF90_DOUBLE
  INTEGER, PARAMETER :: WIND_SPEED_TYPE = NF90_DOUBLE
  INTEGER, PARAMETER :: EMISSIVITY_TYPE = NF90_DOUBLE
  INTEGER, PARAMETER :: DERIVATIVE_TYPE = NF90_DOUBLE


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
!       Write_EmisCoeff_GAtts
!
! PURPOSE:
!       Function to write the global attributes to a netCDF Spectral EmisCoeff
!       data file.
!
! CALLING SEQUENCE:
!       Error_Status = Write_EmisCoeff_GAtts( NC_Filename,              &  ! Input
!                                             NC_FileID,                &  ! Input
!                                             Title       = Title,      &  ! Optional input
!                                             History     = History,    &  ! Optional input
!                                             Comment     = Comment,    &  ! Optional input
!                                             Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF EmisCoeff format data file to create.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:        NetCDF file ID number returned from the
!                         Open_ or Create_EmisCoeff_netCDF() function.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF EmisCoeff file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF EmisCoeff file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF EmisCoeff file.
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

  FUNCTION Write_EmisCoeff_GAtts( NC_Filename,  &  ! Input
                                  NC_FileID,    &  ! Input
                                  Title,        &  ! Optional input
                                  History,      &  ! Optional input
                                  Comment,      &  ! Optional input
                                  Message_Log ) &  ! Error messaging
                                RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: NC_Filename
    INTEGER,                INTENT(IN) :: NC_FileID
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_EmisCoeff_GAtts'
    CHARACTER(*), PARAMETER :: WRITE_MODULE_HISTORY_GATTNAME   = 'write_module_history' 
    CHARACTER(*), PARAMETER :: CREATION_DATE_AND_TIME_GATTNAME = 'creation_date_and_time' 
    INTEGER, PARAMETER :: nPutGAtts = 5
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

  END FUNCTION Write_EmisCoeff_GAtts


!--------------------------------------------------------------------------------
!
! NAME:
!       Read_EmisCoeff_GAtts
!
! PURPOSE:
!       Function to read the global attributes from a netCDF Spectral EmisCoeff
!       data file.
!
! CALLING SEQUENCE:
!       Error_Status = Read_EmisCoeff_GAtts( NC_Filename,                   &  ! Input
!                                            NC_FileID,                     &  ! Input
!                                            Title         = Title,         &  ! Optional output
!                                            History       = History,       &  ! Optional output
!                                            Comment       = Comment,       &  ! Optional output
!                                            Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF EmisCoeff format data file to create.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:        NetCDF file ID number returned from the
!                         Open_ or Create_EmisCoeff_netCDF() function.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
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
!                         attribute field of the netCDF EmisCoeff file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF EmisCoeff file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF EmisCoeff file.
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

  FUNCTION Read_EmisCoeff_GAtts( NC_Filename,   &  ! Input
                                 NC_FileID,     &  ! Input
                                 Title,         &  ! Optional output
                                 History,       &  ! Optional output
                                 Comment,       &  ! Optional output
                                 Message_Log )  &  ! Error messaging
                              RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER,                INTENT(IN)  :: NC_FileID
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_EmisCoeff_GAtts'
    INTEGER, PARAMETER :: nGetGAtts = 3
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
      CHARACTER(*),           INTENT(IN)  :: GAttName
      CHARACTER(*),           INTENT(OUT) :: GAttString
      CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
      INTEGER :: Error_Status
      CHARACTER(10000) :: LongString
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

  END FUNCTION Read_EmisCoeff_GAtts




!--------------------------------------------------------------------------------
!
! NAME:
!       Create_EmisCoeff_netCDF
!
! PURPOSE:
!       Function to create a netCDF Spectral EmisCoeff data file for writing.
!
! CALLING SEQUENCE:
!       Error_Status = Create_EmisCoeff_netCDF( NC_Filename,              &  ! Input
!                                               n_Angles,                 &  ! Input
!                                               n_Frequencies,            &  ! Input
!                                               n_Wind_Speeds,            &  ! Input
!                                               NC_FileID,                &  ! Output
!                                               Title       = Title,      &  ! Optional input
!                                               History     = History,    &  ! Optional input
!                                               Comment     = Comment,    &  ! Optional input
!                                               Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:        Character string specifying the name of the
!                           netCDF EmisCoeff format data file to create.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Angles:           The angle dimension of the emissivity data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Frequencies:      The frequency dimension of the emissivity data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Wind_Speeds:      The wind speed dimension of the emissivity data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Title:              Character string written into the TITLE global
!                           attribute field of the netCDF EmisCoeff file.
!                           Should contain a succinct description of what
!                           is in the netCDF datafile.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the netCDF EmisCoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF EmisCoeff file.
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
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the Message_Handler module.
!                           If == SUCCESS the netCDF file creation was successful
!                              == FAILURE an unrecoverable error occurred
!                              == WARNING an error occurred writing any of the
!                                         supplied global file attributes
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

  FUNCTION Create_EmisCoeff_netCDF( NC_Filename,   &  ! Input
                                    n_Angles,      &  ! Input
                                    n_Frequencies, &  ! Input
                                    n_Wind_Speeds, &  ! Input
                                    NC_FileID,     &  ! Output
                                    Title,         &  ! Optional input
                                    History,       &  ! Optional input
                                    Comment,       &  ! Optional input
                                    Message_Log )  &  ! Error messaging
                                  RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER,                INTENT(IN)  :: n_Angles
    INTEGER,                INTENT(IN)  :: n_Frequencies
    INTEGER,                INTENT(IN)  :: n_Wind_Speeds
    INTEGER,                INTENT(OUT) :: NC_FileID
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Create_EmisCoeff_netCDF'
    ! Local variables
    INTEGER :: NF90_Status
    INTEGER :: Description_Status
    INTEGER :: Longname_Status
    INTEGER :: Units_Status
    INTEGER :: Fillvalue_Status
    INTEGER :: Angle_dimID
    INTEGER :: Frequency_dimID
    INTEGER :: Wind_Speed_dimID
    INTEGER :: varID

    ! Set up
    Error_Status = SUCCESS

    ! Check the dimensions
    IF ( n_Angles      < 1 .OR. &
         n_Frequencies < 1 .OR. &
         n_Wind_Speeds < 1      ) THEN
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
    ! The number of angles
    Error_Status = Def_Dim(ANGLE_DIMNAME, n_Angles, Angle_DimID )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! The number of frequencies
    Error_Status = Def_Dim(FREQUENCY_DIMNAME, n_Frequencies, Frequency_DimID )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! The number of wind speeds
    Error_Status = Def_Dim(WIND_SPEED_DIMNAME, n_WIND_SPEEDS, Wind_Speed_DimID )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Write the global attributes
    Error_Status = Write_EmisCoeff_GAtts( TRIM( NC_Filename ), &
                                          NC_FileID, &
                                          Title       = Title, &
                                          History     = History, &
                                          Comment     = Comment, &
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
    ! Write some attributes
    Description_Status = Put_netCDF_Attribute( NC_FileID, &
                                               DESCRIPTION_ATTNAME, &
                                               RELEASE_DESCRIPTION, &
                                               Variable_Name = RELEASE_VARNAME )
    Fillvalue_Status   = Put_netCDF_Attribute( NC_FileID, &
                                               FILLVALUE_ATTNAME, &
                                               RELEASE_FILLVALUE, &
                                               Variable_Name = RELEASE_VARNAME )
    IF ( Description_Status /= SUCCESS .OR. &
         Fillvalue_Status   /= SUCCESS      ) THEN
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
    ! Write some attributes
    Description_Status = Put_netCDF_Attribute( NC_FileID, &
                                               DESCRIPTION_ATTNAME, &
                                               VERSION_DESCRIPTION, &
                                               Variable_Name = VERSION_VARNAME )
    Fillvalue_Status   = Put_netCDF_Attribute( NC_FileID, &
                                               FILLVALUE_ATTNAME, &
                                               VERSION_FILLVALUE, &
                                               Variable_Name = VERSION_VARNAME )
    IF ( Description_Status /= SUCCESS .OR. &
         Fillvalue_Status   /= SUCCESS      ) THEN
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


    ! The structure type
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                DATA_TYPE_VARNAME, &
                                DATA_TYPE_TYPE, &
                                varid = varID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//DATA_TYPE_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF
    ! Write some attributes
    Description_Status = Put_netCDF_Attribute( NC_FileID, &
                                               DESCRIPTION_ATTNAME, &
                                               DATA_TYPE_DESCRIPTION, &
                                               Variable_Name = DATA_TYPE_VARNAME )
    Fillvalue_Status   = Put_netCDF_Attribute( NC_FileID, &
                                               FILLVALUE_ATTNAME, &
                                               DATA_TYPE_FILLVALUE, &
                                               Variable_Name = DATA_TYPE_VARNAME )
    IF ( Description_Status /= SUCCESS .OR. &
         Fillvalue_Status   /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//DATA_TYPE_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! Angle
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                ANGLE_VARNAME, &
                                ANGLE_TYPE, &
                                varid  = varID, &
                                dimids = Angle_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//ANGLE_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF
    ! Write some attributes
    Description_Status = Put_netCDF_Attribute( NC_FileID, &
                                               DESCRIPTION_ATTNAME, &
                                               ANGLE_DESCRIPTION, &
                                               Variable_Name = ANGLE_VARNAME )
    Longname_Status    = Put_netCDF_Attribute( NC_FileID, &
                                               LONGNAME_ATTNAME, &
                                               ANGLE_LONGNAME, &
                                               Variable_Name = ANGLE_VARNAME )
    Units_Status       = Put_netCDF_Attribute( NC_FileID, &
                                               UNITS_ATTNAME, &
                                               ANGLE_UNITS, &
                                               Variable_Name = ANGLE_VARNAME )
    Fillvalue_Status   = Put_netCDF_Attribute( NC_FileID, &
                                               FILLVALUE_ATTNAME, &
                                               ANGLE_FILLVALUE, &
                                               Variable_Name = ANGLE_VARNAME )
    IF ( Description_Status /= SUCCESS .OR. &
         Longname_Status    /= SUCCESS .OR. &
         Units_Status       /= SUCCESS .OR. &
         Fillvalue_Status   /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//ANGLE_VARNAME//' attributes to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! Frequency
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                FREQUENCY_VARNAME, &
                                FREQUENCY_TYPE, &
                                varid  = varID, &
                                dimids = Frequency_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//FREQUENCY_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF
    ! Write some attributes
    Description_Status = Put_netCDF_Attribute( NC_FileID, &
                                               DESCRIPTION_ATTNAME, &
                                               FREQUENCY_DESCRIPTION, &
                                               Variable_Name = FREQUENCY_VARNAME )
    Longname_Status    = Put_netCDF_Attribute( NC_FileID, &
                                               LONGNAME_ATTNAME, &
                                               FREQUENCY_LONGNAME, &
                                               Variable_Name = FREQUENCY_VARNAME )
    Units_Status       = Put_netCDF_Attribute( NC_FileID, &
                                               UNITS_ATTNAME, &
                                               FREQUENCY_UNITS, &
                                               Variable_Name = FREQUENCY_VARNAME )
    Fillvalue_Status   = Put_netCDF_Attribute( NC_FileID, &
                                               FILLVALUE_ATTNAME, &
                                               FREQUENCY_FILLVALUE, &
                                               Variable_Name = FREQUENCY_VARNAME )
    IF ( Description_Status /= SUCCESS .OR. &
         Longname_Status    /= SUCCESS .OR. &
         Units_Status       /= SUCCESS .OR. &
         Fillvalue_Status   /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//FREQUENCY_VARNAME//' attributes to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! Wind_Speed
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                WIND_SPEED_VARNAME, &
                                WIND_SPEED_TYPE, &
                                varid  = varID, &
                                dimids = Wind_Speed_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//WIND_SPEED_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF
    ! Write some attributes
    Description_Status = Put_netCDF_Attribute( NC_FileID, &
                                               DESCRIPTION_ATTNAME, &
                                               WIND_SPEED_DESCRIPTION, &
                                               Variable_Name = WIND_SPEED_VARNAME )
    Longname_Status    = Put_netCDF_Attribute( NC_FileID, &
                                               LONGNAME_ATTNAME, &
                                               WIND_SPEED_LONGNAME, &
                                               Variable_Name = WIND_SPEED_VARNAME )
    Units_Status       = Put_netCDF_Attribute( NC_FileID, &
                                               UNITS_ATTNAME, &
                                               WIND_SPEED_UNITS, &
                                               Variable_Name = WIND_SPEED_VARNAME )
    Fillvalue_Status   = Put_netCDF_Attribute( NC_FileID, &
                                               FILLVALUE_ATTNAME, &
                                               WIND_SPEED_FILLVALUE, &
                                               Variable_Name = WIND_SPEED_VARNAME )
    IF ( Description_Status /= SUCCESS .OR. &
         Longname_Status    /= SUCCESS .OR. &
         Units_Status       /= SUCCESS .OR. &
         Fillvalue_Status   /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//WIND_SPEED_VARNAME//' attributes to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! Emissivity data
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                EMISSIVITY_VARNAME, &
                                EMISSIVITY_TYPE, &
                                varid = varID, &
                                dimids = (/ Angle_DimID, &
                                            Frequency_DimID, &
                                            Wind_Speed_DimID /) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//EMISSIVITY_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF
    ! Write some attributes
    Description_Status = Put_netCDF_Attribute( NC_FileID, &
                                               DESCRIPTION_ATTNAME, &
                                               EMISSIVITY_DESCRIPTION, &
                                               Variable_Name = EMISSIVITY_VARNAME )
    Longname_Status    = Put_netCDF_Attribute( NC_FileID, &
                                               LONGNAME_ATTNAME, &
                                               EMISSIVITY_LONGNAME, &
                                               Variable_Name = EMISSIVITY_VARNAME )
    Units_Status       = Put_netCDF_Attribute( NC_FileID, &
                                               UNITS_ATTNAME, &
                                               EMISSIVITY_UNITS, &
                                               Variable_Name = EMISSIVITY_VARNAME )
    Fillvalue_Status   = Put_netCDF_Attribute( NC_FileID, &
                                               FILLVALUE_ATTNAME, &
                                               EMISSIVITY_FILLVALUE, &
                                               Variable_Name = EMISSIVITY_VARNAME )
    IF ( Description_Status /= SUCCESS .OR. &
         Longname_Status    /= SUCCESS .OR. &
         Units_Status       /= SUCCESS .OR. &
         Fillvalue_Status   /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//EMISSIVITY_VARNAME//' attributes to '// &
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

  END FUNCTION Create_EmisCoeff_netCDF


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
!       Inquire_EmisCoeff_netCDF
!
! PURPOSE:
!       Function to inquire a netCDF Spectral EmisCoeff format file to obtain
!       the dimension values and global attributes.
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_EmisCoeff_netCDF( NC_Filename,                   &  ! Input
!                                                n_Angles      = n_Angles,      &  ! Optional output
!                                                n_Frequencies = n_Frequencies, &  ! Optional output
!                                                n_Wind_Speeds = n_Wind_Speeds, &  ! Optional output
!                                                Angle         = Angle,         &  ! Optional output
!                                                Frequency     = Frequency,     &  ! Optional output
!                                                Wind_Speed    = Wind_Speed,    &  ! Optional output
!                                                Release       = Release,       &  ! Optional Output
!                                                Version       = Version,       &  ! Optional Output
!                                                Title         = Title,         &  ! Optional output
!                                                History       = History,       &  ! Optional output
!                                                Comment       = Comment,       &  ! Optional output
!                                                RCS_Id        = RCS_Id,        &  ! Revision control
!                                                Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the netCDF EmisCoeff
!                         format data file. Used only for Message output.
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
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Angles:         The angle dimension of the emissivity data.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       n_Frequencies:    The frequency dimension of the emissivity data.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       n_Wind_Speeds:    The wind speed dimension of the emissivity data.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Angle:            Angle dimension values for the emissivity and
!                         derivative data. Size of argument must match the
!                         data file dimensions.
!                         UNITS:      Degrees (from vertical)
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Rank-1 (n_Angles)
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Frequency:        Frequency dimension values for the emissivity and
!                         derivative data. Size of argument must match the
!                         data file dimensions.
!                         UNITS:      Inverse centimetres (cm^-1)
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Rank-1 (n_Frequencies)
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Wind_Speed:       Surface wind speed dimension values for the
!                         emissivity and derivative data. Size of argument
!                         must match the data file dimensions.
!                         UNITS:      metres/second (m.s^-1)
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Rank-1 (n_Wind_Speeds)
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:          The EmisCoeff data/file release number. Used to check
!                         for data/software mismatch.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Version:          The EmisCoeff data/file version number. Used for
!                         purposes only in identifying the dataset for
!                         a particular release.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF EmisCoeff file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF EmisCoeff file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF EmisCoeff file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       RCS_Id:           Character string containing the Revision Control
!                         System Id field for the module.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error status.
!                         The error codes are defined in the Message_Handler module.
!                           If == SUCCESS the netCDF file inquiry was successful
!                              == FAILURE an error occurred reading any of the
!                                         requested data.
!                              == WARNING an error occurred closing the netCDF
!                                         file after a successful read.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION Inquire_EmisCoeff_netCDF( NC_Filename,    &  ! Input
                                     n_Angles,       &  ! Optional output
                                     n_Frequencies,  &  ! Optional output
                                     n_Wind_Speeds,  &  ! Optional output
                                     Angle,          &  ! Optional output
                                     Frequency,      &  ! Optional output
                                     Wind_Speed,     &  ! Optional output
                                     Release,        &  ! Optional output
                                     Version,        &  ! Optional output
                                     Title,          &  ! Optional output
                                     History,        &  ! Optional output
                                     Comment,        &  ! Optional output
                                     RCS_Id,         &  ! Revision control
                                     Message_Log )   &  ! Error messaging
                                   RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),                         INTENT(IN)  :: NC_Filename
    INTEGER,      OPTIONAL,               INTENT(OUT) :: n_Angles
    INTEGER,      OPTIONAL,               INTENT(OUT) :: n_Frequencies
    INTEGER,      OPTIONAL,               INTENT(OUT) :: n_Wind_Speeds
    REAL(fp),     OPTIONAL, DIMENSION(:), INTENT(OUT) :: Angle
    REAL(fp),     OPTIONAL, DIMENSION(:), INTENT(OUT) :: Frequency
    REAL(fp),     OPTIONAL, DIMENSION(:), INTENT(OUT) :: Wind_Speed
    INTEGER,      OPTIONAL,               INTENT(OUT) :: Release
    INTEGER,      OPTIONAL,               INTENT(OUT) :: Version
    CHARACTER(*), OPTIONAL,               INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL,               INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL,               INTENT(OUT) :: Comment
    CHARACTER(*), OPTIONAL,               INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL,               INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Inquire_EmisCoeff_netCDF'
    ! Function variables
    CHARACTER(256) :: Message
    INTEGER :: NF90_Status
    INTEGER :: Close_Status
    INTEGER :: NC_FileID
    INTEGER :: Data_Type
    INTEGER :: l, n, i

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Open the file
    Error_Status = Open_EmisCoeff_netCDF( TRIM( NC_FileNAME ), &
                                         NC_FileID, &
                                         Mode = 'READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF EmisCoeff data file '//TRIM( NC_Filename )
      GOTO 2000
    END IF

    ! Check the structure data type
    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        DATA_TYPE_VARNAME, &
                                        Data_type)
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//DATA_TYPE_VARNAME//&
                ' data from '//TRIM( NC_fileNAME )
      GOTO 1000
    END IF

    ! Check it
    IF ( Data_Type /= SPECTRAL_EMISCOEFF_type) THEN
      Message = 'Invalid EmisCoeff file data type in '//TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! Get the dimensions
    !
    ! The number of angles
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         ANGLE_DIMNAME, &
                                         i, &
                                         Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//ANGLE_DIMNAME//' dimension from '//TRIM(NC_Filename)
      GOTO 1000
    END IF
    IF ( PRESENT( n_Angles ) ) n_Angles = i

    ! The number of frequencies
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         FREQUENCY_DIMNAME, &
                                         l, &
                                         Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//FREQUENCY_DIMNAME//' dimension from '//&
                TRIM( NC_Filename )
      GOTO 1000
    END IF
    IF ( PRESENT( n_Frequencies ) ) n_Frequencies = l

    ! The number of wind speeds
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         WIND_SPEED_DIMNAME, &
                                         n, &
                                         Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//WIND_SPEED_DIMNAME//' dimension from '//&
                TRIM( NC_Filename )
      GOTO 1000
    END IF
    IF ( PRESENT( n_Wind_Speeds ) ) n_Wind_Speeds = n

    ! Get the dimension vector data
    !
    ! The angle
    IF ( PRESENT( Angle ) ) THEN
      ! Check the argument size
      IF ( SIZE( Angle ) /= i ) THEN
        Error_Status = FAILURE
        Message = 'ANGLE size different from netCDF file dimension.'
        GOTO 1000
      END IF
      ! Get the data
      Error_Status = Get_netCDF_Variable( NC_fileID, &
                                          ANGLE_VARNAME, &
                                          Angle )

      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading '//ANGLE_VARNAME//' data from '//&
                  TRIM( NC_fileNAME )
        GOTO 1000
      END IF
    END IF

    ! The frequency grid
    IF ( PRESENT( Frequency ) ) THEN
      ! Check the argument size
      IF ( SIZE( Frequency ) /= l ) THEN
        Error_Status = FAILURE
        Message = 'FREQUENCY size different from netCDF file dimension.'
        GOTO 1000
      END IF
      ! Get the data
      Error_Status = Get_netCDF_Variable( NC_fileID, &
                                          FREQUENCY_VARNAME, &
                                          Frequency )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading '//FREQUENCY_VARNAME//' data from '//&
                  TRIM( NC_fileNAME )
        GOTO 1000
      END IF
    END IF

    ! The wind speed
    IF ( PRESENT( Wind_Speed ) ) THEN
      ! Check the argument size
      IF ( SIZE( Wind_Speed ) /= n ) THEN
        Message = 'WIND_SPEED size different from netCDF file dimension.'
        GOTO 1000
      END IF
      ! Get the data
      Error_Status = Get_netCDF_Variable( NC_fileID, &
                                          WIND_SPEED_VARNAME, &
                                          Wind_Speed )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading '//WIND_SPEED_VARNAME//' data from '//&
                  TRIM( NC_fileNAME )
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
    Error_Status = Read_EmisCoeff_GAtts( NC_Filename, &
                                         NC_FileID, &
                                         Title       = Title, &
                                         History     = History, &
                                         Comment     = Comment, &
                                         Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading global attribute from '// &
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! Close the file
    Close_Status = Close_EmisCoeff_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF EmisCoeff data file '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

    RETURN


    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

    1000 CONTINUE
    Close_Status = Close_EmisCoeff_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing input file during error cleanup.'

    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Inquire_EmisCoeff_netCDF


!--------------------------------------------------------------------------------
!S+
! NAME:
!       Write_EmisCoeff_netCDF
!
! PURPOSE:
!       Function to write Spectral EmisCoeff data to a netCDF file.
!
! CALLING SEQUENCE:
!       Error_Status = Write_EmisCoeff_netCDF( NC_Filename,                   &  ! Input
!                                              EmisCoeff,                     &  ! Input
!                                              Title         = Title,         &  ! Optional input
!                                              History       = History,       &  ! Optional input
!                                              Comment       = Comment,       &  ! Optional input
!                                              Quiet         = Quiet,         &  ! Optional input
!                                              RCS_Id        = RCS_Id,        &  ! Revision control
!                                              Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:     Character string specifying the name of the output
!                        netCDF EmisCoeff format data file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       EmisCoeff:       Structure to write to file.
!                        UNITS:      N/A
!                        TYPE:       EmisCoeff_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Title:           Character string written into the TITLE global
!                        attribute field of the netCDF EmisCoeff file.
!                        Should contain a succinct description of what
!                        is in the netCDF datafile.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       History:         Character string written into the HISTORY global
!                        attribute field of the netCDF EmisCoeff file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Comment:         Character string written into the COMMENT global
!                        attribute field of the netCDF EmisCoeff file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Quiet:           Set this keyword to suppress information messages being
!                        printed to standard output (or the Message log file if
!                        the MESSAGE_LOG optional argument is used.) By default,
!                        information messages are printed.
!                        If QUIET = 0, information messages are OUTPUT.
!                           QUIET = 1, information messages are SUPPRESSED.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
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
!                           == FAILURE - the input EmisCoeff structure contains
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

  FUNCTION Write_EmisCoeff_netCDF( NC_Filename,   &  ! Input
                                   EmisCoeff,     &  ! Input
                                   Title,         &  ! Optional input
                                   History,       &  ! Optional input
                                   Comment,       &  ! Optional input
                                   Quiet,         &  ! Optional input
                                   RCS_Id,        &  ! Revision control
                                   Message_Log )  &  ! Error messaging
                                 RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    TYPE(EmisCoeff_type),   INTENT(IN)  :: EmisCoeff
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment
    INTEGER,      OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_EmisCoeff_netCDF'
    ! Local variables
    CHARACTER(256) :: Message
    LOGICAL :: Noisy
    INTEGER :: Close_Status
    INTEGER :: NC_FileID

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
    IF ( .NOT. Associated_EmisCoeff( EmisCoeff ) ) THEN
      Message = 'Some or all INPUT EmisCoeff pointer members are NOT associated.'
      GOTO 2000
    END IF

    ! Check structure data type
    IF ( EmisCoeff%Data_Type /= SPECTRAL_EMISCOEFF_TYPE) THEN
      Message = 'Invalid EmisCoeff structure data type'
      GOTO 2000
    END IF

    ! Check structure release
    Error_Status = Check_EmisCoeff_Release( EmisCoeff, &
                                            Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'EmisCoeff Release check failed.'
      GOTO 2000
    END IF

    ! Create the output file
    Error_Status = Create_EmisCoeff_netCDF( NC_Filename, &
                                            EmisCoeff%n_Angles, &
                                            EmisCoeff%n_Frequencies, &
                                            EmisCoeff%n_Wind_Speeds, &
                                            NC_FileID, &
                                            Title         = Title, &
                                            History       = History, &
                                            Comment       = Comment, &
                                            Message_Log   = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating output netCDF EmisCoeff file '//&
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! Write the EmisCoeff data
    !
    ! The Release number
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        RELEASE_VARNAME, &
                                        EmisCoeff%Release )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//RELEASE_VARNAME//' number to '// &
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The Version number
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        VERSION_VARNAME, &
                                        EmisCoeff%Version )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//VERSION_VARNAME//' number to '// &
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The structure data type
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        DATA_TYPE_VARNAME, &
                                        EmisCoeff%Data_type)

    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//DATA_TYPE_VARNAME//' number to '// &
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The angles
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        ANGLE_VARNAME, &
                                        EmisCoeff%Angle )
    IF ( Error_Status /= SUCCESS ) THEN
      Message =  'Error writing '//ANGLE_VARNAME//' to '//&
                 TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The frequencies
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        FREQUENCY_VARNAME, &
                                        EmisCoeff%Frequency )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//FREQUENCY_VARNAME//' to '//&
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The wind speeds
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        WIND_SPEED_VARNAME, &
                                        EmisCoeff%Wind_Speed )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//WIND_SPEED_VARNAME//' to '//&
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The emissivity data
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        EMISSIVITY_VARNAME, &
                                        EmisCoeff%Emissivity )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//EMISSIVITY_VARNAME//' to '//&
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! Close the file
    Close_Status = Close_EmisCoeff_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF EmisCoeff data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    ! Outut an info message
    IF ( Noisy ) THEN
      CALL Info_EmisCoeff( EmisCoeff, message )
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
    Close_Status = Close_EmisCoeff_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing output file during error cleanup.'

    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )

  END FUNCTION Write_EmisCoeff_netCDF


!--------------------------------------------------------------------------------
!
! NAME:
!       Read_EmisCoeff_netCDF
!
! PURPOSE:
!       Function to read data from a netCDF format Spectral EmisCoeff file.
!
! CALLING SEQUENCE:
!       Error_Status = Read_EmisCoeff_netCDF( NC_Filename,                   &  ! Input
!                                             EmisCoeff,                     &  ! Output
!                                             Quiet         = Quiet,         &  ! Optional input
!                                             Title         = Title,         &  ! Optional output
!                                             History       = History,       &  ! Optional output
!                                             Comment       = Comment,       &  ! Optional output
!                                             RCS_Id        = RCS_Id,        &  ! Revision control
!                                             Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:     Character string specifying the name of the netCDF EmisCoeff
!                        format data file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:           Set this keyword to suppress information messages being
!                        printed to standard output (or the Message log file if
!                        the MESSAGE_LOG optional argument is used.) By default,
!                        information messages are printed.
!                        If QUIET = 0, information messages are OUTPUT.
!                           QUIET = 1, information messages are SUPPRESSED.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
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
!       EmisCoeff:        Structure to contain the transmittance coefficient
!                        data read from the file.
!                        UNITS:      N/A
!                        TYPE:       EmisCoeff_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT)
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Title:           Character string written into the TITLE global
!                        attribute field of the netCDF EmisCoeff file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       History:         Character string written into the HISTORY global
!                        attribute field of the netCDF EmisCoeff file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Comment:         Character string written into the COMMENT global
!                        attribute field of the netCDF EmisCoeff file.
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
!       If the EmisCoeff argument is defined upon input, it is redefined (or
!       reinitialised) at output.
!
! COMMENTS:
!       Note the INTENT on the output EmisCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION Read_EmisCoeff_netCDF( NC_Filename,   &  ! Input
                                  EmisCoeff,     &  ! Output
                                  Quiet,         &  ! Optional input
                                  Title,         &  ! Optional output
                                  History,       &  ! Optional output
                                  Comment,       &  ! Optional output
                                  RCS_Id,        &  ! Revision control
                                  Message_Log )  &  ! Error messaging
                                RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)     :: NC_Filename
    TYPE(EmisCoeff_type),   INTENT(IN OUT) :: EmisCoeff
    INTEGER,      OPTIONAL, INTENT(IN)     :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_EmisCoeff_netCDF'
    ! Function variables
    CHARACTER(256) :: Message
    LOGICAL :: Noisy
    INTEGER :: Destroy_Status
    INTEGER :: Close_Status
    INTEGER :: NC_FileID
    INTEGER :: n_Frequencies
    INTEGER :: n_Wind_Speeds
    INTEGER :: n_Angles

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF

    ! Check the structure data type
    IF ( EmisCoeff%Data_Type /= SPECTRAL_EMISCOEFF_TYPE) THEN
      Message = 'EmisCoeff structure data type invalid'
      GOTO 2000
    END IF

    ! Inquire the file
    Error_Status = Inquire_EmisCoeff_netCDF( TRIM( NC_Filename ), &
                                             n_Angles      = n_Angles, &
                                             n_Frequencies = n_Frequencies, &
                                             n_Wind_Speeds = n_Wind_Speeds, &
                                             Release       = EmisCoeff%Release, &
                                             Version       = EmisCoeff%Version, &
                                             Title         = Title, &
                                             History       = History, &
                                             Comment       = Comment, &
                                             Message_Log   = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining EmisCoeff dimensions/attributes from '//&
                TRIM( NC_Filename )
      GOTO 2000
    END IF

    ! Check the release
    Error_Status = Check_EmisCoeff_Release( EmisCoeff, &
                                            Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'EmisCoeff Release check failed for '//TRIM( NC_Filename )
      GOTO 2000
    END IF

    ! Allocate the output structure
    Error_Status = Allocate_EmisCoeff( n_Angles, &
                                       n_Frequencies, &
                                       n_Wind_Speeds, &
                                       EmisCoeff, &
                                       Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error occurred allocating EmisCoeff structure.'
      GOTO 2000
    END IF

    ! Open the file for reading
    Error_Status = Open_EmisCoeff_netCDF( TRIM( NC_Filename ), &
                                          NC_FileID, &
                                          Mode = 'READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF EmisCoeff data file '//&
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! Read the EmisCoeff data
    !
    ! The angle
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        ANGLE_VARNAME, &
                                        EmisCoeff%Angle )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//ANGLE_VARNAME//' from '// &
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The frequency
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        FREQUENCY_VARNAME, &
                                        EmisCoeff%Frequency )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//FREQUENCY_VARNAME//' from '// &
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The wind speed
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        WIND_SPEED_VARNAME, &
                                        EmisCoeff%Wind_Speed )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//WIND_SPEED_VARNAME//' from '// &
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! The emissivity data
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        EMISSIVITY_VARNAME, &
                                        EmisCoeff%Emissivity )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//EMISSIVITY_VARNAME//' from '// &
                TRIM( NC_Filename )
      GOTO 1000
    END IF

    ! Close the file
    Close_Status = Close_EmisCoeff_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF EmisCoeff data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    ! Output an info message
    IF ( Noisy ) THEN
      CALL Info_EmisCoeff( EmisCoeff, Message )
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
    Destroy_Status = Destroy_EmisCoeff(EmisCoeff, Message_Log=Message_Log)
    IF ( Destroy_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error destroying EmisCoeff during error cleanup.'
    Close_Status = Close_EmisCoeff_netCDF(NC_FileID)
    IF ( Close_Status /= SUCCESS ) &
      Message = TRIM(Message)//'; Error closing input file during error cleanup.'

    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Read_EmisCoeff_netCDF

END MODULE EmisCoeff_netCDF_IO
