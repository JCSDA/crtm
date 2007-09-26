!
! LBLRTM_netCDF_IO
!
! Module containing routine to read and write netCDF format files of
! LBLRTM output data.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Apr-2002
!                       paul.vandelst@ssec.wisc.edu
!

MODULE LBLRTM_netCDF_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds     , ONLY: fp
  USE Message_Handler, ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                             Display_Message
  USE netcdf
  USE netCDF_Utility,  Open_LBLRTM_netCDF =>  Open_netCDF, &
                      Close_LBLRTM_netCDF => Close_netCDF
  ! Disable all implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Create_LBLRTM_netCDF
  PUBLIC :: Inquire_LBLRTM_netCDF
  PUBLIC :: Write_LBLRTM_netCDF
  PUBLIC :: Read_LBLRTM_netCDF


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Module RCS Id string
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
    '$Id$'

   ! Global attribute names. Case sensitive
  CHARACTER(*), PARAMETER :: TITLE_GATTNAME   = 'title'
  CHARACTER(*), PARAMETER :: HISTORY_GATTNAME = 'history'
  CHARACTER(*), PARAMETER :: COMMENT_GATTNAME = 'comment'
  CHARACTER(*), PARAMETER :: ID_TAG_GATTNAME  = 'id_tag' 

  ! Dimension names
  CHARACTER(*), PARAMETER :: FREQUENCY_DIMNAME = 'n_frequencies'
  CHARACTER(*), PARAMETER :: LAYER_DIMNAME     = 'n_layers'

  ! Variable names
  CHARACTER(*), PARAMETER :: DIRECTION_VARNAME          = 'direction'
  CHARACTER(*), PARAMETER :: BEGIN_FREQUENCY_VARNAME    = 'begin_frequency'
  CHARACTER(*), PARAMETER :: END_FREQUENCY_VARNAME      = 'end_frequency'
  CHARACTER(*), PARAMETER :: FREQUENCY_INTERVAL_VARNAME = 'frequency_interval'
  CHARACTER(*), PARAMETER :: TRANSMITTANCE_VARNAME      = 'transmittance'

  ! Variable long name attribute.
  CHARACTER(*), PARAMETER :: LONGNAME_ATTNAME = 'long_name'
  CHARACTER(*), PARAMETER :: DIRECTION_LONGNAME          = &
    'Direction flag for transmittance calculation. Upwelling=1, Downwelling=2'
  CHARACTER(*), PARAMETER :: BEGIN_FREQUENCY_LONGNAME    = &
    'Begin frequency of the transmittance data'
  CHARACTER(*), PARAMETER :: END_FREQUENCY_LONGNAME      = &
    'End frequency of the transmittance data'
  CHARACTER(*), PARAMETER :: FREQUENCY_INTERVAL_LONGNAME = &
    'Frequency interval of the transmittance data'
  CHARACTER(*), PARAMETER :: TRANSMITTANCE_LONGNAME      = &
    'Layer -> boundary (TOA or SFC) spectral transmittance'

  ! Variable units attribute.
  CHARACTER(*), PARAMETER :: UNITS_ATTNAME = 'units'
  CHARACTER(*), PARAMETER :: BEGIN_FREQUENCY_UNITS    = 'Inverse centimetres (cm^-1)'
  CHARACTER(*), PARAMETER :: END_FREQUENCY_UNITS      = 'Inverse centimetres (cm^-1)'
  CHARACTER(*), PARAMETER :: FREQUENCY_INTERVAL_UNITS = 'Inverse centimetres (cm^-1)'
  CHARACTER(*), PARAMETER :: TRANSMITTANCE_UNITS      = 'None'

  ! Variable _FillValue attribute.
  CHARACTER(*), PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'
  INTEGER     , PARAMETER :: DIRECTION_FILLVALUE          = -1
  REAL(fp)    , PARAMETER :: BEGIN_FREQUENCY_FILLVALUE    = -1.0_fp
  REAL(fp)    , PARAMETER :: END_FREQUENCY_FILLVALUE      = -1.0_fp
  REAL(fp)    , PARAMETER :: FREQUENCY_INTERVAL_FILLVALUE = -1.0_fp
  REAL(fp)    , PARAMETER :: TRANSMITTANCE_FILLVALUE      = -1.0_fp

  ! Variable types
  INTEGER, PARAMETER :: DIRECTION_TYPE          = NF90_INT
  INTEGER, PARAMETER :: BEGIN_FREQUENCY_TYPE    = NF90_DOUBLE
  INTEGER, PARAMETER :: END_FREQUENCY_TYPE      = NF90_DOUBLE
  INTEGER, PARAMETER :: FREQUENCY_INTERVAL_TYPE = NF90_DOUBLE
  INTEGER, PARAMETER :: TRANSMITTANCE_TYPE      = NF90_DOUBLE

  ! Direction flags for transmittance calculation
  INTEGER, PARAMETER :: DOWNWELLING = 0
  INTEGER, PARAMETER :: UPWELLING   = 1


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                          ## PRIVATE MODULE ROUTINES ##                     ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!
! NAME:
!       Write_LBLRTM_GAtts
!
! PURPOSE:
!       Function to write the global attributes to a netCDF format LBLRTM
!       data file.
!
! CALLING SEQUENCE:
!       Error_Status = Write_LBLRTM_GAtts( NC_Filename,              &  ! Input
!                                          NC_FileID,                &  ! Input
!                                          ID_Tag  = ID_Tag,         &  ! Optional input
!                                          Title   = Title,          &  ! Optional input
!                                          History = History,        &  ! Optional input
!                                          Comment = Comment,        &  ! Optional input
!                                          Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:  Character string specifying the name of the
!                     netCDF LBLRTM format data file to write to.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:    NetCDF file ID number.
!                     function.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!
! OPTIONAL INPUT ARGUMENTS:
!       ID_Tag:       Character string written into the ID_TAG global
!                     attribute field of the netCDF LBLRTM file.
!                     Should contain a short tag used to identify the
!                     profile set.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:        Character string written into the TITLE global
!                     attribute field of the netCDF LBLRTM file.
!                     Should contain a succinct description of what
!                     is in the netCDF datafile.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:      Character string written into the HISTORY global
!                     attribute field of the netCDF LBLRTM file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:      Character string written into the COMMENT global
!                     attribute field of the netCDF LBLRTM file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:  Character string specifying a filename in which
!                     any messages will be logged. If not specified,
!                     or if an error occurs opening the log file, the
!                     default action is to output messages to standard
!                     output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the global attribute write was successful
!                        == WARNING an error occurred writing the global attributes
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 11-Jul-2003
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Write_LBLRTM_GAtts( NC_Filename,   &  ! Input
                               NC_FileID,     &  ! Input
                               ID_Tag,        &  ! Optional input
                               Title,         &  ! Optional input
                               History,       &  ! Optional input
                               Comment,       &  ! Optional input
                               Message_Log )  &  ! Error messaging
                             RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: NC_Filename
    INTEGER     ,           INTENT(IN) :: NC_FileID
    CHARACTER(*), OPTIONAL, INTENT(IN) :: ID_Tag
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_LBLRTM_GAtts'
    CHARACTER(*), PARAMETER :: WRITE_MODULE_HISTORY_GATTNAME   = 'write_module_history' 
    CHARACTER(*), PARAMETER :: CREATION_DATE_AND_TIME_GATTNAME = 'creation_date_and_time' 
    INTEGER, PARAMETER :: nPutGAtts = 6
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

  END FUNCTION Write_LBLRTM_GAtts


!------------------------------------------------------------------------------
!
! NAME:
!       Read_LBLRTM_GAtts
!
! PURPOSE:
!       Function to read the global attributes from a netCDF format LBLRTM
!       data file.
!
! CALLING SEQUENCE:
!       Error_Status = Read_LBLRTM_GAtts( NC_Filename,              &  ! Input
!                                         NC_FileID,                &  ! Input
!                                         ID_Tag  = ID_Tag,         &  ! Optional output
!                                         Title   = Title,          &  ! Optional output
!                                         History = History,        &  ! Optional output
!                                         Comment = Comment,        &  ! Optional output
!                                         Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:  Character string specifying the name of the
!                     netCDF LBLRTM format data file to read from.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:    NetCDF file ID number.
!                     function.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which
!                     any messages will be logged. If not specified,
!                     or if an error occurs opening the log file, the
!                     default action is to output messages to standard
!                     output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       ID_Tag:       Character string written into the ID_TAG global
!                     attribute field of the netCDF LBLRTM file.
!                     Should contain a short tag used to identify the
!                     Profile set.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Title:        Character string written into the TITLE global
!                     attribute field of the netCDF LBLRTM file.
!                     Should contain a succinct description of what
!                     is in the netCDF datafile.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       History:      Character string written into the HISTORY global
!                     attribute field of the netCDF LBLRTM file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Comment:      Character string written into the COMMENT global
!                     attribute field of the netCDF LBLRTM file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the global attribute read was successful
!                        == WARNING an error occurred reading the global attributes
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 11-Jul-2003
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Read_LBLRTM_GAtts( NC_Filename,   &  ! Input
                              NC_FileID,     &  ! Input
                              ID_Tag,        &  ! Optional output
                              Title,         &  ! Optional output
                              History,       &  ! Optional output
                              Comment,       &  ! Optional output
                              Message_Log )  &  ! Error messaging
                            RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER     ,           INTENT(IN)  :: NC_FileID
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: ID_Tag
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_LBLRTM_GAtts'
    INTEGER, PARAMETER :: nGetGAtts = 4
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

  END FUNCTION Read_LBLRTM_GAtts


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
!       Create_LBLRTM_netCDF
!
! PURPOSE:
!       Function to create a netCDF LBLRTM data file for writing.
!
! CALLING SEQUENCE:
!       Error_Status = Create_LBLRTM_netCDF( NC_Filename,              &  ! Input
!                                            n_Frequencies,            &  ! Input
!                                            Direction,                &  ! Input
!                                            Begin_Frequency,          &  ! Input
!                                            End_Frequency,            &  ! Input
!                                            Frequency_Interval,       &  ! Input
!                                            ID_Tag      = ID_Tag,     &  ! Optional input
!                                            Title       = Title,      &  ! Optional input
!                                            History     = History,    &  ! Optional input
!                                            Comment     = Comment,    &  ! Optional input
!                                            RCS_Id      = RCS_Id,     &  ! Revision control
!                                            Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:        Character string specifying the name of the
!                           netCDF format LBLRTM data file to create.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Frequencies:      The number of spectral points in each layer of data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Direction:          The direction flag for the spectral data calculation.
!                           If = 0 Downwelling
!                              = 1 Upwelling
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Begin_Frequency:    The begin frequency of the spectral data.
!                           UNITS:      inverse centimetres (cm^-1)
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       End_Frequency:      The end frequency of the spectral data.
!                           UNITS:      inverse centimetres (cm^-1)
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Frequency_Interval: The frequency spacing of the spectral data.
!                           UNITS:      inverse centimetres (cm^-1)
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!
! OPTIONAL INPUT ARGUMENTS:
!       ID_Tag:             Character string written into the ID_TAG global
!                           attribute field of the netCDF LBLRTM file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:              Character string written into the TITLE global attribute
!                           field of the netCDF LBLRTM file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:            Character string written into the HISTORY global attribute
!                           field of the netCDF LBLRTM file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF LBLRTM file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
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
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           Message_Handler module.
!                           If == SUCCESS the LBLRTM netCDF file creation was successful
!                              == FAILURE an unrecoverable error occured
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
!
! SIDE EFFECTS:
!       If the output file already exists, it is overwritten.
!
! RESTRICTIONS:
!       - It is assumed the number of frequencies is the same for every layer
!       - Only transmittance data variables are created in the output netCDF file
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Apr-2002
!                       paul.vandelst@ssec.wisc.edu
!
!--------------------------------------------------------------------------------

  FUNCTION Create_LBLRTM_netCDF( NC_Filename       , &  ! Input
                                 n_Frequencies     , &  ! Input
                                 Direction         , &  ! Input
                                 Begin_Frequency   , &  ! Input
                                 End_Frequency     , &  ! Input
                                 Frequency_Interval, &  ! Input
                                 ID_Tag            , &  ! Optional input
                                 Title             , &  ! Optional input
                                 History           , &  ! Optional input
                                 Comment           , &  ! Optional input
                                 RCS_Id            , &  ! Revision control
                                 Message_Log       ) &  ! Error messaging
                               RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER     ,           INTENT(IN)  :: n_Frequencies
    INTEGER     ,           INTENT(IN)  :: Direction
    REAL(fp)    ,           INTENT(IN)  :: Begin_Frequency
    REAL(fp)    ,           INTENT(IN)  :: End_Frequency
    REAL(fp)    ,           INTENT(IN)  :: Frequency_Interval
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: ID_Tag
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Create_LBLRTM_netCDF'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: NC_FileID
    INTEGER :: NF90_Status
    INTEGER :: WriteGatts_Status
    INTEGER :: Longname_Status
    INTEGER :: Units_Status
    INTEGER :: FillValue_Status
    INTEGER :: Close_Status
    INTEGER :: Frequency_DimID
    INTEGER :: Layer_DimID
    INTEGER :: VarID


    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF

    ! Check input
    IF ( n_Frequencies < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Number of frequencies (spectral points) must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
    IF ( Direction /= DOWNWELLING .AND. Direction /= UPWELLING ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid direction flag', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------
    ! Create the file
    ! ---------------
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


    ! ---------------------
    ! Define the dimensions
    ! ---------------------
    ! The number of spectral points
    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                FREQUENCY_DIMNAME, &
                                n_Frequencies, &
                                Frequency_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining the '//FREQUENCY_DIMNAME//' dimension in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! The number of layers
    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                LAYER_DIMNAME, &
                                NF90_UNLIMITED, &
                                Layer_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining the '//LAYER_DIMNAME//' dimension in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------------------------
    ! Write the global attributes
    ! ---------------------------
    WriteGAtts_Status = Write_LBLRTM_GAtts( NC_Filename            , &
                                            NC_FileID              , &
                                            ID_Tag     =ID_Tag     , &
                                            Title      =Title      , &
                                            History    =History    , &
                                            Comment    =Comment    , & 
                                            Message_Log=Message_Log  )
    IF ( WriteGAtts_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing global attributes to '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! --------------------
    ! Define the variables
    ! --------------------
    ! Direction flag
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                DIRECTION_VARNAME, &
                                DIRECTION_TYPE, &
                                varid = VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//DIRECTION_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF
    Longname_Status  = Put_netCDF_Attribute( NC_FileID, &
                                             LONGNAME_ATTNAME, &
                                             DIRECTION_LONGNAME, &
                                             Variable_Name = DIRECTION_VARNAME )
    FillValue_Status = Put_netCDF_Attribute( NC_FileID, &
                                             FILLVALUE_ATTNAME, &
                                             DIRECTION_FILLVALUE, &
                                             Variable_Name = DIRECTION_VARNAME )
    IF ( Longname_Status  /= SUCCESS .OR. &
         FillValue_Status /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//DIRECTION_VARNAME//' attributes to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! Begin frequency
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                BEGIN_FREQUENCY_VARNAME, &
                                BEGIN_FREQUENCY_TYPE, &
                                varid = VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//BEGIN_FREQUENCY_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF
    Longname_Status  = Put_netCDF_Attribute( NC_FileID, &
                                             LONGNAME_ATTNAME, &
                                             BEGIN_FREQUENCY_LONGNAME, &
                                             Variable_Name = BEGIN_FREQUENCY_VARNAME )
    Units_Status     = Put_netCDF_Attribute( NC_FileID, &
                                             UNITS_ATTNAME, &
                                             BEGIN_FREQUENCY_UNITS, &
                                             Variable_Name = BEGIN_FREQUENCY_VARNAME )
    FillValue_Status = Put_netCDF_Attribute( NC_FileID, &
                                             FILLVALUE_ATTNAME, &
                                             BEGIN_FREQUENCY_FILLVALUE, &
                                             Variable_Name = BEGIN_FREQUENCY_VARNAME )
    IF ( Longname_Status  /= SUCCESS .OR. &
         Units_Status     /= SUCCESS .OR. &
         FillValue_Status /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//BEGIN_FREQUENCY_VARNAME//' attributes to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! End frequency
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                END_FREQUENCY_VARNAME, &
                                END_FREQUENCY_TYPE, &
                                varid = VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//END_FREQUENCY_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF
    Longname_Status  = Put_netCDF_Attribute( NC_FileID, &
                                             LONGNAME_ATTNAME, &
                                             END_FREQUENCY_LONGNAME, &
                                             Variable_Name = END_FREQUENCY_VARNAME )
    Units_Status     = Put_netCDF_Attribute( NC_FileID, &
                                             UNITS_ATTNAME, &
                                             END_FREQUENCY_UNITS, &
                                             Variable_Name = END_FREQUENCY_VARNAME )
    FillValue_Status = Put_netCDF_Attribute( NC_FileID, &
                                             FILLVALUE_ATTNAME, &
                                             END_FREQUENCY_FILLVALUE, &
                                             Variable_Name = END_FREQUENCY_VARNAME )
    IF ( Longname_Status  /= SUCCESS .OR. &
         Units_Status     /= SUCCESS .OR. &
         FillValue_Status /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//END_FREQUENCY_VARNAME//' attributes to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! Frequency interval
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                FREQUENCY_INTERVAL_VARNAME, &
                                FREQUENCY_INTERVAL_TYPE, &
                                varid = VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//FREQUENCY_INTERVAL_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF
    Longname_Status  = Put_netCDF_Attribute( NC_FileID, &
                                             LONGNAME_ATTNAME, &
                                             FREQUENCY_INTERVAL_LONGNAME, &
                                             Variable_Name = FREQUENCY_INTERVAL_VARNAME )
    Units_Status     = Put_netCDF_Attribute( NC_FileID, &
                                             UNITS_ATTNAME, &
                                             FREQUENCY_INTERVAL_UNITS, &
                                             Variable_Name = FREQUENCY_INTERVAL_VARNAME )
    FillValue_Status = Put_netCDF_Attribute( NC_FileID, &
                                             FILLVALUE_ATTNAME, &
                                             FREQUENCY_INTERVAL_FILLVALUE, &
                                             Variable_Name = FREQUENCY_INTERVAL_VARNAME )
    IF ( Longname_Status  /= SUCCESS .OR. &
         Units_Status     /= SUCCESS .OR. &
         FillValue_Status /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//FREQUENCY_INTERVAL_VARNAME//' attributes to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! Transmittance
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                TRANSMITTANCE_VARNAME, &
                                TRANSMITTANCE_TYPE, &
                                dimids = (/ Frequency_DimID, &
                                            Layer_DimID     /), &
                                varid = VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//TRANSMITTANCE_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF
    Longname_Status  = Put_netCDF_Attribute( NC_FileID, &
                                             LONGNAME_ATTNAME, &
                                             TRANSMITTANCE_LONGNAME, &
                                             Variable_Name = TRANSMITTANCE_VARNAME )
    Units_Status     = Put_netCDF_Attribute( NC_FileID, &
                                             UNITS_ATTNAME, &
                                             TRANSMITTANCE_UNITS, &
                                             Variable_Name = TRANSMITTANCE_VARNAME )
    FillValue_Status = Put_netCDF_Attribute( NC_FileID, &
                                             FILLVALUE_ATTNAME, &
                                             TRANSMITTANCE_FILLVALUE, &
                                             Variable_Name = TRANSMITTANCE_VARNAME )
    IF ( Longname_Status  /= SUCCESS .OR. &
         Units_Status     /= SUCCESS .OR. &
         FillValue_Status /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRANSMITTANCE_VARNAME//' attributes to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    ! --------------------------------
    ! Take the file out of define mode
    ! --------------------------------
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


    ! ------------------------
    ! Write the frequency data
    ! ------------------------
    ! Direction flag
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        DIRECTION_VARNAME, &
                                        Direction )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//DIRECTION_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! Begin frequency
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        BEGIN_FREQUENCY_VARNAME, &
                                        Begin_Frequency )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//BEGIN_FREQUENCY_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! End frequency
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        END_FREQUENCY_VARNAME, &
                                        End_Frequency )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//END_FREQUENCY_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! Frequency interval
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        FREQUENCY_INTERVAL_VARNAME, &
                                        Frequency_Interval )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//FREQUENCY_INTERVAL_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------------------
    ! Close the netCDF file
    ! ---------------------
    Close_Status = Close_LBLRTM_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF LBLRTM data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Create_LBLRTM_netCDF


!------------------------------------------------------------------------------
!
! NAME:
!       Inquire_LBLRTM_netCDF
!
! PURPOSE:
!       Function to inquire a netCDF format LBLRTM file to obtain the 
!       dimensions and frequency information.
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_LBLRTM_netCDF( NC_Filename,                             &  ! Input
!                                             n_Frequencies      = n_Frequencies,      &  ! Optional output
!                                             n_Layers           = n_Layers,           &  ! Optional output
!                                             Direction          = Direction,          &  ! Optional output
!                                             Begin_Frequency    = Begin_Frequency,    &  ! Optional output
!                                             End_Frequency      = End_Frequency,      &  ! Optional output
!                                             Frequency_Interval = Frequency_Interval, &  ! Optional output
!                                             ID_Tag             = ID_Tag,             &  ! Optional output
!                                             Title              = Title,              &  ! Optional output
!                                             History            = History,            &  ! Optional output
!                                             Comment            = Comment,            &  ! Optional output
!                                             RCS_Id             = RCS_Id,             &  ! Optional output
!                                             Message_Log        = Message_Log         ) ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:        Character string specifying the name of
!                           the netCDF format LBLRTM data file.
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
!       n_Frequencies:      The number of spectral points in each layer of data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Layers:           The number of atmospheric layers in the netCDF file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Direction:          The direction flag for the spectral data calculation.
!                           If = 0 Downwelling
!                              = 1 Upwelling
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Begin_Frequency:    The begin frequency of the spectral data.
!                           UNITS:      inverse centimetres (cm^-1)
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       End_Frequency:      The end frequency of the spectral data.
!                           UNITS:      inverse centimetres (cm^-1)
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Frequency_Interval: The frequency spacing of the spectral data.
!                           UNITS:      inverse centimetres (cm^-1)
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       ID_Tag:             Character string written into the ID_TAG global
!                           attribute field of the netCDF LBLRTM file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:              Character string written into the TITLE global attribute
!                           field of the netCDF LBLRTM file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:            Character string written into the HISTORY global attribute
!                           field of the netCDF LBLRTM file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF LBLRTM file.
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
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           Message_Handler module.
!                           If == SUCCESS the LBLRTM netCDF file inquiry was
!                                         successful
!                              == FAILURE - an error occurred opening the
!                                           netCDF file, or
!                                         - an error occurred reading any of
!                                           the requested dimension or variable
!                                           data.
!                                         - an error occurred reading any of the
!                                           requested global file attributes
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Apr-2002
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Inquire_LBLRTM_netCDF( NC_Filename       , &  ! Input
                                  n_Frequencies     , &  ! Optional output
                                  n_Layers          , &  ! Optional output
                                  Direction         , &  ! Optional output
                                  Begin_Frequency   , &  ! Optional output
                                  End_Frequency     , &  ! Optional output
                                  Frequency_Interval, &  ! Optional output
                                  ID_Tag            , &  ! Optional output
                                  Title             , &  ! Optional output
                                  History           , &  ! Optional output
                                  Comment           , &  ! Optional output
                                  RCS_Id            , &  ! Revision control
                                  Message_Log       ) &  ! Error messaging
                                RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Frequencies
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Layers
    INTEGER     , OPTIONAL, INTENT(OUT) :: Direction
    REAL(fp)    , OPTIONAL, INTENT(OUT) :: Begin_Frequency
    REAL(fp)    , OPTIONAL, INTENT(OUT) :: End_Frequency
    REAL(fp)    , OPTIONAL, INTENT(OUT) :: Frequency_Interval
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: ID_Tag
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Inquire_LBLRTM_netCDF'
    ! Function variables
    CHARACTER(256) :: Message
    INTEGER :: NF90_Status
    INTEGER :: ReadGAtts_Status
    INTEGER :: Close_Status
    INTEGER :: NC_FileID


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF


    ! -------------
    ! Open the file
    ! -------------
    Error_Status = Open_LBLRTM_netCDF( TRIM( NC_Filename ), &
                                       NC_FileID, &
                                       Mode = 'READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening netCDF LBLRTM data file '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ------------------
    ! Get the dimensions
    ! ------------------
    ! The number of spectral points
    IF ( PRESENT( n_Frequencies ) ) THEN
      Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                           FREQUENCY_DIMNAME, &
                                           n_Frequencies, &
                                           Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error obtaining '//FREQUENCY_DIMNAME//&
                              ' dimension from '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF
    END IF

    ! The number of atmospheric layers
    IF ( PRESENT( n_Layers ) ) THEN
      Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                           LAYER_DIMNAME, &
                                           n_Layers, &
                                           Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error obtaining '//LAYER_DIMNAME//&
                              ' dimension from '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF
    END IF


    ! ----------------------
    ! Get the direction flag
    ! ----------------------
    IF ( PRESENT( Direction ) ) THEN
      Error_Status = Get_netCDF_Variable( NC_FileID, &
                                          DIRECTION_VARNAME, &
                                          Direction )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//DIRECTION_VARNAME//&
                              ' data from '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF
    END IF


    ! -------------------
    ! Get the frequencies
    ! -------------------
    ! Begin frequency
    IF ( PRESENT( Begin_Frequency ) ) THEN
      Error_Status = Get_netCDF_Variable( NC_FileID, &
                                          BEGIN_FREQUENCY_VARNAME, &
                                          Begin_Frequency )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//BEGIN_FREQUENCY_VARNAME//&
                              ' data from '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF
    END IF

    ! End frequency
    IF ( PRESENT( End_Frequency ) ) THEN
      Error_Status = Get_netCDF_Variable( NC_FileID, &
                                          END_FREQUENCY_VARNAME, &
                                          End_Frequency )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//END_FREQUENCY_VARNAME//&
                              ' data from '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF
    END IF

    ! Frequency interval
    IF ( PRESENT( Frequency_Interval ) ) THEN
      Error_Status = Get_netCDF_Variable( NC_FileID, &
                                          FREQUENCY_INTERVAL_VARNAME, &
                                          Frequency_Interval )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//FREQUENCY_INTERVAL_VARNAME//&
                              ' data from '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF
    END IF


    ! -------------------------
    ! Get the global attributes
    ! -------------------------
    Error_Status = Read_LBLRTM_GAtts( TRIM( NC_Filename ), &
                                      NC_FileID, &
                                      ID_Tag  = ID_Tag, &
                                      Title   = Title, &
                                      History = History, &
                                      Comment = Comment, &
                                      Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading global attribute from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! --------------
    ! Close the file
    ! --------------
    Close_Status = Close_LBLRTM_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF LBLRTM data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Inquire_LBLRTM_netCDF


!------------------------------------------------------------------------------
!
! NAME:
!       Write_LBLRTM_netCDF
!
! PURPOSE:
!       Function to write LBLRTM spectral data to a netCDF format LBLRTM file.
!
! CALLING SEQUENCE:
!       Error_Status = Write_LBLRTM_netCDF( NC_Filename,                   &  ! Input
!                                           Transmittance = Transmittance, &  ! Optional input
!                                           RCS_Id        = RCS_Id,        &  ! Revision control
!                                           Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:    Character string specifying the name of the netCDF
!                       format LBLRTM data file to write to.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:      NetCDF file ID number returned from the
!                       Create_LBLRTM_netCDF() or Open_LBLRTM_netCDF()
!                       functions.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Transmittance:  Transmittance data to write to the netCDF LBLRTM file.
!                       UNITS:      N/A
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Rank-1, n_Frequencies
!                       ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT(IN)
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
!                       If == SUCCESS the LBLRTM netCDF file write was successful
!                          == FAILURE an unrecoverable error occurred
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Apr-2002
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Write_LBLRTM_netCDF( NC_Filename  , &  ! Input
                                Transmittance, &  ! Optional input
                                RCS_Id       , &  ! Revision control
                                Message_Log  ) &  ! Error messaging
                               RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    REAL(fp)    , OPTIONAL, INTENT(IN)  :: Transmittance(:)
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_LBLRTM_netCDF'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: NC_FileID
    INTEGER :: NF90_Status
    INTEGER :: Close_Status
    INTEGER :: n_Frequencies
    INTEGER :: n_Layers


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF
    ! Return if no data
    IF ( .NOT. PRESENT(Transmittance) ) RETURN


    ! -----------------------------
    ! Check the frequency dimension
    ! -----------------------------
    ! Read the dimension value
    Error_Status = Inquire_LBLRTM_netCDF( NC_Filename,                   &
                                          n_Frequencies=n_Frequencies, &
                                          n_Layers     =n_Layers, &
                                          Message_Log  =Message_Log    )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining LBLRTM dimensions from '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
    
    ! Check the number of frequencies
    IF ( SIZE(Transmittance) /= n_Frequencies ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Input TRANSMITTANCE array size (",i0,&
                      &") different from netCDF definition (",i0,")")' ) &
                     SIZE(Transmittance), n_Frequencies
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------
    ! Open the file
    ! -------------
    Error_Status = Open_LBLRTM_netCDF( TRIM( NC_Filename ), &
                                       NC_FileID, &
                                       Mode = 'READWRITE' )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening netCDF LBLRTM data file '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------
    ! Write the data
    ! --------------
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        TRANSMITTANCE_VARNAME, &
                                        Transmittance, &
                                        START=(/1,n_Layers+1/), &
                                        COUNT=(/n_Frequencies,1/) )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing TRANSMITTANCE variable to '//TRIM(NC_Filename), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! --------------
    ! Close the file
    ! --------------
    Close_Status = Close_LBLRTM_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF LBLRTM data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Write_LBLRTM_netCDF


!------------------------------------------------------------------------------
!
! NAME:
!       Read_LBLRTM_netCDF
!
! PURPOSE:
!       Function to read LBLRTM spectral data from a netCDF format LBLRTM file.
!
! CALLING SEQUENCE:
!       Error_Status = Read_LBLRTM_netCDF( NC_Filename,                   &  ! Input
!                                          Layer,                         &  ! Input
!                                          Transmittance = Transmittance, &  ! Optional output
!                                          RCS_Id        = RCS_Id,        &  ! Revision control
!                                          Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:    Character string specifying the name of the
!                       netCDF format LBLRTM data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Layer:          Layer index value of the netCDF data to read.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Transmittance:  Transmittance data read from the netCDF LBLRTM file.
!                       UNITS:      N/A
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Rank-1, n_Frequencies
!                       ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
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
!                       If == SUCCESS the LBLRTM netCDF file read was successful
!                          == FAILURE an unrecoverable error occurred
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Apr-2002
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Read_LBLRTM_netCDF( NC_Filename  , &  ! Input
                               Layer        , &  ! Input
                               Transmittance, &  ! Optional output
                               RCS_Id       , &  ! Revision control
                               Message_Log  ) &  ! Error messaging
                               RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER     ,           INTENT(IN)  :: Layer
    REAL(fp)    , OPTIONAL, INTENT(OUT) :: Transmittance(:)
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_LBLRTM_netCDF'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: NC_FileID
    INTEGER :: NF90_Status
    INTEGER :: Close_Status
    INTEGER :: n_Frequencies
    INTEGER :: n_Layers


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF
    ! Return if no data
    IF ( .NOT. PRESENT(Transmittance) ) RETURN


    ! --------------------
    ! Check the dimensions
    ! --------------------
    ! Read the dimension value
    Error_Status = Inquire_LBLRTM_netCDF( NC_Filename, &
                                          n_Frequencies =n_Frequencies, &
                                          n_Layers      =n_Layers, &
                                          Message_Log   =Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining LBLRTM dimensions from '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Check the number of frequencies
    IF ( SIZE(Transmittance) /= n_Frequencies ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Input TRANSMITTANCE array size different from netCDF definition.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! Check the layer index
    IF ( Layer < 1 .OR. Layer > n_Layers ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Input LAYER value invalid for specified netCDF dataset.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -------------
    ! Open the file
    ! -------------
    Error_Status = Open_LBLRTM_netCDF( TRIM( NC_Filename ), &
                                       NC_FileID, &
                                       Mode = 'READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening netCDF LBLRTM data file '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -------------
    ! Read the data
    ! -------------
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        TRANSMITTANCE_VARNAME, &
                                        Transmittance, &
                                        START=(/1, Layer/), &
                                        COUNT=(/n_Frequencies, 1/) )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error writing TRANSMITTANCE variable for layer #", i3, &
                        &" in ", a )' ) Layer, TRIM( NC_Filename )
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! --------------
    ! Close the file
    ! --------------
    Close_Status = Close_LBLRTM_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF LBLRTM data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Read_LBLRTM_netCDF

END MODULE LBLRTM_netCDF_IO
