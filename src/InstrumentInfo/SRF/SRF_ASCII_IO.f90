!
! SRF_ASCII_IO
!
! Module containing routines to read and write ASCII "HMW" format
! SRF data files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Aug-2003
!                       paul.vandelst@ssec.wisc.edu
!
!       Modified by:    Yong Chen, CIRA/CSU/JCSDA 22-Aug-2008
!                       Yong.Chen@noaa.gov
 
MODULE SRF_ASCII_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,        ONLY: fp
  USE File_Utility,      ONLY: Get_Lun, File_Open
  USE Message_Handler,   ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                               Display_Message
  USE String_Utility,    ONLY: StrUpCase
  USE SRF_Define,        ONLY: N_SENSOR_TYPES    , &
                               INVALID_SENSOR    , &    
                               MICROWAVE_SENSOR  , &  
                               INFRARED_SENSOR   , &   
                               VISIBLE_SENSOR    , &   
                               ULTRAVIOLET_SENSOR, & 
                               SENSOR_TYPE_NAME  , &
                               SRF_type        , &
                               Associated_SRF  , &
                               Destroy_SRF     , &
                               Allocate_SRF    , &
                               CheckRelease_SRF, &
                               Info_SRF, &
                               Frequency_SRF, &
                               Integrate_SRF
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Public parameters
  PUBLIC :: MAX_N_SRF_CHANNELS
  ! Public procedures
  PUBLIC :: Read_SRF_ASCII_Header
  PUBLIC :: Write_SRF_ASCII_Header
  PUBLIC :: Read_SRF_ASCII
  PUBLIC :: Write_SRF_ASCII


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  ! Keyword set value
  INTEGER, PARAMETER :: UNSET = 0
  INTEGER, PARAMETER ::   SET = 1
  ! Maximum message string length
  INTEGER, PARAMETER :: ML = 256
  ! Header field delimiter
  CHARACTER(*), PARAMETER :: DELIMITER = ':'
  ! Maximum number of channels in an ASCII SRF file
  INTEGER, PARAMETER :: MAX_N_CHANNELS = 500
  INTEGER, PARAMETER :: MAX_N_SRF_CHANNELS = MAX_N_CHANNELS
  ! The data formats *for writing only*
  CHARACTER(*), PARAMETER :: CH_HDR_FMT    = '(i5)'
  CHARACTER(*), PARAMETER :: SRF_HDR_FMT   = '(3(1x,i5))'
  CHARACTER(*), PARAMETER :: RESPONSE_FMT  = '(8(1x,f9.6))'
  CHARACTER(*), PARAMETER :: BANDS_FMT     = '(8(1x,i5))'
  CHARACTER(*), PARAMETER :: FREQUENCY_FMT = '(8(1x,f12.5))'


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
!       Read_SRF_ASCII_Header
!
! PURPOSE:
!       Function to read the header information from an ASCII "HMW" format
!       SRF data file.
!
! CALLING SEQUENCE:
!       Error_Status = Read_SRF_ASCII_Header( Filename               , &  ! Input
!                                             FileID                 , &  ! In/Output
!                                             n_Channels             , &  ! Output
!                                             Channel_List           , &  ! Output
!                                             Sensor_Id              , &  ! Output       
!                                             Title                  , &  ! Output
!                                             History                , &  ! Output
!                                             Comment                , &  ! Output
!                                             Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:         Character string specifying the name of the
!                         ASCII format SRF data file to read.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       FileID:           ASCII file ID number. If file is not already opened,
!                         it is opened and this argument contains the unit
!                         number for the opened file upon output.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUT ARGUMENTS:
!       n_Channels:       The number of channels of data in the ASCII SRF file
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT)
!
!       Channel_List:     The list of channel numbers for which there is data
!                         in the ASCII SRF file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Rank-1, n_Channels or greater
!                         ATTRIBUTES: INTENT(OUT)
!
!       Sensor_Id:        Character string written into the SENSOR_ID
!                         global attribute field of the ASCII SRF
!                         file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT)
!
!       Title:            Character string written into the TITLE 
!                         attribute field of the ASCII SRF file.
!                         Should contain a succinct one-line description
!                         of what is in the datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT)
!
!       History:          Character string written into the HISTORY 
!                         attribute field of the ASCII SRF file.
!                         Should contain a succinct one-line description
!                         of the datafile history.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT)
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the ASCII SRF file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT)
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
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error
!                         status. The error codes are defined in the
!                         Message_Handler module.
!                         If == SUCCESS the header read was successful
!                            == FAILURE an error occurred
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       If an error occurs, the ASCII file is closed before exit.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Aug-2003
!                       paul.vandelst@ssec.wisc.edu
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Read_SRF_ASCII_Header( Filename    , &  ! Input
                                  FileID      , &  ! In/Output
                                  n_Channels  , &  ! Output
                                  Channel_List, &  ! Output
                                  Sensor_Id   , &  ! Output
                                  Title       , &  ! Output
                                  History     , &  ! Output
                                  Comment     , &  ! Output
                                  Message_Log ) &  ! Error messaging
                                RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)     :: Filename
    INTEGER,                INTENT(IN OUT) :: FileID
    INTEGER,                INTENT(OUT)    :: n_Channels
    INTEGER,                INTENT(OUT)    :: Channel_List(:)
    CHARACTER(*),           INTENT(OUT)    :: Title
    CHARACTER(*),           INTENT(OUT)    :: History
    CHARACTER(*),           INTENT(OUT)    :: Sensor_Id
    CHARACTER(*),           INTENT(OUT)    :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_SRF_ASCII_Header'
    INTEGER,      PARAMETER :: N_ATTRIBUTES = 4
    ! Local variables
    CHARACTER(ML)   :: msg
    CHARACTER(2000) :: Buffer
    CHARACTER(256)  :: Attribute_Name
    CHARACTER(2000) :: Attribute_Value
    INTEGER :: IO_Status
    INTEGER :: i, l, m, n

    ! Setup
    ! -----
    Error_Status = SUCCESS


    ! Open the file if required  
    !--------------------------
    IF ( .NOT. File_Open( TRIM(Filename) ) ) THEN

      ! Get a file unit number
      FileID = Get_Lun()
      IF ( FileID < 0 ) THEN
        msg = 'Error obtianing file unit number'
        CALL ReadHdr_Cleanup(); RETURN
      END IF

      ! Open the file
      OPEN( FileID, FILE   = TRIM(Filename), &
                    STATUS = 'OLD', &
                    FORM   = 'FORMATTED', &
                    ACCESS = 'SEQUENTIAL', &
                    ACTION = 'READ', &
                    IOSTAT = IO_Status )
      IF ( IO_Status /= 0 ) THEN
        WRITE( msg,'("Error opening ASCII SRF file ",a,". IOSTAT = ",i0)' ) &
                   TRIM(Filename), IO_Status
        CALL ReadHdr_Cleanup(); RETURN
      END IF

    ELSE

      ! File is already open, so rewind it
      REWIND( FileID )

    END IF


    ! Read the number of channels dimension
    ! -------------------------------------
    READ( FileID,FMT=CH_HDR_FMT,IOSTAT=IO_Status ) n_Channels
    IF ( IO_Status /= 0 ) THEN
      WRITE( msg,'("Error reading number of channels from ",a,". IOSTAT = ",i0)' ) &
                 TRIM(Filename), IO_Status
      CALL ReadHdr_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Check the size of the channel list argument
    ! -------------------------------------------
    IF ( SIZE(Channel_List) < n_Channels ) THEN
      WRITE( msg,'("CHANNEL_LIST argument not large enough to hold ",i0," channels.")' ) &
                 n_Channels
      CALL ReadHdr_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Read the channel list
    ! ---------------------
    DO l = 1, n_Channels
      READ( FileID,FMT=CH_HDR_FMT,IOSTAT=IO_Status ) Channel_List(l)
      IF ( IO_Status /= 0 ) THEN
        WRITE( msg,'("Error reading channel index ",i0," from ",a,". IOSTAT = ",i0)' ) &
                   l, TRIM(Filename), IO_Status
        CALL ReadHdr_Cleanup(Close_File=.TRUE.); RETURN
      END IF
    END DO


    ! Read the data attributes
    !-------------------------
    Attribute_Read_Loop: DO i = 1, N_ATTRIBUTES


      ! Buffer in the input
      ! -------------------
      READ( FileID,FMT='(a)',IOSTAT=IO_Status ) Buffer
      IF ( IO_Status /= 0 ) THEN
        WRITE( msg,'("Error reading attribute #",i0," from ",a,". IOSTAT = ",i0)' ) &
                   i, TRIM(Filename), IO_Status
        CALL ReadHdr_Cleanup(Close_File=.TRUE.); RETURN
      END IF


      ! Extract the attribute name and value
      ! ------------------------------------
      ! Get the string length and ensure it's not empty
      n = LEN_TRIM(Buffer)
      IF ( n == 0 ) THEN
        WRITE( msg,'("Attribute #",i0," from ",a," is empty.")' ) i, TRIM(Filename)
        CALL ReadHdr_Cleanup(Close_File=.TRUE.); RETURN
      END IF

      ! Search for the delimiter and check
      m = SCAN(Buffer,DELIMITER)
      IF ( m  == 0 .OR. &        ! No delimiter
           m  == 1 .OR. &        ! No attribute name
           m+1 > n      ) THEN   ! No attribute value
        WRITE( msg,'("Attribute #",i0," from ",a," delimiter scan failed.")' ) i, TRIM(Filename)
        CALL ReadHdr_Cleanup(Close_File=.TRUE.); RETURN
      END IF

      ! Extract the data from the buffer string
      Attribute_Name  = ADJUSTL(StrUpCase( Buffer(1:m-1) ))
      Attribute_Value = Buffer(m+1:n)


      ! Match the attribute by name and save
      ! ------------------------------------
      SELECT CASE( TRIM(Attribute_Name) )
        CASE('TITLE')
          Title = TRIM(Attribute_Value)
        CASE('HISTORY')
          History = TRIM(Attribute_Value)
        CASE('SENSOR_ID')
          Sensor_Id = TRIM(Attribute_Value)
        CASE('COMMENT')
          Comment = TRIM(Attribute_Value)
        CASE DEFAULT
          msg = 'Unrecognised attribute name: '//TRIM(Attribute_Name)
          CALL ReadHdr_Cleanup(Close_File=.TRUE.); RETURN
      END SELECT

    END DO Attribute_Read_Loop

  CONTAINS
  
    SUBROUTINE ReadHdr_CleanUp(Close_File)
      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
      ! Close file
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File ) THEN
          CLOSE( FileId,IOSTAT=Error_Status )
          IF ( Error_Status /= 0 ) &
            msg = TRIM(msg)//'; Error closing input file during error cleanup'
        END IF
      END IF
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
    END SUBROUTINE ReadHdr_CleanUp

  END FUNCTION Read_SRF_ASCII_Header


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Write_SRF_ASCII_Header
!
! PURPOSE:
!       Function to write the header information to an ASCII "HMW" format
!       SRF data file.
!
! CALLING SEQUENCE:
!       Error_Status = Write_SRF_ASCII_Header( Filename               , &  ! Input
!                                              FileID                 , &  ! In/Output
!                                              n_Channels             , &  ! Input
!                                              Channel_List           , &  ! Input
!                                              Title                  , &  ! Input
!                                              History                , &  ! Input
!                                              Sensor_Id              , &  ! Input
!                                              Comment                , &  ! Input
!                                              Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:         Character string specifying the name of the
!                         ASCII format SRF data file to read.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       FileID:           ASCII file ID number. If file is not already opened,
!                         it is opened and this argument contains the unit
!                         number for the opened file upon output.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN OUT)
!
!       n_Channels:       The number of channels of data to be written to
!                         the ASCII SRF file
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       Channel_List:     The list of channel numbers to be written to 
!                         the ASCII SRF file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Rank-1, n_Channels or greater
!                         ATTRIBUTES: INTENT(IN)
!
!       Title:            Character string written into the TITLE 
!                         attribute field of the ASCII SRF file.
!                         Should contain a succinct one-line description
!                         of what is in the datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       History:          Character string written into the HISTORY 
!                         attribute field of the ASCII SRF file.
!                         Should contain a succinct one-line description
!                         of the datafile history.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       Sensor_Id:        Character string written into the SENSOR_ID
!                         global attribute field of the ASCII SRF
!                         file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the ASCII SRF file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
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
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error
!                         status. The error codes are defined in the
!                         Message_Handler module.
!                         If == SUCCESS the header write was successful
!                            == FAILURE an error occurred
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       If an error occurs, the ASCII file is closed.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Aug-2003
!                       paul.vandelst@ssec.wisc.edu
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Write_SRF_ASCII_Header( Filename    , &  ! Input
                                   FileID      , &  ! In/Output
                                   n_Channels  , &  ! Input
                                   Channel_List, &  ! Input
                                   Title       , &  ! Input
                                   History     , &  ! Input
                                   Sensor_Id   , &  ! Input
                                   Comment     , &  ! Input
                                   Message_Log ) &  ! Error messaging
                                 RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)     :: Filename
    INTEGER,                INTENT(IN OUT) :: FileID
    INTEGER,                INTENT(IN)     :: n_Channels
    INTEGER,                INTENT(IN)     :: Channel_List(:)
    CHARACTER(*),           INTENT(IN)     :: Title
    CHARACTER(*),           INTENT(IN)     :: History
    CHARACTER(*),           INTENT(IN)     :: Sensor_Id
    CHARACTER(*),           INTENT(IN)     :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_SRF_ASCII_Header'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(80) :: Action
    INTEGER :: IO_Status
    INTEGER :: l

    ! Setup
    ! -----
    Error_Status = SUCCESS


    ! Open the file if required  
    !--------------------------
    IF ( .NOT. File_Open( TRIM(Filename) ) ) THEN

      ! Get a file unit number
      FileID = Get_Lun()
      IF ( FileID < 0 ) THEN
        msg = 'Error obtianing file unit number'
        CALL WriteHdr_Cleanup(); RETURN
      END IF

      ! Open the file
      OPEN( FileID, FILE   = TRIM(Filename), &
                    STATUS = 'REPLACE', &
                    FORM   = 'FORMATTED', &
                    ACCESS = 'SEQUENTIAL', &
                    ACTION = 'WRITE', &
                    IOSTAT = IO_Status )
      IF ( IO_Status /= 0 ) THEN
        WRITE( msg,'("Error opening ASCII SRF file ",a,". IOSTAT = ",i0)' ) &
                   TRIM(Filename), IO_Status
        CALL WriteHdr_Cleanup(); RETURN
      END IF

    ELSE

      ! File is already open, so rewind it
      REWIND( FileID )

      ! Check that the file can be written to
      INQUIRE( UNIT=FileID,ACTION=Action )
      IF ( INDEX(Action,'WRITE') == 0 ) THEN
        msg = 'ASCII SRF file '//TRIM(Filename)//' is not opened for writing.'
        CALL WriteHdr_Cleanup(Close_File=.TRUE.); RETURN
      END IF

    END IF


    ! Check the size of the channel list argument
    ! -------------------------------------------
    IF ( SIZE(Channel_List) < n_Channels ) THEN
      WRITE( msg,'("CHANNEL_LIST argument smaller than N_CHANNELS dimension: ",i0)' ) n_Channels
      CALL WriteHdr_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Write the channel dimension
    ! ---------------------------
    WRITE( FileID,FMT=CH_HDR_FMT,IOSTAT=IO_Status ) n_Channels
    IF ( IO_Status /= 0 ) THEN
      WRITE( msg,'("Error writing channel dimension to ",a,". IOSTAT = ",i0)' ) &
                 TRIM(Filename), IO_Status
      CALL WriteHdr_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Write the channel list
    ! ----------------------
    DO l = 1, n_Channels
      WRITE( FileID,FMT=CH_HDR_FMT,IOSTAT=IO_Status ) Channel_List(l)
      IF ( IO_Status /= 0 ) THEN
        WRITE( msg,'("Error writing channel index ",i0," to ",a,". IOSTAT = ",i0)' ) &
                   l, TRIM(Filename), IO_Status
        CALL WriteHdr_Cleanup(Close_File=.TRUE.); RETURN
      END IF
    END DO


    ! Write the data attributes
    !--------------------------
    ! The Title
    WRITE( FileID,FMT='("Title:",a)',IOSTAT=IO_Status) TRIM(Title)
    IF ( IO_Status /= 0 ) THEN
      WRITE( msg,'("Error writing Title attribute to ",a,". IOSTAT = ",i0 )' ) &
                 TRIM(Filename), IO_Status
      CALL WriteHdr_Cleanup(Close_File=.TRUE.); RETURN
    END IF

    ! The History
    WRITE( FileID,FMT='("History:",a)',IOSTAT=IO_Status) TRIM(History)
    IF ( IO_Status /= 0 ) THEN
      WRITE( msg,'("Error writing History attribute to ",a,". IOSTAT = ",i0 )' ) &
                 TRIM(Filename), IO_Status
      CALL WriteHdr_Cleanup(Close_File=.TRUE.); RETURN
    END IF

    ! The Sensor_Id
    WRITE( FileID,FMT='("Sensor_Id:",a)',IOSTAT=IO_Status) TRIM(Sensor_Id)
    IF ( IO_Status /= 0 ) THEN
      WRITE( msg,'("Error writing Sensor_Id attribute to ",a,". IOSTAT = ",i0 )' ) &
                 TRIM(Filename), IO_Status
      CALL WriteHdr_Cleanup(Close_File=.TRUE.); RETURN
    END IF

    ! The Comment
    WRITE( FileID,FMT='("Comment:",a)',IOSTAT=IO_Status) TRIM(Comment)
    IF ( IO_Status /= 0 ) THEN
      WRITE( msg,'("Error writing Comment attribute to ",a,". IOSTAT = ",i0 )' ) &
                 TRIM(Filename), IO_Status
      CALL WriteHdr_Cleanup(Close_File=.TRUE.); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE WriteHdr_CleanUp(Close_File)
      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
      ! Close file
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File ) THEN
          CLOSE( FileId,IOSTAT=Error_Status )
          IF ( Error_Status /= 0 ) &
            msg = TRIM(msg)//'; Error closing input file during error cleanup'
        END IF
      END IF
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
    END SUBROUTINE WriteHdr_CleanUp

  END FUNCTION Write_SRF_ASCII_Header


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Read_SRF_ASCII
!
! PURPOSE:
!       Function to read a channel SRF from an ASCII "HMW" format SRF file.
!
! CALLING SEQUENCE:
!       Error_Status = Read_SRF_ASCII( Filename               , &  ! Input
!                                      FileID                 , &  ! Input
!                                      Channel                , &  ! Input
!                                      SRF                    , &  ! Output
!                                      Quiet      =Quiet      , &  ! Optional Input
!                                      RCS_Id     =RCS_Id     , &  ! Revision control
!                                      Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:         Character string specifying the name of the
!                         ASCII format SRF data file to inquire.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       FileID:           ASCII file ID number.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       Channel:          Channel number for which the SRF response and frequency
!                         are required.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       SRF:              Data structure containing the requested channel SRF data.
!                         UNITS:      N/A
!                         TYPE:       TYPE(SRF_type)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:            Use this argument to suppress information msgs being
!                         printed to standard output (or the msg log file if
!                         the Message_Log optional argument is used.) By default,
!                         information msgs are printed.
!                         If QUIET = 0, information msgs are OUTPUT (default)
!                            QUIET = 1, information msgs are *SUPPRESSED*.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!       Message_Log:      Character string specifying a filename in which any
!                         msgs will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output msgs to standard output.
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
!       Error_Status:     The return value is an integer defining the error
!                         status. The error codes are defined in the
!                         Message_Handler module.
!                         If == SUCCESS the file read was successful
!                            == FAILURE an error occurred reading the file
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       File is closed if an error occurs.
!
! RESTRICTIONS:
!       The SRF header must be read before this function can be called.
!
! COMMENTS:
!       Note the INTENT on the output SRF argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined on
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Aug-2003
!                       paul.vandelst@ssec.wisc.edu
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Read_SRF_ASCII( Filename    , &  ! Input
                           FileID      , &  ! Input
                           Channel     , &  ! Input
                           SRF         , &  ! Output
                           Quiet       , &  ! Optional input
                           RCS_Id      , &  ! Revision control
                           Message_Log ) &  ! Error messaging
                         RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)     :: Filename
    INTEGER,                INTENT(IN)     :: FileID
    INTEGER,                INTENT(IN)     :: Channel
    TYPE(SRF_type),         INTENT(IN OUT) :: SRF
    INTEGER,      OPTIONAL, INTENT(IN)     :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_SRF_ASCII'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
    INTEGER :: IO_Status(4)
    INTEGER :: ch
    INTEGER :: n_Points, n_Bands, i
    REAL(fp) :: rdummy
    INTEGER  :: idummy

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID
  
    ! Is the file open?
    IF ( .NOT. File_Open( FileID ) ) THEN
      msg = 'ASCII SRF file '//TRIM(Filename)//' is not open.'
      CALL Read_Cleanup(); RETURN
    END IF

    ! Output information messages...
    Noisy = .TRUE.
    ! ...unless the QUIET keyword is set
    IF ( PRESENT(Quiet) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF


    ! Begin loop to skip over unwanted channels
    !------------------------------------------
    Channel_Skip_Loop: DO


      ! Read the current channel SRF header
      ! -----------------------------------
      READ( FileID,FMT=SRF_HDR_FMT,IOSTAT=IO_Status(1) ) ch, n_Points, n_Bands

      ! End of file? EOF is most likely to occur
      ! here if there is a channel number mixup
      IF ( IO_Status(1) < 0 ) THEN
        WRITE( msg,'("End of file reached reading SRF header from ",a,". IOSTAT = ",i0)' ) &
                   TRIM(Filename), IO_Status(1)
        CALL Read_Cleanup(Close_File=.TRUE.); RETURN
      END IF
      ! Error?
      IF ( IO_Status(1) > 0 ) THEN
        Error_Status = FAILURE
        WRITE( msg,'("Error reading SRF header from ",a,". IOSTAT = ",i0)' ) &
                   TRIM(Filename), IO_Status(1)
        CALL Read_Cleanup(Close_File=.TRUE.); RETURN
      END IF


      ! Exit loop if this is the required channel...
      ! --------------------------------------------
      IF ( ch == Channel ) EXIT Channel_Skip_Loop


      ! Otherwise, skip over the current channel's data
      ! -----------------------------------------------
      READ( FileID,FMT=FREQUENCY_FMT,IOSTAT=IO_Status(1) ) ( rdummy,i=1,n_Bands )
      READ( FileID,FMT=FREQUENCY_FMT,IOSTAT=IO_Status(2) ) ( rdummy,i=1,n_Bands )
      READ( FileID,FMT=BANDS_FMT    ,IOSTAT=IO_Status(3) ) ( idummy,i=1,n_Bands )
      READ( FileID,FMT=RESPONSE_FMT ,IOSTAT=IO_Status(4) ) ( rdummy,i=1,n_Points )
      IF ( ANY(IO_Status /= 0) ) THEN
        WRITE( msg,'("Error skipping over channel ",i0," SRF data in ",a,". IOSTAT = ",4(1x,i0))' ) &
                   ch, TRIM(Filename), IO_Status
        CALL Read_Cleanup(Close_File=.TRUE.); RETURN
      END IF

    END DO Channel_Skip_Loop


    ! Allocate the SRF data structure arrays
    ! --------------------------------------
    Error_Status = Allocate_SRF( n_Points,SRF,n_Bands=n_Bands,Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( msg,'("Error allocating SRF structure for channel #",i0)' ) Channel
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! Set the channel number
    SRF%Channel = Channel


    ! Read the required instrument response data
    ! ------------------------------------------
    READ( FileID,FMT=FREQUENCY_FMT,IOSTAT=IO_Status(1) ) SRF%f1_Band
    READ( FileID,FMT=FREQUENCY_FMT,IOSTAT=IO_Status(2) ) SRF%f2_Band
    READ( FileID,FMT=BANDS_FMT    ,IOSTAT=IO_Status(3) ) SRF%npts_Band
    READ( FileID,FMT=RESPONSE_FMT ,IOSTAT=IO_Status(4) ) SRF%Response
    IF ( ANY(IO_Status /= 0) ) THEN
      WRITE( msg,'("Error reading channel ",i0," SRF data from ",a,". IOSTAT = ",4(1x,i0))' ) &
                 SRF%Channel, TRIM(Filename), IO_Status
      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
    END IF


    ! Compute the SRF frequency grid 
    !-------------------------------
    Error_Status = Frequency_SRF( SRF,Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( msg,'("Error computing frequency grid for channel ",i0," SRF from ",a)' ) &
                 Channel, TRIM(Filename)
      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
    END IF


    ! Compute the integrated SRF values
    ! ---------------------------------
    Error_Status = Integrate_SRF( SRF,Message_Log=Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( msg,'("Error integrating channel ",i0," SRF from ",a)' ) &
                 Channel, TRIM(Filename)
      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_SRF( SRF, msg )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(Filename)//'; '//TRIM(msg), &
                            INFORMATION, &
                            Message_Log=Message_Log )
    END IF

  CONTAINS
  
    SUBROUTINE Read_CleanUp( Close_File, Destroy_Structure )
      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
      LOGICAL, OPTIONAL, INTENT(IN) :: Destroy_Structure
      ! Close file
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File ) THEN
          CLOSE( FileId,IOSTAT=Error_Status )
          IF ( Error_Status /= 0 ) &
            msg = TRIM(msg)//'; Error closing input file during error cleanup'
        END IF
      END IF
      ! Destroy the structure if necessary
      IF ( PRESENT(Destroy_Structure) ) THEN
        IF ( Destroy_Structure ) THEN
          Error_Status = Destroy_SRF(SRF, Message_Log=Message_Log)
          IF ( Error_Status /= SUCCESS ) &
            msg = TRIM(msg)//'; Error destroying SRF structure during error cleanup.'
        END IF
      END IF
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(msg), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE Read_CleanUp

  END FUNCTION Read_SRF_ASCII


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Write_SRF_ASCII
!
! PURPOSE:
!       Function to write a channel SRF to an ASCII format SRF file.
!
! CALLING SEQUENCE:
!       Error_Status = Write_SRF_ASCII( Filename               , &  ! Input
!                                       FileID                 , &  ! Input
!                                       SRF                    , &  ! Output
!                                       Quiet      =Quiet      , &  ! Optional Input
!                                       RCS_Id     =RCS_Id     , &  ! Revision control
!                                       Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:         Character string specifying the name of the
!                         ASCII format SRF data file to inquire.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       FileID:           ASCII file ID number.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       SRF:              Data structure containing the requested channel SRF data.
!                         UNITS:      N/A
!                         TYPE:       TYPE(SRF_type)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:      Character string specifying a filename in which any
!                         msgs will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output msgs to standard output.
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
!       Error_Status:     The return value is an integer defining the error
!                         status. The error codes are defined in the
!                         Message_Handler module.
!                         If == SUCCESS the file write was successful
!                            == FAILURE an error occurred writing to file
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       Output file is closed if an I/O error occurs.
!
! RESTRICTIONS:
!       The SRF header must be written before this function can be called.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Aug-2003
!                       paul.vandelst@ssec.wisc.edu
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Write_SRF_ASCII( Filename    , &  ! Input
                            FileID      , &  ! Input
                            SRF         , &  ! Input
                            Quiet       , &  ! Optional input
                            RCS_Id      , &  ! Revision control
                            Message_Log ) &  ! Error messaging
                          RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER,                INTENT(IN)  :: FileID
    TYPE(SRF_type),         INTENT(IN)  :: SRF
    INTEGER,      OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_SRF_ASCII'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
    INTEGER :: IO_Status(4)

    ! Setup
    ! -----
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! Check structure pointer association status
    ! ALL structure pointers must be associated
    IF ( .NOT. Associated_SRF( SRF ) ) THEN
      msg = 'Some or all INPUT SRF pointer members are NOT associated.'
      CALL Write_Cleanup(); RETURN
    END IF

    ! Check SRF channel is valid
    IF ( SRF%Channel < 1 ) THEN
      WRITE( msg,'("Invalid SRF channel, ",i0,". Must be > 0.")' ) SRF%Channel
      CALL Write_CleanUp(); RETURN
    END IF

    ! Check SRF array sizes
    IF ( SRF%n_Points < 1 ) THEN
      WRITE( msg,'("Invalid no. of SRF points, ",i0,". Must be > 0.")' ) SRF%n_Points
      CALL Write_CleanUp(); RETURN
    END IF
    IF ( SRF%n_Bands < 1 ) THEN
      WRITE( msg,'("Invalid no. of SRF bands, ",i0,". Must be > 0.")' ) SRF%n_Bands
      CALL Write_CleanUp(); RETURN
    END IF

    ! Is the file open?
    IF ( .NOT. File_Open( FileID ) ) THEN
      msg = 'ASCII SRF file '//TRIM(Filename)//' is not open.'
      CALL Write_Cleanup(); RETURN
    END IF

    ! Output information messages...
    Noisy = .TRUE.
    ! ...unless the QUIET keyword is set
    IF ( PRESENT(Quiet) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF


    ! Write the current channel SRF header
    ! ------------------------------------
    WRITE( FileID,FMT=SRF_HDR_FMT,IOSTAT=IO_Status(1) ) SRF%Channel,SRF%n_Points,SRF%n_Bands 
    IF ( IO_Status(1) /= 0 ) THEN
      WRITE( msg,'("Error writing channel ",i0," SRF header to ",a,". IOSTAT = ",i0)' ) &
                 SRF%Channel, TRIM(Filename), IO_Status(1)
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Write the SRF data
    ! ------------------
    WRITE( FileID,FMT=FREQUENCY_FMT,IOSTAT=IO_Status(1) ) SRF%f1_Band
    WRITE( FileID,FMT=FREQUENCY_FMT,IOSTAT=IO_Status(2) ) SRF%f2_Band 
    WRITE( FileID,FMT=BANDS_FMT    ,IOSTAT=IO_Status(3) ) SRF%npts_Band 
    WRITE( FileID,FMT=RESPONSE_FMT ,IOSTAT=IO_Status(4) ) SRF%Response
    IF ( ANY(IO_Status /= 0) ) THEN
      WRITE( msg,'("Error writing channel ",i0," SRF data to ",a,". IOSTAT = ",4(1x,i0))' ) &
                 SRF%Channel, TRIM(Filename), IO_Status
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_SRF( SRF, msg )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(Filename)//'; '//TRIM(msg), &
                            INFORMATION, &
                            Message_Log=Message_Log )
    END IF

  CONTAINS
  
    SUBROUTINE Write_CleanUp( Close_File )
      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
      ! Close file
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File ) THEN
          CLOSE( FileId,IOSTAT=Error_Status )
          IF ( Error_Status /= 0 ) &
            msg = TRIM(msg)//'; Error closing output file during error cleanup'
        END IF
      END IF
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(msg), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE Write_CleanUp

  END FUNCTION Write_SRF_ASCII

END MODULE SRF_ASCII_IO
 
