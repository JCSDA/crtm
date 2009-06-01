!
! RadDiag_IO
!
! Module to read GSI radiance diagnostic files
!

MODULE RadDiag_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds,      ONLY: sp=>Single
  USE File_Utility,    ONLY: Get_Lun
  USE Message_Handler, ONLY: SUCCESS, FAILURE, EOF, &
                             Display_Message
  USE RadDiag_Define,  ONLY: RadDiag_Hdr_Scalar_type,  RadDiag_Hdr_Channel_type,  RadDiag_Hdr_type, &
                             RadDiag_Data_Scalar_type, RadDiag_Data_Channel_type, RadDiag_Data_type, &
                             RADDIAG_NFPELEMENTS, &
                             RADDIAG_NCHELEMENTS, &
                             RADDIAG_NPRELEMENTS, &
                             Associated_RadDiag_Hdr, &
                             Destroy_RadDiag_Hdr,    &
                             Allocate_RadDiag_Hdr,   &
                             Assign_RadDiag_Hdr,     &
                             Associated_RadDiag_Data, &
                             Destroy_RadDiag_Data,    &
                             Allocate_RadDiag_Data,   &
                             Assign_RadDiag_Data
  ! Disable implicit typing
  IMPLICIT NONE


  ! ---------------------
  ! Explicit visibilities
  ! ---------------------
  PRIVATE
  ! Inherited derived type definitions
  PUBLIC :: RadDiag_Hdr_type
  PUBLIC :: RadDiag_Data_type
  ! Inherited module subprograms
  PUBLIC :: Associated_RadDiag_Hdr
  PUBLIC :: Destroy_RadDiag_Hdr
  PUBLIC :: Allocate_RadDiag_Hdr
  PUBLIC :: Assign_RadDiag_Hdr
  PUBLIC :: Associated_RadDiag_Data
  PUBLIC :: Destroy_RadDiag_Data
  PUBLIC :: Allocate_RadDiag_Data
  PUBLIC :: Assign_RadDiag_Data
  ! Module parameters
  PUBLIC :: RADDIAG_READMODE  
  PUBLIC :: RADDIAG_WRITEMODE 
  PUBLIC :: RADDIAG_APPENDMODE
  ! Module subprograms
  PUBLIC :: Open_RadDiag
  PUBLIC :: Read_RadDiag_Hdr
  PUBLIC :: Read_RadDiag_Data
  PUBLIC :: Write_RadDiag_Hdr
  PUBLIC :: Write_RadDiag_Data


  ! -----------------
  ! Module parameters
  ! -----------------
  INTEGER,      PARAMETER :: RADDIAG_READMODE   = 1
  INTEGER,      PARAMETER :: RADDIAG_WRITEMODE  = 2
  INTEGER,      PARAMETER :: RADDIAG_APPENDMODE = 3
  CHARACTER(*), PARAMETER, DIMENSION(3) :: RADDIAG_MODENAME = (/ 'read  ', &
                                                                 'write ', &
                                                                 'append' /)
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id$'


CONTAINS


!------------------------------------------------------------------------------------
!
! Function to open a radiance diagnostic file for reading or writing
!
! CALLING SEQUENCE:
!   Error_Status = Open_RadDiag( Filename,                 &  ! Input
!                                FileID,                   &  ! Output
!                                AccessMode = AccessMode,  &  ! Optional input
!                                RCS_Id = RCS_Id,          &  ! Revision control
!                                Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!   Filename:           Name of the radiance diagnostic file to open.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!   FileID:             File unit number than can be used for subsequent
!                       file access.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!   AccessMode:         Integer flag specifying the type of file access required.
!                       Valid parameter values are:
!                         RADDIAG_READMODE:   Open existing file for reading.
!                         RADDIAG_WRITEMODE:  Open new file for writing. 
!                         RADDIAG_APPENDMODE: Open existing file for writing.
!                       If not specified, RADDIAG_READMODE is the default.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!   Message_Log:        Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!   RCS_Id:             Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!   Error_Status:       The return value is an integer defining the error status.
!                       The error codes are defined in the ERROR_HANDLER module.
!                       If == SUCCESS the file open was successful
!                          == FAILURE - an invalid access mode was specified, or
!                                     - an error occurred opening the file for
!                                       the given access mode.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!------------------------------------------------------------------------------------

  FUNCTION Open_RadDiag( Filename,     &  ! Input
                         FileID,       &  ! Output
                         AccessMode,   &  ! Optional input
                         RCS_Id,       &  ! Revision control
                         Message_Log ) &  ! Error messaging
                       RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER,                INTENT(OUT) :: FileID    
    INTEGER,      OPTIONAL, INTENT(IN)  :: AccessMode
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local Parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Open_RadDiag'
    ! Local Variables
    CHARACTER(256) :: Message
    INTEGER :: Lun
    INTEGER :: Mode
    INTEGER :: IO_Status
    CHARACTER(10) :: File_Status
    CHARACTER(10) :: File_Position
    CHARACTER(10) :: File_Action


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    FileID = 0
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
    ! Open the file for reading by default
    Mode = RADDIAG_READMODE
    IF ( PRESENT( AccessMode ) ) Mode = AccessMode


    ! --------------------------
    ! Assign the OPEN specifiers
    ! --------------------------
    SELECT CASE ( Mode )
      CASE (RADDIAG_READMODE)
        File_Status   = 'OLD'
        File_Position = 'ASIS'
        File_Action   = 'READ'
      CASE (RADDIAG_WRITEMODE)
        File_Status   = 'REPLACE'
        File_Position = 'ASIS'
        File_Action   = 'WRITE'
      CASE (RADDIAG_APPENDMODE)
        File_Status   = 'UNKNOWN'
        File_Position = 'APPEND'
        File_Action   = 'READWRITE'
      CASE DEFAULT
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Invalid RadDiag file access mode.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
    END SELECT


    ! ----------------------
    ! Get a free unit number
    ! ----------------------
    Lun = Get_Lun()
    IF ( Lun < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining file unit number for '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -------------
    ! Open the file
    ! -------------
    OPEN( Lun, FILE     = TRIM( Filename ), &
               STATUS   = TRIM( File_Status ), &
               POSITION = TRIM( File_Position ), &
               ACTION   = TRIM( File_Action ), &
               ACCESS   = 'SEQUENTIAL', &
               FORM     = 'UNFORMATTED', &
               IOSTAT   = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error opening ", a, " for ", a, " access. IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), TRIM( RADDIAG_MODENAME(Mode) ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -------------------------
    ! Update the return file id
    ! -------------------------
    FileID = Lun

  END FUNCTION Open_RadDiag


!------------------------------------------------------------------------------------
!
! Function to read a radiance diagnostic file header
!
! CALLING SEQUENCE:
!   Error_Status = Read_RadDiag_Hdr( Filename,                 &  ! Input
!                                    FileID,                   &  ! Input
!                                    RadDiag_Hdr,              &  ! Output
!                                    RCS_Id = RCS_Id,          &  ! Revision control
!                                    Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!   Filename:           Name of the radiance diagnostic file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!   FileID:             File unit number of the radiance diagnostic file to read.
!                       Returned from call to Open_RadDiag() function.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!   RadDiag_Hdr:        RadDiag header structure read from file.
!                       UNITS:      N/A
!                       TYPE:       TYPE(RadDiag_Hdr_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!   Message_Log:        Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!   RCS_Id:             Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!   Error_Status:       The return value is an integer defining the error status.
!                       The error codes are defined in the ERROR_HANDLER module.
!                       If == SUCCESS the file read was successful.
!                          == FAILURE an error occurred reading the file.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!------------------------------------------------------------------------------------

  FUNCTION Read_RadDiag_Hdr( Filename,     &  ! Input
                             FileID,       &  ! Input
                             RadDiag_Hdr,  &  ! Output
                             RCS_Id,       &  ! Revision control
                             Message_Log ) &  ! Error messaging
                           RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)     :: Filename
    INTEGER,                INTENT(IN)     :: FileID
    TYPE(RadDiag_Hdr_type), INTENT(IN OUT) :: RadDiag_Hdr
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_RadDiag_Hdr'
    ! Local variables
    CHARACTER(256) :: Message
    TYPE(RadDiag_Hdr_Scalar_type) :: Scalar
    INTEGER :: IO_Status
    INTEGER :: Destroy_Status
    INTEGER :: i

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Read the fixed part of the header
    READ( FileID, IOSTAT=IO_Status ) Scalar%isis   , &
                                     Scalar%id     , &
                                     Scalar%obstype, &
                                     Scalar%jiter  , &
                                     Scalar%nchan  , &
                                     Scalar%npred  , &
                                     Scalar%idate  , &
                                     Scalar%ireal  , &
                                     Scalar%ipchan , &
                                     Scalar%iextra , &
                                     Scalar%jextra 
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading RadDiag header fixed portion from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF

    ! Check the header format/dimensions
    IF( Scalar%ireal  /= RADDIAG_NFPELEMENTS .OR. &       ! Number of floating point elements
        Scalar%ipchan /= RADDIAG_NCHELEMENTS .OR. &       ! Number of channel elements
        Scalar%npred  /= RADDIAG_NPRELEMENTS      ) THEN  ! Number of bias correction terms
      Error_Status = FAILURE
      Message = 'Invalid RadDiag_Hdr dimension values.'
      GOTO 1000
    END IF

    ! Allocate the RadDiag_Hdr structure
    Error_Status = Allocate_RadDiag_Hdr( Scalar%nchan, &
                                         RadDiag_Hdr, & 
                                         Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error allocating RadDiag_Hdr structure'
      GOTO 1000
    END IF

    ! Copy the fixed portion of the header
    RadDiag_Hdr%Scalar = Scalar

    ! Read the channel portion of the header
    DO i = 1, RadDiag_Hdr%nChannels
      READ( FileID, IOSTAT=IO_Status ) RadDiag_Hdr%Channel(i)%freq    , &
                                       RadDiag_Hdr%Channel(i)%polar   , &
                                       RadDiag_Hdr%Channel(i)%wave    , &
                                       RadDiag_Hdr%Channel(i)%varch   , &
                                       RadDiag_Hdr%Channel(i)%tlapmean, &
                                       RadDiag_Hdr%Channel(i)%iuse    , &
                                       RadDiag_Hdr%Channel(i)%nuchan  , &
                                       RadDiag_Hdr%Channel(i)%iochan  
      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error reading RadDiag header channel index ", i5, &
                          &" data from ", a, ". IOSTAT = ", i5 )' ) &
                        i, TRIM( Filename ), IO_Status
        GOTO 1000
      END IF
    END DO

    RETURN


    ! ----------------------
    ! Process FAILURE errors
    ! ----------------------
    1000 CONTINUE
    CLOSE( FileID )
    Destroy_Status = Destroy_RadDiag_Hdr( RadDiag_Hdr, & 
                                          Message_Log = Message_Log )
    IF ( Destroy_Status /= SUCCESS ) THEN
      Message = TRIM(Message)//'; Error destroying RadDiag_Hdr'
    END IF
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )
    
  END FUNCTION Read_RadDiag_Hdr


!------------------------------------------------------------------------------------
!
! Function to read a radiance diagnostic file's data
!
! CALLING SEQUENCE:
!   Error_Status = Read_RadDiag_Data( Filename,                 &  ! Input
!                                     FileID,                   &  ! Input
!                                     RadDiag_Hdr,              &  ! Input
!                                     RadDiag_Data,             &  ! Output
!                                     RCS_Id = RCS_Id,          &  ! Revision control
!                                     Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!   Filename:           Name of the radiance diagnostic file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!   FileID:             File unit number of the radiance diagnostic file to read.
!                       Returned from call to Open_RadDiag() function.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!   RadDiag_Hdr:        RadDiag header structure for the file.
!                       UNITS:      N/A
!                       TYPE:       TYPE(RadDiag_Hdr_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!   RadDiag_Data:       RadDiag data structure read from file.
!                       UNITS:      N/A
!                       TYPE:       TYPE(RadDiag_Data_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!   Message_Log:        Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!   RCS_Id:             Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!   Error_Status:       The return value is an integer defining the error status.
!                       The error codes are defined in the ERROR_HANDLER module.
!                       If == SUCCESS the file read was successful.
!                          == FAILURE an error occurred reading the file.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!------------------------------------------------------------------------------------

  FUNCTION Read_RadDiag_Data( Filename,     &  ! Input
                              FileID,       &  ! Input
                              RadDiag_Hdr,  &  ! Input
                              RadDiag_Data, &  ! Output
                              RCS_Id,       &  ! Revision control
                              Message_Log ) &  ! Error messaging
                            RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),            INTENT(IN)     :: Filename
    INTEGER,                 INTENT(IN)     :: FileID
    TYPE(RadDiag_Hdr_type),  INTENT(IN)     :: RadDiag_Hdr
    TYPE(RadDiag_Data_type), INTENT(IN OUT) :: RadDiag_Data
    CHARACTER(*), OPTIONAL,  INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL,  INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_RadDiag_Data'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: IO_Status
    INTEGER :: Destroy_Status
    INTEGER :: i
    REAL(sp), DIMENSION(RadDiag_Hdr%Scalar%iextra) :: Extra

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Allocate the RadDiag_Data structure
    Error_Status = Allocate_RadDiag_Data( RadDiag_Hdr%nChannels, & 
                                          RadDiag_Data, &
                                          Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error allocating RadDiag_Data structure'
      GOTO 1000
    END IF

    ! Read all the data
    READ( FileID, IOSTAT=IO_Status ) &
      RadDiag_Data%Scalar%lat       , &
      RadDiag_Data%Scalar%lon       , &
      RadDiag_Data%Scalar%zsges     , &
      RadDiag_Data%Scalar%obstime   , &
      RadDiag_Data%Scalar%senscn_pos, &
      RadDiag_Data%Scalar%satzen_ang, &
      RadDiag_Data%Scalar%satazm_ang, &
      RadDiag_Data%Scalar%solzen_ang, &
      RadDiag_Data%Scalar%solazm_ang, &
      RadDiag_Data%Scalar%sungln_ang, &
      RadDiag_Data%Scalar%water_frac, &
      RadDiag_Data%Scalar%land_frac , &
      RadDiag_Data%Scalar%ice_frac  , &
      RadDiag_Data%Scalar%snow_frac , &
      RadDiag_Data%Scalar%water_temp, &
      RadDiag_Data%Scalar%land_temp , &
      RadDiag_Data%Scalar%ice_temp  , &
      RadDiag_Data%Scalar%snow_temp , &
      RadDiag_Data%Scalar%soil_temp , &
      RadDiag_Data%Scalar%soil_mois , &
      RadDiag_Data%Scalar%land_type , &
      RadDiag_Data%Scalar%veg_frac  , &
      RadDiag_Data%Scalar%snow_depth, &
      RadDiag_Data%Scalar%sfc_wndspd, &
      RadDiag_Data%Scalar%qcdiag1   , &
      RadDiag_Data%Scalar%qcdiag2   , ( RadDiag_Data%Channel(i)%tbobs , &
                                        RadDiag_Data%Channel(i)%omgbc , &
                                        RadDiag_Data%Channel(i)%omgnbc, &
                                        RadDiag_Data%Channel(i)%errinv, & 
                                        RadDiag_Data%Channel(i)%qcmark, & 
                                        RadDiag_Data%Channel(i)%emiss , &  
                                        RadDiag_Data%Channel(i)%tlap  , & 
                                        RadDiag_Data%Channel(i)%bifix , & 
                                        RadDiag_Data%Channel(i)%bilap , &
                                        RadDiag_Data%Channel(i)%bilap2, &
                                        RadDiag_Data%Channel(i)%bicons, & 
                                        RadDiag_Data%Channel(i)%biang , &  
                                        RadDiag_Data%Channel(i)%biclw , & 
                                        i=1,RadDiag_Data%nChannels      ), Extra

    ! First check for end of file
    IF ( IO_Status < 0 ) THEN
      Error_Status = EOF
      WRITE( Message, '( "End-of-file ", a, " reached." )' ) TRIM( Filename )
      GOTO 1000
    END IF

    ! Now check for error
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading RadDiag Data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF

    RETURN


    ! ----------------------
    ! Process FAILURE errors
    ! ----------------------
    1000 CONTINUE
    CLOSE( FileID )
    Destroy_Status = Destroy_RadDiag_Data( RadDiag_Data, & 
                                           Message_Log = Message_Log )
    IF ( Destroy_Status /= SUCCESS ) THEN
      Message = TRIM(Message)//'; Error destroying RadDiag_Data'
    END IF
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )

  END FUNCTION Read_RadDiag_Data


!------------------------------------------------------------------------------------
!
! Function to write a radiance diagnostic file header
!
! CALLING SEQUENCE:
!   Error_Status = Write_RadDiag_Hdr( Filename,                 &  ! Input
!                                     FileID,                   &  ! Input
!                                     RadDiag_Hdr,              &  ! Input
!                                     RCS_Id = RCS_Id,          &  ! Revision control
!                                     Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!   Filename:           Name of the radiance diagnostic file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!   FileID:             File unit number of the radiance diagnostic file to write.
!                       Returned from call to Open_RadDiag() function.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!   RadDiag_Hdr:        RadDiag header structure to write to file.
!                       UNITS:      N/A
!                       TYPE:       TYPE(RadDiag_Hdr_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!   Message_Log:        Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!   RCS_Id:             Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!   Error_Status:       The return value is an integer defining the error status.
!                       The error codes are defined in the ERROR_HANDLER module.
!                       If == SUCCESS the file read was successful.
!                          == FAILURE an error occurred reading the file.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!------------------------------------------------------------------------------------

  FUNCTION Write_RadDiag_Hdr( Filename,     &  ! Input
                              FileID,       &  ! Input
                              RadDiag_Hdr,  &  ! Input
                              RCS_Id,       &  ! Revision control
                              Message_Log ) &  ! Error messaging
                            RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER,                INTENT(IN)  :: FileID
    TYPE(RadDiag_Hdr_type), INTENT(IN)  :: RadDiag_Hdr
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_RadDiag_Hdr'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: IO_Status
    INTEGER :: i

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Write the fixed part of the header
    WRITE( FileID, IOSTAT=IO_Status ) RadDiag_Hdr%Scalar%isis   , &
                                      RadDiag_Hdr%Scalar%id     , &
                                      RadDiag_Hdr%Scalar%obstype, &
                                      RadDiag_Hdr%Scalar%jiter  , &
                                      RadDiag_Hdr%Scalar%nchan  , &
                                      RadDiag_Hdr%Scalar%npred  , &
                                      RadDiag_Hdr%Scalar%idate  , &
                                      RadDiag_Hdr%Scalar%ireal  , &
                                      RadDiag_Hdr%Scalar%ipchan , &
                                      RadDiag_Hdr%Scalar%iextra , &
                                      RadDiag_Hdr%Scalar%jextra 
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing RadDiag header fixed portion to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF

    ! Write the channel portion of the header
    DO i = 1, RadDiag_Hdr%nChannels
      WRITE( FileID, IOSTAT=IO_Status ) RadDiag_Hdr%Channel(i)%freq    , &
                                        RadDiag_Hdr%Channel(i)%polar   , &
                                        RadDiag_Hdr%Channel(i)%wave    , &
                                        RadDiag_Hdr%Channel(i)%varch   , &
                                        RadDiag_Hdr%Channel(i)%tlapmean, &
                                        RadDiag_Hdr%Channel(i)%iuse    , &
                                        RadDiag_Hdr%Channel(i)%nuchan  , &
                                        RadDiag_Hdr%Channel(i)%iochan  
      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error writing RadDiag header channel index ", i5, &
                          &" data to ", a, ". IOSTAT = ", i5 )' ) &
                        i, TRIM( Filename ), IO_Status
        GOTO 1000
      END IF
    END DO

    RETURN


    ! ----------------------
    ! Process FAILURE errors
    ! ----------------------
    1000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )
    CLOSE( FileID )
    
  END FUNCTION Write_RadDiag_Hdr



!------------------------------------------------------------------------------------
!
! Function to write radiance diagnostic data to file
!
! CALLING SEQUENCE:
!   Error_Status = Write_RadDiag_Data( Filename,                 &  ! Input
!                                      FileID,                   &  ! Input
!                                      RadDiag_Hdr,              &  ! Input
!                                      RadDiag_Data,             &  ! Input
!                                      RCS_Id = RCS_Id,          &  ! Revision control
!                                      Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!   Filename:           Name of the radiance diagnostic file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!   FileID:             File unit number of the radiance diagnostic file to write.
!                       Returned from call to Open_RadDiag() function.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!   RadDiag_Hdr:        RadDiag header structure for the file.
!                       UNITS:      N/A
!                       TYPE:       TYPE(RadDiag_Hdr_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!   RadDiag_Data:       RadDiag data structure to write to file.
!                       UNITS:      N/A
!                       TYPE:       TYPE(RadDiag_Data_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!   Message_Log:        Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!   RCS_Id:             Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!   Error_Status:       The return value is an integer defining the error status.
!                       The error codes are defined in the ERROR_HANDLER module.
!                       If == SUCCESS the file read was successful.
!                          == FAILURE an error occurred reading the file.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!------------------------------------------------------------------------------------

  FUNCTION Write_RadDiag_Data( Filename,     &
                               FileID,       &  ! Input
                               RadDiag_Hdr,  &  ! Input
                               RadDiag_Data, &  ! Output
                               RCS_Id,       &  ! Revision control
                               Message_Log ) &  ! Error messaging
                             RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),            INTENT(IN)  :: Filename
    INTEGER,                 INTENT(IN)  :: FileID
    TYPE(RadDiag_Hdr_type),  INTENT(IN)  :: RadDiag_Hdr
    TYPE(RadDiag_Data_type), INTENT(IN)  :: RadDiag_Data
    CHARACTER(*), OPTIONAL,  INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL,  INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_RadDiag_Data'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: IO_Status
    INTEGER :: i
    REAL(sp), DIMENSION(RadDiag_Hdr%Scalar%iextra) :: Extra

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Write all the data
    Extra = 0.0_sp
    WRITE( FileID, IOSTAT=IO_Status ) &
      RadDiag_Data%Scalar%lat       , &
      RadDiag_Data%Scalar%lon       , &
      RadDiag_Data%Scalar%zsges     , &
      RadDiag_Data%Scalar%obstime   , &
      RadDiag_Data%Scalar%senscn_pos, &
      RadDiag_Data%Scalar%satzen_ang, &
      RadDiag_Data%Scalar%satazm_ang, &
      RadDiag_Data%Scalar%solzen_ang, &
      RadDiag_Data%Scalar%solazm_ang, &
      RadDiag_Data%Scalar%sungln_ang, &
      RadDiag_Data%Scalar%water_frac, &
      RadDiag_Data%Scalar%land_frac , &
      RadDiag_Data%Scalar%ice_frac  , &
      RadDiag_Data%Scalar%snow_frac , &
      RadDiag_Data%Scalar%water_temp, &
      RadDiag_Data%Scalar%land_temp , &
      RadDiag_Data%Scalar%ice_temp  , &
      RadDiag_Data%Scalar%snow_temp , &
      RadDiag_Data%Scalar%soil_temp , &
      RadDiag_Data%Scalar%soil_mois , &
      RadDiag_Data%Scalar%land_type , &
      RadDiag_Data%Scalar%veg_frac  , &
      RadDiag_Data%Scalar%snow_depth, &
      RadDiag_Data%Scalar%sfc_wndspd, &
      RadDiag_Data%Scalar%qcdiag1   , &
      RadDiag_Data%Scalar%qcdiag2   , ( RadDiag_Data%Channel(i)%tbobs , &
                                        RadDiag_Data%Channel(i)%omgbc , &
                                        RadDiag_Data%Channel(i)%omgnbc, &
                                        RadDiag_Data%Channel(i)%errinv, & 
                                        RadDiag_Data%Channel(i)%qcmark, & 
                                        RadDiag_Data%Channel(i)%emiss , &  
                                        RadDiag_Data%Channel(i)%tlap  , & 
                                        RadDiag_Data%Channel(i)%bifix , & 
                                        RadDiag_Data%Channel(i)%bilap , &
                                        RadDiag_Data%Channel(i)%bilap2, &
                                        RadDiag_Data%Channel(i)%bicons, & 
                                        RadDiag_Data%Channel(i)%biang , & 
                                        RadDiag_Data%Channel(i)%biclw , &  
                                        i=1,RadDiag_Data%nChannels      ), Extra
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing RadDiag Data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF

    RETURN


    ! ----------------------
    ! Process FAILURE errors
    ! ----------------------
    1000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )
    CLOSE( FileID )

  END FUNCTION Write_RadDiag_Data

END MODULE RadDiag_IO
