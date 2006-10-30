!
! AerosolCoeff_Binary_IO
!
! Module containing routines to read and write Binary format
! AerosolCoeff files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 04-Feb-2005
!                       paul.vandelst@ssec.wisc.edu
!

MODULE AerosolCoeff_Binary_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,          ONLY: Long
  USE File_Utility,        ONLY: File_Exists
  USE Message_Handler,     ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                 Display_Message
  USE Binary_File_Utility, ONLY: Open_Binary_File
  USE AerosolCoeff_Define, ONLY: AerosolCoeff_type, &
                                 N_AEROSOLCOEFF_ITEMS, &
                                 AEROSOLCOEFF_DATA_TYPE, &
                                 AEROSOLCOEFF_DATA_NAME, &
                                 Associated_AerosolCoeff, &
                                 Allocate_AerosolCoeff, &
                                 Check_AerosolCoeff_Release, &
                                 Info_AerosolCoeff
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Module procedures
  PUBLIC :: Inquire_AerosolCoeff_Binary
  PUBLIC :: Read_AerosolCoeff_Binary
  PUBLIC :: Write_AerosolCoeff_Binary


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Module RCS Id string
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
    '$Id: AerosolCoeff_Binary_IO.f90,v 1.4 2006/05/26 19:48:25 wd20pd Exp $'


CONTAINS


!------------------------------------------------------------------------------
!
! NAME:
!       Inquire_AerosolCoeff_Binary
!
! PURPOSE:
!       Function to inquire a Binary format AerosolCoeff file.
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_AerosolCoeff_Binary( Filename,               &  ! Input
!                                                   n_Channels=n_Channels,  &  ! Optional Output
!                                                   Release=Release,        &  ! Optional Output
!                                                   Version=Version,        &  ! Optional Output
!                                                   RCS_Id=RCS_Id,          &  ! Revision control
!                                                   Message_Log=Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:           Character string specifying the name of a
!                           AerosolCoeff format data file.
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
!       n_Channels:         The number of channels dimension for the data 
!                           in the coefficient file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:            The coefficient file release number.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:            The coefficient file version number.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
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
!                           If == SUCCESS the Binary file inquiry was successful
!                              == FAILURE an error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION Inquire_AerosolCoeff_Binary( Filename,     &  ! Input
                                        n_Channels,   &  ! Optional Output
                                        Release,      &  ! Optional Output
                                        Version,      &  ! Optional Output
                                        RCS_Id,       &  ! Revision control
                                        Message_Log ) &  ! Error messaging
                                      RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Channels
    INTEGER,      OPTIONAL, INTENT(OUT) :: Release
    INTEGER,      OPTIONAL, INTENT(OUT) :: Version
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Inquire_AerosolCoeff_Binary'
    ! Function variables
    CHARACTER(256) :: Message
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER(Long) :: File_Release
    INTEGER(Long) :: File_Version
    INTEGER(Long) :: File_n_Channels
 

    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! -------------
    ! Open the file
    ! -------------
    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID, &
                                     Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening AerosolCoeff file '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! --------------------
    ! Read the file header
    ! --------------------
    ! Read the Release/Version information
    READ( FileID, IOSTAT=IO_Status ) File_Release, &
                                       File_Version
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading AerosolCoeff file Release/Version values from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log=Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

    ! Read the dimensions
    READ( FileID, IOSTAT=IO_Status ) File_n_Channels
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading n_Channels dimension from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log=Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ---------------------------
    ! Assign the return arguments
    ! ---------------------------
    ! Dimensions
    IF ( PRESENT( n_Channels ) ) THEN
      n_Channels = File_n_Channels
    END IF
    ! Ancillary info
    IF ( PRESENT( Release ) ) THEN
      Release = File_Release
    END IF
    IF ( PRESENT( Version ) ) THEN
      Version = File_Version
    END IF


    ! --------------
    ! Close the file
    ! --------------
    CLOSE( FileID, STATUS='KEEP', &
                   IOSTAT=IO_Status )
    IF ( IO_Status /= 0 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION Inquire_AerosolCoeff_Binary


!------------------------------------------------------------------------------
!
! NAME:
!       Read_AerosolCoeff_Binary
!
! PURPOSE:
!       Function to read Binary format AerosolCoeff files.
!
! CALLING SEQUENCE:
!       Error_Status = Read_AerosolCoeff_Binary( Filename,                            &  ! Input
!                                                AerosolCoeff,                        &  ! Output
!                                                Quiet=Quiet,                         &  ! Optional input
!                                                Process_ID=Process_ID,               &  ! Optional input
!                                                Output_Process_ID=Output_Process_ID, &  ! Optional input
!                                                RCS_Id=RCS_Id,                       &  ! Revision control
!                                                Message_Log= Message_Log             )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:           Character string specifying the name of the
!                           input binary format AerosolCoeff data file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:              Set this argument to suppress Information messages
!                           being printed to standard output (or the Message
!                           log file if the Message_Log optional argument is
!                           used.) By default, Information messages are printed.
!                           If QUIET = 0, Information messages are OUTPUT.
!                              QUIET = 1, Information messages are SUPPRESSED.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Process_ID:         Set this argument to the MPI process ID that this
!                           function call is running under. This value is used
!                           solely for controlling INFORMATIOn Message output.
!                           If MPI is not being used, ignore this argument.
!                           This argument is ignored if the Quiet argument is set.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Output_Process_ID:  Set this argument to the MPI process ID in which
!                           all Information messages are to be output. If
!                           the passed Process_ID value agrees with this value
!                           the Information messages are output. 
!                           This argument is ignored if the Quiet argument
!                           is set.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
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
! OUTPUT ARGUMENTS:
!       AerosolCoeff:       Structure to contain the aerosol coefficient
!                           data read from the file.
!                           UNITS:      N/A
!                           TYPE:       AerosolCoeff_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
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
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the Message_Handler module.
!                           If == SUCCESS the Binary file read was successful
!                              == FAILURE a read error occurred.
!                              == WARNING an error occurred closing the Binary file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output AerosolCoeff argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined on
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Read_AerosolCoeff_Binary( Filename,          &  ! Input
                                     AerosolCoeff,      &  ! Output
                                     Quiet,             &  ! Optional input
                                     Process_ID,        &  ! Optional input
                                     Output_Process_ID, &  ! Optional input
                                     RCS_Id,            &  ! Revision control
                                     Message_Log )      &  ! Error messaging
                                   RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),            INTENT(IN)     :: Filename
    TYPE(AerosolCoeff_type), INTENT(IN OUT) :: AerosolCoeff
    INTEGER,       OPTIONAL, INTENT(IN)     :: Quiet
    INTEGER,       OPTIONAL, INTENT(IN)     :: Process_ID
    INTEGER,       OPTIONAL, INTENT(IN)     :: Output_Process_ID
    CHARACTER(*),  OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_AerosolCoeff_Binary'
    ! Function variables
    CHARACTER(256) :: Message
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    CHARACTER(128) :: Process_ID_Tag
    INTEGER :: FileID
    INTEGER(Long) :: n_Channels
    INTEGER(Long) :: n_Items, n
    INTEGER(Long), DIMENSION(N_AEROSOLCOEFF_ITEMS) :: Data_Type
 

    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational Messages....
    Noisy = .TRUE.
    ! ....unless....
    IF ( PRESENT(Quiet) ) THEN
      ! ....the QUIET keyword is set.
      IF ( Quiet == 1 ) Noisy = .FALSE.
    ELSE
      ! ....the Process_ID is not selected for output
      IF ( PRESENT( Process_ID ) .AND. PRESENT( Output_Process_ID ) ) THEN
        IF ( Process_ID /= Output_Process_ID ) Noisy = .FALSE.
      END IF
    END IF


    ! -----------------------------------
    ! Create a process ID Message tag for
    ! WARNING and FAILURE Messages
    ! -----------------------------------
    IF ( PRESENT( Process_ID ) ) THEN
      WRITE( Process_ID_Tag, '( ";  MPI Prcess ID: ", i5 )' ) Process_ID
    ELSE
      Process_ID_Tag = ' '
    END IF


    ! -------------
    ! Open the file
    ! -------------
    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID,   &
                                     Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//TRIM( Filename )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! --------------------
    ! Read the file header
    ! --------------------
    ! Read the release/version information
    READ( FileID, IOSTAT=IO_Status ) AerosolCoeff%Release, &
                                     AerosolCoeff%Version
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading AerosolCoeff file Release/Version values from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log=Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

    ! Check the release
    Error_Status = Check_AerosolCoeff_Release( AerosolCoeff, &
                                               Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'AerosolCoeff Release check failed for '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log=Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

    ! Read the dimensions
    READ( FileID, IOSTAT=IO_Status ) n_Channels
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading n_Channels dimension from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log=Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -----------------------------        
    ! Read the number of data items
    ! -----------------------------
    READ( FileID, IOSTAT=IO_Status ) n_Items
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading the number of data items from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log=Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

    ! Check that the number of data items is correct
    IF ( n_Items /= N_AEROSOLCOEFF_ITEMS ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Number of data items in ", a, " (", i2, &
                        &") is inconsistent with definition (", i2, ")." )' ) &
                      TRIM( Filename ), n_Items, N_AEROSOLCOEFF_ITEMS
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log=Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -------------------
    ! Read the data types
    ! -------------------
    READ( FileID, IOSTAT=IO_Status ) Data_Type
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading the data items types from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log=Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

    ! Check that the data items types are correct
    DO n = 1, n_Items
      IF ( Data_Type( n ) /= AEROSOLCOEFF_DATA_TYPE( n ) ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Invalid type for data item #", i2, &
                          &", ", a, ", in ", a )' ) &
                        n, TRIM( AEROSOLCOEFF_DATA_NAME( n ) ), TRIM( Filename )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message )//TRIM( Process_ID_Tag ), &
                              Error_Status, &
                              Message_Log=Message_Log )
        CLOSE( FileID )
        RETURN
      END IF
    END DO



    ! -----------------------------------
    ! Allocate the AerosolCoeff structure
    ! -----------------------------------
    Error_Status = Allocate_AerosolCoeff( n_Channels, &
                                          AerosolCoeff, &
                                          Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error occurred allocating AerosolCoeff structure.'//&
                            TRIM( Process_ID_Tag ), &
                            Error_Status,    &
                            Message_Log=Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -------------------
    ! Read the data items
    ! -------------------
    ! Absorption coefficient array
    READ( FileID, IOSTAT=IO_Status ) AerosolCoeff%Absorption
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Absorption from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log=Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

    ! Scattering coefficient array
    READ( FileID, IOSTAT=IO_Status ) AerosolCoeff%Scattering
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Scattering from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log=Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! --------------
    ! Close the file
    ! --------------
    CLOSE( FileID, STATUS='KEEP', &
                   IOSTAT=IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            WARNING, &
                            Message_Log=Message_Log )
    END IF


    ! ----------------------
    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_AerosolCoeff( AerosolCoeff, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log=Message_Log )
    END IF

  END FUNCTION Read_AerosolCoeff_Binary




!------------------------------------------------------------------------------
!
! NAME:
!       Write_AerosolCoeff_Binary
!
! PURPOSE:
!       Function to write Binary format AerosolCoeff files.
!
! CALLING SEQUENCE:
!       Error_Status = Write_AerosolCoeff_Binary( Filename,               &  ! Input
!                                                 AerosolCoeff,           &  ! Input
!                                                 Quiet=Quiet,            &  ! Optional input
!                                                 RCS_Id=RCS_Id,          &  ! Revision control
!                                                 Message_Log=Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an output
!                     AerosolCoeff format data file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       AerosolCoeff: Structure containing the aerosol coefficient data.
!                     UNITS:      N/A
!                     TYPE:       AerosolCoeff_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:        Set this keyword to suppress information messages being
!                     printed to standard output (or the Message log file if 
!                     the Message_Log optional argument is used.) By default,
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
!                     If == SUCCESS the Binary file write was successful
!                        == FAILURE a write error occurred.
!                        == WARNING an error occurred closing the Binary file.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       - If the output file already exists, it is overwritten.
!       - If an error occurs *during* the write phase, the output file is deleted
!         before returning to the calling routine.
!
!------------------------------------------------------------------------------

  FUNCTION Write_AerosolCoeff_Binary( Filename,     &  ! Input
                                      AerosolCoeff, &  ! Input
                                      Quiet,        &  ! Optional input
                                      RCS_Id,       &  ! Revision control
                                      Message_Log ) &  ! Error messaging
                                    RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),            INTENT(IN) :: Filename
    TYPE(AerosolCoeff_type), INTENT(IN) :: AerosolCoeff
    INTEGER,         OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_AerosolCoeff_Binary'
    CHARACTER(*), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'
    ! Function variables
    CHARACTER(256) :: Message
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: FileID
 

    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational Messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF

    ! Check structure pointer association status
    IF ( .NOT. Associated_AerosolCoeff( AerosolCoeff ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT AerosolCoeff pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the AerosolCoeff structure Release
    Error_Status = Check_AerosolCoeff_Release( AerosolCoeff, &
                                               Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'AerosolCoeff Release check failed.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the AerosolCoeff structure dimensions
    IF ( AerosolCoeff%n_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Channel dimension of AerosolCoeff structure is < or = 0.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! -------------
    ! Open the file
    ! -------------
    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID, &
                                     For_Output=1, &
                                     Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! ---------------------
    ! Write the file header
    ! ---------------------
    ! Write the release/version information
    WRITE( FileID, IOSTAT=IO_Status ) AerosolCoeff%Release, &
                                      AerosolCoeff%Version
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing AerosolCoeff file Release/Version values to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log=Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF

    ! Write the dimensions
    WRITE( FileID, IOSTAT=IO_Status ) AerosolCoeff%n_Channels
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing n_Channels dimension to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log=Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF

    ! Write the number of data items
    WRITE( FileID, IOSTAT=IO_Status ) N_AEROSOLCOEFF_ITEMS
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing the number of data items to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log=Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF

    ! Write the data item types
    WRITE( FileID, IOSTAT=IO_Status ) AEROSOLCOEFF_DATA_TYPE
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing the data items types to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log=Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! --------------------
    ! Write the data items
    ! --------------------
    ! Absorption array
    WRITE( FileID, IOSTAT=IO_Status ) AerosolCoeff%Absorption
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Absorption to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log=Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF

    ! Scattering array
    WRITE( FileID, IOSTAT=IO_Status ) AerosolCoeff%Scattering
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Scattering to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log=Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! --------------
    ! Close the file
    ! --------------
    CLOSE( FileID, STATUS='KEEP',   &
                   IOSTAT=IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            WARNING, &
                            Message_Log=Message_Log )
    END IF


    ! ----------------------
    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_AerosolCoeff( AerosolCoeff, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log=Message_Log )
    END IF

  END FUNCTION Write_AerosolCoeff_Binary

END MODULE AerosolCoeff_Binary_IO
