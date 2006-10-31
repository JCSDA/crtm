!
! TauCoeff_Binary_IO
!
! Module containing routines to read and write Binary format
! TauCoeff files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 02-Jan-2003
!                       paul.vandelst@ssec.wisc.edu
!

MODULE TauCoeff_Binary_IO

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds,          ONLY: Long
  USE Message_Handler,     ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, Display_Message
  USE Binary_File_Utility, ONLY: Open_Binary_File
  USE TauCoeff_Define,     ONLY: TauCoeff_Type, &
                                 N_TAUCOEFF_ITEMS, &
                                 TAUCOEFF_DATA_TYPE, &
                                 TAUCOEFF_DATA_NAME, &
                                 Associated_TauCoeff, &
                                 Allocate_TauCoeff, &
                                 Destroy_TauCoeff, &
                                 Check_TauCoeff_Release, &
                                 Count_TauCoeff_Sensors, &
                                 Info_TauCoeff
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Inquire_TauCoeff_Binary
  PUBLIC :: Read_TauCoeff_Binary
  PUBLIC :: Write_TauCoeff_Binary


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
    '$Id: TauCoeff_Binary_IO.f90,v 5.10 2006/06/23 23:16:43 wd20pd Exp $'


CONTAINS


!------------------------------------------------------------------------------
!
! NAME:
!       Inquire_TauCoeff_Binary
!
! PURPOSE:
!       Function to inquire a Binary format TauCoeff file.
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_TauCoeff_Binary( Filename,                    &  ! Input
!                                               n_Orders     = n_Orders,     &  ! Optional output
!                                               n_Predictors = n_Predictors, &  ! Optional output
!                                               n_Absorbers  = n_Absorbers,  &  ! Optional output
!                                               n_Channels   = n_Channels,   &  ! Optional output
!                                               Release      = Release,      &  ! Optional Output
!                                               Version      = Version,      &  ! Optional Output
!                                               RCS_Id       = RCS_Id,       &  ! Revision control
!                                               Message_Log  = Message_Log   )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:           Character string specifying the name of the binary
!                           TauCoeff data file to inquire.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:        Character string specifying a filename in which any
!                           Messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output Messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Orders:           The maximum polynomial order used to reconstruct the
!                           transmittance coefficients.
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
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the Binary file inquiry was successful
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION Inquire_TauCoeff_Binary( Filename,     &  ! Input
                                    n_Orders,     &  ! Optional output
                                    n_Predictors, &  ! Optional output
                                    n_Absorbers,  &  ! Optional output
                                    n_Channels,   &  ! Optional output
                                    Release,      &  ! Optional Output
                                    Version,      &  ! Optional Output
                                    RCS_Id,       &  ! Revision control
                                    Message_Log ) &  ! Error messaging
                                  RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Orders
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Predictors
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Absorbers
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Channels
    INTEGER,      OPTIONAL, INTENT(OUT) :: Release
    INTEGER,      OPTIONAL, INTENT(OUT) :: Version
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Inquire_TauCoeff_Binary'
    ! Function variables
    CHARACTER(256)  :: Message
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER(Long) :: File_Release
    INTEGER(Long) :: File_Version
    INTEGER(Long) :: File_n_Orders
    INTEGER(Long) :: File_n_Predictors
    INTEGER(Long) :: File_n_Absorbers
    INTEGER(Long) :: File_n_Channels
 
    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Open the Binary format TauCoeff file
    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID,   &
                                     Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening TauCoeff file '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Read the Release/Version information
    READ( FileID, IOSTAT = IO_Status ) File_Release, &
                                       File_Version
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading TauCoeff file Release/Version values from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

    ! Read the dimensions
    READ( FileID, IOSTAT = IO_Status ) File_n_Orders , &
                                       File_n_Predictors, &
                                       File_n_Absorbers, &
                                       File_n_Channels
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading data dimensions from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

    ! Close the file
    CLOSE( FileID, STATUS = 'KEEP', &
                   IOSTAT = IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    ! Assign the return arguments
    IF ( PRESENT( n_Orders     ) ) n_Orders     = File_n_Orders
    IF ( PRESENT( n_Predictors ) ) n_Predictors = File_n_Predictors
    IF ( PRESENT( n_Absorbers  ) ) n_Absorbers  = File_n_Absorbers
    IF ( PRESENT( n_Channels   ) ) n_Channels   = File_n_Channels
    IF ( PRESENT( Release      ) ) Release      = File_Release
    IF ( PRESENT( Version      ) ) Version      = File_Version

  END FUNCTION Inquire_TauCoeff_Binary


!------------------------------------------------------------------------------
!
! NAME:
!       Read_TauCoeff_Binary
!
! PURPOSE:
!       Function to read Binary format TauCoeff files.
!
! CALLING SEQUENCE:
!       Error_Status = Read_TauCoeff_Binary( Filename,                              &  ! Input
!                                            TauCoeff,                              &  ! Output
!                                            Quiet             = Quiet,             &  ! Optional input
!                                            Process_ID        = Process_ID,        &  ! Optional input
!                                            Output_Process_ID = Output_Process_ID, &  ! Optional input
!                                            RCS_Id            = RCS_Id,            &  ! Revision control
!                                            Message_Log       = Message_Log        )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:           Character string specifying the name of the binary
!                           format TauCoeff data file to read.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:              Set this argument to suppress INFORMATION messages
!                           being printed to standard output (or the message
!                           log file if the Message_Log optional argument is
!                           used.) By default, INFORMATION messages are printed.
!                           If QUIET = 0, INFORMATION messages are OUTPUT.
!                              QUIET = 1, INFORMATION messages are SUPPRESSED.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Process_ID:         Set this argument to the MPI process ID that this
!                           function call is running under. This value is used
!                           solely for controlling INFORMATIOn message output.
!                           If MPI is not being used, ignore this argument.
!                           This argument is ignored if the Quiet argument is set.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Output_Process_ID:  Set this argument to the MPI process ID in which
!                           all INFORMATION messages are to be output. If
!                           the passed Process_ID value agrees with this value
!                           the INFORMATION messages are output. 
!                           This argument is ignored if the Quiet argument
!                           is set.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Message_Log:        Character string specifying a filename in which any
!                           Messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output Messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       TauCoeff:           Structure containing the transmittance coefficient data
!                           read from the file.
!                           UNITS:      N/A
!                           TYPE:       TauCoeff_type
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
!                              == FAILURE an unrecoverable read error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
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
!------------------------------------------------------------------------------

  FUNCTION Read_TauCoeff_Binary( Filename,          &  ! Input
                                 TauCoeff,          &  ! Output
                                 Quiet,             &  ! Optional input
                                 Process_ID,        &  ! Optional input
                                 Output_Process_ID, &  ! Optional input
                                 RCS_Id,            &  ! Revision control
                                 Message_Log )      &  ! Error messaging
                               RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)     :: Filename
    TYPE(TauCoeff_type),    INTENT(IN OUT) :: TauCoeff
    INTEGER,      OPTIONAL, INTENT(IN)     :: Quiet
    INTEGER,      OPTIONAL, INTENT(IN)     :: Process_ID
    INTEGER,      OPTIONAL, INTENT(IN)     :: Output_Process_ID
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_TauCoeff_Binary'
    ! Function variables
    CHARACTER(1000) :: Message
    CHARACTER(128) :: Process_ID_Tag
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: Destroy_Status
    INTEGER :: FileID
    INTEGER(Long) :: n_Orders
    INTEGER(Long) :: n_Predictors
    INTEGER(Long) :: n_Absorbers
    INTEGER(Long) :: n_Channels
    INTEGER(Long) :: StrLen
    INTEGER(Long) :: n_items, n
    INTEGER(Long), DIMENSION(N_TAUCOEFF_ITEMS) :: Data_Type
 
    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless....
    IF ( PRESENT( Quiet ) ) THEN
      ! ....the QUIET keyword is set.
      IF ( Quiet == 1 ) Noisy = .FALSE.
    ELSE
      ! ....the Process_ID is not selected for output
      IF ( PRESENT( Process_ID ) .AND. PRESENT( Output_Process_ID ) ) THEN
        IF ( Process_ID /= Output_Process_ID ) Noisy = .FALSE.
      END IF
    END IF

    ! Create a process ID message tag for
    ! WARNING and FAILURE messages
    IF ( PRESENT( Process_ID ) ) THEN
      WRITE( Process_ID_Tag, '( ";  MPI Prcess ID: ", i5 )' ) Process_ID
    ELSE
      Process_ID_Tag = ' '
    END IF

    ! Open the TauCoeff file
    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID,   &
                                     Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//TRIM( Filename )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Read the Release/Version information
    READ( FileID, IOSTAT = IO_Status ) TauCoeff%Release, &
                                       TauCoeff%Version
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading TauCoeff file Release/Version values from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 2000
    END IF

    ! Check the release
    Error_Status = Check_TauCoeff_Release( TauCoeff, &
                                           Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'TauCoeff Release check failed for '//TRIM( Filename )
      GOTO 2000
    END IF

    ! Read the dimensions
    READ( FileID, IOSTAT = IO_Status ) n_Orders, &
                                       n_Predictors, &
                                       n_Absorbers, &
                                       n_Channels
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading data dimensions from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 2000
    END IF

    ! Read the sensor descriptor string length
    READ( FileID, IOSTAT = IO_Status ) StrLen
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading Sensor_Descriptor StrLen from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 2000
    END IF

    ! Check that the string length is consistent
    IF ( StrLen /= TauCoeff%StrLen ) THEN
      WRITE( Message, '( "Sensor descriptor string length ", a, " (", i2, &
                        &") is inconsistent with definition (", i2, ")." )' ) &
                      TRIM( Filename ), &
                      StrLen, &
                      TauCoeff%StrLen
      GOTO 2000
    END IF

    ! Read the number of data items
    READ( FileID, IOSTAT = IO_Status ) n_Items
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading the number of data items from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 2000
    END IF

    ! Check that the number of data items is correct
    IF ( n_Items /= N_TAUCOEFF_ITEMS ) THEN
      WRITE( Message, '( "Number of data items in ", a, " (", i2, &
                        &") is inconsistent with definition (", i2, ")." )' ) &
                      TRIM( Filename ), n_Items, N_TAUCOEFF_ITEMS
      GOTO 2000
    END IF

    ! Read the data types
    READ( FileID, IOSTAT = IO_Status ) Data_Type
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading the data items types from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 2000
    END IF

    ! Check that the data items types are correct
    DO n = 1, n_Items
      IF ( Data_Type(n) /= TAUCOEFF_DATA_TYPE(n) ) THEN
        WRITE( Message, '( "Invalid type for data item #", i2, &
                          &", ", a, ", in ", a )' ) &
                        n, TRIM( TAUCOEFF_DATA_NAME(n) ), TRIM( Filename )
        GOTO 2000
      END IF
    END DO

    ! Allocate the TauCoeff structure for reading
    Error_Status = Allocate_TauCoeff( n_Orders, &
                                      n_Predictors, &
                                      n_Absorbers, &
                                      n_Channels, &
                                      TauCoeff, &
                                      Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error occurred allocating TauCoeff structure.'//TRIM(Process_ID_Tag)
      GOTO 2000
    END IF

    ! Read the sensor ID data
    READ( FileID, IOSTAT = IO_Status ) TauCoeff%Sensor_Descriptor, &
                                       TauCoeff%NCEP_Sensor_ID, &
                                       TauCoeff%WMO_Satellite_ID, &
                                       TauCoeff%WMO_Sensor_ID, &
                                       TauCoeff%Sensor_Channel
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading sensor ID data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF

    ! Read the absorber ID data
    READ( FileID, IOSTAT = IO_Status ) TauCoeff%Absorber_ID
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading absorber ID data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF

    ! Read the alpha data
    READ( FileID, IOSTAT = IO_Status ) TauCoeff%Alpha, &
                                       TauCoeff%Alpha_C1, &
                                       TauCoeff%Alpha_C2
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading Alpha data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF

    ! Read the order indices
    READ( FileID, IOSTAT = IO_Status ) TauCoeff%Order_Index
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading order indices from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF

    ! Read the predictor indices
    READ( FileID, IOSTAT = IO_Status ) TauCoeff%Predictor_Index
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading predictor indices from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF

    ! Read the gas absorption coefficients
    READ( FileID, IOSTAT = IO_Status ) TauCoeff%C
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading gas absorption coefficients from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF

    ! Close the file
    CLOSE( FileID, STATUS = 'KEEP',   &
                   IOSTAT = IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    ! Count the sensors
    CALL Count_TauCoeff_Sensors(TauCoeff)

    ! Output an info message
    IF ( Noisy ) THEN
      CALL Info_TauCoeff( TauCoeff, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( Filename )//'; '//TRIM( Message ), &
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

    2000 CONTINUE
    CLOSE(FileID)
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Read_TauCoeff_Binary


!------------------------------------------------------------------------------
!
! NAME:
!       Write_TauCoeff_Binary
!
! PURPOSE:
!       Function to write Binary format TauCoeff files.
!
! CALLING SEQUENCE:
!       Error_Status = Write_TauCoeff_Binary( Filename,                 &   ! Input
!                                             TauCoeff,                 &   ! Input
!                                             Quiet       = Quiet,      &   ! Optional input
!                                             RCS_Id      = RCS_Id,     &   ! Revision control
!                                             Message_Log = Message_Log )   ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an output
!                     TauCoeff format data file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       TauCoeff:     Structure containing the gas absorption coefficient data.
!                     UNITS:      N/A
!                     TYPE:       TauCoeff_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:        Set this keyword to suppress information Messages being
!                     printed to standard output (or the Message log file if 
!                     the Message_Log optional argument is used.) By default,
!                     information Messages are printed.
!                     If QUIET = 0, information Messages are OUTPUT.
!                        QUIET = 1, information Messages are SUPPRESSED.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Message_Log:  Character string specifying a filename in which any
!                     Messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output Messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(IN)
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
!                        == FAILURE - the input TauCoeff structure contains
!                                     unassociated pointer members, or
!                                   - a unrecoverable write error occurred.
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

  FUNCTION Write_TauCoeff_Binary( Filename,     &  ! Input
                                  TauCoeff,     &  ! Input
                                  Quiet,        &  ! Optional input
                                  RCS_Id,       &  ! Revision control
                                  Message_Log ) &  ! Error messaging
                                RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    TYPE(TauCoeff_type),    INTENT(IN)  :: TauCoeff
    INTEGER,      OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_TauCoeff_Binary'
    CHARACTER(*), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'
    ! Function variables
    CHARACTER(256)  :: Message
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: FileID

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == 1 ) Noisy = .FALSE.
    END IF

    ! Check structure
    IF ( .NOT. Associated_TauCoeff( TauCoeff ) ) THEN
      Message = 'Some or all INPUT TauCoeff pointer members are NOT associated.'
      GOTO 2000
    END IF

    ! Check the TauCoeff structure Release
    Error_Status = Check_TauCoeff_Release( TauCoeff, &
                                           Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'TauCoeff Release check failed.'
      GOTO 2000
    END IF

    ! Check the TauCoeff structure dimensions
    IF ( TauCoeff%n_Orders     < 1 .OR. &
         TauCoeff%n_Predictors < 1 .OR. &
         TauCoeff%n_Absorbers  < 1 .OR. &
         TauCoeff%n_Channels   < 1      ) THEN
      Message = 'One or more dimensions of TauCoeff structure are < or = 0.'
      GOTO 2000
    END IF

    ! Open the TauCoeff data file
    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID,   &
                                     For_Output  = 1, &
                                     Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening '//TRIM( Filename )
      GOTO 2000
    END IF

    ! Write the Release/Version information
    WRITE( FileID, IOSTAT = IO_Status ) TauCoeff%Release, &
                                        TauCoeff%Version 
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing TauCoeff file Release/Version values to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF

    ! Write the dimensions
    WRITE( FileID, IOSTAT = IO_Status ) TauCoeff%n_Orders, &
                                        TauCoeff%n_Predictors, &
                                        TauCoeff%n_Absorbers, &
                                        TauCoeff%n_Channels
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing data dimensions to ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF

    ! Write the sensor descriptor string length
    WRITE( FileID, IOSTAT = IO_Status ) TauCoeff%StrLen
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing Sensor_Descriptor StrLen to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF

    ! Write the number of data items
    WRITE( FileID, IOSTAT = IO_Status ) N_TAUCOEFF_ITEMS
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing the number of data items to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF

    ! Write the data item types
    WRITE( FileID, IOSTAT = IO_Status ) TAUCOEFF_DATA_TYPE
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing the data item types to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF

    ! Write sensor ID data
    WRITE( FileID, IOSTAT = IO_Status ) TauCoeff%Sensor_Descriptor, &
                                        TauCoeff%NCEP_Sensor_ID, &
                                        TauCoeff%WMO_Satellite_ID, &
                                        TauCoeff%WMO_Sensor_ID, &
                                        TauCoeff%Sensor_Channel
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing sensor ID data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF

    ! Write absorber ID data
    WRITE( FileID, IOSTAT = IO_Status ) TauCoeff%Absorber_ID
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing absorber ID data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF

    ! Write the alpha data
    WRITE( FileID, IOSTAT = IO_Status ) TauCoeff%Alpha, &
                                        TauCoeff%Alpha_C1, &
                                        TauCoeff%Alpha_C2
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing Alpha data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF

    ! Write the polynomial order indices
    WRITE( FileID, IOSTAT = IO_Status ) TauCoeff%Order_Index
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing order indices to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF

    ! Write the predictor indices
    WRITE( FileID, IOSTAT = IO_Status ) TauCoeff%Predictor_Index
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing predictor indices to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF

    ! Write the coefficient data
    WRITE( FileID, IOSTAT = IO_Status ) TauCoeff%C
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing gas absorption coefficients to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF

    ! Close the file
    CLOSE( FileID, STATUS = 'KEEP',   &
                   IOSTAT = IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    ! Output an info message
    IF ( Noisy ) THEN
      CALL Info_TauCoeff( TauCoeff, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

    RETURN


    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

    1000 CONTINUE
    CLOSE(FileID,STATUS=FILE_STATUS_ON_ERROR)

    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )

  END FUNCTION Write_TauCoeff_Binary

END MODULE TauCoeff_Binary_IO
