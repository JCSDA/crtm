!
!  Error_Handler
!
!  Module to define simple error/exit codes and output messages.
!
!
!  Written by:     Paul van Delst, CIMSS/SSEC 12-Jun-2000
!                  paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2000, 2006 Paul van Delst
!

MODULE Error_Handler


  ! ---------------------
  ! Module use statements
  ! ---------------------

  USE File_Utility, ONLY: Get_Lun


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Display_Message
  PUBLIC :: Open_Message_Log


  ! ---------------------------------------------------
  ! Integer values that define the error or exit state.
  ! Note: These values are totally arbitrary.
  ! ---------------------------------------------------
 
  INTEGER, PARAMETER, PUBLIC :: SUCCESS     = 0
  INTEGER, PARAMETER, PUBLIC :: INFORMATION = 1
  INTEGER, PARAMETER, PUBLIC :: WARNING     = 2
  INTEGER, PARAMETER, PUBLIC :: FAILURE     = 3
  INTEGER, PARAMETER, PUBLIC :: EOF         = 4
  INTEGER, PARAMETER, PUBLIC :: UNDEFINED   = 5


  ! -----------------------------------
  ! Definitions of local parameter data
  ! -----------------------------------

  ! -- Character descriptors of the error states
  INTEGER,      PARAMETER :: MAX_N_STATES = 5
  CHARACTER(*), PARAMETER, DIMENSION( 0:MAX_N_STATES ) :: &
    STATE_DESCRIPTOR = (/ 'SUCCESS    ', &
                          'INFORMATION', &
                          'WARNING    ', &
                          'FAILURE    ', &
                          'END-OF-FILE', &
                          'UNDEFINED  ' /)


CONTAINS

!
!  Display_Message
!
!  RECURSIVE subroutine to display messages.
!
!  This routine calls itself if the optional argument Message_Log 
!  is passed and an error occurs opening the output log file.
!
!  CALLING SEQUENCE:
!    CALL Display_Message( Routine_Name, &
!                          Message,      &
!                          Error_State,  &
!                          Message_Log = Message_Log )
!
!  INPUT ARGUMENTS:
!    Routine_Name: Name of the routine in which the message originated.
!                  UNITS:      N/A
!                  TYPE:       CHARACTER(*)
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
!    Message:      Message text
!                  UNITS:      N/A
!                  TYPE:       CHARACTER(*)
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
!    Error_State:  Flag corresponding to one of the defined error states.
!                  If not, the error state is set to UNDEFINED.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
!  OPTIONAL INPUT ARGUMENTS:
!    Message_Log:  Character string specifying a filename in which any
!                  messages will be logged. If not specified, or if an
!                  error occurs opening the log file, the default action
!                  is to output messages to the screen.
!                  UNITS:      N/A
!                  TYPE:       CHARACTER(*)
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN), OPTIONAL

  RECURSIVE SUBROUTINE Display_Message ( Routine_Name, &
                                         Message,      &
                                         Error_State,  &
                                         Message_Log   )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: Routine_Name
    CHARACTER(*),           INTENT(IN) :: Message
    INTEGER,                INTENT(IN) :: Error_State
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log

    ! Local parameters
    CHARACTER(*), PARAMETER :: THIS_ROUTINE_NAME = 'Display_Message'
    CHARACTER(*), PARAMETER :: FMT_STRING = '( 1x, a, "(", a, ") : ", a )'

    ! Local variables
    INTEGER :: Error_State_To_Use
    LOGICAL :: Log_To_StdOut
    INTEGER :: File_ID
    INTEGER :: Error_Status


    ! Check the input error state
    Error_State_To_Use = Error_State
    IF ( Error_State < 0 .OR. Error_State > MAX_N_STATES ) THEN
      Error_State_To_Use = UNDEFINED
    END IF

    ! Set the message log. Default is output to stdout
    Log_To_StdOut = .TRUE.
    IF ( PRESENT( Message_Log ) ) THEN
      Log_To_StdOut = .FALSE.
      Error_Status = Open_Message_Log( TRIM( Message_Log ), File_ID )
      IF ( Error_Status /= 0 ) THEN
        CALL Display_Message( THIS_ROUTINE_NAME, &
                              'Error opening message log file', &
                              FAILURE )
        Log_To_StdOut = .TRUE.
      END IF
    END IF

    ! Output the message
    IF ( Log_To_StdOut ) THEN
      WRITE( *, FMT = FMT_STRING ) &
                TRIM( Routine_Name ), &
                TRIM( STATE_DESCRIPTOR( Error_State_To_Use ) ), &
                TRIM( Message )
    ELSE
      WRITE( File_ID, FMT = FMT_STRING ) &
                      TRIM( Routine_Name ), &
                      TRIM( STATE_DESCRIPTOR( Error_State_To_Use ) ), &
                      TRIM( Message )
      CLOSE( File_ID )
    END IF

  END SUBROUTINE Display_Message


!  Open_Message_Log
!
!  Function to open the message log file.
!
!  CALLING SEQUENCE:
!    Error_Status =  open_Message_Log( Message_Log, &  ! Input
!                                      File_ID      )  ! Output
!
!  INPUTS:
!    Message_Log:  Character string specifying the filename to open.
!                  UNITS:      N/A
!                  TYPE:       CHARACTER(*)
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
!  OUTPUTS:
!    File_ID:      Logical unit number associated with the
!                  Message_Log file.
!                  Return value is undefined if an error occurs.
!                  UNITS:      None
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(OUT)
!
!  FUNCTION RESULT:
!    Error_Status: The return value is an integer defining the error status.
!                  The error codes are defined in this ERROR_HANDLER module.
!                  If == SUCCESS the Message_Log file was successfully opened.
!                     == FAILURE an unrecoverable error occurred.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!
!  SIDE EFFECTS:
!    The file is opened for SEQUENTIAL, FORMATTED access with
!    UNKNOWN status, position of APPEND, and action of READWRITE.
!
!    Hopefully all of these options will not cause an existing file
!    to be inadvertantly overwritten.

  FUNCTION Open_Message_Log( Message_Log, File_ID ) RESULT( Error_Status )

    ! Arguments
    CHARACTER(*), INTENT(IN)  :: Message_Log
    INTEGER,      INTENT(OUT) :: File_ID

    ! Function result
    INTEGER :: Error_Status

    ! Local variables
    INTEGER :: Lun
    INTEGER :: IO_Status


    ! Set successful return status
    Error_Status = SUCCESS

    ! Get a file unit number
    Lun = Get_Lun()
    IF ( Lun < 0 ) THEN
      Error_Status = FAILURE
      RETURN
    END IF

    ! Open the file
    OPEN( Lun, FILE     = TRIM( Message_Log ), &
               ACCESS   = 'SEQUENTIAL', &
               FORM     = 'FORMATTED', &
               STATUS   = 'UNKNOWN', &
               POSITION = 'APPEND', &
               ACTION   = 'READWRITE', &
               IOSTAT   = IO_Status )
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      RETURN
    END IF

    ! Return the file ID
    File_ID = Lun

  END FUNCTION Open_Message_Log

END MODULE Error_Handler
