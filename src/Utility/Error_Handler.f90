
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
