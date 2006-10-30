MODULE message_handler

  IMPLICIT NONE
  PRIVATE
 

  ! --------------
  ! RCS Id Keyword
  ! --------------

  CHARACTER( LEN = 128 ), PARAMETER :: &
  rcs_id = '$Id: message_handler.f90,v 1.1 2000/06/29 20:47:29 paulv Exp $'


  ! ----------
  ! Parameters
  ! ----------

  ! -- Maximum message string length
  INTEGER, PARAMETER :: max_message_length = 256

  ! -- Maximum routine name string length (F90 standard)
  INTEGER, PARAMETER :: max_routine_name_length = 31

  ! -- Maximum number of messages in stack
  INTEGER, PARAMETER :: max_n_messages = 100

  ! -- Integer values that define the error state.
  ! -- Note: These values are totally arbitrary. 
  INTEGER, PARAMETER, PUBLIC :: undefined_state        = 0, &
                                complete_success_state = undefined_state + 1,        &
                                success_state          = complete_success_state + 1, &
                                information_state      = success_state + 1,          &
                                warning_state          = information_state + 1,      &
                                failure_state          = warning_state + 1

  ! -- Character descriptors of the error states
  INTEGER, PARAMETER :: max_n_states = 6
  CHARACTER( LEN = 11 ), PARAMETER, DIMENSION( 0:max_n_states-1 ) :: &
    state_descriptor = (/ 'UNDEFINED  ', &
                          'SUCCESS    ', &
                          'SUCCESS    ', &
                          'INFORMATION', &
                          'WARNING    ', &
                          'FAILURE    ' /)

 
  ! ------------- 
  ! Defined TYPEs
  ! ------------- 

  ! -- Message variable
  TYPE :: message
    INTEGER                                     :: state
    CHARACTER ( LEN = max_routine_name_length ) :: routine_name
    CHARACTER ( LEN = max_message_length )      :: message_text
  END TYPE message
 
  ! -- Message state data type
  TYPE, PUBLIC :: message_state
    PRIVATE
    INTEGER                       :: current_n_message, &
                                     total_n_messages
    TYPE( message), &
      DIMENSION( max_n_messages ) :: message_stack
  END TYPE message_state


  ! ----------------------------
  ! Define explicit visibilities
  ! ----------------------------

  ! -- Boolean FUNCTIONs for reporting the current error state
  PUBLIC :: state_is_success,     &
            state_is_information, &
            state_is_warning,     &
            state_is_failure
 
  ! -- Routines for setting state and adding a new message
  PUBLIC :: set_state_to_complete_success,     &
            set_state_to_success,              &
            set_state_to_information,          &
            set_state_to_warning,              &
            set_state_to_failure,              &
            initialize_state
 
  ! -- Routines for getting state information and retrieving message texts
  PUBLIC :: inquire_state

  ! -- Routines for displaying/logging message texts
  PUBLIC :: display_messages
 
CONTAINS


  ! -------------------------------------------------------
  ! Boolean FUNCTIONs for reporting the current error state
  ! -------------------------------------------------------

  !====================================================
  LOGICAL FUNCTION state_is_success( message_variable )
  !====================================================

    ! -- Argument
    TYPE( message_state ), INTENT( IN ) :: message_variable

    ! -- Local
    INTEGER :: current_n_message, &
               current_state

    ! -- Set current message number
    current_n_message = message_variable%current_n_message

    ! -- Set default return state
    state_is_success = .FALSE.

    ! -- Test state
    current_state = message_variable%message_stack( current_n_message )%state

    IF ( current_state == complete_success_state .OR. &
         current_state == success_state ) &
      state_is_success = .TRUE.

  END FUNCTION state_is_success



  !========================================================
  LOGICAL FUNCTION state_is_information( message_variable )
  !===============================--=======================

    ! -- Argument
    TYPE( message_state ), INTENT( IN ) :: message_variable

    ! -- Local
    INTEGER :: current_n_message

    ! -- Set current message number
    current_n_message = message_variable%current_n_message

    ! -- Set default return state
    state_is_information = .FALSE.

    ! -- Test state
    IF ( message_variable%message_stack( current_n_message )%state == information_state ) &
      state_is_information = .TRUE.

  END FUNCTION state_is_information



  !====================================================
  LOGICAL FUNCTION state_is_warning( message_variable )
  !===========================--=======================

    ! -- Argument
    TYPE( message_state ), INTENT( IN ) :: message_variable

    ! -- Local
    INTEGER :: current_n_message

    ! -- Set current message number
    current_n_message = message_variable%current_n_message

    ! -- Set default return state
    state_is_warning = .FALSE.

    ! -- Test state
    IF ( message_variable%message_stack( current_n_message )%state == warning_state ) &
      state_is_warning = .TRUE.

  END FUNCTION state_is_warning



  !====================================================
  LOGICAL FUNCTION state_is_failure( message_variable )
  !===========================--=======================

    ! -- Argument
    TYPE( message_state ), INTENT( IN ) :: message_variable

    ! -- Local
    INTEGER :: current_n_message

    ! -- Set current message number
    current_n_message = message_variable%current_n_message

    ! -- Set default return state
    state_is_failure = .FALSE.

    ! -- Test state
    IF ( message_variable%message_stack( current_n_message )%state == failure_state ) &
      state_is_failure = .TRUE.

  END FUNCTION state_is_failure




  ! ---------------------------------------------------
  ! Routines for setting state and adding a new message
  ! ---------------------------------------------------

  !=======================================================================================
  SUBROUTINE set_state_to_complete_success( message_variable, routine_name, message_text )
  !=======================================================================================

    ! -- Arguments
    TYPE( message_state ), INTENT( INOUT )        :: message_variable
    CHARACTER ( LEN = * ), INTENT( IN ), OPTIONAL :: routine_name
    CHARACTER ( LEN = * ), INTENT( IN ), OPTIONAL :: message_text

    ! -- Local variables
    INTEGER :: state

    ! -- Set the error state
    state = complete_success_state

    ! -- Update the error state
    CALL set_state( message_variable, state, routine_name, message_text )

  END SUBROUTINE set_state_to_complete_success


  !==============================================================================
  SUBROUTINE set_state_to_success( message_variable, routine_name, message_text )
  !==============================================================================

    ! -- Arguments
    TYPE( message_state ), INTENT( INOUT )        :: message_variable
    CHARACTER ( LEN = * ), INTENT( IN ), OPTIONAL :: routine_name
    CHARACTER ( LEN = * ), INTENT( IN ), OPTIONAL :: message_text

    ! -- Local variables
    INTEGER :: state

    ! -- Set the error state
    state = success_state

    ! -- Update the error state
    CALL set_state( message_variable, state, routine_name, message_text )

  END SUBROUTINE set_state_to_success


  !==================================================================================
  SUBROUTINE set_state_to_information( message_variable, routine_name, message_text )
  !==================================================================================

    ! -- Arguments
    TYPE( message_state ), INTENT( INOUT )        :: message_variable
    CHARACTER ( LEN = * ), INTENT( IN ), OPTIONAL :: routine_name
    CHARACTER ( LEN = * ), INTENT( IN ), OPTIONAL :: message_text

    ! -- Local variables
    INTEGER :: state

    ! -- Set the error state
    state = information_state

    ! -- Update the error state
    CALL set_state( message_variable, state, routine_name, message_text )

  END SUBROUTINE set_state_to_information


  !==============================================================================
  SUBROUTINE set_state_to_warning( message_variable, routine_name, message_text )
  !==============================================================================

    ! -- Arguments
    TYPE( message_state ), INTENT( INOUT )        :: message_variable
    CHARACTER ( LEN = * ), INTENT( IN ), OPTIONAL :: routine_name
    CHARACTER ( LEN = * ), INTENT( IN ), OPTIONAL :: message_text

    ! -- Local variables
    INTEGER :: state

    ! -- Set the error state
    state = warning_state

    ! -- Update the error state
    CALL set_state( message_variable, state, routine_name, message_text )

  END SUBROUTINE set_state_to_warning


  !==============================================================================
  SUBROUTINE set_state_to_failure( message_variable, routine_name, message_text )
  !==============================================================================

    ! -- Arguments
    TYPE( message_state ), INTENT( INOUT )        :: message_variable
    CHARACTER ( LEN = * ), INTENT( IN ), OPTIONAL :: routine_name
    CHARACTER ( LEN = * ), INTENT( IN ), OPTIONAL :: message_text

    ! -- Local variables
    INTEGER :: state

    ! -- Set the error state
    state = failure_state

    ! -- Update the error state
    CALL set_state( message_variable, state, routine_name, message_text )

  END SUBROUTINE set_state_to_failure


  !==========================================================================
  SUBROUTINE set_state( message_variable, state, routine_name, message_text )
  !==========================================================================

!   -----------------
!   Type declarations
!   -----------------

!   -- Arguments
    TYPE( message_state ), INTENT( INOUT )        :: message_variable
    INTEGER,               INTENT( IN )           :: state
    CHARACTER ( LEN = * ), INTENT( IN ), OPTIONAL :: routine_name
    CHARACTER ( LEN = * ), INTENT( IN ), OPTIONAL :: message_text

!   -- Local variables
    INTEGER         :: previous_message_number, &
                       this_message_number
    TYPE( message ) :: previous_message


!   -------------------------------
!   Get the previous message number
!   -------------------------------

    previous_message_number = message_variable%total_n_messages


!   ----------------------------------------------------------
!   Set a NEW message only if the previous one is not a 
!   "complete_success" message. Don't want to fill the message
!   stack with success stories
!   ----------------------------------------------------------

    any_previous_message: IF ( previous_message_number > 0 ) THEN

!     -- Get the previous message
      previous_message = message_variable%message_stack( previous_message_number )

!     -- If the state of the previous message is not "complete_success",
!     -- then increment message counters. Overwrite the top of the stack
!     -- if there are too many messages
      IF ( previous_message%state /= complete_success_state ) THEN
        this_message_number = MIN( message_variable%total_n_messages + 1, &
                                   max_n_messages )
      ELSE
        this_message_number = previous_message_number
      END IF

    ELSE

!     -- This is the first message
      this_message_number = 1

    END IF any_previous_message


!   -----------------------
!   Update message counters
!   -----------------------

    message_variable%total_n_messages  = this_message_number
    message_variable%current_n_message = this_message_number


!   ---------------------
!   Set the message state
!   ---------------------

    message_variable%message_stack( this_message_number )%state          = state

    IF ( PRESENT( routine_name ) ) &
      message_variable%message_stack( this_message_number )%routine_name = routine_name

    IF ( PRESENT( message_text ) ) &
      message_variable%message_stack( this_message_number )%message_text = message_text

  END SUBROUTINE set_state


  !==============================================
  SUBROUTINE initialize_state( message_variable )
  !==============================================

     ! -- Argument
     TYPE( message_state ), INTENT( INOUT ) :: message_variable

     ! -- Local variables
     INTEGER :: number_of_messages, i

     ! -- Get the current total number of messages
     number_of_messages = message_variable%total_n_messages

     ! -- Initialise message counters
     message_variable%total_n_messages  = 0
     message_variable%current_n_message = 0

     ! -- Explicitly "undefine" all the error states
     ! -- This shouldn't be necessary but....
     undefine : DO i = 1, number_of_messages
       message_variable%message_stack( i )%state = undefined_state
     END DO undefine

  END SUBROUTINE initialize_state



  ! -------------------------------------------------------------------
  ! Routines for getting state information and retrieving message texts
  ! -------------------------------------------------------------------

  !====================================================
  SUBROUTINE inquire_state( message_variable,         &
                            current_routine_name,     &
                            current_message_text,     &
                            current_message_number,   &
                            total_number_of_messages, &
                            message_length,           &
                            routine_name_length,      &
                            max_number_of_messages )
  !====================================================

    ! -- Arguments
    TYPE( message_state ), INTENT( IN ) ::            message_variable

    CHARACTER ( LEN = max_routine_name_length ), &
                           INTENT( OUT ), OPTIONAL :: current_routine_name

    CHARACTER ( LEN = max_message_length ), &
                           INTENT( OUT ), OPTIONAL :: current_message_text

    INTEGER,               INTENT( OUT ), OPTIONAL :: current_message_number,   &
                                                      total_number_of_messages, &
                                                      message_length,           &
                                                      routine_name_length,      &
                                                      max_number_of_messages

    ! -- Local variables
    INTEGER :: i

    ! -- Set the curent message number for internal use
    i = message_variable%current_n_message

    ! -- Get the current routine name for the error state
    IF ( PRESENT( current_routine_name ) ) THEN
      current_routine_name = message_variable%message_stack( i )%routine_name
    END IF

    ! -- Get the current message text for the error state
    IF ( PRESENT( current_message_text ) ) THEN
      current_message_text = message_variable%message_stack( i )%message_text
    END IF

    ! -- Get the current message number for the error state
    IF ( PRESENT( current_message_number ) ) THEN
      current_message_number = i
    END IF

    ! -- Get the total number of messages in the error state
    IF ( PRESENT( total_number_of_messages ) ) THEN
      total_number_of_messages = message_variable%total_n_messages
    END IF

    ! -- Get the parameter data used to define the elements
    ! -- and size of the error state
    IF ( PRESENT( message_length ) ) THEN
      message_length = max_message_length
    END IF

    ! -- Get the maximum routine name length
    IF ( PRESENT( routine_name_length ) ) THEN
      routine_name_length = max_routine_name_length
    END IF

    ! -- Get the maximum number of error messages, i.e.
    ! - the size of the error "stack"
    IF ( PRESENT( max_number_of_messages ) ) THEN
      max_number_of_messages = max_n_messages
    END IF

  END SUBROUTINE inquire_state


!##############################################################################
!
! NAME:
!       display_messages
!
! PURPOSE:
!       PUBLIC function to display messages
!
! CALLING SEQUENCE:
!       CALL display_messages( message_variable,            &
!                              message_type = message_type, &
!                              message_log  = message_log   )
!
! ARGUMENTS:
!       message_variable: INTENT( IN ) Derived type containing the message
!                         stack.
!
!       message_type:     INTENT( IN ) Optional character argument specifying
!                         what type of message to output. Valid type codes are:
!                           'F' == failure
!                           'W' == warning
!                           'I' == information
!                           'S' == success
!                           'A' == all messages in stack
!                         If not specified, default action is to output all
!                         messages.
!
!       message_log:      INTENT( IN ) Optional character argument specifying
!                         a filename in which the messages will be logged. If
!                         not specified, or if an error occurs opening the 
!                         log file, the default action is to output messages
!                         to the screen.
!
!##############################################################################

  SUBROUTINE display_messages ( message_variable, &
                                message_type,     &
                                message_log       )

!   -----------------
!   Type declarations
!   -----------------

!   -- Arguments
    TYPE( message_state ), INTENT( IN )           :: message_variable
    CHARACTER( LEN = 1 ),  INTENT( IN ), OPTIONAL :: message_type
    CHARACTER( LEN = * ),  INTENT( IN ), OPTIONAL :: message_log

!   -- Local variables
    CHARACTER( LEN = 1 ) :: local_message_type
    INTEGER              :: i, &
                            n_messages, &
                            display, display_log, &
                            file_id, open_status
    TYPE( message )      :: this_message

!   -- Intrinsics used
    INTRINSIC PRESENT, &
              TRIM


!   -- Externals
    INTEGER, EXTERNAL :: get_lun



!   -----------------------------------------------
!   Set the local message type. If message type not
!   specified, display all messages.
!   -----------------------------------------------

    IF ( PRESENT( message_type ) ) THEN
      local_message_type = message_type
    ELSE
      local_message_type = 'A'
    END IF


!   -------------------------------------------------------
!   Set the message log. If not specified, output to screen
!   -------------------------------------------------------

    IF ( PRESENT( message_log ) ) THEN

      display_log = 1

!     -- Open the file 
      file_id = get_lun()
      OPEN( file_id, FILE     = message_log,  &
                     ACCESS   = 'SEQUENTIAL', &
                     FORM     = 'FORMATTED',  &
                     STATUS   = 'UNKNOWN',    &
                     POSITION = 'APPEND',     &
                     ACTION   = 'READWRITE',  & ! Just READ may cause probs on some
                                                ! systems using POSITION = 'APPEND'
                     IOSTAT   = open_status )
      IF ( open_status /= 0 ) display_log = 0

    ELSE

      display_log = 0

    END IF


!   ---------------------------------------
!   Get the number of messages on the stack
!   ---------------------------------------

    n_messages = message_variable%total_n_messages
    IF ( n_messages <= 0 ) RETURN


!   ---------------------------------------------
!   Loop from the bottom of the stack to the top,
!   displaying all the messages
!   ---------------------------------------------

    message_loop: DO i = 1, n_messages

!     -- Get the message
      this_message = message_variable%message_stack( i )

!     -- Determine whether or not to display it
      display = 0
      SELECT CASE ( local_message_type )
        CASE ( 'F' ) ! Failure messages
          IF ( this_message%state == failure_state ) display = 1
        CASE ( 'W' ) ! Warning messages
          IF ( this_message%state == warning_state ) display = 1
        CASE ( 'I' ) ! Information mesages
          IF ( this_message%state == information_state ) display = 1
        CASE ( 'S' ) ! Success messages (not "complete success" though)
          IF ( this_message%state == success_state ) display = 1
        CASE ( 'A' ) ! All messages
          display = 1
        CASE DEFAULT ! Crap input
          RETURN
      END SELECT

!     -- Display the message
      display_message: IF ( display == 1 ) THEN

        log_message: IF ( display_log == 0 ) THEN
          WRITE( *, '( 1x, a, "(", a, ") : ", a )' ) &
                    TRIM( this_message%routine_name ), &
                    TRIM( state_descriptor( this_message%state ) ), &
                    TRIM( this_message%message_text )
        ELSE
          WRITE( file_id, '( 1x, a, "(", a, ") : ", a )' ) &
                          TRIM( this_message%routine_name ), &
                          TRIM( state_descriptor( this_message%state ) ), &
                          TRIM( this_message%message_text )
        END IF log_message

      END IF display_message

    END DO message_loop

    IF ( display_log == 1 ) CLOSE( file_id )

  END SUBROUTINE display_messages

END MODULE message_handler


!===============================================================================
! CVS/RCS modification history
!
! $Log: message_handler.f90,v $
! Revision 1.1  2000/06/29 20:47:29  paulv
! Initial checked in version of message handling routines.
!
!
!===============================================================================
