!
! CRTM_Options_Define
!
! Module defining the CRTM Options optional argument data structure
! and containing routines to manipulate it.
!       
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 25-Sep-2004
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_Options_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use statements
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, Display_Message
  USE Compare_Float_Numbers, ONLY: Compare_Float
  USE CRTM_Parameters      , ONLY: ZERO, SET, NOT_SET, STRLEN
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Structure data type
  PUBLIC :: CRTM_Options_type
  ! Definition procedures
  PUBLIC :: CRTM_Associated_Options
  PUBLIC :: CRTM_Destroy_Options
  PUBLIC :: CRTM_Allocate_Options
  PUBLIC :: CRTM_Assign_Options
  PUBLIC :: CRTM_Equal_Options
  ! Utility procedures
  PUBLIC :: CRTM_RCS_ID_Options


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE CRTM_Associated_Options
    MODULE PROCEDURE Associated_Scalar
    MODULE PROCEDURE Associated_Rank1
  END INTERFACE CRTM_Associated_Options

  INTERFACE CRTM_Destroy_Options
    MODULE PROCEDURE Destroy_Scalar
    MODULE PROCEDURE Destroy_Rank1
  END INTERFACE CRTM_Destroy_Options

  INTERFACE CRTM_Allocate_Options
    MODULE PROCEDURE Allocate_Scalar
    MODULE PROCEDURE Allocate_Rank1
  END INTERFACE CRTM_Allocate_Options

  INTERFACE CRTM_Assign_Options
    MODULE PROCEDURE Assign_Scalar
    MODULE PROCEDURE Assign_Rank1
  END INTERFACE CRTM_Assign_Options

  INTERFACE CRTM_Equal_Options
    MODULE PROCEDURE Equal_Scalar
    MODULE PROCEDURE Equal_Rank1
  END INTERFACE CRTM_Equal_Options


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id$'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256


  ! ----------------------------
  ! Options data type definition
  ! ----------------------------
  !:tdoc+:
  TYPE :: CRTM_Options_type
    INTEGER :: n_Allocates = 0
    ! Dimensions
    INTEGER :: n_Channels = 0  ! L dimension
    ! Index into channel-specific components
    INTEGER :: Channel = 0
    ! Emissivity optional arguments
    INTEGER           :: Emissivity_Switch =  NOT_SET
    REAL(fp), POINTER :: Emissivity(:)     => NULL() ! L
    ! Direct reflectivity optional arguments
    INTEGER           :: Direct_Reflectivity_Switch =  NOT_SET
    REAL(fp), POINTER :: Direct_Reflectivity(:)     => NULL() ! L
    ! Antenna correction application
    INTEGER :: Antenna_Correction = NOT_SET
  END TYPE CRTM_Options_type
  !:tdoc-:


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Associated_Options
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       CRTM Options structure.
!
! CALLING SEQUENCE:
!       Association_Status = CRTM_Associated_Options( Options          , &
!                                                     ANY_Test=Any_Test  )
!
! INPUT ARGUMENTS:
!       Options:             Options structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       CRTM_Options_type
!                            DIMENSION:  Scalar or Rank-1 array
!                            ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            Options structure pointer members are associated.
!                            The default is to test if ALL the pointer members
!                            are associated.
!                            If ANY_Test = 0, test if ALL the pointer members
!                                             are associated.  (DEFAULT)
!                               ANY_Test = 1, test if ANY of the pointer members
!                                             are associated.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Association_Status:  The return value is a logical value indicating the
!                            association status of the Options pointer
!                            members.
!                            .TRUE.  - if ALL the Options pointer members
!                                      are associated, or if the ANY_Test argument
!                                      is set and ANY of the Options
!                                      pointer members are associated.
!                            .FALSE. - some or all of the Options pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Same as input Options argument
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Associated_Scalar( Options , & ! Input
                              ANY_Test) & ! Optional input
                            RESULT( Association_Status )
    ! Arguments
    TYPE(CRTM_Options_type), INTENT(IN) :: Options
    INTEGER,       OPTIONAL, INTENT(IN) :: ANY_Test
    ! Function result
    LOGICAL :: Association_Status
    ! Local variables
    LOGICAL :: ALL_Test

    ! Set up
    ! ------
    ! Default is to test ALL the pointer members
    ! for a true association status....
    ALL_Test = .TRUE.
    ! ...unless the ANY_Test argument is set.
    IF ( PRESENT( ANY_Test ) ) THEN
      IF ( ANY_Test == SET ) ALL_Test = .FALSE.
    END IF
    ! Initialise a result
    Association_Status = .FALSE.

    
    ! Test the structure pointer association
    ! --------------------------------------
    IF ( ALL_Test ) THEN
      IF (ASSOCIATED(Options%Emissivity         ) .AND. &
          ASSOCIATED(Options%Direct_Reflectivity)) THEN 
        Association_Status = .TRUE.
      END IF
    ELSE
      IF (ASSOCIATED(Options%Emissivity         ) .OR. &
          ASSOCIATED(Options%Direct_Reflectivity)) THEN
        Association_Status = .TRUE.
      END IF
    END IF

  END FUNCTION Associated_Scalar

  FUNCTION Associated_Rank1( Options , & ! Input
                             ANY_Test) & ! Optional input
                           RESULT( Association_Status )
    ! Arguments
    TYPE(CRTM_Options_type), INTENT(IN) :: Options(:)
    INTEGER,       OPTIONAL, INTENT(IN) :: ANY_Test
    ! Function result
    LOGICAL :: Association_Status(SIZE(Options))
    ! Local variables
    INTEGER :: i

    DO i = 1, SIZE(Options)
      Association_Status(i) = Associated_Scalar( Options(i),ANY_Test=ANY_Test )
    END DO

  END FUNCTION Associated_Rank1
  

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Destroy_Options
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of a CRTM
!       Options data structure.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_Options( Options                , &
!                                            Message_Log=Message_Log  )
! 
! OUTPUT ARGUMENTS:
!       Options:      Re-initialized Options structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Options_type
!                     DIMENSION:  Scalar or Rank-1
!                     ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     Messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output Messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the structure re-initialisation was successful
!                        == FAILURE - an error occurred, or
!                                   - the structure internal allocation counter
!                                     is not equal to zero (0) upon exiting this
!                                     function. This value is incremented and
!                                     decremented for every structure allocation
!                                     and deallocation respectively.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output Options argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Destroy_Scalar( Options    , &  ! Output
                           No_Clear   , &  ! Optional input
                           Message_Log) &  ! Error messaging
                         RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Options_type), INTENT(IN OUT) :: Options
    INTEGER,       OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Options(Scalar)'
    ! Local variables
    CHARACTER(ML) :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    ! Reset the dimension indicators
    Options%n_Channels = 0
    ! Default is to clear scalar members...
    Clear = .TRUE.
    ! ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == SET ) Clear = .FALSE.
    END IF
    IF ( Clear ) CALL CRTM_Clear_Options( Options )
    ! If ALL pointer members are NOT associated, do nothing
    IF ( .NOT. CRTM_Associated_Options( Options ) ) RETURN


    ! Deallocate the pointer members
    ! ------------------------------
    DEALLOCATE( Options%Emissivity         , &
                Options%Direct_Reflectivity, &
                STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error deallocating Options. STAT = ", i0 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM(Message), &
                            Error_Status,    &
                            Message_Log=Message_Log )
    END IF

    ! Decrement and test allocation counter
    ! -------------------------------------
    Options%n_Allocates = Options%n_Allocates - 1
    IF ( Options%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i0 )' ) &
                      Options%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM(Message), &
                            Error_Status,    &
                            Message_Log=Message_Log )
    END IF
  END FUNCTION Destroy_Scalar

  FUNCTION Destroy_Rank1( Options    , &  ! Output
                          No_Clear   , &  ! Optional input
                          Message_Log) &  ! Error messaging
                        RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Options_type), INTENT(IN OUT) :: Options(:)
    INTEGER,      OPTIONAL,  INTENT(IN)     :: No_Clear
    CHARACTER(*), OPTIONAL,  INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Options(Rank-1)'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: n

    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Loop over Options entries
    ! -------------------------
    DO n = 1, SIZE( Options )
      Scalar_Status = Destroy_Scalar( Options(n)             , &
                                      No_Clear   =No_Clear   , &
                                      Message_Log=Message_Log  )
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error destroying element #", i0, &
                          &" of Options structure array." )' ) n
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
      END IF
    END DO
  END FUNCTION Destroy_Rank1


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Allocate_Options
! 
! PURPOSE:
!       Function to allocate the pointer members of a CRTM Options
!       data structure.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Allocate_Options( n_Channels             , &
!                                             Options                , &
!                                             Message_Log=Message_Log  )
!
! INPUT ARGUMENTS:
!       n_Channels:   Number of sensor channels
!                     Must be > 0
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Options:      Options structure with allocated pointer members.
!                     Upon allocation, all pointer members are initialized to
!                     a value of zero.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Options_type
!                     DIMENSION:  Scalar or Rank-1
!                     ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     Messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output Messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the structure pointer allocations were
!                                   successful
!                        == FAILURE - an error occurred, or
!                                   - the structure internal allocation counter
!                                     is not equal to one (1) upon exiting this
!                                     function. This value is incremented and
!                                     decremented for every structure allocation
!                                     and deallocation respectively.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output Options argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Allocate_Scalar( n_Channels , &  ! Input
                            Options    , &  ! Output
                            Message_Log) &  ! Error messaging
                          RESULT( Error_Status )
    ! Arguments
    INTEGER,                 INTENT(IN)     :: n_Channels
    TYPE(CRTM_Options_type), INTENT(IN OUT) :: Options
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Options(Scalar)'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Allocate_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    ! Dimensions
    IF ( n_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Channels must be > 0.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    IF ( CRTM_Associated_Options( Options, ANY_Test=SET ) ) THEN
      Error_Status = CRTM_Destroy_Options( Options, &
                                           No_Clear=SET, &
                                           Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating Options pointer members.', &
                              Error_Status,    &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF


    ! Perform the pointer allocation
    ! ------------------------------
    ALLOCATE( Options%Emissivity(n_Channels)         , &
              Options%Direct_Reflectivity(n_Channels), &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating Options data arrays. STAT = ", i0 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM(Message), &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Assign dimensions
    ! -----------------
    Options%n_Channels = n_Channels


    ! Initialise the arrays
    ! ---------------------
    Options%Emissivity          = ZERO
    Options%Direct_Reflectivity = ZERO


    ! Increment and test the allocation counter
    ! -----------------------------------------
    Options%n_Allocates = Options%n_Allocates + 1
    IF ( Options%n_Allocates /= 1 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i0 )' ) &
                      Options%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM(Message), &
                            Error_Status,    &
                            Message_Log=Message_Log )
    END IF

  END FUNCTION Allocate_Scalar

  FUNCTION Allocate_Rank1( n_Channels , &  ! Input,  scalar
                           Options    , &  ! Output, rank-1
                           Message_Log) &  ! Error messaging
                         RESULT( Error_Status )
    ! Arguments
    INTEGER,                 INTENT(IN)     :: n_Channels
    TYPE(CRTM_Options_type), INTENT(IN OUT) :: Options(:)
    CHARACTER(*), OPTIONAL,  INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Options(Rank-1)'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: n

    ! Set up
    ! ------
    Error_Status = SUCCESS


    ! Loop over Options entries
    ! -------------------------
    DO n = 1, SIZE(Options)
      Error_Status = Allocate_Scalar( n_Channels, & ! Input
                                      Options(n), & ! Output
                                      Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error allocating element #", i0, &
                          &" of rank-1 Options structure array." )' ) n
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END DO
  END FUNCTION Allocate_Rank1


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Assign_Options
!
! PURPOSE:
!       Function to copy valid CRTM Options structures.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Assign_Options( Options_in             , &
!                                           Options_out            , &
!                                           Message_Log=Message_Log  )
!
! INPUT ARGUMENTS:
!       Options_in:      Options structure which is to be copied.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Options_type
!                        DIMENSION:  Scalar or Rank-1
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Options_out:     Copy of the input structure, Options_in.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Options_type
!                        DIMENSION:  Same as Options_in
!                        ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:     Character string specifying a filename in which any
!                        Messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output Messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the structure assignment was successful
!                           == FAILURE an error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output Options argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Assign_Scalar( Options_in , &  ! Input
                          Options_out, &  ! Output
                          Message_Log) &  ! Error messaging
                        RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Options_type), INTENT(IN)     :: Options_in
    TYPE(CRTM_Options_type), INTENT(IN OUT) :: Options_out
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_Options(Scalar)'

    ! Set up
    ! ------
    Error_Status = SUCCESS
    ! ALL *input* pointers must be associated.
    IF ( .NOT. CRTM_Associated_Options( Options_In ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT Options_in pointer members are NOT associated.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Allocate data arrays
    ! --------------------
    Error_Status = CRTM_Allocate_Options( Options_in%n_Channels  , &
                                          Options_out            , &
                                          Message_Log=Message_Log  )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output Options arrays.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Assign non-dimension scalar members
    ! -----------------------------------
    Options_out%Channel                    = Options_in%Channel
    Options_out%Emissivity_Switch          = Options_in%Emissivity_Switch
    Options_out%Direct_Reflectivity_Switch = Options_in%Direct_Reflectivity_Switch
    Options_out%Antenna_Correction         = Options_in%Antenna_Correction
    
    
    ! Copy array data
    ! ---------------
    Options_out%Emissivity          = Options_in%Emissivity
    Options_out%Direct_Reflectivity = Options_in%Direct_Reflectivity

  END FUNCTION Assign_Scalar

  FUNCTION Assign_Rank1( Options_in , &  ! Input
                         Options_out, &  ! Output
                         Message_Log) &  ! Error messaging
                       RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Options_type), INTENT(IN)     :: Options_in(:)
    TYPE(CRTM_Options_type), INTENT(IN OUT) :: Options_out(:)
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_Options(Rank-1)'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: i, n

    ! Set up
    ! ------
    Error_Status = SUCCESS
    ! Dimensions
    n = SIZE(Options_in)
    IF ( SIZE(Options_out) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Options_in and Options_out arrays'//&
                            ' have different sizes', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Perform the assignment
    ! ----------------------
    DO i = 1, n
      Error_Status = Assign_Scalar( Options_in(i)          , &
                                    Options_out(i)         , &
                                    Message_Log=Message_Log  )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error copying element #", i0, &
                          &" of Options structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END DO
  END FUNCTION Assign_Rank1


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Equal_Options
!
! PURPOSE:
!       Function to test if two CRTM Options structures are equal.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Equal_Options( Options_LHS            , &
!                                          Options_RHS            , &
!                                          ULP_Scale  =ULP_Scale  , &
!                                          Check_All  =Check_All  , &
!                                          Message_Log=Message_Log  )
!
!
! INPUT ARGUMENTS:
!       Options_LHS:        Options structure to be compared; equivalent to the
!                           left-hand side of a lexical comparison, e.g.
!                             IF ( Options_LHS == Options_RHS ).
!                           In the context of the CRTM, rank-1 corresponds to an
!                           vector of profiles.
!                           UNITS:      N/A
!                           TYPE:       CRTM_Options_type
!                           DIMENSION:  Scalar or Rank-1 array
!                           ATTRIBUTES: INTENT(IN)
!
!       Options_RHS:        Options structure to be compared to; equivalent to
!                           right-hand side of a lexical comparison, e.g.
!                             IF ( Options_LHS == Options_RHS ).
!                           UNITS:      N/A
!                           TYPE:       CRTM_Options_type
!                           DIMENSION:  Same as Options_LHS
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ULP_Scale:          Unit of data precision used to scale the floating
!                           point comparison. ULP stands for "Unit in the Last Place,"
!                           the smallest possible increment or decrement that can be
!                           made using a machine's floating point arithmetic.
!                           Value must be positive - if a negative value is supplied,
!                           the absolute value is used. If not specified, the default
!                           value is 1.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Check_All:          Set this argument to check ALL the floating point
!                           channel data of the Options structures. The default
!                           action is return with a FAILURE status as soon as
!                           any difference is found. This optional argument can
!                           be used to get a listing of ALL the differences
!                           between data in Options structures.
!                           If == 0, Return with FAILURE status as soon as
!                                    ANY difference is found  *DEFAULT*
!                              == 1, Set FAILURE status if ANY difference is
!                                    found, but continue to check ALL data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      None
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the Message_Handler module.
!                           If == SUCCESS the structures were equal
!                              == FAILURE - an error occurred, or
!                                         - the structures were different.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Equal_Scalar( Options_LHS, &  ! Input
                         Options_RHS, &  ! Input
                         ULP_Scale  , &  ! Optional input
                         Check_All  , &  ! Optional input
                         Message_Log) &  ! Error messaging
                       RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Options_type), INTENT(IN)  :: Options_LHS
    TYPE(CRTM_Options_type), INTENT(IN)  :: Options_RHS
    INTEGER,       OPTIONAL, INTENT(IN)  :: ULP_Scale
    INTEGER,       OPTIONAL, INTENT(IN)  :: Check_All
    CHARACTER(*),  OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Equal_Options(scalar)'
    ! Local variables
    CHARACTER(ML) :: Message
    LOGICAL :: Check_Once
    INTEGER :: l

    ! Set up
    ! ------
    Error_Status = SUCCESS
    ! Default action is to return on ANY difference...
    Check_Once = .TRUE.
    ! ...unless the Check_All argument is set
    IF ( PRESENT(Check_All) ) THEN
      IF ( Check_All == SET ) Check_Once = .FALSE.
    END IF
    ! Check the structure association status
    IF ( .NOT. CRTM_Associated_Options( Options_LHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT Options_LHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF
    IF ( .NOT. CRTM_Associated_Options( Options_RHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT Options_RHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Check dimensions
    ! ----------------
    IF ( Options_LHS%n_Channels /= Options_RHS%n_Channels ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Structure dimensions are different', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Compare the values
    ! ------------------
    IF ( Options_LHS%Channel /= Options_RHS%Channel ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Channel values are different:",2(1x,i0))') &
                     Options_LHS%Channel, Options_RHS%Channel
      CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( Options_LHS%Emissivity_Switch /= Options_RHS%Emissivity_Switch ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Emissivity_Switch values are different:",2(1x,i0))') &
                     Options_LHS%Emissivity_Switch, Options_RHS%Emissivity_Switch
      CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    DO l = 1, Options_LHS%n_Channels
      IF ( .NOT. Compare_Float( Options_LHS%Emissivity(l), &
                                Options_RHS%Emissivity(l), &
                                ULP=ULP_Scale              ) ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Emissivity values are different at channel index ",i0,&
                        &":",3(1x,es13.6))') &
                       l, Options_LHS%Emissivity(l), &
                          Options_RHS%Emissivity(l), &
                          Options_LHS%Emissivity(l)-Options_RHS%Emissivity(l)
        CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    IF ( Options_LHS%Direct_Reflectivity_Switch /= Options_RHS%Direct_Reflectivity_Switch ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Direct_Reflectivity_Switch values are different:",2(1x,i0))') &
                     Options_LHS%Direct_Reflectivity_Switch, Options_RHS%Direct_Reflectivity_Switch
      CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    DO l = 1, Options_LHS%n_Channels
      IF ( .NOT. Compare_Float( Options_LHS%Direct_Reflectivity(l), &
                                Options_RHS%Direct_Reflectivity(l), &
                                ULP=ULP_Scale                       ) ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Direct_Reflectivity values are different at channel index ",i0,&
                        &":",3(1x,es13.6))') &
                       l, Options_LHS%Direct_Reflectivity(l), &
                          Options_RHS%Direct_Reflectivity(l), &
                          Options_LHS%Direct_Reflectivity(l)-Options_RHS%Direct_Reflectivity(l)
        CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
    IF ( Options_LHS%Antenna_Correction /= Options_RHS%Antenna_Correction ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Antenna_Correction flag values are different:",2(1x,i0))') &
                     Options_LHS%Antenna_Correction, Options_RHS%Antenna_Correction
      CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
  END FUNCTION Equal_Scalar

  FUNCTION Equal_Rank1( Options_LHS, &  ! Input
                        Options_RHS, &  ! Output
                        ULP_Scale  , &  ! Optional input
                        Check_All  , &  ! Optional input
                        Message_Log) &  ! Error messaging
                      RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Options_type), INTENT(IN)  :: Options_LHS(:)
    TYPE(CRTM_Options_type), INTENT(IN)  :: Options_RHS(:)
    INTEGER,       OPTIONAL, INTENT(IN)  :: ULP_Scale
    INTEGER,       OPTIONAL, INTENT(IN)  :: Check_All
    CHARACTER(*),  OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Equal_Options(Rank-1)'
    ! Local variables
    CHARACTER(ML) :: Message
    LOGICAL :: Check_Once
    INTEGER :: Scalar_Status
    INTEGER :: m, n_Profiles

    ! Set up
    ! ------
    Error_Status = SUCCESS
    ! Default action is to return on ANY difference...
    Check_Once = .TRUE.
    ! ...unless the Check_All argument is set
    IF ( PRESENT(Check_All) ) THEN
      IF ( Check_All == SET ) Check_Once = .FALSE.
    END IF
    ! Arguments must conform
    n_Profiles = SIZE(Options_LHS)
    IF ( SIZE(Options_RHS) /= n_Profiles ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Options_LHS and Options_RHS arrays'//&
                            ' have different dimensions', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Test for equality
    ! -----------------
    DO m = 1, n_Profiles
      Scalar_Status = Equal_Scalar( Options_LHS(m), &
                                    Options_RHS(m), &
                                    ULP_Scale  =ULP_Scale, &
                                    Check_All  =Check_All, &
                                    Message_Log=Message_Log )
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error comparing element (",i0,")", &
                          &" of rank-1 Options structure array." )' ) m
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
  END FUNCTION Equal_Rank1


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_RCS_ID_Options
!
! PURPOSE:
!       Subroutine to return the module RCS Id information.
!
! CALLING SEQUENCE:
!       CALL CRTM_RCS_Id_Options( RCS_Id )
!
! OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_RCS_ID_Options( RCS_Id )
    CHARACTER(*), INTENT(OUT) :: RCS_Id
    RCS_Id = MODULE_RCS_ID
  END SUBROUTINE CRTM_RCS_ID_Options


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!----------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Clear_Options
!
! PURPOSE:
!       Subroutine to clear the scalar members of a CRTM Options structure.
!
! CALLING SEQUENCE:
!       CALL CRTM_Clear_Options( Options )
!
! OUTPUT ARGUMENTS:
!       Options:      Options structure for which the scalar members have
!                     been cleared.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Options_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the output Options argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!----------------------------------------------------------------------------------

  SUBROUTINE CRTM_Clear_Options( Options )
    TYPE(CRTM_Options_type), INTENT(IN OUT) :: Options
    Options%Emissivity_Switch          = NOT_SET
    Options%Direct_Reflectivity_Switch = NOT_SET
    Options%Antenna_Correction         = NOT_SET
  END SUBROUTINE CRTM_Clear_Options

END MODULE CRTM_Options_Define
