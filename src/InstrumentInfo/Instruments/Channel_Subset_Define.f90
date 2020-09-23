!
! Channel_Subset_Define
!
! Module containing the Channel Subset type definition and routines
! to manipulate it.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 25-Nov-2002
!                       paul.vandelst@ssec.wisc.edu
!

MODULE Channel_Subset_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Message_Handler, ONLY: SUCCESS, FAILURE, Display_Message
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! The structure definition
  PUBLIC :: Channel_Subset_type
  ! The structure methods
  PUBLIC :: Associated_Channel_Subset
  PUBLIC :: Destroy_Channel_Subset
  PUBLIC :: Allocate_Channel_Subset
  PUBLIC :: Assign_Channel_Subset


  ! -------------------
  ! Procedure overloads
  ! -------------------
  INTERFACE Destroy_Channel_Subset
    MODULE PROCEDURE Destroy_scalar
    MODULE PROCEDURE Destroy_rank1
  END INTERFACE Destroy_Channel_Subset


  ! -----------------
  ! Module Parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  INTEGER, PARAMETER :: SET = 1
  INTEGER, PARAMETER :: INVALID = -1
  INTEGER, PARAMETER :: ML = 256


  ! -----------------------------------
  ! Channel subset data type definition
  ! -----------------------------------
  TYPE :: Channel_Subset_type
    INTEGER :: n_Allocates = 0
    ! Dimensions
    INTEGER :: n_Channels = 0
    ! Channel subset inforamtion
    INTEGER, POINTER :: Channel_Number(:) => NULL()
    INTEGER, POINTER :: Channel_Index(:)  => NULL()
  END TYPE Channel_Subset_type


CONTAINS


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
!       Associated_Channel_Subset
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       Channel Subset structure.
!
! CALLING SEQUENCE:
!       Association_Status = Associated_Channel_Subset( Subset,             &  ! Input
!                                                       ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       Subset:              Channel Subset structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       TYPE(Channel_Subset_type)
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the Channel_Subset
!                            structure pointer members are associated.
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
!                            association status of the Channel_Subset pointer members.
!                            .TRUE.  - if ALL the Channel_Subset pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the Channel_Subset pointer
!                                      members are associated.
!                            .FALSE. - some or all of the Channel_Subset pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION Associated_Channel_Subset( Subset  , & ! Input
                                      ANY_Test) & ! Optional input
                                    RESULT( Association_Status )
    ! Arguments
    TYPE(Channel_Subset_type), INTENT(IN) :: Subset
    INTEGER,      OPTIONAL, INTENT(IN) :: ANY_Test
    ! Function result
    LOGICAL :: Association_Status
    ! Local variables
    LOGICAL :: ALL_Test

    ! Default is to test ALL the pointer members
    ! for a true association status....
    ALL_Test = .TRUE.
    ! ...unless the ANY_Test argument is set.
    IF ( PRESENT( ANY_Test ) ) THEN
      IF ( ANY_Test == SET ) ALL_Test = .FALSE.
    END IF
    
    ! Test the structure associations    
    Association_Status = .FALSE.
    IF ( ALL_Test ) THEN
      IF ( ASSOCIATED(Subset%Channel_Number) .AND. &
           ASSOCIATED(Subset%Channel_Index )) THEN
        Association_Status = .TRUE.
      END IF
    ELSE
      IF ( ASSOCIATED(Subset%Channel_Number) .OR. &
           ASSOCIATED(Subset%Channel_Index )) THEN
        Association_Status = .TRUE.
      END IF
    END IF

  END FUNCTION Associated_Channel_Subset


!------------------------------------------------------------------------------
!
! NAME:
!       Destroy_Channel_Subset
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of
!       Channel_Subset data structures.
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_Channel_Subset( Subset,                , &  ! Output
!                                              RCS_Id     =RCS_Id     , &  ! Optional output
!                                              Message_Log=Message_Log  )  ! Error messaging
!
! OUTPUT ARGUMENTS:
!       Subset:        Re-initialized Channel_Subset structure.
!                      UNITS:      N/A
!                      TYPE:       TYPE(Channel_Subset_type)
!                      DIMENSION:  Scalar
!                                    OR
!                                  Rank-1
!                      ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the Message_Handler module.
!                      If == SUCCESS the structure re-initialisation was successful
!                         == FAILURE - an error occurred, or
!                                    - the structure internal allocation counter
!                                      is not equal to zero (0) upon exiting this
!                                      function. This value is incremented and
!                                      decremented for every structure allocation
!                                      and deallocation respectively.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output Subset argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Destroy_scalar( Subset      , &  ! Output
                           No_Clear    , &  ! Optional input
                           RCS_Id      , &  ! Optional output
                           Message_Log ) &  ! Error messaging
                         RESULT( Error_Status )
    ! Arguments
    TYPE(Channel_Subset_type), INTENT(IN OUT) :: Subset
    INTEGER     ,    OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*),    OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),    OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Destroy_Channel_Subset(scalar)'
    ! Local variables
    CHARACTER(256)  :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
    
    ! Reset the dimension indicators
    Subset%n_Channels = 0

    ! Default is to clear scalar members...
    Clear = .TRUE.
    ! ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == SET ) Clear = .FALSE.
    END IF
    IF ( Clear ) CALL Clear_Channel_Subset(Subset)
    
    ! If ALL pointer members are NOT associated, do nothing
    IF ( .NOT. Associated_Channel_Subset(Subset) ) RETURN
    
    
    ! Deallocate the pointer members
    ! ------------------------------
    DEALLOCATE( Subset%Channel_Number, &
                Subset%Channel_Index , &
                STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '("Error deallocating Channel_Subset. STAT = ",i0)') &
                      Allocate_Status
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Decrement and test allocation counter
    ! -------------------------------------
    Subset%n_Allocates = Subset%n_Allocates - 1
    IF ( Subset%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Allocation counter /= 0, Value = ",i0)') &
                     Subset%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM(Message), &
                            Error_Status,    &
                            Message_Log=Message_Log )
    END IF

  END FUNCTION Destroy_scalar

  FUNCTION Destroy_rank1( Subset      , &  ! Output
                          No_Clear    , &  ! Optional input
                          RCS_Id      , &  ! Optional output
                          Message_Log ) &  ! Error messaging
                        RESULT( Error_Status )
    ! Arguments
    TYPE(Channel_Subset_type), INTENT(IN OUT) :: Subset(:)
    INTEGER     ,    OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*),    OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),    OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Destroy_Channel_Subset(rank-1)'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: n
    INTEGER :: Scalar_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Loop over elements
    ! ------------------
    DO n = 1, SIZE(Subset)

      ! Call scalar function
      Scalar_Status = Destroy_scalar( Subset(n), &
                                      No_Clear   =No_Clear, &
                                      Message_Log=Message_Log )
      ! Process error, but keep going 
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message,'("Error deallocating Channel_Subset element # ",i0)' ) n
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status, &
                              Message_Log=Message_Log )
      END IF
    END DO

  END FUNCTION Destroy_rank1


!------------------------------------------------------------------------------
!
! NAME:
!       Allocate_Channel_Subset
! 
! PURPOSE:
!       Function to allocate the pointer members of the Channel_Subset
!       data structure.
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_Channel_Subset( n_Channels             , &  ! Input
!                                               Subset                 , &  ! Output
!                                               RCS_Id     =RCS_Id     , &  ! Optional output
!                                               Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Channels:         Number of channels dimension.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Subset:             Channel_Subset structure with allocated
!                           pointer members
!                           UNITS:      N/A
!                           TYPE:       TYPE(Channel_Subset_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:        Character string specifying a filename in
!                           which any messages will be logged. If not
!                           specified, or if an error occurs opening
!                           the log file, the default action is to
!                           output messages to standard output.
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
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:        The return value is an integer defining the error status.
!                            The error codes are defined in the Message_Handler module.
!                            If == SUCCESS the structure re-initialisation was successful
!                               == FAILURE - an error occurred, or
!                                          - the structure internal allocation counter
!                                            is not equal to one (1) upon exiting this
!                                            function. This value is incremented and
!                                            decremented for every structure allocation
!                                            and deallocation respectively.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output Subset argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Allocate_Channel_Subset( n_Channels  , &  ! Input
                                    Subset      , &  ! Output
                                    RCS_Id      , &  ! Optional output
                                    Message_Log ) &  ! Error messaging
                                  RESULT( Error_Status )
    ! Arguments
    INTEGER,                   INTENT(IN)     :: n_Channels
    TYPE(Channel_Subset_type), INTENT(IN OUT) :: Subset
    CHARACTER(*),    OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),    OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Allocate_Channel_Subset'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Allocate_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
    
    ! Check dimensions
    IF ( n_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Channel_Subset channel dimension must be > 0.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check if ANY pointers are already associated.
    ! If they are, deallocate them but leave scalars.
    IF ( Associated_Channel_Subset( Subset, ANY_Test=SET ) ) THEN
      Error_Status = Destroy_Channel_Subset( Subset, &
                                             No_Clear=SET, &
                                             Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating Subset pointer members.', &
                              Error_Status,    &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF


    ! Perform the pointer allocation
    ! ------------------------------
    ALLOCATE( Subset%Channel_Number(1:n_Channels), &
              Subset%Channel_Index(1:n_Channels), &
              STAT=Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Error allocating Channel_Subset data arrays. STAT = ",i0)' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Assign the dimensions
    ! ---------------------
    Subset%n_Channels = n_Channels


    ! Initialise the arrays
    ! ---------------------
    Subset%Channel_Number = INVALID
    Subset%Channel_Index  = INVALID


    ! Increment and test the allocation counter
    ! -----------------------------------------
    Subset%n_Allocates = Subset%n_Allocates + 1
    IF ( Subset%n_Allocates /= 1 ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Allocation counter /= 1, Value = ",i0)' ) &
                      Subset%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM(Message), &
                            Error_Status,    &
                            Message_Log=Message_Log )
    END IF

  END FUNCTION Allocate_Channel_Subset


!------------------------------------------------------------------------------
!
! NAME:
!       Assign_Channel_Subset
!
! PURPOSE:
!       Function to copy valid Channel_Subset structures.
!
! CALLING SEQUENCE:
!       Error_Status = Assign_Channel_Subset( Subset_in,              &  ! Input
!                                             Subset_out,             &  ! Output
!                                             RCS_Id     =RCS_Id,     &  ! Revision control
!                                             Message_Log=Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Subset_in:         Channel_Subset structure which is to be copied.
!                          UNITS:      N/A
!                          TYPE:       TYPE(Channel_Subset_type)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Subset_out:        Copy of the input structure, Subset_in.
!                          UNITS:      N/A
!                          TYPE:       TYPE(Channel_Subset_type)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:            Character string containing the Revision Control
!                          System Id field for the module.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:      The return value is an integer defining the error status.
!                          The error codes are defined in the Message_Handler module.
!                          If == SUCCESS the structure assignment was successful
!                             == FAILURE an error occurred
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output Subset argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Assign_Channel_Subset( Subset_in   , &  ! Input
                                  Subset_out  , &  ! Output
                                  RCS_Id      , &  ! Revision control
                                  Message_Log ) &  ! Error messaging
                                RESULT( Error_Status )
    ! Arguments
    TYPE(Channel_Subset_type), INTENT(IN)     :: Subset_in
    TYPE(Channel_Subset_type), INTENT(IN OUT) :: Subset_out
    CHARACTER(*),    OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),    OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assign_Channel_Subset'

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! ALL *input* pointers must be associated
    IF ( .NOT. Associated_Channel_Subset( Subset_In ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT Subset pointer members are NOT associated.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Allocate data arrays
    ! --------------------
    Error_Status = Allocate_Channel_Subset( Subset_in%n_Channels, &
                                            Subset_out, &
                                            Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error allocating output Channel_Subset arrays.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Copy array data
    ! ---------------
    Subset_out%Channel_Number = Subset_in%Channel_Number
    Subset_out%Channel_Index  = Subset_in%Channel_Index

  END FUNCTION Assign_Channel_Subset


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
!       Clear_Channel_Subset
!
! PURPOSE:
!       Subroutine to clear the scalar members of a Channel_Subset structure.
!
! CALLING SEQUENCE:
!       CALL Clear_Channel_Subset( Subset ) ! Output
!
! OUTPUT ARGUMENTS:
!       Subset:         Channel Subset structure for which the scalar members have
!                       been cleared.
!                       UNITS:      N/A
!                       TYPE:       TYPE(Channel_Subset_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the output Subset argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!----------------------------------------------------------------------------------

  SUBROUTINE Clear_Channel_Subset( Subset )
    TYPE(Channel_Subset_type), INTENT(IN OUT) :: Subset
    ! Nothing done so far
  END SUBROUTINE Clear_Channel_Subset

END MODULE Channel_Subset_Define
