!
! CRTM_Predictor_Define
!
! Module defining the CRTM Predictor structure and containing
! routines to manipulate it.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 13-May-2004
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_Predictor_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,      ONLY: fp=>fp_kind
  USE Message_Handler, ONLY: SUCCESS, FAILURE, Display_Message
  USE CRTM_Parameters, ONLY: ZERO, SET
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! CRTM_Predictor data structure definition
  PUBLIC :: CRTM_Predictor_type
  ! CRTM_Predictor structure routines
  PUBLIC :: CRTM_Associated_Predictor
  PUBLIC :: CRTM_Destroy_Predictor
  PUBLIC :: CRTM_Allocate_Predictor
  PUBLIC :: CRTM_Assign_Predictor


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_Predictor_Define.f90,v 2.1 2006/08/31 16:54:29 frpv Exp $'


  ! ----------------------------------
  ! Predictor data type definition
  ! ----------------------------------
  TYPE :: CRTM_Predictor_type
    INTEGER :: n_Allocates = 0
    ! Dimensions
    INTEGER :: n_Layers     = 0  ! K dimension
    INTEGER :: n_Predictors = 0  ! I dimension
    INTEGER :: n_Absorbers  = 0  ! J dimension
    ! Components
    REAL(fp) :: Secant_Sensor_Zenith = ZERO
    ! Integrated absorber array
    REAL(fp), DIMENSION(:,:), POINTER :: A => NULL()   ! 0:K x J
    ! Predictor array
    REAL(fp), DIMENSION(:,:), POINTER :: X => NULL()   ! I x K
  END TYPE CRTM_Predictor_type


CONTAINS


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
!       CRTM_Clear_Predictor
!
! PURPOSE:
!       Subroutine to clear the scalar members of a CRTM_Predictor structure.
!
! CALLING SEQUENCE:
!       CALL CRTM_Clear_Predictor( Predictor ) ! Output
!
! OUTPUT ARGUMENTS:
!       Predictor:  CRTM_Predictor structure for which the scalar
!                   members have been cleared.
!                   UNITS:      N/A
!                   TYPE:       CRTM_Predictor_type
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the output Predictor argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!----------------------------------------------------------------------------------

  SUBROUTINE CRTM_Clear_Predictor( Predictor )
    TYPE(CRTM_Predictor_type), INTENT(IN OUT) :: Predictor
    Predictor%n_Layers     = 0
    Predictor%n_Predictors = 0
    Predictor%n_Absorbers  = 0
    Predictor%Secant_Sensor_Zenith = ZERO
  END SUBROUTINE CRTM_Clear_Predictor





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
!       CRTM_Associated_Predictor
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       Predictor structure.
!
! CALLING SEQUENCE:
!       Association_Status = CRTM_Associated_Predictor( Predictor,        &  ! Input
!                                                       ANY_Test=Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       Predictor:           CRTM_Predictor structure which is to have its
!                            pointer member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       CRTM_Predictor_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            CRTM_Predictor structure pointer members are
!                            associated.
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
!       Association_Status:  The return value is a logical value indicating
!                            the association status of the CRTM_Predictor
!                            pointer members.
!                            .TRUE.  - if ALL the CRTM_Predictor pointer
!                                      members are associated, or if the
!                                      ANY_Test argument is set and ANY of the
!                                      CRTM_Predictor pointer members are
!                                      associated.
!                            .FALSE. - some or all of the CRTM_Predictor
!                                      pointer members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Associated_Predictor( Predictor, & ! Input
                                      ANY_Test ) & ! Optional input
                                    RESULT( Association_Status )
    ! Arguments
    TYPE(CRTM_Predictor_type), INTENT(IN) :: Predictor
    INTEGER,         OPTIONAL, INTENT(IN) :: ANY_Test
    ! Function result
    LOGICAL :: Association_Status
    ! Local variables
    LOGICAL :: ALL_Test


    ! ------
    ! Set up
    ! ------
    ! Default is to test ALL the pointer members
    ! for a true association status....
    ALL_Test = .TRUE.
    ! ...unless the ANY_Test argument is set.
    IF ( PRESENT( ANY_Test ) ) THEN
      IF ( ANY_Test == SET ) ALL_Test = .FALSE.
    END IF


    ! ---------------------------------------------
    ! Test the structure pointer member association
    ! ---------------------------------------------
    Association_Status = .FALSE.
    IF ( ALL_Test ) THEN
      IF (ASSOCIATED(Predictor%A) .AND. &
          ASSOCIATED(Predictor%X)       ) THEN
        Association_Status = .TRUE.
      END IF
    ELSE
      IF (ASSOCIATED(Predictor%A) .OR. &
          ASSOCIATED(Predictor%X)      ) THEN
        Association_Status = .TRUE.
      END IF
    END IF

  END FUNCTION CRTM_Associated_Predictor


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Destroy_Predictor
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of
!       a CRTM_Predictor data structure.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_Predictor( Predictor,              &  ! Output
!                                              RCS_Id=RCS_Id,          &  ! Revision control
!                                              Message_Log=Message_Log )  ! Error messaging
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Predictor:      Re-initialized CRTM_Predictor structure.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Predictor_type
!                       DIMENSION:  Scalar OR Rank-1 array
!                       ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:         Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the structure re-initialisation was successful
!                          == FAILURE - an error occurred, or
!                                     - the structure internal allocation counter
!                                       is not equal to zero (0) upon exiting this
!                                       function. This value is incremented and
!                                       decremented for every structure allocation
!                                       and deallocation respectively.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output Predictor argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Destroy_Predictor( Predictor,    &  ! Output
                                   No_Clear,     &  ! Optional input
                                   RCS_Id,       &  ! Revision control
                                   Message_Log ) &  ! Error messaging
                                 RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Predictor_type), INTENT(IN OUT) :: Predictor
    INTEGER,         OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*),    OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),    OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Predictor'
    ! Local variables
    CHARACTER( 256 ) :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Default is to clear scalar members...
    Clear = .TRUE.
    ! ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == SET ) Clear = .FALSE.
    END IF

    ! Initialise the scalar members
    IF (Clear) CALL CRTM_Clear_Predictor(Predictor)

    ! If ALL pointer members are NOT associated, do nothing
    IF ( .NOT. CRTM_Associated_Predictor(Predictor) ) RETURN


    ! Deallocate the pointer members
    DEALLOCATE( Predictor%A, &
                Predictor%X, &
                STAT=Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error deallocating CRTM_Predictor structure. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM(Message), &
                            Error_Status,    &
                            Message_Log=Message_Log )
    END IF

    ! Decrement and test allocation counter
    Predictor%n_Allocates = Predictor%n_Allocates - 1
    IF ( Predictor%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      Predictor%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM(Message), &
                            Error_Status,    &
                            Message_Log=Message_Log )
    END IF

  END FUNCTION CRTM_Destroy_Predictor


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Allocate_Predictor
! 
! PURPOSE:
!       Function to allocate the pointer members of the CRTM_Predictor
!       data structure.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Allocate_Predictor( n_Layers,               &  ! Input
!                                               n_Predictors,           &  ! Input
!                                               n_Absorbers,            &  ! Input
!                                               Predictor,              &  ! Output
!                                               RCS_Id=RCS_Id,          &  ! Revision control
!                                               Message_Log=Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!         n_Layers:          Number of atmospheric layers.
!                            Must be > 0
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
!         n_Predictors:      Number of absorption predictors.
!                            Must be > 0
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
!         n_Absorbers:       Number of atmospheric absorbers.
!                            Must be > 0
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:         Character string specifying a filename in which any
!                            messages will be logged. If not specified, or if an
!                            error occurs opening the log file, the default action
!                            is to output messages to standard output.
!                            UNITS:      N/A
!                            TYPE:       CHARACTER(*)
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Predictor:           CRTM_Predictor structure with allocated pointer members
!                            UNITS:      N/A
!                            TYPE:       CRTM_Predictor_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN OUT)
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:              Character string containing the Revision Control
!                            System Id field for the module.
!                            UNITS:      N/A
!                            TYPE:       CHARACTER(*)
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(OUT), OPTIONAL
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
!       Note the INTENT on the output Predictor argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Allocate_Predictor( n_Layers,     &  ! Input
                                    n_Predictors, &  ! Input
                                    n_Absorbers,  &  ! Input
                                    Predictor,    &  ! Output
                                    RCS_Id,       &  ! Revision control
                                    Message_Log ) &  ! Error messaging
                                  RESULT( Error_Status )
    ! Arguments
    INTEGER,                   INTENT(IN)     :: n_Layers
    INTEGER,                   INTENT(IN)     :: n_Predictors
    INTEGER,                   INTENT(IN)     :: n_Absorbers
    TYPE(CRTM_Predictor_type), INTENT(IN OUT) :: Predictor
    CHARACTER(*),    OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),    OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Predictor'
    ! Local variables
    CHARACTER( 256 ) :: Message
    INTEGER :: Allocate_Status

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Dimensions
    IF ( n_Layers     < 1 .OR. &
         n_Predictors < 1 .OR. &
         n_Absorbers  < 1      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input CRTM_Predictor dimensions must be > 0.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    IF ( CRTM_Associated_Predictor(Predictor,ANY_Test=1) ) THEN
      Error_Status = CRTM_Destroy_Predictor( Predictor, &
                                             No_Clear=1, &
                                             Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating CRTM_Predictor prior to allocation.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF


    ! Perform the allocation
    ALLOCATE( Predictor%A(0:n_Layers,n_Absorbers), &
              Predictor%X(n_Predictors,n_Layers) , &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating Predictor data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Assign the dimensions and initalise arrays
    Predictor%n_Layers     = n_Layers
    Predictor%n_Predictors = n_Predictors
    Predictor%n_Absorbers  = n_Absorbers
    Predictor%A            = ZERO
    Predictor%X            = ZERO

    ! Increment and test allocation counter
    Predictor%n_Allocates = Predictor%n_Allocates + 1
    IF ( Predictor%n_Allocates /= 1 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      Predictor%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION CRTM_Allocate_Predictor


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Assign_Predictor
!
! PURPOSE:
!       Function to copy valid CRTM_Predictor structures.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Assign_Predictor( Predictor_in,             &  ! Input
!                                             Predictor_out,            &  ! Output
!                                             RCS_Id      = RCS_Id,     &  ! Revision control
!                                             Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Predictor_in:      CRTM_Predictor structure which is to be copied.
!                          UNITS:      N/A
!                          TYPE:       CRTM_Predictor_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
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
! OUTPUT ARGUMENTS:
!       Predictor_out:     Copy of the input structure, CRTM_Predictor_in.
!                          UNITS:      N/A
!                          TYPE:       CRTM_Predictor_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN OUT)
!
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
!       Note the INTENT on the output AtmScatter argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Assign_Predictor( Predictor_in,  &  ! Input
                                  Predictor_out, &  ! Output
                                  RCS_Id,        &  ! Revision control
                                  Message_Log )  &  ! Error messaging
                                RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Predictor_type), INTENT(IN)     :: Predictor_in
    TYPE(CRTM_Predictor_type), INTENT(IN OUT) :: Predictor_out
    CHARACTER(*),    OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),    OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_Predictor'

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! ALL *input* pointers must be associated.
    IF ( .NOT. CRTM_Associated_Predictor(Predictor_In) ) THEN
      ! Destroy the output structure
      Error_Status = CRTM_Destroy_Predictor(Predictor_Out, &
                                            Message_Log=Message_Log)
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error deallocating output CRTM_Predictor pointer members.', &
                              Error_Status, &
                              Message_Log=Message_Log )
      END IF
      ! Inform user of unassociated inputs
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT CRTM_Predictor pointer '//&
                            'members are NOT associated.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Allocate the structure
    Error_Status = CRTM_Allocate_Predictor( Predictor_in%n_Layers, &
                                            Predictor_in%n_Predictors, &
                                            Predictor_in%n_Absorbers, &
                                            Predictor_out, &
                                            Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output CRTM_Predictor arrays.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Assign non-dimension scalar members
    Predictor_out%Secant_Sensor_Zenith = Predictor_in%Secant_Sensor_Zenith

    ! Assign array data
    Predictor_out%A = Predictor_in%A
    Predictor_out%X = Predictor_in%X

  END FUNCTION CRTM_Assign_Predictor

END MODULE CRTM_Predictor_Define
