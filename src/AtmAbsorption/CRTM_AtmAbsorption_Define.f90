!
! CRTM_AtmAbsorption_Define
!
! Module defining the CRTM AtmAbsorption structure and containing
! routines to manipulate it.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 13-May-2004
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_AtmAbsorption_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,      ONLY: fp
  USE Message_Handler, ONLY: SUCCESS, FAILURE, Display_Message
  USE CRTM_Parameters, ONLY: ZERO, SET
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! CRTM_AtmAbsorption data structure definition
  PUBLIC :: CRTM_AtmAbsorption_type
  ! CRTM_AtmAbsorption structure routines
  PUBLIC :: CRTM_Associated_AtmAbsorption
  PUBLIC :: CRTM_Destroy_AtmAbsorption
  PUBLIC :: CRTM_Allocate_AtmAbsorption
  PUBLIC :: CRTM_Assign_AtmAbsorption


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &


  ! ----------------------------------
  ! AtmAbsorption data type definition
  ! ----------------------------------
  TYPE :: CRTM_AtmAbsorption_type
    INTEGER :: n_Allocates = 0
    ! Dimensions
    INTEGER :: n_Layers = 0  ! K dimension
    ! Structure members
    REAL(fp), DIMENSION(:), POINTER :: Optical_Depth => NULL() ! K
  END TYPE CRTM_AtmAbsorption_type


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
!       CRTM_Clear_AtmAbsorption
!
! PURPOSE:
!       Subroutine to clear the scalar members of a CRTM_AtmAbsorption structure.
!
! CALLING SEQUENCE:
!       CALL CRTM_Clear_AtmAbsorption( AtmAbsorption ) ! Output
!
! OUTPUT ARGUMENTS:
!       AtmAbsorption:  CRTM_AtmAbsorption structure for which the scalar
!                       members have been cleared.
!                       UNITS:      N/A
!                       TYPE:       CRTM_AtmAbsorption_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the output AtmAbsorption argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!----------------------------------------------------------------------------------

  SUBROUTINE CRTM_Clear_AtmAbsorption( AtmAbsorption )
    TYPE(CRTM_AtmAbsorption_type), INTENT(IN OUT) :: AtmAbsorption
    AtmAbsorption%n_Layers     = 0
  END SUBROUTINE CRTM_Clear_AtmAbsorption





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
!       CRTM_Associated_AtmAbsorption
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       AtmAbsorption structure.
!
! CALLING SEQUENCE:
!       Association_Status = CRTM_Associated_AtmAbsorption( AtmAbsorption,      &  ! Input
!                                                           ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       AtmAbsorption:       CRTM_AtmAbsorption structure which is to have its
!                            pointer member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       CRTM_AtmAbsorption_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            CRTM_AtmAbsorption structure pointer members are
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
!                            the association status of the CRTM_AtmAbsorption
!                            pointer members.
!                            .TRUE.  - if ALL the CRTM_AtmAbsorption pointer
!                                      members are associated, or if the
!                                      ANY_Test argument is set and ANY of the
!                                      CRTM_AtmAbsorption pointer members are
!                                      associated.
!                            .FALSE. - some or all of the CRTM_AtmAbsorption
!                                      pointer members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Associated_AtmAbsorption( AtmAbsorption, & ! Input
                                          ANY_Test )     & ! Optional input
                                        RESULT( Association_Status )
    ! Arguments
    TYPE(CRTM_AtmAbsorption_type), INTENT(IN) :: AtmAbsorption
    INTEGER,             OPTIONAL, INTENT(IN) :: ANY_Test
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


    ! Test the structure pointer member association
!    Association_Status = .FALSE.
!    IF ( ALL_Test ) THEN
!      IF ( ASSOCIATED( AtmAbsorption%Optical_Depth ) .AND. &
!           ASSOCIATED( AtmAbsorption%othercomp1    ) .AND. &
!           ASSOCIATED( AtmAbsorption%othercomp2    )       ) THEN
!        Association_Status = .TRUE.
!      END IF
!    ELSE
!      IF ( ASSOCIATED( AtmAbsorption%Optical_Depth ) .OR. &
!           ASSOCIATED( AtmAbsorption%othercomp1    ) .OR. &
!           ASSOCIATED( AtmAbsorption%othercomp2    )      ) THEN
!        Association_Status = .TRUE.
!      END IF
!    END IF

    ! Code for current single component test
    Association_Status = .FALSE.
    IF ( ASSOCIATED( AtmAbsorption%Optical_Depth ) ) Association_Status = .TRUE.

  END FUNCTION CRTM_Associated_AtmAbsorption


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Destroy_AtmAbsorption
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of
!       a CRTM_AtmAbsorption data structure.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_AtmAbsorption( AtmAbsorption,            &  ! Output
!                                                  RCS_Id = RCS_Id,          &  ! Revision control
!                                                  Message_Log = Message_Log )  ! Error messaging
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
!       AtmAbsorption:  Re-initialized CRTM_AtmAbsorption structure.
!                       UNITS:      N/A
!                       TYPE:       CRTM_AtmAbsorption_type
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
!       Note the INTENT on the output AtmAbsorption argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Destroy_AtmAbsorption( AtmAbsorption, &  ! Output
                                       No_Clear,      &  ! Optional input
                                       RCS_Id,        &  ! Revision control
                                       Message_Log )  &  ! Error messaging
                                     RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_AtmAbsorption_type), INTENT(IN OUT) :: AtmAbsorption
    INTEGER,             OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*),        OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),        OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_AtmAbsorption'
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
    IF ( Clear ) CALL CRTM_Clear_AtmAbsorption( AtmAbsorption )

    ! If ALL pointer members are NOT associated, do nothing
    IF ( .NOT. CRTM_Associated_AtmAbsorption( AtmAbsorption ) ) RETURN

    ! Deallocate the pointer members
    DEALLOCATE( AtmAbsorption%Optical_Depth, &
                STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error deallocating CRTM_AtmAbsorption structure. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,  &
                            TRIM(Message), &
                            Error_Status,  &
                            Message_Log=Message_Log )
    END IF

    ! Decrement and test allocation counter
    AtmAbsorption%n_Allocates = AtmAbsorption%n_Allocates - 1
    IF ( AtmAbsorption%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      AtmAbsorption%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION CRTM_Destroy_AtmAbsorption


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Allocate_AtmAbsorption
! 
! PURPOSE:
!       Function to allocate the pointer members of the CRTM_AtmAbsorption
!       data structure.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Allocate_AtmAbsorption( n_Layers,                 &  ! Input
!                                                   AtmAbsorption,            &  ! Output
!                                                   RCS_Id = RCS_Id,          &  ! Revision control
!                                                   Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!         n_Layers:          Number of atmospheric layers.
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
!       AtmAbsorption:       CRTM_AtmAbsorption structure with allocated pointer members
!                            UNITS:      N/A
!                            TYPE:       CRTM_AtmAbsorption_type
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
!       Note the INTENT on the output AtmAbsorption argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Allocate_AtmAbsorption( n_Layers,         &  ! Input
                                        AtmAbsorption,    &  ! Output
                                        RCS_Id,           &  ! Revision control
                                        Message_Log )     &  ! Error messaging
                                      RESULT( Error_Status )
    ! Arguments
    INTEGER,                       INTENT(IN)     :: n_Layers
    TYPE(CRTM_AtmAbsorption_type), INTENT(IN OUT) :: AtmAbsorption
    CHARACTER(*),        OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),        OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_AtmAbsorption'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: Allocate_Status

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Dimensions
    IF ( n_Layers < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Layers must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    IF ( CRTM_Associated_AtmAbsorption( AtmAbsorption, ANY_Test = SET ) ) THEN
      Error_Status = CRTM_Destroy_AtmAbsorption( AtmAbsorption, &
                                                 No_Clear = SET, &
                                                 Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating CRTM_AtmAbsorption pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF

    ! Perform the allocation
    ALLOCATE( AtmAbsorption%Optical_Depth( n_Layers ), &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating AtmAbsorption data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Assign the dimensions and initalise arrays
    AtmAbsorption%n_Layers      = n_Layers
    AtmAbsorption%Optical_Depth = ZERO

    ! Increment and test allocation counter
    AtmAbsorption%n_Allocates = AtmAbsorption%n_Allocates + 1
    IF ( AtmAbsorption%n_Allocates /= 1 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      AtmAbsorption%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION CRTM_Allocate_AtmAbsorption


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Assign_AtmAbsorption
!
! PURPOSE:
!       Function to copy valid CRTM_AtmAbsorption structures.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Assign_AtmAbsorption( AtmAbsorption_in,       &  ! Input
!                                                 AtmAbsorption_out,      &  ! Output
!                                                 RCS_Id     =RCS_Id,     &  ! Revision control
!                                                 Message_Log=Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       AtmAbsorption_in:  CRTM_AtmAbsorption structure which is to be copied.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmAbsorption_type
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
!       AtmAbsorption_out: Copy of the input structure, CRTM_AtmAbsorption_in.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmAbsorption_type
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

  FUNCTION CRTM_Assign_AtmAbsorption( AtmAbsorption_in,  &  ! Input
                                      AtmAbsorption_out, &  ! Output
                                      RCS_Id,            &  ! Revision control
                                      Message_Log )      &  ! Error messaging
                                    RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_AtmAbsorption_type), INTENT(IN)     :: AtmAbsorption_in
    TYPE(CRTM_AtmAbsorption_type), INTENT(IN OUT) :: AtmAbsorption_out
    CHARACTER(*),        OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),        OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_AtmAbsorption'

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! ALL *input* pointers must be associated.
    ! If this test succeeds, then some or all of the
    ! input pointers are NOT associated, so destroy
    ! the output structure and return.
    IF ( .NOT. CRTM_Associated_AtmAbsorption( AtmAbsorption_In ) ) THEN
      Error_Status = CRTM_Destroy_AtmAbsorption( AtmAbsorption_Out, &
                                                 Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating output CRTM_AtmAbsorption pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
      RETURN
    END IF

    ! Allocate the structure
    Error_Status = CRTM_Allocate_AtmAbsorption( AtmAbsorption_in%n_Layers, &
                                                AtmAbsorption_out, &
                                                Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output AtmAbsorption arrays.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Assign array data
    AtmAbsorption_out%Optical_Depth = AtmAbsorption_in%Optical_Depth

  END FUNCTION CRTM_Assign_AtmAbsorption

END MODULE CRTM_AtmAbsorption_Define
