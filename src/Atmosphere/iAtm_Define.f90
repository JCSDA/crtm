!
! iAtm_Define
!
! Module for defining the Atmosphere module internal structure
! and associated procedures.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 07-Apr-2009
!                       paul.vandelst@noaa.gov
!

MODULE iAtm_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds     , ONLY: fp
  USE Message_Handler, ONLY: SUCCESS, FAILURE, Display_Message
  USE CRTM_Parameters, ONLY: ZERO, SET
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Public structures
  PUBLIC :: iAtm_type
  ! Public procedures
  PUBLIC :: Associated_iAtm
  PUBLIC :: Destroy_iAtm
  PUBLIC :: Allocate_iAtm
  PUBLIC :: Assign_iAtm


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id$'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256


  ! --------------------
  ! Structure definition
  ! --------------------
  !:tdoc+:
  TYPE :: iAtm_type
    INTEGER :: n_Allocates = 0
    ! Dimensions
    INTEGER :: n_Layers    = 0  ! K dimension
    INTEGER :: n_Absorbers = 0  ! J dimension
    ! Level arrays
    REAL(fp), POINTER :: pl(:)   => NULL()  ! 0:K
    REAL(fp), POINTER :: tl(:)   => NULL()  ! 0:K
    REAL(fp), POINTER :: al(:,:) => NULL()  ! 0:K x J
    ! Layer arrays
    REAL(fp), POINTER :: p(:)   => NULL()  ! K
    REAL(fp), POINTER :: t(:)   => NULL()  ! K
    REAL(fp), POINTER :: a(:,:) => NULL()  ! K x J
    ! Save variables
    REAL(fp)          :: pln_save = ZERO
    REAL(fp)          :: tln_save = ZERO
    REAL(fp), POINTER :: aln_save(:) => NULL()  ! J
    REAL(fp)          :: plint_save = ZERO
    REAL(fp)          :: tlint_save = ZERO
    REAL(fp), POINTER :: alint_save(:) => NULL()  ! J
    REAL(fp), POINTER :: a_save(:,:)   => NULL()  ! K x J
    ! Interpolating polynomials
    REAL(fp) :: ilpoly = ZERO  ! Interpolating polynomial for extra levels to user Pl(0)
    REAL(fp) :: elpoly = ZERO  ! Extrapolating polynomial for user "layer 0" values
  END TYPE iAtm_type
  !:tdoc-:
  
  
CONTAINS


!##################################################################################
!##################################################################################
!##
!##                          ## PUBLIC MODULE ROUTINES ##
!##
!##################################################################################
!##################################################################################

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Associated_iAtm
!
! PURPOSE:
!       Function to test the association status of the components of a
!       CRTM iAtm structure.
!
! CALLING SEQUENCE:
!       Association_Status = Associated_iAtm( iAtm, ANY_Test=Any_Test )
!
! INPUT ARGUMENTS:
!       iAtm:                Structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       iAtm_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            structure components are associated.
!                            The default is to test if ALL the components
!                            are associated.
!                            If ANY_Test = 0, test if ALL the components
!                                             are associated.  (DEFAULT)
!                               ANY_Test = 1, test if ANY of the components
!                                             are associated.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN), OPTIONAL
!
!
! FUNCTION RESULT:
!       Association_Status:  The return value is a logical value indicating the
!                            association status of the structure components.
!                            .TRUE.  - if ALL the structure components are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the structure pointer
!                                      members are associated.
!                            .FALSE. - some or all of the structre pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Associated_iAtm( iAtm    , & ! Input
                            ANY_Test) & ! Optional input
                          RESULT( Association_Status )
    ! Arguments
    TYPE(iAtm_type)  , INTENT(IN) :: iAtm
    INTEGER, OPTIONAL, INTENT(IN) :: ANY_Test
    ! Function result
    LOGICAL :: Association_Status
    ! Local variables
    LOGICAL :: ALL_Test

    ! Set up
    ! ...Default is to test ALL the components
    ALL_Test = .TRUE.
    ! ...unless the ANY_Test argument is set.
    IF ( PRESENT(ANY_Test) ) THEN
      IF ( ANY_Test == SET ) ALL_Test = .FALSE.
    END IF

    ! ...Initialise a result
    Association_Status = .FALSE.


    ! Test the structure components
    IF ( ALL_Test ) THEN
      IF ( ASSOCIATED(iAtm%pl        ) .AND. &
           ASSOCIATED(iAtm%tl        ) .AND. &
           ASSOCIATED(iAtm%al        ) .AND. &
           ASSOCIATED(iAtm%p         ) .AND. &
           ASSOCIATED(iAtm%t         ) .AND. &
           ASSOCIATED(iAtm%a         ) .AND. &
           ASSOCIATED(iAtm%aln_save  ) .AND. &
           ASSOCIATED(iAtm%alint_save) .AND. &
           ASSOCIATED(iAtm%a_save    ) ) THEN
        Association_Status = .TRUE.
      END IF
    ELSE
      IF ( ASSOCIATED(iAtm%pl        ) .OR. &
           ASSOCIATED(iAtm%tl        ) .OR. &
           ASSOCIATED(iAtm%al        ) .OR. &
           ASSOCIATED(iAtm%p         ) .OR. &
           ASSOCIATED(iAtm%t         ) .OR. &
           ASSOCIATED(iAtm%a         ) .OR. &
           ASSOCIATED(iAtm%aln_save  ) .OR. &
           ASSOCIATED(iAtm%alint_save) .OR. &
           ASSOCIATED(iAtm%a_save    ) ) THEN
        Association_Status = .TRUE.
      END IF
    END IF

  END FUNCTION Associated_iAtm


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Destroy_iAtm
! 
! PURPOSE:
!       Function to re-initialize iAtm data structures.
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_iAtm( iAtm, Message_Log=Message_Log  )
!
! OUTPUT ARGUMENTS:
!       iAtm:         Re-initialized iAtm structure.
!                     UNITS:      N/A
!                     TYPE:       iAtm_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
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
!       Note the INTENT on the output structure argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Destroy_iAtm( iAtm       , &  ! Output
                         Message_Log) &  ! Error messaging
                       RESULT( Error_Status )
    ! Arguments
    TYPE(iAtm_type),        INTENT(IN OUT) :: iAtm
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Destroy_iAtm'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Allocate_Status

    ! Set up
    Error_Status = SUCCESS
    
    ! ...Re-initialise the dimensions
    iAtm%n_Layers    = 0
    iAtm%n_Absorbers = 0

    ! ...Clear scalar members
    CALL Clear_iAtm( iAtm )

    ! ...If ALL components are NOT associated, do nothing
    IF ( .NOT. Associated_iAtm( iAtm ) ) RETURN


    ! Deallocate the array components
    DEALLOCATE( iAtm%pl, iAtm%tl, iAtm%al, &
                iAtm%p , iAtm%t , iAtm%a , &
                iAtm%aln_save, iAtm%alint_save, iAtm%a_save, &
                STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE(Message,'("Error deallocating iAtm components. STAT = ",i0)') &
                    Allocate_Status
      CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
      RETURN
    END IF


    ! Decrement and test allocation counter
    iAtm%n_Allocates = iAtm%n_Allocates - 1
    IF ( iAtm%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i0 )' ) &
                      iAtm%n_Allocates
      CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
    END IF

  END FUNCTION Destroy_iAtm


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Allocate_iAtm
! 
! PURPOSE:
!       Function to allocate CRTM iAtm data structures.
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_iAtm( n_Layers               , &
!                                     n_Absorbers            , &
!                                     iAtm                   , &
!                                     Message_Log=Message_Log  )
!
! INPUT ARGUMENTS:
!       n_Layers:     Number of layers dimension.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       n_Absorbers:  Number of absorbers dimension.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       iAtm:         Structure with allocated components.
!                     UNITS:      N/A
!                     TYPE:       iAtm_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the structure allocation was successful
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
!       Note the INTENT on the output structure argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Allocate_iAtm( n_Layers   , &  ! Input
                          n_Absorbers, &  ! Input
                          iAtm       , &  ! Output
                          Message_Log) &  ! Error messaging
                        RESULT( Error_Status )
    ! Arguments
    INTEGER               , INTENT(IN)     :: n_Layers    
    INTEGER               , INTENT(IN)     :: n_Absorbers 
    TYPE(iAtm_type)       , INTENT(IN OUT) :: iAtm
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Allocate_iAtm'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Allocate_Status

    ! Set up
    Error_Status = SUCCESS
    
    ! ...Check dimensions
    IF ( n_Layers    < 1 .OR. &
         n_Absorbers < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Layers and n_Absorbers must be > 0.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    
    ! ...Deallocate if ANY pointers are associated
    IF ( Associated_iAtm( iAtm, ANY_Test=SET ) ) THEN
      Error_Status = Destroy_iAtm( iAtm, Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating iAtm components.', &
                              Error_Status,    &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF


    ! Allocate the array components
    ALLOCATE( iAtm%pl(0:n_Layers), iAtm%tl(0:n_Layers), iAtm%al(0:n_Layers, 1:n_Absorbers), &
              iAtm%p(1:n_Layers) , iAtm%t(1:n_Layers) , iAtm%a(1:n_Layers, 1:n_Absorbers) , &
              iAtm%aln_save(1:n_Absorbers), &
              iAtm%alint_save(1:n_Absorbers), &
              iAtm%a_save(1:n_Layers,1:n_Absorbers), &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE(Message,'("Error allocating iAtm components. STAT = ",i0)') Allocate_Status
      CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
      RETURN
    END IF


    ! Assign the dimensions
    iAtm%n_Layers    = n_Layers
    iAtm%n_Absorbers = n_Absorbers


    ! Increment and test allocation counter
    iAtm%n_Allocates = iAtm%n_Allocates + 1
    IF ( iAtm%n_Allocates /= 1 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i0 )' ) iAtm%n_Allocates
      CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
    END IF

  END FUNCTION Allocate_iAtm
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Assign_iAtm
! 
! PURPOSE:
!       Function to copy a CRTM iAtm data structures.
!
! CALLING SEQUENCE:
!       Error_Status = Assign_iAtm( iAtm_in                , &
!                                   iAtm_out               , &
!                                   Message_Log=Message_Log  )
!
! INPUT ARGUMENTS:
!       iAtm_in:      Structure to be copied.
!                     UNITS:      N/A
!                     TYPE:       iAtm_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       iAtm_out:     Copy of input iAtm_in structure.
!                     UNITS:      N/A
!                     TYPE:       iAtm_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the structure copy was successful
!                        == FAILURE an error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output structure argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Assign_iAtm( iAtm_in    , &  ! Input
                        iAtm_out   , &  ! Output
                        Message_Log) &  ! Error messaging
                      RESULT( Error_Status )
    ! Arguments
    TYPE(iAtm_type)       , INTENT(IN)     :: iAtm_in
    TYPE(iAtm_type)       , INTENT(IN OUT) :: iAtm_out
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assign_iAtm'

    ! Set up
    Error_Status = SUCCESS
    ! ...ALL *input* pointers must be associated
    IF ( .NOT. Associated_iAtm( iAtm_in ) ) THEN
      Error_Status = Destroy_iAtm( iAtm_out, Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error deallocating output iAtm components.', &
                              Error_Status, &
                              Message_Log=Message_Log )
      END IF
      RETURN
    END IF
    

    ! Allocate the structure
    Error_Status = Allocate_iAtm( iAtm_in%n_Layers, &
                                  iAtm_in%n_Absorbers, &
                                  iAtm_out, &
                                  Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output iAtm arrays.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Assign data
    iAtm_out%pl         = iAtm_in%pl
    iAtm_out%tl         = iAtm_in%tl
    iAtm_out%al         = iAtm_in%al
    iAtm_out%p          = iAtm_in%p
    iAtm_out%t          = iAtm_in%t
    iAtm_out%a          = iAtm_in%a
    iAtm_out%pln_save   = iAtm_in%pln_save
    iAtm_out%tln_save   = iAtm_in%tln_save
    iAtm_out%aln_save   = iAtm_in%aln_save
    iAtm_out%plint_save = iAtm_in%plint_save
    iAtm_out%tlint_save = iAtm_in%tlint_save
    iAtm_out%alint_save = iAtm_in%alint_save
    iAtm_out%a_save     = iAtm_in%a_save
    iAtm_out%ilpoly     = iAtm_in%ilpoly
    iAtm_out%elpoly     = iAtm_in%elpoly

  END FUNCTION Assign_iAtm


!##################################################################################
!##################################################################################
!##
!##                          ## PRIVATE MODULE ROUTINES ##
!##
!##################################################################################
!##################################################################################

!----------------------------------------------------------------------------------
!
! NAME:
!       Clear_iAtm
!
! PURPOSE:
!       Subroutine to clear the scalar members of a CRTM iAtm structure.
!
! CALLING SEQUENCE:
!       CALL Clear_iAtm( iAtm )
!
! OUTPUT ARGUMENTS:
!       iAtm:  iAtm structure for which the scalar members have
!              been cleared.
!              UNITS:      N/A
!              TYPE:       iAtm_type
!              DIMENSION:  Scalar
!              ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the output structure argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!----------------------------------------------------------------------------------

  SUBROUTINE Clear_iAtm( iAtm )
    TYPE(iAtm_type), INTENT(IN OUT) :: iAtm
    iAtm%pln_save = ZERO
    iAtm%tln_save = ZERO
    iAtm%plint_save = ZERO
    iAtm%tlint_save = ZERO
    iAtm%ilpoly = ZERO
    iAtm%elpoly = ZERO
  END SUBROUTINE Clear_iAtm

END MODULE iAtm_Define

