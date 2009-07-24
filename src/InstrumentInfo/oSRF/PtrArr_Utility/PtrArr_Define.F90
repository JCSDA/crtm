MODULE PtrArr_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  USE Type_Kinds     , ONLY: fp
  USE Message_Handler, ONLY: SUCCESS, FAILURE, Display_Message
  IMPLICIT NONE


  ! -----------------  
  ! Module parameters
  ! -----------------
  INTEGER , PARAMETER :: ML = 256
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  
    
  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: PtrArr_type
  PUBLIC :: PTRARR_TYPE_COMPONENT
  PUBLIC :: Create_PtrArr
  PUBLIC :: Destroy_PtrArr
#if defined ALLOC
  ! *********************************************
  ! **** START OF ALLOCATABLE COMPONENT CODE ****
  ! *********************************************
  PUBLIC :: Allocated_PtrArr


  ! ---------------
  ! Type definition
  ! ---------------
  TYPE :: PtrArr_type
    INTEGER :: n = 0
    REAL(fp), ALLOCATABLE :: Arr(:)
  END TYPE PtrArr_type
  CHARACTER(*), PARAMETER :: PTRARR_TYPE_COMPONENT = 'ALLOCATABLE'


CONTAINS


  FUNCTION Allocated_PtrArr( p )
    TYPE(PtrArr_type), INTENT(IN) :: p
    LOGICAL :: Allocated_PtrArr
    IF ( ALLOCATED( p%Arr ) ) THEN
        Allocated_PtrArr = .TRUE.
    ELSE
        Allocated_PtrArr = .FALSE.
    END IF
  END FUNCTION Allocated_PtrArr


  FUNCTION Destroy_PtrArr(p) RESULT(err_status)
    ! Arguments
    TYPE(PtrArr_type), INTENT(OUT) :: p
    ! Function result
    INTEGER :: err_status
    ! Set up
    err_status = SUCCESS
    ! Reinitialise
    p%n = 0
  END FUNCTION Destroy_PtrArr


  FUNCTION Create_PtrArr(n, p) RESULT(err_status)
    ! Arguments
    INTEGER,           INTENT(IN)  :: n
    TYPE(PtrArr_type), INTENT(OUT) :: p
    ! Function result
    INTEGER :: err_status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME='Create_PtrArr'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: alloc_status
    ! Set up
    err_status = SUCCESS
    ! Allocate
    ALLOCATE( p%Arr(n), STAT=alloc_status )
    IF ( alloc_status /= 0 ) THEN
      err_status = FAILURE
      WRITE( msg,'("Error allocating. STAT = ",i0)' ) alloc_status
      CALL Display_Message(ROUTINE_NAME, TRIM(msg), err_status)
      RETURN
    END IF
    ! Initialise
    p%n = n
    p%Arr = ZERO
  END FUNCTION Create_PtrArr


#else /* Not ALLOC */
  ! *****************************************
  ! **** START OF POINTER COMPONENT CODE ****
  ! *****************************************
  PUBLIC :: Associated_PtrArr


  ! ---------------
  ! Type definition
  ! ---------------
  TYPE :: PtrArr_type
    INTEGER :: n_Allocates = 0
    INTEGER :: n = 0
    REAL(fp), POINTER :: Arr(:) => NULL()
  END TYPE PtrArr_type
  CHARACTER(*), PARAMETER :: PTRARR_TYPE_COMPONENT = 'POINTER'


CONTAINS


  FUNCTION Associated_PtrArr( p )
    TYPE(PtrArr_type), INTENT(IN) :: p
    LOGICAL :: Associated_PtrArr
    IF ( ASSOCIATED( p%Arr ) ) THEN
        Associated_PtrArr = .TRUE.
    ELSE
        Associated_PtrArr = .FALSE.
    END IF
  END FUNCTION Associated_PtrArr


  FUNCTION Destroy_PtrArr(p) RESULT(err_status)
    ! Arguments
    TYPE(PtrArr_type), INTENT(IN OUT) :: p
    ! Function result
    INTEGER :: err_status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME='Destroy_PtrArr'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: alloc_status
    ! Set up
    err_status = SUCCESS
    ! Reinitialise
    p%n = 0
    ! Deallocate
    DEALLOCATE( p%Arr, STAT=alloc_status )
    IF ( alloc_status /= 0 ) THEN
      err_status = FAILURE
      WRITE( msg,'("Error deallocating. STAT = ",i0)' ) alloc_status
      CALL Display_Message(ROUTINE_NAME, TRIM(msg), err_status)
      RETURN
    END IF
    ! Test allocation counter
    p%n_Allocates = p%n_Allocates - 1
    IF ( p%n_Allocates /= 0 ) THEN
      err_status = FAILURE
      WRITE( msg,'("Allocation counter /= 0, Value = ",i0)' ) p%n_Allocates
      CALL Display_Message(ROUTINE_NAME, TRIM(msg), err_status)
    END IF
  END FUNCTION Destroy_PtrArr


  FUNCTION Create_PtrArr(n, p) RESULT(err_status)
    ! Arguments
    INTEGER,           INTENT(IN)     :: n
    TYPE(PtrArr_type), INTENT(IN OUT) :: p
    ! Function result
    INTEGER :: err_status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME='Create_PtrArr'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: alloc_status
    ! Set up
    err_status = SUCCESS
    ! Destroy if associated
    IF ( Associated_PtrArr(p) ) THEN
      err_status = Destroy_PtrArr(p)
      IF ( err_status /= SUCCESS ) THEN
        CALL Display_Message(ROUTINE_NAME, 'Error deallocating', err_status)
        RETURN
      END IF
    END IF
    ! Allocate
    ALLOCATE( p%Arr(n), STAT=alloc_status )
    IF ( alloc_status /= 0 ) THEN
      err_status = FAILURE
      WRITE( msg,'("Error allocating. STAT = ",i0)' ) alloc_status
      CALL Display_Message(ROUTINE_NAME, TRIM(msg), err_status)
      RETURN
    END IF
    ! Initialise
    p%n = n
    p%Arr = ZERO
    ! Test allocation counter
    p%n_Allocates = p%n_Allocates + 1
    IF ( p%n_Allocates /= 1 ) THEN
      err_status = FAILURE
      WRITE( msg,'("Allocation counter /= 1, Value = ",i0)' ) p%n_Allocates
      CALL Display_Message(ROUTINE_NAME, TRIM(msg), err_status)
    END IF
  END FUNCTION Create_PtrArr
#endif /* Not ALLOC */


!  SUBROUTINE Create(Obj, n)
!    TYPE(PtrArr_type), INTENT(OUT) :: Obj
!    INTEGER, INTENT(IN) :: n
!    IF ( .NOT. ALLOCATED(Obj%Arr) ) THEN
!      ALLOCATE( Obj%Arr(n) )
!      Obj%n = n
!    END IF
!  END SUBROUTINE Create
!  
!  
!  SUBROUTINE Destroy(Obj)
!    TYPE(PtrArr_type) :: Obj
!    IF ( ALLOCATED(Obj%Arr) ) THEN
!      DEALLOCATE(Obj%Arr)
!      Obj%n = 0
!    END IF
!  END SUBROUTINE Destroy
!
!
!  FUNCTION Assign(Obj) RESULT(Copy)
!    TYPE(PtrArr_type), INTENT(IN) :: Obj
!    TYPE(PtrArr_type) :: Copy
!    CALL Create(Copy, Obj%n)
!    Copy%Arr = Obj%Arr
!  END FUNCTION Assign


!  ELEMENTAL FUNCTION Allocated_PtrArr( Ptr )
!    TYPE(PtrArr_type), INTENT(IN) :: Ptr
!    LOGICAL :: Allocated_PtrArr
!    Allocated_PtrArr = .FALSE.
!    IF ( ALLOCATED(Ptr%Arr) ) THEN 
!      Allocated_PtrArr = .TRUE.
!    ELSE
!      Allocated_PtrArr = .FALSE.
!    END IF
!  END FUNCTION Allocated_PtrArr
!
!
!  FUNCTION Destroy_PtrArr( Ptr ) RESULT( Error_Status )
!    TYPE(PtrArr_type), INTENT(IN OUT) :: Ptr
!    INTEGER :: Error_Status
!    INTEGER :: Destroy_Status
!
!    ! Set up
!    Error_Status = SUCCESS
!    ! ...Reinitialise the dimensions
!    Ptr%n = 0
!    ! ...If members are NOT allocated, do nothing
!    IF ( .NOT. Allocated_PtrArr( Ptr ) ) RETURN
!
!
!    ! Deallocate the components
!    DEALLOCATE( Ptr%Arr, STAT=Destroy_Status )
!    IF ( Destroy_Status /= 0 ) THEN
!      Error_Status = FAILURE
!      RETURN
!    END IF
! 
! 
!    ! Decrement and test allocation counter
!    Ptr%n_Allocates = Ptr%n_Allocates - 1
!    IF ( Ptr%n_Allocates /= 0 ) Error_Status = FAILURE
!
!  END FUNCTION Destroy_PtrArr
!
!
!
!  FUNCTION Allocate_PtrArr( n, Ptr ) RESULT( Error_Status )
!    INTEGER,           INTENT(IN)     :: n
!    TYPE(PtrArr_type), INTENT(IN OUT) :: Ptr
!    INTEGER :: Error_Status
!    INTEGER :: Allocate_Status
!
!    ! Set up
!    Error_Status = SUCCESS
!    ! ...Check dimension inputs
!    IF ( n < 1 ) THEN
!      Error_Status = FAILURE
!      RETURN
!    END IF
!    ! ...Check if ANY pointers are already allocated
!    IF ( Allocated_PtrArr( Ptr ) ) THEN
!      Error_Status = Destroy_PtrArr( PtrArr )
!      IF ( Error_Status /= SUCCESS ) RETURN
!    END IF
!
!
!    ! Perform allocations
!    ALLOCATE( Ptr%Arr(n), STAT=Allocate_Status )
!    IF ( Allocate_Status /= 0 ) THEN
!      Error_Status = FAILURE
!      RETURN
!    END IF
!
!
!    ! Assign the dimensions and initialise arrays
!    Ptr%n   = n
!    Ptr%Arr = ZERO
! 
! 
!    ! Increment and test allocation counter
!    Ptr%n_Allocates = Ptr%n_Allocates + 1
!    IF ( Ptr%n_Allocates /= 1 ) Error_Status = FAILURE
!
!  END FUNCTION Allocate_PtrArr
!
!
!
!  FUNCTION Assign_PtrArr( Ptr_in, Ptr_out ) RESULT( Error_Status )
!    TYPE(PtrArr_type), INTENT(IN)     :: Ptr_in
!    TYPE(PtrArr_type), INTENT(IN OUT) :: Ptr_out
!    INTEGER :: Error_Status
!
!    ! Setup
!    Error_Status = SUCCESS
!    ! ...ALL *input* pointers must be allocated
!    IF ( .NOT. Allocated_Ptr( Ptr_In ) ) THEN
!      Error_Status = FAILURE
!      RETURN
!    END IF
!
!
!    ! Allocate the structure
!    Error_Status = Allocate_SRF( Ptr_In%n, Ptr_Out )
!    IF ( Error_Status /= SUCCESS ) RETURN
!
!
!    ! Assign data
!    Ptr_out%Arr = Ptr_in%Arr
!
!  END FUNCTION Assign_PtrArr
!
!
!
!  FUNCTION Equal_PtrArr( Ptr_LHS  , &  ! Input
!                         Ptr_RHS  , &  ! Input
!                         ULP_Scale, &  ! Optional input
!                         Check_All) &  ! Optional input
!                    RESULT( Error_Status )
!    TYPE(PtrArr_type)     , INTENT(IN)  :: Ptr_LHS
!    TYPE(PtrArr_type)     , INTENT(IN)  :: Ptr_RHS
!    INTEGER     , OPTIONAL, INTENT(IN)  :: ULP_Scale
!    LOGICAL     , OPTIONAL, INTENT(IN)  :: Check_All
!    INTEGER :: Error_Status
!    INTEGER :: ULP
!    LOGICAL :: Check_Once
!    INTEGER :: n
!
!    ! Set up
!    Error_Status = SUCCESS
!    ! ...Default precision is a single unit in last place
!    ULP = 1
!    IF ( PRESENT(ULP_Scale) ) ULP = ABS(ULP_Scale)
!    ! ...Default action is to return on ANY difference...
!    Check_Once = .TRUE.
!    IF ( PRESENT(Check_All) ) Check_Once = .NOT. Check_All
!    ! ...Check the structure allocation status
!    IF ( .NOT. Allocated_PtrArr( Ptr_LHS ) .OR. &
!         .NOT. Allocated_PtrArr( Ptr_RHS ) ) THEN
!      Error_Status = FAILURE
!      RETURN
!    END IF
!
!
!    ! Compare the values
!    DO n = 1, Ptr_RHS%n
!      IF ( .NOT. Compare_Float( Ptr_LHS%Arr(n),Ptr_RHS%Arr(n),ULP=ULP ) ) THEN
!        Error_Status = FAILURE
!        IF ( Check_Once ) RETURN
!      END IF
!    END DO
!  
!  END FUNCTION Equal_SRF

END MODULE PtrArr_Define
