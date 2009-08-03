MODULE PtrArr_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, Display_Message
  USE Compare_Float_Numbers, ONLY: Compare_Float
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
  PUBLIC :: Allocated_PtrArr
  PUBLIC :: Create_PtrArr
  PUBLIC :: Destroy_PtrArr
  PUBLIC :: Assign_PtrArr
  PUBLIC :: Equal_PtrArr
  PUBLIC :: Set_Property_PtrArr
  PUBLIC :: Get_Property_PtrArr
  PUBLIC :: Inspect_PtrArr


  ! ---------------
  ! Type definition
  ! ---------------
  TYPE :: PtrArr_type
    INTEGER :: n = 0
    REAL(fp), ALLOCATABLE :: Arr(:)
  END TYPE PtrArr_type


CONTAINS


  ELEMENTAL FUNCTION Allocated_PtrArr(self)
    TYPE(PtrArr_type), INTENT(IN) :: self
    LOGICAL :: Allocated_PtrArr
    IF ( ALLOCATED( self%Arr ) ) THEN
        Allocated_PtrArr = .TRUE.
    ELSE
        Allocated_PtrArr = .FALSE.
    END IF
  END FUNCTION Allocated_PtrArr


  FUNCTION Destroy_PtrArr(self) RESULT(err_status)
    ! Arguments
    TYPE(PtrArr_type), INTENT(OUT) :: self
    ! Function result
    INTEGER :: err_status
    ! Set up
    err_status = SUCCESS
    ! Reinitialise
    self%n = 0
  END FUNCTION Destroy_PtrArr


  FUNCTION Create_PtrArr(self, n) RESULT(err_status)
    ! Arguments
    TYPE(PtrArr_type), INTENT(OUT) :: self
    INTEGER,           INTENT(IN)  :: n
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
    ALLOCATE( self%Arr(n), STAT=alloc_status )
    IF ( alloc_status /= 0 ) THEN
      err_status = FAILURE
      WRITE( msg,'("Error allocating. STAT = ",i0)' ) alloc_status
      CALL Display_Message(ROUTINE_NAME, TRIM(msg), err_status)
      RETURN
    END IF
    ! Initialise
    self%n = n
    self%Arr = ZERO
  END FUNCTION Create_PtrArr


  FUNCTION Assign_PtrArr(self, copy) RESULT(err_Status)
    ! Arguments
    TYPE(PtrArr_type), INTENT(IN)  :: self
    TYPE(PtrArr_type), INTENT(OUT) :: copy
    ! Function result
    INTEGER :: err_status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME='Assign_PtrArr'
    ! Local variables
    CHARACTER(ML) :: msg
    ! Set up
    err_status = SUCCESS
    ! ...ALL *inputs* must be allocated
    IF ( .NOT. Allocated_PtrArr(self) ) THEN
      err_status = FAILURE; msg = 'Input not allocated'
      CALL Display_Message(ROUTINE_NAME, TRIM(msg), err_status)
      RETURN
    END IF
    ! Allocate the structure
    err_status = Create_PtrArr(copy, self%n)
    IF ( err_status /= SUCCESS ) THEN
      err_status = FAILURE; msg = 'Error allocating output'
      CALL Display_Message(ROUTINE_NAME, TRIM(msg), err_status)
      RETURN
    END IF
    ! Assign data
    copy%Arr = self%Arr
  END FUNCTION Assign_PtrArr


  FUNCTION Equal_PtrArr( p_LHS    , &  ! Input
                         p_RHS    , &  ! Input
                         ULP_Scale, &  ! Optional input
                         Check_All) &  ! Optional input
                       RESULT( err_status )
    ! Arguments
    TYPE(PtrArr_type), INTENT(IN) :: p_LHS
    TYPE(PtrArr_type), INTENT(IN) :: p_RHS
    INTEGER, OPTIONAL, INTENT(IN) :: ULP_Scale
    LOGICAL, OPTIONAL, INTENT(IN) :: Check_All
    ! Function result
    INTEGER :: err_status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME='Equal_PtrArr'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: ULP
    LOGICAL :: Check_Once
    INTEGER :: n
    ! Set up
    err_status = SUCCESS
    ! ...Default precision is a single unit in last place
    ULP = 1
    IF ( PRESENT(ULP_Scale) ) ULP = ABS(ULP_Scale)
    ! ...Default action is to return on ANY difference...
    Check_Once = .TRUE.
    IF ( PRESENT(Check_All) ) Check_Once = .NOT. Check_All
    ! ...Check the structure allocation status
    IF ( .NOT. Allocated_PtrArr( p_LHS ) .OR. &
         .NOT. Allocated_PtrArr( p_RHS )      ) THEN
      err_status = FAILURE; msg = 'Input PtrArr not allocated'
      CALL Display_Message(ROUTINE_NAME, TRIM(msg), err_status)
      RETURN
    END IF
    ! Compare the values
    DO n = 1, p_RHS%n
      IF ( .NOT. Compare_Float( p_LHS%Arr(n),p_RHS%Arr(n),ULP=ULP ) ) THEN
        err_status = FAILURE
        WRITE( msg,'("Arr component different at index #",i0)' ) n
        CALL Display_Message(ROUTINE_NAME, TRIM(msg), err_status)
        IF ( Check_Once ) RETURN
      END IF
    END DO
  END FUNCTION Equal_PtrArr


  FUNCTION Set_Property_PtrArr(self,Arr) RESULT(err_status)
    ! Arguments
    TYPE(PtrArr_type),  INTENT(IN OUT) :: self
    REAL(fp), OPTIONAL, INTENT(IN)     :: Arr(:)
    ! Function result
    INTEGER :: err_status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Set_Property_PtrArr'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: n
    ! Set up
    err_status = SUCCESS
    ! Set array data
    IF ( PRESENT(Arr) ) THEN
      n = SIZE(Arr)
      ! ...Check sizes are consistent
      IF ( self%n /= n) THEN
        err_status = FAILURE
        WRITE( msg, '("Input array has different size, ",i0,&
                     &", from structure, ",i0)' ) n, self%n
        CALL Display_Message(ROUTINE_NAME, TRIM(msg), err_status)
        RETURN
      END IF
      ! ...Assign data
      self%Arr = Arr
    END IF
  END FUNCTION Set_Property_PtrArr
  
  
  FUNCTION Get_Property_PtrArr(self,n_Points,Arr) RESULT(err_status)
    ! Arguments
    TYPE(PtrArr_type),  INTENT(IN)  :: self
    INTEGER,  OPTIONAL, INTENT(OUT) :: n_Points
    REAL(fp), OPTIONAL, INTENT(OUT) :: Arr(:)
    ! Function result
    INTEGER :: err_status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Get_Property_PtrArr'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: n
    ! Set up
    err_status = SUCCESS
    ! Get scalar data
    IF ( PRESENT(n_Points) ) n_Points = self%n
    ! Get array data
    IF ( PRESENT(Arr) ) THEN
      n = SIZE(Arr)
      ! ...Check sizes are consistent
      IF ( self%n /= n) THEN
        err_status = FAILURE
        WRITE( msg, '("Output array has different size, ",i0,&
                     &", from structure, ",i0)' ) n, self%n
        CALL Display_Message(ROUTINE_NAME, TRIM(msg), err_status)
        RETURN
      END IF
      ! ...Assign data
      Arr = self%Arr
    END IF
  END FUNCTION Get_Property_PtrArr
  
  
  SUBROUTINE Inspect_PtrArr(self)
    TYPE(PtrArr_type), INTENT(IN) :: self
    WRITE( *,'(4x,"n_Points : ", i0)' ) self%n
    IF ( self%n > 0 ) WRITE( *,'(4x,"Data : ", /,5(1x,es13.6))' ) self%Arr
  END SUBROUTINE Inspect_PtrArr
  
END MODULE PtrArr_Define
