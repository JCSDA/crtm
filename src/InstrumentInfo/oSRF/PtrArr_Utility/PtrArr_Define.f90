MODULE PtrArr_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, Display_Message
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
  IMPLICIT NONE


  ! -----------------  
  ! Module parameters
  ! -----------------
  INTEGER , PARAMETER :: ML = 256
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  
    
  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Derived type definitions
  PUBLIC :: PtrArr_type
  ! Procedures
  PUBLIC :: PtrArr_Associated
  PUBLIC :: PtrArr_Destroy
  PUBLIC :: PtrArr_Create
  PUBLIC :: PtrArr_SetValue
  PUBLIC :: PtrArr_GetValue
  PUBLIC :: PtrArr_Inspect


  ! -------------------
  ! Procedure overloads
  ! -------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE PtrArr_Equal
  END INTERFACE OPERATOR(==)


  ! ---------------
  ! Type definition
  ! ---------------
  TYPE :: PtrArr_type
    INTEGER :: n = 0
    LOGICAL :: Is_Allocated = .FALSE.
    REAL(fp), ALLOCATABLE :: Arr(:)
  END TYPE PtrArr_type


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

  ELEMENTAL FUNCTION PtrArr_Associated(self)
    TYPE(PtrArr_type), INTENT(IN) :: self
    LOGICAL :: PtrArr_Associated
    PtrArr_Associated = self%Is_Allocated
  END FUNCTION PtrArr_Associated


  ELEMENTAL SUBROUTINE PtrArr_Destroy(self)
    TYPE(PtrArr_type), INTENT(OUT) :: self
    self%n = 0
    self%Is_Allocated = .FALSE.
  END SUBROUTINE PtrArr_Destroy


  ELEMENTAL SUBROUTINE PtrArr_Create(self, n)
    ! Arguments
    TYPE(PtrArr_type), INTENT(OUT) :: self
    INTEGER,           INTENT(IN)  :: n
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n < 1 ) RETURN
    
    ! Allocate
    ALLOCATE( self%Arr(n), STAT=alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Initialise
    self%n = n
    self%Arr = ZERO
    
    ! Set allocation indicator
    self%Is_Allocated = .TRUE.
  END SUBROUTINE PtrArr_Create


  FUNCTION PtrArr_SetValue(self,Arr) RESULT(err_stat)
    ! Arguments
    TYPE(PtrArr_type),  INTENT(IN OUT) :: self
    REAL(fp), OPTIONAL, INTENT(IN)     :: Arr(:)
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'PtrArr_SetValue'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: n
    
    ! Set up
    err_stat = SUCCESS
    
    ! Set array data
    IF ( PRESENT(Arr) ) THEN
      n = SIZE(Arr)
      ! ...Check sizes are consistent
      IF ( self%n /= n) THEN
        err_stat = FAILURE
        WRITE( msg, '("Input array has different size, ",i0,&
                     &", from structure, ",i0)' ) n, self%n
        CALL Display_Message(ROUTINE_NAME, msg, err_stat)
        RETURN
      END IF
      ! ...Assign data
      self%Arr = Arr
    END IF
  END FUNCTION PtrArr_SetValue
  
  
  FUNCTION PtrArr_GetValue(self,n_Points,Arr) RESULT(err_stat)
    ! Arguments
    TYPE(PtrArr_type),  INTENT(IN)  :: self
    INTEGER,  OPTIONAL, INTENT(OUT) :: n_Points
    REAL(fp), OPTIONAL, INTENT(OUT) :: Arr(:)
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'PtrArr_GetValue'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: n
    
    ! Set up
    err_stat = SUCCESS
    
    ! Get scalar data
    IF ( PRESENT(n_Points) ) n_Points = self%n
    
    ! Get array data
    IF ( PRESENT(Arr) ) THEN
      n = SIZE(Arr)
      ! ...Check sizes are consistent
      IF ( self%n /= n) THEN
        err_stat = FAILURE
        WRITE( msg, '("Output array has different size, ",i0,&
                     &", from structure, ",i0)' ) n, self%n
        CALL Display_Message(ROUTINE_NAME, msg, err_stat)
        RETURN
      END IF
      ! ...Assign data
      Arr = self%Arr
    END IF
  END FUNCTION PtrArr_GetValue
  
  
  SUBROUTINE PtrArr_Inspect(self)
    TYPE(PtrArr_type), INTENT(IN) :: self
    ! Output the PtrArr components     
    WRITE(*,'(1x,"PtrArr OBJECT")')
    ! Dimensions
    WRITE(*,'(3x,"n          :",1x,i0)') self%n
    IF ( self%Is_Allocated ) THEN
      WRITE(*,'(3x,"Array Data : ")')
      WRITE(*,'(5(1x,es13.6))' ) self%Arr
    END IF
  END SUBROUTINE PtrArr_Inspect  


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  ELEMENTAL FUNCTION PtrArr_Equal( x, y ) RESULT( is_equal )
    TYPE(PtrArr_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal
    
    ! Set up
    is_equal = .FALSE.
    
    ! Check the structure association status
    IF ( (.NOT. PtrArr_Associated(x)) .OR. &
         (.NOT. PtrArr_Associated(y)) ) RETURN
    
    ! Check the contents
    IF ( (x%n == y%n) .AND. ALL(x%Arr .EqualTo. y%Arr ) ) is_Equal = .TRUE.
    
  END FUNCTION PtrArr_Equal
  
END MODULE PtrArr_Define
