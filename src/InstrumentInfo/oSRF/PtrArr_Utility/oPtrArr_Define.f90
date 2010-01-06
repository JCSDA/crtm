MODULE PtrArr_Define

!  USE Type_Kinds, ONLY: fp
  IMPLICIT NONE
  
  PRIVATE
  PUBLIC :: PtrArr_type
  PUBLIC :: Create
  PUBLIC :: Destroy
  PUBLIC :: Assign
  
  TYPE :: PtrArr_type
    INTEGER :: n_Allocates = 0
    INTEGER :: n = 0
!    REAL(fp), ALLOCATABLE :: Arr(:)
    REAL, ALLOCATABLE :: Arr(:)
  CONTAINS
    PROCEDURE, PASS(obj) :: Create
    PROCEDURE, PASS(obj) :: Destroy
    PROCEDURE, PASS(obj) :: Assign
    FINAL :: Destroy
  END TYPE PtrArr_type


CONTAINS

  SUBROUTINE Create(Obj, n)
    TYPE(PtrArr_type), INTENT(OUT) :: Obj
    INTEGER, INTENT(IN) :: n
    IF ( .NOT. ALLOCATED(Obj%Arr) ) THEN
      ALLOCATE( Obj%Arr(n) )
      Obj%n = n
    END IF
  END SUBROUTINE Create
  
  
  SUBROUTINE Destroy(Obj)
    TYPE(PtrArr_type) :: Obj
    IF ( ALLOCATED(Obj%Arr) ) THEN
      DEALLOCATE(Obj%Arr)
      Obj%n = 0
    END IF
  END SUBROUTINE Destroy


  FUNCTION Assign(Obj) RESULT(Copy)
    TYPE(PtrArr_type), INTENT(IN) :: Obj
    TYPE(PtrArr_type) :: Copy
    CALL Create(Copy, Obj%n)
    Copy%Arr = Obj%Arr
  END FUNCTION Assign

END MODULE PtrArr_Define
