MODULE Interp_ND

  USE Type_Kinds, ONLY: fp=>fp_kind
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: Interp_2D
  PUBLIC :: findIdx
  
  INTERFACE findIdx
    MODULE PROCEDURE findRegularIdx
    MODULE PROCEDURE findRandomIdx
  END INTERFACE findIdx
  
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID=&
  '$Id: $'
  REAL(fp), PARAMETER :: ZERO=0.0_fp
  REAL(fp), PARAMETER :: ONE =1.0_fp

CONTAINS

  FUNCTION Interp_2D(d1,d2,y) RESULT(yInt)
    REAL(fp), INTENT(IN) :: d1, d2
    REAL(fp), INTENT(IN) :: y(:,:)
    REAL(fp) :: yInt
    
    yInt = (ONE-d1)*(ONE-d2)*y(1,1) + &
              d1   *(ONE-d2)*y(2,1) + &
           (ONE-d1)*   d2   *y(1,2) + &   
              d1   *   d2   *y(2,2)
  END FUNCTION Interp_2D 

  ! Find lower index for regular spacing
  FUNCTION findRegularIdx(x1, dx, xInt) RESULT(idx)
    REAL(fp), INTENT(IN) :: x1, dx, xInt
    INTEGER :: idx
    idx = FLOOR((xInt-x1)/dx) + 1
  END FUNCTION findRegularIdx
  
  ! Find lower index for random spacing.
  ! Assumption is that x(1) <= xInt <= x(n)
  ! (despite the MIN/MAX test)
  FUNCTION findRandomIdx(x, xInt) RESULT(idx)
    REAL(fp), INTENT(IN) :: x(:)
    REAL(fp), INTENT(IN) :: xInt
    INTEGER :: idx
    INTEGER :: k, n
    n = SIZE(x)
    DO k=1,n
      IF (xInt <= x(k) ) EXIT
    END DO
    idx = MIN(MAX(1,k-1),n-1)
  END FUNCTION findRandomIdx
  
 

END MODULE Interp_ND
