MODULE Interp_ND

  USE Type_Kinds, ONLY: fp=>fp_kind
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: Interp_2D
  PUBLIC :: Interp_3D
  PUBLIC :: Interp_1D
  PUBLIC :: Interp_1D_TL
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
  ! 1-D linear interpolation routine
  FUNCTION Interp_1D(d1, y) RESULT(yInt)
    REAL(fp), INTENT(IN) :: d1
    REAL(fp), INTENT(IN) :: y(:)
    REAL(fp) :: yInt
    
    yInt = ( (ONE-d1)*y(1) ) + &
           (    d1*y(2)    )
  END FUNCTION Interp_1D	   

  ! 2-D linear interpolation routine
  FUNCTION Interp_2D(d1,d2,y) RESULT(yInt)
    REAL(fp), INTENT(IN) :: d1, d2
    REAL(fp), INTENT(IN) :: y(:,:)
    REAL(fp) :: yInt
    
    yInt = ( (ONE-d1)*(ONE-d2)*y(1,1) ) + &
           (    d1   *(ONE-d2)*y(2,1) ) + &
           ( (ONE-d1)*   d2   *y(1,2) ) + &   
           (    d1   *   d2   *y(2,2) )
  END FUNCTION Interp_2D
  
  ! 3-D linear interpolation function
  FUNCTION Interp_3D(d1,d2,d3,y) RESULT(yInt)
    REAL(fp), INTENT(IN) :: d1, d2, d3
    REAL(fp), INTENT(IN) :: y(:,:,:)
    REAL(fp) :: yInt
    
    yInt = ( (ONE-d1)*(ONE-d2)*(ONE-d3)*y(1,1,1) ) + &
           (    d1   *(ONE-d2)*(ONE-d3)*y(2,1,1) ) + &
           (    d1   *   d2   *(ONE-d3)*y(2,2,1) ) + &
           ( (ONE-d1)*   d2   *(ONE-d3)*y(1,2,1) ) + &
           ( (ONE-d1)*(ONE-d2)*   d3   *y(1,1,2) ) + &
           (    d1   *(ONE-d2)*   d3   *y(2,1,2) ) + &
           (    d1   *   d2   *   d3   *y(2,2,2) ) + &
           ( (ONE-d1)*   d2   *   d3   *y(1,2,2) )
  END FUNCTION Interp_3D
  
  
  ! Find lower index for regular spacing
  FUNCTION findRegularIdx(x, dx, xInt) RESULT(idx)
    REAL(fp), INTENT(IN) :: x(:)
    REAL(fp), INTENT(IN) :: dx, xInt
    INTEGER :: idx
    INTEGER :: n
    n = SIZE(x)
    idx = MIN(FLOOR((xInt-x(1))/dx)+1,n-1)
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
  
  ! 1-D linear interpolation routine for tangent-linear
  FUNCTION Interp_1D_TL(d1,d2_TL,y) RESULT(yInt_TL)
    REAL(fp), INTENT(IN) :: d1, d2_TL
    REAL(fp), INTENT(IN) :: y(:,:)
    REAL(fp) :: yInt_TL
    
    yInt_TL = (  d2_TL*(ONE-d1)*y(1,2) ) + &
              ( -d2_TL*(ONE-d1)*y(1,1) ) + &
	      (  d2_TL*   d1   *y(2,2) ) + &  
	      ( -d2_TL*   d1   *y(2,1) )
  END FUNCTION Interp_1D_TL 
  
    
  
END MODULE Interp_ND
