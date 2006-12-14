MODULE Interp_ND

  USE Type_Kinds, ONLY: fp=>fp_kind
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: Interp_2D
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

END MODULE Interp_ND
