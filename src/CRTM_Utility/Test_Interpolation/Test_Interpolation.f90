PROGRAM Test_Interpolation
  USE Type_Kinds, ONLY: fp=>fp_kind
  INTEGER, PARAMETER :: N=2
  REAL(fp), PARAMETER :: ZERO  =0.0_fp
  REAL(fp), PARAMETER :: POINT5=0.5_fp
  REAL(fp), PARAMETER :: ONE   =1.0_fp
  REAL(fp) :: x1(N),x2(N),y(N,N)
  REAL(fp) :: x1Int,x2Int,yInt
  REAL(fp) :: d1   ,d2

  x1=(/ZERO,ONE/)
  x2=(/ZERO,ONE/)
  
  y =RESHAPE((/ZERO,POINT5,POINT5,ONE/),SHAPE(y))

  dx1 = x1(2)-x1(1)
  dx2 = x2(2)-x2(1)
  DO
    WRITE(*,'(/5x,"Enter x1 and x2 [-ve x1 quits]: ")', ADVANCE='NO')
    READ(*,*) x1Int, x2Int
    IF ( x1Int < ZERO ) EXIT
    d1 = (x1Int-x1(1))/dx1
    d2 = (x2Int-x2(1))/dx2
    yInt = Interp_2D(d1,d2,y)
    WRITE(*,'(10x,"y(",f5.2,",",f5.2,") = ", f5.2)') x1Int,x2Int,yInt
  END DO
  
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

END PROGRAM Test_Interpolation
