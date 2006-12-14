PROGRAM Test_Interp_2D
  USE Type_Kinds           , ONLY: fp=>fp_kind
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
  USE Interp_ND
  IMPLICIT NONE
  ! PArameters
  REAL(fp), PARAMETER :: ZERO   =0.0_fp
  REAL(fp), PARAMETER :: POINT2 =0.2_fp
  REAL(fp), PARAMETER :: POINT25=0.25_fp
  REAL(fp), PARAMETER :: POINT3 =0.3_fp
  REAL(fp), PARAMETER :: POINT5 =0.5_fp
  REAL(fp), PARAMETER :: ONE    =1.0_fp
  INTEGER,  PARAMETER :: N=2
  INTEGER,  PARAMETER :: NTESTS=6
  ! Input data
  REAL(fp), PARAMETER :: X1(N) =(/ZERO,ONE/)
  REAL(fp), PARAMETER :: X2(N) =(/ZERO,ONE/)
  REAL(fp), PARAMETER :: Y(N,N)=RESHAPE((/ZERO,POINT5,POINT5,ONE/),SHAPE(Y))
  ! Interpolated data
  REAL(fp), PARAMETER :: X1INT(NTESTS)=(/ZERO,ZERO  ,ONE   ,ONE,POINT5,POINT2/)
  REAL(fp), PARAMETER :: X2INT(NTESTS)=(/ZERO,ONE   ,ZERO  ,ONE,POINT5,POINT3/)
  REAL(fp), PARAMETER :: YINT(NTESTS) =(/ZERO,POINT5,POINT5,ONE,POINT5,POINT25/)
  ! Variables
  INTEGER :: i
  REAL(fp) :: dx1  ,dx2
  REAL(fp) :: d1   ,d2
  REAL(fp) :: yDiff
  
  dx1 = X1(2)-X1(1)
  dx2 = X2(2)-X2(1)

  DO i=1,NTESTS
    d1 = (X1INT(i)-X1(1))/dx1
    d2 = (X2INT(i)-X2(1))/dx2
    yDiff = Interp_2D(d1,d2,Y) - YINT(i)
    IF ( yDiff .EqualTo. ZERO ) THEN
      WRITE(*,'(5x,"Test case ",i0," passed.")') i
    ELSE
      WRITE(*,'(5x,"Test case ",i0," failed. yDiff=",es13.6)') i, yDiff
    END IF
  END DO

END PROGRAM Test_Interp_2D

