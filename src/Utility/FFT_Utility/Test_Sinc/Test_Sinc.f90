PROGRAM Test_Sinc
  USE Type_Kinds           , ONLY: fp=>fp_kind
  USE File_Utility         , ONLY: Get_Lun
  USE Fundamental_Constants, ONLY: PI
  USE FFT_Spectral_Utility , ONLY: Sinc
  IMPLICIT NONE

  INTEGER,  PARAMETER :: N = 10001
  REAL(fp), PARAMETER :: TEN = 10.0_fp
  REAL(fp), PARAMETER :: XBEGIN = -TEN*PI
  REAL(fp), PARAMETER :: XEND   =  TEN*PI

  INTEGER :: i, fileId
  REAL(fp), DIMENSION(N) :: x, y, yNorm
  
  ! Create x array
  x = (/ (i,i=0,N-1) /) / REAL(N-1,fp)
  x = x*(XEND-XBEGIN) + XBEGIN

  ! Call the function
  y     = Sinc(x)
  yNorm = Sinc(x,Normalized=1)
  
  ! Output an ASCII file for viewing results
  fileId=Get_Lun()
  OPEN(fileId,FILE='Test_Sinc.dat',STATUS='UNKNOWN')
  WRITE(fileId,*) N
  DO i=1, N
    WRITE(fileId,*) x(i), y(i), yNorm(i)
  END DO
  CLOSE(fileId)

END PROGRAM Test_Sinc
