PROGRAM Test_CosFilter
  USE Type_Kinds,           ONLY: fp=>fp_kind
  USE FFT_Spectral_Utility, ONLY: CosFilter
  IMPLICIT NONE

  INTEGER,  PARAMETER  :: N = 1000
  REAL(fp), PARAMETER :: F1a = 500.0_fp
  REAL(fp), PARAMETER :: F1b = 600.0_fp
  REAL(fp), PARAMETER :: F2a = 3500.0_fp
  REAL(fp), PARAMETER :: F2b = 3600.0_fp

  INTEGER :: i, ierr1, ierr2
  REAL(fp), DIMENSION(N) :: f, f1, f2
  REAL(fp), DIMENSION(N) :: filter1, filter2
  
  f = (/ (i,i=0,N-1) /) / REAL(N-1,fp)
  f1 = f*(F1b-F1a) + F1a
  f2 = f*(F2b-F2a) + F2a
  
  filter1=1.0_fp
  filter2=1.0_fp
  ierr1=CosFilter(f1, filter1)
  ierr2=CosFilter(f2, filter2, Reverse=1)
  
  OPEN(10,FILE='Test_CosFilter.dat',STATUS='UNKNOWN')
  DO i=1, N
    WRITE(10,*) f1(i), filter1(i)
  END DO
  DO i=1, N
    WRITE(10,*) f2(i), filter2(i)
  END DO
  CLOSE(10)
END PROGRAM Test_CosFilter
