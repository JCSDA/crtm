PROGRAM Test_CosFilter
  USE Type_Kinds          , ONLY: fp=>fp_kind
  USE File_Utility        , ONLY: Get_Lun
  USE Message_Handler     , ONLY: SUCCESS
  USE FFT_Spectral_Utility, ONLY: CosFilter
  IMPLICIT NONE

  INTEGER,  PARAMETER :: N = 1000
  REAL(fp), PARAMETER :: dF = 0.1_fp
  REAL(fp), PARAMETER :: FBEGIN1 = 500.0_fp
  REAL(fp), PARAMETER :: FBEGIN2 = 3500.0_fp

  INTEGER :: i, ierr1, ierr2, fileId
  REAL(fp), DIMENSION(N) :: f, f1, f2
  REAL(fp), DIMENSION(N) :: filter1, filter2
  
  ! Create frequency arrays
  f = (/ (i,i=0,N-1) /)
  f1 = FBEGIN1 + (f*dF)
  f2 = FBEGIN2 + (f*dF)

  ! Call the function
  ierr1=CosFilter(f1, filter1)
  ierr2=CosFilter(f2, filter2, Reverse=1)
  IF ( ierr1 /= SUCCESS .OR. ierr2 /= SUCCESS ) STOP
  
  ! Output an ASCII file for viewing results
  fileId=Get_Lun()
  OPEN(fileId,FILE='Test_CosFilter.dat',STATUS='UNKNOWN')
  WRITE(fileId,*) N
  DO i=1, N
    WRITE(fileId,*) f1(i), filter1(i)
  END DO
  DO i=1, N
    WRITE(fileId,*) f2(i), filter2(i)
  END DO
  CLOSE(fileId)

END PROGRAM Test_CosFilter
