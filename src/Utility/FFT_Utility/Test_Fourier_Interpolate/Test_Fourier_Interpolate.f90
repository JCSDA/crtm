PROGRAM Test_Fourier_Interpolate

  USE Type_Kinds           , ONLY: fp=>fp_kind
  USE File_Utility         , ONLY: Get_Lun
  USE Message_Handler      , ONLY: SUCCESS
  USE Fundamental_Constants, ONLY: PI
  USE FFT_Spectral_Utility , ONLY: Fourier_Interpolate
  IMPLICIT NONE
  ! Parameters
  CHARACTER(*), PARAMETER :: FILENAME='fint_test_spc.bin'
  ! Variables
  REAL(fp),    DIMENSION(:), ALLOCATABLE :: fIn, spcIn
  REAL(fp),    DIMENSION(:), ALLOCATABLE :: fOut, spcOut
  INTEGER :: fileId
  INTEGER :: n, n2
  INTEGER :: i, j, errStatus, ioStatus, allocStatus
    
  ! Read the spectral data
  fileId = Get_Lun()
  OPEN(fileId, FILE  =FILENAME     , &
               STATUS='OLD'        , & 
               FORM  ='UNFORMATTED', & 
               ACCESS='SEQUENTIAL' , & 
               IOSTAT=ioStatus       ) 
  IF ( ioStatus /= 0 ) THEN
    WRITE(*,*) 'Error opening test file. IOSTAT=',ioStatus
    STOP
  END IF
  
  READ(fileId) n
  n2 = 2**15 + 1

  ALLOCATE(fIn(n), spcIn(n), fOut(n2), spcOut(n2), STAT=allocStatus)
  IF ( allocStatus /= 0 ) THEN
    WRITE(*,*) 'Error allocating arrays. STAT=',allocStatus
    STOP
  END IF

  READ(fileId)fIn
  READ(fileId)spcIn ! Only read the real part of the spectrum
  
  CLOSE(fileId)

  ! Call the routine
  errStatus=Fourier_Interpolate(fIn, spcIn, fOut, spcOut, FilterWidth=10.0_fp)
  IF ( errStatus /= SUCCESS ) THEN
    WRITE(*,*) 'Error in Fourier_Interpolate call'
    STOP
  END IF

  ! Output an ASCII file for viewing results
  fileId = Get_Lun()
  OPEN(fileId,FILE='Test_Fourier_Interpolate.dat',STATUS='UNKNOWN')
  WRITE(fileId,*) n, n2
  DO i=1, n
    WRITE(fileId,*) fIn(i), spcIn(i)
  END DO
  DO i=1, n2
    WRITE(fileId,*) fOut(i), spcOut(i)
  END DO
  CLOSE(fileId)
  
END PROGRAM Test_Fourier_Interpolate
