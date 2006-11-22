PROGRAM Test_SPCtoIFG

  USE Type_Kinds           , ONLY: fp=>fp_kind
  USE File_Utility         , ONLY: Get_Lun
  USE Message_Handler      , ONLY: SUCCESS
  USE Fundamental_Constants, ONLY: PI
  USE FFT_Spectral_Utility , ONLY: SPCtoIFG
  IMPLICIT NONE
  ! Parameters
  CHARACTER(*), PARAMETER :: FILENAME='fft_test_spc.bin'
  ! Variables
  REAL(fp),    DIMENSION(:), ALLOCATABLE :: f
  REAL(fp),    DIMENSION(:), ALLOCATABLE :: rSpc, iSpc
  COMPLEX(fp), DIMENSION(:), ALLOCATABLE :: spc
  REAL(fp),    DIMENSION(:), ALLOCATABLE :: x
  COMPLEX(fp), DIMENSION(:), ALLOCATABLE :: ifg
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
  n2 = 2*(n-1)

  ALLOCATE(f(n), rSpc(n), iSpc(n), spc(n), x(n2), ifg(n2), STAT=allocStatus)
  IF ( allocStatus /= 0 ) THEN
    WRITE(*,*) 'Error allocating arrays. STAT=',allocStatus
    STOP
  END IF

  READ(fileId)f
  READ(fileId)rSpc
  READ(fileId)iSpc
  
  CLOSE(fileId)

  ! Call the routine
  spc = CMPLX(rSpc,iSpc,fp)
  errStatus=SPCtoIFG(f, Spc, x, ifg)
  IF ( errStatus /= SUCCESS ) THEN
    WRITE(*,*) 'Error in SPCtoIFG call'
    STOP
  END IF

  ! Output an ASCII file for viewing results
  fileId = Get_Lun()
  OPEN(fileId,FILE='Test_SPCtoIFG.dat',STATUS='UNKNOWN')
  WRITE(fileId,*) n
  DO i=1, n
    WRITE(fileId,*) f(i), rSpc(i), iSpc(i) 
  END DO
  DO i=1, n2
    WRITE(fileId,*) x(i), REAL(ifg(i),fp),AIMAG(ifg(i))
  END DO
  CLOSE(fileId)
  
END PROGRAM Test_SPCtoIFG
