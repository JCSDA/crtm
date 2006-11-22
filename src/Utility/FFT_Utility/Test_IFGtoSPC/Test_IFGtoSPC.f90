PROGRAM Test_IFGtoSPC

  USE Type_Kinds           , ONLY: fp=>fp_kind
  USE File_Utility         , ONLY: Get_Lun
  USE Message_Handler      , ONLY: SUCCESS
  USE Fundamental_Constants, ONLY: PI
  USE FFT_Spectral_Utility , ONLY: IFGtoSPC
  IMPLICIT NONE
  ! Parameters
  CHARACTER(*), PARAMETER :: FILENAME='fft_test_ifg.bin'
  ! Variables
  REAL(fp),    DIMENSION(:), ALLOCATABLE :: x
  REAL(fp),    DIMENSION(:), ALLOCATABLE :: rIfg, iIfg
  COMPLEX(fp), DIMENSION(:), ALLOCATABLE :: ifg
  REAL(fp),    DIMENSION(:), ALLOCATABLE :: f
  COMPLEX(fp), DIMENSION(:), ALLOCATABLE :: spc
  INTEGER :: fileId
  INTEGER :: n, n2
  INTEGER :: i, j, errStatus, ioStatus, allocStatus

  ! Read the interferometric data
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

  READ(fileId) n2
  n = (n2/2)+1

  ALLOCATE(rIfg(n2), iIfg(n2), x(n2), ifg(n2), f(n), spc(n), STAT=allocStatus)
  IF ( allocStatus /= 0 ) THEN
    WRITE(*,*) 'Error allocating arrays. STAT=',allocStatus
    STOP
  END IF

  READ(fileId)x
  READ(fileId)rIfg
  READ(fileId)iIfg
  
  CLOSE(fileId)

  ! Call the routine
  ifg = CMPLX(rIfg,iIfg,fp)
  errStatus=IFGtoSPC(x, ifg, f, spc)
  IF ( errStatus /= SUCCESS ) THEN
    WRITE(*,*) 'Error in IFGtoSPC call'
    STOP
  END IF

  ! Output an ASCII file for viewing results
  fileId = Get_Lun()
  OPEN(fileId,FILE='Test_IFGtoSPC.dat',STATUS='UNKNOWN')
  WRITE(fileId,*) n, n2
  DO i=1, n2
    WRITE(fileId,*) x(i), rIfg(i), iIfg(i)
  END DO
  DO i=1, n
    WRITE(fileId,*) f(i), REAL(spc(i),fp), AIMAG(spc(i))
  END DO
  CLOSE(fileId)
  
END PROGRAM Test_IFGtoSPC
