PROGRAM Test_IFGtoSPC

  USE Type_Kinds           , ONLY: fp=>fp_kind
  USE Binary_File_Utility  , ONLY: Open_Binary_File
  USE Message_Handler      , ONLY: SUCCESS
  USE Fundamental_Constants, ONLY: PI
  USE FFT_Spectral_Utility , ONLY: IFGtoSPC
  IMPLICIT NONE
  ! Parameters
  CHARACTER(*), PARAMETER :: FILENAME='test_ifg.bin'
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
  WRITE(*,'(/5x,"Reading input test interferogram...")' )
  errStatus = Open_Binary_File( FILENAME, &
                                fileId    )
  IF ( errStatus /= SUCCESS ) THEN
    WRITE(*,*) 'Error opening test interferogram file.'
    STOP
  END IF
  READ(fileId) n2  ! The number of IFG points
  n = (n2/2)+1     ! The number of SPC points
  ALLOCATE(rIfg(n2), iIfg(n2), x(n2), ifg(n2), f(n), spc(n), STAT=allocStatus)
  IF ( allocStatus /= 0 ) THEN
    WRITE(*,*) 'Error allocating arrays. STAT=',allocStatus
    STOP
  END IF
  READ(fileId)x     ! Optical delay data
  READ(fileId)rIfg  ! Real part of IFG
  READ(fileId)iIfg  ! Imaginary part of IFG
  CLOSE(fileId)

  ! Make the interferogram complex
  ifg = CMPLX(rIfg,iIfg,fp)

  ! Call the routine
  WRITE(*,'(5x,"Calling IFGtoSPC...")' )
  errStatus=IFGtoSPC(x, ifg, f, spc)
  IF ( errStatus /= SUCCESS ) THEN
    WRITE(*,*) 'Error in IFGtoSPC call'
    STOP
  END IF

  ! Output the IFG and SPC to a file
  WRITE(*,'(5x,"Writing output file......")' )
  errStatus = Open_Binary_File( 'Test_IFGtoSPC.bin', &
                                fileId, &
                                For_Output=1 )
  IF ( errStatus /= SUCCESS ) THEN
    WRITE(*,*) 'Error opening output results file.'
    STOP
  END IF
  ! The SPC data
  WRITE(fileId) n            ! The number of SPC points
  WRITE(fileId) f            ! Frequency data
  WRITE(fileId) REAL(spc,fp) ! Real part of SPC
  WRITE(fileId) AIMAG(spc)   ! Imaginary part of SPC
  ! The IFG data
  WRITE(fileId) n2           ! The number of IFG points
  WRITE(fileId) x            ! Optical delay data
  WRITE(fileId) rIfg         ! Real part of IFG
  WRITE(fileId) iIfg         ! Imaginary part of IFG
  CLOSE(fileId)
  
END PROGRAM Test_IFGtoSPC
