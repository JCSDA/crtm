PROGRAM Test_SPCtoIFG

  USE Type_Kinds           , ONLY: fp=>fp_kind
  USE Binary_File_Utility  , ONLY: Open_Binary_File
  USE Message_Handler      , ONLY: SUCCESS
  USE Fundamental_Constants, ONLY: PI
  USE FFT_Spectral_Utility , ONLY: SPCtoIFG
  IMPLICIT NONE
  ! Parameters
  CHARACTER(*), PARAMETER :: FILENAME='test_spc.bin'
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
  WRITE(*,'(/5x,"Reading input test spectrum...")' )
  errStatus = Open_Binary_File( FILENAME, &
                                fileId    )
  IF ( errStatus /= SUCCESS ) THEN
    WRITE(*,*) 'Error opening test spectrum file.'
    STOP
  END IF
  READ(fileId) n  ! The number of SPC points
  n2 = 2*(n-1)    ! The number of IFG points
  ALLOCATE(f(n), rSpc(n), iSpc(n), spc(n), x(n2), ifg(n2), STAT=allocStatus)
  IF ( allocStatus /= 0 ) THEN
    WRITE(*,*) 'Error allocating arrays. STAT=',allocStatus
    STOP
  END IF
  READ(fileId)f     ! Frequency data
  READ(fileId)rSpc  ! Real part of SPC
  READ(fileId)iSpc  ! Imaginary part of SPC
  CLOSE(fileId)

  ! Make the spectrum complex
  spc = CMPLX(rSpc,iSpc,fp)

  ! Call the routine
  WRITE(*,'(5x,"Calling SPCtoIFG......")' )
  errStatus=SPCtoIFG(f, Spc, x, ifg)
  IF ( errStatus /= SUCCESS ) THEN
    WRITE(*,*) 'Error in SPCtoIFG call'
    STOP
  END IF

  ! Output the SPC and IFG to a file
  WRITE(*,'(5x,"Writing output file......")' )
  errStatus = Open_Binary_File( 'Test_SPCtoIFG.bin', &
                                fileId, &
                                For_Output=1 )
  IF ( errStatus /= SUCCESS ) THEN
    WRITE(*,*) 'Error opening output results file.'
    STOP
  END IF
  ! The SPC data
  WRITE(fileId) n            ! The number of SPC points
  WRITE(fileId) f            ! Frequency data
  WRITE(fileId) rSpc         ! Real part of SPC
  WRITE(fileId) iSpc         ! Imaginary part of SPC
  ! The IFG data
  WRITE(fileId) n2           ! The number of IFG points
  WRITE(fileId) x            ! Optical delay data
  WRITE(fileId) REAL(ifg,fp) ! Real part of IFG
  WRITE(fileId) AIMAG(ifg)   ! Imaginary part of IFG
  CLOSE(fileId)
  
END PROGRAM Test_SPCtoIFG
