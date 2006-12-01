PROGRAM Test_Fourier_Interpolate

  USE Type_Kinds           , ONLY: fp=>fp_kind
  USE Binary_File_Utility  , ONLY: Open_Binary_File
  USE Message_Handler      , ONLY: SUCCESS
  USE Fundamental_Constants, ONLY: PI
  USE FFT_Spectral_Utility , ONLY: Fourier_Interpolate
  IMPLICIT NONE
  ! Parameters
  CHARACTER(*), PARAMETER :: FILENAME='test_spc.bin'
  ! Variables
  REAL(fp),    DIMENSION(:), ALLOCATABLE :: fIn, spcIn
  REAL(fp),    DIMENSION(:), ALLOCATABLE :: fOut, spcOut
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
  n2 = 2**15 + 1  ! The number of zerofilled IFG points
  ALLOCATE(fIn(n), spcIn(n), fOut(n2), spcOut(n2), STAT=allocStatus)
  IF ( allocStatus /= 0 ) THEN
    WRITE(*,*) 'Error allocating arrays. STAT=',allocStatus
    STOP
  END IF
  READ(fileId)fIn   ! Frequency data
  READ(fileId)spcIn ! Only read the real part of SPC 
  CLOSE(fileId)

  ! Call the routine
  WRITE(*,'(5x,"Calling Fouier_Interpolate...")' )
  errStatus=Fourier_Interpolate(fIn, spcIn, fOut, spcOut, FilterWidth=10.0_fp)
  IF ( errStatus /= SUCCESS ) THEN
    WRITE(*,*) 'Error in Fourier_Interpolate call'
    STOP
  END IF

  ! Output an ASCII file for viewing results
  WRITE(*,'(5x,"Writing output file......")' )
  errStatus = Open_Binary_File( 'Test_Fourier_Interpolate.bin', &
                                fileId, &
                                For_Output=1 )
  IF ( errStatus /= SUCCESS ) THEN
    WRITE(*,*) 'Error opening output results file.'
    STOP
  END IF
  ! The original SPC data (REAL ONLY)
  WRITE(fileId) n            ! The number of SPC points
  WRITE(fileId) fIn          ! Frequency data
  WRITE(fileId) spcIn        ! Real part of SPC
  ! The interpolated SPC data (REAL ONLY)
  WRITE(fileId) n2           ! The number of SPC points
  WRITE(fileId) fOut         ! Frequency data
  WRITE(fileId) spcOut       ! Real part of SPC
  CLOSE(fileId)
  
END PROGRAM Test_Fourier_Interpolate
