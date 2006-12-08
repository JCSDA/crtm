PROGRAM Test_Fourier_Interpolate

  USE Type_Kinds           , ONLY: fp=>fp_kind
  USE Binary_File_Utility  , ONLY: Open_Binary_File
  USE Message_Handler      , ONLY: SUCCESS
  USE Fundamental_Constants, ONLY: PI
  USE FFT_Spectral_Utility , ONLY: Fourier_Interpolate
  IMPLICIT NONE
  ! Parameters
  CHARACTER(*), PARAMETER :: INFILE(2)=(/'test_spc.bin   ','test_boxcar.bin'/)
  INTEGER,      PARAMETER :: NFILTERS=4
  REAL(fp),     PARAMETER :: FILTERWIDTH(NFILTERS) = (/10.0_fp,20.0_fp,50.0_fp,100.0_fp/)
  ! Variables
  CHARACTER(256) :: outFile
  REAL(fp),    DIMENSION(:), ALLOCATABLE :: fIn, spcIn
  REAL(fp),    DIMENSION(:), ALLOCATABLE :: fOut, spcOut
  INTEGER  :: fileId
  INTEGER  :: i, j, nIn, po2, nPO2, nOut
  INTEGER  :: errStatus, allocStatus
  CHARACTER(5) :: cFilterWidth
  
  ! Select spectral data
  WRITE(*,'(/5x,"Select spectral data:")')
  DO i=1,2
    WRITE(*,'(10x,i1,") " ,a)') i, INFILE(i)
  END DO
  WRITE(*,'(5x,"Enter choice: ")',ADVANCE='NO')
  READ(*,*) i

  ! Enter a power-of-two value
  WRITE(*,'(/5x,"Enter a power-of-two [14]: ")',ADVANCE='NO')
  READ(*,*) po2

  ! Read the spectral data
  WRITE(*,'(/5x,"Reading input test spectrum ",a,"...")' ) TRIM(INFILE(i))
  errStatus = Open_Binary_File( INFILE(i), &
                                fileId     )
  IF ( errStatus /= SUCCESS ) THEN
    WRITE(*,*) 'Error opening test spectrum file.'
    STOP
  END IF
  READ(fileId) nIn  ! The number of SPC points
  nPO2 = (2**po2 + 1) + 10  ! The number of interpolated output points, plus some extra
  ALLOCATE(fIn(nIn), spcIn(nIn), fOut(nPO2), spcOut(nPO2), STAT=allocStatus)
  IF ( allocStatus /= 0 ) THEN
    WRITE(*,*) 'Error allocating arrays. STAT=',allocStatus
    STOP
  END IF
  READ(fileId)fIn   ! Frequency data
  READ(fileId)spcIn ! Only read the real part of SPC 
  CLOSE(fileId)

  ! Loop over filter widths
  DO j=1,NFILTERS
  
    WRITE(cFilterWidth,'(f5.1)') FILTERWIDTH(j)
    cFilterWidth=ADJUSTL(cFilterWidth)
    
    ! Call the routine
    WRITE(*,'(/5x,"Calling Fourier_Interpolate with filter width of ",a,"cm-1...")' ) TRIM(cFilterWidth)
    errStatus=Fourier_Interpolate(fIn, spcIn, nOut, fOut, spcOut, &
                                  PowerOfTwo =po2, &
                                  FilterWidth=FILTERWIDTH(j))
    IF ( errStatus /= SUCCESS ) THEN
      WRITE(*,*) 'Error in Fourier_Interpolate call'
      STOP
    END IF

    ! Output an ASCII file for viewing results
    WRITE(outFile,'("Test_Fourier_Interpolate.fw-",a,".",a)') TRIM(cFilterWidth), TRIM(INFILE(i))
    WRITE(*,'(5x,"Writing output file ",a,"...")' ) TRIM(outFile)
    errStatus = Open_Binary_File( outFile, &
                                  fileId, &
                                  For_Output=1 )
    IF ( errStatus /= SUCCESS ) THEN
      WRITE(*,*) 'Error opening output results file.'
      STOP
    END IF
    ! The original SPC data (REAL ONLY)
    WRITE(fileId) nIn             ! The number of SPC points
    WRITE(fileId) fIn             ! Frequency data
    WRITE(fileId) spcIn           ! Real part of SPC
    ! The interpolated SPC data (REAL ONLY)
    WRITE(fileId) nOut            ! The number of SPC points
    WRITE(fileId) fOut(1:nOut)    ! Frequency data
    WRITE(fileId) spcOut(1:nOut)  ! Real part of SPC
    CLOSE(fileId)
  END DO
  
END PROGRAM Test_Fourier_Interpolate
