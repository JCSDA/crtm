PROGRAM Test_IASI

  USE Type_Kinds           , ONLY: fp=>fp_kind
  USE Binary_File_Utility  , ONLY: Open_Binary_File
  USE Message_Handler      , ONLY: SUCCESS
  USE Fundamental_Constants, ONLY: PI
  USE IASI_Define          , ONLY: IASI_X, IASI_GFT
  USE FFT_Spectral_Utility , ONLY: IFGtoSPC
  IMPLICIT NONE
  ! Parameters
  INTEGER, PARAMETER, DIMENSION(2) :: N=(/51200, 2**18/)
  ! Variables
  CHARACTER(256) :: outFile
  REAL(fp),    DIMENSION(:), ALLOCATABLE :: x, gft
  REAL(fp),    DIMENSION(:), ALLOCATABLE :: f
  COMPLEX(fp), DIMENSION(:), ALLOCATABLE :: srf
  INTEGER :: i, fileId, nSRF
  INTEGER :: errStatus, allocStatus
  
  ! Loop over test sizes
  DO i = 1, SIZE(N)
  
    ! The number of SRF points (+ve frequencies only)
    nSRF = (N(i)/2)+1
    
    ! Allocate arrays
    ALLOCATE(x(N(i)), gft(N(i)), &
             f(nSRF), srf(nSRF), &
             STAT=allocStatus)
    IF ( allocStatus /= 0 ) THEN
      WRITE(*,*) 'Error allocating arrays. STAT=',allocStatus
      STOP
    END IF
    
    ! Compute optical delay grid
    x = IASI_X(N(i))
    
    ! Compute the GFT
    gft = IASI_GFT(x)
    
    ! FFT GFT to a spectrum
    errStatus=IFGtoSPC(x, gft, f, srf)
    IF ( errStatus /= SUCCESS ) THEN
      WRITE(*,*) 'Error in IFGtoSPC call'
      STOP
    END IF

    ! Output the GFT result
    WRITE(outFile,'("IASI_GFT.",i0,".bin")') N(i)
    errStatus = Open_Binary_File( outFile, &
                                  fileId, &
                                  For_Output=1 )
    IF ( errStatus /= SUCCESS ) THEN
      WRITE(*,*) 'Error opening output results file: ',TRIM(outFile)
      STOP
    END IF
    WRITE(fileId) N(i)
    WRITE(fileId) x
    WRITE(fileId) gft
    CLOSE(fileId)

    ! Output the SRF
    WRITE(outFile,'("IASI_SRF.",i0,".bin")') N(i)
    errStatus = Open_Binary_File( outFile, &
                                  fileId, &
                                  For_Output=1 )
    IF ( errStatus /= SUCCESS ) THEN
      WRITE(*,*) 'Error opening output results file: ',TRIM(outFile)
      STOP
    END IF
    WRITE(fileId) nSRF
    WRITE(fileId) f
    WRITE(fileId) REAL(srf,fp)
    WRITE(fileId) AIMAG(srf)
    CLOSE(fileId)
    
    ! Deallocate arrays
    DEALLOCATE(x, gft, f, srf, STAT=allocStatus)
    IF ( allocStatus /= 0 ) THEN
      WRITE(*,*) 'Error deallocating arrays. STAT=',allocStatus
      STOP
    END IF
    
  END DO

END PROGRAM Test_IASI
