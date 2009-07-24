PROGRAM Test_IASI

  USE Type_Kinds           , ONLY: fp
  USE Binary_File_Utility  , ONLY: Open_Binary_File
  USE Message_Handler      , ONLY: SUCCESS
  USE Fundamental_Constants, ONLY: PI
  USE IASI_Define          , ONLY: IASI_X, IASI_GFT
  USE FFT_Spectral_Utility , ONLY: ComputeNSPC, ComputeMeanDelta, &
                                   IFGtoSPC
  IMPLICIT NONE
  ! Parameters
  INTEGER , PARAMETER :: N_FFT = 51200
  INTEGER , PARAMETER :: N_PO2 = 2**19
  REAL(fp), PARAMETER :: ZERO  = 0.0_fp
  ! Variables
  REAL(fp)   , ALLOCATABLE :: x(:), gft(:)
  REAL(fp)   , ALLOCATABLE :: f(:)
  COMPLEX(fp), ALLOCATABLE :: srf(:)
  REAL(fp) :: dX, maxX
  INTEGER  :: i
  INTEGER  :: n_IFG, n_SPC
  INTEGER  :: nHalf, i1, i2
  INTEGER  :: Error_Status, Allocate_Status
  
  
  ! Compute the nominal IASI GFT
  ! ----------------------------
  ! The number of IFG points
  n_IFG = N_FFT
  ! The number of SRF points (+ve frequencies only)
  n_SPC = ComputeNSPC(n_IFG)
  
  ! Allocate arrays
  ALLOCATE(x(n_IFG), gft(n_IFG), &
           f(n_SPC), srf(n_SPC), &
           STAT=Allocate_Status)
  IF ( Allocate_Status /= 0 ) THEN
    WRITE(*,*) 'Error allocating arrays. STAT=',Allocate_Status
    STOP
  END IF

  ! Compute optical delay grid and output some numbers
  x = IASI_X(n_IFG)
  dX = ComputeMeanDelta(x)
  WRITE( *,'(5x,"-Xmax,+Xmax = ",2(1x,es15.8))' ) x(1),x(n_IFG)
  WRITE( *,'(5x,"dX = ",es15.8)' ) dX
  WRITE( *,'(5x,"-Xmax-dX = ",es15.8)' ) x(1)-dX
  
  ! Compute the GFT
  gft = IASI_GFT(x)
  WRITE( *,'(5x,"GFT(-Xeff),GFT(+Xeff) = ",2(1x,es23.16))' ) gft(1),gft(n_IFG)
  
  ! Output the GFT result
  CALL Write_File(x, gft)

  ! FFT GFT to a spectrum
  Error_Status = IFGtoSPC(x, gft, f, srf)
  IF ( Error_Status /= SUCCESS ) THEN
    WRITE(*,*) 'Error in IFGtoSPC call'
    STOP
  END IF

  ! Output the SRF
  CALL Write_File(f, REAL(srf,fp), yi=AIMAG(srf))
  
  ! Deallocate arrays
  DEALLOCATE(x, gft, f, srf, STAT=Allocate_Status)
  IF ( Allocate_Status /= 0 ) THEN
    WRITE(*,*) 'Error deallocating arrays. STAT=',Allocate_Status
    STOP
  END IF


  ! Compute a "higher resolution" IASI GFT
  ! --------------------------------------
  ! The number of IFG points
  n_IFG = N_PO2
  ! The number of SRF points (+ve frequencies only)
  n_SPC = ComputeNSPC(n_IFG)
  
  ! Allocate arrays
  ALLOCATE(x(n_IFG), gft(n_IFG), &
           f(n_SPC), srf(n_SPC), &
           STAT=Allocate_Status)
  IF ( Allocate_Status /= 0 ) THEN
    WRITE(*,*) 'Error allocating arrays. STAT=',Allocate_Status
    STOP
  END IF

  ! Compute optical delay grid and output some numbers
  maxX = REAL(N_IFG/2,fp) * dX
  nHalf = N_IFG/2
  x(nHalf:N_IFG) = (/(REAL(i,fp),i=0,nHalf)/)/REAL(nHalf,fp)
  x(1:nHalf-1)   = -x(N_IFG-1:nHalf+1:-1)
  x = x*maxX
  WRITE( *,'(5x,"-Xmax,+Xmax = ",2(1x,es15.8))' ) x(1),x(n_IFG)
  WRITE( *,'(5x,"dX = ",es15.8)' ) dX
  WRITE( *,'(5x,"-Xmax-dX = ",es15.8)' ) x(1)-dX
  
  ! Compute the GFT
  gft = ZERO
  i1 = (N_IFG-N_FFT)/2 + 1
  i2 = i1 + N_FFT - 1
  gft(i1:i2) = IASI_GFT(x(i1:i2))
  WRITE( *,'(5x,"GFT(-Xeff),GFT(+Xeff) = ",2(1x,es15.8))' ) gft(i1),gft(i2)
  
  ! Output the GFT result
  CALL Write_File(x, gft)

  ! FFT GFT to a spectrum
  Error_Status = IFGtoSPC(x, gft, f, srf)
  IF ( Error_Status /= SUCCESS ) THEN
    WRITE(*,*) 'Error in IFGtoSPC call'
    STOP
  END IF

  ! Output the SRF
  CALL Write_File(f, REAL(srf,fp), yi=AIMAG(srf))
  
  ! Deallocate arrays
  DEALLOCATE(x, gft, f, srf, STAT=Allocate_Status)
  IF ( Allocate_Status /= 0 ) THEN
    WRITE(*,*) 'Error deallocating arrays. STAT=',Allocate_Status
    STOP
  END IF

CONTAINS

  SUBROUTINE Write_File(x, yr, yi)
    REAL(fp),           INTENT(IN) :: x(:), yr(:)
    REAL(fp), OPTIONAL, INTENT(IN) :: yi(:)
    INTEGER :: n, FileID, Error_Status
    CHARACTER(256) :: Outfile
    n = SIZE(x)
    IF ( PRESENT(yi) ) THEN
      WRITE(Outfile,'("IASI_SRF.",i0,".bin")') n
    ELSE
      WRITE(Outfile,'("IASI_GFT.",i0,".bin")') n
    END IF
    Error_Status = Open_Binary_File( Outfile, &
                                     FileID, &
                                     For_Output=1 )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE(*,*) 'Error opening output results file: ',TRIM(Outfile)
      STOP
    END IF
    WRITE(FileID) n
    WRITE(FileID) x
    WRITE(FileID) yr
    IF ( PRESENT(yi) ) WRITE(FileID) yi
    CLOSE(FileID)
  END SUBROUTINE Write_File

END PROGRAM Test_IASI
