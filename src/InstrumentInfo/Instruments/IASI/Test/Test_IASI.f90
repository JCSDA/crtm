!
! Test_IASI
!
! Program to test the IASI instrument specification modules.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 11-Dec-2006
!                       paul.vandelst@noaa.gov

PROGRAM Test_IASI

  ! Environment setup
  ! ...Module use
  USE Type_Kinds           , ONLY: fp
  USE Binary_File_Utility  , ONLY: Open_Binary_File
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, Program_Message, Display_Message
  USE Fundamental_Constants, ONLY: PI
  USE IASI_Define          , ONLY: N_IASI_BANDS, N_IASI_FFT, &
                                   IASI_X, IASI_GFT, &
                                   IASI_DefineVersion
  USE FFT_Spectral_Utility , ONLY: ComputeNSPC, ComputeMeanDelta, &
                                   IFGtoSPC
  ! ...Disable all implicit typing
  IMPLICIT NONE
  
  ! Parameters
  CHARACTER(*),  PARAMETER :: PROGRAM_NAME   = 'Test_IASI'
  CHARACTER(*),  PARAMETER :: PROGRAM_VERSION_ID = &
    '$Id:$'
  INTEGER , PARAMETER :: N_PO2 = 2**19
  REAL(fp), PARAMETER :: ZERO  = 0.0_fp

  ! Variables
  CHARACTER(256) :: msg, Id
  REAL(fp)   , ALLOCATABLE :: x(:), gft(:)
  REAL(fp)   , ALLOCATABLE :: f(:)
  COMPLEX(fp), ALLOCATABLE :: srf(:)
  REAL(fp) :: dX, maxX
  INTEGER  :: i, n
  INTEGER  :: n_IFG, n_SPC, n_FFT
  INTEGER  :: nHalf, i1, i2
  INTEGER  :: Error_Status, Allocate_Status
  
  
  ! Output header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test the IASI instrument specification modules.', &
                        '$Revision$' )


  ! Version output
  CALL IASI_DefineVersion(Id)
  WRITE( *,'(/,a)' ) TRIM(Id)


  ! Loop over the number of IASI bands
  Band_Loop: DO n = 1, N_IASI_BANDS
    WRITE(*, '(/3x,"BAND ",i0)') n
  
  
    ! Set the number of IASI FFT points
    n_FFT = N_IASI_FFT(n)

  
    ! Compute the nominal IASI GFT
    WRITE(*, '(5x,"Computing the nominal IASI GFT...")')
    ! ...The number of IFG points
    n_IFG = n_FFT
    ! ...The number of SRF points (+ve frequencies only)
    n_SPC = ComputeNSPC(n_IFG)
  
    ! Allocate arrays
    ALLOCATE(x(n_IFG), gft(n_IFG), &
             f(n_SPC), srf(n_SPC), &
             STAT=Allocate_Status)
    IF ( Allocate_Status /= 0 ) THEN
      WRITE(msg, '("Error allocating arrays. STAT=",i0)') Allocate_Status
      CALL Display_Message(PROGRAM_NAME, msg, FAILURE)
      STOP
    END IF

    ! Compute optical delay grid and output some numbers
    WRITE(*, '(7x,"...Computing the optical delay grid...")')
    x = IASI_X(n,n_IFG)
    dX = ComputeMeanDelta(x)
    WRITE(*, '(7x,"-Xmax,+Xmax = ",2(1x,es15.8))' ) x(1),x(n_IFG)
    WRITE(*, '(7x,"dX = ",es15.8)' ) dX
    WRITE(*, '(7x,"-Xmax-dX = ",es15.8)' ) x(1)-dX
  
    ! Compute the GFT
    WRITE(*, '(7x,"...Computing the GFT...")')
    gft = IASI_GFT(n,x)
    WRITE( *,'(7xx,"GFT(-Xeff),GFT(+Xeff) = ",2(1x,es23.16))' ) gft(1),gft(n_IFG)
  
    ! Output the GFT result
    CALL Write_File(x, gft)

    ! FFT GFT to a spectrum
    WRITE(*, '(7x,"...FFTing the GFT to a spectrum...")')
    Error_Status = IFGtoSPC(x, gft, f, srf)
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error in IFGtoSPC call'
      CALL Display_Message(PROGRAM_NAME, msg, FAILURE)
      STOP
    END IF

    ! Output the SRF
    CALL Write_File(f, REAL(srf,fp), yi=AIMAG(srf))
  
    ! Deallocate arrays
    DEALLOCATE(x, gft, f, srf, STAT=Allocate_Status)
    IF ( Allocate_Status /= 0 ) THEN
      WRITE(msg, '("Error deallocating arrays. STAT=",i0)') Allocate_Status
      CALL Display_Message(PROGRAM_NAME, msg, FAILURE)
      STOP
    END IF


    ! Compute a "higher resolution" IASI GFT
    WRITE(*, '(/5x,"Computing the high resolution IASI GFT...")')
    ! The number of IFG points
    n_IFG = N_PO2
    ! The number of SRF points (+ve frequencies only)
    n_SPC = ComputeNSPC(n_IFG)
  
    ! Allocate arrays
    ALLOCATE(x(n_IFG), gft(n_IFG), &
             f(n_SPC), srf(n_SPC), &
             STAT=Allocate_Status)
    IF ( Allocate_Status /= 0 ) THEN
      WRITE(msg, '("Error allocating arrays. STAT=",i0)') Allocate_Status
      CALL Display_Message(PROGRAM_NAME, msg, FAILURE)
      STOP
    END IF

    ! Compute optical delay grid and output some numbers
    WRITE(*, '(7x,"...Computing the optical delay grid...")')
    maxX = REAL(n_IFG/2,fp) * dX
    nHalf = n_IFG/2
    x(nHalf:n_IFG) = (/(REAL(i,fp),i=0,nHalf)/)/REAL(nHalf,fp)
    x(1:nHalf-1)   = -x(n_IFG-1:nHalf+1:-1)
    x = x*maxX
    WRITE(*, '(7x,"-Xmax,+Xmax = ",2(1x,es15.8))') x(1),x(n_IFG)
    WRITE(*, '(7x,"dX = ",es15.8)') dX
    WRITE(*, '(7x,"-Xmax-dX = ",es15.8)') x(1)-dX
  
    ! Compute the GFT
    WRITE(*, '(7x,"...Computing the GFT...")')
    gft = ZERO
    i1 = (n_IFG-n_FFT)/2 + 1
    i2 = i1 + n_FFT - 1
    gft(i1:i2) = IASI_GFT(n,x(i1:i2))
    WRITE(*, '(7x,"GFT(-Xeff),GFT(+Xeff) = ",2(1x,es15.8))') gft(i1),gft(i2)
  
    ! Output the GFT result
    CALL Write_File(x, gft)

    ! FFT GFT to a spectrum
    WRITE(*, '(7x,"...FFTing the GFT to a spectrum...")')
    Error_Status = IFGtoSPC(x, gft, f, srf)
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error in IFGtoSPC call'
      CALL Display_Message(PROGRAM_NAME, msg, FAILURE)
      STOP
    END IF

    ! Output the SRF
    CALL Write_File(f, REAL(srf,fp), yi=AIMAG(srf))
  
    ! Deallocate arrays
    DEALLOCATE(x, gft, f, srf, STAT=Allocate_Status)
    IF ( Allocate_Status /= 0 ) THEN
      WRITE(msg, '("Error deallocating arrays. STAT=",i0)') Allocate_Status
      CALL Display_Message(PROGRAM_NAME, msg, FAILURE)
      STOP
    END IF

  END DO Band_Loop
  
CONTAINS

  SUBROUTINE Write_File(x, yr, yi)
    REAL(fp),           INTENT(IN) :: x(:), yr(:)
    REAL(fp), OPTIONAL, INTENT(IN) :: yi(:)
    INTEGER :: FileID, Error_Status
    CHARACTER(256) :: Outfile
    IF ( PRESENT(yi) ) THEN
      WRITE(Outfile,'("IASI_SRF.band",i0,".",i0,".bin")') n, SIZE(x)
    ELSE
      WRITE(Outfile,'("IASI_GFT.band",i0,".",i0,".bin")') n, SIZE(x)
    END IF
    Error_Status = Open_Binary_File( Outfile, &
                                     FileID, &
                                     For_Output=1 )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error opening output results file: '//TRIM(Outfile)
      CALL Display_Message(PROGRAM_NAME, msg, FAILURE)
      STOP
    END IF
    WRITE(FileID) SIZE(x)
    WRITE(FileID) x
    WRITE(FileID) yr
    IF ( PRESENT(yi) ) WRITE(FileID) yi
    CLOSE(FileID)
  END SUBROUTINE Write_File

END PROGRAM Test_IASI
