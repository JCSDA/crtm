!
! FFT_Spectral_Utility
!
! Module containing utility routines for spectral and interferometric manipulation.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 31-Oct-2006
!                       paul.vandelst@noaa.gov
!

MODULE FFT_Spectral_Utility

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds           , ONLY: fp
  USE File_Utility         , ONLY: Get_Lun
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, Display_Message
  USE Fundamental_Constants, ONLY: PI, LN2
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
  USE SPC_IFG_Utility      , ONLY: ComputeF        , &
                                   ComputeMeanDelta, &
                                   ComputeNIFG     , &
                                   ComputeNPoints  , &
                                   ComputeNextPO2  , &
                                   ComputeX        , &
                                   CosFilter
  USE FFT
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Visibility
  ! ----------
  ! Everything is default private
  PRIVATE
  ! Procedures
  PUBLIC :: SPCtoIFG
  PUBLIC :: IFGtoSPC
  PUBLIC :: Fourier_Interpolate
  PUBLIC :: FFT_Spectral_Utility_Version

  ! ---------
  ! Overloads
  ! ---------
  INTERFACE SPCtoIFG
    MODULE PROCEDURE RealSPC_to_ComplexIFG
    MODULE PROCEDURE ComplexSPC_to_ComplexIFG
  END INTERFACE SPCtoIFG

  INTERFACE IFGtoSPC
    MODULE PROCEDURE RealIFG_to_ComplexSPC
    MODULE PROCEDURE ComplexIFG_to_ComplexSPC
  END INTERFACE IFGtoSPC


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO      = 0.0_fp
  REAL(fp), PARAMETER :: POINT5    = 0.5_fp
  REAL(fp), PARAMETER :: ONE       = 1.0_fp
  REAL(fp), PARAMETER :: ONEPOINT5 = 1.5_fp
  REAL(fp), PARAMETER :: TWO       = 2.0_fp
  ! Fourier interpolation default power-of-two
  INTEGER,  PARAMETER :: DEFAULT_PO2 = 14


CONTAINS


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                           ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SPCtoIFG
!
! PURPOSE:
!       Function to FFT an input spectrum (SPC) to a double-sided
!       interferogram (IFG)
!
! CALLING SEQUENCE:
!       Error_Status = SPCtoIFG( f, spc, & ! Input
!                                x, ifg  ) ! Output
!
! INPUTS:
!       f:            The spectral frequency grid.
!                     UNITS:      Inverse centimetres (cm^-1)
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Rank-1 (nSpc)
!                     ATTRIBUTES: INTENT(IN)
!
!       spc:          The spectrum.
!                     UNITS:      Variable
!                     TYPE:       REAL(fp) or COMPLEX(fp)
!                     DIMENSION:  Rank-1 (nSpc)
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       x:            The optical delay grid
!                     UNITS:      Centimetres (cm)
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Rank-1 (nIfg)
!                     ATTRIBUTES: INTENT(OUT)
!
!       ifg:          The double-sided interferogram
!                     UNITS:      Variable
!                     TYPE:       COMPLEX(fp)
!                     DIMENSION:  Rank-1 (nIfg)
!                     ATTRIBUTES: INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the computation was sucessful
!                        == FAILURE an unrecoverable error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION RealSPC_to_ComplexIFG( &
    Frequency    , &  ! Input
    Spectrum     , &  ! Input
    OpticalDelay , &  ! Output
    Interferogram) &  ! Output
  RESULT(Error_Status)
    ! Arguments
    REAL(fp),    INTENT(IN)  :: Frequency(:)
    REAL(fp),    INTENT(IN)  :: Spectrum(:)
    REAL(fp),    INTENT(OUT) :: OpticalDelay(:)
    COMPLEX(fp), INTENT(OUT) :: Interferogram(:)
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'SPCtoIFG (Real->Complex)'
    ! Local variables
    INTEGER :: nSpc, nIfg
    REAL(fp), DIMENSION(SIZE(Interferogram)) :: rIfg
    REAL(fp), DIMENSION(SIZE(Interferogram)) :: iIfg

    ! Set up
    Error_Status = SUCCESS

    ! Check input dimensions
    nSpc = SIZE(Frequency)
    nIfg = SIZE(OpticalDelay)
    Error_Status = CheckSPCIFGdims( nSpc, SIZE(Spectrum)     , &
                                    nIfg, SIZE(Interferogram), &
                                    ROUTINE_NAME               )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Load the work IFG arrays.
    CALL ReflectSpectrum(Spectrum,rIfg,iIfg)

    ! Call the FFT routine
    CALL cfft(nIfg, rIfg, iIfg)

    ! Circularly shift the result to give a
    ! "regular" double-sided interferogram
    rIfg = CSHIFT(rIfg,nSpc)
    iIfg = CSHIFT(iIfg,nSpc)

    ! Load the return interferogram array
    Interferogram = CMPLX(rIfg,iIfg,fp) * ComputeMeanDelta(Frequency)

    ! Compute the optical delay grid
    OpticalDelay = ComputeX(Frequency)

  END FUNCTION RealSPC_to_ComplexIFG


  FUNCTION ComplexSPC_to_ComplexIFG( &
    Frequency    , &  ! Input
    Spectrum     , &  ! Input
    OpticalDelay , &  ! Output
    Interferogram) &  ! Output
  RESULT(Error_Status)
    ! Arguments
    REAL(fp),    INTENT(IN)  :: Frequency(:)
    COMPLEX(fp), INTENT(IN)  :: Spectrum(:)
    REAL(fp),    INTENT(OUT) :: OpticalDelay(:)
    COMPLEX(fp), INTENT(OUT) :: Interferogram(:)
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'SPCtoIFG (Complex->Complex)'
    ! Local variables
    INTEGER :: nSpc, nIfg
    REAL(fp), DIMENSION(SIZE(Interferogram)) :: rIfg
    REAL(fp), DIMENSION(SIZE(Interferogram)) :: iIfg

    ! Set up
    Error_Status = SUCCESS

    ! Check input dimensions
    nSpc = SIZE(Frequency)
    nIfg = SIZE(OpticalDelay)
    Error_Status = CheckSPCIFGdims( nSpc, SIZE(Spectrum)     , &
                                    nIfg, SIZE(Interferogram), &
                                    ROUTINE_NAME               )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Load the work IFG arrays.
    CALL ReflectSpectrum(REAL(Spectrum,fp),rIfg,iIfg,iSpc=AIMAG(Spectrum))

    ! Call the FFT routine
    CALL cfft(nIfg, rIfg, iIfg)

    ! Circularly shift the result to give a
    ! "regular" double-sided interferogram
    rIfg = CSHIFT(rIfg,nSpc)
    iIfg = CSHIFT(iIfg,nSpc)

    ! Load the return interferogram array
    Interferogram = CMPLX(rIfg,iIfg,fp) * ComputeMeanDelta(Frequency)

    ! Compute the optical delay grid
    OpticalDelay = ComputeX(Frequency)

  END FUNCTION ComplexSPC_to_ComplexIFG


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IFGtoSPC
!
! PURPOSE:
!       Function to FFT a double-sided interferogram (IFG) to a spectrum (SPC)
!
! CALLING SEQUENCE:
!       Error_Status = IFGtoSPC( x, ifg, & ! Input
!                                f, spc  ) ! Output
!
! INPUTS:
!       x:            The optical delay grid
!                     UNITS:      Centimetres (cm)
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Rank-1 (nIfg)
!                     ATTRIBUTES: INTENT(IN)
!
!       ifg:          The double-sided interferogram
!                     UNITS:      Variable
!                     TYPE:       REAL(fp) or COMPLEX(fp)
!                     DIMENSION:  Rank-1 (nIfg)
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       f:            The spectral frequency grid.
!                     UNITS:      Inverse centimetres (cm^-1)
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Rank-1 (nSpc)
!                     ATTRIBUTES: INTENT(OUT)
!
!       spc:          The spectrum.
!                     UNITS:      Variable
!                     TYPE:       COMPLEX(fp)
!                     DIMENSION:  Rank-1 (nSpc)
!                     ATTRIBUTES: INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the computation was sucessful
!                        == FAILURE an unrecoverable error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION RealIFG_to_ComplexSPC( &
    OpticalDelay , &
    Interferogram, &
    Frequency    , &
    Spectrum     ) &
  RESULT(Error_Status)
    ! Arguments
    REAL(fp),    INTENT(IN)  :: OpticalDelay(:)
    REAL(fp),    INTENT(IN)  :: Interferogram(:)
    REAL(fp),    INTENT(OUT) :: Frequency(:)
    COMPLEX(fp), INTENT(OUT) :: Spectrum(:)
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'IFGtoSPC (Real->Complex)'
    ! Local variables
    INTEGER :: nSpc, nIfg
    REAL(fp), DIMENSION(SIZE(Interferogram)) :: rIfg
    REAL(fp), DIMENSION(SIZE(Interferogram)) :: iIfg

    ! Set up
    Error_Status = SUCCESS

    ! Check input dimensions
    nSpc = SIZE(Frequency)
    nIfg = SIZE(OpticalDelay)
    Error_Status = CheckSPCIFGdims( nSpc, SIZE(Spectrum)     , &
                                    nIfg, SIZE(Interferogram), &
                                    ROUTINE_NAME               )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Load the work IFG arrays.
    rIfg = Interferogram
    iIfg = ZERO

    ! Circularly shift the real IFG to give an
    ! "FFT-ready" double-sided interferogram
    rIfg = CSHIFT(rIfg,-nSpc)

    ! Call the FFT routine
    CALL dfft(nIfg, rIfg, iIfg)

    ! Compute the frequency grid
    Frequency = ComputeF(OpticalDelay)

    ! Only save the positive frequencies
    Spectrum = CMPLX(rIfg(1:nSpc),iIfg(1:nSpc),fp) / ComputeMeanDelta(Frequency)

  END FUNCTION RealIFG_to_ComplexSPC


  FUNCTION ComplexIFG_to_ComplexSPC( &
    OpticalDelay , &
    Interferogram, &
    Frequency    , &
    Spectrum     ) &
  RESULT(Error_Status)
    ! Arguments
    REAL(fp),    INTENT(IN)  :: OpticalDelay(:)
    COMPLEX(fp), INTENT(IN)  :: Interferogram(:)
    REAL(fp),    INTENT(OUT) :: Frequency(:)
    COMPLEX(fp), INTENT(OUT) :: Spectrum(:)
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'IFGtoSPC (Complex->Complex)'
    ! Local variables
    INTEGER :: nSpc, nIfg
    REAL(fp), DIMENSION(SIZE(Interferogram)) :: rIfg
    REAL(fp), DIMENSION(SIZE(Interferogram)) :: iIfg

    ! Set up
    Error_Status = SUCCESS

    ! Check input dimensions
    nSpc = SIZE(Frequency)
    nIfg = SIZE(OpticalDelay)
    Error_Status = CheckSPCIFGdims( nSpc, SIZE(Spectrum)     , &
                                    nIfg, SIZE(Interferogram), &
                                    ROUTINE_NAME               )
    IF ( Error_Status /= SUCCESS ) RETURN

    ! Load the work IFG arrays.
    rIfg = REAL(Interferogram,fp)
    iIfg = AIMAG(Interferogram)

    ! Circularly shift the result to give an
    ! "FFT-ready" double-sided interferogram
    rIfg = CSHIFT(rIfg,-nSpc)
    iIfg = CSHIFT(iIfg,-nSpc)

    ! Call the FFT routine
    CALL dfft(nIfg, rIfg, iIfg)

    ! Compute the frequency grid
    Frequency = ComputeF(OpticalDelay)

    ! Only save the positive frequencies
    Spectrum = CMPLX(rIfg(1:nSpc),iIfg(1:nSpc),fp) / ComputeMeanDelta(Frequency)

  END FUNCTION ComplexIFG_to_ComplexSPC


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Fourier_Interpolate
!
! PURPOSE:
!       Function to fourier interpolate an input spectrum.
!
! CALLING SEQUENCE:
!       Error_Status = Fourier_Interpolate( InFrequency              , &  ! Input
!                                           InSpectrum               , &  ! Input
!                                           nOutSpectrum             , &  ! Output
!                                           OutFrequency             , &  ! Output
!                                           OutSpectrum              , &  ! Output
!                                           PowerOfTwo  = PowerOfTwo , &  ! Optional input
!                                           FilterWidth = FilterWidth  )  ! Optional input
!
! INPUTS:
!       InFrequency:  The input spectral frequency grid.
!                     UNITS:      Inverse centimetres (cm^-1)
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Rank-1
!                     ATTRIBUTES: INTENT(IN)
!
!       InSpectrum:   The input spectrum to interpolate.
!                     UNITS:      Variable
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Rank-1 (nSpc)
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       nOutSpectrum: The number of output interpolated spectrum points.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
!       OutFrequency: The output interpolated frequency grid.
!                     UNITS:      Inverse centimetres (cm^-1)
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Rank-1
!                     ATTRIBUTES: INTENT(OUT)
!
!       OutSpectrum:  The output interpolated spectrum.
!                     UNITS:      Variable
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Rank-1
!                     ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       PowerOfTwo:   Specify this argument to set the power-of-two used in
!                     zerofilling the spectrum for the interpolation. If not
!                     specified, the value used is 14.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       FilterWidth:  Set this argument to the width to be used to filter
!                     the input spectrum edges. If not specified, the
!                     default width is used.
!                     UNITS:      Inverse centimetres (cm^-1)
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the computation was sucessful
!                        == FAILURE an unrecoverable error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Fourier_Interpolate( &
    InFrequency , &  ! Input
    InSpectrum  , &  ! Input
    nOutSpectrum, &  ! Output
    OutFrequency, &  ! Output
    OutSpectrum , &  ! Output
    PowerOfTwo  , &  ! Optional input
    FilterWidth ) &  ! Optional input
  RESULT( Error_Status )
    ! Arguments
    REAL(fp),               INTENT(IN)  :: InFrequency(:)
    REAL(fp),               INTENT(IN)  :: InSpectrum(:)
    INTEGER,                INTENT(OUT) :: nOutSpectrum
    REAL(fp),               INTENT(OUT) :: OutFrequency(:)
    REAL(fp),               INTENT(OUT) :: OutSpectrum(:)
    INTEGER,      OPTIONAL, INTENT(IN)  :: PowerOfTwo
    REAL(fp),     OPTIONAL, INTENT(IN)  :: FilterWidth
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Fourier_Interpolate'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Allocate_Status
    INTEGER :: nIn, nOut
    INTEGER :: inPO2, outPO2
    INTEGER :: i, i1, i2
    INTEGER :: nFilter
    INTEGER :: nHalf, nSpcMult
    INTEGER :: nSpcIn, nSpcOut
    INTEGER :: nSpcInPO2, nIfgInPO2
    INTEGER :: nSpcOutPO2, nIfgOutPO2
    REAL(fp) :: dF
    REAL(fp) :: width
    REAL(fp) :: f1, f2
    REAL(fp) :: dX, maxX
    REAL(fp),    DIMENSION(:), ALLOCATABLE :: f       ! I/P to SPC->IFG FFT; O/P from IFG->SPC FFT
    REAL(fp),    DIMENSION(:), ALLOCATABLE :: spc     ! I/P to SPC->IFG FFT
    REAL(fp),    DIMENSION(:), ALLOCATABLE :: opd     ! O/P from SPC->IFG FFT; I/P to IFG->SPC FFT
    COMPLEX(fp), DIMENSION(:), ALLOCATABLE :: ifg     ! O/P from SPC->IFG FFT; I/P to IFG->SPC FFT
    COMPLEX(fp), DIMENSION(:), ALLOCATABLE :: cspc    ! O/P from IFG->SPC FFT
    REAL(fp),    DIMENSION(:), ALLOCATABLE :: filter  ! For the cosine filter

    ! Set up
    Error_Status = SUCCESS
    ! ...Check input
    nIn = SIZE(InFrequency)
    IF ( SIZE(InSpectrum) /= nIn ) THEN
      Message = 'Inconsistent input frequency and spectrum sizes.'
      CALL Cleanup(); RETURN
    END IF
    ! ...Check output size
    nOut = SIZE(OutFrequency)
    IF ( SIZE(OutSpectrum) /= nOut ) THEN
      Message = 'Inconsistent output frequency and spectrum sizes.'
      CALL Cleanup(); RETURN
    END IF

    ! Check output power-of-two
    outPO2 = DEFAULT_PO2
    IF ( PRESENT(PowerOfTwo) ) outPO2 = ABS(PowerOfTwo)


    ! Compute filter parameters
    dF = ComputeMeanDelta(InFrequency)
    nFilter = ComputeNPoints(width, dF)
    width = REAL(nFilter-1,fp) * dF


    ! Compute the spectral and interferogram
    ! lengths for the filtered input spectrum
    nSpcIn    = nIn + 2*(nFilter-1)      ! No. of input SPC points bookended with filter
    inPO2     = ComputeNextPO2(nSpcIn)   ! Next power-of-two for input, filtered SPC
    nSpcInPO2 = 2**inPO2 + 1             ! No. of SPC points input to SPC->IFG FFT
    nIfgInPO2 = ComputeNIFG(nSpcInPO2)   ! No. of IFG points output from SPC->IFG FFT

    IF (inPO2 >= outPO2) THEN
      Message = 'Interpolation(zerofill) power of two too small '//&
                'for number of input spectral points.'
      CALL Cleanup(); RETURN
    END IF


    ! Compute the interferogram and spectral
    ! lengths for the zerofilled output spectrum
    nSpcMult = 2**(outPO2-inPO2)          ! The multplier between the in and out PO2 no. of points
    nSpcOut  = (nIn-1)*nSpcMult + 1       ! The number of interpolated output spectral points
    nSpcOutPO2 = 2**outPO2 + 1            ! No. of SPC points output from IFG->SPC FFT
    nIfgOutPO2 = ComputeNIFG(nSpcOutPO2)  ! No. of IFG points input to IFG->SPC FFT

    IF (nOut < nSpcOut ) THEN
      WRITE(Message,'("Output arrays too small (",i0,&
                     &") to contain interpolated data (",i0,")")') nOut, nSpcOut
      CALL Cleanup(); RETURN
    END IF

print *, 'No. of input SPC points:               ', nIn
print *, 'No. of filtered input SPC points:      ', nSpcIn
print *, 'No. of interpolated output SPC points: ', nSpcOut
print *, 'No. of SPC points for SPC->IFG SPC:    ', nSpcInPO2
print *, 'No. of IFG points for SPC->IFG IFG:    ', nIfgInPO2
print *, 'No. of SPC points for IFG->SPC SPC:    ', nSpcOutPO2
print *, 'No. of IFG points for IFG->SPC IFG:    ', nIfgOutPO2

    ! Allocate local work arrays
    ALLOCATE( filter(nFilter) , &
              f(nSpcOutPO2)   , &  ! Input to SPC->IFG FFT; Output from IFG->SPC FFT
              spc(nSpcInPO2)  , &  ! Input to SPC->IFG FFT
              opd(nIfgOutPO2) , &  ! Output from SPC->IFG FFT; Input to IFG->SPC FFT
              ifg(nIfgOutPO2) , &  ! Output from SPC->IFG FFT; Input to IFG->SPC FFT
              cspc(nSpcOutPO2), &  ! Output from IFG->SPC FFT
              STAT = Allocate_Status )
    IF (Allocate_Status /= 0) THEN
      WRITE(Message,'("Error allocating local work arrays. STAT=",i0)') Allocate_Status
      CALL Cleanup(); RETURN
    END IF


    ! Initialise arrays
    spc = ZERO
    ifg = CMPLX(ZERO,ZERO,fp)


    ! Compute frequency grid
    f1 = InFrequency(1) - width
    f2 = f1 + REAL(nSpcInPO2-1,fp)*dF
    f(1:nSpcInPO2) = (/ (REAL(i,fp),i=0,nSpcInPO2-1) /) / REAL(nSpcInPO2-1,fp)
    f(1:nSpcInPO2) = f(1:nSpcInPO2)*(f2-f1) + f1


    ! Slot in spectrum to SPC work array
    spc(nFilter:nFilter+nIn-1) = InSpectrum

    ! Apply filter to spectrum
    !
    ! Front-end
    Error_Status = CosFilter( f(1:nFilter)           , & ! Input
                              filter                 , & ! Output
                              FilterWidth=FilterWidth  ) ! Optional Input
    spc(1:nFilter) = spc(nFilter) * filter

    ! Back-end
    Error_Status = CosFilter( f(nFilter+nIn-1:nSpcIn), & ! Input
                              filter                 , & ! Output
                              FilterWidth=FilterWidth, & ! Optional Input
                              Reverse=.TRUE.           ) ! Optional Input
    spc(nFilter+nIn-1:nSpcIn) = spc(nFilter+nIn-1) * filter


    ! FFT filtered input spectrum to an interferogram
    Error_Status = SPCtoIFG(f(1:nSpcInPO2)  , &  ! Input
                            spc             , &  ! Input
                            opd(1:nIfgInPO2), &  ! Output
                            ifg(1:nIfgInPO2)  )  ! Output
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'SPC->IFG FFT failed.'
      CALL Cleanup(); RETURN
    END IF


    ! Zerofill interferogram by shifting entire array to new ZPD
    ifg = CSHIFT(ifg, -((nIfgOutPO2/2)-(nIfgInPO2/2)))


    ! Compute the optical delay grid for the zerofilled interferogram
    dX    = ComputeMeanDelta(opd(1:nIfgInPO2))
    nHalf = nIfgOutPO2/2
    maxX  = dX * REAL(nHalf,fp)
    opd(nHalf:nIfgOutPO2) = maxX * (/(REAL(i,fp),i=0,nHalf)/) / REAL(nHalf,fp)
    opd(1:nHalf-1)        = -ONE * opd(nIfgOutPO2-1:nHalf+1:-1)


    ! FFT zerofilled interferogram to a spectrum
    Error_Status = IFGtoSPC(opd , ifg , &  ! Input
                            f   , cspc  )  ! Output
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'IFG->SPC FFT failed.'
      CALL Cleanup(); RETURN
    END IF


    ! Fill return arguments
    i1 = (nFilter-1)*nSpcMult + 1
    i2 = i1 + nSpcOut - 1
    nOutSpectrum            = nSpcOut
    OutFrequency(1:nSpcOut) = f(i1:i2) + InFrequency(1) - width
    OutSpectrum(1:nSpcOut)  = REAL(cspc(i1:i2),fp)

  CONTAINS

    SUBROUTINE Cleanup()
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
    END SUBROUTINE Cleanup

  END FUNCTION Fourier_Interpolate




!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       FFT_Spectral_Utility_Version
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL FFT_Spectral_Utility_Version( Id )
!
! OUTPUTS:
!       Id:   Character string containing the version Id information
!             for the module.
!             UNITS:      N/A
!             TYPE:       CHARACTER(*)
!             DIMENSION:  Scalar
!             ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE FFT_Spectral_Utility_Version( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE FFT_Spectral_Utility_Version


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  SUBROUTINE ReflectSpectrum(rSpc, & ! Input
                             rIfg, & ! Output
                             iIfg, & ! Output
                             iSpc  ) ! Optional input
    ! Arguments
    REAL(fp),           INTENT(IN)  :: rSpc(:)
    REAL(fp),           INTENT(OUT) :: rIfg(:)
    REAL(fp),           INTENT(OUT) :: iIfg(:)
    REAL(fp), OPTIONAL, INTENT(IN)  :: iSpc(:)
    ! Local variables
    INTEGER :: nSpc, nIfg

    ! Get sizes
    nSpc = SIZE(rSpc)
    nIfg = SIZE(rIfg)

    ! Load the return IFG arrays. The ASCII art below describes
    ! how the positive frequencies are reflected.
    !
    ! The "x" represent the input spectrum. The "o" represent how
    ! the data is reflected about the Nyquist frequency prior to
    ! calling the FFT routine.
    !
    ! nSpc = 5
    ! nIfg = 2*(nSpc-1) = 8
    !
    !     Zero            nSpc
    !  frequency     (Nyquist pt)     nIfg
    !      |               |           |
    !      v               v           v
    !
    !      x   x   x   x   x   o   o   o
    !
    !          |   |   |       ^   ^   ^
    !          |   |   `------'    |   |
    !          |   `--------------'    |
    !          `----------------------'
    !
    ! The real part
    rIfg(1:nSpc)      = rSpc
    rIfg(nSpc+1:nIfg) = rIfg(nSpc-1:2:-1)

    ! The imaginary part if provided.
    ! Note that the imaginary component of the spectrum is multiplied
    ! by -1. This is to make the input Hermitian so that the result
    ! is a real, asymmetric interferogram.
    IF ( PRESENT(iSpc) ) THEN
      iIfg(1:nSpc)      = iSpc
      iIfg(nSpc+1:nIfg) = -ONE * iIfg(nSpc-1:2:-1)
    ELSE
      iIfg = ZERO
    END IF
  END SUBROUTINE ReflectSpectrum


  FUNCTION CheckSPCIFGdims(nF, nSpc, nX, nIfg, Routine_Name) RESULT(Error_Status)
    ! Arguments
    INTEGER,      INTENT(IN) :: nF, nSpc, nX, nIfg
    CHARACTER(*), INTENT(IN) :: Routine_Name
    ! Function result
    INTEGER :: Error_Status

    ! Set up
    Error_Status = SUCCESS

    ! Check spectral sizes
    IF ( nF /= nSpc ) THEN
      Error_Status = FAILURE
      CALL Display_Message(Routine_Name, &
                           'Size of Frequency and Spectrum arguments inconsistent.', &
                           Error_Status )
      RETURN
    END IF

    ! Check interferogram sizes
    IF ( nX /= nIfg ) THEN
      Error_Status = FAILURE
      CALL Display_Message(Routine_Name, &
                           'Size of OpticalDelay and Interferogram arguments inconsistent.', &
                           Error_Status )
      RETURN
    END IF

    ! Check spectrum/interferogram size consistency
    IF ( nIfg /= ComputeNIFG(nSpc) ) THEN
      Error_Status = FAILURE
      CALL Display_Message(Routine_Name, &
                           'Size of Spectrum/Interferogram arguments inconsistent.', &
                           Error_Status )
      RETURN
    END IF
  END FUNCTION CheckSPCIFGdims

END MODULE FFT_Spectral_Utility
