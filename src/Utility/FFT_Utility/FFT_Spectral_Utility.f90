!
! FFT_Spectral_Utility
!
MODULE FFT_Spectral_Utility

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds           , ONLY: fp=>fp_kind
  USE File_Utility         , ONLY: Get_Lun
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, Display_Message
  USE Fundamental_Constants, ONLY: PI
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
  USE FFT
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Visibility
  ! ----------
  ! Everything is default private
  PRIVATE
  ! Public module parameters
  PUBLIC :: BARTLETT_APOD
  PUBLIC :: WELCH_APOD
  PUBLIC :: CONNES_APOD
  PUBLIC :: COSINE_APOD
  PUBLIC :: HAMMING_APOD
  PUBLIC :: HANNING_APOD
  PUBLIC :: BEER_APOD
  PUBLIC :: STRONGBEER_APOD
  ! Public functions
  PUBLIC :: Sinc
  PUBLIC :: ApodFunction
  PUBLIC :: CosFilter
  PUBLIC :: SPCtoIFG
  PUBLIC :: IFGtoSPC
  PUBLIC :: Fourier_Interpolate

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
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id: $'
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO      = 0.0_fp
  REAL(fp), PARAMETER :: POINT5    = 0.5_fp
  REAL(fp), PARAMETER :: ONE       = 1.0_fp
  REAL(fp), PARAMETER :: ONEPOINT5 = 1.5_fp
  REAL(fp), PARAMETER :: TWO       = 2.0_fp
  REAL(fp), PARAMETER :: POINT46 = 0.46_fp
  REAL(fp), PARAMETER :: POINT54 = 0.54_fp
  REAL(fp), PARAMETER :: LN2 = 0.693147180559945309417232_fp
  ! Apodisation function type values
  INTEGER, PARAMETER :: BARTLETT_APOD = 1
  INTEGER, PARAMETER :: WELCH_APOD    = 2
  INTEGER, PARAMETER :: CONNES_APOD   = 3
  INTEGER, PARAMETER :: COSINE_APOD   = 4
  INTEGER, PARAMETER :: HAMMING_APOD  = 5
  INTEGER, PARAMETER :: HANNING_APOD  = 6
  INTEGER, PARAMETER :: BEER_APOD       = WELCH_APOD
  INTEGER, PARAMETER :: STRONGBEER_APOD = CONNES_APOD
  ! Cos Filter default rolloff width
  REAL(fp), PARAMETER :: DEFAULT_WIDTH = 10.0_fp
  
  
CONTAINS


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  FUNCTION ComputeNIFG(nSpc) RESULT(nIfg)
    ! Arguments
    INTEGER, INTENT(IN) :: nSpc
    ! Function result
    INTEGER :: nIfg
    ! Compute number of IFG points
    nIfg = 2*(nSpc-1)
  END FUNCTION ComputeNIFG
  
  
  FUNCTION ComputeNSPC(nIfg) RESULT(nSpc)
    ! Arguments
    INTEGER, INTENT(IN) :: nIfg
    ! Function result
    INTEGER :: nSpc
    ! Compute number of SPC points
    nSpc = (nIfg/2)+1
  END FUNCTION ComputeNSPC
  
  
  FUNCTION CheckSPCIFGdims(nF, nSpc, nX, nIfg, Routine_Name, Message_Log) &
                          RESULT(Error_Status)
    ! Arguments
    INTEGER,                INTENT(IN) :: nF, nSpc, nX, nIfg
    CHARACTER(*),           INTENT(IN) :: Routine_Name
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    
    ! Set up
    Error_Status = SUCCESS
    
    ! Check spectral sizes
    IF ( nF /= nSpc ) THEN
      Error_Status = FAILURE
      CALL Display_Message(Routine_Name, &
                           'Size of Frequency and Spectrum arguments inconsistent.', &
                           Error_Status, &
                           Message_Log=Message_Log )
      RETURN
    END IF

    ! Check interferogram sizes
    IF ( nX /= nIfg ) THEN
      Error_Status = FAILURE
      CALL Display_Message(Routine_Name, &
                           'Size of OpticalDelay and Interferogram arguments inconsistent.', &
                           Error_Status, &
                           Message_Log=Message_Log )
      RETURN
    END IF

    ! Check spectrum/interferogram size consistency
    IF ( nIfg /= ComputeNIFG(nSpc) ) THEN
      Error_Status = FAILURE
      CALL Display_Message(Routine_Name, &
                           'Size of Spectrum/Interferogram arguments inconsistent.', &
                           Error_Status, &
                           Message_Log=Message_Log )
      RETURN
    END IF 
  END FUNCTION CheckSPCIFGdims


  FUNCTION ComputeMaxX(f) RESULT(maxX)
    ! Arguments
    REAL(fp), INTENT(IN) :: f(:)
    ! Function result
    REAL(fp) :: maxX
    ! Local variables
    INTEGER  :: nF
    REAL(fp) :: dF
    
    ! Compute average frequency spacing
    nF = SIZE(f)
    dF = SUM((f(2:nF)-f(1:nF-1))) / REAL(nF-1,fp)
    
    ! Compute the maximum OPD
    maxX = ONE/(TWO*dF)
  END FUNCTION ComputeMaxX
  
  
  FUNCTION ComputeNyquistF(x) RESULT(nyquistF)
    ! Arguments
    REAL(fp), INTENT(IN) :: x(:)
    ! Function result
    REAL(fp) :: nyquistF
    ! Local variables
    INTEGER  :: nX
    REAL(fp) :: dX
    
    ! Compute average optical delay
    nX = SIZE(x)
    dX = SUM((x(2:nX)-x(1:nX-1))) / REAL(nX-1,fp)
    
    ! Compute the Nyquist frequency
    nyquistF = ONE/(TWO*dX)
  END FUNCTION ComputeNyquistF
  
  
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


  FUNCTION ComputeX(f) RESULT(x)
    ! Arguments
    REAL(fp), INTENT(IN) :: f(:)
    ! Function result
    REAL(fp), DIMENSION(2*(SIZE(f)-1)) :: x
    ! Local variables
    INTEGER  :: i, nF, nX
    REAL(fp) :: maxX
    
    ! Get sizes
    nF = SIZE(f)
    nX = ComputeNIFG(nF)
    
    ! Compute maximum optical delay
    maxX = ComputeMaxX(f)
    
    ! Compute +ve delays
    x(nF-1:nX) = maxX * (/ (REAL(i,fp),i=0,nF-1) /) / REAL(nF-1,fp)

    ! Reflect for -ve delays
    x(1:nF-2)  = -ONE * x(nX-1:nF:-1)
  END FUNCTION ComputeX


  FUNCTION ComputeF(x) RESULT(f)
    ! Arguments
    REAL(fp), INTENT(IN) :: x(:)
    ! Function result
    REAL(fp), DIMENSION((SIZE(x)/2)+1) :: f
    ! Local variables
    INTEGER  :: i, nF, nX
    REAL(fp) :: nyquistF
    
    ! Get sizes
    nX = SIZE(x)
    nF = ComputeNSPC(nX)
    
    ! Compute Nyquist frequency
    nyquistF = ComputeNyquistF(x)
    
    ! Compute +ve frequencies only
    f = nyquistF * (/ (REAL(i,fp),i=0,nF-1) /) / REAL(nF-1,fp)
  END FUNCTION ComputeF


  FUNCTION ComputeMeanDelta(a) RESULT(dA)
    ! Arguments
    REAL(fp), INTENT(IN) :: a(:)
    ! Function result
    REAL(fp) :: dA
    ! Local variables
    INTEGER :: n
    
    ! Compute the average interval
    n = SIZE(a)
    dA = SUM((a(2:n)-a(1:n-1))) / REAL(n-1,fp)
  END FUNCTION ComputeMeanDelta
  
  
  FUNCTION ComputeNPoints(deltaA, dA) RESULT(nPoints)
    ! Arguments
    REAL(fp), INTENT(IN) :: deltaA  !<- - - - - ->!
    REAL(fp), INTENT(IN) :: dA      !  -->| |<--
    ! Function result
    INTEGER :: nPoints
    
    ! Compute the number of points within a range, deltaA,
    ! spaced at intervals of dA
    nPoints = INT( ( deltaA / dA ) + ONEPOINT5 )
  END FUNCTION ComputeNPoints
  
  
  FUNCTION ComputeNextPO2(n) RESULT(po2)
    ! Arguments
    INTEGER, INTENT(IN) :: n
    ! Function result
    INTEGER :: po2
    ! Local variables
    REAL(fp) :: x
    INTEGER :: ix
    
    ! The base-2 log of n
    x = LOG(REAL(n,fp))/LN2
    ix = INT(x)
    
    ! Check if we have a po2
    IF ( REAL(ix,fp) .EqualTo. x ) THEN
      po2 = ix
    ELSE
      po2 = ix + 1
    END IF
  END FUNCTION ComputeNextPO2


!================
! DEBUG ROUTINES
!================


  SUBROUTINE DEBUG_DumpReal(x,y,fileName)
    REAL(fp),     INTENT(IN) :: x(:)
    REAL(fp),     INTENT(IN) :: y(:)
    CHARACTER(*), INTENT(IN) :: fileName
    INTEGER :: i, n, fileId
    n = SIZE(x)
    fileId = Get_Lun()
    OPEN(fileId,FILE=fileName,STATUS='UNKNOWN')
    WRITE(fileId,*) n
    DO i = 1, n
      WRITE(fileId,*) x(i), y(i)
    END DO
    CLOSE(fileId)
  END SUBROUTINE DEBUG_DumpReal


  SUBROUTINE DEBUG_DumpComplex(x,y,fileName)
    REAL(fp),     INTENT(IN) :: x(:)
    COMPLEX(fp),  INTENT(IN) :: y(:)
    CHARACTER(*), INTENT(IN) :: fileName
    INTEGER :: i, n, fileId
    n = SIZE(x)
    fileId = Get_Lun()
    OPEN(fileId,FILE=fileName,STATUS='UNKNOWN')
    WRITE(fileId,*) n
    DO i = 1, n
      WRITE(fileId,*) x(i), REAL(y(i),fp), AIMAG(y(i))
    END DO
    CLOSE(fileId)
  END SUBROUTINE DEBUG_DumpComplex


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                           ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  FUNCTION Sinc(x, Normalized) RESULT(y)
    ! Arguments
    REAL(fp),          INTENT(IN) :: x(:)
    INTEGER, OPTIONAL, INTENT(IN) :: Normalized
    ! Function result
    REAL(fp), DIMENSION(SIZE(x)) :: y
    ! Local variables
    REAL(fp), DIMENSION(SIZE(x)) :: xScale

    ! Check normalisation
    xScale = x
    IF ( PRESENT( Normalized ) ) THEN
      IF ( Normalized == 1 ) xScale = PI*x
    END IF
    
    ! Compute Sinc function    
    WHERE( xScale /= ZERO )
      y = SIN(xScale)/xScale
    ELSEWHERE
      y = ONE
    END WHERE
  END FUNCTION Sinc
  

  FUNCTION ApodFunction(n,apodType) RESULT(y)
    ! Arguments
    INTEGER,           INTENT(IN) :: n
    INTEGER, OPTIONAL, INTENT(IN) :: apodType
    ! Function result
    REAL(fp), DIMENSION(n) :: y
    ! Local variables
    INTEGER :: aType
    INTEGER :: i, nHalf
    REAL(fp) :: aMax
    REAL(fp), DIMENSION(n/2+1) :: a

    ! Set type
    aType = -1 ! Doesn't match any defined type, so force default
    IF ( PRESENT(apodType) ) aType = apodType
    
    ! Get size
    nHalf=n/2
    
    ! Fill a grid array
    a    = (/(REAL(i,fp),i=0,nHalf)/)
    aMax = REAL(nHalf,fp)

    ! Compute apodisation function for +ve delays
    !
    ! The formulae taken from:
    !   Weisstein, Eric W. "Apodization Function."
    !   From MathWorld--A Wolfram Web Resource.
    !   http://mathworld.wolfram.com/ApodizationFunction.html
    !
    SELECT CASE(aType)
      CASE(BARTLETT_APOD)
        y(nHalf:n) = ONE - (a/aMax)
      CASE(WELCH_APOD)
        y(nHalf:n) = ONE - (a/aMax)**2
      CASE(COSINE_APOD)
        y(nHalf:n) = COS(POINT5*PI*a/aMax)
      CASE(HAMMING_APOD)
        y(nHalf:n) = POINT54 + (POINT46*COS(PI*a/aMax))
      CASE(HANNING_APOD)
        y(nHalf:n) = POINT5*(ONE + COS(PI*a/aMax))
      CASE DEFAULT ! Default function is CONNES_APOD
        y(nHalf:n) = (ONE - (a/aMax)**2)**2
    END SELECT

    ! Reflect for -ve delays
    y(1:nHalf-1) = y(n-1:nHalf+1:-1)
  END FUNCTION ApodFunction
  
  
  FUNCTION CosFilter(Frequency  , & ! Input
                     Filter     , & ! Output
                     FilterWidth, & ! Optional Input
                     Reverse    , & ! Optional Input
                     nFilter    , & ! Optional output
                     RCS_Id     , & ! Revision control
                     Message_Log) & ! Error messaging
                    RESULT(Error_Status)
    ! Arguments
    REAL(fp),               INTENT(IN)  :: Frequency(:)
    REAL(fp),               INTENT(OUT) :: Filter(:)
    REAL(fp),     OPTIONAL, INTENT(IN)  :: FilterWidth
    INTEGER,      OPTIONAL, INTENT(IN)  :: Reverse
    INTEGER,      OPTIONAL, INTENT(OUT) :: nFilter
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Locall parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CosFilter'
    ! Local variables
    REAL(fp) :: Width
    INTEGER  :: n, nPts
    INTEGER  :: i1, i2, i3
    REAL(fp) :: dF

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check input
    n = SIZE(Frequency)
    IF ( SIZE(Filter) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message(ROUTINE_NAME, &
                           'Size of Frequency and Filter arguments inconsistent.', &
                           Error_Status, &
                           Message_Log=Message_Log )
      RETURN
    END IF

    ! Filter width
    Width = DEFAULT_WIDTH
    IF (PRESENT(FilterWidth)) Width=FilterWidth

    ! Mean frequency interval
    dF = ComputeMeanDelta(Frequency)

    ! How many points required for filter?
    nPts = ComputeNPoints(Width, dF)
    ! Not enough?
    IF(nPts <= 1)THEN
      Error_Status = FAILURE
      CALL Display_Message(ROUTINE_NAME, &
                           'Number of filter points too small', &
                           Error_Status, &
                           Message_Log=Message_Log )
      RETURN
    END IF
    ! Too many?
    IF(nPts > n)THEN
      Error_Status = FAILURE
      CALL Display_Message(ROUTINE_NAME, &
                           'Number of filter points too large', &
                           Error_Status, &
                           Message_Log=Message_Log )
      RETURN
    END IF
    
    ! Return number of points if necessary
    IF ( PRESENT(nFilter) ) nFilter = nPts

    ! Define array triplet
    i1 = nPts
    i2 = 1
    i3 = -1
    IF (PRESENT(Reverse)) THEN
      IF (Reverse==1) THEN
        i1 = n-nPts+1
        i2 = n
        i3 = 1
      END IF
    END IF

    ! Initialise filter
    Filter = ONE
    ! Compute filter
    Filter(i1:i2:i3) = POINT5 * (ONE + COS((Frequency(i1:i2:i3)-Frequency(i1))*PI/Width))
    
  END FUNCTION CosFilter 


  FUNCTION RealSPC_to_ComplexIFG( Frequency    , &
                                  Spectrum     , &
                                  OpticalDelay , &
                                  Interferogram, &
                                  RCS_Id       , &
                                  Message_Log  ) &
                                RESULT(Error_Status)
    ! Arguments
    REAL(fp),               INTENT(IN)  :: Frequency(:)
    REAL(fp),               INTENT(IN)  :: Spectrum(:)
    REAL(fp),               INTENT(OUT) :: OpticalDelay(:)
    COMPLEX(fp),            INTENT(OUT) :: Interferogram(:)
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
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
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check input dimensions
    nSpc = SIZE(Frequency)
    nIfg = SIZE(OpticalDelay)
    Error_Status = CheckSPCIFGdims( nSpc, SIZE(Spectrum)     , &
                                    nIfg, SIZE(Interferogram), &
                                    ROUTINE_NAME             , &
                                    Message_Log=Message_Log    )
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

  
  FUNCTION ComplexSPC_to_ComplexIFG( Frequency    , &
                                     Spectrum     , &
                                     OpticalDelay , &
                                     Interferogram, &
                                     RCS_Id       , &
                                     Message_Log  ) &
                                   RESULT(Error_Status)
    ! Arguments
    REAL(fp),               INTENT(IN)  :: Frequency(:)
    COMPLEX(fp),            INTENT(IN)  :: Spectrum(:)
    REAL(fp),               INTENT(OUT) :: OpticalDelay(:)
    COMPLEX(fp),            INTENT(OUT) :: Interferogram(:)
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
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
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check input dimensions
    nSpc = SIZE(Frequency)
    nIfg = SIZE(OpticalDelay)
    Error_Status = CheckSPCIFGdims( nSpc, SIZE(Spectrum)     , &
                                    nIfg, SIZE(Interferogram), &
                                    ROUTINE_NAME             , &
                                    Message_Log=Message_Log    )
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


  FUNCTION RealIFG_to_ComplexSPC( OpticalDelay , &
                                  Interferogram, &
                                  Frequency    , &
                                  Spectrum     , &
                                  RCS_Id       , &
                                  Message_Log  ) &
                                RESULT(Error_Status)
    ! Arguments
    REAL(fp),               INTENT(IN)  :: OpticalDelay(:)
    REAL(fp),               INTENT(IN)  :: Interferogram(:)
    REAL(fp),               INTENT(OUT) :: Frequency(:)
    COMPLEX(fp),            INTENT(OUT) :: Spectrum(:)
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
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
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check input dimensions
    nSpc = SIZE(Frequency)
    nIfg = SIZE(OpticalDelay)
    Error_Status = CheckSPCIFGdims( nSpc, SIZE(Spectrum)     , &
                                    nIfg, SIZE(Interferogram), &
                                    ROUTINE_NAME             , &
                                    Message_Log=Message_Log    )
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


  FUNCTION ComplexIFG_to_ComplexSPC( OpticalDelay , &
                                     Interferogram, &
                                     Frequency    , &
                                     Spectrum     , &
                                     RCS_Id       , &
                                     Message_Log  ) &
                                   RESULT(Error_Status)
    ! Arguments
    REAL(fp),               INTENT(IN)  :: OpticalDelay(:)
    COMPLEX(fp),            INTENT(IN)  :: Interferogram(:)
    REAL(fp),               INTENT(OUT) :: Frequency(:)
    COMPLEX(fp),            INTENT(OUT) :: Spectrum(:)
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
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
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check input dimensions
    nSpc = SIZE(Frequency)
    nIfg = SIZE(OpticalDelay)
    Error_Status = CheckSPCIFGdims( nSpc, SIZE(Spectrum)     , &
                                    nIfg, SIZE(Interferogram), &
                                    ROUTINE_NAME             , &
                                    Message_Log=Message_Log    )
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


  FUNCTION Fourier_Interpolate( InFrequency , &
                                InSpectrum  , &
                                OutFrequency, &
                                OutSpectrum , &
                                FilterWidth , & ! Optional input
                                RCS_Id      , &
                                Message_Log ) &
                              RESULT( Error_Status )
    ! Arguments
    REAL(fp),               INTENT(IN)  :: InFrequency(:)  
    REAL(fp),               INTENT(IN)  :: InSpectrum(:)   
    REAL(fp),               INTENT(OUT) :: OutFrequency(:) 
    REAL(fp),               INTENT(OUT) :: OutSpectrum(:)  
    REAL(fp),     OPTIONAL, INTENT(IN)  :: FilterWidth
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Fourier_Interpolate'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: Allocate_Status
    LOGICAL :: applyFilter
    INTEGER :: nIn, nOut
    INTEGER :: i, nFilter, nSpc, nIfg, nPO2, nIfgPO2, nHalf
    REAL(fp) :: dF
    REAL(fp) :: width
    REAL(fp) :: f1, f2
    REAL(fp) :: dX, maxX
    REAL(fp),    DIMENSION(:), ALLOCATABLE :: f    ! For filtered spectrum
    REAL(fp),    DIMENSION(:), ALLOCATABLE :: spc  ! For filtered spectrum
    REAL(fp),    DIMENSION(:), ALLOCATABLE :: opd  ! Intermediate FFT output
    COMPLEX(fp), DIMENSION(:), ALLOCATABLE :: ifg  ! Intermediate FFT output
    COMPLEX(fp), DIMENSION(:), ALLOCATABLE :: cspc ! Intermediate FFT output
    REAL(fp),    DIMENSION(:), ALLOCATABLE :: filter  ! For the cosine filter

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check input
    nIn = SIZE(InFrequency)
    IF ( SIZE(InSpectrum) /= nIn ) THEN
      Error_Status = FAILURE
      CALL Display_Message(ROUTINE_NAME, &
                           'Inconsistent input frequency and spectrum sizes.', &
                           Error_Status, &
                           Message_Log=Message_Log )
      RETURN
    END IF
    
    ! Check output size
    nOut = SIZE(OutFrequency)
    IF ( SIZE(OutSpectrum) /= nOut ) THEN
      Error_Status = FAILURE
      CALL Display_Message(ROUTINE_NAME, &
                           'Inconsistent output frequency and spectrum sizes.', &
                           Error_Status, &
                           Message_Log=Message_Log )
      RETURN
    END IF

    ! Check filter width
    width = DEFAULT_WIDTH
    applyFilter = .TRUE.
    IF ( PRESENT(FilterWidth) ) THEN
      IF ( FilterWidth > ZERO ) THEN
        width = FilterWidth
      ELSE
        applyFilter = .FALSE.
      END IF
    END IF

    ! Compute required lengths of arrays
    dF = ComputeMeanDelta(InFrequency)
    nFilter = ComputeNPoints(width, dF)
    width = REAL(nFilter-1,fp) * dF
    
    nSpc = nIn + 2*(nFilter-1)
!    nIfg = ComputeNIFG(nSpc) 
    nPO2 = 2**(ComputeNextPO2(nSpc)) + 1
    nIfgPO2 = ComputeNIFG(nPO2) 
    nIfg = ComputeNIFG(nOut) 

print *, nSpc, nIfg, nPO2
    
    ! Allocate arrays
    ALLOCATE( f(nPO2), spc(nPO2), opd(nIfg), ifg(nIfg), filter(nFilter), cspc(nOut), &
              STAT = Allocate_Status )
    
    ! Compute frequency grid
    f1 = InFrequency(1)   - width
    f2 = f1 + REAL(nPO2-1,fp)*dF
    f = (/ (REAL(i,fp),i=0,nPO2-1) /) / REAL(nPO2-1,fp) 
    f = f*(f2-f1) + f1
    
    ! Slot in spectrum to work array
    spc(nFilter:nFilter+nIn-1) = InSpectrum
    
    ! Apply filter to spectrum
    !
    ! Front-end
    Error_Status = CosFilter( f(1:nFilter)           , & ! Input
                              filter                 , & ! Output
                              FilterWidth=width      , & ! Optional Input
                              Message_Log=Message_Log  ) ! Error messaging
    spc(1:nFilter) = spc(nFilter) * filter

    ! Back-end
    Error_Status = CosFilter( f(nFilter+nIn-1:nSpc)  , & ! Input
                              filter                 , & ! Output
                              FilterWidth=width      , & ! Optional Input
                              Reverse=1              , & ! Optional Input
                              Message_Log=Message_Log  ) ! Error messaging
    spc(nFilter+nIn-1:nSpc) = spc(nFilter+nIn-1) * filter

outfrequency=zero
outfrequency(1:npo2)=f
outspectrum=zero
outspectrum(1:npo2)=spc

ifg=CMPLX(ZERO,ZERO,fp)

    ! FFT to an interferogram
    Error_Status = SPCtoIFG(f,spc,opd(1:nIfgPO2),ifg(1:nIfgPO2))
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message(ROUTINE_NAME, &
                           'SPC->IFG FFT failed.', &
                           Error_Status, &
                           Message_Log=Message_Log )
      RETURN
    END IF

call debug_dumpcomplex(opd(1:nIfgPO2),ifg(1:nIfgPO2),'fint_ifg_dump1.dat')
    
    ! Shift interferogram to new ZPD
    ifg = CSHIFT(ifg, -((nIfg/2)-(nIfgPO2/2)))

    ! Fill out optical delay grid
    nHalf=nIfg/2
    dX = ComputeMeanDelta(opd(1:nIfgPO2))
    maxX = dX * REAL(nHalf,fp)
    opd(nHalf:nIfg) = maxX * (/ (REAL(i,fp),i=0,nHalf+1) /) / REAL(nHalf,fp)
    opd(1:nHalf-1)  = -ONE * opd(nIfg-1:nHalf+1:-1)

call debug_dumpcomplex(opd,ifg,'fint_ifg_dump2.dat')
    
    ! FFT to a spectrum
    Error_Status = IFGtoSPC(opd,REAL(ifg,fp),OutFrequency,cspc)
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message(ROUTINE_NAME, &
                           'IFG->SPC FFT failed.', &
                           Error_Status, &
                           Message_Log=Message_Log )
      RETURN
    END IF

outfrequency=outfrequency+f(1)
outspectrum=real(cspc,fp)
call debug_dumpcomplex(OutFrequency,cspc,'fint_cspc_dump.dat')


!1) copy input to tmp spc arrays
!1a) apply cos filter
!2) fft to ifg
!3) zerofill the interferogram
!4) compute zerofilled ifg optical delay grid
!5) fft to spc
!6) translate output frequency grid as required
!6a) truncate output to original bandwidth (due to cos filter)

!First use ifg/opd array slices in fft calls


  END FUNCTION Fourier_Interpolate
  
END MODULE FFT_Spectral_Utility
