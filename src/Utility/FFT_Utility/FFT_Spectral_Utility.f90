!
! FFT_Spectral_Utility
!
MODULE FFT_Spectral_Utility

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds,            ONLY: fp=>fp_kind
  USE Message_Handler,       ONLY: SUCCESS, FAILURE, Display_Message
  USE Fundamental_Constants, ONLY: PI
  USE FFT
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Visibility
  ! ----------
  PRIVATE
  PUBLIC :: Sinc
  PUBLIC :: CosFilter
  PUBLIC :: SPCtoIFG
  PUBLIC :: IFGtoSPC

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
  REAL(fp), PARAMETER :: ZERO          = 0.0_fp
  REAL(fp), PARAMETER :: POINT5        = 0.5_fp
  REAL(fp), PARAMETER :: ONE           = 1.0_fp
  REAL(fp), PARAMETER :: ONEpointFIVE  = 1.5_fp
  REAL(fp), PARAMETER :: TWO           = 2.0_fp
  
CONTAINS


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

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
    IF ( nIfg /= (nSpc-1)*2 ) THEN
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
    nX = 2*(nF-1)
    
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
    nF = (nX/2)+1
    
    ! Compute Nyquist frequency
    nyquistF = ComputeNyquistF(x)
    
    ! Compute +ve frequencies only
    f = nyquistF * (/ (REAL(i,fp),i=0,nF-1) /) / REAL(nF-1,fp)
  END FUNCTION ComputeF


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
  

  FUNCTION CosFilter(Frequency   , & ! Input
                     Filter      , & ! Output
                     RolloffWidth, & ! Optional Input
                     Reverse     , & ! Optional Input
                     RCS_Id      , & ! Revision control
                     Message_Log ) & ! Error messaging
                    RESULT(Error_Status)
    ! Arguments
    REAL(fp),               INTENT(IN)  :: Frequency(:)
    REAL(fp),               INTENT(OUT) :: Filter(:)
    REAL(fp),     OPTIONAL, INTENT(IN)  :: RolloffWidth
    INTEGER,      OPTIONAL, INTENT(IN)  :: Reverse
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Locall parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CosFilter'
    REAL(fp),     PARAMETER :: DEFAULT_WIDTH = 10.0_fp
    ! Local variables
    REAL(fp) :: Width
    INTEGER  :: n, nFilterPts
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
    IF (PRESENT(RolloffWidth)) Width=RolloffWidth

    ! Mean frequency interval
    dF = SUM( (Frequency(2:n)-Frequency(1:n-1)) ) / REAL(n-1,fp)

    ! How many points required for filter?
    nFilterPts = INT( ( Width / dF ) + ONEpointFIVE )
    ! Note enough?
    IF(nFilterPts <= 1)THEN
      Error_Status = FAILURE
      CALL Display_Message(ROUTINE_NAME, &
                           'Number of filter points too small', &
                           Error_Status, &
                           Message_Log=Message_Log )
      RETURN
    END IF
    ! Too many?
    IF(nFilterPts > n)THEN
      Error_Status = FAILURE
      CALL Display_Message(ROUTINE_NAME, &
                           'Number of filter points too large', &
                           Error_Status, &
                           Message_Log=Message_Log )
      RETURN
    END IF

    ! Define array triplet
    i1 = nFilterPts
    i2 = 1
    i3 = -1
    IF (PRESENT(Reverse)) THEN
      IF (Reverse==1) THEN
        i1 = n-nFilterPts+1
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
    Interferogram = CMPLX(rIfg,iIfg,fp)

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
    Interferogram = CMPLX(rIfg,iIfg,fp)

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

    ! Only save the positive frequencies
    Spectrum = CMPLX(rIfg(1:nSpc),iIfg(1:nSpc),fp)

    ! Compute the frequency grid
    Frequency = ComputeF(OpticalDelay)
    
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

    ! Only save the positive frequencies
    Spectrum = CMPLX(rIfg(1:nSpc),iIfg(1:nSpc),fp)

    ! Compute the frequency grid
    Frequency = ComputeF(OpticalDelay)
    
  END FUNCTION ComplexIFG_to_ComplexSPC

END MODULE FFT_Spectral_Utility
