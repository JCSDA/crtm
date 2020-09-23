!
! SPC_IFG_Utility
!
! Module containing utility routines for spectrum (SPC) and
! interferogram (IFG) manipulation.
!
!

MODULE SPC_IFG_Utility

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds           , ONLY: fp
  USE File_Utility         , ONLY: Get_Lun
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, Display_Message
  USE Fundamental_Constants, ONLY: PI, LN2
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Visibility
  ! ----------
  ! Everything is default private
  PRIVATE
  ! Parameters
  ! Procedures
  PUBLIC :: ComputeNIFG
  PUBLIC :: ComputeNSPC
  PUBLIC :: ComputeMaxX
  PUBLIC :: ComputeNyquistF
  PUBLIC :: ComputeX
  PUBLIC :: ComputeF
  PUBLIC :: ComputeMeanDelta
  PUBLIC :: ComputeNPoints
  PUBLIC :: ComputeIndex
  PUBLIC :: ComputeNextPO2
  PUBLIC :: Sinc
  PUBLIC :: CosFilter
  PUBLIC :: SPC_IFG_Utility_Version


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
  ! Cos Filter default rolloff width
  REAL(fp), PARAMETER :: DEFAULT_WIDTH = 10.0_fp


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
!       ComputeNIFG
!
! PURPOSE:
!       Pure function to compute the number of interferogram points given the
!       number of spectral points
!
! CALLING SEQUENCE:
!       nIfg = ComputeNIFG(nSpc)
!
! INPUTS:
!       nSpc:   Number of points in a spectrum.
!               UNITS:      N/A
!               TYPE:       INTEGER
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       nIfg:   Number of points in the associated inteferogram.
!               UNITS:      N/A
!               TYPE:       INTEGER
!               DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION ComputeNIFG(nSpc) RESULT(nIfg)
    ! Arguments
    INTEGER, INTENT(IN) :: nSpc
    ! Function result
    INTEGER :: nIfg
    ! Compute number of IFG points
    nIfg = 2*(nSpc-1)
  END FUNCTION ComputeNIFG


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ComputeNSPC
!
! PURPOSE:
!       Pure function to compute the number of spectral points points given
!       the number of interferogram points
!
! CALLING SEQUENCE:
!       nSpc = ComputeNSPC(nIFg)
!
! INPUTS:
!       nIfg:   Number of points in the inteferogram.
!               UNITS:      N/A
!               TYPE:       INTEGER
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       nSpc:   Number of points in the associated spectrum.
!               UNITS:      N/A
!               TYPE:       INTEGER
!               DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION ComputeNSPC(nIfg) RESULT(nSpc)
    ! Arguments
    INTEGER, INTENT(IN) :: nIfg
    ! Function result
    INTEGER :: nSpc
    ! Compute number of SPC points
    nSpc = (nIfg/2)+1
  END FUNCTION ComputeNSPC


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ComputeMaxX
!
! PURPOSE:
!       Pure function to compute the maximum optical delay corresponding
!       to a spectral frequency grid.
!
! CALLING SEQUENCE:
!       maxX = ComputeMaxX(f)
!
! INPUTS:
!       f:      Evenly spaced spectral frequency grid.
!               UNITS:      Inverse centimetres (cm^-1)
!               TYPE:       REAL(fp)
!               DIMENSION:  Rank-1
!               ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       maxX:   Maximum optical delay for an interferogram corresponding with
!               the input frequency grid.
!               UNITS:      Centimetres (cm)
!               TYPE:       REAL(fp)
!               DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION ComputeMaxX(f) RESULT(maxX)
    ! Arguments
    REAL(fp), INTENT(IN) :: f(:)
    ! Function result
    REAL(fp) :: maxX
    ! Compute the maximum OPD
    maxX = ONE/(TWO*ComputeMeanDelta(f))
  END FUNCTION ComputeMaxX


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ComputeNyquistF
!
! PURPOSE:
!       Pure function to compute the Nyquist frequency corresponding
!       to an interferometric optical delay grid.
!
! CALLING SEQUENCE:
!       nyquistF = ComputeNyquistF(x)
!
! INPUTS:
!       x:         Evenly spaced interferometric optical delay grid
!                  UNITS:      Centimetres (cm)
!                  TYPE:       REAL(fp)
!                  DIMENSION:  Rank-1
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       nyquistF:  Nyquist ("maximum") frequency for a spectrum corresponding with
!                  the input optical delay grid.
!                  UNITS:      Inverse centimetres (cm^-1)
!                  TYPE:       REAL(fp)
!                  DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION ComputeNyquistF(x) RESULT(nyquistF)
    ! Arguments
    REAL(fp), INTENT(IN) :: x(:)
    ! Function result
    REAL(fp) :: nyquistF
    ! Compute the Nyquist frequency
    nyquistF = ONE/(TWO*ComputeMeanDelta(x))
  END FUNCTION ComputeNyquistF


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ComputeX
!
! PURPOSE:
!       Pure function to compute a double-sided optical delay grid for a
!       given spectral frequency grid.
!
! CALLING SEQUENCE:
!       x = ComputeX(f)
!
! INPUTS:
!       f:         Evenly spaced spectral frequency grid
!                  UNITS:      Inverse centimetres (cm^-1)
!                  TYPE:       REAL(fp)
!                  DIMENSION:  Rank-1 (nSpc)
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       x:         Double-sided optical delay grid corresponding to the input
!                  frequency grid.
!                  UNITS:      Centimetres (cm)
!                  TYPE:       REAL(fp)
!                  DIMENSION:  Rank-1 (nIfg)
!
! COMMENTS:
!       Use the ComputeNIFG function to compute the size of the result.
!
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION ComputeX(f) RESULT(x)
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


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ComputeF
!
! PURPOSE:
!       Pure function to compute a spectral frequency grid for a given
!       double-sided optical delay grid.
!
! CALLING SEQUENCE:
!       f = ComputeF(x)
!
! INPUTS:
!       x:         Double-sided optical delay grid.
!                  UNITS:      Centimetres (cm)
!                  TYPE:       REAL(fp)
!                  DIMENSION:  Rank-1 (nIfg)
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       f:         Evenly spaced spectral frequency grid corresponing to the
!                  input optical delay grid.
!                  UNITS:      Inverse centimetres (cm^-1)
!                  TYPE:       REAL(fp)
!                  DIMENSION:  Rank-1 (nSpc)
!
! COMMENTS:
!       Use the ComputeNSPC function to compute the size of the result.
!
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION ComputeF(x) RESULT(f)
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


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ComputeMeanDelta
!
! PURPOSE:
!       Pure function to compute the mean spacing between points in a
!       given array that is monotonically increasing.
!
! CALLING SEQUENCE:
!       dA = ComputeMeanDelta(A)
!
! INPUTS:
!       a:         Input array of evenly spaced, monotonically increasing
!                  data points.
!                  UNITS:      Variable
!                  TYPE:       REAL(fp)
!                  DIMENSION:  Rank-1
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       dA:        Mean point spacing of the input array.
!                  UNITS:      Same as input.
!                  TYPE:       REAL(fp)
!                  DIMENSION:  Scalar
!
! COMMENTS:
!       This function is an alternative to simply doing,
!         dA = a(2)-a(1)
!       I've found a mean-delta approach to give more consistent results
!       for different precisions.
!
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION ComputeMeanDelta(a) RESULT(dA)
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


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ComputeNPoints
!
! PURPOSE:
!       Pure function to compute the number of points within a range, deltaA,
!       spaced at intervals of dA.
!
! CALLING SEQUENCE:
!       n = ComputeNPoints(deltaA, dA)
!
! INPUTS:
!       deltaA:    Range over which the data are spaced.
!                  UNITS:      Variable
!                  TYPE:       REAL(fp)
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
!       dA:        Point spacing of the data.
!                  UNITS:      Same as input.
!                  TYPE:       REAL(fp)
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       n:         Number of points in the specified range.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION ComputeNPoints(deltaA, dA) RESULT(nPoints)
    ! Arguments
    REAL(fp), INTENT(IN) :: deltaA  !<- - - - - ->!
    REAL(fp), INTENT(IN) :: dA      !  -->| |<--
    ! Function result
    INTEGER :: nPoints
    ! Compute the number of points within a range, deltaA,
    ! spaced at intervals of dA
    nPoints = INT(( deltaA / dA ) + ONEPOINT5)
  END FUNCTION ComputeNPoints


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ComputeIndex
!
! PURPOSE:
!       Pure function to compute the index of a particular point value within
!       an equally spaced array given the begin index value and index interval.
!
! CALLING SEQUENCE:
!       idx = ComputeIndex(a, da, a1=a1)
!
! INPUTS:
!       a:         Value for which the index is to be determined.
!                  UNITS:      Variable
!                  TYPE:       REAL(fp)
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
!       da:        Point spacing of the data.
!                  UNITS:      Same as input.
!                  TYPE:       REAL(fp)
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       a1:        Value for begin index value.
!                  If not specified, 0.0 is assumed.
!                  UNITS:      Variable
!                  TYPE:       REAL(fp)
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       idx:       The index position of the point "a" in an array of data
!                  specified by,
!                    a = a1 + (i-1)*da
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION ComputeIndex(a, da, a1) RESULT(idx)
    ! Arguments
    REAL(fp),           INTENT(IN) :: a
    REAL(fp),           INTENT(IN) :: da
    REAL(fp), OPTIONAL, INTENT(IN) :: a1
    ! Function result
    INTEGER :: idx
    ! Local variables
    REAL(fp) :: delta
    ! Set default width
    delta = a
    IF ( PRESENT(a1) ) delta = a - a1
    ! Compute the index
    idx = ComputeNPoints(delta, da)
  END FUNCTION ComputeIndex


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ComputeNextPO2
!
! PURPOSE:
!       Pure function to compute the next power-of-two, po2, that would yield
!       a number of points greater than or equal to the supplied number of
!       points, n.
!
! CALLING SEQUENCE:
!       po2 = ComputeNextPO2(n)
!
! INPUTS:
!       n:         Number of points for which the next power-of-two is required.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       po2:       Power-of-two value such that 2^po2 >= n.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION ComputeNextPO2(n) RESULT(po2)
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


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Sinc
!
! PURPOSE:
!       Pure function to compute a Sinc function.
!
! CALLING SEQUENCE:
!       y = Sinc(x, Normalize=Normalize)
!
! INPUTS:
!       x:           Abscissa values for which the Sinc function is required.
!                    UNITS:      Variable
!                    TYPE:       REAL(fp)
!                    DIMENSION:  Rank-1
!                    ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Normalize:   Set this logical argument to normalize (i.e. multiply by PI)
!                    the abscissa values prior to computing the Sinc.
!                    If == .FALSE. No nomalization (DEFAULT)
!                       == .TRUE.  Normalization is performed.
!                    UNITS:      N/A
!                    TYPE:       LOGICAL
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       y:           Sinc function values for the input x.
!                    UNITS:      N/A
!                    TYPE:       REAL(fp)
!                    DIMENSION:  Same rank and size as input x.
!
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION Sinc(x, Normalize) RESULT(y)
    ! Arguments
    REAL(fp),          INTENT(IN) :: x(:)
    LOGICAL, OPTIONAL, INTENT(IN) :: Normalize
    ! Function result
    REAL(fp), DIMENSION(SIZE(x)) :: y
    ! Local variables
    REAL(fp), DIMENSION(SIZE(x)) :: xScale

    ! Check normalisation
    xScale = x
    IF ( PRESENT(Normalize) ) THEN
      IF ( Normalize ) xScale = PI*x
    END IF

    ! Compute Sinc function
    WHERE( xScale /= ZERO )
      y = SIN(xScale)/xScale
    ELSEWHERE
      y = ONE
    END WHERE
  END FUNCTION Sinc


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CosFilter
!
! PURPOSE:
!       Function to a cosine rolloff filter for application to a spectrum
!       prior to Fourier transforming it to an interferogram
!
! CALLING SEQUENCE:
!       Error_Status = CosFilter( Frequency              , & ! Input
!                                 Filter                 , & ! Output
!                                 FilterWidth=FilterWidth, & ! Optional Input
!                                 Reverse    =Reverse    , & ! Optional Input
!                                 nFilter    =nFilter      ) ! Optional output
!
! INPUTS:
!       Frequency:    The frequencies for which filter values are required.
!                     UNITS:      Inverse centimetres (cm^-1)
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Rank-1
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Filter:       The cosine filter values for the specified frequencies.
!                     UNITS:      N/A
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Same as input Frequency.
!                     ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       FilterWidth:  Set this argument to the width to be used in
!                     computing the filter values. If not specified,
!                     the default is 10cm^-1
!                     UNITS:      Inverse centimetres (cm^-1)
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Reverse:      Set this argument to reverse the filter values with
!                     respect to frequency.
!                     If == .FALSE., computed filter is HIGH-PASS (DEFAULT)
!                        == .TRUE. , computed filter is LOW-PASS
!                     UNITS:      N/A
!                     TYPE:       LOGICAL
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUTS:
!       nFilter:      Specify this argument to return the actual number of
!                     filter points computed. The filter width determines
!                     how the filter is calcuated, and that filter width
!                     may not correspond to an integral number of the input
!                     frequency points,
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
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

  FUNCTION CosFilter( &
    Frequency  , & ! Input
    Filter     , & ! Output
    FilterWidth, & ! Optional Input
    Reverse    , & ! Optional Input
    nFilter    ) & ! Optional Output
  RESULT(Error_Status)
    ! Arguments
    REAL(fp),           INTENT(IN)  :: Frequency(:)
    REAL(fp),           INTENT(OUT) :: Filter(:)
    REAL(fp), OPTIONAL, INTENT(IN)  :: FilterWidth
    LOGICAL,  OPTIONAL, INTENT(IN)  :: Reverse
    INTEGER,  OPTIONAL, INTENT(OUT) :: nFilter
    ! Function result
    INTEGER :: Error_Status
    ! Locall parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CosFilter'
    ! Local variables
    CHARACTER(ML) :: Message
    REAL(fp) :: Width
    INTEGER  :: n, nPts
    INTEGER  :: i1, i2, i3
    REAL(fp) :: dF

    ! Set up
    Error_Status = SUCCESS
    ! ...Check input
    n = SIZE(Frequency)
    IF ( SIZE(Filter) /= n ) THEN
      Message = 'Size of Frequency and Filter arguments inconsistent.'
      CALL Cleanup(); RETURN
    END IF
    ! ...Filter width
    Width = DEFAULT_WIDTH
    IF ( PRESENT(FilterWidth) ) Width=FilterWidth
    

    ! Compute the number of points
    ! ...Mean frequency interval
    dF = ComputeMeanDelta(Frequency)
    ! ...How many points required for filter?
    nPts = ComputeNPoints(Width, dF)
    ! ...Not enough?
    IF ( nPts <= 1 ) THEN
      Message = 'Number of filter points too small'
      CALL Cleanup(); RETURN
    END IF
    ! ...Too many?
    IF ( nPts > n ) THEN
      Message = 'Number of filter points too large'
      CALL Cleanup(); RETURN
    END IF

    ! Return number of points if necessary
    IF ( PRESENT(nFilter) ) nFilter = nPts

    ! Define array triplet
    i1 = nPts
    i2 = 1
    i3 = -1
    IF (PRESENT(Reverse)) THEN
      IF (Reverse) THEN
        i1 = n-nPts+1
        i2 = n
        i3 = 1
      END IF
    END IF

    ! Initialise filter
    Filter = ONE

    ! Compute filter
    Filter(i1:i2:i3) = POINT5 * (ONE + COS((Frequency(i1:i2:i3)-Frequency(i1))*PI/Width))

  CONTAINS

    SUBROUTINE Cleanup()
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
    END SUBROUTINE Cleanup

  END FUNCTION CosFilter


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SPC_IFG_Utility_Version
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL SPC_IFG_Utility_Version( Id )
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

  SUBROUTINE SPC_IFG_Utility_Version( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE SPC_IFG_Utility_Version

END MODULE SPC_IFG_Utility
