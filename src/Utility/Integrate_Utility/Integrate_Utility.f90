!
! Integrate_Utility
!
! Module containing integration routines.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 27-Aug-2001
!                       paul.vandelst@noaa.gov
!

MODULE Integrate_Utility

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds,            ONLY: fp
  USE Message_Handler,       ONLY: SUCCESS, FAILURE, Display_Message
  USE Fundamental_Constants, ONLY: PI
  USE Compare_Float_Numbers, ONLY: Compare_Float
  USE Interpolate_Utility,   ONLY: Linear_Interpolate, &
                                   Spline_Initialize, Spline_Interpolate
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Integral
  PUBLIC :: Gauss_Legendre
  PUBLIC :: Integrate_Utility_Version


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER ::  ONE = 1.0_fp
  REAL(fp), PARAMETER ::  TWO = 2.0_fp
  REAL(fp), PARAMETER :: POINT_FIVE = 0.5_fp
  ! MEssage string length
  INTEGER, PARAMETER :: ML = 256


CONTAINS


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Integral
! 
! PURPOSE:
!       Function to integrate tabulated data.
!
! CALLING SEQUENCE:
!       Error_Status = Integral( x, y,         &
!                                Integral_Sum, &
!                                n_Points     = n_Points    , &
!                                Force_Spline = Force_Spline  )
!
!
! INPUTS:
!       x, y:          The tabulated function to be integrated. The sizes
!                      of x and y must be the same and be more than one
!                      element.
!                      UNITS:      Argument dependent.
!                      TYPE:       REAL(fp)
!                      DIMENSION:  Rank-1
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Integral_Sum:  The integration result.
!                      UNITS:      Argument dependent.
!                      TYPE:       REAL(fp)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       n_Points:      Set this argument to an integer value that selects the
!                      closed Newton-Cotes quadrature formula to be used.
!                      Valid values are:
!                        2: Trapezoidal rule
!                        3: Simpson's rule [DEFAULT]
!                        4: Simpson's 3/8 rule
!                        5: Boole's rule
!                        6: 6-point rule
!                        7: 7-point rule
!                      If not specified, a value of 3 is used.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
!
!       Force_Spline:  Set this argument to force spline interpolation to be used.
!                      Spline interpolation is the default UNLESS the number of
!                      input x,y points is < 50; then linear interpolation is used.
!                      If == .FALSE., Interpolation method is determined by the 
!                                     number of input points. [DEFAULT]
!                         == .TRUE.,  Spline interpolation is always used.
!                      If not specified, default is .FALSE.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the Message_Handler module.
!                      If == SUCCESS the computation was successful.
!                         == FAILURE an unrecoverable error occurred.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Integral( &
    x, y,         &  ! Input   
    Integral_Sum, &  ! Output
    n_Points    , &  ! Optional input
    Force_Spline) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    REAL(fp),           INTENT(IN)  :: x(:)
    REAL(fp),           INTENT(IN)  :: y(:)
    REAL(fp),           INTENT(OUT) :: Integral_Sum
    INTEGER , OPTIONAL, INTENT(IN)  :: n_Points
    LOGICAL , OPTIONAL, INTENT(IN)  :: Force_Spline
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME         = 'Integral'
    REAL(fp),     PARAMETER :: NON_UNIQUE_TOLERANCE = EPSILON(ONE)
    INTEGER, PARAMETER :: DEFAULT_N_POINTS = 3
    INTEGER, PARAMETER :: MIN_N_POINTS = 2
    INTEGER, PARAMETER :: MAX_N_POINTS = 7
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER  :: alloc_stat
    INTEGER  :: i, j, m, n
    INTEGER  :: idx(MAX_N_POINTS)
    INTEGER  :: n_Even, n_Jdx
    LOGICAL  :: Spline_Interpolation
    REAL(fp) :: dx(SIZE(x)-1)
    REAL(fp) :: c0, c(MAX_N_POINTS), h
    REAL(fp) :: error, tmp, se
    REAL(fp), ALLOCATABLE :: xi(:), yi(:)
    INTEGER , ALLOCATABLE :: jdx(:)
    REAL(fp), ALLOCATABLE :: s(:)


    ! Set up
    err_stat = SUCCESS
    idx = (/ (i,i=1,MAX_N_POINTS) /)
    ! ...Check input vector sizes for agreement
    n = SIZE(x)
    IF ( SIZE(y) /= n ) THEN
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'X and Y input vectors must be same size.', &
                            err_stat )
      RETURN
    END IF
    ! ...Check that all points in X are unique to avoid division by zero
    dx = x(2:n) - x(1:n-1)
    IF ( ANY(ABS(dx) < NON_UNIQUE_TOLERANCE) ) THEN
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Non-unique X values found.', &
                            err_stat )
      RETURN
    END IF
    ! ...Check that X is monotonically increasing
    IF ( ANY(dx < ZERO) ) THEN
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'X values must be monotonically increasing.', &
                            err_stat )
      RETURN
    END IF
    ! ...Set interpolation type
    m = DEFAULT_N_POINTS
    IF ( PRESENT(n_Points) ) m = n_Points
    ! ...Set interpolation type based on the number of unique input points
    IF ( n > 50 ) THEN
      Spline_Interpolation = .TRUE.
    ELSE
      Spline_Interpolation = .FALSE.
    END IF
    IF ( PRESENT(Force_Spline) ) Spline_Interpolation = Force_Spline


    ! Get the interpolation coefficients
    CALL Newton_Cotes_Coefficients()
    
    
    ! Perform the interpolation
    ! ...Determine the number of points to interpolate
    n_Even = n
    DO
      IF ( MOD(n_Even-1,m-1) == 0 ) EXIT
      n_Even = n_Even + 1
    END DO
    ! ...Allocate interpolation arrays
    ALLOCATE( xi(n_Even), yi(n_Even), STAT=alloc_stat )
    IF ( alloc_stat /= 0 ) THEN
      err_stat = FAILURE
      WRITE( msg,'("Error allocating interpolation arrays. STAT = ",i0)' ) alloc_stat
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF
    ! ...Construct an evenly spaced abscissa grid
    xi = (/ (REAL(i,fp), i=0,n_Even-1) /) / REAL(n_Even-1,fp)
    xi = xi*(x(n)-x(1)) + x(1)
    ! ...Interpolate data to regular grid
    IF ( Spline_Interpolation ) THEN
      err_stat = Spline_Interpolate( x, y, xi, yi )
    ELSE
      err_stat = Linear_Interpolate( x, y, xi, yi )
    END IF
    IF ( err_stat /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, 'Error interpoalting input data', err_stat )
      RETURN
    END IF
      
    
    ! Perform the integration
    ! ...Determine the number of indexing points
    n_Jdx = (n_Even-1)/(m-1)
    ! ...Allocate the indexing and summation array
    ALLOCATE( jdx(n_Jdx), s(n_Jdx), STAT=alloc_stat )
    IF ( alloc_stat /= 0 ) THEN
      err_stat = FAILURE
      WRITE( msg,'("Error allocating integration index and sum arrays. STAT = ",i0)' ) alloc_stat
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF
    ! ...Set the uniform interval
    h = (xi(n_Even) - xi(1))/REAL(n_Even-1,fp)
    ! ...Construct the index array to use for integration
    jdx = (/ (j, j=1,n_Jdx) /) * (m-1)
    ! ...Accumulate the integral value
    DO j = 1, n_Jdx
      s(j) = c0 * h * SUM(c(1:m) * yi(idx(1:m) + jdx(j) - (m-1)))
    END DO
    ! ...Perform compensated summation
    integral_sum = ZERO
    error = ZERO
    DO j = 1, n_Jdx
      tmp          = integral_sum               ! Save current sum
      se           = s(j) + error               ! Add rounding error to current value
      integral_sum = tmp + se                   ! Compute new sum
      error        = (tmp - integral_sum) + se  ! Compute new rounding error
    END DO


    ! Clean up
    DEALLOCATE( xi, yi, jdx, s, STAT=alloc_stat )
    
  CONTAINS
  
    SUBROUTINE Newton_Cotes_Coefficients()
      SELECT CASE(m)
        ! Trapezoidal rule
        CASE(2)
          c0 = 1.0_fp/2.0_fp
          c(1:2) = (/1.0_fp, 1.0_fp/)
        ! Simpson's 3/8 rule
        CASE(4)
          c0 = 3.0_fp/8.0_fp
          c(1:4) = (/1.0_fp, 3.0_fp, 3.0_fp, 1.0_fp/)
        ! Boole's rule
        CASE(5)
          c0 = 2.0_fp/45.0_fp
          c(1:5) = (/7.0_fp, 32.0_fp, 12.0_fp, 32.0_fp, 7.0_fp/)
        ! And higher orders...
        CASE(6)
          c0 = 5.0_fp/288.0_fp
          c(1:6) = (/19.0_fp, 75.0_fp, 50.0_fp, 50.0_fp, 75.0_fp, 19.0_fp/)
        CASE(7)
          c0 = 1.0_fp/140.0_fp
          c(1:7) = (/41.0_fp, 216.0_fp, 27.0_fp, 272.0_fp, 27.0_fp, 216.0_fp, 41.0_fp/)
        ! Default is regular Simpson's rule
        CASE DEFAULT
          c0 = 1.0_fp/3.0_fp
          c(1:3) = (/1.0_fp, 4.0_fp, 1.0_fp/)
          m = DEFAULT_N_POINTS
      END SELECT
    END SUBROUTINE Newton_Cotes_Coefficients

  END FUNCTION Integral


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Gauss_Legendre
! 
! PURPOSE:
!       Function to compute the abscissae and weights for the Gauss-Legendre
!       N-point quadrature formula.
!
!       Adapted from the "gauleg" subroutine in W.H.Press et al. "Numerical
!       Recipes in Fortran", sec.4.5, pg145, 2nd ed, 1992
!
! CALLING SEQUENCE:
!       Error_Status = Gauss_Legendre( x, weight,              &
!                                      x_Interval = x_Interval )
!
! OUTPUTS:
!       x:               The array of abscissa values to be used in the N-point
!                        quadrature. The size of this array determines the value
!                        of N used in computing the abscissae and must be the
!                        same size as the WEIGHT argument.
!                        UNITS:      Argument dependent.
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1
!                        ATTRIBUTES: INTENT(OUT)
!
!       weight:          The array of weights to be used in the N-point
!                        quadrature. The size of this array determines
!                        the value of N used in computing the abscissae
!                        and must be the same size as the X argument.
!                        UNITS:      Argument dependent.
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1
!                        ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       x_Interval:      A two-element vector containing the interval over which
!                        the abscissa and weights are to be evaluated.
!                        x_Interval(1) must be > x_Interval(2).
!                        If not supplied the abscissa and weights are
!                        returned that correspond to an interval of [-1,1]
!                        UNITS:      Argument dependent.
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1
!                        ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the computation was successful
!                           == FAILURE an unrecoverable error occurred.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Gauss_Legendre( &
    x         , &  ! Output
    Weight    , &  ! Output
    x_Interval) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    REAL(fp),          INTENT(OUT) :: x(:)
    REAL(fp),          INTENT(OUT) :: Weight(:)
    REAL(fp),OPTIONAL, INTENT(IN)  :: x_Interval(2)
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Gauss_Legendre'
    REAL(fp),     PARAMETER :: TOLERANCE      = EPSILON( ONE )
    INTEGER,      PARAMETER :: MAX_ITERATIONS = 1000
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER  :: i,  j,  m, n
    REAL(fp) :: ir, jr,    nr
    REAL(fp) :: x_Lower,   x_Upper
    REAL(fp) :: x_Average, x_Halfwidth
    REAL(fp) :: z, z1
    REAL(fp) :: Pj, Pj_1, Pj_2
    REAL(fp) :: Pj_Prime
    INTEGER :: n_Iterations


    ! Set up
    err_stat = SUCCESS
    ! ...Check the integration limits
    IF ( PRESENT(x_Interval) ) THEN
      IF ( x_Interval(2) < x_Interval(1) .OR. &
           Compare_Float( x_Interval(1), x_Interval(2) ) ) THEN
        err_stat = FAILURE
        msg = 'For input integration interval [a,b], b < or = a.'
        CALL Display_Message( ROUTINE_NAME, msg, err_stat )
        RETURN
      END IF
      x_Lower = x_Interval(1)
      x_Upper = x_Interval(2)
    ELSE
      x_Lower = -ONE
      x_Upper =  ONE
    END IF
    ! ...Check the size of the output arrays
    n = SIZE(x)
    IF ( SIZE(Weight) /= n ) THEN
      err_stat = FAILURE
      msg = 'Size of X and WEIGHT output arrays are different.'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF


    ! -----------------------------------------------------
    ! Find the roots and weights of the required polynomial
    ! -----------------------------------------------------
    ! The size of the output X and WEIGHT arrays
    ! determine the polynomial order. Need a floating
    ! point representation of that value
    nr = REAL(n,fp)

    ! The roots are symmetric in the interval
    ! so only half need to be determined
    m = (n + 1) / 2

    ! The scaling values to convert the computed abscissae from
    ! the interval (-1,1) to the input interval (x_Lower, x_Upper)
    x_Average   = POINT_FIVE * ( x_Upper + x_Lower )  ! Average X of interval
    x_Halfwidth = POINT_FIVE * ( x_Upper - x_Lower )  ! Halfwidth of interval

    ! Loop over the roots to find
    Root_Loop: DO i = 1, m

      ! Approximate the i'th root with the analytic
      ! value for Chebyshev polynomials,
      !
      !            (       i - 0.5  )
      !   z  = COS ( PI * --------- ) 
      !    i       (          n     )
      !
      ir = REAL(i,fp)
      z = COS(PI * (ir - POINT_FIVE) / nr)

      ! Initialise the iteration counter used
      ! to refine the polynomial root value
      n_Iterations = 0

      ! Refine the i'th root by Newton's method
      Root_Find: DO

        ! Increment and test the iteration counter
        n_Iterations = n_Iterations + 1
        IF ( n_Iterations > MAX_ITERATIONS ) THEN
          err_stat = FAILURE
          WRITE( msg,'("Maximum number of iterations exceeded for finding root #",i0)' ) i
          CALL Display_Message( ROUTINE_NAME, msg, err_stat )
          RETURN
        END IF

        ! Initialise the lower order polynomials
        Pj   = ONE    ! j'th   polynomial
        Pj_1 = ZERO   ! j-1'th polynomial

        ! Loop up the recurrence relation to get
        ! the Legendre polynomial evaluated at x
        Recurrence_Loop: DO j = 1, n

          jr = REAL(j,fp)

          ! Demote the previous loop's polynomials
          Pj_2 = Pj_1   ! j-2'th polynomial
          Pj_1 = Pj     ! j-1'th polynomial

          ! Compute the new polynomial using the recurrence relation:
          !
          !  j.P  = (2j-1).x.P     - (j-1).P         .....(1)
          !     j             j-1           j-2
          !
          ! Note that in the NR text the relation (4.5.10) is given as
          !
          !  (j+1).P    = (2j+1).x.P  - (j+1).P      .....(2)
          !         j+1             j          j-1
          !
          ! which is equivalent to the form used in (1) except that
          ! the j+1'th polynomial in (2) is the j'th polynomial in (1)
          Pj = ((((TWO * jr) - ONE) * z * Pj_1) - ((jr - ONE) * Pj_2)) / jr

        END DO Recurrence_Loop

        ! Pj is now the desired Legendre polynomial. Now compute its
        ! derivate, Pj_Prime, using a standard relation involving also
        ! Pj_1, the polynomial of one lower order
        Pj_Prime = nr * ((z * Pj) - Pj_1) / ((z*z) - ONE)

        ! Save the previous root value
        z1 = z

        ! Update the root approximation
        z  = z1 - (Pj / Pj_Prime)

        ! Exit the root finding loop if the desired
        ! accuracy has been reached
        IF ( ABS(z - z1) < TOLERANCE ) EXIT Root_Find

      END DO Root_Find


      ! --------------------------------------------------------
      ! Scale the root value, z, and it's symmetric counterpart
      ! from the (-1,1) interval to (x_Lower,x_Upper)
      !
      ! The input interval and derived quantities:
      !
      !     |                 |                 |
      !  x_Lower          x_Average          x_Upper
      !                       |                 |
      !                       |<- x_Halfwidth ->|
      !
      !
      ! The computed abscissa for half of the interval:
      !
      !                       |  +  +  +  +  +  |
      !                       0                 1
      !
      ! are reflected symmetrically
      !
      !     |  +  +  +  +  +  |
      !    -1                 0
      !
      ! --------------------------------------------------------
      x(i)     = x_Average - (x_Halfwidth * z)
      x(n+1-i) = x_Average + (x_Halfwidth * z)


      ! --------------------------------------------------------
      ! Compute the weight and its symmetric counterpart using 
      ! the special form for the Gauss-Legendre case:
      !
      !                     2
      !   w   = ------------------------   .....(4.5.16 from NR)
      !    i     (1 - x ^2) . P'(x )^2
      !                i       N  i 
      ! --------------------------------------------------------
      Weight(i)     = TWO * x_Halfwidth / ((ONE - (z**2)) * Pj_Prime**2)
      Weight(n+1-i) = Weight(i)

    END DO Root_Loop

  END FUNCTION Gauss_Legendre


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Integrate_Utility_Version
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL Integrate_Utility_Version( Id )
!
! OUTPUTS:
!       Id:    Character string containing the version Id information
!              for the module.
!              UNITS:      N/A
!              TYPE:       CHARACTER(*)
!              DIMENSION:  Scalar
!              ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Integrate_Utility_Version( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE Integrate_Utility_Version

END MODULE Integrate_Utility
