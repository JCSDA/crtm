!
! Integrate_Utility
!
! Module containing integration routines.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Aug-2001
!                       paul.vandelst@ssec.wisc.edu
!

MODULE Integrate_Utility

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds,            ONLY: fp=>fp_kind
  USE Message_Handler,       ONLY: SUCCESS, FAILURE, Display_Message
  USE Compare_Float_Numbers, ONLY: Compare_Float
  USE Interpolate_Utility,   ONLY: Polynomial_Interpolate
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Simpsons_Integral
  PUBLIC :: Gauss_Legendre


  ! -----------------
  ! Module parameters
  ! -----------------

  ! RCS Id field
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
    '$Id: Integrate_Utility.f90,v 1.1 2006/06/08 19:46:40 wd20pd Exp $'

  ! Keyword set value
  INTEGER,  PARAMETER :: SET = 1

  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER ::  ONE = 1.0_fp
  REAL(fp), PARAMETER ::  TWO = 2.0_fp
  REAL(fp), PARAMETER :: FOUR = 4.0_fp
  REAL(fp), PARAMETER ::  SIX = 6.0_fp
  REAL(fp), PARAMETER :: ONEonSIX = ONE / SIX
  REAL(fp), PARAMETER :: ZEROpointTWOFIVE = 0.25_fp
  REAL(fp), PARAMETER :: ZEROpointFIVE = 0.5_fp
  REAL(fp), PARAMETER :: PI = 3.141592653589793238462643383279_fp


CONTAINS


!------------------------------------------------------------------------------
!
! NAME:
!       Simpsons_Integral
! 
! PURPOSE:
!       Function to integrate tabulated values using Simpson's Rule.
!
! CALLING SEQUENCE:
!       Error_Stats = Simpsons_Integral( x, y,                   &  ! Input   
!                                        Integral_Sum,           &  ! Output
!                                        Order      =Order,      &  ! Optional input
!                                        RCS_Id     =RCS_Id,     &  ! Revision control
!                                        Message_Log=Message_Log )  ! Error messaging
!
!
! INPUT ARGUMENTS:
!       x, y:         The tabulated function to be integrated. The sizes
!                     of x and y must be the same and be more than one
!                     element.
!                     UNITS:      Argument dependent.
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Rank-1
!                     ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Order:        The order of the interpolating polynomial to be used in
!                     computing the mid-point y-value for each interval of
!                     the input tabulated data.
!                     This argument is passed to the interpolation procedure.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Integral_Sum: The integration result.
!                     UNITS:      Argument dependent.
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the function executed normally.
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! PROCEDURE:
!       Simpsons rule is can be stated as:
!
!                 h
!         Area = --- ( y1 + 4y2 + y3 )
!                 3
!
!       where y1,y2,y3 are the input tabulated Y values and h is the subinterval
!       width between the input Y's. In this form the subintervals must have
!       equal widths and the number of subintervals must be even. To avoid 
!       having the user pass a equal-spaced tabulated function of an odd number
!       of points, the following is used:
!
!                 ( x2 - x1 )          _
!         Area = ------------- ( y1 + 4y + y2 )
!                      6
!             _                                       _    x1 + x2
!       where y is the interpolated ordinate value of x = --------- 
!                                                             2
!
!       and x1 and x2 are successive abscissa points in the input array. This
!       is applied to each subinterval of the input tabulated data.
!
!       Generally, a higher interpolation order provides better accuracy when 
!       performing the integration.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Aug-2001
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Simpsons_Integral( x, y,         &  ! Input   
                              Integral_Sum, &  ! Output
                              Order,        &  ! Optional input
                              RCS_Id,       &  ! Revision control
                              Message_Log ) &  ! Error messaging
                            RESULT( Error_Status )
    ! Arguments
    REAL(fp), DIMENSION(:), INTENT(IN)  :: x
    REAL(fp), DIMENSION(:), INTENT(IN)  :: y
    REAL(fp),               INTENT(OUT) :: Integral_Sum
    INTEGER,      OPTIONAL, INTENT(IN)  :: Order
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME         = 'Simpsons_Integral'
    REAL(fp),     PARAMETER :: NON_UNIQUE_TOLERANCE = EPSILON( ONE )
    ! Local variables
    INTEGER :: n_Points
    REAL(fp), DIMENSION(SIZE(x)-1) :: dx
    REAL(fp), DIMENSION(SIZE(x)-1) :: xMid
    REAL(fp), DIMENSION(SIZE(x)-1) :: yMid


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check input vector sizes for agreement
    n_Points = SIZE(x)
    IF ( SIZE(y) /= n_Points ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'X and Y input vectors must be same size.', &
                            Error_Status, & 
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check that all points in X are unique to avoid division by zero
    dx = x(2:n_Points) - x(1:n_Points-1)
    IF ( ANY( ABS(dx) < NON_UNIQUE_TOLERANCE ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Non-unique X values found.', &
                            Error_Status, & 
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! -------------------------
    ! Perform the interpolation
    ! -------------------------
    xMid = (ZEROpointFIVE*dx) + x(1:n_Points-1)
    Error_Status = Polynomial_Interpolate( x, y,        &  ! Input
                                           xMid,        &  ! Input
                                           yMid,        &  ! Output
                                           Order=Order, &  ! Order of interpolating polynomial
                                           Message_Log=Message_Log )  ! Error messaging
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error occured in interpolation function.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! ----------------------
    ! And do the integration
    ! ----------------------
    Integral_Sum = SUM( ONEonSIX * dx * &
                        ( y(1:n_Points-1) + (FOUR*yMid) + y(2:n_Points) ) )

  END FUNCTION Simpsons_Integral


!------------------------------------------------------------------------------
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
!       Error_Status = Gauss_Legendre( x,                      &  ! Output
!                                      Weight,                 &  ! Output
!                                      xInterval  =xInterval,  &  ! Optional Input
!                                      RCS_Id     =RCS_Id,     &  ! Revision control
!                                      Message_Log=Message_Log )  ! Error messaging
!
! OPTIONAL INPUT ARGUMENTS:
!       xInterval:       A two-element vector containing the interval over which
!                        the abscissa and weights are to be evaluated.
!                        xInterval(1) must be > xInterval(2).
!                        If not supplied the default abscissa and weights are
!                        returned -- which corresponds to an interval of [-1,1]
!                        UNITS:      Argument dependent.
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1
!                        ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       x:               The array of abscissa values to be used in the N-point
!                        quadrature. The size of this array determines the value
!                        of N used in computing the abscissae and must be the
!                        same size as the WEIGHT argument.
!                        UNITS:      Argument dependent.
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1
!                        ATTRIBUTES: INTENT(OUT)
!
!       Weight:          The array of weights to be used in the N-point
!                        quadrature. The size of this array determines
!                        the value of N used in computing the abscissae
!                        and must be the same size as the X argument.
!                        UNITS:      Argument dependent.
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1
!                        ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:          Character string containing the Revision Control
!                        System Id field for the module.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the abscissa and weight computation was successful
!                           == FAILURE - there was a problem with the arguments, or
!                                      - the root of the polynomial couldn't be found.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! PROCEDURE:
!       This function computes the abscissae and weights for the approximation
!
!            /\ b                __ N
!           |                   \
!           |  W(x).p(x).dx  ~   >  w .p(x )
!           |                   /__  j    j
!          \/ a                    j=1
!
!       where the function, p, is a polynomial and
!
!         W(x) = 1,   -1 < x < 1
!
!       The recurrence relation used to determine the polynomial is
!
!         j.P  = (2j-1).x.P     - (j-1).P
!            j             j-1           j-2
!
!       The results are scaled to the input interval if it is supplied.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Aug-2001
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Gauss_Legendre( x,            &  ! Output
                           Weight,       &  ! Output
                           xInterval,    &  ! Optional input
                           RCS_Id,       &  ! Revision control
                           Message_Log ) &  ! Error messaging
                         RESULT( Error_Status )
    ! Arguments
    REAL(fp),               DIMENSION(:), INTENT(OUT) :: x
    REAL(fp),               DIMENSION(:), INTENT(OUT) :: Weight
    REAL(fp),     OPTIONAL, DIMENSION(2), INTENT(IN)  :: xInterval
    CHARACTER(*), OPTIONAL,               INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL,               INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Gauss_Legendre'
    REAL(fp),     PARAMETER :: TOLERANCE      = EPSILON( ONE )
    INTEGER,      PARAMETER :: MAX_ITERATIONS = 1000
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER  :: i,  j,  m, n
    REAL(fp) :: ir, jr,    nr
    REAL(fp) :: xLower,   xUpper
    REAL(fp) :: xAverage, xHalfwidth
    REAL(fp) :: z, z1
    REAL(fp) :: Pj, Pj_1, Pj_2
    REAL(fp) :: Pj_Prime
    INTEGER :: n_Iterations


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! The integration limits
    IF ( PRESENT( xInterval ) ) THEN
      ! Check them
      IF ( xInterval(2) < xInterval(1) .OR. &
           Compare_Float( xInterval(1), xInterval(2) ) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'For input integration interval [a,b], b < or = a.', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
      ! Assign them
      xLower = xInterval(1)
      xUpper = xInterval(2)
    ELSE
      ! Default interval
      xLower = -ONE
      xUpper =  ONE
    END IF

    ! The size of the output arrays
    n = SIZE( x )
    IF ( SIZE( Weight ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Size of X and WEIGHT output arrays are different.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! -----------------------------------------------------
    ! Find the roots and weights of the required polynomial
    ! -----------------------------------------------------
    ! The size of the output X and WEIGHT arrays
    ! determine the polynomial order. Need a floating
    ! point representation of that value
    nr = REAL( n, fp )

    ! The roots are symmetric in the interval
    ! so only half need to be determined
    m = ( n + 1 ) / 2

    ! The scaling values to convert the computed abscissae from
    ! the interval (-1,1) to the input interval (xLower, xUpper)
    xAverage   = ZEROpointFIVE * ( xUpper + xLower )  ! Average X of interval
    xHalfwidth = ZEROpointFIVE * ( xUpper - xLower )  ! Halfwidth of interval

    ! Loop over the roots to find
    Root_Loop: DO i = 1, m

      ! Approximate the i'th root with the analytic
      ! value for Chebyshev polynomials,
      !
      !            (       i - 0.5  )
      !   z  = COS ( PI * --------- ) 
      !    i       (          n     )
      !
      ir = REAL( i, fp )
!      z = COS( PI * ( ir - ZEROpointTWOFIVE ) / ( nr + ZEROpointFIVE ) )
      z = COS( PI * ( ir - ZEROpointFIVE ) / nr )

      ! Initialise the iteration counter used
      ! to refine the polynomial root value
      n_Iterations = 0

      ! Refine the i'th root by Newton's method
      Root_Find: DO

        ! Increment and test the iteration counter
        n_Iterations = n_Iterations + 1
        IF ( n_Iterations > MAX_ITERATIONS ) THEN
          Error_Status = FAILURE
          WRITE( Message, '( "Maximum number of iterations exceeded for finding root #", i5 )' ) i
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log=Message_Log )
          RETURN
        END IF

        ! Initialise the lower order polynomials
        Pj   = ONE    ! j'th   polynomial
        Pj_1 = ZERO   ! j-1'th polynomial

        ! Loop up the recurrence relation to get
        ! the Legendre polynomial evaluated at x
        Recurrence_Loop: DO j = 1, n

          jr = REAL( j, fp )

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
          Pj = ( ( ( (TWO*jr)-ONE)*z*Pj_1 ) - ( (jr-ONE)*Pj_2 ) ) / jr

        END DO Recurrence_Loop

        ! Pj is now the desired Legendre polynomial. Now compute its
        ! derivate, Pj_Prime, using a standard relation involving also
        ! Pj_1, the polynomial of one lower order
        Pj_Prime = nr * ( ( z * Pj ) - Pj_1 ) / &
        !               ---------------------
                          ( ( z*z ) -  ONE )

        ! Save the previous root value
        z1 = z

        ! Update the root approximation
        z  = z1 - ( Pj / Pj_Prime )

        ! Exit the root finding loop if the desired
        ! accuracy has been reached
        IF ( ABS( z - z1 ) < TOLERANCE ) EXIT Root_Find

      END DO Root_Find


      ! --------------------------------------------------------
      ! Scale the root value, z, and it's symmetric counterpart
      ! from the (-1,1) interval to the (xLower,xUpper) interval
      !
      ! The input interval and derived quantities:
      !
      !     |                 |                 |
      !  xLower           xAverage           xUpper
      !                       |                 |
      !                       |<-- xHalfwidth ->|
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
      x(i)     = xAverage - ( xHalfwidth * z )
      x(n+1-i) = xAverage + ( xHalfwidth * z )


      ! --------------------------------------------------------
      ! Compute the weight and its symmetric counterpart using 
      ! the special form for the Gauss-Legendre case:
      !
      !                     2
      !   w   = ------------------------   .....(4.5.16 from NR)
      !    i     (1 - x ^2) . P'(x )^2
      !                i       N  i 
      ! --------------------------------------------------------
      Weight(i) =          TWO * xHalfwidth          / &
      !           ----------------------------------
                  ( ( ONE - (z**2) ) * Pj_Prime**2 )

      Weight(n+1-i) = Weight(i)

    END DO Root_Loop

  END FUNCTION Gauss_Legendre

END MODULE Integrate_Utility
