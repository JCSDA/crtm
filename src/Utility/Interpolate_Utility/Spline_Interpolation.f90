!
! Spline_Interpolation
!
! Module containing spline interpolation routines
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 06-Oct-2006
!                       paul.vandelst@ssec.wisc.edu
!

MODULE Spline_Interpolation

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Modules used
  USE Type_Kinds,      ONLY: fp=>fp_kind
  USE Message_Handler, ONLY: SUCCESS, FAILURE, Display_Message
  USE Search_Utility,  ONLY: Value_Locate, Bisection_Search
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Spline_Interpolate
  PUBLIC :: Spline_Initialize


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE Spline_Interpolate
    MODULE PROCEDURE splint_scalar
    MODULE PROCEDURE splint_rank1
  END INTERFACE Spline_Interpolate


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id field
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  REAL(fp), PARAMETER :: TWO  = 2.0_fp
  REAL(fp), PARAMETER :: SIX  = 6.0_fp
  REAL(fp), PARAMETER :: TOLERANCE = EPSILON( ZERO )


CONTAINS


!------------------------------------------------------------------------------
!
! NAME:
!       Spline_Initialize
!
! PURPOSE:
!       Function is called to establish the type of interpolating
!       spline for a tabulated set of functional values,
!           Xi, Yi = F(Xi)
!
! CALLING SEQUENCE:
!       Error_Status = Spline_Initialize( x,                      &  ! Input
!                                         y,                      &  ! Input
!                                         y2,                     &  ! Output
!                                         RCS_Id     =RCS_Id,     &  ! Optional output
!                                         Message_Log=Message_Log )  ! Error messaging
! INPUT ARGUMENTS:
!       x:            The abscissa values for the tabulated function, y=F(x).
!                     Must be monotonically ascending values.
!                     UNITS:      Data dependent.
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Rank-1 array
!                     ATTRIBUTES: INTENT(IN)
!
!       y:            An input vector of tabulated values, y=F(x), for which
!                     interpolates are required. Must have the same number of
!                     elements as the X input argument.
!                     UNITS:      Data dependent.
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Rank-1 array
!                     ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to the screen.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       y2:           Values of the second derivative of the interpolating
!                     function at the points defined by X. Must have the same
!                     number of elements as the X/Y input arguments.
!                     UNITS:      Data dependent.
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Rank-1 array
!                     ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control System
!                     Id field for the module.
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
! RESTRICTIONS:
!       Input X vectors *must* be monotonically ascending.
!
! PROCEDURE:
!       Adapted from SPL_INIT in "Numerical Recipes in FORTRAN; The Art of
!       Scientific Computing" 2nd ed. by W.H. Press et al., Cambridge University
!       Press, 1992, New York, New York, pp109.
!
!       SPLINE_INITIALIZE should be called only *once* to process an entire
!       tabulated function in the input arrays X and Y. Once this has been
!       done, values of the interpolated function for any value of X can be
!       obtained by calls to the separate routine SPLINE_INTERPOLATE.
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, CIMSS/SSEC, 22-Mar-1996
!                     paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Spline_Initialize( x,            &  ! Input
                              y,            &  ! Input
                              y2,           &  ! Output
                              RCS_Id,       &  ! Optional output
                              Message_Log ) &  ! Error messaging
                            RESULT ( Error_Status )
    ! Arguments
    REAL(fp), DIMENSION(:), INTENT(IN)  :: x
    REAL(fp), DIMENSION(:), INTENT(IN)  :: y
    REAL(fp), DIMENSION(:), INTENT(OUT) :: y2
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Spline_Initialize'
    ! Local variables
    INTEGER :: i
    INTEGER :: n
    REAL(fp) :: p
    REAL(fp) :: qn
    REAL(fp) :: sig
    REAL(fp) :: un
    REAL(fp) :: numer1
    REAL(fp) :: numer2
    REAL(fp) :: numerator
    REAL(fp) :: denominator
    REAL(fp) :: offset
    REAL(fp), DIMENSION(SIZE(x)) :: u


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS 
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_Id

    ! Are vector sizes consistent?
    n = SIZE(x)
    IF ( SIZE(y) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input X, Y arguments are different sizes.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    IF ( SIZE(y2) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Output Y2 argument has inconsistent size.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Is X-data in ascending order?
    u(1:n-1) = x(2:n) - x(1:n-1)
    IF ( ANY( u(1:n-1) < TOLERANCE ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input X data must be in ascending order', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! ---------------------------------------
    ! Set second derivative at lower boundary
    ! ---------------------------------------
    y2(1) = ZERO
    u(1)  = ZERO


    ! -------------------------------------------------
    ! Begin decomposition loop of tridiagonal algorithm
    ! -------------------------------------------------
    DO i = 2, n - 1
      sig     = ( x(i) - x(i-1) ) / ( x(i+1) - x(i-1) )
      p       = ( sig * y2( i-1 ) ) + TWO
      y2( i ) = ( sig - ONE ) / p

      numer1    = ( y(i+1) - y(i) ) / ( x(i+1) - x(i) )
      numer2    = ( y(i) - y(i-1) ) / ( x(i) - x(i-1) )
      numerator   = SIX * ( numer1 - numer2 )
      denominator = x(i+1) - x(i-1)
      offset      = sig * u(i-1)

      u( i ) = ( ( numerator / denominator ) - offset ) / p
    END DO


    ! ---------------------------------------
    ! Set second derivative at upper boundary
    ! ---------------------------------------
    qn = ZERO
    un = ZERO
    y2( n ) = ( un - ( qn * u(n-1) ) ) / ( ( qn * y2(n-1) ) + ONE )


    ! ----------------------------------------------
    ! Backsubstitution loop of tridiagonal algorithm
    ! ----------------------------------------------
    DO i = n - 1, 1, -1
      y2( i ) = ( y2(i) * y2(i+1) ) + u(i)
    END DO

  END FUNCTION Spline_Initialize


!--------------------------------------------------------------------------------
!
! NAME:
!       Spline_Interpolate
!
! PURPOSE:
!       Function to calculate the cubic-spline interpolated value of a tabulated
!       function, X, Y=F(X), at a given value of Xi.
!
! CALLING SEQUENCE:
!       Error_Status = Spline_Interpolate( x,                      &  ! Input
!                                          y,                      &  ! Input
!                                          x_int,                  &  ! Input
!                                          y_int,                  &  ! Output
!                                          y2         =y2,         &  ! Optional input
!                                          RCS_Id     =RCS_Id,     &  ! Optional output
!                                          Message_Log=Message_Log )  ! Error messaging
!
! INPUTS:
!       x:            The abscissa values for the tabulated function, y=F(x).
!                     Must be monotonically ascending values.
!                     UNITS:      Data dependent.
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Rank-1 array
!                     ATTRIBUTES: INTENT(IN)
!
!       y:            An input vector of tabulated values, y=F(x), for which
!                     interpolates are required. Must have the same number of
!                     elements as the X input argument.
!                     UNITS:      Data dependent.
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Rank-1 array
!                     ATTRIBUTES: INTENT(IN)
!
!       x_int:        Abscissa values for which interpolates are required.
!                     Must be monotonically ascending or descending in the
!                     same direction as the X input argument.
!                     UNITS:      Data dependent, same as x.
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Scalar or Rank-1 array
!                     ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       y2:           Values of the second derivative of the interpolating
!                     function at the points defined by X. Must have the same
!                     number of elements as the X/Y input arguments.
!                     If this argument is not supplied, it is obtained via a 
!                     call to SPLINE_INITIALIZE.
!                     UNITS:      Data dependent.
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Rank-1 array
!                     ATTRIBUTES: INTENT(IN)
!
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to the screen.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! OUTPUTS:
!       y_int:        Interpolate values corresponding to the X_INT abscissa
!                     values. Must have the same number of elements as the
!                     X_INT input argument.
!                     UNITS:      Data dependent, same as y.
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Scalar or Rank-1 array
!                     ATTRIBUTES: INTENT(OUT)
! 
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control System
!                     Id field for the module.
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
! RESTRICTIONS:
!       Input X vectors *must* be monotonically ascending.
!
! PROCEDURE:
!       Taken from "Numerical Recipes in FORTRAN; The Art of Scientific Computing"
!       2nd ed. by W.H. Press et al., Cambridge University Press, 1992, New York,
!       New York, pp109.
!
!       Given arrays of a tabulated function, X and Y = F(X), and optionally 
!       the array of the second derivatives of the interpolating function at
!       the points X, SPLINE_INTERPOLATE calculates a cubic-spline interpolated
!       value for the given value(s) of Xi.
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, CIMSS/SSEC, 22-Mar-1996
!                     paul.vandelst@ssec.wisc.edu
!
!--------------------------------------------------------------------------------

  FUNCTION splint_scalar( x,            &  ! Input
                          y,            &  ! Input
                          x_int,        &  ! Input
                          y_int,        &  ! Output
                          y2,           &  ! Optional input
                          RCS_Id,       &  ! Optional output
                          Message_Log ) &  ! Error messaging
                        RESULT ( Error_Status )
    ! Arguments
    REAL(fp),               DIMENSION(:), INTENT(IN)  :: x
    REAL(fp),               DIMENSION(:), INTENT(IN)  :: y
    REAL(fp),                             INTENT(IN)  :: x_int
    REAL(fp),                             INTENT(OUT) :: y_int
    REAL(fp),     OPTIONAL, DIMENSION(:), INTENT(IN)  :: y2
    CHARACTER(*), OPTIONAL,               INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL,               INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Spline_Interpolate(scalar)'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: n
    INTEGER :: i_lo, i_hi
    REAL(fp) :: a, b, dx
    REAL(fp), DIMENSION(SIZE(x)) :: y2_derivative
 

    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS 
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_Id

    ! Are vector sizes consistent?
    n = SIZE(x)
    IF ( SIZE(y) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input X, Y arguments are different sizes.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check for second derivatives
    IF ( PRESENT( y2 ) ) THEN
      IF ( SIZE( y2 ) /= n ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Input Y2 argument has inconsistent size.', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
      y2_derivative = y2
    ELSE
      ! Calculate them. This is inefficient so should only be done
      ! if X and Y are different each call.
      Error_Status = Spline_Initialize( x, y, y2_derivative,    &
                                        Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error occurred in spline initialisation function.', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF



    ! ---------------------------------------------------------
    ! Find the x-values in x that bracket the input x_int value
    ! by means of bisection. This is optimal if sequential
    ! calls to this function are at random values of x_int.
    ! ---------------------------------------------------------
    i_lo = MAX( 1, Bisection_Search( x, x_int ) )
    i_hi = MIN( i_lo + 1, n )


    ! ---------------------
    ! Calculate dx and test
    ! ---------------------
    dx = x(i_hi) - x(i_lo)
    IF ( dx < TOLERANCE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error : dx < or = 0. i_lo, i_hi : ", i6, 2x, i6, &
                        &" x(i_lo), x(i_hi) : ", f12.6, 2x, f12.6 )' ) &
                      i_lo, i_hi, x(i_lo), x(i_hi)
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! ---------------------
    ! Evaluate cubic dpline
    ! ---------------------
    a = ( x(i_hi) - x_int ) / dx
    b = ( x_int - x(i_lo) ) / dx
    y_int = ( a * y(i_lo) ) + ( b * y(i_hi) ) + &
            ( ( ( (a**3) - a ) * y2_derivative(i_lo) ) + &
              ( ( (b**3) - b ) * y2_derivative(i_hi) )   ) * &
            (dx*dx) / SIX

  END FUNCTION splint_scalar


  FUNCTION splint_rank1( x,            &  ! Input
                         y,            &  ! Input
                         x_int,        &  ! Input
                         y_int,        &  ! Output
                         y2,           &  ! Optional input
                         RCS_Id,       &  ! Optional output
                         Message_Log ) &  ! Error messaging
                       RESULT ( Error_Status )
    ! Arguments
    REAL(fp),               DIMENSION(:), INTENT(IN)  :: x      ! N
    REAL(fp),               DIMENSION(:), INTENT(IN)  :: y      ! N
    REAL(fp),               DIMENSION(:), INTENT(IN)  :: x_int  ! N_int
    REAL(fp),               DIMENSION(:), INTENT(OUT) :: y_int  ! N_int
    REAL(fp),     OPTIONAL, DIMENSION(:), INTENT(IN)  :: y2     ! N
    CHARACTER(*), OPTIONAL,               INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL,               INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Spline_Interpolate(rank-1)'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: n, n_int
    INTEGER :: i
    REAL(fp)  :: a, b, dx
    INTEGER,  DIMENSION(SIZE(x_int)) :: i_lo, i_hi
    REAL(fp), DIMENSION(SIZE(x))     :: y2_derivative
 

    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS 
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_Id

    ! Are input vector sizes consistent?
    n = SIZE(x)
    IF ( SIZE(y) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input X, Y arguments are different sizes.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Are interpolate vector sizes consistent?
    n_int = SIZE(x_int)
    IF ( SIZE(y_int) /= n_int ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Interpolate X_INT, Y_INT arguments are different sizes.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check for second derivatives
    IF ( PRESENT(y2) ) THEN
      IF ( SIZE(y2) /= n ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Input Y2 argument has inconsistent size.', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
      y2_derivative = y2
    ELSE
      ! Calculate them.
      Error_Status = Spline_Initialize( x, y, y2_derivative,    &
                                        Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error occurred in spline initialisation function.', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF


    ! ---------------------------------------------------------
    ! Find the x-values in x that bracket the input x_int value
    ! by means of bisection. This is optimal if sequential
    ! calls to this function are at random values of x_int.
    ! ---------------------------------------------------------
    i_lo = Value_Locate( x, x_int )
    i_hi = i_lo + 1


    ! ----------------------------
    ! Calculate interpolated array
    ! ----------------------------
    DO i = 1, n_int

      ! Calculate dx and test
      dx = x(i_hi(i)) - x(i_lo(i))
      IF ( dx < TOLERANCE ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error : dx < or = 0. i_lo, i_hi : ", i6, 2x, i6, &
                          &" x(i_lo), x(i_hi) : ", f12.6, 2x, f12.6 )' ) &
                        i_lo(i), i_hi(i), x(i_lo(i)), x(i_hi(i))
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF

      ! Evaluate cubic spline
      a = ( x(i_hi(i)) - x_int(i) ) / dx
      b = ( x_int(i) - x(i_lo(i)) ) / dx
      y_int(i) = ( a * y(i_lo(i)) ) + ( b * y(i_hi(i)) ) + &
                 ( ( ( (a**3) - a ) * y2_derivative(i_lo(i)) ) + &
                   ( ( (b**3) - b ) * y2_derivative(i_hi(i)) )   ) * &
                 (dx*dx) / SIX

    END DO

  END FUNCTION splint_rank1

END MODULE Spline_Interpolation
