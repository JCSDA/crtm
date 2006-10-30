!
! Interpolate_Utility
!
! Module containing interpolation routines
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 22-Nov-2000
!                       paul.vandelst@ssec.wisc.edu
!

MODULE Interpolate_Utility

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Modules used
  USE Type_Kinds,      ONLY: fp=>fp_kind
  USE Message_Handler, ONLY: SUCCESS, FAILURE, Display_Message
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Spline_Interpolate
  PUBLIC :: Spline_Initialize
  PUBLIC :: Polynomial_Interpolate
  PUBLIC :: Value_Locate

public :: polyint_test  ! Temporary


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  ! Public functions
  INTERFACE Value_Locate
    MODULE PROCEDURE Bisection_Search
    MODULE PROCEDURE Interpolate_Index
  END INTERFACE Value_Locate

  INTERFACE Polynomial_Interpolate
    MODULE PROCEDURE polyint_scalar
    MODULE PROCEDURE polyint_rank1
  END INTERFACE Polynomial_Interpolate

  INTERFACE Spline_Interpolate
    MODULE PROCEDURE splint_scalar
    MODULE PROCEDURE splint_rank1
  END INTERFACE Spline_Interpolate


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id field
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: Interpolate_Utility.f90,v 1.3 2006/06/07 20:21:32 wd20pd Exp $'
  ! Maximum order for interpolating polynomial
  INTEGER,        PRIVATE, PARAMETER :: MAX_ORDER = 11
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  REAL(fp), PARAMETER :: TWO  = 2.0_fp
  REAL(fp), PARAMETER :: SIX  = 6.0_fp
  REAL(fp), PARAMETER :: TOLERANCE = EPSILON( ZERO )


CONTAINS


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  FUNCTION Check_Order( Order, n, Message_Log ) RESULT( Error_Status )
    ! Arguments
    INTEGER,                INTENT(IN) :: Order
    INTEGER,                INTENT(IN) :: n
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Check_Order'
    ! Local variables
    CHARACTER(256) :: Message

    ! Set up
    Error_Status = SUCCESS

    ! Only odd, non-zero orders accepted
    IF ( MOD( Order, 2 ) == 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Input polynomial order of ", i2, &
                        &" invalid. Must be odd." )' ) Order
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Only positive, non-zero orders accepted
    IF ( Order < 1 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Input polynomial order of ", i2, &
                        &" invalid. Must be > 0." )' ) Order
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check that the order is compatible with the number
    ! of points available for interpolation
    IF ( n < (Order+1) ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Minimum number of input points for order ", i2, &
                        &" interpolation is ", i2, "." )' ) &
                      Order, Order+1
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, & 
                            Message_Log=Message_Log )
      RETURN
    END IF



    ! Only reasonable values also
    IF ( Order > MAX_ORDER ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Input polynomial order of ", i2, &
                        &" invalid. Must be < or = ", i2, "." )' ) &
                      Order, MAX_ORDER
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION Check_Order




!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PUBLIC MODULE ROUTINES ##                        ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!------------------------------------------------------------------------------
!
! NAME:
!       Bisection_Search
!
! PURPOSE:
!       Function to search an array using the bisection method. This function
!       is an adaptation from Numerical Recipes and is most efficient across
!       multiple calls when the value to be searched for in the array occurs
!       randomly.
!
! CALLING SEQUENCE:
!       j = Bisection_Search( x, u,          &  ! Input
!                             xLower=xLower, &  ! Optional input
!                             xUpper=xUpper  )  ! Optional input
!
! INPUT ARGUMENTS:
!       x:         The array to be searched.
!                  UNITS:      N/A
!                  TYPE:       REAL(fp)
!                  DIMENSION:  Rank-1
!                  ATTRIBUTES: INTENT(IN)
!
!       u:         The value to be searched for in the array.
!                  UNITS:      N/A
!                  TYPE:       REAL(fp)
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       xLower:   Set this optional argument to the index of the input
!                  array corresponding to the LOWER search boundary.
!                  If not specified, the default value is 1.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       xUpper:   Set this optional argument to the index of the input
!                  array corresponding to the UPPER search boundary.
!                  If not specified, the default value is SIZE(x).
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       j:         The lower index of the two values in the input array, x,
!                  that bracket the input value, u, i.e.
!                    x(j) < u < x(j+1)
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 22-Nov-2000
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Bisection_Search( x, u, xLower, xUpper ) RESULT( j )
    ! Arguments
    REAL(fp), DIMENSION(:), INTENT(IN) :: x
    REAL(fp),               INTENT(IN) :: u
    INTEGER,  OPTIONAL,     INTENT(IN) :: xLower
    INTEGER,  OPTIONAL,     INTENT(IN) :: xUpper
    ! Function result
    INTEGER :: j
    ! Local variables
    INTEGER :: n
    INTEGER :: jLower
    INTEGER :: jMiddle
    INTEGER :: jUpper

    ! Set up
    n = SIZE( x )

    ! Initialise upper and lower limits to
    ! the valid maximums, 1 and n
    IF ( PRESENT(xLower) ) THEN
      jLower = xLower
    ELSE
      jLower = 1
    END IF

    IF ( PRESENT(xUpper) ) THEN
      jUpper = xUpper
    ELSE
      jUpper = n
    END IF


    ! Search for the required index by bisection
    Bisection_Search_Loop: DO

      ! If the index ranges have converged, we're done
      IF ( (jUpper-jLower) <= 1 ) EXIT Bisection_Search_Loop

      ! Define a middle point
      jMiddle = ( jLower + jUpper ) / 2

      ! Which half is the required value in?
      IF ( ( x(n) > x(1) ) .EQV. ( u > x(jMiddle) ) ) THEN
        jLower = jMiddle ! The "upper" half
      ELSE
        jUpper = jMiddle ! The "lower" half
      END IF

    END DO Bisection_Search_Loop

    ! Define the return value
    j = jLower

  END FUNCTION Bisection_Search


!------------------------------------------------------------------------------
!
! NAME:
!       Value_Locate
!
! PURPOSE:
!       Function that finds the intervals within a given monotonic
!       vector that brackets a given set of one or more search values.
!
!       This function is an adaptation of the locate() routine in 
!       Numerical Recipes and uses the bisection method to locate the
!       interval.
!
! CALLING SEQUENCE:
!       j = Value_Locate( x, u ) ! Input
!
! INPUT ARGUMENTS:
!       x:         The array to be searched.
!                  UNITS:      N/A
!                  TYPE:       REAL(fp)
!                  DIMENSION:  Rank-1
!                  ATTRIBUTES: INTENT(IN)
!
!       u:         The array of values to be searched for in the array.
!                  UNITS:      N/A
!                  TYPE:       REAL(fp)
!                  DIMENSION:  Rank-1
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       j:         The integer array of lower indices of the two values in
!                  the input array, x, that bracket the input values, u.
!                  E.g. for a given u(i):
!                     x(j) < u(i) < x(j+1)
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Rank-1, same size as u input argument.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 22-Nov-2000
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Interpolate_Index( x, u ) RESULT( j )
    ! Arguments
    REAL(fp), DIMENSION(:), INTENT(IN) :: x
    REAL(fp), DIMENSION(:), INTENT(IN) :: u
    ! Function result
    INTEGER, DIMENSION(SIZE(u)) :: j
    ! Local variables
    INTEGER :: nx
    INTEGER :: nu, iu
    INTEGER :: xLower, xUpper
    LOGICAL :: ascending

    ! Set up
    nx = SIZE(x)
    nu = SIZE(u)

    ! Determine if arrays are sorted in ascending or descending order
    IF ( x(nx) > x(1) .AND. u(nu) > u(1) ) THEN
      ascending = .TRUE.
    ELSEIF ( x(nx) < x(1) .AND. u(nu) < u(1) ) THEN
      ascending = .FALSE.
    ELSE
      j(:) = -1
      RETURN
    END IF

    ! Perform the bisection search for each element of u
    IF ( ascending ) THEN
      ! Going up
      xLower = 1
      DO iu = 1, nu
        j(iu) = Bisection_Search( x, u(iu), xLower = xLower )
        xLower = MAX( 1, j(iu) )
      END DO
    ELSE
      ! Going down
      xUpper = nx
      DO iu = 1, nu
        j(iu) = Bisection_Search( x, u(iu), xUpper = xUpper )
        xLower = MIN( j(iu), nx )
      END DO
    END IF

  END FUNCTION Interpolate_Index




  FUNCTION polyint_test( x,            &  ! Input
                         y,            &  ! Input
                         x_int,        &  ! Input
                         y_int,dy,        &  ! Output
                         Order,        &  ! Optional input
                         RCS_Id,       &  ! Optional output
                         Message_Log ) &  ! Error messaging
                       RESULT( Error_Status )
    ! Arguments
    REAL(fp), DIMENSION(:), INTENT(IN)  :: x
    REAL(fp), DIMENSION(:), INTENT(IN)  :: y
    REAL(fp),               INTENT(IN)  :: x_int
    REAL(fp),               INTENT(OUT) :: y_int,dy
    INTEGER,      OPTIONAL, INTENT(IN)  :: Order
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'polyint_test'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: nPoints, nMin
    INTEGER :: n, l, m
    INTEGER :: kBegin, kEnd
    INTEGER, DIMENSION( MAX_ORDER + 1 ) :: k
    INTEGER :: i
    REAL(fp), DIMENSION( MAX_ORDER+1 ) :: c, d
    REAL(fp) :: den, w, ho, hp
integer :: ns


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_Id

    ! Check array sizes for consistency
    nPoints = SIZE(x)
    IF ( nPoints /= SIZE(y) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input X and Y vectors have different sizes.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Interpolating polynomial order
    n = 1  ! Assume linear
    IF ( PRESENT(Order) ) THEN
      ! Check if value if valid
      Error_Status = Check_Order( Order, nPoints, &
                                  Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Invalid polynomial order specified', &
                              Error_Status,    &
                              Message_Log=Message_Log )
        RETURN
      END IF
      ! Assign order to variable
      n = Order
    END IF

    ! Check that input array is large enough
    IF ( nPoints < (n+1) ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Input X and Y arrays must have ", i2, &
                        &" or more points for interpolating", &
                        &" polynomial of order ", i2, "." )' ) &
                      n+1, n
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! --------------------------------------------------------------------------
    !                  Create local interpolation index array
    !
    !  This index array is to reference the input abscissa points that bracket  
    !  those at which interpolates are required. E.g. for linear interpolation  
    !  the bracketing looks like:                                               
    !
    !               x(i+0) < x_int < x(i+1)                                      
    !
    !  where the index modifiers 0 and 1 are the "k" below. For cubic interp-   
    !  olation (order = 3), the bracketing looks like:                          
    !
    !      x(i-1) < x(i+0) < x_int < x(i+1) < x(i+2)                             
    !
    !  where the index modifiers are now -1, 0, +1, and +2. For quadratic       
    !  interpolation (order = 2), the bracketing looks like:                    
    !
    !      x(i-1) < x(i+0) < x_int < x(i+1)                                      
    !
    !  NOTE: For even values of the interpolating polynomial order, the         
    !  bracketing is lopsided with more points to the left of the interpolation 
    !  point. This will produce interpolates that are quasi-discontinuous at    
    !  the bracketing boundaries (by quasi-discontinuous I mean it looks weird. 
    !  So, keep that in mind when you plot the interpolated result.             
    ! --------------------------------------------------------------------------
    ! Set the begin and end. Here
    ! integer division is relied upon
    kBegin = 0 - (n/2)
    kEnd   = 1 + (n/2)
    ! File the array
    k(1:n+1) = (/ ( l, l = kBegin, kEnd ) /)


    ! -----------------------------------------------------
    ! Find the locations for which interpolates are desired
    ! -----------------------------------------------------
    ! Result is the lower index, i.e. x(i) < x_int < x(i+1)
    i = Bisection_Search( x, x_int )

    ! Make indices valid for Lagrange polynomials, i.e. ensure
    ! that the sum of (i+k) does not reference non-existant
    ! indices. For those cases, the result is set to the
    ! maximum and minimum values allowed for the beginning and
    ! end of the array respectively. The result is that
    ! extrapolation may be performed.
    !
    ! NOTE: The value (n+2)/2 rather than (n+1)/2 is used so
    ! that the result is valid for even values of the
    ! polynomial order, n.
    nMin = (n+2)/2
    IF ( i <  nMin            ) i = nMin
    IF ( i > (nPoints - nMin) ) i = nPoints - nMin
    c(1:n+1) = y( i + k(1:n+1) )
    d(1:n+1) = y( i + k(1:n+1) )
    y_Int = y( i )
    ns=n/2
!print *, k(1:n+1)
!print *, n, i
!print *

    DO l = 1, n
      DO m = 1, n - l + 1
!write(*,'(5(1x,i3))')m, l, k(m),i+k(m),i+k(m)+l

        ho = x(i+k(m)  ) - x_Int
        hp = x(i+k(m)+l) - x_Int
        IF ( ABS( ho - hp ) < EPSILON(1.0_fp) ) THEN
          WRITE(*,*)'Ho = Hp ERROR'
          Error_Status = FAILURE
          RETURN
        END IF

        w = c(m+1) - d(m)
        den = w / ( ho - hp )
        c(m) = ho * den
        d(m) = hp * den
      END DO

      IF ( 2*ns < n-l+1 ) THEN
        dy = c(ns+1)
      ELSE
        dy = d(ns)
        ns=ns-1
      END IF

      y_Int = y_Int + dy

    END DO

  END FUNCTION polyint_test



!--------------------------------------------------------------------------------
!
! NAME:
!       Polynomial_Interpolate
!
! PURPOSE:
!       Function that performs polynomial interpolation on input scalars
!       or vectors.
!
! CALLING SEQUENCE:
!       Error_Status = Polynomial_Interpolate( x,                      &  ! Input
!                                              y,                      &  ! Input
!                                              x_int,                  &  ! Input
!                                              y_int,                  &  ! Output
!                                              Order      =Order,      &  ! Optional input
!                                              RCS_Id     =RCS_Id,     &  ! Optional output
!                                              Message_Log=Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       x:            The abscissa values for the tabulated function, y=F(x).
!                     Must be monotonically ascending or descending values.
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
!                     DIMENSION:  Rank-1 array, same as x.
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
! OPTIONAL INPUT ARGUMENTS:
!       Order:        The order of the interpolating polynomial. If not
!                     specified, linear interpolation (Order = 1) is the
!                     default. If specified, must be an odd number > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(IN)
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
! OUTPUT ARGUMENTS:
!       y_int:        Interpolate values corresponding to the X_INT abscissa
!                     values. Must have the same number of elements as the
!                     X_INT input argument.
!                     UNITS:      Data dependent, same as y.
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Scalar or Rank-1 array, same as x_int.
!                     ATTRIBUTES: INTENT(IN)
!
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
!       - Input X and X_INT vectors *must* be monotonically ascending
!         or descending in the same direction.
!       - All floating point inputs and outputs must be either sp
!         OR dp precision, not mixtures of both.
!
! PROCEDURE:
!       The function first calculates the fundamental Lagrange polynomials,
!                                                                          
!                 n+1
!                _____
!                 | |     x   - x(k)
!         p (x) = | |  --------------
!          j      | |    x(j) - x(k)
!                k = 1
!                k /= j
!
!        which are then used to compute the final interpolating polynomial,
!
!                 ___ n+1
!                \
!         P(x) =  >     p (x) . y(j)
!                /___    j
!                     j=1
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 23-Mar-2002
!                       paul.vandelst@ssec.wisc.edu
!
!S-
!--------------------------------------------------------------------------------

  FUNCTION polyint_scalar( x,            &  ! Input
                           y,            &  ! Input
                           x_int,        &  ! Input
                           y_int,        &  ! Output
                           x_idx,        &  ! Optional input
                           Order,        &  ! Optional input
                           RCS_Id,       &  ! Optional output
                           Message_Log ) &  ! Error messaging
                         RESULT( Error_Status )
    ! Arguments
    REAL(fp), DIMENSION(:), INTENT(IN)  :: y
    REAL(fp), DIMENSION(:), INTENT(IN)  :: x
    REAL(fp),               INTENT(IN)  :: x_int
    REAL(fp),               INTENT(OUT) :: y_int
    INTEGER,      OPTIONAL, INTENT(IN)  :: x_idx
    INTEGER,      OPTIONAL, INTENT(IN)  :: Order
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Polynomial_Interpolate(scalar)'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: nPoints, nMin
    INTEGER :: n, l, m
    INTEGER :: kBegin, kEnd, j
    INTEGER, DIMENSION(MAX_ORDER+1) :: k
    INTEGER :: i
    REAL(fp) :: polynomial


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_Id

    ! Check array sizes for consistency
    nPoints = SIZE(x)
    IF ( nPoints /= SIZE(y) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input X and Y vectors have different sizes.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Interpolating polynomial order
    n = 1  ! Assume linear
    IF ( PRESENT(Order) ) THEN
      ! Check if value if valid
      Error_Status = Check_Order( Order, nPoints, &
                                  Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Invalid polynomial Order specified', &
                              Error_Status,    &
                              Message_Log=Message_Log )
        RETURN
      END IF
      ! Assign order to variable
      n = Order
    END IF

    ! Check that input array is large enough
    IF ( nPoints < (n+1) ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Input X and Y arrays must have ", i2, &
                        &" or more points for interpolating", &
                        &" polynomial of order ", i2, "." )' ) &
                      n+1, n
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! --------------------------------------------------------------------------
    !                   Create local interpolation index array
    !
    !  This index array is to reference the input abscissa points that bracket  
    !  those at which interpolates are required. E.g. for linear interpolation  
    !  the bracketing looks like:                                               
    !
    !               x(i+0) < x_int < x(i+1)                                      
    !
    !  where the index modifiers 0 and 1 are the "k" below. For cubic interp-   
    !  olation (order = 3), the bracketing looks like:                          
    !
    !      x(i-1) < x(i+0) < x_int < x(i+1) < x(i+2)                             
    !
    !  where the index modifiers are now -1, 0, +1, and +2. For quadratic       
    !  interpolation (order = 2), the bracketing looks like:                    
    !
    !      x(i-1) < x(i+0) < x_int < x(i+1)                                      
    !
    !  NOTE: For even values of the interpolating polynomial order, the         
    !  bracketing is lopsided with more points to the left of the interpolation 
    !  point. This will produce interpolates that are quasi-discontinuous at    
    !  the bracketing boundaries (by quasi-discontinuous I mean it looks weird. 
    !  So, keep that in mind when you plot the interpolated result.             
    ! --------------------------------------------------------------------------
    ! Set the begin and end. Here
    ! integer division is relied upon
    kBegin = 0 - (n/2)
    kEnd   = 1 + (n/2)
    ! File the array
    k(1:n+1) = (/ ( l, l = kBegin, kEnd ) /)


    ! -----------------------------------------------------
    ! Find the locations for which interpolates are desired
    ! -----------------------------------------------------
    ! Result is the lower index, i.e. x(i) < x_int < x(i+1)
    IF ( PRESENT(x_idx) ) THEN
      i = x_idx
    ELSE
      i = Bisection_Search( x, x_int )
    END IF

    ! Make indices valid for Lagrange polynomials, i.e. ensure
    ! that the sum of (i+k) does not reference non-existant
    ! indices. For those cases, the result is set to the
    ! maximum and minimum values allowed for the beginning and
    ! end of the array respectively. The result is that
    ! extrapolation may be performed.
    ! NOTE: The value (n+2)/2 rather than (n+1)/2 is used so
    ! that the result is valid for even values of the
    ! polynomial order, n.
    nMin = (n+2)/2
    IF ( i <  nMin            ) i = nMin
    IF ( i > (nPoints - nMin) ) i = nPoints - nMin


    ! -------------------------------------------------------------
    ! Compute and sum the Lagrange interpolating polynomials
    !
    !  Here the fundamental polynomials are calculated:
    !
    !           n+1
    !          _____
    !           | |     x   - x(k)
    !   p (x) = | |  --------------
    !    j      | |    x(j) - x(k)
    !          k = 1
    !          k /= j
    !
    !  and then used to compute the final interpolating polynomial,
    !
    !           ___ n+1
    !          \
    !   P(x) =  >     p (x) . y(j)
    !          /___    j
    !               j=1
    ! 
    ! -------------------------------------------------------------
    ! Initialise interpolate result
    y_int = ZERO

    ! Loop over polynomial order
    Order_Loop: DO l = 1, n + 1 

      ! Identify the index *not* to use for current
      ! order (just for clarity in code )
      j = k(l)

      ! Calculate the fundamental polynomial. Note that the
      ! PRODUCT intrinsic is not used here
      !   polynomial = PRODUCT( ( x_int - x(i+k(1:n+1)) )/( x(i+j) - x(i+k(1:n+1)) ), &
      !                         MASK = ( k(1:n+1) /= j )                              )
      ! since, with higher orders, any divide-by-zero floating point
      ! traps must be turned off. The mask applies to the function
      ! result, NOT the array expression evaluation.
      polynomial = ONE
      Product_Loop: DO m = 1, n+1
        IF ( k(m) == j ) CYCLE Product_Loop
        polynomial = polynomial * ( ( x_int - x(i+k(m)) )/( x(i+j) - x(i+k(m)) ) )
      END DO Product_Loop

      ! Calculate and sum the interpolating polynomial
      y_int = y_int + ( polynomial * y(i+j) )

    END DO Order_Loop

  END FUNCTION polyint_scalar


  FUNCTION polyint_rank1( x,            &  ! Input
                          y,            &  ! Input
                          x_int,        &  ! Input
                          y_int,        &  ! Output
                          x_idx,        &  ! Optional input
                          Order,        &  ! Optional input
                          RCS_Id,       &  ! Optional output
                          Message_Log ) &  ! Error messaging
                        RESULT( Error_Status )
    ! Arguments
    REAL(fp), DIMENSION(:),           INTENT(IN)  :: x
    REAL(fp), DIMENSION(:),           INTENT(IN)  :: y
    REAL(fp), DIMENSION(:),           INTENT(IN)  :: x_int
    REAL(fp), DIMENSION(:),           INTENT(OUT) :: y_int
    INTEGER,  DIMENSION(:), OPTIONAL, INTENT(IN)  :: x_idx
    INTEGER,                OPTIONAL, INTENT(IN)  :: Order
    CHARACTER(*),           OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*),           OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Polynomial_Interpolate(rank-1)'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: nPoints, nIntPoints, nMin
    INTEGER :: n, l, m
    INTEGER :: kBegin, kEnd, j
    INTEGER, DIMENSION(MAX_ORDER+1) :: k
    INTEGER,  DIMENSION(SIZE(x_int)) :: i
    REAL(fp), DIMENSION(SIZE(x_int)) :: polynomial


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_Id

    ! Check array sizes for consistency
    nPoints = SIZE(x)
    IF ( nPoints /= SIZE(y) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input X and Y vectors have different sizes.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    nIntPoints = SIZE(x_int)
    IF ( SIZE(y_int) /= nIntPoints ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input X_INT and output Y_INT vectors have different sizes.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    IF ( PRESENT(x_idx) ) THEN
      IF ( SIZE(x_idx) /= nIntPoints ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Input X_IDX vector has an inconsistent size.', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF

    ! Interpolating polynomial order
    n = 1  ! Assume linear
    IF ( PRESENT(Order) ) THEN
      ! Check if value if valid
      Error_Status = Check_Order( Order, nPoints, &
                                  Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Invalid polynomial Order specified', &
                              Error_Status,    &
                              Message_Log=Message_Log )
        RETURN
      END IF
      ! Assign order to variable
      n = Order
    END IF

    ! Check that input array is large enough
    IF ( nPoints < (n+1) ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Input X and Y arrays must have ", i2, &
                        &" or more points for interpolating", &
                        &" polynomial of order ", i2, "." )' ) &
                      n+1, n
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! --------------------------------------------------------------------------
    !                   Create local interpolation index array
    !
    !  This index array is to reference the input abscissa points that bracket  
    !  those at which interpolates are required. E.g. for linear interpolation  
    !  the bracketing looks like:                                               
    !
    !               x(i+0) < x_int < x(i+1)                                      
    !
    !  where the index modifiers 0 and 1 are the "k" below. For cubic interp-   
    !  olation (order = 3), the bracketing looks like:                          
    !
    !      x(i-1) < x(i+0) < x_int < x(i+1) < x(i+2)                             
    !
    !  where the index modifiers are now -1, 0, +1, and +2. For quadratic       
    !  interpolation (order = 2), the bracketing looks like:                    
    !
    !      x(i-1) < x(i+0) < x_int < x(i+1)                                      
    !
    !  NOTE: For even values of the interpolating polynomial order, the         
    !  bracketing is lopsided with more points to the left of the interpolation 
    !  point. This will produce interpolates that are quasi-discontinuous at    
    !  the bracketing boundaries (by quasi-discontinuous I mean it looks weird. 
    !  So, keep that in mind when you plot the interpolated result.             
    ! --------------------------------------------------------------------------
    ! Set the begin and end. Here
    ! integer division is relied upon
    kBegin = 0 - (n/2)
    kEnd   = 1 + (n/2)
    ! File the array
    k(1:n+1) = (/ ( l, l = kBegin, kEnd ) /)


    ! -----------------------------------------------------
    ! Find the locations for which interpolates are desired
    ! -----------------------------------------------------
    ! Result is the lower index, i.e. x(i) < x_int < x(i+1)
    IF ( PRESENT(x_idx) ) THEN
      i = x_idx
    ELSE
      i = Value_Locate( x, x_int )
    END IF

    ! Make indices valid for Lagrange polynomials, i.e. ensure
    ! that the sum of (i+k) does not reference non-existant
    ! indices. For those cases, the result is set to the
    ! maximum and minimum values allowed for the beginning and
    ! end of the array respectively. The result is that
    ! extrapolation may be performed.
    ! NOTE: The value (n+2)/2 rather than (n+1)/2 is used so
    ! that the result is valid for even values of the
    ! polynomial order, n.
    nMin = (n+2)/2
    WHERE( i <  nMin            ) i = nMin
    WHERE( i > (nPoints - nMin) ) i = nPoints - nMin


    ! -------------------------------------------------------------
    ! Compute and sum the Lagrange interpolating polynomials
    !
    !  Here the fundamental polynomials are calculated:
    !
    !           n+1
    !          _____
    !           | |     x   - x(k)
    !   p (x) = | |  --------------
    !    j      | |    x(j) - x(k)
    !          k = 1
    !          k /= j
    !
    !  and then used to compute the final interpolating polynomial,
    !
    !           ___ n+1
    !          \
    !   P(x) =  >     p (x) . y(j)
    !          /___    j
    !               j=1
    ! 
    ! -------------------------------------------------------------
    ! Initialise interpolate result
    y_int = ZERO

    ! Loop over polynomial order
    Order_Loop: DO l = 1, n + 1 

      ! Identify the index *not* to use for current
      ! order (just for clarity in code )
      j = k(l)

      ! Calculate the fundamental polynomial. Note that the
      ! PRODUCT intrinsic is not used here
      !   polynomial = PRODUCT( ( x_int - x(i+k(1:n+1)) )/( x(i+j) - x(i+k(1:n+1)) ), &
      !                         MASK = ( k(1:n+1) /= j )                              )
      ! since, with higher orders, any divide-by-zero floating point
      ! traps must be turned off. The mask applies to the function
      ! result, NOT the array expression evaluation.
      polynomial = ONE
      Product_Loop: DO m = 1, n+1
        IF ( k(m) == j ) CYCLE Product_Loop
        polynomial = polynomial * ( ( x_int - x(i+k(m)) )/( x(i+j) - x(i+k(m)) ) )
      END DO Product_Loop

      ! Calculate and sum the interpolating polynomial
      y_int = y_int + ( polynomial * y(i+j) )

    END DO Order_Loop

  END FUNCTION polyint_rank1


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
    REAL(fp),           DIMENSION(:), INTENT(IN)  :: x  ! N
    REAL(fp),           DIMENSION(:), INTENT(IN)  :: y  ! N
    REAL(fp),           DIMENSION(:), INTENT(IN)  :: x_int   ! N_int
    REAL(fp),           DIMENSION(:), INTENT(OUT) :: y_int   ! N_int
    REAL(fp), OPTIONAL, DIMENSION(:), INTENT(IN)  :: y2 ! N
    CHARACTER(*), OPTIONAL,                 INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL,                 INTENT(IN)  :: Message_Log
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

END MODULE Interpolate_Utility
