!
! Polynomial_Interpolation
!
! Module containing polynomial interpolation routines
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 06-Oct-2006
!                       paul.vandelst@ssec.wisc.edu
!

MODULE Polynomial_Interpolation

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
  PUBLIC :: Polynomial_Interpolate

public :: polyint_test  ! Temporary


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE Polynomial_Interpolate
    MODULE PROCEDURE polyint_scalar
    MODULE PROCEDURE polyint_rank1
  END INTERFACE Polynomial_Interpolate

  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id field
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  ! Maximum order for interpolating polynomial
  INTEGER,        PRIVATE, PARAMETER :: MAX_ORDER = 11
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp


CONTAINS


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!--------------------------------------------------------------------------------
!
! NAME:
!       Check_Order
!
! PURPOSE:
!       Function to check the order of an interpolating polynominal
!
! CALLING SEQUENCE:
!       Error_Status = Check_Order( Order                  , &  ! Input
!                                   n                      , &  ! Input
!                                   Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Order:        The polynomial order of an interpolating polynomial.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       n:            The number of points in the dataset to be
!                     interpolated.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
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
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the polynomial order is valid for the number
!                                   of data points.
!                        == FAILURE - the order is an even number.
!                                   - the order is < or = zero.
!                                   - the number of points is < order+1
!                                   - the order is greater than a maximum size.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

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
!       Function to perform polynomial interpolation on input scalars
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

END MODULE Polynomial_Interpolation
