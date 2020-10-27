!--------------------------------------------------------------------------------
!M+
! NAME:
!       interpolate
!
! PURPOSE:
!       Module containing interpolation routines
!
! CATEGORY:
!       Interpolation
!
! CALLING SEQUENCE:
!       USE interpolate
!
! MODULES:
!       type_kinds:             Module containing definitions for kinds of
!                               variable types
!
!       error_handler:          Module containing error codes and error
!                               handling routines
!                               USEs: FILE_UTILITY module
!
! CONTAINS:
!       polynomial_interpolate: Function that performs polynomial interpolation
!                               on input scalars or vectors.
!
!       spline_interpolate:     Function that performs cubic spline interpolation
!                               on input scalars or vectors.
!
!       spline_initialize:      Function to establish the type of interpolating
!                               spline for a tabulated set of functional values.
!
! EXTERNALS:
!       None.
!
! INCLUDE FILES:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 22-Nov-2000
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2000 Paul van Delst
!
!M-
!--------------------------------------------------------------------------------


MODULE interpolate


  ! ------------
  ! Modules used
  ! ------------

  USE type_kinds
  USE Message_handler


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: polynomial_interpolate
  PUBLIC :: spline_interpolate
  PUBLIC :: spline_initialize


  ! --------------------
  ! Function overloading
  ! --------------------

  ! -- Public functions
  INTERFACE spline_interpolate
    MODULE PROCEDURE splint_scalar_single
    MODULE PROCEDURE splint_scalar_double
    MODULE PROCEDURE splint_rank1_single
    MODULE PROCEDURE splint_rank1_double
  END INTERFACE ! spline_interpolate

  INTERFACE spline_initialize
    MODULE PROCEDURE splinit_single
    MODULE PROCEDURE splinit_double
  END INTERFACE ! spline_initialize

  INTERFACE polynomial_interpolate
    MODULE PROCEDURE polyint_scalar_single
    MODULE PROCEDURE polyint_scalar_double
    MODULE PROCEDURE polyint_rank1_single
    MODULE PROCEDURE polyint_rank1_double
  END INTERFACE ! polynomial_interpolate

  ! -- Private functions
  INTERFACE value_locate
    MODULE PROCEDURE locate_single
    MODULE PROCEDURE locate_double
  END INTERFACE ! value_locate

  INTERFACE bisection_search
    MODULE PROCEDURE bisearch_single
    MODULE PROCEDURE bisearch_double
  END INTERFACE ! bisection_search


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- RCS Id field
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &

  ! -- Maximum order for interpolating polynomial
  INTEGER, PRIVATE, PARAMETER :: MAX_ORDER = 9

  ! - Single precision == REAL*4
  REAL( Single ), PRIVATE, PARAMETER :: S_ZERO = 0.0_Single
  REAL( Single ), PRIVATE, PARAMETER :: S_ONE  = 1.0_Single
  REAL( Single ), PRIVATE, PARAMETER :: S_TWO  = 2.0_Single
  REAL( Single ), PRIVATE, PARAMETER :: S_SIX  = 6.0_Single
  REAL( Single ), PRIVATE, PARAMETER :: S_TOLERANCE = EPSILON( S_ZERO )

  ! -- Double precision == REAL*8
  REAL( Double ), PRIVATE, PARAMETER :: D_ZERO = 0.0_Double
  REAL( Double ), PRIVATE, PARAMETER :: D_ONE  = 1.0_Double
  REAL( Double ), PRIVATE, PARAMETER :: D_TWO  = 2.0_Double
  REAL( Double ), PRIVATE, PARAMETER :: D_SIX  = 6.0_Double
  REAL( Double ), PRIVATE, PARAMETER :: D_TOLERANCE = EPSILON( D_ZERO )


CONTAINS





  FUNCTION polyint_scalar_Single( x,            &  ! Input
                                  y,            &  ! Input
                                  x_int,        &  ! Input
                                  y_int,        &  ! Output
                                  order,        &  ! Optional input
                                  RCS_Id,       &  ! Optional output
                                  message_log ) &  ! Error messaging
                                RESULT( error_status )


    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( Single ), DIMENSION( : ), INTENT( IN )  :: x
    REAL( Single ), DIMENSION( : ), INTENT( IN )  :: y
    REAL( Single ),                 INTENT( IN )  :: x_int

    ! -- Output
    REAL( Single ),                 INTENT( OUT ) :: y_int

    ! -- Optional input
    INTEGER,        OPTIONAL,       INTENT( IN )  :: order

    ! -- Optional output
    CHARACTER( * ), OPTIONAL,       INTENT( OUT ) :: RCS_Id

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,       INTENT( IN )  :: message_log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Polynomial_Interpolate'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: message

    INTEGER :: n_points, n_min
    INTEGER :: n, l
    INTEGER :: k_begin, k_end, j
    INTEGER, DIMENSION( MAX_ORDER + 1 ) :: k
    INTEGER :: i
    REAL( Single ) :: polynomial



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------
    ! Check array sizes for consistency
    ! ---------------------------------

    n_points = SIZE( x )
    IF ( n_points /= SIZE( y ) ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Input X and Y vectors have different sizes.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF


    ! ------------------------------
    ! Interpolating polynomial order
    ! ------------------------------

    ! -- Assume linear interpolation if order not specified...
    n = 1

    ! -- ...otherwise check it
    IF ( PRESENT( order ) ) THEN

      ! -- Only odd, non-zero orders accepted
      IF ( MOD( order, 2 ) == 0 ) THEN
        error_status = FAILURE
        WRITE( message, '( "Input polynomial order of ", i2, &
                          &" invalid. Must be odd." )' ) order
        CALL display_message( ROUTINE_NAME,    &
                              TRIM( message ), &
                              error_status,    &
                              message_log = message_log )
        RETURN
      END IF

      ! -- Only positive, non-zero also
      IF ( order < 1 ) THEN
        error_status = FAILURE
        WRITE( message, '( "Input polynomial order of ", i2, &
                          &" invalid. Must be odd number > 0." )' ) order
        CALL display_message( ROUTINE_NAME,    &
                              TRIM( message ), &
                              error_status,    &
                              message_log = message_log )
        RETURN
      END IF

      ! -- Only reasonable values also
      IF ( order > MAX_ORDER ) THEN
        error_status = FAILURE
        WRITE( message, '( "Input polynomial order of ", i2, &
                          &" invalid. Must be odd number < or = ", i2, "." )' ) &
                        order, MAX_ORDER
        CALL display_message( ROUTINE_NAME,    &
                              TRIM( message ), &
                              error_status,    &
                              message_log = message_log )
        RETURN
      END IF

      ! -- Assign order to variable
      n = order

    END IF


    ! --------------------------------------
    ! Check that input array is large enough
    ! --------------------------------------

    IF ( n_points < ( n+1 ) ) THEN
      error_status = FAILURE
      WRITE( message, '( "Input X and Y arrays must have ", i2, &
                        &" or more points for interpolating", &
                        &" polynomial of order ", i2, "." )' ) &
                      n+1, n
      CALL display_message( ROUTINE_NAME,    &
                            TRIM( message ), &
                            error_status,    &
                            message_log = message_log )
      RETURN
    END IF


    ! -----------------------------------
    ! Set the RCS Id argument if supplied
    ! -----------------------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_Id
    END IF



    !#--------------------------------------------------------------------------#
    !#               -- CREATE LOCAL INTERPOLATION INDEX ARRAY --               #
    !#                                                                          #
    !# This index array is to reference the input abscissa points that bracket  #
    !# those at which interpolates are required. E.g. for linear interpolation  #
    !# the bracketing looks like:                                               #
    !#                                                                          #
    !#              x(i+0) < x_int < x(i+1)                                     # 
    !#                                                                          #
    !# where the index modifiers 0 and 1 are the "k" below. For cubic interp-   #
    !# olation (order = 3), the bracketing looks like:                          #
    !#                                                                          #
    !#     x(i-1) < x(i+0) < x_int < x(i+1) < x(i+2)                            # 
    !#                                                                          #
    !# where the index modifiers are now -1, 0, +1, and +2. This is the main    #
    !# reason only odd polynomial orders are accepted. Even-valued orders would #
    !# produce "lopsided" bracketing which can in turn produce interpolates     #
    !# that are quasi-discontinuou at the bracketing boundaries (by quasi-      #
    !# discontinuous I mean it looks weird :o)                                  #
    !#--------------------------------------------------------------------------#

    ! -- Set the begin and end. Here
    ! -- integer division is relied upon
    k_begin = 0 - (n/2)
    k_end   = 1 + (n/2)

    ! -- File the array
    k(1:n+1) = (/ ( l, l = k_begin, k_end ) /)



    !#--------------------------------------------------------------------------#
    !#       -- FIND THE LOCATIONS FOR WHICH INTERPOLATES ARE DESIRED --        #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------------------------
    ! Result is the lower index, i.e. x(i) < x_int < x(i+1)
    ! -----------------------------------------------------

    i = bisection_search( x, x_int )


    ! --------------------------------------------------------
    ! Make indices valid for Lagrange polynomials, i.e. ensure
    ! that the sum of (i+k) does not reference non-existant
    ! indices. For those cases, the result is set to the
    ! maximum and minimum values allowed for the beginning and
    ! end of the array respectively. The result is that
    ! extrapolation may be performed.
    ! --------------------------------------------------------

    n_min = (n+1)/2

    IF ( i <  n_min             ) i = n_min
    IF ( i > (n_points - n_min) ) i = n_points - n_min



    !#--------------------------------------------------------------------------#
    !#        -- COMPUTE AND SUM THE LAGRANGE INTERPOLATING POLYNOMIALS --      #
    !#                                                                          #
    !# Here the fundamental polynomials are calculated:                         #
    !#                                                                          #
    !#          n+1                                                             #
    !#         _____                                                            #
    !#          | |     x   - x(k)                                              #
    !#  p (x) = | |  --------------                                             #
    !#   j      | |    x(j) - x(k)                                              #
    !#         k = 1                                                            #
    !#         k /= j                                                           #
    !#                                                                          #
    !# and then used to compute the final interpolating polynomial,             #
    !#                                                                          #
    !#          ___ n+1                                                         #
    !#         \                                                                #
    !#  P(x) =  >     p (x) . y(j)                                              #
    !#         /___    j                                                        #
    !#              j=1                                                         #
    !#                                                                          #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Initialise interpolate result
    ! -----------------------------

    y_int = S_ZERO


    ! --------------------------
    ! Loop over polynomial order
    ! --------------------------

    l_order_loop: DO l = 1, n + 1 

      ! -- Identify the index *not* to use for current
      ! -- order (just for clarity in code )
      j = k(l)

      ! -- Calculate the fundamental polynomial
      polynomial = PRODUCT( ( x_int - x(i+k(1:n+1)) )/( x(i+j) - x(i+k(1:n+1)) ), &
                            MASK = ( k(1:n+1) /= j )                              )

      ! -- Calculate and sum the interpolating polynomial
      y_int = y_int + ( polynomial * y(i+j) )

    END DO l_order_loop



    !#--------------------------------------------------------------------------#
    !#                              -- DONE --                                  #
    !#--------------------------------------------------------------------------#

    error_status = SUCCESS

  END FUNCTION polyint_scalar_Single


  FUNCTION polyint_rank1_Single( x,            &  ! Input
                                 y,            &  ! Input
                                 x_int,        &  ! Input
                                 y_int,        &  ! Output
                                 order,        &  ! Optional input
                                 RCS_Id,       &  ! Optional output
                                 message_log ) &  ! Error messaging
                               RESULT( error_status )


    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( Single ), DIMENSION( : ), INTENT( IN )  :: y
    REAL( Single ), DIMENSION( : ), INTENT( IN )  :: x
    REAL( Single ), DIMENSION( : ), INTENT( IN )  :: x_int

    ! -- Output
    REAL( Single ), DIMENSION( : ), INTENT( OUT ) :: y_int

    ! -- Optional input
    INTEGER,        OPTIONAL,       INTENT( IN )  :: order

    ! -- Optional output
    CHARACTER( * ), OPTIONAL,       INTENT( OUT ) :: RCS_Id

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,       INTENT( IN )  :: message_log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Polynomial_Interpolate'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: message

    INTEGER :: n_points, n_min
    INTEGER :: n, l
    INTEGER :: k_begin, k_end, j
    INTEGER, DIMENSION( MAX_ORDER + 1 ) :: k
    INTEGER,        DIMENSION( SIZE( x_int ) ) :: i
    REAL( Single ), DIMENSION( SIZE( x_int ) ) :: polynomial



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------
    ! Check array sizes for consistency
    ! ---------------------------------

    n_points = SIZE( x )
    IF ( n_points /= SIZE( y ) ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Input X and Y vectors have different sizes.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF

    IF ( SIZE( x_int ) /= SIZE( y_int ) ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Input X_INT and output Y_INT vectors have different sizes.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF


    ! ------------------------------
    ! Interpolating polynomial order
    ! ------------------------------

    ! -- Assume linear interpolation if order not specified...
    n = 1

    ! -- ...otherwise check it
    IF ( PRESENT( order ) ) THEN

      ! -- Only odd, non-zero orders accepted
      IF ( MOD( order, 2 ) == 0 ) THEN
        error_status = FAILURE
        WRITE( message, '( "Input polynomial order of ", i2, &
                          &" invalid. Must be odd." )' ) order
        CALL display_message( ROUTINE_NAME,    &
                              TRIM( message ), &
                              error_status,    &
                              message_log = message_log )
        RETURN
      END IF

      ! -- Only positive, non-zero also
      IF ( order < 1 ) THEN
        error_status = FAILURE
        WRITE( message, '( "Input polynomial order of ", i2, &
                          &" invalid. Must be odd number > 0." )' ) order
        CALL display_message( ROUTINE_NAME,    &
                              TRIM( message ), &
                              error_status,    &
                              message_log = message_log )
        RETURN
      END IF

      ! -- Only reasonable values also
      IF ( order > MAX_ORDER ) THEN
        error_status = FAILURE
        WRITE( message, '( "Input polynomial order of ", i2, &
                          &" invalid. Must be odd number < or = ", i2, "." )' ) &
                        order, MAX_ORDER
        CALL display_message( ROUTINE_NAME,    &
                              TRIM( message ), &
                              error_status,    &
                              message_log = message_log )
        RETURN
      END IF

      ! -- Assign order to variable
      n = order

    END IF


    ! --------------------------------------
    ! Check that input array is large enough
    ! --------------------------------------

    IF ( n_points < ( n+1 ) ) THEN
      error_status = FAILURE
      WRITE( message, '( "Input X and Y arrays must have ", i2, &
                        &" or more points for interpolating", &
                        &" polynomial of order ", i2, "." )' ) &
                      n+1, n
      CALL display_message( ROUTINE_NAME,    &
                            TRIM( message ), &
                            error_status,    &
                            message_log = message_log )
      RETURN
    END IF


    ! -----------------------------------
    ! Set the RCS Id argument if supplied
    ! -----------------------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_Id
    END IF



    !#--------------------------------------------------------------------------#
    !#               -- CREATE LOCAL INTERPOLATION INDEX ARRAY --               #
    !#                                                                          #
    !# This index array is to reference the input abscissa points that bracket  #
    !# those at which interpolates are required. E.g. for linear interpolation  #
    !# the bracketing looks like:                                               #
    !#                                                                          #
    !#              x(i+0) < x_int < x(i+1)                                     # 
    !#                                                                          #
    !# where the index modifiers 0 and 1 are the "k" below. For cubic interp-   #
    !# olation (order = 3), the bracketing looks like:                          #
    !#                                                                          #
    !#     x(i-1) < x(i+0) < x_int < x(i+1) < x(i+2)                            # 
    !#                                                                          #
    !# where the index modifiers are now -1, 0, +1, and +2. This is the main    #
    !# reason only odd polynomial orders are accepted. Even-valued orders would #
    !# produce "lopsided" bracketing which can in turn produce interpolates     #
    !# that are quasi-discontinuou at the bracketing boundaries (by quasi-      #
    !# discontinuous I mean it looks weird :o)                                  #
    !#--------------------------------------------------------------------------#

    ! -- Set the begin and end. Here
    ! -- integer division is relied upon
    k_begin = 0 - (n/2)
    k_end   = 1 + (n/2)

    ! -- File the array
    k(1:n+1) = (/ ( l, l = k_begin, k_end ) /)



    !#--------------------------------------------------------------------------#
    !#       -- FIND THE LOCATIONS FOR WHICH INTERPOLATES ARE DESIRED --        #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------------------------
    ! Result is the lower index, i.e. x(i) < x_int < x(i+1)
    ! -----------------------------------------------------

    i = value_locate( x, x_int )


    ! --------------------------------------------------------
    ! Make indices valid for Lagrange polynomials, i.e. ensure
    ! that the sum of (i+k) does not reference non-existant
    ! indices. For those cases, the result is set to the
    ! maximum and minimum values allowed for the beginning and
    ! end of the array respectively. The result is that
    ! extrapolation may be performed.
    ! --------------------------------------------------------

    n_min = (n+1)/2

    WHERE( i <  n_min             ) i = n_min
    WHERE( i > (n_points - n_min) ) i = n_points - n_min



    !#--------------------------------------------------------------------------#
    !#        -- COMPUTE AND SUM THE LAGRANGE INTERPOLATING POLYNOMIALS --      #
    !#                                                                          #
    !# Here the fundamental polynomials are calculated:                         #
    !#                                                                          #
    !#          n+1                                                             #
    !#         _____                                                            #
    !#          | |     x   - x(k)                                              #
    !#  p (x) = | |  --------------                                             #
    !#   j      | |    x(j) - x(k)                                              #
    !#         k = 1                                                            #
    !#         k /= j                                                           #
    !#                                                                          #
    !# and then used to compute the final interpolating polynomial,             #
    !#                                                                          #
    !#          ___ n+1                                                         #
    !#         \                                                                #
    !#  P(x) =  >     p (x) . y(j)                                              #
    !#         /___    j                                                        #
    !#              j=1                                                         #
    !#                                                                          #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Initialise interpolate result
    ! -----------------------------

    y_int = S_ZERO


    ! --------------------------
    ! Loop over polynomial order
    ! --------------------------

    l_order_loop: DO l = 1, n + 1 

      ! -- Identify the index *not* to use for current
      ! -- order (just for clarity in code )
      j = k(l)

      ! -- Calculate the fundamental polynomial
      polynomial = PRODUCT( ( x_int - x(i+k(1:n+1)) )/( x(i+j) - x(i+k(1:n+1)) ), &
                            MASK = ( k(1:n+1) /= j )                              )

      ! -- Calculate and sum the interpolating polynomial
      y_int = y_int + ( polynomial * y(i+j) )

    END DO l_order_loop



    !#--------------------------------------------------------------------------#
    !#                              -- DONE --                                  #
    !#--------------------------------------------------------------------------#

    error_status = SUCCESS

  END FUNCTION polyint_rank1_Single


  FUNCTION polyint_scalar_Double( x,            &  ! Input
                                  y,            &  ! Input
                                  x_int,        &  ! Input
                                  y_int,        &  ! Output
                                  order,        &  ! Optional input
                                  RCS_Id,       &  ! Optional output
                                  message_log ) &  ! Error messaging
                                RESULT( error_status )


    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( Double ), DIMENSION( : ), INTENT( IN )  :: y
    REAL( Double ), DIMENSION( : ), INTENT( IN )  :: x
    REAL( Double ),                 INTENT( IN )  :: x_int

    ! -- Output
    REAL( Double ),                 INTENT( OUT ) :: y_int

    ! -- Optional input
    INTEGER,        OPTIONAL,       INTENT( IN )  :: order

    ! -- Optional output
    CHARACTER( * ), OPTIONAL,       INTENT( OUT ) :: RCS_Id

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,       INTENT( IN )  :: message_log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Polynomial_Interpolate'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: message

    INTEGER :: n_points, n_min
    INTEGER :: n, l
    INTEGER :: k_begin, k_end, j
    INTEGER, DIMENSION( MAX_ORDER + 1 ) :: k
    INTEGER :: i
    REAL( Double ) :: polynomial



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------
    ! Check array sizes for consistency
    ! ---------------------------------

    n_points = SIZE( x )
    IF ( n_points /= SIZE( y ) ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Input X and Y vectors have different sizes.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF


    ! ------------------------------
    ! Interpolating polynomial order
    ! ------------------------------

    ! -- Assume linear interpolation if order not specified...
    n = 1

    ! -- ...otherwise check it
    IF ( PRESENT( order ) ) THEN

      ! -- Only odd, non-zero orders accepted
      IF ( MOD( order, 2 ) == 0 ) THEN
        error_status = FAILURE
        WRITE( message, '( "Input polynomial order of ", i2, &
                          &" invalid. Must be odd." )' ) order
        CALL display_message( ROUTINE_NAME,    &
                              TRIM( message ), &
                              error_status,    &
                              message_log = message_log )
        RETURN
      END IF

      ! -- Only positive, non-zero also
      IF ( order < 1 ) THEN
        error_status = FAILURE
        WRITE( message, '( "Input polynomial order of ", i2, &
                          &" invalid. Must be odd number > 0." )' ) order
        CALL display_message( ROUTINE_NAME,    &
                              TRIM( message ), &
                              error_status,    &
                              message_log = message_log )
        RETURN
      END IF

      ! -- Only reasonable values also
      IF ( order > MAX_ORDER ) THEN
        error_status = FAILURE
        WRITE( message, '( "Input polynomial order of ", i2, &
                          &" invalid. Must be odd number < or = ", i2, "." )' ) &
                        order, MAX_ORDER
        CALL display_message( ROUTINE_NAME,    &
                              TRIM( message ), &
                              error_status,    &
                              message_log = message_log )
        RETURN
      END IF

      ! -- Assign order to variable
      n = order

    END IF


    ! --------------------------------------
    ! Check that input array is large enough
    ! --------------------------------------

    IF ( n_points < ( n+1 ) ) THEN
      error_status = FAILURE
      WRITE( message, '( "Input X and Y arrays must have ", i2, &
                        &" or more points for interpolating", &
                        &" polynomial of order ", i2, "." )' ) &
                      n+1, n
      CALL display_message( ROUTINE_NAME,    &
                            TRIM( message ), &
                            error_status,    &
                            message_log = message_log )
      RETURN
    END IF


    ! -----------------------------------
    ! Set the RCS Id argument if supplied
    ! -----------------------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_Id
    END IF



    !#--------------------------------------------------------------------------#
    !#               -- CREATE LOCAL INTERPOLATION INDEX ARRAY --               #
    !#                                                                          #
    !# This index array is to reference the input abscissa points that bracket  #
    !# those at which interpolates are required. E.g. for linear interpolation  #
    !# the bracketing looks like:                                               #
    !#                                                                          #
    !#              x(i+0) < x_int < x(i+1)                                     # 
    !#                                                                          #
    !# where the index modifiers 0 and 1 are the "k" below. For cubic interp-   #
    !# olation (order = 3), the bracketing looks like:                          #
    !#                                                                          #
    !#     x(i-1) < x(i+0) < x_int < x(i+1) < x(i+2)                            # 
    !#                                                                          #
    !# where the index modifiers are now -1, 0, +1, and +2. This is the main    #
    !# reason only odd polynomial orders are accepted. Even-valued orders would #
    !# produce "lopsided" bracketing which can in turn produce interpolates     #
    !# that are quasi-discontinuou at the bracketing boundaries (by quasi-      #
    !# discontinuous I mean it looks weird :o)                                  #
    !#--------------------------------------------------------------------------#

    ! -- Set the begin and end. Here
    ! -- integer division is relied upon
    k_begin = 0 - (n/2)
    k_end   = 1 + (n/2)

    ! -- File the array
    k(1:n+1) = (/ ( l, l = k_begin, k_end ) /)



    !#--------------------------------------------------------------------------#
    !#       -- FIND THE LOCATIONS FOR WHICH INTERPOLATES ARE DESIRED --        #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------------------------
    ! Result is the lower index, i.e. x(i) < x_int < x(i+1)
    ! -----------------------------------------------------

    i = bisection_search( x, x_int )


    ! --------------------------------------------------------
    ! Make indices valid for Lagrange polynomials, i.e. ensure
    ! that the sum of (i+k) does not reference non-existant
    ! indices. For those cases, the result is set to the
    ! maximum and minimum values allowed for the beginning and
    ! end of the array respectively. The result is that
    ! extrapolation may be performed.
    ! --------------------------------------------------------

    n_min = (n+1)/2

    IF ( i <  n_min             ) i = n_min
    IF ( i > (n_points - n_min) ) i = n_points - n_min



    !#--------------------------------------------------------------------------#
    !#        -- COMPUTE AND SUM THE LAGRANGE INTERPOLATING POLYNOMIALS --      #
    !#                                                                          #
    !# Here the fundamental polynomials are calculated:                         #
    !#                                                                          #
    !#          n+1                                                             #
    !#         _____                                                            #
    !#          | |     x   - x(k)                                              #
    !#  p (x) = | |  --------------                                             #
    !#   j      | |    x(j) - x(k)                                              #
    !#         k = 1                                                            #
    !#         k /= j                                                           #
    !#                                                                          #
    !# and then used to compute the final interpolating polynomial,             #
    !#                                                                          #
    !#          ___ n+1                                                         #
    !#         \                                                                #
    !#  P(x) =  >     p (x) . y(j)                                              #
    !#         /___    j                                                        #
    !#              j=1                                                         #
    !#                                                                          #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Initialise interpolate result
    ! -----------------------------

    y_int = D_ZERO


    ! --------------------------
    ! Loop over polynomial order
    ! --------------------------

    l_order_loop: DO l = 1, n + 1 

      ! -- Identify the index *not* to use for current
      ! -- order (just for clarity in code )
      j = k(l)

      ! -- Calculate the fundamental polynomial
      polynomial = PRODUCT( ( x_int - x(i+k(1:n+1)) )/( x(i+j) - x(i+k(1:n+1)) ), &
                            MASK = ( k(1:n+1) /= j )                              )

      ! -- Calculate and sum the interpolating polynomial
      y_int = y_int + ( polynomial * y(i+j) )

    END DO l_order_loop



    !#--------------------------------------------------------------------------#
    !#                              -- DONE --                                  #
    !#--------------------------------------------------------------------------#

    error_status = SUCCESS

  END FUNCTION polyint_scalar_Double


  FUNCTION polyint_rank1_Double( x,            &  ! Input
                                 y,            &  ! Input
                                 x_int,        &  ! Input
                                 y_int,        &  ! Output
                                 order,        &  ! Optional input
                                 RCS_Id,       &  ! Optional output
                                 message_log ) &  ! Error messaging
                               RESULT( error_status )


    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( Double ), DIMENSION( : ), INTENT( IN )  :: x
    REAL( Double ), DIMENSION( : ), INTENT( IN )  :: y
    REAL( Double ), DIMENSION( : ), INTENT( IN )  :: x_int

    ! -- Output
    REAL( Double ), DIMENSION( : ), INTENT( OUT ) :: y_int

    ! -- Optional input
    INTEGER,        OPTIONAL,       INTENT( IN )  :: order

    ! -- Optional output
    CHARACTER( * ), OPTIONAL,       INTENT( OUT ) :: RCS_Id

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,       INTENT( IN )  :: message_log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Polynomial_Interpolate'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: message

    INTEGER :: n_points, n_min
    INTEGER :: n, l, m
    INTEGER :: k_begin, k_end, j
    INTEGER, DIMENSION( MAX_ORDER + 1 ) :: k
    INTEGER,        DIMENSION( SIZE( x_int ) ) :: i
    REAL( Double ), DIMENSION( SIZE( x_int ) ) :: polynomial



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------
    ! Check array sizes for consistency
    ! ---------------------------------

    n_points = SIZE( x )
    IF ( n_points /= SIZE( y ) ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Input X and Y vectors have different sizes.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF

    IF ( SIZE( x_int ) /= SIZE( y_int ) ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Input X_INT and output Y_INT vectors have different sizes.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF


    ! ------------------------------
    ! Interpolating polynomial order
    ! ------------------------------

    ! -- Assume linear interpolation if order not specified...
    n = 1

    ! -- ...otherwise check it
    IF ( PRESENT( order ) ) THEN

      ! -- Only odd, non-zero orders accepted
      IF ( MOD( order, 2 ) == 0 ) THEN
        error_status = FAILURE
        WRITE( message, '( "Input polynomial order of ", i2, &
                          &" invalid. Must be odd." )' ) order
        CALL display_message( ROUTINE_NAME,    &
                              TRIM( message ), &
                              error_status,    &
                              message_log = message_log )
        RETURN
      END IF

      ! -- Only positive, non-zero also
      IF ( order < 1 ) THEN
        error_status = FAILURE
        WRITE( message, '( "Input polynomial order of ", i2, &
                          &" invalid. Must be odd number > 0." )' ) order
        CALL display_message( ROUTINE_NAME,    &
                              TRIM( message ), &
                              error_status,    &
                              message_log = message_log )
        RETURN
      END IF

      ! -- Only reasonable values also
      IF ( order > MAX_ORDER ) THEN
        error_status = FAILURE
        WRITE( message, '( "Input polynomial order of ", i2, &
                          &" invalid. Must be odd number < or = ", i2, "." )' ) &
                        order, MAX_ORDER
        CALL display_message( ROUTINE_NAME,    &
                              TRIM( message ), &
                              error_status,    &
                              message_log = message_log )
        RETURN
      END IF

      ! -- Assign order to variable
      n = order

    END IF


    ! --------------------------------------
    ! Check that input array is large enough
    ! --------------------------------------

    IF ( n_points < ( n+1 ) ) THEN
      error_status = FAILURE
      WRITE( message, '( "Input X and Y arrays must have ", i2, &
                        &" or more points for interpolating", &
                        &" polynomial of order ", i2, "." )' ) &
                      n+1, n
      CALL display_message( ROUTINE_NAME,    &
                            TRIM( message ), &
                            error_status,    &
                            message_log = message_log )
      RETURN
    END IF


    ! -----------------------------------
    ! Set the RCS Id argument if supplied
    ! -----------------------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_Id
    END IF



    !#--------------------------------------------------------------------------#
    !#               -- CREATE LOCAL INTERPOLATION INDEX ARRAY --               #
    !#                                                                          #
    !# This index array is to reference the input abscissa points that bracket  #
    !# those at which interpolates are required. E.g. for linear interpolation  #
    !# the bracketing looks like:                                               #
    !#                                                                          #
    !#              x(i+0) < x_int < x(i+1)                                     # 
    !#                                                                          #
    !# where the index modifiers 0 and 1 are the "k" below. For cubic interp-   #
    !# olation (order = 3), the bracketing looks like:                          #
    !#                                                                          #
    !#     x(i-1) < x(i+0) < x_int < x(i+1) < x(i+2)                            # 
    !#                                                                          #
    !# where the index modifiers are now -1, 0, +1, and +2. This is the main    #
    !# reason only odd polynomial orders are accepted. Even-valued orders would #
    !# produce "lopsided" bracketing which can in turn produce interpolates     #
    !# that are quasi-discontinuou at the bracketing boundaries (by quasi-      #
    !# discontinuous I mean it looks weird :o)                                  #
    !#--------------------------------------------------------------------------#

    ! -- Set the begin and end. Here
    ! -- integer division is relied upon
    k_begin = 0 - (n/2)
    k_end   = 1 + (n/2)

    ! -- File the array
    k(1:n+1) = (/ ( l, l = k_begin, k_end ) /)



    !#--------------------------------------------------------------------------#
    !#       -- FIND THE LOCATIONS FOR WHICH INTERPOLATES ARE DESIRED --        #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------------------------
    ! Result is the lower index, i.e. x(i) < x_int < x(i+1)
    ! -----------------------------------------------------

    i = value_locate( x, x_int )


    ! --------------------------------------------------------
    ! Make indices valid for Lagrange polynomials, i.e. ensure
    ! that the sum of (i+k) does not reference non-existant
    ! indices. For those cases, the result is set to the
    ! maximum and minimum values allowed for the beginning and
    ! end of the array respectively. The result is that
    ! extrapolation may be performed.
    ! --------------------------------------------------------

    n_min = (n+1)/2

    WHERE( i <  n_min             ) i = n_min
    WHERE( i > (n_points - n_min) ) i = n_points - n_min



    !#--------------------------------------------------------------------------#
    !#        -- COMPUTE AND SUM THE LAGRANGE INTERPOLATING POLYNOMIALS --      #
    !#                                                                          #
    !# Here the fundamental polynomials are calculated:                         #
    !#                                                                          #
    !#          n+1                                                             #
    !#         _____                                                            #
    !#          | |     x   - x(k)                                              #
    !#  p (x) = | |  --------------                                             #
    !#   j      | |    x(j) - x(k)                                              #
    !#         k = 1                                                            #
    !#         k /= j                                                           #
    !#                                                                          #
    !# and then used to compute the final interpolating polynomial,             #
    !#                                                                          #
    !#          ___ n+1                                                         #
    !#         \                                                                #
    !#  P(x) =  >     p (x) . y(j)                                              #
    !#         /___    j                                                        #
    !#              j=1                                                         #
    !#                                                                          #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Initialise interpolate result
    ! -----------------------------

    y_int = D_ZERO


    ! --------------------------
    ! Loop over polynomial order
    ! --------------------------

    l_order_loop: DO l = 1, n + 1 

      ! -- Identify the index *not* to use for current
      ! -- order (just for clarity in code )
      j = k(l)

      ! -- Calculate the fundamental polynomial
      polynomial = D_ONE
      m_product_loop: DO m = 1, n + 1
        IF ( k(m) == j ) CYCLE m_product_loop
       
        polynomial = polynomial * ( ( x_int - x(i+k(m)) )/( x(i+j) - x(i+k(m)) ) )

      END DO m_product_loop

      ! -- Calculate and sum the interpolating polynomial
      y_int = y_int + ( polynomial * y(i+j) )

    END DO l_order_loop



    !#--------------------------------------------------------------------------#
    !#                              -- DONE --                                  #
    !#--------------------------------------------------------------------------#

    error_status = SUCCESS

  END FUNCTION polyint_rank1_Double








  FUNCTION splinit_single( x,            &  ! Input
                           y,            &  ! Input
                           y2,           &  ! Output
                           RCS_Id,       &  ! Optional output
                           message_log ) &  ! Error messaging
                         RESULT ( error_status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( Single ), DIMENSION( : ), INTENT( IN )  :: x
    REAL( Single ), DIMENSION( : ), INTENT( IN )  :: y

    ! -- Output
    REAL( Single ), DIMENSION( : ), INTENT( OUT ) :: y2
 
    ! -- Optional output
    CHARACTER( * ), OPTIONAL,       INTENT( OUT ) :: RCS_Id

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,       INTENT( IN )  :: message_log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Spline_Initialize'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: message

    INTEGER :: i
    INTEGER :: n

    REAL( Single ) :: p
    REAL( Single ) :: qn
    REAL( Single ) :: sig
    REAL( Single ) :: un
    REAL( Single ) :: numer1
    REAL( Single ) :: numer2
    REAL( Single ) :: numerator
    REAL( Single ) :: denominator
    REAL( Single ) :: offset

    REAL( Single ), DIMENSION( SIZE( x ) ) :: u

 

    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! ----------------------------
    ! Are vector sizes consistent?
    ! ----------------------------

    n = SIZE( x )

    IF ( SIZE( y ) /= n ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Input X, Y arguments are different sizes.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF

    IF ( SIZE( y2 ) /= n ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Output Y2 argument has inconsistent size.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF


    ! -----------------------------
    ! Is X-data in ascending order?
    ! -----------------------------

    ! -- Calculate delta-x values
    u( 1:n-1 ) = x( 2:n ) - x( 1:n-1 )

    ! -- Check for any negative or zero differences
    IF ( ANY( u(1:n-1) < S_TOLERANCE ) ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Input X data must be in ascending order', &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF


    ! -----------------------------------
    ! Set the RCS Id argument if supplied
    ! -----------------------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_Id
    END IF



    !#--------------------------------------------------------------------------#
    !#              -- SET SECOND DERIVATIVE AT LOWER BOUNDARY  --              #
    !#--------------------------------------------------------------------------#

    y2(1) = S_ZERO
    u(1)  = S_ZERO



    !#--------------------------------------------------------------------------#
    !#        -- BEGIN DECOMPOSITION LOOP OF TRIDIAGONAL ALGORITHM --           #
    !#--------------------------------------------------------------------------#

    i_decomposition_loop : DO i = 2, n - 1

      sig     = ( x(i) - x(i-1) ) / ( x(i+1) - x(i-1) )
      p       = ( sig * y2( i-1 ) ) + S_TWO
      y2( i ) = ( sig - S_ONE ) / p

      numer1    = ( y(i+1) - y(i) ) / ( x(i+1) - x(i) )
      numer2    = ( y(i) - y(i-1) ) / ( x(i) - x(i-1) )
      numerator   = S_SIX * ( numer1 - numer2 )
      denominator = x(i+1) - x(i-1)
      offset      = sig * u(i-1)

      u( i ) = ( ( numerator / denominator ) - offset ) / p

    END DO i_decomposition_loop



    !#--------------------------------------------------------------------------#
    !#              -- SET SECOND DERIVATIVE AT UPPER BOUNDARY --               #
    !#--------------------------------------------------------------------------#

    qn = S_ZERO
    un = S_ZERO

    y2( n ) = ( un - ( qn * u(n-1) ) ) / ( ( qn * y2(n-1) ) + S_ONE )



    !#--------------------------------------------------------------------------#
    !#           -- BACKSUBSTITUTION LOOP OF TRIDIAGONAL ALGORITHM --           #
    !#--------------------------------------------------------------------------#

    i_backsubstitution_loop : DO i = n - 1, 1, -1

      y2( i ) = ( y2(i) * y2(i+1) ) + u(i)

    END DO i_backsubstitution_loop

 

    !#--------------------------------------------------------------------------#
    !#                                -- DONE --                                #
    !#--------------------------------------------------------------------------#

    error_status = SUCCESS

  END FUNCTION splinit_single


  FUNCTION splinit_double( x,            &  ! Input
                           y,            &  ! Input
                           y2,           &  ! Output
                           RCS_Id,       &  ! Optional output
                           message_log ) &  ! Error messaging
                         RESULT ( error_status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( Double ), DIMENSION( : ), INTENT( IN )  :: x
    REAL( Double ), DIMENSION( : ), INTENT( IN )  :: y

    ! -- Output
    REAL( Double ), DIMENSION( : ), INTENT( OUT ) :: y2
 
    ! -- Optional output
    CHARACTER( * ), OPTIONAL,       INTENT( OUT ) :: RCS_Id

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,       INTENT( IN )  :: message_log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Spline_Initialize'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: message

    INTEGER :: i
    INTEGER :: n

    REAL( Double ) :: p
    REAL( Double ) :: qn
    REAL( Double ) :: sig
    REAL( Double ) :: un
    REAL( Double ) :: numer1
    REAL( Double ) :: numer2
    REAL( Double ) :: numerator
    REAL( Double ) :: denominator
    REAL( Double ) :: offset

    REAL( Double ), DIMENSION( SIZE( x ) ) :: u

 

    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! ----------------------------
    ! Are vector sizes consistent?
    ! ----------------------------

    n = SIZE( x )

    IF ( SIZE( y ) /= n ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Input X, Y arguments are different sizes.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF

    IF ( SIZE( y2 ) /= n ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Output Y2 argument has inconsistent size.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF


    ! -----------------------------
    ! Is X-data in ascending order?
    ! -----------------------------

    ! -- Calculate delta-x values
    u( 1:n-1 ) = x( 2:n ) - x( 1:n-1 )

    ! -- Check for any negative or zero differences
    IF ( ANY( u( 1:n-1 ) < D_TOLERANCE ) ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Input X data must be in ascending order', &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF


    ! -----------------------------------
    ! Set the RCS Id argument if supplied
    ! -----------------------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_Id
    END IF



    !#--------------------------------------------------------------------------#
    !#              -- SET SECOND DERIVATIVE AT LOWER BOUNDARY  --              #
    !#--------------------------------------------------------------------------#

    y2(1) = D_ZERO
    u(1)  = D_ZERO



    !#--------------------------------------------------------------------------#
    !#        -- BEGIN DECOMPOSITION LOOP OF TRIDIAGONAL ALGORITHM --           #
    !#--------------------------------------------------------------------------#

    i_decomposition_loop : DO i = 2, n - 1

      sig     = ( x(i) - x(i-1) ) / ( x(i+1) - x(i-1) )
      p       = ( sig * y2( i-1 ) ) + D_TWO
      y2( i ) = ( sig - D_ONE ) / p

      numer1    = ( y(i+1) - y(i) ) / ( x(i+1) - x(i) )
      numer2    = ( y(i) - y(i-1) ) / ( x(i) - x(i-1) )
      numerator   = D_SIX * ( numer1 - numer2 )
      denominator = x(i+1) - x(i-1)
      offset      = sig * u(i-1)

      u( i ) = ( ( numerator / denominator ) - offset ) / p

    END DO i_decomposition_loop



    !#--------------------------------------------------------------------------#
    !#              -- SET SECOND DERIVATIVE AT UPPER BOUNDARY --               #
    !#--------------------------------------------------------------------------#

    qn = D_ZERO
    un = D_ZERO

    y2( n ) = ( un - ( qn * u(n-1) ) ) / ( ( qn * y2(n-1) ) + D_ONE )



    !#--------------------------------------------------------------------------#
    !#           -- BACKSUBSTITUTION LOOP OF TRIDIAGONAL ALGORITHM --           #
    !#--------------------------------------------------------------------------#

    i_backsubstitution_loop : DO i = n - 1, 1, -1

      y2( i ) = ( y2(i) * y2(i+1) ) + u(i)

    END DO i_backsubstitution_loop

 

    !#--------------------------------------------------------------------------#
    !#                                -- DONE --                                #
    !#--------------------------------------------------------------------------#

    error_status = SUCCESS

  END FUNCTION splinit_double







  FUNCTION splint_scalar_single( x,            &  ! Input
                                 y,            &  ! Input
                                 x_int,        &  ! Input
                                 y_int,        &  ! Output
                                 y2,           &  ! Optional input
                                 RCS_Id,       &  ! Optional output
                                 message_log ) &  ! Error messaging
                               RESULT ( error_status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( Single ),           DIMENSION( : ), INTENT( IN )  :: x
    REAL( Single ),           DIMENSION( : ), INTENT( IN )  :: y
    REAL( Single ),                           INTENT( IN )  :: x_int

    ! -- Output
    REAL( Single ),                           INTENT( OUT ) :: y_int

    ! -- Optional input
    REAL( Single ), OPTIONAL, DIMENSION( : ), INTENT( IN )  :: y2

    ! -- Optional output
    CHARACTER( * ), OPTIONAL,                 INTENT( OUT ) :: RCS_Id

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,                 INTENT( IN )  :: message_log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: error_status

 
    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Spline_Interpolate'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: message

    INTEGER :: n
    INTEGER :: i, i_lo, i_hi

    REAL( Single )  :: a, b, dx

    REAL( Single ), DIMENSION( SIZE( x ) ) :: y2_derivative
 


    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! ----------------------------
    ! Are vector sizes consistent?
    ! ----------------------------

    n = SIZE( x )

    IF ( SIZE( y ) /= n ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Input X, Y arguments are different sizes.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF


    ! -----------------------------------
    ! Set the RCS Id argument if supplied
    ! -----------------------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_Id
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- CHECK IF Y2 PASSED --                         #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( y2 ) ) THEN

      ! -- Yes. Check size
      IF ( SIZE( y2 ) /= n ) THEN
        error_status = FAILURE
        CALL display_message( ROUTINE_NAME, &
                              'Input Y2 argument has inconsistent size.', &
                              error_status, &
                              message_log = message_log )
        RETURN
      END IF

      y2_derivative = y2

    ELSE

      ! -- No. Calculate it. This is inefficient so should only be done
      ! --     if X and Y are different each call.
      error_status = spline_initialize( x, y, y2_derivative,    &
                                        message_log = message_log )

      IF ( error_status /= SUCCESS ) THEN
        CALL display_message( ROUTINE_NAME, &
                              'Error occurred in spline initialisation function.', &
                              error_status, &
                              message_log = message_log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#      -- FIND THE X-VALUES IN X THAT BRACKET THE INPUT X_INT VALUE --     #
    !#      -- BY MEANS OF BISECTION.  THIS IS OPTIMAL IF SEQUENTIAL     --     #
    !#      -- CALLS TO THIS FUNCTION ARE AT RANDOM VALUES OF X_INT.     --     #
    !#--------------------------------------------------------------------------#

    i_lo = MAX( 1, bisection_search( x, x_int ) )
    i_hi = MIN( i_lo + 1, n )



    !#--------------------------------------------------------------------------#
    !#                    -- CALCULATE INTERPOLATED VALUE --                    #
    !#--------------------------------------------------------------------------#

    ! ---------------------
    ! Calculate dx and test
    ! ---------------------

    dx = x( i_hi ) - x( i_lo )

    IF ( dx < S_TOLERANCE ) THEN
      error_status = FAILURE
      WRITE( message, '( "Error : dx < or = 0. i_lo, i_hi : ", i6, 2x, i6, &
                        &" x(i_lo), x(i_hi) : ", f12.6, 2x, f12.6 )' ) &
                      i_lo, i_hi, x(i_lo), x(i_hi)
      CALL display_message( ROUTINE_NAME, &
                            TRIM( message ), &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF


    ! ---------------------
    ! Evaluate cubic spline
    ! ---------------------

    a = ( x(i_hi) - x_int ) / dx
    b = ( x_int - x(i_lo) ) / dx

    y_int = ( a * y(i_lo) ) + ( b * y(i_hi) ) + &
            ( ( ( (a**3) - a ) * y2_derivative(i_lo) ) + &
              ( ( (b**3) - b ) * y2_derivative(i_hi) )   ) * &
            (dx*dx) / S_SIX



    !#--------------------------------------------------------------------------#
    !#                                 -- DONE --                               # 
    !#--------------------------------------------------------------------------#

    error_status = SUCCESS

  END FUNCTION splint_scalar_single


  FUNCTION splint_scalar_double( x,            &  ! Input
                                 y,            &  ! Input
                                 x_int,        &  ! Input
                                 y_int,        &  ! Output
                                 y2,           &  ! Optional input
                                 RCS_Id,       &  ! Optional output
                                 message_log ) &  ! Error messaging
                               RESULT ( error_status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( Double ),           DIMENSION( : ), INTENT( IN )  :: x
    REAL( Double ),           DIMENSION( : ), INTENT( IN )  :: y
    REAL( Double ),                           INTENT( IN )  :: x_int

    ! -- Output
    REAL( Double ),                           INTENT( OUT ) :: y_int

    ! -- Optional input
    REAL( Double ), OPTIONAL, DIMENSION( : ), INTENT( IN )  :: y2

    ! -- Optional output
    CHARACTER( * ), OPTIONAL,                 INTENT( OUT ) :: RCS_Id

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,                 INTENT( IN )  :: message_log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: error_status

 
    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Spline_Interpolate'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: message

    INTEGER :: n
    INTEGER :: i, i_lo, i_hi

    REAL( Double )  :: a, b, dx

    REAL( Double ), DIMENSION( SIZE( x ) ) :: y2_derivative
 


    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! ----------------------------
    ! Are vector sizes consistent?
    ! ----------------------------

    n = SIZE( x )

    IF ( SIZE( y ) /= n ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Input X, Y arguments are different sizes.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF


    ! -----------------------------------
    ! Set the RCS Id argument if supplied
    ! -----------------------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_Id
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- CHECK IF Y2 PASSED --                        #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( y2 ) ) THEN

      ! -- Yes. Check size
      IF ( SIZE( y2 ) /= n ) THEN
        error_status = FAILURE
        CALL display_message( ROUTINE_NAME, &
                              'Input Y2 argument has inconsistent size.', &
                              error_status, &
                              message_log = message_log )
        RETURN
      END IF

      y2_derivative = y2

    ELSE

      ! -- No. Calculate it. This is inefficient so should only be done
      ! --     if X and Y are different each call.
      error_status = spline_initialize( x, y, y2_derivative,    &
                                        message_log = message_log )

      IF ( error_status /= SUCCESS ) THEN
        CALL display_message( ROUTINE_NAME, &
                              'Error occurred in spline initialisation function.', &
                              error_status, &
                              message_log = message_log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#      -- FIND THE X-VALUES IN X THAT BRACKET THE INPUT X_INT VALUE --     #
    !#      -- BY MEANS OF BISECTION.  THIS IS OPTIMAL IF SEQUENTIAL     --     #
    !#      -- CALLS TO THIS FUNCTION ARE AT RANDOM VALUES OF X_INT.     --     #
    !#--------------------------------------------------------------------------#

    i_lo = MAX( 1, bisection_search( x, x_int ) )
    i_hi = MIN( i_lo + 1, n )



    !#--------------------------------------------------------------------------#
    !#                    -- CALCULATE INTERPOLATED VALUE --                    #
    !#--------------------------------------------------------------------------#

    ! ---------------------
    ! Calculate dx and test
    ! ---------------------

    dx = x( i_hi ) - x( i_lo )

    IF ( dx < D_TOLERANCE ) THEN
      error_status = FAILURE
      WRITE( message, '( "Error : dx < or = 0. i_lo, i_hi : ", i6, 2x, i6, &
                        &" x(i_lo), x(i_hi) : ", f12.6, 2x, f12.6 )' ) &
                      i_lo, i_hi, x(i_lo), x(i_hi)
      CALL display_message( ROUTINE_NAME, &
                            TRIM( message ), &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF


    ! ---------------------
    ! Evaluate cubic spline
    ! ---------------------

    a = ( x(i_hi) - x_int ) / dx
    b = ( x_int - x(i_lo) ) / dx

    y_int = ( a * y(i_lo) ) + ( b * y(i_hi) ) + &
            ( ( ( (a**3) - a ) * y2_derivative(i_lo) ) + &
              ( ( (b**3) - b ) * y2_derivative(i_hi) )   ) * &
            (dx*dx) / D_SIX



    !#--------------------------------------------------------------------------#
    !#                                 -- DONE --                               # 
    !#--------------------------------------------------------------------------#

    error_status = SUCCESS

  END FUNCTION splint_scalar_double




  FUNCTION splint_rank1_single( x,            &  ! Input
                                y,            &  ! Input
                                x_int,        &  ! Input
                                y_int,        &  ! Output
                                y2,           &  ! Optional input
                                RCS_Id,       &  ! Optional output
                                message_log ) &  ! Error messaging
                              RESULT ( error_status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( Single ),           DIMENSION( : ), INTENT( IN )  :: x  ! N
    REAL( Single ),           DIMENSION( : ), INTENT( IN )  :: y  ! N
    REAL( Single ),           DIMENSION( : ), INTENT( IN )  :: x_int   ! N_int

    ! -- Output
    REAL( Single ),           DIMENSION( : ), INTENT( OUT ) :: y_int   ! N_int

    ! -- Optional input
    REAL( Single ), OPTIONAL, DIMENSION( : ), INTENT( IN )  :: y2 ! N

    ! -- Optional output
    CHARACTER( * ), OPTIONAL,                 INTENT( OUT ) :: RCS_Id

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,                 INTENT( IN )  :: message_log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: error_status

 
    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Spline_Interpolate'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: message

    INTEGER :: n, n_int
    INTEGER :: i

    REAL( Single )  :: a, b, dx

    INTEGER,        DIMENSION( SIZE( x_int )  ) :: i_lo, i_hi

    REAL( Single ), DIMENSION( SIZE( x ) ) :: y2_derivative
 


    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! ----------------------------------
    ! Are input vector sizes consistent?
    ! ----------------------------------

    n = SIZE( x )

    IF ( SIZE( y ) /= n ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Input X, Y arguments are different sizes.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF


    ! ----------------------------------------
    ! Are interpolate vector sizes consistent?
    ! ----------------------------------------

    n_int = SIZE( x_int )

    IF ( SIZE( y_int ) /= n_int ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Interpolate X_INT, Y_INT arguments are different sizes.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF


    ! -----------------------------------
    ! Set the RCS Id argument if supplied
    ! -----------------------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_Id
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- CHECK IF Y2 PASSED --                         #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( y2 ) ) THEN

      ! -- Yes. Check size
      IF ( SIZE( y2 ) /= n ) THEN
        error_status = FAILURE
        CALL display_message( ROUTINE_NAME, &
                              'Input Y2 argument has inconsistent size.', &
                              error_status, &
                              message_log = message_log )
        RETURN
      END IF

      y2_derivative = y2

    ELSE

      ! -- No. Calculate it.
      error_status = spline_initialize( x, y, y2_derivative,    &
                                        message_log = message_log )

      IF ( error_status /= SUCCESS ) THEN
        CALL display_message( ROUTINE_NAME, &
                              'Error occurred in spline initialisation function.', &
                              error_status, &
                              message_log = message_log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#      -- FIND THE X-VALUES IN X THAT BRACKET THE INPUT X_INT VALUE --     #
    !#      -- BY MEANS OF BISECTION.  THIS IS OPTIMAL IF SEQUENTIAL     --     #
    !#      -- CALLS TO THIS FUNCTION ARE AT RANDOM VALUES OF X_INT.     --     #
    !#--------------------------------------------------------------------------#

    i_lo = value_locate( x, x_int )
    i_hi = i_lo + 1



    !#--------------------------------------------------------------------------#
    !#                    -- CALCULATE INTERPOLATED VALUE --                    #
    !#--------------------------------------------------------------------------#

    DO i = 1, n_int

    
      ! ---------------------
      ! Calculate dx and test
      ! ---------------------

      dx = x( i_hi( i ) ) - x( i_lo( i ) )

      IF ( dx < S_TOLERANCE ) THEN
        error_status = FAILURE
        WRITE( message, '( "Error : dx < or = 0. i_lo, i_hi : ", i6, 2x, i6, &
                          &" x(i_lo), x(i_hi) : ", f12.6, 2x, f12.6 )' ) &
                        i_lo(i), i_hi(i), x(i_lo(i)), x(i_hi(i))
        CALL display_message( ROUTINE_NAME, &
                              TRIM( message ), &
                              error_status, &
                              message_log = message_log )
        RETURN
      END IF


      ! ---------------------
      ! Evaluate cubic spline
      ! ---------------------

      a = ( x(i_hi(i)) - x_int(i) ) / dx
      b = ( x_int(i) - x(i_lo(i)) ) / dx

      y_int(i) = ( a * y(i_lo(i)) ) + ( b * y(i_hi(i)) ) + &
                 ( ( ( (a**3) - a ) * y2_derivative(i_lo(i)) ) + &
                   ( ( (b**3) - b ) * y2_derivative(i_hi(i)) )   ) * &
                 (dx*dx) / S_SIX

    END DO


    !#--------------------------------------------------------------------------#
    !#                                 -- DONE --                               # 
    !#--------------------------------------------------------------------------#

    error_status = SUCCESS

  END FUNCTION splint_rank1_single



  FUNCTION splint_rank1_double( x,            &  ! Input
                                y,            &  ! Input
                                x_int,        &  ! Input
                                y_int,        &  ! Output
                                y2,           &  ! Optional input
                                RCS_Id,       &  ! Optional output
                                message_log ) &  ! Error messaging
                              RESULT ( error_status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( Double ),           DIMENSION( : ), INTENT( IN )  :: x  ! N
    REAL( Double ),           DIMENSION( : ), INTENT( IN )  :: y  ! N
    REAL( Double ),           DIMENSION( : ), INTENT( IN )  :: x_int   ! N_int

    ! -- Output
    REAL( Double ),           DIMENSION( : ), INTENT( OUT ) :: y_int   ! N_int

    ! -- Optional input
    REAL( Double ), OPTIONAL, DIMENSION( : ), INTENT( IN )  :: y2 ! N

    ! -- Optional output
    CHARACTER( * ), OPTIONAL,                 INTENT( OUT ) :: RCS_Id

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,                 INTENT( IN )  :: message_log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: error_status

 
    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Spline_Interpolate'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: message

    INTEGER :: n, n_int
    INTEGER :: i

    REAL( Double )  :: a, b, dx

    INTEGER,        DIMENSION( SIZE( x_int )  ) :: i_lo, i_hi

    REAL( Double ), DIMENSION( SIZE( x ) ) :: y2_derivative
 


    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! ----------------------------------
    ! Are input vector sizes consistent?
    ! ----------------------------------

    n = SIZE( x )

    IF ( SIZE( y ) /= n ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Input X, Y arguments are different sizes.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF


    ! ----------------------------------------
    ! Are interpolate vector sizes consistent?
    ! ----------------------------------------

    n_int = SIZE( x_int )

    IF ( SIZE( y_int ) /= n_int ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Interpolate X_INT, Y_INT arguments are different sizes.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF


    ! -----------------------------------
    ! Set the RCS Id argument if supplied
    ! -----------------------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_Id
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- CHECK IF Y2 PASSED --                         #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( y2 ) ) THEN

      ! -- Yes. Check size
      IF ( SIZE( y2 ) /= n ) THEN
        error_status = FAILURE
        CALL display_message( ROUTINE_NAME, &
                              'Input Y2 argument has inconsistent size.', &
                              error_status, &
                              message_log = message_log )
        RETURN
      END IF

      y2_derivative = y2

    ELSE

      ! -- No. Calculate it.
      error_status = spline_initialize( x, y, y2_derivative,    &
                                        message_log = message_log )

      IF ( error_status /= SUCCESS ) THEN
        CALL display_message( ROUTINE_NAME, &
                              'Error occurred in spline initialisation function.', &
                              error_status, &
                              message_log = message_log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#      -- FIND THE X-VALUES IN X THAT BRACKET THE INPUT X_INT VALUE --     #
    !#      -- BY MEANS OF BISECTION.  THIS IS OPTIMAL IF SEQUENTIAL     --     #
    !#      -- CALLS TO THIS FUNCTION ARE AT RANDOM VALUES OF X_INT.     --     #
    !#--------------------------------------------------------------------------#

    i_lo = value_locate( x, x_int )
    i_hi = i_lo + 1



    !#--------------------------------------------------------------------------#
    !#                    -- CALCULATE INTERPOLATED VALUE --                    #
    !#--------------------------------------------------------------------------#

    DO i = 1, n_int

    
      ! ---------------------
      ! Calculate dx_int and test
      ! ---------------------

      dx = x( i_hi( i ) ) - x( i_lo( i ) )

      IF ( dx < D_TOLERANCE ) THEN
        error_status = FAILURE
        WRITE( message, '( "Error : dx < or = 0. i_lo, i_hi : ", i6, 2x, i6, &
                          &" x(i_lo), x(i_hi) : ", f12.6, 2x, f12.6 )' ) &
                        i_lo(i), i_hi(i), x(i_lo(i)), x(i_hi(i))
        CALL display_message( ROUTINE_NAME, &
                              TRIM( message ), &
                              error_status, &
                              message_log = message_log )
        RETURN
      END IF


      ! ---------------------
      ! Evaluate cubic spline
      ! ---------------------

      a = ( x(i_hi(i)) - x_int(i) ) / dx
      b = ( x_int(i) - x(i_lo(i)) ) / dx

      y_int(i) = ( a * y(i_lo(i)) ) + ( b * y(i_hi(i)) ) + &
                 ( ( ( (a**3) - a ) * y2_derivative(i_lo(i)) ) + &
                   ( ( (b**3) - b ) * y2_derivative(i_hi(i)) )   ) * &
                 (dx*dx) / D_SIX

    END DO


    !#--------------------------------------------------------------------------#
    !#                                 -- DONE --                               # 
    !#--------------------------------------------------------------------------#

    error_status = SUCCESS

  END FUNCTION splint_rank1_double




  !##############################################################################
  !##############################################################################
  !##############################################################################
  !##############################################################################

  !#                               PRIVATE routines                             #

  !##############################################################################
  !##############################################################################
  !##############################################################################
  !##############################################################################

  FUNCTION locate_single( x, u )  &
                        RESULT( j )

    REAL( Single ), DIMENSION( : ), INTENT( IN ) :: x
    REAL( Single ), DIMENSION( : ), INTENT( IN ) :: u

    INTEGER, DIMENSION( SIZE( u ) ) :: j

    INTEGER :: n_x
    INTEGER :: n_u, i_u
    INTEGER :: x_lower, x_upper
    LOGICAL :: ascending

    n_x = SIZE( x )
    n_u = SIZE( u )


    ! ---------------------------------------------------------------
    ! Determine if arrays are sorted in ascending or descending order
    ! ---------------------------------------------------------------

    IF ( x( n_x ) > x( 1 ) .AND. u( n_u ) > u( 1 ) ) THEN
      ascending = .TRUE.
    ELSEIF ( x( n_x ) < x( 1 ) .AND. u( n_u ) < u( 1 ) ) THEN
      ascending = .FALSE.
    ELSE
      j(:) = -1
      RETURN
    END IF


    ! --------------------------------------------------
    ! Perform the bisection search for each element of u
    ! --------------------------------------------------

    IF ( ascending ) THEN

      ! -- Going up
      x_lower = 1

      ascending_loop: DO i_u = 1, n_u
        j( i_u ) = bisection_search( x, u( i_u ), x_lower = x_lower )
        x_lower = MAX( 1, j( i_u ) )
      END DO ascending_loop

    ELSE

      ! -- Going down
      x_upper = n_x

      descending_loop: DO i_u = 1, n_u
        j( i_u ) = bisection_search( x, u( i_u ), x_upper = x_upper )
        x_lower = MIN( j( i_u ), n_x )
      END DO descending_loop

    END IF

  END FUNCTION locate_single



  FUNCTION locate_double( x, u )  &
                        RESULT( j )

    REAL( Double ), DIMENSION( : ), INTENT( IN ) :: x
    REAL( Double ), DIMENSION( : ), INTENT( IN ) :: u

    INTEGER, DIMENSION( SIZE( u ) ) :: j

    INTEGER :: n_x
    INTEGER :: n_u, i_u
    INTEGER :: x_lower, x_upper
    LOGICAL :: ascending

    n_x = SIZE( x )
    n_u = SIZE( u )


    ! ---------------------------------------------------------------
    ! Determine if arrays are sorted in ascending or descending order
    ! ---------------------------------------------------------------

    IF ( x( n_x ) > x( 1 ) .AND. u( n_u ) > u( 1 ) ) THEN
      ascending = .TRUE.
    ELSEIF ( x( n_x ) < x( 1 ) .AND. u( n_u ) < u( 1 ) ) THEN
      ascending = .FALSE.
    ELSE
      j(:) = -1
      RETURN
    END IF


    ! --------------------------------------------------
    ! Perform the bisection search for each element of u
    ! --------------------------------------------------

    IF ( ascending ) THEN

      ! -- Going up
      x_lower = 1

      ascending_loop: DO i_u = 1, n_u
        j( i_u ) = bisection_search( x, u( i_u ), x_lower = x_lower )
        x_lower = MAX( 1, j( i_u ) )
      END DO ascending_loop

    ELSE

      ! -- Going down
      x_upper = n_x

      descending_loop: DO i_u = 1, n_u
        j( i_u ) = bisection_search( x, u( i_u ), x_upper = x_upper )
        x_lower = MIN( j( i_u ), n_x )
      END DO descending_loop

    END IF

  END FUNCTION locate_double



  FUNCTION bisearch_single( x, u,      &
                            x_lower,   &
                            x_upper  ) &
                          RESULT( j )

    REAL( Single ), DIMENSION( : ), INTENT( IN )           :: x
    REAL( Single ),                 INTENT( IN )           :: u
    INTEGER,                        INTENT( IN ), OPTIONAL :: x_lower
    INTEGER,                        INTENT( IN ), OPTIONAL :: x_upper

    INTEGER :: j

    INTEGER :: n
    INTEGER :: j_lower
    INTEGER :: j_middle
    INTEGER :: j_upper


    n = SIZE( x )


    ! ------------------------------------
    ! Initialise upper and lower limits to
    ! the valid maximums, 1 and n
    ! ------------------------------------

    IF ( PRESENT( x_lower ) ) THEN
      j_lower = x_lower
    ELSE
      j_lower = 1
    END IF

    IF ( PRESENT( x_upper ) ) THEN
      j_upper = x_upper
    ELSE
      j_upper = n
    END IF



    ! ------------------------------------------
    ! Search for the required index by bisection
    ! ------------------------------------------

    bisection_search_loop: DO


      ! ----------------------------------------------
      ! If the index ranges have converged, we're done
      ! ----------------------------------------------

      IF ( ( j_upper - j_lower ) <= 1 ) EXIT bisection_search_loop


      ! ---------------------
      ! Define a middle point
      ! ---------------------

      j_middle = ( j_lower + j_upper ) / 2


      ! --------------------------------------------------------
      ! Which half is the required value in?
      !
      ! .EQV. is logical equivalence, i.e. the result is TRUE if
      ! both expressions are TRUE or both expressions are FALSE
      ! --------------------------------------------------------

      IF ( ( x(n) > x(1) ) .EQV. ( u > x( j_middle ) ) ) THEN

        ! -- The "upper" half
        j_lower = j_middle

      ELSE

        ! -- The "lower" half
        j_upper = j_middle

      END IF

    END DO bisection_search_loop


    ! -----------------------
    ! Define the return value
    ! -----------------------

    j = j_lower

  END FUNCTION bisearch_single


  FUNCTION bisearch_double( x, u,      &
                            x_lower,   &
                            x_upper  ) &
                          RESULT( j )

    REAL( Double ), DIMENSION( : ), INTENT( IN )           :: x
    REAL( Double ),                 INTENT( IN )           :: u
    INTEGER,                        INTENT( IN ), OPTIONAL :: x_lower
    INTEGER,                        INTENT( IN ), OPTIONAL :: x_upper

    INTEGER :: j

    INTEGER :: n
    INTEGER :: j_lower
    INTEGER :: j_middle
    INTEGER :: j_upper


    n = SIZE( x )


    ! ------------------------------------
    ! Initialise upper and lower limits to
    ! the valid maximums, 1 and n
    ! ------------------------------------

    IF ( PRESENT( x_lower ) ) THEN
      j_lower = x_lower
    ELSE
      j_lower = 1
    END IF

    IF ( PRESENT( x_upper ) ) THEN
      j_upper = x_upper
    ELSE
      j_upper = n
    END IF



    ! ------------------------------------------
    ! Search for the required index by bisection
    ! ------------------------------------------

    bisection_search_loop: DO


      ! ----------------------------------------------
      ! If the index ranges have converged, we're done
      ! ----------------------------------------------

      IF ( ( j_upper - j_lower ) <= 1 ) EXIT bisection_search_loop


      ! ---------------------
      ! Define a middle point
      ! ---------------------

      j_middle = ( j_lower + j_upper ) / 2


      ! --------------------------------------------------------
      ! Which half is the required value in?
      !
      ! .EQV. is logical equivalence, i.e. the result is TRUE if
      ! both expressions are TRUE or both expressions are FALSE
      ! --------------------------------------------------------

      IF ( ( x(n) > x(1) ) .EQV. ( u > x( j_middle ) ) ) THEN

        ! -- The "upper" half
        j_lower = j_middle

      ELSE

        ! -- The "lower" half
        j_upper = j_middle

      END IF

    END DO bisection_search_loop


    ! -----------------------
    ! Define the return value
    ! -----------------------

    j = j_lower

  END FUNCTION bisearch_double

END MODULE interpolate


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2002/10/08 14:48:26 $
!
! $Revision$
!
! $Name:  $
!
! $State: Exp $
!
! $Log: interpolate.f90,v $
! Revision 3.4  2002/10/08 14:48:26  paulv
! - Synchronising repository.
!
! Revision 3.3  2002/08/22 19:43:59  paulv
! - Updated documentation.
!
! Revision 3.2  2002/07/12 19:05:13  paulv
! - Added RCS_Id optional output arguments to public functions.
! - Updated header documentation.
! - Removed module variable MESSAGE and moved it into each function that
!   required it.
!
! Revision 3.1  2002/03/24 15:16:11  paulv
! - Replaced individual linear, quadratic, and cubic interpolation functions
!   with the generic POLYNOMIAL_INTERPOLATE() function that allows the
!   order of the interpolating polynomial to be selected by the user. Only
!   in the early stages of debugging - scalar versions appear to work but only
!   the double precision version of the rank-1 functions works correctly.
!
! Revision 2.2  2001/10/17 17:40:22  paulv
! - Added cubic (4-pt Lagrangian) interpolation functions.
!
! Revision 2.1  2001/10/10 12:07:10  paulv
! - Added SPLINE_INITIALIZE and SPLINE_INTERPOLATE functions.
! - Overloaded all public functions to allow both single and double
!   precision input and also scalar and vector interpolates.
! - Updated documentation.
!
! Revision 1.2  2000/11/28 13:44:07  paulv
! - Added quadratic interpolation routines for single- and double-precision
!   input vectors.
!
! Revision 1.1  2000/11/27 18:07:31  paulv
! Initial checkin.
!
!
!
