!
! Special_Funcitons
!
! Module containing procedures for providing/evaluating various special
! functions.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 28-Nov-2001
!                       paul.vandelst@nooa.gov
!

MODULE Special_Functions


  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,      ONLY: fp, Single
  USE Message_Handler, ONLY: SUCCESS, FAILURE, Display_Message
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: ln_Gamma
  PUBLIC :: Factorial
  PUBLIC :: Binomial_Coefficient


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  REAL(fp), PARAMETER :: TWO  = 2.0_fp


CONTAINS


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ln_Gamma
!
! PURPOSE:
!       Function to return the natural logarithm of the Gamma function.
!
! CALLING SEQUENCE:
!       result = ln_Gamma( x )
!
! INPUTS:
!       x:            Argument of the Gamma function, Gamma(x), for which 
!                     the Gamma function logarithm is required. x > or = 1.
!                     UNITS:      N/A
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       result:       The return value is LN(GAMMA(x)). If the passed
!                     value of x is less than 1.0, the returned result
!                     is -1.0.
!                     UNITS:      N/A
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Scalar
!
! PROCEDURE:
!       The algorithm used here is based on the Lanczos approximation detailed in,
!
!         [1] Abramowitz, M., and Stegun, I.A. (eds) 1972, "Handbook of Mathematical
!             Functions", Applied Mathematics Series, vol.55 (Washington:
!             National Bureau of Standards)
!
!       and discussed in,
!
!         [2] Press, W.H. etal., 1992, "Numerical Recipes in Fortran", 2nd ed.,
!             Cambridge University Press, pp206-207
!
!       the latter reference from which this code is adapted.
!
!       The Gamma function is defined by the integral,
!
!                     Inf
!                     /\
!                     |  z-1  -t
!         Gamma(z) =  | t   .e   dt
!                    \/
!                      0
!
!       When the argument z is an integer,
!
!         n! = Gamma(n+1)
!
!       The Gamma function satisfies the recurrance relation,
!
!         Gamma(z+1) = z.Gamma(z)     .....(1)
!
!       For z > 0, the Lanczos approximation to the Gamma function
!       can be written as,
!
!                               z+0.5                     ___ [        c1      c2            cN      ]
!         Gamma(z+1) = (z+y+0.5)     .exp(-(z+y+0.5)) . \/2pi [ c0 + ----- + ----- + ... + ----- + e ]
!                                                             [       z+1     z+2           z+N      ]
!
!                                     .....(2)
!
!       where e is the error term.
!
!       For y = 5, N = 6 and using the coefficients from ref.[2], the
!       error is smaller that |e| < 2e-10. For the purposes which this
!       function will be used, that is good enough.
!
!       The natural log of the Gamma function is obtained simply by taking
!       the logarithm of the RHS of eqn(2).
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION ln_Gamma( x )
    ! Arguments
    REAL(fp), INTENT(IN) :: x
    ! Function result
    REAL(fp) :: ln_Gamma
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ln_Gamma'
    ! ...The series coefficients
    INTEGER,  PARAMETER :: N_GAMMA_COEFFICIENTS = 6
    REAL(fp), PARAMETER :: GAMMA_COEFFICIENT( N_GAMMA_COEFFICIENTS ) = &
      [ 76.18009172947146_fp,    -86.50532032941677_fp,   24.01409824083091_fp, &
        -1.231739572450155_fp, 1.208650973866179e-03_fp, -5.395239384953e-06_fp ]
    ! ...The other series parameters
    REAL(fp), PARAMETER :: Y                 = 5.0_fp
    REAL(fp), PARAMETER :: SQRT_2PI          = 2.5066282746310005_fp
    REAL(fp), PARAMETER :: C0_AND_ERROR_TERM = 1.000000000190015_fp
    ! ...Literal constants
    REAL(fp), PARAMETER :: POINT5 = 0.5_fp
    ! Local variables
    CHARACTER(ML) :: msg
    REAL(fp) :: z


    ! Set up
    IF ( x < ONE ) THEN
      ln_Gamma = -ONE
      msg = 'Input X argument must be > or = 1.'
      CALL Display_Message( ROUTINE_NAME, msg, FAILURE )
      RETURN
    END IF
      

    ! Z is the temporary argument variable
    z = x


    ! Calculate the multipler terms for ln(Gamma(x+1))
    ln_Gamma = z + Y + POINT5
    ln_Gamma = ( ( z + POINT5 ) * LOG(ln_Gamma) ) - ln_Gamma


    ! Compute the series approximation term
    ! ...Initialise the sum to c0 + e.
    series_sum = C0_AND_ERROR_TERM
    ! ...Sum the series.
    DO i = 1, N_GAMMA_COEFFICIENTS
      series_sum = series_sum + ( GAMMA_COEFFICIENT(i) / ( z + REAL(i,fp) ) )
    END DO


    ! Complete the calculation. Note that the division
    ! by "z" here is to ensure that ln(Gamma(X)) is
    ! being calculated, NOT ln(Gamma(x+1))
    ln_Gamma = ln_Gamma + LOG(SQRT_2PI * series_sum / z)

  END FUNCTION ln_Gamma



!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Factorial
!
! PURPOSE:
!       Function to compute the factorial, n!
!
! CALLING SEQUENCE:
!       result = Factorial( n )
!
! INPUT ARGUMENTS:
!       n:            Value for which n! is required. n > or = 0. The upper
!                     limit depends on the definition of the floating point
!                     kind type, fp. See RESTRICTIONS below.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       result:       The return value is n!. If the passed value of n is
!                     less than 0, or if it is so large that a floating point
!                     variable with the "fp" kind type cannot represent the
!                     value of n!, the returned result is -1.0.
!                     UNITS:      N/A
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Scalar
!
! RESTRICTIONS:
!       Input n argument must be > or = 0. Upper limits are:
!         Single precision:      n < or =  30.
!         Double/Quad precision: n < or = 168.
!       Note that these limits are one less than the values that would place the
!       result of n! on the bleeding edge of what can be represented.
!
!       The type of floating point variables used are determined by the fp
!       data type defined in the Type_Kinds module.
!
! PROCEDURE:
!       If the input value of n is < or = 30, the returned factorial is obtained from
!       a table of values. For n > 30 and <= 168, 
!
!         n! = EXP(LN(Gamma(n+1)))
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION factorial( n )
    ! Arguments
    INTEGER, INTENT(IN) :: n
    ! Function result
    REAL(fp) :: factorial
    ! Local parameters
    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Factorial'
    ! ...Maximum value of n allowed.
    INTEGER, PARAMETER :: MAX_N = 168
    ! ...The tabulated values up to n = 30
    INTEGER,  PARAMETER :: N_TABULATED_VALUES = 30
    REAL(fp), PARAMETER :: FACTORIAL_TABLE(0:N_TABULATED_VALUES) = &
    !     3         2         1
    ! 3 21098765432109876543210987654321
    [ 1.00000000000000000000000000000000e+00_fp, &
      1.00000000000000000000000000000000e+00_fp, &
      2.00000000000000000000000000000000e+00_fp, &
      6.00000000000000000000000000000000e+00_fp, &
      2.40000000000000000000000000000000e+01_fp, &
      1.20000000000000000000000000000000e+02_fp, &
      7.20000000000000000000000000000000e+02_fp, &
      5.04000000000000000000000000000000e+03_fp, &
      4.03200000000000000000000000000000e+04_fp, &
      3.62880000000000000000000000000000e+05_fp, &
      3.62880000000000000000000000000000e+06_fp, &
      3.99168000000000000000000000000000e+07_fp, &
      4.79001600000000000000000000000000e+08_fp, &
      6.22702080000000000000000000000000e+09_fp, &
      8.71782912000000000000000000000000e+10_fp, &
      1.30767436800000000000000000000000e+12_fp, &
      2.09227898880000000000000000000000e+13_fp, &
      3.55687428096000000000000000000000e+14_fp, &
      6.40237370572800000000000000000000e+15_fp, &
      1.21645100408832000000000000000000e+17_fp, &
      2.43290200817664000000000000000000e+18_fp, &
      5.10909421717094400000000000000000e+19_fp, &
      1.12400072777760768000000000000000e+21_fp, &
      2.58520167388849766400000000000000e+22_fp, &
      6.20448401733239439360000000000000e+23_fp, &
      1.55112100433309859840000000000000e+25_fp, &
      4.03291461126605635584000000000000e+26_fp, &
      1.08888694504183521607680000000000e+28_fp, &
      3.04888344611713860501504000000000e+29_fp, &
      8.84176199373970195454361600000000e+30_fp, &
      2.65252859812191058636308480000000e+32_fp  ]
    ! Local variables
    CHARACTER(ML) :: msg
    REAL(fp) :: x


    ! Setup
    IF ( n < 0 ) THEN
      Factorial = -ONE
      msg = 'Input n argument must be > or = 0.'
      CALL Display_Message( ROUTINE_NAME, msg, FAILURE )
      RETURN
    END IF


    ! Calculate n!
    IF ( n <= N_TABULATED_VALUES ) THEN

      ! ...Get value from table
      Factorial = FACTORIAL_TABLE(n)

    ELSE

      ! ...Check if data type can handle the value
      IF ( fp == Single ) THEN
        Factorial = -ONE
        WRITE(msg,'("Floating point type is single precision. "&
                   &"Cannot represent ",i0,"! accurately")') n
        CALL Display_Message( ROUTINE_NAME, msg, FAILURE )
        RETURN
      ELSE
        IF ( n > MAX_N ) THEN
          Factorial = -ONE
          msg = 'Input value of n is just too big!'
          CALL Display_Message( ROUTINE_NAME, msg, FAILURE )
          RETURN
        END IF
      END IF

      ! ...Calculate factorial using Gamma function.
      x = REAL(n, fp)
      Factorial = EXP(ln_Gamma( x + ONE ))

    END IF

  END FUNCTION Factorial



!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Binomial_Coefficient
!
! PURPOSE:
!       Function to compute the binomial coefficient.
!
! CALLING SEQUENCE:
!       result = Binomial_Coefficient( n, k )
!
! INPUTS:
!       n:            Total number of values from which an unordered
!                     combination is required.
!                     Must be > or = k.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       k:            The number of unordered sequences to select from
!                     n numbers.
!                     Must be > or = 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       result:       The return value is the binomial coefficient. If the
!                     passed arguments are invalid, or the result is not
!                     representable (too big), the returned value is -1.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! PROCEDURE:
!       The number of ways of picking k unordered sequences from n numbers
!       is the binomial coefficient or combinatorial number. It is given by,
!
!                        n!
!         n(C)k = ---------------     .....................(1)
!                  ( n-k )! . k!
!
!       The factorial can be written as a Gamma function,
!
!         n! = Gamma( n+1 )
!
!       thus eqn(1) can be expressed as,
!
!                           Gamma( n+1 )
!         n(C)k = -------------------------------     .....(2)
!                  Gamma( n-k+1 ) . Gamma( k+1 )
!
!       Depending on the values of n and k, the resultant Gamma values can
!       be quite large (and unrepresentable) so eqn(2) can be recast to
!       provide the logarithm of the binomial coefficient,
!
!
!         b = LN(Gamma( n+1 )) - LN(Gamma( n-k+1 )) - LN(Gamma( k+1 ))  
!
!       from which the binomial coefficient is easily obtained,
!
!         n(C)k = EXP( b )     ............................(3)
!
!       In this function, before eqn(3) is evaluated, the value of b
!       is tested so that the integer form of EXP( b ) is representable.
!       
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Binomial_Coefficient( n, k )
    ! Arguments
    INTEGER, INTENT(IN) :: n
    INTEGER, INTENT(IN) :: k
    ! Function result
    INTEGER :: Binomial_Coefficient
    ! Local parameters
    CHARACTER(*),  PARAMETER :: ROUTINE_NAME = 'Binomial_Coefficient'
    ! Local variables
    CHARACTER(ML) :: msg
    REAL(fp) :: xn, xk
    REAL(fp) :: ln_coeff


    ! Set up
    IF ( n < 0 .OR. k < 0 ) THEN
      Binomial_Coefficient = -1
      msg = 'Input n, k arguments must be > or = 0.'
      CALL Display_Message( ROUTINE_NAME, msg, FAILURE )
      RETURN
    END IF
    IF ( n < k ) THEN
      Binomial_Coefficient = 0
      msg = 'Input n is < input k. Setting result to 0.'
      CALL Display_Message( ROUTINE_NAME, msg, WARNING )
      RETURN
    END IF
    IF ( k == 0 ) THEN
      Binomial_Coefficient = 1
      RETURN
    END IF


    ! Convert the input integer arguments to floats
    xn = REAL(n, fp)
    xk = REAL(k, fp)


    ! Calulate the natural log of the binomial
    ! coefficient using the ln_Gamma() function
    ln_coeff = ln_Gamma( xn+ONE ) - ln_Gamma( xk+ONE ) - ln_Gamma( xn-xk+ONE )


    ! Determine if the result is representable
    IF ( ln_coeff > LOG(TWO**DIGITS(Binomial_Coefficient) - ONE) ) THEN
      Binomial_Coefficient = -1
      msg = 'Coefficient value too large to represent.'
      CALL Display_Message( ROUTINE_NAME, msg, FAILURE )
      RETURN
    END IF
    
    
    ! Convert the coefficient logarithm to its actual value
    Binomial_Coefficient = NINT(EXP(ln_coeff))

  END FUNCTION Binomial_Coefficient

END MODULE Special_Functions
