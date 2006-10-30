!
! Program to test the Integrate_Utility module procedures
!
PROGRAM Test_Integrate_Utility

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds,        ONLY: fp=>fp_kind
  USE Message_Handler,   ONLY: SUCCESS, FAILURE, Display_Message
  USE Integrate_Utility, ONLY: Simpsons_Integral, Gauss_Legendre
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Test_Integrate_Utility'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: Test_Integrate_Utility.f90,v 1.1 2006/06/08 19:46:40 wd20pd Exp $'
  ! The test function coefficients
  INTEGER,  PARAMETER :: N_COEFF = 6
  REAL(fp), PARAMETER, DIMENSION( N_COEFF ) :: COEFF = &
    (/   0.2_fp,   25.0_fp, -200.0_fp, &
       675.0_fp, -900.0_fp,  400.0_fp  /)
  ! The maximum number of polynomial order to use in the integration
  INTEGER,  PARAMETER :: MAX_ORDER = 7


  ! ---------
  ! Variables
  ! ---------
  INTEGER :: i


  ! Output some intial info
  WRITE( *, '( /5x, "Function to be integrated:" )' )
  DO i = N_COEFF, 2, -1
    WRITE( *, '( "(",f6.1,")x^",i1," + ")', ADVANCE = 'NO' ) COEFF(i), i-1
  END DO
  WRITE( *, '( f6.1 )' ) COEFF(1)

  ! Perform the Simpson's integration tests
  CALL Simpson_Test1
  CALL Simpson_Test2

  ! Perform the Gauss integrate tests
  CALL Gauss_Test


CONTAINS


  SUBROUTINE Gauss_Test
    ! Parameters
    REAL(fp), PARAMETER, DIMENSION( 2 ) :: XINTERVAL = (/0.0_fp,0.8_fp/)
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: Error_Status
    INTEGER :: Allocate_Status
    INTEGER :: n
    REAL(fp) :: Area
    REAL(fp), DIMENSION(:), ALLOCATABLE :: x, y
    REAL(fp), DIMENSION(:), ALLOCATABLE :: Weight

    ! Output the test description
    WRITE( *, '( /5x, "Gauss_Legendre integration test: ",&
                &/5x,"X-range for which function is defined: [",f6.4,",",f6.4,"]")' ) XINTERVAL

    ! Loop over abscissae
    DO n = 2, 10
      ! Allocate the arrays
      ALLOCATE( x(n), y(n), Weight(n), STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error allocating arrays for n = ", i2, ". STAT = ", i5 )' ) &
                        n, Allocate_Status
        CALL display_message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              FAILURE )
        STOP
      END IF
      ! Compute the weights
      Error_Status = Gauss_Legendre( x, Weight, &
                                     xInterval=xInterval )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error computing abscissa and weights for n = ", i2 )' ) n
        CALL display_message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              FAILURE )
        STOP
      END IF

      ! Compute the function
      y = Func(X)

      ! Integrate the function
      Area = SUM( Weight * y )

      ! Output the result
      WRITE( *, '( 10x, "n= ", i2, " result : ", es17.10 )' ) n, Area

      ! Deallocate the arrays
      DEALLOCATE( x, y, Weight, STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error deallocating arrays for n = ", i2, ". STAT = ", i5 )' ) &
                        n, Allocate_Status
        CALL display_message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              FAILURE )
        STOP
      END IF

    END DO

    ! Output the exact result for comparison
    WRITE( *, '( 10x, "Exact result : ", es17.10 )' ) Integrated_Func(xInterval(1), xInterval(2))

  END SUBROUTINE Gauss_Test


  SUBROUTINE Simpson_Test1
    ! Parameters
    INTEGER,  PARAMETER :: N = 11
    REAL(fp), PARAMETER, DIMENSION( N ) :: X = &
      (/ 0.0_fp,  0.12_fp,  0.22_fp, 0.32_fp, &
         0.36_fp, 0.44_fp,  0.45_fp, 0.54_fp, &
         0.64_fp, 0.70_fp,  0.80_fp /)
    ! Output the test description
    WRITE( *, '( /5x, "Simpsons integration UNEVEN dx test: ",&
                &/5x,"X-values for which function is defined:" )' )
    WRITE( *, '( 8(1x, f6.4) )' ) X
    ! Perform the test
    CALL Simpson_Test(X)
  END SUBROUTINE Simpson_Test1


  SUBROUTINE Simpson_Test2
    ! Parameters
    INTEGER,  PARAMETER :: N = 100
    REAL(fp), PARAMETER :: XBEGIN = 0.0_fp
    REAL(fp), PARAMETER :: XEND   = 0.80_fp
    ! Variables
    INTEGER :: i
    REAL(fp), DIMENSION(N) :: x
    ! Output the test description
    WRITE( *, '( /5x, "Simpsons integration EVEN dx test: ",&
                &/5x,"X-range for which function is defined:" )' )
    WRITE( *, '( i8," points over [",f6.4,",",f6.4,"]")' ) N, XBEGIN, XEND
    ! Create the X-data
    x = (/ (REAL(i,fp), i=0,N-1) /) / REAL(N-1,fp)
    x = (XEND-XBEGIN)*x + XBEGIN
    ! Perform the test
    CALL Simpson_Test(x)
  END SUBROUTINE Simpson_Test2


  SUBROUTINE Simpson_Test(x)
    ! Argument
    REAL(fp), DIMENSION(:), INTENT(IN) :: x
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: Error_Status
    REAL(fp), DIMENSION(SIZE(x)) :: y
    REAL(fp) :: Area
    INTEGER :: n, Order
    REAL(fp), DIMENSION(SIZE(x)-1) :: dx
    REAL(fp), DIMENSION(SIZE(x)-1) :: yAve
    ! Create the data to integrate
    n = SIZE(x)
    y = Func(x)
    ! Loop over different interpolation orders
    DO Order = 1, MAX_ORDER, 2
      ! Integrate the function
      Error_Status = Simpsons_Integral( x, y, Area, Order = Order )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE(Message,'("Error integrating the function for order ", i2)') Order
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              FAILURE )
        STOP
      END IF
      ! Output the result
      WRITE( *, '( 10x, "Order ", i2, " result  : ", es17.10 )' ) Order, Area
    END DO
    ! Output the exact result for comparison
    WRITE( *, '( 10x, "Exact result     : ", es17.10 )' ) Integrated_Func(MINVAL(x), MAXVAL(x))
    ! And a summation value
    dx = x(2:n) - x(1:n-1)
    yAve = 0.5_fp * (y(2:n) + y(1:n-1)) 
    WRITE( *, '( 10x, "Summation result : ", es17.10 )' ) SUM( dx * yAve )
  END SUBROUTINE Simpson_Test


  FUNCTION Func(x) RESULT(y)
    REAL(fp), DIMENSION(:)       :: x
    REAL(fp), DIMENSION(SIZE(x)) :: y
    INTEGER :: i
    y = COEFF(N_COEFF)
    DO i = N_COEFF - 1, 1, -1
      y = (x*y) + COEFF(i)
    END DO
  END FUNCTION Func


  FUNCTION Integrated_Func(x1,x2) RESULT(Area)
    REAL(fp) :: x1, x2
    REAL(fp) :: Area
    REAL(fp) :: y1, y2
    REAL(fp) :: Scaled_Coeff
    INTEGER :: i
    Scaled_Coeff = COEFF(N_COEFF)/REAL(N_COEFF,fp)
    y1 = Scaled_Coeff * x1
    y2 = Scaled_Coeff * x2
    DO i = N_COEFF - 1, 1, -1
      Scaled_Coeff = COEFF(i)/REAL(i,fp)
      y1 = (x1*y1) + (Scaled_Coeff*x1)
      y2 = (x2*y2) + (Scaled_Coeff*x2)
    END DO
    Area = y2 - y1
  END FUNCTION Integrated_Func

END PROGRAM Test_Integrate_Utility
