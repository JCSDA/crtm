MODULE Test_Interpolation_LPoly

  ! Environment setup
  ! -----------------
  USE Type_Kinds
  USE CRTM_Interpolation
  USE Unit_Test
  USE Test_Interpolation_Functions

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: Test_LPoly
  PUBLIC :: Test_LPoly_FWDTL
  PUBLIC :: Test_LPoly_TLAD
  
  ! Module parameters
  ! -----------------
  REAL(fp), PARAMETER :: TOLERANCE = 1.0e-15_fp
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp


CONTAINS

  ! Utility routine to compute quadratic polynomials
  SUBROUTINE LPoly_Quadratic(x, xi, lp)
    REAL(fp), INTENT(IN)  :: x(:)
    REAL(fp), INTENT(IN)  :: xi
    REAL(fp), INTENT(OUT) :: lp(:)
    lp(1) = (xi-x(2))*(xi-x(3)) / ((x(1)-x(2))*(x(1)-x(3)))
    lp(2) = (xi-x(1))*(xi-x(3)) / ((x(2)-x(1))*(x(2)-x(3)))
    lp(3) = (xi-x(1))*(xi-x(2)) / ((x(3)-x(1))*(x(3)-x(2)))
  END SUBROUTINE LPoly_Quadratic


  ! Test routine for polynomial calculation
  ! =======================================
  SUBROUTINE Test_LPoly(UTest)
    ! Arguments
    TYPE(UTest_type), INTENT(IN OUT) :: UTest
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER  :: i
    REAL(fp) :: xint
    REAL(fp) :: x(NPTS), lp_left(NPTS-1), lp_right(NPTS-1)
    TYPE(LPoly_type) :: p
    
    WRITE(Message,'("Lagrangian polynomial test, Order=",i0)') ORDER
    CALL Init_Test(UTest,TRIM(Message),Caller='Test_LPoly')

    ! Create input data
    x    = (/ (REAL(i,fp), i=1,NPTS) /)
    xint = SUM(x)/REAL(NPTS,fp)

    ! Compute polynomials
    CALL LPoly(x, xint, p)
    
    ! Compute test polynomials
    CALL LPoly_Quadratic(x(1:3), xint, lp_left)
    CALL LPoly_Quadratic(x(2:4), xint, lp_right)

    ! Test and report
    DO i = 1, NPTS-1
      CALL Is_Equal_Within(lp_left(i) , p%lp_left(i) , TOLERANCE, UTest)
      CALL Is_Equal_Within(lp_right(i), p%lp_right(i), TOLERANCE, UTest)
    END DO
    CALL Report_Test(UTest)
    
  END SUBROUTINE Test_LPoly


  ! Test routine for FWD/TL polynomial calculations
  ! ===============================================
  SUBROUTINE Test_LPoly_FWDTL(UTest)
    ! Arguments
    TYPE(UTest_type), INTENT(IN OUT) :: UTest
    ! Local parameters
    REAL(fp), PARAMETER :: ALPHA = 0.01_fp
    REAL(fp), PARAMETER :: DX    = 0.1_fp
    ! Local variables
    INTEGER          :: i, j
    REAL(fp)         :: x(NPTS), x_NL(NPTS), x_TL(NPTS)
    TYPE(Lpoly_type) :: p, p_NL, p_TL
    REAL(fp)         :: xint, xint_NL, xint_TL
    REAL(fp)         :: delta
    
    CALL Init_Test(UTest,'Lagrangian polynomial FWD/TL test',Caller='Test_LPoly_FWDTL')

    ! Create input data
    x    = (/ (REAL(i,fp), i=1,NPTS) /)
    xint = SUM(x)/REAL(NPTS,fp)

    ! Compute forward model baseline
    CALL LPoly(x, xint, p)

    ! Perform xint perturbation test
    ! ------------------------------
    ! NL model
    x_NL    = x
    xint_NL = xint + (ALPHA*DX)
    CALL LPoly(x_NL, xint_NL, p_NL)
    ! TL model
    x_TL    = ZERO
    xint_TL = DX
    CALL LPoly_TL(x, xint, p, x_TL, xint_TL, p_TL)
    ! Perform comparisons
    DO i = 1, NPTS-1
      ! Left polynomials
      IF ( ABS(p_TL%lp_left(i)) > ZERO ) THEN
        delta = ( (p_NL%lp_left(i) - p%lp_left(i)) / (ALPHA*p_TL%lp_left(i) ) )
        CALL Is_Equal_Within(delta, ONE, ALPHA, UTest)
      END IF
      ! Right polynomials
      IF ( ABS(p_TL%lp_right(i)) > ZERO ) THEN
        delta = ( (p_NL%lp_right(i) - p%lp_right(i)) / (ALPHA*p_TL%lp_right(i) ) )
        CALL Is_Equal_Within(delta, ONE, ALPHA, UTest)
      END IF
    END DO

    ! Perform x perturbation tests
    ! ----------------------------
    DO j = 1, NPTS
      ! NL model
      x_NL    = x
      x_NL(j) = x_NL(j) + (ALPHA*DX)
      xint_NL = xint
      CALL LPoly(x_NL, xint_NL, p_NL)
      ! TL model
      x_TL    = ZERO
      x_TL(j) = DX
      xint_TL = ZERO
      CALL LPoly_TL(x, xint, p, x_TL, xint_TL, p_TL)
      ! Perform comparisons
      DO i = 1, NPTS-1
        ! Left polynomials
        IF ( ABS(p_TL%lp_left(i)) > ZERO ) THEN
          delta = ( (p_NL%lp_left(i) - p%lp_left(i)) / (ALPHA*p_TL%lp_left(i) ) )
          CALL Is_Equal_Within(delta, ONE, ALPHA, UTest)
        END IF
        ! Right polynomials
        IF ( ABS(p_TL%lp_right(i)) > ZERO ) THEN
          delta = ( (p_NL%lp_right(i) - p%lp_right(i)) / (ALPHA*p_TL%lp_right(i) ) )
          CALL Is_Equal_Within(delta, ONE, ALPHA, UTest)
        END IF
      END DO
    END DO
    
    CALL Report_Test(UTest)
      
  END SUBROUTINE Test_LPoly_FWDTL


  ! Test routine for TL/AD polynomial calculations
  ! ==============================================
  SUBROUTINE Test_LPoly_TLAD(UTest)
    ! Arguments
    TYPE(UTest_type), INTENT(IN OUT) :: UTest
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Test_LPoly_TLAD'
    REAL(fp)    , PARAMETER :: DX = 0.1_fp
    ! Local variables
    INTEGER          :: i
    REAL(fp)         :: x(NPTS), x_TL(NPTS), x_AD(NPTS)
    TYPE(Lpoly_type) :: p      , p_TL      , p_AD
    REAL(fp)         :: xint   , xint_TL   , xint_AD
    REAL(fp)         :: TLtTL, dxtAD
    
    CALL Init_Test(UTest,'Lagrangian polynomial TL/AD test',Caller=ROUTINE_NAME)

    ! Create input data
    x    = (/ (REAL(i,fp), i=1,NPTS) /)
    xint = SUM(x)/REAL(NPTS,fp)

    ! Compute forward model baseline
    CALL LPoly(x, xint, p)

    ! Call TL model
    x_TL    = DX
    xint_TL = DX
    CALL LPoly_TL(x, xint, p, x_TL, xint_TL, p_TL)
    
    ! Call AD model
    ! Note: TL model output is AD model input
    p_AD%lp_left  = p_TL%lp_left
    p_AD%lp_right = p_TL%lp_right
    x_AD    = ZERO
    xint_AD = ZERO
    CALL LPoly_AD(x, xint, p, p_AD, x_AD, xint_AD)
    
    ! Perform comparisons
    TLtTL = DOT_PRODUCT(p_TL%lp_left , p_TL%lp_left ) + &
            DOT_PRODUCT(p_TL%lp_right, p_TL%lp_right)
    dxtAD = DOT_PRODUCT(x_TL,x_AD) + xint_TL*xint_AD
    CALL Is_Equal_Within(TLtTL, dxtAD, TOLERANCE, UTest)
    
    CALL Report_Test(UTest)
      
  END SUBROUTINE Test_LPoly_TLAD

END MODULE Test_Interpolation_LPoly
