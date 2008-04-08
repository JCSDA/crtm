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
  PUBLIC :: Test_LPoly_TLAD
  
  ! Module parameters
  ! -----------------
  REAL(fp), PARAMETER :: TOLERANCE = 1.0e-15_fp
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp


CONTAINS

  SUBROUTINE LPoly_Linear(x, xi, lp)
    REAL(fp), INTENT(IN)  :: x(:)
    REAL(fp), INTENT(IN)  :: xi
    REAL(fp), INTENT(OUT) :: lp(:)
    lp(1) = (xi-x(2)) / (x(1)-x(2))
    lp(2) = (xi-x(1)) / (x(2)-x(1))
  END SUBROUTINE LPoly_Linear

  SUBROUTINE LPoly_Cubic(x, xi, lp)
    REAL(fp), INTENT(IN)  :: x(:)
    REAL(fp), INTENT(IN)  :: xi
    REAL(fp), INTENT(OUT) :: lp(:)
    lp(1) = (xi-x(2))*(xi-x(3))*(xi-x(4)) / ((x(1)-x(2))*(x(1)-x(3))*(x(1)-x(4)))
    lp(2) = (xi-x(1))*(xi-x(3))*(xi-x(4)) / ((x(2)-x(1))*(x(2)-x(3))*(x(2)-x(4)))
    lp(3) = (xi-x(1))*(xi-x(2))*(xi-x(4)) / ((x(3)-x(1))*(x(3)-x(2))*(x(3)-x(4)))
    lp(4) = (xi-x(1))*(xi-x(2))*(xi-x(3)) / ((x(4)-x(1))*(x(4)-x(2))*(x(4)-x(3)))
  END SUBROUTINE LPoly_Cubic

  SUBROUTINE LPoly_Quintic(x, xi, lp)
    REAL(fp), INTENT(IN)  :: x(:)
    REAL(fp), INTENT(IN)  :: xi
    REAL(fp), INTENT(OUT) :: lp(:)
    lp(1) = (xi-x(2))*(xi-x(3))*(xi-x(4))*(xi-x(5))*(xi-x(6)) / ((x(1)-x(2))*(x(1)-x(3))*(x(1)-x(4))*(x(1)-x(5))*(x(1)-x(6)))
    lp(2) = (xi-x(1))*(xi-x(3))*(xi-x(4))*(xi-x(5))*(xi-x(6)) / ((x(2)-x(1))*(x(2)-x(3))*(x(2)-x(4))*(x(2)-x(5))*(x(2)-x(6)))
    lp(3) = (xi-x(1))*(xi-x(2))*(xi-x(4))*(xi-x(5))*(xi-x(6)) / ((x(3)-x(1))*(x(3)-x(2))*(x(3)-x(4))*(x(3)-x(5))*(x(3)-x(6)))
    lp(4) = (xi-x(1))*(xi-x(2))*(xi-x(3))*(xi-x(5))*(xi-x(6)) / ((x(4)-x(1))*(x(4)-x(2))*(x(4)-x(3))*(x(4)-x(5))*(x(4)-x(6)))
    lp(5) = (xi-x(1))*(xi-x(2))*(xi-x(3))*(xi-x(4))*(xi-x(6)) / ((x(5)-x(1))*(x(5)-x(2))*(x(5)-x(3))*(x(5)-x(4))*(x(5)-x(6)))
    lp(6) = (xi-x(1))*(xi-x(2))*(xi-x(3))*(xi-x(4))*(xi-x(5)) / ((x(6)-x(1))*(x(6)-x(2))*(x(6)-x(3))*(x(6)-x(4))*(x(6)-x(5)))
  END SUBROUTINE LPoly_Quintic

  SUBROUTINE Test_LPoly(UTest)
    ! Arguments
    TYPE(UTest_type), INTENT(IN OUT) :: UTest
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER  :: i
    REAL(fp) :: xint
    REAL(fp) :: x(NPTS), lp(NPTS)
    REAL(fp) :: plp
    TYPE(LPoly_type) :: p
    
    WRITE(Message,'("Lagrangian polynomial test, Order=",i0)') ORDER
    CALL Init_Test(UTest,TRIM(Message),Caller='Test_LPoly')

    ! Create input data
    x    = (/ (REAL(i,fp), i=1,NPTS) /)
    xint = SUM(x)/REAL(NPTS,fp)

    ! Compute polynomials
    CALL LPoly(x, xint, p)

    ! Compute test polynomials
    SELECT CASE (ORDER)
      CASE (1)
        CALL LPoly_Linear(x, xint, lp)
      CASE (3)
        CALL LPoly_Cubic(x, xint, lp)
      CASE (5)
        CALL LPoly_Quintic(x, xint, lp)
    END SELECT

    ! Test and report
    DO i = 1, NPTS
      CALL Get_LPoly(p,i,plp)
      CALL Is_Equal_Within(lp(i), plp, TOLERANCE, UTest)
    END DO
    CALL Report_Test(UTest)
    
  END SUBROUTINE Test_LPoly


  SUBROUTINE Test_LPoly_TLAD(UTest)
    ! Arguments
    TYPE(UTest_type), INTENT(IN OUT) :: UTest
    ! Local variables
    INTEGER          :: i, j
    REAL(fp)         :: x(NPTS) , x_TL(NPTS) , x_AD(NPTS)
    TYPE(Lpoly_type) :: p, p_TL, p_AD
    REAL(fp)         :: xint    , xint_TL    , xint_AD
    REAL(fp)         :: TL(NPTS,NPTS+1), AD(NPTS+1,NPTS)
    
    CALL Init_Test(UTest,'Lagrangian polynomial TL/AD test',Caller='Test_LPoly_TLAD')

    ! Create input data
    x    = (/ (REAL(i,fp), i=1,NPTS) /)
    xint = SUM(x)/REAL(NPTS,fp)

    ! Compute forward model baseline
    CALL LPoly(x, xint, p)

    ! Tangent-linear perturbation
    ! ---------------------------
    ! Perturb X input
    DO j = 1, NPTS
      CALL Clear_LPoly(p_TL)
      xint_TL = ZERO
      x_TL    = ZERO
      x_TL(j) = ONE
      CALL LPoly_TL(x, xint, p, x_TL, xint_TL, p_TL)
      DO i = 1, NPTS
        CALL Get_LPoly(p_TL,i,TL(i,j))
      END DO
    END DO
    ! Perturb XINT input
    CALL Clear_LPoly(p_TL)
    xint_TL = ONE
    x_TL    = ZERO
    CALL LPoly_TL(x, xint, p, x_TL, xint_TL, p_TL)
    DO i = 1, NPTS
      CALL Get_LPoly(p_TL,i,TL(i,NPTS+1))
    END DO
    
    
    ! Adjoint perturbation
    ! --------------------
    ! Perturb L values
    DO i = 1, NPTS
      CALL Clear_LPoly(p_AD)
      CALL Set_LPoly(p_AD,i,ONE)
      xint_AD    = ZERO
      x_AD       = ZERO
      CALL lpoly_AD(x, xint, p, p_AD, x_AD , xint_AD)
      AD(1:NPTS,i) = x_AD
      AD(NPTS+1,i) = xint_AD
    END DO

    ! Compare results
    ! ---------------
    CALL Is_Equal_Within(TL, TRANSPOSE(AD), TOLERANCE, UTest)

    CALL Report_Test(UTest)
      
  END SUBROUTINE Test_LPoly_TLAD

END MODULE Test_Interpolation_LPoly
