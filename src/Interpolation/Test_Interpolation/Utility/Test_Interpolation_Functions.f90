MODULE Test_Interpolation_Functions

  ! Environment setup
  ! -----------------
  USE Type_Kinds, ONLY: fp
  USE Unit_Test
  USE CRTM_Interpolation
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: zw, dzw
  PUBLIC :: zx, dzx
  PUBLIC :: zy, dzy
  PUBLIC :: z_1d, z_2d, z_3d
  PUBLIC :: Test_Hingepoint_Interpolation
  PUBLIC :: Test_Actual_Interpolation
  PUBLIC :: Test_TL_Interpolation
  PUBLIC :: Test_AD_Interpolation
  PUBLIC :: Test_FWDTL_Interpolation
  PUBLIC :: Test_TLAD_Interpolation
  
  ! Module variables
  ! ----------------
  INTEGER :: i
  
  ! Module parameters
  ! -----------------
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  REAL(fp), PARAMETER :: TWO  = 2.0_fp
  ! Floating point comparison tolerance
  REAL(fp), PARAMETER :: TOLERANCE = 1.0e-12_fp
  ! The coefficients for the independent variables
  REAL(fp), PARAMETER :: WC = 3.0_fp
  REAL(fp), PARAMETER :: XC = 0.1_fp
  REAL(fp), PARAMETER :: YC = 10.0_fp
  ! The number of dimension points
  INTEGER , PARAMETER :: N = 21
  ! Dimension axes for interpolation tests
  REAL(fp), PARAMETER :: W(N) = (/ (REAL(i,fp), i=(-N/2),(N/2)) /)
  REAL(fp), PARAMETER :: X(N) = W - 5.0_fp
  REAL(fp), PARAMETER :: Y(N) = W + 3.5_fp
  ! Index locations for actual interpolations between points
  INTEGER, PARAMETER :: N_INDICES = 3
  INTEGER, PARAMETER :: INDICES(N_INDICES) = (/ N/2, 1, N-1 /) 
  ! TL perturbation parameters
  REAL(fp), PARAMETER :: TL_FRACTION = 0.1_fp
  INTEGER,  PARAMETER :: N_TLPERT = 3
  REAL(fp), PARAMETER :: TLPERT(N_TLPERT) = (/ -TL_FRACTION, ZERO, TL_FRACTION /)

  
CONTAINS


  SUBROUTINE Test_Hingepoint_Interpolation(UTest)
    ! Arguments
    TYPE(UTest_type), INTENT(IN OUT) :: UTest
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Test_Hingepoint_Interpolation'
    ! Local variables
    INTEGER  :: i, j, k
    INTEGER  :: i1, i2
    INTEGER  :: j1, j2
    INTEGER  :: k1, k2
    INTEGER  :: wPower, xPower, yPower
    REAL(fp) :: z1(N), z2(N,N), z3(N,N,N)
    REAL(fp) :: zint
    TYPE(Lpoly_type) :: wlp, xlp, ylp
    
    ! Get the function powers
    ! -----------------------
    CALL get_powers(wPower,xPower,yPower)
    
    ! 1-D test
    ! --------
    CALL Init_Test(UTest,'1-D hingepoint test',Caller=ROUTINE_NAME)
    z1 = z_1d(W,wPower)
    DO i = 1,N
      CALL find_index(W,W(i),i1,i2)
      CALL lpoly(W(i1:i2),W(i),wlp)
      CALL interp_1D(z1(i1:i2),wlp,zint)
      CALL Is_Equal_Within(z1(i), zint, TOLERANCE, UTest)
    END DO
    CALL Report_Test(UTest)

    ! 2-D test
    ! --------
    CALL Init_Test(UTest,'2-D hingepoint test',Caller=ROUTINE_NAME)
    z2 = z_2d(W,X,wPower,xPower)
    DO j = 1,N
      CALL find_index(X,X(j),j1,j2)
      CALL lpoly(X(j1:j2),X(j),xlp)
      DO i = 1,N
        CALL find_index(W,W(i),i1,i2)
        CALL lpoly(W(i1:i2),W(i),wlp)
        CALL interp_2D(z2(i1:i2,j1:j2),wlp,xlp,zint)
        CALL Is_Equal_Within(z2(i,j), zint, TOLERANCE, UTest)
      END DO
    END DO
    CALL Report_Test(UTest)
  
    ! 3-D test
    ! --------
    CALL Init_Test(UTest,'3-D hingepoint test',Caller=ROUTINE_NAME)
    z3 = z_3d(W,X,Y,wPower,xPower,yPower)
    DO k = 1,N
      CALL find_index(Y,Y(k),k1,k2)
      CALL lpoly(Y(k1:k2),Y(k),ylp)
      DO j = 1,N
        CALL find_index(X,X(j),j1,j2)
        CALL lpoly(X(j1:j2),X(j),xlp)
        DO i = 1,N
          CALL find_index(W,W(i),i1,i2)
          CALL lpoly(W(i1:i2),W(i),wlp)
          CALL interp_3D(z3(i1:i2,j1:j2,k1:k2),wlp,xlp,ylp,zint)
          CALL Is_Equal_Within(z3(i,j,k), zint, TOLERANCE, UTest)
        END DO
      END DO
    END DO
    CALL Report_Test(UTest)
  END SUBROUTINE Test_Hingepoint_Interpolation


  SUBROUTINE Test_Actual_Interpolation(UTest)
    ! Arguments
    TYPE(UTest_type), INTENT(IN OUT) :: UTest
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Test_Actual_Interpolation'
    ! Local variables
    INTEGER  :: i, iN
    INTEGER  :: i1, i2
    INTEGER  :: j1, j2
    INTEGER  :: k1, k2
    INTEGER  :: wPower, xPower, yPower
    REAL(fp) :: wint, xint, yint
    REAL(fp) :: z1(N), z2(N,N), z3(N,N,N)
    REAL(fp) :: zint, zcalc
    TYPE(Lpoly_type) :: wlp, xlp, ylp
    
    ! Compute the values to interpolate between
    ! -----------------------------------------
    CALL get_powers(wPower,xPower,yPower)
    z1 = z_1d(W,wPower)
    z2 = z_2d(W,X,wPower,xPower)
    z3 = z_3d(W,X,Y,wPower,xPower,yPower)
    
    ! 1-D test
    ! --------
    CALL Init_Test(UTest,'1-D interpolation test',Caller=ROUTINE_NAME)
    DO i = 1, N_INDICES
      iN = INDICES(i)
      ! Simple average is the interpolation point
      wint  = (w(iN) + w(iN+1))/TWO
      zcalc = zw(wint,wPower)
      ! Compute the w-dimension polynomials
      CALL find_index(W,wint,i1,i2)
      CALL lpoly(W(i1:i2),wint,wlp)
      ! Perform the interpolation and compare
      CALL interp_1D(z1(i1:i2),wlp,zint)
      CALL Is_Equal_Within(zcalc, zint, TOLERANCE, UTest)
    END DO
    CALL Report_Test(UTest)
    
    ! 2-D test
    ! --------
    CALL Init_Test(UTest,'2-D interpolation test',Caller=ROUTINE_NAME)
    DO i = 1, N_INDICES
      iN = INDICES(i)
      ! Simple averages are the interpolation points
      wint = (w(iN) + w(iN+1))/TWO
      xint = (x(iN) + x(iN+1))/TWO
      zcalc = zw(wint,wPower) + zx(xint,xPower)
      ! Compute the w-dimension polynomials
      CALL find_index(W,wint,i1,i2)
      CALL lpoly(W(i1:i2),wint,wlp)
      ! Compute the x-dimension polynomials
      CALL find_index(X,xint,j1,j2)
      CALL lpoly(X(j1:j2),xint,xlp)
      ! Perform the interpolation and compare
      CALL interp_2D(z2(i1:i2,j1:j2),wlp,xlp,zint)
      CALL Is_Equal_Within(zcalc, zint, TOLERANCE, UTest)
    END DO
    CALL Report_Test(UTest)
    
    ! 3-D test
    ! --------
    CALL Init_Test(UTest,'3-D interpolation test',Caller=ROUTINE_NAME)
    DO i = 1, N_INDICES
      iN = INDICES(i)
      ! Simple averages are the interpolation points
      wint = (w(iN) + w(iN+1))/TWO
      xint = (x(iN) + x(iN+1))/TWO
      yint = (y(iN) + y(iN+1))/TWO
      zcalc = zw(wint,wPower) + zx(xint,xPower) + zy(yint,yPower)
      ! Compute the w-dimension polynomials
      CALL find_index(W,wint,i1,i2)
      CALL lpoly(W(i1:i2),wint,wlp)
      ! Compute the x-dimension polynomials
      CALL find_index(X,xint,j1,j2)
      CALL lpoly(X(j1:j2),xint,xlp)
      ! Compute the y-dimension polynomials
      CALL find_index(Y,yint,k1,k2)
      CALL lpoly(Y(k1:k2),yint,ylp)
      ! Perform the interpolation and compare
      CALL interp_3D(z3(i1:i2,j1:j2,k1:k2),wlp,xlp,ylp,zint)
      CALL Is_Equal_Within(zcalc, zint, TOLERANCE, UTest)
    END DO
    CALL Report_Test(UTest)
  END SUBROUTINE Test_Actual_Interpolation

  
  SUBROUTINE Test_TL_Interpolation(UTest)
    ! Arguments
    TYPE(UTest_type), INTENT(IN OUT) :: UTest
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Test_TL_Interpolation'
    ! Local variables
    INTEGER  :: i, iN
    INTEGER  :: iw, ix, iy
    INTEGER  :: i1, i2
    INTEGER  :: j1, j2
    INTEGER  :: k1, k2
    INTEGER  :: wPower, xPower, yPower
    REAL(fp) :: z1(N), z2(N,N), z3(N,N,N)
    REAL(fp) :: w_TL(N), x_TL(N), y_TL(N)
    REAL(fp) :: z1_TL(N), z2_TL(N,N), z3_TL(N,N,N)
    REAL(fp) :: wint, wint_TL
    REAL(fp) :: xint, xint_TL
    REAL(fp) :: yint, yint_TL
    REAL(fp) :: dzcalc, zint_TL
    TYPE(Lpoly_type) :: wlp   , xlp   , ylp
    TYPE(Lpoly_type) :: wlp_TL, xlp_TL, ylp_TL
    
    ! Compute the values to interpolate between
    ! -----------------------------------------
    CALL get_powers(wPower,xPower,yPower)
    z1 = z_1d(W,wPower)
    z2 = z_2d(W,X,wPower,xPower)
    z3 = z_3d(W,X,Y,wPower,xPower,yPower)
    
    ! Initialise TL values
    ! --------------------
    w_TL = ZERO
    x_TL = ZERO
    y_TL = ZERO
    z1_TL = ZERO
    z2_TL = ZERO
    z3_TL = ZERO
  
    ! 1-D test
    ! --------
    CALL Init_Test(UTest,'1-D TL interpolation test',Caller=ROUTINE_NAME)
    ! Loop over w-perturbation amount
    DO iw = 1, N_TLPERT
      wint_TL = TLPERT(iw)
      ! Loop over number of interpolations
      DO i = 1, N_INDICES
        iN = INDICES(i)
        wint = (w(iN) + w(iN+1))/TWO
        ! Compute the analytic derivative
        dzcalc = dzw(wint,wPower)*wint_TL
        ! Compute the w-dimension polynomials
        CALL find_index(W,wint,i1,i2)
        CALL lpoly(W(i1:i2),wint,wlp)
        CALL lpoly_TL(W(i1:i2),wint,wlp,w_TL(i1:i2),wint_TL,wlp_TL)
        ! Perform the interpolation
        CALL interp_1D_TL(z1(i1:i2),wlp,z1_TL(i1:i2),wlp_TL,zint_TL)
        CALL Is_Equal_Within(dzcalc, zint_TL, TOLERANCE, UTest)
      END DO
    END DO
    CALL Report_Test(UTest)

    ! 2-D test
    ! --------
    CALL Init_Test(UTest,'2-D TL interpolation test',Caller=ROUTINE_NAME)
    ! Loop over x-perturbation amount
    DO ix = 1, N_TLPERT
      xint_TL = TLPERT(ix)
      ! Loop over w-perturbation amount
      DO iw = 1, N_TLPERT
        wint_TL = TLPERT(iw)
        ! Loop over interpolation index
        DO i = 1, N_INDICES
          iN = INDICES(i)
          wint = (w(iN) + w(iN+1))/TWO
          xint = (x(iN) + x(iN+1))/TWO
          ! Compute the analytic derivative
          dzcalc = dzw(wint,wPower)*wint_TL + dzx(xint,xPower)*xint_TL
          ! Compute the w-dimension polynomials
          CALL find_index(W,wint,i1,i2)
          CALL lpoly(W(i1:i2),wint,wlp)
          CALL lpoly_TL(W(i1:i2),wint,wlp,w_TL(i1:i2),wint_TL,wlp_TL)
          ! Compute the x-dimension polynomials
          CALL find_index(X,xint,j1,j2)
          CALL lpoly(X(j1:j2),xint,xlp)
          CALL lpoly_TL(X(j1:j2),xint,xlp,x_TL(j1:j2),xint_TL,xlp_TL)
          ! Perform the interpolation
          CALL interp_2D_TL(z2(i1:i2,j1:j2),wlp,xlp,z2_TL(i1:i2,j1:j2),wlp_TL,xlp_TL,zint_TL)
          CALL Is_Equal_Within(dzcalc, zint_TL, TOLERANCE, UTest)
        END DO
      END DO
    END DO
    CALL Report_Test(UTest)
  
    ! 3-D test
    ! --------
    CALL Init_Test(UTest,'3-D TL interpolation test',Caller=ROUTINE_NAME)
    ! Loop over y-perturbation amount
    DO iy = 1, N_TLPERT
      yint_TL = TLPERT(iy)
      ! Loop over x-perturbation amount
      DO ix = 1, N_TLPERT
        xint_TL = TLPERT(ix)
        ! Loop over w-perturbation amount
        DO iw = 1, N_TLPERT
          wint_TL = TLPERT(iw)
          ! Loop over interpolation index
          DO i = 1, N_INDICES
            iN = INDICES(i)
            wint = (w(iN) + w(iN+1))/TWO
            xint = (x(iN) + x(iN+1))/TWO
            yint = (y(iN) + y(iN+1))/TWO
            ! Compute the analytic derivative
            dzcalc = dzw(wint,wPower)*wint_TL + dzx(xint,xPower)*xint_TL + dzy(yint,yPower)*yint_TL
            ! Compute the w-dimension polynomials
            CALL find_index(W,wint,i1,i2)
            CALL lpoly(W(i1:i2),wint,wlp)
            CALL lpoly_TL(W(i1:i2),wint,wlp,w_TL(i1:i2),wint_TL,wlp_TL)
            ! Compute the x-dimension polynomials
            CALL find_index(X,xint,j1,j2)
            CALL lpoly(X(j1:j2),xint,xlp)
            CALL lpoly_TL(X(j1:j2),xint,xlp,x_TL(j1:j2),xint_TL,xlp_TL)
            ! Compute the y-dimension polynomials
            CALL find_index(Y,yint,k1,k2)
            CALL lpoly(Y(k1:k2),yint,ylp)
            CALL lpoly_TL(Y(k1:k2),yint,ylp,y_TL(k1:k2),yint_TL,ylp_TL)
            ! Perform the interpolation
            CALL interp_3D_TL(z3(i1:i2,j1:j2,k1:k2),wlp,xlp,ylp,z3_TL(i1:i2,j1:j2,k1:k2),wlp_TL,xlp_TL,ylp_TL,zint_TL)
            CALL Is_Equal_Within(dzcalc, zint_TL, TOLERANCE, UTest)
          END DO
        END DO
      END DO
    END DO
    CALL Report_Test(UTest)
  END SUBROUTINE Test_TL_Interpolation


  SUBROUTINE Test_AD_Interpolation(UTest)
    ! Arguments
    TYPE(UTest_type), INTENT(IN OUT) :: UTest
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Test_AD_Interpolation'
    ! Local variables
    INTEGER  :: wPower, xPower, yPower
    INTEGER  :: i, iN
    INTEGER  :: i1, i2
    INTEGER  :: j1, j2
    INTEGER  :: k1, k2
    TYPE(Lpoly_type) :: wlp   , xlp   , ylp
    TYPE(Lpoly_type) :: wlp_AD, xlp_AD, ylp_AD
    REAL(fp) :: w_AD(N), x_AD(N), y_AD(N)
    REAL(fp) :: z1(N)   , z2(N,N)   , z3(N,N,N)
    REAL(fp) :: z1_AD(N), z2_AD(N,N), z3_AD(N,N,N)
    REAL(fp) :: wint, wint_AD
    REAL(fp) :: xint, xint_AD
    REAL(fp) :: yint, yint_AD
    REAL(fp) ::       zint_AD
    REAL(fp) :: dzwcalc, dzxcalc, dzycalc

    ! Compute the values to interpolate between
    ! -----------------------------------------
    CALL get_powers(wPower,xPower,yPower)
    z1 = z_1d(W,wPower)
    z2 = z_2d(W,X,wPower,xPower)
    z3 = z_3d(W,X,Y,wPower,xPower,yPower)
    
    ! Initialise AD values
    ! --------------------
    w_AD = ZERO
    x_AD = ZERO
    y_AD = ZERO
    z1_AD = ZERO
    z2_AD = ZERO
    z3_AD = ZERO
  
    ! 1-D test
    ! --------
    CALL Init_Test(UTest,'1-D AD interpolation test',Caller=ROUTINE_NAME)
    ! Loop over interpolation index
    DO i = 1, N_INDICES
      iN = INDICES(i)
      ! Compute the analytic derivatives
      wint = (w(iN) + w(iN+1))/TWO
      dzwcalc = dzw(wint,wPower)
      ! Compute the w-dimension polynomials
      CALL find_index(W,wint,i1,i2)
      CALL lpoly(W(i1:i2),wint,wlp)
      ! Reset the adjoints
      zint_AD      = ONE
      z1_AD(i1:i2) = ZERO
      CALL Clear_LPoly(wlp_AD)
      wint_AD      = ZERO
      w_AD(i1:i2)  = ZERO
      ! Perform the adjoint interpolation
      CALL interp_1D_AD(z1(i1:i2),wlp,zint_AD,z1_AD(i1:i2),wlp_AD)
      ! Compute the polynomial adjoint
      CALL lpoly_AD(w(i1:i2),wint,wlp,wlp_AD,w_AD(i1:i2),wint_AD)
      CALL Is_Equal_Within(dzwcalc, wint_AD, TOLERANCE, UTest)
    END DO
    CALL Report_Test(UTest)

    ! 2-D test
    ! --------
    CALL Init_Test(UTest,'2-D AD interpolation test',Caller=ROUTINE_NAME)
    ! Loop over interpolation index
    DO i = 1, N_INDICES
      iN = INDICES(i)
      ! Compute the analytic derivatives
      wint = (w(iN) + w(iN+1))/TWO
      xint = (x(iN) + x(iN+1))/TWO
      dzwcalc = dzw(wint,wPower)
      dzxcalc = dzx(xint,xPower)
      ! Compute the w-dimension polynomials
      CALL find_index(W,wint,i1,i2)
      CALL lpoly(W(i1:i2),wint,wlp)
      ! Compute the x-dimension polynomials
      CALL find_index(X,xint,j1,j2)
      CALL lpoly(X(j1:j2),xint,xlp)
      ! Reset the adjoints
      zint_AD            = ONE
      z2_AD(i1:i2,j1:j2) = ZERO
      CALL Clear_LPoly(wlp_AD)
      wint_AD            = ZERO
      w_AD(i1:i2)        = ZERO
      CALL Clear_LPoly(xlp_AD)
      xint_AD            = ZERO
      x_AD(i1:i2)        = ZERO
      ! Perform the adjoint interpolation
      CALL interp_2D_AD(z2(i1:i2,j1:j2),wlp,xlp,zint_AD,z2_AD(i1:i2,j1:j2),wlp_AD,xlp_AD)
      ! Compute the polynomial adjoints
      CALL lpoly_AD(x(j1:j2),xint,xlp,xlp_AD,x_AD(j1:j2),xint_AD)
      CALL lpoly_AD(w(i1:i2),wint,wlp,wlp_AD,w_AD(i1:i2),wint_AD)
      CALL Is_Equal_Within(dzxcalc, xint_AD, TOLERANCE, UTest)
      CALL Is_Equal_Within(dzwcalc, wint_AD, TOLERANCE, UTest)
    END DO
    CALL Report_Test(UTest)

    ! 3-D test
    ! --------
    CALL Init_Test(UTest,'3-D AD interpolation test',Caller=ROUTINE_NAME)
    ! Loop over interpolation index
    DO i = 1, N_INDICES
      iN = INDICES(i)
      ! Compute the analytic derivatives
      wint = (w(iN) + w(iN+1))/TWO
      xint = (x(iN) + x(iN+1))/TWO
      yint = (y(iN) + y(iN+1))/TWO
      dzwcalc = dzw(wint,wPower)
      dzxcalc = dzx(xint,xPower)
      dzycalc = dzy(yint,yPower)
      ! Compute the w-dimension polynomials
      CALL find_index(W,wint,i1,i2)
      CALL lpoly(W(i1:i2),wint,wlp)
      ! Compute the x-dimension polynomials
      CALL find_index(X,xint,j1,j2)
      CALL lpoly(X(j1:j2),xint,xlp)
      ! Compute the y-dimension polynomials
      CALL find_index(Y,yint,k1,k2)
      CALL lpoly(Y(k1:k2),yint,ylp)
      ! Reset the adjoints
      zint_AD                  = ONE
      z3_AD(i1:i2,j1:j2,k1:k2) = ZERO
      CALL Clear_LPoly(wlp_AD)
      wint_AD                  = ZERO
      w_AD(i1:i2)              = ZERO
      CALL Clear_LPoly(xlp_AD)
      xint_AD                  = ZERO
      x_AD(j1:j2)              = ZERO
      CALL Clear_LPoly(ylp_AD)
      yint_AD                  = ZERO
      y_AD(k1:k2)              = ZERO
      ! Perform the adjoint interpolation
      CALL interp_3D_AD(z3(i1:i2,j1:j2,k1:k2),wlp,xlp,ylp,zint_AD,z3_AD(i1:i2,j1:j2,k1:k2),wlp_AD,xlp_AD,ylp_AD)
      ! Compute the polynomial adjoints
      CALL lpoly_AD(y(k1:k2),yint,ylp,ylp_AD,y_AD(k1:k2),yint_AD)
      CALL lpoly_AD(x(j1:j2),xint,xlp,xlp_AD,x_AD(j1:j2),xint_AD)
      CALL lpoly_AD(w(i1:i2),wint,wlp,wlp_AD,w_AD(i1:i2),wint_AD)
      CALL Is_Equal_Within(dzycalc, yint_AD, TOLERANCE, UTest)
      CALL Is_Equal_Within(dzxcalc, xint_AD, TOLERANCE, UTest)
      CALL Is_Equal_Within(dzwcalc, wint_AD, TOLERANCE, UTest)
    END DO
    CALL Report_Test(UTest)
  END SUBROUTINE Test_AD_Interpolation


  SUBROUTINE Test_FWDTL_Interpolation(UTest)
    ! Arguments
    TYPE(UTest_type), INTENT(IN OUT) :: UTest
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Test_FWDTL_Interpolation'
    REAL(fp)    , PARAMETER :: ALPHA = 0.01_fp
    REAL(fp)    , PARAMETER :: DELTA = 0.1_fp
    ! Local variables
    INTEGER  :: i, iN
    INTEGER  :: i1, i2, i1_NL, i2_NL
    INTEGER  :: j1, j2, j1_NL, j2_NL
    INTEGER  :: k1, k2, k1_NL, k2_NL
    INTEGER  :: wPower, xPower, yPower
    REAL(fp) :: z1(N), z2(N,N), z3(N,N,N)
    REAL(fp) :: w_TL(N), x_TL(N), y_TL(N)
    REAL(fp) :: z1_TL(N), z2_TL(N,N), z3_TL(N,N,N)
    REAL(fp) :: wint, wint_NL, wint_TL
    REAL(fp) :: xint, xint_NL, xint_TL
    REAL(fp) :: yint, yint_NL, yint_TL
    REAL(fp) :: zint, zint_NL, zint_TL
    REAL(fp) :: Metric
    TYPE(Lpoly_type) :: wlp   , xlp   , ylp
    TYPE(Lpoly_type) :: wlp_NL, xlp_NL, ylp_NL
    TYPE(Lpoly_type) :: wlp_TL, xlp_TL, ylp_TL
    
    
    ! Compute the forward functions
    ! -----------------------------
    CALL get_powers(wPower,xPower,yPower)
    z1 = z_1d(W,wPower)
    z2 = z_2d(W,X,wPower,xPower)
    z3 = z_3d(W,X,Y,wPower,xPower,yPower)
    
    ! Initialise TL values
    ! --------------------
    w_TL = ZERO
    x_TL = ZERO
    y_TL = ZERO
    z1_TL = ZERO
    z2_TL = ZERO
    z3_TL = ZERO
  
    ! 1-D test
    ! --------
    CALL Init_Test(UTest,'1-D FWD/TL interpolation test',Caller=ROUTINE_NAME)
    ! Loop over number of interpolations
    DO i = 1, N_INDICES
      iN = INDICES(i)
      wint = (w(iN) + w(iN+1))/TWO
      ! Baseline forward computations
      CALL find_index(W,wint,i1,i2)
      CALL lpoly(W(i1:i2),wint,wlp)
      CALL interp_1D(z1(i1:i2),wlp,zint)
      ! Perturbed forward computations
      wint_NL = wint+(ALPHA*DELTA)
      CALL find_index(W,wint_NL,i1_NL,i2_NL)
      CALL lpoly(W(i1_NL:i2_NL),wint_NL,wlp_NL)
      CALL interp_1D(z1(i1_NL:i2_NL),wlp_NL,zint_NL)
      ! Tangent-linear computations
      wint_TL = DELTA
      CALL lpoly_TL(W(i1:i2),wint,wlp,w_TL(i1:i2),wint_TL,wlp_TL)
      CALL interp_1D_TL(z1(i1:i2),wlp,z1_TL(i1:i2),wlp_TL,zint_TL)
      ! Compute metric;  FWD(x+a.dx) - FWD(x) 
      !                 ---------------------- - 1 == O(a)
      !                        a.TL(dx)
      Metric = ABS((zint_NL-zint)/(ALPHA*zint_TL) - ONE)
      ! Check that Metric < ALPHA
      CALL Is_LessThan(Metric, ALPHA, UTest)
    END DO
    CALL Report_Test(UTest)
  
    ! 2-D test
    ! --------
    CALL Init_Test(UTest,'2-D FWD/TL interpolation test',Caller=ROUTINE_NAME)
    ! Loop over number of interpolations
    DO i = 1, N_INDICES
      iN = INDICES(i)
      wint = (w(iN) + w(iN+1))/TWO
      xint = (x(iN) + x(iN+1))/TWO
      ! Baseline forward computations, FWD(x)
      CALL find_index(W,wint,i1,i2)
      CALL find_index(X,xint,j1,j2)
      CALL lpoly(W(i1:i2),wint,wlp)
      CALL lpoly(X(j1:j2),xint,xlp)
      CALL interp_2D(z2(i1:i2,j1:j2),wlp,xlp,zint)
      ! Perturbed forward computations, FWD(x+a.dx)
      wint_NL = wint+(ALPHA*DELTA)
      xint_NL = xint+(ALPHA*DELTA)
      CALL find_index(W,wint_NL,i1_NL,i2_NL)
      CALL find_index(X,xint_NL,j1_NL,j2_NL)
      CALL lpoly(W(i1_NL:i2_NL),wint_NL,wlp_NL)
      CALL lpoly(X(j1_NL:j2_NL),xint_NL,xlp_NL)
      CALL interp_2D(z2(i1_NL:i2_NL,j1_NL:j2_NL),wlp_NL,xlp_NL,zint_NL)
      ! Tangent-linear computations, TL(dx)
      wint_TL = DELTA
      xint_TL = DELTA
      CALL lpoly_TL(W(i1:i2),wint,wlp,w_TL(i1:i2),wint_TL,wlp_TL)
      CALL lpoly_TL(X(j1:j2),xint,xlp,x_TL(j1:j2),xint_TL,xlp_TL)
      CALL interp_2D_TL(z2(i1:i2,j1:j2),wlp,xlp,z2_TL(i1:i2,j1:j2),wlp_TL,xlp_TL,zint_TL)
      ! Compute metric;  FWD(x+a.dx) - FWD(x) 
      !                 ---------------------- - 1 == O(a)
      !                        a.TL(dx)
      Metric = ABS((zint_NL-zint)/(ALPHA*zint_TL) - ONE)
      ! Check that Metric < ALPHA
      CALL Is_LessThan(Metric, ALPHA, UTest)
    END DO
    CALL Report_Test(UTest)
  
    ! 3-D test
    ! --------
    CALL Init_Test(UTest,'3-D FWD/TL interpolation test',Caller=ROUTINE_NAME)
    ! Loop over number of interpolations
    DO i = 1, N_INDICES
      iN = INDICES(i)
      wint = (w(iN) + w(iN+1))/TWO
      xint = (X(iN) + X(iN+1))/TWO
      yint = (Y(iN) + Y(iN+1))/TWO
      ! Baseline forward computations, FWD(x)
      CALL find_index(W,wint,i1,i2)
      CALL find_index(X,xint,j1,j2)
      CALL find_index(Y,yint,k1,k2)
      CALL lpoly(W(i1:i2),wint,wlp)
      CALL lpoly(X(j1:j2),xint,xlp)
      CALL lpoly(Y(k1:k2),yint,ylp)
      CALL interp_3D(z3(i1:i2,j1:j2,k1:k2),wlp,xlp,ylp,zint)
      ! Perturbed forward computations, FWD(x+a.dx)
      wint_NL = wint+(ALPHA*DELTA)
      xint_NL = xint+(ALPHA*DELTA)
      yint_NL = yint+(ALPHA*DELTA)
      CALL find_index(W,wint_NL,i1_NL,i2_NL)
      CALL find_index(X,xint_NL,j1_NL,j2_NL)
      CALL find_index(Y,yint_NL,k1_NL,k2_NL)
      CALL lpoly(W(i1_NL:i2_NL),wint_NL,wlp_NL)
      CALL lpoly(X(j1_NL:j2_NL),xint_NL,xlp_NL)
      CALL lpoly(Y(k1_NL:k2_NL),yint_NL,ylp_NL)
      CALL interp_3D(z3(i1_NL:i2_NL,j1_NL:j2_NL,k1_NL:k2_NL),wlp_NL,xlp_NL,ylp_NL,zint_NL)
      ! Tangent-linear computations, TL(dx)
      wint_TL = DELTA
      xint_TL = DELTA
      yint_TL = DELTA
      CALL lpoly_TL(W(i1:i2),wint,wlp,w_TL(i1:i2),wint_TL,wlp_TL)
      CALL lpoly_TL(X(j1:j2),xint,xlp,x_TL(j1:j2),xint_TL,xlp_TL)
      CALL lpoly_TL(Y(k1:k2),yint,ylp,y_TL(k1:k2),yint_TL,ylp_TL)
      CALL interp_3D_TL(z3(i1:i2,j1:j2,k1:k2),wlp,xlp,ylp,z3_TL(i1:i2,j1:j2,k1:k2),wlp_TL,xlp_TL,ylp_TL,zint_TL)
      ! Compute metric;  FWD(x+a.dx) - FWD(x) 
      !                 ---------------------- - 1 == O(a)
      !                        a.TL(dx)
      Metric = ABS((zint_NL-zint)/(ALPHA*zint_TL) - ONE)
      ! Check that Metric < ALPHA
      CALL Is_LessThan(Metric, ALPHA, UTest)
    END DO
    CALL Report_Test(UTest)
  END SUBROUTINE Test_FWDTL_Interpolation


  SUBROUTINE Test_TLAD_Interpolation(UTest)
    ! Arguments
    TYPE(UTest_type), INTENT(IN OUT) :: UTest
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Test_TLAD_Interpolation'
    REAL(fp)    , PARAMETER :: ALPHA = 0.01_fp
    REAL(fp)    , PARAMETER :: DELTA = 0.1_fp
    ! Local variables
    INTEGER  :: i, iN
    INTEGER  :: i1, i2
    INTEGER  :: j1, j2
    INTEGER  :: k1, k2
    INTEGER  :: wPower, xPower, yPower
    REAL(fp) :: z1(N), z2(N,N), z3(N,N,N)
    REAL(fp) :: w_TL(N), x_TL(N), y_TL(N)
    REAL(fp) :: w_AD(N), x_AD(N), y_AD(N)
    REAL(fp) :: z1_TL(N), z2_TL(N,N), z3_TL(N,N,N)
    REAL(fp) :: z1_AD(N), z2_AD(N,N), z3_AD(N,N,N)
    REAL(fp) :: wint, wint_TL, wint_AD
    REAL(fp) :: xint, xint_TL, xint_AD
    REAL(fp) :: yint, yint_TL, yint_AD
    REAL(fp) :: zint, zint_TL, zint_AD
    REAL(fp) :: TLtTL, dxtAD
    TYPE(Lpoly_type) :: wlp   , xlp   , ylp
    TYPE(Lpoly_type) :: wlp_TL, xlp_TL, ylp_TL
    TYPE(Lpoly_type) :: wlp_AD, xlp_AD, ylp_AD
    
    
    ! Compute the forward functions
    ! -----------------------------
    CALL get_powers(wPower,xPower,yPower)
    z1 = z_1d(W,wPower)
    z2 = z_2d(W,X,wPower,xPower)
    z3 = z_3d(W,X,Y,wPower,xPower,yPower)
    
    ! Initialise values
    ! -----------------
    w_TL = ZERO
    x_TL = ZERO
    y_TL = ZERO
    z1_TL = ZERO
    z2_TL = ZERO
    z3_TL = ZERO
    w_AD = ZERO
    x_AD = ZERO
    y_AD = ZERO
  
    ! 1-D test
    ! --------
    CALL Init_Test(UTest,'1-D TL/AD interpolation test',Caller=ROUTINE_NAME)
    ! Loop over number of interpolations
    DO i = 1, N_INDICES
      iN = INDICES(i)
      wint = (W(iN) + W(iN+1))/TWO
      ! Baseline forward computations
      CALL find_index(W,wint,i1,i2)
      CALL lpoly(W(i1:i2),wint,wlp)
      CALL interp_1D(z1(i1:i2),wlp,zint)
      ! Tangent-linear computations
      wint_TL = DELTA
      CALL lpoly_TL(W(i1:i2),wint,wlp,w_TL(i1:i2),wint_TL,wlp_TL)
      CALL interp_1D_TL(z1(i1:i2),wlp,z1_TL(i1:i2),wlp_TL,zint_TL)
      ! Adjoint computations, with zint_TL input
      zint_AD = zint_TL
      z1_AD(i1:i2) = ZERO
      CALL Clear_LPoly(wlp_AD)
      wint_AD = ZERO
      CALL interp_1D_AD(z1(i1:i2),wlp,zint_AD,z1_AD(i1:i2),wlp_AD)
      CALL lpoly_AD(W(i1:i2),wint,wlp,wlp_AD,w_AD(i1:i2),wint_AD)
      ! Compute metric;  TL^t.TL == dx^t.AD
      TLtTL = zint_TL**2
      dxtAD = wint_TL*wint_AD
      ! Check that metrics agree
      CALL Is_Equal_Within(TLtTL, dxtAD, TOLERANCE, UTest)
    END DO
    CALL Report_Test(UTest)

    ! 2-D test
    ! --------
    CALL Init_Test(UTest,'2-D TL/AD interpolation test',Caller=ROUTINE_NAME)
    ! Loop over number of interpolations
    DO i = 1, N_INDICES
      iN = INDICES(i)
      wint = (W(iN) + W(iN+1))/TWO
      xint = (X(iN) + X(iN+1))/TWO
      ! Baseline forward computations
      CALL find_index(W,wint,i1,i2)
      CALL find_index(X,xint,j1,j2)
      CALL lpoly(W(i1:i2),wint,wlp)
      CALL lpoly(X(j1:j2),xint,xlp)
      CALL interp_2D(z2(i1:i2,j1:j2),wlp,xlp,zint)
      ! Tangent-linear computations
      wint_TL = DELTA
      xint_TL = DELTA
      CALL lpoly_TL(W(i1:i2),wint,wlp,w_TL(i1:i2),wint_TL,wlp_TL)
      CALL lpoly_TL(X(j1:j2),xint,xlp,x_TL(j1:j2),xint_TL,xlp_TL)
      CALL interp_2D_TL(z2(i1:i2,j1:j2),wlp,xlp,z2_TL(i1:i2,j1:j2),wlp_TL,xlp_TL,zint_TL)
      ! Adjoint computations, with zint_TL input
      zint_AD = zint_TL
      z2_AD(i1:i2,j1:j2) = ZERO
      CALL Clear_LPoly(wlp_AD)
      CALL Clear_LPoly(xlp_AD)
      wint_AD = ZERO
      xint_AD = ZERO
      CALL interp_2D_AD(z2(i1:i2,j1:j2),wlp,xlp,zint_AD,z2_AD(i1:i2,j1:j2),wlp_AD,xlp_AD)
      CALL lpoly_AD(X(j1:j2),xint,xlp,xlp_AD,x_AD(j1:j2),xint_AD)
      CALL lpoly_AD(W(i1:i2),wint,wlp,wlp_AD,w_AD(i1:i2),wint_AD)
      ! Compute metric;  TL^t.TL == dx^t.AD
      TLtTL = zint_TL**2
      dxtAD = wint_TL*wint_AD + &
              xint_TL*xint_AD
      ! Check that metrics agree
      CALL Is_Equal_Within(TLtTL, dxtAD, TOLERANCE, UTest)
    END DO
    CALL Report_Test(UTest)

    ! 3-D test
    ! --------
    CALL Init_Test(UTest,'3-D TL/AD interpolation test',Caller=ROUTINE_NAME)
    ! Loop over number of interpolations
    DO i = 1, N_INDICES
      iN = INDICES(i)
      wint = (W(iN) + W(iN+1))/TWO
      xint = (X(iN) + X(iN+1))/TWO
      yint = (Y(iN) + Y(iN+1))/TWO
      ! Baseline forward computations
      CALL find_index(W,wint,i1,i2)
      CALL find_index(X,xint,j1,j2)
      CALL find_index(Y,yint,k1,k2)
      CALL lpoly(W(i1:i2),wint,wlp)
      CALL lpoly(X(j1:j2),xint,xlp)
      CALL lpoly(Y(k1:k2),yint,ylp)
      CALL interp_3D(z3(i1:i2,j1:j2,k1:k2),wlp,xlp,ylp,zint)
     ! Tangent-linear computations
      wint_TL = DELTA
      xint_TL = DELTA
      yint_TL = DELTA
      CALL lpoly_TL(W(i1:i2),wint,wlp,w_TL(i1:i2),wint_TL,wlp_TL)
      CALL lpoly_TL(X(j1:j2),xint,xlp,x_TL(j1:j2),xint_TL,xlp_TL)
      CALL lpoly_TL(Y(k1:k2),yint,ylp,y_TL(k1:k2),yint_TL,ylp_TL)
      CALL interp_3D_TL(z3(i1:i2,j1:j2,k1:k2),wlp,xlp,ylp,z3_TL(i1:i2,j1:j2,k1:k2),wlp_TL,xlp_TL,ylp_TL,zint_TL)
      ! Adjoint computations, with zint_TL input
      zint_AD = zint_TL
      z3_AD(i1:i2,j1:j2,k1:k2) = ZERO
      CALL Clear_LPoly(wlp_AD)
      CALL Clear_LPoly(xlp_AD)
      CALL Clear_LPoly(ylp_AD)
      wint_AD = ZERO
      xint_AD = ZERO
      yint_AD = ZERO
      CALL interp_3D_AD(z3(i1:i2,j1:j2,k1:k2),wlp,xlp,ylp,zint_AD,z3_AD(i1:i2,j1:j2,k1:k2),wlp_AD,xlp_AD,ylp_AD)
      CALL lpoly_AD(Y(k1:k2),yint,ylp,ylp_AD,y_AD(k1:k2),yint_AD)
      CALL lpoly_AD(X(j1:j2),xint,xlp,xlp_AD,x_AD(j1:j2),xint_AD)
      CALL lpoly_AD(W(i1:i2),wint,wlp,wlp_AD,w_AD(i1:i2),wint_AD)
      ! Compute metric;  TL^t.TL == dx^t.AD
      TLtTL = zint_TL**2
      dxtAD = wint_TL*wint_AD + &
              xint_TL*xint_AD + &
              yint_TL*yint_AD
      ! Check that metrics agree
      CALL Is_Equal_Within(TLtTL, dxtAD, TOLERANCE, UTest)
    END DO
    CALL Report_Test(UTest)

  END SUBROUTINE Test_TLAD_Interpolation




  ! ==================
  ! PRIVATE PROCEDURES
  ! ==================

  ! Routine to provide powers of the indep variables
  ! NOTE: Maximum power cannot be greater than that
  !       of the highest order interpolating polynomial
  ! ---------------------------------------------------
  SUBROUTINE get_powers(wPower,xPower,yPower)
    INTEGER, INTENT(OUT) :: wPower,xPower,yPower
    wPower = 2
    xPower = 2
    yPower = 2
  END SUBROUTINE get_powers

  ! z = f(w), f(x), f(y) functions and their derivatives
  ! ----------------------------------------------------
  FUNCTION zw(w,wPower)
    REAL(fp), INTENT(IN) :: w
    INTEGER , INTENT(IN) :: wPower
    REAL(fp) :: zw
    zw = WC*w**wPower
  END FUNCTION zw

  FUNCTION dzw(w,wPower)
    REAL(fp), INTENT(IN) :: w
    INTEGER , INTENT(IN) :: wPower
    REAL(fp) :: dzw
    dzw = WC*wPower*w**(wPower-1)
  END FUNCTION dzw

  FUNCTION zx(x,xPower)
    REAL(fp), INTENT(IN) :: x
    INTEGER , INTENT(IN) :: xPower
    REAL(fp) :: zx
    zx = XC*x**xPower
  END FUNCTION zx

  FUNCTION dzx(x,xPower)
    REAL(fp), INTENT(IN) :: x
    INTEGER , INTENT(IN) :: xPower
    REAL(fp) :: dzx
    dzx = XC*xPower*x**(xPower-1)
  END FUNCTION dzx

  FUNCTION zy(y,yPower)
    REAL(fp), INTENT(IN) :: y
    INTEGER , INTENT(IN) :: yPower
    REAL(fp) :: zy
    zy = YC*y**yPower
  END FUNCTION zy
  
  FUNCTION dzy(y,yPower)
    REAL(fp), INTENT(IN) :: y
    INTEGER , INTENT(IN) :: yPower
    REAL(fp) :: dzy
    dzy = YC*yPower*y**(yPower-1)
  END FUNCTION dzy
  
  ! z = f(w), f(w,x), f(w,x,y) functions
  ! ------------------------------------
  FUNCTION z_1d(w,wPower)
    REAL(fp), INTENT(IN) :: w(:)
    INTEGER , INTENT(IN) :: wPower
    REAL(fp) :: z_1d(SIZE(w))
    INTEGER :: i
    DO i = 1,SIZE(w)
      z_1d(i) = zw(w(i),wPower)
    END DO
  END FUNCTION z_1d

  FUNCTION z_2d(w,x,wPower,xPower)
    REAL(fp), INTENT(IN) :: w(:),x(:)
    INTEGER , INTENT(IN) :: wPower, xPower
    REAL(fp) :: z_2d(SIZE(w),SIZE(x))
    REAL(fp) :: zj
    INTEGER :: i, j
    DO j = 1,SIZE(x)
      zj = zx(x(j),xPower)
      DO i = 1,SIZE(w)
        z_2d(i,j) = zj + zw(w(i),wPower)
      END DO
    END DO
  END FUNCTION z_2d

  FUNCTION z_3d(w,x,y,wPower,xPower,yPower)
    REAL(fp), INTENT(IN) :: w(:),x(:),y(:)
    INTEGER , INTENT(IN) :: wPower, xPower, yPower
    REAL(fp) :: z_3d(SIZE(w),SIZE(x),SIZE(y))
    REAL(fp) :: zj, zk
    INTEGER :: i, j, k
    DO k = 1,SIZE(y)
      zk = zy(y(k),yPower)
      DO j = 1,SIZE(x)
        zj = zx(x(j),xPower)
        DO i = 1,SIZE(w)
          z_3d(i,j,k) = zk + zj + zw(w(i),wPower)
        END DO
      END DO
    END DO
  END FUNCTION z_3d

END MODULE Test_Interpolation_Functions
