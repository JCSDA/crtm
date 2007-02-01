MODULE Test_Assertions
  USE Type_Kinds, ONLY: fp
  USE Compare_Float_Numbers
  IMPLICIT NONE
  
  PRIVATE
  PUBLIC :: assert_equal
  PUBLIC :: init_alltests
  PUBLIC :: init_test
  PUBLIC :: report_test
  PUBLIC :: report_alltests

  INTERFACE assert_equal
    MODULE PROCEDURE ip_equal
    MODULE PROCEDURE fp_equal
  END INTERFACE assert_equal
  
  INTEGER, SAVE :: n_tests     , n_passed_tests     , n_failed_tests
  INTEGER, SAVE :: n_alltests=0, n_passed_alltests=0, n_failed_alltests=0
  LOGICAL, SAVE :: noisy = .FALSE.

CONTAINS

  ! ------
  ! Public
  ! ------
  SUBROUTINE ip_equal(i1,i2)
    INTEGER,           INTENT(IN) :: i1,i2
    IF ( i1 == i2 ) THEN
      IF (noisy) THEN
        WRITE(*,'(4x,"Assert equal passed. Expected ", i0, " and got ", i0 )') i1, i2
      END IF
      CALL test_passed()
    ELSE
      WRITE(*,'(4x,"Assert equal FAILED. Expected ", i0, " but got ", i0 )') i1, i2
      CALL test_failed()
    END IF
  END SUBROUTINE ip_equal
  
  SUBROUTINE fp_equal(r1,r2, ulp)
    REAL(fp),          INTENT(IN) :: r1,r2
    INTEGER, OPTIONAL, INTENT(IN) :: ulp
    CHARACTER(*), PARAMETER :: RFMT = 'es25.18'
    INTEGER :: rel
    rel = 1
    IF ( PRESENT(ulp) ) rel = ulp
    IF ( Compare_Float(r1,r2,ULP=rel) ) THEN
      IF (noisy) THEN
        WRITE(*,'(4x,"Assert equal passed. Expected ", '//RFMT//', " and got ", '//RFMT//' )') r1,r2
      END IF
      CALL test_passed()
    ELSE
      WRITE(*,'(4x,"Assert equal FAILED. Expected ", '//RFMT//', " but got ", '//RFMT//' )') r1,r2
      CALL test_failed()
    END IF
  END SUBROUTINE fp_equal

  SUBROUTINE init_alltests()
    n_alltests        = 0
    n_passed_alltests = 0
    n_failed_alltests = 0
  END SUBROUTINE init_alltests

  SUBROUTINE init_test(message,verbose)
    CHARACTER(*),      INTENT(IN) :: message
    LOGICAL, OPTIONAL, INTENT(IN) :: verbose
    n_tests        = 0
    n_passed_tests = 0
    n_failed_tests = 0
    noisy = .FALSE.
    WRITE(*,'(2x,a)') TRIM(message)
    IF ( PRESENT(verbose) ) noisy=verbose
  END SUBROUTINE init_test

  SUBROUTINE report_test()
    CHARACTER(10) :: notify
    notify = ''
    IF ( n_failed_tests /= 0 ) notify = '  <----<<<'
    WRITE(*,'(4x,"Passed ",i0," of ",i0," tests")') n_passed_tests, n_tests
    WRITE(*,'(4x,"Failed ",i0," of ",i0," tests",a)') n_failed_tests, n_tests, notify
  END SUBROUTINE report_test

  SUBROUTINE report_alltests()
    CHARACTER(10) :: notify
    notify = ''
    IF ( n_failed_alltests /= 0 ) notify = '  <----<<<'
    WRITE(*,'(/2x,"=======================")')
    WRITE(*,'( 2x,"SUMMARY of test results")')
    WRITE(*,'( 2x,"=======================")')
    WRITE(*,'( 2x,"Passed ",i0," of ",i0," tests")') n_passed_alltests, n_alltests
    WRITE(*,'( 2x,"Failed ",i0," of ",i0," tests",a)') n_failed_alltests, n_alltests, notify
  END SUBROUTINE report_alltests

  ! -------  
  ! Private
  ! -------  
  SUBROUTINE test_passed()
    n_tests        = n_tests + 1
    n_passed_tests = n_passed_tests + 1
    n_alltests        = n_alltests        + 1
    n_passed_alltests = n_passed_alltests + 1
  END SUBROUTINE test_passed
    
  SUBROUTINE test_failed()
    n_tests        = n_tests + 1
    n_failed_tests = n_failed_tests + 1
    n_alltests        = n_alltests        + 1
    n_failed_alltests = n_failed_alltests + 1
  END SUBROUTINE test_failed
    
END MODULE Test_Assertions




PROGRAM Test_Interpolation
  ! Set up
  USE Type_Kinds, ONLY: fp
  USE Test_Assertions
  USE CRTM_Interpolation
  IMPLICIT NONE
  
  ! Parameters
  INTEGER , PARAMETER :: ULP = 100
  INTEGER , PARAMETER :: NIDX = 6
  INTEGER , PARAMETER :: N = 21
  
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  REAL(fp), PARAMETER :: TWO  = 2.0_fp

  REAL(fp), PARAMETER :: WC     = 3.0_fp
  INTEGER , PARAMETER :: WPOWER = 2
  REAL(fp), PARAMETER :: XC     = 0.1_fp
  INTEGER , PARAMETER :: XPOWER = 3
  REAL(fp), PARAMETER :: YC     = 10.0_fp
  INTEGER , PARAMETER :: YPOWER = 1

  
  ! Variables
  INTEGER  :: i , j , k
  INTEGER  :: i1, i2
  INTEGER  :: j1, j2
  INTEGER  :: k1, k2
  REAL(fp) :: dx
  REAL(fp) :: x_regular(NIDX), x_random(NIDX)
  REAL(fp) :: w(N), x(N), y(N)
  REAL(fp) :: z1(N)
  REAL(fp) :: z2(N,N)
  REAL(fp) :: z3(N,N,N)
  REAL(fp) :: wint, wint_TL, wint_AD
  REAL(fp) :: xint, xint_TL, xint_AD
  REAL(fp) :: yint, yint_TL, yint_AD
  REAL(fp) :: zint, zint_TL, zint_AD
  REAL(fp) :: zcalc
  REAL(fp) :: dzcalc, dzwcalc, dzxcalc, dzycalc
  LOGICAL  :: verbose

  CALL init_alltests()

  
  ! --------------------------
  ! regular index finding test
  ! --------------------------
  CALL init_test('Regular index finding test')

  dx = 1.0_fp
  x_regular = (/ 0.5_fp, 1.5_fp, 2.5_fp, 3.5_fp, 4.5_fp, 5.5_fp /)
  ! In the middle
  CALL find_index(x_regular,dx,3.0_fp,i1,i2)
  CALL assert_equal(2,i1)
  CALL assert_equal(5,i2)
  
  ! Left edge
  CALL find_index(x_regular,dx,1.0_fp,i1,i2)
  CALL assert_equal(1,i1)
  CALL assert_equal(4,i2)
  
  ! Right edge
  CALL find_index(x_regular,dx,5.0_fp,i1,i2)
  CALL assert_equal(3,i1)
  CALL assert_equal(6,i2)
  
  CALL report_test()
  
  
  ! --------------------------
  ! randomw index finding test
  ! --------------------------
  CALL init_test('Random index finding test')

  x_random = (/ 0.5_fp, 1.0_fp, 2.0_fp, 4.0_fp, 8.0_fp, 16.0_fp /)
  ! In the middle
  CALL find_index(x_random,3.14_fp,i1,i2)
  CALL assert_equal(2,i1)
  CALL assert_equal(5,i2)
  
  ! Left edge
  CALL find_index(x_random,0.75_fp,i1,i2)
  CALL assert_equal(1,i1)
  CALL assert_equal(4,i2)
  
  ! Right edge
  CALL find_index(x_random,15.0_fp,i1,i2)
  CALL assert_equal(3,i1)
  CALL assert_equal(6,i2)
  
  CALL report_test()
  

  ! ------------------------------
  ! Hingepoint interpolation tests
  ! ------------------------------
  ! Compute dimension axes for interpolation tests
  w = (/ (REAL(i,fp), i=(-N/2),(N/2)) /)
  x = w
  y = w

  ! 1-D hingepoint test
  CALL init_test('1-D hingepoint test')
  z1 = z_1d(w)
  DO i = 1,N
    CALL find_index(w,w(i),i1,i2)
    CALL interp_1D(w(i1:i2),z1(i1:i2),w(i),zint)
    CALL assert_equal(z1(i), zint, ulp=ULP)
  END DO
  CALL report_test()

  ! 2-D hingepoint test
  CALL init_test('2-D hingepoint test')
  z2 = z_2d(w,x)
  DO j = 1,N
    CALL find_index(x,x(j),j1,j2)
    DO i = 1,N
      CALL find_index(w,w(i),i1,i2)
      CALL interp_2D(w(i1:i2),x(j1:j2),z2(i1:i2,j1:j2),w(i),x(j),zint)
      CALL assert_equal(z2(i,j), zint, ulp=ULP)
    END DO
  END DO
  CALL report_test()
  
  ! 3-D hingepoint test
  CALL init_test('3-D hingepoint test')
  z3 = z_3d(w,x,y)
  DO k = 1,N
    CALL find_index(y,y(k),k1,k2)
    DO j = 1,N
      CALL find_index(x,x(j),j1,j2)
      DO i = 1,N
        CALL find_index(w,w(i),i1,i2)
        CALL interp_3D(w(i1:i2),x(j1:j2),y(k1:k2),z3(i1:i2,j1:j2,k1:k2),w(i),x(j),y(k),zint)
        CALL assert_equal(z3(i,j,k), zint, ulp=ULP)
      END DO
    END DO
  END DO
  CALL report_test()

  
  ! --------------------------
  ! Actual interpolation tests
  ! --------------------------
  ! 1-D test
  CALL init_test('1-D interpolation test')
  ! In the middle
  wint = (w(N/2) + w(N/2+1))/TWO
  zcalc = zw(wint)
  CALL find_index(w,wint,i1,i2)
  CALL interp_1D(w(i1:i2),z1(i1:i2),wint,zint)
  CALL assert_equal(zcalc, zint, ulp=ULP)
  ! Left edge
  wint = (w(1) + w(2))/TWO
  zcalc = zw(wint)
  CALL find_index(w,wint,i1,i2)
  CALL interp_1D(w(i1:i2),z1(i1:i2),wint,zint)
  CALL assert_equal(zcalc, zint, ulp=ULP)
  ! Right edge
  wint = (w(N-1) + w(N))/TWO
  zcalc = zw(wint)
  CALL find_index(w,wint,i1,i2)
  CALL interp_1D(w(i1:i2),z1(i1:i2),wint,zint)
  CALL assert_equal(zcalc, zint, ulp=ULP)

  CALL report_test()

  ! 2-D test
  CALL init_test('2-D interpolation test')
  ! In the middle
  wint = (w(N/2) + w(N/2+1))/TWO
  xint = (x(N/2) + x(N/2+1))/TWO
  zcalc = zw(wint) + zx(xint)
  CALL find_index(w,wint,i1,i2)
  CALL find_index(x,xint,j1,j2)
  CALL interp_2D(w(i1:i2),x(j1:j2),z2(i1:i2,j1:j2),wint,xint,zint)
  CALL assert_equal(zcalc, zint, ulp=ULP)
  ! Left edge
  wint = (w(1) + w(2))/TWO
  xint = (x(1) + x(2))/TWO
  zcalc = zw(wint) + zx(xint)
  CALL find_index(w,wint,i1,i2)
  CALL find_index(x,xint,j1,j2)
  CALL interp_2D(w(i1:i2),x(j1:j2),z2(i1:i2,j1:j2),wint,xint,zint)
  CALL assert_equal(zcalc, zint, ulp=ULP)
  ! Right edge
  wint = (w(N-1) + w(N))/TWO
  xint = (x(N-1) + x(N))/TWO
  zcalc = zw(wint) + zx(xint)
  CALL find_index(w,wint,i1,i2)
  CALL find_index(x,xint,j1,j2)
  CALL interp_2D(w(i1:i2),x(j1:j2),z2(i1:i2,j1:j2),wint,xint,zint)
  CALL assert_equal(zcalc, zint, ulp=ULP)

  CALL report_test()

  ! 3-D test
  CALL init_test('3-D interpolation test')
  ! In the middle
  wint = (w(N/2) + w(N/2+1))/TWO
  xint = (x(N/2) + x(N/2+1))/TWO
  yint = (y(N/2) + y(N/2+1))/TWO
  zcalc = zw(wint) + zx(xint) + zy(yint)
  CALL find_index(w,wint,i1,i2)
  CALL find_index(x,xint,j1,j2)
  CALL find_index(y,yint,k1,k2)
  CALL interp_3D(w(i1:i2),x(j1:j2),y(k1:k2),z3(i1:i2,j1:j2,k1:k2),wint,xint,yint,zint)
  CALL assert_equal(zcalc, zint, ulp=ULP)
  ! Left edge
  wint = (w(1) + w(2))/TWO
  xint = (x(1) + x(2))/TWO
  yint = (y(1) + y(2))/TWO
  zcalc = zw(wint) + zx(xint) + zy(yint)
  CALL find_index(w,wint,i1,i2)
  CALL find_index(x,xint,j1,j2)
  CALL find_index(y,yint,k1,k2)
  CALL interp_3D(w(i1:i2),x(j1:j2),y(k1:k2),z3(i1:i2,j1:j2,k1:k2),wint,xint,yint,zint)
  CALL assert_equal(zcalc, zint, ulp=ULP)
  ! Right edge
  wint = (w(N-1) + w(N))/TWO
  xint = (x(N-1) + x(N))/TWO
  yint = (y(N-1) + y(N))/TWO
  zcalc = zw(wint) + zx(xint) + zy(yint)
  CALL find_index(w,wint,i1,i2)
  CALL find_index(x,xint,j1,j2)
  CALL find_index(y,yint,k1,k2)
  CALL interp_3D(w(i1:i2),x(j1:j2),y(k1:k2),z3(i1:i2,j1:j2,k1:k2),wint,xint,yint,zint)
  CALL assert_equal(zcalc, zint, ulp=ULP)

  CALL report_test()

  
  ! ----------------------
  ! TL interpolation tests
  ! ----------------------
  ! 1-D test
  verbose = .FALSE.
  CALL init_test('1-D TL interpolation test',verbose=verbose)
  ! In the middle
  wint    = (w(N/2) + w(N/2+1))/TWO
  wint_TL = 0.1_fp
  dzcalc = dzw(wint)*wint_TL
  CALL find_index(w,wint,i1,i2)
  CALL interp_1D_TL(w(i1:i2),z1(i1:i2),wint,wint_TL,zint_TL)
  CALL assert_equal(dzcalc, zint_TL, ulp=ULP)
  ! Left edge
  wint = (w(1) + w(2))/TWO
  wint_TL = -1.314159_fp
  dzcalc = dzw(wint)*wint_TL
  CALL find_index(w,wint,i1,i2)
  CALL interp_1D_TL(w(i1:i2),z1(i1:i2),wint,wint_TL,zint_TL)
  CALL assert_equal(dzcalc, zint_TL, ulp=ULP)
  ! Right edge
  wint    = (w(N-1) + w(N))/TWO
  wint_TL = 1.8275_fp
  dzcalc = dzw(wint)*wint_TL
  CALL find_index(w,wint,i1,i2)
  CALL interp_1D_TL(w(i1:i2),z1(i1:i2),wint,wint_TL,zint_TL)
  CALL assert_equal(dzcalc, zint_TL, ulp=ULP)

  CALL report_test()

  ! 2-D test
  verbose = .FALSE.
  CALL init_test('2-D TL interpolation test',verbose=verbose)
  ! In the middle
  wint = (w(N/2) + w(N/2+1))/TWO
  xint = (x(N/2) + x(N/2+1))/TWO
  wint_TL = 0.1_fp
  xint_TL = 0.1_fp
  dzcalc = dzw(wint)*wint_TL + dzx(xint)*xint_TL
  CALL find_index(w,wint,i1,i2)
  CALL find_index(x,xint,j1,j2)
  CALL interp_2D_TL(w(i1:i2),x(j1:j2),z2(i1:i2,j1:j2),wint,xint,wint_TL,xint_TL,zint_TL)
  CALL assert_equal(dzcalc, zint_TL, ulp=ULP)
  ! Left edge
  wint = (w(1) + w(2))/TWO
  xint = (x(1) + x(2))/TWO
  wint_TL = -1.3142_fp
  xint_TL = -1.1756_fp
  dzcalc = dzw(wint)*wint_TL + dzx(xint)*xint_TL
  CALL find_index(w,wint,i1,i2)
  CALL find_index(x,xint,j1,j2)
  CALL interp_2D_TL(w(i1:i2),x(j1:j2),z2(i1:i2,j1:j2),wint,xint,wint_TL,xint_TL,zint_TL)
  CALL assert_equal(dzcalc, zint_TL, ulp=ULP)
  ! Right edge
  wint = (w(N-1) + w(N))/TWO
  xint = (x(N-1) + x(N))/TWO
  wint_TL = 1.1275_fp
  xint_TL = 1.4563_fp
  dzcalc = dzw(wint)*wint_TL + dzx(xint)*xint_TL
  CALL find_index(w,wint,i1,i2)
  CALL find_index(x,xint,j1,j2)
  CALL interp_2D_TL(w(i1:i2),x(j1:j2),z2(i1:i2,j1:j2),wint,xint,wint_TL,xint_TL,zint_TL)
  CALL assert_equal(dzcalc, zint_TL, ulp=ULP)

  CALL report_test()

  ! 3-D test
  verbose = .FALSE.
  CALL init_test('3-D TL interpolation test',verbose=verbose)
  ! In the middle
  wint = (w(N/2) + w(N/2+1))/TWO
  xint = (x(N/2) + x(N/2+1))/TWO
  yint = (y(N/2) + y(N/2+1))/TWO
  wint_TL = 0.1_fp
  xint_TL = 0.1_fp
  yint_TL = 0.1_fp
  dzcalc = dzw(wint)*wint_TL + dzx(xint)*xint_TL + dzy(yint)*yint_TL
  CALL find_index(w,wint,i1,i2)
  CALL find_index(x,xint,j1,j2)
  CALL find_index(y,yint,k1,k2)
  CALL interp_3D_TL(w(i1:i2),x(j1:j2),y(k1:k2),z3(i1:i2,j1:j2,k1:k2),wint,xint,yint,wint_TL,xint_TL,yint_TL,zint_TL)
  CALL assert_equal(dzcalc, zint_TL, ulp=ULP)
  ! Left edge
  wint = (w(1) + w(2))/TWO
  xint = (x(1) + x(2))/TWO
  yint = (y(1) + y(2))/TWO
  wint_TL = -1.3142_fp
  xint_TL = -1.2000_fp
  yint_TL = -1.0199_fp
  dzcalc = dzw(wint)*wint_TL + dzx(xint)*xint_TL + dzy(yint)*yint_TL
  CALL find_index(w,wint,i1,i2)
  CALL find_index(x,xint,j1,j2)
  CALL find_index(y,yint,k1,k2)
  CALL interp_3D_TL(w(i1:i2),x(j1:j2),y(k1:k2),z3(i1:i2,j1:j2,k1:k2),wint,xint,yint,wint_TL,xint_TL,yint_TL,zint_TL)
  CALL assert_equal(dzcalc, zint_TL, ulp=ULP)
  ! Right edge
  wint = (w(N-1) + w(N))/TWO
  xint = (x(N-1) + x(N))/TWO
  yint = (y(N-1) + y(N))/TWO
  wint_TL = 1.8275_fp
  xint_TL = 1.4563_fp
  yint_TL = 1.2121_fp
  dzcalc = dzw(wint)*wint_TL + dzx(xint)*xint_TL + dzy(yint)*yint_TL
  CALL find_index(w,wint,i1,i2)
  CALL find_index(x,xint,j1,j2)
  CALL find_index(y,yint,k1,k2)
  CALL interp_3D_TL(w(i1:i2),x(j1:j2),y(k1:k2),z3(i1:i2,j1:j2,k1:k2),wint,xint,yint,wint_TL,xint_TL,yint_TL,zint_TL)
  CALL assert_equal(dzcalc, zint_TL, ulp=ULP)

  CALL report_test()

  ! ----------------------
  ! AD interpolation tests
  ! ----------------------
  ! 1-D test
  verbose = .FALSE.
  CALL init_test('1-D AD interpolation test',verbose=verbose)
  ! In the middle
  wint    = (w(N/2) + w(N/2+1))/TWO
  wint_AD = ZERO
  zint_AD = ONE
  dzcalc = dzw(wint)
  CALL find_index(w,wint,i1,i2)
  CALL interp_1D_AD(w(i1:i2),z1(i1:i2),wint,zint_AD,wint_AD)
  CALL assert_equal(dzcalc, wint_AD, ulp=ULP)
  ! Left edge
  wint = (w(1) + w(2))/TWO
  wint_AD = ZERO
  zint_AD = ONE
  dzcalc = dzw(wint)
  CALL find_index(w,wint,i1,i2)
  CALL interp_1D_AD(w(i1:i2),z1(i1:i2),wint,zint_AD,wint_AD)
  CALL assert_equal(dzcalc, wint_AD, ulp=ULP)
  ! Right edge
  wint    = (w(N-1) + w(N))/TWO
  wint_AD = ZERO
  zint_AD = ONE
  dzcalc = dzw(wint)
  CALL find_index(w,wint,i1,i2)
  CALL interp_1D_AD(w(i1:i2),z1(i1:i2),wint,zint_AD,wint_AD)
  CALL assert_equal(dzcalc, wint_AD, ulp=ULP)
  
  CALL report_test()
  
  ! 2-D test
  verbose = .FALSE.
  CALL init_test('2-D AD interpolation test',verbose=verbose)
  ! In the middle
  wint    = (w(N/2) + w(N/2+1))/TWO
  xint    = (x(N/2) + x(N/2+1))/TWO
  wint_AD = ZERO
  xint_AD = ZERO
  zint_AD = ONE
  dzwcalc = dzw(wint)
  dzxcalc = dzx(xint)
  CALL find_index(w,wint,i1,i2)
  CALL find_index(x,xint,j1,j2)
  CALL interp_2D_AD(w(i1:i2),x(j1:j2),z2(i1:i2,j1:j2),wint,xint,zint_AD,wint_AD,xint_AD)
  CALL assert_equal(dzwcalc, wint_AD, ulp=ULP)
  CALL assert_equal(dzxcalc, xint_AD, ulp=ULP)
  ! Left edge
  wint = (w(1) + w(2))/TWO
  xint = (x(1) + x(2))/TWO
  wint_AD = ZERO
  xint_AD = ZERO
  zint_AD = ONE
  dzwcalc = dzw(wint)
  dzxcalc = dzx(xint)
  CALL find_index(w,wint,i1,i2)
  CALL find_index(x,xint,j1,j2)
  CALL interp_2D_AD(w(i1:i2),x(j1:j2),z2(i1:i2,j1:j2),wint,xint,zint_AD,wint_AD,xint_AD)
  CALL assert_equal(dzwcalc, wint_AD, ulp=ULP)
  CALL assert_equal(dzxcalc, xint_AD, ulp=ULP)
  ! Right edge
  wint = (w(N-1) + w(N))/TWO
  xint = (x(N-1) + x(N))/TWO
  wint_AD = ZERO
  xint_AD = ZERO
  zint_AD = ONE
  dzwcalc = dzw(wint)
  dzxcalc = dzx(xint)
  CALL find_index(w,wint,i1,i2)
  CALL find_index(x,xint,j1,j2)
  CALL interp_2D_AD(w(i1:i2),x(j1:j2),z2(i1:i2,j1:j2),wint,xint,zint_AD,wint_AD,xint_AD)
  CALL assert_equal(dzwcalc, wint_AD, ulp=ULP)
  CALL assert_equal(dzxcalc, xint_AD, ulp=ULP)
  
  CALL report_test()

  ! 3-D test
  verbose = .FALSE.
  CALL init_test('3-D AD interpolation test',verbose=verbose)
  ! In the middle
  wint    = (w(N/2) + w(N/2+1))/TWO
  xint    = (x(N/2) + x(N/2+1))/TWO
  yint    = (y(N/2) + y(N/2+1))/TWO
  wint_AD = ZERO
  xint_AD = ZERO
  yint_AD = ZERO
  zint_AD = ONE
  dzwcalc = dzw(wint)
  dzxcalc = dzx(xint)
  dzycalc = dzy(yint)
  CALL find_index(w,wint,i1,i2)
  CALL find_index(x,xint,j1,j2)
  CALL find_index(y,yint,k1,k2)
  CALL interp_3D_AD(w(i1:i2),x(j1:j2),y(k1:k2),z3(i1:i2,j1:j2,k1:k2),wint,xint,yint,zint_AD,wint_AD,xint_AD,yint_AD)
  CALL assert_equal(dzwcalc, wint_AD, ulp=ULP)
  CALL assert_equal(dzxcalc, xint_AD, ulp=ULP)
  CALL assert_equal(dzycalc, yint_AD, ulp=ULP)

  CALL report_test()

  ! ------------
  ! Test summary
  ! ------------
  CALL report_alltests()
  
  
CONTAINS

  FUNCTION zw(w)
    REAL(fp), INTENT(IN) :: w
    REAL(fp) :: zw
    zw = WC*w**WPOWER
  END FUNCTION zw

  FUNCTION dzw(w)
    REAL(fp), INTENT(IN) :: w
    REAL(fp) :: dzw
    dzw = WC*WPOWER*w**(WPOWER-1)
  END FUNCTION dzw

  FUNCTION zx(x)
    REAL(fp), INTENT(IN) :: x
    REAL(fp) :: zx
    zx = XC*x**XPOWER
  END FUNCTION zx

  FUNCTION dzx(x)
    REAL(fp), INTENT(IN) :: x
    REAL(fp) :: dzx
    dzx = XC*XPOWER*x**(XPOWER-1)
  END FUNCTION dzx

  FUNCTION zy(y)
    REAL(fp), INTENT(IN) :: y
    REAL(fp) :: zy
    zy = YC*y**YPOWER
  END FUNCTION zy
  
  FUNCTION dzy(y)
    REAL(fp), INTENT(IN) :: y
    REAL(fp) :: dzy
    dzy = YC*YPOWER*y**(YPOWER-1)
  END FUNCTION dzy
  
  FUNCTION z_1d(w)
    REAL(fp), INTENT(IN) :: w(:)
    REAL(fp) :: z_1d(SIZE(w))
    INTEGER :: i
    DO i = 1,SIZE(w)
      z_1d(i) = zw(w(i))
    END DO
  END FUNCTION z_1d

  FUNCTION z_2d(w,x)
    REAL(fp), INTENT(IN) :: w(:),x(:)
    REAL(fp) :: z_2d(SIZE(w),SIZE(x))
    REAL(fp) :: zj
    INTEGER :: i, j
    DO j = 1,SIZE(x)
      zj = zx(x(j))
      DO i = 1,SIZE(w)
        z_2d(i,j) = zj + zw(w(i))
      END DO
    END DO
  END FUNCTION z_2d

  FUNCTION z_3d(w,x,y)
    REAL(fp), INTENT(IN) :: w(:),x(:),y(:)
    REAL(fp) :: z_3d(SIZE(w),SIZE(x),SIZE(y))
    REAL(fp) :: zj, zk
    INTEGER :: i, j, k
    DO k = 1,SIZE(y)
      zk = zy(y(k))
      DO j = 1,SIZE(x)
        zj = zx(x(j))
        DO i = 1,SIZE(w)
          z_3d(i,j,k) = zk + zj + zw(w(i))
        END DO
      END DO
    END DO
  END FUNCTION z_3d

END PROGRAM Test_Interpolation
