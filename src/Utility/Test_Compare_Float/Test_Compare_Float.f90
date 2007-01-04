!
! Test_Compare_Float
!
! Program to test the routines in the Compare_Float_Numbers module
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 13-Aug-2004
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Test_Compare_Float

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds
  USE Message_Handler
  USE Compare_Float_Numbers
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'Test_Compare_Float'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: Test_Compare_Float.f90,v 1.4 2004/11/30 20:42:12 paulv Exp $'

  ! The test numbers
  INTEGER, PARAMETER :: N_TEST_NUMBERS = 5
  REAL(Single), PARAMETER, DIMENSION(N_TEST_NUMBERS) :: SINGLE_REAL = &
    (/ 1.234567890123456e-16_Single, &
       1.234567890123456e-01_Single, &
       1.234567890123456e+01_Single, &
       1.234567890123456e+16_Single, &
       1.0_Single /)

  REAL(Double), PARAMETER, DIMENSION(N_TEST_NUMBERS) :: DOUBLE_REAL = &
    (/ 1.234567890123456e-16_Double, &
       1.234567890123456e-01_Double, &
       1.234567890123456e+01_Double, &
       1.234567890123456e+16_Double, &
       1.0_Double /)

  COMPLEX(Single), PARAMETER, DIMENSION(N_TEST_NUMBERS) :: SINGLE_COMPLEX = &
    (/ (1.234567890123456e-16_Single,1.234567890123456e-16_Single), &
       (1.234567890123456e-01_Single,1.234567890123456e-01_Single), &
       (1.234567890123456e+01_Single,1.234567890123456e+01_Single), &
       (1.234567890123456e+16_Single,1.234567890123456e+16_Single), &
       (1.0_Single,1.0_Single) /)

  COMPLEX(Double), PARAMETER, DIMENSION(N_TEST_NUMBERS) :: DOUBLE_COMPLEX = &
    (/ (1.234567890123456e-16_Double,1.234567890123456e-16_Double), &
       (1.234567890123456e-01_Double,1.234567890123456e-01_Double), &
       (1.234567890123456e+01_Double,1.234567890123456e+01_Double), &
       (1.234567890123456e+16_Double,1.234567890123456e+16_Double), &
       (1.0_Double,1.0_Double) /)

  ! Literal constants
  REAL(Single), PARAMETER :: STEN = 10.0_Single
  REAL(Double), PARAMETER :: DTEN = 10.0_Double


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Answer
  REAL(Single) :: x,  y1, y2,   y3,  y4
  REAL(Double) :: xd, yd1, yd2, yd3, yd4
  COMPLEX(Single) :: xc,  yc1r,  yc2r,  yc3r,  yc4r,  yc1i,  yc2i,  yc3i,  yc4i
  COMPLEX(Double) :: xcd, ycd1r, ycd2r, ycd3r, ycd4r, ycd1i, ycd2i, ycd3i, ycd4i
  REAL(Single), DIMENSION(N_TEST_NUMBERS) :: xv,  yv1,  yv2, yv3, yv4
  REAL(Double), DIMENSION(N_TEST_NUMBERS) :: xvd, yvd1, yvd2, yvd3, yvd4
  REAL(Single), DIMENSION(N_TEST_NUMBERS,2) :: xa,  ya1,  ya2, ya3, ya4
  REAL(Double), DIMENSION(N_TEST_NUMBERS,2) :: xad, yad1, yad2, yad3, yad4
  INTEGER :: i


  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test the Compare_Float_Numbers module routines.', &
                        '$Revision: 1.4 $' )


  !#----------------------------------------------------------------------------#
  !#                         -- TEST THE SCALAR CALLS --                        #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /2x, "Testing the SCALAR calls...." )' )

  DO i = 1, N_TEST_NUMBERS

    WRITE( *, '(//5x, "TEST NUMBER ", i2, " of ", i2 )' ) i, N_TEST_NUMBERS


    ! ---------------------
    ! Single precision test
    ! ---------------------

    x = SINGLE_REAL(i)
    y1 = NEAREST( x, 1.0_Single )
    y2 = y1 - SPACING( x )
    y3 = NEAREST( x, -1.0_Single )
    y4 = y3 + SPACING( x )
    WRITE( *, '( /5x, "SINGLE TEST. x  = ", es20.13, &
                &/5x, "             y1 = ", es20.13, 2x, ":  NEAREST( x, 1.0 )", &
                &/5x, "             y2 = ", es20.13, 2x, ":  y1 - SPACING( x )", &
                &/5x, "             y3 = ", es20.13, 2x, ":  NEAREST( x,-1.0 )", &
                &/5x, "             y4 = ", es20.13, 2x, ":  y3 + SPACING( x )" )' ) &
              x, y1, y2, y3, y4

    WRITE( *, '( /5x, "  Compare_Float( x, y1 )        = ", l1, &
                &/5x, "  Compare_Float( x, y1, ulp=2 ) = ", l1, &
                &/5x, "  Compare_Float( x, y2 )        = ", l1, &
                &/5x, "  Compare_Float( x, y3 )        = ", l1, &
                &/5x, "  Compare_Float( x, y3, ulp=2 ) = ", l1, &
                &/5x, "  Compare_Float( x, y4 )        = ", l1 )' ) &
              Compare_Float( x, y1 ), Compare_Float( x, y1, ulp=2 ), Compare_Float( x, y2 ), &
              Compare_Float( x, y3 ), Compare_Float( x, y3, ulp=2 ), Compare_Float( x, y4 )

    WRITE( *, '( /5x, "  x .EqualTo. y1     = ", l1, &
                &/5x, "  x .EqualTo. y2     = ", l1, &
                &/5x, "  x .GreaterThan. y1 = ", l1, &
                &/5x, "  x .GreaterThan. y2 = ", l1, &
                &/5x, "  x .LessThan. y1    = ", l1, &
                &/5x, "  x .LessThan. y2    = ", l1 )' ) &
              x .EqualTo. y1, x .EqualTo. y2, &
              x .GreaterThan. y1, x .GreaterThan. y2, &
              x .LessThan. y1, x .LessThan. y2

    WRITE( *, '( /5x, "  x .EqualTo. y3     = ", l1, &
                &/5x, "  x .EqualTo. y4     = ", l1, &
                &/5x, "  x .GreaterThan. y3 = ", l1, &
                &/5x, "  x .GreaterThan. y4 = ", l1, &
                &/5x, "  x .LessThan. y3    = ", l1, &
                &/5x, "  x .LessThan. y4    = ", l1 )' ) &
              x .EqualTo. y3, x .EqualTo. y4, &
              x .GreaterThan. y3, x .GreaterThan. y4, &
              x .LessThan. y3, x .LessThan. y4


    ! ---------------------
    ! Double precision test
    ! ---------------------

    xd = DOUBLE_REAL(i)
    yd1 = NEAREST( xd, 1.0_Double )
    yd2 = yd1 - SPACING( xd )
    yd3 = NEAREST( xd, -1.0_Double )
    yd4 = yd3 + SPACING( xd )
    WRITE( *, '(//5x, "DOUBLE TEST. x  = ", es27.20, &
                &/5x, "             y1 = ", es27.20, 2x, ":  NEAREST( x, 1.0 )", &
                &/5x, "             y2 = ", es27.20, 2x, ":  y1 - SPACING( x )", &
                &/5x, "             y3 = ", es27.20, 2x, ":  NEAREST( x,-1.0 )", &
                &/5x, "             y4 = ", es27.20, 2x, ":  y3 + SPACING( x )" )' ) &
              xd, yd1, yd2, yd3, yd4

    WRITE( *, '( /5x, "  Compare_Float( x, y1 )        = ", l1, &
                &/5x, "  Compare_Float( x, y1, ulp=2 ) = ", l1, &
                &/5x, "  Compare_Float( x, y2 )        = ", l1, &
                &/5x, "  Compare_Float( x, y3 )        = ", l1, &
                &/5x, "  Compare_Float( x, y3, ulp=2 ) = ", l1, &
                &/5x, "  Compare_Float( x, y4 )        = ", l1 )' ) &
              Compare_Float( xd, yd1 ), Compare_Float( xd, yd1, ulp=2 ), Compare_Float( xd, yd2 ), &
              Compare_Float( xd, yd3 ), Compare_Float( xd, yd3, ulp=2 ), Compare_Float( xd, yd4 )

    WRITE( *, '( /5x, "  x .EqualTo. y1     = ", l1, &
                &/5x, "  x .EqualTo. y2     = ", l1, &
                &/5x, "  x .GreaterThan. y1 = ", l1, &
                &/5x, "  x .GreaterThan. y2 = ", l1, &
                &/5x, "  x .LessThan. y1    = ", l1, &
                &/5x, "  x .LessThan. y2    = ", l1 )' ) &
              xd .EqualTo. yd1, xd .EqualTo. yd2, &
              xd .GreaterThan. yd1, xd .GreaterThan. yd2, &
              xd .LessThan. yd1, xd .LessThan. yd2

    WRITE( *, '( /5x, "  x .EqualTo. y3     = ", l1, &
                &/5x, "  x .EqualTo. y4     = ", l1, &
                &/5x, "  x .GreaterThan. y3 = ", l1, &
                &/5x, "  x .GreaterThan. y4 = ", l1, &
                &/5x, "  x .LessThan. y3    = ", l1, &
                &/5x, "  x .LessThan. y4    = ", l1 )' ) &
              xd .EqualTo. yd3, xd .EqualTo. yd4, &
              xd .GreaterThan. yd3, xd .GreaterThan. yd4, &
              xd .LessThan. yd3, xd .LessThan. yd4


    ! -----------------------------
    ! Single precision complex test
    ! -----------------------------

    x = SINGLE_REAL(i)
    y1 = NEAREST( x, 1.0_Single )
    y2 = y1 - SPACING( x )
    y3 = NEAREST( x, -1.0_Single )
    y4 = y3 + SPACING( x )

    xc=CMPLX(x,-x)
    yc1r=CMPLX(y1,-x); yc1i=CMPLX(x,-y1)
    yc2r=CMPLX(y2,-x); yc2i=CMPLX(x,-y2)
    yc3r=CMPLX(y3,-x); yc3i=CMPLX(x,-y3)
    yc4r=CMPLX(y4,-x); yc4i=CMPLX(x,-y4)
    WRITE( *, '(//5x, "SINGLE TEST. xc   = (",es20.13,",",es20.13,")", &
                &/5x, "             yc1r = (",es20.13,",",es20.13,")", 2x, ":  NEAREST( x, 1.0 )", &
                &/5x, "             yc2r = (",es20.13,",",es20.13,")", 2x, ":  y1 - SPACING( x )", &
                &/5x, "             yc3r = (",es20.13,",",es20.13,")", 2x, ":  NEAREST( x,-1.0 )", &
                &/5x, "             yc4r = (",es20.13,",",es20.13,")", 2x, ":  y3 + SPACING( x )", &
                &/5x, "             yc1i = (",es20.13,",",es20.13,")", 2x, ":  NEAREST( x, 1.0 )", &
                &/5x, "             yc2i = (",es20.13,",",es20.13,")", 2x, ":  y1 - SPACING( x )", &
                &/5x, "             yc3i = (",es20.13,",",es20.13,")", 2x, ":  NEAREST( x,-1.0 )", &
                &/5x, "             yc4i = (",es20.13,",",es20.13,")", 2x, ":  y3 + SPACING( x )" )' ) &
              xc, yc1r, yc2r, yc3r, yc4r, yc1i, yc2i, yc3i, yc4i

    WRITE( *, '( /5x, "  Compare_Float( xc, yc1r )        = ", l1, &
                &/5x, "  Compare_Float( xc, yc1r, ulp=2 ) = ", l1, &
                &/5x, "  Compare_Float( xc, yc2r )        = ", l1, &
                &/5x, "  Compare_Float( xc, yc3r )        = ", l1, &
                &/5x, "  Compare_Float( xc, yc3r, ulp=2 ) = ", l1, &
                &/5x, "  Compare_Float( xc, yc4r )        = ", l1, &
                &/5x, "  Compare_Float( xc, yc1i )        = ", l1, &
                &/5x, "  Compare_Float( xc, yc1i, ulp=2 ) = ", l1, &
                &/5x, "  Compare_Float( xc, yc2i )        = ", l1, &
                &/5x, "  Compare_Float( xc, yc3i )        = ", l1, &
                &/5x, "  Compare_Float( xc, yc3i, ulp=2 ) = ", l1, &
                &/5x, "  Compare_Float( xc, yc4i )        = ", l1 )' ) &
              Compare_Float( xc, yc1r ), Compare_Float( xc, yc1r, ulp=2 ), Compare_Float( xc, yc2r ), &
              Compare_Float( xc, yc3r ), Compare_Float( xc, yc3r, ulp=2 ), Compare_Float( xc, yc4r ), &
              Compare_Float( xc, yc1i ), Compare_Float( xc, yc1i, ulp=2 ), Compare_Float( xc, yc2i ), &
              Compare_Float( xc, yc3i ), Compare_Float( xc, yc3i, ulp=2 ), Compare_Float( xc, yc4i )

    ! -----------------------------
    ! Double precision complex test
    ! -----------------------------

    xd = DOUBLE_REAL(i)
    yd1 = NEAREST( xd, 1.0_Double )
    yd2 = yd1 - SPACING( xd )
    yd3 = NEAREST( xd, -1.0_Double )
    yd4 = yd3 + SPACING( xd )

    xcd=CMPLX(xd,-xd)
    ycd1r=CMPLX(yd1,-xd); ycd1i=CMPLX(xd,-yd1)
    ycd2r=CMPLX(yd2,-xd); ycd2i=CMPLX(xd,-yd2)
    ycd3r=CMPLX(yd3,-xd); ycd3i=CMPLX(xd,-yd3)
    ycd4r=CMPLX(yd4,-xd); ycd4i=CMPLX(xd,-yd4)
    WRITE( *, '(//5x, "DOUBLE TEST. xcd   = (",es27.20,",",es27.20,")", &
                &/5x, "             ycd1r = (",es27.20,",",es27.20,")", 2x, ":  NEAREST( x, 1.0 )", &
                &/5x, "             ycd2r = (",es27.20,",",es27.20,")", 2x, ":  y1 - SPACING( x )", &
                &/5x, "             ycd3r = (",es27.20,",",es27.20,")", 2x, ":  NEAREST( x,-1.0 )", &
                &/5x, "             ycd4r = (",es27.20,",",es27.20,")", 2x, ":  y3 + SPACING( x )", &
                &/5x, "             ycd1i = (",es27.20,",",es27.20,")", 2x, ":  NEAREST( x, 1.0 )", &
                &/5x, "             ycd2i = (",es27.20,",",es27.20,")", 2x, ":  y1 - SPACING( x )", &
                &/5x, "             ycd3i = (",es27.20,",",es27.20,")", 2x, ":  NEAREST( x,-1.0 )", &
                &/5x, "             ycd4i = (",es27.20,",",es27.20,")", 2x, ":  y3 + SPACING( x )" )' ) &
              xcd, ycd1r, ycd2r, ycd3r, ycd4r, ycd1i, ycd2i, ycd3i, ycd4i

    WRITE( *, '( /5x, "  Compare_Float( xcd, ycd1r )        = ", l1, &
                &/5x, "  Compare_Float( xcd, ycd1r, ulp=2 ) = ", l1, &
                &/5x, "  Compare_Float( xcd, ycd2r )        = ", l1, &
                &/5x, "  Compare_Float( xcd, ycd3r )        = ", l1, &
                &/5x, "  Compare_Float( xcd, ycd3r, ulp=2 ) = ", l1, &
                &/5x, "  Compare_Float( xcd, ycd4r )        = ", l1, &
                &/5x, "  Compare_Float( xcd, ycd1i )        = ", l1, &
                &/5x, "  Compare_Float( xcd, ycd1i, ulp=2 ) = ", l1, &
                &/5x, "  Compare_Float( xcd, ycd2i )        = ", l1, &
                &/5x, "  Compare_Float( xcd, ycd3i )        = ", l1, &
                &/5x, "  Compare_Float( xcd, ycd3i, ulp=2 ) = ", l1, &
                &/5x, "  Compare_Float( xcd, ycd4i )        = ", l1 )' ) &
              Compare_Float( xcd, ycd1r ), Compare_Float( xcd, ycd1r, ulp=2 ), Compare_Float( xcd, ycd2r ), &
              Compare_Float( xcd, ycd3r ), Compare_Float( xcd, ycd3r, ulp=2 ), Compare_Float( xcd, ycd4r ), &
              Compare_Float( xcd, ycd1i ), Compare_Float( xcd, ycd1i, ulp=2 ), Compare_Float( xcd, ycd2i ), &
              Compare_Float( xcd, ycd3i ), Compare_Float( xcd, ycd3i, ulp=2 ), Compare_Float( xcd, ycd4i )

    WRITE( *, '( /10x, "Press <ENTER> to continue..." )' )
    READ( *, '(a)' ) Answer

  END DO



  !#----------------------------------------------------------------------------#
  !#                         -- TEST THE RANK-1 CALLS --                        #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /2x, "Testing the RANK-1 calls...." )' )


  ! ---------------------
  ! Single precision test
  ! ---------------------

  xv = SINGLE_REAL
  yv1 = NEAREST( xv, (/ ( 1.0_Single, i = 1, N_TEST_NUMBERS) /) )
  yv2 = yv1 - SPACING( xv )
  yv3 = NEAREST( xv, (/ (-1.0_Single, i = 1, N_TEST_NUMBERS) /) )
  yv4 = yv3 + SPACING( xv )

  WRITE( *, '( /5x, "SINGLE TEST.", &
              &/5x, " x  = ", 5es20.13, &
              &/5x, " y1 = ", 5es20.13, 2x, ":  NEAREST( x, 1.0 )", &
              &/5x, " y2 = ", 5es20.13, 2x, ":  y1 - SPACING( x )", &
              &/5x, " y3 = ", 5es20.13, 2x, ":  NEAREST( x,-1.0 )", &
              &/5x, " y4 = ", 5es20.13, 2x, ":  y3 + SPACING( x )" )' ) &
            xv, yv1, yv2, yv3, yv4

  WRITE( *, '( /5x, "  Compare_Float( x, y1 )        = ", 5l2, &
              &/5x, "  Compare_Float( x, y1, ulp=2 ) = ", 5l2, &
              &/5x, "  Compare_Float( x, y2 )        = ", 5l2, &
              &/5x, "  Compare_Float( x, y3 )        = ", 5l2, &
              &/5x, "  Compare_Float( x, y3, ulp=2 ) = ", 5l2, &
              &/5x, "  Compare_Float( x, y4 )        = ", 5l2 )' ) &
            Compare_Float( xv, yv1 ), Compare_Float( xv, yv1, ulp=2 ), Compare_Float( xv, yv2 ), &
            Compare_Float( xv, yv3 ), Compare_Float( xv, yv3, ulp=2 ), Compare_Float( xv, yv4 )

  WRITE( *, '( /5x, "  x .EqualTo. y1     = ", 5l2, &
              &/5x, "  x .EqualTo. y2     = ", 5l2, &
              &/5x, "  x .GreaterThan. y1 = ", 5l2, &
              &/5x, "  x .GreaterThan. y2 = ", 5l2, &
              &/5x, "  x .LessThan. y1    = ", 5l2, &
              &/5x, "  x .LessThan. y2    = ", 5l2 )' ) &
            xv .EqualTo. yv1, xv .EqualTo. yv2, &
            xv .GreaterThan. yv1, xv .GreaterThan. yv2, &
            xv .LessThan. yv1, xv .LessThan. yv2

  WRITE( *, '( /5x, "  x .EqualTo. y3     = ", 5l2, &
              &/5x, "  x .EqualTo. y4     = ", 5l2, &
              &/5x, "  x .GreaterThan. y3 = ", 5l2, &
              &/5x, "  x .GreaterThan. y4 = ", 5l2, &
              &/5x, "  x .LessThan. y3    = ", 5l2, &
              &/5x, "  x .LessThan. y4    = ", 5l2 )' ) &
            xv .EqualTo. yv3, xv .EqualTo. yv4, &
            xv .GreaterThan. yv3, xv .GreaterThan. yv4, &
            xv .LessThan. yv3, xv .LessThan. yv4


  ! ---------------------
  ! Double precision test
  ! ---------------------

  xvd = DOUBLE_REAL
  yvd1 = NEAREST( xvd, (/ ( 1.0_Double, i = 1, N_TEST_NUMBERS) /) )
  yvd2 = yvd1 - SPACING( xvd )
  yvd3 = NEAREST( xvd, (/ (-1.0_Double, i = 1, N_TEST_NUMBERS) /) )
  yvd4 = yvd3 + SPACING( xvd )

  WRITE( *, '( /5x, "DOUBLE TEST.", &
              &/5x, " x  = ", 5es27.20, &
              &/5x, " y1 = ", 5es27.20, 2x, ":  NEAREST( x, 1.0 )", &
              &/5x, " y2 = ", 5es27.20, 2x, ":  y1 - SPACING( x )", &
              &/5x, " y3 = ", 5es27.20, 2x, ":  NEAREST( x,-1.0 )", &
              &/5x, " y4 = ", 5es27.20, 2x, ":  y3 + SPACING( x )" )' ) &
            xvd, yvd1, yvd2, yvd3, yvd4

  WRITE( *, '( /5x, "  Compare_Float( x, y1 )        = ", 5l2, &
              &/5x, "  Compare_Float( x, y1, ulp=2 ) = ", 5l2, &
              &/5x, "  Compare_Float( x, y2 )        = ", 5l2, &
              &/5x, "  Compare_Float( x, y3 )        = ", 5l2, &
              &/5x, "  Compare_Float( x, y3, ulp=2 ) = ", 5l2, &
              &/5x, "  Compare_Float( x, y4 )        = ", 5l2 )' ) &
            Compare_Float( xvd, yvd1 ), Compare_Float( xvd, yvd1, ulp=2 ), Compare_Float( xvd, yvd2 ), &
            Compare_Float( xvd, yvd3 ), Compare_Float( xvd, yvd3, ulp=2 ), Compare_Float( xvd, yvd4 )

  WRITE( *, '( /5x, "  x .EqualTo. y1     = ", 5l2, &
              &/5x, "  x .EqualTo. y2     = ", 5l2, &
              &/5x, "  x .GreaterThan. y1 = ", 5l2, &
              &/5x, "  x .GreaterThan. y2 = ", 5l2, &
              &/5x, "  x .LessThan. y1    = ", 5l2, &
              &/5x, "  x .LessThan. y2    = ", 5l2 )' ) &
            xvd .EqualTo. yvd1, xvd .EqualTo. yvd2, &
            xvd .GreaterThan. yvd1, xvd .GreaterThan. yvd2, &
            xvd .LessThan. yvd1, xvd .LessThan. yvd2

  WRITE( *, '( /5x, "  x .EqualTo. y3     = ", 5l2, &
              &/5x, "  x .EqualTo. y4     = ", 5l2, &
              &/5x, "  x .GreaterThan. y3 = ", 5l2, &
              &/5x, "  x .GreaterThan. y4 = ", 5l2, &
              &/5x, "  x .LessThan. y3    = ", 5l2, &
              &/5x, "  x .LessThan. y4    = ", 5l2 )' ) &
            xvd .EqualTo. yvd3, xvd .EqualTo. yvd4, &
            xvd .GreaterThan. yvd3, xvd .GreaterThan. yvd4, &
            xvd .LessThan. yvd3, xvd .LessThan. yvd4

  WRITE( *, '( /10x, "Press <ENTER> to continue..." )' )
  READ( *, '(a)' ) Answer



  !#----------------------------------------------------------------------------#
  !#                         -- TEST THE RANK-2 CALLS --                        #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /2x, "Testing the RANK-2 calls...." )' )


  ! ---------------------
  ! Single precision test
  ! ---------------------

  xa  = RESHAPE((/SINGLE_REAL,SINGLE_REAL+(STEN*SPACING(SINGLE_REAL))/),(/N_TEST_NUMBERS,2/))
  ya1 = NEAREST( xa, RESHAPE((/ ( 1.0_Single, i = 1, N_TEST_NUMBERS*2) /),(/N_TEST_NUMBERS,2/)) )
  ya2 = ya1 - SPACING( xa )
  ya3 = NEAREST( xa, RESHAPE((/ (-1.0_Single, i = 1, N_TEST_NUMBERS*2) /),(/N_TEST_NUMBERS,2/)) )
  ya4 = ya3 + SPACING( xa )

  WRITE( *, '( /5x, "SINGLE TEST.", &
              &/5x, " x  = ", 5es20.13, /11x, 5es20.13, &
              &/5x, " y1 = ", 5es20.13, /11x, 5es20.13, 2x, ":  NEAREST( x, 1.0 )", &
              &/5x, " y2 = ", 5es20.13, /11x, 5es20.13, 2x, ":  y1 - SPACING( x )", &
              &/5x, " y3 = ", 5es20.13, /11x, 5es20.13, 2x, ":  NEAREST( x,-1.0 )", &
              &/5x, " y4 = ", 5es20.13, /11x, 5es20.13, 2x, ":  y3 + SPACING( x )" )' ) &
            xa, ya1, ya2, ya3, ya4

  WRITE( *, '( /5x, "  Compare_Float( x, y1 )        = ", 5l2, " /", 5l2, &
              &/5x, "  Compare_Float( x, y1, ulp=2 ) = ", 5l2, " /", 5l2, &
              &/5x, "  Compare_Float( x, y2 )        = ", 5l2, " /", 5l2, &
              &/5x, "  Compare_Float( x, y3 )        = ", 5l2, " /", 5l2, &
              &/5x, "  Compare_Float( x, y3, ulp=2 ) = ", 5l2, " /", 5l2, &
              &/5x, "  Compare_Float( x, y4 )        = ", 5l2, " /", 5l2  )' ) &
            Compare_Float( xa, ya1 ), Compare_Float( xa, ya1, ulp=2 ), Compare_Float( xa, ya2 ), &
            Compare_Float( xa, ya3 ), Compare_Float( xa, ya3, ulp=2 ), Compare_Float( xa, ya4 )

  WRITE( *, '( /5x, "  x .EqualTo. y1     = ", 5l2, " /", 5l2, &
              &/5x, "  x .EqualTo. y2     = ", 5l2, " /", 5l2, &
              &/5x, "  x .GreaterThan. y1 = ", 5l2, " /", 5l2, &
              &/5x, "  x .GreaterThan. y2 = ", 5l2, " /", 5l2, &
              &/5x, "  x .LessThan. y1    = ", 5l2, " /", 5l2, &
              &/5x, "  x .LessThan. y2    = ", 5l2, " /", 5l2  )' ) &
            xa .EqualTo. ya1, xa .EqualTo. ya2, &
            xa .GreaterThan. ya1, xa .GreaterThan. ya2, &
            xa .LessThan. ya1, xa .LessThan. ya2

  WRITE( *, '( /5x, "  x .EqualTo. y3     = ", 5l2, " /", 5l2, &
              &/5x, "  x .EqualTo. y4     = ", 5l2, " /", 5l2, &
              &/5x, "  x .GreaterThan. y3 = ", 5l2, " /", 5l2, &
              &/5x, "  x .GreaterThan. y4 = ", 5l2, " /", 5l2, &
              &/5x, "  x .LessThan. y3    = ", 5l2, " /", 5l2, &
              &/5x, "  x .LessThan. y4    = ", 5l2, " /", 5l2  )' ) &
            xa .EqualTo. ya3, xa .EqualTo. ya4, &
            xa .GreaterThan. ya3, xa .GreaterThan. ya4, &
            xa .LessThan. ya3, xa .LessThan. ya4


  ! ---------------------
  ! Double precision test
  ! ---------------------

  xad  = RESHAPE((/DOUBLE_REAL,DOUBLE_REAL+(DTEN*SPACING(DOUBLE_REAL))/),(/N_TEST_NUMBERS,2/))
  yad1 = NEAREST( xad, RESHAPE((/ ( 1.0_Double, i = 1, N_TEST_NUMBERS*2) /),(/N_TEST_NUMBERS,2/)) )
  yad2 = yad1 - SPACING( xad )
  yad3 = NEAREST( xad, RESHAPE((/ (-1.0_Double, i = 1, N_TEST_NUMBERS*2) /),(/N_TEST_NUMBERS,2/)) )
  yad4 = yad3 + SPACING( xad )

  WRITE( *, '( /5x, "DOUBLE TEST.", &
              &/5x, " x  = ", 5es27.20, /11x, 5es27.20, &
              &/5x, " y1 = ", 5es27.20, /11x, 5es27.20, 2x, ":  NEAREST( x, 1.0 )", &
              &/5x, " y2 = ", 5es27.20, /11x, 5es27.20, 2x, ":  y1 - SPACING( x )", &
              &/5x, " y3 = ", 5es27.20, /11x, 5es27.20, 2x, ":  NEAREST( x,-1.0 )", &
              &/5x, " y4 = ", 5es27.20, /11x, 5es27.20, 2x, ":  y3 + SPACING( x )" )' ) &
            xad, yad1, yad2, yad3, yad4

  WRITE( *, '( /5x, "  Compare_Float( x, y1 )        = ", 5l2, " /", 5l2, &
              &/5x, "  Compare_Float( x, y1, ulp=2 ) = ", 5l2, " /", 5l2, &
              &/5x, "  Compare_Float( x, y2 )        = ", 5l2, " /", 5l2, &
              &/5x, "  Compare_Float( x, y3 )        = ", 5l2, " /", 5l2, &
              &/5x, "  Compare_Float( x, y3, ulp=2 ) = ", 5l2, " /", 5l2, &
              &/5x, "  Compare_Float( x, y4 )        = ", 5l2, " /", 5l2  )' ) &
            Compare_Float( xad, yad1 ), Compare_Float( xad, yad1, ulp=2 ), Compare_Float( xad, yad2 ), &
            Compare_Float( xad, yad3 ), Compare_Float( xad, yad3, ulp=2 ), Compare_Float( xad, yad4 )

  WRITE( *, '( /5x, "  x .EqualTo. y1     = ", 5l2, " /", 5l2, &
              &/5x, "  x .EqualTo. y2     = ", 5l2, " /", 5l2, &
              &/5x, "  x .GreaterThan. y1 = ", 5l2, " /", 5l2, &
              &/5x, "  x .GreaterThan. y2 = ", 5l2, " /", 5l2, &
              &/5x, "  x .LessThan. y1    = ", 5l2, " /", 5l2, &
              &/5x, "  x .LessThan. y2    = ", 5l2, " /", 5l2  )' ) &
            xad .EqualTo. yad1, xad .EqualTo. yad2, &
            xad .GreaterThan. yad1, xad .GreaterThan. yad2, &
            xad .LessThan. yad1, xad .LessThan. yad2

  WRITE( *, '( /5x, "  x .EqualTo. y3     = ", 5l2, " /", 5l2, &
              &/5x, "  x .EqualTo. y4     = ", 5l2, " /", 5l2, &
              &/5x, "  x .GreaterThan. y3 = ", 5l2, " /", 5l2, &
              &/5x, "  x .GreaterThan. y4 = ", 5l2, " /", 5l2, &
              &/5x, "  x .LessThan. y3    = ", 5l2, " /", 5l2, &
              &/5x, "  x .LessThan. y4    = ", 5l2, " /", 5l2  )' ) &
            xad .EqualTo. yad3, xad .EqualTo. yad4, &
            xad .GreaterThan. yad3, xad .GreaterThan. yad4, &
            xad .LessThan. yad3, xad .LessThan. yad4

END PROGRAM Test_Compare_Float
