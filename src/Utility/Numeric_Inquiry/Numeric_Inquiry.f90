PROGRAM Numeric_Inquiry
 
  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds     , ONLY: Single, Double
  USE Message_Handler, ONLY: Program_Message


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Numeric_Inquiry'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &

  REAL(Single), PARAMETER :: SP_ONE = 1.0_Single
  REAL(Double), PARAMETER :: DP_ONE = 1.0_Double

  INTEGER, PARAMETER :: N_TEST_NUMBERS = 5
  REAL(Single), PARAMETER :: SP_TEST_NUMBER(N_TEST_NUMBERS) = &
    (/ 1.234567890123456e-16_Single, &
       1.234567890123456e-01_Single, &
       1.234567890123456e+01_Single, &
       1.234567890123456e+16_Single, &
       0.01_Single /)
  REAL(Double), PARAMETER :: DP_TEST_NUMBER(N_TEST_NUMBERS) = &
    (/ 1.234567890123456e-16_Double, &
       1.234567890123456e-01_Double, &
       1.234567890123456e+01_Double, &
       1.234567890123456e+16_Double, &
       0.01_Double /)


  ! ------------
  ! Derived type
  ! ------------

  TYPE :: SP_NI_type
    REAL(Single) :: Number
    INTEGER      :: Digits
    REAL(Single) :: Epsilon
    REAL(Single) :: Huge
    INTEGER      :: MaxExponent
    INTEGER      :: MinExponent
    INTEGER      :: Precision
    INTEGER      :: Radix
    INTEGER      :: Range
    REAL(Single) :: Tiny
    INTEGER      :: Exponent
    REAL(Single) :: Fraction
    REAL(Single) :: mNearest
    REAL(Single) :: pNearest
    REAL(Single) :: RRSpacing
    REAL(Single) :: Scale_x_0
    REAL(Single) :: Set_Exponent_x_0
    REAL(Single) :: Spacing
    REAL(Single) :: Epsilon_on_Spacing
    REAL(Single) :: Epsilon_Delta
  END TYPE SP_NI_type

  TYPE :: DP_NI_type
    REAL(Double) :: Number
    INTEGER      :: Digits
    REAL(Double) :: Epsilon
    REAL(Double) :: Huge
    INTEGER      :: MaxExponent
    INTEGER      :: MinExponent
    INTEGER      :: Precision
    INTEGER      :: Radix
    INTEGER      :: Range
    REAL(Double) :: Tiny
    INTEGER      :: Exponent
    REAL(Double) :: Fraction
    REAL(Double) :: mNearest
    REAL(Double) :: pNearest
    REAL(Double) :: RRSpacing
    REAL(Double) :: Scale_x_0
    REAL(Double) :: Set_Exponent_x_0
    REAL(Double) :: Spacing
    REAL(Double) :: Epsilon_on_Spacing
    REAL(Double) :: Epsilon_Delta
  END TYPE DP_NI_type


  ! ---------
  ! Variables
  ! ---------
  INTEGER :: i
  TYPE(SP_NI_type) :: SP_NI(N_TEST_NUMBERS)
  TYPE(DP_NI_type) :: DP_NI(N_TEST_NUMBERS)


  ! Program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to display numeric inquiry intrinsic results.', &
                        '$Revision$' )


  ! Output data for single type
  WRITE( *, '( /5x, "Single kind type : ", i5 )' ) Single
  DO i = 1, N_TEST_NUMBERS
    SP_NI(i)%Number = SP_TEST_NUMBER(i)
    SP_NI(i)%Digits             = DIGITS(       SP_NI(i)%Number )
    SP_NI(i)%Epsilon            = EPSILON(      SP_NI(i)%Number )
    SP_NI(i)%Huge               = HUGE(         SP_NI(i)%Number )
    SP_NI(i)%MaxExponent        = MAXEXPONENT(  SP_NI(i)%Number )
    SP_NI(i)%MinExponent        = MINEXPONENT(  SP_NI(i)%Number )
    SP_NI(i)%Precision          = PRECISION(    SP_NI(i)%Number )
    SP_NI(i)%Radix              = RADIX(        SP_NI(i)%Number )
    SP_NI(i)%Range              = RANGE(        SP_NI(i)%Number )
    SP_NI(i)%Tiny               = TINY(         SP_NI(i)%Number )
    SP_NI(i)%Exponent           = EXPONENT(     SP_NI(i)%Number )
    SP_NI(i)%Fraction           = FRACTION(     SP_NI(i)%Number )
    SP_NI(i)%mNearest           = NEAREST(      SP_NI(i)%Number, -SP_NI(i)%Number )
    SP_NI(i)%pNearest           = NEAREST(      SP_NI(i)%Number,  SP_NI(i)%Number )
    SP_NI(i)%RRSpacing          = RRSPACING(    SP_NI(i)%Number )
    SP_NI(i)%Scale_x_0          = SCALE(        SP_NI(i)%Number, 0 )
    SP_NI(i)%Set_Exponent_x_0   = SET_EXPONENT( SP_NI(i)%Number, 0 )
    SP_NI(i)%Spacing            = SPACING(      SP_NI(i)%Number )
    SP_NI(i)%Epsilon_on_Spacing = SP_NI(i)%Epsilon / SP_NI(i)%Spacing
    SP_NI(i)%Epsilon_Delta      = SP_NI(i)%Epsilon * REAL(SP_NI(i)%Radix,Single)**(SP_NI(i)%Exponent-1)
  END DO

  WRITE( *, '( /2x, "Number            = ", 10(1x,es20.13) )' ) SP_NI%Number
  WRITE( *, '(  2x, "--------------------", 10(a)          )' ) RESHAPE( (/'---------------------'/), &
                                                                         (/ N_TEST_NUMBERS /), &
                                                                         (/'---------------------'/))
  WRITE( *, '(  2x, "DIGITS()          = ", 10(16x,i5)     )' ) SP_NI%Digits
  WRITE( *, '(  2x, "EPSILON()         = ", 10(1x,es20.13) )' ) SP_NI%Epsilon
  WRITE( *, '(  2x, "HUGE()            = ", 10(1x,es20.13) )' ) SP_NI%Huge
  WRITE( *, '(  2x, "MAXEXPONENT()     = ", 10(16x,i5)     )' ) SP_NI%MaxExponent
  WRITE( *, '(  2x, "MINEXPONENT()     = ", 10(16x,i5)     )' ) SP_NI%MinExponent
  WRITE( *, '(  2x, "PRECISION()       = ", 10(16x,i5)     )' ) SP_NI%Precision
  WRITE( *, '(  2x, "RADIX()           = ", 10(16x,i5)     )' ) SP_NI%Radix
  WRITE( *, '(  2x, "RANGE()           = ", 10(16x,i5)     )' ) SP_NI%Range
  WRITE( *, '(  2x, "TINY()            = ", 10(1x,es20.13) )' ) SP_NI%Tiny
  WRITE( *, '(  2x, "EXPONENT()        = ", 10(16x,i5)     )' ) SP_NI%Exponent
  WRITE( *, '(  2x, "FRACTION()        = ", 10(1x,es20.13) )' ) SP_NI%Fraction
  WRITE( *, '(  2x, "NEAREST(-)        = ", 10(1x,es20.13) )' ) SP_NI%mNearest
  WRITE( *, '(  2x, "NEAREST(+)        = ", 10(1x,es20.13) )' ) SP_NI%pNearest
  WRITE( *, '(  2x, "RRSPACING()       = ", 10(1x,es20.13) )' ) SP_NI%RRSpacing
  WRITE( *, '(  2x, "SCALE(x,0)        = ", 10(1x,es20.13) )' ) SP_NI%Scale_x_0
  WRITE( *, '(  2x, "SET_EXPONENT(x,0) = ", 10(1x,es20.13) )' ) SP_NI%Set_Exponent_x_0
  WRITE( *, '(  2x, "SPACING()         = ", 10(1x,es20.13) )' ) SP_NI%Spacing
  WRITE( *, '(  2x, "EPSILON/SPACING   = ", 10(1x,es20.13) )' ) SP_NI%Epsilon_on_Spacing
  WRITE( *, '(  2x, "EPSILON_DELTA     = ", 10(1x,es20.13) )' ) SP_NI%Epsilon_Delta



  ! Output data for double type
  WRITE( *, '(//5x, "Double kind type : ", i5 )' ) Double
  DO i = 1, N_TEST_NUMBERS
    DP_NI(i)%Number = DP_TEST_NUMBER(i)
    DP_NI(i)%Digits             = DIGITS(       DP_NI(i)%Number )
    DP_NI(i)%Epsilon            = EPSILON(      DP_NI(i)%Number )
    DP_NI(i)%Huge               = HUGE(         DP_NI(i)%Number )
    DP_NI(i)%MaxExponent        = MAXEXPONENT(  DP_NI(i)%Number )
    DP_NI(i)%MinExponent        = MINEXPONENT(  DP_NI(i)%Number )
    DP_NI(i)%Precision          = PRECISION(    DP_NI(i)%Number )
    DP_NI(i)%Radix              = RADIX(        DP_NI(i)%Number )
    DP_NI(i)%Range              = RANGE(        DP_NI(i)%Number )
    DP_NI(i)%Tiny               = TINY(         DP_NI(i)%Number )
    DP_NI(i)%Exponent           = EXPONENT(     DP_NI(i)%Number )
    DP_NI(i)%Fraction           = FRACTION(     DP_NI(i)%Number )
    DP_NI(i)%mNearest           = NEAREST(      DP_NI(i)%Number, -DP_NI(i)%Number )
    DP_NI(i)%pNearest           = NEAREST(      DP_NI(i)%Number,  DP_NI(i)%Number )
    DP_NI(i)%RRSpacing          = RRSPACING(    DP_NI(i)%Number )
    DP_NI(i)%Scale_x_0          = SCALE(        DP_NI(i)%Number, 0 )
    DP_NI(i)%Set_Exponent_x_0   = SET_EXPONENT( DP_NI(i)%Number, 0 )
    DP_NI(i)%Spacing            = SPACING(      DP_NI(i)%Number )
    DP_NI(i)%Epsilon_on_Spacing = DP_NI(i)%Epsilon / DP_NI(i)%Spacing
    DP_NI(i)%Epsilon_Delta      = DP_NI(i)%Epsilon * REAL(DP_NI(i)%Radix,Double)**(DP_NI(i)%Exponent-1)
  END DO
  WRITE( *, '( /2x, "Number            = ", 10(1x,es27.20) )' ) DP_NI%Number
  WRITE( *, '(  2x, "--------------------", 10(a)         )' ) RESHAPE( (/'----------------------------'/), &
                                                                        (/ N_TEST_NUMBERS /), &
                                                                        (/'----------------------------'/))
  WRITE( *, '(  2x, "DIGITS()          = ", 10(23x,i5)     )' ) DP_NI%Digits
  WRITE( *, '(  2x, "EPSILON()         = ", 10(1x,es27.20) )' ) DP_NI%Epsilon
  WRITE( *, '(  2x, "HUGE()            = ", 10(1x,es27.20) )' ) DP_NI%Huge
  WRITE( *, '(  2x, "MAXEXPONENT()     = ", 10(23x,i5)     )' ) DP_NI%MaxExponent
  WRITE( *, '(  2x, "MINEXPONENT()     = ", 10(23x,i5)     )' ) DP_NI%MinExponent
  WRITE( *, '(  2x, "PRECISION()       = ", 10(23x,i5)     )' ) DP_NI%Precision
  WRITE( *, '(  2x, "RADIX()           = ", 10(23x,i5)     )' ) DP_NI%Radix
  WRITE( *, '(  2x, "RANGE()           = ", 10(23x,i5)     )' ) DP_NI%Range
  WRITE( *, '(  2x, "TINY()            = ", 10(1x,es27.20) )' ) DP_NI%Tiny
  WRITE( *, '(  2x, "EXPONENT()        = ", 10(23x,i5)     )' ) DP_NI%Exponent
  WRITE( *, '(  2x, "FRACTION()        = ", 10(1x,es27.20) )' ) DP_NI%Fraction
  WRITE( *, '(  2x, "NEAREST(-)        = ", 10(1x,es27.20) )' ) DP_NI%mNearest
  WRITE( *, '(  2x, "NEAREST(+)        = ", 10(1x,es27.20) )' ) DP_NI%pNearest
  WRITE( *, '(  2x, "RRSPACING()       = ", 10(1x,es27.20) )' ) DP_NI%RRSpacing
  WRITE( *, '(  2x, "SCALE(x,0)        = ", 10(1x,es27.20) )' ) DP_NI%Scale_x_0
  WRITE( *, '(  2x, "SET_EXPONENT(x,0) = ", 10(1x,es27.20) )' ) DP_NI%Set_Exponent_x_0
  WRITE( *, '(  2x, "SPACING()         = ", 10(1x,es27.20) )' ) DP_NI%Spacing
  WRITE( *, '(  2x, "EPSILON/SPACING   = ", 10(1x,es27.20) )' ) DP_NI%Epsilon_on_Spacing
  WRITE( *, '(  2x, "EPSILON_DELTA     = ", 10(1x,es27.20) )' ) DP_NI%Epsilon_Delta

END PROGRAM Numeric_Inquiry


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2004/08/31 19:25:54 $
!
! $Revision$
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Numeric_Inquiry.f90,v $
! Revision 1.3  2004/08/31 19:25:54  paulv
! - Altered code to report data separately for Single and Double kind types.
!
! Revision 1.2  2004/08/13 20:26:28  paulv
! - Fixed a bug using the NEAREST intrinsic.
!
! Revision 1.1  2003/07/03 21:40:13  paulv
! Initial checkin.
!
!
