!
! Apodisation_Utility
!
! Module containing apodisation function routines.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 31-Oct-2006
!                       paul.vandelst@noaa.gov
!

MODULE Apodisation_Utility

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds           , ONLY: fp
  USE Fundamental_Constants, ONLY: PI
  USE SPC_IFG_Utility      , ONLY: ComputeNSPC, &
                                   ComputeMeanDelta
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Visibility
  ! ----------
  ! Everything is default private
  PRIVATE
  ! Parameters
  PUBLIC :: BARTLETT
  PUBLIC :: WELCH
  PUBLIC :: CONNES
  PUBLIC :: COSINE
  PUBLIC :: HAMMING
  PUBLIC :: HANNING
  PUBLIC :: NORTONBEER_WEAK
  PUBLIC :: NORTONBEER_MEDIUM
  PUBLIC :: NORTONBEER_STRONG
  PUBLIC :: BLACKMANHARRIS_3
  PUBLIC :: BLACKMANHARRIS_4
  PUBLIC :: BLACKMANHARRIS_4M
  PUBLIC :: BEER
  PUBLIC :: STRONGBEER
  PUBLIC :: HAPPGENZEL
  ! Procedures
  PUBLIC :: Apodisation_Function
  PUBLIC :: Apodisation_Utility_Version


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO      = 0.0_fp
  REAL(fp), PARAMETER :: POINT5    = 0.5_fp
  REAL(fp), PARAMETER :: ONE       = 1.0_fp
  REAL(fp), PARAMETER :: ONEPOINT5 = 1.5_fp
  REAL(fp), PARAMETER :: TWO       = 2.0_fp
  REAL(fp), PARAMETER :: TEN       = 10.0_fp
  ! Apodisation function coefficients
  !...Hamming apodfn coefficients
  REAL(fp), PARAMETER :: POINT46   = 0.46_fp
  REAL(fp), PARAMETER :: POINT54   = 0.54_fp
  !...Norton-Beer apodfn coefficients
  REAL(fp), PARAMETER :: NBC_WEAK(0:2)   = (/ 0.384093_fp, -0.087577_fp, 0.703484_fp /)
  REAL(fp), PARAMETER :: NBC_MEDIUM(0:2) = (/ 0.152442_fp, -0.136176_fp, 0.983734_fp /)
  REAL(fp), PARAMETER :: NBC_STRONG(0:3) = (/ 0.045335_fp,  ZERO       , 0.554883_fp, 0.399782_fp /)
  !...Blackman-Harris apodfn coefficients
  REAL(fp), PARAMETER :: BHC_3(0:2)  = (/ 0.42323_fp, 0.49755_fp, 0.07922_fp /)
  REAL(fp), PARAMETER :: BHC_4(0:3)  = (/ 0.35875_fp, 0.48829_fp, 0.14128_fp, 0.01168_fp /)
  REAL(fp), PARAMETER :: BHC_4M(0:3) = (/ 0.355766_fp, 0.487395_fp, 0.144234_fp, 0.012605_fp /)
  ! Apodisation function type values
  INTEGER,  PARAMETER :: BARTLETT          = 1
  INTEGER,  PARAMETER :: WELCH             = 2
  INTEGER,  PARAMETER :: CONNES            = 3
  INTEGER,  PARAMETER :: COSINE            = 4
  INTEGER,  PARAMETER :: HAMMING           = 5
  INTEGER,  PARAMETER :: HANNING           = 6
  INTEGER,  PARAMETER :: NORTONBEER_WEAK   = 7
  INTEGER,  PARAMETER :: NORTONBEER_MEDIUM = 8
  INTEGER,  PARAMETER :: NORTONBEER_STRONG = 9
  INTEGER,  PARAMETER :: BLACKMANHARRIS_3  = 10
  INTEGER,  PARAMETER :: BLACKMANHARRIS_4  = 11
  INTEGER,  PARAMETER :: BLACKMANHARRIS_4M = 12
  INTEGER,  PARAMETER :: BEER       = WELCH
  INTEGER,  PARAMETER :: STRONGBEER = CONNES
  INTEGER,  PARAMETER :: HAPPGENZEL = HAMMING


CONTAINS


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                           ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Apodisation_Function
!
! PURPOSE:
!       Pure function to compute various apodisation functions.
!
! CALLING SEQUENCE:
!       y = Apodisation_Function(x, aType=aType, xMax=xMax)
!
! INPUTS:
!       x:      The regularly-spaced optical delay values for which the
!               apodisation function is to be computed.
!               UNITS:      centimetres.
!               TYPE:       REAL(fp)
!               DIMENSION:  Rank-1
!               ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       aType:  Set this argument to the defined parameter values to
!               select the type of apodisation function.
!               If == BARTLETT          for Bartlett apodisation
!                  == WELCH             for Welch apodisation
!                  == CONNES            for Connes apodisation (DEFAULT)
!                  == COSINE            for Cosine apodisation
!                  == HAMMING           for Hamming apodisation
!                  == HANNING           for Hanning apodisation
!                  == NORTONBEER_WEAK   for weak Norton-Beer apodisation
!                  == NORTONBEER_MEDIUM for medium Norton-Beer apodisation
!                  == NORTONBEER_STRONG for strong Norton-Beer apodisation
!                  == BLACKMANHARRIS_3  for Blackman-Harris 3-term
!                  == BLACKMANHARRIS_4  for Blackman-Harris 4-term
!                  == BLACKMANHARRIS_4M for Blackman-Harris modified 4-term
!                  == BEER        alias for WELCH
!                  == STRONGBEER  alias for CONNES
!                  == HAPPGENZEL  alias for HAMMING
!               UNITS:      N/A
!               TYPE:       INTEGER
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       xMax:   The maximum optical delay to use in calculating the
!               apodisation function.
!               - For x > xMax, the apodisation function is set to zero.
!               - If not specified MAXVAL(ABS(x)) is used.
!               UNITS:      centimetres
!               TYPE:       REAL(fp)
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       y:      Apodisation function.
!               UNITS:      N/A
!               TYPE:       REAL(fp)
!               DIMENSION:  Same as input x.
!
! COMMENTS:
!       The formulae for the above apodisation functions are taken from:
!
!         Weisstein, Eric W. "Apodization Function."
!         From MathWorld--A Wolfram Web Resource.
!         http://mathworld.wolfram.com/ApodizationFunction.html
!
!       and
!
!         Naylor,D.A. and M.K. Tahic, "Apodizing functions for Fourier
!         transform spectroscopy, J.Opt.Soc.Am.A 24(11):3644-3648, 2007
!
!:sdoc-:
!--------------------------------------------------------------------------------

  PURE FUNCTION Apodisation_Function( &
    x    , &
    aType, &
    xMax ) &
  RESULT(y)
    ! Arguments
    REAL(fp),           INTENT(IN) :: x(:)
    INTEGER,  OPTIONAL, INTENT(IN) :: aType
    REAL(fp), OPTIONAL, INTENT(IN) :: xMax
    ! Function result
    REAL(fp) :: y(SIZE(x))
    ! Local variables
    INTEGER  :: local_aType
    REAL(fp) :: local_xMax
    REAL(fp) :: tolerance
    REAL(fp) :: xnorm(SIZE(x))
    INTEGER :: i, n
    REAL(fp) :: nbc(0:3), bhc(0:3)


    ! Set the apodisation function type
    local_aType = -1 ! Force default
    IF ( PRESENT(aType) ) local_aType = aType


    ! Set the maximum optical path delay
    IF ( PRESENT(xMax) ) THEN
      local_xMax = ABS(xMax)
    ELSE
      local_xMax = MAXVAL(ABS(x))
    END IF


    ! Compute the normalised optical path delay
    xNorm = ABS(x)/local_xMax


    ! Compute apodisation function for +ve delays. The default
    ! apodisation function is CONNES
    SELECT CASE(local_aType)
      !...Some standard functions
      CASE(BARTLETT); y = ONE - xNorm
      CASE(WELCH)   ; y = ONE - xNorm**2
      CASE(COSINE)  ; y = COS(POINT5*PI*xNorm)
      CASE(HAMMING) ; y = POINT54 + (POINT46*COS(PI*xNorm))
      CASE(HANNING) ; y = POINT5*(ONE + COS(PI*xNorm))

      !...Norton-Beer series
      CASE(NORTONBEER_WEAK, NORTONBEER_MEDIUM, NORTONBEER_STRONG)
        SELECT CASE(local_aType)
          CASE(NORTONBEER_WEAK)  ; n = 2; nbc(0:n) = NBC_WEAK(0:n)
          CASE(NORTONBEER_MEDIUM); n = 2; nbc(0:n) = NBC_MEDIUM(0:n)
          CASE(NORTONBEER_STRONG); n = 3; nbc(0:n) = NBC_STRONG(0:n)
        END SELECT
        y = nbc(0)
        DO i = 1, n
          y = y + nbc(i)*(ONE - xNorm**2)**i
        END DO

      !...Blackman-Harris series
      CASE(BLACKMANHARRIS_3, BLACKMANHARRIS_4, BLACKMANHARRIS_4M)
        SELECT CASE(local_aType)
          CASE(BLACKMANHARRIS_3) ; n = 2; bhc(0:n) = BHC_3(0:n)
          CASE(BLACKMANHARRIS_4) ; n = 3; bhc(0:n) = BHC_4(0:n)
          CASE(BLACKMANHARRIS_4M); n = 3; bhc(0:n) = BHC_4M(0:n)
        END SELECT
        y = bhc(0)
        DO i = 1, n
          y = y + bhc(i)*COS(REAL(i,fp)*PI*xNorm)
        END DO

      !...Default is CONNES
      CASE DEFAULT  ; y = (ONE - xNorm**2)**2
    END SELECT


    ! Set out-of-bounds apodisation to zero
    tolerance = ComputeMeanDelta(x)/TEN
    WHERE ( ABS(x) > local_xMax+tolerance ) y = ZERO

  END FUNCTION Apodisation_Function


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Apodisation_Utility_Version
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL Apodisation_Utility_Version( Id )
!
! OUTPUTS:
!       Id:   Character string containing the version Id information
!             for the module.
!             UNITS:      N/A
!             TYPE:       CHARACTER(*)
!             DIMENSION:  Scalar
!             ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Apodisation_Utility_Version( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE Apodisation_Utility_Version


END MODULE Apodisation_Utility
