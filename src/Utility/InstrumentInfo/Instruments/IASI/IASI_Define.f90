!
! IASI_Define
!
! Module containing IASI instrument definitions
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 08-Dec-2006
!                       paul.vandelst@ssec.wisc.edu
!

MODULE IASI_Define

  ! ----------------
  ! Enviroment setup
  ! ----------------
  ! Module usage
  USE Type_Kinds,            ONLY: fp=>fp_kind
  USE Fundamental_Constants, ONLY: PI
  ! Disable implicit typing
  IMPLICIT NONE
  
  ! ----------
  ! Visibility
  ! ----------
  ! Everything is default private
  PRIVATE
  ! Public module procedures
  PUBLIC :: IASI_MaxX
  PUBLIC :: IASI_X
  PUBLIC :: IASI_GFT
  
  
  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id: $'
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO    = 0.0_fp
  REAL(fp), PARAMETER :: POINT5  = 0.5_fp
  REAL(fp), PARAMETER :: ONE     = 1.0_fp
  REAL(fp), PARAMETER :: TWO     = 2.0_fp
  REAL(fp), PARAMETER :: HUNDRED = 100.0_fp
  REAL(fp), PARAMETER :: M2CM    = HUNDRED
  REAL(fp), PARAMETER :: LN2 = 0.693147180559945309417232_fp
  
  ! Gaussian function FWHM (cm^-1)
  REAL(fp), PARAMETER :: GFT_FWHM = POINT5
  REAL(fp), PARAMETER :: GFT_HWHM = GFT_FWHM/TWO

  ! Laser wavelength (m)
  REAL(fp), PARAMETER :: LASER_WAVELENGTH       = 1.537656349e-06_fp
  REAL(fp), PARAMETER :: LASER_WAVELENGTH_IN_CM = LASER_WAVELENGTH*M2CM

  ! Laser frequency (m^-1)
  REAL(fp), PARAMETER :: LASER_FREQUENCY   = ONE/LASER_WAVELENGTH
  REAL(fp), PARAMETER :: NYQUIST_FREQUENCY = LASER_FREQUENCY/TWO

  ! Field angle (rad)
  REAL(fp), PARAMETER :: FIELD_ANGLE = 0.01605073_fp

  ! Number of double-sided FFT points
  INTEGER,  PARAMETER :: NFFT = 51200

  ! Nominal maximum optical path delay for NFFT (m)
  REAL(fp), PARAMETER :: NOMINAL_MAXX       = 1.9679466e-02_fp
  REAL(fp), PARAMETER :: NOMINAL_MAXX_IN_CM = NOMINAL_MAXX*M2CM


CONTAINS


  ! Function to compute the maximum optical path delay in cm.
  FUNCTION IASI_MaxX(n) RESULT(maxX)
    INTEGER, INTENT(IN) :: n
    REAL(fp) :: maxX
    maxX = REAL((n/2),fp)*(LASER_WAVELENGTH_IN_CM/TWO)*COS(FIELD_ANGLE)
  END FUNCTION IASI_MaxX


  ! Function to compute the optical path delay grid in cm.
  FUNCTION IASI_X(n) RESULT(X)
    ! Arguments
    INTEGER, INTENT(IN) :: n
    ! Function result
    REAL(fp) :: X(n)
    ! Local variables
    REAL(fp) :: maxX
    INTEGER :: i
    
    maxX = IASI_MaxX(n)
    X = (/(REAL(i,fp),i=0,n-1)/)/REAL(n-1,fp)
    X = (X * TWO*maxX) - maxX
  END FUNCTION IASI_X


  ! Function to compute the IASI apodisation function
  FUNCTION IASI_GFT(x) RESULT(gft)
    ! Arguments
    REAL(fp), INTENT(IN) :: x(:)
    ! Function result
    REAL(fp) :: gft(SIZE(x))
    ! Local variables
    INTEGER  :: n
    REAL(fp) :: maxX
    REAL(fp) :: sigma

    ! Compute the IASI GFT
    sigma = LN2/(PI*GFT_HWHM)
    WHERE ( ABS(x) <= NOMINAL_MAXX_IN_CM )
      gft = EXP(-LN2*(x/sigma)**2)
    ELSEWHERE
      gft = ZERO
    END WHERE
  END FUNCTION IASI_GFT

END MODULE IASI_Define
