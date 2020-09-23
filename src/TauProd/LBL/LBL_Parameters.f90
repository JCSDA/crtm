!
! LBL_Parameters
!
! Module containing line-by-line model parameters
!
!
! CREATED: 27-Jul-2012
!

MODULE LBL_Parameters

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds, ONLY: fp, Double
  ! Disable implicit typing
  IMPLICIT NONE
  ! Default visibility
  PRIVATE


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &


  ! -----------------
  ! Public parameters
  ! -----------------
  ! Literal constants
  REAL(fp),PARAMETER, PUBLIC :: ZERO = 0.0_fp
  REAL(fp),PARAMETER, PUBLIC :: ONE  = 1.0_fp
  ! LBL parameters
  INTEGER, PARAMETER, PUBLIC :: MIN_N_ABSORBERS =  7        ! Minimum number of gaseous absorbers
  INTEGER, PARAMETER, PUBLIC :: MAX_N_ABSORBERS = 32        ! Maximum number of gaseous absorbers
  INTEGER, PARAMETER, PUBLIC :: MAX_N_XSECT_ABSORBERS = 2   ! Number of cross-section absorbers
  INTEGER, PARAMETER, PUBLIC :: MAX_IBMAX = 200             ! Maximum number of calculation layer boundaries
  INTEGER, PARAMETER, PUBLIC :: MAX_IMMAX = 3400            ! Maximum number of input layer boundaries

END MODULE LBL_Parameters
