!
! SpcCoeff_Parameters
!
! Module to hold parameter definitions used in SpcCoeff applications
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 03-Oct-2007
!                       paul.vandelst@noaa.gov
!

MODULE SpcCoeff_Parameters

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds, ONLY: fp
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  
  
  ! -----------------
  ! Module parameters
  ! -----------------
  ! The following scaling factors are applied to produce radiances in units
  ! of mW/(m^2.sr.cm^-1) when they are used.
  !
  ! First Planck function constant (C_1) scale factors. Units of C_1 are W.m^2.
  ! Length scaling: To convert to W/(m^2.cm^-4) requires a scaling of m->cm,
  !                 which is 100, to the fourth power, which is 1.0e+08.
  ! Power scaling:  To convert to mW.m^2 requires a scaling of 1000.
  REAL(fp), PUBLIC, PARAMETER :: C_1_LENGTH_SCALE_FACTOR = 1.0e+08_fp
  REAL(fp), PUBLIC, PARAMETER :: C_1_POWER_SCALE_FACTOR  = 1.0e+03_fp
  REAL(fp), PUBLIC, PARAMETER :: C_1_SCALE_FACTOR = C_1_LENGTH_SCALE_FACTOR * &
                                                    C_1_POWER_SCALE_FACTOR
  ! Second Planck function constant (C_2) scale factor. Units of C_2 are K.m,
  ! So to convert to K.cm, a scaling of 100 is applied.
  REAL(fp), PUBLIC, PARAMETER :: C_2_SCALE_FACTOR = 100.0_fp

  ! The number and range of temperatures used in determining the 
  ! polychromatic correction coefficients
  INTEGER,  PUBLIC, PARAMETER :: N_TEMPERATURES = 17
  REAL(fp), PUBLIC, PARAMETER :: MIN_TEMPERATURE = 180.0_fp
  REAL(fp), PUBLIC, PARAMETER :: MAX_TEMPERATURE = 340.0_fp

  ! Solar channel cut-off frequency
  REAL(fp), PUBLIC, PARAMETER :: SOLAR_CUTOFF_WAVENUMBER = 1800.0_fp

  ! Integration methods
  INTEGER, PUBLIC, PARAMETER :: N_INTEGRATION_METHODS = 2
  INTEGER, PUBLIC, PARAMETER :: SUMMATION_METHOD = 1
  INTEGER, PUBLIC, PARAMETER :: INTEGRATE_METHOD = 2
  INTEGER, PUBLIC, PARAMETER :: INTEGRATION_METHOD(N_INTEGRATION_METHODS) = &
    (/ SUMMATION_METHOD, &
       INTEGRATE_METHOD /)
  CHARACTER(*), PUBLIC, PARAMETER :: INTEGRATION_METHOD_NAME(N_INTEGRATION_METHODS) = &
    (/ 'Summation  ', &
       'Integration' /)

  ! Interpolation order
  INTEGER, PUBLIC, PARAMETER :: LINEAR_ORDER = 1
  INTEGER, PUBLIC, PARAMETER ::  CUBIC_ORDER = 3

END MODULE SpcCoeff_Parameters
