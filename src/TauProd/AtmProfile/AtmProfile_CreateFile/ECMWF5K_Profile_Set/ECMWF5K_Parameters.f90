!
! ECMWF5K_Parameters
!
! Module containing the ECMWF5K atmospheric profile set parameters
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 03-Aug-2009
!                       paul.vandelst@noaa.gov

MODULE ECMWF5K_Parameters

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds      , ONLY: fp
  ! Disable implicit typing
  IMPLICIT NONE
  
  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: ECMWF5K_ASCII_FILE
  PUBLIC :: ECMWF5K_FMT_STRING
  PUBLIC :: ECMWF5K_BINARY_FILE
  PUBLIC :: ECMWF5K_RECLEN
  PUBLIC :: N_ECMWF5K_LEVELS
  PUBLIC :: N_ECMWF5K_LAYERS
  PUBLIC :: N_ECMWF5K_ABSORBERS
  PUBLIC :: N_ECMWF5K_PROFILES
  PUBLIC :: ECMWF5K_ABSORBER_ID
  PUBLIC :: ECMWF5K_ABSORBER_UNITS_ID
  PUBLIC :: H2O_ID
  PUBLIC :: O3_ID 

  
  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &

  ! ASCII data file info
  CHARACTER(*), PARAMETER :: ECMWF5K_ASCII_FILE = 'nwp_saf_t_sampled.atm'  
  CHARACTER(*), PARAMETER :: ECMWF5K_FMT_STRING = '(833(e13.6,x),i5,3i3,i7,i5)'
  
  ! Binary data file info
  CHARACTER(*), PARAMETER :: ECMWF5K_BINARY_FILE = ECMWF5K_ASCII_FILE//'.bin'
  INTEGER,      PARAMETER :: ECMWF5K_RECLEN = 6688 ! bytes
  
  ! Dimension info
  INTEGER, PARAMETER :: N_ECMWF5K_LEVELS    = 91
  INTEGER, PARAMETER :: N_ECMWF5K_LAYERS    = N_ECMWF5K_LEVELS - 1
  INTEGER, PARAMETER :: N_ECMWF5K_ABSORBERS = 2
  INTEGER, PARAMETER :: N_ECMWF5K_PROFILES  = 5000

  ! Absorber info
  ! ...Absorber ids
  INTEGER, PARAMETER :: H2O_ID = 1
  INTEGER, PARAMETER :: O3_ID  = 3
  INTEGER, PARAMETER :: ECMWF5K_ABSORBER_ID(N_ECMWF5K_ABSORBERS) = &
    (/ H2O_ID, O3_ID /)
  ! ...Absorber units ids
  INTEGER, PARAMETER :: VOLUME_MIXING_RATIO_UNITS = 1
  INTEGER, PARAMETER :: MASS_MIXING_RATIO_UNITS   = 3
  INTEGER, PARAMETER :: ECMWF5K_ABSORBER_UNITS_ID(N_ECMWF5K_ABSORBERS) = &
    (/ MASS_MIXING_RATIO_UNITS, VOLUME_MIXING_RATIO_UNITS /)

END MODULE ECMWF5K_Parameters
