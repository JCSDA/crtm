!------------------------------------------------------------------------------
!M+
! NAME:
!       parameters
!
! PURPOSE:
!       Module to hold parameter constants
!
! CATEGORY:
!       NCEP RTM
!
! CALLING SEQUENCE:
!       USE parameters
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CIRA/NESDIS/JCSDA 10-Jan-2008
!                       Yong.Chen@noaa.gov
!
!
!  Copyright (C) 2008 Paul van Delst
!
!M-
!------------------------------------------------------------------------------

MODULE Parameters

  ! ---------------------
  ! Module use statements
  ! ---------------------

  USE  Type_Kinds, ONLY : fp_kind


  ! ------------------
  ! Default visibility
  ! ------------------

  PRIVATE

  ! ----------------------------
  !   modlecule set list
  ! ----------------------------
  ! -------------------------------
  ! The molecular set specification
  ! -------------------------------

  ! -- The number of "molecular sets" recognised
  INTEGER, PUBLIC, PARAMETER :: N_MOLECULE_SETS = 23

  ! -- The names of the allowed molecular sets.
  ! -- These values are used in filenames and
  ! -- other transmittance production program inputs
  CHARACTER( * ), PUBLIC, PARAMETER, DIMENSION( N_MOLECULE_SETS ) :: MOLECULE_SET_TAG = &
    (/ 'mol1            ', &  !   1
       'mol2            ', &  !   2
       'mol3            ', &  !   3
       'mol4            ', &  !   4
       'mol5            ', &  !   5
       'mol6            ', &  !   6
       'mol7            ', &  !   7
       'all_nocontinuum ', &  !   8
       'continua_only   ', &  !   9
       'all_withcontinua', &  !  10
       'wvo             ', &  !  11
       'wet             ', &  !  12
       'dry             ', &  !  13
       'ozo             ', &  !  14
       'wco             ', &  !  15
       'effective_mol1  ', &  ! 101
       'effective_wet   ', &  ! 112
       'effective_dry   ', &  ! 113
       'effective_ozo   ', &  ! 114
       'effective_co    ', &  ! 115
       'effective_ch4   ', &  ! 116 
       'effective_co2   ', &  ! 117 
       'effective_n2o   ' /)  ! 118 

  ! -- The ID values associated with the allowed moleculer sets
  INTEGER, PUBLIC, PARAMETER, DIMENSION( N_MOLECULE_SETS ) :: MOLECULE_SET_TAG_ID = &
    (/  1, &  !  mol1
        2, &  !  mol2
        3, &  !  mol3
        4, &  !  mol4
        5, &  !  mol5
        6, &  !  mol6
        7, &  !  mol7
        8, &  !  all_nocontinuum
        9, &  !  continua_only
       10, &  !  all_withcontinua
       11, &  !  wvo
       12, &  !  wet
       13, &  !  dry
       14, &  !  ozo
       15, &  !  wco
      101, &  !  effective_wet (line only)
      112, &  !  effective_wet 
      113, &  !  effective_dry
      114, &  !  effective_ozo
      115, &  !  effective_co 
      116, &  !  effective_ch4
      117, &  !  effective_co2
      118 /)  !  effective_n2o

  
  ! ------------------------------------------------------------
  ! The absorber IDs. Use HITRAN definitions
  ! -----------------------------------------------------------
  INTEGER, PUBLIC, PARAMETER :: H2O_ID  = 1 
  INTEGER, PUBLIC, PARAMETER :: CO2_ID  = 2 
  INTEGER, PUBLIC, PARAMETER :: O3_ID   = 3 
  INTEGER, PUBLIC, PARAMETER :: N2O_ID  = 4 
  INTEGER, PUBLIC, PARAMETER :: CO_ID   = 5 
  INTEGER, PUBLIC, PARAMETER :: CH4_ID  = 6 
 
  ! -- Define the secant of the zenith angles to be used
  INTEGER, PUBLIC,PARAMETER :: N_ZENITH_ANGLES = 7
  


  ! --------------------------------------
  ! Number of absorber layers in algorithm
  ! --------------------------------------

  INTEGER, PUBLIC, PARAMETER :: MAX_N_ABSORBER_LAYERS = 300

 ! ----------------------------------------------------------
 ! Cosmic background temperature. Taken from
 ! Numerical limits based on experiment.
  REAL(fp_kind), PUBLIC, PARAMETER :: LIMIT_EXP = 20.0_fp_kind
  REAL(fp_kind), PUBLIC, PARAMETER :: LIMIT_LOG = 4.8e+08_fp_kind   ! EXP( LIMIT_EXP )


  ! --------------------
  ! Numerical parameters
  ! --------------------

  ! -- Numbers
  REAL( fp_kind ), PUBLIC, PARAMETER :: ZERO      = 0.0_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: ONE       = 1.0_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: TWO       = 2.0_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: THREE     = 3.0_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: FOUR      = 4.0_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: POINT_25   = 0.25_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: POINT_5   = 0.5_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: POINT_75  = 0.75_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: ONE_POINT_5   = 1.5_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: ONE_POINT_25  = 1.25_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: ONE_POINT_75  = 1.75_fp_kind

  ! -- Precision/tolerance
  REAL( fp_kind ), PUBLIC, PARAMETER :: TOLERANCE = EPSILON( ONE )
  REAL( fp_kind ), PUBLIC, PARAMETER :: INFINITE  = HUGE( ONE )

  ! -- Constant to allow degrees->radians conversion
  REAL( fp_kind ), PUBLIC, PARAMETER :: PI = 3.14159265358979323_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: DEGREES_TO_RADIANS = PI / 180.0_fp_kind

  ! -- Top-Of-Atmosphere pressure in hPa
  REAL( fp_kind ), PUBLIC, PARAMETER :: TOA_PRESSURE = 0.005_fp_kind

  ! -- Reciprocal gravity (scaled by 100 for use with pressure in hPa)
  REAL( fp_kind ), PUBLIC, PARAMETER :: RECIPROCAL_GRAVITY = ONE / 980.665_fp_kind

  ! -- Diffusivity angle secant = ACOS( 3/5 ) in degrees (~53.13)
  REAL( fp_kind ), PUBLIC, PARAMETER :: SECANT_DIFFUSIVITY_ANGLE = 5.0_fp_kind / THREE

  ! -- Maximum solar zenith angle secant definition. Should be determined
  ! -- by the maximum angle secant used in generating the transmittance
  ! -- model coefficients, i.e. a secant of 2.25 => 63.6deg. Users have
  ! -- requested the value be 85deg => secant of ~11.47.
  REAL( fp_kind ), PUBLIC, PARAMETER :: MAX_SOLAR_ANGLE = 85.0_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: MAX_SECANT_SOLAR_ANGLE = 11.473711738554476_fp_kind


  ! Minimum component and total tau, below which the transmittance is considered
  ! insignificant. Maximum optical path and minimum layer optical depth 
  REAL( fp_kind ), PUBLIC, PARAMETER :: TAU_MIN  = 1.0e-10_fp_kind 
  REAL( fp_kind ), PUBLIC, PARAMETER :: TOTAL_TAU_MIN  = 1.0e-5_fp_kind 
  REAL( fp_kind ), PUBLIC, PARAMETER :: PATH_MAX = 7.0_fp_kind   
  REAL( fp_kind ), PUBLIC, PARAMETER :: OD_MIN = 0.000001_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: OD_MAX = 20.0_fp_kind

  REAL( fp_kind ), PUBLIC, PARAMETER :: RMISS = -2**15 * ONE
  REAL( fp_kind ), PUBLIC, PARAMETER :: HMISS = RMISS / TWO

  REAL( fp_kind ), PUBLIC, PARAMETER :: VirtEmiss_mw  = 0.0_fp_kind   ! MW virtial emissivity
  REAL( fp_kind ), PUBLIC, PARAMETER :: VirtEmiss_ir  = 0.98_fp_kind  ! IR virtial emissivity
  LOGICAL, PUBLIC, PARAMETER         :: Tb_OPTIMAL = .true.

  ! Interferometer WMO_Sensor_IDs
  INTEGER, PUBLIC, PARAMETER :: INTERFEROMETER_LIST(2) = (/221, 620/)  !IASI and CrIS 
  
END MODULE Parameters

 
