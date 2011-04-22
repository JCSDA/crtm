!
! Tau_Production_Parameters_new
!
! Module defining parameters used in the LBL transmittance production runs.
! This module is mostly used for the spectral convolution code. The Fourier
! transform code uses instrument specific parameter modules. 
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Apr-2002
!                       paul.vandelst@ssec.wisc.edu
!
!
!       Modified by:    Yong Chen, CIRA/JCSDA 22-Sep-2008
!                       Yong.Chen@noaa.gov
!

MODULE Tau_Production_Parameters

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds       , ONLY: fp
  USE LBLRTM_Parameters, ONLY: LBLRTM_FILE_TYPE => LBLRTM_SINGLE_PANEL_TYPE
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything is default private
  PRIVATE
  ! Inherited visibility
  PUBLIC :: LBLRTM_FILE_TYPE


  ! -------------------------------
  ! Atmospheric profile information
  ! -------------------------------
  ! The number of recognised profile sets
  INTEGER, PUBLIC, PARAMETER :: N_PROFILE_SETS = 6

  ! The profile set ID tags
  CHARACTER(*), PUBLIC, PARAMETER, DIMENSION(N_PROFILE_SETS) :: &
    PROFILE_SET_ID_TAG = (/ 'UMBC         ', &
                            'CIMSS        ', &
                            'ECMWF        ', &
                            'UMBC_extended', &
                            'ECMWF83      ', &
                            'ECMWF42      ' /)

  ! The beginning profile is always 1 regardless
  ! of the selected profile set. It's an array
  ! for conformance only.
  INTEGER, PUBLIC, PARAMETER, DIMENSION(N_PROFILE_SETS) :: &
    PROFILE_BEGIN = (/ 1, & ! For UMBC set
                       1, & ! For CIMSS set
                       1, & ! For ECMWF set
                       1, & ! For UMBC extend set (inculding CO2 profile)  
                       1, & ! For ECMWF extend set (inculding CO2 profile) 
                       1 /) ! For ECMWF extend set (inculding CO2 profile)
  ! The end profile is set dependent
  INTEGER, PUBLIC, PARAMETER, DIMENSION(N_PROFILE_SETS) :: &
    PROFILE_END = (/ 48, & ! For UMBC set
                     32, & ! For CIMSS set
                     52, & ! For ECMWF set
                     48, & ! For UMBC extend set (inculding CO2 profile)
                     83, & ! For ECMWF extend set (inculding CO2 profile)
                     42 /) ! For ECMWF extend set (inculding CO2 profile)

  ! The number of profiles in each set.
  ! This parameter is superfluous (always the same
  ! as PROFILE_END), but more intuitive in code.
  INTEGER, PUBLIC, PARAMETER, DIMENSION(N_PROFILE_SETS) :: &
    N_PROFILES = PROFILE_END - PROFILE_BEGIN + 1

  ! The number of calculation levels
  INTEGER, PUBLIC, PARAMETER :: N_LEVELS = 101
  INTEGER, PUBLIC, PARAMETER :: N_LAYERS = 100

  ! The standard AIRS pressure levels, SRC->TOA
  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_LEVELS) :: &
    LEVEL_PRESSURE = (/ 1100.000000_fp, 1070.916940_fp, 1042.231940_fp, 1013.947655_fp, &
                         986.066601_fp,  958.591154_fp,  931.523549_fp,  904.865880_fp, &
                         878.620096_fp,  852.788003_fp,  827.371259_fp,  802.371376_fp, &
                         777.789716_fp,  753.627494_fp,  729.885772_fp,  706.565460_fp, &
                         683.667316_fp,  661.191946_fp,  639.139797_fp,  617.511163_fp, &
                         596.306182_fp,  575.524832_fp,  555.166935_fp,  535.232153_fp, &
                         515.719989_fp,  496.629785_fp,  477.960722_fp,  459.711821_fp, &
                         441.881941_fp,  424.469776_fp,  407.473861_fp,  390.892566_fp, &
                         374.724098_fp,  358.966503_fp,  343.617659_fp,  328.675286_fp, &
                         314.136936_fp,  300.000000_fp,  286.261706_fp,  272.919120_fp, &
                         259.969142_fp,  247.408514_fp,  235.233814_fp,  223.441461_fp, &
                         212.027712_fp,  200.988665_fp,  190.320260_fp,  180.018279_fp, &
                         170.078348_fp,  160.495939_fp,  151.266366_fp,  142.384796_fp, &
                         133.846240_fp,  125.645562_fp,  117.777481_fp,  110.236565_fp, &
                         103.017244_fp,   96.113803_fp,   89.520390_fp,   83.231016_fp, &
                          77.239560_fp,   71.539768_fp,   66.125259_fp,   60.989530_fp, &
                          56.125953_fp,   51.527786_fp,   47.188171_fp,   43.100144_fp, &
                          39.256633_fp,   35.650467_fp,   32.274378_fp,   29.121009_fp, &
                          26.182918_fp,   23.452583_fp,   20.922408_fp,   18.584732_fp, &
                          16.431833_fp,   14.455936_fp,   12.649223_fp,   11.003835_fp, &
                           9.511889_fp,    8.165480_fp,    6.956695_fp,    5.877623_fp, &
                           4.920362_fp,    4.077038_fp,    3.339812_fp,    2.700897_fp, &
                           2.152573_fp,    1.687200_fp,    1.297240_fp,    0.975274_fp, &
                           0.714023_fp,    0.506374_fp,    0.345404_fp,    0.224412_fp, &
                           0.136954_fp,    0.076879_fp,    0.038383_fp,    0.016065_fp, &
                           0.005000_fp /)

  ! Default CO2 mixing ratio in ppmv
  REAL(fp), PUBLIC, PARAMETER :: CO2_MIXING_RATIO = 380.0_fp


  ! ------------------------------------
  ! The frequency calculation parameters
  ! ------------------------------------
  ! The begin and end frequencies
  REAL(fp), PUBLIC, PARAMETER :: FREQUENCY_BEGIN =  500.0_fp
  REAL(fp), PUBLIC, PARAMETER :: FREQUENCY_END   = 3500.0_fp

  ! The number of recognised frequency intervals
  INTEGER, PUBLIC, PARAMETER :: N_FREQUENCY_INTERVALS = 3

  ! The allowed frequency intervals. These are the values
  ! of the frequency spacing of the line-by-line (LBL)
  ! band outputs. The lower resolution interval is for
  ! broadband instruments, the other for high resolution
  ! instruments.
  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY_INTERVALS) :: &
    FREQUENCY_INTERVAL = (/ 0.1000_fp, &
                            0.0025_fp, & ! for AIRS
                            0.001_fp /)  ! for IASI,CrIS

  ! The frequency deltas for a band.
  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY_INTERVALS) :: &
    FREQUENCY_DELTA = (/ 250.0_fp, &  ! For 0.1cm^-1 spacing
                          25.0_fp, &  ! For 0.0025cm^-1 spacing
                         900.0_fp /)  ! For 0.001cm^-1 spacing
  ! The frequency bandwidths. These are the bandwidths
  ! of the line-by-line (LBL) band outputs.
  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY_INTERVALS) :: &
    FREQUENCY_BANDWIDTH = FREQUENCY_DELTA - FREQUENCY_INTERVAL

  ! The number of points in each band.
  ! The values are determined using
  !   N_FREQUENCIES = INT( FREQUENCY_BANDWIDTH / FREQUENCY_INTERVAL ) + 1
  INTEGER, PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY_INTERVALS) :: &
    N_FREQUENCIES = (/ 2500, & ! For 0.1cm^-1 spacing
                      10000, & ! For 0.0025cm^-1 spacing
                     900000 /) ! For 0.001cm^-1 spacing

  ! ----------------------------------------------------
  ! The line-by-line (LBL) band values.
  ! An LBL "band" corresponds to the smallest contiguous
  ! spectral computation performed by the LBL code. The
  ! width of an LBL band is determined by the frequency
  ! bandwidth defined above.
  ! ----------------------------------------------------
  ! The beginning is always 1 regardless
  ! of the selected frequency interval.
  ! It's an array for conformance only.
  INTEGER, PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY_INTERVALS) :: &
    LBLBAND_BEGIN = (/ 1, & ! For 0.1cm^-1 spacing
                       1, & ! For 0.0025cm^-1 spacing
                       1 /) ! For 0.001cm^-1 spacing
  ! The end is frequecy interval dependent
  ! The values are determined using
  !   LBLBAND_END = INT((FREQUENCY_BEGIN-FREQUENCY_END)/FREQUENCY_BANDWIDTH)+1
  INTEGER, PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY_INTERVALS) :: &
    LBLBAND_END = (/ 13, &  ! For 0.1cm^-1 spacing
                    121, &  ! For 0.0025cm^-1 spacing
                      3 /)  ! For 0.001cm^-1 spacing
  ! The number of LBL bands for each
  ! frequency interval
  INTEGER, PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY_INTERVALS) :: &
    N_LBLBANDS = LBLBAND_END - LBLBAND_BEGIN + 1
  
  ! The maximum number of LBL bands than
  ! can be read in at one time
  INTEGER, PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY_INTERVALS) :: &
    MAX_N_LBLBANDS = (/ 6, & ! For 0.1cm^-1 spacing
                       10, & ! For 0.0025cm^-1 spacing
                        1 /) ! For 0.001cm^-1 spacing

  ! -------------------------------
  ! The molecular set specification
  ! -------------------------------
  ! The number of "molecular sets" recognised
  INTEGER, PUBLIC, PARAMETER :: N_MOLECULE_SETS = 36

  ! The names of the allowed molecular sets.
  ! These values are used in filenames and
  ! other transmittance production program inputs
  CHARACTER(*), PUBLIC, PARAMETER, DIMENSION(N_MOLECULE_SETS) :: MOLECULE_SET_TAG = &
    (/ 'mol1   ', &  !   1
       'mol2   ', &  !   2
       'mol3   ', &  !   3
       'mol4   ', &  !   4
       'mol5   ', &  !   5
       'mol6   ', &  !   6
       'mol7   ', &  !   7
       'anc    ', &  !   8 (all, no continua)
       'con    ', &  !   9 (continua_only)
       'awc    ', &  !  10 (all, with continua)
       'wvo    ', &  !  11
       'wet    ', &  !  12
       'dry    ', &  !  13
       'ozo    ', &  !  14
       'wco    ', &  !  15
       'doz    ', &  !  16
       'wvd    ', &  !  17
       'molc2  ', &  !  18 (O2 +CH4)
       'molc3  ', &  !  19 (O2+CH4+CO)
       'molc4  ', &  !  20 (O2+CH4+CO+N2O)
       'molc5  ', &  !  21 (O2+CH4+CO+N2O+CO2)
       'molc6  ', &  !  22 (O2+CH4+CO+N2O+CO2+H20)
       'molt1  ', &  !  ( first 7 molecules + 8 NO)   
       'molt2  ', &  !  ( first 7 molecules + 9 SO2)     
       'molt3  ', &  !  ( first 7 molecules + 10 NO2)    
       'molt4  ', &  !  ( first 7 molecules + 12 HNO3)    
       'molt5  ', &  !  ( first 7 molecules + 19 OCS)    
       'molt6  ', &  !  ( first 7 molecules + 22 N2)    
       'effmol1', &  ! 101
       'effwet ', &  ! 112
       'effdry ', &  ! 113
       'effozo ', &  ! 114
       'effch4 ', &  ! 118
       'effco  ', &  ! 119
       'effn2o ', &  ! 120 
       'effco2 '/)   ! 121 


  ! The ID values associated with the allowed moleculer sets
  INTEGER, PUBLIC, PARAMETER, DIMENSION(N_MOLECULE_SETS) :: MOLECULE_SET_TAG_ID = &
    (/  1, &  !  mol1   
        2, &  !  mol2   
        3, &  !  mol3   
        4, &  !  mol4   
        5, &  !  mol5   
        6, &  !  mol6   
        7, &  !  mol7 (O2)  
        8, &  !  anc    
        9, &  !  con    
       10, &  !  awc    
       11, &  !  wvo    
       12, &  !  wet    
       13, &  !  dry    
       14, &  !  ozo    
       15, &  !  wco    
       16, &  !  doz     
       17, &  !  wvd  
       18, &  !  (O2 +CH4)
       19, &  !  (O2+CH4+CO)
       20, &  !  (O2+CH4+CO+N2O)
       21, &  !  (O2+CH4+CO+N2O+CO2)
       22, &  !  (O2+CH4+CO+N2O+CO2+H20)  
       28, &  !  ( first 7 molecules + 8 NO)   
       29, &  !  ( first 7 molecules + 9 SO2)     
       30, &  !  ( first 7 molecules + 10 NO2)    
       32, &  !  ( first 7 molecules + 12 HNO3)    
       39, &  !  ( first 7 molecules + 19 OCS)    
       42, &  !  ( first 7 molecules + 22 N2)    
      101, &  !  effmol1
      112, &  !  effwet 
      113, &  !  effdry 
      114, &  !  effozo 
      118, &  !  effch4 
      119, &  !  effco  
      120, &  !  effn2o 
      121 /)  !  effco2 


  ! -----------------------------------
  ! The calculation angle specification
  ! -----------------------------------
  ! The begin and end zenith angle indices
  INTEGER, PUBLIC, PARAMETER :: ZENITH_ANGLE_BEGIN = 1
  INTEGER, PUBLIC, PARAMETER :: ZENITH_ANGLE_END   = 7
  
  ! The number of calculation zenith angles
  INTEGER, PUBLIC, PARAMETER :: N_ZENITH_ANGLES = ZENITH_ANGLE_END - ZENITH_ANGLE_BEGIN + 1

  ! The secant of the calculation angles
  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_ZENITH_ANGLES) :: &
    ZENITH_ANGLE_SECANT = (/ 1.00_fp, &  ! Nadir
                             1.25_fp, &  ! 36.870 deg.
                             1.50_fp, &  ! 48.190 deg.
                             1.75_fp, &  ! 55.150 deg.
                             2.00_fp, &  ! 60.000 deg.
                             2.25_fp, &  ! 63.612 deg.
                             3.00_fp /)  ! 70.529 deg.

  ! The actual calculation angles
  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_ZENITH_ANGLES) :: &
    ZENITH_ANGLE = (/  0.0_fp,          &
                      36.8698976458_fp, &
                      48.1896851042_fp, &
                      55.1500954210_fp, &
                      60.0_fp,          &
                      63.6122000388_fp, &
                      70.5287793655_fp /)

  ! -------------------------
  ! The calculation direction
  ! -------------------------
  INTEGER, PUBLIC, PARAMETER :: N_DIRECTIONS = 3
  INTEGER, PUBLIC, PARAMETER ::           RADIANCE    = 0
  INTEGER, PUBLIC, PARAMETER ::   UPWELLING_DIRECTION = 1
  INTEGER, PUBLIC, PARAMETER :: DOWNWELLING_DIRECTION = 2
  CHARACTER(*), PUBLIC, PARAMETER, DIMENSION(N_DIRECTIONS) :: &
    DIRECTION_NAME = (/ 'radiance   ', &
                        'upwelling  ', &
                        'downwelling' /)
  
END MODULE Tau_Production_Parameters
