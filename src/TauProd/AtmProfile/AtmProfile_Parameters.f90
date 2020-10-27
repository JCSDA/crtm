!
! AtmProfile_Parameters
!
! Module defining parameters used in creating AtmProfile netCDF files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 29-Oct-2004
!                       paul.vandelst@ssec.wisc.edu
!

MODULE AtmProfile_Parameters

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds, ONLY: fp
  USE Profile_Utility_Parameters, ONLY: N_ABSORBER_UNITS   , &
                                        ABSORBER_UNITS_ID  , &
                                        ABSORBER_UNITS_NAME, &
                                        ABSORBER_UNITS_CHAR, &
                                        N_ABSORBERS   => MAX_N_MOLECULAR_SPECIES, &
                                        ABSORBER_NAME => MOLECULAR_SYMBOL
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! Info for identifying and naming absorbers and units
  PUBLIC :: N_ABSORBER_UNITS   
  PUBLIC :: ABSORBER_UNITS_ID  
  PUBLIC :: ABSORBER_UNITS_NAME
  PUBLIC :: ABSORBER_UNITS_CHAR
  PUBLIC :: N_ABSORBERS
  PUBLIC :: ABSORBER_NAME
  ! Dimensioning info
  PUBLIC :: N_ATMPROFILE_SETS
  PUBLIC :: ATMPROFILE_SET
  PUBLIC :: N_ATMPROFILES
  PUBLIC :: ATMPROFILE_BEGIN 
  ! The standard AIRS pressure levels
  PUBLIC :: N_ATMPROFILE_LEVELS
  PUBLIC :: N_ATMPROFILE_LAYERS
  PUBLIC :: ATMPROFILE_LEVEL_PRESSURE
  
  
  ! -----------------
  ! Module parameters
  ! -----------------
  ! Module Version string
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &


  ! The current number of profile sets and their ID tag
  INTEGER,      PARAMETER :: N_ATMPROFILE_SETS = 6
  CHARACTER(*), PARAMETER :: ATMPROFILE_SET( N_ATMPROFILE_SETS ) = &
    (/ 'UMBC48 ', &
       'CIMSS32', &
       'ECMWF52', &
       'ECMWF83', &
       'ECMWF5K', &
       'Model6 ' /)
  ! The number of profiles in each set
  INTEGER, PARAMETER :: N_ATMPROFILES(N_ATMPROFILE_SETS) = &
    (/ 48,   &
       32,   &
       52,   &
       83,   &
       5000, &
       6    /)
  INTEGER, PARAMETER :: ATMPROFILE_BEGIN = 1


  ! The Standard AIRS pressure levels
  INTEGER , PARAMETER :: N_ATMPROFILE_LEVELS = 101
  INTEGER , PARAMETER :: N_ATMPROFILE_LAYERS = 100
  REAL(fp), PARAMETER :: ATMPROFILE_LEVEL_PRESSURE(N_ATMPROFILE_LEVELS) = &
    (/ 1100.000000_fp, 1070.916940_fp, 1042.231940_fp, 1013.947655_fp, &
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

  ! --------------------------
  ! The old 46 pressure levels
  ! --------------------------

!  INTEGER, PARAMETER :: N_ATMPROFILE_LEVELS = 46                          
!  INTEGER, PARAMETER :: N_ATMPROFILE_LAYERS = 45                          
!                                                                                  
!  REAL(fp), PARAMETER :: ATMPROFILE_LEVEL_PRESSURE(N_ATMPROFILE_LEVELS) = &                                                 
!    (/ 1050.000_fp, 1025.000_fp, 1000.000_fp,  950.000_fp, &  
!        920.000_fp,  850.000_fp,  780.000_fp,  700.000_fp, &  
!        670.000_fp,  620.000_fp,  570.000_fp,  500.000_fp, &  
!        475.000_fp,  430.000_fp,  400.000_fp,  350.000_fp, &  
!        300.000_fp,  250.000_fp,  200.000_fp,  150.000_fp, &  
!        135.000_fp,  115.000_fp,  100.000_fp,   85.000_fp, &  
!         70.000_fp,   60.000_fp,   50.000_fp,   30.000_fp, &  
!         25.000_fp,   20.000_fp,   15.000_fp,   10.000_fp, &  
!          7.000_fp,    5.000_fp,    4.000_fp,    3.000_fp, &  
!          2.000_fp,    1.500_fp,    1.000_fp,    0.500_fp, &  
!          0.200_fp,    0.100_fp,    0.050_fp,    0.020_fp, &  
!          0.010_fp,    0.005_fp /)                                      


END MODULE AtmProfile_Parameters
