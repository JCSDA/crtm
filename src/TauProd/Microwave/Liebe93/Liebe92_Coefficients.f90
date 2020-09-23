!
! NAME:
!       Liebe92_Coefficients
!
! PURPOSE:
!       Module containing line and coefficient data for microwave attenuation
!       calculations according to the Liebe 92 reference.
!
! CALLING SEQUENCE:
!       USE Liebe92_Coefficients
!
! COMMENTS:
!       The oxygen coefficient data has been scaled so the coefficient values
!       are in the same units as the Liebe 93 oxygen coefficients.
!
! CREATION HISTORY:
!       This data has been transposed from FORTRAN-77 format include
!       files written by:
!         L. Phalippou ECMWF 28-Jul-1992
!
!       Written by:     Paul van Delst, CIMSS/SSEC 21-Apr-2002
!                       paul.vandelst@ssec.wisc.edu
!

MODULE Liebe92_Coefficients

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds, ONLY: fp
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default...
  PRIVATE
  ! Oxygen data
  PUBLIC :: N_O2_LINES
  PUBLIC :: O2_LINE_FREQUENCY
  PUBLIC :: O2_A1
  PUBLIC :: O2_A2
  PUBLIC :: O2_A3
  PUBLIC :: O2_A4
  PUBLIC :: O2_A5
  PUBLIC :: O2_A6
  ! Water vapour data
  PUBLIC :: N_H2O_LINES
  PUBLIC :: H2O_LINE_FREQUENCY
  PUBLIC :: H2O_A1
  PUBLIC :: H2O_A2
  PUBLIC :: H2O_A3
  PUBLIC :: H2O_A4
  PUBLIC :: H2O_A5
  PUBLIC :: H2O_A6


  ! -----------------
  ! Module Parameters
  ! ----------------
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  ! Conversion factors for converting MPM92 coefficients to MPM93 units
  REAL(fp), PARAMETER :: O2_A1_SCALE = 1.0e-07_fp
  REAL(fp), PARAMETER :: O2_A3_SCALE = 1.0e-03_fp
  REAL(fp), PARAMETER :: O2_A5_SCALE = 1.0e-03_fp
  REAL(fp), PARAMETER :: O2_A6_SCALE = 1.0e-03_fp



  !#----------------------------------------------------------------------------#
  !#                              -- OXYGEN DATA--                              #
  !#----------------------------------------------------------------------------#
  ! Number of oxygen lines
  ! ----------------------
  INTEGER, PARAMETER :: N_O2_LINES  = 44

  ! Oxygen line frequency data
  ! --------------------------
  REAL(fp), PARAMETER :: O2_LINE_FREQUENCY( N_O2_LINES ) = &
  (/ 50.474239_fp,  50.987747_fp,  51.503349_fp,  52.021412_fp, &
     52.542393_fp,  53.066906_fp,  53.595749_fp,  54.130001_fp, &
     54.671158_fp,  55.221367_fp,  55.783802_fp,  56.264774_fp, &
     56.363388_fp,  56.968204_fp,  57.612484_fp,  58.323875_fp, &
     58.446590_fp,  59.164207_fp,  59.590984_fp,  60.306061_fp, &
     60.434776_fp,  61.150558_fp,  61.800156_fp,  62.411217_fp, &
     62.486259_fp,  62.997978_fp,  63.568520_fp,  64.127769_fp, &
     64.678902_fp,  65.224068_fp,  65.764771_fp,  66.302094_fp, &
     66.836830_fp,  67.369598_fp,  67.900864_fp,  68.431007_fp, &
     68.960312_fp, 118.750343_fp, 368.498352_fp, 424.763123_fp, &
    487.249359_fp, 715.393127_fp, 773.839661_fp, 834.145325_fp /)

  ! Oxygen coefficient data
  ! -----------------------
  REAL(fp), PARAMETER :: O2_A1( N_O2_LINES ) = O2_A1_SCALE * &
  (/ 0.940e+00_fp,  0.246e+01_fp,  0.608e+01_fp,  0.141e+02_fp, &
     0.310e+02_fp,  0.641e+02_fp,  0.125e+03_fp,  0.228e+03_fp, &
     0.392e+03_fp,  0.632e+03_fp,  0.954e+03_fp,  0.549e+03_fp, &
     0.134e+04_fp,  0.176e+04_fp,  0.214e+04_fp,  0.239e+04_fp, &
     0.146e+04_fp,  0.240e+04_fp,  0.211e+04_fp,  0.212e+04_fp, &
     0.246e+04_fp,  0.250e+04_fp,  0.230e+04_fp,  0.193e+04_fp, &
     0.152e+04_fp,  0.150e+04_fp,  0.109e+04_fp,  0.734e+03_fp, &
     0.464e+03_fp,  0.275e+03_fp,  0.153e+03_fp,  0.801e+02_fp, &
     0.395e+02_fp,  0.183e+02_fp,  0.801e+01_fp,  0.330e+01_fp, &
     0.128e+01_fp,  0.945e+03_fp,  0.679e+02_fp,  0.638e+03_fp, &
     0.235e+03_fp,  0.996e+02_fp,  0.671e+03_fp,  0.180e+03_fp /)

  REAL(fp), PARAMETER :: O2_A2( N_O2_LINES ) = &
  (/     9.694_fp,      8.694_fp,      7.744_fp,      6.844_fp, &
         6.004_fp,      5.224_fp,      4.484_fp,      3.814_fp, &
         3.194_fp,      2.624_fp,      2.119_fp,      0.015_fp, &
         1.660_fp,      1.260_fp,      0.915_fp,      0.626_fp, &
         0.084_fp,      0.391_fp,      0.212_fp,      0.212_fp, &
         0.391_fp,      0.626_fp,      0.915_fp,      1.260_fp, &
         0.083_fp,      1.665_fp,      2.115_fp,      2.620_fp, &
         3.195_fp,      3.815_fp,      4.485_fp,      5.225_fp, &
         6.005_fp,      6.845_fp,      7.745_fp,      8.695_fp, &
         9.695_fp,      0.009_fp,      0.049_fp,      0.044_fp, &
         0.049_fp,      0.145_fp,      0.130_fp,      0.147_fp /)

  REAL(fp), PARAMETER :: O2_A3( N_O2_LINES ) = O2_A3_SCALE * &
  (/ 0.850e+00_fp,  0.870e+00_fp,  0.890e+00_fp,  0.920e+00_fp, &
     0.940e+00_fp,  0.970e+00_fp,  0.100e+01_fp,  0.102e+01_fp, &
     0.105e+01_fp,  0.108e+01_fp,  0.111e+01_fp,  0.165e+01_fp, &
     0.114e+01_fp,  0.118e+01_fp,  0.122e+01_fp,  0.127e+01_fp, &
     0.145e+01_fp,  0.132e+01_fp,  0.136e+01_fp,  0.138e+01_fp, &
     0.130e+01_fp,  0.125e+01_fp,  0.121e+01_fp,  0.117e+01_fp, &
     0.147e+01_fp,  0.114e+01_fp,  0.111e+01_fp,  0.108e+01_fp, &
     0.105e+01_fp,  0.102e+01_fp,  0.100e+01_fp,  0.970e+00_fp, &
     0.940e+00_fp,  0.920e+00_fp,  0.890e+00_fp,  0.870e+00_fp, &
     0.850e+00_fp,  0.163e+01_fp,  0.192e+01_fp,  0.193e+01_fp, &
     0.192e+01_fp,  0.181e+01_fp,  0.181e+01_fp,  0.181e+01_fp /)

  REAL(fp), PARAMETER :: O2_A4( N_O2_LINES ) = &
  (/         ZERO,          ZERO,          ZERO,          ZERO, &                    
             ZERO,          ZERO,          ZERO,          ZERO, &                    
             ZERO,          ZERO,          ZERO,          ZERO, &                    
             ZERO,          ZERO,          ZERO,          ZERO, &                    
             ZERO,          ZERO,          ZERO,          ZERO, &                    
             ZERO,          ZERO,          ZERO,          ZERO, &                    
             ZERO,          ZERO,          ZERO,          ZERO, &                    
             ZERO,          ZERO,          ZERO,          ZERO, &                    
             ZERO,          ZERO,          ZERO,          ZERO, &                    
             ZERO,          ZERO,        0.6_fp,        0.6_fp, &
           0.6_fp,        0.6_fp,        0.6_fp,        0.6_fp /)

  REAL(fp), PARAMETER :: O2_A5( N_O2_LINES ) = O2_A5_SCALE * &
  (/  0.210e+00_fp,   0.190e+00_fp,   0.171e+00_fp,   0.144e+00_fp, &
      0.118e+00_fp,   0.114e+00_fp,   0.200e+00_fp,   0.291e+00_fp, &
      0.325e+00_fp,   0.224e+00_fp,  -0.144e+00_fp,   0.339e+00_fp, &
     -0.258e+00_fp,  -0.362e+00_fp,  -0.533e+00_fp,  -0.178e+00_fp, &
      0.650e+00_fp,  -0.628e+00_fp,   0.665e+00_fp,  -0.613e+00_fp, &
      0.606e+00_fp,   0.900e-01_fp,   0.496e+00_fp,   0.313e+00_fp, &
     -0.433e+00_fp,   0.208e+00_fp,   0.940e-01_fp,  -0.270e+00_fp, &
     -0.366e+00_fp,  -0.326e+00_fp,  -0.232e+00_fp,  -0.146e+00_fp, &
     -0.147e+00_fp,  -0.174e+00_fp,  -0.198e+00_fp,  -0.210e+00_fp, &
     -0.220e+00_fp,  -0.310e-01_fp,           ZERO,           ZERO, &
              ZERO,           ZERO,           ZERO,           ZERO /)

  REAL(fp), PARAMETER :: O2_A6( N_O2_LINES ) = O2_A6_SCALE * &
  (/  0.685e+00_fp,   0.680e+00_fp,   0.673e+00_fp,   0.664e+00_fp, &
      0.653e+00_fp,   0.621e+00_fp,   0.508e+00_fp,   0.375e+00_fp, &
      0.265e+00_fp,   0.295e+00_fp,   0.613e+00_fp,  -0.980e-01_fp, &
      0.655e+00_fp,   0.645e+00_fp,   0.606e+00_fp,   0.440e-01_fp, &
     -0.127e+00_fp,   0.231e+00_fp,  -0.780e-01_fp,   0.700e-01_fp, &
     -0.282e+00_fp,  -0.580e-01_fp,  -0.662e+00_fp,  -0.676e+00_fp, &
      0.840e-01_fp,  -0.668e+00_fp,  -0.614e+00_fp,  -0.289e+00_fp, &
     -0.259e+00_fp,  -0.368e+00_fp,  -0.500e+00_fp,  -0.609e+00_fp, &
     -0.639e+00_fp,  -0.647e+00_fp,  -0.655e+00_fp,  -0.660e+00_fp, &
     -0.665e+00_fp,   0.800e-02_fp,           ZERO,           ZERO, &
              ZERO,           ZERO,           ZERO,           ZERO /)



  !#----------------------------------------------------------------------------#
  !#                           -- WATER VAPOR DATA--                            #
  !#----------------------------------------------------------------------------#
  ! Number of Water vapor lines
  ! ---------------------------
  INTEGER, PARAMETER :: N_H2O_LINES = 35

  ! Water vapor line frequency data
  ! -------------------------------
  REAL(fp), PARAMETER :: H2O_LINE_FREQUENCY( N_H2O_LINES ) = &
  (/ 22.235080_fp,  67.803960_fp,  119.995940_fp, 183.310091_fp, &
    321.225644_fp, 325.152919_fp,  336.222601_fp, 380.197372_fp, &
    390.134508_fp, 437.346667_fp,  439.150812_fp, 443.018295_fp, &
    448.001075_fp, 470.888947_fp,  474.689127_fp, 488.491133_fp, &
    503.568532_fp, 504.482692_fp,  547.676440_fp, 552.020960_fp, &
    556.936002_fp, 620.700807_fp,  645.866155_fp, 658.005280_fp, &
    752.033227_fp, 841.053973_fp,  859.962313_fp, 899.306675_fp, &
    902.616173_fp, 906.207325_fp,  916.171582_fp, 923.118427_fp, &                   
    970.315022_fp, 987.926764_fp, 1780.000000_fp /)

  ! Water vapor coefficient data
  ! ----------------------------
  REAL(fp), PARAMETER :: H2O_A1( N_H2O_LINES ) = &
  (/    0.01130_fp,    0.00012_fp,    0.00008_fp,    0.24200_fp, &
        0.00483_fp,    0.14990_fp,    0.00011_fp,    1.15200_fp, &
        0.00046_fp,    0.00650_fp,    0.09218_fp,    0.01976_fp, &
        1.03200_fp,    0.03297_fp,    0.12620_fp,    0.02520_fp, &
        0.00390_fp,    0.00130_fp,    0.97010_fp,    1.47700_fp, &
       48.74000_fp,    0.50120_fp,    0.00713_fp,    0.03022_fp, &
       23.96000_fp,    0.00140_fp,    0.01472_fp,    0.00605_fp, &
        0.00426_fp,    0.01876_fp,    0.83400_fp,    0.00869_fp, &
        0.89720_fp,   13.21000_fp, 2230.00000_fp /)

  REAL(fp), PARAMETER :: H2O_A2( N_H2O_LINES ) = &
  (/      2.143_fp,      8.735_fp,      8.356_fp,      0.668_fp, &
          6.181_fp,      1.540_fp,      9.829_fp,      1.048_fp, &
          7.350_fp,      5.050_fp,      3.596_fp,      5.050_fp, &
          1.405_fp,      3.599_fp,      2.381_fp,      2.853_fp, &
          6.733_fp,      6.733_fp,      0.114_fp,      0.114_fp, &
          0.159_fp,      2.200_fp,      8.580_fp,      7.820_fp, &
          0.396_fp,      8.180_fp,      7.989_fp,      7.917_fp, &
          8.432_fp,      5.111_fp,      1.442_fp,     10.220_fp, &
          1.920_fp,      0.258_fp,      0.952_fp /)

  REAL(fp), PARAMETER :: H2O_A3( N_H2O_LINES ) = &
  (/  2.811e-03_fp,  2.858e-03_fp,  2.948e-03_fp,  3.050e-03_fp, &
      2.303e-03_fp,  2.783e-03_fp,  2.693e-03_fp,  2.873e-03_fp, &
      2.152e-03_fp,  1.845e-03_fp,  2.100e-03_fp,  1.860e-03_fp, &
      2.632e-03_fp,  2.152e-03_fp,  2.355e-03_fp,  2.602e-03_fp, &
      1.612e-03_fp,  1.612e-03_fp,  2.600e-03_fp,  2.600e-03_fp, &
      3.210e-03_fp,  2.438e-03_fp,  1.800e-03_fp,  3.210e-03_fp, &
      3.060e-03_fp,  1.590e-03_fp,  3.060e-03_fp,  2.985e-03_fp, &
      2.865e-03_fp,  2.408e-03_fp,  2.670e-03_fp,  2.900e-03_fp, &
      2.550e-03_fp,  2.985e-03_fp, 17.620e-03_fp /)

  REAL(fp), PARAMETER :: H2O_A4( N_H2O_LINES ) = &
  (/       4.80_fp,       4.93_fp,       4.78_fp,       5.30_fp, &
           4.69_fp,       4.85_fp,       4.74_fp,       5.38_fp, &
           4.81_fp,       4.23_fp,       4.29_fp,       4.23_fp, &
           4.84_fp,       4.57_fp,       4.65_fp,       5.04_fp, &
           3.98_fp,       4.01_fp,       4.50_fp,       4.50_fp, &
           4.11_fp,       4.68_fp,       4.00_fp,       4.14_fp, &
           4.09_fp,       5.76_fp,       4.09_fp,       4.53_fp, &
           5.10_fp,       4.70_fp,       4.78_fp,       5.00_fp, &
           4.94_fp,       4.55_fp,      30.50_fp /)

  REAL(fp), PARAMETER :: H2O_A5( N_H2O_LINES ) = &
  (/       0.69_fp,       0.69_fp,       0.70_fp,       0.64_fp, &
           0.67_fp,       0.68_fp,       0.69_fp,       0.54_fp, &
           0.63_fp,       0.60_fp,       0.63_fp,       0.60_fp, &
           0.66_fp,       0.66_fp,       0.65_fp,       0.69_fp, &
           0.61_fp,       0.61_fp,       0.70_fp,       0.70_fp, &
           0.69_fp,       0.71_fp,       0.60_fp,       0.69_fp, &
           0.68_fp,       0.33_fp,       0.68_fp,       0.68_fp, &
           0.70_fp,       0.70_fp,       0.70_fp,       0.70_fp, &
           0.64_fp,       0.68_fp,       2.00_fp /)

  REAL(fp), PARAMETER :: H2O_A6( N_H2O_LINES ) = &
  (/       1.00_fp,       0.82_fp,       0.79_fp,       0.85_fp, & 
           0.54_fp,       0.74_fp,       0.61_fp,       0.89_fp, & 
           0.55_fp,       0.48_fp,       0.52_fp,       0.50_fp, & 
           0.67_fp,       0.65_fp,       0.64_fp,       0.72_fp, & 
           0.43_fp,       0.45_fp,       1.00_fp,       1.00_fp, & 
           1.00_fp,       0.68_fp,       0.50_fp,       1.00_fp, & 
           0.84_fp,       0.45_fp,       0.84_fp,       0.90_fp, & 
           0.95_fp,       0.53_fp,       0.78_fp,       0.80_fp, & 
           0.67_fp,       0.90_fp,       5.00_fp /)

END MODULE Liebe92_Coefficients
