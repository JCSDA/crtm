!
! NAME:
!       Liebe93_Coefficients
!
! PURPOSE:
!       Module containing line and coefficient data for microwave line-by-line
!       transmittance calculations according to Liebe 93.
!
! CALLING SEQUENCE:
!       USE Liebe93_Coefficients
!
! CREATION HISTORY:
!       This data has been transposed from FORTRAN-77 format include
!       files written by:
!         L. Phalippou ECMWF 13-Dec-1993
!
!       Written by:     Paul van Delst, CIMSS/SSEC 21-Apr-2002
!                       paul.vandelst@ssec.wisc.edu
!

MODULE Liebe93_Coefficients

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
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  REAL(fp), PARAMETER :: ZERO = 0.0_fp



  !#----------------------------------------------------------------------------#
  !#                              -- OXYGEN DATA--                              #
  !#----------------------------------------------------------------------------#
  ! Number of oxygen lines
  ! ----------------------
  INTEGER, PARAMETER :: N_O2_LINES  = 44

  ! Oxygen line frequency data
  ! --------------------------
  REAL(fp), PARAMETER :: O2_LINE_FREQUENCY( N_O2_LINES ) = &
  (/   50.474238_fp,   50.987749_fp,   51.503350_fp,   52.021410_fp, &
       52.542394_fp,   53.066907_fp,   53.595749_fp,   54.130000_fp, &
       54.671159_fp,   55.221367_fp,   55.783802_fp,   56.264775_fp, &
       56.363389_fp,   56.968206_fp,   57.612484_fp,   58.323877_fp, &
       58.446590_fp,   59.164207_fp,   59.590983_fp,   60.306061_fp, &
       60.434776_fp,   61.150560_fp,   61.800154_fp,   62.411215_fp, &
       62.486260_fp,   62.997977_fp,   63.568518_fp,   64.127767_fp, &
       64.678903_fp,   65.224071_fp,   65.764772_fp,   66.302091_fp, &
       66.836830_fp,   67.369598_fp,   67.900867_fp,   68.431005_fp, &
       68.960311_fp,  118.750343_fp,  368.498350_fp,  424.763124_fp, &
      487.249370_fp,  715.393150_fp,  773.839675_fp,  834.145330_fp /)

  ! Oxygen coefficient data
  ! -----------------------
  REAL(fp), PARAMETER :: O2_A1( N_O2_LINES ) = &
  (/   0.094e-06_fp,   0.246e-06_fp,   0.608e-06_fp,   1.414e-06_fp, &
       3.102e-06_fp,   6.410e-06_fp,  12.470e-06_fp,  22.800e-06_fp, &
      39.180e-06_fp,  63.160e-06_fp,  95.350e-06_fp,  54.890e-06_fp, &
     134.400e-06_fp, 176.300e-06_fp, 214.100e-06_fp, 238.600e-06_fp, &
     145.700e-06_fp, 240.400e-06_fp, 211.200e-06_fp, 212.400e-06_fp, &
     246.100e-06_fp, 250.400e-06_fp, 229.800e-06_fp, 193.300e-06_fp, &
     151.700e-06_fp, 150.300e-06_fp, 108.700e-06_fp,  73.350e-06_fp, &
      46.350e-06_fp,  27.480e-06_fp,  15.300e-06_fp,   8.009e-06_fp, &
       3.946e-06_fp,   1.832e-06_fp,   0.801e-06_fp,   0.330e-06_fp, &
       0.128e-06_fp,  94.500e-06_fp,   6.790e-06_fp,  63.800e-06_fp, &
      23.500e-06_fp,   9.960e-06_fp,  67.100e-06_fp,  18.000e-06_fp /)

  REAL(fp), PARAMETER :: O2_A2( N_O2_LINES ) = &
  (/       9.694_fp,       8.694_fp,       7.744_fp,       6.844_fp, &
           6.004_fp,       5.224_fp,       4.484_fp,       3.814_fp, &
           3.194_fp,       2.624_fp,       2.119_fp,       0.015_fp, &
           1.660_fp,       1.260_fp,       0.915_fp,       0.626_fp, &
           0.084_fp,       0.391_fp,       0.212_fp,       0.212_fp, &
           0.391_fp,       0.626_fp,       0.915_fp,       1.260_fp, &
           0.083_fp,       1.665_fp,       2.115_fp,       2.620_fp, &
           3.195_fp,       3.815_fp,       4.485_fp,       5.225_fp, &
           6.005_fp,       6.845_fp,       7.745_fp,       8.695_fp, &
           9.695_fp,       0.009_fp,       0.049_fp,       0.044_fp, &
           0.049_fp,       0.145_fp,       0.130_fp,       0.147_fp /)

  REAL(fp), PARAMETER :: O2_A3( N_O2_LINES ) = &
  (/   0.890e-03_fp,   0.910e-03_fp,   0.940e-03_fp,   0.970e-03_fp, &
       0.990e-03_fp,   1.020e-03_fp,   1.050e-03_fp,   1.070e-03_fp, &
       1.100e-03_fp,   1.130e-03_fp,   1.170e-03_fp,   1.730e-03_fp, &
       1.200e-03_fp,   1.240e-03_fp,   1.280e-03_fp,   1.330e-03_fp, &
       1.520e-03_fp,   1.390e-03_fp,   1.430e-03_fp,   1.450e-03_fp, &
       1.360e-03_fp,   1.310e-03_fp,   1.270e-03_fp,   1.230e-03_fp, &
       1.540e-03_fp,   1.200e-03_fp,   1.170e-03_fp,   1.130e-03_fp, &
       1.100e-03_fp,   1.070e-03_fp,   1.050e-03_fp,   1.020e-03_fp, &
       0.990e-03_fp,   0.970e-03_fp,   0.940e-03_fp,   0.920e-03_fp, &
       0.900e-03_fp,   1.630e-03_fp,   1.920e-03_fp,   1.930e-03_fp, &
       1.920e-03_fp,   1.810e-03_fp,   1.820e-03_fp,   1.810e-03_fp /)

  REAL(fp), PARAMETER :: O2_A4( N_O2_LINES ) = &
  (/           ZERO,           ZERO,           ZERO,           ZERO, &               
               ZERO,           ZERO,           ZERO,           ZERO, &               
               ZERO,           ZERO,           ZERO,           ZERO, &               
               ZERO,           ZERO,           ZERO,           ZERO, &               
               ZERO,           ZERO,           ZERO,           ZERO, &               
               ZERO,           ZERO,           ZERO,           ZERO, &               
               ZERO,           ZERO,           ZERO,           ZERO, &               
               ZERO,           ZERO,           ZERO,           ZERO, &               
               ZERO,           ZERO,           ZERO,           ZERO, &               
               ZERO,           ZERO,         0.6_fp,         0.6_fp, &               
             0.6_fp,         0.6_fp,         0.6_fp,         0.6_fp /)

  REAL(fp), PARAMETER :: O2_A5( N_O2_LINES ) = &
  (/   0.240e-03_fp,   0.220e-03_fp,   0.197e-03_fp,   0.166e-03_fp, &
       0.136e-03_fp,   0.131e-03_fp,   0.230e-03_fp,   0.335e-03_fp, &
       0.374e-03_fp,   0.258e-03_fp,  -0.166e-03_fp,   0.390e-03_fp, &
      -0.297e-03_fp,  -0.416e-03_fp,  -0.613e-03_fp,  -0.205e-03_fp, &
       0.748e-03_fp,  -0.722e-03_fp,   0.765e-03_fp,  -0.705e-03_fp, &
       0.697e-03_fp,   0.104e-03_fp,   0.570e-03_fp,   0.360e-03_fp, &
      -0.498e-03_fp,   0.239e-03_fp,   0.108e-03_fp,  -0.311e-03_fp, &
      -0.421e-03_fp,  -0.375e-03_fp,  -0.267e-03_fp,  -0.168e-03_fp, &
      -0.169e-03_fp,  -0.200e-03_fp,  -0.228e-03_fp,  -0.240e-03_fp, &
      -0.250e-03_fp,  -0.036e-03_fp,           ZERO,           ZERO, &
               ZERO,           ZERO,           ZERO,           ZERO /)

  REAL(fp), PARAMETER :: O2_A6( N_O2_LINES ) = &
  (/   0.790e-03_fp,   0.780e-03_fp,   0.774e-03_fp,   0.764e-03_fp, &
       0.751e-03_fp,   0.714e-03_fp,   0.584e-03_fp,   0.431e-03_fp, &
       0.305e-03_fp,   0.339e-03_fp,   0.705e-03_fp,  -0.113e-03_fp, &
       0.753e-03_fp,   0.742e-03_fp,   0.697e-03_fp,   0.051e-03_fp, &
      -0.146e-03_fp,   0.266e-03_fp,  -0.090e-03_fp,   0.081e-03_fp, &
      -0.324e-03_fp,  -0.067e-03_fp,  -0.761e-03_fp,  -0.777e-03_fp, &
       0.097e-03_fp,  -0.768e-03_fp,  -0.706e-03_fp,  -0.332e-03_fp, &
      -0.298e-03_fp,  -0.423e-03_fp,  -0.575e-03_fp,  -0.700e-03_fp, &
      -0.735e-03_fp,  -0.744e-03_fp,  -0.753e-03_fp,  -0.760e-03_fp, &
      -0.765e-03_fp,   0.009e-03_fp,           ZERO,           ZERO, &
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
  (/  22.235080_fp,   67.803960_fp,  119.995940_fp,  183.310091_fp, &
     321.225644_fp,  325.152919_fp,  336.222601_fp,  380.197372_fp, &
     390.134508_fp,  437.346667_fp,  439.150812_fp,  443.018295_fp, &
     448.001075_fp,  470.888947_fp,  474.689127_fp,  488.491133_fp, &
     503.568532_fp,  504.482692_fp,  547.676440_fp,  552.020960_fp, &
     556.936002_fp,  620.700807_fp,  645.866155_fp,  658.005280_fp, &
     752.033227_fp,  841.053973_fp,  859.962313_fp,  899.306675_fp, &
     902.616173_fp,  906.207325_fp,  916.171582_fp,  923.118427_fp, &
     970.315022_fp,  987.926764_fp, 1780.000000_fp /)

  ! Water vapor coefficient data
  ! ----------------------------
  REAL(fp), PARAMETER :: H2O_A1( N_H2O_LINES ) = &
  (/    0.01130_fp,     0.00012_fp,     0.00008_fp,     0.24200_fp, &
        0.00483_fp,     0.14990_fp,     0.00011_fp,     1.15200_fp, &
        0.00046_fp,     0.00650_fp,     0.09218_fp,     0.01976_fp, &
        1.03200_fp,     0.03297_fp,     0.12620_fp,     0.02520_fp, &
        0.00390_fp,     0.00130_fp,     0.97010_fp,     1.47700_fp, &
       48.74000_fp,     0.50120_fp,     0.00713_fp,     0.03022_fp, &
       23.96000_fp,     0.00140_fp,     0.01472_fp,     0.00605_fp, &
        0.00426_fp,     0.01876_fp,     0.83400_fp,     0.00869_fp, &
        0.89720_fp,    13.21000_fp,  2230.00000_fp /)

  REAL(fp), PARAMETER :: H2O_A2( N_H2O_LINES ) = &
  (/      2.143_fp,       8.735_fp,       8.356_fp,       0.668_fp, &
          6.181_fp,       1.540_fp,       9.829_fp,       1.048_fp, &
          7.350_fp,       5.050_fp,       3.596_fp,       5.050_fp, &
          1.405_fp,       3.599_fp,       2.381_fp,       2.853_fp, &
          6.733_fp,       6.733_fp,       0.114_fp,       0.114_fp, &
          0.159_fp,       2.200_fp,       8.580_fp,       7.820_fp, &
          0.396_fp,       8.180_fp,       7.989_fp,       7.917_fp, &
          8.432_fp,       5.111_fp,       1.442_fp,      10.220_fp, &
          1.920_fp,       0.258_fp,       0.952_fp /)

  REAL(fp), PARAMETER :: H2O_A3( N_H2O_LINES ) = &
  (/  2.811e-03_fp,   2.858e-03_fp,   2.948e-03_fp,   3.050e-03_fp, &
      2.303e-03_fp,   2.783e-03_fp,   2.693e-03_fp,   2.873e-03_fp, &
      2.152e-03_fp,   1.845e-03_fp,   2.100e-03_fp,   1.860e-03_fp, &
      2.632e-03_fp,   2.152e-03_fp,   2.355e-03_fp,   2.602e-03_fp, &
      1.612e-03_fp,   1.612e-03_fp,   2.600e-03_fp,   2.600e-03_fp, &
      3.210e-03_fp,   2.438e-03_fp,   1.800e-03_fp,   3.210e-03_fp, &
      3.060e-03_fp,   1.590e-03_fp,   3.060e-03_fp,   2.985e-03_fp, &
      2.865e-03_fp,   2.408e-03_fp,   2.670e-03_fp,   2.900e-03_fp, &
      2.550e-03_fp,   2.985e-03_fp,  17.620e-03_fp /)

  REAL(fp), PARAMETER :: H2O_A4( N_H2O_LINES ) = &
  (/       4.80_fp,        4.93_fp,        4.78_fp,        5.30_fp, &
           4.69_fp,        4.85_fp,        4.74_fp,        5.38_fp, &
           4.81_fp,        4.23_fp,        4.29_fp,        4.23_fp, &
           4.84_fp,        4.57_fp,        4.65_fp,        5.04_fp, &
           3.98_fp,        4.01_fp,        4.50_fp,        4.50_fp, &
           4.11_fp,        4.68_fp,        4.00_fp,        4.14_fp, &
           4.09_fp,        5.76_fp,        4.09_fp,        4.53_fp, &
           5.10_fp,        4.70_fp,        4.78_fp,        5.00_fp, &
           4.94_fp,        4.55_fp,       30.50_fp /)

  REAL(fp), PARAMETER :: H2O_A5( N_H2O_LINES ) = &
  (/       0.69_fp,        0.69_fp,        0.70_fp,        0.64_fp, &
           0.67_fp,        0.68_fp,        0.69_fp,        0.54_fp, &
           0.63_fp,        0.60_fp,        0.63_fp,        0.60_fp, &
           0.66_fp,        0.66_fp,        0.65_fp,        0.69_fp, &
           0.61_fp,        0.61_fp,        0.70_fp,        0.70_fp, &
           0.69_fp,        0.71_fp,        0.60_fp,        0.69_fp, &
           0.68_fp,        0.33_fp,        0.68_fp,        0.68_fp, &
           0.70_fp,        0.70_fp,        0.70_fp,        0.70_fp, &
           0.64_fp,        0.68_fp,        2.00_fp /)

  REAL(fp), PARAMETER :: H2O_A6( N_H2O_LINES ) = &
  (/       1.00_fp,        0.82_fp,        0.79_fp,        0.85_fp, &
           0.54_fp,        0.74_fp,        0.61_fp,        0.89_fp, &
           0.55_fp,        0.48_fp,        0.52_fp,        0.50_fp, &
           0.67_fp,        0.65_fp,        0.64_fp,        0.72_fp, &
           0.43_fp,        0.45_fp,        1.00_fp,        1.00_fp, &
           1.00_fp,        0.68_fp,        0.50_fp,        1.00_fp, &
           0.84_fp,        0.45_fp,        0.84_fp,        0.90_fp, &
           0.95_fp,        0.53_fp,        0.78_fp,        0.80_fp, &
           0.67_fp,        0.90_fp,        5.00_fp /)

END MODULE Liebe93_Coefficients
