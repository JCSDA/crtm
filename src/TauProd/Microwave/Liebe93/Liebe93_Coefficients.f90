!------------------------------------------------------------------------------
!M+
! NAME:
!       Liebe93_Coefficients
!
! PURPOSE:
!       Module containing line and coefficient data for microwave line-by-line
!       transmittance calculations according to Liebe 93.
!
! CATEGORY:
!       LBL : Microwave
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       USE Liebe93_Coefficients
!
! MODULES:
!       type_kinds:         Module containing definitions for kinds
!                           of variable types.
!
! CREATION HISTORY:
!       This data has been transposed from FORTRAN-77 format include
!       files written by:
!         L. Phalippou ECMWF 13-Dec-1993
!
!       Written by:     Paul van Delst, CIMSS/SSEC 21-Apr-2002
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2002 Paul van Delst
!
!  This program is free software; you can redistribute it and/or
!  modify it under the terms of the GNU General Public License
!  as published by the Free Software Foundation; either version 2
!  of the License, or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program; if not, write to the Free Software
!  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
!M-
!------------------------------------------------------------------------------


MODULE Liebe93_Coefficients


  ! ------------
  ! Module usage
  ! ------------

  USE type_kinds


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------------
  ! Default visibility
  ! ------------------

  PRIVATE


  ! ----------
  ! Parameters
  ! ----------

  REAL( fp_kind ), PRIVATE, PARAMETER :: ZERO = 0.0_fp_kind



  !#----------------------------------------------------------------------------#
  !#                              -- OXYGEN DATA--                              #
  !#----------------------------------------------------------------------------#

  ! ----------------------
  ! Number of oxygen lines
  ! ----------------------

  INTEGER, PUBLIC, PARAMETER :: N_O2_LINES  = 44


  ! --------------------------
  ! Oxygen line frequency data
  ! --------------------------

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_O2_LINES ) :: O2_LINE_FREQUENCY = &
  (/   50.474238_fp_kind,   50.987749_fp_kind,   51.503350_fp_kind,   52.021410_fp_kind, &
       52.542394_fp_kind,   53.066907_fp_kind,   53.595749_fp_kind,   54.130000_fp_kind, &
       54.671159_fp_kind,   55.221367_fp_kind,   55.783802_fp_kind,   56.264775_fp_kind, &
       56.363389_fp_kind,   56.968206_fp_kind,   57.612484_fp_kind,   58.323877_fp_kind, &
       58.446590_fp_kind,   59.164207_fp_kind,   59.590983_fp_kind,   60.306061_fp_kind, &
       60.434776_fp_kind,   61.150560_fp_kind,   61.800154_fp_kind,   62.411215_fp_kind, &
       62.486260_fp_kind,   62.997977_fp_kind,   63.568518_fp_kind,   64.127767_fp_kind, &
       64.678903_fp_kind,   65.224071_fp_kind,   65.764772_fp_kind,   66.302091_fp_kind, &
       66.836830_fp_kind,   67.369598_fp_kind,   67.900867_fp_kind,   68.431005_fp_kind, &
       68.960311_fp_kind,  118.750343_fp_kind,  368.498350_fp_kind,  424.763124_fp_kind, &
      487.249370_fp_kind,  715.393150_fp_kind,  773.839675_fp_kind,  834.145330_fp_kind /)


  ! -----------------------
  ! Oxygen coefficient data
  ! -----------------------

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_O2_LINES ) :: O2_A1 = &
  (/   0.094e-06_fp_kind,   0.246e-06_fp_kind,   0.608e-06_fp_kind,   1.414e-06_fp_kind, &
       3.102e-06_fp_kind,   6.410e-06_fp_kind,  12.470e-06_fp_kind,  22.800e-06_fp_kind, &
      39.180e-06_fp_kind,  63.160e-06_fp_kind,  95.350e-06_fp_kind,  54.890e-06_fp_kind, &
     134.400e-06_fp_kind, 176.300e-06_fp_kind, 214.100e-06_fp_kind, 238.600e-06_fp_kind, &
     145.700e-06_fp_kind, 240.400e-06_fp_kind, 211.200e-06_fp_kind, 212.400e-06_fp_kind, &
     246.100e-06_fp_kind, 250.400e-06_fp_kind, 229.800e-06_fp_kind, 193.300e-06_fp_kind, &
     151.700e-06_fp_kind, 150.300e-06_fp_kind, 108.700e-06_fp_kind,  73.350e-06_fp_kind, &
      46.350e-06_fp_kind,  27.480e-06_fp_kind,  15.300e-06_fp_kind,   8.009e-06_fp_kind, &
       3.946e-06_fp_kind,   1.832e-06_fp_kind,   0.801e-06_fp_kind,   0.330e-06_fp_kind, &
       0.128e-06_fp_kind,  94.500e-06_fp_kind,   6.790e-06_fp_kind,  63.800e-06_fp_kind, &
      23.500e-06_fp_kind,   9.960e-06_fp_kind,  67.100e-06_fp_kind,  18.000e-06_fp_kind /)

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_O2_LINES ) :: O2_A2 = &
  (/       9.694_fp_kind,       8.694_fp_kind,       7.744_fp_kind,       6.844_fp_kind, &
           6.004_fp_kind,       5.224_fp_kind,       4.484_fp_kind,       3.814_fp_kind, &
           3.194_fp_kind,       2.624_fp_kind,       2.119_fp_kind,       0.015_fp_kind, &
           1.660_fp_kind,       1.260_fp_kind,       0.915_fp_kind,       0.626_fp_kind, &
           0.084_fp_kind,       0.391_fp_kind,       0.212_fp_kind,       0.212_fp_kind, &
           0.391_fp_kind,       0.626_fp_kind,       0.915_fp_kind,       1.260_fp_kind, &
           0.083_fp_kind,       1.665_fp_kind,       2.115_fp_kind,       2.620_fp_kind, &
           3.195_fp_kind,       3.815_fp_kind,       4.485_fp_kind,       5.225_fp_kind, &
           6.005_fp_kind,       6.845_fp_kind,       7.745_fp_kind,       8.695_fp_kind, &
           9.695_fp_kind,       0.009_fp_kind,       0.049_fp_kind,       0.044_fp_kind, &
           0.049_fp_kind,       0.145_fp_kind,       0.130_fp_kind,       0.147_fp_kind /)

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_O2_LINES ) :: O2_A3 = &
  (/   0.890e-03_fp_kind,   0.910e-03_fp_kind,   0.940e-03_fp_kind,   0.970e-03_fp_kind, &
       0.990e-03_fp_kind,   1.020e-03_fp_kind,   1.050e-03_fp_kind,   1.070e-03_fp_kind, &
       1.100e-03_fp_kind,   1.130e-03_fp_kind,   1.170e-03_fp_kind,   1.730e-03_fp_kind, &
       1.200e-03_fp_kind,   1.240e-03_fp_kind,   1.280e-03_fp_kind,   1.330e-03_fp_kind, &
       1.520e-03_fp_kind,   1.390e-03_fp_kind,   1.430e-03_fp_kind,   1.450e-03_fp_kind, &
       1.360e-03_fp_kind,   1.310e-03_fp_kind,   1.270e-03_fp_kind,   1.230e-03_fp_kind, &
       1.540e-03_fp_kind,   1.200e-03_fp_kind,   1.170e-03_fp_kind,   1.130e-03_fp_kind, &
       1.100e-03_fp_kind,   1.070e-03_fp_kind,   1.050e-03_fp_kind,   1.020e-03_fp_kind, &
       0.990e-03_fp_kind,   0.970e-03_fp_kind,   0.940e-03_fp_kind,   0.920e-03_fp_kind, &
       0.900e-03_fp_kind,   1.630e-03_fp_kind,   1.920e-03_fp_kind,   1.930e-03_fp_kind, &
       1.920e-03_fp_kind,   1.810e-03_fp_kind,   1.820e-03_fp_kind,   1.810e-03_fp_kind /)

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_O2_LINES ) :: O2_A4 = &
  (/                ZERO,                ZERO,                ZERO,                ZERO, &
                    ZERO,                ZERO,                ZERO,                ZERO, &
                    ZERO,                ZERO,                ZERO,                ZERO, &
                    ZERO,                ZERO,                ZERO,                ZERO, &
                    ZERO,                ZERO,                ZERO,                ZERO, &
                    ZERO,                ZERO,                ZERO,                ZERO, &
                    ZERO,                ZERO,                ZERO,                ZERO, &
                    ZERO,                ZERO,                ZERO,                ZERO, &
                    ZERO,                ZERO,                ZERO,                ZERO, &
                    ZERO,                ZERO,         0.6_fp_kind,         0.6_fp_kind, &
             0.6_fp_kind,         0.6_fp_kind,         0.6_fp_kind,         0.6_fp_kind /)

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_O2_LINES ) :: O2_A5 = &
  (/   0.240e-03_fp_kind,   0.220e-03_fp_kind,   0.197e-03_fp_kind,   0.166e-03_fp_kind, &
       0.136e-03_fp_kind,   0.131e-03_fp_kind,   0.230e-03_fp_kind,   0.335e-03_fp_kind, &
       0.374e-03_fp_kind,   0.258e-03_fp_kind,  -0.166e-03_fp_kind,   0.390e-03_fp_kind, &
      -0.297e-03_fp_kind,  -0.416e-03_fp_kind,  -0.613e-03_fp_kind,  -0.205e-03_fp_kind, &
       0.748e-03_fp_kind,  -0.722e-03_fp_kind,   0.765e-03_fp_kind,  -0.705e-03_fp_kind, &
       0.697e-03_fp_kind,   0.104e-03_fp_kind,   0.570e-03_fp_kind,   0.360e-03_fp_kind, &
      -0.498e-03_fp_kind,   0.239e-03_fp_kind,   0.108e-03_fp_kind,  -0.311e-03_fp_kind, &
      -0.421e-03_fp_kind,  -0.375e-03_fp_kind,  -0.267e-03_fp_kind,  -0.168e-03_fp_kind, &
      -0.169e-03_fp_kind,  -0.200e-03_fp_kind,  -0.228e-03_fp_kind,  -0.240e-03_fp_kind, &
      -0.250e-03_fp_kind,  -0.036e-03_fp_kind,                ZERO,                ZERO, &
                    ZERO,                ZERO,                ZERO,                ZERO /)

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_O2_LINES ) :: O2_A6 = &
  (/   0.790e-03_fp_kind,   0.780e-03_fp_kind,   0.774e-03_fp_kind,   0.764e-03_fp_kind, &
       0.751e-03_fp_kind,   0.714e-03_fp_kind,   0.584e-03_fp_kind,   0.431e-03_fp_kind, &
       0.305e-03_fp_kind,   0.339e-03_fp_kind,   0.705e-03_fp_kind,  -0.113e-03_fp_kind, &
       0.753e-03_fp_kind,   0.742e-03_fp_kind,   0.697e-03_fp_kind,   0.051e-03_fp_kind, &
      -0.146e-03_fp_kind,   0.266e-03_fp_kind,  -0.090e-03_fp_kind,   0.081e-03_fp_kind, &
      -0.324e-03_fp_kind,  -0.067e-03_fp_kind,  -0.761e-03_fp_kind,  -0.777e-03_fp_kind, &
       0.097e-03_fp_kind,  -0.768e-03_fp_kind,  -0.706e-03_fp_kind,  -0.332e-03_fp_kind, &
      -0.298e-03_fp_kind,  -0.423e-03_fp_kind,  -0.575e-03_fp_kind,  -0.700e-03_fp_kind, &
      -0.735e-03_fp_kind,  -0.744e-03_fp_kind,  -0.753e-03_fp_kind,  -0.760e-03_fp_kind, &
      -0.765e-03_fp_kind,   0.009e-03_fp_kind,                ZERO,                ZERO, &
                    ZERO,                ZERO,                ZERO,                ZERO /)



  !#----------------------------------------------------------------------------#
  !#                           -- WATER VAPOR DATA--                            #
  !#----------------------------------------------------------------------------#

  ! ---------------------------
  ! Number of Water vapor lines
  ! ---------------------------

  INTEGER, PUBLIC, PARAMETER :: N_H2O_LINES = 35


  ! -------------------------------
  ! Water vapor line frequency data
  ! -------------------------------

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_H2O_LINES ) :: H2O_LINE_FREQUENCY = &
  (/  22.235080_fp_kind,   67.803960_fp_kind,  119.995940_fp_kind,  183.310091_fp_kind, &
     321.225644_fp_kind,  325.152919_fp_kind,  336.222601_fp_kind,  380.197372_fp_kind, &
     390.134508_fp_kind,  437.346667_fp_kind,  439.150812_fp_kind,  443.018295_fp_kind, &
     448.001075_fp_kind,  470.888947_fp_kind,  474.689127_fp_kind,  488.491133_fp_kind, &
     503.568532_fp_kind,  504.482692_fp_kind,  547.676440_fp_kind,  552.020960_fp_kind, &
     556.936002_fp_kind,  620.700807_fp_kind,  645.866155_fp_kind,  658.005280_fp_kind, &
     752.033227_fp_kind,  841.053973_fp_kind,  859.962313_fp_kind,  899.306675_fp_kind, &
     902.616173_fp_kind,  906.207325_fp_kind,  916.171582_fp_kind,  923.118427_fp_kind, &
     970.315022_fp_kind,  987.926764_fp_kind, 1780.000000_fp_kind /)


  ! ----------------------------
  ! Water vapor coefficient data
  ! ----------------------------

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_H2O_LINES ) :: H2O_A1 = &
  (/    0.01130_fp_kind,     0.00012_fp_kind,     0.00008_fp_kind,     0.24200_fp_kind, &
        0.00483_fp_kind,     0.14990_fp_kind,     0.00011_fp_kind,     1.15200_fp_kind, &
        0.00046_fp_kind,     0.00650_fp_kind,     0.09218_fp_kind,     0.01976_fp_kind, &
        1.03200_fp_kind,     0.03297_fp_kind,     0.12620_fp_kind,     0.02520_fp_kind, &
        0.00390_fp_kind,     0.00130_fp_kind,     0.97010_fp_kind,     1.47700_fp_kind, &
       48.74000_fp_kind,     0.50120_fp_kind,     0.00713_fp_kind,     0.03022_fp_kind, &
       23.96000_fp_kind,     0.00140_fp_kind,     0.01472_fp_kind,     0.00605_fp_kind, &
        0.00426_fp_kind,     0.01876_fp_kind,     0.83400_fp_kind,     0.00869_fp_kind, &
        0.89720_fp_kind,    13.21000_fp_kind,  2230.00000_fp_kind /)

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_H2O_LINES ) :: H2O_A2 = &
  (/      2.143_fp_kind,       8.735_fp_kind,       8.356_fp_kind,       0.668_fp_kind, &
          6.181_fp_kind,       1.540_fp_kind,       9.829_fp_kind,       1.048_fp_kind, &
          7.350_fp_kind,       5.050_fp_kind,       3.596_fp_kind,       5.050_fp_kind, &
          1.405_fp_kind,       3.599_fp_kind,       2.381_fp_kind,       2.853_fp_kind, &
          6.733_fp_kind,       6.733_fp_kind,       0.114_fp_kind,       0.114_fp_kind, &
          0.159_fp_kind,       2.200_fp_kind,       8.580_fp_kind,       7.820_fp_kind, &
          0.396_fp_kind,       8.180_fp_kind,       7.989_fp_kind,       7.917_fp_kind, &
          8.432_fp_kind,       5.111_fp_kind,       1.442_fp_kind,      10.220_fp_kind, &
          1.920_fp_kind,       0.258_fp_kind,       0.952_fp_kind /)

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_H2O_LINES ) :: H2O_A3 = &
  (/  2.811e-03_fp_kind,   2.858e-03_fp_kind,   2.948e-03_fp_kind,   3.050e-03_fp_kind, &
      2.303e-03_fp_kind,   2.783e-03_fp_kind,   2.693e-03_fp_kind,   2.873e-03_fp_kind, &
      2.152e-03_fp_kind,   1.845e-03_fp_kind,   2.100e-03_fp_kind,   1.860e-03_fp_kind, &
      2.632e-03_fp_kind,   2.152e-03_fp_kind,   2.355e-03_fp_kind,   2.602e-03_fp_kind, &
      1.612e-03_fp_kind,   1.612e-03_fp_kind,   2.600e-03_fp_kind,   2.600e-03_fp_kind, &
      3.210e-03_fp_kind,   2.438e-03_fp_kind,   1.800e-03_fp_kind,   3.210e-03_fp_kind, &
      3.060e-03_fp_kind,   1.590e-03_fp_kind,   3.060e-03_fp_kind,   2.985e-03_fp_kind, &
      2.865e-03_fp_kind,   2.408e-03_fp_kind,   2.670e-03_fp_kind,   2.900e-03_fp_kind, &
      2.550e-03_fp_kind,   2.985e-03_fp_kind,  17.620e-03_fp_kind /)

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_H2O_LINES ) :: H2O_A4 = &
  (/       4.80_fp_kind,        4.93_fp_kind,        4.78_fp_kind,        5.30_fp_kind, &
           4.69_fp_kind,        4.85_fp_kind,        4.74_fp_kind,        5.38_fp_kind, &
           4.81_fp_kind,        4.23_fp_kind,        4.29_fp_kind,        4.23_fp_kind, &
           4.84_fp_kind,        4.57_fp_kind,        4.65_fp_kind,        5.04_fp_kind, &
           3.98_fp_kind,        4.01_fp_kind,        4.50_fp_kind,        4.50_fp_kind, &
           4.11_fp_kind,        4.68_fp_kind,        4.00_fp_kind,        4.14_fp_kind, &
           4.09_fp_kind,        5.76_fp_kind,        4.09_fp_kind,        4.53_fp_kind, &
           5.10_fp_kind,        4.70_fp_kind,        4.78_fp_kind,        5.00_fp_kind, &
           4.94_fp_kind,        4.55_fp_kind,       30.50_fp_kind /)

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_H2O_LINES ) :: H2O_A5 = &
  (/       0.69_fp_kind,        0.69_fp_kind,        0.70_fp_kind,        0.64_fp_kind, &
           0.67_fp_kind,        0.68_fp_kind,        0.69_fp_kind,        0.54_fp_kind, &
           0.63_fp_kind,        0.60_fp_kind,        0.63_fp_kind,        0.60_fp_kind, &
           0.66_fp_kind,        0.66_fp_kind,        0.65_fp_kind,        0.69_fp_kind, &
           0.61_fp_kind,        0.61_fp_kind,        0.70_fp_kind,        0.70_fp_kind, &
           0.69_fp_kind,        0.71_fp_kind,        0.60_fp_kind,        0.69_fp_kind, &
           0.68_fp_kind,        0.33_fp_kind,        0.68_fp_kind,        0.68_fp_kind, &
           0.70_fp_kind,        0.70_fp_kind,        0.70_fp_kind,        0.70_fp_kind, &
           0.64_fp_kind,        0.68_fp_kind,        2.00_fp_kind /)

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_H2O_LINES ) :: H2O_A6 = &
  (/       1.00_fp_kind,        0.82_fp_kind,        0.79_fp_kind,        0.85_fp_kind, &
           0.54_fp_kind,        0.74_fp_kind,        0.61_fp_kind,        0.89_fp_kind, &
           0.55_fp_kind,        0.48_fp_kind,        0.52_fp_kind,        0.50_fp_kind, &
           0.67_fp_kind,        0.65_fp_kind,        0.64_fp_kind,        0.72_fp_kind, &
           0.43_fp_kind,        0.45_fp_kind,        1.00_fp_kind,        1.00_fp_kind, &
           1.00_fp_kind,        0.68_fp_kind,        0.50_fp_kind,        1.00_fp_kind, &
           0.84_fp_kind,        0.45_fp_kind,        0.84_fp_kind,        0.90_fp_kind, &
           0.95_fp_kind,        0.53_fp_kind,        0.78_fp_kind,        0.80_fp_kind, &
           0.67_fp_kind,        0.90_fp_kind,        5.00_fp_kind /)

END MODULE Liebe93_Coefficients


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Liebe93_Coefficients.f90,v 1.2 2002/08/09 22:41:12 paulv Exp $
!
! $Date: 2002/08/09 22:41:12 $
!
! $Revision: 1.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Liebe93_Coefficients.f90,v $
! Revision 1.2  2002/08/09 22:41:12  paulv
! - Checking for network access.
!
! Revision 1.1  2002/05/14 23:33:52  paulv
! - Initial checkin.
! - Incomplete.
!
!
!
!
