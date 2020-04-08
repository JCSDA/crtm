!------------------------------------------------------------------------------
!M+
! NAME:
!       Liebe89_Coefficients
!
! PURPOSE:
!       Module containing line and coefficient data for microwave line-by-line
!       transmittance calculations according to Liebe 89.
!
! CATEGORY:
!       LBL : Microwave
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       USE Liebe89_Coefficients
!
! MODULES:
!       type_kinds:         Module containing definitions for kinds
!                           of variable types.
!
! CREATION HISTORY:
!       This data has been transposed from FORTRAN-77 format include
!       files written by:
!         L. Phalippou 28-Jul-1992
!         Peter Rayer UKMO
!         Roger Saunders UKMO
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


MODULE Liebe89_Coefficients


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
  (/ 50.474238_fp_kind,  50.987749_fp_kind,  51.503350_fp_kind,  52.021410_fp_kind, &
     52.542394_fp_kind,  53.066907_fp_kind,  53.595749_fp_kind,  54.130000_fp_kind, &
     54.671159_fp_kind,  55.221367_fp_kind,  55.783802_fp_kind,  56.264775_fp_kind, &
     56.363389_fp_kind,  56.968206_fp_kind,  57.612484_fp_kind,  58.323877_fp_kind, &
     58.446590_fp_kind,  59.164207_fp_kind,  59.590983_fp_kind,  60.306061_fp_kind, &
     60.434776_fp_kind,  61.150560_fp_kind,  61.800154_fp_kind,  62.411215_fp_kind, &
     62.486260_fp_kind,  62.997977_fp_kind,  63.568518_fp_kind,  64.127767_fp_kind, &
     64.678903_fp_kind,  65.224071_fp_kind,  65.764772_fp_kind,  66.302091_fp_kind, &
     66.836830_fp_kind,  67.369598_fp_kind,  67.900867_fp_kind,  68.431005_fp_kind, &
     68.960311_fp_kind, 118.750343_fp_kind, 368.498350_fp_kind, 424.763124_fp_kind, &
    487.249370_fp_kind, 715.393150_fp_kind, 773.839675_fp_kind, 834.145330_fp_kind /)


  ! -----------------------
  ! Oxygen coefficient data
  ! -----------------------

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_O2_LINES ) :: O2_A1 = &
  (/   0.94e-6_fp_kind,    2.46e-6_fp_kind,    6.08e-6_fp_kind,   14.14e-6_fp_kind, &
      31.02e-6_fp_kind,   64.10e-6_fp_kind,  124.70e-6_fp_kind,  228.00e-6_fp_kind, &
     391.80e-6_fp_kind,  631.60e-6_fp_kind,  953.50e-6_fp_kind,  548.90e-6_fp_kind, &
    1344.00e-6_fp_kind, 1763.00e-6_fp_kind, 2141.00e-6_fp_kind, 2386.00e-6_fp_kind, &
    1457.00e-6_fp_kind, 2404.00e-6_fp_kind, 2112.00e-6_fp_kind, 2124.00e-6_fp_kind, &
    2461.00e-6_fp_kind, 2504.00e-6_fp_kind, 2298.00e-6_fp_kind, 1933.00e-6_fp_kind, &
    1517.00e-6_fp_kind, 1503.00e-6_fp_kind, 1087.00e-6_fp_kind,  733.50e-6_fp_kind, &
     463.50e-6_fp_kind,  274.80e-6_fp_kind,  153.00e-6_fp_kind,   80.09e-6_fp_kind, &
      39.46e-6_fp_kind,   18.32e-6_fp_kind,    8.01e-6_fp_kind,    3.30e-6_fp_kind, &
       1.28e-6_fp_kind,  945.00e-6_fp_kind,   67.90e-6_fp_kind,  638.00e-6_fp_kind, &
     235.00e-6_fp_kind,   99.60e-6_fp_kind,  671.00e-6_fp_kind,  180.00e-6_fp_kind /)

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_O2_LINES ) :: O2_A2 = &
  (/     9.694_fp_kind,      8.694_fp_kind,      7.744_fp_kind,      6.844_fp_kind, &
         6.004_fp_kind,      5.224_fp_kind,      4.484_fp_kind,      3.814_fp_kind, &
         3.194_fp_kind,      2.624_fp_kind,      2.119_fp_kind,      0.015_fp_kind, &
         1.660_fp_kind,      1.260_fp_kind,      0.915_fp_kind,      0.626_fp_kind, &
         0.084_fp_kind,      0.391_fp_kind,      0.212_fp_kind,      0.212_fp_kind, &
         0.391_fp_kind,      0.626_fp_kind,      0.915_fp_kind,      1.260_fp_kind, &
         0.083_fp_kind,      1.665_fp_kind,      2.115_fp_kind,      2.620_fp_kind, &
         3.195_fp_kind,      3.815_fp_kind,      4.485_fp_kind,      5.225_fp_kind, &
         6.005_fp_kind,      6.845_fp_kind,      7.745_fp_kind,      8.695_fp_kind, &
         9.695_fp_kind,      0.009_fp_kind,      0.049_fp_kind,      0.044_fp_kind, &
         0.049_fp_kind,      0.145_fp_kind,      0.130_fp_kind,      0.147_fp_kind /)

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_O2_LINES ) :: O2_A3 = &
  (/   8.60e-3_fp_kind,    8.70e-3_fp_kind,    8.90e-3_fp_kind,    9.20e-3_fp_kind, &
       9.40e-3_fp_kind,    9.70e-3_fp_kind,   10.00e-3_fp_kind,   10.20e-3_fp_kind, &
      10.50e-3_fp_kind,   10.79e-3_fp_kind,   11.10e-3_fp_kind,   16.46e-3_fp_kind, &
      11.44e-3_fp_kind,   11.81e-3_fp_kind,   12.21e-3_fp_kind,   12.66e-3_fp_kind, &
      14.49e-3_fp_kind,   13.19e-3_fp_kind,   13.60e-3_fp_kind,   13.82e-3_fp_kind, &
      12.97e-3_fp_kind,   12.48e-3_fp_kind,   12.07e-3_fp_kind,   11.71e-3_fp_kind, &
      14.68e-3_fp_kind,   11.39e-3_fp_kind,   11.08e-3_fp_kind,   10.78e-3_fp_kind, &
      10.50e-3_fp_kind,   10.20e-3_fp_kind,   10.00e-3_fp_kind,    9.70e-3_fp_kind, &
       9.40e-3_fp_kind,    9.20e-3_fp_kind,    8.90e-3_fp_kind,    8.70e-3_fp_kind, &
       8.60e-3_fp_kind,   16.30e-3_fp_kind,   19.20e-3_fp_kind,   19.16e-3_fp_kind, &
      19.20e-3_fp_kind,   18.10e-3_fp_kind,   18.10e-3_fp_kind,   18.10e-3_fp_kind /)

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_O2_LINES ) :: O2_A4 = &
  (/              ZERO,               ZERO,               ZERO,               ZERO, &
                  ZERO,               ZERO,               ZERO,               ZERO, &
                  ZERO,               ZERO,               ZERO,               ZERO, &
                  ZERO,               ZERO,               ZERO,               ZERO, &
                  ZERO,               ZERO,               ZERO,               ZERO, &
                  ZERO,               ZERO,               ZERO,               ZERO, &
                  ZERO,               ZERO,               ZERO,               ZERO, &
                  ZERO,               ZERO,               ZERO,               ZERO, &
                  ZERO,               ZERO,               ZERO,               ZERO, &
                  ZERO,               ZERO,        0.6_fp_kind,        0.6_fp_kind, &
           0.6_fp_kind,        0.6_fp_kind,        0.6_fp_kind,        0.6_fp_kind /)

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_O2_LINES ) :: O2_A5 = &
  (/  1.600e-3_fp_kind,   1.400e-3_fp_kind,   1.165e-3_fp_kind,   0.883e-3_fp_kind, &
      0.579e-3_fp_kind,   0.252e-3_fp_kind,  -0.066e-3_fp_kind,  -0.314e-3_fp_kind, &
     -0.706e-3_fp_kind,  -1.151e-3_fp_kind,  -0.920e-3_fp_kind,   2.881e-3_fp_kind, &
     -0.596e-3_fp_kind,  -0.556e-3_fp_kind,  -2.414e-3_fp_kind,  -2.635e-3_fp_kind, &
      6.848e-3_fp_kind,  -6.032e-3_fp_kind,   8.266e-3_fp_kind,  -7.170e-3_fp_kind, &
      5.664e-3_fp_kind,   1.731e-3_fp_kind,   1.738e-3_fp_kind,  -0.048e-3_fp_kind, &
     -4.290e-3_fp_kind,   0.134e-3_fp_kind,   0.541e-3_fp_kind,   0.814e-3_fp_kind, &
      0.415e-3_fp_kind,   0.069e-3_fp_kind,  -0.143e-3_fp_kind,  -0.428e-3_fp_kind, &
     -0.726e-3_fp_kind,  -1.002e-3_fp_kind,  -1.255e-3_fp_kind,  -1.500e-3_fp_kind, &
     -1.700e-3_fp_kind,  -0.247e-3_fp_kind,               ZERO,               ZERO, &
                  ZERO,               ZERO,               ZERO,               ZERO /)

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_O2_LINES ) :: O2_A6 = &
  (/  5.520e-3_fp_kind,   5.520e-3_fp_kind,   5.520e-3_fp_kind,   5.520e-3_fp_kind, &
      5.520e-3_fp_kind,   5.520e-3_fp_kind,   5.520e-3_fp_kind,   5.520e-3_fp_kind, &
      5.520e-3_fp_kind,   5.514e-3_fp_kind,   5.025e-3_fp_kind,  -0.069e-3_fp_kind, &
      4.750e-3_fp_kind,   4.104e-3_fp_kind,   3.536e-3_fp_kind,   2.686e-3_fp_kind, &
     -0.647e-3_fp_kind,   1.858e-3_fp_kind,  -1.413e-3_fp_kind,   0.916e-3_fp_kind, &
     -2.323e-3_fp_kind,  -3.039e-3_fp_kind,  -3.797e-3_fp_kind,  -4.277e-3_fp_kind, &
      0.238e-3_fp_kind,  -4.860e-3_fp_kind,  -5.079e-3_fp_kind,  -5.525e-3_fp_kind, &
     -5.520e-3_fp_kind,  -5.520e-3_fp_kind,  -5.520e-3_fp_kind,  -5.520e-3_fp_kind, &
     -5.520e-3_fp_kind,  -5.520e-3_fp_kind,  -5.520e-3_fp_kind,  -5.520e-3_fp_kind, &
     -5.520e-3_fp_kind,   0.003e-3_fp_kind,               ZERO,               ZERO, &
                  ZERO,               ZERO,               ZERO,               ZERO /)




  !#----------------------------------------------------------------------------#
  !#                           -- WATER VAPOR DATA--                            #
  !#----------------------------------------------------------------------------#

  ! ---------------------------
  ! Number of Water vapor lines
  ! ---------------------------

  INTEGER, PUBLIC, PARAMETER :: N_H2O_LINES = 30


  ! -------------------------------
  ! Water vapor line frequency data
  ! -------------------------------

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_H2O_LINES ) :: H2O_LINE_FREQUENCY = &
  (/ 22.235080_fp_kind,  67.813960_fp_kind, 119.995940_fp_kind, 183.310074_fp_kind, &
    321.225644_fp_kind, 325.152919_fp_kind, 336.187000_fp_kind, 380.197372_fp_kind, &
    390.134508_fp_kind, 437.346667_fp_kind, 439.150812_fp_kind, 443.018295_fp_kind, &
    448.001075_fp_kind, 470.888947_fp_kind, 474.689127_fp_kind, 488.491133_fp_kind, &
    503.568532_fp_kind, 504.482692_fp_kind, 556.936002_fp_kind, 620.700807_fp_kind, &
    658.006500_fp_kind, 752.033227_fp_kind, 841.073593_fp_kind, 859.865000_fp_kind, &
    899.407000_fp_kind, 902.555000_fp_kind, 906.205524_fp_kind, 916.171582_fp_kind, &
    970.315022_fp_kind, 987.926764_fp_kind /)


  ! ----------------------------
  ! Water vapor coefficient data
  ! ----------------------------

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_H2O_LINES ) :: H2O_A1 = &
  (/    0.1090_fp_kind,     0.0011_fp_kind,     0.0007_fp_kind,     2.3000_fp_kind, &
        0.0464_fp_kind,     1.5400_fp_kind,     0.0010_fp_kind,    11.9000_fp_kind, &
        0.0044_fp_kind,     0.0637_fp_kind,     0.9210_fp_kind,     0.1940_fp_kind, &
       10.6000_fp_kind,     0.3300_fp_kind,     1.2800_fp_kind,     0.2530_fp_kind, &
        0.0374_fp_kind,     0.0125_fp_kind,   510.0000_fp_kind,     5.0900_fp_kind, &
        0.2740_fp_kind,   250.0000_fp_kind,     0.0130_fp_kind,     0.1330_fp_kind, &
        0.0550_fp_kind,     0.0380_fp_kind,     0.1830_fp_kind,     8.5600_fp_kind, &
        9.1600_fp_kind,   138.0000_fp_kind /)

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_H2O_LINES ) :: H2O_A2 = &
  (/     2.143_fp_kind,      8.735_fp_kind,      8.356_fp_kind,      0.668_fp_kind, &
         6.181_fp_kind,      1.540_fp_kind,      9.829_fp_kind,      1.048_fp_kind, &
         7.350_fp_kind,      5.050_fp_kind,      3.596_fp_kind,      5.050_fp_kind, &
         1.405_fp_kind,      3.599_fp_kind,      2.381_fp_kind,      2.853_fp_kind, &
         6.733_fp_kind,      6.733_fp_kind,      0.159_fp_kind,      2.200_fp_kind, &
         7.820_fp_kind,      0.396_fp_kind,      8.180_fp_kind,      7.989_fp_kind, &
         7.917_fp_kind,      8.432_fp_kind,      5.111_fp_kind,      1.442_fp_kind, &
         1.920_fp_kind,      0.258_fp_kind /)

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_H2O_LINES ) :: H2O_A3 = &
  (/  28.11e-3_fp_kind,   28.58e-3_fp_kind,   29.48e-3_fp_kind,   28.13e-3_fp_kind, &
      23.03e-3_fp_kind,   27.83e-3_fp_kind,   26.93e-3_fp_kind,   28.73e-3_fp_kind, &
      21.52e-3_fp_kind,   18.45e-3_fp_kind,   21.00e-3_fp_kind,   18.60e-3_fp_kind, &
      26.32e-3_fp_kind,   21.52e-3_fp_kind,   23.55e-3_fp_kind,   26.02e-3_fp_kind, &
      16.12e-3_fp_kind,   16.12e-3_fp_kind,   32.10e-3_fp_kind,   24.38e-3_fp_kind, &
      32.10e-3_fp_kind,   30.60e-3_fp_kind,   15.90e-3_fp_kind,   30.60e-3_fp_kind, &
      29.85e-3_fp_kind,   28.65e-3_fp_kind,   24.08e-3_fp_kind,   26.70e-3_fp_kind, &
      25.50e-3_fp_kind,   29.85e-3_fp_kind /)

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_H2O_LINES ) :: H2O_A4 = &
  (/      0.69_fp_kind,       0.69_fp_kind,       0.70_fp_kind,       0.64_fp_kind, &
          0.67_fp_kind,       0.68_fp_kind,       0.69_fp_kind,       0.69_fp_kind, &
          0.63_fp_kind,       0.60_fp_kind,       0.63_fp_kind,       0.60_fp_kind, &
          0.66_fp_kind,       0.66_fp_kind,       0.65_fp_kind,       0.69_fp_kind, &
          0.61_fp_kind,       0.61_fp_kind,       0.69_fp_kind,       0.71_fp_kind, &
          0.69_fp_kind,       0.68_fp_kind,       0.33_fp_kind,       0.68_fp_kind, &
          0.68_fp_kind,       0.70_fp_kind,       0.70_fp_kind,       0.70_fp_kind, &
          0.64_fp_kind,       0.68_fp_kind /)

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_H2O_LINES ) :: H2O_A5 = &
  (/      4.80_fp_kind,       4.93_fp_kind,       4.78_fp_kind,       5.30_fp_kind, &
          4.69_fp_kind,       4.85_fp_kind,       4.74_fp_kind,       5.38_fp_kind, &
          4.81_fp_kind,       4.23_fp_kind,       4.29_fp_kind,       4.23_fp_kind, &
          4.84_fp_kind,       4.57_fp_kind,       4.65_fp_kind,       5.04_fp_kind, &
          3.98_fp_kind,       4.01_fp_kind,       4.11_fp_kind,       4.68_fp_kind, &
          4.14_fp_kind,       4.09_fp_kind,       5.76_fp_kind,       4.09_fp_kind, &
          4.53_fp_kind,       5.10_fp_kind,       4.70_fp_kind,       4.78_fp_kind, &
          4.94_fp_kind,       4.55_fp_kind /)

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_H2O_LINES ) :: H2O_A6 = &
  (/      1.00_fp_kind,       0.82_fp_kind,       0.79_fp_kind,       0.85_fp_kind, &
          0.54_fp_kind,       0.74_fp_kind,       0.61_fp_kind,       0.84_fp_kind, &
          0.55_fp_kind,       0.48_fp_kind,       0.52_fp_kind,       0.50_fp_kind, &
          0.67_fp_kind,       0.65_fp_kind,       0.64_fp_kind,       0.72_fp_kind, &
          0.43_fp_kind,       0.45_fp_kind,       1.00_fp_kind,       0.68_fp_kind, &
          1.00_fp_kind,       0.84_fp_kind,       0.45_fp_kind,       0.84_fp_kind, &
          0.90_fp_kind,       0.95_fp_kind,       0.53_fp_kind,       0.78_fp_kind, &
          0.67_fp_kind,       0.90_fp_kind /)


END MODULE Liebe89_Coefficients


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2002/05/14 23:33:52 $
!
! $Revision$
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Liebe89_Coefficients.f90,v $
! Revision 1.1  2002/05/14 23:33:52  paulv
! - Initial checkin.
! - Incomplete.
!
!
!
!
