

MODULE Rosenkranz03_Coefficients


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

  INTEGER, PUBLIC, PARAMETER :: N_O2_LINES  = 40


  ! -------------------------------------------------------------
  ! Oxygen line frequency data.
  ! Lines are arranged 1-,1+,3-,3+,etc. in spin-rotation spectrum
  ! -------------------------------------------------------------

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_O2_LINES ) :: O2_LINE_FREQUENCY = &  ! F
  (/ 118.7503_fp_kind,  56.2648_fp_kind,  62.4863_fp_kind,  58.4466_fp_kind, & 
      60.3061_fp_kind,  59.5910_fp_kind,  59.1642_fp_kind,  60.4348_fp_kind, &
      58.3239_fp_kind,  61.1506_fp_kind,  57.6125_fp_kind,  61.8002_fp_kind, &
      56.9682_fp_kind,  62.4112_fp_kind,  56.3634_fp_kind,  62.9980_fp_kind, &
      55.7838_fp_kind,  63.5685_fp_kind,  55.2214_fp_kind,  64.1278_fp_kind, &
      54.6712_fp_kind,  64.6789_fp_kind,  54.1300_fp_kind,  65.2241_fp_kind, &
      53.5957_fp_kind,  65.7648_fp_kind,  53.0669_fp_kind,  66.3021_fp_kind, &
      52.5424_fp_kind,  66.8368_fp_kind,  52.0214_fp_kind,  67.3696_fp_kind, &
      51.5034_fp_kind,  67.9009_fp_kind, 368.4984_fp_kind, 424.7632_fp_kind, &
     487.2494_fp_kind, 715.3931_fp_kind, 773.8397_fp_kind, 834.1458_fp_kind /)



  ! ----------------------------------------------------------
  ! Oxygen line intensities at a reference temperature of 300K
  ! See Appendix to Chapter 2 in Atmospheric Remote Sensing
  ! by Microwave Radiometry (M.A. Janssen, ed., 1993)
  ! Equation 2A.2 and preceding text, and Table 2A.1, second
  ! column for S'(T0). Note, not all data here comes from
  ! just this source.
  ! ----------------------------------------------------------

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_O2_LINES ) :: O2_LINE_INTENSITY = &  ! S300
  (/ .2936E-14_fp_kind, .8079E-15_fp_kind, .2480E-14_fp_kind, .2228E-14_fp_kind, &
     .3351E-14_fp_kind, .3292E-14_fp_kind, .3721E-14_fp_kind, .3891E-14_fp_kind, &
     .3640E-14_fp_kind, .4005E-14_fp_kind, .3227E-14_fp_kind, .3715E-14_fp_kind, &
     .2627E-14_fp_kind, .3156E-14_fp_kind, .1982E-14_fp_kind, .2477E-14_fp_kind, &
     .1391E-14_fp_kind, .1808E-14_fp_kind, .9124E-15_fp_kind, .1230E-14_fp_kind, &
     .5603E-15_fp_kind, .7842E-15_fp_kind, .3228E-15_fp_kind, .4689E-15_fp_kind, &
     .1748E-15_fp_kind, .2632E-15_fp_kind, .8898E-16_fp_kind, .1389E-15_fp_kind, &
     .4264E-16_fp_kind, .6899E-16_fp_kind, .1924E-16_fp_kind, .3229E-16_fp_kind, &
     .8191E-17_fp_kind, .1423E-16_fp_kind, .6494E-15_fp_kind, .7083E-14_fp_kind, &
     .3025E-14_fp_kind, .1835E-14_fp_kind, .1158E-13_fp_kind, .3993E-14_fp_kind /)


  ! ---------------------------------------------------------------------
  ! Temperature "b" coefficient of line intensity from Equation 2A.2 in
  ! Atmospheric Remote Sensing by Microwave Radiometry (M.A. Janssen, ed.,
  ! 1993) and Table 2A.1. Note, not all data here comes from just this
  ! source. This coefficient is used to compute line intensities at
  ! temperatures other than the reference of 300K.
  ! ---------------------------------------------------------------------

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_O2_LINES ) :: O2_B = &  ! BE
  (/   .009_fp_kind,  .015_fp_kind,  .083_fp_kind,  .084_fp_kind, &
       .212_fp_kind,  .212_fp_kind,  .391_fp_kind,  .391_fp_kind, &
       .626_fp_kind,  .626_fp_kind,  .915_fp_kind,  .915_fp_kind, &
      1.260_fp_kind, 1.260_fp_kind, 1.660_fp_kind, 1.665_fp_kind, &
      2.119_fp_kind, 2.115_fp_kind, 2.624_fp_kind, 2.625_fp_kind, &
      3.194_fp_kind, 3.194_fp_kind, 3.814_fp_kind, 3.814_fp_kind, &
      4.484_fp_kind, 4.484_fp_kind, 5.224_fp_kind, 5.224_fp_kind, &
      6.004_fp_kind, 6.004_fp_kind, 6.844_fp_kind, 6.844_fp_kind, &
      7.744_fp_kind, 7.744_fp_kind,  .048_fp_kind,  .044_fp_kind, &
       .049_fp_kind,  .145_fp_kind,  .141_fp_kind,  .145_fp_kind /)


  ! -----------------------------------------------------------------
  ! Nonresonant frequency, wb, and exponent, x, from equation 2A.3 in
  ! Atmospheric Remote Sensing by Microwave Radiometry (M.A. Janssen,
  ! ed., 1993), used to compute pressure-broadended line half-widths.
  ! -----------------------------------------------------------------

  REAL( fp_kind ), PUBLIC, PARAMETER :: O2_WB  = 0.56_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: O2_X08 = 0.8_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: O2_X09 = 0.9_fp_kind


  ! -----------------------------------------------------------------
  ! Line half-width coefficients used to compute pressure-broadended
  ! line half-widths. See equation 2A.3 and Table 2A.1 in Atmospheric
  ! Remote Sensing by Microwave Radiometry (M.A. Janssen, ed., 1993)
  ! -----------------------------------------------------------------

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_O2_LINES ) :: O2_W = &  ! W300
  (/   1.67_fp_kind,  1.646_fp_kind, 1.468_fp_kind, 1.449_fp_kind, &
       1.382_fp_kind, 1.360_fp_kind, 1.319_fp_kind, 1.297_fp_kind, &
       1.266_fp_kind, 1.248_fp_kind, 1.221_fp_kind, 1.207_fp_kind, &
       1.181_fp_kind, 1.171_fp_kind, 1.144_fp_kind, 1.139_fp_kind, &
       1.110_fp_kind, 1.108_fp_kind, 1.079_fp_kind, 1.078_fp_kind, &
       1.05_fp_kind,  1.05_fp_kind,  1.02_fp_kind,  1.02_fp_kind,  &
       1.00_fp_kind,  1.00_fp_kind,   .97_fp_kind,   .97_fp_kind,  &
        .94_fp_kind,   .94_fp_kind,   .92_fp_kind,   .92_fp_kind,  &
        .89_fp_kind,   .89_fp_kind,  1.64_fp_kind,  1.64_fp_kind,  &
       1.64_fp_kind,  1.81_fp_kind,  1.81_fp_kind,  1.81_fp_kind  /)


  ! -------------------------------------------------------------
  ! Coefficients used to compute the line interference terms. See
  ! equations 2.56 and 2A.6, and Table 2A.1 in Atmospheric Remote
  ! Sensing by Microwave Radiometry (M.A. Janssen, ed., 1993)
  ! -------------------------------------------------------------

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_O2_LINES ) :: O2_Y = &  ! Y300
  (/   -0.036_fp_kind,   0.2408_fp_kind, -0.3486_fp_kind,  0.5227_fp_kind, &
       -0.5430_fp_kind,  0.5877_fp_kind, -0.3970_fp_kind,  0.3237_fp_kind, &
       -0.1348_fp_kind,  0.0311_fp_kind,  0.0725_fp_kind, -0.1663_fp_kind, &
        0.2832_fp_kind, -0.3629_fp_kind,  0.3970_fp_kind, -0.4599_fp_kind, &
        0.4695_fp_kind, -0.5199_fp_kind,  0.5187_fp_kind, -0.5597_fp_kind, &
        0.5903_fp_kind, -0.6246_fp_kind,  0.6656_fp_kind, -0.6942_fp_kind, &
        0.7086_fp_kind, -0.7325_fp_kind,  0.7348_fp_kind, -0.7546_fp_kind, &
        0.7702_fp_kind, -0.7864_fp_kind,  0.8083_fp_kind, -0.8210_fp_kind, &
        0.8439_fp_kind, -0.8529_fp_kind,  ZERO,            ZERO,   &
        ZERO,            ZERO,            ZERO,            ZERO   /)

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_O2_LINES ) :: O2_V = &  ! V
  (/    0.0079_fp_kind, -0.0978_fp_kind,  0.0844_fp_kind, -0.1273_fp_kind, &
        0.0699_fp_kind, -0.0776_fp_kind,  0.2309_fp_kind, -0.2825_fp_kind, &
        0.0436_fp_kind, -0.0584_fp_kind,  0.6056_fp_kind, -0.6619_fp_kind, &
        0.6451_fp_kind, -0.6759_fp_kind,  0.6547_fp_kind, -0.6675_fp_kind, &
        0.6135_fp_kind, -0.6139_fp_kind,  0.2952_fp_kind, -0.2895_fp_kind, &
        0.2654_fp_kind, -0.2590_fp_kind,  0.3750_fp_kind, -0.3680_fp_kind, &
        0.5085_fp_kind, -0.5002_fp_kind,  0.6206_fp_kind, -0.6091_fp_kind, &
        0.6526_fp_kind, -0.6393_fp_kind,  0.6640_fp_kind, -0.6475_fp_kind, &
        0.6729_fp_kind, -0.6545_fp_kind,  ZERO,            ZERO,   &
        ZERO,            ZERO,            ZERO,            ZERO   /)





  !#----------------------------------------------------------------------------#
  !#                           -- WATER VAPOR DATA--                            #
  !#----------------------------------------------------------------------------#

  ! ---------------------------
  ! Number of Water vapor lines
  ! ---------------------------

  INTEGER, PUBLIC, PARAMETER :: N_H2O_LINES = 15


  ! -------------------------------
  ! Water vapor line frequency data
  ! UNITS: GHz
  ! -------------------------------

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_H2O_LINES ) :: H2O_LINE_FREQUENCY = &  ! FL
  (/    22.2351_fp_kind, 183.3101_fp_kind, 321.2256_fp_kind, 325.1529_fp_kind, &
       380.1974_fp_kind, 439.1508_fp_kind, 443.0183_fp_kind, 448.0011_fp_kind, &
       470.8890_fp_kind, 474.6891_fp_kind, 488.4911_fp_kind, 556.9360_fp_kind, &
       620.7008_fp_kind, 752.0332_fp_kind, 916.1712_fp_kind /)


  ! ----------------------------------------------------------
  ! Water vapor line intensities at a reference temperature of
  ! 300K. See Appendix to Chapter 2 in Atmospheric Remote
  ! Sensing by Microwave Radiometry (M.A. Janssen, ed., 1993)
  ! Equation 2A.2 and preceding text. Note, not all data here
  ! comes from just this source.
  ! UNITS: cm^2.Hz
  ! ----------------------------------------------------------

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_H2O_LINES ) :: H2O_LINE_INTENSITY = &  ! S1
  (/   .1314E-13_fp_kind, .2279E-11_fp_kind, .8058E-13_fp_kind, .2701E-11_fp_kind, &
       .2444E-10_fp_kind, .2185E-11_fp_kind, .4637E-12_fp_kind, .2568E-10_fp_kind, &
       .8392E-12_fp_kind, .3272E-11_fp_kind, .6676E-12_fp_kind, .1535E-08_fp_kind, &
       .1711E-10_fp_kind, .1014E-08_fp_kind, .4238E-10_fp_kind /)


  ! ---------------------------------------------------------------------
  ! Temperature "b" coefficient of line intensity from Equation 2A.2 in
  ! Atmospheric Remote Sensing by Microwave Radiometry (M.A. Janssen, ed.,
  ! 1993). Note, not all data here comes from just this source. This
  ! coefficient is used to compute line intensities at temperatures other
  ! than the reference of 300K.
  ! UNITS: Dimensionless
  ! ---------------------------------------------------------------------

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_H2O_LINES ) :: H2O_B = &  ! B2
  (/   2.144_fp_kind,  .668_fp_kind, 6.179_fp_kind, 1.541_fp_kind, &
       1.048_fp_kind, 3.595_fp_kind, 5.048_fp_kind, 1.405_fp_kind, &
       3.597_fp_kind, 2.379_fp_kind, 2.852_fp_kind,  .159_fp_kind, &
       2.391_fp_kind,  .396_fp_kind, 1.441_fp_kind /)


  ! -----------------------------------------------------------------
  ! Line half-width coefficients used to compute foreign-broadended
  ! line half-widths. See equation 2A.3 in Atmospheric Remote Sensing
  ! by Microwave Radiometry (M.A. Janssen, ed., 1993)
  ! UNITS: GHz/hPa
  ! -----------------------------------------------------------------

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_H2O_LINES ) :: H2O_WF = &  ! W3
  (/   .00281_fp_kind, .00287_fp_kind, .0023_fp_kind,  .00278_fp_kind, &
       .00287_fp_kind, .0021_fp_kind,  .00186_fp_kind, .00263_fp_kind, &
       .00215_fp_kind, .00236_fp_kind, .0026_fp_kind,  .00321_fp_kind, &
       .00244_fp_kind, .00306_fp_kind, .00267_fp_kind /)


  ! -----------------------------------------------------------------
  ! Temperature exponent used in computing the foeign-broadened line
  ! half-widths. See equation 2A.5 in Atmospheric Remote Sensing by 
  ! Microwave Radiometry (M.A. Janssen, ed., 1993)
  ! UNITS: Dimensionless
  ! -----------------------------------------------------------------

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_H2O_LINES ) :: H2O_XF = &  ! X
  (/   .69_fp_kind, .64_fp_kind, .67_fp_kind, .68_fp_kind, &
       .54_fp_kind, .63_fp_kind, .60_fp_kind, .66_fp_kind, &
       .66_fp_kind, .65_fp_kind, .69_fp_kind, .69_fp_kind, &
       .71_fp_kind, .68_fp_kind, .70_fp_kind /)


  ! -----------------------------------------------------------------
  ! Line half-width coefficients used to compute self-broadended
  ! line half-widths. See equation 2A.3 in Atmospheric Remote Sensing
  ! by Microwave Radiometry (M.A. Janssen, ed., 1993)
  ! UNITS: GHz/hPa
  ! -----------------------------------------------------------------

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_H2O_LINES ) :: H2O_WS = &  ! WS
  (/   .01349_fp_kind, .01491_fp_kind, .0108_fp_kind,  .0135_fp_kind,  &
       .01541_fp_kind, .0090_fp_kind,  .00788_fp_kind, .01275_fp_kind, &
       .00983_fp_kind, .01095_fp_kind, .01313_fp_kind, .01320_fp_kind, &
       .01140_fp_kind, .01253_fp_kind, .01275_fp_kind /)


  ! -----------------------------------------------------------------
  ! Temperature exponent used in computing the self-broadened line
  ! half-widths. See equation 2A.5 in Atmospheric Remote Sensing by 
  ! Microwave Radiometry (M.A. Janssen, ed., 1993)
  ! UNITS: Dimensionless
  ! -----------------------------------------------------------------

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_H2O_LINES ) :: H2O_XS = &  ! XS
  (/    .61_fp_kind, .85_fp_kind, .54_fp_kind,  .74_fp_kind, &
        .89_fp_kind, .52_fp_kind, .50_fp_kind,  .67_fp_kind, &
        .65_fp_kind, .64_fp_kind, .72_fp_kind, 1.0_fp_kind,  &
        .68_fp_kind, .84_fp_kind, .78_fp_kind /)


  ! ----------------------------
  ! Ratio of line shift to width
  ! UNITS: Dimensionless
  ! ----------------------------

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_H2O_LINES ) :: H2O_SR = &  ! SR
  (/  ZERO, -0.017_fp_kind, ZERO, ZERO, &
      ZERO,  ZERO,          ZERO, ZERO, &
      ZERO,  ZERO,          ZERO, ZERO, &
      ZERO,  ZERO,          ZERO /)

END MODULE Rosenkranz03_Coefficients


