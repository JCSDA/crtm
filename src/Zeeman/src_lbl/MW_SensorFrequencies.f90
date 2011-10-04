  MODULE MW_SensorFrequencies

  USE Type_Kinds

  IMPLICIT NONE

  Private
 
  REAL( fp_kind ), PRIVATE, PARAMETER :: ZERO = 0.0_fp_kind


  !#----------------------------------------------------------------------------#
  !#         -- THE NUMBER OF CHANNELS FOR EACH INCLUDED INSTRUMENT --          #
  !#----------------------------------------------------------------------------#

  INTEGER, PUBLIC, PARAMETER :: N_AMSUA_CHANNELS = 15
  INTEGER, PUBLIC, PARAMETER :: N_AMSUB_CHANNELS =  5
  INTEGER, PUBLIC, PARAMETER :: N_SSMIS_CHANNELS = 24
 



  !#----------------------------------------------------------------------------#
  !#                   -- MICROWAVE SIDEBAND NUMBERING DATA --                  #
  !#----------------------------------------------------------------------------#

  ! -------------------------------
  ! The maximum number of sidebands
  ! -------------------------------

  INTEGER, PUBLIC, PARAMETER :: MAX_N_SIDEBANDS = 2


  ! ----------------------------
  ! The channel sideband numbers
  ! ----------------------------

  ! -- AMSU-A
  INTEGER, PUBLIC, PARAMETER, DIMENSION( N_AMSUA_CHANNELS ) :: &
    AMSUA_N_SIDEBANDS = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
                           2, 2, 2, 2, &
                           1 /)

  ! -- AMSU-B
  INTEGER, PUBLIC, PARAMETER, DIMENSION( N_AMSUB_CHANNELS ) :: &
    AMSUB_N_SIDEBANDS = 1

  ! -- SSMIS
  INTEGER, PUBLIC, PARAMETER, DIMENSION( N_SSMIS_CHANNELS ) :: &
    SSMIS_N_SIDEBANDS = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
                           1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
                           2, 2, 2, 2 /)



  !#----------------------------------------------------------------------------#
  !#                    -- MICROWAVE SENSOR FREQUENCY DATA --                   #
  !#----------------------------------------------------------------------------#


  ! --------------
  ! NOAA-17 AMSU-A
  ! --------------

  ! -- AMSU-A central frequencies in GHz
  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_AMSUA_CHANNELS ) :: AMSUA_N17_F0 = &
    (/ 23.799210_fp_kind, 31.399660_fp_kind, 50.291801_fp_kind, &
       52.800800_fp_kind, 53.596001_fp_kind, 54.400242_fp_kind, &
       54.939819_fp_kind, 55.499199_fp_kind, 57.290344_fp_kind, &
       57.290344_fp_kind, 57.290344_fp_kind, 57.290344_fp_kind, &
       57.290344_fp_kind, 57.290344_fp_kind, 88.999802_fp_kind /)

  ! -- AMSU-A I/F band limits in GHz. These are the 3dB bandpass
  ! -- filter frequencies from the AMSU-A2 and AMSU-A1 Aerojet
  ! -- calibration logbooks for AMSU-A1 S/N 104 and AMSU-A2
  ! -- S/N 104.
  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS ) :: AMSUA_N17_IF_BAND = &
    RESHAPE( (/ 0.00845_fp_kind,  0.13396_fp_kind,  ZERO,             ZERO,             &     ! AMSU-A2 ch1
                0.00893_fp_kind,  0.08951_fp_kind,  ZERO,             ZERO,             &     ! AMSU-A2 ch2
                0.00894_fp_kind,  0.08952_fp_kind,  ZERO,             ZERO,             &     ! AMSU-A1 ch3
                0.00894_fp_kind,  0.19915_fp_kind,  ZERO,             ZERO,             &     ! AMSU-A1 ch4
                0.03108_fp_kind,  0.19923_fp_kind,  ZERO,             ZERO,             &     ! AMSU-A1 ch5
                0.00894_fp_kind,  0.19920_fp_kind,  ZERO,             ZERO,             &     ! AMSU-A1 ch6
                0.00893_fp_kind,  0.19923_fp_kind,  ZERO,             ZERO,             &     ! AMSU-A1 ch7
                0.00890_fp_kind,  0.16400_fp_kind,  ZERO,             ZERO,             &     ! AMSU-A1 ch8
                0.00897_fp_kind,  0.16388_fp_kind,  ZERO,             ZERO,             &     ! AMSU-A1 ch9
                0.17901_fp_kind,  0.25566_fp_kind,  ZERO,             ZERO,             &     ! AMSU-A1 ch10
                0.257115_fp_kind, 0.291785_fp_kind, 0.353100_fp_kind, 0.387960_fp_kind, &     ! AMSU-A1 ch11
                0.292805_fp_kind, 0.308095_fp_kind, 0.337015_fp_kind, 0.352085_fp_kind, &     ! AMSU-A1 ch12
                0.308190_fp_kind, 0.316110_fp_kind, 0.328305_fp_kind, 0.336195_fp_kind, &     ! AMSU-A1 ch13
                0.316300_fp_kind, 0.319240_fp_kind, 0.325260_fp_kind, 0.328120_fp_kind, &     ! AMSU-A1 ch14
                0.49978_fp_kind,  1.49921_fp_kind,  ZERO,             ZERO              /), & ! AMSU-A1 ch15
             (/ 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS /) )


  ! --------------
  ! NOAA-17 AMSU-B
  ! --------------

  ! -- AMSU-B central frequencies in GHz
  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_AMSUB_CHANNELS ) :: AMSUB_N17_F0 = &
    (/  89.002_fp_kind, 149.984_fp_kind, 183.299_fp_kind, &
       183.299_fp_kind, 183.299_fp_kind /)


  ! -- AMSU-B I/F band limits in GHz.
  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( 2, MAX_N_SIDEBANDS, N_AMSUB_CHANNELS ) :: AMSUB_N17_IF_BAND = &
    RESHAPE( (/ 0.399_fp_kind, 1.406_fp_kind, ZERO, ZERO, &    ! AMSU-B ch16
                0.398_fp_kind, 1.402_fp_kind, ZERO, ZERO, &    ! AMSU-B ch17
                0.751_fp_kind, 1.248_fp_kind, ZERO, ZERO, &    ! AMSU-B ch18
                2.511_fp_kind, 3.267_fp_kind, ZERO, ZERO, &    ! AMSU-B ch19
                6.016_fp_kind, 7.971_fp_kind, ZERO, ZERO /), & ! AMSU-B ch20
             (/ 2, MAX_N_SIDEBANDS, N_AMSUB_CHANNELS /) )


  ! -------------
  ! DMSP-16 SSMIS
  ! -------------

  ! -- Central frequency
  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_SSMIS_CHANNELS ) :: SSMIS_F16_F0 = &
    (/ 50.300000_fp_kind,  52.800000_fp_kind,  53.596000_fp_kind,  54.400000_fp_kind, &
       55.500000_fp_kind,  57.290000_fp_kind,  59.400000_fp_kind, 150.000000_fp_kind, &
      183.310000_fp_kind, 183.310000_fp_kind, 183.310000_fp_kind,  19.350000_fp_kind, &
       19.350000_fp_kind,  22.235000_fp_kind,  37.000000_fp_kind,  37.000000_fp_kind, &
       91.655000_fp_kind,  91.655000_fp_kind,  63.283248_fp_kind,  60.792668_fp_kind, &
       60.792668_fp_kind,  60.792668_fp_kind,  60.792668_fp_kind,  60.792668_fp_kind /)

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_SSMIS_CHANNELS ) :: SSMIS_F16_IF1 = &
    (/  0.000000_fp_kind,   0.000000_fp_kind,   0.000000_fp_kind,   0.000000_fp_kind, &
        0.000000_fp_kind,   0.000000_fp_kind,   0.000000_fp_kind,   1.250000_fp_kind, &
        6.600000_fp_kind,   3.000000_fp_kind,   1.000000_fp_kind,   0.000000_fp_kind, &
        0.000000_fp_kind,   0.000000_fp_kind,   0.000000_fp_kind,   0.000000_fp_kind, &
        0.900000_fp_kind,   0.900000_fp_kind,   0.285271_fp_kind,   0.357892_fp_kind, &
        0.357892_fp_kind,   0.357892_fp_kind,   0.357892_fp_kind,   0.357892_fp_kind /)

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( N_SSMIS_CHANNELS ) :: SSMIS_F16_IF2 = &
    (/  0.000000_fp_kind,   0.000000_fp_kind,   0.000000_fp_kind,   0.000000_fp_kind, &
        0.000000_fp_kind,   0.000000_fp_kind,   0.000000_fp_kind,   0.000000_fp_kind, &
        0.000000_fp_kind,   0.000000_fp_kind,   0.000000_fp_kind,   0.000000_fp_kind, &
        0.000000_fp_kind,   0.000000_fp_kind,   0.000000_fp_kind,   0.000000_fp_kind, &
        0.000000_fp_kind,   0.000000_fp_kind,   0.000000_fp_kind,   0.000000_fp_kind, &
        0.002000_fp_kind,   0.005500_fp_kind,   0.016000_fp_kind,   0.050000_fp_kind/)

  ! -- SSMIS I/F bandwidths in GHz at F1 < F2 < F3 < F4

  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( 2, MAX_N_SIDEBANDS, N_SSMIS_CHANNELS ) :: SSMIS_F16_BW = &
    RESHAPE( (/ 0.386300_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch1
                0.385600_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch2
                0.371300_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch3
                0.375600_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch4
                0.383100_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch5
                0.333100_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch6
                0.239400_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch7
                1.648000_fp_kind,  1.648000_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch8
                1.530000_fp_kind,  1.530000_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch9
                1.017000_fp_kind,  1.017000_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch10
                0.517500_fp_kind,  0.517500_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch11
                0.356300_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch12
                0.358800_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch13
                0.420600_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch14
                1.578000_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch15
                1.542000_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch16
                1.432000_fp_kind,  1.432000_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch17
                1.401000_fp_kind,  1.401000_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch18
                0.001340_fp_kind,  0.001360_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch19
                0.001340_fp_kind,  0.001370_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch20
                0.001260_fp_kind,  0.001230_fp_kind,  0.001330_fp_kind,  0.001330_fp_kind, &     ! SSMIS ch21
                0.002620_fp_kind,  0.002610_fp_kind,  0.002660_fp_kind,  0.002670_fp_kind, &     ! SSMIS ch22
                0.007010_fp_kind,  0.007170_fp_kind,  0.007400_fp_kind,  0.007440_fp_kind, &     ! SSMIS ch23
                0.026630_fp_kind,  0.026330_fp_kind,  0.026040_fp_kind,  0.026880_fp_kind /), &  ! SSMIS ch24
             (/ 2, MAX_N_SIDEBANDS, N_SSMIS_CHANNELS /) )

  ! -- SSMIS I/F band limits in GHz.
  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( 2, MAX_N_SIDEBANDS, N_SSMIS_CHANNELS ) :: SSMIS_F16_IF_BAND = &
    RESHAPE( (/ 0.000000_fp_kind,  0.190000_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch1
                0.000000_fp_kind,  0.194400_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch2
                0.000000_fp_kind,  0.190000_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch3
                0.000000_fp_kind,  0.191250_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch4
                0.000000_fp_kind,  0.195650_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch5
                0.000000_fp_kind,  0.165000_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch6
                0.000000_fp_kind,  0.119400_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch7
                0.429000_fp_kind,  2.071000_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch8
                5.837000_fp_kind,  7.363000_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch9
                2.490500_fp_kind,  3.509500_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch10
                0.743750_fp_kind,  1.256250_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch11
                0.000000_fp_kind,  0.177500_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch12
                0.000000_fp_kind,  0.178350_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch13
                0.000000_fp_kind,  0.203750_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch14
                0.000000_fp_kind,  0.807500_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch15
                0.000000_fp_kind,  0.772500_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch16
                0.191000_fp_kind,  1.609000_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch17
                0.194500_fp_kind,  1.605500_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch18
                0.284591_fp_kind,  0.285951_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch19
                0.357217_fp_kind,  0.358567_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch20
                0.355247_fp_kind,  0.356537_fp_kind,  0.359247_fp_kind,  0.360537_fp_kind, &     ! SSMIS ch21
                0.351082_fp_kind,  0.353702_fp_kind,  0.362082_fp_kind,  0.364702_fp_kind, &     ! SSMIS ch22
                0.338232_fp_kind,  0.345552_fp_kind,  0.370232_fp_kind,  0.377552_fp_kind, &     ! SSMIS ch23
                0.294642_fp_kind,  0.321142_fp_kind,  0.394642_fp_kind,  0.421142_fp_kind /), &  ! SSMIS ch24
             (/ 2, MAX_N_SIDEBANDS, N_SSMIS_CHANNELS /) )


end MODULE MW_SensorFrequencies
