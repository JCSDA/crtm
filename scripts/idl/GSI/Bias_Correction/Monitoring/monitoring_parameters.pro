  ; -------------------------------
  ; Instrument identifiers and info
  ; -------------------------------

  INST_TAG = [ 'goes.010', $
               'goes.012', $
               'hirs2.014', $
               'msu.014', $
               'amsua.015', $
               'amsub.015', $
               'hirs3.016', $
               'amsua.016', $
               'amsub.016', $
               'hirs3.017', $
               'amsua.017', $
               'amsub.017', $
               'hirs4.018', $
               'amsua.018', $
               'mhs.018', $
               'airs.049', $
               'eos_amsua.049', $
               'hsb.049' ]

  INST_N_FOVS = [  90, $  ;GOES-10
                   90, $  ;GOES-12
                   56, $  ;NOAA-14 HIRS/2
                   90, $  ;NOAA-14 MSU
                   30, $  ;NOAA-15 AMSU-A
                   90, $  ;NOAA-15 AMSU-B
                   56, $  ;NOAA-16 HIRS/3
                   30, $  ;NOAA-16 AMSU-A
                   90, $  ;NOAA-16 AMSU-B
                   56, $  ;NOAA-17 HIRS/3
                   30, $  ;NOAA-17 AMSU-A
                   90, $  ;NOAA-17 AMSU-B
                   56, $  ;NOAA-18 HIRS/4
                   30, $  ;NOAA-18 AMSU-A
                   90, $  ;NOAA-18 MHS
                   90, $  ;AQUA AIRS
                   30, $  ;AQUA EOS_AMSUA
                   90  ]  ;AQUA HSB

  INST_FOV_ANGLE_START = [    0.0, $  ;GOES-10
                              0.0, $  ;GOES-12
                            -49.5, $  ;NOAA-14 HIRS/2
                            -47.4, $  ;NOAA-14 MSU
                            -48.3, $  ;NOAA-15 AMSU-A
                            -49.0, $  ;NOAA-15 AMSU-B
                            -49.5, $  ;NOAA-16 HIRS/3
                            -48.3, $  ;NOAA-16 AMSU-A
                            -49.0, $  ;NOAA-16 AMSU-B
                            -49.5, $  ;NOAA-17 HIRS/3
                            -48.3, $  ;NOAA-17 AMSU-A
                            -49.0, $  ;NOAA-17 AMSU-B
                            -49.5, $  ;NOAA-18 HIRS/4
                            -48.3, $  ;NOAA-18 AMSU-A
                            -49.0, $  ;NOAA-18 MHS
                            -49.0, $  ;AQUA AIRS
                            -48.3, $  ;AQUA EOS_AMSUA
                            -49.0  ]  ;AQUA HSB

  INST_FOV_ANGLE_STEP = [  1.0,  $  ;GOES-10
                           1.0,  $  ;GOES-12
                           1.80, $  ;NOAA-14 HIRS/2
                           9.47, $  ;NOAA-14 MSU
                           3.33, $  ;NOAA-15 AMSU-A
                           1.10, $  ;NOAA-15 AMSU-B
                           1.80, $  ;NOAA-16 HIRS/3
                           3.33, $  ;NOAA-16 AMSU-A
                           1.10, $  ;NOAA-16 AMSU-B
                           1.80, $  ;NOAA-17 HIRS/3
                           3.33, $  ;NOAA-17 AMSU-A
                           1.10, $  ;NOAA-17 AMSU-B
                           1.80, $  ;NOAA-18 HIRS/4
                           3.33, $  ;NOAA-18 AMSU-A
                           1.10, $  ;NOAA-18 MHS
                           1.10, $  ;AQUA AIRS
                           3.33, $  ;AQUA EOS_AMSUA
                           1.10  ]  ;AQUA HSB

  INST_N_CHANNELS = [  18, $  ;GOES-10
                       18, $  ;GOES-12
                       19, $  ;NOAA-14 HIRS/2
                        4, $  ;NOAA-14 MSU
                       15, $  ;NOAA-15 AMSU-A
                        5, $  ;NOAA-15 AMSU-B
                       19, $  ;NOAA-16 HIRS/3
                       15, $  ;NOAA-16 AMSU-A
                        5, $  ;NOAA-16 AMSU-B
                       19, $  ;NOAA-17 HIRS/3
                       15, $  ;NOAA-17 AMSU-A
                        5, $  ;NOAA-17 AMSU-B
                       19, $  ;NOAA-18 HIRS/4
                       15, $  ;NOAA-18 AMSU-A
                        5, $  ;NOAA-18 MHS
                      281, $  ;AQUA AIRS
                       15, $  ;AQUA EOS_AMSUA
                        4  ]  ;AQUA HSB

  INST_NAME = [ 'GOES-10', $
                'GOES-12', $
                'NOAA-14 HIRS/2', $
                'NOAA-14 MSU', $
                'NOAA-15 AMSU-A', $
                'NOAA-15 AMSU-B', $
                'NOAA-16 HIRS/3', $
                'NOAA-16 AMSU-A', $
                'NOAA-16 AMSU-B', $
                'NOAA-17 HIRS/3', $
                'NOAA-17 AMSU-A', $
                'NOAA-17 AMSU-B', $
                'NOAA-18 HIRS/4', $
                'NOAA-18 AMSU-A', $
                'NOAA-18 MHS', $
                'AQUA AIRS', $
                'AQUA EOS_AMSUA', $
                'AQUA HSB' ]

  ; ----------------
  ; Numeric literals
  ; ----------------

  POINT5 = 0.5d0
  ZERO = 0.0d0
  ONE  = 1.0d0
  TWO  = 2.0d0


  ; ----------------------------
  ; Angle statistics identifiers
  ; ----------------------------

  ; -- The number of time periods for the angle statistics
  N_ANGLE_TIMES = 3L
  DAY_ANGLE_TIME   = 0L
  WEEK_ANGLE_TIME  = 1L
  MONTH_ANGLE_TIME = 2L
  ANGLE_TIME_NAME = [ '1d',  $
                      '7d', $
                      '30d' ]
  I_ANGLE = [ 0, 1, 2 ] ;LINDGEN( N_ANGLE_TIMES )

  ; -- The angle stats components
  N_ANGLE_COMPONENTS = 5L ;11L
  COUNT_ANGLE      = 0L
  PENALTY_ANGLE    = 1L
  OMG_NBC_ANGLE    = 2L
  TOT_COR_ANGLE    = 3L
  OMG_BC_ANGLE     = 4L
  FIXANG_COR_ANGLE = 5L
  LAPSE_COR_ANGLE  = 6L
  LAPSE2_COR_ANGLE = 7L
  CONST_COR_ANGLE  = 8L
  SCANGL_COR_ANGLE = 9L
  CLW_COR_ANGLE    = 10L
  ANGLE_COMPONENT_NAME = [ 'Number of observations',                    $
                           'Contribution to penalty',                   $
                           'Simulated (w/o bias cor) - Observed',       $
                           'Total bias correction',                     $
                           'Simulated (w/ bias cor) - Observed',        $
                           'Fixed angle bias correction',               $
                           'Integrated lapse rate bias correction',     $
                           'Integrated (lapse rate)^2 bias correction', $
                           'Mean bias correction',                      $
                           'Scan angle bias correction',                $
                           'Cloud liquid water bias correction'         ]


  ; --------------------------------------------------
  ; The number of airmass bias correction coefficients
  ; --------------------------------------------------

  N_AIRMASS_COEFFICIENTS = 5L
  CONSTANT_COEFFICIENT   = 0L
  SCANANGLE_COEFFICIENT  = 1L
  CLW_COEFFICIENT        = 2L
  LAPSERATE2_COEFFICIENT = 3L
  LAPSERATE_COEFFICIENT  = 4L
  COEFFICIENT_NAME = [ 'Mean term',                      $
                       'Scan angle term',                $
                       'Cloud liquid water term',        $
                       'Integrated (lapse rate)^2 term', $
                       'Integrated lapse rate term'      ]


  ; ----------------------------------
  ; The number of geographical regions
  ; ----------------------------------

  N_REGIONS = 7L
  GLOBAL              = 0L
  NORTHERN_HEMISPHERE = 1L
  SOUTHERN_HEMISPHERE = 2L
  TROPICS             = 3L
  NORTH_AMERICA       = 4L
  EUROPE              = 5L
  ASIA                = 6L
  REGION_NAME = [ 'Global (180W-180E, 90S-90N)',              $
                  'Northern Hemisphere (180W-180E, 20N-80N)', $
                  'Southern Hemisphere (180W-180E, 80S-20S)', $
                  'Tropics (180W-180E, 20S-20N)',             $
                  'North America (125W-65W, 25N-55N)',        $
                  'Europe (10W-25E, 35N-70N)',                $
                  'Asia (65E-145E, 5N-45N)'                   ]


  ; -----------------------------------------------
  ; The number of components of the bias correction
  ; -----------------------------------------------

  N_BIAS_COMPONENTS = 7L
  TOTAL_BIAS      = 0L
  FIXANGLE_BIAS   = 1L
  LAPSERATE_BIAS  = 2L
  LAPSERATE2_BIAS = 3L
  CONSTANT_BIAS   = 4L
  SCANANGLE_BIAS  = 5L
  CLW_BIAS        = 6L
  BIAS_COMPONENT_NAME = [ 'Total bias correction (K)',             $
                          'Fixed angle bias correction (K)',       $
                          'Lapse rate bias correction (K)',        $
                          '(Lapse rate)^2 bias correction (K)',    $
                          'Mean bias correction (K)',              $
                          'Scan angle bias correction (K)',        $
                          'Cloud liquid water bias correction (K)' ]

  ; ------------------------
  ; The number of statistics
  ; ------------------------

  N_STATS = 2L
  MEAN_STAT   = 0L
  STDDEV_STAT = 1L
  STAT_NAME = [ 'Mean',  $
                'StdDev' ]


  ; -------------------
  ; Plotting parameters
  ; -------------------

  ; -- Plot header position
  XPOS_PLOT_HEADER = 0.05
  YPOS_PLOT_HEADER = 0.98

  ; -- X/Y-margin defaults
  DEFAULT_XMARGIN = [ 22, 7 ]
  DEFAULT_YMARGIN = !Y.MARGIN

  ; -- The min and max extent of plotting in the X and Y directions
  MINX = ZERO & MINY = ZERO
  MAXX = ONE  & MAXY = 0.925

  ; -- The default number of plots in the X and Y direction
  DEFAULT_NXPLOTS = 1
  DEFAULT_NYPLOTS = 4

  ; -- The character size in normalised units
  CHARACTER_SIZE = CONVERT_COORD( !D.X_CH_SIZE, !D.Y_CH_SIZE, /DEVICE, /TO_NORMAL )

  ; -- The plot colours and linestyles for the statistics
  PURPLE = 1
  CYAN   = 2
  YELLOW = 3
  GREEN  = 4
  RED    = 5
  BLUE   = 6

  MEAN_COLOR = BLUE
  STDDEV_COLOR = RED

  MEAN_ANGLE_COLOR   = [ PURPLE, GREEN, MEAN_COLOR ]
  STDDEV_ANGLE_COLOR = [ PURPLE, GREEN, STDDEV_COLOR ]

  MEAN_ANGLE_LINESTYLE   = REPLICATE( 0, N_ANGLE_TIMES )
  STDDEV_ANGLE_LINESTYLE = REPLICATE( 2, N_ANGLE_TIMES )

  MEAN_ANGLE_THICK   = [ 2, 2, 4 ]
  STDDEV_ANGLE_THICK = [ 2, 2, 4 ]

  ; -- Various charsize scalers
  TITLE_CHARSIZE = 1.5
  LABEL_CHARSIZE = 1.1

  ; -- Various poistional values
  LABEL_XPOS = 0.025


  ; -----------------------
  ; Data and time variables
  ; -----------------------

  DAY_NAME   = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat']
  MONTH_NAME = [ 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', $
                 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'  ]
