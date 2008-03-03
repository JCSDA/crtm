!
! Example_K_Matrix
!
! Program to provide an example of CRTM K-matrix function usage.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 01-Feb-2008
!                       paul.vandelst@noaa.gov
!

PROGRAM Example_K_Matrix

  ! ============================================================================
  ! **** ENVIRONMENT SETUP FOR RTM USAGE ****
  !
  ! Module usage
  USE CRTM_Module
  ! Disable all implicit typing
  IMPLICIT NONE
  ! ============================================================================


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'Example_K_Matrix'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id$'



  ! ============================================================================
  ! 0. **** SOME SET UP PARAMETERS FOR THIS EXAMPLE ****
  !
  ! This example processes TWO profiles of 100 layers and
  !                                          2 absorbers and
  !                                          0 clouds and
  !                                          0 absorbers....
  INTEGER, PARAMETER :: N_PROFILES  = 2
  INTEGER, PARAMETER :: N_LAYERS    = 100
  INTEGER, PARAMETER :: N_ABSORBERS = 2
  INTEGER, PARAMETER :: N_CLOUDS    = 0
  INTEGER, PARAMETER :: N_AEROSOLS  = 0
  ! ...but only ONE Sensor at a time
  INTEGER, PARAMETER :: N_SENSORS = 1

  ! Test GeometryInfo angles. The test scan angle is based
  ! on the default Re (earth radius) and h (satellite height)                                                      
  REAL(fp), PARAMETER :: ZENITH_ANGLE = 30.0_fp
  REAL(fp), PARAMETER :: SCAN_ANGLE   = 26.37293341421_fp
  ! ============================================================================
  
  
  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message, Sensor_Id
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: n_Channels
  INTEGER :: k, l, m
  INTEGER :: Sensor, Channel



  ! ============================================================================
  ! 1. **** DEFINE THE CRTM INTERFACE STRUCTURES ****
  !
  TYPE(CRTM_ChannelInfo_type) , DIMENSION(N_SENSORS)        :: ChannelInfo
  TYPE(CRTM_GeometryInfo_type), DIMENSION(N_PROFILES)       :: GeometryInfo

  ! Define the FORWARD variables
  TYPE(CRTM_Atmosphere_type)  , DIMENSION(N_PROFILES)       :: Atm
  TYPE(CRTM_Surface_type)     , DIMENSION(N_PROFILES)       :: Sfc
  TYPE(CRTM_RTSolution_type)  , DIMENSION(:,:), ALLOCATABLE :: RTSolution

  ! Define the K-MATRIX variables
  TYPE(CRTM_Atmosphere_type)  , DIMENSION(:,:), ALLOCATABLE :: Atm_K
  TYPE(CRTM_Surface_type)     , DIMENSION(:,:), ALLOCATABLE :: Sfc_K
  TYPE(CRTM_RTSolution_type)  , DIMENSION(:,:), ALLOCATABLE :: RTSolution_K
  ! ============================================================================



  ! Program header
  ! --------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to provide an example of CRTM K-matrix function usage.', &
                        '$Revision$' )


  ! Get sensor id from user
  ! -----------------------
  WRITE( *,'(/5x,"Enter sensor id [hirs4_n18, amsua_n18, or mhs_n18]: ")',ADVANCE='NO' )
  READ( *,'(a)' ) Sensor_Id
  Sensor_Id = ADJUSTL(Sensor_Id)
  
  

  ! ============================================================================
  ! 2. **** INITIALIZE THE CRTM ****
  !
  ! 2a. This initializes the CRTM for the sensors 
  !     predefined in the example SENSOR_ID parameter.
  ! --------------------------------------------------
  WRITE( *,'(/5x,"Initializing the CRTM...")' )
  Error_Status = CRTM_Init( ChannelInfo                  , &  ! This is an OUTPUT
                            Sensor_Id=(/Sensor_Id/)      , &
                            File_Path='Coefficient_Data/'  )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error initializing CRTM', & 
                          Error_Status)  
    STOP
  END IF

  ! 2b. Determine the total number of channels
  !     for which the CRTM was initialized
  ! ------------------------------------------
  n_Channels = SUM(ChannelInfo%n_Channels)
  ! ============================================================================




  ! ============================================================================
  ! 3. **** ALLOCATE STRUCTURE ARRAYS ****
  !
  ! 3a. Allocate the ARRAYS
  ! -----------------------
  ! Note that only those structure arrays with a channel
  ! dimension are allocated here because we've parameterized
  ! the number of profiles in the N_PROFILES parameter.
  !
  ! Users can make the number of profiles dynamic also, but
  ! then the INPUT arrays (Atm, Sfc) will also have to be allocated.
  ALLOCATE( RTSolution( n_Channels, N_PROFILES ), &
            Atm_K( n_Channels, N_PROFILES ), &
            Sfc_K( n_Channels, N_PROFILES ), &
            RTSolution_K( n_Channels, N_PROFILES ), &
            STAT = Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating structure arrays', & 
                           Error_Status)  
    STOP
  END IF

  ! 3b. Allocate the STRUCTURES
  ! ---------------------------
  ! The input FORWARD structure
  Error_Status = CRTM_Allocate_Atmosphere( N_LAYERS, N_ABSORBERS, N_CLOUDS, N_AEROSOLS, &
                                           Atm )
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error allocating CRTM Atmosphere structures', & 
                           Error_Status)  
   STOP
  END IF

  ! The output K-MATRIX structure
  DO m = 1, N_PROFILES
    Error_Status = CRTM_Allocate_Atmosphere( N_LAYERS, N_ABSORBERS, N_CLOUDS, N_AEROSOLS, &
                                             Atm_K(:,m) )
    IF ( Error_Status /= SUCCESS ) THEN 
      WRITE( Message,'("Error allocating CRTM Atmosphere_K structures for profile ",i0)' ) m
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), & 
                            Error_Status)  
      STOP
    END IF
  END DO
  ! ============================================================================




  ! ============================================================================
  ! 4. **** ASSIGN INPUT DATA ****
  !
  ! Fill the Atm structure array. 
  ! NOTE: This is an example program for illustrative purposes only.
  !       Typically, one would not assign the data as shown below,
  !       but rather read it from file
  
  ! 4a.1 Profile #1
  ! ---------------
  Sfc(1)%Land_Coverage    = 1.0_fp
  Sfc(1)%Land_Type        = SCRUB
  Sfc(1)%Land_Temperature = 318.0_fp
  
  Atm(1)%Climatology    = TROPICAL
  Atm(1)%Absorber_Id    = (/ H2O_ID                 , O3_ID /)
  Atm(1)%Absorber_Units = (/ MASS_MIXING_RATIO_UNITS, VOLUME_MIXING_RATIO_UNITS /)

  Atm(1)%Level_Pressure = &
  (/0.005,   0.016,   0.038,   0.077,   0.137,   0.224,   0.345,   0.506, &
    0.714,   0.975,   1.297,   1.687,   2.153,   2.701,   3.340,   4.077, &
    4.920,   5.878,   6.957,   8.165,   9.512,  11.004,  12.649,  14.456, &
   16.432,  18.585,  20.922,  23.453,  26.183,  29.121,  32.274,  35.650, &
   39.257,  43.100,  47.188,  51.528,  56.126,  60.990,  66.125,  71.540, &
   77.240,  83.231,  89.520,  96.114, 103.017, 110.237, 117.777, 125.646, &
  133.846, 142.385, 151.266, 160.496, 170.078, 180.018, 190.320, 200.989, &
  212.028, 223.441, 235.234, 247.409, 259.969, 272.919, 286.262, 300.000, &
  314.137, 328.675, 343.618, 358.967, 374.724, 390.893, 407.474, 424.470, &
  441.882, 459.712, 477.961, 496.630, 515.720, 535.232, 555.167, 575.525, &
  596.306, 617.511, 639.140, 661.192, 683.667, 706.565, 729.886, 753.627, &
  777.790, 802.371, 827.371, 852.788, 878.620, 904.866, 931.524, 958.591, &
  986.067,1013.948,1042.232,1070.917,1100.000/)
  
  Atm(1)%Pressure = &
  (/0.009,   0.026,   0.055,   0.104,   0.177,   0.281,   0.421,   0.604, &
    0.838,   1.129,   1.484,   1.910,   2.416,   3.009,   3.696,   4.485, &
    5.385,   6.402,   7.545,   8.822,  10.240,  11.807,  13.532,  15.423, &
   17.486,  19.730,  22.163,  24.793,  27.626,  30.671,  33.934,  37.425, &
   41.148,  45.113,  49.326,  53.794,  58.524,  63.523,  68.797,  74.353, &
   80.198,  86.338,  92.778,  99.526, 106.586, 113.965, 121.669, 129.703, &
  138.072, 146.781, 155.836, 165.241, 175.001, 185.121, 195.606, 206.459, &
  217.685, 229.287, 241.270, 253.637, 266.392, 279.537, 293.077, 307.014, &
  321.351, 336.091, 351.236, 366.789, 382.751, 399.126, 415.914, 433.118, &
  450.738, 468.777, 487.236, 506.115, 525.416, 545.139, 565.285, 585.854, &
  606.847, 628.263, 650.104, 672.367, 695.054, 718.163, 741.693, 765.645, &
  790.017, 814.807, 840.016, 865.640, 891.679, 918.130, 944.993, 972.264, &
  999.942,1028.025,1056.510,1085.394/)

  Atm(1)%Temperature = &
  (/229.108, 226.979, 235.291, 239.315, 243.873, 250.323, 256.563, 262.182, &
    266.536, 269.608, 270.203, 264.526, 251.578, 240.264, 235.095, 232.959, &
    233.017, 233.897, 234.385, 233.681, 232.436, 231.607, 231.192, 230.808, &
    230.088, 228.603, 226.407, 223.654, 220.525, 218.226, 216.668, 215.107, &
    213.538, 212.006, 210.507, 208.883, 206.793, 204.415, 202.058, 199.718, &
    197.668, 196.169, 194.993, 194.835, 195.648, 196.879, 198.830, 201.091, &
    203.558, 206.190, 208.900, 211.736, 214.601, 217.522, 220.457, 223.334, &
    226.156, 228.901, 231.557, 234.173, 236.788, 239.410, 242.140, 244.953, &
    247.793, 250.665, 253.216, 255.367, 257.018, 258.034, 258.778, 259.454, &
    260.225, 261.251, 262.672, 264.614, 266.854, 269.159, 271.448, 273.673, &
    275.955, 278.341, 280.822, 283.349, 285.826, 288.288, 290.721, 293.135, &
    295.609, 298.173, 300.787, 303.379, 305.960, 308.521, 310.916, 313.647, &
    315.244, 315.244, 315.244, 315.244/)

  Atm(1)%Absorber(:,1) = &
  (/1.008E-03,1.219E-03,1.618E-03,1.463E-03,2.563E-03,3.962E-03,3.965E-03,4.043E-03, &
    3.887E-03,3.593E-03,3.055E-03,2.856E-03,2.921E-03,2.555E-03,2.392E-03,2.605E-03, &
    2.573E-03,2.368E-03,2.354E-03,2.333E-03,2.312E-03,2.297E-03,2.287E-03,2.283E-03, &
    2.282E-03,2.286E-03,2.296E-03,2.309E-03,2.324E-03,2.333E-03,2.335E-03,2.335E-03, &
    2.333E-03,2.340E-03,2.361E-03,2.388E-03,2.421E-03,2.458E-03,2.492E-03,2.523E-03, &
    2.574E-03,2.670E-03,2.789E-03,2.944E-03,3.135E-03,3.329E-03,3.530E-03,3.759E-03, &
    4.165E-03,4.718E-03,5.352E-03,6.099E-03,6.845E-03,7.524E-03,8.154E-03,8.381E-03, &
    8.214E-03,8.570E-03,9.672E-03,1.246E-02,1.880E-02,2.720E-02,3.583E-02,4.462E-02, &
    4.548E-02,3.811E-02,3.697E-02,4.440E-02,2.130E-01,6.332E-01,9.945E-01,1.073E+00, &
    1.196E+00,1.674E+00,2.323E+00,2.950E+00,3.557E+00,4.148E+00,4.666E+00,5.092E+00, &
    5.487E+00,5.852E+00,6.137E+00,6.297E+00,6.338E+00,6.234E+00,5.906E+00,5.476E+00, &
    5.176E+00,4.994E+00,4.884E+00,4.832E+00,4.791E+00,4.760E+00,4.736E+00,6.368E+00, &
    7.897E+00,7.673E+00,7.458E+00,7.252E+00/)

  Atm(1)%Absorber(:,2) = &
  (/1.180E-02,7.742E-02,2.258E-01,5.138E-01,9.237E-01,1.350E+00,1.777E+00,2.221E+00, &
    2.742E+00,3.386E+00,4.164E+00,5.159E+00,6.357E+00,7.430E+00,8.174E+00,8.657E+00, &
    8.930E+00,9.056E+00,9.077E+00,8.988E+00,8.778E+00,8.480E+00,8.123E+00,7.694E+00, &
    7.207E+00,6.654E+00,6.060E+00,5.464E+00,4.874E+00,4.299E+00,3.739E+00,3.202E+00, &
    2.688E+00,2.191E+00,1.710E+00,1.261E+00,8.835E-01,5.551E-01,3.243E-01,1.975E-01, &
    1.071E-01,7.026E-02,6.153E-02,5.869E-02,6.146E-02,6.426E-02,6.714E-02,6.989E-02, &
    7.170E-02,7.272E-02,7.346E-02,7.383E-02,7.406E-02,7.418E-02,7.424E-02,7.411E-02, &
    7.379E-02,7.346E-02,7.312E-02,7.284E-02,7.274E-02,7.273E-02,7.272E-02,7.270E-02, &
    7.257E-02,7.233E-02,7.167E-02,7.047E-02,6.920E-02,6.803E-02,6.729E-02,6.729E-02, &
    6.753E-02,6.756E-02,6.717E-02,6.615E-02,6.510E-02,6.452E-02,6.440E-02,6.463E-02, &
    6.484E-02,6.487E-02,6.461E-02,6.417E-02,6.382E-02,6.378E-02,6.417E-02,6.482E-02, &
    6.559E-02,6.638E-02,6.722E-02,6.841E-02,6.944E-02,6.720E-02,6.046E-02,4.124E-02, &
    2.624E-02,2.623E-02,2.622E-02,2.622E-02/)


  ! 4a.2 Profile #2
  ! ---------------
  Sfc(2)%Land_Coverage     = 0.25_fp
  Sfc(2)%Land_Type         = SAND
  Sfc(2)%Land_Temperature  = 275.0_fp
  
  Sfc(2)%Water_Coverage    = 0.75_fp
  Sfc(2)%Water_Type        = SEA_WATER
  Sfc(2)%Water_Temperature = 272.0_fp

  Atm(2)%Climatology    = US_STANDARD_ATMOSPHERE
  Atm(2)%Absorber_Id    = (/ H2O_ID                 , O3_ID /)
  Atm(2)%Absorber_Units = (/ MASS_MIXING_RATIO_UNITS, VOLUME_MIXING_RATIO_UNITS /)

  Atm(2)%Level_Pressure = &
  (/0.005,   0.016,   0.038,   0.077,   0.137,   0.224,   0.345,   0.506, &
    0.714,   0.975,   1.297,   1.687,   2.153,   2.701,   3.340,   4.077, &
    4.920,   5.878,   6.957,   8.165,   9.512,  11.004,  12.649,  14.456, &
   16.432,  18.585,  20.922,  23.453,  26.183,  29.121,  32.274,  35.650, &
   39.257,  43.100,  47.188,  51.528,  56.126,  60.990,  66.125,  71.540, &
   77.240,  83.231,  89.520,  96.114, 103.017, 110.237, 117.777, 125.646, &
  133.846, 142.385, 151.266, 160.496, 170.078, 180.018, 190.320, 200.989, &
  212.028, 223.441, 235.234, 247.409, 259.969, 272.919, 286.262, 300.000, &
  314.137, 328.675, 343.618, 358.967, 374.724, 390.893, 407.474, 424.470, &
  441.882, 459.712, 477.961, 496.630, 515.720, 535.232, 555.167, 575.525, &
  596.306, 617.511, 639.140, 661.192, 683.667, 706.565, 729.886, 753.627, &
  777.790, 802.371, 827.371, 852.788, 878.620, 904.866, 931.524, 958.591, &
  986.067,1013.948,1042.232,1070.917,1100.000/)
  
  Atm(2)%Pressure = &
  (/0.009,   0.026,   0.055,   0.104,   0.177,   0.281,   0.421,   0.604, &
    0.838,   1.129,   1.484,   1.910,   2.416,   3.009,   3.696,   4.485, &
    5.385,   6.402,   7.545,   8.822,  10.240,  11.807,  13.532,  15.423, &
   17.486,  19.730,  22.163,  24.793,  27.626,  30.671,  33.934,  37.425, &
   41.148,  45.113,  49.326,  53.794,  58.524,  63.523,  68.797,  74.353, &
   80.198,  86.338,  92.778,  99.526, 106.586, 113.965, 121.669, 129.703, &
  138.072, 146.781, 155.836, 165.241, 175.001, 185.121, 195.606, 206.459, &
  217.685, 229.287, 241.270, 253.637, 266.392, 279.537, 293.077, 307.014, &
  321.351, 336.091, 351.236, 366.789, 382.751, 399.126, 415.914, 433.118, &
  450.738, 468.777, 487.236, 506.115, 525.416, 545.139, 565.285, 585.854, &
  606.847, 628.263, 650.104, 672.367, 695.054, 718.163, 741.693, 765.645, &
  790.017, 814.807, 840.016, 865.640, 891.679, 918.130, 944.993, 972.264, &
  999.942,1028.025,1056.510,1085.394/)

  Atm(2)%Temperature = &
  (/175.859, 182.237, 203.251, 222.895, 233.669, 239.987, 248.220, 255.085, &
    256.186, 252.608, 247.762, 243.314, 239.018, 235.282, 233.777, 234.909, &
    237.889, 241.238, 243.194, 243.304, 242.977, 243.133, 242.920, 242.026, &
    240.695, 239.379, 238.252, 236.928, 235.452, 234.561, 234.192, 233.774, &
    233.305, 233.053, 233.103, 233.307, 233.702, 234.219, 234.959, 235.940, &
    236.744, 237.155, 237.374, 238.244, 239.736, 240.672, 240.688, 240.318, &
    239.888, 239.411, 238.512, 237.048, 235.388, 233.551, 231.620, 230.418, &
    229.927, 229.511, 229.197, 228.947, 228.772, 228.649, 228.567, 228.517, &
    228.614, 228.861, 229.376, 230.223, 231.291, 232.591, 234.013, 235.508, &
    237.041, 238.589, 240.165, 241.781, 243.399, 244.985, 246.495, 247.918, &
    249.073, 250.026, 251.113, 252.321, 253.550, 254.741, 256.089, 257.692, &
    259.358, 261.010, 262.779, 264.702, 266.711, 268.863, 271.103, 272.793, &
    273.356, 273.356, 273.356, 273.356/)

  Atm(2)%Absorber(:,1) = &
  (/1.612E-03,2.746E-03,3.688E-03,3.914E-03,3.940E-03,4.837E-03,5.271E-03,4.548E-03, &
    4.187E-03,4.401E-03,4.250E-03,3.688E-03,3.516E-03,3.739E-03,3.694E-03,3.449E-03, &
    3.228E-03,3.212E-03,3.245E-03,3.067E-03,2.886E-03,2.796E-03,2.704E-03,2.617E-03, &
    2.568E-03,2.536E-03,2.506E-03,2.468E-03,2.427E-03,2.438E-03,2.493E-03,2.543E-03, &
    2.586E-03,2.632E-03,2.681E-03,2.703E-03,2.636E-03,2.512E-03,2.453E-03,2.463E-03, &
    2.480E-03,2.499E-03,2.526E-03,2.881E-03,3.547E-03,4.023E-03,4.188E-03,4.223E-03, &
    4.252E-03,4.275E-03,4.105E-03,3.675E-03,3.196E-03,2.753E-03,2.338E-03,2.347E-03, &
    2.768E-03,3.299E-03,3.988E-03,4.531E-03,4.625E-03,4.488E-03,4.493E-03,4.614E-03, &
    7.523E-03,1.329E-02,2.468E-02,4.302E-02,6.688E-02,9.692E-02,1.318E-01,1.714E-01, &
    2.149E-01,2.622E-01,3.145E-01,3.726E-01,4.351E-01,5.002E-01,5.719E-01,6.507E-01, &
    7.110E-01,7.552E-01,8.127E-01,8.854E-01,9.663E-01,1.050E+00,1.162E+00,1.316E+00, &
    1.494E+00,1.690E+00,1.931E+00,2.226E+00,2.574E+00,2.939E+00,3.187E+00,3.331E+00, &
    3.352E+00,3.260E+00,3.172E+00,3.087E+00/)

  Atm(2)%Absorber(:,2) = &
  (/3.513E-01,4.097E-01,5.161E-01,7.225E-01,1.016E+00,1.354E+00,1.767E+00,2.301E+00, &
    3.035E+00,3.943E+00,4.889E+00,5.812E+00,6.654E+00,7.308E+00,7.660E+00,7.745E+00, &
    7.696E+00,7.573E+00,7.413E+00,7.246E+00,7.097E+00,6.959E+00,6.797E+00,6.593E+00, &
    6.359E+00,6.110E+00,5.860E+00,5.573E+00,5.253E+00,4.937E+00,4.625E+00,4.308E+00, &
    3.986E+00,3.642E+00,3.261E+00,2.874E+00,2.486E+00,2.102E+00,1.755E+00,1.450E+00, &
    1.208E+00,1.087E+00,1.030E+00,1.005E+00,1.010E+00,1.028E+00,1.068E+00,1.109E+00, &
    1.108E+00,1.071E+00,9.928E-01,8.595E-01,7.155E-01,5.778E-01,4.452E-01,3.372E-01, &
    2.532E-01,1.833E-01,1.328E-01,9.394E-02,6.803E-02,5.152E-02,4.569E-02,4.855E-02, &
    5.461E-02,6.398E-02,7.205E-02,7.839E-02,8.256E-02,8.401E-02,8.412E-02,8.353E-02, &
    8.269E-02,8.196E-02,8.103E-02,7.963E-02,7.741E-02,7.425E-02,7.067E-02,6.702E-02, &
    6.368E-02,6.070E-02,5.778E-02,5.481E-02,5.181E-02,4.920E-02,4.700E-02,4.478E-02, &
    4.207E-02,3.771E-02,3.012E-02,1.941E-02,9.076E-03,2.980E-03,5.117E-03,1.160E-02, &
    1.428E-02,1.428E-02,1.428E-02,1.428E-02/)

  ! 4b. GeometryInfo input
  ! ----------------------
  ! All profiles are given the same value
  GeometryInfo%Sensor_Zenith_Angle = ZENITH_ANGLE
  GeometryInfo%Sensor_Scan_Angle   = SCAN_ANGLE
  ! ============================================================================




  ! ============================================================================
  ! 5. **** INITIALIZE THE K-MATRIX ARGUMENTS ****
  !
  ! 5a. Zero the K-matrix OUTPUT structures
  ! ---------------------------------------
  CALL CRTM_Zero_Atmosphere( Atm_K )
  CALL CRTM_Zero_Surface( Sfc_K )

  ! 5b. Inintialize the K-matrix INPUT so
  !     that all the results are dTb/dx
  ! -------------------------------------
  RTSolution_K%Brightness_Temperature = ONE
  ! ============================================================================




  ! ============================================================================
  ! 6. **** CALL THE CRTM K-MATRIX MODEL ****
  !
  Error_Status = CRTM_K_Matrix( Atm         , &  
                                Sfc         , &  
                                RTSolution_K, &  
                                GeometryInfo, &  
                                ChannelInfo , &  
                                Atm_K       , &  
                                Sfc_K       , &  
                                RTSolution    )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error in CRTM K_Matrix Model', & 
                          Error_Status)  
    STOP
  END IF
  ! ============================================================================




  ! ============================================================================
  ! 7. **** OUTPUT THE RESULTS TO SCREEN ****
  !
  ! 7a. Forward model output (RTSolution)
  ! -------------------------------------
  DO m = 1, N_PROFILES
    WRITE( *,'(/7x,"Profile ",i0," FWD output",&
              &/7x,"Sensor_Id",3x,"Channel",9x,"R",12x,"Tb")' ) m
    Sensor  = 1
    Channel = 1
    DO l = 1, n_Channels
      ! Determine the ChannelInfo sensor 
      IF ( Channel > ChannelInfo(Sensor)%n_Channels ) THEN
        Sensor  = Sensor + 1
        Channel = 1
      END IF
      WRITE( *,'(7x,a9,5x,i2,5x,es13.6,5x,f7.3)') ChannelInfo(Sensor)%Sensor_ID, &
                                                  ChannelInfo(Sensor)%Sensor_Channel(Channel), &
                                                  RTSolution(l,m)%Radiance, &
                                                  RTSolution(l,m)%Brightness_Temperature
      Channel = Channel + 1
    END DO
  END DO

  ! 7b. Sfc K-Matrix output
  ! -----------------------
  DO m = 1, N_PROFILES
    WRITE( *,'(/7x,"Profile ",i0," SFC K-Matrix output",&
              &/7x,"Sensor_Id",3x,"Channel",2x,"dTb/dTland",2x,"dTb/dTwater")' ) m
    Sensor  = 1
    Channel = 1
    DO l = 1, n_Channels
      ! Determine the ChannelInfo sensor 
      IF ( Channel > ChannelInfo(Sensor)%n_Channels ) THEN
        Sensor  = Sensor + 1
        Channel = 1
      END IF
      WRITE( *,'(7x,a9,5x,i2,5x,f7.3,5x,f7.3)') ChannelInfo(Sensor)%Sensor_ID, &
                                                ChannelInfo(Sensor)%Sensor_Channel(Channel), &
                                                Sfc_K(l,m)%Land_Temperature, &
                                                Sfc_K(l,m)%Water_Temperature
      Channel = Channel + 1
    END DO
  END DO
  
  ! 7c. Atm K-Matrix output
  ! -----------------------
  DO m = 1, N_PROFILES
    Sensor  = 1
    Channel = 1
    DO l = 1, n_Channels
      ! Determine the ChannelInfo sensor 
      IF ( Channel > ChannelInfo(Sensor)%n_Channels ) THEN
        Sensor  = Sensor + 1
        Channel = 1
      END IF
      WRITE( *,'(/7x,"Profile ",i0," ATM K-Matrix output for ",a," channel ",i0,&
               &/7x,"Pressure",6x,"dTb/dT",7x,"dTb/dH2O",8x,"dTb/dO3")') &
               m, &
               TRIM(ChannelInfo(Sensor)%Sensor_ID), &
               ChannelInfo(Sensor)%Sensor_Channel(Channel)
      DO k = 1, Atm(m)%n_Layers
        WRITE( *,'(7x,f8.3,2x,es13.6,2x,es13.6,2x,es13.6)' ) &
               Atm(m)%Pressure(k), &
               Atm_K(l,m)%Temperature(k), &
               Atm_K(l,m)%Absorber(k,1), &
               Atm_K(l,m)%Absorber(k,2)
      END DO                                        
      Channel = Channel + 1
    END DO
  END DO
  ! ============================================================================
  


  
  ! ============================================================================
  ! 8. **** DESTROY THE CRTM ****
  !
  WRITE( *, '( /5x, "Destroying the CRTM..." )' )
  Error_Status = CRTM_Destroy( ChannelInfo )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying CRTM', & 
                          Error_Status )
  END IF
  ! ============================================================================




  ! ============================================================================
  ! 9. **** CLEAN UP ****
  !
  ! 9a. Deallocate the structures.
  !     These are the explicitly allocated structures.
  !     Note that in some cases other structures, such as the Sfc
  !     and RTSolution structures, will also be allocated and thus
  !     should also be deallocated here.
  ! -------------------------------------------------------------
  Error_Status = CRTM_Destroy_Atmosphere(Atm_K)
  Error_Status = CRTM_Destroy_Atmosphere(Atm)

  ! 9b. Deallocate the arrays
  ! -------------------------
  DEALLOCATE(RTSolution, RTSolution_K, &
             Sfc_K, Atm_K, &
             STAT = Allocate_Status)
  ! ============================================================================

END PROGRAM Example_K_Matrix
