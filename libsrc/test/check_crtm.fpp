!
! check_crtm
!
! Check/example program for the CRTM Forward and K-Matrix functions.
!
! NOTE: No results are output or compared in this program.
!       At this stage, it is included as an example only,
!       and to determine that the CRTM library built
!       correctly such that it can be linked to create
!       an executable.
!
!

PROGRAM check_crtm

  ! ============================================================================
  ! STEP 1. **** ENVIRONMENT SETUP FOR CRTM USAGE ****
  !
  ! Module usage
  USE CRTM_Module
  ! Disable all implicit typing
  IMPLICIT NONE
  ! ============================================================================



  ! --------------------------
  ! Some non-CRTM-y Parameters
  ! --------------------------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'check_crtm'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &
    '$Id: check_crtm.fpp 60152 2015-08-13 19:19:13Z paul.vandelst@noaa.gov $'



  ! ============================================================================
  ! STEP 2. **** SET UP SOME PARAMETERS FOR THE CRTM RUN ****
  !
  ! Directory location of coefficients
#ifdef LITTLE_ENDIAN
  CHARACTER(*), PARAMETER :: ENDIAN_TYPE='little_endian'
#else
  CHARACTER(*), PARAMETER :: ENDIAN_TYPE='big_endian'
#endif
  CHARACTER(*), PARAMETER :: COEFFICIENT_PATH='coefficients/'//ENDIAN_TYPE//'/'

  ! Directory location of results for comparison [NOT USED YET]
  CHARACTER(*), PARAMETER :: RESULTS_PATH = './results/'

  ! Profile dimensions
  INTEGER, PARAMETER :: N_PROFILES  = 2
  INTEGER, PARAMETER :: N_LAYERS    = 92
  INTEGER, PARAMETER :: N_ABSORBERS = 2
  INTEGER, PARAMETER :: N_CLOUDS    = 1
  INTEGER, PARAMETER :: N_AEROSOLS  = 1
  
  ! Sensor information
  INTEGER     , PARAMETER :: N_SENSORS = 2
  CHARACTER(*), PARAMETER :: SENSOR_ID(N_SENSORS) = (/'cris399_npp','atms_npp   '/)

  ! Some pretend geometry angles. The scan angle is based
  ! on the default Re (earth radius) and h (satellite height)
  REAL(fp), PARAMETER :: ZENITH_ANGLE = 30.0_fp
  REAL(fp), PARAMETER :: SCAN_ANGLE   = 26.37293341421_fp
  ! ============================================================================
  


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: message, version
  INTEGER :: err_stat, alloc_stat
  INTEGER :: n_channels
  INTEGER :: n



  ! ============================================================================
  ! STEP 3. **** DEFINE THE CRTM INTERFACE STRUCTURES ****
  !
  ! 3a. Define the "non-demoninational" arguments
  ! ---------------------------------------------
  TYPE(CRTM_ChannelInfo_type)             :: chinfo(N_SENSORS)
  TYPE(CRTM_Geometry_type)                :: geo(N_PROFILES)


  ! 3b. Define the FORWARD variables
  ! --------------------------------
  TYPE(CRTM_Atmosphere_type)              :: atm(N_PROFILES)
  TYPE(CRTM_Surface_type)                 :: sfc(N_PROFILES)
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: rts(:,:)
  
  
  ! 3c. Define the K-MATRIX variables
  ! ---------------------------------
  TYPE(CRTM_Atmosphere_type), ALLOCATABLE :: atm_K(:,:)
  TYPE(CRTM_Surface_type)   , ALLOCATABLE :: sfc_K(:,:)
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: rts_K(:,:)
  ! ============================================================================


  ! Program header
  ! --------------
  CALL CRTM_Version( Version )
  CALL Program_Message( PROGRAM_NAME, &
    'Check/example program for the CRTM Forward and K-Matrix functions using '//&
    ENDIAN_TYPE//' coefficient datafiles', &
    'CRTM Version: '//TRIM(Version) )



  ! ============================================================================
  ! STEP 4. **** INITIALIZE THE CRTM ****
  !
  ! 4a. Initialise all the sensors at once
  ! --------------------------------------
  WRITE( *,'(/5x,"Initializing the CRTM...")' )
  err_stat = CRTM_Init( SENSOR_ID, &
                        chinfo, &
                        File_Path=COEFFICIENT_PATH, &
                        Quiet=.TRUE.)
  IF ( err_stat /= SUCCESS ) THEN
    message = 'Error initializing CRTM'
    CALL Display_Message( PROGRAM_NAME, message, FAILURE )
    STOP
  END IF

  ! 4b. Output some channel information
  ! -----------------------------------
  n_channels = SUM(CRTM_ChannelInfo_n_Channels(chinfo))
  WRITE( *,'(/5x,"Processing a total of ",i0," channels...")' ) n_channels
  DO n = 1, N_SENSORS
    WRITE( *,'(7x,i0," from ",a)' ) &
      CRTM_ChannelInfo_n_Channels(chinfo(n)), TRIM(SENSOR_ID(n))
  END DO
  ! ============================================================================



  ! Begin loop over sensors
  ! ----------------------
  Sensor_Loop: DO n = 1, N_SENSORS

  
    ! ==========================================================================
    ! STEP 5. **** ALLOCATE STRUCTURE ARRAYS ****
    !
    ! 5a. Determine the number of channels
    !     for the current sensor
    ! ------------------------------------
    n_channels = CRTM_ChannelInfo_n_Channels(chinfo(n))

    
    ! 5b. Allocate the ARRAYS
    ! -----------------------
    ALLOCATE( rts( n_channels, N_PROFILES ), &
              atm_K( n_channels, N_PROFILES ), &
              sfc_K( n_channels, N_PROFILES ), &
              rts_K( n_channels, N_PROFILES ), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) THEN
      message = 'Error allocating structure arrays'
      CALL Display_Message( PROGRAM_NAME, message, FAILURE )
      STOP
    END IF


    ! 5c. Allocate the STRUCTURE INTERNALS
    !     NOTE: Only the Atmosphere structures
    !           are allocated in this example
    ! ----------------------------------------
    ! The input FORWARD structure
    CALL CRTM_Atmosphere_Create( atm, N_LAYERS, N_ABSORBERS, N_CLOUDS, N_AEROSOLS )
    IF ( ANY(.NOT. CRTM_Atmosphere_Associated(atm)) ) THEN
      message = 'Error allocating CRTM Forward Atmosphere structure'
      CALL Display_Message( PROGRAM_NAME, message, FAILURE )
      STOP
    END IF

    ! The output K-MATRIX structure
    CALL CRTM_Atmosphere_Create( atm_K, N_LAYERS, N_ABSORBERS, N_CLOUDS, N_AEROSOLS )
    IF ( ANY(.NOT. CRTM_Atmosphere_Associated(atm_K)) ) THEN
      message = 'Error allocating CRTM K-matrix Atmosphere structure'
      CALL Display_Message( PROGRAM_NAME, message, FAILURE )
      STOP
    END IF
    ! ==========================================================================
  
  
  

    ! ==========================================================================
    ! STEP 6. **** ASSIGN INPUT DATA ****
    !
    ! 6a. Atmosphere and Surface input
    !     NOTE: that this is the hard part (in my opinion :o). The mechanism by
    !     by which the atmosphere and surface data are loaded in to their
    !     respective structures below was done purely to keep the step-by-step
    !     instructions in this program relatively "clean".
    ! ------------------------------------------------------------------------
    CALL Load_Atm_Data()
    CALL Load_Sfc_Data()


    ! 6b. Geometry input
    ! ------------------
    ! All profiles are given the same value
    !  The Sensor_Scan_Angle is optional.
    CALL CRTM_Geometry_SetValue( geo, &
                                 Sensor_Zenith_Angle = ZENITH_ANGLE, &
                                 Sensor_Scan_Angle   = SCAN_ANGLE )
    ! ==========================================================================




    ! ==========================================================================
    ! STEP 7. **** INITIALIZE THE K-MATRIX ARGUMENTS ****
    !
    ! 7a. Zero the K-matrix OUTPUT structures
    ! ---------------------------------------
    CALL CRTM_Atmosphere_Zero( atm_K )
    CALL CRTM_Surface_Zero( sfc_K )


    ! 7b. Inintialize the K-matrix INPUT so
    !     that the results are dTb/dx
    ! -------------------------------------
    rts_K%Radiance               = ZERO
    rts_K%Brightness_Temperature = ONE
    ! ==========================================================================



    
    ! ==========================================================================
    ! STEP 8. **** CALL THE CRTM FUNCTIONS FOR THE CURRENT SENSOR ****
    !
    WRITE( *, '( /5x, "Calling the CRTM functions for ",a,"..." )' ) TRIM(SENSOR_ID(n))
    
    ! 8a. The forward model
    ! ---------------------
    err_stat = CRTM_Forward( atm        , &  ! Input
                             sfc        , &  ! Input
                             geo        , &  ! Input
                             chinfo(n:n), &  ! Input
                             rts          )  ! Output
    IF ( err_stat /= SUCCESS ) THEN
      message = 'Error calling CRTM Forward Model for '//TRIM(SENSOR_ID(n))
      CALL Display_Message( PROGRAM_NAME, message, FAILURE )
      STOP
    END IF
    
    
    ! 8b. The K-matrix model
    ! ----------------------
    err_stat = CRTM_K_Matrix( atm        , &  ! FORWARD  Input
                              sfc        , &  ! FORWARD  Input
                              rts_K      , &  ! K-MATRIX Input
                              geo        , &  ! Input
                              chinfo(n:n), &  ! Input
                              atm_K      , &  ! K-MATRIX Output
                              sfc_K      , &  ! K-MATRIX Output
                              rts          )  ! FORWARD  Output
    IF ( err_stat /= SUCCESS ) THEN
      message = 'Error calling CRTM K-Matrix Model for '//TRIM(SENSOR_ID(n))
      CALL Display_Message( PROGRAM_NAME, message, FAILURE )
      STOP
    END IF
    ! ==========================================================================




    ! ==========================================================================
    ! STEP 9. **** CLEAN UP FOR NEXT SENSOR ****
    !
    ! 9a. Deallocate the structures
    ! -----------------------------
    CALL CRTM_Atmosphere_Destroy(atm_K)
    CALL CRTM_Atmosphere_Destroy(atm)


    ! 9b. Deallocate the arrays
    ! -------------------------
    DEALLOCATE(rts, rts_K, sfc_k, atm_k, STAT = alloc_stat)
    ! ==========================================================================

  END DO Sensor_Loop


  
  
  ! ==========================================================================
  ! 10. **** DESTROY THE CRTM ****
  !
  WRITE( *, '( /5x, "Destroying the CRTM..." )' )
  err_stat = CRTM_Destroy( chinfo )
  IF ( err_stat /= SUCCESS ) THEN
    message = 'Error destroying CRTM'
    CALL Display_Message( PROGRAM_NAME, message, FAILURE )
    STOP
  END IF
  ! ==========================================================================


  
  
  ! ==========================================================================
  ! 11. **** CREATE A SIGNAL FILE FOR TESTING SUCCESS ****
  !
  ! This step is just to allow the CRTM library build process 
  ! to detect success or failure at the shell level
  CALL SignalFile_Create()
  ! ==========================================================================
  

CONTAINS


  ! ==========================================================================
  !                Below are some internal procedures that load the
  !                necessary input structures with some pretend data
  ! ==========================================================================

  !
  ! Internal subprogam to load some test profile data
  !
  SUBROUTINE Load_Atm_Data()
    ! Local variables
    INTEGER :: nc
    INTEGER :: k1, k2


    ! 4a.1 Profile #1
    ! ---------------
    ! ...Profile and absorber definitions
    atm(1)%Climatology         = US_STANDARD_ATMOSPHERE
    atm(1)%Absorber_Id(1:2)    = (/ H2O_ID                 , O3_ID /)
    atm(1)%Absorber_Units(1:2) = (/ MASS_MIXING_RATIO_UNITS, VOLUME_MIXING_RATIO_UNITS /)
    ! ...Profile data
    atm(1)%Level_Pressure = &
    (/0.714_fp,   0.975_fp,   1.297_fp,   1.687_fp,   2.153_fp,   2.701_fp,   3.340_fp,   4.077_fp, &
      4.920_fp,   5.878_fp,   6.957_fp,   8.165_fp,   9.512_fp,  11.004_fp,  12.649_fp,  14.456_fp, &
     16.432_fp,  18.585_fp,  20.922_fp,  23.453_fp,  26.183_fp,  29.121_fp,  32.274_fp,  35.650_fp, &
     39.257_fp,  43.100_fp,  47.188_fp,  51.528_fp,  56.126_fp,  60.990_fp,  66.125_fp,  71.540_fp, &
     77.240_fp,  83.231_fp,  89.520_fp,  96.114_fp, 103.017_fp, 110.237_fp, 117.777_fp, 125.646_fp, &
    133.846_fp, 142.385_fp, 151.266_fp, 160.496_fp, 170.078_fp, 180.018_fp, 190.320_fp, 200.989_fp, &
    212.028_fp, 223.441_fp, 235.234_fp, 247.409_fp, 259.969_fp, 272.919_fp, 286.262_fp, 300.000_fp, &
    314.137_fp, 328.675_fp, 343.618_fp, 358.967_fp, 374.724_fp, 390.893_fp, 407.474_fp, 424.470_fp, &
    441.882_fp, 459.712_fp, 477.961_fp, 496.630_fp, 515.720_fp, 535.232_fp, 555.167_fp, 575.525_fp, &
    596.306_fp, 617.511_fp, 639.140_fp, 661.192_fp, 683.667_fp, 706.565_fp, 729.886_fp, 753.627_fp, &
    777.790_fp, 802.371_fp, 827.371_fp, 852.788_fp, 878.620_fp, 904.866_fp, 931.524_fp, 958.591_fp, &
    986.067_fp,1013.948_fp,1042.232_fp,1070.917_fp,1100.000_fp/)

    atm(1)%Pressure = &
    (/0.838_fp,   1.129_fp,   1.484_fp,   1.910_fp,   2.416_fp,   3.009_fp,   3.696_fp,   4.485_fp, &
      5.385_fp,   6.402_fp,   7.545_fp,   8.822_fp,  10.240_fp,  11.807_fp,  13.532_fp,  15.423_fp, &
     17.486_fp,  19.730_fp,  22.163_fp,  24.793_fp,  27.626_fp,  30.671_fp,  33.934_fp,  37.425_fp, &
     41.148_fp,  45.113_fp,  49.326_fp,  53.794_fp,  58.524_fp,  63.523_fp,  68.797_fp,  74.353_fp, &
     80.198_fp,  86.338_fp,  92.778_fp,  99.526_fp, 106.586_fp, 113.965_fp, 121.669_fp, 129.703_fp, &
    138.072_fp, 146.781_fp, 155.836_fp, 165.241_fp, 175.001_fp, 185.121_fp, 195.606_fp, 206.459_fp, &
    217.685_fp, 229.287_fp, 241.270_fp, 253.637_fp, 266.392_fp, 279.537_fp, 293.077_fp, 307.014_fp, &
    321.351_fp, 336.091_fp, 351.236_fp, 366.789_fp, 382.751_fp, 399.126_fp, 415.914_fp, 433.118_fp, &
    450.738_fp, 468.777_fp, 487.236_fp, 506.115_fp, 525.416_fp, 545.139_fp, 565.285_fp, 585.854_fp, &
    606.847_fp, 628.263_fp, 650.104_fp, 672.367_fp, 695.054_fp, 718.163_fp, 741.693_fp, 765.645_fp, &
    790.017_fp, 814.807_fp, 840.016_fp, 865.640_fp, 891.679_fp, 918.130_fp, 944.993_fp, 972.264_fp, &
    999.942_fp,1028.025_fp,1056.510_fp,1085.394_fp/)

    atm(1)%Temperature = &
    (/256.186_fp, 252.608_fp, 247.762_fp, 243.314_fp, 239.018_fp, 235.282_fp, 233.777_fp, 234.909_fp, &
      237.889_fp, 241.238_fp, 243.194_fp, 243.304_fp, 242.977_fp, 243.133_fp, 242.920_fp, 242.026_fp, &
      240.695_fp, 239.379_fp, 238.252_fp, 236.928_fp, 235.452_fp, 234.561_fp, 234.192_fp, 233.774_fp, &
      233.305_fp, 233.053_fp, 233.103_fp, 233.307_fp, 233.702_fp, 234.219_fp, 234.959_fp, 235.940_fp, &
      236.744_fp, 237.155_fp, 237.374_fp, 238.244_fp, 239.736_fp, 240.672_fp, 240.688_fp, 240.318_fp, &
      239.888_fp, 239.411_fp, 238.512_fp, 237.048_fp, 235.388_fp, 233.551_fp, 231.620_fp, 230.418_fp, &
      229.927_fp, 229.511_fp, 229.197_fp, 228.947_fp, 228.772_fp, 228.649_fp, 228.567_fp, 228.517_fp, &
      228.614_fp, 228.861_fp, 229.376_fp, 230.223_fp, 231.291_fp, 232.591_fp, 234.013_fp, 235.508_fp, &
      237.041_fp, 238.589_fp, 240.165_fp, 241.781_fp, 243.399_fp, 244.985_fp, 246.495_fp, 247.918_fp, &
      249.073_fp, 250.026_fp, 251.113_fp, 252.321_fp, 253.550_fp, 254.741_fp, 256.089_fp, 257.692_fp, &
      259.358_fp, 261.010_fp, 262.779_fp, 264.702_fp, 266.711_fp, 268.863_fp, 271.103_fp, 272.793_fp, &
      273.356_fp, 273.356_fp, 273.356_fp, 273.356_fp/)

    atm(1)%Absorber(:,1) = &
    (/4.187E-03_fp,4.401E-03_fp,4.250E-03_fp,3.688E-03_fp,3.516E-03_fp,3.739E-03_fp,3.694E-03_fp,3.449E-03_fp, &
      3.228E-03_fp,3.212E-03_fp,3.245E-03_fp,3.067E-03_fp,2.886E-03_fp,2.796E-03_fp,2.704E-03_fp,2.617E-03_fp, &
      2.568E-03_fp,2.536E-03_fp,2.506E-03_fp,2.468E-03_fp,2.427E-03_fp,2.438E-03_fp,2.493E-03_fp,2.543E-03_fp, &
      2.586E-03_fp,2.632E-03_fp,2.681E-03_fp,2.703E-03_fp,2.636E-03_fp,2.512E-03_fp,2.453E-03_fp,2.463E-03_fp, &
      2.480E-03_fp,2.499E-03_fp,2.526E-03_fp,2.881E-03_fp,3.547E-03_fp,4.023E-03_fp,4.188E-03_fp,4.223E-03_fp, &
      4.252E-03_fp,4.275E-03_fp,4.105E-03_fp,3.675E-03_fp,3.196E-03_fp,2.753E-03_fp,2.338E-03_fp,2.347E-03_fp, &
      2.768E-03_fp,3.299E-03_fp,3.988E-03_fp,4.531E-03_fp,4.625E-03_fp,4.488E-03_fp,4.493E-03_fp,4.614E-03_fp, &
      7.523E-03_fp,1.329E-02_fp,2.468E-02_fp,4.302E-02_fp,6.688E-02_fp,9.692E-02_fp,1.318E-01_fp,1.714E-01_fp, &
      2.149E-01_fp,2.622E-01_fp,3.145E-01_fp,3.726E-01_fp,4.351E-01_fp,5.002E-01_fp,5.719E-01_fp,6.507E-01_fp, &
      7.110E-01_fp,7.552E-01_fp,8.127E-01_fp,8.854E-01_fp,9.663E-01_fp,1.050E+00_fp,1.162E+00_fp,1.316E+00_fp, &
      1.494E+00_fp,1.690E+00_fp,1.931E+00_fp,2.226E+00_fp,2.574E+00_fp,2.939E+00_fp,3.187E+00_fp,3.331E+00_fp, &
      3.352E+00_fp,3.260E+00_fp,3.172E+00_fp,3.087E+00_fp/)

    atm(1)%Absorber(:,2) = &
    (/3.035E+00_fp,3.943E+00_fp,4.889E+00_fp,5.812E+00_fp,6.654E+00_fp,7.308E+00_fp,7.660E+00_fp,7.745E+00_fp, &
      7.696E+00_fp,7.573E+00_fp,7.413E+00_fp,7.246E+00_fp,7.097E+00_fp,6.959E+00_fp,6.797E+00_fp,6.593E+00_fp, &
      6.359E+00_fp,6.110E+00_fp,5.860E+00_fp,5.573E+00_fp,5.253E+00_fp,4.937E+00_fp,4.625E+00_fp,4.308E+00_fp, &
      3.986E+00_fp,3.642E+00_fp,3.261E+00_fp,2.874E+00_fp,2.486E+00_fp,2.102E+00_fp,1.755E+00_fp,1.450E+00_fp, &
      1.208E+00_fp,1.087E+00_fp,1.030E+00_fp,1.005E+00_fp,1.010E+00_fp,1.028E+00_fp,1.068E+00_fp,1.109E+00_fp, &
      1.108E+00_fp,1.071E+00_fp,9.928E-01_fp,8.595E-01_fp,7.155E-01_fp,5.778E-01_fp,4.452E-01_fp,3.372E-01_fp, &
      2.532E-01_fp,1.833E-01_fp,1.328E-01_fp,9.394E-02_fp,6.803E-02_fp,5.152E-02_fp,4.569E-02_fp,4.855E-02_fp, &
      5.461E-02_fp,6.398E-02_fp,7.205E-02_fp,7.839E-02_fp,8.256E-02_fp,8.401E-02_fp,8.412E-02_fp,8.353E-02_fp, &
      8.269E-02_fp,8.196E-02_fp,8.103E-02_fp,7.963E-02_fp,7.741E-02_fp,7.425E-02_fp,7.067E-02_fp,6.702E-02_fp, &
      6.368E-02_fp,6.070E-02_fp,5.778E-02_fp,5.481E-02_fp,5.181E-02_fp,4.920E-02_fp,4.700E-02_fp,4.478E-02_fp, &
      4.207E-02_fp,3.771E-02_fp,3.012E-02_fp,1.941E-02_fp,9.076E-03_fp,2.980E-03_fp,5.117E-03_fp,1.160E-02_fp, &
      1.428E-02_fp,1.428E-02_fp,1.428E-02_fp,1.428E-02_fp/)


    ! Load CO2 absorber data if there are three absorrbers
    IF ( atm(1)%n_Absorbers > 2 ) THEN
      atm(1)%Absorber_Id(3)    = CO2_ID
      atm(1)%Absorber_Units(3) = VOLUME_MIXING_RATIO_UNITS
      atm(1)%Absorber(:,3)     = 380.0_fp
    END IF


    ! Cloud data
    IF ( atm(1)%n_Clouds > 0 ) THEN
      k1 = 75
      k2 = 79
      DO nc = 1, atm(1)%n_Clouds
        atm(1)%Cloud(nc)%Type = WATER_CLOUD
        atm(1)%Cloud(nc)%Effective_Radius(k1:k2) = 20.0_fp ! microns
        atm(1)%Cloud(nc)%Water_Content(k1:k2)    = 5.0_fp  ! kg/m^2
      END DO
    END IF

    
    ! Aerosol data. Three aerosol types can be loaded:
    !   Dust, Sulphate, and Sea Salt SSCM3
    Load_Aerosol_Data_1: IF ( atm(1)%n_Aerosols > 0 ) THEN

      atm(1)%Aerosol(1)%Type = DUST_AEROSOL
      atm(1)%Aerosol(1)%Effective_Radius = & ! microns
      (/0.000000E+00_fp, 0.000000E+00_fp, &
        0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
        0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
        0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
        0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 5.305110E-16_fp, &
        7.340409E-16_fp, 1.037097E-15_fp, 1.496791E-15_fp, 2.207471E-15_fp, 3.327732E-15_fp, &
        5.128933E-15_fp, 8.083748E-15_fp, 1.303055E-14_fp, 2.148368E-14_fp, 3.622890E-14_fp, &
        6.248544E-14_fp, 1.102117E-13_fp, 1.987557E-13_fp, 3.663884E-13_fp, 6.901587E-13_fp, &
        1.327896E-12_fp, 2.608405E-12_fp, 5.228012E-12_fp, 1.068482E-11_fp, 2.225098E-11_fp, &
        4.717675E-11_fp, 1.017447E-10_fp, 2.229819E-10_fp, 4.960579E-10_fp, 1.118899E-09_fp, &
        2.555617E-09_fp, 5.902789E-09_fp, 1.376717E-08_fp, 3.237321E-08_fp, 7.662427E-08_fp, &
        1.822344E-07_fp, 4.346896E-07_fp, 1.037940E-06_fp, 2.475858E-06_fp, 5.887266E-06_fp, &
        1.392410E-05_fp, 3.267943E-05_fp, 7.592447E-05_fp, 1.741777E-04_fp, 3.935216E-04_fp, &
        8.732308E-04_fp, 1.897808E-03_fp, 4.027868E-03_fp, 8.323272E-03_fp, 1.669418E-02_fp, &
        3.239702E-02_fp, 6.063055E-02_fp, 1.090596E-01_fp, 1.878990E-01_fp, 3.089856E-01_fp, &
        4.832092E-01_fp, 7.159947E-01_fp, 1.001436E+00_fp, 1.317052E+00_fp, 1.622354E+00_fp, &
        1.864304E+00_fp, 1.990457E+00_fp, 1.966354E+00_fp, 1.789883E+00_fp, 1.494849E+00_fp, &
        1.140542E+00_fp, 7.915451E-01_fp, 4.974823E-01_fp, 2.818937E-01_fp, 1.433668E-01_fp, &
        6.514795E-02_fp, 2.633057E-02_fp, 9.421763E-03_fp, 2.971053E-03_fp, 8.218245E-04_fp/)
      atm(1)%Aerosol(1)%Concentration = & ! kg/m^2
      (/0.000000E+00_fp, 0.000000E+00_fp, &
        0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
        0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
        0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
        0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
        0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
        0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
        0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
        0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
        0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
        0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
        0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
        0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 2.458105E-18_fp, 1.983430E-16_fp, &
        1.191432E-14_fp, 5.276880E-13_fp, 1.710270E-11_fp, 4.035105E-10_fp, 6.911389E-09_fp, &
        8.594215E-08_fp, 7.781797E-07_fp, 5.162773E-06_fp, 2.534018E-05_fp, 9.325154E-05_fp, &
        2.617738E-04_fp, 5.727150E-04_fp, 1.002153E-03_fp, 1.446048E-03_fp, 1.782757E-03_fp, &
        1.955759E-03_fp, 1.999206E-03_fp, 1.994698E-03_fp, 1.913109E-03_fp, 1.656122E-03_fp, &
        1.206328E-03_fp, 6.847261E-04_fp, 2.785695E-04_fp, 7.418821E-05_fp, 1.172680E-05_fp, &
        9.900895E-07_fp, 3.987399E-08_fp, 6.786932E-10_fp, 4.291151E-12_fp, 8.785440E-15_fp/)
        
      IF ( atm(1)%n_Aerosols > 1 ) THEN
        atm(1)%Aerosol(2)%Type = SULFATE_AEROSOL
        atm(1)%Aerosol(2)%Effective_Radius = & ! microns
        (/0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp, &
          3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp, &
          3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp, &
          3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp, &
          3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp, &
          3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp, &
          3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp, &
          3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp, &
          3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp, &
          3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp, &
          3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp, &
          3.000000E-01_fp, 3.060238E-01_fp, 3.652677E-01_fp, 4.139419E-01_fp, 4.438249E-01_fp, &
          4.486394E-01_fp, 4.261471E-01_fp, 3.795067E-01_fp, 3.174571E-01_fp, 3.000000E-01_fp, &
          3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp, 3.243099E-01_fp, 4.662931E-01_fp, &
          6.103025E-01_fp, 6.958640E-01_fp, 6.776480E-01_fp, 5.570077E-01_fp, 3.828734E-01_fp, &
          3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp, 3.000000E-01_fp/)
        atm(1)%Aerosol(2)%Concentration = & ! kg/m^2
        (/0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 7.299549E-21_fp, 2.154532E-20_fp, 6.848207E-20_fp, &
          2.339296E-19_fp, 8.562906E-19_fp, 3.346100E-18_fp, 1.389284E-17_fp, 6.094260E-17_fp, &
          2.805828E-16_fp, 1.345656E-15_fp, 6.665967E-15_fp, 3.378989E-14_fp, 1.734933E-13_fp, &
          8.924837E-13_fp, 4.546743E-12_fp, 2.266249E-11_fp, 1.091369E-10_fp, 5.013496E-10_fp, &
          2.168936E-09_fp, 8.725800E-09_fp, 3.224980E-08_fp, 1.082545E-07_fp, 3.266343E-07_fp, &
          8.780083E-07_fp, 2.087760E-06_fp, 4.370441E-06_fp, 8.038113E-06_fp, 1.300537E-05_fp, &
          1.860671E-05_fp, 2.376757E-05_fp, 2.751048E-05_fp, 2.945706E-05_fp, 2.998589E-05_fp, &
          2.995521E-05_fp, 2.909387E-05_fp, 2.609907E-05_fp, 2.031620E-05_fp, 1.274989E-05_fp, &
          5.920554E-06_fp, 1.842346E-06_fp, 3.429331E-07_fp, 3.355556E-08_fp, 1.506455E-09_fp, &
          1.720306E-10_fp, 1.161071E-09_fp, 7.599420E-09_fp, 4.096076E-08_fp, 1.815570E-07_fp, &
          6.623233E-07_fp, 1.994766E-06_fp, 4.987904E-06_fp, 1.044158E-05_fp, 1.850659E-05_fp, &
          2.817442E-05_fp, 3.750360E-05_fp, 4.459276E-05_fp, 4.857087E-05_fp, 4.990199E-05_fp, &
          4.998888E-05_fp, 4.922362E-05_fp, 4.582548E-05_fp, 3.844906E-05_fp, 2.757877E-05_fp, &
          1.615474E-05_fp, 9.509965E-06_fp, 1.672265E-05_fp, 4.602962E-05_fp, 8.740809E-05_fp, &
          1.165118E-04_fp, 1.248318E-04_fp, 1.240508E-04_fp, 1.095622E-04_fp, 7.116027E-05_fp, &
          2.756351E-05_fp, 5.072010E-06_fp, 3.467497E-07_fp, 6.759169E-09_fp, 2.828000E-11_fp/)
      END IF
      
      IF ( atm(1)%n_Aerosols > 2 ) THEN
        atm(1)%Aerosol(3)%Type = SEASALT_SSCM3_AEROSOL
        atm(1)%Aerosol(3)%Effective_Radius = & ! microns
        (/7.600000E+00_fp, 7.600000E+00_fp, &
          7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, &
          7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, &
          7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, &
          7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, &
          7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, &
          7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, &
          7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, &
          7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, &
          7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, &
          7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, &
          7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, &
          7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, &
          7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, &
          7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, &
          7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, &
          7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, &
          7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, &
          7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp, 7.600000E+00_fp/)
        atm(1)%Aerosol(3)%Concentration = & ! kg/m^2
        (/1.834405E-15_fp, 2.004881E-15_fp, &
          2.234084E-15_fp, 2.543453E-15_fp, 2.964461E-15_fp, 3.544295E-15_fp, 4.355235E-15_fp, &
          5.510452E-15_fp, 7.191267E-15_fp, 9.695182E-15_fp, 1.352261E-14_fp, 1.953716E-14_fp, &
          2.926925E-14_fp, 4.550553E-14_fp, 7.346181E-14_fp, 1.231759E-13_fp, 2.145104E-13_fp, &
          3.878653E-13_fp, 7.276576E-13_fp, 1.414927E-12_fp, 2.847645E-12_fp, 5.921044E-12_fp, &
          1.269153E-11_fp, 2.797048E-11_fp, 6.318984E-11_fp, 1.458383E-10_fp, 3.425444E-10_fp, &
          8.153831E-10_fp, 1.958067E-09_fp, 4.720525E-09_fp, 1.136570E-08_fp, 2.718180E-08_fp, &
          6.420674E-08_fp, 1.489302E-07_fp, 3.372331E-07_fp, 7.410874E-07_fp, 1.571399E-06_fp, &
          3.197064E-06_fp, 6.208220E-06_fp, 1.145048E-05_fp, 1.997373E-05_fp, 3.283395E-05_fp, &
          5.072822E-05_fp, 7.354173E-05_fp, 1.000035E-04_fp, 1.276931E-04_fp, 1.535301E-04_fp, &
          1.746342E-04_fp, 1.892127E-04_fp, 1.971011E-04_fp, 1.997815E-04_fp, 1.999842E-04_fp, &
          1.985580E-04_fp, 1.917087E-04_fp, 1.753846E-04_fp, 1.474980E-04_fp, 1.101113E-04_fp, &
          7.010137E-05_fp, 3.636523E-05_fp, 1.460058E-05_fp, 4.282477E-06_fp, 8.603007E-07_fp, &
          1.101800E-07_fp, 8.310010E-09_fp, 3.382006E-10_fp, 6.751810E-12_fp, 3.060195E-13_fp, &
          9.145434E-12_fp, 2.343817E-10_fp, 4.156377E-09_fp, 5.122906E-08_fp, 4.424084E-07_fp, &
          2.708849E-06_fp, 1.194846E-05_fp, 3.874236E-05_fp, 9.466062E-05_fp, 1.795200E-04_fp, &
          2.735688E-04_fp, 3.486493E-04_fp, 3.889143E-04_fp, 3.997242E-04_fp, 3.991008E-04_fp, &
          3.826235E-04_fp, 3.287943E-04_fp, 2.344766E-04_fp, 1.275907E-04_fp, 4.835821E-05_fp, &
          1.156687E-05_fp, 1.570009E-06_fp, 1.078885E-07_fp, 3.321985E-09_fp, 4.023206E-11_fp/)
      END IF
    END IF Load_Aerosol_Data_1



    ! 4a.2 Profile #2
    ! ---------------
    ! ...Profile and absorber definitions
    atm(2)%Climatology         = TROPICAL
    atm(2)%Absorber_Id(1:2)    = (/ H2O_ID                 , O3_ID /)
    atm(2)%Absorber_Units(1:2) = (/ MASS_MIXING_RATIO_UNITS, VOLUME_MIXING_RATIO_UNITS /)
    ! ...Profile data
    atm(2)%Level_Pressure = &
    (/0.714_fp,   0.975_fp,   1.297_fp,   1.687_fp,   2.153_fp,   2.701_fp,   3.340_fp,   4.077_fp, &
      4.920_fp,   5.878_fp,   6.957_fp,   8.165_fp,   9.512_fp,  11.004_fp,  12.649_fp,  14.456_fp, &
     16.432_fp,  18.585_fp,  20.922_fp,  23.453_fp,  26.183_fp,  29.121_fp,  32.274_fp,  35.650_fp, &
     39.257_fp,  43.100_fp,  47.188_fp,  51.528_fp,  56.126_fp,  60.990_fp,  66.125_fp,  71.540_fp, &
     77.240_fp,  83.231_fp,  89.520_fp,  96.114_fp, 103.017_fp, 110.237_fp, 117.777_fp, 125.646_fp, &
    133.846_fp, 142.385_fp, 151.266_fp, 160.496_fp, 170.078_fp, 180.018_fp, 190.320_fp, 200.989_fp, &
    212.028_fp, 223.441_fp, 235.234_fp, 247.409_fp, 259.969_fp, 272.919_fp, 286.262_fp, 300.000_fp, &
    314.137_fp, 328.675_fp, 343.618_fp, 358.967_fp, 374.724_fp, 390.893_fp, 407.474_fp, 424.470_fp, &
    441.882_fp, 459.712_fp, 477.961_fp, 496.630_fp, 515.720_fp, 535.232_fp, 555.167_fp, 575.525_fp, &
    596.306_fp, 617.511_fp, 639.140_fp, 661.192_fp, 683.667_fp, 706.565_fp, 729.886_fp, 753.627_fp, &
    777.790_fp, 802.371_fp, 827.371_fp, 852.788_fp, 878.620_fp, 904.866_fp, 931.524_fp, 958.591_fp, &
    986.067_fp,1013.948_fp,1042.232_fp,1070.917_fp,1100.000_fp/)

    atm(2)%Pressure = &
    (/0.838_fp,   1.129_fp,   1.484_fp,   1.910_fp,   2.416_fp,   3.009_fp,   3.696_fp,   4.485_fp, &
      5.385_fp,   6.402_fp,   7.545_fp,   8.822_fp,  10.240_fp,  11.807_fp,  13.532_fp,  15.423_fp, &
     17.486_fp,  19.730_fp,  22.163_fp,  24.793_fp,  27.626_fp,  30.671_fp,  33.934_fp,  37.425_fp, &
     41.148_fp,  45.113_fp,  49.326_fp,  53.794_fp,  58.524_fp,  63.523_fp,  68.797_fp,  74.353_fp, &
     80.198_fp,  86.338_fp,  92.778_fp,  99.526_fp, 106.586_fp, 113.965_fp, 121.669_fp, 129.703_fp, &
    138.072_fp, 146.781_fp, 155.836_fp, 165.241_fp, 175.001_fp, 185.121_fp, 195.606_fp, 206.459_fp, &
    217.685_fp, 229.287_fp, 241.270_fp, 253.637_fp, 266.392_fp, 279.537_fp, 293.077_fp, 307.014_fp, &
    321.351_fp, 336.091_fp, 351.236_fp, 366.789_fp, 382.751_fp, 399.126_fp, 415.914_fp, 433.118_fp, &
    450.738_fp, 468.777_fp, 487.236_fp, 506.115_fp, 525.416_fp, 545.139_fp, 565.285_fp, 585.854_fp, &
    606.847_fp, 628.263_fp, 650.104_fp, 672.367_fp, 695.054_fp, 718.163_fp, 741.693_fp, 765.645_fp, &
    790.017_fp, 814.807_fp, 840.016_fp, 865.640_fp, 891.679_fp, 918.130_fp, 944.993_fp, 972.264_fp, &
    999.942_fp,1028.025_fp,1056.510_fp,1085.394_fp/)

    atm(2)%Temperature = &
    (/266.536_fp, 269.608_fp, 270.203_fp, 264.526_fp, 251.578_fp, 240.264_fp, 235.095_fp, 232.959_fp, &
      233.017_fp, 233.897_fp, 234.385_fp, 233.681_fp, 232.436_fp, 231.607_fp, 231.192_fp, 230.808_fp, &
      230.088_fp, 228.603_fp, 226.407_fp, 223.654_fp, 220.525_fp, 218.226_fp, 216.668_fp, 215.107_fp, &
      213.538_fp, 212.006_fp, 210.507_fp, 208.883_fp, 206.793_fp, 204.415_fp, 202.058_fp, 199.718_fp, &
      197.668_fp, 196.169_fp, 194.993_fp, 194.835_fp, 195.648_fp, 196.879_fp, 198.830_fp, 201.091_fp, &
      203.558_fp, 206.190_fp, 208.900_fp, 211.736_fp, 214.601_fp, 217.522_fp, 220.457_fp, 223.334_fp, &
      226.156_fp, 228.901_fp, 231.557_fp, 234.173_fp, 236.788_fp, 239.410_fp, 242.140_fp, 244.953_fp, &
      247.793_fp, 250.665_fp, 253.216_fp, 255.367_fp, 257.018_fp, 258.034_fp, 258.778_fp, 259.454_fp, &
      260.225_fp, 261.251_fp, 262.672_fp, 264.614_fp, 266.854_fp, 269.159_fp, 271.448_fp, 273.673_fp, &
      275.955_fp, 278.341_fp, 280.822_fp, 283.349_fp, 285.826_fp, 288.288_fp, 290.721_fp, 293.135_fp, &
      295.609_fp, 298.173_fp, 300.787_fp, 303.379_fp, 305.960_fp, 308.521_fp, 310.916_fp, 313.647_fp, &
      315.244_fp, 315.244_fp, 315.244_fp, 315.244_fp/)

    atm(2)%Absorber(:,1) = &
    (/3.887E-03_fp,3.593E-03_fp,3.055E-03_fp,2.856E-03_fp,2.921E-03_fp,2.555E-03_fp,2.392E-03_fp,2.605E-03_fp, &
      2.573E-03_fp,2.368E-03_fp,2.354E-03_fp,2.333E-03_fp,2.312E-03_fp,2.297E-03_fp,2.287E-03_fp,2.283E-03_fp, &
      2.282E-03_fp,2.286E-03_fp,2.296E-03_fp,2.309E-03_fp,2.324E-03_fp,2.333E-03_fp,2.335E-03_fp,2.335E-03_fp, &
      2.333E-03_fp,2.340E-03_fp,2.361E-03_fp,2.388E-03_fp,2.421E-03_fp,2.458E-03_fp,2.492E-03_fp,2.523E-03_fp, &
      2.574E-03_fp,2.670E-03_fp,2.789E-03_fp,2.944E-03_fp,3.135E-03_fp,3.329E-03_fp,3.530E-03_fp,3.759E-03_fp, &
      4.165E-03_fp,4.718E-03_fp,5.352E-03_fp,6.099E-03_fp,6.845E-03_fp,7.524E-03_fp,8.154E-03_fp,8.381E-03_fp, &
      8.214E-03_fp,8.570E-03_fp,9.672E-03_fp,1.246E-02_fp,1.880E-02_fp,2.720E-02_fp,3.583E-02_fp,4.462E-02_fp, &
      4.548E-02_fp,3.811E-02_fp,3.697E-02_fp,4.440E-02_fp,2.130E-01_fp,6.332E-01_fp,9.945E-01_fp,1.073E+00_fp, &
      1.196E+00_fp,1.674E+00_fp,2.323E+00_fp,2.950E+00_fp,3.557E+00_fp,4.148E+00_fp,4.666E+00_fp,5.092E+00_fp, &
      5.487E+00_fp,5.852E+00_fp,6.137E+00_fp,6.297E+00_fp,6.338E+00_fp,6.234E+00_fp,5.906E+00_fp,5.476E+00_fp, &
      5.176E+00_fp,4.994E+00_fp,4.884E+00_fp,4.832E+00_fp,4.791E+00_fp,4.760E+00_fp,4.736E+00_fp,6.368E+00_fp, &
      7.897E+00_fp,7.673E+00_fp,7.458E+00_fp,7.252E+00_fp/)

    atm(2)%Absorber(:,2) = &
    (/2.742E+00_fp,3.386E+00_fp,4.164E+00_fp,5.159E+00_fp,6.357E+00_fp,7.430E+00_fp,8.174E+00_fp,8.657E+00_fp, &
      8.930E+00_fp,9.056E+00_fp,9.077E+00_fp,8.988E+00_fp,8.778E+00_fp,8.480E+00_fp,8.123E+00_fp,7.694E+00_fp, &
      7.207E+00_fp,6.654E+00_fp,6.060E+00_fp,5.464E+00_fp,4.874E+00_fp,4.299E+00_fp,3.739E+00_fp,3.202E+00_fp, &
      2.688E+00_fp,2.191E+00_fp,1.710E+00_fp,1.261E+00_fp,8.835E-01_fp,5.551E-01_fp,3.243E-01_fp,1.975E-01_fp, &
      1.071E-01_fp,7.026E-02_fp,6.153E-02_fp,5.869E-02_fp,6.146E-02_fp,6.426E-02_fp,6.714E-02_fp,6.989E-02_fp, &
      7.170E-02_fp,7.272E-02_fp,7.346E-02_fp,7.383E-02_fp,7.406E-02_fp,7.418E-02_fp,7.424E-02_fp,7.411E-02_fp, &
      7.379E-02_fp,7.346E-02_fp,7.312E-02_fp,7.284E-02_fp,7.274E-02_fp,7.273E-02_fp,7.272E-02_fp,7.270E-02_fp, &
      7.257E-02_fp,7.233E-02_fp,7.167E-02_fp,7.047E-02_fp,6.920E-02_fp,6.803E-02_fp,6.729E-02_fp,6.729E-02_fp, &
      6.753E-02_fp,6.756E-02_fp,6.717E-02_fp,6.615E-02_fp,6.510E-02_fp,6.452E-02_fp,6.440E-02_fp,6.463E-02_fp, &
      6.484E-02_fp,6.487E-02_fp,6.461E-02_fp,6.417E-02_fp,6.382E-02_fp,6.378E-02_fp,6.417E-02_fp,6.482E-02_fp, &
      6.559E-02_fp,6.638E-02_fp,6.722E-02_fp,6.841E-02_fp,6.944E-02_fp,6.720E-02_fp,6.046E-02_fp,4.124E-02_fp, &
      2.624E-02_fp,2.623E-02_fp,2.622E-02_fp,2.622E-02_fp/)


    ! Load CO2 absorrber data if there are three absorrbers
    IF ( atm(2)%n_Absorbers > 2 ) THEN
      atm(2)%Absorber_Id(3)    = CO2_ID
      atm(2)%Absorber_Units(3) = VOLUME_MIXING_RATIO_UNITS
      atm(2)%Absorber(:,3) =  &
      (/1.100e+02_fp,2.700e+02_fp,3.200e+02_fp,3.300e+02_fp,3.200e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp, &
        3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp, &
        3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp, &
        3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp, &
        3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp, &
        3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp, &
        3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp, &
        3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp, &
        3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp, &
        3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp, &
        3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp, &
        3.300e+02_fp,3.300e+02_fp,3.300e+02_fp,3.300e+02_fp /)
    END IF

      
    ! Cloud data
    IF ( atm(2)%n_Clouds > 0 ) THEN
      k1 = 73
      k2 = 90
      DO nc = 1, atm(2)%n_Clouds
        atm(2)%Cloud(nc)%Type = RAIN_CLOUD
        atm(2)%Cloud(nc)%Effective_Radius(k1:k2) = 1000.0_fp ! microns
        atm(2)%Cloud(nc)%Water_Content(k1:k2)    =    5.0_fp ! kg/m^2
      END DO 
    END IF
    
    
    ! Aerosol data. Three aerosol types can be loaded:
    !   Sea Sat SSAM, Sea Salt SSCM1, and Sea Salt SSCM2
    Load_Aerosol_Data_2: IF ( atm(2)%n_Aerosols > 0 ) THEN
    
      atm(2)%Aerosol(1)%Type = SEASALT_SSAM_AEROSOL
      atm(2)%Aerosol(1)%Effective_Radius = & ! microns
      (/0.000000E+00_fp, 0.000000E+00_fp, &
        0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
        0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
        0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
        0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
        0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
        0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
        0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
        0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
        3.500000E-01_fp, 3.500000E-01_fp, 3.500000E-01_fp, 3.500000E-01_fp, 3.500000E-01_fp, &
        3.500000E-01_fp, 3.500000E-01_fp, 3.500000E-01_fp, 3.500000E-01_fp, 3.500000E-01_fp, &
        3.500000E-01_fp, 3.500000E-01_fp, 3.500000E-01_fp, 3.500000E-01_fp, 3.500000E-01_fp, &
        3.500000E-01_fp, 3.500000E-01_fp, 3.500000E-01_fp, 3.500000E-01_fp, 3.500000E-01_fp, &
        3.500000E-01_fp, 3.500000E-01_fp, 3.500000E-01_fp, 3.500000E-01_fp, 3.500000E-01_fp, &
        3.500000E-01_fp, 3.500000E-01_fp, 3.500000E-01_fp, 3.500000E-01_fp, 3.500000E-01_fp, &
        3.500000E-01_fp, 4.172383E-01_fp, 5.083015E-01_fp, 6.111266E-01_fp, 7.244139E-01_fp, &
        8.457720E-01_fp, 9.716019E-01_fp, 1.097090E+00_fp, 1.216347E+00_fp, 1.322729E+00_fp, &
        1.400000E+00_fp, 1.400000E+00_fp, 1.400000E+00_fp, 1.400000E+00_fp, 1.400000E+00_fp, &
        1.370222E+00_fp, 1.261597E+00_fp, 1.129123E+00_fp, 9.811745E-01_fp, 8.268477E-01_fp/)
      atm(2)%Aerosol(1)%Concentration = & ! kg/m^2
      (/0.000000E+00_fp, 0.000000E+00_fp, &
        0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
        0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
        0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
        0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
        0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
        0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
        0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
        0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
        3.112058E-19_fp, 1.184702E-18_fp, 4.577011E-18_fp, 1.789488E-17_fp, 7.059239E-17_fp, &
        2.801093E-16_fp, 1.114424E-15_fp, 4.430982E-15_fp, 1.754743E-14_fp, 6.897637E-14_fp, &
        2.681926E-13_fp, 1.027837E-12_fp, 3.868968E-12_fp, 1.425352E-11_fp, 5.121245E-11_fp, &
        1.788308E-10_fp, 6.048330E-10_fp, 1.974708E-09_fp, 6.203527E-09_fp, 1.869357E-08_fp, &
        5.387408E-08_fp, 1.480799E-07_fp, 3.871910E-07_fp, 9.608434E-07_fp, 2.258279E-06_fp, &
        5.017946E-06_fp, 1.052599E-05_fp, 2.082121E-05_fp, 3.880948E-05_fp, 6.814300E-05_fp, &
        1.127227E-04_fp, 1.757803E-04_fp, 2.586908E-04_fp, 3.598829E-04_fp, 4.743266E-04_fp, &
        5.939634E-04_fp, 7.091114E-04_fp, 8.104756E-04_fp, 8.911259E-04_fp, 9.478373E-04_fp, &
        9.814733E-04_fp, 9.964914E-04_fp, 9.999501E-04_fp, 9.994838E-04_fp, 9.921395E-04_fp, &
        9.678320E-04_fp, 9.171414E-04_fp, 8.337592E-04_fp, 7.173667E-04_fp, 5.757384E-04_fp/)
        
      IF ( atm(2)%n_Aerosols > 1 ) THEN
        atm(2)%Aerosol(2)%Type = SEASALT_SSCM1_AEROSOL
        atm(2)%Aerosol(2)%Effective_Radius = & ! microns
        (/0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 1.200000E+00_fp, 1.200000E+00_fp, 1.200000E+00_fp, 1.200000E+00_fp, &
          1.200000E+00_fp, 1.200000E+00_fp, 1.200000E+00_fp, 1.200000E+00_fp, 1.200000E+00_fp, &
          2.035608E+00_fp, 3.433539E+00_fp, 4.500000E+00_fp, 4.500000E+00_fp, 4.500000E+00_fp, &
          4.500000E+00_fp, 4.500000E+00_fp, 4.500000E+00_fp, 4.500000E+00_fp, 4.500000E+00_fp/)
        atm(2)%Aerosol(2)%Concentration = & ! kg/m^2
        (/0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 1.718665E-20_fp, 6.364432E-18_fp, 1.294130E-15_fp, 1.453633E-13_fp, &
          9.116027E-12_fp, 3.241673E-10_fp, 6.673036E-09_fp, 8.162075E-08_fp, 6.123529E-07_fp, &
          2.926244E-06_fp, 9.306878E-06_fp, 2.071874E-05_fp, 3.418072E-05_fp, 4.455191E-05_fp, &
          4.926597E-05_fp, 5.000000E-05_fp, 4.924296E-05_fp, 4.412128E-05_fp, 3.247284E-05_fp/)
      END IF
      
      IF ( atm(2)%n_Aerosols > 2 ) THEN
        atm(2)%Aerosol(3)%Type = SEASALT_SSCM2_AEROSOL
        atm(2)%Aerosol(3)%Effective_Radius = & ! microns
        (/0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, &
          3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, &
          3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, &
          3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, &
          3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, &
          3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, &
          3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, &
          3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, &
          3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, &
          3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, &
          3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, &
          3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, &
          3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp, 3.500000E+00_fp/)
        atm(2)%Aerosol(3)%Concentration = & ! kg/m^2
        (/0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, 0.000000E+00_fp, &
          0.000000E+00_fp, 0.000000E+00_fp, 7.258759E-21_fp, 1.408580E-19_fp, 2.671985E-18_fp, &
          4.861044E-17_fp, 8.316902E-16_fp, 1.311926E-14_fp, 1.870485E-13_fp, 2.363806E-12_fp, &
          2.598250E-11_fp, 2.440107E-10_fp, 1.926085E-09_fp, 1.259490E-08_fp, 6.741174E-08_fp, &
          2.926595E-07_fp, 1.024936E-06_fp, 2.891988E-06_fp, 6.598725E-06_fp, 1.228990E-05_fp, &
          1.898153E-05_fp, 2.488012E-05_fp, 2.855754E-05_fp, 2.988952E-05_fp, 2.999200E-05_fp, &
          2.927621E-05_fp, 2.600524E-05_fp, 1.925823E-05_fp, 1.073490E-05_fp, 4.002469E-06_fp, &
          8.719108E-07_fp, 9.516156E-08_fp, 4.374152E-09_fp, 6.968124E-11_fp, 3.094494E-13_fp, &
          3.007755E-16_fp, 1.306643E-19_fp, 8.973748E-18_fp, 6.907477E-16_fp, 3.699227E-14_fp, &
          1.371784E-12_fp, 3.515726E-11_fp, 6.234566E-10_fp, 7.684359E-09_fp, 6.636126E-08_fp, &
          4.063274E-07_fp, 1.792269E-06_fp, 5.811355E-06_fp, 1.419909E-05_fp, 2.692800E-05_fp, &
          4.103532E-05_fp, 5.229739E-05_fp, 5.833714E-05_fp, 5.995863E-05_fp, 5.986513E-05_fp, &
          5.739352E-05_fp, 4.931915E-05_fp, 3.517150E-05_fp, 1.913860E-05_fp, 7.253731E-06_fp, &
          1.735030E-06_fp, 2.355013E-07_fp, 1.618327E-08_fp, 4.982977E-10_fp, 6.034809E-12_fp/)
      END IF
    END IF Load_Aerosol_Data_2

  END SUBROUTINE Load_Atm_Data
  
  
  !
  ! Internal subprogam to load some test surface data
  !
  SUBROUTINE Load_Sfc_Data()
    
    
    ! 4a.0 Surface type definitions for default SfcOptics definitions
    !      For IR and VIS, this is the NPOESS reflectivities.
    ! ---------------------------------------------------------------
    INTEGER, PARAMETER :: TUNDRA_SURFACE_TYPE         = 10  ! NPOESS Land surface type for IR/VIS Land SfcOptics
    INTEGER, PARAMETER :: SCRUB_SURFACE_TYPE          =  7  ! NPOESS Land surface type for IR/VIS Land SfcOptics
    INTEGER, PARAMETER :: COARSE_SOIL_TYPE            =  1  ! Soil type                for MW land SfcOptics
    INTEGER, PARAMETER :: GROUNDCOVER_VEGETATION_TYPE =  7  ! Vegetation type          for MW Land SfcOptics
    INTEGER, PARAMETER :: BARE_SOIL_VEGETATION_TYPE   = 11  ! Vegetation type          for MW Land SfcOptics
    INTEGER, PARAMETER :: SEA_WATER_TYPE              =  1  ! Water type               for all SfcOptics
    INTEGER, PARAMETER :: FRESH_SNOW_TYPE             =  2  ! NPOESS Snow type         for IR/VIS SfcOptics
    INTEGER, PARAMETER :: FRESH_ICE_TYPE              =  1  ! NPOESS Ice type          for IR/VIS SfcOptics



    ! 4a.1 Profile #1
    ! ---------------
    ! ...Land surface characteristics
    sfc(1)%Land_Coverage     = 0.1_fp
    sfc(1)%Land_Type         = TUNDRA_SURFACE_TYPE
    sfc(1)%Land_Temperature  = 272.0_fp
    sfc(1)%Lai               = 0.17_fp
    sfc(1)%Soil_Type         = COARSE_SOIL_TYPE
    sfc(1)%Vegetation_Type   = GROUNDCOVER_VEGETATION_TYPE
    ! ...Water surface characteristics
    sfc(1)%Water_Coverage    = 0.5_fp
    sfc(1)%Water_Type        = SEA_WATER_TYPE
    sfc(1)%Water_Temperature = 275.0_fp
    ! ...Snow coverage characteristics
    sfc(1)%Snow_Coverage    = 0.25_fp
    sfc(1)%Snow_Type        = FRESH_SNOW_TYPE
    sfc(1)%Snow_Temperature = 265.0_fp
    ! ...Ice surface characteristics
    sfc(1)%Ice_Coverage    = 0.15_fp
    sfc(1)%Ice_Type        = FRESH_ICE_TYPE
    sfc(1)%Ice_Temperature = 269.0_fp



    ! 4a.2 Profile #2
    ! ---------------
    ! Surface data
    sfc(2)%Land_Coverage    = 1.0_fp
    sfc(2)%Land_Type        = SCRUB_SURFACE_TYPE
    sfc(2)%Land_Temperature = 318.0_fp
    sfc(2)%Lai              = 0.65_fp
    sfc(2)%Soil_Type        = COARSE_SOIL_TYPE
    sfc(2)%Vegetation_Type  = BARE_SOIL_VEGETATION_TYPE

  END SUBROUTINE Load_Sfc_Data
  
  
  !
  ! Internal subprogam to create a signal file
  !
  SUBROUTINE SignalFile_Create()
    CHARACTER(256) :: Filename
    INTEGER :: fid
    Filename = '.signal'
    fid = Get_Lun()
    OPEN( fid, FILE = Filename )
    WRITE( fid,* ) TRIM(Filename)
    CLOSE( fid )
  END SUBROUTINE SignalFile_Create

END PROGRAM check_crtm
