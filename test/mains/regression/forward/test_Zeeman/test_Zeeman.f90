!
! test_Zeeman
!
! Test program for the CRTM Forward function for SSMIS with
! the inclusion of the Zeeman and Doppler effects.
!
! *** IMPORTANT NOTES ***
!
!    (1) SSMIS channels 19-22 are affected by the Zeeman-splitting effect
!        and channels 19-21 are affected by the Doppler shift due to the
!        Earth rotation.
!
!    (2) To run CRTM with the inclusion of Zeeman and Doppler effects, the
!        ancillary coefficient file
!          zssmis_fxx.TauCoeff.bin
!        must be present, used together with the coefficient file
!          ssmis_fxx.TauCoeff.bin
!        where xx is 16, 17, 18, 19 or 20.  If zssmis_fxx.TauCoeff.bin is
!        not present, the Forward calculations will use the coefficients
!        in the file ssmis_fxx.TauCoeff.bin for all channels. In this case,
!        the Zeeman and Doppler effects will not be taken into account.
!
!    (3) The Zeeman model for channels 19-22 has a top level at 0.000071 mb.
!        If the user profiles reach a height over 0.005mb, the ODAS (OPTRAN)
!        coefficient files should not be used. Instead, the ODPS coeffcient
!        files should be used with the ancillary coefficient file. (There are
!        two sets of ssmis_fxx.TauCoeff.bin files, one for the ODAS algorithm
!        end the other for the ODPS algorithm.)
!
!

PROGRAM test_Zeeman

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
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'test_Zeeman'
  CHARACTER(*), PARAMETER :: COEFFICIENTS_PATH = './testinput/'
  CHARACTER(*), PARAMETER :: RESULTS_PATH = './results/forward/'


  ! ============================================================================
  ! 0. **** SOME SET UP PARAMETERS FOR THIS TEST ****
  !
  ! This example processes a single profile of 155 layers and
  !                                            2 absorbers
  !                                            clear sky
  INTEGER, PARAMETER :: N_PROFILES  = 1
  INTEGER, PARAMETER :: N_LAYERS    = 155
  INTEGER, PARAMETER :: N_ABSORBERS = 2
  INTEGER, PARAMETER :: N_CLOUDS    = 0
  INTEGER, PARAMETER :: N_AEROSOLS  = 0
  ! ...but only ONE Sensor at a time
  INTEGER, PARAMETER :: N_SENSORS = 1

  ! Surface zenith angle (degree), computed with a scan angle 45 degree, assuming a
  ! satellite heith 850 km and an Earth radius 6370.0 km.
  REAL(fp), PARAMETER :: ZENITH_ANGLE = 53.27_fp
  ! ============================================================================


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: Version
  CHARACTER(256) :: Sensor_Id
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: n_Channels
  INTEGER :: l, m
  ! Declarations for RTSolution comparison
  INTEGER :: n_l, n_m
  CHARACTER(256) :: rts_File
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: rts(:,:)


  ! ============================================================================
  ! 1. **** DEFINE THE CRTM INTERFACE STRUCTURES ****
  !
  TYPE(CRTM_ChannelInfo_type)             :: ChannelInfo(N_SENSORS)
  TYPE(CRTM_Geometry_type)                :: Geometry(N_PROFILES)
  TYPE(CRTM_Atmosphere_type)              :: Atm(N_PROFILES)
  TYPE(CRTM_Surface_type)                 :: Sfc(N_PROFILES)
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: RTSolution(:,:)
  TYPE(CRTM_Options_type)                 :: Options(N_PROFILES)
  ! ============================================================================

  
  !First, make sure the right number of inputs have been provided
  IF(COMMAND_ARGUMENT_COUNT().NE.1)THEN
     WRITE(*,*) TRIM(PROGRAM_NAME)//': ERROR, ONLY one command-line argument required, returning'
     STOP 1
  ENDIF
  CALL GET_COMMAND_ARGUMENT(1,Sensor_Id)   !read in the value

  
  ! Program header
  ! --------------
  CALL CRTM_Version( Version )
  CALL Program_Message( PROGRAM_NAME, &
    'Test program for the CRTM Forward function for SSMIS with '//&
    'the inclusion of the Zeeman and Doppler effects.', &
    'CRTM Version: '//TRIM(Version) )


  ! Get sensor id from user
  ! -----------------------
  Sensor_Id = ADJUSTL(Sensor_Id)
  WRITE( *,'(//5x,"Running CRTM for ",a," sensor...")' ) TRIM(Sensor_Id)



  ! ============================================================================
  ! 2. **** INITIALIZE THE CRTM ****
  !
  ! 2a. Initialise the requested sensor
  ! -----------------------------------
  WRITE( *,'(/5x,"Initializing the CRTM...")' )
  Error_Status = CRTM_Init( (/Sensor_Id/), &
                            ChannelInfo, &
                            File_Path=COEFFICIENTS_PATH)
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error initializing CRTM'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF

  ! 2b. Determine the total number of channels
  !     for which the CRTM was initialized
  ! ------------------------------------------
  n_Channels = SUM(CRTM_ChannelInfo_n_Channels(ChannelInfo))
  ! ============================================================================




  ! ============================================================================
  ! 3. **** ALLOCATE STRUCTURE ARRAYS ****
  !
  ! 3a. Allocate the ARRAYS
  ! -----------------------
  ALLOCATE( RTSolution( n_Channels, N_PROFILES ), STAT=Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    Message = 'Error allocating structure arrays'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF

  ! 3b. Allocate the STRUCTURES
  ! ---------------------------
  CALL CRTM_Atmosphere_Create( Atm, N_LAYERS, N_ABSORBERS, N_CLOUDS, N_AEROSOLS )
  IF ( ANY(.NOT. CRTM_Atmosphere_Associated(Atm)) ) THEN
    Message = 'Error allocating CRTM Atmosphere structures'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF
  ! ============================================================================




  ! ============================================================================
  ! 4. **** ASSIGN INPUT DATA ****
  !
  ! 4a. Atmosphere and Surface input
  ! --------------------------------
  CALL Load_AtmSfc_Data()


  ! 4b. GeometryInfo input
  ! ----------------------
  ! All profiles are given the same value (Zenith angle at the surface)
  CALL CRTM_Geometry_SetValue( Geometry, &
                               Sensor_Zenith_Angle = ZENITH_ANGLE)

  ! 4c. Option input
  ! ----------------
  ! Ancillary Input: set parameters for SSMIS Zeeman channels (ch19 - 22)
  !   Field_Strength - Earth magnetic field, in Gauss unit ( 0.2 - 0.7)
  !   COS_ThetaB     - Consine of the angle between the magnetic field and
  !                    propagation direction of the electromagnetic wave ( -1 - 1)
  !   Doppler_Shift  - frequent shift in KHz (-80 - 80)
  DO m = 1, N_PROFILES
    CALL Zeeman_Input_SetValue( Options(m)%Zeeman, &
                                Field_Strength=0.35_fp, &
                                COS_ThetaB    =0.5_fp, &
                                Doppler_Shift =60.0_fp )
  END DO
  ! ============================================================================




  ! ============================================================================
  ! 5. **** CALL THE CRTM FORWARD MODEL ****
  !
  Error_Status = CRTM_Forward( Atm        , &
                               Sfc        , &
                               Geometry   , &
                               ChannelInfo, &
                               RTSolution , &
                               Options = Options )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error in CRTM Forward Model'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF
  ! ============================================================================




  ! ============================================================================
  ! 6. **** OUTPUT THE RESULTS TO SCREEN ****
  !
  DO m = 1, N_PROFILES
    WRITE( *,'(//7x,"Profile ",i0," output for ",a )') m, TRIM(Sensor_Id)
    DO l = 1, n_Channels
      WRITE( *, '(/5x,"Channel ",i0," results")') RTSolution(l,m)%Sensor_Channel
      CALL CRTM_RTSolution_Inspect(RTSolution(l,m))
    END DO
  END DO
  ! ============================================================================




  ! ============================================================================
  ! 7. **** DESTROY THE CRTM ****
  !
  WRITE( *, '( /5x, "Destroying the CRTM..." )' )
  Error_Status = CRTM_Destroy( ChannelInfo )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error destroying CRTM'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF
  ! ============================================================================




  ! ============================================================================
  ! 8. **** COMPARE RTSolution RESULTS TO SAVED VALUES ****
  !
  WRITE( *, '( /5x, "Comparing calculated results with saved ones..." )' )

  ! 8a. Create the output file if it does not exist
  ! -----------------------------------------------
  rts_File = RESULTS_PATH//TRIM(PROGRAM_NAME)//'_'//TRIM(Sensor_Id)//'.RTSolution.bin'
  IF ( .NOT. File_Exists(rts_File) ) THEN
    Message = 'RTSolution save file does not exist. Creating...'
    CALL Display_Message( PROGRAM_NAME, Message, INFORMATION )
    Error_Status = CRTM_RTSolution_WriteFile( rts_File, RTSolution, Quiet=.TRUE. )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating RTSolution save file'
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
      STOP 1
    END IF
  END IF

  ! 8b. Inquire the saved file
  ! --------------------------
  Error_Status = CRTM_RTSolution_InquireFile( rts_File, &
                                              n_Channels = n_l, &
                                              n_Profiles = n_m )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error inquiring RTSolution save file'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF

  ! 8c. Compare the dimensions
  ! --------------------------
  IF ( n_l /= n_Channels .OR. n_m /= N_PROFILES ) THEN
    Message = 'Dimensions of saved data different from that calculated!'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF

  ! 8d. Allocate the structure to read in saved data
  ! ------------------------------------------------
  ALLOCATE( rts( n_l, n_m ), STAT=Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    Message = 'Error allocating RTSolution saved data array'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF

  ! 8e. Read the saved data
  ! -----------------------
  Error_Status = CRTM_RTSolution_ReadFile( rts_File, rts, Quiet=.TRUE. )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error reading RTSolution save file'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF

  ! 8f. Compare the structures
  ! --------------------------
  IF ( ALL(CRTM_RTSolution_Compare(RTSolution, rts)) ) THEN
    Message = 'RTSolution results are the same!'
    CALL Display_Message( PROGRAM_NAME, Message, INFORMATION )
  ELSE
    Message = 'RTSolution results are different!'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    ! Write the current RTSolution results to file
    rts_File = TRIM(PROGRAM_NAME)//'_'//TRIM(Sensor_Id)//'.RTSolution.bin'
    Error_Status = CRTM_RTSolution_WriteFile( rts_File, RTSolution, Quiet=.TRUE. )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating temporary RTSolution save file for failed comparison'
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    END IF
    STOP 1
  END IF
  ! ============================================================================




  ! ============================================================================
  ! 9. **** CLEAN UP ****
  !
  ! 9a. Deallocate the structures
  ! -----------------------------
  CALL CRTM_Atmosphere_Destroy(Atm)

  ! 9b. Deallocate the arrays
  ! -------------------------
  DEALLOCATE(RTSolution, rts, STAT=Allocate_Status)
  ! ============================================================================

  ! Signal the successful completion of this test program
  ! (It is not a necessary step for running CRTM.)


CONTAINS


  ! -------------------------------------------------
  ! Internal subprogam to load some test profile data
  ! -------------------------------------------------
  SUBROUTINE Load_AtmSfc_Data()

    INTEGER, PARAMETER :: SEA_WATER_TYPE  = 1

    ! 4a.1 Profile #1
    ! ---------------
    ! Surface data
    Sfc(1)%Water_Coverage    = 1.0_fp
    Sfc(1)%Water_Type        = SEA_WATER_TYPE
    Sfc(1)%Water_Temperature = 287.0_fp
    Sfc(1)%Wind_Speed        = 10.0_fp  ! (m/s)

    ! Atmospheric profile data
    Atm(1)%Absorber_Id    = (/ H2O_ID                 , O3_ID /)
    Atm(1)%Absorber_Units = (/ MASS_MIXING_RATIO_UNITS, VOLUME_MIXING_RATIO_UNITS /)

    ! Level pressure (mb)
    Atm(1)%Level_Pressure = (/&
        7.1000e-05_fp,  8.2000e-05_fp,  9.4000e-05_fp,  1.0900e-04_fp,  1.2600e-04_fp,  1.4500e-04_fp, &
        1.7000e-04_fp,  1.9900e-04_fp,  2.3300e-04_fp,  2.7300e-04_fp,  3.2000e-04_fp,  3.8000e-04_fp, &
        4.5200e-04_fp,  5.3800e-04_fp,  6.3900e-04_fp,  7.6000e-04_fp,  9.0700e-04_fp,  1.0820e-03_fp, &
        1.2920e-03_fp,  1.5420e-03_fp,  1.8400e-03_fp,  2.1960e-03_fp,  2.6220e-03_fp,  3.1300e-03_fp, &
        3.7360e-03_fp,  4.4600e-03_fp,  5.2930e-03_fp,  6.2820e-03_fp,  7.4550e-03_fp,  8.8470e-03_fp, &
        1.0500e-02_fp,  1.2388e-02_fp,  1.4615e-02_fp,  1.7243e-02_fp,  2.0343e-02_fp,  2.4000e-02_fp, &
        2.8035e-02_fp,  3.2749e-02_fp,  3.8255e-02_fp,  4.4687e-02_fp,  5.2200e-02_fp,  6.0481e-02_fp, &
        7.0077e-02_fp,  8.1194e-02_fp,  9.4075e-02_fp,  1.0900e-01_fp,  1.2532e-01_fp,  1.4409e-01_fp, &
        1.6567e-01_fp,  1.9048e-01_fp,  2.1900e-01_fp,  2.5005e-01_fp,  2.8551e-01_fp,  3.2599e-01_fp, &
        3.7222e-01_fp,  4.2500e-01_fp,  4.8205e-01_fp,  5.4675e-01_fp,  6.2014e-01_fp,  7.0339e-01_fp, &
        7.9780e-01_fp,  9.0387e-01_fp,  1.0240e+00_fp,  1.1605e+00_fp,  1.3154e+00_fp,  1.4910e+00_fp, &
        1.6968e+00_fp,  1.9310e+00_fp,  2.2014e+00_fp,  2.5140e+00_fp,  2.8710e+00_fp,  3.3269e+00_fp, &
        3.8552e+00_fp,  4.4291e+00_fp,  5.0447e+00_fp,  5.7460e+00_fp,  6.5625e+00_fp,  7.4951e+00_fp, &
        8.6801e+00_fp,  1.0193e+01_fp,  1.1970e+01_fp,  1.3912e+01_fp,  1.6168e+01_fp,  1.8807e+01_fp, &
        2.0922e+01_fp,  2.3453e+01_fp,  2.6183e+01_fp,  2.9121e+01_fp,  3.2274e+01_fp,  3.5651e+01_fp, &
        3.9257e+01_fp,  4.3100e+01_fp,  4.7188e+01_fp,  5.1528e+01_fp,  5.6126e+01_fp,  6.0989e+01_fp, &
        6.6125e+01_fp,  7.1540e+01_fp,  7.7240e+01_fp,  8.3231e+01_fp,  8.9520e+01_fp,  9.6114e+01_fp, &
        1.0302e+02_fp,  1.1024e+02_fp,  1.1778e+02_fp,  1.2565e+02_fp,  1.3385e+02_fp,  1.4238e+02_fp, &
        1.5127e+02_fp,  1.6050e+02_fp,  1.7008e+02_fp,  1.8002e+02_fp,  1.9032e+02_fp,  2.0099e+02_fp, &
        2.1203e+02_fp,  2.2344e+02_fp,  2.3523e+02_fp,  2.4741e+02_fp,  2.5997e+02_fp,  2.7292e+02_fp, &
        2.8626e+02_fp,  3.0000e+02_fp,  3.1414e+02_fp,  3.2868e+02_fp,  3.4362e+02_fp,  3.5897e+02_fp, &
        3.7472e+02_fp,  3.9089e+02_fp,  4.0747e+02_fp,  4.2447e+02_fp,  4.4188e+02_fp,  4.5971e+02_fp, &
        4.7796e+02_fp,  4.9663e+02_fp,  5.1572e+02_fp,  5.3523e+02_fp,  5.5517e+02_fp,  5.7552e+02_fp, &
        5.9631e+02_fp,  6.1751e+02_fp,  6.3914e+02_fp,  6.6119e+02_fp,  6.8367e+02_fp,  7.0657e+02_fp, &
        7.2989e+02_fp,  7.5363e+02_fp,  7.7779e+02_fp,  8.0237e+02_fp,  8.2737e+02_fp,  8.5279e+02_fp, &
        8.7862e+02_fp,  9.0487e+02_fp,  9.3152e+02_fp,  9.5859e+02_fp,  9.8607e+02_fp,  1.0139e+03_fp /)

    ! Layer pressure (mb)
    Atm(1)%Pressure = (/&
        7.6368e-05_fp,  8.7863e-05_fp,  1.0132e-04_fp,  1.1729e-04_fp,  1.3528e-04_fp,  1.5717e-04_fp, &
        1.8412e-04_fp,  2.1555e-04_fp,  2.5247e-04_fp,  2.9588e-04_fp,  3.4914e-04_fp,  4.1496e-04_fp, &
        4.9375e-04_fp,  5.8705e-04_fp,  6.9775e-04_fp,  8.3134e-04_fp,  9.9193e-04_fp,  1.1839e-03_fp, &
        1.4133e-03_fp,  1.6866e-03_fp,  2.0128e-03_fp,  2.4027e-03_fp,  2.8685e-03_fp,  3.4241e-03_fp, &
        4.0873e-03_fp,  4.8646e-03_fp,  5.7734e-03_fp,  6.8518e-03_fp,  8.1312e-03_fp,  9.6499e-03_fp, &
        1.1418e-02_fp,  1.3471e-02_fp,  1.5893e-02_fp,  1.8750e-02_fp,  2.2121e-02_fp,  2.5965e-02_fp, &
        3.0331e-02_fp,  3.5431e-02_fp,  4.1388e-02_fp,  4.8346e-02_fp,  5.6239e-02_fp,  6.5161e-02_fp, &
        7.5499e-02_fp,  8.7476e-02_fp,  1.0135e-01_fp,  1.1697e-01_fp,  1.3449e-01_fp,  1.5463e-01_fp, &
        1.7779e-01_fp,  2.0441e-01_fp,  2.3418e-01_fp,  2.6739e-01_fp,  3.0530e-01_fp,  3.4859e-01_fp, &
        3.9803e-01_fp,  4.5293e-01_fp,  5.1372e-01_fp,  5.8267e-01_fp,  6.6089e-01_fp,  7.4960e-01_fp, &
        8.4973e-01_fp,  9.6269e-01_fp,  1.0908e+00_fp,  1.2363e+00_fp,  1.4014e+00_fp,  1.5917e+00_fp, &
        1.8114e+00_fp,  2.0632e+00_fp,  2.3542e+00_fp,  2.6886e+00_fp,  3.0934e+00_fp,  3.5846e+00_fp, &
        4.1355e+00_fp,  4.7302e+00_fp,  5.3877e+00_fp,  6.1452e+00_fp,  7.0185e+00_fp,  8.0731e+00_fp, &
        9.4163e+00_fp,  1.1058e+01_fp,  1.2917e+01_fp,  1.5012e+01_fp,  1.7454e+01_fp,  1.9846e+01_fp, &
        2.2163e+01_fp,  2.4793e+01_fp,  2.7626e+01_fp,  3.0670e+01_fp,  3.3934e+01_fp,  3.7425e+01_fp, &
        4.1149e+01_fp,  4.5113e+01_fp,  4.9326e+01_fp,  5.3794e+01_fp,  5.8524e+01_fp,  6.3522e+01_fp, &
        6.8797e+01_fp,  7.4354e+01_fp,  8.0198e+01_fp,  8.6337e+01_fp,  9.2778e+01_fp,  9.9527e+01_fp, &
        1.0659e+02_fp,  1.1397e+02_fp,  1.2167e+02_fp,  1.2971e+02_fp,  1.3807e+02_fp,  1.4678e+02_fp, &
        1.5584e+02_fp,  1.6524e+02_fp,  1.7500e+02_fp,  1.8512e+02_fp,  1.9561e+02_fp,  2.0646e+02_fp, &
        2.1769e+02_fp,  2.2928e+02_fp,  2.4127e+02_fp,  2.5364e+02_fp,  2.6639e+02_fp,  2.7954e+02_fp, &
        2.9308e+02_fp,  3.0702e+02_fp,  3.2136e+02_fp,  3.3609e+02_fp,  3.5124e+02_fp,  3.6679e+02_fp, &
        3.8275e+02_fp,  3.9912e+02_fp,  4.1591e+02_fp,  4.3312e+02_fp,  4.5074e+02_fp,  4.6878e+02_fp, &
        4.8724e+02_fp,  5.0611e+02_fp,  5.2541e+02_fp,  5.4514e+02_fp,  5.6528e+02_fp,  5.8585e+02_fp, &
        6.0685e+02_fp,  6.2826e+02_fp,  6.5010e+02_fp,  6.7237e+02_fp,  6.9506e+02_fp,  7.1817e+02_fp, &
        7.4170e+02_fp,  7.6565e+02_fp,  7.9002e+02_fp,  8.1481e+02_fp,  8.4002e+02_fp,  8.6564e+02_fp, &
        8.9168e+02_fp,  9.1813e+02_fp,  9.4499e+02_fp,  9.7226e+02_fp,  9.9992e+02_fp /)

    ! Layer temperature (K)
    Atm(1)%Temperature = (/&
        236.855_fp,  230.725_fp,  224.505_fp,  218.105_fp,  211.870_fp,  207.425_fp,  204.685_fp,  201.955_fp, &
        199.220_fp,  196.475_fp,  194.435_fp,  193.095_fp,  191.750_fp,  190.410_fp,  189.070_fp,  188.250_fp, &
        187.950_fp,  187.650_fp,  187.350_fp,  187.050_fp,  187.100_fp,  187.500_fp,  187.900_fp,  188.300_fp, &
        188.700_fp,  189.870_fp,  191.810_fp,  193.750_fp,  195.690_fp,  197.630_fp,  199.580_fp,  201.540_fp, &
        203.500_fp,  205.460_fp,  207.420_fp,  209.520_fp,  211.760_fp,  214.000_fp,  216.240_fp,  218.480_fp, &
        220.970_fp,  223.710_fp,  226.450_fp,  229.190_fp,  231.930_fp,  234.670_fp,  237.410_fp,  240.150_fp, &
        242.890_fp,  245.630_fp,  248.380_fp,  251.140_fp,  253.900_fp,  256.660_fp,  259.420_fp,  261.790_fp, &
        263.770_fp,  265.750_fp,  267.730_fp,  269.710_fp,  270.680_fp,  270.640_fp,  269.970_fp,  268.040_fp, &
        265.480_fp,  262.820_fp,  260.060_fp,  257.300_fp,  254.540_fp,  251.780_fp,  248.900_fp,  245.900_fp, &
        243.010_fp,  240.340_fp,  237.780_fp,  235.200_fp,  232.600_fp,  230.300_fp,  228.600_fp,  227.200_fp, &
        226.000_fp,  225.000_fp,  224.010_fp,  223.185_fp,  222.490_fp,  221.780_fp,  221.080_fp,  220.395_fp, &
        219.740_fp,  219.110_fp,  218.500_fp,  217.905_fp,  217.360_fp,  216.905_fp,  216.700_fp,  216.700_fp, &
        216.700_fp,  216.700_fp,  216.700_fp,  216.700_fp,  216.700_fp,  216.700_fp,  216.700_fp,  216.700_fp, &
        216.700_fp,  216.700_fp,  216.700_fp,  216.700_fp,  216.700_fp,  216.700_fp,  216.700_fp,  216.700_fp, &
        216.710_fp,  216.740_fp,  216.775_fp,  217.545_fp,  219.360_fp,  221.460_fp,  223.525_fp,  225.565_fp, &
        227.580_fp,  229.580_fp,  231.585_fp,  233.575_fp,  235.535_fp,  237.490_fp,  239.435_fp,  241.350_fp, &
        243.250_fp,  245.145_fp,  247.015_fp,  248.860_fp,  250.705_fp,  252.535_fp,  254.335_fp,  256.125_fp, &
        257.910_fp,  259.675_fp,  261.410_fp,  263.145_fp,  264.875_fp,  266.575_fp,  268.255_fp,  269.935_fp, &
        271.605_fp,  273.250_fp,  274.880_fp,  276.500_fp,  278.115_fp,  279.710_fp,  281.285_fp,  282.855_fp, &
        284.420_fp,  285.970_fp,  287.495_fp /)

    ! Layer H2O profile (mixing ratio_fp,  g/kg)
    Atm(1)%Absorber(:, 1) = (/&
     1.780e-04_fp, 1.852e-04_fp, 1.927e-04_fp, 2.003e-04_fp, 2.078e-04_fp, 2.152e-04_fp, 2.227e-04_fp, 2.301e-04_fp, &
     2.376e-04_fp, 2.451e-04_fp, 2.574e-04_fp, 2.748e-04_fp, 2.924e-04_fp, 3.098e-04_fp, 3.271e-04_fp, 3.551e-04_fp, &
     3.936e-04_fp, 4.323e-04_fp, 4.709e-04_fp, 5.094e-04_fp, 5.585e-04_fp, 6.182e-04_fp, 6.779e-04_fp, 7.376e-04_fp, &
     7.974e-04_fp, 8.721e-04_fp, 9.614e-04_fp, 1.051e-03_fp, 1.141e-03_fp, 1.230e-03_fp, 1.323e-03_fp, 1.420e-03_fp, &
     1.516e-03_fp, 1.613e-03_fp, 1.709e-03_fp, 1.799e-03_fp, 1.883e-03_fp, 1.967e-03_fp, 2.051e-03_fp, 2.135e-03_fp, &
     2.221e-03_fp, 2.307e-03_fp, 2.394e-03_fp, 2.481e-03_fp, 2.569e-03_fp, 2.647e-03_fp, 2.715e-03_fp, 2.784e-03_fp, &
     2.852e-03_fp, 2.921e-03_fp, 2.976e-03_fp, 3.020e-03_fp, 3.063e-03_fp, 3.107e-03_fp, 3.150e-03_fp, 3.180e-03_fp, &
     3.196e-03_fp, 3.211e-03_fp, 3.227e-03_fp, 3.242e-03_fp, 3.253e-03_fp, 3.259e-03_fp, 3.262e-03_fp, 3.259e-03_fp, &
     3.253e-03_fp, 3.241e-03_fp, 3.222e-03_fp, 3.201e-03_fp, 3.173e-03_fp, 3.142e-03_fp, 3.117e-03_fp, 3.097e-03_fp, &
     3.081e-03_fp, 3.067e-03_fp, 3.054e-03_fp, 3.038e-03_fp, 3.020e-03_fp, 2.999e-03_fp, 2.976e-03_fp, 2.952e-03_fp, &
     2.921e-03_fp, 2.883e-03_fp, 2.846e-03_fp, 2.814e-03_fp, 2.787e-03_fp, 2.756e-03_fp, 2.712e-03_fp, 2.663e-03_fp, &
     2.619e-03_fp, 2.571e-03_fp, 2.526e-03_fp, 2.489e-03_fp, 2.460e-03_fp, 2.435e-03_fp, 2.414e-03_fp, 2.399e-03_fp, &
     2.389e-03_fp, 2.383e-03_fp, 2.385e-03_fp, 2.394e-03_fp, 2.413e-03_fp, 2.441e-03_fp, 2.587e-03_fp, 2.856e-03_fp, &
     3.119e-03_fp, 3.361e-03_fp, 3.629e-03_fp, 4.370e-03_fp, 5.538e-03_fp, 6.846e-03_fp, 8.500e-03_fp, 1.033e-02_fp, &
     1.274e-02_fp, 1.605e-02_fp, 1.963e-02_fp, 2.436e-02_fp, 3.075e-02_fp, 3.755e-02_fp, 4.759e-02_fp, 6.300e-02_fp, &
     8.028e-02_fp, 1.024e-01_fp, 1.361e-01_fp, 1.758e-01_fp, 2.149e-01_fp, 2.535e-01_fp, 2.917e-01_fp, 3.292e-01_fp, &
     3.772e-01_fp, 4.384e-01_fp, 5.017e-01_fp, 5.675e-01_fp, 6.435e-01_fp, 7.261e-01_fp, 8.074e-01_fp, 9.064e-01_fp, &
     1.030e+00_fp, 1.158e+00_fp, 1.286e+00_fp, 1.435e+00_fp, 1.604e+00_fp, 1.771e+00_fp, 1.944e+00_fp, 2.150e+00_fp, &
     2.382e+00_fp, 2.610e+00_fp, 2.836e+00_fp, 3.060e+00_fp, 3.282e+00_fp, 3.501e+00_fp, 3.722e+00_fp, 3.961e+00_fp, &
     4.212e+00_fp, 4.460e+00_fp, 4.705e+00_fp /)

    ! Dummy layer O3 profile (volume mixing ratio, ppmv)
    Atm(1)%Absorber(:,2) = 0.01_fp

  END SUBROUTINE Load_AtmSfc_Data

  INCLUDE 'SignalFile_Create.inc'

END PROGRAM test_Zeeman
