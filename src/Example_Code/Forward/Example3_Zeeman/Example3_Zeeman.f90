!
! Example3_Zeeman
!
! Program to provide an example of CRTM Forward function usage for SSMIS with the inclusion of
! the Zeeman and Doppler effects.
!  *** Important notes ***
!    (1) SSMIS channels 19-22 are affected by the Zeeman-splitting effect and channels 19-21 are
!        affected by the Doppler shift due to the Earth rotation.
!    (2) To run CRTM with the inclusion of Zeeman and Doppler effects, the ancillary coefficient 
!       file zssmis_fxx.TauCoeff.bin must be present, used together with the coefficient file
!      ssmis_fxx.TauCoeff.bin, where xx is 16, 17, 18, 19 or 20.  If zssmis_fxx.TauCoeff.bin is 
!      not present, the Forward calculations will use the coefficients in the file
!      ssmis_fxx.TauCoeff.bin for all channels. In this case, the Zeeman and Doppler effects
!      will not be taken into account. 
!    (3) The Zeeman model for channels 19-22 has a top level at 0.000071 mb. If the user
!        profiles reach a height over 0.005mb, the ODAS (OPTRAN) coefficient files should not
!        be used. Instead, the ODPS coeffcient files should be used with the ancillary coefficient 
!        file. (There are two sets of ssmis_fxx.TauCoeff.bin files, one for the ODAS algorithm
!        end the other for the ODPS algorithm.)  
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 08-Jan-2009
!                       paul.vandelst@noaa.gov
!
!       Adapted for the Zeeman model by Yong Han, Jan 20, 2010.
!
PROGRAM Example3_Zeeman

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
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'Example3_Zeeman'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id$'
  REAL(fp), PARAMETER :: TB_THRESHOLD = 1.0e-03_fp


  ! ============================================================================
  ! 0. **** SOME SET UP PARAMETERS FOR THIS EXAMPLE ****
  !
  ! This example processes a single profile of 155 layers and
  !                                           1 absorbers
  !                                           clear sky
  INTEGER, PARAMETER :: N_PROFILES  = 1
  INTEGER, PARAMETER :: N_LAYERS    = 155
  INTEGER, PARAMETER :: N_ABSORBERS = 1
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



  ! Program header
  ! --------------
  CALL CRTM_Version( Version )
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to provide a (relatively) simple example of how '//&
                        'to call the CRTM Forward function.', &
                        'CRTM Version: '//TRIM(Version) )


  ! Get sensor id from user
  ! -----------------------
  WRITE( *,'(/5x,"Enter sensor id [ssmis_f16]: ")',ADVANCE='NO' )
  READ( *,'(a)' ) Sensor_Id
  Sensor_Id = ADJUSTL(Sensor_Id)
  WRITE( *,'(//5x,"Running CRTM for ",a," sensor...")' ) TRIM(Sensor_Id)
  
  

  ! ============================================================================
  ! 2. **** INITIALIZE THE CRTM ****
  !
  ! 2a. This initializes the CRTM for the sensors 
  !     predefined in the example SENSOR_ID parameter.
  !     NOTE: The coefficient data file path is hard-
  !           wired for this example.
  ! --------------------------------------------------
  WRITE( *,'(/5x,"Initializing the CRTM...")' )
  Error_Status = CRTM_Init( (/Sensor_Id/), &  ! Input... must be an array, hence the (/../)
                            ChannelInfo  , &  ! Output
                            EmisCoeff_File='Wu-Smith.EmisCoeff.bin', &
                            File_Path='./Coefficient_Data/')
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error initializing CRTM' 
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )  
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
  ALLOCATE( RTSolution( n_Channels, N_PROFILES ), STAT=Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    Message = 'Error allocating structure arrays' 
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )  
    STOP
  END IF

  ! 3b. Allocate the STRUCTURES
  ! ---------------------------
  ! The input FORWARD structure
  CALL CRTM_Atmosphere_Create( Atm, N_LAYERS, N_ABSORBERS, N_CLOUDS, N_AEROSOLS )
  IF ( ANY(.NOT. CRTM_Atmosphere_Associated(Atm)) ) THEN
    Message = 'Error allocating CRTM Atmosphere structures'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )  
    STOP
  END IF
  ! ============================================================================




  ! ============================================================================
  ! 4. **** ASSIGN INPUT DATA ****
  !
  ! Fill the Atm structure array. 
  ! NOTE: This is an example program for illustrative purposes only.
  !       Typically, one would not assign the data as shown below,
  !       but rather read it from file
  
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
  !   COS_ThetaB - Consine of the angle between the magnetic field and
  !                propagation direction of the electromagnetic wave ( -1 - 1)
  !   Doppler_Shift - frequent shift in KHz (-80 - 80)
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
    STOP
  END IF
  ! ============================================================================




  ! ============================================================================
  ! 6. **** OUTPUT THE RESULTS TO SCREEN ****
  !
  ! User should read the user guide or the source code of the routine
  ! CRTM_RTSolution_Inspect in the file CRTM_RTSolution_Define.f90 to
  ! select the needed variables for outputs.  These variables are contained
  ! in the structure RTSolution.  
  DO m = 1, N_PROFILES
    WRITE( *,'(//7x,"Profile ",i0," output for ",a )') m, TRIM(Sensor_Id)
    DO l = 1, n_Channels
      WRITE( *, '(/5x,"Channel ",i0," results")') ChannelInfo(1)%Sensor_Channel(l)
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
    STOP
  END IF
  ! ============================================================================




  ! ============================================================================
  ! 8. **** COMPARE RTSolution RESULTS TO SAVED VALUES ****
  !
  !   Step 8 is not part of the example to show how to use CRTM.        
  !   It is to check the user results against the results in the CRTM package. 
  !   
  WRITE( *, '( /5x, "Comparing calculated results with saved ones..." )' )

  ! 8a. Create the output file if it does not exist
  ! -----------------------------------------------
  rts_File = './Results/'//TRIM(Sensor_Id)//'.RTSolution.bin'
  IF ( .NOT. File_Exists(rts_File) ) THEN
    Message = 'RTSolution save file does not exist. Creating...'
    CALL Display_Message( PROGRAM_NAME, Message, INFORMATION )  
    Error_Status = CRTM_RTSolution_WriteFile( rts_File, RTSolution, Quiet=.TRUE. )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating RTSolution save file'
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE )  
      STOP
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
    STOP
  END IF

  ! 8c. Compare the dimensions
  ! --------------------------
  IF ( n_l /= n_Channels .OR. n_m /= N_PROFILES ) THEN
    Message = 'Dimensions of saved data different from that calculated!'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )  
    STOP
  END IF

  ! 8d. Allocate the structure to read in saved data
  ! ------------------------------------------------
  ALLOCATE( rts( n_l, n_m ), STAT=Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    Message = 'Error allocating RTSolution saved data array' 
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )  
    STOP
  END IF

  ! 8e. Read the saved data
  ! -----------------------
  Error_Status = CRTM_RTSolution_ReadFile( rts_File, rts, Quiet=.TRUE. )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error reading RTSolution save file'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )  
    STOP
  END IF
  
  ! 8f. Compare the structures
  ! --------------------------
  IF ( ALL(CRTM_RTSolution_Compare(RTSolution, rts)) ) THEN
    Message = 'RTSolution results are the same!'
    CALL Display_Message( PROGRAM_NAME, Message, INFORMATION )
  ELSE
    Message = 'RTSolution results are different!'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP
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
  CALL CRTM_Atmosphere_Destroy(Atm)

  ! 9b. Deallocate the arrays
  ! -------------------------
  DEALLOCATE(RTSolution, rts, STAT=Allocate_Status)
  ! ============================================================================

  ! Signal the completion of the program. It is not a necessary step for running CRTM.
  CALL SignalFile_Create()

CONTAINS


  ! -------------------------------------------------
  ! Internal subprogam to load some test profile data
  ! -------------------------------------------------
  SUBROUTINE Load_AtmSfc_Data()
  
    ! 4a.1 Profile #1
    ! ---------------
    ! Surface data
    Sfc(1)%Water_Coverage    = 1.0_fp
    Sfc(1)%Water_Type        = SEA_WATER
    Sfc(1)%Water_Temperature = 287.0_fp
    Sfc(1)%Wind_Speed        = 10.0_fp  ! (m/s)
  
    ! Atmospheric profile data
    Atm(1)%Absorber_Id    = (/ H2O_ID /)
    Atm(1)%Absorber_Units = (/ MASS_MIXING_RATIO_UNITS /)

    ! Level pressure (mb)
  Atm(1)%Level_Pressure = (/&
        7.1000e-05, 8.2000e-05, 9.4000e-05, 1.0900e-04, 1.2600e-04, 1.4500e-04,&
        1.7000e-04, 1.9900e-04, 2.3300e-04, 2.7300e-04, 3.2000e-04, 3.8000e-04,&
        4.5200e-04, 5.3800e-04, 6.3900e-04, 7.6000e-04, 9.0700e-04, 1.0820e-03,&
        1.2920e-03, 1.5420e-03, 1.8400e-03, 2.1960e-03, 2.6220e-03, 3.1300e-03,&
        3.7360e-03, 4.4600e-03, 5.2930e-03, 6.2820e-03, 7.4550e-03, 8.8470e-03,&
        1.0500e-02, 1.2388e-02, 1.4615e-02, 1.7243e-02, 2.0343e-02, 2.4000e-02,&
        2.8035e-02, 3.2749e-02, 3.8255e-02, 4.4687e-02, 5.2200e-02, 6.0481e-02,&
        7.0077e-02, 8.1194e-02, 9.4075e-02, 1.0900e-01, 1.2532e-01, 1.4409e-01,&
        1.6567e-01, 1.9048e-01, 2.1900e-01, 2.5005e-01, 2.8551e-01, 3.2599e-01,&
        3.7222e-01, 4.2500e-01, 4.8205e-01, 5.4675e-01, 6.2014e-01, 7.0339e-01,&
        7.9780e-01, 9.0387e-01, 1.0240e+00, 1.1605e+00, 1.3154e+00, 1.4910e+00,&
        1.6968e+00, 1.9310e+00, 2.2014e+00, 2.5140e+00, 2.8710e+00, 3.3269e+00,&
        3.8552e+00, 4.4291e+00, 5.0447e+00, 5.7460e+00, 6.5625e+00, 7.4951e+00,&
        8.6801e+00, 1.0193e+01, 1.1970e+01, 1.3912e+01, 1.6168e+01, 1.8807e+01,&
        2.0922e+01, 2.3453e+01, 2.6183e+01, 2.9121e+01, 3.2274e+01, 3.5651e+01,&
        3.9257e+01, 4.3100e+01, 4.7188e+01, 5.1528e+01, 5.6126e+01, 6.0989e+01,&
        6.6125e+01, 7.1540e+01, 7.7240e+01, 8.3231e+01, 8.9520e+01, 9.6114e+01,&
        1.0302e+02, 1.1024e+02, 1.1778e+02, 1.2565e+02, 1.3385e+02, 1.4238e+02,&
        1.5127e+02, 1.6050e+02, 1.7008e+02, 1.8002e+02, 1.9032e+02, 2.0099e+02,&
        2.1203e+02, 2.2344e+02, 2.3523e+02, 2.4741e+02, 2.5997e+02, 2.7292e+02,&
        2.8626e+02, 3.0000e+02, 3.1414e+02, 3.2868e+02, 3.4362e+02, 3.5897e+02,&
        3.7472e+02, 3.9089e+02, 4.0747e+02, 4.2447e+02, 4.4188e+02, 4.5971e+02,&
        4.7796e+02, 4.9663e+02, 5.1572e+02, 5.3523e+02, 5.5517e+02, 5.7552e+02,&
        5.9631e+02, 6.1751e+02, 6.3914e+02, 6.6119e+02, 6.8367e+02, 7.0657e+02,&
        7.2989e+02, 7.5363e+02, 7.7779e+02, 8.0237e+02, 8.2737e+02, 8.5279e+02,&
        8.7862e+02, 9.0487e+02, 9.3152e+02, 9.5859e+02, 9.8607e+02, 1.0139e+03 /)

    ! Layer pressure (mb)
  Atm(1)%Pressure = (/&
        7.6368e-05, 8.7863e-05, 1.0132e-04, 1.1729e-04, 1.3528e-04, 1.5717e-04,&
        1.8412e-04, 2.1555e-04, 2.5247e-04, 2.9588e-04, 3.4914e-04, 4.1496e-04,&
        4.9375e-04, 5.8705e-04, 6.9775e-04, 8.3134e-04, 9.9193e-04, 1.1839e-03,&
        1.4133e-03, 1.6866e-03, 2.0128e-03, 2.4027e-03, 2.8685e-03, 3.4241e-03,&
        4.0873e-03, 4.8646e-03, 5.7734e-03, 6.8518e-03, 8.1312e-03, 9.6499e-03,&
        1.1418e-02, 1.3471e-02, 1.5893e-02, 1.8750e-02, 2.2121e-02, 2.5965e-02,&
        3.0331e-02, 3.5431e-02, 4.1388e-02, 4.8346e-02, 5.6239e-02, 6.5161e-02,&
        7.5499e-02, 8.7476e-02, 1.0135e-01, 1.1697e-01, 1.3449e-01, 1.5463e-01,&
        1.7779e-01, 2.0441e-01, 2.3418e-01, 2.6739e-01, 3.0530e-01, 3.4859e-01,&
        3.9803e-01, 4.5293e-01, 5.1372e-01, 5.8267e-01, 6.6089e-01, 7.4960e-01,&
        8.4973e-01, 9.6269e-01, 1.0908e+00, 1.2363e+00, 1.4014e+00, 1.5917e+00,&
        1.8114e+00, 2.0632e+00, 2.3542e+00, 2.6886e+00, 3.0934e+00, 3.5846e+00,&
        4.1355e+00, 4.7302e+00, 5.3877e+00, 6.1452e+00, 7.0185e+00, 8.0731e+00,&
        9.4163e+00, 1.1058e+01, 1.2917e+01, 1.5012e+01, 1.7454e+01, 1.9846e+01,&
        2.2163e+01, 2.4793e+01, 2.7626e+01, 3.0670e+01, 3.3934e+01, 3.7425e+01,&
        4.1149e+01, 4.5113e+01, 4.9326e+01, 5.3794e+01, 5.8524e+01, 6.3522e+01,&
        6.8797e+01, 7.4354e+01, 8.0198e+01, 8.6337e+01, 9.2778e+01, 9.9527e+01,&
        1.0659e+02, 1.1397e+02, 1.2167e+02, 1.2971e+02, 1.3807e+02, 1.4678e+02,&
        1.5584e+02, 1.6524e+02, 1.7500e+02, 1.8512e+02, 1.9561e+02, 2.0646e+02,&
        2.1769e+02, 2.2928e+02, 2.4127e+02, 2.5364e+02, 2.6639e+02, 2.7954e+02,&
        2.9308e+02, 3.0702e+02, 3.2136e+02, 3.3609e+02, 3.5124e+02, 3.6679e+02,&
        3.8275e+02, 3.9912e+02, 4.1591e+02, 4.3312e+02, 4.5074e+02, 4.6878e+02,&
        4.8724e+02, 5.0611e+02, 5.2541e+02, 5.4514e+02, 5.6528e+02, 5.8585e+02,&
        6.0685e+02, 6.2826e+02, 6.5010e+02, 6.7237e+02, 6.9506e+02, 7.1817e+02,&
        7.4170e+02, 7.6565e+02, 7.9002e+02, 8.1481e+02, 8.4002e+02, 8.6564e+02,&
        8.9168e+02, 9.1813e+02, 9.4499e+02, 9.7226e+02, 9.9992e+02 /)

      ! Layer temperature (K)
  Atm(1)%Temperature = (/&
        236.855, 230.725, 224.505, 218.105, 211.870, 207.425, 204.685, 201.955,&
        199.220, 196.475, 194.435, 193.095, 191.750, 190.410, 189.070, 188.250,&
        187.950, 187.650, 187.350, 187.050, 187.100, 187.500, 187.900, 188.300,&
        188.700, 189.870, 191.810, 193.750, 195.690, 197.630, 199.580, 201.540,&
        203.500, 205.460, 207.420, 209.520, 211.760, 214.000, 216.240, 218.480,&
        220.970, 223.710, 226.450, 229.190, 231.930, 234.670, 237.410, 240.150,&
        242.890, 245.630, 248.380, 251.140, 253.900, 256.660, 259.420, 261.790,&
        263.770, 265.750, 267.730, 269.710, 270.680, 270.640, 269.970, 268.040,&
        265.480, 262.820, 260.060, 257.300, 254.540, 251.780, 248.900, 245.900,&
        243.010, 240.340, 237.780, 235.200, 232.600, 230.300, 228.600, 227.200,&
        226.000, 225.000, 224.010, 223.185, 222.490, 221.780, 221.080, 220.395,&
        219.740, 219.110, 218.500, 217.905, 217.360, 216.905, 216.700, 216.700,&
        216.700, 216.700, 216.700, 216.700, 216.700, 216.700, 216.700, 216.700,&
        216.700, 216.700, 216.700, 216.700, 216.700, 216.700, 216.700, 216.700,&
        216.710, 216.740, 216.775, 217.545, 219.360, 221.460, 223.525, 225.565,&
        227.580, 229.580, 231.585, 233.575, 235.535, 237.490, 239.435, 241.350,&
        243.250, 245.145, 247.015, 248.860, 250.705, 252.535, 254.335, 256.125,&
        257.910, 259.675, 261.410, 263.145, 264.875, 266.575, 268.255, 269.935,&
        271.605, 273.250, 274.880, 276.500, 278.115, 279.710, 281.285, 282.855,&
        284.420, 285.970, 287.495 /)

     ! Layer H2O profile (mixing ratio, g/kg)
  Atm(1)%Absorber(:,1) = (/&
     1.780e-04,1.852e-04,1.927e-04,2.003e-04,2.078e-04,2.152e-04,2.227e-04,2.301e-04,&
     2.376e-04,2.451e-04,2.574e-04,2.748e-04,2.924e-04,3.098e-04,3.271e-04,3.551e-04,&
     3.936e-04,4.323e-04,4.709e-04,5.094e-04,5.585e-04,6.182e-04,6.779e-04,7.376e-04,&
     7.974e-04,8.721e-04,9.614e-04,1.051e-03,1.141e-03,1.230e-03,1.323e-03,1.420e-03,&
     1.516e-03,1.613e-03,1.709e-03,1.799e-03,1.883e-03,1.967e-03,2.051e-03,2.135e-03,&
     2.221e-03,2.307e-03,2.394e-03,2.481e-03,2.569e-03,2.647e-03,2.715e-03,2.784e-03,&
     2.852e-03,2.921e-03,2.976e-03,3.020e-03,3.063e-03,3.107e-03,3.150e-03,3.180e-03,&
     3.196e-03,3.211e-03,3.227e-03,3.242e-03,3.253e-03,3.259e-03,3.262e-03,3.259e-03,&
     3.253e-03,3.241e-03,3.222e-03,3.201e-03,3.173e-03,3.142e-03,3.117e-03,3.097e-03,&
     3.081e-03,3.067e-03,3.054e-03,3.038e-03,3.020e-03,2.999e-03,2.976e-03,2.952e-03,&
     2.921e-03,2.883e-03,2.846e-03,2.814e-03,2.787e-03,2.756e-03,2.712e-03,2.663e-03,&
     2.619e-03,2.571e-03,2.526e-03,2.489e-03,2.460e-03,2.435e-03,2.414e-03,2.399e-03,&
     2.389e-03,2.383e-03,2.385e-03,2.394e-03,2.413e-03,2.441e-03,2.587e-03,2.856e-03,&
     3.119e-03,3.361e-03,3.629e-03,4.370e-03,5.538e-03,6.846e-03,8.500e-03,1.033e-02,&
     1.274e-02,1.605e-02,1.963e-02,2.436e-02,3.075e-02,3.755e-02,4.759e-02,6.300e-02,&
     8.028e-02,1.024e-01,1.361e-01,1.758e-01,2.149e-01,2.535e-01,2.917e-01,3.292e-01,&
     3.772e-01,4.384e-01,5.017e-01,5.675e-01,6.435e-01,7.261e-01,8.074e-01,9.064e-01,&
     1.030e+00,1.158e+00,1.286e+00,1.435e+00,1.604e+00,1.771e+00,1.944e+00,2.150e+00,&
     2.382e+00,2.610e+00,2.836e+00,3.060e+00,3.282e+00,3.501e+00,3.722e+00,3.961e+00,&
     4.212e+00,4.460e+00,4.705e+00 /)

  END SUBROUTINE Load_AtmSfc_Data


  SUBROUTINE SignalFile_Create()
    CHARACTER(256) :: Filename
    INTEGER :: fid
    Filename = './Results/'//TRIM(Sensor_Id)//'.signal'
    fid = Get_Lun()
    OPEN( fid, FILE = Filename )
    WRITE( fid,* ) TRIM(Filename)
    CLOSE( fid )
  END SUBROUTINE SignalFile_Create
  
END PROGRAM Example3_Zeeman
