!
! test_AD
!
! Program to provide a (relatively) simple example of how
! to test the CRTM adjoint function. 
! 
! The code checks whether the Jacobian from the Tangent-Linear
! and the Adjoint are consistent.
!
! Copyright Patrick Stegmann, 2020
!
! Modified by Isaac Moradi Isaac.Moradi@NASA.GOV
!             Nov-30-2021
!             Modified to work with active sensor module
!

PROGRAM test_AD

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
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'test_AD'
  CHARACTER(*), PARAMETER :: COEFFICIENTS_PATH = './testinput/'
  CHARACTER(*), PARAMETER :: RESULTS_PATH = './results/unit/'



  ! ============================================================================
  ! 0. **** SOME SET UP PARAMETERS FOR THIS EXAMPLE ****
  !
  ! Profile dimensions...
  INTEGER, PARAMETER :: N_PROFILES  = 2
  INTEGER, PARAMETER :: N_LAYERS    = 92
  INTEGER, PARAMETER :: N_ABSORBERS = 2
  INTEGER, PARAMETER :: N_CLOUDS    = 1
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
  CHARACTER(256) :: Message
  CHARACTER(256) :: Version
  CHARACTER(256) :: Sensor_Id
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: n_Channels
  INTEGER :: l, m
  INTEGER :: test_result
  ! Declarations for Jacobian comparisons
  INTEGER :: n_la, n_ma
  INTEGER :: n_ls, n_ms
  INTEGER :: ii, jj, ilev1, ilev2, iprof, ichan
  CHARACTER(256) :: atmk_File, sfck_File
  ! Declarations for adjoint testing
  REAL(fp) :: Perturbation
  REAL(fp) :: AD(N_LAYERS), TL(N_LAYERS)
  REAL(fp), ALLOCATABLE :: TL_final(:), AD_final(:)
  REAL(fp), DIMENSION(1,1) :: LHS
  REAL(fp), DIMENSION(1,1) :: RHS
  REAL(fp), PARAMETER :: TOLERANCE = 0.1_fp
  REAL(fp), DIMENSION(2,1) :: x_test ! Temperature state vector for the adjoint ctest
  REAL(fp), DIMENSION(2,2) :: L_operator ! Linearized operator
  REAL(fp), DIMENSION(2,2) :: L_operator_T ! Linearized operator
  LOGICAL :: Attenuated_Reflectivity
  INTEGER :: i, n_Layer_Cloud, i_layers(N_LAYERS)
  INTEGER, ALLOCATABLE :: i_Layers_Cloud(:)

  ! ============================================================================
  ! 1. **** DEFINE THE CRTM INTERFACE STRUCTURES ****
  !
  TYPE(CRTM_ChannelInfo_type)             :: ChannelInfo(N_SENSORS)
  TYPE(CRTM_Geometry_type)                :: Geometry(N_PROFILES)

  ! Define the FORWARD variables
  TYPE(CRTM_Atmosphere_type)              :: Atm(N_PROFILES)
  TYPE(CRTM_Surface_type)                 :: Sfc(N_PROFILES)
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: RTSolution(:,:)

  ! Define the Tangent-Linear variables
  TYPE(CRTM_Atmosphere_type) :: Atmosphere_TL(N_PROFILES)
  TYPE(CRTM_Surface_type)    :: Surface_TL(N_PROFILES)
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: RTSolution_TL(:,:)

  ! Define the Adjoint variables
  TYPE(CRTM_Atmosphere_type) :: Atmosphere_AD(N_PROFILES)
  TYPE(CRTM_Surface_type) :: Surface_AD(N_PROFILES)
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: RTSolution_AD(:,:)
  ! ============================================================================


  ! Directory location of coefficients
! #ifdef LITTLE_ENDIAN
  CHARACTER(*), PARAMETER :: ENDIAN_TYPE='little_endian'
! #else
!  CHARACTER(*), PARAMETER :: ENDIAN_TYPE='big_endian'
!#endif
  CHARACTER(*), PARAMETER :: COEFFICIENT_PATH='coefficients/'//ENDIAN_TYPE//'/'
  CHARACTER(*), PARAMETER :: NC_COEFFICIENT_PATH='coefficients/netcdf/'

  ! Aerosol/Cloud coefficient format
  !CHARACTER(*), PARAMETER :: Coeff_Format = 'Binary'
  CHARACTER(*), PARAMETER :: Coeff_Format = 'netCDF'

  ! Aerosol/Cloud coefficient scheme
  CHARACTER(*), PARAMETER :: Aerosol_Model = 'CRTM'
  !CHARACTER(*), PARAMETER :: Aerosol_Model = 'CMAQ'
  !CHARACTER(*), PARAMETER :: Aerosol_Model = 'GOCART-GEOS5'
  !CHARACTER(*), PARAMETER :: Aerosol_Model = 'NAAPS'
  CHARACTER(*), PARAMETER :: Cloud_Model   = 'CRTM'

  CHARACTER(256) :: AerosolCoeff_File
  CHARACTER(256) :: AerosolCoeff_Format
  CHARACTER(256) :: CloudCoeff_File
  CHARACTER(256) :: CloudCoeff_Format

  ! Program header
  ! --------------
  CALL CRTM_Version( Version )
  CALL Program_Message( PROGRAM_NAME, &
    'Program to provide a basic test for the CRTM adjoint operator.', &
    'CRTM Version: '//TRIM(Version) )


  ! Get sensor id from user
  ! -----------------------
  !WRITE( *,'(/5x,"Enter sensor id [hirs4_n18, amsua_metop-a, or mhs_n18]: ")',ADVANCE='NO' )
  !READ( *,'(a)' ) Sensor_Id
  Sensor_Id = 'atms_npp'
  Sensor_Id = ADJUSTL(Sensor_Id)
  WRITE( *,'(//5x,"Running CRTM for ",a," sensor...")' ) TRIM(Sensor_Id)


  ! ============================================================================
  ! STEP 4. **** INITIALIZE THE CRTM ****
  !
  ! 4a. Initialise all the sensors at once
  ! --------------------------------------
  !.. Cloud coefficient information
  IF ( Coeff_Format == 'Binary' ) THEN
    CloudCoeff_Format   = 'Binary'
    CloudCoeff_File     = 'CloudCoeff.bin'
  ! if netCDF I/O
  ELSE IF ( Coeff_Format == 'netCDF' ) THEN
    CloudCoeff_Format   = 'netCDF'
    CloudCoeff_File     = 'CloudCoeff_DDA_ARTS.nc4'
  ELSE
    message = 'Aerosol/Cloud coefficient format is not supported'
    CALL Display_Message( PROGRAM_NAME, message, FAILURE )
    STOP
  END IF

  !.....Aerosol
  IF ( Aerosol_Model == 'CRTM' ) THEN
    IF ( Coeff_Format == 'Binary' ) THEN
      AerosolCoeff_Format = 'Binary'
      AerosolCoeff_File   = 'AerosolCoeff.bin'
    ELSE IF ( Coeff_Format == 'netCDF' ) THEN
      AerosolCoeff_Format = 'netCDF'
      AerosolCoeff_File   = 'AerosolCoeff.nc4'
    ELSE
      message = 'Aerosol coefficient format is not supported'
      CALL Display_Message( PROGRAM_NAME, message, FAILURE )
      STOP
    END IF
  ELSEIF ( Aerosol_Model == 'CMAQ' ) THEN
    IF ( Coeff_Format == 'Binary' ) THEN
      AerosolCoeff_Format = 'Binary'
      AerosolCoeff_File   = 'AerosolCoeff.CMAQ.bin'
    ELSE IF ( Coeff_Format == 'netCDF' ) THEN
      AerosolCoeff_Format = 'netCDF'
      AerosolCoeff_File   = 'AerosolCoeff.CMAQ.nc4'
    ELSE
      message = 'Aerosol coefficient format is not supported'
      CALL Display_Message( PROGRAM_NAME, message, FAILURE )
      STOP
    END IF
  ELSEIF ( Aerosol_Model == 'GOCART-GEOS5' ) THEN
    IF ( Coeff_Format == 'Binary' ) THEN
      AerosolCoeff_Format = 'Binary'
      AerosolCoeff_File   = 'AerosolCoeff.GOCART-GEOS5.bin'
    ELSE IF ( Coeff_Format == 'netCDF' ) THEN
      AerosolCoeff_Format = 'netCDF'
      AerosolCoeff_File   = 'AerosolCoeff.GOCART-GEOS5.nc4'
    ELSE
      message = 'Aerosol coefficient format is not supported'
      CALL Display_Message( PROGRAM_NAME, message, FAILURE )
      STOP
    END IF
  ELSEIF ( Aerosol_Model == 'NAAPS' ) THEN
    IF ( Coeff_Format == 'Binary' ) THEN
      AerosolCoeff_Format = 'Binary'
      AerosolCoeff_File   = 'AerosolCoeff.NAAPS.bin'
    ELSE IF ( Coeff_Format == 'netCDF' ) THEN
      AerosolCoeff_Format = 'netCDF'
      AerosolCoeff_File   = 'AerosolCoeff.NAAPS.nc4'
    ELSE
      message = 'Aerosol coefficient format is not supported'
      CALL Display_Message( PROGRAM_NAME, message, FAILURE )
      STOP
    END IF
  END IF

  ! ============================================================================
  ! 2. **** INITIALIZE THE CRTM ****
  !
  ! 2a. This initializes the CRTM for the sensors
  !     predefined in the example SENSOR_ID parameter.
  !     NOTE: The coefficient data file path is hard-
  !           wired for this example.
  ! --------------------------------------------------
  WRITE( *,'(/5x,"Initializing the CRTM...")' )

  error_status = CRTM_Init( (/ sensor_ID /), &
                        channelInfo, &
                        Aerosol_Model, &
                        AerosolCoeff_Format, &
                        AerosolCoeff_File, &
                        Cloud_Model, &
                        CloudCoeff_Format, &
                        CloudCoeff_File, &
                        File_Path=COEFFICIENT_PATH, &
                        NC_File_Path=NC_COEFFICIENT_PATH, &
                        Quiet=.TRUE.)

  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error initializing CRTM'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP
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
  ! Note that only those structure arrays with a channel
  ! dimension are allocated here because we've parameterized
  ! the number of profiles in the N_PROFILES parameter.
  !
  ! Users can make the number of profiles dynamic also, but
  ! then the INPUT arrays (Atmosphere, Surface) will also have to be allocated.
  ALLOCATE( RTSolution( n_Channels, N_PROFILES ), &
            RTSolution_TL( n_Channels, N_PROFILES ), &
            RTSolution_AD( n_Channels, N_PROFILES ), &
            STAT = Allocate_Status )
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
    Message = 'Error allocating CRTM Atmosphere structure'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP
  END IF
  Atm%Add_Extra_Layers = .FALSE.

  ! The input TL structure
  CALL CRTM_Atmosphere_Create( Atmosphere_TL, N_LAYERS, N_ABSORBERS, N_CLOUDS, N_AEROSOLS )
  IF ( ANY(.NOT. CRTM_Atmosphere_Associated(Atmosphere_TL)) ) THEN
    Message = 'Error allocating CRTM Atmosphere_TL structure'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP
  END IF


  ! The output AD structure
  CALL CRTM_Atmosphere_Create( Atmosphere_AD, N_LAYERS, N_ABSORBERS, N_CLOUDS, N_AEROSOLS )
  IF ( ANY(.NOT. CRTM_Atmosphere_Associated(Atmosphere_AD)) ) THEN
    Message = 'Error allocating CRTM Atmosphere_AD structure'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP
  END IF


CALL CRTM_RTSolution_Create(RTSolution,N_LAYERS)
CALL CRTM_RTSolution_Create(RTSolution_TL,N_LAYERS)
CALL CRTM_RTSolution_Create(RTSolution_AD,N_LAYERS)

  ! ============================================================================

  ! ============================================================================
  ! 4. **** ASSIGN INPUT DATA ****
  !
  ! Fill the Atmosphere structure array.
  ! NOTE: This is an example program for illustrative purposes only.
  !       Typically, one would not assign the data as shown below,
  !       but rather read it from file

  ! 4a. Atmosphere and Surface input
  ! --------------------------------
  CALL Load_Atm_Data()
  CALL Load_Sfc_Data()
  ilev1 = 61
  ilev2 = 77
  iprof = 1
  ichan = 16
  Attenuated_Reflectivity = .TRUE.
  n_Layer_Cloud = COUNT(Atm(iprof)%Cloud(1)%Water_Content .GT. ZERO)
  ALLOCATE(TL_final(n_Layer_Cloud), AD_Final(n_Layer_Cloud), i_Layers_cloud(n_Layer_Cloud))
  i_layers = (/(i, i=1,n_Layers)/) 
  
  Do jj=1,2
    atm(jj)%Height = Calculate_Height(Atm(jj))
    Atmosphere_TL(jj)%Height = atm(jj)%Height
    Atmosphere_AD(jj)%Height = atm(jj)%Height
    
    Atmosphere_TL(jj)%Climatology = atm(jj)%Climatology
    Atmosphere_TL(jj)%Absorber_Id = atm(jj)%Absorber_Id
    Atmosphere_TL(jj)%Absorber_Units = atm(jj)%Absorber_Units
    Atmosphere_TL(jj)%Add_Extra_Layers = .FALSE.

    Atmosphere_AD(jj)%Climatology = atm(jj)%Climatology
    Atmosphere_AD(jj)%Absorber_Id = atm(jj)%Absorber_Id
    Atmosphere_AD(jj)%Absorber_Units = atm(jj)%Absorber_Units
    Atmosphere_AD(jj)%Add_Extra_Layers = .FALSE.
  END DO

  ! 4b. GeometryInfo input
  ! ----------------------
  ! All profiles are given the same value
  !  The Sensor_Scan_Angle is optional.
  CALL CRTM_Geometry_SetValue( Geometry, &
                               Sensor_Zenith_Angle = ZENITH_ANGLE, &
                               Sensor_Scan_Angle   = SCAN_ANGLE )
  ! ============================================================================
  ! Forwad model is not used
  Error_Status = CRTM_Forward( Atm         , &
                                  Sfc         , &
                                  Geometry    , &
                                  ChannelInfo , &
                                  RTSolution  )
                                  
  ! ============================================================================
  ! 5. **** INITIALIZE THE TL ARGUMENTS ****
  
  ! 5a. Zero the LT INPUT structures
  ! ---------------------------------------
  CALL CRTM_Atmosphere_Zero( Atmosphere_TL )
  CALL CRTM_Surface_Zero( Surface_TL )
  Perturbation = ONE  ! kg/m2
  Atmosphere_TL(1)%Cloud(1)%Water_Content = Perturbation
  Atmosphere_TL(2)%Cloud(1)%Water_Content = Perturbation
  
  Error_Status = CRTM_Tangent_Linear( Atm , &
                                    Sfc , &
                                    Atmosphere_TL , &
                                    Surface_TL , &
                                    Geometry , &
                                    ChannelInfo , &
                                    RTSolution , &
                                    RTSolution_TL  )
  IF ( Error_Status /= SUCCESS ) THEN
   Message = 'Error in CRTM Tangent-linear Model'
   CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
   STOP
  END IF

  ! ============================================================================


  ! ============================================================================
  ! 6. **** CALL THE CRTM ADJOINT MODEL ****
  !

  CALL CRTM_Atmosphere_Zero( Atmosphere_AD )
  CALL CRTM_Surface_Zero( Surface_AD )

  ! Initialise the Adjoint INPUT to provide dR/dx derivatives
  RTSolution_AD%Brightness_Temperature = ZERO
  RTSolution_AD%Radiance = ZERO
  DO jj=1, N_LAYERS
    RTSolution_AD%Reflectivity_Attenuated(jj) = ZERO
    RTSolution_AD%Reflectivity(jj) = ZERO
    if (Attenuated_Reflectivity) then
       RTSolution_AD(ichan,:)%Reflectivity_Attenuated(jj) = ONE 
    else
       RTSolution_AD(ichan,:)%Reflectivity(jj) = ONE 
    endif   
  ENDDO
  
  Error_Status = CRTM_Adjoint( Atm , &
                              Sfc , &
                              RTSolution_AD, &
                              Geometry, &
                              ChannelInfo, &
                              Atmosphere_AD, &
                              Surface_AD, &
                              RTSolution  )
  IF ( Error_Status /= SUCCESS ) THEN
   Message = 'Error in Adjoint Model'
   CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
   STOP
  END IF
  ! ============================================================================


  ! ============================================================================
  !  **** PERFORM ADJOINT TEST ****
  !
  if (Attenuated_Reflectivity) then
     TL = RTSolution_TL(ichan,iprof)%Reflectivity_Attenuated
  else
     TL = RTSolution_TL(ichan,iprof)%Reflectivity
  endif   
  AD = Atmosphere_AD(iprof)%Cloud(1)%Water_Content
  
  TL_final = PACK(TL, Atm(iprof)%Cloud(1)%Water_Content .GT. ZERO)
  AD_final = PACK(AD, Atm(iprof)%Cloud(1)%Water_Content .GT. ZERO)
  i_layers_Cloud = PACK(i_Layers, Atm(iprof)%Cloud(1)%Water_Content .GT. ZERO)
  
  WRITE(*,'(3A30)')   'TL', 'AD', 'ABS(TL-AD)/TL'
  DO jj=1,N_LAYER_CLOUD
    i = N_LAYER_CLOUD + 1 - jj
   !if (Atm(iprof)%Cloud(1)%Water_Content(jj) .GT. 0) THEN
      WRITE(*,'(3F30.10)') TL_FINAL(jj), AD_FINAL(i), ABS(TL_FINAL(jj) - AD_FINAL(i)) / TL_FINAL(jj)
   !END IF   
  END DO

  ! ============================================================================
  ! 8. **** DESTROY THE CRTM ****
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
  ! 10. **** CLEAN UP ****
  !
  ! 10a. Deallocate the structures.
  !      These are the explicitly allocated structures.
  !      Note that in some cases other structures, such as the Sfc
  !      and RTSolution structures, will also be allocated and thus
  !      should also be deallocated here.
  ! ---------------------------------------------------------------
  CALL CRTM_Atmosphere_Destroy(Atmosphere_TL)
  CALL CRTM_Atmosphere_Destroy(Atm)

  ! 10b. Deallocate the arrays
  ! --------------------------
  DEALLOCATE(RTSolution, RTSolution_TL, &
             STAT = Allocate_Status)
  ! ============================================================================

 
CONTAINS

  INCLUDE 'Load_Atm_Data.inc'
  INCLUDE 'Load_Sfc_Data.inc'

END PROGRAM test_AD
