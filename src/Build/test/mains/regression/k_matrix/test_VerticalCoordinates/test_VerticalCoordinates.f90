!
! test_VerticalCoordinates
!
! Test program for the CRTM K-Matrix function using different vertical
! coordinates from the training set, under clear sky conditions.
!
!

PROGRAM test_VerticalCoordinates

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
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'test_VerticalCoordinates'
  CHARACTER(*), PARAMETER :: COEFFICIENTS_PATH = './testinput/'
  CHARACTER(*), PARAMETER :: RESULTS_PATH = './results/k_matrix/'

  ! ============================================================================
  ! 0. **** SOME SET UP PARAMETERS FOR THIS TEST ****
  !
  ! Profile dimensions...
  INTEGER, PARAMETER :: N_PROFILES   = 2
  INTEGER, PARAMETER :: N_LAYERS     = 92
  INTEGER, PARAMETER :: N_NAM_LAYERS = 60
  INTEGER, PARAMETER :: N_GFS_LAYERS = 64
  INTEGER, PARAMETER :: N_ABSORBERS  = 2
  INTEGER, PARAMETER :: N_CLOUDS     = 0
  INTEGER, PARAMETER :: N_AEROSOLS   = 0
  ! ...but only ONE Sensor at a time
  INTEGER, PARAMETER :: N_SENSORS = 1

!!$  INTEGER, PARAMETER ::        INVALID_AEROSOL = 0
!!$  INTEGER, PARAMETER ::           DUST_AEROSOL = 1
!!$  INTEGER, PARAMETER ::   SEASALT_SSAM_AEROSOL = 2
!!$  INTEGER, PARAMETER ::  SEASALT_SSCM1_AEROSOL = 3
!!$  INTEGER, PARAMETER ::  SEASALT_SSCM2_AEROSOL = 4
!!$  INTEGER, PARAMETER ::  SEASALT_SSCM3_AEROSOL = 5
!!$  INTEGER, PARAMETER :: ORGANIC_CARBON_AEROSOL = 6
!!$  INTEGER, PARAMETER ::   BLACK_CARBON_AEROSOL = 7
!!$  INTEGER, PARAMETER ::        SULFATE_AEROSOL = 8


  ! Test GeometryInfo angles. The test scan angle is based
  ! on the default Re (earth radius) and h (satellite height)
  REAL(fp), PARAMETER :: ZENITH_ANGLE = 30.0_fp
  REAL(fp), PARAMETER :: SCAN_ANGLE   = 26.37293341421_fp
  REAL(fp), PARAMETER :: SOURCE_ZENITH_ANGLE = 0.0_fp
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
  ! Declarations for Jacobian comparisons
  INTEGER :: n_la, n_ma
  INTEGER :: n_ls, n_ms
  INTEGER :: n_NAM_la, n_NAM_ma
  INTEGER :: n_NAM_ls, n_NAM_ms
  INTEGER :: n_GFS_la, n_GFS_ma
  INTEGER :: n_GFS_ls, n_GFS_ms
  CHARACTER(256) :: atmk_File, sfck_File
  CHARACTER(256) :: atmk_NAM_File, sfck_NAM_File
  CHARACTER(256) :: atmk_GFS_File, sfck_GFS_File
  TYPE(CRTM_Atmosphere_type), ALLOCATABLE :: atm_k(:,:)
  TYPE(CRTM_Atmosphere_type), ALLOCATABLE :: atm_NAM_k(:,:)
  TYPE(CRTM_Atmosphere_type), ALLOCATABLE :: atm_GFS_k(:,:)
  TYPE(CRTM_Surface_type)   , ALLOCATABLE :: sfc_k(:,:)
  TYPE(CRTM_Surface_type)   , ALLOCATABLE :: sfc_NAM_k(:,:)
  TYPE(CRTM_Surface_type)   , ALLOCATABLE :: sfc_GFS_k(:,:)

  ! ============================================================================
  ! 1. **** DEFINE THE CRTM INTERFACE STRUCTURES ****
  !
  TYPE(CRTM_ChannelInfo_type)             :: ChannelInfo(N_SENSORS)
  TYPE(CRTM_Geometry_type)                :: Geometry(N_PROFILES)
  TYPE(CRTM_Options_type)                 :: Options(N_PROFILES)

  ! Define the FORWARD variables
  TYPE(CRTM_Atmosphere_type)              :: Atm(N_PROFILES)
  TYPE(CRTM_Atmosphere_type)              :: Atm_NAM(N_PROFILES)
  TYPE(CRTM_Atmosphere_type)              :: Atm_GFS(N_PROFILES)
  TYPE(CRTM_Surface_type)                 :: Sfc(N_PROFILES)
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: RTSolution(:,:)
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: RTSolution_NAM(:,:)
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: RTSolution_GFS(:,:)

  ! Define the K-MATRIX variables
  TYPE(CRTM_Atmosphere_type), ALLOCATABLE :: Atmosphere_K(:,:)
  TYPE(CRTM_Atmosphere_type), ALLOCATABLE :: Atmosphere_NAM_K(:,:)
  TYPE(CRTM_Atmosphere_type), ALLOCATABLE :: Atmosphere_GFS_K(:,:)
  TYPE(CRTM_Surface_type)   , ALLOCATABLE :: Surface_K(:,:)
  TYPE(CRTM_Surface_type)   , ALLOCATABLE :: Surface_NAM_K(:,:)
  TYPE(CRTM_Surface_type)   , ALLOCATABLE :: Surface_GFS_K(:,:)
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: RTSolution_K(:,:)
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: RTSolution_NAM_K(:,:)
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: RTSolution_GFS_K(:,:)
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
    'Test program for the CRTM K-Matrix function using different vertical '//&
    'coordinates from the training set, under clear sky conditions.', &
    'CRTM Version: '//TRIM(Version) )


  ! Get sensor id from user
  ! -----------------------
  Sensor_Id = ADJUSTL(Sensor_Id)
  WRITE( *,'(//5x,"Running CRTM for ",a," sensor...")' ) TRIM(PROGRAM_NAME)//'_'//TRIM(Sensor_Id)



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
  ALLOCATE( RTSolution( n_Channels, N_PROFILES ), &
            RTSolution_NAM( n_Channels, N_PROFILES ), &
            RTSolution_GFS( n_Channels, N_PROFILES ), &
            Atmosphere_K( n_Channels, N_PROFILES ), &
            Atmosphere_NAM_K( n_Channels, N_PROFILES ), &
            Atmosphere_GFS_K( n_Channels, N_PROFILES ), &
            Surface_K( n_Channels, N_PROFILES ), &
            Surface_NAM_K( n_Channels, N_PROFILES ), &
            Surface_GFS_K( n_Channels, N_PROFILES ), &
            RTSolution_K( n_Channels, N_PROFILES ), &
            RTSolution_NAM_K( n_Channels, N_PROFILES ), &
            RTSolution_GFS_K( n_Channels, N_PROFILES ), &
            STAT = Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    Message = 'Error allocating structure arrays'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF

  ! 3b. Allocate the STRUCTURES
  ! ---------------------------

  ! ----------------------------
  ! The input FORWARD structures
  ! ----------------------------
  ! CRTM build test vertical coordinates
  CALL CRTM_Atmosphere_Create( Atm, N_LAYERS, N_ABSORBERS, N_CLOUDS, N_AEROSOLS )
  IF ( ANY(.NOT. CRTM_Atmosphere_Associated(Atm)) ) THEN
    Message = 'Error allocating CRTM Atmosphere structure'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF
  ! NAM build test vertical coordinates
  CALL CRTM_Atmosphere_Create( Atm_NAM, N_NAM_LAYERS, N_ABSORBERS, N_CLOUDS, N_AEROSOLS )
  IF ( ANY(.NOT. CRTM_Atmosphere_Associated(Atm)) ) THEN
    Message = 'Error allocating CRTM Atmosphere_NAM structure'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF
  ! GFS build test vertical coordinates
  CALL CRTM_Atmosphere_Create( Atm_GFS, N_GFS_LAYERS, N_ABSORBERS, N_CLOUDS, N_AEROSOLS )
  IF ( ANY(.NOT. CRTM_Atmosphere_Associated(Atm)) ) THEN
    Message = 'Error allocating CRTM Atmosphere_GFS structure'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF
  ! ------------------------------
  ! The output K-MATRIX structures
  ! ------------------------------
  ! CRTM output
  CALL CRTM_Atmosphere_Create( Atmosphere_K, N_LAYERS, N_ABSORBERS, N_CLOUDS, N_AEROSOLS )
  IF ( ANY(.NOT. CRTM_Atmosphere_Associated(Atmosphere_K)) ) THEN
    Message = 'Error allocating CRTM Atmosphere_K structure'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF
  ! NAM output
  CALL CRTM_Atmosphere_Create( Atmosphere_NAM_K, N_NAM_LAYERS, N_ABSORBERS, N_CLOUDS, N_AEROSOLS )
  IF ( ANY(.NOT. CRTM_Atmosphere_Associated(Atmosphere_K)) ) THEN
    Message = 'Error allocating CRTM Atmosphere_NAM_K structure'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF
  ! GFS output
  CALL CRTM_Atmosphere_Create( Atmosphere_GFS_K, N_GFS_LAYERS, N_ABSORBERS, N_CLOUDS, N_AEROSOLS )
  IF ( ANY(.NOT. CRTM_Atmosphere_Associated(Atmosphere_K)) ) THEN
    Message = 'Error allocating CRTM Atmosphere_GFS_K structure'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF
  ! ============================================================================



  ! ============================================================================
  ! 4. **** ASSIGN INPUT DATA ****
  !
  ! 4a. Atmosphere and Surface input
  ! --------------------------------
  CALL Load_Atm_Data()
  CALL Load_Sfc_Data()
  CALL Map_To_NCEP_Model_Coordinates()


  ! 4b. Geometry input
  ! ------------------
  ! All profiles are given the same value
  !  The Sensor_Scan_Angle is optional.
  CALL CRTM_Geometry_SetValue( Geometry, &
                               Sensor_Zenith_Angle = ZENITH_ANGLE, &
                               Sensor_Scan_Angle   = SCAN_ANGLE, &
                               Source_Zenith_Angle = SOURCE_ZENITH_ANGLE )
  ! ============================================================================




  ! ============================================================================
  ! 5. **** INITIALIZE THE K-MATRIX ARGUMENTS ****
  !
  ! 5a. Zero the K-matrix OUTPUT structures
  ! ---------------------------------------
  CALL CRTM_Atmosphere_Zero( Atmosphere_K )
  CALL CRTM_Atmosphere_Zero( Atmosphere_NAM_K )
  CALL CRTM_Atmosphere_Zero( Atmosphere_GFS_K )
  CALL CRTM_Surface_Zero( Surface_K )
  CALL CRTM_Surface_Zero( Surface_NAM_K )
  CALL CRTM_Surface_Zero( Surface_GFS_K )

  ! 5b. Inintialize the K-matrix INPUT so
  !     that the IR/MW results are dTb/dx
  !     and the visible results are dR/dx
  ! -------------------------------------
  DO l = 1, n_Channels
    IF ( ChannelInfo(1)%Sensor_Type == INFRARED_SENSOR .OR. &
         ChannelInfo(1)%Sensor_Type == MICROWAVE_SENSOR ) THEN
      RTSolution_K(l,:)%Radiance     = ZERO
      RTSolution_NAM_K(l,:)%Radiance = ZERO
      RTSolution_GFS_K(l,:)%Radiance = ZERO
      RTSolution_K(l,:)%Brightness_Temperature     = ONE
      RTSolution_NAM_K(l,:)%Brightness_Temperature = ONE
      RTSolution_GFS_K(l,:)%Brightness_Temperature = ONE
    ELSE
      RTSolution_K(l,:)%Radiance     = ONE
      RTSolution_NAM_K(l,:)%Radiance = ONE
      RTSolution_GFS_K(l,:)%Radiance = ONE
      RTSolution_K(l,:)%Brightness_Temperature     = ZERO
      RTSolution_NAM_K(l,:)%Brightness_Temperature = ZERO
      RTSolution_GFS_K(l,:)%Brightness_Temperature = ZERO
    END IF
  END DO
  ! ============================================================================




  ! ============================================================================
  ! 6. **** CALL THE CRTM K-Matrix MODEL ****
  !

  ! CRTM build test vertical resolution
  Error_Status = CRTM_K_Matrix( Atm         , &
                                Sfc         , &
                                RTSolution_K, &
                                Geometry    , &
                                ChannelInfo , &
                                Atmosphere_K, &
                                Surface_K   , &
                                RTSolution  , &
                                Options = Options )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error in CRTM K_Matrix Model (Build test resolution)'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF

  ! Regional vertical resolution
  Error_Status = CRTM_K_Matrix( Atm_NAM         , &
                                Sfc             , &
                                RTSolution_NAM_K, &
                                Geometry        , &
                                ChannelInfo     , &
                                Atmosphere_NAM_K, &
                                Surface_NAM_K   , &
                                RTSolution_NAM  , &
                                Options = Options )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error in CRTM K_Matrix Model (NAM resolution)'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF

  ! Global vertical resolution
  Error_Status = CRTM_K_Matrix( Atm_GFS         , &
                                Sfc             , &
                                RTSolution_GFS_K, &
                                Geometry        , &
                                ChannelInfo     , &
                                Atmosphere_GFS_K, &
                                Surface_GFS_K   , &
                                RTSolution_GFS  , &
                                Options = Options )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error in CRTM K_Matrix Model (GFS resolution)'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF
  ! ============================================================================




  ! ============================================================================
  ! 7. **** OUTPUT THE RESULTS TO SCREEN ****
  !
  DO m = 1, N_PROFILES
    WRITE( *,'(//7x,"Profile ",i0," output for ",a )') m, TRIM(PROGRAM_NAME)//'_'//TRIM(Sensor_Id)
    DO l = 1, n_Channels
      WRITE( *, '(/5x,"Channel ",i0," results")') RTSolution(l,m)%Sensor_Channel
      ! FWD output
      WRITE( *, '(/3x,"FORWARD OUTPUT")')
      CALL CRTM_RTSolution_Inspect(RTSolution(l,m))
      CALL CRTM_RTSolution_Inspect(RTSolution_NAM(l,m))
      CALL CRTM_RTSolution_Inspect(RTSolution_GFS(l,m))
      ! K-MATRIX output
      WRITE( *, '(/3x,"K-MATRIX OUTPUT")')
      CALL CRTM_Surface_Inspect(Surface_K(l,m))
      CALL CRTM_Surface_Inspect(Surface_NAM_K(l,m))
      CALL CRTM_Surface_Inspect(Surface_GFS_K(l,m))
      CALL CRTM_Atmosphere_Inspect(Atmosphere_K(l,m))
      CALL CRTM_Atmosphere_Inspect(Atmosphere_NAM_K(l,m))
      CALL CRTM_Atmosphere_Inspect(Atmosphere_GFS_K(l,m))
    END DO
  END DO
  ! ============================================================================

  ! ============================================================================
  ! 9. **** COMPARE Atmosphere_K and Surface_K RESULTS TO SAVED VALUES ****
  !
  WRITE( *, '( /5x, "Comparing calculated results with saved ones..." )' )

  ! 9a. Create the output files if they do not exist
  ! ------------------------------------------------
  ! 9a.1 Atmosphere file
  ! ...Generate filename
  atmk_File = RESULTS_PATH//TRIM(PROGRAM_NAME)//'_'//TRIM(Sensor_Id)//'.Atmosphere.bin'
  atmk_NAM_File = RESULTS_PATH//TRIM(PROGRAM_NAME)//'_'//TRIM(Sensor_Id)//'.NAM.Atmosphere.bin'
  atmk_GFS_File = RESULTS_PATH//TRIM(PROGRAM_NAME)//'_'//TRIM(Sensor_Id)//'.GFS.Atmosphere.bin'

  ! Assign pressures for k-matrix plotting
  DO l = 1, n_Channels
    Atmosphere_NAM_K(l,1)%Pressure = Atm_NAM(1)%Pressure
    Atmosphere_GFS_K(l,1)%Pressure = Atm_GFS(1)%Pressure
    Atmosphere_NAM_K(l,2)%Pressure = Atm_NAM(2)%Pressure
    Atmosphere_GFS_K(l,2)%Pressure = Atm_GFS(2)%Pressure
  END DO
  ! ...Check if the files exist
  ! build test vertical resolution
  IF ( .NOT. File_Exists(atmk_File) ) THEN
    Message = 'Atmosphere_K save file does not exist. Creating...'
    CALL Display_Message( PROGRAM_NAME, Message, INFORMATION )
    ! ...File not found, so write Atmosphere_K structure to file
    Error_Status = CRTM_Atmosphere_WriteFile( atmk_file, Atmosphere_K, Quiet=.TRUE. )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating Atmosphere_K save file'
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
      STOP 1
    END IF
  END IF
  ! NAM vertical resolution
  IF ( .NOT. File_Exists(atmk_NAM_File) ) THEN
    Message = 'Atmosphere_NAM_K save file does not exist. Creating...'
    CALL Display_Message( PROGRAM_NAME, Message, INFORMATION )
    ! ...File not found, so write Atmosphere_K structure to file
    Error_Status = CRTM_Atmosphere_WriteFile( atmk_NAM_file, Atmosphere_NAM_K, Quiet=.TRUE. )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating Atmosphere_NAM_K save file'
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
      STOP 1
    END IF
  END IF
  ! GFS vertical resolution
  IF ( .NOT. File_Exists(atmk_GFS_File) ) THEN
    Message = 'Atmosphere_GFS_K save file does not exist. Creating...'
    CALL Display_Message( PROGRAM_NAME, Message, INFORMATION )
    ! ...File not found, so write Atmosphere_K structure to file
    Error_Status = CRTM_Atmosphere_WriteFile( atmk_GFS_file, Atmosphere_GFS_K, Quiet=.TRUE. )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating Atmosphere_GFS_K save file'
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
      STOP 1
    END IF
  END IF
  ! 9a.2 Surface file
  ! ...Generate filename
  sfck_File = RESULTS_PATH//TRIM(PROGRAM_NAME)//'_'//TRIM(Sensor_Id)//'.Surface.bin'
  sfck_NAM_File = RESULTS_PATH//TRIM(PROGRAM_NAME)//'_'//TRIM(Sensor_Id)//'.NAM.Surface.bin'
  sfck_GFS_File = RESULTS_PATH//TRIM(PROGRAM_NAME)//'_'//TRIM(Sensor_Id)//'.GFS.Surface.bin'
  ! ...Check if the file exists
  ! CRTM build
  IF ( .NOT. File_Exists(sfck_File) ) THEN
    Message = 'Surface_K save file does not exist. Creating...'
    CALL Display_Message( PROGRAM_NAME, Message, INFORMATION )
    ! ...File not found, so write Surface_K structure to file
    Error_Status = CRTM_Surface_WriteFile( sfck_file, Surface_K, Quiet=.TRUE. )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating Surface_K save file'
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
      STOP 1
    END IF
  END IF
  ! NAM results
  IF ( .NOT. File_Exists(sfck_NAM_File) ) THEN
    Message = 'Surface_NAM_K save file does not exist. Creating...'
    CALL Display_Message( PROGRAM_NAME, Message, INFORMATION )
    ! ...File not found, so write Surface_K structure to file
    Error_Status = CRTM_Surface_WriteFile( sfck_NAM_file, Surface_NAM_K, Quiet=.TRUE. )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating Surface_NAM_K save file'
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
      STOP 1
    END IF
  END IF
  ! GFS results
  IF ( .NOT. File_Exists(sfck_GFS_File) ) THEN
    Message = 'Surface_GFS_K save file does not exist. Creating...'
    CALL Display_Message( PROGRAM_NAME, Message, INFORMATION )
    ! ...File not found, so write Surface_K structure to file
    Error_Status = CRTM_Surface_WriteFile( sfck_GFS_file, Surface_GFS_K, Quiet=.TRUE. )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating Surface_GFS_K save file'
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
      STOP 1
    END IF
  END IF

  ! 9b. Inquire the saved files
  ! ---------------------------
  ! 9b.1 Atmosphere file
  ! build test atmosphere
  Error_Status = CRTM_Atmosphere_InquireFile( atmk_File, &
                                              n_Channels = n_la, &
                                              n_Profiles = n_ma )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error inquiring Atmosphere_K save file'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF
  ! NAM atmosphere
  Error_Status = CRTM_Atmosphere_InquireFile( atmk_NAM_File, &
                                              n_Channels = n_NAM_la, &
                                              n_Profiles = n_NAM_ma )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error inquiring Atmosphere_NAM_K save file'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF
  ! GFS atmosphere
  Error_Status = CRTM_Atmosphere_InquireFile( atmk_GFS_File, &
                                              n_Channels = n_GFS_la, &
                                              n_Profiles = n_GFS_ma )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error inquiring Atmosphere_GFS_K save file'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF
  ! 9b.2 Surface file
  ! build test
  Error_Status = CRTM_Surface_InquireFile( sfck_File, &
                                           n_Channels = n_ls, &
                                           n_Profiles = n_ms )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error inquiring Surface_K save file'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF
  ! NAM
  Error_Status = CRTM_Surface_InquireFile( sfck_NAM_File, &
                                           n_Channels = n_NAM_ls, &
                                           n_Profiles = n_NAM_ms )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error inquiring Surface_NAM_K save file'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF
  ! GFS
  Error_Status = CRTM_Surface_InquireFile( sfck_GFS_File, &
                                           n_Channels = n_GFS_ls, &
                                           n_Profiles = n_GFS_ms )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error inquiring Surface_GFS_K save file'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF

  ! 9c. Compare the dimensions
  ! --------------------------
  IF ( n_la /= n_Channels .OR. n_ma /= N_PROFILES .OR. &
       n_ls /= n_Channels .OR. n_ms /= N_PROFILES .OR. &
       n_NAM_la /= n_Channels .OR. n_NAM_ma /= N_PROFILES .OR. &
       n_NAM_ls /= n_Channels .OR. n_NAM_ms /= N_PROFILES .OR. &
       n_GFS_la /= n_Channels .OR. n_GFS_ma /= N_PROFILES .OR. &
       n_GFS_ls /= n_Channels .OR. n_GFS_ms /= N_PROFILES      ) THEN
    Message = 'Dimensions of saved data different from that calculated!'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF

  ! 9d. Read the saved data
  ! -----------------------
  ! 9d.1 Atmosphere file
  ! Build test atmosphere
  Error_Status = CRTM_Atmosphere_ReadFile( atmk_File, atm_k, Quiet=.TRUE. )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error reading Atmosphere_K save file'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF
  ! NAM atmosphere
  Error_Status = CRTM_Atmosphere_ReadFile( atmk_NAM_File, atm_NAM_k, Quiet=.TRUE. )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error reading Atmosphere_NAM_K save file'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF
  ! GFS atmosphere
  Error_Status = CRTM_Atmosphere_ReadFile( atmk_GFS_File, atm_GFS_k, Quiet=.TRUE. )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error reading Atmosphere_GFS_K save file'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF
  ! 9d.2 Surface file
  ! Build
  Error_Status = CRTM_Surface_ReadFile( sfck_File, sfc_k, Quiet=.TRUE. )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error reading Surface_K save file'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF
  ! NAM
  Error_Status = CRTM_Surface_ReadFile( sfck_NAM_File, sfc_NAM_k, Quiet=.TRUE. )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error reading Surface_NAM_K save file'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF
  ! GFS
  Error_Status = CRTM_Surface_ReadFile( sfck_GFS_File, sfc_GFS_k, Quiet=.TRUE. )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error reading Surface_GFS_K save file'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF

  ! 9e. Compare some Jacobians
  ! --------------------------
  ! 9e.1 Atmosphere
  ! Build test resolution
  IF ( ALL(CRTM_Atmosphere_Compare(Atmosphere_K, atm_k)) ) THEN
    Message = 'Atmosphere_K Jacobians are the same!'
    CALL Display_Message( PROGRAM_NAME, Message, INFORMATION )
  ELSE
    Message = 'Atmosphere_K Jacobians are different!'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    ! Write the current Atmosphere_K results to file
    atmk_File = TRIM(PROGRAM_NAME)//'_'//TRIM(Sensor_Id)//'.Atmosphere.bin'
    Error_Status = CRTM_Atmosphere_WriteFile( atmk_file, Atmosphere_K, Quiet=.TRUE. )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating temporary Atmosphere_K save file for failed comparison'
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    END IF
  END IF
  ! NAM resolution
  IF ( ALL(CRTM_Atmosphere_Compare(Atmosphere_NAM_K, atm_NAM_k)) ) THEN
    Message = 'Atmosphere_NAM_K Jacobians are the same!'
    CALL Display_Message( PROGRAM_NAME, Message, INFORMATION )
  ELSE
    Message = 'Atmosphere_NAM_K Jacobians are different!'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    ! Write the current Atmosphere_NAM_K results to file
    atmk_NAM_File = TRIM(PROGRAM_NAME)//'_'//TRIM(Sensor_Id)//'.NAM.Atmosphere.bin'
    Error_Status = CRTM_Atmosphere_WriteFile( atmk_NAM_file, Atmosphere_NAM_K, Quiet=.TRUE. )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating temporary Atmosphere_NAM_K save file for failed comparison'
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    END IF
  END IF
  ! GFS resolution
  IF ( ALL(CRTM_Atmosphere_Compare(Atmosphere_GFS_K, atm_GFS_k)) ) THEN
    Message = 'Atmosphere_GFS_K Jacobians are the same!'
    CALL Display_Message( PROGRAM_NAME, Message, INFORMATION )
  ELSE
    Message = 'Atmosphere_GFS_K Jacobians are different!'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    ! Write the current Atmosphere_GFS_K results to file
    atmk_GFS_File = TRIM(PROGRAM_NAME)//'_'//TRIM(Sensor_Id)//'.GFS.Atmosphere.bin'
    Error_Status = CRTM_Atmosphere_WriteFile( atmk_GFS_file, Atmosphere_GFS_K, Quiet=.TRUE. )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating temporary Atmosphere_GFS_K save file for failed comparison'
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    END IF
  END IF

  ! 9e.2 Surface
  ! Build
  IF ( ALL(CRTM_Surface_Compare(Surface_K, sfc_k, n_SigFig=5)) ) THEN
    Message = 'Surface_K Jacobians are the same!'
    CALL Display_Message( PROGRAM_NAME, Message, INFORMATION )
  ELSE
    Message = 'Surface_K Jacobians are different!'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    ! Write the current Surface_K results to file
    sfck_File = TRIM(PROGRAM_NAME)//'_'//TRIM(Sensor_Id)//'.Surface.bin'
    Error_Status = CRTM_Surface_WriteFile( sfck_file, Surface_K, Quiet=.TRUE. )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating temporary Surface_K save file for failed comparison'
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    END IF
  END IF
  ! NAM
  IF ( ALL(CRTM_Surface_Compare(Surface_NAM_K, sfc_NAM_k, n_SigFig=5)) ) THEN
    Message = 'Surface_NAM_K Jacobians are the same!'
    CALL Display_Message( PROGRAM_NAME, Message, INFORMATION )
  ELSE
    Message = 'Surface_NAM_K Jacobians are different!'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    ! Write the current Surface_NAM_K results to file
    sfck_NAM_File = TRIM(PROGRAM_NAME)//'_'//TRIM(Sensor_Id)//'.NAM.Surface.bin'
    Error_Status = CRTM_Surface_WriteFile( sfck_NAM_file, Surface_NAM_K, Quiet=.TRUE. )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating temporary Surface_NAM_K save file for failed comparison'
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    END IF
  END IF
  ! GFS
  IF ( ALL(CRTM_Surface_Compare(Surface_GFS_K, sfc_GFS_k, n_SigFig=5)) ) THEN
    Message = 'Surface_GFS_K Jacobians are the same!'
    CALL Display_Message( PROGRAM_NAME, Message, INFORMATION )
  ELSE
    Message = 'Surface_GFS_K Jacobians are different!'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    ! Write the current Surface_GFS_K results to file
    sfck_GFS_File = TRIM(PROGRAM_NAME)//'_'//TRIM(Sensor_Id)//'.GFS.Surface.bin'
    Error_Status = CRTM_Surface_WriteFile( sfck_GFS_file, Surface_GFS_K, Quiet=.TRUE. )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating temporary Surface_GFS_K save file for failed comparison'
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    END IF
  END IF
  ! ============================================================================

  ! ============================================================================
  ! 8. **** DESTROY THE CRTM ****
  !
  WRITE( *, '( /5x, "Destroying the CRTM..." )' )
  Error_Status = CRTM_Destroy( ChannelInfo )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error destroying CRTM'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF
  ! ============================================================================

  ! ============================================================================
  ! 10. **** CLEAN UP ****
  !
  ! 10a. Deallocate the structures
  ! ------------------------------
  CALL CRTM_Atmosphere_Destroy(atm_K)
  CALL CRTM_Atmosphere_Destroy(atm_NAM_K)
  CALL CRTM_Atmosphere_Destroy(atm_GFS_K)
  CALL CRTM_Atmosphere_Destroy(Atmosphere_K)
  CALL CRTM_Atmosphere_Destroy(Atmosphere_NAM_K)
  CALL CRTM_Atmosphere_Destroy(Atmosphere_GFS_K)
  CALL CRTM_Atmosphere_Destroy(Atm)
  CALL CRTM_Atmosphere_Destroy(Atm_NAM)
  CALL CRTM_Atmosphere_Destroy(Atm_GFS)

  ! 10b. Deallocate the arrays
  ! --------------------------
  DEALLOCATE(RTSolution, RTSolution_K, &
             Surface_K, Atmosphere_K, &
             sfc_K, atm_K, &
             RTSolution_NAM, RTSolution_NAM_K, &
             Surface_NAM_K, Atmosphere_NAM_K, &
             sfc_NAM_K, atm_NAM_K, &
             RTSolution_GFS, RTSolution_GFS_K, &
             Surface_GFS_K, Atmosphere_GFS_K, &
             sfc_GFS_K, atm_GFS_K, &
             STAT = Allocate_Status)
  ! ============================================================================

  ! Signal the completion of the program. It is not a necessary step for running CRTM.

CONTAINS

  INCLUDE 'Load_Atm_Data.inc'
  INCLUDE 'Load_Sfc_Data.inc'
  INCLUDE 'Map_To_NCEP_Model_Coordinates.inc'

END PROGRAM test_VerticalCoordinates
