!
! test_VerticalCoordinates
!
! Test program for the CRTM Forward function using different vertical
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
  CHARACTER(*), PARAMETER :: RESULTS_PATH = './results/forward/'

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
  ! Declarations for RTSolution comparison
  INTEGER :: n_l, n_m
  INTEGER :: n_NAM_l, n_NAM_m
  INTEGER :: n_GFS_l, n_GFS_m
  CHARACTER(256) :: rts_File
  CHARACTER(256) :: rts_NAM_File
  CHARACTER(256) :: rts_GFS_File
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: rts(:,:)
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: rts_NAM(:,:)
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: rts_GFS(:,:)


  ! ============================================================================
  ! 1. **** DEFINE THE CRTM INTERFACE STRUCTURES ****
  !
  TYPE(CRTM_ChannelInfo_type)             :: ChannelInfo(N_SENSORS)
  TYPE(CRTM_Geometry_type)                :: Geometry(N_PROFILES)
  TYPE(CRTM_Atmosphere_type)              :: Atm(N_PROFILES)
  TYPE(CRTM_Surface_type)                 :: Sfc(N_PROFILES)
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: RTSolution(:,:)
  TYPE(CRTM_Options_type)                 :: Opt(N_PROFILES)
  TYPE(CRTM_Atmosphere_type)              :: Atm_NAM(N_PROFILES)
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: RTSolution_NAM(:,:)
  TYPE(CRTM_Atmosphere_type)              :: Atm_GFS(N_PROFILES)
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: RTSolution_GFS(:,:)
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
    'Test program for the CRTM Forward function using different vertical '//&
    'coordinates from the training set, under clear sky conditions.', &
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


  ! 2b. Determine the number of channels the
  !     CRTM is to process
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
            STAT=Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    Message = 'Error allocating structure arrays'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF

  ! 3b. Allocate the STRUCTURES
  ! ---------------------------
  ! CRTM build test atmosphere
  CALL CRTM_Atmosphere_Create( Atm, N_LAYERS, N_ABSORBERS, N_CLOUDS, N_AEROSOLS )
  IF ( ANY(.NOT. CRTM_Atmosphere_Associated(Atm)) ) THEN
    Message = 'Error allocating CRTM Atmosphere structures'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF
  ! Regional vertical coordinate atmosphere
  CALL CRTM_Atmosphere_Create( Atm_NAM, N_NAM_LAYERS, N_ABSORBERS, N_CLOUDS, N_AEROSOLS )
  IF ( ANY(.NOT. CRTM_Atmosphere_Associated(Atm_NAM)) ) THEN
    Message = 'Error allocating CRTM Atmosphere structures (regional)'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF
  ! Global vertical coordinate atmosphere
  CALL CRTM_Atmosphere_Create( Atm_GFS, N_GFS_LAYERS, N_ABSORBERS, N_CLOUDS, N_AEROSOLS )
  IF ( ANY(.NOT. CRTM_Atmosphere_Associated(Atm_GFS)) ) THEN
    Message = 'Error allocating CRTM Atmosphere structures (global)'
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
  CALL Load_Atm_Data()
  CALL Load_Sfc_Data()
  CALL Map_To_NCEP_Model_Coordinates()


  ! 4b. GeometryInfo input
  ! ----------------------
  ! All profiles are given the same value
  !  The Sensor_Scan_Angle is optional.
  CALL CRTM_Geometry_SetValue( Geometry, &
                               Sensor_Zenith_Angle = ZENITH_ANGLE, &
                               Sensor_Scan_Angle   = SCAN_ANGLE, &
                               Source_Zenith_Angle = SOURCE_ZENITH_ANGLE )
  ! ============================================================================




  ! ============================================================================
  ! 5. **** CALL THE CRTM FORWARD MODEL ****
  !
  ! CRTM build atmosphere calculations
  Error_Status = CRTM_Forward( Atm        , &
                               Sfc        , &
                               Geometry   , &
                               ChannelInfo, &
                               RTSolution , &
                               Options = Opt )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error in CRTM Forward Model'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF
  ! regional atmosphere calculations
  Error_Status = CRTM_Forward( Atm_NAM        , &
                               Sfc            , &
                               Geometry       , &
                               ChannelInfo    , &
                               RTSolution_NAM , &
                               Options = Opt )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error in CRTM Forward Model (regional)'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF
  ! global atmosphere calculations
  Error_Status = CRTM_Forward( Atm_GFS        , &
                               Sfc            , &
                               Geometry       , &
                               ChannelInfo    , &
                               RTSolution_GFS , &
                               Options = Opt )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error in CRTM Forward Model (global)'
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
      CALL CRTM_RTSolution_Inspect(RTSolution_NAM(l,m))
      CALL CRTM_RTSolution_Inspect(RTSolution_GFS(l,m))
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

  ! 8a. Create the output file if necessary
  ! ---------------------------------------
  ! ...Generate filenames
  rts_File = RESULTS_PATH//TRIM(PROGRAM_NAME)//'_'//TRIM(Sensor_Id)//'.RTSolution.bin'
  rts_NAM_File = RESULTS_PATH//TRIM(PROGRAM_NAME)//'_'//TRIM(Sensor_Id)//'.NAM.RTSolution.bin'
  rts_GFS_File = RESULTS_PATH//TRIM(PROGRAM_NAME)//'_'//TRIM(Sensor_Id)//'.GFS.RTSolution.bin'
  ! ------------------------------------
  ! Write CRTM forward output to file if
  ! result files do not already exist
  ! ------------------------------------
  ! CRTM build atmospheres
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
  ! regional atmosphere
  IF ( .NOT. File_Exists(rts_NAM_File) ) THEN
    Message = 'regional RTSolution save file does not exist. Creating...'
    CALL Display_Message( PROGRAM_NAME, Message, INFORMATION )
    Error_Status = CRTM_RTSolution_WriteFile( rts_NAM_File, RTSolution_NAM, Quiet=.TRUE. )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating regional RTSolution save file'
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
      STOP 1
    END IF
  END IF
  ! global atmosphere
  IF ( .NOT. File_Exists(rts_GFS_File) ) THEN
    Message = 'global RTSolution save file does not exist. Creating...'
    CALL Display_Message( PROGRAM_NAME, Message, INFORMATION )
    Error_Status = CRTM_RTSolution_WriteFile( rts_GFS_File, RTSolution_GFS, Quiet=.TRUE. )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating global RTSolution save file'
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
      STOP 1
    END IF
  END IF

  ! 8b. Inquire the saved file
  ! --------------------------
  ! CRTM build atmosphere
  Error_Status = CRTM_RTSolution_InquireFile( rts_File, &
                                              n_Channels = n_l, &
                                              n_Profiles = n_m )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error inquiring RTSolution save file'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF
  ! regional atmosphere
  Error_Status = CRTM_RTSolution_InquireFile( rts_NAM_File, &
                                              n_Channels = n_NAM_l, &
                                              n_Profiles = n_NAM_m )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error inquiring regional RTSolution save file'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF
  ! global atmosphere
  Error_Status = CRTM_RTSolution_InquireFile( rts_GFS_File, &
                                              n_Channels = n_GFS_l, &
                                              n_Profiles = n_GFS_m )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error inquiring global RTSolution save file'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF
  ! 8c. Compare the dimensions
  ! --------------------------
  IF ( n_l /= n_Channels .OR. n_m /= N_PROFILES .OR. &
       n_NAM_l /= n_Channels .OR. n_NAM_m /= N_PROFILES .OR. &
       n_GFS_l /= n_Channels .OR. n_GFS_m /= N_PROFILES      ) THEN
    Message = 'Dimensions of saved data different from that calculated!'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF

  ! 8d. Allocate structures to read in saved data
  ! ---------------------------------------------
  ALLOCATE( rts( n_l, n_m ), &
            rts_NAM( n_NAM_l, n_NAM_m ), &
            rts_GFS( n_GFS_l, n_GFS_m ), &
            STAT=Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    Message = 'Error allocating RTSolution saved data array'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF

  ! 8e. Read the saved data
  ! -----------------------
  ! CRTM build atmosphere
  Error_Status = CRTM_RTSolution_ReadFile( rts_File, rts, Quiet=.TRUE. )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error reading RTSolution save file'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF
  ! regional atmosphere
  Error_Status = CRTM_RTSolution_ReadFile( rts_NAM_File, rts_NAM, Quiet=.TRUE. )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error reading regional RTSolution save file'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF
  ! global atmosphere
  Error_Status = CRTM_RTSolution_ReadFile( rts_GFS_File, rts_GFS, Quiet=.TRUE. )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error reading global RTSolution save file'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF

  ! 8f. Compare the structures
  ! --------------------------
  ! CRTM build atmosphere
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
  END IF
  ! regional atmosphere
  IF ( ALL(CRTM_RTSolution_Compare(RTSolution_NAM, rts_NAM)) ) THEN
    Message = 'regional RTSolution results are the same!'
    CALL Display_Message( PROGRAM_NAME, Message, INFORMATION )
  ELSE
    Message = 'regional RTSolution results are different!'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    ! Write the current RTSolution results to file
    rts_NAM_File = TRIM(PROGRAM_NAME)//'_'//TRIM(Sensor_Id)//'.NAM.RTSolution.bin'
    Error_Status = CRTM_RTSolution_WriteFile( rts_NAM_File, RTSolution_NAM, Quiet=.TRUE. )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating temporary regional RTSolution save file for failed comparison'
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    END IF
  END IF
  ! global atmosphere
  IF ( ALL(CRTM_RTSolution_Compare(RTSolution_GFS, rts_GFS)) ) THEN
    Message = 'global RTSolution results are the same!'
    CALL Display_Message( PROGRAM_NAME, Message, INFORMATION )
  ELSE
    Message = 'global RTSolution results are different!'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    ! Write the current RTSolution results to file
    rts_GFS_File = TRIM(PROGRAM_NAME)//'_'//TRIM(Sensor_Id)//'.GFS.RTSolution.bin'
    Error_Status = CRTM_RTSolution_WriteFile( rts_GFS_File, RTSolution_GFS, Quiet=.TRUE. )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating temporary global RTSolution save file for failed comparison'
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    END IF
  END IF
  ! ============================================================================




  ! ============================================================================
  ! 9. **** CLEAN UP ****
  !
  ! 9a. Deallocate the structures
  ! -----------------------------
  CALL CRTM_Atmosphere_Destroy(Atm)
  CALL CRTM_Atmosphere_Destroy(Atm_NAM)
  CALL CRTM_Atmosphere_Destroy(Atm_GFS)

  ! 9b. Deallocate the arrays
  ! -------------------------
  DEALLOCATE(RTSolution, rts, &
             RTSolution_NAM, rts_NAM, &
             RTSolution_GFS, rts_GFS, &
             STAT=Allocate_Status)
  ! ============================================================================

  ! Signal the completion of the program. It is not a necessary step for running CRTM.

CONTAINS

  INCLUDE 'Load_Atm_Data.inc'
  INCLUDE 'Load_Sfc_Data.inc'
  INCLUDE 'Map_To_NCEP_Model_Coordinates.inc'
  INCLUDE 'SignalFile_Create.inc'

END PROGRAM test_VerticalCoordinates
