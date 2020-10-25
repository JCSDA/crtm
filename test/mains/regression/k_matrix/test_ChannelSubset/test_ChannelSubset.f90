!
! test_ChannelSubset
!
! Test program for the CRTM K-Matrix function while specifying channel
! subsets for sensors.
!
!

PROGRAM Example6_ChannelSubset

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
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'Example6_ChannelSubset'
  CHARACTER(*), PARAMETER :: COEFFICIENTS_PATH = './testinput/'
  CHARACTER(*), PARAMETER :: RESULTS_PATH = './results/k_matrix/'

  ! ============================================================================
  ! 0. **** SOME SET UP PARAMETERS FOR THIS EXAMPLE ****
  !
  ! Profile dimensions...
  INTEGER, PARAMETER :: N_PROFILES  = 2
  INTEGER, PARAMETER :: N_LAYERS    = 92
  INTEGER, PARAMETER :: N_ABSORBERS = 2
  INTEGER, PARAMETER :: N_CLOUDS    = 0
  INTEGER, PARAMETER :: N_AEROSOLS  = 0
  ! ...but only ONE Sensor
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
  ! Declarations for Jacobian comparisons
  INTEGER :: n_la, n_ma
  INTEGER :: n_ls, n_ms
  CHARACTER(256) :: atmk_File, sfck_File
  TYPE(CRTM_Atmosphere_type), ALLOCATABLE :: atm_k(:,:)
  TYPE(CRTM_Surface_type)   , ALLOCATABLE :: sfc_k(:,:)



  ! ============================================================================
  ! 1. **** DEFINE THE CRTM INTERFACE STRUCTURES ****
  !
  TYPE(CRTM_ChannelInfo_type)             :: ChannelInfo(N_SENSORS)
  TYPE(CRTM_Geometry_type)                :: Geometry(N_PROFILES)

  ! Define the FORWARD variables
  TYPE(CRTM_Atmosphere_type)              :: Atm(N_PROFILES)
  TYPE(CRTM_Surface_type)                 :: Sfc(N_PROFILES)
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: RTSolution(:,:)

  ! Define the K-MATRIX variables
  TYPE(CRTM_Atmosphere_type), ALLOCATABLE :: Atmosphere_K(:,:)
  TYPE(CRTM_Surface_type)   , ALLOCATABLE :: Surface_K(:,:)
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: RTSolution_K(:,:)
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
    'Test program for the CRTM K-Matrix function while specifying channel '//&
    'subsets for sensors.', &
    'CRTM Version: '//TRIM(Version) )


  ! Get sensor id from user
  ! -----------------------
  Sensor_Id = ADJUSTL(Sensor_Id)
  WRITE( *,'(//5x,"Running CRTM for ",a," sensor...")' ) TRIM(PROGRAM_NAME)//'_'//TRIM(Sensor_Id)



  ! ============================================================================
  ! 2. **** INITIALIZE THE CRTM ****
  !
  ! 2a. Initialise for the requested sensor
  ! ---------------------------------------
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
  n_Channels = CRTM_ChannelInfo_n_Channels(ChannelInfo(1))

  ! 2c. Construct a channel subset for processing
  ! ---------------------------------------------
  Error_Status = CRTM_ChannelInfo_Subset( ChannelInfo(1), &
                                          Channel_Subset = (/(l, l=1,n_Channels,100)/) )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error subsetting channels for '//TRIM(PROGRAM_NAME)//'_'//TRIM(Sensor_Id)
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF

  ! 2d. Determine the total number of channels
  !     the CRTM will now process
  ! ------------------------------------------
  n_Channels = CRTM_ChannelInfo_n_Channels(ChannelInfo(1))

  ! 2e. Output some info about the subsetting
  ! -----------------------------------------
  WRITE( *,'(/5x,"Processing the following ",i0," channels of ",a,":")' ) &
           n_Channels, TRIM(PROGRAM_NAME)//'_'//TRIM(Sensor_Id)
  WRITE( *,'(1x,10i5)' ) CRTM_ChannelInfo_Channels(ChannelInfo(1))
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
            Atmosphere_K( n_Channels, N_PROFILES ), &
            Surface_K( n_Channels, N_PROFILES ), &
            RTSolution_K( n_Channels, N_PROFILES ), &
            STAT = Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    Message = 'Error allocating structure arrays'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF

  ! 3b. Allocate the STRUCTURES
  ! ---------------------------
  ! The input FORWARD structure
  CALL CRTM_Atmosphere_Create( Atm, N_LAYERS, N_ABSORBERS, N_CLOUDS, N_AEROSOLS )
  IF ( ANY(.NOT. CRTM_Atmosphere_Associated(Atm)) ) THEN
    Message = 'Error allocating CRTM Atmosphere structure'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF

  ! The output K-MATRIX structure
  CALL CRTM_Atmosphere_Create( Atmosphere_K, N_LAYERS, N_ABSORBERS, N_CLOUDS, N_AEROSOLS )
  IF ( ANY(.NOT. CRTM_Atmosphere_Associated(Atmosphere_K)) ) THEN
    Message = 'Error allocating CRTM Atmosphere_K structure'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF
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
  CALL CRTM_Surface_Zero( Surface_K )

  ! 5b. Inintialize the K-matrix INPUT so
  !     that all the results are dTb/dx
  ! -------------------------------------
  RTSolution_K%Brightness_Temperature = ONE
  ! ============================================================================




  ! ============================================================================
  ! 6. **** CALL THE CRTM FORWARD MODEL ****
  !
  Error_Status = CRTM_K_Matrix( Atm         , &
                                Sfc         , &
                                RTSolution_K, &
                                Geometry    , &
                                ChannelInfo , &
                                Atmosphere_K, &
                                Surface_K   , &
                                RTSolution    )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error in CRTM K_Matrix Model'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF
  ! ============================================================================




  ! ============================================================================
  ! 7. **** OUTPUT THE RESULTS TO SCREEN ****
  !
  ! (a) User should read the user guide or the source codes of the routines
  !     CRTM_RTSolution_Inspect in the file CRTM_RTSolution_Define.f90,
  !     CRTM_Surface_Inspect in the file CRTM_Surface_Inspect, and CRTM_Atmosphere_Inspect
  !     in the file CRTM_Atmosphere_Define.f90 to select the needed variables for outputs.
  ! (b) The Forward results are contained in the structure RTSolution
  ! (c) The Jacobians for the atmospheric variables are contained in the structure Atmosphere_K
  ! (d) The Jacobians for the Surface related variables are contained in the structure Surface_K
  DO m = 1, N_PROFILES
    WRITE( *,'(//7x,"Profile ",i0," output for ",a )') m, TRIM(PROGRAM_NAME)//'_'//TRIM(Sensor_Id)
    DO l = 1, n_Channels
      WRITE( *, '(/5x,"Channel ",i0," results")') RTSolution(l,m)%Sensor_Channel
      ! FWD output
      WRITE( *, '(/3x,"FORWARD OUTPUT")')
      CALL CRTM_RTSolution_Inspect(RTSolution(l,m))
      ! K-MATRIX output
      WRITE( *, '(/3x,"K-MATRIX OUTPUT")')
      CALL CRTM_Surface_Inspect(Surface_K(l,m))
      CALL CRTM_Atmosphere_Inspect(Atmosphere_K(l,m))
    END DO
  END DO
  ! ============================================================================




  ! ============================================================================
  ! 8. **** DESTROY THE CRTM ****
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
  ! 9. **** COMPARE Atmosphere_K and Surface_K RESULTS TO SAVED VALUES ****
  !
  !   Step 9 is not part of the example to show how to use CRTM.
  !   It is to check the user results against the results in the CRTM package.

  WRITE( *, '( /5x, "Comparing calculated results with saved ones..." )' )

  ! 9a. Create the output files if they do not exist
  ! ------------------------------------------------
  ! 9a.1 Atmosphere file
  ! ...Generate filename
  atmk_File = RESULTS_PATH//TRIM(PROGRAM_NAME)//'_'//TRIM(Sensor_Id)//'.Atmosphere.bin'
  ! ...Check if the file exists
  IF ( .NOT. File_Exists(atmk_File) ) THEN
    Message = 'Atmosphere_K save file does not exist. Creating...'
    CALL Display_Message( PROGRAM_NAME, Message, INFORMATION )
    ! ...File not found, so write Atmosphere_K structure to file
    Error_Status = CRTM_Atmosphere_WriteFile( atmk_file, Atmosphere_K, Quiet=.TRUE. )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating Atmosphere_K save file'
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
      STOP 1
    END IF
  END IF
  ! 9a.2 Surface file
  ! ...Generate filename
  sfck_File = RESULTS_PATH//TRIM(PROGRAM_NAME)//'_'//TRIM(Sensor_Id)//'.Surface.bin'
  ! ...Check if the file exists
  IF ( .NOT. File_Exists(sfck_File) ) THEN
    Message = 'Surface_K save file does not exist. Creating...'
    CALL Display_Message( PROGRAM_NAME, Message, INFORMATION )
    ! ...File not found, so write Surface_K structure to file
    Error_Status = CRTM_Surface_WriteFile( sfck_file, Surface_K, Quiet=.TRUE. )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating Surface_K save file'
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
      STOP 1
    END IF
  END IF

  ! 9b. Inquire the saved files
  ! ---------------------------
  ! 9b.1 Atmosphere file
  Error_Status = CRTM_Atmosphere_InquireFile( atmk_File, &
                                              n_Channels = n_la, &
                                              n_Profiles = n_ma )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error inquiring Atmosphere_K save file'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF
  ! 9b.2 Surface file
  Error_Status = CRTM_Surface_InquireFile( sfck_File, &
                                           n_Channels = n_ls, &
                                           n_Profiles = n_ms )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error inquiring Surface_K save file'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF

  ! 9c. Compare the dimensions
  ! --------------------------
  IF ( n_la /= n_Channels .OR. n_ma /= N_PROFILES .OR. &
       n_ls /= n_Channels .OR. n_ms /= N_PROFILES      ) THEN
    Message = 'Dimensions of saved data different from that calculated!'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF

  ! 9d. Read the saved data
  ! -----------------------
  ! 9d.1 Atmosphere file
  Error_Status = CRTM_Atmosphere_ReadFile( atmk_File, atm_k, Quiet=.TRUE. )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error reading Atmosphere_K save file'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF
  ! 9d.2 Surface file
  Error_Status = CRTM_Surface_ReadFile( sfck_File, sfc_k, Quiet=.TRUE. )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error reading Surface_K save file'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF

  ! 9e. Compare some Jacobians
  ! --------------------------
  ! 9e.1 Atmosphere
  IF ( ALL(CRTM_Atmosphere_Compare(Atmosphere_K, atm_k)) ) THEN
    Message = 'Atmosphere_K Jacobians are the same!'
    CALL Display_Message( PROGRAM_NAME, Message, INFORMATION )
  ELSE
    Message = 'Atmosphere_K Jacobians are different!'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    ! Write the current Atmosphere_K results to file
    atmk_File = TRIM(PROGRAM_NAME)//'_'//TRIM(Sensor_Id)//'.Atmosphere.bin'
    Error_Status = CRTM_Atmosphere_WriteFile( atmk_file, Atmosphere_K, Quiet=.TRUE. )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating temporary Atmosphere_K save file for failed comparison'
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    END IF
  END IF
  ! 9e.2 Surface
  IF ( ALL(CRTM_Surface_Compare(Surface_K, sfc_k, n_SigFig=5)) ) THEN
    Message = 'Surface_K Jacobians are the same!'
    CALL Display_Message( PROGRAM_NAME, Message, INFORMATION )
  ELSE
    Message = 'Surface_K Jacobians are different!'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    ! Write the current Surface_K results to file
    sfck_File = TRIM(PROGRAM_NAME)//'_'//TRIM(Sensor_Id)//'.Surface.bin'
    Error_Status = CRTM_Surface_WriteFile( sfck_file, Surface_K, Quiet=.TRUE. )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating temporary Surface_K save file for failed comparison'
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    END IF
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
  CALL CRTM_Atmosphere_Destroy(atm_K)
  CALL CRTM_Atmosphere_Destroy(Atmosphere_K)
  CALL CRTM_Atmosphere_Destroy(Atm)

  ! 10b. Deallocate the arrays
  ! --------------------------
  DEALLOCATE(RTSolution, RTSolution_K, &
             Surface_K, Atmosphere_K, &
             sfc_k, atm_k, &
             STAT = Allocate_Status)
  ! ============================================================================

  ! Signal the completion of the program. It is not a necessary step for running CRTM.

CONTAINS

  INCLUDE 'Load_Atm_Data.inc'
  INCLUDE 'Load_Sfc_Data.inc'
  INCLUDE 'SignalFile_Create.inc'

END PROGRAM Example6_ChannelSubset
