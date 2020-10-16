!
! test_Simple
!
! Test program for the CRTM Adjoint function including clouds and aerosols.
!
!

PROGRAM test_Simple

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
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'test_Simple'
  CHARACTER(*), PARAMETER :: COEFFICIENTS_PATH = './testinput/'
  CHARACTER(*), PARAMETER :: RESULTS_PATH = './results/adjoint/'


  ! ============================================================================
  ! 0. **** SOME SET UP PARAMETERS FOR THIS EXAMPLE ****
  !
  ! Profile dimensions...
  INTEGER, PARAMETER :: N_PROFILES  = 2
  INTEGER, PARAMETER :: N_LAYERS    = 92
  INTEGER, PARAMETER :: N_ABSORBERS = 2
  INTEGER, PARAMETER :: N_CLOUDS    = 1
  INTEGER, PARAMETER :: N_AEROSOLS  = 1
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
  ! Declarations for Adjoint comparisons
  INTEGER :: n_la, n_ma
  INTEGER :: n_ls, n_ms
  CHARACTER(256) :: atmad_file, sfcad_file
  TYPE(CRTM_Atmosphere_type), ALLOCATABLE :: atm_AD(:)
  TYPE(CRTM_Surface_type)   , ALLOCATABLE :: sfc_AD(:)



  ! ============================================================================
  ! 1. **** DEFINE THE CRTM INTERFACE STRUCTURES ****
  !
  TYPE(CRTM_ChannelInfo_type)             :: ChannelInfo(N_SENSORS)
  TYPE(CRTM_Geometry_type)                :: Geometry(N_PROFILES)

  ! Define the FORWARD variables
  TYPE(CRTM_Atmosphere_type)              :: Atm(N_PROFILES)
  TYPE(CRTM_Surface_type)                 :: Sfc(N_PROFILES)
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: RTSolution(:,:)

  ! Define the ADJOINT variables
  TYPE(CRTM_Atmosphere_type)              :: Atmosphere_AD(N_PROFILES)
  TYPE(CRTM_Surface_type)                 :: Surface_AD(N_PROFILES)
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: RTSolution_AD(:,:)
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
    'Test program for the CRTM Adjoint function including clouds and aerosols.', &
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
  ALLOCATE( RTSolution( n_Channels, N_PROFILES ), &
            RTSolution_AD( n_Channels, N_PROFILES ), &
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

  ! The output ADJOINT structure
  CALL CRTM_Atmosphere_Create( Atmosphere_AD, N_LAYERS, N_ABSORBERS, N_CLOUDS, N_AEROSOLS )
  IF ( ANY(.NOT. CRTM_Atmosphere_Associated(Atmosphere_AD)) ) THEN
    Message = 'Error allocating CRTM Atmosphere_AD structure'
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


  ! 4b. GeometryInfo input
  ! ----------------------
  ! All profiles are given the same value
  !  The Sensor_Scan_Angle is optional.
  CALL CRTM_Geometry_SetValue( Geometry, &
                               Sensor_Zenith_Angle = ZENITH_ANGLE, &
                               Sensor_Scan_Angle   = SCAN_ANGLE )
  ! ============================================================================




  ! ============================================================================
  ! 5. **** INITIALIZE THE ADJOINT ARGUMENTS ****
  !
  ! 5a. Zero the Adjoint OUTPUT structures
  ! ---------------------------------------
  CALL CRTM_Atmosphere_Zero( Atmosphere_AD )
  CALL CRTM_Surface_Zero( Surface_AD )

  ! 5b. Inintialize the Adjoint INPUT so
  !     that the IR/MW results are dTb/dx
  !     and the visible results are dR/dx
  ! -------------------------------------
  DO l = 1, n_Channels
    IF ( ChannelInfo(1)%Sensor_Type == INFRARED_SENSOR .OR. &
         ChannelInfo(1)%Sensor_Type == MICROWAVE_SENSOR ) THEN
      RTSolution_AD(l,:)%Radiance               = ZERO
      RTSolution_AD(l,:)%Brightness_Temperature = ONE
    ELSE
      RTSolution_AD(l,:)%Radiance               = ONE
      RTSolution_AD(l,:)%Brightness_Temperature = ZERO
    END IF
  END DO
  ! ============================================================================




  ! ============================================================================
  ! 6. **** CALL THE CRTM ADJOINT MODEL ****
  !
  Error_Status = CRTM_Adjoint( Atm          , &
                               Sfc          , &
                               RTSolution_AD, &
                               Geometry     , &
                               ChannelInfo  , &
                               Atmosphere_AD, &
                               Surface_AD   , &
                               RTSolution   )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error in CRTM Adjoint Model'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF
  ! ============================================================================




  ! ============================================================================
  ! 7. **** OUTPUT THE RESULTS TO SCREEN ****
  !
  DO m = 1, N_PROFILES
    WRITE( *,'(//7x,"Profile ",i0," output for ",a )') m, TRIM(Sensor_Id)
    DO l = 1, n_Channels
      WRITE( *, '(/5x,"Channel ",i0," results")') RTSolution(l,m)%Sensor_Channel
      ! FWD output
      WRITE( *, '(/3x,"FORWARD OUTPUT")')
      CALL CRTM_RTSolution_Inspect(RTSolution(l,m))
    END DO
    ! ADJOINT output
    WRITE( *, '(/3x,"ADJOINT OUTPUT")')
    CALL CRTM_Surface_Inspect(Surface_AD(m))
    CALL CRTM_Atmosphere_Inspect(Atmosphere_AD(m))
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
  ! 9. **** COMPARE Atmosphere_AD and Surface_AD RESULTS TO SAVED VALUES ****
  !
  WRITE( *, '( /5x, "Comparing calculated results with saved ones..." )' )

  ! 9a. Create the output files if they do not exist
  ! ------------------------------------------------
  ! 9a.1 Atmosphere file
  ! ...Generate filename
  atmad_file = RESULTS_PATH//TRIM(PROGRAM_NAME)//'_'//TRIM(Sensor_Id)//'.Atmosphere.bin'
  ! ...Check if the file exists
  IF ( .NOT. File_Exists(atmad_file) ) THEN
    Message = 'Atmosphere_AD save file does not exist. Creating...'
    CALL Display_Message( PROGRAM_NAME, Message, INFORMATION )
    ! ...File not found, so write Atmosphere_AD structure to file
    Error_Status = CRTM_Atmosphere_WriteFile( atmad_file, Atmosphere_AD, Quiet=.TRUE. )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating Atmosphere_AD save file'
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
      STOP 1
    END IF
  END IF
  ! 9a.2 Surface file
  ! ...Generate filename
  sfcad_file = RESULTS_PATH//TRIM(PROGRAM_NAME)//'_'//TRIM(Sensor_Id)//'.Surface.bin'
  ! ...Check if the file exists
  IF ( .NOT. File_Exists(sfcad_file) ) THEN
    Message = 'Surface_AD save file does not exist. Creating...'
    CALL Display_Message( PROGRAM_NAME, Message, INFORMATION )
    ! ...File not found, so write Surface_AD structure to file
    Error_Status = CRTM_Surface_WriteFile( sfcad_file, Surface_AD, Quiet=.TRUE. )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating Surface_AD save file'
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
      STOP 1
    END IF
  END IF

  ! 9b. Inquire the saved files
  ! ---------------------------
  ! 9b.1 Atmosphere file
  Error_Status = CRTM_Atmosphere_InquireFile( atmad_file, &
                                              n_Channels = n_la, &
                                              n_Profiles = n_ma )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error inquiring Atmosphere_AD save file'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF
  ! 9b.2 Surface file
  Error_Status = CRTM_Surface_InquireFile( sfcad_file, &
                                           n_Channels = n_ls, &
                                           n_Profiles = n_ms )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error inquiring Surface_AD save file'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF

  ! 9c. Compare the dimensions
  ! --------------------------
  IF ( n_la /= 0 .OR. n_ma /= N_PROFILES .OR. &
       n_ls /= 0 .OR. n_ms /= N_PROFILES      ) THEN
    Message = 'Dimensions of saved data different from that calculated!'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF

  ! 9d. Read the saved data
  ! -----------------------
  ! 9d.1 Atmosphere file
  Error_Status = CRTM_Atmosphere_ReadFile( atmad_file, atm_AD, Quiet=.TRUE. )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error reading Atmosphere_AD save file'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF
  ! 9d.2 Surface file
  Error_Status = CRTM_Surface_ReadFile( sfcad_file, sfc_AD, Quiet=.TRUE. )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error reading Surface_AD save file'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF

  ! 9e. Compare the adjoints
  ! ------------------------
  ! 9e.1 Atmosphere
  IF ( ALL(CRTM_Atmosphere_Compare(Atmosphere_AD, atm_AD, n_SigFig=3)) ) THEN
    Message = 'Atmosphere_AD Adjoints are the same!'
    CALL Display_Message( PROGRAM_NAME, Message, INFORMATION )
  ELSE
    Message = 'Atmosphere_AD Adjoints are different!'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    ! Write the current Atmosphere_AD results to file
    atmad_file = TRIM(Sensor_Id)//'.Atmosphere.bin'
    Error_Status = CRTM_Atmosphere_WriteFile( atmad_file, Atmosphere_AD, Quiet=.TRUE. )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating temporary Atmosphere_AD save file for failed comparison'
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    END IF
  END IF
  ! 9e.2 Surface
  IF ( ALL(CRTM_Surface_Compare(Surface_AD, sfc_AD, n_SigFig=5)) ) THEN
    Message = 'Surface_AD Adjoints are the same!'
    CALL Display_Message( PROGRAM_NAME, Message, INFORMATION )
  ELSE
    Message = 'Surface_AD Adjoints are different!'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    ! Write the current Surface_AD results to file
    sfcad_file = TRIM(Sensor_Id)//'.Surface.bin'
    Error_Status = CRTM_Surface_WriteFile( sfcad_file, Surface_AD, Quiet=.TRUE. )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating temporary Surface_AD save file for failed comparison'
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    END IF
  END IF
  ! ============================================================================




  ! ============================================================================
  ! 10. **** CLEAN UP ****
  !
  ! 10a. Deallocate the structures
  ! ------------------------------
  CALL CRTM_Atmosphere_Destroy(atm_AD)
  CALL CRTM_Atmosphere_Destroy(Atmosphere_AD)
  CALL CRTM_Atmosphere_Destroy(Atm)

  ! 10b. Deallocate the arrays
  ! --------------------------
  DEALLOCATE(RTSolution, RTSolution_AD, &
             STAT = Allocate_Status)
  ! ============================================================================

  ! Signal the completion of the program. It is not a necessary step for running CRTM.

CONTAINS

  INCLUDE 'Load_Atm_Data.inc'
  INCLUDE 'Load_Sfc_Data.inc'
  INCLUDE 'SignalFile_Create.inc'

END PROGRAM test_Simple
