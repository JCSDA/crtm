!
! test_AOD
!
! Test program for the CRTM Aerosol Optical Depth (AOD) Forward function.
!
!

PROGRAM test_AOD

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
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'test_AOD'
  CHARACTER(*), PARAMETER :: COEFFICIENTS_PATH = './testinput/'
  CHARACTER(*), PARAMETER :: RESULTS_PATH = './results/forward/'

  ! ============================================================================
  ! 0. **** SOME SET UP PARAMETERS FOR THIS TEST ****
  !
  ! Profile dimensions...
  INTEGER, PARAMETER :: N_PROFILES  = 2
  INTEGER, PARAMETER :: N_LAYERS    = 92
  INTEGER, PARAMETER :: N_ABSORBERS = 2
  INTEGER, PARAMETER :: N_CLOUDS    = 0
  INTEGER, PARAMETER :: N_AEROSOLS  = 3
  ! ...but only ONE Sensor at a time
  INTEGER, PARAMETER :: N_SENSORS = 1
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
  TYPE(CRTM_Atmosphere_type)              :: Atm(N_PROFILES)
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: RTSolution(:,:)
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
    'Test program for the CRTM Aerosol Optical Depth (AOD) Forward function.', &
    'CRTM Version: '//TRIM(Version) )


  ! Get sensor id from user
  ! -----------------------
  Sensor_Id = ADJUSTL(Sensor_Id)
  WRITE( *,'(//5x,"Running CRTM for ",a," sensor...")' ) TRIM(Sensor_Id)



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
  ! The input structure
  CALL CRTM_Atmosphere_Create( Atm, N_LAYERS, N_ABSORBERS, N_CLOUDS, N_AEROSOLS )
  IF ( ANY(.NOT. CRTM_Atmosphere_Associated(Atm)) ) THEN
    Message = 'Error allocating CRTM Atmosphere structures'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF
  ! The output structure
  CALL CRTM_RTSolution_Create( RTSolution, N_LAYERS )
  IF ( ANY(.NOT. CRTM_RTSolution_Associated(RTSolution)) ) THEN
    Message = 'Error allocating CRTM RTSolution structures'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    call exit(1)
    STOP 1
  END IF
  ! ============================================================================




  ! ============================================================================
  ! 4. **** ASSIGN INPUT DATA ****
  !
  CALL Load_Atm_Data()
  ! ============================================================================




  ! ============================================================================
  ! 5. **** CALL THE CRTM AOD MODEL ****
  !
  Error_Status = CRTM_AOD( Atm        , &
                           ChannelInfo, &
                           RTSolution   )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error in CRTM AOD Model'
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
  ! ...Generate a filename
  rts_File = RESULTS_PATH//TRIM(PROGRAM_NAME)//'_'//TRIM(Sensor_Id)//'.RTSolution.bin'
  ! ...Check if the file exists
  IF ( .NOT. File_Exists(rts_File) ) THEN
    Message = 'RTSolution save file does not exist. Creating...'
    CALL Display_Message( PROGRAM_NAME, Message, INFORMATION )
    ! ...File not found, so write RTSolution structure to file
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
  CALL CRTM_RTSolution_Destroy(RTSolution)
  CALL CRTM_Atmosphere_Destroy(Atm)

  ! 9b. Deallocate the arrays
  ! -------------------------
  DEALLOCATE(RTSolution, rts, STAT=Allocate_Status)
  ! ============================================================================

CONTAINS

  INCLUDE 'Load_Atm_Data.inc'
  INCLUDE 'SignalFile_Create.inc'

END PROGRAM test_AOD
