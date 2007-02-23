!
! Test_Forward
!
! Program to test the CRTM Forward model code and provide an exmaple of how
! to call the Forward function.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 30-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Test_Forward

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE CRTM_Module                ! The main CRTM module
  USE CRTM_Atmosphere_Binary_IO  ! Just for reading test datafiles
  USE CRTM_Surface_Binary_IO     ! Just for reading test datafiles
  USE CRTM_Test_Utility, &
        ONLY: ATMDATA_FILENAME, SFCDATA_FILENAME, USED_NPROFILES, &
              EMISSIVITY_TEST, CLOUDS_TEST, AEROSOLS_TEST, MAX_NTESTS, &
              MAX_NSENSORS, TEST_SENSORID, TEST_ANGLE, &
              Perform_Test, &
              Print_ChannelInfo, &
              Dump_FWD_Model_Results
  USE Timing_Utility             ! For timing runs
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'Test_Forward'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: Test_Forward.f90,v 1.15 2006/09/22 20:07:56 wd20pd Exp $'


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  INTEGER :: i, m, n, iOptions, l1, l2
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  CHARACTER(256) :: Experiment
  INTEGER, DIMENSION(USED_NPROFILES) :: nClouds
  INTEGER, DIMENSION(USED_NPROFILES) :: nAerosols
  TYPE(CRTM_ChannelInfo_type),  DIMENSION(MAX_NSENSORS)     :: ChannelInfo
  TYPE(CRTM_Atmosphere_type),   DIMENSION(USED_NPROFILES)   :: Atmosphere
  TYPE(CRTM_Surface_type),      DIMENSION(USED_NPROFILES)   :: Surface
  TYPE(CRTM_GeometryInfo_type), DIMENSION(USED_NPROFILES)   :: GeometryInfo
  TYPE(CRTM_RTSolution_type),   DIMENSION(:,:), ALLOCATABLE :: RTSolution
  TYPE(CRTM_Options_type),      DIMENSION(USED_NPROFILES)   :: Options
  TYPE(Timing_type) :: Timing



  ! ----------------------------------------------------
  ! Read the atmosphere and surface structure data files
  ! ----------------------------------------------------
  WRITE( *, '( /5x, "Reading ECMWF Atmosphere structure file..." )' )
  Error_Status = CRTM_Read_Atmosphere_Binary( ATMDATA_FILENAME, &
                                              Atmosphere )
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error reading Atmosphere structure file '//&
                           ATMDATA_FILENAME, & 
                           Error_Status )
   STOP
  END IF

  WRITE( *, '( /5x, "Reading Surface structure file..." )' )
  Error_Status = CRTM_Read_Surface_Binary( SFCDATA_FILENAME, &
                                           Surface )
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error reading Surface structure file '//&
                           SFCDATA_FILENAME, & 
                           Error_Status )
   STOP
  END IF

  ! Save the number of clouds and
  ! aerosols in each profile
  nClouds   = Atmosphere%n_Clouds
  nAerosols = Atmosphere%n_Aerosols


  ! -------------------
  ! Initialise the CRTM
  ! -------------------
  WRITE( *, '( /5x, "Initializing the CRTM..." )' )
  Error_Status = CRTM_Init( ChannelInfo, &
                            SensorId=TEST_SENSORID )
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error initializing CRTM', & 
                            Error_Status)  
   STOP
  END IF


  ! ----------------------
  ! Allocate output arrays
  ! ----------------------
  ALLOCATE( RTSolution( SUM(ChannelInfo%n_Channels), USED_NPROFILES ), &
            STAT = Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error allocating RTSolution structure array', & 
                            Error_Status)  
   STOP
  END IF


  ! --------------------------
  ! Allocate the Options input
  ! --------------------------
  Error_Status = CRTM_Allocate_Options( SUM(ChannelInfo%n_Channels), Options )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating Options structure array', & 
                           Error_Status)  
    STOP
  END IF


  ! ------------------
  ! Assign some values
  ! ------------------
  GeometryInfo%Sensor_Zenith_Angle = TEST_ANGLE
  DO m = 1, USED_NPROFILES
    Options(m)%Emissivity = 0.8_fp
  END DO


  ! ------------------------------
  ! Print some initialisation info
  ! ------------------------------
  DO n=1, MAX_NSENSORS
    CALL Print_ChannelInfo(ChannelInfo(n))
  END DO
  

  ! ----------------------
  ! Call the Forward model
  ! ----------------------
  DO i = 0, MAX_NTESTS

    Experiment = ''
    
    ! Turn emissivity option on and off
    IF ( Perform_Test(i,EMISSIVITY_TEST) ) THEN
      Options%Emissivity_Switch = 1
      Experiment = TRIM(Experiment)//' Emissivity option ON'
    ELSE
      Options%Emissivity_Switch = 0
      Experiment = TRIM(Experiment)//' Emissivity option OFF'
    END IF
    
    ! Turn clouds on and off
    IF ( Perform_Test(i,CLOUDS_TEST) ) THEN
      Atmosphere%n_Clouds = nClouds
      Experiment = TRIM(Experiment)//' Clouds ON'
    ELSE
      Atmosphere%n_Clouds = 0
      Experiment = TRIM(Experiment)//' Clouds OFF'
    END IF
    
    ! Turn aerosols on and off
    IF ( Perform_Test(i,AEROSOLS_TEST) ) THEN
      Atmosphere%n_Aerosols = nAerosols
      Experiment = TRIM(Experiment)//' Aerosols ON'
    ELSE
      Atmosphere%n_Aerosols = 0
      Experiment = TRIM(Experiment)//' Aerosols OFF'
    END IF

    WRITE(*,'(/5x,a)') TRIM(Experiment)

    ! Call the CRTM
    CALL Begin_Timing( Timing )
    Error_Status = CRTM_Forward( Atmosphere     , &
                                 Surface        , &
                                 GeometryInfo   , &
                                 ChannelInfo    , &
                                 RTSolution     , &
                                 Options=Options  )
    CALL End_Timing( Timing )
    IF ( Error_Status /= SUCCESS ) THEN 
       CALL Display_Message( PROGRAM_NAME, &
                             'Error in CRTM Forward Model', &
                              Error_Status)  
     STOP
    END IF
    CALL Display_Timing( Timing )

    ! Output some results
    CALL Dump_FWD_Model_Results(i, Experiment, ChannelInfo, &
                                Atmosphere, Surface, RTSolution)
  END DO


  ! ----------------
  ! Destroy the CRTM
  ! ----------------
  WRITE( *, '( /5x, "Destroying the CRTM..." )' )
  Error_Status = CRTM_Destroy( ChannelInfo )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying CRTM', & 
                           Error_Status )
    STOP
  END IF


  ! --------
  ! Clean up
  ! --------
  Error_Status = CRTM_Destroy_Options(Options)
  DEALLOCATE(RTSolution, STAT = Allocate_Status)
  Error_Status = CRTM_Destroy_Surface(Surface)
  Error_Status = CRTM_Destroy_Atmosphere(Atmosphere)

END PROGRAM Test_Forward
