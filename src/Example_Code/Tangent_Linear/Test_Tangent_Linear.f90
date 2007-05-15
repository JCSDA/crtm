!
! Test_Tangent_Linear
!
! Program to test the CRTM Tangent-Linear model code and provide an exmaple of how
! to call the Tangent-Linear function.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 30-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Test_Tangent_Linear

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
              MAX_NSENSORS, TEST_SENSORID, TEST_ANGLE, TEST_DELTA, &
              Perform_Test, &
              Print_ChannelInfo, &
              Write_RTSolution_TestFile, Read_RTSolution_TestFile
  USE Timing_Utility, &          ! For timing runs
        ONLY: Timing_type, &
              Begin_Timing, End_Timing, Display_Timing
  USE Unit_Test, &               ! For test assertions, reporting
        ONLY: Init_AllTests, Init_Test, Assert_Equal, &
              Report_AllTests, n_Tests_Failed
  USE SignalFile_Utility, &      ! For signal file completion
        ONLY: Create_SignalFile
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'Test_Tangent_Linear'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id$'
  CHARACTER(*), PARAMETER :: MESSAGE_LOG = '../Test.report'


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  INTEGER :: i, m, n, nc, na, nChannels
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  CHARACTER(256) :: Exp_ID, Exp_Description
  INTEGER, DIMENSION(USED_NPROFILES) :: nClouds
  INTEGER, DIMENSION(USED_NPROFILES) :: nAerosols
  TYPE(CRTM_ChannelInfo_type) , DIMENSION(MAX_NSENSORS)     :: ChannelInfo
  TYPE(CRTM_Atmosphere_type)  , DIMENSION(USED_NPROFILES)   :: Atmosphere, Atmosphere_TL
  TYPE(CRTM_Surface_type)     , DIMENSION(USED_NPROFILES)   :: Surface,    Surface_TL
  TYPE(CRTM_GeometryInfo_type), DIMENSION(USED_NPROFILES)   :: GeometryInfo
  TYPE(CRTM_RTSolution_type)  , DIMENSION(:,:), ALLOCATABLE :: RTSolution, RTSolution_TL, Baseline
  TYPE(CRTM_Options_type)     , DIMENSION(USED_NPROFILES)   :: Options
  TYPE(Timing_type) :: Timing


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


  ! Allocate output arrays
  ! ----------------------
  nChannels = SUM(ChannelInfo%n_Channels)
  ALLOCATE( RTSolution(    nChannels, USED_NPROFILES ), &
            RTSolution_TL( nChannels, USED_NPROFILES ), &
            Baseline(      nChannels, USED_NPROFILES ), &
            STAT = Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error allocating RTSolution structure array', & 
                            Error_Status)  
   STOP
  END IF

  ! Allocate the RTSolution structures
  ! ----------------------------------
  Error_Status = CRTM_Allocate_RTSolution( Atmosphere(1)%n_Layers, RTSolution )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating RTSolution', & 
                           Error_Status)  
    STOP
  END IF
  Error_Status = CRTM_Allocate_RTSolution( Atmosphere(1)%n_Layers, RTSolution_TL )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating RTSolution_TL', & 
                           Error_Status)  
    STOP
  END IF


  ! Set the tangent-linear values
  ! -----------------------------
  ! Copy and then zero the tangent-linear atmosphere structure
  Error_Status = CRTM_Assign_Atmosphere( Atmosphere, Atmosphere_TL )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error copying Atmosphere structure array.', &
                          Error_Status )
    STOP
  END IF
  CALL CRTM_Zero_Atmosphere( Atmosphere_TL )
  ! Copy and then zero the tangent-linear surface structure
  Error_Status = CRTM_Assign_Surface( Surface, Surface_TL )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error copying Surface structure array.', &
                          Error_Status )
    STOP
  END IF
  CALL CRTM_Zero_Surface( Surface_TL )
  


  ! Allocate the Options input
  ! --------------------------
  Error_Status = CRTM_Allocate_Options( nChannels, Options )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating Options structure array', & 
                           Error_Status)  
    STOP
  END IF


  ! Assign some values
  ! ------------------
  GeometryInfo%Sensor_Zenith_Angle = TEST_ANGLE
  DO m = 1, USED_NPROFILES
    ! The TL perturbations
    Atmosphere_TL(m)%Pressure    = Atmosphere(m)%Pressure    * TEST_DELTA
    Atmosphere_TL(m)%Temperature = Atmosphere(m)%Temperature * TEST_DELTA
    Atmosphere_TL(m)%Absorber    = Atmosphere(m)%Absorber    * TEST_DELTA
    DO nc = 1, Atmosphere(m)%n_Clouds
      Atmosphere_TL(m)%Cloud(nc)%Water_Content    = Atmosphere(m)%Cloud(nc)%Water_Content    * TEST_DELTA
      Atmosphere_TL(m)%Cloud(nc)%Effective_Radius = Atmosphere(m)%Cloud(nc)%Effective_Radius * TEST_DELTA
    END DO
    DO na = 1, Atmosphere(m)%n_Aerosols
      Atmosphere_TL(m)%Aerosol(na)%Concentration    = Atmosphere(m)%Aerosol(na)%Concentration    * TEST_DELTA
      Atmosphere_TL(m)%Aerosol(na)%Effective_Radius = Atmosphere(m)%Aerosol(na)%Effective_Radius * TEST_DELTA
    END DO
    Surface_TL(m)%Land_Temperature  = Surface(m)%Land_Temperature  * TEST_DELTA
    Surface_TL(m)%Water_Temperature = Surface(m)%Water_Temperature * TEST_DELTA
    Surface_TL(m)%Snow_Temperature  = Surface(m)%Snow_Temperature  * TEST_DELTA
    Surface_TL(m)%Ice_Temperature   = Surface(m)%Ice_Temperature   * TEST_DELTA
    ! The optional emissivity
    Options(m)%Emissivity = 0.8_fp
  END DO


  ! Print some initialisation info
  ! ------------------------------
  DO n=1, MAX_NSENSORS
    CALL Print_ChannelInfo(ChannelInfo(n))
  END DO


  ! Initialise the test counters
  ! ----------------------------
  CALL Init_AllTests()

  
  ! Call the Tangent-Linear model
  ! -----------------------------
  DO i = 0, MAX_NTESTS

    Exp_ID = ''
    Exp_Description = ''

    
    ! Turn experiments on and off
    ! ---------------------------
    ! Turn emissivity option on and off
    IF ( Perform_Test(i,EMISSIVITY_TEST) ) THEN
      Options%Emissivity_Switch = 1
      Exp_ID = TRIM(Exp_ID)//'.eON'
      Exp_Description = TRIM(Exp_Description)//' Emissivity ON'
    ELSE
      Options%Emissivity_Switch = 0
      Exp_ID = TRIM(Exp_ID)//'.eOFF'
      Exp_Description = TRIM(Exp_Description)//' Emissivity OFF'
    END IF
    
    ! Turn clouds on and off
    IF ( Perform_Test(i,CLOUDS_TEST) ) THEN
      Atmosphere%n_Clouds = nClouds
      Exp_ID = TRIM(Exp_ID)//'.cON'
      Exp_Description = TRIM(Exp_Description)//' Clouds ON'
    ELSE
      Atmosphere%n_Clouds = 0
      Exp_ID = TRIM(Exp_ID)//'.cOFF'
      Exp_Description = TRIM(Exp_Description)//' Clouds OFF'
    END IF
    
    ! Turn aerosols on and off
    IF ( Perform_Test(i,AEROSOLS_TEST) ) THEN
      Atmosphere%n_Aerosols = nAerosols
      Exp_ID = TRIM(Exp_ID)//'.aON'
      Exp_Description = TRIM(Exp_Description)//' Aerosols ON'
    ELSE
      Atmosphere%n_Aerosols = 0
      Exp_ID = TRIM(Exp_ID)//'.aOFF'
      Exp_Description = TRIM(Exp_Description)//' Aerosols OFF'
    END IF

    WRITE(*,'(/5x,"Experiment: ",a)') TRIM(ADJUSTL(Exp_Description))


    ! Initialise the test
    ! -------------------
    CALL Init_Test( TRIM(ADJUSTL(Exp_Description)), &
                    Caller     =PROGRAM_NAME, &
                    Message_Log=MESSAGE_LOG)


    ! Call the CRTM tangent-linear model
    ! ----------------------------------
    CALL Begin_Timing( Timing )
    Error_Status = CRTM_Tangent_Linear( Atmosphere     , &
                                        Surface        , &
                                        Atmosphere_TL  , &
                                        Surface_TL     , &
                                        GeometryInfo   , &
                                        ChannelInfo    , &
                                        RTSolution     , &
                                        RTSolution_TL  , &
                                        Options=Options  )
    CALL End_Timing( Timing )
    IF ( Error_Status /= SUCCESS ) THEN 
       CALL Display_Message( PROGRAM_NAME, &
                             'Error in CRTM Tangent-Linear Model', &
                              Error_Status)  
     STOP
    END IF
    CALL Display_Timing( Timing, &
                         Caller     =PROGRAM_NAME, &
                         Message_Log=MESSAGE_LOG )


    ! Test TL results for equality
    ! ----------------------------
    ! Output some results
    CALL Write_RTSolution_TestFile( Exp_ID, ChannelInfo, RTSolution_TL )
    
    ! Read baseline results
    CALL Read_RTSolution_TestFile( Exp_ID, ChannelInfo, Baseline )
    
    ! Compare them
    Error_Status = CRTM_Equal_RTSolution( Baseline, RTSolution_TL )
    CALL Assert_Equal(Error_Status,SUCCESS)

  END DO


  ! Report all the test results
  ! ---------------------------
  CALL Report_AllTests( Caller     =PROGRAM_NAME, &
                        Message_Log=MESSAGE_LOG )


  ! Create successful completion signal
  ! file if there are no failed tests
  ! -----------------------------------
  IF ( n_Tests_Failed() == 0 ) THEN
    Error_Status = Create_SignalFile( PROGRAM_NAME, &
                                      Message_Log=MESSAGE_LOG ) 
    IF ( Error_Status /= SUCCESS ) THEN 
      CALL Display_Message( PROGRAM_NAME, &
                            'Error creating '//PROGRAM_NAME//' signal file', &
                             Error_Status, &
                             Message_Log=MESSAGE_LOG )
    END IF
  END IF
  
  
  ! Destroy the CRTM
  ! ----------------
  WRITE( *, '( /5x, "Destroying the CRTM..." )' )
  Error_Status = CRTM_Destroy( ChannelInfo )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying CRTM', & 
                           Error_Status, &
                           Message_Log=MESSAGE_LOG )
  END IF


  ! --------
  ! Clean up
  ! --------
  Error_Status = CRTM_Destroy_Options(Options)
  Error_Status = CRTM_Destroy_RTSolution(RTSolution_TL)
  Error_Status = CRTM_Destroy_RTSolution(RTSolution)
  Error_Status = CRTM_Destroy_RTSolution(Baseline)
  DEALLOCATE(RTSolution_TL, RTSolution, Baseline, STAT=Allocate_Status)
  Error_Status = CRTM_Destroy_Surface(Surface)
  Error_Status = CRTM_Destroy_Surface(Surface_TL)
  Error_Status = CRTM_Destroy_Atmosphere(Atmosphere)
  Error_Status = CRTM_Destroy_Atmosphere(Atmosphere_TL)

END PROGRAM Test_Tangent_Linear
