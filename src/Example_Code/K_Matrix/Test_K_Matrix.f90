!
! Test_K_Matrix
!
! Program to test the CRTM K-Matrix code and provide an exmaple of how
! to call the K-matrix function.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 31-Jan-2005
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Test_K_Matrix

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE CRTM_Module                ! The main CRTM module
  USE CRTM_Atmosphere_Binary_IO  ! Just for reading test datafiles
  USE CRTM_Surface_Binary_IO     ! Just for reading test datafiles
  USE CRTM_Test_Utility, &       ! For standard test settings
        ONLY: ATMDATA_FILENAME, SFCDATA_FILENAME, USED_NPROFILES, &
              EMISSIVITY_TEST, CLOUDS_TEST, AEROSOLS_TEST, MAX_NTESTS, &
              MAX_NSENSORS, TEST_SENSORID, TEST_ANGLE, &
              Perform_Test, &
              Print_ChannelInfo, &
              Write_AtmSfc_TestFile, Read_AtmSfc_TestFile
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
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'Test_K_Matrix'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id$'
  CHARACTER(*), PARAMETER :: MESSAGE_LOG = '../Test.report'


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  INTEGER :: i, l, m, n, nChannels
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: Atm_Status, Sfc_Status
  CHARACTER(256) :: Exp_ID, Exp_Description
  INTEGER, DIMENSION(USED_NPROFILES) :: nClouds
  INTEGER, DIMENSION(USED_NPROFILES) :: nAerosols
  TYPE(CRTM_ChannelInfo_type) , DIMENSION(MAX_NSENSORS)     :: ChannelInfo
  TYPE(CRTM_Atmosphere_type)  , DIMENSION(USED_NPROFILES)   :: Atm
  TYPE(CRTM_Surface_type)     , DIMENSION(USED_NPROFILES)   :: Sfc
  TYPE(CRTM_GeometryInfo_type), DIMENSION(USED_NPROFILES)   :: GeometryInfo
  TYPE(CRTM_Atmosphere_type)  , DIMENSION(:,:), ALLOCATABLE :: Atm_K, Atm_Baseline
  TYPE(CRTM_Surface_type)     , DIMENSION(:,:), ALLOCATABLE :: Sfc_K, Sfc_Baseline
  TYPE(CRTM_RTSolution_type)  , DIMENSION(:,:), ALLOCATABLE :: RTSolution, RTSolution_K
  TYPE(CRTM_Options_type)     , DIMENSION(USED_NPROFILES)   :: Options
  TYPE(Timing_type) :: Timing


  ! Read the atmosphere and surface structure data files
  ! ----------------------------------------------------
  WRITE( *, '( /5x, "Reading ECMWF Atmosphere structure file..." )' )
  Error_Status = CRTM_Read_Atmosphere_Binary( ATMDATA_FILENAME, &
                                              Atm )
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error reading Atmosphere structure file '//&
                           ATMDATA_FILENAME, & 
                           Error_Status )
   STOP
  END IF

  WRITE( *, '( /5x, "Reading Surface structure file..." )' )
  Error_Status = CRTM_Read_Surface_Binary( SFCDATA_FILENAME, &
                                           Sfc )
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error reading Surface structure file '//&
                           SFCDATA_FILENAME, & 
                           Error_Status )
   STOP
  END IF

  ! Save the number of clouds and
  ! aerosols in each profile
  nClouds   = Atm%n_Clouds
  nAerosols = Atm%n_Aerosols


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
  ALLOCATE( Atm_K( nChannels, USED_NPROFILES ), &
            Sfc_K( nChannels, USED_NPROFILES ), &
            Atm_Baseline( nChannels, USED_NPROFILES ), &
            Sfc_Baseline( nChannels, USED_NPROFILES ), &
            RTSolution(   nChannels, USED_NPROFILES ), &
            RTSolution_K( nChannels, USED_NPROFILES ), &
            STAT = Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating structure arrays', & 
                           Error_Status)  
    STOP
  END IF


  ! Allocate the RTSolution structures
  ! ----------------------------------
  Error_Status = CRTM_Allocate_RTSolution( Atm(1)%n_Layers, RTSolution )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating RTSolution', & 
                           Error_Status)  
    STOP
  END IF
  Error_Status = CRTM_Allocate_RTSolution( Atm(1)%n_Layers, RTSolution_K )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating RTSolution_K', & 
                           Error_Status)  
    STOP
  END IF


  ! Set the adjoint values
  ! ----------------------
  ! Copy the atmosphere and surface structures
  ! (inefficient, but 'eh'.)
  DO l = 1, nChannels
    Error_Status = CRTM_Assign_Atmosphere( Atm, Atm_K(l,:) )
    IF ( Error_Status /= SUCCESS ) THEN 
      CALL Display_Message( PROGRAM_NAME, &
                            'Error copying Atmosphere structure array.', &
                            Error_Status )
      STOP
    END IF
    Error_Status = CRTM_Assign_Surface( Sfc, Sfc_K(l,:) )
    IF ( Error_Status /= SUCCESS ) THEN 
      CALL Display_Message( PROGRAM_NAME, &
                            'Error copying Surface structure array.', &
                            Error_Status )
      STOP
    END IF
  END DO
  ! Zero the K-matric structures
  CALL CRTM_Zero_Atmosphere( Atm_K )
  CALL CRTM_Zero_Surface( Sfc_K )
  
  ! The results are all dTb/dx...
  RTSolution_K%Brightness_Temperature = ONE


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

  
  ! Call the K-Matrix model
  ! -----------------------
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
      Atm%n_Clouds = nClouds
      Exp_ID = TRIM(Exp_ID)//'.cON'
      Exp_Description = TRIM(Exp_Description)//' Clouds ON'
    ELSE
      Atm%n_Clouds = 0
      Exp_ID = TRIM(Exp_ID)//'.cOFF'
      Exp_Description = TRIM(Exp_Description)//' Clouds OFF'
    END IF
    
    ! Turn aerosols on and off
    IF ( Perform_Test(i,AEROSOLS_TEST) ) THEN
      Atm%n_Aerosols = nAerosols
      Exp_ID = TRIM(Exp_ID)//'.aON'
      Exp_Description = TRIM(Exp_Description)//' Aerosols ON'
    ELSE
      Atm%n_Aerosols = 0
      Exp_ID = TRIM(Exp_ID)//'.aOFF'
      Exp_Description = TRIM(Exp_Description)//' Aerosols OFF'
    END IF

    WRITE(*,'(/5x,"Experiment: ",a)') TRIM(ADJUSTL(Exp_Description))


    ! Initialise the test
    ! -------------------
    CALL Init_Test( TRIM(ADJUSTL(Exp_Description)), &
                    Caller     =PROGRAM_NAME, &
                    Message_Log=MESSAGE_LOG)


    ! Call the CRTM K-matrix model
    ! ----------------------------
    CALL Begin_Timing( Timing )
    Error_Status = CRTM_K_Matrix( Atm            , &  
                                  Sfc            , &  
                                  RTSolution_K   , &  
                                  GeometryInfo   , &  
                                  ChannelInfo    , &  
                                  Atm_K          , &  
                                  Sfc_K          , &  
                                  RTSolution     , &  
                                  Options=Options  )
    CALL End_Timing( Timing )
    IF ( Error_Status /= SUCCESS ) THEN 
       CALL Display_Message( PROGRAM_NAME, &
                             'Error in CRTM K_Matrix Model', & 
                              Error_Status)  
     STOP
    END IF
    CALL Display_Timing( Timing, &
                         Caller     =PROGRAM_NAME, &
                         Message_Log=MESSAGE_LOG )


    ! Test K results for equality
    ! ---------------------------
    ! Output some results
    CALL Write_AtmSfc_TestFile( Exp_ID, ChannelInfo, Atm_K, Sfc_K )
    
    ! Read baseline results
    CALL Read_AtmSfc_TestFile( Exp_ID, ChannelInfo, Atm_Baseline, Sfc_Baseline )
    
    ! Compare them
    Atm_Status = CRTM_Equal_Atmosphere( Atm_Baseline, Atm_K )
    CALL Assert_Equal(Atm_Status,SUCCESS)
    Sfc_Status = CRTM_Equal_Surface(    Sfc_Baseline, Sfc_K )
    CALL Assert_Equal(Sfc_Status,SUCCESS)

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
  Error_Status = CRTM_Destroy_RTSolution(RTSolution_K)
  Error_Status = CRTM_Destroy_RTSolution(RTSolution)
  Error_Status = CRTM_Destroy_Surface(Sfc_K)
  Error_Status = CRTM_Destroy_Atmosphere(Atm_K)
  Error_Status = CRTM_Destroy_Surface(Sfc_Baseline)
  Error_Status = CRTM_Destroy_Atmosphere(Atm_Baseline)
  DEALLOCATE(RTSolution, RTSolution_K, &
             Sfc_K, Atm_K, &
             Sfc_Baseline, Atm_Baseline, &
             STAT = Allocate_Status)
  Error_Status = CRTM_Destroy_Surface(Sfc)
  Error_Status = CRTM_Destroy_Atmosphere(Atm)

END PROGRAM Test_K_Matrix
