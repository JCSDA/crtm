!
! CRTM Test Utility
!
! Module containing definitions and utility
! routines for CRTM test codes.
!
MODULE CRTM_Test_Utility

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds             , ONLY: fp
  USE File_Utility           , ONLY: Get_Lun, File_Exists
  USE Binary_File_Utility    , ONLY: Open_Binary_File
  USE Message_Handler        , ONLY: SUCCESS, Display_Message
  USE CRTM_Parameters        , ONLY: ZERO, SET
  USE CRTM_ChannelInfo_Define, ONLY: CRTM_ChannelInfo_type
  USE CRTM_Atmosphere_Define , ONLY: CRTM_Atmosphere_type, &
                                     CLIMATOLOGY_MODEL_NAME, &
                                     ABSORBER_ID_NAME, &
                                     CLOUD_TYPE_NAME, &
                                     AEROSOL_TYPE_NAME, &
                                     CRTM_Atmosphere_ReadFile, &
                                     CRTM_Atmosphere_WriteFile
  USE CRTM_Surface_Define    , ONLY: CRTM_Surface_type, &
                                     LAND_SURFACE, &
                                     WATER_SURFACE, &
                                     SNOW_SURFACE, &
                                     ICE_SURFACE, &
                                     SURFACE_TYPE_NAME, &
                                     CRTM_Surface_WriteFile, &
                                     CRTM_Surface_ReadFile
  USE CRTM_RTSolution_Define , ONLY: CRTM_RTSolution_type, &
                                     CRTM_RTSolution_ReadFile, &
                                     CRTM_RTSolution_WriteFile
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! Parameters
  PUBLIC :: ATMDATA_FILENAME
  PUBLIC :: SFCDATA_FILENAME
  PUBLIC :: MAX_N_PROFILES
  PUBLIC :: USED_N_PROFILES
  PUBLIC :: TEST_ATM_FILENAME
  PUBLIC :: TEST_TRUNC_ATM_FILENAME
  PUBLIC :: TEST_SFC_FILENAME
  PUBLIC :: TEST_N_PROFILES
  PUBLIC :: EMISSIVITY_TEST
  PUBLIC :: CLOUDS_TEST
  PUBLIC :: AEROSOLS_TEST
  PUBLIC :: ANTCORR_TEST
  PUBLIC :: MAX_N_TESTS
  PUBLIC :: TEST_ZENITH_ANGLE
  PUBLIC :: TEST_SCAN_ANGLE
  PUBLIC :: TEST_DELTA
  PUBLIC :: FWD_TYPE
  PUBLIC :: TL_TYPE 
  PUBLIC :: AD_TYPE 
  PUBLIC :: KM_TYPE 
  PUBLIC :: TYPE_NAME
  PUBLIC :: D_PERCENT
  ! Procedures
  PUBLIC :: Perform_Test
  PUBLIC :: Print_ChannelInfo

  PUBLIC :: Write_RTSolution_TestFile
  PUBLIC :: Read_RTSolution_TestFile
  PUBLIC :: Write_AtmSfc_TestFile
  PUBLIC :: Read_AtmSfc_TestFile


  ! -------------------
  ! Procedure overloads
  ! -------------------
  INTERFACE Write_AtmSfc_TestFile
    MODULE PROCEDURE Write_AtmSfc_Rank1
    MODULE PROCEDURE Write_AtmSfc_Rank2
  END INTERFACE Write_AtmSfc_TestFile

  INTERFACE Read_AtmSfc_TestFile
    MODULE PROCEDURE Read_AtmSfc_Rank1
    MODULE PROCEDURE Read_AtmSfc_Rank2
  END INTERFACE Read_AtmSfc_TestFile


  ! ------------------------
  ! Public module parameters
  ! ------------------------
  ! Datafile names and dimensions for output comparison tests
  CHARACTER(*), PARAMETER :: ATMDATA_FILENAME = 'ECMWF-Atmosphere.Cloud.Aerosol.bin'
  CHARACTER(*), PARAMETER :: SFCDATA_FILENAME = 'ECMWF-Surface.bin'
  INTEGER,      PARAMETER :: MAX_N_PROFILES   = 52
  INTEGER,      PARAMETER :: USED_N_PROFILES  = 10

  ! Test datafiles
  CHARACTER(*), PARAMETER :: TEST_ATM_FILENAME       = 'Test.Atmosphere.Cloud.Aerosol.bin'
  CHARACTER(*), PARAMETER :: TEST_TRUNC_ATM_FILENAME = 'Truncated.Test.Atmosphere.Cloud.Aerosol.bin'
  CHARACTER(*), PARAMETER :: TEST_SFC_FILENAME       = 'Test.Surface.bin'
  INTEGER,      PARAMETER :: TEST_N_PROFILES         = 6

  ! Set up the tests
  INTEGER, PARAMETER :: EMISSIVITY_TEST = 1
  INTEGER, PARAMETER :: CLOUDS_TEST     = 2
  INTEGER, PARAMETER :: AEROSOLS_TEST   = 4
  INTEGER, PARAMETER :: ANTCORR_TEST    = 8
  INTEGER, PARAMETER :: MAX_N_TESTS = EMISSIVITY_TEST+CLOUDS_TEST+AEROSOLS_TEST+ANTCORR_TEST

  ! Default test angle
  REAL(fp), PARAMETER :: TEST_ZENITH_ANGLE = 30.0_fp
  REAL(fp), PARAMETER :: TEST_SCAN_ANGLE   = 26.37293341421_fp ! Based on default Re and h.
  
  ! Default test pertubation fraction
  REAL(fp), PARAMETER :: TEST_DELTA = 0.05_fp ! 5%
  
  ! Output type flags
  INTEGER, PARAMETER :: NTYPES = 4
  INTEGER, PARAMETER :: FWD_TYPE = 1
  INTEGER, PARAMETER :: TL_TYPE  = 2
  INTEGER, PARAMETER :: AD_TYPE  = 3
  INTEGER, PARAMETER :: KM_TYPE  = 4
  CHARACTER(*), PARAMETER, DIMENSION(NTYPES) :: TYPE_NAME = &
    (/'FWD','TL ','AD ','KM '/)

  ! Default percentage difference for floating point comparisons
  REAL(fp), PARAMETER :: D_PERCENT = 1.0e-04_fp
  
  ! Relative path for the resultant datafiles
  CHARACTER(*),PARAMETER :: RESULT_PATH = 'Results/'

CONTAINS

  
  ! Function to determine if a
  ! test should be carried out
  FUNCTION Perform_Test(TestNumber, TestID)
    INTEGER, INTENT(IN) :: TestNumber
    INTEGER, INTENT(IN) :: TestID
    LOGICAL :: Perform_Test
    Perform_Test = (IAND(TestNumber,TestID) /= 0)  
  END FUNCTION Perform_Test

  
  ! Procedure to output some initialisation info
  SUBROUTINE Print_ChannelInfo(ChannelInfo)
    TYPE(CRTM_ChannelInfo_type), INTENT(IN) :: ChannelInfo
    INTEGER :: l

    ! Output some info
    WRITE(*,100) ChannelInfo%n_Channels
    WRITE(*,200)
    DO l = 1, ChannelInfo%n_Channels
      WRITE(*,300) ChannelInfo%Channel_Index(l), &
                   ChannelInfo%Sensor_ID, &
                   ChannelInfo%WMO_Satellite_ID, &
                   ChannelInfo%WMO_Sensor_ID, &
                   ChannelInfo%Sensor_Channel(l)
    END DO

    ! Format statements
    100 FORMAT( /5x, 'Number of channels indexed: ', i5 )
    200 FORMAT( /2x, 'Channel        Sensor_ID           WMO           WMO     Channel', &
               &/2x, ' Index                         Satellite ID   Sensor ID   Number', &
               &/2x, '----------------------------------------------------------------'  )
    300 FORMAT( 2x, 2x, i4, 2x, '>', a, '<', 5x, i3, 11x, i3, 7x, i4 )
  END SUBROUTINE Print_ChannelInfo



!----------------------------------------------------------------------------------
!
! NAME:
!       Write_RTSolution_TestFile
!
! PURPOSE:
!       Subroutine to write the CRTM RTSolution structure to test file(s)
!
! CALLING SEQUENCE:
!
!----------------------------------------------------------------------------------

  SUBROUTINE Write_RTSolution_TestFile( Experiment , &
                                        ChannelInfo, &
                                        RTSolution   )
    ! Arguments
    CHARACTER(*),                INTENT(IN) :: Experiment
    TYPE(CRTM_ChannelInfo_type), INTENT(IN) :: ChannelInfo(:)   ! N
    TYPE(CRTM_RTSolution_type) , INTENT(IN) :: RTSolution(:,:)  ! L x M
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_RTSolution_TestFile'
    ! Local variables
    INTEGER :: Error_Status
    CHARACTER(256) :: Filename
    INTEGER :: l1, l2, n
    INTEGER :: n_Sensors, n_Channels, n_Profiles

    ! Set up
    ! ------
    ! Obtain dimensions
    n_Sensors  = SIZE(ChannelInfo)
    n_Profiles = SIZE(RTSolution,DIM=2)
    
    ! Initialise channel begin index
    l1=1


    ! Loop over sensors
    ! -----------------
    Sensor_Loop: DO n = 1, n_Sensors
    
      ! Initialise channel end index
      l2 = l1 + ChannelInfo(n)%n_Channels - 1
      n_Channels = ChannelInfo(n)%n_Channels
      
      ! Create filename
      Filename = RESULT_PATH//TRIM(ChannelInfo(n)%Sensor_ID)//TRIM(Experiment)//'.bin'
      
      ! Write the RTSolution data for the current sensor
      Error_Status = CRTM_RTSolution_WriteFile( Filename, RTSolution(l1:l2,:), Quiet=.TRUE. )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing file '//TRIM(Filename), &
                              Error_Status )
        STOP
      END IF

      ! Update channel begin index
      l1 = l2 + 1
    END DO Sensor_Loop
    
  END SUBROUTINE Write_RTSolution_TestFile


!----------------------------------------------------------------------------------
!
! NAME:
!       Read_RTSolution_TestFile
!
! PURPOSE:
!       Subroutine to read the CRTM RTSolution structure from test file(s)
!
! CALLING SEQUENCE:
!
!----------------------------------------------------------------------------------

  SUBROUTINE Read_RTSolution_TestFile( Experiment , &
                                       ChannelInfo, &
                                       RTSolution , &
                                       Quiet        )
    ! Arguments
    CHARACTER(*),                INTENT(IN)     :: Experiment
    TYPE(CRTM_ChannelInfo_type), INTENT(IN)     :: ChannelInfo(:)   ! N
    TYPE(CRTM_RTSolution_type) , INTENT(IN OUT) :: RTSolution(:,:)  ! L x M
    LOGICAL,           OPTIONAL, INTENT(IN)     :: Quiet
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_RTSolution_TestFile'
    ! Local variables
    INTEGER :: Error_Status
    CHARACTER(256) :: Filename
    INTEGER :: l1, l2, n
    INTEGER :: n_Sensors, n_Channels, n_Profiles

    ! Set up
    ! ------
    ! Obtain dimensions
    n_Sensors  = SIZE(ChannelInfo)
    n_Profiles = SIZE(RTSolution,DIM=2)
    
    ! Initialise channel begin index
    l1=1


    ! Loop over sensors
    ! -----------------
    Sensor_Loop: DO n = 1, n_Sensors
    
      ! Initialise channel end index
      l2 = l1 + ChannelInfo(n)%n_Channels - 1
      n_Channels = ChannelInfo(n)%n_Channels
      
      ! Create filename
      Filename = RESULT_PATH//TRIM(ChannelInfo(n)%Sensor_ID)//TRIM(Experiment)//'.bin.Baseline'
      
      ! Read the RTSolution data for the current sensor
      Error_Status = CRTM_RTSolution_ReadFile( Filename, RTSolution(l1:l2,:), Quiet=Quiet )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading file '//TRIM(Filename), &
                              Error_Status )
        STOP
      END IF

      ! Update channel begin index
      l1 = l2 + 1
    END DO Sensor_Loop
    
  END SUBROUTINE Read_RTSolution_TestFile


!----------------------------------------------------------------------------------
!
! NAME:
!       Write_AtmSfc_TestFile
!
! PURPOSE:
!       Subroutine to write the CRTM atm and sfc structures to test files
!
! CALLING SEQUENCE:
!
!----------------------------------------------------------------------------------

  SUBROUTINE Write_AtmSfc_Rank1( Experiment , &
                                 Atm, Sfc     )
    ! Arguments
    CHARACTER(*),                INTENT(IN) :: Experiment
    TYPE(CRTM_Atmosphere_type) , INTENT(IN) :: Atm(:)    ! M
    TYPE(CRTM_Surface_type)    , INTENT(IN) :: Sfc(:)    ! M
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_AtmSfc_TestFile(Rank-1)'
    ! Local variables
    INTEGER :: Error_Status
    CHARACTER(256) :: AtmFile, SfcFile

    ! Write atmosphere data
    ! ---------------------
    AtmFile = RESULT_PATH//'atm'//TRIM(Experiment)//'.bin'
    Error_Status = CRTM_Atmosphere_WriteFile( AtmFile, Atm, Quiet=.TRUE. )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing file '//TRIM(AtmFile), &
                            Error_Status )
      STOP
    END IF

    ! Write surface data
    ! ------------------
    SfcFile = RESULT_PATH//'sfc'//TRIM(Experiment)//'.bin'
    Error_Status = CRTM_Surface_WriteFile( SfcFile, Sfc, Quiet=.TRUE. )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing file '//TRIM(SfcFile), &
                            Error_Status )
      STOP
    END IF

  END SUBROUTINE Write_AtmSfc_Rank1


  SUBROUTINE Write_AtmSfc_Rank2( Experiment , &
                                 ChannelInfo, &
                                 Atm, Sfc     )
    ! Arguments
    CHARACTER(*),                INTENT(IN) :: Experiment
    TYPE(CRTM_ChannelInfo_type), INTENT(IN) :: ChannelInfo(:)   ! N
    TYPE(CRTM_Atmosphere_type) , INTENT(IN) :: Atm(:,:)         ! L x M
    TYPE(CRTM_Surface_type)    , INTENT(IN) :: Sfc(:,:)         ! L x M
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_AtmSfc_TestFile(Rank-2)'
    ! Local variables
    INTEGER :: Error_Status
    CHARACTER(256) :: AtmFile, SfcFile
    INTEGER :: l1, l2, n
    INTEGER :: n_Sensors, n_Channels

    ! Set up
    ! ------
    ! Obtain dimensions
    n_Sensors  = SIZE(ChannelInfo)
    
    ! Initialise channel begin index
    l1=1


    ! Loop over sensors
    ! -----------------
    Sensor_Loop: DO n = 1, n_Sensors
    
      ! Initialise channel end index
      l2 = l1 + ChannelInfo(n)%n_Channels - 1
      n_Channels = ChannelInfo(n)%n_Channels
      
      ! Write the Atmosphere data for the current sensor
      AtmFile = RESULT_PATH//TRIM(ChannelInfo(n)%Sensor_ID)//'.atm'//TRIM(Experiment)//'.bin'
      Error_Status = CRTM_Atmosphere_WriteFile( AtmFile, Atm(l1:l2,:), Quiet=.TRUE. )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing file '//TRIM(AtmFile), &
                              Error_Status )
        STOP
      END IF

      ! Write the Atmosphere data for the current sensor
      SfcFile = RESULT_PATH//TRIM(ChannelInfo(n)%Sensor_ID)//'.sfc'//TRIM(Experiment)//'.bin'
      Error_Status = CRTM_Surface_WriteFile( SfcFile, Sfc(l1:l2,:), Quiet=.TRUE. )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing file '//TRIM(SfcFile), &
                              Error_Status )
        STOP
      END IF

      ! Update channel begin index
      l1 = l2 + 1
    END DO Sensor_Loop
    
  END SUBROUTINE Write_AtmSfc_Rank2


!----------------------------------------------------------------------------------
!
! NAME:
!       Read_AtmSfc_TestFile
!
! PURPOSE:
!       Subroutine to read the CRTM atm and sfc structures from test files
!
! CALLING SEQUENCE:
!
!----------------------------------------------------------------------------------

  SUBROUTINE Read_AtmSfc_Rank1( Experiment , &
                                Atm, Sfc     )
    ! Arguments
    CHARACTER(*),                INTENT(IN)     :: Experiment
    TYPE(CRTM_Atmosphere_type) , INTENT(IN OUT) :: Atm(:)    ! M
    TYPE(CRTM_Surface_type)    , INTENT(IN OUT) :: Sfc(:)    ! M
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_AtmSfc_TestFile(Rank-1)'
    ! Local variables
    INTEGER :: Error_Status
    CHARACTER(256) :: AtmFile, SfcFile

    ! Read atmosphere data
    ! ---------------------
    AtmFile = RESULT_PATH//'atm'//TRIM(Experiment)//'.bin.Baseline'
    Error_Status = CRTM_Atmosphere_ReadFile( AtmFile, Atm, Quiet=.TRUE. )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading file '//TRIM(AtmFile), &
                            Error_Status )
      STOP
    END IF


    ! Read surface data
    ! -----------------
    SfcFile = RESULT_PATH//'sfc'//TRIM(Experiment)//'.bin.Baseline'
    Error_Status = CRTM_Surface_ReadFile( SfcFile, Sfc, Quiet=.TRUE. )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading file '//TRIM(SfcFile), &
                            Error_Status )
      STOP
    END IF

  END SUBROUTINE Read_AtmSfc_Rank1


  SUBROUTINE Read_AtmSfc_Rank2( Experiment , &
                                ChannelInfo, &
                                Atm, Sfc     )
    ! Arguments
    CHARACTER(*),                INTENT(IN)     :: Experiment
    TYPE(CRTM_ChannelInfo_type), INTENT(IN)     :: ChannelInfo(:)   ! N
    TYPE(CRTM_Atmosphere_type) , INTENT(IN OUT) :: Atm(:,:)         ! L x M
    TYPE(CRTM_Surface_type)    , INTENT(IN OUT) :: Sfc(:,:)         ! L x M
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_AtmSfc_TestFile(Rank-2)'
    ! Local variables
    INTEGER :: Error_Status
    CHARACTER(256) :: AtmFile, SfcFile
    INTEGER :: l1, l2, n
    INTEGER :: n_Sensors, n_Channels

    ! Set up
    ! ------
    ! Obtain dimensions
    n_Sensors  = SIZE(ChannelInfo)
    
    ! Initialise channel begin index
    l1=1


    ! Loop over sensors
    ! -----------------
    Sensor_Loop: DO n = 1, n_Sensors
    
      ! Initialise channel end index
      l2 = l1 + ChannelInfo(n)%n_Channels - 1
      n_Channels = ChannelInfo(n)%n_Channels
      
      ! Read the Atmosphere data for the current sensor
      AtmFile = RESULT_PATH//TRIM(ChannelInfo(n)%Sensor_ID)//'.atm'//TRIM(Experiment)//'.bin.Baseline'
      Error_Status = CRTM_Atmosphere_ReadFile( AtmFile, Atm(l1:l2,:), Quiet=.TRUE. )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading file '//TRIM(AtmFile), &
                              Error_Status )
        STOP
      END IF

      ! Read the Atmosphere data for the current sensor
      SfcFile = RESULT_PATH//TRIM(ChannelInfo(n)%Sensor_ID)//'.sfc'//TRIM(Experiment)//'.bin.Baseline'
      Error_Status = CRTM_Surface_ReadFile( SfcFile, Sfc(l1:l2,:), Quiet=.TRUE. )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading file '//TRIM(SfcFile), &
                              Error_Status )
        STOP
      END IF

      ! Update channel begin index
      l1 = l2 + 1
    END DO Sensor_Loop
    
  END SUBROUTINE Read_AtmSfc_Rank2

END MODULE CRTM_Test_Utility
