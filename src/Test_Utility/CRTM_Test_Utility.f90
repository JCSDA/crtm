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
  USE Type_Kinds,              ONLY: fp
  USE File_Utility,            ONLY: Get_Lun, File_Exists
  USE CRTM_Parameters,         ONLY: ZERO
  USE CRTM_ChannelInfo_Define, ONLY: CRTM_ChannelInfo_type
  USE CRTM_Atmosphere_Define,  ONLY: CRTM_Atmosphere_type, &
                                     CLIMATOLOGY_MODEL_NAME, &
                                     CLOUD_TYPE_NAME, &
                                     AEROSOL_TYPE_NAME
  USE CRTM_Surface_Define,     ONLY: CRTM_Surface_type, &
                                     LAND_SURFACE, &
                                     WATER_SURFACE, &
                                     SNOW_SURFACE, &
                                     ICE_SURFACE, &
                                     SURFACE_TYPE_NAME
  USE CRTM_RTSolution_Define,  ONLY: CRTM_RTSolution_type
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! Parameters
  PUBLIC :: ATMDATA_FILENAME
  PUBLIC :: SFCDATA_FILENAME
  PUBLIC :: MAX_NPROFILES
  PUBLIC :: USED_NPROFILES
  PUBLIC :: EMISSIVITY_TEST
  PUBLIC :: CLOUDS_TEST
  PUBLIC :: AEROSOLS_TEST
  PUBLIC :: MAX_NTESTS
  PUBLIC :: MAX_NSENSORS
  PUBLIC :: TEST_SENSORID
  PUBLIC :: TEST_ANGLE
  PUBLIC :: TEST_DELTA
  PUBLIC :: FWD_TYPE
  PUBLIC :: TL_TYPE 
  PUBLIC :: AD_TYPE 
  PUBLIC :: KM_TYPE 
  PUBLIC :: TYPE_NAME
  ! Procedures
  PUBLIC :: Perform_Test
  PUBLIC :: Print_ChannelInfo
  PUBLIC :: Dump_FWD_Model_Results
  PUBLIC :: Dump_TL_Model_Results
  PUBLIC :: Dump_AD_Model_Results
  PUBLIC :: Dump_KM_Model_Results


  ! ------------------------
  ! Public module parameters
  ! ------------------------
  ! Datafile names and dimensions
  CHARACTER(*), PARAMETER :: ATMDATA_FILENAME = 'ECMWF-Atmosphere.Cloud.Aerosol.bin'
  CHARACTER(*), PARAMETER :: SFCDATA_FILENAME = 'ECMWF-Surface.bin'
  INTEGER,      PARAMETER :: MAX_NPROFILES  = 52
  INTEGER,      PARAMETER :: USED_NPROFILES = 10

  ! Set up the tests
  INTEGER, PARAMETER :: EMISSIVITY_TEST = 1
  INTEGER, PARAMETER :: CLOUDS_TEST     = 2
  INTEGER, PARAMETER :: AEROSOLS_TEST   = 4
  INTEGER, PARAMETER :: MAX_NTESTS = EMISSIVITY_TEST+CLOUDS_TEST+AEROSOLS_TEST

  ! Define the sensors to test
  INTEGER, PARAMETER :: MAX_NSENSORS=4
  CHARACTER(*), PARAMETER, DIMENSION(MAX_NSENSORS) :: TEST_SENSORID=&
    (/ 'amsua_n17', &
       'hirs3_n17', &
       'ssmis_f16', &
       'imgr_g11 ' /)

  ! Default test angle
  REAL(fp), PARAMETER :: TEST_ANGLE = 30.0_fp
  
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


  ! -------------------------
  ! Private module parameters
  ! -------------------------


CONTAINS


  ! ------------------
  ! PRIVATE procedures
  ! ------------------
  ! Function to open the test output file
  FUNCTION Open_TestFile(Filename,InStatus) RESULT(FileID)
    CHARACTER(*), INTENT(IN) :: Filename
    CHARACTER(*), INTENT(IN) :: InStatus
    INTEGER :: FileID
    CHARACTER(20) :: Position
    CHARACTER(20) :: Status
    INTEGER :: IO_Status

    Position='ASIS'
    Status  =InStatus
    IF ( TRIM(InStatus) == 'OLD' ) THEN
      IF ( File_Exists(Filename) ) THEN
        Position='APPEND'
      ELSE
        Status='REPLACE'
      END IF
    END IF

    FileID = Get_Lun()
    IF ( FileID < 0 ) THEN
      WRITE( *, * ) 'Error obtaining file id for ', TRIM(Filename)
      STOP
    END IF

    OPEN( FileID, FILE     = Filename, &
                  STATUS   = Status, &
                  FORM     = 'FORMATTED', &
                  POSITION = Position, &
                  IOSTAT   = IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( *, * ) 'Error opening ', TRIM(Filename), ' in ', &
                    TRIM(Status),'/',TRIM(Position), ' mode.'
      STOP
    END IF
  END FUNCTION Open_TestFile


  ! -----------------
  ! PUBLIC procedures
  ! -----------------
  
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
                   ChannelInfo%SensorID(l), &
                   ChannelInfo%WMO_Satellite_ID(l), &
                   ChannelInfo%WMO_Sensor_ID(l), &
                   ChannelInfo%Sensor_Channel(l)
    END DO

    ! Format statements
    100 FORMAT( /5x, 'Number of channels indexed: ', i5 )
    200 FORMAT( /2x, 'Channel        SensorID            WMO           WMO     Channel', &
               &/2x, ' Index                         Satellite ID   Sensor ID   Number', &
               &/2x, '----------------------------------------------------------------'  )
    300 FORMAT( 2x, 2x, i4, 2x, '>', a, '<', 5x, i3, 11x, i3, 7x, i4 )
  END SUBROUTINE Print_ChannelInfo


  ! ----------------------------------------
  ! Subroutine to dump out the ancillary
  ! information for a single atm/sfc profile
  ! ----------------------------------------
  SUBROUTINE Dump_ProfilePreamble(FileID, m, Atm, Sfc)
    ! Arguments
    INTEGER                   , INTENT(IN) :: FileID
    INTEGER                   , INTENT(IN) :: m
    TYPE(CRTM_Atmosphere_type), INTENT(IN) :: Atm(:)  ! M
    TYPE(CRTM_Surface_type)   , INTENT(IN) :: Sfc(:)  ! M
    ! Local variables
    CHARACTER(3) :: Advance
    INTEGER :: n
    INTEGER :: Sfc_Type
    
    WRITE(FileID,'(" Profile ",i0)') m
    WRITE(FileID,'("   Climatology: ", a )') CLIMATOLOGY_MODEL_NAME(Atm(m)%Climatology)
    WRITE(FileID,'("   Dimensions: nLayers, nAbsorbers ",/2i5)') &
                 Atm(m)%n_Layers, Atm(m)%n_Absorbers
    WRITE(FileID,'("   Dimensions: nClouds, nAerosols ",/2i5)') &
                 Atm(m)%n_Clouds, Atm(m)%n_Aerosols
    ! Output cloud info
    IF ( Atm(m)%n_Clouds > 0 ) THEN
      WRITE(FileID,'("   Cloud types: ")',ADVANCE='NO')
      DO n = 1, Atm(m)%n_Clouds
        IF (n < Atm(m)%n_Clouds) THEN
          Advance='NO'
        ELSE
          Advance='YES'
        END IF
        WRITE(FileID,'(a,1x)',ADVANCE=Advance) CLOUD_TYPE_NAME(Atm(m)%Cloud(n)%Type)
      END DO
    END IF
    ! Output aerosol info
    IF ( Atm(m)%n_Aerosols > 0 ) THEN
      WRITE(FileID,'("   Aerosol types: ")',ADVANCE='NO')
      DO n = 1, Atm(m)%n_Aerosols
        IF (n < Atm(m)%n_Aerosols) THEN
          Advance='NO'
        ELSE
          Advance='YES'
        END IF
        WRITE(FileID,'(a,1x)',ADVANCE=Advance) AEROSOL_TYPE_NAME(Atm(m)%Aerosol(n)%Type)
      END DO
    END IF
    ! Output surface info
    Sfc_Type=0
    IF( Sfc(m)%Land_Coverage  >ZERO ) Sfc_Type = LAND_SURFACE
    IF( Sfc(m)%Water_Coverage >ZERO ) Sfc_Type = Sfc_Type + WATER_SURFACE
    IF( Sfc(m)%Snow_Coverage  >ZERO ) Sfc_Type = Sfc_Type + SNOW_SURFACE
    IF( Sfc(m)%Ice_Coverage   >ZERO ) Sfc_Type = Sfc_Type + ICE_SURFACE
    WRITE(FileID,'("   Surface type: ",a)') SURFACE_TYPE_NAME(Sfc_Type)
  END SUBROUTINE Dump_ProfilePreamble


  ! --------------------------------------
  ! Subroutine to dump out results for the
  ! forward model tests
  ! --------------------------------------
  SUBROUTINE Dump_FWD_Model_Results(ExperimentNumber    , &
                                    ExperimentDescriptor, &
                                    ChannelInfo         , &
                                    Atmosphere          , &
                                    Surface             , &
                                    RTSolution            )
    ! Arguments
    INTEGER,                     INTENT(IN) :: ExperimentNumber
    CHARACTER(*),                INTENT(IN) :: ExperimentDescriptor
    TYPE(CRTM_ChannelInfo_type), INTENT(IN) :: ChannelInfo(:)   ! N
    TYPE(CRTM_Atmosphere_type) , INTENT(IN) :: Atmosphere(:)    ! M
    TYPE(CRTM_Surface_type)    , INTENT(IN) :: Surface(:)       ! M
    TYPE(CRTM_RTSolution_type) , INTENT(IN) :: RTSolution(:,:)  ! L x M
    ! Local variables
    CHARACTER(7)   :: Status
    CHARACTER(256) :: SensorID
    CHARACTER(256) :: Filename
    INTEGER :: l1, l2, l, m, n
    INTEGER :: nSensors, nChannels, nProfiles
    INTEGER :: FileID

    ! Obtain dimensions
    nSensors  = SIZE(ChannelInfo)
    nProfiles = SIZE(RTSolution,DIM=2)
    
    ! Open output file
    IF ( ExperimentNumber == 0 ) THEN
      Status = 'REPLACE'
    ELSE
      Status = 'OLD'
    END IF

    ! Initialise channel begin index
    l1=1
    
    ! Loop over sensors
    Sensor_Loop: DO n = 1, nSensors
    
      ! Initialise channel end index
      l2 = l1 + ChannelInfo(n)%n_Channels - 1
      nChannels = ChannelInfo(n)%n_Channels
      
      ! Assumption is one sensor per ChannelInfo element
      SensorID = ChannelInfo(n)%SensorID(1)
      Filename = TRIM(SensorID)//'.CRTM_Test_Forward.dump'
      
      ! Open the dump file for the current sensor
      FileID = Open_TestFile(Filename, Status)

      ! Write information to file
      WRITE(FileID,'(" Datatype: ",a)') TRIM(TYPE_NAME(FWD_TYPE))
      WRITE(FileID,'(" Filename: ",a)') TRIM(Filename)
      WRITE(FileID,'(" Experiment: ", a)') TRIM(ExperimentDescriptor) 
      WRITE(FileID,'(" Dimensions: nChannels, nProfiles ",/2i5)') nChannels, nProfiles

      Profile_Loop: DO m = 1, nProfiles
        ! Output the preamble for each profile
        CALL Dump_ProfilePreamble(FileId, m, Atmosphere, Surface)
        ! Output channel info
        Channel_Loop: DO l = l1, l2
          WRITE(FileID,'(i5,2x,a,3(2x,es16.9))') ChannelInfo(n)%Sensor_Channel(l-l1+1), &
                                                 ChannelInfo(n)%SensorID(l-l1+1), &
                                                 RTSolution(l,m)%Radiance, &
                                                 RTSolution(l,m)%Brightness_Temperature, &
                                                 RTSolution(l,m)%Surface_Emissivity
        END DO Channel_Loop
      END DO Profile_Loop
      
      ! Close dump file for current sensor
      CLOSE( FileID )
      
      ! Update channel begin index
      l1 = l2 + 1
    END DO Sensor_Loop
  END SUBROUTINE Dump_FWD_Model_Results


  ! --------------------------------------
  ! Subroutine to dump out results for the
  ! tangent-linear model tests
  ! --------------------------------------
  SUBROUTINE Dump_TL_Model_Results(ExperimentNumber    , &
                                   ExperimentDescriptor, &
                                   ChannelInfo         , &
                                   Atmosphere          , &
                                   Surface             , &
                                   RTSolution          , &
                                   RTSolution_TL         )
    ! Arguments
    INTEGER,                     INTENT(IN) :: ExperimentNumber
    CHARACTER(*),                INTENT(IN) :: ExperimentDescriptor
    TYPE(CRTM_ChannelInfo_type), INTENT(IN) :: ChannelInfo(:)      ! N
    TYPE(CRTM_Atmosphere_type) , INTENT(IN) :: Atmosphere(:)       ! M
    TYPE(CRTM_Surface_type)    , INTENT(IN) :: Surface(:)          ! M
    TYPE(CRTM_RTSolution_type) , INTENT(IN) :: RTSolution(:,:)     ! L x M
    TYPE(CRTM_RTSolution_type) , INTENT(IN) :: RTSolution_TL(:,:)  ! L x M
    ! Local variables
    CHARACTER(7)   :: Status
    CHARACTER(256) :: SensorID
    CHARACTER(256) :: Filename
    INTEGER :: l1, l2, l, m, n
    INTEGER :: nSensors, nChannels, nProfiles
    INTEGER :: FileID

    ! Obtain dimensions
    nSensors  = SIZE(ChannelInfo)
    nProfiles = SIZE(RTSolution,DIM=2)
    
    ! Open output file
    IF ( ExperimentNumber == 0 ) THEN
      Status = 'REPLACE'
    ELSE
      Status = 'OLD'
    END IF

    ! Initialise channel begin index
    l1=1
    
    ! Loop over sensors
    Sensor_Loop: DO n = 1, nSensors
    
      ! Initialise channel end index
      l2 = l1 + ChannelInfo(n)%n_Channels - 1
      nChannels = ChannelInfo(n)%n_Channels
      
      ! Assumption is one sensor per ChannelInfo element
      SensorID = ChannelInfo(n)%SensorID(1)
      Filename = TRIM(SensorID)//'.CRTM_Test_Tangent_Linear.dump'
      
      ! Open the dump file for the current sensor
      FileID = Open_TestFile(Filename, Status)

      ! Write information to file
      WRITE(FileID,'(" Datatype: ",a)') TRIM(TYPE_NAME(TL_TYPE))
      WRITE(FileID,'(" Filename: ",a)') TRIM(Filename)
      WRITE(FileID,'(" Experiment: ", a)') TRIM(ExperimentDescriptor) 
      WRITE(FileID,'(" Dimensions: nChannels, nProfiles ",/2i5)') nChannels, nProfiles

      Profile_Loop: DO m = 1, nProfiles
        ! Output the preamble for each profile
        CALL Dump_ProfilePreamble(FileId, m, Atmosphere, Surface)
        ! Output channel info
        Channel_Loop: DO l = l1, l2
          WRITE(FileID,'(i5,2x,a,6(2x,es16.9))') ChannelInfo(n)%Sensor_Channel(l-l1+1), &
                                                 ChannelInfo(n)%SensorID(l-l1+1), &
                                                 RTSolution(l,m)%Radiance, &
                                                 RTSolution(l,m)%Brightness_Temperature, &
                                                 RTSolution(l,m)%Surface_Emissivity, &
                                                 RTSolution_TL(l,m)%Radiance, &
                                                 RTSolution_TL(l,m)%Brightness_Temperature, &
                                                 RTSolution_TL(l,m)%Surface_Emissivity
        END DO Channel_Loop
      END DO Profile_Loop
      
      ! Close dump file for current sensor
      CLOSE( FileID )
      
      ! Update channel begin index
      l1 = l2 + 1
    END DO Sensor_Loop
  END SUBROUTINE Dump_TL_Model_Results


  ! --------------------------------------
  ! Subroutine to dump out results for the
  ! adjoint model tests
  ! --------------------------------------
  SUBROUTINE Dump_AD_Model_Results(ExperimentNumber    , &
                                   ExperimentDescriptor, &
                                   ChannelInfo         , &
                                   Atmosphere          , &
                                   Surface             , &
                                   RTSolution          , &
                                   RTSolution_AD       , &
                                   Atmosphere_AD       , &
                                   Surface_AD            )
    ! Arguments
    INTEGER,                     INTENT(IN) :: ExperimentNumber
    CHARACTER(*),                INTENT(IN) :: ExperimentDescriptor
    TYPE(CRTM_ChannelInfo_type), INTENT(IN) :: ChannelInfo(:)      ! N
    TYPE(CRTM_Atmosphere_type) , INTENT(IN) :: Atmosphere(:)       ! M
    TYPE(CRTM_Surface_type)    , INTENT(IN) :: Surface(:)          ! M
    TYPE(CRTM_RTSolution_type) , INTENT(IN) :: RTSolution(:,:)     ! L x M
    TYPE(CRTM_RTSolution_type) , INTENT(IN) :: RTSolution_AD(:,:)  ! L x M
    TYPE(CRTM_Atmosphere_type) , INTENT(IN) :: Atmosphere_AD(:)    ! M
    TYPE(CRTM_Surface_type)    , INTENT(IN) :: Surface_AD(:)       ! M
    ! Local variables
    CHARACTER(7)   :: Status
    CHARACTER(256) :: SensorID
    CHARACTER(256) :: Filename
    INTEGER :: j, l1, l2, l, m, n, nc, na
    INTEGER :: nSensors, nChannels, nProfiles
    INTEGER :: FileID

    ! Obtain dimensions
    nSensors  = SIZE(ChannelInfo)
    nProfiles = SIZE(RTSolution,DIM=2)
    
    ! Open output file
    IF ( ExperimentNumber == 0 ) THEN
      Status = 'REPLACE'
    ELSE
      Status = 'OLD'
    END IF

    ! Initialise channel begin index
    l1=1
    
    ! Loop over sensors
    Sensor_Loop: DO n = 1, nSensors
    
      ! Initialise channel end index
      l2 = l1 + ChannelInfo(n)%n_Channels - 1
      nChannels = ChannelInfo(n)%n_Channels
      
      ! Assumption is one sensor per ChannelInfo element
      SensorID = ChannelInfo(n)%SensorID(1)
      Filename = TRIM(SensorID)//'.CRTM_Test_Adjoint.dump'
      
      ! Open the dump file for the current sensor
      FileID = Open_TestFile(Filename, Status)

      ! Write information to file
      WRITE(FileID,'(" Datatype: ",a)') TRIM(TYPE_NAME(AD_TYPE))
      WRITE(FileID,'(" Filename: ",a)') TRIM(Filename)
      WRITE(FileID,'(" Experiment: ", a)') TRIM(ExperimentDescriptor) 
      WRITE(FileID,'(" Dimensions: nChannels, nProfiles ",/2i5)') nChannels, nProfiles

      Profile_Loop: DO m = 1, nProfiles
        ! Output the preamble for each profile
        CALL Dump_ProfilePreamble(FileId, m, Atmosphere, Surface)
        ! Output channel info
        Channel_Loop: DO l = l1, l2
          WRITE(FileID,'(i5,2x,a,6(2x,es16.9))') ChannelInfo(n)%Sensor_Channel(l-l1+1), &
                                                 ChannelInfo(n)%SensorID(l-l1+1), &
                                                 RTSolution(l,m)%Radiance, &
                                                 RTSolution(l,m)%Brightness_Temperature, &
                                                 RTSolution(l,m)%Surface_Emissivity, &
                                                 RTSolution_AD(l,m)%Surface_Emissivity
        END DO Channel_Loop
        ! Output surface adjoint data
        WRITE(FileID,'(5x,"Surface adjoint data")')
        WRITE(FileID,'(5x,4(2x,es16.9))') Surface_AD(m)%Land_Temperature, &
                                          Surface_AD(m)%Water_Temperature, &
                                          Surface_AD(m)%Snow_Temperature, &
                                          Surface_AD(m)%Ice_Temperature
        ! Output atmospheric absorber adjoint data
        WRITE(FileID,'(5x,"Absorber adjoint data")')
        WRITE(FileID,'(8(2x,es16.9))') Atmosphere_AD(m)%Pressure
        WRITE(FileID,'(8(2x,es16.9))') Atmosphere_AD(m)%Temperature
        DO j = 1, Atmosphere(m)%n_Absorbers
          WRITE(FileID,'(8(2x,es16.9))') Atmosphere_AD(m)%Absorber(:,j)
        END DO
        ! Output atmospheric cloud adjoint data
        IF (Atmosphere(m)%n_Clouds > 0) THEN
          WRITE(FileID,'(5x,"Cloud adjoint data")')
          DO nc = 1, Atmosphere(m)%n_Clouds
            WRITE(FileID,'(8(2x,es16.9))') Atmosphere_AD(m)%Cloud(nc)%Water_Content
            WRITE(FileID,'(8(2x,es16.9))') Atmosphere_AD(m)%Cloud(nc)%Effective_Radius
          END DO
        END IF
        ! Output atmospheric aerosol adjoint data
        IF (Atmosphere(m)%n_Aerosols > 0) THEN
          WRITE(FileID,'(5x,"Aerosol adjoint data")')
          DO na = 1, Atmosphere(m)%n_Aerosols
            WRITE(FileID,'(8(2x,es16.9))') Atmosphere_AD(m)%Aerosol(na)%Concentration
            WRITE(FileID,'(8(2x,es16.9))') Atmosphere_AD(m)%Aerosol(na)%Effective_Radius
          END DO
        END IF
      END DO Profile_Loop
      
      ! Close dump file for current sensor
      CLOSE( FileID )
      
      ! Update channel begin index
      l1 = l2 + 1
    END DO Sensor_Loop
  END SUBROUTINE Dump_AD_Model_Results


  ! --------------------------------------
  ! Subroutine to dump out results for the
  ! K-matrix model tests
  ! --------------------------------------
  SUBROUTINE Dump_KM_Model_Results(ExperimentNumber    , &
                                   ExperimentDescriptor, &
                                   ChannelInfo         , &
                                   Atmosphere          , &
                                   Surface             , &
                                   RTSolution          , &
                                   RTSolution_K        , &
                                   Atmosphere_K        , &
                                   Surface_K             )
    ! Arguments
    INTEGER,                     INTENT(IN) :: ExperimentNumber
    CHARACTER(*),                INTENT(IN) :: ExperimentDescriptor
    TYPE(CRTM_ChannelInfo_type), INTENT(IN) :: ChannelInfo(:)      ! N
    TYPE(CRTM_Atmosphere_type) , INTENT(IN) :: Atmosphere(:)       ! M
    TYPE(CRTM_Surface_type)    , INTENT(IN) :: Surface(:)          ! M
    TYPE(CRTM_RTSolution_type) , INTENT(IN) :: RTSolution(:,:)     ! L x M
    TYPE(CRTM_RTSolution_type) , INTENT(IN) :: RTSolution_K(:,:)   ! L x M
    TYPE(CRTM_Atmosphere_type) , INTENT(IN) :: Atmosphere_K(:,:)   ! L x M
    TYPE(CRTM_Surface_type)    , INTENT(IN) :: Surface_K(:,:)      ! L x M
    ! Local variables
    CHARACTER(7)   :: Status
    CHARACTER(256) :: SensorID
    CHARACTER(256) :: Filename
    INTEGER :: j, l1, l2, l, m, n, nc, na
    INTEGER :: nSensors, nChannels, nProfiles
    INTEGER :: FileID

    ! Obtain dimensions
    nSensors  = SIZE(ChannelInfo)
    nProfiles = SIZE(RTSolution,DIM=2)
    
    ! Open output file
    IF ( ExperimentNumber == 0 ) THEN
      Status = 'REPLACE'
    ELSE
      Status = 'OLD'
    END IF

    ! Initialise channel begin index
    l1=1
    
    ! Loop over sensors
    Sensor_Loop: DO n = 1, nSensors
    
      ! Initialise channel end index
      l2 = l1 + ChannelInfo(n)%n_Channels - 1
      nChannels = ChannelInfo(n)%n_Channels
      
      ! Assumption is one sensor per ChannelInfo element
      SensorID = ChannelInfo(n)%SensorID(1)
      Filename = TRIM(SensorID)//'.CRTM_Test_K_Matrix.dump'
      
      ! Open the dump file for the current sensor
      FileID = Open_TestFile(Filename, Status)

      ! Write information to file
      WRITE(FileID,'(" Datatype: ",a)') TRIM(TYPE_NAME(KM_TYPE))
      WRITE(FileID,'(" Filename: ",a)') TRIM(Filename)
      WRITE(FileID,'(" Experiment: ", a)') TRIM(ExperimentDescriptor) 
      WRITE(FileID,'(" Dimensions: nChannels, nProfiles ",/2i5)') nChannels, nProfiles

      Profile_Loop: DO m = 1, nProfiles
        ! Output the preamble for each profile
        CALL Dump_ProfilePreamble(FileId, m, Atmosphere, Surface)
        ! Output channel info
        Channel_Loop: DO l = l1, l2
          WRITE(FileID,'(i5,2x,a,6(2x,es16.9))') ChannelInfo(n)%Sensor_Channel(l-l1+1), &
                                                 ChannelInfo(n)%SensorID(l-l1+1), &
                                                 RTSolution(l,m)%Radiance, &
                                                 RTSolution(l,m)%Brightness_Temperature, &
                                                 RTSolution(l,m)%Surface_Emissivity, &
                                                 RTSolution_K(l,m)%Surface_Emissivity
          ! Output surface K-matrix data
          WRITE(FileID,'(5x,"Surface K-matrix data")')
          WRITE(FileID,'(5x,4(2x,es16.9))') Surface_K(l,m)%Land_Temperature, &
                                            Surface_K(l,m)%Water_Temperature, &
                                            Surface_K(l,m)%Snow_Temperature, &
                                            Surface_K(l,m)%Ice_Temperature
          ! Output atmospheric absorber K-matrix data
          WRITE(FileID,'(5x,"Absorber K-matrix data")')
          WRITE(FileID,'(8(2x,es16.9))') Atmosphere_K(l,m)%Pressure
          WRITE(FileID,'(8(2x,es16.9))') Atmosphere_K(l,m)%Temperature
          DO j = 1, Atmosphere(m)%n_Absorbers
            WRITE(FileID,'(8(2x,es16.9))') Atmosphere_K(l,m)%Absorber(:,j)
          END DO
          ! Output atmospheric cloud K-matrix data
          IF (Atmosphere(m)%n_Clouds > 0) THEN
            WRITE(FileID,'(5x,"Cloud K-matrix data")')
            DO nc = 1, Atmosphere(m)%n_Clouds
              WRITE(FileID,'(8(2x,es16.9))') Atmosphere_K(l,m)%Cloud(nc)%Water_Content
              WRITE(FileID,'(8(2x,es16.9))') Atmosphere_K(l,m)%Cloud(nc)%Effective_Radius
            END DO
          END IF
          ! Output atmospheric aerosol K-matrix data
          IF (Atmosphere(m)%n_Aerosols > 0) THEN
            WRITE(FileID,'(5x,"Aerosol K-matrix data")')
            DO na = 1, Atmosphere(m)%n_Aerosols
              WRITE(FileID,'(8(2x,es16.9))') Atmosphere_K(l,m)%Aerosol(na)%Concentration
              WRITE(FileID,'(8(2x,es16.9))') Atmosphere_K(l,m)%Aerosol(na)%Effective_Radius
            END DO
          END IF
        END DO Channel_Loop
      END DO Profile_Loop
      
      ! Close dump file for current sensor
      CLOSE( FileID )
      
      ! Update channel begin index
      l1 = l2 + 1
    END DO Sensor_Loop
  END SUBROUTINE Dump_KM_Model_Results

END MODULE CRTM_Test_Utility
