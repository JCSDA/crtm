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
  
  PUBLIC :: EMISSIVITY_TEST
  PUBLIC :: CLOUDS_TEST
  PUBLIC :: AEROSOLS_TEST
  PUBLIC :: MAX_NTESTS
  
  PUBLIC :: MAX_NSENSORS
  PUBLIC :: TEST_SENSORID  
  
  PUBLIC :: FWD_OUTPUT
  PUBLIC :: TL_OUTPUT
  PUBLIC :: AD_OUTPUT
  PUBLIC :: K_OUTPUT
  ! Procedures
  PUBLIC :: Perform_Test
  PUBLIC :: Print_ChannelInfo
  PUBLIC :: Print_Results
  PUBLIC :: Print_FWD_Results
  PUBLIC :: Print_K_Results
  PUBLIC :: Dump_ForwardModel_Results


  ! ------------------------
  ! Public module parameters
  ! ------------------------
  ! Datafile names and dimensions
  CHARACTER(*), PARAMETER :: ATMDATA_FILENAME = 'ECMWF-Atmosphere.Cloud.Aerosol.bin'
  CHARACTER(*), PARAMETER :: SFCDATA_FILENAME = 'ECMWF-Surface.bin'
  INTEGER,      PARAMETER :: MAX_NPROFILES = 52

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

  INTEGER, PARAMETER :: FWD_OUTPUT = 1
  INTEGER, PARAMETER :: TL_OUTPUT  = 2
  INTEGER, PARAMETER :: AD_OUTPUT  = 3
  INTEGER, PARAMETER :: K_OUTPUT   = 4


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
  SUBROUTINE Print_ChannelInfo(Filename,ChannelInfo)
    CHARACTER(*),                INTENT(IN) :: Filename
    TYPE(CRTM_ChannelInfo_type), INTENT(IN) :: ChannelInfo
    INTEGER :: FileID
    INTEGER :: IO_Status
    INTEGER :: l

    ! Open output file
    FileID = Open_TestFile(Filename, 'REPLACE')

    ! Write to stdout and file
    WRITE( *,      100 ) ChannelInfo%n_Channels
    WRITE( FileID, 100 ) ChannelInfo%n_Channels
    WRITE( *,      200 )
    WRITE( FileID, 200 )
    DO l = 1, ChannelInfo%n_Channels
      WRITE( *,      300 ) ChannelInfo%Channel_Index( l ), &
                           ChannelInfo%SensorID( l ), &
                           ChannelInfo%WMO_Satellite_ID( l ), &
                           ChannelInfo%WMO_Sensor_ID( l ), &
                           ChannelInfo%Sensor_Channel( l )
      WRITE( FileID, 300 ) ChannelInfo%Channel_Index( l ), &
                           ChannelInfo%SensorID( l ), &
                           ChannelInfo%WMO_Satellite_ID( l ), &
                           ChannelInfo%WMO_Sensor_ID( l ), &
                           ChannelInfo%Sensor_Channel( l )
    END DO
    CLOSE( FileID )

    ! Format statements
    100 FORMAT( /5x, 'Number of channels indexed: ', i5 )
    200 FORMAT( /2x, 'Channel        SensorID            WMO           WMO     Channel', &
               &/2x, ' Index                         Satellite ID   Sensor ID   Number', &
               &/2x, '----------------------------------------------------------------'  )
    300 FORMAT( 2x, 2x, i4, 2x, '>', a, '<', 5x, i3, 11x, i3, 7x, i4 )

  END SUBROUTINE Print_ChannelInfo


  ! Procedure to output some CRTM run results
  SUBROUTINE Print_FWD_Results(Filename   , &
                               ChannelInfo, &
                               RTSolution   )
    ! Arguments
    CHARACTER(*),                                INTENT(IN) :: Filename
    TYPE(CRTM_ChannelInfo_type),                 INTENT(IN) :: ChannelInfo
    TYPE(CRTM_RTSolution_type),  DIMENSION(:,:), INTENT(IN) :: RTSolution
    ! Local variables
    INTEGER :: FileID
    INTEGER :: IO_Status
    INTEGER :: l, nChannels
    INTEGER :: m, nProfiles

    ! Set up
    nChannels = SIZE(RTSolution,DIM=1)
    nProfiles = SIZE(RTSolution,DIM=2)

    ! Open output file
    FileID = Open_TestFile(Filename, 'REPLACE')

    ! Output header
    !
    ! Output instrument ID
    WRITE(FileID,'(a)') ChannelInfo%SensorID(1)
    ! Output dimensions
    WRITE(FileID,'(2i5)')nChannels, nProfiles

    ! Write to file
    Profile_Loop: DO m = 1, nProfiles
      WRITE(FileID,'("Profile #",i0)') m
      Channel_Loop: DO l = 1, nChannels
        WRITE(FileID,'(i5,2x,f10.6)') ChannelInfo%Sensor_Channel(l), &
                                      RTSolution(l,m)%Brightness_Temperature
      END DO Channel_Loop
    END DO Profile_Loop
    CLOSE( FileID )
  END SUBROUTINE Print_FWD_Results

  SUBROUTINE Print_K_Results(Filename    , &
                             ChannelInfo , &
                             Atmosphere  , &
                             Atmosphere_K, &
                             Surface_K     )
    ! Arguments
    CHARACTER(*),                                INTENT(IN) :: Filename
    TYPE(CRTM_ChannelInfo_type),                 INTENT(IN) :: ChannelInfo
    TYPE(CRTM_Atmosphere_type),  DIMENSION(:)  , INTENT(IN) :: Atmosphere
    TYPE(CRTM_Atmosphere_type),  DIMENSION(:,:), INTENT(IN) :: Atmosphere_K
    TYPE(CRTM_Surface_type),     DIMENSION(:,:), INTENT(IN) :: Surface_K
    ! Local variables
    INTEGER :: FileID
    INTEGER :: IO_Status
    INTEGER :: k, nLayers
    INTEGER :: l, nChannels
    INTEGER :: m, nProfiles

    ! Set up
    nLayers   = Atmosphere_K(1,1)%n_Layers
    nChannels = SIZE(Atmosphere_K,DIM=1)
    nProfiles = SIZE(Atmosphere_K,DIM=2)

    ! Open output file
    FileID = Open_TestFile(Filename, 'REPLACE')

    ! Output header
    !
    ! Output instrument ID
    WRITE(FileID,'(a)') ChannelInfo%SensorID(1)
    ! Output dimensions
    WRITE(FileID,'(3i5)')nLayers, nChannels, nProfiles

    ! Write to file
    Profile_Loop: DO m = 1, nProfiles
      WRITE(FileID,'("Profile #",i0)') m
      Channel_Loop: DO l = 1, nChannels
        WRITE(FileID,'("Channel #",i0)') ChannelInfo%Sensor_Channel(l)
        Layer_Loop: DO k = 1, nLayers
          WRITE(FileID,'(i5,4(2x,es13.6))') k, &
                                        Atmosphere(m)%Pressure(k)       , & ! FWD pressure
                                        Atmosphere_K(l,m)%Temperature(k), & ! K Temperature
                                        Atmosphere_K(l,m)%Absorber(k,1) , & ! K Water vapour
                                        Atmosphere_K(l,m)%Absorber(k,2)     ! K Ozone
        END DO Layer_Loop
      END DO Channel_Loop
    END DO Profile_Loop
    CLOSE( FileID )
  END SUBROUTINE Print_K_Results

  SUBROUTINE Print_Results(Output_Type, Filename, Message, &
                           ChannelInfo, Atmosphere, Surface, RTSolution, &
                           RTSolution_TL, &
                           RTSolution_AD, &
                           RTSolution_K, &
                           Surface_AD, &
                           SURFACE_K)
    INTEGER,                                              INTENT(IN) :: Output_Type
    CHARACTER(*),                                         INTENT(IN) :: Filename
    CHARACTER(*),                                         INTENT(IN) :: Message
    TYPE(CRTM_ChannelInfo_type),                          INTENT(IN) :: ChannelInfo
    TYPE(CRTM_Atmosphere_type),           DIMENSION(:),   INTENT(IN) :: Atmosphere
    TYPE(CRTM_Surface_type),              DIMENSION(:),   INTENT(IN) :: Surface
    TYPE(CRTM_RTSolution_type),           DIMENSION(:,:), INTENT(IN) :: RTSolution
    TYPE(CRTM_RTSolution_type), OPTIONAL, DIMENSION(:,:), INTENT(IN) :: RTSolution_TL
    TYPE(CRTM_RTSolution_type), OPTIONAL, DIMENSION(:,:), INTENT(IN) :: RTSolution_AD
    TYPE(CRTM_RTSolution_type), OPTIONAL, DIMENSION(:,:), INTENT(IN) :: RTSolution_K
    TYPE(CRTM_Surface_type),    OPTIONAL, DIMENSION(:),   INTENT(IN) :: Surface_AD
    TYPE(CRTM_Surface_type),    OPTIONAL, DIMENSION(:,:), INTENT(IN) :: Surface_K
    ! Local parameters for formatting output
    CHARACTER(*), PARAMETER :: FWD_HDR_FMT = &
     '(/2x,"Channel         Sensor            Radiance      Brightness     Surface",'//&
     ' /2x," Index        Descriptor                        Temperature   Emissivity",'//&
     ' /2x,"------------------------------------------------------------------------")'
    CHARACTER(*), PARAMETER :: FWD_FMT = &
     '(4x,i4,2x,">",a,"<",3x,f10.6,5x,f10.6,3x,f10.6)'

    CHARACTER(*), PARAMETER :: TL_HDR_FMT = &
     '(/2x,"Channel         Sensor            Radiance          Tb    '//&
     '     Surface     Emissivity_TL   Radiance_TL       Tb_TL  ",'//&
     ' /2x," Index        Descriptor                                  '//&
     '    Emissivity",'//&
     ' /2x,"----------------------------------------------------------'//&
     '-----------------------------------------------------------")'
    CHARACTER(*), PARAMETER :: TL_FMT = &
     '(4x,i4,2x,">",a,"<",3x,f10.6,5x,f10.6,3x,f10.6,4x,f9.6,5x,f10.6,5x,f10.6 )'

    CHARACTER(*), PARAMETER :: AD_HDR_FMT = &
     '( /2x,"Channel         Sensor            Radiance          Tb       '//&
     '  Surface     Emissivity_AD               Tsfc_AD",'//&
     '  /2x," Index        Descriptor                                     '//&
     ' Emissivity                     Land   Water    Snow    Ice ",'//&
     '  /2x,"-------------------------------------------------------------'//&
     '--------------------------------------------------------------")'
    CHARACTER(*), PARAMETER :: AD_FMT = &
     '(4x,i4,2x,">",a,"<",3x,f10.6,5x,f10.6,3x,f10.6,5x,f10.6,5x,4f8.4)'

    CHARACTER(*), PARAMETER :: K_HDR_FMT = &
     '(/2x,"Channel         Sensor            Radiance          Tb       '//&
     '  Surface     Emissivity_K                Tsfc_K",'//&
     ' /2x," Index        Descriptor                                     '//&
     ' Emissivity                      Land    Water   Snow    Ice",'//&
     ' /2x,"-------------------------------------------------------------'//&
     '--------------------------------------------------------------")'
    CHARACTER(*), PARAMETER :: K_FMT = &
     '(4x,i4,2x,">",a,"<",3x,f10.6,5x,f10.6,3x,f10.6,5x,f10.6,5x,4f8.4)'
    CHARACTER(*), PARAMETER :: K_FMT2 = &
     '(93x,"--------------------------------",/93x,4f8.4)'

    INTEGER :: FileID
    INTEGER :: IO_Status
    INTEGER :: l, m, n
    INTEGER :: Surface_Type

    ! Open output file
    FileID = Open_TestFile(Filename, 'OLD')

    ! Write to file
    WRITE( FileID, '( /5x, a )' ) TRIM(Message)
    Profile_Loop: DO m = 1, SIZE( RTSolution, 2 )

      ! Model independent output
      WRITE(FileID,'(/5x,"Profile #", i2, " results:" )') m
      WRITE(FileID,'(/5x,"Atmosphere climatology: ", a )') CLIMATOLOGY_MODEL_NAME(Atmosphere(m)%Climatology)
      DO n = 1, Atmosphere(m)%n_Clouds
        WRITE(FileID,'(5x,"Cloud ",i3," type: ",a)') n, CLOUD_TYPE_NAME( Atmosphere(m)%Cloud(n)%Type )
      END DO
      Surface_Type=0
      IF( Surface(m)%Land_Coverage  >ZERO ) Surface_Type = LAND_SURFACE
      IF( Surface(m)%Water_Coverage >ZERO ) Surface_Type = Surface_Type + WATER_SURFACE
      IF( Surface(m)%Snow_Coverage  >ZERO ) Surface_Type = Surface_Type + SNOW_SURFACE
      IF( Surface(m)%Ice_Coverage   >ZERO ) Surface_Type = Surface_Type + ICE_SURFACE
      WRITE(FileID,'(5x,"Surface type: ",a)') SURFACE_TYPE_NAME(Surface_Type)

      ! Model dependent output
      SELECT CASE (Output_Type)

        ! Forward model output
        CASE (FWD_OUTPUT)
          WRITE(FileID,FMT=FWD_HDR_FMT)
          FWD_Channel_Loop: DO l = 1, SIZE( RTSolution, 1 )
            WRITE(FileID,FMT=FWD_FMT) ChannelInfo%Channel_Index( l ), &
                                      ChannelInfo%SensorID( l ), &
                                      RTSolution(l,m)%Radiance, &
                                      RTSolution(l,m)%Brightness_Temperature, &
                                      RTSolution(l,m)%Surface_Emissivity
          END DO FWD_Channel_Loop

        ! Tangent-linear model output
        CASE (TL_OUTPUT)
          WRITE(FileID, FMT=TL_HDR_FMT)
          TL_Channel_Loop: DO l = 1, SIZE( RTSolution, 1 )
            WRITE(FileID,FMT=TL_FMT) ChannelInfo%Channel_Index( l ), &
                                     ChannelInfo%SensorID( l ), &
                                     RTSolution(l,m)%Radiance, &
                                     RTSolution(l,m)%Brightness_Temperature, &
                                     RTSolution(l,m)%Surface_Emissivity, &
                                     RTSolution_TL(l,m)%Surface_Emissivity, &
                                     RTSolution_TL(l,m)%Radiance, &
                                     RTSolution_TL(l,m)%Brightness_Temperature
          END DO TL_Channel_Loop

        ! Adjoint model output
        CASE (AD_OUTPUT)
          WRITE(FileID,FMT=AD_HDR_FMT)
          AD_Channel_Loop: DO l = 1, SIZE( RTSolution, 1 )
            WRITE(FileID,FMT=AD_FMT) ChannelInfo%Channel_Index( l ), &
                                     ChannelInfo%SensorID( l ), &
                                     RTSolution(l,m)%Radiance, &
                                     RTSolution(l,m)%Brightness_Temperature, &
                                     RTSolution(l,m)%Surface_Emissivity, &
                                     RTSolution_AD(l,m)%Surface_Emissivity, &
                                     Surface_AD(m)%Land_Temperature, &
                                     Surface_AD(m)%Water_Temperature, &
                                     Surface_AD(m)%Snow_Temperature, &
                                     Surface_AD(m)%Ice_Temperature
          END DO AD_Channel_Loop

        ! K-Matrix model output
        CASE (K_OUTPUT)
          WRITE(FileID,FMT=K_HDR_FMT)
          K_Channel_Loop: DO l = 1, SIZE( RTSolution, 1 )
            WRITE(FileID,FMT=K_FMT) ChannelInfo%Channel_Index( l ), &
                                    ChannelInfo%SensorID( l ), &
                                    RTSolution(l,m)%Radiance, &
                                    RTSolution(l,m)%Brightness_Temperature, &
                                    RTSolution(l,m)%Surface_Emissivity, &
                                    RTSolution_K(l,m)%Surface_Emissivity, &
                                    Surface_K(l,m)%Land_Temperature, &
                                    Surface_K(l,m)%Water_Temperature, &
                                    Surface_K(l,m)%Snow_Temperature, &
                                    Surface_K(l,m)%Ice_Temperature
          END DO K_Channel_Loop
          WRITE(FileID,FMT=K_FMT2)SUM(Surface_K(:,m)%Land_Temperature), &
                                  SUM(Surface_K(:,m)%Water_Temperature), &
                                  SUM(Surface_K(:,m)%Snow_Temperature), &
                                  SUM(Surface_K(:,m)%Ice_Temperature)
      END SELECT
    END DO Profile_Loop
    CLOSE( FileID )
  END SUBROUTINE Print_Results


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


  SUBROUTINE Dump_ForwardModel_Results(ExperimentNumber    , &
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
  END SUBROUTINE Dump_ForwardModel_Results

END MODULE CRTM_Test_Utility
