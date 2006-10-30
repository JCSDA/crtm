!
! CRTM Test Utility
!
! Module containing utility routines for CRTM test codes.
!
MODULE CRTM_Test_Utility

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE File_Utility,           ONLY: Get_Lun
  USE CRTM_Parameters,        ONLY: ZERO
  USE CRTM_ChannelInfo,       ONLY: CRTM_ChannelInfo_type
  USE CRTM_Atmosphere_Define, ONLY: CRTM_Atmosphere_type, &
                                    CLIMATOLOGY_MODEL_NAME, &
                                    CLOUD_TYPE_NAME
  USE CRTM_Surface_Define,    ONLY: CRTM_Surface_type, &
                                    LAND_SURFACE, &
                                    WATER_SURFACE, &
                                    SNOW_SURFACE, &
                                    ICE_SURFACE, &
                                    SURFACE_TYPE_NAME
  USE CRTM_RTSolution_Define, ONLY: CRTM_RTSolution_type
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! Parameters
  PUBLIC :: FWD_OUTPUT
  PUBLIC :: TL_OUTPUT
  PUBLIC :: AD_OUTPUT
  PUBLIC :: K_OUTPUT
  ! Procedures
  PUBLIC :: Print_ChannelInfo
  PUBLIC :: Print_Results


  ! -----------------
  ! Module parameters
  ! -----------------
  INTEGER, PARAMETER :: FWD_OUTPUT = 1
  INTEGER, PARAMETER :: TL_OUTPUT  = 2
  INTEGER, PARAMETER :: AD_OUTPUT  = 3
  INTEGER, PARAMETER :: K_OUTPUT   = 4


CONTAINS


  ! ------------------
  ! PRIVATE procedures
  ! ------------------
  ! Function to open the test output file
  FUNCTION Open_TestFile(Filename,Status) RESULT(FileID)
    CHARACTER(*), INTENT(IN) :: Filename
    CHARACTER(*), INTENT(IN) :: Status
    INTEGER :: FileID
    CHARACTER(6) :: Position
    INTEGER :: IO_Status

    Position='ASIS'
    IF ( TRIM(Status) == 'OLD' ) Position='APPEND'
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
                           ChannelInfo%Sensor_Descriptor( l ), &
                           ChannelInfo%NCEP_Sensor_ID( l ), &
                           ChannelInfo%WMO_Satellite_ID( l ), &
                           ChannelInfo%WMO_Sensor_ID( l ), &
                           ChannelInfo%Sensor_Channel( l )
      WRITE( FileID, 300 ) ChannelInfo%Channel_Index( l ), &
                           ChannelInfo%Sensor_Descriptor( l ), &
                           ChannelInfo%NCEP_Sensor_ID( l ), &
                           ChannelInfo%WMO_Satellite_ID( l ), &
                           ChannelInfo%WMO_Sensor_ID( l ), &
                           ChannelInfo%Sensor_Channel( l )
    END DO
    CLOSE( FileID )

    ! Format statements
    100 FORMAT( /5x, 'Number of channels indexed: ', i5 )
    200 FORMAT( /2x, 'Channel         Sensor             NCEP          WMO           WMO     Channel', &
               &/2x, ' Index        Descriptor         Sensor ID   Satellite ID   Sensor ID   Number', &
               &/2x, '------------------------------------------------------------------------------'  )
    300 FORMAT( 2x, 2x, i4, 2x, '>', a, '<', 5x, i3, 11x, i3, 11x, i3, 7x, i4 )

  END SUBROUTINE Print_ChannelInfo


  ! Procedure to output some CRTM run results
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
     '(4x,i4,2x,">",a,"<",3x,f10.6,5x,f10.6,5x,f8.6)'

    CHARACTER(*), PARAMETER :: TL_HDR_FMT = &
     '(/2x,"Channel         Sensor            Radiance          Tb    '//&
     '     Surface     Emissivity_TL   Radiance_TL       Tb_TL  ",'//&
     ' /2x," Index        Descriptor                                  '//&
     '    Emissivity",'//&
     ' /2x,"----------------------------------------------------------'//&
     '-----------------------------------------------------------")'
    CHARACTER(*), PARAMETER :: TL_FMT = &
     '(4x,i4,2x,">",a,"<",3x,f10.6,5x,f10.6,5x,f8.6,4x,f9.6,5x,f10.6,5x,f10.6 )'

    CHARACTER(*), PARAMETER :: AD_HDR_FMT = &
     '( /2x,"Channel         Sensor            Radiance          Tb       '//&
     '  Surface     Emissivity_AD               Tsfc_AD",'//&
     '  /2x," Index        Descriptor                                     '//&
     ' Emissivity                     Land   Water    Snow    Ice ",'//&
     '  /2x,"-------------------------------------------------------------'//&
     '--------------------------------------------------------------")'
    CHARACTER(*), PARAMETER :: AD_FMT = &
     '(4x,i4,2x,">",a,"<",3x,f10.6,5x,f10.6,5x,f8.6,5x,f10.6,5x,4f8.4)'

    CHARACTER(*), PARAMETER :: K_HDR_FMT = &
     '(/2x,"Channel         Sensor            Radiance          Tb       '//&
     '  Surface     Emissivity_K                Tsfc_K",'//&
     ' /2x," Index        Descriptor                                     '//&
     ' Emissivity                      Land    Water   Snow    Ice",'//&
     ' /2x,"-------------------------------------------------------------'//&
     '--------------------------------------------------------------")'
    CHARACTER(*), PARAMETER :: K_FMT = &
     '(4x,i4,2x,">",a,"<",3x,f10.6,5x,f10.6,5x,f8.6,5x,f10.6,5x,4f8.4)'
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
                                      ChannelInfo%Sensor_Descriptor( l ), &
                                      RTSolution(l,m)%Radiance, &
                                      RTSolution(l,m)%Brightness_Temperature, &
                                      RTSolution(l,m)%Surface_Emissivity
          END DO FWD_Channel_Loop

        ! Tangent-linear model output
        CASE (TL_OUTPUT)
          WRITE(FileID, FMT=TL_HDR_FMT)
          TL_Channel_Loop: DO l = 1, SIZE( RTSolution, 1 )
            WRITE(FileID,FMT=TL_FMT) ChannelInfo%Channel_Index( l ), &
                                     ChannelInfo%Sensor_Descriptor( l ), &
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
                                     ChannelInfo%Sensor_Descriptor( l ), &
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
                                    ChannelInfo%Sensor_Descriptor( l ), &
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

END MODULE CRTM_Test_Utility
