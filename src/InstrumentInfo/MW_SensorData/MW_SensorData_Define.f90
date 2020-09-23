
!  MW_SensorData_Define
!
!  Module defining the MW_SensorData data structure and containing
!  routines to manipulate it.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 03-Jan-2003
!                       paul.vandelst@noaa.gov
!

MODULE MW_SensorData_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
  USE SensorInfo_Parameters, ONLY: XSAT => INVALID_WMO_SATELLITE_ID, &
                                   XSEN => INVALID_WMO_SENSOR_ID   , &
                                   INVALID_POLARIZATION    , &
                                   UNPOLARIZED             , &
                                   INTENSITY               , &
                                   FIRST_STOKES_COMPONENT  , &
                                   SECOND_STOKES_COMPONENT , &
                                   THIRD_STOKES_COMPONENT  , &
                                   FOURTH_STOKES_COMPONENT , &
                                   VL_POLARIZATION         , &
                                   HL_POLARIZATION         , &
                                   plus45L_POLARIZATION    , &
                                   minus45L_POLARIZATION   , &
                                   VL_MIXED_POLARIZATION   , &
                                   HL_MIXED_POLARIZATION   , &
                                   RC_POLARIZATION         , &
                                   LC_POLARIZATION         , &
                                   POLARIZATION_TYPE_NAME
  ! Disable implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: MW_SensorData_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Procedures
  PUBLIC :: MW_SensorData_Associated
  PUBLIC :: MW_SensorData_Destroy
  PUBLIC :: MW_SensorData_Create
  PUBLIC :: MW_SensorData_Inspect
  PUBLIC :: MW_SensorData_ValidRelease
  PUBLIC :: MW_SensorData_Info
  PUBLIC :: MW_SensorData_DefineVersion
  PUBLIC :: MW_SensorData_Load
  PUBLIC :: MW_SensorData_Get_Sensor_Id
  

  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE MW_SensorData_Equal
  END INTERFACE OPERATOR(==)


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Default message string length
  INTEGER, PARAMETER :: ML = 512
  ! Sensor id string length
  INTEGER, PARAMETER :: SL = 20
  ! Current valid release and version numbers
  INTEGER, PARAMETER :: MW_SENSORDATA_RELEASE = 1
  INTEGER, PARAMETER :: MW_SENSORDATA_VERSION = 1
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO   = 0.0_fp
  REAL(fp), PARAMETER ::  ONE   = 1.0_fp
  REAL(fp), PARAMETER ::  TWO   = 2.0_fp
  REAL(fp), PARAMETER :: POINT5 = 0.5_fp
  ! MW_SensorData valid values
  INTEGER, PARAMETER :: INVALID = -1
  INTEGER, PARAMETER ::   VALID =  1
  ! Total number of points per channel for computing the
  ! channel frequencies and responses. Must be evenly
  ! divisible by 2 and 4.
  INTEGER, PARAMETER :: DEFAULT_N_FREQUENCIES = 256


  ! ----------------------------------
  ! MW_SensorData data type definition
  ! ----------------------------------
  TYPE :: MW_SensorData_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Release and version information
    INTEGER :: Release = MW_SENSORDATA_RELEASE
    INTEGER :: Version = MW_SENSORDATA_VERSION
    ! Dimensions
    INTEGER :: n_Frequencies = 0  ! Lm
    INTEGER :: n_Channels    = 0  ! L
    ! Sensor IDs
    CHARACTER(SL) :: Sensor_ID        = ''
    INTEGER       :: WMO_Satellite_ID = XSAT
    INTEGER       :: WMO_Sensor_ID    = XSEN   
    ! Sensor data
    INTEGER , ALLOCATABLE :: Sensor_Channel(:)     ! L
    INTEGER , ALLOCATABLE :: Zeeman(:)             ! L
    INTEGER , ALLOCATABLE :: Polarization(:)       ! L
    INTEGER , ALLOCATABLE :: n_Sidebands(:)        ! L
    REAL(fp), ALLOCATABLE :: Central_Frequency(:)  ! L
    REAL(fp), ALLOCATABLE :: IF_Band(:,:,:)        ! 2 x 2 x L
    REAL(fp), ALLOCATABLE :: Delta_Frequency(:)    ! L
    REAL(fp), ALLOCATABLE :: Frequency(:,:)        ! Lm x L
    REAL(fp), ALLOCATABLE :: Response(:,:)         ! Lm x L
  END TYPE MW_SensorData_type


  ! ----------------
  ! Module variables
  ! ----------------
  INTEGER, PRIVATE :: i


  !#----------------------------------------------------------------------------#
  !                              Sensor Id data
  !#----------------------------------------------------------------------------#

  INTEGER, PARAMETER :: N_VALID_SENSORS = 67

  CHARACTER(*), PARAMETER :: VALID_SENSOR_ID(N_VALID_SENSORS) = &
  [ 'msu_tirosn          ','msu_n06             ','msu_n07             ','msu_n08             ',&
    'msu_n09             ','msu_n10             ','msu_n11             ','msu_n12             ',&
    'msu_n14             ','amsua_n15           ','amsua_n16           ','amsua_n17           ',&
    'amsub_n15           ','amsub_n16           ','amsub_n17           ','ssmi_f13            ',&
    'ssmi_f14            ','ssmi_f15            ','ssmt1_f13           ','ssmt1_f14           ',&
    'ssmt1_f15           ','ssmt2_f13           ','ssmt2_f14           ','ssmt2_f15           ',&
    'ssmis_f16           ','amsua_aqua          ','hsb_aqua            ','amsre_aqua          ',&
    'amsua_n18           ','mhs_n18             ','windsat_coriolis    ','atms_npp            ',&
    'amsua_n19           ','mhs_n19             ','amsua_metop-a       ','mhs_metop-a         ',&
    'amsua_metop-b       ','mhs_metop-b         ','amsua_metop-c       ','mhs_metop-c         ',&
    'ssmi_f08            ','ssmi_f10            ','ssmi_f11            ','mwri_fy3a           ',&
    'mwri_fy3b           ','mwhs_fy3a           ','mwhs_fy3b           ','mwts_fy3a           ',&
    'mwts_fy3b           ','tmi_trmm            ','gmi_gpm             '                       ,&
    'ssmis_f17           ','ssmis_f18           ','ssmis_f19           ','ssmis_f20           ',&
    'madras_meghat       ','saphir_meghat       ','amsr2_gcom-w1       ','hamsr_grip          ',&
    'micromas_cs00       ','micromas_cs01       ','micromas_cs02       ','micromas_cs03       ',&
    'micromas_cs04       ','micromas_cs05       ','geostorm_proposed   ','masc_cubesat        ' ]
  
  INTEGER, PARAMETER :: VALID_WMO_SATELLITE_ID(N_VALID_SENSORS) = &
  [  708, 706, 707, 200, 201, 202, 203, 204, 205, &         ! TIROS-N to NOAA-14 MSU (no NOAA-13)
     206, 207, 208, 206, 207, 208, &                        ! NOAA-15 to -17 AMSU-A; AMSU-B
     246, 247, 248, 246, 247, 248, 246, 247, 248, &         ! DMSP-13 to -15 SSM/I; SSM/T-1; SSM/T-2
     249, &                                                 ! DMSP-16 SSMIS
     784, 784, 784, &                                       ! AQUA AMSU-A, HSB, and AMSR-E
     209, 209, &                                            ! NOAA-18 AMSU-A and MHS
     283, &                                                 ! Coriolis WindSat
     224, &                                                 ! NPP ATMS
     210, 210, &                                            ! NOAA-N' AMSU-A and MHS (GUESS. NOT LISTED IN C-5)
     4, 4, 3, 3, 5, 5, &                                    ! MetOp-A - C AMSU-A; MHS
     241, 243, 244, &                                       ! DMSP-08,-10,-11 SSM/I
     520, 521, 520, 521, 520, 521, &                        ! Fengyun-3A/3B MWRI, MWHS, MWTS
     282, 288, &                                            ! TRMM TMI; GPM GMI
     285, 286, &                                            ! DMSP-17,18
     287, XSAT, &                                           ! DMSP-19,20
     440, 440, &                                            ! Megha-Tropiques MADRAS; SAPHIR
     122, &                                                 ! GCOM-W1 AMSR2
     XSAT, &                                                ! Hamsr-Grip, GRIP aircraft experiment
     XSAT, XSAT, XSAT, XSAT, XSAT, XSAT, &                  ! MicroMAS CubeSat-00 to -05
     XSAT, &                                                ! GeoStorm
     XSAT ]                                                 ! MASC CubeSat (different from MicroMAS)

  INTEGER, PARAMETER :: VALID_WMO_SENSOR_ID(N_VALID_SENSORS) = &
  [  623, 623, 623, 623, 623, 623, 623, 623, 623, &  ! TIROS-N to NOAA-14 MSU (no NOAA-13)
     570, 570, 570, 574, 574, 574, &                 ! NOAA-15 to -17 AMSU-A; AMSU-B
     905, 905, 905, 906, 906, 906, 907, 907, 907, &  ! DMSP-13 to -15 SSM/I; SSM/T-1; SSM/T-2
     908, &                                          ! DMSP-16 SSMIS
     570, 246, 479, &                                ! AQUA AMSU-A, HSB, and AMSR-E
     570, 203, &                                     ! NOAA-18 AMSU-A and MHS
     283, &                                          ! Coriolis WindSat
     621, &                                          ! NPP ATMS
     570, 203, &                                     ! NOAA-N' AMSU-A and MHS
     570, 203, 570, 203, 570, 203, &                 ! MetOp-A - C AMSU-A; MHS
     905, 905, 905, &                                ! DMSP-08,-10,-11 SSM/I
     938, 938, 936, 936, XSEN, XSEN, &               ! Fengyun 3A/3B MWRI, MWHS, MWTS
     365, 519, &                                     ! TRMM TMI; GPM GMI
     908, 908, &                                     ! DMSP-17,18 SSMIS
     908, 908, &                                     ! DMSP-19,20 SSMIS
     942, 941, &                                     ! Megha-Tropiques MADRAS; SAPHIR
     478, &                                          ! GCOM-W1 AMSR2
     XSEN, &                                         ! Hamsr-Grip, GRIP aircraft experiment
     XSEN, XSEN, XSEN, XSEN, XSEN, XSEN, &           ! MicroMAS CubeSat-00 to -05
     XSEN, &                                         ! GeoStorm
     XSEN ]                                          ! MASC CubeSat (different from MicroMAS)


  !#----------------------------------------------------------------------------#
  !                             Sensor channel data
  !#----------------------------------------------------------------------------#

  ! The number of channels for each included instrument
  INTEGER, PARAMETER :: N_MSU_CHANNELS     =  4
  INTEGER, PARAMETER :: N_SSMI_CHANNELS    =  7
  INTEGER, PARAMETER :: N_SSMT1_CHANNELS   =  7
  INTEGER, PARAMETER :: N_SSMT2_CHANNELS   =  5
  INTEGER, PARAMETER :: N_AMSUA_CHANNELS   = 15
  INTEGER, PARAMETER :: N_AMSUB_CHANNELS   =  5
  INTEGER, PARAMETER :: N_HSB_CHANNELS     =  4
  INTEGER, PARAMETER :: N_SSMIS_CHANNELS   = 24
  INTEGER, PARAMETER :: N_MHS_CHANNELS     =  5
  INTEGER, PARAMETER :: N_AMSRE_CHANNELS   = 12
  INTEGER, PARAMETER :: N_WINDSAT_CHANNELS = 16
  INTEGER, PARAMETER :: N_ATMS_CHANNELS    = 22
  INTEGER, PARAMETER :: N_MWRI_CHANNELS    = 10
  INTEGER, PARAMETER :: N_MWHS_CHANNELS    =  5
  INTEGER, PARAMETER :: N_MWTS_CHANNELS    =  4
  INTEGER, PARAMETER :: N_TMI_CHANNELS     =  9
  INTEGER, PARAMETER :: N_GMI_CHANNELS     = 13
  INTEGER, PARAMETER :: N_MADRAS_CHANNELS  =  9
  INTEGER, PARAMETER :: N_SAPHIR_CHANNELS  =  6
  INTEGER, PARAMETER :: N_AMSR2_CHANNELS   = 14
  INTEGER, PARAMETER :: N_HAMSR_CHANNELS   = 25
  INTEGER, PARAMETER :: N_MICROMAS_CHANNELS = 10
  INTEGER, PARAMETER :: N_GEOSTORM_CHANNELS = 10
  INTEGER, PARAMETER :: N_MASC_CHANNELS     =  8
 
  ! The number of channels for the valid sensors
  INTEGER, PARAMETER :: VALID_N_CHANNELS(N_VALID_SENSORS) = &
  [ N_MSU_CHANNELS, N_MSU_CHANNELS, N_MSU_CHANNELS, N_MSU_CHANNELS, & ! TIROS-N to NOAA-08 MSU
    N_MSU_CHANNELS, N_MSU_CHANNELS, N_MSU_CHANNELS, N_MSU_CHANNELS, & ! NOAA-09 to -12 MSU
    N_MSU_CHANNELS, &                                                 ! NOAA-14 MSU (no NOAA-13)
    N_AMSUA_CHANNELS, N_AMSUA_CHANNELS, N_AMSUA_CHANNELS, &           ! NOAA-15 to -17 AMSU-A
    N_AMSUB_CHANNELS, N_AMSUB_CHANNELS, N_AMSUB_CHANNELS, &           ! NOAA-15 to -17 AMSU-B
    N_SSMI_CHANNELS,  N_SSMI_CHANNELS,  N_SSMI_CHANNELS,  &           ! DMSP-13 to -15 SSM/I
    N_SSMT1_CHANNELS, N_SSMT1_CHANNELS, N_SSMT1_CHANNELS, &           ! DMSP-13 to -15 SSM/T-1
    N_SSMT2_CHANNELS, N_SSMT2_CHANNELS, N_SSMT2_CHANNELS, &           ! DMSP-13 to -15 SSM/T-2
    N_SSMIS_CHANNELS, &                                               ! DMSP-16 SSMIS
    N_AMSUA_CHANNELS, N_HSB_CHANNELS, N_AMSRE_CHANNELS, &             ! AQUA AMSU-A, HSB, and AMSR-E
    N_AMSUA_CHANNELS, N_MHS_CHANNELS, &                               ! NOAA-18 AMSU-A and MHS
    N_WINDSAT_CHANNELS, &                                             ! Coriolis WindSat 
    N_ATMS_CHANNELS, &                                                ! NPOESS-NPP ATMS
    N_AMSUA_CHANNELS, N_MHS_CHANNELS, &                               ! NOAA-N' AMSU-A and MHS
    N_AMSUA_CHANNELS, N_MHS_CHANNELS, &                               ! MetOp-A AMSU-A and MHS
    N_AMSUA_CHANNELS, N_MHS_CHANNELS, &                               ! MetOp-B AMSU-A and MHS
    N_AMSUA_CHANNELS, N_MHS_CHANNELS, &                               ! MetOp-C AMSU-A and MHS
    N_SSMI_CHANNELS,  N_SSMI_CHANNELS,  N_SSMI_CHANNELS, &            ! DMSP-08,-10,-11 SSM/I
    N_MWRI_CHANNELS,  N_MWRI_CHANNELS, &                              ! Fengyun 3A to 3B MWRI   
    N_MWHS_CHANNELS,  N_MWHS_CHANNELS, &                              ! Fengyun 3A to 3B MWHS 
    N_MWTS_CHANNELS,  N_MWTS_CHANNELS, &                              ! Fengyun 3A to 3B MWTS
    N_TMI_CHANNELS, N_GMI_CHANNELS, &                                 ! TRMM TMI; GPM GMI
    N_SSMIS_CHANNELS, N_SSMIS_CHANNELS, N_SSMIS_CHANNELS, N_SSMIS_CHANNELS, & ! DMSP-17,18,19,20 SSMIS
    N_MADRAS_CHANNELS,N_SAPHIR_CHANNELS, &                            ! Megha-Tropiques MADRAS; SAPHIR
    N_AMSR2_CHANNELS, N_HAMSR_CHANNELS, &                             ! GCOM-W1 AMSR2, Hamsr-Grip
    N_MICROMAS_CHANNELS, N_MICROMAS_CHANNELS, N_MICROMAS_CHANNELS, &  ! CubeSat-00 to -02 MicroMAS
    N_MICROMAS_CHANNELS, N_MICROMAS_CHANNELS, N_MICROMAS_CHANNELS, &  ! CubeSat-03 to -05 MicroMAS
    N_GEOSTORM_CHANNELS, &                                            ! Proposed GeoStorm
    N_MASC_CHANNELS ]                                                 ! Proposed MASC
    
  ! The sensor channel numbers
  INTEGER, PARAMETER :: MSU_SENSOR_CHANNEL(N_MSU_CHANNELS)           =[(i,i=1,N_MSU_CHANNELS     )]
  INTEGER, PARAMETER :: AMSUA_SENSOR_CHANNEL(N_AMSUA_CHANNELS)       =[(i,i=1,N_AMSUA_CHANNELS   )]
  INTEGER, PARAMETER :: AMSUB_SENSOR_CHANNEL(N_AMSUB_CHANNELS)       =[(i,i=1,N_AMSUB_CHANNELS   )]
  INTEGER, PARAMETER :: SSMI_SENSOR_CHANNEL(N_SSMI_CHANNELS)         =[(i,i=1,N_SSMI_CHANNELS    )]
  INTEGER, PARAMETER :: SSMT1_SENSOR_CHANNEL(N_SSMT1_CHANNELS)       =[(i,i=1,N_SSMT1_CHANNELS   )]
  INTEGER, PARAMETER :: SSMT2_SENSOR_CHANNEL(N_SSMT2_CHANNELS)       =[(i,i=1,N_SSMT2_CHANNELS   )]
  INTEGER, PARAMETER :: SSMIS_SENSOR_CHANNEL(N_SSMIS_CHANNELS)       =[(i,i=1,N_SSMIS_CHANNELS   )]
  INTEGER, PARAMETER :: HSB_SENSOR_CHANNEL(N_HSB_CHANNELS)           =[(i,i=1,N_HSB_CHANNELS     )]
  INTEGER, PARAMETER :: MHS_SENSOR_CHANNEL(N_MHS_CHANNELS)           =[(i,i=1,N_MHS_CHANNELS     )]
  INTEGER, PARAMETER :: AMSRE_SENSOR_CHANNEL(N_AMSRE_CHANNELS)       =[(i,i=1,N_AMSRE_CHANNELS   )]
  INTEGER, PARAMETER :: WINDSAT_SENSOR_CHANNEL(N_WINDSAT_CHANNELS)   =[(i,i=1,N_WINDSAT_CHANNELS )]
  INTEGER, PARAMETER :: ATMS_SENSOR_CHANNEL(N_ATMS_CHANNELS)         =[(i,i=1,N_ATMS_CHANNELS    )]
  INTEGER, PARAMETER :: MWRI_SENSOR_CHANNEL(N_MWRI_CHANNELS)         =[(i,i=1,N_MWRI_CHANNELS    )]
  INTEGER, PARAMETER :: MWHS_SENSOR_CHANNEL(N_MWHS_CHANNELS)         =[(i,i=1,N_MWHS_CHANNELS    )]
  INTEGER, PARAMETER :: MWTS_SENSOR_CHANNEL(N_MWTS_CHANNELS)         =[(i,i=1,N_MWTS_CHANNELS    )]
  INTEGER, PARAMETER :: TMI_SENSOR_CHANNEL(N_TMI_CHANNELS )          =[(i,i=1,N_TMI_CHANNELS     )]
  INTEGER, PARAMETER :: GMI_SENSOR_CHANNEL(N_GMI_CHANNELS )          =[(i,i=1,N_GMI_CHANNELS     )]
  INTEGER, PARAMETER :: MADRAS_SENSOR_CHANNEL(N_MADRAS_CHANNELS )    =[(i,i=1,N_MADRAS_CHANNELS  )]
  INTEGER, PARAMETER :: SAPHIR_SENSOR_CHANNEL(N_SAPHIR_CHANNELS )    =[(i,i=1,N_SAPHIR_CHANNELS  )]
  INTEGER, PARAMETER :: AMSR2_SENSOR_CHANNEL(N_AMSR2_CHANNELS)       =[(i,i=1,N_AMSR2_CHANNELS   )]
  INTEGER, PARAMETER :: HAMSR_SENSOR_CHANNEL(N_HAMSR_CHANNELS )      =[(i,i=1,N_HAMSR_CHANNELS   )]
  INTEGER, PARAMETER :: MICROMAS_SENSOR_CHANNEL(N_MICROMAS_CHANNELS) =[(i,i=1,N_MICROMAS_CHANNELS)]
  INTEGER, PARAMETER :: GEOSTORM_SENSOR_CHANNEL(N_GEOSTORM_CHANNELS) =[(i,i=1,N_GEOSTORM_CHANNELS)]
  INTEGER, PARAMETER :: MASC_SENSOR_CHANNEL(N_MASC_CHANNELS)         =[(i,i=1,N_MASC_CHANNELS    )]


  !#----------------------------------------------------------------------------#
  !                          Sideband numbering data
  !#----------------------------------------------------------------------------#

  ! The maximum number of sidebands
  INTEGER, PARAMETER :: MAX_N_SIDEBANDS = 2

  ! The channel sideband numbers
  INTEGER, PARAMETER :: MSU_N_SIDEBANDS(N_MSU_CHANNELS)        =1
  INTEGER, PARAMETER :: AMSUA_N_SIDEBANDS(N_AMSUA_CHANNELS)    =(/ 1, 1, 1, 1, 1, &
                                                                   1, 1, 1, 1, 1, &
                                                                   2, 2, 2, 2, &
                                                                   1 /)
  INTEGER, PARAMETER :: AMSUB_N_SIDEBANDS(N_AMSUB_CHANNELS)    =1
  INTEGER, PARAMETER :: SSMI_N_SIDEBANDS(N_SSMI_CHANNELS)      =1
  INTEGER, PARAMETER :: SSMT1_N_SIDEBANDS(N_SSMT1_CHANNELS)    =1
  INTEGER, PARAMETER :: SSMT2_N_SIDEBANDS(N_SSMT2_CHANNELS)    =1
  INTEGER, PARAMETER :: SSMIS_N_SIDEBANDS(N_SSMIS_CHANNELS)    =(/ 1, 1, 1, 1, 1, &
                                                                   1, 1, 1, 1, 1, &
                                                                   1, 1, 1, 1, 1, &
                                                                   1, 1, 1, 1, 1, &
                                                                   2, 2, 2, 2 /)
  INTEGER, PARAMETER :: HSB_N_SIDEBANDS(N_HSB_CHANNELS)        =1
  INTEGER, PARAMETER :: MHS_N_SIDEBANDS(N_MHS_CHANNELS)        =1
  INTEGER, PARAMETER :: AMSRE_N_SIDEBANDS(N_AMSRE_CHANNELS)    =1
  INTEGER, PARAMETER :: WINDSAT_N_SIDEBANDS(N_WINDSAT_CHANNELS)=1
  INTEGER, PARAMETER :: ATMS_N_SIDEBANDS(N_ATMS_CHANNELS)      =(/ 1, 1, 1, 1, 1, 1, &
                                                                   1, 1, 1, 1, 1, &
                                                                   2, 2, 2, 2, &
                                                                   1, 1, 1, 1, 1, 1, 1 /)
  INTEGER, PARAMETER :: MWRI_N_SIDEBANDS(N_MWRI_CHANNELS) = 1
  INTEGER, PARAMETER :: MWHS_N_SIDEBANDS(N_MWHS_CHANNELS) = 1
  INTEGER, PARAMETER :: MWTS_N_SIDEBANDS(N_MWTS_CHANNELS) = 1
  INTEGER, PARAMETER :: TMI_N_SIDEBANDS(N_TMI_CHANNELS) = 1
  INTEGER, PARAMETER :: GMI_N_SIDEBANDS(N_GMI_CHANNELS) = 1
  INTEGER, PARAMETER :: MADRAS_N_SIDEBANDS(N_MADRAS_CHANNELS) = 1
  INTEGER, PARAMETER :: SAPHIR_N_SIDEBANDS(N_SAPHIR_CHANNELS) = 1
  INTEGER, PARAMETER :: AMSR2_N_SIDEBANDS(N_AMSR2_CHANNELS)   = 1
  INTEGER, PARAMETER :: HAMSR_N_SIDEBANDS(N_HAMSR_CHANNELS)   = 1
  INTEGER, PARAMETER :: MICROMAS_N_SIDEBANDS(N_MICROMAS_CHANNELS) = 1
  INTEGER, PARAMETER :: GEOSTORM_N_SIDEBANDS(N_GEOSTORM_CHANNELS) = 1
  INTEGER, PARAMETER :: MASC_N_SIDEBANDS(N_MASC_CHANNELS)         = 1
 

  !#----------------------------------------------------------------------------#
  !                          Zeeman-affected channel flag
  !#----------------------------------------------------------------------------#

  INTEGER, PARAMETER :: NO_ZEEMAN = 0
  INTEGER, PARAMETER :: AMSUA_ZEEMAN(N_AMSUA_CHANNELS)    =(/ 0, 0, 0, 0, 0, &
                                                              0, 0, 0, 0, 0, &
                                                              0, 0, 0, 1, 0 /)
  INTEGER, PARAMETER :: SSMIS_ZEEMAN(N_SSMIS_CHANNELS)    =(/ 0, 0, 0, 0, 0, &
                                                              0, 0, 0, 0, 0, &
                                                              0, 0, 0, 0, 0, &
                                                              0, 0, 0, 1, 1, &
                                                              1, 1, 1, 1    /)

 
  !#----------------------------------------------------------------------------#
  !                           Sensor frequency data
  !#----------------------------------------------------------------------------#

  ! TIROS-N->NOAA-14 MSU
  ! --------------------
  ! Central frequency
  REAL(fp), PARAMETER :: MSU_F0(N_MSU_CHANNELS) = &
    (/ 50.30_fp, 53.74_fp, 54.96_fp, 57.95_fp /)
  ! MSU I/F band limits in GHz. All channel bandwidths are 220MHz
  ! with a 20MHz stopband at the local oscillator frequency
  REAL(fp), PARAMETER :: MSU_IF_BAND( 2, MAX_N_SIDEBANDS, N_MSU_CHANNELS ) = &
    RESHAPE( (/ 0.01_fp, 0.1_fp, ZERO, ZERO, &    ! MSU ch1
                0.01_fp, 0.1_fp, ZERO, ZERO, &    ! MSU ch2
                0.01_fp, 0.1_fp, ZERO, ZERO, &    ! MSU ch3
                0.01_fp, 0.1_fp, ZERO, ZERO /), & ! MSU ch4
             (/ 2, MAX_N_SIDEBANDS, N_MSU_CHANNELS /) )


  ! NOAA-15 AMSU-A
  ! --------------
  ! AMSU-A central frequencies in GHz
  REAL(fp), PARAMETER :: AMSUA_N15_F0( N_AMSUA_CHANNELS ) = &
    (/ 23.800370_fp, 31.400420_fp, 50.299910_fp, &
       52.799390_fp, 53.595410_fp, 54.399530_fp, &
       54.940640_fp, 55.498700_fp, 57.290329_fp, &
       57.290329_fp, 57.290329_fp, 57.290329_fp, &
       57.290329_fp, 57.290329_fp, 88.997000_fp /)

  ! AMSU-A I/F band limits in GHz. These are the 3dB bandpass
  ! filter frequencies from the AMSU-A2 and AMSU-A1 Aerojet
  ! calibration logbooks for AMSU-A1 S/N 103 and AMSU-A2
  ! S/N 102
  REAL(fp), PARAMETER :: AMSUA_N15_IF_BAND( 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS ) = &
    RESHAPE( (/ 0.00850_fp,  0.13551_fp,  ZERO,        ZERO,        &     ! AMSU-A2 ch1
                0.00897_fp,  0.08957_fp,  ZERO,        ZERO,        &     ! AMSU-A2 ch2
                0.00895_fp,  0.08952_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch3
                0.00890_fp,  0.19916_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch4
                0.03110_fp,  0.19930_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch5
                0.00894_fp,  0.19921_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch6
                0.00891_fp,  0.19919_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch7
                0.00891_fp,  0.16408_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch8
                0.00891_fp,  0.16412_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch9
                0.17916_fp,  0.25574_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch10
                0.257435_fp, 0.292545_fp, 0.353570_fp, 0.387850_fp, &     ! AMSU-A1 ch11
                0.292745_fp, 0.308035_fp, 0.337050_fp, 0.352130_fp, &     ! AMSU-A1 ch12
                0.308315_fp, 0.316245_fp, 0.328320_fp, 0.336240_fp, &     ! AMSU-A1 ch13
                0.316200_fp, 0.319120_fp, 0.325370_fp, 0.328310_fp, &     ! AMSU-A1 ch14
                0.49871_fp,  1.49820_fp,  ZERO,        ZERO         /), & ! AMSU-A1 ch15
             (/ 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS /) )


  ! NOAA-15 AMSU-B
  ! --------------
  ! AMSU-B central frequencies in GHz
  REAL(fp), PARAMETER :: AMSUB_N15_F0( N_AMSUB_CHANNELS ) = &
    (/  88.992_fp, 149.992_fp, 183.312_fp, &
       183.312_fp, 183.312_fp /)

  ! AMSU-B I/F band limits in GHz. Ch's 18-20 numbers were supplied by
  ! Nigel Atkinson of the UKMO. The on-line User's Guide sideband 1
  ! centre frequencies for these channels were nominal rather than
  ! actual (this is being corrected. The correct sideband 1 centre
  ! frequencies are 1.0025, 3.0015 and 6.9375GHz)
  REAL(fp), PARAMETER :: AMSUB_N15_IF_BAND( 2, MAX_N_SIDEBANDS, N_AMSUB_CHANNELS ) = &
    RESHAPE( (/ 0.399_fp, 1.405_fp, ZERO, ZERO, &    ! AMSU-B ch16
                0.398_fp, 1.401_fp, ZERO, ZERO, &    ! AMSU-B ch17
                0.762_fp, 1.243_fp, ZERO, ZERO, &    ! AMSU-B ch18
                2.512_fp, 3.491_fp, ZERO, ZERO, &    ! AMSU-B ch19
                6.002_fp, 7.873_fp, ZERO, ZERO /), & ! AMSU-B ch20
             (/ 2, MAX_N_SIDEBANDS, N_AMSUB_CHANNELS /) )


  ! NOAA-16 AMSU-A
  ! --------------
  ! AMSU-A central frequencies in GHz
  REAL(fp), PARAMETER :: AMSUA_N16_F0( N_AMSUA_CHANNELS ) = &
    (/ 23.800110_fp, 31.400580_fp, 50.299700_fp, &
       52.800740_fp, 53.595470_fp, 54.399780_fp, &
       54.940770_fp, 55.499540_fp, 57.290324_fp, &
       57.290324_fp, 57.290324_fp, 57.290324_fp, &
       57.290324_fp, 57.290324_fp, 88.999800_fp /)

  ! AMSU-A I/F band limits in GHz. These are the 3dB bandpass
  ! filter frequencies from the AMSU-A2 and AMSU-A1 Aerojet
  ! calibration logbooks for AMSU-A1 S/N 102 and AMSU-A2
  ! S/N 103
  REAL(fp), PARAMETER :: AMSUA_N16_IF_BAND( 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS ) = &
    RESHAPE( (/ 0.00858_fp,  0.13403_fp,  ZERO,        ZERO,        &     ! AMSU-A2 ch1
                0.00887_fp,  0.08957_fp,  ZERO,        ZERO,        &     ! AMSU-A2 ch2
                0.00898_fp,  0.08956_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch3
                0.00891_fp,  0.19916_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch4
                0.03094_fp,  0.19920_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch5
                0.00892_fp,  0.19922_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch6
                0.00888_fp,  0.19919_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch7
                0.00894_fp,  0.16402_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch8
                0.00887_fp,  0.16417_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch9
                0.17876_fp,  0.25545_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch10
                0.257140_fp, 0.291780_fp, 0.353095_fp, 0.387585_fp, &     ! AMSU-A1 ch11
                0.292715_fp, 0.308025_fp, 0.336865_fp, 0.351875_fp, &     ! AMSU-A1 ch12
                0.308195_fp, 0.316165_fp, 0.328350_fp, 0.336250_fp, &     ! AMSU-A1 ch13
                0.316325_fp, 0.319255_fp, 0.325340_fp, 0.328240_fp, &     ! AMSU-A1 ch14
                0.49943_fp,  1.49645_fp,  ZERO,        ZERO         /), & ! AMSU-A1 ch15
             (/ 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS /) )


  ! NOAA-16 AMSU-B
  ! --------------
  ! AMSU-B central frequencies in GHz
  REAL(fp), PARAMETER :: AMSUB_N16_F0( N_AMSUB_CHANNELS ) = &
    (/  89.0056_fp, 150.0080_fp, 183.2990_fp, &
       183.2990_fp, 183.2990_fp /)

  ! AMSU-B I/F band limits in GHz.
  REAL(fp), PARAMETER :: AMSUB_N16_IF_BAND( 2, MAX_N_SIDEBANDS, N_AMSUB_CHANNELS ) = &
    RESHAPE( (/ 0.399_fp, 1.405_fp, ZERO, ZERO, &    ! AMSU-B ch16
                0.397_fp, 1.397_fp, ZERO, ZERO, &    ! AMSU-B ch17
                0.751_fp, 1.245_fp, ZERO, ZERO, &    ! AMSU-B ch18
                2.507_fp, 3.485_fp, ZERO, ZERO, &    ! AMSU-B ch19
                6.011_fp, 7.989_fp, ZERO, ZERO /), & ! AMSU-B ch20
             (/ 2, MAX_N_SIDEBANDS, N_AMSUB_CHANNELS /) )


  ! NOAA-17 AMSU-A
  ! --------------
  ! AMSU-A central frequencies in GHz
  REAL(fp), PARAMETER :: AMSUA_N17_F0( N_AMSUA_CHANNELS ) = &
    (/ 23.799210_fp, 31.399660_fp, 50.291801_fp, &
       52.800800_fp, 53.596001_fp, 54.400242_fp, &
       54.939819_fp, 55.499199_fp, 57.290344_fp, &
       57.290344_fp, 57.290344_fp, 57.290344_fp, &
       57.290344_fp, 57.290344_fp, 88.999802_fp /)

  ! AMSU-A I/F band limits in GHz. These are the 3dB bandpass
  ! filter frequencies from the AMSU-A2 and AMSU-A1 Aerojet
  ! calibration logbooks for AMSU-A1 S/N 104 and AMSU-A2
  ! S/N 104.
  REAL(fp), PARAMETER :: AMSUA_N17_IF_BAND( 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS ) = &
    RESHAPE( (/ 0.00845_fp,  0.13396_fp,  ZERO,        ZERO,        &     ! AMSU-A2 ch1
                0.00893_fp,  0.08951_fp,  ZERO,        ZERO,        &     ! AMSU-A2 ch2
                0.00894_fp,  0.08952_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch3
                0.00894_fp,  0.19915_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch4
                0.03108_fp,  0.19923_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch5
                0.00894_fp,  0.19920_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch6
                0.00893_fp,  0.19923_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch7
                0.00890_fp,  0.16400_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch8
                0.00897_fp,  0.16388_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch9
                0.17901_fp,  0.25566_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch10
                0.257115_fp, 0.291785_fp, 0.353100_fp, 0.387960_fp, &     ! AMSU-A1 ch11
                0.292805_fp, 0.308095_fp, 0.337015_fp, 0.352085_fp, &     ! AMSU-A1 ch12
                0.308190_fp, 0.316110_fp, 0.328305_fp, 0.336195_fp, &     ! AMSU-A1 ch13
                0.316300_fp, 0.319240_fp, 0.325260_fp, 0.328120_fp, &     ! AMSU-A1 ch14
                0.49978_fp,  1.49921_fp,  ZERO,        ZERO         /), & ! AMSU-A1 ch15
             (/ 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS /) )


  ! NOAA-17 AMSU-B
  ! --------------
  ! AMSU-B central frequencies in GHz
  REAL(fp), PARAMETER :: AMSUB_N17_F0( N_AMSUB_CHANNELS ) = &
    (/  89.002_fp, 149.984_fp, 183.299_fp, &
       183.299_fp, 183.299_fp /)

  ! AMSU-B I/F band limits in GHz.
  REAL(fp), PARAMETER :: AMSUB_N17_IF_BAND( 2, MAX_N_SIDEBANDS, N_AMSUB_CHANNELS ) = &
    RESHAPE( (/ 0.399_fp, 1.406_fp, ZERO, ZERO, &    ! AMSU-B ch16
                0.398_fp, 1.402_fp, ZERO, ZERO, &    ! AMSU-B ch17
                0.751_fp, 1.248_fp, ZERO, ZERO, &    ! AMSU-B ch18
                2.511_fp, 3.267_fp, ZERO, ZERO, &    ! AMSU-B ch19
                6.016_fp, 7.971_fp, ZERO, ZERO /), & ! AMSU-B ch20
             (/ 2, MAX_N_SIDEBANDS, N_AMSUB_CHANNELS /) )


  ! SSM/I
  ! Frequency information taken from Table 2.1
  ! "SSM/I Antenna Beamwidths (S/N 002)"
  ! in the Special Sensor Microwave/Imager User's
  ! Guide, NRL, 14-Sep-1987
  ! Note that the "repeated" frequencies are for
  ! different polarisations. That is, the SSM/I
  ! is a seven channel instrument, but only a 
  ! four frequency instrument.
  ! ---------------------------------------------
  ! Central frequency
  REAL(fp), PARAMETER :: SSMI_F0( N_SSMI_CHANNELS ) = &
    (/ 19.35_fp, 19.35_fp, &
       22.235_fp, &
       37.0_fp,  37.0_fp, &
       85.5_fp,  85.5_fp /)

  ! SSM/I I/F band limits in GHz.
  REAL(fp), PARAMETER :: SSMI_IF_BAND( 2, MAX_N_SIDEBANDS, N_SSMI_CHANNELS ) = &
    RESHAPE( (/ 0.010_fp, 0.250_fp, ZERO, ZERO, &    ! SSM/I ch1
                0.010_fp, 0.250_fp, ZERO, ZERO, &    ! SSM/I ch2
                0.010_fp, 0.250_fp, ZERO, ZERO, &    ! SSM/I ch3
                0.100_fp, 1.000_fp, ZERO, ZERO, &    ! SSM/I ch4
                0.100_fp, 1.000_fp, ZERO, ZERO, &    ! SSM/I ch5
                0.100_fp, 1.500_fp, ZERO, ZERO, &    ! SSM/I ch6
                0.100_fp, 1.500_fp, ZERO, ZERO /), & ! SSM/I ch7
             (/ 2, MAX_N_SIDEBANDS, N_SSMI_CHANNELS /) )


  ! SSM/T-1
  ! Frequency information taken from
  !   http://www.saa.noaa.gov/help/detail/DMSP/README.ssmt1
  ! The band WIDTHS are as stated in the above reference,
  ! but the I/F start values are simply set to 0.0 since
  ! there was no other info about this.
  ! -------------------------------------------------------
  ! Central frequency
  REAL(fp), PARAMETER :: SSMT1_F0( N_SSMT1_CHANNELS ) = &
    (/ 50.5_fp, 53.2_fp, 54.35_fp, &
       54.9_fp, 58.4_fp, &
       58.825_fp, &
       59.4_fp /)

  ! SSM/T-1 I/F band limits in GHz.
  REAL(fp), PARAMETER :: SSMT1_IF_BAND( 2, MAX_N_SIDEBANDS, N_SSMT1_CHANNELS ) = &
    RESHAPE( (/ 0.000_fp, 0.2000_fp, ZERO, ZERO, &    ! SSM/T-1 ch1
                0.000_fp, 0.2000_fp, ZERO, ZERO, &    ! SSM/T-1 ch2
                0.000_fp, 0.2000_fp, ZERO, ZERO, &    ! SSM/T-1 ch3
                0.000_fp, 0.2000_fp, ZERO, ZERO, &    ! SSM/T-1 ch4
                0.000_fp, 0.0575_fp, ZERO, ZERO, &    ! SSM/T-1 ch5
                0.000_fp, 0.2000_fp, ZERO, ZERO, &    ! SSM/T-1 ch6
                0.000_fp, 0.1250_fp, ZERO, ZERO /), & ! SSM/T-1 ch7
             (/ 2, MAX_N_SIDEBANDS, N_SSMT1_CHANNELS /) )


  ! SSM/T-2
  ! -------
  ! Central frequency
  REAL(fp), PARAMETER :: SSMT2_F0( N_SSMT2_CHANNELS )= &
    (/ 183.31000_fp, 183.31000_fp, 183.31000_fp, &
        91.66500_fp, 150.00000_fp /)

  ! SSM-T/2 I/F band limits in GHz.
  REAL(fp), PARAMETER :: SSMT2_IF_BAND( 2, MAX_N_SIDEBANDS, N_SSMT2_CHANNELS ) = &
    RESHAPE( (/ 2.500_fp, 3.500_fp, ZERO, ZERO, &    ! SSM-T/2 ch1
                0.750_fp, 1.250_fp, ZERO, ZERO, &    ! SSM-T/2 ch2
                6.250_fp, 7.750_fp, ZERO, ZERO, &    ! SSM-T/2 ch3
                0.500_fp, 2.000_fp, ZERO, ZERO, &    ! SSM-T/2 ch4
                0.500_fp, 2.000_fp, ZERO, ZERO /), & ! SSM-T/2 ch5
             (/ 2, MAX_N_SIDEBANDS, N_SSMT2_CHANNELS /) )


  ! DMSP-16 SSMIS
  !
  ! Derived from SN02 measured parameters in
  !   SSMIS Passband Characterizations for Forward Modeling
  !   CDRL No.A007
  !   Aerojet Report 11892
  !   January, 2001
  !
  ! Average bandpass values used for I/F limits
  ! -------------------------------------------------------
  ! Central frequency
  REAL(fp), PARAMETER :: SSMIS_F16_F0( N_SSMIS_CHANNELS ) = &
    (/ 50.300000_fp,  52.800000_fp,  53.596000_fp,  54.400000_fp, &
       55.500000_fp,  57.290000_fp,  59.400000_fp, 150.000000_fp, &
      183.310000_fp, 183.310000_fp, 183.310000_fp,  19.350000_fp, &
       19.350000_fp,  22.235000_fp,  37.000000_fp,  37.000000_fp, &
       91.655000_fp,  91.655000_fp,  63.283248_fp,  60.792668_fp, &
       60.792668_fp,  60.792668_fp,  60.792668_fp,  60.792668_fp /)

  ! SSMIS I/F band limits in GHz.
  REAL(fp), PARAMETER :: SSMIS_F16_IF_BAND( 2, MAX_N_SIDEBANDS, N_SSMIS_CHANNELS ) = &
    RESHAPE( (/ 0.00000000_fp,  0.19315000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 1
                0.00000000_fp,  0.19280000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 2
                0.00000000_fp,  0.18565000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 3
                0.00000000_fp,  0.18780000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 4
                0.00000000_fp,  0.19155000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 5
                0.00000000_fp,  0.16655000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 6
                0.00000000_fp,  0.11970000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 7
                0.42600000_fp,  2.07400000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 8
                5.83500000_fp,  7.36500000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 9
                2.49150000_fp,  3.50850000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.10
                0.74125000_fp,  1.25875000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.11
                0.00000000_fp,  0.17815000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.12
                0.00000000_fp,  0.17940000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.13
                0.00000000_fp,  0.21030000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.14
                0.00000000_fp,  0.78900000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.15
                0.00000000_fp,  0.77100000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.16
                0.18400000_fp,  1.61600000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.17
                0.19950000_fp,  1.60050000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.18
                0.28459660_fp,  0.28594540_fp,  ZERO         ,  ZERO         ,  &   ! Ch.19
                0.35721302_fp,  0.35857098_fp,  ZERO         ,  ZERO         ,  &   ! Ch.20
                0.35524737_fp,  0.35653662_fp,  0.35924737_fp,  0.36053662_fp,  &   ! Ch.21
                0.35107315_fp,  0.35371085_fp,  0.36207315_fp,  0.36471085_fp,  &   ! Ch.22
                0.33826547_fp,  0.34551852_fp,  0.37026548_fp,  0.37751852_fp,  &   ! Ch.23
                0.29465795_fp,  0.32112605_fp,  0.39465795_fp,  0.42112605_fp /), & ! Ch.24
             (/ 2, MAX_N_SIDEBANDS, N_SSMIS_CHANNELS /) )


  ! DMSP-17 SSMIS
  !
  ! Derived from SN04 measured parameters in
  !   SSMIS Passband Characterizations for Forward Modeling
  !   CDRL No.A007
  !   Aerojet Report 11892
  !   January, 2001
  !
  ! Average bandpass values used for I/F limits
  ! -------------------------------------------------------
  ! Central frequency
  REAL(fp), PARAMETER :: SSMIS_F17_F0( N_SSMIS_CHANNELS ) = &
    (/ 50.300000_fp,  52.800000_fp,  53.596000_fp,  54.400000_fp, &
       55.500000_fp,  57.290000_fp,  59.400000_fp, 150.000000_fp, &
      183.310000_fp, 183.310000_fp, 183.310000_fp,  19.350000_fp, &
       19.350000_fp,  22.235000_fp,  37.000000_fp,  37.000000_fp, &
       91.655000_fp,  91.655000_fp,  63.283248_fp,  60.792668_fp, &
       60.792668_fp,  60.792668_fp,  60.792668_fp,  60.792668_fp /)

  ! SSMIS I/F band limits in GHz.
  REAL(fp), PARAMETER :: SSMIS_F17_IF_BAND( 2, MAX_N_SIDEBANDS, N_SSMIS_CHANNELS ) = &
    RESHAPE( (/ 0.00000000_fp,  0.19095000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 1
                0.00000000_fp,  0.19780000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 2
                0.00000000_fp,  0.18595000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 3
                0.00000000_fp,  0.18815000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 4
                0.00000000_fp,  0.19625000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 5
                0.00000000_fp,  0.17000000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 6
                0.00000000_fp,  0.11750000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 7
                0.41800000_fp,  2.08200000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 8
                5.83500000_fp,  7.36500000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 9
                2.49250000_fp,  3.50750000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.10
                0.74075000_fp,  1.25925000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.11
                0.00000000_fp,  0.18970000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.12
                0.00000000_fp,  0.17815000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.13
                0.00000000_fp,  0.21865000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.14
                0.00000000_fp,  0.76150000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.15
                0.00000000_fp,  0.77400000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.16
                0.17750000_fp,  1.62250000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.17
                0.19500000_fp,  1.60500000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.18
                0.28459600_fp,  0.28594600_fp,  ZERO         ,  ZERO         ,  &   ! Ch.19
                0.35720200_fp,  0.35858200_fp,  ZERO         ,  ZERO         ,  &   ! Ch.20
                0.35526075_fp,  0.35652325_fp,  0.35926075_fp,  0.36052325_fp,  &   ! Ch.21
                0.35107700_fp,  0.35370700_fp,  0.36207700_fp,  0.36470700_fp,  &   ! Ch.22
                0.33819575_fp,  0.34558825_fp,  0.37019575_fp,  0.37758825_fp,  &   ! Ch.23
                0.29460325_fp,  0.32118075_fp,  0.39460325_fp,  0.42118075_fp /), & ! Ch.24
             (/ 2, MAX_N_SIDEBANDS, N_SSMIS_CHANNELS /) )


  ! DMSP-18 SSMIS
  !
  ! Derived from SN03 measured parameters in
  !   SSMIS Passband Characterizations for Forward Modeling
  !   CDRL No.A007
  !   Aerojet Report 11892
  !   January, 2001
  !
  ! Average bandpass values used for I/F limits
  ! -------------------------------------------------------
  ! Central frequency
  REAL(fp), PARAMETER :: SSMIS_F18_F0( N_SSMIS_CHANNELS ) = &
    (/ 50.300000_fp,  52.800000_fp,  53.596000_fp,  54.400000_fp, &
       55.500000_fp,  57.290000_fp,  59.400000_fp, 150.000000_fp, &
      183.310000_fp, 183.310000_fp, 183.310000_fp,  19.350000_fp, &
       19.350000_fp,  22.235000_fp,  37.000000_fp,  37.000000_fp, &
       91.655000_fp,  91.655000_fp,  63.283248_fp,  60.792668_fp, &
       60.792668_fp,  60.792668_fp,  60.792668_fp,  60.792668_fp /)

  ! SSMIS I/F band limits in GHz.
  REAL(fp), PARAMETER :: SSMIS_F18_IF_BAND( 2, MAX_N_SIDEBANDS, N_SSMIS_CHANNELS ) = &
    RESHAPE( (/ 0.00000000_fp,  0.19730000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 1
                0.00000000_fp,  0.19380000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 2
                0.00000000_fp,  0.18655000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 3
                0.00000000_fp,  0.18905000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 4
                0.00000000_fp,  0.19005000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 5
                0.00000000_fp,  0.16485000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 6
                0.00000000_fp,  0.11915000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 7
                0.42700000_fp,  2.07300000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 8
                5.83400000_fp,  7.36600000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 9
                2.48950000_fp,  3.51050000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.10
                0.74475000_fp,  1.25525000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.11
                0.00000000_fp,  0.19125000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.12
                0.00000000_fp,  0.19825000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.13
                0.00000000_fp,  0.22725000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.14
                0.00000000_fp,  0.78350000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.15
                0.00000000_fp,  0.77350000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.16
                0.17900000_fp,  1.62100000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.17
                0.18350000_fp,  1.61650000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.18
                0.28459775_fp,  0.28594425_fp,  ZERO         ,  ZERO         ,  &   ! Ch.19
                0.35720535_fp,  0.35857865_fp,  ZERO         ,  ZERO         ,  &   ! Ch.20
                0.35525507_fp,  0.35652892_fp,  0.35925507_fp,  0.36052893_fp,  &   ! Ch.21
                0.35106789_fp,  0.35371611_fp,  0.36206789_fp,  0.36471611_fp,  &   ! Ch.22
                0.33823812_fp,  0.34554587_fp,  0.37023812_fp,  0.37754587_fp,  &   ! Ch.23
                0.29457251_fp,  0.32121149_fp,  0.39457251_fp,  0.42121149_fp /), & ! Ch.24
             (/ 2, MAX_N_SIDEBANDS, N_SSMIS_CHANNELS /) )


  ! DMSP-19 SSMIS
  !
  ! Derived from SN05 measured parameters in
  !   SSMIS Passband Characterizations for Forward Modeling
  !   CDRL No.A007
  !   Aerojet Report 11892
  !   January, 2001
  !
  ! Average bandpass values used for I/F limits
  ! -------------------------------------------------------
  ! Central frequency
  REAL(fp), PARAMETER :: SSMIS_F19_F0( N_SSMIS_CHANNELS ) = &
    (/ 50.300000_fp,  52.800000_fp,  53.596000_fp,  54.400000_fp, &
       55.500000_fp,  57.290000_fp,  59.400000_fp, 150.000000_fp, &
      183.310000_fp, 183.310000_fp, 183.310000_fp,  19.350000_fp, &
       19.350000_fp,  22.235000_fp,  37.000000_fp,  37.000000_fp, &
       91.655000_fp,  91.655000_fp,  63.283248_fp,  60.792668_fp, &
       60.792668_fp,  60.792668_fp,  60.792668_fp,  60.792668_fp /)

  ! SSMIS I/F band limits in GHz.
  REAL(fp), PARAMETER :: SSMIS_F19_IF_BAND( 2, MAX_N_SIDEBANDS, N_SSMIS_CHANNELS ) = &
    RESHAPE( (/ 0.00000000_fp,  0.19155000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 1
                0.00000000_fp,  0.19780000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 2
                0.00000000_fp,  0.18780000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 3
                0.00000000_fp,  0.19125000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 4
                0.00000000_fp,  0.19655000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 5
                0.00000000_fp,  0.16780000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 6
                0.00000000_fp,  0.11940000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 7
                0.42100000_fp,  2.07900000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 8
                5.83000000_fp,  7.37000000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 9
                2.48850000_fp,  3.51150000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.10
                0.74100000_fp,  1.25900000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.11
                0.00000000_fp,  0.17805000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.12
                0.00000000_fp,  0.17780000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.13
                0.00000000_fp,  0.20190000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.14
                0.00000000_fp,  0.79900000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.15
                0.00000000_fp,  0.80000000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.16
                0.18300000_fp,  1.61700000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.17
                0.18600000_fp,  1.61400000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.18
                0.28459930_fp,  0.28594270_fp,  ZERO         ,  ZERO         ,  &   ! Ch.19
                0.35722100_fp,  0.35856300_fp,  ZERO         ,  ZERO         ,  &   ! Ch.20
                0.35524753_fp,  0.35653648_fp,  0.35924753_fp,  0.36053648_fp,  &   ! Ch.21
                0.35108279_fp,  0.35370121_fp,  0.36208279_fp,  0.36470121_fp,  &   ! Ch.22
                0.33824149_fp,  0.34554251_fp,  0.37024149_fp,  0.37754251_fp,  &   ! Ch.23
                0.29463834_fp,  0.32114566_fp,  0.39463834_fp,  0.42114566_fp /), & ! Ch.24
             (/ 2, MAX_N_SIDEBANDS, N_SSMIS_CHANNELS /) )


  ! DMSP-20 SSMIS
  !
  ! Derived from SN01 measured parameters in
  !   SSMIS Passband Characterizations for Forward Modeling
  !   CDRL No.A007
  !   Aerojet Report 11892
  !   January, 2001
  !
  ! Average bandpass values used for I/F limits
  ! -------------------------------------------------------
  ! Central frequency
  REAL(fp), PARAMETER :: SSMIS_F20_F0( N_SSMIS_CHANNELS ) = &
    (/ 50.300000_fp,  52.800000_fp,  53.596000_fp,  54.400000_fp, &
       55.500000_fp,  57.290000_fp,  59.400000_fp, 150.000000_fp, &
      183.310000_fp, 183.310000_fp, 183.310000_fp,  19.350000_fp, &
       19.350000_fp,  22.235000_fp,  37.000000_fp,  37.000000_fp, &
       91.655000_fp,  91.655000_fp,  63.283248_fp,  60.792668_fp, &
       60.792668_fp,  60.792668_fp,  60.792668_fp,  60.792668_fp /)

  ! SSMIS I/F band limits in GHz.
  REAL(fp), PARAMETER :: SSMIS_F20_IF_BAND( 2, MAX_N_SIDEBANDS, N_SSMIS_CHANNELS ) = &
    RESHAPE( (/ 0.00000000_fp,  0.19000000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 1
                0.00000000_fp,  0.19440000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 2
                0.00000000_fp,  0.19000000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 3
                0.00000000_fp,  0.19125000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 4
                0.00000000_fp,  0.19565000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 5
                0.00000000_fp,  0.16500000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 6
                0.00000000_fp,  0.11940000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 7
                0.42900000_fp,  2.07100000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 8
                5.83700000_fp,  7.36300000_fp,  ZERO         ,  ZERO         ,  &   ! Ch. 9
                2.49050000_fp,  3.50950000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.10
                0.74375000_fp,  1.25625000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.11
                0.00000000_fp,  0.17750000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.12
                0.00000000_fp,  0.17835000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.13
                0.00000000_fp,  0.20375000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.14
                0.00000000_fp,  0.80750000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.15
                0.00000000_fp,  0.77250000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.16
                0.19100000_fp,  1.60900000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.17
                0.19450000_fp,  1.60550000_fp,  ZERO         ,  ZERO         ,  &   ! Ch.18
                0.28459183_fp,  0.28595017_fp,  ZERO         ,  ZERO         ,  &   ! Ch.19
                0.35721420_fp,  0.35856980_fp,  ZERO         ,  ZERO         ,  &   ! Ch.20
                0.35524784_fp,  0.35653616_fp,  0.35924784_fp,  0.36053616_fp,  &   ! Ch.21
                0.35108007_fp,  0.35370392_fp,  0.36208008_fp,  0.36470393_fp,  &   ! Ch.22
                0.33823439_fp,  0.34554961_fp,  0.37023439_fp,  0.37754961_fp,  &   ! Ch.23
                0.29465119_fp,  0.32113281_fp,  0.39465119_fp,  0.42113281_fp /), & ! Ch.24
             (/ 2, MAX_N_SIDEBANDS, N_SSMIS_CHANNELS /) )


  ! Aqua AMSU-A (spec values)
  ! -------------------------
  ! AMSU-A central frequencies in GHz
  REAL(fp), PARAMETER :: AMSUA_AQUA_F0( N_AMSUA_CHANNELS ) = &
    (/ 23.800000_fp, 31.400000_fp, 50.300000_fp, &
       52.800000_fp, 53.596000_fp, 54.400000_fp, &
       54.940000_fp, 55.500000_fp, 57.290344_fp, &
       57.290344_fp, 57.290344_fp, 57.290344_fp, &
       57.290344_fp, 57.290344_fp, 89.000000_fp /)

  ! AMSU-A I/F band limits in GHz.
  REAL(fp), PARAMETER :: AMSUA_AQUA_IF_BAND( 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS ) = &
    RESHAPE( (/ 0.01000_fp,  0.13500_fp,  ZERO,        ZERO,        &     ! AMSU-A2 ch1
                0.01000_fp,  0.09000_fp,  ZERO,        ZERO,        &     ! AMSU-A2 ch2
                0.01000_fp,  0.09000_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch3
                0.01000_fp,  0.20000_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch4
                0.03000_fp,  0.20000_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch5
                0.01000_fp,  0.20000_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch6
                0.01000_fp,  0.20000_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch7
                0.01000_fp,  0.16500_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch8
                0.01000_fp,  0.16500_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch9
                0.17800_fp,  0.25600_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch10
                0.256200_fp, 0.292200_fp, 0.352200_fp, 0.388200_fp, &     ! AMSU-A1 ch11
                0.292200_fp, 0.308200_fp, 0.336200_fp, 0.352200_fp, &     ! AMSU-A1 ch12
                0.308200_fp, 0.316200_fp, 0.328200_fp, 0.336200_fp, &     ! AMSU-A1 ch13
                0.316200_fp, 0.319200_fp, 0.325200_fp, 0.328200_fp, &     ! AMSU-A1 ch14
                0.50000_fp,  1.50000_fp,  ZERO,        ZERO         /), & ! AMSU-A1 ch15
             (/ 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS /) )


  ! Aqua HSB (AMSU-B spec values)
  ! -----------------------------
  ! HSB central frequencies in GHz
  REAL(fp), PARAMETER :: HSB_AQUA_F0( N_HSB_CHANNELS ) = &
    (/ 150.000_fp, 183.310_fp, 183.310_fp, 183.310_fp /)

  ! HSB I/F band limits in GHz.
  REAL(fp), PARAMETER :: HSB_AQUA_IF_BAND( 2, MAX_N_SIDEBANDS, N_HSB_CHANNELS ) = &
    RESHAPE( (/ 0.400_fp, 1.400_fp, ZERO, ZERO, &    ! HSB ch1 == AMSU-B ch2
                0.750_fp, 1.250_fp, ZERO, ZERO, &    ! HSB ch2 == AMSU-B ch3
                2.500_fp, 3.500_fp, ZERO, ZERO, &    ! HSB ch3 == AMSU-B ch4
                6.000_fp, 8.000_fp, ZERO, ZERO /), & ! HSB ch4 == AMSU-B ch5
             (/ 2, MAX_N_SIDEBANDS, N_HSB_CHANNELS /) )


  ! Aqua AMSR-E
  ! Frequency information taken from Table I from
  !   Kawanishi, T. et al (2003), Advanced Microwave Scanning
  !     Radiometer for the Earth Observing System (AMSR-E),
  !     IEEE Transactions on Geoscience and Remote Sensing,
  !     41(2), pp184-194.
  !     doi 10.1109/TGRS.2002.808331
  ! It is assumed there are no stopbands in the same manner
  ! as other microwave instruments with no sidebands so f1
  ! values are all 0.000.
  !
  ! Note each frequency is listed twice. This is for separate
  ! channels with vertical and horizontal polarisations.
  ! -------------------------------------------------------
  ! AMSR-E central frequencies in GHz
  REAL(fp), PARAMETER :: AMSRE_AQUA_F0( N_AMSRE_CHANNELS ) = &
    (/ 6.925_fp,  6.925_fp, 10.650_fp, 10.650_fp, &
      18.700_fp, 18.700_fp, 23.800_fp, 23.800_fp, &
      36.500_fp, 36.500_fp, 89.000_fp, 89.000_fp /)

  ! AMSR-E I/F band limits in GHz.
  REAL(fp), PARAMETER :: AMSRE_AQUA_IF_BAND( 2, MAX_N_SIDEBANDS, N_AMSRE_CHANNELS ) = &
    RESHAPE( (/ 0.000_fp, 0.175_fp, ZERO, ZERO, &     ! 6.925GHz vertical pol.
                0.000_fp, 0.175_fp, ZERO, ZERO, &     ! 6.925GHz horizontal pol.
                0.000_fp, 0.050_fp, ZERO, ZERO, &     ! 10.65GHz vertical pol.
                0.000_fp, 0.050_fp, ZERO, ZERO, &     ! 10.65GHz horizontal pol.
                0.000_fp, 0.100_fp, ZERO, ZERO, &     ! 18.7GHz vertical pol.
                0.000_fp, 0.100_fp, ZERO, ZERO, &     ! 18.7GHz horizontal pol.
                0.000_fp, 0.200_fp, ZERO, ZERO, &     ! 23.8GHz vertical pol.
                0.000_fp, 0.200_fp, ZERO, ZERO, &     ! 23.8GHz horizontal pol.
                0.000_fp, 0.500_fp, ZERO, ZERO, &     ! 36.5GHz vertical pol.
                0.000_fp, 0.500_fp, ZERO, ZERO, &     ! 36.5GHz horizontal pol.
                0.000_fp, 1.500_fp, ZERO, ZERO, &     ! 89.0GHz vertical pol.
                0.000_fp, 1.500_fp, ZERO, ZERO /), &  ! 89.0GHz horizontal pol.
             (/ 2, MAX_N_SIDEBANDS, N_AMSRE_CHANNELS /) )

  ! GCOM-W1 AMSR2
  ! Frequency information taken from 
  ! Norimasa Ito January 18, 2012 "GCOM-W1 Status"
  ! Slide 6
  
  ! It is assumed there are no stopbands in the same manner
  ! as other microwave instruments with no sidebands so f1
  ! values are all 0.000.
  !
  ! Note each frequency is listed twice. This is for separate
  ! channels with vertical and horizontal polarisations.
  ! -------------------------------------------------------
  ! AMSR2 central frequencies in GHz
  REAL(fp), PARAMETER :: AMSR2_GCOMW1_F0( N_AMSR2_CHANNELS ) = &
    (/ 6.925_fp,  6.925_fp,  7.300_fp,  7.300_fp, &
      10.650_fp, 10.650_fp, 18.700_fp, 18.700_fp, &
      23.800_fp, 23.800_fp, 36.500_fp, 36.500_fp, &
      89.000_fp, 89.000_fp /)

  ! AMSR2 I/F band limits in GHz.
  REAL(fp), PARAMETER :: AMSR2_GCOMW1_IF_BAND( 2, MAX_N_SIDEBANDS, N_AMSR2_CHANNELS ) = &
    RESHAPE( (/ 0.000_fp, 0.175_fp, ZERO, ZERO, &     ! 6.925GHz vertical pol.
                0.000_fp, 0.175_fp, ZERO, ZERO, &     ! 6.925GHz horizontal pol.
                0.000_fp, 0.175_fp, ZERO, ZERO, &     ! 7.3GHz vertical pol.
                0.000_fp, 0.175_fp, ZERO, ZERO, &     ! 7.3GHz horizontal pol.
                0.000_fp, 0.050_fp, ZERO, ZERO, &     ! 10.65GHz vertical pol.
                0.000_fp, 0.050_fp, ZERO, ZERO, &     ! 10.65GHz horizontal pol.
                0.000_fp, 0.100_fp, ZERO, ZERO, &     ! 18.7GHz vertical pol.
                0.000_fp, 0.100_fp, ZERO, ZERO, &     ! 18.7GHz horizontal pol.
                0.000_fp, 0.200_fp, ZERO, ZERO, &     ! 23.8GHz vertical pol.
                0.000_fp, 0.200_fp, ZERO, ZERO, &     ! 23.8GHz horizontal pol.
                0.000_fp, 0.500_fp, ZERO, ZERO, &     ! 36.5GHz vertical pol.
                0.000_fp, 0.500_fp, ZERO, ZERO, &     ! 36.5GHz horizontal pol.
                0.000_fp, 1.500_fp, ZERO, ZERO, &     ! 89.0GHz vertical pol.
                0.000_fp, 1.500_fp, ZERO, ZERO /), &  ! 89.0GHz horizontal pol.
             (/ 2, MAX_N_SIDEBANDS, N_AMSR2_CHANNELS /) )


  ! NOAA-18 AMSU-A
  ! --------------
  ! AMSU-A central frequencies in GHz
  ! AMSU-A1 data (ch3-15) interpolated from measured data to 15C temperature
  ! AMSU-A1 ch9-14 values is for PLLO #1. PLLO #2 value is 57.290324GHz
  REAL(fp), PARAMETER :: AMSUA_N18_F0( N_AMSUA_CHANNELS ) = &
    (/ 23.800837_fp, 31.399518_fp, 50.299742_fp, &
       52.800072_fp, 53.595969_fp, 54.399819_fp, &
       54.940337_fp, 55.500255_fp, 57.290335_fp, &
       57.290335_fp, 57.290335_fp, 57.290335_fp, &
       57.290335_fp, 57.290335_fp, 89.009713_fp /)

  ! AMSU-A I/F band limits in GHz. These are the 3dB bandpass
  ! filter frequencies from the AMSU-A2 and AMSU-A1 Aerojet
  ! calibration logbooks for AMSU-A1 S/N 109 and AMSU-A2
  ! S/N 105. These are the 15C values.
  REAL(fp), PARAMETER :: AMSUA_N18_IF_BAND( 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS ) = &
    RESHAPE( (/ 0.00872_fp,  0.13423_fp,  ZERO,        ZERO,        &     ! AMSU-A2 ch1
                0.00877_fp,  0.08912_fp,  ZERO,        ZERO,        &     ! AMSU-A2 ch2
                0.00890_fp,  0.08900_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch3
                0.00912_fp,  0.19916_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch4
                0.03138_fp,  0.19886_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch5
                0.00915_fp,  0.19933_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch6
                0.00910_fp,  0.19919_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch7
                0.00914_fp,  0.16402_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch8
                0.00911_fp,  0.16415_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch9
                0.17901_fp,  0.25534_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch10
                0.256806_fp, 0.291663_fp, 0.352663_fp, 0.388032_fp, &     ! AMSU-A1 ch11
                0.292552_fp, 0.307980_fp, 0.336329_fp, 0.351829_fp, &     ! AMSU-A1 ch12
                0.308288_fp, 0.316136_fp, 0.328226_fp, 0.336086_fp, &     ! AMSU-A1 ch13
                0.316321_fp, 0.319253_fp, 0.325312_fp, 0.328254_fp, &     ! AMSU-A1 ch14
                0.49208_fp,  1.48898_fp,  ZERO,        ZERO         /), & ! AMSU-A1 ch15
             (/ 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS /) )


  ! NOAA-18 MHS
  ! -----------
  ! MHS central frequencies in GHz.
  ! These values are taken from the Table 3-1: Verification Report Matrix
  ! in the Matra Marconi Space document ref:3175-JA323-RPV, pg9.
  ! In PDF form, this is pg807 in the file MHS101_volume3.pdf.
  ! I couldn't find the actual measurement data so I consider these
  ! as provisional until I get confirmation they're final.
  ! NOTE: Channel 5 was implemented using the upper sideband option,
  !       rather than the typical 183.311+/-7GHz.
  REAL(fp), PARAMETER :: MHS_N18_F0( N_MHS_CHANNELS ) = &
    (/  89.000_fp, 157.000_fp, 183.311_fp, &
       183.311_fp, 190.311_fp /)

  ! MHS I/F band limits in GHz.
  ! These were taken from the tables on pg10 and 11 in the Matra Marconi
  ! Space document ref:MHS.TR.JA247.MMP. In PDF form these were pp520-521
  ! of "MHS QRR_volume3.pdf" or pp362-363 of MHS101volume3.pdf.
  ! These are the 15C values for LO A.
  REAL(fp), PARAMETER :: MHS_N18_IF_BAND( 2, MAX_N_SIDEBANDS, N_MHS_CHANNELS ) = &
    RESHAPE( (/ 0.111_fp, 1.207_fp, ZERO, ZERO, &    ! MHS ch16
                0.111_fp, 1.207_fp, ZERO, ZERO, &    ! MHS ch17
                0.752_fp, 1.210_fp, ZERO, ZERO, &    ! MHS ch18
                2.524_fp, 3.434_fp, ZERO, ZERO, &    ! MHS ch19
                0.113_fp, 1.079_fp, ZERO, ZERO /), & ! MHS ch20
             (/ 2, MAX_N_SIDEBANDS, N_MHS_CHANNELS /) )
! These are the LO B values
!    RESHAPE( (/ 0.111_fp, 1.207_fp, ZERO, ZERO, &    ! MHS ch16
!                0.113_fp, 1.209_fp, ZERO, ZERO, &    ! MHS ch17
!                0.750_fp, 1.210_fp, ZERO, ZERO, &    ! MHS ch18
!                2.526_fp, 3.430_fp, ZERO, ZERO, &    ! MHS ch19
!                0.113_fp, 1.079_fp, ZERO, ZERO /), & ! MHS ch20


  ! Coriolis WindSat
  ! Frequency information taken from ppt presentation
  ! It is assumed there are no stopbands in the same manner
  ! as other microwave instruments with no sidebands so f1
  ! values are all 0.000.
  ! 
  ! Note frequencies are repeated. This is for separate
  ! channels with the same frequency but different
  ! polarisations (or combinations of polarisations).
  ! -------------------------------------------------------
  ! WindSat central frequencies in GHz
  REAL(fp), PARAMETER :: WINDSAT_CORIOLIS_F0( N_WINDSAT_CHANNELS ) = &
    (/ 6.8_fp,  6.8_fp, &
      10.7_fp, 10.7_fp, 10.7_fp, 10.7_fp, &
      18.7_fp, 18.7_fp, 18.7_fp, 18.7_fp, &
      23.8_fp, 23.8_fp, &
      37.0_fp, 37.0_fp, 37.0_fp, 37.0_fp /)

  ! WindSat I/F band limits in GHz.
  REAL(fp), PARAMETER :: WINDSAT_CORIOLIS_IF_BAND( 2, MAX_N_SIDEBANDS, N_WINDSAT_CHANNELS ) = &
    RESHAPE( (/ 0.000_fp, 0.0625_fp, ZERO, ZERO, &    !  6.8GHz vertical pol.
                0.000_fp, 0.0625_fp, ZERO, ZERO, &    !  6.8GHz horizontal pol.
                0.000_fp, 0.150_fp,  ZERO, ZERO, &    ! 10.7GHz R(h) + R(v)
                0.000_fp, 0.150_fp,  ZERO, ZERO, &    ! 10.7GHz R(h) - R(v)    
                0.000_fp, 0.150_fp,  ZERO, ZERO, &    ! 10.7GHz R(+45) - R(-45)
                0.000_fp, 0.150_fp,  ZERO, ZERO, &    ! 10.7GHz R(rc) - R(lc)  
                0.000_fp, 0.375_fp,  ZERO, ZERO, &    ! 18.7GHz R(h) + R(v)
                0.000_fp, 0.375_fp,  ZERO, ZERO, &    ! 18.7GHz R(h) - R(v)    
                0.000_fp, 0.375_fp,  ZERO, ZERO, &    ! 18.7GHz R(+45) - R(-45)
                0.000_fp, 0.375_fp,  ZERO, ZERO, &    ! 18.7GHz R(rc) - R(lc)  
                0.000_fp, 0.250_fp,  ZERO, ZERO, &    ! 23.8GHz vertical pol.
                0.000_fp, 0.250_fp,  ZERO, ZERO, &    ! 23.8GHz horizontal pol.
                0.000_fp, 1.000_fp,  ZERO, ZERO, &    ! 37.0GHz R(h) + R(v)
                0.000_fp, 1.000_fp,  ZERO, ZERO, &    ! 37.0GHz R(h) - R(v)    
                0.000_fp, 1.000_fp,  ZERO, ZERO, &    ! 37.0GHz R(+45) - R(-45)
                0.000_fp, 1.000_fp,  ZERO, ZERO /), & ! 37.0GHz R(rc) - R(lc)  
             (/ 2, MAX_N_SIDEBANDS, N_WINDSAT_CHANNELS /) )


  ! NPOESS-NPP ATMS
  ! Frequency information taken from ppt presentation and
  ! supplemented with information from NGC.
  ! See CRTM trac ticket #303,
  !   https://svnemc.ncep.noaa.gov/trac/crtm/ticket/303
  ! It is assumed there are no stopbands in the same manner
  ! as other microwave instruments with no sidebands so f1
  ! values are all 0.000.
  ! -------------------------------------------------------
  ! ATMS central frequencies in GHz
  REAL(fp), PARAMETER :: ATMS_NPP_F0( N_ATMS_CHANNELS ) = &
    (/ 23.800000_fp,  31.400000_fp,  50.300000_fp,  51.760000_fp, &
       52.800000_fp,  53.596000_fp,  54.400000_fp,  54.940000_fp, &
       55.500000_fp,  57.290344_fp,  57.290344_fp,  57.290344_fp, &
       57.290344_fp,  57.290344_fp,  57.290344_fp,  88.200000_fp, &
      165.500000_fp, 183.310000_fp, 183.310000_fp, 183.310000_fp, &
      183.310000_fp, 183.310000_fp /)

  ! ATMS I/F band limits in GHz.
  REAL(fp), PARAMETER :: ATMS_NPP_IF_BAND( 2, MAX_N_SIDEBANDS, N_ATMS_CHANNELS ) = &
    RESHAPE( (/ 0.000000_fp,  0.135000_fp,  ZERO,         ZERO,        &      ! ATMS ch1
                0.000000_fp,  0.090000_fp,  ZERO,         ZERO,        &      ! ATMS ch2
                0.000000_fp,  0.090000_fp,  ZERO,         ZERO,        &      ! ATMS ch3
                0.000000_fp,  0.200000_fp,  ZERO,         ZERO,        &      ! ATMS ch4
                0.000000_fp,  0.200000_fp,  ZERO,         ZERO,        &      ! ATMS ch5
                0.030000_fp,  0.200000_fp,  ZERO,         ZERO,        &      ! ATMS ch6
                0.000000_fp,  0.200000_fp,  ZERO,         ZERO,        &      ! ATMS ch7
                0.000000_fp,  0.200000_fp,  ZERO,         ZERO,        &      ! ATMS ch8
                0.000000_fp,  0.165000_fp,  ZERO,         ZERO,        &      ! ATMS ch9
                0.011500_fp,  0.162900_fp,  ZERO,         ZERO,        &      ! ATMS ch10
                0.178000_fp,  0.256000_fp,  ZERO,         ZERO,        &      ! ATMS ch11
                0.256200_fp,  0.292200_fp,  0.352200_fp,  0.388200_fp, &      ! ATMS ch12
                0.292200_fp,  0.308200_fp,  0.336200_fp,  0.352200_fp, &      ! ATMS ch13
                0.308200_fp,  0.316200_fp,  0.328200_fp,  0.336200_fp, &      ! ATMS ch14
                0.316200_fp,  0.319200_fp,  0.325200_fp,  0.328200_fp, &      ! ATMS ch15
                0.000000_fp,  1.000000_fp,  ZERO,         ZERO,        &      ! ATMS ch16
                0.360000_fp,  1.490000_fp,  ZERO,         ZERO,        &      ! ATMS ch17
                6.038000_fp,  7.958000_fp,  ZERO,         ZERO,        &      ! ATMS ch18
                3.517000_fp,  5.475000_fp,  ZERO,         ZERO,        &      ! ATMS ch19
                2.500000_fp,  3.496000_fp,  ZERO,         ZERO,        &      ! ATMS ch20
                1.305000_fp,  2.300000_fp,  ZERO,         ZERO,        &      ! ATMS ch21
                0.755000_fp,  1.251000_fp,  ZERO,         ZERO        /), &   ! ATMS ch22
             (/ 2, MAX_N_SIDEBANDS, N_ATMS_CHANNELS /) )


  ! NOAA-19 AMSU-A
  ! --------------
  ! *** AMSU-A1 S/N 107 ***
  ! *** AMSU-A2 S/N 109 ***
  ! AMSU-A central frequencies in GHz
  ! AMSU-A1 data (ch3-15) interpolated from measured data to 15C temperature
  ! AMSU-A1 ch9-14 values is for PLLO #1. PLLO #2 value is 57.290343GHz
  REAL(fp), PARAMETER :: AMSUA_N19_F0( N_AMSUA_CHANNELS ) = &
    (/ 23.799744_fp, 31.402100_fp, 50.300271_fp, &
       52.800655_fp, 53.596130_fp, 54.400131_fp, &
       54.939489_fp, 55.498680_fp, 57.290329_fp, &
       57.290329_fp, 57.290329_fp, 57.290329_fp, &
       57.290329_fp, 57.290329_fp, 89.010000_fp /)

  ! AMSU-A I/F band limits in GHz. These are the 3dB bandpass
  ! filter frequencies from the AMSU-A2 and AMSU-A1 Aerojet
  ! calibration logbooks for AMSU-A1 S/N 107 (report #11326)
  ! and AMSU-A2 S/N 109 (report #11331). These are the 15C values.
  REAL(fp), PARAMETER :: AMSUA_N19_IF_BAND( 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS ) = &
    RESHAPE( (/ 0.00861_fp,  0.13415_fp,  ZERO,        ZERO,        &     ! AMSU-A2 ch1 (pg.103)
                0.00875_fp,  0.08915_fp,  ZERO,        ZERO,        &     ! AMSU-A2 ch2 (pg.114)
                0.00884_fp,  0.08896_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch3 (pg.211)
                0.00904_fp,  0.19924_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch4 (pg.222)
                0.03140_fp,  0.19926_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch5 (pg.233)
                0.00914_fp,  0.19911_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch6 (pg.244)
                0.00913_fp,  0.19928_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch7 (pg.255)
                0.00919_fp,  0.16425_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch8 (pg.266)
                0.00916_fp,  0.16422_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch9 (pg.277)
                0.17934_fp,  0.25532_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch10 (pg.288)
                0.256937_fp, 0.291835_fp, 0.352592_fp, 0.387883_fp, &     ! AMSU-A1 ch11 (pp.302[LO],303[HI])
                0.292551_fp, 0.307988_fp, 0.336236_fp, 0.351764_fp, &     ! AMSU-A1 ch12 (pp.315[LO],316[HI])
                0.308247_fp, 0.316070_fp, 0.328234_fp, 0.336887_fp, &     ! AMSU-A1 ch13 (pp.328[LO],329[HI])
                0.316274_fp, 0.319223_fp, 0.325256_fp, 0.328212_fp, &     ! AMSU-A1 ch14 (pp.341[LO],342[HI])
                0.49157_fp,  1.48733_fp,  ZERO,        ZERO         /), & ! AMSU-A1 ch15 (pg.349)
             (/ 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS /) )


  ! NOAA-19 MHS
  ! -----------
  ! MHS central frequencies in GHz.
  ! Instrument serial number: 102-MHS
  ! Data Source Reference: MHS.TR.JA249.MMP
  REAL(fp), PARAMETER :: MHS_N19_F0( N_MHS_CHANNELS ) = &
    (/  89.000_fp, 157.000_fp, 183.311_fp, &
       183.311_fp, 190.311_fp /)

  ! MHS I/F band limits in GHz.
  ! Instrument serial number: 102-MHS
  ! Data Source Reference: MHS.TR.JA249.MMP
  REAL(fp), PARAMETER :: MHS_N19_IF_BAND( 2, MAX_N_SIDEBANDS, N_MHS_CHANNELS ) = &
    RESHAPE( (/ 0.118_fp, 1.217_fp, ZERO, ZERO, &    ! MHS ch16
                0.111_fp, 1.211_fp, ZERO, ZERO, &    ! MHS ch17
                0.745_fp, 1.219_fp, ZERO, ZERO, &    ! MHS ch18
                2.520_fp, 3.367_fp, ZERO, ZERO, &    ! MHS ch19
                0.112_fp, 1.071_fp, ZERO, ZERO /), & ! MHS ch20
             (/ 2, MAX_N_SIDEBANDS, N_MHS_CHANNELS /) )


  ! MetOp-A AMSU-A
  ! --------------
  ! *** AMSU-A1 S/N 106 ***
  ! *** AMSU-A2 S/N 108 ***
  ! AMSU-A central frequencies in GHz
  ! AMSU-A1 data (ch3-15) interpolated from measured data to 15C temperature
  ! AMSU-A1 ch9-14 values is for PLLO #1. PLLO #2 value is 57.290319GHz
  REAL(fp), PARAMETER :: AMSUA_METOPA_F0( N_AMSUA_CHANNELS ) = &
    (/ 23.800904_fp, 31.400728_fp, 50.300069_fp, &
       52.799890_fp, 53.596155_fp, 54.400633_fp, &
       54.940002_fp, 55.499802_fp, 57.290327_fp, &
       57.290327_fp, 57.290327_fp, 57.290327_fp, &
       57.290327_fp, 57.290327_fp, 88.997000_fp /)

  ! AMSU-A I/F band limits in GHz. These are the 3dB bandpass
  ! filter frequencies from the AMSU-A2 and AMSU-A1 Aerojet
  ! calibration logbooks for AMSU-A1 S/N 106 (report #11324)
  ! and AMSU-A2 S/N 108 (report #11329). These are the 15C values.
  REAL(fp), PARAMETER :: AMSUA_METOPA_IF_BAND( 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS ) = &
    RESHAPE( (/ 0.00867_fp,  0.13416_fp,  ZERO,        ZERO,        &     ! AMSU-A2 ch1 (pg.107)
                0.00893_fp,  0.08892_fp,  ZERO,        ZERO,        &     ! AMSU-A2 ch2 (pg.118)
                0.00885_fp,  0.08921_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch3 (pg.208)
                0.00905_fp,  0.19903_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch4 (pg.208)
                0.03134_fp,  0.19909_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch5 (pg.208)
                0.00910_fp,  0.19930_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch6 (pg.208)
                0.00915_fp,  0.19923_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch7 (pg.208)
                0.00906_fp,  0.16419_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch8 (pg.208)
                0.00913_fp,  0.16402_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch9 (pg.208)
                0.17921_fp,  0.25508_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch10 (pg.208)
                0.257043_fp, 0.291937_fp, 0.352609_fp, 0.387988_fp, &     ! AMSU-A1 ch11 (pp.302[LO],303[HI])
                0.292628_fp, 0.308060_fp, 0.336465_fp, 0.351940_fp, &     ! AMSU-A1 ch12 (pp.315[LO],316[HI])
                0.308343_fp, 0.316152_fp, 0.328359_fp, 0.335223_fp, &     ! AMSU-A1 ch13 (pp.328[LO],329[HI])
                0.316276_fp, 0.319209_fp, 0.325346_fp, 0.328300_fp, &     ! AMSU-A1 ch14 (pp.341[LO],342[HI])
                0.48978_fp,  1.48805_fp,  ZERO,        ZERO         /), & ! AMSU-A1 ch15 (pg.208)
             (/ 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS /) )


  ! MetOp-A MHS
  ! -----------
  ! MHS central frequencies in GHz.
  ! Instrument serial number: 103-MHS
  ! Data Source Reference: MHS.TR.JA256.MMP
  REAL(fp), PARAMETER :: MHS_METOPA_F0( N_MHS_CHANNELS ) = &
    (/  89.000_fp, 157.000_fp, 183.311_fp, &
       183.311_fp, 190.311_fp /)

  ! MHS I/F band limits in GHz.
  ! Instrument serial number: 103-MHS
  ! Data Source Reference: MHS.TR.JA256.MMP
  REAL(fp), PARAMETER :: MHS_METOPA_IF_BAND( 2, MAX_N_SIDEBANDS, N_MHS_CHANNELS ) = &
    RESHAPE( (/ 0.112_fp, 1.206_fp, ZERO, ZERO, &    ! MHS ch16
                0.114_fp, 1.211_fp, ZERO, ZERO, &    ! MHS ch17
                0.750_fp, 1.218_fp, ZERO, ZERO, &    ! MHS ch18
                2.500_fp, 3.330_fp, ZERO, ZERO, &    ! MHS ch19
                0.111_fp, 1.065_fp, ZERO, ZERO /), & ! MHS ch20
             (/ 2, MAX_N_SIDEBANDS, N_MHS_CHANNELS /) )


  ! MetOp-B AMSU-A
  ! --------------
  ! *** AMSU-A1 S/N 108 ***
  ! *** AMSU-A2 S/N 106 ***
  ! AMSU-A central frequencies in GHz
  ! AMSU-A1 data (ch3-15) interpolated from measured data to 15C temperature
  ! AMSU-A1 ch9-14 values is for PLLO #1. PLLO #2 value is 57.290319GHz
  REAL(fp), PARAMETER :: AMSUA_METOPB_F0( N_AMSUA_CHANNELS ) = &
    (/ 23.800335_fp, 31.401473_fp, 50.299733_fp, &
       52.800575_fp, 53.596741_fp, 54.399710_fp, &
       54.939924_fp, 55.500450_fp, 57.290313_fp, &
       57.290313_fp, 57.290313_fp, 57.290313_fp, &
       57.290313_fp, 57.290313_fp, 89.003000_fp /)

  ! AMSU-A I/F band limits in GHz. These are the 3dB bandpass
  ! filter frequencies from the AMSU-A2 and AMSU-A1 Aerojet
  ! calibration logbooks for AMSU-A1 S/N 108 (report #11328)
  ! and AMSU-A2 S/N 106 (report #11325). These are the 15C values.
  REAL(fp), PARAMETER :: AMSUA_METOPB_IF_BAND( 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS ) = &
    RESHAPE( (/ 0.00852_fp,  0.13416_fp,  ZERO,        ZERO,        &     ! AMSU-A2 ch1 (pg.103)
                0.00916_fp,  0.08938_fp,  ZERO,        ZERO,        &     ! AMSU-A2 ch2 (pg.114)
                0.00879_fp,  0.08909_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch3 (pg.205)
                0.00907_fp,  0.19936_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch4 (pg.205)
                0.03130_fp,  0.19900_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch5 (pg.205)
                0.00907_fp,  0.19938_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch6 (pg.205)
                0.00913_fp,  0.19932_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch7 (pg.205)
                0.00914_fp,  0.16419_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch8 (pg.205)
                0.00915_fp,  0.16414_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch9 (pg.205)
                0.17914_fp,  0.25527_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch10 (pg.205)
                0.256977_fp, 0.291873_fp, 0.352628_fp, 0.387892_fp, &     ! AMSU-A1 ch11 (pp.206)
                0.292565_fp, 0.307998_fp, 0.336332_fp, 0.351781_fp, &     ! AMSU-A1 ch12 (pp.206)
                0.308312_fp, 0.316124_fp, 0.328339_fp, 0.336215_fp, &     ! AMSU-A1 ch13 (pp.206)
                0.316278_fp, 0.319208_fp, 0.325201_fp, 0.328140_fp, &     ! AMSU-A1 ch14 (pp.206)
                0.49129_fp,  1.48691_fp,  ZERO,        ZERO         /), & ! AMSU-A1 ch15 (pg.205)
             (/ 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS /) )


  ! MetOp-B MHS
  ! -----------
  ! MHS central frequencies in GHz.
  ! Instrument serial number: 104-MHS
  ! Data Source Reference: MHS.TR.JA267.MMP
  REAL(fp), PARAMETER :: MHS_METOPB_F0( N_MHS_CHANNELS ) = &
    (/  89.000_fp, 157.000_fp, 183.311_fp, &
       183.311_fp, 190.311_fp /)


  ! MHS I/F band limits in GHz.
  ! Instrument serial number: 104-MHS
  ! Data Source Reference: MHS.TR.JA267.MMP
  REAL(fp), PARAMETER :: MHS_METOPB_IF_BAND( 2, MAX_N_SIDEBANDS, N_MHS_CHANNELS ) = &
    RESHAPE( (/ 0.113_fp, 1.202_fp, ZERO, ZERO, &    ! MHS ch16
                0.112_fp, 1.205_fp, ZERO, ZERO, &    ! MHS ch17
                0.748_fp, 1.218_fp, ZERO, ZERO, &    ! MHS ch18
                2.517_fp, 3.362_fp, ZERO, ZERO, &    ! MHS ch19
                0.112_fp, 1.077_fp, ZERO, ZERO /), & ! MHS ch20
             (/ 2, MAX_N_SIDEBANDS, N_MHS_CHANNELS /) )


  ! MetOp-C AMSU-A
  ! --------------
  ! *** AMSU-A1 S/N 105 ***
  ! *** AMSU-A2 S/N 107 ***
  ! AMSU-A central frequencies in GHz
  ! AMSU-A1 data (ch3-15) interpolated from measured data to 15C temperature
  ! AMSU-A1 ch9-14 values is for PLLO #1. PLLO #2 value is 57.290334GHz
  REAL(fp), PARAMETER :: AMSUA_METOPC_F0( N_AMSUA_CHANNELS ) = &
    (/ 23.800558_fp, 31.402120_fp, 50.300397_fp, &
       52.799710_fp, 53.596297_fp, 54.399751_fp, &
       54.940644_fp, 55.499839_fp, 57.290335_fp, &
       57.290335_fp, 57.290335_fp, 57.290335_fp, &
       57.290335_fp, 57.290335_fp, 89.000800_fp /)

  ! AMSU-A I/F band limits in GHz. These are the 3dB bandpass
  ! filter frequencies from the AMSU-A2 and AMSU-A1 Aerojet
  ! calibration logbooks for AMSU-A1 S/N 105 (report #11322)
  ! and AMSU-A2 S/N 107 (report #11327). These are the 15C values.
  REAL(fp), PARAMETER :: AMSUA_METOPC_IF_BAND( 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS ) = &
    RESHAPE( (/ 0.00860_fp,  0.13412_fp,  ZERO,        ZERO,        &     ! AMSU-A2 ch1 (pg.103)
                0.00906_fp,  0.08922_fp,  ZERO,        ZERO,        &     ! AMSU-A2 ch2 (pg.114)
                0.00884_fp,  0.08928_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch3 (pg.195)
                0.00920_fp,  0.19904_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch4 (pg.105)
                0.03134_fp,  0.19905_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch5 (pg.195)
                0.00914_fp,  0.19919_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch6 (pg.195)
                0.00917_fp,  0.19926_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch7 (pg.195)
                0.00907_fp,  0.16406_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch8 (pg.195)
                0.00911_fp,  0.16415_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch9 (pg.195)
                0.17887_fp,  0.25511_fp,  ZERO,        ZERO,        &     ! AMSU-A1 ch10 (pg.195)
                0.257070_fp, 0.291921_fp, 0.352406_fp, 0.387740_fp, &     ! AMSU-A1 ch11 (pp.196)
                0.292527_fp, 0.307953_fp, 0.336374_fp, 0.351850_fp, &     ! AMSU-A1 ch12 (pp.196)
                0.308370_fp, 0.316193_fp, 0.328303_fp, 0.336182_fp, &     ! AMSU-A1 ch13 (pp.196)
                0.316245_fp, 0.319180_fp, 0.325187_fp, 0.328350_fp, &     ! AMSU-A1 ch14 (pp.196)
                0.49298_fp,  1.48667_fp,  ZERO,        ZERO         /), & ! AMSU-A1 ch15 (pg.195)
             (/ 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS /) )


  ! MetOp-C MHS
  ! -----------
  ! MHS central frequencies in GHz.
  ! Instrument serial number: 105-MHS
  ! Data Source Reference: MHS.TR.JA377.MMP
  REAL(fp), PARAMETER :: MHS_METOPC_F0( N_MHS_CHANNELS ) = &
    (/  89.000_fp, 157.000_fp, 183.311_fp, &
       183.311_fp, 190.311_fp /)

  ! MHS I/F band limits in GHz.
  ! Instrument serial number: 105-MHS
  ! Data Source Reference: MHS.TR.JA377.MMP
  REAL(fp), PARAMETER :: MHS_METOPC_IF_BAND( 2, MAX_N_SIDEBANDS, N_MHS_CHANNELS ) = &
    RESHAPE( (/ 0.115_fp, 1.209_fp, ZERO, ZERO, &    ! MHS ch16
                0.111_fp, 1.218_fp, ZERO, ZERO, &    ! MHS ch17
                0.743_fp, 1.215_fp, ZERO, ZERO, &    ! MHS ch18
                2.485_fp, 3.358_fp, ZERO, ZERO, &    ! MHS ch19
                0.114_fp, 1.078_fp, ZERO, ZERO /), & ! MHS ch20
             (/ 2, MAX_N_SIDEBANDS, N_MHS_CHANNELS /) )


  ! TRMM TMI
  ! --------
  ! TMI central frequencies in GHz.
  REAL(fp), PARAMETER :: TMI_F0( N_TMI_CHANNELS ) = &
    (/  10.65_fp, 10.65_fp, 19.35_fp, 19.35_fp, 21.3_fp, &
        37.0_fp , 37.0_fp , 85.5_fp , 85.5_fp /)

  ! TMI I/F band limits in GHz.
  REAL(fp), PARAMETER :: TMI_IF_BAND( 2, MAX_N_SIDEBANDS, N_TMI_CHANNELS ) = &
    RESHAPE( (/ ZERO, 0.05_fp, ZERO, ZERO, &    ! TMI ch1
                ZERO, 0.05_fp, ZERO, ZERO, &    ! TMI ch2
                ZERO, 0.25_fp, ZERO, ZERO, &    ! TMI ch3
                ZERO, 0.25_fp, ZERO, ZERO, &    ! TMI ch4
                ZERO, 0.1_fp , ZERO, ZERO, &    ! TMI ch5
                ZERO, 1.0_fp , ZERO, ZERO, &    ! TMI ch6
                ZERO, 1.0_fp , ZERO, ZERO, &    ! TMI ch7
                ZERO, 1.5_fp , ZERO, ZERO, &    ! TMI ch8
                ZERO, 1.5_fp , ZERO, ZERO /), & ! TMI ch9
             (/ 2, MAX_N_SIDEBANDS, N_TMI_CHANNELS /) )


  ! GPM GMI
  ! -------
  ! GMI central frequencies in GHz.
  REAL(fp), PARAMETER :: GMI_F0( N_GMI_CHANNELS ) = &
    (/  10.65_fp, 10.65_fp, 18.7_fp, 18.7_fp, 23.8_fp, &
        36.5_fp , 36.5_fp , 89.0_fp, 89.0_fp, &
       165.5_fp ,165.5_fp , &
       183.31_fp,183.31_fp/)

  ! GMI I/F band limits in GHz.
  ! NOTE: For channel 13, the I/F limits are based on +/- 8GHz sidebands.
  !       This channel may be +/- 9GHz, so the sideband values would then 
  !       be: "6.75_fp, 11.25_fp, ZERO, ZERO"
  REAL(fp), PARAMETER :: GMI_IF_BAND( 2, MAX_N_SIDEBANDS, N_GMI_CHANNELS ) = &
    RESHAPE( (/    ZERO,  0.05_fp, ZERO, ZERO, &    ! GMI ch1
                   ZERO,  0.05_fp, ZERO, ZERO, &    ! GMI ch2
                   ZERO,  0.1_fp , ZERO, ZERO, &    ! GMI ch3
                   ZERO,  0.1_fp , ZERO, ZERO, &    ! GMI ch4
                   ZERO,  0.2_fp , ZERO, ZERO, &    ! GMI ch5
                   ZERO,  0.5_fp , ZERO, ZERO, &    ! GMI ch6
                   ZERO,  0.5_fp , ZERO, ZERO, &    ! GMI ch7
                   ZERO,  3.0_fp , ZERO, ZERO, &    ! GMI ch8
                   ZERO,  3.0_fp , ZERO, ZERO, &    ! GMI ch9
                   ZERO,  1.5_fp , ZERO, ZERO, &    ! GMI ch10
                   ZERO,  1.5_fp , ZERO, ZERO, &    ! GMI ch11
                1.25_fp,  4.75_fp, ZERO, ZERO, &    ! GMI ch12
                5.75_fp, 10.25_fp, ZERO, ZERO /), & ! GMI ch13 For +/- 8GHz. May be 6.75_fp, 11.25_fp for +/-9GHz
             (/ 2, MAX_N_SIDEBANDS, N_GMI_CHANNELS /) )


  ! FengYun-3 MWRI
  ! --------------
  ! Central frequencies in GHz.
  REAL(fp), PARAMETER :: MWRI_F0( N_MWRI_CHANNELS ) = &
    (/ 10.65_fp, 10.65_fp, 18.7_fp, 18.7_fp,  23.8_fp,  23.8_fp, &
       36.5_fp , 36.5_fp , 89.0_fp, 89.0_fp /)

  ! I/F band limits in GHz.
  ! according to Hu Yang, the I/F for channle 9 and 10 is 0.5Ghz, 2.35Ghz
  REAL(fp), PARAMETER :: MWRI_IF_BAND( 2, MAX_N_SIDEBANDS, N_MWRI_CHANNELS ) = &
    RESHAPE( (/ ZERO,    0.09_fp, ZERO, ZERO, &    ! ch1
                ZERO,    0.09_fp, ZERO, ZERO, &    ! ch2
                ZERO,    0.10_fp, ZERO, ZERO, &    ! ch3
                ZERO,    0.10_fp, ZERO, ZERO, &    ! ch4
                ZERO,    0.20_fp, ZERO, ZERO, &    ! ch5
                ZERO,    0.20_fp, ZERO, ZERO, &    ! ch6
                ZERO,    0.45_fp, ZERO, ZERO, &    ! ch7
                ZERO,    0.45_fp, ZERO, ZERO, &    ! ch8
!                ZERO, 2.30_fp, ZERO, ZERO, &    ! ch9  :Only bandwidth info available. No I/F
!                ZERO, 2.30_fp, ZERO, ZERO /), & ! ch10 :Only bandwidth info available. No I/F
                0.50_fp, 2.8_fp, ZERO, ZERO, &     
                0.50_fp, 2.8_fp, ZERO, ZERO /), & 
             (/ 2, MAX_N_SIDEBANDS, N_MWRI_CHANNELS /) )


  ! FengYun-3A MWHS
  ! --------------
  ! Central frequencies in GHz.
  REAL(fp), PARAMETER :: MWHS_FY3A_F0( N_MWHS_CHANNELS ) = &
    (/ 150.006812_fp, 150.006812_fp, &
       183.315120_fp, 183.315120_fp, 183.315120_fp/)

  ! I/F band limits in GHz.
  REAL(fp), PARAMETER :: MWHS_FY3A_IF_BAND( 2, MAX_N_SIDEBANDS, N_MWHS_CHANNELS ) = &
    RESHAPE( (/ 0.3837500000_fp, 1.3852083_fp, ZERO, ZERO, &    ! ch1
                0.4077080000_fp, 1.3947917_fp, ZERO, ZERO, &    ! ch2
                0.7820512821_fp, 1.2628205_fp, ZERO, ZERO, &    ! ch3
                2.4471153850_fp, 3.4807692_fp, ZERO, ZERO, &    ! ch4
                5.9134615380_fp, 8.0961538_fp, ZERO, ZERO /), & ! ch5
             (/ 2, MAX_N_SIDEBANDS, N_MWHS_CHANNELS /) )

  ! FengYun-3B MWHS
  ! --------------
  ! Central frequencies in GHz.
  REAL(fp), PARAMETER :: MWHS_FY3B_F0( N_MWHS_CHANNELS ) = &
    (/ 149.9976_fp,  149.9976_fp, &
       183.3067_fp,  183.3067_fp, 183.3067_fp/)

  ! I/F band limits in GHz.
  REAL(fp), PARAMETER :: MWHS_FY3B_IF_BAND( 2, MAX_N_SIDEBANDS, N_MWHS_CHANNELS ) = &
    RESHAPE( (/ 0.40598_fp, 1.45681_fp, ZERO, ZERO, &    ! ch1
                0.41196_fp, 1.42990_fp, ZERO, ZERO, &    ! ch2
                0.74253_fp, 1.24253_fp, ZERO, ZERO, &    ! ch3
                2.43200_fp, 3.49600_fp, ZERO, ZERO, &    ! ch4
                5.89200_fp, 8.05400_fp, ZERO, ZERO /), & ! ch5
             (/ 2, MAX_N_SIDEBANDS, N_MWHS_CHANNELS /) )

  ! FengYun-3A MWTS
  !
  ! Data from presentation by Hu Yang given at the
  ! 7th GPM International Planning Workshop Tokyo, Japan Dec.5-7 2007
  ! See: http://www.eorc.jaxa.jp/GPM/ws7/program.html, talk 5_pm13,
  !        or
  !      http://www.eorc.jaxa.jp/GPM/ws7/pdf/5thDEC2007/PM/5_pm13.pdf
  ! -----------------------------------------------------------------
  ! Central frequencies in GHz.
  REAL(fp), PARAMETER :: MWTS_FY3A_F0( N_MWTS_CHANNELS ) = &
    (/ 50.30_fp, 53.596_fp, 54.94_fp, 57.290_fp/)

  ! I/F band limits in GHz.
  REAL(fp), PARAMETER :: MWTS_FY3A_IF_BAND( 2, MAX_N_SIDEBANDS, N_MWTS_CHANNELS ) = &
    RESHAPE( (/ ZERO   , 0.09_fp , ZERO, ZERO, &    ! ch1
                0.03_fp, 0.2_fp  , ZERO, ZERO, &    ! ch2
                ZERO   , 0.2_fp  , ZERO, ZERO, &    ! ch3
                ZERO   , 0.165_fp, ZERO, ZERO /), & ! ch4
             (/ 2, MAX_N_SIDEBANDS, N_MWTS_CHANNELS /) )

  ! FengYun-3B MWTS
  ! -----------------------------------------------------------------
  ! Central frequencies in GHz.
  REAL(fp), PARAMETER :: MWTS_FY3B_F0( N_MWTS_CHANNELS ) = &
    (/ 50.36818_fp, 53.58984_fp, 54.93979_fp, 57.30168_fp/)

  ! I/F band limits in GHz.
  REAL(fp), PARAMETER :: MWTS_FY3B_IF_BAND( 2, MAX_N_SIDEBANDS, N_MWTS_CHANNELS ) = &
    RESHAPE( (/ 0.0125_fp, 0.0935_fp, ZERO, ZERO, &    ! ch1
                0.0295_fp, 0.2071_fp, ZERO, ZERO, &    ! ch2
                0.0196_fp, 0.2072_fp, ZERO, ZERO, &    ! ch3
                0.0150_fp, 0.1737_fp, ZERO, ZERO /), & ! ch4
             (/ 2, MAX_N_SIDEBANDS, N_MWTS_CHANNELS /) )

  ! Megha-Tropiques MADRAS
  ! -----------------------------------------------------------------
  ! These were taken from http://meghatropiques.ipsl.polytechnique.fr/instruments.html
  ! MADRAS table 
  ! Central frequencies in GHz.
  REAL(fp), PARAMETER :: MADRAS_MEGHAT_F0( N_MADRAS_CHANNELS ) = &
    (/ 18.7_fp,  18.7_fp,  23.8_fp, &
       36.5_fp,  36.5_fp,  89.0_fp, &
       89.0_fp, 157.0_fp, 157.0_fp/)

  ! I/F band limits in GHz.
  REAL(fp), PARAMETER :: MADRAS_MEGHAT_IF_BAND( 2, MAX_N_SIDEBANDS, N_MADRAS_CHANNELS ) = &
    RESHAPE( (/ 0.00_fp, 0.10_fp, ZERO, ZERO, &    ! ch1
                0.00_fp, 0.10_fp, ZERO, ZERO, &    ! ch2
                0.00_fp, 0.20_fp, ZERO, ZERO, &    ! ch3
                0.00_fp, 0.50_fp, ZERO, ZERO, &    ! ch4
                0.00_fp, 0.50_fp, ZERO, ZERO, &    ! ch5
                0.00_fp, 1.35_fp, ZERO, ZERO, &    ! ch6
                0.00_fp, 1.35_fp, ZERO, ZERO, &    ! ch7
                0.00_fp, 1.35_fp, ZERO, ZERO, &    ! ch8
                0.00_fp, 1.35_fp, ZERO, ZERO /), & ! ch5
             (/ 2, MAX_N_SIDEBANDS, N_MADRAS_CHANNELS /) )

  ! Megha-Tropiques SAPHIR
  ! -----------------------------------------------------------------
  ! These were taken from http://meghatropiques.ipsl.polytechnique.fr/dmdocuments/Aires_Pres_mt.pdf
  ! SAPHIR table from page 32 (1st Mission/Project Meeting Toulouse October 18, 2007)
  ! and http://www.wmo.int/pages/prog/sat/Instruments_and_missions/SAPHIR.html
  ! Central frequencies in GHz.
  REAL(fp), PARAMETER :: SAPHIR_MEGHAT_F0( N_SAPHIR_CHANNELS ) = &
    (/ 183.310_fp,  183.310_fp, 183.310_fp, &
       183.310_fp,  183.310_fp, 183.310_fp/)

  ! I/F band limits in GHz.
  REAL(fp), PARAMETER :: SAPHIR_MEGHAT_IF_BAND( 2, MAX_N_SIDEBANDS, N_SAPHIR_CHANNELS ) = &
    RESHAPE( (/ 0.00_fp, 0.40_fp, ZERO, ZERO, &    ! ch1
                0.75_fp, 1.45_fp, ZERO, ZERO, &    ! ch2
                2.30_fp, 3.30_fp, ZERO, ZERO, &    ! ch3
                3.50_fp, 4.90_fp, ZERO, ZERO, &    ! ch4
                5.60_fp, 8.00_fp, ZERO, ZERO, &    ! ch5
                9.00_fp, 13.0_fp, ZERO, ZERO /), & ! ch6
             (/ 2, MAX_N_SIDEBANDS, N_SAPHIR_CHANNELS /) )

  ! Hamsr-Grip GRIP Aircraft experiment
  ! -----------------------------------------------------------------
  ! These were taken from ftp://ghrc.nasa.gov/pub/doc/grip/griphamsr/HAMSR_L1B_description.docx
  
  REAL(fp), PARAMETER :: HAMSR_GRIP_F0( N_HAMSR_CHANNELS ) = &
    (/ 50.300_fp,  51.810_fp,  52.820_fp,  53.596_fp, &  
       54.410_fp,  54.940_fp,  55.460_fp,  56.300_fp, &   !! channel 8 have problem to use boxcar
      113.270_fp, 115.190_fp, 116.180_fp, 116.700_fp, &  
      117.130_fp, 117.540_fp, 118.750_fp, 118.750_fp, &  
      118.750_fp, 118.750_fp, 166.950_fp, 183.310_fp, &
      183.310_fp, 183.310_fp, 183.310_fp, 183.310_fp, 183.310_fp/)

  ! I/F band limits in GHz.
  REAL(fp), PARAMETER :: HAMSR_GRIP_IF_BAND( 2, MAX_N_SIDEBANDS, N_HAMSR_CHANNELS ) = &
    RESHAPE( (/ 0.00000_fp, 0.09267_fp, ZERO, ZERO, &    ! ch1
                0.00000_fp, 0.22813_fp, ZERO, ZERO, &    ! ch2
                0.00000_fp, 0.22230_fp, ZERO, ZERO, &    ! ch3
                0.03825_fp, 0.19175_fp, ZERO, ZERO, &    ! ch4
                0.00000_fp, 0.22325_fp, ZERO, ZERO, &    ! ch5
                0.00000_fp, 0.22146_fp, ZERO, ZERO, &    ! ch6
                0.00000_fp, 0.18740_fp, ZERO, ZERO, &    ! ch7
                0.18250_fp, 0.43750_fp, ZERO, ZERO, &    ! ch8
                0.00000_fp, 0.50311_fp, ZERO, ZERO, &    ! ch9
                0.00000_fp, 0.50301_fp, ZERO, ZERO, &    ! ch10
                0.00000_fp, 0.25305_fp, ZERO, ZERO, &    ! ch11
                0.00000_fp, 0.25217_fp, ZERO, ZERO, &    ! ch12
                0.00000_fp, 0.21607_fp, ZERO, ZERO, &    ! ch13
                0.00000_fp, 0.20948_fp, ZERO, ZERO, &    ! ch14
                0.57900_fp, 1.02100_fp, ZERO, ZERO, &    ! ch15
                0.29500_fp, 0.60500_fp, ZERO, ZERO, &    ! ch16
                0.17000_fp, 0.30000_fp, ZERO, ZERO, &    ! ch17
                0.07000_fp, 0.17000_fp, ZERO, ZERO, &    ! ch18
                0.00000_fp, 1.90641_fp, ZERO, ZERO, &    ! ch19
                8.95000_fp, 11.0500_fp, ZERO, ZERO, &    ! ch20
                5.80000_fp, 8.20000_fp, ZERO, ZERO, &    ! ch21
                3.42500_fp, 5.57500_fp, ZERO, ZERO, &    ! ch22
                2.45000_fp, 3.55000_fp, ZERO, ZERO, &    ! ch23
                1.24850_fp, 2.35150_fp, ZERO, ZERO, &    ! ch24
                0.73120_fp, 1.26880_fp, ZERO, ZERO /), & ! ch25
             (/ 2, MAX_N_SIDEBANDS, N_HAMSR_CHANNELS /) )


  ! MicroMAS CubeSat-00 to -05 dummy entries
  ! -------------------------------------------------------
  ! Central frequency
  REAL(fp), PARAMETER :: MICROMAS_F0( N_MICROMAS_CHANNELS ) = ZERO

  ! I/F band limits in GHz.
  REAL(fp), PARAMETER :: MICROMAS_IF_BAND( 2, MAX_N_SIDEBANDS, N_MICROMAS_CHANNELS ) = ZERO


  ! GeoStorm dummy entry  
  ! -------------------------------------------------------
  ! Central frequency
  REAL(fp), PARAMETER :: GEOSTORM_F0( N_GEOSTORM_CHANNELS ) = ZERO

  ! I/F band limits in GHz.
  REAL(fp), PARAMETER :: GEOSTORM_IF_BAND( 2, MAX_N_SIDEBANDS, N_GEOSTORM_CHANNELS ) = ZERO


  ! MASC dummy entry  
  ! -------------------------------------------------------
  ! Central frequency
  REAL(fp), PARAMETER :: MASC_F0( N_MASC_CHANNELS ) = ZERO

  ! I/F band limits in GHz.
  REAL(fp), PARAMETER :: MASC_IF_BAND( 2, MAX_N_SIDEBANDS, N_MASC_CHANNELS ) = ZERO


  !#----------------------------------------------------------------------------#
  !                           Sensor polarization data
  !#----------------------------------------------------------------------------#

  ! Parameter definitions inherited from the MW_SensorData_DEFINE module. Note
  ! that UNPOLARIZED, INTENSITY, and FIRST_STOKES_COMPONENT all refer to the
  ! same polarization state; that is, Stokes vector of [1,0,0,0]

  ! NOAA-5->14 MSU
  ! --------------
  INTEGER, PARAMETER :: MSU_POLARIZATION( N_MSU_CHANNELS ) = &
  (/ VL_MIXED_POLARIZATION, &  ! MSU ch1
     HL_MIXED_POLARIZATION, &  ! MSU ch2
     VL_MIXED_POLARIZATION, &  ! MSU ch3
     HL_MIXED_POLARIZATION /)  ! MSU ch4
                          
  ! AMSU-A
  ! ------
  INTEGER, PARAMETER :: AMSUA_POLARIZATION( N_AMSUA_CHANNELS ) = &
  (/ VL_MIXED_POLARIZATION, &  ! AMSU-A2 ch1
     VL_MIXED_POLARIZATION, &  ! AMSU-A2 ch2
     VL_MIXED_POLARIZATION, &  ! AMSU-A1 ch3
     VL_MIXED_POLARIZATION, &  ! AMSU-A1 ch4
     HL_MIXED_POLARIZATION, &  ! AMSU-A1 ch5
     HL_MIXED_POLARIZATION, &  ! AMSU-A1 ch6
     VL_MIXED_POLARIZATION, &  ! AMSU-A1 ch7
     HL_MIXED_POLARIZATION, &  ! AMSU-A1 ch8
     HL_MIXED_POLARIZATION, &  ! AMSU-A1 ch9
     HL_MIXED_POLARIZATION, &  ! AMSU-A1 ch10
     HL_MIXED_POLARIZATION, &  ! AMSU-A1 ch11
     HL_MIXED_POLARIZATION, &  ! AMSU-A1 ch12
     HL_MIXED_POLARIZATION, &  ! AMSU-A1 ch13
     HL_MIXED_POLARIZATION, &  ! AMSU-A1 ch14
     VL_MIXED_POLARIZATION /)  ! AMSU-A1 ch15

  ! AMSU-B
  ! ------
  INTEGER, PARAMETER :: AMSUB_POLARIZATION ( N_AMSUB_CHANNELS ) = &
  (/ VL_MIXED_POLARIZATION, &  ! AMSU-B ch16
     VL_MIXED_POLARIZATION, &  ! AMSU-B ch17
     VL_MIXED_POLARIZATION, &  ! AMSU-B ch18
     VL_MIXED_POLARIZATION, &  ! AMSU-B ch19
     VL_MIXED_POLARIZATION /)  ! AMSU-B ch20

  ! SSM/I
  ! -----
  INTEGER, PARAMETER :: SSMI_POLARIZATION( N_SSMI_CHANNELS ) = &
  (/ VL_POLARIZATION, &  ! SSM/I ch1
     HL_POLARIZATION, &  ! SSM/I ch2
     VL_POLARIZATION, &  ! SSM/I ch3
     VL_POLARIZATION, &  ! SSM/I ch4
     HL_POLARIZATION, &  ! SSM/I ch5
     VL_POLARIZATION, &  ! SSM/I ch6
     HL_POLARIZATION /)  ! SSM/I ch7

  ! SSM/T-1
  ! -------
  INTEGER, PARAMETER :: SSMT1_POLARIZATION( N_SSMT1_CHANNELS ) = &
  (/ VL_POLARIZATION, &  ! SSM/T-1 ch1
     HL_POLARIZATION, &  ! SSM/T-1 ch2
     VL_POLARIZATION, &  ! SSM/T-1 ch3
     VL_POLARIZATION, &  ! SSM/T-1 ch4
     HL_POLARIZATION, &  ! SSM/T-1 ch5
     VL_POLARIZATION, &  ! SSM/T-1 ch6
     HL_POLARIZATION /)  ! SSM/T-1 ch7

  ! SSM/T-2
  ! -------
  INTEGER, PARAMETER :: SSMT2_POLARIZATION( N_SSMT2_CHANNELS ) = &
  (/ HL_POLARIZATION, &  ! SSM/T-2 ch1
     HL_POLARIZATION, &  ! SSM/T-2 ch2
     HL_POLARIZATION, &  ! SSM/T-2 ch3
     HL_POLARIZATION, &  ! SSM/T-2 ch4
     HL_POLARIZATION /)  ! SSM/T-2 ch5

  ! SSMIS
  ! -----
  ! ...DMSP-16
  INTEGER, PARAMETER :: SSMIS_F16_POLARIZATION( N_SSMIS_CHANNELS ) = &
  (/ VL_POLARIZATION, &  ! SSMIS ch1
     VL_POLARIZATION, &  ! SSMIS ch2
     VL_POLARIZATION, &  ! SSMIS ch3
     VL_POLARIZATION, &  ! SSMIS ch4
     VL_POLARIZATION, &  ! SSMIS ch5
     RC_POLARIZATION, &  ! SSMIS ch6
     RC_POLARIZATION, &  ! SSMIS ch7
     HL_POLARIZATION, &  ! SSMIS ch8
     HL_POLARIZATION, &  ! SSMIS ch9
     HL_POLARIZATION, &  ! SSMIS ch10
     HL_POLARIZATION, &  ! SSMIS ch11
     HL_POLARIZATION, &  ! SSMIS ch12
     VL_POLARIZATION, &  ! SSMIS ch13
     VL_POLARIZATION, &  ! SSMIS ch14
     HL_POLARIZATION, &  ! SSMIS ch15
     VL_POLARIZATION, &  ! SSMIS ch16
     VL_POLARIZATION, &  ! SSMIS ch17
     HL_POLARIZATION, &  ! SSMIS ch18
     RC_POLARIZATION, &  ! SSMIS ch19
     RC_POLARIZATION, &  ! SSMIS ch20
     RC_POLARIZATION, &  ! SSMIS ch21
     RC_POLARIZATION, &  ! SSMIS ch22
     RC_POLARIZATION, &  ! SSMIS ch23
     RC_POLARIZATION /)  ! SSMIS ch24
  ! ...DMSP-17 to -20
  INTEGER, PARAMETER :: SSMIS_POLARIZATION( N_SSMIS_CHANNELS ) = &
  (/ HL_POLARIZATION, &  ! SSMIS ch1
     HL_POLARIZATION, &  ! SSMIS ch2
     HL_POLARIZATION, &  ! SSMIS ch3
     HL_POLARIZATION, &  ! SSMIS ch4
     HL_POLARIZATION, &  ! SSMIS ch5
     RC_POLARIZATION, &  ! SSMIS ch6
     RC_POLARIZATION, &  ! SSMIS ch7
     HL_POLARIZATION, &  ! SSMIS ch8
     HL_POLARIZATION, &  ! SSMIS ch9
     HL_POLARIZATION, &  ! SSMIS ch10
     HL_POLARIZATION, &  ! SSMIS ch11
     HL_POLARIZATION, &  ! SSMIS ch12
     VL_POLARIZATION, &  ! SSMIS ch13
     VL_POLARIZATION, &  ! SSMIS ch14
     HL_POLARIZATION, &  ! SSMIS ch15
     VL_POLARIZATION, &  ! SSMIS ch16
     VL_POLARIZATION, &  ! SSMIS ch17
     HL_POLARIZATION, &  ! SSMIS ch18
     RC_POLARIZATION, &  ! SSMIS ch19
     RC_POLARIZATION, &  ! SSMIS ch20
     RC_POLARIZATION, &  ! SSMIS ch21
     RC_POLARIZATION, &  ! SSMIS ch22
     RC_POLARIZATION, &  ! SSMIS ch23
     RC_POLARIZATION /)  ! SSMIS ch24

  ! Aqua HSB (assumed same as NOAA)
  ! -------------------------------
  INTEGER, PARAMETER :: HSB_AQUA_POLARIZATION( N_HSB_CHANNELS ) = &
  (/ VL_MIXED_POLARIZATION, &  ! HSB ch1 == AMSU-B ch2
     VL_MIXED_POLARIZATION, &  ! HSB ch2 == AMSU-B ch3
     VL_MIXED_POLARIZATION, &  ! HSB ch3 == AMSU-B ch4
     VL_MIXED_POLARIZATION /)  ! HSB ch4 == AMSU-B ch5

  ! Aqua AMSR-E
  ! -----------
  INTEGER, PARAMETER :: AMSRE_AQUA_POLARIZATION( N_AMSRE_CHANNELS ) = &
  (/ VL_POLARIZATION, &  ! AMSR-E ch1
     HL_POLARIZATION, &  ! AMSR-E ch2
     VL_POLARIZATION, &  ! AMSR-E ch3
     HL_POLARIZATION, &  ! AMSR-E ch4
     VL_POLARIZATION, &  ! AMSR-E ch5
     HL_POLARIZATION, &  ! AMSR-E ch6
     VL_POLARIZATION, &  ! AMSR-E ch7
     HL_POLARIZATION, &  ! AMSR-E ch8
     VL_POLARIZATION, &  ! AMSR-E ch9
     HL_POLARIZATION, &  ! AMSR-E ch10
     VL_POLARIZATION, &  ! AMSR-E ch11
     HL_POLARIZATION /)  ! AMSR-E ch12

  ! GCOMW1 AMSR2
  ! -----------
  INTEGER, PARAMETER :: AMSR2_GCOMW1_POLARIZATION( N_AMSR2_CHANNELS ) = &
  (/ VL_POLARIZATION, &  ! AMSR2 ch1
     HL_POLARIZATION, &  ! AMSR2 ch2
     VL_POLARIZATION, &  ! AMSR2 ch3
     HL_POLARIZATION, &  ! AMSR2 ch4
     VL_POLARIZATION, &  ! AMSR2 ch5
     HL_POLARIZATION, &  ! AMSR2 ch6
     VL_POLARIZATION, &  ! AMSR2 ch7
     HL_POLARIZATION, &  ! AMSR2 ch8
     VL_POLARIZATION, &  ! AMSR2 ch9
     HL_POLARIZATION, &  ! AMSR2 ch10
     VL_POLARIZATION, &  ! AMSR2 ch11
     HL_POLARIZATION, &  ! AMSR2 ch12
     VL_POLARIZATION, &  ! AMSR2 ch13
     HL_POLARIZATION /)  ! AMSR2 ch14

  ! MHS (needs verification)
  ! ------------------------
  INTEGER, PARAMETER :: MHS_POLARIZATION( N_MHS_CHANNELS ) = &
  (/ VL_MIXED_POLARIZATION, &  ! MHS ch16
     VL_MIXED_POLARIZATION, &  ! MHS ch17
     HL_MIXED_POLARIZATION, &  ! MHS ch18
     HL_MIXED_POLARIZATION, &  ! MHS ch19
     VL_MIXED_POLARIZATION /)  ! MHS ch20

  ! Coriolis WindSat
  ! ----------------
  INTEGER, PARAMETER :: WINDSAT_CORIOLIS_POLARIZATION( N_WINDSAT_CHANNELS ) = &
  (/ VL_POLARIZATION,         &  ! WindSat ch1,   6.8GHz R(v)
     HL_POLARIZATION,         &  ! WindSat ch2,   6.8GHz R(h)
     VL_POLARIZATION,         &  ! WindSat ch3,  10.7GHz R(v)
     HL_POLARIZATION,         &  ! WindSat ch4,  10.7GHz R(h)
     THIRD_STOKES_COMPONENT,  &  ! WindSat ch5,  10.7GHz R(+45) - R(-45)
     FOURTH_STOKES_COMPONENT, &  ! WindSat ch6,  10.7GHz R(rc) - R(lc)
     VL_POLARIZATION,         &  ! WindSat ch7,  18.7GHz R(v)
     HL_POLARIZATION,         &  ! WindSat ch8,  18.7GHz R(h)
     THIRD_STOKES_COMPONENT,  &  ! WindSat ch9,  18.7GHz R(+45) - R(-45)
     FOURTH_STOKES_COMPONENT, &  ! WindSat ch10, 18.7GHz R(rc) - R(lc)
     VL_POLARIZATION,         &  ! WindSat ch11, 23.8GHz vertical pol.
     HL_POLARIZATION,         &  ! WindSat ch12, 23.8GHz horizontal pol.
     VL_POLARIZATION,         &  ! WindSat ch13, 37.0GHz R(v)
     HL_POLARIZATION,         &  ! WindSat ch14, 37.0GHz R(h)
     THIRD_STOKES_COMPONENT,  &  ! WindSat ch15, 37.0GHz R(+45) - R(-45)
     FOURTH_STOKES_COMPONENT /)  ! WindSat ch16, 37.0GHz R(rc) - R(lc)  

  ! NPOESS-NPP ATMS
  ! --------------
  INTEGER, PARAMETER :: ATMS_NPP_POLARIZATION( N_ATMS_CHANNELS ) = &
  (/ VL_MIXED_POLARIZATION, &  ! ATMS ch1
     VL_MIXED_POLARIZATION, &  ! ATMS ch2
     HL_MIXED_POLARIZATION, &  ! ATMS ch3
     HL_MIXED_POLARIZATION, &  ! ATMS ch4
     HL_MIXED_POLARIZATION, &  ! ATMS ch5
     HL_MIXED_POLARIZATION, &  ! ATMS ch6
     HL_MIXED_POLARIZATION, &  ! ATMS ch7
     HL_MIXED_POLARIZATION, &  ! ATMS ch8
     HL_MIXED_POLARIZATION, &  ! ATMS ch9
     HL_MIXED_POLARIZATION, &  ! ATMS ch10
     HL_MIXED_POLARIZATION, &  ! ATMS ch11
     HL_MIXED_POLARIZATION, &  ! ATMS ch12
     HL_MIXED_POLARIZATION, &  ! ATMS ch13
     HL_MIXED_POLARIZATION, &  ! ATMS ch14
     HL_MIXED_POLARIZATION, &  ! ATMS ch15
     VL_MIXED_POLARIZATION, &  ! ATMS ch16
     HL_MIXED_POLARIZATION, &  ! ATMS ch17
     HL_MIXED_POLARIZATION, &  ! ATMS ch18
     HL_MIXED_POLARIZATION, &  ! ATMS ch19
     HL_MIXED_POLARIZATION, &  ! ATMS ch20
     HL_MIXED_POLARIZATION, &  ! ATMS ch21
     HL_MIXED_POLARIZATION /)  ! ATMS ch22

  ! TRMM TMI
  ! --------
  INTEGER, PARAMETER :: TMI_POLARIZATION( N_TMI_CHANNELS ) = &
  (/ VL_POLARIZATION, &  ! TMI ch1
     HL_POLARIZATION, &  ! TMI ch2
     VL_POLARIZATION, &  ! TMI ch3
     HL_POLARIZATION, &  ! TMI ch4
     VL_POLARIZATION, &  ! TMI ch5
     VL_POLARIZATION, &  ! TMI ch6
     HL_POLARIZATION, &  ! TMI ch7
     VL_POLARIZATION, &  ! TMI ch8
     HL_POLARIZATION /)  ! TMI ch9

  ! GPM GMI
  ! -------
  INTEGER, PARAMETER :: GMI_POLARIZATION( N_GMI_CHANNELS ) = &
  (/ VL_POLARIZATION, &  ! GMI ch1
     HL_POLARIZATION, &  ! GMI ch2
     VL_POLARIZATION, &  ! GMI ch3
     HL_POLARIZATION, &  ! GMI ch4
     VL_POLARIZATION, &  ! GMI ch5
     VL_POLARIZATION, &  ! GMI ch6
     HL_POLARIZATION, &  ! GMI ch7
     VL_POLARIZATION, &  ! GMI ch8
     HL_POLARIZATION, &  ! GMI ch9
     VL_POLARIZATION, &  ! GMI ch10
     HL_POLARIZATION, &  ! GMI ch11
     VL_POLARIZATION, &  ! GMI ch12
     VL_POLARIZATION /)  ! GMI ch13

  ! FengYun-3 MWRI
  ! --------------
  INTEGER, PARAMETER :: MWRI_POLARIZATION( N_MWRI_CHANNELS ) = &
  (/ VL_POLARIZATION, &  ! MWRI ch1
     HL_POLARIZATION, &  ! MWRI ch2
     VL_POLARIZATION, &  ! MWRI ch3
     HL_POLARIZATION, &  ! MWRI ch4
     VL_POLARIZATION, &  ! MWRI ch5
     HL_POLARIZATION, &  ! MWRI ch6
     VL_POLARIZATION, &  ! MWRI ch7
     HL_POLARIZATION, &  ! MWRI ch8
     VL_POLARIZATION, &  ! MWRI ch9
     HL_POLARIZATION /)  ! MWRI ch10

  ! FengYun-3 MWHS
  ! --------------
  INTEGER, PARAMETER :: MWHS_POLARIZATION( N_MWHS_CHANNELS ) = &
  (/ VL_MIXED_POLARIZATION, &  ! MWHS ch1
     HL_MIXED_POLARIZATION, &  ! MWHS ch2
     VL_MIXED_POLARIZATION, &  ! MWHS ch3
     VL_MIXED_POLARIZATION, &  ! MWHS ch4
     VL_MIXED_POLARIZATION /)  ! MWHS ch5

  ! FengYun-3 MWTS (copied from MSU)
  ! --------------
  INTEGER, PARAMETER :: MWTS_POLARIZATION( N_MWTS_CHANNELS ) = &
  (/ VL_MIXED_POLARIZATION, &  ! MWTS ch1
     HL_MIXED_POLARIZATION, &  ! MWTS ch2
     VL_MIXED_POLARIZATION, &  ! MWTS ch3
     HL_MIXED_POLARIZATION /)  ! MWTS ch4
     
  ! Megha-Tropiques MADRAS
  ! -------
  INTEGER, PARAMETER :: MADRAS_POLARIZATION( N_MADRAS_CHANNELS ) = &
  (/ VL_POLARIZATION, &  ! MADRAS ch1
     HL_POLARIZATION, &  ! MADRAS ch2
     VL_POLARIZATION, &  ! MADRAS ch3
     VL_POLARIZATION, &  ! MADRAS ch4
     HL_POLARIZATION, &  ! MADRAS ch5
     VL_POLARIZATION, &  ! MADRAS ch6
     HL_POLARIZATION, &  ! MADRAS ch7
     VL_POLARIZATION, &  ! MADRAS ch8
     HL_POLARIZATION /)  ! MADRAS ch9

  ! Megha-Tropiques SAPHIR
  ! -------
  INTEGER, PARAMETER :: SAPHIR_POLARIZATION( N_SAPHIR_CHANNELS ) = &
  (/ VL_MIXED_POLARIZATION, &  ! SAPHIR ch1
     VL_MIXED_POLARIZATION, &  ! SAPHIR ch2
     VL_MIXED_POLARIZATION, &  ! SAPHIR ch3
     VL_MIXED_POLARIZATION, &  ! SAPHIR ch4
     VL_MIXED_POLARIZATION, &  ! SAPHIR ch5
     VL_MIXED_POLARIZATION /)  ! SAPHIR ch6
     
  ! Hamsr-Grip 
  ! -------
  INTEGER, PARAMETER :: HAMSR_POLARIZATION( N_HAMSR_CHANNELS ) = &
  (/ VL_MIXED_POLARIZATION, &  ! HAMSR  ! ch1
     VL_MIXED_POLARIZATION, &  ! HAMSR  ! ch2
     VL_MIXED_POLARIZATION, &  ! HAMSR  ! ch3
     VL_MIXED_POLARIZATION, &  ! HAMSR  ! ch4
     VL_MIXED_POLARIZATION, &  ! HAMSR  ! ch5
     VL_MIXED_POLARIZATION, &  ! HAMSR  ! ch6
     VL_MIXED_POLARIZATION, &  ! HAMSR  ! ch7
     VL_MIXED_POLARIZATION, &  ! HAMSR  ! ch8
     VL_MIXED_POLARIZATION, &  ! HAMSR  ! ch9
     VL_MIXED_POLARIZATION, &  ! HAMSR  ! ch10
     VL_MIXED_POLARIZATION, &  ! HAMSR  ! ch11
     VL_MIXED_POLARIZATION, &  ! HAMSR  ! ch12
     VL_MIXED_POLARIZATION, &  ! HAMSR  ! ch13
     VL_MIXED_POLARIZATION, &  ! HAMSR  ! ch14
     VL_MIXED_POLARIZATION, &  ! HAMSR  ! ch15
     VL_MIXED_POLARIZATION, &  ! HAMSR  ! ch16
     VL_MIXED_POLARIZATION, &  ! HAMSR  ! ch17
     VL_MIXED_POLARIZATION, &  ! HAMSR  ! ch18
     VL_MIXED_POLARIZATION, &  ! HAMSR  ! ch19
     VL_MIXED_POLARIZATION, &  ! HAMSR  ! ch20
     VL_MIXED_POLARIZATION, &  ! HAMSR  ! ch21
     VL_MIXED_POLARIZATION, &  ! HAMSR  ! ch22
     VL_MIXED_POLARIZATION, &  ! HAMSR  ! ch23
     VL_MIXED_POLARIZATION, &  ! HAMSR  ! ch24
     VL_MIXED_POLARIZATION /)  ! HAMSR  ! ch25

  ! MicroMAS CubeSat
  ! -------
  INTEGER, PARAMETER :: MICROMAS_POLARIZATION( N_MICROMAS_CHANNELS ) = HL_POLARIZATION

  ! GeoStorm
  ! -------
  INTEGER, PARAMETER :: GEOSTORM_POLARIZATION( N_GEOSTORM_CHANNELS ) = HL_POLARIZATION

  ! MASC
  ! -------
  INTEGER, PARAMETER :: MASC_POLARIZATION( N_MASC_CHANNELS )         = HL_POLARIZATION


CONTAINS


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MW_SensorData_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of the MW_SensorData structure.
!
! CALLING SEQUENCE:
!       Status = MW_SensorData_Associated( MW_SensorData )
!
! OBJECTS:
!       MW_SensorData: Structure which is to have its member's
!                      status tested.
!                      UNITS:      N/A
!                      TYPE:       MW_SensorData_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:        The return value is a logical value indicating the
!                      status of the MW_SensorData members.
!                       .TRUE.  - if ANY of the MW_SensorData allocatable members
!                                 are in use.
!                       .FALSE. - if ALL of the MW_SensorData allocatable members
!                                 are not in use.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION MW_SensorData_Associated( MW_SensorData ) RESULT( Status )
    TYPE(MW_SensorData_type), INTENT(IN) :: MW_SensorData
    LOGICAL :: Status
    Status = MW_SensorData%Is_Allocated             
  END FUNCTION MW_SensorData_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MW_SensorData_Destroy
! 
! PURPOSE:
!       Elemental subroutine to re-initialize MW_SensorData objects.
!
! CALLING SEQUENCE:
!       CALL MW_SensorData_Destroy( MW_SensorData )
!
! OBJECTS:
!       MW_SensorData: Re-initialized MW_SensorData structure.
!                      UNITS:      N/A
!                      TYPE:       MW_SensorData_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE MW_SensorData_Destroy( MW_SensorData )
    TYPE(MW_SensorData_type), INTENT(OUT) :: MW_SensorData
    MW_SensorData%Is_Allocated     = .FALSE.
    MW_SensorData%n_Frequencies    = 0
    MW_SensorData%n_Channels       = 0
    MW_SensorData%Sensor_ID        = ''
    MW_SensorData%WMO_Satellite_ID = XSAT
    MW_SensorData%WMO_Sensor_ID    = XSEN   
  END SUBROUTINE MW_SensorData_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MW_SensorData_Create
! 
! PURPOSE:
!       Elemental subroutine to create an instance of an MW_SensorData object.
!
! CALLING SEQUENCE:
!       CALL MW_SensorData_Create( &
!              MW_SensorData, &
!              n_Channels   , &
!              n_Frequencies = n_Frequencies )
!
! OBJECTS:
!       MW_SensorData:      MW_SensorData object structure.
!                           UNITS:      N/A
!                           TYPE:       MW_SensorData_type
!                           DIMENSION:  Scalar or any rank
!                           ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Channels:         Number of sensor channels for the MW sensor.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Conformable
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       n_Frequencies:      Total number of points per channel for computing the
!                           channel frequencies and responses. Must be evenly
!                           divisible by 2 and 4, and must be > 0.
!                           If not specified, default value is 256.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Conformable
!                           ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE MW_SensorData_Create( &
    MW_SensorData, &  ! Output
    n_Channels   , &  ! Input
    n_Frequencies  )  ! Optional Input
    ! Arguments
    TYPE(MW_SensorData_type), INTENT(OUT) :: MW_SensorData
    INTEGER                 , INTENT(IN)  :: n_Channels
    INTEGER,       OPTIONAL , INTENT(IN)  :: n_Frequencies
    ! Local variables
    INTEGER :: local_n_frequencies
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Channels < 1 ) RETURN
    IF ( PRESENT(n_Frequencies) ) THEN
      IF ( n_Frequencies < 1 ) RETURN
      local_n_frequencies = n_Frequencies
    ELSE
      local_n_frequencies = DEFAULT_N_FREQUENCIES
    END IF
    ! ...Ensure local_n_frequencies is /2 and /4
    IF ( MOD(local_n_Frequencies,4) /= 0 ) RETURN

    
    ! Perform the allocation
    ALLOCATE( MW_SensorData%Sensor_Channel( n_Channels ), &
              MW_SensorData%Central_Frequency( n_Channels ), &
              MW_SensorData%Zeeman( n_Channels ), &
              MW_SensorData%Polarization( n_Channels ), &
              MW_SensorData%n_Sidebands( n_Channels ), &
              MW_SensorData%IF_Band( 2, MAX_N_SIDEBANDS, n_Channels ), &
              MW_SensorData%Delta_Frequency( n_Channels ), &
              MW_SensorData%Frequency( local_n_frequencies, n_Channels ), &
              MW_SensorData%Response( local_n_frequencies, n_Channels ), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN


    ! Initialise
    ! ...Dimensions
    MW_SensorData%n_Channels    = n_Channels
    MW_SensorData%n_Frequencies = local_n_frequencies
    ! ...Arrays
    MW_SensorData%Sensor_Channel    = 0
    MW_SensorData%Central_Frequency = ZERO
    MW_SensorData%Zeeman            = NO_ZEEMAN
    MW_SensorData%Polarization      = INVALID_POLARIZATION
    MW_SensorData%n_Sidebands       = 0
    MW_SensorData%IF_Band           = ZERO
    MW_SensorData%Delta_Frequency   = ZERO
    MW_SensorData%Frequency         = ZERO
    MW_SensorData%Response          = ZERO


    ! Set allocation indicator
    MW_SensorData%Is_Allocated = .TRUE.

  END SUBROUTINE MW_SensorData_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MW_SensorData_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a MW_SensorData object to stdout.
!
! CALLING SEQUENCE:
!       CALL MW_SensorData_Inspect( MW_SensorData )
!
! OBJECTS:
!       MW_SensorData: MW_SensorData object to display.
!                      UNITS:      N/A
!                      TYPE:       MW_SensorData_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE MW_SensorData_Inspect( MW_SensorData )
    TYPE(MW_SensorData_type), INTENT(IN) :: MW_SensorData
    INTEGER :: n
    WRITE(*,'(1x,"MW_SensorData OBJECT")')
    ! Release/version info
    ! Dimensions
    WRITE(*,'(3x,"n_Channels                :",1x,i0)') MW_SensorData%n_Channels
    WRITE(*,'(3x,"n_Frequencies             :",1x,i0)') MW_SensorData%n_Frequencies
    IF ( .NOT. MW_SensorData_Associated(MW_SensorData) ) RETURN
    ! Scalar info
    WRITE(*,'(3x,"Sensor_Id        :",1x,a )') TRIM(MW_SensorData%Sensor_Id)
    WRITE(*,'(3x,"WMO_Satellite_ID :",1x,i0)') MW_SensorData%WMO_Satellite_ID 
    WRITE(*,'(3x,"WMO_Sensor_ID    :",1x,i0)') MW_SensorData%WMO_Sensor_ID
    ! Data arrays
    WRITE(*,'(3x,"Sensor_Channel   :")')
    WRITE(*,'(10(1x,i5,:))') MW_SensorData%Sensor_Channel
    WRITE(*,'(3x,"Zeeman            :")')
    DO n = 1, MW_SensorData%n_Channels
      WRITE(*,'(5x,"Channel ",i0,": ",l1)') MW_SensorData%Sensor_Channel(n), &
                                            MW_SensorData%Zeeman(n) /= 0
    END DO
    WRITE(*,'(3x,"Polarization      :")')
    DO n = 1, MW_SensorData%n_Channels
      WRITE(*,'(5x,"Channel ",i0,": ",a)') MW_SensorData%Sensor_Channel(n), &
                                           POLARIZATION_TYPE_NAME(MW_SensorData%Polarization(n))
    END DO
    WRITE(*,'(3x,"n_Sidebands       :")')
    WRITE(*,'(10(1x,i5,:))') MW_SensorData%n_Sidebands
    WRITE(*,'(3x,"Central_Frequency :")')
    WRITE(*,'(5(1x,es13.6,:))') MW_SensorData%Central_Frequency
    WRITE(*,'(3x,"IF_Band           :")')
    WRITE(*,'(5(1x,es13.6,:))') MW_SensorData%IF_Band
    WRITE(*,'(3x,"Delta_Frequency   :")')
    WRITE(*,'(5(1x,es13.6,:))') MW_SensorData%Delta_Frequency
    WRITE(*,'(3x,"Frequency         :")')
    WRITE(*,'(5(1x,es13.6,:))') MW_SensorData%Frequency
    WRITE(*,'(3x,"Response          :")')
    WRITE(*,'(5(1x,es13.6,:))') MW_SensorData%Response
    
  END SUBROUTINE MW_SensorData_Inspect
  
  
!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MW_SensorData_ValidRelease
!
! PURPOSE:
!       Function to check the MW_SensorData Release value.
!
! CALLING SEQUENCE:
!       IsValid = MW_SensorData_ValidRelease( MW_SensorData )
!
! INPUTS:
!       MW_SensorData: MW_SensorData object for which the Release component
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       MW_SensorData_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       IsValid:       Logical value defining the release validity.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION MW_SensorData_ValidRelease( MW_SensorData ) RESULT( IsValid )
    ! Arguments
    TYPE(MW_SensorData_type), INTENT(IN) :: MW_SensorData
    ! Function result
    LOGICAL :: IsValid
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'MW_SensorData_ValidRelease'
    ! Local variables
    CHARACTER(ML) :: msg

    ! Set up
    IsValid = .TRUE.


    ! Check release is not too old
    IF ( MW_SensorData%Release < MW_SENSORDATA_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An MW_SensorData data update is needed. ", &
                  &"MW_SensorData release is ",i0,". Valid release is ",i0,"." )' ) &
                  MW_SensorData%Release, MW_SENSORDATA_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    END IF


    ! Check release is not too new
    IF ( MW_SensorData%Release > MW_SENSORDATA_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An MW_SensorData software update is needed. ", &
                  &"MW_SensorData release is ",i0,". Valid release is ",i0,"." )' ) &
                  MW_SensorData%Release, MW_SENSORDATA_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    END IF

  END FUNCTION MW_SensorData_ValidRelease


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MW_SensorData_Info
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about a MW_SensorData object.
!
! CALLING SEQUENCE:
!       CALL MW_SensorData_Info( MW_SensorData, Info )
!
! OBJECTS:
!       MW_SensorData: MW_SensorData object about which info is required.
!                      UNITS:      N/A
!                      TYPE:       MW_SensorData_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Info:          String containing version and dimension information
!                      about the MW_SensorData object.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE MW_SensorData_Info( MW_SensorData, Info )
    ! Arguments
    TYPE(MW_SensorData_type), INTENT(IN)  :: MW_SensorData
    CHARACTER(*),     INTENT(OUT) :: Info
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(2000) :: Long_String

    ! Write the required data to the local string
    WRITE( Long_String, &
           '(a,1x,"MW_SensorData RELEASE.VERSION: ",i2,".",i2.2,a,3x, &
           &"N_CHANNELS=",i0,2x,&
           &"N_FREQUENCIES=",i0 )' ) &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           MW_SensorData%Release, MW_SensorData%Version, &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           MW_SensorData%n_Channels, MW_SensorData%n_Frequencies
                       
    ! Trim the output based on the
    ! dummy argument string length
    Info = Long_String(1:MIN(LEN(Info), LEN_TRIM(Long_String)))

  END SUBROUTINE MW_SensorData_Info
 
 
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MW_SensorData_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL MW_SensorData_DefineVersion( Id )
!
! OUTPUTS:
!       Id:    Character string containing the version Id information
!              for the module.
!              UNITS:      N/A
!              TYPE:       CHARACTER(*)
!              DIMENSION:  Scalar
!              ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE MW_SensorData_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE MW_SensorData_DefineVersion


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MW_SensorData_Load
!
! PURPOSE:
!       Function to load an MW_SensorData object with information
!       based on input sensor ID.
!
! CALLING SEQUENCE:
!       Error_Status = MW_SensorData_Load( &
!                        MW_SensorData, &
!                        Sensor_Id    , &
!                        n_Frequencies = n_Frequencies )
!
! OBJECTS:
!       MW_SensorData:     MW_SensorData object containing the required
!                          sensor data.
!                          UNITS:      N/A
!                          TYPE:       MW_SensorData_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN OUT)
!
! INPUTS:
!       Sensor_Id:         Character string sensor/platform identifier.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL INPUTS:
!       n_Frequencies:     Total number of points per channel for computing the
!                          channel frequencies and responses. Must be evenly
!                          divisible by 2 and 4, and must be > 0.
!                          If not specified, default value is 256.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Error_Status:      The return value is an integer defining the error status.
!                          The error codes are defined in the Message_Handler module.
!                          If == SUCCESS the data load was successful
!                             == FAILURE an error occurred
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION MW_SensorData_Load( &
    MW_SensorData, &  ! Output
    Sensor_Id    , &  ! Input
    n_Frequencies) &  ! Optional input
  RESULT( Error_Status )
    ! Arguments
    TYPE(MW_SensorData_type), INTENT(OUT) :: MW_SensorData
    CHARACTER(*)            , INTENT(IN)  :: Sensor_Id
    INTEGER     ,   OPTIONAL, INTENT(IN)  :: n_Frequencies
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'MW_SensorData_Load'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: n_Halfpoints
    INTEGER :: i, n
    INTEGER :: idx(1)
    INTEGER :: n_Points(2)
    INTEGER :: n_OffsetPoints
    INTEGER :: l, ln
    INTEGER :: i_Upper, i_Lower
    REAL(fp) :: df, f1, f

    ! Set up
    Error_Status = SUCCESS
    ! ...Check sensor id is valid
    n = COUNT(VALID_SENSOR_ID == Sensor_Id)
    IF ( n == 0 ) THEN
      Message = 'Specified Sensor_Id, '//TRIM(Sensor_Id)//', not found in valid list'
      CALL Load_Cleanup(); RETURN
    END IF
    ! ...Determine the sensor ID index
    idx = PACK((/ (i,i=1,N_VALID_SENSORS) /), VALID_SENSOR_ID == Sensor_Id)


    ! Allocate the MW_SensorData structure
    CALL MW_SensorData_Create( &
      MW_SensorData, &
      VALID_N_CHANNELS( idx(1) ), &
      n_Frequencies = n_Frequencies )
    IF ( .NOT. MW_SensorData_Associated( MW_SensorData ) ) THEN
      Message = 'Error allocating MW_SensorData object'
      CALL Load_Cleanup(); RETURN
    END IF


    ! Assign sensor info and defaults
    MW_SensorData%Sensor_ID        = VALID_SENSOR_ID( idx(1) )
    MW_SensorData%WMO_Satellite_ID = VALID_WMO_SATELLITE_ID( idx(1) )
    MW_SensorData%WMO_Sensor_ID    = VALID_WMO_SENSOR_ID( idx(1) )
    MW_SensorData%Zeeman = NO_ZEEMAN
    
    ! Load the structure with the relevant sensor's data
    Load_Data: SELECT CASE ( TRIM(MW_SensorData%Sensor_ID) )

      CASE ('msu_tirosn','msu_n06','msu_n07','msu_n08','msu_n09',&
            'msu_n10','msu_n11','msu_n12','msu_n14'           )
        MW_SensorData%Sensor_Channel    = MSU_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = MSU_F0
        MW_SensorData%Polarization      = MSU_POLARIZATION
        MW_SensorData%n_Sidebands       = MSU_N_SIDEBANDS
        MW_SensorData%IF_Band           = MSU_IF_BAND

      CASE ('amsua_n15')
        MW_SensorData%Sensor_Channel    = AMSUA_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = AMSUA_N15_F0
        MW_SensorData%Zeeman            = AMSUA_ZEEMAN
        MW_SensorData%Polarization      = AMSUA_POLARIZATION
        MW_SensorData%n_Sidebands       = AMSUA_N_SIDEBANDS
        MW_SensorData%IF_Band           = AMSUA_N15_IF_BAND

      CASE ('amsub_n15')
        MW_SensorData%Sensor_Channel    = AMSUB_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = AMSUB_N15_F0
        MW_SensorData%Polarization      = AMSUB_POLARIZATION
        MW_SensorData%n_Sidebands       = AMSUB_N_SIDEBANDS
        MW_SensorData%IF_Band           = AMSUB_N15_IF_BAND

      CASE ('amsua_n16')
        MW_SensorData%Sensor_Channel    = AMSUA_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = AMSUA_N16_F0
        MW_SensorData%Zeeman            = AMSUA_ZEEMAN
        MW_SensorData%Polarization      = AMSUA_POLARIZATION
        MW_SensorData%n_Sidebands       = AMSUA_N_SIDEBANDS
        MW_SensorData%IF_Band           = AMSUA_N16_IF_BAND

      CASE ('amsub_n16')
        MW_SensorData%Sensor_Channel    = AMSUB_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = AMSUB_N16_F0
        MW_SensorData%Polarization      = AMSUB_POLARIZATION
        MW_SensorData%n_Sidebands       = AMSUB_N_SIDEBANDS
        MW_SensorData%IF_Band           = AMSUB_N16_IF_BAND

      CASE ('amsua_n17')
        MW_SensorData%Sensor_Channel    = AMSUA_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = AMSUA_N17_F0
        MW_SensorData%Zeeman            = AMSUA_ZEEMAN
        MW_SensorData%Polarization      = AMSUA_POLARIZATION
        MW_SensorData%n_Sidebands       = AMSUA_N_SIDEBANDS
        MW_SensorData%IF_Band           = AMSUA_N17_IF_BAND

      CASE ('amsub_n17')
        MW_SensorData%Sensor_Channel    = AMSUB_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = AMSUB_N17_F0
        MW_SensorData%Polarization      = AMSUB_POLARIZATION
        MW_SensorData%n_Sidebands       = AMSUB_N_SIDEBANDS
        MW_SensorData%IF_Band           = AMSUB_N17_IF_BAND


      CASE ('ssmi_f13','ssmi_f14','ssmi_f15','ssmi_f08','ssmi_f10','ssmi_f11')
        MW_SensorData%Sensor_Channel    = SSMI_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = SSMI_F0
        MW_SensorData%Polarization      = SSMI_POLARIZATION
        MW_SensorData%n_Sidebands       = SSMI_N_SIDEBANDS
        MW_SensorData%IF_Band           = SSMI_IF_BAND

      CASE ('ssmt1_f13','ssmt1_f14','ssmt1_f15')
        MW_SensorData%Sensor_Channel    = SSMT1_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = SSMT1_F0
        MW_SensorData%Polarization      = SSMT1_POLARIZATION
        MW_SensorData%n_Sidebands       = SSMT1_N_SIDEBANDS
        MW_SensorData%IF_Band           = SSMT1_IF_BAND

      CASE ('ssmt2_f13','ssmt2_f14','ssmt2_f15')
        MW_SensorData%Sensor_Channel    = SSMT2_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = SSMT2_F0
        MW_SensorData%Polarization      = SSMT2_POLARIZATION
        MW_SensorData%n_Sidebands       = SSMT2_N_SIDEBANDS
        MW_SensorData%IF_Band           = SSMT2_IF_BAND

      CASE ('ssmis_f16')
        MW_SensorData%Sensor_Channel    = SSMIS_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = SSMIS_F16_F0
        MW_SensorData%Zeeman            = SSMIS_ZEEMAN
        MW_SensorData%Polarization      = SSMIS_F16_POLARIZATION
        MW_SensorData%n_Sidebands       = SSMIS_N_SIDEBANDS
        MW_SensorData%IF_Band           = SSMIS_F16_IF_BAND

      CASE ('ssmis_f17')
        MW_SensorData%Sensor_Channel    = SSMIS_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = SSMIS_F17_F0
        MW_SensorData%Zeeman            = SSMIS_ZEEMAN
        MW_SensorData%Polarization      = SSMIS_POLARIZATION
        MW_SensorData%n_Sidebands       = SSMIS_N_SIDEBANDS
        MW_SensorData%IF_Band           = SSMIS_F17_IF_BAND

      CASE ('ssmis_f18')
        MW_SensorData%Sensor_Channel    = SSMIS_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = SSMIS_F18_F0
        MW_SensorData%Zeeman            = SSMIS_ZEEMAN
        MW_SensorData%Polarization      = SSMIS_POLARIZATION
        MW_SensorData%n_Sidebands       = SSMIS_N_SIDEBANDS
        MW_SensorData%IF_Band           = SSMIS_F18_IF_BAND

      CASE ('ssmis_f19')
        MW_SensorData%Sensor_Channel    = SSMIS_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = SSMIS_F19_F0
        MW_SensorData%Zeeman            = SSMIS_ZEEMAN
        MW_SensorData%Polarization      = SSMIS_POLARIZATION
        MW_SensorData%n_Sidebands       = SSMIS_N_SIDEBANDS
        MW_SensorData%IF_Band           = SSMIS_F19_IF_BAND

      CASE ('ssmis_f20')
        MW_SensorData%Sensor_Channel    = SSMIS_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = SSMIS_F20_F0
        MW_SensorData%Zeeman            = SSMIS_ZEEMAN
        MW_SensorData%Polarization      = SSMIS_POLARIZATION
        MW_SensorData%n_Sidebands       = SSMIS_N_SIDEBANDS
        MW_SensorData%IF_Band           = SSMIS_F20_IF_BAND

      CASE ('amsua_aqua')
        MW_SensorData%Sensor_Channel    = AMSUA_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = AMSUA_AQUA_F0
        MW_SensorData%Zeeman            = AMSUA_ZEEMAN
        MW_SensorData%Polarization      = AMSUA_POLARIZATION
        MW_SensorData%n_Sidebands       = AMSUA_N_SIDEBANDS
        MW_SensorData%IF_Band           = AMSUA_AQUA_IF_BAND

      CASE ('hsb_aqua')
        MW_SensorData%Sensor_Channel    = HSB_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = HSB_AQUA_F0
        MW_SensorData%Polarization      = HSB_AQUA_POLARIZATION
        MW_SensorData%n_Sidebands       = HSB_N_SIDEBANDS
        MW_SensorData%IF_Band           = HSB_AQUA_IF_BAND

      CASE ('amsre_aqua')
        MW_SensorData%Sensor_Channel    = AMSRE_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = AMSRE_AQUA_F0
        MW_SensorData%Polarization      = AMSRE_AQUA_POLARIZATION
        MW_SensorData%n_Sidebands       = AMSRE_N_SIDEBANDS
        MW_SensorData%IF_Band           = AMSRE_AQUA_IF_BAND

      CASE ('amsr2_gcom-w1')
        MW_SensorData%Sensor_Channel    = AMSR2_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = AMSR2_GCOMW1_F0
        MW_SensorData%Polarization      = AMSR2_GCOMW1_POLARIZATION
        MW_SensorData%n_Sidebands       = AMSR2_N_SIDEBANDS
        MW_SensorData%IF_Band           = AMSR2_GCOMW1_IF_BAND

      CASE ('amsua_n18')
        MW_SensorData%Sensor_Channel    = AMSUA_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = AMSUA_N18_F0
        MW_SensorData%Zeeman            = AMSUA_ZEEMAN
        MW_SensorData%Polarization      = AMSUA_POLARIZATION
        MW_SensorData%n_Sidebands       = AMSUA_N_SIDEBANDS
        MW_SensorData%IF_Band           = AMSUA_N18_IF_BAND

      CASE ('mhs_n18')
        MW_SensorData%Sensor_Channel    = MHS_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = MHS_N18_F0
        MW_SensorData%Polarization      = MHS_POLARIZATION
        MW_SensorData%n_Sidebands       = MHS_N_SIDEBANDS
        MW_SensorData%IF_Band           = MHS_N18_IF_BAND

      CASE ('windsat_coriolis')
        MW_SensorData%Sensor_Channel    = WINDSAT_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = WINDSAT_CORIOLIS_F0
        MW_SensorData%Polarization      = WINDSAT_CORIOLIS_POLARIZATION
        MW_SensorData%n_Sidebands       = WINDSAT_N_SIDEBANDS
        MW_SensorData%IF_Band           = WINDSAT_CORIOLIS_IF_BAND

      CASE ('atms_npp')
        MW_SensorData%Sensor_Channel    = ATMS_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = ATMS_NPP_F0
        MW_SensorData%Polarization      = ATMS_NPP_POLARIZATION
        MW_SensorData%n_Sidebands       = ATMS_N_SIDEBANDS
        MW_SensorData%IF_Band           = ATMS_NPP_IF_BAND

      CASE ('amsua_n19')
        MW_SensorData%Sensor_Channel    = AMSUA_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = AMSUA_N19_F0
        MW_SensorData%Zeeman            = AMSUA_ZEEMAN
        MW_SensorData%Polarization      = AMSUA_POLARIZATION
        MW_SensorData%n_Sidebands       = AMSUA_N_SIDEBANDS
        MW_SensorData%IF_Band           = AMSUA_N19_IF_BAND

      CASE ('mhs_n19')
        MW_SensorData%Sensor_Channel    = MHS_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = MHS_N19_F0
        MW_SensorData%Polarization      = MHS_POLARIZATION
        MW_SensorData%n_Sidebands       = MHS_N_SIDEBANDS
        MW_SensorData%IF_Band           = MHS_N19_IF_BAND

      CASE ('amsua_metop-a')
        MW_SensorData%Sensor_Channel    = AMSUA_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = AMSUA_METOPA_F0
        MW_SensorData%Zeeman            = AMSUA_ZEEMAN
        MW_SensorData%Polarization      = AMSUA_POLARIZATION
        MW_SensorData%n_Sidebands       = AMSUA_N_SIDEBANDS
        MW_SensorData%IF_Band           = AMSUA_METOPA_IF_BAND

      CASE ('mhs_metop-a')
        MW_SensorData%Sensor_Channel    = MHS_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = MHS_METOPA_F0
        MW_SensorData%Polarization      = MHS_POLARIZATION
        MW_SensorData%n_Sidebands       = MHS_N_SIDEBANDS
        MW_SensorData%IF_Band           = MHS_METOPA_IF_BAND

      CASE ('amsua_metop-b')
        MW_SensorData%Sensor_Channel    = AMSUA_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = AMSUA_METOPB_F0
        MW_SensorData%Zeeman            = AMSUA_ZEEMAN
        MW_SensorData%Polarization      = AMSUA_POLARIZATION
        MW_SensorData%n_Sidebands       = AMSUA_N_SIDEBANDS
        MW_SensorData%IF_Band           = AMSUA_METOPB_IF_BAND

      CASE ('mhs_metop-b')
        MW_SensorData%Sensor_Channel    = MHS_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = MHS_METOPB_F0
        MW_SensorData%Polarization      = MHS_POLARIZATION
        MW_SensorData%n_Sidebands       = MHS_N_SIDEBANDS
        MW_SensorData%IF_Band           = MHS_METOPB_IF_BAND

      CASE ('amsua_metop-c')
        MW_SensorData%Sensor_Channel    = AMSUA_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = AMSUA_METOPC_F0
        MW_SensorData%Zeeman            = AMSUA_ZEEMAN
        MW_SensorData%Polarization      = AMSUA_POLARIZATION
        MW_SensorData%n_Sidebands       = AMSUA_N_SIDEBANDS
        MW_SensorData%IF_Band           = AMSUA_METOPC_IF_BAND

      CASE ('mhs_metop-c')
        MW_SensorData%Sensor_Channel    = MHS_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = MHS_METOPC_F0
        MW_SensorData%Polarization      = MHS_POLARIZATION
        MW_SensorData%n_Sidebands       = MHS_N_SIDEBANDS
        MW_SensorData%IF_Band           = MHS_METOPC_IF_BAND

      CASE ('tmi_trmm')
        MW_SensorData%Sensor_Channel    = TMI_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = TMI_F0
        MW_SensorData%Polarization      = TMI_POLARIZATION
        MW_SensorData%n_Sidebands       = TMI_N_SIDEBANDS
        MW_SensorData%IF_Band           = TMI_IF_BAND

      CASE ('gmi_gpm')
        MW_SensorData%Sensor_Channel    = GMI_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = GMI_F0
        MW_SensorData%Polarization      = GMI_POLARIZATION
        MW_SensorData%n_Sidebands       = GMI_N_SIDEBANDS
        MW_SensorData%IF_Band           = GMI_IF_BAND

      CASE ('mwri_fy3a')
        MW_SensorData%Sensor_Channel    = MWRI_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = MWRI_F0
        MW_SensorData%Polarization      = MWRI_POLARIZATION
        MW_SensorData%n_Sidebands       = MWRI_N_SIDEBANDS
        MW_SensorData%IF_Band           = MWRI_IF_BAND

      CASE ('mwhs_fy3a')
        MW_SensorData%Sensor_Channel    = MWHS_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = MWHS_FY3A_F0
        MW_SensorData%Polarization      = MWHS_POLARIZATION
        MW_SensorData%n_Sidebands       = MWHS_N_SIDEBANDS
        MW_SensorData%IF_Band           = MWHS_FY3A_IF_BAND

      CASE ('mwts_fy3a')
        MW_SensorData%Sensor_Channel    = MWTS_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = MWTS_FY3A_F0
        MW_SensorData%Polarization      = MWTS_POLARIZATION
        MW_SensorData%n_Sidebands       = MWTS_N_SIDEBANDS
        MW_SensorData%IF_Band           = MWTS_FY3A_IF_BAND

      CASE ('mwri_fy3b')
        MW_SensorData%Sensor_Channel    = MWRI_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = MWRI_F0
        MW_SensorData%Polarization      = MWRI_POLARIZATION
        MW_SensorData%n_Sidebands       = MWRI_N_SIDEBANDS
        MW_SensorData%IF_Band           = MWRI_IF_BAND

      CASE ('mwhs_fy3b')
        MW_SensorData%Sensor_Channel    = MWHS_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = MWHS_FY3B_F0
        MW_SensorData%Polarization      = MWHS_POLARIZATION
        MW_SensorData%n_Sidebands       = MWHS_N_SIDEBANDS
        MW_SensorData%IF_Band           = MWHS_FY3B_IF_BAND

      CASE ('mwts_fy3b')
        MW_SensorData%Sensor_Channel    = MWTS_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = MWTS_FY3B_F0
        MW_SensorData%Polarization      = MWTS_POLARIZATION
        MW_SensorData%n_Sidebands       = MWTS_N_SIDEBANDS
        MW_SensorData%IF_Band           = MWTS_FY3B_IF_BAND

      CASE ('madras_meghat')
        MW_SensorData%Sensor_Channel    = MADRAS_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = MADRAS_MEGHAT_F0
        MW_SensorData%Polarization      = MADRAS_POLARIZATION
        MW_SensorData%n_Sidebands       = MADRAS_N_SIDEBANDS
        MW_SensorData%IF_Band           = MADRAS_MEGHAT_IF_BAND
 
      CASE ('saphir_meghat')
        MW_SensorData%Sensor_Channel    = SAPHIR_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = SAPHIR_MEGHAT_F0
        MW_SensorData%Polarization      = SAPHIR_POLARIZATION
        MW_SensorData%n_Sidebands       = SAPHIR_N_SIDEBANDS
        MW_SensorData%IF_Band           = SAPHIR_MEGHAT_IF_BAND

      CASE ('hamsr_grip')
        MW_SensorData%Sensor_Channel    = HAMSR_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = HAMSR_GRIP_F0
        MW_SensorData%Polarization      = HAMSR_POLARIZATION
        MW_SensorData%n_Sidebands       = HAMSR_N_SIDEBANDS
        MW_SensorData%IF_Band           = HAMSR_GRIP_IF_BAND

      CASE ('micromas_cs00','micromas_cs01','micromas_cs02','micromas_cs03','micromas_cs04','micromas_cs05')
        MW_SensorData%Sensor_Channel    = MICROMAS_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = MICROMAS_F0
        MW_SensorData%Polarization      = MICROMAS_POLARIZATION
        MW_SensorData%n_Sidebands       = MICROMAS_N_SIDEBANDS
        MW_SensorData%IF_Band           = MICROMAS_IF_BAND

      CASE ('geostorm_proposed')
        MW_SensorData%Sensor_Channel    = GEOSTORM_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = GEOSTORM_F0
        MW_SensorData%Polarization      = GEOSTORM_POLARIZATION
        MW_SensorData%n_Sidebands       = GEOSTORM_N_SIDEBANDS
        MW_SensorData%IF_Band           = GEOSTORM_IF_BAND

      CASE ('masc_cubesat')
        MW_SensorData%Sensor_Channel    = MASC_SENSOR_CHANNEL
        MW_SensorData%Central_Frequency = MASC_F0
        MW_SensorData%Polarization      = MASC_POLARIZATION
        MW_SensorData%n_Sidebands       = MASC_N_SIDEBANDS
        MW_SensorData%IF_Band           = MASC_IF_BAND


      ! No match! Should never get here!
      CASE DEFAULT
        Message = 'No sensor ID match!!'
        CALL Load_Cleanup(); RETURN

    END SELECT Load_Data


    ! Compute the frequency response grids
    ! ...The number of halfpoints
    n_Halfpoints = MW_SensorData%n_Frequencies/2
    ! ...Begin channel loop
    Channel_Response_Loop: DO l = 1, MW_SensorData%n_Channels


      ! NOTE: The logical test
      !         IF( MW_SensorData%IF_Band(1,1,l) == ZERO ) THEN
      !       is to test for no stopband and single passband
      
      
      ! Compute the number of points in the sideband(s)
      ! ...First determine the total frequency range in the sidebands
      df = ZERO
      IF( MW_SensorData%IF_Band(1,1,l) == ZERO ) THEN
        df = TWO*MW_SensorData%IF_Band(2,1,l)
      ELSE
        DO ln = 1, MW_SensorData%n_Sidebands( l )
          df = df + ( MW_SensorData%IF_Band(2,ln,l) - MW_SensorData%IF_Band(1,ln,l) )
        END DO
      ENDIF
      
      ! ...Now determine the frequency interval for this frequency
      !    range to provide the required number of points. Note that
      !    for > 1 sideband channels, the divisor is n-2, not n-1.
      !    This is to exclude the "space" between the sidebands in
      !    the frequency interval calculation. E.g.:
      ! --
      !       Sideband 1             Sideband 2
      !     |-----------|      |-------------------|
      !     x   x   x   x      x   x   x   x   x   x
      !     1   2   3   4      5   6   7   8   9  10   n_Halfpoints (n)
      ! --
      !       1   2   3          4   5   6   7   8     INTERVALS    (n-2)
      !
      IF(MW_SensorData%IF_Band(1,1,l) == ZERO) THEN
        MW_SensorData%Delta_Frequency(l) = df / REAL(MW_SensorData%n_Frequencies-1,fp)        
      ELSE IF( MW_SensorData%n_Sidebands( l ) == 1) THEN
        MW_SensorData%Delta_Frequency(l) = df / REAL(n_Halfpoints-1,fp)
      ELSE
        MW_SensorData%Delta_Frequency(l) = df / REAL(n_Halfpoints-2,fp)
      END IF
      ! ...Now determine the number of points for each sideband.
      n_Points(:) = 0
      IF( MW_SensorData%IF_Band(1,1,l) == ZERO) THEN
        ! Case of single passband *AND* no stopband
        n_Points(1) = MW_SensorData%n_Frequencies
      ELSE
        ! All other channels:
        ! - single passband with stopband
        ! - double passband
        ! - quadruple passband
        DO ln = 1, MW_SensorData%n_Sidebands( l )
          df = MW_SensorData%IF_Band(2,ln,l) - MW_SensorData%IF_Band(1,ln,l)
          n_Points(ln) = NINT(df/MW_SensorData%Delta_Frequency(l)) + 1
        END DO
        ! Check the result
        IF ( SUM(n_Points) /= n_HalfPoints ) THEN
          WRITE( Message,'("Error comparing SUM(n_Points) to n_HalfPoints for channel ",i0,&
                          &" of ",a,". Sum of n_Points ",i0)' ) &
                          MW_SensorData%Sensor_Channel(l), &
                          TRIM(MW_SensorData%Sensor_Id), &
                          SUM(n_Points)
          CALL Load_Cleanup(); RETURN
        END IF
      END IF


      ! Fill the frequency array. It's a bit convoluted as I
      ! want the frequencies to be in ascending order.
      !
      ! For the first sideband loop:
      !
      !   -Sideband2   -Sideband1    F0     +Sideband1   +Sideband2
      !                               ^
      !    |------|   |----------|    |    |----------|   |------|
      !                               |
      !               |<.........|         |.........>|
      !                  n_Lower             n_Upper
      !
      ! For the second sideband loop:
      !
      !                              F0      Sideband1    Sideband2
      !                               ^                
      !    |------|   |----------|    |    |----------|   |------|
      !                               |
      !    |<.....|                                       |.....>|
      !     n_Lower                                        n_Upper
      !
      ! -----------------------------------------------------------
      
      IF( MW_SensorData%IF_Band(1,1,l) == ZERO) THEN
        ! Fill the frequencies for instances of no stopband and single passband      
        f1 = MW_SensorData%Central_Frequency(l) - MW_SensorData%IF_Band(2,1,l)
        DO i = 1, n_Points(1)
          MW_SensorData%Frequency(i,l) = f1 + ( REAL(i-1,fp) * MW_SensorData%Delta_Frequency(l) )
        END DO
      ELSE      
        ! Initialise the sideband point offset. This is the
        ! the number of points offset from the central frequency
        ! for a sideband. 
        n_OffsetPoints = 0
        ! Loop over the number of sidebands
        DO ln = 1, MW_SensorData%n_Sidebands(l)

          ! Assign the start intermediate frequency for the sideband
          f1 = MW_SensorData%IF_Band(1,ln,l)

          ! Loop over the number of points in the sideband
          DO i = 1, n_Points( ln )

            ! Determine the positions of the frequencies in the array
            i_Upper = n_Halfpoints + n_OffsetPoints + i
            i_Lower = n_Halfpoints - n_OffsetPoints - i + 1

            ! Compute the frequency offset
            f = f1 + ( REAL(i-1,fp) * MW_SensorData%Delta_Frequency(l) )

            ! Apply the offset to the central frequency
            MW_SensorData%Frequency(i_Upper,l) = MW_SensorData%Central_Frequency(l) + f
            MW_SensorData%Frequency(i_Lower,l) = MW_SensorData%Central_Frequency(l) - f

          END DO

          ! Update the number of offset points
          n_OffsetPoints = n_OffsetPoints + n_Points( ln )
        END DO
        
      END IF

      ! The response is assumed unity
      MW_SensorData%Response(:,l) = ONE
    END DO Channel_Response_Loop
 
  CONTAINS
  
    SUBROUTINE Load_Cleanup()
      CALL MW_SensorData_Destroy( MW_SensorData )
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
    END SUBROUTINE Load_Cleanup
    
  END FUNCTION MW_SensorData_Load
  

  FUNCTION MW_SensorData_Get_Sensor_ID( Reset ) RESULT( Sensor_Id )
    ! Arguments
    LOGICAL, OPTIONAL, INTENT(IN) :: Reset
    ! Function result
    CHARACTER(SL) :: Sensor_ID
    ! Local variables
    INTEGER, SAVE :: Idx = 0
    
    ! Reset the index counter if required
    IF ( PRESENT(Reset) ) THEN
      IF ( Reset ) Idx = 0
    END IF
    
    ! Increment and test the index counter
    Idx = Idx + 1
    IF ( Idx > N_VALID_SENSORS ) THEN
      Sensor_Id = ''
      RETURN
    END IF
    
    ! Assign a sensor Id
    Sensor_ID = VALID_SENSOR_ID(Idx)
    
  END FUNCTION MW_SensorData_Get_Sensor_ID
  
  
!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!------------------------------------------------------------------------------
!
! NAME:
!       MW_SensorData_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two MW_SensorData objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = MW_SensorData_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two MW_SensorData objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       MW_SensorData_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       is_equal:      Logical value indicating whether the inputs are equal.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as inputs.
!
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION MW_SensorData_Equal( x, y ) RESULT( is_equal )
    TYPE(MW_SensorData_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.
   
    ! Check the object association status
    IF ( (.NOT. MW_SensorData_Associated(x)) .OR. &
         (.NOT. MW_SensorData_Associated(y))      ) RETURN

    ! Check contents
    ! ...Release/version info
    IF ( (x%Release /= y%Release) .OR. &
         (x%Version /= y%Version) ) RETURN
    ! ...Dimensions
    IF ( (x%n_Channels    /= y%n_Channels    ) .OR. &
         (x%n_Frequencies /= y%n_Frequencies ) ) RETURN
    ! ...Scalars
    IF ( (x%Sensor_Id        /= y%Sensor_Id       ) .OR. &
         (x%WMO_Satellite_ID /= y%WMO_Satellite_ID) .OR. &
         (x%WMO_Sensor_ID    /= y%WMO_Sensor_ID   ) ) RETURN
    ! ...Arrays
    IF ( ALL(x%Sensor_Channel       ==     y%Sensor_Channel    ) .AND. &
         ALL(x%Zeeman               ==     y%Zeeman            ) .AND. &
         ALL(x%Polarization         ==     y%Polarization      ) .AND. &
         ALL(x%n_Sidebands          ==     y%n_Sidebands       ) .AND. &
         ALL(x%Central_Frequency .EqualTo. y%Central_Frequency ) .AND. &
         ALL(x%IF_Band           .EqualTo. y%IF_Band           ) .AND. &
         ALL(x%Delta_Frequency   .EqualTo. y%Delta_Frequency   ) .AND. &
         ALL(x%Frequency         .EqualTo. y%Frequency         ) .AND. &
         ALL(x%Response          .EqualTo. y%Response          ) ) &
      is_equal = .TRUE.

  END FUNCTION MW_SensorData_Equal

END MODULE MW_SensorData_Define
