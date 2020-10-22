!------------------------------------------------------------------------------
!P+
! NAME:
!       Generate_CRTM_Stats
!
! PURPOSE:
!       Program to generate profile set statistics for the Community Radiative
!       Transfer Model (CRTM). 
!
! CATEGORY:
!       CRTM
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:             Module containing definitions for kinds
!                               of variable types.
!
!       File_Utility:           Module containing generic file utility routines
!
!       Message_Handler:        Module to define simple error codes and
!                               handle error conditions
!                               USEs: FILE_UTILITY module
!
!       CRTM_Atmosphere_Define: Module defining the AtmProfile data structure
!                               and containing routines to manipulate it.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!
!
!       SensorInfo_Define:      Module defining the SensorInfo data structure and
!                               containing routines to manipulate it.
!                               USEs: TYPE_KINDS module
!                                     FILE_UTILITY module
!                                     ERROR_HANDLER module
!
!       SensorInfo_LinkedList:  Module defining the SensorInfo Linked List
!                               data structure and containing routines to
!                               manipulate it.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!                                     SENSORINFO_DEFINE module
!
!       SensorInfo_IO:          Module continaing routines to read and write ASCII
!                               SensorInfo format files.
!                               USEs: TYPE_KINDS module
!                                     FILE_UTILITY module
!                                     ERROR_HANDLER module
!                                     SENSORINFO_DEFINE module
!
!       TauProfile_Define:      Module defining the TauProfile data structure
!                               and containing routines to manipulate it.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!
!       TauProfile_netCDF_IO:   Module containing routines to read and write
!                               TauProfile netCDF format files.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!                                     TAUPROFILE_DEFINE module
!                                     NETCDF module
!                                     NETCDF_UTILITY module
!
!       CRTMstats_Define:      Module defining the pCRTMstats data structure
!                               and containing routines to manipulate it.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!
!       CRTMstats_netCDF_IO:   Module containing routines to read and write
!                               pCRTMstats netCDF format files.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!                                     RTMSTATS_DEFINE module
!                                     NETCDF module
!                                     NETCDF_UTILITY module
!
!       Initialize:             Module for pCRTM initialisation.
!                               USEs: ERROR_HANDLER module
!                                     SPECTRAL_COEFFICIENTS module
!                                     TRANSMITTANCE_COEFFICIENTS module
!
!       Parameters:             Module to hold CRTM parameter constants.
!                               USEs: TYPE_KINDS module
!
!       Forward_Model:          Module containing the CRTM forward model
!                               function.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!                                     PARAMETERS module
!                                     SPECTRAL_COEFFICIENTS module
!                                     ABSORBER_PROFILE module
!                                     PREDICTORS module
!                                     TRANSMITTANCE module
!                                     RADIANCE module
!
!       Spectral_Coefficients:  Module containing the pCRTM spectral
!                               coefficients.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!                                     PARAMETERS module
!                                     SPCCOEFF_DEFINE module
!                                     SPCCOEFF_BINARY_IO module
!
!       Radiance:              Module containing the pCRTM radiative
!                              transfer routines.
!                              USEs: TYPE_KINDS module
!                                    PARAMETERS module
!                                    SPECTRAL_COEFFICIENTS module
!                                    SENSOR_PLANCK_ROUTINES module
!
! CONTAINS:
!       None.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       Input: - ASCII format SensorInfo file
!              - netCDF format AtmProfile file
!              - ASCII format list file of profiles to process
!              - Binary format SpcCoeff file } As part of
!              - Binary format TauCoeff file } the pCRTM
!
!       Output: - netCDF format pCRTMstats file.
!
! SIDE EFFECTS:
!       If the output file already exists, it is overwritten.
!
! RESTRICTIONS:
!       Processing ois performed only if all the mandatory input
!       files are present.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 20-Jun-2003
!                       paul.vandelst@ssec.wisc.edu
!       Modified by:    David Groff, SAIC 03-May-2007
!
!  Copyright (C) 2003, 2004 Paul van Delst
!
!P-
!------------------------------------------------------------------------------

PROGRAM Generate_CRTM_Stats


  ! ------------
  ! Module usage
  ! ------------

 
  USE File_Utility
!  USE List_File_Utility
  
  USE Type_Kinds                
  USE Message_Handler,           ONLY: SUCCESS, WARNING, Display_Message
  USE CRTM_Parameters,           ONLY: SET, NOT_SET, ONE,    &
                                       MAX_N_PHASE_ELEMENTS, &
                                       MAX_N_LEGENDRE_TERMS, &
                                       MAX_N_STOKES,         &
                                       MAX_N_ANGLES,         &
                                       ZERO,                 &
                                       LIMIT_EXP
  USE CRTM_Atmosphere_Define
  USE CRTM_Atmosphere_Binary_IO
  USE CRTM_Surface_Define
  USE CRTM_SensorData_Define
  USE CRTM_GeometryInfo_Define
  USE CRTM_ChannelInfo_Define
  USE CRTM_Options_Define
  USE CRTM_Parameters
  USE CRTM_SfcOptics
  USE CRTM_SfcOptics_Define
  USE CRTM_AtmOptics,            ONLY: CRTM_Combine_AtmOptics,  &
                                       CRTM_AOVariables_Type
  USE CRTM_RTSolution
  USE CRTM_RTSolution_Define
  USE CRTM_Forward_Module
  USE CRTM_LifeCycle
  USE CRTMstats_Define
  USE CRTMstats_netCDF_IO
  USE SensorInfo_Define
  USE CRTM_TauCoeff
  USE CRTM_SpcCoeff
  USE CRTM_AtmScatter_Define
  USE TauProfile_Define
  USE TauProfile_netCDF_IO
  USE CRTM_CloudScatter
  USE CRTM_AerosolScatter
  USE CRTM_AtmAbsorption_Define
  !USE CRTM_AtmOptics

  

  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ),  PARAMETER :: PROGRAM_NAME   = 'Generate_CRTM_Stats'
  CHARACTER( * ),  PARAMETER :: PROGRAM_RCS_ID = &
  CHARACTER( * ),  PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'
  
  ! -- Variable used to represent the name of the atmosphere file
  CHARACTER( * ), PARAMETER :: ATMOSPHERE_FILENAME = 'CloudSat_GDAS.Atmosphere.Cloud.bin.A3'

  ! -- Number of Stoke and Angle arguments
  INTEGER, PARAMETER :: SFCOPTICS_N_STOKES = 1
  INTEGER, PARAMETER :: SFCOPTICS_N_ANGLES = 1
  INTEGER, PARAMETER :: SURFACE_CHANNELS = 0

  ! -- Number of Profiles in data --
  INTEGER, PARAMETER :: N_PROFILES = 32
  
  ! -- Regression profile tag
  CHARACTER( * ), PARAMETER :: REG_PROFILE_ID_TAG = 'CloudSat'
  
  ! layer depths
  REAL(fp), PARAMETER :: DELTA_Z = 0.240_fp
    
  ! -- Angle threshold --
  REAL ( fp ), PARAMETER :: ANGLETH = 2.95
  
  !******************TEMPORARY**********!
  INTEGER, PARAMETER :: N_CRTM_CHANNELS = 19
  
  ! -- relevant molecule sets
  INTEGER, PARAMETER :: TAU_ALL_INDEX = 10
  INTEGER, PARAMETER :: TAU_WET_INDEX = 12
  INTEGER, PARAMETER :: TAU_OZO_INDEX = 114
  INTEGER, PARAMETER :: TAU_DRY_INDEX = 113
  ! -- Number of molecule sets to test
  INTEGER,  PARAMETER :: N_MOLECULE_SETS = 1
 
  ! -- Default emissivities and IR reflectivity "type"
  ! -- These values are used for land parameterization
  ! -- The surface reflectivity is declared to be 1-(surface emissivity)
  REAL( fp ), PARAMETER :: MICROWAVE_EMISSIVITY = 0.6_fp
  REAL( fp ), PARAMETER :: INFRARED_EMISSIVITY  = 0.96_fp
  
  REAL( fp ), PARAMETER :: ANGLE_CALCULATION = 1.0_fp
  ! ---------
  ! Variables
  ! ---------
  
  ! ---------------------
  !   -Declare RTV-
  ! ---------------------
  TYPE( CRTM_RTVariables_type ) :: RTV 
  
  ! These variables will be used in the initialization of the CRTM. They are used to reference the coeff-file names
  CHARACTER( 256 ) :: TauProfile_File
  CHARACTER( 256 ) :: Sensor_Id
  CHARACTER( 256 ) :: Satellite_Name
  CHARACTER( 256 ) :: Instrument_Name
  CHARACTER( 256 ) :: Output_Filename
  CHARACTER( 2 ) :: m_format
  
  ! Declare the variables that will hold the channel and sensor indices
  INTEGER :: SensorIndex
  INTEGER :: ChannelIndex
  
  ! Declare the variable to hold the number of streams
  INTEGER :: n_Full_Streams
    
  ! Declare the structure that will hold the Atmosphere data
  TYPE( CRTM_Atmosphere_type ), DIMENSION( N_PROFILES ) :: Atmosphere
  
  ! Declare the structure that will hold the SfcOptics data
  TYPE( CRTM_SfcOptics_type ), DIMENSION( N_PROFILES ) :: SfcOptics
  
  ! Declare the structure that will hold the Surface data
  TYPE( CRTM_Surface_type ), DIMENSION( N_PROFILES ) :: Surface
  
  ! Declare the structure that will hold the GeometryInfo data
  TYPE( CRTM_GeometryInfo_type ), DIMENSION( N_PROFILES ) :: GeometryInfo 
  
  ! Declare the structure that will hold the Options data
  TYPE( CRTM_Options_type ), DIMENSION( N_PROFILES ) :: Options
  
  ! Declare internal variables
  TYPE(CRTM_CSVariables_type) :: CSV  ! CloudScatter
  TYPE(CRTM_ASVariables_type) :: ASV  ! AerosolScatter
  TYPE(CRTM_AOVariables_type) :: AOV  ! AtmOptics
  
  ! Declare the CRTMstats structure
  TYPE( CRTMstats_type ) :: CRTMstats
  
  ! Declare array that will hold the transmittance data
  REAL( fp ), DIMENSION(:), ALLOCATABLE :: Tau
  
  ! Declare arrays that will hold the transmittances for
  ! the three gas absorption components considered
  REAL( fp ), DIMENSION(:), ALLOCATABLE :: Tau_DRY
  REAL( fp ), DIMENSION(:), ALLOCATABLE :: Tau_WET
  REAL( fp ), DIMENSION(:), ALLOCATABLE :: Tau_OZO
  REAL( fp ), DIMENSION(:), ALLOCATABLE :: Tau_ALL
  
  ! Declare Diff_BTemp array
  REAL(fp), DIMENSION(:), ALLOCATABLE :: Diff_BTemp 
    
  ! Declare array that will hold total integrated water vapor
  REAL( fp ), DIMENSION( N_PROFILES ) :: Int_Water_Vapor
  
  ! Declare array that will hold CRTM transmittances
  REAL( fp ), DIMENSION(:), ALLOCATABLE :: CRTM_Tau
  
  ! Declare variable to hold (dP/Gravity)
  REAL( fp ) :: dPonG
  
  ! Declare array that will hold TOA CRTM transmittances  
  REAL( fp ) :: CRTM_Transmittance
  
  ! Declare array that will hold TOA LBL transmittances
  REAL( fp ) :: LBL_Transmittance
  
  ! Declare array that will hold TOA CRTM Optical Depths
  REAL( fp ) :: CRTM_Optical_Depth
  
  ! Declare array that will hold TOA LBL Optical Depths
  REAL( fp ) :: LBL_Optical_Depth
  
  ! Declare variables holding TauProfile_File information
  REAL( fp ),      DIMENSION(:), ALLOCATABLE :: Angle_List
  INTEGER( Long ), DIMENSION(:), ALLOCATABLE :: Profile_List
  INTEGER( Long ), DIMENSION(:), ALLOCATABLE :: Channel_List
  
  TYPE( CRTM_ChannelInfo_type ), DIMENSION(1) :: ChannelInfo
  
  ! Declare the AtmOptics structure to hold cloud
  ! OD results
  TYPE( CRTM_AtmScatter_type ) :: CloudScatter
  
  ! Declare the AtmOptics structure to hold aerosol
  ! OD results
  TYPE( CRTM_AtmScatter_type ) :: AerosolScatter
  
  ! Declare the AtmOptics structure to hold combined
  ! OD results
  TYPE( CRTM_AtmScatter_type ) :: AtmOptics
  
  ! Declare structure to hold AtmAbsorption
  TYPE( CRTM_AtmAbsorption_type ) :: AtmAbsorption
    
  ! This variable represents the name of the CRTMStats netcdf file
  CHARACTER( 256 ) :: REGstats_Filename
  
  ! Declare the array that will hold CRTMSolution data for all angles
  TYPE( CRTM_RTSolution_type ), DIMENSION(:,:,:), ALLOCATABLE :: CRTMSolution 
  
  ! Declare the structure that will hold the LBLSolution
  TYPE( CRTM_RTSolution_type ), DIMENSION(:,:), ALLOCATABLE :: LBLSolution
  
  ! Declare SensorInfo variable
  TYPE( SensorInfo_type ) :: SensorInfo
  
  ! This variable is used to read in the netcdf inquire module. Which provides information 
  ! about the TauProfile netcdf data array sizes. 
  CHARACTER( 16 )   :: LBL_Profile_ID_Tag
  CHARACTER( 5000 ) :: History
  CHARACTER( 5000 ) :: Comment
  
  ! Used to determine/find allocation of space or IO problems 
  INTEGER :: Error_Status
  INTEGER :: IO_Status
  INTEGER :: Allocate_Status
  
  ! Information to describe the dimensions of the TauProfile data 
  INTEGER :: n_Layers
  INTEGER :: n_Channels
  INTEGER :: n_Angles
  
  ! -- Loop counters
  INTEGER :: i, j, k, l, m, n
  
  !#----------------------------------------------------------------------------#
  !#                       -- Get the coefficient filenames --                  #
  !#----------------------------------------------------------------------------#
  
  ! Enter the instrument file prefix
  WRITE( *, FMT    = '( /5x, "Enter the Sensor ID: ")', &
       ADVANCE = 'NO' )
  READ( *, '(a)' ) Sensor_Id
  Sensor_Id = ADJUSTL( Sensor_ID )
  Instrument_Name = Sensor_Id
  Satellite_Name = Sensor_Id
  
  ! Declare the Sensor Index (SensorIndex should be 1)
  SensorIndex = 1 
  
  !#-------------------------------------------------------------------------
  !#                        -- INITIALIZE THE CRTM --
  !#-------------------------------------------------------------------------
  !# The ChannelInfo is populated during the initialization
  WRITE( *, '( /5x, "Initializing the CRTM..." )' )
  Error_Status = CRTM_Init( ChannelInfo,          &
       SensorId=(/Sensor_Id/) )
  
  
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
          'Error initializing CRTM', & 
          Error_Status)  
     STOP
  END IF
  
  
  
  ! Read Atmosphere binary information and fill Atmosphere data structure array
  WRITE( *, '( /5x, "Reading the Atmosphere structure file..." )' )
  Error_Status = CRTM_Read_Atmosphere_Binary( ATMOSPHERE_FILENAME, &
       Atmosphere           )
  
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
          'Error reading Atmosphere structure file '//&
          ATMOSPHERE_FILENAME, & 
          Error_Status )
     STOP
  END IF
  
  ! Turn clouds on or off
  !Atmosphere(:)%n_Clouds = 0
  
  !  Allocate for CRTMSolution
  ALLOCATE( CRTMSolution( N_CRTM_CHANNELS, N_PROFILES, 1 ), &
       STAT = Error_Status         )
  
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
          'Error Allocating CRTMSolution Structures.', & 
          Error_Status )
     STOP
  END IF
  
  ! ------------------------------------------------------------
  ! Allocate the Options structure. This will
  ! be used in the forward call and will be used to declare
  ! to declare the surface emissivity in the SfcOptics structure
  ! ------------------------------------------------------------
  Error_Status = CRTM_Allocate_Options( N_CRTM_CHANNELS,                 &  !  Input
       Options                          )  !  Output
  
  IF ( Error_Status /= SUCCESS ) THEN
     CALL Display_Message( PROGRAM_NAME, &
          'Error Allocating the Options structure.', &
          Error_Status )
     STOP
  END IF
  
  ! Fill arguments used in CRTM_Forward call and calculate Int_Wat_Vapor       
  DO m = 1, N_PROFILES
     
     
     !PRINT *, Atmosphere(m)%n_Layers
     ! Allocate the RTSolution structures
     Error_Status = CRTM_Allocate_RTSolution(  Atmosphere(m)%n_Layers,           &  ! Input
          CRTMSolution(:,m,1)               )  ! Output
     
     IF ( Error_Status /= SUCCESS ) THEN 
        CALL Display_Message( PROGRAM_NAME, &
             'Error Allocating CRTMSolution Structure.', & 
             Error_Status )
        STOP
     END IF
     ! Fill the Options Emissivity fields
     Options(m)%Emissivity_Switch = SET
     Options(m)%Emissivity = 0.99_fp
     ! Fill the surface argument for each profile
     Surface(m)%Land_Coverage = ONE
     Surface(m)%Land_Temperature = Atmosphere(m)%Temperature(Atmosphere(m)%n_Layers) - &
          (((Atmosphere(m)%Temperature(Atmosphere(m)%n_Layers - 1) - Atmosphere(m)%Temperature(Atmosphere(m)%n_Layers))/DELTA_Z) * &
          (DELTA_Z/TWO))
     
     ! Turn off ice clouds
     ! This is a specific and temporary test
     Ice_Cloud_Removal_Loop: DO i = 1, Atmosphere(m)%n_Clouds      
        IF(Atmosphere(m)%Cloud(i)%TYPE == ICE_CLOUD) THEN
           Atmosphere(m)%Cloud(i)%Water_Content = ZERO
        END IF
     END DO Ice_Cloud_Removal_Loop
     
     ! Calculate the total integrated water vapor
     DO k = 1, Atmosphere(m)%n_Layers
        ! Calculate dP/g for current layer
        dPonG = RECIPROCAL_GRAVITY * (Atmosphere(m)%Level_Pressure(k) - Atmosphere(m)%Level_Pressure(k-1))
        ! Calculate water vapor amount and accumulate 
        Int_Water_Vapor(m) = Int_Water_Vapor(m) + (dPonG * Atmosphere(m)%Absorber(k,H2O_ID))
     END DO
  END DO
  
  DO i = 1, 1
     
     WRITE(*,'(5x,"Computing CRTM stats for angle ",i0)') i
     !PRINT *, Atmosphere(1)%n_Clouds
     
     !PRINT *, SIZE(CRTMSolution,DIM=2)
     ! Fill the GeometryInfo argument to the CRTM_Forward module
     GeometryInfo(:)%Sensor_Zenith_Angle = (1/DEGREES_TO_RADIANS) * ACOS(ONE/ANGLE_CALCULATION)
     !PRINT *, GeometryInfo(:)%Sensor_Zenith_Angle
     GeometryInfo(:)%Source_Zenith_Angle = 180.0_fp
     !PRINT *, GeometryInfo(:)%Sensor_Zenith_Angle
     ! Set GeometryInfo(n_Profiles) array         
     !  Calculate CRTM brigtness temperatures and TOA radiances
     Error_Status = CRTM_Forward( Atmosphere,                       & ! Input
          Surface,                          & ! Input
          GeometryInfo,                     & ! Input
          ChannelInfo,                      & ! Input
          CRTMSolution(:,:,i),              & ! Output
          Options = Options                 ) ! Optional input 
     
  END DO
  
  
  ! Assign the output filename
  ! REGstats_Filename = TRIM( Sensor_Id )//'.REGstats.nc'
  
  ! ----------------------------------------------------------------
  ! Fill some of the CRTMstats fields (for plotting purposes mainly)
  ! ----------------------------------------------------------------
  
  ! Begin the profile loop  
  Profile_Loop: DO m=1, N_PROFILES
     
     
     
     IF ( m>16 ) CYCLE Profile_Loop
     
     ! Allocate difference in BTemp array
     ALLOCATE( Diff_BTemp(ChannelInfo(1)%n_Channels),     &
          CRTM_Tau(Atmosphere(m)%n_Layers),          &
          STAT = Error_Status                        )
     IF ( Error_Status /= SUCCESS ) THEN 
        CALL Display_Message( PROGRAM_NAME, &
             'Error Allocating BTemp info.', & 
             Error_Status )
        STOP
     END IF
     
     
     
     ! write m to character m_format
     ! for filenaming purposes
     WRITE(m_format,'(I2.2)') m
     m_format=ADJUSTL(m_format)
     
     REGstats_Filename = TRIM( Sensor_Id )// '_P' // TRIM(m_format) // '_CS' // '.REGstats.nc'
     
     ! Declare TauProfile_File name
     TauProfile_File = 'upwelling.'//TRIM( Sensor_Id )//'.P'//TRIM(m_format)//'.TauProfile.nc'
     
     !PRINT *, TauProfile_File
     
     ! Get TauProfile array dimensions
     Error_Status = Inquire_TauProfile_netCDF( TRIM( TauProfile_File ),                   &
          n_Layers     = n_Layers,             &
          n_Channels   = n_Channels,           &
          n_Angles     = n_Angles              )
     !ID_Tag  = LBL_Profile_ID_Tag,        &
     !                                                    History = History,                   &
     !                                                    Comment = Comment                    )
     IF ( Error_Status /= SUCCESS ) THEN 
        CALL Display_Message( PROGRAM_NAME, &
             'Error Inquiring TauProfile structure dimensions.', & 
             Error_Status )
        STOP
     END IF
     
     !#------------------------------------------------------------------------#
     !#              -- ALLOCATE THE OUTPUT CRTMstats STRUCTURE --             #
     !#------------------------------------------------------------------------#
     
     Error_Status = Allocate_CRTMstats(  n_Layers,        &
          n_Channels,      &
          1,               &   ! Do not include the maximum angle
          1,               &
          1,               &
          CRTMstats        )
     
     
     
     IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
             'Error allocating CRTMstats structure.', &
             Error_Status )
        STOP
     ENDIF
     
     ! ----------------------------------------------------------------
     ! Fill some of the CRTMstats fields (for plotting purposes mainly)
     ! ----------------------------------------------------------------
     
     CRTMstats%LBL_Profile_ID_Tag = TRIM( LBL_Profile_ID_Tag )
     CRTMstats%REG_Profile_ID_Tag = REG_PROFILE_ID_TAG
     
     !CRTMstats%NCEP_Sensor_ID = ChannelInfo(SensorIndex)%NCEP_Sensor_ID
     CRTMstats%WMO_Satellite_ID = ChannelInfo(SensorIndex)%WMO_Satellite_ID
     CRTMstats%WMO_Sensor_ID = ChannelInfo(SensorIndex)%WMO_Sensor_ID
     SensorInfo%Sensor_Name = Instrument_Name
     SensorInfo%Satellite_Name = Satellite_Name
     CRTMstats%Sensor_Channel = ChannelInfo(SensorIndex)%Sensor_Channel
     CRTMstats%Angle = Angle_List(1:n_Angles - 1)
     CRTMstats%Profile = Profile_List(1:N_PROFILES)
     CRTMstats%Molecule_Set = (/ TAU_ALL_INDEX /)
     CRTMstats%Frequency = SC(SensorIndex)%Wavenumber
     
     
     !PRINT *, n_Angles, n_Channels
     ! Allocate for TauProfile metadata
     ALLOCATE( Angle_List( n_Angles ),              & 
          Channel_List( n_Channels ),          & 
          Profile_List( 1 ),          & 
          STAT = Error_Status         )
     
     IF ( Error_Status /= SUCCESS ) THEN 
        CALL Display_Message( PROGRAM_NAME, &
             'Error Allocating for TauProfile metadata.', & 
             Error_Status )
        STOP
     END IF
     
     
     ! Get TauProfile metadata
     Error_Status = Inquire_TauProfile_netCDF( TRIM( TauProfile_File ),                    &
          Channel_List = Channel_List,         &  
          Angle_List   = Angle_List  ,         &  
          Profile_List = Profile_List)!,         &  
     !ID_Tag  = LBL_Profile_ID_Tag,        &
     !                                                     History = History,                   &
     !                                                     Comment = Comment                    )
     IF ( Error_Status /= SUCCESS ) THEN 
        CALL Display_Message( PROGRAM_NAME, &
             'Error Inquiring TauProfile metadata.', & 
             Error_Status )
        STOP
     END IF
     
     WRITE(*,'(5x,"Computing LBL stats for profile ",i0)') m
     
     ! Check for clouds and aerosols 
     !IF ( (Atmosphere(m)%n_Clouds > 0) .OR. (Atmosphere(m)%n_Aerosols > 0) ) &
     !    CYCLE Profile_Loop
     
     ! ------------------------------------
     ! Allocate the AtmAbsorption structure
     ! ------------------------------------
     Error_Status = CRTM_Allocate_AtmAbsorption( Atmosphere(m)%n_Layers, &  ! Input
          AtmAbsorption           )  ! Output
     
     IF ( Error_Status /= SUCCESS ) THEN 
        CALL Display_Message( PROGRAM_NAME, &
             'Error computing AtmAbsorption values.', & 
             Error_Status )
        STOP
     END IF
     
     ! -------------------------
     ! Allocate for CloudScatter
     ! -------------------------
     Error_Status = CRTM_Allocate_AtmScatter(   Atmosphere(m)%n_Layers , &  ! Input
          MAX_N_LEGENDRE_TERMS   , &  ! Input
          MAX_N_PHASE_ELEMENTS   , &  ! Input
          CloudScatter             )  ! Output
     
     IF ( Error_Status /= SUCCESS ) THEN 
        CALL Display_Message( PROGRAM_NAME, &
             'Error computing AtmOptics values.', & 
             Error_Status )
        STOP
     END IF
     
     ! ---------------------------
     ! Allocate for AerosolScatter
     ! ---------------------------
     Error_Status = CRTM_Allocate_AtmScatter(   Atmosphere(m)%n_Layers , &  ! Input
          MAX_N_LEGENDRE_TERMS   , &  ! Input
          MAX_N_PHASE_ELEMENTS   , &  ! Input
          AerosolScatter           )  ! Output
     
     IF ( Error_Status /= SUCCESS ) THEN 
        CALL Display_Message( PROGRAM_NAME, &
             'Error computing AtmOptics values.', & 
             Error_Status )
        STOP
     END IF
     
     
     ! --------------------------------
     ! Allocate the AtmOptics structure     
     ! --------------------------------
     Error_Status = CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers, &  ! Input
          MAX_N_LEGENDRE_TERMS,   &  ! Input
          MAX_N_PHASE_ELEMENTS,   &  ! Input
          AtmOptics               )  ! Output
     
     IF ( Error_Status /= SUCCESS ) THEN 
        CALL Display_Message( PROGRAM_NAME, &
             'Error computing AtmOptics values.', & 
             Error_Status )
        STOP
     END IF
     
     ! Begin the angle loop
     Angle_Loop: DO i=1, n_Angles 
        
        ! Calculate sensor zenith angle
        GeometryInfo(m)%Sensor_Zenith_Angle = ACOS(ONE/Angle_List(i)) / DEGREES_TO_RADIANS
        ! Fill GeometryInfo structure
        GeometryInfo(m)%Source_Zenith_Angle = 180.0_fp
        Error_Status = CRTM_Compute_GeometryInfo( GeometryInfo(m) )
        
        IF ( Error_Status /= SUCCESS ) THEN 
           CALL Display_Message( PROGRAM_NAME, &
                'Error computing GeometryInfo values.', & 
                Error_Status                           )
           STOP
        END IF
        
        !  Allocate for LBLSolution
        ALLOCATE( LBLSolution( n_Channels, N_PROFILES ),    &
             STAT = Error_Status           )
        
        IF ( Error_Status /= SUCCESS ) THEN 
           CALL Display_Message( PROGRAM_NAME, &
                'Error Allocating LBLSolution Structure.', & 
                Error_Status )
           STOP
        END IF
        
        ! Allocate the RTSolution structures
        Error_Status = CRTM_Allocate_RTSolution(  n_Layers,                         &  ! Input
             LBLSolution                       )  ! Output
        
        IF ( Error_Status /= SUCCESS ) THEN 
           CALL Display_Message( PROGRAM_NAME, &
                'Error Allocating LBLSolution Structure.', & 
                Error_Status )
           STOP
        END IF
        
        ! Cycle loop for sec(theta) > threshold
        IF ( Angle_List(i) > ANGLETH ) &
             CYCLE Angle_Loop 
        
        ! Allocate the SfcOptics structure
        Error_Status = CRTM_Allocate_SfcOptics( MAX_N_ANGLES,                     &  ! Input
             MAX_N_STOKES,                     &  ! Input
             SfcOptics(m)                      )  ! Output
        
        IF ( Error_Status /= SUCCESS ) THEN 
           CALL Display_Message( PROGRAM_NAME, &
                'Error Allocating SfcOptics Structure.', & 
                Error_Status )
           STOP
        END IF
        
        
        ! Determine the Surface temperature for the SfcOptics
        CALL CRTM_Compute_SurfaceT( Surface(m), SfcOptics(m) )
        
        IF ( Error_Status /= SUCCESS ) THEN
           CALL Display_Message( PROGRAM_NAME, &
                'Error calling CRTM_Forward.', &
                Error_Status )
           STOP
        END IF
        
        ! Begin the channel loop
        Channel_Loop: DO l=1, n_Channels
           CRTM_Optical_Depth=ZERO
           LBL_Optical_Depth=ZERO
           CRTM_Transmittance=ZERO
           LBL_Transmittance=ZERO
           
           ! Allocate for the Transmittance(Tau) arrays
           ALLOCATE( Tau( n_Layers ),     &
                Tau_ALL( n_Layers ), &
                STAT=Error_Status    )
           
           IF ( Error_Status /= SUCCESS ) THEN 
              CALL Display_Message( PROGRAM_NAME, &
                   'Error Allocating Tau.', & 
                   Error_Status )
              STOP
           END IF
           
           ! Declare the Surface Emissivity and Reflectivity for the profile/channel
           SfcOptics(m)%Emissivity(1,1) = Options(m)%Emissivity(l) 
           SfcOptics(m)%Reflectivity(1,1,1,1) = ONE - Options(m)%Emissivity(l)
           SfcOptics(m)%Direct_Reflectivity(1,1) = SfcOptics(m)%Reflectivity(1,1,1,1)
           SfcOptics(m)%Compute_Switch = NOT_SET
           
           ! Read TauProfile_File and assign data to rank-1 array holding Tau -> H20
           !Error_Status = Read_TauProfile_netCDF( TRIM(TauProfile_File),            &
           !                                                    Channel_List(l),             &
           !                                                    Angle_List(i),               &
           !                                                    Profile_List(m),             &    
           !                                                    TAU_WET_INDEX,               &
           !                                                    Tau_WET, Quiet=1             )
           !        IF ( Error_Status /= SUCCESS ) THEN 
           !           CALL Display_Message( PROGRAM_NAME, &
           !                                'Error reading TauProfile_File (H2O).', & 
           !                                 Error_Status )
           !          STOP
           !        END IF
           !        
           !        ! Filter unphysical transmittances
           !        Tau_WET = Filter_Transmittance(Tau_WET,n_Layers)
           !        
           !        ! Read TauProfile_File and assign data to rank-1 array holding Tau -> OZO
           !        Error_Status = Read_TauProfile_netCDF( TRIM(TauProfile_File),            &
           !                                                    Channel_List(l),             &
           !                                                    Angle_List(i),               &
           !                                                    Profile_List(m),             &    
           !                                                    TAU_OZO_INDEX,               &
           !                                                    Tau_OZO, Quiet=1             )
           !        IF ( Error_Status /= SUCCESS ) THEN 
           !           CALL Display_Message( PROGRAM_NAME, &
           !                                'Error reading TauProfile_File (OZO).', & 
           !                                 Error_Status )
           !          STOP
           !        END IF
           
           ! Filter unphysical transmittances
           !Tau_OZO = Filter_Transmittance(Tau_OZO,n_Layers)
           !PRINT *, Channel_List, l, Angle_List
           ! Read TauProfile_File and assign data to rank-1 array holding Tau -> DRY
           Error_Status = Read_TauProfile_netCDF( TRIM(TauProfile_File),            &
                Channel_List(l),             &
                Angle_List(i),               &
                Profile_List(1),             &    
                TAU_ALL_INDEX,               &
                Tau_ALL, Quiet=1             )
           IF ( Error_Status /= SUCCESS ) THEN 
              CALL Display_Message( PROGRAM_NAME, &
                   'Error reading TauProfile_File (DRY).', & 
                   Error_Status )
              STOP
           END IF
           !PRINT *, Channel_List
           ! Filter unphysical transmittances
           Tau_ALL = Filter_Transmittance(Tau_ALL,n_Layers)
           
           ! Calculate total transmittances from the
           ! individual transmittance components
           Tau = Tau_ALL
           !PRINT *, Tau
           ! Calculate Optical Depth for first layer
           IF (Tau(1)>ZERO) THEN
              AtmOptics%Optical_Depth(1) = -ONE * LOG(Tau(1)) / Angle_List(i)
              CRTM_Tau(1) = EXP((CRTMSolution(l,m,i)%Layer_Optical_Depth(1)*Angle_List(i))/(-ONE))
           ELSE
              !PRINT *, Tau(1)
              AtmOptics%Optical_Depth(1) = CRTMSolution(l,m,i)%Layer_Optical_Depth(1)
              CRTM_Tau(1) = EXP((CRTMSolution(l,m,i)%Layer_Optical_Depth(1)*Angle_List(i))/(-ONE))
           END IF
           
           ! Assign first layer Optical Depth to CRTMstats
           CRTMstats%LBL_OD(1,l,1,1,1) = Tau(1)
           CRTMstats%REG_OD(1,l,1,1,1) = CRTM_Tau(1)
           CRTMstats%dOD(1,l,1,1,1) = Tau(1) - CRTM_Tau(1)
           
           
           ! Convert Total transmittance to ODepth for all layers
           Layer_Loop: DO k=2, n_Layers
              ! Check to make sure transmittance is
              ! greater than zero
              IF (Tau(k)==ZERO) THEN
                 AtmAbsorption%Optical_Depth(k) = CRTMSolution(l,m,i)%Layer_Optical_Depth(k)
                 CRTM_Tau(k) = Tau(k-1)*EXP(CRTMSolution(l,m,i)%Layer_Optical_Depth(k)*Angle_List(i)/(-ONE))
                 CRTMstats%LBL_OD(k,l,1,1,1) = Tau(k)
                 CRTMstats%REG_OD(k,l,1,1,1) = CRTM_Tau(k)
                 CRTMstats%dOD(k,l,1,1,1) = Tau(k) - CRTM_Tau(k)
                 CYCLE Layer_Loop
              ENDIF
              !PRINT *, 'HERE', l, k     
              AtmAbsorption%Optical_Depth(k) = -ONE * LOG(Tau(k)/Tau(k-1))/Angle_List(i)
              CRTM_Tau(k) = Tau(k-1)*EXP(CRTMSolution(l,m,i)%Layer_Optical_Depth(k)*Angle_List(i)/(-ONE))
              CRTMstats%LBL_OD(k,l,1,1,1) = Tau(k)
              CRTMstats%REG_OD(k,l,1,1,1) = CRTM_Tau(k)
              CRTMstats%dOD(k,l,1,1,1) = Tau(k) - CRTM_Tau(k)
              ! PRINT *, CRTMstats%LBL_OD(k,l,i,m,1), CRTMstats%REG_OD(k,l,i,m,1)
           END DO Layer_Loop
           
           n_Full_Streams = CRTM_Compute_nStreams( Atmosphere(m),                              &  ! Input
                ChannelInfo(SensorIndex)%Sensor_Index,      &  ! Input
                ChannelInfo(SensorIndex)%Channel_Index(l),  &  ! Input
                LBLSolution(l,m)                            )  ! Output
           
           CloudScatter%n_Legendre_Terms = n_Full_Streams
           AtmOptics%n_Legendre_Terms = n_Full_Streams
           
           
           Error_Status = CRTM_Compute_CloudScatter( Atmosphere(m),                             &  ! Input
                ChannelInfo(SensorIndex)%Sensor_Index,     &  ! Input
                ChannelInfo(SensorIndex)%Channel_Index(l), &  ! Input
                CloudScatter,                              &  ! Output
                CSV                                        )  ! Internal Variable Output
           
           IF ( Error_Status /= SUCCESS ) THEN 
              CALL Display_Message( PROGRAM_NAME,                         &
                   'Error calculating cloud component.',  & 
                   Error_Status )
              STOP
           END IF
           PRINT *, Atmosphere(m)%n_Clouds, ChannelInfo(SensorIndex)%Sensor_Index, ChannelInfo(SensorIndex)%Channel_Index(l)
           !  PRINT *, ChannelInfo(SensorIndex)%Sensor_Index, ChannelInfo(SensorIndex)%Channel_Index(l)
           !  PRINT *, CloudScatter%Optical_Depth(85)
           CALL CRTM_Combine_AtmOptics( AtmAbsorption,      &    ! Input
                CloudScatter,       &    ! Input
                AerosolScatter,     &    ! Input
                AtmOptics,          &    ! Output
                AOV                 )    ! Internal variable output
           !PRINT *, AtmOptics%Optical_Depth(k)
           
           IF ( Error_Status /= SUCCESS ) THEN 
              CALL Display_Message( PROGRAM_NAME,                         &
                   'Error compining OD components.',      & 
                   Error_Status )
              STOP
           END IF
           
           !AtmOptics%Optical_Depth = AtmAbsorption%Optical_Depth
           ! Short(en) name for Channel Index
           ChannelIndex = ChannelInfo(SensorIndex)%Channel_Index(l)
           
           
           ! Calculate LBL Brightness Temperatures
           Error_Status = CRTM_Compute_RTSolution( Atmosphere(m),                            & ! Input
                Surface(m),                               & ! Input
                AtmOptics,                                & ! Input
                SfcOptics(m),                             & ! Input
                GeometryInfo(m),                          & ! Input
                SensorIndex,                              & ! Input
                ChannelIndex,                             & ! Input
                LBLSolution(l,m),                         & ! Output 
                RTV                                       ) ! Internal variable output
           
           ! Calculate Total Optical depths using the Layer_Optical_Depths
           TOA_Transmittance_Layer_Loop: DO k=1, n_layers 
              CRTM_Optical_Depth = CRTMSolution(l,m,i)%Layer_Optical_Depth(k) + CRTM_Optical_Depth
              LBL_Optical_Depth = LBLSolution(l,m)%Layer_Optical_Depth(k) + LBL_Optical_Depth
           END DO TOA_Transmittance_Layer_Loop
           
           ! Convert TOA Optical Depths to TOA Transmittances
           CRTM_Transmittance = EXP(-((CRTM_Optical_Depth)*(Angle_List(i))))
           LBL_Transmittance = EXP(-((LBL_Optical_Depth)*(Angle_List(i))))
           
           !PRINT *, (LBL_Transmittance - CRTM_Transmittance), k, l, m
           !CRTMstats%REG_BT(l, i, m, 1) = CRTMSolution(l,m,i)%Brightness_Temperature
           !        CRTMstats%LBL_BT(l, i, m, 1) =  LBLSolution(l,m)%Brightness_Temperature
           !        
           ! Deallocate Tau
           DEALLOCATE(    Tau,                   &
                Tau_ALL,               &
                STAT=Error_Status      )
           
           IF ( Error_Status /= SUCCESS ) THEN 
              CALL Display_Message( PROGRAM_NAME, &
                   'Error DEAllocating Tau array.', & 
                   Error_Status )
              STOP
           END IF
           !PRINT *, LBLSolution(l,m)%Brightness_Temperature, CRTMSolution(l,m,1)%Brightness_Temperature
        END DO Channel_Loop
        
        
        
        Output_Filename = 'BT_data_T1_WO_' // TRIM(m_format) // '.ascii'
        
        Diff_BTemp = LBLSolution(:,m)%Brightness_Temperature - CRTMSolution(:,m,i)%Brightness_Temperature
        
        ! Write BT information to file (LBLRTM(TauProfile),CRTM,LBLRTM-CRTM)
        Error_Status = Write_Brightness_Temperatures( ChannelInfo(1)%n_Channels,                    & ! Input
             CRTMSolution(:,m,i)%Brightness_Temperature,   & ! Input
             LBLSolution(:,m)%Brightness_Temperature,      & ! Input
             Diff_BTemp,                                   & ! Input
             Output_Filename                               ) ! Output
        IF ( Error_Status /= SUCCESS ) THEN
           CALL Display_Message( PROGRAM_NAME,    &
                'Error Writing BTemps to file', &
                Error_Status ) 
           STOP
        END IF
        
        
        ! -----------------------------------------------------------------
        !       --Deallocate Structures and arrays for current view angle--
        ! -----------------------------------------------------------------
        
        Error_Status = CRTM_Destroy_RTSolution( LBLSolution )
        
        IF ( Error_Status   /= SUCCESS ) THEN
           CALL Display_Message( PROGRAM_NAME, &
                'Error deallocating LBLSolution structures', &
                Error_Status )
           STOP
        END IF
        
        ! LBLSolution
        DEALLOCATE( LBLSolution,                     &
             STAT = Error_Status              )
        
        IF ( Error_Status /= SUCCESS ) THEN 
           CALL Display_Message( PROGRAM_NAME, &
                'Error DEAllocating LBLSolution Structure.', & 
                Error_Status )
           STOP
        END IF
        
        ! The SfcOptics Structures
        Error_Status = CRTM_Destroy_SfcOptics( SfcOptics(m) )
        
        IF ( Error_Status   /= SUCCESS ) THEN
           CALL Display_Message( PROGRAM_NAME, &
                'Error deallocating SfcOptics structures', &
                Error_Status )
           STOP
        END IF
        
     END DO Angle_Loop
     
     ! Deallocate structures for current profile
     ! The AtmOptics Structures
     Error_Status = CRTM_Destroy_AtmScatter( AtmOptics )
     
     IF ( Error_Status   /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
             'Error deallocating AtmOptics structures', &
             Error_Status )
        STOP
     END IF
     
     ! Deallocate diff_btemp array
     DEALLOCATE( Diff_BTemp,         &
          Profile_List,       &
          Angle_List,         &
          Channel_List,       &
          CRTM_Tau,           &
          STAT = Error_Status )                
     IF ( Error_Status /= SUCCESS ) THEN 
        CALL Display_Message( PROGRAM_NAME, &
             'Error Deallocating BTemp and channel radiance info.', & 
             Error_Status )
        STOP
     END IF
     
     ! Compute the statistics
     Error_Status = Compute_CRTMstats( CRTMstats )
     
     IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
             'Error computing CRTMstats.', &
             Error_Status )
        STOP
     END IF
     
     ! Output the structure      
     Error_Status = Write_CRTMstats_netCDF( TRIM( REGstats_Filename ), &
          CRTMstats, &
          Title = 'CRTM statistics for LBL and CRTM BT comparison.', &
          History = PROGRAM_RCS_ID//'; '//TRIM( History ), &
          Sensor_Name = TRIM( SensorInfo%Sensor_Name ), &
          Platform_Name = TRIM( SensorInfo%Satellite_Name), &
          Comment = 'Stats are for LBL - CRTM; '//TRIM( Comment ) )
     
     IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
             'Error writing CRTMstats.', &
             Error_Status )
        STOP
     END IF
     
     ! Deallocate the CRTM stats structure
     Error_Status = Destroy_CRTMstats( CRTMstats )
     
     IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
             'Error destroying CRTMstats structure.', &
             Error_Status )
        STOP
     END IF
     
     
  END DO Profile_Loop
  
  ! The Options structures
  Error_Status = CRTM_Destroy_Options( Options )
  
  IF ( Error_Status   /= SUCCESS ) THEN
     CALL Display_Message( PROGRAM_NAME, &
          'Error deallocating Options structures', &
          Error_Status )
     STOP
  END IF
  
  ! Destroy CRTMSolution
  DO i = 1, 1
     Error_Status = CRTM_Destroy_RTSolution( CRTMSolution(:,:,i) )
     IF ( Error_Status   /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
             'Error deallocating CRTMSolution structures', &
             Error_Status )
        STOP
     END IF
  END DO
  
  ! Deallocate CRTMSolution
  DEALLOCATE( CRTMSolution,               &
       STAT = Error_Status         )
  
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
          'Error DEAllocating CRTMSolution Structure.', & 
          Error_Status )
     STOP
  END IF
  
  ! The Surface structure
  Error_Status = CRTM_Destroy_Surface( Surface )
  
  IF ( Error_Status /= SUCCESS ) THEN
     CALL Display_Message( PROGRAM_NAME, &
          'Error deallocating Surface structure', &
          Error_Status                            )
     STOP
  END IF
  
  ! The Atmosphere structure  
  Error_Status = CRTM_Destroy_Atmosphere( Atmosphere )
  
  IF ( Error_Status   /= SUCCESS ) THEN
     CALL Display_Message( PROGRAM_NAME, &
          'Error deallocating Atmosphere structure', &
          Error_Status )
     STOP
  END IF
  
  ! Destroying the CRTM 
  Error_Status = CRTM_Destroy( ChannelInfo ) 
  
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
          'Error destroying CRTM', & 
          WARNING )
  END IF
  
  
CONTAINS
  
  ! Function to filter unphysical transmittances
  FUNCTION Filter_Transmittance(Tau,n_Layers) RESULT(Corrected_Tau)
    INTEGER,  INTENT(IN) :: n_Layers 
    REAL(fp), DIMENSION(n_Layers), INTENT(IN) :: Tau
    REAL(fp), DIMENSION(n_Layers) :: Corrected_Tau
    INTEGER :: k
    ! Check first layer for 
    ! unphysical transmittances
    IF(Tau(1)<ZERO) THEN
       Corrected_Tau(1)=ZERO
    ELSE IF(Tau(1)>ONE) THEN
       Corrected_Tau(1)=ONE
    ELSE
       Corrected_Tau(1)=Tau(1)
    END IF
    ! Check layers 2 to N_Layers
    ! for unphysical transmittances
    DO k = 2, n_Layers      
       IF(Tau(k)<ZERO) THEN
          Corrected_Tau(k)=ZERO
       ELSE IF(Tau(k)>Corrected_Tau(k-1)) THEN
          Corrected_Tau(k)=Corrected_Tau(k-1)
       ELSE 
          Corrected_Tau(k)=Tau(k)
       END IF
    END DO
  END FUNCTION Filter_Transmittance
  
  FUNCTION Total_Transmittance(Tau1,Tau2,Tau3,n_Layers) RESULT(Tau)
    REAL(fp), DIMENSION(n_Layers), INTENT(IN) :: Tau1, Tau2, Tau3
    INTEGER, INTENT(IN) :: n_Layers
    REAL(fp), DIMENSION(n_Layers) :: Tau
    INTEGER :: k
    ! Calculate total transmittances
    DO k = 1, n_Layers
       Tau(k)=Tau1(k)*Tau2(k)*Tau3(k)
    END DO
  END FUNCTION Total_Transmittance
  
  FUNCTION Write_Brightness_Temperatures(n_Channels,              &   ! number of Channels
       CRTM_BTemp,              &   ! CRTM BT
       LBLDIS_BTemp,            &   ! LBL BT
       Diff_BTemp,              &   ! BT difference
       Output_Filename        ) &   ! Output Filenames
       RESULT ( Error_Status )
    ! Arguments
    INTEGER, INTENT(IN) :: n_Channels
    REAL(fp), DIMENSION(n_Channels), INTENT(IN) :: CRTM_BTemp
    REAL(fp), DIMENSION(n_Channels), INTENT(IN) :: LBLDIS_BTemp
    REAL(fp), DIMENSION(n_Channels), INTENT(IN) :: Diff_BTemp
    CHARACTER(*), INTENT(OUT) :: Output_Filename
    
    ! Internal Arguments
    INTEGER :: file_id, Error_Status
    INTEGER :: l
    
    Error_Status=SUCCESS                                
    !PRINT *, CRTM_BTemp
    ! Write BT information                                                  
    file_id = Get_Lun()        
    OPEN( file_id, FILE   = Output_Filename,                                  &
         STATUS = 'REPLACE',                                        &
         ACCESS = 'SEQUENTIAL',                                     &
         FORM   = 'FORMATTED',                                      &
         ACTION = 'WRITE'                                           )
    Channel_Loop: DO l = 1, n_Channels
       WRITE(file_id, FMT = '(1X, 3F14.8)', ADVANCE = "YES") CRTM_BTemp(l), &
            LBLDIS_BTemp(l), &
            Diff_BTemp(l)
    END DO Channel_Loop
    CLOSE(file_id) 
    
  END FUNCTION Write_Brightness_Temperatures
  
END PROGRAM Generate_CRTM_Stats

!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2006/11/27 14:38:15 $
!
! $Revision: 1.5 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Generate_CRTM_Stats.f90,v $
! Revision 1.5  2006/11/27 14:38:15  dgroff
! The bottom three layers are now included in the total transmittance calculations.
!
! Revision 1.4  2006/11/21 23:41:09  dgroff
! The CRTM_Compute_Optical_Depth module no longer exist and needed to be
! removed from the list of modules being used in the program.
!
! Revision 1.3  2006/11/21 18:12:51  dgroff
! The surface structure has been filled to set the
! SfcOptics%Surface_Temperature equal to the
! Atmospheric temperature in the bottom layer.
!
! Revision 1.2  2006/10/27 18:12:15  dgroff
! The SfcOptics%Surface_Temperature has been set equal to the temperature of the lowest layer.
! Additions were also made in order to write TOA transmittance data to the CRTMstats netcdf files.
!
! Revision 2.4  2006/05/02 14:58:35  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 2.3  2005/02/07 17:40:35  paulv
! - Updated onscreen output from RTM to pCRTM.
!
! Revision 2.2  2005/01/07 18:43:40  paulv
! - Catagory change to pCRTM.
!
! Revision 2.1  2005/01/06 18:48:49  paulv
! - Modified filename to include "upwelling" prefix.
!
! Revision 2.0  2004/10/06 15:57:31  paulv
! - Removed RTMarg use. Replaced with individual allocated arrays for RTM inputs.
! - Now using new Fortran-95 utility modules.
!
! Revision 1.6  2004/03/29 18:05:17  paulv
! - Added default solar reflectivity.
! - Corrected bug in call to forward model due to solar transmittance argument.
!
! Revision 1.5  2004/02/13 17:15:21  paulv
! - Added RTMstats initialisation call(!)
! - Added TauProfile history and comment attributes to the output RTMstats file.
! - Corrected bugs in passing arguments to the Compute_Radiance subroutine.
! - No longer include the maximum angle in the computations.
!
! Revision 1.4  2004/02/12 17:28:35  paulv
! - Stats no longer include the largest angle in the calcs.
! - Corrected a bug in saving the total transmittance.
!
! Revision 1.3  2004/01/28 21:41:32  paulv
! - Using the RTMstats modules to collect and output the statistics.
!
! Revision 1.2  2003/12/05 22:38:02  paulv
! - Continuing updates. Not completed.
!
! Revision 1.1  2003/06/20 22:00:00  paulv
! Initial checkin. Incomplete.
!
! Revision 2.1  2003/05/16 18:46:57  paulv
! - New version.
!
!
!
