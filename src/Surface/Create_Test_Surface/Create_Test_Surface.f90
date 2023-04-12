!
! Create_Test_Surface
!
! Program to create a Surface datafile for CRTM testing
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 12-Nov-2008
!                       paul.vandelst@noaa.gov
!

PROGRAM Create_Test_Surface

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds            , ONLY: fp
  USE Message_Handler       , ONLY: SUCCESS, FAILURE, INFORMATION, &
                                    Display_Message, Program_Message
  USE CRTM_Parameters       , ONLY: ZERO
  USE CRTM_Surface_Define   , ONLY: CRTM_Surface_type, &
                                    BROADLEAF_FOREST, SCRUB, TILLED_SOIL, TUNDRA, &
                                    SEA_WATER, FRESH_WATER, &
                                    CRUST_SNOW, MEDIUM_SNOW, &
                                    FRESH_ICE
  USE CRTM_Surface_Binary_IO, ONLY: CRTM_Write_Surface_Binary
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*),  PARAMETER :: PROGRAM_NAME   = 'Create_Test_Surface'
  CHARACTER(*),  PARAMETER :: PROGRAM_RCS_ID = &
  ! Filenames
  CHARACTER(*), PARAMETER :: TEST_FILENAME = 'Test.Surface.bin'
  ! Structure dimensions
  INTEGER, PARAMETER :: N_PROFILES = 6


  ! ---------
  ! Variables
  ! ---------
  INTEGER :: Error_Status
  TYPE(CRTM_Surface_type) :: Surface(N_PROFILES)


  ! Output header
  ! -------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to create a Surface datafile for CRTM testing.', &
                        '$Revision$' )


  ! Profile 1: Tropical
  ! -------------------
  ! Land, water, and snow surface
  Surface(1)%Land_Coverage  = 0.50_fp
  Surface(1)%Water_Coverage = 0.25_fp
  Surface(1)%Snow_Coverage  = 0.25_fp
  Surface(1)%Ice_Coverage   = ZERO
  ! Surface type independent data
  Surface(1)%Wind_Speed     = 3.25_fp
  Surface(1)%Wind_Direction = ZERO
  ! Land surface type data
  Surface(1)%Land_Type             = BROADLEAF_FOREST
  Surface(1)%Land_Temperature      = 305.0_fp
  Surface(1)%Soil_Moisture_Content = 0.4_fp
  Surface(1)%Canopy_Water_Content  = 0.5_fp
  Surface(1)%Vegetation_Fraction   = 0.8_fp
  Surface(1)%Soil_Temperature      = 295.0_fp
  ! Water type data
  Surface(1)%Water_Type        = SEA_WATER
  Surface(1)%Water_Temperature = 285.0_fp
  Surface(1)%Salinity          = 35.0_fp
  ! Snow surface type data
  Surface(1)%Snow_Type        = CRUST_SNOW
  Surface(1)%Snow_Temperature = 270.0_fp
  Surface(1)%Snow_Depth       = 20.0_fp
  Surface(1)%Snow_Density     = 0.25_fp
  Surface(1)%Snow_Grain_Size  = 1.5e3_fp


  ! Profile 2: Mid-latitude summer
  ! ------------------------------
  ! Land, water, and snow surface
  Surface(2)%Land_Coverage  = 0.40_fp
  Surface(2)%Water_Coverage = 0.35_fp
  Surface(2)%Snow_Coverage  = 0.25_fp
  Surface(2)%Ice_Coverage   = ZERO
  ! Surface type independent data
  Surface(2)%Wind_Speed     = 8.0_fp
  Surface(2)%Wind_Direction = 270.0_fp
  ! Land surface type data
  Surface(2)%Land_Type             = SCRUB
  Surface(2)%Land_Temperature      = 305.0_fp
  Surface(2)%Soil_Moisture_Content = 0.2_fp
  Surface(2)%Canopy_Water_Content  = 0.4_fp
  Surface(2)%Vegetation_Fraction   = 0.8_fp
  Surface(2)%Soil_Temperature      = 295.0_fp
  ! Water type data
  Surface(2)%Water_Type        = SEA_WATER
  Surface(2)%Water_Temperature = 280.0_fp
  Surface(2)%Salinity          = 32.5_fp
  ! Snow surface type data
  Surface(2)%Snow_Type        = CRUST_SNOW
  Surface(2)%Snow_Temperature = 270.0_fp
  Surface(2)%Snow_Depth       = 20.0_fp
  Surface(2)%Snow_Density     = 0.25_fp
  Surface(2)%Snow_Grain_Size  = 1.5e3_fp


  ! Profile 3: Mid-latitude winter
  ! ------------------------------
  ! Land, water, snow, and ice surface
  Surface(3)%Land_Coverage  = 0.30_fp
  Surface(3)%Water_Coverage = 0.35_fp
  Surface(3)%Snow_Coverage  = 0.25_fp
  Surface(3)%Ice_Coverage   = 0.10_fp
  ! Surface type independent data
  Surface(3)%Wind_Speed     = 6.5_fp
  Surface(3)%Wind_Direction = 300.0_fp
  ! Land surface type data
  Surface(3)%Land_Type             = TILLED_SOIL
  Surface(3)%Land_Temperature      = 280.0_fp
  Surface(3)%Soil_Moisture_Content = 0.3_fp
  Surface(3)%Canopy_Water_Content  = 0.2_fp
  Surface(3)%Vegetation_Fraction   = 0.3_fp
  Surface(3)%Soil_Temperature      = 275.0_fp
  ! Water type data
  Surface(3)%Water_Type        = FRESH_WATER
  Surface(3)%Water_Temperature = 275.0_fp
  Surface(3)%Salinity          = ZERO
  ! Snow surface type data
  Surface(3)%Snow_Type        = MEDIUM_SNOW
  Surface(3)%Snow_Temperature = 265.0_fp
  Surface(3)%Snow_Depth       = 100.0_fp
  Surface(3)%Snow_Density     = 0.1_fp
  Surface(3)%Snow_Grain_Size  = 0.5e3_fp
  ! Ice surface type data
  Surface(3)%Ice_Type        = FRESH_ICE
  Surface(3)%Ice_Temperature = 265.0_fp
  Surface(3)%Ice_Thickness   = 500.0_fp
  Surface(3)%Ice_Density     = 0.83_fp
  Surface(3)%Ice_Roughness   = 0.5_fp


  ! Profile 4: Sub-arctic summer
  ! ----------------------------
  ! Land, water, snow, and ice surface
  Surface(4)%Land_Coverage  = 0.25_fp
  Surface(4)%Water_Coverage = 0.25_fp
  Surface(4)%Snow_Coverage  = 0.25_fp
  Surface(4)%Ice_Coverage   = 0.25_fp
  ! Surface type independent data
  Surface(4)%Wind_Speed     = 10.0_fp
  Surface(4)%Wind_Direction = 190.0_fp
  ! Land surface type data
  Surface(4)%Land_Type             = TUNDRA
  Surface(4)%Land_Temperature      = 295.0_fp
  Surface(4)%Soil_Moisture_Content = 0.2_fp
  Surface(4)%Canopy_Water_Content  = 0.3_fp
  Surface(4)%Vegetation_Fraction   = 0.4_fp
  Surface(4)%Soil_Temperature      = 290.0_fp
  ! Water type data
  Surface(4)%Water_Type        = SEA_WATER
  Surface(4)%Water_Temperature = 275.0_fp
  Surface(4)%Salinity          = 30.0_fp
  ! Snow surface type data
  Surface(4)%Snow_Type        = CRUST_SNOW
  Surface(4)%Snow_Temperature = 270.0_fp
  Surface(4)%Snow_Depth       = 200.0_fp
  Surface(4)%Snow_Density     = 0.18_fp
  Surface(4)%Snow_Grain_Size  = 1.0e3_fp
  ! Ice surface type data
  Surface(4)%Ice_Type        = FRESH_ICE
  Surface(4)%Ice_Temperature = 260.0_fp
  Surface(4)%Ice_Thickness   = 1000.0_fp
  Surface(4)%Ice_Density     = 0.8_fp
  Surface(4)%Ice_Roughness   = 0.5_fp


  ! Profile 5: Sub-arctic winter
  ! ----------------------------
  ! Land, water, snow, and ice surface
  Surface(5)%Land_Coverage  = 0.15_fp
  Surface(5)%Water_Coverage = 0.15_fp
  Surface(5)%Snow_Coverage  = 0.20_fp
  Surface(5)%Ice_Coverage   = 0.50_fp
  ! Surface type independent data
  Surface(5)%Wind_Speed     = 12.3_fp
  Surface(5)%Wind_Direction = 45.0_fp
  ! Land surface type data
  Surface(5)%Land_Type             = TUNDRA
  Surface(5)%Land_Temperature      = 273.0_fp
  Surface(5)%Soil_Moisture_Content = 0.3_fp
  Surface(5)%Canopy_Water_Content  = 0.1_fp
  Surface(5)%Vegetation_Fraction   = 0.05_fp
  Surface(5)%Soil_Temperature      = 260.0_fp
  ! Water type data
  Surface(5)%Water_Type        = SEA_WATER
  Surface(5)%Water_Temperature = 277.0_fp
  Surface(5)%Salinity          = 28.0_fp
  ! Snow surface type data
  Surface(5)%Snow_Type        = MEDIUM_SNOW
  Surface(5)%Snow_Temperature = 265.0_fp
  Surface(5)%Snow_Depth       = 400.0_fp
  Surface(5)%Snow_Density     = 0.1_fp
  Surface(5)%Snow_Grain_Size  = 0.5e3_fp
  ! Ice surface type data
  Surface(5)%Ice_Type        = FRESH_ICE
  Surface(5)%Ice_Temperature = 255.0_fp
  Surface(5)%Ice_Thickness   = 1500.0_fp
  Surface(5)%Ice_Density     = 0.77_fp
  Surface(5)%Ice_Roughness   = 0.5_fp


  ! Profile 6: U.S.Standard atmosphere
  ! ----------------------------------
  ! Land, water, snow, and ice surface
  Surface(6)%Land_Coverage  = 0.25_fp
  Surface(6)%Water_Coverage = 0.25_fp
  Surface(6)%Snow_Coverage  = 0.25_fp
  Surface(6)%Ice_Coverage   = 0.25_fp
  ! Surface type independent data
  Surface(6)%Wind_Speed     = 5.0_fp
  Surface(6)%Wind_Direction = 180.0_fp
  ! Land surface type data
  Surface(6)%Land_Type             = SCRUB
  Surface(6)%Land_Temperature      = 290.0_fp
  Surface(6)%Soil_Moisture_Content = 0.25_fp
  Surface(6)%Canopy_Water_Content  = 0.3_fp
  Surface(6)%Vegetation_Fraction   = 0.55_fp
  Surface(6)%Soil_Temperature      = 285.0_fp
  ! Water type data
  Surface(6)%Water_Type        = SEA_WATER
  Surface(6)%Water_Temperature = 285.0_fp
  Surface(6)%Salinity          = 31.0_fp
  ! Snow surface type data
  Surface(6)%Snow_Type        = MEDIUM_SNOW
  Surface(6)%Snow_Temperature = 268.0_fp
  Surface(6)%Snow_Depth       = 60.0_fp
  Surface(6)%Snow_Density     = 0.2_fp
  Surface(6)%Snow_Grain_Size  = 1.0e3_fp
  ! Ice surface type data
  Surface(6)%Ice_Type        = FRESH_ICE
  Surface(6)%Ice_Temperature = 265.0_fp
  Surface(6)%Ice_Thickness   = 500.0_fp
  Surface(6)%Ice_Density     = 0.83_fp
  Surface(6)%Ice_Roughness   = 0.5_fp


  ! Write the data to file
  ! ----------------------
  Error_Status = CRTM_Write_Surface_Binary( TEST_FILENAME, Surface )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME,'Error writing file.',FAILURE ); STOP
  END IF

END PROGRAM Create_Test_Surface
