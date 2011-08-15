!
! Create_NPOESS_EmisCoeff
!
! Program to create the NPOESS land emissivity/reflectance coefficient files
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst 15-Aug-2011
!                       paul.vandelst@noaa.gov
!

PROGRAM Create_NPOESS_EmisCoeff

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds               , ONLY: dp=>Double
  USE File_Utility             , ONLY: Get_Lun, File_Exists
  USE Message_Handler          , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                       Display_Message, Program_Message
  USE Spectral_Units_Conversion, ONLY: micron_to_inverse_cm
  USE IRLSE_NPOESS_Define      , ONLY: IRLSE_NPOESS_type      , &
                                       IRLSE_NPOESS_Associated, &
                                       IRLSE_NPOESS_Create    , &
                                       IRLSE_NPOESS_Destroy   
  USE IRLSE_NPOESS_IO          , ONLY: IRLSE_NPOESS_WriteFile
  ! Disable all implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Create_NPOESS_EmisCoeff'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &
  '$Id$'

  ! Dimensions
  ! ...The spectral dimension
  INTEGER, PARAMETER :: N_WAVELENGTHS = 74
  REAL(dp), PARAMETER :: WAVELENGTH(N_WAVELENGTHS) = &
    (/ 0.200_dp, 0.225_dp, 0.250_dp, 0.275_dp, 0.300_dp, 0.325_dp, 0.350_dp, 0.375_dp, &
       0.400_dp, 0.425_dp, 0.450_dp, 0.475_dp, 0.500_dp, 0.525_dp, 0.550_dp, 0.575_dp, &
       0.600_dp, 0.625_dp, 0.650_dp, 0.675_dp, 0.700_dp, 0.725_dp, 0.750_dp, 0.775_dp, &
       0.800_dp, 0.825_dp, 0.850_dp, 0.875_dp, 0.900_dp, 0.925_dp, 0.950_dp, 0.975_dp, &
       1.000_dp, 1.050_dp, 1.100_dp, 1.150_dp, 1.200_dp, 1.250_dp, 1.300_dp, 1.350_dp, &
       1.400_dp, 1.450_dp, 1.500_dp, 1.550_dp, 1.600_dp, 1.650_dp, 1.700_dp, 1.750_dp, &
       1.800_dp, 1.850_dp, 1.900_dp, 1.950_dp, 2.000_dp, 2.500_dp, 3.000_dp, 3.500_dp, &
       4.000_dp, 4.500_dp, 5.000_dp, 5.500_dp, 6.000_dp, 6.500_dp, 7.000_dp, 7.500_dp, &
       8.000_dp, 8.500_dp, 9.000_dp, 9.500_dp,10.000_dp,11.000_dp,12.000_dp,13.000_dp, &
      14.000_dp,15.000_dp /)
    
  ! ...The non-water/snow/ice NPOESS surface types
  INTEGER, PARAMETER :: N_SURFACE_TYPES = 20


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: msg
  INTEGER :: err_stat
  INTEGER :: n
  TYPE(IRLSE_NPOESS_type) :: npoess
  
  
  ! Program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to generate the NPOESS land '//&
                        'reflectance coefficient files.', &
                        '$Revision$' )


  ! Create structure
  CALL IRLSE_NPOESS_Create( &
         npoess, &
         n_Frequencies   = N_WAVELENGTHS, &
         n_Surface_Types = N_SURFACE_TYPES )
  IF ( .NOT. IRLSE_NPOESS_Associated(npoess) ) THEN
    msg = 'Error creating IRLSE_NPOESS structure'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF


  ! Compute frequency
  npoess%Frequency = micron_to_inverse_cm( WAVELENGTH )
  ! ...Now flip it to ascending order
  npoess%Frequency = npoess%Frequency(N_WAVELENGTHS:1:-1)


  ! Fill structure with reflectance data
  DO n = 1, N_SURFACE_TYPES
    CALL Load_NPOESS_Data(n)  
  END DO
  
  
  ! Write data to file
  err_stat = IRLSE_NPOESS_WriteFile( &
               'NPOESS.EmisCoeff.nc', &
               npoess, &
               netCDF = .TRUE., &
               Title = 'IR/VIS NPOESS Surface Reflectances', &
               History = PROGRAM_VERSION_ID, &
               Comment = 'Data extracted from CRTM_surface_ir_emissivity module.' )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error writing NPOESS data to file.'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF


  ! Clean up
  CALL IRLSE_NPOESS_Destroy( npoess )
  
CONTAINS

  SUBROUTINE Load_NPOESS_Data(Sfc_Idx)
    INTEGER, INTENT(IN) :: Sfc_Idx
    
    ! The surface type indices
    INTEGER, PARAMETER :: COMPACTED_SOIL           =  1
    INTEGER, PARAMETER :: TILLED_SOIL              =  2
    INTEGER, PARAMETER :: SAND                     =  3
    INTEGER, PARAMETER :: ROCK                     =  4
    INTEGER, PARAMETER :: IRRIGATED_LOW_VEGETATION =  5
    INTEGER, PARAMETER :: MEADOW_GRASS             =  6
    INTEGER, PARAMETER :: SCRUB                    =  7
    INTEGER, PARAMETER :: BROADLEAF_FOREST         =  8
    INTEGER, PARAMETER :: PINE_FOREST              =  9
    INTEGER, PARAMETER :: TUNDRA                   = 10
    INTEGER, PARAMETER :: GRASS_SOIL               = 11
    INTEGER, PARAMETER :: BROADLEAF_PINE_FOREST    = 12
    INTEGER, PARAMETER :: GRASS_SCRUB              = 13
    INTEGER, PARAMETER :: SOIL_GRASS_SCRUB         = 14
    INTEGER, PARAMETER :: URBAN_CONCRETE           = 15
    INTEGER, PARAMETER :: PINE_BRUSH               = 16
    INTEGER, PARAMETER :: BROADLEAF_BRUSH          = 17
    INTEGER, PARAMETER :: WET_SOIL                 = 18
    INTEGER, PARAMETER :: SCRUB_SOIL               = 19
    INTEGER, PARAMETER :: BROADLEAF70_PINE30       = 20

    ! The surface type names
    CHARACTER(*), PARAMETER :: SURFACE_TYPE(N_SURFACE_TYPES) = &
     (/ 'compacted_soil          ', &
        'tilled_soil             ', &
        'sand                    ', &
        'rock                    ', &
        'irrigated_low_vegetation', &
        'meadow_grass            ', &
        'scrub                   ', &
        'broadleaf_forest        ', &
        'pine_forest             ', &
        'tundra                  ', &
        'grass_soil              ', &
        'broadleaf_pine_forest   ', &
        'grass_scrub             ', &
        'soil_grass_scrub        ', &
        'urban_concrete          ', &
        'pine_brush              ', &
        'broadleaf_brush         ', &
        'wet_soil                ', &
        'scrub_soil              ', &
        'broadleaf70_pine30      ' /)    
    
    ! The reflectance data
    REAL(dp), SAVE :: reflectance(N_WAVELENGTHS, N_SURFACE_TYPES)
    INTEGER :: i
    ! ...compacted_soil               
    DATA (reflectance(i,COMPACTED_SOIL),i=1,N_WAVELENGTHS)/ &
    0.024_dp, 0.024_dp, 0.024_dp, 0.024_dp, 0.024_dp, 0.024_dp, 0.024_dp, 0.024_dp, &
    0.024_dp, 0.025_dp, 0.030_dp, 0.037_dp, 0.050_dp, 0.089_dp, 0.102_dp, 0.122_dp, &
    0.141_dp, 0.158_dp, 0.174_dp, 0.198_dp, 0.206_dp, 0.200_dp, 0.219_dp, 0.237_dp, &
    0.248_dp, 0.256_dp, 0.263_dp, 0.264_dp, 0.273_dp, 0.190_dp, 0.200_dp, 0.205_dp, &
    0.205_dp, 0.210_dp, 0.225_dp, 0.225_dp, 0.232_dp, 0.237_dp, 0.237_dp, 0.225_dp, &
    0.187_dp, 0.149_dp, 0.162_dp, 0.190_dp, 0.212_dp, 0.225_dp, 0.225_dp, 0.212_dp, &
    0.205_dp, 0.187_dp, 0.095_dp, 0.062_dp, 0.075_dp, 0.050_dp, 0.040_dp, 0.070_dp, &
    0.140_dp, 0.125_dp, 0.118_dp, 0.100_dp, 0.060_dp, 0.050_dp, 0.040_dp, 0.030_dp, &
    0.030_dp, 0.040_dp, 0.050_dp, 0.050_dp, 0.045_dp, 0.040_dp, 0.025_dp, 0.030_dp, &
    0.020_dp, 0.020_dp/
    ! ...tilled_soil
    DATA (reflectance(i,TILLED_SOIL),i=1,N_WAVELENGTHS)/ &
    0.010_dp, 0.010_dp, 0.010_dp, 0.010_dp, 0.010_dp, 0.010_dp, 0.010_dp, 0.010_dp,  &
    0.011_dp, 0.012_dp, 0.013_dp, 0.014_dp, 0.016_dp, 0.023_dp, 0.025_dp, 0.025_dp,  &
    0.028_dp, 0.031_dp, 0.034_dp, 0.035_dp, 0.035_dp, 0.035_dp, 0.034_dp, 0.034_dp,  &
    0.038_dp, 0.042_dp, 0.045_dp, 0.049_dp, 0.051_dp, 0.040_dp, 0.043_dp, 0.046_dp,  &
    0.049_dp, 0.051_dp, 0.054_dp, 0.057_dp, 0.065_dp, 0.070_dp, 0.076_dp, 0.076_dp,  &
    0.074_dp, 0.074_dp, 0.075_dp, 0.090_dp, 0.098_dp, 0.100_dp, 0.101_dp, 0.101_dp,  &
    0.100_dp, 0.100_dp, 0.050_dp, 0.037_dp, 0.040_dp, 0.035_dp, 0.028_dp, 0.049_dp,  &
    0.098_dp, 0.088_dp, 0.083_dp, 0.070_dp, 0.042_dp, 0.035_dp, 0.028_dp, 0.021_dp,  &
    0.021_dp, 0.028_dp, 0.035_dp, 0.035_dp, 0.031_dp, 0.028_dp, 0.018_dp, 0.030_dp,  &
    0.020_dp, 0.020_dp/
    ! ...sand
    DATA (reflectance(i,SAND),i=1,N_WAVELENGTHS)/ &
    0.200_dp, 0.210_dp, 0.220_dp, 0.230_dp, 0.250_dp, 0.260_dp, 0.270_dp, 0.287_dp,  &
    0.300_dp, 0.312_dp, 0.325_dp, 0.350_dp, 0.375_dp, 0.387_dp, 0.400_dp, 0.423_dp,  &
    0.450_dp, 0.455_dp, 0.460_dp, 0.470_dp, 0.475_dp, 0.483_dp, 0.487_dp, 0.495_dp,  &
    0.500_dp, 0.501_dp, 0.505_dp, 0.508_dp, 0.510_dp, 0.515_dp, 0.517_dp, 0.520_dp,  &
    0.525_dp, 0.535_dp, 0.545_dp, 0.555_dp, 0.565_dp, 0.575_dp, 0.585_dp, 0.595_dp,  &
    0.605_dp, 0.615_dp, 0.625_dp, 0.630_dp, 0.636_dp, 0.641_dp, 0.647_dp, 0.652_dp,  &
    0.658_dp, 0.663_dp, 0.669_dp, 0.674_dp, 0.680_dp, 0.325_dp, 0.315_dp, 0.500_dp,  &
    0.400_dp, 0.150_dp, 0.050_dp, 0.050_dp, 0.150_dp, 0.100_dp, 0.100_dp, 0.100_dp,  &
    0.100_dp, 0.100_dp, 0.100_dp, 0.090_dp, 0.080_dp, 0.020_dp, 0.020_dp, 0.020_dp,  &
    0.020_dp, 0.020_dp/
    ! ...rock
    DATA (reflectance(i,ROCK),i=1,N_WAVELENGTHS)/ &
    0.025_dp, 0.028_dp, 0.037_dp, 0.049_dp, 0.072_dp, 0.078_dp, 0.088_dp, 0.100_dp,  &
    0.118_dp, 0.132_dp, 0.151_dp, 0.151_dp, 0.150_dp, 0.150_dp, 0.152_dp, 0.170_dp,  &
    0.176_dp, 0.175_dp, 0.175_dp, 0.175_dp, 0.177_dp, 0.199_dp, 0.218_dp, 0.215_dp,  &
    0.208_dp, 0.202_dp, 0.210_dp, 0.212_dp, 0.213_dp, 0.217_dp, 0.225_dp, 0.234_dp,  &
    0.240_dp, 0.265_dp, 0.285_dp, 0.279_dp, 0.270_dp, 0.265_dp, 0.252_dp, 0.247_dp,  &
    0.228_dp, 0.227_dp, 0.227_dp, 0.227_dp, 0.226_dp, 0.225_dp, 0.225_dp, 0.225_dp,  &
    0.225_dp, 0.220_dp, 0.212_dp, 0.205_dp, 0.200_dp, 0.050_dp, 0.100_dp, 0.200_dp,  &
    0.120_dp, 0.180_dp, 0.070_dp, 0.050_dp, 0.070_dp, 0.080_dp, 0.050_dp, 0.040_dp,  &
    0.100_dp, 0.110_dp, 0.130_dp, 0.140_dp, 0.120_dp, 0.050_dp, 0.030_dp, 0.020_dp,  &
    0.020_dp, 0.020_dp/
    ! ...irrigated_low_vegetation
    DATA (reflectance(i,IRRIGATED_LOW_VEGETATION),i=1,N_WAVELENGTHS)/ &
    0.021_dp, 0.021_dp, 0.021_dp, 0.021_dp, 0.021_dp, 0.025_dp, 0.029_dp, 0.034_dp,  &
    0.038_dp, 0.040_dp, 0.046_dp, 0.050_dp, 0.048_dp, 0.045_dp, 0.046_dp, 0.048_dp,  &
    0.045_dp, 0.034_dp, 0.040_dp, 0.061_dp, 0.080_dp, 0.268_dp, 0.338_dp, 0.429_dp,  &
    0.478_dp, 0.510_dp, 0.511_dp, 0.577_dp, 0.598_dp, 0.654_dp, 0.659_dp, 0.662_dp,  &
    0.663_dp, 0.665_dp, 0.677_dp, 0.593_dp, 0.570_dp, 0.546_dp, 0.456_dp, 0.366_dp,  &
    0.275_dp, 0.185_dp, 0.100_dp, 0.138_dp, 0.180_dp, 0.223_dp, 0.266_dp, 0.308_dp,  &
    0.252_dp, 0.196_dp, 0.141_dp, 0.085_dp, 0.029_dp, 0.018_dp, 0.034_dp, 0.038_dp,  &
    0.043_dp, 0.039_dp, 0.034_dp, 0.032_dp, 0.027_dp, 0.034_dp, 0.036_dp, 0.036_dp,  &
    0.036_dp, 0.036_dp, 0.036_dp, 0.036_dp, 0.036_dp, 0.036_dp, 0.045_dp, 0.036_dp,  &
    0.018_dp, 0.018_dp/
    ! ...meadow_grass
    DATA (reflectance(i,MEADOW_GRASS),i=1,N_WAVELENGTHS)/ &
    0.025_dp, 0.025_dp, 0.025_dp, 0.025_dp, 0.025_dp, 0.025_dp, 0.025_dp, 0.030_dp,  &
    0.035_dp, 0.040_dp, 0.065_dp, 0.065_dp, 0.065_dp, 0.085_dp, 0.100_dp, 0.085_dp,  &
    0.075_dp, 0.070_dp, 0.065_dp, 0.140_dp, 0.225_dp, 0.325_dp, 0.500_dp, 0.600_dp,  &
    0.635_dp, 0.640_dp, 0.641_dp, 0.645_dp, 0.645_dp, 0.648_dp, 0.650_dp, 0.660_dp,  &
    0.675_dp, 0.670_dp, 0.665_dp, 0.660_dp, 0.655_dp, 0.650_dp, 0.546_dp, 0.442_dp,  &
    0.338_dp, 0.234_dp, 0.130_dp, 0.169_dp, 0.208_dp, 0.247_dp, 0.286_dp, 0.325_dp,  &
    0.272_dp, 0.219_dp, 0.166_dp, 0.113_dp, 0.060_dp, 0.150_dp, 0.050_dp, 0.125_dp,  &
    0.200_dp, 0.260_dp, 0.285_dp, 0.295_dp, 0.060_dp, 0.105_dp, 0.060_dp, 0.045_dp,  &
    0.050_dp, 0.060_dp, 0.068_dp, 0.075_dp, 0.100_dp, 0.165_dp, 0.150_dp, 0.125_dp,  &
    0.105_dp, 0.090_dp/
    ! ...scrub
    DATA (reflectance(i,SCRUB),i=1,N_WAVELENGTHS)/ &
    0.012_dp, 0.012_dp, 0.012_dp, 0.012_dp, 0.012_dp, 0.012_dp, 0.012_dp, 0.012_dp,  &
    0.015_dp, 0.025_dp, 0.045_dp, 0.050_dp, 0.067_dp, 0.040_dp, 0.054_dp, 0.052_dp,  &
    0.043_dp, 0.030_dp, 0.031_dp, 0.033_dp, 0.035_dp, 0.058_dp, 0.073_dp, 0.114_dp,  &
    0.166_dp, 0.180_dp, 0.190_dp, 0.199_dp, 0.210_dp, 0.355_dp, 0.350_dp, 0.348_dp,  &
    0.343_dp, 0.334_dp, 0.326_dp, 0.317_dp, 0.308_dp, 0.300_dp, 0.269_dp, 0.238_dp,  &
    0.207_dp, 0.176_dp, 0.145_dp, 0.163_dp, 0.181_dp, 0.200_dp, 0.180_dp, 0.160_dp,  &
    0.147_dp, 0.135_dp, 0.123_dp, 0.110_dp, 0.098_dp, 0.042_dp, 0.050_dp, 0.065_dp,  &
    0.100_dp, 0.150_dp, 0.130_dp, 0.120_dp, 0.030_dp, 0.040_dp, 0.060_dp, 0.055_dp,  &
    0.050_dp, 0.040_dp, 0.030_dp, 0.035_dp, 0.040_dp, 0.050_dp, 0.060_dp, 0.050_dp,  &
    0.040_dp, 0.070_dp/
    ! ...broadleaf_forest
    DATA (reflectance(i,BROADLEAF_FOREST),i=1,N_WAVELENGTHS)/ &
    0.030_dp, 0.030_dp, 0.032_dp, 0.030_dp, 0.030_dp, 0.029_dp, 0.027_dp, 0.034_dp,  &
    0.035_dp, 0.034_dp, 0.036_dp, 0.038_dp, 0.038_dp, 0.023_dp, 0.056_dp, 0.042_dp,  &
    0.035_dp, 0.012_dp, 0.014_dp, 0.013_dp, 0.013_dp, 0.074_dp, 0.255_dp, 0.337_dp,  &
    0.318_dp, 0.314_dp, 0.316_dp, 0.315_dp, 0.314_dp, 0.465_dp, 0.456_dp, 0.453_dp,  &
    0.451_dp, 0.448_dp, 0.446_dp, 0.445_dp, 0.444_dp, 0.441_dp, 0.391_dp, 0.341_dp,  &
    0.290_dp, 0.240_dp, 0.190_dp, 0.222_dp, 0.255_dp, 0.287_dp, 0.319_dp, 0.351_dp,  &
    0.288_dp, 0.226_dp, 0.163_dp, 0.101_dp, 0.038_dp, 0.020_dp, 0.038_dp, 0.048_dp,  &
    0.038_dp, 0.038_dp, 0.038_dp, 0.038_dp, 0.038_dp, 0.038_dp, 0.038_dp, 0.038_dp,  &
    0.038_dp, 0.038_dp, 0.038_dp, 0.038_dp, 0.038_dp, 0.038_dp, 0.050_dp, 0.038_dp,  &
    0.019_dp, 0.019_dp/
    ! ...pine_forest
    DATA (reflectance(i,PINE_FOREST),i=1,N_WAVELENGTHS)/ &
    0.012_dp, 0.013_dp, 0.014_dp, 0.015_dp, 0.016_dp, 0.017_dp, 0.018_dp, 0.019_dp,  &
    0.020_dp, 0.023_dp, 0.025_dp, 0.027_dp, 0.030_dp, 0.035_dp, 0.040_dp, 0.037_dp,  &
    0.035_dp, 0.033_dp, 0.031_dp, 0.030_dp, 0.075_dp, 0.150_dp, 0.250_dp, 0.260_dp,  &
    0.270_dp, 0.280_dp, 0.290_dp, 0.295_dp, 0.300_dp, 0.310_dp, 0.305_dp, 0.300_dp,  &
    0.280_dp, 0.295_dp, 0.300_dp, 0.270_dp, 0.230_dp, 0.250_dp, 0.210_dp, 0.160_dp,  &
    0.110_dp, 0.075_dp, 0.095_dp, 0.115_dp, 0.125_dp, 0.130_dp, 0.140_dp, 0.150_dp,  &
    0.120_dp, 0.090_dp, 0.070_dp, 0.090_dp, 0.105_dp, 0.120_dp, 0.100_dp, 0.095_dp,  &
    0.090_dp, 0.070_dp, 0.050_dp, 0.030_dp, 0.025_dp, 0.022_dp, 0.015_dp, 0.012_dp,  &
    0.010_dp, 0.010_dp, 0.010_dp, 0.010_dp, 0.010_dp, 0.010_dp, 0.010_dp, 0.010_dp,  &
    0.010_dp, 0.010_dp/
    ! ...tundra
    DATA (reflectance(i,TUNDRA),i=1,N_WAVELENGTHS)/ &
    0.032_dp, 0.032_dp, 0.034_dp, 0.037_dp, 0.039_dp, 0.041_dp, 0.042_dp, 0.046_dp,  &
    0.058_dp, 0.063_dp, 0.075_dp, 0.073_dp, 0.070_dp, 0.075_dp, 0.081_dp, 0.083_dp,  &
    0.084_dp, 0.084_dp, 0.086_dp, 0.103_dp, 0.123_dp, 0.154_dp, 0.201_dp, 0.228_dp,  &
    0.242_dp, 0.245_dp, 0.250_dp, 0.254_dp, 0.257_dp, 0.256_dp, 0.257_dp, 0.258_dp,  &
    0.270_dp, 0.274_dp, 0.279_dp, 0.276_dp, 0.274_dp, 0.273_dp, 0.244_dp, 0.216_dp,  &
    0.180_dp, 0.150_dp, 0.128_dp, 0.143_dp, 0.156_dp, 0.167_dp, 0.174_dp, 0.179_dp,  &
    0.164_dp, 0.147_dp, 0.118_dp, 0.098_dp, 0.087_dp, 0.065_dp, 0.050_dp, 0.087_dp,  &
    0.099_dp, 0.124_dp, 0.104_dp, 0.095_dp, 0.041_dp, 0.055_dp, 0.039_dp, 0.032_dp,  &
    0.045_dp, 0.046_dp, 0.053_dp, 0.056_dp, 0.057_dp, 0.056_dp, 0.047_dp, 0.043_dp,  &
    0.041_dp, 0.040_dp/
    ! ...grass_soil
    DATA (reflectance(i,GRASS_SOIL),i=1,N_WAVELENGTHS)/ &
    0.024_dp, 0.024_dp, 0.024_dp, 0.024_dp, 0.024_dp, 0.024_dp, 0.024_dp, 0.026_dp,  &
    0.028_dp, 0.031_dp, 0.044_dp, 0.048_dp, 0.056_dp, 0.087_dp, 0.101_dp, 0.107_dp,  &
    0.115_dp, 0.123_dp, 0.130_dp, 0.175_dp, 0.214_dp, 0.250_dp, 0.331_dp, 0.382_dp,  &
    0.403_dp, 0.410_dp, 0.414_dp, 0.416_dp, 0.422_dp, 0.373_dp, 0.380_dp, 0.387_dp,  &
    0.393_dp, 0.394_dp, 0.401_dp, 0.399_dp, 0.401_dp, 0.402_dp, 0.361_dp, 0.312_dp,  &
    0.247_dp, 0.183_dp, 0.149_dp, 0.182_dp, 0.210_dp, 0.234_dp, 0.249_dp, 0.257_dp,  &
    0.232_dp, 0.200_dp, 0.123_dp, 0.082_dp, 0.069_dp, 0.090_dp, 0.044_dp, 0.092_dp,  &
    0.164_dp, 0.179_dp, 0.185_dp, 0.178_dp, 0.060_dp, 0.072_dp, 0.048_dp, 0.036_dp,  &
    0.038_dp, 0.048_dp, 0.057_dp, 0.060_dp, 0.067_dp, 0.090_dp, 0.075_dp, 0.068_dp,  &
    0.054_dp, 0.048_dp/
    ! ...broadleaf_pine_forest
    DATA (reflectance(i,BROADLEAF_PINE_FOREST),i=1,N_WAVELENGTHS)/ &
    0.017_dp, 0.018_dp, 0.019_dp, 0.020_dp, 0.020_dp, 0.021_dp, 0.021_dp, 0.024_dp,  &
    0.025_dp, 0.026_dp, 0.028_dp, 0.030_dp, 0.032_dp, 0.031_dp, 0.045_dp, 0.039_dp,  &
    0.035_dp, 0.027_dp, 0.026_dp, 0.025_dp, 0.056_dp, 0.127_dp, 0.252_dp, 0.283_dp,  &
    0.284_dp, 0.290_dp, 0.298_dp, 0.301_dp, 0.304_dp, 0.357_dp, 0.350_dp, 0.346_dp,  &
    0.331_dp, 0.341_dp, 0.344_dp, 0.323_dp, 0.294_dp, 0.307_dp, 0.264_dp, 0.214_dp,  &
    0.164_dp, 0.124_dp, 0.124_dp, 0.147_dp, 0.164_dp, 0.177_dp, 0.194_dp, 0.280_dp,  &
    0.170_dp, 0.131_dp, 0.098_dp, 0.093_dp, 0.085_dp, 0.090_dp, 0.081_dp, 0.081_dp,  &
    0.074_dp, 0.060_dp, 0.046_dp, 0.032_dp, 0.029_dp, 0.027_dp, 0.022_dp, 0.020_dp,  &
    0.018_dp, 0.018_dp, 0.018_dp, 0.018_dp, 0.018_dp, 0.018_dp, 0.022_dp, 0.018_dp,  &
    0.013_dp, 0.013_dp/
    ! ...grass_scrub
    DATA (reflectance(i,GRASS_SCRUB),i=1,N_WAVELENGTHS)/ &
    0.019_dp, 0.019_dp, 0.019_dp, 0.019_dp, 0.019_dp, 0.019_dp, 0.019_dp, 0.021_dp,  &
    0.025_dp, 0.033_dp, 0.055_dp, 0.058_dp, 0.066_dp, 0.063_dp, 0.077_dp, 0.069_dp,  &
    0.059_dp, 0.050_dp, 0.048_dp, 0.087_dp, 0.130_dp, 0.192_dp, 0.287_dp, 0.357_dp,  &
    0.401_dp, 0.410_dp, 0.416_dp, 0.422_dp, 0.428_dp, 0.502_dp, 0.500_dp, 0.504_dp,  &
    0.509_dp, 0.502_dp, 0.496_dp, 0.489_dp, 0.482_dp, 0.475_dp, 0.408_dp, 0.340_dp,  &
    0.273_dp, 0.205_dp, 0.138_dp, 0.166_dp, 0.195_dp, 0.224_dp, 0.233_dp, 0.243_dp,  &
    0.210_dp, 0.177_dp, 0.145_dp, 0.112_dp, 0.079_dp, 0.096_dp, 0.050_dp, 0.095_dp,  &
    0.150_dp, 0.205_dp, 0.208_dp, 0.208_dp, 0.045_dp, 0.073_dp, 0.060_dp, 0.050_dp,  &
    0.050_dp, 0.050_dp, 0.049_dp, 0.055_dp, 0.070_dp, 0.108_dp, 0.105_dp, 0.088_dp,  &
    0.073_dp, 0.080_dp/
    ! ...soil_grass_scrub
    DATA (reflectance(i,SOIL_GRASS_SCRUB),i=1,N_WAVELENGTHS)/ &
    0.021_dp, 0.021_dp, 0.021_dp, 0.021_dp, 0.021_dp, 0.021_dp, 0.021_dp, 0.022_dp,  &
    0.025_dp, 0.030_dp, 0.045_dp, 0.049_dp, 0.060_dp, 0.073_dp, 0.087_dp, 0.090_dp,  &
    0.092_dp, 0.093_dp, 0.098_dp, 0.131_dp, 0.160_dp, 0.195_dp, 0.260_dp, 0.309_dp,  &
    0.340_dp, 0.348_dp, 0.355_dp, 0.359_dp, 0.366_dp, 0.377_dp, 0.380_dp, 0.384_dp,  &
    0.387_dp, 0.385_dp, 0.387_dp, 0.383_dp, 0.382_dp, 0.380_dp, 0.339_dp, 0.294_dp,  &
    0.238_dp, 0.183_dp, 0.147_dp, 0.176_dp, 0.201_dp, 0.224_dp, 0.230_dp, 0.230_dp,  &
    0.208_dp, 0.181_dp, 0.125_dp, 0.092_dp, 0.077_dp, 0.078_dp, 0.046_dp, 0.085_dp,  &
    0.146_dp, 0.173_dp, 0.172_dp, 0.165_dp, 0.051_dp, 0.064_dp, 0.052_dp, 0.042_dp,  &
    0.042_dp, 0.046_dp, 0.049_dp, 0.053_dp, 0.060_dp, 0.081_dp, 0.073_dp, 0.065_dp,  &
    0.052_dp, 0.056_dp/
    ! ...urban_concrete
    DATA (reflectance(i,URBAN_CONCRETE),i=1,N_WAVELENGTHS)/ &
    0.026_dp, 0.026_dp, 0.026_dp, 0.026_dp, 0.026_dp, 0.026_dp, 0.027_dp, 0.028_dp,  &
    0.028_dp, 0.029_dp, 0.031_dp, 0.032_dp, 0.035_dp, 0.037_dp, 0.043_dp, 0.047_dp,  &
    0.051_dp, 0.052_dp, 0.055_dp, 0.060_dp, 0.063_dp, 0.084_dp, 0.091_dp, 0.099_dp,  &
    0.100_dp, 0.104_dp, 0.109_dp, 0.116_dp, 0.119_dp, 0.126_dp, 0.129_dp, 0.130_dp,  &
    0.133_dp, 0.134_dp, 0.137_dp, 0.129_dp, 0.127_dp, 0.125_dp, 0.119_dp, 0.109_dp,  &
    0.094_dp, 0.083_dp, 0.078_dp, 0.087_dp, 0.094_dp, 0.099_dp, 0.102_dp, 0.107_dp,  &
    0.102_dp, 0.096_dp, 0.079_dp, 0.069_dp, 0.065_dp, 0.032_dp, 0.029_dp, 0.039_dp,  &
    0.033_dp, 0.074_dp, 0.072_dp, 0.058_dp, 0.026_dp, 0.023_dp, 0.023_dp, 0.021_dp,  &
    0.027_dp, 0.029_dp, 0.032_dp, 0.033_dp, 0.030_dp, 0.023_dp, 0.020_dp, 0.019_dp,  &
    0.016_dp, 0.016_dp/
    ! ...pine_brush
    DATA (reflectance(i,PINE_BRUSH),i=1,N_WAVELENGTHS)/ &
    0.012_dp, 0.013_dp, 0.013_dp, 0.014_dp, 0.014_dp, 0.015_dp, 0.015_dp, 0.016_dp,  &
    0.018_dp, 0.024_dp, 0.035_dp, 0.039_dp, 0.049_dp, 0.038_dp, 0.047_dp, 0.045_dp,  &
    0.039_dp, 0.032_dp, 0.031_dp, 0.032_dp, 0.055_dp, 0.104_dp, 0.162_dp, 0.187_dp,  &
    0.218_dp, 0.230_dp, 0.240_dp, 0.247_dp, 0.255_dp, 0.333_dp, 0.328_dp, 0.324_dp,  &
    0.312_dp, 0.315_dp, 0.313_dp, 0.294_dp, 0.269_dp, 0.275_dp, 0.240_dp, 0.199_dp,  &
    0.159_dp, 0.126_dp, 0.120_dp, 0.139_dp, 0.153_dp, 0.165_dp, 0.160_dp, 0.205_dp,  &
    0.134_dp, 0.113_dp, 0.097_dp, 0.100_dp, 0.102_dp, 0.081_dp, 0.075_dp, 0.080_dp,  &
    0.095_dp, 0.110_dp, 0.090_dp, 0.075_dp, 0.028_dp, 0.031_dp, 0.038_dp, 0.034_dp,  &
    0.030_dp, 0.025_dp, 0.020_dp, 0.023_dp, 0.025_dp, 0.030_dp, 0.035_dp, 0.030_dp,  &
    0.025_dp, 0.040_dp/
    ! ...broadleaf_brush
    DATA (reflectance(i,BROADLEAF_BRUSH),i=1,N_WAVELENGTHS)/ &
    0.019_dp, 0.019_dp, 0.020_dp, 0.019_dp, 0.019_dp, 0.019_dp, 0.018_dp, 0.021_dp,  &
    0.023_dp, 0.029_dp, 0.041_dp, 0.045_dp, 0.055_dp, 0.033_dp, 0.055_dp, 0.048_dp,  &
    0.040_dp, 0.023_dp, 0.024_dp, 0.025_dp, 0.026_dp, 0.064_dp, 0.146_dp, 0.203_dp,  &
    0.227_dp, 0.234_dp, 0.240_dp, 0.245_dp, 0.252_dp, 0.399_dp, 0.392_dp, 0.390_dp,  &
    0.386_dp, 0.380_dp, 0.374_dp, 0.368_dp, 0.362_dp, 0.356_dp, 0.318_dp, 0.279_dp,  &
    0.240_dp, 0.202_dp, 0.163_dp, 0.187_dp, 0.211_dp, 0.235_dp, 0.236_dp, 0.236_dp,  &
    0.203_dp, 0.171_dp, 0.139_dp, 0.106_dp, 0.074_dp, 0.033_dp, 0.045_dp, 0.058_dp,  &
    0.075_dp, 0.105_dp, 0.093_dp, 0.087_dp, 0.033_dp, 0.039_dp, 0.051_dp, 0.048_dp,  &
    0.045_dp, 0.039_dp, 0.033_dp, 0.036_dp, 0.039_dp, 0.045_dp, 0.056_dp, 0.045_dp,  &
    0.032_dp, 0.050_dp/
    ! ...wet_soil
    DATA (reflectance(i,WET_SOIL),i=1,N_WAVELENGTHS)/ &
    0.029_dp, 0.029_dp, 0.029_dp, 0.029_dp, 0.026_dp, 0.026_dp, 0.026_dp, 0.026_dp,  &
    0.035_dp, 0.038_dp, 0.041_dp, 0.041_dp, 0.038_dp, 0.057_dp, 0.063_dp, 0.073_dp,  &
    0.082_dp, 0.082_dp, 0.090_dp, 0.102_dp, 0.106_dp, 0.103_dp, 0.113_dp, 0.122_dp,  &
    0.128_dp, 0.132_dp, 0.135_dp, 0.136_dp, 0.140_dp, 0.107_dp, 0.112_dp, 0.114_dp,  &
    0.114_dp, 0.117_dp, 0.125_dp, 0.125_dp, 0.128_dp, 0.131_dp, 0.131_dp, 0.125_dp,  &
    0.106_dp, 0.087_dp, 0.093_dp, 0.107_dp, 0.118_dp, 0.124_dp, 0.124_dp, 0.118_dp,  &
    0.114_dp, 0.105_dp, 0.059_dp, 0.043_dp, 0.049_dp, 0.026_dp, 0.021_dp, 0.045_dp,  &
    0.080_dp, 0.073_dp, 0.069_dp, 0.055_dp, 0.035_dp, 0.035_dp, 0.030_dp, 0.025_dp,  &
    0.025_dp, 0.025_dp, 0.030_dp, 0.030_dp, 0.028_dp, 0.025_dp, 0.018_dp, 0.025_dp,  &
    0.025_dp, 0.025_dp/
    ! ...scrub_soil
    DATA (reflectance(i,SCRUB_SOIL),i=1,N_WAVELENGTHS)/ &
    0.019_dp, 0.019_dp, 0.019_dp, 0.019_dp, 0.019_dp, 0.019_dp, 0.019_dp, 0.019_dp,  &
    0.020_dp, 0.025_dp, 0.036_dp, 0.042_dp, 0.057_dp, 0.069_dp, 0.083_dp, 0.094_dp,  &
    0.102_dp, 0.107_dp, 0.117_dp, 0.132_dp, 0.138_dp, 0.143_dp, 0.161_dp, 0.188_dp,  &
    0.215_dp, 0.226_dp, 0.234_dp, 0.238_dp, 0.248_dp, 0.256_dp, 0.260_dp, 0.262_dp,  &
    0.260_dp, 0.260_dp, 0.265_dp, 0.262_dp, 0.262_dp, 0.262_dp, 0.250_dp, 0.230_dp,  &
    0.195_dp, 0.160_dp, 0.155_dp, 0.179_dp, 0.200_dp, 0.215_dp, 0.207_dp, 0.191_dp,  &
    0.182_dp, 0.166_dp, 0.106_dp, 0.081_dp, 0.084_dp, 0.047_dp, 0.044_dp, 0.068_dp,  &
    0.124_dp, 0.135_dp, 0.123_dp, 0.108_dp, 0.048_dp, 0.046_dp, 0.048_dp, 0.040_dp,  &
    0.038_dp, 0.040_dp, 0.042_dp, 0.044_dp, 0.043_dp, 0.044_dp, 0.039_dp, 0.038_dp,  &
    0.028_dp, 0.040_dp/
    ! ...broadleaf70_pine30
    DATA (reflectance(i,BROADLEAF70_PINE30),i=1,N_WAVELENGTHS)/ &
    0.025_dp, 0.025_dp, 0.027_dp, 0.026_dp, 0.026_dp, 0.025_dp, 0.024_dp, 0.030_dp,  &
    0.031_dp, 0.031_dp, 0.033_dp, 0.035_dp, 0.036_dp, 0.027_dp, 0.051_dp, 0.041_dp,  &
    0.035_dp, 0.018_dp, 0.019_dp, 0.018_dp, 0.032_dp, 0.097_dp, 0.254_dp, 0.314_dp,  &
    0.304_dp, 0.304_dp, 0.308_dp, 0.309_dp, 0.310_dp, 0.419_dp, 0.411_dp, 0.407_dp,  &
    0.400_dp, 0.402_dp, 0.402_dp, 0.393_dp, 0.380_dp, 0.384_dp, 0.337_dp, 0.287_dp,  &
    0.236_dp, 0.191_dp, 0.162_dp, 0.190_dp, 0.216_dp, 0.240_dp, 0.265_dp, 0.321_dp,  &
    0.238_dp, 0.185_dp, 0.135_dp, 0.098_dp, 0.058_dp, 0.050_dp, 0.057_dp, 0.062_dp,  &
    0.054_dp, 0.048_dp, 0.042_dp, 0.036_dp, 0.034_dp, 0.033_dp, 0.031_dp, 0.030_dp,  &
    0.030_dp, 0.030_dp, 0.030_dp, 0.030_dp, 0.030_dp, 0.030_dp, 0.038_dp, 0.030_dp,  &
    0.016_dp, 0.016_dp/


    ! Assign the data for the current surface index
    npoess%surface_type(Sfc_Idx)  = SURFACE_TYPE(Sfc_Idx)
    npoess%reflectance(:,Sfc_Idx) = reflectance(:,Sfc_Idx)
    ! ...Flip reflectance to ascending frequency order
    npoess%reflectance(:,Sfc_Idx) = reflectance(N_WAVELENGTHS:1:-1,Sfc_Idx)

  END SUBROUTINE Load_NPOESS_Data

END PROGRAM Create_NPOESS_EmisCoeff
