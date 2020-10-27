MODULE IRVISlandCoeff_Module

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds               , ONLY: dp=>Double
  USE File_Utility             , ONLY: Get_Lun, File_Exists
  USE Message_Handler          , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                       Display_Message
  USE Spectral_Units_Conversion, ONLY: micron_to_inverse_cm
  USE SEcategory_Define        , ONLY: SEcategory_type      , &
                                       OPERATOR(==)          , &
                                       SEcategory_Associated, &
                                       SEcategory_Create    , &
                                       SEcategory_Destroy   , &
                                       SEcategory_WriteFile , &
                                       SEcategory_ReadFile
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Visibility
  ! ----------
  PRIVATE
  PUBLIC :: IRVISlandCoeff_CreateFile


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  CHARACTER(*), PARAMETER :: SURFACE_TYPE_NAME = 'land'
  CHARACTER(*), PARAMETER :: MODULE_NAME = 'IRVIS'//SURFACE_TYPE_NAME//'Coeff_Module'
  INTEGER , PARAMETER :: ML = 256
  REAL(dp), PARAMETER :: IR_CUTOFF_WAVELENGTH  = 3.0_dp
  REAL(dp), PARAMETER :: VIS_CUTOFF_WAVELENGTH = 4.0_dp

  ! The NPOESS surface type indices
  INTEGER, PARAMETER :: INVALID = 0
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


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                           ## PUBLIC PROCEDURES ##                          ##
!##                                                                            ##
!################################################################################
!################################################################################

  SUBROUTINE IRVISlandCoeff_CreateFile(visible)
    LOGICAL, OPTIONAL, INTENT(IN) :: visible
    ! Local variables
    TYPE(SEcategory_type) :: npoess
    TYPE(SEcategory_type) :: usgs
    TYPE(SEcategory_type) :: igbp

    ! Fill structures with data
    CALL Load_NPOESS_Data(npoess, Visible=visible)
    CALL Load_USGS_Data(npoess,usgs)
    CALL Load_IGBP_Data(npoess,igbp)

    ! Write data to file
    CALL Write_File(npoess, Visible=visible)
    CALL Write_File(usgs, Visible=visible)
    CALL Write_File(igbp, Visible=visible)

    ! Cleanup
    CALL SEcategory_Destroy( npoess )
    CALL SEcategory_Destroy( usgs )
    CALL SEcategory_Destroy( igbp )

  END SUBROUTINE IRVISlandCoeff_CreateFile


!################################################################################
!################################################################################
!##                                                                            ##
!##                          ## PRIVATE PROCEDURES ##                          ##
!##                                                                            ##
!################################################################################
!################################################################################

  SUBROUTINE Write_File(sec, visible)
    TYPE(SEcategory_type), INTENT(IN) :: sec
    LOGICAL,     OPTIONAL, INTENT(IN) :: visible
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_File'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: sensor_type_name, filename_suffix
    INTEGER :: err_stat
    LOGICAL :: infrared
    TYPE(SEcategory_type) :: sec_copy

    ! Setup
    infrared = .TRUE.
    IF ( PRESENT(Visible) ) infrared = .NOT. Visible
    IF ( infrared ) THEN
      sensor_type_name = 'IR'
    ELSE
      sensor_type_name = 'VIS'
    END IF
    filename_suffix  = '.'//TRIM(sensor_type_name)//SURFACE_TYPE_NAME//'.EmisCoeff.bin'

    ! Write data to file
    err_stat = SEcategory_WriteFile( &
                 sec, &
                 TRIM(sec%Classification_Name)//TRIM(filename_suffix), &
                 Title = TRIM(sensor_type_name)//' '//&
                         TRIM(sec%Classification_Name)//' Surface Reflectances', &
                 History = MODULE_VERSION_ID, &
                 Comment = 'Data extracted from CRTM_surface_ir_emissivity module.' )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing '//TRIM(sec%Classification_Name)//' data to file.'
      CALL Display_Message( MODULE_NAME//','//ROUTINE_NAME, msg, FAILURE ); STOP
    END IF


    ! Read data from file
    err_stat = SEcategory_ReadFile( &
                 sec_copy, &
                 TRIM(sec%Classification_Name)//TRIM(filename_suffix) )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading '//TRIM(sec%Classification_Name)//' data from file.'
      CALL Display_Message( MODULE_NAME//','//ROUTINE_NAME, msg, FAILURE ); STOP
    END IF


    ! Check that structures are equal
    IF ( sec == sec_copy ) THEN
      msg = TRIM(sec%Classification_Name)//' structure read from file is the same!'
    ELSE
      msg = TRIM(sec%Classification_Name)//' structure read from file is different!'
    END IF
    CALL Display_Message( MODULE_NAME//','//ROUTINE_NAME, msg, INFORMATION )


    ! Clean up
    CALL SEcategory_Destroy( sec_copy )

  END SUBROUTINE Write_File



  SUBROUTINE Load_NPOESS_Data(npoess, visible)
    TYPE(SEcategory_type), INTENT(OUT) :: npoess
    LOGICAL,     OPTIONAL, INTENT(IN)  :: visible
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Load_NPOESS_Data'
    ! ...The surface types
    INTEGER     , PARAMETER :: N_SURFACE_TYPES = 20
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
    ! ...The valid surface type indicators (all TRUE for NPOESS)
    LOGICAL, PARAMETER :: SURFACE_TYPE_ISVALID(N_SURFACE_TYPES) = &
     (/ .TRUE., &  ! compacted_soil
        .TRUE., &  ! tilled_soil
        .TRUE., &  ! sand
        .TRUE., &  ! rock
        .TRUE., &  ! irrigated_low_vegetation
        .TRUE., &  ! meadow_grass
        .TRUE., &  ! scrub
        .TRUE., &  ! broadleaf_forest
        .TRUE., &  ! pine_forest
        .TRUE., &  ! tundra
        .TRUE., &  ! grass_soil
        .TRUE., &  ! broadleaf_pine_forest
        .TRUE., &  ! grass_scrub
        .TRUE., &  ! soil_grass_scrub
        .TRUE., &  ! urban_concrete
        .TRUE., &  ! pine_brush
        .TRUE., &  ! broadleaf_brush
        .TRUE., &  ! wet_soil
        .TRUE., &  ! scrub_soil
        .TRUE. /)  ! broadleaf70_pine30
    ! ...The spectral dimension
    INTEGER , PARAMETER :: N_WAVELENGTHS = 74
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
    ! ...The reflectance data
    REAL(dp) :: reflectance(N_WAVELENGTHS, N_SURFACE_TYPES)
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
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: infrared
    INTEGER :: n, n_points
    INTEGER, ALLOCATABLE :: idx(:)

    ! Setup
    infrared = .TRUE.
    IF ( PRESENT(Visible) ) infrared = .NOT. Visible


    ! Select indices for spectral region
    IF ( infrared ) THEN
      n_points = COUNT(WAVELENGTH >= IR_CUTOFF_WAVELENGTH)
      ALLOCATE(idx(n_points))
      idx = PACK((/(i,i=1,N_WAVELENGTHS)/),WAVELENGTH >= IR_CUTOFF_WAVELENGTH)
    ELSE
      n_points = COUNT(WAVELENGTH <= VIS_CUTOFF_WAVELENGTH)
      ALLOCATE(idx(n_points))
      idx = PACK((/(i,i=1,N_WAVELENGTHS)/),WAVELENGTH <= VIS_CUTOFF_WAVELENGTH)
    END IF
    ! ...Reverse the index array so the frequencies are in ascending
    ! ...order when the wavelength units are converted.
    idx = idx(n_points:1:-1)


    ! Create structure
    CALL SEcategory_Create( &
           npoess, &
           n_Frequencies   = n_points, &
           n_Surface_Types = N_SURFACE_TYPES )
    IF ( .NOT. SEcategory_Associated(npoess) ) THEN
      msg = 'Error creating NPOESS SEcategory structure'
      CALL Display_Message( MODULE_NAME//','//ROUTINE_NAME, msg, FAILURE ); STOP
    END IF


    ! Assign the classification type and spectral ordinate
    npoess%Classification_Name = 'NPOESS'
    npoess%Frequency = micron_to_inverse_cm(WAVELENGTH(idx))


    ! Assign the data to the structure
    npoess%Surface_Type         = SURFACE_TYPE
    npoess%Surface_Type_IsValid = SURFACE_TYPE_ISVALID
    DO n = 1, N_SURFACE_TYPES
      npoess%Reflectance(:,n) = reflectance(idx,n)
    END DO


    ! Clean up
    DEALLOCATE(idx)

  END SUBROUTINE Load_NPOESS_Data



  SUBROUTINE Load_USGS_Data(npoess,usgs)
    TYPE(SEcategory_type), INTENT(IN)  :: npoess
    TYPE(SEcategory_type), INTENT(OUT) :: usgs
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Load_USGS_Data'
    ! ...The surface types
    INTEGER     , PARAMETER :: N_SURFACE_TYPES = 27
    CHARACTER(*), PARAMETER :: SURFACE_TYPE(N_SURFACE_TYPES) = &
     (/ 'urban_and_built-up_land                     ', &
        'dryland_cropland_and_pasture                ', &
        'irrigated_cropland_and_pasture              ', &
        'mixed_dryland/irrigated_cropland_and_pasture', &
        'cropland/grassland_mosaic                   ', &
        'cropland/woodland_mosaic                    ', &
        'grassland                                   ', &
        'shrubland                                   ', &
        'mixed_shrubland/grassland                   ', &
        'savanna                                     ', &
        'deciduous_broadleaf_forest                  ', &
        'deciduous_needleleaf_forest                 ', &
        'evergreen_broadleaf_forest                  ', &
        'evergreen_needleleaf_forest                 ', &
        'mixed_forest                                ', &
        'water_bodies (empty)                        ', &
        'herbaceous_wetland                          ', &
        'wooded_wetland                              ', &
        'barren_or_sparsely_vegetated                ', &
        'herbaceous_tundra                           ', &
        'wooded_tundra                               ', &
        'mixed_tundra                                ', &
        'bare_ground_tundra                          ', &
        'snow_or_ice (empty)                         ', &
        'playa                                       ', &
        'lava                                        ', &
        'white_sand                                  ' /)
    ! ...The valid surface type indicators (FALSE for non-land types)
    LOGICAL, PARAMETER :: SURFACE_TYPE_ISVALID(N_SURFACE_TYPES) = &
     (/ .TRUE.,  &  ! urban_and_built-up_land
        .TRUE.,  &  ! dryland_cropland_and_pasture
        .TRUE.,  &  ! irrigated_cropland_and_pasture
        .TRUE.,  &  ! mixed_dryland/irrigated_cropland_and_pasture
        .TRUE.,  &  ! cropland/grassland_mosaic
        .TRUE.,  &  ! cropland/woodland_mosaic
        .TRUE.,  &  ! grassland
        .TRUE.,  &  ! shrubland
        .TRUE.,  &  ! mixed_shrubland/grassland
        .TRUE.,  &  ! savanna
        .TRUE.,  &  ! deciduous_broadleaf_forest
        .TRUE.,  &  ! deciduous_needleleaf_forest
        .TRUE.,  &  ! evergreen_broadleaf_forest
        .TRUE.,  &  ! evergreen_needleleaf_forest
        .TRUE.,  &  ! mixed_forest
        .FALSE., &  ! water_bodies (empty)
        .TRUE.,  &  ! herbaceous_wetland
        .TRUE.,  &  ! wooded_wetland
        .TRUE.,  &  ! barren_or_sparsely_vegetated
        .TRUE.,  &  ! herbaceous_tundra
        .TRUE.,  &  ! wooded_tundra
        .TRUE.,  &  ! mixed_tundra
        .TRUE.,  &  ! bare_ground_tundra
        .FALSE., &  ! snow_or_ice (empty)
        .TRUE.,  &  ! playa
        .TRUE.,  &  ! lava
        .TRUE.  /)  ! white_sand
    ! ...The NPOESS->USGS mapping
    INTEGER, PARAMETER :: NPOESS_TO_USGS(N_SURFACE_TYPES) = &
      (/ URBAN_CONCRETE          , &  ! urban_and_built-up_land
         COMPACTED_SOIL          , &  ! dryland_cropland_and_pasture
         IRRIGATED_LOW_VEGETATION, &  ! irrigated_cropland_and_pasture
         GRASS_SOIL              , &  ! mixed_dryland/irrigated_cropland_and_pasture
         MEADOW_GRASS            , &  ! cropland/grassland_mosaic
         MEADOW_GRASS            , &  ! cropland/woodland_mosaic
         MEADOW_GRASS            , &  ! grassland
         SCRUB                   , &  ! shrubland
         GRASS_SCRUB             , &  ! mixed_shrubland/grassland
         MEADOW_GRASS            , &  ! savanna
         BROADLEAF_FOREST        , &  ! deciduous_broadleaf_forest
         PINE_FOREST             , &  ! deciduous_needleleaf_forest
         BROADLEAF_FOREST        , &  ! evergreen_broadleaf_forest
         PINE_FOREST             , &  ! evergreen_needleleaf_forest
         BROADLEAF_PINE_FOREST   , &  ! mixed_forest
         INVALID                 , &  ! water_bodies (empty)
         WET_SOIL                , &  ! herbaceous_wetland
         WET_SOIL                , &  ! wooded_wetland
         IRRIGATED_LOW_VEGETATION, &  ! barren_or_sparsely_vegetated
         TUNDRA                  , &  ! herbaceous_tundra
         TUNDRA                  , &  ! wooded_tundra
         TUNDRA                  , &  ! mixed_tundra
         TUNDRA                  , &  ! bare_ground_tundra
         INVALID                 , &  ! snow_or_ice (empty)
         COMPACTED_SOIL          , &  ! playa
         ROCK                    , &  ! lava
         SAND                     /)  ! white_sand
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: n


    ! Create structure
    CALL SEcategory_Create( &
           usgs, &
           n_Frequencies   = npoess%n_Frequencies, &
           n_Surface_Types = N_SURFACE_TYPES )
    IF ( .NOT. SEcategory_Associated(npoess) ) THEN
      msg = 'Error creating USGS SEcategory structure'
      CALL Display_Message( MODULE_NAME//','//ROUTINE_NAME, msg, FAILURE ); STOP
    END IF


    ! Assign the classification type and spectral ordinate
    usgs%Classification_Name = 'USGS'
    usgs%Frequency = npoess%Frequency


    ! Assign the data to the structure
    usgs%Surface_Type         = SURFACE_TYPE
    usgs%Surface_Type_IsValid = SURFACE_TYPE_ISVALID
    DO n = 1, N_SURFACE_TYPES
      IF ( SURFACE_TYPE_ISVALID(n) ) THEN
        usgs%Reflectance(:,n) = npoess%Reflectance(:,NPOESS_TO_USGS(n))
      ELSE
        usgs%Reflectance(:,n) = -1.0_dp
      END IF
    END DO

  END SUBROUTINE Load_USGS_Data



  SUBROUTINE Load_IGBP_Data(npoess,igbp)
    TYPE(SEcategory_type), INTENT(IN)  :: npoess
    TYPE(SEcategory_type), INTENT(OUT) :: igbp
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Load_IGBP_Data'
    ! ...The surface types
    INTEGER     , PARAMETER :: N_SURFACE_TYPES = 20
    CHARACTER(*), PARAMETER :: SURFACE_TYPE(N_SURFACE_TYPES) = &
     (/ 'evergreen_needleleaf_forest       ', &
        'evergreen_broadleaf_forest        ', &
        'deciduous_needleleaf_forest       ', &
        'deciduous_broadleaf_forest        ', &
        'mixed_forests                     ', &
        'closed_shrublands                 ', &
        'open_shrublands                   ', &
        'woody_savannas                    ', &
        'savannas                          ', &
        'grasslands                        ', &
        'permanent_wetlands                ', &
        'croplands                         ', &
        'urban_and_built-up                ', &
        'cropland/natural_vegetation_mosaic', &
        'snow_and_ice (empty)              ', &
        'barren_or_sparsely_vegetated      ', &
        'water (empty)                     ', &
        'wooded_tundra                     ', &
        'mixed_tundra                      ', &
        'bare_ground_tundra                ' /)
    ! ...The valid surface type indicators (FALSE for non-land types)
    LOGICAL, PARAMETER :: SURFACE_TYPE_ISVALID(N_SURFACE_TYPES) = &
     (/ .TRUE.,  &  ! evergreen_needleleaf_forest
        .TRUE.,  &  ! evergreen_broadleaf_forest
        .TRUE.,  &  ! deciduous_needleleaf_forest
        .TRUE.,  &  ! deciduous_broadleaf_forest
        .TRUE.,  &  ! mixed_forests
        .TRUE.,  &  ! closed_shrublands
        .TRUE.,  &  ! open_shrublands
        .TRUE.,  &  ! woody_savannas
        .TRUE.,  &  ! savannas
        .TRUE.,  &  ! grasslands
        .TRUE.,  &  ! permanent_wetlands
        .TRUE.,  &  ! croplands
        .TRUE.,  &  ! urban_and_built-up
        .TRUE.,  &  ! cropland/natural_vegetation_mosaic
        .FALSE., &  ! snow_and_ice (empty)
        .TRUE.,  &  ! barren_or_sparsely_vegetated
        .FALSE., &  ! water (empty)
        .TRUE.,  &  ! wooded_tundra
        .TRUE.,  &  ! mixed_tundra
        .TRUE.  /)  ! bare_ground_tundra
    ! ...The NPOESS->IGBP mapping
    INTEGER, PARAMETER :: NPOESS_TO_IGBP(N_SURFACE_TYPES) = &
      (/ PINE_FOREST          , &  ! evergreen_needleleaf_forest
         BROADLEAF_FOREST     , &  ! evergreen_broadleaf_forest
         PINE_FOREST          , &  ! deciduous_needleleaf_forest
         BROADLEAF_FOREST     , &  ! deciduous_broadleaf_forest
         BROADLEAF_PINE_FOREST, &  ! mixed_forests
         SCRUB                , &  ! closed_shrublands
         SCRUB_SOIL           , &  ! open_shrublands
         BROADLEAF_BRUSH      , &  ! woody_savannas
         BROADLEAF_BRUSH      , &  ! savannas
         SCRUB                , &  ! grasslands
         BROADLEAF_BRUSH      , &  ! permanent_wetlands
         TILLED_SOIL          , &  ! croplands
         URBAN_CONCRETE       , &  ! urban_and_built-up
         TILLED_SOIL          , &  ! cropland/natural_vegetation_mosaic
         INVALID              , &  ! snow_and_ice (empty)
         COMPACTED_SOIL       , &  ! barren_or_sparsely_vegetated
         INVALID              , &  ! water (empty)
         TUNDRA               , &  ! wooded_tundra
         TUNDRA               , &  ! mixed_tundra
         TUNDRA                /)  ! bare_ground_tundra
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: n


    ! Create structure
    CALL SEcategory_Create( &
           igbp, &
           n_Frequencies   = npoess%n_Frequencies, &
           n_Surface_Types = N_SURFACE_TYPES )
    IF ( .NOT. SEcategory_Associated(npoess) ) THEN
      msg = 'Error creating IGBP SEcategory structure'
      CALL Display_Message( MODULE_NAME//','//ROUTINE_NAME, msg, FAILURE ); STOP
    END IF


    ! Assign the classification type and spectral ordinate
    igbp%Classification_Name = 'IGBP'
    igbp%Frequency = npoess%Frequency


    ! Assign the data to the structure
    igbp%Surface_Type         = SURFACE_TYPE
    igbp%Surface_Type_IsValid = SURFACE_TYPE_ISVALID
    DO n = 1, N_SURFACE_TYPES
      IF ( SURFACE_TYPE_ISVALID(n) ) THEN
        igbp%Reflectance(:,n) = npoess%Reflectance(:,NPOESS_TO_IGBP(n))
      ELSE
        igbp%Reflectance(:,n) = -1.0_dp
      END IF
    END DO

  END SUBROUTINE Load_IGBP_Data

END MODULE IRVISlandCoeff_Module
