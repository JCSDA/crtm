MODULE IRVISsnowCoeff_Module

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
  PUBLIC :: IRVISsnowCoeff_CreateFile
  

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  CHARACTER(*), PARAMETER :: SURFACE_TYPE_NAME = 'snow'
  CHARACTER(*), PARAMETER :: MODULE_NAME = 'IRVIS'//SURFACE_TYPE_NAME//'Coeff_Module'
  INTEGER , PARAMETER :: ML = 256
  REAL(dp), PARAMETER :: IR_CUTOFF_WAVELENGTH  = 3.0_dp
  REAL(dp), PARAMETER :: VIS_CUTOFF_WAVELENGTH = 4.0_dp  
  

CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                           ## PUBLIC PROCEDURES ##                          ##
!##                                                                            ##
!################################################################################
!################################################################################

  SUBROUTINE IRVISsnowCoeff_CreateFile(visible)
    LOGICAL, OPTIONAL, INTENT(IN) :: visible
    ! Local variables
    TYPE(SEcategory_type) :: npoess

    ! Fill structures with data
    CALL Load_NPOESS_Data(npoess, Visible=visible)
  
    ! Write data to file
    CALL Write_File(npoess, Visible=visible)
  
    ! Cleanup
    CALL SEcategory_Destroy( npoess )

  END SUBROUTINE IRVISsnowCoeff_CreateFile
  

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
    ! ...The surface type indices
    INTEGER, PARAMETER :: INVALID = 0
    INTEGER, PARAMETER :: OLD_SNOW   = 1
    INTEGER, PARAMETER :: FRESH_SNOW = 2
    ! ...The surface types
    INTEGER     , PARAMETER :: N_SURFACE_TYPES = 2
    CHARACTER(*), PARAMETER :: SURFACE_TYPE(N_SURFACE_TYPES) = &
     (/ 'old_snow  ', &
        'fresh_snow' /)    
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
    ! ...old snow
    DATA (reflectance(i,OLD_SNOW),i=1,N_WAVELENGTHS)/ &
      0.882_dp,0.892_dp,0.903_dp,0.913_dp,0.924_dp,0.934_dp,0.946_dp,0.955_dp, &
      0.958_dp,0.959_dp,0.954_dp,0.951_dp,0.944_dp,0.940_dp,0.930_dp,0.917_dp, &
      0.908_dp,0.900_dp,0.889_dp,0.876_dp,0.856_dp,0.833_dp,0.807_dp,0.779_dp, &
      0.754_dp,0.727_dp,0.677_dp,0.642_dp,0.583_dp,0.537_dp,0.473_dp,0.419_dp, &
      0.351_dp,0.367_dp,0.360_dp,0.287_dp,0.164_dp,0.124_dp,0.132_dp,0.125_dp, &
      0.094_dp,0.021_dp,0.002_dp,0.000_dp,0.000_dp,0.000_dp,0.008_dp,0.009_dp, &
      0.015_dp,0.037_dp,0.025_dp,0.025_dp,0.000_dp,0.000_dp,0.031_dp,0.013_dp, &
      0.015_dp,0.010_dp,0.014_dp,0.010_dp,0.008_dp,0.009_dp,0.009_dp,0.009_dp, &
      0.009_dp,0.009_dp,0.008_dp,0.008_dp,0.006_dp,0.010_dp,0.018_dp,0.020_dp, &
      0.025_dp,0.020_dp/
    ! ...fresh snow
    DATA (reflectance(i,FRESH_SNOW),i=1,N_WAVELENGTHS)/ &
      0.961_dp,0.964_dp,0.966_dp,0.969_dp,0.972_dp,0.974_dp,0.977_dp,0.979_dp, &
      0.979_dp,0.979_dp,0.979_dp,0.979_dp,0.979_dp,0.977_dp,0.977_dp,0.974_dp, &
      0.972_dp,0.970_dp,0.969_dp,0.969_dp,0.964_dp,0.960_dp,0.953_dp,0.946_dp, &
      0.937_dp,0.923_dp,0.912_dp,0.893_dp,0.874_dp,0.858_dp,0.840_dp,0.813_dp, &
      0.790_dp,0.779_dp,0.786_dp,0.758_dp,0.654_dp,0.611_dp,0.612_dp,0.607_dp, &
      0.516_dp,0.291_dp,0.145_dp,0.115_dp,0.158_dp,0.183_dp,0.251_dp,0.291_dp, &
      0.306_dp,0.425_dp,0.150_dp,0.070_dp,0.044_dp,0.092_dp,0.031_dp,0.013_dp, &
      0.023_dp,0.009_dp,0.015_dp,0.008_dp,0.007_dp,0.008_dp,0.008_dp,0.009_dp, &
      0.008_dp,0.009_dp,0.006_dp,0.006_dp,0.005_dp,0.008_dp,0.018_dp,0.020_dp, &
      0.025_dp,0.020_dp/
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
    npoess%Surface_Type = SURFACE_TYPE
    DO n = 1, N_SURFACE_TYPES
      npoess%Reflectance(:,n) = reflectance(idx,n)
    END DO


    ! Clean up
    DEALLOCATE(idx)
    
  END SUBROUTINE Load_NPOESS_Data

END MODULE IRVISsnowCoeff_Module
