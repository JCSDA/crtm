MODULE IRVISiceCoeff_Module

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
  PUBLIC :: IRVISiceCoeff_CreateFile
  

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  CHARACTER(*), PARAMETER :: SURFACE_TYPE_NAME = 'ice'
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

  SUBROUTINE IRVISiceCoeff_CreateFile(visible)
    LOGICAL, OPTIONAL, INTENT(IN) :: visible
    ! Local variables
    TYPE(SEcategory_type) :: npoess

    ! Fill structures with data
    CALL Load_NPOESS_Data(npoess, Visible=visible)
  
    ! Write data to file
    CALL Write_File(npoess, Visible=visible)
  
    ! Cleanup
    CALL SEcategory_Destroy( npoess )

  END SUBROUTINE IRVISiceCoeff_CreateFile
  

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
    INTEGER, PARAMETER :: NEW_ICE = 1
    ! ...The surface types
    INTEGER     , PARAMETER :: N_SURFACE_TYPES = 1
    CHARACTER(*), PARAMETER :: SURFACE_TYPE(N_SURFACE_TYPES) = &
     (/ 'new_ice' /)    
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
    ! ...new ice
    DATA (reflectance(i,NEW_ICE),i=1,N_WAVELENGTHS)/ &
      0.032_dp,0.030_dp,0.029_dp,0.027_dp,0.025_dp,0.025_dp,0.024_dp,0.023_dp, &
      0.023_dp,0.023_dp,0.023_dp,0.023_dp,0.023_dp,0.023_dp,0.023_dp,0.023_dp, &
      0.022_dp,0.022_dp,0.022_dp,0.022_dp,0.022_dp,0.022_dp,0.022_dp,0.022_dp, &
      0.022_dp,0.022_dp,0.022_dp,0.022_dp,0.022_dp,0.022_dp,0.022_dp,0.022_dp, &
      0.021_dp,0.021_dp,0.021_dp,0.021_dp,0.021_dp,0.021_dp,0.021_dp,0.021_dp, &
      0.021_dp,0.021_dp,0.021_dp,0.021_dp,0.020_dp,0.020_dp,0.020_dp,0.020_dp, &
      0.019_dp,0.019_dp,0.018_dp,0.018_dp,0.018_dp,0.010_dp,0.050_dp,0.030_dp, &
      0.020_dp,0.010_dp,0.010_dp,0.010_dp,0.010_dp,0.010_dp,0.010_dp,0.010_dp, &
      0.010_dp,0.010_dp,0.010_dp,0.010_dp,0.010_dp,0.020_dp,0.040_dp,0.050_dp, &
      0.050_dp,0.050_dp/
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

END MODULE IRVISiceCoeff_Module
