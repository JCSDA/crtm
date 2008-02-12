!
! Create_CloudSat_Files
!
! Program to create a CRTM Surface datafile matched to CloudSat profile
! locations. The CloudSat profiles are replicated with the first set matched
! to a land surface, and the second set matched to a sea surface.
! 
! CREATION HISTORY:
!       Written by:     David Neil Groff, SAIC 30-May-2007
!                       david.groff@noaa.gov
!

PROGRAM Create_CloudSat_Files

  
  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds               
  USE Message_Handler           , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                        Display_Message, Program_Message
  USE CRTM_Atmosphere_Binary_IO 
  USE CRTM_Atmosphere_Define
  USE CRTM_Surface_Binary_IO
  USE CRTM_Surface_Define
  USE netCDF
  USE netCDF_Utility
  USE netCDF_Variable_Utility
  USE CRTM_Parameters
  
  
  IMPLICIT NONE
  
  ! Parameters
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Create_CloudSat_Files'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: $'
  CHARACTER(*), PARAMETER :: Atmosphere_Filename = 'CloudSat_InputData.bin'
  CHARACTER(*), PARAMETER :: Surface_Filename = 'gdas2.t18z.sfluxgrbf00.nc'
  
  INTEGER, PARAMETER :: N_Dimensions = 2
  INTEGER, PARAMETER :: N_Lats = 94
  INTEGER, PARAMETER :: N_Lons = 192
  INTEGER, PARAMETER :: N_Profiles = 16
  INTEGER, PARAMETER :: N_Layers = 4
  ! Two surface types for each profile (sea and land)
  INTEGER, PARAMETER :: n_Surface_Types = 2
  
  ! latitude boundaries for the climatologies
  INTEGER, PARAMETER :: SUBARCTIC_N_BOUNDARY = 70
  INTEGER, PARAMETER :: SUBARCTIC_S_BOUNDARY = 50
  INTEGER, PARAMETER :: MIDLATITUDE_N_BOUNDARY = 45
  INTEGER, PARAMETER :: MIDLATITUDE_S_BOUNDARY = 30
  INTEGER, PARAMETER :: TROPICAL_S_BOUNDARY = -20
  INTEGER, PARAMETER :: TROPICAL_N_BOUNDARY = 20
  
  ! GDAS vegetation types
  INTEGER, PARAMETER :: WATER = 0
  INTEGER, PARAMETER :: BROADLEAF_EVERGREEN_TREES = 1
  INTEGER, PARAMETER :: MIXED_FOREST = 3
  INTEGER, PARAMETER :: BARE_SOIL = 11
  
  ! ICE flags
  INTEGER, PARAMETER :: NOT_ICE = 0
  INTEGER, PARAMETER :: ICE = 1
  
  ! Parameters needed to read in netcdf variables
  INTEGER, PARAMETER, DIMENSION(N_Dimensions) :: Start = (/1,1/)
  INTEGER, PARAMETER, DIMENSION(N_Dimensions) :: Stride = (/1,1/)
  INTEGER, PARAMETER, DIMENSION(N_Dimensions) :: Count = (/N_Lons,N_Lats/)
  ! Parameters needed to read the 3 dimensional soil data
  INTEGER, PARAMETER, DIMENSION(N_Dimensions+1) :: Start2 = (/1,1,1/)
  INTEGER, PARAMETER, DIMENSION(N_Dimensions+1) :: Stride2 = (/1,1,1/)
  INTEGER, PARAMETER, DIMENSION(N_Dimensions+1) :: Count2 = (/N_Lons,N_Lats,n_Layers/)
  
  ! Declare character names of variables
  CHARACTER(*), PARAMETER :: VEG_TYPE  = 'VGTYP_98_SFC_10'
  CHARACTER(*), PARAMETER :: SOIL_LIQ  = 'SOILL_98_DBLY_10'
  CHARACTER(*), PARAMETER :: SNOW_COV  = 'SNOWC_98_SFC_ave'
  CHARACTER(*), PARAMETER :: TMP_SFC   = 'TMP_98_SFC_10'
  CHARACTER(*), PARAMETER :: TMP_SOIL  = 'TMP_98_DBLY_10'
  CHARACTER(*), PARAMETER :: LON       = 'lon_98'
  CHARACTER(*), PARAMETER :: LAT       = 'lat_98'
  CHARACTER(*), PARAMETER :: CAN_WAT   = 'C_WAT_98_SFC_10'
  CHARACTER(*), PARAMETER :: SOIL_LF   = 'SOILW_98_DBLY_10'
  CHARACTER(*), PARAMETER :: ICE_TK    = 'ICETK_98_SFC_10'
  CHARACTER(*), PARAMETER :: ICE_COV   = 'ICE_C_98_SFC_10'
  CHARACTER(*), PARAMETER :: VEG_COV   = 'VEG_98_SFC_10'
  CHARACTER(*), PARAMETER :: SOIL_MOI  = 'SOIL_M_98_DBLY_10'
  CHARACTER(*), PARAMETER :: LAND_MASK = 'LAND_98_SFC_10'
  CHARACTER(*), PARAMETER :: SNO_DEP   = 'SNO_D_98_SFC_10'
  CHARACTER(*), PARAMETER :: U         = 'U_GRD_98_HTGL_10'
  CHARACTER(*), PARAMETER :: V         = 'V_GRD_98_HTGL_10'
  CHARACTER(*), PARAMETER :: SOIL_TYP  = 'SOTYP_98_SFC_10'
  
  ! Variables
  ! Logical variables used to determine surface type(land or sea)
  LOGICAL :: Sea_Flag
  LOGICAL :: Land_Flag
  
  ! netCDF file ID
  INTEGER :: NC_FileID
  
  ! Error_Status/Error_Counter are used to control error messaging
  INTEGER :: Error_Status, Error_Counter
  
  ! i corresponds to lons, j corresponds to lats, m represents profile #
  INTEGER :: i, j, m
  
  ! land/sea index 
  INTEGER :: S_type
  
  ! Atmosphere Profile
  TYPE(CRTM_Atmosphere_type), DIMENSION(N_Profiles) :: Atmosphere
  
  ! The 16 profiles are duplicated. One set is for land and one set is for sea.
  TYPE(CRTM_Atmosphere_type), DIMENSION(N_Profiles*n_Surface_Types) :: Atmosphere_Expanded
  
  ! The number of surface types matches the number of atmospheric profiles.
  ! Each Surface filled corresponds to a particular atmospheric profile
  TYPE(CRTM_Surface_type), DIMENSION(N_Profiles*n_Surface_Types) :: Surface
  
  ! The variables obtained from reading the netcdf data
  REAL, DIMENSION(N_Lons, N_Lats) :: Vegetation_Cover, Snow_Cover, &
                                     Canopy_Water_Content, Ice_Thickness, &
                                     Ice_Mask, Soil_Moisture_Content, &
                                     Snow_Depth, Surface_Temperature, &
                                     U_Wind, V_Wind
  REAL, DIMENSION(N_Lons, N_Lats, n_Layers) :: Soil_Temperature, Liq_Vol_SM, &
                                               Vol_SM
  ! Lat/Lon information 
  REAL, DIMENSION(N_Lons) :: Longitude
  REAL, DIMENSION(N_Lats) :: Latitude
  
  INTEGER, DIMENSION(N_Lons, N_Lats):: Vegetation_Type, Land_Sea_Mask, Soil_Type
  
  
  ! Program header
  ! --------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to create a CRTM Surface datafile matched to CloudSat profile '//&
                        'locations. The CloudSat profiles are replicated with the first set matched '//&
                        'to a land surface, and the second set matched to a sea surface.', &
                        '$Revision: $' )
  
  ! Initialize the error counter for reading the variables.  
  Error_Counter = 0
  
  ! Read the atmospheric profile file
  WRITE( *, '( /5x, "Reading the atmosphere structure file..." )' )
  Error_Status = CRTM_Read_Atmosphere_Binary(  Atmosphere_Filename,  &
                                               Atmosphere  )
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error reading Atmosphere structure file '//&
                           Atmosphere_Filename, & 
                           Error_Status )
   STOP
  END IF
  
  ! Duplicate the Atmosphere Structure.
  ! because each profile will be matched
  ! to a particular land and sea surface
  ! that is consistent with the profile
  ! defined climatology
  DO S_type = 1, n_Surface_Types
    DO m = 1, N_Profiles
      IF(S_Type==2) THEN
        Atmosphere_Expanded(m+N_PROFILES) = Atmosphere(m)
      ELSE
        Atmosphere_Expanded(m) = Atmosphere(m)
      END IF
    END DO
  END DO
    
  
  ! Open the netcdf file for reading
  WRITE( *, '( /5x, "Opening the netcdf surface data ...")' )
  Error_Status = Open_netCDF( Surface_Filename,    &  ! Input
                              NC_FileID,           &  ! Output
                              Mode='READ'          )
                              
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error opening surface file', &
                          Error_Status )
  END IF
  
  ! Get data relevant to filling the CRTM surface structure
  
  ! non-frozen soil moisture                                 
  Error_Status = Get_netCDF_Variable( NC_FileID,           &
                                      SOIL_LIQ,            &
                                      Liq_Vol_SM,          &
                                      Start = Start2,      &
                                      Count = Count2,      &
                                      Stride = Stride2     )

  IF(Error_Status /= SUCCESS) THEN
    Error_Counter = Error_Counter + 1
  END IF
  
  ! fractional snow coverage                                   
  Error_Status = Get_netCDF_Variable( NC_FileID,          &
                                      SNOW_COV,           &
                                      Snow_Cover,         &
                                      Start = Start,      &
                                      Count = Count,      &
                                      Stride = Stride     )

  IF(Error_Status /= SUCCESS) THEN
    Error_Counter = Error_Counter + 1
  END IF
                                      
  ! Vegetation type (0:13)                                  
  Error_Status = Get_netCDF_Variable( NC_FileID,           &
                                      VEG_TYPE,            &
                                      Vegetation_Type,     &
                                      Start = Start,       &
                                      Count = Count,       &
                                      Stride = Stride      )
  
  IF(Error_Status /= SUCCESS) THEN
     Error_Counter = Error_Counter + 1
  END IF
                                         
  ! Surface/skin temperature                                   
  Error_Status = Get_netCDF_Variable( NC_FileID,               &
                                      TMP_SFC,                 &
                                      Surface_Temperature,     &
                                      Start = Start,           &
                                      Count = Count,           &
                                      Stride = Stride          ) 
                                      
  IF(Error_Status /= SUCCESS) THEN
    Error_Counter = Error_Counter + 1
  END IF
  
  ! Soil Temperature
  Error_Status = Get_netCDF_Variable( NC_FileID,         &
                                      TMP_SOIL,          &
                                      Soil_Temperature,  &
                                      Start = Start2,    &
                                      Count = Count2,    &
                                      Stride = Stride2   )
                                      
  IF(Error_Status /= SUCCESS) THEN
    Error_Counter = Error_Counter + 1
  END IF       
  
  ! longitude                                   
  Error_Status = Get_netCDF_Variable( NC_FileID,       &
                                      LON,             &
                                      Longitude        )
                                              
  IF(Error_Status /= SUCCESS) THEN
     Error_Counter = Error_Counter + 1
  END IF
                                      
  ! latitude                                    
  Error_Status = Get_netCDF_Variable( NC_FileID,       &
                                      LAT,             &
                                      Latitude         )
                                      
  IF(Error_Status /= SUCCESS) THEN
     Error_Counter = Error_Counter + 1
  END IF                                
                                      
  ! Canopy water content                                    
  Error_Status = Get_netCDF_Variable( NC_FileID,            &
                                      CAN_WAT,              &
                                      Canopy_Water_Content, &
                                      Start = Start,        &
                                      Count = Count,        &
                                      Stride = Stride       )

  IF(Error_Status /= SUCCESS) THEN
    Error_Counter = Error_Counter + 1
  END IF
  
  ! Soil moisture Liquid + Frozen                                    
  Error_Status = Get_netCDF_Variable( NC_FileID,              &
                                      SOIL_LF,                &
                                      Vol_SM,                 &
                                      Start = Start2,         &
                                      Count = Count2,         &
                                      Stride = Stride2        )

  IF(Error_Status /= SUCCESS) THEN
    Error_Counter = Error_Counter + 1
  END IF
  
  ! Ice thickness                                    
  Error_Status = Get_netCDF_Variable( NC_FileID,             &
                                      ICE_TK,                &                       
                                      Ice_Thickness,         &
                                      Start = Start,         &
                                      Count = Count,         &
                                      Stride = Stride        )

  IF(Error_Status /= SUCCESS) THEN
    Error_Counter = Error_Counter + 1
  END IF
  
  ! Ice Mask                                    
  Error_Status = Get_netCDF_Variable( NC_FileID,          &
                                      ICE_COV,            &
                                      Ice_Mask,           &
                                      Start = Start,      &
                                      Count = Count,      &
                                      Stride = Stride     )

  IF(Error_Status /= SUCCESS) THEN
    Error_Counter = Error_Counter + 1
  END IF
  
  ! Vegetation cover                                    
  Error_Status = Get_netCDF_Variable( NC_FileID,             &
                                      VEG_COV,               &
                                      Vegetation_Cover,      &
                                      Start = Start,         &
                                      Count = Count,         &
                                      Stride = Stride        )

  IF(Error_Status /= SUCCESS) THEN
    Error_Counter = Error_Counter + 1
  END IF
  
  ! Soil moisture                                    
  Error_Status = Get_netCDF_Variable( NC_FileID,             &
                                      SOIL_MOI,              &
                                      Soil_Moisture_Content, &
                                      Start = Start,         &
                                      Count = Count,         &
                                      Stride = Stride        )

  IF(Error_Status /= SUCCESS) THEN
    Error_Counter = Error_Counter + 1
  END IF
  
  ! Land/Sea Mask                                   
  Error_Status = Get_netCDF_Variable( NC_FileID,             &
                                      LAND_MASK,             &
                                      Land_Sea_mask,         &
                                      Start = Start,         &
                                      Count = Count,         &
                                      Stride = Stride        ) 

  IF(Error_Status /= SUCCESS) THEN
    Error_Counter = Error_Counter + 1
  END IF
  
  ! Snow depth                                    
  Error_Status = Get_netCDF_Variable( NC_FileID,            &
                                      SNO_DEP,              &
                                      Snow_Depth,           &
                                      Start = Start,        &
                                      Count = Count,        &
                                      Stride = Stride       )                                                        
  
  IF(Error_Status /= SUCCESS) THEN
    Error_Counter = Error_Counter + 1
  END IF
  
  ! u component of wind 
  Error_Status = Get_netCDF_Variable( NC_FileID,             &
                                      U,                     &
                                      U_Wind,                &
                                      Start = Start,         &
                                      Count = Count,         &
                                      Stride = Stride        )                                                        
  
  IF(Error_Status /= SUCCESS) THEN
    Error_Counter = Error_Counter + 1
  END IF
  
  ! v component of wind 
  Error_Status = Get_netCDF_Variable( NC_FileID,           &
                                      V,                   &
                                      V_Wind,              &
                                      Start = Start,       &
                                      Count = Count,       &
                                      Stride = Stride      )
   
  IF(Error_Status /= SUCCESS) THEN
    Error_Counter = Error_Counter + 1
  END IF
        
  ! Soil Type
  Error_Status = Get_netCDF_Variable( NC_FileID,      &
                                      SOIL_Typ,       &
                                      Soil_Type,      &
                                      Start = Start,  &
                                      Count = Count,  &
                                      Stride = Stride )
                                      
  IF(Error_Counter>0) THEN
    CALL Display_Message( PROGRAM_NAME,              &
                          'Error reading variables', &
                          Error_Counter              )
  END IF                        
 
  ! Close the netCDF file
  WRITE( *, '( /5x, "Closing the netcdf surface data ...")' )
  Error_Status = Close_netCDF( NC_FileID )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME,                 &
                          'Error closing surface file', &
                          Error_Status                  )
  END IF 
  
  ! Assign GDAS data to CRTM Surface structure
  
  ! Loop over N_Profiles*n_Surface_Types(land + sea)
  Profile_Loop: DO m = 1, N_Profiles*n_Surface_Types
    ! Set Sea_Flag. The Atmosphere was duplicated
    ! to provide two sets of Atmospheres.
    ! The first set is for land surfaces and
    ! the second set is for water surfaces.
    IF(m > N_Profiles) THEN
      Sea_Flag  = .TRUE.
      Land_Flag = .FALSE.
    ELSE 
      Sea_Flag  = .FALSE.
      Land_Flag = .TRUE.
    END IF
    Longitude_Loop: DO j = 1, N_Lats
      Latitude_Loop: DO i = 1, N_Lons
        ! land surface for a tropical atmosphere
        IF(Vegetation_Type(i,j)==BROADLEAF_EVERGREEN_TREES .AND. &
           Atmosphere_Expanded(m)%Climatology==TROPICAL .AND. &
           Land_Flag .AND. &
           Latitude(j)>TROPICAL_S_BOUNDARY .AND. &
           Latitude(j)<TROPICAL_N_BOUNDARY) THEN
          Surface(m)%Land_Coverage = ONE
          Surface(m)%Wind_Speed = SQRT(U_Wind(i,j)**2 + V_Wind(i,j)**2)
          Surface(m)%Land_Type = BROADLEAF_FOREST
          Surface(m)%Land_Temperature =  Surface_Temperature(i,j)
          ! Bulk density sandy loam approximately 1.4 g/cm^3
          Surface(m)%Soil_Moisture_Content = Liq_Vol_SM(i,j,1)*1.40_fp
          Surface(m)%Canopy_Water_Content = Canopy_Water_Content(i,j)/10.0_fp
          Surface(m)%Vegetation_Fraction = Vegetation_Cover(i,j)/100.0_fp
          Surface(m)%Soil_Temperature = Soil_Temperature(i,j,1)
          EXIT Longitude_Loop
        END IF
        ! water surface for a tropical atmosphere
        IF(Land_Sea_mask(i,j)==WATER .AND. &
           Atmosphere_Expanded(m)%Climatology==TROPICAL .AND. Sea_Flag .AND. &
           Latitude(j)>TROPICAL_S_BOUNDARY .AND. Latitude(j)<TROPICAL_N_BOUNDARY) THEN
          Surface(m)%Water_Coverage = ONE 
          Surface(m)%Wind_Speed = SQRT(U_Wind(i,j)**2 + V_Wind(i,j)**2)
          IF(U_Wind(i,j)<ZERO .AND. V_Wind(i,j)<ZERO) THEN
            Surface(m)%Wind_Direction = (1/DEGREES_TO_RADIANS) * ATAN(U_Wind(i,j)/V_Wind(i,j))
          END IF
          IF(U_Wind(i,j)<ZERO .AND. V_Wind(i,j)>ZERO) THEN
            Surface(m)%Wind_Direction = 180.0_fp - ((1/DEGREES_TO_RADIANS) * ATAN(ABS(U_Wind(i,j))/V_Wind(i,j)))
          END IF
          IF(U_Wind(i,j)>ZERO .AND. V_Wind(i,j)>ZERO) THEN
            Surface(m)%Wind_Direction = 180.0_fp + ((1/DEGREES_TO_RADIANS) * ATAN(U_Wind(i,j)/V_Wind(i,j)))
          END IF
          IF(U_Wind(i,j)>ZERO .AND. V_Wind(i,j)<ZERO) THEN
            Surface(m)%Wind_Direction = 360.0_fp - ((1/DEGREES_TO_RADIANS) * ATAN(U_Wind(i,j)/ABS(V_Wind(i,j))))
          END IF
          Surface(m)%Water_Temperature = Surface_Temperature(i,j)
          EXIT Longitude_Loop
        END IF
        ! land surface for midlatitude summer atmosphere
        IF(Vegetation_Type(i,j)==BARE_SOIL .AND. &
        Atmosphere_Expanded(m)%Climatology==MIDLATITUDE_SUMMER  .AND. Land_Flag .AND. &
        Latitude(j)>MIDLATITUDE_S_BOUNDARY .AND. Latitude(j)<MIDLATITUDE_N_BOUNDARY) THEN
          Surface(m)%Land_Coverage = ONE
          Surface(m)%Wind_Speed = SQRT(U_Wind(i,j)**2 + V_Wind(i,j)**2)
          Surface(m)%Land_Type = IRRIGATED_LOW_VEGETATION
          Surface(m)%Land_Temperature = Surface_Temperature(i,j)
          ! Bulk density of loam approximately = 1.2 g/cm^3
          Surface(m)%Soil_Moisture_Content = Liq_Vol_SM(i,j,1)*1.20_fp
          Surface(m)%Canopy_Water_Content = Canopy_Water_Content(i,j)/10.0_fp
          Surface(m)%Vegetation_Fraction = Vegetation_Cover(i,j)/100.0_fp
          Surface(m)%Soil_Temperature = Soil_Temperature(i,j,1)          
          EXIT Longitude_Loop
        END IF
        ! Water surface for midlatitude summer atmosphere
        IF(Land_Sea_Mask(i,j)==WATER .AND. &
        Atmosphere_Expanded(m)%Climatology==MIDLATITUDE_SUMMER .AND. Sea_Flag .AND. &
        Latitude(j)>MIDLATITUDE_S_BOUNDARY .AND. Latitude(j)<MIDLATITUDE_N_BOUNDARY) THEN
          Surface(m)%Water_Coverage = ONE
          Surface(m)%Wind_Speed = SQRT(U_Wind(i,j)**2 + V_Wind(i,j)**2)
          IF(U_Wind(i,j)<ZERO .AND. V_Wind(i,j)<ZERO) THEN
            Surface(m)%Wind_Direction = (1/DEGREES_TO_RADIANS) * ATAN(U_Wind(i,j)/V_Wind(i,j))
          END IF
          IF(U_Wind(i,j)<ZERO .AND. V_Wind(i,j)>ZERO) THEN
            Surface(m)%Wind_Direction = 180.0_fp - ((1/DEGREES_TO_RADIANS) * ATAN(ABS(U_Wind(i,j))/V_Wind(i,j)))
          END IF
          IF(U_Wind(i,j)>ZERO .AND. V_Wind(i,j)>ZERO) THEN
            Surface(m)%Wind_Direction = 180.0_fp + ((1/DEGREES_TO_RADIANS) * ATAN(U_Wind(i,j)/V_Wind(i,j)))
          END IF
          IF(U_Wind(i,j)>ZERO .AND. V_Wind(i,j)<ZERO) THEN
            Surface(m)%Wind_Direction = 360.0_fp - ((1/DEGREES_TO_RADIANS) * ATAN(U_Wind(i,j)/ABS(V_Wind(i,j))))
          END IF
          Surface(m)%Water_Temperature = Surface_Temperature(i,j)
          EXIT Longitude_Loop
        END IF
        ! Land surface for midlatitude winter atmosphere  
        IF(Vegetation_Type(i,j)==BARE_SOIL .AND. &
        Atmosphere_Expanded(m)%Climatology==MIDLATITUDE_WINTER .AND. Land_Flag .AND. &
        Latitude(j)>MIDLATITUDE_S_BOUNDARY .AND. Latitude(j)<MIDLATITUDE_N_BOUNDARY) THEN
          Surface(m)%Land_Coverage = ONE
          Surface(m)%Wind_Speed = SQRT(U_Wind(i,j)**2 + V_Wind(i,j)**2)
          Surface(m)%Land_Type = IRRIGATED_LOW_VEGETATION
          Surface(m)%Land_Temperature = Surface_Temperature(i,j)
          ! Bulk density of loam approximately 1.20_fp
          Surface(m)%Soil_Moisture_Content = Liq_Vol_SM(i,j,1)*1.20_fp
          Surface(m)%Canopy_Water_Content = Canopy_Water_Content(i,j)/10.0_fp
          Surface(m)%Vegetation_Fraction = Vegetation_Cover(i,j)/100.0_fp
          Surface(m)%Soil_Temperature = Soil_Temperature(i,j,1)
          EXIT Longitude_Loop
        END IF
        ! Water surface for midlatitude winter atmosphere        
        IF(Land_Sea_mask(i,j)==WATER .AND. &
        Atmosphere_Expanded(m)%Climatology==MIDLATITUDE_WINTER .AND. Sea_Flag .AND. &
        Latitude(j)>MIDLATITUDE_S_BOUNDARY .AND. Latitude(j)<MIDLATITUDE_N_BOUNDARY) THEN
          Surface(m)%Water_Coverage = ONE
          Surface(m)%Wind_Speed = SQRT(U_Wind(i,j)**2 + V_Wind(i,j)**2)
          IF(U_Wind(i,j)<ZERO .AND. V_Wind(i,j)<ZERO) THEN
            Surface(m)%Wind_Direction = (1/DEGREES_TO_RADIANS) * ATAN(U_Wind(i,j)/V_Wind(i,j))
          END IF
          IF(U_Wind(i,j)<ZERO .AND. V_Wind(i,j)>ZERO) THEN
            Surface(m)%Wind_Direction = 180.0_fp - ((1/DEGREES_TO_RADIANS) * ATAN(ABS(U_Wind(i,j))/V_Wind(i,j)))
          END IF
          IF(U_Wind(i,j)>ZERO .AND. V_Wind(i,j)>ZERO) THEN
            Surface(m)%Wind_Direction = 180.0_fp + ((1/DEGREES_TO_RADIANS) * ATAN(U_Wind(i,j)/V_Wind(i,j)))
          END IF
          IF(U_Wind(i,j)>ZERO .AND. V_Wind(i,j)<ZERO) THEN
            Surface(m)%Wind_Direction = 360.0_fp - ((1/DEGREES_TO_RADIANS) * ATAN(U_Wind(i,j)/ABS(V_Wind(i,j))))
          END IF
          Surface(m)%Water_Temperature = Surface_Temperature(i,j)
          EXIT Longitude_Loop
        END IF
        ! Land surface for subarctic summer atmosphere
        IF(Ice_Mask(i,j)==NOT_ICE .AND. Snow_Cover(i,j)==ZERO .AND. & 
        Vegetation_Type(i,j)==MIXED_FOREST .AND. &
        Atmosphere_Expanded(m)%Climatology==SUBARCTIC_SUMMER .AND. Land_Flag .AND. &
        Latitude(j)>MIDLATITUDE_S_BOUNDARY .AND. Latitude(j)<MIDLATITUDE_N_BOUNDARY) THEN
          Surface(m)%Land_Coverage = ONE
          Surface(m)%Wind_Speed = SQRT(U_Wind(i,j)**2 + V_Wind(i,j)**2)
          Surface(m)%Land_Type = BROADLEAF_PINE_FOREST
          Surface(m)%Land_Temperature = Surface_Temperature(i,j)
          ! Bulk Density of loam approximately 1.2
          Surface(m)%Soil_Moisture_Content = Liq_Vol_SM(i,j,1)*1.20_fp
          Surface(m)%Canopy_Water_Content = Canopy_Water_Content(i,j)/10.0_fp
          Surface(m)%Vegetation_Fraction = Vegetation_Cover(i,j)/100.0_fp
          Surface(m)%Soil_Temperature = Soil_Temperature(i,j,1)
          EXIT Longitude_Loop
        END IF
        ! Water surface for subarctic summer atmosphere
        IF(Ice_Mask(i,j)==NOT_ICE .AND. Land_Sea_mask(i,j)==WATER .AND. &
        Atmosphere_Expanded(m)%Climatology==SUBARCTIC_SUMMER .AND. Sea_Flag .AND. &
        Latitude(j)>SUBARCTIC_S_BOUNDARY .AND. Latitude(j)<SUBARCTIC_N_BOUNDARY) THEN
          Surface(m)%Water_Coverage = ONE
          Surface(m)%Wind_Speed = SQRT(U_Wind(i,j)**2 + V_Wind(i,j)**2)
          IF(U_Wind(i,j)<ZERO .AND. V_Wind(i,j)<ZERO) THEN
            Surface(m)%Wind_Direction = (1/DEGREES_TO_RADIANS) * ATAN(U_Wind(i,j)/V_Wind(i,j))
          END IF
          IF(U_Wind(i,j)<ZERO .AND. V_Wind(i,j)>ZERO) THEN
            Surface(m)%Wind_Direction = 180.0_fp - ((1/DEGREES_TO_RADIANS) * ATAN(ABS(U_Wind(i,j))/V_Wind(i,j)))
          END IF
          IF(U_Wind(i,j)>ZERO .AND. V_Wind(i,j)>ZERO) THEN
            Surface(m)%Wind_Direction = 180.0_fp + ((1/DEGREES_TO_RADIANS) * ATAN(U_Wind(i,j)/V_Wind(i,j)))
          END IF
          IF(U_Wind(i,j)>ZERO .AND. V_Wind(i,j)<ZERO) THEN
            Surface(m)%Wind_Direction = 360.0_fp - ((1/DEGREES_TO_RADIANS) * ATAN(U_Wind(i,j)/ABS(V_Wind(i,j))))
          END IF
          Surface(m)%Water_Temperature = Surface_Temperature(i,j)
          EXIT Longitude_Loop
        END IF 
        ! Snow/Land surface for subarctic winter atmosphere
        IF(Snow_Cover(i,j)>ZERO .AND. Vegetation_Type(i,j)==MIXED_FOREST .AND. &
        Atmosphere_Expanded(m)%Climatology==SUBARCTIC_WINTER .AND. Land_Flag .AND. &
        Latitude(j)>SUBARCTIC_S_BOUNDARY .AND. Latitude(j)<SUBARCTIC_N_BOUNDARY) THEN
          IF(Snow_Depth(i,j)>0.1) THEN
            Surface(m)%Snow_Type = DEEP_SNOW
          ELSE 
            Surface(m)%Snow_Type = SHALLOW_SNOW
          END IF
          Surface(m)%Land_Coverage = ONE - (Snow_Cover(i,j)/100.0_fp)
          Surface(m)%Wind_Speed = SQRT(U_Wind(i,j)**2 + V_Wind(i,j)**2)
          Surface(m)%Snow_Coverage = Snow_Cover(i,j)/100.0_fp
          Surface(m)%Snow_Depth = Snow_Depth(i,j)*1000.0_fp
          Surface(m)%Snow_Temperature = Surface_Temperature(i,j)
          Surface(m)%Land_Temperature = Surface_Temperature(i,j)
          Surface(m)%Land_Type = BROADLEAF_PINE_FOREST
          ! Bulk density of loam approximately 1.2
          Surface(m)%Soil_Moisture_Content = Liq_Vol_SM(i,j,1)*1.20_fp
          Surface(m)%Canopy_Water_Content = Canopy_Water_Content(i,j)/10.0_fp
          Surface(m)%Vegetation_Fraction = Vegetation_Cover(i,j)/100.0_fp
          Surface(m)%Soil_Temperature = Soil_Temperature(i,j,1)
          EXIT Longitude_Loop
        END IF
        ! Sea ice surface for subarctic winter atmosphere
        IF(Ice_Mask(i,j)==ICE .AND. Land_Sea_mask(i,j)==WATER .AND. &
        Atmosphere_Expanded(m)%Climatology==SUBARCTIC_WINTER .AND. Sea_Flag .AND. &
        Latitude(j)>SUBARCTIC_S_BOUNDARY .AND. Latitude(j)<SUBARCTIC_N_BOUNDARY) THEN
          Surface(m)%Ice_Coverage = ONE
          Surface(m)%Wind_Speed = SQRT(U_Wind(i,j)**2 + V_Wind(i,j)**2)
          Surface(m)%Ice_Temperature = Surface_Temperature(i,j)
          Surface(m)%Ice_Thickness = Ice_Thickness(i,j)*1000.0_fp
          EXIT Longitude_Loop
        END IF         
      END DO Latitude_Loop
    END DO Longitude_Loop
  END DO Profile_Loop
       
  Error_Status = CRTM_Write_Surface_Binary( 'CloudSat_Surface.bin'     , & ! Filename
                                             Surface            ) ! Data to be written
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME,                         &
                          'Error writing data to surface file', &
                          Error_Status                          )
  END IF 
  
  Error_Status = CRTM_Write_Atmosphere_Binary( 'CloudSat_Atmosphere.bin', & ! Filename
                                               Atmosphere_Expanded        ) ! Data to be written      
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME,                            &
                          'Error writing data to Atmosphere file', &
                          Error_Status                             )
  END IF 
                
END PROGRAM Create_CloudSat_Files
  
  
  
  
  
  
                    
