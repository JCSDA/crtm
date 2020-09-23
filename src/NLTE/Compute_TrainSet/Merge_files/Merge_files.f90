!--------------------------------------------------------------------
!  Program to merge LTE, NLTE and TAPE5 profile files into a single
!  netcdf file.
!
!    Written by Yong Han, August 2010
!
PROGRAM Merge_files



  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds       , ONLY: fp
  USE Message_Handler  , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                               Display_Message
  USE DeltaRad_TrainSet_Define,   ONLY: TrainSet_type, &
                               TrainSet_Associated, &
                               TrainSet_Create
  USE DeltaRadTrainSet_netCDF_IO, ONLY: Write_TrainSet_netCDF
  
  ! Disable implicit typing
  IMPLICIT NONE

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Merge_Files'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &


  INTEGER, PARAMETER :: RELEASE_NUMBER = 1
  INTEGER, PARAMETER :: VERSION_NUMBER = 1

  REAL(fp), PARAMETER :: D2R=3.141592653589793238462643_fp/180.0_fp

  INTEGER, PARAMETER :: SL = 128
  CHARACTER(256)     :: fname_nlte, fname_lte, fname_out, fname_t5
  CHARACTER(SL)      :: Sensor_Id
  INTEGER :: WMO_satellite_id 
  INTEGER :: WMO_sensor_id  
  INTEGER :: n_channels, n_Sensor_Angles, n_Sun_Angles, n_Layers, n_Profiles
  INTEGER, PARAMETER :: MAX_N_LEVELS   = 2000
  INTEGER, PARAMETER :: MAX_N_CHANNELS = 20000
  REAL(fP) :: Level_Pressure(MAX_N_LEVELS)
  REAL(fP) :: Level_Temperature(MAX_N_LEVELS)
  REAL(fP) :: Level_CO2(MAX_N_LEVELS)  
  REAL(fP) :: Rad_nlte(20000), Rad_lte(20000)
  INTEGER  :: Channel(20000), Channel_tmp(20000)
  REAL(fP) :: airmass, Zangle, ScanAngle, SunAngle 
  INTEGER  :: Allocate_Status, Error_Status, IO_Status
  INTEGER, PARAMETER  :: fid_rad = 101, fid_t5 = 105
  TYPE(TrainSet_type) :: TrainSet
  CHARACTER(256)  :: ID_Tag, Title
  CHARACTER(1024) :: History, Comment

  WRITE(*, FMT='(/5x,"Enter the TAPE5 filename: ")', ADVANCE='NO')
  READ(*,'(a)') fname_t5
  fname_t5 = ADJUSTL(fname_t5)

  WRITE(*, FMT='(/5x,"Enter the nlte radiance filename: ")', ADVANCE='NO')
  READ(*,'(a)') fname_nlte
  fname_nlte = ADJUSTL(fname_nlte)

  WRITE(*, FMT='(/5x,"Enter the lte radiance filename: ")', ADVANCE='NO')
  READ(*,'(a)') fname_lte
  fname_lte = ADJUSTL(fname_lte)

  WRITE(*, FMT='(/5x,"Enter the airmass [i.g. 1.0]: ")', ADVANCE='NO')
  READ(*,'(f16.8)') airmass
  Zangle = ACOS(1.0_fp/airmass)/D2R

  WRITE(*, FMT='(/5x,"Enter the Sun angle value in degree [i.g. 45.235]: ")', ADVANCE='NO')
  READ(*,'(f16.8)') SunAngle

  WRITE(*, FMT='(/5x,"Enter the output netCDF filename: ")', ADVANCE='NO')
  READ(*,'(a)') fname_out
  fname_out = ADJUSTL(fname_out)

  CALL Get_TAPE5_Data(fname_t5, &
                      n_Layers, &
                      Level_Pressure, &
                      Level_Temperature, &
                      Level_CO2, &
                      ScanAngle)
  CALL Get_RadData( fname_nlte, &
                    n_Channels, Rad_nlte, Channel, &
                    Sensor_Id, WMO_satellite_id, WMO_sensor_id )

  CALL Get_RadData( fname_lte, &
                    n_Channels, Rad_lte, Channel_tmp, &
                   Sensor_Id, WMO_satellite_id, WMO_sensor_id )
  IF(ANY( Channel /= Channel_tmp) )THEN
    CALL Display_Message( PROGRAM_NAME, &                                                              
                          "Error: channel indexes are different between the NLTE and LTE datasets", &  
                          FAILURE)                                                                     
    STOP                                                                                               
  END IF
  
  n_Sensor_Angles = 1
  n_Sun_Angles = 1
  n_Profiles = 1

  CALL TrainSet_Create(TrainSet,        &
                       n_Channels,      &
                       n_Sensor_Angles, &
                       n_Sun_Angles,    &
                       n_Layers,        &
                       n_Profiles )
                       
  IF( .NOT. TrainSet_Associated( TrainSet ) )THEN
    CALL Display_Message( PROGRAM_NAME, &                                                              
                          "Error: allocating memory for TrainSet", &  
                          FAILURE)                                                                     
    STOP                                                                                               
  END IF
 
  TrainSet%Radiance_nlte(:, 1, 1, 1) = Rad_nlte(1:n_Channels)                    
  TrainSet%Radiance_lte(:, 1, 1)     = Rad_lte(1:n_Channels)                    
  TrainSet%Sensor_Angle(1)           = Zangle
  TrainSet%Sun_Angle(1)              = SunAngle 
  TrainSet%Level_Pressure(:, 1)      = Level_Pressure(0:n_Layers)                 
  TrainSet%Level_Temperature(:, 1)   = Level_Temperature(0:n_Layers)                 
  TrainSet%Level_CO2(:, 1)           = Level_CO2(0:n_Layers)  
  TrainSet%Channel                   = Channel(1:n_Channels)               
  TrainSet%Release                   = RELEASE_NUMBER               
  TrainSet%Version                   = VERSION_NUMBER               
  TrainSet%Sensor_ID                 = TRIM(Sensor_ID)               
  TrainSet%WMO_Satellite_ID          = WMO_Satellite_ID               
  TrainSet%WMO_Sensor_ID             = WMO_Sensor_ID    
            
  ID_Tag    = "UMBC_NLTE"                                                                      
  Title     = "Delta NLTE radiance training set"                                               
  History   = "Created by Y. Han, June, 2010"                                                  
  Comment   = "NLTE and LTE radiance spectra are computed using LBLRTM-11.7"//&                
       " with the NLTE capability. The profiles are those of the UMBC profiles projected on the"//&
       " levels of the vibrational temperature profiles from Manuel Lopez-Puertas."

  ! Write out a netCDF file
  Error_Status = Write_TrainSet_netCDF(      &
                 fname_out,                  &
                 TrainSet%Radiance_nlte,     &
                 TrainSet%Radiance_lte,      &
                 TrainSet%Sensor_Angle,      &
                 TrainSet%Sun_Angle,         &
                 TrainSet%Level_Pressure,    &
                 TrainSet%Level_Temperature, &
                 TrainSet%Level_CO2,         &
                 TrainSet%Channel,           &
                 Release          = TrainSet%Release,           &
                 Version          = TrainSet%Version,           &
                 Sensor_ID        = TrainSet%Sensor_ID,         &
                 WMO_Satellite_ID = TrainSet%WMO_Satellite_ID,  &
                 WMO_Sensor_ID    = TrainSet%WMO_Sensor_ID,     &
                 ID_Tag           = ID_Tag,           &
                 Title            = Title,            &
                 History          = History,          &
                 Comment          = Comment)
   IF( Error_Status /= SUCCESS )THEN
    CALL Display_Message( PROGRAM_NAME, &                                                              
                          "Error: writing data into a netCDF file", &  
                          FAILURE)                                                                     
    STOP                                                                                               
  END IF
   
                 
CONTAINS

   ! Subroutine to get atmospheric profile and zenith angle from TAPE5
   SUBROUTINE Get_TAPE5_Data(fname_t5,       &
                             n_Layers,       &
                             Level_Pressure, &                                  
                             Level_Temperature, &                               
                             Level_CO2, &                                       
                             ScanAngle)  
     CHARACTER(*),INTENT(IN)  :: fname_t5                                         
     INTEGER,     INTENT(OUT) :: n_Layers                                          
     REAL(fp),    INTENT(OUT) :: Level_Pressure(:)
     REAL(fp),    INTENT(OUT) :: Level_Temperature(:)
     REAL(fp),    INTENT(OUT) :: Level_CO2(:)
     REAL(fp),    INTENT(OUT) :: ScanAngle                                        
     ! Local                                                                    
     INTEGER :: IO_Status                                                       
     INTEGER :: n_levels                                             
     INTEGER :: i, m, k, kk                                                        
     REAL(fp) :: dummy                                                          
                                                                                
     ! Open the file                                                            
     OPEN( fid_t5, FILE   = TRIM(fname_t5),  &                                 
                   STATUS = 'OLD', &                                            
                   ACTION = 'READ', &                                           
                   IOSTAT = IO_Status )                                         
     IF ( IO_Status /= 0 ) THEN                                                 
       WRITE( Message, '( "Error opening ", a, ". IOSTAT = ", i5 )' ) &         
                       TRIM( fname_t5 ), IO_Status                              
       CALL Display_Message( PROGRAM_NAME, &                                    
                             TRIM( Message ), &                                 
                             Error_Status)                                      
       STOP                                                                     
     END IF                                                                     
                                                                                
     ! skip line                                                                
     DO i = 1, 5                                                                
       READ(fid_t5, *)                                                          
     END DO                                                                     
                                                                                
     ! get angle                                                                
     READ(fid_t5, *) dummy, dummy, ScanAngle                                       
     ScanAngle = 180.0_fp - ScanAngle                                                 
                                                                                
     ! skip more lines                                                          
     DO i = 7, 17                                                               
       READ(fid_t5, *)                                                          
     END DO                                                                     
                                                                                
     READ(fid_t5, *) n_levels                                                   
     IF(n_Levels < 1 .OR. n_Levels > Max_N_LEVELS)THEN                                  
       WRITE( Message,'("Error: n_levels < 1 or n_levels > ",i0)') Max_N_LEVELS
       CALL Display_Message( PROGRAM_NAME, &                                    
                             TRIM(Message), &
                             FAILURE)                                           
       STOP                                                                     
     END IF                                                                     

     n_Layers = n_levels-1                                                      
                                                                                
     DO k = 0, n_layers                                                         
       kk = n_layers - k                                                        
       READ(fid_t5, '(3f10.4)')dummy, Level_Pressure(kk), Level_Temperature(kk) 
       READ(fid_t5, '(2e15.8)')dummy, Level_CO2(kk)                             
     END DO                                                                     

     CLOSE(fid_t5)                                                              
                                                                                
   END SUBROUTINE Get_TAPE5_Data                                                           
      
  ! Subroutine to get LBL radiance data
  SUBROUTINE Get_RadData(fname,            &
                         n_Channels,       &
                         Rad,              &
                         Channel,          &
                         Sensor_id,        &
                         WMO_satellite_id, &
                         WMO_sensor_id )                         
    CHARACTER(*), INTENT(IN)  :: fname
    INTEGER,      INTENT(OUT) :: n_Channels
    REAL(fp),     INTENT(OUT) :: Rad(:)
    INTEGER,      INTENT(OUT) :: Channel(:) 
    CHARACTER(*), INTENT(OUT) :: Sensor_id
    INTEGER,      INTENT(OUT) :: WMO_satellite_id  
    INTEGER,      INTENT(OUT) :: WMO_sensor_id  
      
    ! Local
    INTEGER :: SL_data 
               
    ! Open the file
    OPEN( fid_rad, FILE   = TRIM( fname ), &
                  STATUS = 'OLD', &
                  ACTION = 'READ', &
                  ACCESS = 'SEQUENTIAL', &
                  FORM   = 'UNFORMATTED', &
                  IOSTAT = IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error opening ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( fname_nlte ), IO_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            IO_Status)
      STOP
    END IF

    READ(fid_rad)SL_data
    IF(SL_data /= SL)THEN
      CALL Display_Message( PROGRAM_NAME, &
                            "Error: LEN(Sensor_id) /= the string length in the binary data file", &
                            Error_Status)
      STOP
    END IF
    
    READ(fid_rad)n_channels
    IF(n_channels < 1 .OR. n_channels > MAX_N_CHANNELS)THEN
      WRITE( Message,'("Error: n_channels < 1 or n_channels > ",i0)') MAX_N_CHANNELS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            FAILURE )
      STOP
    END IF
      
    READ(fid_rad)Sensor_id, wmo_satellite_id, wmo_sensor_id
    READ(fid_rad)Channel(1:n_Channels)
    READ(fid_rad)Rad(1:n_Channels)
    
    CLOSE(fid_rad)

  END SUBROUTINE Get_RadData     

END PROGRAM Merge_files
