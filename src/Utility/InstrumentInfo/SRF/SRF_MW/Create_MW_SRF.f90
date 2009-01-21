PROGRAM Create_MW_SRF
  
  ! Program usage
  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE CRTM_PARAMETERS
  USE SensorInfo_IO
  USE SensorInfo_LinkedList
  USE MW_SensorData_Define
  USE SRF_Define
  USE SRF_netCDF_IO
  USE SensorInfo_Define
  
  ! Parameters
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id:$'
  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Create_MW_SRF'  
  CHARACTER( * ), PARAMETER :: SENSORINFO_FILENAME = 'SensorInfo'
  INTEGER, PARAMETER :: VERSION = 1
  
  ! Variables
  
  ! Structures
  TYPE( MW_SensorData_type ) :: MW_SensorData
  TYPE( SensorInfo_List_type ) :: SensorInfo_List
  TYPE( SRF_type ) :: SRF
  TYPE( SensorInfo_type ) :: SensorInfo
  
  ! Sensor channel array
  INTEGER , DIMENSION(:), ALLOCATABLE :: Sensor_Channel
  
  ! Sensor Id
  CHARACTER(500) :: Sensor_Id
  
  ! Variables for nc file creation
  CHARACTER(500) :: NC_Filename
  CHARACTER(500) :: Title, Comment
  INTEGER :: WMO_Satellite_Id, WMO_Sensor_Id  
  
  ! number of sensors
  INTEGER :: n_Sensors
  
  ! number of data points
  ! This is actually set
  ! in MW_SensorData
  INTEGER :: n_Points
  
  ! Type of Sensor 
  INTEGER :: Sensor_Type
  
  ! number of nchannels
  INTEGER :: n_Channels
  
  ! Loop counters
  INTEGER :: n, l
  
  ! Error/IO status
  INTEGER :: Error_Status, IO_Status
  
  ! number of side band points
  INTEGER, DIMENSION(:), ALLOCATABLE :: n_SBPoints
  
  ! frequency spacing
  REAL(fp) :: df
  
  ! Read SensorInfo file
  Error_Status = Read_SensorInfo( SENSORINFO_FILENAME,  & ! Input
                                  SensorInfo_List       ) ! Output
  IF ( Error_Status /= SUCCESS ) THEN
     CALL Display_Message( PROGRAM_NAME,                     &
                           'Error reading SensorInfo_file',  &
                           Error_Status                      )
    STOP
  END IF
  
  ! Get number of sensors
  n_Sensors = Count_SensorInfo_Nodes( SensorInfo_List )
  
  ! Loop over sensors
  Sensor_Loop: DO n = 1, n_Sensors 
  
    ! Get SensorInfo for sensor n
    Error_Status = GetFrom_SensorInfo_List( SensorInfo_List,     &  ! Input
                                            n,                   &  ! Input
                                            SensorInfo           )  ! Output
    IF ( Error_Status /= SUCCESS ) THEN
       CALL Display_Message( PROGRAM_NAME,                &
                             'Error getting sensor node', &
                             Error_Status                 )
    END IF 
  
    ! Cycle if its not a microwave sensor
    IF ( .NOT. (SensorInfo%Microwave_Flag==1) ) CYCLE Sensor_Loop
    
    ! Cycle if it is an ssmis sensor
    IF ( SensorInfo%Sensor_Name == 'SSMIS') CYCLE Sensor_Loop
    
    ! Name of file to write to for sensor
    Sensor_Id=ADJUSTL(SensorInfo%Sensor_Id)
    Sensor_Id=TRIM(SensorInfo%Sensor_Id)
    NC_Filename= TRIM(Sensor_Id) // '.srf.nc'
    
    ! Allocate for Sensor_Channel allocatable array
    ALLOCATE( Sensor_Channel(SIZE(SensorInfo%Sensor_Channel)),  &
                             STAT = Error_Status                )
    IF ( Error_Status /= SUCCESS ) THEN
       CALL Display_Message( PROGRAM_NAME,                                 &
                             'Error allocating for Sensor_Channel array',  &
                             Error_Status                                  )
    END IF
     
    ! Set variables used to create the nc files    
    Sensor_Channel = SensorInfo%Sensor_Channel
    WMO_Satellite_Id = SensorInfo%WMO_Satellite_ID
    WMO_Sensor_Id = SensorInfo%WMO_Sensor_ID
    Title = TRIM('Microwave channel ' // Sensor_Id // ' Spectral Response Functions')
    Comment = 'The SRF data for this sensor is assumed to be a box car shape.' // &
              ' This is a temporary state until real data becomes available.'
    Sensor_Type = SensorInfo%Microwave_Flag
    n_Channels = SensorInfo%n_Channels

    ! Create srf netcdf files for the MW
    Error_Status = Create_SRF_netCDF( NC_Filename                         ,  & ! Input
                                      Sensor_Type                         ,  & ! Input (MW)
                                      Sensor_Channel                      ,  & ! Input Channel list                               
                                      Version = VERSION                   ,  & ! Optional Input
                                      Sensor_Id = Sensor_Id               ,  & ! Optional Input
                                      WMO_Satellite_Id = WMO_Satellite_Id ,  & ! Optional Input
                                      WMO_Sensor_Id = WMO_Sensor_Id       ,  & ! Optional Input
                                      Title = Title                       ,  & ! Optional Input
                                      History = PROGRAM_RCS_ID            ,  & ! Optional Input
                                      Comment = Comment                      ) ! Optional Input
     
    ! Allocate for MW_SensorData structure
    Error_Status = Allocate_MW_SensorData( n_Channels     ,  &  ! Input
                                           MW_SensorData     )  ! Output
    IF ( Error_Status /= SUCCESS ) THEN
       CALL Display_Message( PROGRAM_NAME,                               &
                             'Error allocating MW_SensorData structure', &
                             Error_Status                                )
    END IF                                      
                                           
    ! Fill the MW_SensorData structure
    Error_Status = Load_MW_SensorData( MW_SensorData,        &
                                       SensorInfo%Sensor_Id  )
    IF ( Error_Status /= SUCCESS ) THEN
       CALL Display_Message( PROGRAM_NAME,                &
                             'Error getting sensor node', &
                             Error_Status                 )
    END IF 
                                        
    ! Loop over channels                                     
    Channel_Loop: DO l = 1, n_Channels
      
      ! Allocate Sideband points array
      ALLOCATE( n_SBPoints(MW_SensorData%n_Sidebands(l)),  &
                    STAT = Error_Status                    )
      IF ( Error_Status /= SUCCESS ) THEN
         CALL Display_Message( PROGRAM_NAME,                             &
                               'Error allocating for n_SBPoints array',  &
                               Error_Status                              )
      END IF
      
      ! Set the number of points for the channel
      n_Points = SIZE(MW_SensorData%Frequency(:,l))
      
      ! Set the number of bands for the channel
      IF(MW_SensorData%n_Sidebands(l)==0) THEN
        n_Bands=1
      ELSE  
        n_Bands = MW_SensorData%n_Sidebands(l)*2
      END IF
      
      ! Allocate for SRF structure
      Error_Status = Allocate_SRF( n_Points              ,  &  ! Input
                                   SRF                   ,  &  ! Output
                                   n_Bands                  )  ! Optional Output                                   
      IF ( Error_Status /= SUCCESS ) THEN
         CALL Display_Message( PROGRAM_NAME,                &
                               'Error Allocating for SRF',  &
                               Error_Status                 )
      END IF               
      
      ! Fill the SRF structure                                  
      SRF%Release          = RELEASE  
      SRF%Version          = VERSION
      SRF%Sensor_Id        = TRIM(Sensor_Id)
      SRF%WMO_Satellite_ID = WMO_Satellite_ID
      SRF%WMO_Sensor_ID    = WMO_Sensor_ID
      SRF%Sensor_Type      = Sensor_Type
      SRF%Channel          = Sensor_Channel(l)
      SRF%Frequency        = MW_SensorData%Frequency(:,l)
      SRF%Response         = MW_SensorData%Response(:,l)
            
      ! Obtain number of points for sidebands
      n_SBPoints(:)=0
      DO ln = 1, MW_SensorData%n_Sidebands( l )
        df = MW_SensorData%IF_Band(2,ln,l) - MW_SensorData%IF_Band(1,ln,l)
        n_SBPoints(ln) = NINT(df/MW_SensorData%Delta_Frequency(l)) + 1
      END DO
      
      ! assign the number of points to corresponding band and 
      ! assign the bounds for the bands
      IF (n_Bands==1) THEN
        SRF%f1_Band(1)=MW_SensorData%Central_Frequency(l)-MW_SensorData%IF_Band(2,1,l)
        SRF%f2_Band(1)=MW_SensorData%Central_Frequency(l)+MW_SensorData%IF_Band(2,1,l)
        SRF%npts_Band(1)=256
      ELSE IF(n_Bands==2) THEN
        SRF%f1_Band(1)=MW_SensorData%Central_Frequency(l)-MW_SensorData%IF_Band(2,1,l)
        SRF%f2_Band(1)=MW_SensorData%Central_Frequency(l)-MW_SensorData%IF_Band(1,1,l)
        SRF%f1_Band(2)=MW_SensorData%Central_Frequency(l)+MW_SensorData%IF_Band(1,1,l)
        SRF%f2_Band(2)=MW_SensorData%Central_Frequency(l)+MW_SensorData%IF_Band(2,1,l)
        SRF%npts_Band(1)=n_SBPoints(1)
        SRF%npts_Band(2)=n_SBPoints(1)
      ELSE IF(n_Bands==4) THEN
        SRF%f1_Band(1)=MW_SensorData%Central_Frequency(l)-MW_SensorData%IF_Band(2,2,l)
        SRF%f2_Band(1)=MW_SensorData%Central_Frequency(l)-MW_SensorData%IF_Band(1,2,l)
        SRF%f1_Band(2)=MW_SensorData%Central_Frequency(l)-MW_SensorData%IF_Band(2,1,l)
        SRF%f2_Band(2)=MW_SensorData%Central_Frequency(l)-MW_SensorData%IF_Band(1,1,l)
        SRF%f1_Band(3)=MW_SensorData%Central_Frequency(l)+MW_SensorData%IF_Band(1,1,l)
        SRF%f2_Band(3)=MW_SensorData%Central_Frequency(l)+MW_SensorData%IF_Band(2,1,l)
        SRF%f1_Band(4)=MW_SensorData%Central_Frequency(l)+MW_SensorData%IF_Band(1,2,l)
        SRF%f2_Band(4)=MW_SensorData%Central_Frequency(l)+MW_SensorData%IF_Band(2,2,l)
        SRF%npts_Band(1)=n_SBPoints(2)
        SRF%npts_Band(2)=n_SBPoints(1)
        SRF%npts_Band(3)=n_SBPoints(1)
        SRF%npts_Band(4)=n_SBPoints(2)
      END IF
      
      ! Fill the integrated and summation fields
      Error_Status = Integrate_SRF( SRF ) ! In/Output
      IF ( Error_Status /= SUCCESS ) THEN
         CALL Display_Message( PROGRAM_NAME,                                      &
                               'Error Calculating Integrated and Summation SRF',  &
                               Error_Status                                       )
      END IF
      
      ! Write the SRF data
      Error_Status = Write_SRF_netCDF(  NC_Filename,   &  ! In/Output
                                        SRF            )  ! Input
      IF ( Error_Status /= SUCCESS ) THEN
         CALL Display_Message( PROGRAM_NAME,                                      &
                               'Error writing the SRF data to file',              &
                               Error_Status                                       )
      END IF 
      
      ! Destroy the SRF structure
      Error_Status = Destroy_SRF( SRF )
      IF ( Error_Status /= SUCCESS ) THEN
         CALL Display_Message( PROGRAM_NAME,                                      &
                               'Error destroying SRF structure',                  &
                               Error_Status                                       )
      END IF 
      
      ! Allocate Sideband points array
      DEALLOCATE( n_SBPoints,          &
                  STAT = Error_Status  )
      IF ( Error_Status /= SUCCESS ) THEN
         CALL Display_Message( PROGRAM_NAME,                  &
                               'Error allocating for array',  &
                               Error_Status                   )
      END IF                                 
            
    END DO Channel_Loop
    
    ! Allocate for Sensor_Channel allocatable array
    DEALLOCATE( Sensor_Channel,      &
                STAT = Error_Status  )
    IF ( Error_Status /= SUCCESS ) THEN
       CALL Display_Message( PROGRAM_NAME,                    &
                             'Error deallocating for array',  &
                             Error_Status                     )
    END IF
    
    ! Destroy MW_SensorData structure
    Error_Status = Destroy_MW_SensorData( MW_SensorData ) 
    IF ( Error_Status /= SUCCESS ) THEN
         CALL Display_Message( PROGRAM_NAME,                                      &
                               'Error destroying MW_SensorData structure',        &
                               Error_Status                                       )
    END IF
    
  END DO Sensor_Loop
        
END PROGRAM Create_MW_SRF            
      
      
      
      
