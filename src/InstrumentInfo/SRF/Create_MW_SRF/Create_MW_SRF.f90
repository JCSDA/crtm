!
! Create_MW_SRF
!
! Program to create netcdf SRF files for the Microwave using data 
! from the MW_Sensordata_Define module
!       
!
! CREATION HISTORY:
!       Written by:     David Groff, SAIC 01-Jan-2009
!                       david.groff@noaa.gov
! ------------------------------------------------------------------

PROGRAM Create_MW_SRF
  
  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds
  USE File_Utility
  USE Message_Handler            
  USE CRTM_Parameters           
  USE MW_SensorData_Define,      ONLY: MW_SensorData_type,      &
                                       Allocate_MW_SensorData,  &
                                       Load_MW_SensorData,      &
                                       Destroy_MW_SensorData 
  USE SRF_Define,                ONLY: SRF_type,                &
                                       Integrate_SRF,           &
                                       Allocate_SRF,            &
                                       Destroy_SRF
  USE SRF_netCDF_IO,             ONLY: Create_SRF_netCDF,       &
                                       Write_SRF_netCDF
  USE SensorInfo_IO,             ONLY: Read_SensorInfo
  USE SensorInfo_LinkedList,     ONLY: SensorInfo_List_type,    &
                                       Count_SensorInfo_Nodes,  &
                                       GetFrom_SensorInfo_List
  USE SensorInfo_Define,         ONLY: SensorInfo_type
  USE SensorInfo_Parameters,     ONLY: MICROWAVE_SENSOR
                                       
  
  ! Parameters
  ! ----------
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Create_MW_SRF'

  
  ! Variables
  ! ---------
  TYPE( MW_SensorData_type ) :: MW_SensorData
  TYPE( SensorInfo_List_type ) :: SensorInfo_List
  TYPE( SRF_type ) :: SRF
  TYPE( SensorInfo_type ) :: SensorInfo
  INTEGER, DIMENSION(:), ALLOCATABLE :: n_SBPoints
  CHARACTER(256)  :: SensorInfo_Filename
  CHARACTER(256)  :: NC_Filename
  CHARACTER(5000) :: Title, Comment
  INTEGER :: n_Frequencies
  INTEGER :: n_Sensors
  INTEGER :: n_Bands
  INTEGER :: n, l, ln
  INTEGER :: Error_Status
  INTEGER :: Version
  REAL(fp) :: df
  
  ! Program header
  ! --------------
  CALL Program_Message( PROGRAM_NAME,                                            &
                        'Program to create netcdf SRF files for the microwave.', &
                        '$Revision$'                                      )
  
  ! Get user specified inputs
  ! -------------------------
  
  ! Set a user specified SensorInfo filename
  WRITE( *,FMT    ='(/5x,"Enter a SensorInfo filename: ")', &
           ADVANCE='NO' )
  READ( *,FMT='(a)' ) SensorInfo_Filename
  SensorInfo_Filename = ADJUSTL(SensorInfo_Filename)
  
  ! Read SensorInfo file
  Error_Status = Read_SensorInfo( SensorInfo_Filename,  & ! Input
                                  SensorInfo_List       ) ! Output
  IF ( Error_Status /= SUCCESS ) THEN
     CALL Display_Message( PROGRAM_NAME,                     &
                           'Error reading SensorInfo_file',  &
                           Error_Status                      )
    STOP
  END IF
  
  ! Get number of sensors
  n_Sensors = Count_SensorInfo_Nodes( SensorInfo_List )
    
  ! Set a user specified version for the SRF files to be created
  WRITE( *,FMT='(/5x,"Default SRF file version is: ",i0, &
               &".  Enter value: ")', &
           ADVANCE='NO' ) SRF%VERSION
  READ( *,* ) Version
  IF ( Version < SRF%VERSION ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid version number specified. Using default.', &
                          INFORMATION )
    Version = SRF%VERSION
  END IF

  ! Get the number of frequency points
  WRITE( *,FMT    ='(/5x,"Enter number of frequencies [256] :")', &
           ADVANCE='NO' )
  READ( *,* ) n_Frequencies
  
  ! Loop over sensors
  !--------------------------------
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
    
    ! Cycle if its not a microwave sensor or if the sensor name is SSMIS 
    IF ( .NOT. (SensorInfo%Sensor_Type==MICROWAVE_SENSOR) ) CYCLE Sensor_Loop !.OR. &
    !      TRIM(SensorInfo%Sensor_Name)=='SSMIS' ) CYCLE Sensor_Loop
          
    ! Name of file to write to for sensor
    NC_Filename= TRIM(SensorInfo%Sensor_Id) // '.srf.nc'
     
    ! Set the title 
    Title = 'Microwave sensor ' // TRIM(SensorInfo%Sensor_Id) // ' Spectral Response Functions'
    Comment = 'The SRF data for this sensor is assumed to be a box car shape.'     // &
              ' This is a temporary state until real data becomes available. The ' // &
              'files now contain 256 points. Further testing will be performed'    // &
              ' to determine if this is an adequate number of points'
    
    ! Create srf netcdf files for the MW
    Error_Status = Create_SRF_netCDF( NC_Filename                                    ,  & ! Input
                                      MICROWAVE_SENSOR                               ,  & ! Input (MW)
                                      SensorInfo%Sensor_Channel                      ,  & ! Input Channel list                               
                                      Version          = Version                     ,  & ! Optional Input               
                                      Sensor_Id        = TRIM(SensorInfo%Sensor_Id)  ,  & ! Optional Input                
                                      WMO_Satellite_Id = SensorInfo%WMO_Satellite_Id ,  & ! Optional Input                
                                      WMO_Sensor_Id    = SensorInfo%WMO_Sensor_Id    ,  & ! Optional Input             
                                      Title            = Title                       ,  & ! Optional Input     
                                      History          = PROGRAM_RCS_ID              ,  & ! Optional Input       
                                      Comment          = Comment                        ) ! Optional Input
    IF ( Error_Status /= SUCCESS ) THEN
       CALL Display_Message( PROGRAM_NAME,                      &
                             'Error creating SRF netcdf file',  &
                             Error_Status                       )
    END IF                         
                                           
    ! Fill the MW_SensorData structure
    Error_Status = Load_MW_SensorData( MW_SensorData,                  &
                                       n_Frequencies=n_Frequencies,    &
                                       Sensor_Id=SensorInfo%Sensor_Id  )
    IF ( Error_Status /= SUCCESS ) THEN
       CALL Display_Message( PROGRAM_NAME,                  &
                             'Error loading MW_SensorData', &
                             Error_Status                   )
    END IF
                                               
    ! Loop over channels 
    ! -------------------------------------------                                    
    Channel_Loop: DO l = 1, SensorInfo%n_Channels
      
      ! Allocate Sideband points array
      ALLOCATE( n_SBPoints(MW_SensorData%n_Sidebands(l)),  &
                    STAT = Error_Status                    )
      IF ( Error_Status /= SUCCESS ) THEN
         CALL Display_Message( PROGRAM_NAME,                             &
                               'Error allocating for n_SBPoints array',  &
                               Error_Status                              )
      END IF
      
      ! Set the number of bands for the channel
      IF(MW_SensorData%IF_Band(1,1,l)==ZERO) THEN
        n_Bands = 1
      ELSE  
        n_Bands = MW_SensorData%n_Sidebands(l)*2
      END IF
      
      ! Allocate for SRF structure
      Error_Status = Allocate_SRF( n_Frequencies,  &  ! Input
                                   SRF          ,  &  ! Output
                                   n_Bands         )  ! Optional Output                                   
      IF ( Error_Status /= SUCCESS ) THEN
         CALL Display_Message( PROGRAM_NAME,                &
                               'Error Allocating for SRF',  &
                               Error_Status                 )
      END IF               
      
      ! Assign components of SRF structure
      ! ----------------------------------                                        
      SRF%Channel          = SensorInfo%Sensor_Channel(l)
      SRF%Frequency        = MW_SensorData%Frequency(:,l)
      SRF%Response         = MW_SensorData%Response(:,l)
      SRF%Version          = Version
      SRF%Sensor_Id        = SensorInfo%Sensor_Id
      SRF%WMO_Satellite_ID = SensorInfo%WMO_Satellite_ID
      SRF%WMO_Sensor_ID    = SensorInfo%WMO_Sensor_ID 
      SRF%Sensor_Type      = MICROWAVE_SENSOR
            
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
        SRF%npts_Band(1)=n_Frequencies
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
      
      ! Deallocate Sideband points array
      DEALLOCATE( n_SBPoints,          &
                  STAT = Error_Status  )
      IF ( Error_Status /= SUCCESS ) THEN
         CALL Display_Message( PROGRAM_NAME,                               &
                               'Error deallocating for n_SBPoints array',  &
                               Error_Status                                )
      END IF                                 
            
    END DO Channel_Loop
    
    ! Destroy MW_SensorData structure
    Error_Status = Destroy_MW_SensorData( MW_SensorData ) 
    IF ( Error_Status /= SUCCESS ) THEN
         CALL Display_Message( PROGRAM_NAME,                                      &
                               'Error destroying MW_SensorData structure',        &
                               Error_Status                                       )
    END IF
    
  END DO Sensor_Loop
        
END PROGRAM Create_MW_SRF            
      
      
      
      
