!
! MW_SensorData2oSRF
!
! Program to create netcdf oSRF files for the Microwave using data 
! from the MW_Sensordata_Define module
!       
!
! CREATION HISTORY:
!       Written by:     David Groff, SAIC 01-Jan-2009
!                       david.groff@noaa.gov
! ------------------------------------------------------------------

PROGRAM MW_SensorData2oSRF
  
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
  USE oSRF_File_Define,          ONLY: oSRF_File_type,          &
                                       oSRF_File_Create,        &
                                       oSRF_File_Write,         &
                                       oSRF_File_Destroy
  USE oSRF_Define,               ONLY: oSRF_Integrate, &
                                       oSRF_Create
  USE SensorInfo_IO,             ONLY: Read_SensorInfo
  USE SensorInfo_LinkedList,     ONLY: SensorInfo_List_type,    &
                                       Count_SensorInfo_Nodes,  &
                                       GetFrom_SensorInfo_List
  USE SensorInfo_Define,         ONLY: SensorInfo_type
  USE SensorInfo_Parameters,     ONLY: MICROWAVE_SENSOR
                                       
  
  ! Parameters
  ! ----------
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'
  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'MW_SensorData2oSRF'

  
  ! Variables
  ! ---------
  TYPE( MW_SensorData_type ) :: MW_SensorData
  TYPE( SensorInfo_List_type ) :: SensorInfo_List
  TYPE( oSRF_File_type ) :: oSRF_File
  TYPE( SensorInfo_type ) :: SensorInfo
  INTEGER, DIMENSION(:), ALLOCATABLE :: n_BPoints
  CHARACTER(256)  :: SensorInfo_Filename
  CHARACTER(256)  :: NC_Filename
  CHARACTER(5000) :: Title, Comment
  INTEGER :: n_Frequencies
  INTEGER :: n_Sensors
  INTEGER :: n_Bands
  INTEGER :: n, l, ln
  INTEGER :: Error_Status
  INTEGER :: Version
  INTEGER :: Band1_End_Index
  INTEGER :: Band2_Begin_Index, Band2_End_Index
  INTEGER :: Band3_Begin_Index, Band3_End_Index
  INTEGER :: Band4_Begin_Index, Band4_End_Index
  REAL(fp) :: df1, df2
  
  ! Program header
  ! --------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to construct oSRF files from SRF data' // &
                        ' in the MW_SensorData_Define module.', &
                        '$Revision$'                             )
  
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
  WRITE( *,FMT='(/5x,"Default oSRF file version is: ",i0, &
               &".  Enter value: ")', &
           ADVANCE='NO' ) oSRF_File%Version
  READ( *,* ) Version
  IF ( Version < oSRF_File%Version ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid version number specified. Using default.', &
                          INFORMATION )
    Version = oSRF_File%Version
  END IF
  
  oSRF_File%Version = Version

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
     
    ! Set the title 
    Title = 'Microwave sensor ' // TRIM(SensorInfo%Sensor_Id) // ' Spectral Response Functions'
    Comment = 'The oSRF data for this sensor is assumed to be a box car shape.' // &
              ' This is a temporary state until real data becomes available. The ' // &
              'files now contain 256 points. Further testing will be performed'    // &
              ' to determine if this is an adequate number of points' 
    
    ! Create an instance of oSRF_File
    CALL oSRF_File_Create( oSRF_File,SensorInfo%n_Channels )
    
    oSRF_File%Filename         = TRIM(SensorInfo%Sensor_Id) // '.srf.nc'
    oSRF_File%Sensor_ID        = TRIM(SensorInfo%Sensor_Id)
    oSRF_File%WMO_Satellite_Id = SensorInfo%WMO_Satellite_Id
    oSRF_File%WMO_Sensor_Id    = SensorInfo%WMO_Sensor_Id
    oSRF_File%Sensor_Type      = MICROWAVE_SENSOR
    oSRF_File%Title            = Title
    oSRF_File%Comment          = Comment
                                           
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
      
      ! Set the number of bands for the channel
      IF(MW_SensorData%IF_Band(1,1,l)==ZERO) THEN
        n_Bands = 1
      ELSE  
        n_Bands = MW_SensorData%n_Sidebands(l)*2
      END IF 
      
      ! Allocate band points array
      ALLOCATE( n_BPoints(n_Bands),  &
                STAT = Error_Status  )
      IF ( Error_Status /= SUCCESS ) THEN
         CALL Display_Message( PROGRAM_NAME,                            &
                               'Error allocating for n_BPoints array',  &
                               Error_Status                             )
      END IF
      
      IF (n_Bands==1) THEN
        n_BPoints(1)=n_Frequencies
      ELSE IF (n_Bands==2) THEN
        df1 = MW_SensorData%IF_Band(2,1,l) - MW_SensorData%IF_Band(1,1,l)
        n_BPoints(1) = NINT(df1/MW_SensorData%Delta_Frequency(l)) + 1
        n_BPoints(2) = NINT(df1/MW_SensorData%Delta_Frequency(l)) + 1
        Band1_End_Index   = n_BPoints(1)
        Band2_Begin_Index = n_BPoints(1) + 1
        Band2_End_Index   = n_BPoints(1) + n_BPoints(2)
      ELSE IF (n_Bands==4) THEN
        df1 = MW_SensorData%IF_Band(2,1,l) - MW_SensorData%IF_Band(1,1,l)
        df2 = MW_SensorData%IF_Band(2,2,l) - MW_SensorData%IF_Band(1,2,l)
        n_BPoints(1) = NINT(df2/MW_SensorData%Delta_Frequency(l)) + 1
        n_BPoints(2) = NINT(df1/MW_SensorData%Delta_Frequency(l)) + 1
        n_BPoints(3) = NINT(df1/MW_SensorData%Delta_Frequency(l)) + 1
        n_BPoints(4) = NINT(df2/MW_SensorData%Delta_Frequency(l)) + 1
        Band1_End_Index   = n_BPoints(1)
        Band2_Begin_Index = n_BPoints(1) + 1
        Band2_End_Index   = n_BPoints(1) + n_BPoints(2)
        Band3_Begin_Index = n_BPoints(1) + n_BPoints(2) + 1
        Band3_End_Index   = n_BPoints(1) + n_BPoints(2) + n_BPoints(3) 
        Band4_Begin_Index = n_BPoints(1) + n_BPoints(2) + n_BPoints(3) + 1
        Band4_End_Index   = n_BPoints(1) + n_BPoints(2) + n_BPoints(3) + n_BPoints(4)
      END IF
      
      ! Create an instance of oSRF in oSRF_File
      CALL oSRF_Create( oSRF_File%oSRF(l),n_BPoints )
      ! Assign components to instance of oSRF structure
      ! -----------------------------------------------                                        
      oSRF_File%oSRF(l)%Channel          = SensorInfo%Sensor_Channel(l)
      oSRF_File%oSRF(l)%Version          = Version
      oSRF_File%oSRF(l)%Sensor_Id        = SensorInfo%Sensor_Id
      oSRF_File%oSRF(l)%WMO_Satellite_ID = SensorInfo%WMO_Satellite_ID
      oSRF_File%oSRF(l)%WMO_Sensor_ID    = SensorInfo%WMO_Sensor_ID 
      oSRF_File%oSRF(l)%Sensor_Type      = MICROWAVE_SENSOR
      oSRF_File%oSRF(l)%f0               = MW_SensorData%Central_Frequency(l)
      
      ! assign the number of points to corresponding band and 
      ! assign the bounds for the bands
      IF (n_Bands==1) THEN
        oSRF_File%oSRF(l)%f1(1)=MW_SensorData%Central_Frequency(l)-MW_SensorData%IF_Band(2,1,l)
        oSRF_File%oSRF(l)%f2(1)=MW_SensorData%Central_Frequency(l)+MW_SensorData%IF_Band(2,1,l)
        oSRF_File%oSRF(l)%Frequency(1)%Arr = MW_SensorData%Frequency(:,l)
        oSRF_File%oSRF(l)%Response(1)%Arr  = MW_SensorData%Response(:,l)
      ELSE IF(n_Bands==2) THEN
        oSRF_File%oSRF(l)%f1(1)=MW_SensorData%Central_Frequency(l)-MW_SensorData%IF_Band(2,1,l)
        oSRF_File%oSRF(l)%f2(1)=MW_SensorData%Central_Frequency(l)-MW_SensorData%IF_Band(1,1,l)
        oSRF_File%oSRF(l)%f1(2)=MW_SensorData%Central_Frequency(l)+MW_SensorData%IF_Band(1,1,l)
        oSRF_File%oSRF(l)%f2(2)=MW_SensorData%Central_Frequency(l)+MW_SensorData%IF_Band(2,1,l)
        oSRF_File%oSRF(l)%Frequency(1)%Arr = MW_SensorData%Frequency(1:Band1_End_Index,l)
        oSRF_File%oSRF(l)%Response(1)%Arr  = MW_SensorData%Response(1:Band1_End_Index,l)
        oSRF_File%oSRF(l)%Frequency(2)%Arr = MW_SensorData%Frequency(Band2_Begin_Index:Band2_End_Index,l)
        oSRF_File%oSRF(l)%Response(2)%Arr  = MW_SensorData%Response(Band2_Begin_Index:Band2_End_Index,l)
      ELSE IF(n_Bands==4) THEN
        oSRF_File%oSRF(l)%f1(1)=MW_SensorData%Central_Frequency(l)-MW_SensorData%IF_Band(2,2,l)
        oSRF_File%oSRF(l)%f2(1)=MW_SensorData%Central_Frequency(l)-MW_SensorData%IF_Band(1,2,l)
        oSRF_File%oSRF(l)%f1(2)=MW_SensorData%Central_Frequency(l)-MW_SensorData%IF_Band(2,1,l)
        oSRF_File%oSRF(l)%f2(2)=MW_SensorData%Central_Frequency(l)-MW_SensorData%IF_Band(1,1,l)
        oSRF_File%oSRF(l)%f1(3)=MW_SensorData%Central_Frequency(l)+MW_SensorData%IF_Band(1,1,l)
        oSRF_File%oSRF(l)%f2(3)=MW_SensorData%Central_Frequency(l)+MW_SensorData%IF_Band(2,1,l)
        oSRF_File%oSRF(l)%f1(4)=MW_SensorData%Central_Frequency(l)+MW_SensorData%IF_Band(1,2,l)
        oSRF_File%oSRF(l)%f2(4)=MW_SensorData%Central_Frequency(l)+MW_SensorData%IF_Band(2,2,l)
        oSRF_File%oSRF(l)%Frequency(1)%Arr = MW_SensorData%Frequency(1:Band1_End_Index,l)
        oSRF_File%oSRF(l)%Response(1)%Arr  = MW_SensorData%Response(1:Band1_End_Index,l)
        oSRF_File%oSRF(l)%Frequency(2)%Arr = MW_SensorData%Frequency(Band2_Begin_Index:Band2_End_Index,l)
        oSRF_File%oSRF(l)%Response(2)%Arr  = MW_SensorData%Response(Band2_Begin_Index:Band2_End_Index,l)
        oSRF_File%oSRF(l)%Frequency(3)%Arr = MW_SensorData%Frequency(Band3_Begin_Index:Band3_End_Index,l)
        oSRF_File%oSRF(l)%Response(3)%Arr  = MW_SensorData%Response(Band3_Begin_Index:Band3_End_Index,l)
        oSRF_File%oSRF(l)%Frequency(4)%Arr = MW_SensorData%Frequency(Band4_Begin_Index:Band4_End_Index,l)
        oSRF_File%oSRF(l)%Response(4)%Arr  = MW_SensorData%Response(Band4_Begin_Index:Band4_End_Index,l)
      END IF

      ! Fill the integrated and summation fields
      Error_Status = oSRF_Integrate( oSRF_File%oSRF(l) ) ! In/Output
      IF ( Error_Status /= SUCCESS ) THEN
         CALL Display_Message( PROGRAM_NAME,                                      &
                               'Error Calculating Integrated and Summation SRF',  &
                               Error_Status                                       )
      END IF
      
      ! Deallocate Sideband points array
      DEALLOCATE( n_BPoints,          &
                  STAT = Error_Status )
      IF ( Error_Status /= SUCCESS ) THEN
         CALL Display_Message( PROGRAM_NAME,                              &
                               'Error deallocating for n_BPoints array',  &
                               Error_Status                               )
      END IF                      
            
    END DO Channel_Loop


    ! Write the oSRF_File instance to the nc file
    Error_Status = oSRF_File_Write(  oSRF_File,          &  ! In/Output
                                     oSRF_File%Filename  )  ! Input
    IF ( Error_Status /= SUCCESS ) THEN
       CALL Display_Message( PROGRAM_NAME,                                      &
                             'Error writing the SRF data to file',              &
                             Error_Status                                       )
    END IF 
  
    ! Destroy the instance of oSRF_File
    CALL oSRF_File_Destroy(oSRF_File)
    
    ! Destroy MW_SensorData structure
    Error_Status = Destroy_MW_SensorData( MW_SensorData ) 
    IF ( Error_Status /= SUCCESS ) THEN
         CALL Display_Message( PROGRAM_NAME,                                      &
                               'Error destroying MW_SensorData structure',        &
                               Error_Status                                       )
    END IF
    
  END DO Sensor_Loop
        
END PROGRAM MW_SensorData2oSRF            
      
      
      
      
