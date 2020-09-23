!
! IR_SRF2oSRF
!
! Program to create netcdf oSRF files for IR sensors using data 
! from the old SRF format
!       
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CIRA/CSU 18-Sep-2012
!                       Yong.Chen@noaa.gov
! ------------------------------------------------------------------

PROGRAM IR_SRF2oSRF
  
  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds
  USE File_Utility
  USE Message_Handler            
  USE CRTM_Parameters           
  USE SRF_Define,                ONLY: SRF_type_old => SRF_type,  &                 
                                       Destroy_SRF_old => Destroy_SRF               
  USE SRF_netCDF_IO,             ONLY: Read_SRF_netCDF_old => Read_SRF_netCDF, &
                                       Inquire_SRF_netCDF_old => Inquire_SRF_netCDF
  USE oSRF_File_Define,          ONLY: oSRF_File_type,          &
                                       oSRF_File_Create,        &
                                       oSRF_File_Write,         &
                                       oSRF_File_Destroy
  USE oSRF_Define,               ONLY: oSRF_SetFrequencyGHz,            &
                                       oSRF_Polychromatic_Coefficients, &
                                       oSRF_Planck_Coefficients,        &
                                       oSRF_Central_Frequency,          &
                                       oSRF_Integrate,                  &
                                       oSRF_Create 
                                       
  USE SensorInfo_IO,             ONLY: Read_SensorInfo
  USE SensorInfo_LinkedList,     ONLY: SensorInfo_List_type,    &
                                       Count_SensorInfo_Nodes,  &
                                       GetFrom_SensorInfo_List
  USE SensorInfo_Define,         ONLY: SensorInfo_type
  USE SensorInfo_Parameters,     ONLY: INFRARED_SENSOR
                                         
  ! Parameters
  ! ----------
  CHARACTER( * ), PARAMETER :: PROGRAM_VERSION_ID = &
  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'IR_SRF2oSRF'

  
  ! Variables
  ! ---------
  TYPE( SRF_type_old ) :: SRF_Old
  TYPE( SensorInfo_List_type ) :: SensorInfo_List
  TYPE( oSRF_File_type ) :: oSRF_File
  TYPE( SensorInfo_type ) :: SensorInfo
  INTEGER, ALLOCATABLE :: n_BPoints(:), n_Points(:)
  REAL(fp),ALLOCATABLE :: Begin_Frequency(:), End_Frequency(:)
  CHARACTER(256)  :: msg
  CHARACTER(2000) :: History=''
  CHARACTER(2000) :: Comment=''
  CHARACTER(256)  :: Title=''
  CHARACTER(256)  :: SensorInfo_Filename
  CHARACTER(256)  :: SRF_Filename
  INTEGER :: n_Sensors
  INTEGER :: n_Bands
  INTEGER :: i, n, l, m
  INTEGER :: Error_Status, alloc_stat
  INTEGER :: Version
  REAL(fp) :: fi(2,4)
  
  ! Program header
  ! --------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to construct oSRF files from SRF data' // &
                        ' for IR sensors.', &
                        '$Revision$'                             )
  
  
  ! Read the SensorInfo file
  ! ... GET filename
  WRITE( *,FMT    ='(/5x,"Enter a SensorInfo filename: ")', &
           ADVANCE='NO' )
  READ( *,FMT='(a)' ) SensorInfo_Filename
  SensorInfo_Filename = ADJUSTL(SensorInfo_Filename)
  ! ...Read it
  Error_Status = Read_SensorInfo( SensorInfo_Filename, SensorInfo_List )
  IF ( Error_Status /= SUCCESS ) THEN
    msg = 'Error reading SensorInfo_file'
    CALL Display_Message( PROGRAM_NAME,msg,Error_Status )
    STOP
  END IF
  ! ...Get number of sensors
  n_Sensors = Count_SensorInfo_Nodes( SensorInfo_List )


  ! Ask for a user specified version for the SRF files to be created
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

  ! =================
  ! Loop over sensors
  ! =================
  Sensor_Loop: DO n = 1, n_Sensors 
    
    ! Get SensorInfo for sensor n
    Error_Status = GetFrom_SensorInfo_List( SensorInfo_List,     &  ! Input
                                            n,                   &  ! Input
                                            SensorInfo           )  ! Output
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( msg,'("Error getting SensorInfo node #",i0," from list. Cycling loop...")' ) n
      CALL Display_Message( PROGRAM_NAME,msg,Error_Status )
      CYCLE Sensor_Loop
    END IF 
    
    ! Cycle if its not a infrared sensor
    IF ( .NOT. (SensorInfo%Sensor_Type==INFRARED_SENSOR) ) CYCLE Sensor_Loop
        
    SRF_Filename = TRIM( SensorInfo%Sensor_Id )//'.srf.nc'

    IF ( .NOT. File_Exists( TRIM( SRF_Filename ) ) ) CYCLE Sensor_Loop   

    WRITE( *,'(/5x,"Creating oSRF format  for ",a,"...")' ) TRIM(SensorInfo%Sensor_Id)
    
    ! Create an instance of oSRF_File
    CALL oSRF_File_Create( oSRF_File,SensorInfo%n_Channels )
    ! ...Copy over other information
    oSRF_File%Filename         = TRIM(SensorInfo%Sensor_Id) // '.osrf.nc'
    oSRF_File%Sensor_ID        = TRIM(SensorInfo%Sensor_Id)
    oSRF_File%WMO_Satellite_Id = SensorInfo%WMO_Satellite_Id
    oSRF_File%WMO_Sensor_Id    = SensorInfo%WMO_Sensor_Id
    oSRF_File%Sensor_Type      = INFRARED_SENSOR
    
    ALLOCATE( n_Points(SensorInfo%n_Channels), &
              Begin_Frequency(SensorInfo%n_Channels), &
              End_Frequency(SensorInfo%n_Channels), &
             STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) THEN                                                 
      msg = 'Error allocating n_Points, Begin_Frequency, End_Frequency  array'  
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE )                        
      STOP                                                                      
    END IF                                                                 
         
    Error_Status =  Inquire_SRF_netCDF_old( SRF_Filename, &
                                            n_Points         = n_Points, &
                                            Begin_Frequency  = Begin_Frequency,  &
                                            End_Frequency    = End_Frequency,    &
                                            Title            = Title,   &     
                                            History          = History, &     
                                            Comment          = Comment   )    

    IF ( Error_Status /= SUCCESS ) THEN
      CALL display_message( PROGRAM_NAME, &
                            'Error inquire old format SRF file '//&
                            TRIM( SRF_Filename  ), &
                            Error_Status )
      STOP
    END IF

    oSRF_File%Title            = trim(Title) 
    oSRF_File%History          = PROGRAM_VERSION_ID//trim(History)
    oSRF_File%Comment          = 'Convert from old SRF to oSRF; ' //trim(Comment)   

    ! ==================
    ! Loop over channels 
    ! ==================
    Channel_Loop: DO l = 1, SensorInfo%n_Channels

      ! read old srf
      Error_Status = Read_SRF_netCDF_old( SRF_Filename, &
                                          SensorInfo%Sensor_Channel(l), &
                                          SRF_old)

      IF ( Error_Status /= SUCCESS ) THEN
        CALL display_message( PROGRAM_NAME, &
                              'Error reading old format SRF file '//&
                              TRIM( SRF_Filename  ), &
                              Error_Status )
        STOP
      END IF
     
      n_Bands = 1
      
      ! Initialise arrays
      fi = ZERO
       
      ! Set the number of bands for the channel
      ! and reorganise the frequencies
      n_Bands = 1
      fi(1,1) = Begin_Frequency(l)
      fi(2,1) = End_Frequency(l) 
      
      ! Fill the band points array for oSRF allocation
      ! ...Allocate band points array
      ALLOCATE( n_BPoints(n_Bands), STAT = alloc_stat )
      IF ( alloc_stat /= 0 ) THEN
        msg = 'Error allocating n_BPoints array'
        CALL Display_Message( PROGRAM_NAME, msg, FAILURE )
        STOP 
      END IF
      ! ...Set the number of points/band
      SELECT CASE(n_Bands)
        CASE (1)
          n_BPoints(1)=n_Points(l) 
        CASE DEFAULT
          msg = 'Invalid number of bands!'
          CALL Display_Message( PROGRAM_NAME, msg, FAILURE )
          STOP     
      END SELECT
       
      ! Create an instance of oSRF in oSRF_File
      CALL oSRF_Create( oSRF_File%oSRF(l), n_BPoints )
      ! ...Copy over non-srf info
      oSRF_File%oSRF(l)%Channel          = SensorInfo%Sensor_Channel(l)
      oSRF_File%oSRF(l)%Version          = Version
      oSRF_File%oSRF(l)%Sensor_Id        = SensorInfo%Sensor_Id
      oSRF_File%oSRF(l)%WMO_Satellite_ID = SensorInfo%WMO_Satellite_ID
      oSRF_File%oSRF(l)%WMO_Sensor_ID    = SensorInfo%WMO_Sensor_ID 
      oSRF_File%oSRF(l)%Sensor_Type      = INFRARED_SENSOR

      ! Fill the frequency and response arrays band-by-band
      DO i = 1, n_Bands
        oSRF_File%oSRF(l)%f1(i) = fi(1,i)
        oSRF_File%oSRF(l)%f2(i) = fi(2,i)
        oSRF_File%oSRF(l)%Frequency(i)%Arr = fi(1, i) + (fi(2,i)-fi(1,i)) *(/ (REAL(m,fp),m=0,n_Points(l)-1) /) &
                                             / REAL(n_Points(l)-1,fp)
        oSRF_File%oSRF(l)%Response(i)%Arr  = SRF_old%Response
      END DO
       
      ! Fill the integrated and summation fields
      Error_Status = oSRF_Integrate( oSRF_File%oSRF(l) ) ! In/Output
      IF ( Error_Status /= SUCCESS ) THEN
         CALL Display_Message( PROGRAM_NAME,                                      &
                               'Error Calculating Integrated and Summation SRF',  &
                               Error_Status                                       )
      END IF
      
      ! Fill the Central Frequency field
      Error_Status = oSRF_Central_Frequency( oSRF_File%oSRF(l) ) ! In/Output
      IF ( Error_Status /= SUCCESS ) THEN
         CALL Display_Message( PROGRAM_NAME,                               &
                               'Error Calculating the central frequency',  &
                               Error_Status                                )
      END IF
      
      ! Fill the Polychromatic coefficient field
      Error_Status = oSRF_Polychromatic_Coefficients( oSRF_File%oSRF(l) ) ! In/Output
      IF ( Error_Status /= SUCCESS ) THEN
         CALL Display_Message( PROGRAM_NAME,                                    &
                               'Error Calculating Polychromatic Coefficients',  &
                               Error_Status                                     )
      END IF

      ! Fill the Planck coefficient field
      Error_Status = oSRF_Planck_Coefficients( oSRF_File%oSRF(l) ) ! In/Output
      IF ( Error_Status /= SUCCESS ) THEN
         CALL Display_Message( PROGRAM_NAME,                             &
                               'Error Calculating Planck Coefficients',  &
                               Error_Status                              )
      END IF
      
      ! Deallocate Sideband points array
      DEALLOCATE( n_BPoints, STAT = alloc_stat )
      IF ( alloc_stat /= 0 ) THEN
        msg = 'Error deallocating n_BPoints array'
        CALL Display_Message( PROGRAM_NAME, msg, FAILURE )
      END IF                      
            
    END DO Channel_Loop
    
     ! Deallocate Sideband points array                   
     DEALLOCATE( n_Points, Begin_Frequency, End_Frequency, &
        STAT = alloc_stat )           
     IF ( alloc_stat /= 0 ) THEN                          
       msg = 'Error deallocating n_Points,Begin_Frequency, and End_Frequency array'         
       CALL Display_Message( PROGRAM_NAME, msg, FAILURE ) 
     END IF                                               

     Error_Status = Destroy_SRF_old( SRF_Old )

      IF ( Error_Status /= SUCCESS ) THEN  
       msg = 'Error destroying SRF_old data structure'
       CALL Display_Message( PROGRAM_NAME, msg, FAILURE )
      END IF


    ! Write the oSRF_File instance to the nc file
    Error_Status = oSRF_File_Write( oSRF_File, oSRF_File%Filename )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error writing the SRF data to file'
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE )
    END IF                      

    ! Clean up  
    CALL oSRF_File_Destroy(oSRF_File)
     
  END DO Sensor_Loop
        
END PROGRAM IR_SRF2oSRF            
      
      
      
      
