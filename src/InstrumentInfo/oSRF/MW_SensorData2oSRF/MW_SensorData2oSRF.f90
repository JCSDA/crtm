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
                                       MW_SensorData_Load,      &
                                       MW_SensorData_Destroy,   & 
                                       MW_SensorData_DefineVersion
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
  USE SensorInfo_Parameters,     ONLY: MICROWAVE_SENSOR
                                         
  ! Parameters
  ! ----------
  CHARACTER( * ), PARAMETER :: PROGRAM_VERSION_ID = &
  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'MW_SensorData2oSRF'

  
  ! Variables
  ! ---------
  TYPE( MW_SensorData_type ) :: MW_SensorData
  TYPE( SensorInfo_List_type ) :: SensorInfo_List
  TYPE( oSRF_File_type ) :: oSRF_File
  TYPE( SensorInfo_type ) :: SensorInfo
  INTEGER, ALLOCATABLE :: n_BPoints(:)
  CHARACTER(256)  :: msg
  CHARACTER(2000) :: History=''
  CHARACTER(256)  :: SensorInfo_Filename
  INTEGER :: n_Frequencies
  INTEGER :: n_Sensors
  INTEGER :: n_Bands, n_Sidebands
  INTEGER :: i, n, l
  INTEGER :: Error_Status, alloc_stat
  INTEGER :: Version
  INTEGER :: bidx(4), eidx(0:4)
  REAL(fp) :: bf(4), ef(4), f0
  REAL(fp) :: df1, df2
  REAL(fp) :: fi(2,4)
  
  ! Program header
  ! --------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to construct oSRF files from SRF data' // &
                        ' in the MW_SensorData_Define module.', &
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


  ! Get the number of frequency points
  WRITE( *,FMT    ='(/5x,"Enter number of frequencies [256] :")', &
           ADVANCE='NO' )
  READ( *,* ) n_Frequencies
  
  
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
    

    ! Cycle if its not a microwave sensor
    IF ( .NOT. (SensorInfo%Sensor_Type==MICROWAVE_SENSOR) ) CYCLE Sensor_Loop


    WRITE( *,'(/5x,"Creating boxcar oSRF for ",a,"...")' ) TRIM(SensorInfo%Sensor_Id)
        
    
    ! Fill the MW_SensorData structure
    Error_Status = MW_SensorData_Load( MW_SensorData,                  &
                                       SensorInfo%Sensor_Id, &
                                       n_Frequencies=n_Frequencies      )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error loading MW_SensorData'
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE )
      STOP
    END IF

    CALL MW_SensorData_DefineVersion( History )
    History = '; '//TRIM(History)


    ! Create an instance of oSRF_File
    CALL oSRF_File_Create( oSRF_File,SensorInfo%n_Channels )
    ! ...Copy over other information
    oSRF_File%Filename         = TRIM(SensorInfo%Sensor_Id) // '.osrf.nc'
    oSRF_File%Sensor_ID        = TRIM(SensorInfo%Sensor_Id)
    oSRF_File%WMO_Satellite_Id = SensorInfo%WMO_Satellite_Id
    oSRF_File%WMO_Sensor_Id    = SensorInfo%WMO_Sensor_Id
    oSRF_File%Sensor_Type      = MICROWAVE_SENSOR
    oSRF_File%Title            = TRIM(SensorInfo%Sensor_Id)//' Boxcar Spectral Response Functions'
    oSRF_File%History          = PROGRAM_VERSION_ID//History
    oSRF_File%Comment          = 'The SRF for this sensor is computed from central frequencies and ' // &
                                 'intermediate frequencies assuming a boxcar response across the passband-width. ' // &
                                 'Each channel contains 256 points.' 



    ! ==================
    ! Loop over channels 
    ! ==================
    Channel_Loop: DO l = 1, SensorInfo%n_Channels

      
      ! Initialise arrays
      bidx = 0
      eidx = 0
      fi = ZERO
      bf = ZERO
      ef = ZERO

      
      ! Set the number of bands for the channel
      ! and reorganise the intermediate frequencies
      IF(MW_SensorData%IF_Band(1,1,l)==ZERO) THEN
        n_Bands = 1
        fi(1,1) = -MW_SensorData%IF_Band(2,1,l)
        fi(2,1) =  MW_SensorData%IF_Band(2,1,l)
      ELSE  
        n_Sidebands = MW_SensorData%n_Sidebands(l)
        n_Bands     = n_Sidebands*2
        fi(1:2,1:n_SideBands)         = -MW_SensorData%IF_Band(2:1:-1,n_SideBands:1:-1,l)
        fi(1:2,n_SideBands+1:n_Bands) =  MW_SensorData%IF_Band(  1:2 ,1:n_SideBands   ,l)
      END IF 
      

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
          n_BPoints(1)=n_Frequencies
        CASE (2)
          df1 = MW_SensorData%IF_Band(2,1,l) - MW_SensorData%IF_Band(1,1,l)
          n_BPoints(1) = NINT(df1/MW_SensorData%Delta_Frequency(l)) + 1
          n_BPoints(2) = NINT(df1/MW_SensorData%Delta_Frequency(l)) + 1
        CASE (4)
          df1 = MW_SensorData%IF_Band(2,1,l) - MW_SensorData%IF_Band(1,1,l)
          df2 = MW_SensorData%IF_Band(2,2,l) - MW_SensorData%IF_Band(1,2,l)
          n_BPoints(1) = NINT(df2/MW_SensorData%Delta_Frequency(l)) + 1
          n_BPoints(2) = NINT(df1/MW_SensorData%Delta_Frequency(l)) + 1
          n_BPoints(3) = NINT(df1/MW_SensorData%Delta_Frequency(l)) + 1
          n_BPoints(4) = NINT(df2/MW_SensorData%Delta_Frequency(l)) + 1
        CASE DEFAULT
          msg = 'Invalid number of bands!'
          CALL Display_Message( PROGRAM_NAME, msg, FAILURE )
          STOP     
      END SELECT


      ! Compute the indexing values for each band
      DO i = 1, n_Bands
        bidx(i) = eidx(i-1) + 1
        eidx(i) = SUM(n_BPoints(1:i))
      END DO

      
      ! Create an instance of oSRF in oSRF_File
      CALL oSRF_Create( oSRF_File%oSRF(l), n_BPoints )
      ! ...Copy over non-srf info
      oSRF_File%oSRF(l)%Channel          = SensorInfo%Sensor_Channel(l)
      oSRF_File%oSRF(l)%Version          = Version
      oSRF_File%oSRF(l)%Sensor_Id        = SensorInfo%Sensor_Id
      oSRF_File%oSRF(l)%WMO_Satellite_ID = SensorInfo%WMO_Satellite_ID
      oSRF_File%oSRF(l)%WMO_Sensor_ID    = SensorInfo%WMO_Sensor_ID 
      oSRF_File%oSRF(l)%Sensor_Type      = MICROWAVE_SENSOR

      
      ! Fill the frequency and response arrays band-by-band
      f0 = MW_SensorData%Central_Frequency(l)
      DO i = 1, n_Bands
        oSRF_File%oSRF(l)%f1(i) = f0+fi(1,i)
        oSRF_File%oSRF(l)%f2(i) = f0+fi(2,i)
        oSRF_File%oSRF(l)%Frequency(i)%Arr = MW_SensorData%Frequency(bidx(i):eidx(i),l)
        oSRF_File%oSRF(l)%Response(i)%Arr  = MW_SensorData%Response(bidx(i):eidx(i),l)
      END DO
      
      ! Set the frequency units bit flag to indicate units of GHz
      CALL oSRF_SetFrequencyGHz(oSRF_File%oSRF(l))

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


    ! Write the oSRF_File instance to the nc file
    Error_Status = oSRF_File_Write( oSRF_File, oSRF_File%Filename )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error writing the SRF data to file'
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE )
    END IF                      


    ! Clean up  
    CALL oSRF_File_Destroy(oSRF_File)
    CALL MW_SensorData_Destroy( MW_SensorData ) 
    
  END DO Sensor_Loop
        
END PROGRAM MW_SensorData2oSRF            
      
      
      
      
