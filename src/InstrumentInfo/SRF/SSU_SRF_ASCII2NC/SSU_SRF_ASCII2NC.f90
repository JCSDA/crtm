!
! SSU_SRF_ASCII2NC
!
! Program to convert the SSU SRF ASCII files to netCDF format
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 24-Jul-2007
!                       paul.vandelst@noaa.gov
!

PROGRAM SSU_SRF_ASCII2NC

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, &
                                   Display_Message, Program_Message
  USE File_Utility         , ONLY: Get_Lun, File_Exists
  USE SensorInfo_Define    , ONLY: SensorInfo_type, &
                                   Allocate_SensorInfo, &
                                   Destroy_SensorInfo
  USE SensorInfo_LinkedList, ONLY: SensorInfo_List_type, &
                                   New_SensorInfo_List, &
                                   Destroy_SensorInfo_List, &
                                   AddTo_SensorInfo_List, &
                                   GetFrom_SensorInfo_List, &
                                   Count_SensorInfo_Nodes 
  USE SensorInfo_IO        , ONLY: Read_SensorInfo, Write_SensorInfo
  USE SRF_Define           , ONLY: SRF_type, &
                                   Allocate_SRF, &
                                   Destroy_SRF, &
                                   Integrate_SRF
  USE SRF_netCDF_IO        !, ONLY:
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*),  PARAMETER :: PROGRAM_NAME   = 'SSU_SRF_ASCII2NC'
  CHARACTER(*),  PARAMETER :: PROGRAM_RCS_ID = &
  
  REAL(fp), PARAMETER :: ONE = 1.0_fp
  INTEGER , PARAMETER :: SET = 1
  
  INTEGER,      PARAMETER :: N_SSU_CHANNELS = 3
  INTEGER,      PARAMETER :: N_SSU_SENSORS = 7
  CHARACTER(*), PARAMETER :: SENSOR_ID(N_SSU_SENSORS) = &
    (/ 'ssu_tirosn          ', 'ssu_n06             ', &
       'ssu_n07             ', 'ssu_n08             ', &
       'ssu_n09             ', 'ssu_n11             ', &
       'ssu_n14             ' /)
       

  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: SensorInfo_Filename
  CHARACTER(256) :: ASCII_Filename
  CHARACTER(256) :: NC_Filename
  CHARACTER(256) :: Title
  CHARACTER(256) :: Comment
  INTEGER :: Error_Status, IO_Status
  INTEGER :: FileID
  INTEGER :: n_Sensors, n
  INTEGER :: n_Pts, i
  REAL(fp) :: Norm_Factor(N_SSU_CHANNELS)
  REAL(fp) :: Cell_Pressure(N_SSU_CHANNELS)
  TYPE(SRF_type) :: SRF(N_SSU_CHANNELS)
  TYPE(SensorInfo_type)      :: SensorInfo
  TYPE(SensorInfo_List_type) :: SensorInfo_List

                                                           
  ! Output header
  ! -------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to read ASCII column format SSU SRF data files '//&
                        'and write netCDF format SRF data files.', &
                        '$Revision$' )

  ! Load the SensorInfo information
  ! -------------------------------
  ! Get the filename
  WRITE( *,FMT='(/5x,"Enter a SensorInfo filename: ")',ADVANCE='NO' )
  READ( *,FMT='(a)' ) SensorInfo_Filename
  SensorInfo_Filename = ADJUSTL( SensorInfo_Filename )
  
  ! Read the file
  Error_Status = Read_SensorInfo( SensorInfo_Filename, &
                                  SensorInfo_List, &
                                  Quiet=SET )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading SensorInfo file '//TRIM(SensorInfo_Filename), &
                          FAILURE )
    STOP
  END IF

  ! Count the number of sensors
  n_Sensors = Count_SensorInfo_Nodes( SensorInfo_List )
  IF ( n_Sensors < 1 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'SensorInfo_List is empty.', &
                          FAILURE )
    STOP
  END IF
  WRITE( *,'(5x,"Number of sensors in list: ",i0)' ) n_Sensors


  ! Begin loop over SensorInfo sensors
  ! ----------------------------------
  Sensor_Loop: DO n = 1, n_Sensors
  
  
    ! Get the current SensorInfo node from the list
    ! ---------------------------------------------
    Error_Status = GetFrom_SensorInfo_List( SensorInfo_List, &
                                            n, &
                                            SensorInfo )
    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Error retrieving SensorInfo data for sensor # ",i0)' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            Error_Status )
      STOP
    END IF
    
    
    ! Cycle loop if not an SSU sensor
    ! -------------------------------
    IF ( .NOT. ANY(SENSOR_ID == SensorInfo%Sensor_Id) ) CYCLE Sensor_Loop


    ! Create filenames
    ! ----------------
    ASCII_Filename = TRIM(SensorInfo%Sensor_Id)//'.srf'
    NC_Filename    = TRIM(ASCII_Filename)//'.nc'
    
    
    ! Cycle loop if ASCII file not available
    ! --------------------------------------
    IF ( .NOT. File_Exists( ASCII_Filename ) ) CYCLE Sensor_Loop

    CALL Display_Message( PROGRAM_NAME, &
                          'Processing '//TRIM(ASCII_Filename), &
                          INFORMATION )
    

    ! Read ASCII SRF file
    ! -------------------
    ! Open file
    FileID = Get_Lun()
    IF ( FileID < 0 ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error obtaining FileID for '//TRIM(ASCII_Filename), &
                            FAILURE )
      STOP
    END IF
    OPEN( FileID,FILE  =ASCII_Filename, &
                 STATUS='OLD', &
                 ACCESS='SEQUENTIAL', &
                 FORM  ='FORMATTED', &
                 ACTION='READ', &
                 IOSTAT=IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error opening ",a,". IOSTAT=",i0)' ) TRIM(ASCII_Filename), IO_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            FAILURE )
      STOP
    END IF

    ! Read header
    READ( FileID,*,IOSTAT=IO_Status ) n_Pts, Norm_Factor, Cell_Pressure
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading ",a," header. IOSTAT=",i0)' ) TRIM(ASCII_Filename), IO_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            FAILURE )
      STOP
    END IF

    ! Allocate SRF data structures
    DO i = 1, N_SSU_CHANNELS
      Error_Status = Allocate_SRF( n_Pts, &
                                   SRF(i) )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message,'("Error allocating channel ",i0," SRF for ",a)' ) &
                       i, TRIM(SensorInfo%Sensor_Id)
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              FAILURE )
        STOP
      END IF
      ! Assign channel number
      SRF(i)%Channel = SensorInfo%Sensor_Channel(i)
    END DO
    
    ! Read SRF data
    DO i = 1, n_Pts
      READ( FileID,*,IOSTAT=IO_Status ) SRF(1)%Frequency(i), &
                                        SRF(1)%Response(i) , &
                                        SRF(2)%Response(i) , &
                                        SRF(3)%Response(i)
      IF ( IO_Status /= 0 ) THEN
        WRITE( Message,'("Error reading ",a," data at line #",i0,". IOSTAT=",i0)' ) &
                       TRIM(ASCII_Filename), i, IO_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              FAILURE )
        STOP
      END IF
    END DO
    WRITE( *,'(5x,"Read ",i0," SRF data poionts")' ) n_Pts

    ! Copy over other SRF structure bits
    SRF%Sensor_Name      = SensorInfo%Sensor_Name
    SRF%Platform_Name    = SensorInfo%Satellite_Name
    SRF%WMO_Satellite_Id = SensorInfo%WMO_Satellite_Id
    SRF%WMO_Sensor_Id    = SensorInfo%WMO_Sensor_Id
    SRF%Begin_Frequency  = SRF(1)%Frequency(1)
    SRF%End_Frequency    = SRF(1)%Frequency(n_Pts)
    DO i = 2, N_SSU_CHANNELS
      SRF(i)%Frequency  = SRF(1)%Frequency
    END DO
    
    ! Compute the integrated SRF values
    DO i = 1, N_SSU_CHANNELS
      Error_Status = Integrate_SRF(SRF(i))
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message,'("Error computing integrated SRFs for channel ",i0," SRF from ",a)' ) &
                        SRF(i)%Channel, TRIM(ASCII_Filename)
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              FAILURE )
        STOP
      END IF
    END DO
    
    ! Close file
    CLOSE( FileID )
    
    
    ! Write the SRFs to the netCDF file
    ! ---------------------------------
    ! Create global attribute strings
    Title   = 'SRFs for '//TRIM(SensorInfo%Sensor_Id)
    WRITE( Comment,'("ASCII SRF data provided by Q.Liu. Cell pressures(hPa):",3(1x,es13.6))' ) &
                   Cell_Pressure
    ! Create the output netCDF file
    Error_Status = Create_SRF_netCDF( TRIM(NC_Filename), &
                                      SensorInfo%Sensor_Channel, &
                                      WMO_Satellite_ID=SensorInfo%WMO_Satellite_ID, &
                                      WMO_Sensor_ID   =SensorInfo%WMO_Sensor_ID, &
                                      Title           =TRIM(Title), &
                                      History         =PROGRAM_RCS_ID, &
                                      Sensor_Name     =TRIM(SensorInfo%Sensor_Name), &
                                      Platform_Name   =TRIM(SensorInfo%Satellite_Name), &
                                      Comment         =TRIM(Comment) )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error creating '//TRIM(NC_Filename), &
                            FAILURE )
      STOP
    END IF
    
    ! Write each channel to the file
    DO i = 1, N_SSU_CHANNELS
      Error_Status = Write_SRF_netCDF( NC_Filename, SRF(i) )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message,'("Error occurred writing channel #",i0," SRF to ",a)' ) &
                       SRF(i)%Channel, TRIM(NC_Filename)
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              FAILURE )
        STOP
      END IF
    END DO
    
    
    ! Destroy the SRF structure array
    ! -------------------------------
    Error_Status = Destroy_SRF( SRF )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message,'("Error allocating channel ",i0," SRF for ",a)' ) &
                     i, TRIM(SensorInfo%Sensor_Id)
      CALL Display_Message( PROGRAM_NAME, &
                            'Error destroying SRFs for '//TRIM(SensorInfo%Sensor_Id), &
                            FAILURE )
      STOP
    END IF
    
    
    ! Destroy the current SensorInfo node in
    ! preparation for the next node retrieval
    ! ---------------------------------------
    Error_Status = Destroy_SensorInfo( SensorInfo )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error destroying SensorInfo data structure.', &
                            Error_status )
      STOP
    END IF

  END DO Sensor_Loop
  
  
  ! Destroy the SensorInfo linked list
  ! ----------------------------------
  Error_Status = Destroy_SensorInfo_List( SensorInfo_List )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying SensorInfo_List.', &
                          FAILURE )
  END IF

END PROGRAM SSU_SRF_ASCII2NC
