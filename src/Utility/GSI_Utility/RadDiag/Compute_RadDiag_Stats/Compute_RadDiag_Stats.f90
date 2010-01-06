
! Program to read the GSI radiance diagnostic output files and
! compute scan and time series statistics.
!
PROGRAM Compute_RadDiag_Stats

  ! Module usage
  USE Type_Kinds             , ONLY: sp=>Single
  USE File_Utility           , ONLY: Get_Lun, File_Exists, Count_Lines_in_File
  USE Message_Handler        , ONLY: SUCCESS, FAILURE, EOF, &
                                     Program_Message, Display_Message
  USE List_File_Utility      , ONLY: Character_List_File_type, &
                                     Read_List_File,           &
                                     Get_List_Size,            &
                                     Get_List_Entry
  USE SensorInfo_Define      , ONLY: SensorInfo_type, &
                                     Allocate_SensorInfo, &
                                     Destroy_SensorInfo
  USE SensorInfo_LinkedList  , ONLY: SensorInfo_List_type, &
                                     GetFrom_SensorInfo_List, &
                                     Count_SensorInfo_Nodes, &
                                     Destroy_SensorInfo_List 
  USE SensorInfo_IO          , ONLY: Read_SensorInfo
  USE SatBias_IO             , ONLY: SatBiasAirMass_type, &
                                     MAX_SATBIAS_PREDICTORS, &
                                     Read_SatBias
  USE RadDiag_IO             , ONLY: RadDiag_Hdr_type,   &
                                     RadDiag_Data_type,  &
                                     RADDIAG_READMODE,   &
                                     RADDIAG_WRITEMODE,  &
                                     RADDIAG_APPENDMODE, &
                                     RadDiag_OpenFile,       &
                                     RadDiag_Hdr_ReadFile,   &
                                     RadDiag_Data_ReadFile
  USE RadDiag_Stats_Define   , ONLY: RadDiag_Stats_type,    &
                                     INVALID_FOV,           &
                                     N_VARIABLES,           &
                                     IBC,                   &
                                     INBC,                  &
                                     ISCAN,                 &
                                     ICONST,                &
                                     IANGLE,                &
                                     ILPSR,                 &
                                     ILPSR2,                &
                                     ICLW,                  &
                                     RadDiag_Stats_Destroy, &
                                     RadDiag_Stats_Create
  USE RadDiag_Stats_netCDF_IO, ONLY: RadDiag_Stats_WriteFile


  ! Disable implicit typing
  IMPLICIT NONE

  ! Parameters
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Compute_RadDiag_Stats'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &
  '$Id$'
  
  CHARACTER(*), PARAMETER :: SENSORINFO_FILE = 'SensorInfo'
  REAL(sp),     PARAMETER :: ZERO = 0.0_sp                   
  REAL(sp),     PARAMETER :: minInverseVariance = 1.0e-06_sp 
  INTEGER,      PARAMETER :: NHEADER = 3                     
  INTEGER,      PARAMETER :: SET = 1


  ! Variables
  CHARACTER(200)  :: Message
  CHARACTER(200)  :: TimeList_Filename
  CHARACTER(200)  :: RadDiag_Filename
  CHARACTER(200)  :: SatBiasAirMass_Filename
  CHARACTER(200)  :: Output_Filename
  CHARACTER(1000) :: Title
  CHARACTER(1000) :: Comment
  INTEGER :: FileID
  INTEGER :: Error_Status
  INTEGER :: Read_Status
  TYPE(SensorInfo_type) :: SensorInfo      
  TYPE(SensorInfo_List_type) :: SensorInfo_List
  INTEGER :: n_Sensors  
  TYPE(Character_List_File_type) :: RadDiag_TimeList
  CHARACTER(200), DIMENSION(NHEADER), TARGET :: RadDiag_TimeList_Hdr
  CHARACTER(200), POINTER :: Sensor_Id             => NULL()
  CHARACTER(200), POINTER :: RadDiag_Prefix        => NULL()
  CHARACTER(200), POINTER :: SatBiasAirMass_Prefix => NULL()
  TYPE(SatBiasAirMass_type), DIMENSION(:), ALLOCATABLE :: SatBiasAirMass
  TYPE(RadDiag_Hdr_type)  :: RadDiag_Hdr
  TYPE(RadDiag_Data_type) :: RadDiag_Data
  TYPE(RadDiag_Stats_type) :: RadDiag_Stats
  INTEGER, DIMENSION(:), ALLOCATABLE :: MatchIndex
  INTEGER :: i, j, k, m, n
  INTEGER :: n_Lines
  INTEGER :: iTime, n_Times
  INTEGER :: n_FOVs
  CHARACTER(10) :: TimeStamp
  REAL(sp) :: rnSamples


  ! Output a program header message
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to read GSI radiance diagnostic (RadDiag) files and '//&
                        'compute various statistics over scan position and time.', &
                        '$Revision$' )


  ! Read the SensorInfo file
  IF ( .NOT. File_Exists(SENSORINFO_FILE) ) THEN
    CALL Display_Message(PROGRAM_NAME, 'SensorInfo file not found', FAILURE)
    STOP
  END IF
  Error_Status = Read_SensorInfo( SENSORINFO_FILE,SensorInfo_List,Quiet=1 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message(PROGRAM_NAME, 'Error reading SensorInfo file '//SENSORINFO_FILE, FAILURE)
    STOP
  END IF
  n_Sensors = Count_SensorInfo_Nodes( SensorInfo_List )
  IF ( n_Sensors < 1 ) THEN
    CALL Display_Message( PROGRAM_NAME, 'SensorInfo_List is empty.', FAILURE )
    STOP
  END IF
  
  
  ! Get the RadDiag List filename
  WRITE( *,FMT='(/5x,"Enter a RadDiag time list filename: ")', ADVANCE='NO' )
  READ( *,'(a)' ) TimeList_Filename
  TimeList_Filename = ADJUSTL(TimeList_Filename)


  ! Read the RadDiag list file
  Error_Status = Read_List_File( TimeList_Filename, RadDiag_TimeList )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message(PROGRAM_NAME, 'Error reading list file '//TRIM(TimeList_Filename), FAILURE)
    STOP
  END IF


  ! Get the RadDiag_Stats output filename
  WRITE( *,FMT='(/5x,"Enter a RadDiag_Stats output filename: ")', ADVANCE='NO' )
  READ( *,'(a)' ) Output_Filename
  Output_Filename = ADJUSTL(Output_Filename)


  ! Get an output file comment
  WRITE( *,'(/5x,"Enter comment string for output file:")' )
  READ( *,'(a)' ) Comment
  Comment = ADJUSTL(Comment)


  ! Determine the number of list time entries
  ! - First entry is the sensor name id,
  ! - Second is the RadDiag file prefix,
  ! - Third is the air mass bias correction file prefix
  ! hence the -3
  n_Times = Get_List_Size( RadDiag_TimeList ) - NHEADER

  ! Read the time list header
  DO i = 1, NHEADER
    Error_Status = Get_List_Entry( RadDiag_TimeList, &
                                   i, &
                                   RadDiag_TimeList_Hdr(i) )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error retrieving header line #", i5, " from RadDiag_TimeList" )' ) i
      CALL Display_Message(PROGRAM_NAME, &
                           TRIM(Message), &
                           Error_Status)
      STOP
    END IF
    RadDiag_TimeList_Hdr(i) = ADJUSTL(RadDiag_TimeList_Hdr(i))
  END DO

  ! Alias the time list header
  Sensor_Id             => RadDiag_TimeList_Hdr(1)
  RadDiag_Prefix        => RadDiag_TimeList_Hdr(2)
  SatBiasAirMass_Prefix => RadDiag_TimeList_Hdr(3)


  ! Search the SensorInfo list for the required sensor attributes
  n_FOVs = -1
  Sensor_Loop: DO n = 1, n_Sensors
    ! Get the current SensorInfo node from the list
    Error_Status = GetFrom_SensorInfo_List( SensorInfo_List,n,SensorInfo )
    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Error retrieving SensorInfo data for sensor # ",i0)' ) n
      CALL Display_Message( PROGRAM_NAME,Message,Error_Status )
      STOP
    END IF
    ! Is it the required sensor?
    IF ( TRIM(Sensor_Id) == TRIM(SensorInfo%Sensor_Id) ) EXIT Sensor_Loop
  END DO Sensor_Loop
  ! Check result
  IF ( TRIM(Sensor_Id) /= TRIM(SensorInfo%Sensor_Id) ) THEN
    CALL Display_Message( PROGRAM_NAME,TRIM(Sensor_Id)//' sensor not found!', FAILURE )
    STOP
  END IF

  

  ! Loop over RadDiag times
  m = 0
  File_Loop: DO iTime = 1, n_Times

    ! Get the current RadDiag filename
    Error_Status = Get_List_Entry( RadDiag_TimeList, &
                                   iTime+NHEADER, &
                                   TimeStamp )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error retrieving timestamp #", i5, " from RadDiag_TimeList" )' ) iTime
      CALL Display_Message(PROGRAM_NAME, TRIM(Message), Error_Status)
      STOP
    END IF

    ! Create the input filenames
    RadDiag_Filename        = TRIM(RadDiag_Prefix)//'.'//TRIM(TimeStamp)
    SatBiasAirMass_Filename = TRIM(SatBiasAirMass_Prefix)//'.'//TRIM(TimeStamp)

    ! Cycle file loop if both files don't exist
    IF ( .NOT. File_Exists(RadDiag_Filename)       .OR. &
         .NOT. File_Exists(SatBiasAirMass_Filename)     ) THEN
      CALL Display_Message(PROGRAM_NAME, 'Files for timestamp '//TRIM(TimeStamp)//' not present', FAILURE)
      CYCLE File_Loop
    END IF

    ! Increment the time/file counter
    m = m + 1

    ! Count the number of lines(channels) in the SatBias AirMass file
    n_Lines = Count_Lines_in_File( SatBiasAirMass_Filename, NoComment='!', NoBlank=SET )
    IF ( n_Lines == 0 ) THEN
      CALL Display_Message(PROGRAM_NAME, 'Error counting lines in '//TRIM(SatBiasAirMass_Filename), FAILURE)
      STOP
    END IF
    
    ! Open the RadDiag file
    Error_Status = RadDiag_OpenFile( RadDiag_Filename, FileID )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message(PROGRAM_NAME, 'Error opening '//TRIM(RadDiag_Filename), FAILURE)
      STOP
    END IF

    ! Read the RadDiag file header
    WRITE(*, '( /5x, "Reading ", a, " header...." )' ) TRIM( RadDiag_Filename )
    Error_Status = RadDiag_Hdr_ReadFile( FileID, RadDiag_Hdr )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message(PROGRAM_NAME, 'Error reading header from '//TRIM(RadDiag_Filename), FAILURE)
      STOP
    END IF

    ! Perform the various allocations
    IF ( iTime == 1 ) THEN
      ALLOCATE( SatBiasAirMass( n_Lines ), MatchIndex(RadDiag_Hdr%n_Channels), STAT=Error_Status )
      IF ( Error_Status /= 0 ) THEN
        CALL Display_Message(PROGRAM_NAME, 'Error allocating SatBiasAirMass structure array', FAILURE)
        STOP
      END IF
      CALL RadDiag_Stats_Create( RadDiag_Stats, &
                                 MAX_SATBIAS_PREDICTORS, &
                                 RadDiag_Hdr%n_Channels, &
                                 SensorInfo%n_FOVs, &
                                 n_Times )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message(PROGRAM_NAME, 'Error allocating RadDiag_Stats arrays', FAILURE)
        STOP
      END IF
      ! Save the channel numbers
      RadDiag_Stats%Channel = RadDiag_Hdr%Channel(:)%nuchan
      ! Fill the Sensor Id values
      RadDiag_Stats%Sensor_Id        = SensorInfo%Sensor_Id
      RadDiag_Stats%WMO_Satellite_ID = SensorInfo%WMO_Satellite_ID
      RadDiag_Stats%WMO_Sensor_ID    = SensorInfo%WMO_Sensor_ID
    END IF

    ! Save the current date/time
    RadDiag_Stats%DateTime(m) = RadDiag_Hdr%Scalar%idate


    ! Read the SatBias AirMass file
    WRITE(*, '( 5x, "Reading ", a, " SatBiasAirMass file...." )' ) TRIM( SatBiasAirMass_Filename )
    Error_Status = Read_SatBias( SatBiasAirMass_Filename, &
                                 SatBiasAirMass )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message(PROGRAM_NAME, 'Error reading '//TRIM(SatBiasAirMass_Filename), FAILURE)
      STOP
    END IF

    ! Assign the SatBias AirMass data into the RadDiag_Stats structure
    n = COUNT( SatBiasAirMass%Sensor_Id == Sensor_Id )
    IF ( n /= RadDiag_Hdr%n_Channels ) THEN
      CALL Display_Message(PROGRAM_NAME, 'Channel count mismatch in SatBias entries', FAILURE)
      STOP
    END IF
    MatchIndex = PACK( (/ (n,n=1,n_Lines) /), SatBiasAirMass%Sensor_Id == Sensor_Id )
    DO n=1,RadDiag_Hdr%n_Channels
      RadDiag_Stats%AirMassCoefficients(:,n,iTime) = SatBiasAirMass(MatchIndex(n))%c
    END DO


    ! Initialise file data record counter
    n = 0

    ! Loop to read RadDiag data until end-of-file
    WRITE(*, '( 5x, "Begin ", a, " data read/summation loop...." )' ) TRIM( RadDiag_Filename )
    Read_Loop: DO

      ! Read the current entry
      Read_Status = RadDiag_Data_ReadFile( FileID, RadDiag_Hdr, RadDiag_Data )
      SELECT CASE (Read_Status)
        CASE (EOF)
          EXIT Read_Loop
        CASE (FAILURE)
          CALL Display_Message(PROGRAM_NAME, 'Error reading data from '//TRIM( RadDiag_Filename ), FAILURE)
          EXIT Read_Loop !STOP
        CASE DEFAULT
          ! Success or warning: do nothing
      END SELECT

      ! Increment data record counter
      n = n + 1

      ! Save the current scan position
      k = RadDiag_Data%Scalar%senscn_pos
      RadDiag_Stats%FOV(k) = k

      ! Loop over channels to sum data
      Channel_Loop: DO j = 1, RadDiag_Data%n_Channels
        IF ( RadDiag_Data%Channel(j)%errinv > minInverseVariance ) THEN

          ! Scan position summation
          RadDiag_Stats%scan_Data(iBC,j,k)    = RadDiag_Stats%scan_Data(iBC,j,k)    + RadDiag_Data%Channel(j)%omgbc 
          RadDiag_Stats%scan_Data(iNBC,j,k)   = RadDiag_Stats%scan_Data(iNBC,j,k)   + RadDiag_Data%Channel(j)%omgnbc
          RadDiag_Stats%scan_Data(iScan,j,k)  = RadDiag_Stats%scan_Data(iScan,j,k)  + RadDiag_Data%Channel(j)%bifix
          RadDiag_Stats%scan_Data(iConst,j,k) = RadDiag_Stats%scan_Data(iConst,j,k) + RadDiag_Data%Channel(j)%bicons
          RadDiag_Stats%scan_Data(iAngle,j,k) = RadDiag_Stats%scan_Data(iAngle,j,k) + RadDiag_Data%Channel(j)%biang
          RadDiag_Stats%scan_Data(iLpsR ,j,k) = RadDiag_Stats%scan_Data(iLpsR ,j,k) + RadDiag_Data%Channel(j)%bilap
          RadDiag_Stats%scan_Data(iLpsR2,j,k) = RadDiag_Stats%scan_Data(iLpsR2,j,k) + RadDiag_Data%Channel(j)%bilap2
          RadDiag_Stats%scan_Data(iCLW  ,j,k) = RadDiag_Stats%scan_Data(iCLW  ,j,k) + RadDiag_Data%Channel(j)%biclw
          RadDiag_Stats%scan_nSamples(j,k)    = RadDiag_Stats%scan_nSamples(j,k) + 1

          ! Timeseries summation
          RadDiag_Stats%time_Data(iBC,j,m)    = RadDiag_Stats%time_Data(iBC,j,m)    + RadDiag_Data%Channel(j)%omgbc 
          RadDiag_Stats%time_Data(iNBC,j,m)   = RadDiag_Stats%time_Data(iNBC,j,m)   + RadDiag_Data%Channel(j)%omgnbc
          RadDiag_Stats%time_Data(iScan,j,m)  = RadDiag_Stats%time_Data(iScan,j,m)  + RadDiag_Data%Channel(j)%bifix
          RadDiag_Stats%time_Data(iConst,j,m) = RadDiag_Stats%time_Data(iConst,j,m) + RadDiag_Data%Channel(j)%bicons
          RadDiag_Stats%time_Data(iAngle,j,m) = RadDiag_Stats%time_Data(iAngle,j,m) + RadDiag_Data%Channel(j)%biang
          RadDiag_Stats%time_Data(iLpsR ,j,m) = RadDiag_Stats%time_Data(iLpsR ,j,m) + RadDiag_Data%Channel(j)%bilap
          RadDiag_Stats%time_Data(iLpsR2,j,m) = RadDiag_Stats%time_Data(iLpsR2,j,m) + RadDiag_Data%Channel(j)%bilap2
          RadDiag_Stats%time_Data(iCLW  ,j,m) = RadDiag_Stats%time_Data(iCLW  ,j,m) + RadDiag_Data%Channel(j)%biclw
          RadDiag_Stats%time_nSamples(j,m)    = RadDiag_Stats%time_nSamples(j,m) + 1

        END IF
      END DO Channel_Loop
    END DO Read_Loop
    WRITE(*, '( 5x, "Number of records read: ", i7 )' ) n
  END DO File_Loop

  ! Replace the RadDiag_Stats nTimes dimension with
  ! the actual number of times/files read
  RadDiag_Stats%n_Times = m

  ! Compute the scan averages
  WRITE(*, '( /5x, "Computing scan averages...." )' )
  FOV_Loop: DO k = 1, RadDiag_Stats%n_FOVS

    ! If FOV not used, go to next one
    IF ( RadDiag_Stats%FOV(k) == INVALID_FOV ) CYCLE FOV_Loop

    ! Loop over channels to compute averages
    DO j = 1, RadDiag_Stats%n_Channels
      rnSamples = REAL(RadDiag_Stats%scan_nSamples(j,k),sp)
      DO i = 1, RadDiag_Stats%n_Variables
        RadDiag_Stats%scan_Data(i,j,k) = RadDiag_Stats%scan_Data(i,j,k)/rnSamples
      END DO
    END DO
  END DO FOV_Loop


  ! Compute the timeseries averages
  WRITE(*, '( /5x, "Computing timeseries averages...." )' )
  DateTime_Loop: DO m = 1, RadDiag_Stats%n_Times

    ! Loop over channels to compute averages
    DO j = 1, RadDiag_Stats%n_Channels
      rnSamples = REAL(RadDiag_Stats%time_nSamples(j,m),sp)
      DO i = 1, RadDiag_Stats%n_Variables
        RadDiag_Stats%time_Data(i,j,m) = RadDiag_Stats%time_Data(i,j,m)/rnSamples
      END DO
    END DO
  END DO DateTime_Loop


  ! Write averages to file
  WRITE(Title,'("RadDiag Stats for ", a, " from ", i10, " to ", i10 )' ) &
              TRIM(Sensor_Id), &
              RadDiag_Stats%DateTime(1), &
              RadDiag_Stats%DateTime(RadDiag_Stats%n_Times)
  WRITE(*, '( /5x, "Writing output file ", a, "...." )' ) TRIM(Output_Filename)
  
  Error_Status = RadDiag_Stats_WriteFile( TRIM(Output_Filename), &
                                          RadDiag_Stats, &
                                          Title   = TRIM(Title), &
                                          History = PROGRAM_VERSION_ID, &
                                          Comment = TRIM(Comment) )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message(PROGRAM_NAME, 'Error writing output file '//TRIM(Output_Filename), Error_Status)
    STOP
  END IF


  ! Cleanup
  DEALLOCATE( SatBiasAirMass, MatchIndex )
  CALL RadDiag_Stats_Destroy( RadDiag_Stats )
  Error_Status = Destroy_SensorInfo( SensorInfo )
  Error_Status = Destroy_SensorInfo_List( SensorInfo_List )

END PROGRAM Compute_RadDiag_Stats
