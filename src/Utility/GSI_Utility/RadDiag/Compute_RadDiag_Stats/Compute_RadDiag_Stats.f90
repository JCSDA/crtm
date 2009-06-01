
! Program to read the GSI radiance diagnostic output files and
! compute scan and time series statistics.
!
PROGRAM Compute_RadDiag_Stats

  ! Module usage
  USE Type_Kinds,              ONLY: sp=>Single
  USE File_Utility,            ONLY: Get_Lun, File_Exists, Count_Lines_in_File
  USE Message_Handler,         ONLY: SUCCESS, FAILURE, EOF, &
                                     Program_Message, Display_Message
  USE List_File_Utility,       ONLY: Character_List_File_type, &
                                     Read_List_File,           &
                                     Get_List_Size,            &
                                     Get_List_Entry
  USE Sensor_Utility,          ONLY: Get_SensorAttributes
  USE SatBias_IO,              ONLY: SatBiasAirMass_type, &
                                     MAX_SATBIAS_PREDICTORS, &
                                     Read_SatBias
  USE RadDiag_IO,              ONLY: RadDiag_Hdr_type,   &
                                     RadDiag_Data_type,  &
                                     RADDIAG_READMODE,   &
                                     RADDIAG_WRITEMODE,  &
                                     RADDIAG_APPENDMODE, &
                                     Open_RadDiag,       &
                                     Read_RadDiag_Hdr,   &
                                     Read_RadDiag_Data
  USE RadDiag_Stats_Define,    ONLY: RadDiag_Stats_type,    &
                                     invalidFOV,            &
                                     nVariables,            &
                                     iBC,                   &
                                     iNBC,                  &
                                     iScan,                 &
                                     iConst,                &
                                     iAngle,                &
                                     iLpsR,                 &
                                     iLpsR2,                &
                                     iCLW,                  &
                                     Destroy_RadDiag_Stats, &
                                     Allocate_RadDiag_Stats
  USE RadDiag_Stats_netCDF_IO, ONLY: Write_RadDiag_Stats_netCDF


  ! Disable implicit typing
  IMPLICIT NONE

  ! Parameters
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'RadDiag_Stats'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'
  INTEGER,  PARAMETER :: SET = 1
  REAL(sp), PARAMETER :: ZERO = 0.0_sp
  REAL(sp), PARAMETER :: minInverseVariance = 1.0e-06_sp
  INTEGER,  PARAMETER :: NHEADER = 3


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
  INTEGER :: nLines
  INTEGER :: iTime, nTimes
  INTEGER :: nFOVs
  CHARACTER(10) :: TimeStamp
  CHARACTER(80) :: Sensor_Name
  CHARACTER(80) :: Platform_Name
  REAL(sp) :: rnSamples



  ! Output a program header message
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to read GSI radiance diagnostic (RadDiag) files and '//&
                        'compute various statistics over scan position and time.', &
                        '$Revision$' )

  ! Get the RadDiag List filename
  WRITE( *, FMT = '( /5x, "Enter a RadDiag time list filename: " )', ADVANCE = 'NO' )
  READ( *, '( a )' ) TimeList_Filename
  TimeList_Filename = ADJUSTL(TimeList_Filename)

  ! Read the RadDiag list file
  Error_Status = Read_List_File( TimeList_Filename, &
                                 RadDiag_TimeList )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message(PROGRAM_NAME, 'Error reading list file '//TRIM(TimeList_Filename), Error_Status)
    STOP
  END IF

  ! Get the RadDiag_Stats output filename
  WRITE( *, FMT = '( /5x, "Enter a RadDiag_Stats output filename: " )', ADVANCE = 'NO' )
  READ( *, '( a )' ) Output_Filename
  Output_Filename = ADJUSTL(Output_Filename)

  ! Get an output file comment
  WRITE( *, '( /5x, "Enter Comment string for output file:" )' )
  READ( *, '( a )' ) Comment
  Comment = ADJUSTL(Comment)

  ! Determine the number of list time entries
  ! - First entry is the sensor name id,
  ! - Second is the RadDiag file prefix,
  ! - Third is the air mass bias correction file prefix
  ! hence the -3
  nTimes = Get_List_Size( RadDiag_TimeList ) - NHEADER

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

  ! Get the sensor attributes
  CALL Get_SensorAttributes(Sensor_Id, &
                            nFOVs=nFOVs, &
                            Sensor_Name=Sensor_Name, &
                            Platform_Name=Platform_Name)

  ! Loop over RadDiag times
  m = 0
  File_Loop: DO iTime = 1, nTimes

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
    nLines = Count_Lines_in_File( SatBiasAirMass_Filename, NoComment='!', NoBlank=SET )
    IF ( nLines == 0 ) THEN
      CALL Display_Message(PROGRAM_NAME, 'Error counting lines in '//TRIM(SatBiasAirMass_Filename), FAILURE)
      STOP
    END IF
    
    ! Open the RadDiag file
    Error_Status = Open_RadDiag( RadDiag_Filename, FileID )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message(PROGRAM_NAME, 'Error opening '//TRIM(RadDiag_Filename), FAILURE)
      STOP
    END IF

    ! Read the RadDiag file header
    WRITE(*, '( /5x, "Reading ", a, " header...." )' ) TRIM( RadDiag_Filename )
    Error_Status = Read_RadDiag_Hdr( RadDiag_Filename, FileID, RadDiag_Hdr )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message(PROGRAM_NAME, 'Error reading header from '//TRIM(RadDiag_Filename), FAILURE)
      STOP
    END IF

    ! Perform the various allocations
    IF ( iTime == 1 ) THEN
      ALLOCATE( SatBiasAirMass( nLines ), MatchIndex(RadDiag_Hdr%nChannels), STAT=Error_Status )
      IF ( Error_Status /= 0 ) THEN
        CALL Display_Message(PROGRAM_NAME, 'Error allocating SatBiasAirMass structure array', FAILURE)
        STOP
      END IF
      Error_Status = Allocate_RadDiag_Stats( MAX_SATBIAS_PREDICTORS, &
                                             RadDiag_Hdr%nChannels, &
                                             nFOVs, &
                                             nTimes, &
                                             nVariables, &
                                             RadDiag_Stats)
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message(PROGRAM_NAME, 'Error allocating RadDiag_Stats arrays', FAILURE)
        STOP
      END IF
      ! Save the channel numbers
      RadDiag_Stats%Channel = RadDiag_Hdr%Channel(:)%nuchan
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
    IF ( n /= RadDiag_Hdr%nChannels ) THEN
      CALL Display_Message(PROGRAM_NAME, 'Channel count mismatch in SatBias entries', FAILURE)
      STOP
    END IF
    MatchIndex = PACK( (/ (n,n=1,nLines) /), SatBiasAirMass%Sensor_Id == Sensor_Id )
    DO n=1,RadDiag_Hdr%nChannels
      RadDiag_Stats%AirMassCoefficients(:,n,iTime) = SatBiasAirMass(MatchIndex(n))%c
    END DO


    ! Initialise file data record counter
    n = 0

    ! Loop to read RadDiag data until end-of-file
    WRITE(*, '( 5x, "Begin ", a, " data read/summation loop...." )' ) TRIM( RadDiag_Filename )
    Read_Loop: DO

      ! Read the current entry
      Read_Status = Read_RadDiag_Data( RadDiag_Filename, FileID, RadDiag_Hdr, RadDiag_Data )
      SELECT CASE (Read_Status)
        CASE (EOF)
          EXIT Read_Loop
        CASE (FAILURE)
          CALL Display_Message(PROGRAM_NAME, 'Error reading data from '//TRIM( RadDiag_Filename ), FAILURE)
          STOP
        CASE DEFAULT
          ! Success or warning: do nothing
      END SELECT

      ! Increment data record counter
      n = n + 1

      ! Save the current scan position
      k = RadDiag_Data%Scalar%senscn_pos
      RadDiag_Stats%FOV(k) = k

      ! Loop over channels to sum data
      Channel_Loop: DO j = 1, RadDiag_Data%nChannels
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
          RadDiag_Stats%scan_nSamples(j,k)       = RadDiag_Stats%scan_nSamples(j,k) + 1

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
  RadDiag_Stats%nTimes = m

  ! Compute the scan averages
  WRITE(*, '( /5x, "Computing scan averages...." )' )
  FOV_Loop: DO k = 1, RadDiag_Stats%nFOVS

    ! If FOV not used, go to next one
    IF ( RadDiag_Stats%FOV(k) == invalidFOV ) CYCLE FOV_Loop

    ! Loop over channels to compute averages
    DO j = 1, RadDiag_Stats%nChannels
      rnSamples = REAL(RadDiag_Stats%scan_nSamples(j,k),sp)
      DO i = 1, RadDiag_Stats%nVariables
        RadDiag_Stats%scan_Data(i,j,k) = RadDiag_Stats%scan_Data(i,j,k)/rnSamples
      END DO
    END DO
  END DO FOV_Loop


  ! Compute the timeseries averages
  WRITE(*, '( /5x, "Computing timeseries averages...." )' )
  DateTime_Loop: DO m = 1, RadDiag_Stats%nTimes

    ! Loop over channels to compute averages
    DO j = 1, RadDiag_Stats%nChannels
      rnSamples = REAL(RadDiag_Stats%time_nSamples(j,m),sp)
      DO i = 1, RadDiag_Stats%nVariables
        RadDiag_Stats%time_Data(i,j,m) = RadDiag_Stats%time_Data(i,j,m)/rnSamples
      END DO
    END DO
  END DO DateTime_Loop


  ! Write averages to file
  WRITE(Title,'("RadDiag Stats for ", a, " from ", i10, " to ", i10 )' ) &
              TRIM(Sensor_Id), &
              RadDiag_Stats%DateTime(1), &
              RadDiag_Stats%DateTime(RadDiag_Stats%nTimes)
  WRITE(*, '( /5x, "Writing output file ", a, "...." )' ) TRIM(Output_Filename)
  
  Error_Status = Write_RadDiag_Stats_netCDF( TRIM(Output_Filename), &
                                             RadDiag_Stats, &
                                             Title        =TRIM(Title), &
                                             History      =PROGRAM_RCS_ID, &
                                             Sensor_Name  =TRIM(Sensor_Name), &
                                             Platform_Name=TRIM(Platform_Name), &
                                             Comment      =TRIM(Comment) )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message(PROGRAM_NAME, 'Error writing output file '//TRIM(Output_Filename), Error_Status)
    STOP
  END IF


  ! Deallocate the arrays
  DEALLOCATE( SatBiasAirMass, MatchIndex )
  Error_Status = Destroy_RadDiag_Stats( RadDiag_Stats )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message(PROGRAM_NAME, 'Error deallocating RadDiag_Stats structure', Error_Status)
    STOP
  END IF

END PROGRAM Compute_RadDiag_Stats
