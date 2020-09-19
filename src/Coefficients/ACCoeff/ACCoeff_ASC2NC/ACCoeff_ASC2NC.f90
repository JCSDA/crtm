!
! ACCoeff_ASC2NC
!
! Program to read the AAPP AMSU-A/AMSU-B/MHS antenna correction
! ASCII files and output the normalised antenna efficiencies to
! a netCDF format ACCoeff file.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 28-Jun-2007
!                       paul.vandelst@noaa.gov
!

PROGRAM ACCoeff_ASC2NC

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                   Display_Message, Program_Message
  USE File_Utility         , ONLY: Get_Lun, File_Exists
  USE SensorInfo_Define    , ONLY: SensorInfo_type, &
                                   Destroy_SensorInfo
  USE SensorInfo_LinkedList, ONLY: SensorInfo_List_type, &
                                   Count_SensorInfo_Nodes, &
                                   GetFrom_SensorInfo_List, &
                                   Destroy_SensorInfo_List
  USE SensorInfo_IO        , ONLY: Read_SensorInfo
  USE ACCoeff_Define       , ONLY: ACCoeff_type, &
                                   ACCoeff_Associated, &
                                   ACCoeff_Create, &
                                   ACCoeff_Destroy
  USE ACCoeff_IO           , ONLY: ACCoeff_WriteFile
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*),  PARAMETER :: PROGRAM_NAME   = 'ACCoeff_ASC2NC'
  CHARACTER(*),  PARAMETER :: PROGRAM_VERSION_ID = &
  ! The sensorinfo file name
  CHARACTER(*), PARAMETER :: SENSORINFO_FILENAME = 'SensorInfo'
  ! The sensors
  INTEGER     , PARAMETER :: N_AC_SENSORS = 14
  CHARACTER(*), PARAMETER :: SENSOR_ID(N_AC_SENSORS) = &
    (/ 'amsua_n15    ', &
       'amsua_n16    ', &
       'amsua_n17    ', &
       'amsua_n18    ', &
       'amsua_metop-a', &
       'amsua_metop-b', &
       'amsub_n15    ', &
       'amsub_n16    ', &
       'amsub_n17    ', &
       'mhs_n18      ', &
       'mhs_metop-a  ', &
       'mhs_metop-b  ', &
       'amsua_n19    ', &
       'mhs_n19      ' /)
  INTEGER, PARAMETER :: AMSUA_WMO_SENSOR_ID = 570
  INTEGER, PARAMETER :: AMSUB_WMO_SENSOR_ID = 574
  INTEGER, PARAMETER :: MHS_WMO_SENSOR_ID   = 203
  CHARACTER(*), PARAMETER :: AMSUA_REFERENCE = &
    'T. Mo, (1999), "AMSU-A Antenna Pattern Corrections", IEEE Transactions '//&
    'on Geoscience and Remote Sensing, Vol.37, pp.103-112'
  CHARACTER(*), PARAMETER :: AMSUBMHS_REFERENCE = &
    'Hewison, T.J. and R.Saunders (1996), "Measurements of the AMSU-B '//&
    'antenna pattern", IEEE Transactions on Geoscience and Remote Sensing, Vol.34, pp.405-412'
  ! The FDF identifiers used to construct filenames
  INTEGER     , PARAMETER :: N_FDF_IDS = 3
  CHARACTER(*), PARAMETER :: FDF_ID(N_FDF_IDS) = &
    (/ '6.4', &  ! AAPP version number
       '6.8', &  ! AAPP version number
       '1.1' /)  ! Data version number
  ! The FDF version id used in the ACCoeff files
  INTEGER     , PARAMETER :: FDF_VERSION(N_FDF_IDS) = &
    (/ 4, &
       5, &
       6 /)


  ! ---------  
  ! Variables
  ! ---------  
  CHARACTER(256) :: Message
  CHARACTER(256) :: ASC_Filename, NC_Filename
  CHARACTER(500) :: Title
  CHARACTER(500) :: Comment
  CHARACTER(500) :: Reference
  INTEGER :: idx_FDF_Id
  INTEGER :: Error_Status, IO_Status
  INTEGER :: l, n, FileID
  INTEGER :: n_Sensors, n_FOVs, n_Channels
  TYPE(SensorInfo_type)      :: SensorInfo
  TYPE(SensorInfo_List_type) :: SensorInfo_List
  TYPE(ACCoeff_type) :: AC
  
  
  ! Output header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to read the AAPP-format AMSU-A/AMSU-B/MHS antenna '//&
                        'correction ASCII files and output the normalised antenna '//&
                        'efficiencies to a netCDF format ACCoeff file.', &
                        '$Revision$' )


  ! Read the SensorInfo file
  Error_Status = Read_SensorInfo( SENSORINFO_FILENAME, &
                                  SensorInfo_List    , &
                                  Quiet=1             )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error reading SensorInfo file '//SENSORINFO_FILENAME
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE ); STOP
  END IF
  ! ...Get the number of sensors read in
  n_Sensors = Count_SensorInfo_Nodes( SensorInfo_List )
  IF ( n_Sensors < 1 ) THEN
    CALL Display_Message( PROGRAM_NAME, 'SensorInfo_List is empty.', FAILURE ); STOP
  END IF


  ! Get the AAPP data file identifier
  WRITE( *,'(/5x,"Select the data file identifier: ")' )
  DO n = 1, N_FDF_IDS
    WRITE( *,'(7x,i0,") ",a)' ) n, FDF_ID(n)
  END DO
  WRITE( *,'(5x,"Enter choice: ")',ADVANCE='NO' )
  READ( *,'(i10)' ) idx_FDF_Id
  IF ( idx_FDF_Id < 1 .OR. idx_FDF_Id > N_FDF_IDS ) THEN
    Message = 'Unrecognised data file identifier selection'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE ); STOP
  END IF

    
  ! Begin loop over sensors
  Sensor_Loop: DO n = 1, n_Sensors


    ! Get the current SensorInfo data from the list
    Error_Status = GetFrom_SensorInfo_List( SensorInfo_List, n, SensorInfo )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message,'("Error retrieving SensorInfo data for sensor # ",i0)' ) n
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE ); STOP
    END IF


    ! Only operate on the specified microwave sensors
    IF ( .NOT. ANY(SENSOR_ID == SensorInfo%Sensor_Id) ) CYCLE Sensor_Loop
    WRITE(*,'(/5x,"Converting antenna correction data for ",a)') TRIM(SensorInfo%Sensor_Id)

    
    ! Construct filenames and determine if they exist
    ASC_Filename = TRIM(SensorInfo%Sensor_Id)//'.fdf_v'//TRIM(FDF_ID(idx_FDF_Id))
    NC_Filename  = TRIM(SensorInfo%Sensor_Id)//'.ACCoeff.nc'
    IF ( .NOT. File_Exists(ASC_Filename) ) THEN
      Message = 'ASCII file '//TRIM(ASC_Filename)//' not found. Skipping...'
      CALL Display_Message( PROGRAM_NAME, Message, INFORMATION )
      CYCLE Sensor_Loop
    END IF
    

    ! Read the ASCII file
    ! ...Open it
    FileID = Get_Lun()
    OPEN( FileID, FILE   = ASC_Filename, &
                  STATUS = 'OLD', &
                  FORM   = 'FORMATTED', &
                  ACCESS = 'SEQUENTIAL', &
                  IOSTAT = IO_Status )
    IF ( IO_Status /= 0 ) THEN
      Message = 'Error opening ASCII file '//TRIM(ASC_Filename)
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE ); STOP
    END IF
    ! ...Read initial comment
    READ( FileID,'(a)' ) Title
    Title = Title(2:)
    ! ...Read the dimensions
    READ( FileID,'(2i5)' ) n_FOVs, n_Channels
    ! ...Allocate the ACCoeff structure
    CALL ACCoeff_Create( AC, n_FOVs, n_Channels )
    IF ( .NOT. ACCoeff_Associated( AC ) ) THEN
      Message = 'Error allocating ACCoeff structure for '//TRIM(SensorInfo%Sensor_Id)
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE ); STOP
    END IF
    ! ...Begin channel loop
    DO l = 1, n_Channels
      ! ...Read the channel number
      READ( FileID,'(i5)' ) AC%Sensor_Channel(l)
      ! ...Read the antenna efficiencies
      READ( FileID,'(10f8.6)' ) AC%A_earth(:,l)
      READ( FileID,'(10f8.6)' ) AC%A_platform(:,l)
      READ( FileID,'(10f8.6)' ) AC%A_space(:,l)
    END DO
    CLOSE(FileID)
    

    ! Assign the version
    AC%Version = FDF_VERSION(idx_FDF_Id)

    
    ! Fill the Sensor id info
    AC%Sensor_Id        = TRIM(SensorInfo%Sensor_Id)
    AC%WMO_Satellite_Id = SensorInfo%WMO_Satellite_Id
    AC%WMO_Sensor_Id    = SensorInfo%WMO_Sensor_Id


    ! Construct a file comment
    Comment = 'Created from ASCII file '//TRIM(ASC_Filename)
    
  
    ! Set the data reference
    SELECT CASE (AC%Sensor_Id(1:INDEX(AC%Sensor_Id,'_')-1))
      CASE('amsua')
        Reference = AMSUA_REFERENCE
      CASE('amsub','mhs')
        Reference = AMSUBMHS_REFERENCE
      CASE DEFAULT
        Message = 'Data reference select case failed for '//TRIM(SensorInfo%Sensor_Id)
        CALL Display_Message( PROGRAM_NAME, Message, FAILURE ); STOP
    END SELECT
    
    
    ! Write the netCDF file
    Error_Status = ACCoeff_WriteFile( NC_Filename, &
                                      AC, &
                                      netCDF = .TRUE., &
                                      Title = TRIM(Title), &
                                      History = PROGRAM_VERSION_ID, &
                                      Comment = TRIM(Comment)//'; Reference: '//TRIM(Reference) )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing to '//TRIM(NC_Filename)
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE ); STOP
    END IF
    
    
    ! Destroy the ACCoeff structure
    CALL ACCoeff_Destroy( AC )
    IF ( ACCoeff_Associated( AC ) ) THEN
      Message = 'Error destroying ACCoeff structure used for '//TRIM(SensorInfo%Sensor_Id)
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE ); STOP
    END IF

    
    ! Destroy the SensorInfo structure
    Error_Status = Destroy_SensorInfo( SensorInfo )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error destroying SensorInfo data structure.'
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE ); STOP
    END IF
    
  END DO Sensor_Loop
  
  
  ! Destroy the SensorInfo linked list
  Error_Status = Destroy_SensorInfo_List( SensorInfo_List )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying SensorInfo linked list.', &
                          WARNING )
  END IF

END PROGRAM ACCoeff_ASC2NC
