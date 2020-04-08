!
! AAPP_AMSU_AntCorr_ASC2NC
!
! Program to read the AAPP AMSU-A/AMSU-B/MHS antenna correction
! ASCII files and output the normalised antenna efficiencies to
! a netCDF format AntCorr file.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 28-Jun-2007
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM AAPP_AMSU_AntCorr_ASC2NC

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
  USE AntCorr_Define       , ONLY: AntCorr_type, &
                                   Allocate_AntCorr, &
                                   Destroy_AntCorr
  USE AntCorr_netCDF_IO    , ONLY: Write_AntCorr_netCDF
  ! Disable all implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*),  PARAMETER :: PROGRAM_NAME   = 'AAPP_AMSU_AntCorr_ASC2NC'
  CHARACTER(*),  PARAMETER :: PROGRAM_RCS_ID = &
  INTEGER, PARAMETER :: SET = 1
  ! The sensors
  INTEGER, PARAMETER :: N_AC_SENSORS = 12
  CHARACTER(*), PARAMETER :: SENSOR_ID(N_AC_SENSORS) = &
    (/ 'amsua_n15    ', &
       'amsua_n16    ', &
       'amsua_n17    ', &
       'amsua_n18    ', &
       'amsua_metop-a', &
       'amsub_n15    ', &
       'amsub_n16    ', &
       'amsub_n17    ', &
       'mhs_n18      ', &
       'mhs_metop-a  ', &
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


  ! ---------  
  ! Variables
  ! ---------  
  CHARACTER(256) :: Message
  CHARACTER(256) :: SensorInfo_Filename
  CHARACTER(256) :: ASC_Filename, NC_Filename
  CHARACTER(500) :: Title
  CHARACTER(500) :: Comment
  CHARACTER(500) :: Reference
  CHARACTER(256) :: FDF_Id
  INTEGER :: Error_Status, IO_Status
  INTEGER :: Version
  INTEGER :: l, n, FileID
  INTEGER :: n_Sensors, n_FOVs, n_Channels
  TYPE(SensorInfo_type)      :: SensorInfo
  TYPE(SensorInfo_List_type) :: SensorInfo_List
  TYPE(AntCorr_type) :: AC
  
  
  ! Output header
  ! -------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to read the AAPP AMSU-A/AMSU-B/MHS antenna correction '//&
                        'ASCII files and output the normalised antenna efficiencies '//&
                        'to a netCDF format AntCorr file.', &
                        '$Revision$' )


  ! Get user inputs
  ! ---------------
  ! The SensorInfo filename and data
  WRITE( *,FMT='(/5x,"Enter a SensorInfo filename: ")',ADVANCE='NO' )
  READ( *,FMT='(a)' ) SensorInfo_Filename
  SensorInfo_Filename = ADJUSTL(SensorInfo_Filename)

  Error_Status = Read_SensorInfo( SensorInfo_Filename, &
                                  SensorInfo_List    , &
                                  Quiet=SET            )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading SensorInfo file '//&
                          TRIM(SensorInfo_Filename), &
                          FAILURE )
    STOP
  END IF

  n_Sensors = Count_SensorInfo_Nodes( SensorInfo_List )
  IF ( n_Sensors < 1 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'SensorInfo_List is empty.', &
                          FAILURE )
    STOP
  END IF

  ! Enter the AAPP data file name
  WRITE( *,'(/5x,"Enter the AAPP fdf identifier: ")', ADVANCE='NO' )
  READ( *,'(a)' ) FDF_Id
  FDF_Id = ADJUSTL(FDF_Id)
  
  ! Assign a data version number based on the FDF id
  ! (NOTE: NESDIS original data is considered v1.)
  SELECT CASE (TRIM(FDF_Id))
    CASE ('')
      Version = 2
    CASE ('_halw')
      Version = 3
    CASE ('_v6.4')
      Version = 4
    CASE ('_v6.8')
      Version = 5
    CASE DEFAULT
      CALL Display_Message( PROGRAM_NAME, &
                            'Unrecognised FDF Id, '//TRIM(FDF_Id), &
                            FAILURE )
      STOP
  END SELECT
    
  ! Enter an output comment attribute
  WRITE( *,'(/5x,"Enter a file comment: ")' )
  READ( *,'(a)' ) Comment
  Comment = ADJUSTL(Comment)
  

  ! Begin loop over sensors
  ! -----------------------
  Sensor_Loop: DO n = 1, n_Sensors


    ! Get the current SensorInfo data from the list
    ! ---------------------------------------------
    Error_Status = GetFrom_SensorInfo_List( SensorInfo_List, &
                                            n, &
                                            SensorInfo )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message,'("Error retrieving SensorInfo data for sensor # ",i0)' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            FAILURE )
      STOP
    END IF


    ! Only operate on the specified microwave sensors
    ! -----------------------------------------------
    IF ( .NOT. ANY(SENSOR_ID == SensorInfo%Sensor_Id) ) CYCLE Sensor_Loop


    ! Begin conversion...
    ! -------------------
    WRITE(*,'(/5x,"Converting antenna correction data for ",a)') TRIM(SensorInfo%Sensor_Id)
    
    ! Construct filenames and determine if they exist
    ! -----------------------------------------------
    ASC_Filename = TRIM(SensorInfo%Sensor_Id)//'.fdf'//TRIM(FDF_Id)
    NC_Filename  = TRIM(SensorInfo%Sensor_Id)//'.AntCorr.nc'

    IF ( .NOT. File_Exists(ASC_Filename) ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'ASCII file '//TRIM(ASC_Filename)//' not found. Skipping...', &
                            INFORMATION )
      CYCLE Sensor_Loop
    END IF
    

    ! Read the ASCII data
    ! -------------------
    ! Open ASCII file
    FileID = Get_Lun()
    OPEN( FileID, FILE   = ASC_Filename, &
                  STATUS = 'OLD', &
                  FORM   = 'FORMATTED', &
                  ACCESS = 'SEQUENTIAL', &
                  IOSTAT = IO_Status )
    IF ( IO_Status /= 0 ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error opening ASCII file '//TRIM(ASC_Filename), &
                            FAILURE )
      STOP
    END IF

    ! Read initial comment
    READ( FileID,'(a)' ) Title
    Title = Title(2:)
    
    ! Read the dimensions
    READ( FileID,'(2i5)' ) n_FOVs, n_Channels
    
    ! Allocate the AntCorr structure
    Error_Status = Allocate_AntCorr( n_FOVs, n_Channels, AC )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error allocating AntCorr structure for '//TRIM(SensorInfo%Sensor_Id), &
                            FAILURE )
      STOP
    END IF

    ! Assign the version
    AC%Version = Version
    
    ! Begin channel loop
    DO l = 1, n_Channels
    
      ! Read the channel number
      READ( FileID,'(i5)' ) AC%Sensor_Channel(l)
      
      ! Read the antenna efficiencies
      READ( FileID,'(10f8.6)' ) AC%A_earth(:,l)
      READ( FileID,'(10f8.6)' ) AC%A_platform(:,l)
      READ( FileID,'(10f8.6)' ) AC%A_space(:,l)
    END DO
    
    
    ! Fill the Sensor id info
    ! -----------------------
    AC%Sensor_Id        = TRIM(SensorInfo%Sensor_Id)
    AC%WMO_Satellite_Id = SensorInfo%WMO_Satellite_Id
    AC%WMO_Sensor_Id    = SensorInfo%WMO_Sensor_Id

    CLOSE(FileID)


    ! Set the data reference
    ! ----------------------
    SELECT CASE (AC%Sensor_Id(1:INDEX(AC%Sensor_Id,'_')-1))
      CASE('amsua')
        Reference = AMSUA_REFERENCE
      CASE('amsub','mhs')
        Reference = AMSUBMHS_REFERENCE
      CASE DEFAULT
        CALL Display_Message( PROGRAM_NAME, &
                              'Data reference select case failed for '//TRIM(SensorInfo%Sensor_Id), &
                              FAILURE )
        STOP
    END SELECT
    
    
    ! Write the netCDF file
    ! ---------------------
    Error_Status = Write_AntCorr_netCDF( NC_Filename, &
                                         AC, &
                                         Title = TRIM(Title), &
                                         History = PROGRAM_RCS_ID, &
                                         Comment = TRIM(Comment)//&
                                                   '; Reference: '//TRIM(Reference) )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error writing to '//TRIM(NC_Filename), &
                            FAILURE )
      STOP
    END IF
    
    
    ! Destroy the AntCorr structure
    ! -----------------------------
    Error_Status = Destroy_AntCorr( AC )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error destroying AntCorr structure used for '//TRIM(SensorInfo%Sensor_Id), &
                            FAILURE )
      STOP
    END IF
    
    ! Destroy the SensorInfo structure
    ! --------------------------------
    Error_Status = Destroy_SensorInfo( SensorInfo )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error destroying SensorInfo data structure.', &
                            FAILURE )
      STOP
    END IF
    
  END DO Sensor_Loop
  
  
  ! Destroy the SensorInfo linked list
  ! ----------------------------------
  Error_Status = Destroy_SensorInfo_List( SensorInfo_List )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying SensorInfo linked list.', &
                          WARNING )
  END IF

END PROGRAM AAPP_AMSU_AntCorr_ASC2NC
