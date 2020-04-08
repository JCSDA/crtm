!
! SRF_ASCII2NC
!
! Program to read the ASCII HMW format SRF data files and write
! netCDF format SRF data files.
!
!
! FILES ACCESSED:
!       Input: 
!         - User specified SensorInfo data file.
!         - ASCII format SRF data file(s) specified from SensorInfo entries.
!
!       Output:
!         - netCDF format SRF data file(s)
!
! SIDE EFFECTS:
!       If the netCDF format SRF file already exists, it is overwritten.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 22-Jan-2002
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM SRF_ASCII2NC

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, WARNING, &
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
  USE SRF_ASCII_IO         , ONLY: MAX_N_SRF_CHANNELS, &
                                   Read_SRF_ASCII_Header, &
                                   Read_SRF_ASCII
  USE SRF_netCDF_IO        , ONLY: Create_SRF_netCDF, &
                                   Write_SRF_netCDF
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'SRF_ASCII2NC'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  ! Keyword set value
  INTEGER, PARAMETER :: SET = 1
  ! Invalid NCEP sensor ID. The NCEP sensor ID is no longer used and this
  ! is a temporary fix until the SRF software is updated.
  INTEGER, PARAMETER :: INVALID_NCEP_SENSOR_ID = -1


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: SensorInfo_Filename
  CHARACTER(256) :: ASCII_SRF_Filename
  CHARACTER(256) :: NC_SRF_Filename
  CHARACTER(256) :: Title
  CHARACTER(256) :: History
  CHARACTER(256) :: Sensor_Name
  CHARACTER(256) :: Platform_Name
  CHARACTER(2000) :: Comment
  INTEGER :: ASCII_SRF_FileID
  INTEGER :: Error_Status
  INTEGER :: n_Channels, l
  INTEGER :: n_Sensors, n
  INTEGER :: Channel_List(MAX_N_SRF_CHANNELS)
  TYPE(SRF_type) :: SRF
  TYPE(SensorInfo_type)      :: SensorInfo
  TYPE(SensorInfo_List_type) :: SensorInfo_List


  ! Output program header
  ! ---------------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to read ASCII format SRF data files and write '//&
                        'netCDF format SRF data files.', &
                        '$Revision$' )

  ! Initialise the linked list
  ! --------------------------
  SensorInfo_List = New_SensorInfo_List()


  ! Read the SensorInfo file and count the sensors
  ! ----------------------------------------------
  WRITE( *,FMT='(/5x,"Enter a SensorInfo filename: ")',ADVANCE='NO' )
  READ( *,FMT='(a)' ) SensorInfo_Filename
  SensorInfo_Filename = ADJUSTL(SensorInfo_Filename)

  Error_Status = Read_SensorInfo( SensorInfo_Filename, &
                                  SensorInfo_List, &
                                  Quiet = SET )
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


  ! Loop over the sensors
  ! ---------------------
  Sensor_Loop: DO n = 1, n_Sensors


    ! Get the current SensorInfo data from thelist
    ! --------------------------------------------
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


    ! Construct the SRF filenames
    ! ---------------------------
    ASCII_SRF_Filename = TRIM(SensorInfo%Sensor_Id)//'.srf'
    NC_SRF_Filename    = TRIM(ASCII_SRF_Filename)//'.nc'


    ! Cycle loop if ASCII file doesn't exist
    ! --------------------------------------
    IF ( .NOT. File_Exists( TRIM(ASCII_SRF_Filename) ) ) CYCLE Sensor_Loop

    WRITE( *,'(5x,"Processing SRF file ",a,"...")' ) TRIM(ASCII_SRF_Filename)


    ! Read the ASCII SRF file header
    ! ------------------------------
    Error_Status = Read_SRF_ASCII_Header( TRIM(ASCII_SRF_Filename), &
                                          ASCII_SRF_FileID        , &
                                          n_Channels              , &
                                          Channel_List            , &
                                          Title                   , &
                                          History                 , &
                                          Sensor_Name             , &
                                          Platform_Name           , &
                                          Comment                   )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error reading header from '//TRIM(ASCII_SRF_Filename), &
                            FAILURE )
      STOP
    END IF


    ! Double check the SENSOR and PLATFORM names
    ! ------------------------------------------
    IF ( TRIM(Sensor_Name) /= TRIM(SensorInfo%Sensor_Name) ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'ASCII SRF sensor name, '//&
                            TRIM(Sensor_Name)//&
                            ', different from SensorInfo entry, '//&
                            TRIM(SensorInfo%Sensor_Name), &
                            WARNING )
    END IF
         
    IF ( TRIM(Platform_Name) /= TRIM(SensorInfo%Satellite_Name) ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'ASCII SRF platform name, '//&
                            TRIM(Platform_Name)//&
                            ', different from SensorInfo entry, '//&
                            TRIM(SensorInfo%Satellite_Name), &
                            WARNING )
    END IF


    ! Create the output netCDF file
    ! -----------------------------
    Error_Status = Create_SRF_netCDF( TRIM(NC_SRF_Filename), &
                                      Channel_List(1:n_Channels), &
                                      NCEP_Sensor_ID   = INVALID_NCEP_SENSOR_ID, &
                                      WMO_Satellite_ID = SensorInfo%WMO_Satellite_ID, &
                                      WMO_Sensor_ID    = SensorInfo%WMO_Sensor_ID, &
                                      Title            = TRIM(Title), &
                                      History          = PROGRAM_RCS_ID//';'//TRIM(History), &
                                      Sensor_Name      = TRIM(Sensor_Name), &
                                      Platform_Name    = TRIM(Platform_Name), &
                                      Comment          = TRIM(Comment) )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error creating '//TRIM(NC_SRF_Filename), &
                            FAILURE )
      STOP
    END IF


    ! Begin read/write loop over channels
    ! -----------------------------------
    Channel_Loop: DO l = 1, n_Channels

      ! Read SRF from ASCII file
      Error_Status = Read_SRF_ASCII( TRIM(ASCII_SRF_Filename), &
                                     ASCII_SRF_FileID, &
                                     Channel_List(l), &
                                     SRF, &
                                     Quiet = SET )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message,'("Error reading SRF channel ",i0," from ",a)' ) &
                       Channel_List(l), TRIM(ASCII_SRF_Filename)
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              FAILURE )
        STOP
      END IF

      ! Load sensor IDs. These don't change for
      ! different channels in a file, but the SRF
      ! allocation clears the structure and the
      ! netCDF write double checks these values
      ! against the file contents.
      SRF%NCEP_Sensor_Id   = INVALID_NCEP_SENSOR_ID
      SRF%WMO_Satellite_Id = SensorInfo%WMO_Satellite_ID
      SRF%WMO_Sensor_Id    = SensorInfo%WMO_Sensor_ID
      
      ! Write the SRF to the netCDF file
      Error_Status = Write_SRF_netCDF( NC_SRF_Filename, &
                                       SRF )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message,'("Error occurred writing channel #",i0," SRF to ",a,"." )' ) &
                       Channel_List(l), TRIM(NC_SRF_Filename)
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              FAILURE )
        STOP
      END IF

      ! Destroy the SRF structure for next use
      Error_Status = Destroy_SRF( SRF )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message,'("Error occurred destroying SRF structure ",&
                        &"for channel #",i0," read.")' ) &
                        Channel_List(l)
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              FAILURE )
        STOP
      END IF

    END DO Channel_Loop

    ! Close the input ASCII SRF file
    CLOSE( ASCII_SRF_FileID )


    ! Destroy the current SensorInfo structure
    ! ----------------------------------------
    Error_Status = Destroy_SensorInfo( SensorInfo )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message,'("Error destroying SensorInfo data for sensor # ",i0)' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            Error_Status )
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

END PROGRAM SRF_ASCII2NC
