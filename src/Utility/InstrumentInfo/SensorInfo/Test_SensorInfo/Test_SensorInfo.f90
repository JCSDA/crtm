!
! Test_SensorInfo
!
! Program to test the SensorInfo linked list and I/O routines and
! demonstrate how to use them to read a SensorInfo file, extract
! data from the resulting linked list, and write a SensorInfo file.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 28-Feb-2003
!                       paul.vandelst@ssec.wisc.edu

PROGRAM Test_SensorInfo

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                   Display_Message, Program_Message
  USE SensorInfo_Define    , ONLY: SensorInfo_type, &
                                   Allocate_SensorInfo, &
                                   Destroy_SensorInfo
  USE SensorInfo_LinkedList, ONLY: SensorInfo_List_type, &
                                   New_SensorInfo_List, &
                                   Destroy_SensorInfo_List, &
                                   AddTo_SensorInfo_List, &
                                   GetFrom_SensorInfo_List, &
                                   Count_SensorInfo_Nodes 
  USE SensorInfo_IO        , ONLY: SENSORINFO_FORMAT, &
                                   Read_SensorInfo, Write_SensorInfo
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Test_SensorInfo'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'
  INTEGER, PARAMETER :: SET = 1
  INTEGER, PARAMETER :: MAX_N_READS = 10000


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  INTEGER :: Error_Status
  INTEGER :: n_Sensors, n
  CHARACTER(256)             :: SensorInfo_Filename
  TYPE(SensorInfo_type)      :: SensorInfo
  TYPE(SensorInfo_List_type) :: SensorInfo_List


  ! Program header
  ! --------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test the SensorInfo linked list and I/O routines.', &
                        '$Revision$' )


  ! Read the SensorInfo file
  ! ------------------------
  WRITE( *, '( /5x, "Test reading the input SensorInfo file..." )' )
  SensorInfo_Filename = 'SensorInfo.input_test'
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
  WRITE( *, '( 5x, "Number of sensors in list: ", i5 )' ) n_Sensors


  ! Loop over all sensor in the list
  ! --------------------------------
  WRITE( *, '( /5x, "Looping over SensorInfo list entries to extract data....", / )' )
  WRITE( *, '("                                                 +--------------------------- Microwave flag" )' )
  WRITE( *, '("    +------------------- Sensor name             |                            (0==IR, 1==MW)" )' )
  WRITE( *, '("    |            +------ Satellite/platform name |" )' )
  WRITE( *, '("    |            |  +--- Sensor_Id               |          +---------------- WMO sensor ID" )' )
  WRITE( *, '("    |            |  |                            |          |     +---------- WMO satellite ID" )' )
  WRITE( *, '("    |            |  +-------------+              |          |     |      +--- No. of channels" )' )
  WRITE( *, '("    |            |                |              |          |     |      |" )' )

  n_Sensor_loop: DO n = 1, n_Sensors


    ! Get the current SensorInfo node from the list
    ! ---------------------------------------------
    Error_Status = GetFrom_SensorInfo_List( SensorInfo_List, &
                                            n, &
                                            SensorInfo )
    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error retrieving SensorInfo data for sensor # ", i5 )' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            Error_Status )
      STOP
    END IF


    ! Output some data for the current sensor
    ! ---------------------------------------
    WRITE( *,FMT=SENSORINFO_FORMAT ) SensorInfo%Sensor_Name     , &
                                     SensorInfo%Satellite_Name  , &
                                     SensorInfo%Sensor_Id       , &
                                     SensorInfo%Microwave_Flag  , &
                                     SensorInfo%WMO_Satellite_ID, &
                                     SensorInfo%WMO_Sensor_ID   , &
                                     SensorInfo%n_Channels


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


    ! Pause to have a lookee....
    ! --------------------------
    IF ( MOD(n,25 ) == 0 ) THEN
      WRITE( *,'(10x,"Press <ENTER> to continue...")' )
      READ( *,* )
    END IF

  END DO n_Sensor_loop


  ! Write a SensorInfo file
  ! -----------------------
  WRITE( *, '( /5x, "Test writing an output SensorInfo file..." )' )
  SensorInfo_Filename = 'SensorInfo.output_test'
  Error_Status = Write_SensorInfo( TRIM(SensorInfo_Filename), &
                                   SensorInfo_List, &
                                   Quiet=SET )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing SensorInfo file '//TRIM(SensorInfo_Filename), &
                          FAILURE )
    STOP
  END IF


  ! Destroy the SensorInfo linked list
  ! ----------------------------------
  Error_Status = Destroy_SensorInfo_List( SensorInfo_List )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying SensorInfo_List.', &
                          WARNING )
  END IF


  ! Loop over read function to accumulate memory leaks
  ! --------------------------------------------------
  WRITE( *,'(/5x,"Looping over read function for memory leak test (run top)...")' )
  SensorInfo_Filename = 'SensorInfo.input_test'

  DO n = 1, MAX_N_READS

    Error_Status = Read_SensorInfo( SensorInfo_Filename, &
                                    SensorInfo_List, &
                                    Quiet=SET )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error reading SensorInfo file '//TRIM(SensorInfo_Filename), &
                            FAILURE )
      STOP
    END IF

    IF ( MOD( n, 1000 ) == 0 ) THEN
      WRITE( Message,'("Number of reads performed: ",i0," of ",i0)' ) n, MAX_N_READS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            INFORMATION )
    END IF

  END DO

END PROGRAM Test_SensorInfo
