!
!  Info_SAT2SENSOR
!
!  Program to generate a SensorInfo file from a SatInfo file.
!
!
!  Written by:     Paul van Delst, CIMSS/SSEC 28-Feb-2003
!                  paul.vandelst@ssec.wisc.edu
!

PROGRAM Info_SAT2SENSOR

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE SatInfo_IO
  USE SensorInfo_Define
  USE SensorInfo_LinkedList
  USE SensorInfo_IO
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Info_SAT2SENSOR'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  INTEGER, PARAMETER :: maxChannels = 5000


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  INTEGER :: Error_Status
  INTEGER :: i, l, ls, n
  INTEGER :: maxSensors
  INTEGER :: outputSensors
  INTEGER :: nChannels
  LOGICAL :: NotFound
  CHARACTER(20)  :: Last_Sensor_Id
  CHARACTER(256) :: SatInfo_Filename
  CHARACTER(256) :: Master_SensorInfo_Filename
  CHARACTER(256) :: Output_SensorInfo_Filename
  TYPE(SensorInfo_type)      :: SensorInfo
  TYPE(SensorInfo_List_type) :: Master_SensorInfo_List
  TYPE(SensorInfo_List_type) :: Output_SensorInfo_List
  TYPE(SatInfo_type), DIMENSION(maxChannels) :: SatInfo



  ! ------------------------------
  ! Create SensorInfo linked lists
  ! ------------------------------
  Master_SensorInfo_List = New_SensorInfo_List()
  Output_SensorInfo_List = New_SensorInfo_List()



  ! ---------------------
  ! Output program header
  ! ---------------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to generate a SensorInfo file from a SatInfo file.', &
                        '$Revision: 1.8 $' )



  ! ---------------------
  ! Read the SatInfo file
  ! ---------------------
  ! Enter the file name
  WRITE( *, FMT = '( /5x, "Enter a SatInfo filename: " )', ADVANCE = 'NO' )
  READ( *, '( a )' ) SatInfo_Filename
  SatInfo_Filename = ADJUSTL( SatInfo_Filename )

  ! Count the number of lines(channels) in the SatInfo file
  nChannels = Count_Lines_in_File( SatInfo_Filename, NoComment='!', NoBlank=1 )
  IF ( nChannels == 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error counting the number of SatInfo channel entries in '//&
                          TRIM( SatInfo_Filename ), &
                          Error_Status )
    STOP
  END IF

  ! Read the file
  Error_Status = Read_SatInfo( SatInfo_Filename, &
                               SatInfo(1:nChannels) )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading SatInfo file '//TRIM( SatInfo_Filename ), &
                          Error_Status )
    STOP
  END IF


  ! -------------------------------
  ! Read the master SensorInfo file
  ! -------------------------------
  ! Enter the file name
  WRITE( *, FMT = '( /5x, "Enter the MASTER SensorInfo filename: " )', ADVANCE = 'NO' )
  READ( *, '( a )' ) Master_SensorInfo_Filename
  Master_SensorInfo_Filename = ADJUSTL( Master_SensorInfo_Filename )

  ! Read the SensorInfo file
  Error_Status = Read_SensorInfo( Master_SensorInfo_Filename, &
                                  Master_SensorInfo_List, &
                                  Quiet = 1 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading Master SensorInfo file '//&
                          TRIM( Master_SensorInfo_Filename ), &
                          FAILURE )
    STOP
  END IF

  ! Count the number of sensors
  maxSensors = Count_SensorInfo_Nodes( Master_SensorInfo_List )
  IF ( maxSensors < 1 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Master_SensorInfo_List is empty.', &
                          FAILURE )
    STOP
  END IF

  WRITE( *, '( /5x, "Number of sensors in master list: ", i5 )' ) maxSensors


  ! -------------------------
  ! Loop over SatInfo entries
  ! -------------------------
  ! Initialisation
  Last_Sensor_Id = ' '
  outputSensors = 0

  ! Channel loop
  SatInfo_Entry_Loop: DO l = 1, nChannels
    New_SatInfo_Sensor: IF ( TRIM(SatInfo(l)%Sensor_Id) /= TRIM(Last_Sensor_Id) ) THEN
      Last_Sensor_Id = SatInfo(l)%Sensor_Id

      ! Search SensorInfo list for same sensor Id
      NotFound = .TRUE.
      SensorInfo_Search_Loop: DO n = 1, maxSensors

        ! Get the current node
        Error_Status = GetFrom_SensorInfo_List( Master_SensorInfo_List, &
                                                n, &
                                                SensorInfo )
        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error getting node #", i3, " from Master_SensorInfo_List" )' ) n
          CALL Display_Message( PROGRAM_NAME, &
                               TRIM( Message ), &
                               FAILURE )
          STOP
        END IF

        ! Check the sensor name
        IF ( SensorInfo%File_Prefix == SatInfo(l)%Sensor_Id ) THEN
          outputSensors = outputSensors + 1
          NotFound = .FALSE.
          EXIT SensorInfo_Search_Loop
        END IF

        ! Haven't found the sensor, so destroy
        ! the structure and try again
        Error_Status = Destroy_SensorInfo( SensorInfo )
        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error destroying SensorInfo structure for node #", i3, &
                            &" from Master_SensorInfo_List" )' ) n
          CALL Display_Message( PROGRAM_NAME, &
                               TRIM( Message ), &
                               FAILURE )
          STOP
        END IF

      END DO SensorInfo_Search_Loop

      ! Issue error if sensor not found
      IF ( NotFound ) THEN
        WRITE( Message, '( "SatInfo sensor ", a, &
                          &" not found in Master_SensorInfo_List." )' ) &
                        TRIM(Last_Sensor_Id)
        CALL Display_Message( PROGRAM_NAME, &
                             TRIM( Message ), &
                             FAILURE )
        STOP
      END IF

      ! Copy over the SatInfo data into the SensorInfo structure
      DO ls = 1, SensorInfo%n_Channels
        SensorInfo%Use_Flag(ls) = SatInfo(l+ls-1)%Use_Flag
        SensorInfo%Noise(ls)    = SatInfo(l+ls-1)%Error
      END DO

      ! Add the current SensorInfo node
      ! to the end of the output list
      Error_Status = AddTo_SensorInfo_List( SensorInfo, &
                                            Output_SensorInfo_List )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error adding node #", i3, " to Output_SensorInfo_List" )' ) &
                        outputSensors
        CALL Display_Message( PROGRAM_NAME, &
                             TRIM( Message ), &
                             FAILURE )
        STOP
      END IF

      ! Destroy the SensorInfo structure
      Error_Status = Destroy_SensorInfo( SensorInfo )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error destroying SensorInfo structure for node #", i3, &
                          &" in Output_SensorInfo_List" )' ) outputSensors
        CALL Display_Message( PROGRAM_NAME, &
                             TRIM( Message ), &
                             FAILURE )
        STOP
      END IF

    END IF New_SatInfo_Sensor
  END DO SatInfo_Entry_Loop

  WRITE( *, '(/5x, "Number of sensors in output list: ", i5 )' ) outputSensors


  ! --------------------------------
  ! Write the output SensorInfo file
  ! --------------------------------
  Output_SensorInfo_Filename = TRIM( SatInfo_Filename )//'.SensorInfo'
  Error_Status = Write_SensorInfo( TRIM( Output_SensorInfo_Filename ), &
                                   Output_SensorInfo_List )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing output SensorInfo file '//&
                          TRIM( Output_SensorInfo_Filename ), &
                          Error_Status )
    STOP
  END IF


  ! ----------------------------
  ! Destroy the SensorInfo lists
  ! ----------------------------
  Error_Status = Destroy_SensorInfo_List( Output_SensorInfo_List )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying Output_SensorInfo_List.', &
                          WARNING )
  END IF

  Error_Status = Destroy_SensorInfo_List( Master_SensorInfo_List )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying Master_SensorInfo_List.', &
                          WARNING )
  END IF

END PROGRAM Info_SAT2SENSOR
