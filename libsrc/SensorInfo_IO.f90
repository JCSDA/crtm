!
! SensorInfo_IO
!
! Module containing routines to read and write ASCII format SensorInfo
! data files.
!       
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 09-Aug-2002
!                       paul.vandelst@ssec.wisc.edu
!

MODULE SensorInfo_IO

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE File_Utility         , ONLY: Get_Lun, File_Exists 
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                   Display_Message
  USE SensorInfo_Define    , ONLY: SensorInfo_type, &
                                   Allocate_SensorInfo, &
                                   Destroy_SensorInfo
  USE SensorInfo_LinkedList, ONLY: SensorInfo_List_type, &
                                   New_SensorInfo_List, &
                                   Destroy_SensorInfo_List, &
                                   AddTo_SensorInfo_List, &
                                   GetFrom_SensorInfo_List, &
                                   Count_SensorInfo_Nodes 
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Parameters
  PUBLIC :: SENSORINFO_FORMAT
  PUBLIC :: CHANNELINFO_FORMAT  
  ! Module procedures
  PUBLIC :: Read_SensorInfo
  PUBLIC :: Write_SensorInfo


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Keyword set value
  INTEGER, PARAMETER :: SET = 1
  ! Input data formats
  CHARACTER(*), PARAMETER ::  SENSORINFO_FORMAT = '(1x,2(1x,a12),1x,a20,1x,i1,6x,4(1x,i5))'
  CHARACTER(*), PARAMETER :: CHANNELINFO_FORMAT = '(i5,3x,i2,5x,es13.6)'


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!
! NAME:
!       Read_SensorInfo
!
! PURPOSE:
!       Function to read ASCII format SensorInfo file data into a
!       SensorInfo linked list.
!
! CALLING SEQUENCE:
!       Error_Status = Read_SensorInfo( Filename,         &  ! Input
!                                       SensorInfo_List,  &  ! Output
!                                       Quiet      =Quiet )  ! Optional input
!
! INPUT ARGUMENTS:
!       Filename:        Character string specifying the name of an ASCII
!                        format SensorInfo data file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       SensorInfo_List: Linked list containing the SensorInfo data. Each list
!                        node corresponds to a SensorInfo file entry.
!                        UNITS:      N/A
!                        TYPE:       SensorInfo_List_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:           Set this keyword to suppress information Messages being
!                        printed to standard output (or the Message log file if
!                        the Message_Log optional argument is used.) By default,
!                        information Messages are printed.
!                        If QUIET = 0, information Messages are OUTPUT.
!                           QUIET = 1, information Messages are SUPPRESSED.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the SensorInfo data read was successful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output SensorInfo_List argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined on
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Read_SensorInfo( Filename,        &  ! Input
                            SensorInfo_List, &  ! Output
                            Quiet          ) &  ! Optional input
                          RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)              , INTENT(IN)     :: Filename
    TYPE(SensorInfo_List_type), INTENT(IN OUT) :: SensorInfo_List
    INTEGER     ,     OPTIONAL, INTENT(IN)     :: Quiet
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_SensorInfo'
    ! Function variables
    CHARACTER(256) :: Message
    CHARACTER(256) :: Line_Buffer
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: l
    INTEGER :: n_Sensors
    TYPE(SensorInfo_type) :: SensorInfo, dummy


    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Does the file exist?
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'File '//TRIM(Filename)//' not found.', &
                            Error_Status )
      RETURN
    END IF

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT(Quiet) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF


    ! Create a new SensorInfo linked list
    ! -----------------------------------
    Error_Status = Destroy_SensorInfo_List( SensorInfo_List, &
                                            Quiet      =Quiet )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying SensorInfo_List.', &
                            Error_Status )
      RETURN
    END IF
    SensorInfo_List = New_SensorInfo_List()


    ! Open the SensorInfo file
    ! ------------------------
    FileID = Get_Lun()
    IF ( FileID < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining file unit number.', &
                            Error_Status )
      RETURN
    END IF
    OPEN( FileID, FILE   = TRIM(ADJUSTL(Filename)), &
                  STATUS = 'OLD', &
                  ACCESS = 'SEQUENTIAL', &
                  FORM   = 'FORMATTED', &
                  ACTION = 'READ', &
                  IOSTAT = IO_Status )
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//TRIM(Filename), &
                            Error_Status )
      RETURN
    END IF


    ! Loop over comment lines
    ! -----------------------
    Comment_Read_loop: DO

      ! Read a line of the file
      READ( FileID, FMT   ='(a)', &
                    IOSTAT=IO_Status ) Line_Buffer
      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error reading SensorInfo file in comment skip. IOSTAT = ",i5)' ) &
                        IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status )
        CLOSE( FileID )
        RETURN
      END IF

      ! Exit loop if this is NOT a comment or blank line
      IF ( Line_Buffer(1:1) /= '!' .AND. LEN_TRIM(Line_Buffer) /= 0 ) THEN
        BACKSPACE( FileID )
        EXIT Comment_Read_loop
      END IF

    END DO Comment_Read_loop


    ! Initialise sensor counter
    ! -------------------------
    n_Sensors = 0


    ! Begin open loop over sensors
    ! ----------------------------
    SensorInfo_Read_loop: DO


      ! Read a line of the file into a character buffer
      ! -----------------------------------------------
      READ( FileID, FMT   ='(a)', &
                    IOSTAT=IO_Status ) Line_Buffer

      ! End of file?
      IF ( IO_Status < 0 ) EXIT SensorInfo_Read_Loop

      ! Read error
      IF ( IO_Status > 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error reading SensorInfo file in sensor header read. ",&
                        &"Sensors already read = ",i0,". IOSTAT = ",i0)' ) &
                        n_Sensors, IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status )
        CLOSE( FileID )
        RETURN
      END IF

      ! Cycle loop if this is a blank line
      IF ( LEN_TRIM(Line_Buffer) == 0 ) CYCLE SensorInfo_Read_Loop


      ! Increment sensor counter
      ! ------------------------
      n_Sensors = n_Sensors + 1


      ! Read the SensorInfo data line into variables
      ! --------------------------------------------
      READ( Line_Buffer, FMT   =SENSORINFO_FORMAT, &
                         IOSTAT=IO_Status ) dummy%Sensor_Name, &
                                            dummy%Satellite_Name, &
                                            dummy%Sensor_Id, &
                                            dummy%Sensor_Type, &
                                            dummy%WMO_Sensor_ID, &
                                            dummy%WMO_Satellite_ID, &
                                            dummy%n_Channels, &
                                            dummy%n_FOVs

      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error reading SensorInfo line buffer in sensor header read. ",&
                        &"Sensors already read = ",i0,". IOSTAT = ",i0)' ) &
                        n_Sensors, IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status )
        CLOSE( FileID )
        RETURN
      END IF 


      ! Allocate the SensorInfo structure pointer components
      ! ----------------------------------------------------
      Error_Status = Allocate_SensorInfo( dummy%n_Channels, &
                                          SensorInfo )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error allocating SensorInfo structure for '//&
                              TRIM(dummy%Sensor_Id), &
                              Error_Status )
        CLOSE( FileID )
        RETURN
      END IF


      ! Assign the non-dimensional SensorInfo data
      SensorInfo%Sensor_Name      = dummy%Sensor_Name
      SensorInfo%Satellite_Name   = dummy%Satellite_Name
      SensorInfo%Sensor_Id        = dummy%Sensor_Id
      SensorInfo%Sensor_Type      = dummy%Sensor_Type
      SensorInfo%WMO_Sensor_ID    = dummy%WMO_Sensor_ID
      SensorInfo%WMO_Satellite_ID = dummy%WMO_Satellite_ID
      SensorInfo%n_FOVs           = dummy%n_FOVs      


      ! Output an info message
      ! ----------------------
      IF ( Noisy ) THEN
        WRITE( Message,'("SENSOR ID: ",a,", N_CHANNELS=",i0)' ) &
                       TRIM(SensorInfo%Sensor_Id), &
                       SensorInfo%n_Channels
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              INFORMATION )
      END IF


      ! Read the channel information
      ! ----------------------------
      ChannelInfo_Read_loop: DO l = 1, SensorInfo%n_Channels

        READ( FileID, FMT   =CHANNELINFO_FORMAT, &
                      IOSTAT=IO_Status ) SensorInfo%Sensor_Channel(l), &
                                         SensorInfo%Use_Flag(l), &
                                         SensorInfo%Noise(l)
        IF ( IO_Status /= 0 ) THEN
          Error_Status = FAILURE
          WRITE( Message,'("Error reading ChannelInfo data for ",a,&
                          &", channel # ",i0,". IOSTAT = ",i0)' ) &
                         TRIM(SensorInfo%Sensor_Id), &
                         l, IO_Status
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM(Message), &
                                Error_Status )
          CLOSE( FileID )
          RETURN
        END IF

      END DO ChannelInfo_Read_loop


      ! Add the current SensorInfo structure to the list
      ! ------------------------------------------------
      Error_Status = AddTo_SensorInfo_List( SensorInfo, &
                                            SensorInfo_List, &
                                            Node_Number=n_Sensors )
      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error adding '//&
                              TRIM(SensorInfo%Sensor_Id)//' to SensorInfo list.', &
                              Error_Status )
        CLOSE( FileID )
        RETURN
      END IF


      ! Destroy the SensorInfo structure for the next read
      ! --------------------------------------------------
      Error_Status = Destroy_SensorInfo( SensorInfo )
      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error destroying SensorInfo structures at sensor # ",i0)' ) &
                       n_Sensors
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status )
        CLOSE( FileID )
        RETURN
      END IF

    END DO SensorInfo_Read_loop


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      WRITE( Message,'("FILE: ",a,", N_SENSORS=",i0)' ) &
                      TRIM(Filename), n_Sensors
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            INFORMATION )
    END IF


    ! Close the file
    ! --------------
    CLOSE( FileID, STATUS='KEEP',   &
                   IOSTAT=IO_Status )
    IF ( IO_Status /= 0 ) THEN
      Error_Status = WARNING
      WRITE( Message,'("Error closing ",a,". IOSTAT = ",i0)' ) &
                      TRIM(Filename), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status )
    END IF

  END FUNCTION Read_SensorInfo


!------------------------------------------------------------------------------
!
! NAME:
!       Write_SensorInfo
!
! PURPOSE:
!       Function to write the data within a SensorInfo linked list to an
!       ASCII format SensorInfo file.
!
! CALLING SEQUENCE:
!       Error_Status = Write_SensorInfo( Filename,         &  ! Input
!                                        SensorInfo_List,  &  ! Input
!                                        Quiet      =Quiet )  ! Optional input
!
! INPUT ARGUMENTS:
!       Filename:        Character string specifying the name of an output
!                        SensorInfo data file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       SensorInfo_List: Linked list containing the SensorInfo data to write.
!                        Each list node corresponds to a SensorInfo file entry.
!                        UNITS:      N/A
!                        TYPE:       SensorInfo_List_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:           Set this keyword to suppress information Messages being
!                        printed to standard output (or the Message log file if
!                        the Message_Log optional argument is used.) By default,
!                        information Messages are printed.
!                        If QUIET = 0, information Messages are OUTPUT.
!                           QUIET = 1, information Messages are SUPPRESSED.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the SensorInfo data write was successful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       - If the output file already exists, it is overwritten.
!       - If an error occurs in this routine, the output file is deleted
!         before returning to the calling routine.
!
! RESTRICTIONS:
!       This function checks the association status of the SensorInfo linked
!       list nodes. Therefore, this function should *only* be called
!       *after* the SensorInfo linked list has been filled with data.
!
!------------------------------------------------------------------------------

  FUNCTION Write_SensorInfo( Filename,        &  ! Input
                             SensorInfo_List, &  ! Input
                             Quiet          ) &  ! Optional input
                           RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)              , INTENT(IN) :: Filename
    TYPE(SensorInfo_List_type), INTENT(IN) :: SensorInfo_List
    INTEGER     ,     OPTIONAL, INTENT(IN) :: Quiet
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_SensorInfo'
    ! Function variables
    CHARACTER(256) :: Message
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: l
    INTEGER :: n_Sensors, n
    TYPE(SensorInfo_type) :: SensorInfo

    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Does the file exist?
    IF ( File_Exists( TRIM(Filename) ) ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'File '//TRIM(Filename)//' will be overwritten.', &
                            WARNING )
    END IF

    ! Output informational Messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF


    ! Create the SensorInfo file
    ! --------------------------
    FileID = Get_Lun()
    IF ( FileID < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining file unit number.', &
                            Error_Status )
      RETURN
    END IF
    OPEN( FileID, FILE   = Filename, &
                  STATUS = 'REPLACE', &
                  ACCESS = 'SEQUENTIAL', &
                  FORM   = 'FORMATTED', &
                  ACTION = 'WRITE', &
                  IOSTAT = IO_Status )
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//TRIM(Filename), &
                            Error_Status )
      RETURN
    END IF


    ! Determine the number of sensors in the list
    ! -------------------------------------------
    n_Sensors = Count_SensorInfo_Nodes( SensorInfo_List )
    IF ( n_Sensors < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'SensorInfo list is empty', &
                            Error_Status )
      CLOSE( FileID, STATUS='DELETE' )
      RETURN
    END IF


    ! Loop over the number of sensors
    ! -------------------------------
    SensorInfo_Write_loop: DO n = 1, n_Sensors

      ! Get the current sensor data from the list
      Error_Status = GetFrom_SensorInfo_List( SensorInfo_List, &
                                              n, &
                                              SensorInfo )
      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error retrieving SensorInfo data for sensor # ",i0)' ) n
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status )
        CLOSE( FileID, STATUS='DELETE' )
        RETURN
      END IF

      ! Write the SensorInfo data
      WRITE( FileID, FMT   =SENSORINFO_FORMAT, &
                     IOSTAT=IO_Status ) SensorInfo%Sensor_Name, &
                                        SensorInfo%Satellite_Name, &
                                        SensorInfo%Sensor_Id, &
                                        SensorInfo%Sensor_Type, &
                                        SensorInfo%WMO_Sensor_ID, &
                                        SensorInfo%WMO_Satellite_ID, &
                                        SensorInfo%n_Channels, &
                                        SensorInfo%n_FOVs
      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error writing SensorInfo data for sensor # ",i0,&
                        &". IOSTAT = ",i0)' ) n, IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status )
        CLOSE( FileID, STATUS='DELETE' )
        RETURN
      END IF


      ! Output an info message
      IF ( Noisy ) THEN
        WRITE( Message,'("SENSOR ID: ",a,", N_CHANNELS=",i0)' ) &
                        TRIM(SensorInfo%Sensor_Id), SensorInfo%n_Channels
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              INFORMATION )
      END IF

      ! Write the ChannelInfo data
      ChannelInfo_Write_loop: DO l = 1, SensorInfo%n_Channels
        WRITE( FileID, FMT   =CHANNELINFO_FORMAT, &
                       IOSTAT=IO_Status ) SensorInfo%Sensor_Channel(l), &
                                          SensorInfo%Use_Flag(l), &
                                          SensorInfo%Noise(l)
        IF ( IO_Status /= 0 ) THEN
          Error_Status = FAILURE
          WRITE( Message,'("Error writing ChannelInfo data for ", a, &
                          &", channel # ",i0,". IOSTAT = ",i0)' ) &
                          TRIM(SensorInfo%Sensor_Id), l, IO_Status
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM(Message), &
                                Error_Status )
          CLOSE( FileID, STATUS='DELETE' )
          RETURN
        END IF
      END DO ChannelInfo_Write_loop

      ! Destroy the SensorInfo structure for the next node
      Error_Status = Destroy_SensorInfo( SensorInfo )
      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error destroying SensorInfo structure at sensor # ",i0)' ) &
                        n_Sensors
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              Error_Status )
        CLOSE( FileID, STATUS='DELETE' )
        RETURN
      END IF

    END DO SensorInfo_Write_loop


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      WRITE( Message,'("FILE: ",a,", N_SENSORS=",i0)' ) &
                      TRIM(Filename), n_Sensors
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            INFORMATION )
    END IF


    ! Close the file
    ! --------------
    CLOSE( FileID, STATUS='KEEP',   &
                   IOSTAT=IO_Status )
    IF ( IO_Status /= 0 ) THEN
      Error_Status = WARNING
      WRITE( Message,'("Error closing ",a,". IOSTAT = ",i0)' ) &
                     TRIM(Filename), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status )
    END IF

  END FUNCTION Write_SensorInfo

END MODULE SensorInfo_IO
