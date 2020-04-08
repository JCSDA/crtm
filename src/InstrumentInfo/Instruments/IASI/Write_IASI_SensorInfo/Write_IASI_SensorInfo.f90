!
! Write_IASI_SensorInfo
!
! Program to create IASI channel subset SensorInfo entries.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 06-Mar-2008
!                       paul.vandelst@noaa.gov
!

PROGRAM Write_IASI_SensorInfo

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE String_Utility
  USE List_File_Utility
  USE Sort_Utility
  USE Channel_Subset_Define
  USE IASI_Define
  USE IASI_Subset
  USE SensorInfo_Parameters
  USE SensorInfo_Define
  USE SensorInfo_LinkedList
  USE SensorInfo_IO
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Write_IASI_SensorInfo'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &

  ! Channel subset selections
  INTEGER,      PARAMETER :: N_SETS = 8
  INTEGER,      PARAMETER :: BAND1_SUBSET             = 1
  INTEGER,      PARAMETER :: BAND2_SUBSET             = 2
  INTEGER,      PARAMETER :: BAND3_SUBSET             = 3
  INTEGER,      PARAMETER :: ALL_CHANNELS             = 4
  INTEGER,      PARAMETER :: EUMETSAT300_SUBSET       = 5
  INTEGER,      PARAMETER :: NESDIS316_SUBSET         = 6
  INTEGER,      PARAMETER :: EUMETSATNESDIS616_SUBSET = 7
  INTEGER,      PARAMETER :: USER_SPECIFIED_SUBSET    = 8
  CHARACTER(*), PARAMETER :: SET_NAME(N_SETS) = &
    (/ 'Band 1 channels                   ', &
       'Band 2 channels                   ', &
       'Band 3 channels                   ', &
       'All channels                      ', &
       'EUMETSAT 300 channel subset       ', &
       'NESDIS   316 channel subset       ', &
       'EUMETSAT+NESDIS 616 channel subset', &
       'User specified channel subset     ' /)
  CHARACTER(*), PARAMETER :: USER_SUBSET_FILENAME = 'User_Channel_List.IASI'
  CHARACTER(*), PARAMETER :: SENSORINFO_FILENAME  = 'SensorInfo.IASI'

  ! SensorInfo default for IASI instrument
  CHARACTER(*), PARAMETER :: SATELLITE_NAME   = 'MetOp-A'
  INTEGER,      PARAMETER :: WMO_SENSOR_ID    = 221
  INTEGER,      PARAMETER :: WMO_SATELLITE_ID = 4
  REAL(fp),     PARAMETER :: DEFAULT_NOISE    = 100.0_fp

  ! ---------
  ! Variables
  ! ---------

  CHARACTER(256) :: Message
  CHARACTER(20)  :: Sensor_Id
  INTEGER :: Error_Status
  INTEGER :: n, l
  INTEGER :: n_Channels
  INTEGER, ALLOCATABLE :: Channel_List(:)
  TYPE(Integer_List_File_type) :: User_Channel_List
  TYPE(SensorInfo_type)        :: SensorInfo
  TYPE(SensorInfo_List_type)   :: SensorInfo_List

  ! Output program header
  ! ---------------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to create IASI channel subset SensorInfo entries. '//&
                        'A user specified channel subset list is read from the file '//&
                        USER_SUBSET_FILENAME//' if it exists.', &
                        '$Revision$' )


  ! Create a new SensorInfo list
  ! ----------------------------
  SensorInfo_List = New_SensorInfo_List()


  ! Loop over channel subset lists
  ! ------------------------------
  Subset_Loop: DO n = 1, N_SETS


    ! Get the required channels list
    ! ------------------------------
    SELECT CASE (n)

      ! IASI Band channels
      CASE ( BAND1_SUBSET:BAND3_SUBSET )
        n_Channels = N_IASI_CHANNELS_PER_BAND(n)
        ALLOCATE( Channel_List( n_Channels ) )
        Channel_List = (/(l,l=IASI_BAND_BEGIN_CHANNEL(n),IASI_BAND_END_CHANNEL(n))/)
        WRITE( Sensor_Id,'("iasiB",i1,"_",a)') n, StrLowCase(SATELLITE_NAME)

      ! All the IASI channels
      CASE ( ALL_CHANNELS )
        n_Channels = N_IASI_CHANNELS
        ALLOCATE( Channel_List( n_Channels ) )
        Channel_List = (/(l,l=1,n_Channels)/)
        Sensor_Id = 'iasi_'//StrLowCase(SATELLITE_NAME)

      ! The EUMETSAT 300 channel subset
      CASE ( EUMETSAT300_SUBSET )
        n_Channels = N_IASI_SUBSET_300
        ALLOCATE( Channel_List( n_Channels ) )
        Channel_List = IASI_SUBSET_300
        Sensor_Id = 'iasi300_'//StrLowCase(SATELLITE_NAME)

      ! The NESDIS 316 channel subset
      CASE ( NESDIS316_SUBSET )
        n_Channels = N_IASI_SUBSET_316
        ALLOCATE( Channel_List( n_Channels ) )
        Channel_List = IASI_SUBSET_316
        Sensor_Id = 'iasi316_'//StrLowCase(SATELLITE_NAME)

      ! The combined EUMETSAT+NESDIS 616 channel subset
      CASE ( EUMETSATNESDIS616_SUBSET )
        n_Channels = N_IASI_SUBSET_616
        ALLOCATE( Channel_List( n_Channels ) )
        Channel_List = IASI_SUBSET_616
        CALL InsertionSort( Channel_List )
        Sensor_Id = 'iasi616_'//StrLowCase(SATELLITE_NAME)

      ! A user specified channel subset
      CASE ( USER_SPECIFIED_SUBSET )

        ! If the user file does not exist, exit
        IF ( .NOT. File_Exists(USER_SUBSET_FILENAME) ) EXIT Subset_Loop
        ! Read the channel subset list file
        Error_Status = Read_List_File( USER_SUBSET_FILENAME, User_Channel_List )
        IF ( Error_Status /= SUCCESS ) THEN
          CALL Display_Message( PROGRAM_NAME, &
                                'Error reading list file '//TRIM(USER_SUBSET_FILENAME), &
                                FAILURE )
          STOP
        END IF
        ! Retrieve the number of subset channels
        n_Channels = Get_List_Size( User_Channel_List )
        IF ( n_Channels < 1 ) THEN
          CALL Display_Message( PROGRAM_NAME, &
                                'No channels listed in '//TRIM(USER_SUBSET_FILENAME), &
                                FAILURE )
          STOP
        END IF
        ! Check the number of channels
        IF ( n_Channels < 1 .OR. n_Channels > N_IASI_CHANNELS ) THEN
          CALL Display_Message( PROGRAM_NAME, &
                                'Number of channels listed in '//TRIM(USER_SUBSET_FILENAME)//' outside of valid range.', &
                                FAILURE )
          STOP
        END IF
        ! Fill the subset list
        ALLOCATE( Channel_List( n_Channels ) )
        DO l = 1, n_Channels
          Error_Status = Get_List_Entry( User_Channel_List, l, Channel_List(l) )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error retrieving user subset channel list entry ",i0 )' ) l
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM(Message), &
                                  FAILURE )
            STOP
          END IF
        END DO
        ! Create the sensor id
        WRITE( Sensor_Id,'("iasiU",i0,"_",a)' ) n_Channels, StrLowCase(SATELLITE_NAME)

    END SELECT


    ! Add the current subset to the SensorInfo list
    ! ---------------------------------------------
    WRITE( *,'(5x,"Adding ",a," to the SensorInfo list.")' ) TRIM(Sensor_Id)
    ! Allocate the SensorInfo structure
    Error_Status = Allocate_SensorInfo( n_Channels, SensorInfo )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error allocating SensorInfo structure.', &
                            FAILURE ) 
      STOP
    END IF
    ! Assign the scalar members
    SensorInfo%Sensor_Name      = 'IASI'
    SensorInfo%Satellite_Name   = SATELLITE_NAME
    SensorInfo%Sensor_Id        = Sensor_Id
    SensorInfo%WMO_Sensor_ID    = WMO_SENSOR_ID   
    SensorInfo%WMO_Satellite_ID = WMO_SATELLITE_ID
    SensorInfo%Sensor_Type      = INFRARED_SENSOR
    ! Assign the array members
    SensorInfo%Sensor_Channel = Channel_List
    SensorInfo%Use_Flag       = 1
    SensorInfo%Noise          = DEFAULT_NOISE
    ! Add to the list
    Error_Status = AddTo_SensorInfo_List( SensorInfo, SensorInfo_List )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error adding '//TRIM(Sensor_Id)//' to SensorInfo list.', &
                            FAILURE )
      STOP
    END IF


    ! Deallocate the channel list array for the next subset
    ! -----------------------------------------------------
    DEALLOCATE( Channel_List )
    
  END DO Subset_Loop
  
  
  ! Write the SensorInfo data
  ! -------------------------
  Error_Status = Write_SensorInfo( SENSORINFO_FILENAME, &
                                   SensorInfo_List, &
                                   Quiet = 1 )
  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = FAILURE
    CALL display_message( PROGRAM_NAME, &
                          'Error writing SensorInfo data to '//SENSORINFO_FILENAME, &
                          Error_Status )
    STOP
  END IF


  ! Clean up
  ! --------
  ! Destroy the SensorInfo structure
  Error_Status = Destroy_SensorInfo( SensorInfo )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying SensorInfo structure.', &
                          WARNING )
  END IF
  ! Destroy the SensorInfo linked list
  Error_Status = Destroy_SensorInfo_List( SensorInfo_List )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying SensorInfo linked list.', &
                          WARNING )
  END IF

END PROGRAM Write_IASI_SensorInfo
