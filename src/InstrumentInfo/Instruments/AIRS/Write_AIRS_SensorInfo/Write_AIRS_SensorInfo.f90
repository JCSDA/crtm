!
! Write_AIRS_SensorInfo
!
! Program to read the AIRS channel properties file and output the
! information for inclusion in a SensorInfo file.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 09-Jan-2003
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Write_AIRS_SensorInfo

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds
  USE Message_Handler
  USE List_File_Utility
  USE AIRS_Define
  USE AIRS_Subset
  USE AIRS_ChannelProperties
  USE SensorInfo_Define
  USE SensorInfo_LinkedList
  USE SensorInfo_IO
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Write_AIRS_ChannelInfo'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &

  ! Channel subset selections
  INTEGER,      PARAMETER :: N_VALID_SETS = 4
  INTEGER,      PARAMETER :: ALL_CHANNELS          = 1
  INTEGER,      PARAMETER :: CHANNEL_SUBSET_281    = 2
  INTEGER,      PARAMETER :: CHANNEL_SUBSET_324    = 3
  INTEGER,      PARAMETER :: USER_SPECIFIED_SUBSET = 4
  CHARACTER(*), PARAMETER, DIMENSION( N_VALID_SETS ) :: &
    VALID_SET_NAME = (/ 'All channels         ', &
                        '281 channel subset   ', &
                        '324 channel subset   ', &
                        'User specified subset' /)
  INTEGER, PARAMETER :: MAX_N_INVALID = 5

  ! The minimum radiometric quality flag value to accept.
  ! Any more and the Use_Flag for the AIRS channel is set to 0.
  INTEGER, PARAMETER :: RAD_FLAG = 0

  ! SensorInfo default for AIRS instrument
  CHARACTER(*), PARAMETER :: SENSOR_NAME      = 'AIRS'
  CHARACTER(*), PARAMETER :: SATELLITE_NAME   = 'Aqua'
  INTEGER,      PARAMETER :: MICROWAVE_FLAG   =   0
  INTEGER,      PARAMETER :: NCEP_SENSOR_ID   =  49
  INTEGER,      PARAMETER :: WMO_SENSOR_ID    = 420
  INTEGER,      PARAMETER :: WMO_SATELLITE_ID = 784


  ! ---------
  ! Variables
  ! ---------

  CHARACTER(256) :: message
  CHARACTER(256) :: List_Filename
  CHARACTER(256) :: Sensor_Descriptor
  CHARACTER(256) :: L2_Filename
  CHARACTER(256) :: SensorInfo_Filename
  CHARACTER(1) :: Answer
  INTEGER :: i, Channel_Set, Invalid
  LOGICAL :: Output_By_Module = .FALSE.
  INTEGER :: Error_Status
  INTEGER :: IO_Status
  INTEGER :: Allocate_Status
  INTEGER :: n, l, l1, l2
  INTEGER :: n_Channels, n_Bad_Channels, n_Total_Bad_Channels
  INTEGER :: Channel
  INTEGER, DIMENSION(:), ALLOCATABLE :: Channel_List
  TYPE(AIRS_ChannelProperties_type), DIMENSION( N_AIRS_CHANNELS ) :: ChannelProperties
  TYPE(Integer_List_File_type) :: User_Channel_List
  TYPE(SensorInfo_type)        :: SensorInfo
  TYPE(SensorInfo_List_type)   :: SensorInfo_List

  ! Output program header
  CALL Program_MEssage(PROGRAM_NAME, &
                       'Program to read the AIRS channel properties file and '//&
                       'output the information for inclusion in a SensorInfo '//&
                       'file.', &
                       '$Revision: 1.4' )

  ! Select a channel subset list
  Invalid = 0
  Channel_Set_Loop: DO

    ! Prompt user to select a subset set 
    WRITE( *, '( /5x, "Select an AIRS channel subset" )' )
    DO i = 1, N_VALID_SETS
      WRITE( *, '( 10x, i1, ") ", a )' ) i, VALID_SET_NAME(i)
    END DO
    WRITE( *, FMT     = '( 5x, "Enter choice: " )', &
              ADVANCE = 'NO' )
    READ( *, FMT    = '( i2 )', &
             IOSTAT = IO_Status ) Channel_Set

    ! Check the input
    IF ( IO_Status /= 0 ) THEN
      Invalid = Invalid + 1
    ELSE
      IF ( Channel_Set < 1 .OR. Channel_Set > N_VALID_SETS ) THEN
        Invalid = Invalid + 1
      ELSE
        EXIT Channel_Set_Loop
      END IF
    END IF
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid subset selection', &
                          INFORMATION )

    ! Only loop so many times for correct input
    IF ( Invalid == MAX_N_INVALID ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Too many invalid entries.', &
                            FAILURE )
      STOP
    END IF
  END DO Channel_Set_Loop

  ! Get the required channels list
  SELECT CASE ( Channel_Set )

    ! All the AIRS channels
    CASE ( ALL_CHANNELS )
      n_Channels = N_AIRS_CHANNELS
      ALLOCATE( Channel_List( n_Channels ), &
                STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error allocating Channel_List array. STAT = ", i5 )' ) Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              FAILURE )
        STOP
      END IF
      Channel_List = (/ ( l, l = 1, N_AIRS_CHANNELS ) /)
      Sensor_Descriptor = 'airs_aqua'

      ! Output data by module, or in one big chunk?
      WRITE( *, FMT     = '( /5x, "Output ALL AIRS channels by module? [y/n] : " )', &
                ADVANCE = 'NO' )
      READ( *, '( a )' ) Answer
      IF ( Answer == 'y' .OR. Answer =='Y' ) THEN
        Output_By_Module = .TRUE.
      ELSE
        Output_By_Module = .FALSE.
      END IF

    ! The 281 channel subset
    CASE ( CHANNEL_SUBSET_281 )
      n_Channels = N_AIRS_SUBSET_281
      ALLOCATE( Channel_List( n_Channels ), &
                STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error allocating Channel_List array. STAT = ", i5 )' ) Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              FAILURE )
        STOP
      END IF
      Channel_List = AIRS_SUBSET_281
      Sensor_Descriptor = 'airs281SUBSET_aqua'

    ! The 324 channel subset
    CASE ( CHANNEL_SUBSET_324 )
      n_Channels = N_AIRS_SUBSET_324
      ALLOCATE( Channel_List( n_Channels ), &
                STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error allocating Channel_List array. STAT = ", i5 )' ) Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              FAILURE )
        STOP
      END IF
      Channel_List = AIRS_SUBSET_324
      Sensor_Descriptor = 'airs324SUBSET_aqua'

    ! A user specified channel subset
    CASE ( USER_SPECIFIED_SUBSET )

      ! Get a channel subset list filename
      WRITE( *, FMT     = '( /5x, "Enter an AIRS channel subset list filename : " )', &
                ADVANCE = 'NO' )
      READ( *, FMT = '( a )' ) List_Filename
      List_Filename = ADJUSTL( List_Filename )

      ! Read the channel subset list file
      Error_Status = Read_List_File( List_Filename, &
                                     User_Channel_List )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error reading list file '//TRIM( List_Filename ), &
                              Error_Status )
        STOP
      END IF

      ! Retrieve the number of subset channels
      n_Channels = Get_List_Size( User_Channel_List )
      IF ( n_Channels < 1 ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'No channels listed in '//TRIM( List_Filename ), &
                              Error_Status )
        STOP
      END IF

      ! Check the number of channels
      IF ( n_Channels < 1 .OR. n_Channels > N_AIRS_CHANNELS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Number of channels listed in '//TRIM( List_Filename )//' outside of valid range.', &
                              Error_Status )
        STOP
      END IF

      ! Allocate the subset list to use
      ALLOCATE( Channel_List( n_Channels ), &
                STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error allocating Channel_List array. STAT = ", i5 )' ) Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              FAILURE )
        STOP
      END IF

      ! Fill the subset list
      DO l = 1, n_Channels
        Error_Status = Get_List_Entry( User_Channel_List, l, Channel_List(l) )
        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error retrieving user subset channel list entry ", i4 )' ) l
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                Error_Status )
          STOP
        END IF
      END DO

      ! Create the sensor descriptor
      WRITE( Sensor_Descriptor, '( i4 )' ) n_Channels
      Sensor_Descriptor = 'airs'//TRIM( ADJUSTL( Sensor_Descriptor ) )//'SUBSET_aqua'
  END SELECT

  ! Create a new SensorInfo list
  SensorInfo_List = New_SensorInfo_List()

  ! Get the channel properties filename
  WRITE( *, FMT     = '( /5x, "Enter the AIRS L2 channel properties filename: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) L2_Filename
  L2_Filename = ADJUSTL( L2_Filename )

  ! ...and read it
  Error_Status = Read_AIRS_ChannelProperties( L2_Filename, &
                                              ChannelProperties )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error reading AIRS L2 channel properties file '//&
                          TRIM( L2_Filename ), &
                          Error_Status )
    STOP
  END IF

  ! Transfer the data into the SensorInfo linked list
  n_Total_Bad_Channels = 0
  By_Module: IF ( Output_By_Module ) THEN

    ! Loop over the AIRS modules
    Module_Loop: DO n = 1, N_AIRS_MODULES

      ! Allocate the SensorInfo structure
      ! for the current module
      Error_Status = Allocate_SensorInfo( N_AIRS_CHANNELS_PER_MODULE(n), &
                                          SensorInfo )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error allocating SensorInfo structure for '//&
                              TRIM( AIRS_MODULE(n) ), &
                              Error_Status ) 
        STOP
      END IF

      ! Assign the scalar members
      SensorInfo%Sensor_Name    = SENSOR_NAME   
      SensorInfo%Satellite_Name = SATELLITE_NAME
      SensorInfo%File_Prefix    = 'airs'//TRIM(AIRS_MODULE(n))//'_aqua'
      SensorInfo%Microwave_Flag = MICROWAVE_FLAG
      SensorInfo%NCEP_Sensor_ID   = NCEP_SENSOR_ID  
      SensorInfo%WMO_Sensor_ID    = WMO_SENSOR_ID   
      SensorInfo%WMO_Satellite_ID = WMO_SATELLITE_ID

      ! Assign the array members
      l1 = AIRS_MODULE_BEGIN_CHANNEL(n)
      l2 = AIRS_MODULE_END_CHANNEL(n)
      SensorInfo%Sensor_Channel = ChannelProperties(l1:l2)%Channel_Number
      SensorInfo%Use_Flag       = 1
      SensorInfo%Noise          = ChannelProperties(l1:l2)%NEdT

      ! Determine the value of the Use_Flag
      n_Bad_Channels = 0
      Channel = 0
      Channel_Loop: DO l = l1, l2
        Channel = Channel + 1
        IF ( ( ChannelProperties(l)%Radiometric_Quality > RAD_FLAG ) .OR. &
             ( ChannelProperties(l)%Bad_Flag == 1 )                       ) THEN
          SensorInfo%Use_Flag(Channel) = 0
          n_Bad_Channels = n_Bad_Channels + 1
        END IF
      END DO Channel_Loop
      WRITE( *, '( 5x, "No. of bad channels in ", a, " : ", i3, " of ", i3 )' ) &
                AIRS_MODULE( n ), n_Bad_Channels, N_AIRS_CHANNELS_PER_MODULE( n )
      n_Total_Bad_Channels = n_Total_Bad_Channels + n_Bad_Channels

      ! Add the current SensorInfo structure to the list
      Error_Status = AddTo_SensorInfo_List( SensorInfo, &
                                            SensorInfo_List, &
                                            Node_Number = n )
      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = FAILURE
        CALL Display_Message( PROGRAM_NAME, &
                              'Error adding '//&
                              TRIM( SensorInfo%Satellite_Name )//' '//&
                              TRIM( SensorInfo%Sensor_Name )//' to SensorInfo list.', &
                              Error_Status )
        STOP
      END IF

      ! Destroy the SensorInfo structure for the next read
      Error_Status = Destroy_SensorInfo( SensorInfo )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error destroying SensorInfo structure for '//&
                              TRIM( AIRS_MODULE(n) )//' module', &
                              Error_Status )
        STOP
      END IF
    END DO Module_Loop

  ELSE By_Module

    ! Allocate the SensorInfo structure
    Error_Status = Allocate_SensorInfo( n_Channels, &
                                        SensorInfo )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error allocating SensorInfo structure.', &
                            Error_Status ) 
      STOP
    END IF

    ! Assign the scalar members
    SensorInfo%Sensor_Name    = SENSOR_NAME   
    SensorInfo%Satellite_Name = SATELLITE_NAME
    SensorInfo%File_Prefix    = Sensor_Descriptor
    SensorInfo%Microwave_Flag = MICROWAVE_FLAG
    SensorInfo%NCEP_Sensor_ID   = NCEP_SENSOR_ID  
    SensorInfo%WMO_Sensor_ID    = WMO_SENSOR_ID   
    SensorInfo%WMO_Satellite_ID = WMO_SATELLITE_ID

    ! Assign the array members
    SensorInfo%Sensor_Channel = ChannelProperties(Channel_List)%Channel_Number
    SensorInfo%Use_Flag       = 1
    SensorInfo%Noise          = ChannelProperties(Channel_list)%NEdT

    ! Determine the value of the Use_Flag
    n_Total_Bad_Channels = 0
    Channel_Loop2: DO l = 1, n_Channels
      IF ( ( ChannelProperties(Channel_List(l))%Radiometric_Quality > RAD_FLAG ) .OR. &
           ( ChannelProperties(Channel_List(l))%Bad_Flag == 1 )                       ) THEN
        SensorInfo%Use_Flag(l) = 0
        n_Total_Bad_Channels = n_Total_Bad_Channels + 1
      END IF
    END DO Channel_Loop2

    ! Add the current SensorInfo structure to the list
    Error_Status = AddTo_SensorInfo_List( SensorInfo, &
                                          SensorInfo_List )
    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( PROGRAM_NAME, &
                            'Error adding '//&
                            TRIM( Sensor_Descriptor )//' to SensorInfo list.', &
                            Error_Status )
      STOP
    END IF

    ! Destroy the SensorInfo structure for the next read
    Error_Status = Destroy_SensorInfo( SensorInfo )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error destroying SensorInfo structure.', &
                            Error_Status )
      STOP
    END IF
  END IF By_Module

  WRITE( *, '( 5x, "Total no. of bad channels  :", i4, " of ", i4 )' ) &
            n_Total_Bad_Channels, n_Channels

  ! Write the SensorInfo data
  SensorInfo_Filename = TRIM( Sensor_Descriptor )//'.SensorInfo'
  Error_Status = Write_SensorInfo( SensorInfo_Filename, &
                                   SensorInfo_List, &
                                   Quiet = 1 )
  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = FAILURE
    CALL display_message( PROGRAM_NAME, &
                          'Error writing SEnsorInfo data to '//TRIM( SensorInfo_Filename ), &
                          Error_Status )
    STOP
  END IF

  ! Destroy the SensorInfo linked list
  Error_Status = Destroy_SensorInfo_List( SensorInfo_List )
  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying SensorInfo linked list.', &
                          Error_Status )
  END IF

END PROGRAM Write_AIRS_SensorInfo
