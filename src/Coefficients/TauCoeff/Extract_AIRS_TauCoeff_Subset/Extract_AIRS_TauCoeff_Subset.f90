!
! Extract_AIRS_TauCoeff_Subset
!
! Program to extract the AIRS channel subset from the individual
! AIRS module netCDF format TauCoeff data files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 28-Jan-2004
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Extract_AIRS_TauCoeff_Subset

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Message_Handler,       ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                   Display_Message, Program_Message
  USE Compare_Float_Numbers, ONLY: Compare_Float
  USE List_File_Utility,     ONLY: Integer_List_File_type, &
                                   Read_List_File, &
                                   Get_List_Size, Get_List_Entry
  USE TauCoeff_Define,       ONLY: TauCoeff_type, &
                                   Allocate_TauCoeff, Destroy_TauCoeff
  USE TauCoeff_netCDF_IO,    ONLY: Inquire_TauCoeff_netCDF, &
                                   Read_TauCoeff_netCDF, &
                                   Write_TauCoeff_netCDF
  USE AIRS_Define,           ONLY: N_AIRS_MODULES, &
                                   N_AIRS_CHANNELS, &
                                   AIRS_MODULE
  USE AIRS_Subset,           ONLY: AIRS_Subset_type, &
                                   N_AIRS_SUBSET_281, &
                                   N_AIRS_SUBSET_324, &
                                   AIRS_SUBSET_281, &
                                   AIRS_SUBSET_324, &
                                   AIRS_SUBSET_281_COMMENT, &
                                   AIRS_SUBSET_324_COMMENT, &
                                   Destroy_Airs_Subset, &
                                   Index_AIRS_Subset
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Extract_AIRS_TauCoeff_Subset'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Extract_AIRS_TauCoeff_Subset.f90,v 1.10 2006/06/15 15:24:27 wd20pd Exp $'
  INTEGER,        PARAMETER :: N_VALID_SETS = 3
  CHARACTER( * ), PARAMETER, DIMENSION( N_VALID_SETS ) :: &
    VALID_SET_NAME = (/ '281 channel set', &
                        '324 channel set', &
                        'User specified ' /)
  INTEGER, PARAMETER :: MAX_N_INVALID = 5


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: List_Filename
  CHARACTER(256) :: Sensor_Descriptor
  CHARACTER(256) :: In_Filename
  CHARACTER(256) :: Out_Filename
  CHARACTER(256) :: Sensor_Name
  CHARACTER(256) :: Platform_Name
  CHARACTER(256) :: ID_Tag
  CHARACTER(5000) :: History
  CHARACTER(5000) :: Comment
  CHARACTER(256) :: Subset_Comment
  INTEGER :: i, Set, Invalid
  LOGICAL :: First_Module
  INTEGER :: n_Orders
  INTEGER :: n_Predictors     
  INTEGER :: n_Absorbers      
  INTEGER :: Release
  INTEGER :: Version
  INTEGER :: Error_Status
  INTEGER :: IO_Status
  INTEGER :: Allocate_Status
  INTEGER :: j, l, l1, l2
  INTEGER :: n_Subset_Channels
  INTEGER, DIMENSION(:), ALLOCATABLE :: Subset_List
  TYPE( Integer_List_File_type ) :: User_Subset_List
  TYPE( TauCoeff_type ) :: In_TauCoeff
  TYPE( TauCoeff_type ) :: Out_TauCoeff
  TYPE( AIRS_Subset_type ) :: Subset

  ! Output program header
  CALL Program_Message(PROGRAM_NAME, &
                       'Program to extract the AIRS channel SUBSET transmittance '//&
                       'coefficient data from the individual module netCDF '//&
                       'TauCoeff files and write them to a separate netCDF '//&
                       'datafile.', &
                       '$Revision: 1.10 $' )

  ! Select a subset set
  Invalid = 0
  Set_Loop: DO
    ! Prompt user to select a subset set 
    WRITE( *, '( /5x, "Select an AIRS channel subset" )' )
    DO i = 1, N_VALID_SETS
      WRITE( *, '( 10x, i1, ") ", a )' ) i, VALID_SET_NAME(i)
    END DO
    WRITE( *, FMT     = '( 5x, "Enter choice: " )', &
              ADVANCE = 'NO' )
    READ( *, FMT    = '( i2 )', &
             IOSTAT = IO_Status ) Set

    ! Check the input
    IF ( IO_Status /= 0 ) THEN
      Invalid = Invalid + 1
    ELSE
      IF ( Set < 1 .OR. Set > N_VALID_SETS ) THEN
        Invalid = Invalid + 1
      ELSE
        EXIT Set_Loop
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

  END DO Set_Loop

  ! Get the required channels list
  SELECT CASE ( Set )

    ! The 281 subset
    CASE ( 1 )
      n_Subset_Channels = N_AIRS_SUBSET_281
      Subset_Comment    = AIRS_SUBSET_281_COMMENT
      ALLOCATE( Subset_List( n_Subset_Channels ), &
                STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error allocating Subset_List array. STAT = ", i5 )' ) Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              FAILURE )
        STOP
      END IF
      Subset_List       = AIRS_SUBSET_281
      Sensor_Descriptor = 'airs281SUBSET_aqua'

    ! The 324 subset
    CASE ( 2 )
      n_Subset_Channels = N_AIRS_SUBSET_324
      Subset_Comment    = AIRS_SUBSET_324_COMMENT
      ALLOCATE( Subset_List( n_Subset_Channels ), &
                STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error allocating Subset_List array. STAT = ", i5 )' ) Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              FAILURE )
        STOP
      END IF
      Subset_List       = AIRS_SUBSET_324
      Sensor_Descriptor = 'airs324SUBSET_aqua'

    ! A user specified channel subset
    CASE ( 3 )
      ! Get a channel subset list filename
      WRITE( *, FMT     = '( /5x, "Enter an AIRS channel subset list filename : " )', &
                ADVANCE = 'NO' )
      READ( *, FMT = '( a )' ) List_Filename
      List_Filename = ADJUSTL( List_Filename )

      ! Read the channel subset list file
      Error_Status = Read_List_File( List_Filename, &
                                     User_Subset_List )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error reading list file '//TRIM( List_Filename ), &
                              Error_Status )
        STOP
      END IF

      ! Retrieve the number of subset channels
      n_Subset_Channels = Get_List_Size( User_Subset_List )
      IF ( n_Subset_Channels < 1 ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'No channels listed in '//TRIM( List_Filename ), &
                              Error_Status )
        STOP
      END IF

      ! Check the number of channels
      IF ( n_Subset_Channels < 1 .OR. n_Subset_Channels > N_AIRS_CHANNELS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Number of channels listed in '//TRIM( List_Filename )//' outside of valid range.', &
                              Error_Status )
        STOP
      END IF

      ! Allocate the subset list to use
      ALLOCATE( Subset_List( n_Subset_Channels ), &
                STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error allocating Subset_List array. STAT = ", i5 )' ) Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              FAILURE )
        STOP
      END IF

      ! Fill the subset list
      DO l = 1, n_Subset_Channels
        Error_Status = Get_List_Entry( User_Subset_List, l, Subset_List(l) )
        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error retrieving user subset channel list entry ", i4 )' ) l
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                Error_Status )
          STOP
        END IF
      END DO

      ! Create the sensor descriptor
      WRITE( Sensor_Descriptor, '( i4 )' ) n_Subset_Channels
      Sensor_Descriptor = 'airs'//TRIM( ADJUSTL( Sensor_Descriptor ) )//'SUBSET_aqua'

  END SELECT

  ! Initialise the start index for output
  ! and the "initial module" flag
  l1 = 1
  First_Module = .TRUE.

  ! Begin module loop
  Module_Loop: DO l = 1, N_AIRS_MODULES

    ! Determine the subset channel indices
    ! for the current module
    Error_Status = Index_AIRS_Subset( l, Subset_List, Subset )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error extracting subset channel indices for module '//&
                            TRIM( AIRS_MODULE( l ) ), &
                            Error_Status )
      STOP
    END IF

    ! Output the number of channels to extract and their numbers
    WRITE( *, '(/10x, "There are ", i3, " channels to be extracted from module ", a, ":" )' ) &
              Subset%n_Channels, TRIM( AIRS_MODULE( l ) )

    ! Only read the input file if channels are requested
    Non_Zero_n_Channels: IF ( Subset%n_Channels > 0 ) THEN

      ! Output the list of channel numbers to extract
      WRITE( *, FMT = '( 10x, 10i5 )' ) Subset%Channel_Number

      ! Define the filename
      In_Filename = 'airs'//&
                       TRIM( AIRS_MODULE( l ) )//&
                       '_aqua.TauCoeff.nc'

      ! Get the file release/version info
      ! dimensions, and global attributes
      ! Get them for the first module
      IF ( First_Module ) THEN

        ! **NOTE: No Update of First_Module here. It's used later**

        ! Inquire the file
        Error_Status = Inquire_TauCoeff_netCDF( TRIM( In_Filename ), &
                                                n_Orders      = n_Orders, &
                                                n_Predictors  = n_Predictors, &
                                                n_Absorbers   = n_Absorbers, &
                                                Release       = Out_TauCoeff%Release, &
                                                Version       = Out_TauCoeff%Version, &
                                                History       = History, &
                                                Sensor_Name   = Sensor_Name, &
                                                Platform_Name = Platform_Name, &
                                                Comment       = Comment, &
                                                ID_Tag        = ID_Tag )
        IF ( Error_Status /= SUCCESS ) THEN
          CALL Display_Message( PROGRAM_NAME, &
                                'Error inquiring the input netCDF TauCoeff file '//&
                                TRIM( In_Filename ), &
                                Error_Status )
          STOP
        END IF

        ! Append onto the comment attribute.
        Comment = TRIM( Subset_Comment )//'; '//TRIM( Comment )

        ! Allocate the output structure
        Error_Status = Allocate_TauCoeff( n_Orders, &
                                          n_Predictors, &
                                          n_Absorbers, &
                                          n_Subset_Channels, &
                                          Out_TauCoeff )
        IF ( Error_Status /= SUCCESS ) THEN
          CALL Display_Message( PROGRAM_NAME, &
                                'Error allocating output TauCoeff data structure.', &
                                Error_Status )
          STOP
        END IF

      END IF

      ! Get the Release/Version info for comparison
      Error_Status = Inquire_TauCoeff_netCDF( TRIM( In_Filename ), &
                                              Release = Release, &
                                              Version = Version )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error inquiring the input netCDF TauCoeff file '//&
                              TRIM( In_Filename )//' for Release/Version info.', &
                              Error_Status )
        STOP
      END IF

      ! Check the Release and Version numbers
      !
      ! Check the Release value. If different - issue error and stop
      IF ( Out_TauCoeff%Release /= Release ) THEN
        WRITE( Message, '( "Input file ", a, " Release, ", i2, &
                          &", is different from previous file value, ", i2, "." )' ) &
                        TRIM( In_Filename ), Release, Out_TauCoeff%Release
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              FAILURE )
        STOP
      END IF
      ! Check the Version value. If different - issue warning and continue, 
      ! but modify the Comment global attribute field for output
      IF ( Out_TauCoeff%Version /= Version ) THEN
        WRITE( Message, '( "Input file ", a, " Version, ", i2, &
                          &", is different from previous file value, ", i2, "." )' ) &
                        TRIM( In_Filename ), Version, Out_TauCoeff%Version
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              WARNING )
        Comment = TRIM( Message )//'; '//TRIM( Comment )
      END IF

      ! Read the data
      Error_Status = Read_TauCoeff_netCDF( In_Filename, &
                                           In_TauCoeff )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error reading netCDF AIRS TauCoeff file '//&
                              TRIM( In_Filename ), &
                              Error_Status )
        STOP
      END IF
    
      ! Copy or check the absorber id and alpha data
      IF ( First_Module ) THEN

        ! Now update the First_Module flag
        First_Module = .FALSE.

        ! Simply copy these for the first module
        Out_TauCoeff%Absorber_ID = In_TauCoeff%Absorber_ID
        Out_TauCoeff%Alpha       = In_TauCoeff%Alpha
        Out_TauCoeff%Alpha_C1    = In_TauCoeff%Alpha_C1
        Out_TauCoeff%Alpha_C2    = In_TauCoeff%Alpha_C2

      ELSE

        DO j = 1, In_TauCoeff%n_Absorbers

          ! Check Absorber IDs
          IF ( In_TauCoeff%Absorber_ID(j) /= Out_TauCoeff%Absorber_ID(j) ) THEN
            Error_Status = FAILURE
            WRITE( Message, '( "Absorber #",i2," ID values are different for module ",a)' ) &
                            j, TRIM(AIRS_Module(l))
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM(Message), &
                                  Error_Status )
            STOP
          END IF

          ! Check Alpha value
          IF ( .NOT. Compare_Float( In_TauCoeff%Alpha(j), Out_TauCoeff%Alpha(j) ) ) THEN
            Error_Status = FAILURE
            WRITE( Message, '( "Absorber #",i2," Alpha values are different for module ",a)' ) &
                            j, TRIM(AIRS_Module(l))
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM(Message), &
                                  Error_Status )
            STOP
          END IF

          ! Check Alpha_C1 value
          IF ( .NOT. Compare_Float( In_TauCoeff%Alpha_C1(j), Out_TauCoeff%Alpha_C1(j) ) ) THEN
            Error_Status = FAILURE
            WRITE( Message, '( "Absorber #",i2," Alpha_C1 values are different for module ",a)' ) &
                            j, TRIM(AIRS_Module(l))
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM(Message), &
                                  Error_Status )
            STOP
          END IF

          ! Check Alpha_C2 value
          IF ( .NOT. Compare_Float( In_TauCoeff%Alpha_C2(j), Out_TauCoeff%Alpha_C2(j) ) ) THEN
            Error_Status = FAILURE
            WRITE( Message, '( "Absorber #",i2," ALpha_C2 values are different for module ",a)' ) &
                            j, TRIM(AIRS_Module(l))
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM(Message), &
                                  Error_Status )
            STOP
          END IF

        END DO

      END IF

      ! Copy the required channel's data
      l2 = l1 + Subset%n_Channels - 1
      Out_TauCoeff%Sensor_Descriptor( l1:l2 ) = TRIM( Sensor_Descriptor )
      Out_TauCoeff%NCEP_Sensor_ID( l1:l2 )    = In_TauCoeff%NCEP_Sensor_ID( Subset%Channel_Index )
      Out_TauCoeff%WMO_Satellite_ID( l1:l2 )  = In_TauCoeff%WMO_Satellite_ID( Subset%Channel_Index )
      Out_TauCoeff%WMO_Sensor_ID( l1:l2 )     = In_TauCoeff%WMO_Sensor_ID( Subset%Channel_Index )
      Out_TauCoeff%Sensor_Channel( l1:l2 )    = In_TauCoeff%Sensor_Channel( Subset%Channel_Index )
      Out_TauCoeff%Order_Index(:,:,l1:l2)     = In_TauCoeff%Order_Index(:,:,Subset%Channel_Index)
      Out_TauCoeff%Predictor_Index(:,:,l1:l2) = In_TauCoeff%Predictor_Index(:,:,Subset%Channel_Index)
      Out_TauCoeff%C(:,:,:,l1:l2)             = In_TauCoeff%C(:,:,:,Subset%Channel_Index)
      l1 = l2 + 1

      ! Destroy the input TauCoeff structure
      ! for the next module read
      Error_Status = Destroy_TauCoeff( In_TauCoeff )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error destroying TauCoeff structure for input from '//&
                              TRIM( In_Filename ), &
                              Error_Status )
        STOP
      END IF

    END IF Non_Zero_n_Channels

    ! Destroy the AIRS subset structure
    Error_Status = Destroy_AIRS_Subset( Subset )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error destroying AIRS_Subset structure for input from '//&
                            TRIM( In_Filename ), &
                            Error_Status )
      STOP
    END IF

  END DO Module_Loop

  ! The output filename
  Out_Filename = TRIM( Sensor_Descriptor )//'.TauCoeff.nc'

  ! Set the number of sensors
  Out_TauCoeff%n_Sensors = 1

  ! Write the data
  WRITE( *, '(/10x, "Creating the output file..." )' )
  Error_Status = Write_TauCoeff_netCDF( TRIM( Out_Filename ), &
                                        Out_TauCoeff, &
                                        Title         = 'Optical depth coefficients for '//&
                                                        TRIM( Platform_Name )//' '//&
                                                        TRIM( Subset_Comment ), &
                                        History       = PROGRAM_RCS_ID//'; '//&
                                                        TRIM( History ), &
                                        Sensor_Name   = TRIM( Sensor_Name ), &
                                        Platform_Name = TRIM( Platform_Name ), &
                                        Comment       = 'Data extracted from the individual '//&
                                                        'AIRS module TauCoeff datafiles.; '//&
                                                        TRIM( Comment ), &
                                        ID_Tag        = TRIM( ID_Tag ) )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing the AIRS subset TauCoeff file '//&
                          TRIM( Out_Filename ), &
                          FAILURE )
    STOP
  END IF

  ! Destroy the output TauCoeff structure
  Error_Status = Destroy_TauCoeff( Out_TauCoeff )
  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying TauCoeff structure for output to '//&
                          TRIM( Out_Filename ), &
                          Error_Status )
  END IF

END PROGRAM Extract_AIRS_TauCoeff_Subset
