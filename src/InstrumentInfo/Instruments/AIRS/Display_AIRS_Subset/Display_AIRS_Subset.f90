!
! Display_AIRS_Subset
!
! Program to display subsets of the AIRS L2 channel properties file.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 04-Apr-2005
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Display_AIRS_Subset

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds
  USE Message_Handler
  USE List_File_Utility
  USE AIRS_Define
  USE AIRS_ChannelProperties
  USE AIRS_Subset
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Display_AIRS_Subset'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  INTEGER,      PARAMETER :: N_VALID_SETS = 3
  CHARACTER(*), PARAMETER, DIMENSION( N_VALID_SETS ) :: &
    VALID_SET_NAME = (/ '281 channel set', &
                        '324 channel set', &
                        'User specified ' /)
  INTEGER, PARAMETER :: MAX_N_INVALID = 5


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: IO_Status
  CHARACTER(256) :: Filename
  TYPE(AIRS_ChannelProperties_type), DIMENSION(N_AIRS_CHANNELS) :: ChannelProperties
  INTEGER :: i, Set, Invalid
  CHARACTER(256)               :: List_Filename
  TYPE(Integer_List_File_type) :: User_Subset_List
  INTEGER :: l
  INTEGER                            :: n_Subset_Channels
  INTEGER, DIMENSION(:), ALLOCATABLE :: Subset_List

  ! Output program header
  CALL Program_MEssage(PROGRAM_NAME, &
                       'Program to display subsets of the AIRS L2 channel properties file.', &
                       '$Revision: 1.2' )

  ! Get AIRS L2 channel properties filename
  WRITE( *, FMT     = '( /5x, "Enter an AIRS L2 channel properties filename: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) Filename
  Filename = ADJUSTL( Filename )

  ! ...and read it.
  Error_Status = Read_AIRS_ChannelProperties( Filename, &
                                              ChannelProperties )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading AIRS L2 channel properties file '//&
                          TRIM( Filename ), &
                          Error_Status )
    STOP
  END IF

  ! Select a subset list
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

    ! The 281 channel subset
    CASE ( 1 )
      n_Subset_Channels = N_AIRS_SUBSET_281
      ALLOCATE( Subset_List( n_Subset_Channels ), &
                STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error allocating Subset_List array. STAT = ", i5 )' ) Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              FAILURE )
        STOP
      END IF
      Subset_List = AIRS_SUBSET_281

    ! The 324 channel subset
    CASE ( 2 )
      n_Subset_Channels = N_AIRS_SUBSET_324
      ALLOCATE( Subset_List( n_Subset_Channels ), &
                STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error allocating Subset_List array. STAT = ", i5 )' ) Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              FAILURE )
        STOP
      END IF
      Subset_List = AIRS_SUBSET_324

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
  END SELECT

  ! Output the data
  WRITE( *, '(/,"    +---------------------------Subset channel number")' )
  WRITE( *, '(  "    |    +----------------------Channel number")' )
  WRITE( *, '(  "    |    |     +----------------SRF centroid Frequnecy (cm-1)")' )
  WRITE( *, '(  "    |    |     |     +----------Array name")' )
  WRITE( *, '(  "    |    |     |     |      +---Calibration channel index")' )
  WRITE( *, '(  "    |    |     |     |      |                                       +-RTA Fitting Error")' )
  WRITE( *, '(  "    |    |     |     |      |    NeDT  FWHM   Cij      Centroid     |   AB_State")' )
  WRITE( *, '(  "    |    |     |     |      |      |     |     |        x       y   |      | Rad Qual")' )
  WRITE( *, '(  "    |    |     |     |      |      |     |     |        |       |   |      |  |  L2_ignore")' )
  WRITE( *, '(  "    |    |     |     |      |      |     |     |        |       |   |      |  |  | Comments")' )
  WRITE( *, '(  "    |    |     |     |      |      |     |     |        |       |   |      |  |  |   |")' )
  DO l = 1, n_Subset_Channels
    WRITE( *, '(i5, i5,f9.3,1x,a5,i5,f7.4,f6.3,f8.4,2f8.1,f6.3,i3,i3,i3,1x,a8)' ) &
      l, ChannelProperties( Subset_List(l) )
  END DO

END PROGRAM Display_AIRS_Subset
