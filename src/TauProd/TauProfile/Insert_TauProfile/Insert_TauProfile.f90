!
! Insert_TauProfile
!
! Program to insert TauProfile data from one file into another, e.g. from
! a TauProfile file for a single profile, angle, molecule set into a 
! "master" TauProfile file containing all profiles, angles, and molecule
! sets.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Jan-2006
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Insert_TauProfile


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE String_Utility

  USE SensorInfo_Define
  USE SensorInfo_LinkedList
  USE SensorInfo_IO

  USE TauProfile_Define
  USE TauProfile_netCDF_IO


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Insert_TauProfile'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &

  INTEGER, PARAMETER :: N_DIRECTIONS = 2
  CHARACTER( * ),  PARAMETER, DIMENSION( N_DIRECTIONS ) :: &
    DIRECTION_NAME = (/ 'upwelling  ', &
                        'downwelling' /)

  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  CHARACTER( 256 )             :: SensorInfo_Filename
  TYPE( SensorInfo_type )      :: SensorInfo
  TYPE( SensorInfo_List_type ) :: SensorInfo_List
  INTEGER :: n_Sensors, iDir, i, j, m, n
  CHARACTER( 256 ) :: Path
  CHARACTER( 256 ) :: InFile
  CHARACTER( 256 ) :: OutFile
  CHARACTER( 5000 ) :: History
  CHARACTER( 5000 ) :: Comment, New_Comment, Tmp_Comment
  TYPE( TauProfile_type ) :: TauProfile
  INTEGER :: i_nK, i_nL, i_nI, i_nM, i_nJ
  INTEGER :: i_NCEP_Sensor_ID
  CHARACTER( 80 ) :: i_ID_Tag, i_Sensor_Name, i_Platform_Name
  INTEGER,         DIMENSION(:), ALLOCATABLE :: i_Channel_List
  REAL( fp_kind ), DIMENSION(:), ALLOCATABLE :: i_Angle_List
  INTEGER,         DIMENSION(:), ALLOCATABLE :: i_Profile_List
  INTEGER,         DIMENSION(:), ALLOCATABLE :: i_Molecule_Set_List
  INTEGER :: o_nK, o_nL, o_nI, o_nM, o_nJ
  INTEGER :: o_NCEP_Sensor_ID
  CHARACTER( 80 ) :: o_ID_Tag, o_Sensor_Name, o_Platform_Name
  INTEGER,         DIMENSION(:), ALLOCATABLE :: o_Channel_List
  REAL( fp_kind ), DIMENSION(:), ALLOCATABLE :: o_Angle_List
  INTEGER,         DIMENSION(:), ALLOCATABLE :: o_Profile_List
  INTEGER,         DIMENSION(:), ALLOCATABLE :: o_Molecule_Set_List

  ! Output program header
  CALL Program_Message(PROGRAM_NAME, &
                       'Program to insert data from one (smaller) netCDF format '//&
                       'TauProfile file into another (larger) netCDF format '//&
                       'TauProfile file', &
                       '$Revision: 1.7 $' )


  !#----------------------------------------------------------------------------#
  !#                      -- READ THE SensorInfo FILE --                        #
  !#----------------------------------------------------------------------------#

  ! ---------------------------
  ! Get the SensorInfo filename
  ! ---------------------------

  WRITE( *, FMT     = '( /5x, "Enter the SensorInfo filename: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) SensorInfo_Filename

  SensorInfo_Filename = ADJUSTL( SensorInfo_Filename )


  ! ------------------------
  ! Read the SensorInfo data
  ! ------------------------

  ! -- First create a new list
  SensorInfo_List = New_SensorInfo_List()

  ! -- Now fill it
  Error_Status = Read_SensorInfo( SensorInfo_Filename, &
                                  SensorInfo_List, &
                                  Quiet = 1 )
                               
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading SensorInfo file '//TRIM( SensorInfo_Filename ), &
                          FAILURE )
    STOP
  END IF

  ! -- Count the number of sensors
  n_Sensors = Count_SensorInfo_Nodes( SensorInfo_List )

  IF ( n_Sensors < 1 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'SensorInfo_List is empty.', &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                   -- ENTER THE PATH OF THE DATA TO INSERT --               #
  !#----------------------------------------------------------------------------#

  WRITE( *, FMT    ='( /5x, "Enter the path of the datafiles to insert [e.g. ./TauProfile_data] :" )', &
            ADVANCE='NO' )
  READ( *, '(a)' ) Path

  WRITE( *, '( // )' )



  !#----------------------------------------------------------------------------#
  !#            -- BEGIN MAIN LOOP OVER SENSOR/PLATFORM FILES --                #
  !#----------------------------------------------------------------------------#

  Sensor_Loop: DO n = 1, n_Sensors



    !#--------------------------------------------------------------------------#
    !#          -- GET THE CURRENT SensorInfo DATA FROM THE LIST --             #
    !#--------------------------------------------------------------------------#

    Error_Status = GetFrom_SensorInfo_List( SensorInfo_List, &
                                            n, &
                                            SensorInfo )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error retrieving SensorInfo data for sensor # ", i5 )' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- LOOP OVER DIRECTIONS --                         #
    !#--------------------------------------------------------------------------#

    Direction_Loop: DO iDir = 1, N_DIRECTIONS


      ! ------------------------------------
      ! Construct input and output filenames
      ! ------------------------------------

      OutFile = TRIM(DIRECTION_NAME(iDir))//'.'//TRIM(SensorInfo%File_Prefix)//'.TauProfile.nc'
      InFile  = TRIM(Path)//'/'//TRIM(OutFile)


      ! ---------------------------
      ! Check that both files exist
      ! ---------------------------

      IF ( .NOT. File_Exists( InFile  ) .OR. &
           .NOT. File_Exists( OutFile )      ) CYCLE Direction_Loop


      ! ---------------------------------
      ! Inquire the input TauProfile file
      ! ---------------------------------

      ! -- Get the dimensions
      Error_Status = Inquire_TauProfile_netCDF( Infile, &
                                                n_Layers        = i_nK, &
                                                n_Channels      = i_nL, &
                                                n_Angles        = i_nI, &
                                                n_Profiles      = i_nM, &
                                                n_Molecule_Sets = i_nJ, &
                                                NCEP_Sensor_ID  = i_NCEP_Sensor_ID, &
                                                ID_Tag          = i_ID_Tag,         &
                                                Sensor_Name     = i_Sensor_Name,    &
                                                Platform_Name   = i_Platform_Name   )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error inquiring netCDF TauProfile file '//&
                              TRIM(InFile)//&
                              ' for dimensions and global attributes', &
                              Error_Status )
        STOP
      END IF

      ! -- Allocate the dimension list arrays
      ALLOCATE( i_Channel_List( i_nL ), &
                i_Angle_List( i_nI ), &
                i_Profile_List( i_nM ), &
                i_Molecule_Set_List( i_nJ ), &
                STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error allocating dimension list arrays for input file ", a, &
                          &". STAT = ", i5 )' ) &
                        TRIM( InFile ), Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF

      ! -- Get the dimension lists
      Error_Status = Inquire_TauProfile_netCDF( InFile, &
                                                Channel_List      = i_Channel_List,     &
                                                Angle_List        = i_Angle_List,       &
                                                Profile_List      = i_Profile_List,     &
                                                Molecule_Set_List = i_Molecule_Set_List )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error inquiring netCDF TauProfile file '//&
                              TRIM(InFile)//&
                              ' for dimensions lists', &
                              Error_Status )
        STOP
      END IF


      ! ----------------------------------
      ! Inquire the output TauProfile file
      ! ----------------------------------

      ! -- Get the dimensions
      Error_Status = Inquire_TauProfile_netCDF( Outfile, &
                                                n_Layers        = o_nK, &
                                                n_Channels      = o_nL, &
                                                n_Angles        = o_nI, &
                                                n_Profiles      = o_nM, &
                                                n_Molecule_Sets = o_nJ, &
                                                NCEP_Sensor_ID  = o_NCEP_Sensor_ID, &
                                                History         = History, &
                                                Comment         = Comment, &
                                                ID_Tag          = o_ID_Tag,         &
                                                Sensor_Name     = o_Sensor_Name,    &
                                                Platform_Name   = o_Platform_Name   )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error inquiring netCDF TauProfile file '//&
                              TRIM(OutFile)//&
                              ' for dimensions and global attributes', &
                              Error_Status )
        STOP
      END IF

      ! -- Allocate the dimension list arrays
      ALLOCATE( o_Channel_List( o_nL ), &
                o_Angle_List( o_nI ), &
                o_Profile_List( o_nM ), &
                o_Molecule_Set_List( o_nJ ), &
                STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error allocating dimension list arrays for output file ", a, &
                          &". STAT = ", i5 )' ) &
                        TRIM( OutFile ), Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              FAILURE )
        STOP
      END IF

      ! -- Get the dimension lists
      Error_Status = Inquire_TauProfile_netCDF( OutFile, &
                                                Channel_List      = o_Channel_List,     &
                                                Angle_List        = o_Angle_List,       &
                                                Profile_List      = o_Profile_List,     &
                                                Molecule_Set_List = o_Molecule_Set_List )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error inquiring netCDF TauProfile file '//&
                              TRIM(OutFile)//&
                              ' for dimensions lists', &
                              Error_Status )
        STOP
      END IF


      ! ----------------------------
      ! Compare the TauProfile files
      ! ----------------------------

      ! -- Layer and channel dimensions must agree
      IF ( i_nK /= o_nK .OR. &
           i_nL /= o_nL      ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Incompatible layer/channel dimensions for '//&
                              TRIM(SensorInfo%File_Prefix)//&
                              ' TauProfile files', &
                              FAILURE )
        STOP
      END IF

      ! -- The channel number lists must be the same
      IF ( ANY( (i_Channel_List - o_Channel_List ) /= 0 ) ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Different channel number lists for '//&
                              TRIM(SensorInfo%File_Prefix)//&
                              ' TauProfile files', &
                              FAILURE )
        STOP
      END IF

      ! -- Angle value
      DO i = 1, i_nI
        IF ( COUNT( o_Angle_List == i_Angle_List(i) ) /= 1 ) THEN
          WRITE( Message, '( "Input file angle #, ", i3, "(", f4.2, ") not present in output file ", a )' ) &
                          i, i_Angle_List(i), TRIM( OutFile )
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                FAILURE )
          STOP
        END IF
      END DO

      ! -- Profile number
      DO m = 1, i_nM
        IF ( COUNT( o_Profile_List == i_Profile_List(m) ) /= 1 ) THEN
          WRITE( Message, '( "Input file profile #, ", i3, " not present in output file ", a )' ) &
                          i_Profile_List(m), TRIM( OutFile )
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                FAILURE )
          STOP
        END IF
      END DO

      ! -- Molecule set number
      DO j = 1, i_nJ
        IF ( COUNT( o_Molecule_Set_List == i_Molecule_Set_List(j) ) /= 1 ) THEN
          WRITE( Message, '( "Input file molecule set #, ", i3, " not present in output file ", a )' ) &
                          i_Molecule_Set_List(j), TRIM( OutFile )
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                FAILURE )
          STOP
        END IF
      END DO

      ! -- Other IDs/names/etc
      IF ( i_NCEP_Sensor_ID /= o_NCEP_Sensor_ID ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Different NCEP Sensor IDs for '//&
                              TRIM(SensorInfo%File_Prefix)//&
                              ' TauProfile files', &
                              FAILURE )
        STOP
      END IF

      IF ( TRIM(i_ID_Tag) /= TRIM(o_ID_Tag) ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Different profile set ID tag for '//&
                              TRIM(SensorInfo%File_Prefix)//&
                              ' TauProfile files', &
                              FAILURE )
        STOP
      END IF

      IF ( StrUpCase(TRIM(i_Sensor_Name)) /= StrUpCase(TRIM(o_Sensor_Name)) ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Different sensor identifiers for '//&
                              TRIM(SensorInfo%File_Prefix)//&
                              ' TauProfile files', &
                              FAILURE )
        STOP
      END IF

      IF ( StrUpCase(TRIM(i_Platform_Name)) /= StrUpCase(TRIM(o_Platform_Name)) ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Different platform identifiers for '//&
                              TRIM(SensorInfo%File_Prefix)//&
                              ' TauProfile files: '//TRIM(i_Platform_Name)//&
                              ' and '//TRIM(o_Platform_Name), &
                              FAILURE )
        STOP
      END IF



      ! -------------------------
      ! Read the input TauProfile
      ! -------------------------

      Error_Status = Read_TauProfile_netCDF( InFile, &
                                             TauProfile )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error reading netCDF TauProfile file '//&
                              TRIM( InFile ), &
                              Error_Status )
        STOP
      END IF


      ! -----------------------------------------------------------
      ! Write the transmittance data. The data is written as rank-2
      ! arrays looping over molecule set, profile, and angle.
      ! -----------------------------------------------------------

      j_Loop: DO j = 1, i_nJ
        m_Loop: DO m = 1, i_nM
          i_Loop: DO i = 1, i_nI

            Error_Status = Write_TauProfile_netCDF( OutFile, &
                                                    TauProfile%Tau(:,:,i,m,j), &
                                                    i_Angle_List(i), &
                                                    i_Profile_List(m), &
                                                    i_Molecule_Set_List(j) )

            IF ( Error_Status /= SUCCESS ) THEN
              WRITE( Message, '( "Error inserting angle ", i3, "(", f4.2, "), profile ", i3, &
                                &" and molecule set ", i3, " into ", a )' ) &
                              i, i_Angle_List(i), i_Profile_List(m), i_Molecule_Set_List(j), &
                              TRIM( OutFile )
              CALL Display_Message( PROGRAM_NAME, &
                                    TRIM( Message ), &
                                    Error_Status )
              STOP
            END IF

          END DO i_Loop
        END DO m_Loop
      END DO j_Loop


      ! ---------------------------------------
      ! Modify the output file TauProfile GAtts
      ! ---------------------------------------

      WRITE( New_Comment, '( "Updated data for angle(s): ", 7(f5.3,:) )' ) i_Angle_List
      WRITE( Tmp_Comment, '( ", profile(s): ", 99(i3,:) )' ) i_Profile_List
      New_Comment = TRIM( New_Comment )//TRIM( Tmp_Comment )
      WRITE( Tmp_Comment, '( ", molecule set(s): ", 99(i4,:) )' ) i_Molecule_Set_List
      New_Comment = TRIM( New_Comment )//TRIM( Tmp_Comment )


      Error_Status = Modify_TauProfile_GAtts( OutFile, &
                                              Sensor_Name   = TRIM( SensorInfo%Sensor_Name ), &
                                              Platform_Name = TRIM( SensorInfo%Satellite_Name ), &
                                              Title = TRIM( SensorInfo%Sensor_Name )//' '//&
                                                      TRIM(DIRECTION_NAME(iDir))//&
                                                      ' transmittances for '//&
                                                      TRIM( SensorInfo%Satellite_Name ), &
                                              History = PROGRAM_RCS_ID//'; '//&
                                                        TRIM( History ), &
                                              Comment = TRIM( New_Comment )//'; '//&
                                                        TRIM( Comment ) )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error modifying GAtts for netCDF TauProfile file '//&
                              TRIM( OutFile ), &
                              Error_Status )
        STOP
      END IF

      WRITE( *, '( 5x, a, ". ", a )' ) TRIM( OutFile ), TRIM( New_Comment )


      ! ------------------------------------
      ! Deallocate the dimension list arrays
      ! ------------------------------------

      DEALLOCATE( i_Channel_List, i_Angle_List, i_Profile_List, i_Molecule_Set_List, &
                  o_Channel_List, o_Angle_List, o_Profile_List, o_Molecule_Set_List, &
                  STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error deallocating dimension list arrays for input file ", a, &
                          &". STAT = ", i5 )' ) &
                        TRIM( InFile ), Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF

    END DO Direction_Loop

  END DO Sensor_Loop


  !#----------------------------------------------------------------------------#
  !#            -- DESTROY THE SensorInfo STRUCTURE AND LINKED LIST --          #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_SensorInfo( SensorInfo )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying SensorInfo data structure.', &
                          WARNING )
  END IF


  Error_Status = Destroy_SensorInfo_List( SensorInfo_List )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          '( "Error destroying SensorInfo_List.', &
                          WARNING )
  END IF

END PROGRAM Insert_TauProfile
