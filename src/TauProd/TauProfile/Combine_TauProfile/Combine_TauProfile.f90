
PROGRAM Combine_TauProfile


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler

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

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Combine_TauProfile'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Combine_TauProfile.f90,v 1.4 2006/06/30 16:47:16 dgroff Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  CHARACTER( * ),  PARAMETER, DIMENSION( 2 ) :: DIRECTION_NAME = (/ 'upwelling  ', &
                                                                    'downwelling' /)

  ! -- Maximum dimension values
  INTEGER, PARAMETER :: MAX_N_LAYERS        = 100
  INTEGER, PARAMETER :: MAX_N_LEVELS        = MAX_N_LAYERS + 1
  INTEGER, PARAMETER :: MAX_N_ANGLES        = 7
  INTEGER, PARAMETER :: MAX_N_PROFILES      = 52
  INTEGER, PARAMETER :: MAX_N_MOLECULE_SETS = 20


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  CHARACTER( 256 )  :: ID_Tag
  CHARACTER( 5000 ) :: History
  CHARACTER( 5000 ) :: Comment

  CHARACTER( 256 ) :: SensorInfo_Filename
  CHARACTER( 256 ) :: TauProfile_Filename
  CHARACTER( 256 ) :: Output_TauProfile_Filename

  INTEGER :: IO_Status
  INTEGER :: Error_Status

  INTEGER ::    n_Layers, n_Levels
  INTEGER :: i, n_Angles
  INTEGER :: m, n_Profiles
  INTEGER :: j, n_Molecule_Sets
  INTEGER :: n, n_Sensors

  REAL( fp_kind ), DIMENSION( MAX_N_LEVELS )        :: Level_Pressure
  INTEGER,         DIMENSION( MAX_N_ANGLES )        :: Angle_Numbers
  REAL( fp_kind ), DIMENSION( MAX_N_ANGLES )        :: Angle_Secant  
  INTEGER,         DIMENSION( MAX_N_PROFILES )      :: Profile_Numbers
  INTEGER,         DIMENSION( MAX_N_MOLECULE_SETS ) :: Molecule_Set_Numbers

  INTEGER :: Direction

  LOGICAL :: All_Files_Present

  TYPE( SensorInfo_type )      :: SensorInfo
  TYPE( SensorInfo_List_type ) :: SensorInfo_List

  TYPE( TauProfile_type ) :: TauProfile



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to assemble individual TauProfile datafiles  ")' )
  WRITE( *, '( 5x, "   into a single datafile.                            ")' )
  WRITE( *, '(/5x, " $Revision: 1.4 $")' )
  WRITE( *, '( 5x, a, / )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                       -- READ THE SensorInfo FILE --                       #
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
  !#         -- ENTER DATA FOR TRANSMITTANCE DATA FILE IDENTIFICATION --        #
  !#----------------------------------------------------------------------------#

  ! -------------------------
  ! The angle index number(s)
  ! -------------------------

  ! -- How many angles?
  WRITE( *, FMT     = '( /5x, "Enter the number of ANGLES to combine: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT    = '( i2 )', &
           IOSTAT = IO_Status ) n_Angles

  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid input.', &
                          FAILURE )
    STOP
  END IF

  IF ( n_Angles < 1 .OR. n_Angles > MAX_N_ANGLES ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Value outside of valid range.', &
                          FAILURE )
    STOP
  ENDIF


  ! -- Now enter the actual molecule set numbers
  Angle_Numbers = -1
  WRITE( *, FMT     = '( 5x, "Enter the ANGLE INDEX numbers to process: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT    = *, &
           IOSTAT = IO_Status ) Angle_Numbers( 1:n_Angles )

  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid ANGLE INDEX input.', &
                          FAILURE )
    STOP
  END IF


  ! ---------------------------
  ! The profile index number(s)
  ! ---------------------------

  ! -- How many profiles?
  WRITE( *, FMT     = '( /5x, "Enter the number of PROFILES to combine: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT    = '( i2 )', &
           IOSTAT = IO_Status ) n_Profiles

  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid input.', &
                          FAILURE )
    STOP
  END IF

  IF ( n_Profiles < 1 .OR. n_Profiles > MAX_N_PROFILES ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Value outside of valid range.', &
                          FAILURE )
    STOP
  ENDIF

  ! -- Now enter the actual profile numbers
  Profile_Numbers = -1
  WRITE( *, FMT     = '( 5x, "Enter the PROFILE INDEX numbers to process: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT    = *, &
           IOSTAT = IO_Status ) Profile_Numbers( 1:n_Profiles )

  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid PROFILE INDEX input.', &
                          FAILURE )
    STOP
  END IF


  ! --------------------------------
  ! The molecule set index number(s)
  ! --------------------------------

  ! -- How many molecule sets?
  WRITE( *, FMT     = '( /5x, "Enter the number of MOLECULE SETS to assemble: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT    = '( i2 )', &
           IOSTAT = IO_Status ) n_Molecule_Sets

  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid input.', &
                          FAILURE )
    STOP
  END IF

  IF ( n_Molecule_Sets < 1 .OR. n_Molecule_Sets > MAX_N_MOLECULE_SETS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Value outside of valid range.', &
                          FAILURE )
    STOP
  ENDIF


  ! -- Now enter the actual molecule set numbers
  Molecule_Set_Numbers = -1
  WRITE( *, FMT     = '( 5x, "Enter the MOLECULE SET numbers to process: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT    = *, &
           IOSTAT = IO_Status ) Molecule_Set_Numbers( 1:n_Molecule_Sets )

  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid MOLECULE SET input.', &
                          FAILURE )
    STOP
  END IF


  ! -------------
  ! The direction
  ! -------------

  WRITE( *, FMT     = '( /5x, "Select atmospheric path", /10x, "1) ", a, /10x, "2) ", a, /5x, "Enter choice: " )', &
            ADVANCE = 'NO' ) DIRECTION_NAME( 1 ), DIRECTION_NAME( 2 )
  READ( *, FMT    = '( i5 )', &
           IOSTAT = IO_Status ) Direction

  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid ATMOSPHERIC PATH identifier input.', &
                          FAILURE )
    STOP
  END IF

  IF ( Direction /= 1 .AND. Direction /= 2 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid ATMOSPERIC PATH identifier value.', &
                          FAILURE )
    STOP
  ENDIF



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
    !#              -- CHECK THAT *ALL* THE REQUIRED FILES EXIST --             #
    !#--------------------------------------------------------------------------#

    All_Files_Present = .TRUE.

    File_Search: DO j = 1, n_Molecule_Sets
      DO m = 1, n_Profiles
        DO i = 1, n_Angles


          ! -----------------------------
          ! Construct TauProfile filename
          ! -----------------------------

          TauProfile_Filename = ' '
          WRITE( TauProfile_Filename, '( "./moleculeset", i3.3, &
                                        &"/profile", i2.2, &
                                        &"/angle", i1, &
                                        &"/", a, ".", a, ".TauProfile.nc" )' ) &
                                      Molecule_Set_Numbers( j ), &
                                      Profile_Numbers( m ), &
                                      Angle_Numbers( i ), &
                                      TRIM( DIRECTION_NAME( Direction ) ), &
                                      TRIM( SensorInfo%File_Prefix )


          ! -------------------------------
          ! Check that the file even exists
          ! -------------------------------

          IF ( .NOT. File_Exists( TRIM( TauProfile_Filename ) ) ) THEN
            All_Files_Present = .FALSE.
            EXIT File_Search
          END IF


          ! ------------------------------------
          ! Inquire the first file only for the
          ! level pressure and global attributes
          ! ------------------------------------

          Pressure_Inquire: IF ( i == 1 .AND. m == 1 .AND. j == 1 ) THEN

            ! -- Get the number of layers and the global atts
            Error_Status = Inquire_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                                      n_Layers = n_Layers, &
                                                      ID_Tag  = ID_Tag, &
                                                      History = History, &
                                                      Comment = Comment ) 
            IF ( Error_Status /= SUCCESS ) THEN
              CALL Display_Message( PROGRAM_NAME, &
                                    'Error retrieving n_Layers dimension from inquire of '//&
                                    TRIM( TauProfile_Filename ), &
                                    Error_Status )
              STOP
            END IF

            ! -- Do a double check
            n_Levels = n_Layers + 1
            IF ( n_Levels > MAX_N_LEVELS ) THEN
              Error_Status = FAILURE
              CALL Display_Message( PROGRAM_NAME, &
                                    'n_Layers dimension of '//TRIM( TauProfile_Filename )//&
                                    ' is larger than defined maximum.', &
                                    Error_Status )
              STOP
            END IF

            ! -- Get the level pressure data
            Error_Status = Inquire_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                                      Level_Pressure = Level_Pressure(1:n_Levels) ) 
            IF ( Error_Status /= SUCCESS ) THEN
              CALL Display_Message( PROGRAM_NAME, &
                                    'Error retrieving Level_Pressure data from inquire of '//&
                                    TRIM( TauProfile_Filename ), &
                                    Error_Status )
              STOP
            END IF

          END IF Pressure_Inquire


          ! ------------------------------------------------------
          ! Inquire the first n_Angles files for the angle secants
          ! ------------------------------------------------------

          Angle_Inquire: IF ( m == 1 .AND. j == 1 ) THEN

            Error_Status = Inquire_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                                      Angle_List = Angle_Secant(i:i) ) 
            IF ( Error_Status /= SUCCESS ) THEN
              CALL Display_Message( PROGRAM_NAME, &
                                    'Error retrieving Angle_List data from inquire of '//&
                                    TRIM( TauProfile_Filename ), &
                                    Error_Status )
              STOP
            END IF

          END IF Angle_Inquire

        END DO
      END DO
    END DO File_Search



    !#--------------------------------------------------------------------------#
    !#                       -- MAIN PROCESSING BRANCH --                       #
    !#--------------------------------------------------------------------------#

    Available_Data: IF ( All_Files_Present ) THEN


      ! ----------------------
      ! Create the output file
      ! ----------------------

      Output_TauProfile_Filename = TRIM( DIRECTION_NAME( Direction ) )//'.'//&
                                   TRIM( SensorInfo%File_Prefix )//'.TauProfile.nc'

      Error_Status = Create_TauProfile_netCDF( TRIM( Output_TauProfile_Filename ), &

                                               Level_Pressure(1:n_Levels), &
                                               SensorInfo%Sensor_Channel, &
                                               Angle_Secant(1:n_Angles), &
                                               Profile_Numbers(1:n_Profiles), &
                                               Molecule_Set_Numbers(1:n_Molecule_Sets), &

                                               NCEP_Sensor_ID   = SensorInfo%NCEP_Sensor_ID, &
                                               WMO_Satellite_ID = SensorInfo%WMO_Satellite_ID, &
                                               WMO_Sensor_ID    = SensorInfo%WMO_Sensor_ID, &

                                               ID_Tag = TRIM( ID_Tag ), &
                                               Title = TRIM( SensorInfo%Sensor_Name )//' '//&
                                                       TRIM( DIRECTION_NAME( Direction ) )//&
                                                       ' transmittances for '//&
                                                       TRIM( SensorInfo%Satellite_Name ), &
                                               History = PROGRAM_RCS_ID//'; '//TRIM( History ), &
                                               Sensor_Name   = TRIM( SensorInfo%Sensor_Name ), &
                                               Platform_Name = TRIM( SensorInfo%Satellite_Name ), &
                                               Comment       = 'File created by concatenating individual '//&
                                                               'angle, profile, and molecule set files. '//&
                                                               TRIM( Comment ) )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error creating netCDF TauProfile file '//&
                              TRIM( Output_TauProfile_Filename ), &
                              Error_Status )
        STOP
      END IF



      !#------------------------------------------------------------------------#
      !#                         -- BEGIN LOOP NEST --                          #
      !#------------------------------------------------------------------------#

      DO j = 1, n_Molecule_Sets
        DO m = 1, n_Profiles
          DO i = 1, n_Angles

            
            ! -------------------------------------
            ! Construct current TauProfile filename
            ! -------------------------------------

            TauProfile_Filename = ' '
            WRITE( TauProfile_Filename, '( "./moleculeset", i3.3, &
                                          &"/profile", i2.2, &
                                          &"/angle", i1, &
                                          &"/", a, ".", a, ".TauProfile.nc" )' ) &
                                        Molecule_Set_Numbers( j ), &
                                        Profile_Numbers( m ), &
                                        Angle_Numbers( i ), &
                                        TRIM( DIRECTION_NAME( Direction ) ), &
                                        TRIM( SensorInfo%File_Prefix )


            ! --------------------------------
            ! Read the current TauProfile data
            ! --------------------------------

            Error_Status = Read_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                                   TauProfile )

            IF ( Error_Status /= SUCCESS ) THEN
              CALL Display_Message( PROGRAM_NAME, &
                                    'Error reading TauProfile file '//&
                                    TRIM( TauProfile_Filename ), &
                                    Error_Status )
              STOP
            END IF


            ! -----------------------------------
            ! Write the data into the output file
            ! -----------------------------------

            Error_Status = Write_TauProfile_netCDF( TRIM( Output_TauProfile_Filename ), &
                                                    TauProfile%Tau(:,:,1,1,1), &
                                                    Angle_Secant(i), &
                                                    Profile_Numbers(m), &
                                                    Molecule_Set_Numbers(j) )

            IF ( Error_Status /= SUCCESS ) THEN
              CALL Display_Message( PROGRAM_NAME, &
                                    'Error writing data from '//&
                                    TRIM( TauProfile_Filename )//&
                                    ' into '//TRIM( Output_TauProfile_Filename ), &
                                    Error_Status )
              STOP
            END IF


            ! ----------------------------------------------
            ! Destroy the TauPRofile structure for next read
            ! ----------------------------------------------

            Error_Status = Destroy_TauProfile( TauProfile )

            IF ( Error_Status /= SUCCESS ) THEN
              CALL Display_Message( PROGRAM_NAME, &
                                    'Error destroying TauProfile structure after reading '//&
                                    TRIM( TauProfile_Filename ), &
                                    Error_Status )
              STOP
            END IF

          END DO
        END DO
      END DO

    END IF Available_Data


    !#--------------------------------------------------------------------------#
    !#              -- DESTROY THE CURRENT SensorInfo STRUCTURE --              #
    !#--------------------------------------------------------------------------#

    Error_Status = Destroy_SensorInfo( SensorInfo )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error destroying SensorInfo structure for sensor # ", i5 )' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF

  END DO Sensor_Loop



  !#----------------------------------------------------------------------------#
  !#                 -- DESTROY THE SensorInfo LINKED LIST --                   #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_SensorInfo_List( SensorInfo_List )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying SensorInfo_List', &
                          WARNING )
  END IF

END PROGRAM Combine_TauProfile


