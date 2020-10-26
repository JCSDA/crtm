
PROGRAM Check_TauProfile_Data


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler

  USE ProcessControl_Define
  USE ProcessControl_IO

  USE TauProfile_Define
  USE TauProfile_netCDF_IO

  USE Tau_Production_Parameters


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Check_TauProfile_Data'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Check_TauProfile_Data.f90,v 1.6 2006/06/30 16:47:16 dgroff Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  REAL( fp_kind ), PARAMETER :: ZERO = 0.0_fp_kind
  REAL( fp_kind ), PARAMETER :: ONE  = 1.0_fp_kind

  REAL( fp_kind ), PARAMETER :: TOLERANCE     = EPSILON( ONE )
  REAL( fp_kind ), PARAMETER :: TAU_TOLERANCE = 100.0_fp_kind * TOLERANCE

  CHARACTER( * ),  PARAMETER, DIMENSION( 2 ) :: DIRECTION_NAME = (/ 'upwelling  ', &
                                                                    'downwelling' /)


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  INTEGER :: IO_Status
  INTEGER :: Error_Status

  INTEGER :: Profile_Set
  INTEGER :: Molecule_Set
  INTEGER :: Direction
  INTEGER :: Channel

  CHARACTER( 256 ) :: SensorInfo_Filename
  CHARACTER( 256 ) :: Control_Filename
  CHARACTER( 256 ) :: TauProfile_Filename
  CHARACTER( 256 ) :: Signal_Filename

  INTEGER :: n_Profile_Errors
  INTEGER :: n_Profile_Set_Errors
  INTEGER :: Line_Count
  INTEGER :: Signal_ID

  INTEGER :: i, j, k, l, m, n
  INTEGER :: n_m, n_j

  TYPE( ProcessControl_type ) :: ProcessControl
  TYPE( TauProfile_type )     :: TauProfile



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to check the individual TauProfile datafiles  ")' )
  WRITE( *, '( 5x, "   for complete data.                                ")' )
  WRITE( *, '(/5x, " $Revision: 1.6 $")' )
  WRITE( *, '( 5x, a, / )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#         -- ENTER DATA FOR TRANSMITTANCE DATA FILE IDENTIFICATION --        #
  !#----------------------------------------------------------------------------#

  ! -------------------------------
  ! The profile set being processed
  ! -------------------------------

  WRITE( *, FMT = '( /5x, "Select the DEPENDENT PROFILE SET" )' )
  DO n_m = 1, N_PROFILE_SETS
    WRITE( *, FMT = '( 10x, i2, ") ", a, " profile set" )' ) &
              n_m, TRIM( PROFILE_SET_ID_TAG( n_m ) )
  END DO
  WRITE( *, FMT = '( 5x, "Enter choice: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT    = '( i1 )', &
           IOSTAT = IO_Status ) Profile_Set

  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid DEPENDENT PROFILE SET identifier input.', &
                          FAILURE )
    STOP
  END IF

  IF ( Profile_Set < 1 .OR. Profile_Set > N_PROFILE_SETS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid DEPENDENT PROFILE SET identifier value.', &
                          FAILURE )
    STOP
  ENDIF


  ! ----------------
  ! The molecule set
  ! ----------------

  WRITE( *, FMT = '( /5x, "Select the MOLECULE SET" )' )
  DO n_j = 1, N_MOLECULE_SETS
    WRITE( *, FMT = '( 10x, i2, ") ", a )' ) &
              n_j, TRIM( MOLECULE_SET_TAG( n_j ) )
  END DO
  WRITE( *, FMT = '( 5x, "Enter choice: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT    = '( i2 )', &
           IOSTAT = IO_Status ) Molecule_Set

  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid MOLECULE SET identifier input.', &
                          FAILURE )
    STOP
  END IF

  IF ( Molecule_Set < 1 .OR. Molecule_Set > N_MOLECULE_SETS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid DEPENDENT PROFILE SET identifier value.', &
                          FAILURE )
    STOP
  ENDIF


  ! ---------------
  ! The "direction"
  ! ---------------

  WRITE( *, FMT     = '( /5x, "Select atmospheric path", &
                       &/10x, "1) ", a, &
                       &/10x, "2) ", a, &
                       &/5x, "Enter choice: " )', &
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
  !#                     -- BEGIN MAIN LOOP OVER PROFILES --                    #
  !#----------------------------------------------------------------------------#

  Profile_Loop: DO m = 1, N_PROFILES( profile_set )


    WRITE( *, '( /5x, "Checking profile ", i3, "..." )' ) m


    ! ------------------------------------------------
    ! Initialise the number of errors for this profile
    ! ------------------------------------------------

    n_Profile_Errors = 0


    ! --------------------------------------------------
    ! Construct the process control and signal filenames
    ! --------------------------------------------------

    WRITE( Control_Filename, '( "./profile", i2.2, "/ProcessControl.", a, ".", a )' ) &
                                m, &
                                TRIM( MOLECULE_SET_TAG( Molecule_Set ) ), &
                                TRIM( DIRECTION_NAME( Direction ) )

    Signal_Filename = TRIM( Control_Filename )//'.completed.signal'


    ! -----------------------------------
    ! Cycle the profile loop if the
    ! Process Control file does NOT exist
    ! -----------------------------------

    IF ( .NOT. File_Exists( TRIM( Control_Filename ) ) ) CYCLE Profile_Loop



    ! -----------------------------
    ! Read the Process Control file
    ! -----------------------------

    Error_Status = Read_ProcessControl( TRIM( Control_Filename ), &
                                        ProcessControl )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error reading Process Control file '//&
                            TRIM( Control_Filename )//'.', &
                            FAILURE )
      STOP
    END IF

    WRITE( *, '( 5x, "Number of sensors to be checked: ", i5 )' ) ProcessControl%n_Files
    DO n = 1, ProcessControl%n_Files
      WRITE( *, FMT = '( 5x, a )', ADVANCE = 'NO' ) TRIM( ProcessControl%File_Prefix(n) )
      IF ( MOD( n, 5 ) == 0 ) WRITE(*,*)
    END DO
    WRITE( *, * )



    !#--------------------------------------------------------------------------#
    !#                       -- BEGIN LOOP OVER SENSORS --                      #
    !#--------------------------------------------------------------------------#

    Sensor_Loop: DO n = 1, ProcessControl%n_Files


      ! --------------------------------------------
      ! Construct the transmittance profile filename
      ! --------------------------------------------

      WRITE( TauProfile_Filename, '( "./profile", i2.2, "/", a, ".", &
                                    &"profile", i2.2, "_", a, ".", a, &
                                    &".TauProfile.nc" )' ) &
                                  m, &
                                  TRIM( DIRECTION_NAME( Direction ) ), &
                                  m, &
                                  TRIM( MOLECULE_SET_TAG( Molecule_Set ) ), &
                                  TRIM( ProcessControl%File_Prefix(n) )


      ! ------------------------------------------------
      ! If the TauProfile does not exist, then set the
      ! "Processed" flags to 0 and cycle the sensor loop
      ! ------------------------------------------------

      IF ( .NOT. File_Exists( TRIM( TauProfile_Filename ) ) ) THEN

        ! -- Output message
        WRITE( Message, '( "Required TauProfile file ", a, " not found!" )' ) &
                        TRIM( TauProfile_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              WARNING )

        ! -- Reinitialise ALL channels for this sensor
        ProcessControl%List%Processed = 0

        ! -- Increment error count to force ProcessControl rewrite
        n_Profile_Errors = n_Profile_Errors + 1

        CYCLE Sensor_Loop

      END IF


      ! --------------------------------
      ! Read the current input file data
      ! --------------------------------

      Error_Status = Read_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                             TauProfile, &
                                             Quiet = 1 )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error reading netCDF TauProfile file '//&
                              TRIM( TauProfile_Filename ), &
                              Error_Status )
        STOP
      END IF


      ! ----------------------------------------------------
      ! Check that it's the correct profile and molecule set
      ! ----------------------------------------------------

      IF ( TauProfile%Profile( 1 )      /= m .OR. &
           TauProfile%Molecule_Set( 1 ) /= MOLECULE_SET_TAG_ID( Molecule_Set ) ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Profile and molecule_set value for ", a, &
                          &" (",i3,",",i2,") are different than expected", &
                          &" (",i3,",",i2,")." )' ) &
                        TRIM( TauProfile_Filename ), &
                        TauProfile%Profile( 1 ), TauProfile%Molecule_Set( 1 ), &
                        m,                       MOLECULE_SET_TAG_ID( Molecule_Set )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF


      ! --------------------------------------
      ! Correct transmittances close to 0 or 1
      ! but non-physical due to numerics
      ! --------------------------------------

      ! -- Check for 1 < tau 1+e
      WHERE( TauProfile%Tau > ONE .AND. TauProfile%Tau < (ONE+TAU_TOLERANCE) )
        TauProfile%Tau = ONE
      END WHERE

      ! -- Check for 0-e < tau < 0
      WHERE( TauProfile%Tau > (ZERO-TAU_TOLERANCE) .AND. TauProfile%Tau < ZERO )
        TauProfile%Tau = ZERO
      END WHERE



      ! -----------------------------------------------
      ! Now check for grossly crappy transmittances > 1
      ! -----------------------------------------------

      IF ( ANY( TauProfile%Tau > ONE ) ) THEN
        WRITE( Message, '( "Number of transmittances > 1.0 found in ", a, &
                          &": ", i10 )' ) &
                        TRIM( TauProfile_Filename ), COUNT( TauProfile%Tau > ONE )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              WARNING )

        ! -- Update the error count
        n_Profile_Errors = n_Profile_Errors + COUNT( TauProfile%Tau > ONE )


        ! -- Now check transmittances for each channel/angle
        Line_Count = 0

        Angle_Loop_GT_1: DO i = 1, TauProfile%n_Angles

          Channel_Loop_GT_1: DO l = 1, TauProfile%n_Channels

            IF ( ANY( TauProfile%Tau(:,l,i,1,1) > ONE ) ) THEN

              ! -- Obtain the ProcessControl channel indices
              Channel = ProcessControl%Channel_Index(1,n) + l - 1

              ! -- Reset processed flag
              ProcessControl%List(Channel)%Processed = 0

              WRITE( *, '( i10, " transmittances > 1.0 for channel ", i4, &
                          &", angle ", f4.2, ", profile ", i3, &
                          &", and molecule ", a, ". LBL band limits: ", i2,":",i2 )' ) &
                        COUNT( TauProfile%Tau(:,l,i,1,1) > ONE ), &
                        TauProfile%Channel(l), &
                        TauProfile%Angle(i), &
                        m, &
                        TRIM( MOLECULE_SET_TAG( Molecule_Set ) ), &
                        ProcessControl%List(Channel)%Begin_LBLband, &
                        ProcessControl%List(Channel)%End_LBLband


            END IF
          
          END DO Channel_Loop_GT_1

        END DO Angle_Loop_GT_1

      END IF


      ! -----------------------------------------------
      ! Now check for grossly crappy transmittances < 0
      ! -----------------------------------------------

      IF ( ANY( TauProfile%Tau < ZERO ) ) THEN
        WRITE( Message, '( "Number of transmittances < 0.0 found in ", a, &
                          &": ", i10 )' ) &
                        TRIM( TauProfile_Filename ), COUNT( TauProfile%Tau < ZERO )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              WARNING )


        ! -- Update the error count
        n_Profile_Errors = n_Profile_Errors + COUNT( TauProfile%Tau < ZERO )


        ! -- Now check transmittances for each channel/angle
        Line_Count = 0

        Angle_Loop_LT_0: DO i = 1, TauProfile%n_Angles

          Channel_Loop_LT_0: DO l = 1, TauProfile%n_Channels

            IF ( ANY( TauProfile%Tau(:,l,i,1,1) < ZERO ) ) THEN

              ! -- Obtain the ProcessControl channel indices
              Channel = ProcessControl%Channel_Index(1,n) + l - 1

              ! -- Reset processed flag
              ProcessControl%List(Channel)%Processed = 0

              WRITE( *, '( i10, " transmittances < 0.0 for channel ", i4, &
                          &", angle ", f4.2, ", profile ", i3, &
                          &", and molecule ", a, ". LBL band limits: ", i2,":",i2 )' ) &
                        COUNT( TauProfile%Tau(:,l,i,1,1) < ZERO ), &
                        TauProfile%Channel(l), &
                        TauProfile%Angle(i), &
                        m, &
                        TRIM( MOLECULE_SET_TAG( Molecule_Set ) ), &
                        ProcessControl%List(Channel)%Begin_LBLband, &
                        ProcessControl%List(Channel)%End_LBLband


            END IF
          
          END DO Channel_Loop_LT_0

        END DO Angle_Loop_LT_0

      END IF


      ! --------------------------------
      ! Destroy the TauProfile structure
      ! --------------------------------

      Error_Status = Destroy_TauProfile( TauProfile )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error destroying TauProfile structure for profile #", i2, &
                          &", molecule set ", a, ", reading from ", a, "." )' ) &
                        m, TRIM( MOLECULE_SET_TAG( Molecule_Set ) ), TRIM( TauProfile_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF

    END DO Sensor_Loop


    ! --------------------------------------------------
    ! Output Process Control data if any errors detected
    ! --------------------------------------------------

    IF ( n_Profile_Errors > 0 ) THEN

      Error_Status = Write_ProcessControl( TRIM( Control_Filename ), &
                                           ProcessControl )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error writing ProcessControl file '//&
                              TRIM( Control_Filename )//'.', &
                              FAILURE )
        STOP
      END IF


      ! ---------------------------------
      ! Remove the completion signal file
      ! ---------------------------------

      Signal_ID = Get_Lun()

      OPEN( Signal_ID,  FILE   = TRIM( Signal_Filename ), &
                        STATUS = 'UNKNOWN', &
                        IOSTAT = IO_Status )
      CLOSE( Signal_ID, STATUS = 'DELETE', &
                        IOSTAT = IO_Status )

    ELSE

      WRITE( *, '( 10x, "All data o.k.!", / )' )

    END IF


    ! ----------------------------------------------------------
    ! Destroy the Process Control structure for the next profile
    ! ----------------------------------------------------------

    Error_Status = Destroy_ProcessControl( ProcessControl )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error destroying Process Control structure for profile # ", i3 )' ) m
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF

  END DO Profile_Loop

END PROGRAM Check_TauProfile_Data


