
PROGRAM Create_Process_Defaults_File


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler

  USE Tau_Production_Parameters
  USE Tau_Production_Utility


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Create_Process_Defaults_File'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Create_Process_Defaults_File.f90,v 1.9 2006/06/30 16:47:16 dgroff Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  CHARACTER( 256 ) :: Defaults_Filename
  INTEGER          :: Defaults_FileID

  INTEGER :: IO_Status_iw
  INTEGER :: IO_Status

  INTEGER :: n, l

  CHARACTER( 10 ) :: cBand
  CHARACTER( 10 ) :: cFreq
  CHARACTER( 10 ) :: cProfile
  CHARACTER( 10 ) :: cProfile_Set
  CHARACTER( 10 ) :: cAngle



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to create the defaults file for the transmittance")' )
  WRITE( *, '( 5x, "   production shell scripts. The created file links the   ")' )
  WRITE( *, '( 5x, "   shell script defaults to those specified in the        ")' )
  WRITE( *, '( 5x, "   Tau_Production_Parameters module.                      ")' )
  WRITE( *, '(/5x, " $Revision: 1.9 $")' )
  WRITE( *, '( 5x, a, / )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                         -- SELECT THE PROFILE SET --                       #
  !#----------------------------------------------------------------------------#

  Profile_Set_Select: DO

    ! -- Output user prompt
    WRITE( *, '( /5x, "Select the profile set to process:" )' )

    DO n = 1, N_PROFILE_SETS
      WRITE( *, '( 10x, i1, ") ", a, " dependent set" )' ) n, PROFILE_SET_ID_TAG( n )
    END DO
    WRITE( *, FMT = '( 5x, "Enter choice: " )', &
              ADVANCE = 'NO' )

    ! -- Read value
    READ( *, * ) n

    ! -- Check for validity
    IF ( n > 0 .AND. n <= N_PROFILE_SETS ) EXIT Profile_Set_Select

    WRITE( *, '( 10x, "** Invalid selection **" )' )

  END DO Profile_Set_Select



  !#----------------------------------------------------------------------------#
  !#                      -- SELECT THE FREQUENCY INTERVAL --                   #
  !#----------------------------------------------------------------------------#

  Frequency_Interval_Select: DO

    ! -- Output user prompt
    WRITE( *, '( /5x, "Select the frequency interval to use:" )' )

    DO l = 1, N_FREQUENCY_INTERVALS
      WRITE( *, '( 10x, i1, ") ", f6.4, " cm-1" )' ) l, FREQUENCY_INTERVAL( l )
    END DO
    WRITE( *, FMT = '( 5x, "Enter choice: " )', &
              ADVANCE = 'NO' )

    ! -- Read value
    READ( *, * ) l

    ! -- Check for validity
    IF ( l > 0 .AND. l <= N_FREQUENCY_INTERVALS ) EXIT Frequency_Interval_Select

    WRITE( *, '( 10x, "** Invalid selection **" )' )

  END DO Frequency_Interval_Select



  !#----------------------------------------------------------------------------#
  !#                         -- OPEN THE DEFAULTS FILE --                       #
  !#----------------------------------------------------------------------------#

  Defaults_Filename = 'Transmittance_Production.processing_defaults'


  ! ------------------------------
  ! Get a free logical unit number
  ! ------------------------------

  Defaults_FileID = Get_Lun()
  IF ( Defaults_FileID < 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error obtaining a file unit number for output.', &
                          FAILURE )
    STOP
  END IF


  ! -------------
  ! Open the file
  ! -------------

  OPEN( Defaults_FileID, FILE   = TRIM( Defaults_Filename ), &
                         ACCESS = 'SEQUENTIAL', &
                         FORM   = 'FORMATTED',  &
                         STATUS = 'REPLACE',    &
                         ACTION = 'WRITE',      &
                         IOSTAT = IO_Status     )

  IF ( IO_Status /= 0 ) THEN
    WRITE( Message, '( "Error opening output Transmittance Production ", &
                      &"defaults file. IOSTAT = ", i5 )' ) IO_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          FAILURE )
    STOP
  END IF





  !#----------------------------------------------------------------------------#
  !#                            -- OUTPUT A HEADER --                           #
  !#----------------------------------------------------------------------------#

  WRITE( Defaults_FileID, FMT    = '(  "# ", a, &
                                    &/,"# Transmittance production defaults file.", &
                                    &/,"#", &
                                    &/,"# Syntax is", &
                                    &/,"#   :NAME:VALUE:DESCRIPTION", &
                                    &/,"#", &
                                    &/,"# Each entry NAME must be unique", &
                                    &/,"#" )', &
                          IOSTAT = IO_Status ) PROGRAM_RCS_ID

  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing header to '//TRIM( Defaults_Filename ), &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                            -- OUTPUT THE DATA --                           #
  !#----------------------------------------------------------------------------#

  ! ----------------------------
  ! The default processing QUEUE
  ! ----------------------------

  WRITE( Defaults_FileID, FMT    = '( ":QUEUE:dev:Default batch processing queue" )', &
                          IOSTAT = IO_Status )

  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing QUEUE to '//TRIM( Defaults_Filename ), &
                          FAILURE )
    STOP
  END IF


  ! -------------------------------
  ! The beginning and end LBL bands
  ! -------------------------------

  ! -- The begin band for the selected frequency interval
  WRITE( cBand, FMT = '( i3 )', IOSTAT = IO_Status_iw ) LBLBAND_BEGIN(l)

  WRITE( Defaults_FileID, FMT    = '( ":BAND1:",a,":Beginning band to process" )', &
                          IOSTAT = IO_Status ) TRIM( ADJUSTL( cBand ) )

  IF ( IO_Status_iw /= 0 .OR. IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing BAND1 to '//TRIM( Defaults_Filename ), &
                          FAILURE )
    STOP
  END IF

  ! -- The end band for the selected frequency interval
  WRITE( cBand, FMT = '( i3 )', IOSTAT = IO_Status_iw ) LBLBAND_END(l)

  WRITE( Defaults_FileID, FMT    = '( ":BAND2:",a,":Ending band to process" )', &
                          IOSTAT = IO_Status ) TRIM( ADJUSTL( cBand ) )

  IF ( IO_Status_iw /= 0 .OR. IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing BAND2 to '//TRIM( Defaults_Filename ), &
                          FAILURE )
    STOP
  END IF


  ! ----------------------------------------
  ! The beginning frequency of LBLBAND_BEGIN
  ! ----------------------------------------

  WRITE( cFreq, FMT = '( i4 )', IOSTAT = IO_Status_iw ) INT( FREQUENCY_BEGIN )

  WRITE( Defaults_FileID, FMT    = '( ":F1_BAND1:",a,":The begin frequency of band #1" )', &
                          IOSTAT = IO_Status ) TRIM( ADJUSTL( cFreq ) )

  IF ( IO_Status_iw /= 0 .OR. IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing F1_BAND1 to '//TRIM( Defaults_Filename ), &
                          FAILURE )
    STOP
  END IF


  ! -----------------------
  ! The effective bandwidth
  ! -----------------------

  WRITE( cFreq, FMT = '( i4 )', IOSTAT = IO_Status_iw ) INT( FREQUENCY_DELTA(l) )

  WRITE( Defaults_FileID, FMT    = '( ":DF_BAND:",a,":The effective bandwidth of each band" )', &
                          IOSTAT = IO_Status ) TRIM( ADJUSTL( cFreq ) )

  IF ( IO_Status_iw /= 0 .OR. IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing DF_BAND to '//TRIM( Defaults_Filename ), &
                          FAILURE )
    STOP
  END IF


  ! ------------------------------------
  ! The averaging kernel width and index
  ! ------------------------------------

  ! -- The width
  WRITE( Defaults_FileID, FMT    = '( ":DF_AVG:",f6.4,":The averaging kernel frequency width. Must have 4dp" )', &
                          IOSTAT = IO_Status ) FREQUENCY_INTERVAL(l)

  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing DF_AVG to '//TRIM( Defaults_Filename ), &
                          FAILURE )
    STOP
  END IF

  ! -- The index
  WRITE( Defaults_FileID, FMT    = '( ":DF_INDEX:",i1,":The averaging kernel frequency width index" )', &
                          IOSTAT = IO_Status ) l

  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing DF_INDEX to '//TRIM( Defaults_Filename ), &
                          FAILURE )
    STOP
  END IF



  ! -------------------------------------------------------------------
  ! The TAPE3 molecule sets for production
  !
  ! 1-7  == molecule numbers
  !  8   == all first seven molecules (no continua)
  !  9   == continua only
  ! 10   == all first seven molecules (and their continua)
  ! 11   == water vapor + ozone only (and their continua)
  ! 12   == water vapor only (and it's continua)
  ! 13   == "dry" gases. Everything but h2o and o3 (and their continua)
  ! 14   == ozone only (and it's continua)
  ! 15   == water vapour continua only
  ! -------------------------------------------------------------------

  WRITE( Defaults_FileID, FMT    = '( ":TAPE3_LIST:1 10 11 12 13 14 15:Molecule set numbers" )', &
                          IOSTAT = IO_Status )

  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing TAPE3_LIST to '//TRIM( Defaults_Filename ), &
                          FAILURE )
    STOP
  END IF


  ! --------------------------
  ! The LBLRTM TAPE3 ID to use
  ! --------------------------

  WRITE( Defaults_FileID, FMT    = '( ":TAPE3_ID:2000_AER:The spectroscopic database to use" )', &
                          IOSTAT = IO_Status )

  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing TAPE3_ID to '//TRIM( Defaults_Filename ), &
                          FAILURE )
    STOP
  END IF


  ! ----------------------------------------------
  ! The relative location of the TAPE5 input files
  ! ----------------------------------------------

  WRITE( Defaults_FileID, FMT    = '( ":TAPE5_DIR:./TAPE5_files:The directory where the TAPE5 file are located." )', &
                          IOSTAT = IO_Status )

  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing TAPE5_DIR to '//TRIM( Defaults_Filename ), &
                          FAILURE )
    STOP
  END IF


  ! -------------------------------------
  ! The beginning and end profile numbers
  ! -------------------------------------

  ! -- The begin profile
  WRITE( cProfile, FMT = '( i3 )', IOSTAT = IO_Status_iw ) PROFILE_BEGIN( n )

  WRITE( Defaults_FileID, FMT    = '( ":PROFILE1:",a,":Beginning profile to process" )', &
                          IOSTAT = IO_Status ) TRIM( ADJUSTL( cProfile ) )

  IF ( IO_Status_iw /= 0 .OR. IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing PROFILE1 to '//TRIM( Defaults_Filename ), &
                          FAILURE )
    STOP
  END IF

  ! -- The end profile
  WRITE( cProfile, FMT = '( i3 )', IOSTAT = IO_Status_iw ) PROFILE_END( n )

  WRITE( Defaults_FileID, FMT    = '( ":PROFILE2:",a,":Ending profile to process" )', &
                          IOSTAT = IO_Status ) TRIM( ADJUSTL( cProfile ) )

  IF ( IO_Status_iw /= 0 .OR. IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing PROFILE2 to '//TRIM( Defaults_Filename ), &
                          FAILURE )
    STOP
  END IF


  ! -----------------------------
  ! The profile set ID and number
  ! -----------------------------

  ! -- The ID tag
  WRITE( Defaults_FileID, FMT    = '( ":PROFILE_SET_ID:", a, &
                                     &":Profile set identifier" )', &
                          IOSTAT = IO_Status ) TRIM( PROFILE_SET_ID_TAG( n ) )

  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing PROFILE_SET_ID to '//TRIM( Defaults_Filename ), &
                          FAILURE )
    STOP
  END IF

  ! -- The ID number
  WRITE( cProfile_Set, FMT = '( i3 )', IOSTAT = IO_Status_iw ) n

  WRITE( Defaults_FileID, FMT    = '( ":PROFILE_SET_NUMBER:", a, &
                                     &":Profile set identifier number" )', &
                          IOSTAT = IO_Status ) TRIM( ADJUSTL( cProfile_Set ) )

  IF ( IO_Status_iw /= 0 .OR. IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing PROFILE_SET_NUMBER to '//TRIM( Defaults_Filename ), &
                          FAILURE )
    STOP
  END IF


  ! -----------------------------------
  ! The beginning and end angle numbers
  ! -----------------------------------

  ! -- The begin angle
  WRITE( cAngle, FMT = '( i3 )', IOSTAT = IO_Status_iw ) ZENITH_ANGLE_BEGIN

  WRITE( Defaults_FileID, FMT    = '( ":ANGLE1:",a,":Beginning angle to process" )', &
                          IOSTAT = IO_Status ) TRIM( ADJUSTL( cAngle ) )

  IF ( IO_Status_iw /= 0 .OR. IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing ANGLE1 to '//TRIM( Defaults_Filename ), &
                          FAILURE )
    STOP
  END IF

  ! -- The end angle
  WRITE( cAngle, FMT = '( i3 )', IOSTAT = IO_Status_iw ) ZENITH_ANGLE_END

  WRITE( Defaults_FileID, FMT    = '( ":ANGLE2:",a,":Ending angle to process" )', &
                          IOSTAT = IO_Status ) TRIM( ADJUSTL( cAngle ) )

  IF ( IO_Status_iw /= 0 .OR. IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing ANGLE2 to '//TRIM( Defaults_Filename ), &
                          FAILURE )
    STOP
  END IF


  ! -----------------------------------
  ! The default CO2 mixing ratio to use
  ! -----------------------------------

  WRITE( Defaults_FileID, FMT    = '( ":CO2MR:",f7.3,":Default CO2 mixing ratio in ppmv. Must be in XXX.XXX format" )', &
                          IOSTAT = IO_Status ) CO2_MIXING_RATIO

  IF ( IO_Status_iw /= 0 .OR. IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing CO2MR to '//TRIM( Defaults_Filename ), &
                          FAILURE )
    STOP
  END IF

  CLOSE( Defaults_FileID )

END PROGRAM Create_Process_Defaults_File


