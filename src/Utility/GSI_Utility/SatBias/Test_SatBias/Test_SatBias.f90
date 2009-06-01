!
! Program to test the SatBias module routines
!
! Written by:     Paul van Delst, CIMSS/SSEC 29-Dec-2005
!                 paul.vandelst@ssec.wisc.edu
!
PROGRAM Test_SatBias
 

  ! ------------------
  ! Environment set-up
  ! ------------------
  ! Module use
  USE Message_Handler
  USE SatInfo_IO
  USE SatBias_IO
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER( * ), PARAMETER :: PROGRAM_NAME   = 'Test_SatBias'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id:$'

  ! -- Test input filename and number of entries
  CHARACTER( * ), PARAMETER :: SATINFO_FILENAME        = 'global_satinfo.gsi'
  CHARACTER( * ), PARAMETER :: ANGLE_INPUT_FILENAME    = 'satbias.angle'
  CHARACTER( * ), PARAMETER :: ANGLE_OUTPUT_FILENAME   = 'satbias.angle.Test_Write'
  CHARACTER( * ), PARAMETER :: AIRMASS_INPUT_FILENAME  = 'satbias.airmass'
  CHARACTER( * ), PARAMETER :: AIRMASS_OUTPUT_FILENAME = 'satbias.airmass.Test_Write'
  INTEGER,        PARAMETER :: N_ENTRIES = 592


  ! ---------
  ! Variables
  ! ---------
  CHARACTER( 256 ) :: Message
  INTEGER :: Error_Status
  INTEGER :: nEntries
  TYPE( SatInfo_type ),        DIMENSION( N_ENTRIES ) :: SatInfo
  TYPE( SatBiasAngle_type ),   DIMENSION( N_ENTRIES ) :: SatBiasAngle
  TYPE( SatBiasAirMass_type ), DIMENSION( N_ENTRIES ) :: SatBiasAirMass


  ! ---------------------
  ! Output program header
  ! ---------------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test the routines in the SatBias_IO module', &
                        '$Revision:$' )


  ! ---------------------
  ! Read the satinfo file
  ! ---------------------
  Error_Status = Read_SatInfo( SATINFO_FILENAME, SatInfo )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading SatInfo file '//SATINFO_FILENAME, &
                          Error_status )
    STOP
  END IF


  ! ---------------------------------------------
  ! Test the read functions sans SatInfo ordering
  ! ---------------------------------------------
  ! The SatBiasAngle
  WRITE( *, '( 5x, "Reading SatBiasAngle file ", a, ". No SatInfo ordering..." )' ) ANGLE_INPUT_FILENAME
  Error_Status = Read_SatBias( ANGLE_INPUT_FILENAME, SatBiasAngle, &
                               nEntries=nEntries )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading SatBiasAngle file '//ANGLE_INPUT_FILENAME, &
                          Error_status )
    STOP
  END IF
  WRITE(*,'(10x, "Number of valid entries read: ", i5 )' ) nEntries

  ! The SatBiasAirMass
  WRITE( *, '( 5x, "Reading SatBiasAirMass file ", a, ". No SatInfo ordering..." )' ) AIRMASS_INPUT_FILENAME
  Error_Status = Read_SatBias( AIRMASS_INPUT_FILENAME, SatBiasAirMass, &
                               nEntries=nEntries )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading SatBiasAirMass file '//AIRMASS_INPUT_FILENAME, &
                          Error_status )
    STOP
  END IF
  WRITE(*,'(10x, "Number of valid entries read: ", i5 )' ) nEntries


  ! ---------------------------------------------
  ! Test the read functions with SatInfo ordering
  ! ---------------------------------------------
  ! The SatBiasAngle
  WRITE( *, '( /5x, "Reading SatBiasAngle file ", a, " with SatInfo ordering..." )' ) ANGLE_INPUT_FILENAME
  Error_Status = Read_SatBias( ANGLE_INPUT_FILENAME, SatBiasAngle, &
                               SatInfo=SatInfo, &
                               nEntries=nEntries )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading SatBiasAngle file '//ANGLE_INPUT_FILENAME, &
                          Error_status )
    STOP
  END IF
  WRITE(*,'(10x, "Number of valid entries read: ", i5 )' ) nEntries

  ! The SatBiasAirMass
  WRITE( *, '( 5x, "Reading SatBiasAirMass file ", a, " with SatInfo ordering..." )' ) AIRMASS_INPUT_FILENAME
  Error_Status = Read_SatBias( AIRMASS_INPUT_FILENAME, SatBiasAirMass, &
                               SatInfo=SatInfo, &
                               nEntries=nEntries )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading SatBiasAirMass file '//AIRMASS_INPUT_FILENAME, &
                          Error_status )
    STOP
  END IF
  WRITE(*,'(10x, "Number of valid entries read: ", i5 )' ) nEntries


  ! ------------------------
  ! Test the write functions
  ! ------------------------
  ! The SatBiasAngle
  WRITE( *, '( /5x, "Writing SatBiasAngle file ", a, "..." )' ) ANGLE_OUTPUT_FILENAME
  Error_Status = Write_SatBias( ANGLE_OUTPUT_FILENAME, SatBiasAngle )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing SatBiasAngle file '//ANGLE_OUTPUT_FILENAME, &
                          Error_status )
    STOP
  END IF

  ! The SatBiasAirMass
  WRITE( *, '( 5x, "Writing SatBiasAirMass file ", a, "..." )' ) AIRMASS_OUTPUT_FILENAME
  Error_Status = Write_SatBias( AIRMASS_OUTPUT_FILENAME, SatBiasAirMass )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing SatBiasAirMass file '//AIRMASS_OUTPUT_FILENAME, &
                          Error_status )
    STOP
  END IF

END PROGRAM Test_SatBias
