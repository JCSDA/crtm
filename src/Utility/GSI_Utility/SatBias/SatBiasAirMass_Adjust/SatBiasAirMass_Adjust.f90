!
! SatBiasAirMass_Adjust
!
! Program to apply the mean scan angle bias to the constant term
! of the air mass biass coefficients.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 04-Dec-2006
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM SatBiasAirMass_Adjust
 
  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds, ONLY: fp=>fp_kind
  USE Message_Handler
  USE String_Utility
  USE Sensor_Utility
  USE SatBias_IO
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'SatBiasAirMass_Adjust'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id$'
  REAL(fp),     PARAMETER :: C0_SCALEFACTOR = 100.0_fp

  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: AngleFile, AirMassFile
  INTEGER :: Allocate_Status
  INTEGER :: Error_Status
  INTEGER :: nChannels
  TYPE(SatBiasAngle_type),   ALLOCATABLE, DIMENSION(:) :: SatBiasAngle
  TYPE(SatBiasAirMass_type), ALLOCATABLE, DIMENSION(:) :: SatBiasAirMass

  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to insert the apply the mean scan angle bias to '//&
                        'the constant term of the air mass biass coefficients.', &
                        '$Revision$' )

  ! Get the input filenames
  WRITE( *, '( /5x, "Enter the SatBiasAngle file:   " )', ADVANCE = 'NO' )
  READ(*,'(a)') AngleFile
  AngleFile = ADJUSTL(AngleFile)

  WRITE( *, '( /5x, "Enter the SatBiasAirMass file: " )', ADVANCE = 'NO' )
  READ(*,'(a)') AirMassFile
  AirMassFile = ADJUSTL(AirMassFile)

  ! Get and check the number of input channels
  nChannels = n_SatBiasAngle_Entries(AngleFile)
  IF ( nChannels == 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error counting the number of channel entries in '//TRIM(AngleFile), &
                          FAILURE )
    STOP
  END IF
  IF ( n_SatBiasAirMass_Entries(AirMassFile) /= nChannels ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'The number of Angle entries is different from '//&
                          'the number of AirMass entries.', &
                          FAILURE )
    STOP
  END IF
  WRITE(*,'(10x,i0," channel entries detected.")') nChannels

  ! Allocate the structure arrays
  ALLOCATE( SatBiasAngle( nChannels ), &
            SatBiasAirMass( nChannels ), &
            STAT = Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error allocating structure arrays. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          FAILURE )
    STOP
  END IF

  ! Read the SatBiasAngle structure array
  WRITE( *, '( /5x, "Reading SatBiasAngle file ", a, "..." )' ) TRIM(AngleFile)
  Error_Status = Read_SatBias( AngleFile, SatBiasAngle )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading SatBiasAngle file '//TRIM(AngleFile), &
                          Error_Status )
    STOP
  END IF

  ! Read the SatBiasAirMass structure array
  WRITE( *, '( /5x, "Reading SatBiasAirMass file ", a, "..." )' ) TRIM(AirMassFile)
  Error_Status = Read_SatBias( AirMassFile, SatBiasAirMass )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading SatBiasAirMass file '//TRIM(AirMAssFile), &
                          Error_Status )
    STOP
  END IF

  ! Add the angle means to the air mass constant term
  SatBiasAirMass%c(1) = SatBiasAirMass%c(1) + (C0_SCALEFACTOR*SatBiasAngle%Bias_Mean)

  ! Write the updated SatBiasAirMass structure array
  AirMassFile = TRIM(AirMassFile)//'.updated'
  WRITE( *, '( 5x, "Writing SatBiasAirMass file ", a, "..." )' ) TRIM(AirMassFile)
  Error_Status = Write_SatBias( AirMassFile, SatBiasAirMass )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing SatBiasAirMass file '//TRIM(AirMassFile), &
                          Error_Status )
    STOP
  END IF

END PROGRAM SatBiasAirMass_Adjust
