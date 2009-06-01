!
! SatBiasAngle_Mean
!
! Program to insert the mean scan angle bias and number of FOVs to the 
! header record for each channel entry in a SatBiasAngle file.
!
! Also, if specifically requested, the computed mean is removed from
! the scan angle bias values.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 11-Jan-2006
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM SatBiasAngle_Mean
 
  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds
  USE Message_Handler
  USE String_Utility
  USE Sensor_Utility
  USE SatBias_IO
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'SatBiasAngle_Mean'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id$'


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: InFile, OutFile
  CHARACTER(256) :: Answer
  LOGICAL :: Subtract_Mean=.FALSE.
  INTEGER :: IO_Status
  INTEGER :: Allocate_Status
  INTEGER :: Error_Status
  INTEGER :: nChannels
  TYPE(SatBiasAngle_type), ALLOCATABLE, DIMENSION(:) :: SatBiasAngle
  INTEGER :: i, n

  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to insert the scan angle bias mean and number of '//&
                        'FOVs to the header record for each channel entry in a '//&
                        'SatBiasAngle file. Also, if specifically requested, the '//&
                        'computed mean is removed from the scan angle bias values.', &
                        '$Revision$' )

  ! Get the input filename
  WRITE( *, '( /5x, "Enter the SatBiasAngle file to process: " )', ADVANCE = 'NO' )
  READ(*,'(a)') InFile
  InFile = ADJUSTL(InFile)
  OutFile = TRIM(InFile)//'.Mean'

  ! Get the number of input channels
  nChannels = n_SatBiasAngle_Entries(InFile)
  IF ( nChannels == 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error counting the number of channel entries in '//TRIM(InFile), &
                          FAILURE )
    STOP
  END IF
  WRITE(*,'(10x,i0," channel entries detected in ",a)') nChannels, TRIM(InFile)

  ! Ask if mean should be subtracted from angle bias
  WRITE( *, '( /5x, "Subtract mean from angle bias? [y/n] : " )', ADVANCE = 'NO' )
  READ(*,'(a)') Answer
  Answer = ADJUSTL(Answer)
  IF ( StrUpCase(TRIM(Answer)) == 'Y' .OR. &
       StrUpCase(TRIM(Answer)) == 'YES'    ) Subtract_Mean=.TRUE.

  ! Allocate the SatBiasAngle structure array
  ALLOCATE( SatBiasAngle( nChannels ), STAT = Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error allocating SatBiasAngle structure array. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          FAILURE )
    STOP
  END IF


  ! Read the SatBiasAngle structure array
  WRITE( *, '( /5x, "Reading SatBiasAngle file ", a, "..." )' ) TRIM(InFile)
  Error_Status = Read_SatBias( InFile, SatBiasAngle )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading SatBiasAngle file '//TRIM(InFile), &
                          Error_Status )
    STOP
  END IF


  ! Loop over the SatBiasAngle entries
  ! and compute the scan averages
  WRITE( *, '( 5x, "Computing SatBiasAngle mean..." )' )
  DO i = 1, nChannels
    ! Get the number of FOVs and save it
    CALL Get_SensorAttributes( SatBiasAngle(i)%Sensor_ID, nFOVs=n )
    SatBiasAngle(i)%nFOVs = n
    ! Compute the average 
    SatBiasAngle(i)%Bias_Mean = SUM( SatBiasAngle(i)%Bias(1:n) ) / REAL( n, fp_kind )
    ! Subtract the average if required
    IF ( Subtract_Mean ) THEN
      SatBiasAngle(i)%Bias(1:n) = SatBiasAngle(i)%Bias(1:n) - SatBiasAngle(i)%Bias_Mean
    END IF
  END DO

  ! Write the updated SatBiasAngle structure array
  WRITE( *, '( 5x, "Writing SatBiasAngle file ", a, "..." )' ) TRIM(OutFile)
  Error_Status = Write_SatBias( OutFile, SatBiasAngle )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing SatBiasAngle file '//TRIM(OutFile), &
                          Error_Status )
    STOP
  END IF

END PROGRAM SatBiasAngle_Mean
