!
! BeCoeff_NC2BIN
!
! Program to convert a BeCoeff data file from netCDF to Binary format.
!
!
! CREATION HISTORY:
!       Written by: Paul van Delst, 13-Feb-2010
!                   paul.vandelst@noaa.gov
!

PROGRAM BeCoeff_NC2BIN

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE File_Utility   , ONLY: File_Exists
  USE Message_Handler, ONLY: SUCCESS, FAILURE, INFORMATION, &
                             Program_Message, Display_Message
  USE BeCoeff_Define , ONLY: BeCoeff_type, &
                             OPERATOR(==), &
                             BeCoeff_Destroy
  USE BeCoeff_IO     , ONLY: BeCoeff_WriteFile, &
                             BeCoeff_ReadFile
  USE SignalFile_Utility
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'BeCoeff_NC2BIN'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &
  
  ! ---------
  ! Variables
  ! ---------
  INTEGER :: Error_Status
  CHARACTER(256) :: NC_Filename, BIN_Filename
  TYPE(BeCoeff_type) :: BeC, BeC_Check
  
  ! Program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to convert a BeCoeff data file from netCDF to Binary format.', &
                        '$Revision$')
  
  ! Get the filenames
  ! ...Input netCDF
  WRITE(*,FMT='(/5x,"Enter the INPUT netCDF BeCoeff filename: ")', ADVANCE='NO')
  READ(*,'(a)') NC_Filename
  NC_Filename = ADJUSTL(NC_Filename)
  IF ( .NOT. File_Exists( TRIM(NC_Filename) ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'File '//TRIM(NC_Filename)//' not found.', &
                          FAILURE )
    STOP
  END IF
  ! ...Output Binary
  WRITE(*,FMT='(/5x,"Enter the OUTPUT Binary BeCoeff filename: ")', ADVANCE='NO')
  READ(*,'(a)') BIN_Filename
  BIN_Filename = ADJUSTL(BIN_Filename)
  ! ...Check that the netCDF file isn't accidentally overwritten
  IF ( TRIM(NC_Filename) == TRIM(BIN_Filename) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Output filename is the same as the input filename!', &
                          FAILURE )
    STOP
  END IF

  
  ! Read the netCDF File
  WRITE(*,'(/5x,"Reading the netCDF file...")')
  Error_Status = BeCoeff_ReadFile( NC_Filename, BeC, netCDF=.TRUE. )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading netCDF BeCoeff file '//&
                          TRIM(NC_Filename), &
                          Error_Status )
    STOP
  END IF
  
  ! Write the Binary File
  WRITE(*,'(/5x,"Writing the Binary file...")')
  Error_Status = BeCoeff_WriteFile( BIN_Filename, BeC )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing Binary BeCoeff file '//&
                          TRIM(BIN_Filename), &
                          Error_Status )
    STOP
  END IF
  
  ! Check the created Binary file
  WRITE(*,'(/5x,"Checking the Binary file...")')
  Error_Status = BeCoeff_ReadFile( BIN_Filename, BeC_Check )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error test reading generated Binary BeCoeff file '//&
                          TRIM(BIN_Filename), &
                          Error_Status )
    STOP
  END IF
  IF ( BeC == BeC_Check ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Structures are the same!', &
                          INFORMATION )
    CALL BeCoeff_Destroy( BeC )
    CALL BeCoeff_Destroy( BeC_Check )
  ELSE  
    CALL Display_Message( PROGRAM_NAME, &
                          'Structures are different!', &
                           FAILURE )
    STOP
  END IF

  ! Create a signal file indicating success
  Error_Status = Create_SignalFile( BIN_Filename )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error creating signal file for '//TRIM(BIN_Filename), &
                          FAILURE )
  END IF

END PROGRAM BeCoeff_NC2BIN

