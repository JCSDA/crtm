!
! CloudCoeff_Inspect
!
! Program to inspect the contents of a CRTM Binary format CloudCoeff file.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 20-Jun-2006
!                       paul.vandelst@noaa.gov
!

PROGRAM CloudCoeff_Inspect

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE File_Utility,         ONLY: File_Exists
  USE Message_Handler,      ONLY: SUCCESS, FAILURE, Program_Message, Display_Message
  USE CloudCoeff_Define,    ONLY: CloudCoeff_type, CloudCoeff_Destroy, &
                                  Inspect => CloudCoeff_Inspect
  USE CloudCoeff_Binary_IO, ONLY: CloudCoeff_Binary_ReadFile
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'CloudCoeff_Inspect'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'

  ! ---------
  ! Variables
  ! ---------
  INTEGER :: Error_Status
  CHARACTER(256) :: Filename
  TYPE(CloudCoeff_type) :: C

  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to display the contents of a CRTM Binary format '//&
                        'CloudCoeff file to stdout.', &
                        '$Revision$' )

  ! Get the filename
  WRITE( *,FMT='(/5x,"Enter the Binary CloudCoeff filename: ")',ADVANCE='NO' )
  READ( *,'(a)' ) Filename
  Filename = ADJUSTL( Filename )
  IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'File '//TRIM( Filename )//' not found.', &
                          FAILURE )
    STOP
  END IF

  ! Read the binary data file
  Error_Status = CloudCoeff_Binary_ReadFile( Filename, C )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading Binary CloudCoeff file '//&
                          TRIM(Filename), &
                          Error_Status )
    STOP
  END IF

  ! Display the contents
  CALL Inspect( C )

  ! Clean up
  CALL CloudCoeff_Destroy( C )

END PROGRAM CloudCoeff_Inspect
