!
! AerosolCoeff_Inspect
!
! Program to inspect the contents of a CRTM Binary format AerosolCoeff file.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 20-Jun-2006
!                       paul.vandelst@noaa.gov
!

PROGRAM AerosolCoeff_Inspect

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE File_Utility,           ONLY: File_Exists
  USE Message_Handler,        ONLY: SUCCESS, FAILURE, Program_Message, Display_Message
  USE AerosolCoeff_Define,    ONLY: AerosolCoeff_type, AerosolCoeff_Destroy, &
                                    Inspect => AerosolCoeff_Inspect
  USE AerosolCoeff_Binary_IO, ONLY: AerosolCoeff_Binary_ReadFile
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'AerosolCoeff_Inspect'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &

  ! ---------
  ! Variables
  ! ---------
  INTEGER :: Error_Status
  CHARACTER(256) :: Filename
  TYPE(AerosolCoeff_type) :: A

  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to display the contents of a CRTM Binary format '//&
                        'AerosolCoeff file to stdout.', &
                        '$Revision$' )

  ! Get the filename
  WRITE( *,FMT='(/5x,"Enter the Binary AerosolCoeff filename: ")',ADVANCE='NO' )
  READ( *,'(a)' ) Filename
  Filename = ADJUSTL( Filename )
  IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'File '//TRIM( Filename )//' not found.', &
                          FAILURE )
    STOP
  END IF

  ! Read the binary data file
  Error_Status = AerosolCoeff_Binary_ReadFile( Filename, A )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading Binary AerosolCoeff file '//&
                          TRIM(Filename), &
                          Error_Status )
    STOP
  END IF

  ! Display the contents
  CALL Inspect( A )

  ! Clean up
  CALL AerosolCoeff_Destroy( A )

END PROGRAM AerosolCoeff_Inspect
