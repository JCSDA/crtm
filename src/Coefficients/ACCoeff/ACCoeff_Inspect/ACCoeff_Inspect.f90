!
! ACCoeff_Inspect
!
! Program to inspect the contents of a CRTM Binary format ACCoeff file.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 27-Jan-2011
!                       paul.vandelst@noaa.gov
!

PROGRAM ACCoeff_Inspect

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE File_Utility     , ONLY: File_Exists
  USE Message_Handler  , ONLY: SUCCESS, FAILURE, Program_Message, Display_Message
  USE ACCoeff_Define   , ONLY: ACCoeff_type, ACCoeff_Destroy, &
                               Inspect => ACCoeff_Inspect
  USE ACCoeff_Binary_IO, ONLY: ACCoeff_Binary_ReadFile
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'ACCoeff_Inspect'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &

  ! ---------
  ! Variables
  ! ---------
  INTEGER :: err_stat
  CHARACTER(256) :: filename, msg
  TYPE(ACCoeff_type) :: nc

  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to display the contents of a CRTM '//&
                        'Binary format ACCoeff file to stdout.', &
                        '$Revision$' )

  ! Get the filename
  WRITE( *,FMT='(/5x,"Enter the Binary ACCoeff filename: ")',ADVANCE='NO' )
  READ( *,'(a)' ) filename
  filename = ADJUSTL(filename)
  IF ( .NOT. File_Exists( TRIM(filename) ) ) THEN
    msg = 'File '//TRIM(filename)//' not found.'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF

  ! Read the binary data file
  err_stat = ACCoeff_Binary_ReadFile( filename, nc )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error reading Binary ACCoeff file '//TRIM(filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF

  ! Display the contents
  CALL Inspect( nc )

  ! Clean up
  CALL ACCoeff_Destroy( nc )

END PROGRAM ACCoeff_Inspect
