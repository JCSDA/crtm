!
! NLTECoeff_Inspect
!
! Program to inspect the contents of a CRTM Binary format NLTECoeff file.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 25-Jan-2011
!                       paul.vandelst@noaa.gov
!

PROGRAM NLTECoeff_Inspect

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE File_Utility       , ONLY: File_Exists
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, Program_Message, Display_Message
  USE NLTECoeff_Define   , ONLY: NLTECoeff_type, NLTECoeff_Destroy, &
                                 Inspect => NLTECoeff_Inspect
  USE NLTECoeff_Binary_IO, ONLY: NLTECoeff_Binary_ReadFile
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'NLTECoeff_Inspect'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &

  ! ---------
  ! Variables
  ! ---------
  INTEGER :: err_stat
  CHARACTER(256) :: filename, msg
  TYPE(NLTECoeff_type) :: nc

  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to display the contents of a CRTM '//&
                        'Binary format NLTECoeff file to stdout.', &
                        '$Revision$' )

  ! Get the filename
  WRITE( *,FMT='(/5x,"Enter the Binary NLTECoeff filename: ")',ADVANCE='NO' )
  READ( *,'(a)' ) filename
  filename = ADJUSTL(filename)
  IF ( .NOT. File_Exists( TRIM(filename) ) ) THEN
    msg = 'File '//TRIM(filename)//' not found.'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF

  ! Read the binary data file
  err_stat = NLTECoeff_Binary_ReadFile( filename, nc )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error reading Binary NLTECoeff file '//TRIM(filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF

  ! Display the contents
  CALL Inspect( nc )

  ! Clean up
  CALL NLTECoeff_Destroy( nc )

END PROGRAM NLTECoeff_Inspect
