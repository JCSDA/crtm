!
! SEcategory_Inspect
!
! Program to inspect the contents of an SEcategory file.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 31-Aug-2011
!                       paul.vandelst@noaa.gov
!

PROGRAM SEcategory_Inspect

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE File_Utility,      ONLY: File_Exists
  USE Message_Handler,   ONLY: SUCCESS, FAILURE, Program_Message, Display_Message
  USE SEcategory_Define, ONLY: SEcategory_type, &
                               SEcategory_Destroy, &
                               SEcategory_ReadFile, &
                               Inspect => SEcategory_Inspect
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'SEcategory_Inspect'
  !CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &

  ! ---------
  ! Variables
  ! ---------
  INTEGER :: err_stat
  CHARACTER(256) :: msg, filename
  TYPE(SEcategory_type) :: C

  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to display the contents of an '//&
                        'SEcategory file to stdout.', &
                        '$Revision$' )

  ! Get the filename
  WRITE( *,FMT='(/5x,"Enter the SEcategory filename: ")',ADVANCE='NO' )
  READ( *,'(a)' ) filename
  filename = ADJUSTL(filename)
  IF ( .NOT. File_Exists( TRIM(filename) ) ) THEN
    msg = 'File '//TRIM(filename)//' not found.'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF

  ! Read the binary data file
  err_stat = SEcategory_ReadFile( C, filename )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error reading SEcategory file '//TRIM(filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF

  ! Display the contents
  CALL Inspect( C )

  ! Clean up
  CALL SEcategory_Destroy( C )

END PROGRAM SEcategory_Inspect
