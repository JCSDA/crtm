!
! LSEcategory_Inspect
!
! Program to inspect the contents of an LSEcategory file.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 31-Aug-2011
!                       paul.vandelst@noaa.gov
!

PROGRAM LSEcategory_Inspect

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE File_Utility,       ONLY: File_Exists
  USE Message_Handler,    ONLY: SUCCESS, FAILURE, Program_Message, Display_Message
  USE LSEcategory_Define, ONLY: LSEcategory_type, &
                                LSEcategory_Destroy, &
                                LSEcategory_ReadFile, &
                                Inspect => LSEcategory_Inspect
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'LSEcategory_Inspect'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'

  ! ---------
  ! Variables
  ! ---------
  INTEGER :: err_stat
  CHARACTER(256) :: msg, filename
  TYPE(LSEcategory_type) :: C

  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to display the contents of an '//&
                        'LSEcategory file to stdout.', &
                        '$Revision$' )

  ! Get the filename
  WRITE( *,FMT='(/5x,"Enter the LSEcategory filename: ")',ADVANCE='NO' )
  READ( *,'(a)' ) filename
  filename = ADJUSTL(filename)
  IF ( .NOT. File_Exists( TRIM(filename) ) ) THEN
    msg = 'File '//TRIM(filename)//' not found.'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF

  ! Read the binary data file
  err_stat = LSEcategory_ReadFile( C, filename )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error reading LSEcategory file '//TRIM(filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF

  ! Display the contents
  CALL Inspect( C )

  ! Clean up
  CALL LSEcategory_Destroy( C )

END PROGRAM LSEcategory_Inspect
