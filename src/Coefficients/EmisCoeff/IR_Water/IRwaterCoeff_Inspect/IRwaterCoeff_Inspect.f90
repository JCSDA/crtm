!
! IRwaterCoeff_Inspect
!
! Program to inspect the contents of an IRwaterCoeff file.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 04-May-2012
!                       paul.vandelst@noaa.gov
!

PROGRAM IRwaterCoeff_Inspect

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE File_Utility,        ONLY: File_Exists
  USE Message_Handler,     ONLY: SUCCESS, FAILURE, Program_Message, Display_Message
  USE IRwaterCoeff_Define, ONLY: IRwaterCoeff_type, &
                                 IRwaterCoeff_Destroy, &
                                 IRwaterCoeff_ReadFile, &
                                 Inspect => IRwaterCoeff_Inspect
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'IRwaterCoeff_Inspect'
  !CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &

  ! ---------
  ! Variables
  ! ---------
  INTEGER :: err_stat
  CHARACTER(2000) :: title
  CHARACTER(2000) :: history
  CHARACTER(2000) :: comment
  CHARACTER(256) :: msg, filename
  TYPE(IRwaterCoeff_type) :: C

  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to display the contents of an '//&
                        'IRwaterCoeff file to stdout.', &
                        '$Revision$' )

  ! Get the filename
  WRITE( *,FMT='(/5x,"Enter the IRwaterCoeff filename: ")',ADVANCE='NO' )
  READ( *,'(a)' ) filename
  filename = ADJUSTL(filename)
  IF ( .NOT. File_Exists( TRIM(filename) ) ) THEN
    msg = 'File '//TRIM(filename)//' not found.'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF

  ! Read the binary data file
  err_stat = IRwaterCoeff_ReadFile( &
    C, &
    filename, &
    Title   = title  , &
    History = history, &
    Comment = comment  )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error reading IRwaterCoeff file '//TRIM(filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF

  ! Display the contents
  WRITE(*,'(/5x,"TITLE attribute:",/,a)') TRIM(title)
  WRITE(*,'(/5x,"HISTORY attribute:",/,a)') TRIM(history)
  WRITE(*,'(/5x,"COMMENT attribute:",/,a)') TRIM(comment)
  CALL Inspect( C )

  ! Clean up
  CALL IRwaterCoeff_Destroy( C )

END PROGRAM IRwaterCoeff_Inspect
