!
! MWwaterCoeff_Inspect
!
! Program to inspect the contents of an MWwaterCoeff file.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 10-Nov-2011
!                       paul.vandelst@noaa.gov
!

PROGRAM MWwaterCoeff_Inspect

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE File_Utility       , ONLY: File_Exists
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, Program_Message, Display_Message
  USE MWwaterCoeff_Define, ONLY: MWwaterCoeff_type, &
                                 MWwaterCoeff_Destroy, &
                                 MWwaterCoeff_ReadFile, &
                                 Inspect => MWwaterCoeff_Inspect
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'MWwaterCoeff_Inspect'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &

  ! ---------
  ! Variables
  ! ---------
  INTEGER :: err_stat
  CHARACTER(256) :: msg, filename
  TYPE(MWwaterCoeff_type) :: C

  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to display the contents of an '//&
                        'MWwaterCoeff file to stdout.', &
                        '$Revision$' )

  ! Get the filename
  WRITE( *,FMT='(/5x,"Enter the MWwaterCoeff filename: ")',ADVANCE='NO' )
  READ( *,'(a)' ) filename
  filename = ADJUSTL(filename)
  IF ( .NOT. File_Exists( TRIM(filename) ) ) THEN
    msg = 'File '//TRIM(filename)//' not found.'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF

  ! Read the binary data file
  err_stat = MWwaterCoeff_ReadFile( C, filename )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error reading MWwaterCoeff file '//TRIM(filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF

  ! Display the contents
  CALL Inspect( C )

  ! Clean up
  CALL MWwaterCoeff_Destroy( C )

END PROGRAM MWwaterCoeff_Inspect
