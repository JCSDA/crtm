!
! CRTM_AtmOptics_Inspect
!
! Program to inspect the contents of a CRTM AtmOptics file.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 12-Dec-2011
!                       paul.vandelst@noaa.gov
!

PROGRAM CRTM_AtmOptics_Inspect

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE File_Utility         , ONLY: File_Exists
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, Program_Message, Display_Message
  USE CRTM_AtmOptics_Define, ONLY: CRTM_AtmOptics_type, &
                                   CRTM_AtmOptics_Destroy, &
                                   CRTM_AtmOptics_ReadFile, &
                                   Inspect => CRTM_AtmOptics_Inspect
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'CRTM_AtmOptics_Inspect'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &

  ! ---------
  ! Variables
  ! ---------
  INTEGER :: err_stat
  CHARACTER(256) :: msg, filename
  TYPE(CRTM_AtmOptics_type), ALLOCATABLE :: C(:)

  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to display the contents of an '//&
                        'CRTM AtmOptics file to stdout.', &
                        '$Revision$' )

  ! Get the filename
  WRITE( *,FMT='(/5x,"Enter the CRTM AtmOptics filename: ")',ADVANCE='NO' )
  READ( *,'(a)' ) filename
  filename = ADJUSTL(filename)
  IF ( .NOT. File_Exists( TRIM(filename) ) ) THEN
    msg = 'File '//TRIM(filename)//' not found.'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF

  ! Read the binary data file
  err_stat = CRTM_AtmOptics_ReadFile( C, filename )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error reading CRTM AtmOptics file '//TRIM(filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF

  ! Display the contents
  CALL Inspect( C )

  ! Clean up
  CALL CRTM_AtmOptics_Destroy( C )

END PROGRAM CRTM_AtmOptics_Inspect
