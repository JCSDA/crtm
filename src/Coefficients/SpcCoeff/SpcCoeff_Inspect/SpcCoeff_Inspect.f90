!
! SpcCoeff_Inspect
!
! Program to inspect the contents of a CRTM Binary format SpcCoeff file.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 03-Feb-2011
!                       paul.vandelst@noaa.gov
!

PROGRAM SpcCoeff_Inspect

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE File_Utility      , ONLY: File_Exists
  USE Message_Handler   , ONLY: SUCCESS, FAILURE, Program_Message, Display_Message
  USE SpcCoeff_Define   , ONLY: SpcCoeff_type, SpcCoeff_Destroy, &
                                Inspect => SpcCoeff_Inspect
  USE SpcCoeff_Binary_IO, ONLY: SpcCoeff_Binary_ReadFile
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'SpcCoeff_Inspect'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &
  '$Id$'

  ! ---------
  ! Variables
  ! ---------
  INTEGER :: err_stat
  CHARACTER(256) :: filename, msg
  INTEGER :: n_args
  TYPE(SpcCoeff_type) :: sc

  ! Generate a string containing the SpcCoeff release for info
  WRITE(msg,'(i10)') sc%Release
  
  
  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to display the contents of a CRTM '//&
                        'Binary format R'//TRIM(ADJUSTL(msg))//' SpcCoeff '//&
                        'file to stdout.', &
                        '$Revision$' )

  ! Get the filename
  n_args = COMMAND_ARGUMENT_COUNT() 
  IF ( n_args > 0 ) THEN 
    CALL GET_COMMAND_ARGUMENT(1, filename) 
  ELSE 
    WRITE( *,FMT='(/5x,"Enter the Binary SpcCoeff filename: ")',ADVANCE='NO' ) 
    READ( *,'(a)' ) filename 
  END IF
  filename = ADJUSTL(filename)
  IF ( .NOT. File_Exists( TRIM(filename) ) ) THEN
    msg = 'File '//TRIM(filename)//' not found.'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF

  ! Read the binary data file
  err_stat = SpcCoeff_Binary_ReadFile( filename, sc )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error reading Binary SpcCoeff file '//TRIM(filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF

  ! Display the contents
  CALL Inspect( sc )

  ! Clean up
  CALL SpcCoeff_Destroy( sc )

END PROGRAM SpcCoeff_Inspect
