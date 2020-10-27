!
! oSRF_Inspect
!
! Program to inspect the contents of an oSRF file.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 25-Aug-2014
!                       paul.vandelst@noaa.gov
!

PROGRAM oSRF_Inspect

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE File_Utility    , ONLY: File_Exists
  USE Message_Handler , ONLY: SUCCESS, FAILURE, Program_Message, Display_Message
  USE oSRF_File_Define, ONLY: oSRF_File_type, &
                              oSRF_File_Read, &
                              oSRF_File_Destroy, &
                              Inspect => oSRF_File_Inspect
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'oSRF_Inspect'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &

  ! ---------
  ! Variables
  ! ---------
  INTEGER :: err_stat
  CHARACTER(256) :: filename, msg
  TYPE(oSRF_File_type) :: osrf_file

  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to display the contents of a netCDF '//&
                        'format oSRF file to stdout.', &
                        '$Revision$' )

  ! Get the filename
  WRITE( *,FMT='(/5x,"Enter the netCDF oSRF filename: ")',ADVANCE='NO' )
  READ( *,'(a)' ) filename
  filename = ADJUSTL(filename)
  IF ( .NOT. File_Exists( TRIM(filename) ) ) THEN
    msg = 'File '//TRIM(filename)//' not found.'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF

  ! Read the data file
  err_stat = oSRF_File_Read( osrf_file, filename )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error reading netCDF oSRF file '//TRIM(filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF
  
  ! Display the contents
  CALL Inspect( osrf_file )

  ! Clean up
  CALL oSRF_File_Destroy( osrf_file )

END PROGRAM oSRF_Inspect
