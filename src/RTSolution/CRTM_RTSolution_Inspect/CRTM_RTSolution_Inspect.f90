!
! CRTM_RTSolution_Inspect
!
! Program to inspect the contents of a CRTM RTSolution file.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 29-Mar-2012
!                       paul.vandelst@noaa.gov
!

PROGRAM CRTM_RTSolution_Inspect

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE File_Utility          , ONLY: File_Exists
  USE Message_Handler       , ONLY: SUCCESS, FAILURE, Program_Message, Display_Message
  USE CRTM_RTSolution_Define, ONLY: CRTM_RTSolution_type, CRTM_RTSolution_Destroy, &
                                    Inspect => CRTM_RTSolution_Inspect, &
                                    CRTM_RTSolution_InquireFile, &
                                    CRTM_RTSolution_ReadFile
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'CRTM_RTSolution_Inspect'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &

  ! ---------
  ! Variables
  ! ---------
  INTEGER :: err_stat, alloc_stat
  CHARACTER(256) :: filename, msg
  INTEGER :: n_channels, n_profiles
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: rts(:,:)

  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to display the contents of a CRTM '//&
                        'Binary format RTSolution file to stdout.', &
                        '$Revision$' )

  ! Get the filename
  WRITE( *,FMT='(/5x,"Enter the CRTM RTSolution filename: ")',ADVANCE='NO' )
  READ( *,'(a)' ) filename
  filename = ADJUSTL(filename)
  IF ( .NOT. File_Exists( TRIM(filename) ) ) THEN
    msg = 'File '//TRIM(filename)//' not found.'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF


  ! Inquire the file for the dimensions
  err_stat = CRTM_RTSolution_InquireFile( filename, &
                                          n_Channels = n_channels, &
                                          n_Profiles = n_profiles )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error inquiring CRTM RTSolution file '//TRIM(filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF


  ! Allocate channel x profile array
  ALLOCATE( rts(n_channels,n_profiles), STAT=alloc_stat )
  IF ( alloc_stat /= 0 ) THEN
    WRITE( msg,'("Error allocating RTSolution array. STAT=",i0)' ) alloc_stat
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF


  ! Read data file
  err_stat = CRTM_RTSolution_ReadFile( filename, rts, Quiet=.TRUE. )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error reading CRTM RTSolution file '//TRIM(filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF


  ! Display the contents
  CALL Inspect( rts )


  ! Clean up
  DEALLOCATE( rts, STAT=alloc_stat )

END PROGRAM CRTM_RTSolution_Inspect
