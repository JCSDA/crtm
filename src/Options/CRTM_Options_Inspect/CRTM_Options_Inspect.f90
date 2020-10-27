!
! CRTM_Options_Inspect
!
! Program to inspect the contents of a CRTM Options file.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 29-Apr-2012
!                       paul.vandelst@noaa.gov
!

PROGRAM CRTM_Options_Inspect

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE File_Utility       , ONLY: File_Exists
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, Program_Message, Display_Message
  USE CRTM_Options_Define, ONLY: CRTM_Options_type, &
                                 CRTM_Options_Destroy, &
                                 CRTM_Options_InquireFile, &
                                 CRTM_Options_ReadFile, &
                                 Inspect => CRTM_Options_Inspect
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'CRTM_Options_Inspect'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &

  ! ---------
  ! Variables
  ! ---------
  INTEGER :: err_stat, alloc_stat
  CHARACTER(256) :: filename, msg
  INTEGER :: m, n_profiles
  TYPE(CRTM_Options_type), ALLOCATABLE :: opt(:)

  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to display the contents of a CRTM '//&
                        'Binary format Options file to stdout.', &
                        '$Revision$' )

  ! Get the filename
  WRITE( *,FMT='(/5x,"Enter the CRTM Options filename: ")',ADVANCE='NO' )
  READ( *,'(a)' ) filename
  filename = ADJUSTL(filename)
  IF ( .NOT. File_Exists( TRIM(filename) ) ) THEN
    msg = 'File '//TRIM(filename)//' not found.'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF

  ! Inquire the file for the dimensions
  err_stat = CRTM_Options_InquireFile( &
               filename, &
               n_Profiles = n_profiles )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error inquiring CRTM Options file '//TRIM(filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF


  ! Allocate the output array
  ALLOCATE( opt(n_profiles), STAT=alloc_stat )
  IF ( alloc_stat /= 0 ) THEN
    WRITE( msg,'("Error allocating options array. STAT=",i0)' ) alloc_stat
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF


  ! Read the datafile
  err_stat = CRTM_Options_ReadFile( filename, opt, Quiet=.TRUE. )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error reading CRTM Options file '//TRIM(filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF


  ! Display the contents
  DO m = 1, n_profiles
    WRITE(*, FMT='(/1x,"PROFILE INDEX:",i0," - ")', ADVANCE='NO') m
    CALL Inspect( opt(m) )
  END DO


  ! Clean up
  DEALLOCATE( opt, STAT=alloc_stat )

END PROGRAM CRTM_Options_Inspect
