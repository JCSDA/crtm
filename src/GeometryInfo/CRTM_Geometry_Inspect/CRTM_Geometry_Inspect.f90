!
! CRTM_Geometry_Inspect
!
! Program to inspect the contents of a CRTM Geometry file.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 10-May-2012
!                       paul.vandelst@noaa.gov
!

PROGRAM CRTM_Geometry_Inspect

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE File_Utility        , ONLY: File_Exists
  USE Message_Handler     , ONLY: SUCCESS, FAILURE, Program_Message, Display_Message
  USE CRTM_Geometry_Define, ONLY: CRTM_Geometry_type, CRTM_Geometry_Destroy, &
                                  Inspect => CRTM_Geometry_Inspect, &
                                  CRTM_Geometry_InquireFile, &
                                  CRTM_Geometry_ReadFile
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'CRTM_Geometry_Inspect'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &

  ! ---------
  ! Variables
  ! ---------
  INTEGER :: err_stat, alloc_stat
  CHARACTER(256) :: filename, msg
  INTEGER :: n_profiles, m
  TYPE(CRTM_Geometry_type), ALLOCATABLE :: geo(:)

  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to display the contents of a CRTM '//&
                        'Binary format Geometry file to stdout.', &
                        '$Revision$' )

  ! Get the filename
  WRITE( *,FMT='(/5x,"Enter the CRTM Geometry filename: ")',ADVANCE='NO' )
  READ( *,'(a)' ) filename
  filename = ADJUSTL(filename)
  IF ( .NOT. File_Exists( TRIM(filename) ) ) THEN
    msg = 'File '//TRIM(filename)//' not found.'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF


  ! Inquire the file for the dimensions
  err_stat = CRTM_Geometry_InquireFile( filename, n_Profiles = n_profiles )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error inquiring CRTM Geometry file '//TRIM(filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF


  ! Allocate array
  ALLOCATE( geo(n_profiles), STAT=alloc_stat )
  IF ( alloc_stat /= 0 ) THEN
    WRITE( msg,'("Error allocating geo array. STAT=",i0)' ) alloc_stat
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF


  ! Read profile-only data file
  err_stat = CRTM_Geometry_ReadFile( filename, geo, Quiet=.TRUE. )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error reading CRTM Geometry file '//TRIM(filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF
  
  
  ! Display the contents
  DO m = 1, n_profiles
    WRITE(*, FMT='(1x,"PROFILE INDEX:",i0," - ")', ADVANCE='NO') m
    CALL Inspect( geo(m) )
  END DO
  
  
  ! Clean up
  DEALLOCATE( geo, STAT=alloc_stat )

END PROGRAM CRTM_Geometry_Inspect
