!
! CRTM_Surface_Inspect
!
! Program to inspect the contents of a CRTM Surface file.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 09-Mar-2012
!                       paul.vandelst@noaa.gov
!

PROGRAM CRTM_Surface_Inspect

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE File_Utility       , ONLY: File_Exists
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, Program_Message, Display_Message
  USE CRTM_Surface_Define, ONLY: CRTM_Surface_type, CRTM_Surface_Destroy, &
                                 Inspect => CRTM_Surface_Inspect, &
                                 CRTM_Surface_InquireFile, &
                                 CRTM_Surface_ReadFile
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'CRTM_Surface_Inspect'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &

  ! ---------
  ! Variables
  ! ---------
  INTEGER :: err_stat, alloc_stat
  CHARACTER(256) :: filename, msg
  INTEGER :: n_channels, n_profiles, l, m
  TYPE(CRTM_Surface_type), ALLOCATABLE :: r1_sfc(:), r2_sfc(:,:)

  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to display the contents of a CRTM '//&
                        'Binary format Surface file to stdout.', &
                        '$Revision$' )

  ! Get the filename
  WRITE( *,FMT='(/5x,"Enter the CRTM Surface filename: ")',ADVANCE='NO' )
  READ( *,'(a)' ) filename
  filename = ADJUSTL(filename)
  IF ( .NOT. File_Exists( TRIM(filename) ) ) THEN
    msg = 'File '//TRIM(filename)//' not found.'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF

  ! Inquire the file for the dimensions
  err_stat = CRTM_Surface_InquireFile( filename, &
                                       n_Channels = n_channels, &
                                       n_Profiles = n_profiles )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error inquiring CRTM Surface file '//TRIM(filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF

  ! Display the data file appropriately
  IF ( n_channels == 0 ) THEN

    ! ...Allocate profile-only array
    ALLOCATE( r1_sfc(n_profiles), STAT=alloc_stat )
    IF ( alloc_stat /= 0 ) THEN
      WRITE( msg,'("Error allocating rank-1 sfcosphere array. STAT=",i0)' ) alloc_stat
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF
    ! ...Read profile-only data file
    err_stat = CRTM_Surface_ReadFile( filename, r1_sfc, Quiet=.TRUE. )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading CRTM Surface file '//TRIM(filename)
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF
    ! ...Display the contents
    DO m = 1, n_profiles
      WRITE(*, FMT='(1x,"PROFILE INDEX:",i0," - ")', ADVANCE='NO') m
      CALL Inspect( r1_sfc(m) )
    END DO
    ! ...Clean up
    DEALLOCATE( r1_sfc, STAT=alloc_stat )

  ELSE

    ! ...Allocate channel x profile array
    ALLOCATE( r2_sfc(n_channels,n_profiles), STAT=alloc_stat )
    IF ( alloc_stat /= 0 ) THEN
      WRITE( msg,'("Error allocating rank-2 sfcosphere array. STAT=",i0)' ) alloc_stat
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF
    ! ...Read channel x profile data file
    err_stat = CRTM_Surface_ReadFile( filename, r2_sfc, Quiet=.TRUE. )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading CRTM Surface file '//TRIM(filename)
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF
    ! ...Display the contents
    DO m = 1, n_profiles
      DO l = 1, n_channels
        WRITE(*, FMT='(1x,"CHANNEL,PROFILE INDICES:",i0,",",i0," - ")', ADVANCE='NO') l, m
        CALL Inspect( r2_sfc(l,m) )
      END DO
    END DO
    ! ...Clean up
    DEALLOCATE( r2_sfc, STAT=alloc_stat )

  END IF

END PROGRAM CRTM_Surface_Inspect
