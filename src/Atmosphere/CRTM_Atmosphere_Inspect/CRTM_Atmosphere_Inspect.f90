!
! CRTM_Atmosphere_Inspect
!
! Program to inspect the contents of a CRTM Atmosphere file.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 13-Apr-2011
!                       paul.vandelst@noaa.gov
!

PROGRAM CRTM_Atmosphere_Inspect

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE File_Utility          , ONLY: File_Exists
  USE Message_Handler       , ONLY: SUCCESS, FAILURE, Program_Message, Display_Message
  USE CRTM_Atmosphere_Define, ONLY: CRTM_Atmosphere_type, CRTM_Atmosphere_Destroy, &
                                    Inspect => CRTM_Atmosphere_Inspect, &
                                    CRTM_Atmosphere_InquireFile, &
                                    CRTM_Atmosphere_ReadFile
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'CRTM_Atmosphere_Inspect'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: filename, msg
  INTEGER :: n_args
  INTEGER :: err_stat, alloc_stat
  INTEGER :: n_channels, n_profiles, l, m
  TYPE(CRTM_Atmosphere_type), ALLOCATABLE :: r1_atm(:), r2_atm(:,:)

  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to display the contents of a CRTM '//&
                        'Binary format Atmosphere file to stdout.', &
                        '$Revision$' )

  ! Get the filename
  n_args = COMMAND_ARGUMENT_COUNT()
  IF ( n_args > 0 ) THEN
    CALL GET_COMMAND_ARGUMENT(1, filename)
  ELSE
    WRITE( *,FMT='(/5x,"Enter the CRTM Atmosphere filename: ")',ADVANCE='NO' )
    READ( *,'(a)' ) filename
  END IF
  filename = ADJUSTL(filename)
  IF ( .NOT. File_Exists( TRIM(filename) ) ) THEN
    msg = 'File '//TRIM(filename)//' not found.'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF

  ! Inquire the file for the dimensions
  err_stat = CRTM_Atmosphere_InquireFile( filename, &
                                          n_Channels = n_channels, &
                                          n_Profiles = n_profiles )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error inquiring CRTM Atmosphere file '//TRIM(filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF

  ! Display the data file appropriately
  IF ( n_channels == 0 ) THEN

    ! ...Allocate profile-only array
    ALLOCATE( r1_atm(n_profiles), STAT=alloc_stat )
    IF ( alloc_stat /= 0 ) THEN
      WRITE( msg,'("Error allocating rank-1 atmosphere array. STAT=",i0)' ) alloc_stat
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF
    ! ...Read profile-only data file
    err_stat = CRTM_Atmosphere_ReadFile( filename, r1_atm, Quiet=.TRUE. )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading CRTM Atmosphere file '//TRIM(filename)
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF
    ! ...Display the contents
    DO m = 1, n_profiles
      WRITE(*, FMT='(1x,"PROFILE INDEX:",i0," - ")', ADVANCE='NO') m
      CALL Inspect( r1_atm(m) )
    END DO
    ! ...Clean up
    DEALLOCATE( r1_atm, STAT=alloc_stat )

  ELSE

    ! ...Allocate channel x profile array
    ALLOCATE( r2_atm(n_channels,n_profiles), STAT=alloc_stat )
    IF ( alloc_stat /= 0 ) THEN
      WRITE( msg,'("Error allocating rank-2 atmosphere array. STAT=",i0)' ) alloc_stat
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF
    ! ...Read channel x profile data file
    err_stat = CRTM_Atmosphere_ReadFile( filename, r2_atm, Quiet=.TRUE. )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading CRTM Atmosphere file '//TRIM(filename)
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF
    ! ...Display the contents
    DO m = 1, n_profiles
      DO l = 1, n_channels
        WRITE(*, FMT='(1x,"CHANNEL,PROFILE INDICES:",i0,",",i0," - ")', ADVANCE='NO') l, m
        CALL Inspect( r2_atm(l,m) )
      END DO
    END DO
    ! ...Clean up
    DEALLOCATE( r2_atm, STAT=alloc_stat )

  END IF

END PROGRAM CRTM_Atmosphere_Inspect
