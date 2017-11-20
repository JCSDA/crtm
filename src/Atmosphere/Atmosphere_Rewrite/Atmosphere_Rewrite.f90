!
! Atmosphere_Rewrite
!
! Program to convert old Atmosphere data file formats to the latest one.
!
! This program RELIES upon:
!   1. The WRITE procedures being up to date with the latest format
!   2. The READ procedures still being set for the old format.
! Once this program has been run on existing OLD FORMAT datafiles,
! the read procedures can be updated.
!
! NOTE: This application OVERWRITES existing files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 08-Apr-2005
!                       paul.vandelst@noaa.gov
!

PROGRAM Atmosphere_Rewrite

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds
  USE Message_Handler
  USE File_Utility

  USE CRTM_Atmosphere_Define
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'Atmosphere_Rewrite'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &
    '$Id$'
  INTEGER, PARAMETER :: ML = 256
  LOGICAL, PARAMETER :: QUIET = .TRUE.
  
  
  ! ---------
  ! Variables
  ! ---------
  CHARACTER(ML) :: err_msg
  CHARACTER(ML) :: filename
  INTEGER :: n_args
  INTEGER :: err_stat, alloc_stat
  INTEGER :: n_channels, n_profiles
  TYPE(CRTM_Atmosphere_type), ALLOCATABLE :: atm(:), atm_K(:)


  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to read old format Atmosphere datafiles and '//&
                        'write new format files. NOTE: This application OVERWRITES '//&
                        'existing files.', &
                        '$Revision$' )


  ! Get the filename
  n_args = COMMAND_ARGUMENT_COUNT() 
  IF ( n_args > 0 ) THEN 
    CALL GET_COMMAND_ARGUMENT(1, filename) 
  ELSE 
    WRITE( *,FMT='(/5x,"Enter the Binary Atmosphere filename: ")',ADVANCE='NO' ) 
    READ( *,'(a)' ) filename 
  END IF
  filename = ADJUSTL(filename)
  IF ( .NOT. File_Exists( TRIM(filename) ) ) THEN
    err_msg = 'File '//TRIM(filename)//' not found.'
    CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
  END IF
  
  WRITE(*,'(/5x,"Reformatting ",a,"...")') TRIM(filename)


  ! Inquire the old format file
  err_stat = CRTM_Atmosphere_InquireFile( &
    filename, &
    n_Channels = n_channels, &
    n_Profiles = n_profiles  )
  IF ( err_stat /= SUCCESS ) THEN
    err_msg = 'Error inquiring the old format Atmosphere file.'
    CALL Display_Message( PROGRAM_NAME, err_msg, err_stat ); STOP
  END IF


  ! Branch on the number of channels
  IF ( n_channels > 0 ) THEN
  
    ! We have a K-matrix file
    ! ...Read the old format file
    err_stat = CRTM_Atmosphere_ReadFile( &
      filename, &
      atm_K   , &
      Quiet = QUIET )
    IF ( err_stat /= SUCCESS ) THEN
      err_msg = 'Error reading the old format Atmosphere K-matrix file.'
      CALL Display_Message( PROGRAM_NAME, err_msg, err_stat ); STOP
    END IF
    ! ...Write the new format file
    err_stat = CRTM_Atmosphere_WriteFile( &
      filename, &
      atm_K   , &
      Quiet = QUIET )
    IF ( err_stat /= SUCCESS ) THEN
      err_msg = 'Error writing the new format Atmosphere K-matrix file.'
      CALL Display_Message( PROGRAM_NAME, err_msg, err_stat ); STOP
    END IF
    ! ...Cleanup
    DEALLOCATE( atm_K, STAT=alloc_stat )

  ELSE
  
    ! We have a "regular" file file
    ! ...Read the old format file
    err_stat = CRTM_Atmosphere_ReadFile( &
      filename, &
      atm     , &
      Quiet = QUIET )
    IF ( err_stat /= SUCCESS ) THEN
      err_msg = 'Error reading the old format Atmosphere file.'
      CALL Display_Message( PROGRAM_NAME, err_msg, err_stat ); STOP
    END IF

    ! ...Write the new format file
    err_stat = CRTM_Atmosphere_WriteFile( &
      filename, &
      atm     , &
      Quiet = QUIET )
    IF ( err_stat /= SUCCESS ) THEN
      err_msg = 'Error writing the new format Atmosphere file.'
      CALL Display_Message( PROGRAM_NAME, err_msg, err_stat ); STOP
    END IF
    ! ...Cleanup
    DEALLOCATE( atm, STAT=alloc_stat )
    
  END IF

END PROGRAM Atmosphere_Rewrite
