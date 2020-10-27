!
! RTSolution_Rewrite
!
! Program to convert old RTSolution data file formats to the latest one.
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
!       Written by:     Paul van Delst, 11-May-2012
!                       paul.vandelst@noaa.gov
!

PROGRAM RTSolution_Rewrite

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds
  USE Message_Handler
  USE File_Utility

  USE CRTM_RTSolution_Define
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME       = 'RTSolution_Rewrite'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &
  INTEGER, PARAMETER :: ML = 256
  LOGICAL, PARAMETER :: QUIET = .TRUE.


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(ML) :: err_msg
  CHARACTER(ML) :: filename
  INTEGER :: n_args
  INTEGER :: err_stat, alloc_stat
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: rts(:,:)


  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to read old format RTSolution datafiles and '//&
                        'write new format files. NOTE: This application OVERWRITES '//&
                        'existing files.', &
                        '$Revision$' )

  ! Get the filename
  n_args = COMMAND_ARGUMENT_COUNT() 
  IF ( n_args > 0 ) THEN 
    CALL GET_COMMAND_ARGUMENT(1, filename) 
  ELSE 
    WRITE( *,FMT='(/5x,"Enter the Binary RTSolution filename: ")',ADVANCE='NO' ) 
    READ( *,'(a)' ) filename 
  END IF
  filename = ADJUSTL(filename)
  IF ( .NOT. File_Exists( TRIM(filename) ) ) THEN
    err_msg = 'File '//TRIM(filename)//' not found.'
    CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
  END IF
  
  WRITE(*,'(/5x,"Reformatting ",a,"...")') TRIM(filename)


  ! Read the old format file
  err_stat = CRTM_RTSolution_ReadFile( &
    filename, &
    rts     , &
    Quiet = QUIET )
  IF ( err_stat /= SUCCESS ) THEN
    err_msg = 'Error reading the old format RTSolution file.'
    CALL Display_Message( PROGRAM_NAME, err_msg, err_stat ); STOP
  END IF
  
  
  ! Write the new format file
  err_stat = CRTM_RTSolution_WriteFile( &
    filename, &
    rts     , &
    Quiet = QUIET )
  IF ( err_stat /= SUCCESS ) THEN
    err_msg = 'Error writing the new format RTSolution file.'
    CALL Display_Message( PROGRAM_NAME, err_msg, err_stat ); STOP
  END IF
  

  ! Cleanup
  DEALLOCATE( rts, STAT=alloc_stat )

END PROGRAM RTSolution_Rewrite
