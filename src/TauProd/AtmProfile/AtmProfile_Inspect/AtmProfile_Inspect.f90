!
! AtmProfile_Inspect
!
! Program to inspect the contents of an AtmProfile file.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 24-May-2010
!                       paul.vandelst@noaa.gov
!

PROGRAM AtmProfile_Inspect

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE File_Utility     , ONLY: File_Exists
  USE Message_Handler  , ONLY: SUCCESS, FAILURE, Program_Message, Display_Message
  USE AtmProfile_Define, ONLY: AtmProfile_type, &
                               AtmProfile_Destroy, &
                               Inspect => AtmProfile_Inspect
  USE AtmProfile_IO    , ONLY: AtmProfile_InquireFile, &
                               AtmProfile_ReadFile
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'AtmProfile_Inspect'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'

  ! ---------
  ! Variables
  ! ---------
  INTEGER :: err_stat
  CHARACTER(256) :: msg
  CHARACTER(256) :: Filename
  INTEGER :: n_Profiles, m
  TYPE(AtmProfile_type) :: A(1)

  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to display the contents of an '//&
                        'AtmProfile file to stdout.', &
                        '$Revision$' )

  ! Get the filename
  WRITE( *,FMT='(/5x,"Enter the AtmProfile filename: ")',ADVANCE='NO' )
  READ( *,'(a)' ) Filename
  Filename = ADJUSTL(Filename)
  IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
    CALL Display_Message( PROGRAM_NAME,'File '//TRIM(Filename)//' not found.',FAILURE )
    STOP
  END IF


  ! Inquire the data file for the number of profiles
  err_stat = AtmProfile_InquireFile( Filename, n_Profiles = n_Profiles )
  IF ( err_stat /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME,'Error reading '//TRIM(Filename),FAILURE )
    STOP
  END IF


  ! Begin open loop for inspection
  Inspect_Loop: DO
  
    ! Enter profile to inspect
    WRITE( *,'(//4x,"Enter profile number [1-",i0,"] : ")',ADVANCE='NO') n_Profiles
    READ( *,* ) m
    ! ...Exit if requested profile out of range
    IF ( m < 1 .OR. m > n_Profiles ) EXIT Inspect_Loop
    
    ! Read the requested profile
    err_stat = AtmProfile_ReadFile( Filename, A, Profile_List=(/m/) )
    IF ( err_stat /= SUCCESS ) THEN
      WRITE( msg,'("Error reading profile #",i0," from ",a)' ) m, TRIM(Filename)
      CALL Display_Message( PROGRAM_NAME,msg,FAILURE )
      STOP
    END IF
    
    ! Display the contents
    CALL Inspect( A(1) )

  END DO Inspect_Loop

  
  ! Clean up
  CALL AtmProfile_Destroy( A )
  WRITE(*,'(/2x,"Goodbye...")')
 
END PROGRAM AtmProfile_Inspect
