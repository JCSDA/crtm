!
! AtmProfile_to_LBLinput
!
! Program to convert AtmProfile data into LBL input files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 09-Jul-2010
!                       paul.vandelst@noaa.gov
!

PROGRAM AtmProfile_to_LBLinput

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE File_Utility     , ONLY: File_Exists
  USE Message_Handler  , ONLY: SUCCESS, FAILURE, Program_Message, Display_Message
  USE AtmProfile_Define, ONLY: AtmProfile_type, &
                               AtmProfile_Destroy
  USE AtmProfile_IO    , ONLY: AtmProfile_InquireFile, &
                               AtmProfile_ReadFile
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'AtmProfile_to_LBLinput'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &

  ! ---------
  ! Variables
  ! ---------
  INTEGER :: err_stat, alloc_stat
  CHARACTER(256) :: msg
  CHARACTER(256) :: filename
  CHARACTER(5000) :: title, history, comment, profile_set_id
  INTEGER :: n_profiles, m
  TYPE(AtmProfile_type), ALLOCATABLE :: A(:)

  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to convert AtmProfile data into LBL input files.', &
                        '$Revision$' )

  ! Get the input AtmProfile filename
  WRITE( *,FMT='(/5x,"Enter the AtmProfile filename: ")',ADVANCE='NO' )
  READ( *,'(a)' ) filename
  filename = ADJUSTL(filename)
  IF ( .NOT. File_Exists( TRIM(filename) ) ) THEN
    CALL Display_Message( PROGRAM_NAME,'File '//TRIM(filename)//' not found.',FAILURE )
    STOP
  END IF


  ! Read the AtmProfile data
  ! ...Inquire the data file for the number of profiles
  err_stat = AtmProfile_InquireFile( Filename, n_Profiles = n_profiles )
  IF ( err_stat /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME,'Error reading '//TRIM(filename),FAILURE )
    STOP
  END IF
  ! ...Perform the allocation
  ALLOCATE( A(n_profiles), STAT=alloc_stat )
  IF ( alloc_stat /= 0 ) THEN
    WRITE( msg,'("Error allocating AtmProfile structure array. STAT = ",i0)' ) alloc_stat
    CALL Display_Message( PROGRAM_NAME,msg,FAILURE )
    STOP
  END IF
  ! ...Fill up the array with data
  err_stat = AtmProfile_ReadFile( Filename, &
                                  A, &
                                  Title          = title         , &
                                  History        = history       , &
                                  Comment        = comment       , &
                                  Profile_Set_Id = profile_set_id  )
  IF ( err_stat /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME,'Error reading data from '//TRIM(filename),FAILURE )
    STOP
  END IF




  
  ! Clean up
  CALL AtmProfile_Destroy( A )
  WRITE(*,'(/2x,"Goodbye...")')
 
END PROGRAM AtmProfile_to_LBLinput
