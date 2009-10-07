!
! Dump_AtmProfile
!
! Program to dump AtmProfile contents to stdout.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 07-Oct-2009
!                       paul.vandelst@noaa.gov
!

PROGRAM Dump_AtmProfile
  
  ! -----------------
  ! Environment setup
  ! -----------------
  USE Message_Handler     , ONLY: SUCCESS, FAILURE, &
                                  Display_Message, Program_Message
  USE File_Utility        , ONLY: File_Exists
  USE AtmProfile_Define   , ONLY: AtmProfile_type, &
                                  Inspect_AtmProfile
  USE AtmProfile_netCDF_IO, ONLY: Inquire_AtmProfile_netCDF, &
                                  Read_AtmProfile_netCDF
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Dump_AtmProfile'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'

  
  ! ---------
  ! Variables
  ! ---------
  CHARACTER(512) :: Filename
  INTEGER :: Error_Status
  INTEGER :: n_Profiles 
  TYPE(AtmProfile_type) :: AtmProfile(1)
  INTEGER :: iProfile(1)

  
  ! Output header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to dump AtmProfile contents to stdout.', &
                        '$Revision$' )
  
  
  ! Inquire the file
  ! ...Get the filename
  WRITE(*, '(/2x,"Enter netCDF AtmProfile file to inspect : ")', ADVANCE='NO')
  READ(*, '(a)') Filename
  ! ...Check it exists
  IF ( .NOT. File_Exists(Filename) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'File '//TRIM(Filename)//' not found.', &
                          FAILURE )
    STOP
  END IF
  ! ...Get the number of profiles
  Error_Status = Inquire_AtmProfile_netCDF( Filename, n_Profiles = n_Profiles )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error inquiring the netCDF AtmProfile file '//&
                          TRIM(Filename), &
                          FAILURE )
    STOP
  END IF


  ! Begin open loop for dumping
  Dump_Loop: DO
  
    ! Enter profile to inspect
    WRITE(*, '(//4x,"Enter profile number [1-",i0,"] : ")', ADVANCE='NO') n_Profiles
    READ(*, *) iProfile(1)
    ! ...Exit if requested profile out of range
    IF ( iProfile(1) < 1 .OR. iProfile(1) > n_Profiles ) EXIT Dump_Loop
    
    ! Read the requested profile
    Error_Status = Read_AtmProfile_netCDF( Filename, &
                                           AtmProfile, &
                                           Profile_Set = iProfile, &
                                           Quiet = .TRUE. )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error reading requested profile from '//&
                            TRIM(Filename), &
                            FAILURE )
      STOP
    END IF
    
    ! Inspect it
    CALL Inspect_AtmProfile( AtmProfile(1) )
    
  END DO Dump_Loop
  
  WRITE(*,'(/2x,"Goodbye...")')
  
END PROGRAM Dump_AtmProfile
