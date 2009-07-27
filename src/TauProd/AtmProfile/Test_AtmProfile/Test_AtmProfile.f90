!
! Test_AtmProfile
!
! Program to test the AtmProfile definition and netCDF I/O routines.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Aug-2004
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Test_AtmProfile
  
  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds          , ONLY: fp
  USE Message_Handler     , ONLY: SUCCESS, FAILURE, INFORMATION, &
                                  Display_Message, Program_Message
  USE File_Utility        , ONLY: File_Exists
  USE AtmProfile_Define   , ONLY: AtmProfile_type,         &
                                  Allocate_AtmProfile,     &
                                  Destroy_AtmProfile,      &
                                  Assign_AtmProfile,       &
                                  Associated_AtmProfile,   &
                                  Equal_AtmProfile,        &
                                  CheckRelease_AtmProfile, &
                                  Info_AtmProfile
  USE AtmProfile_netCDF_IO, ONLY: Inquire_AtmProfile_netCDF, &
                                  Read_AtmProfile_netCDF,    &
                                  Write_AtmProfile_netCDF

  ! Disable all implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------

  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Test_AtmProfile'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'

  CHARACTER(*), PARAMETER ::  INPUT_FILENAME  = 'Test.AtmProfile.nc'
  CHARACTER(*), PARAMETER ::  OUTPUT_FILENAME = 'Out.Test.AtmProfile.nc'

  INTEGER, PARAMETER :: SET = 1
  INTEGER, PARAMETER :: MAX_N_LOOPS  = 1000
  INTEGER, PARAMETER :: INFO_N_LOOPS = 100
  
  ! ---------
  ! Variables
  ! ---------
  INTEGER :: Error_Status
  INTEGER :: n_Layers   
  INTEGER :: n_Absorbers
  INTEGER :: n_Profiles 
  INTEGER :: m
  TYPE(AtmProfile_type), ALLOCATABLE :: AtmProfile1(:)
  TYPE(AtmProfile_type), ALLOCATABLE :: AtmProfile2(:)
  TYPE(AtmProfile_type), ALLOCATABLE :: AtmProfile3(:)
  INTEGER, ALLOCATABLE :: Profile_Set(:)
  
  ! Output header
  ! -------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test all the AtmProfile structure '//&
                        'manipulation and netCDF I/O functions.', &
                        '$Revision$' )
  
  
  ! Test the inquire function
  ! -------------------------
  WRITE( *,'(5x,"Testing the INQUIRE function...")' )
  Error_Status = Inquire_AtmProfile_netCDF( INPUT_FILENAME, &
                                            n_Layers    = n_Layers, &
                                            n_Absorbers = n_Absorbers, &
                                            n_Profiles  = n_Profiles )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error inquiring the netCDF AtmProfile file '//&
                          TRIM(INPUT_FILENAME), &
                          FAILURE )
    STOP
  END IF

  
  ! Allocate arrays for tests
  ALLOCATE( AtmProfile1(n_Profiles)   , &
            AtmProfile2(n_Profiles)   , &
            AtmProfile3(n_Profiles+10), &
            Profile_Set(n_Profiles)     )   


  ! Create profile set list
  Profile_Set = (/ (m, m = 1, n_Profiles) /)
  
  
  ! Test the read function
  ! ----------------------                      
  WRITE( *,'(//5x,"Testing the READ function...")' )
  
  
  ! Read 1 profile at a time
  DO m = 1, n_Profiles
    Error_Status = Read_AtmProfile_netCDF( INPUT_FILENAME, &
                                           AtmProfile1(m:m), &
                                           Profile_Set=Profile_Set(m:m) )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error reading into the 1 element array from '//&
                            TRIM(INPUT_FILENAME), &
                            FAILURE )
      STOP
    END IF
  END DO


  ! Read Into an array that is the 
  ! same size as number of profiles in file
  Error_Status = Read_AtmProfile_netCDF( INPUT_FILENAME, &
                                         AtmProfile2, &
                                         Profile_Set=Profile_Set, &
                                         Quiet = SET )
  IF ( Error_Status /= SUCCESS ) THEN                              
    CALL Display_Message( PROGRAM_NAME, &                         
                          'Error reading into the array the same size as file '//& 
                           TRIM(INPUT_FILENAME), &            
                           FAILURE )                               
    STOP                                                          
  END IF                                                          

  
  ! Test that inputs are the same
  Error_Status = Equal_AtmProfile( AtmProfile1, AtmProfile2 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Read(Scalar)/Read(Rank-1)/Equal test failed',&
                          FAILURE )
    STOP                                                          
  END IF
  
  
  ! Read into an array that is 
  ! larger than number of profiles in file
  Error_Status = Read_AtmProfile_netCDF( INPUT_FILENAME, &
                                         AtmProfile3, &
                                         Quiet = SET )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &                         
                          'Error reading into array larger than size of file '//& 
                           TRIM(INPUT_FILENAME), &
                           FAILURE )
    STOP
  END IF

  
  ! Test that inputs are the same
  Error_Status = Equal_AtmProfile( AtmProfile2, AtmProfile3(1:n_Profiles) )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Read(=Rank-1)/Read(>Rank-1)/Equal test failed',&
                           FAILURE )
    STOP
  END IF

                                             
  ! Test the write function
  ! -----------------------                      
  WRITE( *,'(//5x,"Testing the WRITE function...")' )
  
  
  ! Write an AtmProfile structure array 
  ! to file without passing a profile_set
  Error_Status = Write_AtmProfile_netCDF( OUTPUT_FILENAME, &
                                          AtmProfile2 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing AtmProfile structure array to file '//&
                          TRIM(OUTPUT_FILENAME),&
                          FAILURE )
    STOP
  END IF
  
  ! Read the file just written
  Error_Status = Read_AtmProfile_netCDF( OUTPUT_FILENAME,  &
                                         AtmProfile2, &
                                         Quiet = SET )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading AtmProfile structure array from file '//&
                          TRIM(OUTPUT_FILENAME),&
                          FAILURE )
    STOP
  END IF
  
  
  ! Test the results are the same
  Error_Status = Equal_AtmProfile( AtmProfile1, AtmProfile2 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Write/Read/Equal test failed',&
                           FAILURE )
    STOP
  END IF
 
  
  ! Write an AtmProfile structure array
  ! to file using a profile_set argument
  Error_Status = Write_AtmProfile_netCDF( OUTPUT_FILENAME, &
                                          AtmProfile2, &
                                          Profile_Set = Profile_Set, &
                                          Quiet = SET )
  IF ( Error_Status /= SUCCESS ) THEN                              
    CALL Display_Message( PROGRAM_NAME, &                          
                          'Error writing AtmProfile structure array using Profile_Set to file '//&
                          TRIM(OUTPUT_FILENAME),&
                          FAILURE )
    STOP
  END IF


  ! Read the file just written
  Error_Status = Read_AtmProfile_netCDF( OUTPUT_FILENAME,  &
                                         AtmProfile2, &
                                         Quiet = SET )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading AtmProfile structure array from file '//&
                          TRIM(OUTPUT_FILENAME), &
                          FAILURE )
    STOP
  END IF

  
  ! Test the results are the same
  Error_Status = Equal_AtmProfile( AtmProfile1, AtmProfile2 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Write(Profile_Set)/Read/Equal test failed',&
                           FAILURE )
    STOP
  END IF

                                             
  ! Test the definition functions
  ! -----------------------------                      
  WRITE( *,'(//5x,"Testing the structure functions...")' )
  
  DO m = 1, n_Profiles
  
    WRITE( *,'(7x,"Profile #: ",i0)', ADVANCE='NO' ) m
    
    ! Test scalar assign routine
    Error_Status = Assign_AtmProfile( AtmProfile1(m), AtmProfile2(m) )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME,'Scalar Assign failed.',FAILURE )
      STOP
    ENDIF
    
    ! Test scalar association routine
    IF ( Associated_AtmProfile( AtmProfile1(m) ) ) THEN
      WRITE( *,'("; AtmProfile1 Associated")' )
    ELSE
      WRITE( *,'("; AtmProfile1 Not Associated")' )
    ENDIF
    
    ! Test CheckRelease    
    Error_Status = CheckRelease_AtmProfile( AtmProfile1(m) )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, 'CheckRelease failed', FAILURE )
      STOP
    ENDIF
    
    ! Test Scalar Destroy routine
    Error_Status = Destroy_AtmProfile( AtmProfile1(m) )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME,'Scalar Destroy failed.',FAILURE )
      STOP
    ENDIF

  END DO

  
  ! Test rank-1 assign routine
  Error_Status = Assign_AtmProfile( AtmProfile2, AtmProfile3(1:n_Profiles) )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME,'Rank-1 Assign failed.',FAILURE )
    STOP
  ENDIF

    
  ! Test rank-1 Allocate functions
  Error_Status = Allocate_AtmProfile( 20, 4, AtmProfile2 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, 'Rank-1 Allocate failed.', FAILURE )
    STOP
  ENDIF

  
  ! Test rank-1 Destroy functions
  Error_Status = Destroy_AtmProfile( AtmProfile2 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, 'Rank-1 Destroy(2) failed.', FAILURE )
    STOP
  ENDIF
  
  Error_Status = Destroy_AtmProfile( AtmProfile3 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, 'Rank-1 Destroy(3) failed.', FAILURE )
    STOP
  ENDIF

  
  ! Deallocate arrays
  DEALLOCATE( AtmProfile1, &
              AtmProfile2, &
              AtmProfile3, &
              Profile_Set  )   


  CALL Display_Message( PROGRAM_NAME, 'AtmProfile tests passed', SUCCESS )

END PROGRAM Test_AtmProfile
