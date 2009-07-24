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

  CHARACTER(*), PARAMETER ::  NC_FILENAME = 'Test.AtmProfile.nc'

  INTEGER, PARAMETER :: SET = 1
  INTEGER, PARAMETER :: MAX_N_LOOPS  = 1000
  INTEGER, PARAMETER :: INFO_N_LOOPS = 100
  INTEGER, PARAMETER :: N_FILE_PROFILES = 3
  
  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Info
  CHARACTER(256) :: Message
  CHARACTER(256) :: AtmProfile_Filename
  INTEGER :: Error_Status
  LOGICAL :: Association_Status
  INTEGER :: n_Layers   
  INTEGER :: n_Absorbers
  INTEGER :: n_Profiles 
  INTEGER :: Allocate_Status
  INTEGER :: n, m
  TYPE(AtmProfile_type), DIMENSION(N_FILE_PROFILES)   :: AtmProfile1
  TYPE(AtmProfile_type), DIMENSION(N_FILE_PROFILES)   :: AtmProfile2
  TYPE(AtmProfile_type), DIMENSION(N_FILE_PROFILES+1) :: AtmProfile3
  INTEGER, DIMENSION(N_FILE_PROFILES) :: Profile_Set = (/1,2,3/)
  
  ! Output header
  ! -------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test all the AtmProfile structure '//&
                        'manipulation and netCDF I/O functions.', &
                        '$Revision$' )
  
  ! Get an input netCDF file
  ! ------------------------
  WRITE( *,FMT='(/5x,"Enter an netCDF AtmProfile filename: ")',ADVANCE='NO' )
  READ( *,'(a)' ) AtmProfile_Filename
  AtmProfile_Filename = ADJUSTL(AtmProfile_Filename)
  IF ( .NOT. File_Exists( TRIM(AtmProfile_Filename) ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'File '//TRIM(AtmProfile_Filename)//' not found.', &
                          FAILURE )
    STOP
  END IF
  
  ! Test the inquire function                      
  WRITE( *,'(10x,"Inquiring...")' )
  Error_Status = Inquire_AtmProfile_netCDF( AtmProfile_Filename, &
                                            n_Layers    = n_Layers, &
                                            n_Absorbers = n_Absorbers, &
                                            n_Profiles  = n_Profiles )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error inquiring the netCDF AtmProfile file '//&
                          TRIM(AtmProfile_Filename), &
                          FAILURE )
    STOP
  END IF
  
  ! Read 1 profile at a time
  DO m = 1, N_FILE_PROFILES
    
    Error_Status = Read_AtmProfile_netCDF( AtmProfile_Filename,          &
                                           AtmProfile1(m:m),             &
                                           Profile_Set=Profile_Set(m:m)  )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error reading into the 1 element array '//&
                            TRIM(AtmProfile_Filename), &
                            FAILURE )
      STOP
    END IF
    
  END DO
  
  ! Read Into an array that is the 
  ! same size as N_FILE_PROFILES  
  Error_Status = Read_AtmProfile_netCDF( AtmProfile_Filename,    &
                                         AtmProfile2,            &
                                         Profile_Set=Profile_Set )
  IF ( Error_Status /= SUCCESS ) THEN                              
    CALL Display_Message( PROGRAM_NAME,                                           &                         
                          'Error reading into the array of size N_FILE_PROFILES'//& 
                           TRIM(AtmProfile_Filename),                             &            
                           FAILURE                                                )                               
    STOP                                                          
  END IF                                                          
  
  ! Read into an array that is 
  ! larger than N_FILE_PROFILES
  Error_Status = Read_AtmProfile_netCDF( AtmProfile_Filename,  &
                                         AtmProfile3           )
  IF ( Error_Status /= SUCCESS ) THEN                              
    CALL Display_Message( PROGRAM_NAME,                                           &                         
                          'Error reading into array larger than N_FILE_PROFILES'//& 
                           TRIM(AtmProfile_Filename),                             &            
                           FAILURE                                                )                               
    STOP                                                          
  END IF   
  
  ! Compare Array structures of length N_FILE_PROFILES and (N_FILE_PROFILES + 1)
  Error_Status = Equal_AtmProfile( AtmProfile2              ,       &  ! Input
                                   AtmProfile3(1:N_FILE_PROFILES)   )
  IF ( Error_Status /= SUCCESS ) THEN                              
    CALL Display_Message( PROGRAM_NAME,&                         
                           'Structure array of N_FILE_PROFILES and (N_FILE_PROFILES+1)'//& 
                           'are not the same',&           
                            FAILURE                   )                               
    STOP                                                          
  END IF
 
  ! Compare the AtmProfile structure arrays where data is read in one profile
  ! at a time and where data is read in N_FILE_PROFILES at a time
  Error_Status = Equal_AtmProfile( AtmProfile1              , &  ! Input
                                   AtmProfile2                )
  IF ( Error_Status /= SUCCESS ) THEN                              
    CALL Display_Message( PROGRAM_NAME, &                          
                           'Structure Array read in one profile at a time is different '//&
                            &'from structure array that was read in N_FILE_PROFILES at a time',&             
                            FAILURE                )                               
    STOP                                                          
  END IF
                                             
  ! Write an AtmProfile structure array 
  ! to file without passing a profile_set
  Error_Status = Write_AtmProfile_netCDF( AtmProfile_Filename,  &
                                          AtmProfile2           )
  IF ( Error_Status /= SUCCESS ) THEN                              
    CALL Display_Message( PROGRAM_NAME, &                          
                           'Error writing AtmProfile structure array to file ',&
                            FAILURE                )                               
    STOP                                                          
  END IF
  
  ! Read the file written into an AtmProfile structure array
  Error_Status = Read_AtmProfile_netCDF( AtmProfile_Filename,  &
                                         AtmProfile2           )
  IF ( Error_Status /= SUCCESS ) THEN                              
    CALL Display_Message( PROGRAM_NAME, &                          
                           'Error writing AtmProfile structure array to file ',&
                            FAILURE                )                               
    STOP                                                          
  END IF
  
  ! Compare the file written in this test 
  ! program to the original file that was read
  Error_Status = Equal_AtmProfile( AtmProfile1              , &  ! Input
                                   AtmProfile2                )
  IF ( Error_Status /= SUCCESS ) THEN                              
    CALL Display_Message( PROGRAM_NAME, &                          
                           'Structure Array read in one profile at a time is different '//&
                            &'from structure array that was read in N_FILE_PROFILES at a time',&             
                            FAILURE                )                               
    STOP                                                          
  END IF
  
  ! Write an AtmProfile structure array
  ! to file using a profile_set argument
  Error_Status = Write_AtmProfile_netCDF( AtmProfile_Filename,  &
                                          AtmProfile2,          &
                                          Profile_Set           )                                          
  IF ( Error_Status /= SUCCESS ) THEN                              
    CALL Display_Message( PROGRAM_NAME, &                          
                           'Error writing AtmProfile structure array to file '//&
                           &'when passing a Profile_Set argument' ,&
                           FAILURE                   )                               
    STOP                                                          
  END IF
  
  ! Read the file written into an AtmProfile structure array
  Error_Status = Read_AtmProfile_netCDF( AtmProfile_Filename,  &
                                         AtmProfile2           )
  IF ( Error_Status /= SUCCESS ) THEN                              
    CALL Display_Message( PROGRAM_NAME, &                          
                           'Error writing AtmProfile structure array to file ',&
                            FAILURE                )                               
    STOP                                                          
  END IF
  
  ! Compare the file written in this test 
  ! program to the original file that was read
  Error_Status = Equal_AtmProfile( AtmProfile1              , &  ! Input
                                   AtmProfile2                )
  IF ( Error_Status /= SUCCESS ) THEN                              
    CALL Display_Message( PROGRAM_NAME, &                          
                           'Structure Array read in one profile at a time is different '//&
                            &'from structure array that was read in N_FILE_PROFILES at a time',&             
                            FAILURE                )                               
    STOP                                                          
  END IF    
  
  DO m=1, n_Profiles
  
    ! Test Assign routine
    Error_Status = Assign_AtmProfile( AtmProfile1(m),  &
                                      AtmProfile2(m)   )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME,                     &
                            'Not able to assign'&
                            &'AtmProfile2 from AtmProfile1',  &
                            FAILURE                           )
      STOP
    ENDIF
    
    ! Test CheckRelease    
    Error_Status = CheckRelease_AtmProfile( AtmProfile1(m) )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME,                       &
                            'Release number is not consistent', &
                            FAILURE                             )
      STOP
    ENDIF
    
    ! Test Destroy routine
    Error_Status = Destroy_AtmProfile( AtmProfile1(m) )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME,                        &
                            'Error re-initializing AtmProfile1', &
                            FAILURE                              )
      STOP
    ENDIF
    
    Error_Status = Destroy_AtmProfile( AtmProfile2(m) )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME,                        &
                            'Error re-initializing AtmProfile2', &
                            FAILURE                              )
      STOP
    ENDIF
    
    Error_Status = Destroy_AtmProfile( AtmProfile3(m) )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME,                        &
                            'Error re-initializing AtmProfile2', &
                            FAILURE                              )
      STOP
    ENDIF
       
  END DO 
  
  CALL Display_Message( PROGRAM_NAME,                        &
                        'Test of AtmProfile_netCDF_IO and '//&
                        &'AtmProfile_Define routines was'//&
                        &'succesful',                      &
                        SUCCESS                              )

END PROGRAM Test_AtmProfile
