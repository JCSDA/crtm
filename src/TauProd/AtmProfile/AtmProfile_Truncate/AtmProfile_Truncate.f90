!
! AtmProfile_Truncate
!
! Program to truncate an AtmProfile structure at a user specified pressure level
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 25-Oct-2007
!                       paul.vandelst@noaa.gov

PROGRAM AtmProfile_Truncate

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds          , ONLY: fp
  USE File_Utility        , ONLY: File_Exists
  USE Message_Handler     , ONLY: SUCCESS, FAILURE, &
                                  Display_Message, Program_Message
  USE AtmProfile_Define   , ONLY: AtmProfile_type, &
                                  AtmProfile_Associated, &
                                  AtmProfile_Create
  USE AtmProfile_netCDF_IO, ONLY: AtmProfile_netCDF_ReadFile, &
                                  AtmProfile_netCDF_WriteFile
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'AtmProfile_Truncate'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &
  ! LIteral constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(2000) :: title
  CHARACTER(2000) :: comment, truncation_comment
  CHARACTER(2000) :: history
  CHARACTER(256)  :: profile_set
  CHARACTER(256) :: msg
  CHARACTER(256) :: file_in, file_out
  INTEGER :: err_stat
  INTEGER :: alloc_stat
  INTEGER :: n_levels, n_layers
  INTEGER :: m, n_profiles
  REAL(fp) :: truncation_pressure
  TYPE(AtmProfile_type), ALLOCATABLE :: atm_in(:), atm_out(:)


  ! Program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to truncate an AtmProfile structure at a user '//&
                        'specified pressure level', &
                        '$Revision$' )

  ! Get user input
  ! ...An AtmProfile filename
  WRITE( *,'(/5x,"Enter an netCDF AtmProfile filename: ")',ADVANCE='NO' )
  READ( *,'(a)' ) file_in
  file_in = ADJUSTL(file_in)
  IF ( .NOT. File_Exists( file_in ) ) THEN
    msg = 'File '//TRIM(file_in)//' not found.'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF
  ! ...The truncation pressure
  WRITE( *,'(/5x,"Enter the truncation pressure (hPa): ")',ADVANCE='NO' )
  READ( *,* ) truncation_pressure
  WRITE( truncation_comment,'("Profile truncated at ",es13.6,"hPa")' ) truncation_pressure
  

  ! Read the AtmProfile file
  err_stat = AtmProfile_netCDF_ReadFile( &
    atm_in, &
    file_in, &
    Title       = title, &
    Comment     = comment, &
    History     = history, &
    Profile_set = profile_set )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error reading the AtmProfile file '//TRIM(file_in)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF


  ! Allocate output array structure
  n_profiles = SIZE(atm_in)
  ALLOCATE( atm_out(n_profiles), STAT=alloc_stat )
  IF ( alloc_stat /= 0 ) THEN
    msg = 'Error allocating the output AtmProfile array'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF


  ! Begin profile loop
  Profile_loop: DO m = 1, n_profiles
  
  
    ! Determine how many levels/layers to keep
    n_levels = MINLOC( truncation_pressure - atm_in(m)%Level_Pressure, &
                       DIM=1, &
                       MASK=(truncation_pressure - atm_in(m)%Level_Pressure) > ZERO ) - 1
    n_layers = n_levels-1

    WRITE( *,'(5x,"Truncating ",i0," levels from profile #",i0,"...")' ) &
             atm_in(m)%n_Layers - n_layers, m
         
         
    ! Allocate the truncated structure
    CALL AtmProfile_Create( &
      atm_out(m), &
      n_layers, &
      atm_in(m)%n_Absorbers )
    IF ( .NOT. AtmProfile_Associated(atm_out(m)) ) THEN
      WRITE( msg,'("Error allocating truncated AtmProfile for profile #",i0)' ) m
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF
  
  
    ! Copy structure data
    ! ...Version increment
    atm_out(m)%Version = atm_in(m)%Version + 1
    ! ...Metadata
    atm_out(m)%Profile           = atm_in(m)%Profile          
    atm_out(m)%Description       = atm_in(m)%Description      
    atm_out(m)%Climatology_Model = atm_in(m)%Climatology_Model
    atm_out(m)%Year              = atm_in(m)%Year             
    atm_out(m)%Month             = atm_in(m)%Month            
    atm_out(m)%Day               = atm_in(m)%Day              
    atm_out(m)%Hour              = atm_in(m)%Hour             
    atm_out(m)%Latitude          = atm_in(m)%Latitude         
    atm_out(m)%Longitude         = atm_in(m)%Longitude        
    atm_out(m)%Surface_Altitude  = atm_in(m)%Surface_Altitude 
    ! ...Absorber info
    atm_out(m)%Absorber_ID         = atm_in(m)%Absorber_ID
    atm_out(m)%Absorber_Name       = atm_in(m)%Absorber_Name
    atm_out(m)%Absorber_Units_ID   = atm_in(m)%Absorber_Units_ID
    atm_out(m)%Absorber_Units_Name = atm_in(m)%Absorber_Units_Name
    atm_out(m)%Absorber_Units_LBL  = atm_in(m)%Absorber_Units_LBL
    ! ...Profile LEVEL data
    atm_out(m)%Level_Pressure    = atm_in(m)%Level_Pressure(0:n_layers)
    atm_out(m)%Level_Temperature = atm_in(m)%Level_Temperature(0:n_layers)
    atm_out(m)%Level_Absorber    = atm_in(m)%Level_Absorber(0:n_layers,:)
    atm_out(m)%Level_Altitude    = atm_in(m)%Level_Altitude(0:n_layers)
    ! ...Profile LAYER data
    atm_out(m)%Layer_Pressure    = atm_in(m)%Layer_Pressure(1:n_layers)
    atm_out(m)%Layer_Temperature = atm_in(m)%Layer_Temperature(1:n_layers)
    atm_out(m)%Layer_Absorber    = atm_in(m)%Layer_Absorber(1:n_layers,:)
    atm_out(m)%Layer_Delta_Z     = atm_in(m)%Layer_Delta_Z(1:n_layers)

  END DO Profile_loop
  

  ! Write the truncated data to file
  file_out = 'Truncated.'//TRIM(file_in)
  err_stat = AtmProfile_netCDF_WriteFile( &
    atm_out, &
    file_out, &
    Quiet   = .TRUE., &
    Clobber = .TRUE., &
    Title       = TRIM(title)//' - truncated', &
    Comment     = TRIM(truncation_comment)//'; '//comment, &
    History     = PROGRAM_VERSION_ID//'; '//TRIM(history), &
    Profile_set = TRIM(profile_set) )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error writing the AtmProfile file '//TRIM(file_out)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF
  
  
  ! Clean up
  IF ( ALLOCATED(atm_out) ) DEALLOCATE(atm_out)
  IF ( ALLOCATED(atm_in ) ) DEALLOCATE(atm_in)

END PROGRAM AtmProfile_Truncate
