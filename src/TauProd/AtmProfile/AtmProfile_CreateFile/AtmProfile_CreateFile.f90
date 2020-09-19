!
! AtmProfile_CreateFile
!
! Program to create netCDF AtmProfile data files. This format requires
! all the profiles to have the same number of levels/layers so profile
! sets with different layering between profiles are interpolated to the
! AIRS set of levels.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 15-Jul-2002
!                       paul.vandelst@noaa.gov
!

PROGRAM AtmProfile_CreateFile
  
  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                   Display_Message, Program_Message
  USE Interpolate_Utility  , ONLY: Polynomial_Interpolate
  USE Profile_Utility      , ONLY: ID_H2O, &
                                   MR_to_PP, &
                                   Geopotential_Height
  USE UMBC_Profile_Set     , ONLY: N_UMBC_PROFILES, &
                                   N_UMBC_ABSORBERS, &
                                   Load_UMBC_Profile
  USE CIMSS_Profile_Set    , ONLY: N_CIMSS_PROFILES, &
                                   N_CIMSS_ABSORBERS, &
                                   Load_CIMSS_Profile
  USE ECMWF52_Profile_Set  , ONLY: N_ECMWF52_PROFILES, &
                                   N_ECMWF52_ABSORBERS, &
                                   Load_ECMWF52_Profile
  USE ECMWF83_Profile_Set  , ONLY: N_ECMWF83_PROFILES, &
                                   N_ECMWF83_ABSORBERS, &
                                   Load_ECMWF83_Profile
  USE ECMWF5K_Profile_Set  , ONLY: N_ECMWF5K_PROFILES,  &
                                   N_ECMWF5K_ABSORBERS, &
                                   N_ECMWF5K_LAYERS,    &
                                   Load_ECMWF5K_Profile
  USE Model_Profile_Set    , ONLY: N_MODEL_PROFILES,  &
                                   N_MODEL_ABSORBERS, &
                                   Load_Model_Profile
  USE AtmProfile_Parameters, ONLY: N_ATMPROFILE_SETS,     &
                                   ATMPROFILE_SET, &
                                   N_ATMPROFILES, & 
                                   N_ATMPROFILE_LEVELS, &
                                   N_ATMPROFILE_LAYERS, &
                                   ATMPROFILE_LEVEL_PRESSURE
  USE AtmProfile_Define    , ONLY: OPERATOR(==), &
                                   AtmProfile_type, &
                                   AtmProfile_Associated, &
                                   AtmProfile_Destroy, &
                                   AtmProfile_Create, &
                                   AtmProfile_Inspect, &
                                   AtmProfile_Absorber_Name, &
                                   AtmProfile_Absorber_Units_Name, &
                                   AtmProfile_Absorber_Units_LBL

  USE AtmProfile_netCDF_IO , ONLY: AtmProfile_netCDF_WriteFile, &
                                   AtmProfile_netCDF_ReadFile
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'AtmProfile_CreateFile'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ZEROpointFIVE = 0.5_fp
  ! Pressure cutoff
  REAL(fp), PARAMETER :: PRESSURE_CUTOFF = 10.0_fp


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: message
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  CHARACTER(256) :: AtmProfile_Filename
  REAL(fp), POINTER :: Pressure(:)    => NULL()
  REAL(fp), POINTER :: Temperature(:) => NULL()
  REAL(fp), POINTER :: Absorber(:,:)  => NULL()
  REAL(fp) :: acorr(0:N_ATMPROFILE_LEVELS)
  REAL(fp) :: H2O_Pressure(0:N_ATMPROFILE_LAYERS)
  LOGICAL :: Profile_Interpolate
  INTEGER :: n_Layers, k, ik
  INTEGER :: n_Absorbers, j, j_idx(1)
  INTEGER :: n_Profiles, m, im
  INTEGER :: Profile_Set
  LOGICAL :: Quiet
  CHARACTER(512) :: History
  CHARACTER(256) :: Comment
  CHARACTER(256) :: Interpolation_Version_Id
  CHARACTER(256) :: Profile_Set_Version_Id
  TYPE(AtmProfile_type), ALLOCATABLE :: AtmProfile(:), Read_Check(:)
  INTEGER :: upper_k, lower_k



  ! Output descriuptive header
  ! --------------------------
  CALL Program_Message( PROGRAM_NAME, &
    'Program to create netCDF AtmProfile data files. This format requires all '//&
    'the profiles to have the same number of levels/layers so profile sets '//&
    'with different layering between profiles are interpolated to the AIRS '//&
    'set of levels.', &
    '$Revision$' )


  ! Setup for processing data
  ! ...Select a profile set to process
  Profile_Set_select: DO
    WRITE( *,'(/5x,"Select the profile set to process:")' )
    DO m = 1, N_ATMPROFILE_SETS
      WRITE( *,'(10x,i1,") ",a," set")' ) m, ATMPROFILE_SET( m )
    END DO
    WRITE( *,FMT='(5x,"Enter choice: ")',ADVANCE='NO' )
    READ( *,* ) Profile_Set
    IF ( Profile_Set > 0 .AND. Profile_Set <= N_ATMPROFILE_SETS ) EXIT
    WRITE( *,'(10x,"** Invalid selection **")' )
  END DO Profile_Set_select
  ! ...Assign dimensions
  SELECT CASE ( TRIM(ATMPROFILE_SET( Profile_Set) ) )
    CASE ( 'UMBC48' )
      n_Layers    = N_ATMPROFILE_LAYERS
      n_Absorbers = N_UMBC_ABSORBERS
      n_Profiles  = N_UMBC_PROFILES
      Profile_Interpolate = .TRUE.  ! Profiles at different layering
    CASE ( 'CIMSS32' )
      n_Layers    = N_ATMPROFILE_LAYERS
      n_Absorbers = N_CIMSS_ABSORBERS
      n_Profiles  = N_CIMSS_PROFILES
      Profile_Interpolate = .TRUE.
    CASE ( 'ECMWF52' )
      n_Layers    = N_ATMPROFILE_LAYERS
      n_Absorbers = N_ECMWF52_ABSORBERS
      n_Profiles  = N_ECMWF52_PROFILES
      Profile_Interpolate = .FALSE.  ! Already at 101 levels
    CASE ( 'ECMWF83' )
      n_Layers    = N_ATMPROFILE_LAYERS
      n_Absorbers = N_ECMWF83_ABSORBERS
      n_Profiles  = N_ECMWF83_PROFILES
      Profile_Interpolate = .FALSE.  ! Already at 101 levels
    CASE ( 'ECMWF5K' )
      n_Layers    = N_ECMWF5K_LAYERS
      n_Absorbers = N_ECMWF5K_ABSORBERS
      n_Profiles  = N_ECMWF5K_PROFILES
      Profile_Interpolate = .FALSE.  ! Keep at 91 levels (different pressures though)
    CASE ( 'Model6' )
      n_Layers    = N_ATMPROFILE_LAYERS
      n_Absorbers = N_MODEL_ABSORBERS
      n_Profiles  = N_MODEL_PROFILES
      Profile_Interpolate = .TRUE.
    CASE DEFAULT
      CALL Display_Message( PROGRAM_NAME,'This option not yet implemented.',INFORMATION )
      STOP
  END SELECT
  ! ...Doublecheck the number of profiles
  IF ( n_Profiles /= N_ATMPROFILES( Profile_Set ) ) THEN
    WRITE( Message,'("Number of ",a," profiles, ",i0,", is different from definition, ",i0,".")' ) &
                    TRIM(ATMPROFILE_SET( Profile_Set )), &
                    n_Profiles, &
                    N_ATMPROFILES( Profile_Set )
    CALL Display_Message( PROGRAM_NAME,Message,FAILURE ); STOP
  END IF

  
  ! Allocate the AtmProfile structure
  ! ...The array
  ALLOCATE( AtmProfile(n_Profiles), &
            STAT = Error_Status  )
  IF ( Error_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME,'Error allocating AtmProfile structure array.',FAILURE ); STOP
  END IF
  ! ...and internals
  CALL AtmProfile_Create( AtmProfile, n_Layers, n_Absorbers )
  IF ( ANY(.NOT. AtmProfile_Associated( AtmProfile ) ) ) THEN
    CALL Display_Message( PROGRAM_NAME,'Error allocating AtmProfile structures.',FAILURE ); STOP
  END IF


  ! Process profile by profile
  WRITE( *,* )
  m = 1
  Profile_Loop: DO im = 1, n_Profiles
  
    IF ( n_Profiles < 100 ) THEN
      WRITE( *,'(5x,"Processing ",a," profile #",i0)' ) TRIM(ATMPROFILE_SET(Profile_Set)), im
    ELSE
      IF ( MOD(im,100) == 0 ) THEN
        WRITE( *,'(5x,"Processing ",a," profile #",i0)' ) TRIM(ATMPROFILE_SET(Profile_Set)), im
      END IF
    END IF


    ! Load the profile data
    Load_Profile_Set: SELECT CASE ( TRIM(ATMPROFILE_SET( Profile_Set )) )
      CASE ( 'UMBC48' )
        Error_Status = Load_UMBC_Profile( im, &
                                          Pressure, &
                                          Temperature, &
                                          Absorber, &
                                          Absorber_ID       = AtmProfile(m)%Absorber_ID, &
                                          Absorber_Units_ID = AtmProfile(m)%Absorber_Units_ID, &
                                          Description       = AtmProfile(m)%Description, &
                                          Climatology_Model = AtmProfile(m)%Climatology_Model, &
                                          Year              = AtmProfile(m)%Year, &
                                          Month             = AtmProfile(m)%Month, &
                                          Day               = AtmProfile(m)%Day, &
                                          Hour              = AtmProfile(m)%Hour, &
                                          Latitude          = AtmProfile(m)%Latitude, &
                                          Longitude         = AtmProfile(m)%Longitude, &
                                          Surface_Altitude  = AtmProfile(m)%Surface_Altitude, &
                                          RCS_Id = Profile_Set_Version_Id )
      CASE ( 'CIMSS32' )
        Error_Status = Load_CIMSS_Profile( im, &
                                           Pressure, &
                                           Temperature, &
                                           Absorber, &
                                           Absorber_ID       = AtmProfile(m)%Absorber_ID, &
                                           Absorber_Units_ID = AtmProfile(m)%Absorber_Units_ID, &
                                           Description       = AtmProfile(m)%Description, &
                                           Climatology_Model = AtmProfile(m)%Climatology_Model, &
                                           Year              = AtmProfile(m)%Year, &
                                           Month             = AtmProfile(m)%Month, &
                                           Day               = AtmProfile(m)%Day, &
                                           Hour              = AtmProfile(m)%Hour, &
                                           Latitude          = AtmProfile(m)%Latitude, &
                                           Longitude         = AtmProfile(m)%Longitude, &
                                           Surface_Altitude  = AtmProfile(m)%Surface_Altitude, &
                                           RCS_Id = Profile_Set_Version_Id )
      CASE ( 'ECMWF52' )
        Error_Status = Load_ECMWF52_Profile( im, &
                                             Pressure, &
                                             Temperature, &
                                             Absorber, &
                                             Absorber_ID       = AtmProfile(m)%Absorber_ID, &
                                             Absorber_Units_ID = AtmProfile(m)%Absorber_Units_ID, &
                                             Description       = AtmProfile(m)%Description, &
                                             Climatology_Model = AtmProfile(m)%Climatology_Model, &
                                             Year              = AtmProfile(m)%Year, &
                                             Month             = AtmProfile(m)%Month, &
                                             Day               = AtmProfile(m)%Day, &
                                             Hour              = AtmProfile(m)%Hour, &
                                             Latitude          = AtmProfile(m)%Latitude, &
                                             Longitude         = AtmProfile(m)%Longitude, &
                                             Surface_Altitude  = AtmProfile(m)%Surface_Altitude, &
                                             RCS_Id = Profile_Set_Version_Id )
      CASE ( 'ECMWF83' )
        Error_Status = Load_ECMWF83_Profile( im, &
                                             Pressure, &
                                             Temperature, &
                                             Absorber, &
                                             Absorber_ID       = AtmProfile(m)%Absorber_ID, &
                                             Absorber_Units_ID = AtmProfile(m)%Absorber_Units_ID, &
                                             Description       = AtmProfile(m)%Description, &
                                             Climatology_Model = AtmProfile(m)%Climatology_Model, &
                                             Year              = AtmProfile(m)%Year, &
                                             Month             = AtmProfile(m)%Month, &
                                             Day               = AtmProfile(m)%Day, &
                                             Hour              = AtmProfile(m)%Hour, &
                                             Latitude          = AtmProfile(m)%Latitude, &
                                             Longitude         = AtmProfile(m)%Longitude, &
                                             Surface_Altitude  = AtmProfile(m)%Surface_Altitude, &
                                             RCS_Id = Profile_Set_Version_Id )
      CASE ( 'ECMWF5K' )
        Error_Status = Load_ECMWF5K_Profile( im, &
                                             Pressure, &
                                             Temperature, &
                                             Absorber, &
                                             Absorber_ID       = AtmProfile(m)%Absorber_ID, &
                                             Absorber_Units_ID = AtmProfile(m)%Absorber_Units_ID, &
                                             Description       = AtmProfile(m)%Description, &
                                             Climatology_Model = AtmProfile(m)%Climatology_Model, &
                                             Year              = AtmProfile(m)%Year, &
                                             Month             = AtmProfile(m)%Month, &
                                             Day               = AtmProfile(m)%Day, &
                                             Hour              = AtmProfile(m)%Hour, &
                                             Latitude          = AtmProfile(m)%Latitude, &
                                             Longitude         = AtmProfile(m)%Longitude, &
                                             Surface_Altitude  = AtmProfile(m)%Surface_Altitude, &
                                             RCS_Id = Profile_Set_Version_Id )
      CASE ( 'Model6' )
        Error_Status = Load_Model_Profile( im, &
                                           Pressure, &
                                           Temperature, &
                                           Absorber, &
                                           Absorber_ID       = AtmProfile(m)%Absorber_ID, &
                                           Absorber_Units_ID = AtmProfile(m)%Absorber_Units_ID, &
                                           Description       = AtmProfile(m)%Description, &
                                           Climatology_Model = AtmProfile(m)%Climatology_Model, &
                                           Year              = AtmProfile(m)%Year, &
                                           Month             = AtmProfile(m)%Month, &
                                           Day               = AtmProfile(m)%Day, &
                                           Hour              = AtmProfile(m)%Hour, &
                                           Latitude          = AtmProfile(m)%Latitude, &
                                           Longitude         = AtmProfile(m)%Longitude, &
                                           Surface_Altitude  = AtmProfile(m)%Surface_Altitude, &
                                           RCS_Id = Profile_Set_Version_Id )
      CASE DEFAULT
        CALL Display_Message( PROGRAM_NAME,'Invalid option.',FAILURE )
        STOP
    END SELECT Load_Profile_Set
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message,'("Error loading ",a," profile # ",i0,".")' ) &
                      TRIM(ATMPROFILE_SET( Profile_Set )), m
      CALL Display_Message( PROGRAM_NAME,Message,FAILURE ); STOP
    END IF

    ! Fill in the remainder of the structure
    AtmProfile(m)%Profile = im
    CALL AtmProfile_Absorber_Name(AtmProfile(m))
    CALL AtmProfile_Absorber_Units_Name(AtmProfile(m))
    CALL AtmProfile_Absorber_Units_LBL(AtmProfile(m))


    ! Only keep profiles that have high enough surface pressure
    IF ( Pressure(1) < PRESSURE_CUTOFF ) CYCLE Profile_loop
    
        
    ! Fill the pressure array with the required pressure levels/layers
    IF ( Profile_Interpolate ) THEN
      AtmProfile(m)%Level_Pressure = ATMPROFILE_LEVEL_PRESSURE
    ELSE
      AtmProfile(m)%Level_Pressure = Pressure
    END IF
    AtmProfile(m)%Layer_Pressure = ( AtmProfile(m)%Level_Pressure(0:n_Layers-1) - &
                                     AtmProfile(m)%Level_Pressure(1:n_Layers) ) / &
    !                              -------------------------------------------------
                                    LOG( AtmProfile(m)%Level_Pressure(0:n_Layers-1) / &
                                         AtmProfile(m)%Level_Pressure(1:n_Layers) )       

    
    ! Interpolate the profile as necessary
    Perform_Interpolation: IF ( Profile_Interpolate ) THEN

      ! Set the AtmProfile file COMMENT attribute
      Comment = 'Original profiles interpolated to the 101 AIRS pressure levels.'


      ! Determine index bounds of valid output data
      ! ...Lower
      lower_k_loop: DO lower_k = 0, n_Layers
        IF ( AtmProfile(m)%Level_Pressure(lower_k) <= MAXVAL(Pressure) ) EXIT lower_k_loop
      END DO lower_k_loop
      ! ...Upper
      upper_k_loop: DO upper_k = n_Layers, 0, -1
        IF ( AtmProfile(m)%Level_Pressure(upper_k) >= MINVAL(Pressure) ) EXIT upper_k_loop
      END DO upper_k_loop


      ! Interpolate the temperature
      Error_Status = Polynomial_Interpolate( LOG(Pressure),                     &  ! X
                                             Temperature,                       &  ! Y
                                             LOG(AtmProfile(m)%Level_Pressure), &  ! XINT
                                             AtmProfile(m)%Level_Temperature,   &  ! YINT
                                             Order  = 1,                        &  ! Linear
                                             RCS_Id = Interpolation_Version_Id )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message,'("Error interpolating temperature for profile #",i0,".")' ) m
        CALL Display_Message( PROGRAM_NAME,TRIM(Message),FAILURE ); STOP
      END IF


      ! Interpolate the absorber amounts
      Absorber_Interpolation_Loop: DO j = 1, n_Absorbers
        Error_Status = Polynomial_Interpolate( LOG(Pressure),                     &  ! X
                                               Absorber(:,j),                     &  ! Y
                                               LOG(AtmProfile(m)%Level_Pressure), &  ! XINT
                                               AtmProfile(m)%Level_Absorber(:,j), &  ! YINT
                                               Order = 1                          )  ! Linear
        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message,'("Error interpolating absorber#",i0," amount, profile #",i0,".")' ) j, m
          CALL Display_Message( PROGRAM_NAME,TRIM(Message),FAILURE ); STOP
        END IF


        ! Check the interpolation result and correct if necessary
        Negative_Absorber: IF ( ANY(AtmProfile(m)%Level_Absorber(:,j) < ZERO) ) THEN
          ! Load data into correction array
          acorr(0:n_Layers) = AtmProfile(m)%Level_Absorber(:,j)
          acorr(n_Layers+1) = acorr(n_Layers)
          ! Loop over all levels
          Correction_Check_Loop: DO k = 0, n_Layers
            ! Check for non-physical absorber amount
            Correct_Absorber: IF ( acorr(k) > ZERO ) THEN
              ! No correction, cycle loop
              CYCLE Correction_Check_Loop
            ELSE Correct_Absorber
              ! Set invalid value equal to next non-negative value
              Correction_Loop: DO ik = k+1, n_Layers+1
                IF ( acorr( ik ) > ZERO ) THEN
                  acorr(k) = acorr(ik)
                  EXIT Correction_Loop
                END IF
              END DO Correction_Loop
            END IF Correct_Absorber
          END DO Correction_Check_Loop
          ! Restore the corrected profile
          AtmProfile(m)%Level_Absorber(:,j) = acorr(0:n_Layers)
        END IF Negative_Absorber
      END DO Absorber_Interpolation_Loop

    ELSE Perform_Interpolation

      ! No interpolation, just copy the data
      ! Set the AtmProfile file COMMENT attribute
      Comment = 'No interpolation performed.'


      ! Copy the temperature and absorber data
      AtmProfile(m)%Level_Temperature = Temperature
      AtmProfile(m)%Level_Absorber    = Absorber

    END IF Perform_Interpolation


    ! Compute geopotential height and thicknesses
    ! ...Is water vapor present?
    IF ( COUNT(AtmProfile(m)%Absorber_ID == ID_H2O) /= 1 ) THEN
      CALL Display_Message( PROGRAM_NAME,'No water vapor data.',FAILURE ); STOP
    END IF
    ! ...Find the water vapor index
    j_idx = PACK((/ ( j, j = 1, AtmProfile(m)%n_Absorbers ) /), &
                 AtmProfile(m)%Absorber_ID == ID_H2O)
    ! ...Convert from mixing ratio
    CALL MR_to_PP( AtmProfile(m)%Level_Pressure, &
                   AtmProfile(m)%Level_Absorber(:,j_idx(1)), &
                   H2O_Pressure(0:n_Layers) )
    ! ...Compute the geopotential height profile
    Error_Status = Geopotential_Height( AtmProfile(m)%Level_Pressure,     &  ! Input
                                        AtmProfile(m)%Level_Temperature,  &  ! Input
                                        H2O_Pressure(0:n_Layers),         &  ! Input
                                        AtmProfile(m)%Level_Altitude,     &  ! Output
                                        Gravity_Correction = .TRUE.,      &  ! Optional Input
                                        Latitude = AtmProfile(m)%Latitude )  ! Optional Input
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME,'Error calculating geopotentials.',Error_Status ); STOP
    END IF
    ! ...Calculate the thicknesses
    AtmProfile(m)%Layer_Delta_Z = ABS(AtmProfile(m)%Level_Altitude(0:n_Layers-1) - &
                                      AtmProfile(m)%Level_Altitude(1:n_Layers  )   )


    ! Average the temperature
    Temperature_Average_loop: DO k = 1, n_Layers
      AtmProfile(m)%Layer_Temperature(k) = ZEROpointFIVE * &
                                          ( AtmProfile(m)%Level_Temperature( k-1 ) + &
                                            AtmProfile(m)%Level_Temperature( k   )   )
    END DO Temperature_Average_loop


    ! Average the absorbers
    DO j = 1, n_Absorbers
      DO k = 1, n_Layers
        AtmProfile(m)%Layer_Absorber(k,j) = ZEROpointFIVE * &
                                           ( AtmProfile(m)%Level_Absorber( k-1,j ) + &
                                             AtmProfile(m)%Level_Absorber( k  ,j )   )
      END DO
    END DO


    ! Deallocate pointers for next profile
    ! ...Pressure
    IF ( ASSOCIATED(Pressure) ) THEN
      DEALLOCATE( Pressure, STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message,'("Error deallocating PRESSURE input array for profile #",i0,". STAT = ",i0)') &
                        m, Allocate_Status
        CALL Display_Message( PROGRAM_NAME,TRIM(Message),WARNING )
      END IF
    END IF
    ! ...Temperature
    IF ( ASSOCIATED(Temperature) ) THEN
      DEALLOCATE( Temperature, STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message,'("Error deallocating TEMPERATURE input array for profile #",i0,". STAT = ",i0)') &
                        m, Allocate_Status
        CALL Display_Message( PROGRAM_NAME,TRIM(Message),WARNING )
      END IF
    END IF
    ! ...Absorber
    IF ( ASSOCIATED(Absorber) ) THEN
      DEALLOCATE( Absorber, STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message,'("Error deallocating ABSORBER input array for profile #",i0,". STAT = ",i0)') &
                        m, Allocate_Status
        CALL Display_Message( PROGRAM_NAME,TRIM(Message),WARNING )
      END IF
    END IF


    ! Increment valid profile counter
    m = m + 1
    
  END DO Profile_Loop
  
  
  ! Output number of profiles retained
  n_Profiles = m-1
  WRITE( *,'(/5x,"Number of profiles retained : ",i0)' ) n_Profiles


  ! Set dummy lat/lon before write
  WHERE( ABS(AtmProfile%Longitude) > 360.0_fp )
    AtmProfile%Longitude = -999.0_fp
  END WHERE
  WHERE( ABS(AtmProfile%Latitude) > 90.0_fp )
    AtmProfile%Latitude = -999.0_fp
  END WHERE


  ! Write the AtmProfile data file
  ! ...Create the netCDF file name
  WRITE( AtmProfile_Filename,'(a,".AtmProfile.nc")' ) &
                              TRIM(ATMPROFILE_SET( Profile_Set ))
  WRITE( *,'(/5x,"Writing the output file ",a,"...")' ) TRIM(AtmProfile_Filename)
  ! ...Create the History global attribute
  IF ( Profile_Interpolate ) THEN
    History = PROGRAM_VERSION_ID//'; '//&
              TRIM(Interpolation_Version_Id)// '; '//&
              TRIM(Profile_Set_Version_Id)
  ELSE
    History = PROGRAM_VERSION_ID//'; '//&
              TRIM(Profile_Set_Version_Id)
  END IF
  ! ...Set the quiet flag for large data sets
  IF ( n_Profiles > 100 ) THEN
    Quiet = .TRUE.
  ELSE
    Quiet = .FALSE.
  END IF
  ! ...Write the data to file
  Error_Status = AtmProfile_netCDF_WriteFile( AtmProfile, &
                                              AtmProfile_Filename, &
                                              Quiet   = Quiet, &
                                              Clobber = .TRUE., &
                                              Title   = TRIM(ATMPROFILE_SET( Profile_Set ))//&
                                                        ' atmospheric profile set.', &
                                              History = TRIM(History), &
                                              Comment = TRIM(Comment), &
                                              Profile_Set = TRIM(ATMPROFILE_SET( Profile_Set )) )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME,'Error writing '//TRIM(AtmProfile_Filename),FAILURE ); STOP
  END IF


  ! Test read the AtmProfile data file
  WRITE( *,'(/5x,"Test reading ",a,"...")' ) TRIM(AtmProfile_Filename)
  ! ...Read the data
  Error_Status = AtmProfile_netCDF_ReadFile( Read_Check, &
                                             AtmProfile_Filename, &
                                             Quiet = Quiet )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME,'Error reading '//TRIM(AtmProfile_Filename),FAILURE ); STOP
  END IF
  ! ...Compare the data
  IF ( .NOT. ALL(Read_Check == AtmProfile) ) THEN
    CALL Display_Message( PROGRAM_NAME,TRIM(AtmProfile_Filename)//' read check failed',FAILURE ); STOP
  END IF
  
  ! ...Print out some data
  m = MIN(6,n_Profiles)
  CALL AtmProfile_Inspect( AtmProfile(m) )


  ! Cleanup
  IF ( ALLOCATED(Read_Check) ) DEALLOCATE(Read_Check)
  IF ( ALLOCATED(AtmProfile) ) DEALLOCATE(AtmProfile)

END PROGRAM AtmProfile_CreateFile
