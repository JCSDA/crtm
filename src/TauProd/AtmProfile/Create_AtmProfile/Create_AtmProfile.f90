!
! Create_AtmProfile
!
! Program to create netCDF AtmProfile data files. This format requires
! all the profiles to have the same number of levels/layers so profile
! sets with different layering between profiles are interpolated to the
! AIRS set of levels.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Jul-2002
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Create_AtmProfile

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
  USE Model_Profile_Set    , ONLY: N_MODEL_PROFILES, &
                                   N_MODEL_ABSORBERS, &
                                   Load_Model_Profile
  USE AtmProfile_Parameters, ONLY: N_ATMPROFILE_SETS, &
                                   ATMPROFILE_SET_ID_TAG, &
                                   N_ATMPROFILES, & 
                                   N_ATMPROFILE_LEVELS, &
                                   N_ATMPROFILE_LAYERS, &
                                   ATMPROFILE_LEVEL_PRESSURE
  USE AtmProfile_Define    , ONLY: AtmProfile_type, &
                                   Destroy_AtmProfile, &
                                   Allocate_AtmProfile
  USE AtmProfile_netCDF_IO , ONLY: Write_AtmProfile_netCDF, &
                                   Read_AtmProfile_netCDF
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Create_AtmProfile'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ZEROpointFIVE = 0.5_fp


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
  REAL(fp) :: acorr(N_ATMPROFILE_LEVELS+1)
  REAL(fp) :: H2O_Pressure(N_ATMPROFILE_LEVELS)
  LOGICAL :: Profile_Interpolate
  INTEGER :: n_Layers, k, ik
  INTEGER :: n_Levels
  INTEGER :: n_Absorbers, j, j_idx(1)
  INTEGER :: n_Profiles, m
  INTEGER :: Profile_Set
  CHARACTER(512) :: History
  CHARACTER(256) :: Comment
  CHARACTER(256) :: Interpolation_RCS_Id
  CHARACTER(256) :: Profile_Set_RCS_Id
  TYPE(AtmProfile_type) :: AtmProfile

  ! Output descriuptive header
  ! --------------------------
  CALL Program_Message( PROGRAM_NAME, &
    'Program to create netCDF AtmProfile data files. This format requires all '//&
    'the profiles to have the same number of levels/layers so profile sets '//&
    'with different layering between profiles are interpolated to the AIRS '//&
    'set of levels.', &
    '$Revision$' )


  ! Setup for processing data
  ! -------------------------
  ! Select a profile set to process
  Profile_Set_select: DO
    WRITE( *,'(/5x,"Select the profile set to process:")' )
    DO m = 1, N_ATMPROFILE_SETS
      WRITE( *,'(10x,i1,") ",a," set")' ) m, ATMPROFILE_SET_ID_TAG( m )
    END DO
    WRITE( *,FMT='(5x,"Enter choice: ")',ADVANCE='NO' )
    READ( *,* ) Profile_Set
    IF ( Profile_Set > 0 .AND. Profile_Set <= N_ATMPROFILE_SETS ) EXIT
    WRITE( *,'(10x,"** Invalid selection **")' )
  END DO Profile_Set_select

  ! Assign the number of available absorbers and
  ! profiles and set the interpolation flag
  SELECT CASE ( TRIM(ATMPROFILE_SET_ID_TAG( Profile_Set) ) )
    CASE ( 'UMBC48' )
      n_Layers    = N_ATMPROFILE_LAYERS
      n_Levels    = n_Layers + 1
      n_Absorbers = N_UMBC_ABSORBERS
      n_Profiles  = N_UMBC_PROFILES
      Profile_Interpolate = .TRUE.
    CASE ( 'CIMSS32' )
      n_Layers    = N_ATMPROFILE_LAYERS
      n_Levels    = n_Layers + 1
      n_Absorbers = N_CIMSS_ABSORBERS
      n_Profiles  = N_CIMSS_PROFILES
      Profile_Interpolate = .TRUE.
    CASE ( 'ECMWF52' )
      n_Layers    = N_ATMPROFILE_LAYERS
      n_Levels    = n_Layers + 1
      n_Absorbers = N_ECMWF52_ABSORBERS
      n_Profiles  = N_ECMWF52_PROFILES
      Profile_Interpolate = .FALSE.  ! Already at 101 levels
    CASE ( 'ECMWF83' )
      n_Layers    = N_ATMPROFILE_LAYERS
      n_Levels    = n_Layers + 1
      n_Absorbers = N_ECMWF83_ABSORBERS
      n_Profiles  = N_ECMWF83_PROFILES
      Profile_Interpolate = .FALSE.  ! Already at 101 levels
    CASE ( 'Model6' )
      n_Layers    = N_ATMPROFILE_LAYERS
      n_Levels    = n_Layers + 1
      n_Absorbers = N_MODEL_ABSORBERS
      n_Profiles  = N_MODEL_PROFILES
      Profile_Interpolate = .TRUE.
    CASE DEFAULT
      CALL Display_Message( PROGRAM_NAME,'This option not yet implemented.',INFORMATION )
      STOP
  END SELECT

  ! Doublecheck the number of profiles
  IF ( n_Profiles /= N_ATMPROFILES( Profile_Set ) ) THEN
    WRITE( Message,'("Number of ",a," profiles, ",i0,", is different from definition, ",i0,".")' ) &
                    TRIM(ATMPROFILE_SET_ID_TAG( Profile_Set )), &
                    n_Profiles, &
                    N_ATMPROFILES( Profile_Set )
    CALL Display_Message( PROGRAM_NAME,TRIM(Message),FAILURE ); STOP
  END IF

  ! Allocate the AtmProfile structure
  Error_Status = Allocate_AtmProfile( n_Layers, &
                                      n_Absorbers, &
                                      n_Profiles, &
                                      AtmProfile )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME,'Error allocating AtmProfile.',FAILURE ); STOP
  END IF


  ! Process profile by profile
  ! --------------------------
  WRITE( *, * )
  Profile_Loop: DO m = 1, n_Profiles
    WRITE( *,'(5x,"Processing ",a," profile #",i0)' ) TRIM(ATMPROFILE_SET_ID_TAG( Profile_Set )), m


    ! Load the profile data
    ! ---------------------
    Load_Profile_Set: SELECT CASE ( TRIM(ATMPROFILE_SET_ID_TAG( Profile_Set )) )
      CASE ( 'UMBC48' )
        Error_Status = Load_UMBC_Profile( m, &
                                          Pressure, &
                                          Temperature, &
                                          Absorber, &
                                          Absorber_ID       = AtmProfile%Absorber_ID, &
                                          Absorber_Units_ID = AtmProfile%Absorber_Units_ID, &
                                          Description       = AtmProfile%Description(m), &
                                          Climatology_Model = AtmProfile%Climatology_Model(m), &
                                          Year              = AtmProfile%DateTime(m)%Year, &
                                          Month             = AtmProfile%DateTime(m)%Month, &
                                          Day               = AtmProfile%DateTime(m)%Day, &
                                          Hour              = AtmProfile%DateTime(m)%Hour, &
                                          Latitude          = AtmProfile%Location(m)%Latitude, &
                                          Longitude         = AtmProfile%Location(m)%Longitude, &
                                          Surface_Altitude  = AtmProfile%Location(m)%Surface_Altitude, &
                                          RCS_Id = Profile_Set_RCS_Id )
      CASE ( 'CIMSS32' )
        Error_Status = Load_CIMSS_Profile( m, &
                                           Pressure, &
                                           Temperature, &
                                           Absorber, &
                                           Absorber_ID       = AtmProfile%Absorber_ID, &
                                           Absorber_Units_ID = AtmProfile%Absorber_Units_ID, &
                                           Description       = AtmProfile%Description(m), &
                                           Climatology_Model = AtmProfile%Climatology_Model(m), &
                                           Year              = AtmProfile%DateTime(m)%Year, &
                                           Month             = AtmProfile%DateTime(m)%Month, &
                                           Day               = AtmProfile%DateTime(m)%Day, &
                                           Hour              = AtmProfile%DateTime(m)%Hour, &
                                           Latitude          = AtmProfile%Location(m)%Latitude, &
                                           Longitude         = AtmProfile%Location(m)%Longitude, &
                                           Surface_Altitude  = AtmProfile%Location(m)%Surface_Altitude, &
                                           RCS_Id = Profile_Set_RCS_Id )
      CASE ( 'ECMWF52' )
        Error_Status = Load_ECMWF52_Profile( m, &
                                             Pressure, &
                                             Temperature, &
                                             Absorber, &
                                             Absorber_ID       = AtmProfile%Absorber_ID, &
                                             Absorber_Units_ID = AtmProfile%Absorber_Units_ID, &
                                             Description       = AtmProfile%Description(m), &
                                             Climatology_Model = AtmProfile%Climatology_Model(m), &
                                             Year              = AtmProfile%DateTime(m)%Year, &
                                             Month             = AtmProfile%DateTime(m)%Month, &
                                             Day               = AtmProfile%DateTime(m)%Day, &
                                             Hour              = AtmProfile%DateTime(m)%Hour, &
                                             Latitude          = AtmProfile%Location(m)%Latitude, &
                                             Longitude         = AtmProfile%Location(m)%Longitude, &
                                             Surface_Altitude  = AtmProfile%Location(m)%Surface_Altitude, &
                                             RCS_Id = Profile_Set_RCS_Id )
      CASE ( 'ECMWF83' )
        Error_Status = Load_ECMWF83_Profile( m, &
                                             Pressure, &
                                             Temperature, &
                                             Absorber, &
                                             Absorber_ID       = AtmProfile%Absorber_ID, &
                                             Absorber_Units_ID = AtmProfile%Absorber_Units_ID, &
                                             Description       = AtmProfile%Description(m), &
                                             Climatology_Model = AtmProfile%Climatology_Model(m), &
                                             Year              = AtmProfile%DateTime(m)%Year, &
                                             Month             = AtmProfile%DateTime(m)%Month, &
                                             Day               = AtmProfile%DateTime(m)%Day, &
                                             Hour              = AtmProfile%DateTime(m)%Hour, &
                                             Latitude          = AtmProfile%Location(m)%Latitude, &
                                             Longitude         = AtmProfile%Location(m)%Longitude, &
                                             Surface_Altitude  = AtmProfile%Location(m)%Surface_Altitude, &
                                             RCS_Id = Profile_Set_RCS_Id )
      CASE ( 'Model6' )
        Error_Status = Load_Model_Profile( m, &
                                           Pressure, &
                                           Temperature, &
                                           Absorber, &
                                           Absorber_ID       = AtmProfile%Absorber_ID, &
                                           Absorber_Units_ID = AtmProfile%Absorber_Units_ID, &
                                           Description       = AtmProfile%Description(m), &
                                           Climatology_Model = AtmProfile%Climatology_Model(m), &
                                           Year              = AtmProfile%DateTime(m)%Year, &
                                           Month             = AtmProfile%DateTime(m)%Month, &
                                           Day               = AtmProfile%DateTime(m)%Day, &
                                           Hour              = AtmProfile%DateTime(m)%Hour, &
                                           Latitude          = AtmProfile%Location(m)%Latitude, &
                                           Longitude         = AtmProfile%Location(m)%Longitude, &
                                           Surface_Altitude  = AtmProfile%Location(m)%Surface_Altitude, &
                                           RCS_Id = Profile_Set_RCS_Id )
      CASE DEFAULT
        CALL Display_Message( PROGRAM_NAME,'Invalid option.',FAILURE )
        STOP
    END SELECT Load_Profile_Set

    ! Check result
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message,'("Error loading ",a," profile # ",i0,".")' ) &
                      TRIM(ATMPROFILE_SET_ID_TAG( Profile_Set )), m
      CALL Display_Message( PROGRAM_NAME,TRIM(Message),FAILURE ); STOP
    END IF

    ! Fill the pressure array with the required pressure levels/layers
    AtmProfile%Level_Pressure(:,m) = ATMPROFILE_LEVEL_PRESSURE
    AtmProfile%Layer_Pressure(:,m) = ( ATMPROFILE_LEVEL_PRESSURE(1:n_Levels-1) - &
                                       ATMPROFILE_LEVEL_PRESSURE(2:n_Levels)     ) / &
    !                                 -------------------------------------------------
                                      LOG( ATMPROFILE_LEVEL_PRESSURE(1:n_Levels-1) / &
                                           ATMPROFILE_LEVEL_PRESSURE(2:n_Levels)     )


    ! Interpolate the profile as necessary
    ! ------------------------------------
    Perform_Interpolation: IF ( Profile_Interpolate ) THEN
      WRITE( *,'(10x,"Interpolating...")' )
      
      ! Set the AtmProfile file COMMENT attribute
      Comment = 'Original profiles interpolated to the 101 AIRS pressure levels.'

      ! Interpolate the temperature
      Error_Status = Polynomial_Interpolate( LOG(Pressure),                       &  ! X
                                             Temperature,                         &  ! Y
                                             LOG(AtmProfile%Level_Pressure(:,m)), &  ! XINT
                                             AtmProfile%Level_Temperature(:,m),   &  ! YINT
                                             Order  = 1,                          &  ! Linear
                                             RCS_Id = Interpolation_RCS_Id )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message,'("Error interpolating temperature for profile #",i0,".")' ) m
        CALL Display_Message( PROGRAM_NAME,TRIM(Message),FAILURE ); STOP
      END IF

      ! Interpolate the absorber amounts
      Absorber_Interpolation_Loop: DO j = 1, n_Absorbers
        Error_Status = Polynomial_Interpolate( LOG(Pressure),                       &  ! X
                                               Absorber(:,j),                       &  ! Y
                                               LOG(AtmProfile%Level_Pressure(:,m)), &  ! XINT
                                               AtmProfile%Level_Absorber(:,j,m),    &  ! YINT
                                               Order = 1                            )  ! Linear
        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message,'("Error interpolating absorber#",i0," amount, profile #",i0,".")' ) j, m
          CALL Display_Message( PROGRAM_NAME,TRIM(Message),FAILURE ); STOP
        END IF

        ! Check the interpolation result and correct if necessary
        Negative_Absorber: IF ( ANY(AtmProfile%Level_Absorber(:,j,m) < ZERO) ) THEN
          ! Output warning message
          WRITE( Message,'("Correcting interpolated amount for absorber #",i0, &
                          &", profile #",i0," is < 0. Inspect result.")' ) j, m
          CALL Display_Message( PROGRAM_NAME,TRIM(Message),WARNING )
          ! Load data into correction array
          acorr(1:n_Levels) = AtmProfile%Level_Absorber(:,j,m)
          acorr(n_Levels+1) = acorr(n_Levels)
          ! Loop over all levels
          Correction_Check_Loop: DO k = 1, n_Levels
            ! Check for non-physical absorber amount
            Correct_Absorber: IF ( acorr(k) > ZERO ) THEN
              ! No correction, cycle loop
              CYCLE Correction_Check_Loop
            ELSE Correct_Absorber
              ! Output some info
              WRITE( Message,'("Absorber #",i0," amount for level ",i0," (",es9.3,"hPa) : ",es13.6)' ) &
                              j, k, ATMPROFILE_LEVEL_PRESSURE(k), acorr(k)
              CALL Display_Message( PROGRAM_NAME,TRIM(Message),WARNING )
              ! Set invalid value equal to next non-negative value
              Correction_Loop: DO ik = k+1, n_Levels+1
                IF ( acorr( ik ) > ZERO ) THEN
                  acorr(k) = acorr(ik)
                  EXIT Correction_Loop
                END IF
              END DO Correction_Loop
            END IF Correct_Absorber
          END DO Correction_Check_Loop
          ! Restore the corrected profile
          AtmProfile%Level_Absorber(:,j,m) = acorr(1:n_Levels)
        END IF Negative_Absorber
      END DO Absorber_Interpolation_Loop
    ELSE Perform_Interpolation
      ! No interpolation, just copy the data
      ! Set the AtmProfile file COMMENT attribute
      Comment = 'No interpolation performed.'
      ! Copy the temperature and absorber data
      AtmProfile%Level_Temperature(:,m) = Temperature
      AtmProfile%Level_Absorber(:,:,m)  = Absorber
    END IF Perform_Interpolation


    ! Compute geopotential height and thicknesses
    ! -------------------------------------------
    ! Is water vapor present?
    IF ( COUNT(AtmProfile%Absorber_ID == ID_H2O) /= 1 ) THEN
      CALL Display_Message( PROGRAM_NAME,'No water vapor data.',FAILURE ); STOP
    END IF
    ! Find the water vapor index
    j_idx = PACK((/ ( j, j = 1, AtmProfile%n_Absorbers ) /), &
                 AtmProfile%Absorber_ID == ID_H2O)
    ! Convert from mixing ratio
    H2O_Pressure = MR_to_PP( ATMPROFILE_LEVEL_PRESSURE, &
                             AtmProfile%Level_Absorber(:,j_idx(1),m) )
    ! Check the result
    IF ( ANY(H2O_Pressure < ZERO) ) THEN
      CALL Display_Message( PROGRAM_NAME,'Water vapor unit conversion failed.',FAILURE ); STOP
    END IF

    ! Compute the geopotential height profile
    Error_Status = Geopotential_Height( AtmProfile%Level_Pressure(:,m),    &  ! Input
                                        AtmProfile%Level_Temperature(:,m), &  ! Input
                                        H2O_Pressure,                      &  ! Input
                                        AtmProfile%Level_Altitude(:,m),    &  ! Output
                                        Gravity_Correction = 1,                    &  ! Optional Input
                                        Latitude = AtmProfile%Location(m)%Latitude )  ! Optional Input
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME,'Error calculating geopotentials.',Error_Status ); STOP
    END IF

    ! Calculate the thicknesses
    AtmProfile%Layer_Delta_Z(:,m) = ABS(AtmProfile%Level_Altitude(1:n_Levels-1,m) - &
                                        AtmProfile%Level_Altitude(2:n_Levels,  m)   )

    ! Average the temperature
    Temperature_Average_loop: DO k = 1, n_Layers
      AtmProfile%Layer_Temperature(k,m) = ZEROpointFIVE * &
                                          ( AtmProfile%Level_Temperature( k,  m ) + &
                                            AtmProfile%Level_Temperature( k+1,m )   )
    END DO Temperature_Average_loop

    ! Average the absorbers
    j_Absorber_Average_loop: DO j = 1, n_Absorbers
      k_Absorber_Average_loop: DO k = 1, n_Layers
        AtmProfile%Layer_Absorber(k,j,m) = ZEROpointFIVE * &
                                           ( AtmProfile%Level_Absorber( k,  j,m ) + &
                                             AtmProfile%Level_Absorber( k+1,j,m )   )
      END DO k_Absorber_Average_loop
    END DO j_Absorber_Average_loop


    ! ------------------------------------
    ! Deallocate pointers for next profile
    ! ------------------------------------
    ! Pressure
    IF ( ASSOCIATED(Pressure) ) THEN
      DEALLOCATE( Pressure, STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message,'("Error deallocating PRESSURE input array for profile #",i0,". STAT = ",i0)') &
                        m, Allocate_Status
        CALL Display_Message( PROGRAM_NAME,TRIM(Message),WARNING )
      END IF
    END IF
    ! Temperature
    IF ( ASSOCIATED(Temperature) ) THEN
      DEALLOCATE( Temperature, STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message,'("Error deallocating TEMPERATURE input array for profile #",i0,". STAT = ",i0)') &
                        m, Allocate_Status
        CALL Display_Message( PROGRAM_NAME,TRIM(Message),WARNING )
      END IF
    END IF
    ! Absorber
    IF ( ASSOCIATED(Absorber) ) THEN
      DEALLOCATE( Absorber, STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message,'("Error deallocating ABSORBER input array for profile #",i0,". STAT = ",i0)') &
                        m, Allocate_Status
        CALL Display_Message( PROGRAM_NAME,TRIM(Message),WARNING )
      END IF
    END IF

  END DO Profile_Loop

  ! Set dummy lat/lon before write
  WHERE( ABS(AtmProfile%Location%Longitude) > 360.0_fp )
    AtmProfile%Location%Longitude = -999.0_fp
  END WHERE

  WHERE( ABS(AtmProfile%Location%Latitude) > 90.0_fp )
    AtmProfile%Location%Latitude = -999.0_fp
  END WHERE


  ! Write the AtmProfile data file
  ! ------------------------------
  ! Create the netCDF file name
  WRITE( AtmProfile_Filename,'(a,".AtmProfile.nc")' ) &
                              TRIM(ATMPROFILE_SET_ID_TAG( Profile_Set ))
  WRITE( *,'(/5x,"Writing the output file ",a,"...")' ) TRIM(AtmProfile_Filename)

  ! Create the History global attribute
  IF ( Profile_Interpolate ) THEN
    History = PROGRAM_RCS_ID//'; '//&
              TRIM(Interpolation_RCS_Id)// '; '//&
              TRIM(Profile_Set_RCS_Id)
  ELSE
    History = PROGRAM_RCS_ID//'; '//&
              TRIM(Profile_Set_RCS_Id)
  END IF

  ! Write the interpolated data to file
  Error_Status = Write_AtmProfile_netCDF( AtmProfile_Filename, &
                                          AtmProfile, &
                                          Title   = TRIM(ATMPROFILE_SET_ID_TAG( Profile_Set ))//&
                                                    ' atmospheric profile set.', &
                                          History = TRIM(History), &
                                          Comment = TRIM(Comment), &
                                          ID_Tag  = TRIM(ATMPROFILE_SET_ID_TAG( Profile_Set )) )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME,'Error writing '//TRIM(AtmProfile_Filename),FAILURE ); STOP
  END IF


  ! Test read the AtmProfile data file
  ! ----------------------------------
  WRITE( *,'(/5x,"Test reading ",a,"...")' ) TRIM(AtmProfile_Filename)

  ! Destroy the current AtmProfile structure
  Error_Status = Destroy_AtmProfile( AtmProfile )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME,'Error destroying AtmProfile for test read',FAILURE ); STOP
  END IF

  ! Read the data
  Error_Status = Read_AtmProfile_netCDF( AtmProfile_Filename, &
                                         AtmProfile )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME,'Error reading '//TRIM(AtmProfile_Filename),FAILURE ); STOP
  END IF

  ! Print out some data
  m = MIN(23,AtmProfile%n_Profiles)
  WRITE( *,'(/5x,"Description for profile#",i0," read:")' ) m
  WRITE( *,'(7x,a)' ) TRIM(AtmProfile%Description(m))
  WRITE( *,'(5x, "Date/Time for profile#",i0," read:")' ) m
  WRITE( *,'(7x,i2.2,"/",i2.2,"/",i4.4," at ",i2.2,"00UTC")' ) &
           AtmProfile%DateTime(m)%Day, &
           AtmProfile%DateTime(m)%Month, &
           AtmProfile%DateTime(m)%Year, &
           AtmProfile%DateTime(m)%Hour
  WRITE( *,'(5x,"Location for profile#",i0," read:")' ) m
  WRITE( *,'(7x,"Lat: ",f8.3,", Lon: ",f8.3)' ) &
           AtmProfile%Location(m)%Latitude, &
           AtmProfile%Location(m)%Longitude

  ! Cleanup
  Error_Status = Destroy_AtmProfile( AtmProfile )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME,'Error destroying AtmProfile',WARNING )
  END IF

END PROGRAM Create_AtmProfile
