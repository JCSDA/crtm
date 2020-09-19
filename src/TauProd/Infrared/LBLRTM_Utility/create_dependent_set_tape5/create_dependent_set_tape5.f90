PROGRAM create_dependent_set_tape5

  USE type_kinds
  USE error_handler
  USE lblrtm_utility
  USE dependent_profile_set
  USE meteorology

  IMPLICIT NONE


  CHARACTER( LEN = 24 ) :: database_file = 'dependent_profile_set.nc'

  INTEGER( Long ) :: i
  INTEGER( Long ) :: error_status
  INTEGER( Long ) :: n_profiles
  INTEGER( Long ) :: n_levels
  INTEGER( Long ) :: n_absorbers
  INTEGER( Long ) :: profile
  INTEGER( Long ) :: absorber

  REAL( Single )  :: surface_altitude


! -- Profile data arrays
  REAL( Double ),       ALLOCATABLE, DIMENSION( : )       :: pressure
  REAL( Double ),       ALLOCATABLE, DIMENSION( :, : )    :: temperature
  REAL( Double ),       ALLOCATABLE, DIMENSION( :, :, : ) :: absorber_amount

  REAL( Single ),       ALLOCATABLE, DIMENSION( : )       :: profile_t
  REAL( Single ),       ALLOCATABLE, DIMENSION( : )       :: profile_mr
  REAL( Single ),       ALLOCATABLE, DIMENSION( : )       :: altitude
  REAL( Single ),       ALLOCATABLE, DIMENSION( :, : )    :: profile_a

  INTEGER( Long ),      ALLOCATABLE, DIMENSION( : )       :: absorber_id
  INTEGER( Long ),      ALLOCATABLE, DIMENSION( : )       :: profile_number

  CHARACTER( LEN = 1 ), ALLOCATABLE, DIMENSION( : )       :: absorber_units


! -- Spectral band parameters for LBLRTM runs
  REAL( Single ), PARAMETER :: v_begin   = 1000.0
  REAL( Single ), PARAMETER :: v_end     = 2000.0


  INTEGER( Long ) :: profile_begin
  INTEGER( Long ) :: profile_end
  INTEGER( Long ) :: tmp

  REAL( Single ) :: v1
  REAL( Single ) :: v2
  REAL( Single ) :: za

  CHARACTER( LEN = 78 ) :: tape5_filename
  CHARACTER( LEN = 78 ) :: tape5_header


! -- RCS Id

  CHARACTER( LEN = 128 ), PARAMETER :: rcs_Id = &



!-------------------------------------------------------------------------------
!                         -- Create the TAPE5 files! --
!-------------------------------------------------------------------------------

! --------------------
! Inquire the database
! --------------------

  error_status = inquire_profile_set( database_file, &
                                      n_profiles  = n_profiles, &
                                      n_absorbers = n_absorbers, &
                                      n_levels    = n_levels )

  IF ( error_status /= 1 ) THEN
    WRITE( *, '( /5x, "Error inquiring file." )' )
    STOP
  END IF

! -- Output results
  WRITE( *, '( /5x, "n_profiles           : ", i6, &
              &/5x, "n_absorbers          : ", i6, &
              &/5x, "n_levels             : ", i6 )' ) &
            n_profiles, &
            n_absorbers, &
            n_levels


! ---------------
! Allocate arrays
! ---------------

  ALLOCATE( pressure( n_levels ), &
            temperature( n_levels, n_profiles ), &
            profile_t( n_levels ), &
            absorber_amount( n_levels, n_absorbers, n_profiles), &
            profile_a( n_levels, n_absorbers ), &
            profile_mr( n_levels ), &
            altitude( n_levels ), &
            absorber_id( n_absorbers ), &
            absorber_units( n_absorbers ), &
            profile_number( n_profiles ) )


! -------------
! Read the data
! -------------

  error_status = read_profile_set( database_file, &
                                   pressure        = pressure, &
                                   temperature     = temperature, &
                                   absorber_amount = absorber_amount, &
                                   absorber_id     = absorber_id, &
                                   profile_number  = profile_number )

  IF ( error_status /= 1 ) THEN
    WRITE( *, '( /5x, "Error reading file." )' )
    STOP
  END IF


! --------------------------------
! Assign the absorber units (ppmv)
! --------------------------------

  DO i = 1, n_absorbers
    absorber_units( i ) = 'A'
  END DO


! ----------------------------------------------------
! Determine the number of profiles to begin and end at
! ----------------------------------------------------

  WRITE( *, '( /5x, "Enter profile # limits [0,",i2,"] : " )', &
            ADVANCE = 'NO' ) n_profiles - 1
  READ( *, * ) profile_begin, profile_end

  profile_begin = MAX( 0,              profile_begin )
  profile_end   = MIN( n_profiles - 1, profile_end   )

  IF ( profile_begin > profile_end ) THEN
    tmp = profile_begin
    profile_begin = profile_end
    profile_end   = tmp
  END IF


! --------------------------------
! Assign zenith angle - in DEGREES
! --------------------------------

  za = 0.0


! ------------------
! Loop over profiles
! ------------------

  profile_loop: DO profile = profile_begin, profile_end


!   -- Separate out the current profiles
    profile_t = REAL( temperature( :, profile + 1 ), Single )
    profile_a = REAL( absorber_amount( :, :, profile + 1 ), Single )

!   -- Convert h2o ppmv to g/kg
    profile_mr = 1.0e-03 * profile_a( :, 1 ) * 18.0153 / 28.9658


!   -- Calculate the geopotential altitudes
    surface_altitude = 0.0
    altitude = geopotential_height( REAL( pressure, Single ), &
                                    profile_t,  &
                                    profile_mr,  &
                                    surface_altitude )

!   -- Create a TAPE5 file directory and name
    WRITE( tape5_filename, '( "tape5.profile", i2.2, ".rdk" )' ) &
                           profile

!   -- Create a header string
    WRITE( tape5_header, '( "X" )' )

!   -- Create the TAPE5 file
    error_status = lblrtm_create_tape5( REAL( pressure, Single ), &
                                        profile_t, &
                                        profile_a, &
                                        absorber_units, &
                                        absorber_id, &
                                        altitude, &
                                        surface_altitude, &
                                        v_begin, v_end, &
                                        zenith_angle = za, &
                                        calculation_levels = altitude, &
                                        continuum_option = 0, &
                                        merge_option = 3, &
                                        filename = './tape5_files/' // &
                                                   TRIM( tape5_filename ), &
                                        header   = TRIM( tape5_header ) )

    IF ( error_status /= success_state ) THEN
      PRINT *,'Error creating ', tape5_filename
      EXIT profile_loop
    END IF
 
  END DO profile_loop


END PROGRAM create_dependent_set_tape5
