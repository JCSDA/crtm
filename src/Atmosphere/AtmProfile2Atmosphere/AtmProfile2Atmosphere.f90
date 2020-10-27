!
! AtmProfile2Atmosphere
!
! Program to convert netCDF AtmProfile datafiles into Binary
! Atmosphere datafiles.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 02-Jul-2004
!                       paul.vandelst@noaa.gov
!

PROGRAM AtmProfile2Atmosphere

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds               , ONLY: fp
  USE Message_Handler          , ONLY: SUCCESS, FAILURE, WARNING, &
                                       Display_Message, Program_Message
  USE AtmProfile_Define        , ONLY: AtmProfile_type, &
                                       
  USE AtmProfile_netCDF_IO
  USE CRTM_Atmosphere_Define
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*),  PARAMETER :: PROGRAM_NAME   = 'AtmProfile2Atmosphere'
  CHARACTER(*),  PARAMETER :: PROGRAM_VERSION_ID = &

  INTEGER, PARAMETER :: N_CLOUDS   = 0  ! No cloud data
  INTEGER, PARAMETER :: N_AEROSOLS = 0  ! No aerosol data


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: AtmProfile_Filename
  CHARACTER(256) :: Atmosphere_Filename
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: m, n_profiles
  TYPE(AtmProfile_type)     , ALLOCATABLE :: AtmProfile(:)
  TYPE(CRTM_Atmosphere_type), ALLOCATABLE :: Atmosphere(:)


  ! Output header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to convert netCDF AtmProfile datafiles into '//&
                        'Binary Atmosphere datafiles.', &
                        '$Revision$' )


  ! Read the AtmProfile file
  WRITE(*,FMT='(/5x,"Enter an AtmProfile Filename: ")', ADVANCE='NO')
  READ(*,FMT='(a)') AtmProfile_Filename
  AtmProfile_Filename = ADJUSTL( AtmProfile_Filename )
  ! ...Get the number of profiles
  Error_Status = AtmProfile_netCDF_InquireFile( AtmProfile_Filename, &
                                                n_Profiles = n_profiles )
  IF ( Error_Status /= SUCCESS ) THEN
    message = 'Error inquiring AtmProfile file '//TRIM(AtmProfile_Filename)
    CALL Display_Message( PROGRAM_NAME, message, FAILURE ); STOP
  END IF
  ! ...Allocate the AtmProfile structure array
  ALLOCATE( AtmProfile( n_profiles ), &
            STAT = Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    message = 'Error allocating the AtmProfile structure array.'
    CALL Display_Message( PROGRAM_NAME, message, FAILURE ); STOP
  END IF
  ! ...Read the data
  Error_Status = AtmProfile_netCDF_ReadFile( AtmProfile_Filename, &
                                             AtmProfile, &
                                             Reverse = .TRUE. )
  IF ( Error_Status /= SUCCESS ) THEN
    message = 'Error reading AtmProfile file '//TRIM(AtmProfile_Filename)
    CALL Display_Message( PROGRAM_NAME, message, FAILURE ); STOP
  END IF


  ! Allocate the Atmosphere structure array
  ALLOCATE( Atmosphere( n_profiles ), &
            STAT = Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    message = 'Error allocating the Atmosphere structure array.'
    CALL Display_Message( PROGRAM_NAME, message, FAILURE ); STOP
  END IF


  ! Loop over profiles
  Profile_Loop: DO m = 1, n_profiles

    ! Allocate the current profile Atmosphere structure
    CALL CRTM_Atmosphere_Create( Atmosphere(m), &
                                 AtmProfile(m)%n_Layers, &
                                 AtmProfile(m)%n_Absorbers, &
                                 N_CLOUDS, &
                                 N_AEROSOLS )
    IF ( .NOT. CRTM_Atmosphere_Associated(Atmosphere(m)) ) THEN
      WRITE( message,'("Error allocating Atmosphere structure for profile #",i0)') m
      CALL Display_Message( PROGRAM_NAME, message, FAILURE ); STOP
    END IF

    ! Copy over the data
    Atmosphere(m)%Climatology    = AtmProfile(m)%Climatology_Model
    Atmosphere(m)%Absorber_ID    = AtmProfile(m)%Absorber_ID
    Atmosphere(m)%Absorber_Units = AtmProfile(m)%Absorber_Units_ID
    Atmosphere(m)%Level_Pressure = AtmProfile(m)%Level_Pressure
    Atmosphere(m)%Pressure       = AtmProfile(m)%Layer_Pressure
    Atmosphere(m)%Temperature    = AtmProfile(m)%Layer_Temperature
    Atmosphere(m)%Absorber       = AtmProfile(m)%Layer_Absorber

  END DO Profile_Loop


  ! Write the Atmosphere binary file
  WRITE(*,FMT='(/5x,"Enter an output Atmosphere Filename: ")', ADVANCE='NO')
  READ(*,FMT='(a)') Atmosphere_Filename
  Atmosphere_Filename = ADJUSTL( Atmosphere_Filename )
  WRITE(*,'(/5x,"Writing the Atmosphere data file....")')
  Error_Status = CRTM_Atmosphere_WriteFile( Atmosphere_Filename, &
                                            Atmosphere )
  IF ( Error_Status /= SUCCESS ) THEN
    message = 'Error writing Atmosphere file '//TRIM(Atmosphere_Filename)
    CALL Display_Message( PROGRAM_NAME, message, FAILURE ); STOP
  END IF


  ! Clean up
  CALL AtmProfile_Destroy( AtmProfile )
  CALL CRTM_Atmosphere_Destroy( Atmosphere )
  DEALLOCATE( AtmProfile, Atmosphere, STAT = Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    message = 'Error deallocating structure arrays.'
    CALL Display_Message( PROGRAM_NAME, message, WARNING )
  END IF

END PROGRAM AtmProfile2Atmosphere
