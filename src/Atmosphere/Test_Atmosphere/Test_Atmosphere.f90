!
! Test_Atmosphere
!
! Program to test the CRTM Atmosphere structure manipulation and
! I/O functions
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 23-Mar-2005
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Test_Atmosphere

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds               , ONLY: fp
  USE Message_Handler          , ONLY: SUCCESS, FAILURE, INFORMATION, &
                                       Display_Message, PRogram_MEssage
  USE File_Utility
  USE CRTM_Parameters          , ONLY: SET
  USE CRTM_Atmosphere_Define
  USE CRTM_Atmosphere_Binary_IO
  USE CRTM_Cloud_Binary_IO
  USE CRTM_Aerosol_Binary_IO
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*),  PARAMETER :: PROGRAM_NAME   = 'Test_Atmosphere'
  CHARACTER(*),  PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: Test_Atmosphere.f90,v 1.4 2006/05/02 14:58:34 dgroff Exp $'

  CHARACTER(*), PARAMETER ::    AEROSOL_FILENAME = 'Test.Aerosol.bin'
  CHARACTER(*), PARAMETER ::      CLOUD_FILENAME = 'Test.Cloud.bin'
  CHARACTER(*), PARAMETER :: ATMOSPHERE_FILENAME = 'Test.Atmosphere.bin'

  INTEGER, PARAMETER :: MAX_N_LOOPS  = 100000
  INTEGER, PARAMETER :: INFO_N_LOOPS = 5000
  ! Structure dimensions
  INTEGER, PARAMETER :: N_PROFILES  = 6
  INTEGER, PARAMETER :: N_LAYERS    = 100
  INTEGER, PARAMETER :: N_ABSORBERS = 3
  INTEGER, PARAMETER :: N_CLOUDS    = 6
  INTEGER, PARAMETER :: N_AEROSOLS  = 7
  ! Weights
  REAL(fp), PARAMETER :: W1 = 10.0_fp
  REAL(fp), PARAMETER :: W2 =  0.5_fp


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  INTEGER :: Error_Status
  INTEGER :: j, m, mc, ma, n
  TYPE(CRTM_Cloud_type),      DIMENSION(N_CLOUDS)   :: Cloud
  TYPE(CRTM_Aerosol_type),    DIMENSION(N_AEROSOLS) :: Aerosol
  TYPE(CRTM_Atmosphere_type), DIMENSION(N_PROFILES) :: Atmosphere, &
                                                       Atmosphere_Copy
                                                           
  ! Output header
  ! -------------
  CALL PRogram_Message( PROGRAM_NAME, &
                        'Program to test all the CRTM Atmosphere structure '//&
                        'manipulation and I/O functions.', &
                        '$Revision: 1.4 $' )


  ! Allocate the Atmosphere structure
  ! ---------------------------------
  Error_Status = CRTM_Allocate_Atmosphere( N_LAYERS,     &  ! Input
                                           N_ABSORBERS,  &  ! Input
                                           N_CLOUDS,     &  ! Input
                                           N_AEROSOLS,   &  ! Input
                                           Atmosphere    )  ! Output
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating Atmosphere structure array.', &
                          FAILURE )
    STOP
  END IF


  ! Fill structure with pretend data
  ! --------------------------------
  DO m = 1, N_PROFILES
    Atmosphere(m)%Climatology    = m
    Atmosphere(m)%Absorber_ID    = (/ (j, j = 1, N_ABSORBERS ) /)
    Atmosphere(m)%Absorber_Units = SPECIFIC_AMOUNT_UNITS
    Atmosphere(m)%Level_Pressure = 1.0_fp
    Atmosphere(m)%Pressure       = 3.0_fp
    Atmosphere(m)%Temperature    = 4.0_fp
    Atmosphere(m)%Absorber       = 5.0_fp

    DO mc = 1, N_CLOUDS
      Atmosphere(m)%Cloud(mc)%Type = mc
      Atmosphere(m)%Cloud(mc)%Effective_Radius   = 6.0_fp
      Atmosphere(m)%Cloud(mc)%Effective_Variance = 7.0_fp
      Atmosphere(m)%Cloud(mc)%Water_Content      = 8.0_fp
    END DO
  
    DO ma = 1, N_AEROSOLS
      Atmosphere(m)%Aerosol(ma)%Type = ma
      Atmosphere(m)%Aerosol(ma)%Effective_Radius   =  9.0_fp
      Atmosphere(m)%Aerosol(ma)%Concentration      = 11.0_fp
    END DO

  END DO


  ! Test the weighted sum routine
  ! -----------------------------
  WRITE( *, '( /5x, "Testing WeightedSum functions ..." )' )

  ! Copy the structure array
  Error_Status = CRTM_Assign_Atmosphere( Atmosphere, Atmosphere_Copy )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error copying Atmophere structure array for WeightedSum test', &
                          FAILURE )
    STOP
  END IF

  ! Compute the weighted sum
  Error_Status = CRTM_WeightedSum_Atmosphere( Atmosphere, &
                                              Atmosphere_Copy, &
                                              W1, &
                                              w2 = W2 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error computing Atmosphere weighted sum', &
                          FAILURE )
    STOP
  END IF

  ! Print out some results
  DO m = 1, N_PROFILES
    WRITE(*,'( 5x, "Profile #", i0 )' ) m
    WRITE(*,*) Atmosphere(m)%Level_Pressure(1), &
               Atmosphere(m)%Pressure(1), &
               Atmosphere(m)%Temperature(1), &
               Atmosphere(m)%Absorber(1,1)

    DO mc = 1, N_CLOUDS
      WRITE(*,'( 10x, "Cloud #", i0 )' ) mc
      WRITE(*,*) Atmosphere(m)%Cloud(mc)%Effective_Radius(1), &
                 Atmosphere(m)%Cloud(mc)%Effective_Variance(1), &
                 Atmosphere(m)%Cloud(mc)%Water_Content(1)
    END DO
  
    DO ma = 1, N_AEROSOLS
      WRITE(*,'( 10x, "Aerosol #", i0 )' ) ma
      WRITE(*,*) Atmosphere(m)%Aerosol(ma)%Effective_Radius(1), &
                 Atmosphere(m)%Aerosol(ma)%Concentration(1)
    END DO
  END DO
  

  ! Test the zero routine
  ! ---------------------
  WRITE( *, '( /5x, "Testing Zero subroutines ..." )' )

  ! Zero the structure array
  CALL CRTM_Zero_Atmosphere( Atmosphere_Copy )

  ! Print out some results
  DO m = 1, N_PROFILES
    WRITE(*,'( 5x, "Profile #", i0 )' ) m
    WRITE(*,*) Atmosphere_Copy(m)%Level_Pressure(1), &
               Atmosphere_Copy(m)%Pressure(1), &
               Atmosphere_Copy(m)%Temperature(1), &
               Atmosphere_Copy(m)%Absorber(1,1)

    DO mc = 1, N_CLOUDS
      WRITE(*,'( 10x, "Cloud #", i0 )' ) mc
      WRITE(*,*) Atmosphere_Copy(m)%Cloud(mc)%Effective_Radius(1), &
                 Atmosphere_Copy(m)%Cloud(mc)%Effective_Variance(1), &
                 Atmosphere_Copy(m)%Cloud(mc)%Water_Content(1)
    END DO
  
    DO ma = 1, N_AEROSOLS
      WRITE(*,'( 10x, "Aerosol #", i0 )' ) ma
      WRITE(*,*) Atmosphere_Copy(m)%Aerosol(ma)%Effective_Radius(1), &
                 Atmosphere_Copy(m)%Aerosol(ma)%Concentration(1)
    END DO
  END DO
  

  ! Test the cloud I/O functions
  ! ----------------------------
  WRITE(*,'( /5x, "Testing Cloud I/O functions ..." )' )

  ! Write the test datafile
  WRITE(*,'( 10x, "Writing test Cloud datafile ..." )' )
  Error_Status = CRTM_Write_Cloud_Binary( CLOUD_FILENAME, &
                                          Atmosphere(1)%Cloud )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing Cloud data file.', &
                          FAILURE )
    STOP
  END IF

  ! Test read the datafile
  WRITE( *, '( 10x, "Looping for Cloud Binary read memory leak test ..." )' )
  DO n = 1, MAX_N_LOOPS
    Error_Status = CRTM_Read_Cloud_Binary( CLOUD_FILENAME, &
                                           Cloud )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error reading Cloud datafile on attempt # ", i0 )' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF
    IF ( MOD( n, INFO_N_LOOPS ) == 0 ) THEN
      WRITE( Message, '( "Completed loop #", i0, " of ", i0 )' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            INFORMATION )
    END IF
  END DO


  ! Test the aerosol I/O routines
  ! -----------------------------
  WRITE(*,'( /5x, "Testing Aerosol I/O functions ..." )' )

  ! Write the test datafile
  WRITE(*,'( 10x, "Writing test Aerosol datafile ..." )' )
  Error_Status = CRTM_Write_Aerosol_Binary( AEROSOL_FILENAME, &
                                            Atmosphere(1)%Aerosol )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing Aerosol data file.', &
                          FAILURE )
    STOP
  END IF

  ! Test read the datafile
  WRITE(*,'( 10x, "Looping for Aerosol Binary read memory leak test ..." )' )
  DO n = 1, MAX_N_LOOPS
    Error_Status = CRTM_Read_Aerosol_Binary( AEROSOL_FILENAME, &
                                             Aerosol )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error reading Aerosol datafile on attempt # ", i0 )' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF
    IF ( MOD( n, INFO_N_LOOPS ) == 0 ) THEN
      WRITE( Message, '( "Completed loop #", i0, " of ", i0 )' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            INFORMATION )
    END IF
  END DO


  ! Test the Atmosphere I/O functions
  ! ---------------------------------
  WRITE(*,'( /5x, "Testing Atmosphere I/O functions ..." )' )

  ! Write the test datafile
  WRITE(*,'( 10x, "Writing test Atmosphere datafile ..." )' )
  Error_Status = CRTM_Write_Atmosphere_Binary( ATMOSPHERE_FILENAME, &
                                               Atmosphere )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing Atmosphere data file.', &
                          FAILURE )
    STOP
  END IF

  ! Test read the datafile
  WRITE(*,'( 10x, "Looping for Atmosphere Binary read memory leak test ..." )' )
  DO n = 1, MAX_N_LOOPS
    Error_Status = CRTM_Read_Atmosphere_Binary( ATMOSPHERE_FILENAME, &
                                                Atmosphere, &
                                                Quiet = SET )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error reading Atmophere datafile on attempt # ", i0 )' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF
    IF ( MOD( n, INFO_N_LOOPS ) == 0 ) THEN
      WRITE( Message, '( "Completed loop #", i0, " of ", i0 )' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            INFORMATION )
    END IF
  END DO


  ! Loop for assign leak test
  ! -------------------------
  WRITE(*,'( /5x, "Looping for Atmosphere structure copy memory leak test ..." )' )
  DO n = 1, MAX_N_LOOPS
    Error_Status = CRTM_Assign_Atmosphere( Atmosphere, Atmosphere_Copy )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error copying Atmophere structure array on attempt # ", i0 )' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF
    IF ( MOD( n, INFO_N_LOOPS ) == 0 ) THEN
      WRITE( Message, '( "Completed loop #", i0, " of ", i0 )' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            INFORMATION )
    END IF
  END DO


  ! Destroy the structure arrays
  ! ----------------------------
  Error_Status = CRTM_Destroy_Atmosphere( Atmosphere )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying Atmosphere structure array.', &
                          Error_Status )
    STOP
  END IF
  Error_Status = CRTM_Destroy_Atmosphere( Atmosphere_Copy )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying Atmosphere_Copy structure array.', &
                          Error_Status )
    STOP
  END IF

  Error_Status = CRTM_Destroy_Cloud( Cloud )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying Cloud structure array.', &
                          Error_Status )
    STOP
  END IF

  Error_Status = CRTM_Destroy_Aerosol( Aerosol )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying Aerosol structure array.', &
                          Error_Status )
    STOP
  END IF

END PROGRAM Test_Atmosphere
