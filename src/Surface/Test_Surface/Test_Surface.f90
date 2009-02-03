!
! Test_Surface
!
! Program to test the CRTM Surface routines.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 22-Jul-2004
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Test_Surface

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds            , ONLY: fp
  USE Message_Handler       , ONLY: SUCCESS, FAILURE, INFORMATION, &
                                    Display_Message, Program_Message
  USE CRTM_Parameters       , ONLY: SET
  USE CRTM_Surface_Define   , ONLY: CRTM_Surface_type, &
                                    CRTM_Destroy_Surface, &
                                    CRTM_Allocate_Surface, &
                                    CRTM_Assign_Surface, &
                                    CRTM_Equal_Surface, &
                                    CRTM_Sum_Surface, &
                                    CRTM_Zero_Surface
  USE CRTM_Surface_Binary_IO, ONLY: CRTM_Inquire_Surface_Binary, &
                                    CRTM_Write_Surface_Binary, &
                                    CRTM_Read_Surface_Binary
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'Test_Surface'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id$'

  CHARACTER(*), PARAMETER :: TEST_SURFACE_FILENAME = 'Test.Surface.bin'
  CHARACTER(*), PARAMETER :: SURFACE_FILENAME = 'Surface.bin'
  
  INTEGER, PARAMETER :: MAX_N_LOOPS  = 10000
  INTEGER, PARAMETER :: INFO_N_LOOPS = 1000
  ! Structure dimensions
  INTEGER, PARAMETER :: N_PROFILES  = 2
  INTEGER, PARAMETER :: N_CHANNELS  = 20
  
  REAL(fp), PARAMETER :: SCALE_FACTOR = 10.0_fp
  REAL(fp), PARAMETER :: OFFSET       =  0.5_fp


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: m, n
  TYPE(CRTM_Surface_type), DIMENSION(N_PROFILES) :: Surface, Surface_Copy
  TYPE(CRTM_Surface_type), ALLOCATABLE :: Sfc(:)


  ! Output header
  ! -------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test all the CRTM Surface structure '//&
                        'manipulation and I/O functions.', &
                        '$Revision$' )


  ! Allocate the SensorData structure
  ! ---------------------------------
  Error_Status = CRTM_Allocate_Surface( N_CHANNELS, Surface )  ! Output
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating Surface structure array.', &
                          FAILURE )
    STOP
  END IF

  ! Fill structure with pretend data
  ! --------------------------------
  DO m = 1, N_PROFILES

    Surface(m)%Land_Coverage         = 0.25_fp
    Surface(m)%Water_Coverage        = 0.25_fp
    Surface(m)%Snow_Coverage         = 0.25_fp
    Surface(m)%Ice_Coverage          = 0.25_fp
    Surface(m)%Wind_Speed            = 5.0_fp
    Surface(m)%Land_Type             = 2
    Surface(m)%Land_Temperature      = 280.0_fp
    Surface(m)%Soil_Moisture_Content = 0.1_fp
    Surface(m)%Canopy_Water_Content  = 0.2_fp
    Surface(m)%Vegetation_Fraction   = 0.5_fp
    Surface(m)%Soil_Temperature      = 283.0_fp
    Surface(m)%Water_Type            = 1
    Surface(m)%Water_Temperature     = 278.0_fp
    Surface(m)%Wind_Direction        = 90.0_fp
    Surface(m)%Salinity              = 35.0_fp
    Surface(m)%Snow_Type             = 1
    Surface(m)%Snow_Temperature      = 273.0_fp
    Surface(m)%Snow_Depth            = 0.01_fp
    Surface(m)%Snow_Density          = 0.95_fp
    Surface(m)%Snow_Grain_Size       = 13.0_fp
    Surface(m)%Ice_Type              = 1
    Surface(m)%Ice_Temperature       = 269.0_fp
    Surface(m)%Ice_Thickness         = 25.0_fp
    Surface(m)%Ice_Density           = 0.99_fp
    Surface(m)%Ice_Roughness         = 1.4_fp

  END DO


  ! Test the sum routine
  ! --------------------
  WRITE( *, '( /5x, "Testing Sum functions ..." )' )

  ! Copy the structure array
  Error_Status = CRTM_Assign_Surface( Surface, Surface_Copy )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error copying Surface structure array for WeightedSum test', &
                          FAILURE )
    STOP
  END IF

  ! Compute the sum
  Error_Status = CRTM_Sum_Surface( Surface, &
                                   Surface_Copy, &
                                   Scale_Factor=SCALE_FACTOR, &
                                   Offset      =OFFSET )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error computing Surface sum', &
                          FAILURE )
    STOP
  END IF

  ! Print out some results
  DO m = 1, N_PROFILES
    WRITE(*,'( 5x, "Profile #", i0 )' ) m
    WRITE(*,*) Surface(m)%Land_Coverage , &
               Surface(m)%Water_Coverage, &
               Surface(m)%Snow_Coverage , &
               Surface(m)%Ice_Coverage  

    WRITE(*,*) Surface(m)%Wind_Speed
    
    WRITE(*,*) Surface(m)%Land_Type            , &
               Surface(m)%Land_Temperature     , &
               Surface(m)%Soil_Moisture_Content, &
               Surface(m)%Canopy_Water_Content , &
               Surface(m)%Vegetation_Fraction  , &
               Surface(m)%Soil_Temperature     
    
    WRITE(*,*) Surface(m)%Water_Temperature, &
               Surface(m)%Wind_Direction   , &
               Surface(m)%Salinity         
    
    WRITE(*,*) Surface(m)%Snow_Type       , &
               Surface(m)%Snow_Temperature, &
               Surface(m)%Snow_Depth      , &
               Surface(m)%Snow_Density    , &
               Surface(m)%Snow_Grain_Size 
               
    WRITE(*,*) Surface(m)%Ice_Type       , &
               Surface(m)%Ice_Temperature, &
               Surface(m)%Ice_Thickness  , &
               Surface(m)%Ice_Density    , &
               Surface(m)%Ice_Roughness  
  END DO
  
  
  ! Test the zero routine
  ! ---------------------
  WRITE( *, '( /5x, "Testing Zero subroutines ..." )' )

  ! Zero the structure array
  CALL CRTM_Zero_Surface( Surface_Copy )

  ! Print out some results
  DO m = 1, N_PROFILES
    WRITE(*,'( 5x, "Profile #", i0 )' ) m
    WRITE(*,*) Surface_Copy(m)%Land_Coverage , &
               Surface_Copy(m)%Water_Coverage, &
               Surface_Copy(m)%Snow_Coverage , &
               Surface_Copy(m)%Ice_Coverage  

    WRITE(*,*) Surface_Copy(m)%Wind_Speed
    
    WRITE(*,*) Surface_Copy(m)%Land_Type            , &
               Surface_Copy(m)%Land_Temperature     , &
               Surface_Copy(m)%Soil_Moisture_Content, &
               Surface_Copy(m)%Canopy_Water_Content , &
               Surface_Copy(m)%Vegetation_Fraction  , &
               Surface_Copy(m)%Soil_Temperature     
    
    WRITE(*,*) Surface_Copy(m)%Water_Temperature, &
               Surface_Copy(m)%Wind_Direction   , &
               Surface_Copy(m)%Salinity         
    
    WRITE(*,*) Surface_Copy(m)%Snow_Type       , &
               Surface_Copy(m)%Snow_Temperature, &
               Surface_Copy(m)%Snow_Depth      , &
               Surface_Copy(m)%Snow_Density    , &
               Surface_Copy(m)%Snow_Grain_Size 
               
    WRITE(*,*) Surface_Copy(m)%Ice_Type       , &
               Surface_Copy(m)%Ice_Temperature, &
               Surface_Copy(m)%Ice_Thickness  , &
               Surface_Copy(m)%Ice_Density    , &
               Surface_Copy(m)%Ice_Roughness  
  END DO


  ! Test the Surface I/O functions
  ! ---------------------------------
  WRITE(*,'( /5x, "Testing Surface I/O functions ..." )' )

  ! Write the test datafile
  WRITE(*,'( 10x, "Writing test Surface datafile ..." )' )
  Error_Status = CRTM_Write_Surface_Binary( TEST_SURFACE_FILENAME, &
                                            Surface )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing Surface data file.', &
                          FAILURE )
    STOP
  END IF

  ! Test read the datafile
  WRITE(*,'( 10x, "Looping for Surface Binary read memory leak test ..." )' )
  DO n = 1, MAX_N_LOOPS
    Error_Status = CRTM_Read_Surface_Binary( TEST_SURFACE_FILENAME, &
                                             Surface, &
                                             Quiet = SET )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error reading Surface datafile on attempt # ", i0 )' ) n
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
  WRITE(*,'( /5x, "Looping for Surface structure copy memory leak test ..." )' )
  DO n = 1, MAX_N_LOOPS
    Error_Status = CRTM_Assign_Surface( Surface, Surface_Copy )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error copying Surface structure array on attempt # ", i0 )' ) n
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


  ! Read the file to be used in CRTM testing
  ! ----------------------------------------
  WRITE(*,'( /5x, "Reading the CRTM test Surface file..." )' )

  ! Inquire the datafile
  Error_Status = CRTM_Inquire_Surface_Binary( SURFACE_FILENAME, &
                                              n_Profiles = m )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error inquiring CRTM test Surface datafile', &
                          FAILURE )
    STOP
  END IF

  ! Allocate the array
  ALLOCATE( Sfc(m), STAT=Allocate_Status)
  IF ( Allocate_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating Sfc array for CRTM test Surface datafile I/O', &
                          FAILURE )
    STOP
  END IF
  
  ! Read the datafile
  Error_Status = CRTM_Read_Surface_Binary( SURFACE_FILENAME, &
                                           Sfc )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading CRTM test Surface datafile', &
                          FAILURE )
    STOP
  END IF
  
  ! Write the datafile
  Error_Status = CRTM_Write_Surface_Binary( SURFACE_FILENAME//'.copy', &
                                            Sfc )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing CRTM test Surface datafile.', &
                          FAILURE )
    STOP
  END IF

  ! Deallocate
  DEALLOCATE(Sfc)
  

  ! Destroy the structure arrays
  ! ----------------------------
  Error_Status = CRTM_Destroy_Surface( Surface )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying Surface structure array.', &
                          Error_Status )
    STOP
  END IF
  Error_Status = CRTM_Destroy_Surface( Surface_Copy )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying Surface_Copy structure array.', &
                          Error_Status )
    STOP
  END IF

END PROGRAM Test_Surface
