!
! Test_Forward
!
! Program to test the CRTM Forward model code and provide an exmaple of how
! to call the Forward function.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 30-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Test_Forward

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE CRTM_Module, fp=>fp_kind   ! The main CRTM module
  USE CRTM_Atmosphere_Binary_IO  ! Just for reading test datafiles
  USE CRTM_Surface_Binary_IO     ! Just for reading test datafiles
  USE CRTM_Test_Utility          ! For test output
  USE Timing_Utility             ! For timing runs
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'Test_Forward'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: Test_Forward.f90,v 1.15 2006/09/22 20:07:56 wd20pd Exp $'
  CHARACTER(*), PARAMETER :: TEST_ATMDATA_FILENAME = 'ECMWF-Atmosphere.Cloud.Aerosol.bin'
  CHARACTER(*), PARAMETER :: TEST_SFCDATA_FILENAME = 'ECMWF-Surface.bin'
  INTEGER,      PARAMETER :: MAX_TEST_CASES = 52
  CHARACTER(*), PARAMETER :: TEST_OUTPUT_FILENAME = 'CRTM_Test_Forward.output'
  INTEGER,      PARAMETER :: N_OPTIONS_CASES = 2
  INTEGER,      PARAMETER :: NO_OPTIONS  = 1
  INTEGER,      PARAMETER :: FWD_OPTIONS = 2
  INTEGER,      PARAMETER, DIMENSION( N_OPTIONS_CASES ) :: &
    OPTIONS_CASES = (/ NO_OPTIONS,  &
                       FWD_OPTIONS /)


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  INTEGER :: i, m, iOptions
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  CHARACTER(256) :: File_Prefix
  CHARACTER(256) :: SpcCoeff_File
  CHARACTER(256) :: TauCoeff_File
  CHARACTER(256) :: AerosolCoeff_File
  CHARACTER(256) :: CloudCoeff_File
  CHARACTER(256) :: EmisCoeff_File
  TYPE(CRTM_ChannelInfo_type)                               :: ChannelInfo
  TYPE(CRTM_Atmosphere_type),   DIMENSION(MAX_TEST_CASES)   :: Atmosphere
  TYPE(CRTM_Surface_type),      DIMENSION(MAX_TEST_CASES)   :: Surface
  TYPE(CRTM_GeometryInfo_type), DIMENSION(MAX_TEST_CASES)   :: GeometryInfo
  TYPE(CRTM_RTSolution_type),   DIMENSION(:,:), ALLOCATABLE :: RTSolution
  TYPE(CRTM_Options_type),      DIMENSION(MAX_TEST_CASES)   :: Options
  TYPE(Timing_type) :: Timing



  ! ----------------------------------------------------
  ! Read the atmosphere and surface structure data files
  ! ----------------------------------------------------
  WRITE( *, '( /5x, "Reading ECMWF Atmosphere structure file..." )' )
  Error_Status = CRTM_Read_Atmosphere_Binary( TEST_ATMDATA_FILENAME, &
                                              Atmosphere )
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error reading Atmosphere structure file '//&
                           TEST_ATMDATA_FILENAME, & 
                           Error_Status )
   STOP
  END IF

  WRITE( *, '( /5x, "Reading Surface structure file..." )' )
  Error_Status = CRTM_Read_Surface_Binary( TEST_SFCDATA_FILENAME, &
                                           Surface )
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error reading Surface structure file '//&
                           TEST_SFCDATA_FILENAME, & 
                           Error_Status )
   STOP
  END IF


  ! -----------------------------
  ! Get the coefficient filenames
  ! -----------------------------
  ! Enter the instrument file prefix, e.g. hirs3_n16
  WRITE( *, FMT     = '( /5x, "Enter the instrument file prefix : " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) File_Prefix
  File_Prefix = ADJUSTL(File_Prefix)

  ! Create the filenames
  SpcCoeff_File = TRIM(File_Prefix)//'.SpcCoeff.bin'
  TauCoeff_File = TRIM(File_Prefix)//'.TauCoeff.bin'
  AerosolCoeff_File = 'AerosolCoeff.bin'
  CloudCoeff_File   = 'CloudCoeff.bin'
  EmisCoeff_File    = 'EmisCoeff.bin'


  ! -------------------
  ! Initialise the CRTM
  ! -------------------
  WRITE( *, '( /5x, "Initializing the CRTM..." )' )
  Error_Status = CRTM_Init( ChannelInfo, &
                            SpcCoeff_File     = SpcCoeff_File, &
                            TauCoeff_File     = TauCoeff_File, &
                            AerosolCoeff_File = AerosolCoeff_File, &
                            CloudCoeff_File   = CloudCoeff_File, &
                            EmisCoeff_File    = EmisCoeff_File )
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error initializing CRTM', & 
                            Error_Status)  
   STOP
  END IF


  ! ----------------------
  ! Allocate output arrays
  ! ----------------------
  ALLOCATE( RTSolution( ChannelInfo%n_Channels, MAX_TEST_CASES ), &
            STAT = Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error allocating RTSolution structure array', & 
                            Error_Status)  
   STOP
  END IF


  ! --------------------------
  ! Allocate the Options input
  ! --------------------------
  Error_Status = CRTM_Allocate_Options( ChannelInfo%n_Channels, Options )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating Options structure array', & 
                           Error_Status)  
    STOP
  END IF


  ! ------------------
  ! Assign some values
  ! ------------------
  GeometryInfo%Sensor_Zenith_Angle = ZERO   ! Nadir
  DO m = 1, MAX_TEST_CASES
    Options(m)%Emissivity = 0.8_fp
  END DO


  ! ------------------------------
  ! Print some initialisation info
  ! ------------------------------
  CALL Print_ChannelInfo(TRIM(File_Prefix)//'.'//TEST_OUTPUT_FILENAME, ChannelInfo)


  ! ----------------------
  ! Call the Forward model
  ! ----------------------
  DO i = 1, N_OPTIONS_CASES

    ! Set the optional emissivity data switches
    iOptions = OPTIONS_CASES( i )
    SELECT CASE ( iOptions )
      CASE ( NO_OPTIONS )
        Options%Emissivity_Switch = 0
        Message = 'Calling the Forward CRTM...'
      CASE ( FWD_OPTIONS )
        Options%Emissivity_Switch = 1
        Message = 'Calling the Forward CRTM with FWD Options...'
    END SELECT
    WRITE( *, '( /5x, a )' ) TRIM( Message )

    ! Call the CRTM
    CALL Begin_Timing( Timing )
    Error_Status = CRTM_Forward( Atmosphere, &
                                 Surface, &
                                 GeometryInfo, &
                                 ChannelInfo, &
                                 RTSolution, &
                                 Options = Options )
    CALL End_Timing( Timing )
    IF ( Error_Status /= SUCCESS ) THEN 
       CALL Display_Message( PROGRAM_NAME, &
                             'Error in CRTM Forward Model', &
                              Error_Status)  
     STOP
    END IF

    ! Output some results
    CALL Print_Results(FWD_OUTPUT, &
                       TRIM(File_Prefix)//'.'//TEST_OUTPUT_FILENAME, Message, &
                       ChannelInfo, Atmosphere, Surface, RTSolution)
      CALL Print_FWD_Results(TRIM(File_Prefix)//'.dat', &
                             ChannelInfo, RTSolution)
    CALL Display_Timing( Timing )

  END DO


  ! ----------------
  ! Destroy the CRTM
  ! ----------------
  WRITE( *, '( /5x, "Destroying the CRTM..." )' )
  Error_Status = CRTM_Destroy( ChannelInfo )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying CRTM', & 
                           Error_Status )
    STOP
  END IF


  ! --------
  ! Clean up
  ! --------
  Error_Status = CRTM_Destroy_Options(Options)
  DEALLOCATE(RTSolution, STAT = Allocate_Status)
  Error_Status = CRTM_Destroy_Surface(Surface)
  Error_Status = CRTM_Destroy_Atmosphere(Atmosphere)

END PROGRAM Test_Forward
