!
! test_TL_convergence
!
! Program to provide a (relatively) simple example of how
! to test the CRTM tangent-linear function.
! This code checks the convergence between the tangent-linear
! operator and the nonlinear CRTM Forward function when 
! the magnitude of the atmospheric input state perturbation
! is step-wise reduced.
! The convergence should be monotonous, which it isn't right.
! For this reason the test in its current state will fai. 
!
! Copyright Patrick Stegmann, 2020
!

PROGRAM test_TL_convergence

  ! ============================================================================
  ! **** ENVIRONMENT SETUP FOR RTM USAGE ****
  !
  ! Module usage
  USE CRTM_Module
  !USE UnitTest_Define, ONLY: UnitTest_IsEqualWithin
  ! Disable all implicit typing
  IMPLICIT NONE
  ! ============================================================================


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'test_TL_convergence'
  CHARACTER(*), PARAMETER :: RESULTS_PATH = './results/unit/'
  CHARACTER(*), PARAMETER :: COEFFICIENTS_PATH = './testinput/'

  ! ============================================================================
  ! 0. **** SOME SET UP PARAMETERS FOR THIS EXAMPLE ****
  !
  ! Profile dimensions...
  INTEGER, PARAMETER :: N_PROFILES  = 2
  INTEGER, PARAMETER :: N_LAYERS    = 92
  INTEGER, PARAMETER :: N_ABSORBERS = 2
  INTEGER, PARAMETER :: N_CLOUDS    = 0
  INTEGER, PARAMETER :: N_AEROSOLS  = 0
  ! ...but only ONE Sensor at a time
  INTEGER, PARAMETER :: N_SENSORS = 1

  ! Test GeometryInfo angles. The test scan angle is based
  ! on the default Re (earth radius) and h (satellite height)
  REAL(fp), PARAMETER :: ZENITH_ANGLE = 30.0_fp
  REAL(fp), PARAMETER :: SCAN_ANGLE   = 26.37293341421_fp
  ! ============================================================================


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: Version
  CHARACTER(256) :: Sensor_Id
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: n_Channels
  INTEGER :: l, m
  INTEGER :: ii
  INTEGER :: testresult 
  ! Declarations for Jacobian comparisons
  INTEGER :: n_la, n_ma
  INTEGER :: n_ls, n_ms
  CHARACTER(256) :: atmk_File, sfck_File
  REAL(fp) :: Perturbation
  REAL(fp) :: Ratio_new, Ratio_old
  REAL(fp), PARAMETER :: TOLERANCE = 0.1_fp


  ! ============================================================================
  ! 1. **** DEFINE THE CRTM INTERFACE STRUCTURES ****
  !
  TYPE(CRTM_ChannelInfo_type)             :: ChannelInfo(N_SENSORS)
  TYPE(CRTM_Geometry_type)                :: Geometry(N_PROFILES)

  ! Define the FORWARD variables
  TYPE(CRTM_Atmosphere_type)              :: Atm(N_PROFILES)
  TYPE(CRTM_Surface_type)                 :: Sfc(N_PROFILES)
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: RTSolution(:,:)
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: RTSolution_Perturb(:,:)

  ! Define the Tangent-Linear variables
  TYPE(CRTM_Atmosphere_type) :: Atmosphere_TL(N_PROFILES)
  !TYPE(CRTM_Atmosphere_type) :: Perturbation
  TYPE(CRTM_Surface_type)    :: Surface_TL(N_PROFILES)
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: RTSolution_TL(:,:)
  ! ============================================================================



  ! Program header
  ! --------------
  CALL CRTM_Version( Version )
  CALL Program_Message( PROGRAM_NAME, &
    'Program to provide a (relatively) simple example of how '//&
    'to call the CRTM K-Matrix function.', &
    'CRTM Version: '//TRIM(Version) )


  ! Get sensor id from user
  ! -----------------------
  !WRITE( *,'(/5x,"Enter sensor id [hirs4_n18, amsua_metop-a, or mhs_n18]: ")',ADVANCE='NO' )
  !READ( *,'(a)' ) Sensor_Id
  Sensor_Id = 'amsua_metop-a'
  Sensor_Id = ADJUSTL(Sensor_Id)
  WRITE( *,'(//5x,"Running CRTM for ",a," sensor...")' ) TRIM(Sensor_Id)


  ! ============================================================================
  ! 2. **** INITIALIZE THE CRTM ****
  !
  ! 2a. This initializes the CRTM for the sensors
  !     predefined in the example SENSOR_ID parameter.
  !     NOTE: The coefficient data file path is hard-
  !           wired for this example.
  ! --------------------------------------------------
  WRITE( *,'(/5x,"Initializing the CRTM...")' )
  Error_Status = CRTM_Init( (/Sensor_Id/), &  ! Input... must be an array, hence the (/../)
                            ChannelInfo  , &  ! Output
                            File_Path=COEFFICIENTS_PATH )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error initializing CRTM'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF

  ! 2b. Determine the total number of channels
  !     for which the CRTM was initialized
  ! ------------------------------------------
  n_Channels = SUM(CRTM_ChannelInfo_n_Channels(ChannelInfo))
  ! ============================================================================




  ! ============================================================================
  ! 3. **** ALLOCATE STRUCTURE ARRAYS ****
  !
  ! 3a. Allocate the ARRAYS
  ! -----------------------
  ! Note that only those structure arrays with a channel
  ! dimension are allocated here because we've parameterized
  ! the number of profiles in the N_PROFILES parameter.
  !
  ! Users can make the number of profiles dynamic also, but
  ! then the INPUT arrays (Atmosphere, Surface) will also have to be allocated.
  ALLOCATE( RTSolution( n_Channels, N_PROFILES ), &
            RTSolution_Perturb( n_Channels, N_PROFILES ), &
            RTSolution_TL( n_Channels, N_PROFILES ), &
            STAT = Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    Message = 'Error allocating structure arrays'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF

  ! 3b. Allocate the STRUCTURES
  ! ---------------------------
  ! The input FORWARD structure
  CALL CRTM_Atmosphere_Create( Atm, N_LAYERS, N_ABSORBERS, N_CLOUDS, N_AEROSOLS )
  IF ( ANY(.NOT. CRTM_Atmosphere_Associated(Atm)) ) THEN
    Message = 'Error allocating CRTM Atmosphere structure'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF

  ! The input TL structure
  CALL CRTM_Atmosphere_Create( Atmosphere_TL, N_LAYERS, N_ABSORBERS, N_CLOUDS, N_AEROSOLS )
  IF ( ANY(.NOT. CRTM_Atmosphere_Associated(Atmosphere_TL)) ) THEN
    Message = 'Error allocating CRTM Atmosphere_TL structure'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF
  ! ============================================================================




  ! ============================================================================
  ! 4. **** ASSIGN INPUT DATA ****
  !
  ! Fill the Atmosphere structure array.
  ! NOTE: This is an example program for illustrative purposes only.
  !       Typically, one would not assign the data as shown below,
  !       but rather read it from file

  ! 4a. Atmosphere and Surface input
  ! --------------------------------
  CALL Load_Atm_Data()
  CALL Load_Sfc_Data()


  ! 4b. GeometryInfo input
  ! ----------------------
  ! All profiles are given the same value
  !  The Sensor_Scan_Angle is optional.
  CALL CRTM_Geometry_SetValue( Geometry, &
                               Sensor_Zenith_Angle = ZENITH_ANGLE, &
                               Sensor_Scan_Angle   = SCAN_ANGLE )
  ! ============================================================================

  


  ! ============================================================================
  ! 5. **** INITIALIZE THE TL ARGUMENTS ****
  !
  ! 5a. Zero the LT INPUT structures
  ! ---------------------------------------
  CALL CRTM_Atmosphere_Zero( Atmosphere_TL )
  CALL CRTM_Surface_Zero( Surface_TL )

  OPEN(444,FILE='convergence.txt',STATUS='UNKNOWN')
  
  testresult = 0
  Ratio_old = 100.0_fp 

  convergence_loop: DO ii = 0, 20

    Perturbation = Atm(1)%Temperature(92)*0.1_fp/(2.0_fp**ii)

    Atmosphere_TL(1)%Temperature(92) = Perturbation


    ! ============================================================================




    ! ============================================================================
    ! 6. **** CALL THE CRTM TANGENT-LINEAR MODEL ****
    !
    Error_Status = CRTM_Tangent_Linear( Atm , &
                                      Sfc , &
                                      Atmosphere_TL , &
                                      Surface_TL , &
                                      Geometry , &
                                      ChannelInfo , &
                                      RTSolution , &
                                      RTSolution_TL  )
    IF ( Error_Status /= SUCCESS ) THEN
     Message = 'Error in CRTM Tangent-linear Model'
     CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
     STOP 1
    END IF

    !WRITE(*,*) 'Tangent-linear Result: ', RTSolution_TL(5,1)%Radiance

    ! ============================================================================



    ! ============================================================================
    ! 6. **** CALL THE CRTM FORWARD MODEL ****
    !
    Error_Status = CRTM_Forward( Atm         , &
                                  Sfc         , &
                                  Geometry    , &
                                  ChannelInfo , &
                                  RTSolution  )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error in CRTM Forward Model'
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
      STOP 1
    END IF
    ! ============================================================================

    Atm(1)%Temperature(92) = Atm(1)%Temperature(92) + Perturbation

    ! ============================================================================
    ! 6. **** CALL THE PERTURBED CRTM FORWARD MODEL ****
    !
    Error_Status = CRTM_Forward( Atm         , &
                                  Sfc         , &
                                  Geometry    , &
                                  ChannelInfo , &
                                  RTSolution_Perturb  )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error in perturbed CRTM Forward Model'
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
      STOP 1
    END IF
    ! ============================================================================

    !WRITE(*,*) 'Nonlinear Perturbation: ', ( RTSolution_Perturb(5,1)%Radiance - RTSolution(5,1)%Radiance )
    Ratio_new = ONE - ( RTSolution_Perturb(5,1)%Radiance - RTSolution(5,1)%Radiance ) &
                 / RTSolution_TL(5,1)%Radiance
    !WRITE(*,*) 'Ratio: ', Ratio
    WRITE(444,*) Perturbation, ABS(Ratio_new)  
    ! Compare the ratio from the previous iteration with the current one.
    WRITE(*,*) ABS(Ratio_new), ABS(Ratio_old)
    IF ( ABS(Ratio_new) > ABS(Ratio_old) ) THEN
      WRITE(*,*) 'Test failed!'
      testresult = 1
    ELSE IF (Ratio_new > ONE) THEN
      testresult = 1
    END IF  

    ! Reassign Ratio for next iteration.
    Ratio_old = Ratio_new 

  END DO convergence_loop

  ! ============================================================================
  ! 8. **** DESTROY THE CRTM ****
  !
  WRITE( *, '( /5x, "Destroying the CRTM..." )' )
  Error_Status = CRTM_Destroy( ChannelInfo )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error destroying CRTM'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF
  ! ============================================================================


  CLOSE(444)

  

  ! ============================================================================
  ! 10. **** CLEAN UP ****
  !
  ! 10a. Deallocate the structures.
  !      These are the explicitly allocated structures.
  !      Note that in some cases other structures, such as the Sfc
  !      and RTSolution structures, will also be allocated and thus
  !      should also be deallocated here.
  ! ---------------------------------------------------------------
  CALL CRTM_Atmosphere_Destroy(Atmosphere_TL)
  CALL CRTM_Atmosphere_Destroy(Atm)

  ! 10b. Deallocate the arrays
  ! --------------------------
  DEALLOCATE(RTSolution, RTSolution_TL, &
             STAT = Allocate_Status)
  ! ============================================================================
  IF( ( testresult == 0 ) .AND. ( Ratio_old < TOLERANCE ) ) THEN
    testresult = 0
    STOP 0
  ELSE
    testresult = 1
    STOP 1
  END IF


CONTAINS

  INCLUDE 'Load_Atm_Data.inc'
  INCLUDE 'Load_Sfc_Data.inc'

END PROGRAM test_TL_convergence
