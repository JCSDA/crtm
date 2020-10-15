!
! test_AD
!
! Program to provide a (relatively) simple example of how
! to test the CRTM adjoint function. 
! 
! The code checks whether the Jacobian from the Tangent-Linear
! and the Adjoint are consistent.
!
! Copyright Patrick Stegmann, 2020
!

PROGRAM test_AD

  ! ============================================================================
  ! **** ENVIRONMENT SETUP FOR RTM USAGE ****
  !
  ! Module usage
  USE CRTM_Module

  ! Disable all implicit typing
  IMPLICIT NONE
  ! ============================================================================

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'test_AD'
  CHARACTER(*), PARAMETER :: COEFFICIENTS_PATH = './testinput/'
  CHARACTER(*), PARAMETER :: RESULTS_PATH = './results/unit/'



  ! ============================================================================
  ! 0. **** SOME SET UP PARAMETERS FOR THIS EXAMPLE ****
  !
  ! Profile dimensions...
  INTEGER, PARAMETER :: N_PROFILES  = 2
  INTEGER, PARAMETER :: N_LAYERS    = 72
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
  INTEGER :: test_result
  ! Declarations for Jacobian comparisons
  INTEGER :: n_la, n_ma
  INTEGER :: n_ls, n_ms
  INTEGER :: ii, jj
  CHARACTER(256) :: atmk_File, sfck_File
  ! Declarations for adjoint testing
  REAL(fp) :: Perturbation
  REAL(fp) :: Ratio
  REAL(fp), DIMENSION(1,1) :: LHS
  REAL(fp), DIMENSION(1,1) :: RHS
  REAL(fp), PARAMETER :: TOLERANCE = 0.1_fp
  REAL(fp), DIMENSION(2,1) :: x_test ! Temperature state vector for the adjoint ctest
  REAL(fp), DIMENSION(2,2) :: L_operator ! Linearized operator
  REAL(fp), DIMENSION(2,2) :: L_operator_T ! Linearized operator


  ! ============================================================================
  ! 1. **** DEFINE THE CRTM INTERFACE STRUCTURES ****
  !
  TYPE(CRTM_ChannelInfo_type)             :: ChannelInfo(N_SENSORS)
  TYPE(CRTM_Geometry_type)                :: Geometry(N_PROFILES)

  ! Define the FORWARD variables
  TYPE(CRTM_Atmosphere_type)              :: Atm(N_PROFILES)
  TYPE(CRTM_Surface_type)                 :: Sfc(N_PROFILES)
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: RTSolution(:,:)

  ! Define the Tangent-Linear variables
  TYPE(CRTM_Atmosphere_type) :: Atmosphere_TL(N_PROFILES)
  TYPE(CRTM_Surface_type)    :: Surface_TL(N_PROFILES)
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: RTSolution_TL(:,:)

  ! Define the Adjoint variables
  TYPE(CRTM_Atmosphere_type) :: Atmosphere_AD(N_PROFILES)
  TYPE(CRTM_Surface_type) :: Surface_AD(N_PROFILES)
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: RTSolution_AD(:,:)
  ! ============================================================================



  ! Program header
  ! --------------
  CALL CRTM_Version( Version )
  CALL Program_Message( PROGRAM_NAME, &
    'Program to provide a basic test for the CRTM adjoint operator.', &
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
    STOP
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
            RTSolution_TL( n_Channels, N_PROFILES ), &
            RTSolution_AD( n_Channels, N_PROFILES ), &
            STAT = Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    Message = 'Error allocating structure arrays'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP
  END IF

  ! 3b. Allocate the STRUCTURES
  ! ---------------------------
  ! The input FORWARD structure
  CALL CRTM_Atmosphere_Create( Atm, N_LAYERS, N_ABSORBERS, N_CLOUDS, N_AEROSOLS )
  IF ( ANY(.NOT. CRTM_Atmosphere_Associated(Atm)) ) THEN
    Message = 'Error allocating CRTM Atmosphere structure'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP
  END IF

  ! The input TL structure
  CALL CRTM_Atmosphere_Create( Atmosphere_TL, N_LAYERS, N_ABSORBERS, N_CLOUDS, N_AEROSOLS )
  IF ( ANY(.NOT. CRTM_Atmosphere_Associated(Atmosphere_TL)) ) THEN
    Message = 'Error allocating CRTM Atmosphere_TL structure'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP
  END IF


  ! The output AD structure
  CALL CRTM_Atmosphere_Create( Atmosphere_AD, N_LAYERS, N_ABSORBERS, N_CLOUDS, N_AEROSOLS )
  IF ( ANY(.NOT. CRTM_Atmosphere_Associated(Atmosphere_AD)) ) THEN
    Message = 'Error allocating CRTM Atmosphere_AD structure'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP
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

  ! Set the test state vector as the first to Temperature values.
  ! -------------------------------------------------------------
  x_test(1,1) = Atm(1)%Temperature(61)
  x_test(2,1) = Atm(1)%Temperature(72)

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
  
  ! 5a. Zero the LT INPUT structures
  ! ---------------------------------------
  CALL CRTM_Atmosphere_Zero( Atmosphere_TL )
  CALL CRTM_Surface_Zero( Surface_TL )
  Perturbation = ONE
  !Perturbation = Atm(1)%Temperature(72)*0.1_fp**6
  Atmosphere_TL(1)%Temperature(72) = Perturbation


  ! ============================================================================
  ! 5b. Zero the AD OUTPUT structures
  ! ---------------------------------------
  CALL CRTM_Atmosphere_Zero( Atmosphere_AD )
  CALL CRTM_Surface_Zero( Surface_AD )



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
   STOP
  END IF

  WRITE(*,*) 'Tangent-linear Result 1: ', RTSolution_TL(5,1)%Radiance
  L_operator(1,1) = RTSolution_TL(5,1)%Radiance
  L_operator(1,2) = RTSolution_TL(6,1)%Radiance

  CALL CRTM_Atmosphere_Zero( Atmosphere_TL )
  CALL CRTM_Surface_Zero( Surface_TL )

  !Perturbation = Atm(1)%Temperature(61)*0.1_fp
  Perturbation = ONE
  Atmosphere_TL(1)%Temperature(61) = Perturbation

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
   STOP
  END IF

  WRITE(*,*) 'Tangent-linear Result 2: ', RTSolution_TL(5,1)%Radiance
  L_operator(2,1) = RTSolution_TL(5,1)%Radiance
  L_operator(2,2) = RTSolution_TL(6,1)%Radiance


  ! ============================================================================


  ! ============================================================================
  ! 6. **** CALL THE CRTM ADJOINT MODEL ****
  !

  CALL CRTM_Atmosphere_Zero( Atmosphere_AD )
  CALL CRTM_Surface_Zero( Surface_AD )

  ! Initialise the Adjoint INPUT to provide dR/dx derivatives
  RTSolution_AD%Radiance = ZERO 
  RTSolution_AD(5,1)%Radiance = ONE ! Check only channel 5 in the first AD run.
  RTSolution_AD%Brightness_Temperature = ZERO

  Error_Status = CRTM_Adjoint( Atm , &
                              Sfc , &
                              RTSolution_AD, &
                              Geometry, &
                              ChannelInfo, &
                              Atmosphere_AD, &
                              Surface_AD, &
                              RTSolution  )
  IF ( Error_Status /= SUCCESS ) THEN
   Message = 'Error in Adjoint Model'
   CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
   STOP
  END IF

  WRITE(*,*) 'Adjoint Result 1: ', Atmosphere_AD(1)%Temperature(72)
  L_operator_T(1,1) = Atmosphere_AD(1)%Temperature(72)
  L_operator_T(1,2) = Atmosphere_AD(1)%Temperature(61)

  CALL CRTM_Atmosphere_Zero( Atmosphere_AD )
  CALL CRTM_Surface_Zero( Surface_AD )

  ! Initialise the Adjoint INPUT to provide dR/dx derivatives
  RTSolution_AD%Radiance = ZERO
  RTSolution_AD(6,1)%Radiance = ONE ! Check only channel 6 in the first AD run.
  RTSolution_AD%Brightness_Temperature = ZERO

  Error_Status = CRTM_Adjoint( Atm , &
                              Sfc , &
                              RTSolution_AD, &
                              Geometry, &
                              ChannelInfo, &
                              Atmosphere_AD, &
                              Surface_AD, &
                              RTSolution  )
  IF ( Error_Status /= SUCCESS ) THEN
   Message = 'Error in CRTM Adjoint Model'
   CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
   STOP
  END IF

  WRITE(*,*) 'Adjoint Result 2: ', Atmosphere_AD(1)%Temperature(61)
  L_operator_T(2,1) = Atmosphere_AD(1)%Temperature(72)
  L_operator_T(2,2) = Atmosphere_AD(1)%Temperature(61)

  ! ============================================================================


  ! ============================================================================
  !  **** PERFORM ADJOINT TEST ****
  !
  WRITE(*,*) 'Tangent-Linear: ', TRANSPOSE(L_operator)
  WRITE(*,*) ' '
  WRITE(*,*) 'Adjoint: ', L_operator_T

  L_operator = TRANSPOSE(L_operator)
  test_result = 0
  test_loop: DO ii = 1,2
    DO jj = 1,2
      IF((test_result) == 0 .AND. &
        (L_operator(jj,ii) - L_operator_T(ii,jj))<1.e-7_fp) THEN
        test_result = 0
        STOP 0
      ELSE
        test_result = 1
        STOP 1
      END IF
    END DO
  END DO test_loop 

  ! ============================================================================
  ! 8. **** DESTROY THE CRTM ****
  !
  WRITE( *, '( /5x, "Destroying the CRTM..." )' )
  Error_Status = CRTM_Destroy( ChannelInfo )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error destroying CRTM'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP
  END IF
  ! ============================================================================
 

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

 
CONTAINS

  INCLUDE 'Load_Atm_Data.inc'
  INCLUDE 'Load_Sfc_Data.inc'

END PROGRAM test_AD
