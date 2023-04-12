!
! test_TL.f90
!
! Program to test the CRTM hypsometric equation module.
!
! This test simply checks if the hypsometric equation function
! in the CRTM runs successfully.
!
! Copyright Patrick Stegmann, 2021
!
PROGRAM test_Hypsometric

  ! ============================================================================
  ! **** ENVIRONMENT SETUP FOR RTM USAGE ****
  !
  ! Module usage
  USE CRTM_Module
  USE CRTM_Hypsometric, ONLY: HypsometricEq
  USE UnitTest_Define,  ONLY: UnitTest_IsEqualWithin
  ! Disable all implicit typing
  IMPLICIT NONE
  ! ============================================================================


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'test_Hypsometric'
  CHARACTER(*), PARAMETER :: COEFFICIENTS_PATH = './testinput/'
  CHARACTER(*), PARAMETER :: RESULTS_PATH = './results/unit/'

  ! ============================================================================
  ! 0. **** SOME SET UP PARAMETERS FOR THIS EXAMPLE ****
  !
  ! Profile dimensions...
  INTEGER, PARAMETER :: N_PROFILES  = 2
  INTEGER, PARAMETER :: N_LAYERS    = 92
  INTEGER, PARAMETER :: N_ABSORBERS = 2
  INTEGER, PARAMETER :: N_CLOUDS    = 0
  INTEGER, PARAMETER :: N_AEROSOLS  = 0
  REAL(KIND=fp), DIMENSION(0:N_LAYERS) :: altitude
  REAL(KIND=fp), DIMENSION(N_LAYERS) :: Layer_Test_Temperature
  ! ============================================================================


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: Version
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: l, m
  INTEGER :: testresult
  REAL(fp) :: Ratio
  REAL(fp), PARAMETER :: TOLERANCE = 0.1_fp


  ! ============================================================================
  ! 1. **** DEFINE THE CRTM INTERFACE STRUCTURES ****
  !

  ! Define the FORWARD variables
  TYPE(CRTM_Atmosphere_type)              :: Atm(N_PROFILES)

  ! ============================================================================



  ! Program header
  ! --------------
  CALL CRTM_Version( Version )
  CALL Program_Message( PROGRAM_NAME, &
    'Unit test for the CRTM '//&
    'hypsometric equation module.', &
    'CRTM Version: '//TRIM(Version) )

  ! UMBC48 profile 01 altitude data in [m]:
  altitude(0:N_LAYERS) = (/ 1983,2241,2500,2761,3025,3291,3560,&
    3832,4105,4381,4659,4940,5222,5507,5794,6084,6376,6670,6966,7266,7567,7871,&
    8178,8488,8799,9114,9431,9752,10075,10400,10729,11061,11395,11732,12073,12416,&
    12763,13113,13467,13824,14184,14548,14915,15286,15661,16039,16422,16809,17201,&
    17601,18011,18434,18871,19324,19793,20280,20785,21309,21855,22423,23014,23629,&
    24268,24934,25627,26349,27104,27894,28722,29590,30502,31463,32477,33550,34687,&
    35897,37188,38570,40055,41657,43395,45290,47367,49651,52152,54886,57880,61179,&
    64822,68868,73451,78799,85424 /)
  altitude = altitude(UBOUND(altitude,DIM=1):0:-1)

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

  ! 3b. Allocate the STRUCTURES
  ! ---------------------------
  ! The input FORWARD structure
  CALL CRTM_Atmosphere_Create( Atm, N_LAYERS, N_ABSORBERS, N_CLOUDS, N_AEROSOLS )
  IF ( ANY(.NOT. CRTM_Atmosphere_Associated(Atm)) ) THEN
    Message = 'Error allocating CRTM Atmosphere structure'
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

  ! ============================================================================



  ! ============================================================================
  ! 5. **** CALL THE HYPSOMETRIC EQUATION ****
  !

  ! --------------------------------
  Layer_Test_Temperature = HypsometricEq( Atm(1), &
                                          altitude)
  WRITE(*,*) Layer_Test_Temperature
  ! ============================================================================

  

  ! ============================================================================
  ! 6. **** CLEAN UP ****
  !
  ! 6a. Deallocate the structures.
  !      These are the explicitly allocated structures.
  !      Note that in some cases other structures, such as the Sfc
  !      and RTSolution structures, will also be allocated and thus
  !      should also be deallocated here.
  ! ---------------------------------------------------------------

  CALL CRTM_Atmosphere_Destroy(Atm)

  ! ============================================================================
  IF(ALL(Layer_Test_Temperature == Layer_Test_Temperature)) THEN
    STOP 0
  ELSE
    Message = 'Error in the computation of the hypsometric equation.'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF 
 
CONTAINS

  INCLUDE 'Load_Atm_Data.inc'

END PROGRAM test_Hypsometric
