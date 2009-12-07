!
! RTV_Define
!
! Module containing the intermediate variables for RTSolution module.
!
!
! CREATION HISTORY:
!       Written by:     Quanhua Liu,    QSS at JCSDA;    Quanhua.Liu@noaa.gov 
!                       Yong Han,       NOAA/NESDIS;     Yong.Han@noaa.gov
!                       Paul van Delst, CIMSS/SSEC;      paul.vandelst@ssec.wisc.edu
!                       08-Jun-2004

MODULE RTV_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use statements
  USE Type_Kinds,                ONLY: fp
  USE Message_Handler,           ONLY: SUCCESS, FAILURE, Display_Message
  USE CRTM_Parameters,           ONLY: SET, ZERO, ONE, TWO, PI, &
                                       MAX_N_LAYERS, MAX_N_ANGLES, MAX_N_LEGENDRE_TERMS, &
                                       DEGREES_TO_RADIANS, &
                                       SECANT_DIFFUSIVITY, &
                                       SCATTERING_ALBEDO_THRESHOLD, &
                                       OPTICAL_DEPTH_THRESHOLD
  USE CRTM_SfcOptics,            ONLY: CRTM_SOVariables_type
  ! Disable all implicit typing
  IMPLICIT NONE


  ! --------------------
  ! Default visibilities
  ! --------------------
  ! Everything private by default
  PRIVATE

  ! CRTM_RTSolution structure routines inherited
  ! from the CRTM_RTSolution_Define module

  PUBLIC :: Allocate_RTV
  PUBLIC :: Destroy_RTV
  PUBLIC :: CRTM_RTVariables_type
  
  ! -----------------
  ! Module parameters
  ! -----------------

  ! RCS Id for the module
  CHARACTER(*),  PARAMETER :: MODULE_RCS_ID = &
  '$Id$'

  ! Threshold for determing if an additional stream
  ! angle is required for the satellite zenith angle
  REAL(fp), PARAMETER, PUBLIC :: ANGLE_THRESHOLD = 1.0e-7_fp

  ! Small positive value used to replace negative
  ! values in the computed phase function
  REAL(fp), PARAMETER, PUBLIC :: PHASE_THRESHOLD = 1.0e-7_fp
  REAL(fp), PARAMETER, PUBLIC :: DELTA_OPTICAL_DEPTH = 1.0e-8_fp
  REAL(fp), PARAMETER, PUBLIC :: max_albedo = 0.999999_fp
  
  ! Threshold layer optical depth for single scattering
  REAL(fp), PARAMETER :: small_od_for_sc = 1.E-5_fp
  ! --------------------------------------
  ! Structure definition to hold forward
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------

  TYPE :: CRTM_RTVariables_type

    ! Dimension information
    INTEGER :: n_Layers       = 0       ! Total number of atmospheric layers
    INTEGER :: n_Added_Layers = 0       ! Number of layers appended to TOA
    INTEGER :: n_Angles       = 0       ! Number of angles to be considered
    REAL(fp):: COS_SUN = ZERO           ! Cosine of sun zenith angle
    REAL(fp):: Solar_irradiance = ZERO  ! channel solar iiradiance at TOA 
            
    ! Variable to hold the various portions of the
    ! radiance for emissivity retrieval algorithms
    ! Passed to FWD RTSolution structure argument output
    REAL(fp) :: Up_Radiance         = ZERO
    REAL(fp) :: Down_Solar_Radiance = ZERO

    REAL(fp) :: Secant_Down_Angle = 0

    ! Emission model variables
    REAL(fp) :: Total_OD  = ZERO
    REAL(fp), DIMENSION(   MAX_N_LAYERS ) :: e_Layer_Trans_UP   = ZERO
    REAL(fp), DIMENSION(   MAX_N_LAYERS ) :: e_Layer_Trans_DOWN = ZERO
    REAL(fp), DIMENSION( 0:MAX_N_LAYERS ) :: e_Level_Rad_UP     = ZERO
    REAL(fp), DIMENSION( 0:MAX_N_LAYERS ) :: e_Level_Rad_DOWN   = ZERO

    ! Planck radiances
    REAL(fp)                               :: Planck_Surface    = ZERO
    REAL(fp), DIMENSION(  0:MAX_N_LAYERS ) :: Planck_Atmosphere = ZERO

    ! Quadrature information
    REAL(fp), DIMENSION( MAX_N_ANGLES ) :: COS_Angle  = ZERO  ! Gaussian quadrature abscissa
    REAL(fp), DIMENSION( MAX_N_ANGLES ) :: COS_Weight = ZERO  ! Gaussian quadrature weights

    ! Logical switches
    LOGICAL :: Diffuse_Surface = .TRUE.

    ! Scattering, visible model variables    
    INTEGER :: n_Streams      = 0  !  Number of *hemispheric* stream angles used in RT    
    INTEGER :: mth_Azi     ! mth fourier component
    INTEGER :: n_Azi       ! number of fourier components
    LOGICAL :: Solar_Flag_true = .FALSE.
    LOGICAL :: Visible_Flag_true 
    LOGICAL :: Scattering_RT   = .FALSE.

    !-------------------------------------------------------
    ! Variables used in the ADA routines
    !-------------------------------------------------------
    ! Flag to indicate the following arrays have all been allocated
    LOGICAL :: mAllocated = .FALSE.
     
    ! Phase function variables
    ! Forward and backward scattering phase matrices
    REAL(fp), ALLOCATABLE :: Pff(:,:,:)  ! MAX_N_ANGLES, MAX_N_ANGLES+1, MAX_N_LAYERS
    REAL(fp), ALLOCATABLE :: Pbb(:,:,:)  ! MAX_N_ANGLES, MAX_N_ANGLES+1, MAX_N_LAYERS

    ! Positive and negative cosine angle Legendre phase functions
    REAL(fp), ALLOCATABLE :: Pplus(:,:)  ! 0:MAX_N_LEGENDRE_TERMS, MAX_N_ANGLES
    REAL(fp), ALLOCATABLE :: Pminus(:,:) ! 0:MAX_N_LEGENDRE_TERMS, MAX_N_ANGLES
    REAL(fp), ALLOCATABLE :: Pleg(:,:)   ! 0:MAX_N_LEGENDRE_TERMS, MAX_N_ANGLES+1
    
    ! Original forward and backward scattering phase matrices.
    ! These may be slightly negative and, if so, need to be made
    ! positive and thus adjusted to ensure energy conservation
    REAL(fp), ALLOCATABLE :: Off(:,:,:)  ! MAX_N_ANGLES, MAX_N_ANGLES+1, MAX_N_LAYERS
    REAL(fp), ALLOCATABLE :: Obb(:,:,:)  ! MAX_N_ANGLES, MAX_N_ANGLES+1, MAX_N_LAYERS
    
    ! Normalisation factor and intermediate sum used for original
    ! phase matrix energy conservation.
    REAL(fp), ALLOCATABLE :: n_Factor(:,:)  ! MAX_N_ANGLES, MAX_N_LAYERS
    REAL(fp), ALLOCATABLE :: sum_fac(:,:)   ! 0:MAX_N_ANGLES, MAX_N_LAYERS

    ! Adding-Doubling model variables

    REAL(fp), ALLOCATABLE :: Inv_Gamma(:,:,:)         ! MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS
    REAL(fp), ALLOCATABLE :: Inv_GammaT(:,:,:)        ! MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS
    REAL(fp), ALLOCATABLE :: Refl_Trans(:,:,:)        ! MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS

    REAL(fp), ALLOCATABLE :: s_Layer_Trans(:,:,:)     ! MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS
    REAL(fp), ALLOCATABLE :: s_Layer_Refl(:,:,:)      ! MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS

    REAL(fp), ALLOCATABLE :: s_Level_Refl_UP(:,:,:)   ! MAX_N_ANGLES, MAX_N_ANGLES, 0:MAX_N_LAYERS
    REAL(fp), ALLOCATABLE :: s_Level_Rad_UP(:,:)      ! MAX_N_ANGLES, 0:MAX_N_LAYERS
     
    REAL(fp), ALLOCATABLE :: s_Layer_Source_UP(:,:)   ! MAX_N_ANGLES, MAX_N_LAYERS
    REAL(fp), ALLOCATABLE :: s_Layer_Source_DOWN(:,:) ! MAX_N_ANGLES, MAX_N_LAYERS

   ! AMOM layer variables
   ! dimensions, MAX_N_ANGLES, MAX_N_LAYERS
    REAL(fp), ALLOCATABLE :: Thermal_C(:,:)
    REAL(fp), ALLOCATABLE :: EigVa(:,:)
    REAL(fp), ALLOCATABLE :: Exp_x(:,:)
    REAL(fp), ALLOCATABLE :: EigValue(:,:)
    
    ! dimensions, MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS
    REAL(fp), ALLOCATABLE :: HH(:,:,:)
    REAL(fp), ALLOCATABLE :: PM(:,:,:)
    REAL(fp), ALLOCATABLE :: PP(:,:,:)
    REAL(fp), ALLOCATABLE :: PPM(:,:,:)
    REAL(fp), ALLOCATABLE :: PPP(:,:,:)
    REAL(fp), ALLOCATABLE :: i_PPM(:,:,:)
    REAL(fp), ALLOCATABLE :: i_PPP(:,:,:)
    REAL(fp), ALLOCATABLE :: EigVe(:,:,:)
    REAL(fp), ALLOCATABLE :: Gm(:,:,:)
    REAL(fp), ALLOCATABLE :: i_Gm(:,:,:)
    REAL(fp), ALLOCATABLE :: Gp(:,:,:)
    REAL(fp), ALLOCATABLE :: EigVeF(:,:,:)
    REAL(fp), ALLOCATABLE :: EigVeVa(:,:,:)
    REAL(fp), ALLOCATABLE :: A1(:,:,:)
    REAL(fp), ALLOCATABLE :: A2(:,:,:)
    REAL(fp), ALLOCATABLE :: A3(:,:,:)
    REAL(fp), ALLOCATABLE :: A4(:,:,:)
    REAL(fp), ALLOCATABLE :: A5(:,:,:)
    REAL(fp), ALLOCATABLE :: A6(:,:,:)
    REAL(fp), ALLOCATABLE :: Gm_A5(:,:,:)
    REAL(fp), ALLOCATABLE :: i_Gm_A5(:,:,:)

    ! The surface optics forward variables
    TYPE(CRTM_SOVariables_type) :: SOV

  END TYPE CRTM_RTVariables_type


CONTAINS



!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################


 ! --------------------------------------------------------!
 !  FUNCTION: allocate structure RTV ALLOCATABLE arrays.       !
 ! --------------------------------------------------------
  FUNCTION Allocate_RTV( RTV,              &  ! Output
                         RCS_Id,           &  ! Revision control          
                         Message_Log )     &  ! Error messaging           
                       RESULT( Error_Status )                             
    TYPE(CRTM_RTVariables_type),   INTENT(IN OUT) :: RTV
    CHARACTER(*),        OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),        OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Allocate_RTV'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: Allocate_Status

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Perform the allocation for phase functions
    ALLOCATE( RTV%Pff(MAX_N_ANGLES, MAX_N_ANGLES+1, MAX_N_LAYERS), &
              RTV%Pbb(MAX_N_ANGLES, MAX_N_ANGLES+1, MAX_N_LAYERS), &
              RTV%Pplus (0:MAX_N_LEGENDRE_TERMS, MAX_N_ANGLES),&
              RTV%Pminus(0:MAX_N_LEGENDRE_TERMS, MAX_N_ANGLES),&
              RTV%Pleg(0:MAX_N_LEGENDRE_TERMS, MAX_N_ANGLES+1),&
              RTV%Off(MAX_N_ANGLES, MAX_N_ANGLES+1, MAX_N_LAYERS),&
              RTV%Obb(MAX_N_ANGLES, MAX_N_ANGLES+1, MAX_N_LAYERS),&
              RTV%n_Factor (MAX_N_ANGLES, MAX_N_LAYERS),&
              RTV%sum_fac(0:MAX_N_ANGLES, MAX_N_LAYERS),&
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating RTV data arrays for phase function variables. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF
    
    ! Perform the allocation
    ALLOCATE( RTV%Inv_Gamma (MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS),&
              RTV%Inv_GammaT(MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS),&
              RTV%Refl_Trans(MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS),&
              RTV%s_Layer_Trans(MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS),&
              RTV%s_Layer_Refl (MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS),&
              RTV%s_Level_Refl_UP(MAX_N_ANGLES, MAX_N_ANGLES, 0:MAX_N_LAYERS),&
              RTV%s_Level_Rad_UP(MAX_N_ANGLES, 0:MAX_N_LAYERS),&
              RTV%s_Layer_Source_UP  (MAX_N_ANGLES, MAX_N_LAYERS),&
              RTV%s_Layer_Source_DOWN(MAX_N_ANGLES, MAX_N_LAYERS),&
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating RTV data arrays for adding-doubling variables. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Perform the allocation for AMOM layer variables
    ALLOCATE( RTV%Thermal_C(MAX_N_ANGLES, MAX_N_LAYERS),&
              RTV%EigVa(MAX_N_ANGLES, MAX_N_LAYERS),&
              RTV%Exp_x( MAX_N_ANGLES, MAX_N_LAYERS),&
              RTV%EigValue(MAX_N_ANGLES, MAX_N_LAYERS),&
              RTV%HH(MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS ),&
              RTV%PM(MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS),&
              RTV%PP(MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS ),&
              RTV%PPM(MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS),&
              RTV%PPP(MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS),&
              RTV%i_PPM(MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS),&
              RTV%i_PPP(MAX_N_ANGLES,MAX_N_ANGLES,MAX_N_LAYERS),&
              RTV%EigVe(MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS),&
              RTV%Gm(MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS ),&
              RTV%i_Gm(MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS),&
              RTV%Gp(MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS ),&
              RTV%EigVeF(MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS),&
              RTV%EigVeVa(MAX_N_ANGLES,MAX_N_ANGLES,MAX_N_LAYERS),&
              RTV%A1(MAX_N_ANGLES,MAX_N_ANGLES, MAX_N_LAYERS),&
              RTV%A2(MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS),&
              RTV%A3(MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS),&
              RTV%A4(MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS),&
              RTV%A5(MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS),&
              RTV%A6(MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS),&
              RTV%Gm_A5(MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS),&
              RTV%i_Gm_A5(MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS),&  
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating RTV data arrays for AMOM layer variables. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF
    
    RTV%mAllocated = .TRUE.
              
                 
  END FUNCTION Allocate_RTV


 ! --------------------------------------------------------!
 !  FUNCTION: deallocate structure RTV ALLOCATABLE arrays.     !
 ! --------------------------------------------------------
  FUNCTION Destroy_RTV( RTV,              &  ! Output
                        RCS_Id,           &  ! Revision control           
                        Message_Log )     &  ! Error messaging            
                        RESULT( Error_Status )                             
    TYPE(CRTM_RTVariables_type),   INTENT(IN OUT) :: RTV
    CHARACTER(*),        OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),        OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Destroy_RTV'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: Allocate_Status

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Deallocate the variablea
    DEALLOCATE( RTV%Pff, &
                RTV%Pbb, &
                RTV%Pplus,&
                RTV%Pminus,&
                RTV%Pleg,&
                RTV%Off,&
                RTV%Obb,&
                RTV%n_Factor,&
                RTV%sum_fac,&
                RTV%Inv_Gamma,&
                RTV%Inv_GammaT,&
                RTV%Refl_Trans,&
                RTV%s_Layer_Trans,&
                RTV%s_Layer_Refl,&
                RTV%s_Level_Refl_UP,&
                RTV%s_Level_Rad_UP,&
                RTV%s_Layer_Source_UP,&
                RTV%s_Layer_Source_DOWN,&                
                RTV%Thermal_C,RTV%EigVa,RTV%Exp_x,RTV%EigValue,&
                RTV%HH,RTV%PM,RTV%PP,RTV%PPM,RTV%PPP,RTV%i_PPM, RTV%i_PPP,&
                RTV%EigVe,RTV%Gm,RTV%i_Gm,RTV%Gp,RTV%EigVeF,RTV%EigVeVa,&
                RTV%A1,RTV%A2,RTV%A3,RTV%A4,RTV%A5,RTV%A6,RTV%Gm_A5,RTV%i_Gm_A5,&       
                STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error deallocating RTV structure. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,  &
                            TRIM(Message), &
                            Error_Status,  &
                            Message_Log=Message_Log )
    END IF

    RTV%mAllocated = .FALSE.
    
  END FUNCTION Destroy_RTV
  

END MODULE RTV_Define
