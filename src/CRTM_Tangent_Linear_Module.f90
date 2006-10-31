!
! CRTM_Tangent_Linear_Module
!
! Module containing the CRTM tangent-linear model function.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Jan-2005
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_Tangent_Linear_Module


  ! ------------
  ! Module usage
  ! ------------
  USE Type_Kinds,               ONLY: fp=>fp_kind
  USE Message_Handler,          ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE CRTM_Parameters,          ONLY: SET, NOT_SET, ONE, &
                                      MAX_N_PROFILES, &
                                      MAX_N_ABSORBERS, &
                                      MAX_N_PREDICTORS, &
                                      MAX_N_PHASE_ELEMENTS, &
                                      MAX_N_LEGENDRE_TERMS, &
                                      MAX_N_STOKES, &
                                      MAX_N_ANGLES
  USE CRTM_Atmosphere_Define,   ONLY: CRTM_Atmosphere_type
  USE CRTM_Surface_Define,      ONLY: CRTM_Surface_type
  USE CRTM_GeometryInfo_Define, ONLY: CRTM_GeometryInfo_type   , &
                                      CRTM_Compute_GeometryInfo
  USE CRTM_ChannelInfo_Define,  ONLY: CRTM_ChannelInfo_type
  USE CRTM_Options_Define,      ONLY: CRTM_Options_type
  USE CRTM_Predictor,           ONLY: CRTM_Predictor_type       , &
                                      CRTM_APVariables_type     , &
                                      CRTM_Allocate_Predictor   , &
                                      CRTM_Destroy_Predictor    , &
                                      CRTM_Compute_Predictors   , &
                                      CRTM_Compute_Predictors_TL
  USE CRTM_AtmAbsorption,       ONLY: CRTM_AtmAbsorption_type      , &
                                      CRTM_Allocate_AtmAbsorption  , &
                                      CRTM_Destroy_AtmAbsorption   , &
                                      CRTM_Compute_AtmAbsorption   , &
                                      CRTM_Compute_AtmAbsorption_TL
  USE CRTM_AtmScatter_Define,   ONLY: CRTM_AtmScatter_type    , &
                                      CRTM_Allocate_AtmScatter, &
                                      CRTM_Destroy_AtmScatter
  USE CRTM_AerosolScatter,      ONLY: CRTM_Compute_AerosolScatter   , &
                                      CRTM_Compute_AerosolScatter_TL
  USE CRTM_CloudScatter,        ONLY: CRTM_CSVariables_type       , &
                                      CRTM_Compute_CloudScatter   , &
                                      CRTM_Compute_CloudScatter_TL
  USE CRTM_AtmOptics,           ONLY: CRTM_AOVariables_type    , &
                                      CRTM_Combine_AtmOptics   , &
                                      CRTM_Combine_AtmOptics_TL
  USE CRTM_SfcOptics,           ONLY: CRTM_SfcOptics_type     , &
                                      CRTM_Allocate_SfcOptics , &
                                      CRTM_Destroy_SfcOptics  , &
                                      CRTM_Compute_SurfaceT   , &
                                      CRTM_Compute_SurfaceT_TL  
  USE CRTM_RTSolution,          ONLY: CRTM_RTSolution_type      , &
                                      CRTM_RTVariables_type     , &
                                      CRTM_Compute_n_Streams    , &
                                      CRTM_Compute_RTSolution   , &
                                      CRTM_Compute_RTSolution_TL


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Public procedures
  PUBLIC :: CRTM_Tangent_Linear


  ! --------------------
  ! Function overloading
  ! --------------------
  INTERFACE CRTM_Tangent_Linear
    MODULE PROCEDURE CRTM_Tangent_Linear_scalar
    MODULE PROCEDURE CRTM_Tangent_Linear_rank1
  END INTERFACE CRTM_Tangent_Linear


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_Tangent_Linear_Module.f90,v 1.12 2006/08/31 17:04:04 frpv Exp $'


CONTAINS


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Tangent_Linear
!
! PURPOSE:
!       Function that calculates tangent-linear top-of-atmosphere (TOA)
!       radiances and brightness temperatures for an input atmospheric
!       profile or profile set and user specified satellites/channels.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Tangent_Linear( Atmosphere,             &  ! FWD Input
!                                           Surface,                &  ! FWD Input
!                                           Atmosphere_TL,          &  ! TL  Input
!                                           Surface_TL,             &  ! TL  Input
!                                           GeometryInfo,           &  ! Input
!                                           ChannelInfo,            &  ! Input
!                                           RTSolution,             &  ! FWD Output
!                                           RTSolution_TL,          &  ! TL  Output
!                                           Options=Options,        &  ! Optional FWD input
!                                           RCS_Id=RCS_Id,          &  ! Revision control
!                                           Message_Log=Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Atmosphere:     Structure containing the Atmosphere data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Scalar
!                                     or
!                                   Rank-1 (M)
!                       ATTRIBUTES: INTENT(IN)
!
!       Surface:        Structure containing the Surface data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Surface_type
!                       DIMENSION:  Scalar
!                                     or
!                                   Rank-1 (M)
!                       ATTRIBUTES: INTENT(IN)
!
!       Atmosphere_TL:  Structure containing the tangent-linear Atmosphere data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Scalar
!                                     or
!                                   Rank-1 (M)
!                       ATTRIBUTES: INTENT(IN)
!
!       Surface_TL:     Structure containing the tangent-linear Surface data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Surface_type
!                       DIMENSION:  Scalar
!                                     or
!                                   Rank-1 (M)
!                       ATTRIBUTES: INTENT(IN)
!
!       GeometryInfo:   Structure containing the view geometry
!                       information.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_GeometryInfo_type)
!                       DIMENSION:  Scalar
!                                     or
!                                   Rank-1 (M)
!                       ATTRIBUTES: INTENT(IN)
!
!       ChannelInfo:    Structure returned from the CRTM_Init() function
!                       that contains the satellite/sesnor channel index
!                       information.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_ChannelInfo_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Options:        Options structure containing the optional forward model
!                       arguments for the CRTM.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Options_type
!                       DIMENSION:  Same as input Atmosphere structure
!                                   See dimensionality table in COMMENTS below.
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to the screen.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       RTSolution:     Structure containing the solution to the RT equation
!                       for the given inputs.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!       RTSolution_TL:  Structure containing the solution to the tangent-
!                       linear RT equation for the given inputs.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:         Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the computation was sucessful
!                          == FAILURE an unrecoverable error occurred
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! COMMENTS:
!       - The folowing tables details the input/output argument dimensionality
!         association, where L == n_Channels, M == n_Profiles:
!
!                                              |   OPTIONAL   |
!                  INPUTS                      |    INPUT     |   OUTPUTS
!                                              |              |
!     Atmosphere     Surface    GeometryInfo   |   Options    |   RTSolution
!    Atmosphere_TL  Surface_TL                 |              |  RTSolution_TL
!  --------------------------------------------+--------------+----------------
!       Scalar       Scalar        Scalar      |    Scalar    |      L
!                                              |              |
!         M            M             M         |      M       |    L x M
!
!         Thus one can process either a single profile or multiple profiles.
!         The routines for each specific case above have been overloaded to
!         the generic interface described in the header above.
!
!       - Note that the Options optional input structure arguments contain
!         spectral information (e.g. emissivity) that must have the same
!         spectral dimensionality (the "L" dimension) as the output
!         RTSolution structures.
!
!       - Note the INTENT on the output RTSolution arguments are IN OUT rather
!         than just OUT. This is necessary because the arguments may be defined
!         upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------


  !#----------------------------------------------------------------------------#
  !#----------------------------------------------------------------------------#
  !#                -- RANK-1 (N_PROFILES) SPECIFIC FUNCTION --                 #
  !#----------------------------------------------------------------------------#
  !#----------------------------------------------------------------------------#

  FUNCTION CRTM_Tangent_Linear_rank1( Atmosphere,    &  ! FWD Input, M
                                      Surface,       &  ! FWD Input, M
                                      Atmosphere_TL, &  ! TL  Input, M
                                      Surface_TL,    &  ! TL  Input, M
                                      GeometryInfo,  &  ! Input, M
                                      ChannelInfo,   &  ! Input, Scalar  
                                      RTSolution,    &  ! FWD Output, L x M  
                                      RTSolution_TL, &  ! TL  Output, L x M  
                                      Options,       &  ! Optional FWD input, M
                                      RCS_Id,        &  ! Revision control
                                      Message_Log )  &  ! Error messaging
                                    RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Atmosphere_type),        DIMENSION(:),   INTENT(IN)     :: Atmosphere    ! M
    TYPE(CRTM_Surface_type),           DIMENSION(:),   INTENT(IN)     :: Surface       ! M
    TYPE(CRTM_Atmosphere_type),        DIMENSION(:),   INTENT(IN)     :: Atmosphere_TL ! M
    TYPE(CRTM_Surface_type),           DIMENSION(:),   INTENT(IN)     :: Surface_TL    ! M
    TYPE(CRTM_GeometryInfo_type),      DIMENSION(:),   INTENT(IN OUT) :: GeometryInfo  ! M
    TYPE(CRTM_ChannelInfo_type),                       INTENT(IN)     :: ChannelInfo   ! Scalar
    TYPE(CRTM_RTSolution_type),        DIMENSION(:,:), INTENT(IN OUT) :: RTSolution    ! L x M
    TYPE(CRTM_RTSolution_type),        DIMENSION(:,:), INTENT(IN OUT) :: RTSolution_TL ! L x M
    TYPE(CRTM_Options_type), OPTIONAL, DIMENSION(:),   INTENT(IN)     :: Options       ! M
    CHARACTER(*),            OPTIONAL,                 INTENT(OUT)    :: RCS_Id
    CHARACTER(*),            OPTIONAL,                 INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Tangent_Linear(Rank-1)'
    ! Local variables
    CHARACTER(256) :: Message
    CHARACTER(10)  :: Value_Input, Value_Allowed
    LOGICAL :: Options_Present
    INTEGER :: Status_TL
    INTEGER :: m, n_Profiles


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! ----------------------------
    ! Check the number of profiles
    ! ----------------------------

    ! Number of atmospheric profiles.
    n_Profiles = SIZE( Atmosphere )
    IF ( SIZE( Atmosphere_TL ) /= n_Profiles ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent profile dimensionality for '//&
                            'Atmosphere input argument.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Check that the number of profiles is not greater than
    ! MAX_N_PROFILES. This is simply a limit to restrict the
    ! size of the input arrays so they're not TOO big.
    IF ( n_Profiles > MAX_N_PROFILES ) THEN
      Error_Status = FAILURE
      WRITE( Value_Input,   '( i5 )' ) n_Profiles
      WRITE( Value_Allowed, '( i5 )' ) MAX_N_PROFILES
      CALL Display_Message( ROUTINE_NAME, &
                            'Number of passed profiles ('// &
                            TRIM( ADJUSTL( Value_Input ) )// &
                            ') > maximum number of profiles allowed ('// &
                            TRIM( ADJUSTL( Value_Allowed ) )//').', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the profile dimensionality
    ! of the other arguments
    IF ( SIZE( Surface )    /= n_Profiles .OR. &
         SIZE( Surface_TL ) /= n_Profiles    ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent profile dimensionality for '//&
                            'Surface input argument(s).', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    
    IF ( SIZE( GeometryInfo ) /= n_Profiles ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent profile dimensionality for '//&
                            'GeomtryInfo input argument.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    IF ( SIZE( RTSolution,    2 ) /= n_Profiles .OR. &
         SIZE( RTSolution_TL, 2 ) /= n_Profiles      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent profile dimensionality for '//&
                            'RTSolution output argument(s).', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    Options_Present = .FALSE.
    IF ( PRESENT( Options ) ) THEN
      Options_Present = .TRUE.
      IF ( SIZE( Options ) /= n_Profiles ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Inconsistent profile dimensionality for '//&
                              'Options optional input argument.', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF



    !#--------------------------------------------------------------------------#
    !#                           -- PROFILE LOOP --                             #
    !#                                                                          #
    !#  Note that there are two loops, one with the Options argument, and       #
    !#  one without. This is done so that, in the former case, the Options      #
    !#  argument can be indexed with the profile index variable, m, without     #
    !#  an IF(PRESENT(Options)) test inside the profile loop.                   #
    !#--------------------------------------------------------------------------#

    Options_Check: IF ( Options_Present ) THEN


      ! -------------------------
      ! Loop for Options argument
      ! -------------------------

      DO m = 1, n_Profiles
        Status_TL = CRTM_Tangent_Linear_scalar( Atmosphere(m),          &  ! Input, Scalar
                                                Surface(m),             &  ! Input, Scalar
                                                Atmosphere_TL(m),       &  ! Input, Scalar
                                                Surface_TL(m),          &  ! Input, Scalar
                                                GeometryInfo(m),        &  ! Input, Scalar
                                                ChannelInfo,            &  ! Input, Scalar
                                                RTSolution(:,m),        &  ! Output, L   
                                                RTSolution_TL(:,m),     &  ! Output, L   
                                                Options=Options(m),     &  ! Optional input, Scalar
                                                Message_Log=Message_Log )  ! Error messaging
        IF ( Status_TL /= SUCCESS ) THEN
          Error_Status = Status_TL
          IF ( Error_Status == FAILURE ) THEN
            WRITE( Message, '( "Error occured in CRTM_Tangent_Linear(Scalar,", &
                              &"with Options) for profile #", i5 )' ) m
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
            RETURN
          END IF
        END IF
      END DO


    ELSE


      ! ----------------------------
      ! Loop for no Options argument
      ! ----------------------------

      DO m = 1, n_Profiles
        Status_TL = CRTM_Tangent_Linear_scalar( Atmosphere(m),          &  ! Input, Scalar
                                                Surface(m),             &  ! Input, Scalar
                                                Atmosphere_TL(m),       &  ! Input, Scalar
                                                Surface_TL(m),          &  ! Input, Scalar
                                                GeometryInfo(m),        &  ! Input, Scalar
                                                ChannelInfo,            &  ! Input, Scalar
                                                RTSolution(:,m),        &  ! Output, L   
                                                RTSolution_TL(:,m),     &  ! Output, L
                                                Message_Log=Message_Log )  ! Error messaging
        IF ( Status_TL /= SUCCESS ) THEN
          Error_Status = Status_TL
          IF ( Error_Status == FAILURE ) THEN
            WRITE( Message, '( "Error occured in CRTM_Tangent_Linear(Scalar,", &
                              &"no Options) for profile #", i5 )' ) m
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
            RETURN
          END IF
        END IF
      END DO

    END IF Options_Check

  END FUNCTION CRTM_Tangent_Linear_rank1



  !#----------------------------------------------------------------------------#
  !#----------------------------------------------------------------------------#
  !#              -- SCALAR (SINGLE PROFILE) SPECIFIC FUNCTION --               #
  !#----------------------------------------------------------------------------#
  !#----------------------------------------------------------------------------#

  FUNCTION CRTM_Tangent_Linear_scalar( Atmosphere,    &  ! Input, Scalar
                                       Surface,       &  ! Input, Scalar
                                       Atmosphere_TL, &  ! Input, Scalar
                                       Surface_TL,    &  ! Input, Scalar
                                       GeometryInfo,  &  ! Input, Scalar
                                       ChannelInfo,   &  ! Input, Scalar  
                                       RTSolution,    &  ! Output, L   
                                       RTSolution_TL, &  ! Output, L   
                                       Options,       &  ! Optional FWD input, Scalar
                                       RCS_Id,        &  ! Revision control
                                       Message_Log )  &  ! Error messaging
                                     RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Atmosphere_type),               INTENT(IN)     :: Atmosphere
    TYPE(CRTM_Surface_type),                  INTENT(IN)     :: Surface
    TYPE(CRTM_Atmosphere_type),               INTENT(IN)     :: Atmosphere_TL
    TYPE(CRTM_Surface_type),                  INTENT(IN)     :: Surface_TL
    TYPE(CRTM_GeometryInfo_type),             INTENT(IN OUT) :: GeometryInfo
    TYPE(CRTM_ChannelInfo_type),              INTENT(IN)     :: ChannelInfo
    TYPE(CRTM_RTSolution_type), DIMENSION(:), INTENT(IN OUT) :: RTSolution    ! L
    TYPE(CRTM_RTSolution_type), DIMENSION(:), INTENT(IN OUT) :: RTSolution_TL ! L
    TYPE(CRTM_Options_type), OPTIONAL,        INTENT(IN)     :: Options
    CHARACTER(*),            OPTIONAL,        INTENT(OUT)    :: RCS_Id
    CHARACTER(*),            OPTIONAL,        INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Tangent_Linear(Scalar)'
    INTEGER,      PARAMETER :: FW=1, TL=2  ! AllocStatus indices
    ! Local variables
    CHARACTER(256) :: Message
    LOGICAL :: User_Emissivity
    LOGICAL :: User_Direct_Reflectivity
    INTEGER :: Status_FWD, Status_TL
    INTEGER :: l, n_Full_Streams, n_Layers
    INTEGER, DIMENSION(2,6) :: AllocStatus
    TYPE(CRTM_Predictor_type)     :: Predictor,      Predictor_TL
    TYPE(CRTM_AtmAbsorption_type) :: AtmAbsorption,  AtmAbsorption_TL
    TYPE(CRTM_AtmScatter_type)    :: AerosolScatter, AerosolScatter_TL
    TYPE(CRTM_AtmScatter_type)    :: CloudScatter,   CloudScatter_TL
    TYPE(CRTM_AtmScatter_type)    :: AtmOptics,      AtmOptics_TL
    TYPE(CRTM_SfcOptics_type)     :: SfcOptics,      SfcOptics_TL
    ! Internal variables
    TYPE(CRTM_APVariables_type) :: APV  ! Predictor
    TYPE(CRTM_CSVariables_type) :: CSV  ! CloudScatter
    TYPE(CRTM_AOVariables_type) :: AOV  ! AtmOptics
    TYPE(CRTM_RTVariables_type) :: RTV  ! RTSolution


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
    ! If no channels, simply return
    IF ( ChannelInfo%n_Channels == 0 ) RETURN
    ! Output array too small
    IF ( SIZE( RTSolution    ) < ChannelInfo%n_Channels .OR. &
         SIZE( RTSolution_TL ) < ChannelInfo%n_Channels      ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Output RTSolution structure arrays too small (", i5, 1x, i5, &
                        &") to hold results for the number of requested channels (", i5, ")" )' ) &
                      SIZE( RTSolution ), SIZE( RTSolution_TL ), ChannelInfo%n_Channels
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! ---------------------------------------------
    ! Check the optional Options structure argument
    ! ---------------------------------------------
    ! Default action is NOT to use user specified Options
    User_Emissivity = .FALSE.
    !.... other User_XXX flags as added.

    ! Check the Options argument
    Options_Present: IF ( PRESENT( Options ) ) THEN

      ! Check if the supplied emissivity should be used
      Check_Emissivity: IF ( Options%Emissivity_Switch == SET ) THEN
        ! Are the channel dimensions consistent
        IF ( Options%n_Channels < ChannelInfo%n_Channels ) THEN
          Error_Status = FAILURE
          WRITE( Message, '( "Input Options channel dimension (", i5, ") is less ", &
                            &"than the number of requested channels (",i5, ")" )' ) &
                          Options%n_Channels, ChannelInfo%n_Channels
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log=Message_Log )
          RETURN
        END IF
        ! Set to use the supplied emissivity
        User_Emissivity = .TRUE.
        ! Check if the supplied direct reflectivity should be used
        User_Direct_Reflectivity = .FALSE.
        IF ( Options%Direct_Reflectivity_Switch == SET ) User_Direct_Reflectivity = .TRUE.
      END IF Check_Emissivity
    END IF Options_Present


    ! ------------------------
    ! Compute derived geometry
    ! ------------------------
    Error_Status = CRTM_Compute_GeometryInfo( GeometryInfo, &
                                              Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error computing derived GeometryInfo components', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- ALLOCATE ALL LOCAL STRUCTURES --                  #
    !#--------------------------------------------------------------------------#
    ! The Predictor and AtmAbsorption structures
    AllocStatus(FW,1)=CRTM_Allocate_Predictor( Atmosphere%n_Layers   , &  ! Input
                                               MAX_N_PREDICTORS      , &  ! Input
                                               MAX_N_ABSORBERS       , &  ! Input
                                               Predictor             , &  ! Output
                                               Message_Log=Message_Log )  ! Error messaging
    AllocStatus(TL,1)=CRTM_Allocate_Predictor( Atmosphere%n_Layers   , &  ! Input
                                               MAX_N_PREDICTORS      , &  ! Input
                                               MAX_N_ABSORBERS       , &  ! Input
                                               Predictor_TL          , &  ! Output
                                               Message_Log=Message_Log )  ! Error messaging
    AllocStatus(FW,2)=CRTM_Allocate_AtmAbsorption( Atmosphere%n_Layers   , &  ! Input
                                                   AtmAbsorption         , &  ! Output
                                                   Message_Log=Message_Log )  ! Error messaging
    AllocStatus(TL,2)=CRTM_Allocate_AtmAbsorption( Atmosphere%n_Layers   , &  ! Input
                                                   AtmAbsorption_TL      , &  ! Output
                                                   Message_Log=Message_Log )  ! Error messaging
    ! The CloudScatter structures
    AllocStatus(FW,3)=CRTM_Allocate_AtmScatter( Atmosphere%n_Layers,    &  ! Input
                                                MAX_N_LEGENDRE_TERMS,   &  ! Input
                                                MAX_N_PHASE_ELEMENTS,   &  ! Input
                                                CloudScatter,           &  ! Output
                                                Message_Log=Message_Log )  ! Error messaging
    AllocStatus(TL,3)=CRTM_Allocate_AtmScatter( Atmosphere%n_Layers,    &  ! Input
                                                MAX_N_LEGENDRE_TERMS,   &  ! Input
                                                MAX_N_PHASE_ELEMENTS,   &  ! Input
                                                CloudScatter_TL,        &  ! Output
                                                Message_Log=Message_Log )  ! Error messaging
    ! The AerosolScatter structures
    AllocStatus(FW,4)=CRTM_Allocate_AtmScatter( Atmosphere%n_Layers,    &  ! Input
                                                MAX_N_LEGENDRE_TERMS,   &  ! Input
                                                MAX_N_PHASE_ELEMENTS,   &  ! Input
                                                AerosolScatter,         &  ! Output
                                                Message_Log=Message_Log )  ! Error messaging
    AllocStatus(TL,4)=CRTM_Allocate_AtmScatter( Atmosphere%n_Layers,    &  ! Input
                                                MAX_N_LEGENDRE_TERMS,   &  ! Input
                                                MAX_N_PHASE_ELEMENTS,   &  ! Input
                                                AerosolScatter_TL,      &  ! Output
                                                Message_Log=Message_Log )  ! Error messaging
    ! The AtmOptics structure
    AllocStatus(FW,5)=CRTM_Allocate_AtmScatter( Atmosphere%n_Layers,    &  ! Input
                                                MAX_N_LEGENDRE_TERMS,   &  ! Input
                                                MAX_N_PHASE_ELEMENTS,   &  ! Input
                                                AtmOptics,              &  ! Output
                                                Message_Log=Message_Log )  ! Error messaging
    AllocStatus(TL,5)=CRTM_Allocate_AtmScatter( Atmosphere%n_Layers,    &  ! Input
                                                MAX_N_LEGENDRE_TERMS,   &  ! Input
                                                MAX_N_PHASE_ELEMENTS,   &  ! Input
                                                AtmOptics_TL,           &  ! Output
                                                Message_Log=Message_Log )  ! Error messaging
    ! The SfcOptics structure
    AllocStatus(FW,6)=CRTM_Allocate_SfcOptics( MAX_N_ANGLES,           &  ! Input
                                               MAX_N_STOKES,           &  ! Input
                                               SfcOptics,              &  ! Output
                                               Message_Log=Message_Log )  ! Error messaging
    AllocStatus(TL,6)=CRTM_Allocate_SfcOptics( MAX_N_ANGLES,           &  ! Input
                                               MAX_N_STOKES,           &  ! Input
                                               SfcOptics_TL,           &  ! Output
                                               Message_Log=Message_Log )  ! Error messaging
    IF ( ANY(AllocStatus /= SUCCESS) ) THEN
      Error_Status=FAILURE
      CALL DIsplay_Message( ROUTINE_NAME, &
                            'Error allocating local data structures', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! --------------------------
    ! Preprocess some input data
    ! --------------------------
    ! Average surface skin temperature for multi-surface types
    CALL CRTM_Compute_SurfaceT( Surface, SfcOptics )
    CALL CRTM_Compute_SurfaceT_TL( Surface, Surface_TL, SfcOptics_TL )


    ! ------------------------------------------
    ! Compute predictors for AtmAbsorption calcs
    ! ------------------------------------------
    CALL CRTM_Compute_Predictors( Atmosphere  , &  ! Input
                                  GeometryInfo, &  ! Input
                                  Predictor   , &  ! Output
                                  APV           )  ! Internal variable output
    CALL CRTM_Compute_Predictors_TL( Atmosphere,    &  ! Input
                                     Predictor,     &  ! Input
                                     Atmosphere_TL, &  ! Input
                                     GeometryInfo,  &  ! Input
                                     Predictor_TL,  &  ! Output
                                     APV            )  ! Internal variable input



    !#--------------------------------------------------------------------------#
    !#                           -- CHANNEL LOOP --                             #
    !#--------------------------------------------------------------------------#

    Channel_Loop: DO l = 1, ChannelInfo%n_Channels


      ! --------------------------------
      ! Compute the layer optical depths
      ! due to gaseous absorption
      ! --------------------------------
      CALL CRTM_Compute_AtmAbsorption( ChannelInfo%Channel_Index(l), &  ! Input
                                       Predictor                   , &  ! Input
                                       AtmAbsorption                 )  ! Output
      CALL CRTM_Compute_AtmAbsorption_TL( ChannelInfo%Channel_Index(l), &  ! Input
                                          Predictor                   , &  ! Input
                                          Predictor_TL                , &  ! Input
                                          AtmAbsorption_TL              )  ! Output


      ! ---------------------------------------------------------------
      ! Determine the number of streams (n_Full_Streams) in up+downward
      ! directions. Currently, n_Full_Streams is determined from the
      ! cloud parameters only. It will also use the aerosol parameters 
      ! when aerosol scattering is included.
      ! ---------------------------------------------------------------
      CALL CRTM_Compute_n_Streams( Atmosphere, &
                                   ChannelInfo%Channel_Index(l), &
                                   n_Full_Streams, &
                                   RTSolution(l) )

      ! Transfer the number of streams
      ! to all the scattering structures
      AtmOptics%n_Legendre_Terms      = n_Full_Streams
      AtmOptics_TL%n_Legendre_Terms   = n_Full_Streams
      CloudScatter%n_Legendre_Terms   = n_Full_Streams
      AerosolScatter%n_Legendre_Terms = n_Full_Streams


      ! -----------------------------------------------------------
      ! Compute the cloud particle absorption/scattering properties
      ! -----------------------------------------------------------
      IF( Atmosphere%n_Clouds > 0 ) THEN

        ! Forward
        Status_FWD = CRTM_Compute_CloudScatter( Atmosphere,                   &  ! Input
                                                ChannelInfo%Channel_Index(l), &  ! Input
                                                CloudScatter,                 &  ! Output
                                                CSV,                          &  ! Internal variable output
                                                Message_Log=Message_Log     )  ! Error messaging

        ! Tangent-linear
        Status_TL = CRTM_Compute_CloudScatter_TL( Atmosphere,                   &  ! Input
                                                  CloudScatter,                 &  ! Input 
                                                  Atmosphere_TL,                &  ! Input
                                                  ChannelInfo%Channel_Index(l), &  ! Input
                                                  CloudScatter_TL,              &  ! Output
                                                  CSV,                          &  ! Internal variable input
                                                  Message_Log=Message_Log     )  ! Error messaging

        IF ( Status_FWD /= SUCCESS .OR. Status_TL /= SUCCESS ) THEN
          Error_Status = FAILURE
          WRITE( Message, '( "Error computing CloudScatter for ", a, &
                            &", channel ", i4 )' ) &
                          TRIM( ChannelInfo%Sensor_Descriptor(l) ), &
                          ChannelInfo%Sensor_Channel(l)
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log=Message_Log )
          RETURN
        END IF

      ENDIF


      ! ----------------------------------------------------
      ! Compute the aerosol absorption/scattering properties
      ! ----------------------------------------------------
      IF ( Atmosphere%n_Aerosols > 0 ) THEN

        ! Forward
        Status_FWD = CRTM_Compute_AerosolScatter( Atmosphere,                   &  ! Input
                                                  GeometryInfo,                 &  ! Input
                                                  ChannelInfo%Channel_Index(l), &  ! Input
                                                  AerosolScatter,               &  ! Output
                                                  Message_Log=Message_Log     )  ! Error messaging

        ! Tangent-linear
        Status_TL = CRTM_Compute_AerosolScatter_TL( Atmosphere,                   &  ! Input
                                                    AerosolScatter,               &  ! Input
                                                    Atmosphere_TL,                &  ! Input
                                                    GeometryInfo,                 &  ! Input
                                                    ChannelInfo%Channel_Index(l), &  ! Input
                                                    AerosolScatter_TL,            &  ! Output
                                                    Message_Log=Message_Log     )  ! Error messaging

        IF ( Status_FWD /= SUCCESS .OR. Status_TL /= SUCCESS ) THEN
          Error_Status = FAILURE
          WRITE( Message, '( "Error computing AerosolScatter for ", a, ", channel ", i4 )' ) &
                          TRIM( ChannelInfo%Sensor_Descriptor(l) ), &
                          ChannelInfo%Sensor_Channel(l)
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log=Message_Log )
          RETURN
!            OR
!          CYCLE Channel_Loop
        END IF

      END IF


      ! ---------------------------------------------------
      ! Compute the combined atmospheric optical properties
      ! ---------------------------------------------------
      ! Forward
      CALL CRTM_Combine_AtmOptics( AtmAbsorption,  & ! Input
                                   CloudScatter,   & ! Input
                                   AerosolScatter, & ! Input
                                   AtmOptics,      & ! Output
                                   AOV             ) ! Internal variable output


      ! Tangent-linear
      CALL CRTM_Combine_AtmOptics_TL( AtmAbsorption,     & ! FWD Input
                                      CloudScatter,      & ! FWD Input
                                      AerosolScatter,    & ! FWD Input
                                      AtmOptics,         & ! FWD Input
                                      AtmAbsorption_TL,  & ! TL  Input
                                      CloudScatter_TL,   & ! TL  Input
                                      AerosolScatter_TL, & ! TL  Input
                                      AtmOptics_TL,      & ! TL  Output
                                      AOV                ) ! Internal variable input


      ! -------------------------------------
      ! Fill the SfcOptics structures for the
      ! optional emissivity input case.
      ! -------------------------------------
      ! Indicate SfcOptics ARE to be computed
      SfcOptics%Compute_Switch = SET

      ! Change SfcOptics emissivity/reflectivity
      ! contents/computation status
      IF ( User_Emissivity ) THEN

        SfcOptics%Compute_Switch  = NOT_SET

        SfcOptics%Emissivity(1,1)       = Options%Emissivity(l)
        SfcOptics%Reflectivity(1,1,1,1) = ONE - Options%Emissivity(l)

        IF ( User_Direct_Reflectivity ) THEN
          SfcOptics%Direct_Reflectivity(1,1) = Options%Direct_Reflectivity(l)
        ELSE
          SfcOptics%Direct_Reflectivity(1,1) = SfcOptics%Reflectivity(1,1,1,1)
        END IF

      END IF


      ! ------------------------------------
      ! Solve the radiative transfer problem
      ! ------------------------------------
      ! Forward model
      Error_Status = CRTM_Compute_RTSolution( Atmosphere,                   &  ! Input
                                              Surface,                      &  ! Input
                                              AtmOptics,                    &  ! Input
                                              SfcOptics,                    &  ! Input
                                              GeometryInfo,                 &  ! Input
                                              ChannelInfo%Channel_Index(l), &  ! Input
                                              RTSolution(l),                &  ! Output
                                              RTV,                          &  ! Internal variable output
                                              Message_Log=Message_Log     )  ! Error messaging

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error computing CRTM_Compute_RTSolution for ", a, &
                          &", channel ", i4 )' ) &
                        TRIM( ChannelInfo%Sensor_Descriptor(l) ), &
                        ChannelInfo%Sensor_Channel(l)
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF

      ! Tangent-linear models
      Error_Status = CRTM_Compute_RTSolution_TL( Atmosphere,                   &  ! Input
                                                 Surface,                      &  ! Input
                                                 AtmOptics,                    &  ! Input
                                                 SfcOptics,                    &  ! Input/Output
                                                 RTSolution(l),                &  ! Input
                                                 Atmosphere_TL,                &  ! Input
                                                 Surface_TL,                   &  ! Input
                                                 AtmOptics_TL,                 &  ! Input
                                                 SfcOptics_TL,                 &  ! Input
                                                 GeometryInfo,                 &  ! Input
                                                 ChannelInfo%Channel_Index(l), &  ! Input
                                                 RTSolution_TL(l),             &  ! Output
                                                 RTV,                          &  ! Internal variable input
                                                 Message_Log=Message_Log     )  ! Error messaging
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error computing CRTM_Compute_RTSolution_TL for ", a, &
                          &", channel ", i4 )' ) &
                        TRIM( ChannelInfo%Sensor_Descriptor(l) ), &
                        ChannelInfo%Sensor_Channel(l)
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF

    END DO Channel_Loop


    ! ---------------------------
    ! Deallocate local structures
    ! ---------------------------
    AllocStatus(TL,6)=CRTM_Destroy_SfcOptics( SfcOptics_TL )
    AllocStatus(FW,6)=CRTM_Destroy_SfcOptics( SfcOptics )
    AllocStatus(TL,5)=CRTM_Destroy_AtmScatter( AtmOptics_TL )
    AllocStatus(FW,5)=CRTM_Destroy_AtmScatter( AtmOptics )
    AllocStatus(TL,4)=CRTM_Destroy_AtmScatter( AerosolScatter_TL )
    AllocStatus(FW,4)=CRTM_Destroy_AtmScatter( AerosolScatter )
    AllocStatus(TL,3)=CRTM_Destroy_AtmScatter( CloudScatter_TL )
    AllocStatus(FW,3)=CRTM_Destroy_AtmScatter( CloudScatter )
    AllocStatus(TL,2)=CRTM_Destroy_AtmAbsorption( AtmAbsorption_TL )
    AllocStatus(FW,2)=CRTM_Destroy_AtmAbsorption( AtmAbsorption )
    AllocStatus(TL,1)=CRTM_Destroy_Predictor( Predictor_TL )
    AllocStatus(FW,1)=CRTM_Destroy_Predictor( Predictor )
    IF ( ANY(AllocStatus /= SUCCESS ) ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating local structures', &
                            Error_Status, &
                            Message_Log=Message_Log )
    END IF

  END FUNCTION CRTM_Tangent_Linear_scalar

END MODULE CRTM_Tangent_Linear_Module
