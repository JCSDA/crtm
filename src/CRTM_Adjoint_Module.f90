!
! CRTM_Adjoint_Module
!
! Module containing the CRTM adjoint model function.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 28-Jan-2005
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_Adjoint_Module


  ! ------------
  ! Module usage
  ! ------------
  USE Type_Kinds,               ONLY: fp=>fp_kind
  USE Message_Handler,          ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE CRTM_Parameters,          ONLY: SET, NOT_SET, ZERO, ONE, &
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
                                      CRTM_Compute_Predictors_AD
  USE CRTM_AtmAbsorption,       ONLY: CRTM_AtmAbsorption_type      , &
                                      CRTM_Allocate_AtmAbsorption  , &
                                      CRTM_Destroy_AtmAbsorption   , &
                                      CRTM_Compute_AtmAbsorption   , &
                                      CRTM_Compute_AtmAbsorption_AD
  USE CRTM_AtmScatter_Define,   ONLY: CRTM_AtmScatter_type    , &
                                      CRTM_Allocate_AtmScatter, &
                                      CRTM_Destroy_AtmScatter
  USE CRTM_AerosolScatter,      ONLY: CRTM_Compute_AerosolScatter   , &
                                      CRTM_Compute_AerosolScatter_AD
  USE CRTM_CloudScatter,        ONLY: CRTM_CSVariables_type       , &
                                      CRTM_Compute_CloudScatter   , &
                                      CRTM_Compute_CloudScatter_AD
  USE CRTM_AtmOptics,           ONLY: CRTM_AOVariables_type    , &
                                      CRTM_Combine_AtmOptics   , &
                                      CRTM_Combine_AtmOptics_AD
  USE CRTM_SfcOptics,           ONLY: CRTM_SfcOptics_type     , &
                                      CRTM_Allocate_SfcOptics , &
                                      CRTM_Destroy_SfcOptics  , &
                                      CRTM_Compute_SurfaceT   , &
                                      CRTM_Compute_SurfaceT_AD
  USE CRTM_RTSolution,          ONLY: CRTM_RTSolution_type      , &
                                      CRTM_RTVariables_type     , &
                                      CRTM_Compute_n_Streams    , &
                                      CRTM_Compute_RTSolution   , &
                                      CRTM_Compute_RTSolution_AD


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
  PUBLIC :: CRTM_Adjoint


  ! --------------------
  ! Function overloading
  ! --------------------
  INTERFACE CRTM_Adjoint
    MODULE PROCEDURE CRTM_Adjoint_scalar
    MODULE PROCEDURE CRTM_Adjoint_rank1
  END INTERFACE CRTM_Adjoint


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_Adjoint_Module.f90,v 1.10 2006/08/31 17:04:04 frpv Exp $'


CONTAINS


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Adjoint
!
! PURPOSE:
!       Function that calculates the adjoint of top-of-atmosphere (TOA)
!       radiances and brightness temperatures for an input atmospheric
!       profile or profile set and user specified satellites/channels.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Adjoint( Atmosphere,             &  ! FWD Input
!                                    Surface,                &  ! FWD Input
!                                    RTSolution_AD,          &  ! AD  Input
!                                    GeometryInfo,           &  ! Input
!                                    ChannelInfo,            &  ! Input
!                                    Atmosphere_AD,          &  ! AD  Output
!                                    Surface_AD,             &  ! AD  Output
!                                    RTSolution,             &  ! FWD Output
!                                    Options=Options,        &  ! Optional FWD input
!                                    RCS_Id=RCS_Id,          &  ! Revision control
!                                    Message_Log=Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Atmosphere:     Structure containing the Atmosphere data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Scalar
!                                     or
!                                   Rank-1 (M)
!                                   See dimensionality table in COMMENTS below.
!                       ATTRIBUTES: INTENT(IN)
!
!       Surface:        Structure containing the Surface data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Surface_type
!                       DIMENSION:  Same as input Atmosphere argument.
!                                   See dimensionality table in COMMENTS below.
!                       ATTRIBUTES: INTENT(IN)
!
!       RTSolution_AD:  Structure containing the RT solution adjoint inputs.
!                       **NOTE: On EXIT from this function, the contents of
!                               this structure may be modified (e.g. set to
!                               zero.)
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Rank-1 (L)
!                                     or
!                                   Rank-2 (L x M)
!                                   See dimensionality table in COMMENTS below.
!                       ATTRIBUTES: INTENT(IN OUT)
!
!       GeometryInfo:   Structure containing the view geometry
!                       information.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_GeometryInfo_type)
!                       DIMENSION:  Same as input Atmosphere argument
!                                   See dimensionality table in COMMENTS below.
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
!       Atmosphere_AD:  Structure containing the adjoint Atmosphere data.
!                       **NOTE: On ENTRY to this function, the contents of
!                               this structure should be defined (e.g.
!                               initialized to some value based on the
!                               position of this function in the call chain.)
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Same as input Atmosphere argument
!                                   See dimensionality table in COMMENTS below.
!                       ATTRIBUTES: INTENT(IN OUT)
!
!       Surface_AD:     Structure containing the tangent-linear Surface data.
!                       **NOTE: On ENTRY to this function, the contents of
!                               this structure should be defined (e.g.
!                               initialized to some value based on the
!                               position of this function in the call chain.)
!                       UNITS:      N/A
!                       TYPE:       CRTM_Surface_type
!                       DIMENSION:  Same as input Atmosphere argument
!                                   See dimensionality table in COMMENTS below.
!                       ATTRIBUTES: INTENT(IN OUT)
!
!       RTSolution:     Structure containing the solution to the RT equation
!                       for the given inputs.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Same as input RTSolution_AD argument
!                                   See dimensionality table in COMMENTS below.
!                       ATTRIBUTES: INTENT(IN OUT)
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
! SIDE EFFECTS:
!      Note that the input adjoint arguments are modified upon exit, and
!      the output adjoint arguments must be defined upon entry. This is
!      a consequence of the adjoint formulation where, effectively, the
!      chain rule is being used and this function could reside anywhere
!      in the chain of derivative terms.
!
! COMMENTS:
!       - The folowing tables details the input/output argument dimensionality
!         association, where L == n_Channels, M == n_Profiles:
!
!                                             | OPTIONAL |
!                  INPUTS                     |  INPUT   |         OUTPUTS
!                                             |          |
!     Atmosphere  RTSolution_AD  GeometryInfo | Options  | RTSolution    Atmosphere_AD
!      Surface                                |          |                Surface_AD
!  -------------------------------------------+----------+-----------------------------
!       Scalar         L            Scalar    |  Scalar  |     L             Scalar
!                                             |          |
!         M          L x M            M       |    M     |   L x M             M
!
!         Thus one can process either a single profile or multiple profiles.
!         The routines for each specific case above have been overloaded to
!         the generic interface described in the header above.
!
!       - Note that the Options optional structure arguments contain
!         spectral information (e.g. emissivity) that must have the same
!         spectral dimensionality (the "L" dimension) as the RTSolution
!         structures.
!
!       - Note the INTENT on the output RTSolution, Atmosphere_AD, and
!         Surface_AD arguments are IN OUT rather than just OUT. This is
!         necessary because the arguments should be defined upon input.
!         To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  !#----------------------------------------------------------------------------#
  !#----------------------------------------------------------------------------#
  !#                -- RANK-1 (N_PROFILES) SPECIFIC FUNCTION --                 #
  !#----------------------------------------------------------------------------#
  !#----------------------------------------------------------------------------#

  FUNCTION CRTM_Adjoint_rank1( Atmosphere,    &  ! FWD Input, M
                               Surface,       &  ! FWD Input, M
                               RTSolution_AD, &  ! AD  Input, L x M   
                               GeometryInfo,  &  ! Input, M
                               ChannelInfo,   &  ! Input, Scalar  
                               Atmosphere_AD, &  ! AD  Output, M
                               Surface_AD,    &  ! AD  Output, M
                               RTSolution,    &  ! FWD Output, L x M
                               Options,       &  ! Optional FWD input,  M
                               RCS_Id,        &  ! Revision control
                               Message_Log )  &  ! Error messaging
                             RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Atmosphere_type),        DIMENSION(:),   INTENT(IN)     :: Atmosphere    ! M
    TYPE(CRTM_Surface_type),           DIMENSION(:),   INTENT(IN)     :: Surface       ! M
    TYPE(CRTM_RTSolution_type),        DIMENSION(:,:), INTENT(IN OUT) :: RTSolution_AD ! L x M
    TYPE(CRTM_GeometryInfo_type),      DIMENSION(:),   INTENT(IN OUT) :: GeometryInfo  ! M
    TYPE(CRTM_ChannelInfo_type),                       INTENT(IN)     :: ChannelInfo   ! Scalar
    TYPE(CRTM_Atmosphere_type),        DIMENSION(:),   INTENT(IN OUT) :: Atmosphere_AD ! M
    TYPE(CRTM_Surface_type),           DIMENSION(:),   INTENT(IN OUT) :: Surface_AD    ! M
    TYPE(CRTM_RTSolution_type),        DIMENSION(:,:), INTENT(IN OUT) :: RTSolution    ! L x M
    TYPE(CRTM_Options_type), OPTIONAL, DIMENSION(:),   INTENT(IN)     :: Options       ! M
    CHARACTER(*),            OPTIONAL,                 INTENT(OUT)    :: RCS_Id
    CHARACTER(*),            OPTIONAL,                 INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Adjoint(Rank-1)'
    ! Local variables
    CHARACTER(256) :: Message
    CHARACTER( 10 )  :: Value_Input, Value_Allowed
    LOGICAL :: Options_Present
    INTEGER :: Status_AD
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
    IF ( SIZE( Atmosphere_AD ) /= n_Profiles ) THEN
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
         SIZE( Surface_AD ) /= n_Profiles    ) THEN
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
         SIZE( RTSolution_AD, 2 ) /= n_Profiles      ) THEN
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
        Status_AD = CRTM_Adjoint_scalar( Atmosphere(m),          &  ! Input, Scalar
                                         Surface(m),             &  ! Input, Scalar
                                         RTSolution_AD(:,m),     &  ! Input, L   
                                         GeometryInfo(m),        &  ! Input, Scalar
                                         ChannelInfo,            &  ! Input, Scalar
                                         Atmosphere_AD(m),       &  ! Output, Scalar
                                         Surface_AD(m),          &  ! Output, Scalar
                                         RTSolution(:,m),        &  ! Output, L
                                         Options=Options(m),     &  ! Optional input, Scalar
                                         Message_Log=Message_Log )  ! Error messaging
        IF ( Status_AD /= SUCCESS ) THEN
          Error_Status = Status_AD
          IF ( Error_Status == FAILURE ) THEN
            WRITE( Message, '( "Error occured in CRTM_Adjoint(Scalar,", &
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
        Status_AD = CRTM_Adjoint_scalar( Atmosphere(m),          &  ! Input, Scalar
                                         Surface(m),             &  ! Input, Scalar
                                         RTSolution_AD(:,m),     &  ! Input, L   
                                         GeometryInfo(m),        &  ! Input, Scalar
                                         ChannelInfo,            &  ! Input, Scalar
                                         Atmosphere_AD(m),       &  ! Output, Scalar
                                         Surface_AD(m),          &  ! Output, Scalar
                                         RTSolution(:,m),        &  ! Output, L   
                                         Message_Log=Message_Log )  ! Error messaging
        IF ( Status_AD /= SUCCESS ) THEN
          Error_Status = Status_AD
          IF ( Error_Status == FAILURE ) THEN
            WRITE( Message, '( "Error occured in CRTM_Adjoint(Scalar,", &
                              &"no options) for profile #", i5 )' ) m
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
            RETURN
          END IF
        END IF
      END DO

    END IF Options_Check

  END FUNCTION CRTM_Adjoint_rank1



  !#----------------------------------------------------------------------------#
  !#----------------------------------------------------------------------------#
  !#              -- SCALAR (SINGLE PROFILE) SPECIFIC FUNCTION --               #
  !#----------------------------------------------------------------------------#
  !#----------------------------------------------------------------------------#

  FUNCTION CRTM_Adjoint_scalar( Atmosphere,    &  ! FWD Input
                                Surface,       &  ! FWD Input
                                RTSolution_AD, &  ! AD  Input, L   
                                GeometryInfo,  &  ! Input
                                ChannelInfo,   &  ! Input
                                Atmosphere_AD, &  ! AD  Output
                                Surface_AD,    &  ! AD  Output
                                RTSolution,    &  ! FWD Output, L
                                Options,       &  ! Optional FWD input
                                RCS_Id,        &  ! Revision control
                                Message_Log )  &  ! Error messaging
                              RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Atmosphere_type),               INTENT(IN)     :: Atmosphere    ! Scalar
    TYPE(CRTM_Surface_type),                  INTENT(IN)     :: Surface       ! Scalar
    TYPE(CRTM_RTSolution_type), DIMENSION(:), INTENT(IN OUT) :: RTSolution_AD ! L
    TYPE(CRTM_GeometryInfo_type),             INTENT(IN OUT) :: GeometryInfo  ! Scalar
    TYPE(CRTM_ChannelInfo_type),              INTENT(IN)     :: ChannelInfo   ! Scalar 
    TYPE(CRTM_Atmosphere_type),               INTENT(IN OUT) :: Atmosphere_AD ! Scalar
    TYPE(CRTM_Surface_type),                  INTENT(IN OUT) :: Surface_AD    ! Scalar
    TYPE(CRTM_RTSolution_type), DIMENSION(:), INTENT(IN OUT) :: RTSolution    ! L
    TYPE(CRTM_Options_type), OPTIONAL,        INTENT(IN)     :: Options       ! M
    CHARACTER(*),            OPTIONAL,        INTENT(OUT)    :: RCS_Id
    CHARACTER(*),            OPTIONAL,        INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Adjoint(Scalar)'
    INTEGER,      PARAMETER :: FW=1, AD=2  ! AllocStatus indices
    ! Local variables
    CHARACTER(256) :: Message
    LOGICAL :: User_Emissivity
    LOGICAL :: User_Direct_Reflectivity
    INTEGER :: Status_FWD, Status_AD
    INTEGER :: l, n_Full_Streams
    INTEGER, DIMENSION(2,6) :: AllocStatus
    TYPE(CRTM_Predictor_type)     :: Predictor,      Predictor_AD
    TYPE(CRTM_AtmAbsorption_type) :: AtmAbsorption,  AtmAbsorption_AD
    TYPE(CRTM_AtmScatter_type)    :: AerosolScatter, AerosolScatter_AD
    TYPE(CRTM_AtmScatter_type)    :: CloudScatter,   CloudScatter_AD
    TYPE(CRTM_AtmScatter_type)    :: AtmOptics,      AtmOptics_AD 
    TYPE(CRTM_SfcOPtics_type)     :: SfcOptics,      SfcOptics_AD
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
         SIZE( RTSolution_AD ) < ChannelInfo%n_Channels      ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Output RTSolution structure arrays too small (", i5, 1x, i5, &
                        &") to hold results for the number of requested channels (", i5, ")" )' ) &
                      SIZE( RTSolution ), SIZE( RTSolution_AD ), ChannelInfo%n_Channels
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
    AllocStatus(AD,1)=CRTM_Allocate_Predictor( Atmosphere%n_Layers   , &  ! Input
                                               MAX_N_PREDICTORS      , &  ! Input
                                               MAX_N_ABSORBERS       , &  ! Input
                                               Predictor_AD          , &  ! Output
                                               Message_Log=Message_Log )  ! Error messaging
    AllocStatus(FW,2)=CRTM_Allocate_AtmAbsorption( Atmosphere%n_Layers   , &  ! Input
                                                   AtmAbsorption         , &  ! Output
                                                   Message_Log=Message_Log )  ! Error messaging
    AllocStatus(AD,2)=CRTM_Allocate_AtmAbsorption( Atmosphere%n_Layers   , &  ! Input
                                                   AtmAbsorption_AD      , &  ! Output
                                                   Message_Log=Message_Log )  ! Error messaging
    ! The CloudScatter structures
    AllocStatus(FW,3)=CRTM_Allocate_AtmScatter( Atmosphere%n_Layers,    &  ! Input
                                                MAX_N_LEGENDRE_TERMS,   &  ! Input
                                                MAX_N_PHASE_ELEMENTS,   &  ! Input
                                                CloudScatter,           &  ! Output
                                                Message_Log=Message_Log )  ! Error messaging
    AllocStatus(AD,3)=CRTM_Allocate_AtmScatter( Atmosphere%n_Layers,    &  ! Input
                                                MAX_N_LEGENDRE_TERMS,   &  ! Input
                                                MAX_N_PHASE_ELEMENTS,   &  ! Input
                                                CloudScatter_AD,        &  ! Output
                                                Message_Log=Message_Log )  ! Error messaging
    ! The AerosolScatter structures
    AllocStatus(FW,4)=CRTM_Allocate_AtmScatter( Atmosphere%n_Layers,    &  ! Input
                                                MAX_N_LEGENDRE_TERMS,   &  ! Input
                                                MAX_N_PHASE_ELEMENTS,   &  ! Input
                                                AerosolScatter,         &  ! Output
                                                Message_Log=Message_Log )  ! Error messaging
    AllocStatus(AD,4)=CRTM_Allocate_AtmScatter( Atmosphere%n_Layers,    &  ! Input
                                                MAX_N_LEGENDRE_TERMS,   &  ! Input
                                                MAX_N_PHASE_ELEMENTS,   &  ! Input
                                                AerosolScatter_AD,      &  ! Output
                                                Message_Log=Message_Log )  ! Error messaging
    ! The AtmOptics structure
    AllocStatus(FW,5)=CRTM_Allocate_AtmScatter( Atmosphere%n_Layers,    &  ! Input
                                                MAX_N_LEGENDRE_TERMS,   &  ! Input
                                                MAX_N_PHASE_ELEMENTS,   &  ! Input
                                                AtmOptics,              &  ! Output
                                                Message_Log=Message_Log )  ! Error messaging
    AllocStatus(AD,5)=CRTM_Allocate_AtmScatter( Atmosphere%n_Layers,    &  ! Input
                                                MAX_N_LEGENDRE_TERMS,   &  ! Input
                                                MAX_N_PHASE_ELEMENTS,   &  ! Input
                                                AtmOptics_AD,           &  ! Output
                                                Message_Log=Message_Log )  ! Error messaging
    ! The SfcOptics structure
    AllocStatus(FW,6)=CRTM_Allocate_SfcOptics( MAX_N_ANGLES,           &  ! Input
                                               MAX_N_STOKES,           &  ! Input
                                               SfcOptics,              &  ! Output
                                               Message_Log=Message_Log )  ! Error messaging
    AllocStatus(AD,6)=CRTM_Allocate_SfcOptics( MAX_N_ANGLES,           &  ! Input
                                               MAX_N_STOKES,           &  ! Input
                                               SfcOptics_AD,           &  ! Output
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


    ! ------------------------------------------
    ! Compute predictors for AtmAbsorption calcs
    ! ------------------------------------------
    CALL CRTM_Compute_Predictors( Atmosphere  , &  ! Input
                                  GeometryInfo, &  ! Input
                                  Predictor   , &  ! Output
                                  APV           )  ! Internal variable output


    !#--------------------------------------------------------------------------#
    !#                           -- CHANNEL LOOP --                             #
    !#--------------------------------------------------------------------------#

    Channel_Loop: DO l = 1, ChannelInfo%n_Channels



      !#------------------------------------------------------------------------#
      !#                        -- FORWARD CALCULATIONS --                      #
      !#------------------------------------------------------------------------#

      ! --------------------------------
      ! Compute the layer optical depths
      ! due to gaseous absorption
      ! --------------------------------
      CALL CRTM_Compute_AtmAbsorption( ChannelInfo%Channel_Index(l), &  ! Input
                                       Predictor                   , &  ! Input
                                       AtmAbsorption                 )  ! Output


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
      AtmOptics_AD%n_Legendre_Terms   = n_Full_Streams
      CloudScatter%n_Legendre_Terms   = n_Full_Streams
      AerosolScatter%n_Legendre_Terms = n_Full_Streams


      ! -----------------------------------------------------------
      ! Compute the cloud particle absorption/scattering properties
      ! -----------------------------------------------------------
      IF( Atmosphere%n_Clouds > 0 ) THEN
        Status_FWD = CRTM_Compute_CloudScatter( Atmosphere,                   &  ! Input
                                                ChannelInfo%Channel_Index(l), &  ! Input
                                                CloudScatter,                 &  ! Output
                                                CSV,                          &  ! Internal variable output
                                                Message_Log=Message_Log       )  ! Error messaging
        IF ( Status_FWD /= SUCCESS ) THEN
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
        Status_FWD = CRTM_Compute_AerosolScatter( Atmosphere,                   &  ! Input
                                                  GeometryInfo,                 &  ! Input
                                                  ChannelInfo%Channel_Index(l), &  ! Input
                                                  AerosolScatter,               &  ! Output
                                                  Message_Log=Message_Log       )  ! Error messaging
        IF ( Status_FWD /= SUCCESS ) THEN
          Error_Status = FAILURE
          WRITE( Message, '( "Error computing AerosolScatter for ", a, ", channel ", i4 )' ) &
                          TRIM( ChannelInfo%Sensor_Descriptor(l) ), &
                          ChannelInfo%Sensor_Channel(l)
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log=Message_Log )
          RETURN
        END IF
      END IF


      ! ---------------------------------------------------
      ! Compute the combined atmospheric optical properties
      ! ---------------------------------------------------
      CALL CRTM_Combine_AtmOptics( AtmAbsorption,  & ! Input
                                   CloudScatter,   & ! Input
                                   AerosolScatter, & ! Input
                                   AtmOptics,      & ! Output
                                   AOV             ) ! Internal variable output

                                  
      ! ---------------------------------------------
      ! Fill the SfcOptics structure for the optional
      ! emissivity input case.
      ! ---------------------------------------------
      ! Indicate SfcOptics ARE to be computed
      SfcOptics%Compute_Switch = SET

      ! Change FWD SfcOptics emissivity/reflectivity
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
      Error_Status = CRTM_Compute_RTSolution( Atmosphere,                   &  ! Input
                                              Surface,                      &  ! Input
                                              AtmOptics,                    &  ! Input
                                              SfcOptics,                    &  ! Input
                                              GeometryInfo,                 &  ! Input
                                              ChannelInfo%Channel_Index(l), &  ! Input
                                              RTSolution(l),                &  ! Output
                                              RTV,                          &  ! Internal variable output
                                              Message_Log=Message_Log       )  ! Error messaging
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



      !#------------------------------------------------------------------------#
      !#                        -- ADJOINT CALCULATIONS --                      #
      !#------------------------------------------------------------------------#

      ! --------------------------------------------------
      ! Reinitialise profile independent adjoint variables
      ! --------------------------------------------------
      AtmOptics_AD%Phase_Coefficient     = ZERO
      AtmOptics_AD%Asymmetry_Factor      = ZERO
      AtmOptics_AD%Delta_Truncation      = ZERO
      AtmOptics_AD%Optical_Depth         = ZERO
      AtmOptics_AD%Single_Scatter_Albedo = ZERO


      ! -------------------------------------
      ! The adjoint of the radiative transfer
      ! -------------------------------------
      Status_AD = CRTM_Compute_RTSolution_AD( Atmosphere,                   &  ! FWD Input
                                              Surface,                      &  ! FWD Input
                                              AtmOptics,                    &  ! FWD Input
                                              SfcOptics,                    &  ! FWD Input
                                              RTSolution(l),                &  ! FWD Input
                                              RTSolution_AD(l),             &  ! AD  Input
                                              GeometryInfo,                 &  ! Input
                                              ChannelInfo%Channel_Index(l), &  ! Input
                                              Atmosphere_AD,                &  ! AD Output
                                              Surface_AD,                   &  ! AD Output
                                              AtmOptics_AD,                 &  ! AD Output
                                              SfcOptics_AD,                 &  ! AD Output
                                              RTV,                          &  ! Internal variable input
                                              Message_Log=Message_Log     )  ! Error messaging
      IF ( Status_AD /= SUCCESS ) THEN
        Error_Status = FAILURE
        CALL DIsplay_Message( ROUTINE_NAME, &
                              'Error in CRTM_Compute_RTSolution_AD', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF


      ! ------------------------------------------------------------------
      ! Compute the adjoint of the combined atmospheric optical properties
      ! ------------------------------------------------------------------
      CALL CRTM_Combine_AtmOptics_AD( AtmAbsorption,     &  ! FWD Input
                                      CloudScatter,      &  ! FWD Input
                                      AerosolScatter,    &  ! FWD Input
                                      AtmOptics,         &  ! FWD Input
                                      AtmOptics_AD,      &  ! AD Input
                                      AtmAbsorption_AD,  &  ! AD Output
                                      CloudScatter_AD,   &  ! AD Output
                                      AerosolScatter_AD, &  ! AD Output
                                      AOV                )  ! Internal variable input


      ! ------------------------------------------------------------
      ! Compute the adjoint aerosol absorption/scattering properties
      ! ------------------------------------------------------------
      IF ( Atmosphere%n_Aerosols > 0 ) THEN
        Status_AD = CRTM_Compute_AerosolScatter_AD( Atmosphere,                   &  ! Input
                                                    AerosolScatter,               &  ! Input
                                                    AerosolScatter_AD,            &  ! Input
                                                    GeometryInfo,                 &  ! Input
                                                    ChannelInfo%Channel_Index(l), &  ! Input
                                                    Atmosphere_AD,                &  ! In/Output
                                                    Message_Log=Message_Log       )  ! Error messaging
        IF ( Status_AD /= SUCCESS ) THEN
          Error_Status = FAILURE
          CALL DIsplay_Message( ROUTINE_NAME, &
                                'Error in CRTM_Compute_AerosolScatter_AD', &
                                Error_Status, &
                                Message_Log=Message_Log )
          RETURN
        END IF
      END IF


      ! ----------------------------------------------------------
      ! Compute the adjoint cloud absorption/scattering properties
      ! ----------------------------------------------------------
      IF( Atmosphere%n_Clouds > 0 ) THEN
        Status_AD = CRTM_Compute_CloudScatter_AD( Atmosphere,                   &  ! Input
                                                  CloudScatter,                 &  ! Input
                                                  CloudScatter_AD,              &  ! Input
                                                  ChannelInfo%Channel_Index(l), &  ! Input
                                                  Atmosphere_AD,                &  ! In/Output
                                                  CSV,                          &  ! Internal variable input
                                                  Message_Log=Message_Log       )  ! Error messaging
        IF ( Status_AD /= SUCCESS ) THEN
          Error_Status = FAILURE
          CALL DIsplay_Message( ROUTINE_NAME, &
                                'Error in CRTM_Compute_CloudScatter_AD', &
                                Error_Status, &
                                Message_Log=Message_Log )
          RETURN
        END IF
      END IF


      ! ----------------------------------------
      ! Compute the adjoint layer optical depths
      ! due to gaseous absorption
      ! ----------------------------------------
      CALL CRTM_Compute_AtmAbsorption_AD( ChannelInfo%Channel_Index(l), &  ! Input
                                          Predictor,                    &  ! FWD Input
                                          AtmAbsorption_AD,             &  ! AD Input
                                          Predictor_AD                  )  ! AD Output

    END DO Channel_Loop

    ! -------------------------------------
    ! Adjoint of the predictor calculations
    ! -------------------------------------
    CALL CRTM_Compute_Predictors_AD ( Atmosphere,    &  ! FWD Input
                                      Predictor,     &  ! FWD Input
                                      Predictor_AD,  &  ! AD Input
                                      GeometryInfo,  &  ! Input
                                      Atmosphere_AD, &  ! AD Output
                                      APV            )  ! Internal variable input

    ! ---------------------------
    ! Postprocess some input data
    ! ---------------------------
    ! Adjoint of average surface skin temperature for multi-surface types
    CALL CRTM_Compute_SurfaceT_AD( Surface, SfcOptics_AD, Surface_AD )  


    ! ---------------------------
    ! Deallocate local structures
    ! ---------------------------
    AllocStatus(AD,6)=CRTM_Destroy_SfcOptics( SfcOptics_AD )
    AllocStatus(FW,6)=CRTM_Destroy_SfcOptics( SfcOptics )
    AllocStatus(AD,5)=CRTM_Destroy_AtmScatter( AtmOptics_AD )
    AllocStatus(FW,5)=CRTM_Destroy_AtmScatter( AtmOptics )
    AllocStatus(AD,4)=CRTM_Destroy_AtmScatter( AerosolScatter_AD )
    AllocStatus(FW,4)=CRTM_Destroy_AtmScatter( AerosolScatter )
    AllocStatus(AD,3)=CRTM_Destroy_AtmScatter( CloudScatter_AD )
    AllocStatus(FW,3)=CRTM_Destroy_AtmScatter( CloudScatter )
    AllocStatus(AD,2)=CRTM_Destroy_AtmAbsorption( AtmAbsorption_AD )
    AllocStatus(FW,2)=CRTM_Destroy_AtmAbsorption( AtmAbsorption )
    AllocStatus(AD,1)=CRTM_Destroy_Predictor( Predictor_AD )
    AllocStatus(FW,1)=CRTM_Destroy_Predictor( Predictor )
    IF ( ANY(AllocStatus /= SUCCESS ) ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating local structures', &
                            Error_Status, &
                            Message_Log=Message_Log )
    END IF

  END FUNCTION CRTM_Adjoint_scalar

END MODULE CRTM_Adjoint_Module
