!
! CRTM_K_Matrix_Module
!
! Module containing the CRTM K-matrix model function.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 28-Jan-2005
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_K_Matrix_Module


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
  USE CRTM_Atmosphere_Define
  USE CRTM_Surface_Define
  USE CRTM_GeometryInfo_Define
  USE CRTM_ChannelInfo_Define
  USE CRTM_Options_Define
  USE CRTM_AtmAbsorption
  USE CRTM_AerosolScatter
  USE CRTM_CloudScatter
  USE CRTM_SfcOptics
  USE CRTM_AtmOptics
  USE CRTM_RTSolution


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
  PUBLIC :: CRTM_K_Matrix


  ! --------------------
  ! Function overloading
  ! --------------------
  INTERFACE CRTM_K_Matrix
    MODULE PROCEDURE CRTM_K_Matrix_scalar
    MODULE PROCEDURE CRTM_K_Matrix_rank1
  END INTERFACE CRTM_K_Matrix


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER( * ), PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_K_Matrix_Module.f90,v 1.8 2006/05/25 18:35:34 wd20pd Exp $'


CONTAINS


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_K_Matrix
!
! PURPOSE:
!       Function that calculates the K-matrix of top-of-atmosphere (TOA)
!       radiances and brightness temperatures for an input atmospheric
!       profile or profile set and user specified satellites/channels.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_K_Matrix( Atmosphere,               &  ! FWD Input
!                                     Surface,                  &  ! FWD Input
!                                     RTSolution_K,             &  ! K   Input
!                                     GeometryInfo,             &  ! Input
!                                     ChannelInfo,              &  ! Input
!                                     Atmosphere_K,             &  ! K   Output
!                                     Surface_K,                &  ! K   Output
!                                     RTSolution,               &  ! FWD Output
!                                     Options     = Options,    &  ! Optional FWD input,  M
!                                     Options_K   = Options_K,  &  ! Optional K   output, M
!                                     RCS_Id      = RCS_Id,     &  ! Revision control
!                                     Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Atmosphere:     Structure containing the Atmosphere data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Scalar
!                                     or
!                                   Rank-1 (M)
!                                   See dimensionality table in COMMENTS below.
!                       ATTRIBUTES: INTENT( IN )
!
!       Surface:        Structure containing the Surface data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Surface_type
!                       DIMENSION:  Same as input Atmosphere argument.
!                                   See dimensionality table in COMMENTS below.
!                       ATTRIBUTES: INTENT( IN )
!
!       RTSolution_K:   Structure containing the RT solution K-matrix inputs.
!                       **NOTE: On EXIT from this function, the contents of
!                               this structure may be modified (e.g. set to
!                               zero.)
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Rank-1 (L)
!                                     or
!                                   Rank-2 (L x M)
!                                   See dimensionality table in COMMENTS below.
!                       ATTRIBUTES: INTENT( IN OUT )
!
!       GeometryInfo:   Structure containing the view geometry
!                       information.
!                       UNITS:      N/A
!                       TYPE:       TYPE( CRTM_GeometryInfo_type )
!                       DIMENSION:  Same as input Atmosphere argument
!                                   See dimensionality table in COMMENTS below.
!                       ATTRIBUTES: INTENT( IN )
!
!       ChannelInfo:    Structure returned from the CRTM_Init() function
!                       that contains the satellite/sesnor channel index
!                       information.
!                       UNITS:      N/A
!                       TYPE:       TYPE( CRTM_ChannelInfo_type )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Options:        Options structure containing the optional forward model
!                       arguments for the CRTM.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Options_type
!                       DIMENSION:  Same as input Atmosphere structure
!                                   See dimensionality table in COMMENTS below.
!                       ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to the screen.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER( * )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Atmosphere_K:   Structure containing the K-matrix Atmosphere data.
!                       **NOTE: On ENTRY to this function, the contents of
!                               this structure should be defined (e.g.
!                               initialized to some value based on the
!                               position of this function in the call chain.)
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Same as input RTSolution_K argument
!                                   See dimensionality table in COMMENTS below.
!                       ATTRIBUTES: INTENT( IN OUT )
!
!       Surface_K:      Structure containing the tangent-linear Surface data.
!                       **NOTE: On ENTRY to this function, the contents of
!                               this structure should be defined (e.g.
!                               initialized to some value based on the
!                               position of this function in the call chain.)
!                       UNITS:      N/A
!                       TYPE:       CRTM_Surface_type
!                       DIMENSION:  Same as input RTSolution_K argument
!                                   See dimensionality table in COMMENTS below.
!                       ATTRIBUTES: INTENT( IN OUT )
!
!       RTSolution:     Structure containing the solution to the RT equation
!                       for the given inputs.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Same as input RTSolution_K argument
!                                   See dimensionality table in COMMENTS below.
!                       ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Options_K:      Options structure containing the optional K-matrix
!                       model arguments for the CRTM.
!                       **NOTE: Unlike the Atmosphere_K and Surface_K output
!                               K-matrix arguments, on ENTRY to this function,
!                               the contents of this structure are not used in
!                               computing the emissivity/direct reflectivity
!                               adjoints. This structure is simply a container
!                               to hold the adjoint result should the user request
!                               it.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Options_type
!                       DIMENSION:  Same as input Atmosphere structure
!                                   See dimensionality table in COMMENTS below.
!                       ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       RCS_Id:         Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the ERROR_HANDLER module.
!                       If == SUCCESS the computation was sucessful
!                          == FAILURE an unrecoverable error occurred
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! SIDE EFFECTS:
!      Note that the input K-matrix arguments are modified upon exit, and
!      the output K-matrix arguments must be defined upon entry. This is
!      a consequence of the K-matrix formulation where, effectively, the
!      chain rule is being used and this funtion could reside anywhere
!      in the chain of derivative terms.
!
! COMMENTS:
!       - The folowing tables details the input/output argument dimensionality
!         association, where L == n_Channels, M == n_Profiles:
!
!                                            | OPTIONAL |
!                  INPUTS                    |  INPUT   |         OUTPUTS
!                                            |          |
!     Atmosphere  RTSolution_K  GeometryInfo | Options  | RTSolution    Atmosphere_K
!      Surface                               |          |                Surface_K
!  ------------------------------------------+----------+----------------------------
!       Scalar         L           Scalar    |  Scalar  |     L             Scalar
!                                            |          |
!         M          L x M           M       |    M     |   L x M             M
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
!       - Note the INTENT on the output RTSolution, Atmosphere_K, and Surface_K,
!         arguments are IN OUT rather than just OUT. This is necessary because
!         the arguments should be defined upon input. To prevent memory leaks,
!         the IN OUT INTENT is a must.
!
!S-
!--------------------------------------------------------------------------------


  !#----------------------------------------------------------------------------#
  !#----------------------------------------------------------------------------#
  !#                -- RANK-1 (N_PROFILES) SPECIFIC FUNCTION --                 #
  !#----------------------------------------------------------------------------#
  !#----------------------------------------------------------------------------#

  FUNCTION CRTM_K_Matrix_rank1( Atmosphere,    &  ! FWD Input, M
                                Surface,       &  ! FWD Input, M
                                RTSolution_K,  &  ! K   Input, L x M   
                                GeometryInfo,  &  ! Input, M
                                ChannelInfo,   &  ! Input, Scalar  
                                Atmosphere_K,  &  ! K   Output, L x M
                                Surface_K,     &  ! K   Output, L x M
                                RTSolution,    &  ! FWD Output, L x M
                                Options,       &  ! Optional FWD input,  M
                                RCS_Id,        &  ! Revision control
                                Message_Log )  &  ! Error messaging
                              RESULT( Error_Status )
    ! Arguments
    TYPE( CRTM_Atmosphere_type ),        DIMENSION( : ),   INTENT( IN OUT)  :: Atmosphere   ! M
    TYPE( CRTM_Surface_type ),           DIMENSION( : ),   INTENT( IN )     :: Surface      ! M
    TYPE( CRTM_RTSolution_type ),        DIMENSION( :,: ), INTENT( IN OUT ) :: RTSolution_K ! L x M
    TYPE( CRTM_GeometryInfo_type ),      DIMENSION( : ),   INTENT( IN OUT ) :: GeometryInfo ! M
    TYPE( CRTM_ChannelInfo_type ),                         INTENT( IN )     :: ChannelInfo  ! Scalar
    TYPE( CRTM_Atmosphere_type ),        DIMENSION( :,: ), INTENT( IN OUT ) :: Atmosphere_K ! L x M
    TYPE( CRTM_Surface_type ),           DIMENSION( :,: ), INTENT( IN OUT ) :: Surface_K    ! L x M
    TYPE( CRTM_RTSolution_type ),        DIMENSION( :,: ), INTENT( IN OUT ) :: RTSolution   ! L x M
    TYPE( CRTM_Options_type ), OPTIONAL, DIMENSION( : ),   INTENT( IN )     :: Options      ! M
    CHARACTER( * ),            OPTIONAL,                   INTENT( OUT )    :: RCS_Id
    CHARACTER( * ),            OPTIONAL,                   INTENT( IN )     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_K_Matrix(Rank-1)'
    ! Local variables
    CHARACTER( 256 ) :: Message
    CHARACTER( 10 )  :: Value_Input, Value_Allowed
    LOGICAL :: Options_Present
    INTEGER :: Status_K
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
    IF ( SIZE( Atmosphere_K, 2 ) /= n_Profiles ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent profile dimensionality for '//&
                            'Atmosphere input argument.', &
                            Error_Status, &
                            Message_Log = Message_Log )
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
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Check the profile dimensionality
    ! of the other arguments
    IF ( SIZE( Surface )      /= n_Profiles .OR. &
         SIZE( Surface_K, 2 ) /= n_Profiles    ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent profile dimensionality for '//&
                            'Surface input argument(s).', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
    
    IF ( SIZE( GeometryInfo ) /= n_Profiles ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent profile dimensionality for '//&
                            'GeomtryInfo input argument.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( SIZE( RTSolution,   2 ) /= n_Profiles .OR. &
         SIZE( RTSolution_K, 2 ) /= n_Profiles      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent profile dimensionality for '//&
                            'RTSolution output argument(s).', &
                            Error_Status, &
                            Message_Log = Message_Log )
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
                              Message_Log = Message_Log )
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
        Status_K = CRTM_K_Matrix_scalar( Atmosphere(m),            &  ! Input, Scalar
                                         Surface(m),               &  ! Input, Scalar
                                         RTSolution_K(:,m),        &  ! Input, L   
                                         GeometryInfo(m),          &  ! Input, Scalar
                                         ChannelInfo,              &  ! Input, Scalar
                                         Atmosphere_K(:,m),        &  ! Output, L
                                         Surface_K(:,m),           &  ! Output, L
                                         RTSolution(:,m),          &  ! Output, L
                                         Options = Options(m),     &  ! Optional input, Scalar
                                         Message_Log = Message_Log )  ! Error messaging
        IF ( Status_K /= SUCCESS ) THEN
          Error_Status = Status_K
          IF ( Error_Status == FAILURE ) THEN
            WRITE( Message, '( "Error occured in CRTM_K_Matrix(Scalar,", &
                              &"with Options) for profile #", i5 )' ) m
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            RETURN
          END IF
        END IF
      END DO

    ELSE


      ! ----------------------------
      ! Loop for no Options argument
      ! ----------------------------

      DO m = 1, n_Profiles
        Status_K = CRTM_K_Matrix_scalar( Atmosphere(m),            &  ! Input, Scalar
                                         Surface(m),               &  ! Input, Scalar
                                         RTSolution_K(:,m),        &  ! Input, L   
                                         GeometryInfo(m),          &  ! Input, Scalar
                                         ChannelInfo,              &  ! Input, Scalar
                                         Atmosphere_K(:,m),        &  ! Output, L
                                         Surface_K(:,m),           &  ! Output, L
                                         RTSolution(:,m),          &  ! Output, L   
                                         Message_Log = Message_Log )  ! Error messaging
        IF ( Status_K /= SUCCESS ) THEN
          Error_Status = Status_K
          IF ( Error_Status == FAILURE ) THEN
            WRITE( Message, '( "Error occured in CRTM_K_Matrix(Scalar,", &
                              &"no options) for profile #", i5 )' ) m
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            RETURN
          END IF
        END IF
      END DO

    END IF Options_Check

  END FUNCTION CRTM_K_Matrix_rank1



  !#----------------------------------------------------------------------------#
  !#----------------------------------------------------------------------------#
  !#              -- SCALAR (SINGLE PROFILE) SPECIFIC FUNCTION --               #
  !#----------------------------------------------------------------------------#
  !#----------------------------------------------------------------------------#

  FUNCTION CRTM_K_Matrix_scalar( Atmosphere,   &  ! Input, Scalar
                                 Surface,      &  ! Input, Scalar
                                 RTSolution_K, &  ! Input, L   
                                 GeometryInfo, &  ! Input, Scalar
                                 ChannelInfo,  &  ! Input, Scalar  
                                 Atmosphere_K, &  ! Output, L
                                 Surface_K,    &  ! Output, L
                                 RTSolution,   &  ! Output, L
                                 Options,      &  ! Optional FWD input
                                 RCS_Id,       &  ! Revision control
                                 Message_Log ) &  ! Error messaging
                               RESULT( Error_Status )
    ! Arguments
    TYPE( CRTM_Atmosphere_type ),                 INTENT( IN OUT)  :: Atmosphere
    TYPE( CRTM_Surface_type ),                    INTENT( IN )     :: Surface
    TYPE( CRTM_RTSolution_type ), DIMENSION( : ), INTENT( IN OUT ) :: RTSolution_K  ! L
    TYPE( CRTM_GeometryInfo_type ),               INTENT( IN OUT ) :: GeometryInfo
    TYPE( CRTM_ChannelInfo_type ),                INTENT( IN )     :: ChannelInfo
    TYPE( CRTM_Atmosphere_type ), DIMENSION( : ), INTENT( IN OUT ) :: Atmosphere_K  ! L
    TYPE( CRTM_Surface_type ),    DIMENSION( : ), INTENT( IN OUT ) :: Surface_K     ! L
    TYPE( CRTM_RTSolution_type ), DIMENSION( : ), INTENT( IN OUT ) :: RTSolution    ! L
    TYPE( CRTM_Options_type ), OPTIONAL,          INTENT( IN )     :: Options
    CHARACTER( * ),            OPTIONAL,          INTENT( OUT )    :: RCS_Id
    CHARACTER( * ),            OPTIONAL,          INTENT( IN )     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_K_Matrix(Scalar)'
    ! Local variables
    CHARACTER( 256 ) :: Message
    LOGICAL :: User_Emissivity
    LOGICAL :: User_Direct_Reflectivity
    INTEGER :: Status_FWD, Status_K
    INTEGER :: l, n_Full_Streams
    TYPE( CRTM_AtmAbsorption_type ) :: AtmAbsorption,  AtmAbsorption_K
    TYPE( CRTM_AtmScatter_type )    :: AerosolScatter, AerosolScatter_K
    TYPE( CRTM_AtmScatter_type )    :: CloudScatter,   CloudScatter_K
    TYPE( CRTM_AtmScatter_type )    :: AtmOptics,      AtmOptics_K
    TYPE( CRTM_SfcOPtics_type )     :: SfcOptics,      SfcOptics_K
    ! Internal variables
    TYPE( CRTM_CSVariables_type ) :: CSV  ! CloudScatter
    TYPE( CRTM_AOVariables_type ) :: AOV  ! AtmOptics
    TYPE( CRTM_RTVariables_type ) :: RTV  ! RTSolution


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! ----------------------------
    ! Check the number of channels
    ! ----------------------------
    ! If no channels, simply return
    IF ( ChannelInfo%n_Channels == 0 ) RETURN
    ! Output structure arrays too small
    IF ( SIZE( RTSolution   ) < ChannelInfo%n_Channels .OR. &
         SIZE( RTSolution_K ) < ChannelInfo%n_Channels .OR. &
         SIZE( Atmosphere_K ) < ChannelInfo%n_Channels .OR. &
         SIZE( Surface_K    ) < ChannelInfo%n_Channels      ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Output structure arrays too small to hold results ", &
                        &"for the number of requested channels (", i5, ")" )' ) &
                      ChannelInfo%n_Channels
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( MEssage ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#              -- CHECK THE OPTIONAL STRUCTURE ARGUMENTS --                #
    !#                                                                          #
    !# Note the use of the "User_Emissivity" logical flag in this section is    #
    !# not intended as the generic "user options are present" flag. It is       #
    !# anticipated that future additions to the Options structure argument will #
    !# necessitate additional flag variables.                                   #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------------------------
    ! Default action is NOT to use user specified Options
    ! ---------------------------------------------------

    User_Emissivity   = .FALSE.


    ! ------------------------------
    ! Check the FWD Options argument
    ! ------------------------------

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
                                Message_Log = Message_Log )
          RETURN
        END IF

        ! Set to use the supplied emissivity
        User_Emissivity = .TRUE.

        ! Check if the supplied direct reflectivity should be used
        User_Direct_Reflectivity = .FALSE.
        IF ( Options%Direct_Reflectivity_Switch == SET ) User_Direct_Reflectivity = .TRUE.

      END IF Check_Emissivity

    END IF Options_Present



    !#--------------------------------------------------------------------------#
    !#         -- COMPUTE THE DERIVED GEOMETRY FROM THE USER SPECIFIED --       #
    !#         -- COMPONENTS OF THE CRTM_GeometryInfo STRUCTURE        --       #
    !#--------------------------------------------------------------------------#

    Error_Status = CRTM_Compute_GeometryInfo( GeometryInfo, &
                                              Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error computing derived GeometryInfo components', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                     -- ALLOCATE ALL LOCAL STRUCTURES --                  #
    !#--------------------------------------------------------------------------#

    ! ----------------------------
    ! The AtmAbsorption structures
    ! ----------------------------

    ! Forward
    Status_FWD = CRTM_Allocate_AtmAbsorption( Atmosphere%n_Layers,      &  ! Input
                                              MAX_N_PREDICTORS,         &  ! Input
                                              MAX_N_ABSORBERS,          &  ! Input
                                              AtmAbsorption,            &  ! Output
                                              Message_Log = Message_Log )  ! Error messaging

    ! Adjoint
    Status_K = CRTM_Allocate_AtmAbsorption( Atmosphere%n_Layers,      &  ! Input
                                            MAX_N_PREDICTORS,         &  ! Input
                                            MAX_N_ABSORBERS,          &  ! Input
                                            AtmAbsorption_K,          &  ! Output
                                            Message_Log = Message_Log )  ! Error messaging

    IF ( Status_FWD /= SUCCESS .OR. Status_K /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL DIsplay_Message( ROUTINE_NAME, &
                            'Error allocating AtmAbsorption structures', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------------------
    ! The CloudScatter structures
    ! ---------------------------

    ! Forward
    Status_FWD = CRTM_Allocate_AtmScatter( Atmosphere%n_Layers,      &  ! Input
                                           MAX_N_LEGENDRE_TERMS,     &  ! Input
                                           MAX_N_PHASE_ELEMENTS,     &  ! Input
                                           CloudScatter,             &  ! Output
                                           Message_Log = Message_Log )  ! Error messaging

    ! Adjoint
    Status_K = CRTM_Allocate_AtmScatter( Atmosphere%n_Layers,      &  ! Input
                                         MAX_N_LEGENDRE_TERMS,     &  ! Input
                                         MAX_N_PHASE_ELEMENTS,     &  ! Input
                                         CloudScatter_K,           &  ! Output
                                         Message_Log = Message_Log )  ! Error messaging

    IF ( Status_FWD /= SUCCESS .OR. Status_K /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL DIsplay_Message( ROUTINE_NAME, &
                            'Error allocating CloudScatter structures', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! The AerosolScatter structures
    ! -----------------------------

    ! Forward
    Status_FWD = CRTM_Allocate_AtmScatter( Atmosphere%n_Layers,      &  ! Input
                                           MAX_N_LEGENDRE_TERMS,     &  ! Input
                                           MAX_N_PHASE_ELEMENTS,     &  ! Input
                                           AerosolScatter,           &  ! Output
                                           Message_Log = Message_Log )  ! Error messaging

    ! Adjoint
    Status_K = CRTM_Allocate_AtmScatter( Atmosphere%n_Layers,      &  ! Input
                                         MAX_N_LEGENDRE_TERMS,     &  ! Input
                                         MAX_N_PHASE_ELEMENTS,     &  ! Input
                                         AerosolScatter_K,         &  ! Output
                                         Message_Log = Message_Log )  ! Error messaging

    IF ( Status_FWD /= SUCCESS .OR. Status_K /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL DIsplay_Message( ROUTINE_NAME, &
                            'Error allocating AerosolScatter structures', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    ! -----------------------
    ! The AtmOptics structure
    ! -----------------------

    ! Forward
    Status_FWD = CRTM_Allocate_AtmScatter( Atmosphere%n_Layers,      &  ! Input
                                           MAX_N_LEGENDRE_TERMS,     &  ! Input
                                           MAX_N_PHASE_ELEMENTS,     &  ! Input
                                           AtmOptics,                &  ! Output
                                           Message_Log = Message_Log )  ! Error messaging

    ! Adjoint
    Status_K = CRTM_Allocate_AtmScatter( Atmosphere%n_Layers,      &  ! Input
                                         MAX_N_LEGENDRE_TERMS,     &  ! Input
                                         MAX_N_PHASE_ELEMENTS,     &  ! Input
                                         AtmOptics_K,              &  ! Output
                                         Message_Log = Message_Log )  ! Error messaging

    IF ( Status_FWD /= SUCCESS .OR. Status_K /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL DIsplay_Message( ROUTINE_NAME, &
                            'Error allocating AtmOptics structures', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------
    ! The SfcOptics structure
    ! -----------------------

    ! Forward
    Status_FWD = CRTM_Allocate_SfcOptics( MAX_N_ANGLES,             &  ! Input
                                          MAX_N_STOKES,             &  ! Input
                                          SfcOptics,                &  ! Output
                                          Message_Log = Message_Log )  ! Error messaging

    ! Adjoint
    Status_K = CRTM_Allocate_SfcOptics( MAX_N_ANGLES,             &  ! Input
                                        MAX_N_STOKES,             &  ! Input
                                        SfcOptics_K,              &  ! Output
                                        Message_Log = Message_Log )  ! Error messaging

    IF ( Status_FWD /= SUCCESS .OR. Status_K /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL DIsplay_Message( ROUTINE_NAME, &
                            'Error allocating SfcOptics structures', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- PREPROCESS SOME INPUT DATA --                     #
    !#--------------------------------------------------------------------------#


    ! --------------------------------------------------------
    ! Average surface skin temperature for multi-surface types
    ! --------------------------------------------------------

    CALL CRTM_Compute_SurfaceT( Surface, SfcOptics )



    !#--------------------------------------------------------------------------#
    !#             -- SET UP FOR FORWARD GASEOUS ABSORPTION CALCS --            #
    !#--------------------------------------------------------------------------#

    Status_FWD = CRTM_SetUp_AtmAbsorption( Atmosphere,               &  ! Input
                                           GeometryInfo,             &  ! Input
                                           AtmAbsorption,            &  ! Output
                                           Message_Log = Message_Log )  ! Error messaging

    IF ( Status_FWD /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL DIsplay_Message( ROUTINE_NAME, &
                            'Error setting up AtmAbsorption structure', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



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

      Status_FWD = CRTM_Compute_AtmAbsorption( ChannelInfo%Channel_Index(l), &  ! Input
                                               AtmAbsorption,                &  ! In/Output
                                               Message_Log = Message_Log     )  ! Error messaging

      IF ( Status_FWD /= SUCCESS ) THEN
        Error_Status = FAILURE
        CALL DIsplay_Message( ROUTINE_NAME, &
                              'Error in CRTM_Compute_AtmAbsorption', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF


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
      AtmOptics_K%n_Legendre_Terms    = n_Full_Streams


      ! -----------------------------------------------------------
      ! Compute the cloud particle absorption/scattering properties
      ! -----------------------------------------------------------

      IF( Atmosphere%n_Clouds > 0 ) THEN

        CloudScatter%n_Legendre_Terms   = n_Full_Streams
        Status_FWD = CRTM_Compute_CloudScatter( Atmosphere,                   &  ! Input
                                                ChannelInfo%Channel_Index(l), &  ! Input
                                                CloudScatter,                 &  ! Output
                                                CSV,                          &  ! Internal variable output
                                                Message_Log = Message_Log     )  ! Error messaging
        IF ( Status_FWD /= SUCCESS ) THEN
          Error_Status = FAILURE
          WRITE( Message, '( "Error computing CloudScatter for ", a, &
                            &", channel ", i4 )' ) &
                          TRIM( ChannelInfo%Sensor_Descriptor(l) ), &
                          ChannelInfo%Sensor_Channel(l)
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          RETURN
!            OR
!          CYCLE Channel_Loop
        END IF

      ENDIF


      ! ----------------------------------------------------
      ! Compute the aerosol absorption/scattering properties
      ! ----------------------------------------------------

      IF ( Atmosphere%n_Aerosols > 0 ) THEN

        AerosolScatter%n_Legendre_Terms = n_Full_Streams
        Status_FWD = CRTM_Compute_AerosolScatter( Atmosphere,                   &  ! Input
                                                  GeometryInfo,                 &  ! Input
                                                  ChannelInfo%Channel_Index(l), &  ! Input
                                                  AerosolScatter,               &  ! Output
                                                  Message_Log = Message_Log     )  ! Error messaging

        IF ( Status_FWD /= SUCCESS ) THEN
          Error_Status = FAILURE
          WRITE( Message, '( "Error computing AerosolScatter for ", a, ", channel ", i4 )' ) &
                          TRIM( ChannelInfo%Sensor_Descriptor(l) ), &
                          ChannelInfo%Sensor_Channel(l)
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          RETURN
!            OR
!          CYCLE Channel_Loop
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
                                              Message_Log = Message_Log     )  ! Error messaging

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error computing CRTM_Compute_RTSolution for ", a, &
                          &", channel ", i4 )' ) &
                        TRIM( ChannelInfo%Sensor_Descriptor(l) ), &
                        ChannelInfo%Sensor_Channel(l)
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF



      !#------------------------------------------------------------------------#
      !#                        -- ADJOINT CALCULATIONS --                      #
      !#------------------------------------------------------------------------#
                                   
      ! --------------------------------------------------
      ! Reinitialise profile independent adjoint variables
      ! --------------------------------------------------
      AtmOptics_K%Optical_Depth         = ZERO
      AtmOptics_K%Single_Scatter_Albedo = ZERO

      IF( AtmOptics%n_Legendre_Terms > 0 ) THEN
        AtmOptics_K%Phase_Coefficient     = ZERO
        AtmOptics_K%Asymmetry_Factor      = ZERO
        AtmOptics_K%Delta_Truncation      = ZERO
      END IF


      ! -------------------------------------
      ! The adjoint of the radiative transfer
      ! -------------------------------------

      Status_K = CRTM_Compute_RTSolution_AD( Atmosphere,                   &  ! FWD Input
                                             Surface,                      &  ! FWD Input
                                             AtmOptics,                    &  ! FWD Input
                                             SfcOptics,                    &  ! FWD Input
                                             RTSolution(l),                &  ! FWD Input
                                             RTSolution_K(l),              &  ! K  Input
                                             GeometryInfo,                 &  ! Input
                                             ChannelInfo%Channel_Index(l), &  ! Input
                                             Atmosphere_K(l),              &  ! K Output
                                             Surface_K(l),                 &  ! K Output
                                             AtmOptics_K,                  &  ! K Output
                                             SfcOptics_K,                  &  ! K Output
                                             RTV,                          &  ! Internal variable input
                                             Message_Log = Message_Log     )  ! Error messaging

      IF ( Status_K /= SUCCESS ) THEN
        Error_Status = FAILURE
        CALL DIsplay_Message( ROUTINE_NAME, &
                              'Error in CRTM_Compute_RTSolution_AD', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF


      ! ------------------------------------------------------------------
      ! Compute the adjoint of the combined atmospheric optical properties
      ! ------------------------------------------------------------------

      CALL CRTM_Combine_AtmOptics_AD( AtmAbsorption,    &  ! FWD Input
                                      CloudScatter,     &  ! FWD Input
                                      AerosolScatter,   &  ! FWD Input
                                      AtmOptics,        &  ! FWD Input
                                      AtmOptics_K,      &  ! K Input
                                      AtmAbsorption_K,  &  ! K Output
                                      CloudScatter_K,   &  ! K Output
                                      AerosolScatter_K, &  ! K Output
                                      AOV               )  ! Internal variable input


      ! ------------------------------------------------------------
      ! Compute the adjoint aerosol absorption/scattering properties
      ! ------------------------------------------------------------

      IF ( Atmosphere%n_Aerosols > 0 ) THEN

        Status_K = CRTM_Compute_AerosolScatter_AD( Atmosphere,                   &  ! Input
                                                   AerosolScatter,               &  ! Input
                                                   AerosolScatter_K,             &  ! Input
                                                   GeometryInfo,                 &  ! Input
                                                   ChannelInfo%Channel_Index(l), &  ! Input
                                                   Atmosphere_K(l),              &  ! In/Output
                                                   Message_Log = Message_Log     )  ! Error messaging

        IF ( Status_K /= SUCCESS ) THEN
          Error_Status = FAILURE
          CALL DIsplay_Message( ROUTINE_NAME, &
                                'Error in CRTM_Compute_AerosolScatter_AD', &
                                Error_Status, &
                                Message_Log = Message_Log )
          RETURN
        END IF

      END IF


      ! ----------------------------------------------------------
      ! Compute the adjoint cloud absorption/scattering properties
      ! ----------------------------------------------------------

      IF( Atmosphere%n_Clouds > 0 ) THEN

        Status_K = CRTM_Compute_CloudScatter_AD( Atmosphere,                   &  ! Input
                                                 CloudScatter,                 &  ! Input
                                                 CloudScatter_K,               &  ! Input
                                                 ChannelInfo%Channel_Index(l), &  ! Input
                                                 Atmosphere_K(l),              &  ! In/Output
                                                 CSV,                          &  ! Internal variable input
                                                 Message_Log = Message_Log     )  ! Error messaging

        IF ( Status_K /= SUCCESS ) THEN
          Error_Status = FAILURE
          CALL DIsplay_Message( ROUTINE_NAME, &
                                'Error in CRTM_Compute_CloudScatter_AD', &
                                Error_Status, &
                                Message_Log = Message_Log )
          RETURN
        END IF

      END IF


      ! ----------------------------------------
      ! Compute the adjoint layer optical depths
      ! due to gaseous absorption
      ! ----------------------------------------

      ! The optical depth calculation
      Status_K = CRTM_Compute_AtmAbsorption_AD( ChannelInfo%Channel_Index(l), &  ! Input
                                                AtmAbsorption,                &  ! Input
                                                AtmAbsorption_K,              &  ! In/Output
                                                Message_Log = Message_Log     )  ! Error messaging

      IF ( Status_K /= SUCCESS ) THEN
        Error_Status = FAILURE
        CALL DIsplay_Message( ROUTINE_NAME, &
                              'Error in CRTM_Compute_AtmAbsorption_AD', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF


      ! The predictor and absorber space calculation
      Status_K = CRTM_SetUp_AtmAbsorption_AD( Atmosphere,               &  ! Input
                                              AtmAbsorption,            &  ! Input
                                              AtmAbsorption_K,          &  ! Input
                                              GeometryInfo,             &  ! Input
                                              Atmosphere_K(l),          &  ! In/Output
                                              Message_Log = Message_Log )  ! Error messaging

      IF ( Status_K /= SUCCESS ) THEN
        Error_Status = FAILURE
        CALL DIsplay_Message( ROUTINE_NAME, &
                              'Error in CRTM_SetUp_AtmAbsorption_AD', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF



      !#------------------------------------------------------------------------#
      !#                     -- POSTPROCESS SOME INPUT DATA --                  #
      !#------------------------------------------------------------------------#

      ! -------------------------------------------------------------------
      ! Adjoint of average surface skin temperature for multi-surface types
      ! -------------------------------------------------------------------

      CALL CRTM_Compute_SurfaceT_AD( Surface, SfcOptics_K, Surface_K(l) )  


    END DO Channel_Loop



    !#--------------------------------------------------------------------------#
    !#                    -- DEALLOCATE LOCAL STRUCTURES --                     #
    !#--------------------------------------------------------------------------#

    ! ------------------------
    ! The SfcOptics structures
    ! ------------------------

    Status_FWD = CRTM_Destroy_SfcOptics( SfcOptics )
    Status_K   = CRTM_Destroy_SfcOptics( SfcOptics_K )

    IF ( Status_FWD /= SUCCESS .OR. Status_K /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating SfcOptics structures', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! ------------------------
    ! The AtmOptics structures
    ! ------------------------

    Status_FWD = CRTM_Destroy_AtmScatter( AtmOptics )
    Status_K   = CRTM_Destroy_AtmScatter( AtmOptics_K )

    IF ( Status_FWD /= SUCCESS .OR. Status_K /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating CloudScatter structures', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! -----------------------------
    ! The AerosolScatter structures
    ! -----------------------------

    Status_FWD = CRTM_Destroy_AtmScatter( AerosolScatter )
    Status_K   = CRTM_Destroy_AtmScatter( AerosolScatter_K )

    IF ( Status_FWD /= SUCCESS .OR. Status_K /= SUCCESS ) THEN
      Error_status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating AerosolScatter structures', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! ---------------------------
    ! The CloudScatter structures
    ! ---------------------------

    Status_FWD = CRTM_Destroy_AtmScatter( CloudScatter )
    Status_K   = CRTM_Destroy_AtmScatter( CloudScatter_K )

    IF ( Status_FWD /= SUCCESS .OR. Status_K /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating CloudScatter structures', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! ----------------------------
    ! The AtmAbsorption structures
    ! ----------------------------

    Status_FWD = CRTM_Destroy_AtmAbsorption( AtmAbsorption )
    Status_K   = CRTM_Destroy_AtmAbsorption( AtmAbsorption_K )

    IF ( Status_FWD /= SUCCESS .OR. Status_K /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating AtmAbsorption structures', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION CRTM_K_Matrix_scalar

END MODULE CRTM_K_Matrix_Module
