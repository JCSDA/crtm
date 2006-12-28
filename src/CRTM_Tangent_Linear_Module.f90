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
  USE CRTM_Parameters,          ONLY: SET, NOT_SET, ONE   , &
                                      MAX_N_PROFILES      , &
                                      MAX_N_ABSORBERS     , &
                                      MAX_N_PREDICTORS    , &
                                      MAX_N_PHASE_ELEMENTS, &
                                      MAX_N_LEGENDRE_TERMS, &
                                      MAX_N_STOKES        , &
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
                                      CRTM_AAVariables_type        , &
                                      CRTM_Allocate_AtmAbsorption  , &
                                      CRTM_Destroy_AtmAbsorption   , &
                                      CRTM_Compute_AtmAbsorption   , &
                                      CRTM_Compute_AtmAbsorption_TL
  USE CRTM_AtmScatter_Define,   ONLY: CRTM_AtmScatter_type    , &
                                      CRTM_Allocate_AtmScatter, &
                                      CRTM_Destroy_AtmScatter
  USE CRTM_AerosolScatter,      ONLY: CRTM_ASVariables_type         , &
                                      CRTM_Compute_AerosolScatter   , &
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
                                      CRTM_Compute_nStreams     , &
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


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id$'


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
!       Error_Status = CRTM_Tangent_Linear( Atmosphere             , &  ! FWD Input
!                                           Surface                , &  ! FWD Input
!                                           Atmosphere_TL          , &  ! TL  Input
!                                           Surface_TL             , &  ! TL  Input
!                                           GeometryInfo           , &  ! Input
!                                           ChannelInfo            , &  ! Input
!                                           RTSolution             , &  ! FWD Output
!                                           RTSolution_TL          , &  ! TL  Output
!                                           Options    =Options    , &  ! Optional FWD input
!                                           RCS_Id     =RCS_Id     , &  ! Revision control
!                                           Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Atmosphere:     Structure containing the Atmosphere data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Atmosphere_type)
!                       DIMENSION:  Rank-1 (nProfiles)
!                       ATTRIBUTES: INTENT(IN)
!
!       Surface:        Structure containing the Surface data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Surface_type)
!                       DIMENSION:  Same as input Atmosphere structure
!                       ATTRIBUTES: INTENT(IN)
!
!       Atmosphere_TL:  Structure containing the tangent-linear Atmosphere data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Atmosphere_type)
!                       DIMENSION:  Same as input Atmosphere structure
!                       ATTRIBUTES: INTENT(IN)
!
!       Surface_TL:     Structure containing the tangent-linear Surface data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Surface_type)
!                       DIMENSION:  Same as input Atmosphere structure
!                       ATTRIBUTES: INTENT(IN)
!
!       GeometryInfo:   Structure containing the view geometry
!                       information.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_GeometryInfo_type)
!                       DIMENSION:  Same as input Atmosphere structure
!                       ATTRIBUTES: INTENT(IN)
!
!       ChannelInfo:    Structure returned from the CRTM_Init() function
!                       that contains the satellite/sensor channel index
!                       information.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_ChannelInfo_type)
!                       DIMENSION:  Rank-1 (nSensors)
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Options:        Options structure containing the optional forward model
!                       arguments for the CRTM.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Options_type)
!                       DIMENSION:  Same as input Atmosphere structure
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
!                       TYPE:       TYPE(CRTM_RTSolution_type)
!                       DIMENSION:  Rank-2 (nChannels x nProfiles)
!                       ATTRIBUTES: INTENT(IN OUT)
!
!       RTSolution_TL:  Structure containing the solution to the tangent-
!                       linear RT equation for the given inputs.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_RTSolution_type)
!                       DIMENSION:  Rank-2 (nChannels x nProfiles)
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
!       - The Options optional input structure arguments contain
!         spectral information (e.g. emissivity) that must have the same
!         spectral dimensionality (the "L" dimension) as the output
!         RTSolution structures.
!
!       - The INTENT on the output RTSolution arguments are IN OUT rather
!         than just OUT. This is necessary because the arguments may be defined
!         upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Tangent_Linear( Atmosphere   , &  ! FWD Input, M
                                Surface      , &  ! FWD Input, M
                                Atmosphere_TL, &  ! TL  Input, M
                                Surface_TL   , &  ! TL  Input, M
                                GeometryInfo , &  ! Input, M
                                ChannelInfo  , &  ! Input, nSensors  
                                RTSolution   , &  ! FWD Output, L x M  
                                RTSolution_TL, &  ! TL  Output, L x M  
                                Options      , &  ! Optional FWD input, M
                                RCS_Id       , &  ! Revision control
                                Message_Log )  &  ! Error messaging
                              RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Atmosphere_type)       , INTENT(IN)     :: Atmosphere(:)      ! M
    TYPE(CRTM_Surface_type)          , INTENT(IN)     :: Surface(:)         ! M
    TYPE(CRTM_Atmosphere_type)       , INTENT(IN)     :: Atmosphere_TL(:)   ! M
    TYPE(CRTM_Surface_type)          , INTENT(IN)     :: Surface_TL(:)      ! M
    TYPE(CRTM_GeometryInfo_type)     , INTENT(IN OUT) :: GeometryInfo(:)    ! M
    TYPE(CRTM_ChannelInfo_type)      , INTENT(IN)     :: ChannelInfo(:)     ! nSensors
    TYPE(CRTM_RTSolution_type)       , INTENT(IN OUT) :: RTSolution(:,:)    ! L x M
    TYPE(CRTM_RTSolution_type)       , INTENT(IN OUT) :: RTSolution_TL(:,:) ! L x M
    TYPE(CRTM_Options_type), OPTIONAL, INTENT(IN)     :: Options(:)         ! M
    CHARACTER(*),            OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),            OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Tangent_Linear(Rank-1)'
    ! Local variables
    CHARACTER(256) :: Message
    LOGICAL :: Options_Present
    LOGICAL :: User_Emissivity
    LOGICAL :: User_Direct_Reflectivity
    INTEGER :: Status_FWD, Status_TL
    INTEGER :: n, nSensors,  SensorIndex
    INTEGER :: l, nChannels, ChannelIndex
    INTEGER :: m, nProfiles
    INTEGER :: ln
    INTEGER :: n_Full_Streams, n_Layers
    INTEGER, DIMENSION(6) :: AllocStatus, AllocStatus_TL
    ! Component variables
    TYPE(CRTM_Predictor_type)     :: Predictor,      Predictor_TL
    TYPE(CRTM_AtmAbsorption_type) :: AtmAbsorption,  AtmAbsorption_TL
    TYPE(CRTM_AtmScatter_type)    :: AerosolScatter, AerosolScatter_TL
    TYPE(CRTM_AtmScatter_type)    :: CloudScatter,   CloudScatter_TL
    TYPE(CRTM_AtmScatter_type)    :: AtmOptics,      AtmOptics_TL
    TYPE(CRTM_SfcOptics_type)     :: SfcOptics,      SfcOptics_TL
    ! Component variable internals
    TYPE(CRTM_APVariables_type) :: APV  ! Predictor
    TYPE(CRTM_AAVariables_type) :: AAV  ! AtmAbsorption
    TYPE(CRTM_CSVariables_type) :: CSV  ! CloudScatter
    TYPE(CRTM_ASVariables_type) :: ASV  ! AerosolScatter
    TYPE(CRTM_AOVariables_type) :: AOV  ! AtmOptics
    TYPE(CRTM_RTVariables_type) :: RTV  ! RTSolution


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! ----------------------------------------
    ! If no sensors or channels, simply return
    ! ----------------------------------------
    nSensors  = SIZE(ChannelInfo)
    nChannels = SUM(ChannelInfo%n_Channels)
    IF ( nSensors == 0 .OR. nChannels == 0 ) RETURN


    ! ---------------------------------
    ! Output RTSolution array too small
    ! ---------------------------------
    IF ( SIZE(RTSolution,   DIM=1) < nChannels .OR. &
         SIZE(RTSolution_TL,DIM=1) < nChannels      ) THEN
      Error_Status = FAILURE
      WRITE(Message,'("Output RTSolution structure arrays too small (",i0," and ",i0,&
                     &") to hold results for the number of requested channels (",i0,")")') &
                     SIZE(RTSolution,DIM=1), SIZE(RTSolution_TL,DIM=1), nChannels
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! ----------------------------
    ! Check the number of profiles
    ! ----------------------------

    ! Number of atmospheric profiles.
    nProfiles = SIZE(Atmosphere)

    ! Check that the number of profiles is not greater than
    ! MAX_N_PROFILES. This is simply a limit to restrict the
    ! size of the input arrays so they're not TOO big.
    IF ( nProfiles > MAX_N_PROFILES ) THEN
      Error_Status = FAILURE
      WRITE(Message,'("Number of passed profiles (",i0,&
                     &") > maximum number of profiles allowed(",i0,")")') &
                    nProfiles, MAX_N_PROFILES
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the profile dimensionality
    ! of the other mandatory arguments
    IF ( SIZE(Surface)             /= nProfiles .OR. &
         SIZE(Atmosphere_TL)       /= nProfiles .OR. &
         SIZE(Surface_TL)          /= nProfiles .OR. &
         SIZE(GeometryInfo)        /= nProfiles .OR. &
         SIZE(RTSolution,   DIM=2) /= nProfiles .OR. &
         SIZE(RTSolution_TL,DIM=2) /= nProfiles      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent profile dimensionality for '//&
                            'input arguments.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the profile dimensionality
    ! of the other optional arguments
    Options_Present = .FALSE.
    IF ( PRESENT(Options) ) THEN
      Options_Present = .TRUE.
      IF ( SIZE(Options) /= nProfiles ) THEN
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
    !#--------------------------------------------------------------------------#
    Profile_Loop: DO m = 1, nProfiles


      ! ---------------------------------------------
      ! Check the optional Options structure argument
      ! ---------------------------------------------
      ! Default action is NOT to use user specified Options
      User_Emissivity = .FALSE.
      !.... other User_XXX flags as added.

      ! Check the Options argument
      IF (Options_Present) THEN

        ! Check if the supplied emissivity should be used
        IF ( Options(m)%Emissivity_Switch == SET ) THEN
          ! Are the channel dimensions consistent
          IF ( Options(m)%n_Channels < nChannels ) THEN
            Error_Status = FAILURE
            WRITE( Message, '( "Input Options channel dimension (", i0, ") is less ", &
                              &"than the number of requested channels (",i0, ")" )' ) &
                            Options(m)%n_Channels, nChannels
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
          IF ( Options(m)%Direct_Reflectivity_Switch == SET ) User_Direct_Reflectivity = .TRUE.
        END IF
      END IF


      ! ------------------------
      ! Compute derived geometry
      ! ------------------------
      Error_Status = CRTM_Compute_GeometryInfo( GeometryInfo(m), &
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
      AllocStatus(1)   =CRTM_Allocate_Predictor( Atmosphere(m)%n_Layers , &  ! Input
                                                 MAX_N_PREDICTORS       , &  ! Input
                                                 MAX_N_ABSORBERS        , &  ! Input
                                                 Predictor              , &  ! Output
                                                 Message_Log=Message_Log  )  ! Error messaging
      AllocStatus_TL(1)=CRTM_Allocate_Predictor( Atmosphere(m)%n_Layers , &  ! Input
                                                 MAX_N_PREDICTORS       , &  ! Input
                                                 MAX_N_ABSORBERS        , &  ! Input
                                                 Predictor_TL           , &  ! Output
                                                 Message_Log=Message_Log  )  ! Error messaging
      AllocStatus(2)   =CRTM_Allocate_AtmAbsorption( Atmosphere(m)%n_Layers , &  ! Input
                                                     AtmAbsorption          , &  ! Output
                                                     Message_Log=Message_Log  )  ! Error messaging
      AllocStatus_TL(2)=CRTM_Allocate_AtmAbsorption( Atmosphere(m)%n_Layers , &  ! Input
                                                     AtmAbsorption_TL       , &  ! Output
                                                     Message_Log=Message_Log  )  ! Error messaging
      ! The CloudScatter structures
      AllocStatus(3)   =CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers , &  ! Input
                                                  MAX_N_LEGENDRE_TERMS   , &  ! Input
                                                  MAX_N_PHASE_ELEMENTS   , &  ! Input
                                                  CloudScatter           , &  ! Output
                                                  Message_Log=Message_Log  )  ! Error messaging
      AllocStatus_TL(3)=CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers , &  ! Input
                                                  MAX_N_LEGENDRE_TERMS   , &  ! Input
                                                  MAX_N_PHASE_ELEMENTS   , &  ! Input
                                                  CloudScatter_TL        , &  ! Output
                                                  Message_Log=Message_Log  )  ! Error messaging
      ! The AerosolScatter structures
      AllocStatus(4)   =CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers , &  ! Input
                                                  MAX_N_LEGENDRE_TERMS   , &  ! Input
                                                  MAX_N_PHASE_ELEMENTS   , &  ! Input
                                                  AerosolScatter         , &  ! Output
                                                  Message_Log=Message_Log  )  ! Error messaging
      AllocStatus_TL(4)=CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers , &  ! Input
                                                  MAX_N_LEGENDRE_TERMS   , &  ! Input
                                                  MAX_N_PHASE_ELEMENTS   , &  ! Input
                                                  AerosolScatter_TL      , &  ! Output
                                                  Message_Log=Message_Log  )  ! Error messaging
      ! The AtmOptics structure
      AllocStatus(5)   =CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers , &  ! Input
                                                  MAX_N_LEGENDRE_TERMS   , &  ! Input
                                                  MAX_N_PHASE_ELEMENTS   , &  ! Input
                                                  AtmOptics              , &  ! Output
                                                  Message_Log=Message_Log  )  ! Error messaging
      AllocStatus_TL(5)=CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers , &  ! Input
                                                  MAX_N_LEGENDRE_TERMS   , &  ! Input
                                                  MAX_N_PHASE_ELEMENTS   , &  ! Input
                                                  AtmOptics_TL           , &  ! Output
                                                  Message_Log=Message_Log  )  ! Error messaging
      ! The SfcOptics structure
      AllocStatus(6)   =CRTM_Allocate_SfcOptics( MAX_N_ANGLES           , &  ! Input
                                                 MAX_N_STOKES           , &  ! Input
                                                 SfcOptics              , &  ! Output
                                                 Message_Log=Message_Log  )  ! Error messaging
      AllocStatus_TL(6)=CRTM_Allocate_SfcOptics( MAX_N_ANGLES           , &  ! Input
                                                 MAX_N_STOKES           , &  ! Input
                                                 SfcOptics_TL           , &  ! Output
                                                 Message_Log=Message_Log  )  ! Error messaging
      IF ( ANY(AllocStatus    /= SUCCESS) .OR. &
           ANY(AllocStatus_TL /= SUCCESS)      ) THEN
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
      CALL CRTM_Compute_SurfaceT( Surface(m), SfcOptics )
      CALL CRTM_Compute_SurfaceT_TL( Surface(m), Surface_TL(m), SfcOptics_TL )


      ! ------------------------------------------
      ! Compute predictors for AtmAbsorption calcs
      ! ------------------------------------------
      CALL CRTM_Compute_Predictors( Atmosphere(m)  , &  ! Input
                                    GeometryInfo(m), &  ! Input
                                    Predictor      , &  ! Output
                                    APV              )  ! Internal variable output
      CALL CRTM_Compute_Predictors_TL( Atmosphere(m)   , &  ! Input
                                       Predictor       , &  ! Input
                                       Atmosphere_TL(m), &  ! Input
                                       GeometryInfo(m) , &  ! Input
                                       Predictor_TL    , &  ! Output
                                       APV               )  ! Internal variable input


      ! Initialise channel counter for sensor(n)/channel(l) count
      ln = 0
    
      ! -----------
      ! Sensor loop
      ! -----------
      Sensor_Loop: DO n = 1, nSensors

        ! Descriptive name
        SensorIndex = n

        ! ------------
        ! Channel loop
        ! ------------
        Channel_Loop: DO l = 1, ChannelInfo(n)%n_Channels

          ! Shorter name
          ChannelIndex = ChannelInfo(n)%Channel_Index(l)
          
          ! Increment channel counter
          ln = ln + 1

          
          ! ---------------------------------------------------------------
          ! Determine the number of streams (n_Full_Streams) in up+downward
          ! directions. Currently, n_Full_Streams is determined from the
          ! cloud parameters only. It will also use the aerosol parameters 
          ! when aerosol scattering is included.
          ! ---------------------------------------------------------------
          n_Full_Streams = CRTM_Compute_nStreams( Atmosphere(m)   , &  ! Input
                                                  SensorIndex     , &  ! Input
                                                  ChannelIndex    , &  ! Input
                                                  RTSolution(ln,m)  )  ! Output
          ! Transfer the number of streams
          ! to all the scattering structures
          AtmOptics%n_Legendre_Terms    = n_Full_Streams
          AtmOptics_TL%n_Legendre_Terms = n_Full_Streams


          ! --------------------------
          ! Compute the gas absorption
          ! --------------------------
          CALL CRTM_Compute_AtmAbsorption( SensorIndex  , &  ! Input
                                           ChannelIndex , &  ! Input
                                           Predictor    , &  ! Input
                                           AtmAbsorption, &  ! Output
                                           AAV            )  ! Internal variable output
          CALL CRTM_Compute_AtmAbsorption_TL( SensorIndex     , &  ! Input
                                              ChannelIndex    , &  ! Input
                                              Predictor       , &  ! Input
                                              Predictor_TL    , &  ! Input
                                              AtmAbsorption_TL, &  ! Output
                                              AAV               )  ! Internal variable input

          
          ! -----------------------------------------------------------
          ! Compute the cloud particle absorption/scattering properties
          ! -----------------------------------------------------------
          IF( Atmosphere(m)%n_Clouds > 0 ) THEN
            CloudScatter%n_Legendre_Terms = n_Full_Streams
            Status_FWD = CRTM_Compute_CloudScatter( Atmosphere(m)          , &  ! Input
                                                    SensorIndex            , &  ! Input
                                                    ChannelIndex           , &  ! Input
                                                    CloudScatter           , &  ! Output
                                                    CSV                    , &  ! Internal variable output
                                                    Message_Log=Message_Log  )  ! Error messaging
            Status_TL = CRTM_Compute_CloudScatter_TL( Atmosphere(m)          , &  ! FWD Input
                                                      CloudScatter           , &  ! FWD Input
                                                      Atmosphere_TL(m)       , &  ! TL  Input
                                                      SensorIndex            , &  ! Input
                                                      ChannelIndex           , &  ! Input
                                                      CloudScatter_TL        , &  ! TL  Output
                                                      CSV                    , &  ! Internal variable input
                                                      Message_Log=Message_Log  )  ! Error messaging
            IF ( Status_FWD /= SUCCESS .OR. Status_TL /= SUCCESS) THEN
              Error_Status = FAILURE
              WRITE(Message,'("Error computing CloudScatter for ",a,&
                             &", channel ",i0)') &
                              TRIM( ChannelInfo(n)%SensorID(l) ), &
                              ChannelInfo(n)%Sensor_Channel(l)
              CALL Display_Message( ROUTINE_NAME, &
                                    TRIM(Message), &
                                    Error_Status, &
                                    Message_Log=Message_Log )
              RETURN
            END IF
          END IF


          ! ----------------------------------------------------
          ! Compute the aerosol absorption/scattering properties
          ! ----------------------------------------------------
          IF ( Atmosphere(m)%n_Aerosols > 0 ) THEN
            AerosolScatter%n_Legendre_Terms = n_Full_Streams
            Status_FWD = CRTM_Compute_AerosolScatter( Atmosphere(m)          , &  ! Input
                                                      SensorIndex            , &  ! Input
                                                      ChannelIndex           , &  ! Input
                                                      AerosolScatter         , &  ! In/Output
                                                      ASV                    , &  ! Internal variable output
                                                      Message_Log=Message_Log  )  ! Error messaging
            Status_TL  = CRTM_Compute_AerosolScatter_TL( Atmosphere(m)          , &  ! FWD Input
                                                         AerosolScatter         , &  ! FWD Input
                                                         Atmosphere_TL(m)       , &  ! TL  Input
                                                         SensorIndex            , &  ! Input
                                                         ChannelIndex           , &  ! Input
                                                         AerosolScatter_TL      , &  ! TL  Output  
                                                         ASV                    , &  ! Internal variable input
                                                         Message_Log=Message_Log  )  ! Error messaging
            IF ( Status_FWD /= SUCCESS .OR. Status_TL /= SUCCESS) THEN
              Error_Status = FAILURE
              WRITE(Message,'("Error computing AerosolScatter for ",a,&
                             &", channel ",i0)') &
                              TRIM( ChannelInfo(n)%SensorID(l) ), &
                              ChannelInfo(n)%Sensor_Channel(l)
              CALL Display_Message( ROUTINE_NAME, &
                                    TRIM(Message), &
                                    Error_Status, &
                                    Message_Log=Message_Log )
              RETURN
            END IF
          END IF


          ! ---------------------------------------------------
          ! Compute the combined atmospheric optical properties
          ! ---------------------------------------------------
          CALL CRTM_Combine_AtmOptics( AtmAbsorption , & ! Input
                                       CloudScatter  , & ! Input
                                       AerosolScatter, & ! Input
                                       AtmOptics     , & ! Output
                                       AOV             ) ! Internal variable output
          CALL CRTM_Combine_AtmOptics_TL( AtmAbsorption    , & ! FWD Input
                                          CloudScatter     , & ! FWD Input
                                          AerosolScatter   , & ! FWD Input
                                          AtmOptics        , & ! FWD Input
                                          AtmAbsorption_TL , & ! TL  Input
                                          CloudScatter_TL  , & ! TL  Input
                                          AerosolScatter_TL, & ! TL  Input
                                          AtmOptics_TL     , & ! TL  Output
                                          AOV                ) ! Internal variable input


          ! ------------------------------------
          ! Fill the SfcOptics structure for the
          ! optional emissivity input case.
          ! ------------------------------------
          ! Indicate SfcOptics ARE to be computed
          SfcOptics%Compute_Switch = SET
          ! Change SfcOptics emissivity/reflectivity
          ! contents/computation status
          IF ( User_Emissivity ) THEN
            SfcOptics%Compute_Switch  = NOT_SET
            SfcOptics%Emissivity(1,1)       = Options(m)%Emissivity(ln)
            SfcOptics%Reflectivity(1,1,1,1) = ONE - Options(m)%Emissivity(ln)

            IF ( User_Direct_Reflectivity ) THEN
              SfcOptics%Direct_Reflectivity(1,1) = Options(m)%Direct_Reflectivity(ln)
            ELSE
              SfcOptics%Direct_Reflectivity(1,1) = SfcOptics%Reflectivity(1,1,1,1)
            END IF

          END IF


          ! ------------------------------------
          ! Solve the radiative transfer problem
          ! ------------------------------------
          ! Forward model
          Error_Status = CRTM_Compute_RTSolution( Atmosphere(m)          , &  ! Input
                                                  Surface(m)             , &  ! Input
                                                  AtmOptics              , &  ! Input
                                                  SfcOptics              , &  ! Input
                                                  GeometryInfo(m)        , &  ! Input
                                                  SensorIndex            , &  ! Input
                                                  ChannelIndex           , &  ! Input
                                                  RTSolution(ln,m)       , &  ! Output
                                                  RTV                    , &  ! Internal variable output
                                                  Message_Log=Message_Log  )  ! Error messaging
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing RTSolution for ", a, &
                              &", channel ", i0 )' ) &
                            TRIM( ChannelInfo(n)%SensorID(l) ), &
                            ChannelInfo(n)%Sensor_Channel(l)
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM(Message), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
            RETURN
          END IF
          
          ! Tangent-linear model
          Error_Status = CRTM_Compute_RTSolution_TL( Atmosphere(m)          , &  ! FWD Input
                                                     Surface(m)             , &  ! FWD Input
                                                     AtmOptics              , &  ! FWD Input
                                                     SfcOptics              , &  ! FWD Input
                                                     RTSolution(ln,m)       , &  ! FWD Input
                                                     Atmosphere_TL(m)       , &  ! TL  Input
                                                     Surface_TL(m)          , &  ! TL  Input
                                                     AtmOptics_TL           , &  ! TL  Input
                                                     SfcOptics_TL           , &  ! TL  Input 
                                                     GeometryInfo(m)        , &  ! Input
                                                     SensorIndex            , &  ! Input
                                                     ChannelIndex           , &  ! Input
                                                     RTSolution_TL(ln,m)    , &  ! TL  Output
                                                     RTV                    , &  ! Internal variable input
                                                     Message_Log=Message_Log  )  ! Error messaging
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing RTSolution_TL for ", a, &
                              &", channel ", i0 )' ) &
                            TRIM( ChannelInfo(n)%SensorID(l) ), &
                            ChannelInfo(n)%Sensor_Channel(l)
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM(Message), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
            RETURN
          END IF
          
        END DO Channel_Loop
      END DO Sensor_Loop


      ! ---------------------------
      ! Deallocate local structures
      ! ---------------------------
      AllocStatus_TL(6)=CRTM_Destroy_SfcOptics( SfcOptics_TL )
      AllocStatus(6)   =CRTM_Destroy_SfcOptics( SfcOptics )
      AllocStatus_TL(5)=CRTM_Destroy_AtmScatter( AtmOptics_TL )
      AllocStatus(5)   =CRTM_Destroy_AtmScatter( AtmOptics )
      AllocStatus_TL(4)=CRTM_Destroy_AtmScatter( AerosolScatter_TL )
      AllocStatus(4)   =CRTM_Destroy_AtmScatter( AerosolScatter )
      AllocStatus_TL(3)=CRTM_Destroy_AtmScatter( CloudScatter_TL )
      AllocStatus(3)   =CRTM_Destroy_AtmScatter( CloudScatter )
      AllocStatus_TL(2)=CRTM_Destroy_AtmAbsorption( AtmAbsorption_TL )
      AllocStatus(2)   =CRTM_Destroy_AtmAbsorption( AtmAbsorption )
      AllocStatus_TL(1)=CRTM_Destroy_Predictor( Predictor_TL )
      AllocStatus(1)   =CRTM_Destroy_Predictor( Predictor )
      IF ( ANY(AllocStatus /= SUCCESS ) .OR. ANY(AllocStatus_TL /= SUCCESS ) ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error deallocating local structures', &
                              Error_Status, &
                              Message_Log=Message_Log )
      END IF

    END DO Profile_Loop

  END FUNCTION CRTM_Tangent_Linear

END MODULE CRTM_Tangent_Linear_Module
