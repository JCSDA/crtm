!
! CRTM_K_Matrix_Module
!
! Module containing the CRTM K-matrix model function.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 28-Jan-2005
!                       paul.vandelst@noaa.gov
!
!       Modified by:    James Rosinski, 08-Feb-2019
!                       Rosinski@ucar.edu
!
! (C) Copyright 2019 UCAR
!


MODULE CRTM_K_Matrix_Module

  ! ------------
  ! Module usage
  ! ------------
  USE Type_Kinds,                 ONLY: fp, LLong
  USE Message_Handler,            ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE CRTM_Parameters,            ONLY: SET, NOT_SET, ZERO, ONE, &
                                        MAX_N_LAYERS           , &
                                        MAX_N_PHASE_ELEMENTS   , &
                                        MAX_N_LEGENDRE_TERMS   , &
                                        MAX_N_STOKES           , &
                                        MAX_N_ANGLES           , &
                                        MAX_N_AZIMUTH_FOURIER  , &
                                        MAX_SOURCE_ZENITH_ANGLE, &
                                        MAX_N_STREAMS          , &
                                        MIN_COVERAGE_THRESHOLD , &
                                        SCATTERING_ALBEDO_THRESHOLD
  USE CRTM_SpcCoeff,              ONLY: SC, &
                                        SpcCoeff_IsInfraredSensor , &
                                        SpcCoeff_IsMicrowaveSensor, &
                                        SpcCoeff_IsVisibleSensor
  USE CRTM_Atmosphere_Define,     ONLY: OPERATOR(+)                    , &
                                        CRTM_Atmosphere_type           , &
                                        CRTM_Atmosphere_Destroy        , &
                                        CRTM_Atmosphere_IsValid        , &
                                        CRTM_Atmosphere_Zero           , &
                                        CRTM_Atmosphere_AddLayerCopy   , &
                                        CRTM_Atmosphere_NonVariableCopy, &
                                        CRTM_Get_PressureLevelIdx      , &
                                        CRTM_Atmosphere_Inspect
  USE CRTM_Surface_Define,        ONLY: CRTM_Surface_type              , &
                                        CRTM_Surface_IsValid           , &
                                        CRTM_Surface_NonVariableCopy   , &
                                        CRTM_Surface_Inspect
  USE CRTM_Geometry_Define,       ONLY: CRTM_Geometry_type, &
                                        CRTM_Geometry_IsValid
  USE CRTM_ChannelInfo_Define,    ONLY: CRTM_ChannelInfo_type, &
                                        CRTM_ChannelInfo_n_Channels
  USE CRTM_RTSolution_Define,     ONLY: CRTM_RTSolution_type   , &
                                        CRTM_RTSolution_Destroy, &
                                        CRTM_RTSolution_Zero,    &
                                        CRTM_RTSolution_Inspect
  USE CRTM_Options_Define,        ONLY: CRTM_Options_type, &
                                        CRTM_Options_IsValid
  USE CRTM_Atmosphere,            ONLY: CRTM_Atmosphere_AddLayers      , &
                                        CRTM_Atmosphere_AddLayers_AD   , &
                                        CRTM_Atmosphere_IsFractional   , &
                                        CRTM_Atmosphere_Coverage       , &
                                        CRTM_Atmosphere_ClearSkyCopy   , &
                                        CRTM_Atmosphere_ClearSkyCopy_AD
  USE CRTM_GeometryInfo_Define,   ONLY: CRTM_GeometryInfo_type, &
                                        CRTM_GeometryInfo_SetValue, &
                                        CRTM_GeometryInfo_GetValue
  USE CRTM_GeometryInfo,          ONLY: CRTM_GeometryInfo_Compute
  USE CRTM_Predictor_Define,      ONLY: CRTM_Predictor_type      , &
                                        CRTM_Predictor_Associated, &
                                        CRTM_Predictor_Destroy   , &
                                        CRTM_Predictor_Create
  USE CRTM_Predictor,             ONLY: CRTM_PVar_type => iVar_type, &
                                        CRTM_Compute_Predictors    , &
                                        CRTM_Compute_Predictors_AD
  USE CRTM_AtmAbsorption,         ONLY: CRTM_AAvar_type => iVar_type , &
                                        CRTM_Compute_AtmAbsorption   , &
                                        CRTM_Compute_AtmAbsorption_AD
  USE CRTM_AtmOptics_Define,      ONLY: CRTM_AtmOptics_type      , &
                                        CRTM_AtmOptics_Associated, &
                                        CRTM_AtmOptics_Create    , &
                                        CRTM_AtmOptics_Destroy   , &
                                        CRTM_AtmOptics_Zero
  USE CRTM_AerosolScatter,        ONLY: CRTM_Compute_AerosolScatter   , &
                                        CRTM_Compute_AerosolScatter_AD
  USE CRTM_CloudScatter,          ONLY: CRTM_Compute_CloudScatter   , &
                                        CRTM_Compute_CloudScatter_AD
  USE CRTM_AtmOptics,             ONLY: AOvar_type  , &
                                        AOvar_Create, &
                                        CRTM_No_Scattering           , &
                                        CRTM_Include_Scattering      , &
                                        CRTM_Compute_Transmittance   , &
                                        CRTM_Compute_Transmittance_AD, &
                                        CRTM_AtmOptics_Combine       , &
                                        CRTM_AtmOptics_Combine_AD    , &
                                        CRTM_AtmOptics_NoScatterCopy , &
                                        CRTM_AtmOptics_NoScatterCopy_AD
  USE CRTM_SfcOptics_Define,      ONLY: OPERATOR(+)              , &
                                        CRTM_SfcOptics_type      , &
                                        CRTM_SfcOptics_Associated, &
                                        CRTM_SfcOptics_Create    , &
                                        CRTM_SfcOptics_Destroy   , &
                                        CRTM_SfcOptics_Zero, crtm_sfcoptics_inspect
  USE CRTM_SfcOptics,             ONLY: CRTM_Compute_SurfaceT   , &
                                        CRTM_Compute_SurfaceT_AD
  USE CRTM_RTSolution,            ONLY: CRTM_RTSolution_type      , &
                                        CRTM_Compute_nStreams     , &
                                        CRTM_Compute_RTSolution   , &
                                        CRTM_Compute_RTSolution_AD
  USE CRTM_AntennaCorrection,     ONLY: CRTM_Compute_AntCorr, &
                                        CRTM_Compute_AntCorr_AD
  USE CRTM_MoleculeScatter,       ONLY: CRTM_Compute_MoleculeScatter, &
                                        CRTM_Compute_MoleculeScatter_AD
  USE CRTM_AncillaryInput_Define, ONLY: CRTM_AncillaryInput_type
  USE CRTM_CloudCoeff,            ONLY: CRTM_CloudCoeff_IsLoaded
  USE CRTM_AerosolCoeff,          ONLY: CRTM_AerosolCoeff_IsLoaded
  USE CRTM_NLTECorrection,        ONLY: NLTE_Predictor_type       , &
                                        NLTE_Predictor_IsActive   , &
                                        Compute_NLTE_Predictor    , &
                                        Compute_NLTE_Predictor_AD , &
                                        Compute_NLTE_Correction   , &
                                        Compute_NLTE_Correction_AD
  USE ACCoeff_Define,             ONLY: ACCoeff_Associated
  USE NLTECoeff_Define,           ONLY: NLTECoeff_Associated
  USE CRTM_Planck_Functions,      ONLY: CRTM_Planck_Temperature   , &
                                        CRTM_Planck_Temperature_AD
  USE CRTM_CloudCover_Define,     ONLY: CRTM_CloudCover_type
  USE CRTM_Active_Sensor,         ONLY: CRTM_Compute_Reflectivity, &
                                        CRTM_Compute_Reflectivity_AD, &
                                        Calculate_Cloud_Water_Density

  ! Internal variable definition modules
  ! ...AtmOptics
  USE AOvar_Define, ONLY: AOvar_type, &
                          AOvar_Associated, &
                          AOvar_Destroy   , &
                          AOvar_Create
  ! ...CloudScatter
  USE CSvar_Define, ONLY: CSvar_type, &
                          CSvar_Associated, &
                          CSvar_Destroy   , &
                          CSvar_Create
  ! ...AerosolScatter
  USE ASvar_Define, ONLY: ASvar_type, &
                          ASvar_Associated, &
                          ASvar_Destroy   , &
                          ASvar_Create
  ! ...Radiative transfer
  USE RTV_Define,   ONLY: RTV_type      , &
                          RTV_Associated, &
                          RTV_Destroy   , &
                          RTV_Create

  ! ...OpenMP API
  USE OMP_LIB

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


CONTAINS


!--------------------------------------------------------------------------------
!:sdoc+:
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
!       Error_Status = CRTM_K_Matrix( Atmosphere       , &
!                                     Surface          , &
!                                     RTSolution_K     , &
!                                     Geometry         , &
!                                     ChannelInfo      , &
!                                     Atmosphere_K     , &
!                                     Surface_K        , &
!                                     RTSolution       , &
!                                     Options = Options  )
!
! INPUTS:
!       Atmosphere:     Structure containing the Atmosphere data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Rank-1 (n_Profiles)
!                       ATTRIBUTES: INTENT(IN)
!
!       Surface:        Structure containing the Surface data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Surface_type
!                       DIMENSION:  Same as input Atmosphere argument.
!                       ATTRIBUTES: INTENT(IN)
!
!       RTSolution_K:   Structure containing the RT solution K-matrix inputs.
!                       **NOTE: On EXIT from this function, the contents of
!                               this structure may be modified (e.g. set to
!                               zero.)
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Rank-2 (n_Channels x n_Profiles)
!                       ATTRIBUTES: INTENT(IN OUT)
!
!       Geometry:       Structure containing the view geometry
!                       information.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Geometry_type
!                       DIMENSION:  Same as input Atmosphere argument
!                       ATTRIBUTES: INTENT(IN)
!
!       ChannelInfo:    Structure returned from the CRTM_Init() function
!                       that contains the satellite/sesnor channel index
!                       information.
!                       UNITS:      N/A
!                       TYPE:       CRTM_ChannelInfo_type
!                       DIMENSION:  Rank-1 (n_Sensors)
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Options:        Options structure containing the optional forward model
!                       arguments for the CRTM.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Options_type
!                       DIMENSION:  Same as input Atmosphere structure
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUTS:
!       Atmosphere_K:   Structure containing the K-matrix Atmosphere data.
!                       **NOTE: On ENTRY to this function, the contents of
!                               this structure should be defined (e.g.
!                               initialized to some value based on the
!                               position of this function in the call chain.)
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Same as input RTSolution_K argument
!                       ATTRIBUTES: INTENT(IN OUT)
!
!       Surface_K:      Structure containing the tangent-linear Surface data.
!                       **NOTE: On ENTRY to this function, the contents of
!                               this structure should be defined (e.g.
!                               initialized to some value based on the
!                               position of this function in the call chain.)
!                       UNITS:      N/A
!                       TYPE:       CRTM_Surface_type
!                       DIMENSION:  Same as input RTSolution_K argument
!                       ATTRIBUTES: INTENT(IN OUT)
!
!       RTSolution:     Structure containing the solution to the RT equation
!                       for the given inputs.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Same as input RTSolution_K argument
!                       ATTRIBUTES: INTENT(IN OUT)
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
!      Note that the input K-matrix arguments are modified upon exit, and
!      the output K-matrix arguments must be defined upon entry. This is
!      a consequence of the K-matrix formulation where, effectively, the
!      chain rule is being used and this funtion could reside anywhere
!      in the chain of derivative terms.
!
! COMMENTS:
!       - The Options optional structure arguments contain
!         spectral information (e.g. emissivity) that must have the same
!         spectral dimensionality (the "L" dimension) as the RTSolution
!         structures.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_K_Matrix( &
    Atmosphere  , &  ! FWD Input, M
    Surface     , &  ! FWD Input, M
    RTSolution_K, &  ! K   Input, L x M
    Geometry    , &  ! Input, M
    ChannelInfo , &  ! Input, n_Sensors
    Atmosphere_K, &  ! K   Output, L x M
    Surface_K   , &  ! K   Output, L x M
    RTSolution  , &  ! FWD Output, L x M
    Options     ) &  ! Optional FWD input,  M
  RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Atmosphere_type)       , INTENT(IN OUT) :: Atmosphere(:)     ! M
    TYPE(CRTM_Surface_type)          , INTENT(IN)     :: Surface(:)        ! M
    TYPE(CRTM_RTSolution_type)       , INTENT(IN OUT) :: RTSolution_K(:,:) ! L x M
    TYPE(CRTM_Geometry_type)         , INTENT(IN)     :: Geometry(:)       ! M
    TYPE(CRTM_ChannelInfo_type)      , INTENT(IN)     :: ChannelInfo(:)    ! n_Sensors
    TYPE(CRTM_Atmosphere_type)       , INTENT(IN OUT) :: Atmosphere_K(:,:) ! L x M
    TYPE(CRTM_Surface_type)          , INTENT(IN OUT) :: Surface_K(:,:)    ! L x M
    TYPE(CRTM_RTSolution_type)       , INTENT(IN OUT) :: RTSolution(:,:)   ! L x M
    TYPE(CRTM_Options_type), OPTIONAL, INTENT(IN)     :: Options(:)        ! M
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_K_Matrix'
    ! Local variables
    CHARACTER(256) :: Message
    LOGICAL :: Options_Present
    INTEGER :: n_Sensors, nc
    INTEGER :: n_Channels
    INTEGER :: m, n_Profiles
    ! Local ancillary input structure
    TYPE(CRTM_AncillaryInput_type) :: AncillaryInput
    ! Local options structure for default values
    TYPE(CRTM_Options_type) :: Default_Options, Opt

    ! Local variables required by threading, timing, and output verification
    INTEGER(LLong) :: count_rate, count_start, count_end
    REAL :: elapsed
    REAL :: elapsed_running = 0.         ! running total of elapsed times
    LOGICAL, PARAMETER :: enable_timing = .FALSE.
    LOGICAL, PARAMETER :: output_verification = .FALSE.
    INTEGER :: ret(SIZE(Atmosphere))     ! return codes from profile_solution
    INTEGER :: nfailure                  ! number of non-success calls to profile_solution

    ! OpenMP-related variables
    INTEGER :: n_omp_threads
    INTEGER :: n_profile_threads
    INTEGER :: n_channel_threads

    ! ------
    ! SET UP
    ! ------
    Error_Status = SUCCESS
    IF (enable_timing) THEN
      CALL SYSTEM_CLOCK (count_rate=count_rate)
      CALL SYSTEM_CLOCK (count=count_start)
    END IF

    ! If no sensors or channels, simply return
    n_Sensors  = SIZE(ChannelInfo)
    n_Channels = SUM(CRTM_ChannelInfo_n_Channels(ChannelInfo))
    IF ( n_Sensors == 0 .OR. n_Channels == 0 ) RETURN


    ! Check spectral arrays
    IF ( SIZE(RTSolution  ,DIM=1) < n_Channels .OR. &
         SIZE(Atmosphere_K,DIM=1) < n_Channels .OR. &
         SIZE(Surface_K   ,DIM=1) < n_Channels .OR. &
         SIZE(RTSolution_K,DIM=1) < n_Channels      ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("RTSolution and K-matrix (Atm,Sfc,RT) structure arrays too small (",&
             &3(i0,","),i0,") for the number of requested channels (",i0,")")') &
             SIZE(RTSolution  ,DIM=1), &
             SIZE(Atmosphere_K,DIM=1), &
             SIZE(Surface_K   ,DIM=1), &
             SIZE(RTSolution_K,DIM=1), &
             n_Channels
      CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
      RETURN
    END IF


    ! Check the number of profiles
    ! ...Number of atmospheric profiles.
    n_Profiles = SIZE(Atmosphere)

    ! ...Check the profile dimensionality of the other mandatory arguments
    IF ( SIZE(Surface)            /= n_Profiles .OR. &
         SIZE(RTSolution_K,DIM=2) /= n_Profiles .OR. &
         SIZE(Geometry)           /= n_Profiles .OR. &
         SIZE(Atmosphere_K,DIM=2) /= n_Profiles .OR. &
         SIZE(Surface_K   ,DIM=2) /= n_Profiles .OR. &
         SIZE(RTSolution  ,DIM=2) /= n_Profiles      ) THEN
      Error_Status = FAILURE
      Message = 'Inconsistent profile dimensionality for input arguments.'
      CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
      RETURN
    END IF
    ! ...Check the profile dimensionality of the other optional arguments
    Options_Present = .FALSE.
    IF ( PRESENT( Options ) ) THEN
      Options_Present = .TRUE.
      IF ( SIZE( Options ) /= n_Profiles ) THEN
        Error_Status = FAILURE
        Message = 'Inconsistent profile dimensionality for Options optional input argument.'
        CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
        RETURN
      END IF
    END IF

    ! -------
    ! OpenMP
    ! -------
!$OMP PARALLEL
!$OMP SINGLE
    n_omp_threads = OMP_GET_NUM_THREADS()
!$OMP END SINGLE
!$OMP END PARALLEL

    ! Determine how many threads to use for profiles and channels
    ! After profiles get what they need, we use the left-over threads
    ! to parallelize channels
    IF ( n_Profiles >= n_omp_threads ) THEN
      n_profile_threads = n_omp_threads
      n_channel_threads = 1
      CALL OMP_SET_MAX_ACTIVE_LEVELS(1)
    ELSE

      n_profile_threads = n_Profiles  !** e.g, NOMP = 8, n_profiles = 2 -> n_profile_threads = 2
      !** if n_channels < (n_omp_threads-n_profile_threads), no need to use additional threads
      n_channel_threads = MIN(n_channels, MAX(1,n_omp_threads-n_profile_threads))
      IF (n_profile_threads + n_channel_threads > n_omp_threads) THEN
         ! throw an error
         WRITE( Message,'("ERROR: n_profile_threads + n_channel_threads > n_omp_threads",3i3)' ) &
              n_profile_threads, n_channel_threads, n_omp_threads
         CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
      END IF
      if(n_channel_threads > 1) THEN
        CALL OMP_SET_MAX_ACTIVE_LEVELS(2)
      ELSE
        CALL OMP_SET_MAX_ACTIVE_LEVELS(1)
      END IF
    END IF

    WRITE(6,*)
    WRITE(6,'("   Using",i3," OpenMP threads =",i3," for profiles and",i3," for channels.")') &
         n_omp_threads, n_profile_threads, n_channel_threads

    ! ------------
    ! PROFILE LOOPS
    ! ------------

!JR First loop just checks validity of Atmosphere(m) contents
!$OMP PARALLEL DO PRIVATE ( nc, Message ) NUM_THREADS( n_profile_threads )
    Profile_Loop1: DO m = 1, n_Profiles
      ! Check the cloud and aerosol coeff. data for cases with clouds and aerosol
       IF ( Atmosphere(m)%n_Clouds > 0) THEN
          !** clear clouds where cloud_fraction < threshold
          DO nc = 1, Atmosphere(m)%n_clouds
             WHERE (Atmosphere(m)%Cloud_Fraction(:) < MIN_COVERAGE_THRESHOLD)
                Atmosphere(m)%Cloud_Fraction(:) = ZERO
                Atmosphere(m)%Cloud(nc)%Water_Content(:)    = ZERO
                Atmosphere(m)%Cloud(nc)%Effective_Radius(:) = ZERO
             END WHERE
          END DO


          IF(.NOT. CRTM_CloudCoeff_IsLoaded() )THEN

             Error_Status = FAILURE
             WRITE( Message,'("The CloudCoeff data must be loaded (with CRTM_Init routine) ", &
                  &"for the cloudy case profile #",i0)' ) m
             CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
             CYCLE Profile_Loop1
          END IF
       END IF
      IF( Atmosphere(m)%n_Aerosols > 0 .AND. .NOT. CRTM_AerosolCoeff_IsLoaded() )THEN
         Error_Status = FAILURE
         WRITE( Message,'("The AerosolCoeff data must be loaded (with CRTM_Init routine) ", &
                &"for the aerosol case profile #",i0)' ) m
         CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
         CYCLE Profile_Loop1
      END IF
    END DO Profile_Loop1
!$OMP END PARALLEL DO

    IF (Error_Status == FAILURE) THEN
       RETURN
    END IF

!$OMP PARALLEL DO PRIVATE ( m, Opt, AncillaryInput ) NUM_THREADS( n_profile_threads ) SCHEDULE ( runtime )
    Profile_Loop2: DO m = 1, n_Profiles
      ! Check the optional Options structure argument
      Opt = Default_Options
      IF ( Options_Present ) THEN
        Opt = Options(m)
        ! Copy over ancillary input
        AncillaryInput%SSU    = Options(m)%SSU
        AncillaryInput%Zeeman = Options(m)%Zeeman
      END IF
      ret(m) = profile_solution (m, Opt, AncillaryInput)
    END DO Profile_Loop2
!$OMP END PARALLEL DO
    nfailure = COUNT (ret(:) /= SUCCESS)
    IF (nfailure > 0) THEN
      Error_Status = FAILURE
      WRITE(Message,'(i0," profiles failed")') nfailure
      CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
      RETURN
    END IF

    IF (enable_timing) THEN
      CALL SYSTEM_CLOCK (count=count_end)
      elapsed = REAL (count_end - count_start) / REAL (count_rate)
      elapsed_running = elapsed_running + elapsed
      WRITE(6,*) 'CRTM_K_Matrix elapsed              =',elapsed
      WRITE(6,*) 'CRTM_K_Matrix elapsed running total=',elapsed_running
    END IF

    IF (output_verification) THEN
      WRITE(6,*)'CRTM_K_Matrix inspecting RTSolution...'
      CALL CRTM_RTSolution_Inspect (RTSolution(:,:))
      WRITE(6,*)'CRTM_K_Matrix inspecting RTSolution_K...'
      CALL CRTM_RTSolution_Inspect (RTSolution_K(:,:))
      WRITE(6,*)'CRTM_K_Matrix inspecting Atmosphere_K...'
      CALL CRTM_Atmosphere_Inspect (Atmosphere_K(:,:))
      WRITE(6,*)'CRTM_K_Matrix inspecting Surface_K...'
      CALL CRTM_Surface_Inspect (Surface_K(:,:))
    END IF
    RETURN


  CONTAINS

    ! Function profile_solution contains all the computational code inside of CRTM_K_Matrix that
    ! is contained inside an OMP loop. It is "contain"ed inside the function mainly so it can
    ! access arrays which are arguments to CRTM_K_Matrix. Subroutines Pre_Process_RTSolution_K
    ! and Post_Process_RTSolution_K also access CRTM_K_Matrix data, but multi-level function
    ! "contain" clauses cause compiler errors so arguments to these functions were needed.
    FUNCTION profile_solution (m, Opt, AncillaryInput) RESULT( Error_Status )
      INTEGER, INTENT(in) :: m               ! profile index
      TYPE(CRTM_Options_type), INTENT(IN) :: Opt
      TYPE(CRTM_AncillaryInput_type), INTENT(IN) :: AncillaryInput

      ! Local variables
      INTEGER :: Error_Status
      CHARACTER(256) :: Message
      LOGICAL :: compute_antenna_correction
      LOGICAL :: Atmosphere_Invalid, Surface_Invalid, Geometry_Invalid, Options_Invalid
      INTEGER :: Status_FWD, Status_K
      INTEGER :: iFOV
      INTEGER :: n, l    ! sensor index, channel index
      INTEGER :: SensorIndex
      INTEGER :: ChannelIndex
      INTEGER :: ln
      INTEGER :: n_Full_Streams, mth_Azi
      INTEGER :: cloud_coverage_flag
      REAL(fp) :: Source_ZA
      REAL(fp) :: Wavenumber
      REAL(fp) :: transmittance, transmittance_K
      REAL(fp) :: transmittance_clear, transmittance_clear_K
      REAL(fp) :: r_cloudy

      ! Local openMP related variables
      INTEGER :: nt, start_ch, end_ch, chunk_ch, n_sensor_channels
      INTEGER :: n_inactive_channels(n_channel_threads)

      ! Local atmosphere structure for extra layering
      TYPE(CRTM_Atmosphere_type) :: Atm, Atm_K(n_channel_threads)

      ! Clear sky structures
      TYPE(CRTM_Atmosphere_type) :: Atm_Clear       , Atm_Clear_K(n_channel_threads)
      TYPE(CRTM_AtmOptics_type)  :: AtmOptics_Clear(n_channel_threads) , AtmOptics_Clear_K(n_channel_threads)
      TYPE(CRTM_SfcOptics_type)  :: SfcOptics_Clear(n_channel_threads) , SfcOptics_Clear_K(n_channel_threads)
      TYPE(CRTM_RTSolution_type) :: RTSolution_Clear(n_channel_threads), RTSolution_Clear_K(n_channel_threads)
      TYPE(RTV_type)             :: RTV_Clear(n_channel_threads)
      ! Component variables
      TYPE(CRTM_GeometryInfo_type) :: GeometryInfo
      TYPE(CRTM_Predictor_type)    :: Predictor(n_channel_threads), Predictor_K(n_channel_threads)
      TYPE(CRTM_AtmOptics_type)    :: AtmOptics(n_channel_threads), AtmOptics_K(n_channel_threads)
      TYPE(CRTM_SfcOPtics_type)    :: SfcOptics(n_channel_threads), SfcOptics_K(n_channel_threads)
      ! Component variable internals
      TYPE(CRTM_PVar_type)  :: PVar(n_channel_threads)   ! Predictor
      TYPE(CRTM_AAvar_type) :: AAvar(n_channel_threads)  ! AtmAbsorption
      TYPE(CSvar_type)      :: CSvar(n_channel_threads)  ! CloudScatter
      TYPE(ASvar_type)      :: ASvar(n_channel_threads)  ! AerosolScatter
      TYPE(AOvar_type)      :: AOvar(n_channel_threads)  ! AtmOptics
      TYPE(RTV_type)        :: RTV(n_channel_threads)    ! RTSolution
      ! NLTE correction term predictors
      TYPE(NLTE_Predictor_type) :: NLTE_Predictor, NLTE_Predictor_K(n_channel_threads)
      ! Cloud cover object
      TYPE(CRTM_CloudCover_type) :: CloudCover, CloudCover_K(n_channel_threads)

      Error_Status = SUCCESS

      ! Silence gfortran complaints about maybe-used-uninit by init to huge()
      r_cloudy = HUGE(r_cloudy)

      ! Reinitialise the output RTSolution
      CALL CRTM_RTSolution_Zero(RTSolution(:,m))

      ! Allocate the profile independent surface optics local structure
!$OMP PARALLEL DO NUM_THREADS(n_channel_threads) PRIVATE(Message)
      DO nt = 1, n_channel_threads
        CALL CRTM_SfcOptics_Create( SfcOptics(nt)  , MAX_N_ANGLES, MAX_N_STOKES )
        CALL CRTM_SfcOptics_Create( SfcOptics_K(nt), MAX_N_ANGLES, MAX_N_STOKES )
        IF ( (.NOT. CRTM_SfcOptics_Associated(SfcOptics(nt)  )) .OR. &
             (.NOT. CRTM_SfcOptics_Associated(SfcOptics_K(nt))) ) THEN
          Error_Status = FAILURE
          Message = 'Error allocating SfcOptics data structures'
          CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
          CYCLE
        END IF
      END DO
!$OMP END PARALLEL DO
      IF ( Error_Status == FAILURE) RETURN
      ! Copy over forward "non-variable" inputs to K-matrix outputs
!$OMP PARALLEL DO NUM_THREADS(n_channel_threads)
      DO l = 1, n_Channels
        CALL CRTM_Atmosphere_NonVariableCopy( Atmosphere(m), Atmosphere_K(l,m) )
        CALL CRTM_Surface_NonVariableCopy( Surface(m), Surface_K(l,m) )
      END DO
!$OMP END PARALLEL DO
      ! ...Assign the option specific SfcOptics input
!$OMP PARALLEL DO NUM_THREADS(n_channel_threads)
      DO nt = 1, n_channel_threads
        SfcOptics(nt)%Use_New_MWSSEM = .NOT. Opt%Use_Old_MWSSEM
      END DO
!$OMP END PARALLEL DO
      ! Check whether to skip this profile
      IF ( Opt%Skip_Profile ) RETURN

      ! Check the input data if required
      IF ( Opt%Check_Input ) THEN
        ! ...Mandatory inputs
        Atmosphere_Invalid = .NOT. CRTM_Atmosphere_IsValid( Atmosphere(m) )
        Surface_Invalid    = .NOT. CRTM_Surface_IsValid( Surface(m) )
        Geometry_Invalid   = .NOT. CRTM_Geometry_IsValid( Geometry(m) )
        IF ( Atmosphere_Invalid .OR. Surface_Invalid .OR. Geometry_Invalid ) THEN
          Error_Status = FAILURE
          WRITE( Message,'("Input data check failed for profile #",i0)' ) m
          CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
          RETURN
        END IF
        ! ...Optional input
        IF ( Options_Present ) THEN
          Options_Invalid = .NOT. CRTM_Options_IsValid( Options(m) )
          IF ( Options_Invalid ) THEN
            Error_Status = FAILURE
            WRITE( Message,'("Options data check failed for profile #",i0)' ) m
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            RETURN
          END IF
          ! Are the channel dimensions consistent if emissivity is passed?
          IF ( Options(m)%Use_Emissivity ) THEN
            IF ( Options(m)%n_Channels < n_Channels ) THEN
              Error_Status = FAILURE
              WRITE( Message,'( "Input Options channel dimension (", i0, ") is less ", &
                     &"than the number of requested channels (",i0, ")" )' ) &
                     Options(m)%n_Channels, n_Channels
              CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
              RETURN
            END IF
          END IF
          ! Check value for user-defined n_Streams
          IF ( Options(m)%Use_N_Streams ) THEN
            IF ( Options(m)%n_Streams <= 0 .OR. MOD(Options(m)%n_Streams,2) /= 0 .OR. &
                 Options(m)%n_Streams > MAX_N_STREAMS ) THEN
                Error_Status = FAILURE
                WRITE( Message,'( "Input Options n_Streams (", i0, ") is invalid" )' ) &
                       Options(m)%n_Streams
                CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
                RETURN
            END IF
          END IF
        END IF
      END IF

      ! Process geometry
      ! ...Compute derived geometry
      CALL CRTM_GeometryInfo_SetValue( GeometryInfo, Geometry=Geometry(m) )
      CALL CRTM_GeometryInfo_Compute( GeometryInfo )
      ! ...Retrieve components into local variable
      CALL CRTM_GeometryInfo_GetValue( &
             GeometryInfo, &
             iFOV = iFOV, &
             Source_Zenith_Angle = Source_ZA )

      ! Add extra layers to current atmosphere profile
      ! if necessary to handle upper atmosphere
      Error_Status = CRTM_Atmosphere_AddLayers( Atmosphere(m), Atm )
      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error adding FWD extra layers to profile #",i0)' ) m
        CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
        RETURN
      END IF
      ! ...Check the total number of Atm layers
      IF ( Atm%n_Layers > MAX_N_LAYERS ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Added layers [",i0,"] cause total [",i0,"] to exceed the ",&
               &"maximum allowed [",i0,"] for profile #",i0)' ) &
               Atm%n_Added_Layers, Atm%n_Layers, MAX_N_LAYERS, m
        CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
        RETURN
      END IF

      ! Calculate cloud water density
      CALL Calculate_Cloud_Water_Density(Atm)

      ! Prepare the atmospheric optics structures
      ! ...Allocate the atmospheric optics structures based on Atm extension

!$OMP PARALLEL DO NUM_THREADS(n_channel_threads) PRIVATE(Message)
      DO nt = 1, n_channel_threads
        CALL CRTM_AtmOptics_Create( AtmOptics(nt), &
                                    Atm%n_Layers        , &
                                    MAX_N_LEGENDRE_TERMS, &
                                    MAX_N_PHASE_ELEMENTS  )
        CALL CRTM_AtmOptics_Create( AtmOptics_K(nt), &
                                    Atm%n_Layers        , &
                                    MAX_N_LEGENDRE_TERMS, &
                                    MAX_N_PHASE_ELEMENTS  )
        IF ( .NOT. CRTM_AtmOptics_Associated( Atmoptics(nt) ) .OR. &
             .NOT. CRTM_AtmOptics_Associated( Atmoptics_K(nt) ) ) THEN
          Error_Status = FAILURE
          WRITE( Message,'("Error allocating AtmOptics data structures for profile #",i0)' ) m
          CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
          CYCLE
        END IF
        ! ...Set the Scattering Switch
        AtmOptics(nt)%Include_Scattering   = Opt%Include_Scattering
        AtmOptics_K(nt)%Include_Scattering = Opt%Include_Scattering
        ! ...Allocate the atmospheric optics internal structure
        CALL AOvar_Create( AOvar(nt), Atm%n_Layers )


        ! Allocate the scattering internal variables if necessary
        ! ...Cloud
        IF ( Atm%n_Clouds > 0 ) THEN
          CALL CSvar_Create( CSvar(nt), &
                             MAX_N_LEGENDRE_TERMS, &
                             MAX_N_PHASE_ELEMENTS, &
                             Atm%n_Layers        , &
                             Atm%n_Clouds          )
        END IF
        ! ...Aerosol
        IF ( Atm%n_Aerosols > 0 ) THEN
          CALL ASvar_Create( ASvar(nt), &
                             MAX_N_LEGENDRE_TERMS, &
                             MAX_N_PHASE_ELEMENTS, &
                             Atm%n_Layers        , &
                             Atm%n_Aerosols        )
        END IF
      END DO
!$OMP END PARALLEL DO
      IF ( Error_Status == FAILURE) RETURN

      ! Determine the type of cloud coverage
      cloud_coverage_flag = CRTM_Atmosphere_Coverage( atm )


      ! Setup for fractional cloud coverage
      IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN

        ! Compute cloudcover
        Error_Status = CloudCover%Compute_CloudCover(atm, Overlap = opt%Overlap_Id)
        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message,'("Error computing cloud cover in profile #",i0)' ) m
          CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
          RETURN
        END IF
        ! ...Mold the K-matrix object based on the forward, and reinitialise
!$OMP PARALLEL DO NUM_THREADS(n_channel_threads)
        DO nt = 1, n_channel_threads
          CloudCover_K(nt) = CloudCover
          CALL CloudCover_K(nt)%Set_To_Zero()
        END DO
!$OMP END PARALLEL DO

        ! Allocate some of the CLEAR sky structure for fractional cloud coverage
        ! (The AtmOptics structures are allocated during a copy)
        ! ...Clear sky atmosphere
        Error_Status = CRTM_Atmosphere_ClearSkyCopy(Atm, Atm_Clear)
        IF ( Error_Status /= SUCCESS  ) THEN
          Error_status = FAILURE
          WRITE( Message,'("Error copying CLEAR SKY Atmosphere structures for profile #",i0)' ) m
          CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
          RETURN
        END IF

!$OMP PARALLEL DO NUM_THREADS(n_channel_threads) PRIVATE(Message)
        DO nt = 1, n_channel_threads
          ! ...Clear sky SfcOptics
          CALL CRTM_SfcOptics_Create( SfcOptics_Clear(nt)  , MAX_N_ANGLES, MAX_N_STOKES )
          CALL CRTM_SfcOptics_Create( SfcOptics_Clear_K(nt), MAX_N_ANGLES, MAX_N_STOKES )
          IF ( (.NOT. CRTM_SfcOptics_Associated(SfcOptics_Clear(nt))) .OR. &
               (.NOT. CRTM_SfcOptics_Associated(SfcOptics_Clear_K(nt)))) THEN
            Error_Status = FAILURE
            WRITE( Message,'("Error allocating CLEAR SKY SfcOptics data structures for profile #",i0)' ) m
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            CYCLE
          END IF
          ! ...Copy over surface optics input
          SfcOptics_Clear(nt)%Use_New_MWSSEM = .NOT. Opt%Use_Old_MWSSEM
          ! ...CLEAR SKY average surface skin temperature for multi-surface types
          CALL CRTM_Compute_SurfaceT( Surface(m), SfcOptics_Clear(nt) )
        END DO
!$OMP END PARALLEL DO

        IF ( Error_Status == FAILURE) RETURN

      END IF


      ! Average surface skin temperature for multi-surface types
!$OMP PARALLEL DO NUM_THREADS(n_channel_threads)
      DO nt = 1, n_channel_threads
        CALL CRTM_Compute_SurfaceT( Surface(m), SfcOptics(nt) )
      END DO
!$OMP END PARALLEL DO

      ! -----------
      ! SENSOR LOOP
      ! -----------
      ! Initialise channel counter for sensor(n)/channel(l) count

      ln = 0
      Sensor_Loop: DO n = 1, n_Sensors
        ! Shorter name
        SensorIndex = ChannelInfo(n)%Sensor_Index

        ! Check if antenna correction to be applied for current sensor
        compute_antenna_correction = ( Opt%Use_Antenna_Correction               .AND. &
                                       ACCoeff_Associated( SC(SensorIndex)%AC ) .AND. &
                                       iFOV /= 0 )


!$OMP PARALLEL DO NUM_THREADS(n_channel_threads) PRIVATE(Message)
        DO nt = 1, n_channel_threads
          ! Allocate the AtmAbsorption predictor structures
          CALL CRTM_Predictor_Create( &
                 Predictor(nt)   , &
                 atm%n_Layers, &
                 SensorIndex , &
                 SaveFWV = 1   )
          CALL CRTM_Predictor_Create( &
                 Predictor_K(nt) , &
                 atm%n_Layers, &
                 SensorIndex   )
          IF ( (.NOT. CRTM_Predictor_Associated(Predictor(nt))) .OR. &
               (.NOT. CRTM_Predictor_Associated(Predictor_K(nt))) ) THEN
            Error_Status=FAILURE
            WRITE( Message,'("Error allocating predictor structures for profile #",i0, &
                   &" and ",a," sensor.")' ) m, SC(SensorIndex)%Sensor_Id
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            CYCLE
          END IF


          ! ...Compute forward predictors
          CALL CRTM_Compute_Predictors( SensorIndex   , &  ! Input
                                        Atm           , &  ! Input
                                        GeometryInfo  , &  ! Input
                                        AncillaryInput, &  ! Input
                                        Predictor(nt)     , &  ! Output
                                        PVar(nt)            )  ! Internal variable output


          ! Allocate the RTV structure if necessary
          IF( ( Atm%n_Clouds   > 0 .OR. &
                Atm%n_Aerosols > 0 .OR. &
                SpcCoeff_IsVisibleSensor(SC(SensorIndex)) ) .AND. &
              AtmOptics(nt)%Include_Scattering ) THEN
            CALL RTV_Create( RTV(nt), MAX_N_ANGLES, MAX_N_LEGENDRE_TERMS, Atm%n_Layers )
            IF ( .NOT. RTV_Associated(RTV(nt)) ) THEN
              Error_Status=FAILURE
              WRITE( Message,'("Error allocating RTV structure for profile #",i0, &
                     &" and ",a," sensor.")' ) m, TRIM(SC(SensorIndex)%Sensor_Id)
              CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
              CYCLE
            END IF
            ! Assign algorithm selector
            RTV(nt)%RT_Algorithm_Id = Opt%RT_Algorithm_Id
          END IF

        END DO
!$OMP END PARALLEL DO
        IF ( Error_Status == FAILURE) RETURN

        ! Compute NLTE correction predictors
        IF ( Opt%Apply_NLTE_Correction ) THEN
          CALL Compute_NLTE_Predictor( &
                 SC(SensorIndex)%NC, &  ! Input
                 Atm               , &  ! Input
                 GeometryInfo      , &  ! Input
                 NLTE_Predictor      )  ! Output
        END IF


        ! ----------------------------
        ! counters for thread loop
        ! -----------------------------
        n_sensor_channels = ChannelInfo(n)%n_Channels
        chunk_ch = n_sensor_channels / n_channel_threads

        !count inactive channels in each chunk
        n_inactive_channels(:) = 0
        nt = 1
        DO l = 1, n_sensor_channels-1
          IF ( .NOT. ChannelInfo(n)%Process_Channel(l) ) THEN
             n_inactive_channels(nt) = n_inactive_channels(nt) + 1
             nt = MIN((l / chunk_ch + 1), n_channel_threads)
          END IF
        END DO
        DO nt = 2, n_channel_threads
          n_inactive_channels(nt) = n_inactive_channels(nt) + n_inactive_channels(nt - 1)
        END DO
        DO nt = n_channel_threads, 2, -1
          n_inactive_channels(nt) = n_inactive_channels(nt - 1)
        END DO
        n_inactive_channels(1) = 0
        !end count
        ! ------------
        ! THREAD LOOP
        ! ------------
!$OMP PARALLEL DO NUM_THREADS(n_channel_threads)                        &
!$OMP    FIRSTPRIVATE(ln, r_cloudy)                                     &
!$OMP    PRIVATE(Message, ChannelIndex, n_Full_Streams, PVar, AAvar,    &
!$OMP          start_ch, end_ch, Wavenumber, Status_FWD, Status_K,      &
!$OMP          transmittance, transmittance_K, transmittance_clear,     &
!$OMP          transmittance_clear_K, l, mth_Azi)
        Thread_Loop: DO nt = 1, n_channel_threads
          start_ch = (nt - 1) * chunk_ch + 1
          IF ( nt == n_channel_threads) THEN
            end_ch = n_sensor_channels
          ELSE
            end_ch = start_ch + chunk_ch - 1
          END IF
          ln = (start_ch - 1) - n_inactive_channels(nt)

          ! -------------
          ! CHANNEL LOOP
          ! -------------
          Channel_Loop: DO l = start_ch, end_ch
            ! Channel setup
            ! ...Skip channel if requested
            IF ( .NOT. ChannelInfo(n)%Process_Channel(l) ) CYCLE Channel_Loop
            ! ...Shorter name
            ChannelIndex = ChannelInfo(n)%Channel_Index(l)
            ! ...Increment the processed channel counter
            ln = ln + 1
            ! ...Assign sensor+channel information to output
            RTSolution(ln,m)%Sensor_Id        = ChannelInfo(n)%Sensor_Id
            RTSolution(ln,m)%WMO_Satellite_Id = ChannelInfo(n)%WMO_Satellite_Id
            RTSolution(ln,m)%WMO_Sensor_Id    = ChannelInfo(n)%WMO_Sensor_Id
            RTSolution(ln,m)%Sensor_Channel   = ChannelInfo(n)%Sensor_Channel(l)

            RTSolution_K(ln,m)%Sensor_Id        = RTSolution(ln,m)%Sensor_Id
            RTSolution_K(ln,m)%WMO_Satellite_Id = RTSolution(ln,m)%WMO_Satellite_Id
            RTSolution_K(ln,m)%WMO_Sensor_Id    = RTSolution(ln,m)%WMO_Sensor_Id
            RTSolution_K(ln,m)%Sensor_Channel   = RTSolution(ln,m)%Sensor_Channel
            ! ...Same for clear structures
            RTSolution_Clear(nt)%Sensor_Id        = RTSolution(ln,m)%Sensor_Id
            RTSolution_Clear(nt)%WMO_Satellite_Id = RTSolution(ln,m)%WMO_Satellite_Id
            RTSolution_Clear(nt)%WMO_Sensor_Id    = RTSolution(ln,m)%WMO_Sensor_Id
            RTSolution_Clear(nt)%Sensor_Channel   = RTSolution(ln,m)%Sensor_Channel
            RTSolution_Clear_K(nt)%Sensor_Id        = RTSolution(ln,m)%Sensor_Id
            RTSolution_Clear_K(nt)%WMO_Satellite_Id = RTSolution(ln,m)%WMO_Satellite_Id
            RTSolution_Clear_K(nt)%WMO_Sensor_Id    = RTSolution(ln,m)%WMO_Sensor_Id
            RTSolution_Clear_K(nt)%Sensor_Channel   = RTSolution(ln,m)%Sensor_Channel


            ! Initialisations
            CALL CRTM_AtmOptics_Zero( AtmOptics(nt) )
            CALL CRTM_AtmOptics_Zero( AtmOptics_K(nt) )
            CALL CRTM_AtmOptics_Zero( AtmOptics_Clear(nt) )
            CALL CRTM_AtmOptics_Zero( AtmOptics_Clear_K(nt) )
            transmittance_K = ZERO
            CALL CRTM_RTSolution_Zero( RTSolution_Clear(nt) )
            CALL CRTM_RTSolution_Zero( RTSolution_Clear_K(nt) )

            ! Copy the input K-matrix atmosphere with extra layers if necessary
            Atm_K(nt) = CRTM_Atmosphere_AddLayerCopy( Atmosphere_K(ln,m), Atm%n_Added_Layers )
            ! ...Same for K-matrix CLEAR sky structure for fractional cloud coverage
            IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
              Error_Status = CRTM_Atmosphere_ClearSkyCopy(Atm_K(nt), Atm_Clear_K(nt))
              IF ( Error_Status /= SUCCESS  ) THEN
                Error_status = FAILURE
                WRITE( Message,'("Error copying CLEAR SKY Atmosphere_K structure for ",a,&
                       &", channel ",i0,", profile #",i0)') &
                       TRIM(ChannelInfo(n)%Sensor_ID), &
                       ChannelInfo(n)%Sensor_Channel(l), &
                       m
                CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
                CYCLE Thread_Loop
              END IF
              CALL CRTM_Atmosphere_Zero( Atm_Clear_K(nt) )
            END IF
            
            !CALL Calculate_Cloud_Water_Density(Atm, Atm_K(nt))
            Atm_K(nt)%Height = Atm%Height

            ! Determine the number of streams (n_Full_Streams) in up+downward directions
            IF ( Opt%Use_N_Streams ) THEN
              ! 1. AtmOptics(nt)%n_Legendre_Terms = n_Full_Streams = the number of pcoeff
              !    terms used to reconstruct the phase function;
              ! 2. AerosolCoeff and CloudCoeff LUTs Require at least 4 terms to properly
              !    reconstruct the phase function and set up truncation factor.
              n_Full_Streams = max(4, Opt%n_Streams)
              RTSolution(ln,m)%n_Full_Streams = max(4, Opt%n_Streams + 2)
              RTSolution(ln,m)%Scattering_Flag = .TRUE.
            ELSE
              n_Full_Streams = CRTM_Compute_nStreams( Atm             , &  ! Input
                                                      SensorIndex     , &  ! Input
                                                      ChannelIndex    , &  ! Input
                                                      RTSolution(ln,m)  )  ! Output
            END IF
            ! ...Transfer stream count to scattering structures
            AtmOptics(nt)%n_Legendre_Terms   = n_Full_Streams
            AtmOptics_K(nt)%n_Legendre_Terms = n_Full_Streams
            ! ...Ensure clear-sky object dimensions are consistent
            AtmOptics_Clear_K(nt)%n_Legendre_Terms = AtmOptics_K(nt)%n_Legendre_Terms

            ! Compute the gas absorption
            CALL CRTM_Compute_AtmAbsorption( SensorIndex   , &  ! Input
                                             ChannelIndex  , &  ! Input
                                             AncillaryInput, &  ! Input
                                             Predictor(nt)     , &  ! Input
                                             AtmOptics(nt)     , &  ! Output
                                             AAvar(nt)           )  ! Internal variable output


            ! Compute the molecular scattering properties
            ! ...Solar radiation
            IF( SC(SensorIndex)%Solar_Irradiance(ChannelIndex) > ZERO .AND. &
                Source_ZA < MAX_SOURCE_ZENITH_ANGLE) THEN
               RTV(nt)%Solar_Flag_true = .TRUE.
            END IF
            ! ...Visible channel with solar radiation
            IF( SpcCoeff_IsVisibleSensor( SC(SensorIndex) ) .AND. RTV(nt)%Solar_Flag_true ) THEN
              RTV(nt)%Visible_Flag_true = .TRUE.
              ! Rayleigh phase function has 0, 1, 2 components.
              ! Make sure CRTM always use a minmum of 6 streams for visible calculation.
              IF( AtmOptics(nt)%n_Legendre_Terms == 4 ) THEN
                RTSolution(ln,m)%n_Full_Streams = AtmOptics(nt)%n_Legendre_Terms + 2
              END IF
              RTV(nt)%n_Azi = MIN( AtmOptics(nt)%n_Legendre_Terms - 1, MAX_N_AZIMUTH_FOURIER )
              ! Get molecular scattering and extinction
              Wavenumber = SC(SensorIndex)%Wavenumber(ChannelIndex)
              Error_Status = CRTM_Compute_MoleculeScatter( &
                               Wavenumber, &  ! Input
                               Atm       , &  ! Input
                               AtmOptics(nt)   )  ! Input/Output
              IF ( Error_Status /= SUCCESS ) THEN
                WRITE( Message,'("Error computing MoleculeScatter for ",a,&
                       &", channel ",i0,", profile #",i0)') &
                       TRIM(ChannelInfo(n)%Sensor_ID), &
                       ChannelInfo(n)%Sensor_Channel(l), &
                       m
                CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
                CYCLE Thread_Loop
              END IF
            ELSE
              RTV(nt)%Visible_Flag_true = .FALSE.
              RTV(nt)%n_Azi = 0
              IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
                RTV_Clear(nt)%Visible_Flag_true = .FALSE.
                RTV_Clear(nt)%n_Azi = 0
              END IF
            END IF

            ! Copy the clear-sky AtmOptics
            IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
              Status_FWD = CRTM_AtmOptics_NoScatterCopy( AtmOptics(nt), AtmOptics_Clear(nt) )
              Status_K   = CRTM_AtmOptics_NoScatterCopy( AtmOptics(nt), AtmOptics_Clear_K(nt) )
              IF ( Status_FWD /= SUCCESS .OR. Status_K /= SUCCESS ) THEN
                Error_Status = FAILURE
                WRITE( Message,'("Error copying CLEAR SKY AtmOptics for ",a,&
                       &", channel ",i0,", profile #",i0)' ) &
                       TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
                CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
                CYCLE Thread_Loop
              END IF
              ! Initialise the adjoint
              CALL CRTM_AtmOptics_Zero( AtmOptics_Clear_K(nt) )
            END IF


            ! Compute the cloud particle absorption/scattering properties
            IF( Atm%n_Clouds > 0 ) THEN
              Error_Status = CRTM_Compute_CloudScatter( Atm         , &  ! Input
                                                        GeometryInfo, &  ! Input
                                                        SensorIndex , &  ! Input
                                                        ChannelIndex, &  ! Input
                                                        AtmOptics(nt)   , &  ! Output
                                                        CSvar(nt)         )  ! Internal variable output
              IF (Error_Status /= SUCCESS) THEN
                WRITE( Message,'("Error computing CloudScatter for ",a,&
                       &", channel ",i0,", profile #",i0)' ) &
                       TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
                CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
                CYCLE Thread_Loop
              END IF
            END IF


            ! Compute the aerosol absorption/scattering properties
            IF ( Atm%n_Aerosols > 0 ) THEN
              Error_Status = CRTM_Compute_AerosolScatter( Atm         , &  ! Input
                                                          SensorIndex , &  ! Input
                                                          ChannelIndex, &  ! Input
                                                          AtmOptics(nt)   , &  ! In/Output
                                                          ASvar(nt)         )  ! Internal variable output
              IF ( Error_Status /= SUCCESS ) THEN
                WRITE( Message,'("Error computing AerosolScatter for ",a,&
                       &", channel ",i0,", profile #",i0)' ) &
                       TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
                CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
                CYCLE Thread_Loop
              END IF
            END IF


            ! Compute the combined atmospheric optical properties
            IF( AtmOptics(nt)%Include_Scattering ) THEN
              CALL CRTM_AtmOptics_Combine( AtmOptics(nt), AOvar(nt) )
            END IF
            ! ...Save vertically integrated scattering optical depth for output
            RTSolution(ln,m)%SOD = AtmOptics(nt)%Scattering_Optical_Depth


            ! Compute the all-sky atmospheric transmittance
            ! for use in FASTEM-X reflection correction
            CALL CRTM_Compute_Transmittance(AtmOptics(nt),transmittance)
            SfcOptics(nt)%Transmittance = transmittance
            ! ...Clear sky for fractional cloud cover
            IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
              CALL CRTM_Compute_Transmittance(AtmOptics_Clear(nt),transmittance_clear)
              SfcOptics_Clear(nt)%Transmittance = transmittance_clear
            END IF


            ! Fill the SfcOptics structure for the optional emissivity input case.
            SfcOptics(nt)%Compute       = .TRUE.
            SfcOptics_Clear(nt)%Compute = .TRUE.
            IF ( Opt%Use_Emissivity ) THEN
              SfcOptics(nt)%Compute = .FALSE.
              SfcOptics(nt)%Emissivity(1,1)       = Opt%Emissivity(ln)
              SfcOptics(nt)%Reflectivity(1,1,1,1) = ONE - Opt%Emissivity(ln)
              IF ( Opt%Use_Direct_Reflectivity ) THEN
                SfcOptics(nt)%Direct_Reflectivity(1,1) = Opt%Direct_Reflectivity(ln)
              ELSE
                SfcOptics(nt)%Direct_Reflectivity(1,1) = SfcOptics(nt)%Reflectivity(1,1,1,1)
              END IF
              ! ...Repeat for fractional clear-sky case
              IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
                SfcOptics_Clear(nt)%Compute = .FALSE.
                SfcOptics_Clear(nt)%Emissivity(1,1)       = Opt%Emissivity(ln)
                SfcOptics_Clear(nt)%Reflectivity(1,1,1,1) = ONE - Opt%Emissivity(ln)
                IF ( Opt%Use_Direct_Reflectivity ) THEN
                  SfcOptics_Clear(nt)%Direct_Reflectivity(1,1) = Opt%Direct_Reflectivity(ln)
                ELSE
                  SfcOptics_Clear(nt)%Direct_Reflectivity(1,1) = SfcOptics(nt)%Reflectivity(1,1,1,1)
                END IF
              END IF
            END IF


            ! Fourier component loop for azimuth angles (VIS).
            ! mth_Azi = 0 is for an azimuth-averaged value (IR, MW)
            ! ...Initialise radiance
            RTSolution(ln,m)%Radiance = ZERO

            ! ###################################################
            ! TEMPORARY FIX : SENSOR-DEPENDENT AZIMUTH ANGLE LOOP
            ! ###################################################
            Sensor_Dependent_RTSolution: &
            IF ( SpcCoeff_IsInfraredSensor( SC(SensorIndex) ) .OR. &
                 SpcCoeff_IsMicrowaveSensor( SC(SensorIndex) ) ) THEN
              ! ------------------------------
              ! INFRARED and MICROWAVE sensors
              ! ------------------------------

              ! Set dependent component counters
              RTV(nt)%mth_Azi = 0
              SfcOptics(nt)%mth_Azi = 0

              ! Solve the forward radiative transfer problem
              Error_Status = CRTM_Compute_RTSolution( &
                               Atm             , &  ! Input
                               Surface(m)      , &  ! Input
                               AtmOptics(nt)       , &  ! Input
                               SfcOptics(nt)       , &  ! Input
                               GeometryInfo    , &  ! Input
                               SensorIndex     , &  ! Input
                               ChannelIndex    , &  ! Input
                               RTSolution(ln,m), &  ! Output
                               RTV(nt)               )  ! Internal variable output
              IF ( Error_Status /= SUCCESS ) THEN
                WRITE( Message,'( "Error computing RTSolution for ", a, &
                       &", channel ", i0,", profile #",i0)' ) &
                       TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
                CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
                CYCLE Thread_Loop
              END IF


              ! Perform clear sky calcs for fractionally cloudy atmospheres
              IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN

                ! Repeat radiative transfer for clear-sky
                RTV_Clear(nt)%mth_Azi = RTV(nt)%mth_Azi
                SfcOptics_Clear(nt)%mth_Azi = SfcOptics(nt)%mth_Azi
                Error_Status = CRTM_Compute_RTSolution( &
                                 Atm_Clear       , &  ! Input
                                 Surface(m)      , &  ! Input
                                 AtmOptics_Clear(nt) , &  ! Input
                                 SfcOptics_Clear(nt) , &  ! Input
                                 GeometryInfo    , &  ! Input
                                 SensorIndex     , &  ! Input
                                 ChannelIndex    , &  ! Input
                                 RTSolution_Clear(nt), &  ! Output
                                 RTV_Clear(nt)         )  ! Internal variable output
                IF ( Error_Status /= SUCCESS ) THEN
                  WRITE( Message,'( "Error computing CLEAR SKY RTSolution for ", a, &
                         &", channel ", i0,", profile #",i0)' ) &
                         TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
                  CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
                  CYCLE Thread_Loop
                END IF

                ! Combine cloudy and clear radiances for fractional cloud coverage
                r_cloudy = RTSolution(ln,m)%Radiance  ! Save the 100% cloudy radiance
                RTSolution(ln,m)%Radiance = &
                    ((ONE - CloudCover%Total_Cloud_Cover) * RTSolution_Clear(nt)%Radiance) + &
                    (CloudCover%Total_Cloud_Cover * RTSolution(ln,m)%Radiance)
                ! ...Save the cloud cover in the output structure
                RTSolution(ln,m)%Total_Cloud_Cover = CloudCover%Total_Cloud_Cover
              END IF
              ! The radiance post-processing
              CALL Post_Process_RTSolution(Opt, RTSolution(ln,m), &
                                           NLTE_Predictor, &
                                           ChannelIndex, SensorIndex, &
                                           compute_antenna_correction, GeometryInfo)
              
              !======= Active sensor =======
              ! Calculate reflectivity for active instruments
              IF  ( SC(SensorIndex)%Is_Active_Sensor .AND. AtmOptics(nt)%Include_Scattering) THEN
                  CALL CRTM_Compute_Reflectivity(Atm             , & ! Input
                                                 AtmOptics(nt)   , & ! Input
                                                 GeometryInfo    , & ! Input
                                                 SensorIndex     , & ! Input
                                                 ChannelIndex    , & ! Input
                                                 RTSolution(ln,m))   ! Input/Output
              ENDIF
              !=============================

              ! Perform clear-sky post and pre-processing
              IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
                ! Radiance post-processing
                CALL Post_Process_RTSolution(Opt, RTSolution_Clear(nt), &
                                             NLTE_Predictor, &
                                             ChannelIndex, SensorIndex, &
                                             compute_antenna_correction, GeometryInfo)
                RTSolution(ln,m)%R_Clear  = RTSolution_Clear(nt)%Radiance
                RTSolution(ln,m)%Tb_Clear = RTSolution_Clear(nt)%Brightness_Temperature

                ! Adjoint radiance pre-processing
                RTSolution_Clear_K(nt)%Brightness_Temperature = RTSolution_Clear_K(nt)%Brightness_Temperature + &
                                                            RTSolution_K(ln,m)%Tb_Clear
                RTSolution_K(ln,m)%Tb_Clear               = ZERO
                RTSolution_Clear_K(nt)%Radiance = RTSolution_Clear_K(nt)%Radiance + &
                                              RTSolution_K(ln,m)%R_Clear
                RTSolution_K(ln,m)%R_Clear  = ZERO
                CALL Pre_Process_RTSolution_K(Opt, RTSolution_Clear(nt), RTSolution_Clear_K(nt), &
                                              NLTE_Predictor, NLTE_Predictor_K(nt), &
                                              ChannelIndex, SensorIndex, &
                                              compute_antenna_correction, GeometryInfo)
              END IF

              ! The adjoint radiance pre-processing
              CALL Pre_Process_RTSolution_K(Opt, RTSolution(ln,m), RTSolution_K(ln,m), &
                                            NLTE_Predictor, NLTE_Predictor_K(nt), &
                                            ChannelIndex, SensorIndex, &
                                            compute_antenna_correction, GeometryInfo)

              ! More fractionally cloudy atmospheres processing
              IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
                ! The adjoint of the clear and cloudy radiance combination
                CloudCover_K(nt)%Total_Cloud_Cover = CloudCover_K(nt)%Total_Cloud_Cover + &
                                                 RTSolution_K(ln,m)%Total_Cloud_Cover
                RTSolution_K(ln,m)%Total_Cloud_Cover = ZERO
                RTSolution_Clear_K(nt)%Radiance    = RTSolution_Clear_K(nt)%Radiance + &
                                                 ((ONE - CloudCover%Total_Cloud_Cover) * RTSolution_K(ln,m)%Radiance)
                CloudCover_K(nt)%Total_Cloud_Cover = CloudCover_K(nt)%Total_Cloud_Cover + &
                                                 ((r_cloudy - RTSolution_Clear(nt)%Radiance) * RTSolution_K(ln,m)%Radiance)
                RTSolution_K(ln,m)%Radiance    = CloudCover%Total_Cloud_Cover * RTSolution_K(ln,m)%Radiance
                ! The adjoint of the clear sky radiative transfer for fractionally cloudy atmospheres
                Error_Status = CRTM_Compute_RTSolution_AD( &
                                 Atm_Clear         , &  ! FWD Input
                                 Surface(m)        , &  ! FWD Input
                                 AtmOptics_Clear(nt)   , &  ! FWD Input
                                 SfcOptics_Clear(nt)   , &  ! FWD Input
                                 RTSolution_Clear(nt)  , &  ! FWD Input
                                 RTSolution_Clear_K(nt), &  ! K   Input
                                 GeometryInfo      , &  ! Input
                                 SensorIndex       , &  ! Input
                                 ChannelIndex      , &  ! Input
                                 Atm_Clear_K(nt)       , &  ! K  Output
                                 Surface_K(ln,m)   , &  ! K  Output
                                 AtmOptics_Clear_K(nt) , &  ! K  Output
                                 SfcOptics_Clear_K(nt) , &  ! K  Output
                                 RTV_Clear(nt)            )  ! Internal variable input
                IF ( Error_Status /= SUCCESS ) THEN
                  WRITE( Message,'( "Error computing CLEAR SKY RTSolution_K for ", a, &
                         &", channel ", i0,", profile #",i0)' ) &
                         TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
                  CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
                  CYCLE Thread_Loop
                END IF
              END IF

              ! The adjoint of the radiative transfer
              Error_Status = CRTM_Compute_RTSolution_AD( &
                               Atm               , &  ! FWD Input
                               Surface(m)        , &  ! FWD Input
                               AtmOptics(nt)         , &  ! FWD Input
                               SfcOptics(nt)         , &  ! FWD Input
                               RTSolution(ln,m)  , &  ! FWD Input
                               RTSolution_K(ln,m), &  ! K   Input
                               GeometryInfo      , &  ! Input
                               SensorIndex       , &  ! Input
                               ChannelIndex      , &  ! Input
                               Atm_K(nt)             , &  ! K  Output
                               Surface_K(ln,m)   , &  ! K  Output
                               AtmOptics_K(nt)       , &  ! K  Output
                               SfcOptics_K(nt)       , &  ! K  Output
                               RTV(nt)                 )  ! Internal variable input
              IF ( Error_Status /= SUCCESS ) THEN
                WRITE( Message,'( "Error computing RTSolution_K for ", a, &
                       &", channel ", i0,", profile #",i0)' ) &
                       TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
                CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
                CYCLE Thread_Loop
              END IF

              ! Calculate the adjoint for the active sensor reflectivity
              IF  ( SC(SensorIndex)%Is_Active_Sensor .AND. AtmOptics(nt)%Include_Scattering) THEN
                  CALL CRTM_Compute_Reflectivity_AD(Atm             , &  ! Input 
                                                    AtmOptics(nt)   , &  ! Input 
                                                    RTSolution(ln,m), & ! Input
                                                    GeometryInfo    , & ! Input
                                                    SensorIndex     , &  ! Input
                                                    ChannelIndex    , &  ! Input
                                                    AtmOptics_K(nt) , &  ! Input/Output
                                                    RTSolution_K(ln,m))   ! Input/Output
              ENDIF
              
            ELSE Sensor_Dependent_RTSolution
              ! --------------
              ! VISIBLE sensor
              ! --------------
              ! ...Fourier expansion over azimuth angle
              Azimuth_Fourier_Loop: DO mth_Azi = 0, RTV(nt)%n_Azi

                ! Set dependent component counters
                RTV(nt)%mth_Azi = mth_Azi
                SfcOptics(nt)%mth_Azi = mth_Azi

                ! Solve the forward radiative transfer problem
                Error_Status = CRTM_Compute_RTSolution( &
                                 Atm             , &  ! Input
                                 Surface(m)      , &  ! Input
                                 AtmOptics(nt)       , &  ! Input
                                 SfcOptics(nt)       , &  ! Input
                                 GeometryInfo    , &  ! Input
                                 SensorIndex     , &  ! Input
                                 ChannelIndex    , &  ! Input
                                 RTSolution(ln,m), &  ! Output
                                 RTV(nt)               )  ! Internal variable output
                IF ( Error_Status /= SUCCESS ) THEN
                  WRITE( Message,'( "Error computing RTSolution for ", a, &
                         &", channel ", i0,", profile #",i0)' ) &
                         TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
                  CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
                  CYCLE Thread_Loop
                END IF


                ! Repeat clear sky for fractionally cloudy atmospheres
                IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
                  RTV_Clear(nt)%mth_Azi = RTV(nt)%mth_Azi
                  SfcOptics_Clear(nt)%mth_Azi = SfcOptics(nt)%mth_Azi
                  Error_Status = CRTM_Compute_RTSolution( &
                                   Atm_Clear       , &  ! Input
                                   Surface(m)      , &  ! Input
                                   AtmOptics_Clear(nt) , &  ! Input
                                   SfcOptics_Clear(nt) , &  ! Input
                                   GeometryInfo    , &  ! Input
                                   SensorIndex     , &  ! Input
                                   ChannelIndex    , &  ! Input
                                   RTSolution_Clear(nt), &  ! Output
                                   RTV_Clear(nt)         )  ! Internal variable output
                  IF ( Error_Status /= SUCCESS ) THEN
                    WRITE( Message,'( "Error computing CLEAR SKY RTSolution for ", a, &
                           &", channel ", i0,", profile #",i0)' ) &
                           TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
                    CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
                    CYCLE Thread_Loop
                  END IF
                END IF
              END DO Azimuth_Fourier_Loop

              ! All of the "in-between" FWD and AD processing is for fractional cloud coverage only
              IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN

                ! FORWARD #1: Combine cloudy and clear radiances for fractional cloud coverage
                r_cloudy = RTSolution(ln,m)%Radiance  ! Save the 100% cloudy radiance
                RTSolution(ln,m)%Radiance = &
                    ((ONE - CloudCover%Total_Cloud_Cover) * RTSolution_Clear(nt)%Radiance) + &
                    (CloudCover%Total_Cloud_Cover * RTSolution(ln,m)%Radiance)
                ! FORWARD #2: Save the cloud cover and clear radiance in the output structure
                RTSolution(ln,m)%Total_Cloud_Cover = CloudCover%Total_Cloud_Cover
                RTSolution(ln,m)%R_Clear           = RTSolution_Clear(nt)%Radiance
                RTSolution(ln,m)%Tb_Clear          = ZERO      ! No Tb for visible

                ! ADJOINT #2: Of the cloud cover and clear radiance saving
                RTSolution_Clear_K(nt)%Tb_Clear = ZERO   ! No Tb for visible
                RTSolution_Clear_K(nt)%Radiance = RTSolution_Clear_K(nt)%Radiance + &
                                              RTSolution_K(ln,m)%R_Clear
                RTSolution_K(ln,m)%R_Clear  = ZERO
                CloudCover_K(nt)%Total_Cloud_Cover = CloudCover_K(nt)%Total_Cloud_Cover + &
                                                 RTSolution_K(ln,m)%Total_Cloud_Cover
                RTSolution_K(ln,m)%Total_Cloud_Cover = ZERO

                ! ADJOINT #1: Of the clear+cloudy combination
                RTSolution_Clear_K(nt)%Radiance    = RTSolution_Clear_K(nt)%Radiance + &
                                                 ((ONE - CloudCover%Total_Cloud_Cover) * RTSolution_K(ln,m)%Radiance)
                CloudCover_K(nt)%Total_Cloud_Cover = CloudCover_K(nt)%Total_Cloud_Cover + &
                                                 ((r_cloudy - RTSolution_Clear(nt)%Radiance) * RTSolution_K(ln,m)%Radiance)
                RTSolution_K(ln,m)%Radiance    = CloudCover%Total_Cloud_Cover * RTSolution_K(ln,m)%Radiance
              END IF


              ! Adjoint Fourier expansion over azimuth angle
              Azimuth_Fourier_Loop_K: DO mth_Azi = 0, RTV(nt)%n_Azi

                ! Set dependent component counters
                RTV(nt)%mth_Azi = mth_Azi
                SfcOptics(nt)%mth_Azi = mth_Azi


                ! The adjoint of the clear sky radiative transfer for fractionally cloudy atmospheres
                IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
                  RTV_Clear(nt)%mth_Azi = RTV(nt)%mth_Azi
                  SfcOptics_Clear(nt)%mth_Azi = SfcOptics(nt)%mth_Azi
                  Error_Status = CRTM_Compute_RTSolution_AD( &
                                   Atm_Clear         , &  ! FWD Input
                                   Surface(m)        , &  ! FWD Input
                                   AtmOptics_Clear(nt)   , &  ! FWD Input
                                   SfcOptics_Clear(nt)   , &  ! FWD Input
                                   RTSolution_Clear(nt)  , &  ! FWD Input
                                   RTSolution_Clear_K(nt), &  ! AD  Input
                                   GeometryInfo      , &  ! Input
                                   SensorIndex       , &  ! Input
                                   ChannelIndex      , &  ! Input
                                   Atm_Clear_K(nt)       , &  ! AD Output
                                   Surface_K(ln,m)   , &  ! AD Output
                                   AtmOptics_Clear_K(nt) , &  ! AD Output
                                   SfcOptics_Clear_K(nt) , &  ! AD Output
                                   RTV_Clear(nt)           )  ! Internal variable input
                  IF ( Error_Status /= SUCCESS ) THEN
                    WRITE( Message,'( "Error computing CLEAR SKY RTSolution_AD for ", a, &
                           &", channel ", i0,", profile #",i0)' ) &
                           TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
                    CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
                    CYCLE Thread_Loop
                  END IF
                END IF


                ! The adjoint of the radiative transfer
                Error_Status = CRTM_Compute_RTSolution_AD( &
                                 Atm               , &  ! FWD Input
                                 Surface(m)        , &  ! FWD Input
                                 AtmOptics(nt)         , &  ! FWD Input
                                 SfcOptics(nt)         , &  ! FWD Input
                                 RTSolution(ln,m)  , &  ! FWD Input
                                 RTSolution_K(ln,m), &  ! K   Input
                                 GeometryInfo      , &  ! Input
                                 SensorIndex       , &  ! Input
                                 ChannelIndex      , &  ! Input
                                 Atm_K(nt)             , &  ! K  Output
                                 Surface_K(ln,m)   , &  ! K  Output
                                 AtmOptics_K(nt)       , &  ! K  Output
                                 SfcOptics_K(nt)       , &  ! K  Output
                                 RTV(nt)                 )  ! Internal variable input
                IF ( Error_Status /= SUCCESS ) THEN
                  WRITE( Message,'( "Error computing RTSolution_K for ", a, &
                         &", channel ", i0,", profile #",i0)' ) &
                         TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
                  CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
                  CYCLE Thread_Loop
                END IF
              END DO Azimuth_Fourier_Loop_K

            END IF Sensor_Dependent_RTSolution
            ! ###################################################
            ! TEMPORARY FIX : SENSOR-DEPENDENT AZIMUTH ANGLE LOOP
            ! ###################################################

            ! Compute the adjoint of the all-sky atmospheric transmittance
            ! for use in FASTEM-X reflection correction
            transmittance_K = SfcOptics_K(nt)%transmittance
            SfcOptics_K(nt)%transmittance = ZERO
            CALL CRTM_Compute_Transmittance_AD(AtmOptics(nt),transmittance_K,AtmOptics_K(nt))
            ! ...Clear sky for fractional cloud cover
            IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
              transmittance_clear_K = SfcOptics_Clear_K(nt)%transmittance
              SfcOptics_Clear_K(nt)%transmittance = ZERO
              CALL CRTM_Compute_Transmittance_AD(AtmOptics_Clear(nt),transmittance_clear_K,AtmOptics_Clear_K(nt))
            END IF


            ! Compute the adjoint of the combined atmospheric optical properties
            AtmOptics_K(nt)%Scattering_Optical_Depth = AtmOptics_K(nt)%Scattering_Optical_Depth + &
                                                   RTSolution_K(ln,m)%SOD
            RTSolution_K(ln,m)%SOD               = ZERO
            IF( AtmOptics(nt)%Include_Scattering ) THEN
              CALL CRTM_AtmOptics_Combine_AD( AtmOptics(nt), AtmOptics_K(nt), AOvar(nt) )
            END IF


            ! Compute the adjoint aerosol absorption/scattering properties
            IF ( Atm%n_Aerosols > 0 ) THEN
              Error_Status = CRTM_Compute_AerosolScatter_AD( Atm         , &  ! FWD Input
                                                             AtmOptics(nt)   , &  ! FWD Input
                                                             AtmOptics_K(nt) , &  ! K   Input
                                                             SensorIndex , &  ! Input
                                                             ChannelIndex, &  ! Input
                                                             Atm_K(nt)       , &  ! K   Output
                                                             ASvar(nt)         )  ! Internal variable input
              IF ( Error_Status /= SUCCESS ) THEN
                WRITE( Message,'("Error computing AerosolScatter_K for ",a,&
                       &", channel ",i0,", profile #",i0)' ) &
                       TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
                CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
                CYCLE Thread_Loop
              END IF
            END IF


            ! Compute the adjoint cloud absorption/scattering properties
            IF ( Atm%n_Clouds > 0 ) THEN
              Error_Status = CRTM_Compute_CloudScatter_AD( Atm         , &  ! FWD Input
                                                           AtmOptics(nt)   , &  ! FWD Input
                                                           AtmOptics_K(nt) , &  ! K   Input
                                                           GeometryInfo    , &  ! Input
                                                           SensorIndex , &  ! Input
                                                           ChannelIndex, &  ! Input
                                                           Atm_K(nt)       , &  ! K   Output
                                                           CSvar(nt)         )  ! Internal variable input
              IF ( Error_Status /= SUCCESS ) THEN
                WRITE( Message,'("Error computing CloudScatter_K for ",a,&
                       &", channel ",i0,", profile #",i0)' ) &
                       TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
                CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
                CYCLE Thread_Loop
              END IF
            END IF


            ! Adjoint of clear-sky AtmOptics copy
            IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
              Error_Status = CRTM_AtmOptics_NoScatterCopy_AD( AtmOptics(nt), AtmOptics_Clear_K(nt), AtmOptics_K(nt) )
              IF ( Error_Status /= SUCCESS ) THEN
                WRITE( Message,'("Error computing CLEAR SKY AtmOptics_K for ",a,&
                       &", channel ",i0,", profile #",i0)' ) &
                       TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
                CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
                CYCLE Thread_Loop
              END IF
            END IF


            ! Compute the adjoint molecular scattering properties
            IF( RTV(nt)%Visible_Flag_true ) THEN
              Wavenumber = SC(SensorIndex)%Wavenumber(ChannelIndex)
              Error_Status = CRTM_Compute_MoleculeScatter_AD( &
                               Wavenumber , &
                               AtmOptics_K(nt), &
                               Atm_K(nt)        )
              IF ( Error_Status /= SUCCESS ) THEN
                WRITE( Message,'("Error computing MoleculeScatter_K for ",a,&
                       &", channel ",i0,", profile #",i0)' ) &
                       TRIM(ChannelInfo(n)%Sensor_ID), &
                       ChannelInfo(n)%Sensor_Channel(l), &
                       m
                CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
                CYCLE Thread_Loop
              END IF
            END IF

            ! Compute the adjoint gaseous absorption
            CALL CRTM_Compute_AtmAbsorption_AD( SensorIndex , &  ! Input
                                                ChannelIndex, &  ! Input
                                                Predictor(nt)   , &  ! FWD Input
                                                AtmOptics_K(nt) , &  ! K   Input
                                                Predictor_K(nt) , &  ! K   Output
                                                AAvar(nt)         )  ! Internal variable input


            ! K-matrix of the NLTE correction predictor calculations
            IF ( Opt%Apply_NLTE_Correction ) THEN
              CALL Compute_NLTE_Predictor_AD( &
                     NLTE_Predictor   , &  ! Input
                     NLTE_Predictor_K(nt) , &  ! Input
                     Atm_K(nt)              )  ! Output
            END IF


            ! K-matrix of the predictor calculations
            CALL CRTM_Compute_Predictors_AD( SensorIndex   , &  ! Input
                                             Atm           , &  ! FWD Input
                                             Predictor(nt)     , &  ! FWD Input
                                             Predictor_K(nt)   , &  ! K   Input
                                             AncillaryInput, &  ! Input
                                             Atm_K(nt)         , &  ! K   Output
                                             PVar(nt)            )  ! Internal variable input


            ! K-matrix of average surface skin temperature for multi-surface types
            CALL CRTM_Compute_SurfaceT_AD( Surface(m), SfcOptics_K(nt), Surface_K(ln,m) )

            ! Adjoint of cloud cover setup
            IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN

              ! Post process the CLEAR sky structures for fractional cloud coverage
              ! ...Clear sky SfcOptics
              CALL CRTM_Compute_SurfaceT_AD( Surface(m), SfcOptics_Clear_K(nt), Surface_K(ln,m) )
              CALL CRTM_SfcOptics_Zero(SfcOptics_Clear_K(nt))
              ! ...Clear sky atmosphere
              Error_Status = CRTM_Atmosphere_ClearSkyCopy_AD(Atm, Atm_Clear_K(nt), Atm_K(nt))
              IF ( Error_Status /= SUCCESS ) THEN
                Error_status = FAILURE
                WRITE( Message,'("Error computing CLEAR SKY Atm_K object for ",a,&
                       &", channel ",i0,", profile #",i0)' ) &
                       TRIM(ChannelInfo(n)%Sensor_ID), &
                       ChannelInfo(n)%Sensor_Channel(l), &
                       m
                CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
                CYCLE Thread_Loop
              END IF

              ! K-matrix of the cloud coverage
              Error_Status = CloudCover_K(nt)%Compute_CloudCover_AD(CloudCover, atm, atm_K(nt))
              IF ( Error_Status /= SUCCESS ) THEN
                Error_Status = FAILURE
                WRITE( Message,'("Error computing K-MATRIX cloud cover for ",a,&
                       &", channel ",i0,", profile #",i0)' ) &
                       TRIM(ChannelInfo(n)%Sensor_ID), &
                       ChannelInfo(n)%Sensor_Channel(l), &
                       m
                CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
                CYCLE Thread_Loop
              END IF
            END IF


            ! K-matrix of the atmosphere layer addition
            Error_Status = CRTM_Atmosphere_AddLayers_AD( Atmosphere(m), Atm_K(nt), Atmosphere_K(ln,m) )
            IF ( Error_Status /= SUCCESS ) THEN
              Error_Status = FAILURE
              WRITE( Message,'("Error computing K-MATRIX atmosphere extra layers for ",a,&
                     &", channel ",i0,", profile #",i0)' ) &
                     TRIM(ChannelInfo(n)%Sensor_ID), &
                     ChannelInfo(n)%Sensor_Channel(l), &
                     m
              CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
              CYCLE Thread_Loop
            END IF

          END DO Channel_Loop
        END DO Thread_Loop
!$OMP END PARALLEL DO
        IF ( Error_Status == FAILURE ) RETURN
        ln = ln + n_sensor_channels - n_inactive_channels(n_channel_threads)

      END DO Sensor_Loop
      ! Deallocate local sensor independent data structures
      ! ...Atmospheric optics
      ! Clean up
      CALL CRTM_Atmosphere_Destroy( Atm )
      CALL CRTM_Atmosphere_Destroy( Atm_Clear )
!$OMP PARALLEL DO NUM_THREADS(n_channel_threads)
      DO nt = 1, n_channel_threads
        CALL CRTM_Predictor_Destroy( Predictor(nt) )
        CALL CRTM_Predictor_Destroy( Predictor_K(nt) )
        CALL CRTM_AtmOptics_Destroy( AtmOptics(nt) )
        CALL CRTM_AtmOptics_Destroy( AtmOptics_K(nt) )
        CALL CRTM_AtmOptics_Destroy( AtmOptics_Clear(nt) )
        CALL CRTM_AtmOptics_Destroy( AtmOptics_Clear_K(nt) )
        CALL CRTM_SfcOptics_Destroy( SfcOptics(nt) )
        CALL CRTM_SfcOptics_Destroy( SfcOptics_K(nt) )
        CALL CRTM_SfcOptics_Destroy( SfcOptics_Clear(nt) )
        CALL CRTM_SfcOptics_Destroy( SfcOptics_Clear_K(nt) )
        CALL CRTM_Atmosphere_Destroy( Atm_K(nt) )
        CALL CRTM_Atmosphere_Destroy( Atm_Clear_K(nt) )
        ! ...Internal variables
        CALL AOvar_Destroy( AOvar(nt) )
        CALL CSvar_Destroy( CSvar(nt) )
        CALL ASvar_Destroy( ASvar(nt) )
        CALL RTV_Destroy( RTV(nt) )
      END DO
!$OMP END PARALLEL DO

    END FUNCTION profile_solution

    ! ----------------------------------------------------------------
    ! Local subroutine to post-process the FORWARD radiance, as it is
    ! the same for all-sky and fractional clear-sky cases.
    !
    !   1. Apply non-LTE correction to radiance
    !   2. Convert radiance to brightness temperature
    !   3. Apply antenna correction to brightness temperature
    ! ----------------------------------------------------------------

    SUBROUTINE Post_Process_RTSolution(Opt, rts, &
                                       NLTE_Predictor, &
                                       ChannelIndex, SensorIndex, &
                                       compute_antenna_correction, GeometryInfo)
      TYPE(CRTM_Options_type),      INTENT(IN) :: Opt
      TYPE(CRTM_RTSolution_type),   INTENT(IN OUT) :: rts
      TYPE(NLTE_Predictor_type),    INTENT(IN)     :: NLTE_Predictor
      INTEGER,                      INTENT(IN) :: ChannelIndex, SensorIndex
      LOGICAL,                      INTENT(IN) :: compute_antenna_correction
      TYPE(CRTM_GeometryInfo_type), INTENT(IN) :: GeometryInfo

      ! Compute non-LTE correction to radiance if required
      IF ( Opt%Apply_NLTE_Correction .AND. NLTE_Predictor_IsActive(NLTE_Predictor) ) THEN
        CALL Compute_NLTE_Correction( &
             SC(SensorIndex)%NC, &  ! Input
             ChannelIndex      , &  ! Input
             NLTE_Predictor    , &  ! Input
             rts%Radiance        )  ! In/Output
      END IF
      ! Convert the radiance to brightness temperature
      CALL CRTM_Planck_Temperature( &
           SensorIndex               , & ! Input
           ChannelIndex              , & ! Input
           rts%Radiance              , & ! Input
           rts%Brightness_Temperature  ) ! Output
      ! Compute Antenna correction to brightness temperature if required

      IF ( compute_antenna_correction ) THEN
        CALL CRTM_Compute_AntCorr( &
             GeometryInfo, &  ! Input
             SensorIndex , &  ! Input
             ChannelIndex, &  ! Input
             rts           )  ! Output
      END IF

    END SUBROUTINE Post_Process_RTSolution


    ! ----------------------------------------------------------------
    ! Local subroutine to pre-process the K-MATRIX radiance, as it is
    ! the same for all-sky and fractional clear-sky cases.
    !
    !   1. Apply adjoint antenna correction to brightness temperatures
    !   2. Convert adjoint radiances to brightness temperatures
    !   3. Apply adjoint non-LTE correction to radiances
    ! ----------------------------------------------------------------

    SUBROUTINE Pre_Process_RTSolution_K(Opt, rts, rts_K, &
                                        NLTE_Predictor, NLTE_Predictor_K, &
                                        ChannelIndex, SensorIndex, &
                                        compute_antenna_correction, GeometryInfo)
      TYPE(CRTM_Options_type),      INTENT(IN) :: Opt
      TYPE(CRTM_RTSolution_type),   INTENT(IN OUT) :: rts, rts_K
      TYPE(NLTE_Predictor_type),    INTENT(IN)     :: NLTE_Predictor
      TYPE(NLTE_Predictor_type),    INTENT(IN OUT) :: NLTE_Predictor_K
      INTEGER,                      INTENT(IN) :: ChannelIndex, SensorIndex
      LOGICAL,                      INTENT(IN) :: compute_antenna_correction
      TYPE(CRTM_GeometryInfo_type), INTENT(IN) :: GeometryInfo

      ! Compute adjoint antenna correction to brightness temperature if required
      IF ( compute_antenna_correction ) THEN
        CALL CRTM_Compute_AntCorr_AD( &
             GeometryInfo, &  ! Input
             SensorIndex , &  ! Input
             ChannelIndex, &  ! Input
             rts_K         )  ! Output
      END IF
      ! Compute the Planck temperature adjoint
      CALL CRTM_Planck_Temperature_AD( &
           SensorIndex                 , & ! Input
           ChannelIndex                , & ! Input
           rts%Radiance                , & ! Input
           rts_K%Brightness_Temperature, & ! Input
           rts_K%Radiance                ) ! Output
      rts_K%Brightness_Temperature = ZERO
      ! Compute non-LTE correction adjoint if required
      IF ( Opt%Apply_NLTE_Correction .AND. NLTE_Predictor_IsActive(NLTE_Predictor) ) THEN
        CALL Compute_NLTE_Correction_AD( &
             SC(SensorIndex)%NC, &  ! Input
             ChannelIndex      , &  ! Input
             rts_K%Radiance    , &  ! Input
             NLTE_Predictor_K    )  ! Output
      END IF
    END SUBROUTINE Pre_Process_RTSolution_K
  END FUNCTION CRTM_K_Matrix
END MODULE CRTM_K_Matrix_Module
