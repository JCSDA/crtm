!
! RH_MR
!
! Module containing forward, tangent-linear and adjoint subroutines
! for relative humidity to/from mixing ratio units conversion.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 17-Aug-2009
!                       paul.vandelst@noaa.gov
!

MODULE RH_MR

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Modules used
  USE Type_Kinds, ONLY: fp
  USE Profile_Utility_Parameters, ONLY: ZERO, ONE, HUNDRED, &
                                        MAX_N_MOLECULAR_SPECIES, &
                                        MW_DRYAIR, MOLECULAR_WEIGHT, &
                                        FROM_PERCENT, TO_PERCENT
  USE Atmospheric_Properties, ONLY: Saturation_Mixing_Ratio, &
                                    Saturation_Mixing_Ratio_TL, &
                                    Saturation_Mixing_Ratio_AD

  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: RH_to_MR, RH_to_MR_TL, RH_to_MR_AD
  PUBLIC :: MR_to_RH, MR_to_RH_TL, MR_to_RH_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &


CONTAINS


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       RH_to_MR
!
! PURPOSE:
!       Elemental subroutine to convert relative humidity to water vapor
!       mass mixing ratio
!
! CALLING SEQUENCE:
!       CALL RH_to_MR( Pressure                         , &  ! Input
!                      Temperature                      , &  ! Input
!                      Relative_Humidity                , &  ! Input
!                      Mixing_Ration                    , &  ! Output
!                      Ice_Temperature = Ice_Temperature, &  ! Optional input
!                      Min_Pressure    = Min_Pressure     )  ! Optional input
!
! INPUTS:
!       Pressure:              Total atmospheric pressure.
!                              Must be > 0.
!                              UNITS:      hectoPascals, hPa
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Scalar or any rank
!                              ATTRIBUTES: INTENT(IN)
!
!       Temperature:           Atmospheric temperature.
!                              Must be > 0.
!                              UNITS:      Kelvin, K
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Same as Pressure
!                              ATTRIBUTES: INTENT(IN)
!
!       Relative_Humidity:     Atmospheric relative humidity.
!                              Must be >=0 and <=100.
!                              UNITS:      %
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Same as Pressure
!                              ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Mixing_Ratio:          Water vapor mass mixing ratio.
!                              Set to zero for invalid input.
!                              UNITS:      g/kg
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Same as input Pressure
!                              ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Ice_Temperature:       Temperature below which the saturation vapor
!                              pressure over ice is used in the conversion.
!                              By default, only the saturation vapor pressure
!                              over water is used.
!                              UNITS:      Kelvin, K
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Min_Pressure:          Pressure value below which the saturation
!                              mixing ratio is not calculated. The default
!                              is 50mb. Saturation mixing ratios below the
!                              minimum pressure are set to zero. This is
!                              because at pressures less than 50mb, the
!                              saturation vapour Pressure, which is based
!                              only on temperature, can exceed the total
!                              air Pressure.
!                              UNITS:      hectoPascals, hPa
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! PROCEDURE:
!       Once the saturation mixing ratio is calculated the mixing ratio
!       corresponding to the input relative humidity is determined using:
!
!                       Relative_Humidity * Saturation_Mixing_Ratio
!       Mixing_Ratio = ---------------------------------------------
!                                        100
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 02-Mar-1999
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE RH_to_MR( &
    P              , &  ! Input
    T              , &  ! Input
    rh             , &  ! Input
    mr             , &  ! Output
    Ice_Temperature, &  ! Optional Input
    Min_Pressure     )  ! Optional Input
    ! Arguments
    REAL(fp),           INTENT(IN)  :: P
    REAL(fp),           INTENT(IN)  :: T
    REAL(fp),           INTENT(IN)  :: rh
    REAL(fp),           INTENT(OUT) :: mr
    REAL(fp), OPTIONAL, INTENT(IN)  :: Ice_Temperature
    REAL(fp), OPTIONAL, INTENT(IN)  :: Min_Pressure
    ! Local variables
    REAL(fp) :: smr

    ! Setup
    IF ( P < ZERO .OR. T < ZERO .OR. rh < ZERO .OR. rh > HUNDRED ) THEN
      mr = ZERO
      RETURN
    ENDIF

    ! Calculate saturation mixing ratio in g/kg
    CALL Saturation_Mixing_Ratio( P, &
                                  T, &
                                  smr, &
                                  Ice_Temperature=Ice_Temperature, &
                                  Min_Pressure   =Min_Pressure )

    ! Calculate mixing ratio in g/kg
    IF ( smr > ZERO ) THEN
      mr = FROM_PERCENT * rh * smr
    ELSE
      mr = ZERO
    END IF

  END SUBROUTINE RH_to_MR


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       RH_to_MR_TL
!
! PURPOSE:
!       Tangent-linear for of elemental subroutine to convert relative
!       humidity to water vapor mass mixing ratio
!
! CALLING SEQUENCE:
!       CALL RH_to_MR_TL( Pressure                         , &  ! FWD Input
!                         Temperature                      , &  ! FWD Input
!                         Relative_Humidity                , &  ! FWD Input
!                         Pressure_TL                      , &  ! TL  Input
!                         Temperature_TL                   , &  ! TL  Input
!                         Relative_Humidity_TL             , &  ! TL  Input
!                         Mixing_Ration_TL                 , &  ! TL  Output
!                         Ice_Temperature = Ice_Temperature, &  ! Optional input
!                         Min_Pressure    = Min_Pressure     )  ! Optional input
!
! INPUTS:
!       Pressure:              Total atmospheric pressure.
!                              Must be > 0.
!                              UNITS:      hectoPascals, hPa
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Scalar or any rank
!                              ATTRIBUTES: INTENT(IN)
!
!       Temperature:           Atmospheric temperature.
!                              Must be > 0.
!                              UNITS:      Kelvin, K
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Same as Pressure
!                              ATTRIBUTES: INTENT(IN)
!
!       Relative_Humidity:     Atmospheric relative humidity.
!                              Must be >=0 and <=100.
!                              UNITS:      %
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Same as Pressure
!                              ATTRIBUTES: INTENT(IN)
!
!       Pressure_TL:           Tangent-linear atmospheric pressure.
!                              UNITS:      hectoPascals, hPa
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Same as Pressure
!                              ATTRIBUTES: INTENT(IN)
!
!       Temperature_TL:        Tangent-linear atmospheric temperature.
!                              UNITS:      Kelvin, K
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Same as Pressure
!                              ATTRIBUTES: INTENT(IN)
!
!       Relative_Humidity_TL:  Tangent-linear atmospheric relative humidity.
!                              UNITS:      %
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Same as Pressure
!                              ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Mixing_Ratio_TL:       Tangent-linear water vapor mass mixing ratio.
!                              Set to zero for invalid input.
!                              UNITS:      g/kg
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Same as input Pressure
!                              ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Ice_Temperature:       Temperature below which the saturation vapor
!                              pressure over ice is used in the conversion.
!                              By default, only the saturation vapor pressure
!                              over water is used.
!                              UNITS:      Kelvin, K
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Min_Pressure:          Pressure value below which the saturation
!                              mixing ratio is not calculated. The default
!                              is 50mb. Saturation mixing ratios below the
!                              minimum pressure are set to zero. This is
!                              because at pressures less than 50mb, the
!                              saturation vapour Pressure, which is based
!                              only on temperature, can exceed the total
!                              air Pressure.
!                              UNITS:      hectoPascals, hPa
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 17-Aug-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE RH_to_MR_TL( &
    P              , &  ! FWD Input
    T              , &  ! FWD Input
    rh             , &  ! FWD Input
    P_TL           , &  ! TL  Input
    T_TL           , &  ! TL  Input
    rh_TL          , &  ! TL  Input
    mr_TL          , &  ! TL  Output
    Ice_Temperature, &  ! Optional Input
    Min_Pressure     )  ! Optional Input
    ! Arguments
    REAL(fp),           INTENT(IN)  :: P
    REAL(fp),           INTENT(IN)  :: T
    REAL(fp),           INTENT(IN)  :: rh
    REAL(fp),           INTENT(IN)  :: P_TL
    REAL(fp),           INTENT(IN)  :: T_TL
    REAL(fp),           INTENT(IN)  :: rh_TL
    REAL(fp),           INTENT(OUT) :: mr_TL
    REAL(fp), OPTIONAL, INTENT(IN)  :: Ice_Temperature
    REAL(fp), OPTIONAL, INTENT(IN)  :: Min_Pressure
    ! Local variables
    REAL(fp) :: smr, smr_TL

    ! Setup
    IF ( P < ZERO .OR. T < ZERO .OR. rh < ZERO .OR. rh > HUNDRED ) THEN
      mr_TL = ZERO
      RETURN
    ENDIF

    ! Calculate saturation mixing ratios in g/kg
    CALL Saturation_Mixing_Ratio( P, &
                                  T, &
                                  smr, &
                                  Ice_Temperature = Ice_Temperature, &
                                  Min_Pressure    = Min_Pressure )
    CALL Saturation_Mixing_Ratio_TL( P, &
                                     T, &
                                     P_TL, &
                                     T_TL, &
                                     smr_TL, &
                                     Ice_Temperature = Ice_Temperature, &
                                     Min_Pressure    = Min_Pressure )

    ! Calculate mixing ratio in g/kg
    IF ( smr > ZERO ) THEN
      mr_TL = FROM_PERCENT * ((rh * smr_TL) + (smr * rh_TL))
    ELSE
      mr_TL = ZERO
    END IF

  END SUBROUTINE RH_to_MR_TL


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       RH_to_MR_AD
!
! PURPOSE:
!       Adjoint form of elemental subroutine to convert relative
!       humidity to water vapor mass mixing ratio
!
! CALLING SEQUENCE:
!       CALL RH_to_MR_AD( Pressure                         , &  ! FWD Input
!                         Temperature                      , &  ! FWD Input
!                         Relative_Humidity                , &  ! FWD Input
!                         Mixing_Ration_AD                 , &  ! AD  Input
!                         Pressure_AD                      , &  ! AD  Output
!                         Temperature_AD                   , &  ! AD  Output
!                         Relative_Humidity_AD             , &  ! AD  Output
!                         Ice_Temperature = Ice_Temperature, &  ! Optional input
!                         Min_Pressure    = Min_Pressure     )  ! Optional input
!
! INPUTS:
!       Pressure:              Total atmospheric pressure.
!                              Must be > 0.
!                              UNITS:      hectoPascals, hPa
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Scalar or any rank
!                              ATTRIBUTES: INTENT(IN)
!
!       Temperature:           Atmospheric temperature.
!                              Must be > 0.
!                              UNITS:      Kelvin, K
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Same as Pressure
!                              ATTRIBUTES: INTENT(IN)
!
!       Relative_Humidity:     Atmospheric relative humidity.
!                              Must be >=0 and <=100.
!                              UNITS:      %
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Same as Pressure
!                              ATTRIBUTES: INTENT(IN)
!
!       Mixing_Ratio_AD:       Adjoint water vapor mass mixing ratio.
!                              *** SET TO ZERO ON EXIT ***
!                              UNITS:      g/kg
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Same as Pressure
!                              ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUTS:
!       Pressure_AD:           Adjoint atmospheric pressure.
!                              *** MUST HAVE VALUE ON ENTRY ***
!                              UNITS:      hectoPascals, hPa
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Same as input Pressure
!                              ATTRIBUTES: INTENT(IN OUT)
!
!       Temperature_AD:        Adjoint atmospheric temperature.
!                              *** MUST HAVE VALUE ON ENTRY ***
!                              UNITS:      Kelvin, K
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Same as input Pressure
!                              ATTRIBUTES: INTENT(IN OUT)
!
!       Relative_Humidity_AD:  Adjoint atmospheric relative humidity.
!                              *** MUST HAVE VALUE ON ENTRY ***
!                              UNITS:      %
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Same as input Pressure
!                              ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       Ice_Temperature:       Temperature below which the saturation vapor
!                              pressure over ice is used in the conversion.
!                              By default, only the saturation vapor pressure
!                              over water is used.
!                              UNITS:      Kelvin, K
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Min_Pressure:          Pressure value below which the saturation
!                              mixing ratio is not calculated. The default
!                              is 50mb. Saturation mixing ratios below the
!                              minimum pressure are set to zero. This is
!                              because at pressures less than 50mb, the
!                              saturation vapour Pressure, which is based
!                              only on temperature, can exceed the total
!                              air Pressure.
!                              UNITS:      hectoPascals, hPa
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 17-Aug-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE RH_to_MR_AD( &
    P              , &  ! FWD Input
    T              , &  ! FWD Input
    rh             , &  ! FWD Input
    mr_AD          , &  ! AD  Input
    P_AD           , &  ! AD  Output
    T_AD           , &  ! AD  Output
    rh_AD          , &  ! AD  Output
    Ice_Temperature, &  ! Optional Input
    Min_Pressure     )  ! Optional Input
    ! Arguments
    REAL(fp),           INTENT(IN)     :: P
    REAL(fp),           INTENT(IN)     :: T
    REAL(fp),           INTENT(IN)     :: rh
    REAL(fp),           INTENT(IN OUT) :: mr_AD
    REAL(fp),           INTENT(IN OUT) :: P_AD
    REAL(fp),           INTENT(IN OUT) :: T_AD
    REAL(fp),           INTENT(IN OUT) :: rh_AD
    REAL(fp), OPTIONAL, INTENT(IN)     :: Ice_Temperature
    REAL(fp), OPTIONAL, INTENT(IN)     :: Min_Pressure
    ! Local variables
    REAL(fp) :: smr, smr_AD

    ! Setup
    IF ( P < ZERO .OR. T < ZERO .OR. rh < ZERO .OR. rh > HUNDRED ) THEN
      mr_AD = ZERO
      P_AD  = ZERO
      T_AD  = ZERO
      rh_AD = ZERO
      RETURN
    ENDIF

    ! Calculate saturation mixing ratio
    CALL Saturation_Mixing_Ratio( P, &
                                  T, &
                                  smr, &
                                  Ice_Temperature = Ice_Temperature, &
                                  Min_Pressure    = Min_Pressure )

    ! Adjoint form of RH->MR conversion
    smr_AD = ZERO
    IF ( smr > ZERO ) THEN
      rh_AD  = rh_AD  + (FROM_PERCENT * smr * mr_AD)
      smr_AD = smr_AD + (FROM_PERCENT * rh * mr_AD)
    END IF
    mr_AD = ZERO
    CALL Saturation_Mixing_Ratio_AD( P, &
                                     T, &
                                     smr_AD, &
                                     P_AD, &
                                     T_AD, &
                                     Ice_Temperature = Ice_Temperature, &
                                     Min_Pressure    = Min_Pressure )

  END SUBROUTINE RH_to_MR_AD


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       MR_to_RH
!
! PURPOSE:
!       Elemental subroutine to convert water vapor mass mixing ratio
!       to relative humidity
!
! CALLING SEQUENCE:
!       CALL MR_to_RH( Pressure                         , &  ! Input
!                      Temperature                      , &  ! Input
!                      Mixing_Ratio                     , &  ! Input
!                      Relative_Humidity                , &  ! Output
!                      Ice_Temperature = Ice_Temperature, &  ! Optional input
!                      Min_Pressure    = Min_Pressure     )  ! Optional input
!
! INPUTS:
!       Pressure:             Total atmospheric pressure.
!                             Must be > 0.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or Rank-1 (K x 1)
!                             ATTRIBUTES: INTENT(IN)
!
!       Temperature:          Atmospheric temperature.
!                             Must be > 0.
!                             UNITS:      Kelvin, K
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Mixing_Ratio:         Water vapor mixing ratio.
!                             Must be > 0.
!                             UNITS:      g/kg
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Relative_Humidity:    Relative humidity.
!                             Set to zero for invalid input.
!                             UNITS:      %
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Ice_Temperature:      Temperature below which the saturation vapor
!                             pressure over ice is used in the conversion.
!                             By default, only the saturation vapor pressure
!                             over water is used.
!                             UNITS:      Kelvin, K
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Min_Pressure:         Pressure value below which the saturation
!                             mixing ratio is not calculated. The default
!                             is 50mb. Saturation mixing ratios below the
!                             minimum pressure are set to zero. This is
!                             because at pressures less than 50mb, the
!                             saturation vapour Pressure, which is based
!                             only on temperature, can exceed the total
!                             air Pressure.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! PROCEDURE:
!       Once the saturation mixing ratio is calculated the relative humidity
!       corresponding to the input mixing ratio is determined using:
!
!                                         Mixing_Ratio
!       Relative_Humidity = 100.0 * -------------------------
!                                    Saturation_Mixing_Ratio
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 02-Mar-1999
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE MR_to_RH( &
    P              , &  ! Input
    T              , &  ! Input
    mr             , &  ! Input
    rh             , &  ! Output
    Ice_Temperature, &  ! Optional Input
    Min_Pressure     )  ! Optional Input
    ! Arguments
    REAL(fp),           INTENT(IN)  :: P
    REAL(fp),           INTENT(IN)  :: T
    REAL(fp),           INTENT(IN)  :: mr
    REAL(fp),           INTENT(OUT) :: rh
    REAL(fp), OPTIONAL, INTENT(IN)  :: Ice_Temperature
    REAL(fp), OPTIONAL, INTENT(IN)  :: Min_Pressure
    ! Local variables
    REAL(fp) :: smr

    ! Setup
    IF ( P < ZERO .OR. T < ZERO .OR. mr < ZERO ) THEN
      rh = ZERO
      RETURN
    ENDIF

    ! Calculate saturation mixing ratio in g/kg
    CALL Saturation_Mixing_Ratio( P, &
                                  T, &
                                  smr, &
                                  Ice_Temperature=Ice_Temperature, &
                                  Min_Pressure   =Min_Pressure )

    ! Calculate relative humidity in %
    IF ( smr > ZERO ) THEN
      rh = TO_PERCENT * mr / smr
    ELSE
      rh = ZERO
    END IF

  END SUBROUTINE MR_to_RH


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       MR_to_RH_TL
!
! PURPOSE:
!       Tangent-linear form of elemental subroutine to convert water vapor
!       mass mixing ratio to relative humidity
!
! CALLING SEQUENCE:
!       CALL MR_to_RH_TL( Pressure                         , &  ! FWD Input
!                         Temperature                      , &  ! FWD Input
!                         Mixing_Ratio                     , &  ! FWD Input
!                         Pressure_TL                      , &  ! TL  Input
!                         Temperature_TL                   , &  ! TL  Input
!                         Mixing_Ratio_TL                  , &  ! TL  Input
!                         Relative_Humidity_TL             , &  ! TL  Output
!                         Ice_Temperature = Ice_Temperature, &  ! Optional input
!                         Min_Pressure    = Min_Pressure     )  ! Optional input
!
! INPUTS:
!       Pressure:             Total atmospheric pressure.
!                             Must be > 0.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or Rank-1 (K x 1)
!                             ATTRIBUTES: INTENT(IN)
!
!       Temperature:          Atmospheric temperature.
!                             Must be > 0.
!                             UNITS:      Kelvin, K
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Mixing_Ratio:         Water vapor mixing ratio.
!                             Must be > 0.
!                             UNITS:      g/kg
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Pressure_TL:          Tangent-linear atmospheric pressure.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Temperature_TL:       Tangent-linear atmospheric temperature.
!                             UNITS:      Kelvin, K
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Mixing_Ratio_TL:      Tangent-linear water vapor mixing ratio.
!                             UNITS:      g/kg
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!
! OUTPUTS:
!       Relative_Humidity_TL: Tangent-linear relative humidity.
!                             Set to zero for invalid input.
!                             UNITS:      %
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Ice_Temperature:      Temperature below which the saturation vapor
!                             pressure over ice is used in the conversion.
!                             By default, only the saturation vapor pressure
!                             over water is used.
!                             UNITS:      Kelvin, K
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Min_Pressure:         Pressure value below which the saturation
!                             mixing ratio is not calculated. The default
!                             is 50mb. Saturation mixing ratios below the
!                             minimum pressure are set to zero. This is
!                             because at pressures less than 50mb, the
!                             saturation vapour Pressure, which is based
!                             only on temperature, can exceed the total
!                             air Pressure.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 17-Aug-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE MR_to_RH_TL( &
    P              , &  ! FWD Input
    T              , &  ! FWD Input
    mr             , &  ! FWD Input
    P_TL           , &  ! TL  Input
    T_TL           , &  ! TL  Input
    mr_TL          , &  ! TL  Input
    rh_TL          , &  ! TL  Output
    Ice_Temperature, &  ! Optional Input
    Min_Pressure     )  ! Optional Input
    ! Arguments
    REAL(fp),           INTENT(IN)  :: P
    REAL(fp),           INTENT(IN)  :: T
    REAL(fp),           INTENT(IN)  :: mr
    REAL(fp),           INTENT(IN)  :: P_TL
    REAL(fp),           INTENT(IN)  :: T_TL
    REAL(fp),           INTENT(IN)  :: mr_TL
    REAL(fp),           INTENT(OUT) :: rh_TL
    REAL(fp), OPTIONAL, INTENT(IN)  :: Ice_Temperature
    REAL(fp), OPTIONAL, INTENT(IN)  :: Min_Pressure
    ! Local variables
    REAL(fp) :: smr, smr_TL

    ! Setup
    IF ( P < ZERO .OR. T < ZERO .OR. mr < ZERO ) THEN
      rh_TL = ZERO
      RETURN
    ENDIF

    ! Calculate saturation mixing ratio in g/kg
    CALL Saturation_Mixing_Ratio( P, &
                                  T, &
                                  smr, &
                                  Ice_Temperature=Ice_Temperature, &
                                  Min_Pressure   =Min_Pressure )
    CALL Saturation_Mixing_Ratio_TL( P, &
                                     T, &
                                     P_TL, &
                                     T_TL, &
                                     smr_TL, &
                                     Ice_Temperature = Ice_Temperature, &
                                     Min_Pressure    = Min_Pressure )

    ! Calculate relative humidity in %
    IF ( smr > ZERO ) THEN
      rh_TL = TO_PERCENT * ((smr * mr_TL) - (mr * smr_TL)) / smr**2
    ELSE
      rh_TL = ZERO
    END IF

  END SUBROUTINE MR_to_RH_TL


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       MR_to_RH_AD
!
! PURPOSE:
!       Adjoint form of elemental subroutine to convert water vapor
!       mass mixing ratio to relative humidity
!
! CALLING SEQUENCE:
!       CALL MR_to_RH_AD( Pressure                         , &  ! FWD Input
!                         Temperature                      , &  ! FWD Input
!                         Mixing_Ratio                     , &  ! FWD Input
!                         Relative_Humidity_AD             , &  ! AD  Input
!                         Pressure_AD                      , &  ! AD  Output
!                         Temperature_AD                   , &  ! AD  Output
!                         Mixing_Ratio_AD                  , &  ! AD  Output
!                         Ice_Temperature = Ice_Temperature, &  ! Optional input
!                         Min_Pressure    = Min_Pressure     )  ! Optional input
!
! INPUTS:
!       Pressure:             Total atmospheric pressure.
!                             Must be > 0.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or Rank-1 (K x 1)
!                             ATTRIBUTES: INTENT(IN)
!
!       Temperature:          Atmospheric temperature.
!                             Must be > 0.
!                             UNITS:      Kelvin, K
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Mixing_Ratio:         Water vapor mixing ratio.
!                             Must be > 0.
!                             UNITS:      g/kg
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Relative_Humidity_AD: Adjoint relative humidity.
!                             *** SET TO ZERO ON EXIT ***
!                             UNITS:      %
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUTS:
!       Pressure_AD:          Adjoint atmospheric pressure.
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: INTENT(IN OUT)
!
!       Temperature_AD:       Adjoint atmospheric temperature.
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      Kelvin, K
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: INTENT(IN OUT)
!
!       Mixing_Ratio_AD:      Adjoint water vapor mixing ratio.
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      g/kg
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Ice_Temperature:      Temperature below which the saturation vapor
!                             pressure over ice is used in the conversion.
!                             By default, only the saturation vapor pressure
!                             over water is used.
!                             UNITS:      Kelvin, K
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Min_Pressure:         Pressure value below which the saturation
!                             mixing ratio is not calculated. The default
!                             is 50mb. Saturation mixing ratios below the
!                             minimum pressure are set to zero. This is
!                             because at pressures less than 50mb, the
!                             saturation vapour Pressure, which is based
!                             only on temperature, can exceed the total
!                             air Pressure.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 17-Aug-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE MR_to_RH_AD( &
    P              , &  ! FWD Input
    T              , &  ! FWD Input
    mr             , &  ! FWD Input
    rh_AD          , &  ! AD  Input
    P_AD           , &  ! AD  Output
    T_AD           , &  ! AD  Output
    mr_AD          , &  ! AD  Output
    Ice_Temperature, &  ! Optional Input
    Min_Pressure     )  ! Optional Input
    ! Arguments
    REAL(fp),           INTENT(IN)     :: P
    REAL(fp),           INTENT(IN)     :: T
    REAL(fp),           INTENT(IN)     :: mr
    REAL(fp),           INTENT(IN OUT) :: rh_AD
    REAL(fp),           INTENT(IN OUT) :: P_AD
    REAL(fp),           INTENT(IN OUT) :: T_AD
    REAL(fp),           INTENT(IN OUT) :: mr_AD
    REAL(fp), OPTIONAL, INTENT(IN)     :: Ice_Temperature
    REAL(fp), OPTIONAL, INTENT(IN)     :: Min_Pressure
    ! Local variables
    REAL(fp) :: smr, smr_AD

    ! Setup
    IF ( P < ZERO .OR. T < ZERO .OR. mr < ZERO ) THEN
      rh_AD = ZERO
      P_AD  = ZERO
      T_AD  = ZERO
      mr_AD = ZERO
      RETURN
    ENDIF

    ! Calculate saturation mixing ratio
    CALL Saturation_Mixing_Ratio( P, &
                                  T, &
                                  smr, &
                                  Ice_Temperature = Ice_Temperature, &
                                  Min_Pressure    = Min_Pressure )

    ! Adjoint form of MR->RH conversion
    smr_AD = ZERO
    IF ( smr > ZERO ) THEN
      mr_AD  = mr_AD  + (TO_PERCENT * rh_AD / smr)
      smr_AD = smr_AD - (TO_PERCENT * mr * rh_AD / smr**2)
    END IF
    rh_AD = ZERO
    CALL Saturation_Mixing_Ratio_AD( P, &
                                     T, &
                                     smr_AD, &
                                     P_AD, &
                                     T_AD, &
                                     Ice_Temperature = Ice_Temperature, &
                                     Min_Pressure    = Min_Pressure )

  END SUBROUTINE MR_to_RH_AD

END MODULE RH_MR
