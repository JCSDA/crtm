!
! Atmospheric_Properties
!
! Module containing utility routines to calculate various and sundry
! atmospheric properties.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 01-May-2000
!                       paul.vandelst@noaa.gov
!

MODULE Atmospheric_Properties

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Modules used
  USE Type_Kinds, ONLY: fp
  USE Profile_Utility_Parameters, ONLY: ZERO, ONE, &
                                        P0, R0, R_DRYAIR, Cp_DRYAIR, EPS, &
                                        MW_H2O, MW_DRYAIR, &
                                        PA_TO_HPA, G_TO_KG, KG_TO_G, &
                                        CELSIUS_TO_KELVIN
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: MW_Air, MW_Air_TL, MW_Air_AD
  PUBLIC :: Density, Density_TL, Density_AD
  PUBLIC :: SVP_Water, SVP_Water_TL, SVP_Water_AD
  PUBLIC :: SVP_Ice, SVP_Ice_TL, SVP_Ice_AD
  PUBLIC :: Saturation_Mixing_Ratio, Saturation_Mixing_Ratio_TL, Saturation_Mixing_Ratio_AD
  PUBLIC :: T_to_Tv, T_to_Tv_TL, T_to_Tv_AD
  PUBLIC :: Tv_to_T, Tv_to_T_TL, Tv_to_T_AD
  PUBLIC :: T_to_Theta, T_to_Theta_TL, T_to_Theta_AD
  PUBLIC :: Theta_to_T, Theta_to_T_TL, Theta_to_T_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &

  ! Scalng factor to return density in kg.m^-3
  REAL(fp), PARAMETER :: RHO_SCALE_FACTOR = 0.1_fp

  ! Coefficients for saturation vapor pressure over water
  INTEGER,  PARAMETER :: N_SVPW_COEFFICIENTS = 8
  REAL(fp), PARAMETER :: SVPW_COEFFICIENTS(0:N_SVPW_COEFFICIENTS) = &
    (/-3.21582393e-16_fp, 3.79534310e-14_fp, 7.02620698e-11_fp, &
       2.03154182e-08_fp, 2.99291081e-06_fp, 2.64224321e-04_fp, &
       1.43177157e-02_fp, 4.44606896e-01_fp, 6.11583699e+00_fp /)
  ! ...Valid temperature range
  REAL(fp), PARAMETER :: MIN_SVPW_TEMPERATURE = 188.15_fp
  REAL(fp), PARAMETER :: MAX_SVPW_TEMPERATURE = 343.15_fp

  ! Coefficients for saturation vapor pressure over ice
  INTEGER,  PARAMETER :: N_SVPI_COEFFICIENTS = 8
  REAL(fp), PARAMETER :: SVPI_COEFFICIENTS(0:N_SVPI_COEFFICIENTS) = &
    (/ 1.61444444e-15_fp, 1.05785160e-12_fp, 3.07839583e-10_fp, &
       5.21693933e-08_fp, 5.65392987e-06_fp, 4.02737184e-04_fp, &
       1.84672631e-02_fp, 4.99320233e-01_fp, 6.09868993e+00_fp /)
  ! ...Valid temperature range
  REAL(fp), PARAMETER :: MIN_SVPI_TEMPERATURE = 183.15_fp
  REAL(fp), PARAMETER :: MAX_SVPI_TEMPERATURE = 273.15_fp

  ! Minimum pressure for saturation mixing ratio calculation
  REAL(fp), PARAMETER :: MIN_SMR_PRESSURE = 50.0_fp

  ! Parameters for potential temperature procedures
  ! ...Standard atmosphere in hectoPascals
  REAL(fp), PARAMETER :: PSTD = P0 * PA_TO_HPA
  ! ...Exponent term
  REAL(fp), PARAMETER :: EXPONENT_TERM = R_DRYAIR / Cp_DRYAIR


CONTAINS


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       MW_Air
!
! PURPOSE:
!       Elemental subroutine to calculate the effective, water vapor weighted
!       molecular weight of air.
!
! CALLING SEQUENCE:
!       CALL MW_Air( Pressure            , &  ! Input
!                    Water_Vapor_Pressure, &  ! Input
!                    Molecular_Weight      )  ! Output
!
! INPUTS:
!       Pressure:              Total atmospheric pressure
!                              Must be > 0
!                              UNITS:      hectoPascals, hPa
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Scalar or any rank
!                              ATTRIBUTES: INTENT(IN)
!
!       Water_Vapor_Pressure:  Water vapor partial pressure
!                              Must be > 0
!                              UNITS:      hectoPascals, hPa.
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Same as Pressure
!                              ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Molecular_Weight:      The effective molecular weight of air.
!                              Set to zero for invalid input.
!                              UNITS:      grams, g
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Same as Pressure
!                              ATTRIBUTES: INTENT(OUT)
!
! PROCEDURE:
!       The change in the effective molecular weight of dry air
!       due to water vapor is given by:
!
!                      pp(h2o) * ( MW(H2O) - MW(DRY_AIR) )
!         d(MW_Air) = -------------------------------------
!                                   Pressure
!
!       and the final result is given by:
!
!         MW(Air) = MW(DRY_AIR) + d(MW_Air)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE MW_Air( &
    p , &  ! Input
    pp, &  ! Input
    mw  )  ! Output
    ! Arguments
    REAL(fp), INTENT(IN)  :: p
    REAL(fp), INTENT(IN)  :: pp
    REAL(fp), INTENT(OUT) :: mw
    ! Local variables
    REAL(fp) :: dmw

    ! Setup
    IF ( p < ZERO .OR. pp < ZERO ) THEN
      mw = ZERO
      RETURN
    END IF

    ! Compute the water vapour weight air molecular weight
    dmw = (MW_H2O - MW_DRYAIR) * pp / p
    mw  = MW_DRYAIR + dmw

  END SUBROUTINE MW_Air


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       MW_Air_TL
!
! PURPOSE:
!       Tangent-linear form of elemental subroutine to calculate the effective,
!       water vapor weighted molecular weight of air.
!
! CALLING SEQUENCE:
!       CALL MW_Air_TL( Pressure               , &  ! FWD Input
!                       Water_Vapor_Pressure   , &  ! FWD Input
!                       Pressure_TL            , &  ! TL  Input
!                       Water_Vapor_Pressure_TL, &  ! TL  Input
!                       Molecular_Weight_TL      )  ! TL  Output
!
! INPUTS:
!       Pressure:                 Total atmospheric pressure
!                                 Must be > 0
!                                 UNITS:      hectoPascals, hPa
!                                 TYPE:       REAL(fp)
!                                 DIMENSION:  Scalar or any rank
!                                 ATTRIBUTES: INTENT(IN)
!
!       Water_Vapor_Pressure:     Water vapor partial pressure
!                                 Must be > 0
!                                 UNITS:      hectoPascals, hPa.
!                                 TYPE:       REAL(fp)
!                                 DIMENSION:  Same as Pressure
!                                 ATTRIBUTES: INTENT(IN)
!
!       Pressure_TL:              Tangent-linear atmospheric pressure
!                                 UNITS:      hectoPascals, hPa
!                                 TYPE:       REAL(fp)
!                                 DIMENSION:  Same as Pressure
!                                 ATTRIBUTES: INTENT(IN)
!
!       Water_Vapor_Pressure_TL:  Tangent-linear water vapor partial pressure
!                                 UNITS:      hectoPascals, hPa.
!                                 TYPE:       REAL(fp)
!                                 DIMENSION:  Same as Pressure
!                                 ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Molecular_Weight_TL:      Tangent-linear effective molecular weight
!                                 of air.
!                                 Set to zero for invalid input.
!                                 UNITS:      grams, g
!                                 TYPE:       REAL(fp)
!                                 DIMENSION:  Same as Pressure
!                                 ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE MW_Air_TL( &
    p    , &  ! FWD Input
    pp   , &  ! FWD Input
    p_TL , &  ! TL  Input
    pp_TL, &  ! TL  Input
    mw_TL  )  ! TL  Output
    ! Arguments
    REAL(fp), INTENT(IN)  :: p
    REAL(fp), INTENT(IN)  :: pp
    REAL(fp), INTENT(IN)  :: p_TL
    REAL(fp), INTENT(IN)  :: pp_TL
    REAL(fp), INTENT(OUT) :: mw_TL

    ! Setup
    IF ( p < ZERO .OR. pp < ZERO ) THEN
      mw_TL = ZERO
      RETURN
    END IF

    ! Tangent-linear form of the water vapour weighted air molecular weight
    mw_TL = (MW_H2O - MW_DRYAIR) * ((pp_TL / p) - (pp * p_TL / p**2))

  END SUBROUTINE MW_Air_TL


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       MW_Air_AD
!
! PURPOSE:
!       Adjoint form of elemental subroutine to calculate the effective,
!       water vapor weighted molecular weight of air.
!
! CALLING SEQUENCE:
!       CALL MW_Air_AD( Pressure               , &  ! FWD Input
!                       Water_Vapor_Pressure   , &  ! FWD Input
!                       Molecular_Weight_AD    , &  ! AD  Input
!                       Pressure_AD            , &  ! AD  Output
!                       Water_Vapor_Pressure_AD  )  ! AD  Output
!
! INPUTS:
!       Pressure:                 Total atmospheric pressure
!                                 Must be > 0
!                                 UNITS:      hectoPascals, hPa
!                                 TYPE:       REAL(fp)
!                                 DIMENSION:  Scalar or any rank
!                                 ATTRIBUTES: INTENT(IN)
!
!       Water_Vapor_Pressure:     Water vapor partial pressure
!                                 Must be > 0
!                                 UNITS:      hectoPascals, hPa.
!                                 TYPE:       REAL(fp)
!                                 DIMENSION:  Same as Pressure
!                                 ATTRIBUTES: INTENT(IN)
!
!       Molecular_Weight_AD:      Adjoint effective molecular weight
!                                 of air.
!                                 *** SET TO ZERO ON EXIT ***
!                                 UNITS:      grams, g
!                                 TYPE:       REAL(fp)
!                                 DIMENSION:  Same as Pressure
!                                 ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUTS:
!       Pressure_AD:              Adjoint atmospheric pressure
!                                 *** MUST HAVE VALUE ON ENTRY ***
!                                 UNITS:      g/hPa
!                                 TYPE:       REAL(fp)
!                                 DIMENSION:  Same as Pressure
!                                 ATTRIBUTES: INTENT(IN OUT)
!
!       Water_Vapor_Pressure_AD:  Adjoint water vapor partial pressure
!                                 *** MUST HAVE VALUE ON ENTRY ***
!                                 UNITS:      g/hPa.
!                                 TYPE:       REAL(fp)
!                                 DIMENSION:  Same as Pressure
!                                 ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE MW_Air_AD( &
    p    , &  ! FWD Input
    pp   , &  ! FWD Input
    mw_AD, &  ! AD  Input
    p_AD , &  ! AD  Output
    pp_AD  )  ! AD  Output
    ! Arguments
    REAL(fp), INTENT(IN)     :: p
    REAL(fp), INTENT(IN)     :: pp
    REAL(fp), INTENT(IN OUT) :: mw_AD
    REAL(fp), INTENT(IN OUT) :: p_AD
    REAL(fp), INTENT(IN OUT) :: pp_AD

    ! Setup
    IF ( p < ZERO .OR. pp < ZERO ) THEN
      mw_AD = ZERO
      p_AD  = ZERO
      pp_AD = ZERO
      RETURN
    END IF

    ! Adjoint form of the water vapour weighted air molecular weight
    p_AD  = p_AD  - ((MW_H2O - MW_DRYAIR) * pp * mw_AD / p**2)
    pp_AD = pp_AD + ((MW_H2O - MW_DRYAIR) * mw_AD / p)
    mw_AD = ZERO

  END SUBROUTINE MW_Air_AD


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       Density
!
! PURPOSE:
!       Elemental subroutine to calculate gas density using the ideal gas law.
!
! CALLING SEQUENCE:
!       CALL Density( Pressure        , &  ! Input
!                     Temperature     , &  ! Input
!                     Molecular_Weight, &  ! Input
!                     Gas_Density       )  ! Output
!
! INPUTS:
!       Pressure:             Pressure of gas
!                             UNITS:      hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or any rank.
!                             ATTRIBUTES: INTENT(IN)
!
!       Temperature:          Temperature of gas
!                             UNITS:      Kelvin
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Molecular_Weight:     Molecular weight of the gas.
!                             UNITS:      g.mol^-1
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Gas_Density:          The gas density for the specified conditions.
!                             Set to zero for invalid input
!                             UNITS:      kg.m^-3
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(OUT)
!
! PROCEDURE:
!       The density is calculated using the ideal gas equation
!
!                    p . MW
!         Density = --------
!                    R0 . T
!
!       where R0 = universal gas constant.
!
!       Units:
!       ------
!       Pressure           : hPa == 100 Pa == 100 kg.m^-1.s^-2
!       Molecular_Weight   : g.mol^-1 == 0.001 kg.mol^-1
!       MOLAR_GAS_CONSTANT : J.K^-1.mol^-1 == kg.m^2.s^-2.K^-1.mol^-1
!       Temperature        : K
!
!                  100 kg.m^-1.s^-2 . 0.001 kg.mol^-1
!       Density = -----------------------------------
!                     kg.m^2.s^-2.K^-1.mol^-1 . K
!
!                  0.1 kg^2.m^-1.s^-2
!               = --------------------
!                      kg.m^2.s^-2
!
!               = 0.1 kg.m^-3
!
!       Thus the result is scaled by 0.1 to return density in units
!       of kg.m^-3.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Density( &
    P , &  ! Input
    T , &  ! Input
    mw, &  ! Input
    rho )  ! Output
    ! Arguments
    REAL(fp), INTENT(IN)  :: P
    REAL(fp), INTENT(IN)  :: T
    REAL(fp), INTENT(IN)  :: mw
    REAL(fp), INTENT(OUT) :: rho

    ! Setup
    IF ( P < ZERO .OR. T < ZERO .OR. mw < ZERO ) THEN
      rho = ZERO
      RETURN
    END IF

    ! Calculate density
    rho = RHO_SCALE_FACTOR * P * mw / ( R0 * T )

  END SUBROUTINE Density


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       Density_TL
!
! PURPOSE:
!       Tangent-linear form of elemental subroutine to calculate gas density
!       using the ideal gas law.
!
! CALLING SEQUENCE:
!       CALL Density_TL( Pressure           , &  ! FWD Input
!                        Temperature        , &  ! FWD Input
!                        Molecular_Weight   , &  ! FWD Input
!                        Pressure_TL        , &  ! TL  Input
!                        Temperature_TL     , &  ! TL  Input
!                        Molecular_Weight_TL, &  ! TL  Input
!                        Gas_Density_TL       )  ! TL  Output
!
! INPUTS:
!       Pressure:             Pressure of gas
!                             UNITS:      hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or any rank.
!                             ATTRIBUTES: INTENT(IN)
!
!       Temperature:          Temperature of gas
!                             UNITS:      Kelvin
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Molecular_Weight:     Molecular weight of the gas.
!                             UNITS:      g.mol^-1
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Pressure_TL:          Tangent-linear pressure of gas
!                             UNITS:      hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Temperature_TL:       Tangent-linear temperature of gas
!                             UNITS:      Kelvin
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Molecular_Weight_TL:  Tangent-linear molecular weight of the gas.
!                             UNITS:      g.mol^-1
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Gas_Density_TL:       Tangent-linear gas density for the specified
!                             conditions.
!                             Set to zero for invalid input
!                             UNITS:      kg.m^-3
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Density_TL( &
    P    , &  ! FWD Input
    T    , &  ! FWD Input
    mw   , &  ! FWD Input
    P_TL , &  ! TL  Input
    T_TL , &  ! TL  Input
    mw_TL, &  ! TL  Input
    rho_TL )  ! TL  Output
    ! Arguments
    REAL(fp), INTENT(IN)  :: P
    REAL(fp), INTENT(IN)  :: T
    REAL(fp), INTENT(IN)  :: mw
    REAL(fp), INTENT(IN)  :: P_TL
    REAL(fp), INTENT(IN)  :: T_TL
    REAL(fp), INTENT(IN)  :: mw_TL
    REAL(fp), INTENT(OUT) :: rho_TL
    ! Local variables
    REAL(fp) :: c

    ! Setup
    IF ( P < ZERO .OR. T < ZERO .OR. mw < ZERO ) THEN
      rho_TL = ZERO
      RETURN
    END IF

    ! Tangent-linear form of density
    c = RHO_SCALE_FACTOR / R0

    rho_TL = c * ((P_TL * mw / T) + (mw_TL * P / T) - (T_TL * P * mw / T**2))

  END SUBROUTINE Density_TL


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       Density_AD
!
! PURPOSE:
!       Adjoint form of elemental subroutine to calculate gas density
!       using the ideal gas law.
!
! CALLING SEQUENCE:
!       CALL Density_AD( Pressure           , &  ! FWD Input
!                        Temperature        , &  ! FWD Input
!                        Molecular_Weight   , &  ! FWD Input
!                        Pressure_AD        , &  ! AD  Input
!                        Temperature_AD     , &  ! AD  Input
!                        Molecular_Weight_AD, &  ! AD  Input
!                        Gas_Density_AD       )  ! AD  Output
!
! INPUTS:
!       Pressure:             Pressure of gas
!                             UNITS:      hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or any rank.
!                             ATTRIBUTES: INTENT(IN)
!
!       Temperature:          Temperature of gas
!                             UNITS:      Kelvin
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Molecular_Weight:     Molecular weight of the gas.
!                             UNITS:      g.mol^-1
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Gas_Density_AD:       Adjoint gas density for the specified
!                             conditions.
!                             *** SET TO ZERO ON EXIT ***
!                             UNITS:      kg.m^-3
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUTS:
!       Pressure_AD:          Adjoint pressure of gas
!                             Set to zero for invalid input
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      kg.m^-3/hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN OUT)
!
!       Temperature_AD:       Adjoint temperature of gas
!                             Set to zero for invalid input
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      kg.m^-3/Kelvin
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN OUT)
!
!       Molecular_Weight_AD:  Adjoint molecular weight of the gas.
!                             Set to zero for invalid input
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      kg.m^-3/g.mol^-1
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Density_AD( &
    P     , &  ! FWD Input
    T     , &  ! FWD Input
    mw    , &  ! FWD Input
    rho_AD, &  ! AD  Input
    P_AD  , &  ! AD  Output
    T_AD  , &  ! AD  Output
    mw_AD   )  ! AD  Output
    ! Arguments
    REAL(fp), INTENT(IN)     :: P
    REAL(fp), INTENT(IN)     :: T
    REAL(fp), INTENT(IN)     :: mw
    REAL(fp), INTENT(IN OUT) :: rho_AD
    REAL(fp), INTENT(IN OUT) :: P_AD
    REAL(fp), INTENT(IN OUT) :: T_AD
    REAL(fp), INTENT(IN OUT) :: mw_AD
    ! Local variables
    REAL(fp) :: c

    ! Setup
    IF ( P < ZERO .OR. T < ZERO .OR. mw < ZERO ) THEN
      rho_AD = ZERO
      P_AD   = ZERO
      T_AD   = ZERO
      mw_AD  = ZERO
      RETURN
    END IF

    ! Tangent-linear form of density
    c = RHO_SCALE_FACTOR / R0

    T_AD   = T_AD  - (rho_AD * c * P * mw / T**2)
    mw_AD  = mw_AD + (rho_AD * c * P / T)
    P_AD   = P_AD  + (rho_AD * c * mw / T)
    rho_AD = ZERO

  END SUBROUTINE Density_AD


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       SVP_Water
!
! PURPOSE:
!       Elemental subroutine to calculate the saturation vapor pressure
!       over water.
!
! CALLING SEQUENCE:
!       CALL SVP_Water( Temperature   , &  ! Input
!                       Vapor_Pressure  )  ! Output
!
! INPUTS:
!       Temperature:      Temperatures for which the saturation vapor
!                         pressure is required.
!                         Valid temperature range is 188K - 343K (-85C - +70C).
!                         UNITS:      Kelvin, K
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Scalar or any rank.
!                         ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Vapor_Pressure:   The saturation vapor pressure over water.
!                         Value is set to zero if input temperatures are
!                         outside the valid range.
!                         UNITS:      hectoPascals, hPa
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Same as input Temperature
!                         ATTRIBUTES: INTENT(OUT)
!
! PROCEDURE:
!       Flatau,P.J., R.L.Walko, and W.R.Cotton, 1992: "Polynomial fits to
!         saturation vapor pressure", J.Appl.Met., v31, pp1507-1513
!
!                           __ N
!                          \            i
!         SVP_Water = c0 +  >   c(i) . T
!                          /__
!                             i=1
!
!       where the c(i) are the relative error norm coefficients obtained
!       from the reference above.
!
!       Horner's method is used to evaluate the above polynomial.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE SVP_Water( &
    Temperature,   &  ! Input
    Vapor_Pressure )  ! Output
    ! Arguments
    REAL(fp), INTENT(IN)  :: Temperature
    REAL(fp), INTENT(OUT) :: Vapor_Pressure
    ! Local variables
    INTEGER :: i
    REAL(fp) :: T

    ! Setup
    IF ( Temperature < MIN_SVPW_TEMPERATURE .OR. Temperature > MAX_SVPW_TEMPERATURE ) THEN
      Vapor_Pressure = ZERO
      RETURN
    END IF

    ! Calculate saturation vapor pressure
    T = Temperature - CELSIUS_TO_KELVIN
    Vapor_Pressure = SVPW_COEFFICIENTS(0)
    DO i = 1, N_SVPW_COEFFICIENTS
      Vapor_Pressure = (Vapor_Pressure * T) + SVPW_COEFFICIENTS(i)
    END DO

  END SUBROUTINE SVP_Water


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       SVP_Water_TL
!
! PURPOSE:
!       Tangent-linear form of elemental subroutine to calculate the saturation
!       vapor pressure over water.
!
! CALLING SEQUENCE:
!       CALL SVP_Water_TL( Temperature      , &  ! FWD Input
!                          Temperature_TL   , &  ! TL  Input
!                          Vapor_Pressure_TL  )  ! TL  Output
!
! INPUTS:
!       Temperature:       Temperature for which the saturation vapor
!                          pressure is required.
!                          Valid temperature range is 188K - 343K (-85C - +70C).
!                          UNITS:      Kelvin, K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar or any rank.
!                          ATTRIBUTES: INTENT(IN)
!
!       Temperature_TL:    Tangent-linear temperature.
!                          UNITS:      Kelvin, K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as input Temperature
!                          ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Vapor_Pressure_TL: The tangent-linear saturation vapor pressure over
!                          water.
!                          Value is set to zero if input temperatures are
!                          outside the valid range.
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as input Temperature
!                          ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE SVP_Water_TL( &
    Temperature      , &  ! FWD Input
    Temperature_TL   , &  ! TL  Input
    Vapor_Pressure_TL  )  ! TL  Output
    ! Arguments
    REAL(fp), INTENT(IN)  :: Temperature
    REAL(fp), INTENT(IN)  :: Temperature_TL
    REAL(fp), INTENT(OUT) :: Vapor_Pressure_TL
    ! Local variables
    INTEGER :: i
    REAL(fp) :: T, T_TL
    REAL(fp) :: Vapor_Pressure

    ! Setup
    IF ( Temperature < MIN_SVPW_TEMPERATURE .OR. Temperature > MAX_SVPW_TEMPERATURE ) THEN
      Vapor_Pressure_TL = ZERO
      RETURN
    END IF

    ! Tangent-linear form of saturation vapor pressure calculation
    T    = Temperature - CELSIUS_TO_KELVIN
    T_TL = Temperature_TL
    Vapor_Pressure    = SVPW_COEFFICIENTS(0)
    Vapor_Pressure_TL = ZERO
    DO i = 1, N_SVPW_COEFFICIENTS
      Vapor_Pressure_TL = (T * Vapor_Pressure_TL) + (Vapor_Pressure * T_TL)
      Vapor_Pressure    = (Vapor_Pressure * T) + SVPW_COEFFICIENTS(i)
    END DO

  END SUBROUTINE SVP_Water_TL


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       SVP_Water_AD
!
! PURPOSE:
!       Adjoint form of elemental subroutine to calculate the saturation
!       vapor pressure over water.
!
! CALLING SEQUENCE:
!       CALL SVP_Water_AD( Temperature      , &  ! FWD Input
!                          Vapor_Pressure_AD, &  ! AD  Input
!                          Temperature_AD     )  ! AD  Output
!
! INPUTS:
!       Temperature:       Temperature for which the saturation vapor
!                          pressure is required.
!                          Valid temperature range is 188K - 343K (-85C - +70C).
!                          UNITS:      Kelvin, K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar or any rank.
!                          ATTRIBUTES: INTENT(IN)
!
!       Vapor_Pressure_AD: The adjoint saturation vapor pressure over water.
!                          *** SET TO ZERO ON EXIT ***
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as input Temperature
!                          ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUTS:
!       Temperature_AD:    Adjoint temperature.
!                          *** MUST HAVE VALUE ON ENTRY ***
!                          UNITS:      hPa/K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as input Temperature
!                          ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE SVP_Water_AD( &
    Temperature      , &  ! FWD Input
    Vapor_Pressure_AD, &  ! AD  Input
    Temperature_AD     )  ! AD  Output
    ! Arguments
    REAL(fp), INTENT(IN)     :: Temperature
    REAL(fp), INTENT(IN OUT) :: Vapor_Pressure_AD
    REAL(fp), INTENT(IN OUT) :: Temperature_AD
    ! Local variables
    INTEGER :: i
    REAL(fp) :: T
    REAL(fp) :: Vapor_Pressure(0:N_SVPW_COEFFICIENTS)

    ! Setup
    IF ( Temperature < MIN_SVPW_TEMPERATURE .OR. Temperature > MAX_SVPW_TEMPERATURE ) THEN
      Vapor_Pressure_AD = ZERO
      Temperature_AD    = ZERO
      RETURN
    END IF

    ! Forward calculation
    T = Temperature - CELSIUS_TO_KELVIN
    Vapor_Pressure(0) = SVPW_COEFFICIENTS(0)
    DO i = 1, N_SVPW_COEFFICIENTS
      Vapor_Pressure(i) = (Vapor_Pressure(i-1) * T) + SVPW_COEFFICIENTS(i)
    END DO

    ! Adjoint form of saturation vapor pressure calculation
    DO i = N_SVPW_COEFFICIENTS, 1, -1
      Temperature_AD    = Temperature_AD + (Vapor_Pressure(i-1) * Vapor_Pressure_AD)
      Vapor_Pressure_AD = T * Vapor_Pressure_AD
    END DO
    Vapor_Pressure_AD = ZERO

  END SUBROUTINE SVP_Water_AD


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       SVP_Ice
!
! PURPOSE:
!       Elemental subroutine to calculate the saturation vapor pressure
!       over ice.
!
! CALLING SEQUENCE:
!       CALL SVP_Ice( Temperature   , &  ! Input
!                     Vapor_Pressure  )  ! Output
!
! INPUTS:
!       Temperature:      Temperatures for which the saturation vapor
!                         pressure is required.
!                          Valid temperature range is 183K - 273K (-90C - 0C).
!                         UNITS:      Kelvin, K
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Scalar or any rank.
!                         ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Vapor_Pressure:   The saturation vapor pressure over ice.
!                         Value is set to zero if input temperatures are
!                         outside the valid range.
!                         UNITS:      hectoPascals, hPa
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Same as input Temperature
!                         ATTRIBUTES: INTENT(OUT)
!
! PROCEDURE:
!       Flatau,P.J., R.L.Walko, and W.R.Cotton, 1992: "Polynomial fits to
!         saturation vapor pressure", J.Appl.Met., v31, pp1507-1513
!
!                         __ N
!                        \            i
!         SVP_Ice = c0 +  >   c(i) . T
!                        /__
!                           i=1
!
!       where the c(i) are the relative error norm coefficients obtained
!       from the reference above.
!
!       Horner's method is used to evaluate the above polynomial.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE SVP_Ice( &
    Temperature,   &  ! Input
    Vapor_Pressure )  ! Output
    ! Arguments
    REAL(fp), INTENT(IN)  :: Temperature
    REAL(fp), INTENT(OUT) :: Vapor_Pressure
    ! Local variables
    INTEGER  :: i
    REAL(fp) :: T

    ! Setup
    IF ( Temperature < MIN_SVPI_TEMPERATURE .OR. Temperature > MAX_SVPI_TEMPERATURE ) THEN
      Vapor_Pressure = ZERO
      RETURN
    END IF

    ! Calculate saturation vapor pressure
    T = Temperature - CELSIUS_TO_KELVIN
    Vapor_Pressure = SVPI_COEFFICIENTS(0)
    DO i = 1, N_SVPI_COEFFICIENTS
      Vapor_Pressure = (Vapor_Pressure * T) + SVPI_COEFFICIENTS(i)
    END DO

  END SUBROUTINE SVP_Ice


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       SVP_Ice_TL
!
! PURPOSE:
!       Tangent-linear form of elemental subroutine to calculate the saturation
!       vapor pressure over ice.
!
! CALLING SEQUENCE:
!       CALL SVP_Ice_TL( Temperature      , &  ! FWD Input
!                        Temperature_TL   , &  ! TL  Input
!                        Vapor_Pressure_TL  )  ! TL  Output
!
! INPUTS:
!       Temperature:       Temperature for which the saturation vapor
!                          pressure is required.
!                          Valid temperature range is 183K - 273K (-90C - 0C).
!                          UNITS:      Kelvin, K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar or any rank.
!                          ATTRIBUTES: INTENT(IN)
!
!       Temperature_TL:    Tangent-linear temperature.
!                          UNITS:      Kelvin, K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as input Temperature
!                          ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Vapor_Pressure_TL: The tangent-linear saturation vapor pressure over
!                          ice.
!                          Value is set to zero if input temperatures are
!                          outside the valid range.
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as input Temperature
!                          ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE SVP_Ice_TL( &
    Temperature      , &  ! FWD Input
    Temperature_TL   , &  ! TL  Input
    Vapor_Pressure_TL  )  ! TL  Output
    ! Arguments
    REAL(fp), INTENT(IN)  :: Temperature
    REAL(fp), INTENT(IN)  :: Temperature_TL
    REAL(fp), INTENT(OUT) :: Vapor_Pressure_TL
    ! Local variables
    INTEGER :: i
    REAL(fp) :: T, T_TL
    REAL(fp) :: Vapor_Pressure

    ! Setup
    IF ( Temperature < MIN_SVPI_TEMPERATURE .OR. Temperature > MAX_SVPI_TEMPERATURE ) THEN
      Vapor_Pressure_TL = ZERO
      RETURN
    END IF

    ! Tangent-linear form of saturation vapor pressure calculation
    T    = Temperature - CELSIUS_TO_KELVIN
    T_TL = Temperature_TL
    Vapor_Pressure    = SVPI_COEFFICIENTS(0)
    Vapor_Pressure_TL = ZERO
    DO i = 1, N_SVPI_COEFFICIENTS
      Vapor_Pressure_TL = (T * Vapor_Pressure_TL) + (Vapor_Pressure * T_TL)
      Vapor_Pressure    = (Vapor_Pressure * T) + SVPI_COEFFICIENTS(i)
    END DO

  END SUBROUTINE SVP_Ice_TL


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       SVP_Ice_AD
!
! PURPOSE:
!       Adjoint form of elemental subroutine to calculate the saturation
!       vapor pressure over ice.
!
! CALLING SEQUENCE:
!       CALL SVP_Ice_AD( Temperature      , &  ! FWD Input
!                        Vapor_Pressure_AD, &  ! AD  Input
!                        Temperature_AD     )  ! AD  Output
!
! INPUTS:
!       Temperature:       Temperature for which the saturation vapor
!                          pressure is required.
!                          Valid temperature range is 183K - 273K (-90C - 0C).
!                          UNITS:      Kelvin, K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar or any rank.
!                          ATTRIBUTES: INTENT(IN)
!
!       Vapor_Pressure_AD: The adjoint saturation vapor pressure over ice.
!                          *** SET TO ZERO ON EXIT ***
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as input Temperature
!                          ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUTS:
!       Temperature_AD:    Adjoint temperature.
!                          *** MUST HAVE VALUE ON ENTRY ***
!                          UNITS:      hPa/K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as input Temperature
!                          ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE SVP_Ice_AD( &
    Temperature      , &  ! FWD Input
    Vapor_Pressure_AD, &  ! AD  Input
    Temperature_AD     )  ! AD  Output
    ! Arguments
    REAL(fp), INTENT(IN)     :: Temperature
    REAL(fp), INTENT(IN OUT) :: Vapor_Pressure_AD
    REAL(fp), INTENT(IN OUT) :: Temperature_AD
    ! Local variables
    INTEGER :: i
    REAL(fp) :: T
    REAL(fp) :: Vapor_Pressure(0:N_SVPI_COEFFICIENTS)

    ! Setup
    IF ( Temperature < MIN_SVPI_TEMPERATURE .OR. Temperature > MAX_SVPI_TEMPERATURE ) THEN
      Vapor_Pressure_AD = ZERO
      Temperature_AD    = ZERO
      RETURN
    END IF

    ! Forward calculation
    T = Temperature - CELSIUS_TO_KELVIN
    Vapor_Pressure(0) = SVPI_COEFFICIENTS(0)
    DO i = 1, N_SVPI_COEFFICIENTS
      Vapor_Pressure(i) = (Vapor_Pressure(i-1) * T) + SVPI_COEFFICIENTS(i)
    END DO

    ! Adjoint form of saturation vapor pressure calculation
    DO i = N_SVPI_COEFFICIENTS, 1, -1
      Temperature_AD    = Temperature_AD + (Vapor_Pressure(i-1) * Vapor_Pressure_AD)
      Vapor_Pressure_AD = T * Vapor_Pressure_AD
    END DO
    Vapor_Pressure_AD = ZERO

  END SUBROUTINE SVP_Ice_AD


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       Saturation_Mixing_Ratio
!
! PURPOSE:
!       Elemental subroutine to calculate the saturation mixing ratio for
!       a given pressure and temperature
!
! CALLING SEQUENCE:
!       CALL Saturation_Mixing_Ratio( Pressure                         , &  ! Input
!                                     Temperature                      , &  ! Input
!                                     Mixing_Ratio                     , &  ! Output
!                                     Ice_Temperature = Ice_Temperature, &  ! Optional input
!                                     Min_Pressure    = Min_Pressure     )  ! Optional input
!
! INPUTS:
!       Pressure:          Total atmospheric pressure.
!                          Valid pressures are > 50hPa.
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar or any rank.
!                          ATTRIBUTES: INTENT(IN)
!
!       Temperature:       Atmospheric Temperature.
!                          Valid temperature ranges for saturation vapor
!                          pressure calculation are:
!                            Over ice:   183K - 273K (-90C - 0C).
!                            Over water: 188K - 343K (-85C - +70C).
!                          UNITS:      Kelvin, K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Pressure
!                          ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Mixing_Ratio:      The saturation mixing ratio for the supplied
!                          pressure and temperature.
!                          Value is set to zero for invalid input.
!                          UNITS:      g/kg
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Pressure
!                          ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Ice_Temperature:   Temperature below which the saturation vapor
!                          pressure over ice is used in the conversion.
!                          By default, only the saturation vapor pressure
!                          over water is used.
!                          UNITS:      Kelvin, K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Min_Pressure:      Pressure value below which the saturation
!                          mixing ratio is not calculated. The default, and
!                          absolute, minimum value used in this routine is
!                          50hPa. Saturation mixing ratios at pressures
!                          less than the minimum pressure are set to zero.
!                          This is because at pressures less than 50mb, the
!                          saturation vapour pressure, which is based only on
!                          temperature, can exceed the total air pressure.
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! PROCEDURE:
!       The saturation mixing ratio can be defined as:
!
!                rho_ws
!          ws = --------     .....(1)
!                rho_d
!
!       where rho_ws = the partial density of water vapour required to
!                      saturate air with respect to water at a Temperature, T
!             rho_d  = the partial density of dry air.
!
!       Equation (1) can be rewritten as:
!
!                   es
!               ---------
!                R_w . T
!         ws = ------------
!                p - es
!               ---------
!                R_d . T
!
!               R_d       es
!            = ----- . --------
!               R_w     p - es
!
!               M_w       es
!            = ----- . --------     .....(2)
!               M_d     p - es
!
!       where M_w = molecular weight of water
!             M_d = molecular weight of dry air
!             es  = water vapor partial pressure
!             p   = total air pressure
!             R_d = gas constant for dry air
!             R_w = gas constant for water vapor
!
!       The units of equation (2) are:
!
!               g     hPa
!         ws = --- . -----
!               g     hPa
!
!                      g
!            = 1000.0 ----
!                      kg
!
!       A factor of 1000 is used to return values in units of g/kg.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Saturation_Mixing_Ratio( &
    Pressure       , &  ! Input
    Temperature    , &  ! Input
    Mixing_Ratio   , &  ! Output
    Ice_Temperature, &  ! Optional Input
    Min_Pressure     )  ! Optional Input
    ! Arguments
    REAL(fp),           INTENT(IN)  :: Pressure
    REAL(fp),           INTENT(IN)  :: Temperature
    REAL(fp),           INTENT(OUT) :: Mixing_Ratio
    REAL(fp), OPTIONAL, INTENT(IN)  :: Ice_Temperature
    REAL(fp), OPTIONAL, INTENT(IN)  :: Min_Pressure
    ! Local variables
    REAL(fp) :: Pmin, Tmin, Tice
    REAL(fp) :: svp
    REAL(fp) :: dp

    ! Setup
    ! ...Check optional arguments
    IF ( PRESENT(Min_Pressure) ) THEN
      Pmin = MAX(Min_Pressure, MIN_SMR_PRESSURE)
    ELSE
      Pmin = MIN_SMR_PRESSURE
    END IF
    IF ( PRESENT(Ice_Temperature) ) THEN
      Tice = Ice_Temperature
      Tmin = MIN_SVPI_TEMPERATURE
    ELSE
      Tice = ZERO
      Tmin = MIN_SVPW_TEMPERATURE
    END IF
    ! ...Check input
    IF ( Pressure < Pmin .OR. Temperature < Tmin ) THEN
      Mixing_Ratio = ZERO
      RETURN
    ENDIF


    ! Calculate saturation vapor pressure
    IF ( Temperature > Tice ) THEN
      CALL SVP_Water( Temperature, svp )
    ELSE
      CALL SVP_Ice( Temperature, svp )
    END IF


    ! Calculate saturation mixing ratio only if the
    ! total pressure is greater than the saturation
    ! vapor pressure.
    dp = Pressure - svp
    IF ( dp > ZERO ) THEN
      Mixing_Ratio = KG_TO_G * EPS * svp / dp
    ELSE
      Mixing_Ratio = ZERO
    END IF

  END SUBROUTINE Saturation_Mixing_Ratio


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       Saturation_Mixing_Ratio_TL
!
! PURPOSE:
!       Tangent-linear form of elemental subroutine to calculate the saturation
!       mixing ratio for a given pressure and temperature
!
! CALLING SEQUENCE:
!       CALL Saturation_Mixing_Ratio_TL( Pressure                         , &  ! FWD Input
!                                        Temperature                      , &  ! FWD Input
!                                        Pressure_TL                      , &  ! TL  Input
!                                        Temperature_TL                   , &  ! TL  Input
!                                        Mixing_Ratio_TL                  , &  ! TL  Output
!                                        Ice_Temperature = Ice_Temperature, &  ! Optional input
!                                        Min_Pressure    = Min_Pressure     )  ! Optional input
!
! INPUTS:
!       Pressure:          Total atmospheric pressure.
!                          Valid pressures are > 50hPa.
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar or any rank.
!                          ATTRIBUTES: INTENT(IN)
!
!       Temperature:       Atmospheric Temperature.
!                          Valid temperature ranges for saturation vapor
!                          pressure calculation are:
!                            Over ice:   183K - 273K (-90C - 0C).
!                            Over water: 188K - 343K (-85C - +70C).
!                          UNITS:      Kelvin, K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Pressure
!                          ATTRIBUTES: INTENT(IN)
!
!       Pressure_TL:       Tangent-linear atmospheric pressure.
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Pressure
!                          ATTRIBUTES: INTENT(IN)
!
!       Temperature_TL:    Tangent-linear atmospheric temperature.
!                          UNITS:      Kelvin, K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Pressure
!                          ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Mixing_Ratio_TL:   Tangent-linear saturation mixing ratio.
!                          Value is set to zero for invalid input.
!                          UNITS:      g/kg
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Pressure
!                          ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Ice_Temperature:   Temperature below which the saturation vapor
!                          pressure over ice is used in the conversion.
!                          By default, only the saturation vapor pressure
!                          over water is used.
!                          UNITS:      Kelvin, K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Min_Pressure:      Pressure value below which the saturation
!                          mixing ratio is not calculated. The default, and
!                          absolute, minimum value used in this routine is
!                          50hPa. Saturation mixing ratios at pressures
!                          less than the minimum pressure are set to zero.
!                          This is because at pressures less than 50mb, the
!                          saturation vapour pressure, which is based only on
!                          temperature, can exceed the total air pressure.
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Saturation_Mixing_Ratio_TL( &
    Pressure       , &  ! FWD Input
    Temperature    , &  ! FWD Input
    Pressure_TL    , &  ! TL  Input
    Temperature_TL , &  ! TL  Input
    Mixing_Ratio_TL, &  ! TL  Output
    Ice_Temperature, &  ! Optional Input
    Min_Pressure     )  ! Optional Input
    ! Arguments
    REAL(fp),           INTENT(IN)  :: Pressure
    REAL(fp),           INTENT(IN)  :: Temperature
    REAL(fp),           INTENT(IN)  :: Pressure_TL
    REAL(fp),           INTENT(IN)  :: Temperature_TL
    REAL(fp),           INTENT(OUT) :: Mixing_Ratio_TL
    REAL(fp), OPTIONAL, INTENT(IN)  :: Ice_Temperature
    REAL(fp), OPTIONAL, INTENT(IN)  :: Min_Pressure
    ! Local variables
    REAL(fp) :: Pmin, Tmin, Tice
    REAL(fp) :: svp, svp_TL
    REAL(fp) :: dp, dp_TL

    ! Setup
    ! ...Check optional arguments
    IF ( PRESENT(Min_Pressure) ) THEN
      Pmin = MAX(Min_Pressure, MIN_SMR_PRESSURE)
    ELSE
      Pmin = MIN_SMR_PRESSURE
    END IF
    IF ( PRESENT(Ice_Temperature) ) THEN
      Tice = Ice_Temperature
      Tmin = MIN_SVPI_TEMPERATURE
    ELSE
      Tice = ZERO
      Tmin = MIN_SVPW_TEMPERATURE
    END IF
    ! ...Check input
    IF ( Pressure < Pmin .OR. Temperature < Tmin ) THEN
      Mixing_Ratio_TL = ZERO
      RETURN
    ENDIF


    ! Calculate saturation vapor pressure
    IF ( Temperature > Tice ) THEN
      CALL SVP_Water( Temperature, svp )
      CALL SVP_Water_TL( Temperature, Temperature_TL, svp_TL )
    ELSE
      CALL SVP_Ice( Temperature, svp )
      CALL SVP_Ice_TL( Temperature, Temperature_TL, svp_TL )
    END IF


    ! Calculate saturation mixing ratio only if the
    ! total pressure is greater than the saturation
    ! vapor pressure.
    dp = Pressure - svp
    IF ( dp > ZERO ) THEN
      dp_TL = Pressure_TL - svp_TL
      Mixing_Ratio_TL = KG_TO_G * EPS * ( (dp * svp_TL) - (svp * dp_TL) ) / dp**2
    ELSE
      Mixing_Ratio_TL = ZERO
    END IF

  END SUBROUTINE Saturation_Mixing_Ratio_TL


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       Saturation_Mixing_Ratio_AD
!
! PURPOSE:
!       Adjoint form of elemental subroutine to calculate the saturation
!       mixing ratio for a given pressure and temperature
!
! CALLING SEQUENCE:
!       CALL Saturation_Mixing_Ratio_AD( Pressure                         , &  ! FWD Input
!                                        Temperature                      , &  ! FWD Input
!                                        Mixing_Ratio_AD                  , &  ! AD  Input
!                                        Pressure_AD                      , &  ! AD  Output
!                                        Temperature_AD                   , &  ! AD  Output
!                                        Ice_Temperature = Ice_Temperature, &  ! Optional input
!                                        Min_Pressure    = Min_Pressure     )  ! Optional input
!
! INPUTS:
!       Pressure:          Total atmospheric pressure.
!                          Valid pressures are > 50hPa.
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar or any rank.
!                          ATTRIBUTES: INTENT(IN)
!
!       Temperature:       Atmospheric Temperature.
!                          Valid temperature ranges for saturation vapor
!                          pressure calculation are:
!                            Over ice:   183K - 273K (-90C - 0C).
!                            Over water: 188K - 343K (-85C - +70C).
!                          UNITS:      Kelvin, K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Pressure
!                          ATTRIBUTES: INTENT(IN)
!
!       Mixing_Ratio_AD:   Adjoint saturation mixing ratio.
!                          *** SET TO ZERO ON EXIT ***
!                          UNITS:      g/kg
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Pressure
!                          ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUTS:
!       Pressure_AD:       Adjoint atmospheric pressure.
!                          *** MUST HAVE VALUE ON ENTRY ***
!                          UNITS:      (g/kg)/hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Pressure
!                          ATTRIBUTES: INTENT(IN OUT)
!
!       Temperature_AD:    Adjoint atmospheric temperature.
!                          *** MUST HAVE VALUE ON ENTRY ***
!                          UNITS:      (g/kg)/K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Pressure
!                          ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       Ice_Temperature:   Temperature below which the saturation vapor
!                          pressure over ice is used in the conversion.
!                          By default, only the saturation vapor pressure
!                          over water is used.
!                          UNITS:      Kelvin, K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Min_Pressure:      Pressure value below which the saturation
!                          mixing ratio is not calculated. The default, and
!                          absolute, minimum value used in this routine is
!                          50hPa. Saturation mixing ratios at pressures
!                          less than the minimum pressure are set to zero.
!                          This is because at pressures less than 50mb, the
!                          saturation vapour pressure, which is based only on
!                          temperature, can exceed the total air pressure.
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Saturation_Mixing_Ratio_AD( &
    Pressure       , &  ! FWD Input
    Temperature    , &  ! FWD Input
    Mixing_Ratio_AD, &  ! AD  Input
    Pressure_AD    , &  ! AD  Output
    Temperature_AD , &  ! AD  Output
    Ice_Temperature, &  ! Optional Input
    Min_Pressure     )  ! Optional Input
    ! Arguments
    REAL(fp),           INTENT(IN)     :: Pressure
    REAL(fp),           INTENT(IN)     :: Temperature
    REAL(fp),           INTENT(IN OUT) :: Mixing_Ratio_AD
    REAL(fp),           INTENT(IN OUT) :: Pressure_AD
    REAL(fp),           INTENT(IN OUT) :: Temperature_AD
    REAL(fp), OPTIONAL, INTENT(IN)     :: Ice_Temperature
    REAL(fp), OPTIONAL, INTENT(IN)     :: Min_Pressure
    ! Local variables
    REAL(fp) :: Pmin, Tmin, Tice
    REAL(fp) :: svp, svp_AD
    REAL(fp) :: dp, dp_AD
    REAL(fp) :: c

    ! Setup
    ! ...Check optional arguments
    IF ( PRESENT(Min_Pressure) ) THEN
      Pmin = MAX(Min_Pressure, MIN_SMR_PRESSURE)
    ELSE
      Pmin = MIN_SMR_PRESSURE
    END IF
    IF ( PRESENT(Ice_Temperature) ) THEN
      Tice = Ice_Temperature
      Tmin = MIN_SVPI_TEMPERATURE
    ELSE
      Tice = ZERO
      Tmin = MIN_SVPW_TEMPERATURE
    END IF
    ! ...Check input
    IF ( Pressure < Pmin .OR. Temperature < Tmin ) THEN
      Mixing_Ratio_AD = ZERO
      Pressure_AD     = ZERO
      Temperature_AD  = ZERO
      RETURN
    ENDIF


    ! Calculate saturation vapor pressure
    IF ( Temperature > Tice ) THEN
      CALL SVP_Water( Temperature, svp )
    ELSE
      CALL SVP_Ice( Temperature, svp )
    END IF


    ! Adjoint form of the saturation mixing ratio calculation
    dp = Pressure - svp
    c =  KG_TO_G * EPS / dp**2
    IF ( dp > ZERO ) THEN
      dp_AD  = -(c * svp * Mixing_Ratio_AD)
      svp_AD =  (c * dp  * Mixing_Ratio_AD) - dp_AD
      Pressure_AD = Pressure_AD + dp_AD
    END IF
    Mixing_Ratio_AD = ZERO


    ! Adjoint form of the saturation vapor pressure calculation
    IF ( Temperature > Tice ) THEN
      CALL SVP_Water_AD( Temperature, svp_AD, Temperature_AD )
    ELSE
      CALL SVP_Ice_AD( Temperature, svp_AD, Temperature_AD )
    END IF

  END SUBROUTINE Saturation_Mixing_Ratio_AD


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       T_to_Tv
!
! PURPOSE:
!       Elemental subroutine to compute the virtual temperature given the
!       temperature and water vapor mixing ratio.
!
! CALLING SEQUENCE:
!       CALL T_to_Tv( Temperature        , &  ! Input
!                     Mixing_Ratio       , &  ! Input
!                     Virtual_Temperature  )  ! Output
!
! INPUTS:
!       Temperature:               Atmospheric temperature.
!                                  Must be > 0.
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Scalar or any rank
!                                  ATTRIBUTES: INTENT(IN)
!
!       Mixing_Ratio:              Water vapor mass mixing ratio.
!                                  Must be > 0.
!                                  UNITS:      g/kg
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Same as Temperature
!                                  ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Virtual_Temperature:       The virtual temperature.
!                                  Set to zero for invalid input.
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Same as Temperature
!                                  ATTRIBUTES: INTENT(OUT)
!
! PROCEDURE:
!       The virtual temperature, the temperature that dry air must have in
!       order to have the same density as moist air at the same pressure, is
!       calculated using:
!
!                  [    eps + w    ]
!         Tv = T * [ ------------- ]     .......................(1)
!                  [ eps ( 1 + w ) ]
!
!       where T   = temperature,
!             w   = water vapour mixing ratio, and
!             eps = ratio of the molecular weights of water and dry air.
!
!       An approximation to eqn.(1) is,
!
!                  [      1 - eps    ]
!         Tv = T * [ 1 + --------- w ]
!                  [        eps      ]
!
!            = T * [ 1 + ( 0.608 * w ) ]   .....................(2)
!
!       however, depending on what accuracy is required (keeping in mind that
!       water vapor measurements are probably good to 2-5%), eqn.(2) can
!       differ from (1) by around 0.06-0.08K near the surface.
!
!       If virtual temperature is used to calculate geopotential heights,
!       this difference can lead to errors of up to 0.6-0.7m.
!
!       So I took the slightly more computationally expensive road
!       and use eqn.(1).
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE T_to_Tv( &
    T, &  ! Input
    W, &  ! Input
    Tv )  ! Output
    ! Arguments
    REAL(fp), INTENT(IN)  :: T
    REAL(fp), INTENT(IN)  :: W
    REAL(fp), INTENT(OUT) :: Tv
    ! Local variables
    REAL(fp) :: mr

    ! Setup
    IF ( T < ZERO .OR. W < ZERO ) THEN
      Tv = ZERO
      RETURN
    ENDIF

    ! Calculate the virtual temperature
    mr = G_TO_KG * W
    Tv = T * (EPS + mr) / (EPS * (ONE + mr))

  END SUBROUTINE T_to_Tv


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       T_to_Tv_TL
!
! PURPOSE:
!       Tangent-linear form of elemental subroutine to compute the virtual
!       temperature given the temperature and water vapor mixing ratio.
!
! CALLING SEQUENCE:
!       CALL T_to_Tv_TL( Temperature           , &  ! FWD Input
!                        Mixing_Ratio          , &  ! FWD Input
!                        Temperature_TL        , &  ! TL  Input
!                        Mixing_Ratio_TL       , &  ! TL  Input
!                        Virtual_Temperature_TL  )  ! TL  Output
!
! INPUTS:
!       Temperature:               Atmospheric temperature.
!                                  Must be > 0.
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Scalar or any rank
!                                  ATTRIBUTES: INTENT(IN)
!
!       Mixing_Ratio:              Water vapor mass mixing ratio.
!                                  Must be > 0.
!                                  UNITS:      g/kg
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Same as Temperature
!                                  ATTRIBUTES: INTENT(IN)
!
!       Temperature_TL:            Tangent-linear atmospheric temperature.
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Same as Temperature
!                                  ATTRIBUTES: INTENT(IN)
!
!       Mixing_Ratio_TL:           Tangent-linear water vapor mass mixing ratio.
!                                  UNITS:      g/kg
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Same as Temperature
!                                  ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Virtual_Temperature_TL:    Tangent-linear virtual temperature.
!                                  Set to zero for invalid FWD input.
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Same as Temperature
!                                  ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE T_to_Tv_TL( &
    T   , &  ! FWD Input
    W   , &  ! FWD Input
    T_TL, &  ! TL  Input
    W_TL, &  ! TL  Input
    Tv_TL )  ! TL  Output
    ! Arguments
    REAL(fp), INTENT(IN)  :: T
    REAL(fp), INTENT(IN)  :: W
    REAL(fp), INTENT(IN)  :: T_TL
    REAL(fp), INTENT(IN)  :: W_TL
    REAL(fp), INTENT(OUT) :: Tv_TL
    ! Local variables
    REAL(fp) :: mr, mr_TL
    REAL(fp) :: denom, f1, f2

    ! Setup
    IF ( T < ZERO .OR. W < ZERO ) THEN
      Tv_TL = ZERO
      RETURN
    ENDIF

    ! Tangent-linear form of the virtual temperature calculation
    mr    = G_TO_KG * W
    mr_TL = G_TO_KG * W_TL

    denom = (EPS * (ONE + mr))
    f1 = (EPS + mr) / denom
    f2 = T * EPS * (ONE - EPS) / denom**2

    Tv_TL = (f1 * T_TL) + (f2 * mr_TL)

  END SUBROUTINE T_to_Tv_TL


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       T_to_Tv_AD
!
! PURPOSE:
!       Adjoint form of elemental subroutine to compute the virtual
!       temperature given the temperature and water vapor mixing ratio.
!
! CALLING SEQUENCE:
!       CALL T_to_Tv_AD( Temperature           , &  ! FWD Input
!                        Mixing_Ratio          , &  ! FWD Input
!                        Virtual_Temperature_AD, &  ! AD  Input
!                        Temperature_AD        , &  ! AD  Output
!                        Mixing_Ratio_AD         )  ! AD  Output
!
! INPUTS:
!       Temperature:               Atmospheric temperature.
!                                  Must be > 0.
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Scalar or any rank
!                                  ATTRIBUTES: INTENT(IN)
!
!       Mixing_Ratio:              Water vapor mass mixing ratio.
!                                  Must be > 0.
!                                  UNITS:      g/kg
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Same as Temperature
!                                  ATTRIBUTES: INTENT(IN)
!
!       Virtual_Temperature_AD:    Adjoint virtual temperature.
!                                  *** SET TO ZERO ON EXIT ***
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Same as Temperature
!                                  ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUTS:
!       Temperature_AD:            Adjoint atmospheric temperature.
!                                  *** MUST HAVE VALUE ON ENTRY ***
!                                  UNITS:      K/K
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Same as Temperature
!                                  ATTRIBUTES: INTENT(IN OUT)
!
!       Mixing_Ratio_AD:           Adjoint water vapor mass mixing ratio.
!                                  *** MUST HAVE VALUE ON ENTRY ***
!                                  UNITS:      K/(g/kg)
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Same as Temperature
!                                  ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE T_to_Tv_AD( &
    T    , &  ! FWD Input
    W    , &  ! FWD Input
    Tv_AD, &  ! AD  Input
    T_AD , &  ! AD  Output
    W_AD   )  ! AD  Output
    ! Arguments
    REAL(fp), INTENT(IN)     :: T
    REAL(fp), INTENT(IN)     :: W
    REAL(fp), INTENT(IN OUT) :: Tv_AD
    REAL(fp), INTENT(IN OUT) :: T_AD
    REAL(fp), INTENT(IN OUT) :: W_AD
    ! Local variables
    REAL(fp) :: mr, mr_AD
    REAL(fp) :: denom, f1, f2

    ! Setup
    IF ( T < ZERO .OR. W < ZERO ) THEN
      Tv_AD = ZERO
      T_AD  = ZERO
      W_AD  = ZERO
      RETURN
    ENDIF

    ! Adjoint form of the virtual temperature calculation
    ! ...Forward calculations
    mr    = G_TO_KG * W
    denom = (EPS * (ONE + mr))
    f1 = (EPS + mr) / denom
    f2 = T * EPS * (ONE - EPS) / denom**2
    ! ...Adjoint calcs.
    mr_AD =         (f2 * Tv_AD)
    T_AD  = T_AD  + (f1 * Tv_AD)
    Tv_AD = ZERO
    w_AD  = w_AD + (G_TO_KG * mr_AD)

  END SUBROUTINE T_to_Tv_AD


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       Tv_to_T
!
! PURPOSE:
!       Elemental subroutine to compute the temperature given the
!       virtual temperature and water vapor mixing ratio.
!
! CALLING SEQUENCE:
!       CALL Tv_to_T( Virtual_Temperature, &  ! Input
!                     Mixing_Ratio       , &  ! Input
!                     Temperature          )  ! Output
!
! INPUTS:
!       Virtual_Temperature:       The virtual temperature.
!                                  Must be > 0.
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Scalar or any rank
!                                  ATTRIBUTES: INTENT(IN)
!
!       Mixing_Ratio:              Water vapor mass mixing ratio.
!                                  Must be > 0.
!                                  UNITS:      g/kg
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Same as Virtual_Temperature
!                                  ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Temperature:               Atmospheric temperature.
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Same as Virtual_Temperature
!                                  ATTRIBUTES: INTENT(OUT)
!
! PROCEDURE:
!       The temperature, T, can be computed from the virtual temperature using:
!
!                  [ eps ( 1 + w ) ]
!         T = Tv * [ ------------- ]     .......................(1)
!                  [    eps + w    ]
!
!       where Tv  = virtual temperature,
!             w   = water vapour mixing ratio, and
!             eps = ratio of the molecular weights of water and dry air.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Tv_to_T( &
    Tv, &  ! Input
    W , &  ! Input
    T   )  ! Output
    ! Arguments
    REAL(fp), INTENT(IN)  :: Tv
    REAL(fp), INTENT(IN)  :: W
    REAL(fp), INTENT(OUT) :: T
    ! Local variables
    REAL(fp) :: mr

    ! Setup
    IF ( Tv < ZERO .OR. W < ZERO ) THEN
      T = ZERO
      RETURN
    ENDIF

    ! Calculate the virtual temperature
    mr = G_TO_KG * W
    T = Tv * (EPS * (ONE + mr)) / (EPS + mr)

  END SUBROUTINE Tv_to_T


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       Tv_to_T_TL
!
! PURPOSE:
!       Tangent-linear for of elemental subroutine to compute the temperature
!       given the virtual temperature and water vapor mixing ratio.
!
! CALLING SEQUENCE:
!       CALL Tv_to_T_TL( Virtual_Temperature   , &  ! FWD Input
!                        Mixing_Ratio          , &  ! FWD Input
!                        Virtual_Temperature_TL, &  ! TL  Input
!                        Mixing_Ratio_TL       , &  ! TL  Input
!                        Temperature_TL          )  ! TL  Output
!
! INPUTS:
!       Virtual_Temperature:       The virtual temperature.
!                                  Must be > 0.
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Scalar or any rank
!                                  ATTRIBUTES: INTENT(IN)
!
!       Mixing_Ratio:              Water vapor mass mixing ratio.
!                                  Must be > 0.
!                                  UNITS:      g/kg
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Same as Virtual_Temperature
!                                  ATTRIBUTES: INTENT(IN)
!
!       Virtual_Temperature_TL:    Tangent-linear virtual temperature.
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Same as Virtual_Temperature
!                                  ATTRIBUTES: INTENT(IN)
!
!       Mixing_Ratio_TL:           Tangent-linear water vapor mass mixing ratio.
!                                  UNITS:      g/kg
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Same as Virtual_Temperature
!                                  ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       TemperatureTL:             Tangent-linear atmospheric temperature.
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Same as Virtual_Temperature
!                                  ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Tv_to_T_TL( &
    Tv   , &  ! FWD Input
    W    , &  ! FWD Input
    Tv_TL, &  ! TL  Input
    W_TL , &  ! TL  Input
    T_TL   )  ! TL  Output
    ! Arguments
    REAL(fp), INTENT(IN)  :: Tv
    REAL(fp), INTENT(IN)  :: W
    REAL(fp), INTENT(IN)  :: Tv_TL
    REAL(fp), INTENT(IN)  :: W_TL
    REAL(fp), INTENT(OUT) :: T_TL
    ! Local variables
    REAL(fp) :: mr, mr_TL
    REAL(fp) :: denom, f1, f2

    ! Setup
    IF ( Tv < ZERO .OR. W < ZERO ) THEN
      T_TL = ZERO
      RETURN
    ENDIF

    ! Tangent-linear form of the Tv->T conversion
    mr    = G_TO_KG * W
    mr_TL = G_TO_KG * W_TL

    denom = EPS + mr
    f1 = EPS * (ONE + mr) / denom
    f2 = Tv * EPS * (EPS - ONE) / denom**2

    T_TL = (f1 * Tv_TL) + (f2 * mr_TL)

  END SUBROUTINE Tv_to_T_TL


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       Tv_to_T_AD
!
! PURPOSE:
!       Adjoint form of elemental subroutine to compute the temperature
!       given the virtual temperature and water vapor mixing ratio.
!
! CALLING SEQUENCE:
!       CALL Tv_to_T_AD( Virtual_Temperature   , &  ! FWD Input
!                        Mixing_Ratio          , &  ! FWD Input
!                        Temperature_AD        , &  ! AD  Input
!                        Virtual_Temperature_AD, &  ! AD  Output
!                        Mixing_Ratio_AD         )  ! AD  Output
!
! INPUTS:
!       Virtual_Temperature:       Virtual temperature.
!                                  Must be > 0.
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Scalar or any rank
!                                  ATTRIBUTES: INTENT(IN)
!
!       Mixing_Ratio:              Water vapor mass mixing ratio.
!                                  Must be > 0.
!                                  UNITS:      g/kg
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Same as Virtual_Temperature
!                                  ATTRIBUTES: INTENT(IN)
!
!       Temperature_AD:            Adjoint atmospheric temperature.
!                                  *** SET TO ZERO ON EXIT ***
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Same as Virtual_Temperature
!                                  ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUTS:
!       Virtual_Temperature_AD:    Adjoint virtual temperature.
!                                  *** MUST HAVE VALUE ON ENTRY ***
!                                  UNITS:      K/K
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Same as Virtual_Temperature
!                                  ATTRIBUTES: INTENT(IN OUT)
!
!       Mixing_Ratio_AD:           Adjoint water vapor mass mixing ratio.
!                                  *** MUST HAVE VALUE ON ENTRY ***
!                                  UNITS:      K/(g/kg)
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Same as Virtual_Temperature
!                                  ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Tv_to_T_AD( &
    Tv   , &  ! FWD Input
    W    , &  ! FWD Input
    T_AD , &  ! AD  Input
    Tv_AD, &  ! AD  Output
    W_AD   )  ! AD  Output
    ! Arguments
    REAL(fp), INTENT(IN)     :: Tv
    REAL(fp), INTENT(IN)     :: W
    REAL(fp), INTENT(IN OUT) :: Tv_AD
    REAL(fp), INTENT(IN OUT) :: W_AD
    REAL(fp), INTENT(IN OUT) :: T_AD
    ! Local variables
    REAL(fp) :: mr, mr_AD
    REAL(fp) :: denom, f1, f2

    ! Setup
    IF ( Tv < ZERO .OR. W < ZERO ) THEN
      Tv_AD = ZERO
      W_AD  = ZERO
      T_AD  = ZERO
      RETURN
    ENDIF

    ! Adjoint form of the Tv->T conversion
    ! ...Forward calculations
    mr    = G_TO_KG * W
    denom = EPS + mr
    f1 = EPS * (ONE + mr) / denom
    f2 = Tv * EPS * (EPS - ONE) / denom**2
    ! ...Adjoint calcs.
    mr_AD =         (f2 * T_AD)
    Tv_AD = Tv_AD  + (f1 * T_AD)
    T_AD  = ZERO
    w_AD  = w_AD + (G_TO_KG * mr_AD)

  END SUBROUTINE Tv_to_T_AD


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       T_to_Theta
!
! PURPOSE:
!       Elemental subroutine to calculate the potential temperature given the
!       temperature and pressure.
!
! CALLING SEQUENCE:
!       CALL T_to_Theta( Temperature, &  ! Input
!                        Pressure   , &  ! Input
!                        Theta        )  ! Output
!
! INPUTS:
!       Temperature:       Atmospheric temperature.
!                          Must be > 0.
!                          UNITS:      Kelvin, K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar or any rank
!                          ATTRIBUTES: INTENT(IN)
!
!       Pressure:          Atmospheric pressure
!                          Must be > 0.
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Temperature
!                          ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Theta:             The potential temperature.
!                          Set to zero for invalid input
!                          UNITS:      Kelvin, K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Temperature
!                          ATTRIBUTES: INTENT(OUT)
!
! PROCEDURE:
!       The potential temperature of a parcel of air is that temperature
!       the parcel would have if it were expanded or compressed adiabatically
!       to some reference pressure.
!
!       The conversion is given by Poisson's equation:
!
!                             R/Cp
!                     [  P0  ]
!         Theta = T * [ ---- ]           .......................(1)
!                     [  p   ]
!
!       where T   = temperature,
!             p   = pressure,
!             P0  = standard pressure
!             R   = gas constant,
!             Cp  = specific heat of gas at constant pressure.
!
!       This routine uses the standard atmosphere as the reference pressure,
!       R for dry air, and Cp for an ideal diatomic gas:
!
!               7      R0
!         Cp = --- . -------
!               2     MWair
!
!       where R0    = universal gas constant
!             MWair = molecular weight of dry air
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE T_to_Theta( &
    T    , &  ! Input
    P    , &  ! Input
    Theta  )  ! Output
    ! Arguments
    REAL(fp), INTENT(IN)  :: T
    REAL(fp), INTENT(IN)  :: P
    REAL(fp), INTENT(OUT) :: Theta
    ! Local variables
    REAL(fp) :: P_term

    ! Setup
    IF ( T < ZERO .OR. P < ZERO ) THEN
      Theta = ZERO
      RETURN
    ENDIF

    ! Calculate the potential temperature
    P_term = (PSTD / P)**EXPONENT_TERM
    Theta = T * P_term

  END SUBROUTINE T_to_Theta


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       T_to_Theta_TL
!
! PURPOSE:
!       Tangent-linear form of elemental subroutine to calculate the potential
!       temperature given the temperature and pressure.
!
! CALLING SEQUENCE:
!       CALL T_to_Theta_TL( Temperature   , &  ! Input
!                           Pressure      , &  ! Input
!                           Temperature_TL, &  ! Input
!                           Pressure_TL   , &  ! Input
!                           Theta_TL        )  ! Output
!
! INPUTS:
!       Temperature:       Atmospheric temperature.
!                          Must be > 0.
!                          UNITS:      Kelvin, K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar or any rank
!                          ATTRIBUTES: INTENT(IN)
!
!       Pressure:          Atmospheric pressure
!                          Must be > 0.
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Temperature
!                          ATTRIBUTES: INTENT(IN)
!
!       Temperature_TL:    Tangent-linear atmospheric temperature.
!                          UNITS:      Kelvin, K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Temperature
!                          ATTRIBUTES: INTENT(IN)
!
!       Pressure_TL:       Tangent-linear atmospheric pressure
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Temperature
!                          ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Theta_TL:          Tangent-linear potential temperature.
!                          Set to zero for invalid input
!                          UNITS:      Kelvin, K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Temperature
!                          ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE T_to_Theta_TL( &
    T       , &  ! FWD Input
    P       , &  ! FWD Input
    T_TL    , &  ! TL  Input
    P_TL    , &  ! TL  Input
    Theta_TL  )  ! TL  Output
    ! Arguments
    REAL(fp), INTENT(IN)  :: T
    REAL(fp), INTENT(IN)  :: P
    REAL(fp), INTENT(IN)  :: T_TL
    REAL(fp), INTENT(IN)  :: P_TL
    REAL(fp), INTENT(OUT) :: Theta_TL
    ! Local variables
    REAL(fp) :: f1, f2

    ! Setup
    IF ( T < ZERO .OR. P < ZERO ) THEN
      Theta_TL = ZERO
      RETURN
    ENDIF

    ! Tangent-linear form of T->Theta conversion
    f1 = (PSTD / P)**EXPONENT_TERM
    f2 = f1 * EXPONENT_TERM * T / P

    Theta_TL = (f1 * T_TL) - (f2 * P_TL)

  END SUBROUTINE T_to_Theta_TL


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       T_to_Theta_AD
!
! PURPOSE:
!       Adjoint form of elemental subroutine to compute the potential temperature
!       given the temperature and pressure
!
! CALLING SEQUENCE:
!       CALL T_to_Theta_AD( Temperature             , &  ! FWD Input
!                           Pressure                , &  ! FWD Input
!                           Potential_Temperature_AD, &  ! AD  Input
!                           Temperature_AD          , &  ! AD  Output
!                           Pressure_AD               )  ! AD  Output
!
! INPUTS:
!      Temperature:                Atmospheric temperature.
!                                  Must be > 0.
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Scalar or any rank
!                                  ATTRIBUTES: INTENT(IN)
!
!       Pressure:                  Atmospheric pressure
!                                  Must be > 0.
!                                  UNITS:      hPa
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Same as Temperature
!                                  ATTRIBUTES: INTENT(IN)
!
!       Potential_Temperature_AD:  Adjoint potential temperature.
!                                  *** SET TO ZERO ON EXIT ***
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Same as Temperature
!                                  ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUTS:
!       Temperature_AD:            Adjoint atmospheric temperature.
!                                  *** MUST HAVE VALUE ON ENTRY ***
!                                  UNITS:      K/K
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Same as Temperature
!                                  ATTRIBUTES: INTENT(IN OUT)
!
!       Pressure_AD:               Adjoint atmospheric pressure.
!                                  *** MUST HAVE VALUE ON ENTRY ***
!                                  UNITS:      K/h_Pa
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Same as Temperature
!                                  ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE T_to_Theta_AD( &
    T       , &  ! FWD Input
    P       , &  ! FWD Input
    Theta_AD, &  ! AD  Input
    T_AD    , &  ! AD  Output
    P_AD      )  ! AD  Output
    ! Arguments
    REAL(fp), INTENT(IN)     :: T
    REAL(fp), INTENT(IN)     :: P
    REAL(fp), INTENT(IN OUT) :: Theta_AD
    REAL(fp), INTENT(IN OUT) :: T_AD
    REAL(fp), INTENT(IN OUT) :: P_AD
    ! Local variables
    REAL(fp) :: f1, f2

    ! Setup
    IF ( T < ZERO .OR. P < ZERO ) THEN
      Theta_AD = ZERO
      T_AD     = ZERO
      P_AD     = ZERO
      RETURN
    ENDIF

    ! Adjoint form of T->Theta conversion
    ! ...Forward calculations
    f1 = (PSTD / P)**EXPONENT_TERM
    f2 = f1 * EXPONENT_TERM * T / P
    ! ...Adjoint calculations
    P_AD = P_AD - (f2 * Theta_AD)
    T_AD = T_AD + (f1 * Theta_AD)
    Theta_AD = ZERO

  END SUBROUTINE T_to_Theta_AD


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       Theta_to_T
!
! PURPOSE:
!       Elemental subroutine to calculate the temperature given the potential
!       temperature and pressure.
!
! CALLING SEQUENCE:
!       CALL Theta_to_T( Theta      , &  ! Input
!                        Pressure   , &  ! Input
!                        Temperature  )  ! Output
!
! INPUTS:
!       Theta:             Potential temperature.
!                          Must be > 0.
!                          UNITS:      Kelvin, K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar or any rank
!                          ATTRIBUTES: INTENT(IN)
!
!       Pressure:          Atmospheric pressure
!                          Must be > 0.
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Theta
!                          ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Temperature:       The atmospheric temperature.
!                          Set to zero for invalid input
!                          UNITS:      Kelvin, K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Theta
!                          ATTRIBUTES: INTENT(OUT)
!
! PROCEDURE:
!       The potential temperature of a parcel of air is that temperature
!       the parcel would have if it were expanded or compressed adiabatically
!       to some reference pressure.
!
!       The conversion of potential temperature to temperature is given by
!       the inverse of Poisson's equation:
!
!                             R/Cp
!                     [  p   ]
!         T = Theta * [ ---- ]           .......................(1)
!                     [  P0  ]
!
!       where Theta = potential temperature,
!             p     = pressure,
!             P0    = standard pressure
!             R     = gas constant,
!             Cp    = specific heat of gas at constant pressure.
!
!       This routine uses the standard atmosphere as the reference pressure,
!       R for dry air, and Cp for an ideal diatomic gas:
!
!               7      R0
!         Cp = --- . -------
!               2     MWair
!
!       where R0    = universal gas constant
!             MWair = molecular weight of dry air
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Theta_to_T( &
    Theta, &  ! Input
    P    , &  ! Input
    T      )  ! Output
    ! Arguments
    REAL(fp), INTENT(IN)  :: Theta
    REAL(fp), INTENT(IN)  :: P
    REAL(fp), INTENT(OUT) :: T
    ! Local variables
    REAL(fp) :: P_term

    ! Setup
    IF ( Theta < ZERO .OR. P < ZERO ) THEN
      T = ZERO
      RETURN
    ENDIF

    ! Perform the Theta->Temperature conversion
    P_term = ( ( P / PSTD )**EXPONENT_TERM )
    T = Theta * P_term

  END SUBROUTINE Theta_to_T


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       Theta_to_T_TL
!
! PURPOSE:
!       Tangent-linear form of elemental subroutine to calculate the temperature
!       given the potential temperature and pressure.
!
! CALLING SEQUENCE:
!       CALL Theta_to_T_TL( Theta         , &  ! Input
!                           Pressure      , &  ! Input
!                           Theta_TL      , &  ! Input
!                           Pressure_TL   , &  ! Input
!                           Temperature_TL  )  ! Output
!
! INPUTS:
!       Theta:             Potential temperature.
!                          Must be > 0.
!                          UNITS:      Kelvin, K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar or any rank
!                          ATTRIBUTES: INTENT(IN)
!
!       Pressure:          Atmospheric pressure
!                          Must be > 0.
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Theta
!                          ATTRIBUTES: INTENT(IN)
!
!       Theta_TL:          Tangent-linear potential temperature.
!                          UNITS:      Kelvin, K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Theta
!                          ATTRIBUTES: INTENT(IN)
!
!       Pressure_TL:       Tangent-linear atmospheric pressure
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Theta
!                          ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Temperature_TL:    Tangent-linear atmospheric temperature.
!                          Set to zero for invalid input
!                          UNITS:      Kelvin, K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Theta
!                          ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Theta_to_T_TL( &
    Theta   , &  ! FWD Input
    P       , &  ! FWD Input
    Theta_TL, &  ! TL  Input
    P_TL    , &  ! TL  Input
    T_TL      )  ! TL  Output
    ! Arguments
    REAL(fp), INTENT(IN)  :: Theta
    REAL(fp), INTENT(IN)  :: P
    REAL(fp), INTENT(IN)  :: Theta_TL
    REAL(fp), INTENT(IN)  :: P_TL
    REAL(fp), INTENT(OUT) :: T_TL
    ! Local variables
    REAL(fp) :: f1, f2

    ! Setup
    IF ( Theta < ZERO .OR. P < ZERO ) THEN
      T_TL = ZERO
      RETURN
    ENDIF

    ! Tangent-linear form of Theta->T conversion
    f1 = (P / PSTD)**EXPONENT_TERM
    f2 = f1 * EXPONENT_TERM * Theta / P

    T_TL = (f1 * Theta_TL) + (f2 * P_TL)

  END SUBROUTINE Theta_to_T_TL


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       Theta_to_T_AD
!
! PURPOSE:
!       Adjoint form of elemental subroutine to compute the temperature
!       given the potential temperature and pressure
!
! CALLING SEQUENCE:
!       CALL Theta_to_T_AD( Potential_Temperature   , &  ! FWD Input
!                           Pressure                , &  ! FWD Input
!                           Temperature_AD          , &  ! AD  Input
!                           Potential_Temperature_AD, &  ! AD  Output
!                           Pressure_AD               )  ! AD  Output
!
! INPUTS:
!       Potential_Temperature:     Potential temperature.
!                                  Must be > 0.
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Scalar or any rank
!                                  ATTRIBUTES: INTENT(IN)
!
!       Pressure:                  Atmospheric pressure
!                                  Must be > 0.
!                                  UNITS:      hPa
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Same as Potential_Temperature
!                                  ATTRIBUTES: INTENT(IN)
!
!       Temperature_AD:            Adjoint atmospheric temperature.
!                                  *** SET TO ZERO ON EXIT ***
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Same as Potential_Temperature
!                                  ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUTS:
!       Potential_Temperature_AD:  Adjoint Potential temperature.
!                                  *** MUST HAVE VALUE ON ENTRY ***
!                                  UNITS:      K/K
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Same as Potential_Temperature
!                                  ATTRIBUTES: INTENT(IN OUT)
!
!       Pressure_AD:               Adjoint atmospheric pressure.
!                                  *** MUST HAVE VALUE ON ENTRY ***
!                                  UNITS:      K/h_Pa
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Same as Potential_Temperature
!                                  ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Theta_to_T_AD( &
    Theta   , &  ! FWD Input
    P       , &  ! FWD Input
    T_AD    , &  ! AD  Input
    Theta_AD, &  ! AD  Output
    P_AD      )  ! AD  Output
    ! Arguments
    REAL(fp), INTENT(IN)     :: Theta
    REAL(fp), INTENT(IN)     :: P
    REAL(fp), INTENT(IN OUT) :: T_AD
    REAL(fp), INTENT(IN OUT) :: Theta_AD
    REAL(fp), INTENT(IN OUT) :: P_AD
    ! Local variables
    REAL(fp) :: f1, f2

    ! Setup
    IF ( Theta < ZERO .OR. P < ZERO ) THEN
      T_AD     = ZERO
      Theta_AD = ZERO
      P_AD     = ZERO
      RETURN
    ENDIF

    ! Adjoint form of Theta->T conversion
    ! ...Forward calculations
    f1 = (P / PSTD)**EXPONENT_TERM
    f2 = f1 * EXPONENT_TERM * Theta / P
    ! ...Adjoint calculations
    P_AD     = P_AD     + (f2 * T_AD)
    Theta_AD = Theta_AD + (f1 * T_AD)
    T_AD = ZERO

  END SUBROUTINE Theta_to_T_AD

END MODULE Atmospheric_Properties
