!
! PP_MD
!
! Module containing forward, tangent-linear and adjoint subroutines
! for partial pressure to/from mass density units conversion.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 24-Sep-2009
!                       paul.vandelst@noaa.gov
!

MODULE PP_MD

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Modules used
  USE Type_Kinds, ONLY: fp
  USE Profile_Utility_Parameters, ONLY: ZERO, ONE, &
                                        MAX_N_MOLECULAR_SPECIES, &
                                        R0, MOLECULAR_WEIGHT, &
                                        PA_TO_HPA, HPA_TO_PA
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: PP_to_MD, PP_to_MD_TL, PP_to_MD_AD
  PUBLIC :: MD_to_PP, MD_to_PP_TL, MD_to_PP_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &


CONTAINS


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       PP_to_MD
!
! PURPOSE:
!       Elemental subroutine to convert gas concentrations from partial
!       pressure to mass density.
!
! CALLING SEQUENCE:
!       CALL PP_to_MD( Temperature              , &  ! Input
!                      Partial_Pressure         , &  ! Input
!                      Mass_Density             , &  ! Output
!                      Molecule_ID = Molecule_ID  )  ! Optional Input
!
! INPUTS:
!       Temperature:       Atmospheric temperature
!                          UNITS:      Kelvin, K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar or any rank
!                          ATTRIBUTES: INTENT(IN)
!
!       Partial_Pressure:  Partial pressure of gas.
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Temperature
!                          ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Mass_Density:      Mass density of gas.
!                          Set to zero for invalid input.
!                          UNITS:      g/m^3
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as input Temperature
!                          ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Molecule_ID:       HITRAN molecular designation identifying the
!                          molecule for which the concentration units
!                          conversion is required. If not specified, the
!                          default value is that for water vapor.
!                          Valid values are:
!                            1: H2O       9: SO2      17: HI       25: H2O2
!                            2: CO2      10: NO2      18: ClO      26: C2H2
!                            3: O3       11: NH3      19: OCS      27: C2H6
!                            4: N2O      12: HNO3     20: H2CO     28: PH3
!                            5: CO       13: OH       21: HOCl     29: COF2
!                            6: CH4      14: HF       22: N2       30: SF6
!                            7: O2       15: HCl      23: HCN      31: H2S
!                            8: NO       16: HBr      24: CH3Cl    32: HCOOH
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! PROCEDURE:
!       The ideal gas law is
!
!         p.V = n.R.T
!
!       where,
!         p = Pressure
!         V = volume
!         n = number of moles of the gas,
!         R = molar gas constant,
!         T = Temperature.
!
!       This can be written in terms of the number of molecules, N,
!
!                 N
!         p.V = ---- . R.T                  ..............(1)
!                NA
!
!       where NA = Avogadro's constant (number of molecules in 1 mole.)
!
!       Eqn.(1) can be recast to provide the number of molecules in the
!       volume, the number density, nd,
!
!               N     p.NA
!         nd = --- = ------  molecules/m^3  ..............(2)
!               V     R.T
!
!       The mass density is related to the number density by the simple
!       relation,
!
!                    MW
!         md = nd . ----                    ..............(3)
!                    NA
!
!       Substituting equation (2) into (3) gives,
!
!               p.MW
!         md = ------  g/m^3                ..............(4)
!               R.T
!
!       The value of p0 used in this routine is expressed in pascals (Pa) whereas
!       the input partial Pressure is expected to be hPa (more common unit). Thus
!       there is a factor of 100 to include,
!
!               100.p.MW
!         md = ----------  g/m^3
!                 R.T
!
! UNITS ANALYSIS:
!       p  :  kg.m^-1.s^-2  (Pascals)
!       MW :  g.mol^-1
!       R  :  J.mol^-1.K^-1 == kg.m^2.s^-2.mol^-1.K^-1
!       T  :  K
!
!                  kg       g      s^2.mol.K     1
!         md == ------- . ----- . ----------- . ---
!                m.s^2     mol      kg.m^2       K
!
!                 g
!            == -----
!                m^3
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 08-Nov-2004
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE PP_to_MD( &
    Temperature     , &  ! Input
    Partial_Pressure, &  ! Input
    Mass_Density    , &  ! Output
    Molecule_ID       )  ! Optional Input
    ! Arguments
    REAL(fp),          INTENT(IN)  :: Temperature
    REAL(fp),          INTENT(IN)  :: Partial_Pressure
    REAL(fp),          INTENT(OUT) :: Mass_Density
    INTEGER, OPTIONAL, INTENT(IN)  :: Molecule_ID
    ! Local variables
    INTEGER :: Id

    ! Setup
    Mass_Density = ZERO
    IF ( Temperature < ZERO .OR. Partial_Pressure < ZERO ) RETURN
    IF ( PRESENT( Molecule_ID ) ) THEN
      IF ( Molecule_ID < 1 .OR. Molecule_ID > MAX_N_MOLECULAR_SPECIES ) RETURN
      Id = Molecule_ID
    ELSE
      Id = 1  ! Default value is for water vapor
    END IF

    ! Convert partial pressure to mass density
    Mass_Density = HPA_TO_PA * Partial_Pressure * MOLECULAR_WEIGHT(Id) / (Temperature * R0)

  END SUBROUTINE PP_to_MD


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       PP_to_MD_TL
!
! PURPOSE:
!       Tangent-linear form of elemental subroutine to convert gas
!       concentrations from partial pressure to mass density.
!
! CALLING SEQUENCE:
!       CALL PP_to_MD_TL( Temperature              , &  ! FWD Input
!                         Partial_Pressure         , &  ! FWD Input
!                         Temperature_TL           , &  ! TL  Input
!                         Partial_Pressure_TL      , &  ! TL  Input
!                         Mass_Density_TL          , &  ! TL  Output
!                         Molecule_ID = Molecule_ID  )  ! Optional Input
!
! INPUTS:
!       Temperature:          Atmospheric temperature
!                             UNITS:      Kelvin, K
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN)
!
!       Partial_Pressure:     Partial pressure of gas.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Temperature
!                             ATTRIBUTES: INTENT(IN)
!
!       Temperature_TL:       Tangent-linear atmospheric temperature
!                             UNITS:      Kelvin, K
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Temperature
!                             ATTRIBUTES: INTENT(IN)
!
!       Partial_Pressure_TL:  Tangent-linear partial pressure of gas.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Temperature
!                             ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Mass_Density_TL:      Tangent-linear mass density of gas.
!                             Set to zero for invalid input.
!                             UNITS:      g/m^3
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Temperature
!                             ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Molecule_ID:          HITRAN molecular designation identifying the
!                             molecule for which the concentration units
!                             conversion is required. If not specified, the
!                             default value is that for water vapor.
!                             Valid values are:
!                               1: H2O       9: SO2      17: HI       25: H2O2
!                               2: CO2      10: NO2      18: ClO      26: C2H2
!                               3: O3       11: NH3      19: OCS      27: C2H6
!                               4: N2O      12: HNO3     20: H2CO     28: PH3
!                               5: CO       13: OH       21: HOCl     29: COF2
!                               6: CH4      14: HF       22: N2       30: SF6
!                               7: O2       15: HCl      23: HCN      31: H2S
!                               8: NO       16: HBr      24: CH3Cl    32: HCOOH
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 24-Sep-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE PP_to_MD_TL( &
    Temperature        , &  ! FWD Input
    Partial_Pressure   , &  ! FWD Input
    Temperature_TL     , &  ! TL  Input
    Partial_Pressure_TL, &  ! TL  Input
    Mass_Density_TL    , &  ! TL  Output
    Molecule_ID          )  ! Optional Input
    ! Arguments
    REAL(fp),          INTENT(IN)  :: Temperature
    REAL(fp),          INTENT(IN)  :: Partial_Pressure
    REAL(fp),          INTENT(IN)  :: Temperature_TL
    REAL(fp),          INTENT(IN)  :: Partial_Pressure_TL
    REAL(fp),          INTENT(OUT) :: Mass_Density_TL
    INTEGER, OPTIONAL, INTENT(IN)  :: Molecule_ID
    ! Local variables
    INTEGER :: Id
    REAL(fp) :: c

    ! Setup
    Mass_Density_TL = ZERO
    IF ( Temperature < ZERO .OR. Partial_Pressure < ZERO ) RETURN
    IF ( PRESENT( Molecule_ID ) ) THEN
      IF ( Molecule_ID < 1 .OR. Molecule_ID > MAX_N_MOLECULAR_SPECIES ) RETURN
      Id = Molecule_ID
    ELSE
      Id = 1  ! Default value is for water vapor
    END IF

    ! Tangent-linear form of partial pressure to mass density conversion
    c = HPA_TO_PA * MOLECULAR_WEIGHT(Id) / R0
    Mass_Density_TL = c * ((Temperature * Partial_Pressure_TL) - &
                           (Partial_Pressure * Temperature_TL)) / Temperature**2

  END SUBROUTINE PP_to_MD_TL


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       PP_to_MD_AD
!
! PURPOSE:
!       Adjoint form of elemental subroutine to convert gas
!       concentrations from partial pressure to mass density.
!
! CALLING SEQUENCE:
!       CALL PP_to_MD_AD( Temperature              , &  ! FWD Input
!                         Partial_Pressure         , &  ! FWD Input
!                         Mass_Density_AD          , &  ! AD  Input
!                         Temperature_AD           , &  ! AD  Output
!                         Partial_Pressure_AD      , &  ! AD  Output
!                         Molecule_ID = Molecule_ID  )  ! Optional Input
!
! INPUTS:
!       Temperature:          Atmospheric temperature
!                             UNITS:      Kelvin, K
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN)
!
!       Partial_Pressure:     Partial pressure of gas.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Temperature
!                             ATTRIBUTES: INTENT(IN)
!
!       Mass_Density_AD:      Adjoint mass density of gas.
!                             *** SET TO ZERO ON EXIT ***
!                             UNITS:      g/m^3
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Temperature
!                             ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUTS:
!       Temperature_AD:       Adjoint atmospheric temperature
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      Kelvin, K
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Temperature
!                             ATTRIBUTES: INTENT(IN OUT)
!
!       Partial_Pressure_AD:  Adjoint partial pressure of gas.
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Temperature
!                             ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       Molecule_ID:          HITRAN molecular designation identifying the
!                             molecule for which the concentration units
!                             conversion is required. If not specified, the
!                             default value is that for water vapor.
!                             Valid values are:
!                               1: H2O       9: SO2      17: HI       25: H2O2
!                               2: CO2      10: NO2      18: ClO      26: C2H2
!                               3: O3       11: NH3      19: OCS      27: C2H6
!                               4: N2O      12: HNO3     20: H2CO     28: PH3
!                               5: CO       13: OH       21: HOCl     29: COF2
!                               6: CH4      14: HF       22: N2       30: SF6
!                               7: O2       15: HCl      23: HCN      31: H2S
!                               8: NO       16: HBr      24: CH3Cl    32: HCOOH
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 24-Sep-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE PP_to_MD_AD( &
    Temperature        , &  ! FWD Input
    Partial_Pressure   , &  ! FWD Input
    Mass_Density_AD    , &  ! AD  Input
    Temperature_AD     , &  ! AD  Output
    Partial_Pressure_AD, &  ! AD  Output
    Molecule_ID          )  ! Optional Input
    ! Arguments
    REAL(fp),          INTENT(IN)     :: Temperature
    REAL(fp),          INTENT(IN)     :: Partial_Pressure
    REAL(fp),          INTENT(IN OUT) :: Mass_Density_AD
    REAL(fp),          INTENT(IN OUT) :: Temperature_AD
    REAL(fp),          INTENT(IN OUT) :: Partial_Pressure_AD
    INTEGER, OPTIONAL, INTENT(IN)     :: Molecule_ID
    ! Local variables
    INTEGER :: Id
    REAL(fp) :: c

    ! Setup
    IF ( Temperature < ZERO .OR. Partial_Pressure < ZERO ) THEN
      Mass_Density_AD     = ZERO
      Temperature_AD      = ZERO
      Partial_Pressure_AD = ZERO
      RETURN
    END IF
    IF ( PRESENT( Molecule_ID ) ) THEN
      IF ( Molecule_ID < 1 .OR. Molecule_ID > MAX_N_MOLECULAR_SPECIES ) THEN
        Mass_Density_AD     = ZERO
        Temperature_AD      = ZERO
        Partial_Pressure_AD = ZERO
        RETURN
      END IF
      Id = Molecule_ID
    ELSE
      Id = 1  ! Default value is for water vapor
    END IF

    ! Adjoint form of partial pressure to mass density conversion
    c = HPA_TO_PA * MOLECULAR_WEIGHT(Id) / R0
    Temperature_AD = Temperature_AD - (c * Partial_Pressure * Mass_Density_AD / Temperature**2)
    Partial_Pressure_AD = Partial_Pressure_AD + (c * Mass_Density_AD / Temperature)
    Mass_Density_AD = ZERO

  END SUBROUTINE PP_to_MD_AD


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       MD_to_PP
!
! PURPOSE:
!       Elemental subroutine to convert gas concentration mass density
!       to partial pressure.
!
! CALLING SEQUENCE:
!       CALL MD_to_PP( Temperature              , &  ! Input
!                      Mass_Density             , &  ! Input
!                      Partial_Pressure         , &  ! Output
!                      Molecule_ID = Molecule_ID  )  ! Optional Input
!
! INPUTS:
!       Temperature:       Atmospheric temperature
!                          UNITS:      Kelvin, K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar or any rank
!                          ATTRIBUTES: INTENT(IN)
!
!       Mass_Density:      Mass density of gas.
!                          UNITS:      g/m^3
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Temperature
!                          ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Partial_Pressure:  Partial pressure of gas.
!                          Set to zero for invalid input.
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as input Temperature
!                          ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Molecule_ID:       HITRAN molecular designation identifying the
!                          molecule for which the concentration units
!                          conversion is required. If not specified, the
!                          default value is that for water vapor.
!                          Valid values are:
!                            1: H2O       9: SO2      17: HI       25: H2O2
!                            2: CO2      10: NO2      18: ClO      26: C2H2
!                            3: O3       11: NH3      19: OCS      27: C2H6
!                            4: N2O      12: HNO3     20: H2CO     28: PH3
!                            5: CO       13: OH       21: HOCl     29: COF2
!                            6: CH4      14: HF       22: N2       30: SF6
!                            7: O2       15: HCl      23: HCN      31: H2S
!                            8: NO       16: HBr      24: CH3Cl    32: HCOOH
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! PROCEDURE:
!       The ideal gas law is
!
!         p.V = n.R.T
!
!       where,
!         p = Pressure
!         V = volume
!         n = number of moles of the gas,
!         R = molar gas constant,
!         T = Temperature.
!
!       This can be written in terms of the number of molecules, N,
!
!                 N
!         p.V = ---- . R.T                  ..............(1)
!                NA
!
!       where NA = Avogadro's constant (number of molecules in 1 mole.)
!
!       Eqn.(1) can be recast to provide the number of molecules in the
!       volume, the number density, nd,
!
!               N     p.NA
!         nd = --- = ------  molecules/m^3  ..............(2)
!               V     R.T
!
!       The mass density is related to the number density by the simple
!       relation,
!
!                    MW
!         md = nd . ----                    ..............(3)
!                    NA
!
!       Substituting equation (2) into (3) gives,
!
!               p.MW
!         md = ------  g/m^3                ..............(4)
!               R.T
!
!       and inverting equation (4) gives,
!
!              md.R.T
!         p = -------- Pa
!                MW
!
!       The pressure result above is determined in units of Pascals, so there
!       is a factor of 100 to include to return the pressure in units of
!       hectoPascals,
!
!              md.R.T
!         p = -------- hPa
!              100.MW
!
! UNITS ANALYSIS:
!       md :  g.m^-3
!       R  :  J.mol^-1.K^-1 == kg.m^2.s^-2.mol^-1.K^-1
!       T  :  K
!       MW :  g.mol^-1
!
!                g        kg.m^2          mol
!         p == ----- . ----------- . K . -----
!               m^3     s^2.mol.K          g
!
!                 kg
!           == -------
!               m.s^2
!
!               kg.m.s^-2      F
!           == ----------- == --- == pressure.
!                  m^2         A
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 08-Nov-2004
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE MD_to_PP( &
    Temperature     , &  ! Input
    Mass_Density    , &  ! Input
    Partial_Pressure, &  ! Output
    Molecule_ID       )  ! Optional Input
    ! Arguments
    REAL(fp),          INTENT(IN)  :: Temperature
    REAL(fp),          INTENT(IN)  :: Mass_Density
    REAL(fp),          INTENT(OUT) :: Partial_Pressure
    INTEGER, OPTIONAL, INTENT(IN)  :: Molecule_ID
    ! Local variables
    INTEGER :: Id

    ! Setup
    Partial_Pressure = ZERO
    IF ( Temperature < ZERO .OR. Mass_Density < ZERO ) RETURN
    IF ( PRESENT( Molecule_ID ) ) THEN
      IF ( Molecule_ID < 1 .OR. Molecule_ID > MAX_N_MOLECULAR_SPECIES ) RETURN
      Id = Molecule_ID
    ELSE
      Id = 1  ! Default value is for water vapor
    END IF

    ! Convert mass density to partial pressure
    Partial_Pressure = PA_TO_HPA * Mass_Density * R0 * Temperature / MOLECULAR_WEIGHT(Id)

  END SUBROUTINE MD_to_PP


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       MD_to_PP_TL
!
! PURPOSE:
!       Tangent-linear form of elemental subroutine to convert gas
!       concentration mass density to partial pressure.
!
! CALLING SEQUENCE:
!       CALL MD_to_PP_TL( Temperature              , &  ! FWD Input
!                         Mass_Density             , &  ! FWD Input
!                         Temperature_TL           , &  ! TL  Input
!                         Mass_Density_TL          , &  ! TL  Input
!                         Partial_Pressure_TL      , &  ! TL  Output
!                         Molecule_ID = Molecule_ID  )  ! Optional Input
!
! INPUTS:
!       Temperature:          Atmospheric temperature
!                             UNITS:      Kelvin, K
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN)
!
!       Mass_Density:         Mass density of gas.
!                             UNITS:      g/m^3
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Temperature
!                             ATTRIBUTES: INTENT(IN)
!
!       Temperature_TL:       Tangent-linear atmospheric temperature
!                             UNITS:      Kelvin, K
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Temperature
!                             ATTRIBUTES: INTENT(IN)
!
!       Mass_Density_TL:      Tangent-linear mass density of gas.
!                             UNITS:      g/m^3
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Temperature
!                             ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Partial_Pressure_TL:  Tangent-linear partial pressure of gas.
!                             Set to zero for invalid input.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Temperature
!                             ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Molecule_ID:          HITRAN molecular designation identifying the
!                             molecule for which the concentration units
!                             conversion is required. If not specified, the
!                             default value is that for water vapor.
!                             Valid values are:
!                               1: H2O       9: SO2      17: HI       25: H2O2
!                               2: CO2      10: NO2      18: ClO      26: C2H2
!                               3: O3       11: NH3      19: OCS      27: C2H6
!                               4: N2O      12: HNO3     20: H2CO     28: PH3
!                               5: CO       13: OH       21: HOCl     29: COF2
!                               6: CH4      14: HF       22: N2       30: SF6
!                               7: O2       15: HCl      23: HCN      31: H2S
!                               8: NO       16: HBr      24: CH3Cl    32: HCOOH
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 24-Sep-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE MD_to_PP_TL( &
    Temperature        , &  ! FWD Input
    Mass_Density       , &  ! FWD Input
    Temperature_TL     , &  ! TL  Input
    Mass_Density_TL    , &  ! TL  Input
    Partial_Pressure_TL, &  ! TL  Output
    Molecule_ID          )  ! Optional Input
    ! Arguments
    REAL(fp),          INTENT(IN)  :: Temperature
    REAL(fp),          INTENT(IN)  :: Mass_Density
    REAL(fp),          INTENT(IN)  :: Temperature_TL
    REAL(fp),          INTENT(IN)  :: Mass_Density_TL
    REAL(fp),          INTENT(OUT) :: Partial_Pressure_TL
    INTEGER, OPTIONAL, INTENT(IN)  :: Molecule_ID
    ! Local variables
    INTEGER :: Id
    REAL(fp) :: c

    ! Setup
    Partial_Pressure_TL = ZERO
    IF ( Temperature < ZERO .OR. Mass_Density < ZERO ) RETURN
    IF ( PRESENT( Molecule_ID ) ) THEN
      IF ( Molecule_ID < 1 .OR. Molecule_ID > MAX_N_MOLECULAR_SPECIES ) RETURN
      Id = Molecule_ID
    ELSE
      Id = 1  ! Default value is for water vapor
    END IF

    ! Tangent-linear form of mass density to partial pressure conversion
    c = PA_TO_HPA * R0 / MOLECULAR_WEIGHT(Id)
    Partial_Pressure_TL = c * ((Mass_Density * Temperature_TL) + &
                               (Temperature * Mass_Density_TL) )

  END SUBROUTINE MD_to_PP_TL


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       MD_to_PP_AD
!
! PURPOSE:
!       Adjoint form of elemental subroutine to convert gas
!       concentration mass density to partial pressure.
!
! CALLING SEQUENCE:
!       CALL MD_to_PP_AD( Temperature              , &  ! FWD Input
!                         Mass_Density             , &  ! FWD Input
!                         Partial_Pressure_AD      , &  ! AD  Input
!                         Temperature_AD           , &  ! AD  Output
!                         Mass_Density_AD          , &  ! AD  Output
!                         Molecule_ID = Molecule_ID  )  ! Optional Input
!
! INPUTS:
!       Temperature:          Atmospheric temperature
!                             UNITS:      Kelvin, K
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN)
!
!       Mass_Density:         Mass density of gas.
!                             UNITS:      g/m^3
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Temperature
!                             ATTRIBUTES: INTENT(IN)
!
!       Partial_Pressure_AD:  Adjoint partial pressure of gas.
!                             *** SET TO ZERO ON EXIT ***
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Temperature
!                             ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUTS:
!       Temperature_AD:       Adjoint atmospheric temperature
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      Kelvin, K
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Temperature
!                             ATTRIBUTES: INTENT(IN OUT)
!
!       Mass_Density_AD:      Adjoint mass density of gas.
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      g/m^3
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Temperature
!                             ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       Molecule_ID:          HITRAN molecular designation identifying the
!                             molecule for which the concentration units
!                             conversion is required. If not specified, the
!                             default value is that for water vapor.
!                             Valid values are:
!                               1: H2O       9: SO2      17: HI       25: H2O2
!                               2: CO2      10: NO2      18: ClO      26: C2H2
!                               3: O3       11: NH3      19: OCS      27: C2H6
!                               4: N2O      12: HNO3     20: H2CO     28: PH3
!                               5: CO       13: OH       21: HOCl     29: COF2
!                               6: CH4      14: HF       22: N2       30: SF6
!                               7: O2       15: HCl      23: HCN      31: H2S
!                               8: NO       16: HBr      24: CH3Cl    32: HCOOH
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 24-Sep-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE MD_to_PP_AD( &
    Temperature        , &  ! FWD Input
    Mass_Density       , &  ! FWD Input
    Partial_Pressure_AD, &  ! AD  Input
    Temperature_AD     , &  ! AD  Output
    Mass_Density_AD    , &  ! AD  Output
    Molecule_ID          )  ! Optional Input
    ! Arguments
    REAL(fp),          INTENT(IN)     :: Temperature
    REAL(fp),          INTENT(IN)     :: Mass_Density
    REAL(fp),          INTENT(IN OUT) :: Partial_Pressure_AD
    REAL(fp),          INTENT(IN OUT) :: Temperature_AD
    REAL(fp),          INTENT(IN OUT) :: Mass_Density_AD
    INTEGER, OPTIONAL, INTENT(IN)     :: Molecule_ID
    ! Local variables
    INTEGER :: Id
    REAL(fp) :: c

    ! Setup
    IF ( Temperature < ZERO .OR. Mass_Density < ZERO ) THEN
      Partial_Pressure_AD = ZERO
      Temperature_AD      = ZERO
      Mass_Density_AD     = ZERO
      RETURN
    END IF
    IF ( PRESENT( Molecule_ID ) ) THEN
      IF ( Molecule_ID < 1 .OR. Molecule_ID > MAX_N_MOLECULAR_SPECIES ) THEN
        Partial_Pressure_AD = ZERO
        Temperature_AD      = ZERO
        Mass_Density_AD     = ZERO
        RETURN
      END IF
      Id = Molecule_ID
    ELSE
      Id = 1  ! Default value is for water vapor
    END IF

    ! Adjoint form of mass density to partial pressure conversion
    c = PA_TO_HPA * R0 / MOLECULAR_WEIGHT(Id)
    Mass_Density_AD = Mass_Density_AD + (c * Temperature  * Partial_Pressure_AD)
    Temperature_AD  = Temperature_AD  + (c * Mass_Density * Partial_Pressure_AD)
    Partial_Pressure_AD = ZERO

  END SUBROUTINE MD_to_PP_AD

END MODULE PP_MD
