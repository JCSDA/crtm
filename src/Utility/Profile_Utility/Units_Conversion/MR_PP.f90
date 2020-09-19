!
! MR_PP
!
! Module containing forward, tangent-linear and adjoint subroutines
! for mixing ratio to/from partial pressure units conversion.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 23-Sep-2009
!                       paul.vandelst@noaa.gov
!

MODULE MR_PP

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Modules used
  USE Type_Kinds, ONLY: fp
  USE Profile_Utility_Parameters, ONLY: ZERO, ONE, &
                                        MAX_N_MOLECULAR_SPECIES, &
                                        MW_DRYAIR, MW_H2O, MOLECULAR_WEIGHT, &
                                        G_TO_KG, KG_TO_G
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: MR_to_PP, MR_to_PP_TL, MR_to_PP_AD
  PUBLIC :: PP_to_MR, PP_to_MR_TL, PP_to_MR_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &


CONTAINS


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       MR_to_PP
!
! PURPOSE:
!       Elemental subroutine to convert gas concentrations from mixing ratio
!       in g/kg to partial pressure in hectoPascals.
!
! CALLING SEQUENCE:
!       CALL MR_to_PP( Pressure                 , &  ! Input
!                      Mixing_Ratio             , &  ! Input
!                      Partial_Pressure         , &  ! Output
!                      Molecule_ID = Molecule_ID, &  ! Optional Input
!                      Water_Vapor = Water_Vapor  )  ! Optional Input
!
! INPUTS:
!       Pressure:          Total atmospheric pressure.
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar or any rank
!                          ATTRIBUTES: INTENT(IN)
!
!       Mixing_Ratio:      Mass mixing ratio of gas.
!                          UNITS:      g/kg
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Pressure
!                          ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Partial_Pressure:  Gas partial pressure.
!                          Set to zero for invalid input.
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as input Pressure
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
!       Water_Vapor:       Water vapor partial pressure. If this argument is
!                          not supplied, the mandatory MIXING_RATIO argument
!                          is assumed to be water vapor.
!                          This argument is ignored if the specified or default
!                          molecule ID is set to 1.
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as input Pressure
!                          ATTRIBUTES: INTENT(IN)
!
! PROCEDURE:
!       First a definition. In this routine, the mass mixing ratio of a gas
!       is defined to be, for a given volume, the ratio of the mass of the gas
!       in question to that of DRY air.
!
!       The ideal gas equation can be written as,
!
!         p = n.R0.T     ................................................(1)
!
!       for unit volume, where
!         n  = number of moles of gas,
!         R0 = universal gas constant
!         T  = temperature.
!
!       The mass mixing ratio of a gas is defined as the mass of the gas with
!       respect to the mass of dry air in the same volume. If we use eqn(1)
!       to construct expressions for the partial pressures of a particular
!       gas and dry air, we get,
!
!         pp(MOL)    = n(MOL).R0.T     .................................(2a)
!
!       and
!
!         pp(DryAir) = n(DryAir).R0.T     ..............................(2b)
!
!
!       Dividing eqn(2a) by (2b) and rearranging we get,
!
!                     n(MOL)
!         pp(MOL) = ----------- . pp(DryAir)     ........................(3)
!                    n(DryAir)
!
!       Replacing the expssion for the number of moles of a substance into
!       eqn(3) gives us,
!
!                     m(MOL)     MW(DryAir)
!         pp(MOL) = --------- . ------------ . pp(DryAir)
!                    MW(MOL)      m(DryAir)
!
!                     m(MOL)       MW(DryAir)
!                 = ----------- . ------------ . pp(DryAir)
!                    m(DryAir)      MW(MOL)
!
!                                     MW(DryAir)
!                 = 0.001 . w(MOL) . ------------ . pp(DryAir)     ......(4)
!                                      MW(MOL)
!
!       where m(MOL)     = mass of gas MOL in grams,
!             m(DryAir)  = mass of dry air in grams,
!             MW(MOL)    = molecular weight of of gas MOL in grams,
!             MW(DryAir) = effective molecular weight of dry air in grams,
!             w(MOL)     = mass mixing ratio of gas MOL in g/kg.
!
!       The factor of 0.001 in eqn(4) is to convert the units of the mixing
!       ratio from g/kg to g/g.
!
!       Two cases need to be addressed:
!
!
!       -------------
!       1) MOL == H2O
!       -------------
!
!       If the gas for which the mixing ratio is to be converted is water
!       vapor, then eqn(4) can be written as,
!
!
!         pp(H2O) = WX . ( p(Total) - pp(H2O) )     .....................(5)
!
!                                    MW(DryAir)
!       where WX = 0.001 . w(H2O) . ------------
!                                     MW(H2O)
!
!       Expanding eqn(5) further, we get
!
!         pp(H2O) = ( WX . p(Total) ) - ( WX . pp(H2O) )
!
!       and,
!
!         pp(H2O) . ( 1 + WX ) = WX . p(Total)
!
!       and finally,
!
!                        WX
!         pp(H2O) = ------------ . p(Total)     .........................(6)
!                    ( 1 + WX )
!
!       Eqn(6) is used to determine the partial pressure for water vapor
!       input in this routine.
!
!
!       -------------------
!       2) MOL is *not* H2O
!       -------------------
!
!       Using eqn(4) to determine an expression for the non-water vapor
!       gases, we get,
!
!         pp(MOL) = WX . p(DryAir)     ..................................(7)
!
!                                    MW(DryAir)
!       where WX = 0.001 . w(MOL) . ------------
!                                     MW(MOL)
!
!       and w(MOL) is still defined as in eqn(4).
!
!
!       This is based on the same methods used in the LBLRTM conversion code
!       in its lblatm.f module where molecular mixing ratios (either ppmv or
!       g/kg) are always with respect to DRY AIR.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 20-Sep-2002
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE MR_to_PP( &
    Pressure        , &  ! Input
    Mixing_Ratio    , &  ! Input
    Partial_Pressure, &  ! Output
    Molecule_ID     , &  ! Optional Input
    Water_Vapor       )  ! Optional Input
    ! Arguments
    REAL(fp),           INTENT(IN)  :: Pressure
    REAL(fp),           INTENT(IN)  :: Mixing_Ratio
    REAL(fp),           INTENT(OUT) :: Partial_Pressure
    INTEGER,  OPTIONAL, INTENT(IN)  :: Molecule_ID
    REAL(fp), OPTIONAL, INTENT(IN)  :: Water_Vapor
    ! Local variables
    INTEGER :: Id
    REAL(fp) :: w

    ! Setup
    Partial_Pressure = ZERO
    IF ( Pressure < ZERO .OR. Mixing_Ratio < ZERO ) RETURN
    IF ( PRESENT(Molecule_ID) ) THEN
      IF ( Molecule_ID < 1 .OR. Molecule_ID > MAX_N_MOLECULAR_SPECIES ) RETURN
      Id = Molecule_ID
    ELSE
      Id = 1  ! Default value is for water vapor
    END IF
    IF ( PRESENT(Water_Vapor) ) THEN
      IF ( Water_Vapor < ZERO ) RETURN
    END IF
    IF ( Id > 1 .AND. (.NOT. PRESENT(Water_Vapor)) ) RETURN


    ! Calculate the "w" factor
    w = G_TO_KG * Mixing_Ratio * MW_DRYAIR / MOLECULAR_WEIGHT(Id)


    ! Convert amount based on molecule ID
    IF ( Id == 1 ) THEN
      Partial_Pressure = (w / (ONE + w)) * Pressure
    ELSE
      Partial_Pressure = w * (Pressure - Water_Vapor)
    END IF

  END SUBROUTINE MR_to_PP


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       MR_to_PP_TL
!
! PURPOSE:
!       Tangent-linear form of elemental subroutine to convert gas
!       concentrations from mixing ratio in g/kg to partial pressure
!       in hectoPascals.
!
! CALLING SEQUENCE:
!       CALL MR_to_PP_TL( Pressure                       , &  ! FWD Input
!                         Mixing_Ratio                   , &  ! FWD Input
!                         Pressure_TL                    , &  ! TL  Input
!                         Mixing_Ratio_TL                , &  ! TL  Input
!                         Partial_Pressure_TL            , &  ! TL  Output
!                         Molecule_ID    = Molecule_ID   , &  ! Optional Input
!                         Water_Vapor    = Water_Vapor   , &  ! Optional FWD Input
!                         Water_Vapor_TL = Water_Vapor_TL  )  ! Optional TL  Input
!
! INPUTS:
!       Pressure:             Total atmospheric pressure.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN)
!
!       Mixing_Ratio:         Mass mixing ratio of gas.
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
!       Mixing_Ratio_TL:      Tangent-linear mass mixing ratio of gas.
!                             UNITS:      g/kg
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Partial_Pressure_TL:  Tangent-linear gas partial pressure.
!                             Set to zero for invalid input.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
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
!       Water_Vapor:          Water vapor partial pressure. If this argument is
!                             not supplied, the mandatory MIXING_RATIO argument
!                             is assumed to be water vapor.
!                             This argument is ignored if the specified or default
!                             molecule ID is set to 1.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Water_Vapor_TL:       Tangent-linear water vapor partial pressure.
!                             This argument is ignored if the optional
!                             WATER_VAPOR argument is not supplied also or if the
!                             specified or default molecule ID is set to 1.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 23-Sep-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE MR_to_PP_TL( &
    Pressure           , &  ! FWD Input
    Mixing_Ratio       , &  ! FWD Input
    Pressure_TL        , &  ! TL  Input
    Mixing_Ratio_TL    , &  ! TL  Input
    Partial_Pressure_TL, &  ! TL  Output
    Molecule_ID        , &  ! Optional Input
    Water_Vapor        , &  ! Optional FWD Input
    Water_Vapor_TL       )  ! Optional TL  Input
    ! Arguments
    REAL(fp),           INTENT(IN)  :: Pressure
    REAL(fp),           INTENT(IN)  :: Mixing_Ratio
    REAL(fp),           INTENT(IN)  :: Pressure_TL
    REAL(fp),           INTENT(IN)  :: Mixing_Ratio_TL
    REAL(fp),           INTENT(OUT) :: Partial_Pressure_TL
    INTEGER,  OPTIONAL, INTENT(IN)  :: Molecule_ID
    REAL(fp), OPTIONAL, INTENT(IN)  :: Water_Vapor
    REAL(fp), OPTIONAL, INTENT(IN)  :: Water_Vapor_TL
    ! Local variables
    INTEGER :: Id
    REAL(fp) :: c
    REAL(fp) :: wv_TL
    REAL(fp) :: w, w_TL

    ! Setup
    Partial_Pressure_TL = ZERO
    IF ( Pressure < ZERO .OR. Mixing_Ratio < ZERO ) RETURN
    IF ( PRESENT(Molecule_ID) ) THEN
      IF ( Molecule_ID < 1 .OR. Molecule_ID > MAX_N_MOLECULAR_SPECIES ) RETURN
      Id = Molecule_ID
    ELSE
      Id = 1  ! Default value is for water vapor
    END IF
    IF ( PRESENT(Water_Vapor) ) THEN
      IF ( Water_Vapor < ZERO ) RETURN
      ! ...Local tangent-linear variable assignment
      IF ( PRESENT(Water_Vapor_TL) ) THEN
        wv_TL = Water_Vapor_TL
      ELSE
        wv_TL = ZERO
      END IF
    END IF
    IF ( Id > 1 .AND. (.NOT. PRESENT(Water_Vapor)) ) RETURN


    ! Calculate the "w" factor
    c = G_TO_KG * MW_DRYAIR / MOLECULAR_WEIGHT(Id)
    w    = c * Mixing_Ratio
    w_TL = c * Mixing_Ratio_TL


    ! Convert amount based on molecule ID
    IF ( Id == 1 ) THEN
      Partial_Pressure_TL = ((Pressure * w_TL) + (w * (ONE + w) * Pressure_TL)) / (ONE + w)**2
    ELSE
      Partial_Pressure_TL = ((Pressure - Water_Vapor) * w_TL) + (w * Pressure_TL) - (w * wv_TL)
    END IF

  END SUBROUTINE MR_to_PP_TL


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       MR_to_PP_AD
!
! PURPOSE:
!       Adjoint form of elemental subroutine to convert gas
!       concentrations from mixing ratio in g/kg to partial pressure
!       in hectoPascals.
!
! CALLING SEQUENCE:
!       CALL MR_to_PP_AD( Pressure                       , &  ! FWD Input
!                         Mixing_Ratio                   , &  ! FWD Input
!                         Partial_Pressure_AD            , &  ! AD  Input
!                         Pressure_AD                    , &  ! AD  Output
!                         Mixing_Ratio_AD                , &  ! AD  Output
!                         Molecule_ID    = Molecule_ID   , &  ! Optional Input
!                         Water_Vapor    = Water_Vapor   , &  ! Optional FWD Input
!                         Water_Vapor_AD = Water_Vapor_AD  )  ! Optional AD  Output
!
! INPUTS:
!       Pressure:             Total atmospheric pressure.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN)
!
!       Mixing_Ratio:         Mass mixing ratio of gas.
!                             UNITS:      g/kg
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Partial_Pressure_AD:  Adjoint gas partial pressure.
!                             *** SET TO ZERO ON EXIT ***
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUTS:
!       Pressure_AD:          Adjoint atmospheric pressure.
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN OUT)
!
!       Mixing_Ratio_AD:      Adjoint mass mixing ratio of gas.
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      g/kg
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN) OUT
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
!       Water_Vapor:          Water vapor partial pressure. If this argument is
!                             not supplied, the mandatory MIXING_RATIO argument
!                             is assumed to be water vapor.
!                             This argument is ignored if the specified or default
!                             molecule ID is set to 1.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       Water_Vapor_AD:       Adjoint water vapor partial pressure.
!                             This argument is ignored if the optional
!                             WATER_VAPOR argument is not supplied also or if the
!                             specified or default molecule ID is set to 1.
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: OPTIONAL, INTENT(IN OUT)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 23-Sep-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE MR_to_PP_AD( &
    Pressure           , &  ! FWD Input
    Mixing_Ratio       , &  ! FWD Input
    Partial_Pressure_AD, &  ! AD  Input
    Pressure_AD        , &  ! AD  Output
    Mixing_Ratio_AD    , &  ! AD  Output
    Molecule_ID        , &  ! Optional Input
    Water_Vapor        , &  ! Optional FWD Input
    Water_Vapor_AD       )  ! Optional AD  Output
    ! Arguments
    REAL(fp),           INTENT(IN)     :: Pressure
    REAL(fp),           INTENT(IN)     :: Mixing_Ratio
    REAL(fp),           INTENT(IN OUT) :: Partial_Pressure_AD
    REAL(fp),           INTENT(IN OUT) :: Pressure_AD
    REAL(fp),           INTENT(IN OUT) :: Mixing_Ratio_AD
    INTEGER,  OPTIONAL, INTENT(IN)     :: Molecule_ID
    REAL(fp), OPTIONAL, INTENT(IN)     :: Water_Vapor
    REAL(fp), OPTIONAL, INTENT(IN OUT) :: Water_Vapor_AD
    ! Local variables
    INTEGER :: Id
    REAL(fp) :: c
    REAL(fp) :: wv_AD
    REAL(fp) :: w, w_AD

    ! Setup
    IF ( Pressure < ZERO .OR. Mixing_Ratio < ZERO ) THEN
      Partial_Pressure_AD = ZERO
      Pressure_AD         = ZERO
      Mixing_Ratio_AD     = ZERO
      RETURN
    END IF
    IF ( PRESENT(Molecule_ID) ) THEN
      IF ( Molecule_ID < 1 .OR. Molecule_ID > MAX_N_MOLECULAR_SPECIES ) THEN
        Partial_Pressure_AD = ZERO
        Pressure_AD         = ZERO
        Mixing_Ratio_AD     = ZERO
        RETURN
      END IF
      Id = Molecule_ID
    ELSE
      Id = 1  ! Default value is for water vapor
    END IF
    IF ( PRESENT(Water_Vapor) ) THEN
      IF ( Water_Vapor < ZERO ) THEN
        Partial_Pressure_AD = ZERO
        Pressure_AD         = ZERO
        Mixing_Ratio_AD     = ZERO
        RETURN
      END IF
      ! ...Local adjoint variable assignment
      IF ( PRESENT(Water_Vapor_AD) ) THEN
        wv_AD = Water_Vapor_AD
      ELSE
        wv_AD = ZERO
      END IF
    END IF
    IF ( Id > 1 .AND. (.NOT. PRESENT(Water_Vapor)) ) THEN
      Partial_Pressure_AD = ZERO
      Pressure_AD         = ZERO
      Mixing_Ratio_AD     = ZERO
      RETURN
    END IF


    ! Forward calculations
    c = G_TO_KG * MW_DRYAIR / MOLECULAR_WEIGHT(Id)
    w = c * Mixing_Ratio


    ! Adjoint form of conversion based on molecule ID
    IF ( Id == 1 ) THEN
      Pressure_AD = Pressure_AD + (w * Partial_Pressure_AD / (ONE + w))
      w_AD = Pressure * Partial_Pressure_AD / (ONE + w)**2
    ELSE
      wv_AD       = wv_AD       - (w * Partial_Pressure_AD)
      Pressure_AD = Pressure_AD + (w * Partial_Pressure_AD)
      w_AD = (Pressure - Water_Vapor) * Partial_Pressure_AD
      ! ...Save local mods to Water_Vapor_AD argument
      IF ( PRESENT(Water_Vapor_AD) ) Water_Vapor_AD = wv_AD
    END IF
    Partial_Pressure_AD = ZERO

    ! Adjoint form of "w" calculation
    Mixing_Ratio_AD = Mixing_Ratio_AD + (c * w_AD)

  END SUBROUTINE MR_to_PP_AD


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       PP_to_MR
!
! PURPOSE:
!       Elemental subroutine to convert gas concentrations from partial
!       pressure in hectoPascals to mass mixing ratio in g/kg.
!
! CALLING SEQUENCE:
!       CALL PP_to_MR( Pressure                 , &  ! Input
!                      Partial_Pressure         , &  ! Input
!                      Mixing_Ratio             , &  ! Output
!                      Molecule_ID = Molecule_ID, &  ! Optional Input
!                      Water_Vapor = Water_Vapor  )  ! Optional Input
!
! INPUTS:
!       Pressure:          Total atmospheric pressure.
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar or any rank
!                          ATTRIBUTES: INTENT(IN)
!
!       Partial_Pressure:  Partial pressure of gas.
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Pressure
!                          ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Mixing_Ratio:      Mass mixing ratio of gas.
!                          Set to zero for invalid input.
!                          UNITS:      g/kg
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as input Pressure
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
!       Water_Vapor:       Water vapor mass mixing ratio. If this argument is
!                          not supplied, the mandatory PARTIAL_PRESSURE argument
!                          is assumed to be water vapor.
!                          This argument is ignored if the specified or default
!                          molecule ID is set to 1.
!                          UNITS:      g/kg
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as input Pressure
!                          ATTRIBUTES: INTENT(IN)
!
! PROCEDURE:
!       First a definition. In this routine, the mass mixing ratio of a gas
!       is defined to be, for a given volume, the ratio of the mass of the gas
!       in question to that of DRY air.
!
!       The ideal gas equation can be written as,
!
!         p = n.R0.T     ................................................(1)
!
!       for unit volume, where
!         n  = number of moles of gas,
!         R0 = universal gas constant
!         T  = temperature.
!
!       The mass mixing ratio of a gas is defined as the mass of the gas with
!       respect to the mass of dry air in the same volume. If we use eqn(1)
!       to construct expressions for the partial pressures of a particular
!       gas and dry air, we get,
!
!         pp(MOL)    = n(MOL).R0.T     .................................(2a)
!
!       and
!
!         pp(DryAir) = n(DryAir).R0.T     ..............................(2b)
!
!
!       Dividing eqn(2a) by (2b) and rearranging we get,
!
!                     n(MOL)
!         pp(MOL) = ----------- . pp(DryAir)     ........................(3)
!                    n(DryAir)
!
!       Replacing the expssion for the number of moles of a substance into
!       eqn(3) gives us,
!
!                     m(MOL)     MW(DryAir)
!         pp(MOL) = --------- . ------------ . pp(DryAir)
!                    MW(MOL)      m(DryAir)
!
!                     m(MOL)       MW(DryAir)
!                 = ----------- . ------------ . pp(DryAir)
!                    m(DryAir)      MW(MOL)
!
!                                     MW(DryAir)
!                 = 0.001 . w(MOL) . ------------ . pp(DryAir)     ......(4)
!                                      MW(MOL)
!
!       where m(MOL)     = mass of gas MOL in grams,
!             m(DryAir)  = mass of dry air in grams,
!             MW(MOL)    = molecular weight of of gas MOL in grams,
!             MW(DryAir) = effective molecular weight of dry air in grams,
!             w(MOL)     = mass mixing ratio of gas MOL in g/kg.
!
!       The factor of 0.001 in eqn(4) is to convert the units of the mixing
!       ratio from g/kg to g/g.
!
!       Thus to determine the mixing ratio of the gas in question from its
!       partial pressure, eqn(4) is rearranged to give,
!
!                            MW(MOL)        pp(MOL)
!         w(MOL) = 1000 . ------------ . ------------     ...............(5)
!                          MW(DryAir)     pp(DryAir)
!
!
!       Two cases need to be addressed:
!
!
!       -------------
!       1) MOL == H2O
!       -------------
!
!       If the gas for which the partial pressure is to be converted is water
!       vapor, then the dry air partial pressure required in eqn(5) is simply
!       computed using,
!
!         pp(DryAir) = p(Total) - pp(H2O)
!
!       which is then used in eqn(5) to compute the water vapour mixing ratio.
!
!
!       -------------------
!       2) MOL is *not* H2O
!       -------------------
!
!       For this gas, the dry air partial pressure must be computed using the
!       water vapor mixing ratio supplied in the optional Water_Vapor argument.
!
!       Eqn(4) above can be rewritten as,
!
!         pp(H2O) = WX . p(DryAir)     ..................................(6)
!
!                                    MW(DryAir)
!       where WX = 0.001 . w(H2O) . ------------
!                                     MW(H2O)
!
!       Rearranging eqn(6), we then get,
!
!                      pp(H2O)
!         p(DryAir) = ---------     .....................................(7)
!                        WX
!
!       But, eqn(6) can also be written as,
!
!         pp(H2O) = WX . ( p(Total) - pp(H2O) )
!
!                 = ( WX . p(Total) ) - ( WX . pp(H2O) )
!
!       and thus,
!
!         pp(H2O) . ( 1 + WX ) = WX . p(Total)
!
!       with finally,
!
!                        WX
!         pp(H2O) = ------------ . p(Total)     .........................(8)
!                    ( 1 + WX )
!
!       Substituting eqn(8) into eqn(7) gives,
!
!                      p(Total)
!         p(DryAir) = ----------     ....................................(9)
!                      1 + WX
!
!       Eqn(9) is used to compute the dry air partial pressure which is then
!       used in enq(5) to compute the mixing ratio of the gas in question.
!
!
!       This is based on the same methods used in the LBLRTM conversion code
!       in its lblatm.f module where molecular mixing ratios (either ppmv or
!       g/kg) are always with respect to DRY AIR.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 20-Sep-2002
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE PP_to_MR( &
    Pressure        , &  ! Input
    Partial_Pressure, &  ! Input
    Mixing_Ratio    , &  ! Output
    Molecule_ID     , &  ! Optional Input
    Water_Vapor       )  ! Optional Input
    ! Arguments
    REAL(fp),           INTENT(IN)  :: Pressure
    REAL(fp),           INTENT(IN)  :: Partial_Pressure
    REAL(fp),           INTENT(OUT) :: Mixing_Ratio
    INTEGER,  OPTIONAL, INTENT(IN)  :: Molecule_ID
    REAL(fp), OPTIONAL, INTENT(IN)  :: Water_Vapor
    ! Local variables
    INTEGER :: Id
    REAL(fp) :: w
    REAL(fp) :: Dry_Air_Pressure

    ! Setup
    Mixing_Ratio = ZERO
    IF ( Pressure < ZERO .OR. Partial_Pressure < ZERO ) RETURN
    IF ( PRESENT(Molecule_ID) ) THEN
      IF ( Molecule_ID < 1 .OR. Molecule_ID > MAX_N_MOLECULAR_SPECIES ) RETURN
      Id = Molecule_ID
    ELSE
      Id = 1  ! Default value is for water vapor
    END IF
    IF ( PRESENT(Water_Vapor) ) THEN
      IF ( Water_Vapor < ZERO ) RETURN
    END IF
    IF ( Id > 1 .AND. (.NOT. PRESENT(Water_Vapor)) ) RETURN



    ! Calculate the dry air partial pressure
    IF ( Id == 1 ) THEN
      Dry_Air_Pressure = Pressure - Partial_Pressure
    ELSE
      w = G_TO_KG * Water_Vapor * MW_DRYAIR / MW_H2O
      Dry_Air_Pressure = Pressure / (ONE + w)
    END IF

    ! Calculate the mass mixing ratio
    w = KG_TO_G * MOLECULAR_WEIGHT(Id) / MW_DRYAIR
    Mixing_Ratio = w * Partial_Pressure / Dry_Air_Pressure

  END SUBROUTINE PP_to_MR


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       PP_to_MR_TL
!
! PURPOSE:
!       Tangent-linear form of elemental subroutine to convert gas
!       concentrations from partial pressure in hectoPascals to mass
!       mixing ratio in g/kg.
!
! CALLING SEQUENCE:
!       CALL PP_to_MR_TL( Pressure                       , &  ! FWD Input
!                         Partial_Pressure               , &  ! FWD Input
!                         Pressure_TL                    , &  ! TL  Input
!                         Partial_Pressure_TL            , &  ! TL  Input
!                         Mixing_Ratio_TL                , &  ! TL  Output
!                         Molecule_ID    = Molecule_ID   , &  ! Optional Input
!                         Water_Vapor    = Water_Vapor   , &  ! Optional FWD Input
!                         Water_Vapor_TL = Water_Vapor_TL  )  ! Optional TL  Input
!
! INPUTS:
!       Pressure:             Total atmospheric pressure.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN)
!
!       Partial_Pressure:     Partial pressure of gas.
!                             UNITS:      hectoPascals, hPa
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
!       Partial_Pressure_TL:  Tangentlinear partial pressure of gas.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Mixing_Ratio_TL:      Tangent-linear mass mixing ratio of gas.
!                             Set to zero for invalid input.
!                             UNITS:      g/kg
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
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
!       Water_Vapor:          Water vapor mass mixing ratio. If this argument is
!                             not supplied, the mandatory PARTIAL_PRESSURE argument
!                             is assumed to be water vapor.
!                             This argument is ignored if the specified or default
!                             molecule ID is set to 1.
!                             UNITS:      g/kg
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Water_Vapor_TL:       Tangent-linear water vapor mass mixing ratio.
!                             This argument is ignored if the optional
!                             WATER_VAPOR argument is not supplied also or if the
!                             specified or default molecule ID is set to 1.
!                             UNITS:      g/kg
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 23-Sep-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE PP_to_MR_TL( &
    Pressure           , &  ! FWD Input
    Partial_Pressure   , &  ! FWD Input
    Pressure_TL        , &  ! TL  Input
    Partial_Pressure_TL, &  ! TL  Input
    Mixing_Ratio_TL    , &  ! TL  Output
    Molecule_ID        , &  ! Optional Input
    Water_Vapor        , &  ! Optional FWD Input
    Water_Vapor_TL       )  ! Optional TL  Input
    ! Arguments
    REAL(fp),           INTENT(IN)  :: Pressure
    REAL(fp),           INTENT(IN)  :: Partial_Pressure
    REAL(fp),           INTENT(IN)  :: Pressure_TL
    REAL(fp),           INTENT(IN)  :: Partial_Pressure_TL
    REAL(fp),           INTENT(OUT) :: Mixing_Ratio_TL
    INTEGER,  OPTIONAL, INTENT(IN)  :: Molecule_ID
    REAL(fp), OPTIONAL, INTENT(IN)  :: Water_Vapor
    REAL(fp), OPTIONAL, INTENT(IN)  :: Water_Vapor_TL
    ! Local variables
    INTEGER :: Id
    REAL(fp) :: c
    REAL(fp) :: wv_TL
    REAL(fp) :: w, w_TL
    REAL(fp) :: Dry_Air_Pressure, Dry_Air_Pressure_TL

    ! Setup
    Mixing_Ratio_TL = ZERO
    IF ( Pressure < ZERO .OR. Partial_Pressure < ZERO ) RETURN
    IF ( PRESENT(Molecule_ID) ) THEN
      IF ( Molecule_ID < 1 .OR. Molecule_ID > MAX_N_MOLECULAR_SPECIES ) RETURN
      Id = Molecule_ID
    ELSE
      Id = 1  ! Default value is for water vapor
    END IF
    IF ( PRESENT(Water_Vapor) ) THEN
      IF ( Water_Vapor < ZERO ) RETURN
      ! ...Local tangent-linear variable assignment
      IF ( PRESENT(Water_Vapor_TL) ) THEN
        wv_TL = Water_Vapor_TL
      ELSE
        wv_TL = ZERO
      END IF
    END IF
    IF ( Id > 1 .AND. (.NOT. PRESENT(Water_Vapor)) ) RETURN



    ! Calculate the dry air partial pressure
    IF ( Id == 1 ) THEN
      Dry_Air_Pressure = Pressure - Partial_Pressure
      Dry_Air_Pressure_TL = Pressure_TL - Partial_Pressure_TL
    ELSE
      c = G_TO_KG * MW_DRYAIR / MW_H2O
      w    = c * Water_Vapor
      w_TL = c * wv_TL
      Dry_Air_Pressure = Pressure / (ONE + w)
      Dry_Air_Pressure_TL = (((ONE + w) * Pressure_TL) - (Pressure * w_TL)) / (ONE + w)**2
    END IF


    ! Calculate the mass mixing ratio
    c = KG_TO_G * MOLECULAR_WEIGHT(Id) / MW_DRYAIR
    Mixing_Ratio_TL = c * ((Dry_Air_Pressure * Partial_Pressure_TL) - &
                           (Partial_Pressure * Dry_Air_Pressure_TL)) / Dry_Air_Pressure**2

  END SUBROUTINE PP_to_MR_TL


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       PP_to_MR_AD
!
! PURPOSE:
!       Adjoint form of elemental subroutine to convert gas
!       concentrations from partial pressure in hectoPascals to mass
!       mixing ratio in g/kg.
!
! CALLING SEQUENCE:
!       CALL PP_to_MR_AD( Pressure                       , &  ! FWD Input
!                         Partial_Pressure               , &  ! FWD Input
!                         Mixing_Ratio_AD                , &  ! AD  Input
!                         Pressure_AD                    , &  ! AD  Output
!                         Partial_Pressure_AD            , &  ! AD  Output
!                         Molecule_ID    = Molecule_ID   , &  ! Optional Input
!                         Water_Vapor    = Water_Vapor   , &  ! Optional FWD Input
!                         Water_Vapor_AD = Water_Vapor_AD  )  ! Optional AD  Output
!
! INPUTS:
!       Pressure:             Total atmospheric pressure.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN)
!
!       Partial_Pressure:     Partial pressure of gas.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Mixing_Ratio_AD:      Adjoint mass mixing ratio of gas.
!                             *** SET TO ZERO ON EXIT ***
!                             UNITS:      g/kg
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUTS:
!       Pressure_AD:          Adjoint atmospheric pressure.
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN OUT)
!
!       Partial_Pressure_AD:  Adjoint partial pressure of gas.
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
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
!       Water_Vapor:          Water vapor mass mixing ratio. If this argument is
!                             not supplied, the mandatory PARTIAL_PRESSURE argument
!                             is assumed to be water vapor.
!                             This argument is ignored if the specified or default
!                             molecule ID is set to 1.
!                             UNITS:      g/kg
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       Water_Vapor_AD:       Adjoint water vapor mass mixing ratio.
!                             This argument is ignored if the optional
!                             WATER_VAPOR argument is not supplied also or if the
!                             specified or default molecule ID is set to 1.
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      g/kg
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: OPTIONAL, INTENT(IN OUT)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 23-Sep-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE PP_to_MR_AD( &
    Pressure           , &  ! FWD Input
    Partial_Pressure   , &  ! FWD Input
    Mixing_Ratio_AD    , &  ! AD  Input
    Pressure_AD        , &  ! AD  Output
    Partial_Pressure_AD, &  ! AD  Output
    Molecule_ID        , &  ! Optional Input
    Water_Vapor        , &  ! Optional FWD Input
    Water_Vapor_AD       )  ! Optional AD  Output
    ! Arguments
    REAL(fp),           INTENT(IN)     :: Pressure
    REAL(fp),           INTENT(IN)     :: Partial_Pressure
    REAL(fp),           INTENT(IN OUT) :: Mixing_Ratio_AD
    REAL(fp),           INTENT(IN OUT) :: Pressure_AD
    REAL(fp),           INTENT(IN OUT) :: Partial_Pressure_AD
    INTEGER,  OPTIONAL, INTENT(IN)     :: Molecule_ID
    REAL(fp), OPTIONAL, INTENT(IN)     :: Water_Vapor
    REAL(fp), OPTIONAL, INTENT(IN OUT) :: Water_Vapor_AD
    ! Local variables
    INTEGER :: Id
    REAL(fp) :: c
    REAL(fp) :: wv_AD
    REAL(fp) :: w, w_AD
    REAL(fp) :: Dry_Air_Pressure, Dry_Air_Pressure_AD

    ! Setup
    IF ( Pressure < ZERO .OR. Partial_Pressure < ZERO ) THEN
      Mixing_Ratio_AD     = ZERO
      Pressure_AD         = ZERO
      Partial_Pressure_AD = ZERO
      RETURN
    END IF
    IF ( PRESENT(Molecule_ID) ) THEN
      IF ( Molecule_ID < 1 .OR. Molecule_ID > MAX_N_MOLECULAR_SPECIES ) THEN
        Mixing_Ratio_AD     = ZERO
        Pressure_AD         = ZERO
        Partial_Pressure_AD = ZERO
        RETURN
      END IF
      Id = Molecule_ID
    ELSE
      Id = 1  ! Default value is for water vapor
    END IF
    IF ( PRESENT(Water_Vapor) ) THEN
      IF ( Water_Vapor < ZERO ) THEN
        Mixing_Ratio_AD     = ZERO
        Pressure_AD         = ZERO
        Partial_Pressure_AD = ZERO
        RETURN
      END IF
      ! ...Local adjoint variable assignment
      IF ( PRESENT(Water_Vapor_AD) ) THEN
        wv_AD = Water_Vapor_AD
      ELSE
        wv_AD = ZERO
      END IF
    END IF
    IF ( Id > 1 .AND. (.NOT. PRESENT(Water_Vapor)) ) THEN
      Mixing_Ratio_AD     = ZERO
      Pressure_AD         = ZERO
      Partial_Pressure_AD = ZERO
      RETURN
    END IF

    ! Calculate the dry air partial pressure
    IF ( Id == 1 ) THEN
      Dry_Air_Pressure = Pressure - Partial_Pressure
    ELSE
      w = Water_Vapor * G_TO_KG * MW_DRYAIR / MW_H2O
      Dry_Air_Pressure = Pressure / (ONE + w)
    END IF


    ! Adjoint form of mass mixing ratio conversion
    c = KG_TO_G * MOLECULAR_WEIGHT(Id) / MW_DRYAIR
    Dry_Air_Pressure_AD = -(c * Partial_Pressure * Mixing_Ratio_AD / Dry_Air_Pressure**2)
    Partial_Pressure_AD = Partial_Pressure_AD + (c * Mixing_Ratio_AD / Dry_Air_Pressure)
    Mixing_Ratio_AD = ZERO


    ! Adjoint form of dry air partial pressure calculation
    IF ( Id == 1 ) THEN
      Partial_Pressure_AD = Partial_Pressure_AD - Dry_Air_Pressure_AD
      Pressure_AD         = Pressure_AD         + Dry_Air_Pressure_AD
    ELSE
      c = G_TO_KG * MW_DRYAIR / MW_H2O
      w = c * Water_Vapor
      w_AD = -(Pressure * Dry_Air_Pressure_AD / (ONE + w)**2)
      Pressure_AD = Pressure_AD + (Dry_Air_Pressure_AD / (ONE + w))
      wv_AD = wv_AD + (c * w_AD)
      ! ...Save local mods to Water_Vapor_AD argument
      IF ( PRESENT(Water_Vapor_AD) ) Water_Vapor_AD = wv_AD
    END IF

  END SUBROUTINE PP_to_MR_AD

END MODULE MR_PP
