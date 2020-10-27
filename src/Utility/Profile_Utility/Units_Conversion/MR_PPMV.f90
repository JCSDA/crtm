!
! MR_PPMV
!
! Module containing forward, tangent-linear and adjoint subroutines
! for mixing ratio to/from ppmv units conversion.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 05-Aug-2009
!                       paul.vandelst@noaa.gov
!

MODULE MR_PPMV

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Modules used
  USE Type_Kinds, ONLY: fp
  USE Profile_Utility_Parameters, ONLY: ZERO, ONE, &
                                        MAX_N_MOLECULAR_SPECIES, &
                                        MW_DRYAIR, MOLECULAR_WEIGHT, &
                                        G_TO_KG, PPV_TO_PPMV, &
                                        KG_TO_G, PPMV_TO_PPV
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: MR_to_PPMV, MR_to_PPMV_TL, MR_to_PPMV_AD
  PUBLIC :: PPMV_to_MR, PPMV_to_MR_TL, PPMV_to_MR_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  REAL(fp), PARAMETER :: MR_TO_PPMV_SCALE_FACTOR = G_TO_KG * PPV_TO_PPMV
  REAL(fp), PARAMETER :: PPMV_TO_MR_SCALE_FACTOR = KG_TO_G * PPMV_TO_PPV


CONTAINS


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       MR_to_PPMV
!
! PURPOSE:
!       Elemental subroutine to convert gas concentrations from mass mixing
!       ratio in g/kg to volume mixing ratio in ppmv
!
! CALLING SEQUENCE:
!       CALL MR_to_PPMV( Mixing_Ratio             , &  ! Input
!                        ppmv                     , &  ! Output
!                        Molecule_ID = Molecule_ID  )  ! Optional input
!
! INPUTS:
!       Mixing_Ratio:     Mass mixing ratio of gas.
!                         Must be > or = 0.0
!                         UNITS:      g/kg
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Scalar or any rank
!                         ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       ppmv:             Volume mixing ratio of gas.
!                         Set to 0.0 if input < 0.0
!                         UNITS:      ppmv
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Same as Mixing_Ratio argument
!                         ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Molecule_ID:      HITRAN molecular designation identifying the
!                         molecule for which the concentration units
!                         conversion is required. If not specified, the
!                         default value is that for water vapor.
!                         Valid values are:
!                           1: H2O       9: SO2      17: HI       25: H2O2
!                           2: CO2      10: NO2      18: ClO      26: C2H2
!                           3: O3       11: NH3      19: OCS      27: C2H6
!                           4: N2O      12: HNO3     20: H2CO     28: PH3
!                           5: CO       13: OH       21: HOCl     29: COF2
!                           6: CH4      14: HF       22: N2       30: SF6
!                           7: O2       15: HCl      23: HCN      31: H2S
!                           8: NO       16: HBr      24: CH3Cl    32: HCOOH
!                         Output is set to zero if an invalid Molecule_Id
!                         is supplied.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! PROCEDURE:
!       To convert mixing ratio in g/kg to parts-per-million, the following
!       is used:
!                                                 MW(Dry Air)
!         ppmv(MOL) = 1000 . Mixing_Ratio(MOL) . -------------
!                                                   MW(MOL)
!
!       where MW(Dry Air) = Average molecular weight of dry air
!             MW(MOL)     = Molecular weight of the gas in question.
!
!       The factor of 1000 derives from the product of the g/kg to g/g
!       scale factor (0.001) and the "parts-per" to "parts-per-million"
!       scale factor (1.0e+06)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 03-May-2000
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE MR_to_PPMV( &
    Mixing_Ratio, &  ! Input
    ppmv        , &  ! Output
    Molecule_ID   )  ! Optional Input
    ! Arguments
    REAL(fp),          INTENT(IN) :: Mixing_Ratio
    REAL(fp),          INTENT(OUT):: ppmv
    INTEGER, OPTIONAL, INTENT(IN) :: Molecule_ID
    ! Local variables
    INTEGER :: Id

    ! Error checks
    ! ...Zero output for -ve input
    IF ( Mixing_Ratio < ZERO ) THEN
      ppmv = ZERO
      RETURN
    ENDIF
    ! ...Zero output for invalid id
    IF ( PRESENT(Molecule_ID) ) THEN
      IF ( Molecule_ID < 1 .OR. Molecule_ID > MAX_N_MOLECULAR_SPECIES ) THEN
        ppmv = ZERO
        RETURN
      END IF
      Id = Molecule_ID
    ELSE
      Id = 1  ! Default value is for water vapor
    END IF

    ! Convert mass to volume mixing ratio
    ppmv = Mixing_Ratio * MR_TO_PPMV_SCALE_FACTOR * MW_DRYAIR / MOLECULAR_WEIGHT( Id )

  END SUBROUTINE MR_to_PPMV


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       MR_to_PPMV_TL
!
! PURPOSE:
!       Tangent-linear form of elemental subroutine to convert gas
!       concentrations from mass mixing ratio in g/kg to volume mixing
!       ratio in ppmv
!
! CALLING SEQUENCE:
!       CALL MR_to_PPMV_TL( Mixing_Ratio_TL          , &  ! Input
!                           ppmv_TL                  , &  ! Output
!                           Molecule_ID = Molecule_ID  )  ! Optional input
!
! INPUTS:
!       Mixing_Ratio_TL:  Tangent-linear mass mixing ratio of gas.
!                         UNITS:      g/kg
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Scalar or any rank
!                         ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       ppmv_TL:          Tangent-linear volume mixing ratio of gas.
!                         UNITS:      ppmv
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Same as Mixing_Ratio_TL argument
!                         ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Molecule_ID:      HITRAN molecular designation identifying the
!                         molecule for which the concentration units
!                         conversion is required. If not specified, the
!                         default value is that for water vapor.
!                         Valid values are:
!                           1: H2O       9: SO2      17: HI       25: H2O2
!                           2: CO2      10: NO2      18: ClO      26: C2H2
!                           3: O3       11: NH3      19: OCS      27: C2H6
!                           4: N2O      12: HNO3     20: H2CO     28: PH3
!                           5: CO       13: OH       21: HOCl     29: COF2
!                           6: CH4      14: HF       22: N2       30: SF6
!                           7: O2       15: HCl      23: HCN      31: H2S
!                           8: NO       16: HBr      24: CH3Cl    32: HCOOH
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 05-Aug-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE MR_to_PPMV_TL( &
    Mixing_Ratio_TL, &  ! TL  Input
    ppmv_TL        , &  ! TL  Output
    Molecule_ID      )  ! Optional Input
    ! Arguments
    REAL(fp),          INTENT(IN)  :: Mixing_Ratio_TL
    REAL(fp),          INTENT(OUT) :: ppmv_TL
    INTEGER, OPTIONAL, INTENT(IN)  :: Molecule_ID
    ! Local variables
    INTEGER :: Id

    ! Error checks
    ! ...Zero output for invalid id
    IF ( PRESENT(Molecule_ID) ) THEN
      IF ( Molecule_ID < 1 .OR. Molecule_ID > MAX_N_MOLECULAR_SPECIES ) THEN
        ppmv_TL = ZERO
        RETURN
      END IF
      Id = Molecule_ID
    ELSE
      Id = 1  ! Default value is for water vapor
    END IF

    ! Tangent-linear conversion
    ppmv_TL = Mixing_Ratio_TL * MR_TO_PPMV_SCALE_FACTOR * MW_DRYAIR / MOLECULAR_WEIGHT(Id)

  END SUBROUTINE MR_to_PPMV_TL


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       MR_to_PPMV_AD
!
! PURPOSE:
!       Adjoint form of elemental subroutine to convert gas
!       concentrations from mass mixing ratio in g/kg to volume mixing
!       ratio in ppmv
!
! CALLING SEQUENCE:
!       CALL MR_to_PPMV_AD( ppmv_AD                  , &  ! Input
!                           Mixing_Ratio_AD          , &  ! Output
!                           Molecule_ID = Molecule_ID  )  ! Optional input
!
! INPUTS:
!       ppmv_AD:          Adjoint volume mixing ratio of gas.
!                         *** SET TO ZERO ON EXIT ***
!                         UNITS:      N/A
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Scalar or any rank
!                         ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUTS:
!       Mixing_Ratio_AD:  Adjoint mass mixing ratio of gas.
!                         *** MUST HAVE VALUE ON ENTRY ***
!                         UNITS:      N/A
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Same as ppmv_AD argument.
!                         ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       Molecule_ID:      HITRAN molecular designation identifying the
!                         molecule for which the concentration units
!                         conversion is required. If not specified, the
!                         default value is that for water vapor.
!                         Valid values are:
!                           1: H2O       9: SO2      17: HI       25: H2O2
!                           2: CO2      10: NO2      18: ClO      26: C2H2
!                           3: O3       11: NH3      19: OCS      27: C2H6
!                           4: N2O      12: HNO3     20: H2CO     28: PH3
!                           5: CO       13: OH       21: HOCl     29: COF2
!                           6: CH4      14: HF       22: N2       30: SF6
!                           7: O2       15: HCl      23: HCN      31: H2S
!                           8: NO       16: HBr      24: CH3Cl    32: HCOOH
!                         Nothing is done if an invalid Molecule_Id is supplied.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 05-Aug-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE MR_to_PPMV_AD( &
    ppmv_AD        , &  ! Input
    Mixing_Ratio_AD, &  ! Output
    Molecule_ID      )  ! Optional Input
    ! Arguments
    REAL(fp),          INTENT(IN OUT) :: ppmv_AD
    REAL(fp),          INTENT(IN OUT) :: Mixing_Ratio_AD
    INTEGER, OPTIONAL, INTENT(IN)     :: Molecule_ID
    ! Local variables
    INTEGER :: Id

    ! Error checks
    ! ...Do nothing for invalid id
    IF ( PRESENT(Molecule_ID) ) THEN
      IF ( Molecule_ID < 1 .OR. Molecule_ID > MAX_N_MOLECULAR_SPECIES ) RETURN
      Id = Molecule_ID
    ELSE
      Id = 1  ! Default value is for water vapor
    END IF

    ! Adjoint conversion
    Mixing_Ratio_AD = Mixing_Ratio_AD + &
                      ppmv_AD * MR_TO_PPMV_SCALE_FACTOR * MW_DRYAIR / MOLECULAR_WEIGHT(Id)
    ppmv_AD = ZERO

  END SUBROUTINE MR_to_PPMV_AD


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       PPMV_to_MR
!
! PURPOSE:
!       Elemental subroutine to convert gas concentrations from volume mixing
!       ratio in ppmv to mass mixing ratio in g/kg.
!
! CALLING SEQUENCE:
!       CALL PPMV_to_MR( ppmv                     , &  ! Input
!                        Mixing_Ratio             , &  ! Output
!                        Molecule_ID = Molecule_ID  )  ! Optional input
!
! INPUTS:
!       ppmv:             Volume mixing ratio of gas.
!                         Must be > or = 0.0
!                         UNITS:      ppmv
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Scalar or any rank
!                         ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Mixing_Ratio:     Mass mixing ratio of gas.
!                         Set to 0.0 if input < 0.0
!                         UNITS:      g/kg
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Same as ppmv argument
!                         ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Molecule_ID:      HITRAN molecular designation identifying the
!                         molecule for which the concentration units
!                         conversion is required. If not specified, the
!                         default value is that for water vapor.
!                         Valid values are:
!                           1: H2O       9: SO2      17: HI       25: H2O2
!                           2: CO2      10: NO2      18: ClO      26: C2H2
!                           3: O3       11: NH3      19: OCS      27: C2H6
!                           4: N2O      12: HNO3     20: H2CO     28: PH3
!                           5: CO       13: OH       21: HOCl     29: COF2
!                           6: CH4      14: HF       22: N2       30: SF6
!                           7: O2       15: HCl      23: HCN      31: H2S
!                           8: NO       16: HBr      24: CH3Cl    32: HCOOH
!                         Output is set to zero if an invalid Molecule_Id
!                         is supplied.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! PROCEDURE:
!       To convert ppmv to mixing ratio, the following is used:
!
!                                          MW(MOL)
!         mr(MOL) = 0.001 . ppmv(MOL) . -------------
!                                        MW(Dry Air)
!
!       where MW(Dry Air) = Average molecular weight of dry air
!             MW(MOL)     = Molecular weight of the gas in question.
!
!       The factor of 0.001 derives from the product of the g/g to g/kg
!       scale factor (1000) and the "parts-per-million" to "parts-per"
!       scale factor (1.0e-06)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 03-May-2000
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE PPMV_to_MR( &
    ppmv        , &  ! Input
    Mixing_Ratio, &  ! Output
    Molecule_ID   )  ! Optional input
    ! Arguments
    REAL(fp),          INTENT(IN)  :: ppmv
    REAL(fp),          INTENT(OUT) :: Mixing_Ratio
    INTEGER, OPTIONAL, INTENT(IN)  :: Molecule_ID
    ! Local variables
    INTEGER :: Id

    ! Error checks
    ! ...Zero output for -ve input
    IF ( ppmv < ZERO ) THEN
      Mixing_Ratio = ZERO
      RETURN
    ENDIF
    ! ...Zero output for invalid id
    IF ( PRESENT(Molecule_ID) ) THEN
      IF ( Molecule_ID < 1 .OR. Molecule_ID > MAX_N_MOLECULAR_SPECIES ) THEN
        Mixing_Ratio = ZERO
        RETURN
      END IF
      Id = Molecule_ID
    ELSE
      Id = 1  ! Default value is for water vapor
    END IF

    ! Convert volume to mass mixing ratio
    Mixing_Ratio = ppmv * PPMV_TO_MR_SCALE_FACTOR * MOLECULAR_WEIGHT(Id) / MW_DRYAIR

  END SUBROUTINE PPMV_to_MR


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       PPMV_to_MR_TL
!
! PURPOSE:
!       Tangent-linear form of elemental subroutine to convert gas
!       concentrations from volume mixing ratio in ppmv to mass mixing ratio
!       in g/kg.
!
! CALLING SEQUENCE:
!       CALL PPMV_to_MR_TL( ppmv_TL                  , &  ! Input
!                           Mixing_Ratio_TL          , &  ! Output
!                           Molecule_ID = Molecule_ID  )  ! Optional input
!
! INPUTS:
!       ppmv_TL:          Tangent-linear volume mixing ratio of gas.
!                         UNITS:      ppmv
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Scalar or any rank
!                         ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Mixing_Ratio_TL:  Tangent-linear mass mixing ratio of gas.
!                         UNITS:      g/kg
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Same as ppmv argument
!                         ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Molecule_ID:      HITRAN molecular designation identifying the
!                         molecule for which the concentration units
!                         conversion is required. If not specified, the
!                         default value is that for water vapor.
!                         Valid values are:
!                           1: H2O       9: SO2      17: HI       25: H2O2
!                           2: CO2      10: NO2      18: ClO      26: C2H2
!                           3: O3       11: NH3      19: OCS      27: C2H6
!                           4: N2O      12: HNO3     20: H2CO     28: PH3
!                           5: CO       13: OH       21: HOCl     29: COF2
!                           6: CH4      14: HF       22: N2       30: SF6
!                           7: O2       15: HCl      23: HCN      31: H2S
!                           8: NO       16: HBr      24: CH3Cl    32: HCOOH
!                         Output is set to zero if an invalid Molecule_Id
!                         is supplied.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 06-Aug-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE PPMV_to_MR_TL( &
    ppmv_TL        , &  ! Input
    Mixing_Ratio_TL, &  ! Output
    Molecule_ID      )  ! Optional input
    ! Arguments
    REAL(fp),          INTENT(IN)  :: ppmv_TL
    REAL(fp),          INTENT(OUT) :: Mixing_Ratio_TL
    INTEGER, OPTIONAL, INTENT(IN)  :: Molecule_ID
    ! Local variables
    INTEGER :: Id

    ! Error checks
    ! ...Zero output for invalid id
    IF ( PRESENT(Molecule_ID) ) THEN
      IF ( Molecule_ID < 1 .OR. Molecule_ID > MAX_N_MOLECULAR_SPECIES ) THEN
        Mixing_Ratio_TL = ZERO
        RETURN
      END IF
      Id = Molecule_ID
    ELSE
      Id = 1  ! Default value is for water vapor
    END IF

    ! Tangent-linear conversion
    Mixing_Ratio_TL = ppmv_TL * PPMV_TO_MR_SCALE_FACTOR * MOLECULAR_WEIGHT(Id) / MW_DRYAIR

  END SUBROUTINE PPMV_to_MR_TL


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       PPMV_to_MR_AD
!
! PURPOSE:
!       Adjoint form of elemental subroutine to convert gas concentrations
!       from volume mixing ratio in ppmv to mass mixing ratio in g/kg.
!
! CALLING SEQUENCE:
!       CALL PPMV_to_MR_AD( Mixing_Ratio_AD          , &  ! Input
!                           ppmv_AD                  , &  ! Output
!                           Molecule_ID = Molecule_ID  )  ! Optional input
!
! INPUTS:
!       Mixing_Ratio_AD:  Adjoint mass mixing ratio of gas.
!                         *** SET TO ZERO ON EXIT ***
!                         UNITS:      N/A
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Scalar or any rank
!                         ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUTS:
!       ppmv_AD:          Adjoint volume mixing ratio of gas.
!                         *** MUST HAVE VALUE ON ENTRY ***
!                         UNITS:      N/A
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Same as Mixing_Ratio_AD argument
!                         ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Molecule_ID:      HITRAN molecular designation identifying the
!                         molecule for which the concentration units
!                         conversion is required. If not specified, the
!                         default value is that for water vapor.
!                         Valid values are:
!                           1: H2O       9: SO2      17: HI       25: H2O2
!                           2: CO2      10: NO2      18: ClO      26: C2H2
!                           3: O3       11: NH3      19: OCS      27: C2H6
!                           4: N2O      12: HNO3     20: H2CO     28: PH3
!                           5: CO       13: OH       21: HOCl     29: COF2
!                           6: CH4      14: HF       22: N2       30: SF6
!                           7: O2       15: HCl      23: HCN      31: H2S
!                           8: NO       16: HBr      24: CH3Cl    32: HCOOH
!                         Nothing is done if an invalid Molecule_Id is supplied.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 06-Aug-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE PPMV_to_MR_AD( &
    Mixing_Ratio_AD, &  ! Input
    ppmv_AD        , &  ! Output
    Molecule_ID      )  ! Optional input
    ! Arguments
    REAL(fp),          INTENT(IN OUT) :: Mixing_Ratio_AD
    REAL(fp),          INTENT(IN OUT) :: ppmv_AD
    INTEGER, OPTIONAL, INTENT(IN)     :: Molecule_ID
    ! Local variables
    INTEGER :: Id

    ! Error checks
    ! ...Do nothing for invalid id
    IF ( PRESENT(Molecule_ID) ) THEN
      IF ( Molecule_ID < 1 .OR. Molecule_ID > MAX_N_MOLECULAR_SPECIES ) RETURN
      Id = Molecule_ID
    ELSE
      Id = 1  ! Default value is for water vapor
    END IF

    ! Adjoint conversion
    ppmv_AD = ppmv_AD + &
              Mixing_Ratio_AD * PPMV_TO_MR_SCALE_FACTOR * MOLECULAR_WEIGHT(Id) / MW_DRYAIR
    Mixing_Ratio_AD = ZERO

  END SUBROUTINE PPMV_to_MR_AD

END MODULE MR_PPMV
