!
! PPMV_PP
!
! Module containing forward, tangent-linear and adjoint subroutines
! for volume mixing ration to/from partial pressure units conversion.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 21-Sep-2009
!                       paul.vandelst@noaa.gov
!

MODULE PPMV_PP

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Modules used
  USE Type_Kinds, ONLY: fp
  USE Profile_Utility_Parameters, ONLY: ZERO, ONE, &
                                        PPV_TO_PPMV, &
                                        PPMV_TO_PPV
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: PPMV_to_PP, PPMV_to_PP_TL, PPMV_to_PP_AD
  PUBLIC :: PP_to_PPMV, PP_to_PPMV_TL, PP_to_PPMV_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &


CONTAINS


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       PPMV_to_PP
!
! PURPOSE:
!       Elemental subroutine to convert gas concentrations from volume
!       mixing ratio in ppmv to partial pressure.
!
! CALLING SEQUENCE:
!       CALL PPMV_to_PP( Pressure               , &  ! Input
!                        ppmv                   , &  ! Input
!                        Partial_Pressure       , &  ! Output
!                        Water_Vapor=Water_Vapor  )  ! Optional Input
!
! INPUTS:
!       Pressure:          Total atmospheric pressure.
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar or and rank
!                          ATTRIBUTES: INTENT(IN)
!
!       ppmv:              Gas volume mixing ratio in ppmv.
!                          UNITS:      ppmv
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Pressure
!                          ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Partial_Pressure:  Gas partial pressure.
!                          Set to 0.0 if input < 0.0
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as input Pressure
!                          ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Water_Vapor:       Water vapor partial pressure. If this argument is
!                          not supplied, the mandatory input PPMV argument is
!                          assumed to be water vapor.
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as input Pressure
!                          ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! PROCEDURE:
!       To convert volume mixing ratio in ppmv of a molecular species,
!       designated by MOL, to partial pressure, the following is used,
!
!         pp(MOL) = 1.0e-06 . ppmv(MOL) . ( Pressure - pp(H2O) )
!
!       If the input molecule is water vapor, the partial pressure is
!       determined using,
!                                                                 1
!         pp(H2O) = 1.0e-06 . ppmv(H2O) . Pressure . -----------------------------
!                                                     1 + ( 1.0e-06 . ppmv(H20) )
!
!       This is based on the same methods used in the LBLRTM conversion code
!       in its lblatm.f module where molecular mixing ratios (either ppmv or
!       g/kg) are always with respect to DRY AIR.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 03-May-2000
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE PPMV_to_PP( &
    Pressure        , &  ! Input
    ppmv            , &  ! Input
    Partial_Pressure, &  ! Output
    Water_Vapor       )  ! Optional Input
    ! Arguments
    REAL(fp),           INTENT(IN)  :: Pressure
    REAL(fp),           INTENT(IN)  :: ppmv
    REAL(fp),           INTENT(OUT) :: Partial_Pressure
    REAL(fp), OPTIONAL, INTENT(IN)  :: Water_Vapor
    ! Local variables
    REAL(fp) :: ppv
    REAL(fp) :: Dry_Air_Pressure

    ! Setup
    IF ( Pressure < ZERO .OR. ppmv < ZERO ) THEN
      Partial_Pressure = ZERO
      RETURN
    ENDIF
    IF ( PRESENT(Water_Vapor) ) THEN
      IF ( Water_Vapor < ZERO ) THEN
        Partial_Pressure = ZERO
        RETURN
      ENDIF
    END IF


    ! Convert input to parts-per
    ppv = PPMV_TO_PPV * ppmv


    ! Calculate the dry air partial pressure
    IF ( PRESENT(Water_Vapor) ) THEN
      ! ...Gas other than water vapour
      Dry_Air_Pressure = Pressure - Water_Vapor
    ELSE
      ! ...Water vapour
      Dry_Air_Pressure = Pressure * ( ONE / ( ONE + ppv ) )
    END IF


    ! Calculate the partial pressure
    Partial_Pressure = ppv * Dry_Air_Pressure

  END SUBROUTINE PPMV_to_PP


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       PPMV_to_PP_TL
!
! PURPOSE:
!       Tangent-linear form of elemental subroutine to convert gas
!       concentrations from volume mixing ratio in ppmv to partial
!       pressure.
!
! CALLING SEQUENCE:
!       CALL PPMV_to_PP_TL( Pressure                       , &  ! FWD Input
!                           ppmv                           , &  ! FWD Input
!                           Pressure_TL                    , &  ! TL  Input
!                           ppmv_TL                        , &  ! TL  Input
!                           Partial_Pressure_TL            , &  ! TL  Output
!                           Water_Vapor    = Water_Vapor   , &  ! Optional FWD Input
!                           Water_Vapor_TL = Water_Vapor_TL  )  ! Optional TL  Input
!
! INPUTS:
!       Pressure:             Total atmospheric pressure.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or and rank
!                             ATTRIBUTES: INTENT(IN)
!
!       ppmv:                 Gas volume mixing ratio in ppmv.
!                             UNITS:      ppmv
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
!       ppmv_TL:              Tangent-linear gas volume mixing ratio.
!                             UNITS:      ppmv
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Partial_Pressure_TL:  Tangent-linear gas partial pressure.
!                             Set to 0.0 for invalid input
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Water_Vapor:          Water vapor partial pressure. If this argument
!                             is not supplied, the mandatory input PPMV and
!                             PPMV_TL arguments are assumed to be for water
!                             vapor.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Water_Vapor_TL:       Tangent-linear water vapor partial pressure.
!                             This argument is ignored if the optional
!                             Water_Vapor argument is not supplied also.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 21-Sep-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE PPMV_to_PP_TL( &
    Pressure           , &  ! FWD Input
    ppmv               , &  ! FWD Input
    Pressure_TL        , &  ! TL  Input
    ppmv_TL            , &  ! TL  Input
    Partial_Pressure_TL, &  ! TL  Output
    Water_Vapor        , &  ! Optional FWD Input
    Water_Vapor_TL       )  ! Optional TL  Input
    ! Arguments
    REAL(fp),           INTENT(IN)  :: Pressure
    REAL(fp),           INTENT(IN)  :: ppmv
    REAL(fp),           INTENT(IN)  :: Pressure_TL
    REAL(fp),           INTENT(IN)  :: ppmv_TL
    REAL(fp),           INTENT(OUT) :: Partial_Pressure_TL
    REAL(fp), OPTIONAL, INTENT(IN)  :: Water_Vapor
    REAL(fp), OPTIONAL, INTENT(IN)  :: Water_Vapor_TL
    ! Local variables
    REAL(fp) :: wv_TL
    REAL(fp) :: ppv, ppv_TL
    REAL(fp) :: Dry_Air_Pressure, Dry_Air_Pressure_TL

    ! Setup
    IF ( Pressure < ZERO .OR. ppmv < ZERO ) THEN
      Partial_Pressure_TL = ZERO
      RETURN
    ENDIF
    IF ( PRESENT(Water_Vapor) ) THEN
      IF ( Water_Vapor < ZERO ) THEN
        Partial_Pressure_TL = ZERO
        RETURN
      END IF
      ! ...Local tangent-linear variable assignment
      IF ( PRESENT(Water_Vapor_TL) ) THEN
        wv_TL = Water_Vapor_TL
      ELSE
        wv_TL = ZERO
      END IF
    END IF


    ! Forward calculations
    ! ...Convert FWD input to parts-per
    ppv = PPMV_TO_PPV * ppmv
    ! ...Calculate the dry air partial pressure
    IF ( PRESENT(Water_Vapor) ) THEN
      Dry_Air_Pressure = Pressure - Water_Vapor
    ELSE
      Dry_Air_Pressure = Pressure * (ONE / (ONE + ppv))
    END IF


    ! Tangent-linear form of conversion to parts-per
    ppv_TL = PPMV_TO_PPV * ppmv_TL


    ! Tangent-linear form of dry air partial pressure
    IF ( PRESENT(Water_Vapor) ) THEN
      ! ...Gas other than water vapour
      Dry_Air_Pressure_TL = Pressure_TL - wv_TL
    ELSE
      ! ...Water vapour
      Dry_Air_Pressure_TL = (((ONE + ppv) * Pressure_TL) - (Pressure * ppv_TL))/(ONE + ppv)**2
    END IF


    ! Tangent-linear form of the partial pressure
    Partial_Pressure_TL = (ppv * Dry_Air_Pressure_TL) + (ppv_TL * Dry_Air_Pressure)

  END SUBROUTINE PPMV_to_PP_TL


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       PPMV_to_PP_AD
!
! PURPOSE:
!       Adjoint form of elemental subroutine to convert gas
!       concentrations from volume mixing ratio in ppmv to partial
!       pressure.
!
! CALLING SEQUENCE:
!       CALL PPMV_to_PP_AD( Pressure                       , &  ! FWD Input
!                           ppmv                           , &  ! FWD Input
!                           Partial_Pressure_AD            , &  ! AD  Input
!                           Pressure_AD                    , &  ! AD  Output
!                           ppmv_AD                        , &  ! AD  Output
!                           Water_Vapor    = Water_Vapor   , &  ! Optional FWD Input
!                           Water_Vapor_AD = Water_Vapor_AD  )  ! Optional AD  Output
!
! INPUTS:
!       Pressure:             Total atmospheric pressure.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or and rank
!                             ATTRIBUTES: INTENT(IN)
!
!       ppmv:                 Gas volume mixing ratio in ppmv.
!                             UNITS:      ppmv
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Partial_Pressure_AD:  Adjoint gas partial pressure.
!                             *** SET TO ZERO ON EXIT ***
!                             UNITS:      hectoPascals, hPa
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
!       ppmv_AD:              Adjoint gas volume mixing ratio.
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      ppmv
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       Water_Vapor:          Water vapor partial pressure. If this argument
!                             is not supplied, the mandatory input PPMV and
!                             PPMV_TL arguments are assumed to be for water
!                             vapor.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       Water_Vapor_AD:       Adjoint water vapor partial pressure.
!                             This argument is ignored if the optional
!                             Water_Vapor argument is not supplied also.
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: OPTIONAL, INTENT(IN OUT)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 21-Sep-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE PPMV_to_PP_AD( &
    Pressure           , &  ! FWD Input
    ppmv               , &  ! FWD Input
    Partial_Pressure_AD, &  ! AD  Input
    Pressure_AD        , &  ! AD  Output
    ppmv_AD            , &  ! AD  Output
    Water_Vapor        , &  ! Optional FWD Input
    Water_Vapor_AD       )  ! Optional AD  Output
    ! Arguments
    REAL(fp),           INTENT(IN)     :: Pressure
    REAL(fp),           INTENT(IN)     :: ppmv
    REAL(fp),           INTENT(IN OUT) :: Partial_Pressure_AD
    REAL(fp),           INTENT(IN OUT) :: Pressure_AD
    REAL(fp),           INTENT(IN OUT) :: ppmv_AD
    REAL(fp), OPTIONAL, INTENT(IN)     :: Water_Vapor
    REAL(fp), OPTIONAL, INTENT(IN OUT) :: Water_Vapor_AD
    ! Local variables
    REAL(fp) :: wv_AD
    REAL(fp) :: ppv, ppv_AD
    REAL(fp) :: Dry_Air_Pressure, Dry_Air_Pressure_AD

    ! Setup
    IF ( Pressure < ZERO .OR. ppmv < ZERO ) THEN
      Partial_Pressure_AD = ZERO
      Pressure_AD         = ZERO
      ppmv_AD             = ZERO
      RETURN
    ENDIF
    IF ( PRESENT(Water_Vapor) ) THEN
      IF ( Water_Vapor < ZERO ) THEN
        Partial_Pressure_AD = ZERO
        Pressure_AD         = ZERO
        ppmv_AD             = ZERO
        RETURN
      END IF
      ! ...Local adjoint variable assignment
      IF ( PRESENT(Water_Vapor_AD) ) THEN
        wv_AD = Water_Vapor_AD
      ELSE
        wv_AD = ZERO
      END IF
    END IF


    ! Forward calculations
    ! ...Convert FWD input to parts-per
    ppv = PPMV_TO_PPV * ppmv
    ! ...Calculate the dry air partial pressure
    IF ( PRESENT(Water_Vapor) ) THEN
      Dry_Air_Pressure = Pressure - Water_Vapor
    ELSE
      Dry_Air_Pressure = Pressure * (ONE / (ONE + ppv))
    END IF


    ! Adjoint form of partial pressure calculation
    ppv_AD              = Dry_Air_Pressure * Partial_Pressure_AD
    Dry_Air_Pressure_AD = ppv              * Partial_Pressure_AD
    Partial_Pressure_AD = ZERO


    ! Adjoint form of dry air pressure calculation
    IF ( PRESENT(Water_Vapor) ) THEN
      ! ...Gas other than water vapor
      wv_AD       = wv_AD       - Dry_Air_Pressure_AD
      Pressure_AD = Pressure_AD + Dry_Air_Pressure_AD
      ! ...Save local mods to Water_Vapor_AD argument
      IF ( PRESENT(Water_Vapor_AD) ) Water_Vapor_AD = wv_AD
    ELSE
      ! ...Water vapour
      ppv_AD      = ppv_AD      - (Dry_Air_Pressure_AD * Pressure / (ONE + ppv)**2)
      Pressure_AD = Pressure_AD + (Dry_Air_Pressure_AD / (ONE + ppv))
    END IF


    ! Adjoint form of part-per conversion
    ppmv_AD = ppmv_AD + (PPMV_TO_PPV * ppv_AD)

  END SUBROUTINE PPMV_to_PP_AD


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       PP_to_PPMV
!
! PURPOSE:
!       Elemental subroutine to convert gas concentrations from partial
!       pressure to volume mixing ratio in ppmv.
!
! CALLING SEQUENCE:
!       CALL PP_to_PPMV( Pressure                 , &  ! Input
!                        Partial_Pressure         , &  ! Input
!                        ppmv                     , &  ! Input
!                        Water_Vapor = Water_Vapor  )  ! Optional Input
!
! INPUTS:
!       Pressure:          Total atmospheric pressure.
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar or any rank
!                          ATTRIBUTES: INTENT(IN)
!
!       Partial_Pressure:  Gas partial pressure
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as pressure
!                          ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       ppmv:              Gas volume mixing ratio.
!                          Set to 0.0 if input < 0.0
!                          UNITS:      ppmv
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as input Pressure
!                          ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Water_Vapor:       Water vapor volume mixing ratio. If this argument is
!                          not supplied, the mandatory Partial_Pressure argument
!                          is assumed to be water vapor.
!                          UNITS:      ppmv
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Pressure
!                          ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! PROCEDURE:
!       To convert the partial pressure of a molecular species, designated
!       by MOL, to volume mixing ratio in ppmv, the following is used,
!
!                                     pp(MOL)
!         ppmv(MOL) = 1.0e+06 . ------------------
!                                Dry_Air_Pressure
!
!       where
!                                                     1
!         Dry_Air_Pressure = Pressure . ----------------------------
!                                        1 + ( ppmv(H2O) . 1.0e-6 )
!
!       If the input molecule is water vapor, the dry air pressure is
!       determined simply using,
!
!         Dry_Air_Pressure = Pressure - pp(H2O)
!
!       This is based on the same methods used in the LBLRTM conversion code
!       in its lblatm.f module where molecular mixing ratios (either ppmv or
!       g/kg) are always with respect to DRY AIR.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 03-May-2000
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE PP_to_PPMV( &
    Pressure,         &  ! Input
    Partial_Pressure, &  ! Input
    ppmv            , &  ! Output
    Water_Vapor       )  ! Optional Input
    ! Arguments
    REAL(fp),           INTENT(IN)  :: Pressure
    REAL(fp),           INTENT(IN)  :: Partial_Pressure
    REAL(fp),           INTENT(OUT) :: ppmv
    REAL(fp), OPTIONAL, INTENT(IN)  :: Water_Vapor
    ! Local variables
    REAL(fp) :: ppv
    REAL(fp) :: Dry_Air_Pressure

    ! Setup
    IF ( Pressure < ZERO .OR. Partial_Pressure < ZERO ) THEN
      ppmv = ZERO
      RETURN
    ENDIF
    IF ( PRESENT(Water_Vapor) ) THEN
      IF ( Water_Vapor < ZERO ) THEN
        ppmv = ZERO
        RETURN
      ENDIF
    END IF


    ! Calculate the dry air partial pressure
    IF ( PRESENT(Water_Vapor) ) THEN
      ! ...Gas other than water vapor
      ppv = PPMV_TO_PPV * Water_Vapor
      Dry_Air_Pressure = Pressure * ( ONE / ( ONE + ppv ) )
    ELSE
      ! ...Water vapour
      Dry_Air_Pressure = Pressure - Partial_Pressure
    END IF


    ! Calculate the volume mixing ratio
    ppmv = PPV_TO_PPMV * Partial_Pressure / Dry_Air_Pressure

  END SUBROUTINE PP_to_PPMV


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       PP_to_PPMV_TL
!
! PURPOSE:
!       Tangent-linear form of elemental subroutine to convert gas
!       concentrations from partial pressure to volume mixing ratio
!       in ppmv.
!
! CALLING SEQUENCE:
!       CALL PP_to_PPMV_TL( Pressure                       , &  ! FWD Input
!                           Partial_Pressure               , &  ! FWD Input
!                           Pressure_TL                    , &  ! TL  Input
!                           Partial_Pressure_TL            , &  ! TL  Input
!                           ppmv_TL                        , &  ! TL  Output
!                           Water_Vapor    = Water_Vapor   , &  ! Optional FWD Input
!                           Water_Vapor_TL = Water_Vapor_TL  )  ! Optional TL  Input
!
! INPUTS:
!       Pressure:             Total atmospheric pressure.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN)
!
!       Partial_Pressure:     Gas partial pressure
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Pressure_TL:          Tangent-linear total atmospheric pressure.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Partial_Pressure_TL:  Tangent-linear gas partial pressure
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       ppmv_TL:              Tangent-linear gas volume mixing ratio.
!                             Set to 0.0 if invalid input
!                             UNITS:      ppmv
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Water_Vapor:          Water vapor partial pressure. If this argument
!                             is not supplied, the mandatory input PPMV and
!                             PPMV_TL arguments are assumed to be for water
!                             vapor.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Water_Vapor_TL:       Tangent-linear water vapor partial pressure.
!                             This argument is ignored if the optional
!                             Water_Vapor argument is not supplied also.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 21-Sep-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE PP_to_PPMV_TL( &
    Pressure           , &  ! FWD Input
    Partial_Pressure   , &  ! FWD Input
    Pressure_TL        , &  ! TL  Input
    Partial_Pressure_TL, &  ! TL  Input
    ppmv_TL            , &  ! TL  Output
    Water_Vapor        , &  ! Optional FWD Input
    Water_Vapor_TL       )  ! Optional TL  Input
    ! Arguments
    REAL(fp),           INTENT(IN)  :: Pressure
    REAL(fp),           INTENT(IN)  :: Partial_Pressure
    REAL(fp),           INTENT(IN)  :: Pressure_TL
    REAL(fp),           INTENT(IN)  :: Partial_Pressure_TL
    REAL(fp),           INTENT(OUT) :: ppmv_TL
    REAL(fp), OPTIONAL, INTENT(IN)  :: Water_Vapor
    REAL(fp), OPTIONAL, INTENT(IN)  :: Water_Vapor_TL
    ! Local variables
    REAL(fp) :: wv_TL
    REAL(fp) :: ppv, ppv_TL
    REAL(fp) :: Dry_Air_Pressure, Dry_Air_Pressure_TL

    ! Setup
    IF ( Pressure < ZERO .OR. Partial_Pressure < ZERO ) THEN
      ppmv_TL = ZERO
      RETURN
    ENDIF
    IF ( PRESENT(Water_Vapor) ) THEN
      IF ( Water_Vapor < ZERO ) THEN
        ppmv_TL = ZERO
        RETURN
      ENDIF
      ! ...Local tangent-linear variable assignment
      IF ( PRESENT(Water_Vapor_TL) ) THEN
        wv_TL = Water_Vapor_TL
      ELSE
        wv_TL = ZERO
      END IF
    END IF


    ! Tangent-linear form of dry air partial pressure
    IF ( PRESENT(Water_Vapor) ) THEN
      ! ...Gas other than water vapor
      ppv = PPMV_TO_PPV * Water_Vapor
      Dry_Air_Pressure = Pressure * (ONE / (ONE + ppv))
      ppv_TL = PPMV_TO_PPV * wv_TL
      Dry_Air_Pressure_TL = (((ONE + ppv) * Pressure_TL) - (Pressure * ppv_TL))/(ONE + ppv)**2
    ELSE
      ! ...Water vapour
      Dry_Air_Pressure = Pressure - Partial_Pressure
      Dry_Air_Pressure_TL = Pressure_TL - Partial_Pressure_TL
    END IF


    ! Tangent-linear form of volume mixing ratio calculation
    ppmv_TL = PPV_TO_PPMV * ((Dry_Air_Pressure * Partial_Pressure_TL) - &
                             (Partial_Pressure * Dry_Air_Pressure_TL)) / Dry_Air_Pressure**2

  END SUBROUTINE PP_to_PPMV_TL


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       PP_to_PPMV_AD
!
! PURPOSE:
!       Adjoint form of elemental subroutine to convert gas
!       concentrations from partial pressure to volume mixing ratio
!       in ppmv.
!
! CALLING SEQUENCE:
!       CALL PP_to_PPMV_AD( Pressure                       , &  ! FWD Input
!                           Partial_Pressure               , &  ! FWD Input
!                           ppmv_AD                        , &  ! AD  Input
!                           Pressure_AD                    , &  ! AD  Output
!                           Partial_Pressure_AD            , &  ! AD  Output
!                           Water_Vapor    = Water_Vapor   , &  ! Optional FWD Input
!                           Water_Vapor_AD = Water_Vapor_AD  )  ! Optional AD  Output
!
! INPUTS:
!       Pressure:             Total atmospheric pressure.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN)
!
!       Partial_Pressure:     Gas partial pressure
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       ppmv_AD:              Adjoint gas volume mixing ratio.
!                             *** SET TO ZERO ON EXIT ***
!                             UNITS:      ppmv
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUTS:
!       Pressure_AD:          Adjoint total atmospheric pressure.
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN OUT)
!
!       Partial_Pressure_AD:  Adjoint gas partial pressure
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       Water_Vapor:          Water vapor partial pressure. If this argument
!                             is not supplied, the mandatory input PPMV and
!                             PPMV_AD arguments are assumed to be for water
!                             vapor.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       Water_Vapor_AD:       Adjoint water vapor partial pressure.
!                             This argument is ignored if the optional
!                             Water_Vapor argument is not supplied also.
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: OPTIONAL, INTENT(IN OUT)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 21-Sep-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE PP_to_PPMV_AD( &
    Pressure           , &  ! FWD Input
    Partial_Pressure   , &  ! FWD Input
    ppmv_AD            , &  ! AD  Input
    Pressure_AD        , &  ! AD  Output
    Partial_Pressure_AD, &  ! AD  Output
    Water_Vapor        , &  ! Optional FWD Input
    Water_Vapor_AD       )  ! Optional AD  Output
    ! Arguments
    REAL(fp),           INTENT(IN)     :: Pressure
    REAL(fp),           INTENT(IN)     :: Partial_Pressure
    REAL(fp),           INTENT(IN OUT) :: ppmv_AD
    REAL(fp),           INTENT(IN OUT) :: Pressure_AD
    REAL(fp),           INTENT(IN OUT) :: Partial_Pressure_AD
    REAL(fp), OPTIONAL, INTENT(IN)     :: Water_Vapor
    REAL(fp), OPTIONAL, INTENT(IN OUT) :: Water_Vapor_AD
    ! Local variables
    REAL(fp) :: wv_AD
    REAL(fp) :: ppv, ppv_AD
    REAL(fp) :: Dry_Air_Pressure, Dry_Air_Pressure_AD

    ! Setup
    IF ( Pressure < ZERO .OR. Partial_Pressure < ZERO ) THEN
      ppmv_AD             = ZERO
      Pressure_AD         = ZERO
      Partial_Pressure_AD = ZERO
      RETURN
    ENDIF
    IF ( PRESENT(Water_Vapor) ) THEN
      IF ( Water_Vapor < ZERO ) THEN
        ppmv_AD             = ZERO
        Pressure_AD         = ZERO
        Partial_Pressure_AD = ZERO
        RETURN
      ENDIF
      ! ...Local adjoint variable assignment
      IF ( PRESENT(Water_Vapor_AD) ) THEN
        wv_AD = Water_Vapor_AD
      ELSE
        wv_AD = ZERO
      END IF
    END IF


    ! Forward calculations
    IF ( PRESENT(Water_Vapor) ) THEN
      ! ...Gas other than water vapor
      ppv = PPMV_TO_PPV * Water_Vapor
      Dry_Air_Pressure = Pressure * (ONE / (ONE + ppv))
    ELSE
      ! ...Water vapour
      Dry_Air_Pressure = Pressure - Partial_Pressure
    END IF


    ! Adjoint form of volume mixing ratio calculation
    Dry_Air_Pressure_AD = -(PPV_TO_PPMV * Partial_Pressure * ppmv_AD / Dry_Air_Pressure**2)
    Partial_Pressure_AD = Partial_Pressure_AD + (PPV_TO_PPMV * ppmv_AD / Dry_Air_Pressure)
    ppmv_AD = ZERO


    ! Adjoint form of dry air partial pressure
    IF ( PRESENT(Water_Vapor) ) THEN
      ! ...Gas other than water vapour
      ppv_AD      = -(Pressure * Dry_Air_Pressure_AD / (ONE + ppv)**2)
      Pressure_AD = Pressure_AD + (Dry_Air_Pressure_AD / (ONE + ppv))
      wv_AD       = wv_AD + PPMV_TO_PPV*ppv_AD
      ! ...Save local mods to Water_Vapor_AD argument
      IF ( PRESENT(Water_Vapor_AD) ) Water_Vapor_AD = wv_AD
    ELSE
      ! ...Water vapor
      Partial_Pressure_AD = Partial_Pressure_AD - Dry_Air_Pressure_AD
      Pressure_AD         = Pressure_AD         + Dry_Air_Pressure_AD
    END IF

  END SUBROUTINE PP_to_PPMV_AD

END MODULE PPMV_PP
