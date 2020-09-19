!
! PPMV_ND
!
! Module containing forward, tangent-linear and adjoint subroutines
! for volume mixing ratio to/from number density units conversion.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 26-Sep-2009
!                       paul.vandelst@noaa.gov
!

MODULE PPMV_ND

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Modules used
  USE Type_Kinds, ONLY: fp
  USE Profile_Utility_Parameters, ONLY: ZERO, ONE, &
                                        PPV_TO_PPMV, &
                                        PPMV_to_PPV
  USE PP_ND
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: PPMV_to_ND, PPMV_to_ND_TL, PPMV_to_ND_AD
  PUBLIC :: ND_to_PPMV, ND_to_PPMV_TL, ND_to_PPMV_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &


CONTAINS


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       PPMV_to_ND
!
! PURPOSE:
!       Elemental subroutine to convert gas concentrations from volume
!       mixing ratio in ppmv to number density.
!
! CALLING SEQUENCE:
!       CALL PPMV_to_ND( Pressure                 , &  ! Input
!                        Temperature              , &  ! Input
!                        ppmv                     , &  ! Input
!                        Number_Density           , &  ! Output
!                        Water_Vapor = Water_Vapor  )  ! Optional Input
!
! INPUTS:
!       Pressure:          Total atmospheric pressure.
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar or and rank
!                          ATTRIBUTES: INTENT(IN)
!
!       Temperature:       Atmospheric temperature.
!                          UNITS:      Kelvin, K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Pressure
!                          ATTRIBUTES: INTENT(IN)
!
!       ppmv:              Gas volume mixing ratio in ppmv.
!                          UNITS:      ppmv
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Pressure
!                          ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Number_Density:    Gas number density.
!                          Set to 0.0 if input < 0.0
!                          UNITS:      molecules/m^3
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as input Pressure
!                          ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Water_Vapor:       Water vapor number density. If this argument is
!                          not supplied, the mandatory input PPMV argument is
!                          assumed to be water vapor.
!                          UNITS:      molecules/m^3
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as input Pressure
!                          ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! PROCEDURE:
!       To convert the volume mixing ratio of a molecular species, designated
!       by MOL, to number density, the following is used,
!
!         nd(MOL) = 1.0e-06 . ppmv(MOL) . ( nd(TOT) - nd(H2O) )     .....(1)
!
!       where
!                         Pressure          T0
!         nd(TOT) = L0 . ---------- . -------------  molecules/m^3
!                            p0        Temperature
!
!       and L0 = Loschmidt number,
!           p0 = Standard pressure,
!           T0 = Standard temperature.
!
!       If the input molecule is water vapor, the number density is
!       determined using,
!
!                        ppmv(H2O) . 1.0e-06
!         nd(H2O) = ----------------------------- . nd(TOT)     .....(2)
!                    1 + ( ppmv(H2O) . 1.0e-06 )
!
!       Rearranging eqn.(2) gives the same form as eqn.(1).
!
!       This is based on the same methods used in the LBLRTM conversion code
!       in its lblatm.f module where molecular mixing ratios (either ppmv or
!       g/kg) are always with respect to DRY AIR.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 15-Nov-2001
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE PPMV_to_ND( &
    Pressure      , &  ! Input
    Temperature   , &  ! Input
    ppmv          , &  ! Input
    Number_Density, &  ! Output
    Water_Vapor     )  ! Optional Input
    ! Arguments
    REAL(fp),           INTENT(IN)  :: Pressure
    REAL(fp),           INTENT(IN)  :: Temperature
    REAL(fp),           INTENT(IN)  :: ppmv
    REAL(fp),           INTENT(OUT) :: Number_Density
    REAL(fp), OPTIONAL, INTENT(IN)  :: Water_Vapor
    ! Local variables
    REAL(fp) :: ppv
    REAL(fp) :: Total_Density

    ! Setup
    Number_Density = ZERO
    IF ( Pressure < ZERO .OR. Temperature < ZERO .OR. ppmv < ZERO ) RETURN
    IF ( PRESENT(Water_Vapor) ) THEN
      IF ( Water_Vapor < ZERO ) RETURN
    END IF


    ! Convert input to parts-per
    ppv = PPMV_to_PPV * ppmv


    ! Calculate air number density
    CALL PP_to_ND( Temperature, Pressure, Total_Density )


    ! Calculate the molecular number density
    IF ( PRESENT(Water_Vapor) ) THEN
      ! ...Gas other than water vapour
      Number_Density = ppv * (Total_Density - Water_Vapor)
    ELSE
      ! ...Water vapour
      Number_Density = (ppv / (ONE + ppv)) * Total_Density
    END IF

  END SUBROUTINE PPMV_to_ND


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       PPMV_to_ND_TL
!
! PURPOSE:
!       Tangent-linear form of elemental subroutine to convert gas
!       concentrations from volume mixing ratio in ppmv to number
!       density.
!
! CALLING SEQUENCE:
!       CALL PPMV_to_ND_TL( Pressure                       , &  ! FWD Input
!                           Temperature                    , &  ! FWD Input
!                           ppmv                           , &  ! FWD Input
!                           Pressure_TL                    , &  ! TL  Input
!                           Temperature_TL                 , &  ! TL  Input
!                           ppmv_TL                        , &  ! TL  Input
!                           Number_Density_TL              , &  ! TL  Output
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
!       Temperature:          Atmospheric temperature.
!                             UNITS:      Kelvin, K
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
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
!       Temperature_TL:       Tangent-linear atmospheric temperature.
!                             UNITS:      Kelvin, K
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
!       Number_Density_TL:    Tangent-linear gas number density.
!                             Set to 0.0 for invalid input
!                             UNITS:      molecules/m^3
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Water_Vapor:          Water vapor number density. If this argument
!                             is not supplied, the mandatory input PPMV and
!                             PPMV_TL arguments are assumed to be for water
!                             vapor.
!                             UNITS:      molecules/m^3
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Water_Vapor_TL:       Tangent-linear water vapor number density.
!                             This argument is ignored if the optional
!                             Water_Vapor argument is not supplied also.
!                             UNITS:      molecules/m^3
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 26-Sep-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE PPMV_to_ND_TL( &
    Pressure         , &  ! FWD Input
    Temperature      , &  ! FWD Input
    ppmv             , &  ! FWD Input
    Pressure_TL      , &  ! TL  Input
    Temperature_TL   , &  ! TL  Input
    ppmv_TL          , &  ! TL  Input
    Number_Density_TL, &  ! TL  Output
    Water_Vapor      , &  ! Optional FWD Input
    Water_Vapor_TL     )  ! Optional TL  Input
    ! Arguments
    REAL(fp),           INTENT(IN)  :: Pressure
    REAL(fp),           INTENT(IN)  :: Temperature
    REAL(fp),           INTENT(IN)  :: ppmv
    REAL(fp),           INTENT(IN)  :: Pressure_TL
    REAL(fp),           INTENT(IN)  :: Temperature_TL
    REAL(fp),           INTENT(IN)  :: ppmv_TL
    REAL(fp),           INTENT(OUT) :: Number_Density_TL
    REAL(fp), OPTIONAL, INTENT(IN)  :: Water_Vapor
    REAL(fp), OPTIONAL, INTENT(IN)  :: Water_Vapor_TL
    ! Local variables
    REAL(fp) :: wv_TL
    REAL(fp) :: ppv, ppv_TL
    REAL(fp) :: Total_Density, Total_Density_TL

    ! Setup
    Number_Density_TL = ZERO
    IF ( Pressure < ZERO .OR. Temperature < ZERO .OR. ppmv < ZERO ) RETURN
    IF ( PRESENT(Water_Vapor) ) THEN
      IF ( Water_Vapor < ZERO ) RETURN
      ! ...Local tangent-linear variable assignment
      IF ( PRESENT(Water_Vapor_TL) ) THEN
        wv_TL = Water_Vapor_TL
      ELSE
        wv_TL = ZERO
      END IF
    END IF

    ! Convert ppmv input to parts-per
    ppv = PPMV_to_PPV * ppmv
    ppv_TL = PPMV_to_PPV * ppmv_TL

    ! Calculate air number density
    CALL PP_to_ND( Temperature, Pressure, Total_Density )
    CALL PP_to_ND_TL( Temperature, Pressure, Temperature_TL, Pressure_TL, Total_Density_TL )

    ! Tangent-linear form of number density calculation
    IF ( PRESENT(Water_Vapor) ) THEN
      ! ...Gas other than water vapour
      Number_Density_TL = ((Total_Density - Water_Vapor) * ppv_TL) + &
                          (ppv * Total_Density_TL) - &
                          (ppv * wv_TL)
    ELSE
      ! ...Water vapour
      Number_Density_TL = (ppv * Total_Density_TL / (ONE + ppv)) + &
                          (Total_Density * ppv_TL / (ONE + ppv)**2)
    END IF

  END SUBROUTINE PPMV_to_ND_TL


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       PPMV_to_ND_AD
!
! PURPOSE:
!       Adjoint form of elemental subroutine to convert gas
!       concentrations from volume mixing ratio in ppmv to number
!       density.
!
! CALLING SEQUENCE:
!       CALL PPMV_to_ND_AD( Pressure                       , &  ! FWD Input
!                           Temperature                    , &  ! FWD Input
!                           ppmv                           , &  ! FWD Input
!                           Number_Density_AD              , &  ! AD  Input
!                           Pressure_AD                    , &  ! AD  Output
!                           Temperature_AD                 , &  ! AD  Output
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
!       Temperature:          Atmospheric temperature.
!                             UNITS:      Kelvin, K
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       ppmv:                 Gas volume mixing ratio in ppmv.
!                             UNITS:      ppmv
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Number_Density_AD:    Adjoint gas number density.
!                             *** SET TO ZERO ON EXIT ***
!                             UNITS:      molecules/m^3
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
!       ppmv_AD:              Adjoint gas volume mixing ratio.
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      ppmv
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       Water_Vapor:          Water vapor number density. If this argument
!                             is not supplied, the mandatory input PPMV and
!                             PPMV_TL arguments are assumed to be for water
!                             vapor.
!                             UNITS:      molecules/m^3
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       Water_Vapor_AD:       Adjoint water vapor number density.
!                             This argument is ignored if the optional
!                             Water_Vapor argument is not supplied also.
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      molecules/m^3
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: OPTIONAL, INTENT(IN OUT)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 26-Sep-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE PPMV_to_ND_AD( &
    Pressure         , &  ! FWD Input
    Temperature      , &  ! FWD Input
    ppmv             , &  ! FWD Input
    Number_Density_AD, &  ! AD  Input
    Pressure_AD      , &  ! AD  Output
    Temperature_AD   , &  ! AD  Output
    ppmv_AD          , &  ! AD  Output
    Water_Vapor      , &  ! Optional FWD Input
    Water_Vapor_AD     )  ! Optional AD  Output
    ! Arguments
    REAL(fp),           INTENT(IN)     :: Pressure
    REAL(fp),           INTENT(IN)     :: Temperature
    REAL(fp),           INTENT(IN)     :: ppmv
    REAL(fp),           INTENT(IN OUT) :: Number_Density_AD
    REAL(fp),           INTENT(IN OUT) :: Pressure_AD
    REAL(fp),           INTENT(IN OUT) :: Temperature_AD
    REAL(fp),           INTENT(IN OUT) :: ppmv_AD
    REAL(fp), OPTIONAL, INTENT(IN)     :: Water_Vapor
    REAL(fp), OPTIONAL, INTENT(IN OUT) :: Water_Vapor_AD
    ! Local variables
    REAL(fp) :: wv_AD
    REAL(fp) :: ppv, ppv_AD
    REAL(fp) :: Total_Density, Total_Density_AD

    ! Setup
    IF ( Pressure < ZERO .OR. Temperature < ZERO .OR. ppmv < ZERO ) THEN
      Number_Density_AD = ZERO
      Pressure_AD       = ZERO
      Temperature_AD    = ZERO
      ppmv_AD           = ZERO
      RETURN
    ENDIF
    IF ( PRESENT(Water_Vapor) ) THEN
      IF ( Water_Vapor < ZERO ) THEN
        Number_Density_AD = ZERO
        Pressure_AD       = ZERO
        Temperature_AD    = ZERO
        ppmv_AD           = ZERO
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
    ppv = PPMV_to_PPV * ppmv
    CALL PP_to_ND( Temperature, Pressure, Total_Density )

    ! Adjoint form of number density calculation
    IF ( PRESENT(Water_Vapor) ) THEN
      ! ...Gas other than water vapour
      wv_AD            = wv_AD - (ppv * Number_Density_AD)
      Total_Density_AD = ppv * Number_Density_AD
      ppv_AD           = (Total_Density - Water_Vapor) * Number_Density_AD
      ! ...Save local mods to Water_Vapor_AD argument
      IF ( PRESENT(Water_Vapor_AD) ) Water_Vapor_AD = wv_AD
    ELSE
      ! ...Water vapour
      ppv_AD           = Total_Density * Number_Density_AD / (ONE + ppv)**2
      Total_Density_AD = ppv * Number_Density_AD / (ONE + ppv)
    END IF
    Number_Density_AD = ZERO

    ! Adjoint form of air number density
    CALL PP_to_ND_AD( Temperature, Pressure, Total_Density_AD, Temperature_AD, Pressure_AD )


    ! Adjoint form of ppmv to parts-per conversion
    ppmv_AD = ppmv_AD + (PPMV_to_PPV * ppv_AD)

  END SUBROUTINE PPMV_to_ND_AD


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       ND_to_PPMV
!
! PURPOSE:
!       Elemental subroutine to convert gas concentrations from number
!       density to volume mixing ratio in ppmv.
!
! CALLING SEQUENCE:
!       CALL ND_to_PPMV( Pressure                 , &  ! Input
!                        Temperature              , &  ! Input
!                        Number_Density           , &  ! Input
!                        ppmv                     , &  ! Output
!                        Water_Vapor = Water_Vapor  )  ! Optional Input
!
! INPUTS:
!       Pressure:          Total atmospheric pressure.
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar or any rank
!                          ATTRIBUTES: INTENT(IN)
!
!       Temperature:       Atmospheric temperature.
!                          UNITS:      Kelvin, K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Pressure
!                          ATTRIBUTES: INTENT(IN)
!
!       Number_Density:    Gas number density
!                          UNITS:      molecules/m^3
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
!                          not supplied, the mandatory Number_Density argument
!                          is assumed to be water vapor.
!                          UNITS:      ppmv
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Pressure
!                          ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! PROCEDURE:
!       To convert the number density of a molecular species, designated
!       by MOL, to volume mixing ratio, the following is used,
!
!                                  nd(MOL)
!         ppmv(MOL) = 1.0e+06 . -------------
!                                nd(DRY_AIR)
!
!       where
!
!         nd(DRY_AIR) = nd(TOT) - nd(H2O)
!
!       and
!                         Pressure          T0
!         nd(TOT) = L0 . ---------- . -------------  molecules/m^3
!                            p0        Temperature
!
!       with L0 = Loschmidt number,
!            p0 = Standard pressure,
!            T0 = Standard temperature, and
!
!       If the input molecule is NOT water vapor, the dry air number density
!       is determined using,
!
!                                     1
!         nd(DRY_AIR) = ----------------------------- . nd(TOT)
!                        1 + ( ppmv(H2O) . 1.0e-06 )
!
!       This is based on the same methods used in the LBLRTM conversion code
!       in its lblatm.f module where molecular mixing ratios (either ppmv or
!       g/kg) are always with respect to DRY AIR.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 15-Nov-2001
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE ND_to_PPMV( &
    Pressure      , &  ! Input
    Temperature   , &  ! Input
    Number_Density, &  ! Input
    ppmv          , &  ! Output
    Water_Vapor     )  ! Optional Input
    ! Arguments
    REAL(fp),           INTENT(IN)  :: Pressure
    REAL(fp),           INTENT(IN)  :: Temperature
    REAL(fp),           INTENT(IN)  :: Number_Density
    REAL(fp),           INTENT(OUT) :: ppmv
    REAL(fp), OPTIONAL, INTENT(IN)  :: Water_Vapor
    ! Local variables
    REAL(fp) :: ppv
    REAL(fp) :: Total_Density
    REAL(fp) :: Dry_Air_Density

    ! Setup
    ppmv = ZERO
    IF ( Pressure < ZERO .OR. Temperature < ZERO .OR. Number_Density < ZERO ) RETURN
    IF ( PRESENT(Water_Vapor) ) THEN
      IF ( Water_Vapor < ZERO ) RETURN
    END IF

    ! Calculate total number density
    CALL PP_to_ND( Temperature, Pressure, Total_Density )

    ! Calculate the dry air number density
    IF ( PRESENT(Water_Vapor) ) THEN
      ! ...Gas other than water vapor
      ppv = PPMV_TO_PPV * Water_Vapor
      Dry_Air_Density = Total_Density * (ONE / (ONE + ppv))
    ELSE
      ! ...Water vapour
      Dry_Air_Density = Total_Density - Number_Density
    END IF

    ! Calculate the volume mixing ratio
    ppmv = PPV_TO_PPMV * Number_Density / Dry_Air_Density

  END SUBROUTINE ND_to_PPMV


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       ND_to_PPMV_TL
!
! PURPOSE:
!       Tangent-linear form of elemental subroutine to convert gas
!       concentrations from number density to volume mixing ratio
!       in ppmv.
!
! CALLING SEQUENCE:
!       CALL ND_to_PPMV_TL( Pressure                       , &  ! FWD Input
!                           Temperature                    , &  ! FWD Input
!                           Number_Density                 , &  ! FWD Input
!                           Pressure_TL                    , &  ! TL  Input
!                           Temperature_TL                 , &  ! TL  Input
!                           Number_Density_TL              , &  ! TL  Input
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
!       Temperature:          Atmospheric temperature.
!                             UNITS:      Kelvin, K
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Number_Density:       Gas number density
!                             UNITS:      molecules/m^3
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
!       Temperature_TL:       Tangent-linear atmospheric temperature.
!                             UNITS:      Kelvin, K
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Number_Density_TL:    Tangent-linear gas number density
!                             UNITS:      molecules/m^3
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
!       Water_Vapor:          Water vapor volume mixing ratio. If this
!                             argument is not supplied, the mandatory
!                             input NUMBER_DENSITY and NUMBER_DENSITY_TL
!                             arguments are assumed to be for water
!                             vapor.
!                             UNITS:      ppmv
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Water_Vapor_TL:       Tangent-linear water vapor volume mixing ratio.
!                             This argument is ignored if the optional
!                             WATER_VAPOR argument is not supplied also.
!                             UNITS:      ppmv
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 26-Sep-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE ND_to_PPMV_TL( &
    Pressure         , &  ! FWD Input
    Temperature      , &  ! FWD Input
    Number_Density   , &  ! FWD Input
    Pressure_TL      , &  ! TL  Input
    Temperature_TL   , &  ! TL  Input
    Number_Density_TL, &  ! TL  Input
    ppmv_TL          , &  ! TL  Output
    Water_Vapor      , &  ! Optional FWD Input
    Water_Vapor_TL     )  ! Optional TL  Input
    ! Arguments
    REAL(fp),           INTENT(IN)  :: Pressure
    REAL(fp),           INTENT(IN)  :: Temperature
    REAL(fp),           INTENT(IN)  :: Number_Density
    REAL(fp),           INTENT(IN)  :: Pressure_TL
    REAL(fp),           INTENT(IN)  :: Temperature_TL
    REAL(fp),           INTENT(IN)  :: Number_Density_TL
    REAL(fp),           INTENT(OUT) :: ppmv_TL
    REAL(fp), OPTIONAL, INTENT(IN)  :: Water_Vapor
    REAL(fp), OPTIONAL, INTENT(IN)  :: Water_Vapor_TL
    ! Local variables
    REAL(fp) :: wv_TL
    REAL(fp) :: ppv            , ppv_TL
    REAL(fp) :: Total_Density  , Total_Density_TL
    REAL(fp) :: Dry_Air_Density, Dry_Air_Density_TL

    ! Setup
    ppmv_TL = ZERO
    IF ( Pressure < ZERO .OR. Temperature < ZERO .OR. Number_Density < ZERO ) RETURN
    IF ( PRESENT(Water_Vapor) ) THEN
      IF ( Water_Vapor < ZERO ) RETURN
      ! ...Local tangent-linear variable assignment
      IF ( PRESENT(Water_Vapor_TL) ) THEN
        wv_TL = Water_Vapor_TL
      ELSE
        wv_TL = ZERO
      END IF
    END IF

    ! Calculate total number density
    CALL PP_to_ND( Temperature, Pressure, Total_Density )
    CALL PP_to_ND_TL( Temperature, Pressure, Temperature_TL, Pressure_TL, Total_Density_TL )

    ! Tangent-linear form of dry air number density
    IF ( PRESENT(Water_Vapor) ) THEN
      ! ...Gas other than water vapor
      ppv    = PPMV_TO_PPV * Water_Vapor
      ppv_TL = PPMV_TO_PPV * wv_TL
      Dry_Air_Density    = Total_Density * (ONE / (ONE + ppv))
      Dry_Air_Density_TL = (((ONE + ppv) * Total_Density_TL) - (Total_Density * ppv_TL)) / (ONE + ppv)**2
    ELSE
      ! ...Water vapour
      Dry_Air_Density    = Total_Density - Number_Density
      Dry_Air_Density_TL = Total_Density_TL - Number_Density_TL
    END IF

    ! Tangent-linear form of volume mixing ratio
    ppmv_TL = PPV_TO_PPMV * ((Dry_Air_Density * Number_Density_TL) - &
                             (Number_Density * Dry_Air_Density_TL)) / Dry_Air_Density**2

  END SUBROUTINE ND_to_PPMV_TL


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       ND_to_PPMV_AD
!
! PURPOSE:
!       Adjoint form of elemental subroutine to convert gas
!       concentrations from number density to volume mixing ratio
!       in ppmv.
!
! CALLING SEQUENCE:
!       CALL ND_to_PPMV_AD( Pressure                       , &  ! FWD Input
!                           Temperature                    , &  ! FWD Input
!                           Number_Density                 , &  ! FWD Input
!                           ppmv_AD                        , &  ! AD  Input
!                           Pressure_AD                    , &  ! AD  Output
!                           Temperature_AD                 , &  ! AD  Output
!                           Number_Density_AD              , &  ! AD  Output
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
!       Temperature:          Atmospheric temperature.
!                             UNITS:      Kelvin, K
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Number_Density:       Gas number density
!                             UNITS:      molecules/m^3
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
!       Temperature_AD:       Adjoint atmospheric temperature.
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      Kelvin, K
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN OUT)
!
!       Number_Density_AD:    Adjoint gas number density
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      molecules/m^3
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       Water_Vapor:          Water vapor volume mixing ratio. If this
!                             argument is not supplied, the mandatory
!                             input NUMBER_DENSITY and NUMBER_DENSITY_AD
!                             arguments are assumed to be for water vapor.
!                             UNITS:      ppmv
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       Water_Vapor_AD:       Adjoint water vapor volume mixing ratio.
!                             This argument is ignored if the optional
!                             WATER_VAPOR argument is not supplied also.
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      ppmv
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: OPTIONAL, INTENT(IN OUT)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 27-Sep-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE ND_to_PPMV_AD( &
    Pressure         , &  ! FWD Input
    Temperature      , &  ! FWD Input
    Number_Density   , &  ! FWD Input
    ppmv_AD          , &  ! AD  Input
    Pressure_AD      , &  ! AD  Output
    Temperature_AD   , &  ! AD  Output
    Number_Density_AD, &  ! AD  Output
    Water_Vapor      , &  ! Optional FWD Input
    Water_Vapor_AD     )  ! Optional AD  Output
    ! Arguments
    REAL(fp),           INTENT(IN)     :: Pressure
    REAL(fp),           INTENT(IN)     :: Temperature
    REAL(fp),           INTENT(IN)     :: Number_Density
    REAL(fp),           INTENT(IN OUT) :: ppmv_AD
    REAL(fp),           INTENT(IN OUT) :: Pressure_AD
    REAL(fp),           INTENT(IN OUT) :: Temperature_AD
    REAL(fp),           INTENT(IN OUT) :: Number_Density_AD
    REAL(fp), OPTIONAL, INTENT(IN)     :: Water_Vapor
    REAL(fp), OPTIONAL, INTENT(IN OUT) :: Water_Vapor_AD
    ! Local variables
    REAL(fp) :: wv_AD
    REAL(fp) :: ppv            , ppv_AD
    REAL(fp) :: Total_Density  , Total_Density_AD
    REAL(fp) :: Dry_Air_Density, Dry_Air_Density_AD

    ! Setup
    IF ( Pressure < ZERO .OR. Temperature < ZERO .OR. Number_Density < ZERO ) THEN
      ppmv_AD           = ZERO
      Pressure_AD       = ZERO
      Temperature_AD    = ZERO
      Number_Density_AD = ZERO
      RETURN
    ENDIF
    IF ( PRESENT(Water_Vapor) ) THEN
      IF ( Water_Vapor < ZERO ) THEN
        ppmv_AD           = ZERO
        Pressure_AD       = ZERO
        Temperature_AD    = ZERO
        Number_Density_AD = ZERO
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
    CALL PP_to_ND( Temperature, Pressure, Total_Density )
    IF ( PRESENT(Water_Vapor) ) THEN
      ! ...Gas other than water vapor
      ppv = PPMV_TO_PPV * Water_Vapor
      Dry_Air_Density = Total_Density * (ONE / (ONE + ppv))
    ELSE
      ! ...Water vapour
      Dry_Air_Density = Total_Density - Number_Density
    END IF

    ! Adjoint form of volume mixing ratio calculation
    Dry_Air_Density_AD = -(PPV_TO_PPMV * Number_Density * ppmv_AD / Dry_Air_Density**2)
    Number_Density_AD  = Number_Density_AD + (PPV_TO_PPMV * ppmv_AD / Dry_Air_Density)
    ppmv_AD = ZERO

    ! Adjoint form of dry air number density calculation
    IF ( PRESENT(Water_Vapor) ) THEN
      ! ...Gas other than water vapor
      ppv    = PPMV_TO_PPV * Water_Vapor
      ppv_AD           = -(Total_Density * Dry_Air_Density_AD / (ONE + ppv)**2)
      Total_Density_AD = Dry_Air_Density_AD / (ONE + ppv)
      wv_AD            = wv_AD + (PPMV_TO_PPV * ppv_AD)
      ! ...Save local mods to Water_Vapor_AD argument
      IF ( PRESENT(Water_Vapor_AD) ) Water_Vapor_AD = wv_AD
    ELSE
      ! ...Water vapour
      Number_Density_AD = Number_Density_AD - Dry_Air_Density_AD
      Total_Density_AD  = Dry_Air_Density_AD
    END IF

    ! Adjoint form of total number density calculation
    CALL PP_to_ND_AD( Temperature, Pressure, Total_Density_AD, Temperature_AD, Pressure_AD )

  END SUBROUTINE ND_to_PPMV_AD

END MODULE PPMV_ND
