!
! PPMV_CD
!
! Module containing forward, tangent-linear and adjoint subroutines
! for volume mixing ratio to/from column density units conversion.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 28-Sep-2009
!                       paul.vandelst@noaa.gov
!

MODULE PPMV_CD

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Modules used
  USE Type_Kinds, ONLY: fp
  USE Profile_Utility_Parameters, ONLY: ZERO, ONE, &
                                        NA, &
                                        PPV_TO_PPMV, &
                                        PPMV_to_PPV
  USE PP_ND
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: PPMV_to_CD, PPMV_to_CD_TL, PPMV_to_CD_AD
  PUBLIC :: CD_to_PPMV, CD_to_PPMV_TL, CD_to_PPMV_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  REAL(fp),     PARAMETER :: SCALE_FACTOR = 1.0e-07_fp / NA


CONTAINS


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       PPMV_to_CD
!
! PURPOSE:
!       Elemental subroutine to convert layer gas concentrations from volume
!       mixing ratio in ppmv to column density.
!
! CALLING SEQUENCE:
!       CALL PPMV_to_CD( Pressure                 , &  ! Input
!                        Temperature              , &  ! Input
!                        ppmv                     , &  ! Input
!                        Delta_Z                  , &  ! Input
!                        Column_Density           , &  ! Output
!                        Water_Vapor = Water_Vapor  )  ! Optional Input
!
! INPUTS:
!       Pressure:          Average layer pressure.
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar or any rank
!                          ATTRIBUTES: INTENT(IN)
!
!       Temperature:       Average layer temperature.
!                          UNITS:      Kelvin, K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Pressure
!                          ATTRIBUTES: INTENT(IN)
!
!       ppmv:              Average layer gas volume mixing ratio.
!                          UNITS:      ppmv
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Pressure
!                          ATTRIBUTES: INTENT(IN)
!
!       Delta_Z:           Layer thickness
!                          UNITS:      metres, m
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Pressure
!                          ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Column_Density:    Gas column density.
!                          Set to 0.0 if input < 0.0
!                          UNITS:      kmol/cm^2
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as input Pressure
!                          ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Water_Vapor:       Water vapor column density. If this argument is
!                          not supplied, the mandatory input PPMV argument is
!                          assumed to be water vapor.
!                          UNITS:      kmol/cm^2
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as input Pressure
!                          ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! PROCEDURE:
!       The column density of a particular molecular species, designated
!       by MOL, given its volume mixing ratio is then given by,
!
!         cd(MOL) = 1.0e-06 . ppmv(MOL) . cd(DRY_AIR)  kmol.cm^-2
!
!       First, the total number density is calculated using,
!
!                         p      T0
!         nd(TOT) = L0 . ---- . ----  molecules.m^-3
!                         p0     T
!
!       and L0 = Loschmidt number,
!           p0 = Standard pressure,
!           T0 = Standard temperature.
!
!       The total column density is calculated by multiplying the number
!       density by the layer thickness in metres, dz,
!
!         cd(TOT) = dz . nd(TOT)  molecules.m^-2
!
!       This result is scaled by:
!         a) 10^-4 to convert molecules.m^-2 to molecules.cm^-2
!         b) 1/Na to convert molecules.cm^-2 to mol.cm^-2, and
!         c) 10^-3 to convert mol.cm^-2 to kmol.cm^-2
!       giving,
!                    1.0e-07
!         cd(TOT) = --------- . dz . nd(TOT)  kmol.cm^-2
!                      Na
!
!       The dry air column density is then calculated using,
!
!                                    1
!         cd(DRY_AIR) = ----------------------------- . cd(TOT)  kmol.cm^-2
!                        1 + ( ppmv(H2O) . 1.0e-06 )
!
!       for water vapor ppmv input or,
!
!         cd(DRY_AIR) = cd(TOT) - cd(H2O)
!
!       if cd(H2O) is supplied via the optional Water_Vapor argument.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 04-Jan-2001
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE PPMV_to_CD( &
    Pressure      , &  ! Input
    Temperature   , &  ! Input
    ppmv          , &  ! Input
    Delta_Z       , &  ! Input
    Column_Density, &  ! Output
    Water_Vapor     )  ! Optional Input
    ! Arguments
    REAL(fp),           INTENT(IN)  :: Pressure
    REAL(fp),           INTENT(IN)  :: Temperature
    REAL(fp),           INTENT(IN)  :: ppmv
    REAL(fp),           INTENT(IN)  :: Delta_Z
    REAL(fp),           INTENT(OUT) :: Column_Density
    REAL(fp), OPTIONAL, INTENT(IN)  :: Water_Vapor
    ! Local variables
    REAL(fp) :: ppv
    REAL(fp) :: Number_Density
    REAL(fp) :: Total_Density

    ! Setup
    Column_Density = ZERO
    IF ( Pressure < ZERO .OR. Temperature < ZERO .OR. ppmv < ZERO .OR. Delta_Z < ZERO ) RETURN
    IF ( PRESENT(Water_Vapor) ) THEN
      IF ( Water_Vapor < ZERO ) RETURN
    END IF

    ! Convert input to parts-per
    ppv = PPMV_to_PPV * ppmv

    ! Calculate air column density
    CALL PP_to_ND( Temperature, Pressure, Number_Density )
    Total_Density = SCALE_FACTOR * Delta_Z * Number_Density

    ! Calculate the molecular column density
    IF ( PRESENT(Water_Vapor) ) THEN
      ! ...Gas other than water vapour
      Column_Density = ppv * (Total_Density - Water_Vapor)
    ELSE
      ! ...Water vapour
      Column_Density = (ppv / (ONE + ppv)) * Total_Density
    END IF

  END SUBROUTINE PPMV_to_CD


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       PPMV_to_CD_TL
!
! PURPOSE:
!       Tangent-linear form of elemental subroutine to convert layer gas
!       concentrations from volume mixing ratio in ppmv to column density.
!
! CALLING SEQUENCE:
!       CALL PPMV_to_CD_TL( Pressure                       , &  ! FWD Input
!                           Temperature                    , &  ! FWD Input
!                           ppmv                           , &  ! FWD Input
!                           Delta_Z                        , &  ! FWD Input
!                           Pressure_TL                    , &  ! TL  Input
!                           Temperature_TL                 , &  ! TL  Input
!                           ppmv_TL                        , &  ! TL  Input
!                           Delta_Z_TL                     , &  ! TL  Input
!                           Column_Density_TL              , &  ! TL  Output
!                           Water_Vapor    = Water_Vapor   , &  ! Optional FWD Input
!                           Water_Vapor_TL = Water_Vapor_TL  )  ! Optional TL  Input
!
! INPUTS:
!       Pressure:             Average layer pressure.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN)
!
!       Temperature:          Average layer temperature.
!                             UNITS:      Kelvin, K
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       ppmv:                 Average layer gas volume mixing ratio.
!                             UNITS:      ppmv
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Delta_Z:              Layer thickness
!                             UNITS:      metres, m
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Pressure_TL:          Tangent-linear layer pressure.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Temperature_TL:       Tangent-linear layer temperature.
!                             UNITS:      Kelvin, K
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       ppmv_TL:              Tangent-linear layer gas volume mixing ratio.
!                             UNITS:      ppmv
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Delta_Z_TL:           Tangent-linear layer thickness
!                             UNITS:      metres, m
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)

! OUTPUTS:
!       Column_Density_TL:    Tangent-linear gas column density.
!                             Set to 0.0 for invalid input
!                             UNITS:      kmol/cm^2
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Water_Vapor:          Water vapor column density. If this argument
!                             is not supplied, the mandatory input PPMV and
!                             PPMV_TL arguments are assumed to be for water
!                             vapor.
!                             UNITS:      kmol/cm^2
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Water_Vapor_TL:       Tangent-linear water vapor column density.
!                             This argument is ignored if the optional
!                             Water_Vapor argument is not supplied also.
!                             UNITS:      kmol/cm^2
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 28-Sep-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE PPMV_to_CD_TL( &
    Pressure         , &  ! FWD Input
    Temperature      , &  ! FWD Input
    ppmv             , &  ! FWD Input
    Delta_Z          , &  ! FWD Input
    Pressure_TL      , &  ! TL  Input
    Temperature_TL   , &  ! TL  Input
    ppmv_TL          , &  ! TL  Input
    Delta_Z_TL       , &  ! TL  Input
    Column_Density_TL, &  ! TL  Output
    Water_Vapor      , &  ! Optional FWD Input
    Water_Vapor_TL     )  ! Optional TL  Input
    ! Arguments
    REAL(fp),           INTENT(IN)  :: Pressure
    REAL(fp),           INTENT(IN)  :: Temperature
    REAL(fp),           INTENT(IN)  :: ppmv
    REAL(fp),           INTENT(IN)  :: Delta_Z
    REAL(fp),           INTENT(IN)  :: Pressure_TL
    REAL(fp),           INTENT(IN)  :: Temperature_TL
    REAL(fp),           INTENT(IN)  :: ppmv_TL
    REAL(fp),           INTENT(IN)  :: Delta_Z_TL
    REAL(fp),           INTENT(OUT) :: Column_Density_TL
    REAL(fp), OPTIONAL, INTENT(IN)  :: Water_Vapor
    REAL(fp), OPTIONAL, INTENT(IN)  :: Water_Vapor_TL
    ! Local variables
    REAL(fp) :: wv_TL
    REAL(fp) :: ppv           , ppv_TL
    REAL(fp) :: Number_Density, Number_Density_TL
    REAL(fp) :: Total_Density , Total_Density_TL

    ! Setup
    Column_Density_TL = ZERO
    IF ( Pressure < ZERO .OR. Temperature < ZERO .OR. ppmv < ZERO .OR. Delta_Z < ZERO ) RETURN
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

    ! Calculate air column density
    CALL PP_to_ND( Temperature, Pressure, Number_Density )
    CALL PP_to_ND_TL( Temperature, Pressure, Temperature_TL, Pressure_TL, Number_Density_TL )
    Total_Density    = SCALE_FACTOR * Delta_Z * Number_Density
    Total_Density_TL = SCALE_FACTOR * ((Delta_Z * Number_Density_TL) + (Number_Density * Delta_Z_TL))

    ! Tangent-linear form of column density calculation
    IF ( PRESENT(Water_Vapor) ) THEN
      ! ...Gas other than water vapour
      Column_Density_TL = ((Total_Density - Water_Vapor) * ppv_TL) + &
                          (ppv * Total_Density_TL) - &
                          (ppv * wv_TL)
    ELSE
      ! ...Water vapour
      Column_Density_TL = (ppv * Total_Density_TL / (ONE + ppv)) + &
                          (Total_Density * ppv_TL / (ONE + ppv)**2)
    END IF

  END SUBROUTINE PPMV_to_CD_TL


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       PPMV_to_CD_AD
!
! PURPOSE:
!       Adjoint form of elemental subroutine to convert layer gas
!       concentrations from volume mixing ratio in ppmv to column
!       density.
!
! CALLING SEQUENCE:
!       CALL PPMV_to_CD_AD( Pressure                       , &  ! FWD Input
!                           Temperature                    , &  ! FWD Input
!                           ppmv                           , &  ! FWD Input
!                           Delta_Z                        , &  ! FWD Input
!                           Column_Density_AD              , &  ! AD  Input
!                           Pressure_AD                    , &  ! AD  Output
!                           Temperature_AD                 , &  ! AD  Output
!                           ppmv_AD                        , &  ! AD  Output
!                           Delta_Z_AD                     , &  ! AD  Output
!                           Water_Vapor    = Water_Vapor   , &  ! Optional FWD Input
!                           Water_Vapor_AD = Water_Vapor_AD  )  ! Optional AD  Output
!
! INPUTS:
!       Pressure:             Average layer pressure.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN)
!
!       Temperature:          Average layer temperature.
!                             UNITS:      Kelvin, K
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       ppmv:                 Average layer gas volume mixing ratio.
!                             UNITS:      ppmv
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Delta_Z:              Layer thickness
!                             UNITS:      metres, m
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Column_Density_AD:    Adjoint gas column density.
!                             *** SET TO ZERO ON EXIT ***
!                             UNITS:      kmol/cm^2
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUTS:
!       Pressure_AD:          Adjoint layer pressure.
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: INTENT(IN OUT)
!
!       Temperature_AD:       Adjoint layer temperature.
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      Kelvin, K
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: INTENT(IN OUT)
!
!       ppmv_AD:              Adjoint layer gas volume mixing ratio.
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      ppmv
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: INTENT(IN OUT)
!
!       Delta_Z_AD:           Adjoint layer thickness.
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      metres, m
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       Water_Vapor:          Water vapor column density. If this argument
!                             is not supplied, the mandatory input PPMV and
!                             PPMV_TL arguments are assumed to be for water
!                             vapor.
!                             UNITS:      kmol/cm^2
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       Water_Vapor_AD:       Adjoint water vapor column density.
!                             This argument is ignored if the optional
!                             Water_Vapor argument is not supplied also.
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      kmol/cm^2
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: OPTIONAL, INTENT(IN OUT)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 28-Sep-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE PPMV_to_CD_AD( &
    Pressure         , &  ! FWD Input
    Temperature      , &  ! FWD Input
    ppmv             , &  ! FWD Input
    Delta_Z          , &  ! FWD Input
    Column_Density_AD, &  ! AD  Input
    Pressure_AD      , &  ! AD  Output
    Temperature_AD   , &  ! AD  Output
    ppmv_AD          , &  ! AD  Output
    Delta_Z_AD       , &  ! AD  Output
    Water_Vapor      , &  ! Optional FWD Input
    Water_Vapor_AD     )  ! Optional AD  Output
    ! Arguments
    REAL(fp),           INTENT(IN)     :: Pressure
    REAL(fp),           INTENT(IN)     :: Temperature
    REAL(fp),           INTENT(IN)     :: ppmv
    REAL(fp),           INTENT(IN)     :: Delta_Z
    REAL(fp),           INTENT(IN OUT) :: Column_Density_AD
    REAL(fp),           INTENT(IN OUT) :: Pressure_AD
    REAL(fp),           INTENT(IN OUT) :: Temperature_AD
    REAL(fp),           INTENT(IN OUT) :: ppmv_AD
    REAL(fp),           INTENT(IN OUT) :: Delta_Z_AD
    REAL(fp), OPTIONAL, INTENT(IN)     :: Water_Vapor
    REAL(fp), OPTIONAL, INTENT(IN OUT) :: Water_Vapor_AD
    ! Local variables
    REAL(fp) :: wv_AD
    REAL(fp) :: ppv           , ppv_AD
    REAL(fp) :: Number_Density, Number_Density_AD
    REAL(fp) :: Total_Density , Total_Density_AD

    ! Setup
    IF ( Pressure < ZERO .OR. Temperature < ZERO .OR. ppmv < ZERO ) THEN
      Column_Density_AD = ZERO
      Pressure_AD       = ZERO
      Temperature_AD    = ZERO
      ppmv_AD           = ZERO
      Delta_Z_AD        = ZERO
      RETURN
    ENDIF
    IF ( PRESENT(Water_Vapor) ) THEN
      IF ( Water_Vapor < ZERO ) THEN
        Column_Density_AD = ZERO
        Pressure_AD       = ZERO
        Temperature_AD    = ZERO
        ppmv_AD           = ZERO
        Delta_Z_AD        = ZERO
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
    CALL PP_to_ND( Temperature, Pressure, Number_Density )
    Total_Density = SCALE_FACTOR * Delta_Z * Number_Density

    ! Adjoint form of column density calculation
    IF ( PRESENT(Water_Vapor) ) THEN
      ! ...Gas other than water vapour
      wv_AD            = wv_AD - (ppv * Column_Density_AD)
      Total_Density_AD = ppv * Column_Density_AD
      ppv_AD           = (Total_Density - Water_Vapor) * Column_Density_AD
      ! ...Save local mods to Water_Vapor_AD argument
      IF ( PRESENT(Water_Vapor_AD) ) Water_Vapor_AD = wv_AD
    ELSE
      ! ...Water vapour
      ppv_AD           = Total_Density * Column_Density_AD / (ONE + ppv)**2
      Total_Density_AD = ppv * Column_Density_AD / (ONE + ppv)
    END IF
    Column_Density_AD = ZERO

    ! Adjoint form of air column density
    Delta_Z_AD        = Delta_Z_AD + (SCALE_FACTOR * Number_Density * Total_Density_AD )
    Number_Density_AD =               SCALE_FACTOR * Delta_Z * Total_Density_AD
    CALL PP_to_ND_AD( Temperature, Pressure, Number_Density_AD, Temperature_AD, Pressure_AD )

    ! Adjoint form of ppmv to parts-per conversion
    ppmv_AD = ppmv_AD + (PPMV_to_PPV * ppv_AD)

  END SUBROUTINE PPMV_to_CD_AD


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       CD_to_PPMV
!
! PURPOSE:
!       Elemental subroutine to convert layer gas concentrations from column
!       density to volume mixing ratio in ppmv.
!
! CALLING SEQUENCE:
!       CALL CD_to_PPMV( Pressure                 , &  ! Input
!                        Temperature              , &  ! Input
!                        Column_Density           , &  ! Input
!                        Delta_Z                  , &  ! Input
!                        ppmv                     , &  ! Output
!                        Water_Vapor = Water_Vapor  )  ! Optional Input
!
! INPUTS:
!       Pressure:          Average layer pressure.
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar or any rank
!                          ATTRIBUTES: INTENT(IN)
!
!       Temperature:       Average layer temperature.
!                          UNITS:      Kelvin, K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Pressure
!                          ATTRIBUTES: INTENT(IN)
!
!       Column_Density:    Gas layer column density.
!                          Set to 0.0 if input < 0.0
!                          UNITS:      kmol/cm^2
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as input Pressure
!                          ATTRIBUTES: INTENT(IN)
!
!       Delta_Z:           Layer thickness
!                          UNITS:      metres, m
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Pressure
!                          ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       ppmv:              Layer gas volume mixing ratio.
!                          Set to 0.0 if input < 0.0
!                          UNITS:      ppmv
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as input Pressure
!                          ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Water_Vapor:       Water vapor volume mixing ratio. If this argument is
!                          not supplied, the mandatory input COLUMN_DENSITY
!                          argument is assumed to be water vapor.
!                          UNITS:      ppmv
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Pressure
!                          ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! PROCEDURE:
!       The volume mixing ratio of a particular molecular species, designated
!       by MOL, given it's column density is given by,
!
!                                  cd(MOL)
!         ppmv(MOL) = 1.0e-06 . -------------
!                                cd(DRY_AIR)
!
!       First, the total number density is calculated using,
!
!                         p      T0
!         nd(TOT) = L0 . ---- . ----  molecules.m^-3
!                         p0     T
!
!       and L0 = Loschmidt number,
!           p0 = Standard pressure,
!           T0 = Standard temperature.
!
!       The total column density is calculated by multiplying the number
!       density by the layer thickness in metres, dz,
!
!         cd(TOT) = dz . nd(TOT)  molecules.m^-2
!
!       This result is scaled by:
!         a) 10^-4 to convert molecules.m^-2 to molecules.cm^-2
!         b) 1/Na to convert molecules.cm^-2 to mol.cm^-2, and
!         c) 10^-3 to convert mol.cm^-2 to kmol.cm^-2
!       giving,
!                    1.0e-07
!         cd(TOT) = --------- . dz . nd(TOT)  kmol.cm^-2
!                      Na
!
!       The dry air column density is then calculated using,
!
!                                    1
!         cd(DRY_AIR) = ----------------------------- . cd(TOT)  kmol.cm^-2
!                        1 + ( ppmv(H2O) . 1.0e-06 )
!
!       for water vapor ppmv input or,
!
!         cd(DRY_AIR) = cd(TOT) - cd(H2O)
!
!       if cd(H2O) is supplied via the optional Water_Vapor argument.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 04-Jan-2001
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CD_to_PPMV( &
     Pressure      , &  ! Input
     Temperature   , &  ! Input
     Column_Density, &  ! Input
     Delta_Z       , &  ! Input
     ppmv          , &  ! Output
     Water_Vapor     )  ! Optional Input
    ! Arguments
    REAL(fp),           INTENT(IN)  :: Pressure
    REAL(fp),           INTENT(IN)  :: Temperature
    REAL(fp),           INTENT(IN)  :: Column_Density
    REAL(fp),           INTENT(IN)  :: Delta_Z
    REAL(fp),           INTENT(OUT) :: ppmv
    REAL(fp), OPTIONAL, INTENT(IN)  :: Water_Vapor
    ! Local variables
    REAL(fp) :: ppv
    REAL(fp) :: Number_Density
    REAL(fp) :: Total_Density
    REAL(fp) :: Dry_Air_Density

    ! Setup
    ppmv = ZERO
    IF ( Pressure < ZERO .OR. Temperature < ZERO .OR. &
         Column_Density < ZERO .OR. Delta_Z < ZERO ) RETURN
    IF ( PRESENT(Water_Vapor) ) THEN
      IF ( Water_Vapor < ZERO ) RETURN
    END IF

    ! Calculate air column density
    CALL PP_to_ND( Temperature, Pressure, Number_Density )
    Total_Density = SCALE_FACTOR * Delta_Z * Number_Density

    ! Calculate the dry air column density
    IF ( PRESENT(Water_Vapor) ) THEN
      ! ...Gas other than water vapor
      ppv = PPMV_TO_PPV * Water_Vapor
      Dry_Air_Density = Total_Density * (ONE / (ONE + ppv))
    ELSE
      ! ...Water vapour
      Dry_Air_Density = Total_Density - Column_Density
    END IF

    ! Convert the column density ratio to ppmv
    ppmv = PPV_TO_PPMV * Column_Density / Dry_Air_Density

  END SUBROUTINE CD_to_PPMV


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       CD_to_PPMV_TL
!
! PURPOSE:
!       Tangent-linear form of elemental subroutine to convert layer gas
!       concentrations from column density to volume mixing ratio in ppmv.
!
! CALLING SEQUENCE:
!       CALL CD_to_PPMV_TL( Pressure                       , &  ! FWD Input
!                           Temperature                    , &  ! FWD Input
!                           Column_Density                 , &  ! FWD Input
!                           Delta_Z                        , &  ! FWD Input
!                           Pressure_TL                    , &  ! TL  Input
!                           Temperature_TL                 , &  ! TL  Input
!                           Column_Density_TL              , &  ! TL  Input
!                           Delta_Z_TL                     , &  ! TL  Input
!                           ppmv_TL                        , &  ! TL  Output
!                           Water_Vapor    = Water_Vapor   , &  ! Optional FWD Input
!                           Water_Vapor_TL = Water_Vapor_TL  )  ! Optional TL  Input
!
! INPUTS:
!       Pressure:             Average layer pressure.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN)
!
!       Temperature:          Average layer temperature.
!                             UNITS:      Kelvin, K
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Column_Density:       Gas layer column density.
!                             UNITS:      kmol/cm^2
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Delta_Z:              Layer thickness
!                             UNITS:      metres, m
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Pressure_TL:          Tangent-linear layer pressure.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Temperature_TL:       Tangent-linear layer temperature.
!                             UNITS:      Kelvin, K
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Column_Density_TL:    Tangent-linear gas layer column density.
!                             UNITS:      kmol/cm^2
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Delta_Z_TL:           Tangent-linear layer thickness
!                             UNITS:      metres, m
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
!                             input COLUMN_DENSITY and COLUMN_DENSITY_TL
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
!       Written by:     Paul van Delst, 28-Sep-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CD_to_PPMV_TL( &
    Pressure         , &  ! FWD Input
    Temperature      , &  ! FWD Input
    Column_Density   , &  ! FWD Input
    Delta_Z          , &  ! FWD Input
    Pressure_TL      , &  ! TL  Input
    Temperature_TL   , &  ! TL  Input
    Column_Density_TL, &  ! TL  Input
    Delta_Z_TL       , &  ! TL  Input
    ppmv_TL          , &  ! TL  Output
    Water_Vapor      , &  ! Optional FWD Input
    Water_Vapor_TL     )  ! Optional TL  Input
    ! Arguments
    REAL(fp),           INTENT(IN)  :: Pressure
    REAL(fp),           INTENT(IN)  :: Temperature
    REAL(fp),           INTENT(IN)  :: Column_Density
    REAL(fp),           INTENT(IN)  :: Delta_Z
    REAL(fp),           INTENT(IN)  :: Pressure_TL
    REAL(fp),           INTENT(IN)  :: Temperature_TL
    REAL(fp),           INTENT(IN)  :: Column_Density_TL
    REAL(fp),           INTENT(IN)  :: Delta_Z_TL
    REAL(fp),           INTENT(OUT) :: ppmv_TL
    REAL(fp), OPTIONAL, INTENT(IN)  :: Water_Vapor
    REAL(fp), OPTIONAL, INTENT(IN)  :: Water_Vapor_TL
    ! Local variables
    REAL(fp) :: wv_TL
    REAL(fp) :: ppv            , ppv_TL
    REAL(fp) :: Number_Density , Number_Density_TL
    REAL(fp) :: Total_Density  , Total_Density_TL
    REAL(fp) :: Dry_Air_Density, Dry_Air_Density_TL

    ! Setup
    ppmv_TL = ZERO
    IF ( Pressure < ZERO .OR. Temperature < ZERO .OR. &
         Column_Density < ZERO .OR. Delta_Z < ZERO ) RETURN
    IF ( PRESENT(Water_Vapor) ) THEN
      IF ( Water_Vapor < ZERO ) RETURN
      ! ...Local tangent-linear variable assignment
      IF ( PRESENT(Water_Vapor_TL) ) THEN
        wv_TL = Water_Vapor_TL
      ELSE
        wv_TL = ZERO
      END IF
    END IF

    ! Calculate total column density
    CALL PP_to_ND( Temperature, Pressure, Number_Density )
    CALL PP_to_ND_TL( Temperature, Pressure, Temperature_TL, Pressure_TL, Number_Density_TL )
    Total_Density    = SCALE_FACTOR * Delta_Z * Number_Density
    Total_Density_TL = SCALE_FACTOR * ((Delta_Z * Number_Density_TL) + (Number_Density * Delta_Z_TL))

    ! Tangent-linear form of dry air column density
    IF ( PRESENT(Water_Vapor) ) THEN
      ! ...Gas other than water vapor
      ppv    = PPMV_TO_PPV * Water_Vapor
      ppv_TL = PPMV_TO_PPV * wv_TL
      Dry_Air_Density    = Total_Density * (ONE / (ONE + ppv))
      Dry_Air_Density_TL = (((ONE + ppv) * Total_Density_TL) - (Total_Density * ppv_TL)) / (ONE + ppv)**2
    ELSE
      ! ...Water vapour
      Dry_Air_Density    = Total_Density    - Column_Density
      Dry_Air_Density_TL = Total_Density_TL - Column_Density_TL
    END IF

    ! Tangent-linear form of volume mixing ratio
    ppmv_TL = PPV_TO_PPMV * ((Dry_Air_Density * Column_Density_TL) - &
                             (Column_Density * Dry_Air_Density_TL)) / Dry_Air_Density**2

  END SUBROUTINE CD_to_PPMV_TL


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       CD_to_PPMV_AD
!
! PURPOSE:
!       Adjoint form of elemental subroutine to convert layer gas
!       concentrations from column density to volume mixing ratio
!       in ppmv.
!
! CALLING SEQUENCE:
!       CALL CD_to_PPMV_AD( Pressure                       , &  ! FWD Input
!                           Temperature                    , &  ! FWD Input
!                           Column_Density                 , &  ! FWD Input
!                           Delta_Z                        , &  ! FWD Input
!                           ppmv_AD                        , &  ! AD  Input
!                           Pressure_AD                    , &  ! AD  Output
!                           Temperature_AD                 , &  ! AD  Output
!                           Column_Density_AD              , &  ! AD  Output
!                           Delta_Z_AD                     , &  ! AD  Output
!                           Water_Vapor    = Water_Vapor   , &  ! Optional FWD Input
!                           Water_Vapor_AD = Water_Vapor_AD  )  ! Optional AD  Output
!
! INPUTS:
!       Pressure:             Average layer pressure.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN)
!
!       Temperature:          Average layer temperature.
!                             UNITS:      Kelvin, K
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Column_Density:       Gas layer column density.
!                             UNITS:      kmol/cm^2
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       Delta_Z:              Layer thickness
!                             UNITS:      metres, m
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN)
!
!       ppmv_AD:              Adjoint gas volume mixing ratio.
!                             *** SET TO ZERO ON EXIT ***
!                             UNITS:      ppmv
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Pressure
!                             ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUTS:
!       Pressure_AD:          Adjoint layer pressure.
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: INTENT(IN OUT)
!
!       Temperature_AD:       Adjoint layer temperature.
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      Kelvin, K
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: INTENT(IN OUT)
!
!       Column_Density_AD:    Adjoint gas layer column density.
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      kmol/cm^2
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: INTENT(IN OUT)
!
!       Delta_Z_AD:           Adjoint layer thickness
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      metres, m
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Pressure
!                             ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       Water_Vapor:          Water vapor volume mixing ratio. If this
!                             argument is not supplied, the mandatory
!                             input COLUMN_DENSITY and COLUMN_DENSITY_AD
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
!       Written by:     Paul van Delst, 28-Sep-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CD_to_PPMV_AD( &
    Pressure         , &  ! FWD Input
    Temperature      , &  ! FWD Input
    Column_Density   , &  ! FWD Input
    Delta_Z          , &  ! FWD Input
    ppmv_AD          , &  ! AD  Input
    Pressure_AD      , &  ! AD  Output
    Temperature_AD   , &  ! AD  Output
    Column_Density_AD, &  ! AD  Output
    Delta_Z_AD       , &  ! AD  Output
    Water_Vapor      , &  ! Optional FWD Input
    Water_Vapor_AD     )  ! Optional AD  Output
    ! Arguments
    REAL(fp),           INTENT(IN)     :: Pressure
    REAL(fp),           INTENT(IN)     :: Temperature
    REAL(fp),           INTENT(IN)     :: Column_Density
    REAL(fp),           INTENT(IN)     :: Delta_Z
    REAL(fp),           INTENT(IN OUT) :: ppmv_AD
    REAL(fp),           INTENT(IN OUT) :: Pressure_AD
    REAL(fp),           INTENT(IN OUT) :: Temperature_AD
    REAL(fp),           INTENT(IN OUT) :: Column_Density_AD
    REAL(fp),           INTENT(IN OUT) :: Delta_Z_AD
    REAL(fp), OPTIONAL, INTENT(IN)     :: Water_Vapor
    REAL(fp), OPTIONAL, INTENT(IN OUT) :: Water_Vapor_AD
    ! Local variables
    REAL(fp) :: wv_AD
    REAL(fp) :: ppv            , ppv_AD
    REAL(fp) :: Number_Density , Number_Density_AD
    REAL(fp) :: Total_Density  , Total_Density_AD
    REAL(fp) :: Dry_Air_Density, Dry_Air_Density_AD

    ! Setup
    IF ( Pressure < ZERO .OR. Temperature < ZERO .OR. &
         Column_Density < ZERO .OR. Delta_Z < ZERO ) THEN
      ppmv_AD           = ZERO
      Pressure_AD       = ZERO
      Temperature_AD    = ZERO
      Column_Density_AD = ZERO
      RETURN
    ENDIF
    IF ( PRESENT(Water_Vapor) ) THEN
      IF ( Water_Vapor < ZERO ) THEN
        ppmv_AD           = ZERO
        Pressure_AD       = ZERO
        Temperature_AD    = ZERO
        Column_Density_AD = ZERO
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
    CALL PP_to_ND( Temperature, Pressure, Number_Density )
    Total_Density = SCALE_FACTOR * Delta_Z * Number_Density
    IF ( PRESENT(Water_Vapor) ) THEN
      ! ...Gas other than water vapor
      ppv = PPMV_TO_PPV * Water_Vapor
      Dry_Air_Density = Total_Density * (ONE / (ONE + ppv))
    ELSE
      ! ...Water vapour
      Dry_Air_Density = Total_Density - Column_Density
    END IF

    ! Adjoint form of volume mixing ratio calculation
    Dry_Air_Density_AD = -(PPV_TO_PPMV * Column_Density * ppmv_AD / Dry_Air_Density**2)
    Column_Density_AD  = Column_Density_AD + (PPV_TO_PPMV * ppmv_AD / Dry_Air_Density)
    ppmv_AD = ZERO

    ! Adjoint form of dry air column density calculation
    IF ( PRESENT(Water_Vapor) ) THEN
      ! ...Gas other than water vapor
      ppv_AD           = -(Total_Density * Dry_Air_Density_AD / (ONE + ppv)**2)
      Total_Density_AD = Dry_Air_Density_AD / (ONE + ppv)
      wv_AD            = wv_AD + (PPMV_TO_PPV * ppv_AD)
      ! ...Save local mods to Water_Vapor_AD argument
      IF ( PRESENT(Water_Vapor_AD) ) Water_Vapor_AD = wv_AD
    ELSE
      ! ...Water vapour
      Column_Density_AD = Column_Density_AD - Dry_Air_Density_AD
      Total_Density_AD  = Dry_Air_Density_AD
    END IF

    ! Adjoint form of total column density calculation
    Delta_Z_AD        = Delta_Z_AD + (SCALE_FACTOR * Number_Density * Total_Density_AD )
    Number_Density_AD =               SCALE_FACTOR * Delta_Z * Total_Density_AD
    CALL PP_to_ND_AD( Temperature, Pressure, Number_Density_AD, Temperature_AD, Pressure_AD )

  END SUBROUTINE CD_to_PPMV_AD

END MODULE PPMV_CD
