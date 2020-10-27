!
! PP_ND
!
! Module containing forward, tangent-linear and adjoint subroutines
! for partial pressure to/from number density units conversion.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 25-Sep-2009
!                       paul.vandelst@noaa.gov
!

MODULE PP_ND

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Modules used
  USE Type_Kinds, ONLY: fp
  USE Profile_Utility_Parameters, ONLY: ZERO, ONE, &
                                        MAX_N_MOLECULAR_SPECIES, &
                                        P0, T0, L0, &
                                        PA_TO_HPA, HPA_TO_PA
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: PP_to_ND, PP_to_ND_TL, PP_to_ND_AD
  PUBLIC :: ND_to_PP, ND_to_PP_TL, ND_to_PP_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &


CONTAINS


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       PP_to_ND
!
! PURPOSE:
!       Elemental subroutine to convert gas concentrations from partial
!       pressure to number density.
!
! CALLING SEQUENCE:
!       CALL PP_to_ND( Temperature     , &  ! Input
!                      Partial_Pressure, &  ! Input
!                      Number_Density    )  ! Output
!
! INPUTS:
!       Temperature:       Atmospheric temperature
!                          UNITS:      Kelvin, K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar or any rank
!                          ATTRIBUTES: INTENT(IN)
!
!       Partial_Pressure:  Partial pressure of gas.
!                          NOTE: Can be the total atmospheric
!                                pressure to compute the total
!                                air number density.
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Temperature
!                          ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Number_Density:    Number density of gas.
!                          Set to zero for invalid input.
!                          UNITS:      molecules/m^3
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as input Temperature
!                          ATTRIBUTES: INTENT(OUT)
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
!       At standard Temperature and Pressure (T0=273.15K, p0=101325Pa), this
!       number density is know as the Loschmidt constant, L0, the molecular
!       density of 1 mole of an ideal gas. Thus we have the generic form of
!       eqn.(2) and the STP form,
!
!               p0.NA
!         L0 = -------  molecules/m^3       ..............(3)
!               R.T0
!
!       Taking the ratio of eqns.(2) and (3) gives,
!
!         nd    p.NA     R.T0
!         -- = ------ . -------
!         L0    R.T      p0.NA
!
!       and rearranging gives,
!
!                    p      T0
!         nd = L0 . ---- . ----  molecules/m^3
!                    p0     T
!
!       The value of p0 used in this routine is expressed in pascals (Pa) whereas
!       the input partial Pressure is expected to be hPa (more common unit). Thus
!       there is a factor of 100 to include,
!
!                    100.p     T0
!         nd = L0 . ------- . ----  molecules/m^3
!                     p0       T
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 15-Nov-2001
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE PP_to_ND( &
    Temperature     , &  ! Input
    Partial_Pressure, &  ! Input
    Number_Density    )  ! Output
    ! Arguments
    REAL(fp), INTENT(IN)  :: Temperature
    REAL(fp), INTENT(IN)  :: Partial_Pressure
    REAL(fp), INTENT(OUT) :: Number_Density
    ! Local variables
    REAL(fp) :: c

    ! Setup
    Number_Density = ZERO
    IF ( Temperature < ZERO .OR. Partial_Pressure < ZERO ) RETURN

    ! Convert partial pressure to number density
    c = HPA_TO_PA * L0 * T0 / P0
    Number_Density = c * Partial_Pressure / Temperature

  END SUBROUTINE PP_to_ND


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       PP_to_ND_TL
!
! PURPOSE:
!       Tangent-linear form of elemental subroutine to convert gas
!       concentrations from partial pressure to number density.
!
! CALLING SEQUENCE:
!       CALL PP_to_ND_TL( Temperature        , &  ! FWD Input
!                         Partial_Pressure   , &  ! FWD Input
!                         Temperature_TL     , &  ! TL  Input
!                         Partial_Pressure_TL, &  ! TL  Input
!                         Number_Density_TL    )  ! TL  Output
!
! INPUTS:
!       Temperature:          Atmospheric temperature
!                             UNITS:      Kelvin, K
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN)
!
!       Partial_Pressure:     Partial pressure of gas.
!                             NOTE: Can be the total atmospheric
!                                   pressure to compute the total
!                                   air number density.
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
!       Number_Density_TL:    Tangent-linear number density of gas.
!                             Set to zero for invalid input.
!                             UNITS:      molecules/m^3
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Temperature
!                             ATTRIBUTES: INTENT(OUT)
!
!       Written by:     Paul van Delst, 25-Sep-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE PP_to_ND_TL( &
    Temperature        , &  ! FWD Input
    Partial_Pressure   , &  ! FWD Input
    Temperature_TL     , &  ! TL  Input
    Partial_Pressure_TL, &  ! TL  Input
    Number_Density_TL    )  ! TL  Output
    ! Arguments
    REAL(fp), INTENT(IN)  :: Temperature
    REAL(fp), INTENT(IN)  :: Partial_Pressure
    REAL(fp), INTENT(IN)  :: Temperature_TL
    REAL(fp), INTENT(IN)  :: Partial_Pressure_TL
    REAL(fp), INTENT(OUT) :: Number_Density_TL
    ! Local variables
    REAL(fp) :: c

    ! Setup
    Number_Density_TL = ZERO
    IF ( Temperature < ZERO .OR. Partial_Pressure < ZERO ) RETURN

    ! Tangent-linear form of partial pressure to number density conversion
    c = HPA_TO_PA * L0 * T0 / P0
    Number_Density_TL = c * ((Temperature * Partial_Pressure_TL) - &
                             (Partial_Pressure * Temperature_TL)) / Temperature**2

  END SUBROUTINE PP_to_ND_TL


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       PP_to_ND_AD
!
! PURPOSE:
!       Adjoint form of elemental subroutine to convert gas
!       concentrations from partial pressure to number density.
!
! CALLING SEQUENCE:
!       CALL PP_to_ND_AD( Temperature        , &  ! FWD Input
!                         Partial_Pressure   , &  ! FWD Input
!                         Number_Density_AD  , &  ! AD  Input
!                         Temperature_AD     , &  ! AD  Output
!                         Partial_Pressure_AD  )  ! AD  Output
!
! INPUTS:
!       Temperature:          Atmospheric temperature
!                             UNITS:      Kelvin, K
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN)
!
!       Partial_Pressure:     Partial pressure of gas.
!                             NOTE: Can be the total atmospheric
!                                   pressure to compute the total
!                                   air number density.
!                             UNITS:      hectoPascals, hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as Temperature
!                             ATTRIBUTES: INTENT(IN)
!
!       Number_Density_AD:    Adjoint number density of gas.
!                             *** SET TO ZERO ON EXIT ***
!                             UNITS:      molecules/m^3
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
! CREATION HISTORY:
!       Written by:     Paul van Delst, 25-Sep-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE PP_to_ND_AD( &
    Temperature        , &  ! FWD Input
    Partial_Pressure   , &  ! FWD Input
    Number_Density_AD  , &  ! AD  Input
    Temperature_AD     , &  ! AD  Output
    Partial_Pressure_AD  )  ! AD  Output
    ! Arguments
    REAL(fp), INTENT(IN)     :: Temperature
    REAL(fp), INTENT(IN)     :: Partial_Pressure
    REAL(fp), INTENT(IN OUT) :: Number_Density_AD
    REAL(fp), INTENT(IN OUT) :: Temperature_AD
    REAL(fp), INTENT(IN OUT) :: Partial_Pressure_AD
    ! Local variables
    REAL(fp) :: c

    ! Setup
    IF ( Temperature < ZERO .OR. Partial_Pressure < ZERO ) THEN
      Number_Density_AD     = ZERO
      Temperature_AD      = ZERO
      Partial_Pressure_AD = ZERO
      RETURN
    END IF

    ! Adjoint form of partial pressure to number density conversion
    c = HPA_TO_PA * L0 * T0 / P0
    Temperature_AD = Temperature_AD - (c * Partial_Pressure * Number_Density_AD / Temperature**2)
    Partial_Pressure_AD = Partial_Pressure_AD + (c * Number_Density_AD / Temperature)
    Number_Density_AD = ZERO

  END SUBROUTINE PP_to_ND_AD


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       ND_to_PP
!
! PURPOSE:
!       Elemental subroutine to convert gas concentration number density
!       to partial pressure.
!
! CALLING SEQUENCE:
!       CALL ND_to_PP( Temperature     , &  ! Input
!                      Number_Density  , &  ! Input
!                      Partial_Pressure  )  ! Output
!
! INPUTS:
!       Temperature:       Atmospheric temperature
!                          UNITS:      Kelvin, K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar or any rank
!                          ATTRIBUTES: INTENT(IN)
!
!       Number_Density:    Number density of gas.
!                          NOTE: Can be the total air number
!                                density to compute the total
!                                atmospheric pressure.
!                          UNITS:      molecules/m^3
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
!       Recasting eqn.(1) to provide the Pressure,
!
!              nd
!         p = ---- . R.T  Pa                ..............(2)
!              NA
!
!       where nd = the number density in molecules/m^3 = N/V.
!
!       At standard Temperature and Pressure (T0=273.15K, p0=101325Pa), the
!       number density of eqn.(2) is known as the Loschmidt constant, L0,
!       the molecular density of 1 mole of an ideal gas. Thus we have the
!       generic form of eqn.(2) and the STP form,
!
!               L0
!         p0 = ---- . R.T0  Pa              ..............(3)
!               NA
!
!       Taking the ratio of eqns.(2) and (3) gives,
!
!          p      nd     T
!         ---- = ---- . ----  Pa
!          p0     L0     T0
!
!       and rearranging gives,
!
!                   nd      T
!         p = p0 . ---- . ----  Pa
!                   L0     T0
!
!       The value of p0 used in this routine is expressed in pascals (Pa) whereas
!       the output Pressure is returned as hPa (more common unit). Thus there
!       is a factor of 100 to include,
!
!                          nd      T
!         p = 0.01 . p0 . ---- . ----  hPa
!                          L0     T0
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 15-Nov-2001
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE ND_to_PP( &
    Temperature     , &  ! Input
    Number_Density  , &  ! Input
    Partial_Pressure  )  ! Output
    ! Arguments
    REAL(fp), INTENT(IN)  :: Temperature
    REAL(fp), INTENT(IN)  :: Number_Density
    REAL(fp), INTENT(OUT) :: Partial_Pressure
    ! Local variables
    REAL(fp) :: c

    ! Setup
    Partial_Pressure = ZERO
    IF ( Temperature < ZERO .OR. Number_Density < ZERO ) RETURN

    ! Convert number density to partial pressure
    c = PA_TO_HPA * P0 / ( L0 * T0 )
    Partial_Pressure = c * Number_Density * Temperature

  END SUBROUTINE ND_to_PP


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       ND_to_PP_TL
!
! PURPOSE:
!       Tangent-linear form of elemental subroutine to convert gas
!       concentration number density to partial pressure.
!
! CALLING SEQUENCE:
!       CALL ND_to_PP_TL( Temperature        , &  ! FWD Input
!                         Number_Density     , &  ! FWD Input
!                         Temperature_TL     , &  ! TL  Input
!                         Number_Density_TL  , &  ! TL  Input
!                         Partial_Pressure_TL  )  ! TL  Output
!
! INPUTS:
!       Temperature:          Atmospheric temperature
!                             UNITS:      Kelvin, K
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN)
!
!       Number_Density:       Number density of gas.
!                             NOTE: Can be the total air number
!                                   density to compute the total
!                                   atmospheric pressure.
!                             UNITS:      molecules/m^3
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
!       Number_Density_TL:    Tangent-linear number density of gas.
!                             UNITS:      molecules/m^3
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
! CREATION HISTORY:
!       Written by:     Paul van Delst, 25-Sep-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE ND_to_PP_TL( &
    Temperature        , &  ! FWD Input
    Number_Density     , &  ! FWD Input
    Temperature_TL     , &  ! TL  Input
    Number_Density_TL  , &  ! TL  Input
    Partial_Pressure_TL  )  ! TL  Output
    ! Arguments
    REAL(fp), INTENT(IN)  :: Temperature
    REAL(fp), INTENT(IN)  :: Number_Density
    REAL(fp), INTENT(IN)  :: Temperature_TL
    REAL(fp), INTENT(IN)  :: Number_Density_TL
    REAL(fp), INTENT(OUT) :: Partial_Pressure_TL
    ! Local variables
    REAL(fp) :: c

    ! Setup
    Partial_Pressure_TL = ZERO
    IF ( Temperature < ZERO .OR. Number_Density < ZERO ) RETURN

    ! Tangent-linear form of number density to partial pressure conversion
    c = PA_TO_HPA * P0 / ( L0 * T0 )
    Partial_Pressure_TL = c * ((Number_Density * Temperature_TL) + &
                               (Temperature * Number_Density_TL) )

  END SUBROUTINE ND_to_PP_TL


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       ND_to_PP_AD
!
! PURPOSE:
!       Adjoint form of elemental subroutine to convert gas
!       concentration number density to partial pressure.
!
! CALLING SEQUENCE:
!       CALL ND_to_PP_AD( Temperature        , &  ! FWD Input
!                         Number_Density     , &  ! FWD Input
!                         Partial_Pressure_AD, &  ! AD  Input
!                         Temperature_AD     , &  ! AD  Output
!                         Number_Density_AD    )  ! AD  Output
!
! INPUTS:
!       Temperature:          Atmospheric temperature
!                             UNITS:      Kelvin, K
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN)
!
!       Number_Density:       Number density of gas.
!                             NOTE: Can be the total air number
!                                   density to compute the total
!                                   atmospheric pressure.
!                             UNITS:      molecules/m^3
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
!       Number_Density_AD:    Adjoint number density of gas.
!                             *** MUST HAVE VALUE ON ENTRY ***
!                             UNITS:      molecules/m^3
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input Temperature
!                             ATTRIBUTES: INTENT(IN OUT)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 25-Sep-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE ND_to_PP_AD( &
    Temperature        , &  ! FWD Input
    Number_Density     , &  ! FWD Input
    Partial_Pressure_AD, &  ! AD  Input
    Temperature_AD     , &  ! AD  Output
    Number_Density_AD    )  ! AD  Output
    ! Arguments
    REAL(fp), INTENT(IN)     :: Temperature
    REAL(fp), INTENT(IN)     :: Number_Density
    REAL(fp), INTENT(IN OUT) :: Partial_Pressure_AD
    REAL(fp), INTENT(IN OUT) :: Temperature_AD
    REAL(fp), INTENT(IN OUT) :: Number_Density_AD
    ! Local variables
    REAL(fp) :: c

    ! Setup
    IF ( Temperature < ZERO .OR. Number_Density < ZERO ) THEN
      Partial_Pressure_AD = ZERO
      Temperature_AD      = ZERO
      Number_Density_AD     = ZERO
      RETURN
    END IF

    ! Adjoint form of number density to partial pressure conversion
    c = PA_TO_HPA * P0 / ( L0 * T0 )
    Number_Density_AD = Number_Density_AD + (c * Temperature    * Partial_Pressure_AD)
    Temperature_AD    = Temperature_AD    + (c * Number_Density * Partial_Pressure_AD)
    Partial_Pressure_AD = ZERO

  END SUBROUTINE ND_to_PP_AD

END MODULE PP_ND
