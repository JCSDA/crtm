!
! SA_MR
!
! Module containing forward, tangent-linear and adjoint subroutines
! for specific amount to/from mixing ratio units conversion.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 06-Aug-2009
!                       paul.vandelst@noaa.gov
!

MODULE SA_MR

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Modules used
  USE Type_Kinds, ONLY: fp
  USE Message_Handler
  USE Profile_Utility_Parameters, ONLY: ZERO, ONE, &
                                        MAX_N_MOLECULAR_SPECIES, &
                                        MW_DRYAIR, MOLECULAR_WEIGHT, &
                                        G_TO_KG
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: SA_to_MR, SA_to_MR_TL, SA_to_MR_AD
  PUBLIC :: MR_to_SA, MR_to_SA_TL, MR_to_SA_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &


CONTAINS


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       SA_to_MR
!
! PURPOSE:
!       Elemental subroutine to convert gas concentrations from specific amount
!       in g/kg to mass mixing ratio in g/kg.
!
! CALLING SEQUENCE:
!       CALL SA_to_MR( Specific_Amount          , &  ! Input
!                      Mixing_Ratio             , &  ! Output
!                      Water_Vapor = Water_Vapor  )  ! Optional input
!
! INPUTS:
!       Specific_Amount:   Specific amount of gas.
!                          Must be > or = 0.0
!                          UNITS:      g/kg
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar or any rank
!                          ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Mixing_Ratio:      Mass mixing ratio of gas.
!                          Set to 0.0 if input < 0.0
!                          UNITS:      g/kg
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Specific_Amount argument
!                          ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT:
!       Water_Vapor:       Water vapor mass mixing ratio. If this argument is
!                          not supplied, the input SPECIFIC_AMOUNT argument is
!                          assumed to be water vapor.
!                          Output set to 0.0 if < 0.0
!                          UNITS:      g/kg
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Specific_Amount argument
!                          ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! PROCEDURE:
!       The specific amount is defined as the ratio of the mass of gas
!       to the total mass of air and water vapor in a given volume:
!
!                  Mg
!         SA = ---------   .......(1)
!               Md + Mw
!
!       where Mg = mass of gas
!             Mw = mass of water vapor
!             Md = mass of dry air (including Mg).
!
!       The gas mass mixing ratio is defined as the ratio of the mass
!       of the gas to the mass of dry air in a given volume:
!
!                Mg
!         MRg = ----    .......(2)
!                Md
!
!       Rearranging (1) and substituting in (2) gives,
!
!          1      Md + Mw
!         ---- = ---------
!          SA       Mg
!
!                 Md     Mw
!              = ---- + ----
!                 Mg     Mg
!
!                 Md     Mw   Md                                      Md
!              = ---- + ----.----    ( multiplying the second term by ---- )
!                 Mg     Md   Mg                                      Md
!
!                  1
!              = ----- ( 1 + MRw )
!                 MRg
!
!       therefore,
!
!         MRg = SA ( 1 + MRw )
!
!       for input units of g/g or kg/kg. For input units of g/kg then,
!
!         MRg = SA ( 1 + 0.001*MRw )   .......(3)
!
!       If the input specific amount is for water vapor (specific humidity)
!       then (3) becomes,
!
!         MRw = SA ( 1 + 0.001*MRw )
!
!       i.e.
!
!         MRw = SA + 0.001*MRw*SA
!
!       i.e.
!
!         MRw( 1 - 0.001*SA ) = SA
!
!       therefore,
!
!                      SA
!         MRw = ------------------   .......(4)
!                ( 1 - 0.001*SA )
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 05-Dec-2003
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE SA_to_MR( &
    Specific_Amount, &  ! Input
    Mixing_Ratio   , &  ! Output
    Water_Vapor      )  ! Optional Input
    ! Arguments
    REAL(fp),           INTENT(IN)  :: Specific_Amount
    REAL(fp),           INTENT(OUT) :: Mixing_Ratio
    REAL(fp), OPTIONAL, INTENT(IN)  :: Water_Vapor

    ! Error checks
    ! ...Zero output for -ve input
    IF ( Specific_Amount < ZERO ) THEN
      Mixing_Ratio = ZERO
      RETURN
    ENDIF

    ! Calculate mixing ratio in g/kg
    IF ( PRESENT( Water_Vapor ) ) THEN
      ! ...Zero output for -ve input
      IF ( Water_Vapor < ZERO ) THEN
        Mixing_Ratio = ZERO
        RETURN
      ENDIF
      ! ...Gas other than water vapour
      Mixing_Ratio = Specific_Amount * ( ONE + ( G_TO_KG * Water_Vapor ) )
    ELSE
      ! ...Water vapour
      Mixing_Ratio = Specific_Amount / ( ONE - ( G_TO_KG * Specific_Amount ) )
    END IF

  END SUBROUTINE SA_to_MR


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       SA_to_MR_TL
!
! PURPOSE:
!       Tangent-linear form of elemental subroutine to convert gas
!       concentrations from specific amount in g/kg to mass mixing
!       ratio in g/kg.
!
! CALLING SEQUENCE:
!       CALL SA_to_MR_TL( Specific_Amount   , &  ! FWD Input
!                         Specific_Amount_TL, &  ! TL  Input
!                         Mixing_Ratio_TL   , &  ! TL  Output
!                         Water_Vapor       , &  ! Optional FWD Input
!                         Water_Vapor_TL      )  ! Optional TL  Input
!
! INPUTS:
!       Specific_Amount:     Specific amount of gas.
!                            Must be > or = 0.0
!                            UNITS:      g/kg
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Scalar or any rank
!                            ATTRIBUTES: INTENT(IN)
!
!       Specific_Amount_TL:  Tangent-linear specific amount of gas.
!                            UNITS:      g/kg
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Same as Specific_Amount argument
!                            ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Mixing_Ratio_TL:     Tangent-linear mass mixing ratio of gas.
!                            Set to 0.0 if input Specific_Amount < 0.0
!                            UNITS:      g/kg
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Same as Specific_Amount argument
!                            ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT:
!       Water_Vapor:         Water vapor mass mixing ratio. If this argument is
!                            not supplied, the input SPECIFIC_AMOUNT argument is
!                            assumed to be water vapor.
!                            Output set to 0.0 if < 0.0
!                            UNITS:      g/kg
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Same as Specific_Amount argument
!                            ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Water_Vapor_TL:      Tangent-linear water vapor mass mixing ratio. This
!                            argument is ignored if the optional Water_Vapor
!                            argument is not supplied also.
!                            UNITS:      g/kg
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Same as Specific_Amount argument
!                            ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 14-Aug-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE SA_to_MR_TL( &
    Specific_Amount   , &  ! FWD Input
    Specific_Amount_TL, &  ! TL  Input
    Mixing_Ratio_TL   , &  ! TL  Output
    Water_Vapor       , &  ! Optional FWD Input
    Water_Vapor_TL      )  ! Optional TL  Input
    ! Arguments
    REAL(fp),           INTENT(IN)  :: Specific_Amount
    REAL(fp),           INTENT(IN)  :: Specific_Amount_TL
    REAL(fp),           INTENT(OUT) :: Mixing_Ratio_TL
    REAL(fp), OPTIONAL, INTENT(IN)  :: Water_Vapor
    REAL(fp), OPTIONAL, INTENT(IN)  :: Water_Vapor_TL

    ! Error checks
    ! ...Zero output for -ve input
    IF ( Specific_Amount < ZERO ) THEN
      Mixing_Ratio_TL = ZERO
      RETURN
    ENDIF

    ! Tangent-linear conversion
    IF ( PRESENT(Water_Vapor) ) THEN
      ! ...Zero output for -ve input
      IF ( Water_Vapor < ZERO ) THEN
        Mixing_Ratio_TL = ZERO
        RETURN
      ENDIF
      ! ...Gas other than water vapour
      Mixing_Ratio_TL = ((ONE + (G_TO_KG * Water_Vapor)) * Specific_Amount_TL)
      IF ( PRESENT(Water_Vapor_TL) ) THEN
        Mixing_Ratio_TL = Mixing_Ratio_TL + (G_TO_KG * Specific_Amount * Water_Vapor_TL)
      END IF
    ELSE
      ! ...Water vapour
      Mixing_Ratio_TL = Specific_Amount_TL / (ONE - (G_TO_KG * Specific_Amount))**2
    END IF

  END SUBROUTINE SA_to_MR_TL


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       SA_to_MR_AD
!
! PURPOSE:
!       Adjoint form of elemental subroutine to convert gas concentrations
!       from specific amount in g/kg to mass mixing ratio in g/kg.
!
! CALLING SEQUENCE:
!       CALL SA_to_MR_AD( Specific_Amount                , &  ! FWD Input
!                         Mixing_Ratio_AD                , &  ! AD  Input
!                         Specific_Amount_AD             , &  ! AD  Output
!                         Water_Vapor    = Water_Vapor   , &  ! Optional FWD Input
!                         Water_Vapor_AD = Water_Vapor_AD  )  ! Optional AD  Output
!
! INPUTS:
!       Specific_Amount:     Specific amount of gas.
!                            Must be > or = 0.0
!                            UNITS:      g/kg
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Scalar or any rank
!                            ATTRIBUTES: INTENT(IN)
!
!       Mixing_Ratio_AD:     Adjoint mass mixing ratio of gas.
!                            *** SET TO ZERO ON EXIT ***
!                            UNITS:      g/kg
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Same as Specific_Amount argument
!                            ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUTS:
!       Specific_Amount_AD:  Adjoint specific amount of gas.
!                            *** MUST HAVE VALUE ON ENTRY ***
!                            UNITS:      g/kg
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Same as Specific_Amount argument
!                            ATTRIBUTES: INTENT(IN OUT)
!
!
! OPTIONAL INPUT:
!       Water_Vapor:         Water vapor mass mixing ratio. If this argument is
!                            not supplied, the input SPECIFIC_AMOUNT argument is
!                            assumed to be water vapor.
!                            Output set to 0.0 if < 0.0
!                            UNITS:      g/kg
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Same as Specific_Amount argument
!                            ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! OPTIONAL OUTPUT:
!       Water_Vapor_AD:      Adjoint water vapor mass mixing ratio. This
!                            argument is ignored if the optional Water_Vapor
!                            argument is not supplied also.
!                            *** MUST HAVE VALUE ON ENTRY ***
!                            UNITS:      g/kg
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Same as Specific_Amount argument
!                            ATTRIBUTES: OPTIONAL, INTENT(IN OUT)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 14-Aug-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE SA_to_MR_AD( &
    Specific_Amount   , &  ! FWD Input
    Mixing_Ratio_AD   , &  ! AD  Input
    Specific_Amount_AD, &  ! AD  Output
    Water_Vapor       , &  ! Optional FWD Input
    Water_Vapor_AD      )  ! Optional AD  Output
    ! Arguments
    REAL(fp),           INTENT(IN)     :: Specific_Amount
    REAL(fp),           INTENT(IN OUT) :: Mixing_Ratio_AD
    REAL(fp),           INTENT(IN OUT) :: Specific_Amount_AD
    REAL(fp), OPTIONAL, INTENT(IN)     :: Water_Vapor
    REAL(fp), OPTIONAL, INTENT(IN OUT) :: Water_Vapor_AD

    ! Error checks
    ! ...Zero output for -ve input
    IF ( Specific_Amount < ZERO ) THEN
      Mixing_Ratio_AD    = ZERO
      Specific_Amount_AD = ZERO
      RETURN
    ENDIF

    ! Adjoint conversion
    IF ( PRESENT(Water_Vapor) ) THEN
      ! ...Zero output for -ve input
      IF ( Water_Vapor < ZERO ) THEN
        Mixing_Ratio_AD    = ZERO
        Specific_Amount_AD = ZERO
        RETURN
      ENDIF
      ! ...Gas other than water vapour
      IF ( PRESENT(Water_Vapor_AD) ) THEN
        Water_Vapor_AD = Water_Vapor_AD + (G_TO_KG * Specific_Amount * Mixing_Ratio_AD)
      END IF
      Specific_Amount_AD = Specific_Amount_AD + ((ONE + (G_TO_KG * Water_Vapor)) * Mixing_Ratio_AD)
    ELSE
      ! ...Water vapour
      Specific_Amount_AD = Specific_Amount_AD + (Mixing_Ratio_AD / (ONE - (G_TO_KG * Specific_Amount))**2)
    END IF
    Mixing_Ratio_AD = ZERO

  END SUBROUTINE SA_to_MR_AD


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       MR_to_SA
!
! PURPOSE:
!       Elemental subroutine to convert gas concentrations from mass mixing
!       ratio in g/kg to specific amount in g/kg.
!
! CALLING SEQUENCE:
!       CALL MR_to_SA( Mixing_Ratio             , &  ! Input
!                      Specific_Amount          , &  ! Output
!                      Water_Vapor = Water_Vapor  )  ! Optional input
!
! INPUTS:
!       Mixing_Ratio:      Mass mixing ratio of gas.
!                          Must be > or = 0.0
!                          UNITS:      g/kg
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar or any rank
!                          ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Specific_Amount:   Specific amount of gas.
!                          Set to 0.0 if input < 0.0
!                          UNITS:      g/kg
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Mixing_Ratio argument
!                          ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT:
!       Water_Vapor:       Water vapor specific humidity. If this argument is
!                          not supplied, the input MIXING_RATIO argument is
!                          assumed to be water vapor.
!                          Output set to 0.0 if < 0.0
!                          UNITS:      g/kg
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Same as Mixing_Ratio argument
!                          ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! PROCEDURE:
!       From the SA_to_MR conversion, we know that for input units of
!       g/kg that,
!
!         MRg = SAg ( 1 + 0.001*MRw )   .......(1)
!
!       and
!                      SAw
!         MRw = -------------------   .......(2)
!                ( 1 - 0.001*SAw )
!
!       where MRg = mass mixing ratio of gas
!             MRw = mass mixing ratio of water vapor
!             SAg = specific amount of gas
!             SAw = specific amount of water vapor (specific humidity)
!
!       Rearranging (1) and (2) gives,
!
!         SAg = MRg ( 1 - 0.001*SAw )
!
!       and for water vapor only,
!
!                       MRw
!         SAw = -------------------
!                ( 1 + 0.001*MRw )
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 05-Dec-2003
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE MR_to_SA( &
    Mixing_Ratio   , &  ! Input
    Specific_Amount, &  ! Output
    Water_Vapor      )  ! Optional Input
    ! Arguments
    REAL(fp),           INTENT(IN)  :: Mixing_Ratio
    REAL(fp),           INTENT(OUT) :: Specific_Amount
    REAL(fp), OPTIONAL, INTENT(IN)  :: Water_Vapor

    ! Error checks
    ! ...Zero output for -ve input
    IF ( Mixing_Ratio < ZERO ) THEN
      Specific_Amount = ZERO
      RETURN
    ENDIF

    ! Calculate specific amount in g/kg
    IF ( PRESENT(Water_Vapor) ) THEN
      ! ...Zero output for -ve input
      IF ( Water_Vapor < ZERO ) THEN
        Specific_Amount = ZERO
        RETURN
      ENDIF
      ! ...Gas other than water vapour
      Specific_Amount = Mixing_Ratio * ( ONE - ( G_TO_KG * Water_Vapor ) )
    ELSE
      ! ...Water vapour
      Specific_Amount = Mixing_Ratio / ( ONE + ( G_TO_KG * Mixing_Ratio ) )
    END IF

  END SUBROUTINE MR_to_SA


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       MR_to_SA_TL
!
! PURPOSE:
!       Tangent-linear form of elemental subroutine to convert gas
!       concentrations from mass mixing ratio in g/kg to specific
!       amount in g/kg.
!
! CALLING SEQUENCE:
!       CALL MR_to_SA_TL( Mixing_Ratio                   , &  ! FWD Input
!                         Mixing_Ratio_TL                , &  ! TL  Input
!                         Specific_Amount_TL             , &  ! TL  Output
!                         Water_Vapor    = Water_Vapor   , &  ! Optional FWD input
!                         Water_Vapor_TL = Water_Vapor_TL  )  ! Optional TL input
!
! INPUTS:
!       Mixing_Ratio:        Mass mixing ratio of gas.
!                            Must be > or = 0.0
!                            UNITS:      g/kg
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Scalar or any rank
!                            ATTRIBUTES: INTENT(IN)
!
!       Mixing_Ratio_TL:     Tangent-linear mass mixing ratio of gas.
!                            UNITS:      g/kg
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Same as Mixing_Ratio argument.
!                            ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Specific_Amount_TL:  Tangent-linear specific amount of gas.
!                            Set to 0.0 if input mixing ratio < 0.0
!                            UNITS:      g/kg
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Same as Mixing_Ratio argument
!                            ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT:
!       Water_Vapor:         Water vapor specific humidity. If this argument is
!                            not supplied, the input MIXING_RATIO argument is
!                            assumed to be water vapor.
!                            Output set to 0.0 if < 0.0
!                            UNITS:      g/kg
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Same as Mixing_Ratio argument
!                            ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Water_Vapor_TL:      Tangent-linear specific humidty. This argument is
!                            ignored if the optional Water_Vapor argument is
!                            not supplied also.
!                            UNITS:      g/kg
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Same as Mixing_Ratio argument
!                            ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 14-Aug-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE MR_to_SA_TL( &
    Mixing_Ratio      , &  ! FWD Input
    Mixing_Ratio_TL   , &  ! TL Input
    Specific_Amount_TL, &  ! TL Output
    Water_Vapor       , &  ! Optional FWD Input
    Water_Vapor_TL      )  ! Optional TL  Input
    ! Arguments
    REAL(fp),           INTENT(IN)  :: Mixing_Ratio
    REAL(fp),           INTENT(IN)  :: Mixing_Ratio_TL
    REAL(fp),           INTENT(OUT) :: Specific_Amount_TL
    REAL(fp), OPTIONAL, INTENT(IN)  :: Water_Vapor
    REAL(fp), OPTIONAL, INTENT(IN)  :: Water_Vapor_TL

    ! Error checks
    ! ...Zero output for -ve input
    IF ( Mixing_Ratio < ZERO ) THEN
      Specific_Amount_TL = ZERO
      RETURN
    ENDIF

    ! Tangent-linear conversion
    IF ( PRESENT(Water_Vapor) ) THEN
      ! ...Zero output for -ve input
      IF ( Water_Vapor < ZERO ) THEN
        Specific_Amount_TL = ZERO
        RETURN
      ENDIF
      ! ...Gas other than water vapour
      Specific_Amount_TL = ((ONE - (G_TO_KG * Water_Vapor)) * Mixing_Ratio_TL)
      IF ( PRESENT(Water_Vapor_TL) ) THEN
        Specific_Amount_TL = Specific_Amount_TL - (G_TO_KG * Mixing_Ratio * Water_Vapor_TL)
      END IF
    ELSE
      ! ...Water vapour
      Specific_Amount_TL = Mixing_Ratio_TL / (ONE + (G_TO_KG * Mixing_Ratio))**2
    END IF

  END SUBROUTINE MR_to_SA_TL


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       MR_to_SA_AD
!
! PURPOSE:
!       Adjoint form of elemental subroutine to convert gas concentrations
!       from mass mixing ratio in g/kg to specific amount in g/kg.
!
! CALLING SEQUENCE:
!       CALL MR_to_SA_AD( Mixing_Ratio                   , &  ! FWD Input
!                         Specific_Amount_AD             , &  ! AD  Input
!                         Mixing_Ratio_AD                , &  ! AD  Output
!                         Water_Vapor    = Water_Vapor   , &  ! Optional FWD input
!                         Water_Vapor_AD = Water_Vapor_AD  )  ! Optional AD  output
!
! INPUTS:
!       Mixing_Ratio:        Mass mixing ratio of gas.
!                            Must be > or = 0.0
!                            UNITS:      g/kg
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Scalar or any rank
!                            ATTRIBUTES: INTENT(IN)
!
!       Specific_Amount_AD:  Adjoint specific amount of gas.
!                            *** SET TO ZERO ON EXIT ***
!                            UNITS:      g/kg
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Same as Mixing_Ratio argument
!                            ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUTS:
!       Mixing_Ratio_AD:     Adjoint mass mixing ratio of gas.
!                            *** MUST HAVE VALUE ON ENTRY ***
!                            UNITS:      g/kg
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Same as Mixing_Ratio argument.
!                            ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT:
!       Water_Vapor:         Water vapor specific humidity. If this argument is
!                            not supplied, the input MIXING_RATIO argument is
!                            assumed to be water vapor.
!                            Output set to 0.0 if < 0.0
!                            UNITS:      g/kg
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Same as Mixing_Ratio argument
!                            ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! OPTIONAL OUTPUT:
!       Water_Vapor_AD:      Adjoint specific humidty. This argument is
!                            ignored if the optional Water_Vapor argument is
!                            not supplied also.
!                            *** MUST HAVE VALUE ON ENTRY ***
!                            UNITS:      g/kg
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Same as Mixing_Ratio argument
!                            ATTRIBUTES: OPTIONAL, INTENT(IN OUT)
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 14-Aug-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE MR_to_SA_AD( &
    Mixing_Ratio      , &  ! FWD Input
    Specific_Amount_AD, &  ! AD Input
    Mixing_Ratio_AD   , &  ! AD Output
    Water_Vapor       , &  ! Optional FWD Input
    Water_Vapor_AD      )  ! Optional AD  Output
    ! Arguments
    REAL(fp),           INTENT(IN)     :: Mixing_Ratio
    REAL(fp),           INTENT(IN OUT) :: Specific_Amount_AD
    REAL(fp),           INTENT(IN OUT) :: Mixing_Ratio_AD
    REAL(fp), OPTIONAL, INTENT(IN)     :: Water_Vapor
    REAL(fp), OPTIONAL, INTENT(IN OUT) :: Water_Vapor_AD

    ! Error checks
    ! ...Zero output for -ve input
    IF ( Mixing_Ratio < ZERO ) THEN
      Specific_Amount_AD = ZERO
      Mixing_Ratio_AD    = ZERO
      RETURN
    ENDIF

    ! Tangent-linear conversion
    IF ( PRESENT(Water_Vapor) .AND. PRESENT(Water_Vapor_AD) ) THEN
      ! ...Zero output for -ve input
      IF ( Water_Vapor < ZERO ) THEN
        Specific_Amount_AD = ZERO
        Mixing_Ratio_AD    = ZERO
        Water_Vapor_AD     = ZERO
        RETURN
      ENDIF
      ! ...Gas other than water vapour
      Water_Vapor_AD  = Water_Vapor_AD - (G_TO_KG * Mixing_Ratio * Specific_Amount_AD)
      Mixing_Ratio_AD = Mixing_Ratio_AD + ((ONE - (G_TO_KG * Water_Vapor)) * Specific_Amount_AD)
    ELSE
      ! ...Water vapour
      Mixing_Ratio_AD = Mixing_Ratio_AD + (Specific_Amount_AD / (ONE + (G_TO_KG * Mixing_Ratio))**2)
    END IF
    Specific_Amount_AD = ZERO

  END SUBROUTINE MR_to_SA_AD

END MODULE SA_MR
