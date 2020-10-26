
MODULE absorber_profile


  ! ---------------------
  ! Module use statements
  ! ---------------------

  USE type_kinds, ONLY : fp_kind
  USE parameters


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------------
  ! Default visibility
  ! ------------------

  PRIVATE


  ! ----------------------------------
  ! Explicit visibility of subprograms
  ! ----------------------------------

  PUBLIC :: Compute_Absorber_Amount
  PUBLIC :: Compute_Absorber_Amount_TL
  PUBLIC :: Compute_Absorber_Amount_AD


CONTAINS




  SUBROUTINE Compute_Absorber_Amount( Pressure,    &  ! Input,  K
                                      Water_Vapor, &  ! Input,  K
                                      Ozone,       &  ! Input,  K
                                      Absorber     )  ! Output, 0:K x J


    !#--------------------------------------------------------------------------#
    !#                         -- Type declarations --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: Pressure     ! Input,  K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: Water_Vapor  ! Input,  K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: Ozone        ! Input,  K

    REAL( fp_kind ), DIMENSION( 0:, : ), INTENT( OUT ) :: Absorber     ! Output, 0:K x J


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: k
    REAL( fp_kind ) :: dP



    !#--------------------------------------------------------------------------#
    !#                  -- INITIALISE 0'TH LEVEL AMOUNTS --                     #
    !#                                                                          #
    !# This is done so that layer differences and averages can be calculated    #
    !# simply in the predictor and transmittance routines.                      #
    !#--------------------------------------------------------------------------#

    Absorber( 0, 1 ) = ZERO          ! Wet
    Absorber( 0, 2 ) = TOA_PRESSURE  ! Dry
    Absorber( 0, 3 ) = ZERO          ! Ozo



    !#--------------------------------------------------------------------------#
    !#                   -- NADIR LEVEL ABSORBER PROFILES --                    #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------------
    ! First layer. The reason this isn't in the 
    ! loop below is due to the dP calculation.
    ! -----------------------------------------

    dP = Pressure(1) - TOA_PRESSURE

    Absorber( 1, 1 ) = RECIPROCAL_GRAVITY * dP * Water_Vapor( 1 )
    Absorber( 1, 2 ) = Pressure( 1 )
    Absorber( 1, 3 ) = RECIPROCAL_GRAVITY * dP * Ozone( 1 )


    ! --------------------------------
    ! Loop over layers, TOA - 1 -> SFC
    ! --------------------------------

    k_layer_loop: DO k = 2, SIZE( Pressure )

      dP = Pressure( k ) - Pressure( k-1 )

      Absorber( k, 1 ) = Absorber( k-1, 1 ) + ( RECIPROCAL_GRAVITY * dP * Water_Vapor( k ) )
      Absorber( k, 2 ) = Pressure( k )
      Absorber( k, 3 ) = Absorber( k-1, 3 ) + ( RECIPROCAL_GRAVITY * dP * Ozone( k ) )

    END DO k_layer_loop

  END SUBROUTINE Compute_Absorber_Amount






  SUBROUTINE Compute_Absorber_Amount_TL( &
                                         ! -- Forward input
                                         Pressure,       &  ! Input,  K
                                         Water_Vapor,    &  ! Input,  K
                                         Ozone,          &  ! Input,  K

                                         ! -- Tangent-linear input
                                         Pressure_TL,    &  ! Input,  K
                                         Water_Vapor_TL, &  ! Input,  K
                                         Ozone_TL,       &  ! Input,  K

                                         ! -- Tangent-linear output
                                         Absorber_TL     )  ! Output, 0:K x J


    !#--------------------------------------------------------------------------#
    !#                         -- Type declarations --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Forward input
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: Pressure        ! Input,  K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: Water_Vapor     ! Input,  K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: Ozone           ! Input,  K

    ! -- Tangent-linear input
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: Pressure_TL     ! Input,  K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: Water_Vapor_TL  ! Input,  K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: Ozone_TL        ! Input,  K

    ! -- Tangent-linear output
    REAL( fp_kind ), DIMENSION( 0:, : ), INTENT( OUT ) :: Absorber_TL     ! Output, 0:K x J



    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: k

    REAL( fp_kind ) :: dP
    REAL( fp_kind ) :: dP_TL



    !#--------------------------------------------------------------------------#
    !#                  -- INITIALISE 0'TH LEVEL AMOUNTS --                     #
    !#                                                                          #
    !# This is done so that layer differences and averages can be calculated    #
    !# simply in the predictor and transmittance routines.                      #
    !#--------------------------------------------------------------------------#

    absorber_TL( 0, : ) = ZERO



    !#--------------------------------------------------------------------------#
    !#           -- NADIR LEVEL TANGENT-LINEAR ABSORBER PROFILES --             #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------------
    ! First layer. The reason this isn't in the 
    ! loop below is due to the dP calculation.
    ! -----------------------------------------

    dP =    Pressure(1) - TOA_PRESSURE
    dP_TL = Pressure_TL(1)

    Absorber_TL( 1, 1 ) = RECIPROCAL_GRAVITY * ( ( dP    * Water_Vapor_TL( 1 ) ) + &
                                                 ( dP_TL * Water_Vapor( 1 )    )   )
    Absorber_TL( 1, 2 ) = Pressure_TL(1)
    Absorber_TL( 1, 3 ) = RECIPROCAL_GRAVITY * ( ( dP    * Ozone_TL( 1 ) ) + &
                                                 ( dP_TL * Ozone( 1 )    )   )


    ! --------------------------------
    ! Loop over layers, TOA - 1 -> SFC
    ! --------------------------------

    k_layer_loop: DO k = 2, SIZE( pressure )

      ! -- Layer pressure difference
      dP    = Pressure( k )    - Pressure( k-1 )
      dP_TL = Pressure_TL( k ) - Pressure_TL( k-1 )

      ! -- Integrated absorber amounts
      Absorber_TL( k, 1 ) = Absorber_TL( k-1, 1 ) + &
                            ( RECIPROCAL_GRAVITY * ( ( dP    * Water_Vapor_TL( k ) ) + &
                                                     ( dP_TL * Water_Vapor( k )    )   ) )
      Absorber_TL( k, 2 ) = Pressure_TL( k )

      Absorber_TL( k, 3 ) = Absorber_TL( k-1, 3 ) + &
                            ( RECIPROCAL_GRAVITY * ( ( dP    * Ozone_TL( k ) ) + &
                                                     ( dP_TL * Ozone( k )    )   ) )

    END DO k_layer_loop

  END SUBROUTINE Compute_Absorber_Amount_TL






  SUBROUTINE  Compute_Absorber_Amount_AD( &
                                          ! -- Forward input
                                          Pressure,       &  ! Input, K
                                          Water_Vapor,    &  ! Input, K
                                          Ozone,          &  ! Input, K

                                          ! -- Adjoint input
                                          Absorber_AD,    &  ! In/Output, 0:K x J

                                          ! -- Adjoint output
                                          Pressure_AD,    &  ! In/Output, K
                                          Water_Vapor_AD, &  ! In/Output, K
                                          Ozone_AD        )  ! In/Output, K



    !#--------------------------------------------------------------------------#
    !#                         -- Type declarations --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Forward input
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )     :: Pressure        ! Input, K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )     :: Water_Vapor     ! Input, K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )     :: Ozone           ! Input, K

    ! -- Adjoint input
    REAL( fp_kind ), DIMENSION( 0:, : ), INTENT( IN OUT ) :: Absorber_AD     ! In/Output, 0:K x J

    ! -- Adjoint output
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN OUT ) :: Pressure_AD     ! In/Output, K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN OUT ) :: Water_Vapor_AD  ! In/Output, K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN OUT ) :: Ozone_AD        ! In/Output, K


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: k

    REAL( fp_kind ) :: dP
    REAL( fp_kind ) :: dP_AD



    !#--------------------------------------------------------------------------#
    !#                     -- CALCULATE ADJOINT VALUES --                       #
    !#--------------------------------------------------------------------------#

    ! ------------------------------------------
    ! Loop over atmospheric layers, SFC -> TOA-1
    ! ------------------------------------------

    k_layer_loop: DO k = SIZE( Pressure ), 2, -1

      ! -- Layer pressure difference
      dP = Pressure( k ) - Pressure( k-1 )

      ! -- Ozone amount adjoint
      Ozone_AD( k ) = Ozone_AD( k ) + ( RECIPROCAL_GRAVITY * dP * Absorber_AD( k, 3 ) )

      ! -- Pressure adjoint
      Pressure_AD( k ) = Pressure_AD( k ) + Absorber_AD( k, 2 )

      ! -- Water vapor amount adjoint
      Water_Vapor_AD( k ) = Water_Vapor_AD( k ) + ( RECIPROCAL_GRAVITY * dP * Absorber_AD( k, 1 ) )

      ! -- Layer pressure difference adjoint
      dP_AD = RECIPROCAL_GRAVITY * ( ( Water_Vapor( k ) * Absorber_AD( k, 1 ) ) + &
                                     ( Ozone( k )       * Absorber_AD( k, 3 ) )   )
      Pressure_AD( k )   = Pressure_AD( k )   + dP_AD
      Pressure_AD( k-1 ) = Pressure_AD( k-1 ) - dP_AD

      ! -- Previous layer absorber amounts
      Absorber_AD( k-1, 3 ) = Absorber_AD( k-1, 3 ) + Absorber_AD( k, 3 )
      Absorber_AD( k, 3 ) = ZERO

      Absorber_AD( k, 2 ) = ZERO

      Absorber_AD( k-1, 1 ) = Absorber_AD( k-1, 1 ) + Absorber_AD( k, 1 )
      Absorber_AD( k, 1 ) = ZERO

    END DO k_layer_loop


    ! --------------------------------
    ! First layer adjoint calculations
    ! --------------------------------

    ! -- First layer dP
    dP = Pressure(1) - TOA_PRESSURE

    ! -- Adjoint of first layer OZONE absorber amount calculation
    dP_AD         = RECIPROCAL_GRAVITY * Ozone( 1 ) * Absorber_AD( 1, 3 )
    Ozone_AD( 1 ) = Ozone_AD( 1 ) + ( RECIPROCAL_GRAVITY * dP * Absorber_AD( 1, 3 ) )
    Absorber_AD( 1, 3 ) = ZERO

    ! -- Adjoint of first layer DRY GAS absorber amount calculation
    Pressure_AD( 1 ) = Pressure_AD( 1 ) + Absorber_AD( 1, 2 )
    Absorber_AD( 1, 2 ) = ZERO

    ! -- Adjoint of first layer WATER VAPOR absorber amount calculation
    dP_AD               = dP_AD + ( RECIPROCAL_GRAVITY * Water_Vapor( 1 ) * Absorber_AD( 1, 1 ) )
    Water_Vapor_AD( 1 ) = Water_Vapor_AD( 1 ) + ( RECIPROCAL_GRAVITY * dP * Absorber_AD( 1, 1 ) )
    Absorber_AD( 1, 1 ) = ZERO

    ! -- Adjoint of pressure difference calculation
    Pressure_AD( 1 ) = Pressure_AD( 1 ) + dP_AD
    dP_AD = ZERO


  END SUBROUTINE Compute_Absorber_Amount_AD

END MODULE absorber_profile


