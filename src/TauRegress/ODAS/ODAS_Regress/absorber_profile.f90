!------------------------------------------------------------------------------
!M+
! NAME:
!       absorber_profile
!
! PURPOSE:
!       Module containing routines to compute the integrated absorber profiles.
!
! CATEGORY:
!       NCEP RTM
!
! CALLING SEQUENCE:
!       USE absorber_profile
!
! OUTPUTS:
!       None.
!
! MODULES:
!       type_kinds:      Module containing data type kind definitions.
!
!       parameters:      Module containing parameter definitions for the
!                        RT model.
!                        USEs: TYPE_KINDS module
!
! CONTAINS:
!       Compute_Absorber_Amount:     PUBLIC subroutine to compute the integrated
!                                    absorber profiles. Currently the absorbers
!                                    are:
!                                      - Water vapor
!                                      - Dry/fixed gases
!                                      - Ozone
!
!       Compute_Absorber_Amount_TL:  PUBLIC subroutine to compute the tangent-
!                                    linear form of the integrated absorber 
!                                    profiles.
!
!       Compute_Absorber_Amount_AD:  PUBLIC subroutine to compute the adjoint of
!                                    the integrated absorber profiles.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       All of the array documentation lists the dimensions by a single letter.
!       Throughout the RTM code these are:
!         I: Array dimension is of I predictors (Istd and Iint are variants).
!         J: Array dimension is of J absorbing species.
!         K: Array dimension is of K atmospheric layers.
!         L: Array dimension is of L spectral channels.
!         M: Array dimension is of M profiles.
!       Not all of these dimensions will appear in every module.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS@NOAA/NCEP 01-Aug-2000
!                       pvandelst@ncep.noaa.gov
!
!       Adapted from code written by: Thomas J.Kleespies
!                                     NOAA/NESDIS/ORA
!                                     tkleespies@nesdis.noaa.gov
!
!  Copyright (C) 2000 Thomas Kleespies, Paul van Delst
!
!------------------------------------------------------------------------------

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


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2003/02/04 21:13:05 $
!
! $Revision$
!
! $Name:  $
!
! $State: Exp $
!
! $Log: absorber_profile.f90,v $
! Revision 2.0  2003/02/04 21:13:05  paulv
! - New release.
!
! Revision 1.12  2002/10/21 20:14:40  paulv
! - Synchronisation with repository for algorithm upgrade. Incomplete.
!
! Revision 1.11  2001/09/25 15:51:29  paulv
! - Changed the calculation of the bracketing absorber space layer in
!   sbroutine FIND_ABSORBER_LAYER_INDEX from
!     MIN( ka, MAX_N_ABSORBERS_LAYERS )
!   to
!     MAX( MIN( ka, MAX_N_ABSORBERS_LAYERS ), 1 )
!   so as to avoid the result ever being zero - which could happen before if
!   adjacent layers of the input absorber profile were zero.
!
! Revision 1.10  2001/08/31 20:41:18  paulv
! - Altered method of searching for bracketing absorber space layers in
!   FIND_ABSORBER_LAYER_INDEX. Previosuly a trickle down search was performed.
!   Now the actual corresponding layer is calculated using the exponential
!   factor used in generating the absorber space.
!
! Revision 1.9  2001/08/16 16:30:38  paulv
! - Updated documentation.
!
! Revision 1.8  2001/08/01 16:36:34  paulv
! - Removed use of module ABSORBER_SPACE and replaced it with
!     USE transmittance_coefficients, ONLY : absorber_space_levels
!   to reflect changes in code. The absorber space levels are no longer
!   calculated during model initialisation, but are precalculated and stored
!   in the transmittance coefficient data file.
!
! Revision 1.7  2001/06/05 21:18:10  paulv
! - Changed adjoint routine slightly to make adjoint calcs a bit clearer
!   when looking at the tangent-linear code.
! - Corrected bug in TOA layer pressure_AD calculation.
!
! Revision 1.6  2001/05/29 17:32:51  paulv
! - Some cosmetic changes
! - Removed subtraction of the TOA_PRESSURE parameter from the DRY absorber
!   calculation. This was causing the upper level channels to produce
!   spurious numbers in the forward calculation.
! - Added the  FIND_ABSORBER_LAYER_INDEX routine. Removed it from the FORWARD_MODEL
!   module. It seemed more appropriate in this one.
! - Using pressure array data directly in first layer calcs rather than
!   dp variable.
!
! Revision 1.5  2001/03/26 18:45:59  paulv
! - Now use TYPE_KINDS module parameter FP_KIND to set the floating point
!   data type.
! - Module parameter RECIPROCAL_GRAVITY moved to PARAMETERS module.
! - ONLY clause used in USE PARAMETERS statement. Only parameters available
!   in ABSORBER_PROFILE module are ZERO, TOA_PRESSURE, and RECIPROCAL_GRAVITY.
! - Output ABSORBER argument is now dimensioned as 0:K. This eliminates the
!   need for using an ABSORBER_KM1 variable in other routines that use the
!   ABSORBER array variable where the layer loop always goes from 1 -> n_layers.
! - Removed output arguments of AVE_ABSORBER and DELTA_ABSORBER due to the
!   ABSORBER dimension change to 0:K. Calculating the average and layer
!   difference absorber amounts in other routines can now be done simply
!   by averaging or differencing ABOSRBER(K) and ABSORBER(K-1) even for
!   layer #1.
! - Integration of absorber amount for the TOA layer is done OUTSIDE of the
!   layer loop. This avoids the need for a PRESSURE_KM1 variable since
!   pressure is dimensioned 1:K.
! - Layer loop, thus, goes from 2 -> n_layers.
! - Changed order of argument list in COMPUTE_PREDICTORS_TL and its
!   associated routines. All forward arguments are listed followed by
!   the tangent-linear arguments rather than interspersing them as before.
! - Added adjoint routine COMPUTE_ABSORBER_AMOUNT_AD.
!
! Revision 1.4  2001/03/26 18:30:54  paulv
! - Removed integrate_absorber_profile and integrate_absorber_profile_tl
!   functions. Integration is now done in-line in the main routines.
!
! Revision 1.3  2000/11/09 20:29:40  paulv
! - Added tangent linear form of the absorber profile routines.
!
! Revision 1.2  2000/08/31 19:36:31  paulv
! - Added documentation delimiters.
! - Updated documentation headers.
!
! Revision 1.1  2000/08/24 13:11:27  paulv
! Initial checkin.
!
!
!
!
