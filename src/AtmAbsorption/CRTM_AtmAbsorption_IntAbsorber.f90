!------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_AtmAbsorption_IntAbsorber
!
! PURPOSE:
!       Module to compute the integrated absorber profiles for the gaseous
!       absorption algorithm.
!
! CATEGORY:
!       CRTM : Gas Absorption : Integrated Absorber
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_AtmAbsorption_IntAbsorber
!
! MODULES:
!       Type_Kinds:                 Module containing definitions for kinds
!                                   of variable types.
!
!       Message_Handler:            Module to define simple error codes and
!                                   handle error conditions
!                                   USEs: FILE_UTILITY module
!
!       CRTM_Parameters:            Module of parameter definitions for the CRTM.
!                                   USEs: TYPE_KINDS module
!
!       CRTM_Atmosphere_Define:     Module defining the CRTM Atmosphere
!                                   structure and containing routines to 
!                                   manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         CRTM_CLOUD_DEFINE module
!
!
! CONTAINS:
!       CRTM_Compute_IntAbsorber:      Subroutine to compute the integrated
!                                      absorber profiles.
!
!       CRTM_Compute_IntAbsorber_TL:   Subroutine to compute the tangent-linear
!                                      integrated absorber profiles.
!
!       CRTM_Compute_IntAbsorber_AD:   Subroutine to compute the adjoint
!                                      integrated absorber profiles.
!
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 03-Jan-2005
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2005 Paul van Delst
!
!  This program is free software; you can redistribute it and/or
!  modify it under the terms of the GNU General Public License
!  as published by the Free Software Foundation; either version 2
!  of the License, or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program; if not, write to the Free Software
!  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
!M-
!------------------------------------------------------------------------------

MODULE CRTM_AtmAbsorption_IntAbsorber


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Message_Handler

  ! -- CRTM modules
  USE CRTM_Parameters
  USE CRTM_Atmosphere_Define, ONLY: CRTM_Atmosphere_type, H2O_ID, O3_ID


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  ! -- Everything private by default
  PRIVATE

  ! -- Science routines in this module
  PUBLIC :: CRTM_Compute_IntAbsorber
  PUBLIC :: CRTM_Compute_IntAbsorber_TL
  PUBLIC :: CRTM_Compute_IntAbsorber_AD


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_AtmAbsorption_IntAbsorber.f90,v 1.5 2006/05/25 19:26:00 wd20pd Exp $'


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Compute_IntAbsorber
!
! PURPOSE:
!       Subroutine to compute the integrated absorber profiles.
!
! CATEGORY:
!       CRTM : Gas Absorption
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_IntAbsorber( Atmosphere, &  ! Input
!                                      IntAbsorber )  ! Output
!
! INPUT ARGUMENTS:
!       Atmosphere:   CRTM Atmosphere structure containing the atmospheric
!                     state data.
!                     UNITS:      N/A
!                     TYPE:       TYPE( CRTM_Atmosphere_type )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       IntAbsorber:  Array containing the profile LEVEL integrated
!                     absorber data.
!                     UNITS:      Absorber dependent
!                     TYPE:       REAL( fp_kind )
!                     DIMENSION:  Rank-2 (0:K x J)
!                     ATTRIBUTES: INTENT( OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_IntAbsorber( Atmosphere, &  ! Input
                                       IntAbsorber )  ! Output


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    TYPE( CRTM_Atmosphere_type ),        INTENT( IN )  :: Atmosphere
    REAL( fp_kind ), DIMENSION( 0:, : ), INTENT( OUT ) :: IntAbsorber



    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_IntAbsorber'

    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: k
    REAL( fp_kind ) :: dPonG
    INTEGER :: H2O_Index
    INTEGER ::  O3_Index



    !#--------------------------------------------------------------------------#
    !#                  -- INITIALISE 0'TH LEVEL AMOUNTS --                     #
    !#                                                                          #
    !# This is done so that layer differences and averages can be calculated    #
    !# simply in the predictor and transmittance routines.                      #
    !#--------------------------------------------------------------------------#

    IntAbsorber( 0, WET_ABSORBER_INDEX ) = ZERO
    IntAbsorber( 0, DRY_ABSORBER_INDEX ) = TOA_PRESSURE
    IntAbsorber( 0, OZO_ABSORBER_INDEX ) = ZERO



    !#--------------------------------------------------------------------------#
    !#            -- GET THE Atmosphere GASEOUS ABSORBER INDICES --             #
    !#--------------------------------------------------------------------------#

    H2O_Index = MINLOC( ABS( Atmosphere%Absorber_ID - H2O_ID ), DIM = 1 )
    O3_Index  = MINLOC( ABS( Atmosphere%Absorber_ID - O3_ID  ), DIM = 1 )



    !#--------------------------------------------------------------------------#
    !#                   -- NADIR LEVEL ABSORBER PROFILES --                    #
    !#--------------------------------------------------------------------------#

    ! ----------------------------
    ! Loop over layers, TOA -> SFC
    ! ----------------------------

    DO k = 1, Atmosphere%n_Layers

      ! -- Compute dP/g for the current layer
      dPonG = RECIPROCAL_GRAVITY * ( Atmosphere%Level_Pressure( k ) - Atmosphere%Level_Pressure( k-1 ) )

      ! -- Compute and accumulate the sum for the
      ! -- layer absorber amounts for each absorber
      IntAbsorber( k, WET_ABSORBER_INDEX ) = IntAbsorber( k-1, WET_ABSORBER_INDEX ) + &
                                             ( dPonG * Atmosphere%Absorber( k, H2O_Index ) )
      IntAbsorber( k, DRY_ABSORBER_INDEX ) = Atmosphere%Level_Pressure( k )

      IntAbsorber( k, OZO_ABSORBER_INDEX ) = IntAbsorber( k-1, OZO_ABSORBER_INDEX ) + &
                                             ( dPonG * Atmosphere%Absorber( k, O3_Index ) )

    END DO

  END SUBROUTINE CRTM_Compute_IntAbsorber


!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Compute_IntAbsorber_TL
!
! PURPOSE:
!       Subroutine to compute the tangent-linear integrated absorber profiles.
!
! CATEGORY:
!       CRTM : Gas Absorption
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_IntAbsorber_TL( Atmosphere,    &  ! Input
!                                         Atmosphere_TL, &  ! Input
!                                         IntAbsorber_TL )  ! Output
!
! INPUT ARGUMENTS:
!       Atmosphere:     CRTM Atmosphere structure containing the atmospheric
!                       state data.
!                       UNITS:      N/A
!                       TYPE:       TYPE( CRTM_Atmosphere_type )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       Atmosphere_TL:  CRTM Atmosphere structure containing the tangent-linear
!                       atmospheric state data, i.e. the perturbations.
!                       UNITS:      N/A
!                       TYPE:       TYPE( CRTM_Atmosphere_type )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       IntAbsorber_TL: Array containing the tangent-linear profile LEVEL
!                       integrated absorber data.
!                       UNITS:      Absorber dependent
!                       TYPE:       REAL( fp_kind )
!                       DIMENSION:  Rank-2 (0:K x J)
!                       ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! CALLS:
!       None
!
! SIDE EFFECTS:
!       None known.
!
! RESTRICTIONS:
!       None.
!
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_IntAbsorber_TL( Atmosphere,    &  ! Input
                                          Atmosphere_TL, &  ! Input
                                          IntAbsorber_TL )  ! Output



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    TYPE( CRTM_Atmosphere_type ),        INTENT( IN )  :: Atmosphere
    TYPE( CRTM_Atmosphere_type ),        INTENT( IN )  :: Atmosphere_TL
    REAL( fp_kind ), DIMENSION( 0:, : ), INTENT( OUT ) :: IntAbsorber_TL


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_IntAbsorber_TL'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: k
    REAL( fp_kind ) :: dPonG
    REAL( fp_kind ) :: dPonG_TL
    INTEGER :: H2O_Index
    INTEGER ::  O3_Index



    !#--------------------------------------------------------------------------#
    !#                  -- INITIALISE 0'TH LEVEL AMOUNTS --                     #
    !#                                                                          #
    !# This is done so that layer differences and averages can be calculated    #
    !# simply in the predictor and transmittance routines.                      #
    !#--------------------------------------------------------------------------#

    IntAbsorber_TL( 0, : ) = ZERO



    !#--------------------------------------------------------------------------#
    !#            -- GET THE Atmosphere GASEOUS ABSORBER INDICES --             #
    !#--------------------------------------------------------------------------#

    H2O_Index = MINLOC( ABS( Atmosphere%Absorber_ID - H2O_ID ), DIM = 1 )
    O3_Index  = MINLOC( ABS( Atmosphere%Absorber_ID - O3_ID  ), DIM = 1 )



    !#--------------------------------------------------------------------------#
    !#           -- NADIR LEVEL TANGENT-LINEAR ABSORBER PROFILES --             #
    !#--------------------------------------------------------------------------#

    ! ----------------------------
    ! Loop over layers, TOA -> SFC
    ! ----------------------------

    DO k = 1, Atmosphere_TL%n_Layers

      ! -- Compute dP/g for the current layer
      dPonG    = RECIPROCAL_GRAVITY * ( Atmosphere%Level_Pressure( k )    - Atmosphere%Level_Pressure( k-1 ) )
      dPonG_TL = RECIPROCAL_GRAVITY * ( Atmosphere_TL%Level_Pressure( k ) - Atmosphere_TL%Level_Pressure( k-1 ) )

      ! -- Compute and accumulate the sum for the
      ! -- layer absorber amounts for each absorber
      IntAbsorber_TL( k, WET_ABSORBER_INDEX ) = IntAbsorber_TL( k-1, WET_ABSORBER_INDEX ) + &
                                                ( dPonG * Atmosphere_TL%Absorber( k, H2O_Index ) ) + &
                                                ( dPonG_TL * Atmosphere%Absorber( k, H2O_Index ) )

      IntAbsorber_TL( k, DRY_ABSORBER_INDEX ) = Atmosphere_TL%Level_Pressure( k )

      IntAbsorber_TL( k, OZO_ABSORBER_INDEX ) = IntAbsorber_TL( k-1, OZO_ABSORBER_INDEX ) + &
                                                ( dPonG * Atmosphere_TL%Absorber( k, O3_Index ) ) + &
                                                ( dPonG_TL * Atmosphere%Absorber( k, O3_Index ) )

    END DO

  END SUBROUTINE CRTM_Compute_IntAbsorber_TL


!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Compute_IntAbsorber_AD
!
! PURPOSE:
!       Subroutine to compute the adjoint of the integrated absorber profiles.
!
! CATEGORY:
!       CRTM : Gas Absorption
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_IntAbsorber_AD( Atmosphere,     &  ! Input
!                                         IntAbsorber_AD, &  ! Input
!                                         Atmosphere_AD,  &  ! Output
!
! INPUT ARGUMENTS:
!       Atmosphere:     CRTM Atmosphere structure containing the atmospheric
!                       state data.
!                       UNITS:      N/A
!                       TYPE:       TYPE( CRTM_Atmosphere_type )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       IntAbsorber_AD: Array containing the adjoint profile LEVEL
!                       integrated absorber data.
!                       ** THIS ARGUMENT IS SET TO ZERO ON OUTPUT **.
!                       UNITS:      Absorber dependent
!                       TYPE:       REAL( fp_kind )
!                       DIMENSION:  Rank-2 (0:K x J)
!                       ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Atmosphere_AD:  CRTM Atmosphere structure containing the adjoint
!                       atmospheric state data, i.e. the Jacobians.
!                       UNITS:      N/A
!                       TYPE:       TYPE( CRTM_Atmosphere_type )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! CALLS:
!       None
!
! SIDE EFFECTS:
!       The input argument IntAbsorber_AD is set to zero on output.
!
! RESTRICTIONS:
!       None.
!
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_IntAbsorber_AD( Atmosphere,     &  ! Input
                                          IntAbsorber_AD, &  ! Input
                                          Atmosphere_AD   )  ! Output


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    TYPE( CRTM_Atmosphere_type ),        INTENT( IN )     :: Atmosphere
    REAL( fp_kind ), DIMENSION( 0:, : ), INTENT( IN OUT ) :: IntAbsorber_AD  ! 0:K x J

    ! -- Outputs
    TYPE( CRTM_Atmosphere_type ),        INTENT( IN OUT ) :: Atmosphere_AD


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: k
    REAL( fp_kind ) :: dPonG
    REAL( fp_kind ) :: dPonG_AD
    INTEGER :: H2O_Index
    INTEGER ::  O3_Index



    !#--------------------------------------------------------------------------#
    !#            -- GET THE Atmosphere GASEOUS ABSORBER INDICES --             #
    !#--------------------------------------------------------------------------#

    H2O_Index = MINLOC( ABS( Atmosphere%Absorber_ID - H2O_ID ), DIM = 1 )
    O3_Index  = MINLOC( ABS( Atmosphere%Absorber_ID - O3_ID  ), DIM = 1 )



    !#--------------------------------------------------------------------------#
    !#               -- ADJOINT OF THE NADIR ABSORBER PROFILES --               #
    !#--------------------------------------------------------------------------#

    ! ----------------------------
    ! Loop over layers, SFC -> TOA
    ! ----------------------------

    DO k = Atmosphere_AD%n_Layers, 1, -1

      ! -- Compute dP/g for the current layer
      dPonG = RECIPROCAL_GRAVITY * ( Atmosphere%Level_Pressure( k ) - Atmosphere%Level_Pressure( k-1 ) )

      ! -- Ozone amount adjoint
      Atmosphere_AD%Absorber( k, O3_Index ) = Atmosphere_AD%Absorber( k, O3_Index ) + &
                                              ( dPonG * IntAbsorber_AD( k, OZO_ABSORBER_INDEX ) )

      ! -- Pressure adjoint
      Atmosphere_AD%Level_Pressure( k ) = Atmosphere_AD%Level_Pressure( k ) + IntAbsorber_AD( k, DRY_ABSORBER_INDEX )

      ! -- Water vapor amount adjoint
      Atmosphere_AD%Absorber( k, H2O_Index ) = Atmosphere_AD%Absorber( k, H2O_Index ) + &
                                               ( dPonG * IntAbsorber_AD( k, WET_ABSORBER_INDEX ) )


      ! -- dP/g adjoint
      dPonG_AD = ( Atmosphere%Absorber( k,  O3_Index ) * IntAbsorber_AD( k, OZO_ABSORBER_INDEX ) ) + &
                 ( Atmosphere%Absorber( k, H2O_Index ) * IntAbsorber_AD( k, WET_ABSORBER_INDEX ) )

      Atmosphere_AD%Level_Pressure( k-1 ) = Atmosphere_AD%Level_Pressure( k-1 ) - ( RECIPROCAL_GRAVITY * dPonG_AD )
      Atmosphere_AD%Level_Pressure(  k  ) = Atmosphere_AD%Level_Pressure(  k  ) + ( RECIPROCAL_GRAVITY * dPonG_AD )

      ! -- Previous layer absorber amounts
      IntAbsorber_AD( k-1, OZO_ABSORBER_INDEX ) = IntAbsorber_AD( k-1, OZO_ABSORBER_INDEX ) + &
                                                  IntAbsorber_AD(  k,  OZO_ABSORBER_INDEX )
      IntAbsorber_AD(  k,  OZO_ABSORBER_INDEX ) = ZERO

      IntAbsorber_AD(  k,  DRY_ABSORBER_INDEX ) = ZERO

      IntAbsorber_AD( k-1, WET_ABSORBER_INDEX ) = IntAbsorber_AD( k-1, WET_ABSORBER_INDEX ) + &
                                                  IntAbsorber_AD(  k,  WET_ABSORBER_INDEX )
      IntAbsorber_AD(  k,  WET_ABSORBER_INDEX ) = ZERO

    END DO

  END SUBROUTINE CRTM_Compute_IntAbsorber_AD

END MODULE CRTM_AtmAbsorption_IntAbsorber


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: CRTM_AtmAbsorption_IntAbsorber.f90,v 1.5 2006/05/25 19:26:00 wd20pd Exp $
!
! $Date: 2006/05/25 19:26:00 $
!
! $Revision: 1.5 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_AtmAbsorption_IntAbsorber.f90,v $
! Revision 1.5  2006/05/25 19:26:00  wd20pd
! - Removed redundant parameter definitions.
!
! Revision 1.4  2006/05/02 14:58:34  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.3  2005/03/28 16:26:08  paulv
! - Removed all the first layer specific code. Previously, this was needed
!   because the level pressures were only specified for the layer dimensions
!   (layer 1->K) therefore the first level (level "0") was set to a default
!   TOA value. Now, the level pressures are specified at levels 0->K so the
!   first layer specific code is no longer needed.
!
! Revision 1.2  2005/01/28 21:09:45  paulv
! - Cosmetic changes only.
!
! Revision 1.1  2005/01/21 18:28:01  paulv
! Initial checkin. Untested.
!
!
!
!



