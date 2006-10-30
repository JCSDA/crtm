!--------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_AtmOptics
!
! PURPOSE:
!       Module containing routines to combining the optical properties from the
!       CRTM AtmAbsorption, CloudScatter and AerosolScatter results.
!
! CATEGORY:
!       CRTM : AtmOptics
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_AtmOptics
!
! MODULES:
!       Type_Kinds:                 Module containing data type kind definitions.
!
!       CRTM_Parameters:            Module of parameter definitions for the CRTM.
!                                   USEs: TYPE_KINDS module
!
!       CRTM_AtmAbsorption_Define:  Module defining the CRTM AtmAbsorption
!                                   structure and containing routines to
!                                   manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!       CRTM_AtmScatter_Define:     Module defining the CRTM AtmScatter structure
!                                   and containing routines to manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
! CONTAINS:
!       PUBLIC subprograms
!       ------------------
!         CRTM_Combine_AtmOptics:      Function to combine the optical properties
!                                      from AtmAbsorption, CloudScatter, and
!                                      AerosolScatter calculations.
!
!         CRTM_Combine_AtmOptics_TL:   Function to combine the tangent-linear
!                                      optical properties from AtmAbsorption,
!                                      CloudScatter, and AerosolScatter calculations.
!
!         CRTM_Combine_AtmOptics_AD:   Function to compute the adjoint of the
!                                      combined optical properties from AtmAbsorption,
!                                      CloudScatter, and AerosolScatter calculations.
!
!       PRIVATE subprograms
!       -------------------
!       
!         None.
!
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       None known.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Quanhua Liu,    QSS Group, Inc;  Quanhua.Liu@noaa.gov  
!                       Yong Han,       NOAA/NESDIS;     Yong.Han@noaa.gov
!                       Paul van Delst, CIMSS/SSEC;      paul.vandelst@ssec.wisc.edu
!                       08-Jun-2005
!
!  Copyright (C) 2005 Yong Han, Quanhua Liu, Paul van Delst
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
!--------------------------------------------------------------------------------

MODULE CRTM_AtmOptics


  ! ---------------------
  ! Module use statements
  ! ---------------------

  USE Type_Kinds
  USE Message_Handler

  ! -- CRTM modules
  USE CRTM_Parameters
  USE CRTM_AtmAbsorption_Define, ONLY: CRTM_AtmAbsorption_type
  USE CRTM_AtmScatter_Define,    ONLY: CRTM_AtmScatter_type


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! --------------------
  ! Default visibilities
  ! --------------------

  ! -- Everything private by default
  PRIVATE

  ! -- The AtmOptics combination routines                               
  PUBLIC :: CRTM_Combine_AtmOptics
  PUBLIC :: CRTM_Combine_AtmOptics_TL
  PUBLIC :: CRTM_Combine_AtmOptics_AD


  ! -------------------------                                           
  ! PRIVATE Module parameters                                           
  ! -------------------------                                           

  ! -- RCS Id for the module                                            
  CHARACTER( * ),  PARAMETER, PRIVATE :: MODULE_RCS_ID = &              
  '$Id: CRTM_AtmOptics.f90,v 1.14 2006/05/02 14:58:34 dgroff Exp $'        


  ! --------------------------------------
  ! Structure definition to hold forward
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------

  TYPE, PUBLIC :: CRTM_AOVariables_type
    PRIVATE

    REAL( fp_kind ), DIMENSION( MAX_N_LAYERS ) :: Optical_Depth = ZERO
    REAL( fp_kind ), DIMENSION( MAX_N_LAYERS ) :: bs_Cloud      = ZERO
    REAL( fp_kind ), DIMENSION( MAX_N_LAYERS ) :: bs_Aerosol    = ZERO
    REAL( fp_kind ), DIMENSION( MAX_N_LAYERS ) :: bs            = ZERO
    REAL( fp_kind ), DIMENSION( MAX_N_LAYERS ) :: w             = ZERO

  END TYPE CRTM_AOVariables_type

CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!  *** USERS INSERT PRIVATE SUBPROGRAMS HERE ***


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
!       CRTM_Combine_AtmOptics
!
! PURPOSE:
!       Subroutine to combine the optical properties from AtmAbsorption,
!       CloudScatter, and AerosolScatter calculations.
!
! CATEGORY:
!       CRTM : RT Solution
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Combine_AtmOptics( AtmAbsorption,  &  ! Input
!                                    CloudScatter,   &  ! Input
!                                    AerosolScatter, &  ! Input
!                                    AtmOptics,      &  ! Output
!                                    AOVariables     )  ! Internal variable output
!
! INPUT ARGUMENTS:
!
!       AtmAbsorption:  Structure containing the atmospheric gas absorption
!                       data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_AtmAbsorption_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       CloudScatter:   Structure containing the cloud particle absorption and
!                       scattering parameter data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_AtmScatter_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       AerosolScatter: Structure containing the aerosol absorption and scattering
!                       parameter data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_AtmScatter_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       AtmOptics:      Structure containing the combined atmospheric optical
!                       parameters
!                       UNITS:      N/A
!                       TYPE:       CRTM_AtmScatter_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN OUT )
!
!       AOVariables:    Structure containing internal variables required for
!                       subsequent tangent-linear or adjoint model calls.
!                       The contents of this structure are NOT accessible
!                       outside of the CRTM_AtmOptics module.
!                       UNITS:      N/A
!                       TYPE:       CRTM_AOVariables_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       There is no argument checking or structure allocation performed in
!       this subroutine.
!
! COMMENTS:
!       Note the INTENT on the output AtmOptics argument is IN OUT rather than
!       just OUT. This is necessary because the argument MUST be defined upon
!       input. To prevent memory leaks, and in this case errors in accessing
!       unallocated memory, the IN OUT INTENT is a must.
!
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Combine_AtmOptics( AtmAbsorption,  &  ! Input
                                     CloudScatter,   &  ! Input
                                     AerosolScatter, &  ! Input
                                     AtmOptics,      &  ! Output
                                     AOV             )  ! Internal variable output


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    TYPE( CRTM_AtmAbsorption_type ), INTENT( IN )     :: AtmAbsorption
    TYPE( CRTM_AtmScatter_type ),    INTENT( IN )     :: CloudScatter
    TYPE( CRTM_AtmScatter_type ),    INTENT( IN )     :: AerosolScatter

    ! -- Outputs
    TYPE( CRTM_AtmScatter_type ),    INTENT( IN OUT ) :: AtmOptics

    ! -- Internal variable output
    TYPE( CRTM_AOVariables_type ),   INTENT( OUT )    :: AOV


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Combine_AtmOptics'


    ! ---------------
    ! Local variables
    ! ---------------


    INTEGER :: i, k, l
    REAL( fp_kind ) :: d, g
    REAL( fp_kind ) :: Omd
    REAL( fp_kind ) :: c, rl



    !#--------------------------------------------------------------------------#
    !#                -- INITIALISE OUTPUT STRUCTURE VARIABLES --               #
    !#                                                                          #
    !# These forward variables are only computed if there is significant        #
    !# scattering. Otherwise, they're set to zero                               #
    !#--------------------------------------------------------------------------#

    AtmOptics%Phase_Coefficient     = ZERO
    AtmOptics%Asymmetry_Factor      = ZERO
    AtmOptics%Delta_Truncation      = ZERO
    AtmOptics%Single_Scatter_Albedo = ZERO



    !#--------------------------------------------------------------------------#
    !#                         -- NO SCATTERING CASE --                         #
    !#--------------------------------------------------------------------------#

    IF( AtmOptics%n_Legendre_Terms == 0 ) THEN
      AtmOptics%Optical_Depth = AtmAbsorption%Optical_Depth
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- BEGIN MAIN LAYER LOOP --                      #
    !#--------------------------------------------------------------------------#

    Layer_Loop: DO k = 1, AtmAbsorption%n_Layers


      ! ---------------------------------------------
      ! Compute the total optical depth for the layer
      ! ---------------------------------------------

      AtmOptics%Optical_Depth(k) =  AtmAbsorption%Optical_Depth(k) + &
                                     CloudScatter%Optical_Depth(k) + &
                                   AerosolScatter%Optical_Depth(k)

      ! -- Save the unmodified optical depth
      AOV%Optical_Depth(k) = AtmOptics%Optical_Depth(k)


      ! ------------------------------------------------------
      ! Only proceed if the total optical depth is significant
      ! ------------------------------------------------------

      Significant_Optical_Depth: IF( AOV%Optical_Depth(k) > OPTICAL_DEPTH_THRESHOLD ) THEN


        ! ------------------------------------------------------
        ! Compute the cloud (_C) and aerosol (_A) scattering
        ! coefficients
        ! 
        !                  tau(scatter)
        !   w = --------------------------------
        !        tau(absorption) + tau(scatter)
        ! 
        !            bs
        !     == ---------
        !         ba + bs
        ! 
        ! 
        !     =  bs
        !       ----
        !        be
        ! 
        ! so,
        ! 
        !   bs = w.be
        ! 
        ! where w   == single scattering albedo
        !       tau == optical depth
        !       bs  == cloud/aerosol scattering coefficient
        !       ba  == cloud/aerosol absorption coefficient
        !       be  == cloud/aerosol total extinction coefficient
        !
        ! -------------------------------------------------------

        AOV%bs_Cloud(k)   =   CloudScatter%Single_Scatter_Albedo(k) *   CloudScatter%Optical_Depth(k)
        AOV%bs_Aerosol(k) = AerosolScatter%Single_Scatter_Albedo(k) * AerosolScatter%Optical_Depth(k)

        AOV%bs(k) = AOV%bs_Cloud(k) + AOV%bs_Aerosol(k)


        ! ------------------------------
        ! Only proceed if the scattering
        ! coefficient is significant
        ! ------------------------------

        Significant_Scattering: IF( AOV%bs(k) > BS_THRESHOLD) THEN


          ! ------------------------------------------------------
          ! The weighted average single scatter albedo, w, from
          ! cloud (_C), aerosol (_A), and atmospheric (_Atm) data.
          !
          !   _     w_C.tau_C + w_A.tau_A
          !   w = -------------------------
          !        tau_C + tau_A + tau_Atm
          !
          !        bs
          !     = -----
          !        tau
          !
          ! ------------------------------------------------------

          AOV%w(k) = AOV%bs(k) / AtmOptics%Optical_Depth(k)


          ! --------------------------------------------------
          ! Test if the phase coefficients need to be averaged
          ! --------------------------------------------------

          Phase_Function_Type: IF( .NOT. HGphase .AND. AtmOptics%n_Legendre_Terms > 2 ) THEN


            ! -----------------------------------------------------------
            ! Compute the weighted average phase function coefficients,
            ! P, from the cloud (_C) and aerosol (_A) data.
            !
            !   _    P_C.w_C.tau_C + P_A.w_A.tau_A
            !   P = -------------------------------
            !            w_C.tau_C + w_A.tau_A
            !
            !        (P_C.bs_C ) + (P_A.bs_A )
            !     = ---------------------------
            !                   bs
            !
            ! -----------------------------------------------------------

            DO i = 1, AtmOptics%n_Phase_Elements
              DO l = 0, AtmOptics%n_Legendre_Terms

                 AtmOptics%Phase_Coefficient(l,i,k) = &
                   ( (  CloudScatter%Phase_Coefficient(l,i,k) * AOV%bs_Cloud(k)  ) + &
                     (AerosolScatter%Phase_Coefficient(l,i,k) * AOV%bs_Aerosol(k))   ) / AOV%bs(k)

              END DO
            END DO   


            ! ------------------------------------------
            ! Recalculate the asymmetry factor and delta
            ! truncation for given n_Streams
            ! ------------------------------------------

            AtmOptics%Asymmetry_Factor(k) = AtmOptics%Phase_Coefficient(1,1,k) / ONEpointFIVE
            AtmOptics%Delta_Truncation(k) = AtmOptics%Phase_Coefficient(AtmOptics%n_Legendre_Terms,1,k)


          ELSE Phase_Function_Type

            ! -- Temporary variables
            d = AtmOptics%Delta_Truncation(k)
            g = AtmOptics%Asymmetry_Factor(k)
            Omd = ONE - d


            ! ----------------------------------------
            ! The weighted average asymmetry factor, g
            !
            !      _    (g_C.w_C.tau_C) + (g_A.w_A.tau_A)
            !      g = -----------------------------------
            !               (w_C.tau_C) + (w_A.tau_A)
            !
            !           (g_C.bs_C ) + (g_A.bs_A )
            !        = ---------------------------
            !                      bs
            !
            ! ----------------------------------------

            AtmOptics%Asymmetry_Factor(k) = &
              ( (   CloudScatter%Asymmetry_Factor(k) * AOV%bs_Cloud(k)   ) + &
                ( AerosolScatter%Asymmetry_Factor(k) * AOV%bs_Aerosol(k) )   ) / AOV%bs(k)


            ! ----------------------------------------------
            ! Compute the delta truncation for a HG function
            !
            !   d = g**L
            !
            ! where L = number of Legendre terms
            ! ----------------------------------------------

            AtmOptics%Delta_Truncation(k) = AtmOptics%Asymmetry_Factor(k)**AtmOptics%n_Legendre_Terms


            ! ----------------------------------------------------------
            ! Compute the phase coefficients for HG function
            ! Loop over Legendre terms. The phase coefficients given by,
            ! 
            !                  l
            !        2l+1     g  - d
            !   P = ------ . --------
            !         2        1 - d
            !
            ! are rewritten as,
            ! 
            !                l
            !               g  - d
            !   P = c(l) . --------
            !                1 - d
            ! 
            ! ----------------------------------------------------------

            ! -- Normalization condition for phase function for 0'th term
            AtmOptics%Phase_Coefficient(0,1,k) = POINT_5

            ! -- Legendre term loop
            DO l = 1, AtmOptics%n_Legendre_Terms - 1

              c  = REAL((2*l)+1, fp_kind ) / TWO
              rl = REAL(  l,     fp_kind )

              AtmOptics%Phase_Coefficient(l,1,k) = c * ( g**l - d ) / Omd

            END DO


            ! ----------------------------------------------------
            ! Recalculate the asymmetry factor for given n_Streams
            ! ----------------------------------------------------

            AtmOptics%Asymmetry_Factor(k) = (g - d)/Omd

          END IF Phase_Function_Type


          ! -----------------------------------------------------
          ! Redfine the total optical depth and single scattering
          ! albedo for the delta-function adjustment
          ! -----------------------------------------------------

          AtmOptics%Optical_Depth(k) = ( ONE - ( AtmOptics%Delta_Truncation(k) * AOV%w(k) )) * &
                                       AtmOptics%Optical_Depth(k)

          AtmOptics%Single_Scatter_Albedo(k) = ( ONE - AtmOptics%Delta_Truncation(k) ) * AOV%w(k)   / &
                                               ( ONE - ( AtmOptics%Delta_Truncation(k) * AOV%w(k) ) )

        END IF Significant_Scattering

      END IF Significant_Optical_Depth

    END DO Layer_Loop

  END SUBROUTINE CRTM_Combine_AtmOptics



!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Combine_AtmOptics_TL
!
! PURPOSE:
!       Subroutine to compute the tangent-linear form of the optical properties
!       from AtmAbsorption, CloudScatter, and AerosolScatter calculations.
!
! CATEGORY:
!       CRTM : AtmOptics
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Combine_AtmOptics_TL( AtmAbsorption,     &  ! FWD Input
!                                       CloudScatter,      &  ! FWD Input
!                                       AerosolScatter,    &  ! FWD Input
!                                       AtmOptics,         &  ! FWD Input
!                                       AtmAbsorption_TL,  &  ! TL Input
!                                       CloudScatter_TL,   &  ! TL Input
!                                       AerosolScatter_TL, &  ! TL Input
!                                       AtmOptics_TL,      &  ! TL Output
!                                       AOVariables        )  ! Internal variable input
! INPUT ARGUMENTS:
!
!       AtmAbsorption:     Structure containing the atmospheric gas absorption
!                          data.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmAbsorption_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       CloudScatter:      Structure containing the cloud particle absorption and
!                          scattering parameter data.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmScatter_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       AerosolScatter:    Structure containing the aerosol absorption and scattering
!                          parameter data.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmScatter_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       AtmOptics:         Structure containing the combined atmospheric optical
!                          parameters
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmScatter_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       AtmAbsorption_TL:  Structure containing the tangent linear
!                          atmospheric gas absorption data.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmAbsorption_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       CloudScatter_TL:   Structure containing the tangent linear cloud
!                          particle absorption and scattering parameter data.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmScatter_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       AerosolScatter_TL: Structure containing the tangent linear aerosol
!                          absorption and scattering parameter data.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmScatter_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       AOVariables:       Structure containing internal forward model variables
!                          required for subsequent tangent-linear or adjoint model
!                          calls. The contents of this structure are NOT accessible
!                          outside of the CRTM_AtmOptics module.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AOVariables_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       AtmOptics_TL:      Structure containing the tangent linear combined
!                          atmospheric optical parameters
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmScatter_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTUPT ARGUMENTS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       There is no argument checking or structure allocation performed in
!       this subroutine.
!
! COMMENTS:
!       Note the INTENT on the output AtmOptics_TL argument is IN OUT rather than
!       just OUT. This is necessary because the argument MUST be defined upon
!       input. To prevent memory leaks, and in this case errors in accessing
!       unallocated memory, the IN OUT INTENT is a must.
!
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Combine_AtmOptics_TL( AtmAbsorption,     &  ! FWD Input
                                        CloudScatter,      &  ! FWD Input
                                        AerosolScatter,    &  ! FWD Input
                                        AtmOptics,         &  ! FWD Input
                                        AtmAbsorption_TL,  &  ! TL Input
                                        CloudScatter_TL,   &  ! TL Input
                                        AerosolScatter_TL, &  ! TL Input
                                        AtmOptics_TL,      &  ! TL Output
                                        AOV                )  ! Internal variable input



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#
            
    ! ---------
    ! Arguments
    ! ---------

    ! -- FWD Inputs
    TYPE( CRTM_AtmAbsorption_type ), INTENT( IN )     :: AtmAbsorption
    TYPE( CRTM_AtmScatter_type ),    INTENT( IN )     :: CloudScatter
    TYPE( CRTM_AtmScatter_type ),    INTENT( IN )     :: AerosolScatter
    TYPE( CRTM_AtmScatter_type ),    INTENT( IN )     :: AtmOptics

    ! -- TL Inputs
    TYPE( CRTM_AtmAbsorption_type ), INTENT( IN )     :: AtmAbsorption_TL
    TYPE( CRTM_AtmScatter_type ),    INTENT( IN )     :: CloudScatter_TL
    TYPE( CRTM_AtmScatter_type ),    INTENT( IN )     :: AerosolScatter_TL

    ! -- Outputs
    TYPE( CRTM_AtmScatter_type ),    INTENT( IN OUT ) :: AtmOptics_TL

    ! -- Internal variable input
    TYPE( CRTM_AOVariables_type ),   INTENT( IN )     :: AOV


    ! ----------------
    ! Local parameters
    ! ----------------
                               
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Combine_AtmOptics_TL'

                                           
    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, k, l
    REAL( fp_kind ) :: d, g
    REAL( fp_kind ) :: Omd
    REAL( fp_kind ) :: c, rl
    REAL( fp_kind ) :: w_TL
    REAL( fp_kind ) :: bs_Cloud_TL, bs_Aerosol_TL, bs_TL



    !#--------------------------------------------------------------------------#
    !#                -- INITIALISE OUTPUT STRUCTURE VARIABLES --               #
    !#                                                                          #
    !# These tangent-linear variables are only computed if there is significant #
    !# scattering. Otherwise, they're set to zero                               #
    !#--------------------------------------------------------------------------#

    AtmOptics_TL%Phase_Coefficient     = ZERO
    AtmOptics_TL%Asymmetry_Factor      = ZERO
    AtmOptics_TL%Delta_Truncation      = ZERO
    AtmOptics_TL%Single_Scatter_Albedo = ZERO



    !#--------------------------------------------------------------------------#
    !#                          -- NO SCATTERING CASE --                        #
    !#--------------------------------------------------------------------------#

    IF( AtmOptics%n_Legendre_Terms == 0 ) THEN
      AtmOptics_TL%Optical_Depth = AtmAbsorption_TL%Optical_Depth
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- BEGIN MAIN LAYER LOOP --                      #
    !#--------------------------------------------------------------------------#

    Layer_Loop: DO k = 1, AtmAbsorption%n_Layers


      ! --------------------------------
      ! Compute the tangent linear total
      ! optical depth for the layer
      ! --------------------------------

      AtmOptics_TL%Optical_Depth(k) = AtmAbsorption_TL%Optical_Depth(k) + &
                                       CloudScatter_TL%Optical_Depth(k) + &
                                     AerosolScatter_TL%Optical_Depth(k)


      ! ----------------------------------------------------------
      ! Only proceed if the total FWD optical depth is significant
      ! ----------------------------------------------------------
                     
      Significant_Optical_Depth: IF( AOV%Optical_Depth(k) > OPTICAL_DEPTH_THRESHOLD ) THEN


        ! ------------------------------------------------------
        ! Compute the tangent-linear cloud (_C) and aerosol (_A)
        ! scattering coefficients
        ! 
        !                  tau(scatter)
        !   w = --------------------------------
        !        tau(absorption) + tau(scatter)
        ! 
        !            bs
        !     == ---------
        !         ba + bs
        ! 
        ! 
        !     =  bs
        !       ----
        !        be
        ! 
        ! so,
        ! 
        !   bs = w.be
        ! 
        ! where w   == single scattering albedo
        !       tau == optical depth
        !       bs  == cloud/aerosol scattering coefficient
        !       ba  == cloud/aerosol absorption coefficient
        !       be  == cloud/aerosol total extinction coefficient
        ! 
        ! so,
        !   bs_C = w_C * tau_C
        !   bs_A = w_A * tau_A
        !   bs   = bs_C + bs_A
        ! 
        ! and,
        ! 
        !   bs_C_TL = ( w_C_TL * tau_C ) + ( w_C * tau_C_TL )
        !   bs_A_TL = ( w_A_TL * tau_A ) + ( w_A * tau_A_TL )
        !   b_TL    = bs_C_TL + bs_A_TL
        !
        ! ------------------------------------------------------

        
        bs_Cloud_TL   = (   CloudScatter_TL%Single_Scatter_Albedo(k) *      CloudScatter%Optical_Depth(k) ) + &
                        (      CloudScatter%Single_Scatter_Albedo(k) *   CloudScatter_TL%Optical_Depth(k) )
        bs_Aerosol_TL = ( AerosolScatter_TL%Single_Scatter_Albedo(k) *    AerosolScatter%Optical_Depth(k) ) + &
                        (    AerosolScatter%Single_Scatter_Albedo(k) * AerosolScatter_TL%Optical_Depth(k) )
        bs_TL = bs_Cloud_TL + bs_Aerosol_TL


        ! ----------------------------------
        ! Only proceed if the FWD scattering
        ! coefficient is significant
        ! ----------------------------------
                                           
        Significant_Scattering: IF( AOV%bs(k) > BS_THRESHOLD) THEN


          ! ------------------------------------------------------
          ! The weighted average single scatter albedo, w, from
          ! cloud (_C), aerosol (_A), and atmospheric (_Atm) data.
          !
          !   _     w_C.tau_C + w_A.tau_A
          !   w = -------------------------
          !        tau_C + tau_A + tau_Atm
          !
          !        bs
          !     = -----
          !        tau
          !
          !     _       bs_TL       w.tau_TL
          ! So, w_TL = -------  -  ----------
          !              tau          tau
          !
          ! ------------------------------------------------------

          w_TL = ( bs_TL / AOV%Optical_Depth(k) ) - &
                 ( AtmOptics_TL%Optical_Depth(k) * AOV%w(k) / AOV%Optical_Depth(k) )


          ! --------------------------------------------------
          ! Test if the phase coefficients need to be averaged
          ! --------------------------------------------------
                                         
          Phase_Function_Type: IF( .NOT. HGphase .AND. AtmOptics%n_Legendre_Terms > 2 ) THEN


            ! -----------------------------------------------------------
            ! Compute the tangent-linear weighted average phase function
            ! coefficients, P, from the cloud (_C) and aerosol (_A) data.
            !
            !      _    (P_C.w_C.tau_C) + (P_A.w_A.tau_A)
            !      P = -----------------------------------
            !               (w_C.tau_C) + (w_A.tau_A)
            !
            !           (P_C.bs_C ) + (P_A.bs_A )
            !        = ---------------------------
            !                      bs
            !                                                                                  _
            !     _       (P_C_TL.bs_C ) + (P_C.bs_C_TL ) + (P_A_TL.bs_A ) + (P_A.bs_A_TL ) - (P.bs_TL)
            ! So, P_TL = -------------------------------------------------------------------------------
            !                                                bs
            !
            ! -----------------------------------------------------------

            DO i = 1, AtmOptics%n_Phase_Elements
              DO l = 0, AtmOptics%n_Legendre_Terms

                 AtmOptics_TL%Phase_Coefficient(l,i,k) = &
                  ( ( CloudScatter_TL%Phase_Coefficient(l,i,k)*AOV%bs_Cloud(k) ) + &
                    (    CloudScatter%Phase_Coefficient(l,i,k)*    bs_Cloud_TL ) + &
                    ( AerosolScatter_TL%Phase_Coefficient(l,i,k)*AOV%bs_Aerosol(k) ) + &
                    (    AerosolScatter%Phase_Coefficient(l,i,k)*    bs_Aerosol_TL ) - &
                    ( AtmOptics%Phase_Coefficient(l,i,k) * bs_TL ) ) / AOV%bs(k)

              END DO
            END DO


            ! -----------------------------------------------
            ! Recalculate the tangent-linear asymmetry factor
            ! and delta truncation for given n_Streams
            ! -----------------------------------------------

            AtmOptics_TL%Asymmetry_Factor(k) = AtmOptics_TL%Phase_Coefficient(1,1,k) / ONEpointFIVE
            AtmOptics_TL%Delta_Truncation(k) = AtmOptics_TL%Phase_Coefficient(AtmOptics%n_Legendre_Terms,1,k)


          ELSE Phase_Function_Type

            ! -- Temporary FWD variables
            d   = AtmOptics%Delta_Truncation(k)
            g   = AtmOptics%Asymmetry_Factor(k)
            Omd = ONE - d


            ! ----------------------------------------
            ! The weighted average asymmetry factor, g
            !
            !      _    (g_C.w_C.tau_C) + (g_A.w_A.tau_A)
            !      g = -----------------------------------
            !               (w_C.tau_C) + (w_A.tau_A)
            !
            !           (g_C.bs_C ) + (g_A.bs_A )
            !        = ---------------------------
            !                      bs
            !                                                                                  _
            !     _       (g_C_TL.bs_C ) + (g_C.bs_C_TL ) + (g_A_TL.bs_A ) + (g_A.bs_A_TL ) - (g.bs_TL)
            ! So, g_TL = -------------------------------------------------------------------------------
            !                                                bs
            !
            ! ----------------------------------------

            AtmOptics_TL%Asymmetry_Factor(k) = &
              ( (   CloudScatter_TL%Asymmetry_Factor(k) * AOV%bs_Cloud(k) ) + &
                (      CloudScatter%Asymmetry_Factor(k) *     bs_Cloud_TL ) + &
                ( AerosolScatter_TL%Asymmetry_Factor(k) * AOV%bs_Aerosol(k) ) + &
                (    AerosolScatter%Asymmetry_Factor(k) *     bs_Aerosol_TL ) - &
                ( AtmOptics%Asymmetry_Factor(k) * bs_TL ) ) / AOV%bs(k)


            ! -------------------------------------------------------------
            ! Compute the tangent-linear delta truncation for a HG function
            !
            !   d = g**L
            !
            ! where L = number of Legendre terms, so
            !
            !   d_TL = L . g**(L-1) . g_TL 
            !
            ! -------------------------------------------------------------

            AtmOptics_TL%Delta_Truncation(k) = &
              REAL( AtmOptics%n_Legendre_Terms, fp_kind ) * &
              AtmOptics_TL%Asymmetry_Factor(k) * &
              ( AtmOptics%Asymmetry_Factor(k)**(AtmOptics%n_Legendre_Terms-1) )


            ! -------------------------------------------------------------
            ! Compute the tangent-linear phase coefficients for HG function
            ! Loop over Legendre terms. The phase coefficients given by,
            !
            !                   l
            !         2l+1     g  - d
            !    P = ------ . --------
            !          2        1 - d
            !
            !    are rewritten as,
            !
            !                 l
            !                g  - d
            !    P = c(l) . --------
            !                 1 - d
            !
            !    The tangent-linear form of the above is,
            !
            !                    l-1
            !            c(l).l.g                ( P(l) - c(l) )
            !    P_TL = ------------ . g_TL  +  ----------------- . d_TL
            !              1 - d                      1 - d
            !
            !    The expression was order the above way to make the adjoint
            !    form easy to determine from the TL form.
            ! -------------------------------------------------------------

            ! -- Normalization condition for phase function for 0'th term
            AtmOptics_TL%Phase_Coefficient(0,1,k) = ZERO

            ! -- Th Legendre term loop
            DO l = 1, AtmOptics%n_Legendre_Terms - 1

              c  = REAL((2*l)+1, fp_kind ) / TWO
              rl = REAL(  l,     fp_kind )

              AtmOptics_TL%Phase_Coefficient(l,1,k) = &
                ( ( c * rl * g**(l-1) * AtmOptics_TL%Asymmetry_Factor(k) ) + &
                  ( ( AtmOptics%Phase_Coefficient(l,1,k) - c ) * AtmOptics_TL%Delta_Truncation(k) ) ) / Omd

            END DO


            ! ----------------------------------------------------
            ! Recalculate the asymmetry factor for given n_Streams
            !
            !      g - d                g_TL       (g - 1 ).d_TL
            ! g = ------- so, g_TL = ---------- + ---------------
            !      1 - d               1 - d          1 - d
            !
            ! Again, the TL expression above was expressed such
            ! that the adjoint form is simple to determine.
            ! ----------------------------------------------------

            AtmOptics_TL%Asymmetry_Factor(k) = &
              ( AtmOptics_TL%Asymmetry_Factor(k) + &
                ( ( g - ONE ) * AtmOptics_TL%Delta_Truncation(k) ) ) / Omd

          END IF Phase_Function_Type


          ! ----------------------------------------------------------
          ! Redefine the tangent-linear total optical depth and
          ! single scattering albedo for the delta-function adjustment
          !
          ! The expressions below are ordered to make the adjoint
          ! form easy to determine from the TL form.
          ! ----------------------------------------------------------

          ! -- The optical depth
          ! --
          ! -- tau = ( 1 - d.w ) . tau
          ! --
          ! -- so,
          ! --
          ! -- tau_TL = ( 1 - d.w ).tau_TL - d.tau.w_TL - w.tau.d_TL
          ! --
          ! -- Note that the optical depth from the AOV structure is
          ! -- used on the RHS of these expressions.

          AtmOptics_TL%Optical_Depth(k) = &
            ( ( ONE - ( AtmOptics%Delta_Truncation(k) * AOV%w(k) ) ) * AtmOptics_TL%Optical_Depth(k) ) - &
            ( AtmOptics%Delta_Truncation(k) * AOV%Optical_Depth(k) * w_TL ) - &
            ( AOV%w(k) * AOV%Optical_Depth(k) * AtmOptics_TL%Delta_Truncation(k) )


          ! -- The single scatter albedo, SSA
          ! --
          ! --        (1 - d).w
          ! -- SSA = -----------
          ! --         1 - d.w
          ! --
          ! -- so,
          ! --
          ! --           ( 1 - d + SSA.d )              ( SSA - 1 ).w
          ! -- SSA_TL = ------------------- . w_TL  +  --------------- . d_TL
          ! --               1 - d.w                       1 - d.w

          AtmOptics_TL%Single_Scatter_Albedo(k) = &
            ( ( ( ONE - AtmOptics%Delta_Truncation(k) + &
                  ( AtmOptics%Single_Scatter_Albedo(k)*AtmOptics%Delta_Truncation(k) ) ) * w_TL ) + &
              ( ( AtmOptics%Single_Scatter_Albedo(k) - ONE ) * AOV%w(k) * AtmOptics_TL%Delta_Truncation(k) ) ) / &
            ( ONE - ( AtmOptics%Delta_Truncation(k) * AOV%w(k) ) )

        END IF Significant_Scattering
                           
      ENDIF Significant_Optical_Depth

    END DO Layer_Loop
                                      
  END SUBROUTINE CRTM_Combine_AtmOptics_TL



!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Combine_AtmOptics_AD
!
! PURPOSE:
!       Subroutine to compute the adjoint form of the optical properties
!       from AtmAbsorption, CloudScatter, and AerosolScatter calculations.
!
! CATEGORY:
!       CRTM : AtmOptics
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Combine_AtmOptics_AD( AtmAbsorption,     &  ! FWD Input
!                                       CloudScatter,      &  ! FWD Input
!                                       AerosolScatter,    &  ! FWD Input
!                                       AtmOptics,         &  ! FWD Input
!                                       AtmOptics_AD,      &  ! AD Input
!                                       AtmAbsorption_AD,  &  ! AD Output
!                                       CloudScatter_AD,   &  ! AD Output
!                                       AerosolScatter_AD, &  ! AD Output
!                                       RTVariables        )  ! Internal variable input
!
! INPUT ARGUMENTS:
!       AtmAbsorption:     Structure containing the atmospheric gas absorption
!                          data.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmAbsorption_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       CloudScatter:      Structure containing the cloud particle absorption and
!                          scattering parameter data.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmScatter_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       AerosolScatter:    Structure containing the aerosol absorption and scattering
!                          parameter data.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmScatter_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       AtmOptics:         Structure containing the combined atmospheric optical
!                          parameters
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmScatter_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       AtmOptics_AD:      Structure containing the combined adjoint atmospheric
!                          optical parameters.
!                          NOTE: The components of this structures are all zeroed
!                                upon exit from this routine.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmScatter_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN OUT )
!
!       AOVariables:       Structure containing internal forward model variables
!                          required for subsequent tangent-linear or adjoint model
!                          calls. The contents of this structure are NOT accessible
!                          outside of the CRTM_AtmOptics module.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AOVariables_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       AtmAbsorption_AD:  Structure containing the adjoint atmospheric gas
!                          absorption data.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmAbsorption_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN OUT )
!
!       CloudScatter_AD:   Structure containing the adjoint cloud particle
!                          absorption and scattering parameter data.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmScatter_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN OUT )
!
!       AerosolScatter_AD: Structure containing the adjoint aerosol
!                          absorption and scattering parameter data.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmScatter_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTUPT ARGUMENTS:
!       None.
!
! SIDE EFFECTS:
!       The input AtmOptics_AD structure components are zeroed upon exit.
!
! RESTRICTIONS:
!       There is no argument checking or structure allocation performed in
!       this subroutine.
!
! COMMENTS:
!       Note the INTENT on the output adjoint arguments is IN OUT rather than
!       just OUT. This is necessary because the arguments MUST be defined upon
!       input. To prevent memory leaks, and in this case errors in accessing
!       unallocated memory, the IN OUT INTENT is a must.
!
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Combine_AtmOptics_AD( AtmAbsorption,     &  ! FWD Input
                                        CloudScatter,      &  ! FWD Input
                                        AerosolScatter,    &  ! FWD Input
                                        AtmOptics,         &  ! FWD Input
                                        AtmOptics_AD,      &  ! AD Input
                                        AtmAbsorption_AD,  &  ! AD Output
                                        CloudScatter_AD,   &  ! AD Output
                                        AerosolScatter_AD, &  ! AD Output
                                        AOV                )  ! Internal variable input

    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#
            
    ! ---------
    ! Arguments
    ! ---------

    ! -- FWD Inputs
    TYPE( CRTM_AtmAbsorption_type ), INTENT( IN )     :: AtmAbsorption
    TYPE( CRTM_AtmScatter_type ),    INTENT( IN )     :: CloudScatter
    TYPE( CRTM_AtmScatter_type ),    INTENT( IN )     :: AerosolScatter
    TYPE( CRTM_AtmScatter_type ),    INTENT( IN )     :: AtmOptics

    ! -- AD Inputs
    TYPE( CRTM_AtmScatter_type ),    INTENT( IN OUT ) :: AtmOptics_AD

    ! -- AD Outputs
    TYPE( CRTM_AtmAbsorption_type ), INTENT( IN OUT ) :: AtmAbsorption_AD
    TYPE( CRTM_AtmScatter_type ),    INTENT( IN OUT ) :: CloudScatter_AD
    TYPE( CRTM_AtmScatter_type ),    INTENT( IN OUT ) :: AerosolScatter_AD

    ! -- Internal variable input
    TYPE( CRTM_AOVariables_type ),   INTENT( IN )     :: AOV


    ! ----------------
    ! Local parameters
    ! ----------------
                               
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Combine_AtmOptics_AD'

                                           
    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, k, l
    REAL( fp_kind ) :: d, g
    REAL( fp_kind ) :: Omd
    REAL( fp_kind ) :: c, rl
    REAL( fp_kind ) :: w_AD
    REAL( fp_kind ) :: bs_Cloud_AD, bs_Aerosol_AD, bs_AD



    !#--------------------------------------------------------------------------#
    !#             -- INITIALISE OUTPUT ADJOINT STRUCTURE VARIABLES --          #
    !#                                                                          #
    !# These adjoint variables are only computed if there is significant        #
    !# scattering. Otherwise, they're set to zero. Strictly, these variables    #
    !# should be zeroed in the Adjoint and K-Matrix modules, but initialisation #
    !# here minimises duplicate code.                                           #
    !#--------------------------------------------------------------------------#
                                            
    AerosolScatter_AD%Phase_Coefficient     = ZERO
    AerosolScatter_AD%Asymmetry_Factor      = ZERO
    AerosolScatter_AD%Delta_Truncation      = ZERO
    AerosolScatter_AD%Optical_Depth         = ZERO
    AerosolScatter_AD%Single_Scatter_Albedo = ZERO

    CloudScatter_AD%Phase_Coefficient     = ZERO
    CloudScatter_AD%Asymmetry_Factor      = ZERO
    CloudScatter_AD%Delta_Truncation      = ZERO
    CloudScatter_AD%Optical_Depth         = ZERO
    CloudScatter_AD%Single_Scatter_Albedo = ZERO

    AtmAbsorption_AD%Optical_Depth = ZERO 



    !#--------------------------------------------------------------------------#
    !#                          -- NO SCATTERING CASE --                        #
    !#--------------------------------------------------------------------------#

    IF( AtmOptics%n_Legendre_Terms == 0 ) THEN

      ! -- Assign the AtmAbsorption optical depth
      AtmAbsorption_AD%Optical_Depth = AtmOptics_AD%Optical_Depth

      ! -- Zero the input adjoint structure components
      AtmOptics_AD%Optical_Depth         = ZERO
      AtmOptics_AD%Single_Scatter_Albedo = ZERO
      AtmOptics_AD%Delta_Truncation      = ZERO
      AtmOptics_AD%Asymmetry_Factor      = ZERO
      AtmOptics_AD%Phase_Coefficient     = ZERO

      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- BEGIN MAIN LAYER LOOP --                      #
    !#--------------------------------------------------------------------------#
                                            
    Layer_Loop: DO k = AtmAbsorption%n_Layers, 1, -1


      ! -----------------------------------------------------
      ! Initialise local, layer independent adjoint variables
      ! -----------------------------------------------------

      w_AD          = ZERO
      bs_AD         = ZERO
      bs_Aerosol_AD = ZERO
      bs_Cloud_AD   = ZERO

                     
      ! ------------------------------------------------------
      ! Only proceed if the total optical depth is significant
      ! ------------------------------------------------------
                     
      Significant_Optical_Depth: IF( AOV%Optical_Depth(k) > OPTICAL_DEPTH_THRESHOLD ) THEN


        ! ------------------------------
        ! Only proceed if the scattering
        ! coefficient is significant
        ! ------------------------------
                                           
        Significant_Scattering: IF( AOV%bs(k) > BS_THRESHOLD) THEN


          ! ---------------------------------------------------
          ! Compute the adjoint total optical depth and single
          ! scattering albedo for the delta function adjustment
          ! ---------------------------------------------------

          ! -- The tangent-linear single scatter albedo, SSA_TL
          ! --
          ! --             ( 1 - d + SSA.d )              ( SSA - 1 ).w
          ! --   SSA_TL = ------------------- . w_TL  +  --------------- . d_TL
          ! --                  1 - d.w                      1 - d.w
          ! --
          ! -- so,
          ! --                   ( SSA - 1 ).w
          ! --   d_AD = d_AD + ---------------- . SSA_AD
          ! --                      1 - d.w
          ! --
          ! --                  ( 1 - d + SSA.d ) 
          ! --   w_AD = w_AD + ------------------- . SSA_AD
          ! --                       1 - d.w     
          ! --
          ! --   SSA_AD = 0

          AtmOptics_AD%Delta_Truncation(k) = AtmOptics_AD%Delta_Truncation(k) + &
            ( ( AtmOptics%Single_Scatter_Albedo(k) - ONE ) * AOV%w(k) * AtmOptics_AD%Single_Scatter_Albedo(k) / &
              ( ONE - ( AtmOptics%Delta_Truncation(k) * AOV%w(k) ) ) )

          w_AD = w_AD + ( ( ONE - AtmOptics%Delta_Truncation(k) + &
                            ( AtmOptics%Single_Scatter_Albedo(k)*AtmOptics%Delta_Truncation(k) ) ) * &
                          AtmOptics_AD%Single_Scatter_Albedo(k) / &
                          ( ONE - ( AtmOptics%Delta_Truncation(k) * AOV%w(k) ) ) )

          AtmOptics_AD%Single_Scatter_Albedo(k) = ZERO


          ! -- The tangent-linear optical depth, tau_TL
          ! --
          ! --   tau_TL = ( 1 - d.w ).tau_TL - d.tau.w_TL - w.tau.d_TL
          ! --
          ! -- so,
          ! --
          ! --   d_AD = d_AD - w.tau.tau_AD
          ! --
          ! --   w_AD = w_AD - d.tau.tau_AD
          ! --
          ! --   tau_AD = ( 1 - d.w ).tau_AD
          ! --
          ! -- Note that the optical depth from the AOV structure is
          ! -- used on the RHS of the above expressions.

          AtmOptics_AD%Delta_Truncation(k) = AtmOptics_AD%Delta_Truncation(k) - &
            ( AOV%w(k)                    * &  ! w
              AOV%Optical_Depth(k)        * &  ! tau
              AtmOptics_AD%Optical_Depth(k) )  ! tau_AD

          w_AD = w_AD - ( AtmOptics%Delta_Truncation(k) * &  ! d
                          AOV%Optical_Depth(k)          * &  ! tau
                          AtmOptics_AD%Optical_Depth(k)   )  ! tau_AD

          AtmOptics_AD%Optical_Depth(k) = ( ONE - ( AtmOptics%Delta_Truncation(k) * AOV%w(k) ) ) * &
                                          AtmOptics_AD%Optical_Depth(k)


          ! --------------------------------------------------
          ! Test if the phase coefficients need to be averaged
          ! --------------------------------------------------
                                         
          Phase_Function_Type: IF( .NOT. HGphase .AND. AtmOptics%n_Legendre_Terms > 2 ) THEN


            ! ------------------------------------
            ! The adjoint asymmetry factor and
            ! delta truncation for given n_Streams
            !
            !   d_TL = P_TL(L,1,k)
            ! and
            !   g_TL = P_TL(1,1,k)/1.5
            !
            ! where L == number of Legendre terms, so
            !
            !   P_AD(L,1,k) = P_AD(L,1,k) + d_AD
            !   d_AD = ZERO
            ! and
            !   P_AD(1,1,k) = P_AD(1,1,k) + g_AD/1.5
            !   g_AD = ZERO
            ! ------------------------------------

            ! -- Delta truncation adjoint
            L = AtmOptics%n_Legendre_Terms
            AtmOptics_AD%Phase_Coefficient(L,1,k) = AtmOptics_AD%Phase_Coefficient(L,1,k) + &
                                                    AtmOptics_AD%Delta_Truncation(k)
            AtmOptics_AD%Delta_Truncation(k) = ZERO

            ! -- Asymmetry factor adjoint
            AtmOptics_AD%Phase_Coefficient(1,1,k) = AtmOptics_AD%Phase_Coefficient(1,1,k) + &
                                                    ( AtmOptics_AD%Asymmetry_Factor(k) / ONEpointFIVE )
            AtmOptics_AD%Asymmetry_Factor(k) = ZERO


            ! ---------------------------------------------------------
            ! Compute the adjoint of the weighted average phase
            ! function coefficients, P, from the cloud (_C) and
            ! aerosol (_A) data.
            !
            !   _       (P_C_TL.bs_C ) + (P_C.bs_C_TL ) + (P_A_TL.bs_A ) + (P_A.bs_A_TL ) - (P.bs_TL)
            !   P_TL = -------------------------------------------------------------------------------
            !                                              bs
            ! So,
            !                         _
            !                     P . P_AD
            !   bs_AD = bs_AD - -----------
            !                       bs
            !                              _
            !                        P_A . P_AD
            !   bs_A_AD = bs_A_AD + ------------
            !                            bs
            !                             _
            !                      bs_A . P_AD
            !   P_A_AD = P_A_AD + -------------
            !                          bs
            !                              _
            !                        P_C . P_AD
            !   bs_C_AD = bs_C_AD + ------------
            !                            bs
            !                             _
            !                      bs_C . P_AD
            !   P_C_AD = P_C_AD + -------------
            !                          bs
            !   _
            !   P_AD = 0
            ! 
            ! ---------------------------------------------------------

            DO i = 1, AtmOptics%n_Phase_Elements
              DO l = 0, AtmOptics%n_Legendre_Terms

                bs_AD = bs_AD - &
                  ( AtmOptics%Phase_Coefficient(l,i,k) * &
                    AtmOptics_AD%Phase_Coefficient(l,i,k) / AOV%bs(k) )

                bs_Aerosol_AD = bs_Aerosol_AD + &
                  ( AerosolScatter%Phase_Coefficient(l,i,k) * &
                    AtmOptics_AD%Phase_Coefficient(l,i,k) / AOV%bs(k) ) 

                AerosolScatter_AD%Phase_Coefficient(l,i,k) = &
                  AerosolScatter_AD%Phase_Coefficient(l,i,k) + &
                  ( AOV%bs_Aerosol(k) * AtmOptics_AD%Phase_Coefficient(l,i,k) / AOV%bs(k) )

                bs_Cloud_AD = bs_Cloud_AD + &
                  ( CloudScatter%Phase_Coefficient(l,i,k) * &
                    AtmOptics_AD%Phase_Coefficient(l,i,k) / AOV%bs(k) ) 

                CloudScatter_AD%Phase_Coefficient(l,i,k) = &
                  CloudScatter_AD%Phase_Coefficient(l,i,k) + &
                  ( AOV%bs_Cloud(k) * AtmOptics_AD%Phase_Coefficient(l,i,k) / AOV%bs(k) )

                AtmOptics_AD%Phase_Coefficient(l,i,k ) = ZERO

              END DO
            END DO

          ELSE Phase_Function_Type

            ! -- Temporary variables
            d   = AtmOptics%Delta_Truncation(k)
            g   = AtmOptics%Asymmetry_Factor(k)
            Omd = ONE - d


            ! ----------------------------------------------------
            ! The adjoint asymmetry factor for given n_Streams
            !
            !             g_TL       (g - 1 ).d_TL
            !   g_TL = ---------- + ---------------
            !            1 - d          1 - d
            !
            ! so,
            !
            !                  g - 1
            !   d_AD = d_AD + -------.g_AD
            !                  1 - d
            !
            !            g_AD
            !   g_AD = -------
            !           1 - d
            !
            ! ----------------------------------------------------

            AtmOptics_AD%Delta_Truncation(k) = AtmOptics_AD%Delta_Truncation(k) + &
              ( ( g - ONE ) * AtmOptics_AD%Asymmetry_Factor(k) / Omd )

            AtmOptics_AD%Asymmetry_Factor(k) = AtmOptics_AD%Asymmetry_Factor(k) / Omd


            ! -------------------------------------------------------
            ! Compute the adjoint phase coefficients for HG function.
            ! Loop over Legendre terms. The TL phase coefficients are 
            !
            !                    l-1
            !            c(l).l.g                ( P(l) - c(l) )
            !    P_TL = ------------ . g_TL  +  ----------------- . d_TL
            !              1 - d                      1 - d
            !
            ! so,
            !                  ( P(l) - c(l) )
            !   d_AD = d_AD + ----------------- . P_AD
            !                       1 - d
            !
            !                          l-1
            !                  c(l).l.g   
            !   g_AD = g_AD + ------------ . P_AD
            !                    1 - d
            !
            !   P_AD = 0
            !
            ! -------------------------------------------------------

            DO l = AtmOptics%n_Legendre_Terms - 1, 1, -1

              c  = REAL((2*l)+1, fp_kind ) / TWO
              rl = REAL(  l,     fp_kind )

              AtmOptics_AD%Delta_Truncation(k) = AtmOptics_AD%Delta_Truncation(k) + &
                ( ( ( AtmOptics%Phase_Coefficient(l,1,k) - c ) * AtmOptics_AD%Phase_Coefficient(l,1,k) ) / Omd )

              AtmOptics_AD%Asymmetry_Factor(k) = AtmOptics_AD%Asymmetry_Factor(k) + &
                ( c * rl * g**(l-1) * AtmOptics_AD%Phase_Coefficient(l,1,k) / Omd )

              AtmOptics_AD%Phase_Coefficient(l,1,k) = ZERO

            END DO

            ! -- Normalization condition for phase function for 0'th term
            AtmOptics_AD%Phase_Coefficient(0,1,k) = ZERO


            ! ------------------------------------------------------
            ! Compute the adjoint delta truncation for a HG function
            !
            !   d_TL = L . g**(L-1) . g_TL 
            !
            ! where L = number of Legendre terms, so
            !
            !   g_AD = g_AD  +  ( L . g**(L-1) . d_AD )
            !
            !   d_AD = 0
            !
            ! ------------------------------------------------------

            AtmOptics_AD%Asymmetry_Factor(k) = AtmOptics_AD%Asymmetry_Factor(k) + &
              ( REAL( AtmOptics%n_Legendre_Terms, fp_kind ) * &
                ( g**(AtmOptics%n_Legendre_Terms-1) ) * &
                AtmOptics_AD%Delta_Truncation(k) )

            AtmOptics_AD%Delta_Truncation(k) = ZERO


            ! ----------------------------------------------------
            ! The adjoint of the weighted average asymmetry factor
            !
            !   _       (g_C_TL.bs_C ) + (g_C.bs_C_TL ) + (g_A_TL.bs_A ) + (g_A.bs_A_TL ) - (g.bs_TL)
            !   g_TL = -------------------------------------------------------------------------------
            !                                              bs
            ! So,
            !
            !                       _
            !                   g . g_AD
            ! bs_AD = bs_AD - -----------
            !                     bs
            !                            _
            !                      g_A . g_AD
            ! bs_A_AD = bs_A_AD + ------------
            !                          bs
            !                           _
            !                    bs_A . g_AD
            ! g_A_AD = g_A_AD + -------------
            !                        bs
            !                            _
            !                      g_C . g_AD
            ! bs_C_AD = bs_C_AD + ------------
            !                          bs
            !                           _
            !                    bs_C . g_AD
            ! g_C_AD = g_C_AD + -------------
            !                        bs
            ! _
            ! g_AD = 0
            ! 
            ! ----------------------------------------------------

            bs_AD = bs_AD - ( g * AtmOptics_AD%Asymmetry_Factor(k) / AOV%bs(k) )

            bs_Aerosol_AD = bs_Aerosol_AD + &
              ( AerosolScatter%Asymmetry_Factor(k) * AtmOptics_AD%Asymmetry_Factor(k) / AOV%bs(k) ) 

            AerosolScatter_AD%Asymmetry_Factor(k) = AerosolScatter_AD%Asymmetry_Factor(k) + &
              ( AOV%bs_Aerosol(k) * AtmOptics_AD%Asymmetry_Factor(k) / AOV%bs(k) )

            bs_Cloud_AD = bs_Cloud_AD + &
              ( CloudScatter%Asymmetry_Factor(k) * AtmOptics_AD%Asymmetry_Factor(k) / AOV%bs(k) ) 

            CloudScatter_AD%Asymmetry_Factor(k) = CloudScatter_AD%Asymmetry_Factor(k) + &
              ( AOV%bs_Cloud(k) * AtmOptics_AD%Asymmetry_Factor(k) / AOV%bs(k) )

            AtmOptics_AD%Asymmetry_Factor(k) = ZERO

          END IF Phase_Function_Type


          ! ---------------------------------------------------------
          ! The adjoint of the weighted average single scatter albedo
          !
          !   _       bs_TL       w.tau_TL
          !   w_TL = -------  -  ----------
          !            tau          tau
          !
          ! So,
          !                          _
          !                      w . w_AD
          !   tau_AD = tau_AD - -----------
          !                        tau
          !                    _
          !                    w_AD
          !   bs_AD = bs_AD + ------
          !                    tau
          !   _
          !   w_AD = 0
          ! ----------------------------------------------------

          AtmOptics_AD%Optical_Depth(k) = AtmOptics_AD%Optical_Depth(k) - &
            ( AOV%w(k) * w_AD / AOV%Optical_Depth(k) )

          bs_AD = bs_AD + ( w_AD / AOV%Optical_Depth(k) )

          w_AD = ZERO

        END IF Significant_Scattering
  

        ! ------------------------------------------------------
        ! Compute the adjoint of the cloud (_C) and aerosol (_A)
        ! scattering coefficients
        ! 
        !   bs_C_TL = ( w_C_TL * tau_C ) + ( w_C * tau_C_TL )
        !   bs_A_TL = ( w_A_TL * tau_A ) + ( w_A * tau_A_TL )
        !   b_TL    = bs_C_TL + bs_A_TL
        ! 
        ! So,
        !
        !   bs_A_AD = bs_A_AD + bs_AD
        !   bs_C_AD = bs_C_AD + bs_AD
        !   bs_AD   = 0
        !
        !   tau_A_AD = tau_A_AD + ( w_A   * bs_A_AD )
        !   w_A_AD   = w_A_AD   + ( tau_A * bs_A_AD )
        !   bs_A_AD  = 0
        !
        !   tau_C_AD = tau_C_AD + ( w_C   * bs_C_AD )
        !   w_C_AD   = w_C_AD   + ( tau_C * bs_C_AD )
        !   bs_C_AD  = 0
        !
        ! ------------------------------------------------------

        ! -- Total term
        bs_Aerosol_AD = bs_Aerosol_AD + bs_AD
        bs_Cloud_AD   = bs_Cloud_AD   + bs_AD
        bs_AD = ZERO

        ! -- Aerosol terms
        AerosolScatter_AD%Optical_Depth(k) = AerosolScatter_AD%Optical_Depth(k) + &
          ( AerosolScatter%Single_Scatter_Albedo(k) * bs_Aerosol_AD )

        AerosolScatter_AD%Single_Scatter_Albedo(k) = AerosolScatter_AD%Single_Scatter_Albedo(k) + &
          ( AerosolScatter%Optical_Depth(k) * bs_Aerosol_AD )

        bs_Aerosol_AD = ZERO

        ! -- Cloud terms
        CloudScatter_AD%Optical_Depth(k) = CloudScatter_AD%Optical_Depth(k) + &
          ( CloudScatter%Single_Scatter_Albedo(k) * bs_Cloud_AD )

        CloudScatter_AD%Single_Scatter_Albedo(k) = CloudScatter_AD%Single_Scatter_Albedo(k) + &
          ( CloudScatter%Optical_Depth(k) * bs_Cloud_AD )

        bs_Cloud_AD = ZERO

      END IF Significant_Optical_Depth


      ! --------------------------------
      ! Compute the adjoint of the total
      ! optical depth for the layer
      ! --------------------------------

      AerosolScatter_AD%Optical_Depth(k) = AerosolScatter_AD%Optical_Depth(k) + AtmOptics_AD%Optical_Depth(k)
      CloudScatter_AD%Optical_Depth(k)   = CloudScatter_AD%Optical_Depth(k)   + AtmOptics_AD%Optical_Depth(k)
      AtmAbsorption_AD%Optical_Depth(k)  = AtmAbsorption_AD%Optical_Depth(k)  + AtmOptics_AD%Optical_Depth(k)

      AtmOptics_AD%Optical_Depth(k) = ZERO

    END DO Layer_Loop



    !#--------------------------------------------------------------------------#
    !#      -- INPUT ADJOINT STRUCTURE COMPONENTS NO LONGER CONTRIBUTE --       #
    !#--------------------------------------------------------------------------#

    AtmOptics_AD%Single_Scatter_Albedo = ZERO
    AtmOptics_AD%Delta_Truncation      = ZERO
    AtmOptics_AD%Asymmetry_Factor      = ZERO
    AtmOptics_AD%Phase_Coefficient     = ZERO

  END SUBROUTINE CRTM_Combine_AtmOptics_AD

END MODULE CRTM_AtmOptics


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: CRTM_AtmOptics.f90,v 1.14 2006/05/02 14:58:34 dgroff Exp $
!
! $Date: 2006/05/02 14:58:34 $
!
! $Revision: 1.14 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_AtmOptics.f90,v $
! Revision 1.14  2006/05/02 14:58:34  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.13  2006/02/07 15:06:51  paulv
! - Added no scattering case check for when the number of Legendre terms is
!   zero. All components simply assign the required output and return.
!
! Revision 1.12  2005/10/18 12:08:51  paulv
! - Added initialisation of output adjoint structure members to the AD routine.
!   Strictly this should be done in the calling code, but doing it here
!   minimises code duplication (i.e. adjoint and K-matrix module.)
!
! Revision 1.11  2005/10/12 21:32:56  paulv
! - Corrected bug in the TL and AD single scatter albedo codes.
!   The tangent-linear form was written as
!
!                ( 1 - d + SSA )              ( SSA - 1 ).w
!      SSA_TL = ----------------- . w_TL  +  --------------- . d_TL
!                    1 - d.w                    1 - d.w
!
!   but it should have been --,
!                             |
!               ( 1 - d + SSA.d )              ( SSA - 1 ).w
!      SSA_TL = ------------------- . w_TL  +  --------------- . d_TL
!                     1 - d.w                      1 - d.w
!
!   The relevant adjoint line was changed from
!
!                     ( 1 - d + SSA )
!      w_AD = w_AD + ----------------- . SSA_AD
!                         1 - d.w
!
!   to -----------------------------,
!                                   |
!                     ( 1 - d + SSA.d )
!      w_AD = w_AD + ------------------- . SSA_AD
!                          1 - d.w
!
! Revision 1.10  2005/10/12 17:20:19  paulv
! - Corrected bug in name of internal AOV variable type definition.
! - Added various missing local variable declarations.
! - Corrected bug in continuation character specification.
! - Corrected bs reference to AOV%bs(k).
! - Corrected references to Phase_Coefficient in various AtmScatter
!   functions.
!
! Revision 1.9  2005/10/06 22:06:55  paulv
! - Updated documentation so it was in sync with the code.
! - Modified order of TL and AD arguments to be consistent with FWD model.
! - Added explanatory comments.
! - Added internal variable structure definition (AOVariables_type) for
!   forward variables that are required in the TL and AD calcs.
! - Added internal variable argument to FWD (output) and TL, AD (input)
!   routines.
! - Modifed FWD, TL, and AD Routines to use internal variable structure.
!   All temporary arrays for this purpose have been removed.
!
! Revision 1.8  2005/10/03 15:35:03  qliu
! -- Revised for efficiency, change AtmOptics as IN OUT.
!
! Revision 1.5  2005/08/17 21:15:09  qliu
! -- Deleted "USE CRTM_CloudScatter, ONLY : HGphase".
!
! Revision 1.4  2005/08/16 18:38:18  qliu
! - Converted from DOS to Unix file format.
!
! Revision 1.3  2005/08/16 16:30:35  qliu
! - Added header documentation for TL and AD routines.
!
! Revision 1.2  2005/08/04 20:31:31  paulv
! - Updated the forward routine, CRTM_Combine_AtmOptics. Untested. TL and AD
!   forms are unchanged.
!
! Revision 1.1  2005/07/15 16:36:25  paulv
! Initial checkin. Incomplete.
!
!
!

