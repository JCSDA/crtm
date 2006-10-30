!------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_AtmAbsorption_Predictor
!
! PURPOSE:
!       Module continaing routines to compute the predictors for the gas
!       absorption model.
!
! CATEGORY:
!       CRTM : Gas Absorption : Predictor
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_AtmAbsorption_Predictor
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
!       CRTM_AtmAbsorption_Define:  Module defining the CRTM AtmAbsorption
!                                   structure and containing routines to 
!                                   manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!
! CONTAINS:
!       PUBLIC subprograms
!       ------------------
!       CRTM_Compute_Predictors:       Subroutine to calculate the gas absorption
!                                      model predictors.
!
!       CRTM_Compute_Predictors_TL:    Subroutine to calculate the gas absorption
!                                      model tangent-linear predictors.
!
!       CRTM_Compute_Predictors_AD:    Subroutine to calculate the adjoint gas
!                                      absorption model predictors.
!
!       PRIVATE subprograms
!       -------------------
!       CRTM_Standard_Predictors:      Subroutine to compute the integrated
!                                      absorber amount INDEPENDENT predictors
!                                      for the gas absorption model.
!
!       CRTM_Integrated_Predictors:    Subroutine to compute the integrated
!                                      absorber amount DEPENDENT predictors
!                                      for the gas absorption model.
!
!       CRTM_Standard_Predictors_TL:   Subroutine to compute the integrated
!                                      absorber amount INDEPENDENT tangent-
!                                      linear predictors for the gas absorption
!                                      model.
!
!       CRTM_Integrated_Predictors_TL: Subroutine to compute the integrated
!                                      absorber amount DEPENDENT tangent-
!                                      linear predictors for the gas absorption
!                                      model.
!
!       CRTM_Standard_Predictors_AD:   Subroutine to compute the integrated
!                                      absorber amount INDEPENDENT adjoint
!                                      predictors for the gas absorption
!                                      model.
!
!       CRTM_Integrated_Predictors_AD: Subroutine to compute the integrated
!                                      absorber amount DEPENDENT adjoint
!                                      predictors for the gas absorption
!                                      model.
!
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

MODULE CRTM_AtmAbsorption_Predictor


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Message_Handler

  ! -- CRTM modules
  USE CRTM_Parameters
  USE CRTM_Atmosphere_Define, ONLY: CRTM_Atmosphere_type, H2O_ID, O3_ID
  USE CRTM_AtmAbsorption_Define, ONLY: CRTM_AtmAbsorption_type


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
  PUBLIC :: CRTM_Compute_Predictors
  PUBLIC :: CRTM_Compute_Predictors_TL
  PUBLIC :: CRTM_Compute_Predictors_AD


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_AtmAbsorption_Predictor.f90,v 1.4 2006/05/25 19:26:00 wd20pd Exp $'


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
!       CRTM_Compute_Predictors
!
! PURPOSE:
!       Subroutine to calculate the gas absorption model predictors.
!
! CATEGORY:
!       CRTM : Gas Absorption : Predictor
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_Predictors ( Atmosphere,    &  ! Input
!                                      AtmAbsorption, &  ! In/Output
!                                      No_Standard    )  ! Optional input
!
! INPUT ARGUMENTS:
!       Atmosphere:     CRTM Atmosphere structure containing the atmospheric
!                       state data.
!                       UNITS:      N/A
!                       TYPE:       TYPE( CRTM_Atmosphere_type )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       AtmAbsorption:  On INPUT, this structure contains the integrated
!                       absorber amounts.
!                       UNITS:      N/A
!                       TYPE:       TYPE( CRTM_AtmAbsorption_type )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL INPUT ARGUMENTS:
!       No_Standard:    If present, the standard predictors are not calculated.
!                       This prevents recalculation of the standard predictors
!                       is only the view angle has changed - which only affects
!                       the integrated predictors.
!                       UNITS:      N/A
!                       TYPE:       INETEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       AtmAbsorption:  On OUTPUT, this structure contains the predictors for
!                       the integrated absorber amounts.
!                       UNITS:      N/A
!                       TYPE:       TYPE( CRTM_AtmAbsorption_type )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! CALLS:
!       CRTM_Standard_Predictors:      Subroutine to compute the integrated
!                                      absorber amount INDEPENDENT predictors
!                                      for the gas absorption model.
!
!       CRTM_Integrated_Predictors:    Subroutine to compute the integrated
!                                      absorber amount DEPENDENT predictors
!                                      for the gas absorption model.
!
! SIDE EFFECTS:
!       Note that the argument, AtmAbsorption, is used as both input and
!       output and has its INTENT declared accordingly as IN OUT.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       The predictors used in the gas absorption model are organised in
!       the following manner:
!
! ------------------------------------------------------------------------------
! | 1 | 2 | 3 | ... | 9 | 10 | 11 | 12 |....| 17 | 18 |....| 23 | 24 |....| 29 |
! ------------------------------------------------------------------------------
!
! \                              /\             /\             /\             /
!  \                            /  \           /  \           /  \           /
!   ----------------------------    -----------    -----------    -----------
!                 |                      |              |              |
!                 v                      v              v              v
!
!             Standard               Integrated     Integrated     Integrated
!            Predictors              predictors     predictors     predictors
!                                       for            for            for
!                                    Absorber 1     Absorber 2     Absorber 3
!                                   (water vapor)   (dry gases)     (ozone)
!
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_Predictors ( Atmosphere,    &  ! Input
                                       AtmAbsorption, &  ! In/Output
                                       No_Standard    )  ! Optional input



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    TYPE( CRTM_Atmosphere_type ),    INTENT( IN )     :: Atmosphere

    ! -- In/Outputs
    TYPE( CRTM_AtmAbsorption_type ), INTENT( IN OUT ) :: AtmAbsorption

    ! -- Optional Inputs
    INTEGER,               OPTIONAL, INTENT( IN )     :: No_Standard


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_Predictors'


    ! ---------------
    ! Local variables
    ! ---------------

    LOGICAL :: Compute_Standard
    INTEGER :: i1, i2, j



    !#--------------------------------------------------------------------------#
    !#                        -- CHECK OPTIONAL ARGUMENT --                     #
    !#--------------------------------------------------------------------------#

    Compute_Standard = .TRUE.
    IF ( PRESENT( No_Standard ) ) THEN
      IF ( No_Standard == SET ) Compute_Standard = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- CALCULATE THE STANDARD PREDICTORS --               #
    !#--------------------------------------------------------------------------#

    IF ( Compute_Standard ) THEN
      CALL CRTM_Standard_Predictors( Atmosphere, &
                                     AtmAbsorption%Predictor( 1:MAX_N_STANDARD_PREDICTORS, : ) )
    END IF


                                                   
    !#--------------------------------------------------------------------------#
    !#                -- CALCULATE THE INTEGRATED PREDICTORS --                 #
    !#--------------------------------------------------------------------------#

    Absorber_Loop: DO j = 1, AtmAbsorption%n_Absorbers

      ! -- Determine indices of current absorber predictors
      i1 = MAX_N_STANDARD_PREDICTORS + ( ( j - 1 ) * MAX_N_INTEGRATED_PREDICTORS ) + 1
      i2 = i1 + MAX_N_INTEGRATED_PREDICTORS - 1

      ! -- Compute the predictors for the current absorber
      CALL CRTM_Integrated_Predictors( Atmosphere, &
                                       AtmAbsorption%IntAbsorber( 0:, j ), &
                                       AtmAbsorption%Predictor( i1:i2, : ) )

    END DO Absorber_Loop

  END SUBROUTINE CRTM_Compute_Predictors





!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Compute_Predictors_TL
!
! PURPOSE:
!       Subroutine to calculate the gas absorption model tangent-linear
!       predictors.
!
! CATEGORY:
!       CRTM : Gas Absorption : Predictor
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_Predictors_TL ( Atmosphere,       &  ! Input
!                                         AtmAbsorption,    &  ! Input
!                                         Atmosphere_TL,    &  ! Input
!                                         AtmAbsorption_TL, &  ! In/Output
!                                         No_Standard       )  ! Optional input
!
! INPUT ARGUMENTS:
!       Atmosphere:        CRTM Atmosphere structure containing the atmospheric
!                          state data.
!                          UNITS:      N/A
!                          TYPE:       TYPE( CRTM_Atmosphere_type )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       AtmAbsorption:     CRTM AtmAbsorption structure containing the integrated
!                          absorber amounts.
!                          UNITS:      N/A
!                          TYPE:       TYPE( CRTM_AtmAbsorption_type )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       Atmosphere_TL:     CRTM Atmosphere structure containing the tangent-linear
!                          atmospheric state data, i.e. the perturbations.
!                          UNITS:      N/A
!                          TYPE:       TYPE( CRTM_Atmosphere_type )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       AtmAbsorption_TL:  On INPUT, this structure contains the tangent-linear
!                          integrated absorber amounts.
!                          UNITS:      N/A
!                          TYPE:       TYPE( CRTM_AtmAbsorption_type )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL INPUT ARGUMENTS:
!       No_Standard:       If present, the standard predictors are not calculated.
!                          This prevents recalculation of the standard predictors
!                          is only the view angle has changed - which only affects
!                          the integrated predictors.
!                          UNITS:      N/A
!                          TYPE:       Integer
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       AtmAbsorption_TL:  On OUTPUT, this structure contains the tangent-linear
!                          predictors.
!                          UNITS:      N/A
!                          TYPE:       TYPE( CRTM_AtmAbsorption_type )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! CALLS:
!       CRTM_Standard_Predictors_TL:   Subroutine to compute the integrated
!                                      absorber amount INDEPENDENT tangent-
!                                      linear predictors for the gas absorption
!                                      model.
!
!       CRTM_Integrated_Predictors_TL: Subroutine to compute the integrated
!                                      absorber amount DEPENDENT tangent-
!                                      linear predictors for the gas absorption
!                                      model.
!
! SIDE EFFECTS:
!       Note that the argument, AtmAbsorption_TL, is used as both input and
!       output and has its INTENT declared accordingly as IN OUT.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       The predictors used in the gas absorption model are organised in
!       the following manner:
!
! ------------------------------------------------------------------------------
! | 1 | 2 | 3 | ... | 9 | 10 | 11 | 12 |....| 17 | 18 |....| 23 | 24 |....| 29 |
! ------------------------------------------------------------------------------
!
! \                              /\             /\             /\             /
!  \                            /  \           /  \           /  \           /
!   ----------------------------    -----------    -----------    -----------
!                 |                      |              |              |
!                 v                      v              v              v
!
!             Standard               Integrated     Integrated     Integrated
!            Predictors              predictors     predictors     predictors
!                                       for            for            for
!                                    Absorber 1     Absorber 2     Absorber 3
!                                   (water vapor)   (dry gases)     (ozone)
!
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_Predictors_TL( Atmosphere,       &  ! Input
                                         AtmAbsorption,    &  ! Input
                                         Atmosphere_TL,    &  ! Input
                                         AtmAbsorption_TL, &  ! In/Output
                                         No_Standard       )  ! Optional input



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    TYPE( CRTM_Atmosphere_type ),    INTENT( IN )     :: Atmosphere
    TYPE( CRTM_AtmAbsorption_type ), INTENT( IN )     :: AtmAbsorption
    TYPE( CRTM_Atmosphere_type ),    INTENT( IN )     :: Atmosphere_TL

    ! -- In/Outputs
    TYPE( CRTM_AtmAbsorption_type ), INTENT( IN OUT ) :: AtmAbsorption_TL

    ! -- Optional Input
    INTEGER, OPTIONAL,               INTENT( IN )     :: No_Standard


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_Predictors_TL'


    ! ---------------
    ! Local variables
    ! ---------------

    LOGICAL :: Compute_Standard
    INTEGER :: i1, i2, j



    !#--------------------------------------------------------------------------#
    !#                        -- CHECK OPTIONAL ARGUMENT --                     #
    !#--------------------------------------------------------------------------#

    Compute_Standard = .TRUE.
    IF ( PRESENT( No_Standard ) ) THEN
      IF ( No_Standard == SET ) Compute_Standard = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- CALCULATE THE TANGENT-LINEAR STANDARD PREDICTORS --         #
    !#--------------------------------------------------------------------------#

    IF ( Compute_Standard ) THEN
      CALL CRTM_Standard_Predictors_TL( Atmosphere, &
                                        Atmosphere_TL, &
                                        AtmAbsorption_TL%Predictor( 1:MAX_N_STANDARD_PREDICTORS, : ) )
    END IF


                                                   
    !#--------------------------------------------------------------------------#
    !#                -- CALCULATE THE INTEGRATED PREDICTORS --                 #
    !#--------------------------------------------------------------------------#

    Absorber_Loop: DO j = 1, AtmAbsorption_TL%n_Absorbers

      ! -- Determine indices of current absorber predictors
      i1 = MAX_N_STANDARD_PREDICTORS + ( ( j - 1 ) * MAX_N_INTEGRATED_PREDICTORS ) + 1
      i2 = i1 + MAX_N_INTEGRATED_PREDICTORS - 1

      ! -- Calculate the tangent-linear predictors for current absorber
      CALL CRTM_Integrated_Predictors_TL( Atmosphere, &
                                          AtmAbsorption%IntAbsorber( 0:, j ), &
                                          Atmosphere_TL, &
                                          AtmAbsorption_TL%IntAbsorber( 0:, j ), &
                                          AtmAbsorption_TL%Predictor( i1:i2, : ) )

    END DO Absorber_Loop

  END SUBROUTINE CRTM_Compute_Predictors_TL





!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Compute_Predictors_AD
!
! PURPOSE:
!       Subroutine to calculate the adjoint gas absorption model predictors.
!
! CATEGORY:
!       CRTM : Gas Absorption : Predictor
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_Predictors_AD( Atmosphere,       &  ! Input
!                                        AtmAbsorption,    &  ! Input
!                                        AtmAbsorption_AD, &  ! In/Output
!                                        Atmosphere_AD,    &  ! Output
!                                        No_Standard       )  ! Optional input
! INPUT ARGUMENTS:
!       Atmosphere:        CRTM Atmosphere structure containing the atmospheric
!                          state data.
!                          UNITS:      N/A
!                          TYPE:       TYPE( CRTM_Atmosphere_type )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       AtmAbsorption:     CRTM AtmAbsorption structure containing the integrated
!                          absorber amounts.
!                          UNITS:      N/A
!                          TYPE:       TYPE( CRTM_AtmAbsorption_type )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       AtmAbsorption_AD:  On INPUT, this structure contains the predictor
!                          adjoints.
!                          UNITS:      N/A
!                          TYPE:       TYPE( CRTM_AtmAbsorption_type )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL INPUT ARGUMENTS:
!       No_Standard:       If present, the standard predictors are not calculated.
!                          This prevents recalculation of the standard predictors
!                          is only the view angle has changed - which only affects
!                          the integrated predictors.
!                          UNITS:      N/A
!                          TYPE:       Integer
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       AtmAbsorption_AD:  On OUTPUT, this structure contains the integrated
!                          absorber adjoints.
!                          UNITS:      N/A
!                          TYPE:       TYPE( CRTM_AtmAbsorption_type )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN OUT )
!
!       Atmosphere_AD:     CRTM Atmosphere structure containing the adjoints
!                          of the atmospheric state data.
!                          UNITS:      N/A
!                          TYPE:       TYPE( CRTM_Atmosphere_type )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! CALLS:
!       CRTM_Standard_Predictors_AD:   Subroutine to compute the integrated
!                                      absorber amount INDEPENDENT predictors
!                                      for the adjoint gas absorption model.
!
!       CRTM_Integrated_Predictors_AD: Subroutine to compute the integrated
!                                      absorber amount DEPENDENT predictors
!                                      for the adjoint gas absorption model.
!
! SIDE EFFECTS:
!       Note that the argument, AtmAbsorption_AD, is used as both input and
!       output and has its INTENT declared accordingly as IN OUT.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note that the output adjoint argument, Atmosphere_AD, has INTENT of
!       IN OUT. This is because the pressure, temperature, and absorber
!       components of the Atmosphere_AD structure are assumed to have some
!       initial value (which could simply be zero) that is added to when
!       contructing the pressure, temperature and absorber adjoints.
!
!       The predictors used in the gas absorption model are organised in
!       the following manner:
!
! ------------------------------------------------------------------------------
! | 1 | 2 | 3 | ... | 9 | 10 | 11 | 12 |....| 17 | 18 |....| 23 | 24 |....| 29 |
! ------------------------------------------------------------------------------
!
! \                              /\             /\             /\             /
!  \                            /  \           /  \           /  \           /
!   ----------------------------    -----------    -----------    -----------
!                 |                      |              |              |
!                 v                      v              v              v
!
!             Standard               Integrated     Integrated     Integrated
!            Predictors              predictors     predictors     predictors
!                                       for            for            for
!                                    water vapor    dry gases        ozone
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_Predictors_AD( Atmosphere,       &  ! Input
                                         AtmAbsorption,    &  ! Input
                                         AtmAbsorption_AD, &  ! In/Output
                                         Atmosphere_AD,    &  ! Output
                                         No_Standard       )  ! Optional input



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    TYPE( CRTM_Atmosphere_type ),    INTENT( IN )     :: Atmosphere
    TYPE( CRTM_AtmAbsorption_type ), INTENT( IN )     :: AtmAbsorption

    ! -- In/Outputs
    TYPE( CRTM_AtmAbsorption_type ), INTENT( IN OUT ) :: AtmAbsorption_AD

    ! -- Outputs
    TYPE( CRTM_Atmosphere_type ),    INTENT( IN OUT ) :: Atmosphere_AD

    ! -- Optional input
    INTEGER, OPTIONAL,               INTENT( IN )     :: No_Standard


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_Predictors_AD'


    ! ---------------
    ! Local variables
    ! ---------------

    LOGICAL :: Compute_Standard
    INTEGER :: i1, i2, j



    !#--------------------------------------------------------------------------#
    !#                        -- CHECK OPTIONAL ARGUMENT --                     #
    !#--------------------------------------------------------------------------#

    Compute_Standard = .TRUE.
    IF ( PRESENT( No_Standard ) ) THEN
      IF ( No_Standard == SET ) Compute_Standard = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#         -- CALCULATE THE ADJOINT OF THE INTEGRATED PREDICTORS --         #
    !#--------------------------------------------------------------------------#

    Absorber_Loop: DO j = 1, AtmAbsorption_AD%n_Absorbers

      ! -- Determine indices of current Absorber Predictors
      i1 = MAX_N_STANDARD_PREDICTORS + ( ( j - 1 ) * MAX_N_INTEGRATED_PREDICTORS ) + 1
      i2 = i1 + MAX_N_INTEGRATED_PREDICTORS - 1

      ! -- Compute the predictor adjoints for the current absorber
      CALL CRTM_Integrated_Predictors_AD( Atmosphere, &
                                          AtmAbsorption%IntAbsorber( 0:, j ), &
                                          AtmAbsorption_AD%Predictor( i1:i2, : ), &
                                          Atmosphere_AD, &
                                          AtmAbsorption_AD%IntAbsorber( 0:, j ) )

    END DO Absorber_Loop                                    



    !#--------------------------------------------------------------------------#
    !#          -- CALCULATE THE ADJOINT OF THE STANDARD PREDICTORS --          #
    !#--------------------------------------------------------------------------#

    IF ( Compute_Standard ) THEN
      CALL CRTM_Standard_Predictors_AD( Atmosphere, &
                                        AtmAbsorption_AD%Predictor( 1:MAX_N_STANDARD_PREDICTORS, : ), &
                                        Atmosphere_AD )
    END IF

  END SUBROUTINE CRTM_Compute_Predictors_AD





!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PRIVATE MODULE ROUTINES ##                      ##
!##                                                                            ##
!################################################################################
!################################################################################


!#------------------------------------------------------------------------------#
!#                      FORWARD COMPONENT PREDICTOR ROUTINES                    #
!#------------------------------------------------------------------------------#

!--------------------------------------------------------------------------------
! NAME:
!       CRTM_Standard_Predictors
!
! PURPOSE:
!       Subroutine to compute the integrated absorber amount INDEPENDENT
!       predictors for the gas absorption model.
!
! CATEGORY:
!       CRTM : Gas Absorption : Predictor
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Standard_Predictors( Atmosphere, &  ! Input
!                                      Predictor   )  ! Output, Istd x K
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
!       None
!
! OUTPUT ARGUMENTS:
!       Predictor:    Array containing the calculated standard predictors.
!                     UNITS:      Varies with predictor type.
!                     TYPE:       REAL( fp_kind )
!                     DIMENSION:  Rank-2 (Istd x K)
!                     ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None
!
! CALLS:
!       None
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Standard_Predictors( Atmosphere, &  ! Input
                                       Predictor   )  ! Output, Istd x K



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    TYPE( CRTM_Atmosphere_type ),       INTENT( IN )  :: Atmosphere

    ! -- Outputs
    REAL( fp_kind ), DIMENSION( :, : ), INTENT( OUT ) :: Predictor  ! Istd x K


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Standard_Predictors'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: k
    REAL( fp_kind ) :: p2
    REAL( fp_kind ) :: t2
    INTEGER :: H2O_Index



    !#--------------------------------------------------------------------------#
    !#              -- GET THE Atmosphere INDEX FOR WATER VAPOR --              #
    !#--------------------------------------------------------------------------#

    H2O_Index = MINLOC( ABS( Atmosphere%Absorber_ID - H2O_ID ), DIM = 1 )



    !#--------------------------------------------------------------------------#
    !#                -- CALCULATE THE STANDARD PREDICTOR SET --                #
    !#--------------------------------------------------------------------------#

    Layer_Loop: DO k = 1, Atmosphere%n_Layers

      ! -- Precalculate the squared terms
      p2 = Atmosphere%Pressure( k )    * Atmosphere%Pressure( k )
      t2 = Atmosphere%Temperature( k ) * Atmosphere%Temperature( k )

      ! -- Calculate and assign the absorber independent predictors
      Predictor(  1, k ) = Atmosphere%Temperature( k )
      Predictor(  2, k ) = Atmosphere%Pressure( k )
      Predictor(  3, k ) = t2
      Predictor(  4, k ) = p2
      Predictor(  5, k ) = Atmosphere%Temperature( k ) * Atmosphere%Pressure( k )
      Predictor(  6, k ) = t2 * Atmosphere%Pressure( k )
      Predictor(  7, k ) = Atmosphere%Temperature( k ) * p2
      Predictor(  8, k ) = t2 * p2
      Predictor(  9, k ) = Atmosphere%Pressure( k )**POINT_25
      Predictor( 10, k ) = Atmosphere%Absorber( k, H2O_Index )
      Predictor( 11, k ) = Atmosphere%Absorber( k, H2O_Index ) / t2

    END DO Layer_Loop

  END SUBROUTINE CRTM_Standard_Predictors


!--------------------------------------------------------------------------------
! NAME:
!       CRTM_Integrated_Predictors
!
! PURPOSE:
!       Subroutine to compute the integrated absorber amount DEPENDENT
!       predictors for the gas absorption model.
!
! CATEGORY:
!       CRTM : Gas Absorption : Predictor
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Integrated_Predictors( Atmosphere,  &  ! Input
!                                        IntAbsorber, &  ! Input, 0:K
!                                        Predictor    )  ! Output, Iint x K
!
! INPUT ARGUMENTS:
!       Atmosphere:   CRTM Atmosphere structure containing the atmospheric
!                     state data.
!                     UNITS:      N/A
!                     TYPE:       TYPE( CRTM_Atmosphere_type )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
!       IntAbsorber:  Array containing the profile LEVEL integrated
!                     absorber data.
!                     UNITS:      Absorber dependent
!                     TYPE:       REAL( fp_kind )
!                     DIMENSION:  Rank-2 (0:K x J)
!                     ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL INPUT ARGUMENTS:
!       None
!
! OUTPUT ARGUMENTS:
!       Predictor:    Array containing the calculated integrated predictors
!                     for the passed absorber.
!                     UNITS:      Varies with predictor type.
!                     TYPE:       REAL( fp_kind )
!                     DIMENSION:  Rank-2 (Iint x K)
!                     ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None
!
! CALLS:
!       None
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Integrated_Predictors( Atmosphere,  &  ! Input
                                         IntAbsorber, &  ! Input,  0:K
                                         Predictor    )  ! Output, Iint x K



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    TYPE( CRTM_Atmosphere_type ),       INTENT( IN )  :: Atmosphere
    REAL( fp_kind ), DIMENSION( 0: ),   INTENT( IN )  :: IntAbsorber  ! 0:K

    ! -- Outputs
    REAL( fp_kind ), DIMENSION( :, : ), INTENT( OUT ) :: Predictor    ! Iint x K


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Integrated_Predictors'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, k
    INTEGER :: n_Predictors

    REAL( fp_kind ) :: d_IntAbsorber
    REAL( fp_kind ) :: Factor_1
    REAL( fp_kind ) :: Factor_2
    REAL( fp_kind ) :: Inverse_1
    REAL( fp_kind ) :: Inverse_2
    REAL( fp_kind ) :: Inverse_3

    ! -- Square of the Absorber amount. 0:K
    REAL( fp_kind ), DIMENSION( 0:Atmosphere%n_Layers )     :: IntAbsorber_2

    ! -- Intermediate summation array. Iint
    REAL( fp_kind ), DIMENSION( SIZE( Predictor, DIM=1 ) )  :: s

    ! -- LEVEL Predictor, Iint x 0:K
    REAL( fp_kind ), DIMENSION( SIZE( Predictor, DIM=1 ), &
                                0:Atmosphere%n_Layers )     :: x



    !#--------------------------------------------------------------------------#
    !#                 -- DETERMINE THE NUMBER OF PREDICTORS --                 #
    !#--------------------------------------------------------------------------#

    n_Predictors = SIZE( Predictor, DIM = 1 )



    !#--------------------------------------------------------------------------#
    !#                         -- INITIALISE VALUES --                          #
    !#--------------------------------------------------------------------------#

    IntAbsorber_2( 0 ) = IntAbsorber(0) * IntAbsorber(0)

    s( : )    = ZERO
    x( :, 0 ) = ZERO



    !#--------------------------------------------------------------------------#
    !#               -- CALCULATE THE INTEGRATED PREDICTOR SET --               #
    !#--------------------------------------------------------------------------#

    Layer_Loop: DO k = 1, Atmosphere%n_Layers


      ! -----------------------------------------
      ! Calculate Absorber multiplicative Factors
      ! -----------------------------------------

      IntAbsorber_2( k ) = IntAbsorber( k ) * IntAbsorber( k )

      d_IntAbsorber = IntAbsorber( k ) - IntAbsorber( k-1 )                         ! For * terms
      Factor_1      = ( IntAbsorber( k )   + IntAbsorber( k-1 )   ) * d_IntAbsorber ! For ** terms
      Factor_2      = ( IntAbsorber_2( k ) + IntAbsorber_2( k-1 ) ) * d_IntAbsorber ! For *** terms


      ! -------------------------------
      ! Calculate the intermediate sums
      ! -------------------------------

      s( 1 ) = s( 1 ) + ( Atmosphere%Temperature( k ) * d_IntAbsorber )  ! T*
      s( 2 ) = s( 2 ) + ( Atmosphere%Pressure( k )    * d_IntAbsorber )  ! P*

      s( 3 ) = s( 3 ) + ( Atmosphere%Temperature( k ) * Factor_1 )       ! T**
      s( 4 ) = s( 4 ) + ( Atmosphere%Pressure( k )    * Factor_1 )       ! P**

      s( 5 ) = s( 5 ) + ( Atmosphere%Temperature( k ) * Factor_2 )       ! T***
      s( 6 ) = s( 6 ) + ( Atmosphere%Pressure( k )    * Factor_2 )       ! P***


      ! ---------------------------------
      ! Calculate the normalising factors
      ! for the integrated predictors
      ! ---------------------------------

      IF ( IntAbsorber( k ) > MINIMUM_ABSORBER_AMOUNT ) THEN
        Inverse_1 = ONE / IntAbsorber( k )
      ELSE
        Inverse_1 = ZERO
      END IF

      Inverse_2 = Inverse_1 * Inverse_1
      Inverse_3 = Inverse_2 * Inverse_1


      ! ---------------------------------------
      ! Compute the LEVEL integrated predictors
      ! ---------------------------------------

      x( 1, k ) = POINT_5  * s( 1 ) * Inverse_1  ! T*
      x( 2, k ) = POINT_5  * s( 2 ) * Inverse_1  ! P*

      x( 3, k ) = POINT_5  * s( 3 ) * Inverse_2  ! T**
      x( 4, k ) = POINT_5  * s( 4 ) * Inverse_2  ! P**

      x( 5, k ) = POINT_75 * s( 5 ) * Inverse_3  ! T***
      x( 6, k ) = POINT_75 * s( 6 ) * Inverse_3  ! P***


      ! ----------------------------
      ! Sum predictors across layers
      ! ----------------------------

      DO i = 1, n_Predictors
        Predictor( i, k ) = x( i, k ) + x( i, k-1 )
      END DO

    END DO Layer_Loop

  END SUBROUTINE CRTM_Integrated_Predictors





!#------------------------------------------------------------------------------#
!#                   TANGENT-LINEAR COMPONENT PREDICTOR ROUTINES                #
!#------------------------------------------------------------------------------#


!--------------------------------------------------------------------------------
! NAME:
!       CRTM_Standard_Predictors_TL
!
! PURPOSE:
!       Subroutine to compute the integrated absorber amount INDEPENDENT
!       tangent-linear predictors for the gas absorption model.
!
! CATEGORY:
!       CRTM : Gas Absorption : Predictor
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Standard_Predictors_TL( Atmosphere,    &  ! Input
!                                         Atmosphere_TL, &  ! Input
!                                         Predictor_TL   )  ! Output, Istd x K
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
! OPTIONAL INPUT ARGUMENTS:
!       None
!
! OUTPUT ARGUMENTS:
!       Predictor_TL:   Array containing the calculated tangent-linear
!                       standard predictors.
!                       UNITS:      Varies with Predictor type.
!                       TYPE:       REAL( fp_kind )
!                       DIMENSION:  Istd x K
!                       ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None
!
! CALLS:
!       None
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Standard_Predictors_TL( Atmosphere,    &  ! Input
                                          Atmosphere_TL, &  ! Input
                                          Predictor_TL   )  ! Output, Istd x K



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_Atmosphere_type ),       INTENT( IN )  :: Atmosphere
    TYPE( CRTM_Atmosphere_type ),       INTENT( IN )  :: Atmosphere_TL

    ! -- Output
    REAL( fp_kind ), DIMENSION( :, : ), INTENT( OUT ) :: Predictor_TL  ! Istd x K


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Standard_Predictors_TL'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: k
    REAL( fp_kind ) :: p2, p2_TL
    REAL( fp_kind ) :: t2, t2_TL
    INTEGER :: H2O_Index



    !#--------------------------------------------------------------------------#
    !#              -- GET THE Atmosphere INDEX FOR WATER VAPOR --              #
    !#--------------------------------------------------------------------------#

    H2O_Index = MINLOC( ABS( Atmosphere%Absorber_ID - H2O_ID ), DIM = 1 )



    !#--------------------------------------------------------------------------#
    !#        -- CALCULATE THE TANGENT-LINEAR STANDARD PREDICTOR SET --         #
    !#--------------------------------------------------------------------------#

    Layer_loop: DO k = 1, Atmosphere%n_Layers


      ! -- Precalculate the squared terms
      p2 = Atmosphere%Pressure( k )    * Atmosphere%Pressure( k )
      t2 = Atmosphere%Temperature( k ) * Atmosphere%Temperature( k )

      ! -- Tangent-linear of squared terms
      p2_TL = TWO * Atmosphere%Pressure( k )    * Atmosphere_TL%Pressure( k )
      t2_TL = TWO * Atmosphere%Temperature( k ) * Atmosphere_TL%Temperature( k )
      
      ! -- Calculate and assign the absorber independent predictors
      Predictor_TL(  1, k ) = Atmosphere_TL%Temperature( k )
      Predictor_TL(  2, k ) = Atmosphere_TL%Pressure( k )
      Predictor_TL(  3, k ) = t2_TL
      Predictor_TL(  4, k ) = p2_TL
      Predictor_TL(  5, k ) = ( Atmosphere%Temperature( k ) * Atmosphere_TL%Pressure( k )    ) + &
                              ( Atmosphere%Pressure( k )    * Atmosphere_TL%Temperature( k ) )
      Predictor_TL(  6, k ) = ( Atmosphere%Pressure( k ) * t2_TL ) + &
                              ( t2 * Atmosphere_TL%Pressure( k ) )
      Predictor_TL(  7, k ) = ( Atmosphere%Temperature( k ) * p2_TL ) + &
                              ( p2 * Atmosphere_TL%Temperature( k ) )
      Predictor_TL(  8, k ) = ( t2 * p2_TL ) + &
                              ( p2 * t2_TL )
      Predictor_TL(  9, k ) = POINT_25 * (Atmosphere%Pressure( k )**(-POINT_75)) * Atmosphere_TL%Pressure( k )
      Predictor_TL( 10, k ) = Atmosphere_TL%Absorber( k, H2O_Index )
      Predictor_TL( 11, k ) = ( Atmosphere_TL%Absorber( k, H2O_Index ) - &
                                ( Atmosphere%Absorber( k, H2O_Index ) * t2_TL / t2 ) ) / t2

    END DO Layer_loop

  END SUBROUTINE CRTM_Standard_Predictors_TL





!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Integrated_Predictors_TL
!
! PURPOSE:
!       Subroutine to compute the integrated absorber amount DEPENDENT
!       tangent-linear predictors for the gas absorption model.
!
! CATEGORY:
!       CRTM : Gas Absorption : Predictor
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Integrated_Predictors_TL( Atmosphere,     &  ! Input
!                                           IntAbsorber,    &  ! Input, 0:K
!                                           Atmosphere_TL,  &  ! Input
!                                           IntAbsorber_TL, &  ! Input, 0:K
!                                           Predictor_TL    )  ! Output, Iint x K
!
! INPUT ARGUMENTS:
!       Atmosphere:      CRTM Atmosphere structure containing the atmospheric
!                        state data.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_Atmosphere_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       IntAbsorber:     Array containing profile LEVEL integrated
!                        absorber data.
!                        UNITS:      Absorber dependent
!                        TYPE:       REAL( fp_kind )
!                        DIMENSION:  Rank-2 (0:K)
!                        ATTRIBUTES: INTENT( OUT )
!
!       Atmosphere_TL:   CRTM Atmosphere structure containing the tangent-linear
!                        atmospheric state data, i.e. the perturbations.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_Atmosphere_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       IntAbsorber_TL:  Array containing the tangent-linear profile LEVEL
!                        integrated absorber data.
!                        UNITS:      Absorber dependent
!                        TYPE:       REAL( fp_kind )
!                        DIMENSION:  Rank-2 (0:K)
!                        ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL INPUT ARGUMENTS:
!       None
!
! OUTPUT ARGUMENTS:
!       Predictor_TL:    Array containing the calculated tangent-linear 
!                        integrated predictors for the passed absorber.
!                        UNITS:      Varies with predictor type.
!                        TYPE:       REAL( fp_kind )
!                        DIMENSION:  Rank-2 (Iint x K)
!                        ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None
!
! CALLS:
!       None
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Integrated_Predictors_TL( Atmosphere,     &  ! Input
                                            IntAbsorber,    &  ! Input, 0:K
                                            Atmosphere_TL,  &  ! Input
                                            IntAbsorber_TL, &  ! Input, 0:K
                                            Predictor_TL    )  ! Output, Iint x K
 


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_Atmosphere_type ),       INTENT( IN )  :: Atmosphere
    REAL( fp_kind ), DIMENSION( 0: ),   INTENT( IN )  :: IntAbsorber     ! 0:K
    TYPE( CRTM_Atmosphere_type ),       INTENT( IN )  :: Atmosphere_TL
    REAL( fp_kind ), DIMENSION( 0: ),   INTENT( IN )  :: IntAbsorber_TL  ! 0:K

    ! -- Output
    REAL( fp_kind ), DIMENSION( :, : ), INTENT( OUT ) :: Predictor_TL    ! Iint x K


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Integrated_Predictors_TL'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, k
    INTEGER :: n_Predictors

    REAL( fp_kind ) :: d_IntAbsorber
    REAL( fp_kind ) :: d_IntAbsorber_TL
    REAL( fp_kind ) :: Factor_1
    REAL( fp_kind ) :: Factor_1_TL
    REAL( fp_kind ) :: Factor_2
    REAL( fp_kind ) :: Factor_2_TL
    REAL( fp_kind ) :: Inverse_1
    REAL( fp_kind ) :: Inverse_2
    REAL( fp_kind ) :: Inverse_3
    REAL( fp_kind ) :: Inverse_4
    REAL( fp_kind ) :: Inverse_1_TL
    REAL( fp_kind ) :: Inverse_2_TL
    REAL( fp_kind ) :: Inverse_3_TL

    ! -- Square of the Absorber amount. 0:K
    REAL( fp_kind ), DIMENSION( 0:Atmosphere%n_Layers )     :: IntAbsorber_2
    REAL( fp_kind ), DIMENSION( 0:Atmosphere%n_Layers )     :: IntAbsorber_2_TL

    ! -- Intermediate summation arrays. Iint
    REAL( fp_kind ), DIMENSION( SIZE( Predictor_TL, DIM=1 ) ) :: s
    REAL( fp_kind ), DIMENSION( SIZE( Predictor_TL, DIM=1 ) ) :: s_TL

    ! -- LEVEL Predictor, Iint x 0:K
    REAL( fp_kind ), DIMENSION( SIZE( Predictor_TL, DIM=1 ), &
                                0:Atmosphere%n_Layers )     :: x_TL



    !#--------------------------------------------------------------------------#
    !#                 -- DETERMINE THE NUMBER OF PREDICTORS --                 #
    !#--------------------------------------------------------------------------#

    n_Predictors = SIZE( Predictor_TL, DIM = 1 )



    !#--------------------------------------------------------------------------#
    !#                         -- INITIALISE VALUES --                          #
    !#--------------------------------------------------------------------------#

    IntAbsorber_2(0)    = IntAbsorber(0) * IntAbsorber(0)
    IntAbsorber_2_TL(0) = TWO * IntAbsorber(0) * IntAbsorber_TL(0)

    s( : )       = ZERO
    s_TL( : )    = ZERO
    x_TL( :, 0 ) = ZERO



    !#--------------------------------------------------------------------------#
    !#               -- CALCULATE THE INTEGRATED PREDICTOR SET --               #
    !#--------------------------------------------------------------------------#

    Layer_loop: DO k = 1, Atmosphere%n_Layers


      ! -----------------------------------------
      ! Calculate absorber multiplicative Factors
      ! -----------------------------------------

      IntAbsorber_2(k)    = IntAbsorber(k) * IntAbsorber(k)
      IntAbsorber_2_TL(k) = TWO * IntAbsorber(k) * IntAbsorber_TL(k)

      ! -- For the * terms
      d_IntAbsorber    = IntAbsorber(k)    - IntAbsorber(k-1)
      d_IntAbsorber_TL = IntAbsorber_TL(k) - IntAbsorber_TL(k-1)

      ! -- For the ** terms
      Factor_1    = ( IntAbsorber(k) + IntAbsorber(k-1) ) * d_IntAbsorber
      Factor_1_TL = ( ( IntAbsorber(k)    + IntAbsorber(k-1)    ) * d_IntAbsorber_TL ) + &
                    ( ( IntAbsorber_TL(k) + IntAbsorber_TL(k-1) ) * d_IntAbsorber    )

      ! -- For the *** terms       
      Factor_2    = ( IntAbsorber_2(k) + IntAbsorber_2(k-1) ) * d_IntAbsorber
      Factor_2_TL = ( ( IntAbsorber_2(k)    + IntAbsorber_2(k-1)    ) * d_IntAbsorber_TL ) + &
                    ( ( IntAbsorber_2_TL(k) + IntAbsorber_2_TL(k-1) ) * d_IntAbsorber )


      ! -------------------------------
      ! Calculate the intermediate sums
      ! -------------------------------

      ! -- The forward terms
      s(1) = s(1) + ( Atmosphere%Temperature(k) * d_IntAbsorber  )  ! T*
      s(2) = s(2) + ( Atmosphere%Pressure(k)    * d_IntAbsorber  )  ! P*

      s(3) = s(3) + ( Atmosphere%Temperature(k) * Factor_1  )    ! T**
      s(4) = s(4) + ( Atmosphere%Pressure(k)    * Factor_1  )    ! P**

      s(5) = s(5) + ( Atmosphere%Temperature(k) * Factor_2  )    ! T***
      s(6) = s(6) + ( Atmosphere%Pressure(k)    * Factor_2  )    ! P***


      ! -- The tangent-linear terms
      s_TL(1) = s_TL(1) + ( Atmosphere_TL%Temperature(k) * d_IntAbsorber    ) + &  ! T*
                          ( Atmosphere%Temperature(k)    * d_IntAbsorber_TL )
      s_TL(2) = s_TL(2) + ( Atmosphere_TL%Pressure(k)    * d_IntAbsorber    ) + &  ! P*
                          ( Atmosphere%Pressure(k)       * d_IntAbsorber_TL )

      s_TL(3) = s_TL(3) + ( Atmosphere_TL%Temperature(k) * Factor_1    ) + &       ! T**
                          ( Atmosphere%Temperature(k)    * Factor_1_TL )
      s_TL(4) = s_TL(4) + ( Atmosphere_TL%Pressure(k)    * Factor_1    ) + &       ! P**
                          ( Atmosphere%Pressure(k)       * Factor_1_TL )

      s_TL(5) = s_TL(5) + ( Atmosphere_TL%Temperature(k) * Factor_2    ) + &       ! T***
                          ( Atmosphere%Temperature(k)    * Factor_2_TL )
      s_TL(6) = s_TL(6) + ( Atmosphere_TL%Pressure(k)    * Factor_2    ) + &       ! P***
                          ( Atmosphere%Pressure(k)       * Factor_2_TL )


      ! ---------------------------------
      ! Calculate the normalising factors
      ! for the integrated predictors
      ! ---------------------------------

      IF ( IntAbsorber(k) > MINIMUM_ABSORBER_AMOUNT ) THEN
        Inverse_1 = ONE / IntAbsorber(k)
      ELSE
        Inverse_1 = ZERO
      END IF

      Inverse_2 = Inverse_1 * Inverse_1
      Inverse_3 = Inverse_2 * Inverse_1
      Inverse_4 = Inverse_3 * Inverse_1

      Inverse_1_TL = -Inverse_2 * IntAbsorber_TL(k)
      Inverse_2_TL = -Inverse_3 * IntAbsorber_TL(k) * TWO
      Inverse_3_TL = -Inverse_4 * IntAbsorber_TL(k) * THREE


      ! ------------------------------------------------------
      ! Compute the tangent-linear LEVEL integrated predictors
      ! ------------------------------------------------------

      x_TL(1,k) = POINT_5  * ( ( s_TL(1) * Inverse_1    ) + &  ! T*
                               ( s(1)    * Inverse_1_TL ) )
      x_TL(2,k) = POINT_5  * ( ( s_TL(2) * Inverse_1    ) + &  ! P*
                               ( s(2)    * Inverse_1_TL ) )

      x_TL(3,k) = POINT_5  * ( ( s_TL(3) * Inverse_2    ) + &  ! T**
                               ( s(3)    * Inverse_2_TL ) )
      x_TL(4,k) = POINT_5  * ( ( s_TL(4) * Inverse_2    ) + &  ! P**
                               ( s(4)    * Inverse_2_TL ) )

      x_TL(5,k) = POINT_75 * ( ( s_TL(5) * Inverse_3    ) + &  ! T***
                               ( s(5)    * Inverse_3_TL ) )
      x_TL(6,k) = POINT_75 * ( ( s_TL(6) * Inverse_3    ) + &  ! P***
                               ( s(6)    * Inverse_3_TL ) )


      ! ----------------------------
      ! Sum predictors across layers
      ! ----------------------------

      DO i = 1, n_Predictors
        Predictor_TL(i,k) = x_TL(i,k) + x_TL(i,k-1)
      END DO

    END DO Layer_loop

  END SUBROUTINE CRTM_Integrated_Predictors_TL





!#------------------------------------------------------------------------------#
!#                     ADJOINT COMPONENT PREDICTOR ROUTINES                     #
!#------------------------------------------------------------------------------#


!--------------------------------------------------------------------------------
! NAME:
!       CRTM_Standard_Predictors_AD
!
! PURPOSE:
!       Subroutine to compute the integrated absorber amount INDEPENDENT
!       predictors for the adjoint gas absorption model.
!
! CATEGORY:
!       CRTM : Gas Absorption : Predictor
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Standard_Predictors_AD( Atmosphere,   &  ! Input
!                                         Predictor_AD, &  ! Input, Istd x K
!                                         Atmosphere_AD )  ! Output
!                                         
!
! INPUT ARGUMENTS:
!       Atmosphere:     CRTM Atmosphere structure containing the atmospheric
!                       state data.
!                       UNITS:      N/A
!                       TYPE:       TYPE( CRTM_Atmosphere_type )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       Predictor_AD:   Adjoint of the layer predictor arrays.
!                       UNITS:      Varies with predictor type.
!                       TYPE:       REAL( fp_kind )
!                       DIMENSION:  Istd x K
!                       ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None
!
! OUTPUT ARGUMENTS:
!       Atmosphere_AD:  CRTM Atmosphere structure containing the adjoints of
!                       the standard predictors.
!                       UNITS:      N/A
!                       TYPE:       TYPE( CRTM_Atmosphere_type )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None
!
! CALLS:
!       None
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note that the output adjoint argument, Atmosphere_AD, has INTENT of
!       IN OUT. This is because the pressure, temperature, and absorber
!       components of the Atmosphere_AD structure are assumed to have some
!       initial value (which could simply be zero) that is added to when
!       contructing the pressure, temperature and absorber adjoints.
!
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Standard_Predictors_AD( Atmosphere,   &  ! Input
                                          Predictor_AD, &  ! Input, Istd x K
                                          Atmosphere_AD )  ! Output




    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    TYPE( CRTM_Atmosphere_type ),        INTENT( IN )    :: Atmosphere
    REAL( fp_kind ), DIMENSION( :, : ),  INTENT( IN )    :: Predictor_AD  ! Istd x K

    ! -- Output
    TYPE( CRTM_Atmosphere_type ),        INTENT( IN OUT) :: Atmosphere_AD


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Standard_Predictors_AD'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: k
    REAL( fp_kind ) :: p2, p2_AD
    REAL( fp_kind ) :: t2, t2_AD
    REAL( fp_kind ) :: t4
    INTEGER :: H2O_Index


    !#--------------------------------------------------------------------------#
    !#             -- GET THE Atmosphere INDEX FOR WATER VAPOR --               #
    !#--------------------------------------------------------------------------#

    H2O_Index = MINLOC( ABS( Atmosphere%Absorber_ID - H2O_ID ), DIM = 1 )



    !#--------------------------------------------------------------------------#
    !#           -- CALCULATE THE STANDARD PREDICTOR SET ADJOINTS --            #
    !#                                                                          #
    !# Don't have to loop backwards here as this is a parallel loop.            #
    !#                                                                          #
    !# Pressure and temperature squared adjoint terms are not zeroed out every  #
    !# loop iteration as they are local to each iteration and can be simply     #
    !# re-assigned.                                                             #
    !#--------------------------------------------------------------------------#

    Layer_loop: DO k = 1, Atmosphere%n_Layers


      ! -- Precalculate the squared terms
      p2 = Atmosphere%Pressure( k )    * Atmosphere%Pressure( k )
      t2 = Atmosphere%Temperature( k ) * Atmosphere%Temperature( k )
      t4 = t2 * t2

      ! -- Pressure squared adjoint
      p2_AD =                                 Predictor_AD( 4, k )   + &   ! Predictor #4, P^2
              ( Atmosphere%Temperature( k ) * Predictor_AD( 7, k ) ) + &   ! Predictor #7, T.P^2
              ( t2                          * Predictor_AD( 8, k ) )       ! Predictor #8, T^2.P^2

      ! -- Temperature squared adjoint
      t2_AD =                                         Predictor_AD( 3, k )         + &  ! Predictor #3, T^2
              ( Atmosphere%Pressure( k )            * Predictor_AD( 6, k ) )       + &  ! Predictor #6, T^2.P
              ( p2                                  * Predictor_AD( 8, k ) )       + &  ! Predictor #8, T^2.P^2
              (-Atmosphere%Absorber( k, H2O_Index ) * Predictor_AD( 11, k ) / t4 )      ! Predictor #11, W/T^2

      ! -- Water vapor adjoint
      Atmosphere_AD%Absorber( k, H2O_Index ) = Atmosphere_AD%Absorber( k, H2O_Index ) + &
        Predictor_AD( 10, k ) + &       ! Predictor #10, W
        ( Predictor_AD( 11, k ) / t2 )  ! Predictor #11, W/T^2

      ! -- Temperature adjoint
      Atmosphere_AD%Temperature( k ) = Atmosphere_AD%Temperature( k ) + &
        ( TWO * Atmosphere%Temperature( k ) * t2_AD )       + &  ! T^2 term
                                     Predictor_AD( 1, k )   + &  ! Predictor #1, T
        ( Atmosphere%Pressure( k ) * Predictor_AD( 5, k ) ) + &  ! Predictor #5, T.P
        ( p2                       * Predictor_AD( 7, k ) )      ! Predictor #7, T.P^2

      ! -- Pressure adjoint
      Atmosphere_AD%Pressure( k ) = Atmosphere_AD%Pressure( k ) + &
        ( TWO * Atmosphere%Pressure( k ) * p2_AD )             + &                     ! P^2 term
                                        Predictor_AD( 2, k )   + &                     ! Predictor #2, P
        ( Atmosphere%Temperature( k ) * Predictor_AD( 5, k ) ) + &                     ! Predictor #5, T.P
        ( t2                          * Predictor_AD( 6, k ) ) + &                     ! Predictor #6, T^2.P
        ( POINT_25 * (Atmosphere%Pressure( k )**(-POINT_75)) * Predictor_AD( 9, k ) )  ! Predictor #9, P^1/4

    END DO Layer_loop

  END SUBROUTINE CRTM_Standard_Predictors_AD


!--------------------------------------------------------------------------------
! NAME:
!       CRTM_Integrated_Predictors_AD
!
! PURPOSE:
!       Subroutine to compute the integrated absorber amount DEPENDENT
!       predictors for the adjoint gas absorption model.
!
! CATEGORY:
!       CRTM : Gas Absorption : Predictor
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Integrated_Predictors_AD( Atmosphere,    &  ! Input
!                                           IntAbsorber,   &  ! Input, 0:K
!                                           Predictor_AD,  &  ! Input, Iint x K
!                                           Atmosphere_AD, &  ! Output
!                                           IntAbsorber_AD )  ! Output, 0:K
!
! INPUT ARGUMENTS:
!       Atmosphere:      CRTM Atmosphere structure containing the atmospheric
!                        state data.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_Atmosphere_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       IntAbsorber:     Array containing profile LEVEL integrated
!                        absorber data.
!                        UNITS:      Absorber dependent
!                        TYPE:       REAL( fp_kind )
!                        DIMENSION:  Rank-1 (0:K)
!                        ATTRIBUTES: INTENT( OUT )
!
!       Predictor_AD:    Adjoint of the layer predictor arrays.
!                        UNITS:      Varies with predictor type.
!                        TYPE:       REAL( fp_kind )
!                        DIMENSION:  Rank-2 (Iint x K)
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None
!
! OUTPUT ARGUMENTS:
!       Atmosphere_AD:   CRTM Atmosphere structure containing the adjoints of
!                        the integrated predictors.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_Atmosphere_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN OUT )
!
!       IntAbsorber_AD:  Profile LEVEL adjoint integrated absorber
!                        amount array.
!                        UNITS:      Absorber dependent
!                        TYPE:       REAL( fp_kind )
!                        DIMENSION:  Rank-1 (0:K)
!                        ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None
!
! CALLS:
!       None
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note that the output adjoint arguments, Atmosphere_AD and IntAbsorber_AD,
!       have INTENTs of IN OUT. This is because the pressure, temperature, and
!       absorber components of the Atmosphere_AD structure and the IntAbsorber
!       array itself are assumed to have some initial value (which could simply
!       be zero) that is added to when contructing the pressure, temperature
!       and absorber adjoints.
!
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Integrated_Predictors_AD( Atmosphere,    &  ! Input
                                            IntAbsorber,   &  ! Input, 0:K
                                            Predictor_AD,  &  ! Input, Iint x K
                                            Atmosphere_AD, &  ! Output
                                            IntAbsorber_AD )  ! Output, 0:K



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_Atmosphere_type ),       INTENT( IN )     :: Atmosphere
    REAL( fp_kind ), DIMENSION( 0: ),   INTENT( IN )     :: IntAbsorber     ! 0:K
    REAL( fp_kind ), DIMENSION( :, : ), INTENT( IN )     :: Predictor_AD    ! Iint x K

    ! -- Output
    TYPE( CRTM_Atmosphere_type ),       INTENT( IN OUT ) :: Atmosphere_AD
    REAL( fp_kind ), DIMENSION( 0: ),   INTENT( IN OUT ) :: IntAbsorber_AD  ! 0:K


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Integrated_Predictors_AD'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, k
    INTEGER :: n_Predictors

    REAL( fp_kind ) :: d_IntAbsorber_AD
    REAL( fp_kind ) :: Factor_1_AD
    REAL( fp_kind ) :: Factor_2_AD
    REAL( fp_kind ) :: Inverse_1
    REAL( fp_kind ) :: Inverse_2
    REAL( fp_kind ) :: Inverse_3
    REAL( fp_kind ) :: Inverse_4
    REAL( fp_kind ) :: Inverse_1_AD
    REAL( fp_kind ) :: Inverse_2_AD
    REAL( fp_kind ) :: Inverse_3_AD
    REAL( fp_kind ) :: Multiplier
    REAL( fp_kind ) :: Add_Factor

    ! -- Square of the absorber amount. 0:K
    REAL( fp_kind ), DIMENSION( 0:Atmosphere%n_Layers ) :: IntAbsorber_2
    REAL( fp_kind ), DIMENSION( 0:Atmosphere%n_Layers ) :: IntAbsorber_2_AD

    ! -- Multiplicative factors, K
    REAL( fp_kind ), DIMENSION( Atmosphere%n_Layers ) :: d_IntAbsorber
    REAL( fp_kind ), DIMENSION( Atmosphere%n_Layers ) :: Factor_1
    REAL( fp_kind ), DIMENSION( Atmosphere%n_Layers ) :: Factor_2

    ! -- Intermediate summation arrays, Iint x 0:K and Iint
    REAL( fp_kind ), DIMENSION( SIZE( Predictor_AD, DIM=1 ), &
                                0:Atmosphere%n_Layers )       :: s
    REAL( fp_kind ), DIMENSION( SIZE( Predictor_AD, DIM=1 ) ) :: s_AD

    ! -- LEVEL predictor, Iint x 0:K
    REAL( fp_kind ), DIMENSION( SIZE( Predictor_AD, DIM=1 ), &
                                0:Atmosphere%n_Layers )       :: x_AD



    !#--------------------------------------------------------------------------#
    !#                 -- DETERMINE THE NUMBER OF PREDICTORS --                 #
    !#--------------------------------------------------------------------------#

    n_Predictors = SIZE( Predictor_AD, DIM = 1 )



    !#--------------------------------------------------------------------------#
    !#          -- RECALCULATE THE INTERMEDIATE FORWARD MODEL SUMS --           #
    !#--------------------------------------------------------------------------#

    ! ------------------------------
    ! Initialise top level of arrays
    ! ------------------------------

    IntAbsorber_2( 0 ) = IntAbsorber(0) * IntAbsorber(0)
    s( :, 0 ) = ZERO


    ! ----------------
    ! Loop over Layers
    ! ----------------

    Forward_Layer_Loop: DO k = 1, Atmosphere%n_Layers


      ! -----------------------------------------
      ! Calculate absorber multiplicative factors
      ! and save for adjoint calculation.
      ! -----------------------------------------

      IntAbsorber_2(k) = IntAbsorber(k) * IntAbsorber(k)

      d_IntAbsorber(k) = IntAbsorber(k) - IntAbsorber(k-1)                            ! For * terms
      Factor_1(k)      = ( IntAbsorber(k)   + IntAbsorber(k-1)   ) * d_IntAbsorber(k) ! For ** terms
      Factor_2(k)      = ( IntAbsorber_2(k) + IntAbsorber_2(k-1) ) * d_IntAbsorber(k) ! For *** terms


      ! ------------------------------------------------------
      ! Calculate and save the intermediate sums at each layer
      ! ------------------------------------------------------

      s(1,k) = s(1,k-1) + ( Atmosphere%Temperature(k) * d_IntAbsorber(k) )  ! T*
      s(2,k) = s(2,k-1) + ( Atmosphere%Pressure(k)    * d_IntAbsorber(k) )  ! P*

      s(3,k) = s(3,k-1) + ( Atmosphere%Temperature(k) * Factor_1(k) )       ! T**
      s(4,k) = s(4,k-1) + ( Atmosphere%Pressure(k)    * Factor_1(k) )       ! P**

      s(5,k) = s(5,k-1) + ( Atmosphere%Temperature(k) * Factor_2(k) )       ! T***
      s(6,k) = s(6,k-1) + ( Atmosphere%Pressure(k)    * Factor_2(k) )       ! P***

    END DO Forward_Layer_Loop



    !#--------------------------------------------------------------------------#
    !#                -- INITIALISE LOCAL ADJOINT VARIABLES --                  #
    !#--------------------------------------------------------------------------#

    x_AD( :, Atmosphere%n_Layers )          = ZERO
    s_AD( : )                               = ZERO
    IntAbsorber_2_AD( Atmosphere%n_Layers ) = ZERO



    !#--------------------------------------------------------------------------#
    !#            -- CALCULATE THE INTEGRATED PREDICTOR ADJOINTS --             #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------
    ! Here loop order does matter as this
    ! is a sequential loop
    ! -----------------------------------

    Adjoint_Layer_Loop: DO k = Atmosphere%n_Layers, 1, -1


      ! ---------------------------------
      ! Calculate the normalising factors
      ! for the integrated predictors
      ! ---------------------------------

      IF ( IntAbsorber( k ) > MINIMUM_ABSORBER_AMOUNT ) THEN
        Inverse_1 = ONE / IntAbsorber( k )
      ELSE
        Inverse_1 = ZERO
      END IF

      Inverse_2 = Inverse_1 * Inverse_1
      Inverse_3 = Inverse_2 * Inverse_1
      Inverse_4 = Inverse_3 * Inverse_1


      ! --------------------------------------------
      ! Adjoint of predictor summation across Layers
      ! --------------------------------------------


      DO i = 1, n_Predictors
        x_AD( i, k )   = x_AD( i, k ) + Predictor_AD( i, k )
        x_AD( i, k-1 ) = Predictor_AD( i, k )
      END DO


      ! --------------------------------------------------------------
      ! Adjoint of the LEVEL integrated predictors intermediate sums
      !
      ! Note that the adjoint variables Inverse_X_AD are local to this
      ! loop iteration so they are simply assigned when they are first
      ! used.
      ! --------------------------------------------------------------

      ! -- P* and T*, Predictor indices #2 and 1
      ! -- Simply assign a value for Inverse_1_AD
      Multiplier   = POINT_5 * Inverse_1
      s_AD( 1 )    = s_AD( 1 ) + ( Multiplier * x_AD( 1, k ) )
      s_AD( 2 )    = s_AD( 2 ) + ( Multiplier * x_AD( 2, k ) )
      Inverse_1_AD = POINT_5 * ( ( s( 1, k ) * x_AD( 1, k ) ) + &
                                 ( s( 2, k ) * x_AD( 2, k ) ) )

      ! -- P** and T**, Predictor indices #4 and 3
      Multiplier   = POINT_5 * Inverse_2
      s_AD( 3 )    = s_AD( 3 ) + ( Multiplier * x_AD( 3, k ) )
      s_AD( 4 )    = s_AD( 4 ) + ( Multiplier * x_AD( 4, k ) )
      Inverse_2_AD = POINT_5 * ( ( s( 3, k ) * x_AD( 3, k ) ) + &
                                 ( s( 4, k ) * x_AD( 4, k ) ) )

      ! -- P*** and T***, Predictor indices #6 and 5
      Multiplier   = POINT_75 * Inverse_3
      s_AD( 5 )    = s_AD( 5 ) + ( Multiplier * x_AD( 5, k ) )
      s_AD( 6 )    = s_AD( 6 ) + ( Multiplier * x_AD( 6, k ) )
      Inverse_3_AD = POINT_75 * ( ( s( 5, k ) * x_AD( 5, k ) ) + &
                                  ( s( 6, k ) * x_AD( 6, k ) ) )

      ! -- Adjoint of Inverse terms. Note that the Inverse_X_AD
      ! -- terms are *not* zeroed out as they are re-assigned values
      ! -- each loop iteration above.
      IntAbsorber_AD( k ) = IntAbsorber_AD( k ) - (         Inverse_2 * Inverse_1_AD ) - &
                                                  ( TWO *   Inverse_3 * Inverse_2_AD ) - &
                                                  ( THREE * Inverse_4 * Inverse_3_AD )


      ! ---------------------------------
      ! Pressure and temperature adjoints
      ! ---------------------------------

      ! -- Pressure
      Atmosphere_AD%Pressure( k ) = Atmosphere_AD%Pressure( k ) + &
                                    ( d_IntAbsorber( k ) * s_AD( 2 ) ) + &  ! P*
                                    ( Factor_1( k )      * s_AD( 4 ) ) + &  ! P**
                                    ( Factor_2( k )      * s_AD( 6 ) )      ! P***


      ! -- Temperature
      Atmosphere_AD%Temperature( k ) = Atmosphere_AD%Temperature( k ) + &
                                       ( d_IntAbsorber( k ) * s_AD( 1 ) ) + &  ! T*
                                       ( Factor_1( k )      * s_AD( 3 ) ) + &  ! T**
                                       ( Factor_2( k )      * s_AD( 5 ) )      ! T***


      ! --------------------------------------------------
      ! Adjoint of the absorber amount
      !
      ! Note that the adjoint variables Factor_X_AD and
      ! d_IntAbsorber_AD are local to this loop iteration
      ! so they are simply assigned when they are first
      ! used (and thus not zeroed out at the end of each
      ! iteration)
      !
      ! Note there are no
      !   s_AD() = 0
      ! because all the tangent-linear forms are
      !   s_TL() = s_TL() + (...)
      ! summing from the previous Layer.
      ! --------------------------------------------------

      ! -- Multiplicative factors
      Factor_1_AD = ( Atmosphere%Temperature( k ) * s_AD( 3 ) ) + &
                    ( Atmosphere%Pressure( k )    * s_AD( 4 ) )

      Factor_2_AD = ( Atmosphere%Temperature( k ) * s_AD( 5 ) ) + &
                    ( Atmosphere%Pressure( k )    * s_AD( 6 ) )

      ! -- Adjoint of IntIntAbsorber_2().
      ! -- Note that IntAbsorber_2_AD() is a LOCAL adjoint variable,
      ! -- so the initialisation of IntAbsorber_2_AD( k-1 ) here for
      ! -- each "k-1" is o.k. rather than
      ! --   IntAbsorber_2_AD( k-1 ) = IntAbsorber_2_AD( k-1 ) + ( d_IntAbsorber( k ) * Factor_2_AD )
      ! --   IntAbsorber_2_AD(  k  ) = IntAbsorber_2_AD(  k  ) + ( d_IntAbsorber( k ) * Factor_2_AD )
      ! -- since only IntAbsorber_2_AD( n_Layers ) is initialised outside the
      ! -- current layer loop.
      IntAbsorber_2_AD( k-1 ) = d_IntAbsorber( k ) * Factor_2_AD
      IntAbsorber_2_AD(  k  ) = IntAbsorber_2_AD(  k  ) + IntAbsorber_2_AD( k-1 )

      ! -- Adjoint of IntAbsorber(). Here, since IntAbsorber_AD() is NOT a local adjoint
      ! -- variable, we can't use the same form as for IntAbsorber_2_AD() above.
      d_IntAbsorber_AD = ( Atmosphere%Temperature( k ) * s_AD( 1 ) ) + &
                         ( Atmosphere%Pressure( k )    * s_AD( 2 ) ) + &
                         ( ( IntAbsorber( k )   + IntAbsorber( k-1 )   ) * Factor_1_AD ) + &
                         ( ( IntAbsorber_2( k ) + IntAbsorber_2( k-1 ) ) * Factor_2_AD )

      Add_Factor = d_IntAbsorber( k ) * Factor_1_AD
      IntAbsorber_AD( k-1 ) = IntAbsorber_AD( k-1 ) + Add_Factor - d_IntAbsorber_AD
      IntAbsorber_AD(  k  ) = IntAbsorber_AD(  k  ) + Add_Factor + d_IntAbsorber_AD + &
                                                      ( TWO * IntAbsorber(k) * IntAbsorber_2_AD(k) )
      IntAbsorber_2_AD( k ) = ZERO

    END DO Adjoint_Layer_Loop

    ! -- Adjoint of level 0 IntAbsorber
    IntAbsorber_AD(0)   = IntAbsorber_AD(0) + ( TWO * IntAbsorber(0) * IntAbsorber_2_AD(0) )
    IntAbsorber_2_AD(0) = ZERO

  END SUBROUTINE CRTM_Integrated_Predictors_AD

END MODULE CRTM_AtmAbsorption_Predictor


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: CRTM_AtmAbsorption_Predictor.f90,v 1.4 2006/05/25 19:26:00 wd20pd Exp $
!
! $Date: 2006/05/25 19:26:00 $
!
! $Revision: 1.4 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_AtmAbsorption_Predictor.f90,v $
! Revision 1.4  2006/05/25 19:26:00  wd20pd
! - Removed redundant parameter definitions.
!
! Revision 1.3  2006/05/02 14:58:34  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.2  2005/01/28 21:10:52  paulv
! - Corrected a number of naming and indexing errors.
!
! Revision 1.1  2005/01/21 18:28:01  paulv
! Initial checkin. Untested.
!
!
!


