!
! CRTM_Predictor
!
! Module continaing routines to compute the predictors for the gas
! absorption (AtmAbsorption) model.
!
! Combines the functionality of
!   CRTM_AtmAbsorption_IntAbsorber.f90
! and
!   CRTM_AtmAbsorption_Predictor.f90
! but using the separate CRTM_Predictor_type structure.
!
! The Compute_Predictor routines replace the SetUp_AtmAbsorption routines
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 29-Aug-2006
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_Predictor

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,               ONLY: fp=>fp_kind
  USE Message_Handler,          ONLY: SUCCESS, FAILURE, Display_Message
  USE CRTM_Parameters
  USE CRTM_Atmosphere_Define,   ONLY: CRTM_Atmosphere_type, H2O_ID, O3_ID
  USE CRTM_GeometryInfo_Define, ONLY: CRTM_GeometryInfo_type
  USE CRTM_Predictor_Define,    ONLY: CRTM_Predictor_type      , &
                                      CRTM_Associated_Predictor, &
                                      CRTM_Destroy_Predictor   , &
                                      CRTM_Allocate_Predictor  , &
                                      CRTM_Assign_Predictor
  ! Disable implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! CRTM_Predictor structure data type
  ! in the CRTM_Predictor_Define module
  PUBLIC :: CRTM_Predictor_type
  ! CRTM_Predictor structure routines inherited
  ! from the CRTM_Predictor_Define module
  PUBLIC :: CRTM_Associated_Predictor
  PUBLIC :: CRTM_Destroy_Predictor
  PUBLIC :: CRTM_Allocate_Predictor
  PUBLIC :: CRTM_Assign_Predictor
  ! Science routines in this module
  PUBLIC :: CRTM_Compute_Predictors
  PUBLIC :: CRTM_Compute_Predictors_TL
  PUBLIC :: CRTM_Compute_Predictors_AD
  ! Internal variable structure
  PUBLIC :: CRTM_APVariables_type

  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_Predictor.f90,v 2.1 2006/08/31 16:54:29 frpv Exp $'


  ! -------------------------------------------------
  ! Structure definition to hold integrated predictor
  ! forward variables across FWD, TL, and AD calls
  ! -------------------------------------------------
  TYPE :: CRTM_APVariables_type
    PRIVATE
    REAL(fp), DIMENSION(0:MAX_N_LAYERS,MAX_N_ABSORBERS) :: A_2 = ZERO
    REAL(fp), DIMENSION(MAX_N_LAYERS,MAX_N_ABSORBERS) :: d_A = ZERO
    REAL(fp), DIMENSION(MAX_N_LAYERS,MAX_N_ABSORBERS) :: Factor_1 = ZERO
    REAL(fp), DIMENSION(MAX_N_LAYERS,MAX_N_ABSORBERS) :: Factor_2 = ZERO
    REAL(fp), DIMENSION(MAX_N_INTEGRATED_PREDICTORS,0:MAX_N_LAYERS,MAX_N_ABSORBERS) :: s = ZERO
  END TYPE CRTM_APVariables_type


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                       ## PRIVATE MODULE ROUTINES ##                        ##
!##                                                                            ##
!################################################################################
!################################################################################

!================================================================================
!                  -- INTEGRATED ABSORBER COMPUTATION ROUTINES --
!================================================================================

!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_IntAbsorber
!
! PURPOSE:
!       Subroutine to compute the integrated absorber profiles.
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_IntAbsorber( Atmosphere, &  ! Input
!                                      Predictor   )  ! Output
!
! INPUT ARGUMENTS:
!       Atmosphere:   CRTM Atmosphere structure containing the atmospheric
!                     state data.
!                     UNITS:      N/A
!                     TYPE:       TYPE(CRTM_Atmosphere_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Predictor:    CRTM Predictor structure containing the calculated
!                     integrated absorber profiles
!                     UNITS:      N/A
!                     TYPE:       TYPE(CRTM_Predictor_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_IntAbsorber( Atm, &  ! Input
                                       Pred )  ! Output
    ! Arguments
    TYPE(CRTM_Atmosphere_type), INTENT(IN)     :: Atm
    TYPE(CRTM_Predictor_type),  INTENT(IN OUT) :: Pred
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_IntAbsorber'
    ! Local variables
    INTEGER  :: k
    REAL(fp) :: dPonG
    INTEGER  :: H2O_Index
    INTEGER  ::  O3_Index

    ! Initialise 0'th level amounts
    Pred%A(0,WET_ABSORBER_INDEX) = ZERO
    Pred%A(0,DRY_ABSORBER_INDEX) = TOA_PRESSURE
    Pred%A(0,OZO_ABSORBER_INDEX) = ZERO

    ! Get the atmosphere gaseous absorber indices
    H2O_Index = MINLOC(ABS(Atm%Absorber_ID - H2O_ID), DIM=1 )
    O3_Index  = MINLOC(ABS(Atm%Absorber_ID - O3_ID ), DIM=1 )

    ! Loop over layers, TOA -> SFC
    DO k = 1, Atm%n_Layers

      ! Compute dP/g for the current layer
      dPonG = RECIPROCAL_GRAVITY * (Atm%Level_Pressure(k) - Atm%Level_Pressure(k-1))

      ! Compute and accumulate the sum for the
      ! layer absorber amounts for each absorber
      Pred%A( k, WET_ABSORBER_INDEX ) = Pred%A(k-1,WET_ABSORBER_INDEX) + &
                                        (dPonG * Atm%Absorber(k,H2O_Index))
      Pred%A( k, DRY_ABSORBER_INDEX ) = Atm%Level_Pressure(k)

      Pred%A( k, OZO_ABSORBER_INDEX ) = Pred%A(k-1,OZO_ABSORBER_INDEX) + &
                                        (dPonG * Atm%Absorber(k,O3_Index))

    END DO

  END SUBROUTINE CRTM_Compute_IntAbsorber


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_IntAbsorber_TL
!
! PURPOSE:
!       Subroutine to compute the tangent-linear integrated absorber profiles.
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_IntAbsorber_TL( Atmosphere,    &  ! Input
!                                         Atmosphere_TL, &  ! Input
!                                         Predictor_TL   )  ! Output
!
! INPUT ARGUMENTS:
!       Atmosphere:     CRTM Atmosphere structure containing the atmospheric
!                       state data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Atmosphere_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Atmosphere_TL:  CRTM Atmosphere structure containing the tangent-linear
!                       atmospheric state data, i.e. the perturbations.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Atmosphere_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!
! OUTPUT ARGUMENTS:
!       Predictor_TL:   CRTM Predictor structure containing the calculated
!                       tangent-linear integrated absorber profiles
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Predictor_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_IntAbsorber_TL( Atm,    &  ! Input
                                          Atm_TL, &  ! Input
                                          Pred_TL )  ! Output
    ! Arguments
    TYPE(CRTM_Atmosphere_type), INTENT(IN)     :: Atm
    TYPE(CRTM_Atmosphere_type), INTENT(IN)     :: Atm_TL
    TYPE(CRTM_Predictor_type),  INTENT(IN OUT) :: Pred_TL
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_IntAbsorber_TL'
    ! Local variables
    INTEGER  :: k
    REAL(fp) :: dPonG
    REAL(fp) :: dPonG_TL
    INTEGER  :: H2O_Index
    INTEGER  ::  O3_Index

    ! Initalise 0'th level amounts
    Pred_TL%A(0,:) = ZERO

    ! Get the atmosphere gaseous absorber indices
    H2O_Index = MINLOC(ABS(Atm%Absorber_ID - H2O_ID), DIM=1 )
     O3_Index = MINLOC(ABS(Atm%Absorber_ID -  O3_ID), DIM=1 )

    ! Loop over layers, TOA -> SFC
    DO k = 1, Atm_TL%n_Layers

      ! Compute dP/g for the current layer
      dPonG    = RECIPROCAL_GRAVITY * (Atm%Level_Pressure(k)    - Atm%Level_Pressure(k-1))
      dPonG_TL = RECIPROCAL_GRAVITY * (Atm_TL%Level_Pressure(k) - Atm_TL%Level_Pressure(k-1))

      ! Compute and accumulate the sum for the
      ! layer absorber amounts for each absorber
      Pred_TL%A(k,WET_ABSORBER_INDEX) = Pred_TL%A(k-1,WET_ABSORBER_INDEX) + &
                                        (dPonG * Atm_TL%Absorber(k,H2O_Index)) + &
                                        (dPonG_TL * Atm%Absorber(k,H2O_Index))

      Pred_TL%A(k,DRY_ABSORBER_INDEX) = Atm_TL%Level_Pressure(k)

      Pred_TL%A(k,OZO_ABSORBER_INDEX) = Pred_TL%A(k-1,OZO_ABSORBER_INDEX) + &
                                        (dPonG * Atm_TL%Absorber(k,O3_Index)) + &
                                        (dPonG_TL * Atm%Absorber(k,O3_Index))

    END DO

  END SUBROUTINE CRTM_Compute_IntAbsorber_TL


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_IntAbsorber_AD
!
! PURPOSE:
!       Subroutine to compute the adjoint of the integrated absorber profiles.
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_IntAbsorber_AD( Atmosphere,   &  ! Input
!                                         Predictor_AD, &  ! Input
!                                         Atmosphere_AD )  ! Output
!
! INPUT ARGUMENTS:
!       Atmosphere:     CRTM Atmosphere structure containing the atmospheric
!                       state data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Atmosphere_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Predictor_AD:   CRTM Predictor structure that, on input, contains the
!                       calculated adjoint integrated absorber profiles.
!                       These values are set to zero on output.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Predictor_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUT ARGUMENTS:
!       Atmosphere_AD:  CRTM Atmosphere structure containing the adjoint
!                       atmospheric state data, i.e. the Jacobians.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Atmosphere_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
! SIDE EFFECTS:
!       Components of the input structure, Predictor_AD, are set to zero
!       on output.
!
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_IntAbsorber_AD( Atm,     &  ! Input
                                          Pred_AD, &  ! Input
                                          Atm_AD   )  ! Output
    ! Arguments
    TYPE(CRTM_Atmosphere_type), INTENT(IN)     :: Atm
    TYPE(CRTM_Predictor_type),  INTENT(IN OUT) :: Pred_AD
    TYPE(CRTM_Atmosphere_type), INTENT(IN OUT) :: Atm_AD
    ! Local variables
    INTEGER  :: k
    REAL(fp) :: dPonG
    REAL(fp) :: dPonG_AD
    INTEGER  :: H2O_Index
    INTEGER  ::  O3_Index

    ! Get the atmosphere gaseous absorber indices
    H2O_Index = MINLOC(ABS(Atm%Absorber_ID - H2O_ID), DIM=1 )
     O3_Index = MINLOC(ABS(Atm%Absorber_ID -  O3_ID), DIM=1 )

    ! Loop over layers, SFC -> TOA
    DO k = Atm_AD%n_Layers, 1, -1

      ! Compute dP/g for the current layer
      dPonG = RECIPROCAL_GRAVITY * (Atm%Level_Pressure(k) - Atm%Level_Pressure(k-1))

      ! Ozone amount adjoint
      Atm_AD%Absorber(k,O3_Index) = Atm_AD%Absorber(k,O3_Index) + &
                                    (dPonG * Pred_AD%A(k,OZO_ABSORBER_INDEX))

      ! Pressure adjoint
      Atm_AD%Level_Pressure(k) = Atm_AD%Level_Pressure(k) + Pred_AD%A(k,DRY_ABSORBER_INDEX)

      ! Water vapor amount adjoint
      Atm_AD%Absorber(k,H2O_Index) = Atm_AD%Absorber(k,H2O_Index) + &
                                     (dPonG * Pred_AD%A(k,WET_ABSORBER_INDEX))


      ! dP/g adjoint
      dPonG_AD = ( Atm%Absorber(k, O3_Index) * Pred_AD%A(k,OZO_ABSORBER_INDEX)) + &
                 ( Atm%Absorber(k,H2O_Index) * Pred_AD%A(k,WET_ABSORBER_INDEX))

      Atm_AD%Level_Pressure(k-1) = Atm_AD%Level_Pressure(k-1) - (RECIPROCAL_GRAVITY * dPonG_AD)
      Atm_AD%Level_Pressure( k ) = Atm_AD%Level_Pressure( k ) + (RECIPROCAL_GRAVITY * dPonG_AD)

      ! Previous layer absorber amounts
      Pred_AD%A(k-1,OZO_ABSORBER_INDEX) = Pred_AD%A(k-1,OZO_ABSORBER_INDEX) + &
                                          Pred_AD%A( k, OZO_ABSORBER_INDEX)
      Pred_AD%A( k, OZO_ABSORBER_INDEX) = ZERO

      Pred_AD%A( k, DRY_ABSORBER_INDEX) = ZERO

      Pred_AD%A(k-1,WET_ABSORBER_INDEX) = Pred_AD%A(k-1,WET_ABSORBER_INDEX) + &
                                          Pred_AD%A( k, WET_ABSORBER_INDEX)
      Pred_AD%A( k, WET_ABSORBER_INDEX) = ZERO

    END DO

  END SUBROUTINE CRTM_Compute_IntAbsorber_AD




!================================================================================
!                      -- PREDICTOR COMPUTATION ROUTINES --
!================================================================================

!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Standard_Predictors
!
! PURPOSE:
!       Subroutine to compute the integrated absorber INDEPENDENT
!       predictors for the gas absorption model.
!
! CALLING SEQUENCE:
!       CALL CRTM_Standard_Predictors( Atmosphere, &  ! Input
!                                      Predictor   )  ! Output
!
! INPUT ARGUMENTS:
!       Atmosphere:   CRTM Atmosphere structure containing the atmospheric
!                     state data.
!                     UNITS:      N/A
!                     TYPE:       TYPE(CRTM_Atmosphere_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Predictor:    CRTM Predictor structure containing the calculated
!                     standard predictors.
!                     UNITS:      N/A
!                     TYPE:       TYPE(CRTM_Predictor_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Standard_Predictors( Atm, &  ! Input
                                       Pred )  ! Output, Istd x K
    ! Arguments
    TYPE(CRTM_Atmosphere_type), INTENT(IN)     :: Atm
    TYPE(CRTM_Predictor_type),  INTENT(IN OUT) :: Pred
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Standard_Predictors'
    ! Local variables
    INTEGER  :: k
    REAL(fp) :: p2
    REAL(fp) :: t2
    INTEGER  :: H2O_Index

    ! Get the H2O absorber index
    H2O_Index = MINLOC(ABS(Atm%Absorber_ID - H2O_ID), DIM=1)

    ! Compute the standard predictor set
    Layer_Loop: DO k = 1, Atm%n_Layers

      ! Precalculate the squared terms
      p2 = Atm%Pressure(k)    * Atm%Pressure(k)
      t2 = Atm%Temperature(k) * Atm%Temperature(k)

      ! Calculate the standard predictors
      Pred%X( 1,k) = Atm%Temperature(k)
      Pred%X( 2,k) = Atm%Pressure(k)
      Pred%X( 3,k) = t2
      Pred%X( 4,k) = p2
      Pred%X( 5,k) = Atm%Temperature(k) * Atm%Pressure(k)
      Pred%X( 6,k) = t2 * Atm%Pressure(k)
      Pred%X( 7,k) = Atm%Temperature(k) * p2
      Pred%X( 8,k) = t2 * p2
      Pred%X( 9,k) = Atm%Pressure(k)**POINT_25
      Pred%X(10,k) = Atm%Absorber(k,H2O_Index)
      Pred%X(11,k) = Atm%Absorber(k,H2O_Index) / t2

    END DO Layer_Loop

  END SUBROUTINE CRTM_Standard_Predictors


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Integrated_Predictors
!
! PURPOSE:
!       Subroutine to compute the integrated absorber DEPENDENT
!       predictors for the gas absorption model.
!
! CALLING SEQUENCE:
!       CALL CRTM_Integrated_Predictors( Atmosphere,  &  ! Input
!                                        Predictor,   &  ! In/Output
!                                        APVariables  )  ! Internal variable output
!
! INPUT ARGUMENTS:
!       Atmosphere:   CRTM Atmosphere structure containing the atmospheric
!                     state data.
!                     UNITS:      N/A
!                     TYPE:       TYPE(CRTM_Atmosphere_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Predictor:    CRTM_Predictor structure containing the calculated
!                     integrated predictors.
!                     UNITS:      N/A
!                     TYPE:       TYPE(CRTM_Predictor)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
!       APVariables:  Structure containing internal variables required for
!                     subsequent tangent-linear or adjoint model calls.
!                     The contents of this structure are NOT accessible
!                     outside of the CRTM_Predictor module.
!                     UNITS:      N/A
!                     TYPE:       CRTM_APVariables_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)

!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Integrated_Predictors( Atm,  &  ! Input
                                         Pred, &  ! Input/output
                                         APV   )  ! Internal variable output
    ! Arguments
    TYPE(CRTM_Atmosphere_type),  INTENT(IN)      :: Atm
    TYPE(CRTM_Predictor_type),   INTENT(IN OUT)  :: Pred
    TYPE(CRTM_APVariables_type), INTENT(OUT)     :: APV
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Integrated_Predictors'
    ! Local variables
    INTEGER :: i, i1, j, k
    REAL(fp) :: Inverse_1
    REAL(fp) :: Inverse_2
    REAL(fp) :: Inverse_3
    ! LEVEL Predictor, Iint x 0:K
    REAL(fp), DIMENSION(MAX_N_INTEGRATED_PREDICTORS,0:Atm%n_Layers) :: xL

    ! Begin absorber loop
    Absorber_Loop: DO j = 1, Pred%n_Absorbers

      ! Determine being index of current absorber predictors
      i1 = MAX_N_STANDARD_PREDICTORS + ((j-1) * MAX_N_INTEGRATED_PREDICTORS) + 1

      ! Initialise values
      APV%A_2(0,j) = Pred%A(0,j) * Pred%A(0,j)
      APV%s(:,0,j) = ZERO
      xL(:,0) = ZERO

      ! Compute the integrated predictor set
      Layer_Loop: DO k = 1, Pred%n_Layers

        ! Calculate Absorber multiplicative Factors
        APV%A_2(k,j)      = Pred%A(k,j)*Pred%A(k,j)
        APV%d_A(k,j)      = Pred%A(k,j)-Pred%A(k-1,j)                      ! For * terms
        APV%Factor_1(k,j) = (Pred%A(k,j)  + Pred%A(k-1,j) ) * APV%d_A(k,j) ! For ** terms
        APV%Factor_2(k,j) = (APV%A_2(k,j) + APV%A_2(k-1,j)) * APV%d_A(k,j) ! For *** terms

        ! Calculate the intermediate sums
        APV%s(1,k,j) = APV%s(1,k-1,j) + ( Atm%Temperature(k) * APV%d_A(k,j) )      ! T*
        APV%s(2,k,j) = APV%s(2,k-1,j) + ( Atm%Pressure(k)    * APV%d_A(k,j) )      ! P*
        APV%s(3,k,j) = APV%s(3,k-1,j) + ( Atm%Temperature(k) * APV%Factor_1(k,j) ) ! T**
        APV%s(4,k,j) = APV%s(4,k-1,j) + ( Atm%Pressure(k)    * APV%Factor_1(k,j) ) ! P**
        APV%s(5,k,j) = APV%s(5,k-1,j) + ( Atm%Temperature(k) * APV%Factor_2(k,j) ) ! T***
        APV%s(6,k,j) = APV%s(6,k-1,j) + ( Atm%Pressure(k)    * APV%Factor_2(k,j) ) ! P***

        ! Calculate the normalising factors
        ! for the integrated predictors
        IF ( Pred%A(k,j) > MINIMUM_ABSORBER_AMOUNT ) THEN
          Inverse_1 = ONE / Pred%A(k,j)
        ELSE
          Inverse_1 = ZERO
        END IF
        Inverse_2 = Inverse_1 * Inverse_1
        Inverse_3 = Inverse_2 * Inverse_1

        ! Compute the LEVEL integrated predictors
        xL(1,k) = POINT_5  * APV%s(1,k,j) * Inverse_1  ! T*
        xL(2,k) = POINT_5  * APV%s(2,k,j) * Inverse_1  ! P*
        xL(3,k) = POINT_5  * APV%s(3,k,j) * Inverse_2  ! T**
        xL(4,k) = POINT_5  * APV%s(4,k,j) * Inverse_2  ! P**
        xL(5,k) = POINT_75 * APV%s(5,k,j) * Inverse_3  ! T***
        xL(6,k) = POINT_75 * APV%s(6,k,j) * Inverse_3  ! P***

        ! Sum predictors for current absorber across layers
        DO i = 1, MAX_N_INTEGRATED_PREDICTORS
          Pred%X(i1+i-1,k) = xL(i,k) + xL(i,k-1)
        END DO

      END DO Layer_Loop
    END DO Absorber_Loop

  END SUBROUTINE CRTM_Integrated_Predictors


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Standard_Predictors_TL
!
! PURPOSE:
!       Subroutine to compute the integrated absorber INDEPENDENT
!       tangent-linear predictors for the gas absorption model.
!
! CALLING SEQUENCE:
!       CALL CRTM_Standard_Predictors_TL( Atmosphere,    &  ! Input
!                                         Atmosphere_TL, &  ! Input
!                                         Predictor_TL   )  ! Output
!
! INPUT ARGUMENTS:
!       Atmosphere:     CRTM Atmosphere structure containing the atmospheric
!                       state data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Atmosphere_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Atmosphere_TL:  CRTM Atmosphere structure containing the tangent-linear
!                       atmospheric state data, i.e. the perturbations.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Atmosphere_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Predictor_TL:   CRTM Predictor structure containing the calculated
!                       tangent-linear standard predictors.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Predictor_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Standard_Predictors_TL( Atm,    &  ! Input
                                          Atm_TL, &  ! Input
                                          Pred_TL )  ! Output
    ! Arguments
    TYPE(CRTM_Atmosphere_type), INTENT(IN)  :: Atm
    TYPE(CRTM_Atmosphere_type), INTENT(IN)  :: Atm_TL
    TYPE(CRTM_Predictor_type),  INTENT(IN OUT)  :: Pred_TL
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Standard_Predictors_TL'
    ! Local variables
    INTEGER  :: k
    REAL(fp) :: p2, p2_TL
    REAL(fp) :: t2, t2_TL
    INTEGER  :: H2O_Index

    ! Get the H2O absorber index
    H2O_Index = MINLOC(ABS(Atm%Absorber_ID - H2O_ID), DIM=1)

    ! Compute the tangent-linear standard predictor set
    Layer_loop: DO k = 1, Atm%n_Layers

      ! Precalculate the squared terms
      p2 = Atm%Pressure(k)    * Atm%Pressure(k)
      t2 = Atm%Temperature(k) * Atm%Temperature(k)

      ! Tangent-linear of squared terms
      p2_TL = TWO * Atm%Pressure(k)    * Atm_TL%Pressure(k)
      t2_TL = TWO * Atm%Temperature(k) * Atm_TL%Temperature(k)
      
      ! Calculate and assign the integrated absorber independent predictors
      Pred_TL%X( 1,k) = Atm_TL%Temperature(k)
      Pred_TL%X( 2,k) = Atm_TL%Pressure(k)
      Pred_TL%X( 3,k) = t2_TL
      Pred_TL%X( 4,k) = p2_TL
      Pred_TL%X( 5,k) = ( Atm%Temperature(k) * Atm_TL%Pressure(k)    ) + &
                        ( Atm%Pressure(k)    * Atm_TL%Temperature(k) )
      Pred_TL%X( 6,k) = ( Atm%Pressure(k) * t2_TL ) + &
                        ( t2 * Atm_TL%Pressure(k) )
      Pred_TL%X( 7,k) = ( Atm%Temperature(k) * p2_TL ) + &
                        ( p2 * Atm_TL%Temperature(k) )
      Pred_TL%X( 8,k) = ( t2 * p2_TL ) + &
                        ( p2 * t2_TL )
      Pred_TL%X( 9,k) = POINT_25 * (Atm%Pressure(k)**(-POINT_75)) * Atm_TL%Pressure(k)
      Pred_TL%X(10,k) = Atm_TL%Absorber(k,H2O_Index)
      Pred_TL%X(11,k) = ( Atm_TL%Absorber(k,H2O_Index) - &
                        ( Atm%Absorber(k,H2O_Index) * t2_TL / t2 ) ) / t2

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
! CALLING SEQUENCE:
!       CALL CRTM_Integrated_Predictors_TL( Atmosphere,    &  ! Input
!                                           Predictor,     &  ! Input
!                                           Atmosphere_TL, &  ! Input
!                                           Predictor_TL,  &  ! In/Output
!                                           APVariables    )  ! Internal variable input
!
! INPUT ARGUMENTS:
!       Atmosphere:      CRTM Atmosphere structure containing the atmospheric
!                        state data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Atmosphere_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor:       CRTM Predictor structure containing the calculated
!                        integrated predictors.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Predictor_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Atmosphere_TL:   CRTM Atmosphere structure containing the tangent-linear
!                        atmospheric state data, i.e. the perturbations.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Atmosphere_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       APVariables:     Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the CRTM_Predictor module.
!                        UNITS:      N/A
!                        TYPE:       CRTM_APVariables_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
! OUTPUT ARGUMENTS:
!       Predictor_TL:    CRTM Predictor structure containing the calculated
!                        tangent-linear integrated predictors.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Predictor_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Integrated_Predictors_TL( Atm,     &  ! Input
                                            Pred,    &  ! Input
                                            Atm_TL,  &  ! Input
                                            Pred_TL, &  ! Output
                                            APV      )  ! Internal variable input
    ! Arguments
    TYPE(CRTM_Atmosphere_type),  INTENT(IN)      :: Atm
    TYPE(CRTM_Predictor_type),   INTENT(IN)      :: Pred
    TYPE(CRTM_Atmosphere_type),  INTENT(IN)      :: Atm_TL
    TYPE(CRTM_Predictor_type),   INTENT(IN OUT)  :: Pred_TL
    TYPE(CRTM_APVariables_type), INTENT(IN)      :: APV
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Integrated_Predictors_TL'
    ! Local variables
    INTEGER :: i, i1, j, k
    INTEGER :: n_Predictors
    REAL(fp) :: d_A_TL
    REAL(fp) :: Factor_1_TL
    REAL(fp) :: Factor_2_TL
    REAL(fp) :: Inverse_1
    REAL(fp) :: Inverse_2
    REAL(fp) :: Inverse_3
    REAL(fp) :: Inverse_4
    REAL(fp) :: Inverse_1_TL
    REAL(fp) :: Inverse_2_TL
    REAL(fp) :: Inverse_3_TL
    ! Square of the Absorber amount. 0:K
    REAL(fp), DIMENSION(0:Atm%n_Layers) :: A_2_TL
    ! Intermediate summation arrays. Iint
    REAL(fp), DIMENSION(MAX_N_INTEGRATED_PREDICTORS) :: s_TL
    ! LEVEL Predictor, Iint x 0:K
    REAL(fp), DIMENSION(MAX_N_INTEGRATED_PREDICTORS,0:Atm%n_Layers) :: xL_TL

    ! Begin absorber loop
    Absorber_Loop: DO j = 1, Pred_TL%n_Absorbers

      ! Determine being index of current absorber predictors
      i1 = MAX_N_STANDARD_PREDICTORS + ((j-1) * MAX_N_INTEGRATED_PREDICTORS) + 1

      ! Initialise values
      A_2_TL(0) = TWO * Pred%A(0,j) * Pred_TL%A(0,j)
      s_TL(:)    = ZERO
      xL_TL(:,0) = ZERO

      ! Compute the integrated predictor set
      Layer_loop: DO k = 1, Atm%n_Layers

        ! Calculate absorber multiplicative Factors
        A_2_TL(k) = TWO * Pred%A(k,j) * Pred_TL%A(k,j)

        ! For the * terms
        d_A_TL = Pred_TL%A(k,j) - Pred_TL%A(k-1,j)

        ! For the ** terms
        Factor_1_TL = ( ( Pred%A(k,j)    + Pred%A(k-1,j)    ) * d_A_TL ) + &
                      ( ( Pred_TL%A(k,j) + Pred_TL%A(k-1,j) ) * APV%d_A(k,j) )

        ! For the *** terms       
        Factor_2_TL = ( ( APV%A_2(k,j) + APV%A_2(k-1,j)) * d_A_TL ) + &
                      ( ( A_2_TL(k)  + A_2_TL(k-1) ) * APV%d_A(k,j))

        ! Calculate the intermediate sums
        s_TL(1) = s_TL(1) + ( Atm_TL%Temperature(k) * APV%d_A(k,j)) + &      ! T*
                            ( Atm%Temperature(k)    * d_A_TL )
        s_TL(2) = s_TL(2) + ( Atm_TL%Pressure(k)    * APV%d_A(k,j)) + &      ! P*
                            ( Atm%Pressure(k)       * d_A_TL )
        s_TL(3) = s_TL(3) + ( Atm_TL%Temperature(k) * APV%Factor_1(k,j)) + & ! T**
                            ( Atm%Temperature(k)    * Factor_1_TL )
        s_TL(4) = s_TL(4) + ( Atm_TL%Pressure(k)    * APV%Factor_1(k,j)) + & ! P**
                            ( Atm%Pressure(k)       * Factor_1_TL )
        s_TL(5) = s_TL(5) + ( Atm_TL%Temperature(k) * APV%Factor_2(k,j)) + & ! T***
                            ( Atm%Temperature(k)    * Factor_2_TL )
        s_TL(6) = s_TL(6) + ( Atm_TL%Pressure(k)    * APV%Factor_2(k,j)) + & ! P***
                            ( Atm%Pressure(k)       * Factor_2_TL )

        ! Calculate the normalising factors
        ! for the integrated predictors
        IF ( Pred%A(k,j) > MINIMUM_ABSORBER_AMOUNT ) THEN
          Inverse_1 = ONE / Pred%A(k,j)
        ELSE
          Inverse_1 = ZERO
        END IF
        Inverse_2 = Inverse_1 * Inverse_1
        Inverse_3 = Inverse_2 * Inverse_1
        Inverse_4 = Inverse_3 * Inverse_1
        Inverse_1_TL = -Inverse_2 * Pred_TL%A(k,j)
        Inverse_2_TL = -Inverse_3 * Pred_TL%A(k,j) * TWO
        Inverse_3_TL = -Inverse_4 * Pred_TL%A(k,j) * THREE

        ! Compute the tangent-linear LEVEL integrated predictors
        xL_TL(1,k) = POINT_5  * ( ( s_TL(1)      * Inverse_1    ) + &  ! T*
                                  ( APV%s(1,k,j) * Inverse_1_TL ) )
        xL_TL(2,k) = POINT_5  * ( ( s_TL(2)      * Inverse_1    ) + &  ! P*
                                  ( APV%s(2,k,j) * Inverse_1_TL ) )
        xL_TL(3,k) = POINT_5  * ( ( s_TL(3)      * Inverse_2    ) + &  ! T**
                                  ( APV%s(3,k,j) * Inverse_2_TL ) )
        xL_TL(4,k) = POINT_5  * ( ( s_TL(4)      * Inverse_2    ) + &  ! P**
                                  ( APV%s(4,k,j) * Inverse_2_TL ) )
        xL_TL(5,k) = POINT_75 * ( ( s_TL(5)      * Inverse_3    ) + &  ! T***
                                  ( APV%s(5,k,j) * Inverse_3_TL ) )
        xL_TL(6,k) = POINT_75 * ( ( s_TL(6)      * Inverse_3    ) + &  ! P***
                                  ( APV%s(6,k,j) * Inverse_3_TL ) )

        ! Sum predictors across layers
        DO i = 1, MAX_N_INTEGRATED_PREDICTORS
          Pred_TL%X(i1+i-1,k) = xL_TL(i,k) + xL_TL(i,k-1)
        END DO

      END DO Layer_loop
    END DO Absorber_Loop

  END SUBROUTINE CRTM_Integrated_Predictors_TL


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Standard_Predictors_AD
!
! PURPOSE:
!       Subroutine to compute the integrated absorber amount INDEPENDENT
!       predictors for the adjoint gas absorption model.
!
! CALLING SEQUENCE:
!       CALL CRTM_Standard_Predictors_AD( Atmosphere,   &  ! Input
!                                         Predictor_AD, &  ! Input
!                                         Atmosphere_AD )  ! Output
!                                         
!
! INPUT ARGUMENTS:
!       Atmosphere:     CRTM Atmosphere structure containing the atmospheric
!                       state data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Atmosphere_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Predictor_AD:   CRTM Predictor structure containing the calculated
!                       adjoint integrated predictors.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Predictor_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUT ARGUMENTS:
!       Atmosphere_AD:  CRTM Atmosphere structure containing the adjoints of
!                       the standard predictors.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Atmosphere_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note that the output adjoint argument, Atmosphere_AD, has INTENT of
!       IN OUT. This is because the pressure, temperature, and absorber
!       components of the Atmosphere_AD structure are assumed to have some
!       initial value (which could simply be zero) that is added to when
!       contructing the pressure, temperature and absorber adjoints.
!
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Standard_Predictors_AD( Atm,     &  ! Input
                                          Pred_AD, &  ! Input
                                          Atm_AD   )  ! Output
    ! Arguments
    TYPE(CRTM_Atmosphere_type), INTENT(IN)     :: Atm
    TYPE(CRTM_Predictor_type),  INTENT(IN OUT) :: Pred_AD
    TYPE(CRTM_Atmosphere_type), INTENT(IN OUT) :: Atm_AD
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Standard_Predictors_AD'
    ! Local variables
    INTEGER  :: k
    REAL(fp) :: p2, p2_AD
    REAL(fp) :: t2, t2_AD
    REAL(fp) :: t4
    INTEGER  :: H2O_Index

    ! Get the H2O absorber index
    H2O_Index = MINLOC(ABS(Atm%Absorber_ID - H2O_ID), DIM=1)

    ! Compute the standard predictor set
    ! Don't have to loop backwards here as this is a parallel loop
    Layer_loop: DO k = 1, Atm%n_Layers

      ! Precalculate the squared terms
      p2 = Atm%Pressure(k)    * Atm%Pressure(k)
      t2 = Atm%Temperature(k) * Atm%Temperature(k)
      t4 = t2 * t2

      ! Pressure squared adjoint
      p2_AD =                        Pred_AD%X(4,k)   + &   ! Predictor #4, P^2
              ( Atm%Temperature(k) * Pred_AD%X(7,k) ) + &   ! Predictor #7, T.P^2
              ( t2                 * Pred_AD%X(8,k) )       ! Predictor #8, T^2.P^2

      ! Temperature squared adjoint
      t2_AD =                               Pred_AD%X(3,k)     + &  ! Predictor #3, T^2
              ( Atm%Pressure(k)           * Pred_AD%X(6,k) )   + &  ! Predictor #6, T^2.P
              ( p2                        * Pred_AD%X(8,k) )   + &  ! Predictor #8, T^2.P^2
              (-Atm%Absorber(k,H2O_Index) * Pred_AD%X(11,k) / t4 )  ! Predictor #11, W/T^2

      ! Water vapor adjoint
      Atm_AD%Absorber(k,H2O_Index) = Atm_AD%Absorber(k,H2O_Index) + &
          Pred_AD%X(10,k) + &     ! Predictor #10, W
        ( Pred_AD%X(11,k) / t2 )  ! Predictor #11, W/T^2

      ! Temperature adjoint
      Atm_AD%Temperature(k) = Atm_AD%Temperature(k) + &
        ( TWO * Atm%Temperature(k) * t2_AD ) + &  ! T^2 term
                            Pred_AD%X(1,k)   + &  ! Predictor #1, T
        ( Atm%Pressure(k) * Pred_AD%X(5,k) ) + &  ! Predictor #5, T.P
        ( p2              * Pred_AD%X(7,k) )      ! Predictor #7, T.P^2

      ! Pressure adjoint
      Atm_AD%Pressure(k) = Atm_AD%Pressure(k) + &
        ( TWO * Atm%Pressure(k) * p2_AD )       + &                     ! P^2 term
                               Pred_AD%X(2,k)   + &                     ! Predictor #2, P
        ( Atm%Temperature(k) * Pred_AD%X(5,k) ) + &                     ! Predictor #5, T.P
        ( t2                 * Pred_AD%X(6,k) ) + &                     ! Predictor #6, T^2.P
        ( POINT_25 * (Atm%Pressure(k)**(-POINT_75)) * Pred_AD%X(9,k) )  ! Predictor #9, P^1/4

    END DO Layer_loop

  END SUBROUTINE CRTM_Standard_Predictors_AD


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Integrated_Predictors_AD
!
! PURPOSE:
!       Subroutine to compute the integrated absorber amount DEPENDENT
!       predictors for the adjoint gas absorption model.
!
! CALLING SEQUENCE:
!       CALL CRTM_Integrated_Predictors_AD( Atmosphere,    &  ! Input
!                                           Predictor,     &  ! Input
!                                           Predictor_AD,  &  ! In/Output
!                                           Atmosphere_AD, &  ! Output
!                                           APVariables    )  ! Internal variable input
!
! INPUT ARGUMENTS:
!       Atmosphere:      CRTM Atmosphere structure containing the atmospheric
!                        state data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Atmosphere_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor:       CRTM Predictor structure containing the calculated
!                        integrated predictors.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Predictor_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor_AD:    CRTM Predictor structure that, on input, contains
!                        the adjoint integrated predictors.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Predictor_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUT ARGUMENTS:
!       Predictor_AD:    CRTM Predictor structure that, on output, contains
!                        the adjoint integrated absorber amounts.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Predictor_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!       Atmosphere_AD:   CRTM Atmosphere structure containing the adjoints of
!                        the integrated predictors.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Atmosphere_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note that all the adjoint arguments have INTENTs of IN OUT. This is
!       because they are assumed to have some value upon entry even if they 
!       are labeled as output arguments.
!
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Integrated_Predictors_AD( Atm,     &  ! Input
                                            Pred,    &  ! Input
                                            Pred_AD, &  ! In/Output
                                            Atm_AD,  &  ! Output
                                            APV      )  ! Internal variable input
    ! Arguments
    TYPE(CRTM_Atmosphere_type),  INTENT(IN)     :: Atm
    TYPE(CRTM_Predictor_type),   INTENT(IN)     :: Pred
    TYPE(CRTM_Predictor_type),   INTENT(IN OUT) :: Pred_AD
    TYPE(CRTM_Atmosphere_type),  INTENT(IN OUT) :: Atm_AD
    TYPE(CRTM_APVariables_type), INTENT(IN)     :: APV
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Integrated_Predictors_AD'
    ! Local variables
    INTEGER :: i, i1, j, k
    INTEGER :: n_Predictors
    REAL(fp) :: d_A_AD
    REAL(fp) :: Factor_1_AD
    REAL(fp) :: Factor_2_AD
    REAL(fp) :: Inverse_1
    REAL(fp) :: Inverse_2
    REAL(fp) :: Inverse_3
    REAL(fp) :: Inverse_4
    REAL(fp) :: Inverse_1_AD
    REAL(fp) :: Inverse_2_AD
    REAL(fp) :: Inverse_3_AD
    REAL(fp) :: Multiplier
    REAL(fp) :: Add_Factor
    ! Square of the absorber amount. 0:K
    REAL(fp), DIMENSION(0:Atm%n_Layers) :: A_2_AD
    ! Intermediate summation array, Iint x 0:K and Iint
    REAL(fp), DIMENSION(MAX_N_INTEGRATED_PREDICTORS) :: s_AD
    ! LEVEL predictor, Iint x 0:K
    REAL(fp), DIMENSION(MAX_N_INTEGRATED_PREDICTORS,0:Atm%n_Layers ) :: xL_AD

    ! Begin absorber loop
    Absorber_Loop: DO j = 1, Pred_AD%n_Absorbers

      ! Determine being index of current absorber predictors
      i1 = MAX_N_STANDARD_PREDICTORS + ((j-1) * MAX_N_INTEGRATED_PREDICTORS) + 1

      ! Initialise values
      xL_AD(:,Atm%n_Layers) = ZERO
      s_AD(:)               = ZERO
      A_2_AD(Atm%n_Layers)  = ZERO

      ! Compute the integrated predictor set adjoints
      Layer_Loop: DO k = Atm%n_Layers, 1, -1

        ! Calculate the normalising factors
        ! for the integrated predictors
        IF ( Pred%A(k,j) > MINIMUM_ABSORBER_AMOUNT ) THEN
          Inverse_1 = ONE / Pred%A(k,j)
        ELSE
          Inverse_1 = ZERO
        END IF

        Inverse_2 = Inverse_1 * Inverse_1
        Inverse_3 = Inverse_2 * Inverse_1
        Inverse_4 = Inverse_3 * Inverse_1

        ! Adjoint of predictor summation across layers
        DO i = 1, n_Predictors
          xL_AD(i,k)   = xL_AD(i,k) + Pred_AD%X(i1+i-1,k)
          xL_AD(i,k-1) = Pred_AD%X(i1+i-1,k)
        END DO

        ! Adjoint of the LEVEL integrated predictors intermediate sums
        !
        ! Note that the adjoint variables Inverse_X_AD are local to this
        ! loop iteration so they are simply assigned when they are first
        ! used.
        !
        ! P* and T*, Predictor indices #2 and 1
        ! Simply assign a value for Inverse_1_AD
        Multiplier   = POINT_5 * Inverse_1
        s_AD(1)    = s_AD(1) + ( Multiplier * xL_AD(1,k) )
        s_AD(2)    = s_AD(2) + ( Multiplier * xL_AD(2,k) )
        Inverse_1_AD = POINT_5 * ( ( APV%s(1,k,j) * xL_AD(1,k) ) + &
                                   ( APV%s(2,k,j) * xL_AD(2,k) ) )
        ! P** and T**, Predictor indices #4 and 3
        Multiplier   = POINT_5 * Inverse_2
        s_AD(3)    = s_AD(3) + ( Multiplier * xL_AD(3,k) )
        s_AD(4)    = s_AD(4) + ( Multiplier * xL_AD(4,k) )
        Inverse_2_AD = POINT_5 * ( ( APV%s(3,k,j) * xL_AD(3,k) ) + &
                                   ( APV%s(4,k,j) * xL_AD(4,k) ) )
        ! P*** and T***, Predictor indices #6 and 5
        Multiplier   = POINT_75 * Inverse_3
        s_AD(5)    = s_AD(5) + ( Multiplier * xL_AD(5,k) )
        s_AD(6)    = s_AD(6) + ( Multiplier * xL_AD(6,k) )
        Inverse_3_AD = POINT_75 * ( ( APV%s(5,k,j) * xL_AD(5,k) ) + &
                                    ( APV%s(6,k,j) * xL_AD(6,k) ) )

        ! Adjoint of Inverse terms. Note that the Inverse_X_AD
        ! terms are *not* zeroed out as they are re-assigned values
        ! each loop iteration above.
        Pred_AD%A(k,j) = Pred_AD%A(k,j) - (Inverse_2 * Inverse_1_AD ) - &
                                          (TWO   * Inverse_3 * Inverse_2_AD ) - &
                                          (THREE * Inverse_4 * Inverse_3_AD )

        ! Pressure adjoint
        Atm_AD%Pressure(k) = Atm_AD%Pressure(k) + &
                             ( APV%d_A(k,j)      * s_AD(2) ) + &  ! P*
                             ( APV%Factor_1(k,j) * s_AD(4) ) + &  ! P**
                             ( APV%Factor_2(k,j) * s_AD(6) )      ! P***


        ! Temperature adjoint
        Atm_AD%Temperature(k) = Atm_AD%Temperature(k) + &
                                ( APV%d_A(k,j)      * s_AD(1) ) + &  ! T*
                                ( APV%Factor_1(k,j) * s_AD(3) ) + &  ! T**
                                ( APV%Factor_2(k,j) * s_AD(5) )      ! T***

        ! Adjoint of the absorber amount
        !
        ! Note that the adjoint variables Factor_X_AD and
        ! d_A_AD are local to this loop iteration
        ! so they are simply assigned when they are first
        ! used (and thus not zeroed out at the end of each
        ! iteration)
        !
        ! Note there are no
        !   s_AD() = 0
        ! because all the tangent-linear forms are
        !   s_TL() = s_TL() + (...)
        ! summing from the previous Layer.
        !
        ! Multiplicative factors
        Factor_1_AD = ( Atm%Temperature(k) * s_AD(3) ) + &
                      ( Atm%Pressure(k)    * s_AD(4) )

        Factor_2_AD = ( Atm%Temperature(k) * s_AD(5) ) + &
                      ( Atm%Pressure(k)    * s_AD(6) )

        ! Adjoint of the square integrated absorber amount.
        !
        ! Note that A_2_AD() is a LOCAL adjoint variable,
        ! so the initialisation of A_2_AD(k-1) here for
        ! each "k-1" is o.k. rather than
        !   A_2_AD(k-1) = A_2_AD(k-1) + ( d_A(k) * Factor_2_AD )
        !   A_2_AD( k ) = A_2_AD( k ) + ( d_A(k) * Factor_2_AD )
        ! since only A_2_AD( n_Layers ) is initialised outside the
        ! current layer loop.
        A_2_AD(k-1) = APV%d_A(k,j) * Factor_2_AD
        A_2_AD( k ) = A_2_AD( k ) + A_2_AD(k-1)

        ! Adjoint of A(). Here, since Pred_AD%A() is NOT a local adjoint
        ! variable, we can't use the same form as for A_2_AD() above.
        d_A_AD = ( Atm%Temperature(k) * s_AD(1) ) + &
                 ( Atm%Pressure(k)    * s_AD(2) ) + &
                 ( ( Pred%A(k,j)  + Pred%A(k-1,j)  ) * Factor_1_AD ) + &
                 ( ( APV%A_2(k,j) + APV%A_2(k-1,j) ) * Factor_2_AD )

        Add_Factor = APV%d_A(k,j) * Factor_1_AD
        Pred_AD%A(k-1,j) = Pred_AD%A(k-1,j) + Add_Factor - d_A_AD
        Pred_AD%A( k ,j) = Pred_AD%A( k ,j) + Add_Factor + d_A_AD + &
                           ( TWO * Pred%A(k,j) * A_2_AD(k) )
        A_2_AD(k) = ZERO

      END DO Layer_Loop

      ! Adjoint of level 0 A
      Pred_AD%A(0,j)   = Pred_AD%A(0,j) + ( TWO * Pred%A(0,j) * A_2_AD(0) )
      A_2_AD(0) = ZERO

    END DO Absorber_Loop
    
  END SUBROUTINE CRTM_Integrated_Predictors_AD


!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PUBLIC MODULE ROUTINES ##                        ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_Predictors
!
! PURPOSE:
!       Subroutine to calculate the gas absorption model predictors.
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_Predictors ( Atmosphere,   &  ! Input
!                                      GeometryInfo, &  ! Input
!                                      Predictor,    &  ! Output
!                                      APVariables   )  ! Internal variable output
!
! INPUT ARGUMENTS:
!       Atmosphere:     CRTM Atmosphere structure containing the atmospheric
!                       state data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Atmosphere_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       GeometryInfo:   CRTM_GeometryInfo structure containing the 
!                       view geometry information.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_GeometryInfo_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Predictor:      CRTM Predictor structure containing the integrated absorber
!                       and predictor profiles.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Predictor_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!       APVariables:    Structure containing internal variables required for
!                       subsequent tangent-linear or adjoint model calls.
!                       The contents of this structure are NOT accessible
!                       outside of the CRTM_Predictor module.
!                       UNITS:      N/A
!                       TYPE:       CRTM_APVariables_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
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
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_Predictors( Atmosphere,   &  ! Input
                                      GeometryInfo, &  ! Input
                                      Predictor,    &  ! Output
                                      APV           )  ! Internal variable output
    ! Arguments
    TYPE(CRTM_Atmosphere_type),   INTENT(IN)     :: Atmosphere
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    TYPE(CRTM_Predictor_type),    INTENT(IN OUT) :: Predictor
    TYPE(CRTM_APVariables_type),  INTENT(OUT)    :: APV
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_Predictors'
    ! Local variables
    INTEGER :: i1, i2, j

    ! Save the angle information
    Predictor%Secant_Sensor_Zenith = GeometryInfo%Secant_Sensor_Zenith

    ! Compute the nadir integrated absorber profiles
    CALL CRTM_Compute_IntAbsorber( Atmosphere, &  ! Input
                                   Predictor   )  ! Output

    ! Modify absorber quantities by the angle secant
    Predictor%A = Predictor%Secant_Sensor_Zenith * Predictor%A

    ! Compute the predictors
    !
    ! Standard predictors
    CALL CRTM_Standard_Predictors( Atmosphere, &
                                   Predictor   )
    ! Integrated predictors
    CALL CRTM_Integrated_Predictors( Atmosphere, &
                                     Predictor,  &
                                     APV         )

  END SUBROUTINE CRTM_Compute_Predictors


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_Predictors_TL
!
! PURPOSE:
!       Subroutine to calculate the gas absorption model tangent-linear
!       predictors.
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_Predictors_TL ( Atmosphere,    &  ! FWD Input
!                                         Predictor,     &  ! FWD Input
!                                         Atmosphere_TL, &  ! TL Input
!                                         GeometryInfo,  &  ! Input
!                                         Predictor_TL,  &  ! TL Output
!                                         APVariables    )  ! Internal variable input
!
! INPUT ARGUMENTS:
!       Atmosphere:        CRTM Atmosphere structure containing the atmospheric
!                          state data.
!                          UNITS:      N/A
!                          TYPE:       TYPE(CRTM_Atmosphere_type)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Predictor:         CRTM Predictor structure containing the integrated absorber
!                          and predictor profiles.
!                          UNITS:      N/A
!                          TYPE:       TYPE(CRTM_Predictor_type)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Atmosphere_TL:     CRTM Atmosphere structure containing the tangent-linear
!                          atmospheric state data, i.e. the perturbations.
!                          UNITS:      N/A
!                          TYPE:       TYPE(CRTM_Atmosphere_type)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       GeometryInfo:      CRTM_GeometryInfo structure containing the
!                          view geometry information.
!                          UNITS:      N/A
!                          TYPE:       TYPE(CRTM_GeometryInfo_type)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       APVariables:       Structure containing internal variables required for
!                          subsequent tangent-linear or adjoint model calls.
!                          The contents of this structure are NOT accessible
!                          outside of the CRTM_Predictor module.
!                          UNITS:      N/A
!                          TYPE:       CRTM_APVariables_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT)
!
! OUTPUT ARGUMENTS:
!       Predictor_TL:      CRTM Predictor structure containing the tangent-linear
!                          integrated absorber and predictor profiles.
!                          UNITS:      N/A
!                          TYPE:       TYPE(CRTM_Predictor_type)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN OUT)
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

  SUBROUTINE CRTM_Compute_Predictors_TL( Atmosphere,    &  ! FWD Input
                                         Predictor,     &  ! FWD Input
                                         Atmosphere_TL, &  ! TL Input
                                         GeometryInfo,  &  ! Input
                                         Predictor_TL,  &  ! TL Output
                                         APV            )  ! Internal variable input
    ! Arguments
    TYPE(CRTM_Atmosphere_type),   INTENT(IN)     :: Atmosphere
    TYPE(CRTM_Predictor_type),    INTENT(IN)     :: Predictor
    TYPE(CRTM_Atmosphere_type),   INTENT(IN)     :: Atmosphere_TL
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    TYPE(CRTM_Predictor_type),    INTENT(IN OUT) :: Predictor_TL
    TYPE(CRTM_APVariables_type),  INTENT(IN)     :: APV
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_Predictors_TL'
    ! Local variables
    INTEGER :: i1, i2, j

    ! Save the angle information
    Predictor_TL%Secant_Sensor_Zenith = GeometryInfo%Secant_Sensor_Zenith

    ! Compute the tangent-linear nadir integrated absorber profiles
    CALL CRTM_Compute_IntAbsorber_TL( Atmosphere   , &  ! Input
                                      Atmosphere_TL, &  ! Input
                                      Predictor_TL   )  ! Output

    ! Modify absorber quantities by the angle secant
    Predictor_TL%A = Predictor_TL%Secant_Sensor_Zenith * Predictor_TL%A

    ! Compute the predictors
    !
    ! Standard predictors
    CALL CRTM_Standard_Predictors_TL( Atmosphere   , &  ! Input
                                      Atmosphere_TL, &  ! Input
                                      Predictor_TL   )  ! Output
    ! Integrated predictors
    CALL CRTM_Integrated_Predictors_TL( Atmosphere   , &  ! Input
                                        Predictor    , &  ! Input
                                        Atmosphere_TL, &  ! Input
                                        Predictor_TL , &  ! Output
                                        APV            )  ! Internal variable input

  END SUBROUTINE CRTM_Compute_Predictors_TL


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_Predictors_AD
!
! PURPOSE:
!       Subroutine to calculate the adjoint gas absorption model predictors.
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_Predictors_AD ( Atmosphere,    &  ! FWD Input
!                                         Predictor,     &  ! FWD Input
!                                         Predictor_AD,  &  ! AD Input
!                                         GeometryInfo,  &  ! Input
!                                         Atmosphere_AD, &  ! AD Output
!                                         APVariables    )  ! Internal variable input
! INPUT ARGUMENTS:
!       Atmosphere:        CRTM Atmosphere structure containing the atmospheric
!                          state data.
!                          UNITS:      N/A
!                          TYPE:       TYPE(CRTM_Atmosphere_type)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Predictor:         CRTM Predictor structure containing the integrated absorber
!                          and predictor profiles.
!                          UNITS:      N/A
!                          TYPE:       TYPE(CRTM_Predictor_type)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Predictor_AD:      CRTM Predictor structure containing the adjoint
!                          integrated absorber and predictor profiles.
!                          UNITS:      N/A
!                          TYPE:       TYPE(CRTM_Predictor_type)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN OUT)
!
!       GeometryInfo:      CRTM_GeometryInfo structure containing the
!                          view geometry information.
!                          UNITS:      N/A
!                          TYPE:       TYPE(CRTM_GeometryInfo_type)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       APVariables:       Structure containing internal variables required for
!                          subsequent tangent-linear or adjoint model calls.
!                          The contents of this structure are NOT accessible
!                          outside of the CRTM_Predictor module.
!                          UNITS:      N/A
!                          TYPE:       CRTM_APVariables_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT)
!
! OUTPUT ARGUMENTS:
!       Atmosphere_AD:     CRTM Atmosphere structure containing the adjoint
!                          atmospheric state data, i.e. the Jacobians
!                          UNITS:      N/A
!                          TYPE:       TYPE(CRTM_Atmosphere_type)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN OUT)
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
!                                    water vapor    dry gases        ozone
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_Predictors_AD ( Atmosphere,    &  ! FWD Input
                                          Predictor,     &  ! FWD Input
                                          Predictor_AD,  &  ! AD Input
                                          GeometryInfo,  &  ! Input
                                          Atmosphere_AD, &  ! AD Output
                                          APV            )  ! Internal variable input
    ! Arguments
    TYPE(CRTM_Atmosphere_type),   INTENT(IN)     :: Atmosphere
    TYPE(CRTM_Predictor_type),    INTENT(IN)     :: Predictor
    TYPE(CRTM_Predictor_type),    INTENT(IN OUT) :: Predictor_AD
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    TYPE(CRTM_Atmosphere_type),   INTENT(IN OUT) :: Atmosphere_AD
    TYPE(CRTM_APVariables_type),  INTENT(IN)     :: APV
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_Predictors_AD'
    ! Local variables
    LOGICAL :: Compute_Standard
    INTEGER :: i1, i2, j

    ! Save the angle information
    Predictor_AD%Secant_Sensor_Zenith = GeometryInfo%Secant_Sensor_Zenith

    ! Calculate the predictor adjoints
    !
    ! Integrated predictors
    CALL CRTM_Integrated_Predictors_AD( Atmosphere   , &  ! Input
                                        Predictor    , &  ! Input
                                        Predictor_AD , &  ! In/Output
                                        Atmosphere_AD, &  ! Output
                                        APV            )  ! Internal variable input
    ! Standard predictors
    CALL CRTM_Standard_Predictors_AD( Atmosphere   , &  ! Input
                                      Predictor_AD , &  ! Input
                                      Atmosphere_AD  )  ! Output

    ! Modify absorber quantities by the angle secant
    Predictor_AD%A = Predictor_AD%Secant_Sensor_Zenith * Predictor_AD%A

    ! Compute the adjoint nadir integrated absorber profiles
    CALL CRTM_Compute_IntAbsorber_AD( Atmosphere   , &  ! Input
                                      Predictor_AD , &  ! Output
                                      Atmosphere_AD  )  ! Input

  END SUBROUTINE CRTM_Compute_Predictors_AD

END MODULE CRTM_Predictor
