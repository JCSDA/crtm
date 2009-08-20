!
! CRTM_AtmAbsorption
!
! Module containing routines to compute the optical depth profile
! due to gaseous absorption.
!
!
! CREATION HISTORY:
!       Modifed by:     Yong Han, NESDIS/STAR 25-June-2008
!                       yong.han@noaa.gov


MODULE CRTM_AtmAbsorption

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,                ONLY: fp
  USE Message_Handler,           ONLY: SUCCESS, FAILURE, Display_Message
  USE CRTM_Parameters,           ONLY: TAU_ODAS, TAU_ODPS, ZERO
  USE CRTM_Atmosphere_Define,    ONLY: CRTM_Atmosphere_type
  USE CRTM_TauCoeff,             ONLY: CRTM_TauCoeff_type, &
                                       TC
  USE CRTM_AtmAbsorption_Define, ONLY: CRTM_AtmAbsorption_type, &
                                       CRTM_Associated_AtmAbsorption, &  
                                       CRTM_Destroy_AtmAbsorption, &     
                                       CRTM_Allocate_AtmAbsorption, &    
                                       CRTM_Assign_AtmAbsorption      
  USE CRTM_GeometryInfo_Define,  ONLY: CRTM_GeometryInfo_type
  USE ODAS_AtmAbsorption,        ONLY: ODAS_Compute_AtmAbsorption    => Compute_AtmAbsorption,    &
                                       ODAS_Compute_AtmAbsorption_TL => Compute_AtmAbsorption_TL, &
                                       ODAS_Compute_AtmAbsorption_AD => Compute_AtmAbsorption_AD, &
                                       ODAS_AAVariables_type      => AAVariables_type                                       
  USE ODAS_Predictor,            ONLY: ODAS_Compute_Predictors    => Compute_Predictors,    &
                                       ODAS_Compute_Predictors_TL => Compute_Predictors_TL, &
                                       ODAS_Compute_Predictors_AD => Compute_Predictors_AD, &
                                       ODAS_Predictor_type        => Predictor_type,        &
                                       ODAS_Allocate_Predictor    => Allocate_Predictor,    &
                                       ODAS_APVariables_type      => APVariables_type,      &
                                       ODAS_Destroy_Predictor     => Destroy_Predictor,     &
                                       ODAS_MAX_N_PREDICTORS      => MAX_N_PREDICTORS,      &
                                       ODAS_MAX_N_ABSORBERS       => MAX_N_ABSORBERS

  USE ODPS_AtmAbsorption,        ONLY: ODPS_Compute_AtmAbsorption,    &
                                       ODPS_Compute_AtmAbsorption_TL, &
                                       ODPS_Compute_AtmAbsorption_AD, &
                                       ODPS_AAVariables_type,         &
                                       ODPS_Compute_Predictors,       &
                                       ODPS_Compute_Predictors_TL,    &
                                       ODPS_Compute_Predictors_AD,    &
                                       ALLOW_OPTRAN
                            
  USE ODPS_Predictor,            ONLY: ODPS_Predictor_type        => Predictor_type,        &
                                       ODPS_APVariables_type,                               &
                                       ODPS_Get_n_Components      => Get_n_Components ,     &
                                       ODPS_Get_max_n_Predicotrs  => Get_max_n_Predicotrs,  &
                                       ODPS_Get_n_Absorbers       => Get_n_Absorbers,       &
                                       ODPS_Destroy_Predictor     => Destroy_Predictor,     &
                                       ODPS_Allocate_Predictor    => Allocate_Predictor,    &
                                       ODPS_Destroy_PAFV          => Destroy_PAFV,          &
                                       ODPS_Allocate_PAFV         => Allocate_PAFV,         &
                                       ODPS_Get_SaveFWVFlag       => Get_SaveFWVFlag 

                                                                              
  ! Disable implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! CRTM_AtmAbsorption structure data type
  ! in the CRTM_AtmAbsorption_Define module
  PUBLIC :: CRTM_AtmAbsorption_type
  ! CRTM_AtmAbsorption structure routines inherited
  ! from the CRTM_AtmAbsorption_Define module
  PUBLIC :: CRTM_Associated_AtmAbsorption
  PUBLIC :: CRTM_Destroy_AtmAbsorption
  PUBLIC :: CRTM_Allocate_AtmAbsorption
  PUBLIC :: CRTM_Assign_AtmAbsorption
  ! routines in this modules
  PUBLIC :: CRTM_Compute_AtmAbsorption
  PUBLIC :: CRTM_Compute_AtmAbsorption_TL
  PUBLIC :: CRTM_Compute_AtmAbsorption_AD
  PUBLIC :: CRTM_Compute_Predictors
  PUBLIC :: CRTM_Compute_Predictors_TL
  PUBLIC :: CRTM_Compute_Predictors_AD
  PUBLIC :: CRTM_Destroy_Predictor
  PUBLIC :: CRTM_Allocate_Predictor
  ! CRTM Predictor structure type
  PUBLIC :: CRTM_Predictor_type
  ! Internal variable structure
  PUBLIC :: CRTM_AAVariables_type
  PUBLIC :: CRTM_APVariables_type

  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id$'

  ! Message string length
  INTEGER, PARAMETER :: ML = 256

  ! ---------------------
  ! Structure definitions
  ! ---------------------
  ! Predictor container structure definition
  TYPE :: CRTM_Predictor_type
    PRIVATE
    TYPE(ODAS_Predictor_type)   :: ODAS
    TYPE(ODPS_Predictor_type)   :: ODPS
  END TYPE CRTM_Predictor_type

  ! Structure to hold AtmAbsorption
  ! forward model variables across
  ! FWD, TL, and AD calls
  TYPE :: CRTM_AAVariables_type
    PRIVATE
    TYPE(ODAS_AAVariables_type)   :: ODAS
    TYPE(ODPS_AAVariables_type)   :: ODPS
  END TYPE CRTM_AAVariables_type

  ! Structure to hold Predictor
  ! forward model variables across
  ! FWD, TL, and AD calls
  TYPE :: CRTM_APVariables_type
    PRIVATE
    TYPE(ODAS_APVariables_type)   :: ODAS
    TYPE(ODPS_APVariables_type)   :: ODPS
  END TYPE CRTM_APVariables_type

  
CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_AtmAbsorption
!
! PURPOSE:
!       Subroutine to calculate the layer optical depths due to gaseous
!       absorption for a given sensor and channel and atmospheric profile.
!       It is a wrapper which calls the algorithm specific routine.
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_AtmAbsorption( SensorIndex  , &  ! Input
!                                        ChannelIndex , &  ! Input
!                                        Predictor    , &  ! Input
!                                        AtmAbsorption, &  ! Output
!                                        AAVariables    )  ! Internal variable output
!
! INPUT ARGUMENTS:
!       SensorIndex:     Sensor index id. This is a unique index associated
!                        with a (supported) sensor used to access the
!                        shared coefficient data for a particular sensor.
!                        See the ChannelIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:    Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data for a particular sensor's
!                        channel.
!                        See the SensorIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor:       Structure containing the integrated absorber and
!                        predictor profile data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Predictor_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!        AtmAbsorption:  Structure containing computed optical depth
!                        profile data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_AtmAbsorption_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!       AAVariables:     Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the CRTM_AtmAbsorption module.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_AAVariables_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT)
!
! COMMENTS:
!       Note the INTENT on the structure arguments are IN OUT rather
!       than just OUT. This is necessary because the argument is defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_AtmAbsorption( SensorIndex  ,   &  ! Input
                                         ChannelIndex ,   &  ! Input                       
                                         Predictor    ,   &  ! Input                       
                                         AtmAbsorption,   &  ! Output                      
                                         AAV            )    ! Internal variable output    
    ! Arguments
    INTEGER                      , INTENT(IN)     :: SensorIndex
    INTEGER                      , INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_Predictor_type)    , INTENT(IN OUT) :: Predictor
    TYPE(CRTM_AtmAbsorption_type), INTENT(IN OUT) :: AtmAbsorption
    TYPE(CRTM_AAVariables_type)  , INTENT(IN OUT) :: AAV

    ! Local
    CHARACTER(ML) :: Message

    SELECT CASE( TC%Algorithm_ID(SensorIndex) )
      CASE( TAU_ODAS )
          CALL ODAS_Compute_AtmAbsorption( &
                    TC%Sensor_LoIndex(SensorIndex), &  ! Input                             
                    ChannelIndex ,                  &  ! Input                                          
                    Predictor%ODAS,                 &  ! Input                                          
                    AtmAbsorption,                  &  ! Output                                         
                    AAV%ODAS     )                     ! Internal variable output      
      CASE( TAU_ODPS )
          CALL ODPS_Compute_AtmAbsorption( &
                    TC%Sensor_LoIndex(SensorIndex), &  ! Input                             
                    ChannelIndex ,                  &  ! Input 
                    Predictor%ODPS,                 &  ! Input                                          
                    AtmAbsorption)                     ! Output                                         
    END SELECT

  END SUBROUTINE CRTM_Compute_AtmAbsorption               

!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_AtmAbsorption_TL
!
! PURPOSE:
!       Subroutine to calculate the tangent-linear layer optical depths due
!       to gaseous absorption for a given sensor and channel and atmospheric
!       profile. It is a wrapper which calls the algorithm specific routine.
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_AtmAbsorption_TL( SensorIndex     , &  ! Input
!                                           ChannelIndex    , &  ! Input
!                                           Predictor       , &  ! FWD Input
!                                           Predictor_TL    , &  ! TL Input
!                                           AtmAbsorption_TL, &  ! TL Output
!                                           AAVariables       )  ! Internal variable input
!
! INPUT ARGUMENTS:
!       SensorIndex:        Sensor index id. This is a unique index associated
!                           with a (supported) sensor used to access the
!                           shared coefficient data for a particular sensor.
!                           See the ChannelIndex argument.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:       Channel index id. This is a unique index associated
!                           with a (supported) sensor channel used to access the
!                           shared coefficient data for a particular sensor's
!                           channel.
!                           See the SensorIndex argument.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Predictor:          Structure containing the integrated absorber and
!                           predictor profile data.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_Predictor_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Predictor_TL:       Structure containing the tangent-linear integrated
!                           absorber and predictor profile data.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_Predictor_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       AAVariables:        Structure containing internal variables required for
!                           subsequent tangent-linear or adjoint model calls.
!                           The contents of this structure are NOT accessible
!                           outside of the CRTM_AtmAbsorption module.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_AAVariables_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT)
!
! OUTPUT ARGUMENTS:
!        AtmAbsorption_TL:  Structure containing the computed tangent-linear
!                           optical depth profile data.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_AtmAbsorption_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the AtmAbsorption_TL argument is IN OUT rather
!       than just OUT. This is necessary because the argument is defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_AtmAbsorption_TL( SensorIndex     , &  ! Input
                                            ChannelIndex    , &  ! Input
                                            Predictor       , &  ! Input
                                            Predictor_TL    , &  ! Input
                                            AtmAbsorption_TL, &  ! Output
                                            AAV               )  ! Internal variable input
    ! Arguments
    INTEGER                      , INTENT(IN)     :: SensorIndex
    INTEGER                      , INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_Predictor_type)    , INTENT(IN)     :: Predictor
    TYPE(CRTM_Predictor_type)    , INTENT(IN OUT) :: Predictor_TL
    TYPE(CRTM_AtmAbsorption_type), INTENT(IN OUT) :: AtmAbsorption_TL
    TYPE(CRTM_AAVariables_type)  , INTENT(IN OUT) :: AAV

    ! Local
    CHARACTER(ML) :: Message

    SELECT CASE( TC%Algorithm_ID(SensorIndex) )
      CASE( TAU_ODAS )
          CALL ODAS_Compute_AtmAbsorption_TL( &
                    TC%Sensor_LoIndex(SensorIndex), &  ! Input                             
                    ChannelIndex ,                  &  ! Input                                          
                    Predictor%ODAS,                 &  ! Input
                    Predictor_TL%ODAS,              &  ! Input                                          
                    AtmAbsorption_TL,               &  ! Output                                         
                    AAV%ODAS     )                     ! Internal variable output      

      CASE( TAU_ODPS )
          CALL ODPS_Compute_AtmAbsorption_TL( &
                    TC%Sensor_LoIndex(SensorIndex), &  ! Input
                    ChannelIndex ,                  &  ! Input
                    Predictor%ODPS,                 &  ! Input
                    Predictor_TL%ODPS,              &  ! Input
                    AtmAbsorption_TL)                  ! Output

    END SELECT

  END SUBROUTINE CRTM_Compute_AtmAbsorption_TL               

!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_AtmAbsorption_AD
!
! PURPOSE:
!       Subroutine to calculate the layer optical depth adjoints due to
!       gaseous absorption for a given sensor and channel and atmospheric
!       profile. It is a wrapper which calls the algorithm specific routine.
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_AtmAbsorption_AD( SensorIndex     , &  ! Input
!                                           ChannelIndex    , &  ! Input
!                                           Predictor       , &  ! FWD Input
!                                           AtmAbsorption_AD, &  ! TL  Input
!                                           Predictor_AD    , &  ! TL  Output
!                                           AAVariables       )  ! Internal variable input
!
! INPUT ARGUMENTS:
!       SensorIndex:        Sensor index id. This is a unique index associated
!                           with a (supported) sensor used to access the
!                           shared coefficient data for a particular sensor.
!                           See the ChannelIndex argument.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:       Channel index id. This is a unique index associated
!                           with a (supported) sensor channel used to access the
!                           shared coefficient data for a particular sensor's
!                           channel.
!                           See the SensorIndex argument.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Predictor:          Structure containing the integrated absorber and
!                           predictor profile data.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_Predictor_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       AtmAbsorption_AD:   Structure containing the computed adjoint
!                           optical depth profile data.
!                           Set to zero upon output.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_AtmAbsorption_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
!       AAVariables:        Structure containing internal variables required for
!                           subsequent tangent-linear or adjoint model calls.
!                           The contents of this structure are NOT accessible
!                           outside of the CRTM_AtmAbsorption module.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_AAVariables_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT)
!
! OUTPUT ARGUMENTS:
!       Predictor_AD:       Structure containing the adjoint integrated
!                           absorber and predictor profile data.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_Predictor_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
! SIDE EFFECTS:
!       Components of the AtmAbsorption_AD structure argument are modified
!       in this function.
!
!------------------------------------------------------------------------------
  SUBROUTINE CRTM_Compute_AtmAbsorption_AD( SensorIndex     , &  ! Input
                                            ChannelIndex    , &  ! Input
                                            Predictor       , &  ! FWD Input
                                            AtmAbsorption_AD, &  ! AD  Input
                                            Predictor_AD    , &  ! AD  Output
                                            AAV               )  ! Internal variable input
    ! Arguments
    INTEGER,                       INTENT(IN)     :: SensorIndex
    INTEGER,                       INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_Predictor_type),     INTENT(IN)     :: Predictor
    TYPE(CRTM_AtmAbsorption_type), INTENT(IN OUT) :: AtmAbsorption_AD
    TYPE(CRTM_Predictor_type),     INTENT(IN OUT) :: Predictor_AD
    TYPE(CRTM_AAVariables_type)  , INTENT(IN OUT) :: AAV

    ! Local
    CHARACTER(ML) :: Message

    SELECT CASE( TC%Algorithm_ID(SensorIndex) )
      CASE( TAU_ODAS )
          CALL ODAS_Compute_AtmAbsorption_AD( &
                    TC%Sensor_LoIndex(SensorIndex), &  ! Input                             
                    ChannelIndex ,                  &  ! Input                                          
                    Predictor%ODAS,                 &  ! FWD Input
                    AtmAbsorption_AD,               &  ! AD Input                                         
                    Predictor_AD%ODAS,              &  ! AD Output                         
                    AAV%ODAS     )                     ! Internal variable output      

      CASE( TAU_ODPS )
          CALL ODPS_Compute_AtmAbsorption_AD( &
                    TC%Sensor_LoIndex(SensorIndex), &  ! Input
                    ChannelIndex ,                  &  ! Input
                    Predictor%ODPS,                 &  ! FWD Input
                    AtmAbsorption_AD,               &  ! AD Input
                    Predictor_AD%ODPS)                 ! AD Output

    END SELECT

  END SUBROUTINE CRTM_Compute_AtmAbsorption_AD              

!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_Predictors
!
! PURPOSE:
!       Subroutine to calculate the gas absorption model predictors.
!       It is a wrapper which calls the algorithm specific routine.
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_Predictors ( SensorIndex,  &  ! Input
!                                      Atmosphere,   &  ! Input
!                                      GeometryInfo, &  ! Input
!                                      Predictor,    &  ! Output
!                                      APVariables   )  ! Internal variable output
!
! INPUT ARGUMENTS:
!       SensorIndex:     Sensor index id. This is a unique index associated
!                        with a (supported) sensor used to access the
!                        shared coefficient data for a particular sensor.
!                        See the ChannelIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
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
!                       TYPE:       TYPE(CRTM_APVariables_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! COMMENTS:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_Predictors(SensorIndex,  &  ! Input
                                     Atmosphere,   &  ! Input                                            
                                     GeometryInfo, &  ! Input                                         
                                     Predictor,    &  ! Output 
                                     APV       )      ! Internal variable output
    ! Arguments
    INTEGER,                      INTENT(IN)     :: SensorIndex
    TYPE(CRTM_Atmosphere_type),   INTENT(IN)     :: Atmosphere
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    TYPE(CRTM_Predictor_type),    INTENT(IN OUT) :: Predictor
    TYPE(CRTM_APVariables_type),  INTENT(IN OUT) :: APV
    ! Local
    INTEGER :: SLoIndex

    SLoIndex = TC%Sensor_LoIndex(SensorIndex)  
    SELECT CASE( TC%Algorithm_ID(SensorIndex) )
      CASE( TAU_ODAS )
         CALL ODAS_Compute_Predictors(Atmosphere,                  &  ! Input
                                      GeometryInfo,                &  ! Input        
                                      TC%ODAS(SLoIndex)%Max_Order, &  ! Input
                                      TC%ODAS(SLoIndex)%Alpha,     &  ! Input
                                      Predictor%ODAS,              &  ! Output
                                      APV%ODAS   )                  

      CASE( TAU_ODPS )
         CALL ODPS_Compute_Predictors(SLoIndex,          &  ! Input
                                      Atmosphere,        &  ! Input
                                      GeometryInfo,      &  ! Input        
                                      Predictor%ODPS)       ! Output  
              
    END SELECT

  END SUBROUTINE CRTM_Compute_Predictors

!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_Predictors_TL
!
! PURPOSE:
!       Subroutine to calculate the gas absorption model tangent-linear
!       predictors. It is a wrapper which calls the algorithm specific routine.
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_Predictors_TL ( SensorIndex,   &  ! Input
!                                         Atmosphere,    &  ! FWD Input
!                                         Predictor,     &  ! FWD Input
!                                         Atmosphere_TL, &  ! TL Input
!                                         GeometryInfo,  &  ! Input
!                                         Predictor_TL,  &  ! TL Output
!                                         APVariables    )  ! Internal variable input
!
! INPUT ARGUMENTS:
!       SensorIndex:       Sensor index id. This is a unique index associated
!                          with a (supported) sensor used to access the
!                          shared coefficient data for a particular sensor.
!                          See the ChannelIndex argument.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
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
!S-
!--------------------------------------------------------------------------------
  SUBROUTINE CRTM_Compute_Predictors_TL( SensorIndex,   &  ! Input
                                         Atmosphere,    &  ! FWD Input
                                         Predictor,     &  ! FWD Input
                                         Atmosphere_TL, &  ! TL Input
                                         GeometryInfo,  &  ! Input
                                         Predictor_TL,  &  ! TL Output
                                         APV            )  ! Internal variable input
    ! Arguments
    INTEGER,                      INTENT(IN)     :: SensorIndex
    TYPE(CRTM_Atmosphere_type),   INTENT(IN)     :: Atmosphere
    TYPE(CRTM_Predictor_type),    INTENT(IN)     :: Predictor
    TYPE(CRTM_Atmosphere_type),   INTENT(IN)     :: Atmosphere_TL
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    TYPE(CRTM_Predictor_type),    INTENT(IN OUT) :: Predictor_TL
    TYPE(CRTM_APVariables_type),  INTENT(IN OUT) :: APV
    ! Local
    INTEGER :: SLoIndex

    SLoIndex = TC%Sensor_LoIndex(SensorIndex)  
    SELECT CASE( TC%Algorithm_ID(SensorIndex) )
      CASE( TAU_ODAS )
         CALL ODAS_Compute_Predictors_TL(Atmosphere,                  &  ! FWD Input
                                         Predictor%ODAS,              &  ! FWD Input
                                         Atmosphere_TL,               &  ! TL Input
                                         GeometryInfo,                &  ! Input    
                                         TC%ODAS(SLoIndex)%Max_Order, &  ! Input
                                         TC%ODAS(SLoIndex)%Alpha,     &  ! Input
                                         Predictor_TL%ODAS,           &  ! TL Output
                                         APV%ODAS   )                  

      CASE( TAU_ODPS )
         CALL ODPS_Compute_Predictors_TL(SLoIndex,           &  ! Input
                                         Atmosphere,         &  ! FWD Input
                                         GeometryInfo,       &  ! Input
                                         Predictor%ODPS,     &  ! FWD Input
                                         Atmosphere_TL,      &  ! TL Input
                                         Predictor_TL%ODPS)     ! TL Output
              
    END SELECT

  END SUBROUTINE CRTM_Compute_Predictors_TL

!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_Predictors_AD
!
! PURPOSE:
!       Subroutine to calculate the adjoint gas absorption model predictors.
!       It is a wrapper which calls the algorithm specific routine.
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_Predictors_AD ( SensorIndex,   &  ! Input
!                                         Atmosphere,    &  ! FWD Input
!                                         Predictor,     &  ! FWD Input
!                                         Predictor_AD,  &  ! AD Input
!                                         GeometryInfo,  &  ! Input
!                                         Atmosphere_AD, &  ! AD Output
!                                         APVariables    )  ! Internal variable input
! INPUT ARGUMENTS:
!       SensorIndex:       Sensor index id. This is a unique index associated
!                          with a (supported) sensor used to access the
!                          shared coefficient data for a particular sensor.
!                          See the ChannelIndex argument.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
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
!                          **NOTE: This structure is zeroed upon output
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
!S-
!--------------------------------------------------------------------------------
  SUBROUTINE CRTM_Compute_Predictors_AD ( SensorIndex,   &  ! Input
                                          Atmosphere,    &  ! FWD Input
                                          Predictor,     &  ! FWD Input
                                          Predictor_AD,  &  ! AD Input
                                          GeometryInfo,  &  ! Input
                                          Atmosphere_AD, &  ! AD Output
                                          APV            )  ! Internal variable input
    ! Arguments
    INTEGER,                      INTENT(IN)     :: SensorIndex
    TYPE(CRTM_Atmosphere_type),   INTENT(IN)     :: Atmosphere
    TYPE(CRTM_Predictor_type),    INTENT(IN)     :: Predictor
    TYPE(CRTM_Predictor_type),    INTENT(IN OUT) :: Predictor_AD
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    TYPE(CRTM_Atmosphere_type),   INTENT(IN OUT) :: Atmosphere_AD
    TYPE(CRTM_APVariables_type),  INTENT(IN OUT) :: APV
    ! Local
    INTEGER :: SLoIndex

    SLoIndex = TC%Sensor_LoIndex(SensorIndex)  
    SELECT CASE( TC%Algorithm_ID(SensorIndex) )
      CASE( TAU_ODAS )
         CALL ODAS_Compute_Predictors_AD(Atmosphere,                  &  ! FWD Input
                                         Predictor%ODAS,              &  ! FWD Input
                                         Predictor_AD%ODAS,           &  ! AD Intput
                                         GeometryInfo,                &  ! Input    
                                         TC%ODAS(SLoIndex)%Max_Order, &  ! Input
                                         TC%ODAS(SLoIndex)%Alpha,     &  ! Input
                                         Atmosphere_AD,               &  ! AD Output
                                         APV%ODAS   )

      CASE( TAU_ODPS )
         CALL ODPS_Compute_Predictors_AD(SLoIndex,            &  ! Input
                                         Atmosphere,          &  ! FWD Input
                                         GeometryInfo,        &  ! Input     
                                         Predictor%ODPS,      &  ! FWD Input
                                         Predictor_AD%ODPS,   &  ! AD Intput
                                         Atmosphere_AD)          ! AD Output
              
    END SELECT

  END SUBROUTINE CRTM_Compute_Predictors_AD

!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Destroy_Predictor
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of
!       a CRTM_Predictor data structure.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_Predictor( SensorIndex            , &  ! Input
!                                              Predictor              , &  ! Output
!                                              RCS_Id     =RCS_Id     , &  ! Revision control
!                                              Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       SensorIndex:     Sensor index id. This is a unique index associated
!                        with a (supported) sensor used to access the
!                        shared coefficient data for a particular sensor.
!                        See the ChannelIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Predictor:      Re-initialized CRTM_Predictor structure.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Predictor_type
!                       DIMENSION:  Scalar OR Rank-1 array
!                       ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:         Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the structure re-initialisation was successful
!                          == FAILURE - an error occurred, or
!                                     - the structure internal allocation counter
!                                       is not equal to zero (0) upon exiting this
!                                       function. This value is incremented and
!                                       decremented for every structure allocation
!                                       and deallocation respectively.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output Predictor argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Destroy_Predictor( SensorIndex, &  ! Input            
                                   Predictor  , &  ! Output           
                                   No_Clear   , &  ! Optional input   
                                   RCS_Id     , &  ! Revision control 
                                   Message_Log) &  ! Error messaging  
                                 RESULT(Error_Status)                 
    ! Arguments
    INTEGER,                   INTENT(IN)     :: SensorIndex
    TYPE(CRTM_Predictor_type), INTENT(IN OUT) :: Predictor 
    INTEGER,      OPTIONAL   , INTENT(IN)     :: No_Clear
    CHARACTER(*), OPTIONAL   , INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL   , INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Predictor'
    ! Local variables
    CHARACTER(ML) :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status
    
    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID
    
    Error_Status = SUCCESS

    ! ----------------------------------------------
    ! Destroy TauCoeff structure
    ! ----------------------------------------------   
    SELECT CASE( TC%Algorithm_ID( SensorIndex ) )                            
      CASE ( TAU_ODAS )                                                       

        Error_Status = ODAS_Destroy_Predictor( Predictor%ODAS,   &
                                         No_Clear    = No_Clear, &
                                         RCS_Id      =  RCS_Id,  &           
                                         Message_Log = Message_Log )          
      CASE ( TAU_ODPS )                                                       

        Error_Status = ODPS_Destroy_Predictor( Predictor%ODPS,   &
                                         No_Clear    = No_Clear, &
                                         RCS_Id      =  RCS_Id,  &           
                                         Message_Log = Message_Log )
        IF( Error_Status == SUCCESS )THEN
          Error_Status = ODPS_Destroy_PAFV(& 
                         Predictor%ODPS%PAFV,    &                    
                         RCS_Id      =  RCS_Id,  &                           
                         Message_Log = Message_Log )
        END IF
                                                              
    END SELECT                                                                

    IF ( Error_Status /= SUCCESS ) THEN                                       
      WRITE( Message, '( "Error deallocating Predictor structure member" )')  
      CALL Display_Message( ROUTINE_NAME,    &                                
                            TRIM( Message ), &                                
                            Error_Status,    &                                
                            Message_Log = Message_Log )                       
    END IF                                                                    

  END FUNCTION CRTM_Destroy_Predictor

!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Allocate_Predictor
! 
! PURPOSE:
!       Function to allocate the pointer members of the CRTM_Predictor
!       data structure.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Allocate_Predictor( SensorIndex            , &  ! Input
!                                               n_Layers               , &  ! Input
!                                               Predictor              , &  ! Output
!                                               RCS_Id     =RCS_Id     , &  ! Revision control
!                                               Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       SensorIndex:         Sensor index id. This is a unique index associated
!                            with a (supported) sensor used to access the
!                            shared coefficient data for a particular sensor.
!                            See the ChannelIndex argument.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)

!         n_Layers:          Number of atmospheric layers.
!                            Must be > 0
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:         Character string specifying a filename in which any
!                            messages will be logged. If not specified, or if an
!                            error occurs opening the log file, the default action
!                            is to output messages to standard output.
!                            UNITS:      N/A
!                            TYPE:       CHARACTER(*)
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Predictor:           CRTM_Predictor structure with allocated pointer members
!                            UNITS:      N/A
!                            TYPE:       CRTM_Predictor_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN OUT)
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       SaveFWV:             Flag indicating the predictor allocation is for FW calculation.
!                            This flag may be used for some algorithms for additional memory 
!                            allocation to save FW variables for TL and AD calculations.
!                            UNITS:      N/A
!                            TYPE:       Integer
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN), OPTIONAL
! 
!       RCS_Id:              Character string containing the Revision Control
!                            System Id field for the module.
!                            UNITS:      N/A
!                            TYPE:       CHARACTER(*)
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:        The return value is an integer defining the error status.
!                            The error codes are defined in the Message_Handler module.
!                            If == SUCCESS the structure re-initialisation was successful
!                               == FAILURE - an error occurred, or
!                                          - the structure internal allocation counter
!                                            is not equal to one (1) upon exiting this
!                                            function. This value is incremented and
!                                            decremented for every structure allocation
!                                            and deallocation respectively.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output Predictor argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Allocate_Predictor( SensorIndex  , &  ! Input
                                    n_Layers     , &  ! Input            
                                    GeometryInfo,  &  ! Input
                                    Predictor    , &  ! Output 
                                    SaveFWV      , &  ! Optional Input          
                                    RCS_Id       , &  ! Revision control 
                                    Message_Log  ) &  ! Error messaging  
                                  RESULT( Error_Status )                 
    ! Arguments
    INTEGER                     , INTENT(IN)  :: SensorIndex    
    INTEGER                     , INTENT(IN)  :: n_Layers       
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)  :: GeometryInfo   
    TYPE(CRTM_Predictor_type), INTENT(IN OUT) :: Predictor
    INTEGER,      OPTIONAL   , INTENT(IN)     :: SaveFWV
    CHARACTER(*), OPTIONAL   , INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL   , INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Predictor'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Allocate_Status
    INTEGER :: i, SLoIndex
    LOGICAL :: Calc_Sun_Angle_Secant
    
    Error_Status=SUCCESS

    SLoIndex = TC%Sensor_LoIndex(SensorIndex)
    SELECT CASE( TC%Algorithm_ID(SensorIndex) )
      CASE( TAU_ODAS )
         Allocate_Status = ODAS_Allocate_Predictor( &
                               n_Layers ,                   &  ! Input
                               ODAS_MAX_N_PREDICTORS,       &  ! Input
                               ODAS_MAX_N_ABSORBERS,        &  ! Input
                               MAXVAL(TC%ODAS(SLoIndex)%Max_Order), &  ! Input
                               Predictor%ODAS   ,           &  ! Output
                               RCS_Id = RCS_Id,             &  ! Revision control               
                               Message_Log=Message_Log  )      ! Error messaging 
      CASE( TAU_ODPS )
         i = TC%ODPS(SLoIndex)%Group_Index
         IF(TC%ODPS(SLoIndex)%n_OCoeffs > 0 .AND. ALLOW_OPTRAN)THEN  
           Predictor%ODPS%OPTRAN = .TRUE.                      
         END IF                                                              
         Allocate_Status = ODPS_Allocate_Predictor( &
                               TC%ODPS(SLoIndex)%n_Layers,  & ! input  - n internal layers
                               ODPS_Get_n_Components(i),    & ! Input
                               ODPS_Get_max_n_Predicotrs(i),& ! Input                              
                               Predictor%ODPS   ,           & ! Output            
                               OPTRAN = Predictor%ODPS%OPTRAN, & ! Input, OPTRAN flag         
                               n_User_Layers = n_Layers ,   & ! Input - n user layers
                               RCS_Id = RCS_Id,             & ! Revision control                
                               Message_Log=Message_Log  )     ! Error messaging 
         ! Allocate memory for FW variables, whose values will be saved in the FW routines and
         ! used in the TL and AD routines
         IF(PRESENT(SaveFWV) .AND. ODPS_Get_SaveFWVFlag(i))THEN
           Allocate_Status = ODPS_Allocate_PAFV( &
                                          TC%ODPS(SLoIndex)%n_Layers, & ! Input
                                          ODPS_Get_n_Absorbers(i),    & ! Input
                                          n_Layers,                   & ! Input
                                          Predictor%ODPS%OPTRAN,      & ! Input
                                          Predictor%ODPS%PAFV,        & ! Output
                                          RCS_Id = RCS_Id,            & ! Revision control     
                                          Message_Log=Message_Log  )    ! Error messaging 
         END IF
    END SELECT

    IF ( Allocate_Status /= SUCCESS ) THEN                                             
      Error_Status=FAILURE                                                              
      WRITE( Message,'("Error allocating Predictor_ODAS for the sensor index",i0)' ) SensorIndex  
      CALL Display_Message( ROUTINE_NAME, &                                             
                            TRIM(Message), &                                            
                            Error_Status, &                                             
                            Message_Log=Message_Log )                                   
      RETURN                                                                            
    END IF                                                                              

  END FUNCTION CRTM_Allocate_Predictor

END MODULE CRTM_AtmAbsorption

