!
! ODSSU_AtmAbsorption
!
! Module containing routines to compute the optical depth profile for SSUs
!
!
! CREATION HISTORY:
!       Based on CRTM_AtmAbsorption_ssu     by:  Quanhua Liu, JCSDA,      Dec  1, 2007
!       Rewritten for CRTMv2.0              by:  Yong Han, NOAA/NESDIS,   Oct  6, 2009
!       Revised                             by:  Paul van Delst, , JCSDA, Oct 26, 2009
!       Revised                             by:  Quanhua Liu, JCSDA,      Oct 29, 2009
!

MODULE ODSSU_AtmAbsorption

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,                ONLY: fp
  USE Message_Handler,           ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE CRTM_Parameters,           ONLY: ZERO, ONE
  USE CRTM_GeometryInfo_Define,  ONLY: CRTM_GeometryInfo_type 
  USE CRTM_AtmAbsorption_Define, ONLY: CRTM_AtmAbsorption_type
  USE ODAS_AtmAbsorption,        ONLY: ODx_AAVariables_type         => AAVariables_type,         &
                                       ODx_Compute_AtmAbsorption    => Compute_AtmAbsorption,    &
                                       ODx_Compute_AtmAbsorption_TL => Compute_AtmAbsorption_TL, &
                                       ODx_Compute_AtmAbsorption_AD => Compute_AtmAbsorption_AD
  USE ODAS_Predictor,            ONLY: Predictor_type
  USE ODSSU_TauCoeff,            ONLY: TC

  USE SSU_Input_Define, ONLY: SSU_Input_type, &
                              SSU_Input_Get_Property, &
                              SSU_Input_Cell_Pressure_Set
  USE Search_Utility, ONLY :  Bisection_Search
  ! Disable implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! CRTM_AtmAbsorption structure data type
  ! in the CRTM_AtmAbsorption_Define module
  PUBLIC :: AAVariables_type
  PUBLIC :: Compute_AtmAbsorption
  PUBLIC :: Compute_AtmAbsorption_TL
  PUBLIC :: Compute_AtmAbsorption_AD

  
  ! ----------
  ! Parameters
  ! ----------
  ! RCS Id for the module
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id$'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  
  
  ! ------------------------------------------
  ! Structure definition to hold forward model
  ! variables across FWD, TL, and AD calls
  ! ------------------------------------------
  TYPE :: AAVariables_type
    PRIVATE
    TYPE(ODx_AAVariables_type) :: ODx(2)
    REAL(fp) :: Weight(2) = ZERO
    REAL(fp) :: CO2_Cell = ZERO
    INTEGER  :: Index_low = 1
  END TYPE AAVariables_type


  
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
!       Compute_AtmAbsorption
!
! PURPOSE:
!       Subroutine to calculate the layer optical depths due to gaseous
!       absorption for a given sensor and channel and atmospheric profile.
!
! CALLING SEQUENCE:
!       CALL Compute_AtmAbsorption( GeometryInfo , &  ! Inputq
!                                   SensorIndex  , &  ! Input
!                                   ChannelIndex , &  ! Input                        
!                                   Predictor    , &  ! Input                        
!                                   AtmAbsorption, &  ! Output                       
!                                   AAVariables    )  ! Internal variable output     
!
! INPUT ARGUMENTS:
!       GeometryInfo:    Structure containing the view geometry
!                        information.
!                        UNITS:      N/A
!                        TYPE:       CRTM_GeometryInfo_type
!                        DIMENSION:  Same as input Atmosphere structure
!                        ATTRIBUTES: INTENT(IN)
!
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
!                        TYPE:       TYPE(Predictor_type)
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
!                        TYPE:       TYPE(AAVariables_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT)
!
! COMMENTS:
!       Note the INTENT on the structure arguments are IN OUT rather
!       than just OUT. This is necessary because the argument is defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  SUBROUTINE Compute_AtmAbsorption( SSU_Input    , &  ! Input
                                    SensorIndex  , &  ! Input
                                    ChannelIndex , &  ! Input                        
                                    Predictor    , &  ! Input                        
                                    AtmAbsorption, &  ! Output                       
                                    AAV            )  ! Internal variable output     
    ! Arguments
    TYPE(SSU_Input_type)         , INTENT(IN)     :: SSU_Input
    INTEGER                      , INTENT(IN)     :: SensorIndex
    INTEGER                      , INTENT(IN)     :: ChannelIndex
    TYPE(Predictor_type)         , INTENT(IN OUT) :: Predictor
    TYPE(CRTM_AtmAbsorption_type), INTENT(IN OUT) :: AtmAbsorption
    TYPE(AAVariables_type)       , INTENT(OUT)    :: AAV
    ! Parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_AtmAbsorption (ODSSU)'
    ! Variables
    CHARACTER(ML) :: msg
    REAL(fp) :: Optical_Depth(Predictor%n_Layers)
    REAL(fp) :: Time, Cell_Pressure

    ! Compute the CO2 cell pressure
    IF( SSU_Input_Cell_Pressure_Set(SSU_Input) ) THEN
      ! Use cell pressure data
      CALL SSU_Input_Get_Property( SSU_Input, &
                                   ChannelIndex=ChannelIndex, &
                                   Cell_Pressure=Cell_Pressure )
      AAV%CO2_Cell = Cell_Pressure
      AAV%Index_low = Bisection_Search( TC(SensorIndex)%TC_CellPressure(:,ChannelIndex), AAV%CO2_Cell )
    ELSE
      ! Use mission time data
      CALL SSU_Input_Get_Property( SSU_Input, Time=Time )
      IF( Time < TC(SensorIndex)%Ref_Time(1) )THEN
        Time = TC(SensorIndex)%Ref_Time(1)
        WRITE( msg,'("Invalid time. Reset to ",f8.2)' ) Time
        CALL Display_Message( ROUTINE_NAME, TRIM(msg), WARNING )
      END IF
      ! obtain CO2 cell pressure for Time
      CALL get_CO2_Cell_p( SensorIndex, ChannelIndex, Time, AAV%CO2_Cell )
      ! get index
      AAV%Index_low = Bisection_Search( TC(SensorIndex)%TC_CellPressure(:,ChannelIndex), AAV%CO2_Cell )
    END IF

    ! Compute the interpolation weights
    AAV%Weight(2) = (AAV%CO2_Cell - TC(SensorIndex)%TC_CellPressure(AAV%Index_low,ChannelIndex))/ &
                      (TC(SensorIndex)%TC_CellPressure(AAV%Index_low+1,ChannelIndex) - &
                       TC(SensorIndex)%TC_CellPressure(AAV%Index_low  ,ChannelIndex)   )
    AAV%Weight(1) = ONE - AAV%Weight(2)

    ! Compute the optical depths
    ! ...At cell pressure 1
    CALL ODx_Compute_AtmAbsorption( TC(SensorIndex)%TC(AAV%Index_low), &
                                    ChannelIndex                     , &
                                    Predictor                        , &
                                    AtmAbsorption                    , &
                                    AAV%ODx(1) )    
    Optical_Depth = AtmAbsorption%Optical_Depth
    ! ...At cell pressure 2
    CALL ODx_Compute_AtmAbsorption( TC(SensorIndex)%TC(AAV%Index_low+1), &    
                                    ChannelIndex                       , &       
                                    Predictor                          , &       
                                    AtmAbsorption                      , &    
                                    AAV%ODx(2) )  
    ! ...Weighted average
    AtmAbsorption%Optical_Depth = AAV%Weight(1)*AtmAbsorption%Optical_Depth + &
                                  AAV%Weight(2)*Optical_Depth
                                  
  END SUBROUTINE Compute_AtmAbsorption

!---------------------------------------------------
! TL routine corresponding to Compute_AtmAbsorption
!---------------------------------------------------

  SUBROUTINE Compute_AtmAbsorption_TL( SSU_Input       , &
                                       SensorIndex     , &  
                                       ChannelIndex    , &      
                                       Predictor       , &      
                                       Predictor_TL    , &      
                                       AtmAbsorption_TL, &      
                                       AAV               )      
    ! Arguments
    TYPE(SSU_Input_type)         , INTENT(IN)     :: SSU_Input
    INTEGER                      , INTENT(IN)     :: SensorIndex
    INTEGER                      , INTENT(IN)     :: ChannelIndex
    TYPE(Predictor_type)         , INTENT(IN)     :: Predictor
    TYPE(Predictor_type)         , INTENT(IN)     :: Predictor_TL
    TYPE(CRTM_AtmAbsorption_type), INTENT(IN OUT) :: AtmAbsorption_TL
    TYPE(AAVariables_type)       , INTENT(IN)     :: AAV
    ! Parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_AtmAbsorption_TL (ODSSU)'
    ! Variables
    REAL(fp) :: Optical_Depth_TL(Predictor%n_Layers)

    CALL ODx_Compute_AtmAbsorption_TL( TC(SensorIndex)%TC(AAV%Index_low), &
                                       ChannelIndex                     , &
                                       Predictor                        , &
                                       Predictor_TL                     , &
                                       AtmAbsorption_TL                 , &
                                       AAV%ODx(1) )    
    Optical_Depth_TL = AtmAbsorption_TL%Optical_Depth
    CALL ODx_Compute_AtmAbsorption_TL( TC(SensorIndex)%TC(AAV%Index_low+1), &
                                       ChannelIndex                       , &
                                       Predictor                          , &
                                       Predictor_TL                       , &
                                       AtmAbsorption_TL                   , &
                                       AAV%ODx(2) )    
    AtmAbsorption_TL%Optical_Depth = AAV%Weight(1)*AtmAbsorption_TL%Optical_Depth + &
                                     AAV%Weight(2)*Optical_Depth_TL
                                  
  END SUBROUTINE Compute_AtmAbsorption_TL

!---------------------------------------------------
! AD routine corresponding to Compute_AtmAbsorption
!---------------------------------------------------

  SUBROUTINE Compute_AtmAbsorption_AD( SSU_Input       , &
                                       SensorIndex     , &  
                                       ChannelIndex    , &       
                                       Predictor       , &       
                                       AtmAbsorption_AD, &       
                                       Predictor_AD    , &       
                                       AAV               )       
    ! Arguments
    TYPE(SSU_Input_type)         , INTENT(IN)     :: SSU_Input
    INTEGER,                       INTENT(IN)     :: SensorIndex
    INTEGER,                       INTENT(IN)     :: ChannelIndex
    TYPE(Predictor_type),          INTENT(IN)     :: Predictor
    TYPE(CRTM_AtmAbsorption_type), INTENT(IN OUT) :: AtmAbsorption_AD
    TYPE(Predictor_type),          INTENT(IN OUT) :: Predictor_AD
    TYPE(AAVariables_type)       , INTENT(IN)     :: AAV
    ! Parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_AtmAbsorption_AD (ODSSU)'
    ! Variables
    REAL(fp) :: Optical_Depth_AD(Predictor%n_Layers)

    Optical_Depth_AD               = AAV%Weight(2)*AtmAbsorption_AD%Optical_Depth
    AtmAbsorption_AD%Optical_Depth = AAV%Weight(1)*AtmAbsorption_AD%Optical_Depth
    CALL ODx_Compute_AtmAbsorption_AD( TC(SensorIndex)%TC(AAV%Index_low+1), &
                                       ChannelIndex                       , &
                                       Predictor                          , &
                                       AtmAbsorption_AD                   , &
                                       Predictor_AD                       , &
                                       AAV%ODx(2) )    
    AtmAbsorption_AD%Optical_Depth = AtmAbsorption_AD%Optical_Depth + Optical_Depth_AD
    CALL ODx_Compute_AtmAbsorption_AD( TC(SensorIndex)%TC(AAV%Index_low), &
                                       ChannelIndex                     , &   
                                       Predictor                        , &
                                       AtmAbsorption_AD                 , &
                                       Predictor_AD                     , &
                                       AAV%ODx(1) )    

  END SUBROUTINE Compute_AtmAbsorption_AD
!
!
    SUBROUTINE get_CO2_Cell_p(SensorIndex,ChannelIndex,u,y0)
! -------------------------------------------------------------------
!  Using an sensor "SensorIndex" and time "u" to find CO2 cell pressure "y0".
! -------------------------------------------------------------------
       INTEGER, INTENT( IN ) :: SensorIndex, ChannelIndex
       REAL(fp), INTENT( IN ) :: u
       REAL(fp), INTENT( OUT ) :: y0 
       INTEGER :: n, jLower, jUpper, jMiddle, indx

       n = SIZE(TC(SensorIndex)%Ref_Time)
       jLower = 1
       jUpper = n

       if(u.ge.TC(SensorIndex)%Ref_Time(n)) then
         y0 = TC(SensorIndex)%Ref_CellPressure(n,ChannelIndex)
       return
       else if(u.le.TC(SensorIndex)%Ref_Time(1)) then
         y0 = TC(SensorIndex)%Ref_CellPressure(1,ChannelIndex) 
       return
       endif

       indx = Bisection_Search( TC(SensorIndex)%Ref_Time, u )
      
       y0 = TC(SensorIndex)%Ref_CellPressure(indx,ChannelIndex) + &
          (TC(SensorIndex)%Ref_CellPressure(indx+1,ChannelIndex)- &
          TC(SensorIndex)%Ref_CellPressure(indx,ChannelIndex))/  &
          (TC(SensorIndex)%Ref_Time(indx+1)-TC(SensorIndex)%Ref_Time(indx))* &
          (u-TC(SensorIndex)%Ref_Time(indx))
       RETURN
    END SUBROUTINE get_CO2_Cell_p
!
  
END MODULE ODSSU_AtmAbsorption

