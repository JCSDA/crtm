!
! ODZeeman_AtmAbsorption
!
! Module containing routines to compute the optical depth profile
! due to gaseous absorption affected by Zeeman spilitting in 
! the Optical Depth Pressure Space (ODPS).
!
!
! CREATION HISTORY:
!       Written by:     Yong Han, JCSDA, NOAA/NESDIS 10-NOV-2009
!

MODULE ODZeeman_AtmAbsorption

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,                ONLY: fp
  USE Message_Handler,           ONLY: SUCCESS, FAILURE, Display_Message
  USE CRTM_Atmosphere_Define,    ONLY: CRTM_Atmosphere_type, H2O_ID
  USE CRTM_AtmAbsorption_Define, ONLY: CRTM_AtmAbsorption_type
  USE CRTM_GeometryInfo_Define,  ONLY: CRTM_GeometryInfo_type
  USE ODPS_Predictor_Define,     ONLY: Predictor_type
  USE ODPS_Define,               ONLY: ODPS_type
  USE ODZeeman_Predictor,        ONLY: Compute_Predictors_zssmis,     &
                                       Compute_Predictors_zssmis_TL,  &
                                       Compute_Predictors_zssmis_AD,  &
                                       N_ZCOMPONENTS,                 &
                                       N_ZABSORBERS,                  &
                                       MAX_N_PREDICTORS_ZSSMIS,       &
                                       ZSSMIS_ChannelMap,             &
                                       ODPS_gINDEX_SSMIS
  USE CRTM_SensorInput_Define,   ONLY: CRTM_SensorInput_type, &
                                       CRTM_SensorInput_Get_Property
  USE ODPS_CoordinateMapping,    ONLY: Map_Input, Map_Input_TL, Map_Input_AD, &
                                       Interpolate_Profile,    &
                                       Interpolate_Profile_F1_TL, &
                                       Interpolate_Profile_F1_AD, &
                                       Compute_Interp_Index
                                       
  ! Disable implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Public routines
  PUBLIC :: ZSSMIS_Compute_Predictors
  PUBLIC :: ZSSMIS_Compute_Predictors_TL
  PUBLIC :: ZSSMIS_Compute_Predictors_AD
  PUBLIC :: ZSSMIS_Compute_AtmAbsorption
  PUBLIC :: ZSSMIS_Compute_AtmAbsorption_TL
  PUBLIC :: ZSSMIS_Compute_AtmAbsorption_AD
  ! routines from other modules
  PUBLIC :: Is_ZSSMIS_Channel
  PUBLIC :: Is_ZSSMIS
  
  ! Public 
  PUBLIC :: N_ZCOMPONENTS
  PUBLIC :: N_ZABSORBERS
  PUBLIC :: MAX_N_PREDICTORS_ZSSMIS

  ! ----------
  ! Parameters
  ! ----------

  REAL(fp), PARAMETER :: ZERO = 0.0_fp, ONE = 1.0_fp

CONTAINS

!------------------------------------------------------------------------------
!
! NAME:
!      ZSSMIS_Compute_AtmAbsorption
!
! PURPOSE:
!       Subroutine to compute slant path optical path for SSMIS channels 19 - 22 
!       which are affected by Zeeman splitting
!
! CALLING SEQUENCE:
!
!    SUBROUTINE ZSSMIS_Compute_AtmAbsorption(TC, &
!                                            ChannelIndex,  &
!                                            Predictor,     &       
!                                            AtmAbsorption )
!
! INPUT ARGUMENTS:
!
!            TC:         ODPS structure holding coefficient data
!                        UNITS:      N/A         
!                        TYPE:       ODPS_type   
!                        DIMENSION:  Scalar      
!                        ATTRIBUTES: INTENT(IN) 
!
!      ChannelIndex:     Channel index (a sequential number for the channels in the structure TC)
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar  
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor:       Predictor structure containing the predictors for estimating of optical depth
!                        UNITS:      N/A
!                        TYPE:       TYPE(Predictor_type)
!                        DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!   OUTPUT ARGUMENTS:
!        AtmAbsorption:  Structure containing computed optical depth
!                        profile data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_AtmAbsorption_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!------------------------------------------------------------------------------
  SUBROUTINE ZSSMIS_Compute_AtmAbsorption( TC           , &  ! Input
                                           ChannelIndex , &  ! Input   
                                           Predictor    , &  ! Input   
                                           AtmAbsorption)    ! Output  

    ! Arguments
    TYPE(ODPS_type)              , INTENT(IN)     :: TC
    INTEGER                      , INTENT(IN)     :: ChannelIndex
    TYPE(Predictor_type)         , INTENT(IN OUT) :: Predictor
    TYPE(CRTM_AtmAbsorption_type), INTENT(IN OUT) :: AtmAbsorption
    ! Local variables
    INTEGER  :: n_User_Layers
    REAL(fp) :: OD(Predictor%n_Layers)
    REAL(fp) :: OD_Path(0:Predictor%n_Layers)
    REAL(fp) :: User_OD_Path(0:Predictor%n_User_Layers)
    INTEGER  :: ODPS2User_Idx(2, 0:Predictor%n_User_Layers)
    INTEGER  :: idx

    n_User_Layers = Predictor%n_User_Layers
    
    idx = ZSSMIS_ChannelMap(ChannelIndex)

    CALL Compute_ODPath_zssmis(idx,        &           
                               TC,         &   
                               Predictor,  &  
                               OD_Path)         

    ! Interpolate the path profile back on the user pressure grids,
    ! Compute layer optical depths (vertical direction)

    IF(Predictor%PAFV%Active)THEN  
      ! save forwad variables
      Predictor%PAFV%OD = OD
      Predictor%PAFV%OD_Path = OD_Path
      ! If interpolation indexes are known
      User_OD_Path(0) = ZERO
      CALL Interpolate_Profile(Predictor%PAFV%ODPS2User_Idx,    &
                               OD_Path,                         &
                               Predictor%Ref_Level_LnPressure,  &
                               Predictor%User_Level_LnPressure, &
                               User_OD_Path)
    ELSE ! interpolation indexes are not known

      CALL Compute_Interp_Index(Predictor%Ref_Level_LnPressure, &
                                Predictor%User_Level_LnPressure,&
                                ODPS2User_Idx)  

      CALL Interpolate_Profile(ODPS2User_Idx,                   &
                               OD_Path,                         &
                               Predictor%Ref_Level_LnPressure,  &
                               Predictor%User_Level_LnPressure, &
                               User_OD_Path)
      
    END IF

    ! Optical depth profile scaled to zenith.  Note that the scaling
    ! factor is the surface secant zenith angle.
    AtmAbsorption%Optical_Depth = (User_OD_Path(1:n_User_Layers) - &
                                   User_OD_Path(0:n_User_Layers-1)) / &
                                   Predictor%Secant_Zenith_Surface

  END SUBROUTINE ZSSMIS_Compute_AtmAbsorption

!------------------------------------------------------------------------------
!
! NAME:
!      ZSSMIS_Compute_AtmAbsorption_TL
!
! PURPOSE:
!       Subroutine to compute TL slant path optical path for SSMIS channels 19 - 22 
!       which are affected by Zeeman splitting
!
! CALLING SEQUENCE:
!
!        CALL ZSSMIS_Compute_AtmAbsorption_TL(TC,            &
!                                             ChannelIndex,  &
!                                             Predictor,     &  
!                                             Predictor_TL,  &  
!                                             AtmAbsorption_TL )
!
! INPUT ARGUMENTS:
!
!            TC:         ODPS structure holding coefficient data
!                        UNITS:      N/A         
!                        TYPE:       ODPS_type   
!                        DIMENSION:  Scalar      
!                        ATTRIBUTES: INTENT(IN) 
!
!      ChannelIndex:     Channel index (a sequential number for the channels in the structure TC)
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar  
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor:       Predictor structure containing the predictors for estimating of optical depth
!                        UNITS:      N/A
!                        TYPE:       TYPE(Predictor_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor_TL:    Predictor structure containing the TL predictors
!                        UNITS:      N/A
!                        TYPE:       TYPE(Predictor_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(INOUT)
!
!   OUTPUT ARGUMENTS:
!      AtmAbsorption_TL: Structure containing computed TL optical depth
!                        profile data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_AtmAbsorption_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!------------------------------------------------------------------------------
  SUBROUTINE ZSSMIS_Compute_AtmAbsorption_TL(TC           ,    &  ! Input
                                             ChannelIndex ,    &  ! Input    
                                             Predictor    ,    &  ! Input    
                                             Predictor_TL,     &  ! Input       
                                             AtmAbsorption_TL)    ! Output   
    ! Arguments
    TYPE(ODPS_type)              , INTENT(IN)     :: TC
    INTEGER                      , INTENT(IN)     :: ChannelIndex
    TYPE(Predictor_type)         , INTENT(IN)     :: Predictor
    TYPE(Predictor_type)         , INTENT(INOUT)  :: Predictor_TL
    TYPE(CRTM_AtmAbsorption_type), INTENT(INOUT)  :: AtmAbsorption_TL
    ! Local variables
    INTEGER  :: n_User_Layers
    REAL(fp) :: OD_TL(Predictor%n_Layers)  
    REAL(fp) :: OD_Path_TL(0:Predictor%n_Layers)
    REAL(fp) :: User_OD_Path_TL(0:Predictor%n_User_Layers)
    INTEGER  :: ODPS2User_Idx(2, 0:Predictor%n_User_Layers)
    INTEGER  :: idx

    n_User_Layers = Predictor%n_User_Layers

    idx = ZSSMIS_ChannelMap(ChannelIndex)

    CALL Compute_ODPath_zssmis_TL(idx,             &   
                                  TC,              &   
                                  Predictor,       &   
                                  Predictor_TL,    &   
                                  OD_Path_TL )         

    ! Interpolate the path profile back on the user pressure grids,
    ! Compute layer optical depths (vertical direction)
    CALL Interpolate_Profile_F1_TL(Predictor%PAFV%ODPS2User_Idx,    &
                                   Predictor%PAFV%OD_Path,          &
                                   Predictor%Ref_Level_LnPressure,  &
                                   Predictor%User_Level_LnPressure, &
                                   OD_Path_TL,                      &
                                   User_OD_Path_TL)

    AtmAbsorption_TL%Optical_Depth = (User_OD_Path_TL(1:n_User_Layers) - &
                                   User_OD_Path_TL(0:n_User_Layers-1)) / &
                                   Predictor%Secant_Zenith_Surface

  END SUBROUTINE ZSSMIS_Compute_AtmAbsorption_TL

!------------------------------------------------------------------------------
!
! NAME:
!     ZSSMIS_Compute_AtmAbsorption_AD
!
! PURPOSE:
!       Subroutine to compute AD slant path optical path for SSMIS channels 19 - 22 
!       which are affected by Zeeman splitting
!
! CALLING SEQUENCE:
!
!        CALL ZSSMIS_Compute_AtmAbsorption_AD(TC,           &
!                                      ChannelIndex,        &
!                                      Predictor,           &
!                                      AtmAbsorption_AD,    &  
!                                      Predictor_AD)    
!
! INPUT ARGUMENTS:
!
!            TC:         ODPS structure holding coefficient data
!                        UNITS:      N/A         
!                        TYPE:       ODPS_type   
!                        DIMENSION:  Scalar      
!                        ATTRIBUTES: INTENT(IN) 
!
!      ChannelIndex:     Channel index (a sequential number for the channels in the structure TC)
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar  
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor:       Predictor structure containing the predictors for estimating of optical depth
!                        UNITS:      N/A
!                        TYPE:       TYPE(Predictor_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!         OD_Path_AD:    AD Slant path optical path profile (from space down) 
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1 (0:n_Layers)
!                        ATTRIBUTES: INTENT(INOUT)
!
!   OUTPUT ARGUMENTS:
!      AtmAbsorption_AD: Structure containing computed AD optical depth
!                        profile data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_AtmAbsorption_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!------------------------------------------------------------------------------
  SUBROUTINE ZSSMIS_Compute_AtmAbsorption_AD( TC           ,    &  ! Input
                                              ChannelIndex ,    &  ! Input   
                                              Predictor    ,    &  ! Input   
                                              AtmAbsorption_AD, &  ! Input   
                                              Predictor_AD)        ! Output  
    ! Arguments
    TYPE(ODPS_type)              , INTENT(IN)     :: TC
    INTEGER                      , INTENT(IN)     :: ChannelIndex
    TYPE(Predictor_type)         , INTENT(IN)     :: Predictor
    TYPE(CRTM_AtmAbsorption_type), INTENT(IN OUT) :: AtmAbsorption_AD
    TYPE(Predictor_type)         , INTENT(IN OUT) :: Predictor_AD
    ! Local variables
    INTEGER  :: n_User_Layers, k
    REAL(fp) :: OD_AD(Predictor%n_Layers)                 
    REAL(fp) :: OD_Path_AD(0:Predictor%n_Layers) 
    REAL(fp) :: User_OD_Path_AD(0:Predictor%n_User_Layers)
    INTEGER  :: ODPS2User_Idx(2, 0:Predictor%n_User_Layers)
    INTEGER  :: idx

    n_User_Layers = Predictor%n_User_Layers
    idx = ZSSMIS_ChannelMap(ChannelIndex)
 
     !------- Adjoint part ---------
    
    ! Interpolate the path profile back on the user pressure grids,
    ! Compute layer optical depths (vertical direction)
    User_OD_Path_AD(n_User_Layers) = ZERO
    DO k = n_User_Layers, 1, -1
      User_OD_Path_AD(k) = User_OD_Path_AD(k) &
                           + AtmAbsorption_AD%Optical_Depth(k)/Predictor%Secant_Zenith_Surface
      ! combined with initilization
      User_OD_Path_AD(k-1) = -AtmAbsorption_AD%Optical_Depth(k)/Predictor%Secant_Zenith_Surface
    END DO
    AtmAbsorption_AD%Optical_Depth = ZERO

    OD_Path_AD = ZERO          
    CALL Interpolate_Profile_F1_AD(Predictor%PAFV%ODPS2User_Idx,       &
                                   Predictor%PAFV%OD_Path,             &
                                   Predictor%Ref_Level_LnPressure,     &
                                   Predictor%User_Level_LnPressure,    &
                                   User_OD_Path_AD,                    &
                                   OD_Path_AD )

 
    User_OD_Path_AD(0) = ZERO

    CALL Compute_ODPath_zssmis_AD(idx,          &  
                                  TC,           &   
                                  Predictor,    &  
                                  OD_Path_AD,   &  
                                  Predictor_AD )   

  END SUBROUTINE ZSSMIS_Compute_AtmAbsorption_AD
  
!------------------------------------------------------------------------------
!
! NAME:
!      Compute_ODPath_zssmis
!
! PURPOSE:
!       Subroutine to compute slant path optical path for ZSSMIS 
!       (a virtual sensor created for the SSMIS 4 Zeeman channels:
!       ch 19, 20, 21, 22)
!
! CALLING SEQUENCE:
!
!    SUBROUTINE Compute_ODPath_zssmis(ChannelIndex,  &
!                                     TC,            &       
!                                     Predictor,     &       
!                                     OD_Path )
!
! INPUT ARGUMENTS:
!
!      ChannelIndex:     Channel index (a sequential number for the channels in the structure TC)
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar  
!                        ATTRIBUTES: INTENT(IN)
!
!            TC:         ODPS structure holding coefficient data
!                        UNITS:      N/A         
!                        TYPE:       ODPS_type   
!                        DIMENSION:  Scalar      
!                        ATTRIBUTES: INTENT(IN) 
!
!       Predictor:       Predictor structure containing the predictors for estimating of optical depth
!                        UNITS:      N/A
!                        TYPE:       TYPE(Predictor_type)
!                        DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!   OUTPUT ARGUMENTS:
!         OD_Path:      Slant path optical path profile (from space down) 
!                       UNITS:      N/A
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Rank-1 (0:n_Layers)
!                       ATTRIBUTES: INTENT(OUT)
!
!------------------------------------------------------------------------------

  SUBROUTINE Compute_ODPath_zssmis(ChannelIndex,  &
                                   TC,            &       
                                   Predictor,     &       
                                   OD_Path )
    INTEGER,              INTENT( IN )     :: ChannelIndex
    TYPE(ODPS_type),      INTENT( IN )     :: TC
    TYPE(Predictor_type), INTENT( INOUT )  :: Predictor
    REAL(fp),             INTENT( OUT)     :: OD_Path(0:)
                                          
    ! Local
    REAL(fp), DIMENSION(Predictor%n_Layers) :: OD1, OD2, OD
    REAL(fp) :: OD_tmp
    REAL(fp) :: w1, w2, Doppler_shift
    INTEGER  :: i, j, j1, j2, js1, js2, k, inode, n_nodes, n_Layers, nc, np

    OD_Path = ZERO
    np = TC%n_Predictors(1, ChannelIndex)                                     

    ! Check if there is any absorption for the component&channel combination.  
    IF( np > 0 ) THEN  
      
      !----------------------------------------------------------------------------------
      ! (1) Find the nodes of the Doppler shift frequencies, which bracket the user  
      ! Doppler frequency; (2) compute weights for interpolation.
      !----------------------------------------------------------------------------------                                      
      Doppler_Shift = Predictor%u
      j = TC%Pos_Index(1, ChannelIndex)  
                                   
      n_nodes = INT(TC%C(j))

      n_Layers = Predictor%n_Layers
      j = j + 1
      IF(Doppler_Shift < TC%C(j))THEN
        j1 = j
        w1 = ONE
        w2 = ZERO
        inode = 1
      ELSE IF(Doppler_Shift > TC%C(j+n_nodes-1))THEN
        j1 = j+n_nodes-2
        w1 = ZERO
        w2 = ONE
        inode = n_nodes-2
      ELSE
        DO i = 1, n_nodes-1 
          j1 = j + i - 1
          j2 = j1 + 1
          IF(Doppler_Shift >= TC%C(j1) .AND. Doppler_Shift <= TC%C(j2))THEN
            w1 = (TC%C(j2) - Doppler_Shift)/(TC%C(j2) - TC%C(j1))
            w2 = ONE - w1
            inode = i
            EXIT
          END IF
        END DO
      END IF

      !--------------------------------------------
      ! Compute optical depths at the two nodes
      !--------------------------------------------
      OD1 = ZERO
      OD2 = ZERO
      nc = np * n_Layers
      j1 = j + n_nodes + (inode-1)*nc 
      j2 = j1 + nc
      DO i = 1, np                                                             
        js1 = j1+(i-1)*n_Layers-1                                               
        js2 = j2+(i-1)*n_Layers-1                                               
        DO k = 1, n_Layers 
          OD1(k) = OD1(k) + TC%C(js1+k)*Predictor%X(k, i, 1)                     
          OD2(k) = OD2(k) + TC%C(js2+k)*Predictor%X(k, i, 1)                     
        END DO                                                                 
      END DO
      !-------------------------------------------------------
      ! (1) Interpolate on the user requested Doppler frequency
      ! (2) Compute the Slant path optical depth profile.
      !-------------------------------------------------------
      DO k = 1, n_Layers
        IF(ChannelIndex == 2)THEN
          OD(k) = w1*EXP(OD1(k)) + w2*EXP(OD2(k))
          OD_tmp = OD(k) 
        ELSE
          OD(k) = w1*OD1(k) + w2*OD2(k)
          OD_tmp = OD(k)
          IF(OD(k) < ZERO)OD_tmp = ZERO
        END IF
        OD_Path(k) = OD_Path(k-1) + OD_tmp*Predictor%Secant_Zenith(k)
      END DO

      ! Save FW variables
      IF(Predictor%PAFV%Active)THEN
        Predictor%PAFV%OD = OD
        Predictor%PAFV%w1 = w1
        Predictor%PAFV%w2 = w2
        Predictor%PAFV%inode = inode
      END IF

    END IF
    
  END SUBROUTINE Compute_ODPath_zssmis       

!------------------------------------------------------------------------------
!
! NAME:
!      Compute_ODPath_zssmis_TL
!
! PURPOSE:
!       Subroutine to compute TL slant path optical path for ZSSMIS 
!       (a virtual sensor created for the SSMIS 4 Zeeman channels:
!       ch 19, 20, 21, 22)
!
! CALLING SEQUENCE:
!
!        CALL Compute_ODPath_zssmis_TL(ChannelIndex,  &
!                                      TC,            & 
!                                      Predictor,     &   
!                                      Predictor_TL,  &    
!                                      OD_Path_TL )
!
! INPUT ARGUMENTS:
!
!      ChannelIndex:     Channel index (a sequential number for the channels in the structure TC)
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar  
!                        ATTRIBUTES: INTENT(IN)
!
!            TC:         ODPS structure holding coefficient data
!                        UNITS:      N/A         
!                        TYPE:       ODPS_type   
!                        DIMENSION:  Scalar      
!                        ATTRIBUTES: INTENT(IN) 
!
!       Predictor:       Predictor structure containing the predictors for estimating of optical depth
!                        UNITS:      N/A
!                        TYPE:       TYPE(Predictor_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor_TL:    Predictor structure containing the TL predictors
!                        UNITS:      N/A
!                        TYPE:       TYPE(Predictor_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(INOUT)
!
!   OUTPUT ARGUMENTS:
!         OD_Path_TL:   TL Slant path optical path profile (from space down) 
!                       UNITS:      N/A
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Rank-1 (0:n_Layers)
!                       ATTRIBUTES: INTENT(OUT)
!
!------------------------------------------------------------------------------
  SUBROUTINE Compute_ODPath_zssmis_TL(ChannelIndex,  &
                                      TC,            & 
                                      Predictor,     &   
                                      Predictor_TL,  &    
                                      OD_Path_TL )
    INTEGER,              INTENT( IN )     :: ChannelIndex
    TYPE(ODPS_type),      INTENT( IN )     :: TC
    TYPE(Predictor_type), INTENT( IN )     :: Predictor
    TYPE(Predictor_type), INTENT( INOUT )  :: Predictor_TL
    REAL(fp),             INTENT(OUT)      :: OD_Path_TL(0:)
                                          
    ! Local
    REAL(fp), DIMENSION(Predictor%n_Layers) :: OD1, OD2, OD1_TL, OD2_TL
    REAL(fp) :: O1, O2
    REAL(fp) :: OD_TL
    INTEGER  :: i, j, j1, j2, js1, js2, k, n_nodes, n_Layers, nc, np
    
    OD_Path_TL = ZERO
    np = TC%n_Predictors(1, ChannelIndex)                                     

    ! Check if there is any absorption for the component&channel combination.  
    IF( np > 0 ) THEN                                         

      !----------------------------------------------------------------------------------
      ! (1) Find the nodes of the Doppler shift frequencies, which bracket the user  
      ! Doppler frequency; (2) compute weights for interpolation.
      !---------------------------------------------------------------------------------- 
      j = TC%Pos_Index(1, ChannelIndex)                                     
      n_nodes = INT(TC%C(j))
      n_Layers = Predictor%n_Layers
      j = j + 1

      !--------------------------------------------
      ! Compute optical depths at the two nodes
      !--------------------------------------------

      OD1 = ZERO
      OD2 = ZERO
      OD1_TL = ZERO
      OD2_TL = ZERO
      nc = np * n_Layers
      j1 = j + n_nodes + (Predictor%PAFV%inode-1)*nc 
      j2 = j1 + nc
      DO i = 1, np                                                             
        js1 = j1+(i-1)*n_Layers-1                                               
        js2 = j2+(i-1)*n_Layers-1
        DO k = 1, n_Layers 
          OD1(k) = OD1(k) + TC%C(js1+k)*Predictor%X(k, i, 1)                     
          OD2(k) = OD2(k) + TC%C(js2+k)*Predictor%X(k, i, 1)                     
          OD1_TL(k) = OD1_TL(k) + TC%C(js1+k)*Predictor_TL%X(k, i, 1)                     
          OD2_TL(k) = OD2_TL(k) + TC%C(js2+k)*Predictor_TL%X(k, i, 1)                     
        END DO                                                                 
      END DO
      !-------------------------------------------------------
      ! (1) Interpolate on the user requested Doppler frequency
      ! (2) Compute the Slant path optical depth profile.
      !-------------------------------------------------------
      DO k = 1, n_Layers
        IF(ChannelIndex == 2)THEN
          O1 = EXP(OD1(k))
          O2 = EXP(OD2(k))
          OD_TL = Predictor%PAFV%w1*O1*OD1_TL(k) + Predictor%PAFV%w2*O2*OD2_TL(k)
        ELSE
          OD_TL = Predictor%PAFV%w1*OD1_TL(k) + Predictor%PAFV%w2*OD2_TL(k)
          IF(Predictor%PAFV%OD(k) < ZERO)OD_TL = ZERO
        END IF
        OD_Path_TL(k) = OD_Path_TL(k-1) + OD_TL*Predictor%Secant_Zenith(k)
      END DO
    END IF
  END SUBROUTINE Compute_ODPath_zssmis_TL       

!------------------------------------------------------------------------------
!
! NAME:
!      Compute_ODPath_zssmis_AD
!
! PURPOSE:
!       Subroutine to compute AD slant path optical path for ZSSMIS 
!       (a virtual sensor created for the SSMIS 4 Zeeman channels:
!       ch 19, 20, 21, 22)
!
! CALLING SEQUENCE:
!
!        CALL Compute_ODPath_zssmis_AD(ChannelIndex,  &
!                                      TC,            & 
!                                      Predictor,     & 
!                                      OD_Path_AD,    &  
!                                      Predictor_AD)    
!
! INPUT ARGUMENTS:
!
!      ChannelIndex:     Channel index (a sequential number for the channels in the structure TC)
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar  
!                        ATTRIBUTES: INTENT(IN)
!
!            TC:         ODPS structure holding coefficient data
!                        UNITS:      N/A         
!                        TYPE:       ODPS_type   
!                        DIMENSION:  Scalar      
!                        ATTRIBUTES: INTENT(IN) 
!
!       Predictor:       Predictor structure containing the predictors for estimating of optical depth
!                        UNITS:      N/A
!                        TYPE:       TYPE(Predictor_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!         OD_Path_AD:    AD Slant path optical path profile (from space down) 
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1 (0:n_Layers)
!                        ATTRIBUTES: INTENT(INOUT)
!
!   OUTPUT ARGUMENTS:
!       Predictor_AD:    Predictor structure containing the AD predictors
!                        UNITS:      N/A
!                        TYPE:       TYPE(Predictor_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(INOUT)
!
!------------------------------------------------------------------------------
  SUBROUTINE Compute_ODPath_zssmis_AD(ChannelIndex,  &
                                      TC,            & 
                                      Predictor,     & 
                                      OD_Path_AD,    &  
                                      Predictor_AD)    
    INTEGER,              INTENT( IN )     :: ChannelIndex
    TYPE(ODPS_type),      INTENT( IN )     :: TC
    TYPE(Predictor_type), INTENT( IN )     :: Predictor
    REAL(fp),             INTENT( INOUT )  :: OD_Path_AD(0:)
    TYPE(Predictor_type), INTENT( INOUT )  :: Predictor_AD

    ! Local
    REAL(fp), DIMENSION(Predictor%n_Layers) :: OD1, OD2, OD1_AD, OD2_AD
    REAL(fp) :: O1, O2
    REAL(fp) :: OD_AD
    INTEGER  :: i, j, j1, j2, js1, js2, k, n_nodes, n_Layers, nc, np

    np = TC%n_Predictors(1, ChannelIndex)                                     

    !------------------------
    ! Forward calculation
    !------------------------
    ! Check if there is any absorption for the component&channel combination.  
    IF( np > 0 ) THEN                                         
      j = TC%Pos_Index(1, ChannelIndex)                                     
      n_nodes = INT(TC%C(j))
      n_Layers = Predictor%n_Layers
      j = j + 1

      OD1 = ZERO
      OD2 = ZERO
      nc = np * n_Layers
      j1 = j + n_nodes + (Predictor%PAFV%inode-1)*nc 
      j2 = j1 + nc
      DO i = 1, np                                                             
        js1 = j1+(i-1)*n_Layers-1                                               
        js2 = j2+(i-1)*n_Layers-1                                               
        DO k = 1, n_Layers 
          OD1(k) = OD1(k) + TC%C(js1+k)*Predictor%X(k, i, 1)                     
          OD2(k) = OD2(k) + TC%C(js2+k)*Predictor%X(k, i, 1)                     
        END DO                                                                 
      END DO

     !-------------------------------
     ! AD calculation
     !-------------------------------
      DO k = n_Layers, 1, -1
        OD_AD = Predictor%Secant_Zenith(k)*OD_Path_AD(k) ! OD_AD does not cumulate
        OD_Path_AD(k-1) = OD_Path_AD(k-1) + OD_Path_AD(k)
        IF(ChannelIndex == 2)THEN
          O1 = EXP(OD1(k))
          O2 = EXP(OD2(k))
          
          OD1_AD(k) = Predictor%PAFV%w1*O1*OD_AD  ! OD1_AD(k) and OD2_AD(k) do not cumulate
          OD2_AD(k) = Predictor%PAFV%w2*O2*OD_AD          
        ELSE
          IF(Predictor%PAFV%OD(k) < ZERO)OD_AD = ZERO
          OD1_AD(k) = Predictor%PAFV%w1*OD_AD
          OD2_AD(k) = Predictor%PAFV%w2*OD_AD          
        END IF
      END DO

      DO i = 1, np                                                             
        js1 = j1+(i-1)*n_Layers-1                                               
        js2 = j2+(i-1)*n_Layers-1                                               
        DO k = n_Layers, 1, -1
          Predictor_AD%X(k, i, 1) = Predictor_AD%X(k, i, 1) + &
                                    OD1_AD(k)*TC%C(js1+k)
          Predictor_AD%X(k, i, 1) = Predictor_AD%X(k, i, 1) + &
                                    OD2_AD(k)*TC%C(js2+k)
        END DO                                                                 
      END DO
      
      OD_Path_AD = ZERO
      
    END IF

  END SUBROUTINE Compute_ODPath_zssmis_AD

!--------------------------------------------------------------------------------
!
! NAME:
!       ZSSMIS_Compute_Predictors
!
! PURPOSE:
!       Subroutine to calculate the gas absorption model predictors. It first
!       Interpolates the user temperature and absorber profiles on the
!       internal pressure grids and then call the predictor computation
!       routine to compute the predictors
!
! CALLING SEQUENCE:
!       CALL ZSSMIS_Compute_Predictors ( SensorInput   &  ! Input
!                                        TC,           &  ! Input                   
!                                        Atm,          &  ! Input                   
!                                        GeoInfo,      &  ! Input                      
!                                        Predictor,    )  ! Output                  
!
! INPUT ARGUMENTS:
!     SensorInput :     Structure holding sensor specific user inputs
!                        UNITS:      N/A
!                        TYPE:       SensorInput_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!          TC:           ODPS structure holding tau coefficients
!                        UNITS:      N/A
!                        TYPE:       ODPS_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Atm       :     CRTM Atmosphere structure containing the atmospheric
!                       state data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Atmosphere_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       GeoInfo     :   CRTM_GeometryInfo structure containing the 
!                       view geometry information.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_GeometryInfo_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Predictor:      Predictor structure containing the integrated absorber
!                       and predictor profiles.
!                       UNITS:      N/A
!                       TYPE:       TYPE(Predictor_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE ZSSMIS_Compute_Predictors(SensorInput,  &
                                       TC,           &
                                       Atm,          &  
                                       GeoInfo,      &
                                       Predictor)     
    ! Arguments
    TYPE(CRTM_SensorInput_type)  , INTENT(IN)     :: SensorInput
    TYPE(ODPS_type)              , INTENT(IN)     :: TC
    TYPE(CRTM_Atmosphere_type)   , INTENT(IN)     :: Atm
    TYPE(CRTM_GeometryInfo_type) , INTENT(IN)     :: GeoInfo
    TYPE(Predictor_type)         , INTENT(IN OUT) :: Predictor

    ! Local variables

    ! SSMIS SensorInput data
    REAL(fp) :: Temperature(Predictor%n_Layers)
    REAL(fp) :: Absorber(Predictor%n_Layers, TC%n_Absorbers)
    INTEGER  :: H2O_idx
    REAL(fp) :: Be, CosBK, Doppler_Shift

    !------------------------------------------------------------------
    ! Mapping data from user to internal fixed pressure layers/levels.
    !------------------------------------------------------------------
    CALL Map_Input(Atm,                             &  ! Input
                   TC,                              &  ! Input
                   GeoInfo,                         &  ! Input
                   Temperature,                     &  ! Output 
                   Absorber,                        &  ! output
                   Predictor%User_Level_LnPressure, &  ! Output, non variable
                   Predictor%Ref_Level_LnPressure,  &  ! Output, non variable
                   Predictor%Secant_Zenith,         &  ! Output, non variable
                   H2O_idx,                         &
                   Predictor%PAFV)                     ! structure holding FW parameters
                   
    ! store the surface secant zenith angle
    Predictor%Secant_Zenith_Surface = GeoInfo%Secant_Sensor_Zenith
                                  
    !-------------------------------------------
    ! Compute predictor
    !-------------------------------------------
    
    CALL CRTM_SensorInput_Get_Property( SensorInput%SSMIS, &              
                                        Field_Strength = Be, &            
                                        COS_ThetaB     = CosBk, &         
                                        Doppler_Shift  = Doppler_Shift )  
    CALL Compute_Predictors_zssmis(Temperature,    &                      
                                   Be,             &                      
                                   CosBK,          &                      
                                   Doppler_Shift,  &                      
                                   Predictor%Secant_Zenith,&              
                                   Predictor)                             
         
    IF(Predictor%PAFV%Active)THEN
      ! Set and save the interpolation index array for absorption
      ! calculations. Since the indexes do not depend on channel but
      ! the absorption calculations do, put the index calculation here
      ! can improve efficency.
      CALL Compute_Interp_Index(Predictor%Ref_Level_LnPressure ,  &
                                Predictor%User_Level_LnPressure,  &
                                Predictor%PAFV%ODPS2User_Idx) 
    END IF 
                    
  END SUBROUTINE ZSSMIS_Compute_Predictors     

!--------------------------------------------------------------------------------
!
! NAME:
!       ZSSMIS_Compute_Predictors_TL
!
! PURPOSE:
!       Subroutine to calculate the gas absorption model predictors. It first
!       Interpolates the user temperature and absorber profiles on the
!       internal pressure grids and then call the predictor computation
!       routine to compute the predictors
!
! CALLING SEQUENCE:
!       CALL ZSSMIS_Compute_Predictors_TL ( SensorInput      &  ! Input
!                                           TC,              &  ! Input                   
!                                           Atm,             &  ! Input                   
!                                           GeoInfo,         &  ! Input                   
!                                           Predictor,       &  ! Input                   
!                                           Atm_TL,          &  ! Input                   
!                                           Predictor_TL     )  ! Output                  
!
! INPUT ARGUMENTS:
!     SensorInput :     Structure holding sensor specific user inputs
!                        UNITS:      N/A
!                        TYPE:       SensorInput_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!          TC:           ODPS structure holding tau coefficients
!                        UNITS:      N/A
!                        TYPE:       ODPS_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Atm       :     CRTM Atmosphere structure containing the atmospheric
!                       state data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Atmosphere_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Atm_TL    :     CRTM Atmosphere structure containing the atmospheric
!                       state data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Atmosphere_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       GeoInfo     :   CRTM_GeometryInfo structure containing the 
!                       view geometry information.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_GeometryInfo_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Predictor:      Predictor structure containing the integrated absorber
!                       and predictor profiles.
!                       UNITS:      N/A
!                       TYPE:       TYPE(Predictor_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Predictor_TL:   Predictor structure containing the integrated absorber
!                       and predictor profiles.
!                       UNITS:      N/A
!                       TYPE:       TYPE(Predictor_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE ZSSMIS_Compute_Predictors_TL(SensorInput,    &
                                          TC,             & 
                                          Atm,            &  
                                          GeoInfo,        & 
                                          Predictor,      & 
                                          Atm_TL,         & 
                                          Predictor_TL)     
 
    TYPE(CRTM_SensorInput_type)  , INTENT(IN)     :: SensorInput
    TYPE(ODPS_type)              , INTENT(IN)     :: TC
    TYPE(CRTM_Atmosphere_type)   , INTENT(IN)     :: Atm         
    TYPE(CRTM_GeometryInfo_type) , INTENT(IN)     :: GeoInfo
    TYPE(Predictor_type)         , INTENT(IN)     :: Predictor
    TYPE(CRTM_Atmosphere_type)   , INTENT(IN)     :: Atm_TL
    TYPE(Predictor_type)         , INTENT(IN OUT) :: Predictor_TL

    ! Local variables
    REAL(fp) :: Absorber_TL(Predictor%n_Layers, TC%n_Absorbers)
    REAL(fp) :: Temperature_TL(Predictor%n_Layers)
    ! SSMIS SensorInput data
    REAL(fp) :: Be, CosBK, Doppler_Shift

    !------------------------------------------------------------------
    ! Mapping data from user to internal fixed pressure layers/levels.
    !------------------------------------------------------------------
    CALL Map_Input_TL(Atm,            &  ! Input
                      TC,             &  ! Input
                      Atm_TL,         &  ! Input
                      Temperature_TL, &  ! Output
                      Absorber_TL,    &  ! Output
                      Predictor%PAFV)    ! Input

    !-------------------------------------------
    ! Compute predictor
    !-------------------------------------------
    CALL CRTM_SensorInput_Get_Property( SensorInput%SSMIS, &                   
                                        Field_Strength = Be, &                 
                                        COS_ThetaB     = CosBk, &              
                                        Doppler_Shift  = Doppler_Shift )       
    CALL Compute_Predictors_zssmis_TL(Predictor%PAFV%Temperature, &  
                                      Be,                         &            
                                      CosBK,                      &            
                                      Doppler_Shift,              &            
                                      Predictor%Secant_Zenith,    &     
                                      Temperature_TL,             &     
                                      Predictor_TL)                            

    
  END SUBROUTINE ZSSMIS_Compute_Predictors_TL     

!--------------------------------------------------------------------------------
!
! NAME:
!       ZSSMIS_Predictors_AD
!
! PURPOSE:
!       Subroutine to calculate the gas absorption model predictors. It first
!       Interpolates the user temperature and absorber profiles on the
!       internal pressure grids and then call the predictor computation
!       routine to compute the predictors
!
! CALLING SEQUENCE:
!       CALL ZSSMIS_Predictors_AD ( SensorInput      &  ! Input
!                                   TC,              &  ! Input                         
!                                   Atm,             &  ! Input                         
!                                   GeoInfo,         &  ! Input                         
!                                   Predictor,       &  ! Input                         
!                                   Predictor_AD,    &  ! Input                         
!                                   Atm_AD)             ! Output                        
!
! INPUT ARGUMENTS:
!     SensorInput :     Structure holding sensor specific user inputs
!                        UNITS:      N/A
!                        TYPE:       SensorInput_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!          TC:           ODPS structure holding tau coefficients
!                        UNITS:      N/A
!                        TYPE:       ODPS_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Atm       :     CRTM Atmosphere structure containing the atmospheric
!                       state data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Atmosphere_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       GeoInfo     :   CRTM_GeometryInfo structure containing the 
!                       view geometry information.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_GeometryInfo_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Predictor:      Predictor structure containing the integrated absorber
!                       and predictor profiles.
!                       UNITS:      N/A
!                       TYPE:       TYPE(Predictor_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Predictor_AD:   Predictor structure containing the integrated absorber
!                       and predictor profiles.
!                       UNITS:      N/A
!                       TYPE:       TYPE(Predictor_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!
!       Atm_AD    :     CRTM Atmosphere structure containing the atmospheric
!                       state data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Atmosphere_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE ZSSMIS_Compute_Predictors_AD(SensorInput,    &
                                          TC,             & 
                                          Atm,            &  
                                          GeoInfo,        & 
                                          Predictor,      & 
                                          Predictor_AD,   & 
                                          Atm_AD)           

    TYPE(CRTM_SensorInput_type)  , INTENT(IN)     :: SensorInput
    TYPE(ODPS_type)              , INTENT(IN)     :: TC
    TYPE(CRTM_Atmosphere_type)   , INTENT(IN)     :: Atm
    TYPE(CRTM_GeometryInfo_type) , INTENT(IN)     :: GeoInfo
    TYPE(Predictor_type)         , INTENT(IN)     :: Predictor
    TYPE(Predictor_type)         , INTENT(IN OUT) :: predictor_AD
    TYPE(CRTM_Atmosphere_type)   , INTENT(IN OUT) :: Atm_AD

    ! Local variables
    REAL(fp) :: Absorber_AD(Predictor%n_Layers, TC%n_Absorbers)
    REAL(fp) :: Temperature_AD(Predictor%n_Layers)
    ! SSMIS SensorInput data
    REAL(fp) :: Be, CosBK, Doppler_Shift

    ! initialization
    Temperature_AD = ZERO
    Absorber_AD    = ZERO
     
    !-----------------------------------------------------------
    ! adjoint part
    !-----------------------------------------------------------

    !-------------------------------------------
    ! Compute predictor
    !-------------------------------------------
      CALL CRTM_SensorInput_Get_Property( SensorInput%SSMIS, &
                                          Field_Strength = Be, &
                                          COS_ThetaB     = CosBk, &
                                          Doppler_Shift  = Doppler_Shift )
      CALL Compute_Predictors_zssmis_AD(Predictor%PAFV%Temperature, &
                                        Be,                         &
                                        CosBK,                      & 
                                        Doppler_Shift,              & 
                                        Predictor%Secant_Zenith,    &
                                        Predictor_AD,               &
                                        Temperature_AD )

    !------------------------------------------------------------------
    ! Mapping data from user to internal fixed pressure layers/levels.
    !------------------------------------------------------------------
    CALL Map_Input_AD(Atm,            & ! Input
                      TC,             & ! Input
                      Temperature_AD, & ! Input  
                      Absorber_AD,    & ! Input
                      Atm_AD,         & ! output
                      Predictor%PAFV)   ! Input
         
  END SUBROUTINE ZSSMIS_Compute_Predictors_AD

  FUNCTION Is_ZSSMIS_Channel(TC, ChannelIndex) RESULT( ZChannel )
    TYPE(ODPS_type), INTENT(IN)     :: TC
    INTEGER,         INTENT(IN)     :: ChannelIndex
    
    LOGICAL :: ZChannel
    
    ZChannel = ( TC%Group_Index == ODPS_gINDEX_SSMIS .AND. ZSSMIS_ChannelMap(ChannelIndex) > 0 )
    
  END FUNCTION Is_ZSSMIS_Channel
  
  FUNCTION Is_ZSSMIS(TC) RESULT( ZSSMIS )
    TYPE(ODPS_type), INTENT(IN)     :: TC
    
    LOGICAL :: ZSSMIS
    
    ZSSMIS = ( TC%Group_Index == ODPS_gINDEX_SSMIS )
    
  END FUNCTION Is_ZSSMIS

END MODULE ODZeeman_AtmAbsorption
