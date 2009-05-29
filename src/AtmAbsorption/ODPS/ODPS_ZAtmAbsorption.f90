!
! ODPS_ZAtmAbsorption
!
! Module containing routines to compute the optical depth profile
! due to gaseous absorption and affected by Zeeman spilitting in 
! the Optical Depth Pressure Space (ODPS).
!
!
! CREATION HISTORY:
!       Written by:     Yong Han, JCSDA, NOAA/NESDIS 20-Sep-2008
!

MODULE ODPS_ZAtmAbsorption

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,                ONLY : fp
  USE Message_Handler,           ONLY : SUCCESS, FAILURE, Display_Message
  USE ODPS_Predictor_Define,     ONLY : Predictor_type
  USE ODPS_Define,               ONLY : ODPS_type

  ! Disable implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Public routines
  PUBLIC :: Compute_Predictors_zssmis
  PUBLIC :: Compute_Predictors_zssmis_TL
  PUBLIC :: Compute_Predictors_zssmis_AD
  PUBLIC :: Compute_ODPath_zssmis
  PUBLIC :: Compute_ODPath_zssmis_TL
  PUBLIC :: Compute_ODPath_zssmis_AD

  INTEGER, PUBLIC, PARAMETER  :: N_ZCOMPONENTS = 1
  ! set this variable to 1 although it should be 1 to consistant with the ODPS structure (for
  ! Zeeman applications only O2 need to be considered, which will be include as a fixed gas, decribed
  ! by pressure and temperature. 
  INTEGER, PARAMETER          :: N_ZABSORBERS  = 1
  
  INTEGER, PUBLIC, PARAMETER  :: MAX_N_PREDICTORS_ZSSMIS = 18

  ! Four SSMIS upper air sounding channels affected by Zeeman splitting
  INTEGER, PARAMETER  :: N_CHANNELS_ZSSMIS = 4
  INTEGER, PARAMETER  :: CHANNEL_INDEX_SSMIS(N_CHANNELS_ZSSMIS) = &
                         (/19, 20, 21, 22/)  

  ! Predictor Mask: 1 - included; 0 - not included
  INTEGER, PUBLIC, PARAMETER, DIMENSION(MAX_N_PREDICTORS_ZSSMIS, N_CHANNELS_ZSSMIS) :: &
                      PREDICTOR_MASK = RESHAPE( (/& 
      ! 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18
        1,1,1,1,0,1,1,1,1,1, 0, 1, 1, 1, 0, 0, 0, 0, &
        1,1,1,1,1,1,1,1,1,1, 1, 1, 1, 0, 0, 0, 0, 0, &
        1,1,0,0,0,0,1,1,0,0, 0, 0, 1, 0, 1, 1, 1, 1, &
        1,1,0,0,0,0,1,1,0,0, 0, 0, 0, 0, 1, 1, 1, 1/), &
        (/MAX_N_PREDICTORS_ZSSMIS, N_CHANNELS_ZSSMIS/) )
        
   REAL(fp), PARAMETER :: ZERO = 0.0_fp, ONE = 1.0_fp

CONTAINS

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

!------------------------------------------------------------------------------
!
! NAME:
!       Compute_Predictors_zssmis
!
! PURPOSE:
!       Subroutine to compute predictors for the ZSSMIS sensor (a virtual sensor
!       created for SSMIS Zeeman channels 19, 20, 21 and 22
!
! CALLING SEQUENCE:
!       CALL  Compute_Predictors_zssmis(Temperature,   &
!                                       Be,            &
!                                       CosBK,         &
!                                       Doppler_Shift, &
!                                       Secang,        &
!                                       Predictor )
!
! INPUT ARGUMENTS:
!
!       Temperature:     Temperature profile
!                        UNITS:      K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array  
!                        ATTRIBUTES: INTENT(IN)
!
!         Be    :        Earth magnetic filed strength
!                        UNITS:      Guass (range 0.2 - 0.7)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  scalar
!                        ATTRIBUTES: INTENT(IN)
!
!         CosBK :        Cosine of the angle between the Earth magnetic
!                        field and the electromagnetic wave propagation
!                        direction
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Doppler_Shift :  Doppler shift due Earth ratation, positive when
!                        the emitter moves towards the sensor
!                        UNITS:      KHz
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!             Secang  :  Secant of the zenith angle profile
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array  
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Predictor:      Predictor structure
!                       UNITS:      N/A
!                       TYPE:       TYPE(Predictor_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!------------------------------------------------------------------------------
      
  SUBROUTINE Compute_Predictors_zssmis(Temperature,   &
                                       Be,            &
                                       CosBK,         &
                                       Doppler_Shift, &
                                       Secang,        &
                                       Predictor )
    REAL(fp), INTENT( IN ) :: Temperature(:)
    REAL(fp), INTENT( IN ) :: Be
    REAL(fp), INTENT( IN ) :: CosBK
    REAL(fp), INTENT( IN ) :: Doppler_Shift
    REAL(fp), INTENT( IN ) :: Secang(:)
    TYPE(Predictor_type), INTENT( INOUT ) :: Predictor
    
    ! LOCAL 
    REAL(fp) :: Be2, Be3, OneOverBe, OneOverBe2, CosBK2, OneOverT
    INTEGER  :: k
   
    Be2         = Be  * Be
    Be3         = Be2 * Be
    OneOverBe   = ONE / Be
    OneOverBe2  = OneOverBe * OneOverBe 
    CosBK2     = CosBK * CosBK 
    
    DO k = 1, Predictor%n_Layers   
      ! Predictors associated Temperature  
      OneOverT = 300.0_fp / Temperature(k)                                    
      Predictor%X(k, 1,  1)  = OneOverT                ! 1/T                   
      Predictor%X(k, 2,  1)  = OneOverT * CosBK2       ! COS(BK_ang)**2 / T    
      Predictor%X(k, 3,  1)  = OneOverT / Be           ! 1 / (T*Be)            
      Predictor%X(k, 4,  1)  = OneOverT * CosBK / Be   ! COS(BK_ang) / (T*Be)  
      Predictor%X(k, 5,  1)  = OneOverT * CosBK / Be2  ! COS(BK_ang) / (T*Be**2)  
    END DO
    
    ! Constant predictor                                                        
    Predictor%X(:, 6,  1)  = ONE                       ! 1  - constant term
    Predictor%X(:, 7,  1)  = CosBK                     ! COS(BK_ang)              
    Predictor%X(:, 8,  1)  = CosBK2                    ! COS(BK_ang)**2           
    Predictor%X(:, 9,  1)  = OneOverBe                 ! 1 / Be                   
    Predictor%X(:, 10, 1)  = OneOverBe2                ! 1 / Be**2                
    Predictor%X(:, 11, 1)  = OneOverBe2 * OneOverBe    ! 1 / Be^3                 
    Predictor%X(:, 12, 1)  = CosBK * Be2               ! COS(BK_ang) * Be**2      
    Predictor%X(:, 13, 1)  = CosBK2 * Be2              ! (COS(BK_ang) * Be)**2    
    Predictor%X(:, 14, 1)  = CosBK * Be3               ! COS(BK_ang) * Be**3 
    Predictor%X(:, 15, 1)  = Be                        ! Be
    Predictor%X(:, 16, 1)  = Be3                       ! Be**3
    Predictor%X(:, 17, 1)  = CosBK  * Be               ! COS(BK_ang)*Be
    Predictor%X(:, 18, 1)  = CosBK2 * Be               ! COS(BK_ang)**2*Be
    
    Predictor%n_CP(1) = MAX_N_PREDICTORS_ZSSMIS 

    ! pass Doppler shift and Secant angle to Predicotr structre
    Predictor%u = Doppler_Shift   
    Predictor%Secant_Zenith = Secang                      
   
  END SUBROUTINE Compute_Predictors_zssmis    

!------------------------------------------------------------------------------
!
! NAME:
!       Compute_Predictors_zssmis_TL
!
! PURPOSE:
!       Subroutine to compute TL predictors for the ZSSMIS sensor (a virtual sensor
!       created for SSMIS Zeeman channels 19, 20, 21 and 22)
!
! CALLING SEQUENCE:
!
!       CALL Compute_Predictors_zssmis_TL(Temperature,   &
!                                         Be,            & 
!                                         CosBK,         & 
!                                         Doppler_Shift, &  
!                                         Secang,        &  
!                                         Temperature_TL,& 
!                                         Predictor_TL )   
! INPUT ARGUMENTS:
!
!       Temperature:     Temperature profile
!                        UNITS:      K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array  
!                        ATTRIBUTES: INTENT(IN)
!
!         Be    :        Earth magnetic filed strength
!                        UNITS:      Guass (range 0.2 - 0.7)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  scalar
!                        ATTRIBUTES: INTENT(IN)
!
!         CosBK :        Cosine of the angle between the Earth magnetic
!                        field and the electromagnetic wave propagation
!                        direction
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Doppler_Shift :  Doppler shift due Earth ratation, positive when
!                        the emitter moves towards the sensor
!                        UNITS:      KHz
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!             Secang  :  Secant of the zenith angle profile
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array  
!                        ATTRIBUTES: INTENT(IN)
!
!       Temperature_TL:  TL Temperature profile
!                        UNITS:      K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array  
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Predictor_TL:   TL Predictor structure
!                       UNITS:      N/A
!                       TYPE:       TYPE(Predictor_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!------------------------------------------------------------------------------

  SUBROUTINE Compute_Predictors_zssmis_TL(Temperature,   &
                                          Be,            &
                                          CosBK,         &
                                          Doppler_Shift, & 
                                          Secang,        & 
                                          Temperature_TL,&
                                          Predictor_TL )
    REAL(fp), INTENT( IN ) :: Temperature(:)
    REAL(fp), INTENT( IN ) :: Be
    REAL(fp), INTENT( IN ) :: CosBK
    REAL(fp), INTENT( IN ) :: Doppler_Shift
    REAL(fp), INTENT( IN ) :: Secang(:)
    REAL(fp), INTENT( IN ) :: Temperature_TL(:)
    TYPE(Predictor_type), INTENT( INOUT ) :: Predictor_TL
    
    ! LOCAL 
    REAL(fp) :: Be2, CosBK2, OneOverT, OneOverT_TL
    INTEGER  :: k
    
    Be2         = Be  * Be
    CosBK2     = CosBK * CosBK 
    
    DO k = 1, Predictor_TL%n_Layers   
      ! Predictors associated Temperature  
      OneOverT = 300.0_fp / Temperature(k)      
      OneOverT_TL = -(OneOverT / Temperature(k))*Temperature_TL(k)                                   
      Predictor_TL%X(k, 1,  1)  = OneOverT_TL                
      Predictor_TL%X(k, 2,  1)  = OneOverT_TL * CosBK2       
      Predictor_TL%X(k, 3,  1)  = OneOverT_TL / Be           
      Predictor_TL%X(k, 4,  1)  = OneOverT_TL * CosBK / Be   
      Predictor_TL%X(k, 5,  1)  = OneOverT_TL * CosBK / Be2   
    END DO
    
    ! Constant predictor
    Predictor_TL%X(:, 6:18,  1)  = ZERO                                                        
    
    Predictor_TL%n_CP(1) = MAX_N_PREDICTORS_ZSSMIS 
    
  END SUBROUTINE Compute_Predictors_zssmis_TL    

!------------------------------------------------------------------------------
!
! NAME:
!       Compute_Predictors_zssmis_AD
!
! PURPOSE:
!       Subroutine to compute AD predictors for the ZSSMIS sensor (a virtual sensor
!       created for SSMIS Zeeman channels 19, 20, 21 and 22)
!
! CALLING SEQUENCE:
!
!        CALL Compute_Predictors_zssmis_AD(Temperature,   &
!                                          Be,            &
!                                          CosBK,         &
!                                          Doppler_Shift, & 
!                                          Secang,        & 
!                                          Predictor_AD,  &
!                                          Temperature_AD )
! INPUT ARGUMENTS:
!
!       Temperature:     Temperature profile
!                        UNITS:      K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array  
!                        ATTRIBUTES: INTENT(IN)
!
!         Be    :        Earth magnetic filed strength
!                        UNITS:      Guass (range 0.2 - 0.7)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  scalar
!                        ATTRIBUTES: INTENT(IN)
!
!         CosBK :        Cosine of the angle between the Earth magnetic
!                        field and the electromagnetic wave propagation
!                        direction
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Doppler_Shift :  Doppler shift due Earth ratation, positive when
!                        the emitter moves towards the sensor
!                        UNITS:      KHz
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!             Secang  :  Secant of the zenith angle profile
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array  
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor_AD:    AD Predictor structure
!                        UNITS:      N/A
!                        TYPE:       TYPE(Predictor_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(INOUT)
!
! OUTPUT ARGUMENTS:
!       Temperature_AD:  AD Temperature profile
!                        UNITS:      K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array  
!                        ATTRIBUTES: INTENT(INOUT)
!
!------------------------------------------------------------------------------
  SUBROUTINE Compute_Predictors_zssmis_AD(Temperature,   &
                                          Be,            &
                                          CosBK,         &
                                          Doppler_Shift, & 
                                          Secang,        & 
                                          Predictor_AD,  &
                                          Temperature_AD )
    REAL(fp), INTENT( IN ) :: Temperature(:)
    REAL(fp), INTENT( IN ) :: Be
    REAL(fp), INTENT( IN ) :: CosBK
    REAL(fp), INTENT( IN ) :: Doppler_Shift
    REAL(fp), INTENT( IN ) :: Secang(:)
    TYPE(Predictor_type), INTENT( INOUT ) :: Predictor_AD
    REAL(fp), INTENT( INOUT ) :: Temperature_AD(:)
    
    ! LOCAL 
    REAL(fp) :: Be2, CosBK2, OneOverT, OneOverT_AD
    INTEGER  :: k
    
    Be2         = Be  * Be
    CosBK2     = CosBK * CosBK 
    
    DO k = 1, Predictor_AD%n_Layers   
      ! Predictors associated Temperature  
      OneOverT = 300.0_fp / Temperature(k)
      
      OneOverT_AD = Predictor_AD%X(k, 1,  1) + &
                    Predictor_AD%X(k, 2,  1)*CosBK2 + &
                    Predictor_AD%X(k, 3,  1) / Be + &
                    Predictor_AD%X(k, 4,  1) * CosBK / Be + &
                    Predictor_AD%X(k, 5,  1) * CosBK / Be2
      Temperature_AD(k) = Temperature_AD(k) - &
                     (OneOverT / Temperature(k)) * OneOverT_AD      
    END DO
    
    ! Constant predictor
    Predictor_AD%X(:, :,  1)  = ZERO                                                        
    
    Predictor_AD%n_CP(1) = MAX_N_PREDICTORS_ZSSMIS 
    
  END SUBROUTINE Compute_Predictors_zssmis_AD    
                                                                                  
END MODULE ODPS_ZAtmAbsorption
