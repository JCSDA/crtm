!------------------------------------------------------------------------------
!M+
! NAME:
!       ODCAPS_AtmAbsorption
!
! PURPOSE:
!       Module containing routines to compute the optical depth profile
!       due to gaseous absorption.
!
! CATEGORY:
!       CRTM : Gas Absorption
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE ODCAPS_AtmAbsorption
!
! MODULES:
!       Type_Kinds:                      Module containing definitions for kinds
!                                        of variable types.
!
!       Message_Handler:                   Module to define simple error codes and
!                                        handle error conditions
!                                        USEs: FILE_UTILITY module
!
!       CRTM_Parameters:                 Module of parameter definitions for the CRTM.
!                                        USEs: TYPE_KINDS module
!
!       ODCAPS_TauCoeff:                 Module containing the shared CRTM gas
!                                        absorption coefficients (ODCAPS_TauCoeff)
!                                        and their load/destruction routines. 
!                                        USEs TYPE_KINDS module
!                                             Message_Handler module
!                                              ODCAPS_DEFINE module
!                                              ODCAPS_BINARY_IO module
!                                             CRTM_PARAMETERS module
!
!       CRTM_GeometryInfo_Define:        Module defining the CRTM GeometryInfo
!                                        structure and containing routines to 
!                                        manipulate it.
!                                        USEs: TYPE_KINDS module
!                                              Message_Handler module
!
!       ODCAPS_Predictor_Define:       Module defining the ODCAPS_Predictor
!                                        structure and containing routines to 
!                                        manipulate it.
!                                        USEs: TYPE_KINDS module
!                                              Message_Handler module
!
!
!       ODCAPS_Predictor:    Module containing the routines to compute
!                                        the predictor profiles for the CRTM gas
!                                        absorption model.
!                                        USEs: TYPE_KINDS module
!                                              Message_Handler module
!                                              CRTM_PARAMETERS module
!                                              CRTM_ATMOSPHERE_DEFINE module
!                                              ODCAPS_Predictor_DEFINE module
!
!       CRTM_Interpolation:             Module containing routines for profile interpolation
!
! CONTAINS:
!       PUBLIC subprograms
!       ------------------
!       Compute_AtmAbsorption:     SUBROUTINE to calculate the layer optical
!                                        depths due to gaseous absorption for a given
!                                        input atmospheric profile for a single channel.
!
!       Compute_AtmAbsorption_TL:  SUBROUTINE to calculate the tangent-linear
!                                        layer optical depths due to gaseous absorption
!                                        for a given input atmospheric profile for a
!                                        single channel.
!
!       Compute_AtmAbsorption_AD:  SUBROUTINE to calculate the layer optical depths
!                                        adjoint due to gaseous absorption for a given
!                                        input atmospheric profile for a single channel.
!
!       PRIVATE subprograms
!       -------------------
!       None.
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
!       Written by:     Yong Chen, CSU/CIRA 24-May-2006
!                       Yong.Chen@noaa.gov
!
!  Copyright (C) 2006 Yong Chen
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

MODULE ODCAPS_AtmAbsorption


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds,                      ONLY: fp=>fp_kind
  USE Message_Handler
  USE CRTM_GeometryInfo

  ! -- CRTM modules
  USE CRTM_Parameters
  USE ODCAPS_TauCoeff,                   ONLY: ODCAPS_TC =>TC

  USE ODCAPS_Define,                     ONLY: ODCAPS_TauCoeff_type    => ODCAPS_type

  USE CRTM_GeometryInfo_Define, ONLY: CRTM_GeometryInfo_type

  USE CRTM_AtmScatter_Define,    ONLY: CRTM_AtmAbsorption_type => CRTM_AtmScatter_type

  ! -- The AtmAbsorption structure definition module
  ! -- The PUBLIC entities in ODCAPS_Predictor_Define
  ! -- are also explicitly defined as PUBLIC here so 
  ! -- a user need only USE ODCAPS_Predictor_Define.
  USE ODCAPS_Predictor_Define

  ! -- The AtmAbsorption algorithm support modules
  USE ODCAPS_Predictor

  USE CRTM_Profile_Interpolation, ONLY: Interpolate_Profile,    &
                                        Interpolate_Profile_TL, &
                                        Interpolate_Profile_AD
  
  
  
  
  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  ! -- Everything private by default
  PRIVATE

  PUBLIC :: CRTM_AtmAbsorption_type
  ! CRTM_AtmAbsorption structure routines inherited
  ! from the CRTM_AtmAbsorption_Define module
  PUBLIC :: CRTM_Associated_AtmAbsorption
  PUBLIC :: CRTM_Destroy_AtmAbsorption
  PUBLIC :: CRTM_Allocate_AtmAbsorption
  PUBLIC :: CRTM_Assign_AtmAbsorption

  ! Science routines in this modules
  PUBLIC :: Compute_AtmAbsorption
  PUBLIC :: Compute_AtmAbsorption_TL
  PUBLIC :: Compute_AtmAbsorption_AD
  ! Internal variable structure
  PUBLIC :: AAVariables_type
  ! -------------------------

  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &

  ! -- Keyword set value
  !INTEGER, PRIVATE, PARAMETER :: SET = 1

  ! -- The maximum number of subset  
!  INTEGER, PRIVATE, PARAMETER :: MAX_N_SUBSETS = 7

  ! -- The maximum number of layers for ODCAPS 
!  INTEGER, PRIVATE, PARAMETER :: MAX_N_ODCAPS_LAYERS = 100   

  ! ------------------------------------------
  ! Structure definition to hold forward model
  ! variables across FWD, TL, and AD calls
  ! ------------------------------------------
  TYPE :: AAVariables_type
    PRIVATE
  END TYPE AAVariables_type

CONTAINS

!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PRIVATE MODULE ROUTINES ##                      ##
!##                                                                            ##
!################################################################################
!################################################################################
 
  !#------------------------------------------------------------------------------#
  !#       Subroutine to calculate the layer optical depths due to gaseous	  #
  !#       absorption for a given input atmospheric profile for a single	  #
  !#       channel.								  #
  !#------------------------------------------------------------------------------#
 
   
  SUBROUTINE Compute_Optical_Depth_Subset(  Sensor_Index,            & ! Input
                                            Channel_Subset_Index,    & ! Input
                                            Channel_Subset_Position, & ! Input
                                            Predictor,               & ! In/Output
					    Do_Sun_Calc)               ! Optional Input 
                                         
    ! ---------
    ! Arguments
    ! ---------
    ! -- Inputs
    INTEGER,  INTENT( IN )     ::  Sensor_Index 
    INTEGER,  INTENT( IN )     ::  Channel_Subset_Index 
    INTEGER,  INTENT( IN )     ::  Channel_Subset_Position 

    ! -- In/Outputs
    TYPE( Predictor_type ), INTENT( IN OUT ) :: Predictor

    ! -- Optional Inputs
    INTEGER,               OPTIONAL, INTENT( IN )     :: Do_Sun_Calc 

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Optical_Depth_Subset'

    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER ::      I   
    INTEGER ::	    J  
    INTEGER ::	    M  
    INTEGER ::      K 
    INTEGER ::   ICO2   
    INTEGER ::  IHNO3   
    INTEGER ::   IN2O   
    INTEGER ::   ISO2   
      
    REAL( fp ) ::   KCON      
    REAL( fp ) ::   KFIX 
    REAL( fp ) ::   KOZO      
    REAL( fp ) ::   KMET 
    REAL( fp ) ::    KCO 
    REAL( fp ) ::     DK      
    REAL( fp ) ::  DKCO2      
    REAL( fp ) :: DKHNO3      
    REAL( fp ) ::  DKN2O      
    REAL( fp ) ::  DKSO2      
    REAL( fp ) :: KLAYER      
    REAL( fp ) ::     KZ      

    LOGICAL ::  LCO2   
    LOGICAL ::  LH2O   
    LOGICAL :: LHNO3   
    LOGICAL ::  LN2O   
    LOGICAL ::  LSO2   
 
  ! for CALOKW
    INTEGER ::  IH2O
    REAL( fp), DIMENSION(Predictor%n_Layers) :: KW

    LOGICAL :: Cal_Sun
    REAL( fp ), DIMENSION(4, MAX_N_SUBSET_TOTAL_PREDICTORS, Predictor%n_Layers) :: Predictor_Subset
    REAL( fp ) ::    XZ 
    REAL( fp ), DIMENSION(MAX_N_TRACEGASES_PREDICTORS,Predictor%n_Layers) :: TraceGas_Predictors
    INTEGER :: idx
    TYPE( ODCAPS_TauCoeff_type ), POINTER  ::  TC => NULL()

    TC => ODCAPS_TC(Sensor_Index)
 

    Cal_Sun = .TRUE.                                            

    M = Channel_Subset_Index  
   
    I = Channel_Subset_Position 

    idx = MINLOC(ABS(TC%Sensor_Channel - TC%ODCAPS_Subset(M)%Channel_Index(I) ), DIM =1 )
!    write(65,*) 'Subset_Index: ', M, ', Subset_Position: ', I, ', Channel location:', idx

    J = idx    !TC%Sensor_Channel( TC%ODCAPS_Subset(M)%Channel_Index(I) )

  ! Determine whether or not to do variable CO2 calc     
    ICO2 = TC%Channel_CO2_Perturbation( idx )			         
    IF (ICO2 > 0) THEN			         
       LCO2=.TRUE.				         
    ELSE					         
       LCO2=.FALSE.				         
    ENDIF					         

  ! Determine whether or not to do variable SO2 calc     
    ISO2 = TC%Channel_SO2_Perturbation( idx  )			         
    IF (ISO2 > 0) THEN			         
       LSO2=.TRUE.				         
    ELSE					         
       LSO2=.FALSE.				         
    ENDIF					         

  ! Determine whether or not to do variable HNO3 calc    
    IHNO3 = TC%Channel_HNO3_Perturbation( idx  )
    IF (IHNO3 > 0) THEN			         
       LHNO3=.TRUE.				         
    ELSE					         
       LHNO3=.FALSE.				         
    ENDIF					         

  ! Determine whether or not to do variable N2O calc     
    IN2O = TC%Channel_N2O_Perturbation( idx )   		         
    IF (IN2O > 0) THEN  			       
       LN2O=.TRUE.				       
    ELSE	        			       
       LN2O=.FALSE.				       
    ENDIF	        			       

  !  -------------------------
  !  Do OPTRAN water if needed
  !  -------------------------
    IH2O = TC%Channel_H2O_OPTRAN( idx  ) 
    IF (IH2O > 0) THEN					     
       LH2O=.FALSE.						     
  
  !    Calc OPTRAN water 					     
       CALL Compute_WOPTRAN_Optics(Sensor_Index, IH2O, Predictor, KW)		     

    ELSE							     
       LH2O=.TRUE.						     
    ENDIF							     


    IF ( PRESENT (Do_Sun_Calc) .AND. &  					     
    	 Predictor%Calc_Sun_Angle_Secant) THEN				    
      Predictor_Subset(1:4, :, :) = Predictor%Predictor_Subset_Sun(1:4, :, :)  
      TraceGas_Predictors = Predictor%TraceGas_Predictors_Sun
      XZ = Predictor%Sun_Fudge 
      Cal_Sun = .TRUE.  							    
    ELSE									     
      Predictor_Subset(1:4, :, :) = Predictor%Predictor_Subset(4:7, :, :)	  
      TraceGas_Predictors = Predictor%TraceGas_Predictors
      XZ = ONE  
      Cal_Sun = .FALSE. 							    
    END IF									     

  ! Initialize the layer-to-space optical depth
    KZ = ZERO
  ! KZFW = ZERO

    !#------------------------------------------------------------------------#  
    !#  		  -- BEGIN LOOP OVER LAYERS --  		      #  
    !#------------------------------------------------------------------------#  

    Layer_Loop: DO K = 1, Predictor%n_Layers				 

 
       Subset_Select: SELECT CASE ( M )
 
	   CASE ( 1 )
	   
       !    --------------------------- 		   
       !    Compute the water continuum 		   
       !    --------------------------- 		   
            KCON = TC%Tuning_Multiple(3,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(1,K,I) * Predictor%Predictor_Subset(M,1,K) &
        	   + TC%ODCAPS_Subset(M)%C(2,K,I) * Predictor%Predictor_Subset(M,2,K) &
        	   + TC%ODCAPS_Subset(M)%C(3,K,I) * Predictor%Predictor_Subset(M,3,K) &
        	   + TC%ODCAPS_Subset(M)%C(4,K,I) * Predictor%Predictor_Subset(M,4,K) &
        	   + TC%ODCAPS_Subset(M)%C(5,K,I) * Predictor%Predictor_Subset(M,5,K) &
        	   + TC%ODCAPS_Subset(M)%C(6,K,I) * Predictor%Predictor_Subset(M,6,K) &
        	   + TC%ODCAPS_Subset(M)%C(7,K,I) * Predictor%Predictor_Subset(M,7,K) )
        		 
            IF ( KCON < ZERO ) THEN		   
               KCON = ZERO				   
            ELSE IF ( KCON > TEN ) THEN 		   
               KCON = TEN				   
            ENDIF					   
  
       !    -----------------------------
       !    Calc the fixed gases abs coef
       !    -----------------------------
            KFIX = TC%Tuning_Multiple(1,J) &
        	 * ( TC%ODCAPS_Subset(M)%C( 8,K,I) * Predictor%Predictor_Subset(M, 8,K) &
        	   + TC%ODCAPS_Subset(M)%C( 9,K,I) * Predictor%Predictor_Subset(M, 9,K) &
        	   + TC%ODCAPS_Subset(M)%C(10,K,I) * Predictor%Predictor_Subset(M,10,K) &
        	   + TC%ODCAPS_Subset(M)%C(11,K,I) * Predictor%Predictor_Subset(M,11,K) &
        	   + TC%ODCAPS_Subset(M)%C(12,K,I) * Predictor%Predictor_Subset(M,12,K) &
        	   + TC%ODCAPS_Subset(M)%C(13,K,I) * Predictor%Predictor_Subset(M,13,K) &
        	   + TC%ODCAPS_Subset(M)%C(14,K,I) * Predictor%Predictor_Subset(M,14,K) &
        	   + TC%ODCAPS_Subset(M)%C(15,K,I) * Predictor%Predictor_Subset(M,15,K) )
 
            KFIX = KFIX * Predictor%Fix_Amount_Multiplier( K ) 
 
            IF ( KFIX < ZERO ) THEN	     
               KFIX = ZERO			
            ELSE IF ( KFIX > TEN ) THEN      
               KFIX = TEN			
            ENDIF				

       !    --------------------------
       !    Compute the water abs coef
       !    --------------------------
            IF (LH2O) THEN
       !    Not an OPTRAN water channel
            
              KW( K ) = TC%Tuning_Multiple(2,J) &
        	    * ( TC%ODCAPS_Subset(M)%C(16,K,I) * Predictor%Predictor_Subset(M,16,K) &
        	      + TC%ODCAPS_Subset(M)%C(17,K,I) * Predictor%Predictor_Subset(M,17,K) &
        	      + TC%ODCAPS_Subset(M)%C(18,K,I) * Predictor%Predictor_Subset(M,18,K) &
        	      + TC%ODCAPS_Subset(M)%C(19,K,I) * Predictor%Predictor_Subset(M,19,K) &
        	      + TC%ODCAPS_Subset(M)%C(20,K,I) * Predictor%Predictor_Subset(M,20,K) &
        	      + TC%ODCAPS_Subset(M)%C(21,K,I) * Predictor%Predictor_Subset(M,21,K) &
        	      + TC%ODCAPS_Subset(M)%C(22,K,I) * Predictor%Predictor_Subset(M,22,K) &
        	      + TC%ODCAPS_Subset(M)%C(23,K,I) * Predictor%Predictor_Subset(M,23,K) &
        	      + TC%ODCAPS_Subset(M)%C(24,K,I) * Predictor%Predictor_Subset(M,24,K) &
        	      + TC%ODCAPS_Subset(M)%C(25,K,I) * Predictor%Predictor_Subset(M,25,K) &
        	      + TC%ODCAPS_Subset(M)%C(26,K,I) * Predictor%Predictor_Subset(M,26,K) )
 
              IF ( KW( K ) < ZERO ) KW( K ) = ZERO
            ENDIF

       !    --------------------------
       !    Compute the ozone abs coef
       !    --------------------------
            KOZO = TC%Tuning_Multiple(4,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(27,K,I) * Predictor%Predictor_Subset(M,27,K) &   
        	   + TC%ODCAPS_Subset(M)%C(28,K,I) * Predictor%Predictor_Subset(M,28,K) &   
        	   + TC%ODCAPS_Subset(M)%C(29,K,I) * Predictor%Predictor_Subset(M,29,K) &   
        	   + TC%ODCAPS_Subset(M)%C(30,K,I) * Predictor%Predictor_Subset(M,30,K) &   
        	   + TC%ODCAPS_Subset(M)%C(31,K,I) * Predictor%Predictor_Subset(M,31,K) )  
 
            IF ( KOZO < ZERO ) THEN
               KOZO = ZERO
            ELSE IF ( KOZO > TEN ) THEN
               KOZO = TEN
            ENDIF
      
       !    --------------------------
       !    The other gases abs coef
       !    --------------------------
      	    KCO  = ZERO
	    KMET = ZERO

	   CASE ( 2 )
	   
       !    --------------------------- 		   
       !    Compute the water continuum 		   
       !    --------------------------- 		   
            KCON = TC%Tuning_Multiple(3,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(1,K,I) * Predictor%Predictor_Subset(M,1,K) &
        	   + TC%ODCAPS_Subset(M)%C(2,K,I) * Predictor%Predictor_Subset(M,2,K) &
        	   + TC%ODCAPS_Subset(M)%C(3,K,I) * Predictor%Predictor_Subset(M,3,K) &
        	   + TC%ODCAPS_Subset(M)%C(4,K,I) * Predictor%Predictor_Subset(M,4,K) &
        	   + TC%ODCAPS_Subset(M)%C(5,K,I) * Predictor%Predictor_Subset(M,5,K) &
        	   + TC%ODCAPS_Subset(M)%C(6,K,I) * Predictor%Predictor_Subset(M,6,K) &
        	   + TC%ODCAPS_Subset(M)%C(7,K,I) * Predictor%Predictor_Subset(M,7,K) )
        		 
            IF ( KCON < ZERO ) THEN		   
               KCON = ZERO				   
            ELSE IF ( KCON > TEN ) THEN 		   
               KCON = TEN				   
            ENDIF					   
  
       !    -----------------------------
       !    Calc the fixed gases abs coef
       !    -----------------------------
            KFIX = TC%Tuning_Multiple(1,J) &
        	 * ( TC%ODCAPS_Subset(M)%C( 8,K,I) * Predictor%Predictor_Subset(M, 8,K) &
        	   + TC%ODCAPS_Subset(M)%C( 9,K,I) * Predictor%Predictor_Subset(M, 9,K) &
        	   + TC%ODCAPS_Subset(M)%C(10,K,I) * Predictor%Predictor_Subset(M,10,K) &
        	   + TC%ODCAPS_Subset(M)%C(11,K,I) * Predictor%Predictor_Subset(M,11,K) &
        	   + TC%ODCAPS_Subset(M)%C(12,K,I) * Predictor%Predictor_Subset(M,12,K) &
        	   + TC%ODCAPS_Subset(M)%C(13,K,I) * Predictor%Predictor_Subset(M,13,K) &
        	   + TC%ODCAPS_Subset(M)%C(14,K,I) * Predictor%Predictor_Subset(M,14,K) &
        	   + TC%ODCAPS_Subset(M)%C(15,K,I) * Predictor%Predictor_Subset(M,15,K) )
 
            KFIX = KFIX * Predictor%Fix_Amount_Multiplier( K )

            IF ( KFIX < ZERO ) THEN	     
               KFIX = ZERO			
            ELSE IF ( KFIX > TEN ) THEN      
               KFIX = TEN			
            ENDIF				

       !    --------------------------
       !    Compute the ozone abs coef
       !    --------------------------
            KOZO = TC%Tuning_Multiple(4,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(16,K,I) * Predictor%Predictor_Subset(M,16,K) &   
        	   + TC%ODCAPS_Subset(M)%C(17,K,I) * Predictor%Predictor_Subset(M,17,K) &   
        	   + TC%ODCAPS_Subset(M)%C(18,K,I) * Predictor%Predictor_Subset(M,18,K) &   
        	   + TC%ODCAPS_Subset(M)%C(19,K,I) * Predictor%Predictor_Subset(M,19,K) &   
        	   + TC%ODCAPS_Subset(M)%C(20,K,I) * Predictor%Predictor_Subset(M,20,K) &   
        	   + TC%ODCAPS_Subset(M)%C(21,K,I) * Predictor%Predictor_Subset(M,21,K) &   
        	   + TC%ODCAPS_Subset(M)%C(22,K,I) * Predictor%Predictor_Subset(M,22,K) &   
        	   + TC%ODCAPS_Subset(M)%C(23,K,I) * Predictor%Predictor_Subset(M,23,K) &   
        	   + TC%ODCAPS_Subset(M)%C(24,K,I) * Predictor%Predictor_Subset(M,24,K) &   
        	   + TC%ODCAPS_Subset(M)%C(25,K,I) * Predictor%Predictor_Subset(M,25,K) )   
  
            IF ( KOZO < ZERO ) THEN
               KOZO = ZERO
            ELSE IF ( KOZO > TEN ) THEN
               KOZO = TEN
            ENDIF

       !    --------------------------
       !    Compute the water abs coef
       !    --------------------------
            IF (LH2O) THEN
       !    Not an OPTRAN water channel
            
              KW( K ) = TC%Tuning_Multiple(2,J) &
         	      * ( TC%ODCAPS_Subset(M)%C(26,K,I) * Predictor%Predictor_Subset(M,26,K) & 
        	        + TC%ODCAPS_Subset(M)%C(27,K,I) * Predictor%Predictor_Subset(M,27,K) & 
        	        + TC%ODCAPS_Subset(M)%C(28,K,I) * Predictor%Predictor_Subset(M,28,K) & 
        	        + TC%ODCAPS_Subset(M)%C(29,K,I) * Predictor%Predictor_Subset(M,29,K) & 
        	        + TC%ODCAPS_Subset(M)%C(30,K,I) * Predictor%Predictor_Subset(M,30,K) & 
        	        + TC%ODCAPS_Subset(M)%C(31,K,I) * Predictor%Predictor_Subset(M,31,K) &
        	        + TC%ODCAPS_Subset(M)%C(32,K,I) * Predictor%Predictor_Subset(M,32,K) & 
        	        + TC%ODCAPS_Subset(M)%C(33,K,I) * Predictor%Predictor_Subset(M,33,K) & 
        	        + TC%ODCAPS_Subset(M)%C(34,K,I) * Predictor%Predictor_Subset(M,34,K) & 
        	        + TC%ODCAPS_Subset(M)%C(35,K,I) * Predictor%Predictor_Subset(M,35,K) & 
        	        + TC%ODCAPS_Subset(M)%C(36,K,I) * Predictor%Predictor_Subset(M,36,K) )
 
              IF ( KW( K ) < ZERO ) THEN
	         KW( K ) = ZERO
              ELSE IF( KW( K ) > TEN ) THEN
                 KW( K ) = TEN
	      ENDIF
	      	 
            ENDIF

       !    --------------------------
       !    The other gases abs coef
       !    --------------------------
      	    KCO  = ZERO
	    KMET = ZERO

	   CASE ( 3 )
	   
       !    --------------------------- 		   
       !    Compute the water continuum 		   
       !    --------------------------- 		   
            KCON = TC%Tuning_Multiple(3,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(1,K,I) * Predictor%Predictor_Subset(M,1,K) &
        	   + TC%ODCAPS_Subset(M)%C(2,K,I) * Predictor%Predictor_Subset(M,2,K) &
        	   + TC%ODCAPS_Subset(M)%C(3,K,I) * Predictor%Predictor_Subset(M,3,K) &
        	   + TC%ODCAPS_Subset(M)%C(4,K,I) * Predictor%Predictor_Subset(M,4,K) &
        	   + TC%ODCAPS_Subset(M)%C(5,K,I) * Predictor%Predictor_Subset(M,5,K) &
        	   + TC%ODCAPS_Subset(M)%C(6,K,I) * Predictor%Predictor_Subset(M,6,K) &
        	   + TC%ODCAPS_Subset(M)%C(7,K,I) * Predictor%Predictor_Subset(M,7,K) )
        		 
            IF ( KCON < ZERO ) THEN		   
               KCON = ZERO				   
            ELSE IF ( KCON > TEN ) THEN 		   
               KCON = TEN				   
            ENDIF					   
  
       !    -----------------------------
       !    Calc the fixed gases abs coef
       !    -----------------------------
            KFIX = TC%Tuning_Multiple(1,J) &
        	 * ( TC%ODCAPS_Subset(M)%C( 8,K,I) * Predictor%Predictor_Subset(M, 8,K) &
        	   + TC%ODCAPS_Subset(M)%C( 9,K,I) * Predictor%Predictor_Subset(M, 9,K) &
        	   + TC%ODCAPS_Subset(M)%C(10,K,I) * Predictor%Predictor_Subset(M,10,K) &
        	   + TC%ODCAPS_Subset(M)%C(11,K,I) * Predictor%Predictor_Subset(M,11,K) &
        	   + TC%ODCAPS_Subset(M)%C(12,K,I) * Predictor%Predictor_Subset(M,12,K) &
        	   + TC%ODCAPS_Subset(M)%C(13,K,I) * Predictor%Predictor_Subset(M,13,K) &
        	   + TC%ODCAPS_Subset(M)%C(14,K,I) * Predictor%Predictor_Subset(M,14,K) &
        	   + TC%ODCAPS_Subset(M)%C(15,K,I) * Predictor%Predictor_Subset(M,15,K) )
 
            KFIX = KFIX * Predictor%Fix_Amount_Multiplier( K )

            IF ( KFIX < ZERO ) THEN	     
               KFIX = ZERO			
            ELSE IF ( KFIX > TEN ) THEN      
               KFIX = TEN			
            ENDIF				

 
       !    ----------------------------
       !    Compute the methane abs coef
       !    ----------------------------
 	    KMET = TC%Tuning_Multiple(6,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(16,K,I) * Predictor%Predictor_Subset(M,16,K) &   
        	   + TC%ODCAPS_Subset(M)%C(17,K,I) * Predictor%Predictor_Subset(M,17,K) &   
        	   + TC%ODCAPS_Subset(M)%C(18,K,I) * Predictor%Predictor_Subset(M,18,K) &   
        	   + TC%ODCAPS_Subset(M)%C(19,K,I) * Predictor%Predictor_Subset(M,19,K) &   
        	   + TC%ODCAPS_Subset(M)%C(20,K,I) * Predictor%Predictor_Subset(M,20,K) &   
        	   + TC%ODCAPS_Subset(M)%C(21,K,I) * Predictor%Predictor_Subset(M,21,K) &   
        	   + TC%ODCAPS_Subset(M)%C(22,K,I) * Predictor%Predictor_Subset(M,22,K) &   
        	   + TC%ODCAPS_Subset(M)%C(23,K,I) * Predictor%Predictor_Subset(M,23,K) &   
        	   + TC%ODCAPS_Subset(M)%C(24,K,I) * Predictor%Predictor_Subset(M,24,K) )   
 
             IF ( KMET < ZERO ) THEN
                KMET = ZERO
             ELSEIF ( KMET > TEN ) THEN
                KMET = TEN
             ENDIF
	     
       !    --------------------------
       !    Compute the water abs coef
       !    --------------------------
            IF (LH2O) THEN
       !    Not an OPTRAN water channel
            
              KW( K ) = TC%Tuning_Multiple(2,J) &
         	      * ( TC%ODCAPS_Subset(M)%C(25,K,I) * Predictor%Predictor_Subset(M,25,K) &
		        + TC%ODCAPS_Subset(M)%C(26,K,I) * Predictor%Predictor_Subset(M,26,K) & 
        	        + TC%ODCAPS_Subset(M)%C(27,K,I) * Predictor%Predictor_Subset(M,27,K) & 
        	        + TC%ODCAPS_Subset(M)%C(28,K,I) * Predictor%Predictor_Subset(M,28,K) & 
        	        + TC%ODCAPS_Subset(M)%C(29,K,I) * Predictor%Predictor_Subset(M,29,K) & 
        	        + TC%ODCAPS_Subset(M)%C(30,K,I) * Predictor%Predictor_Subset(M,30,K) & 
        	        + TC%ODCAPS_Subset(M)%C(31,K,I) * Predictor%Predictor_Subset(M,31,K) &
        	        + TC%ODCAPS_Subset(M)%C(32,K,I) * Predictor%Predictor_Subset(M,32,K) & 
        	        + TC%ODCAPS_Subset(M)%C(33,K,I) * Predictor%Predictor_Subset(M,33,K) & 
        	        + TC%ODCAPS_Subset(M)%C(34,K,I) * Predictor%Predictor_Subset(M,34,K) & 
        	        + TC%ODCAPS_Subset(M)%C(35,K,I) * Predictor%Predictor_Subset(M,35,K) ) 
  
              IF ( KW( K ) < ZERO ) KW( K ) = ZERO
		 
            ENDIF

       !    --------------------------
       !    The other gases abs coef
       !    --------------------------
      	    KCO  = ZERO
	    KOZO = ZERO
	    
	   CASE ( 4 )
	   
       !    --------------------------- 		   
       !    Compute the water continuum 		   
       !    --------------------------- 		   
            KCON = TC%Tuning_Multiple(3,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(1,K,I) * Predictor_Subset(M-3,1,K) &
        	   + TC%ODCAPS_Subset(M)%C(2,K,I) * Predictor_Subset(M-3,2,K) &
        	   + TC%ODCAPS_Subset(M)%C(3,K,I) * Predictor_Subset(M-3,3,K) &
        	   + TC%ODCAPS_Subset(M)%C(4,K,I) * Predictor_Subset(M-3,4,K) &
        	   + TC%ODCAPS_Subset(M)%C(5,K,I) * Predictor_Subset(M-3,5,K) &
        	   + TC%ODCAPS_Subset(M)%C(6,K,I) * Predictor_Subset(M-3,6,K) &
        	   + TC%ODCAPS_Subset(M)%C(7,K,I) * Predictor_Subset(M-3,7,K) )
        		 
            IF ( KCON < ZERO ) THEN		   
               KCON = ZERO				   
            ELSE IF ( KCON > TEN ) THEN 		   
               KCON = TEN				   
            ENDIF					   
  
       !    -----------------------------
       !    Calc the fixed gases abs coef
       !    -----------------------------
            KFIX = TC%Tuning_Multiple(1,J) &
        	 * ( TC%ODCAPS_Subset(M)%C( 8,K,I) * Predictor_Subset(M-3, 8,K) &
        	   + TC%ODCAPS_Subset(M)%C( 9,K,I) * Predictor_Subset(M-3, 9,K) &
        	   + TC%ODCAPS_Subset(M)%C(10,K,I) * Predictor_Subset(M-3,10,K) &
        	   + TC%ODCAPS_Subset(M)%C(11,K,I) * Predictor_Subset(M-3,11,K) &
        	   + TC%ODCAPS_Subset(M)%C(12,K,I) * Predictor_Subset(M-3,12,K) &
        	   + TC%ODCAPS_Subset(M)%C(13,K,I) * Predictor_Subset(M-3,13,K) &
        	   + TC%ODCAPS_Subset(M)%C(14,K,I) * Predictor_Subset(M-3,14,K) &
        	   + TC%ODCAPS_Subset(M)%C(15,K,I) * Predictor_Subset(M-3,15,K) &
        	   + TC%ODCAPS_Subset(M)%C(16,K,I) * Predictor_Subset(M-3,16,K) &
        	   + TC%ODCAPS_Subset(M)%C(17,K,I) * Predictor_Subset(M-3,17,K) &
        	   + TC%ODCAPS_Subset(M)%C(18,K,I) * Predictor_Subset(M-3,18,K) )
 
            KFIX = KFIX * Predictor%Fix_Amount_Multiplier( K )

            IF ( KFIX < ZERO ) THEN	     
               KFIX = ZERO			
            ELSE IF ( KFIX > TEN ) THEN      
               KFIX = TEN			
            ENDIF				

 
       !    -----------------------
       !    Compute the CO abs coef
       !    -----------------------
            KCO = TC%Tuning_Multiple(5,J) &
        	* ( TC%ODCAPS_Subset(M)%C(19,K,I) * Predictor_Subset(M-3,19,K) & 
         	  + TC%ODCAPS_Subset(M)%C(20,K,I) * Predictor_Subset(M-3,20,K) & 
        	  + TC%ODCAPS_Subset(M)%C(21,K,I) * Predictor_Subset(M-3,21,K) & 
        	  + TC%ODCAPS_Subset(M)%C(22,K,I) * Predictor_Subset(M-3,22,K) & 
        	  + TC%ODCAPS_Subset(M)%C(23,K,I) * Predictor_Subset(M-3,23,K) & 
        	  + TC%ODCAPS_Subset(M)%C(24,K,I) * Predictor_Subset(M-3,24,K) & 
        	  + TC%ODCAPS_Subset(M)%C(25,K,I) * Predictor_Subset(M-3,25,K) & 
        	  + TC%ODCAPS_Subset(M)%C(26,K,I) * Predictor_Subset(M-3,26,K) & 
        	  + TC%ODCAPS_Subset(M)%C(27,K,I) * Predictor_Subset(M-3,27,K) & 
        	  + TC%ODCAPS_Subset(M)%C(28,K,I) * Predictor_Subset(M-3,28,K) & 
        	  + TC%ODCAPS_Subset(M)%C(29,K,I) * Predictor_Subset(M-3,29,K) ) 
	    
            IF ( KCO < ZERO ) THEN	  
               KCO = ZERO		  
            ELSEIF ( KCO > TEN ) THEN 
               KCO = TEN		  
            ENDIF			  

       !    --------------------------
       !    Compute the ozone abs coef
       !    --------------------------
            KOZO = TC%Tuning_Multiple(4,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(30,K,I) * Predictor_Subset(M-3,30,K) &   
        	   + TC%ODCAPS_Subset(M)%C(31,K,I) * Predictor_Subset(M-3,31,K) &   
        	   + TC%ODCAPS_Subset(M)%C(32,K,I) * Predictor_Subset(M-3,32,K) )   
   
            IF ( KOZO < ZERO ) THEN
               KOZO = ZERO
            ELSE IF ( KOZO > TEN ) THEN
               KOZO = TEN
            ENDIF
 
       !    --------------------------
       !    Compute the water abs coef
       !    --------------------------
            IF (LH2O) THEN
       !    Not an OPTRAN water channel
            
              KW( K ) = TC%Tuning_Multiple(2,J) &
		      * ( TC%ODCAPS_Subset(M)%C(33,K,I) * Predictor_Subset(M-3,33,K) &
		        + TC%ODCAPS_Subset(M)%C(34,K,I) * Predictor_Subset(M-3,34,K) &
 		        + TC%ODCAPS_Subset(M)%C(35,K,I) * Predictor_Subset(M-3,35,K) &
		        + TC%ODCAPS_Subset(M)%C(36,K,I) * Predictor_Subset(M-3,36,K) & 
        	        + TC%ODCAPS_Subset(M)%C(37,K,I) * Predictor_Subset(M-3,37,K) & 
        	        + TC%ODCAPS_Subset(M)%C(38,K,I) * Predictor_Subset(M-3,38,K) & 
        	        + TC%ODCAPS_Subset(M)%C(39,K,I) * Predictor_Subset(M-3,39,K) & 
        	        + TC%ODCAPS_Subset(M)%C(40,K,I) * Predictor_Subset(M-3,40,K) & 
        	        + TC%ODCAPS_Subset(M)%C(41,K,I) * Predictor_Subset(M-3,41,K) &
        	        + TC%ODCAPS_Subset(M)%C(42,K,I) * Predictor_Subset(M-3,42,K) & 
        	        + TC%ODCAPS_Subset(M)%C(43,K,I) * Predictor_Subset(M-3,43,K) & 
        	        + TC%ODCAPS_Subset(M)%C(44,K,I) * Predictor_Subset(M-3,44,K) & 
        	        + TC%ODCAPS_Subset(M)%C(45,K,I) * Predictor_Subset(M-3,45,K) ) 

              IF ( KW( K ) < ZERO ) THEN
	         KW( K ) = ZERO
              ELSE IF( KW( K ) > TEN ) THEN
                 KW( K ) = TEN
 	      ENDIF	 
            ENDIF

       !    --------------------------
       !    The other gases abs coef
       !    --------------------------
 	    KMET = ZERO

	   CASE ( 5 )
	   
       !    --------------------------- 		   
       !    Compute the water continuum 		   
       !    --------------------------- 		   
            KCON = TC%Tuning_Multiple(3,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(1,K,I) * Predictor_Subset(M-3,1,K) &
        	   + TC%ODCAPS_Subset(M)%C(2,K,I) * Predictor_Subset(M-3,2,K) &
        	   + TC%ODCAPS_Subset(M)%C(3,K,I) * Predictor_Subset(M-3,3,K) &
        	   + TC%ODCAPS_Subset(M)%C(4,K,I) * Predictor_Subset(M-3,4,K) &
        	   + TC%ODCAPS_Subset(M)%C(5,K,I) * Predictor_Subset(M-3,5,K) &
        	   + TC%ODCAPS_Subset(M)%C(6,K,I) * Predictor_Subset(M-3,6,K) &
        	   + TC%ODCAPS_Subset(M)%C(7,K,I) * Predictor_Subset(M-3,7,K) )
        		 
            IF ( KCON < ZERO ) THEN		   
               KCON = ZERO				   
            ELSE IF ( KCON > TEN ) THEN 		   
               KCON = TEN				   
            ENDIF					   
  
       !    -----------------------------
       !    Calc the fixed gases abs coef
       !    -----------------------------
            KFIX = TC%Tuning_Multiple(1,J) &
        	 * ( TC%ODCAPS_Subset(M)%C( 8,K,I) * Predictor_Subset(M-3, 8,K) &
        	   + TC%ODCAPS_Subset(M)%C( 9,K,I) * Predictor_Subset(M-3, 9,K) &
        	   + TC%ODCAPS_Subset(M)%C(10,K,I) * Predictor_Subset(M-3,10,K) &
        	   + TC%ODCAPS_Subset(M)%C(11,K,I) * Predictor_Subset(M-3,11,K) &
        	   + TC%ODCAPS_Subset(M)%C(12,K,I) * Predictor_Subset(M-3,12,K) &
        	   + TC%ODCAPS_Subset(M)%C(13,K,I) * Predictor_Subset(M-3,13,K) &
        	   + TC%ODCAPS_Subset(M)%C(14,K,I) * Predictor_Subset(M-3,14,K) &
        	   + TC%ODCAPS_Subset(M)%C(15,K,I) * Predictor_Subset(M-3,15,K) &
        	   + TC%ODCAPS_Subset(M)%C(16,K,I) * Predictor_Subset(M-3,16,K) &
        	   + TC%ODCAPS_Subset(M)%C(17,K,I) * Predictor_Subset(M-3,17,K) &
        	   + TC%ODCAPS_Subset(M)%C(18,K,I) * Predictor_Subset(M-3,18,K) )
 
            KFIX = KFIX * Predictor%Fix_Amount_Multiplier( K )

            IF ( KFIX < ZERO ) THEN	     
               KFIX = ZERO			
            ELSE IF ( KFIX > TEN ) THEN      
               KFIX = TEN			
            ENDIF				

       !    --------------------------
       !    Compute the water abs coef
       !    --------------------------
            IF (LH2O) THEN
       !    Not an OPTRAN water channel
            
              KW( K ) = TC%Tuning_Multiple(2,J) &
		      * ( TC%ODCAPS_Subset(M)%C(19,K,I) * Predictor_Subset(M-3,19,K) &
		        + TC%ODCAPS_Subset(M)%C(20,K,I) * Predictor_Subset(M-3,20,K) &
 		        + TC%ODCAPS_Subset(M)%C(21,K,I) * Predictor_Subset(M-3,21,K) )

              IF ( KW( K ) < ZERO ) THEN
	         KW( K ) = ZERO
              ELSE IF( KW( K ) > TEN ) THEN
                 KW( K ) = TEN
              ENDIF
	    ENDIF
 
       !    --------------------------
       !    Compute the ozone abs coef
       !    --------------------------
            KOZO = TC%Tuning_Multiple(4,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(22,K,I) * Predictor_Subset(M-3,22,K) )   
    
            IF ( KOZO < ZERO ) THEN
               KOZO = ZERO
            ELSE IF ( KOZO > TEN ) THEN
               KOZO = TEN
            ENDIF

       !    --------------------------
       !    The other gases abs coef
       !    --------------------------
      	    KCO  = ZERO
	    KMET = ZERO
 
	   CASE ( 6 )
	   
       !    --------------------------- 		   
       !    Compute the water continuum 		   
       !    --------------------------- 		   
            KCON = TC%Tuning_Multiple(3,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(1,K,I) * Predictor_Subset(M-3,1,K) &
        	   + TC%ODCAPS_Subset(M)%C(2,K,I) * Predictor_Subset(M-3,2,K) &
        	   + TC%ODCAPS_Subset(M)%C(3,K,I) * Predictor_Subset(M-3,3,K) &
        	   + TC%ODCAPS_Subset(M)%C(4,K,I) * Predictor_Subset(M-3,4,K) &
        	   + TC%ODCAPS_Subset(M)%C(5,K,I) * Predictor_Subset(M-3,5,K) &
        	   + TC%ODCAPS_Subset(M)%C(6,K,I) * Predictor_Subset(M-3,6,K) &
        	   + TC%ODCAPS_Subset(M)%C(7,K,I) * Predictor_Subset(M-3,7,K) )
       !     write(30, *)K, M, KCON, 'con'		 
        		 
            IF ( KCON < ZERO ) THEN		   
               KCON = ZERO				   
            ELSE IF ( KCON > TEN ) THEN 		   
               KCON = TEN				   
            ENDIF					   
  
       !    -----------------------------
       !    Calc the fixed gases abs coef
       !    -----------------------------
            KFIX = TC%Tuning_Multiple(1,J) &
        	 * ( TC%ODCAPS_Subset(M)%C( 8,K,I) * Predictor_Subset(M-3, 8,K) &
        	   + TC%ODCAPS_Subset(M)%C( 9,K,I) * Predictor_Subset(M-3, 9,K) &
        	   + TC%ODCAPS_Subset(M)%C(10,K,I) * Predictor_Subset(M-3,10,K) &
        	   + TC%ODCAPS_Subset(M)%C(11,K,I) * Predictor_Subset(M-3,11,K) &
        	   + TC%ODCAPS_Subset(M)%C(12,K,I) * Predictor_Subset(M-3,12,K) &
        	   + TC%ODCAPS_Subset(M)%C(13,K,I) * Predictor_Subset(M-3,13,K) &
        	   + TC%ODCAPS_Subset(M)%C(14,K,I) * Predictor_Subset(M-3,14,K) &
        	   + TC%ODCAPS_Subset(M)%C(15,K,I) * Predictor_Subset(M-3,15,K) )
 
      !      write(30, *)K, M, KFIX, 'fix' 		 
            KFIX = KFIX * Predictor%Fix_Amount_Multiplier( K )

            IF ( KFIX < ZERO ) THEN	     
               KFIX = ZERO			
            ELSE IF ( KFIX > TEN ) THEN      
               KFIX = TEN			
            ENDIF				

       !    --------------------------
       !    Compute the water abs coef
       !    --------------------------
            IF (LH2O) THEN
       !    Not an OPTRAN water channel
            
              KW( K ) = TC%Tuning_Multiple(2,J) &
		      * ( TC%ODCAPS_Subset(M)%C(16,K,I) * Predictor_Subset(M-3,16,K) &
        	        + TC%ODCAPS_Subset(M)%C(17,K,I) * Predictor_Subset(M-3,17,K) &
        	        + TC%ODCAPS_Subset(M)%C(18,K,I) * Predictor_Subset(M-3,18,K) &
                        + TC%ODCAPS_Subset(M)%C(19,K,I) * Predictor_Subset(M-3,19,K) &
		        + TC%ODCAPS_Subset(M)%C(20,K,I) * Predictor_Subset(M-3,20,K) &
 		        + TC%ODCAPS_Subset(M)%C(21,K,I) * Predictor_Subset(M-3,21,K) &
 		        + TC%ODCAPS_Subset(M)%C(22,K,I) * Predictor_Subset(M-3,22,K) )
             
       !      write(30, *)K, M, KW(K), 'kw' 		 
              
              IF ( KW( K ) < ZERO ) THEN
	         KW( K ) = ZERO
              ELSE IF( KW( K ) > TEN ) THEN
                 KW( K ) = TEN
              ENDIF
	    ENDIF
 
       !    --------------------------
       !    Compute the ozone abs coef
       !    --------------------------
            KOZO = TC%Tuning_Multiple(4,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(23,K,I) * Predictor_Subset(M-3,23,K) )   
    
	!    write(30, *)K, M, KOZO, 'kozo' 		 
            IF ( KOZO < ZERO ) THEN
               KOZO = ZERO
            ELSE IF ( KOZO > TEN ) THEN
               KOZO = TEN
            ENDIF

       !    --------------------------
       !    The other gases abs coef
       !    --------------------------
      	    KCO  = ZERO
	    KMET = ZERO
 
	   CASE ( 7 )
	   
       !    --------------------------- 		   
       !    Compute the water continuum 		   
       !    --------------------------- 		   
            KCON = TC%Tuning_Multiple(3,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(1,K,I) * Predictor_Subset(M-3,1,K) &
        	   + TC%ODCAPS_Subset(M)%C(2,K,I) * Predictor_Subset(M-3,2,K) &
        	   + TC%ODCAPS_Subset(M)%C(3,K,I) * Predictor_Subset(M-3,3,K) &
        	   + TC%ODCAPS_Subset(M)%C(4,K,I) * Predictor_Subset(M-3,4,K) &
        	   + TC%ODCAPS_Subset(M)%C(5,K,I) * Predictor_Subset(M-3,5,K) &
        	   + TC%ODCAPS_Subset(M)%C(6,K,I) * Predictor_Subset(M-3,6,K) &
        	   + TC%ODCAPS_Subset(M)%C(7,K,I) * Predictor_Subset(M-3,7,K) )
      !      write(31, *)K, M, KCON, 'con'		 
            IF ( KCON < ZERO ) THEN		   
               KCON = ZERO				   
            ELSE IF ( KCON > TEN ) THEN 		   
               KCON = TEN				   
            ENDIF					   
  
       !    -----------------------------
       !    Calc the fixed gases abs coef
       !    -----------------------------
            KFIX = TC%Tuning_Multiple(1,J) &
        	 * ( TC%ODCAPS_Subset(M)%C( 8,K,I) * Predictor_Subset(M-3, 8,K) &
        	   + TC%ODCAPS_Subset(M)%C( 9,K,I) * Predictor_Subset(M-3, 9,K) &
        	   + TC%ODCAPS_Subset(M)%C(10,K,I) * Predictor_Subset(M-3,10,K) &
        	   + TC%ODCAPS_Subset(M)%C(11,K,I) * Predictor_Subset(M-3,11,K) &
        	   + TC%ODCAPS_Subset(M)%C(12,K,I) * Predictor_Subset(M-3,12,K) &
        	   + TC%ODCAPS_Subset(M)%C(13,K,I) * Predictor_Subset(M-3,13,K) &
        	   + TC%ODCAPS_Subset(M)%C(14,K,I) * Predictor_Subset(M-3,14,K) &
        	   + TC%ODCAPS_Subset(M)%C(15,K,I) * Predictor_Subset(M-3,15,K) )
            
	!    write(31, *)K, M, KFIX, 'fix'		 
 
            KFIX = KFIX * Predictor%Fix_Amount_Multiplier( K )

            IF ( KFIX < ZERO ) THEN	     
               KFIX = ZERO			
            ELSE IF ( KFIX > TEN ) THEN      
               KFIX = TEN			
            ENDIF				

       !    --------------------------
       !    Compute the water abs coef
       !    --------------------------
            IF (LH2O) THEN
       !    Not an OPTRAN water channel
            
              KW( K ) = TC%Tuning_Multiple(2,J) &
		      * ( TC%ODCAPS_Subset(M)%C(16,K,I) * Predictor_Subset(M-3,16,K) &
        	        + TC%ODCAPS_Subset(M)%C(17,K,I) * Predictor_Subset(M-3,17,K) &
        	        + TC%ODCAPS_Subset(M)%C(18,K,I) * Predictor_Subset(M-3,18,K) &
                        + TC%ODCAPS_Subset(M)%C(19,K,I) * Predictor_Subset(M-3,19,K) &
		        + TC%ODCAPS_Subset(M)%C(20,K,I) * Predictor_Subset(M-3,20,K) &
 		        + TC%ODCAPS_Subset(M)%C(21,K,I) * Predictor_Subset(M-3,21,K) &
 		        + TC%ODCAPS_Subset(M)%C(22,K,I) * Predictor_Subset(M-3,22,K) &
        	        + TC%ODCAPS_Subset(M)%C(23,K,I) * Predictor_Subset(M-3,23,K) &
                        + TC%ODCAPS_Subset(M)%C(24,K,I) * Predictor_Subset(M-3,24,K) &
		        + TC%ODCAPS_Subset(M)%C(25,K,I) * Predictor_Subset(M-3,25,K) &
 		        + TC%ODCAPS_Subset(M)%C(26,K,I) * Predictor_Subset(M-3,26,K) &
 		        + TC%ODCAPS_Subset(M)%C(27,K,I) * Predictor_Subset(M-3,27,K) &
 		        + TC%ODCAPS_Subset(M)%C(28,K,I) * Predictor_Subset(M-3,28,K) )
	      
	!      write(31, *)K, M, KW(K), 'kw' 		 

              IF ( KW( K ) < ZERO ) THEN
	         KW( K ) = ZERO
              ELSE IF( KW( K ) > TEN ) THEN
                 KW( K ) = TEN
              ENDIF
	    ENDIF
 
       !    --------------------------
       !    Compute the ozone abs coef
       !    --------------------------
            KOZO = TC%Tuning_Multiple(4,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(29,K,I) * Predictor_Subset(M-3,29,K) )   
	!    write(31, *)K, M, KOZO, 'kozo' 		 
    
            IF ( KOZO < ZERO ) THEN
               KOZO = ZERO
            ELSE IF ( KOZO > TEN ) THEN
               KOZO = TEN
            ENDIF

       !    --------------------------
       !    The other gases abs coef
       !    --------------------------
      	    KCO  = ZERO
	    KMET = ZERO

       END SELECT Subset_Select


  !    ----------------------------
  !    Calc change in total optical
  !    depth due to variable CO2
  !    ----------------------------
       IF ( LCO2 .AND. Predictor%CO2_Multiplier( K ) /= ZERO ) THEN
 	  DKCO2 = ( TC%ODCAPS_TraceGas(1)%Trace_Coeff(1,K,ICO2)* TraceGas_Predictors(1,K) & 
	        +   TC%ODCAPS_TraceGas(1)%Trace_Coeff(2,K,ICO2)* TraceGas_Predictors(2,K) &
	        +   TC%ODCAPS_TraceGas(1)%Trace_Coeff(3,K,ICO2)* TraceGas_Predictors(3,K) &
	        +   TC%ODCAPS_TraceGas(1)%Trace_Coeff(4,K,ICO2)* TraceGas_Predictors(4,K) )
 
  	  DKCO2 = DKCO2 * Predictor%CO2_Multiplier( K ) 
       ELSE
 	  DKCO2 = ZERO
       ENDIF

  !    ----------------------------
  !    Calc change in total optical
  !    depth due to variable SO2
  !    ----------------------------
       IF ( LSO2 .AND. Predictor%SO2_Multiplier( K ) /= ZERO ) THEN
          DKSO2 = ( TC%ODCAPS_TraceGas(2)%Trace_Coeff(1,K,ISO2)* TraceGas_Predictors(1,K) & 
	        +   TC%ODCAPS_TraceGas(2)%Trace_Coeff(2,K,ISO2)* TraceGas_Predictors(2,K) &
	        +   TC%ODCAPS_TraceGas(2)%Trace_Coeff(3,K,ISO2)* TraceGas_Predictors(3,K) &
	        +   TC%ODCAPS_TraceGas(2)%Trace_Coeff(4,K,ISO2)* TraceGas_Predictors(4,K) )
 
          DKSO2 = DKSO2 * Predictor%SO2_Multiplier( K ) 
       ELSE
          DKSO2 = ZERO
       ENDIF
  
  !    ----------------------------
  !    Calc change in total optical
  !    depth due to variable HNO3
  !    ----------------------------
       IF ( LHNO3 .AND. Predictor%HNO3_Multiplier( K ) /= ZERO ) THEN
 	  DKHNO3 = ( TC%ODCAPS_TraceGas(3)%Trace_Coeff(1,K,IHNO3)* TraceGas_Predictors(1,K) & 
	         +   TC%ODCAPS_TraceGas(3)%Trace_Coeff(2,K,IHNO3)* TraceGas_Predictors(2,K) &
	         +   TC%ODCAPS_TraceGas(3)%Trace_Coeff(3,K,IHNO3)* TraceGas_Predictors(3,K) &
	         +   TC%ODCAPS_TraceGas(3)%Trace_Coeff(4,K,IHNO3)* TraceGas_Predictors(4,K) )
 
 	  DKHNO3 = DKHNO3 * Predictor%HNO3_Multiplier( K )
       ELSE
 	  DKHNO3 = ZERO
       ENDIF
 
  !	----------------------------
  !	Calc change in total optical
  !	depth due to variable N2O
  !	----------------------------
 	IF ( LN2O .AND. Predictor%N2O_Multiplier( K ) /= ZERO ) THEN
 	   DKN2O = ( TC%ODCAPS_TraceGas(4)%Trace_Coeff(1,K,IN2O)* TraceGas_Predictors(1,K) & 
	         +   TC%ODCAPS_TraceGas(4)%Trace_Coeff(2,K,IN2O)* TraceGas_Predictors(2,K) &
	         +   TC%ODCAPS_TraceGas(4)%Trace_Coeff(3,K,IN2O)* TraceGas_Predictors(3,K) &
	         +   TC%ODCAPS_TraceGas(4)%Trace_Coeff(4,K,IN2O)* TraceGas_Predictors(4,K) &
	         +   TC%ODCAPS_TraceGas(4)%Trace_Coeff(5,K,IN2O)* TraceGas_Predictors(5,K) &
	         +   TC%ODCAPS_TraceGas(4)%Trace_Coeff(6,K,IN2O)* TraceGas_Predictors(6,K) &
	         +   TC%ODCAPS_TraceGas(4)%Trace_Coeff(7,K,IN2O)* TraceGas_Predictors(7,K) )
  	   
	   DKN2O = DKN2O * Predictor%N2O_Multiplier( K ) 
 	ELSE
 	   DKN2O = ZERO
 	ENDIF

  !	Limit -DK so it can never totally totally cancel KFIX 
 	DK = DKCO2 + DKSO2 + DKHNO3 + DKN2O		      
 	IF ( -DK >= KFIX ) THEN 			      
 	   DK = -0.999 * KFIX				      
 	ENDIF						      
 							      
 	KLAYER = KCON + KFIX + KW( K ) + KOZO + KCO + KMET + DK	      

  !	Adjust the optical depth of the bottom layer	      
 	IF ( K == Predictor%n_Layers) KLAYER = Predictor%BL_Frac_Multi * KLAYER	      

  !	Calc layer-to-space optical depth		      
 	KZ = KZ + KLAYER					      

        IF( .NOT. Cal_Sun) THEN 
           Predictor%Optical_Depth( K ) = KLAYER 
	
	   Predictor%LTS_Optical_Depth( K ) = KZ
        ENDIF

    END DO Layer_Loop
    
  
    IF ( Cal_Sun ) THEN
       
       ! For Subset 1-3
!       IF ( M < 4 ) THEN 
      	  
!	 Predictor%Surf_To_Space_Optical_Depth = KZ * &
!	    Predictor%Secant_Source_Zenith(Predictor%n_Layers) &
!	    * Predictor%Sun_Fudge / Predictor%Secant_Sensor_Zenith( Predictor%n_Layers )

       ! For Subset 4-7
!       ELSE
       IF (M >= 4 ) THEN 
	 Predictor%Surf_To_Space_Optical_Depth = KZ * XZ
       ENDIF	 
        
    ENDIF	

    NULLIFY(TC)
  END SUBROUTINE Compute_Optical_Depth_Subset 
  
  !#------------------------------------------------------------------------------#
  !#       Subroutine to calculate the layer optical depths due to gaseous	  #
  !#       absorption for a given input atmospheric profile for a single	  #
  !#       channel. Tangent-linear model 	    			          #
  !#------------------------------------------------------------------------------#
 
   
  SUBROUTINE Compute_Optical_Depth_Subset_TL(  Sensor_Index,            &  ! Input
                                               Channel_Subset_Index,    & ! Input
                                               Channel_Subset_Position, & ! Input
                                               Predictor,               & ! Input
					       Predictor_TL,            & ! In/Output
					       Do_Sun_Calc)               ! Optional Input 
                                         
    ! ---------
    ! Arguments
    ! ---------
    ! -- Inputs
    INTEGER,  INTENT( IN )     ::  Sensor_Index 
    INTEGER,  INTENT( IN )     ::  Channel_Subset_Index 
    INTEGER,  INTENT( IN )     ::  Channel_Subset_Position 

    ! -- Inputs
    TYPE( Predictor_type ), INTENT( IN ) :: Predictor

    ! -- In/Outputs
    TYPE( Predictor_type ), INTENT( IN OUT ) :: Predictor_TL

    ! -- Optional Inputs
    INTEGER,               OPTIONAL, INTENT( IN )     :: Do_Sun_Calc 

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Optical_Depth_Subset_TL'

    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER ::      I   
    INTEGER ::	    J  
    INTEGER ::	    M  
    INTEGER ::      K 
    INTEGER ::   ICO2   
    INTEGER ::  IHNO3   
    INTEGER ::   IN2O   
    INTEGER ::   ISO2   
      
    REAL( fp ) ::   KCON, KCON_TL    
    REAL( fp ) ::   KFIX, KFIX_TL, KFIXX, KFIXX_TL
    REAL( fp ) ::   KOZO, KOZO_TL    
    REAL( fp ) ::   KMET, KMET_TL 
    REAL( fp ) ::    KCO, KCO_TL 
    REAL( fp ) ::     DK, DK_TL    
    REAL( fp ) ::  DKCO2, DKCO2_TL, DKCO2X, DKCO2X_TL    
    REAL( fp ) :: DKHNO3, DKHNO3_TL, DKHNO3X, DKHNO3X_TL   
    REAL( fp ) ::  DKN2O, DKN2O_TL, DKN2OX, DKN2OX_TL   
    REAL( fp ) ::  DKSO2, DKSO2_TL,  DKSO2X, DKSO2X_TL  
    REAL( fp ) :: KLAYER, KLAYER_TL    
    REAL( fp ) ::     KZ, KZ_TL    

    LOGICAL ::  LCO2   
    LOGICAL ::  LH2O   
    LOGICAL :: LHNO3   
    LOGICAL ::  LN2O   
    LOGICAL ::  LSO2   
 
  ! for CALOKW
    INTEGER ::  IH2O
    REAL( fp), DIMENSION(Predictor%n_Layers) :: KW, KW_TL

    LOGICAL :: Cal_Sun
    REAL( fp ), DIMENSION(4, MAX_N_SUBSET_TOTAL_PREDICTORS,Predictor%n_Layers) :: &
                                Predictor_Subset, Predictor_Subset_TL
    REAL( fp ), DIMENSION(MAX_N_TRACEGASES_PREDICTORS,Predictor%n_Layers) :: &
                                TraceGas_Predictors, TraceGas_Predictors_TL
    REAL( fp ) ::    XZ, XZ_TL 
    INTEGER :: idx
    TYPE( ODCAPS_TauCoeff_type ), POINTER  ::  TC => NULL()

    TC => ODCAPS_TC(Sensor_Index)
 
    Cal_Sun = .TRUE.                                            

    M = Channel_Subset_Index  
   
    I = Channel_Subset_Position 
    
    idx = MINLOC(ABS(TC%Sensor_Channel - TC%ODCAPS_Subset(M)%Channel_Index(I) ), DIM =1 )
    
    J =  idx

  ! Determine whether or not to do variable CO2 calc     
    ICO2 = TC%Channel_CO2_Perturbation( idx )			         
    IF (ICO2 > 0) THEN			         
       LCO2=.TRUE.				         
    ELSE					         
       LCO2=.FALSE.				         
    ENDIF					         

  ! Determine whether or not to do variable SO2 calc     
    ISO2 = TC%Channel_SO2_Perturbation( idx )			         
    IF (ISO2 > 0) THEN			         
       LSO2=.TRUE.				         
    ELSE					         
       LSO2=.FALSE.				         
    ENDIF					         

  ! Determine whether or not to do variable HNO3 calc    
    IHNO3 = TC%Channel_HNO3_Perturbation( idx )
    IF (IHNO3 > 0) THEN			         
       LHNO3=.TRUE.				         
    ELSE					         
       LHNO3=.FALSE.				         
    ENDIF					         

  ! Determine whether or not to do variable N2O calc     
    IN2O = TC%Channel_N2O_Perturbation( idx )   		         
    IF (IN2O > 0) THEN  			       
       LN2O=.TRUE.				       
    ELSE	        			       
       LN2O=.FALSE.				       
    ENDIF	        			       

  !  -------------------------
  !  Do OPTRAN water if needed
  !  -------------------------
    IH2O = TC%Channel_H2O_OPTRAN( idx ) 
    IF (IH2O > 0) THEN					     
       LH2O=.FALSE.						     
  
  !    Calc OPTRAN water 					     
       CALL Compute_WOPTRAN_Optics_TL(Sensor_Index, IH2O, Predictor, Predictor_TL, &
                                             KW, KW_TL )
!       KW_TL = ZERO				     		     

    ELSE							     
       LH2O=.TRUE.						     
    ENDIF							     


    IF ( PRESENT (Do_Sun_Calc) .AND. &  					     
    	 Predictor%Calc_Sun_Angle_Secant) THEN	
      Predictor_Subset(1:4, :, :) = Predictor%Predictor_Subset_Sun(1:4, :, :)  
      TraceGas_Predictors = Predictor%TraceGas_Predictors_Sun
      XZ = Predictor%Sun_Fudge 
	 			    
      Predictor_Subset_TL(1:4, :, :) = Predictor_TL%Predictor_Subset_Sun(1:4, :, :)  
      TraceGas_Predictors_TL = Predictor_TL%TraceGas_Predictors_Sun
      XZ_TL = ZERO   !Predictor_TL%Sun_Fudge 
      Cal_Sun = .TRUE.  							    
    ELSE		
      Predictor_Subset(1:4, :, :) = Predictor%Predictor_Subset(4:7, :, :)	  
      TraceGas_Predictors = Predictor%TraceGas_Predictors
      XZ = ZERO  
    							     
      Predictor_Subset_TL(1:4, :, :) = Predictor_TL%Predictor_Subset(4:7, :, :)	  
      TraceGas_Predictors_TL = Predictor_TL%TraceGas_Predictors
      XZ_TL = ZERO  
      Cal_Sun = .FALSE. 							    
    END IF									     

  ! Initialize the layer-to-space optical depth
    KZ = ZERO
    KZ_TL = ZERO
    
  ! KZFW = ZERO

    !#------------------------------------------------------------------------#  
    !#  		  -- BEGIN LOOP OVER LAYERS --  		      #  
    !#------------------------------------------------------------------------#  

    Layer_Loop: DO K = 1, Predictor%n_Layers				 

 
       Subset_Select: SELECT CASE ( M )
 
	   CASE ( 1 )
	   
       !    --------------------------- 		   
       !    Compute the water continuum 		   
       !    --------------------------- 		   
            KCON = TC%Tuning_Multiple(3,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(1,K,I) * Predictor%Predictor_Subset(M,1,K) &
        	   + TC%ODCAPS_Subset(M)%C(2,K,I) * Predictor%Predictor_Subset(M,2,K) &
        	   + TC%ODCAPS_Subset(M)%C(3,K,I) * Predictor%Predictor_Subset(M,3,K) &
        	   + TC%ODCAPS_Subset(M)%C(4,K,I) * Predictor%Predictor_Subset(M,4,K) &
        	   + TC%ODCAPS_Subset(M)%C(5,K,I) * Predictor%Predictor_Subset(M,5,K) &
        	   + TC%ODCAPS_Subset(M)%C(6,K,I) * Predictor%Predictor_Subset(M,6,K) &
        	   + TC%ODCAPS_Subset(M)%C(7,K,I) * Predictor%Predictor_Subset(M,7,K) )

            KCON_TL = TC%Tuning_Multiple(3,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(1,K,I) * Predictor_TL%Predictor_Subset(M,1,K) &
        	   + TC%ODCAPS_Subset(M)%C(2,K,I) * Predictor_TL%Predictor_Subset(M,2,K) &
        	   + TC%ODCAPS_Subset(M)%C(3,K,I) * Predictor_TL%Predictor_Subset(M,3,K) &
        	   + TC%ODCAPS_Subset(M)%C(4,K,I) * Predictor_TL%Predictor_Subset(M,4,K) &
        	   + TC%ODCAPS_Subset(M)%C(5,K,I) * Predictor_TL%Predictor_Subset(M,5,K) &
        	   + TC%ODCAPS_Subset(M)%C(6,K,I) * Predictor_TL%Predictor_Subset(M,6,K) &
        	   + TC%ODCAPS_Subset(M)%C(7,K,I) * Predictor_TL%Predictor_Subset(M,7,K) )
 	    	 
            IF ( KCON < ZERO ) THEN		   
               KCON = ZERO				   
               KCON_TL = ZERO				   
            ELSE IF ( KCON > TEN ) THEN 		   
               KCON = TEN				   
               KCON_TL = ZERO				   
            ENDIF					   
   
       !    -----------------------------
       !    Calc the fixed gases abs coef
       !    -----------------------------
            KFIXX = TC%Tuning_Multiple(1,J) &
        	 * ( TC%ODCAPS_Subset(M)%C( 8,K,I) * Predictor%Predictor_Subset(M, 8,K) &
        	   + TC%ODCAPS_Subset(M)%C( 9,K,I) * Predictor%Predictor_Subset(M, 9,K) &
        	   + TC%ODCAPS_Subset(M)%C(10,K,I) * Predictor%Predictor_Subset(M,10,K) &
        	   + TC%ODCAPS_Subset(M)%C(11,K,I) * Predictor%Predictor_Subset(M,11,K) &
        	   + TC%ODCAPS_Subset(M)%C(12,K,I) * Predictor%Predictor_Subset(M,12,K) &
        	   + TC%ODCAPS_Subset(M)%C(13,K,I) * Predictor%Predictor_Subset(M,13,K) &
        	   + TC%ODCAPS_Subset(M)%C(14,K,I) * Predictor%Predictor_Subset(M,14,K) &
        	   + TC%ODCAPS_Subset(M)%C(15,K,I) * Predictor%Predictor_Subset(M,15,K) )

            KFIXX_TL = TC%Tuning_Multiple(1,J) &
        	 * ( TC%ODCAPS_Subset(M)%C( 8,K,I) * Predictor_TL%Predictor_Subset(M, 8,K) &
        	   + TC%ODCAPS_Subset(M)%C( 9,K,I) * Predictor_TL%Predictor_Subset(M, 9,K) &
        	   + TC%ODCAPS_Subset(M)%C(10,K,I) * Predictor_TL%Predictor_Subset(M,10,K) &
        	   + TC%ODCAPS_Subset(M)%C(11,K,I) * Predictor_TL%Predictor_Subset(M,11,K) &
        	   + TC%ODCAPS_Subset(M)%C(12,K,I) * Predictor_TL%Predictor_Subset(M,12,K) &
        	   + TC%ODCAPS_Subset(M)%C(13,K,I) * Predictor_TL%Predictor_Subset(M,13,K) &
        	   + TC%ODCAPS_Subset(M)%C(14,K,I) * Predictor_TL%Predictor_Subset(M,14,K) &
        	   + TC%ODCAPS_Subset(M)%C(15,K,I) * Predictor_TL%Predictor_Subset(M,15,K) )
 
            KFIX_TL = KFIXX_TL * Predictor%Fix_Amount_Multiplier( K )  &
                    + KFIXX * Predictor_TL%Fix_Amount_Multiplier( K )
 
!            KFIX_TL = ZERO
	    KFIX = KFIXX * Predictor%Fix_Amount_Multiplier( K )
 
                     
            IF ( KFIX < ZERO ) THEN	     
               KFIX = ZERO
	       KFIX_TL = ZERO			
            ELSE IF ( KFIX > TEN ) THEN      
               KFIX = TEN
	       KFIX_TL = ZERO			
            ENDIF				

       !    --------------------------
       !    Compute the water abs coef
       !    --------------------------
            IF (LH2O) THEN
       !    Not an OPTRAN water channel
            
              KW( K ) = TC%Tuning_Multiple(2,J) &
        	    * ( TC%ODCAPS_Subset(M)%C(16,K,I) * Predictor%Predictor_Subset(M,16,K) &
        	      + TC%ODCAPS_Subset(M)%C(17,K,I) * Predictor%Predictor_Subset(M,17,K) &
        	      + TC%ODCAPS_Subset(M)%C(18,K,I) * Predictor%Predictor_Subset(M,18,K) &
        	      + TC%ODCAPS_Subset(M)%C(19,K,I) * Predictor%Predictor_Subset(M,19,K) &
        	      + TC%ODCAPS_Subset(M)%C(20,K,I) * Predictor%Predictor_Subset(M,20,K) &
        	      + TC%ODCAPS_Subset(M)%C(21,K,I) * Predictor%Predictor_Subset(M,21,K) &
        	      + TC%ODCAPS_Subset(M)%C(22,K,I) * Predictor%Predictor_Subset(M,22,K) &
        	      + TC%ODCAPS_Subset(M)%C(23,K,I) * Predictor%Predictor_Subset(M,23,K) &
        	      + TC%ODCAPS_Subset(M)%C(24,K,I) * Predictor%Predictor_Subset(M,24,K) &
        	      + TC%ODCAPS_Subset(M)%C(25,K,I) * Predictor%Predictor_Subset(M,25,K) &
        	      + TC%ODCAPS_Subset(M)%C(26,K,I) * Predictor%Predictor_Subset(M,26,K) )
 
              KW_TL( K ) = TC%Tuning_Multiple(2,J) &
        	    * ( TC%ODCAPS_Subset(M)%C(16,K,I) * Predictor_TL%Predictor_Subset(M,16,K) &
        	      + TC%ODCAPS_Subset(M)%C(17,K,I) * Predictor_TL%Predictor_Subset(M,17,K) &
        	      + TC%ODCAPS_Subset(M)%C(18,K,I) * Predictor_TL%Predictor_Subset(M,18,K) &
        	      + TC%ODCAPS_Subset(M)%C(19,K,I) * Predictor_TL%Predictor_Subset(M,19,K) &
        	      + TC%ODCAPS_Subset(M)%C(20,K,I) * Predictor_TL%Predictor_Subset(M,20,K) &
        	      + TC%ODCAPS_Subset(M)%C(21,K,I) * Predictor_TL%Predictor_Subset(M,21,K) &
        	      + TC%ODCAPS_Subset(M)%C(22,K,I) * Predictor_TL%Predictor_Subset(M,22,K) &
        	      + TC%ODCAPS_Subset(M)%C(23,K,I) * Predictor_TL%Predictor_Subset(M,23,K) &
        	      + TC%ODCAPS_Subset(M)%C(24,K,I) * Predictor_TL%Predictor_Subset(M,24,K) &
        	      + TC%ODCAPS_Subset(M)%C(25,K,I) * Predictor_TL%Predictor_Subset(M,25,K) &
        	      + TC%ODCAPS_Subset(M)%C(26,K,I) * Predictor_TL%Predictor_Subset(M,26,K) )
!              KW_TL( K ) = ZERO

              IF ( KW( K ) < ZERO ) THEN
	        KW( K ) = ZERO 
	        KW_TL( K ) = ZERO
	      ENDIF	
            ENDIF

       !    --------------------------
       !    Compute the ozone abs coef
       !    --------------------------
            KOZO = TC%Tuning_Multiple(4,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(27,K,I) * Predictor%Predictor_Subset(M,27,K) &   
        	   + TC%ODCAPS_Subset(M)%C(28,K,I) * Predictor%Predictor_Subset(M,28,K) &   
        	   + TC%ODCAPS_Subset(M)%C(29,K,I) * Predictor%Predictor_Subset(M,29,K) &   
        	   + TC%ODCAPS_Subset(M)%C(30,K,I) * Predictor%Predictor_Subset(M,30,K) &   
        	   + TC%ODCAPS_Subset(M)%C(31,K,I) * Predictor%Predictor_Subset(M,31,K) )  

            KOZO_TL = TC%Tuning_Multiple(4,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(27,K,I) * Predictor_TL%Predictor_Subset(M,27,K) &   
        	   + TC%ODCAPS_Subset(M)%C(28,K,I) * Predictor_TL%Predictor_Subset(M,28,K) &   
        	   + TC%ODCAPS_Subset(M)%C(29,K,I) * Predictor_TL%Predictor_Subset(M,29,K) &   
        	   + TC%ODCAPS_Subset(M)%C(30,K,I) * Predictor_TL%Predictor_Subset(M,30,K) &   
        	   + TC%ODCAPS_Subset(M)%C(31,K,I) * Predictor_TL%Predictor_Subset(M,31,K) )  

!            KOZO_TL = ZERO
 
            IF ( KOZO < ZERO ) THEN
               KOZO = ZERO
               KOZO_TL = ZERO
            ELSE IF ( KOZO > TEN ) THEN
               KOZO = TEN
               KOZO_TL = ZERO
            ENDIF
      
       !    --------------------------
       !    The other gases abs coef
       !    --------------------------
      	    KCO  = ZERO
	    KMET = ZERO

      	    KCO_TL  = ZERO
	    KMET_TL = ZERO

	   CASE ( 2 )
	   
       !    --------------------------- 		   
       !    Compute the water continuum 		   
       !    --------------------------- 		   
            KCON = TC%Tuning_Multiple(3,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(1,K,I) * Predictor%Predictor_Subset(M,1,K) &
        	   + TC%ODCAPS_Subset(M)%C(2,K,I) * Predictor%Predictor_Subset(M,2,K) &
        	   + TC%ODCAPS_Subset(M)%C(3,K,I) * Predictor%Predictor_Subset(M,3,K) &
        	   + TC%ODCAPS_Subset(M)%C(4,K,I) * Predictor%Predictor_Subset(M,4,K) &
        	   + TC%ODCAPS_Subset(M)%C(5,K,I) * Predictor%Predictor_Subset(M,5,K) &
        	   + TC%ODCAPS_Subset(M)%C(6,K,I) * Predictor%Predictor_Subset(M,6,K) &
        	   + TC%ODCAPS_Subset(M)%C(7,K,I) * Predictor%Predictor_Subset(M,7,K) )
        		 
            KCON_TL = TC%Tuning_Multiple(3,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(1,K,I) * Predictor_TL%Predictor_Subset(M,1,K) &
        	   + TC%ODCAPS_Subset(M)%C(2,K,I) * Predictor_TL%Predictor_Subset(M,2,K) &
        	   + TC%ODCAPS_Subset(M)%C(3,K,I) * Predictor_TL%Predictor_Subset(M,3,K) &
        	   + TC%ODCAPS_Subset(M)%C(4,K,I) * Predictor_TL%Predictor_Subset(M,4,K) &
        	   + TC%ODCAPS_Subset(M)%C(5,K,I) * Predictor_TL%Predictor_Subset(M,5,K) &
        	   + TC%ODCAPS_Subset(M)%C(6,K,I) * Predictor_TL%Predictor_Subset(M,6,K) &
        	   + TC%ODCAPS_Subset(M)%C(7,K,I) * Predictor_TL%Predictor_Subset(M,7,K) )
!            KCON_TL = ZERO

            IF ( KCON < ZERO ) THEN		   
               KCON = ZERO				   
               KCON_TL = ZERO				   
            ELSE IF ( KCON > TEN ) THEN 		   
               KCON = TEN				   
               KCON_TL = ZERO				   
            ENDIF					   
  
       !    -----------------------------
       !    Calc the fixed gases abs coef
       !    -----------------------------
            KFIXX = TC%Tuning_Multiple(1,J) &
        	 * ( TC%ODCAPS_Subset(M)%C( 8,K,I) * Predictor%Predictor_Subset(M, 8,K) &
        	   + TC%ODCAPS_Subset(M)%C( 9,K,I) * Predictor%Predictor_Subset(M, 9,K) &
        	   + TC%ODCAPS_Subset(M)%C(10,K,I) * Predictor%Predictor_Subset(M,10,K) &
        	   + TC%ODCAPS_Subset(M)%C(11,K,I) * Predictor%Predictor_Subset(M,11,K) &
        	   + TC%ODCAPS_Subset(M)%C(12,K,I) * Predictor%Predictor_Subset(M,12,K) &
        	   + TC%ODCAPS_Subset(M)%C(13,K,I) * Predictor%Predictor_Subset(M,13,K) &
        	   + TC%ODCAPS_Subset(M)%C(14,K,I) * Predictor%Predictor_Subset(M,14,K) &
        	   + TC%ODCAPS_Subset(M)%C(15,K,I) * Predictor%Predictor_Subset(M,15,K) )
 
            KFIXX_TL = TC%Tuning_Multiple(1,J) &
        	 * ( TC%ODCAPS_Subset(M)%C( 8,K,I) * Predictor_TL%Predictor_Subset(M, 8,K) &
        	   + TC%ODCAPS_Subset(M)%C( 9,K,I) * Predictor_TL%Predictor_Subset(M, 9,K) &
        	   + TC%ODCAPS_Subset(M)%C(10,K,I) * Predictor_TL%Predictor_Subset(M,10,K) &
        	   + TC%ODCAPS_Subset(M)%C(11,K,I) * Predictor_TL%Predictor_Subset(M,11,K) &
        	   + TC%ODCAPS_Subset(M)%C(12,K,I) * Predictor_TL%Predictor_Subset(M,12,K) &
        	   + TC%ODCAPS_Subset(M)%C(13,K,I) * Predictor_TL%Predictor_Subset(M,13,K) &
        	   + TC%ODCAPS_Subset(M)%C(14,K,I) * Predictor_TL%Predictor_Subset(M,14,K) &
        	   + TC%ODCAPS_Subset(M)%C(15,K,I) * Predictor_TL%Predictor_Subset(M,15,K) )

            KFIX_TL = KFIXX_TL * Predictor%Fix_Amount_Multiplier( K )  &
                    + KFIXX * Predictor_TL%Fix_Amount_Multiplier( K )

            KFIX = KFIXX * Predictor%Fix_Amount_Multiplier( K )

!            KFIX_TL = ZERO			
		    
            IF ( KFIX < ZERO ) THEN	     
               KFIX = ZERO			
               KFIX_TL = ZERO			
            ELSE IF ( KFIX > TEN ) THEN      
               KFIX = TEN			
               KFIX_TL = ZERO			
            ENDIF				

       !    --------------------------
       !    Compute the ozone abs coef
       !    --------------------------
            KOZO = TC%Tuning_Multiple(4,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(16,K,I) * Predictor%Predictor_Subset(M,16,K) &   
        	   + TC%ODCAPS_Subset(M)%C(17,K,I) * Predictor%Predictor_Subset(M,17,K) &   
        	   + TC%ODCAPS_Subset(M)%C(18,K,I) * Predictor%Predictor_Subset(M,18,K) &   
        	   + TC%ODCAPS_Subset(M)%C(19,K,I) * Predictor%Predictor_Subset(M,19,K) &   
        	   + TC%ODCAPS_Subset(M)%C(20,K,I) * Predictor%Predictor_Subset(M,20,K) &   
        	   + TC%ODCAPS_Subset(M)%C(21,K,I) * Predictor%Predictor_Subset(M,21,K) &   
        	   + TC%ODCAPS_Subset(M)%C(22,K,I) * Predictor%Predictor_Subset(M,22,K) &   
        	   + TC%ODCAPS_Subset(M)%C(23,K,I) * Predictor%Predictor_Subset(M,23,K) &   
        	   + TC%ODCAPS_Subset(M)%C(24,K,I) * Predictor%Predictor_Subset(M,24,K) &   
        	   + TC%ODCAPS_Subset(M)%C(25,K,I) * Predictor%Predictor_Subset(M,25,K) )   
  
            KOZO_TL = TC%Tuning_Multiple(4,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(16,K,I) * Predictor_TL%Predictor_Subset(M,16,K) &   
        	   + TC%ODCAPS_Subset(M)%C(17,K,I) * Predictor_TL%Predictor_Subset(M,17,K) &   
        	   + TC%ODCAPS_Subset(M)%C(18,K,I) * Predictor_TL%Predictor_Subset(M,18,K) &   
        	   + TC%ODCAPS_Subset(M)%C(19,K,I) * Predictor_TL%Predictor_Subset(M,19,K) &   
        	   + TC%ODCAPS_Subset(M)%C(20,K,I) * Predictor_TL%Predictor_Subset(M,20,K) &   
        	   + TC%ODCAPS_Subset(M)%C(21,K,I) * Predictor_TL%Predictor_Subset(M,21,K) &   
        	   + TC%ODCAPS_Subset(M)%C(22,K,I) * Predictor_TL%Predictor_Subset(M,22,K) &   
        	   + TC%ODCAPS_Subset(M)%C(23,K,I) * Predictor_TL%Predictor_Subset(M,23,K) &   
        	   + TC%ODCAPS_Subset(M)%C(24,K,I) * Predictor_TL%Predictor_Subset(M,24,K) &   
        	   + TC%ODCAPS_Subset(M)%C(25,K,I) * Predictor_TL%Predictor_Subset(M,25,K) )   

            IF ( KOZO < ZERO ) THEN
               KOZO = ZERO
               KOZO_TL = ZERO
            ELSE IF ( KOZO > TEN ) THEN
               KOZO = TEN
               KOZO_TL = ZERO
            ENDIF

       !    --------------------------
       !    Compute the water abs coef
       !    --------------------------
            IF (LH2O) THEN
       !    Not an OPTRAN water channel
            
              KW( K ) = TC%Tuning_Multiple(2,J) &
         	      * ( TC%ODCAPS_Subset(M)%C(26,K,I) * Predictor%Predictor_Subset(M,26,K) & 
        	        + TC%ODCAPS_Subset(M)%C(27,K,I) * Predictor%Predictor_Subset(M,27,K) & 
        	        + TC%ODCAPS_Subset(M)%C(28,K,I) * Predictor%Predictor_Subset(M,28,K) & 
        	        + TC%ODCAPS_Subset(M)%C(29,K,I) * Predictor%Predictor_Subset(M,29,K) & 
        	        + TC%ODCAPS_Subset(M)%C(30,K,I) * Predictor%Predictor_Subset(M,30,K) & 
        	        + TC%ODCAPS_Subset(M)%C(31,K,I) * Predictor%Predictor_Subset(M,31,K) &
        	        + TC%ODCAPS_Subset(M)%C(32,K,I) * Predictor%Predictor_Subset(M,32,K) & 
        	        + TC%ODCAPS_Subset(M)%C(33,K,I) * Predictor%Predictor_Subset(M,33,K) & 
        	        + TC%ODCAPS_Subset(M)%C(34,K,I) * Predictor%Predictor_Subset(M,34,K) & 
        	        + TC%ODCAPS_Subset(M)%C(35,K,I) * Predictor%Predictor_Subset(M,35,K) & 
        	        + TC%ODCAPS_Subset(M)%C(36,K,I) * Predictor%Predictor_Subset(M,36,K) )
 
              KW_TL( K ) = TC%Tuning_Multiple(2,J) &
         	      * ( TC%ODCAPS_Subset(M)%C(26,K,I) * Predictor_TL%Predictor_Subset(M,26,K) & 
        	        + TC%ODCAPS_Subset(M)%C(27,K,I) * Predictor_TL%Predictor_Subset(M,27,K) & 
        	        + TC%ODCAPS_Subset(M)%C(28,K,I) * Predictor_TL%Predictor_Subset(M,28,K) & 
        	        + TC%ODCAPS_Subset(M)%C(29,K,I) * Predictor_TL%Predictor_Subset(M,29,K) & 
        	        + TC%ODCAPS_Subset(M)%C(30,K,I) * Predictor_TL%Predictor_Subset(M,30,K) & 
        	        + TC%ODCAPS_Subset(M)%C(31,K,I) * Predictor_TL%Predictor_Subset(M,31,K) &
        	        + TC%ODCAPS_Subset(M)%C(32,K,I) * Predictor_TL%Predictor_Subset(M,32,K) & 
        	        + TC%ODCAPS_Subset(M)%C(33,K,I) * Predictor_TL%Predictor_Subset(M,33,K) & 
        	        + TC%ODCAPS_Subset(M)%C(34,K,I) * Predictor_TL%Predictor_Subset(M,34,K) & 
        	        + TC%ODCAPS_Subset(M)%C(35,K,I) * Predictor_TL%Predictor_Subset(M,35,K) & 
        	        + TC%ODCAPS_Subset(M)%C(36,K,I) * Predictor_TL%Predictor_Subset(M,36,K) )
              IF ( KW( K ) < ZERO ) THEN
	         KW( K ) = ZERO
	         KW_TL( K ) = ZERO
              ELSE IF( KW( K ) > TEN ) THEN
                 KW( K ) = TEN
                 KW_TL( K ) = ZERO
	      ENDIF
	      	 
            ENDIF

       !    --------------------------
       !    The other gases abs coef
       !    --------------------------
      	    KCO  = ZERO
	    KMET = ZERO

      	    KCO_TL  = ZERO
	    KMET_TL = ZERO

	   CASE ( 3 )
	   
       !    --------------------------- 		   
       !    Compute the water continuum 		   
       !    --------------------------- 		   
            KCON = TC%Tuning_Multiple(3,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(1,K,I) * Predictor%Predictor_Subset(M,1,K) &
        	   + TC%ODCAPS_Subset(M)%C(2,K,I) * Predictor%Predictor_Subset(M,2,K) &
        	   + TC%ODCAPS_Subset(M)%C(3,K,I) * Predictor%Predictor_Subset(M,3,K) &
        	   + TC%ODCAPS_Subset(M)%C(4,K,I) * Predictor%Predictor_Subset(M,4,K) &
        	   + TC%ODCAPS_Subset(M)%C(5,K,I) * Predictor%Predictor_Subset(M,5,K) &
        	   + TC%ODCAPS_Subset(M)%C(6,K,I) * Predictor%Predictor_Subset(M,6,K) &
        	   + TC%ODCAPS_Subset(M)%C(7,K,I) * Predictor%Predictor_Subset(M,7,K) )
        		 
            KCON_TL = TC%Tuning_Multiple(3,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(1,K,I) * Predictor_TL%Predictor_Subset(M,1,K) &
        	   + TC%ODCAPS_Subset(M)%C(2,K,I) * Predictor_TL%Predictor_Subset(M,2,K) &
        	   + TC%ODCAPS_Subset(M)%C(3,K,I) * Predictor_TL%Predictor_Subset(M,3,K) &
        	   + TC%ODCAPS_Subset(M)%C(4,K,I) * Predictor_TL%Predictor_Subset(M,4,K) &
        	   + TC%ODCAPS_Subset(M)%C(5,K,I) * Predictor_TL%Predictor_Subset(M,5,K) &
        	   + TC%ODCAPS_Subset(M)%C(6,K,I) * Predictor_TL%Predictor_Subset(M,6,K) &
        	   + TC%ODCAPS_Subset(M)%C(7,K,I) * Predictor_TL%Predictor_Subset(M,7,K) )
 
!            KCON_TL = ZERO				   

            IF ( KCON < ZERO ) THEN		   
               KCON = ZERO				   
               KCON_TL = ZERO				   
            ELSE IF ( KCON > TEN ) THEN 		   
               KCON = TEN				   
               KCON_TL = ZERO				   
            ENDIF					   
  
       !    -----------------------------
       !    Calc the fixed gases abs coef
       !    -----------------------------
            KFIXX = TC%Tuning_Multiple(1,J) &
        	 * ( TC%ODCAPS_Subset(M)%C( 8,K,I) * Predictor%Predictor_Subset(M, 8,K) &
        	   + TC%ODCAPS_Subset(M)%C( 9,K,I) * Predictor%Predictor_Subset(M, 9,K) &
        	   + TC%ODCAPS_Subset(M)%C(10,K,I) * Predictor%Predictor_Subset(M,10,K) &
        	   + TC%ODCAPS_Subset(M)%C(11,K,I) * Predictor%Predictor_Subset(M,11,K) &
        	   + TC%ODCAPS_Subset(M)%C(12,K,I) * Predictor%Predictor_Subset(M,12,K) &
        	   + TC%ODCAPS_Subset(M)%C(13,K,I) * Predictor%Predictor_Subset(M,13,K) &
        	   + TC%ODCAPS_Subset(M)%C(14,K,I) * Predictor%Predictor_Subset(M,14,K) &
        	   + TC%ODCAPS_Subset(M)%C(15,K,I) * Predictor%Predictor_Subset(M,15,K) )
 
            KFIXX_TL = TC%Tuning_Multiple(1,J) &
        	 * ( TC%ODCAPS_Subset(M)%C( 8,K,I) * Predictor_TL%Predictor_Subset(M, 8,K) &
        	   + TC%ODCAPS_Subset(M)%C( 9,K,I) * Predictor_TL%Predictor_Subset(M, 9,K) &
        	   + TC%ODCAPS_Subset(M)%C(10,K,I) * Predictor_TL%Predictor_Subset(M,10,K) &
        	   + TC%ODCAPS_Subset(M)%C(11,K,I) * Predictor_TL%Predictor_Subset(M,11,K) &
        	   + TC%ODCAPS_Subset(M)%C(12,K,I) * Predictor_TL%Predictor_Subset(M,12,K) &
        	   + TC%ODCAPS_Subset(M)%C(13,K,I) * Predictor_TL%Predictor_Subset(M,13,K) &
        	   + TC%ODCAPS_Subset(M)%C(14,K,I) * Predictor_TL%Predictor_Subset(M,14,K) &
        	   + TC%ODCAPS_Subset(M)%C(15,K,I) * Predictor_TL%Predictor_Subset(M,15,K) )

            KFIX_TL = KFIXX_TL * Predictor%Fix_Amount_Multiplier( K ) &  
                    + KFIXX * Predictor_TL%Fix_Amount_Multiplier( K )

            KFIX = KFIXX * Predictor%Fix_Amount_Multiplier( K )
!            KFIX_TL = ZERO     
            IF ( KFIX < ZERO ) THEN	     
               KFIX = ZERO			
               KFIX_TL = ZERO			
            ELSE IF ( KFIX > TEN ) THEN      
               KFIX = TEN			
               KFIX_TL = ZERO			
            ENDIF				

 
       !    ----------------------------
       !    Compute the methane abs coef
       !    ----------------------------
 	    KMET = TC%Tuning_Multiple(6,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(16,K,I) * Predictor%Predictor_Subset(M,16,K) &   
        	   + TC%ODCAPS_Subset(M)%C(17,K,I) * Predictor%Predictor_Subset(M,17,K) &   
        	   + TC%ODCAPS_Subset(M)%C(18,K,I) * Predictor%Predictor_Subset(M,18,K) &   
        	   + TC%ODCAPS_Subset(M)%C(19,K,I) * Predictor%Predictor_Subset(M,19,K) &   
        	   + TC%ODCAPS_Subset(M)%C(20,K,I) * Predictor%Predictor_Subset(M,20,K) &   
        	   + TC%ODCAPS_Subset(M)%C(21,K,I) * Predictor%Predictor_Subset(M,21,K) &   
        	   + TC%ODCAPS_Subset(M)%C(22,K,I) * Predictor%Predictor_Subset(M,22,K) &   
        	   + TC%ODCAPS_Subset(M)%C(23,K,I) * Predictor%Predictor_Subset(M,23,K) &   
        	   + TC%ODCAPS_Subset(M)%C(24,K,I) * Predictor%Predictor_Subset(M,24,K) )   

 	    KMET_TL = TC%Tuning_Multiple(6,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(16,K,I) * Predictor_TL%Predictor_Subset(M,16,K) &   
        	   + TC%ODCAPS_Subset(M)%C(17,K,I) * Predictor_TL%Predictor_Subset(M,17,K) &   
        	   + TC%ODCAPS_Subset(M)%C(18,K,I) * Predictor_TL%Predictor_Subset(M,18,K) &   
        	   + TC%ODCAPS_Subset(M)%C(19,K,I) * Predictor_TL%Predictor_Subset(M,19,K) &   
        	   + TC%ODCAPS_Subset(M)%C(20,K,I) * Predictor_TL%Predictor_Subset(M,20,K) &   
        	   + TC%ODCAPS_Subset(M)%C(21,K,I) * Predictor_TL%Predictor_Subset(M,21,K) &   
        	   + TC%ODCAPS_Subset(M)%C(22,K,I) * Predictor_TL%Predictor_Subset(M,22,K) &   
        	   + TC%ODCAPS_Subset(M)%C(23,K,I) * Predictor_TL%Predictor_Subset(M,23,K) &   
        	   + TC%ODCAPS_Subset(M)%C(24,K,I) * Predictor_TL%Predictor_Subset(M,24,K) )   

!            KMET_TL = ZERO
 
             IF ( KMET < ZERO ) THEN
                KMET = ZERO
                KMET_TL = ZERO
             ELSEIF ( KMET > TEN ) THEN
                KMET = TEN
                KMET_TL = ZERO
             ENDIF
	     
       !    --------------------------
       !    Compute the water abs coef
       !    --------------------------
            IF (LH2O) THEN
       !    Not an OPTRAN water channel
            
              KW( K ) = TC%Tuning_Multiple(2,J) &
         	      * ( TC%ODCAPS_Subset(M)%C(25,K,I) * Predictor%Predictor_Subset(M,25,K) &
		        + TC%ODCAPS_Subset(M)%C(26,K,I) * Predictor%Predictor_Subset(M,26,K) & 
        	        + TC%ODCAPS_Subset(M)%C(27,K,I) * Predictor%Predictor_Subset(M,27,K) & 
        	        + TC%ODCAPS_Subset(M)%C(28,K,I) * Predictor%Predictor_Subset(M,28,K) & 
        	        + TC%ODCAPS_Subset(M)%C(29,K,I) * Predictor%Predictor_Subset(M,29,K) & 
        	        + TC%ODCAPS_Subset(M)%C(30,K,I) * Predictor%Predictor_Subset(M,30,K) & 
        	        + TC%ODCAPS_Subset(M)%C(31,K,I) * Predictor%Predictor_Subset(M,31,K) &
        	        + TC%ODCAPS_Subset(M)%C(32,K,I) * Predictor%Predictor_Subset(M,32,K) & 
        	        + TC%ODCAPS_Subset(M)%C(33,K,I) * Predictor%Predictor_Subset(M,33,K) & 
        	        + TC%ODCAPS_Subset(M)%C(34,K,I) * Predictor%Predictor_Subset(M,34,K) & 
        	        + TC%ODCAPS_Subset(M)%C(35,K,I) * Predictor%Predictor_Subset(M,35,K) ) 

              KW_TL( K ) = TC%Tuning_Multiple(2,J) &
         	      * ( TC%ODCAPS_Subset(M)%C(25,K,I) * Predictor_TL%Predictor_Subset(M,25,K) &
		        + TC%ODCAPS_Subset(M)%C(26,K,I) * Predictor_TL%Predictor_Subset(M,26,K) & 
        	        + TC%ODCAPS_Subset(M)%C(27,K,I) * Predictor_TL%Predictor_Subset(M,27,K) & 
        	        + TC%ODCAPS_Subset(M)%C(28,K,I) * Predictor_TL%Predictor_Subset(M,28,K) & 
        	        + TC%ODCAPS_Subset(M)%C(29,K,I) * Predictor_TL%Predictor_Subset(M,29,K) & 
        	        + TC%ODCAPS_Subset(M)%C(30,K,I) * Predictor_TL%Predictor_Subset(M,30,K) & 
        	        + TC%ODCAPS_Subset(M)%C(31,K,I) * Predictor_TL%Predictor_Subset(M,31,K) &
        	        + TC%ODCAPS_Subset(M)%C(32,K,I) * Predictor_TL%Predictor_Subset(M,32,K) & 
        	        + TC%ODCAPS_Subset(M)%C(33,K,I) * Predictor_TL%Predictor_Subset(M,33,K) & 
        	        + TC%ODCAPS_Subset(M)%C(34,K,I) * Predictor_TL%Predictor_Subset(M,34,K) & 
        	        + TC%ODCAPS_Subset(M)%C(35,K,I) * Predictor_TL%Predictor_Subset(M,35,K) ) 
  
              IF ( KW( K ) < ZERO ) THEN
	         KW( K ) = ZERO
	         KW_TL( K ) = ZERO
	      ENDIF	 
            ENDIF

       !    --------------------------
       !    The other gases abs coef
       !    --------------------------
      	    KCO  = ZERO
	    KOZO = ZERO
	    
      	    KCO_TL  = ZERO
	    KOZO_TL = ZERO
	    
	   CASE ( 4 )
	   
       !    --------------------------- 		   
       !    Compute the water continuum 		   
       !    --------------------------- 		   
            KCON = TC%Tuning_Multiple(3,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(1,K,I) * Predictor_Subset(M-3,1,K) &
        	   + TC%ODCAPS_Subset(M)%C(2,K,I) * Predictor_Subset(M-3,2,K) &
        	   + TC%ODCAPS_Subset(M)%C(3,K,I) * Predictor_Subset(M-3,3,K) &
        	   + TC%ODCAPS_Subset(M)%C(4,K,I) * Predictor_Subset(M-3,4,K) &
        	   + TC%ODCAPS_Subset(M)%C(5,K,I) * Predictor_Subset(M-3,5,K) &
        	   + TC%ODCAPS_Subset(M)%C(6,K,I) * Predictor_Subset(M-3,6,K) &
        	   + TC%ODCAPS_Subset(M)%C(7,K,I) * Predictor_Subset(M-3,7,K) )
        		 
            KCON_TL = TC%Tuning_Multiple(3,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(1,K,I) * Predictor_Subset_TL(M-3,1,K) &
        	   + TC%ODCAPS_Subset(M)%C(2,K,I) * Predictor_Subset_TL(M-3,2,K) &
        	   + TC%ODCAPS_Subset(M)%C(3,K,I) * Predictor_Subset_TL(M-3,3,K) &
        	   + TC%ODCAPS_Subset(M)%C(4,K,I) * Predictor_Subset_TL(M-3,4,K) &
        	   + TC%ODCAPS_Subset(M)%C(5,K,I) * Predictor_Subset_TL(M-3,5,K) &
        	   + TC%ODCAPS_Subset(M)%C(6,K,I) * Predictor_Subset_TL(M-3,6,K) &
        	   + TC%ODCAPS_Subset(M)%C(7,K,I) * Predictor_Subset_TL(M-3,7,K) )
!            KCON_TL = ZERO
            IF ( KCON < ZERO ) THEN		   
               KCON_TL = ZERO				   
               KCON = ZERO				   
            ELSE IF ( KCON > TEN ) THEN 		   
               KCON = TEN				   
               KCON_TL = ZERO				   
            ENDIF					   
  
       !    -----------------------------
       !    Calc the fixed gases abs coef
       !    -----------------------------
            KFIXX = TC%Tuning_Multiple(1,J) &
        	 * ( TC%ODCAPS_Subset(M)%C( 8,K,I) * Predictor_Subset(M-3, 8,K) &
        	   + TC%ODCAPS_Subset(M)%C( 9,K,I) * Predictor_Subset(M-3, 9,K) &
        	   + TC%ODCAPS_Subset(M)%C(10,K,I) * Predictor_Subset(M-3,10,K) &
        	   + TC%ODCAPS_Subset(M)%C(11,K,I) * Predictor_Subset(M-3,11,K) &
        	   + TC%ODCAPS_Subset(M)%C(12,K,I) * Predictor_Subset(M-3,12,K) &
        	   + TC%ODCAPS_Subset(M)%C(13,K,I) * Predictor_Subset(M-3,13,K) &
        	   + TC%ODCAPS_Subset(M)%C(14,K,I) * Predictor_Subset(M-3,14,K) &
        	   + TC%ODCAPS_Subset(M)%C(15,K,I) * Predictor_Subset(M-3,15,K) &
        	   + TC%ODCAPS_Subset(M)%C(16,K,I) * Predictor_Subset(M-3,16,K) &
        	   + TC%ODCAPS_Subset(M)%C(17,K,I) * Predictor_Subset(M-3,17,K) &
        	   + TC%ODCAPS_Subset(M)%C(18,K,I) * Predictor_Subset(M-3,18,K) )
 
            KFIXX_TL = TC%Tuning_Multiple(1,J) &
        	 * ( TC%ODCAPS_Subset(M)%C( 8,K,I) * Predictor_Subset_TL(M-3, 8,K) &
        	   + TC%ODCAPS_Subset(M)%C( 9,K,I) * Predictor_Subset_TL(M-3, 9,K) &
        	   + TC%ODCAPS_Subset(M)%C(10,K,I) * Predictor_Subset_TL(M-3,10,K) &
        	   + TC%ODCAPS_Subset(M)%C(11,K,I) * Predictor_Subset_TL(M-3,11,K) &
        	   + TC%ODCAPS_Subset(M)%C(12,K,I) * Predictor_Subset_TL(M-3,12,K) &
        	   + TC%ODCAPS_Subset(M)%C(13,K,I) * Predictor_Subset_TL(M-3,13,K) &
        	   + TC%ODCAPS_Subset(M)%C(14,K,I) * Predictor_Subset_TL(M-3,14,K) &
        	   + TC%ODCAPS_Subset(M)%C(15,K,I) * Predictor_Subset_TL(M-3,15,K) &
        	   + TC%ODCAPS_Subset(M)%C(16,K,I) * Predictor_Subset_TL(M-3,16,K) &
        	   + TC%ODCAPS_Subset(M)%C(17,K,I) * Predictor_Subset_TL(M-3,17,K) &
        	   + TC%ODCAPS_Subset(M)%C(18,K,I) * Predictor_Subset_TL(M-3,18,K) )

            KFIX_TL = KFIXX_TL * Predictor%Fix_Amount_Multiplier( K ) &  
                    + KFIXX * Predictor_TL%Fix_Amount_Multiplier( K )

            KFIX = KFIXX * Predictor%Fix_Amount_Multiplier( K )
!            KFIX_TL = ZERO
            IF ( KFIX < ZERO ) THEN	     
               KFIX = ZERO			
               KFIX_TL = ZERO			
            ELSE IF ( KFIX > TEN ) THEN      
               KFIX = TEN			
               KFIX_TL = ZERO			
            ENDIF				

 
       !    -----------------------
       !    Compute the CO abs coef
       !    -----------------------
            KCO = TC%Tuning_Multiple(5,J) &
        	* ( TC%ODCAPS_Subset(M)%C(19,K,I) * Predictor_Subset(M-3,19,K) & 
         	  + TC%ODCAPS_Subset(M)%C(20,K,I) * Predictor_Subset(M-3,20,K) & 
        	  + TC%ODCAPS_Subset(M)%C(21,K,I) * Predictor_Subset(M-3,21,K) & 
        	  + TC%ODCAPS_Subset(M)%C(22,K,I) * Predictor_Subset(M-3,22,K) & 
        	  + TC%ODCAPS_Subset(M)%C(23,K,I) * Predictor_Subset(M-3,23,K) & 
        	  + TC%ODCAPS_Subset(M)%C(24,K,I) * Predictor_Subset(M-3,24,K) & 
        	  + TC%ODCAPS_Subset(M)%C(25,K,I) * Predictor_Subset(M-3,25,K) & 
        	  + TC%ODCAPS_Subset(M)%C(26,K,I) * Predictor_Subset(M-3,26,K) & 
        	  + TC%ODCAPS_Subset(M)%C(27,K,I) * Predictor_Subset(M-3,27,K) & 
        	  + TC%ODCAPS_Subset(M)%C(28,K,I) * Predictor_Subset(M-3,28,K) & 
        	  + TC%ODCAPS_Subset(M)%C(29,K,I) * Predictor_Subset(M-3,29,K) ) 
	    
            KCO_TL = TC%Tuning_Multiple(5,J) &
        	* ( TC%ODCAPS_Subset(M)%C(19,K,I) * Predictor_Subset_TL(M-3,19,K) & 
         	  + TC%ODCAPS_Subset(M)%C(20,K,I) * Predictor_Subset_TL(M-3,20,K) & 
        	  + TC%ODCAPS_Subset(M)%C(21,K,I) * Predictor_Subset_TL(M-3,21,K) & 
        	  + TC%ODCAPS_Subset(M)%C(22,K,I) * Predictor_Subset_TL(M-3,22,K) & 
        	  + TC%ODCAPS_Subset(M)%C(23,K,I) * Predictor_Subset_TL(M-3,23,K) & 
        	  + TC%ODCAPS_Subset(M)%C(24,K,I) * Predictor_Subset_TL(M-3,24,K) & 
        	  + TC%ODCAPS_Subset(M)%C(25,K,I) * Predictor_Subset_TL(M-3,25,K) & 
        	  + TC%ODCAPS_Subset(M)%C(26,K,I) * Predictor_Subset_TL(M-3,26,K) & 
        	  + TC%ODCAPS_Subset(M)%C(27,K,I) * Predictor_Subset_TL(M-3,27,K) & 
        	  + TC%ODCAPS_Subset(M)%C(28,K,I) * Predictor_Subset_TL(M-3,28,K) & 
        	  + TC%ODCAPS_Subset(M)%C(29,K,I) * Predictor_Subset_TL(M-3,29,K) ) 

!            KCO_TL = ZERO		  

            IF ( KCO < ZERO ) THEN	  
               KCO = ZERO		  
               KCO_TL = ZERO		  
            ELSEIF ( KCO > TEN ) THEN 
               KCO = TEN		  
               KCO_TL = ZERO		  
            ENDIF			  

       !    --------------------------
       !    Compute the ozone abs coef
       !    --------------------------
            KOZO = TC%Tuning_Multiple(4,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(30,K,I) * Predictor_Subset(M-3,30,K) &   
        	   + TC%ODCAPS_Subset(M)%C(31,K,I) * Predictor_Subset(M-3,31,K) &   
        	   + TC%ODCAPS_Subset(M)%C(32,K,I) * Predictor_Subset(M-3,32,K) )   
   
            KOZO_TL = TC%Tuning_Multiple(4,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(30,K,I) * Predictor_Subset_TL(M-3,30,K) &   
        	   + TC%ODCAPS_Subset(M)%C(31,K,I) * Predictor_Subset_TL(M-3,31,K) &   
        	   + TC%ODCAPS_Subset(M)%C(32,K,I) * Predictor_Subset_TL(M-3,32,K) )   

            IF ( KOZO < ZERO ) THEN
               KOZO = ZERO
               KOZO_TL = ZERO
            ELSE IF ( KOZO > TEN ) THEN
               KOZO = TEN
               KOZO_TL = ZERO
            ENDIF
 
       !    --------------------------
       !    Compute the water abs coef
       !    --------------------------
            IF (LH2O) THEN
       !    Not an OPTRAN water channel
            
              KW( K ) = TC%Tuning_Multiple(2,J) &
		      * ( TC%ODCAPS_Subset(M)%C(33,K,I) * Predictor_Subset(M-3,33,K) &
		        + TC%ODCAPS_Subset(M)%C(34,K,I) * Predictor_Subset(M-3,34,K) &
 		        + TC%ODCAPS_Subset(M)%C(35,K,I) * Predictor_Subset(M-3,35,K) &
		        + TC%ODCAPS_Subset(M)%C(36,K,I) * Predictor_Subset(M-3,36,K) & 
        	        + TC%ODCAPS_Subset(M)%C(37,K,I) * Predictor_Subset(M-3,37,K) & 
        	        + TC%ODCAPS_Subset(M)%C(38,K,I) * Predictor_Subset(M-3,38,K) & 
        	        + TC%ODCAPS_Subset(M)%C(39,K,I) * Predictor_Subset(M-3,39,K) & 
        	        + TC%ODCAPS_Subset(M)%C(40,K,I) * Predictor_Subset(M-3,40,K) & 
        	        + TC%ODCAPS_Subset(M)%C(41,K,I) * Predictor_Subset(M-3,41,K) &
        	        + TC%ODCAPS_Subset(M)%C(42,K,I) * Predictor_Subset(M-3,42,K) & 
        	        + TC%ODCAPS_Subset(M)%C(43,K,I) * Predictor_Subset(M-3,43,K) & 
        	        + TC%ODCAPS_Subset(M)%C(44,K,I) * Predictor_Subset(M-3,44,K) & 
        	        + TC%ODCAPS_Subset(M)%C(45,K,I) * Predictor_Subset(M-3,45,K) ) 

              KW_TL( K ) = TC%Tuning_Multiple(2,J) &
		      * ( TC%ODCAPS_Subset(M)%C(33,K,I) * Predictor_Subset_TL(M-3,33,K) &
		        + TC%ODCAPS_Subset(M)%C(34,K,I) * Predictor_Subset_TL(M-3,34,K) &
 		        + TC%ODCAPS_Subset(M)%C(35,K,I) * Predictor_Subset_TL(M-3,35,K) &
		        + TC%ODCAPS_Subset(M)%C(36,K,I) * Predictor_Subset_TL(M-3,36,K) & 
        	        + TC%ODCAPS_Subset(M)%C(37,K,I) * Predictor_Subset_TL(M-3,37,K) & 
        	        + TC%ODCAPS_Subset(M)%C(38,K,I) * Predictor_Subset_TL(M-3,38,K) & 
        	        + TC%ODCAPS_Subset(M)%C(39,K,I) * Predictor_Subset_TL(M-3,39,K) & 
        	        + TC%ODCAPS_Subset(M)%C(40,K,I) * Predictor_Subset_TL(M-3,40,K) & 
        	        + TC%ODCAPS_Subset(M)%C(41,K,I) * Predictor_Subset_TL(M-3,41,K) &
        	        + TC%ODCAPS_Subset(M)%C(42,K,I) * Predictor_Subset_TL(M-3,42,K) & 
        	        + TC%ODCAPS_Subset(M)%C(43,K,I) * Predictor_Subset_TL(M-3,43,K) & 
        	        + TC%ODCAPS_Subset(M)%C(44,K,I) * Predictor_Subset_TL(M-3,44,K) & 
        	        + TC%ODCAPS_Subset(M)%C(45,K,I) * Predictor_Subset_TL(M-3,45,K) ) 

              IF ( KW( K ) < ZERO ) THEN
	         KW( K ) = ZERO
	         KW_TL( K ) = ZERO
              ELSE IF( KW( K ) > TEN ) THEN
                 KW( K ) = TEN
	         KW_TL( K ) = ZERO
 	      ENDIF	 
            ENDIF

       !    --------------------------
       !    The other gases abs coef
       !    --------------------------
 	    KMET = ZERO
 	    KMET_TL = ZERO

	   CASE ( 5 )
	   
       !    --------------------------- 		   
       !    Compute the water continuum 		   
       !    --------------------------- 		   
            KCON = TC%Tuning_Multiple(3,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(1,K,I) * Predictor_Subset(M-3,1,K) &
        	   + TC%ODCAPS_Subset(M)%C(2,K,I) * Predictor_Subset(M-3,2,K) &
        	   + TC%ODCAPS_Subset(M)%C(3,K,I) * Predictor_Subset(M-3,3,K) &
        	   + TC%ODCAPS_Subset(M)%C(4,K,I) * Predictor_Subset(M-3,4,K) &
        	   + TC%ODCAPS_Subset(M)%C(5,K,I) * Predictor_Subset(M-3,5,K) &
        	   + TC%ODCAPS_Subset(M)%C(6,K,I) * Predictor_Subset(M-3,6,K) &
        	   + TC%ODCAPS_Subset(M)%C(7,K,I) * Predictor_Subset(M-3,7,K) )
        		 
            KCON_TL = TC%Tuning_Multiple(3,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(1,K,I) * Predictor_Subset_TL(M-3,1,K) &
        	   + TC%ODCAPS_Subset(M)%C(2,K,I) * Predictor_Subset_TL(M-3,2,K) &
        	   + TC%ODCAPS_Subset(M)%C(3,K,I) * Predictor_Subset_TL(M-3,3,K) &
        	   + TC%ODCAPS_Subset(M)%C(4,K,I) * Predictor_Subset_TL(M-3,4,K) &
        	   + TC%ODCAPS_Subset(M)%C(5,K,I) * Predictor_Subset_TL(M-3,5,K) &
        	   + TC%ODCAPS_Subset(M)%C(6,K,I) * Predictor_Subset_TL(M-3,6,K) &
        	   + TC%ODCAPS_Subset(M)%C(7,K,I) * Predictor_Subset_TL(M-3,7,K) )
!            KCON_TL = ZERO	
            IF ( KCON < ZERO ) THEN		   
               KCON = ZERO				   
               KCON_TL = ZERO				   
            ELSE IF ( KCON > TEN ) THEN 		   
               KCON = TEN				   
               KCON_TL = ZERO				   
            ENDIF					   
  
       !    -----------------------------
       !    Calc the fixed gases abs coef
       !    -----------------------------
            KFIXX = TC%Tuning_Multiple(1,J) &
        	 * ( TC%ODCAPS_Subset(M)%C( 8,K,I) * Predictor_Subset(M-3, 8,K) &
        	   + TC%ODCAPS_Subset(M)%C( 9,K,I) * Predictor_Subset(M-3, 9,K) &
        	   + TC%ODCAPS_Subset(M)%C(10,K,I) * Predictor_Subset(M-3,10,K) &
        	   + TC%ODCAPS_Subset(M)%C(11,K,I) * Predictor_Subset(M-3,11,K) &
        	   + TC%ODCAPS_Subset(M)%C(12,K,I) * Predictor_Subset(M-3,12,K) &
        	   + TC%ODCAPS_Subset(M)%C(13,K,I) * Predictor_Subset(M-3,13,K) &
        	   + TC%ODCAPS_Subset(M)%C(14,K,I) * Predictor_Subset(M-3,14,K) &
        	   + TC%ODCAPS_Subset(M)%C(15,K,I) * Predictor_Subset(M-3,15,K) &
        	   + TC%ODCAPS_Subset(M)%C(16,K,I) * Predictor_Subset(M-3,16,K) &
        	   + TC%ODCAPS_Subset(M)%C(17,K,I) * Predictor_Subset(M-3,17,K) &
        	   + TC%ODCAPS_Subset(M)%C(18,K,I) * Predictor_Subset(M-3,18,K) )
 
            KFIXX_TL = TC%Tuning_Multiple(1,J) &
        	 * ( TC%ODCAPS_Subset(M)%C( 8,K,I) * Predictor_Subset_TL(M-3, 8,K) &
        	   + TC%ODCAPS_Subset(M)%C( 9,K,I) * Predictor_Subset_TL(M-3, 9,K) &
        	   + TC%ODCAPS_Subset(M)%C(10,K,I) * Predictor_Subset_TL(M-3,10,K) &
        	   + TC%ODCAPS_Subset(M)%C(11,K,I) * Predictor_Subset_TL(M-3,11,K) &
        	   + TC%ODCAPS_Subset(M)%C(12,K,I) * Predictor_Subset_TL(M-3,12,K) &
        	   + TC%ODCAPS_Subset(M)%C(13,K,I) * Predictor_Subset_TL(M-3,13,K) &
        	   + TC%ODCAPS_Subset(M)%C(14,K,I) * Predictor_Subset_TL(M-3,14,K) &
        	   + TC%ODCAPS_Subset(M)%C(15,K,I) * Predictor_Subset_TL(M-3,15,K) &
        	   + TC%ODCAPS_Subset(M)%C(16,K,I) * Predictor_Subset_TL(M-3,16,K) &
        	   + TC%ODCAPS_Subset(M)%C(17,K,I) * Predictor_Subset_TL(M-3,17,K) &
        	   + TC%ODCAPS_Subset(M)%C(18,K,I) * Predictor_Subset_TL(M-3,18,K) )

            KFIX_TL = KFIXX_TL * Predictor%Fix_Amount_Multiplier( K ) &  
                    + KFIXX * Predictor_TL%Fix_Amount_Multiplier( K )
            
	    KFIX = KFIXX * Predictor%Fix_Amount_Multiplier( K )
!            KFIX_TL = ZERO		    
            IF ( KFIX < ZERO ) THEN	     
               KFIX = ZERO			
               KFIX_TL = ZERO			
            ELSE IF ( KFIX > TEN ) THEN      
               KFIX = TEN			
               KFIX_TL = ZERO			
            ENDIF				

       !    --------------------------
       !    Compute the water abs coef
       !    --------------------------
            IF (LH2O) THEN
       !    Not an OPTRAN water channel
            
              KW( K ) = TC%Tuning_Multiple(2,J) &
		      * ( TC%ODCAPS_Subset(M)%C(19,K,I) * Predictor_Subset(M-3,19,K) &
		        + TC%ODCAPS_Subset(M)%C(20,K,I) * Predictor_Subset(M-3,20,K) &
 		        + TC%ODCAPS_Subset(M)%C(21,K,I) * Predictor_Subset(M-3,21,K) )

              KW_TL( K ) = TC%Tuning_Multiple(2,J) &
		      * ( TC%ODCAPS_Subset(M)%C(19,K,I) * Predictor_Subset_TL(M-3,19,K) &
		        + TC%ODCAPS_Subset(M)%C(20,K,I) * Predictor_Subset_TL(M-3,20,K) &
 		        + TC%ODCAPS_Subset(M)%C(21,K,I) * Predictor_Subset_TL(M-3,21,K) )
              IF ( KW( K ) < ZERO ) THEN
	         KW( K ) = ZERO
	         KW_TL( K ) = ZERO
              ELSE IF( KW( K ) > TEN ) THEN
                 KW( K ) = TEN
	         KW_TL( K ) = ZERO
              ENDIF
	    ENDIF
 
       !    --------------------------
       !    Compute the ozone abs coef
       !    --------------------------
            KOZO = TC%Tuning_Multiple(4,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(22,K,I) * Predictor_Subset(M-3,22,K) )   
    
            KOZO_TL = TC%Tuning_Multiple(4,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(22,K,I) * Predictor_Subset_TL(M-3,22,K) )
		    
            IF ( KOZO < ZERO ) THEN
               KOZO = ZERO
               KOZO_TL = ZERO
            ELSE IF ( KOZO > TEN ) THEN
               KOZO = TEN
               KOZO_TL = ZERO
            ENDIF

       !    --------------------------
       !    The other gases abs coef
       !    --------------------------
      	    KCO  = ZERO
	    KMET = ZERO

      	    KCO_TL  = ZERO
	    KMET_TL = ZERO
 
	   CASE ( 6 )
	   
       !    --------------------------- 		   
       !    Compute the water continuum 		   
       !    --------------------------- 		   
            KCON = TC%Tuning_Multiple(3,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(1,K,I) * Predictor_Subset(M-3,1,K) &
        	   + TC%ODCAPS_Subset(M)%C(2,K,I) * Predictor_Subset(M-3,2,K) &
        	   + TC%ODCAPS_Subset(M)%C(3,K,I) * Predictor_Subset(M-3,3,K) &
        	   + TC%ODCAPS_Subset(M)%C(4,K,I) * Predictor_Subset(M-3,4,K) &
        	   + TC%ODCAPS_Subset(M)%C(5,K,I) * Predictor_Subset(M-3,5,K) &
        	   + TC%ODCAPS_Subset(M)%C(6,K,I) * Predictor_Subset(M-3,6,K) &
        	   + TC%ODCAPS_Subset(M)%C(7,K,I) * Predictor_Subset(M-3,7,K) )
        		 
            KCON_TL = TC%Tuning_Multiple(3,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(1,K,I) * Predictor_Subset_TL(M-3,1,K) &
        	   + TC%ODCAPS_Subset(M)%C(2,K,I) * Predictor_Subset_TL(M-3,2,K) &
        	   + TC%ODCAPS_Subset(M)%C(3,K,I) * Predictor_Subset_TL(M-3,3,K) &
        	   + TC%ODCAPS_Subset(M)%C(4,K,I) * Predictor_Subset_TL(M-3,4,K) &
        	   + TC%ODCAPS_Subset(M)%C(5,K,I) * Predictor_Subset_TL(M-3,5,K) &
        	   + TC%ODCAPS_Subset(M)%C(6,K,I) * Predictor_Subset_TL(M-3,6,K) &
        	   + TC%ODCAPS_Subset(M)%C(7,K,I) * Predictor_Subset_TL(M-3,7,K) )
!            KCON_TL = ZERO	
            IF ( KCON < ZERO ) THEN		   
               KCON = ZERO				   
               KCON_TL = ZERO				   
            ELSE IF ( KCON > TEN ) THEN 		   
               KCON = TEN				   
               KCON_TL = ZERO				   
            ENDIF					   
  
       !    -----------------------------
       !    Calc the fixed gases abs coef
       !    -----------------------------
            KFIXX = TC%Tuning_Multiple(1,J) &
        	 * ( TC%ODCAPS_Subset(M)%C( 8,K,I) * Predictor_Subset(M-3, 8,K) &
        	   + TC%ODCAPS_Subset(M)%C( 9,K,I) * Predictor_Subset(M-3, 9,K) &
        	   + TC%ODCAPS_Subset(M)%C(10,K,I) * Predictor_Subset(M-3,10,K) &
        	   + TC%ODCAPS_Subset(M)%C(11,K,I) * Predictor_Subset(M-3,11,K) &
        	   + TC%ODCAPS_Subset(M)%C(12,K,I) * Predictor_Subset(M-3,12,K) &
        	   + TC%ODCAPS_Subset(M)%C(13,K,I) * Predictor_Subset(M-3,13,K) &
        	   + TC%ODCAPS_Subset(M)%C(14,K,I) * Predictor_Subset(M-3,14,K) &
        	   + TC%ODCAPS_Subset(M)%C(15,K,I) * Predictor_Subset(M-3,15,K) )
 
            KFIXX_TL = TC%Tuning_Multiple(1,J) &
        	 * ( TC%ODCAPS_Subset(M)%C( 8,K,I) * Predictor_Subset_TL(M-3, 8,K) &
        	   + TC%ODCAPS_Subset(M)%C( 9,K,I) * Predictor_Subset_TL(M-3, 9,K) &
        	   + TC%ODCAPS_Subset(M)%C(10,K,I) * Predictor_Subset_TL(M-3,10,K) &
        	   + TC%ODCAPS_Subset(M)%C(11,K,I) * Predictor_Subset_TL(M-3,11,K) &
        	   + TC%ODCAPS_Subset(M)%C(12,K,I) * Predictor_Subset_TL(M-3,12,K) &
        	   + TC%ODCAPS_Subset(M)%C(13,K,I) * Predictor_Subset_TL(M-3,13,K) &
        	   + TC%ODCAPS_Subset(M)%C(14,K,I) * Predictor_Subset_TL(M-3,14,K) &
        	   + TC%ODCAPS_Subset(M)%C(15,K,I) * Predictor_Subset_TL(M-3,15,K) )
            
	    KFIX_TL = KFIXX_TL * Predictor%Fix_Amount_Multiplier( K ) &  
                    + KFIXX * Predictor_TL%Fix_Amount_Multiplier( K )

	    KFIX = KFIXX * Predictor%Fix_Amount_Multiplier( K )
!            KFIX_TL = ZERO
            IF ( KFIX < ZERO ) THEN	     
               KFIX = ZERO			
               KFIX_TL = ZERO			
            ELSE IF ( KFIX > TEN ) THEN      
               KFIX = TEN			
               KFIX_TL = ZERO			
            ENDIF				

       !    --------------------------
       !    Compute the water abs coef
       !    --------------------------
            IF (LH2O) THEN
       !    Not an OPTRAN water channel
            
              KW( K ) = TC%Tuning_Multiple(2,J) &
		      * ( TC%ODCAPS_Subset(M)%C(16,K,I) * Predictor_Subset(M-3,16,K) &
        	        + TC%ODCAPS_Subset(M)%C(17,K,I) * Predictor_Subset(M-3,17,K) &
        	        + TC%ODCAPS_Subset(M)%C(18,K,I) * Predictor_Subset(M-3,18,K) &
                        + TC%ODCAPS_Subset(M)%C(19,K,I) * Predictor_Subset(M-3,19,K) &
		        + TC%ODCAPS_Subset(M)%C(20,K,I) * Predictor_Subset(M-3,20,K) &
 		        + TC%ODCAPS_Subset(M)%C(21,K,I) * Predictor_Subset(M-3,21,K) &
 		        + TC%ODCAPS_Subset(M)%C(22,K,I) * Predictor_Subset(M-3,22,K) )

              KW_TL( K ) = TC%Tuning_Multiple(2,J) &
		      * ( TC%ODCAPS_Subset(M)%C(16,K,I) * Predictor_Subset_TL(M-3,16,K) &
        	        + TC%ODCAPS_Subset(M)%C(17,K,I) * Predictor_Subset_TL(M-3,17,K) &
        	        + TC%ODCAPS_Subset(M)%C(18,K,I) * Predictor_Subset_TL(M-3,18,K) &
                        + TC%ODCAPS_Subset(M)%C(19,K,I) * Predictor_Subset_TL(M-3,19,K) &
		        + TC%ODCAPS_Subset(M)%C(20,K,I) * Predictor_Subset_TL(M-3,20,K) &
 		        + TC%ODCAPS_Subset(M)%C(21,K,I) * Predictor_Subset_TL(M-3,21,K) &
 		        + TC%ODCAPS_Subset(M)%C(22,K,I) * Predictor_Subset_TL(M-3,22,K) )

              IF ( KW( K ) < ZERO ) THEN
	         KW( K ) = ZERO
	         KW_TL( K ) = ZERO
              ELSE IF( KW( K ) > TEN ) THEN
                 KW( K ) = TEN
	         KW_TL( K ) = ZERO
              ENDIF
	    ENDIF
 
       !    --------------------------
       !    Compute the ozone abs coef
       !    --------------------------
            KOZO = TC%Tuning_Multiple(4,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(23,K,I) * Predictor_Subset(M-3,23,K) )   
    
            KOZO_TL = TC%Tuning_Multiple(4,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(23,K,I) * Predictor_Subset_TL(M-3,23,K) )   

            IF ( KOZO < ZERO ) THEN
               KOZO = ZERO
               KOZO_TL = ZERO
            ELSE IF ( KOZO > TEN ) THEN
               KOZO = TEN
               KOZO_TL = ZERO
            ENDIF

       !    --------------------------
       !    The other gases abs coef
       !    --------------------------
      	    KCO  = ZERO
	    KMET = ZERO
      	    KCO_TL  = ZERO
	    KMET_TL = ZERO
 
	   CASE ( 7 )
	   
       !    --------------------------- 		   
       !    Compute the water continuum 		   
       !    --------------------------- 		   
            KCON = TC%Tuning_Multiple(3,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(1,K,I) * Predictor_Subset(M-3,1,K) &
        	   + TC%ODCAPS_Subset(M)%C(2,K,I) * Predictor_Subset(M-3,2,K) &
        	   + TC%ODCAPS_Subset(M)%C(3,K,I) * Predictor_Subset(M-3,3,K) &
        	   + TC%ODCAPS_Subset(M)%C(4,K,I) * Predictor_Subset(M-3,4,K) &
        	   + TC%ODCAPS_Subset(M)%C(5,K,I) * Predictor_Subset(M-3,5,K) &
        	   + TC%ODCAPS_Subset(M)%C(6,K,I) * Predictor_Subset(M-3,6,K) &
        	   + TC%ODCAPS_Subset(M)%C(7,K,I) * Predictor_Subset(M-3,7,K) )
        		 
            KCON_TL = TC%Tuning_Multiple(3,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(1,K,I) * Predictor_Subset_TL(M-3,1,K) &
        	   + TC%ODCAPS_Subset(M)%C(2,K,I) * Predictor_Subset_TL(M-3,2,K) &
        	   + TC%ODCAPS_Subset(M)%C(3,K,I) * Predictor_Subset_TL(M-3,3,K) &
        	   + TC%ODCAPS_Subset(M)%C(4,K,I) * Predictor_Subset_TL(M-3,4,K) &
        	   + TC%ODCAPS_Subset(M)%C(5,K,I) * Predictor_Subset_TL(M-3,5,K) &
        	   + TC%ODCAPS_Subset(M)%C(6,K,I) * Predictor_Subset_TL(M-3,6,K) &
        	   + TC%ODCAPS_Subset(M)%C(7,K,I) * Predictor_Subset_TL(M-3,7,K) )
!            KCON_TL = ZERO
            IF ( KCON < ZERO ) THEN		   
               KCON = ZERO				   
               KCON_TL = ZERO				   
            ELSE IF ( KCON > TEN ) THEN 		   
               KCON = TEN				   
               KCON_TL = ZERO				   
            ENDIF					   
  
       !    -----------------------------
       !    Calc the fixed gases abs coef
       !    -----------------------------
            KFIXX = TC%Tuning_Multiple(1,J) &
        	 * ( TC%ODCAPS_Subset(M)%C( 8,K,I) * Predictor_Subset(M-3, 8,K) &
        	   + TC%ODCAPS_Subset(M)%C( 9,K,I) * Predictor_Subset(M-3, 9,K) &
        	   + TC%ODCAPS_Subset(M)%C(10,K,I) * Predictor_Subset(M-3,10,K) &
        	   + TC%ODCAPS_Subset(M)%C(11,K,I) * Predictor_Subset(M-3,11,K) &
        	   + TC%ODCAPS_Subset(M)%C(12,K,I) * Predictor_Subset(M-3,12,K) &
        	   + TC%ODCAPS_Subset(M)%C(13,K,I) * Predictor_Subset(M-3,13,K) &
        	   + TC%ODCAPS_Subset(M)%C(14,K,I) * Predictor_Subset(M-3,14,K) &
        	   + TC%ODCAPS_Subset(M)%C(15,K,I) * Predictor_Subset(M-3,15,K) )
 
            KFIXX_TL = TC%Tuning_Multiple(1,J) &
        	 * ( TC%ODCAPS_Subset(M)%C( 8,K,I) * Predictor_Subset_TL(M-3, 8,K) &
        	   + TC%ODCAPS_Subset(M)%C( 9,K,I) * Predictor_Subset_TL(M-3, 9,K) &
        	   + TC%ODCAPS_Subset(M)%C(10,K,I) * Predictor_Subset_TL(M-3,10,K) &
        	   + TC%ODCAPS_Subset(M)%C(11,K,I) * Predictor_Subset_TL(M-3,11,K) &
        	   + TC%ODCAPS_Subset(M)%C(12,K,I) * Predictor_Subset_TL(M-3,12,K) &
        	   + TC%ODCAPS_Subset(M)%C(13,K,I) * Predictor_Subset_TL(M-3,13,K) &
        	   + TC%ODCAPS_Subset(M)%C(14,K,I) * Predictor_Subset_TL(M-3,14,K) &
        	   + TC%ODCAPS_Subset(M)%C(15,K,I) * Predictor_Subset_TL(M-3,15,K) )

            KFIX_TL = KFIXX_TL * Predictor%Fix_Amount_Multiplier( K ) &  
                    + KFIXX * Predictor_TL%Fix_Amount_Multiplier( K )

            KFIX = KFIXX * Predictor%Fix_Amount_Multiplier( K )
!            KFIX_TL = ZERO
            IF ( KFIX < ZERO ) THEN	     
               KFIX = ZERO			
               KFIX_TL = ZERO			
            ELSE IF ( KFIX > TEN ) THEN      
               KFIX = TEN			
               KFIX_TL = ZERO			
            ENDIF				

       !    --------------------------
       !    Compute the water abs coef
       !    --------------------------
            IF (LH2O) THEN
       !    Not an OPTRAN water channel
            
              KW( K ) = TC%Tuning_Multiple(2,J) &
		      * ( TC%ODCAPS_Subset(M)%C(16,K,I) * Predictor_Subset(M-3,16,K) &
        	        + TC%ODCAPS_Subset(M)%C(17,K,I) * Predictor_Subset(M-3,17,K) &
        	        + TC%ODCAPS_Subset(M)%C(18,K,I) * Predictor_Subset(M-3,18,K) &
                        + TC%ODCAPS_Subset(M)%C(19,K,I) * Predictor_Subset(M-3,19,K) &
		        + TC%ODCAPS_Subset(M)%C(20,K,I) * Predictor_Subset(M-3,20,K) &
 		        + TC%ODCAPS_Subset(M)%C(21,K,I) * Predictor_Subset(M-3,21,K) &
 		        + TC%ODCAPS_Subset(M)%C(22,K,I) * Predictor_Subset(M-3,22,K) &
        	        + TC%ODCAPS_Subset(M)%C(23,K,I) * Predictor_Subset(M-3,23,K) &
                        + TC%ODCAPS_Subset(M)%C(24,K,I) * Predictor_Subset(M-3,24,K) &
		        + TC%ODCAPS_Subset(M)%C(25,K,I) * Predictor_Subset(M-3,25,K) &
 		        + TC%ODCAPS_Subset(M)%C(26,K,I) * Predictor_Subset(M-3,26,K) &
 		        + TC%ODCAPS_Subset(M)%C(27,K,I) * Predictor_Subset(M-3,27,K) &
 		        + TC%ODCAPS_Subset(M)%C(28,K,I) * Predictor_Subset(M-3,28,K) )

              KW_TL( K ) = TC%Tuning_Multiple(2,J) &
		      * ( TC%ODCAPS_Subset(M)%C(16,K,I) * Predictor_Subset_TL(M-3,16,K) &
        	        + TC%ODCAPS_Subset(M)%C(17,K,I) * Predictor_Subset_TL(M-3,17,K) &
        	        + TC%ODCAPS_Subset(M)%C(18,K,I) * Predictor_Subset_TL(M-3,18,K) &
                        + TC%ODCAPS_Subset(M)%C(19,K,I) * Predictor_Subset_TL(M-3,19,K) &
		        + TC%ODCAPS_Subset(M)%C(20,K,I) * Predictor_Subset_TL(M-3,20,K) &
 		        + TC%ODCAPS_Subset(M)%C(21,K,I) * Predictor_Subset_TL(M-3,21,K) &
 		        + TC%ODCAPS_Subset(M)%C(22,K,I) * Predictor_Subset_TL(M-3,22,K) &
        	        + TC%ODCAPS_Subset(M)%C(23,K,I) * Predictor_Subset_TL(M-3,23,K) &
                        + TC%ODCAPS_Subset(M)%C(24,K,I) * Predictor_Subset_TL(M-3,24,K) &
		        + TC%ODCAPS_Subset(M)%C(25,K,I) * Predictor_Subset_TL(M-3,25,K) &
 		        + TC%ODCAPS_Subset(M)%C(26,K,I) * Predictor_Subset_TL(M-3,26,K) &
 		        + TC%ODCAPS_Subset(M)%C(27,K,I) * Predictor_Subset_TL(M-3,27,K) &
 		        + TC%ODCAPS_Subset(M)%C(28,K,I) * Predictor_Subset_TL(M-3,28,K) )

              IF ( KW( K ) < ZERO ) THEN
	         KW( K ) = ZERO
	         KW_TL( K ) = ZERO
              ELSE IF( KW( K ) > TEN ) THEN
                 KW( K ) = TEN
	         KW_TL( K ) = ZERO
              ENDIF
	    ENDIF
 
       !    --------------------------
       !    Compute the ozone abs coef
       !    --------------------------
            KOZO = TC%Tuning_Multiple(4,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(29,K,I) * Predictor_Subset(M-3,29,K) )   
    
            KOZO_TL = TC%Tuning_Multiple(4,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(29,K,I) * Predictor_Subset_TL(M-3,29,K) )   

            IF ( KOZO < ZERO ) THEN
               KOZO = ZERO
               KOZO_TL = ZERO
            ELSE IF ( KOZO > TEN ) THEN
               KOZO = TEN
               KOZO_TL = ZERO
            ENDIF

       !    --------------------------
       !    The other gases abs coef
       !    --------------------------
      	    KCO  = ZERO
	    KMET = ZERO

      	    KCO_TL  = ZERO
	    KMET_TL = ZERO
       END SELECT Subset_Select


  !    ----------------------------
  !    Calc change in total optical
  !    depth due to variable CO2
  !    ----------------------------
       IF ( LCO2 .AND. Predictor%CO2_Multiplier( K ) /= ZERO ) THEN
 	  DKCO2X = ( TC%ODCAPS_TraceGas(1)%Trace_Coeff(1,K,ICO2)* TraceGas_Predictors(1,K) & 
	        +   TC%ODCAPS_TraceGas(1)%Trace_Coeff(2,K,ICO2)* TraceGas_Predictors(2,K) &
	        +   TC%ODCAPS_TraceGas(1)%Trace_Coeff(3,K,ICO2)* TraceGas_Predictors(3,K) &
	        +   TC%ODCAPS_TraceGas(1)%Trace_Coeff(4,K,ICO2)* TraceGas_Predictors(4,K) )
 
 	  DKCO2X_TL = ( TC%ODCAPS_TraceGas(1)%Trace_Coeff(1,K,ICO2)* TraceGas_Predictors_TL(1,K) & 
	        +   TC%ODCAPS_TraceGas(1)%Trace_Coeff(2,K,ICO2)* TraceGas_Predictors_TL(2,K) &
	        +   TC%ODCAPS_TraceGas(1)%Trace_Coeff(3,K,ICO2)* TraceGas_Predictors_TL(3,K) &
	        +   TC%ODCAPS_TraceGas(1)%Trace_Coeff(4,K,ICO2)* TraceGas_Predictors_TL(4,K) )

  	  DKCO2_TL = DKCO2X_TL * Predictor%CO2_Multiplier( K ) &
                   + DKCO2X * Predictor_TL%CO2_Multiplier( K )  
	    
  	  DKCO2 = DKCO2X * Predictor%CO2_Multiplier( K ) 
!          DKCO2_TL = ZERO
       ELSE
 	  DKCO2 = ZERO
 	  DKCO2_TL = ZERO
       ENDIF

  !    ----------------------------
  !    Calc change in total optical
  !    depth due to variable SO2
  !    ----------------------------
       IF ( LSO2 .AND. Predictor%SO2_Multiplier( K ) /= ZERO ) THEN
          DKSO2X = ( TC%ODCAPS_TraceGas(2)%Trace_Coeff(1,K,ISO2)* TraceGas_Predictors(1,K) & 
	        +   TC%ODCAPS_TraceGas(2)%Trace_Coeff(2,K,ISO2)* TraceGas_Predictors(2,K) &
	        +   TC%ODCAPS_TraceGas(2)%Trace_Coeff(3,K,ISO2)* TraceGas_Predictors(3,K) &
	        +   TC%ODCAPS_TraceGas(2)%Trace_Coeff(4,K,ISO2)* TraceGas_Predictors(4,K) )
 
          DKSO2X_TL = ( TC%ODCAPS_TraceGas(2)%Trace_Coeff(1,K,ISO2)* TraceGas_Predictors_TL(1,K) & 
	        +   TC%ODCAPS_TraceGas(2)%Trace_Coeff(2,K,ISO2)* TraceGas_Predictors_TL(2,K) &
	        +   TC%ODCAPS_TraceGas(2)%Trace_Coeff(3,K,ISO2)* TraceGas_Predictors_TL(3,K) &
	        +   TC%ODCAPS_TraceGas(2)%Trace_Coeff(4,K,ISO2)* TraceGas_Predictors_TL(4,K) )
  	  
	  DKSO2_TL = DKSO2X_TL * Predictor%SO2_Multiplier( K )  & 
                   + DKSO2X * Predictor_TL%SO2_Multiplier( K )  

          DKSO2 = DKSO2X * Predictor%SO2_Multiplier( K ) 
!          DKSO2_TL = ZERO
       ELSE
          DKSO2 = ZERO
          DKSO2_TL = ZERO
       ENDIF
  
  !    ----------------------------
  !    Calc change in total optical
  !    depth due to variable HNO3
  !    ----------------------------
       IF ( LHNO3 .AND. Predictor%HNO3_Multiplier( K ) /= ZERO ) THEN
 	  DKHNO3X = ( TC%ODCAPS_TraceGas(3)%Trace_Coeff(1,K,IHNO3)* TraceGas_Predictors(1,K) & 
	         +   TC%ODCAPS_TraceGas(3)%Trace_Coeff(2,K,IHNO3)* TraceGas_Predictors(2,K) &
	         +   TC%ODCAPS_TraceGas(3)%Trace_Coeff(3,K,IHNO3)* TraceGas_Predictors(3,K) &
	         +   TC%ODCAPS_TraceGas(3)%Trace_Coeff(4,K,IHNO3)* TraceGas_Predictors(4,K) )
 
 	  DKHNO3X_TL = ( TC%ODCAPS_TraceGas(3)%Trace_Coeff(1,K,IHNO3)* TraceGas_Predictors_TL(1,K) & 
	         +   TC%ODCAPS_TraceGas(3)%Trace_Coeff(2,K,IHNO3)* TraceGas_Predictors_TL(2,K) &
	         +   TC%ODCAPS_TraceGas(3)%Trace_Coeff(3,K,IHNO3)* TraceGas_Predictors_TL(3,K) &
	         +   TC%ODCAPS_TraceGas(3)%Trace_Coeff(4,K,IHNO3)* TraceGas_Predictors_TL(4,K) )

	  DKHNO3_TL = DKHNO3X_TL * Predictor%HNO3_Multiplier( K ) &   
                    + DKHNO3X * Predictor_TL%HNO3_Multiplier( K )  

 	  DKHNO3 = DKHNO3X * Predictor%HNO3_Multiplier( K )
! 	  DKHNO3_TL = ZERO
       ELSE
 	  DKHNO3 = ZERO
 	  DKHNO3_TL = ZERO
       ENDIF
 
  !	----------------------------
  !	Calc change in total optical
  !	depth due to variable N2O
  !	----------------------------
 	IF ( LN2O .AND. Predictor%N2O_Multiplier( K ) /= ZERO ) THEN
 	   DKN2OX = ( TC%ODCAPS_TraceGas(4)%Trace_Coeff(1,K,IN2O)* TraceGas_Predictors(1,K) & 
	         +   TC%ODCAPS_TraceGas(4)%Trace_Coeff(2,K,IN2O)* TraceGas_Predictors(2,K) &
	         +   TC%ODCAPS_TraceGas(4)%Trace_Coeff(3,K,IN2O)* TraceGas_Predictors(3,K) &
	         +   TC%ODCAPS_TraceGas(4)%Trace_Coeff(4,K,IN2O)* TraceGas_Predictors(4,K) &
	         +   TC%ODCAPS_TraceGas(4)%Trace_Coeff(5,K,IN2O)* TraceGas_Predictors(5,K) &
	         +   TC%ODCAPS_TraceGas(4)%Trace_Coeff(6,K,IN2O)* TraceGas_Predictors(6,K) &
	         +   TC%ODCAPS_TraceGas(4)%Trace_Coeff(7,K,IN2O)* TraceGas_Predictors(7,K) )
  	   
 	   DKN2OX_TL = ( TC%ODCAPS_TraceGas(4)%Trace_Coeff(1,K,IN2O)* TraceGas_Predictors_TL(1,K) & 
	         +   TC%ODCAPS_TraceGas(4)%Trace_Coeff(2,K,IN2O)* TraceGas_Predictors_TL(2,K) &
	         +   TC%ODCAPS_TraceGas(4)%Trace_Coeff(3,K,IN2O)* TraceGas_Predictors_TL(3,K) &
	         +   TC%ODCAPS_TraceGas(4)%Trace_Coeff(4,K,IN2O)* TraceGas_Predictors_TL(4,K) &
	         +   TC%ODCAPS_TraceGas(4)%Trace_Coeff(5,K,IN2O)* TraceGas_Predictors_TL(5,K) &
	         +   TC%ODCAPS_TraceGas(4)%Trace_Coeff(6,K,IN2O)* TraceGas_Predictors_TL(6,K) &
	         +   TC%ODCAPS_TraceGas(4)%Trace_Coeff(7,K,IN2O)* TraceGas_Predictors_TL(7,K) )

	   DKN2O_TL = DKN2OX_TL * Predictor%N2O_Multiplier( K )  &
                    + DKN2OX * Predictor_TL%N2O_Multiplier( K )
		     
	   DKN2O = DKN2OX * Predictor%N2O_Multiplier( K ) 
! 	   DKN2O_TL = ZERO
 	ELSE
 	   DKN2O = ZERO
 	   DKN2O_TL = ZERO
 	ENDIF

  !	Limit -DK so it can never totally totally cancel KFIX 
 	DK = DKCO2 + DKSO2 + DKHNO3 + DKN2O		      

 	DK_TL = DKCO2_TL + DKSO2_TL + DKHNO3_TL + DKN2O_TL
			      
 	IF ( -DK >= KFIX ) THEN 			      
 	   DK = -0.999 * KFIX
	   DK_TL = -0.999 * KFIX_TL
 	ENDIF						      
! 	DK_TL = ZERO						      

 	KLAYER = KCON + KFIX + KW( K ) + KOZO + KCO + KMET + DK	      
 	KLAYER_TL = KCON_TL + KFIX_TL + KW_TL( K ) + KOZO_TL + KCO_TL + KMET_TL + DK_TL	      

  !	Adjust the optical depth of the bottom layer	      
 	IF ( K == Predictor%n_Layers) THEN
	  KLAYER = Predictor%BL_Frac_Multi * KLAYER	      
	  KLAYER_TL = Predictor%BL_Frac_Multi * KLAYER_TL	      
        ENDIF
  !	Calc layer-to-space optical depth		      
 	KZ = KZ + KLAYER					      
 	KZ_TL = KZ_TL + KLAYER_TL					      

        IF( .NOT. Cal_Sun) THEN 
            
	   Predictor_TL%Optical_Depth( K ) = KLAYER_TL 
	
	   Predictor_TL%LTS_Optical_Depth( K ) = KZ_TL
        ENDIF
 	
    END DO Layer_Loop
    
  
    IF ( Cal_Sun ) THEN
       
       ! For Subset 1-3
!       IF ( M < 4 ) THEN 
      	  
!	 Predictor%Surf_To_Space_Optical_Depth = KZ * &
!	    Predictor%Secant_Source_Zenith(Predictor%n_Layers) &
!	    * Predictor%Sun_Fudge / Predictor%Secant_Sensor_Zenith( Predictor%n_Layers )
!      Predictor_TL%Surf_To_Space_Optical_Depth = &
!        Predictor_TL%LTS_Optical_Depth( Predictor%n_Layers ) &			          
!	* Predictor%Secant_Source_Zenith(Predictor%n_Layers) &			          
!	* Predictor%Sun_Fudge &
!	/ Predictor%Secant_Sensor_Zenith( Predictor%n_Layers ) &
!        + Predictor%LTS_Optical_Depth( Predictor%n_Layers )&			          
!	* Predictor_TL%Secant_Source_Zenith(Predictor%n_Layers) &			          
!	* Predictor%Sun_Fudge &
!	/ Predictor%Secant_Sensor_Zenith( Predictor%n_Layers ) &  
!	+ Predictor%LTS_Optical_Depth( Predictor%n_Layers )&			          
!	* Predictor%Secant_Source_Zenith(Predictor%n_Layers) &			          
!	* Predictor_TL%Sun_Fudge &
!	/ Predictor%Secant_Sensor_Zenith( Predictor%n_Layers ) &  
!        - Predictor%LTS_Optical_Depth( Predictor%n_Layers ) &			          
!	* Predictor%Secant_Source_Zenith(Predictor%n_Layers) &			          
!	* Predictor%Sun_Fudge &
!        * Predictor_TL%Secant_Sensor_Zenith( Predictor%n_Layers ) &
!	/ (Predictor%Secant_Sensor_Zenith( Predictor%n_Layers ))**TWO 

        ! For Subset 4-7
!       ELSE
        IF ( M >= 4) THEN
	 Predictor_TL%Surf_To_Space_Optical_Depth = KZ_TL * XZ + KZ * XZ_TL
        ENDIF	 
        
    ENDIF	

    NULLIFY(TC)
  END SUBROUTINE Compute_Optical_Depth_Subset_TL 
  

  !#------------------------------------------------------------------------------#
  !#       Subroutine to calculate the layer optical depths due to gaseous	  #
  !#       absorption for a given input atmospheric profile for a single	  #
  !#       channel. Adjoint model     	    			                  #
  !#------------------------------------------------------------------------------#
 
   
  SUBROUTINE Compute_Optical_Depth_Subset_AD(  Sensor_Index,            &  ! Input
                                               Channel_Subset_Index,    & ! Input
                                               Channel_Subset_Position, & ! Input
                                               Predictor,               & ! Input
					       Predictor_AD,            & ! In/Output
					       Do_Sun_Calc)               ! Optional Input 
                                         
    ! ---------
    ! Arguments
    ! ---------
    ! -- Inputs
    INTEGER,  INTENT( IN )     ::  Sensor_Index 
    INTEGER,  INTENT( IN )     ::  Channel_Subset_Index 
    INTEGER,  INTENT( IN )     ::  Channel_Subset_Position 

    ! -- Inputs
    TYPE( Predictor_type ), INTENT( IN ) :: Predictor

    ! -- In/Outputs
    TYPE( Predictor_type ), INTENT( IN OUT ) :: Predictor_AD

    ! -- Optional Inputs
    INTEGER,               OPTIONAL, INTENT( IN )     :: Do_Sun_Calc 

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Optical_Depth_Subset_AD'

    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER ::      I   
    INTEGER ::	    J  
    INTEGER ::	    M  
    INTEGER ::      K 
    INTEGER ::   ICO2   
    INTEGER ::  IHNO3   
    INTEGER ::   IN2O   
    INTEGER ::   ISO2   
      
    REAL( fp ), DIMENSION(Predictor%n_Layers) ::   KCON, KCON_AD       
    REAL( fp ), DIMENSION(Predictor%n_Layers) ::   KFIX, KFIXX, KFIX_AD, KFIXX_AD 
    REAL( fp ), DIMENSION(Predictor%n_Layers) ::   KOZO, KOZO_AD      
    REAL( fp ), DIMENSION(Predictor%n_Layers) ::   KMET, KMET_AD 
    REAL( fp ), DIMENSION(Predictor%n_Layers) ::    KCO, KCO_AD 
    REAL( fp ), DIMENSION(Predictor%n_Layers) ::     DK, DK_AD      
    REAL( fp ), DIMENSION(Predictor%n_Layers) ::  DKCO2, DKCO2_AD, DKCO2X, DKCO2X_AD      
    REAL( fp ), DIMENSION(Predictor%n_Layers) :: DKHNO3, DKHNO3_AD, DKHNO3X, DKHNO3X_AD   
    REAL( fp ), DIMENSION(Predictor%n_Layers) ::  DKN2O, DKN2O_AD, DKN2OX, DKN2OX_AD   
    REAL( fp ), DIMENSION(Predictor%n_Layers) ::  DKSO2, DKSO2_AD, DKSO2X, DKSO2X_AD    
    REAL( fp ), DIMENSION(Predictor%n_Layers) :: KLAYER, KLAYER_AD, KLAYERX, KLAYERX_AD      
    REAL( fp ), DIMENSION(0:Predictor%n_Layers) ::   KZ, KZ_AD      

    LOGICAL ::  LCO2   
    LOGICAL ::  LH2O   
    LOGICAL :: LHNO3   
    LOGICAL ::  LN2O   
    LOGICAL ::  LSO2   
 
  ! for CALOKW
    INTEGER ::  IH2O
    REAL( fp), DIMENSION(Predictor%n_Layers) :: KW, KW_AD

    LOGICAL :: Cal_Sun
    REAL( fp ), DIMENSION(4, MAX_N_SUBSET_TOTAL_PREDICTORS,Predictor%n_Layers ) :: &
                                  Predictor_Subset,  Predictor_Subset_AD
    REAL( fp ), DIMENSION(MAX_N_TRACEGASES_PREDICTORS,Predictor%n_Layers) :: & 
                                  TraceGas_Predictors,  TraceGas_Predictors_AD
    REAL( fp ) ::    XZ, XZ_AD 
    INTEGER :: idx
    TYPE( ODCAPS_TauCoeff_type ), POINTER  ::  TC => NULL()

    TC => ODCAPS_TC(Sensor_Index)
 

    Cal_Sun = .TRUE.                                            

    M = Channel_Subset_Index  
    
    I = Channel_Subset_Position 
    idx = MINLOC(ABS(TC%Sensor_Channel - TC%ODCAPS_Subset(M)%Channel_Index(I) ), DIM =1 )

    J = idx   !TC%Sensor_Channel( TC%ODCAPS_Subset(M)%Channel_Index(I) )

  ! Determine whether or not to do variable CO2 calc     
    ICO2 = TC%Channel_CO2_Perturbation( idx )			         
    IF (ICO2 > 0) THEN			         
       LCO2=.TRUE.				         
    ELSE					         
       LCO2=.FALSE.				         
    ENDIF					         

  ! Determine whether or not to do variable SO2 calc     
    ISO2 = TC%Channel_SO2_Perturbation( idx )			         
    IF (ISO2 > 0) THEN			         
       LSO2=.TRUE.				         
    ELSE					         
       LSO2=.FALSE.				         
    ENDIF					         

  ! Determine whether or not to do variable HNO3 calc    
    IHNO3 = TC%Channel_HNO3_Perturbation( idx )
    IF (IHNO3 > 0) THEN			         
       LHNO3=.TRUE.				         
    ELSE					         
       LHNO3=.FALSE.				         
    ENDIF					         

  ! Determine whether or not to do variable N2O calc     
    IN2O = TC%Channel_N2O_Perturbation( idx )   		         
    IF (IN2O > 0) THEN  			       
       LN2O=.TRUE.				       
    ELSE	        			       
       LN2O=.FALSE.				       
    ENDIF	        			       

  ! Forward Model 
  !  -------------------------
  !  Do OPTRAN water if needed
  !  -------------------------
    IH2O = TC%Channel_H2O_OPTRAN( idx ) 
    IF (IH2O > 0) THEN					     
       LH2O=.FALSE.						     
  
  !    Calc OPTRAN water 					     
       CALL Compute_WOPTRAN_Optics(Sensor_Index, IH2O, Predictor, KW)		     

    ELSE							     
       LH2O=.TRUE.						     
    ENDIF							     

    IF ( PRESENT (Do_Sun_Calc) .AND. &  					     
    	 Predictor%Calc_Sun_Angle_Secant) THEN				    
      Predictor_Subset(1:4, :, :) = Predictor%Predictor_Subset_Sun(1:4, :, :)  
      TraceGas_Predictors = Predictor%TraceGas_Predictors_Sun
      XZ = Predictor%Sun_Fudge 
      Cal_Sun = .TRUE.  							    
    ELSE									     
      Predictor_Subset(1:4, :, :) = Predictor%Predictor_Subset(4:7, :, :)	  
      TraceGas_Predictors = Predictor%TraceGas_Predictors
      XZ = ONE  
      Cal_Sun = .FALSE. 							    
    END IF									     

  ! Initialize the layer-to-space optical depth
    KZ( 0 ) = ZERO
  ! KZFW = ZERO

    !#------------------------------------------------------------------------#  
    !#  		  -- BEGIN LOOP OVER LAYERS --  		      #  
    !#------------------------------------------------------------------------#  

    Layer_Loop: DO K = 1, Predictor%n_Layers				 

 
       Subset_Select: SELECT CASE ( M )
 
	   CASE ( 1 )
	   
       !    --------------------------- 		   
       !    Compute the water continuum 		   
       !    --------------------------- 		   
            KCON(K) = TC%Tuning_Multiple(3,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(1,K,I) * Predictor%Predictor_Subset(M,1,K) &
        	   + TC%ODCAPS_Subset(M)%C(2,K,I) * Predictor%Predictor_Subset(M,2,K) &
        	   + TC%ODCAPS_Subset(M)%C(3,K,I) * Predictor%Predictor_Subset(M,3,K) &
        	   + TC%ODCAPS_Subset(M)%C(4,K,I) * Predictor%Predictor_Subset(M,4,K) &
        	   + TC%ODCAPS_Subset(M)%C(5,K,I) * Predictor%Predictor_Subset(M,5,K) &
        	   + TC%ODCAPS_Subset(M)%C(6,K,I) * Predictor%Predictor_Subset(M,6,K) &
        	   + TC%ODCAPS_Subset(M)%C(7,K,I) * Predictor%Predictor_Subset(M,7,K) )
        		 
            IF ( KCON(K) < ZERO ) THEN		   
               KCON(K) = ZERO				   
            ELSE IF ( KCON(K) > TEN ) THEN 		   
               KCON(K) = TEN				   
            ENDIF					   
  
       !    -----------------------------
       !    Calc the fixed gases abs coef
       !    -----------------------------
              KFIXX(K) = TC%Tuning_Multiple(1,J) &
        	 * ( TC%ODCAPS_Subset(M)%C( 8,K,I) * Predictor%Predictor_Subset(M, 8,K) &
        	   + TC%ODCAPS_Subset(M)%C( 9,K,I) * Predictor%Predictor_Subset(M, 9,K) &
        	   + TC%ODCAPS_Subset(M)%C(10,K,I) * Predictor%Predictor_Subset(M,10,K) &
        	   + TC%ODCAPS_Subset(M)%C(11,K,I) * Predictor%Predictor_Subset(M,11,K) &
        	   + TC%ODCAPS_Subset(M)%C(12,K,I) * Predictor%Predictor_Subset(M,12,K) &
        	   + TC%ODCAPS_Subset(M)%C(13,K,I) * Predictor%Predictor_Subset(M,13,K) &
        	   + TC%ODCAPS_Subset(M)%C(14,K,I) * Predictor%Predictor_Subset(M,14,K) &
        	   + TC%ODCAPS_Subset(M)%C(15,K,I) * Predictor%Predictor_Subset(M,15,K) )
 
            KFIX(K) = KFIXX(K) * Predictor%Fix_Amount_Multiplier( K ) 
  
            IF ( KFIX(K) < ZERO ) THEN	     
               KFIX(K) = ZERO			
            ELSE IF ( KFIX(K) > TEN ) THEN      
               KFIX(K) = TEN			
            ENDIF				

       !    --------------------------
       !    Compute the water abs coef
       !    --------------------------
            IF (LH2O) THEN
       !    Not an OPTRAN water channel
            
              KW( K ) = TC%Tuning_Multiple(2,J) &
        	    * ( TC%ODCAPS_Subset(M)%C(16,K,I) * Predictor%Predictor_Subset(M,16,K) &
        	      + TC%ODCAPS_Subset(M)%C(17,K,I) * Predictor%Predictor_Subset(M,17,K) &
        	      + TC%ODCAPS_Subset(M)%C(18,K,I) * Predictor%Predictor_Subset(M,18,K) &
        	      + TC%ODCAPS_Subset(M)%C(19,K,I) * Predictor%Predictor_Subset(M,19,K) &
        	      + TC%ODCAPS_Subset(M)%C(20,K,I) * Predictor%Predictor_Subset(M,20,K) &
        	      + TC%ODCAPS_Subset(M)%C(21,K,I) * Predictor%Predictor_Subset(M,21,K) &
        	      + TC%ODCAPS_Subset(M)%C(22,K,I) * Predictor%Predictor_Subset(M,22,K) &
        	      + TC%ODCAPS_Subset(M)%C(23,K,I) * Predictor%Predictor_Subset(M,23,K) &
        	      + TC%ODCAPS_Subset(M)%C(24,K,I) * Predictor%Predictor_Subset(M,24,K) &
        	      + TC%ODCAPS_Subset(M)%C(25,K,I) * Predictor%Predictor_Subset(M,25,K) &
        	      + TC%ODCAPS_Subset(M)%C(26,K,I) * Predictor%Predictor_Subset(M,26,K) )
 
              IF ( KW( K ) < ZERO ) KW( K ) = ZERO
            ENDIF

       !    --------------------------
       !    Compute the ozone abs coef
       !    --------------------------
            KOZO(K) = TC%Tuning_Multiple(4,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(27,K,I) * Predictor%Predictor_Subset(M,27,K) &   
        	   + TC%ODCAPS_Subset(M)%C(28,K,I) * Predictor%Predictor_Subset(M,28,K) &   
        	   + TC%ODCAPS_Subset(M)%C(29,K,I) * Predictor%Predictor_Subset(M,29,K) &   
        	   + TC%ODCAPS_Subset(M)%C(30,K,I) * Predictor%Predictor_Subset(M,30,K) &   
        	   + TC%ODCAPS_Subset(M)%C(31,K,I) * Predictor%Predictor_Subset(M,31,K) )  
 
            IF ( KOZO(K) < ZERO ) THEN
               KOZO(K) = ZERO
            ELSE IF ( KOZO(K) > TEN ) THEN
               KOZO(K) = TEN
            ENDIF
      
       !    --------------------------
       !    The other gases abs coef
       !    --------------------------
      	    KCO(K)  = ZERO
	    KMET(K) = ZERO

	   CASE ( 2 )
	   
       !    --------------------------- 		   
       !    Compute the water continuum 		   
       !    --------------------------- 		   
            KCON(K) = TC%Tuning_Multiple(3,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(1,K,I) * Predictor%Predictor_Subset(M,1,K) &
        	   + TC%ODCAPS_Subset(M)%C(2,K,I) * Predictor%Predictor_Subset(M,2,K) &
        	   + TC%ODCAPS_Subset(M)%C(3,K,I) * Predictor%Predictor_Subset(M,3,K) &
        	   + TC%ODCAPS_Subset(M)%C(4,K,I) * Predictor%Predictor_Subset(M,4,K) &
        	   + TC%ODCAPS_Subset(M)%C(5,K,I) * Predictor%Predictor_Subset(M,5,K) &
        	   + TC%ODCAPS_Subset(M)%C(6,K,I) * Predictor%Predictor_Subset(M,6,K) &
        	   + TC%ODCAPS_Subset(M)%C(7,K,I) * Predictor%Predictor_Subset(M,7,K) )
        		 
            IF ( KCON(K) < ZERO ) THEN		   
               KCON(K) = ZERO				   
            ELSE IF ( KCON(K) > TEN ) THEN 		   
               KCON(K) = TEN				   
            ENDIF					   
  
       !    -----------------------------
       !    Calc the fixed gases abs coef
       !    -----------------------------
             KFIXX(K) = TC%Tuning_Multiple(1,J) &
        	 * ( TC%ODCAPS_Subset(M)%C( 8,K,I) * Predictor%Predictor_Subset(M, 8,K) &
        	   + TC%ODCAPS_Subset(M)%C( 9,K,I) * Predictor%Predictor_Subset(M, 9,K) &
        	   + TC%ODCAPS_Subset(M)%C(10,K,I) * Predictor%Predictor_Subset(M,10,K) &
        	   + TC%ODCAPS_Subset(M)%C(11,K,I) * Predictor%Predictor_Subset(M,11,K) &
        	   + TC%ODCAPS_Subset(M)%C(12,K,I) * Predictor%Predictor_Subset(M,12,K) &
        	   + TC%ODCAPS_Subset(M)%C(13,K,I) * Predictor%Predictor_Subset(M,13,K) &
        	   + TC%ODCAPS_Subset(M)%C(14,K,I) * Predictor%Predictor_Subset(M,14,K) &
        	   + TC%ODCAPS_Subset(M)%C(15,K,I) * Predictor%Predictor_Subset(M,15,K) )
 
            KFIX(K) =  KFIXX(K) * Predictor%Fix_Amount_Multiplier( K )
 
            IF ( KFIX(K) < ZERO ) THEN	     
               KFIX(K) = ZERO			
            ELSE IF ( KFIX(K) > TEN ) THEN      
               KFIX(K) = TEN			
            ENDIF				

       !    --------------------------
       !    Compute the ozone abs coef
       !    --------------------------
            KOZO(K) = TC%Tuning_Multiple(4,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(16,K,I) * Predictor%Predictor_Subset(M,16,K) &   
        	   + TC%ODCAPS_Subset(M)%C(17,K,I) * Predictor%Predictor_Subset(M,17,K) &   
        	   + TC%ODCAPS_Subset(M)%C(18,K,I) * Predictor%Predictor_Subset(M,18,K) &   
        	   + TC%ODCAPS_Subset(M)%C(19,K,I) * Predictor%Predictor_Subset(M,19,K) &   
        	   + TC%ODCAPS_Subset(M)%C(20,K,I) * Predictor%Predictor_Subset(M,20,K) &   
        	   + TC%ODCAPS_Subset(M)%C(21,K,I) * Predictor%Predictor_Subset(M,21,K) &   
        	   + TC%ODCAPS_Subset(M)%C(22,K,I) * Predictor%Predictor_Subset(M,22,K) &   
        	   + TC%ODCAPS_Subset(M)%C(23,K,I) * Predictor%Predictor_Subset(M,23,K) &   
        	   + TC%ODCAPS_Subset(M)%C(24,K,I) * Predictor%Predictor_Subset(M,24,K) &   
        	   + TC%ODCAPS_Subset(M)%C(25,K,I) * Predictor%Predictor_Subset(M,25,K) )   
  
            IF ( KOZO(K) < ZERO ) THEN
               KOZO(K) = ZERO
            ELSE IF ( KOZO(K) > TEN ) THEN
               KOZO(K) = TEN
            ENDIF

       !    --------------------------
       !    Compute the water abs coef
       !    --------------------------
            IF (LH2O) THEN
       !    Not an OPTRAN water channel
            
              KW( K ) = TC%Tuning_Multiple(2,J) &
         	      * ( TC%ODCAPS_Subset(M)%C(26,K,I) * Predictor%Predictor_Subset(M,26,K) & 
        	        + TC%ODCAPS_Subset(M)%C(27,K,I) * Predictor%Predictor_Subset(M,27,K) & 
        	        + TC%ODCAPS_Subset(M)%C(28,K,I) * Predictor%Predictor_Subset(M,28,K) & 
        	        + TC%ODCAPS_Subset(M)%C(29,K,I) * Predictor%Predictor_Subset(M,29,K) & 
        	        + TC%ODCAPS_Subset(M)%C(30,K,I) * Predictor%Predictor_Subset(M,30,K) & 
        	        + TC%ODCAPS_Subset(M)%C(31,K,I) * Predictor%Predictor_Subset(M,31,K) &
        	        + TC%ODCAPS_Subset(M)%C(32,K,I) * Predictor%Predictor_Subset(M,32,K) & 
        	        + TC%ODCAPS_Subset(M)%C(33,K,I) * Predictor%Predictor_Subset(M,33,K) & 
        	        + TC%ODCAPS_Subset(M)%C(34,K,I) * Predictor%Predictor_Subset(M,34,K) & 
        	        + TC%ODCAPS_Subset(M)%C(35,K,I) * Predictor%Predictor_Subset(M,35,K) & 
        	        + TC%ODCAPS_Subset(M)%C(36,K,I) * Predictor%Predictor_Subset(M,36,K) )
 
              IF ( KW( K ) < ZERO ) THEN
	         KW( K ) = ZERO
              ELSE IF( KW( K ) > TEN ) THEN
                 KW( K ) = TEN
	      ENDIF
	      	 
            ENDIF

       !    --------------------------
       !    The other gases abs coef
       !    --------------------------
      	    KCO(K)  = ZERO
	    KMET(K) = ZERO

	   CASE ( 3 )
	   
       !    --------------------------- 		   
       !    Compute the water continuum 		   
       !    --------------------------- 		   
            KCON(K) = TC%Tuning_Multiple(3,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(1,K,I) * Predictor%Predictor_Subset(M,1,K) &
        	   + TC%ODCAPS_Subset(M)%C(2,K,I) * Predictor%Predictor_Subset(M,2,K) &
        	   + TC%ODCAPS_Subset(M)%C(3,K,I) * Predictor%Predictor_Subset(M,3,K) &
        	   + TC%ODCAPS_Subset(M)%C(4,K,I) * Predictor%Predictor_Subset(M,4,K) &
        	   + TC%ODCAPS_Subset(M)%C(5,K,I) * Predictor%Predictor_Subset(M,5,K) &
        	   + TC%ODCAPS_Subset(M)%C(6,K,I) * Predictor%Predictor_Subset(M,6,K) &
        	   + TC%ODCAPS_Subset(M)%C(7,K,I) * Predictor%Predictor_Subset(M,7,K) )
        		 
            IF ( KCON(K) < ZERO ) THEN		   
               KCON(K) = ZERO				   
            ELSE IF ( KCON(K) > TEN ) THEN 		   
               KCON(K) = TEN				   
            ENDIF					   
  
       !    -----------------------------
       !    Calc the fixed gases abs coef
       !    -----------------------------
             KFIXX(K)   = TC%Tuning_Multiple(1,J) &
        	 * ( TC%ODCAPS_Subset(M)%C( 8,K,I) * Predictor%Predictor_Subset(M, 8,K) &
        	   + TC%ODCAPS_Subset(M)%C( 9,K,I) * Predictor%Predictor_Subset(M, 9,K) &
        	   + TC%ODCAPS_Subset(M)%C(10,K,I) * Predictor%Predictor_Subset(M,10,K) &
        	   + TC%ODCAPS_Subset(M)%C(11,K,I) * Predictor%Predictor_Subset(M,11,K) &
        	   + TC%ODCAPS_Subset(M)%C(12,K,I) * Predictor%Predictor_Subset(M,12,K) &
        	   + TC%ODCAPS_Subset(M)%C(13,K,I) * Predictor%Predictor_Subset(M,13,K) &
        	   + TC%ODCAPS_Subset(M)%C(14,K,I) * Predictor%Predictor_Subset(M,14,K) &
        	   + TC%ODCAPS_Subset(M)%C(15,K,I) * Predictor%Predictor_Subset(M,15,K) )
 
            KFIX(K) = KFIXX(K) * Predictor%Fix_Amount_Multiplier( K )
 
            IF ( KFIX(K) < ZERO ) THEN	     
               KFIX(K) = ZERO			
            ELSE IF ( KFIX(K) > TEN ) THEN      
               KFIX(K) = TEN			
            ENDIF				

 
       !    ----------------------------
       !    Compute the methane abs coef
       !    ----------------------------
 	    KMET(K) = TC%Tuning_Multiple(6,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(16,K,I) * Predictor%Predictor_Subset(M,16,K) &   
        	   + TC%ODCAPS_Subset(M)%C(17,K,I) * Predictor%Predictor_Subset(M,17,K) &   
        	   + TC%ODCAPS_Subset(M)%C(18,K,I) * Predictor%Predictor_Subset(M,18,K) &   
        	   + TC%ODCAPS_Subset(M)%C(19,K,I) * Predictor%Predictor_Subset(M,19,K) &   
        	   + TC%ODCAPS_Subset(M)%C(20,K,I) * Predictor%Predictor_Subset(M,20,K) &   
        	   + TC%ODCAPS_Subset(M)%C(21,K,I) * Predictor%Predictor_Subset(M,21,K) &   
        	   + TC%ODCAPS_Subset(M)%C(22,K,I) * Predictor%Predictor_Subset(M,22,K) &   
        	   + TC%ODCAPS_Subset(M)%C(23,K,I) * Predictor%Predictor_Subset(M,23,K) &   
        	   + TC%ODCAPS_Subset(M)%C(24,K,I) * Predictor%Predictor_Subset(M,24,K) )   
 
             IF ( KMET(K) < ZERO ) THEN
                KMET(K) = ZERO
             ELSEIF ( KMET(K) > TEN ) THEN
                KMET(K) = TEN
             ENDIF
	     
       !    --------------------------
       !    Compute the water abs coef
       !    --------------------------
            IF (LH2O) THEN
       !    Not an OPTRAN water channel
            
              KW( K ) = TC%Tuning_Multiple(2,J) &
         	      * ( TC%ODCAPS_Subset(M)%C(25,K,I) * Predictor%Predictor_Subset(M,25,K) &
		        + TC%ODCAPS_Subset(M)%C(26,K,I) * Predictor%Predictor_Subset(M,26,K) & 
        	        + TC%ODCAPS_Subset(M)%C(27,K,I) * Predictor%Predictor_Subset(M,27,K) & 
        	        + TC%ODCAPS_Subset(M)%C(28,K,I) * Predictor%Predictor_Subset(M,28,K) & 
        	        + TC%ODCAPS_Subset(M)%C(29,K,I) * Predictor%Predictor_Subset(M,29,K) & 
        	        + TC%ODCAPS_Subset(M)%C(30,K,I) * Predictor%Predictor_Subset(M,30,K) & 
        	        + TC%ODCAPS_Subset(M)%C(31,K,I) * Predictor%Predictor_Subset(M,31,K) &
        	        + TC%ODCAPS_Subset(M)%C(32,K,I) * Predictor%Predictor_Subset(M,32,K) & 
        	        + TC%ODCAPS_Subset(M)%C(33,K,I) * Predictor%Predictor_Subset(M,33,K) & 
        	        + TC%ODCAPS_Subset(M)%C(34,K,I) * Predictor%Predictor_Subset(M,34,K) & 
        	        + TC%ODCAPS_Subset(M)%C(35,K,I) * Predictor%Predictor_Subset(M,35,K) ) 
  
              IF ( KW( K ) < ZERO ) KW( K ) = ZERO
		 
            ENDIF

       !    --------------------------
       !    The other gases abs coef
       !    --------------------------
      	    KCO(K)  = ZERO
	    KOZO(K) = ZERO
	    
	   CASE ( 4 )
	   
       !    --------------------------- 		   
       !    Compute the water continuum 		   
       !    --------------------------- 		   
            KCON(K) = TC%Tuning_Multiple(3,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(1,K,I) * Predictor_Subset(M-3,1,K) &
        	   + TC%ODCAPS_Subset(M)%C(2,K,I) * Predictor_Subset(M-3,2,K) &
        	   + TC%ODCAPS_Subset(M)%C(3,K,I) * Predictor_Subset(M-3,3,K) &
        	   + TC%ODCAPS_Subset(M)%C(4,K,I) * Predictor_Subset(M-3,4,K) &
        	   + TC%ODCAPS_Subset(M)%C(5,K,I) * Predictor_Subset(M-3,5,K) &
        	   + TC%ODCAPS_Subset(M)%C(6,K,I) * Predictor_Subset(M-3,6,K) &
        	   + TC%ODCAPS_Subset(M)%C(7,K,I) * Predictor_Subset(M-3,7,K) )
        		 
            IF ( KCON(K) < ZERO ) THEN		   
               KCON(K) = ZERO				   
            ELSE IF ( KCON(K) > TEN ) THEN 		   
               KCON(K) = TEN				   
            ENDIF					   
  
       !    -----------------------------
       !    Calc the fixed gases abs coef
       !    -----------------------------
            KFIXX(K) = TC%Tuning_Multiple(1,J) &
        	 * ( TC%ODCAPS_Subset(M)%C( 8,K,I) * Predictor_Subset(M-3, 8,K) &
        	   + TC%ODCAPS_Subset(M)%C( 9,K,I) * Predictor_Subset(M-3, 9,K) &
        	   + TC%ODCAPS_Subset(M)%C(10,K,I) * Predictor_Subset(M-3,10,K) &
        	   + TC%ODCAPS_Subset(M)%C(11,K,I) * Predictor_Subset(M-3,11,K) &
        	   + TC%ODCAPS_Subset(M)%C(12,K,I) * Predictor_Subset(M-3,12,K) &
        	   + TC%ODCAPS_Subset(M)%C(13,K,I) * Predictor_Subset(M-3,13,K) &
        	   + TC%ODCAPS_Subset(M)%C(14,K,I) * Predictor_Subset(M-3,14,K) &
        	   + TC%ODCAPS_Subset(M)%C(15,K,I) * Predictor_Subset(M-3,15,K) &
        	   + TC%ODCAPS_Subset(M)%C(16,K,I) * Predictor_Subset(M-3,16,K) &
        	   + TC%ODCAPS_Subset(M)%C(17,K,I) * Predictor_Subset(M-3,17,K) &
        	   + TC%ODCAPS_Subset(M)%C(18,K,I) * Predictor_Subset(M-3,18,K) )
 
            KFIX(K) = KFIXX(K) * Predictor%Fix_Amount_Multiplier( K )

            IF ( KFIX(K) < ZERO ) THEN	     
               KFIX(K) = ZERO			
            ELSE IF ( KFIX(K) > TEN ) THEN      
               KFIX(K) = TEN			
            ENDIF				

 
       !    -----------------------
       !    Compute the CO abs coef
       !    -----------------------
            KCO(K) = TC%Tuning_Multiple(5,J) &
        	* ( TC%ODCAPS_Subset(M)%C(19,K,I) * Predictor_Subset(M-3,19,K) & 
         	  + TC%ODCAPS_Subset(M)%C(20,K,I) * Predictor_Subset(M-3,20,K) & 
        	  + TC%ODCAPS_Subset(M)%C(21,K,I) * Predictor_Subset(M-3,21,K) & 
        	  + TC%ODCAPS_Subset(M)%C(22,K,I) * Predictor_Subset(M-3,22,K) & 
        	  + TC%ODCAPS_Subset(M)%C(23,K,I) * Predictor_Subset(M-3,23,K) & 
        	  + TC%ODCAPS_Subset(M)%C(24,K,I) * Predictor_Subset(M-3,24,K) & 
        	  + TC%ODCAPS_Subset(M)%C(25,K,I) * Predictor_Subset(M-3,25,K) & 
        	  + TC%ODCAPS_Subset(M)%C(26,K,I) * Predictor_Subset(M-3,26,K) & 
        	  + TC%ODCAPS_Subset(M)%C(27,K,I) * Predictor_Subset(M-3,27,K) & 
        	  + TC%ODCAPS_Subset(M)%C(28,K,I) * Predictor_Subset(M-3,28,K) & 
        	  + TC%ODCAPS_Subset(M)%C(29,K,I) * Predictor_Subset(M-3,29,K) ) 
	    
            IF ( KCO(K) < ZERO ) THEN	  
               KCO(K) = ZERO		  
            ELSEIF ( KCO(K) > TEN ) THEN 
               KCO(K) = TEN		  
            ENDIF			  

       !    --------------------------
       !    Compute the ozone abs coef
       !    --------------------------
            KOZO(K) = TC%Tuning_Multiple(4,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(30,K,I) * Predictor_Subset(M-3,30,K) &   
        	   + TC%ODCAPS_Subset(M)%C(31,K,I) * Predictor_Subset(M-3,31,K) &   
        	   + TC%ODCAPS_Subset(M)%C(32,K,I) * Predictor_Subset(M-3,32,K) )   
   
            IF ( KOZO(K) < ZERO ) THEN
               KOZO(K) = ZERO
            ELSE IF ( KOZO(K) > TEN ) THEN
               KOZO(K) = TEN
            ENDIF
 
       !    --------------------------
       !    Compute the water abs coef
       !    --------------------------
            IF (LH2O) THEN
       !    Not an OPTRAN water channel
            
              KW( K ) = TC%Tuning_Multiple(2,J) &
		      * ( TC%ODCAPS_Subset(M)%C(33,K,I) * Predictor_Subset(M-3,33,K) &
		        + TC%ODCAPS_Subset(M)%C(34,K,I) * Predictor_Subset(M-3,34,K) &
 		        + TC%ODCAPS_Subset(M)%C(35,K,I) * Predictor_Subset(M-3,35,K) &
		        + TC%ODCAPS_Subset(M)%C(36,K,I) * Predictor_Subset(M-3,36,K) & 
        	        + TC%ODCAPS_Subset(M)%C(37,K,I) * Predictor_Subset(M-3,37,K) & 
        	        + TC%ODCAPS_Subset(M)%C(38,K,I) * Predictor_Subset(M-3,38,K) & 
        	        + TC%ODCAPS_Subset(M)%C(39,K,I) * Predictor_Subset(M-3,39,K) & 
        	        + TC%ODCAPS_Subset(M)%C(40,K,I) * Predictor_Subset(M-3,40,K) & 
        	        + TC%ODCAPS_Subset(M)%C(41,K,I) * Predictor_Subset(M-3,41,K) &
        	        + TC%ODCAPS_Subset(M)%C(42,K,I) * Predictor_Subset(M-3,42,K) & 
        	        + TC%ODCAPS_Subset(M)%C(43,K,I) * Predictor_Subset(M-3,43,K) & 
        	        + TC%ODCAPS_Subset(M)%C(44,K,I) * Predictor_Subset(M-3,44,K) & 
        	        + TC%ODCAPS_Subset(M)%C(45,K,I) * Predictor_Subset(M-3,45,K) ) 

              IF ( KW( K ) < ZERO ) THEN
	         KW( K ) = ZERO
              ELSE IF( KW( K ) > TEN ) THEN
                 KW( K ) = TEN
 	      ENDIF	 
            ENDIF

       !    --------------------------
       !    The other gases abs coef
       !    --------------------------
 	    KMET(K) = ZERO

	   CASE ( 5 )
	   
       !    --------------------------- 		   
       !    Compute the water continuum 		   
       !    --------------------------- 		   
            KCON(K) = TC%Tuning_Multiple(3,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(1,K,I) * Predictor_Subset(M-3,1,K) &
        	   + TC%ODCAPS_Subset(M)%C(2,K,I) * Predictor_Subset(M-3,2,K) &
        	   + TC%ODCAPS_Subset(M)%C(3,K,I) * Predictor_Subset(M-3,3,K) &
        	   + TC%ODCAPS_Subset(M)%C(4,K,I) * Predictor_Subset(M-3,4,K) &
        	   + TC%ODCAPS_Subset(M)%C(5,K,I) * Predictor_Subset(M-3,5,K) &
        	   + TC%ODCAPS_Subset(M)%C(6,K,I) * Predictor_Subset(M-3,6,K) &
        	   + TC%ODCAPS_Subset(M)%C(7,K,I) * Predictor_Subset(M-3,7,K) )
        		 
            IF ( KCON(K) < ZERO ) THEN		   
               KCON(K) = ZERO				   
            ELSE IF ( KCON(K) > TEN ) THEN 		   
               KCON(K) = TEN				   
            ENDIF					   
  
       !    -----------------------------
       !    Calc the fixed gases abs coef
       !    -----------------------------
            KFIXX(K) = TC%Tuning_Multiple(1,J) &
        	 * ( TC%ODCAPS_Subset(M)%C( 8,K,I) * Predictor_Subset(M-3, 8,K) &
        	   + TC%ODCAPS_Subset(M)%C( 9,K,I) * Predictor_Subset(M-3, 9,K) &
        	   + TC%ODCAPS_Subset(M)%C(10,K,I) * Predictor_Subset(M-3,10,K) &
        	   + TC%ODCAPS_Subset(M)%C(11,K,I) * Predictor_Subset(M-3,11,K) &
        	   + TC%ODCAPS_Subset(M)%C(12,K,I) * Predictor_Subset(M-3,12,K) &
        	   + TC%ODCAPS_Subset(M)%C(13,K,I) * Predictor_Subset(M-3,13,K) &
        	   + TC%ODCAPS_Subset(M)%C(14,K,I) * Predictor_Subset(M-3,14,K) &
        	   + TC%ODCAPS_Subset(M)%C(15,K,I) * Predictor_Subset(M-3,15,K) &
        	   + TC%ODCAPS_Subset(M)%C(16,K,I) * Predictor_Subset(M-3,16,K) &
        	   + TC%ODCAPS_Subset(M)%C(17,K,I) * Predictor_Subset(M-3,17,K) &
        	   + TC%ODCAPS_Subset(M)%C(18,K,I) * Predictor_Subset(M-3,18,K) )
 
            KFIX(K) = KFIXX(K) * Predictor%Fix_Amount_Multiplier( K )

            IF ( KFIX(K) < ZERO ) THEN	     
               KFIX(K) = ZERO			
            ELSE IF ( KFIX(K) > TEN ) THEN      
               KFIX(K) = TEN			
            ENDIF				

       !    --------------------------
       !    Compute the water abs coef
       !    --------------------------
            IF (LH2O) THEN
       !    Not an OPTRAN water channel
            
              KW( K ) = TC%Tuning_Multiple(2,J) &
		      * ( TC%ODCAPS_Subset(M)%C(19,K,I) * Predictor_Subset(M-3,19,K) &
		        + TC%ODCAPS_Subset(M)%C(20,K,I) * Predictor_Subset(M-3,20,K) &
 		        + TC%ODCAPS_Subset(M)%C(21,K,I) * Predictor_Subset(M-3,21,K) )

              IF ( KW( K ) < ZERO ) THEN
	         KW( K ) = ZERO
              ELSE IF( KW( K ) > TEN ) THEN
                 KW( K ) = TEN
              ENDIF
	    ENDIF
 
       !    --------------------------
       !    Compute the ozone abs coef
       !    --------------------------
            KOZO(K) = TC%Tuning_Multiple(4,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(22,K,I) * Predictor_Subset(M-3,22,K) )   
    
            IF ( KOZO(K) < ZERO ) THEN
               KOZO(K) = ZERO
            ELSE IF ( KOZO(K) > TEN ) THEN
               KOZO(K) = TEN
            ENDIF

       !    --------------------------
       !    The other gases abs coef
       !    --------------------------
      	    KCO(K)  = ZERO
	    KMET(K) = ZERO
 
	   CASE ( 6 )
	   
       !    --------------------------- 		   
       !    Compute the water continuum 		   
       !    --------------------------- 		   
            KCON(K) = TC%Tuning_Multiple(3,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(1,K,I) * Predictor_Subset(M-3,1,K) &
        	   + TC%ODCAPS_Subset(M)%C(2,K,I) * Predictor_Subset(M-3,2,K) &
        	   + TC%ODCAPS_Subset(M)%C(3,K,I) * Predictor_Subset(M-3,3,K) &
        	   + TC%ODCAPS_Subset(M)%C(4,K,I) * Predictor_Subset(M-3,4,K) &
        	   + TC%ODCAPS_Subset(M)%C(5,K,I) * Predictor_Subset(M-3,5,K) &
        	   + TC%ODCAPS_Subset(M)%C(6,K,I) * Predictor_Subset(M-3,6,K) &
        	   + TC%ODCAPS_Subset(M)%C(7,K,I) * Predictor_Subset(M-3,7,K) )
        		 
            IF ( KCON(K) < ZERO ) THEN		   
               KCON(K) = ZERO				   
            ELSE IF ( KCON(K) > TEN ) THEN 		   
               KCON(K) = TEN				   
            ENDIF					   
  
       !    -----------------------------
       !    Calc the fixed gases abs coef
       !    -----------------------------
            KFIXX(K)  = TC%Tuning_Multiple(1,J) &
        	 * ( TC%ODCAPS_Subset(M)%C( 8,K,I) * Predictor_Subset(M-3, 8,K) &
        	   + TC%ODCAPS_Subset(M)%C( 9,K,I) * Predictor_Subset(M-3, 9,K) &
        	   + TC%ODCAPS_Subset(M)%C(10,K,I) * Predictor_Subset(M-3,10,K) &
        	   + TC%ODCAPS_Subset(M)%C(11,K,I) * Predictor_Subset(M-3,11,K) &
        	   + TC%ODCAPS_Subset(M)%C(12,K,I) * Predictor_Subset(M-3,12,K) &
        	   + TC%ODCAPS_Subset(M)%C(13,K,I) * Predictor_Subset(M-3,13,K) &
        	   + TC%ODCAPS_Subset(M)%C(14,K,I) * Predictor_Subset(M-3,14,K) &
        	   + TC%ODCAPS_Subset(M)%C(15,K,I) * Predictor_Subset(M-3,15,K) )
 
            KFIX(K) = KFIXX(K) * Predictor%Fix_Amount_Multiplier( K )

            IF ( KFIX(K) < ZERO ) THEN	     
               KFIX(K) = ZERO			
            ELSE IF ( KFIX(K) > TEN ) THEN      
               KFIX(K) = TEN			
            ENDIF				

       !    --------------------------
       !    Compute the water abs coef
       !    --------------------------
            IF (LH2O) THEN
       !    Not an OPTRAN water channel
            
              KW( K ) = TC%Tuning_Multiple(2,J) &
		      * ( TC%ODCAPS_Subset(M)%C(16,K,I) * Predictor_Subset(M-3,16,K) &
        	        + TC%ODCAPS_Subset(M)%C(17,K,I) * Predictor_Subset(M-3,17,K) &
        	        + TC%ODCAPS_Subset(M)%C(18,K,I) * Predictor_Subset(M-3,18,K) &
                        + TC%ODCAPS_Subset(M)%C(19,K,I) * Predictor_Subset(M-3,19,K) &
		        + TC%ODCAPS_Subset(M)%C(20,K,I) * Predictor_Subset(M-3,20,K) &
 		        + TC%ODCAPS_Subset(M)%C(21,K,I) * Predictor_Subset(M-3,21,K) &
 		        + TC%ODCAPS_Subset(M)%C(22,K,I) * Predictor_Subset(M-3,22,K) )

              IF ( KW( K ) < ZERO ) THEN
	         KW( K ) = ZERO
              ELSE IF( KW( K ) > TEN ) THEN
                 KW( K ) = TEN
              ENDIF
	    ENDIF
 
       !    --------------------------
       !    Compute the ozone abs coef
       !    --------------------------
            KOZO(K) = TC%Tuning_Multiple(4,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(23,K,I) * Predictor_Subset(M-3,23,K) )   
    
            IF ( KOZO(K) < ZERO ) THEN
               KOZO(K) = ZERO
            ELSE IF ( KOZO(K) > TEN ) THEN
               KOZO(K) = TEN
            ENDIF

       !    --------------------------
       !    The other gases abs coef
       !    --------------------------
      	    KCO(K)  = ZERO
	    KMET(K) = ZERO
 
	   CASE ( 7 )
	   
       !    --------------------------- 		   
       !    Compute the water continuum 		   
       !    --------------------------- 		   
            KCON(K) = TC%Tuning_Multiple(3,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(1,K,I) * Predictor_Subset(M-3,1,K) &
        	   + TC%ODCAPS_Subset(M)%C(2,K,I) * Predictor_Subset(M-3,2,K) &
        	   + TC%ODCAPS_Subset(M)%C(3,K,I) * Predictor_Subset(M-3,3,K) &
        	   + TC%ODCAPS_Subset(M)%C(4,K,I) * Predictor_Subset(M-3,4,K) &
        	   + TC%ODCAPS_Subset(M)%C(5,K,I) * Predictor_Subset(M-3,5,K) &
        	   + TC%ODCAPS_Subset(M)%C(6,K,I) * Predictor_Subset(M-3,6,K) &
        	   + TC%ODCAPS_Subset(M)%C(7,K,I) * Predictor_Subset(M-3,7,K) )
        		 
            IF ( KCON(K) < ZERO ) THEN		   
               KCON(K) = ZERO				   
            ELSE IF ( KCON(K) > TEN ) THEN 		   
               KCON(K) = TEN				   
            ENDIF					   
  
       !    -----------------------------
       !    Calc the fixed gases abs coef
       !    -----------------------------
            KFIXX(K)  = TC%Tuning_Multiple(1,J) &
        	 * ( TC%ODCAPS_Subset(M)%C( 8,K,I) * Predictor_Subset(M-3, 8,K) &
        	   + TC%ODCAPS_Subset(M)%C( 9,K,I) * Predictor_Subset(M-3, 9,K) &
        	   + TC%ODCAPS_Subset(M)%C(10,K,I) * Predictor_Subset(M-3,10,K) &
        	   + TC%ODCAPS_Subset(M)%C(11,K,I) * Predictor_Subset(M-3,11,K) &
        	   + TC%ODCAPS_Subset(M)%C(12,K,I) * Predictor_Subset(M-3,12,K) &
        	   + TC%ODCAPS_Subset(M)%C(13,K,I) * Predictor_Subset(M-3,13,K) &
        	   + TC%ODCAPS_Subset(M)%C(14,K,I) * Predictor_Subset(M-3,14,K) &
        	   + TC%ODCAPS_Subset(M)%C(15,K,I) * Predictor_Subset(M-3,15,K) )
 
            KFIX(K) = KFIXX(K) * Predictor%Fix_Amount_Multiplier( K )

            IF ( KFIX(K) < ZERO ) THEN	     
               KFIX(K) = ZERO			
            ELSE IF ( KFIX(K) > TEN ) THEN      
               KFIX(K) = TEN			
            ENDIF				

       !    --------------------------
       !    Compute the water abs coef
       !    --------------------------
            IF (LH2O) THEN
       !    Not an OPTRAN water channel
            
              KW( K ) = TC%Tuning_Multiple(2,J) &
		      * ( TC%ODCAPS_Subset(M)%C(16,K,I) * Predictor_Subset(M-3,16,K) &
        	        + TC%ODCAPS_Subset(M)%C(17,K,I) * Predictor_Subset(M-3,17,K) &
        	        + TC%ODCAPS_Subset(M)%C(18,K,I) * Predictor_Subset(M-3,18,K) &
                        + TC%ODCAPS_Subset(M)%C(19,K,I) * Predictor_Subset(M-3,19,K) &
		        + TC%ODCAPS_Subset(M)%C(20,K,I) * Predictor_Subset(M-3,20,K) &
 		        + TC%ODCAPS_Subset(M)%C(21,K,I) * Predictor_Subset(M-3,21,K) &
 		        + TC%ODCAPS_Subset(M)%C(22,K,I) * Predictor_Subset(M-3,22,K) &
        	        + TC%ODCAPS_Subset(M)%C(23,K,I) * Predictor_Subset(M-3,23,K) &
                        + TC%ODCAPS_Subset(M)%C(24,K,I) * Predictor_Subset(M-3,24,K) &
		        + TC%ODCAPS_Subset(M)%C(25,K,I) * Predictor_Subset(M-3,25,K) &
 		        + TC%ODCAPS_Subset(M)%C(26,K,I) * Predictor_Subset(M-3,26,K) &
 		        + TC%ODCAPS_Subset(M)%C(27,K,I) * Predictor_Subset(M-3,27,K) &
 		        + TC%ODCAPS_Subset(M)%C(28,K,I) * Predictor_Subset(M-3,28,K) )

              IF ( KW( K ) < ZERO ) THEN
	         KW( K ) = ZERO
              ELSE IF( KW( K ) > TEN ) THEN
                 KW( K ) = TEN
              ENDIF
	    ENDIF
 
       !    --------------------------
       !    Compute the ozone abs coef
       !    --------------------------
            KOZO(K) = TC%Tuning_Multiple(4,J) &
        	 * ( TC%ODCAPS_Subset(M)%C(29,K,I) * Predictor_Subset(M-3,29,K) )   
    
            IF ( KOZO(K) < ZERO ) THEN
               KOZO(K) = ZERO
            ELSE IF ( KOZO(K) > TEN ) THEN
               KOZO(K) = TEN
            ENDIF

       !    --------------------------
       !    The other gases abs coef
       !    --------------------------
      	    KCO(K)  = ZERO
	    KMET(K) = ZERO

       END SELECT Subset_Select


  !    ----------------------------
  !    Calc change in total optical
  !    depth due to variable CO2
  !    ----------------------------
       IF ( LCO2 .AND. Predictor%CO2_Multiplier( K ) /= ZERO ) THEN
 	  DKCO2X(K) = ( TC%ODCAPS_TraceGas(1)%Trace_Coeff(1,K,ICO2)* TraceGas_Predictors(1,K) & 
	        +   TC%ODCAPS_TraceGas(1)%Trace_Coeff(2,K,ICO2)* TraceGas_Predictors(2,K) &
	        +   TC%ODCAPS_TraceGas(1)%Trace_Coeff(3,K,ICO2)* TraceGas_Predictors(3,K) &
	        +   TC%ODCAPS_TraceGas(1)%Trace_Coeff(4,K,ICO2)* TraceGas_Predictors(4,K) )
 
  	  DKCO2(K) = DKCO2X(K) * Predictor%CO2_Multiplier( K ) 
       ELSE
 	  DKCO2(K) = ZERO
       ENDIF

  !    ----------------------------
  !    Calc change in total optical
  !    depth due to variable SO2
  !    ----------------------------
       IF ( LSO2 .AND. Predictor%SO2_Multiplier( K ) /= ZERO ) THEN
          DKSO2X(K) = ( TC%ODCAPS_TraceGas(2)%Trace_Coeff(1,K,ISO2)* TraceGas_Predictors(1,K) & 
	        +   TC%ODCAPS_TraceGas(2)%Trace_Coeff(2,K,ISO2)* TraceGas_Predictors(2,K) &
	        +   TC%ODCAPS_TraceGas(2)%Trace_Coeff(3,K,ISO2)* TraceGas_Predictors(3,K) &
	        +   TC%ODCAPS_TraceGas(2)%Trace_Coeff(4,K,ISO2)* TraceGas_Predictors(4,K) )
 
          DKSO2(K) = DKSO2X(K) * Predictor%SO2_Multiplier( K ) 
       ELSE
          DKSO2(K) = ZERO
       ENDIF
  
  !    ----------------------------
  !    Calc change in total optical
  !    depth due to variable HNO3
  !    ----------------------------
       IF ( LHNO3 .AND. Predictor%HNO3_Multiplier( K ) /= ZERO ) THEN
 	  DKHNO3X(K) = ( TC%ODCAPS_TraceGas(3)%Trace_Coeff(1,K,IHNO3)* TraceGas_Predictors(1,K) & 
	         +   TC%ODCAPS_TraceGas(3)%Trace_Coeff(2,K,IHNO3)* TraceGas_Predictors(2,K) &
	         +   TC%ODCAPS_TraceGas(3)%Trace_Coeff(3,K,IHNO3)* TraceGas_Predictors(3,K) &
	         +   TC%ODCAPS_TraceGas(3)%Trace_Coeff(4,K,IHNO3)* TraceGas_Predictors(4,K) )
 
 	  DKHNO3(K) = DKHNO3X(K) * Predictor%HNO3_Multiplier( K )
       ELSE
 	  DKHNO3(K) = ZERO
       ENDIF
 
  !	----------------------------
  !	Calc change in total optical
  !	depth due to variable N2O
  !	----------------------------
 	IF ( LN2O .AND. Predictor%N2O_Multiplier( K ) /= ZERO ) THEN
 	   DKN2OX(K) = ( TC%ODCAPS_TraceGas(4)%Trace_Coeff(1,K,IN2O)* TraceGas_Predictors(1,K) & 
	         +   TC%ODCAPS_TraceGas(4)%Trace_Coeff(2,K,IN2O)* TraceGas_Predictors(2,K) &
	         +   TC%ODCAPS_TraceGas(4)%Trace_Coeff(3,K,IN2O)* TraceGas_Predictors(3,K) &
	         +   TC%ODCAPS_TraceGas(4)%Trace_Coeff(4,K,IN2O)* TraceGas_Predictors(4,K) &
	         +   TC%ODCAPS_TraceGas(4)%Trace_Coeff(5,K,IN2O)* TraceGas_Predictors(5,K) &
	         +   TC%ODCAPS_TraceGas(4)%Trace_Coeff(6,K,IN2O)* TraceGas_Predictors(6,K) &
	         +   TC%ODCAPS_TraceGas(4)%Trace_Coeff(7,K,IN2O)* TraceGas_Predictors(7,K) )
  	   
	   DKN2O(K) = DKN2OX(K) * Predictor%N2O_Multiplier( K ) 
 	ELSE
 	   DKN2O(K) = ZERO
 	ENDIF

  !	Limit -DK so it can never totally totally cancel KFIX 
 	DK(K) = DKCO2(K) + DKSO2(K) + DKHNO3(K) + DKN2O(K)		      
 	IF ( -DK(K) >= KFIX(K) ) THEN 			      
 	   DK(K) = -0.999 * KFIX(K)				      
 	ENDIF						      
 							      
 	KLAYER(K) = KCON(K) + KFIX(K) + KW( K ) + KOZO(K) + KCO(K) + KMET(K) + DK(K)	
	      

  !	Adjust the optical depth of the bottom layer	      
 	IF ( K == Predictor%n_Layers) THEN 
	  KLAYERX(K) = Predictor%BL_Frac_Multi * KLAYER(K)	      
        ELSE
	  KLAYERX(K) = KLAYER(K)
	ENDIF  

  !	Calc layer-to-space optical depth		      
 	KZ( K ) = KZ( K -1 ) + KLAYERX(K)					      

  !      IF( .NOT. Cal_Sun) THEN 
  !        Predictor%Optical_Depth( K ) = KLAYER(K) 
	
  !	   Predictor%LTS_Optical_Depth( K ) = KZ(K)
  !      ENDIF
 	
    END DO Layer_Loop
    
  
!    IF ( Cal_Sun ) THEN
       
       ! For Subset 1-3
!       IF ( M < 4 ) THEN 
      	  
!	 Predictor%Surf_To_Space_Optical_Depth = KZ * &
!	    Predictor%Secant_Source_Zenith(Predictor%n_Layers) &
!	    * Predictor%Sun_Fudge / Predictor%Secant_Sensor_Zenith( Predictor%n_Layers )

       ! For Subset 4-7
!       ELSE
         
!	 Predictor%Surf_To_Space_Optical_Depth = KZ(K) * XZ
!       ENDIF	 
        
!    ENDIF	

    KZ_AD( : )     = ZERO
    KLAYER_AD( : ) = ZERO
    KLAYERX_AD( : )= ZERO
    KCON_AD( : )   = ZERO
    KFIX_AD( : )   = ZERO
    KW_AD( : )     = ZERO
    KOZO_AD( : )   = ZERO
    KCO_AD( : )    = ZERO
    KMET_AD( : )   = ZERO
    DK_AD( : )     = ZERO      
    TraceGas_Predictors_AD(:,:)  = ZERO
    Predictor_Subset_AD(:, :, :) = ZERO
        
    ! Adjoint Model
    IF( Cal_Sun ) THEN
      
      IF ( M >= 4) THEN
    
       KZ_AD(Predictor%n_Layers) = KZ_AD(Predictor%n_Layers) + &
            Predictor_AD%Surf_To_Space_Optical_Depth * XZ
    
       XZ_AD = KZ(Predictor%n_Layers) * Predictor_AD%Surf_To_Space_Optical_Depth
      ENDIF
    
    ENDIF

    Adjoint_Layer_Loop: DO K = Predictor%n_Layers, 1,  -1				 

        IF( .NOT. Cal_Sun) THEN 
           KZ_AD( K ) = KZ_AD( K ) + Predictor_AD%LTS_Optical_Depth( K )
           
	   KLAYERX_AD(K) =  KLAYERX_AD(K) + Predictor_AD%Optical_Depth( K )
	   
        ENDIF

        KLAYERX_AD(K) = KLAYERX_AD(K) + KZ_AD( K )
	KZ_AD( K-1 ) = KZ_AD( K-1 ) + KZ_AD( K )
	KZ_AD( K ) = ZERO
	
 	IF ( K == Predictor%n_Layers) THEN
	  KLAYER_AD(K) = Predictor%BL_Frac_Multi * KLAYERX_AD(K) + KLAYER_AD(K)
	ELSE
	  KLAYER_AD(K) = KLAYER_AD(K) + KLAYERX_AD(K)
	ENDIF
	
	KLAYERX_AD(K) = ZERO
	
	KCON_AD(K) = KLAYER_AD(K)
	KFIX_AD(K) = KLAYER_AD(K)
	KW_AD( K ) = KLAYER_AD(K)
	KOZO_AD(K) = KLAYER_AD(K)
	KCO_AD(K)  = KLAYER_AD(K)
	KMET_AD(K) = KLAYER_AD(K)
	DK_AD(K)   = KLAYER_AD(K)
!	KFIX_AD(K) = ZERO
!	KW_AD( K ) = ZERO
!	KOZO_AD(K) = ZERO
!	KCO_AD(K)  = ZERO
!	KMET_AD(K) = ZERO
!	DK_AD(K)   = ZERO

        KLAYER_AD(K) = ZERO
	
 	IF ( -DK(K) >= KFIX(K) ) THEN 			      
 	   KFIX_AD(K) = -0.999 * DK_AD(K) + KFIX_AD(K)
	ENDIF						      
	
	DKCO2_AD(K)  = DK_AD(K)
	DKSO2_AD(K)  = DK_AD(K)
	DKHNO3_AD(K) = DK_AD(K)
	DKN2O_AD(K)  = DK_AD(K)

	DK_AD(K) = ZERO
	
!	DKCO2_AD(K)  = ZERO 
!	DKSO2_AD(K)  = ZERO 
!	DKHNO3_AD(K) = ZERO 
!	DKN2O_AD(K)  = ZERO 
	
  !	----------------------------
  !	Calc change in total optical
  !	depth due to variable N2O
  !	----------------------------
 	IF ( LN2O .AND. Predictor%N2O_Multiplier( K ) /= ZERO ) THEN
	
	  DKN2OX_AD(K) = DKN2O_AD(K) * Predictor%N2O_Multiplier( K )
	  Predictor_AD%N2O_Multiplier( K ) = Predictor_AD%N2O_Multiplier( K ) + &
	                                         DKN2OX(K) * DKN2O_AD(K)
          						 
	  TraceGas_Predictors_AD(1,K) = TraceGas_Predictors_AD(1,K) + &
	                                TC%ODCAPS_TraceGas(4)%Trace_Coeff(1,K,IN2O) * DKN2OX_AD(K)
	  TraceGas_Predictors_AD(2,K) = TraceGas_Predictors_AD(2,K) + &
	                                TC%ODCAPS_TraceGas(4)%Trace_Coeff(2,K,IN2O) * DKN2OX_AD(K)
	  TraceGas_Predictors_AD(3,K) = TraceGas_Predictors_AD(3,K) + &
	                                TC%ODCAPS_TraceGas(4)%Trace_Coeff(3,K,IN2O) * DKN2OX_AD(K)
	  TraceGas_Predictors_AD(4,K) = TraceGas_Predictors_AD(4,K) + &
	                                TC%ODCAPS_TraceGas(4)%Trace_Coeff(4,K,IN2O) * DKN2OX_AD(K)
	  TraceGas_Predictors_AD(5,K) = TraceGas_Predictors_AD(5,K) + &
	                                TC%ODCAPS_TraceGas(4)%Trace_Coeff(5,K,IN2O) * DKN2OX_AD(K)
	  TraceGas_Predictors_AD(6,K) = TraceGas_Predictors_AD(6,K) + &
	                                TC%ODCAPS_TraceGas(4)%Trace_Coeff(6,K,IN2O) * DKN2OX_AD(K)
	  TraceGas_Predictors_AD(7,K) = TraceGas_Predictors_AD(7,K) + &
	                                TC%ODCAPS_TraceGas(4)%Trace_Coeff(7,K,IN2O) * DKN2OX_AD(K)
	  DKN2OX_AD(K) = ZERO 
	  DKN2O_AD(K) = ZERO
        ENDIF
  !    ----------------------------
  !    Calc change in total optical
  !    depth due to variable HNO3
  !    ----------------------------
       IF ( LHNO3 .AND. Predictor%HNO3_Multiplier( K ) /= ZERO ) THEN
 
	  DKHNO3X_AD(K) = DKHNO3_AD(K) * Predictor%HNO3_Multiplier( K )
	  Predictor_AD%HNO3_Multiplier( K ) = Predictor_AD%HNO3_Multiplier( K ) + &
	                                         DKHNO3X(K) * DKHNO3_AD(K)
 
	  TraceGas_Predictors_AD(1,K) = TraceGas_Predictors_AD(1,K) + &
	                                TC%ODCAPS_TraceGas(3)%Trace_Coeff(1,K,IHNO3) * DKHNO3X_AD(K)
	  TraceGas_Predictors_AD(2,K) = TraceGas_Predictors_AD(2,K) + &
	                                TC%ODCAPS_TraceGas(3)%Trace_Coeff(2,K,IHNO3) * DKHNO3X_AD(K)
	  TraceGas_Predictors_AD(3,K) = TraceGas_Predictors_AD(3,K) + &
	                                TC%ODCAPS_TraceGas(3)%Trace_Coeff(3,K,IHNO3) * DKHNO3X_AD(K)
	  TraceGas_Predictors_AD(4,K) = TraceGas_Predictors_AD(4,K) + &
	                                TC%ODCAPS_TraceGas(3)%Trace_Coeff(4,K,IHNO3) * DKHNO3X_AD(K)
          DKHNO3X_AD(K) = ZERO 
	  DKHNO3_AD(K) = ZERO
       ENDIF	

  !    ----------------------------
  !    Calc change in total optical
  !    depth due to variable SO2
  !    ----------------------------
       IF ( LSO2 .AND. Predictor%SO2_Multiplier( K ) /= ZERO ) THEN

	  DKSO2X_AD(K) = DKSO2_AD(K) * Predictor%SO2_Multiplier( K )
	  Predictor_AD%SO2_Multiplier( K ) = Predictor_AD%SO2_Multiplier( K ) + &
	                                         DKSO2X(K) * DKSO2_AD(K)
 
	  TraceGas_Predictors_AD(1,K) = TraceGas_Predictors_AD(1,K) + &
	                                TC%ODCAPS_TraceGas(2)%Trace_Coeff(1,K,ISO2) * DKSO2X_AD(K)
	  TraceGas_Predictors_AD(2,K) = TraceGas_Predictors_AD(2,K) + &
	                                TC%ODCAPS_TraceGas(2)%Trace_Coeff(2,K,ISO2) * DKSO2X_AD(K)
	  TraceGas_Predictors_AD(3,K) = TraceGas_Predictors_AD(3,K) + &
	                                TC%ODCAPS_TraceGas(2)%Trace_Coeff(3,K,ISO2) * DKSO2X_AD(K)
	  TraceGas_Predictors_AD(4,K) = TraceGas_Predictors_AD(4,K) + &
	                                TC%ODCAPS_TraceGas(2)%Trace_Coeff(4,K,ISO2) * DKSO2X_AD(K)
          DKSO2X_AD(K) = ZERO 
	  DKSO2_AD(K) = ZERO
       ENDIF

  !    ----------------------------
  !    Calc change in total optical
  !    depth due to variable CO2
  !    ----------------------------
       IF ( LCO2 .AND. Predictor%CO2_Multiplier( K ) /= ZERO ) THEN
	  DKCO2X_AD(K) = DKCO2_AD(K) * Predictor%CO2_Multiplier( K )
	  Predictor_AD%CO2_Multiplier( K ) = Predictor_AD%CO2_Multiplier( K ) + &
	                                         DKCO2X(K) * DKCO2_AD(K)
 
	  TraceGas_Predictors_AD(1,K) = TraceGas_Predictors_AD(1,K) + &
	                                TC%ODCAPS_TraceGas(1)%Trace_Coeff(1,K,ICO2) * DKCO2X_AD(K)
	  TraceGas_Predictors_AD(2,K) = TraceGas_Predictors_AD(2,K) + &
	                                TC%ODCAPS_TraceGas(1)%Trace_Coeff(2,K,ICO2) * DKCO2X_AD(K)
	  TraceGas_Predictors_AD(3,K) = TraceGas_Predictors_AD(3,K) + &
	                                TC%ODCAPS_TraceGas(1)%Trace_Coeff(3,K,ICO2) * DKCO2X_AD(K)
	  TraceGas_Predictors_AD(4,K) = TraceGas_Predictors_AD(4,K) + &
	                                TC%ODCAPS_TraceGas(1)%Trace_Coeff(4,K,ICO2) * DKCO2X_AD(K)
          DKCO2X_AD(K) = ZERO 
	  DKCO2_AD(K) = ZERO
       ENDIF

       AD_Subset_Select: SELECT CASE ( M )
 
	   CASE ( 1 )
      	    KCO_AD(K)  = ZERO
	    KMET_AD(K) = ZERO

       !    --------------------------
       !    Compute the ozone abs coef
       !    --------------------------
            IF ( KOZO(K) < ZERO ) THEN
               KOZO_AD(K) = ZERO
            ELSE IF ( KOZO(K) > TEN ) THEN
               KOZO_AD(K) = ZERO
            ENDIF

            Predictor_AD%Predictor_Subset(M,27,K) = Predictor_AD%Predictor_Subset(M,27,K) + &
	                   TC%Tuning_Multiple(4,J) * TC%ODCAPS_Subset(M)%C(27,K,I) * KOZO_AD(K)
            Predictor_AD%Predictor_Subset(M,28,K) = Predictor_AD%Predictor_Subset(M,28,K) + &
	                   TC%Tuning_Multiple(4,J) * TC%ODCAPS_Subset(M)%C(28,K,I) * KOZO_AD(K)
            Predictor_AD%Predictor_Subset(M,29,K) = Predictor_AD%Predictor_Subset(M,29,K) + &
	                   TC%Tuning_Multiple(4,J) * TC%ODCAPS_Subset(M)%C(29,K,I) * KOZO_AD(K)
            Predictor_AD%Predictor_Subset(M,30,K) = Predictor_AD%Predictor_Subset(M,30,K) + &
	                   TC%Tuning_Multiple(4,J) * TC%ODCAPS_Subset(M)%C(30,K,I) * KOZO_AD(K)
            Predictor_AD%Predictor_Subset(M,31,K) = Predictor_AD%Predictor_Subset(M,31,K) + &
	                   TC%Tuning_Multiple(4,J) * TC%ODCAPS_Subset(M)%C(31,K,I) * KOZO_AD(K)
	    KOZO_AD(K) = ZERO
       !    --------------------------
       !    Compute the water abs coef
       !    --------------------------
            IF (LH2O) THEN
       !    Not an OPTRAN water channel

              IF ( KW( K ) < ZERO ) THEN
 	        KW_AD( K ) = ZERO
	      ENDIF	
              
	      Predictor_AD%Predictor_Subset(M,16,K) = Predictor_AD%Predictor_Subset(M,16,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(16,K,I) * KW_AD( K )      
	      Predictor_AD%Predictor_Subset(M,17,K) = Predictor_AD%Predictor_Subset(M,17,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(17,K,I) * KW_AD( K )      
	      Predictor_AD%Predictor_Subset(M,18,K) = Predictor_AD%Predictor_Subset(M,18,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(18,K,I) * KW_AD( K )      
	      Predictor_AD%Predictor_Subset(M,19,K) = Predictor_AD%Predictor_Subset(M,19,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(19,K,I) * KW_AD( K )      
	      Predictor_AD%Predictor_Subset(M,20,K) = Predictor_AD%Predictor_Subset(M,20,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(20,K,I) * KW_AD( K )      
	      Predictor_AD%Predictor_Subset(M,21,K) = Predictor_AD%Predictor_Subset(M,21,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(21,K,I) * KW_AD( K )      
	      Predictor_AD%Predictor_Subset(M,22,K) = Predictor_AD%Predictor_Subset(M,22,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(22,K,I) * KW_AD( K )      
	      Predictor_AD%Predictor_Subset(M,23,K) = Predictor_AD%Predictor_Subset(M,23,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(23,K,I) * KW_AD( K )      
	      Predictor_AD%Predictor_Subset(M,24,K) = Predictor_AD%Predictor_Subset(M,24,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(24,K,I) * KW_AD( K )      
	      Predictor_AD%Predictor_Subset(M,25,K) = Predictor_AD%Predictor_Subset(M,25,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(25,K,I) * KW_AD( K )      
	      Predictor_AD%Predictor_Subset(M,26,K) = Predictor_AD%Predictor_Subset(M,26,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(26,K,I) * KW_AD( K )      
       
              KW_AD( K ) = ZERO
            ENDIF
	    
       !    -----------------------------
       !    Calc the fixed gases abs coef
       !    -----------------------------
            IF ( KFIX(K) < ZERO ) THEN	     
 	       KFIX_AD(K) = ZERO			
            ELSE IF ( KFIX(K) > TEN ) THEN      
 	       KFIX_AD(K) = ZERO			
            ENDIF				
            
!	    KFIX_AD(K) = ZERO

	    KFIXX_AD(K) = KFIX_AD(K) * Predictor%Fix_Amount_Multiplier( K )
	    Predictor_AD%Fix_Amount_Multiplier( K ) = Predictor_AD%Fix_Amount_Multiplier( K ) + &
	                                                  KFIXX(K) * KFIX_AD(K)
							  
            Predictor_AD%Predictor_Subset(M, 8,K) = Predictor_AD%Predictor_Subset(M, 8,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C( 8,K,I) * KFIXX_AD(K)
            Predictor_AD%Predictor_Subset(M, 9,K) = Predictor_AD%Predictor_Subset(M, 9,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C( 9,K,I) * KFIXX_AD(K)
            Predictor_AD%Predictor_Subset(M,10,K) = Predictor_AD%Predictor_Subset(M,10,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(10,K,I) * KFIXX_AD(K)
            Predictor_AD%Predictor_Subset(M,11,K) = Predictor_AD%Predictor_Subset(M,11,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(11,K,I) * KFIXX_AD(K)
            Predictor_AD%Predictor_Subset(M,12,K) = Predictor_AD%Predictor_Subset(M,12,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(12,K,I) * KFIXX_AD(K)
            Predictor_AD%Predictor_Subset(M,13,K) = Predictor_AD%Predictor_Subset(M,13,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(13,K,I) * KFIXX_AD(K)
            Predictor_AD%Predictor_Subset(M,14,K) = Predictor_AD%Predictor_Subset(M,14,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(14,K,I) * KFIXX_AD(K)
            Predictor_AD%Predictor_Subset(M,15,K) = Predictor_AD%Predictor_Subset(M,15,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(15,K,I) * KFIXX_AD(K)

            KFIXX_AD(K) = ZERO
	    KFIX_AD(K) = ZERO
       !    --------------------------- 		   
       !    Compute the water continuum 		   
       !    --------------------------- 		   
            IF ( KCON(k) < ZERO ) THEN		   
               KCON_AD(K) = ZERO				   
            ELSE IF ( KCON(K) > TEN ) THEN 		   
                KCON_AD(K) = ZERO				   
            ENDIF					   

!            KCON_AD(K) = ZERO
	    
            Predictor_AD%Predictor_Subset(M,1,K) = Predictor_AD%Predictor_Subset(M,1,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(1,K,I) * KCON_AD(K) 
            Predictor_AD%Predictor_Subset(M,2,K) = Predictor_AD%Predictor_Subset(M,2,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(2,K,I) * KCON_AD(K)
            Predictor_AD%Predictor_Subset(M,3,K) = Predictor_AD%Predictor_Subset(M,3,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(3,K,I) * KCON_AD(K)
            Predictor_AD%Predictor_Subset(M,4,K) = Predictor_AD%Predictor_Subset(M,4,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(4,K,I) * KCON_AD(K)
            Predictor_AD%Predictor_Subset(M,5,K) = Predictor_AD%Predictor_Subset(M,5,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(5,K,I) * KCON_AD(K)
            Predictor_AD%Predictor_Subset(M,6,K) = Predictor_AD%Predictor_Subset(M,6,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(6,K,I) * KCON_AD(K)
            Predictor_AD%Predictor_Subset(M,7,K) = Predictor_AD%Predictor_Subset(M,7,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(7,K,I) * KCON_AD(K)

            KCON_AD(K) = ZERO
	    
	   CASE ( 2 )
       !    --------------------------
       !    The other gases abs coef
       !    --------------------------
       	    KCO_AD(K)  = ZERO
	    KMET_AD(K) = ZERO


       !    --------------------------
       !    Compute the water abs coef
       !    --------------------------
            IF (LH2O) THEN
       !    Not an OPTRAN water channel
              IF ( KW( K ) < ZERO ) THEN
 	         KW_AD( K ) = ZERO
              ELSE IF( KW( K ) > TEN ) THEN
                 KW_AD( K ) = ZERO
	      ENDIF
	      
	      Predictor_AD%Predictor_Subset(M,26,K) = Predictor_AD%Predictor_Subset(M,26,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(26,K,I) * KW_AD( K )      
	      Predictor_AD%Predictor_Subset(M,27,K) = Predictor_AD%Predictor_Subset(M,27,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(27,K,I) * KW_AD( K )      
	      Predictor_AD%Predictor_Subset(M,28,K) = Predictor_AD%Predictor_Subset(M,28,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(28,K,I) * KW_AD( K )      
	      Predictor_AD%Predictor_Subset(M,29,K) = Predictor_AD%Predictor_Subset(M,29,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(29,K,I) * KW_AD( K )      
	      Predictor_AD%Predictor_Subset(M,30,K) = Predictor_AD%Predictor_Subset(M,30,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(30,K,I) * KW_AD( K )      
	      Predictor_AD%Predictor_Subset(M,31,K) = Predictor_AD%Predictor_Subset(M,31,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(31,K,I) * KW_AD( K )      
	      Predictor_AD%Predictor_Subset(M,32,K) = Predictor_AD%Predictor_Subset(M,32,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(32,K,I) * KW_AD( K )      
	      Predictor_AD%Predictor_Subset(M,33,K) = Predictor_AD%Predictor_Subset(M,33,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(33,K,I) * KW_AD( K )      
	      Predictor_AD%Predictor_Subset(M,34,K) = Predictor_AD%Predictor_Subset(M,34,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(34,K,I) * KW_AD( K )      
	      Predictor_AD%Predictor_Subset(M,35,K) = Predictor_AD%Predictor_Subset(M,35,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(35,K,I) * KW_AD( K )      
	      Predictor_AD%Predictor_Subset(M,36,K) = Predictor_AD%Predictor_Subset(M,36,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(36,K,I) * KW_AD( K )      
	      
	      KW_AD( K ) = ZERO	 
            ENDIF

       !    --------------------------
       !    Compute the ozone abs coef
       !    --------------------------
            IF ( KOZO(K) < ZERO ) THEN
               KOZO_AD(K) = ZERO
            ELSE IF ( KOZO(K) > TEN ) THEN
               KOZO_AD(K) = ZERO
            ENDIF

            Predictor_AD%Predictor_Subset(M,16,K) = Predictor_AD%Predictor_Subset(M,16,K) + &
	                   TC%Tuning_Multiple(4,J) * TC%ODCAPS_Subset(M)%C(16,K,I) * KOZO_AD(K)
            Predictor_AD%Predictor_Subset(M,17,K) = Predictor_AD%Predictor_Subset(M,17,K) + &
	                   TC%Tuning_Multiple(4,J) * TC%ODCAPS_Subset(M)%C(17,K,I) * KOZO_AD(K)
            Predictor_AD%Predictor_Subset(M,18,K) = Predictor_AD%Predictor_Subset(M,18,K) + &
	                   TC%Tuning_Multiple(4,J) * TC%ODCAPS_Subset(M)%C(18,K,I) * KOZO_AD(K)
            Predictor_AD%Predictor_Subset(M,19,K) = Predictor_AD%Predictor_Subset(M,19,K) + &
	                   TC%Tuning_Multiple(4,J) * TC%ODCAPS_Subset(M)%C(19,K,I) * KOZO_AD(K)
            Predictor_AD%Predictor_Subset(M,20,K) = Predictor_AD%Predictor_Subset(M,20,K) + &
	                   TC%Tuning_Multiple(4,J) * TC%ODCAPS_Subset(M)%C(20,K,I) * KOZO_AD(K)
            Predictor_AD%Predictor_Subset(M,21,K) = Predictor_AD%Predictor_Subset(M,21,K) + &
	                   TC%Tuning_Multiple(4,J) * TC%ODCAPS_Subset(M)%C(21,K,I) * KOZO_AD(K)
            Predictor_AD%Predictor_Subset(M,22,K) = Predictor_AD%Predictor_Subset(M,22,K) + &
	                   TC%Tuning_Multiple(4,J) * TC%ODCAPS_Subset(M)%C(22,K,I) * KOZO_AD(K)
            Predictor_AD%Predictor_Subset(M,23,K) = Predictor_AD%Predictor_Subset(M,23,K) + &
	                   TC%Tuning_Multiple(4,J) * TC%ODCAPS_Subset(M)%C(23,K,I) * KOZO_AD(K)
            Predictor_AD%Predictor_Subset(M,24,K) = Predictor_AD%Predictor_Subset(M,24,K) + &
	                   TC%Tuning_Multiple(4,J) * TC%ODCAPS_Subset(M)%C(24,K,I) * KOZO_AD(K)
            Predictor_AD%Predictor_Subset(M,25,K) = Predictor_AD%Predictor_Subset(M,25,K) + &
	                   TC%Tuning_Multiple(4,J) * TC%ODCAPS_Subset(M)%C(25,K,I) * KOZO_AD(K)

            KOZO_AD(K) = ZERO

       !    -----------------------------
       !    Calc the fixed gases abs coef
       !    -----------------------------
            IF ( KFIX(K) < ZERO ) THEN	     
 	       KFIX_AD(K) = ZERO			
            ELSE IF ( KFIX(K) > TEN ) THEN      
 	       KFIX_AD(K) = ZERO			
            ENDIF				
!            KFIX_AD(K) = ZERO
	    KFIXX_AD(K) = KFIX_AD(K) * Predictor%Fix_Amount_Multiplier( K )
	    Predictor_AD%Fix_Amount_Multiplier( K ) = Predictor_AD%Fix_Amount_Multiplier( K ) + &
	                                                  KFIXX(K) * KFIX_AD(K)
							  
            Predictor_AD%Predictor_Subset(M, 8,K) = Predictor_AD%Predictor_Subset(M, 8,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C( 8,K,I) * KFIXX_AD(K)
            Predictor_AD%Predictor_Subset(M, 9,K) = Predictor_AD%Predictor_Subset(M, 9,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C( 9,K,I) * KFIXX_AD(K)
            Predictor_AD%Predictor_Subset(M,10,K) = Predictor_AD%Predictor_Subset(M,10,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(10,K,I) * KFIXX_AD(K)
            Predictor_AD%Predictor_Subset(M,11,K) = Predictor_AD%Predictor_Subset(M,11,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(11,K,I) * KFIXX_AD(K)
            Predictor_AD%Predictor_Subset(M,12,K) = Predictor_AD%Predictor_Subset(M,12,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(12,K,I) * KFIXX_AD(K)
            Predictor_AD%Predictor_Subset(M,13,K) = Predictor_AD%Predictor_Subset(M,13,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(13,K,I) * KFIXX_AD(K)
            Predictor_AD%Predictor_Subset(M,14,K) = Predictor_AD%Predictor_Subset(M,14,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(14,K,I) * KFIXX_AD(K)
            Predictor_AD%Predictor_Subset(M,15,K) = Predictor_AD%Predictor_Subset(M,15,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(15,K,I) * KFIXX_AD(K)

            KFIXX_AD(K) = ZERO 
	    KFIX_AD(K) = ZERO
       !    --------------------------- 		   
       !    Compute the water continuum 		   
       !    --------------------------- 		   
            IF ( KCON(k) < ZERO ) THEN		   
               KCON_AD(K) = ZERO				   
            ELSE IF ( KCON(K) > TEN ) THEN 		   
                KCON_AD(K) = ZERO				   
            ENDIF					   
!            KCON_AD(K) = ZERO
            Predictor_AD%Predictor_Subset(M,1,K) = Predictor_AD%Predictor_Subset(M,1,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(1,K,I) * KCON_AD(K)
            Predictor_AD%Predictor_Subset(M,2,K) = Predictor_AD%Predictor_Subset(M,2,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(2,K,I) * KCON_AD(K)
            Predictor_AD%Predictor_Subset(M,3,K) = Predictor_AD%Predictor_Subset(M,3,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(3,K,I) * KCON_AD(K)
            Predictor_AD%Predictor_Subset(M,4,K) = Predictor_AD%Predictor_Subset(M,4,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(4,K,I) * KCON_AD(K)
            Predictor_AD%Predictor_Subset(M,5,K) = Predictor_AD%Predictor_Subset(M,5,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(5,K,I) * KCON_AD(K)
            Predictor_AD%Predictor_Subset(M,6,K) = Predictor_AD%Predictor_Subset(M,6,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(6,K,I) * KCON_AD(K)
            Predictor_AD%Predictor_Subset(M,7,K) = Predictor_AD%Predictor_Subset(M,7,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(7,K,I) * KCON_AD(K)
            KCON_AD(K) = ZERO

	   CASE ( 3 )
       !    --------------------------
       !    The other gases abs coef
       !    --------------------------
      	    KCO_AD(K)  = ZERO
	    KOZO_AD(K) = ZERO

       !    --------------------------
       !    Compute the water abs coef
       !    --------------------------
            IF (LH2O) THEN
       !    Not an OPTRAN water channel
              IF ( KW( K ) < ZERO ) THEN
 	         KW_AD( K ) = ZERO
!              ELSE IF( KW( K ) > TEN ) THEN
!                 KW_AD( K ) = ZERO
	      ENDIF
	      
	      Predictor_AD%Predictor_Subset(M,25,K) = Predictor_AD%Predictor_Subset(M,25,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(25,K,I) * KW_AD( K )      
	      Predictor_AD%Predictor_Subset(M,26,K) = Predictor_AD%Predictor_Subset(M,26,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(26,K,I) * KW_AD( K )      
	      Predictor_AD%Predictor_Subset(M,27,K) = Predictor_AD%Predictor_Subset(M,27,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(27,K,I) * KW_AD( K )      
	      Predictor_AD%Predictor_Subset(M,28,K) = Predictor_AD%Predictor_Subset(M,28,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(28,K,I) * KW_AD( K )      
	      Predictor_AD%Predictor_Subset(M,29,K) = Predictor_AD%Predictor_Subset(M,29,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(29,K,I) * KW_AD( K )      
	      Predictor_AD%Predictor_Subset(M,30,K) = Predictor_AD%Predictor_Subset(M,30,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(30,K,I) * KW_AD( K )      
	      Predictor_AD%Predictor_Subset(M,31,K) = Predictor_AD%Predictor_Subset(M,31,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(31,K,I) * KW_AD( K )      
	      Predictor_AD%Predictor_Subset(M,32,K) = Predictor_AD%Predictor_Subset(M,32,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(32,K,I) * KW_AD( K )      
	      Predictor_AD%Predictor_Subset(M,33,K) = Predictor_AD%Predictor_Subset(M,33,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(33,K,I) * KW_AD( K )      
	      Predictor_AD%Predictor_Subset(M,34,K) = Predictor_AD%Predictor_Subset(M,34,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(34,K,I) * KW_AD( K )      
	      Predictor_AD%Predictor_Subset(M,35,K) = Predictor_AD%Predictor_Subset(M,35,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(35,K,I) * KW_AD( K )      
 	      	 
 	      KW_AD( K ) = ZERO
            ENDIF
	    
       !    ----------------------------
       !    Compute the methane abs coef
       !    ----------------------------
             IF ( KMET(k) < ZERO ) THEN
                KMET_AD(K) = ZERO
             ELSEIF ( KMET(K) > TEN ) THEN
                KMET_AD(K) = ZERO
             ENDIF
!            KMET_AD(K) = ZERO

            Predictor_AD%Predictor_Subset(M,16,K) = Predictor_AD%Predictor_Subset(M,16,K) + &
	                   TC%Tuning_Multiple(6,J) * TC%ODCAPS_Subset(M)%C(16,K,I) * KMET_AD(K)
            Predictor_AD%Predictor_Subset(M,17,K) = Predictor_AD%Predictor_Subset(M,17,K) + &
	                   TC%Tuning_Multiple(6,J) * TC%ODCAPS_Subset(M)%C(17,K,I) * KMET_AD(K)
            Predictor_AD%Predictor_Subset(M,18,K) = Predictor_AD%Predictor_Subset(M,18,K) + &
	                   TC%Tuning_Multiple(6,J) * TC%ODCAPS_Subset(M)%C(18,K,I) * KMET_AD(K)
            Predictor_AD%Predictor_Subset(M,19,K) = Predictor_AD%Predictor_Subset(M,19,K) + &
	                   TC%Tuning_Multiple(6,J) * TC%ODCAPS_Subset(M)%C(19,K,I) * KMET_AD(K)
            Predictor_AD%Predictor_Subset(M,20,K) = Predictor_AD%Predictor_Subset(M,20,K) + &
	                   TC%Tuning_Multiple(6,J) * TC%ODCAPS_Subset(M)%C(20,K,I) * KMET_AD(K)
            Predictor_AD%Predictor_Subset(M,21,K) = Predictor_AD%Predictor_Subset(M,21,K) + &
	                   TC%Tuning_Multiple(6,J) * TC%ODCAPS_Subset(M)%C(21,K,I) * KMET_AD(K)
            Predictor_AD%Predictor_Subset(M,22,K) = Predictor_AD%Predictor_Subset(M,22,K) + &
	                   TC%Tuning_Multiple(6,J) * TC%ODCAPS_Subset(M)%C(22,K,I) * KMET_AD(K)
            Predictor_AD%Predictor_Subset(M,23,K) = Predictor_AD%Predictor_Subset(M,23,K) + &
	                   TC%Tuning_Multiple(6,J) * TC%ODCAPS_Subset(M)%C(23,K,I) * KMET_AD(K)
            Predictor_AD%Predictor_Subset(M,24,K) = Predictor_AD%Predictor_Subset(M,24,K) + &
	                   TC%Tuning_Multiple(6,J) * TC%ODCAPS_Subset(M)%C(24,K,I) * KMET_AD(K)
 	     
            KMET_AD(K) = ZERO
       !    -----------------------------
       !    Calc the fixed gases abs coef
       !    -----------------------------
            IF ( KFIX(K) < ZERO ) THEN	     
 	       KFIX_AD(K) = ZERO			
            ELSE IF ( KFIX(K) > TEN ) THEN      
 	       KFIX_AD(K) = ZERO			
            ENDIF				
!            KFIX_AD(K) = ZERO
	    KFIXX_AD(K) = KFIX_AD(K) * Predictor%Fix_Amount_Multiplier( K )
	    Predictor_AD%Fix_Amount_Multiplier( K ) = Predictor_AD%Fix_Amount_Multiplier( K ) + &
	                                                  KFIXX(K) * KFIX_AD(K)
							  
            Predictor_AD%Predictor_Subset(M, 8,K) = Predictor_AD%Predictor_Subset(M, 8,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C( 8,K,I) * KFIXX_AD(K)
            Predictor_AD%Predictor_Subset(M, 9,K) = Predictor_AD%Predictor_Subset(M, 9,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C( 9,K,I) * KFIXX_AD(K)
            Predictor_AD%Predictor_Subset(M,10,K) = Predictor_AD%Predictor_Subset(M,10,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(10,K,I) * KFIXX_AD(K)
            Predictor_AD%Predictor_Subset(M,11,K) = Predictor_AD%Predictor_Subset(M,11,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(11,K,I) * KFIXX_AD(K)
            Predictor_AD%Predictor_Subset(M,12,K) = Predictor_AD%Predictor_Subset(M,12,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(12,K,I) * KFIXX_AD(K)
            Predictor_AD%Predictor_Subset(M,13,K) = Predictor_AD%Predictor_Subset(M,13,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(13,K,I) * KFIXX_AD(K)
            Predictor_AD%Predictor_Subset(M,14,K) = Predictor_AD%Predictor_Subset(M,14,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(14,K,I) * KFIXX_AD(K)
            Predictor_AD%Predictor_Subset(M,15,K) = Predictor_AD%Predictor_Subset(M,15,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(15,K,I) * KFIXX_AD(K)
	    KFIXX_AD(K) = ZERO
	    KFIX_AD(K) = ZERO
       !    --------------------------- 		   
       !    Compute the water continuum 		   
       !    --------------------------- 		   
            IF ( KCON(k) < ZERO ) THEN		   
               KCON_AD(K) = ZERO				   
            ELSE IF ( KCON(K) > TEN ) THEN 		   
                KCON_AD(K) = ZERO				   
            ENDIF					   
!            KCON_AD(K) = ZERO
            Predictor_AD%Predictor_Subset(M,1,K) = Predictor_AD%Predictor_Subset(M,1,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(1,K,I) * KCON_AD(K)
            Predictor_AD%Predictor_Subset(M,2,K) = Predictor_AD%Predictor_Subset(M,2,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(2,K,I) * KCON_AD(K)
            Predictor_AD%Predictor_Subset(M,3,K) = Predictor_AD%Predictor_Subset(M,3,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(3,K,I) * KCON_AD(K)
            Predictor_AD%Predictor_Subset(M,4,K) = Predictor_AD%Predictor_Subset(M,4,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(4,K,I) * KCON_AD(K)
            Predictor_AD%Predictor_Subset(M,5,K) = Predictor_AD%Predictor_Subset(M,5,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(5,K,I) * KCON_AD(K)
            Predictor_AD%Predictor_Subset(M,6,K) = Predictor_AD%Predictor_Subset(M,6,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(6,K,I) * KCON_AD(K)
            Predictor_AD%Predictor_Subset(M,7,K) = Predictor_AD%Predictor_Subset(M,7,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(7,K,I) * KCON_AD(K)
	    
            KCON_AD(K) = ZERO
	    
	   CASE ( 4 )
       !    --------------------------
       !    The other gases abs coef
       !    --------------------------
 	    KMET_AD(K) = ZERO

       !    --------------------------
       !    Compute the water abs coef
       !    --------------------------
            IF (LH2O) THEN
       !    Not an OPTRAN water channel
              IF ( KW( K ) < ZERO ) THEN
 	         KW_AD( K ) = ZERO
              ELSE IF( KW( K ) > TEN ) THEN
                 KW_AD( K ) = ZERO
	      ENDIF
	      
	      Predictor_Subset_AD(M-3,33,K) = Predictor_Subset_AD(M-3,33,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(33,K,I) * KW_AD( K )      
	      Predictor_Subset_AD(M-3,34,K) = Predictor_Subset_AD(M-3,34,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(34,K,I) * KW_AD( K )      
	      Predictor_Subset_AD(M-3,35,K) = Predictor_Subset_AD(M-3,35,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(35,K,I) * KW_AD( K )      
 	      Predictor_Subset_AD(M-3,36,K) = Predictor_Subset_AD(M-3,36,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(36,K,I) * KW_AD( K )      
	      Predictor_Subset_AD(M-3,37,K) = Predictor_Subset_AD(M-3,37,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(37,K,I) * KW_AD( K )      
	      Predictor_Subset_AD(M-3,38,K) = Predictor_Subset_AD(M-3,38,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(38,K,I) * KW_AD( K )      
	      Predictor_Subset_AD(M-3,39,K) = Predictor_Subset_AD(M-3,39,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(39,K,I) * KW_AD( K )      
	      Predictor_Subset_AD(M-3,40,K) = Predictor_Subset_AD(M-3,40,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(40,K,I) * KW_AD( K )      
	      Predictor_Subset_AD(M-3,41,K) = Predictor_Subset_AD(M-3,41,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(41,K,I) * KW_AD( K )      
	      Predictor_Subset_AD(M-3,42,K) = Predictor_Subset_AD(M-3,42,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(42,K,I) * KW_AD( K )      
	      Predictor_Subset_AD(M-3,43,K) = Predictor_Subset_AD(M-3,43,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(43,K,I) * KW_AD( K )      
	      Predictor_Subset_AD(M-3,44,K) = Predictor_Subset_AD(M-3,44,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(44,K,I) * KW_AD( K )      
	      Predictor_Subset_AD(M-3,45,K) = Predictor_Subset_AD(M-3,45,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(45,K,I) * KW_AD( K )      
 	      	 
              KW_AD( K ) = ZERO
            ENDIF

       !    --------------------------
       !    Compute the ozone abs coef
       !    --------------------------
            IF ( KOZO(K) < ZERO ) THEN
               KOZO_AD(K) = ZERO
            ELSE IF ( KOZO(K) > TEN ) THEN
               KOZO_AD(K) = ZERO
            ENDIF

            Predictor_Subset_AD(M-3,30,K) = Predictor_Subset_AD(M-3,30,K) + &
	                   TC%Tuning_Multiple(4,J) * TC%ODCAPS_Subset(M)%C(30,K,I) * KOZO_AD(K)
            Predictor_Subset_AD(M-3,31,K) = Predictor_Subset_AD(M-3,31,K) + &
	                   TC%Tuning_Multiple(4,J) * TC%ODCAPS_Subset(M)%C(31,K,I) * KOZO_AD(K)
            Predictor_Subset_AD(M-3,32,K) = Predictor_Subset_AD(M-3,32,K) + &
	                   TC%Tuning_Multiple(4,J) * TC%ODCAPS_Subset(M)%C(32,K,I) * KOZO_AD(K)
            KOZO_AD(K) = ZERO
	     
       !    -----------------------
       !    Compute the CO abs coef
       !    -----------------------
            IF ( KCO(K) < ZERO ) THEN	  
               KCO_AD(K) = ZERO		  
            ELSEIF ( KCO(K) > TEN ) THEN 
               KCO_AD(K) = ZERO		  
            ENDIF
!            KCO_AD(K) = ZERO		  
	    			  
	    Predictor_Subset_AD(M-3,19,K) = Predictor_Subset_AD(M-3,19,K) + &				 
            		   TC%Tuning_Multiple(5,J) * TC%ODCAPS_Subset(M)%C(19,K,I) * KCO_AD( K )	 
	    Predictor_Subset_AD(M-3,20,K) = Predictor_Subset_AD(M-3,20,K) + &				 
            		   TC%Tuning_Multiple(5,J) * TC%ODCAPS_Subset(M)%C(20,K,I) * KCO_AD( K )	 
	    Predictor_Subset_AD(M-3,21,K) = Predictor_Subset_AD(M-3,21,K) + &				 
            		   TC%Tuning_Multiple(5,J) * TC%ODCAPS_Subset(M)%C(21,K,I) * KCO_AD( K )	 
 	    Predictor_Subset_AD(M-3,22,K) = Predictor_Subset_AD(M-3,22,K) + &				 
            		   TC%Tuning_Multiple(5,J) * TC%ODCAPS_Subset(M)%C(22,K,I) * KCO_AD( K )	 
	    Predictor_Subset_AD(M-3,23,K) = Predictor_Subset_AD(M-3,23,K) + &				 
            		   TC%Tuning_Multiple(5,J) * TC%ODCAPS_Subset(M)%C(23,K,I) * KCO_AD( K )	 
	    Predictor_Subset_AD(M-3,24,K) = Predictor_Subset_AD(M-3,24,K) + &				 
            		   TC%Tuning_Multiple(5,J) * TC%ODCAPS_Subset(M)%C(24,K,I) * KCO_AD( K )	 
	    Predictor_Subset_AD(M-3,25,K) = Predictor_Subset_AD(M-3,25,K) + &				 
            		   TC%Tuning_Multiple(5,J) * TC%ODCAPS_Subset(M)%C(25,K,I) * KCO_AD( K )	 
	    Predictor_Subset_AD(M-3,26,K) = Predictor_Subset_AD(M-3,26,K) + &				 
            		   TC%Tuning_Multiple(5,J) * TC%ODCAPS_Subset(M)%C(26,K,I) * KCO_AD( K )	 
	    Predictor_Subset_AD(M-3,27,K) = Predictor_Subset_AD(M-3,27,K) + &				 
            		   TC%Tuning_Multiple(5,J) * TC%ODCAPS_Subset(M)%C(27,K,I) * KCO_AD( K )	 
	    Predictor_Subset_AD(M-3,28,K) = Predictor_Subset_AD(M-3,28,K) + &				 
            		   TC%Tuning_Multiple(5,J) * TC%ODCAPS_Subset(M)%C(28,K,I) * KCO_AD( K )	 
	    Predictor_Subset_AD(M-3,29,K) = Predictor_Subset_AD(M-3,29,K) + &				 
            		   TC%Tuning_Multiple(5,J) * TC%ODCAPS_Subset(M)%C(29,K,I) * KCO_AD( K )	 
            KCO_AD(K) = ZERO		  

       !    -----------------------------
       !    Calc the fixed gases abs coef
       !    -----------------------------
            IF ( KFIX(K) < ZERO ) THEN	     
 	       KFIX_AD(K) = ZERO			
            ELSE IF ( KFIX(K) > TEN ) THEN      
 	       KFIX_AD(K) = ZERO			
            ENDIF				
!            KFIX_AD(K) = ZERO
	    KFIXX_AD(K) = KFIX_AD(K) * Predictor%Fix_Amount_Multiplier( K )
	    Predictor_AD%Fix_Amount_Multiplier( K ) = Predictor_AD%Fix_Amount_Multiplier( K ) + &
	                                                  KFIXX(K) * KFIX_AD(K)
							  
            Predictor_Subset_AD(M-3, 8,K) = Predictor_Subset_AD(M-3, 8,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C( 8,K,I) * KFIXX_AD(K)
            Predictor_Subset_AD(M-3, 9,K) = Predictor_Subset_AD(M-3, 9,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C( 9,K,I) * KFIXX_AD(K)
            Predictor_Subset_AD(M-3,10,K) = Predictor_Subset_AD(M-3,10,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(10,K,I) * KFIXX_AD(K)
            Predictor_Subset_AD(M-3,11,K) = Predictor_Subset_AD(M-3,11,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(11,K,I) * KFIXX_AD(K)
            Predictor_Subset_AD(M-3,12,K) = Predictor_Subset_AD(M-3,12,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(12,K,I) * KFIXX_AD(K)
            Predictor_Subset_AD(M-3,13,K) = Predictor_Subset_AD(M-3,13,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(13,K,I) * KFIXX_AD(K)
            Predictor_Subset_AD(M-3,14,K) = Predictor_Subset_AD(M-3,14,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(14,K,I) * KFIXX_AD(K)
            Predictor_Subset_AD(M-3,15,K) = Predictor_Subset_AD(M-3,15,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(15,K,I) * KFIXX_AD(K)
            Predictor_Subset_AD(M-3,16,K) = Predictor_Subset_AD(M-3,16,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(16,K,I) * KFIXX_AD(K)
            Predictor_Subset_AD(M-3,17,K) = Predictor_Subset_AD(M-3,17,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(17,K,I) * KFIXX_AD(K)
            Predictor_Subset_AD(M-3,18,K) = Predictor_Subset_AD(M-3,18,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(18,K,I) * KFIXX_AD(K)
            
	    KFIXX_AD(K) = ZERO
	    KFIX_AD(K) = ZERO
       !    --------------------------- 		   
       !    Compute the water continuum 		   
       !    --------------------------- 		   
            IF ( KCON(k) < ZERO ) THEN		   
               KCON_AD(K) = ZERO				   
            ELSE IF ( KCON(K) > TEN ) THEN 		   
                KCON_AD(K) = ZERO				   
            ENDIF					   
!            KCON_AD(K) = ZERO
            Predictor_Subset_AD(M-3,1,K) = Predictor_Subset_AD(M-3,1,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(1,K,I) * KCON_AD(K)
            Predictor_Subset_AD(M-3,2,K) = Predictor_Subset_AD(M-3,2,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(2,K,I) * KCON_AD(K)
            Predictor_Subset_AD(M-3,3,K) = Predictor_Subset_AD(M-3,3,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(3,K,I) * KCON_AD(K)
            Predictor_Subset_AD(M-3,4,K) = Predictor_Subset_AD(M-3,4,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(4,K,I) * KCON_AD(K)
            Predictor_Subset_AD(M-3,5,K) = Predictor_Subset_AD(M-3,5,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(5,K,I) * KCON_AD(K)
            Predictor_Subset_AD(M-3,6,K) = Predictor_Subset_AD(M-3,6,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(6,K,I) * KCON_AD(K)
            Predictor_Subset_AD(M-3,7,K) = Predictor_Subset_AD(M-3,7,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(7,K,I) * KCON_AD(K)

            KCON_AD(K) = ZERO
	    
	   CASE ( 5 )

       !    --------------------------
       !    The other gases abs coef
       !    --------------------------
       	    KCO_AD(K)  = ZERO
	    KMET_AD(K) = ZERO
 
       !    --------------------------
       !    Compute the ozone abs coef
       !    --------------------------
            IF ( KOZO(K) < ZERO ) THEN
               KOZO_AD(K) = ZERO
            ELSE IF ( KOZO(K) > TEN ) THEN
               KOZO_AD(K) = ZERO
            ENDIF

            Predictor_Subset_AD(M-3,22,K) = Predictor_Subset_AD(M-3,22,K) + &
	                   TC%Tuning_Multiple(4,J) * TC%ODCAPS_Subset(M)%C(22,K,I) * KOZO_AD(K)
            KOZO_AD(K) = ZERO
	   
       !    --------------------------
       !    Compute the water abs coef
       !    --------------------------
            IF (LH2O) THEN
       !    Not an OPTRAN water channel
              IF ( KW( K ) < ZERO ) THEN
 	         KW_AD( K ) = ZERO
              ELSE IF( KW( K ) > TEN ) THEN
                 KW_AD( K ) = ZERO
	      ENDIF
	      
	      Predictor_Subset_AD(M-3,19,K) = Predictor_Subset_AD(M-3,19,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(19,K,I) * KW_AD( K )      
	      Predictor_Subset_AD(M-3,20,K) = Predictor_Subset_AD(M-3,20,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(20,K,I) * KW_AD( K )      
	      Predictor_Subset_AD(M-3,21,K) = Predictor_Subset_AD(M-3,21,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(21,K,I) * KW_AD( K )      

              KW_AD( K ) = ZERO
	    ENDIF

       !    -----------------------------
       !    Calc the fixed gases abs coef
       !    -----------------------------
            IF ( KFIX(K) < ZERO ) THEN	     
 	       KFIX_AD(K) = ZERO			
            ELSE IF ( KFIX(K) > TEN ) THEN      
 	       KFIX_AD(K) = ZERO			
            ENDIF				
!            KFIX_AD(K) = ZERO
	    KFIXX_AD(K) = KFIX_AD(K) * Predictor%Fix_Amount_Multiplier( K )
	    Predictor_AD%Fix_Amount_Multiplier( K ) = Predictor_AD%Fix_Amount_Multiplier( K ) + &
	                                                  KFIXX(K) * KFIX_AD(K)
							  
            Predictor_Subset_AD(M-3, 8,K) = Predictor_Subset_AD(M-3, 8,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C( 8,K,I) * KFIXX_AD(K)
            Predictor_Subset_AD(M-3, 9,K) = Predictor_Subset_AD(M-3, 9,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C( 9,K,I) * KFIXX_AD(K)
            Predictor_Subset_AD(M-3,10,K) = Predictor_Subset_AD(M-3,10,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(10,K,I) * KFIXX_AD(K)
            Predictor_Subset_AD(M-3,11,K) = Predictor_Subset_AD(M-3,11,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(11,K,I) * KFIXX_AD(K)
            Predictor_Subset_AD(M-3,12,K) = Predictor_Subset_AD(M-3,12,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(12,K,I) * KFIXX_AD(K)
            Predictor_Subset_AD(M-3,13,K) = Predictor_Subset_AD(M-3,13,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(13,K,I) * KFIXX_AD(K)
            Predictor_Subset_AD(M-3,14,K) = Predictor_Subset_AD(M-3,14,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(14,K,I) * KFIXX_AD(K)
            Predictor_Subset_AD(M-3,15,K) = Predictor_Subset_AD(M-3,15,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(15,K,I) * KFIXX_AD(K)
            Predictor_Subset_AD(M-3,16,K) = Predictor_Subset_AD(M-3,16,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(16,K,I) * KFIXX_AD(K)
            Predictor_Subset_AD(M-3,17,K) = Predictor_Subset_AD(M-3,17,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(17,K,I) * KFIXX_AD(K)
            Predictor_Subset_AD(M-3,18,K) = Predictor_Subset_AD(M-3,18,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(18,K,I) * KFIXX_AD(K)

            KFIXX_AD(K) = ZERO 
	    KFIX_AD(K) = ZERO
       !    --------------------------- 		   
       !    Compute the water continuum 		   
       !    --------------------------- 		   
            IF ( KCON(k) < ZERO ) THEN		   
               KCON_AD(K) = ZERO				   
            ELSE IF ( KCON(K) > TEN ) THEN 		   
                KCON_AD(K) = ZERO				   
            ENDIF					   
!            KCON_AD(K) = ZERO
            Predictor_Subset_AD(M-3,1,K) = Predictor_Subset_AD(M-3,1,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(1,K,I) * KCON_AD(K)
            Predictor_Subset_AD(M-3,2,K) = Predictor_Subset_AD(M-3,2,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(2,K,I) * KCON_AD(K)
            Predictor_Subset_AD(M-3,3,K) = Predictor_Subset_AD(M-3,3,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(3,K,I) * KCON_AD(K)
            Predictor_Subset_AD(M-3,4,K) = Predictor_Subset_AD(M-3,4,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(4,K,I) * KCON_AD(K)
            Predictor_Subset_AD(M-3,5,K) = Predictor_Subset_AD(M-3,5,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(5,K,I) * KCON_AD(K)
            Predictor_Subset_AD(M-3,6,K) = Predictor_Subset_AD(M-3,6,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(6,K,I) * KCON_AD(K)
            Predictor_Subset_AD(M-3,7,K) = Predictor_Subset_AD(M-3,7,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(7,K,I) * KCON_AD(K)

            KCON_AD(K) = ZERO
 
	   CASE ( 6 )
       !    --------------------------
       !    The other gases abs coef
       !    --------------------------
       	    KCO_AD(K)  = ZERO
	    KMET_AD(K) = ZERO
 
       !    --------------------------
       !    Compute the ozone abs coef
       !    --------------------------
            IF ( KOZO(K) < ZERO ) THEN
               KOZO_AD(K) = ZERO
            ELSE IF ( KOZO(K) > TEN ) THEN
               KOZO_AD(K) = ZERO
            ENDIF

            Predictor_Subset_AD(M-3,23,K) = Predictor_Subset_AD(M-3,23,K) + &
	                   TC%Tuning_Multiple(4,J) * TC%ODCAPS_Subset(M)%C(23,K,I) * KOZO_AD(K)
            KOZO_AD(K) = ZERO
	   
       !    --------------------------
       !    Compute the water abs coef
       !    --------------------------
            IF (LH2O) THEN
       !    Not an OPTRAN water channel
              IF ( KW( K ) < ZERO ) THEN
 	         KW_AD( K ) = ZERO
              ELSE IF( KW( K ) > TEN ) THEN
                 KW_AD( K ) = ZERO
	      ENDIF
	      
              Predictor_Subset_AD(M-3,16,K) = Predictor_Subset_AD(M-3,16,K) + &
	                     TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(16,K,I) * KW_AD( K )
              Predictor_Subset_AD(M-3,17,K) = Predictor_Subset_AD(M-3,17,K) + &
	                     TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(17,K,I) * KW_AD( K )
              Predictor_Subset_AD(M-3,18,K) = Predictor_Subset_AD(M-3,18,K) + &
	                     TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(18,K,I) * KW_AD( K )
	      Predictor_Subset_AD(M-3,19,K) = Predictor_Subset_AD(M-3,19,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(19,K,I) * KW_AD( K )      
	      Predictor_Subset_AD(M-3,20,K) = Predictor_Subset_AD(M-3,20,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(20,K,I) * KW_AD( K )      
	      Predictor_Subset_AD(M-3,21,K) = Predictor_Subset_AD(M-3,21,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(21,K,I) * KW_AD( K )      
	      Predictor_Subset_AD(M-3,22,K) = Predictor_Subset_AD(M-3,22,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(22,K,I) * KW_AD( K )      
              KW_AD( K ) = ZERO

	    ENDIF

       !    -----------------------------
       !    Calc the fixed gases abs coef
       !    -----------------------------
            IF ( KFIX(K) < ZERO ) THEN	     
 	       KFIX_AD(K) = ZERO			
            ELSE IF ( KFIX(K) > TEN ) THEN      
 	       KFIX_AD(K) = ZERO			
            ENDIF				
!            KFIX_AD(K) = ZERO
	    KFIXX_AD(K) = KFIX_AD(K) * Predictor%Fix_Amount_Multiplier( K )
	    Predictor_AD%Fix_Amount_Multiplier( K ) = Predictor_AD%Fix_Amount_Multiplier( K ) + &
	                                                  KFIXX(K) * KFIX_AD(K)
							  
            Predictor_Subset_AD(M-3, 8,K) = Predictor_Subset_AD(M-3, 8,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C( 8,K,I) * KFIXX_AD(K)
            Predictor_Subset_AD(M-3, 9,K) = Predictor_Subset_AD(M-3, 9,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C( 9,K,I) * KFIXX_AD(K)
            Predictor_Subset_AD(M-3,10,K) = Predictor_Subset_AD(M-3,10,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(10,K,I) * KFIXX_AD(K)
            Predictor_Subset_AD(M-3,11,K) = Predictor_Subset_AD(M-3,11,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(11,K,I) * KFIXX_AD(K)
            Predictor_Subset_AD(M-3,12,K) = Predictor_Subset_AD(M-3,12,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(12,K,I) * KFIXX_AD(K)
            Predictor_Subset_AD(M-3,13,K) = Predictor_Subset_AD(M-3,13,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(13,K,I) * KFIXX_AD(K)
            Predictor_Subset_AD(M-3,14,K) = Predictor_Subset_AD(M-3,14,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(14,K,I) * KFIXX_AD(K)
            Predictor_Subset_AD(M-3,15,K) = Predictor_Subset_AD(M-3,15,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(15,K,I) * KFIXX_AD(K)

            KFIXX_AD(K) = ZERO
	    KFIX_AD(K) = ZERO
       !    --------------------------- 		   
       !    Compute the water continuum 		   
       !    --------------------------- 		   
            IF ( KCON(k) < ZERO ) THEN		   
               KCON_AD(K) = ZERO				   
            ELSE IF ( KCON(K) > TEN ) THEN 		   
                KCON_AD(K) = ZERO				   
            ENDIF					   
!            KCON_AD(K) = ZERO
            Predictor_Subset_AD(M-3,1,K) = Predictor_Subset_AD(M-3,1,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(1,K,I) * KCON_AD(K)
            Predictor_Subset_AD(M-3,2,K) = Predictor_Subset_AD(M-3,2,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(2,K,I) * KCON_AD(K)
            Predictor_Subset_AD(M-3,3,K) = Predictor_Subset_AD(M-3,3,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(3,K,I) * KCON_AD(K)
            Predictor_Subset_AD(M-3,4,K) = Predictor_Subset_AD(M-3,4,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(4,K,I) * KCON_AD(K)
            Predictor_Subset_AD(M-3,5,K) = Predictor_Subset_AD(M-3,5,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(5,K,I) * KCON_AD(K)
            Predictor_Subset_AD(M-3,6,K) = Predictor_Subset_AD(M-3,6,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(6,K,I) * KCON_AD(K)
            Predictor_Subset_AD(M-3,7,K) = Predictor_Subset_AD(M-3,7,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(7,K,I) * KCON_AD(K)
            KCON_AD(K) = ZERO	   
 
	   CASE ( 7 )
	   
       !    --------------------------
       !    The other gases abs coef
       !    --------------------------
       	    KCO_AD(K)  = ZERO
	    KMET_AD(K) = ZERO
 
       !    --------------------------
       !    Compute the ozone abs coef
       !    --------------------------
            IF ( KOZO(K) < ZERO ) THEN
               KOZO_AD(K) = ZERO
            ELSE IF ( KOZO(K) > TEN ) THEN
               KOZO_AD(K) = ZERO
            ENDIF

            Predictor_Subset_AD(M-3,29,K) = Predictor_Subset_AD(M-3,29,K) + &
	                   TC%Tuning_Multiple(4,J) * TC%ODCAPS_Subset(M)%C(29,K,I) * KOZO_AD(K)

            KOZO_AD(K) = ZERO
	   
       !    --------------------------
       !    Compute the water abs coef
       !    --------------------------
            IF (LH2O) THEN
       !    Not an OPTRAN water channel
              IF ( KW( K ) < ZERO ) THEN
 	         KW_AD( K ) = ZERO
              ELSE IF( KW( K ) > TEN ) THEN
                 KW_AD( K ) = ZERO
	      ENDIF
	      
              Predictor_Subset_AD(M-3,16,K) = Predictor_Subset_AD(M-3,16,K) + &
	                     TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(16,K,I) * KW_AD( K )
              Predictor_Subset_AD(M-3,17,K) = Predictor_Subset_AD(M-3,17,K) + &
	                     TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(17,K,I) * KW_AD( K )
              Predictor_Subset_AD(M-3,18,K) = Predictor_Subset_AD(M-3,18,K) + &
	                     TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(18,K,I) * KW_AD( K )
	      Predictor_Subset_AD(M-3,19,K) = Predictor_Subset_AD(M-3,19,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(19,K,I) * KW_AD( K )      
	      Predictor_Subset_AD(M-3,20,K) = Predictor_Subset_AD(M-3,20,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(20,K,I) * KW_AD( K )      
	      Predictor_Subset_AD(M-3,21,K) = Predictor_Subset_AD(M-3,21,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(21,K,I) * KW_AD( K )      
	      Predictor_Subset_AD(M-3,22,K) = Predictor_Subset_AD(M-3,22,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(22,K,I) * KW_AD( K )      
	      Predictor_Subset_AD(M-3,23,K) = Predictor_Subset_AD(M-3,23,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(23,K,I) * KW_AD( K )      
	      Predictor_Subset_AD(M-3,24,K) = Predictor_Subset_AD(M-3,24,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(24,K,I) * KW_AD( K )      
	      Predictor_Subset_AD(M-3,25,K) = Predictor_Subset_AD(M-3,25,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(25,K,I) * KW_AD( K )      
	      Predictor_Subset_AD(M-3,26,K) = Predictor_Subset_AD(M-3,26,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(26,K,I) * KW_AD( K )      
	      Predictor_Subset_AD(M-3,27,K) = Predictor_Subset_AD(M-3,27,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(27,K,I) * KW_AD( K )      
	      Predictor_Subset_AD(M-3,28,K) = Predictor_Subset_AD(M-3,28,K) + &
                             TC%Tuning_Multiple(2,J) * TC%ODCAPS_Subset(M)%C(28,K,I) * KW_AD( K )      
              KW_AD( K ) = ZERO
	    
	    ENDIF

       !    -----------------------------
       !    Calc the fixed gases abs coef
       !    -----------------------------
            IF ( KFIX(K) < ZERO ) THEN	     
 	       KFIX_AD(K) = ZERO			
            ELSE IF ( KFIX(K) > TEN ) THEN      
 	       KFIX_AD(K) = ZERO			
            ENDIF				
!            KFIX_AD(K) = ZERO
	    KFIXX_AD(K) = KFIX_AD(K) * Predictor%Fix_Amount_Multiplier( K )
	    Predictor_AD%Fix_Amount_Multiplier( K ) = Predictor_AD%Fix_Amount_Multiplier( K ) + &
	                                                  KFIXX(K) * KFIX_AD(K)
							  
            Predictor_Subset_AD(M-3, 8,K) = Predictor_Subset_AD(M-3, 8,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C( 8,K,I) * KFIXX_AD(K)
            Predictor_Subset_AD(M-3, 9,K) = Predictor_Subset_AD(M-3, 9,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C( 9,K,I) * KFIXX_AD(K)
            Predictor_Subset_AD(M-3,10,K) = Predictor_Subset_AD(M-3,10,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(10,K,I) * KFIXX_AD(K)
            Predictor_Subset_AD(M-3,11,K) = Predictor_Subset_AD(M-3,11,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(11,K,I) * KFIXX_AD(K)
            Predictor_Subset_AD(M-3,12,K) = Predictor_Subset_AD(M-3,12,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(12,K,I) * KFIXX_AD(K)
            Predictor_Subset_AD(M-3,13,K) = Predictor_Subset_AD(M-3,13,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(13,K,I) * KFIXX_AD(K)
            Predictor_Subset_AD(M-3,14,K) = Predictor_Subset_AD(M-3,14,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(14,K,I) * KFIXX_AD(K)
            Predictor_Subset_AD(M-3,15,K) = Predictor_Subset_AD(M-3,15,K) + &
	                  TC%Tuning_Multiple(1,J) * TC%ODCAPS_Subset(M)%C(15,K,I) * KFIXX_AD(K)
            KFIXX_AD(K) = ZERO 
	    KFIX_AD(K) = ZERO
       !    --------------------------- 		   
       !    Compute the water continuum 		   
       !    --------------------------- 		   
            IF ( KCON(k) < ZERO ) THEN		   
               KCON_AD(K) = ZERO				   
            ELSE IF ( KCON(K) > TEN ) THEN 		   
                KCON_AD(K) = ZERO				   
            ENDIF					   
!            KCON_AD(K) = ZERO
            Predictor_Subset_AD(M-3,1,K) = Predictor_Subset_AD(M-3,1,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(1,K,I) * KCON_AD(K)
            Predictor_Subset_AD(M-3,2,K) = Predictor_Subset_AD(M-3,2,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(2,K,I) * KCON_AD(K)
            Predictor_Subset_AD(M-3,3,K) = Predictor_Subset_AD(M-3,3,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(3,K,I) * KCON_AD(K)
            Predictor_Subset_AD(M-3,4,K) = Predictor_Subset_AD(M-3,4,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(4,K,I) * KCON_AD(K)
            Predictor_Subset_AD(M-3,5,K) = Predictor_Subset_AD(M-3,5,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(5,K,I) * KCON_AD(K)
            Predictor_Subset_AD(M-3,6,K) = Predictor_Subset_AD(M-3,6,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(6,K,I) * KCON_AD(K)
            Predictor_Subset_AD(M-3,7,K) = Predictor_Subset_AD(M-3,7,K) + &
                          TC%Tuning_Multiple(3,J) * TC%ODCAPS_Subset(M)%C(7,K,I) * KCON_AD(K)
            KCON_AD(K) = ZERO
	    	   
        END SELECT AD_Subset_Select

    END DO Adjoint_Layer_Loop
    
    IF ( Cal_Sun ) THEN
!      Predictor_AD%Sun_Fudge = Predictor_AD%Sun_Fudge + XZ_AD
      XZ_AD = ZERO
      Predictor_AD%TraceGas_Predictors_Sun = Predictor_AD%TraceGas_Predictors_Sun &
                     + TraceGas_Predictors_AD
      Predictor_AD%Predictor_Subset_Sun(1:4, :, :) = Predictor_AD%Predictor_Subset_Sun(1:4, :, :) & 
                     +  Predictor_Subset_AD(1:4, :, :) 
    ELSE
      XZ_AD = ZERO
      Predictor_AD%TraceGas_Predictors = Predictor_AD%TraceGas_Predictors &
                     + TraceGas_Predictors_AD
      Predictor_AD%Predictor_Subset(4:7, :, :) = Predictor_AD%Predictor_Subset(4:7, :, :) & 
                     +  Predictor_Subset_AD(1:4, :, :) 
    ENDIF

    IF (IH2O > 0) THEN					     
  !    Calc OPTRAN water 					     
       CALL Compute_WOPTRAN_Optics_AD(Sensor_Index, IH2O, Predictor, KW_AD, Predictor_AD )		     
    ENDIF
    
    NULLIFY(TC)

  END SUBROUTINE Compute_Optical_Depth_Subset_AD 
  
  
  
  SUBROUTINE  Compute_WOPTRAN_Optics( Sensor_Index,       & ! Input
                                      H2O_Channel_Index,  & ! Input 
                                      Predictor,	  & ! Input        
				      H2O_Optical_Depth )   ! Output       
				     				           
    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    INTEGER,  INTENT( IN )     ::   Sensor_Index 
    INTEGER,  INTENT( IN )     ::   H2O_Channel_Index 
    TYPE( Predictor_type ), INTENT( IN ) :: Predictor

    ! -- Outputs
    REAL( fp ), DIMENSION(MAX_N_ODCAPS_LAYERS), INTENT( OUT ) :: H2O_Optical_Depth
  
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_WOPTRAN_Optics'

    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER ::	 L, I, J	  
    INTEGER ::   LOP	  
    REAL( fp), DIMENSION(MAX_N_WATER_OPTRAN_LAYERS) :: KWOP
    INTEGER :: idx
    TYPE( ODCAPS_TauCoeff_type ), POINTER  ::  TC => NULL()

    TC => ODCAPS_TC(Sensor_Index)
 
    J = H2O_Channel_Index
    idx = MINLOC(ABS(TC%Sensor_Channel - TC%ODCAPS_ODAS%Channel_Index(J)), DIM=1)
    
    I = idx  !TC%Sensor_Channel( TC%ODCAPS_ODAS%Channel_Index(J) )
    ! ---------------------------------
    ! Loop over the OPTRAN water levels
    ! ---------------------------------
    
    ! Only do calc for OPTRAN levels that are needed
    
    DO LOP = Predictor%Min_OPTRAN_Level, Predictor%Max_OPTRAN_Level
      IF ( Predictor%OPTRAN_Level_Use( LOP ) ) THEN
   	KWOP(LOP) =  TC%Tuning_Multiple(2,I) * &
   	 ( TC%ODCAPS_ODAS%Water_Coeff(1,LOP,J) * Predictor%Optran_Water_Predictors(1,LOP) + &
   	   TC%ODCAPS_ODAS%Water_Coeff(2,LOP,J) * Predictor%Optran_Water_Predictors(2,LOP) + &
   	   TC%ODCAPS_ODAS%Water_Coeff(3,LOP,J) * Predictor%Optran_Water_Predictors(3,LOP) + &
   	   TC%ODCAPS_ODAS%Water_Coeff(4,LOP,J) * Predictor%Optran_Water_Predictors(4,LOP) + &
   	   TC%ODCAPS_ODAS%Water_Coeff(5,LOP,J) * Predictor%Optran_Water_Predictors(5,LOP) + &
   	   TC%ODCAPS_ODAS%Water_Coeff(6,LOP,J) * Predictor%Optran_Water_Predictors(6,LOP) + & 
   	   TC%ODCAPS_ODAS%Water_Coeff(7,LOP,J) * Predictor%Optran_Water_Predictors(7,LOP) + &
   	   TC%ODCAPS_ODAS%Water_Coeff(8,LOP,J) * Predictor%Optran_Water_Predictors(8,LOP) + &
   	   TC%ODCAPS_ODAS%Water_Coeff(9,LOP,J) * Predictor%Optran_Water_Predictors(9,LOP) )
    
    !  Remove WAOP scaling factor
        KWOP(LOP) = KWOP(LOP) / Predictor%Water_OPTRAN_Scaling(LOP)
    
    !  Check for negative value
        IF ( KWOP(LOP) < ZERO ) KWOP(LOP) = ZERO
      ENDIF
    END DO
 
    ! -------------------------
    ! Loop over the AIRS layers
    ! -------------------------
    DO L = 1, Predictor%n_Layers

    !	Interpolate abs coef and convert to optical depth
 	H2O_Optical_Depth( L ) = ( Predictor%OPTRAN_Interp_Frac( L )  &
	             * ( KWOP(Predictor%Lower_OPTRAN_Level( L ) + 1)  &
   	               - KWOP(Predictor%Lower_OPTRAN_Level( L ) ) )   &
	             + KWOP(Predictor%Lower_OPTRAN_Level( L ) ) )     &
	             * Predictor%Layer_Water_Amount( L )

 	IF ( H2O_Optical_Depth( L ) < ZERO ) H2O_Optical_Depth( L ) = ZERO

    ENDDO
  
    NULLIFY(TC)
  END SUBROUTINE Compute_WOPTRAN_Optics 


  SUBROUTINE  Compute_WOPTRAN_Optics_TL( Sensor_Index,       & ! Input
                                         H2O_Channel_Index,  & ! Input 
                                         Predictor,          & ! Input      	 
					 Predictor_TL,       & ! Input      	 
					 H2O_Optical_Depth,  & ! Output     	 
					 H2O_Optical_Depth_TL )   ! Output  	 
					    
    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    INTEGER,  INTENT( IN )     ::   Sensor_Index 
    INTEGER,  INTENT( IN )     ::   H2O_Channel_Index 
    TYPE( Predictor_type ), INTENT( IN ) :: Predictor
    TYPE( Predictor_type ), INTENT( IN ) :: Predictor_TL

    ! -- Outputs
    REAL( fp ), DIMENSION(MAX_N_ODCAPS_LAYERS), INTENT( OUT ) :: H2O_Optical_Depth
    REAL( fp ), DIMENSION(MAX_N_ODCAPS_LAYERS), INTENT( OUT ) :: H2O_Optical_Depth_TL
  
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_WOPTRAN_Optics_TL'

    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER ::	 L, I, J	  
    INTEGER ::   LOP	  
    REAL( fp), DIMENSION(MAX_N_WATER_OPTRAN_LAYERS) ::KWOP, KWOPP, KWOP_TL, KWOPP_TL
    INTEGER :: idx
    TYPE( ODCAPS_TauCoeff_type ), POINTER  ::  TC => NULL()

    TC => ODCAPS_TC(Sensor_Index)

    J = H2O_Channel_Index
    idx = MINLOC(ABS(TC%Sensor_Channel - TC%ODCAPS_ODAS%Channel_Index(J)), DIM=1)

    I = idx  !TC%Sensor_Channel( TC%ODCAPS_ODAS%Channel_Index(J) )
    ! ---------------------------------
    ! Loop over the OPTRAN water levels
    ! ---------------------------------
    
    ! Only do calc for OPTRAN levels that are needed
    
    DO LOP = Predictor%Min_OPTRAN_Level, Predictor%Max_OPTRAN_Level
      IF ( Predictor%OPTRAN_Level_Use( LOP ) ) THEN
   	KWOPP(LOP) =  TC%Tuning_Multiple(2,I) * &
   	 ( TC%ODCAPS_ODAS%Water_Coeff(1,LOP,J) * Predictor%Optran_Water_Predictors(1,LOP) + &
   	   TC%ODCAPS_ODAS%Water_Coeff(2,LOP,J) * Predictor%Optran_Water_Predictors(2,LOP) + &
   	   TC%ODCAPS_ODAS%Water_Coeff(3,LOP,J) * Predictor%Optran_Water_Predictors(3,LOP) + &
   	   TC%ODCAPS_ODAS%Water_Coeff(4,LOP,J) * Predictor%Optran_Water_Predictors(4,LOP) + &
   	   TC%ODCAPS_ODAS%Water_Coeff(5,LOP,J) * Predictor%Optran_Water_Predictors(5,LOP) + &
   	   TC%ODCAPS_ODAS%Water_Coeff(6,LOP,J) * Predictor%Optran_Water_Predictors(6,LOP) + & 
   	   TC%ODCAPS_ODAS%Water_Coeff(7,LOP,J) * Predictor%Optran_Water_Predictors(7,LOP) + &
   	   TC%ODCAPS_ODAS%Water_Coeff(8,LOP,J) * Predictor%Optran_Water_Predictors(8,LOP) + &
   	   TC%ODCAPS_ODAS%Water_Coeff(9,LOP,J) * Predictor%Optran_Water_Predictors(9,LOP) )
    
   	KWOPP_TL(LOP) =  TC%Tuning_Multiple(2,I) * &
   	 ( TC%ODCAPS_ODAS%Water_Coeff(1,LOP,J) * Predictor_TL%Optran_Water_Predictors(1,LOP) + &
   	   TC%ODCAPS_ODAS%Water_Coeff(2,LOP,J) * Predictor_TL%Optran_Water_Predictors(2,LOP) + &
   	   TC%ODCAPS_ODAS%Water_Coeff(3,LOP,J) * Predictor_TL%Optran_Water_Predictors(3,LOP) + &
   	   TC%ODCAPS_ODAS%Water_Coeff(4,LOP,J) * Predictor_TL%Optran_Water_Predictors(4,LOP) + &
   	   TC%ODCAPS_ODAS%Water_Coeff(5,LOP,J) * Predictor_TL%Optran_Water_Predictors(5,LOP) + &
   	   TC%ODCAPS_ODAS%Water_Coeff(6,LOP,J) * Predictor_TL%Optran_Water_Predictors(6,LOP) + & 
   	   TC%ODCAPS_ODAS%Water_Coeff(7,LOP,J) * Predictor_TL%Optran_Water_Predictors(7,LOP) + &
   	   TC%ODCAPS_ODAS%Water_Coeff(8,LOP,J) * Predictor_TL%Optran_Water_Predictors(8,LOP) + &
   	   TC%ODCAPS_ODAS%Water_Coeff(9,LOP,J) * Predictor_TL%Optran_Water_Predictors(9,LOP) )

    !  Remove WAOP scaling factor
        KWOP(LOP) = KWOPP(LOP) / Predictor%Water_OPTRAN_Scaling(LOP)

        KWOP_TL(LOP) = KWOPP_TL(LOP) / Predictor%Water_OPTRAN_Scaling(LOP)  !&
!	             - KWOPP(LOP) * Predictor_TL%Water_OPTRAN_Scaling(LOP) &
!		       / (Predictor%Water_OPTRAN_Scaling(LOP))**TWO
        
    
    !  Check for negative value
        IF ( KWOP(LOP) < ZERO ) THEN 
	  KWOP(LOP) = ZERO
	  KWOP_TL(LOP) = ZERO
	ENDIF  
      ENDIF
    END DO
 
    ! -------------------------
    ! Loop over the AIRS layers
    ! -------------------------
    DO L = 1, Predictor%n_Layers

    !	Interpolate abs coef and convert to optical depth
 	H2O_Optical_Depth( L ) = ( Predictor%OPTRAN_Interp_Frac( L )  &
	             * ( KWOP(Predictor%Lower_OPTRAN_Level( L ) + 1)  &
   	               - KWOP(Predictor%Lower_OPTRAN_Level( L ) ) )   &
	             + KWOP(Predictor%Lower_OPTRAN_Level( L ) ) )     &
	             * Predictor%Layer_Water_Amount( L )

 	H2O_Optical_Depth_TL( L ) = ( Predictor%OPTRAN_Interp_Frac( L )  &
	              * ( KWOP(Predictor%Lower_OPTRAN_Level( L ) + 1)  &
   	                - KWOP(Predictor%Lower_OPTRAN_Level( L ) ) )   &
	              + KWOP(Predictor%Lower_OPTRAN_Level( L ) ) )     &
	              * Predictor_TL%Layer_Water_Amount( L )  &
		     +  ( Predictor_TL%OPTRAN_Interp_Frac( L )  &
	              * ( KWOP(Predictor%Lower_OPTRAN_Level( L ) + 1)  &
   	                - KWOP(Predictor%Lower_OPTRAN_Level( L ) ) )   &
		      +	  Predictor%OPTRAN_Interp_Frac( L )  &
	              * ( KWOP_TL(Predictor%Lower_OPTRAN_Level( L ) + 1)  &
   	                - KWOP_TL(Predictor%Lower_OPTRAN_Level( L ) ) )   &
	              + KWOP_TL(Predictor%Lower_OPTRAN_Level( L ) ) )     &
	              * Predictor%Layer_Water_Amount( L )  
 
 	IF ( H2O_Optical_Depth( L ) < ZERO ) THEN
	  H2O_Optical_Depth( L ) = ZERO
	  H2O_Optical_Depth_TL( L ) = ZERO
        ENDIF
    ENDDO
    
    NULLIFY(TC)
  END SUBROUTINE Compute_WOPTRAN_Optics_TL 


  SUBROUTINE  Compute_WOPTRAN_Optics_AD( Sensor_Index,          & ! Input
                                         H2O_Channel_Index,     & ! Input 
                                         Predictor, 	        & ! Input    	    
                                         H2O_Optical_Depth_AD,  & ! Input    							
  					 Predictor_AD )	          ! In/Output	    
					    
    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    INTEGER,  INTENT( IN )     ::   Sensor_Index 
    INTEGER,  INTENT( IN )     ::   H2O_Channel_Index 
    TYPE( Predictor_type ), INTENT( IN ) :: Predictor
    REAL( fp ), DIMENSION(MAX_N_ODCAPS_LAYERS), INTENT( IN ) :: H2O_Optical_Depth_AD
 
    ! -- In/Outputs
    TYPE( Predictor_type ), INTENT( IN OUT ) :: Predictor_AD
  
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_WOPTRAN_Optics_AD'

    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER ::	 L, I, J	  
    INTEGER ::   LOP	  
    REAL( fp), DIMENSION(MAX_N_WATER_OPTRAN_LAYERS) :: KWOP, KWOPP, KWOP_AD, KWOPP_AD
    REAL( fp ), DIMENSION(MAX_N_ODCAPS_LAYERS):: H2O_Optical_Depth
    INTEGER :: idx
    TYPE( ODCAPS_TauCoeff_type ), POINTER  ::  TC => NULL()

    TC => ODCAPS_TC(Sensor_Index)
 
    J = H2O_Channel_Index
    idx = MINLOC(ABS(TC%Sensor_Channel - TC%ODCAPS_ODAS%Channel_Index(J)), DIM=1)

    I = idx !TC%Sensor_Channel( TC%ODCAPS_ODAS%Channel_Index(J) )

    ! Forward model
    ! ---------------------------------
    ! Loop over the OPTRAN water levels
    ! ---------------------------------

    ! Only do calc for OPTRAN levels that are needed
    
    DO LOP = Predictor%Min_OPTRAN_Level, Predictor%Max_OPTRAN_Level
      IF ( Predictor%OPTRAN_Level_Use( LOP ) ) THEN
   	KWOPP(LOP) =  TC%Tuning_Multiple(2,I) * &
   	 ( TC%ODCAPS_ODAS%Water_Coeff(1,LOP,J) * Predictor%Optran_Water_Predictors(1,LOP) + &
   	   TC%ODCAPS_ODAS%Water_Coeff(2,LOP,J) * Predictor%Optran_Water_Predictors(2,LOP) + &
   	   TC%ODCAPS_ODAS%Water_Coeff(3,LOP,J) * Predictor%Optran_Water_Predictors(3,LOP) + &
   	   TC%ODCAPS_ODAS%Water_Coeff(4,LOP,J) * Predictor%Optran_Water_Predictors(4,LOP) + &
   	   TC%ODCAPS_ODAS%Water_Coeff(5,LOP,J) * Predictor%Optran_Water_Predictors(5,LOP) + &
   	   TC%ODCAPS_ODAS%Water_Coeff(6,LOP,J) * Predictor%Optran_Water_Predictors(6,LOP) + & 
   	   TC%ODCAPS_ODAS%Water_Coeff(7,LOP,J) * Predictor%Optran_Water_Predictors(7,LOP) + &
   	   TC%ODCAPS_ODAS%Water_Coeff(8,LOP,J) * Predictor%Optran_Water_Predictors(8,LOP) + &
   	   TC%ODCAPS_ODAS%Water_Coeff(9,LOP,J) * Predictor%Optran_Water_Predictors(9,LOP) )
    
    !  Remove WAOP scaling factor
        KWOP(LOP) = KWOPP(LOP) / Predictor%Water_OPTRAN_Scaling(LOP)
    
    !  Check for negative value
        IF ( KWOP(LOP) < ZERO ) KWOP(LOP) = ZERO
      ENDIF
    END DO
 
    ! -------------------------
    ! Loop over the AIRS layers
    ! -------------------------
    DO L = 1, Predictor%n_Layers

    !	Interpolate abs coef and convert to optical depth
 	H2O_Optical_Depth( L ) = ( Predictor%OPTRAN_Interp_Frac( L )  &
	             * ( KWOP(Predictor%Lower_OPTRAN_Level( L ) + 1)  &
   	               - KWOP(Predictor%Lower_OPTRAN_Level( L ) ) )   &
	             + KWOP(Predictor%Lower_OPTRAN_Level( L ) ) )     &
	             * Predictor%Layer_Water_Amount( L )

 	IF ( H2O_Optical_Depth( L ) < ZERO ) H2O_Optical_Depth( L ) = ZERO

    ENDDO
   
    ! Adjoint model
    KWOP_AD = ZERO
    KWOPP_AD = ZERO
    ! -------------------------
    ! Loop over the AIRS layers
    ! -------------------------
    DO L = 1, Predictor%n_Layers
        
        Predictor_AD%Layer_Water_Amount( L ) = Predictor_AD%Layer_Water_Amount( L ) + &
	                ( Predictor%OPTRAN_Interp_Frac( L )  &
	              * ( KWOP(Predictor%Lower_OPTRAN_Level( L ) + 1)  &
   	                - KWOP(Predictor%Lower_OPTRAN_Level( L ) ) )   &
	              + KWOP(Predictor%Lower_OPTRAN_Level( L ) ) )     &
	              * H2O_Optical_Depth_AD( L ) 
		       
        Predictor_AD%OPTRAN_Interp_Frac( L ) = Predictor_AD%OPTRAN_Interp_Frac( L ) + &
	       H2O_Optical_Depth_AD( L ) * ( KWOP(Predictor%Lower_OPTRAN_Level( L ) + 1)  &
   	                                   - KWOP(Predictor%Lower_OPTRAN_Level( L ) ) ) &
	     * Predictor%Layer_Water_Amount( L )
	
	     
	KWOP_AD(Predictor%Lower_OPTRAN_Level( L ) + 1) = KWOP_AD(Predictor%Lower_OPTRAN_Level( L ) + 1)&
	        + Predictor%OPTRAN_Interp_Frac( L ) &
	        * Predictor%Layer_Water_Amount( L ) * H2O_Optical_Depth_AD( L )
	
		
	KWOP_AD(Predictor%Lower_OPTRAN_Level( L ) ) = KWOP_AD(Predictor%Lower_OPTRAN_Level( L ) ) &
	             + ( ONE -  Predictor%OPTRAN_Interp_Frac( L ) ) &   
                     * Predictor%Layer_Water_Amount( L ) * H2O_Optical_Depth_AD( L )
	
    ENDDO

    ! ---------------------------------
    ! Loop over the OPTRAN water levels
    ! ---------------------------------
    
    ! Only do calc for OPTRAN levels that are needed
    
    DO LOP = Predictor%Min_OPTRAN_Level, Predictor%Max_OPTRAN_Level
      IF ( Predictor%OPTRAN_Level_Use( LOP ) ) THEN

    !  Check for negative value
        IF ( KWOP(LOP) < ZERO ) THEN
 	  KWOP_AD(LOP) = ZERO
	ENDIF  
         
	KWOPP_AD(LOP) = KWOP_AD(LOP) / Predictor%Water_OPTRAN_Scaling(LOP)
	
!	Predictor_AD%Water_OPTRAN_Scaling(LOP) = Predictor_AD%Water_OPTRAN_Scaling(LOP)  &
!	                -  KWOPP(LOP) * KWOP_AD(LOP) &
!		         / (Predictor%Water_OPTRAN_Scaling(LOP))**TWO
	
	
	Predictor_AD%Optran_Water_Predictors(1,LOP) = Predictor_AD%Optran_Water_Predictors(1,LOP) + &
	                 TC%Tuning_Multiple(2,I) * TC%ODCAPS_ODAS%Water_Coeff(1,LOP,J) * KWOPP_AD(LOP)
			 
	Predictor_AD%Optran_Water_Predictors(2,LOP) = Predictor_AD%Optran_Water_Predictors(2,LOP) + &
	                 TC%Tuning_Multiple(2,I) * TC%ODCAPS_ODAS%Water_Coeff(2,LOP,J) * KWOPP_AD(LOP)

	Predictor_AD%Optran_Water_Predictors(3,LOP) = Predictor_AD%Optran_Water_Predictors(3,LOP) + &
	                 TC%Tuning_Multiple(2,I) * TC%ODCAPS_ODAS%Water_Coeff(3,LOP,J) * KWOPP_AD(LOP)

	Predictor_AD%Optran_Water_Predictors(4,LOP) = Predictor_AD%Optran_Water_Predictors(4,LOP) + &
	                 TC%Tuning_Multiple(2,I) * TC%ODCAPS_ODAS%Water_Coeff(4,LOP,J) * KWOPP_AD(LOP)

	Predictor_AD%Optran_Water_Predictors(5,LOP) = Predictor_AD%Optran_Water_Predictors(5,LOP) + &
	                 TC%Tuning_Multiple(2,I) * TC%ODCAPS_ODAS%Water_Coeff(5,LOP,J) * KWOPP_AD(LOP)

	Predictor_AD%Optran_Water_Predictors(6,LOP) = Predictor_AD%Optran_Water_Predictors(6,LOP) + &
	                 TC%Tuning_Multiple(2,I) * TC%ODCAPS_ODAS%Water_Coeff(6,LOP,J) * KWOPP_AD(LOP)
 
 	Predictor_AD%Optran_Water_Predictors(7,LOP) = Predictor_AD%Optran_Water_Predictors(7,LOP) + &
	                 TC%Tuning_Multiple(2,I) * TC%ODCAPS_ODAS%Water_Coeff(7,LOP,J) * KWOPP_AD(LOP)

 	Predictor_AD%Optran_Water_Predictors(8,LOP) = Predictor_AD%Optran_Water_Predictors(8,LOP) + &
	                 TC%Tuning_Multiple(2,I) * TC%ODCAPS_ODAS%Water_Coeff(8,LOP,J) * KWOPP_AD(LOP)
      
 	Predictor_AD%Optran_Water_Predictors(9,LOP) = Predictor_AD%Optran_Water_Predictors(9,LOP) + &
	                 TC%Tuning_Multiple(2,I) * TC%ODCAPS_ODAS%Water_Coeff(9,LOP,J) * KWOPP_AD(LOP)
	
	KWOP_AD(LOP) = ZERO
	KWOPP_AD(LOP) = ZERO		 
      ENDIF
      
    END DO

    NULLIFY(TC)
  END SUBROUTINE Compute_WOPTRAN_Optics_AD 

  !-------------------------------------------------------------
  !  This SUBROUTINE interpolates ODCAPS optical depth profile on
  !  user pressure grids.
  !-------------------------------------------------------------

  SUBROUTINE ToUserOpticalDepth(Predictor, AtmAbsorption)

    ! Inputs
    TYPE(Predictor_type),       INTENT( IN )          :: Predictor

    ! In/Outputs
    TYPE( CRTM_AtmAbsorption_type), INTENT( IN OUT )  :: AtmAbsorption 

    !-- local
    REAL(fp),DIMENSION(0:Predictor%n_Layers)      :: path_ODCAPS
    REAL(fp),DIMENSION(0:Predictor%n_User_Layers)       :: path_usr
    INTEGER       :: n_Layers_ODCAPS, n_Layers_usr, k

    n_Layers_ODCAPS = Predictor%n_Layers
    n_Layers_usr   = Predictor%n_User_Layers
    
    path_ODCAPS(0) = ZERO
    
    path_ODCAPS(1 : n_Layers_ODCAPS) = Predictor%LTS_Optical_Depth( 1 : n_Layers_ODCAPS )
     
    CALL Interpolate_Profile(path_ODCAPS, &
                Predictor%Level_pressure(0:n_Layers_ODCAPS), &
                Predictor%User_Level_Pressure(0:n_Layers_usr), &
                path_usr)   

    DO k = 1, n_Layers_usr
       AtmAbsorption%Optical_Depth(k) = path_usr(k) - path_usr(k-1)
    ENDDO
!   Change to the same as OPTRAN algorithm 08/17/06
    AtmAbsorption%Optical_Depth = AtmAbsorption%Optical_Depth / Predictor%Secant_Sensor_Zenith(1)
     
  END SUBROUTINE ToUserOpticalDepth

  !-------------------------------------------------------------
  !  Tangent-linear of the SUBROUTINE ToUserOpticalDepth. 
  !-------------------------------------------------------------

  SUBROUTINE ToUserOpticalDepth_TL(Predictor, Predictor_TL, AtmAbsorption_TL)

    ! Inputs
    TYPE(Predictor_type),          INTENT( IN )          :: Predictor
    TYPE(Predictor_type),          INTENT( IN )          :: Predictor_TL

    ! In/Outputs
    TYPE( CRTM_AtmAbsorption_type), INTENT( IN OUT )  :: AtmAbsorption_TL 

    !-- local
    REAL(fp),DIMENSION(0:Predictor%n_Layers) :: path_ODCAPS, path_ODCAPS_TL
    REAL(fp),DIMENSION(0:Predictor%n_User_Layers)  :: path_usr, path_usr_TL
    INTEGER       :: n_Layers_ODCAPS, n_Layers_usr, k

    n_Layers_ODCAPS = Predictor%n_Layers
    n_Layers_usr   = Predictor%n_User_Layers

    path_ODCAPS(0) = ZERO
    path_ODCAPS_TL(0) = ZERO

    path_ODCAPS(1 : n_Layers_ODCAPS) = Predictor%LTS_Optical_Depth( 1 : n_Layers_ODCAPS )
    path_ODCAPS_TL(1 : n_Layers_ODCAPS) = Predictor_TL%LTS_Optical_Depth( 1 : n_Layers_ODCAPS )
    
    CALL Interpolate_Profile_TL(path_ODCAPS, &
                   Predictor%Level_pressure(0:n_Layers_ODCAPS), &
                   Predictor%User_Level_Pressure(0:n_Layers_usr), &
                   path_ODCAPS_TL, &
                   path_usr_TL)   
    
    DO k = 1, n_Layers_usr
      AtmAbsorption_TL%Optical_Depth(k) = path_usr_TL(k) - path_usr_TL(k-1)
    ENDDO
!   Change to the same as OPTRAN algorithm 08/17/06
    AtmAbsorption_TL%Optical_Depth = AtmAbsorption_TL%Optical_Depth / Predictor%Secant_Sensor_Zenith(1)
    
  END SUBROUTINE ToUserOpticalDepth_TL

  !-------------------------------------------------------------
  !  Adjoint of the SUBROUTINE ToUserOpticalDepth_TL. 
  !-------------------------------------------------------------

  SUBROUTINE ToUserOpticalDepth_AD(Predictor, AtmAbsorption_AD, Predictor_AD) 

    ! Inputs
    TYPE(Predictor_type),          INTENT( IN )          :: Predictor

    ! In/Outputs
    TYPE(CRTM_AtmAbsorption_type), INTENT( IN OUT )      :: AtmAbsorption_AD
    TYPE(Predictor_type),          INTENT( IN OUT )      :: Predictor_AD


    !-- local
    REAL(fp),DIMENSION(0:Predictor%n_Layers)     :: path_ODCAPS, path_ODCAPS_AD
    REAL(fp),DIMENSION(0:Predictor%n_User_Layers)      :: path_usr, path_usr_AD
    INTEGER       :: n_Layers_ODCAPS, n_Layers_usr, k

    n_Layers_ODCAPS = Predictor%n_Layers
    n_Layers_usr   = Predictor%n_User_Layers 

    path_ODCAPS_AD = ZERO
    path_usr_AD   = ZERO

!   Change to the same as OPTRAN algorithm 08/17/06
    AtmAbsorption_AD%Optical_Depth = AtmAbsorption_AD%Optical_Depth / Predictor%Secant_Sensor_Zenith(1)
    
    DO k = n_Layers_usr, 1, -1

      path_usr_AD(k) = path_usr_AD(k) + AtmAbsorption_AD%Optical_Depth(k)
      path_usr_AD(k-1) = path_usr_AD(k-1) - AtmAbsorption_AD%Optical_Depth(k)
      AtmAbsorption_AD%Optical_Depth(k) = ZERO

    ENDDO

    path_ODCAPS(0) = ZERO

    path_ODCAPS(1 : n_Layers_ODCAPS) = Predictor%LTS_Optical_Depth( 1 : n_Layers_ODCAPS )
    
    CALL Interpolate_Profile_AD(path_ODCAPS, &
                   Predictor%Level_Pressure(0:n_Layers_ODCAPS), &
                   Predictor%User_Level_Pressure(0:n_Layers_usr), &
                   path_usr_AD, &
                   path_ODCAPS_AD)
    
    DO k = n_Layers_ODCAPS, 1, -1
      Predictor_AD%LTS_Optical_Depth(k) = &
              Predictor_AD%LTS_Optical_Depth(k) + path_ODCAPS_AD(k) 
      path_ODCAPS_AD(k) = ZERO
    ENDDO
    
    path_ODCAPS_AD(0) = ZERO

  END SUBROUTINE ToUserOpticalDepth_AD

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
!       SUBROUTINE to calculate the layer optical depths due to gaseous
!       absorption for a given input atmospheric profile for a single
!       channel.
!
! CALLING SEQUENCE:
!       CALL Compute_AtmAbsorption ( Sensor_Index,             &  ! Input 
!                                    Channel_Index,            &  ! Input, scalar                  
!                                    Predictor ,               &  ! Input                        
!                                    AtmAbsorption,            &  ! In/Output 
!                                    AAVariables    )  ! Internal variable output     
! INPUT ARGUMENTS:
!       Sensor_Index:    Sensor index id. This is a unique index associated
!                        with a (supported) algorithm ID used to determine the 
!                        algorithm.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Channel_Index:   Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data.
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
! OUTPUT ARGUMENTS:
!        AtmAbsorption:  Upon OUTPUT, this structure contains valid optical
!                        depth profile data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_AtmAbsorption_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!       AAVariables:     Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the ODCAPS_AtmAbsorption module.
!                        UNITS:      N/A
!                        TYPE:       TYPE(AAVariables_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT)
!
! COMMENTS:
!       Note the INTENT on the AtmAbsorption argument is IN OUT rather
!       than just OUT. This is necessary because the argument is defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------
  SUBROUTINE Compute_AtmAbsorption(Sensor_Index,  &  ! Input
                                   Channel_Index, &  ! Input, scalar        
                                   Predictor,     &  ! Input                            
                                   AtmAbsorption, &  ! In/Output          
                                   AAV            )  ! Internal variable output     

    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    INTEGER,                         INTENT( IN )     :: Sensor_Index
    INTEGER,                         INTENT( IN )     :: Channel_Index
 
    ! -- In/Outputs
    TYPE( Predictor_type)          , INTENT( IN OUT ) :: Predictor
    TYPE( CRTM_AtmAbsorption_type ), INTENT( IN OUT ) :: AtmAbsorption    
    TYPE(AAVariables_type)         , INTENT( OUT )    :: AAV

    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_AtmAbsorption'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i       ! Channel subet index
    INTEGER :: j       ! Channel Subset Position 
    INTEGER :: INONLTE ! Channel index for Non_LTE  

    INTEGER :: Do_Sun_Calc 
    TYPE( ODCAPS_TauCoeff_type ), POINTER  ::  TC => NULL()

    TC => ODCAPS_TC(Sensor_Index)
 
 
    !#--------------------------------------------------------------------------#
    !#                -- CALCULATE THE LAYER OPTICAL DEPTHS --                  #
    !#--------------------------------------------------------------------------#

    ! ----------------------------------------
    ! the channel subset index, subset position, and do solar calc 
    ! ----------------------------------------
 
    i = TC%Channel_Subset_Index( Channel_Index )
    j = TC%Channel_Subset_Position( Channel_Index )
!    write(65, '(3I5)') Channel_Index, i, j
    
    INONLTE = TC%Channel_NON_LTE( Channel_Index )
    Do_Sun_Calc = 1

    ! ----------------------------------------
    ! Calculate the layer optical depths  
    ! ----------------------------------------
    CALL Compute_Optical_Depth_Subset(Sensor_Index, i, j, Predictor )
   
    ! No sun; set the sun surface-to-space trans to zero, default
    ! Predictor%Surf_To_Space_Optical_Depth = MAX_LIMIT

    ! For do solar calculation and subset 4 - 7   
    IF ( i > 3 .AND. Predictor%Calc_Sun_Angle_Secant ) THEN
      
      CALL Compute_Optical_Depth_Subset(Sensor_Index, i, j, Predictor, Do_Sun_Calc )
    
    ELSE IF ( i < 4 .AND. Predictor%Calc_Sun_Angle_Secant ) THEN
    
      Predictor%Surf_To_Space_Optical_Depth = &
        Predictor%LTS_Optical_Depth( Predictor%n_Layers ) &			          
	* Predictor%Secant_Source_Zenith(Predictor%n_Layers) &			          
	* Predictor%Sun_Fudge &
	/ Predictor%Secant_Sensor_Zenith( Predictor%n_Layers )    

    ENDIF
    
    ! Adjust a LTE atmospheric radiance for a non-LTE upper atmosphere
    IF ( Predictor%Calc_Sun_Angle_Secant .AND. INONLTE > 0) THEN
      
         Predictor%R_non_LTE =TC%Tuning_Multiple(7,Channel_Index) &
	           * (TC%Non_LTE_Coeff( 1, INONLTE ) * Predictor%Non_LTE_Predictors( 1 ) &
	           + TC%Non_LTE_Coeff( 2, INONLTE ) * Predictor%Non_LTE_Predictors( 2 ) &
	           + TC%Non_LTE_Coeff( 3, INONLTE ) * Predictor%Non_LTE_Predictors( 3 ) &
	           + TC%Non_LTE_Coeff( 4, INONLTE ) * Predictor%Non_LTE_Predictors( 4 ) &
	           + TC%Non_LTE_Coeff( 5, INONLTE ) * Predictor%Non_LTE_Predictors( 5 ) &
	           + TC%Non_LTE_Coeff( 6, INONLTE ) * Predictor%Non_LTE_Predictors( 6 ) )  
    
    ELSE

      Predictor%R_non_LTE = ZERO

    ENDIF

!    write(64,'(I5, 4x, E14.8)') Channel_Index, exp(-Predictor%LTS_Optical_Depth( Predictor%n_Layers )) 
    !#--------------------------------------------------------------------------#
    !#     -- Interpolate optical depth profile on user pressure grids --       #
    !#--------------------------------------------------------------------------#

    CALL ToUserOpticalDepth(Predictor, AtmAbsorption)
    
    NULLIFY(TC)
   
  END SUBROUTINE Compute_AtmAbsorption
  
!------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_AtmAbsorption_TL
!
! PURPOSE:
!       SUBROUTINE to calculate the tangent-linear layer optical depths due
!       to gaseous absorption for a given input atmospheric profile for a
!       single channel.
!
! CALLING SEQUENCE:
!       CALL Compute_AtmAbsorption_TL( Sensor_Index,             &  ! Input, scalar
!                                      Channel_Index,            &  ! Input                             
!                                      Predictor,                &  ! FWD Input                  
!                                      Predictor_TL,             &  ! TL Input                   
!                                      AtmAbsorption_TL,         &  ! In/Output 
!                                      AAVariables       )  ! Internal variable input    
! INPUT ARGUMENTS:
!       Sensor_Index:       Sensor index id. This is a unique index associated
!                           with a (supported) algorithm ID used to determine the 
!                           algorithm.
!                           UNITS:	N/A
!                           TYPE:	INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Channel_Index:      Channel index id. This is a unique index associated
!                           with a (supported) sensor channel used to access
!                           the shared coefficient data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Predictor:          Structure containing the integrated absorber and
!                           predictor profile data.
!                           UNITS:      N/A
!                           TYPE:       TYPE(Predictor_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Predictor_TL:       Structure containing the tangent-linear integrated
!                           absorber and predictor profile data.
!                           UNITS:      N/A
!                           TYPE:       TYPE(Predictor_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       AAVariables:        Structure containing internal variables required for
!                           subsequent tangent-linear or adjoint model calls.
!                           The contents of this structure are NOT accessible
!                           outside of the ODCAPS_AtmAbsorption module.
!                           UNITS:      N/A
!                           TYPE:       TYPE(AAVariables_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT)
!
! OUTPUT ARGUMENTS:
!        AtmAbsorption_TL:  AtmAbsorption structure containing the
!                           tangent-linear gaseous absorption data.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_AtmAbsorption_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the output AtmAbsorption_TL argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------
  SUBROUTINE Compute_AtmAbsorption_TL( Sensor_Index,         &  ! Input
                                       Channel_Index,        &  ! Input, scalar     
                                       Predictor,            &  ! Input, scalar     
                                       Predictor_TL,         &  ! Input, scalar                   
                                       AtmAbsorption_TL,     &  ! In/Output         
                                       AAV               )  ! Internal variable input     
    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    INTEGER,                         INTENT( IN )     :: Sensor_Index
    INTEGER,                         INTENT( IN )     :: Channel_Index
    TYPE( Predictor_type ),          INTENT( IN )     :: Predictor
    TYPE( Predictor_type ),          INTENT(IN OUT)   :: Predictor_TL
    TYPE( CRTM_AtmAbsorption_type ), INTENT(IN OUT)   :: AtmAbsorption_TL
    TYPE(AAVariables_type)         , INTENT( IN )     :: AAV

  
    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_AtmAbsorption_TL'

    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i       ! Channel subet index
    INTEGER :: j       ! Channel Subset Position 
    INTEGER :: k       ! Layer index
    INTEGER :: l       ! Predictor index
    INTEGER :: INONLTE ! Channel index for Non_LTE  

    INTEGER :: Do_Sun_Calc 
    TYPE( ODCAPS_TauCoeff_type ), POINTER  ::  TC => NULL()

    TC => ODCAPS_TC(Sensor_Index)
 
    !#--------------------------------------------------------------------------#
    !#                -- CALCULATE THE TL OF LAYER OPTICAL DEPTHS --                  #
    !#--------------------------------------------------------------------------#

    ! ----------------------------------------
    ! the channel subset index, subset position, and do solar calc 
    ! ----------------------------------------
 
    i = TC%Channel_Subset_Index( Channel_Index )
    j = TC%Channel_Subset_Position( Channel_Index )
    INONLTE = TC%Channel_NON_LTE( Channel_Index )
    Do_Sun_Calc = 1

    ! ----------------------------------------
    ! Calculate the layer optical depths  
    ! ----------------------------------------
    CALL Compute_Optical_Depth_Subset_TL(Sensor_Index, i, j, Predictor,Predictor_TL  )
   
    ! No sun; set the sun surface-to-space trans to zero, default
    ! Predictor%Surf_To_Space_Optical_Depth = MAX_LIMIT

    ! For do solar calculation and subset 4 - 7   
    IF ( i > 3 .AND. Predictor%Calc_Sun_Angle_Secant ) THEN
      
      CALL Compute_Optical_Depth_Subset_TL(Sensor_Index, i, j, Predictor, Predictor_TL, Do_Sun_Calc )
    
    ELSE IF ( i < 4 .AND. Predictor%Calc_Sun_Angle_Secant ) THEN
      Predictor_TL%Surf_To_Space_Optical_Depth = &
        Predictor_TL%LTS_Optical_Depth( Predictor%n_Layers ) &			          
	* Predictor%Secant_Source_Zenith(Predictor%n_Layers) &			          
	* Predictor%Sun_Fudge &
	/ Predictor%Secant_Sensor_Zenith( Predictor%n_Layers )    

    ENDIF
    
    ! Adjust a LTE atmospheric radiance for a non-LTE upper atmosphere
    IF ( Predictor%Calc_Sun_Angle_Secant .AND. INONLTE > 0) THEN
	 Predictor_TL%R_non_LTE =TC%Tuning_Multiple(7,Channel_Index) &
	              * (TC%Non_LTE_Coeff( 1, INONLTE ) * Predictor_TL%Non_LTE_Predictors( 1 ) &
	              + TC%Non_LTE_Coeff( 2, INONLTE ) * Predictor_TL%Non_LTE_Predictors( 2 ) &
	              + TC%Non_LTE_Coeff( 3, INONLTE ) * Predictor_TL%Non_LTE_Predictors( 3 ) &
	              + TC%Non_LTE_Coeff( 4, INONLTE ) * Predictor_TL%Non_LTE_Predictors( 4 ) &
	              + TC%Non_LTE_Coeff( 5, INONLTE ) * Predictor_TL%Non_LTE_Predictors( 5 ) &
	              + TC%Non_LTE_Coeff( 6, INONLTE ) * Predictor_TL%Non_LTE_Predictors( 6 ) )  
    
    ELSE

       Predictor_TL%R_non_LTE = ZERO

    ENDIF

    !#--------------------------------------------------------------------------#
    !#-- TL of converting optical depth profile on user pressure grids --      #
    !#--------------------------------------------------------------------------#

    CALL ToUserOpticalDepth_TL(Predictor, Predictor_TL, AtmAbsorption_TL)

    NULLIFY(TC)

  END SUBROUTINE Compute_AtmAbsorption_TL

!--------------------------------------------------------------------------------
!
! NAME:
!       Compute_AtmAbsorption_AD
!
! PURPOSE:
!       SUBROUTINE to calculate the layer optical depths adjoint due
!       to gaseous absorption for a given input atmospheric profile for a
!       single channel.
!
! CALLING SEQUENCE:
!       CALL Compute_AtmAbsorption_AD(       Sensor_Index,             &  ! Input, scalar
!                                            Channel_Index,            &  ! Input                        
!                                            Predictor,                &  ! Input                        
!                                            AtmAbsorption_AD,         &  ! AD  Input                   
!                                            Predictor_AD,             &   ! In/Output                    
!                                            AAVariables       )  ! Internal variable input     
! INPUT ARGUMENTS:
!       Sensor_Index:       Sensor index id. This is a unique index associated
!                           with a (supported) algorithm ID used to determine the 
!                           algorithm.
!                           UNITS:	N/A
!                           TYPE:	INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Channel_Index:      Channel index id. This is a unique index associated
!                           with a (supported) sensor channel used to access
!                           the shared coefficient data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Predictor:          Structure containing the integrated absorber and
!                           predictor profile data.
!                           UNITS:      N/A
!                           TYPE:       TYPE(Predictor_type)
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
!                           outside of the ODCAPS_AtmAbsorption module.
!                           UNITS:      N/A
!                           TYPE:       TYPE(AAVariables_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT)
!
! OUTPUT ARGUMENTS:
!       Predictor_AD:       Structure containing the adjoint integrated
!                           absorber and predictor profile data.
!                           UNITS:      N/A
!                           TYPE:       TYPE(Predictor_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
! SIDE EFFECTS:
!       Components of the AtmAbsorption_AD structure argument are modified
!       in this SUBROUTINE.
!
!------------------------------------------------------------------------------
  SUBROUTINE Compute_AtmAbsorption_AD( Sensor_Index,     &  ! Input
                                       Channel_Index,    &  ! Input                 
                                       Predictor,        &  ! Input             
                                       AtmAbsorption_AD, &  ! AD  Input                   
                                       Predictor_AD,     &  ! Output             
                                       AAV               )  ! Internal variable input     
    ! Arguments
    ! ---------

    ! -- Inputs
    INTEGER,                         INTENT( IN )     :: Sensor_Index
    INTEGER,                         INTENT( IN )     :: Channel_Index
    TYPE( Predictor_type ),          INTENT( IN )     :: Predictor 
    TYPE(AAVariables_type),          INTENT( IN )     :: AAV
    ! -- In/Outputs
    TYPE( Predictor_type ),          INTENT( IN OUT ) :: Predictor_AD 
    TYPE(CRTM_AtmAbsorption_type),   INTENT( IN OUT ) :: AtmAbsorption_AD
 
    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_AtmAbsorption_AD'

    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i       ! Channel subet index
    INTEGER :: j       ! Channel Subset Position 
    INTEGER :: INONLTE ! Channel index for Non_LTE  
 
    INTEGER :: Do_Sun_Calc 
    
    TYPE( ODCAPS_TauCoeff_type ), POINTER  ::  TC => NULL()

    TC => ODCAPS_TC(Sensor_Index)
 
    !#--------------------------------------------------------------------------#
    !#                -- CALCULATE THE TL OF LAYER OPTICAL DEPTHS --                  #
    !#--------------------------------------------------------------------------#

    ! ----------------------------------------
    ! the channel subset index, subset position, and do solar calc 
    ! ----------------------------------------
 
    i = TC%Channel_Subset_Index( Channel_Index )
    j = TC%Channel_Subset_Position( Channel_Index )
    INONLTE = TC%Channel_NON_LTE(Channel_Index)
    Do_Sun_Calc = 1
    
 
    !#--------------------------------------------------------------------------#
    !# -- Adjoint of converting optical depth profile on user pressure grids -- #
    !#--------------------------------------------------------------------------#
    
    CALL ToUserOpticalDepth_AD(Predictor, AtmAbsorption_AD, Predictor_AD) 

    ! Adjust a LTE atmospheric radiance for a non-LTE upper atmosphere
    IF ( Predictor%Calc_Sun_Angle_Secant .AND. INONLTE > 0) THEN						   
    
       Predictor_AD%Non_LTE_Predictors( 1 ) = Predictor_AD%Non_LTE_Predictors( 1 ) + &  		   
    	  TC%Tuning_Multiple(7,Channel_Index) * TC%Non_LTE_Coeff( 1, INONLTE ) * Predictor_AD%R_non_LTE      
       Predictor_AD%Non_LTE_Predictors( 2 ) = Predictor_AD%Non_LTE_Predictors( 2 ) + &  		   
    	  TC%Tuning_Multiple(7,Channel_Index) * TC%Non_LTE_Coeff( 2, INONLTE ) * Predictor_AD%R_non_LTE      
       Predictor_AD%Non_LTE_Predictors( 3 ) = Predictor_AD%Non_LTE_Predictors( 3 ) + &  		   
    	  TC%Tuning_Multiple(7,Channel_Index) * TC%Non_LTE_Coeff( 3, INONLTE ) * Predictor_AD%R_non_LTE       
       Predictor_AD%Non_LTE_Predictors( 4 ) = Predictor_AD%Non_LTE_Predictors( 4 ) + &  		   
    	  TC%Tuning_Multiple(7,Channel_Index) * TC%Non_LTE_Coeff( 4, INONLTE ) * Predictor_AD%R_non_LTE      
       Predictor_AD%Non_LTE_Predictors( 5 ) = Predictor_AD%Non_LTE_Predictors( 5 ) + &  		   
    	  TC%Tuning_Multiple(7,Channel_Index) * TC%Non_LTE_Coeff( 5, INONLTE ) * Predictor_AD%R_non_LTE       
       Predictor_AD%Non_LTE_Predictors( 6 ) = Predictor_AD%Non_LTE_Predictors( 6 ) + &  		   
    	  TC%Tuning_Multiple(7,Channel_Index) * TC%Non_LTE_Coeff( 6, INONLTE ) * Predictor_AD%R_non_LTE      
    														   
    ENDIF													   

    !#--------------------------------------------------------------------------#
    !#                -- CALCULATE THE ADJOINT OF LAYER OPTICAL DEPTHS --       #
    !#--------------------------------------------------------------------------#
    IF ( i > 3 .AND. Predictor%Calc_Sun_Angle_Secant ) THEN
      
      CALL Compute_Optical_Depth_Subset_AD( Sensor_Index,i, j, Predictor, Predictor_AD, Do_Sun_Calc )
    
    ELSE IF ( i < 4 .AND. Predictor%Calc_Sun_Angle_Secant ) THEN
      
      Predictor_AD%LTS_Optical_Depth( Predictor%n_Layers ) = &
          Predictor_AD%LTS_Optical_Depth( Predictor%n_Layers ) &
	+ Predictor_AD%Surf_To_Space_Optical_Depth &
	* Predictor%Secant_Source_Zenith(Predictor%n_Layers) &			          
	* Predictor%Sun_Fudge &
	/ Predictor%Secant_Sensor_Zenith( Predictor%n_Layers ) 
      
    ENDIF

    ! ----------------------------------------
    ! Calculate the layer optical depths  
    ! ----------------------------------------
    CALL Compute_Optical_Depth_Subset_AD( Sensor_Index, i, j, Predictor, Predictor_AD )
 
    NULLIFY(TC)

  END SUBROUTINE Compute_AtmAbsorption_AD
  

END MODULE ODCAPS_AtmAbsorption
