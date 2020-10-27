!------------------------------------------------------------------------------
!M+
! NAME:
!       ODCAPS_Predictor
!
! PURPOSE:
!       Module continaing routines to compute the Optical Depth Combining Absorber
!       and Pressure Space (ODCAPS) predictors for the gas absorption model.
!
! CATEGORY:
!       CRTM : Gas Absorption : Predictor
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE ODCAPS_Predictor
!
! MODULES:
!       Type_Kinds:                 Module containing definitions for kinds
!                                   of variable types.
!
!       Message_Handler:              Module to define simple error codes and
!                                   handle error conditions
!                                   USEs: FILE_UTILITY module
!
!       CRTM_Parameters:            Module of parameter definitions for the CRTM.
!                                   USEs: TYPE_KINDS module
!
!       CRTM_GeometryInfo:          Module to convert the surface solar zenith
!                                   angle SZA into the local solar angle 
!                                   at altitude ALT, and convert the AIRS satellite 
!                                   viewing angle into the local path angle.
!                                   USEs: TYPE_KINDS module
!                                         CRTM_Parameters module
!
!
!       ODCAPS_Predictor_Define:  Module defining the ODCAPS_Predictor
!                                   structure and containing routines to 
!                                   manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         Message_Handler module
!
!
! CONTAINS:
!       Private subprograms
!       ------------------
!   Compute_Predictors_Subset:         Subroutine to calculate the gas absorption
!   				       model predictors for subset.
!
!   Compute_Predictors_Subset_TL:      Subroutine to calculate the gas absorption
!   				       model tangent-linear predictors.
!
!   Compute_Predictors_Subset_AD:      Subroutine to calculate the adjoint gas
!   				       absorption model predictors.
!
!   Compute_TraceGas_Predictors:       Subroutine to calculate the trace gas absorption
!   				       model predictors.
!
!   Compute_TraceGas_Predictors_TL:    Subroutine to calculate the trace gas absorption
!   				       model tangent-linear predictors.
!
!   Compute_TraceGas_Predictors_AD:    Subroutine to calculate the adjoint trace gas
!   				       absorption model predictors.
!
!   Compute_WOPTRAN_Predictors:        Subroutine to calculate the water vapor absorption
!   				       model predictors for OPTRAN.
!
!   Compute_WOPTRAN_Predictors_TL:     Subroutine to calculate the water vapor absorption
!   				       model tangent-linear predictors for OPTRAN.
!
!   Compute_WOPTRAN_Predictors_AD:     Subroutine to calculate the adjoint water vapor
!   				       absorption model predictors for OPTRAN.
!
!   Compute_Non_LTE_Predictors:        Subroutine to calculate the water vapor absorption
!   				       model predictors for OPTRAN.
!
!   Compute_Non_LTE_Predictors_TL:     Subroutine to calculate the water vapor absorption
!   				       model tangent-linear predictors for OPTRAN.
!
!   Compute_Non_LTE_Predictors_AD:     Subroutine to calculate the adjoint water vapor
!   				       absorption model predictors for OPTRAN.
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
!M-
!------------------------------------------------------------------------------

MODULE ODCAPS_Predictor


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Message_Handler
  USE CRTM_GeometryInfo

  
  ! -- CRTM modules
  USE CRTM_Parameters
  USE CRTM_Atmosphere_Define, ONLY: CRTM_Atmosphere_type, H2O_ID, O3_ID, &
                                    CO2_ID, SO2_ID, HNO3_ID, N2O_ID, &
				    CH4_ID, CO_ID, &
                                    VOLUME_MIXING_RATIO_UNITS, MASS_MIXING_RATIO_UNITS
  USE ODCAPS_Predictor_Define !, ONLY: Predictor_type      , &   
                              !       Associated_Predictor, &   
                              !       Destroy_Predictor   , &   
                              !       Allocate_Predictor  , &   
                              !       Assign_Predictor    , &   
                              !       Zero_Predictor            
   
  USE ODCAPS_Define,            ONLY: ODCAPS_TauCoeff_type    => ODCAPS_type
  USE ODCAPS_TauCoeff,          ONLY: ODCAPS_TC => TC

  USE CRTM_GeometryInfo_Define

  USE CRTM_Profile_Interpolation, ONLY: Interpolate_Profile,     &
                                        Interpolate_Profile_TL,  &
                                        Interpolate_Profile_AD
  
  USE Units_Conversion
  USE Fundamental_Constants, ONLY : R0 => MOLAR_GAS_CONSTANT,   &
                                    G0 => STANDARD_GRAVITY
 
  USE Profile_Utility_Parameters, ONLY : MW_DRYAIR, MW_H2O
  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  ! -- Everything private by default
  PRIVATE
  ! Predictor data structure definition
  PUBLIC :: Predictor_type

  ! ODCAPS_Predictor structure routines inherited
  ! from the ODCAPS_Predictor_Define module
  PUBLIC :: Associated_Predictor
  PUBLIC :: Destroy_Predictor
  PUBLIC :: Allocate_Predictor
  PUBLIC :: Assign_Predictor
  PUBLIC :: Zero_Predictor 


  ! -- Science routines in this module
  PUBLIC :: Compute_Predictors
  PUBLIC :: Compute_Predictors_TL
  PUBLIC :: Compute_Predictors_AD

  ! Internal variable structure
  PUBLIC :: APVariables_type


  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &

  TYPE :: APVariables_type
    PRIVATE
  END TYPE APVariables_type

  ! -- Constant value
  REAL( fp ), PRIVATE, PARAMETER :: PMULT  = 0.58_fp         ! fudge factor * (0.622=M_H2O/M_AIR)
  REAL( fp ), PRIVATE, PARAMETER :: STDDEN = 2.6867E+19_fp   ! Loschmidt aka standard density
  REAL( fp ), PRIVATE, PARAMETER :: STDTMP = 273.15_fp       ! Standard Temperature
  REAL( fp ), PRIVATE, PARAMETER :: KMOLE  = 6.022045E+26_fp ! 1000 * Avagadro's Number

  REAL( fp ), PRIVATE, PARAMETER :: MAX_SATELLITE_VIEW_ANGLE = 53.0_fp ! max satellite view angle 

CONTAINS
  
   

  SUBROUTINE  Compute_Predictors_Subset( Sensor_Index,           &  ! Input
                                         Set_Index,              &  ! Input
                                         Predictor,          &  ! In/Output
					 Calc_Sun_Angle_Secant)     ! Optional Input

    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    INTEGER,  INTENT( IN )     :: Sensor_Index 
    INTEGER,  INTENT( IN )     :: Set_Index 
    
    ! -- In/Outputs
    TYPE( Predictor_type ), INTENT( IN OUT ) :: Predictor

    ! -- Optional Inputs
    INTEGER,               OPTIONAL, INTENT( IN )     :: Calc_Sun_Angle_Secant 


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Predictors_Subset'


    ! ---------------
    ! Local variables
    ! ---------------
      INTEGER :: L			    
      REAL( fp_kind ) ::    PDP 	    
      REAL( fp_kind ) ::  PNORM 	    
      REAL( fp_kind ) ::     DT 	    
      REAL( fp_kind ) ::     TR 	    
      REAL( fp_kind ) ::     TZ 	    
      REAL( fp_kind ) ::    TRZ 	    
      REAL( fp_kind ) ::    A_W 	    
      REAL( fp_kind ) ::  WZREF 	    
      REAL( fp_kind ) ::     WZ 	    
      REAL( fp_kind ) ::   AZ_W 	    
      REAL( fp_kind ) ::    A_O 	    
      REAL( fp_kind ) ::  XZREF 	    
      REAL( fp_kind ) ::     XZ 	    
      REAL( fp_kind ) ::   XZ_O 	    
      REAL( fp_kind ) ::  OZREF 	    
      REAL( fp_kind ) ::     OZ 	    
      REAL( fp_kind ) ::   AZ_O 	    
      REAL( fp_kind ) ::    TOZ 	    
      REAL( fp_kind ) ::  TAZ_O 	    
      REAL( fp_kind ) ::    A_C 	    
      REAL( fp_kind ) ::     CZ 	    
      REAL( fp_kind ) ::  CZREF 	    
      REAL( fp_kind ) ::   AZ_C 	    
      REAL( fp_kind ) ::    A_M 	    
      REAL( fp_kind ) ::  MZREF 	    
      REAL( fp_kind ) ::     MZ 	    
      REAL( fp_kind ) ::   AZ_M 	    
      REAL( fp_kind ) ::    TMZ 	    
      REAL( fp_kind ) ::  TAZ_M 	    
      REAL( fp_kind ) :: TJUNKS 	    
      REAL( fp_kind ) :: WJUNKA 	    
      REAL( fp_kind ) :: WJUNKR 	    
      REAL( fp_kind ) :: WJUNKS 	    
      REAL( fp_kind ) :: WJUNKZ 	    
      REAL( fp_kind ) :: WJUNK4 	    
      REAL( fp_kind ) :: OJUNKA 	    
      REAL( fp_kind ) :: OJUNKR 	    
      REAL( fp_kind ) :: OJUNKZ 	    
      REAL( fp_kind ) :: OJUNKX 	    
      REAL( fp_kind ) :: CJUNKA 	    
      REAL( fp_kind ) :: CJUNKR 	    
      REAL( fp_kind ) :: CJUNKS 	    
      REAL( fp_kind ) :: CJUNKZ 	    
      REAL( fp_kind ) :: MJUNKA 	    
      REAL( fp_kind ) :: MJUNKR 	    
      REAL( fp_kind ) :: MJUNKZ 
      	    
      REAL( fp_kind ), DIMENSION(MAX_N_ODCAPS_LAYERS) :: SECANG
 
      INTEGER :: H2O_Index		    
      INTEGER :: O3_Index		    
      INTEGER :: CO_Index		    
      INTEGER :: CH4_Index		    
    
      LOGICAL :: Cal_Sun
 
      REAL( fp_kind ), DIMENSION(MAX_N_SUBSET_TOTAL_PREDICTORS, MAX_N_ODCAPS_LAYERS) ::  Predictor_Subset1
      REAL( fp_kind ), DIMENSION(MAX_N_SUBSET_TOTAL_PREDICTORS, MAX_N_ODCAPS_LAYERS) ::  Predictor_Subset2
      REAL( fp_kind ), DIMENSION(MAX_N_SUBSET_TOTAL_PREDICTORS, MAX_N_ODCAPS_LAYERS) ::  Predictor_Subset3
      REAL( fp_kind ), DIMENSION(MAX_N_SUBSET_TOTAL_PREDICTORS, MAX_N_ODCAPS_LAYERS) ::  Predictor_Subset4
      REAL( fp_kind ), DIMENSION(MAX_N_SUBSET_TOTAL_PREDICTORS, MAX_N_ODCAPS_LAYERS) ::  Predictor_Subset5
      REAL( fp_kind ), DIMENSION(MAX_N_SUBSET_TOTAL_PREDICTORS, MAX_N_ODCAPS_LAYERS) ::  Predictor_Subset6
      REAL( fp_kind ), DIMENSION(MAX_N_SUBSET_TOTAL_PREDICTORS, MAX_N_ODCAPS_LAYERS) ::  Predictor_Subset7
      TYPE( ODCAPS_TauCoeff_type ), POINTER  ::  TC => NULL()

      TC => ODCAPS_TC(Sensor_Index)
        
      
 
    !#--------------------------------------------------------------------------#
    !#              -- GET THE Atmosphere GASES INDEX  --              #
    !#--------------------------------------------------------------------------#
       H2O_Index = MINLOC( ABS( Predictor%Absorber_ID - H2O_ID ), DIM = 1 )
       O3_Index  = MINLOC( ABS( Predictor%Absorber_ID - O3_ID ),  DIM = 1 )
       CO_Index  = MINLOC( ABS( Predictor%Absorber_ID - CO_ID ),  DIM = 1 )
       CH4_Index = MINLOC( ABS( Predictor%Absorber_ID - CH4_ID ), DIM = 1 )

    !#--------------------------------------------------------------------------#
    !#                    --INITIALIZE THE SUM TERMS TO ZERO   --               #
    !#--------------------------------------------------------------------------#
       PNORM = ZERO
       TZ    = ZERO
       WZREF = ZERO
       WZ    = ZERO
       XZREF = ZERO
       XZ    = ZERO
       OZREF = ZERO
       OZ    = ZERO
       TOZ   = ZERO
       CZREF = ZERO
       CZ    = ZERO
       MZREF = ZERO
       MZ    = ZERO
       TMZ   = ZERO
       
       Cal_Sun = .TRUE.                                            
    !#--------------------------------------------------------------------------#
    !#                -- CALCULATE THE INTEGRATED PREDICTORS --                 #
    !#--------------------------------------------------------------------------#

    Layer_Loop : DO L = 1, Predictor%n_Layers 

      IF (L == 1) THEN					        
         PDP = TC%Ref_Profile_Data(3,1)*( TC%Ref_Profile_Data(3,2) &    
               -TC%Ref_Profile_Data(3,1))			        
         TRZ   = ZERO
         TAZ_O = ZERO						        
         TAZ_M = ZERO						        
      ELSE							        
         PDP = TC%Ref_Profile_Data(3,L)*( TC%Ref_Profile_Data(3,L) &    
               -TC%Ref_Profile_Data(3,L-1) )			        
         PNORM = PNORM + PDP					        

      !  Note: TRZ, TOZ, and TMZ use layer-above terms  	        
         TZ    = TZ + PDP*TR 					        
         TRZ   = TZ/PNORM						        

         TOZ   = TOZ + PDP*DT*A_O					        
         TAZ_O = TOZ/PNORM					        

         TMZ   = TMZ + PDP*TR*A_M					        
         TAZ_M = TMZ/PNORM					        
      ENDIF							        

      !  Temperature terms					      
         DT = Predictor%Temperature(L) - TC%Ref_Profile_Data(4,L) 				      
         TR = Predictor%Temperature(L) / TC%Ref_Profile_Data(4,L)					      

         IF ( PRESENT (Calc_Sun_Angle_Secant) .AND. & 
	      Predictor%Calc_Sun_Angle_Secant) THEN
 
      !	 Total secant
	   SECANG(L) = Predictor%Secant_Source_Zenith(L)
	   Cal_Sun = .TRUE.
	 ELSE
	   SECANG(L) = Predictor%Secant_Sensor_Zenith(L)
	   Cal_Sun = .FALSE. 
	 END IF
	 
      !  Water terms						      
         A_W   = Predictor%Absorber(L,H2O_Index)/TC%Ref_Profile_Data(6,L)				      
         WZREF = WZREF + PDP*TC%Ref_Profile_Data(6,L)				      
         WZ    = WZ + PDP*Predictor%Absorber(L,H2O_Index)					      
         AZ_W  = WZ/WZREF						      

      !  Ozone terms						      
         A_O   = Predictor%Absorber(L,O3_Index)/TC%Ref_Profile_Data(7,L)				      
         XZREF = XZREF + TC%Ref_Profile_Data(7,L)				      
         XZ    = XZ + Predictor%Absorber(L,O3_Index)					      
         XZ_O  = XZ/XZREF						      
         OZREF = OZREF + PDP*TC%Ref_Profile_Data(7,L)				      
         OZ    = OZ + PDP*Predictor%Absorber(L,O3_Index)					      
         AZ_O  = OZ/OZREF						      

      !  Carbon monoxide terms
         A_C=Predictor%Absorber(L,CO_Index)/TC%Ref_Profile_Data(8,L)     
         CZREF=CZREF + PDP*TC%Ref_Profile_Data(8,L)   
         CZ=CZ + PDP*Predictor%Absorber(L,CO_Index) 	     
         AZ_C=CZ/CZREF		     

      !  Methane terms
         A_M=Predictor%Absorber(L,CH4_Index)/TC%Ref_Profile_Data(9,L) 
         MZREF=MZREF + PDP*TC%Ref_Profile_Data(9,L)  
         MZ=MZ + PDP*Predictor%Absorber(L,CH4_Index) 	     
         AZ_M=MZ/MZREF		     

      !  ----------------
      !   Fixed 
      !  ----------------
         TJUNKS    = TR*TR
      !   FIXMUL(L) = Predictor%Fix_Amount_Multiplier( L )

       !  -----
       !  Ozone
       !  -----
         OJUNKA = SECANG(L) *A_O
         OJUNKR = SQRT( OJUNKA )
         OJUNKZ = OJUNKA/XZ_O
         OJUNKX = SECANG(L)*XZ_O

      !  -----
      !  Water
      !  -----
         WJUNKA = SECANG(L)*A_W
         WJUNKR = SQRT( WJUNKA )
         WJUNKS = WJUNKA*WJUNKA
         WJUNKZ = WJUNKA*A_W/AZ_W
         WJUNK4 = SQRT( WJUNKR )

      !  ---------------
      !  Carbon monoxide for FCOW = set4
      !  ---------------
         CJUNKA=SECANG(L)*A_C   
         CJUNKR=SQRT( CJUNKA )  
         CJUNKS=CJUNKA*CJUNKA   
         CJUNKZ=CJUNKA*A_C/AZ_C 
      
      !  ------- 	       
      !  Methane for FMW = set3 
      !  ------- 	       
         MJUNKA=SECANG(L)*A_M  
         MJUNKR=SQRT(MJUNKA)   
         MJUNKZ=SECANG(L)*AZ_M 

      !  ----------------------
      !  Load up the predictors
      !  ----------------------
         
	 Subset_Select: SELECT CASE ( Set_Index)

	   CASE ( 1 )
         !  ---------------			      
         !  Water continuum (for FWO, FOW, FMW, FCOW)
         !  ---------------
            Predictor_Subset1(1,L) = WJUNKA/TJUNKS
            Predictor_Subset1(2,L) = WJUNKA/TJUNKS * A_W/TJUNKS
            Predictor_Subset1(3,L) = WJUNKA/TR
            Predictor_Subset1(4,L) = WJUNKA/TR *A_W
            Predictor_Subset1(5,L) = WJUNKA/TJUNKS *A_W
            Predictor_Subset1(6,L) = WJUNKA/TJUNKS**2
            Predictor_Subset1(7,L) = WJUNKA
      
         !  ----------------
         !   Fixed (for FWO)
         !  ----------------
            Predictor_Subset1( 8,L) = SECANG(L)
            Predictor_Subset1( 9,L) = SECANG(L)* SECANG(L) 
            Predictor_Subset1(10,L) = SECANG(L)*TR
            Predictor_Subset1(11,L) = SECANG(L)*TJUNKS
            Predictor_Subset1(12,L) = TR
            Predictor_Subset1(13,L) = TJUNKS
            Predictor_Subset1(14,L) = SECANG(L)*TRZ
            Predictor_Subset1(15,L) = SECANG(L)*TRZ/TR 
      
         !  ----------------
         !  Water predictors for FWO = set1  
         !  ----------------
            Predictor_Subset1(16,L) = WJUNKA		 
            Predictor_Subset1(17,L) = WJUNKR		 
            Predictor_Subset1(18,L) = WJUNKZ		 
            Predictor_Subset1(19,L) = WJUNKA*DT 	 
            Predictor_Subset1(20,L) = WJUNKS		 
            Predictor_Subset1(21,L) = WJUNKR*DT 	 
            Predictor_Subset1(22,L) = WJUNK4		 
            Predictor_Subset1(23,L) = WJUNKZ/WJUNKR	 
            Predictor_Subset1(24,L) = WJUNKS*WJUNKA	 
            Predictor_Subset1(25,L) = A_W		 
            Predictor_Subset1(26,L) = WJUNKA*DT*ABS( DT )
 
          !  -----
          !  Ozone predictors for FWO = set1
          !  -----
            Predictor_Subset1(27,L)=OJUNKA    
            Predictor_Subset1(28,L)=OJUNKR    
            Predictor_Subset1(29,L)=OJUNKA*DT	      
            Predictor_Subset1(30,L)=OJUNKA*OJUNKA 
            Predictor_Subset1(31,L)=OJUNKR*DT	      
         
	    IF( .NOT. Cal_Sun) THEN
	      Predictor%Predictor_Subset(1, :, L) = Predictor_Subset1(:, L)
	    ENDIF
	    
	   CASE ( 2 )

         !  ---------------			      
         !  Water continuum (for FWO, FOW, FMW, FCOW)
         !  ---------------
 
            Predictor_Subset2(1,L) = WJUNKA/TJUNKS
            Predictor_Subset2(2,L) = WJUNKA/TJUNKS * A_W/TJUNKS
            Predictor_Subset2(3,L) = WJUNKA/TR
            Predictor_Subset2(4,L) = WJUNKA/TR *A_W
            Predictor_Subset2(5,L) = WJUNKA/TJUNKS *A_W
            Predictor_Subset2(6,L) = WJUNKA/TJUNKS**2
            Predictor_Subset2(7,L) = WJUNKA
      
         !  ----------------
         !   Fixed (for FOW)
         !  ----------------
            Predictor_Subset2( 8,L) = SECANG(L)
            Predictor_Subset2( 9,L) = SECANG(L)* SECANG(L)
            Predictor_Subset2(10,L) = SECANG(L)*TR
            Predictor_Subset2(11,L) = SECANG(L)*TJUNKS
            Predictor_Subset2(12,L) = TR
            Predictor_Subset2(13,L) = TJUNKS
            Predictor_Subset2(14,L) = SECANG(L)*TRZ
            Predictor_Subset2(15,L) = SECANG(L)*TRZ/TR 

         !  -----			     
         !  Ozone predictors for FOW = set2  
         !  -----			     
            Predictor_Subset2(16,L) = OJUNKA
            Predictor_Subset2(17,L) = OJUNKR
            Predictor_Subset2(18,L) = OJUNKA*DT
            Predictor_Subset2(19,L) = OJUNKA*OJUNKA
            Predictor_Subset2(20,L) = OJUNKR*DT
            Predictor_Subset2(21,L) = OJUNKZ*A_O
            Predictor_Subset2(22,L) = OJUNKR*A_O/XZ_O
            Predictor_Subset2(23,L) = OJUNKZ*AZ_O
            Predictor_Subset2(24,L) = OJUNKA*SQRT( OJUNKX )
            Predictor_Subset2(25,L) = OJUNKA*TAZ_O*SECANG(L)

         !  ----------------
         !  Water predictors for FOW = set2  
         !  ----------------
            Predictor_Subset2(26,L) = WJUNKA
            Predictor_Subset2(27,L) = WJUNKR
            Predictor_Subset2(28,L) = WJUNKA*DT
            Predictor_Subset2(29,L) = WJUNKA*OJUNKX
            Predictor_Subset2(30,L) = WJUNKS
            Predictor_Subset2(31,L) = WJUNK4
            Predictor_Subset2(32,L) = WJUNKR*DT
            Predictor_Subset2(33,L) = WJUNKZ
            Predictor_Subset2(34,L) = WJUNKA*WJUNKS
            Predictor_Subset2(35,L) = WJUNKA*OJUNKX*OJUNKX
            Predictor_Subset2(36,L) = WJUNKZ/WJUNKR

	    IF( .NOT. Cal_Sun) THEN
  	      Predictor%Predictor_Subset(2, :, L) = Predictor_Subset2(:, L)
	    ENDIF
	      
	   CASE ( 3 )
         !  ---------------			      
         !  Water continuum (for FWO, FOW, FMW, FCOW)
         !  ---------------

            Predictor_Subset3(1,L) = WJUNKA/TJUNKS
            Predictor_Subset3(2,L) = WJUNKA/TJUNKS * A_W/TJUNKS
            Predictor_Subset3(3,L) = WJUNKA/TR
            Predictor_Subset3(4,L) = WJUNKA/TR *A_W
            Predictor_Subset3(5,L) = WJUNKA/TJUNKS *A_W
            Predictor_Subset3(6,L) = WJUNKA/TJUNKS**2
            Predictor_Subset3(7,L) = WJUNKA
      
         !  ----------------
         !   Fixed (for FMW)
         !  ----------------
            Predictor_Subset3( 8,L) = SECANG(L)
            Predictor_Subset3( 9,L) = SECANG(L)* SECANG(L) 
            Predictor_Subset3(10,L) = SECANG(L)*TR
            Predictor_Subset3(11,L) = SECANG(L)*TJUNKS
            Predictor_Subset3(12,L) = TR
            Predictor_Subset3(13,L) = TJUNKS
            Predictor_Subset3(14,L) = SECANG(L)*TRZ
            Predictor_Subset3(15,L) = SECANG(L)*TRZ/TR 

         !  -------
         !  Methane for FMW = set3
         !  -------
            Predictor_Subset3(16,L) = MJUNKA
            Predictor_Subset3(17,L) = MJUNKR
            Predictor_Subset3(18,L) = MJUNKA*DT
            Predictor_Subset3(19,L) = MJUNKA*MJUNKA
            Predictor_Subset3(20,L) = MJUNKA*SECANG(L)
            Predictor_Subset3(21,L) = MJUNKZ
            Predictor_Subset3(22,L) = A_M*DT
            Predictor_Subset3(23,L) = TAZ_M*SECANG(L)
            Predictor_Subset3(24,L) = SQRT( MJUNKZ )

         !  -------
         !  water predictors for FMW = set3
         !  -------
            Predictor_Subset3(25,L) = WJUNKA
            Predictor_Subset3(26,L) = WJUNKR
            Predictor_Subset3(27,L) = WJUNKZ
            Predictor_Subset3(28,L) = WJUNKA*DT
            Predictor_Subset3(29,L) = WJUNKS
            Predictor_Subset3(30,L) = WJUNKR*DT
            Predictor_Subset3(31,L) = WJUNK4
            Predictor_Subset3(32,L) = WJUNKS*WJUNKA
            Predictor_Subset3(33,L) = A_W
            Predictor_Subset3(34,L) = WJUNKZ/WJUNKR
            Predictor_Subset3(35,L) = WJUNKR*MJUNKZ

	    IF( .NOT. Cal_Sun) THEN
  	      Predictor%Predictor_Subset(3, :, L) = Predictor_Subset3(:, L)
	    ENDIF
	      
	   CASE ( 4 )
         !  ---------------			      
         !  Water continuum (for FWO, FOW, FMW, FCOW)
         !  ---------------

            Predictor_Subset4(1,L) = WJUNKA/TJUNKS
            Predictor_Subset4(2,L) = WJUNKA/TJUNKS * A_W/TJUNKS
            Predictor_Subset4(3,L) = WJUNKA/TR
            Predictor_Subset4(4,L) = WJUNKA/TR *A_W
            Predictor_Subset4(5,L) = WJUNKA/TJUNKS *A_W
            Predictor_Subset4(6,L) = WJUNKA/TJUNKS**2
            Predictor_Subset4(7,L) = WJUNKA

         !  ----------------
         !   Fixed (for FCOW)
         !  ----------------
            Predictor_Subset4( 8,L) = SECANG(L) 
            Predictor_Subset4( 9,L) = SECANG(L) * SECANG(L) 
            Predictor_Subset4(10,L) = SECANG(L) *TR
            Predictor_Subset4(11,L) = SECANG(L) *TJUNKS
            Predictor_Subset4(12,L) = TR
            Predictor_Subset4(13,L) = TJUNKS
            Predictor_Subset4(14,L) = SECANG(L) *TRZ
            Predictor_Subset4(15,L) = SECANG(L) *SECANG(L) *TRZ
            Predictor_Subset4(16,L) = SECANG(L) *SECANG(L)*TR
            Predictor_Subset4(17,L) = SECANG(L) *SECANG(L) *SECANG(L) 
            Predictor_Subset4(18,L) = SQRT(SECANG(L) ) 

         !  ---------------
         !  Carbon monoxide for FCOW = set4
         !  ---------------
            Predictor_Subset4(19,L) = CJUNKA
            Predictor_Subset4(20,L) = CJUNKR
            Predictor_Subset4(21,L) = CJUNKA*DT
            Predictor_Subset4(22,L) = CJUNKS
            Predictor_Subset4(23,L) = CJUNKZ
            Predictor_Subset4(24,L) = CJUNKR*DT
            Predictor_Subset4(25,L) = SQRT( CJUNKR )
            Predictor_Subset4(26,L) = CJUNKZ/CJUNKR
            Predictor_Subset4(27,L) = A_C
            Predictor_Subset4(28,L) = CJUNKA*SECANG(L) 
            Predictor_Subset4(29,L) = CJUNKR*SECANG(L) 
	    
         !  ---------------
         !  ozone predictors for FCOW = set4
         !  ---------------
            Predictor_Subset4(30,L) = OJUNKA
            Predictor_Subset4(31,L) = OJUNKR
            Predictor_Subset4(32,L) = OJUNKA*DT

         !  -------
         !  water predictors for FCOW = set4 
         !  -------
            Predictor_Subset4(33,L) = WJUNKA
            Predictor_Subset4(34,L) = A_W
            Predictor_Subset4(35,L) = WJUNKR
            Predictor_Subset4(36,L) = WJUNKA*DT
            Predictor_Subset4(37,L) = WJUNKS
            Predictor_Subset4(38,L) = WJUNKR*DT
            Predictor_Subset4(39,L) = WJUNK4
            Predictor_Subset4(40,L) = WJUNKZ
            Predictor_Subset4(41,L) = WJUNKA*SECANG(L) 
            Predictor_Subset4(42,L) = WJUNKS*WJUNKA
            Predictor_Subset4(43,L) = WJUNKA*AZ_C*SECANG(L) 
            Predictor_Subset4(44,L) = WJUNKZ/WJUNKR
            Predictor_Subset4(45,L) = WJUNKA*DT*SECANG(L) 
	    
	    IF( .NOT. Cal_Sun) THEN
	      Predictor%Predictor_Subset(4, :, L) = Predictor_Subset4(:, L)
            ELSE
	      Predictor%Predictor_Subset_Sun(1, :, L) = Predictor_Subset4(:, L)
            ENDIF

	   CASE ( 5 )
         !  ---------------			      
         !  Water continuum (for FWO, FOW, FMW, FCOW)
         !  ---------------

            Predictor_Subset5(1,L) = WJUNKA/TJUNKS
            Predictor_Subset5(2,L) = WJUNKA/TJUNKS * A_W/TJUNKS
            Predictor_Subset5(3,L) = WJUNKA/TR
            Predictor_Subset5(4,L) = WJUNKA/TR *A_W
            Predictor_Subset5(5,L) = WJUNKA/TJUNKS *A_W
            Predictor_Subset5(6,L) = WJUNKA/TJUNKS**2
            Predictor_Subset5(7,L) = WJUNKA

         !  ----------------
         !   Fixed for FWO sun bfsw = set5
         !  ----------------
            Predictor_Subset5( 8,L) = SECANG(L) 
            Predictor_Subset5( 9,L) = SECANG(L)*SECANG(L)
            Predictor_Subset5(10,L) = SECANG(L) *TR
            Predictor_Subset5(11,L) = SECANG(L) *TJUNKS
            Predictor_Subset5(12,L) = TR
            Predictor_Subset5(13,L) = TJUNKS
            Predictor_Subset5(14,L) = SECANG(L) *TRZ
            Predictor_Subset5(15,L) = SECANG(L) *TRZ/TR
            Predictor_Subset5(16,L) = SECANG(L) *SECANG(L) *TR
            Predictor_Subset5(17,L) = SQRT(SECANG(L) )
            Predictor_Subset5(18,L) = TRZ

         !  ----------------
         !  Water predictors for FWO sun bfsw = set5
         !  ----------------
            Predictor_Subset5(19,L) = WJUNKA
            Predictor_Subset5(20,L) = WJUNKA*WJUNKR
            Predictor_Subset5(21,L) = WJUNKA*DT

         !  ----------------
         !  ozone predictors for FWO sun bfsw = set5
         !  ----------------
            Predictor_Subset5(22,L) = OJUNKA
	  
	    IF( .NOT. Cal_Sun) THEN
	      Predictor%Predictor_Subset(5, :, L) = Predictor_Subset5(:, L)
	    ELSE
	      Predictor%Predictor_Subset_Sun(2, :, L) = Predictor_Subset5(:, L)
            ENDIF

	   CASE ( 6 )
         !  ---------------			      
         !  Water continuum (for FWO, FOW, FMW, FCOW)
         !  ---------------

            Predictor_Subset6(1,L) = WJUNKA/TJUNKS
            Predictor_Subset6(2,L) = WJUNKA/TJUNKS * A_W/TJUNKS
            Predictor_Subset6(3,L) = WJUNKA/TR
            Predictor_Subset6(4,L) = WJUNKA/TR *A_W
            Predictor_Subset6(5,L) = WJUNKA/TJUNKS *A_W
            Predictor_Subset6(6,L) = WJUNKA/TJUNKS**2
            Predictor_Subset6(7,L) = WJUNKA

         !  ----------------
         !   Fixed for FWO sun mfmw = set6 
         !  ----------------
            Predictor_Subset6( 8,L) = SECANG(L) 
            Predictor_Subset6( 9,L) = SECANG(L) *SECANG(L) 
            Predictor_Subset6(10,L) = SECANG(L) *TR
            Predictor_Subset6(11,L) = SECANG(L) *TJUNKS
            Predictor_Subset6(12,L) = TR
            Predictor_Subset6(13,L) = TJUNKS
            Predictor_Subset6(14,L) = SECANG(L) *TRZ
            Predictor_Subset6(15,L) = SQRT(SECANG(L) )

         !  ----------------
         !  Water predictors for FWO sun mfmw = set6
         !  ----------------
            Predictor_Subset6(16,L) = WJUNKA
            Predictor_Subset6(17,L) = WJUNKA*WJUNKR
            Predictor_Subset6(18,L) = WJUNKA*DT
            Predictor_Subset6(19,L) = WJUNKS
            Predictor_Subset6(20,L) = WJUNKA*WJUNKR*DT
            Predictor_Subset6(21,L) = WJUNKA*WJUNKS
            Predictor_Subset6(22,L) = WJUNKA*SECANG(L) 

         !  ----------------
         !  ozone predictors for FWO sun mfmw = set6 
         !  ----------------
            Predictor_Subset6(23,L) = OJUNKA

	    IF( .NOT. Cal_Sun) THEN
	      Predictor%Predictor_Subset(6, :, L) = Predictor_Subset6(:, L)
            ELSE
	      Predictor%Predictor_Subset_Sun(3, :, L) = Predictor_Subset6(:, L)
	    ENDIF
	        
	   CASE ( 7 )
         !  ---------------			      
         !  Water continuum (for FWO, FOW, FMW, FCOW)
         !  ---------------

            Predictor_Subset7(1,L) = WJUNKA/TJUNKS
            Predictor_Subset7(2,L) = WJUNKA/TJUNKS * A_W/TJUNKS
            Predictor_Subset7(3,L) = WJUNKA/TR
            Predictor_Subset7(4,L) = WJUNKA/TR *A_W
            Predictor_Subset7(5,L) = WJUNKA/TJUNKS *A_W
            Predictor_Subset7(6,L) = WJUNKA/TJUNKS**2
            Predictor_Subset7(7,L) = WJUNKA

         !  ----------------
         !   Fixed for FWO sun mfbw = set7 
         !  ----------------
            Predictor_Subset7( 8,L) = SECANG(L) 
            Predictor_Subset7( 9,L) = SECANG(L) *SECANG(L) 
            Predictor_Subset7(10,L) = SECANG(L) *TR
            Predictor_Subset7(11,L) = SECANG(L) *TJUNKS
            Predictor_Subset7(12,L) = TR
            Predictor_Subset7(13,L) = TJUNKS
            Predictor_Subset7(14,L) = SECANG(L) *TRZ
            Predictor_Subset7(15,L) = SQRT(SECANG(L) )

         !  ----------------
         !  Water predictors for FWO sun mfbw = set7 
         !  ----------------
            Predictor_Subset7(16,L) = WJUNKA
            Predictor_Subset7(17,L) = WJUNKA*WJUNKR
            Predictor_Subset7(18,L) = WJUNKA*DT
            Predictor_Subset7(19,L) = WJUNKS
            Predictor_Subset7(20,L) = WJUNKA*WJUNKR*DT
            Predictor_Subset7(21,L) = WJUNKA*WJUNKS
            Predictor_Subset7(22,L) = WJUNKA*SECANG(L) 
            Predictor_Subset7(23,L) = WJUNKZ
            Predictor_Subset7(24,L) = WJUNKZ*WJUNKR
            Predictor_Subset7(25,L) = WJUNKA*WJUNK4
            Predictor_Subset7(26,L) = WJUNKA*WJUNKZ
            Predictor_Subset7(27,L) = WJUNKA*A_W
            Predictor_Subset7(28,L) = WJUNKS/WJUNK4

         !  ----------------
         !  ozone predictors for FWO sun mfbw = set7 
         !  ----------------
            Predictor_Subset7(29,L) = OJUNKA

	    IF( .NOT. Cal_Sun) THEN
	      Predictor%Predictor_Subset(7, :, L) = Predictor_Subset7(:, L)
            ELSE
	      Predictor%Predictor_Subset_Sun(4, :, L) = Predictor_Subset7(:, L)
	    ENDIF
	     
	 END SELECT Subset_Select
	 
    END DO Layer_Loop

    NULLIFY(TC)
  END SUBROUTINE  Compute_Predictors_Subset

  ! 7 subset tangent-linear predictors for the gas absorption model
  SUBROUTINE  Compute_Predictors_Subset_TL( Sensor_Index,           &  ! Input
                                            Set_Index,              &  ! Input
                                            Predictor,          &  ! Input
					    Predictor_TL,       &  ! In/Output
					 Calc_Sun_Angle_Secant)     ! Optional Input

    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    INTEGER,  INTENT( IN )     :: Set_Index 
    INTEGER,  INTENT( IN )     :: Sensor_Index 

    ! -- Inputs
    TYPE( Predictor_type ), INTENT( IN ) :: Predictor
   
    ! -- In/Outputs
    TYPE( Predictor_type ), INTENT( IN OUT ) :: Predictor_TL

    ! -- Optional Inputs
    INTEGER,               OPTIONAL, INTENT( IN )     :: Calc_Sun_Angle_Secant 


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Predictors_Subset_TL'


    ! ---------------
    ! Local variables
    ! ---------------
      INTEGER :: L			    
      REAL( fp_kind ) ::    PDP,     PDP_TL 	    
      REAL( fp_kind ) ::  PNORM,   PNORM_TL  	   
      REAL( fp_kind ) ::     DT,      DT_TL            
      REAL( fp_kind ) ::     TR,      TR_TL            
      REAL( fp_kind ) ::     TZ,      TZ_TL            
      REAL( fp_kind ) ::    TRZ,     TRZ_TL            
      REAL( fp_kind ) ::    A_W,     A_W_TL            
      REAL( fp_kind ) ::  WZREF,   WZREF_TL            
      REAL( fp_kind ) ::     WZ,      WZ_TL            
      REAL( fp_kind ) ::   AZ_W,    AZ_W_TL            
      REAL( fp_kind ) ::    A_O,     A_O_TL            
      REAL( fp_kind ) ::  XZREF,   XZREF_TL            
      REAL( fp_kind ) ::     XZ,      XZ_TL            
      REAL( fp_kind ) ::   XZ_O,    XZ_O_TL            
      REAL( fp_kind ) ::  OZREF,   OZREF_TL            
      REAL( fp_kind ) ::     OZ,      OZ_TL            
      REAL( fp_kind ) ::   AZ_O,    AZ_O_TL            
      REAL( fp_kind ) ::    TOZ,     TOZ_TL            
      REAL( fp_kind ) ::  TAZ_O,   TAZ_O_TL            
      REAL( fp_kind ) ::    A_C,     A_C_TL            
      REAL( fp_kind ) ::     CZ,      CZ_TL            
      REAL( fp_kind ) ::  CZREF,   CZREF_TL            
      REAL( fp_kind ) ::   AZ_C,    AZ_C_TL            
      REAL( fp_kind ) ::    A_M,     A_M_TL            
      REAL( fp_kind ) ::  MZREF,   MZREF_TL            
      REAL( fp_kind ) ::     MZ,      MZ_TL            
      REAL( fp_kind ) ::   AZ_M,    AZ_M_TL            
      REAL( fp_kind ) ::    TMZ,     TMZ_TL            
      REAL( fp_kind ) ::  TAZ_M,   TAZ_M_TL            
      REAL( fp_kind ) :: TJUNKS,  TJUNKS_TL            
      REAL( fp_kind ) :: WJUNKA,  WJUNKA_TL            
      REAL( fp_kind ) :: WJUNKR,  WJUNKR_TL            
      REAL( fp_kind ) :: WJUNKS,  WJUNKS_TL            
      REAL( fp_kind ) :: WJUNKZ,  WJUNKZ_TL            
      REAL( fp_kind ) :: WJUNK4,  WJUNK4_TL            
      REAL( fp_kind ) :: OJUNKA,  OJUNKA_TL            
      REAL( fp_kind ) :: OJUNKR,  OJUNKR_TL            
      REAL( fp_kind ) :: OJUNKZ,  OJUNKZ_TL            
      REAL( fp_kind ) :: OJUNKX,  OJUNKX_TL            
      REAL( fp_kind ) :: CJUNKA,  CJUNKA_TL            
      REAL( fp_kind ) :: CJUNKR,  CJUNKR_TL            
      REAL( fp_kind ) :: CJUNKS,  CJUNKS_TL            
      REAL( fp_kind ) :: CJUNKZ,  CJUNKZ_TL            
      REAL( fp_kind ) :: MJUNKA,  MJUNKA_TL            
      REAL( fp_kind ) :: MJUNKR,  MJUNKR_TL           
      REAL( fp_kind ) :: MJUNKZ,  MJUNKZ_TL  
      	    
      REAL( fp_kind ), DIMENSION(MAX_N_ODCAPS_LAYERS) :: SECANG, SECANG_TL
 
      INTEGER :: H2O_Index		    
      INTEGER :: O3_Index		    
      INTEGER :: CO_Index		    
      INTEGER :: CH4_Index		    
    
      LOGICAL :: Cal_Sun
 
      REAL( fp_kind ), DIMENSION(MAX_N_SUBSET_TOTAL_PREDICTORS, MAX_N_ODCAPS_LAYERS) ::  Predictor_Subset1_TL
      REAL( fp_kind ), DIMENSION(MAX_N_SUBSET_TOTAL_PREDICTORS, MAX_N_ODCAPS_LAYERS) ::  Predictor_Subset2_TL
      REAL( fp_kind ), DIMENSION(MAX_N_SUBSET_TOTAL_PREDICTORS, MAX_N_ODCAPS_LAYERS) ::  Predictor_Subset3_TL
      REAL( fp_kind ), DIMENSION(MAX_N_SUBSET_TOTAL_PREDICTORS, MAX_N_ODCAPS_LAYERS) ::  Predictor_Subset4_TL
      REAL( fp_kind ), DIMENSION(MAX_N_SUBSET_TOTAL_PREDICTORS, MAX_N_ODCAPS_LAYERS) ::  Predictor_Subset5_TL
      REAL( fp_kind ), DIMENSION(MAX_N_SUBSET_TOTAL_PREDICTORS, MAX_N_ODCAPS_LAYERS) ::  Predictor_Subset6_TL
      REAL( fp_kind ), DIMENSION(MAX_N_SUBSET_TOTAL_PREDICTORS, MAX_N_ODCAPS_LAYERS) ::  Predictor_Subset7_TL
      TYPE( ODCAPS_TauCoeff_type ), POINTER  ::  TC => NULL()

      TC => ODCAPS_TC(Sensor_Index)
         
      
 
    !#--------------------------------------------------------------------------#
    !#              -- GET THE Atmosphere GASES INDEX  --              #
    !#--------------------------------------------------------------------------#
       H2O_Index = MINLOC( ABS( Predictor%Absorber_ID - H2O_ID ), DIM = 1 )
       O3_Index  = MINLOC( ABS( Predictor%Absorber_ID - O3_ID ),  DIM = 1 )
       CO_Index  = MINLOC( ABS( Predictor%Absorber_ID - CO_ID ),  DIM = 1 )
       CH4_Index = MINLOC( ABS( Predictor%Absorber_ID - CH4_ID ), DIM = 1 )

    !#--------------------------------------------------------------------------#
    !#                    --INITIALIZE THE SUM TERMS TO ZERO   --               #
    !#--------------------------------------------------------------------------#
       PNORM = ZERO
       TZ    = ZERO
       WZREF = ZERO
       WZ    = ZERO
       XZREF = ZERO
       XZ    = ZERO
       OZREF = ZERO
       OZ    = ZERO
       TOZ   = ZERO
       CZREF = ZERO
       CZ    = ZERO
       MZREF = ZERO
       MZ    = ZERO
       TMZ   = ZERO

       PNORM_TL = ZERO
       TZ_TL    = ZERO
       WZREF_TL = ZERO
       WZ_TL    = ZERO
       XZREF_TL = ZERO
       XZ_TL    = ZERO
       OZREF_TL = ZERO
       OZ_TL    = ZERO
       TOZ_TL   = ZERO
       CZREF_TL = ZERO
       CZ_TL    = ZERO
       MZREF_TL = ZERO
       MZ_TL    = ZERO
       TMZ_TL   = ZERO
       
       Cal_Sun = .TRUE.                                            
    !#--------------------------------------------------------------------------#
    !#                -- CALCULATE THE INTEGRATED PREDICTORS --                 #
    !#--------------------------------------------------------------------------#

    Layer_Loop : DO L = 1, Predictor%n_Layers 

      IF (L == 1) THEN					        
         PDP = TC%Ref_Profile_Data(3,1)*( TC%Ref_Profile_Data(3,2) &    
               -TC%Ref_Profile_Data(3,1))	
	 PDP_TL = ZERO

         TRZ    = ZERO	
	 TRZ_TL = ZERO
	 					        
         TAZ_O    = ZERO						        
         TAZ_O_TL = ZERO
	 
	 TAZ_M    = ZERO
	 TAZ_M_TL = ZERO
	        		        
      ELSE							        

         PDP = TC%Ref_Profile_Data(3,L)*( TC%Ref_Profile_Data(3,L) &    
               -TC%Ref_Profile_Data(3,L-1) )			        
	 PDP_TL = ZERO

         PNORM = PNORM + PDP					        
         PNORM_TL = PNORM_TL + PDP_TL					        

      !  Note: TRZ, TOZ, and TMZ use layer-above terms  	        
         TZ    = TZ + PDP*TR 					        
         TRZ   = TZ/PNORM						        

         TOZ   = TOZ + PDP*DT*A_O					        
         TAZ_O = TOZ/PNORM					        

         TMZ   = TMZ + PDP*TR*A_M					        
         TAZ_M = TMZ/PNORM		
	 
         TZ_TL    = TZ_TL + PDP_TL * TR + PDP * TR_TL 					        
         TRZ_TL   = TZ_TL/PNORM - TZ * PNORM_TL / PNORM**TWO						        

         TOZ_TL   = TOZ_TL + PDP_TL * DT * A_O	&
	          + PDP * DT_TL * A_O + PDP * DT * A_O_TL				        
         TAZ_O_TL = TOZ_TL/PNORM - TOZ * PNORM_TL / PNORM**TWO 					        

         TMZ_TL   = TMZ_TL + PDP_TL * TR * A_M &
	          + PDP * TR_TL * A_M + PDP * TR * A_M_TL				        
         TAZ_M_TL = TMZ_TL/PNORM - TMZ * PNORM_TL / PNORM**TWO 					        
	
	 			        
      ENDIF							        

      !  Temperature terms					      
         DT = Predictor%Temperature(L) - TC%Ref_Profile_Data(4,L) 				      
         TR = Predictor%Temperature(L) / TC%Ref_Profile_Data(4,L)					      
         
	 DT_TL = Predictor_TL%Temperature(L)
         TR_TL = Predictor_TL%Temperature(L) / TC%Ref_Profile_Data(4,L)
       
         IF ( PRESENT (Calc_Sun_Angle_Secant) .AND. & 
	      Predictor%Calc_Sun_Angle_Secant) THEN
 
      !	 Total secant
	   SECANG(L) = Predictor%Secant_Source_Zenith(L)
 	   Cal_Sun = .TRUE.
	 ELSE
	   SECANG(L) = Predictor%Secant_Sensor_Zenith(L)
	   Cal_Sun = .FALSE. 
	 END IF
	 
      !  Water terms						      
         A_W   = Predictor%Absorber(L,H2O_Index)/TC%Ref_Profile_Data(6,L)				      
         WZREF = WZREF + PDP*TC%Ref_Profile_Data(6,L)				      
         WZ    = WZ + PDP*Predictor%Absorber(L,H2O_Index)					      
         AZ_W  = WZ/WZREF						      

         A_W_TL   = Predictor_TL%Absorber(L,H2O_Index)/TC%Ref_Profile_Data(6,L)				      
         WZREF_TL = WZREF_TL + PDP_TL * TC%Ref_Profile_Data(6,L)				      
         WZ_TL    = WZ_TL + PDP_TL * Predictor%Absorber(L,H2O_Index) &
	                  + PDP * Predictor_TL%Absorber(L,H2O_Index)					      
         AZ_W_TL  = WZ_TL/WZREF - WZ * WZREF_TL / WZREF**TWO
	 						      
      !  Ozone terms						      
         A_O   = Predictor%Absorber(L,O3_Index)/TC%Ref_Profile_Data(7,L)				      
         XZREF = XZREF + TC%Ref_Profile_Data(7,L)				      
         XZ    = XZ + Predictor%Absorber(L,O3_Index)					      
         XZ_O  = XZ/XZREF						      
         OZREF = OZREF + PDP*TC%Ref_Profile_Data(7,L)				      
         OZ    = OZ + PDP*Predictor%Absorber(L,O3_Index)					      
         AZ_O  = OZ/OZREF						      

         A_O_TL   = Predictor_TL%Absorber(L,O3_Index)/TC%Ref_Profile_Data(7,L)				      
         XZREF_TL = XZREF_TL  				      
         XZ_TL    = XZ_TL + Predictor_TL%Absorber(L,O3_Index)					      
         XZ_O_TL  = XZ_TL/XZREF - XZ * XZREF_TL / XZREF**TWO 						      
         OZREF_TL = OZREF_TL + PDP_TL * TC%Ref_Profile_Data(7,L)				      
         OZ_TL    = OZ_TL + PDP_TL * Predictor%Absorber(L,O3_Index) &
	 		  + PDP * Predictor_TL%Absorber(L,O3_Index)			      
         AZ_O_TL  = OZ_TL/OZREF - OZ * OZREF_TL /OZREF**TWO						      

      !  Carbon monoxide terms
         A_C=Predictor%Absorber(L,CO_Index)/TC%Ref_Profile_Data(8,L)     
         CZREF=CZREF + PDP*TC%Ref_Profile_Data(8,L)   
         CZ=CZ + PDP*Predictor%Absorber(L,CO_Index) 	     
         AZ_C=CZ/CZREF		     

         A_C_TL   = Predictor_TL%Absorber(L,CO_Index)/TC%Ref_Profile_Data(8,L)     
         CZREF_TL = CZREF_TL + PDP_TL * TC%Ref_Profile_Data(8,L)   
         CZ_TL    = CZ_TL + PDP_TL * Predictor%Absorber(L,CO_Index) &
	  	          + PDP * Predictor_TL%Absorber(L,CO_Index) 
         AZ_C_TL  =CZ_TL/CZREF - CZ * CZREF_TL / CZREF**TWO 		     

      !  Methane terms
         A_M=Predictor%Absorber(L,CH4_Index)/TC%Ref_Profile_Data(9,L) 
         MZREF=MZREF + PDP*TC%Ref_Profile_Data(9,L)  
         MZ=MZ + PDP*Predictor%Absorber(L,CH4_Index) 	     
         AZ_M=MZ/MZREF		     

         A_M_TL   = Predictor_TL%Absorber(L,CH4_Index)/TC%Ref_Profile_Data(9,L) 
         MZREF_TL = MZREF_TL + PDP_TL * TC%Ref_Profile_Data(9,L)  
         MZ_TL    = MZ_TL + PDP_TL * Predictor%Absorber(L,CH4_Index) &
	  	          + PDP * Predictor_TL%Absorber(L,CH4_Index)
         AZ_M_TL  = MZ_TL/MZREF - MZ * MZREF_TL / MZREF**TWO		     

      !  ----------------
      !   Fixed 
      !  ----------------
         TJUNKS    = TR*TR
	 TJUNKS_TL    = TWO * TR * TR_TL
      !   FIXMUL(L) = Predictor%Fix_Amount_Multiplier( L )

       !  -----
       !  Ozone
       !  -----
         OJUNKA = SECANG(L) *A_O
         OJUNKR = SQRT( OJUNKA )
         OJUNKZ = OJUNKA/XZ_O
         OJUNKX = SECANG(L)*XZ_O

         OJUNKA_TL = SECANG(L) * A_O_TL
         OJUNKR_TL = POINT_5 * OJUNKA**(-POINT_5) * OJUNKA_TL
         OJUNKZ_TL = OJUNKA_TL / XZ_O - OJUNKA * XZ_O_TL / XZ_O**TWO
         OJUNKX_TL = SECANG(L) * XZ_O_TL

      !  -----
      !  Water
      !  -----
         WJUNKA = SECANG(L)*A_W
         WJUNKR = SQRT( WJUNKA )
         WJUNKS = WJUNKA*WJUNKA
         WJUNKZ = WJUNKA*A_W/AZ_W
         WJUNK4 = SQRT( WJUNKR )

         WJUNKA_TL = SECANG(L) * A_W_TL
         WJUNKR_TL = POINT_5 * WJUNKA**(-POINT_5) * WJUNKA_TL  
         WJUNKS_TL = TWO * WJUNKA_TL * WJUNKA
         WJUNKZ_TL = WJUNKA_TL * A_W / AZ_W + WJUNKA * A_W_TL / AZ_W &
	             - WJUNKA * A_W * AZ_W_TL / AZ_W**TWO
         WJUNK4_TL = POINT_5 * WJUNKR**(-POINT_5) * WJUNKR_TL
	 
      !  ---------------
      !  Carbon monoxide for FCOW = set4
      !  ---------------
         CJUNKA = SECANG(L)*A_C   
         CJUNKR = SQRT( CJUNKA )  
         CJUNKS = CJUNKA*CJUNKA   
         CJUNKZ = CJUNKA*A_C/AZ_C 
      
         CJUNKA_TL = SECANG(L) * A_C_TL  
         CJUNKR_TL = POINT_5 * CJUNKA**(-POINT_5) * CJUNKA_TL  
         CJUNKS_TL = TWO * CJUNKA_TL * CJUNKA   
         CJUNKZ_TL = CJUNKA_TL * A_C / AZ_C + CJUNKA * A_C_TL / AZ_C &
	             - CJUNKA * A_C * AZ_C_TL / AZ_C**TWO
      !  ------- 	       
      !  Methane for FMW = set3 
      !  ------- 	       
         MJUNKA = SECANG(L)*A_M  
         MJUNKR = SQRT(MJUNKA)   
         MJUNKZ = SECANG(L)*AZ_M 

         MJUNKA_TL = SECANG(L) * A_M_TL   
         MJUNKR_TL = POINT_5 * MJUNKA**(-POINT_5) * MJUNKA_TL    
         MJUNKZ_TL = SECANG(L) * AZ_M_TL
	 
      !  ----------------------
      !  Load up the predictors
      !  ----------------------
         
	 Subset_Select: SELECT CASE ( Set_Index)

	   CASE ( 1 )
         !  ---------------			      
         !  Water continuum (for FWO, FOW, FMW, FCOW)
         !  ---------------
            Predictor_Subset1_TL(1,L) = WJUNKA_TL / TJUNKS - WJUNKA * TJUNKS_TL / TJUNKS**2
            Predictor_Subset1_TL(2,L) = WJUNKA_TL * A_W / TJUNKS**2  + WJUNKA *A_W_TL / TJUNKS**2 &
                                      - WJUNKA * A_W * TWO * TJUNKS_TL / TJUNKS**3
            Predictor_Subset1_TL(3,L) = WJUNKA_TL / TR - WJUNKA * TR_TL / TR**2
            Predictor_Subset1_TL(4,L) = WJUNKA_TL * A_W / TR + WJUNKA * A_W_TL / TR &
                                      - WJUNKA * A_W * TR_TL / TR**2 
            Predictor_Subset1_TL(5,L) = WJUNKA_TL * A_W / TJUNKS + WJUNKA * A_W_TL / TJUNKS &
                                      - WJUNKA * A_W * TJUNKS_TL / TJUNKS**2 
            Predictor_Subset1_TL(6,L) = WJUNKA_TL / TJUNKS**2 - TWO * WJUNKA * TJUNKS_TL / TJUNKS**3
            Predictor_Subset1_TL(7,L) = WJUNKA_TL
      
         !  ----------------
         !   Fixed (for FWO)
         !  ----------------
            Predictor_Subset1_TL( 8,L) = ZERO
            Predictor_Subset1_TL( 9,L) = ZERO
            Predictor_Subset1_TL(10,L) = SECANG(L) * TR_TL
            Predictor_Subset1_TL(11,L) = SECANG(L) * TJUNKS_TL
            Predictor_Subset1_TL(12,L) = TR_TL
            Predictor_Subset1_TL(13,L) = TJUNKS_TL
            Predictor_Subset1_TL(14,L) = SECANG(L) * TRZ_TL
            Predictor_Subset1_TL(15,L) = SECANG(L) * TRZ_TL / TR &
	                               - SECANG(L) * TRZ * TR_TL / TR**TWO

         !  ----------------
         !  Water predictors for FWO = set1  
         !  ----------------
            Predictor_Subset1_TL(16,L) = WJUNKA_TL		 
            Predictor_Subset1_TL(17,L) = WJUNKR_TL		 
            Predictor_Subset1_TL(18,L) = WJUNKZ_TL		 
            Predictor_Subset1_TL(19,L) = WJUNKA_TL * DT + WJUNKA * DT_TL	 
            Predictor_Subset1_TL(20,L) = WJUNKS_TL		 
            Predictor_Subset1_TL(21,L) = WJUNKR_TL * DT + WJUNKR * DT_TL	 
            Predictor_Subset1_TL(22,L) = WJUNK4_TL		 
            Predictor_Subset1_TL(23,L) = WJUNKZ_TL / WJUNKR - WJUNKZ * WJUNKR_TL / WJUNKR**TWO 	 
            Predictor_Subset1_TL(24,L) = WJUNKS_TL * WJUNKA + WJUNKS * WJUNKA_TL	 
            Predictor_Subset1_TL(25,L) = A_W_TL	
	    IF( DT > ZERO) THEN	 
              Predictor_Subset1_TL(26,L) = WJUNKA_TL* DT * DT + TWO * WJUNKA * DT_TL * DT  
            ELSE
              Predictor_Subset1_TL(26,L) = - WJUNKA_TL* DT * DT - TWO * WJUNKA * DT_TL * DT  
	    ENDIF
            
          !  -----
          !  Ozone predictors for FWO = set1
          !  -----
            Predictor_Subset1_TL(27,L) = OJUNKA_TL    
            Predictor_Subset1_TL(28,L) = OJUNKR_TL    
            Predictor_Subset1_TL(29,L) = OJUNKA_TL * DT	+ OJUNKA * DT_TL      
            Predictor_Subset1_TL(30,L) = TWO * OJUNKA_TL * OJUNKA  
            Predictor_Subset1_TL(31,L) = OJUNKR_TL * DT	+ OJUNKR * DT_TL      
         
	    IF( .NOT. Cal_Sun) THEN
	      Predictor_TL%Predictor_Subset(1, :, L) = Predictor_Subset1_TL(:, L)
	    ENDIF
	    
	   CASE ( 2 )

         !  ---------------			      
         !  Water continuum (for FWO, FOW, FMW, FCOW)
         !  ---------------
            Predictor_Subset2_TL(1,L) = WJUNKA_TL / TJUNKS - WJUNKA * TJUNKS_TL / TJUNKS**2
            Predictor_Subset2_TL(2,L) = WJUNKA_TL *A_W / TJUNKS**2 + WJUNKA *A_W_TL / TJUNKS**2 &
                                      - WJUNKA * A_W * TWO * TJUNKS_TL / TJUNKS**3
            Predictor_Subset2_TL(3,L) = WJUNKA_TL / TR - WJUNKA * TR_TL / TR**2
            Predictor_Subset2_TL(4,L) = WJUNKA_TL * A_W / TR + WJUNKA * A_W_TL / TR &
                                      - WJUNKA * A_W * TR_TL / TR**2 
            Predictor_Subset2_TL(5,L) = WJUNKA_TL * A_W / TJUNKS + WJUNKA * A_W_TL / TJUNKS &
                                      - WJUNKA * A_W * TJUNKS_TL / TJUNKS**2 
            Predictor_Subset2_TL(6,L) = WJUNKA_TL / TJUNKS**2 - TWO * WJUNKA * TJUNKS_TL / TJUNKS**3
            Predictor_Subset2_TL(7,L) = WJUNKA_TL

 
         !  ----------------
         !   Fixed (for FOW)
         !  ----------------
            Predictor_Subset2_TL( 8,L) = ZERO
            Predictor_Subset2_TL( 9,L) = ZERO
            Predictor_Subset2_TL(10,L) = SECANG(L) * TR_TL
            Predictor_Subset2_TL(11,L) = SECANG(L) * TJUNKS_TL
            Predictor_Subset2_TL(12,L) = TR_TL
            Predictor_Subset2_TL(13,L) = TJUNKS_TL
            Predictor_Subset2_TL(14,L) = SECANG(L) * TRZ_TL
            Predictor_Subset2_TL(15,L) = SECANG(L) * TRZ_TL / TR &
	                               - SECANG(L) * TRZ * TR_TL / TR**TWO
 
         !  -----			     
         !  Ozone predictors for FOW = set2  
         !  -----			     
            Predictor_Subset2_TL(16,L) = OJUNKA_TL
            Predictor_Subset2_TL(17,L) = OJUNKR_TL
            Predictor_Subset2_TL(18,L) = OJUNKA_TL * DT + OJUNKA * DT_TL
            Predictor_Subset2_TL(19,L) = TWO * OJUNKA_TL * OJUNKA
            Predictor_Subset2_TL(20,L) = OJUNKR_TL * DT + OJUNKR * DT_TL
            Predictor_Subset2_TL(21,L) = OJUNKZ_TL * A_O + OJUNKZ * A_O_TL
            Predictor_Subset2_TL(22,L) = OJUNKR_TL * A_O / XZ_O + OJUNKR * A_O_TL / XZ_O &
	                               - OJUNKR * A_O * XZ_O_TL / XZ_O**TWO
            Predictor_Subset2_TL(23,L) = OJUNKZ_TL * AZ_O + OJUNKZ * AZ_O_TL
            Predictor_Subset2_TL(24,L) = OJUNKA_TL * SQRT( OJUNKX ) &
	                               + OJUNKA * POINT_5 * OJUNKX**(-POINT_5) * OJUNKX_TL 
            Predictor_Subset2_TL(25,L) = OJUNKA_TL * TAZ_O * SECANG(L) + OJUNKA * TAZ_O_TL * SECANG(L)  

         !  ----------------
         !  Water predictors for FOW = set2  
         !  ----------------
            Predictor_Subset2_TL(26,L) = WJUNKA_TL
            Predictor_Subset2_TL(27,L) = WJUNKR_TL
            Predictor_Subset2_TL(28,L) = WJUNKA_TL * DT + WJUNKA * DT_TL 
            Predictor_Subset2_TL(29,L) = WJUNKA_TL * OJUNKX + WJUNKA * OJUNKX_TL 
            Predictor_Subset2_TL(30,L) = WJUNKS_TL
            Predictor_Subset2_TL(31,L) = WJUNK4_TL
            Predictor_Subset2_TL(32,L) = WJUNKR_TL * DT + WJUNKR * DT_TL
            Predictor_Subset2_TL(33,L) = WJUNKZ_TL
            Predictor_Subset2_TL(34,L) = WJUNKA_TL * WJUNKS + WJUNKA * WJUNKS_TL
            Predictor_Subset2_TL(35,L) = WJUNKA_TL * OJUNKX * OJUNKX + TWO * WJUNKA * OJUNKX * OJUNKX_TL
            Predictor_Subset2_TL(36,L) = WJUNKZ_TL / WJUNKR - WJUNKZ * WJUNKR_TL / WJUNKR**TWO

	    IF( .NOT. Cal_Sun) THEN
  	      Predictor_TL%Predictor_Subset(2, :, L) = Predictor_Subset2_TL(:, L)
	    ENDIF
	      
	   CASE ( 3 )
         !  ---------------			      
         !  Water continuum (for FWO, FOW, FMW, FCOW)
         !  ---------------
            Predictor_Subset3_TL(1,L) = WJUNKA_TL / TJUNKS - WJUNKA * TJUNKS_TL / TJUNKS**2
            Predictor_Subset3_TL(2,L) = WJUNKA_TL *A_W / TJUNKS**2 + WJUNKA *A_W_TL / TJUNKS**2 &
                                      - WJUNKA * A_W * TWO * TJUNKS_TL / TJUNKS**3
            Predictor_Subset3_TL(3,L) = WJUNKA_TL / TR - WJUNKA * TR_TL / TR**TWO
            Predictor_Subset3_TL(4,L) = WJUNKA_TL * A_W / TR + WJUNKA * A_W_TL / TR &
                                      - WJUNKA * A_W * TR_TL / TR**2 
            Predictor_Subset3_TL(5,L) = WJUNKA_TL * A_W / TJUNKS + WJUNKA * A_W_TL / TJUNKS &
                                      - WJUNKA * A_W * TJUNKS_TL / TJUNKS**2 
            Predictor_Subset3_TL(6,L) = WJUNKA_TL / TJUNKS**2 - TWO * WJUNKA * TJUNKS_TL / TJUNKS**3
            Predictor_Subset3_TL(7,L) = WJUNKA_TL
 
         !  ----------------
         !   Fixed (for FMW)
         !  ----------------
            Predictor_Subset3_TL( 8,L) = ZERO
            Predictor_Subset3_TL( 9,L) = ZERO 
            Predictor_Subset3_TL(10,L) = SECANG(L) * TR_TL
            Predictor_Subset3_TL(11,L) = SECANG(L) * TJUNKS_TL
            Predictor_Subset3_TL(12,L) = TR_TL
            Predictor_Subset3_TL(13,L) = TJUNKS_TL
            Predictor_Subset3_TL(14,L) = SECANG(L) * TRZ_TL
            Predictor_Subset3_TL(15,L) = SECANG(L) * TRZ_TL / TR &
	                               - SECANG(L) * TRZ * TR_TL / TR**TWO
 
         !  -------
         !  Methane for FMW = set3
         !  -------
            Predictor_Subset3_TL(16,L) = MJUNKA_TL
            Predictor_Subset3_TL(17,L) = MJUNKR_TL
            Predictor_Subset3_TL(18,L) = MJUNKA_TL * DT + MJUNKA * DT_TL
            Predictor_Subset3_TL(19,L) = TWO * MJUNKA_TL * MJUNKA
            Predictor_Subset3_TL(20,L) = MJUNKA_TL * SECANG(L)
            Predictor_Subset3_TL(21,L) = MJUNKZ_TL
            Predictor_Subset3_TL(22,L) = A_M_TL * DT + A_M * DT_TL  
            Predictor_Subset3_TL(23,L) = TAZ_M_TL * SECANG(L) 
            Predictor_Subset3_TL(24,L) = POINT_5 * MJUNKZ**(-POINT_5) * MJUNKZ_TL 

         !  -------
         !  water predictors for FMW = set3
         !  -------
            Predictor_Subset3_TL(25,L) = WJUNKA_TL
            Predictor_Subset3_TL(26,L) = WJUNKR_TL
            Predictor_Subset3_TL(27,L) = WJUNKZ_TL
            Predictor_Subset3_TL(28,L) = WJUNKA_TL * DT + WJUNKA * DT_TL 
            Predictor_Subset3_TL(29,L) = WJUNKS_TL
            Predictor_Subset3_TL(30,L) = WJUNKR_TL * DT + WJUNKR * DT_TL
            Predictor_Subset3_TL(31,L) = WJUNK4_TL
            Predictor_Subset3_TL(32,L) = WJUNKS_TL * WJUNKA + WJUNKS * WJUNKA_TL
            Predictor_Subset3_TL(33,L) = A_W_TL
            Predictor_Subset3_TL(34,L) = WJUNKZ_TL / WJUNKR - WJUNKZ * WJUNKR_TL / WJUNKR**TWO
            Predictor_Subset3_TL(35,L) = WJUNKR_TL * MJUNKZ + WJUNKR * MJUNKZ_TL

	    IF( .NOT. Cal_Sun) THEN
  	      Predictor_TL%Predictor_Subset(3, :, L) = Predictor_Subset3_TL(:, L)
	    ENDIF
	      
	   CASE ( 4 )
         !  ---------------			      
         !  Water continuum (for FWO, FOW, FMW, FCOW)
         !  ---------------
            Predictor_Subset4_TL(1,L) = WJUNKA_TL / TJUNKS - WJUNKA * TJUNKS_TL / TJUNKS**2
            Predictor_Subset4_TL(2,L) = WJUNKA_TL *A_W / TJUNKS**2 + WJUNKA *A_W_TL / TJUNKS**2 &
                                      - WJUNKA * A_W * TWO * TJUNKS_TL / TJUNKS**3
            Predictor_Subset4_TL(3,L) = WJUNKA_TL / TR - WJUNKA * TR_TL / TR**2
            Predictor_Subset4_TL(4,L) = WJUNKA_TL * A_W / TR + WJUNKA * A_W_TL / TR &
                                      - WJUNKA * A_W * TR_TL / TR**2 
            Predictor_Subset4_TL(5,L) = WJUNKA_TL * A_W / TJUNKS + WJUNKA * A_W_TL / TJUNKS &
                                      - WJUNKA * A_W * TJUNKS_TL / TJUNKS**2 
            Predictor_Subset4_TL(6,L) = WJUNKA_TL / TJUNKS**2 - TWO * WJUNKA * TJUNKS_TL / TJUNKS**3
            Predictor_Subset4_TL(7,L) = WJUNKA_TL
 
         !  ----------------
         !   Fixed (for FCOW)
         !  ----------------
            Predictor_Subset4_TL( 8,L) = ZERO 
            Predictor_Subset4_TL( 9,L) = ZERO 
            Predictor_Subset4_TL(10,L) = SECANG(L) * TR_TL
            Predictor_Subset4_TL(11,L) = SECANG(L) * TJUNKS_TL
            Predictor_Subset4_TL(12,L) = TR_TL
            Predictor_Subset4_TL(13,L) = TJUNKS_TL
            Predictor_Subset4_TL(14,L) = SECANG(L) * TRZ_TL
            Predictor_Subset4_TL(15,L) = SECANG(L) * SECANG(L) * TRZ_TL 
            Predictor_Subset4_TL(16,L) = SECANG(L) * SECANG(L) * TR_TL
            Predictor_Subset4_TL(17,L) = ZERO  
            Predictor_Subset4_TL(18,L) = ZERO   

         !  ---------------
         !  Carbon monoxide for FCOW = set4
         !  ---------------
            Predictor_Subset4_TL(19,L) = CJUNKA_TL
            Predictor_Subset4_TL(20,L) = CJUNKR_TL
            Predictor_Subset4_TL(21,L) = CJUNKA_TL * DT + CJUNKA * DT_TL
            Predictor_Subset4_TL(22,L) = CJUNKS_TL
            Predictor_Subset4_TL(23,L) = CJUNKZ_TL
            Predictor_Subset4_TL(24,L) = CJUNKR_TL * DT + CJUNKR * DT_TL
            Predictor_Subset4_TL(25,L) = POINT_5 * CJUNKR**(-POINT_5) * CJUNKR_TL 
            Predictor_Subset4_TL(26,L) = CJUNKZ_TL / CJUNKR - CJUNKZ * CJUNKR_TL / CJUNKR**TWO
            Predictor_Subset4_TL(27,L) = A_C_TL
            Predictor_Subset4_TL(28,L) = CJUNKA_TL * SECANG(L)  
            Predictor_Subset4_TL(29,L) = CJUNKR_TL * SECANG(L)  
	    
         !  ---------------
         !  ozone predictors for FCOW = set4
         !  ---------------
            Predictor_Subset4_TL(30,L) = OJUNKA_TL
            Predictor_Subset4_TL(31,L) = OJUNKR_TL
            Predictor_Subset4_TL(32,L) = OJUNKA_TL * DT + OJUNKA * DT_TL 

         !  -------
         !  water predictors for FCOW = set4 
         !  -------
            Predictor_Subset4_TL(33,L) = WJUNKA_TL
            Predictor_Subset4_TL(34,L) = A_W_TL
            Predictor_Subset4_TL(35,L) = WJUNKR_TL
            Predictor_Subset4_TL(36,L) = WJUNKA_TL * DT + WJUNKA * DT_TL
            Predictor_Subset4_TL(37,L) = WJUNKS_TL
            Predictor_Subset4_TL(38,L) = WJUNKR_TL * DT + WJUNKR * DT_TL
            Predictor_Subset4_TL(39,L) = WJUNK4_TL
            Predictor_Subset4_TL(40,L) = WJUNKZ_TL
            Predictor_Subset4_TL(41,L) = WJUNKA_TL * SECANG(L)  
            Predictor_Subset4_TL(42,L) = WJUNKS_TL * WJUNKA + WJUNKS * WJUNKA_TL
            Predictor_Subset4_TL(43,L) = WJUNKA_TL* AZ_C * SECANG(L) + WJUNKA * AZ_C_TL * SECANG(L)  
            Predictor_Subset4_TL(44,L) = WJUNKZ_TL / WJUNKR - WJUNKZ * WJUNKR_TL / WJUNKR**TWO
            Predictor_Subset4_TL(45,L) = WJUNKA_TL* DT * SECANG(L) + WJUNKA * DT_TL * SECANG(L)  
	    
	    IF( .NOT. Cal_Sun) THEN
	      Predictor_TL%Predictor_Subset(4, :, L) = Predictor_Subset4_TL(:, L)
            ELSE
	      Predictor_TL%Predictor_Subset_Sun(1, :, L) = Predictor_Subset4_TL(:, L)
            ENDIF

	   CASE ( 5 )
         !  ---------------			      
         !  Water continuum (for FWO, FOW, FMW, FCOW)
         !  ---------------
            Predictor_Subset5_TL(1,L) = WJUNKA_TL / TJUNKS - WJUNKA * TJUNKS_TL / TJUNKS**2
            Predictor_Subset5_TL(2,L) = WJUNKA_TL *A_W / TJUNKS**2 + WJUNKA *A_W_TL / TJUNKS**2 &
                                      - WJUNKA * A_W * TWO * TJUNKS_TL / TJUNKS**3
            Predictor_Subset5_TL(3,L) = WJUNKA_TL / TR - WJUNKA * TR_TL / TR**TWO
            Predictor_Subset5_TL(4,L) = WJUNKA_TL * A_W / TR + WJUNKA * A_W_TL / TR &
                                      - WJUNKA * A_W * TR_TL / TR**2 
            Predictor_Subset5_TL(5,L) = WJUNKA_TL * A_W / TJUNKS + WJUNKA * A_W_TL / TJUNKS &
                                      - WJUNKA * A_W * TJUNKS_TL / TJUNKS**2 
            Predictor_Subset5_TL(6,L) = WJUNKA_TL / TJUNKS**2 - TWO * WJUNKA * TJUNKS_TL / TJUNKS**3
            Predictor_Subset5_TL(7,L) = WJUNKA_TL
 
         !  ----------------
         !   Fixed for FWO sun bfsw = set5
         !  ----------------
            Predictor_Subset5_TL( 8,L) = ZERO 
            Predictor_Subset5_TL( 9,L) = ZERO 
            Predictor_Subset5_TL(10,L) = SECANG(L) * TR_TL
            Predictor_Subset5_TL(11,L) = SECANG(L) * TJUNKS_TL
            Predictor_Subset5_TL(12,L) = TR_TL
            Predictor_Subset5_TL(13,L) = TJUNKS_TL
            Predictor_Subset5_TL(14,L) = SECANG(L) * TRZ_TL
            Predictor_Subset5_TL(15,L) = SECANG(L) * TRZ_TL / TR - SECANG(L) * TRZ * TR_TL / TR**TWO
            Predictor_Subset5_TL(16,L) = SECANG(L) * SECANG(L) * TR_TL
            Predictor_Subset5_TL(17,L) = ZERO   
	    Predictor_Subset5_TL(18,L) = TRZ_TL
 
         !  ----------------
         !  Water predictors for FWO sun bfsw = set5
         !  ----------------
            Predictor_Subset5_TL(19,L) = WJUNKA_TL
            Predictor_Subset5_TL(20,L) = WJUNKA_TL * WJUNKR + WJUNKA * WJUNKR_TL
            Predictor_Subset5_TL(21,L) = WJUNKA_TL * DT + WJUNKA * DT_TL

         !  ----------------
         !  ozone predictors for FWO sun bfsw = set5
         !  ----------------
            Predictor_Subset5_TL(22,L) = OJUNKA_TL
	  
	    IF( .NOT. Cal_Sun) THEN
	      Predictor_TL%Predictor_Subset(5, :, L) = Predictor_Subset5_TL(:, L)
	    ELSE
	      Predictor_TL%Predictor_Subset_Sun(2, :, L) = Predictor_Subset5_TL(:, L)
            ENDIF

	   CASE ( 6 )
         !  ---------------			      
         !  Water continuum (for FWO, FOW, FMW, FCOW)
         !  ---------------
            Predictor_Subset6_TL(1,L) = WJUNKA_TL / TJUNKS - WJUNKA * TJUNKS_TL / TJUNKS**2
            Predictor_Subset6_TL(2,L) = WJUNKA_TL *A_W / TJUNKS**2 + WJUNKA *A_W_TL / TJUNKS**2 &
                                      - WJUNKA * A_W * TWO * TJUNKS_TL / TJUNKS**3
            Predictor_Subset6_TL(3,L) = WJUNKA_TL / TR - WJUNKA * TR_TL / TR**2
            Predictor_Subset6_TL(4,L) = WJUNKA_TL * A_W / TR + WJUNKA * A_W_TL / TR &
                                      - WJUNKA * A_W * TR_TL / TR**2 
            Predictor_Subset6_TL(5,L) = WJUNKA_TL * A_W / TJUNKS + WJUNKA * A_W_TL / TJUNKS &
                                      - WJUNKA * A_W * TJUNKS_TL / TJUNKS**2 
            Predictor_Subset6_TL(6,L) = WJUNKA_TL / TJUNKS**2 - TWO * WJUNKA * TJUNKS_TL / TJUNKS**3
            Predictor_Subset6_TL(7,L) = WJUNKA_TL
 
         !  ----------------
         !   Fixed for FWO sun mfmw = set6 
         !  ----------------
            Predictor_Subset6_TL( 8,L) = ZERO
            Predictor_Subset6_TL( 9,L) = ZERO 
            Predictor_Subset6_TL(10,L) = SECANG(L) * TR_TL
            Predictor_Subset6_TL(11,L) = SECANG(L) * TJUNKS_TL
            Predictor_Subset6_TL(12,L) = TR_TL
            Predictor_Subset6_TL(13,L) = TJUNKS_TL
            Predictor_Subset6_TL(14,L) = SECANG(L) * TRZ_TL
            Predictor_Subset6_TL(15,L) = ZERO   

         !  ----------------
         !  Water predictors for FWO sun mfmw = set6
         !  ----------------
            Predictor_Subset6_TL(16,L) = WJUNKA_TL
            Predictor_Subset6_TL(17,L) = WJUNKA_TL * WJUNKR + WJUNKA * WJUNKR_TL
            Predictor_Subset6_TL(18,L) = WJUNKA_TL * DT + WJUNKA * DT_TL 
            Predictor_Subset6_TL(19,L) = WJUNKS_TL
            Predictor_Subset6_TL(20,L) = WJUNKA_TL * WJUNKR * DT + WJUNKA * WJUNKR_TL * DT &
	                               + WJUNKA * WJUNKR * DT_TL
            Predictor_Subset6_TL(21,L) = WJUNKA_TL * WJUNKS + WJUNKA * WJUNKS_TL
            Predictor_Subset6_TL(22,L) = WJUNKA_TL * SECANG(L)

         !  ----------------
         !  ozone predictors for FWO sun mfmw = set6 
         !  ----------------
            Predictor_Subset6_TL(23,L) = OJUNKA_TL

	    IF( .NOT. Cal_Sun) THEN
	      Predictor_TL%Predictor_Subset(6, :, L) = Predictor_Subset6_TL(:, L)
            ELSE
	      Predictor_TL%Predictor_Subset_Sun(3, :, L) = Predictor_Subset6_TL(:, L)
	    ENDIF
	        
	   CASE ( 7 )
         !  ---------------			      
         !  Water continuum (for FWO, FOW, FMW, FCOW)
         !  ---------------
            Predictor_Subset7_TL(1,L) = WJUNKA_TL / TJUNKS - WJUNKA * TJUNKS_TL / TJUNKS**2
            Predictor_Subset7_TL(2,L) = WJUNKA_TL *A_W / TJUNKS**2 + WJUNKA *A_W_TL / TJUNKS**2 &
                                      - WJUNKA * A_W * TWO * TJUNKS_TL / TJUNKS**3
            Predictor_Subset7_TL(3,L) = WJUNKA_TL / TR - WJUNKA * TR_TL / TR**TWO
            Predictor_Subset7_TL(4,L) = WJUNKA_TL * A_W / TR + WJUNKA * A_W_TL / TR &
                                      - WJUNKA * A_W * TR_TL / TR**2 
            Predictor_Subset7_TL(5,L) = WJUNKA_TL * A_W / TJUNKS + WJUNKA * A_W_TL / TJUNKS &
                                      - WJUNKA * A_W * TJUNKS_TL / TJUNKS**2 
            Predictor_Subset7_TL(6,L) = WJUNKA_TL / TJUNKS**2 - TWO * WJUNKA * TJUNKS_TL / TJUNKS**3
            Predictor_Subset7_TL(7,L) = WJUNKA_TL
 
         !  ----------------
         !   Fixed for FWO sun mfbw = set7 
         !  ----------------
            Predictor_Subset7_TL( 8,L) = ZERO
            Predictor_Subset7_TL( 9,L) = ZERO 
            Predictor_Subset7_TL(10,L) = SECANG(L) * TR_TL
            Predictor_Subset7_TL(11,L) = SECANG(L) * TJUNKS_TL
            Predictor_Subset7_TL(12,L) = TR_TL
            Predictor_Subset7_TL(13,L) = TJUNKS_TL
            Predictor_Subset7_TL(14,L) = SECANG(L) * TRZ_TL
            Predictor_Subset7_TL(15,L) = ZERO   
 
         !  ----------------
         !  Water predictors for FWO sun mfbw = set7 
         !  ----------------
            Predictor_Subset7_TL(16,L) = WJUNKA_TL
            Predictor_Subset7_TL(17,L) = WJUNKA_TL * WJUNKR + WJUNKA * WJUNKR_TL
            Predictor_Subset7_TL(18,L) = WJUNKA_TL * DT + WJUNKA * DT_TL 
            Predictor_Subset7_TL(19,L) = WJUNKS_TL
            Predictor_Subset7_TL(20,L) = WJUNKA_TL * WJUNKR * DT + WJUNKA * WJUNKR_TL * DT &
	                               + WJUNKA * WJUNKR * DT_TL
            Predictor_Subset7_TL(21,L) = WJUNKA_TL * WJUNKS + WJUNKA * WJUNKS_TL
            Predictor_Subset7_TL(22,L) = WJUNKA_TL * SECANG(L)  
            Predictor_Subset7_TL(23,L) = WJUNKZ_TL
            Predictor_Subset7_TL(24,L) = WJUNKZ_TL * WJUNKR + WJUNKZ * WJUNKR_TL
            Predictor_Subset7_TL(25,L) = WJUNKA_TL * WJUNK4 + WJUNKA * WJUNK4_TL
            Predictor_Subset7_TL(26,L) = WJUNKA_TL * WJUNKZ + WJUNKA * WJUNKZ_TL
            Predictor_Subset7_TL(27,L) = WJUNKA_TL * A_W + WJUNKA * A_W_TL
            Predictor_Subset7_TL(28,L) = WJUNKS_TL / WJUNK4 - WJUNKS * WJUNK4_TL / WJUNK4**TWO

         !  ----------------
         !  ozone predictors for FWO sun mfbw = set7 
         !  ----------------
            Predictor_Subset7_TL(29,L) = OJUNKA_TL

	    IF( .NOT. Cal_Sun) THEN
	      Predictor_TL%Predictor_Subset(7, :, L) = Predictor_Subset7_TL(:, L)
            ELSE
	      Predictor_TL%Predictor_Subset_Sun(4, :, L) = Predictor_Subset7_TL(:, L)
	    ENDIF
	     
	 END SELECT Subset_Select
	 
    END DO Layer_Loop
    
    NULLIFY(TC)
  END SUBROUTINE  Compute_Predictors_Subset_TL

  ! 7 subset adjoint predictors for the gas absorption model
  SUBROUTINE  Compute_Predictors_Subset_AD( Sensor_Index,           &  ! Input
                                            Set_Index,              &  ! Input
                                            Predictor,          &  ! Input
					    Predictor_AD,       &  ! In/Output
					 Calc_Sun_Angle_Secant)     ! Optional Input

    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    INTEGER,  INTENT( IN )     :: Sensor_Index 
    INTEGER,  INTENT( IN )     :: Set_Index 

    ! -- Inputs
    TYPE( Predictor_type ), INTENT( IN ) :: Predictor
   
    ! -- In/Outputs
    TYPE( Predictor_type ), INTENT( IN OUT ) :: Predictor_AD

    ! -- Optional Inputs
    INTEGER,               OPTIONAL, INTENT( IN )     :: Calc_Sun_Angle_Secant 


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Predictors_Subset_AD'


    ! ---------------
    ! Local variables
    ! ---------------
      INTEGER :: L			    
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) ::    PDP,     PDP_AD 	    
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) ::  PNORM,   PNORM_AD  	   
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) ::     DT,      DT_AD            
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) ::     TR,      TR_AD            
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) ::     TZ,      TZ_AD            
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) ::    TRZ,     TRZ_AD            
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) ::    A_W,     A_W_AD            
      REAL( fp_kind ), DIMENSION(0:Predictor%n_Layers) :: WZREF,   WZREF_AD            
      REAL( fp_kind ), DIMENSION(0:Predictor%n_Layers) ::    WZ,      WZ_AD            
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) ::   AZ_W,    AZ_W_AD            
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) ::    A_O,     A_O_AD            
      REAL( fp_kind ), DIMENSION(0:Predictor%n_Layers) ::  XZREF,   XZREF_AD            
      REAL( fp_kind ), DIMENSION(0:Predictor%n_Layers) ::     XZ,      XZ_AD            
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) ::   XZ_O,    XZ_O_AD            
      REAL( fp_kind ), DIMENSION(0:Predictor%n_Layers) ::  OZREF,   OZREF_AD            
      REAL( fp_kind ), DIMENSION(0:Predictor%n_Layers) ::     OZ,      OZ_AD            
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) ::   AZ_O,    AZ_O_AD            
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) ::    TOZ,     TOZ_AD            
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) ::  TAZ_O,   TAZ_O_AD            
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) ::    A_C,     A_C_AD            
      REAL( fp_kind ), DIMENSION(0:Predictor%n_Layers) ::     CZ,      CZ_AD            
      REAL( fp_kind ), DIMENSION(0:Predictor%n_Layers) ::  CZREF,   CZREF_AD            
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) ::   AZ_C,    AZ_C_AD            
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) ::    A_M,     A_M_AD            
      REAL( fp_kind ), DIMENSION(0:Predictor%n_Layers) ::  MZREF,   MZREF_AD            
      REAL( fp_kind ), DIMENSION(0:Predictor%n_Layers) ::     MZ,      MZ_AD            
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) ::   AZ_M,    AZ_M_AD            
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) ::    TMZ,     TMZ_AD            
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) ::  TAZ_M,   TAZ_M_AD            
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) :: TJUNKS,  TJUNKS_AD            
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) :: WJUNKA,  WJUNKA_AD            
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) :: WJUNKR,  WJUNKR_AD            
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) :: WJUNKS,  WJUNKS_AD            
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) :: WJUNKZ,  WJUNKZ_AD            
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) :: WJUNK4,  WJUNK4_AD            
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) :: OJUNKA,  OJUNKA_AD            
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) :: OJUNKR,  OJUNKR_AD            
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) :: OJUNKZ,  OJUNKZ_AD            
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) :: OJUNKX,  OJUNKX_AD            
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) :: CJUNKA,  CJUNKA_AD            
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) :: CJUNKR,  CJUNKR_AD            
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) :: CJUNKS,  CJUNKS_AD            
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) :: CJUNKZ,  CJUNKZ_AD            
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) :: MJUNKA,  MJUNKA_AD            
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) :: MJUNKR,  MJUNKR_AD           
      REAL( fp_kind ), DIMENSION(Predictor%n_Layers) :: MJUNKZ,  MJUNKZ_AD  
      	    
      REAL( fp_kind ), DIMENSION(MAX_N_ODCAPS_LAYERS) :: SECANG, SECANG_AD
 
      INTEGER :: H2O_Index		    
      INTEGER :: O3_Index		    
      INTEGER :: CO_Index		    
      INTEGER :: CH4_Index		    
    
      LOGICAL :: Cal_Sun
 
      REAL( fp_kind ), DIMENSION(MAX_N_SUBSET_TOTAL_PREDICTORS, MAX_N_ODCAPS_LAYERS) ::  Predictor_Subset1_AD
      REAL( fp_kind ), DIMENSION(MAX_N_SUBSET_TOTAL_PREDICTORS, MAX_N_ODCAPS_LAYERS) ::  Predictor_Subset2_AD
      REAL( fp_kind ), DIMENSION(MAX_N_SUBSET_TOTAL_PREDICTORS, MAX_N_ODCAPS_LAYERS) ::  Predictor_Subset3_AD
      REAL( fp_kind ), DIMENSION(MAX_N_SUBSET_TOTAL_PREDICTORS, MAX_N_ODCAPS_LAYERS) ::  Predictor_Subset4_AD
      REAL( fp_kind ), DIMENSION(MAX_N_SUBSET_TOTAL_PREDICTORS, MAX_N_ODCAPS_LAYERS) ::  Predictor_Subset5_AD
      REAL( fp_kind ), DIMENSION(MAX_N_SUBSET_TOTAL_PREDICTORS, MAX_N_ODCAPS_LAYERS) ::  Predictor_Subset6_AD
      REAL( fp_kind ), DIMENSION(MAX_N_SUBSET_TOTAL_PREDICTORS, MAX_N_ODCAPS_LAYERS) ::  Predictor_Subset7_AD
      TYPE( ODCAPS_TauCoeff_type ), POINTER  ::  TC => NULL()

      TC => ODCAPS_TC(Sensor_Index)
         
      
 
    !#--------------------------------------------------------------------------#
    !#              -- GET THE Atmosphere GASES INDEX  --              #
    !#--------------------------------------------------------------------------#
       H2O_Index = MINLOC( ABS( Predictor%Absorber_ID - H2O_ID ), DIM = 1 )
       O3_Index  = MINLOC( ABS( Predictor%Absorber_ID - O3_ID ),  DIM = 1 )
       CO_Index  = MINLOC( ABS( Predictor%Absorber_ID - CO_ID ),  DIM = 1 )
       CH4_Index = MINLOC( ABS( Predictor%Absorber_ID - CH4_ID ), DIM = 1 )

    !#--------------------------------------------------------------------------#
    !#                    --INITIALIZE THE SUM TERMS TO ZERO   --               #
    !#--------------------------------------------------------------------------#
       PNORM(1) = ZERO
       TZ(1)    = ZERO
       WZREF(0) = ZERO
       WZ(0)    = ZERO
       XZREF(0) = ZERO
       XZ(0)    = ZERO
       OZREF(0) = ZERO
       OZ(0)    = ZERO
       TOZ(1)   = ZERO
       CZREF(0) = ZERO
       CZ(0)    = ZERO
       MZREF(0) = ZERO
       MZ(0)    = ZERO
       TMZ(1)   = ZERO
   
       Cal_Sun = .TRUE.                                            
    !#--------------------------------------------------------------------------#
    !#                -- CALCULATE THE INTEGRATED PREDICTORS --                 #
    !#--------------------------------------------------------------------------#

    Forward_Layer_Loop : DO L = 1, Predictor%n_Layers 
      IF (L == 1) THEN					        
         PDP(L) = TC%Ref_Profile_Data(3,1)*( TC%Ref_Profile_Data(3,2) &    
                -TC%Ref_Profile_Data(3,1))			        
         TRZ(L)   = ZERO
         TAZ_O(L) = ZERO						        
         TAZ_M(L) = ZERO						        
      ELSE							        
         PDP(L) = TC%Ref_Profile_Data(3,L)*( TC%Ref_Profile_Data(3,L) &    
                -TC%Ref_Profile_Data(3,L-1) )			        
         PNORM(L) = PNORM(L-1) + PDP(L)					        

      !  Note: TRZ, TOZ, and TMZ use layer-above terms  	        
         TZ(L)    = TZ(L-1) + PDP(L)* TR(L-1) 					        
         TRZ(L)   = TZ(L)/PNORM(L)						        

         TOZ(L)   = TOZ(L-1) + PDP(L)*DT(L-1)*A_O(L-1)					        
         TAZ_O(L) = TOZ(L)/PNORM(L)					        

         TMZ(L)   = TMZ(L-1) + PDP(L)*TR(L-1)*A_M(L-1)					        
         TAZ_M(L) = TMZ(L)/PNORM(L)					        
      ENDIF							        

      !  Temperature terms					      
         DT(L) = Predictor%Temperature(L) - TC%Ref_Profile_Data(4,L) 				      
         TR(L) = Predictor%Temperature(L) / TC%Ref_Profile_Data(4,L)					      

         IF ( PRESENT (Calc_Sun_Angle_Secant) .AND. & 
	      Predictor%Calc_Sun_Angle_Secant) THEN
 
      !	 Total secant
	   SECANG(L) = Predictor%Secant_Source_Zenith(L)
	   Cal_Sun = .TRUE.
	 ELSE
	   SECANG(L) = Predictor%Secant_Sensor_Zenith(L)
	   Cal_Sun = .FALSE. 
	 END IF
	 
      !  Water terms						      
         A_W(L)   = Predictor%Absorber(L,H2O_Index)/TC%Ref_Profile_Data(6,L)				      
         WZREF(L) = WZREF(L-1) + PDP(L)*TC%Ref_Profile_Data(6,L)				      
         WZ(L)    = WZ(L-1) + PDP(L)*Predictor%Absorber(L,H2O_Index)					      
         AZ_W(L)  = WZ(L)/WZREF(L)						      

      !  Ozone terms						      
         A_O(L)   = Predictor%Absorber(L,O3_Index)/TC%Ref_Profile_Data(7,L)				      
         XZREF(L) = XZREF(L-1) + TC%Ref_Profile_Data(7,L)				      
         XZ(L)    = XZ(L-1) + Predictor%Absorber(L,O3_Index)					      
         XZ_O(L)  = XZ(L)/XZREF(L)						      
         OZREF(L) = OZREF(L-1) + PDP(L)*TC%Ref_Profile_Data(7,L)				      
         OZ(L)    = OZ(L-1) + PDP(L)*Predictor%Absorber(L,O3_Index)					      
         AZ_O(L)  = OZ(L)/OZREF(L)						      

      !  Carbon monoxide terms
         A_C(L)  = Predictor%Absorber(L,CO_Index)/TC%Ref_Profile_Data(8,L)     
         CZREF(L)= CZREF(L-1) + PDP(L)*TC%Ref_Profile_Data(8,L)   
         CZ(L)   = CZ(L-1) + PDP(L)*Predictor%Absorber(L,CO_Index) 	     
         AZ_C(L) = CZ(L)/CZREF(L)		     

      !  Methane terms
         A_M(L)  = Predictor%Absorber(L,CH4_Index)/TC%Ref_Profile_Data(9,L) 
         MZREF(L)= MZREF(L-1) + PDP(L)*TC%Ref_Profile_Data(9,L)  
         MZ(L)   = MZ(L-1) + PDP(L)*Predictor%Absorber(L,CH4_Index) 	     
         AZ_M(L) = MZ(L)/MZREF(L)		     

      !  ----------------
      !   Fixed 
      !  ----------------
         TJUNKS(L)    = TR(L)*TR(L)
      !   FIXMUL(L) = Predictor%Fix_Amount_Multiplier( L )

       !  -----
       !  Ozone
       !  -----
         OJUNKA(L) = SECANG(L) *A_O(L)
         OJUNKR(L) = SQRT( OJUNKA(L) )
         OJUNKZ(L) = OJUNKA(L)/XZ_O(L)
         OJUNKX(L) = SECANG(L)*XZ_O(L)

      !  -----
      !  Water
      !  -----
         WJUNKA(L) = SECANG(L)*A_W(L)
         WJUNKR(L) = SQRT( WJUNKA(L) )
         WJUNKS(L) = WJUNKA(L)*WJUNKA(L)
         WJUNKZ(L) = WJUNKA(L)*A_W(L)/AZ_W(L)
         WJUNK4(L) = SQRT( WJUNKR(L) )

      !  ---------------
      !  Carbon monoxide for FCOW = set4
      !  ---------------
         CJUNKA(L) = SECANG(L)*A_C(L)   
         CJUNKR(L) = SQRT( CJUNKA(L) )  
         CJUNKS(L) = CJUNKA(L)*CJUNKA(L)   
         CJUNKZ(L) = CJUNKA(L)*A_C(L)/AZ_C(L) 
      
      !  ------- 	       
      !  Methane for FMW = set3 
      !  ------- 	       
         MJUNKA(L) = SECANG(L)*A_M(L) 
         MJUNKR(L) = SQRT(MJUNKA(L))  
         MJUNKZ(L) = SECANG(L)*AZ_M(L)

    END DO Forward_Layer_Loop

    !#--------------------------------------------------------------------------#
    !#                -- INITIALISE LOCAL ADJOINT VARIABLES --                  #
    !#--------------------------------------------------------------------------#

       PNORM_AD( : )  = ZERO
       TZ_AD( : )     = ZERO
       WZREF_AD( : )  = ZERO
       WZ_AD( : )     = ZERO
       XZREF_AD( : )  = ZERO
       XZ_AD( : )     = ZERO
       OZREF_AD( : )  = ZERO
       OZ_AD( : )     = ZERO
       TOZ_AD( : )    = ZERO
       CZREF_AD( : )  = ZERO
       CZ_AD( : )     = ZERO
       MZREF_AD( : )  = ZERO
       MZ_AD( : )     = ZERO
       TMZ_AD( : )    = ZERO
       PDP_AD( : )    = ZERO
       DT_AD( : )     = ZERO 
       TR_AD( : )     = ZERO 
       TRZ_AD( : )    = ZERO 
       A_W_AD( : )    = ZERO 
       AZ_W_AD( : )   = ZERO 
       A_O_AD( : )    = ZERO 
       XZ_O_AD( : )   = ZERO 
       AZ_O_AD( : )   = ZERO 
       TAZ_O_AD( : )  = ZERO 
       A_C_AD( : )    = ZERO 
       AZ_C_AD( : )   = ZERO 
       A_M_AD( : )    = ZERO 
       AZ_M_AD( : )   = ZERO 
       TAZ_M_AD( : )  = ZERO 
       TJUNKS_AD( : ) = ZERO 
       WJUNKA_AD( : ) = ZERO 
       WJUNKR_AD( : ) = ZERO 
       WJUNKS_AD( : ) = ZERO 
       WJUNKZ_AD( : ) = ZERO 
       WJUNK4_AD( : ) = ZERO 
       OJUNKA_AD( : ) = ZERO 
       OJUNKR_AD( : ) = ZERO 
       OJUNKZ_AD( : ) = ZERO 
       OJUNKX_AD( : ) = ZERO 
       CJUNKA_AD( : ) = ZERO 
       CJUNKR_AD( : ) = ZERO 
       CJUNKS_AD( : ) = ZERO 
       CJUNKZ_AD( : ) = ZERO 
       MJUNKA_AD( : ) = ZERO 
       MJUNKR_AD( : ) = ZERO 
       MJUNKZ_AD( : ) = ZERO 
       
    Adjoint_Layer_Loop : DO L = Predictor%n_Layers, 1, -1 

      !  ----------------------
      !  Load up the predictors
      !  ----------------------
         
	 Subset_Select: SELECT CASE ( Set_Index)

	   CASE ( 1 )
	   
	    IF( .NOT. Cal_Sun) THEN
	      Predictor_Subset1_AD(:, L) = Predictor_AD%Predictor_Subset(1, :, L)
	    ENDIF
        
	 !  -----
         !  Ozone predictors for FWO = set1
         !  -----
	    OJUNKA_AD(L) = OJUNKA_AD(L) + Predictor_Subset1_AD(27,L) +  Predictor_Subset1_AD(29,L) * DT(L) &
	                 + TWO * Predictor_Subset1_AD(30,L) * OJUNKA(L) 
	    OJUNKR_AD(L) = OJUNKR_AD(L) + Predictor_Subset1_AD(28,L) + Predictor_Subset1_AD(31,L) * DT(L) 	 
	   
	    DT_AD(L) = DT_AD(L) + OJUNKA(L) * Predictor_Subset1_AD(29,L) &
	             + OJUNKR(L) * Predictor_Subset1_AD(31,L)
		     
         !  ----------------
         !  Water predictors for FWO = set1  
         !  ----------------
            WJUNKA_AD(L) = WJUNKA_AD(L) + Predictor_Subset1_AD(16,L) + Predictor_Subset1_AD(19,L) * DT(L) &
                         + Predictor_Subset1_AD(24,L) * WJUNKS(L) &
                         + Predictor_Subset1_AD(26,L) * DT(L) * ABS( DT(L) ) 
            WJUNKR_AD(L) = WJUNKR_AD(L) + Predictor_Subset1_AD(17,L) + Predictor_Subset1_AD(21,L) * DT(L)  &
                         - WJUNKZ(L) * Predictor_Subset1_AD(23,L) / WJUNKR(L)**TWO 
            WJUNKZ_AD(L) = WJUNKZ_AD(L) + Predictor_Subset1_AD(18,L) + Predictor_Subset1_AD(23,L)/ WJUNKR(L) 
            WJUNKS_AD(L) = WJUNKS_AD(L) + Predictor_Subset1_AD(20,L) + Predictor_Subset1_AD(24,L) * WJUNKA(L) 
            WJUNK4_AD(L) = WJUNK4_AD(L) + Predictor_Subset1_AD(22,L)  
            A_W_AD(L) = A_W_AD(L) + Predictor_Subset1_AD(25,L)
	    IF( DT(L) > ZERO) THEN	 
	    
             DT_AD(L) = DT_AD(L) + WJUNKA(L) * Predictor_Subset1_AD(19,L) &
                                 + WJUNKR(L) * Predictor_Subset1_AD(21,L) &
                                 + TWO * WJUNKA(L) * DT(L) * Predictor_Subset1_AD(26,L)  
            ELSE
	    
             DT_AD(L) = DT_AD(L) + WJUNKA(L) * Predictor_Subset1_AD(19,L) &
                                 + WJUNKR(L) * Predictor_Subset1_AD(21,L) &
                                 - TWO * WJUNKA(L) * DT(L) * Predictor_Subset1_AD(26,L)  
	      
 	    ENDIF				                   
	     
         !  ----------------
         !   Fixed (for FWO)
         !  ----------------
            TR_AD(L) = TR_AD(L) + SECANG(L) * Predictor_Subset1_AD(10,L) &
                     + Predictor_Subset1_AD(12,L) - SECANG(L) * TRZ(L) * Predictor_Subset1_AD(15,L)/ TR(L)**TWO
            TJUNKS_AD(L) = TJUNKS_AD(L) + SECANG(L) * Predictor_Subset1_AD(11,L) + Predictor_Subset1_AD(13,L) 
            TRZ_AD(L) = TRZ_AD(L) + SECANG(L) * Predictor_Subset1_AD(14,L) &
                      + SECANG(L) * Predictor_Subset1_AD(15,L) / TR(L)   
        
         !  ---------------			      
         !  Water continuum (for FWO, FOW, FMW, FCOW)
         !  ---------------
            WJUNKA_AD(L) = WJUNKA_AD(L) + Predictor_Subset1_AD(1,L) / TJUNKS(L)   &
                         + Predictor_Subset1_AD(2,L) * A_W(L) / TJUNKS(L)**2 &
                         + Predictor_Subset1_AD(3,L) / TR(L)  &
                         + Predictor_Subset1_AD(4,L) * A_W(L) / TR(L)  &      
                         + Predictor_Subset1_AD(5,L) * A_W(L) / TJUNKS(L)  & 
                         + Predictor_Subset1_AD(6,L) / TJUNKS(L)**2 &
                         + Predictor_Subset1_AD(7,L)
 			  
            TJUNKS_AD(L) = TJUNKS_AD(L) - WJUNKA(L) * Predictor_Subset1_AD(1,L) / TJUNKS(L)**2 &
                         - WJUNKA(L) * A_W(L)* TWO * Predictor_Subset1_AD(2,L) / TJUNKS(L)**3 &
                         - WJUNKA(L) * A_W(L) * Predictor_Subset1_AD(5,L) / TJUNKS(L)**2  &
                         - TWO * WJUNKA(L) * Predictor_Subset1_AD(6,L) / TJUNKS(L)**3
            A_W_AD(L) = A_W_AD(L) + WJUNKA(L) * Predictor_Subset1_AD(2,L) / TJUNKS(L)**2 &
                      + WJUNKA(L) * Predictor_Subset1_AD(4,L) / TR(L)  &
                      + WJUNKA(L) * Predictor_Subset1_AD(5,L) / TJUNKS(L)          
            TR_AD(L) = TR_AD(L) - WJUNKA(L) * Predictor_Subset1_AD(3,L) / TR(L)**2 &
                     - WJUNKA(L) * A_W(L) * Predictor_Subset1_AD(4,L) / TR(L)**2 
            
	    Predictor_Subset1_AD(:, L) = ZERO
	    
	   CASE ( 2 )

	    IF( .NOT. Cal_Sun) THEN
	      Predictor_Subset2_AD(:, L) = Predictor_AD%Predictor_Subset(2, :, L)
	    ENDIF
        
         !  ----------------
         !  Water predictors for FOW = set2  
         !  ----------------
            WJUNKA_AD(L) = WJUNKA_AD(L) + Predictor_Subset2_AD(26,L) + Predictor_Subset2_AD(28,L) * DT(L) &
                         + Predictor_Subset2_AD(29,L) * OJUNKX(L) &
                         + Predictor_Subset2_AD(34,L) * WJUNKS(L) &
			 + Predictor_Subset2_AD(35,L) * OJUNKX(L) * OJUNKX(L)  
			    
            WJUNKR_AD(L) = WJUNKR_AD(L) + Predictor_Subset2_AD(27,L) + Predictor_Subset2_AD(32,L) * DT(L)  &
                         - WJUNKZ(L) * Predictor_Subset2_AD(36,L) / WJUNKR(L)**TWO
			  
            WJUNKZ_AD(L) = WJUNKZ_AD(L) + Predictor_Subset2_AD(33,L) + Predictor_Subset2_AD(36,L)/ WJUNKR(L)
	     
            WJUNKS_AD(L) = WJUNKS_AD(L) + Predictor_Subset2_AD(30,L) + Predictor_Subset2_AD(34,L) * WJUNKA(L) 
	    
            WJUNK4_AD(L) = WJUNK4_AD(L) + Predictor_Subset2_AD(31,L)
	      
            OJUNKX_AD(L) = OJUNKX_AD(L) + WJUNKA(L) * Predictor_Subset2_AD(29,L) &
	                 + TWO * WJUNKA(L) * OJUNKX(L) * Predictor_Subset2_AD(35,L) 
			     
	    DT_AD(L) = DT_AD(L) + WJUNKA(L) * Predictor_Subset2_AD(28,L) &
                                + WJUNKR(L) * Predictor_Subset2_AD(32,L)                
	     
	 !  -----
         !  Ozone predictors for FOW = set2
         !  -----
	    OJUNKA_AD(L) = OJUNKA_AD(L) + Predictor_Subset2_AD(16,L) +  Predictor_Subset2_AD(18,L) * DT(L) &
	                 + TWO * Predictor_Subset2_AD(19,L) * OJUNKA(L) &
			 + Predictor_Subset2_AD(24,L) * SQRT( OJUNKX(L) ) &
			 + Predictor_Subset2_AD(25,L) * TAZ_O(L) * SECANG(L)
			 
	    OJUNKR_AD(L) = OJUNKR_AD(L) + Predictor_Subset2_AD(17,L) + Predictor_Subset2_AD(20,L) * DT(L) &
	                 + Predictor_Subset2_AD(22,L) * A_O(L) / XZ_O(L)
			 
	    OJUNKZ_AD(L) = OJUNKZ_AD(L) + Predictor_Subset2_AD(21,L) * A_O(L) &
	                 + Predictor_Subset2_AD(23,L) * AZ_O(L) 		 
	    
	    OJUNKX_AD(L) = OJUNKX_AD(L) + OJUNKA(L) * POINT_5 * OJUNKX(L)**(-POINT_5) * Predictor_Subset2_AD(24,L)
	    
	    DT_AD(L) = DT_AD(L) + OJUNKA(L) * Predictor_Subset2_AD(18,L) &
	             + OJUNKR(L) * Predictor_Subset2_AD(20,L)

            A_O_AD(L) = A_O_AD(L) + OJUNKZ(L) * Predictor_Subset2_AD(21,L) &
	              + OJUNKR(L) * Predictor_Subset2_AD(22,L) / XZ_O(L)

            XZ_O_AD(L) = XZ_O_AD(L) - OJUNKR(L) * A_O(L) * Predictor_Subset2_AD(22,L) / XZ_O(L)**TWO 
	    
	    AZ_O_AD(L) = AZ_O_AD(L) + OJUNKZ(L) * Predictor_Subset2_AD(23,L)
	    
	    TAZ_O_AD(L) = TAZ_O_AD(L) + OJUNKA(L) * Predictor_Subset2_AD(25,L) * SECANG(L)
	    
         !  ----------------
         !   Fixed (for FWO) 
         !  ----------------
			    
            TR_AD(L) = TR_AD(L) + SECANG(L) * Predictor_Subset2_AD(10,L) &
                     + Predictor_Subset2_AD(12,L) - SECANG(L) * TRZ(L) * Predictor_Subset2_AD(15,L)/ TR(L)**TWO
            
	    TJUNKS_AD(L) = TJUNKS_AD(L) + SECANG(L) * Predictor_Subset2_AD(11,L) + Predictor_Subset2_AD(13,L) 
            
	    TRZ_AD(L) = TRZ_AD(L) + SECANG(L) * Predictor_Subset2_AD(14,L) &
                      + SECANG(L) * Predictor_Subset2_AD(15,L) / TR(L)   
        
         !  ---------------			      
         !  Water continuum (for FWO, FOW, FMW, FCOW)
         !  ---------------
            WJUNKA_AD(L) = WJUNKA_AD(L) + Predictor_Subset2_AD(1,L) / TJUNKS(L)   &
                         + Predictor_Subset2_AD(2,L) * A_W(L) / TJUNKS(L)**2 &
                         + Predictor_Subset2_AD(3,L) / TR(L)  &
                         + Predictor_Subset2_AD(4,L) * A_W(L) / TR(L)  &      
                         + Predictor_Subset2_AD(5,L) * A_W(L) / TJUNKS(L)  & 
                         + Predictor_Subset2_AD(6,L) / TJUNKS(L)**2 &
                         + Predictor_Subset2_AD(7,L) 
            TJUNKS_AD(L) = TJUNKS_AD(L) - WJUNKA(L) * Predictor_Subset2_AD(1,L) / TJUNKS(L)**2 &
                         - WJUNKA(L) * A_W(L) * TWO * Predictor_Subset2_AD(2,L) / TJUNKS(L)**3 &
                         - WJUNKA(L) * A_W(L) * Predictor_Subset2_AD(5,L) / TJUNKS(L)**2 &
                         - TWO * WJUNKA(L) * Predictor_Subset2_AD(6,L) / TJUNKS(L)**3
            A_W_AD(L) = A_W_AD(L) + WJUNKA(L) * Predictor_Subset2_AD(2,L) / TJUNKS(L)**2 &
                      + WJUNKA(L) * Predictor_Subset2_AD(4,L) / TR(L)  &
                      + WJUNKA(L) * Predictor_Subset2_AD(5,L) / TJUNKS(L)          
            TR_AD(L) = TR_AD(L) - WJUNKA(L) * Predictor_Subset2_AD(3,L) / TR(L)**2 &
                     - WJUNKA(L) * A_W(L) * Predictor_Subset2_AD(4,L) / TR(L)**2 
	    
	    Predictor_Subset2_AD(:,L) = ZERO	     

	   CASE ( 3 )

	    IF( .NOT. Cal_Sun) THEN
	      Predictor_Subset3_AD(:, L) = Predictor_AD%Predictor_Subset(3, :, L)
	    ENDIF
        
         !  ----------------
         !  Water predictors for FMW = set3   
         !  ----------------
            WJUNKA_AD(L) = WJUNKA_AD(L) + Predictor_Subset3_AD(25,L) + Predictor_Subset3_AD(28,L) * DT(L) &
                         + Predictor_Subset3_AD(32,L) * WJUNKS(L) 
  			    
            WJUNKR_AD(L) = WJUNKR_AD(L) + Predictor_Subset3_AD(26,L) + Predictor_Subset3_AD(30,L) * DT(L)  &
                         - WJUNKZ(L) * Predictor_Subset3_AD(34,L) / WJUNKR(L)**TWO & 
			 + Predictor_Subset3_AD(35,L) * MJUNKZ(L)

            WJUNKZ_AD(L) = WJUNKZ_AD(L) + Predictor_Subset3_AD(27,L) + Predictor_Subset3_AD(34,L)/ WJUNKR(L)
	     
            WJUNKS_AD(L) = WJUNKS_AD(L) + Predictor_Subset3_AD(29,L) + Predictor_Subset3_AD(32,L) * WJUNKA(L) 
	    
            WJUNK4_AD(L) = WJUNK4_AD(L) + Predictor_Subset3_AD(31,L)
 			     
	    DT_AD(L) = DT_AD(L) + WJUNKA(L) * Predictor_Subset3_AD(28,L) &
                                + WJUNKR(L) * Predictor_Subset3_AD(30,L)
            	                    
	    MJUNKZ_AD(L) = MJUNKZ_AD(L) + WJUNKR(L) * Predictor_Subset3_AD(35,L)
	    
	    A_W_AD(L) = A_W_AD(L) + Predictor_Subset3_AD(33,L)
	    
         !  -------
         !  Methane for FMW = set3
         !  -------
      	    MJUNKA_AD(L) = MJUNKA_AD(L) + Predictor_Subset3_AD(16,L) + Predictor_Subset3_AD(18,L) * DT(L) &
	                 + TWO * Predictor_Subset3_AD(19,L) * MJUNKA(L) &
			 + Predictor_Subset3_AD(20,L) * SECANG(L) 
	    
	    MJUNKR_AD(L) = MJUNKR_AD(L) + Predictor_Subset3_AD(17,L) 
	     
	    MJUNKZ_AD(L) = MJUNKZ_AD(L) + Predictor_Subset3_AD(21,L) &
	                 + POINT_5 * MJUNKZ(L)**(-POINT_5) * Predictor_Subset3_AD(24,L)
	    
	    DT_AD(L) = DT_AD(L) + MJUNKA(L) * Predictor_Subset3_AD(18,L) &
	             + A_M(L) * Predictor_Subset3_AD(22,L)
		     
	    A_M_AD(L) = A_M_AD(L) + Predictor_Subset3_AD(22,L) * DT(L)
	    
	    TAZ_M_AD(L) = TAZ_M_AD(L) + Predictor_Subset3_AD(23,L) * SECANG(L)
	    
			   
         !  ----------------
         !   Fixed (for FMW) 
         !  ----------------
			    
            TR_AD(L) = TR_AD(L) + SECANG(L) * Predictor_Subset3_AD(10,L) &
                     + Predictor_Subset3_AD(12,L) - SECANG(L) * TRZ(L) * Predictor_Subset3_AD(15,L)/ TR(L)**TWO
            
	    TJUNKS_AD(L) = TJUNKS_AD(L) + SECANG(L) * Predictor_Subset3_AD(11,L) + Predictor_Subset3_AD(13,L) 
            
	    TRZ_AD(L) = TRZ_AD(L) + SECANG(L) * Predictor_Subset3_AD(14,L) &
                      + SECANG(L) * Predictor_Subset3_AD(15,L) / TR(L)   
        
         !  ---------------			      
         !  Water continuum (for FWO, FOW, FMW, FCOW)
         !  ---------------
            WJUNKA_AD(L) = WJUNKA_AD(L) + Predictor_Subset3_AD(1,L) / TJUNKS(L)   &
                         + Predictor_Subset3_AD(2,L) * A_W(L) / TJUNKS(L)**2 &
                         + Predictor_Subset3_AD(3,L) / TR(L)  &
                         + Predictor_Subset3_AD(4,L) * A_W(L) / TR(L)  &      
                         + Predictor_Subset3_AD(5,L) * A_W(L) / TJUNKS(L)  & 
                         + Predictor_Subset3_AD(6,L) / TJUNKS(L)**2 &
                         + Predictor_Subset3_AD(7,L) 
            TJUNKS_AD(L) = TJUNKS_AD(L) - WJUNKA(L) * Predictor_Subset3_AD(1,L) / TJUNKS(L)**2 &
                         - WJUNKA(L) * A_W(L) * TWO * Predictor_Subset3_AD(2,L) / TJUNKS(L)**3 &
                         - WJUNKA(L) * A_W(L) * Predictor_Subset3_AD(5,L) / TJUNKS(L)**2 &
                         - TWO * WJUNKA(L) * Predictor_Subset3_AD(6,L) / TJUNKS(L)**3
            A_W_AD(L) = A_W_AD(L) + WJUNKA(L) * Predictor_Subset3_AD(2,L) / TJUNKS(L)**2 &
                      + WJUNKA(L) * Predictor_Subset3_AD(4,L) / TR(L)  &
                      + WJUNKA(L) * Predictor_Subset3_AD(5,L) / TJUNKS(L)          
            TR_AD(L) = TR_AD(L) - WJUNKA(L) * Predictor_Subset3_AD(3,L) / TR(L)**2 &
                     - WJUNKA(L) * A_W(L) * Predictor_Subset3_AD(4,L) / TR(L)**2 

            Predictor_Subset3_AD(:,L) = ZERO
	   
	   CASE ( 4 )
	   
	    IF( .NOT. Cal_Sun) THEN
	      Predictor_Subset4_AD(:, L) = Predictor_AD%Predictor_Subset(4, :, L) 
            ELSE
	      Predictor_Subset4_AD(:, L) = Predictor_AD%Predictor_Subset_Sun(1, :, L) 
            ENDIF

         !  ----------------
         !  Water predictors for FCOW = set4    
         !  ----------------
            WJUNKA_AD(L) = WJUNKA_AD(L) + Predictor_Subset4_AD(33,L) + Predictor_Subset4_AD(36,L) * DT(L) &
	                 + Predictor_Subset4_AD(41,L) * SECANG(L) &  
                         + Predictor_Subset4_AD(42,L) * WJUNKS(L) &
			 + Predictor_Subset4_AD(43,L) * AZ_C(L) * SECANG(L) &
			 + Predictor_Subset4_AD(45,L) * DT(L) * SECANG(L)
			    
            WJUNKR_AD(L) = WJUNKR_AD(L) + Predictor_Subset4_AD(35,L) + Predictor_Subset4_AD(38,L) * DT(L) &
	                 - WJUNKZ(L) * Predictor_Subset4_AD(44,L) / WJUNKR(L)**TWO  

            WJUNKZ_AD(L) = WJUNKZ_AD(L)+ Predictor_Subset4_AD(40,L)+ Predictor_Subset4_AD(44,L) / WJUNKR(L)  
	     
            WJUNKS_AD(L) = WJUNKS_AD(L) + Predictor_Subset4_AD(37,L) + Predictor_Subset4_AD(42,L) * WJUNKA(L) 
	    
            WJUNK4_AD(L) = WJUNK4_AD(L) + Predictor_Subset4_AD(39,L)
 			     
	    DT_AD(L) = DT_AD(L) + WJUNKA(L) * Predictor_Subset4_AD(36,L) &
                                + WJUNKR(L) * Predictor_Subset4_AD(38,L) &
				+ WJUNKA(L) * Predictor_Subset4_AD(45,L) * SECANG(L)
            
	    A_W_AD(L) = A_W_AD(L) + Predictor_Subset4_AD(34,L)
	    
	    
	    AZ_C_AD(L) = AZ_C_AD(L) + WJUNKA(L) * Predictor_Subset4_AD(43,L) * SECANG(L)		

         !  ---------------
         !  ozone predictors for FCOW = set4
         !  ---------------
	    OJUNKA_AD(L) = OJUNKA_AD(L) + Predictor_Subset4_AD(30,L) + Predictor_Subset4_AD(32,L) * DT(L)
	    OJUNKR_AD(L) = OJUNKR_AD(L) + Predictor_Subset4_AD(31,L)
	    DT_AD(L) = DT_AD(L) + OJUNKA(L) * Predictor_Subset4_AD(32,L)
	       		 
         !  ---------------
         !  Carbon monoxide for FCOW = set4
         !  ---------------
            CJUNKA_AD(L) = CJUNKA_AD(L) + Predictor_Subset4_AD(19,L) + Predictor_Subset4_AD(21,L) * DT(L) &
	                 + Predictor_Subset4_AD(28,L) * SECANG(L)
            
	    CJUNKR_AD(L) = CJUNKR_AD(L) + Predictor_Subset4_AD(20,L) + Predictor_Subset4_AD(24,L) * DT(L) &
	                 + POINT_5 * CJUNKR(L)**(-POINT_5) * Predictor_Subset4_AD(25,L) &
			 - CJUNKZ(L) * Predictor_Subset4_AD(26,L) / CJUNKR(L)**TWO &
			 + Predictor_Subset4_AD(29,L) * SECANG(L) 
            			 
            CJUNKS_AD(L) = CJUNKS_AD(L) + Predictor_Subset4_AD(22,L)
    
            CJUNKZ_AD(L) = CJUNKZ_AD(L) + Predictor_Subset4_AD(23,L) + Predictor_Subset4_AD(26,L) / CJUNKR(L)
	    
	    DT_AD(L) = DT_AD(L) + CJUNKA(L) * Predictor_Subset4_AD(21,L) &
	             + CJUNKR(L) * Predictor_Subset4_AD(24,L)
		     
            A_C_AD(L) = A_C_AD(L) + Predictor_Subset4_AD(27,L)
	    
	    
         !  ----------------
         !   Fixed (for FCOW) 
         !  ----------------
			    
            TR_AD(L) = TR_AD(L) + SECANG(L) * Predictor_Subset4_AD(10,L) &
                     + Predictor_Subset4_AD(12,L) + SECANG(L) * SECANG(L) *  Predictor_Subset4_AD(16,L)
            
	    TJUNKS_AD(L) = TJUNKS_AD(L) + SECANG(L) * Predictor_Subset4_AD(11,L) + Predictor_Subset4_AD(13,L) 
            
	    TRZ_AD(L) = TRZ_AD(L) + SECANG(L) * Predictor_Subset4_AD(14,L) &
                      + SECANG(L) * SECANG(L) * Predictor_Subset4_AD(15,L)     
        
         !  ---------------			      
         !  Water continuum (for FWO, FOW, FMW, FCOW)
         !  ---------------
            WJUNKA_AD(L) = WJUNKA_AD(L) + Predictor_Subset4_AD(1,L) / TJUNKS(L)   &
                         + Predictor_Subset4_AD(2,L) * A_W(L) / TJUNKS(L)**2 &
                         + Predictor_Subset4_AD(3,L) / TR(L)  &
                         + Predictor_Subset4_AD(4,L) * A_W(L) / TR(L)  &      
                         + Predictor_Subset4_AD(5,L) * A_W(L) / TJUNKS(L)  & 
                         + Predictor_Subset4_AD(6,L) / TJUNKS(L)**2 &
                         + Predictor_Subset4_AD(7,L) 
            TJUNKS_AD(L) = TJUNKS_AD(L) - WJUNKA(L) * Predictor_Subset4_AD(1,L) / TJUNKS(L)**2 &
                         - WJUNKA(L) * A_W(L) * TWO * Predictor_Subset4_AD(2,L) / TJUNKS(L)**3 &
                         - WJUNKA(L) * A_W(L) * Predictor_Subset4_AD(5,L) / TJUNKS(L)**2 &
                         - TWO * WJUNKA(L) * Predictor_Subset4_AD(6,L) / TJUNKS(L)**3
            A_W_AD(L) = A_W_AD(L) + WJUNKA(L) * Predictor_Subset4_AD(2,L) / TJUNKS(L)**2 &
                      + WJUNKA(L) * Predictor_Subset4_AD(4,L) / TR(L)  &
                      + WJUNKA(L) * Predictor_Subset4_AD(5,L) / TJUNKS(L)          
            TR_AD(L) = TR_AD(L) - WJUNKA(L) * Predictor_Subset4_AD(3,L) / TR(L)**2 &
                     - WJUNKA(L) * A_W(L) * Predictor_Subset4_AD(4,L) / TR(L)**2 
	    
	    Predictor_Subset4_AD(:,L) = ZERO
	   
	   CASE ( 5 )
	   
	    IF( .NOT. Cal_Sun) THEN
	      Predictor_Subset5_AD(:, L) = Predictor_AD%Predictor_Subset(5, :, L) 
            ELSE
	      Predictor_Subset5_AD(:, L) = Predictor_AD%Predictor_Subset_Sun(2, :, L) 
            ENDIF
         !  ----------------
         !  ozone predictors for FWO sun bfsw = set5
         !  ----------------
            OJUNKA_AD(L) = OJUNKA_AD(L) + Predictor_Subset5_AD(22,L)
	    
         !  ----------------
         !  Water predictors for FWO sun bfsw = set5   
         !  ----------------
            WJUNKA_AD(L) = WJUNKA_AD(L) + Predictor_Subset5_AD(19,L) + Predictor_Subset5_AD(21,L) * DT(L) &
                         + Predictor_Subset5_AD(20,L) * WJUNKR(L) 
			    
            WJUNKR_AD(L) = WJUNKR_AD(L) + Predictor_Subset5_AD(20,L) * WJUNKA(L) 
  			     
	    DT_AD(L) = DT_AD(L) + WJUNKA(L) * Predictor_Subset5_AD(21,L) 
	    
         !  ----------------
         !   Fixed  FWO sun bfsw = set5  
         !  ----------------
			    
            TR_AD(L) = TR_AD(L) + SECANG(L) * Predictor_Subset5_AD(10,L) &
                     + Predictor_Subset5_AD(12,L) &
		     - SECANG(L) * TRZ(L) * Predictor_Subset5_AD(15,L) / TR(L)**TWO &
		     + SECANG(L) * SECANG(L) *  Predictor_Subset5_AD(16,L)
            
	    TJUNKS_AD(L) = TJUNKS_AD(L) + SECANG(L) * Predictor_Subset5_AD(11,L) + Predictor_Subset5_AD(13,L) 
            
	    TRZ_AD(L) = TRZ_AD(L) + SECANG(L) * Predictor_Subset5_AD(14,L) &
                      + SECANG(L) * Predictor_Subset5_AD(15,L) / TR(L) + Predictor_Subset5_AD(18,L)     
        
         !  ---------------			      
         !  Water continuum (for FWO, FOW, FMW, FCOW)
         !  ---------------
            WJUNKA_AD(L) = WJUNKA_AD(L) + Predictor_Subset5_AD(1,L) / TJUNKS(L)   &
                         + Predictor_Subset5_AD(2,L) * A_W(L) / TJUNKS(L)**2 &
                         + Predictor_Subset5_AD(3,L) / TR(L)  &
                         + Predictor_Subset5_AD(4,L) * A_W(L) / TR(L)  &      
                         + Predictor_Subset5_AD(5,L) * A_W(L) / TJUNKS(L)  & 
                         + Predictor_Subset5_AD(6,L) / TJUNKS(L)**2 &
                         + Predictor_Subset5_AD(7,L) 
            TJUNKS_AD(L) = TJUNKS_AD(L) - WJUNKA(L) * Predictor_Subset5_AD(1,L) / TJUNKS(L)**2 &
                         - WJUNKA(L) * A_W(L) * TWO * Predictor_Subset5_AD(2,L) / TJUNKS(L)**3 &
                         - WJUNKA(L) * A_W(L) * Predictor_Subset5_AD(5,L) / TJUNKS(L)**2 &
                         - TWO * WJUNKA(L) * Predictor_Subset5_AD(6,L) / TJUNKS(L)**3
            A_W_AD(L) = A_W_AD(L) + WJUNKA(L) * Predictor_Subset5_AD(2,L) / TJUNKS(L)**2 &
                      + WJUNKA(L) * Predictor_Subset5_AD(4,L) / TR(L)  &
                      + WJUNKA(L) * Predictor_Subset5_AD(5,L) / TJUNKS(L)          
            TR_AD(L) = TR_AD(L) - WJUNKA(L) * Predictor_Subset5_AD(3,L) / TR(L)**2 &
                     - WJUNKA(L) * A_W(L) * Predictor_Subset5_AD(4,L) / TR(L)**2 
            
	    Predictor_Subset5_AD(:,L) = ZERO
   
	   CASE ( 6 )

	    IF( .NOT. Cal_Sun) THEN
	      Predictor_Subset6_AD(:, L) = Predictor_AD%Predictor_Subset(6, :, L) 
            ELSE
	      Predictor_Subset6_AD(:, L) = Predictor_AD%Predictor_Subset_Sun(3, :, L) 
            ENDIF
         !  ----------------
         !  ozone predictors for FWO sun mfmw = set6 
         !  ----------------
            OJUNKA_AD(L) = OJUNKA_AD(L) + Predictor_Subset6_AD(23,L)
	    
         !  ----------------
         !  Water predictors for FWO sun mfmw = set6    
         !  ----------------
            WJUNKA_AD(L) = WJUNKA_AD(L) + Predictor_Subset6_AD(16,L) &
                         + Predictor_Subset6_AD(17,L) * WJUNKR(L) &
			 + Predictor_Subset6_AD(18,L) * DT(L) &
			 + Predictor_Subset6_AD(20,L) * WJUNKR(L) * DT(L) &
			 + Predictor_Subset6_AD(21,L) * WJUNKS(L) &
			 + Predictor_Subset6_AD(22,L) * SECANG(L)
			    
            WJUNKR_AD(L) = WJUNKR_AD(L) + Predictor_Subset6_AD(17,L) * WJUNKA(L) &
	                 + WJUNKA(L) * Predictor_Subset6_AD(20,L) * DT(L) 
			 
  	    WJUNKS_AD(L) = WJUNKS_AD(L) + Predictor_Subset6_AD(19,L) +	WJUNKA(L) * Predictor_Subset6_AD(21,L)      
	    
	    DT_AD(L) = DT_AD(L) + WJUNKA(L) * Predictor_Subset6_AD(18,L) &
	             + WJUNKA(L) * WJUNKR(L) * Predictor_Subset6_AD(20,L) 
	    
         
	 !  ----------------
         !   Fixed  FWO sun mfmw = set6   
         !  ----------------
			    
            TR_AD(L) = TR_AD(L) + SECANG(L) * Predictor_Subset6_AD(10,L) &
                     + Predictor_Subset6_AD(12,L)  
		                 
	    TJUNKS_AD(L) = TJUNKS_AD(L) + SECANG(L) * Predictor_Subset6_AD(11,L) + Predictor_Subset6_AD(13,L) 
            
	    TRZ_AD(L) = TRZ_AD(L) + SECANG(L) * Predictor_Subset6_AD(14,L)       
        
         !  ---------------			      
         !  Water continuum (for FWO, FOW, FMW, FCOW)
         !  ---------------
            WJUNKA_AD(L) = WJUNKA_AD(L) + Predictor_Subset6_AD(1,L) / TJUNKS(L)   &
                         + Predictor_Subset6_AD(2,L) * A_W(L) / TJUNKS(L)**2 &
                         + Predictor_Subset6_AD(3,L) / TR(L)  &
                         + Predictor_Subset6_AD(4,L) * A_W(L) / TR(L)  &      
                         + Predictor_Subset6_AD(5,L) * A_W(L) / TJUNKS(L)  & 
                         + Predictor_Subset6_AD(6,L) / TJUNKS(L)**2 &
                         + Predictor_Subset6_AD(7,L) 
            TJUNKS_AD(L) = TJUNKS_AD(L) - WJUNKA(L) * Predictor_Subset6_AD(1,L) / TJUNKS(L)**2 &
                         - WJUNKA(L) * A_W(L) * TWO * Predictor_Subset6_AD(2,L) / TJUNKS(L)**3 &
                         - WJUNKA(L) * A_W(L) * Predictor_Subset6_AD(5,L) / TJUNKS(L)**2 &
                         - TWO * WJUNKA(L) * Predictor_Subset6_AD(6,L) / TJUNKS(L)**3
            A_W_AD(L) = A_W_AD(L) + WJUNKA(L) * Predictor_Subset6_AD(2,L) / TJUNKS(L)**2 &
                      + WJUNKA(L) * Predictor_Subset6_AD(4,L) / TR(L)  &
                      + WJUNKA(L) * Predictor_Subset6_AD(5,L) / TJUNKS(L)          
            TR_AD(L) = TR_AD(L) - WJUNKA(L) * Predictor_Subset6_AD(3,L) / TR(L)**2 &
                     - WJUNKA(L) * A_W(L) * Predictor_Subset6_AD(4,L) / TR(L)**2 
 	    
	    Predictor_Subset6_AD(:,L) = ZERO
	        
	   CASE ( 7 )

	    IF( .NOT. Cal_Sun) THEN
	      Predictor_Subset7_AD(:, L) = Predictor_AD%Predictor_Subset(7, :, L) 
            ELSE
	      Predictor_Subset7_AD(:, L) = Predictor_AD%Predictor_Subset_Sun(4, :, L) 
            ENDIF
	    
         !  ----------------
         !  ozone predictors for FWO sun mfbw = set7  
         !  ----------------
            OJUNKA_AD(L) = OJUNKA_AD(L) + Predictor_Subset7_AD(29,L)
	    
         !  ----------------
         !  Water predictors for FWO sun mfbw = set7     
         !  ----------------
            WJUNKA_AD(L) = WJUNKA_AD(L) + Predictor_Subset7_AD(16,L) &
                         + Predictor_Subset7_AD(17,L) * WJUNKR(L) &
			 + Predictor_Subset7_AD(18,L) * DT(L) &
			 + Predictor_Subset7_AD(20,L) * WJUNKR(L) * DT(L) &
			 + Predictor_Subset7_AD(21,L) * WJUNKS(L) &
			 + Predictor_Subset7_AD(22,L) * SECANG(L) &
			 + Predictor_Subset7_AD(25,L) * WJUNK4(L) &
			 + Predictor_Subset7_AD(26,L) * WJUNKZ(L) &
			 + Predictor_Subset7_AD(27,L) * A_W(L)
			    
            WJUNKR_AD(L) = WJUNKR_AD(L) + Predictor_Subset7_AD(17,L) * WJUNKA(L) &
	                 + WJUNKA(L) * Predictor_Subset7_AD(20,L) * DT(L) &
			 + WJUNKZ(L) * Predictor_Subset7_AD(24,L) 
			 
  	    WJUNKS_AD(L) = WJUNKS_AD(L) + Predictor_Subset7_AD(19,L) +	WJUNKA(L) * Predictor_Subset7_AD(21,L)  &    
	                 + Predictor_Subset7_AD(28,L) / WJUNK4(L)
			 
	    WJUNKZ_AD(L) = WJUNKZ_AD(L) + Predictor_Subset7_AD(23,L) + Predictor_Subset7_AD(24,L) * WJUNKR(L) &
	    		 + WJUNKA(L) * Predictor_Subset7_AD(26,L)
	
	    WJUNK4_AD(L) = WJUNK4_AD(L) + WJUNKA(L) * Predictor_Subset7_AD(25,L) &
	                 - WJUNKS(L) * Predictor_Subset7_AD(28,L) / WJUNK4(L)**TWO		 
			 
	    DT_AD(L) = DT_AD(L) + WJUNKA(L) * Predictor_Subset7_AD(18,L) &
	             + WJUNKA(L) * WJUNKR(L) * Predictor_Subset7_AD(20,L)
		      
	    A_W_AD(L) = A_W_AD(L) + WJUNKA(L) * Predictor_Subset7_AD(27,L)
	    
         
	 !  ----------------
         !   Fixed  FWO sun  mfbw = set7   
         !  ----------------
			    
            TR_AD(L) = TR_AD(L) + SECANG(L) * Predictor_Subset7_AD(10,L) &
                     + Predictor_Subset7_AD(12,L)  
		                 
	    TJUNKS_AD(L) = TJUNKS_AD(L) + SECANG(L) * Predictor_Subset7_AD(11,L) + Predictor_Subset7_AD(13,L) 
            
	    TRZ_AD(L) = TRZ_AD(L) + SECANG(L) * Predictor_Subset7_AD(14,L)       
        
         !  ---------------			      
         !  Water continuum (for FWO, FOW, FMW, FCOW)
         !  ---------------
            WJUNKA_AD(L) = WJUNKA_AD(L) + Predictor_Subset7_AD(1,L) / TJUNKS(L)   &
                         + Predictor_Subset7_AD(2,L) * A_W(L) / TJUNKS(L)**2 &
                         + Predictor_Subset7_AD(3,L) / TR(L)  &
                         + Predictor_Subset7_AD(4,L) * A_W(L) / TR(L)  &      
                         + Predictor_Subset7_AD(5,L) * A_W(L) / TJUNKS(L)  & 
                         + Predictor_Subset7_AD(6,L) / TJUNKS(L)**2 &
                         + Predictor_Subset7_AD(7,L) 
            TJUNKS_AD(L) = TJUNKS_AD(L) - WJUNKA(L) * Predictor_Subset7_AD(1,L) / TJUNKS(L)**2 &
                         - WJUNKA(L) * A_W(L) * TWO * Predictor_Subset7_AD(2,L) / TJUNKS(L)**3 &
                         - WJUNKA(L) * A_W(L) * Predictor_Subset7_AD(5,L) / TJUNKS(L)**2 &
                         - TWO * WJUNKA(L) * Predictor_Subset7_AD(6,L) / TJUNKS(L)**3
            A_W_AD(L) = A_W_AD(L) + WJUNKA(L) * Predictor_Subset7_AD(2,L) / TJUNKS(L)**2 &
                      + WJUNKA(L) * Predictor_Subset7_AD(4,L) / TR(L)  &
                      + WJUNKA(L) * Predictor_Subset7_AD(5,L) / TJUNKS(L)          
            TR_AD(L) = TR_AD(L) - WJUNKA(L) * Predictor_Subset7_AD(3,L) / TR(L)**2 &
                     - WJUNKA(L) * A_W(L) * Predictor_Subset7_AD(4,L) / TR(L)**2 
	    
	    Predictor_Subset7_AD(:,L) = ZERO	     
 	     
	 END SELECT Subset_Select
      !  ------- 	       
      !  Methane for FMW = set3 
      !  ------- 	       
         AZ_M_AD(L) = AZ_M_AD(L) + SECANG(L) * MJUNKZ_AD(L)
 	 MJUNKA_AD(L) = MJUNKA_AD(L) + POINT_5 * MJUNKA(L)**(-POINT_5) * MJUNKR_AD(L)
	 A_M_AD(L) = A_M_AD(L) + SECANG(L) * MJUNKA_AD(L)
	 MJUNKZ_AD(L) = ZERO
	 MJUNKR_AD(L) = ZERO
	 MJUNKA_AD(L) = ZERO
      !  ---------------
      !  Carbon monoxide for FCOW = set4
      !  ---------------
         CJUNKA_AD(L) = CJUNKA_AD(L) + CJUNKZ_AD(L) * A_C(L) / AZ_C(L) &
	              + TWO * CJUNKS_AD(L) * CJUNKA(L) &
		      + POINT_5 * CJUNKA(L)**(-POINT_5) * CJUNKR_AD(L)
	 A_C_AD(L) = A_C_AD(L) + CJUNKA(L) * CJUNKZ_AD(L) / AZ_C(L) + SECANG(L) * CJUNKA_AD(L)
	 AZ_C_AD(L) = AZ_C_AD(L) - CJUNKA(L) * A_C(L) * CJUNKZ_AD(L)  / AZ_C(L)**TWO 
         CJUNKZ_AD(L) = ZERO
	 CJUNKS_AD(L) = ZERO
	 CJUNKR_AD(L) = ZERO
	 CJUNKA_AD(L) = ZERO

      !  -----
      !  Water
      !  -----
         WJUNKR_AD(L) = WJUNKR_AD(L) + POINT_5 * WJUNKR(L)**(-POINT_5) * WJUNK4_AD(L)
	 WJUNK4_AD(L) = ZERO
	 WJUNKA_AD(L) = WJUNKA_AD(L) + WJUNKZ_AD(L) * A_W(L) / AZ_W(L) &
	              + WJUNKS_AD(L) * TWO * WJUNKA(L) &
		      + POINT_5 * WJUNKA(L)**(-POINT_5) * WJUNKR_AD(L)
         A_W_AD(L) = A_W_AD(L) + WJUNKA(L) * WJUNKZ_AD(L) / AZ_W(L) + SECANG(L) * WJUNKA_AD(L)
	 AZ_W_AD(L) = AZ_W_AD(L) - WJUNKA(L) * A_W(L) * WJUNKZ_AD(L) / AZ_W(L)**TWO
	 WJUNKZ_AD(L) = ZERO
	 WJUNKS_AD(L) = ZERO
	 WJUNKR_AD(L) = ZERO
	 WJUNKA_AD(L) = ZERO

       !  -----
       !  Ozone
       !  -----
	 OJUNKA_AD(L) = OJUNKA_AD(L) + OJUNKZ_AD(L)/ XZ_O(L) &
	              + POINT_5 * OJUNKA(L)**(-POINT_5) * OJUNKR_AD(L)
         XZ_O_AD(L) = XZ_O_AD(L) + SECANG(L) *	OJUNKX_AD(L) &
	            - OJUNKA(L) * OJUNKZ_AD(L) / XZ_O(L)**TWO
         A_O_AD(L) = A_O_AD(L) + SECANG(L) * OJUNKA_AD(L)
         OJUNKX_AD(L) = ZERO
	 OJUNKZ_AD(L) = ZERO
	 OJUNKR_AD(L) = ZERO
	 OJUNKA_AD(L) = ZERO		     	       
      !  ----------------
      !   Fixed 
      !  ----------------
         TR_AD(L) = TR_AD(L) + TWO * TR(L) * TJUNKS_AD(L)
         TJUNKS_AD(L) = ZERO
	 
      !  Methane terms
         MZ_AD(L) = MZ_AD(L) + AZ_M_AD(L) / MZREF(L)
	 MZREF_AD(L) = MZREF_AD(L) - MZ(L) * AZ_M_AD(L) / MZREF(L)**TWO
	 MZ_AD(L-1) = MZ_AD(L-1) + MZ_AD(L)  
         PDP_AD(L) = PDP_AD(L) +  MZ_AD(L) * Predictor%Absorber(L,CH4_Index)
	 Predictor_AD%Absorber(L,CH4_Index) = Predictor_AD%Absorber(L,CH4_Index) &
	                         +  PDP(L) *  MZ_AD(L) 
	 MZREF_AD(L-1) = MZREF_AD(L-1) + MZREF_AD(L)			 
   	 PDP_AD(L) = PDP_AD(L) + MZREF_AD(L) * TC%Ref_Profile_Data(9,L)	
	 Predictor_AD%Absorber(L,CH4_Index) = Predictor_AD%Absorber(L,CH4_Index) &
	                         +  A_M_AD(L) / TC%Ref_Profile_Data(9,L)
	 AZ_M_AD(L) = ZERO
	 MZ_AD(L) = ZERO
	 MZREF_AD(L) = ZERO
	 A_M_AD(L) = ZERO			 
	
      !  Carbon monoxide terms
         CZ_AD(L) = CZ_AD(L) + AZ_C_AD(L) / CZREF(L)
	 CZREF_AD(L) = CZREF_AD(L) - CZ(L) * AZ_C_AD(L) / CZREF(L)**TWO
	 CZ_AD(L-1) = CZ_AD(L-1) + CZ_AD(L)  
         PDP_AD(L) = PDP_AD(L) +  CZ_AD(L) * Predictor%Absorber(L,CO_Index) 
	 Predictor_AD%Absorber(L,CO_Index) = Predictor_AD%Absorber(L,CO_Index) &
	                         +  PDP(L) *  CZ_AD(L) 
	 CZREF_AD(L-1) = CZREF_AD(L-1) + CZREF_AD(L)			 
   	 PDP_AD(L) = PDP_AD(L) + CZREF_AD(L) * TC%Ref_Profile_Data(8,L)	
	 Predictor_AD%Absorber(L,CO_Index) = Predictor_AD%Absorber(L,CO_Index) &
	                         +  A_C_AD(L) / TC%Ref_Profile_Data(8,L)
	 AZ_C_AD(L) = ZERO
	 CZ_AD(L) = ZERO
	 CZREF_AD(L) = ZERO
	 A_C_AD(L) = ZERO
				
 	 
      !  Ozone terms						      
         OZ_AD(L) = OZ_AD(L) + AZ_O_AD(L) / OZREF(L)
	 OZREF_AD(L) = OZREF_AD(L) - OZ(L) * AZ_O_AD(L) / OZREF(L)**TWO
	 OZ_AD(L-1) = OZ_AD(L-1) + OZ_AD(L)  
         PDP_AD(L) = PDP_AD(L) +  OZ_AD(L) * Predictor%Absorber(L,O3_Index) 
	 Predictor_AD%Absorber(L,O3_Index) = Predictor_AD%Absorber(L,O3_Index) &
	                         +  PDP(L) *  OZ_AD(L) 
	 OZREF_AD(L-1) = OZREF_AD(L-1) + OZREF_AD(L)			 
   	 PDP_AD(L) = PDP_AD(L) + OZREF_AD(L) * TC%Ref_Profile_Data(7,L)	

         XZ_AD(L) = XZ_AD(L) + XZ_O_AD(L) / XZREF(L)
	 XZREF_AD(L) = XZREF_AD(L) - XZ(L) * XZ_O_AD(L) / XZREF(L)**TWO
	 XZ_AD(L-1) = XZ_AD(L-1) + XZ_AD(L)  
	 Predictor_AD%Absorber(L,O3_Index) = Predictor_AD%Absorber(L,O3_Index) + XZ_AD(L) 
	 XZREF_AD(L-1) = XZREF_AD(L-1) + XZREF_AD(L)			 
	 Predictor_AD%Absorber(L,O3_Index) = Predictor_AD%Absorber(L,O3_Index) &
	                         +  A_O_AD(L) / TC%Ref_Profile_Data(7,L)
	 AZ_O_AD(L) = ZERO
	 OZ_AD(L) = ZERO
	 OZREF_AD(L) = ZERO
	 XZ_O_AD(L) = ZERO
	 XZ_AD(L) = ZERO
	 XZREF_AD(L) = ZERO
	 A_O_AD(L) = ZERO			   
	 
      !  Water terms						      
         WZ_AD(L) = WZ_AD(L) + AZ_W_AD(L) / WZREF(L)
	 WZREF_AD(L) = WZREF_AD(L) - WZ(L) * AZ_W_AD(L) / WZREF(L)**TWO
	 WZ_AD(L-1) = WZ_AD(L-1) + WZ_AD(L)  
         PDP_AD(L) = PDP_AD(L) +  WZ_AD(L) * Predictor%Absorber(L,H2O_Index) 
	 Predictor_AD%Absorber(L,H2O_Index) = Predictor_AD%Absorber(L,H2O_Index) &
	                         +  PDP(L) *  WZ_AD(L) 
	 WZREF_AD(L-1) = WZREF_AD(L-1) + WZREF_AD(L)			 
   	 PDP_AD(L) = PDP_AD(L) + WZREF_AD(L) * TC%Ref_Profile_Data(6,L)	
	 Predictor_AD%Absorber(L,H2O_Index) = Predictor_AD%Absorber(L,H2O_Index) &
	                         +  A_W_AD(L) / TC%Ref_Profile_Data(6,L)
	 AZ_W_AD(L) = ZERO
	 WZ_AD(L) = ZERO
	 WZREF_AD(L) = ZERO
	 A_W_AD(L) = ZERO			 
 
 
      !	 Total secant
 	 
	 Predictor_AD%Temperature(L) = Predictor_AD%Temperature(L) &
	             + TR_AD(L) / TC%Ref_Profile_Data(4,L) + DT_AD(L)	
         
	 TR_AD(L) = ZERO
	 DT_AD(L) = ZERO		     		 		   
         
	 IF (L == 1 ) THEN
	 
	    TAZ_M_AD(L) = ZERO
	    TAZ_O_AD(L) = ZERO
	    TRZ_AD(L) = ZERO
	    PDP_AD(L) = ZERO
	 
	 ELSE
	    
	    TMZ_AD(L) = TMZ_AD(L) + TAZ_M_AD(L) / PNORM(L)
	    PNORM_AD(L) = PNORM_AD(L) - TMZ(L) * TAZ_M_AD(L) / PNORM(L)**TWO
	    TMZ_AD(L-1) = TMZ_AD(L-1) + TMZ_AD(L)
	    PDP_AD(L) = PDP_AD(L) +  TMZ_AD(L) * TR(L-1) * A_M(L-1)
	    TR_AD(L-1) = TR_AD(L-1) + PDP(L) * TMZ_AD(L) * A_M(L-1)
	    A_M_AD(L-1) = A_M_AD(L-1) + PDP(L) * TR(L-1) * TMZ_AD(L) 
	    
	    TAZ_M_AD(L) = ZERO
 	    TMZ_AD(L) = ZERO
	    
	    TOZ_AD(L) = TOZ_AD(L) + TAZ_O_AD(L) / PNORM(L)
	    PNORM_AD(L) = PNORM_AD(L) - TOZ(L) * TAZ_O_AD(L) / PNORM(L)**TWO
	    TOZ_AD(L-1) = TOZ_AD(L-1) + TOZ_AD(L)
	    PDP_AD(L) = PDP_AD(L) +  TOZ_AD(L) * DT(L-1) * A_O(L-1)
	    DT_AD(L-1) = DT_AD(L-1) + PDP(L) * TOZ_AD(L) * A_O(L-1)
	    A_O_AD(L-1) = A_O_AD(L-1) + PDP(L) * DT(L-1) * TOZ_AD(L) 
	    
	    TAZ_O_AD(L) = ZERO
	    TOZ_AD(L) = ZERO
	     
	    TZ_AD(L) = TZ_AD(L) + TRZ_AD(L) / PNORM(L)
	    PNORM_AD(L) = PNORM_AD(L) - TZ(L) * TRZ_AD(L) / PNORM(L)**TWO
	    TZ_AD(L-1) = TZ_AD(L-1) + TZ_AD(L)
	    PDP_AD(L) = PDP_AD(L) +  TZ_AD(L) * TR(L-1) 
	    TR_AD(L-1) = TR_AD(L-1) + PDP(L) * TZ_AD(L)
   
            TRZ_AD(L) = ZERO
	    TZ_AD(L) = ZERO
	    
	    PNORM_AD(L-1) = PNORM_AD(L-1) + PNORM_AD(L)
	    PDP_AD(L) = PDP_AD(L) + PNORM_AD(L) 
	    PNORM_AD(L) = ZERO
	    PDP_AD(L) = ZERO  
	    
	 ENDIF

    END DO Adjoint_Layer_Loop
    
    NULLIFY(TC)

  END SUBROUTINE  Compute_Predictors_Subset_AD



  ! Trace gases channel predictors 
  SUBROUTINE  Compute_TraceGas_Predictors( Sensor_Index,       &  ! Input
                                           Predictor,      &  ! In/Output
                                           Calc_Sun_Angle_Secant) ! Optional Input 

    ! ---------
    ! Arguments
    ! ---------
    ! -- Inputs
    INTEGER, INTENT(IN)  :: Sensor_Index 
    ! -- In/Outputs
    TYPE( Predictor_type ), INTENT( IN OUT ) :: Predictor

    ! -- Optional Inputs
    INTEGER,               OPTIONAL, INTENT( IN )     :: Calc_Sun_Angle_Secant 

    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_TraceGas_Predictors'

    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER :: L
    REAL( fp_kind ) :: TR
    REAL( fp_kind ) :: TJUNKS
    			    
    REAL( fp_kind ), DIMENSION(MAX_N_TRACEGASES_PREDICTORS,Predictor%n_Layers) :: TraceGas_Predictors
    REAL( fp_kind ), DIMENSION(MAX_N_ODCAPS_LAYERS) :: SECANG
    LOGICAL :: Cal_Sun
    TYPE( ODCAPS_TauCoeff_type ), POINTER  ::  TC => NULL()

    TC => ODCAPS_TC(Sensor_Index)
 
    Cal_Sun = .True.
        
    Layer_Loop : DO L = 1, Predictor%n_Layers 

       IF ( PRESENT (Calc_Sun_Angle_Secant) .AND. &	    
    	    Predictor%Calc_Sun_Angle_Secant) THEN	    
 
    !  Total secant					    
    	 SECANG(L) = Predictor%Secant_Source_Zenith(L)  
    	 Cal_Sun = .TRUE.				    
       ELSE						    
    	 SECANG(L) = Predictor%Secant_Sensor_Zenith(L)  
    	 Cal_Sun = .FALSE. 				    
       END IF						    
       
       TR = Predictor%Temperature(L) / TC%Ref_Profile_Data(4,L)					      
       TJUNKS = TR * TR
    !  ---------------  					  
    !  trace gas perturbation predictors			  
    !  ---------------  					  
    !  The first 4 trace predictors are used by all trace gases   
       TraceGas_Predictors(1,L) = SECANG(L) 					  
       TraceGas_Predictors(2,L) = TR						  
       TraceGas_Predictors(3,L) = SECANG(L) *TR 				  
       TraceGas_Predictors(4,L) = SECANG(L) *TJUNKS				  
    !  The last 3 trace predictors are only used by N2O 	  
       TraceGas_Predictors(5,L) = SECANG(L) &
                                  *SECANG(L) 			  
       TraceGas_Predictors(6,L) = ONE  					  
       TraceGas_Predictors(7,L) = SQRT( SECANG(L))				  
   
       IF( .NOT. Cal_Sun) THEN
	 Predictor%TraceGas_Predictors(:, L) = TraceGas_Predictors(:, L)
       ELSE
	 Predictor%TraceGas_Predictors_Sun(:, L) = TraceGas_Predictors(:, L)
       ENDIF

    END DO Layer_Loop
    
    NULLIFY(TC)   
  END SUBROUTINE  Compute_TraceGas_Predictors 


  ! Trace gases tangent-linear channel predictors 
  SUBROUTINE  Compute_TraceGas_Predictors_TL( Sensor_Index,       &  ! Input
                                              Predictor,      &  ! Input
                                              Predictor_TL,   &  ! In/Output
                                           Calc_Sun_Angle_Secant) ! Optional Input 

    ! ---------
    ! Arguments
    ! ---------
    ! -- Inputs
    INTEGER,                          INTENT(IN )  :: Sensor_Index
    TYPE( Predictor_type ), INTENT( IN ) :: Predictor

    ! -- In/Outputs
    TYPE( Predictor_type ), INTENT( IN OUT ) :: Predictor_TL

    ! -- Optional Inputs
    INTEGER,               OPTIONAL, INTENT( IN )     :: Calc_Sun_Angle_Secant 

    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_TraceGas_Predictors_TL'

    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER :: L
    REAL( fp_kind ) :: TR, TR_TL
    REAL( fp_kind ) :: TJUNKS, TJUNKS_TL
    			    
    REAL( fp_kind ), DIMENSION(MAX_N_TRACEGASES_PREDICTORS,Predictor%n_Layers) :: TraceGas_Predictors_TL
    REAL( fp_kind ), DIMENSION(MAX_N_ODCAPS_LAYERS) :: SECANG, SECANG_TL
    LOGICAL :: Cal_Sun
    TYPE( ODCAPS_TauCoeff_type ), POINTER  ::  TC => NULL()

    TC => ODCAPS_TC(Sensor_Index)
 
    Cal_Sun = .True.
        
    Layer_Loop : DO L = 1, Predictor%n_Layers 

       IF ( PRESENT (Calc_Sun_Angle_Secant) .AND. &	    
    	    Predictor%Calc_Sun_Angle_Secant) THEN	    
 
    !  Total secant					    
    	 SECANG(L) = Predictor%Secant_Source_Zenith(L)
    	 Cal_Sun = .TRUE.				    
       ELSE						    
    	 SECANG(L) = Predictor%Secant_Sensor_Zenith(L) 
    	 Cal_Sun = .FALSE. 				    
       END IF						    
       
       TR = Predictor%Temperature(L) / TC%Ref_Profile_Data(4,L)
       TR_TL = Predictor_TL%Temperature(L) / TC%Ref_Profile_Data(4,L)				      
       
       TJUNKS = TR * TR
       TJUNKS_TL = TWO * TR * TR_TL
    !  ---------------  					  
    !  trace gas perturbation predictors			  
    !  ---------------  					  
    !  The first 4 trace predictors are used by all trace gases   
       TraceGas_Predictors_TL(1,L) = ZERO 					  
       TraceGas_Predictors_TL(2,L) = TR_TL						  
       TraceGas_Predictors_TL(3,L) = TR_TL * SECANG(L) 				  
       TraceGas_Predictors_TL(4,L) = TJUNKS_TL * SECANG(L)				  
    !  The last 3 trace predictors are only used by N2O 	  
       TraceGas_Predictors_TL(5,L) = ZERO 			  
       TraceGas_Predictors_TL(6,L) = ZERO  					  
       TraceGas_Predictors_TL(7,L) = ZERO				  
   
       IF( .NOT. Cal_Sun) THEN
	 Predictor_TL%TraceGas_Predictors(:, L) = TraceGas_Predictors_TL(:, L)
       ELSE
	 Predictor_TL%TraceGas_Predictors_Sun(:, L) = TraceGas_Predictors_TL(:, L)
       ENDIF

    END DO Layer_Loop
    
    NULLIFY(TC)
       
  END SUBROUTINE  Compute_TraceGas_Predictors_TL 

  ! Trace gases adjoint channel predictors 
  SUBROUTINE  Compute_TraceGas_Predictors_AD( Sensor_Index,           &  ! Input
                                                 Predictor,      &  ! Input
                                                 Predictor_AD,   &  ! In/Output
                                           Calc_Sun_Angle_Secant) ! Optional Input 

    ! ---------
    ! Arguments
    ! ---------
    ! -- Inputs
    INTEGER,                          INTENT( IN ) :: Sensor_Index
    TYPE( Predictor_type ), INTENT( IN ) :: Predictor

    ! -- In/Outputs
    TYPE( Predictor_type ), INTENT( IN OUT ) :: Predictor_AD

    ! -- Optional Inputs
    INTEGER,               OPTIONAL, INTENT( IN )     :: Calc_Sun_Angle_Secant 

    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_TraceGas_Predictors_AD'

    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER :: L
    REAL( fp_kind ) :: TR, TR_AD
    REAL( fp_kind ) :: TJUNKS, TJUNKS_AD
    			    
    REAL( fp_kind ), DIMENSION(MAX_N_TRACEGASES_PREDICTORS,Predictor%n_Layers) :: TraceGas_Predictors_AD
    REAL( fp_kind ), DIMENSION(MAX_N_ODCAPS_LAYERS) :: SECANG, SECANG_AD
    LOGICAL :: Cal_Sun
    TYPE( ODCAPS_TauCoeff_type ), POINTER  ::  TC => NULL()

    TC => ODCAPS_TC(Sensor_Index)
 
    Cal_Sun = .True.
    
    Layer_Loop : DO L = 1, Predictor%n_Layers 

       IF ( PRESENT (Calc_Sun_Angle_Secant) .AND. &	    
    	    Predictor%Calc_Sun_Angle_Secant) THEN	    
 
    !  Total secant					    
    	 SECANG(L) = Predictor%Secant_Source_Zenith(L)
    	 Cal_Sun = .TRUE.				    
       ELSE						    
    	 SECANG(L) = Predictor%Secant_Sensor_Zenith(L) 
    	 Cal_Sun = .FALSE. 				    
       END IF	
       					    
       TR = Predictor%Temperature(L) / TC%Ref_Profile_Data(4,L)
       TJUNKS = TR * TR
 
       IF( .NOT. Cal_Sun) THEN
	 TraceGas_Predictors_AD(:, L) = Predictor_AD%TraceGas_Predictors(:, L)  
       ELSE
	 TraceGas_Predictors_AD(:, L) = Predictor_AD%TraceGas_Predictors_Sun(:, L)  
       ENDIF
       
       TJUNKS_AD = SECANG(L) * TraceGas_Predictors_AD(4,L)
       
       TR_AD = TWO * TR * TJUNKS_AD + TraceGas_Predictors_AD(2,L) &
               + SECANG(L) * TraceGas_Predictors_AD(3,L)
       
       Predictor_AD%Temperature(L) = Predictor_AD%Temperature(L) &
               + TR_AD / TC%Ref_Profile_Data(4,L)
       
       TJUNKS_AD = ZERO
       TR_AD = ZERO	
       TraceGas_Predictors_AD(:,L) = ZERO       
       
       	      
    END DO Layer_Loop
    
    NULLIFY(TC)
        
  END SUBROUTINE  Compute_TraceGas_Predictors_AD 

   
  ! Water vapor OPTRAN channel predictors 
  SUBROUTINE  Compute_WOPTRAN_Predictors( Sensor_Index,   &  ! Input
                                          Predictor )    ! In/Output

    ! ---------
    ! Arguments
    ! ---------
    ! -- Inputs
    INTEGER,                         INTENT( IN )     :: Sensor_Index
    ! -- In/Outputs
    TYPE( Predictor_type ), INTENT( IN OUT ) :: Predictor

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_WOPTRAN_Predictors'

    ! ---------------
    ! Local variables
    ! ---------------
    REAL(fp_kind), DIMENSION(MAX_N_ODCAPS_LAYERS) :: WAANG
    INTEGER :: LOPMIN
    INTEGER :: LOPMAX
    REAL(fp_kind), DIMENSION( MAX_N_WATER_OPTRAN_PREDICTORS,MAX_N_WATER_OPTRAN_LAYERS) :: Optran_Water_Predictors
    LOGICAL, DIMENSION(MAX_N_WATER_OPTRAN_LAYERS)  :: LOPUSE 
    INTEGER, DIMENSION(MAX_N_ODCAPS_LAYERS) :: LOPLOW 
    REAL(fp_kind), DIMENSION(MAX_N_ODCAPS_LAYERS)  :: DAOP 
    
    INTEGER :: L			    
    INTEGER ::	LL	  
    INTEGER ::  LOP	  
    INTEGER ::  LOPL	  
    INTEGER ::  LOPU	  
    INTEGER ::	LU	  
    REAL(fp_kind), DIMENSION(MAX_N_ODCAPS_LAYERS) ::     WAZ   
    REAL(fp_kind), DIMENSION(MAX_N_ODCAPS_LAYERS) ::     PZ    
    REAL(fp_kind), DIMENSION(MAX_N_ODCAPS_LAYERS) ::     TZ   
    REAL(fp_kind) :: WAZSUM 	  
    REAL(fp_kind) :: WPZSUM 	  
    REAL(fp_kind) :: WTZSUM 	  
    REAL(fp_kind) ::     DA 	  
    REAL(fp_kind) ::    POP 	  
    REAL(fp_kind) ::    TOP 	  
    REAL(fp_kind) ::   PZOP 	  
    REAL(fp_kind) ::   TZOP 	  
    REAL(fp_kind) ::  ANGOP 	  
    LOGICAL ::  LAST	  
    INTEGER :: H2O_Index		    
    TYPE( ODCAPS_TauCoeff_type ), POINTER  ::  TC => NULL()

    TC => ODCAPS_TC(Sensor_Index)
    
    !#--------------------------------------------------------------------------#
    !#              -- GET THE Atmosphere GASES INDEX  --              #
    !#--------------------------------------------------------------------------#
    H2O_Index = MINLOC( ABS( Predictor%Absorber_ID - H2O_ID ), DIM = 1 )


    !  Initialize amount above sums
    WAZSUM = ZERO
    WPZSUM = ZERO 
    WTZSUM = ZERO 

   !   ---------------------------------------
   !   Calculate raw predictors for all layers
   !   ---------------------------------------
    Layer_Loop : DO L = 1, Predictor%n_Layers 

   !  Layer amount*angle
      WAANG(L) = Predictor%Absorber(L, H2O_Index) * Predictor%Secant_Sensor_Zenith(L)
 
   !  Center-of-layer-to-space amount*angle
   !  Note: do this before updating AZSUM
      WAZ(L)= POINT_5 * WAANG(L) + WAZSUM

   !  Bottom-of-layer-to-space amount sum
      WAZSUM = WAANG(L) + WAZSUM

   !  Pressure above sum
      WPZSUM = WAANG(L)* TC%Ref_Profile_Data(3, L)  + WPZSUM
      PZ(L)= WPZSUM/WAZSUM
 
   !  Temperature above sum
      WTZSUM = WAANG(L)* Predictor%Temperature(L) + WTZSUM
      TZ(L) = WTZSUM/WAZSUM
       
    END DO Layer_Loop


   !  --------------------------------------------------
   !  Find the max OPTRAN level that is less than WAZ(1)
   !  --------------------------------------------------
       LOPMIN = 1
       LOPMAX = 1
    30 IF (TC%ODCAPS_ODAS%Water_Amount(LOPMIN+1) < WAZ(1)) THEN
          LOPMIN=LOPMIN + 1
          GOTO 30
       ENDIF

   !  Initialize the upper and lower (pressure) layer index
       LL=1
       LU=2
       LAST=.FALSE.

   !  ----------------------------------------
   !   Loop over the OPTRAN layers (while loop)
   !  ----------------------------------------
       LOP = LOPMIN
   10  IF (LOP <= MAX_N_WATER_OPTRAN_LAYERS) THEN

   !  --------------------------------------------------------
   !   Find the two pressure layers closest to the OPTRAN layer
   !  --------------------------------------------------------
   20     IF ( WAZ(LU) < TC%ODCAPS_ODAS%Water_Amount(LOP)) THEN
             IF (LU < Predictor%n_Layers ) THEN
                LL=LU
                LU=LU + 1
                GOTO 20
             ELSE
                LAST=.TRUE.
             ENDIF
          ENDIF
 
   !     Compute the interpolation fractor
          DA = ( TC%ODCAPS_ODAS%Water_Amount( LOP ) - WAZ( LL )) &
	        / ( WAZ( LU ) - WAZ( LL ) )
   
   !     Do the interpolation
          POP   = ( DA*( TC%Ref_Profile_Data(3, LU) -  TC%Ref_Profile_Data(3, LL) )  & 
	           + TC%Ref_Profile_Data(3, LL) ) &
		    / TC%ODCAPS_ODAS%Water_ProfAve(1,LOP)
          TOP   = ( DA*( Predictor%Temperature(LU) -  Predictor%Temperature(LL) ) & 
	           + Predictor%Temperature(LL) ) &
		   / TC%ODCAPS_ODAS%Water_ProfAve(2,LOP)
          PZOP  = ( DA*( PZ(LU) - PZ(LL) ) + PZ(LL) ) &
	           / TC%ODCAPS_ODAS%Water_ProfAve(3,LOP)
          TZOP  = ( DA*( TZ(LU) - TZ(LL) ) + TZ(LL) ) &
	           / TC%ODCAPS_ODAS%Water_ProfAve(4,LOP)
          ANGOP = DA*( Predictor%Secant_Sensor_Zenith(LU)  &
	          - Predictor%Secant_Sensor_Zenith(LL) ) &
		  + Predictor%Secant_Sensor_Zenith(LL)


    !     Assign the predictors
          Optran_Water_Predictors(1,LOP) = ONE      
          Optran_Water_Predictors(2,LOP) = POP         
          Optran_Water_Predictors(3,LOP) = TOP         
          Optran_Water_Predictors(4,LOP) = SQRT( POP ) 
          Optran_Water_Predictors(5,LOP) = TOP*TOP      
          Optran_Water_Predictors(6,LOP) = POP*TOP     
          Optran_Water_Predictors(7,LOP) = ANGOP       
          Optran_Water_Predictors(8,LOP) = PZOP        
          Optran_Water_Predictors(9,LOP) = TZOP        
 
    !     Update LOP and loop
          IF ( LAST )  THEN
             LOPMAX = LOP

    !     Set LOP > MXOWLY to exit loop over LOP
             LOP = MAX_N_WATER_OPTRAN_LAYERS + 1
          ELSE
             LOP = LOP + 1
          ENDIF
          GOTO 10

       ENDIF
    !  End while loop over LOP

    !------------------------------------------------------------------------------------------------------------------------
    ! There is a bug for the orginal SARTA program, what if LAST=.FALSE. and LOP >  MAX_N_WATER_OPTRAN_LAYERS
    ! for the case input atmospheric profile have large water amounts which are exceed the  TC%ODCAPS_ODAS%Water_Amount?
    ! the LOPMAX is undefine
    ! In this case, we should let the LOPMAX = MAX_N_WATER_OPTRAN_LAYERS
    !------------------------------------------------------------------------------------------------------------------------
       IF ( .NOT. LAST  .AND. LOP > MAX_N_WATER_OPTRAN_LAYERS) THEN
        LOPMAX = MAX_N_WATER_OPTRAN_LAYERS
       ENDIF

    !  -----------------
    !  Initialize LOPUSE
    !  -----------------
       DO LOP = 1, MAX_N_WATER_OPTRAN_LAYERS 
          LOPUSE( LOP )= .FALSE.
       ENDDO

   !   ---------------------------------------
   !   Determine what OPTRAN layers are needed
   !   ---------------------------------------
   !   Initialize LOPL and LOPU
       LOPL = LOPMIN
       LOPU = LOPMIN + 1

   !   Loop over the AIRS pressure layers
       DO L=1, Predictor%n_Layers
   
   !   Find the two OPTRAN levels that bracket the AIRS layer
    40    IF ( TC%ODCAPS_ODAS%Water_Amount(LOPU) < WAZ(L) .AND. LOPU < LOPMAX) THEN
             LOPL = LOPU
             LOPU = LOPU + 1
             GOTO 40
          ENDIF

          LOPUSE(LOPL) = .TRUE.
          LOPUSE(LOPU) = .TRUE.
    
    !     Assign the lower OPTRAN level
          LOPLOW(L) = LOPL
    
    !     Assign the interpolation fraction
          DAOP(L)=(WAZ(L) - TC%ODCAPS_ODAS%Water_Amount(LOPL)) &
	          / (TC%ODCAPS_ODAS%Water_Amount(LOPU)  &
		  - TC%ODCAPS_ODAS%Water_Amount(LOPL))
       ENDDO
       
    !  OPTRAN absorption coefficient scaling factor Water_OPTRAN_Scaling  
       Predictor%Water_OPTRAN_Scaling( 1 ) = TC%ODCAPS_ODAS%Water_Amount(1)
       Predictor%Water_OPTRAN_Scaling( 2: MAX_N_WATER_OPTRAN_LAYERS ) = &
                TC%ODCAPS_ODAS%Water_Amount( 2: MAX_N_WATER_OPTRAN_LAYERS ) - &
                TC%ODCAPS_ODAS%Water_Amount( 1: MAX_N_WATER_OPTRAN_LAYERS-1 )
     
    !  Water amount in profile layer		
       Predictor%Layer_Water_Amount( 1:Predictor%n_Layers ) = WAANG( 1:Predictor%n_Layers )
       
    !  min OPTRAN level to use 
       Predictor%Min_OPTRAN_Level = LOPMIN
        
    !  max OPTRAN level to use 
       Predictor%Max_OPTRAN_Level = LOPMAX
     
    !  OPTRAN level needed?
       Predictor%OPTRAN_Level_Use = LOPUSE
        
    !  OPTRAN predictors
       Predictor%Optran_Water_Predictors(:,LOPMIN:LOPMAX ) = Optran_Water_Predictors(:,LOPMIN:LOPMAX )
        
    !  low bracketing OPTRAN lev
       Predictor%Lower_OPTRAN_Level = LOPLOW
        
    !  OPTRAN-to-AIRS interp frac
       Predictor%OPTRAN_Interp_Frac = DAOP   
  
       NULLIFY(TC)
  END SUBROUTINE  Compute_WOPTRAN_Predictors 

  ! Water vapor OPTRAN channel tangent-linear predictors 
  SUBROUTINE  Compute_WOPTRAN_Predictors_TL( Sensor_Index,      &  ! Input
                                             Predictor,     &  ! Input
                                             Predictor_TL )    ! In/Output

    ! ---------
    ! Arguments
    ! ---------
    ! -- Input
    INTEGER,                          INTENT( IN )  :: Sensor_Index
    TYPE( Predictor_type ), INTENT( IN )  :: Predictor

    ! -- In/Output
    TYPE( Predictor_type ), INTENT( IN OUT ) :: Predictor_TL

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_WOPTRAN_Predictors_TL'

    ! ---------------
    ! Local variables
    ! ---------------
    REAL(fp_kind), DIMENSION(MAX_N_ODCAPS_LAYERS) :: WAANG
    REAL(fp_kind), DIMENSION(MAX_N_ODCAPS_LAYERS) :: WAANG_TL
    REAL(fp_kind), DIMENSION( MAX_N_WATER_OPTRAN_PREDICTORS,MAX_N_WATER_OPTRAN_LAYERS) :: Optran_Water_Predictors_TL
    REAL(fp_kind), DIMENSION(MAX_N_ODCAPS_LAYERS)  :: DAOP_TL 
    
    INTEGER ::  L			    
    INTEGER ::	LL	  
    INTEGER ::  LOPL	  
    INTEGER ::  LOPU	  
    INTEGER ::	LU	  
    REAL(fp_kind), DIMENSION(MAX_N_ODCAPS_LAYERS) ::     WAZ, WAZ_TL   
    REAL(fp_kind), DIMENSION(MAX_N_ODCAPS_LAYERS) ::     PZ, PZ_TL    
    REAL(fp_kind), DIMENSION(MAX_N_ODCAPS_LAYERS) ::     TZ, TZ_TL   
    REAL(fp_kind) :: WAZSUM,  WAZSUM_TL	  
    REAL(fp_kind) :: WPZSUM,  WPZSUM_TL	  
    REAL(fp_kind) :: WTZSUM,  WTZSUM_TL	  
    REAL(fp_kind) ::     DA,  DA_TL 	  
    REAL(fp_kind) ::    POP,  POP_TL 	  
    REAL(fp_kind) ::    TOP,  TOP_TL 	  
    INTEGER :: H2O_Index		    
    TYPE( ODCAPS_TauCoeff_type ), POINTER  ::  TC => NULL()

    TC => ODCAPS_TC(Sensor_Index)
    
    !#--------------------------------------------------------------------------#
    !#              -- GET THE Atmosphere GASES INDEX  --              #
    !#--------------------------------------------------------------------------#
    H2O_Index = MINLOC( ABS( Predictor%Absorber_ID - H2O_ID ), DIM = 1 )


    !  Initialize amount above sums
    WAZSUM = ZERO
    WAZSUM_TL = ZERO
    
    WPZSUM = ZERO 
    WPZSUM_TL = ZERO 

    WTZSUM = ZERO 
    WTZSUM_TL = ZERO 

   !   ---------------------------------------
   !   Calculate raw predictors for all layers
   !   ---------------------------------------
    Layer_Loop : DO L = 1, Predictor%n_Layers 

   !  Layer amount*angle
      WAANG(L) = Predictor%Absorber(L, H2O_Index) * Predictor%Secant_Sensor_Zenith(L)
      WAANG_TL(L) = Predictor_TL%Absorber(L, H2O_Index) * Predictor%Secant_Sensor_Zenith(L)  
    
   !  Center-of-layer-to-space amount*angle
   !  Note: do this before updating AZSUM
      WAZ(L)= POINT_5 * WAANG(L) + WAZSUM
      WAZ_TL(L)= POINT_5 * WAANG_TL(L) + WAZSUM_TL
      
   !  Bottom-of-layer-to-space amount sum
      WAZSUM = WAANG(L) + WAZSUM
      WAZSUM_TL = WAANG_TL(L) + WAZSUM_TL

   !  Pressure above sum
      WPZSUM = WAANG(L)* TC%Ref_Profile_Data(3, L)  + WPZSUM
      PZ(L)= WPZSUM/WAZSUM

      WPZSUM_TL = WAANG_TL(L) * TC%Ref_Profile_Data(3, L)  + WPZSUM_TL
      PZ_TL(L)= WPZSUM_TL/WAZSUM - WPZSUM * WAZSUM_TL/ WAZSUM**TWO  
 
   !  Temperature above sum
      WTZSUM = WAANG(L)* Predictor%Temperature(L) + WTZSUM
      TZ(L) = WTZSUM/WAZSUM

      WTZSUM_TL = WAANG_TL(L) * Predictor%Temperature(L) + WTZSUM_TL &
                + WAANG(L)* Predictor_TL%Temperature(L)   
      TZ_TL(L) = WTZSUM_TL/WAZSUM - WTZSUM * WAZSUM_TL/ WAZSUM**TWO
      
        
    END DO Layer_Loop

   !  Initialize the upper and lower (pressure) layer index
       LL=1
       LU=2
  
    OPTRAN_Loop : DO L = Predictor%Min_OPTRAN_Level, Predictor%Max_OPTRAN_Level
   !  --------------------------------------------------------
   !   Find the two pressure layers closest to the OPTRAN layer
   !  --------------------------------------------------------
   20     IF ( WAZ(LU) < TC%ODCAPS_ODAS%Water_Amount(L)) THEN
             IF (LU < Predictor%n_Layers ) THEN
                LL=LU
                LU=LU + 1
                GOTO 20
             ENDIF
          ENDIF
   !     Compute the interpolation fractor
          DA = ( TC%ODCAPS_ODAS%Water_Amount( L ) - WAZ( LL )) &
	        / ( WAZ( LU ) - WAZ( LL ) )
          DA_TL = -( TC%ODCAPS_ODAS%Water_Amount( L ) - WAZ( LL )) &
	        * ( WAZ_TL( LU ) - WAZ_TL( LL ) ) / ( WAZ( LU ) - WAZ( LL ) )**TWO &
		- WAZ_TL( LL ) / ( WAZ( LU ) - WAZ( LL ) )

   !     Do the interpolation
          POP   = ( DA*( TC%Ref_Profile_Data(3, LU) -  TC%Ref_Profile_Data(3, LL) )  & 
	           + TC%Ref_Profile_Data(3, LL) ) &
		    / TC%ODCAPS_ODAS%Water_ProfAve(1,L)
          POP_TL = DA_TL * ( TC%Ref_Profile_Data(3, LU) -  TC%Ref_Profile_Data(3, LL) ) &
		    / TC%ODCAPS_ODAS%Water_ProfAve(1,L)
          TOP   = ( DA*( Predictor%Temperature(LU) -  Predictor%Temperature(LL) ) & 
	           + Predictor%Temperature(LL) ) &
		   / TC%ODCAPS_ODAS%Water_ProfAve(2,L)
	  TOP_TL =  ( DA_TL *( Predictor%Temperature(LU) -  Predictor%Temperature(LL) ) &
	           + DA*( Predictor_TL%Temperature(LU) -  Predictor_TL%Temperature(LL) ) &
	           + Predictor_TL%Temperature(LL) ) / TC%ODCAPS_ODAS%Water_ProfAve(2,L)  

    !     Assign the predictors
          Optran_Water_Predictors_TL(1,L) = ZERO      
          Optran_Water_Predictors_TL(2,L) = POP_TL         
          Optran_Water_Predictors_TL(3,L) = TOP_TL         
          Optran_Water_Predictors_TL(4,L) = POINT_5 * (POP**(-POINT_5)) * POP_TL 
          Optran_Water_Predictors_TL(5,L) = TWO*TOP*TOP_TL      
          Optran_Water_Predictors_TL(6,L) = POP_TL*TOP + POP*TOP_TL       
          Optran_Water_Predictors_TL(7,L) = DA_TL*( Predictor%Secant_Sensor_Zenith(LU) &
	                                           -  Predictor%Secant_Sensor_Zenith(LL))	        
          Optran_Water_Predictors_TL(8,L) = ( DA*( PZ_TL(LU) - PZ_TL(LL) ) + PZ_TL(LL) &
	                                    + DA_TL *(PZ(LU) - PZ(LL)) ) &
	                                    / TC%ODCAPS_ODAS%Water_ProfAve(3,L)         
          Optran_Water_Predictors_TL(9,L) =  ( DA*( TZ_TL(LU) - TZ_TL(LL) ) + TZ_TL(LL) &
	                                    + DA_TL *(TZ(LU) - TZ(LL)) ) &
	                                    / TC%ODCAPS_ODAS%Water_ProfAve(4,L)         
  

    END DO OPTRAN_Loop

   !   ---------------------------------------
   !   Determine what OPTRAN layers are needed
   !   ---------------------------------------
   !   Initialize LOPL and LOPU
       LOPL = Predictor%Min_OPTRAN_Level
       LOPU = Predictor%Min_OPTRAN_Level + 1

   !   Loop over the AIRS pressure layers
       DO L=1, Predictor%n_Layers
   
   !   Find the two OPTRAN levels that bracket the AIRS layer
    40    IF ( TC%ODCAPS_ODAS%Water_Amount(LOPU) < WAZ(L) .AND. &
               LOPU < Predictor%Max_OPTRAN_Level ) THEN
             LOPL = LOPU
             LOPU = LOPU + 1
             GOTO 40
          ENDIF

    !     Assign the interpolation fraction
          DAOP_TL(L) = WAZ_TL(L) / (TC%ODCAPS_ODAS%Water_Amount(LOPU)  &
		                  - TC%ODCAPS_ODAS%Water_Amount(LOPL))
       ENDDO
       
    !  OPTRAN absorption coefficient scaling factor Water_OPTRAN_Scaling  
       Predictor_TL%Water_OPTRAN_Scaling = ZERO

    !  Water amount in profile layer		
       Predictor_TL%Layer_Water_Amount = WAANG_TL 
       
    !  OPTRAN predictors
       Predictor_TL%Optran_Water_Predictors = Optran_Water_Predictors_TL
       
    !  OPTRAN-to-AIRS interp frac
       Predictor_TL%OPTRAN_Interp_Frac = DAOP_TL 
       
       NULLIFY(TC)  
 
  END SUBROUTINE  Compute_WOPTRAN_Predictors_TL
   
  ! Water vapor OPTRAN channel adjoint predictors 
  SUBROUTINE  Compute_WOPTRAN_Predictors_AD( Sensor_Index,      &  ! Input
                                             Predictor,     &  ! Input
                                             Predictor_AD )    ! In/Output

    ! ---------
    ! Arguments
    ! ---------
    ! -- Input
    INTEGER,                          INTENT( IN )  :: Sensor_Index
    TYPE( Predictor_type ), INTENT( IN )  :: Predictor

    ! -- In/Output
    TYPE( Predictor_type ), INTENT( IN OUT ) :: Predictor_AD

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_WOPTRAN_Predictors_AD'

    ! ---------------
    ! Local variables
    ! ---------------
    REAL(fp_kind), DIMENSION(MAX_N_ODCAPS_LAYERS) :: WAANG
    REAL(fp_kind), DIMENSION(MAX_N_ODCAPS_LAYERS) :: WAANG_AD
    REAL(fp_kind), DIMENSION(MAX_N_WATER_OPTRAN_PREDICTORS,MAX_N_WATER_OPTRAN_LAYERS) :: Optran_Water_Predictors_AD
    REAL(fp_kind), DIMENSION(MAX_N_ODCAPS_LAYERS)  :: DAOP_AD 
    
    INTEGER ::  L			    
    INTEGER ::	LL	  
    INTEGER ::  LOPL	  
    INTEGER ::  LOPU	  
    INTEGER ::	LU	  
    REAL(fp_kind), DIMENSION(MAX_N_ODCAPS_LAYERS) ::     WAZ, WAZ_AD   
    REAL(fp_kind), DIMENSION(MAX_N_ODCAPS_LAYERS) ::     PZ, PZ_AD    
    REAL(fp_kind), DIMENSION(MAX_N_ODCAPS_LAYERS) ::     TZ, TZ_AD   
    REAL(fp_kind), DIMENSION(0:Predictor%n_Layers) :: WAZSUM,  WAZSUM_AD	  
    REAL(fp_kind), DIMENSION(0:Predictor%n_Layers) :: WPZSUM,  WPZSUM_AD	  
    REAL(fp_kind), DIMENSION(0:Predictor%n_Layers) :: WTZSUM,  WTZSUM_AD	  
    REAL(fp_kind) ::     DA,  DA_AD 	  
    REAL(fp_kind) ::    POP,  POP_AD 	  
    REAL(fp_kind) ::    TOP,  TOP_AD 	  
    INTEGER :: H2O_Index		    
    TYPE( ODCAPS_TauCoeff_type ), POINTER  ::  TC => NULL()

    TC => ODCAPS_TC(Sensor_Index)
    
    !#--------------------------------------------------------------------------#
    !#              -- GET THE Atmosphere GASES INDEX  --              #
    !#--------------------------------------------------------------------------#
    H2O_Index = MINLOC( ABS( Predictor%Absorber_ID - H2O_ID ), DIM = 1 )


    !  Initialize amount above sums
    WAZSUM(0) = ZERO
     
    WPZSUM(0) = ZERO 
 
    WTZSUM(0) = ZERO 
 

   !   ---------------------------------------
   !   Calculate raw predictors for all layers
   !   ---------------------------------------
    Forward_Layer_Loop : DO L = 1, Predictor%n_Layers 

   !  Layer amount*angle
      WAANG(L) = Predictor%Absorber(L, H2O_Index) * Predictor%Secant_Sensor_Zenith(L)
     
   !  Center-of-layer-to-space amount*angle
   !  Note: do this before updating AZSUM
      WAZ(L)= POINT_5 * WAANG(L) + WAZSUM(L-1)
       
   !  Bottom-of-layer-to-space amount sum
      WAZSUM(L) = WAANG(L) + WAZSUM(L-1)
 
   !  Pressure above sum
      WPZSUM(L) = WAANG(L)* TC%Ref_Profile_Data(3, L)  + WPZSUM(L-1)
      PZ(L)= WPZSUM(L)/WAZSUM(L)
 
   !  Temperature above sum
      WTZSUM(L) = WAANG(L)* Predictor%Temperature(L) + WTZSUM(L-1)
      TZ(L) = WTZSUM(L)/WAZSUM(L)

    END DO Forward_Layer_Loop

    !  OPTRAN predictors
       Optran_Water_Predictors_AD = Predictor_AD%Optran_Water_Predictors  
       
    !  OPTRAN-to-AIRS interp frac
       DAOP_AD = Predictor_AD%OPTRAN_Interp_Frac  

    !  Water amount in profile layer		
       WAANG_AD = Predictor_AD%Layer_Water_Amount
       
   !   ---------------------------------------
   !   Determine what OPTRAN layers are needed
   !   ---------------------------------------
   !   Initialize LOPL and LOPU
       LOPL = Predictor%Min_OPTRAN_Level
       LOPU = Predictor%Min_OPTRAN_Level + 1

       WAZ_AD( : ) = ZERO
       PZ_AD( : ) = ZERO
       TZ_AD( : ) = ZERO
   !   Loop over the AIRS pressure layers
       DO L=1, Predictor%n_Layers
   
   !   Find the two OPTRAN levels that bracket the AIRS layer
    40    IF ( TC%ODCAPS_ODAS%Water_Amount(LOPU) < WAZ(L) .AND. &
               LOPU < Predictor%Max_OPTRAN_Level ) THEN
             LOPL = LOPU
             LOPU = LOPU + 1
             GOTO 40
          ENDIF

    !     Assign the interpolation fraction
          WAZ_AD(L) = WAZ_AD(L) + DAOP_AD(L) / (TC%ODCAPS_ODAS%Water_Amount(LOPU)  &
		                  - TC%ODCAPS_ODAS%Water_Amount(LOPL))
       ENDDO


   !  Initialize the upper and lower (pressure) layer index
       LL=1
       LU=2
       POP_AD = ZERO
       TOP_AD = ZERO
       
    OPTRAN_Loop : DO L = Predictor%Min_OPTRAN_Level, Predictor%Max_OPTRAN_Level
   !  --------------------------------------------------------
   !   Find the two pressure layers closest to the OPTRAN layer
   !  --------------------------------------------------------
   20     IF ( WAZ(LU) < TC%ODCAPS_ODAS%Water_Amount(L)) THEN
             IF (LU < Predictor%n_Layers ) THEN
                LL=LU
                LU=LU + 1
                GOTO 20
             ENDIF
          ENDIF
   !     Compute the interpolation fractor
          DA = ( TC%ODCAPS_ODAS%Water_Amount( L ) - WAZ( LL )) &
	        / ( WAZ( LU ) - WAZ( LL ) )
 
   !     Do the interpolation
          POP   = ( DA*( TC%Ref_Profile_Data(3, LU) -  TC%Ref_Profile_Data(3, LL) )  & 
	           + TC%Ref_Profile_Data(3, LL) ) &
		    / TC%ODCAPS_ODAS%Water_ProfAve(1,L)
          TOP   = ( DA*( Predictor%Temperature(LU) -  Predictor%Temperature(LL) ) & 
	           + Predictor%Temperature(LL) ) &
		   / TC%ODCAPS_ODAS%Water_ProfAve(2,L)

          POP_AD =  Optran_Water_Predictors_AD(2,L) &
	           + POINT_5 * (POP**(-POINT_5)) * Optran_Water_Predictors_AD(4,L) &
		   + TOP * Optran_Water_Predictors_AD(6,L)
          TOP_AD =  Optran_Water_Predictors_AD(3,L) &
	           + TWO*TOP*Optran_Water_Predictors_AD(5,L) &
		   + POP * Optran_Water_Predictors_AD(6,L)
	  DA_AD  =  Optran_Water_Predictors_AD(7,L) &
	          *( Predictor%Secant_Sensor_Zenith(LU)-Predictor%Secant_Sensor_Zenith(LL)) &
		  + Optran_Water_Predictors_AD(8,L)*(PZ(LU) - PZ(LL)) / TC%ODCAPS_ODAS%Water_ProfAve(3,L) &
		  + Optran_Water_Predictors_AD(9,L)*(TZ(LU) - TZ(LL)) / TC%ODCAPS_ODAS%Water_ProfAve(4,L) &
		  + TOP_AD * ( Predictor%Temperature(LU)  & 
		             - Predictor%Temperature(LL) ) / TC%ODCAPS_ODAS%Water_ProfAve(2,L) &
		  + POP_AD * ( TC%Ref_Profile_Data(3, LU)  &
		             - TC%Ref_Profile_Data(3, LL) ) / TC%ODCAPS_ODAS%Water_ProfAve(1,L)	        
	  WAZ_AD(LL) = WAZ_AD(LL) + ( (TC%ODCAPS_ODAS%Water_Amount( L ) - WAZ( LL )) * DA_AD &
	 	          / ( WAZ( LU ) - WAZ( LL ) )**TWO &
			 - DA_AD /( WAZ( LU ) - WAZ( LL ) ) )
          WAZ_AD(LU) = WAZ_AD(LU) - ( TC%ODCAPS_ODAS%Water_Amount( L ) - WAZ( LL )) &
	                 * DA_AD / ( WAZ( LU ) - WAZ( LL ) )**TWO

	  TZ_AD(LL) = TZ_AD(LL) + ( Optran_Water_Predictors_AD(9,L) - DA * Optran_Water_Predictors_AD(9,L) ) &
    		                    / TC%ODCAPS_ODAS%Water_ProfAve(4,L) 
	  TZ_AD(LU) = TZ_AD(LU) + DA * Optran_Water_Predictors_AD(9,L) / TC%ODCAPS_ODAS%Water_ProfAve(4,L)
	  
	  PZ_AD(LL) = PZ_AD(LL) + ( Optran_Water_Predictors_AD(8,L) - DA * Optran_Water_Predictors_AD(8,L) ) &
    		                    / TC%ODCAPS_ODAS%Water_ProfAve(3,L) 
	  PZ_AD(LU) = PZ_AD(LU) + DA * Optran_Water_Predictors_AD(8,L) / TC%ODCAPS_ODAS%Water_ProfAve(3,L)


          Predictor_AD%Temperature(LL) = Predictor_AD%Temperature(LL) &
	             + TOP_AD * (ONE - DA) / TC%ODCAPS_ODAS%Water_ProfAve(2,L)
	  Predictor_AD%Temperature(LU) = Predictor_AD%Temperature(LU) &
	             + DA* TOP_AD  / TC%ODCAPS_ODAS%Water_ProfAve(2,L)
		     
          Optran_Water_Predictors_AD(:,L) = ZERO
	  		     
    END DO OPTRAN_Loop

          WAZSUM_AD(0:Predictor%n_Layers) = ZERO
          WPZSUM_AD(0:Predictor%n_Layers) = ZERO
          WTZSUM_AD(0:Predictor%n_Layers) = ZERO
    Adjoint_Layer_Loop: DO L = Predictor%n_Layers, 1, -1

	 WAZSUM_AD(L) = WAZSUM_AD(L) - WTZSUM(L) * TZ_AD(L) / WAZSUM(L)**TWO &
	                             - WPZSUM(L) * PZ_AD(L) / WAZSUM(L)**TWO 

         WTZSUM_AD(L) = WTZSUM_AD(L) + TZ_AD(L) / WAZSUM(L)
 	 Predictor_AD%Temperature(L) = Predictor_AD%Temperature(L) + WAANG(L) * WTZSUM_AD(L) 

         WPZSUM_AD(L) = WPZSUM_AD(L) + PZ_AD(L) / WAZSUM(L)
 	 
	 WAZSUM_AD(L-1) = WAZSUM_AD(L-1) +  WAZSUM_AD(L) + WAZ_AD(L)
	 WTZSUM_AD(L-1) = WTZSUM_AD(L-1) +  WTZSUM_AD(L) 
	 WPZSUM_AD(L-1) = WPZSUM_AD(L-1) +  WPZSUM_AD(L) 
        
	 WAANG_AD(L) = WAANG_AD(L) + WTZSUM_AD(L) * Predictor%Temperature(L) &
	                           + WPZSUM_AD(L) * TC%Ref_Profile_Data(3, L) &
				   + POINT_5 * WAZ_AD(L) + WAZSUM_AD(L)
	 Predictor_AD%Absorber(L, H2O_Index) = Predictor_AD%Absorber(L, H2O_Index) & 	 
	           + WAANG_AD(L) * Predictor%Secant_Sensor_Zenith(L)
		   
         

	 TZ_AD(L) = ZERO
	 PZ_AD(L) = ZERO
	 WAZ_AD(L) = ZERO
	 WAANG_AD(L) = ZERO
	 WAZSUM_AD(L) = ZERO
	 WPZSUM_AD(L) = ZERO
	 WTZSUM_AD(L) = ZERO
    END DO Adjoint_Layer_Loop
         
	 WAZSUM_AD(0) = ZERO
	 WPZSUM_AD(0) = ZERO
	 WTZSUM_AD(0) = ZERO
         
	 NULLIFY(TC) 
  END SUBROUTINE  Compute_WOPTRAN_Predictors_AD
   

  ! Calculate the channel independent non-LTE predictors
  SUBROUTINE  Compute_Non_LTE_Predictors( GeometryInfo,   &  ! Input
                                          Predictor )    ! In/Output
    ! ---------
    ! Arguments
    ! ---------
    ! -- Inputs
    TYPE( CRTM_GeometryInfo_type ),  INTENT( IN )     :: GeometryInfo

    ! -- In/Outputs
    TYPE( Predictor_type ), INTENT( IN OUT ) :: Predictor

    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Non_LTE_Predictors'
    
    ! ---------------
    ! Local variables
    ! ---------------
    REAL(fp_kind) :: Temp_ave
    REAL(fp_kind), DIMENSION (MAX_N_NON_LTE_PREDICTORS)  :: Non_LTE_Predictors
    
    Temp_ave =( Predictor%Temperature(1) + Predictor%Temperature(2) &
              + Predictor%Temperature(3) + Predictor%Temperature(4) &
              + Predictor%Temperature(5) ) / FIVE    
    Non_LTE_Predictors(1) = ONE
    Non_LTE_Predictors(2) = Predictor%Source_COS_Layer1
    Non_LTE_Predictors(3) = Predictor%Source_COS_Layer1 &
                          * Predictor%Source_COS_Layer1
    Non_LTE_Predictors(4) = Predictor%Source_COS_Layer1 &
                          * Predictor%Secant_Sensor_Zenith(1)
    Non_LTE_Predictors(5) = Predictor%Source_COS_Layer1 * Temp_ave
    Non_LTE_Predictors(6) = COS( DEGREES_TO_RADIANS*GeometryInfo%Source_Zenith_Angle )
    
    Predictor%Non_LTE_Predictors = Non_LTE_Predictors

  END SUBROUTINE Compute_Non_LTE_Predictors

  ! Calculate the channel independent tangent-linear non-LTE predictors
  SUBROUTINE  Compute_Non_LTE_Predictors_TL( GeometryInfo,    &  ! Input
                                             Predictor,   &  ! Input
					     Predictor_TL)   ! In/Output
    ! ---------
    ! Arguments
    ! ---------
    ! -- Inputs
    TYPE( CRTM_GeometryInfo_type ),  INTENT( IN )     :: GeometryInfo
    TYPE( Predictor_type ), INTENT( IN )     :: Predictor

    ! -- In/Outputs
    TYPE( Predictor_type ), INTENT( IN OUT ) :: Predictor_TL

    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Non_LTE_Predictors_TL'
    
    ! ---------------
    ! Local variables
    ! ---------------
    REAL(fp_kind) :: Temp_ave, Temp_ave_TL
    REAL(fp_kind), DIMENSION (MAX_N_NON_LTE_PREDICTORS)  :: Non_LTE_Predictors_TL
    
    Temp_ave =( Predictor%Temperature(1) + Predictor%Temperature(2) &
              + Predictor%Temperature(3) + Predictor%Temperature(4) &
              + Predictor%Temperature(5) ) / FIVE
	         
    Temp_ave_TL =( Predictor_TL%Temperature(1) + Predictor_TL%Temperature(2) &
                 + Predictor_TL%Temperature(3) + Predictor_TL%Temperature(4) &
                 + Predictor_TL%Temperature(5) ) / FIVE
	       
    Non_LTE_Predictors_TL(1) = ZERO
    Non_LTE_Predictors_TL(2) = ZERO
    Non_LTE_Predictors_TL(3) = ZERO 
    Non_LTE_Predictors_TL(4) = ZERO 
    Non_LTE_Predictors_TL(5) = Predictor%Source_COS_Layer1 * Temp_ave_TL
    Non_LTE_Predictors_TL(6) = ZERO 
    
    Predictor_TL%Non_LTE_Predictors = Non_LTE_Predictors_TL

  END SUBROUTINE Compute_Non_LTE_Predictors_TL


  ! Calculate the channel independent adjoint non-LTE predictors
  SUBROUTINE  Compute_Non_LTE_Predictors_AD( GeometryInfo,       &  ! Input
                                             Predictor,      &  ! Input
					     Predictor_AD)   ! In/Output
    ! ---------
    ! Arguments
    ! ---------
    ! -- Inputs
    TYPE( CRTM_GeometryInfo_type ),  INTENT( IN )     :: GeometryInfo
    TYPE( Predictor_type ), INTENT( IN )     :: Predictor

    ! -- In/Outputs
    TYPE( Predictor_type ), INTENT( IN OUT ) :: Predictor_AD

    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Non_LTE_Predictors_AD'
    
    ! ---------------
    ! Local variables
    ! ---------------
    REAL(fp_kind) :: Temp_ave, Temp_ave_AD
    REAL(fp_kind), DIMENSION (MAX_N_NON_LTE_PREDICTORS)  :: Non_LTE_Predictors_AD
    
    Temp_ave =( Predictor%Temperature(1) + Predictor%Temperature(2) &
              + Predictor%Temperature(3) + Predictor%Temperature(4) &
              + Predictor%Temperature(5) ) / FIVE

    Non_LTE_Predictors_AD = Predictor_AD%Non_LTE_Predictors	      

    Temp_ave_AD =  Predictor%Source_COS_Layer1 * Non_LTE_Predictors_AD(5) 
    
	    
     
    Predictor_AD%Temperature(1) = Predictor_AD%Temperature(1) + Temp_ave_AD / FIVE
    Predictor_AD%Temperature(2) = Predictor_AD%Temperature(2) + Temp_ave_AD / FIVE
    Predictor_AD%Temperature(3) = Predictor_AD%Temperature(3) + Temp_ave_AD / FIVE
    Predictor_AD%Temperature(4) = Predictor_AD%Temperature(4) + Temp_ave_AD / FIVE
    Predictor_AD%Temperature(5) = Predictor_AD%Temperature(5) + Temp_ave_AD / FIVE
    Temp_ave_AD = ZERO

  END SUBROUTINE Compute_Non_LTE_Predictors_AD

  !----------------------------------------------------------------
  ! The SUBROUTINE interpolates the user profiles on ODCAPS pressure
  ! grids; it also convert profile units to the required units
  ! used in ODCAPS modules.
  !----------------------------------------------------------------
   
  SUBROUTINE ConvertToODCAPSProfile( Atmosphere,       &   ! Input
                                  GeometryInfo,     &   ! Input
                                  Sensor_Index,     &   ! Input
				  Predictor )      ! In/Output
    ! -- Inputs
    TYPE( CRTM_Atmosphere_type ),    INTENT( IN )     :: Atmosphere
    TYPE( Predictor_type ),INTENT( IN OUT ) :: Predictor
    TYPE( CRTM_GeometryInfo_type ),  INTENT( IN )     :: GeometryInfo
    INTEGER,                         INTENT( IN )     :: Sensor_Index

    ! ---------------
    ! Function result
    ! ---------------
    INTEGER :: Error_Status

    ! ----------------
    ! Local parameters
    ! ----------------
 
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'ConvertToODCAPSProfile'
    
    ! ---------------
    ! Local variables
    ! ---------------
    
    CHARACTER(256) :: Message

    REAL(fp), DIMENSION(MAX_N_ODCAPS_LAYERS) :: temperature, CO_Amount, CH4_Amount
    REAL(fp), DIMENSION(MAX_N_ODCAPS_LAYERS) :: vm_H2O, Delta_Z 
    REAL(fp), DIMENSION(0:MAX_N_ODCAPS_LAYERS) :: Altitude
    REAL(fp), DIMENSION(MAX_N_ODCAPS_LAYERS, MAX_N_ABSORBERS_ODCAPS) :: vj   
    INTEGER       :: n_Layers_usr, n_Layers, n_Absorbers, j, k 
    INTEGER       :: H2O_Index, O3_index,CO_Index, CH4_Index 
    INTEGER       :: CO2_Index, SO2_Index, HNO3_Index, N2O_Index
    REAL(fp) :: psurf, m_air, delp
    REAL(fp), DIMENSION(Atmosphere%n_Layers) :: mr_H2O 
    TYPE( ODCAPS_TauCoeff_type ), POINTER  ::  TC => NULL()

    TC => ODCAPS_TC(Sensor_Index)

    Error_Status = SUCCESS

    n_Layers_usr = Atmosphere%n_Layers
    n_Absorbers  = Atmosphere%n_Absorbers

    !----------------------------------------------------------------------
    ! set the ODCAPS pressure profile into Predictor;      
    ! the thickness of the ODCAPS surface layer is adjusted to that    
    ! between the ODCAPS upper boundary of the surface layer and the
    ! surface pressure of the user pressure profile.
    !----------------------------------------------------------------------

    psurf = Atmosphere%Level_pressure(n_Layers_usr)
    
    !--------------------------------------------------------------------------
    !       -- GET THE BOTTOM LAYER NUMBER AND FRACTIONAL MULTIPLIER --        
    !--------------------------------------------------------------------------

    CALL Get_ODCAPS_Layers ( n_Layers_usr, REAL(TC%Standard_Level_Pressure, fp), psurf, & 
                      n_Layers, Predictor%BL_Frac_Multi ) 

    Predictor%n_Layers = n_Layers

    ! copy ODCAPS level pressures except the last level                                
    Predictor%Level_Pressure(0:n_Layers-1) = TC%Standard_Level_Pressure(0:n_Layers-1)    
    Predictor%Level_Pressure(n_Layers) = psurf
    
    ! Mean layer pressure
    Predictor%Layer_Pressure(1:n_Layers) = &                                    
      ( Predictor%Level_Pressure(1:n_Layers) - Predictor%Level_Pressure(0:n_Layers-1)) / &
    !----------------------------------------------------------------------------------------------------                         
      LOG(Predictor%Level_Pressure(1:n_Layers) / Predictor%Level_Pressure(0:n_Layers-1) )

    !-------------------------------------------------
    ! Temperature profile interpolation on the ODCAPS grids
    !-------------------------------------------------

    CALL Interpolate_Profile( &
                Atmosphere%Temperature(1:n_Layers_usr), &
                Atmosphere%Pressure(1:n_Layers_usr), &
                Predictor%Layer_Pressure(1:n_Layers), &
                temperature(1:n_Layers) ) !, &
                !interp_index = Predictor%Index_Interpolation(1:n_Layers, :))
    
    Predictor%Temperature( 1:n_Layers ) = temperature( 1:n_Layers )

    !----------------------------------------------------------
    !  Interpolate layer gas mixing ratio on the ODCAPS grids
    !---------------------------------------------------------- 

    ! -- find user water vapor index
    H2O_Index = MINLOC( ABS( Atmosphere%Absorber_ID - H2O_ID ), DIM = 1 )

    ! -- find user ozone index
    ! O3_Index = MINLOC( ABS( Atmosphere%Absorber_ID - O3_ID ), DIM = 1 )

    !----------------------------------------------------------------
    ! -- Optional index for user profile, but are necessary for ODCAPS
    !---------------------------------------------------------------- 

    ! -- find user CO index
    j = COUNT( Atmosphere%Absorber_ID(1:n_absorbers) == CO_ID ) 
 
    IF ( j < 1) THEN ! no CO gas
    ! Reset CO_Amount if input Profile lacks CO profile
      CO_Index = 0
      CO_Amount(1:n_Layers) = TC%Ref_Profile_Data(8,1:n_Layers)
    ELSE
      CO_Index = MINLOC( ABS( Atmosphere%Absorber_ID - CO_ID ), DIM = 1 ) 
    ENDIF
    
    ! -- find user CH4 index
    j = COUNT( Atmosphere%Absorber_ID(1:n_absorbers) == CH4_ID ) 
 
    IF ( j < 1) THEN ! no CH4 gas
    ! Reset CH4_Amount if input Profile lacks CH4 profile
      CH4_Index = 0
      CH4_Amount(1:n_Layers) = TC%Ref_Profile_Data(9,1:n_Layers)
    ELSE
      CH4_Index = MINLOC( ABS( Atmosphere%Absorber_ID - CH4_ID ), DIM = 1 ) 
    ENDIF
    
    ! -- find user CO2 index
    j = COUNT( Atmosphere%Absorber_ID(1:n_absorbers) == CO2_ID ) 
 
    IF ( j < 1) THEN ! no CO2 gas
    ! Reset CO2_Amount if input Profile lacks CO2 profile
      CO2_Index = 0
      Predictor%CO2_Amount(1:n_Layers) = TC%Ref_Profile_Data(5,1:n_Layers)
    ELSE
      CO2_Index = MINLOC( ABS( Atmosphere%Absorber_ID - CO2_ID ), DIM = 1 ) 
    ENDIF

    ! -- find user SO2 index
    j = COUNT( Atmosphere%Absorber_ID(1:n_absorbers) == SO2_ID ) 
 
    IF ( j < 1) THEN ! no SO2 gas
    ! Reset SO2_Amount if input Profile lacks SO2 profile
      SO2_Index = 0
      Predictor%SO2_Amount(1:n_Layers) = TC%Ref_Profile_Data(10,1:n_Layers)
    ELSE
      SO2_Index = MINLOC( ABS( Atmosphere%Absorber_ID - SO2_ID ), DIM = 1 ) 
    ENDIF

    ! -- find user HNO3 index
    j = COUNT( Atmosphere%Absorber_ID(1:n_absorbers) == HNO3_ID ) 
 
    IF ( j < 1) THEN ! no HNO3 gas
    ! Reset HNO3_Amount if input Profile lacks HNO3 profile
      HNO3_Index = 0
      Predictor%HNO3_Amount(1:n_Layers) = TC%Ref_Profile_Data(11,1:n_Layers)
    ELSE
      HNO3_Index = MINLOC( ABS( Atmosphere%Absorber_ID - HNO3_ID ), DIM = 1 ) 
    ENDIF

    ! -- find user N2O index
    j = COUNT( Atmosphere%Absorber_ID(1:n_absorbers) == N2O_ID ) 
 
    IF ( j < 1) THEN ! no N2O gas
    ! Reset N2O_Amount if input Profile lacks N2O profile
      N2O_Index = 0
      Predictor%N2O_Amount(1:n_Layers) = TC%Ref_Profile_Data(12,1:n_Layers)
    ELSE
      N2O_Index = MINLOC( ABS( Atmosphere%Absorber_ID - N2O_ID ), DIM = 1 ) 
    ENDIF

    ! -- water vapor
    IF( Atmosphere%Absorber_Units(H2O_Index) == MASS_MIXING_RATIO_UNITS  )THEN
      
      Error_Status = MR_to_PPMV (Atmosphere%Absorber(1:n_Layers_usr, H2O_Index), &
                                 mr_H2O(1:n_Layers_usr) )
				 
      IF ( Error_Status/=SUCCESS ) THEN     
       CALL Display_Message( ROUTINE_NAME, &
                            'Error calculating the Tangent-linear of Predictor.', &
                            Error_Status )
       RETURN
      END IF
 
      CALL Interpolate_Profile( &
                  !Predictor%Index_Interpolation(1:n_Layers, :), &
                  mr_H2O(1:n_Layers_usr), &
                  Atmosphere%Pressure(1:n_Layers_usr), &
                  Predictor%Layer_Pressure(1:n_Layers), &
                  vm_H2O(1:n_Layers))
      
    ELSE IF( Atmosphere%Absorber_Units(H2O_Index) == VOLUME_MIXING_RATIO_UNITS )THEN

        CALL Interpolate_Profile( &
                  !Predictor%Index_Interpolation(1:n_Layers, :), &
                  Atmosphere%Absorber(1:n_Layers_usr, H2O_Index), &
                  Atmosphere%Pressure(1:n_Layers_usr), &
                  Predictor%Layer_Pressure(1:n_Layers), &
                  vm_H2O(1:n_Layers))

    ELSE

      Error_Status = FAILURE                                                        
      WRITE( Message, '( "The water vapor units type ", i4, " is currently not supported")') & 
                          Atmosphere%Absorber_Units(H2O_Index)                              
      CALL Display_Message( ROUTINE_NAME, &                                         
                            TRIM( Message ), &                                      
                            Error_Status )                                         
      RETURN                                                                        
    ENDIF


    ! Calculate the profile altitude and layer thickness
    Altitude( 0 ) = GeometryInfo%Surface_Altitude
      
    DO j = 1, n_Layers
      k = n_Layers + 1 - j 
    ! Average wet air mass
      m_air = (ONE - vm_H2O( k ) * 1.0e-6_fp ) * MW_DRYAIR + &
	       vm_H2O( k ) * 1.0e-6_fp * MW_H2O
      delp = Predictor%Level_Pressure( k ) - Predictor%Level_Pressure( k -1 ) 
      
      Altitude( j ) =  Altitude( j -1 ) + delp * Predictor%Temperature(k) / &
	                 ( G0 * m_air / R0 * Predictor%Layer_Pressure(k) )
    ENDDO    
    
    DO j = 1, n_Layers
      k = n_Layers + 1 - j 
      Predictor%Altitude( j ) = ( Altitude( k ) + Altitude( k -1 ) ) / TWO * 1000.0_fp
      Delta_Z(j) = (Altitude( k ) - Altitude( k -1 ) ) * 1000.0_fp  ! convert km to m
    ENDDO
 
    ! -- water vapor
 	
    Error_Status = PPMV_to_KMOL( Predictor%Layer_Pressure(1:n_Layers), &       	     
		  	 	 Predictor%Temperature(1:n_Layers), &    	     
		  		 Delta_Z(1:n_Layers), &	        	     
                  		 vm_H2O(1:n_Layers), &
				 Predictor%Absorber(1:n_Layers, H2O_Index) )			        	     
    
    IF ( Error_Status/=SUCCESS ) THEN     
       CALL Display_Message( ROUTINE_NAME, &
                            'Error calculating the Tangent-linear of Predictor.', &
                            Error_Status )
      RETURN
    END IF
 
    !-- Other gases
    DO j = 1, n_Absorbers  ! j - ODCAPS absorber index

      IF( j == H2O_Index ) CYCLE   ! H2O is already done

      IF( Atmosphere%Absorber_Units( j ) == VOLUME_MIXING_RATIO_UNITS ) THEN

        CALL Interpolate_Profile( &
                    !Predictor%Index_Interpolation(1:n_Layers, :), &
                    Atmosphere%Absorber(1:n_Layers_usr, j), &
                    Atmosphere%Pressure(1:n_Layers_usr), &                 
                    Predictor%Layer_Pressure(1:n_Layers), &
                    vj(1:n_Layers, j))  
         		    
	Error_Status = PPMV_to_KMOL( Predictor%Layer_Pressure(1:n_Layers), &    		     
				     Predictor%Temperature(1:n_Layers), & 		     
				     Delta_Z(1:n_Layers), &	     		    
                		     vj(1:n_Layers, j), &
				     Predictor%Absorber(1:n_Layers,j), &			     		     
				     Predictor%Absorber(1:n_Layers, H2O_Index) )			     		    
					 
        IF ( Error_Status/=SUCCESS ) THEN     
           CALL Display_Message( ROUTINE_NAME, &
        			'Error calculating the Tangent-linear of Predictor.', &
        			Error_Status )
          RETURN
        END IF

      ELSE

        Error_Status = FAILURE                                                        
        WRITE( Message, '( "The absorber units type ", i4, " is currently not supported")') &
                            Atmosphere%Absorber_Units(j)                              
        CALL Display_Message( ROUTINE_NAME, &                                         
                              TRIM( Message ), &                                      
                              Error_Status )                                         
        RETURN                                                                        
                                 
      ENDIF

    ENDDO
    


    ! The input gas must have CH4 and CO for ODCAPS
    Predictor%n_Absorbers = n_Absorbers


    Predictor%Absorber_ID(1:n_Absorbers)  =  Atmosphere%Absorber_ID(1:n_Absorbers)
 
    IF ( CO_Index == 0 .AND. CH4_Index == 0 ) THEN
       
      Predictor%Absorber(1:n_Layers,n_Absorbers+1) = CO_Amount(1:n_Layers)
      Predictor%Absorber_ID(n_Absorbers+1) = CO_ID
      
      Predictor%Absorber(1:n_Layers,n_Absorbers+2) = CH4_Amount(1:n_Layers)
      Predictor%Absorber_ID(n_Absorbers+2) = CH4_ID
      
      Predictor%n_Absorbers = n_Absorbers+2

    ELSE IF ( CO_Index > 0 .AND. CH4_Index == 0  ) THEN
      Predictor%Absorber(1:n_Layers,n_Absorbers+1) = CH4_Amount(1:n_Layers)
      Predictor%Absorber_ID(n_Absorbers+1) = CH4_ID
      Predictor%n_Absorbers = n_Absorbers+1
    
    ELSE IF ( CO_Index == 0 .AND. CH4_Index > 0  ) THEN
      Predictor%Absorber(1:n_Layers,n_Absorbers+1) = CO_Amount(1:n_Layers)
      Predictor%Absorber_ID(n_Absorbers+1) = CO_ID
      Predictor%n_Absorbers = n_Absorbers+1  
 
    ENDIF

    IF ( CO2_Index > 0 ) THEN
      Predictor%CO2_Amount(1:n_Layers) = Predictor%Absorber(1:n_Layers, CO2_Index)
    ENDIF
    
    IF ( SO2_Index > 0 ) THEN
      Predictor%SO2_Amount(1:n_Layers) = Predictor%Absorber(1:n_Layers, SO2_Index)
    ENDIF
    
    IF ( HNO3_Index > 0 ) THEN
      Predictor%HNO3_Amount(1:n_Layers) = Predictor%Absorber(1:n_Layers,HNO3_Index)
    ENDIF

    IF ( N2O_Index > 0 ) THEN
      Predictor%N2O_Amount(1:n_Layers) = Predictor%Absorber(1:n_Layers,N2O_Index)
    ENDIF
     
    ! ODCAPS Secant Zenith for different altitude
    CALL Compute_Secant_Zenith( GeometryInfo,  &  ! Input
                                Predictor )   ! In/Output

    ! ODCAPS Fix and Trace Gas Multipliers
    CALL Fix_And_Trace_Gas_Multi( Sensor_Index,   &  ! Input
                                  GeometryInfo,   &  ! Input
                                  Predictor)     ! In/Output
				
    NULLIFY(TC)
  END SUBROUTINE ConvertToODCAPSProfile


  !----------------------------------------------------------------
  ! The SUBROUTINE interpolates the user profiles on ODCAPS pressure
  ! grids; it also convert profile units to the required units
  ! used in ODCAPS modules. Tangent-linear model. 
  !----------------------------------------------------------------
   
  SUBROUTINE ConvertToODCAPSProfile_TL( Atmosphere,       &   ! Input
                                     Predictor,    &   ! Input
                                     Atmosphere_TL,    &   ! Input
                                     GeometryInfo,     &   ! Input
                                     Sensor_Index,     &   ! Input
				     Predictor_TL )   ! In/Output
    !-- Inputs
    TYPE( CRTM_Atmosphere_type ),    INTENT( IN )     :: Atmosphere
    TYPE( CRTM_Atmosphere_type ),    INTENT( IN )     :: Atmosphere_TL
    TYPE( Predictor_type ),INTENT( IN )     :: Predictor
    TYPE( CRTM_GeometryInfo_type ),  INTENT( IN )     :: GeometryInfo
    INTEGER,                         INTENT( IN )     :: Sensor_Index

    !-- In/Outputs
    TYPE( Predictor_type ), INTENT( IN OUT )    :: Predictor_TL

    ! ---------------
    ! Function result
    ! ---------------
    INTEGER :: Error_Status

    ! ----------------
    ! Local parameters
    ! ----------------
 
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'ConvertToODCAPSprofile_TL'
    
    ! ---------------
    ! Local variables
    ! ---------------
    
    CHARACTER(256) :: Message

    REAL(fp), DIMENSION(Predictor%n_Layers) :: temperature_TL !!,tempzero_TL  
    REAL(fp), DIMENSION(Predictor%n_Layers) :: CO_Amount_TL, CH4_Amount_TL 
    REAL(fp), DIMENSION(Predictor%n_Layers) :: vm_H2O, vm_H2O_TL,Delta_Z,Delta_Z_TL 
    REAL(fp), DIMENSION(0:Predictor%n_Layers) :: Altitude, Altitude_TL 
    REAL(fp), DIMENSION(Predictor%n_Layers, Predictor%n_Absorbers) :: vj, vj_TL   
    INTEGER       :: n_Layers_usr, n_Layers, n_Absorbers, j, k 
    INTEGER       :: H2O_Index,O3_index,CO_Index, CH4_Index
    INTEGER       :: CO2_Index, SO2_Index, HNO3_Index, N2O_Index 
    REAL(fp) :: m_air, delp, m_air_TL  
    REAL(fp), DIMENSION(Atmosphere%n_Layers) :: mr_H2O, mr_H2O_TL
    

    Error_Status = SUCCESS

    n_Layers_usr = Atmosphere%n_Layers
    n_Absorbers  = Atmosphere%n_Absorbers

    !----------------------------------------------------------------------
    ! set the ODCAPS pressure profile into Predictor;      
    ! the thickness of the ODCAPS surface layer is adjusted to that    
    ! between the ODCAPS upper boundary of the surface layer and the
    ! surface pressure of the user pressure profile.
    !----------------------------------------------------------------------

    !--------------------------------------------------------------------------
    !       -- GET THE BOTTOM LAYER NUMBER AND FRACTIONAL MULTIPLIER --        
    !--------------------------------------------------------------------------

    n_Layers = Predictor%n_Layers  

    !-------------------------------------------------
    ! Temperature profile interpolation on the ODCAPS grids
    !-------------------------------------------------

    CALL Interpolate_Profile_TL( &
                !Predictor%Index_Interpolation(1:n_Layers, :), &
                Atmosphere%Temperature(1:n_Layers_usr), &
                Atmosphere%Pressure(1:n_Layers_usr), &
                Predictor%Layer_Pressure(1:n_Layers), &
                Atmosphere_TL%Temperature, &
		temperature_TL(1:n_Layers) )
    
    Predictor_TL%Temperature( 1:n_Layers ) = temperature_TL( 1:n_Layers )

    !----------------------------------------------------------
    !  Interpolate layer gas mixing ratio on the ODCAPS grids
    !---------------------------------------------------------- 

    ! -- find user water vapor index
    H2O_Index = MINLOC( ABS( Atmosphere%Absorber_ID - H2O_ID ), DIM = 1 )
    !----------------------------------------------------------------
    ! -- Optional index for user profile, but are necessary for ODCAPS
    !---------------------------------------------------------------- 

    ! -- find user CO index
    j = COUNT( Atmosphere%Absorber_ID(1:n_absorbers) == CO_ID ) 
 
    IF ( j < 1) THEN ! no CO gas
    ! Reset CO_Amount if input Profile lacks CO profile
      CO_Index = 0
      CO_Amount_TL(1:n_Layers) = ZERO
    ELSE
      CO_Index = MINLOC( ABS( Atmosphere%Absorber_ID - CO_ID ), DIM = 1 ) 
    ENDIF
    
    ! -- find user CH4 index
    j = COUNT( Atmosphere%Absorber_ID(1:n_absorbers) == CH4_ID ) 
 
    IF ( j < 1) THEN ! no CH4 gas
    ! Reset CH4_Amount if input Profile lacks CH4 profile
      CH4_Index = 0
      CH4_Amount_TL(1:n_Layers) = ZERO
    ELSE
      CH4_Index = MINLOC( ABS( Atmosphere%Absorber_ID - CH4_ID ), DIM = 1 ) 
    ENDIF
    
    ! -- find user CO2 index
    j = COUNT( Atmosphere%Absorber_ID(1:n_absorbers) == CO2_ID ) 
 
    IF ( j < 1) THEN ! no CO2 gas
    ! Reset CO2_Amount if input Profile lacks CO2 profile
      CO2_Index = 0
      Predictor_TL%CO2_Amount(1:n_Layers) = ZERO
    ELSE
      CO2_Index = MINLOC( ABS( Atmosphere%Absorber_ID - CO2_ID ), DIM = 1 ) 
    ENDIF

    ! -- find user SO2 index
    j = COUNT( Atmosphere%Absorber_ID(1:n_absorbers) == SO2_ID ) 
 
    IF ( j < 1) THEN ! no SO2 gas
    ! Reset SO2_Amount if input Profile lacks SO2 profile
      SO2_Index = 0
      Predictor_TL%SO2_Amount(1:n_Layers) = ZERO
    ELSE
      SO2_Index = MINLOC( ABS( Atmosphere%Absorber_ID - SO2_ID ), DIM = 1 ) 
    ENDIF

    ! -- find user HNO3 index
    j = COUNT( Atmosphere%Absorber_ID(1:n_absorbers) == HNO3_ID ) 
 
    IF ( j < 1) THEN ! no HNO3 gas
    ! Reset HNO3_Amount if input Profile lacks HNO3 profile
      HNO3_Index = 0
      Predictor_TL%HNO3_Amount(1:n_Layers) = ZERO
    ELSE
      HNO3_Index = MINLOC( ABS( Atmosphere%Absorber_ID - HNO3_ID ), DIM = 1 ) 
    ENDIF

    ! -- find user N2O index
    j = COUNT( Atmosphere%Absorber_ID(1:n_absorbers) == N2O_ID ) 
 
    IF ( j < 1) THEN ! no N2O gas
    ! Reset N2O_Amount if input Profile lacks N2O profile
      N2O_Index = 0
      Predictor_TL%N2O_Amount(1:n_Layers) = ZERO
    ELSE
      N2O_Index = MINLOC( ABS( Atmosphere%Absorber_ID - N2O_ID ), DIM = 1 ) 
    ENDIF

    ! -- water vapor
    IF( Atmosphere%Absorber_Units(H2O_Index) == MASS_MIXING_RATIO_UNITS  )THEN
      
      Error_Status = MR_to_PPMV (Atmosphere%Absorber(1:n_Layers_usr, H2O_Index), &
                                 mr_H2O(1:n_Layers_usr) )
				 
      IF ( Error_Status/=SUCCESS ) THEN     
       CALL Display_Message( ROUTINE_NAME, &
                            'Error calculating the Tangent-linear of Predictor.', &
                            Error_Status )
       RETURN
      END IF

      Error_Status = MR_to_PPMV_TL (Atmosphere%Absorber(1:n_Layers_usr, H2O_Index), &
                                    mr_H2O(1:n_Layers_usr), &
				    Atmosphere_TL%Absorber(1:n_Layers_usr, H2O_Index), &
				    mr_H2O_TL(1:n_Layers_usr) )
				 
      IF ( Error_Status/=SUCCESS ) THEN     
       CALL Display_Message( ROUTINE_NAME, &
                            'Error calculating the Tangent-linear of Predictor.', &
                            Error_Status )
       RETURN
      END IF
 
      CALL Interpolate_Profile( &
                  !Predictor%Index_Interpolation(1:n_Layers, :), &
                  mr_H2O(1:n_Layers_usr), &
                  Atmosphere%Pressure(1:n_Layers_usr), &
                  Predictor%Layer_Pressure(1:n_Layers), &
                  vm_H2O(1:n_Layers))

      CALL Interpolate_Profile_TL( &
                  !Predictor%Index_Interpolation(1:n_Layers, :), &
                  mr_H2O(1:n_Layers_usr), &
                  Atmosphere%Pressure(1:n_Layers_usr), &
                  Predictor%Layer_Pressure(1:n_Layers), &
		  mr_H2O_TL(1:n_Layers_usr), &
                  vm_H2O_TL(1:n_Layers))
      
   
    ELSE IF( Atmosphere%Absorber_Units(H2O_Index) == VOLUME_MIXING_RATIO_UNITS )THEN

        CALL Interpolate_Profile( &
                  !Predictor%Index_Interpolation(1:n_Layers, :), &
                  Atmosphere%Absorber(1:n_Layers_usr, H2O_Index), &
                  Atmosphere%Pressure(1:n_Layers_usr), &
                  Predictor%Layer_Pressure(1:n_Layers), &
                  vm_H2O(1:n_Layers))

        CALL Interpolate_Profile_TL( &
                  !Predictor%Index_Interpolation(1:n_Layers, :), &
                  Atmosphere%Absorber(1:n_Layers_usr, H2O_Index), &
                  Atmosphere%Pressure(1:n_Layers_usr), &
                  Predictor%Layer_Pressure(1:n_Layers), &
                  Atmosphere_TL%Absorber(1:n_Layers_usr, H2O_Index), &
		  vm_H2O_TL(1:n_Layers))

    ELSE

      Error_Status = FAILURE                                                        
      WRITE( Message, '( "The water vapor units type ", i4, " is currently not supported")') & 
                          Atmosphere%Absorber_Units(H2O_Index)                              
      CALL Display_Message( ROUTINE_NAME, &                                         
                            TRIM( Message ), &                                      
                            Error_Status )                                         
      RETURN                                                                        
    ENDIF

    ! Calculate the profile altitude and layer thickness
    Altitude( 0 ) = GeometryInfo%Surface_Altitude
    Altitude_TL(0) = ZERO
     
    DO j = 1, n_Layers
      k = n_Layers + 1 - j 
    ! Average wet air mass
      m_air = (ONE - vm_H2O( k ) * 1.0e-6_fp ) * MW_DRYAIR + &
	       vm_H2O( k ) * 1.0e-6_fp * MW_H2O

      m_air_TL = vm_H2O_TL( K ) * 1.0e-6_fp  * ( MW_H2O - MW_DRYAIR)
      	       
      delp = Predictor%Level_Pressure( k ) - Predictor%Level_Pressure( k -1 ) 
      
      Altitude( j ) =  Altitude( j -1 ) + delp * Predictor%Temperature(k) / &
	                 ( G0 * m_air / R0 * Predictor%Layer_Pressure(k) )
      Altitude_TL( j ) =  Altitude_TL( j -1 ) + delp * Predictor_TL%Temperature(k) / &
	                 ( G0 * m_air / R0 * Predictor%Layer_Pressure(k) ) &
			 - delp * Predictor%Temperature(k) * m_air_TL &
			 /( G0 * m_air**TWO / R0 * Predictor%Layer_Pressure(k) )
      			 
    ENDDO    
    
    DO j = 1, n_Layers
      k = n_Layers + 1 - j 
      Delta_Z(j) = (Altitude( k ) - Altitude( k -1 )) * 1000.0_fp
     
      Predictor_TL%Altitude( j ) = ( Altitude_TL( k ) + Altitude_TL( k -1 ) ) / TWO * 1000.0_fp
      
      Delta_Z_TL(j) = (Altitude_TL( k ) - Altitude_TL( k -1 )) * 1000.0_fp
    ENDDO
 
     
    ! -- water vapor
    Error_Status =  PPMV_to_KMOL_TL( Predictor%Layer_Pressure(1:n_Layers), &       	     
		  		     Predictor%Temperature(1:n_Layers), &    	     
		  		     Delta_Z(1:n_Layers), &	        	     
                  		     vm_H2O(1:n_Layers), &
				     Predictor_TL%Temperature(1:n_Layers), &    	     
		  		     Delta_Z_TL(1:n_Layers), &	        	     
                  		     vm_H2O_TL(1:n_Layers), & 
                                     Predictor_TL%Absorber(1:n_Layers, H2O_Index) )
    IF ( Error_Status/=SUCCESS ) THEN     
       CALL Display_Message( ROUTINE_NAME, &
                            'Error calculating the Tangent-linear of Predictor.', &
                            Error_Status)
      RETURN
    END IF

    !-- Other gases
    DO j = 1, n_Absorbers  ! j - ODCAPS absorber index

      IF( j == H2O_Index ) CYCLE   ! H2O is already done

      IF( Atmosphere%Absorber_Units( j ) == VOLUME_MIXING_RATIO_UNITS ) THEN

        CALL Interpolate_Profile( &
                    !Predictor%Index_Interpolation(1:n_Layers, :), &
                    Atmosphere%Absorber(1:n_Layers_usr, j), &
                    Atmosphere%Pressure(1:n_Layers_usr), &                 
                    Predictor%Layer_Pressure(1:n_Layers), &
                    vj(1:n_Layers, j))  
        		    
        CALL Interpolate_Profile_TL( &
                    !Predictor%Index_Interpolation(1:n_Layers, :), &
                    Atmosphere%Absorber(1:n_Layers_usr, j), &
                    Atmosphere%Pressure(1:n_Layers_usr), &                 
                    Predictor%Layer_Pressure(1:n_Layers), &
		    Atmosphere_TL%Absorber(1:n_Layers_usr, j), &
                    vj_TL(1:n_Layers, j))  

        Error_Status =  PPMV_to_KMOL_TL( Predictor%Layer_Pressure(1:n_Layers), &	   
        				 Predictor%Temperature(1:n_Layers), & 	 
        				 Delta_Z(1:n_Layers), &				 
        				 vj(1:n_Layers, j), &
        				 Predictor_TL%Temperature(1:n_Layers), & 
        				 Delta_Z_TL(1:n_Layers), &			 
        				 vj_TL(1:n_Layers,j), & 
        				 Predictor_TL%Absorber(1:n_Layers,j), &
					 Predictor%Absorber(1:n_Layers, H2O_Index) )
					 
        IF ( Error_Status/=SUCCESS ) THEN     
           CALL Display_Message( ROUTINE_NAME, &
        			'Error calculating the Tangent-linear of Predictor.', &
        			Error_Status)
          RETURN
        END IF

      ELSE

        Error_Status = FAILURE                                                        
        WRITE( Message, '( "The absorber units type ", i4, " is currently not supported")') &
                            Atmosphere%Absorber_Units(j)                              
        CALL Display_Message( ROUTINE_NAME, &                                         
                              TRIM( Message ), &                                      
                              Error_Status )                                         
        RETURN                                                                        
                                 
      ENDIF

    ENDDO

    ! ODCAPS input profile altitude (default)
    Predictor_TL%Altitude(1:n_Layers) = ZERO

    IF ( CO_Index == 0 .AND. CH4_Index == 0 ) THEN
       
      Predictor_TL%Absorber(1:n_Layers,n_Absorbers+1) = CO_Amount_TL(1:n_Layers)
       
      Predictor_TL%Absorber(1:n_Layers,n_Absorbers+2) = CH4_Amount_TL(1:n_Layers)
       
     
    ELSE IF ( CO_Index > 0 .AND. CH4_Index == 0  ) THEN
      Predictor_TL%Absorber(1:n_Layers,n_Absorbers+1) = CH4_Amount_TL(1:n_Layers)
     
    ELSE IF ( CO_Index == 0 .AND. CH4_Index > 0  ) THEN
      Predictor_TL%Absorber(1:n_Layers,n_Absorbers+1) = CO_Amount_TL(1:n_Layers)
  
    ENDIF

    IF ( CO2_Index > 0 ) THEN
      Predictor_TL%CO2_Amount(1:n_Layers) = Predictor_TL%Absorber(1:n_Layers, CO2_Index)
    ENDIF
    
    IF ( SO2_Index > 0 ) THEN
      Predictor_TL%SO2_Amount(1:n_Layers) = Predictor_TL%Absorber(1:n_Layers, SO2_Index)
    ENDIF
    
    IF ( HNO3_Index > 0 ) THEN
      Predictor_TL%HNO3_Amount(1:n_Layers) = Predictor_TL%Absorber(1:n_Layers,HNO3_Index)
    ENDIF

    IF ( N2O_Index > 0 ) THEN
      Predictor_TL%N2O_Amount(1:n_Layers) = Predictor_TL%Absorber(1:n_Layers,N2O_Index)
    ENDIF
    
     
    ! ODCAPS Secant Zenith for different altitude

    ! ODCAPS Fix and Trace Gas Multipliers
    CALL Fix_And_Trace_Gas_Multi_TL( Sensor_Index,    &  ! Input
                                     GeometryInfo,    &  ! Input
                                     Predictor,   &  ! Input
                                     Predictor_TL)   ! In/Output
				
  END SUBROUTINE ConvertToODCAPSprofile_TL


  !----------------------------------------------------------------
  ! The SUBROUTINE interpolates the user profiles on ODCAPS pressure
  ! grids; it also convert profile units to the required units
  ! used in ODCAPS modules. Adjoint model. 
  !----------------------------------------------------------------
   
  SUBROUTINE ConvertToODCAPSProfile_AD( Atmosphere,       &   ! Input
                                     Predictor,    &   ! Input
                                     Predictor_AD, &   ! Input
                                     GeometryInfo,     &   ! Input
				     Sensor_Index,     &   ! Input
                                     Atmosphere_AD )       ! In/Output
    !-- Inputs
    TYPE( CRTM_Atmosphere_type ),     INTENT( IN )     :: Atmosphere
    TYPE( Predictor_type ), INTENT( IN )     :: Predictor
    TYPE( CRTM_GeometryInfo_type ),   INTENT( IN )     :: GeometryInfo
    TYPE( Predictor_type ), INTENT( IN OUT ) :: Predictor_AD
    INTEGER,                          INTENT( IN )     :: Sensor_Index

    !-- In/Outputs
    TYPE( CRTM_Atmosphere_type ),    INTENT( IN OUT)  :: Atmosphere_AD

    ! ---------------
    ! Function result
    ! ---------------
    INTEGER :: Error_Status

    ! ----------------
    ! Local parameters
    ! ----------------
 
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'ConvertToODCAPSprofile_AD'
    
    ! ---------------
    ! Local variables
    ! ---------------
    
    CHARACTER(256) :: Message

    REAL(fp), DIMENSION(Predictor%n_Layers) :: temperature_AD !!,tempzero_AD 
    REAL(fp), DIMENSION(Predictor%n_Layers) :: CO_Amount_AD, CH4_Amount_AD 
    REAL(fp), DIMENSION(Predictor%n_Layers) :: vm_H2O, vm_H2O_AD, Delta_Z, Delta_Z_AD 
    REAL(fp), DIMENSION(0:Predictor%n_Layers) :: Altitude, Altitude_AD
    REAL(fp), DIMENSION(Predictor%n_Layers, MAX_N_ABSORBERS_ODCAPS) :: vj, vj_AD   
    INTEGER       :: n_Layers_usr, n_Layers, n_Absorbers, j, k 
    INTEGER       :: H2O_Index,O3_index,CO_Index, CH4_Index
    INTEGER       :: CO2_Index, SO2_Index, HNO3_Index, N2O_Index   
    REAL(fp), DIMENSION(Predictor%n_Layers) :: m_air, delp, m_air_AD 
    REAL(fp), DIMENSION(Atmosphere%n_Layers) :: mr_H2O, mr_H2O_AD
     

    Error_Status = SUCCESS

    n_Layers_usr = Atmosphere%n_Layers
    n_Absorbers  = Atmosphere%n_Absorbers

    !----------------------------------------------------------------------
    ! set the ODCAPS pressure profile into Predictor;      
    ! the thickness of the ODCAPS surface layer is adjusted to that    
    ! between the ODCAPS upper boundary of the surface layer and the
    ! surface pressure of the user pressure profile.
    !----------------------------------------------------------------------

    !--------------------------------------------------------------------------
    !       -- GET THE BOTTOM LAYER NUMBER AND FRACTIONAL MULTIPLIER --        
    !--------------------------------------------------------------------------

    n_Layers = Predictor%n_Layers  

    ! -- find user water vapor index
    H2O_Index = MINLOC( ABS( Atmosphere%Absorber_ID - H2O_ID ), DIM = 1 )

    !----------------------------------------------------------------
    ! -- Optional index for user profile, but are necessary for ODCAPS
    !---------------------------------------------------------------- 

    ! -- find user CO index
    j = COUNT( Atmosphere%Absorber_ID(1:n_absorbers) == CO_ID ) 
 
    IF ( j < 1) THEN ! no CO gas
    ! Reset CO_Amount if input Profile lacks CO profile
      CO_Index = 0
    ELSE
      CO_Index = MINLOC( ABS( Atmosphere%Absorber_ID - CO_ID ), DIM = 1 ) 
    ENDIF
    
    ! -- find user CH4 index
    j = COUNT( Atmosphere%Absorber_ID(1:n_absorbers) == CH4_ID ) 
 
    IF ( j < 1) THEN ! no CH4 gas
    ! Reset CH4_Amount if input Profile lacks CH4 profile
      CH4_Index = 0
    ELSE
      CH4_Index = MINLOC( ABS( Atmosphere%Absorber_ID - CH4_ID ), DIM = 1 ) 
    ENDIF
    
    ! -- find user CO2 index
    j = COUNT( Atmosphere%Absorber_ID(1:n_absorbers) == CO2_ID ) 
 
    IF ( j < 1) THEN ! no CO2 gas
    ! Reset CO2_Amount if input Profile lacks CO2 profile
      CO2_Index = 0
    ELSE
      CO2_Index = MINLOC( ABS( Atmosphere%Absorber_ID - CO2_ID ), DIM = 1 ) 
    ENDIF

    ! -- find user SO2 index
    j = COUNT( Atmosphere%Absorber_ID(1:n_absorbers) == SO2_ID ) 
 
    IF ( j < 1) THEN ! no SO2 gas
    ! Reset SO2_Amount if input Profile lacks SO2 profile
      SO2_Index = 0
    ELSE
      SO2_Index = MINLOC( ABS( Atmosphere%Absorber_ID - SO2_ID ), DIM = 1 ) 
    ENDIF

    ! -- find user HNO3 index
    j = COUNT( Atmosphere%Absorber_ID(1:n_absorbers) == HNO3_ID ) 
 
    IF ( j < 1) THEN ! no HNO3 gas
    ! Reset HNO3_Amount if input Profile lacks HNO3 profile
      HNO3_Index = 0
    ELSE
      HNO3_Index = MINLOC( ABS( Atmosphere%Absorber_ID - HNO3_ID ), DIM = 1 ) 
    ENDIF

    ! -- find user N2O index
    j = COUNT( Atmosphere%Absorber_ID(1:n_absorbers) == N2O_ID ) 
 
    IF ( j < 1) THEN ! no N2O gas
    ! Reset N2O_Amount if input Profile lacks N2O profile
      N2O_Index = 0
    ELSE
      N2O_Index = MINLOC( ABS( Atmosphere%Absorber_ID - N2O_ID ), DIM = 1 ) 
    ENDIF
    ! Forward Model
    ! -- water vapor
    IF( Atmosphere%Absorber_Units(H2O_Index) == MASS_MIXING_RATIO_UNITS  )THEN
      
      Error_Status = MR_to_PPMV (Atmosphere%Absorber(1:n_Layers_usr, H2O_Index), &
                                 mr_H2O(1:n_Layers_usr) )
				 
      IF ( Error_Status/=SUCCESS ) THEN     
       CALL Display_Message( ROUTINE_NAME, &
                            'Error calculating the Tangent-linear of Predictor.', &
                            Error_Status )
       RETURN
      END IF
 
      CALL Interpolate_Profile( &
                  !Predictor%Index_Interpolation(1:n_Layers, :), &
                  mr_H2O(1:n_Layers_usr), &
                  Atmosphere%Pressure(1:n_Layers_usr), &
                  Predictor%Layer_Pressure(1:n_Layers), &
                  vm_H2O(1:n_Layers))
      
    ELSE IF( Atmosphere%Absorber_Units(H2O_Index) == VOLUME_MIXING_RATIO_UNITS )THEN

        CALL Interpolate_Profile( &
                  !Predictor%Index_Interpolation(1:n_Layers, :), &
                  Atmosphere%Absorber(1:n_Layers_usr, H2O_Index), &
                  Atmosphere%Pressure(1:n_Layers_usr), &
                  Predictor%Layer_Pressure(1:n_Layers), &
                  vm_H2O(1:n_Layers))

    ELSE

      Error_Status = FAILURE                                                        
      WRITE( Message, '( "The water vapor units type ", i4, " is currently not supported")') & 
                          Atmosphere%Absorber_Units(H2O_Index)                              
      CALL Display_Message( ROUTINE_NAME, &                                         
                            TRIM( Message ), &                                      
                            Error_Status )                                         
      RETURN                                                                        
    ENDIF

    ! Calculate the profile altitude and layer thickness
    Altitude( 0 ) = GeometryInfo%Surface_Altitude

    DO j = 1, n_Layers
      k = n_Layers + 1 - j 
    ! Average wet air mass
      m_air(j) = (ONE - vm_H2O( k ) * 1.0e-6_fp ) * MW_DRYAIR + &
	         vm_H2O( k ) * 1.0e-6_fp * MW_H2O
      delp(j) = Predictor%Level_Pressure( k ) - Predictor%Level_Pressure( k -1 ) 
      
      Altitude( j ) =  Altitude( j -1 ) + delp(j) * Predictor%Temperature(k) / &
	                 ( G0 * m_air(j) / R0 * Predictor%Layer_Pressure(k) )
    ENDDO    
    
    DO j = 1, n_Layers
      k = n_Layers + 1 - j 
      Delta_Z(j) = (Altitude( k ) - Altitude( k -1 )) * 1000.0_fp   
    ENDDO

     
    !-- Other gases
    DO j = 1, n_Absorbers  ! j - ODCAPS absorber index

      IF( j == H2O_Index ) CYCLE   ! H2O is already done

      IF( Atmosphere%Absorber_Units( j ) == VOLUME_MIXING_RATIO_UNITS ) THEN

        CALL Interpolate_Profile( &
                    !Predictor%Index_Interpolation(1:n_Layers, :), &
                    Atmosphere%Absorber(1:n_Layers_usr, j), &
                    Atmosphere%Pressure(1:n_Layers_usr), &                 
                    Predictor%Layer_Pressure(1:n_Layers), &
                    vj(1:n_Layers, j))  
 
      ELSE

        Error_Status = FAILURE                                                        
        WRITE( Message, '( "The absorber units type ", i4, " is currently not supported")') &
                            Atmosphere%Absorber_Units(j)                              
        CALL Display_Message( ROUTINE_NAME, &                                         
                              TRIM( Message ), &                                      
                              Error_Status )                                         
        RETURN                                                                        
                                 
      ENDIF

    ENDDO

    

    ! Adjoint Model
    ! ODCAPS Fix and Trace Gas Multipliers
    CALL Fix_And_Trace_Gas_Multi_AD( Sensor_Index,    &  ! Input
                                     GeometryInfo,    &  ! Input
                                     Predictor,   &  ! Input
                                     Predictor_AD)   ! In/Output
    ! ODCAPS Secant Zenith for different altitude
    

    IF ( CO_Index == 0 .AND. CH4_Index == 0 ) THEN
       
      CO_Amount_AD(1:n_Layers) = Predictor_AD%Absorber(1:n_Layers,n_Absorbers+1)
       
      CH4_Amount_AD(1:n_Layers)= Predictor_AD%Absorber(1:n_Layers,n_Absorbers+2) 
       
 
    ELSE IF ( CO_Index > 0 .AND. CH4_Index == 0  ) THEN
      CH4_Amount_AD(1:n_Layers) = Predictor_AD%Absorber(1:n_Layers,n_Absorbers+1) 
     
    ELSE IF ( CO_Index == 0 .AND. CH4_Index > 0  ) THEN
      CO_Amount_AD(1:n_Layers) = Predictor_AD%Absorber(1:n_Layers,n_Absorbers+1) 
  
    ENDIF

    IF ( CO2_Index > 0 ) THEN
      Predictor_AD%Absorber(1:n_Layers, CO2_Index) = Predictor_AD%Absorber(1:n_Layers, CO2_Index) &
                      + Predictor_AD%CO2_Amount(1:n_Layers)  
    ENDIF
    
    IF ( SO2_Index > 0 ) THEN
      Predictor_AD%Absorber(1:n_Layers, SO2_Index) = Predictor_AD%Absorber(1:n_Layers, SO2_Index) &
                      + Predictor_AD%SO2_Amount(1:n_Layers)  
    ENDIF
    
    IF ( HNO3_Index > 0 ) THEN
      Predictor_AD%Absorber(1:n_Layers,HNO3_Index) = Predictor_AD%Absorber(1:n_Layers,HNO3_Index) &
                      + Predictor_AD%HNO3_Amount(1:n_Layers)  
    ENDIF

    IF ( N2O_Index > 0 ) THEN
      Predictor_AD%Absorber(1:n_Layers,N2O_Index) = Predictor_AD%Absorber(1:n_Layers,N2O_Index) &
                      + Predictor_AD%N2O_Amount(1:n_Layers) 
    ENDIF
    
    Predictor_AD%Altitude = ZERO
    
    Delta_Z_AD(:) = ZERO
    vj_AD(:,:) = ZERO
    vm_H2O_AD(:) = ZERO
    
    !-- Other gases
    DO j = 1, n_Absorbers  ! j - ODCAPS absorber index

      IF( j == H2O_Index ) CYCLE   ! H2O is already done

      IF( Atmosphere%Absorber_Units( j ) == VOLUME_MIXING_RATIO_UNITS ) THEN
        Error_Status =  PPMV_to_KMOL_AD( Predictor%Layer_Pressure(1:n_Layers), &	   
        				 Predictor%Temperature(1:n_Layers), & 	 
        				 Delta_Z(1:n_Layers), &				 
        				 vj(1:n_Layers, j), &
        				 Predictor_AD%Absorber(1:n_Layers,j), &
					 Predictor_AD%Temperature(1:n_Layers), &  
        				 Delta_Z_AD(1:n_Layers), &			 
        				 vj_AD(1:n_Layers,j), & 
 					 Predictor%Absorber(1:n_Layers, H2O_Index) )
					 
        IF ( Error_Status/=SUCCESS ) THEN     
           CALL Display_Message( ROUTINE_NAME, &
        			'Error calculating the Tangent-linear of Predictor.', &
        			Error_Status)
          RETURN
        END IF

        CALL Interpolate_Profile_AD( &
                    !Predictor%Index_Interpolation(1:n_Layers, :), &
                    Atmosphere%Absorber(1:n_Layers_usr, j), &
                    Atmosphere%Pressure(1:n_Layers_usr), &                 
                    Predictor%Layer_Pressure(1:n_Layers), &
		    vj_AD(1:n_Layers, j), &
                    Atmosphere_AD%Absorber(1:n_Layers_usr, j))  

      ELSE

        Error_Status = FAILURE                                                        
        WRITE( Message, '( "The absorber units type ", i4, " is currently not supported")') &
                            Atmosphere%Absorber_Units(j)                              
        CALL Display_Message( ROUTINE_NAME, &                                         
                              TRIM( Message ), &                                      
                              Error_Status )                                         
        RETURN                                                                        
                                 
      ENDIF

    ENDDO

    ! -- water vapor
 	
    Error_Status =  PPMV_to_KMOL_AD( Predictor%Layer_Pressure(1:n_Layers), &       	     
		  		     Predictor%Temperature(1:n_Layers), &    	     
		  		     Delta_Z(1:n_Layers), &	        	     
                  		     vm_H2O(1:n_Layers), &
				     Predictor_AD%Absorber(1:n_Layers, H2O_Index), &
				     Predictor_AD%Temperature(1:n_Layers), &   
		  		     Delta_Z_AD(1:n_Layers), &	        	     
                  		     vm_H2O_AD(1:n_Layers) )
				     
    IF ( Error_Status/=SUCCESS ) THEN     
       CALL Display_Message( ROUTINE_NAME, &
                            'Error calculating the Tangent-linear of Predictor.', &
                            Error_Status )
      RETURN
    END IF

    
    Altitude_AD( : ) = ZERO
    m_air_AD(:) = ZERO
    
    DO j = 1, n_Layers
      k = n_Layers + 1 - j 
      Altitude_AD( k ) = Altitude_AD(k) + (Delta_Z_AD(j) + & 
                         Predictor_AD%Altitude( j )/TWO)* 1000.0_fp
      Altitude_AD( k -1  ) = Altitude_AD( K -1 ) + (Predictor_AD%Altitude( j )/TWO - &
                            Delta_Z_AD(j)) * 1000.0_fp
      
      
    ENDDO

    DO j = n_Layers, 1,  -1
      k = n_Layers + 1 - j 
      Altitude_AD( j -1 ) = Altitude_AD( j -1 ) + Altitude_AD( j )  
      Predictor_AD%Temperature(k) = Predictor_AD%Temperature(k) + &
                         delp(j) * Altitude_AD( j ) / &
	                 ( G0 * m_air(j) / R0 * Predictor%Layer_Pressure(k) ) 
      m_air_AD(j) =  m_air_AD(j) - delp(j) * Predictor%Temperature(k) * Altitude_AD( j ) &
			 /( G0 * m_air(j)**TWO / R0 * Predictor%Layer_Pressure(k) )			 
      
      vm_H2O_AD(K) = vm_H2O_AD(K) + m_air_AD(j)* 1.0e-6_fp  * ( MW_H2O - MW_DRYAIR)
      
      Altitude_AD( j ) = ZERO
      m_air_AD(j) = ZERO	       
    ENDDO    
    Altitude_AD( 0 ) = ZERO
    mr_H2O_AD(:) = ZERO

    ! -- water vapor
    IF( Atmosphere%Absorber_Units(H2O_Index) == MASS_MIXING_RATIO_UNITS  )THEN
      CALL Interpolate_Profile_AD( &
                  !Predictor%Index_Interpolation(1:n_Layers, :), &
                  mr_H2O(1:n_Layers_usr), &
                  Atmosphere%Pressure(1:n_Layers_usr), &
                  Predictor%Layer_Pressure(1:n_Layers), &
                  vm_H2O_AD(1:n_Layers), &
		  mr_H2O_AD(1:n_Layers_usr))
      
      Error_Status = MR_to_PPMV_AD (Atmosphere%Absorber(1:n_Layers_usr, H2O_Index), &
                                    mr_H2O(1:n_Layers_usr), &
				    mr_H2O_AD(1:n_Layers_usr), &
				    Atmosphere_AD%Absorber(1:n_Layers_usr, H2O_Index) )
				 
      IF ( Error_Status/=SUCCESS ) THEN     
       CALL Display_Message( ROUTINE_NAME, &
                            'Error calculating the Adjoint of Predictor.', &
                            Error_Status )
       RETURN
      END IF

 
    ELSE IF( Atmosphere%Absorber_Units(H2O_Index) == VOLUME_MIXING_RATIO_UNITS )THEN
        CALL Interpolate_Profile_AD( &
                  !Predictor%Index_Interpolation(1:n_Layers, :), &
                  Atmosphere%Absorber(1:n_Layers_usr, H2O_Index), &
                  Atmosphere%Pressure(1:n_Layers_usr), &
                  Predictor%Layer_Pressure(1:n_Layers), &
                  vm_H2O_AD(1:n_Layers), &
		  Atmosphere_AD%Absorber(1:n_Layers_usr, H2O_Index))

    ELSE

      Error_Status = FAILURE                                                        
      WRITE( Message, '( "The water vapor units type ", i4, " is currently not supported")') & 
                          Atmosphere%Absorber_Units(H2O_Index)                              
      CALL Display_Message( ROUTINE_NAME, &                                         
                            TRIM( Message ), &                                      
                            Error_Status )                                         
      RETURN                                                                        
    ENDIF


    IF( CO_Index == 0  ) CO_Amount_AD(1:n_Layers)   = ZERO
    IF( CH4_Index == 0 ) CH4_Amount_AD(1:n_Layers)  = ZERO
    IF( CO2_Index == 0 ) Predictor_AD%CO2_Amount(1:n_Layers)  = ZERO
    IF( SO2_Index == 0 ) Predictor_AD%SO2_Amount(1:n_Layers)  = ZERO
    IF( HNO3_Index == 0) Predictor_AD%HNO3_Amount(1:n_Layers) = ZERO
    IF( N2O_Index == 0 ) Predictor_AD%N2O_Amount(1:n_Layers)  = ZERO

    !-------------------------------------------------
    ! Temperature profile interpolation on the ODCAPS grids
    !-------------------------------------------------

    temperature_AD( 1:n_Layers ) = Predictor_AD%Temperature( 1:n_Layers )
    CALL Interpolate_Profile_AD( &
                !Predictor%Index_Interpolation(1:n_Layers, :), &
                Atmosphere%Temperature(1:n_Layers_usr), &
                Atmosphere%Pressure(1:n_Layers_usr), &
                Predictor%Layer_Pressure(1:n_Layers), &
                temperature_AD(1:n_Layers), &
		Atmosphere_AD%Temperature )
        
 			
  END SUBROUTINE ConvertToODCAPSprofile_AD
 
  ! #--------------------------------------------------------------#
  ! # This Subroutine obtain the lowest ODCAPS layer according to   #
  ! # the user surface pressure                                    #
  ! #--------------------------------------------------------------#

  SUBROUTINE Get_ODCAPS_Layers( n_Layers, & ! Input, number of profile layers
                               Plev,     & ! Input, standard layer pres level boundaries
			       Psurf,    & ! Input, surface pressure
			       Lbot,     & ! Output, bottom layer number
			       Blmult)     ! Output, bot layer fractional mult

    ! -- Inputs
    INTEGER, INTENT( IN ) :: n_Layers
    
    REAL( fp ), DIMENSION( 0:MAX_N_ODCAPS_LAYERS),   INTENT( IN )  ::  Plev  ! 0:K
    REAL( fp ), INTENT( IN )  ::  Psurf

    ! -- Outputs
    INTEGER, INTENT( OUT ) :: Lbot 
    
    REAL( fp ), INTENT( OUT ) :: Blmult

    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER :: Lbotx  ! unrestricted bottom layer number
    REAL( fp ):: Delpx     ! 5% of layer thickness in pressure

    !  Determine Lbot from Psurf by comparing to Plev
       Lbotx = MAX_N_ODCAPS_LAYERS 
       
    10 CONTINUE
       Delpx = 0.05*( Plev(Lbotx) - Plev(Lbotx-1) )
       IF (Psurf  <  Plev(Lbotx-1)+Delpx) THEN
          Lbotx = Lbotx - 1
          GOTO 10
       ENDIF

    ! have bug here, if the input profile is arbitary instead of fix pressure layers for ODCAPS
    ! for example, if input profile only have 40 layers spread from 0.0050 to 1000hPa,
    ! and Lbotx=89, then we has problem
      ! IF (Lbotx > n_Layers) THEN
      !    Lbot = n_Layers 
      ! ELSE
      !    Lbot = Lbotx
      ! ENDIF

    ! Fix with Lbot = Lbotx 01/11/2007
       Lbot = Lbotx
       
    !  Calc bottom layer multiplier (fractional layer)
       Blmult = (Psurf - Plev(Lbot-1))/(Plev(Lbot) - Plev(Lbot-1))

  END SUBROUTINE Get_ODCAPS_Layers 
  
  ! #---------------------------------------------------------------#
  ! # This Subroutine compute the secant zenith angle for different #
  ! # layer height                                                  #
  ! #--------------------------------------------------------------#

  SUBROUTINE Compute_Secant_Zenith( GeometryInfo,  &  ! Input
                                    Predictor )   ! In/Output
                                       
    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    TYPE( CRTM_GeometryInfo_type ),  INTENT( IN )     :: GeometryInfo 

    ! -- In/Outputs
    TYPE( Predictor_type ), INTENT( IN OUT ) :: Predictor

    INTEGER :: Error_Status
    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Secant_Zenith'

    CHARACTER( 256 ) :: Message
    REAL(fp) :: SVA, Local_Zenith_Angle 
    REAL(fp) :: EVA
    REAL(fp) :: RJUNK1 
    REAL(fp) :: RJUNK2
    REAL(fp) :: Suncos       
    INTEGER :: L

    Error_Status = SUCCESS

    !#--------------------------------------------------------------------------#
    !#       -- GET THE BOTTOM LAYER NUMBER AND FRACTIONAL MULTIPLIER --        #
    !#--------------------------------------------------------------------------#
 			 
    ! Convert Sensor_Zenith_Angle or Sensor_Scan_Angle to viewing angle
       IF (GeometryInfo%Sensor_Zenith_Angle  > 0.0  .AND. &
           GeometryInfo%Sensor_Zenith_Angle  < 63.0 ) THEN
    !    Convert zenith angle at surface to view angle at satellite
           CALL SACONV( GeometryInfo%Sensor_Zenith_Angle, &
	         SATELLITE_HEIGHT*1000.0, Local_Zenith_Angle )
           SVA = Local_Zenith_Angle / DEGREES_TO_RADIANS
       ELSE
    !    Check if scan angle is valid
          IF (GeometryInfo%Sensor_Scan_Angle  > -49.6 .AND.  &
	      GeometryInfo%Sensor_Scan_Angle < 49.6 ) THEN
    !       View angle should be within a few degrees of scan angle
             SVA = ABS( GeometryInfo%Sensor_Scan_Angle )
          ELSE
             Error_Status = FAILURE
	     WRITE(Message,'( "Invalid angles for Sensor_Zenith_Angle ", 1PE11.4, &
	              &"and Sensor_Scan_Angle", E11.4 )' ) &
                    GeometryInfo%Sensor_Zenith_Angle,GeometryInfo%Sensor_Scan_Angle   
             CALL Display_Message( ROUTINE_NAME, &
                                   TRIM( Message ), &
                                   Error_Status )
            RETURN
          ENDIF
	ENDIF		              

    ! Truncate angle if too big
        IF ( SVA > MAX_SATELLITE_VIEW_ANGLE ) THEN
          SVA = MAX_SATELLITE_VIEW_ANGLE 
        ENDIF

    ! Convert from satellite to earth viewing angle (in radians)
        DO L = 1, Predictor%n_Layers
	  CALL VACONV( SVA, SATELLITE_HEIGHT, &
	               Predictor%Altitude(L),  EVA )
          Predictor%Secant_Sensor_Zenith( L ) =  GeometryInfo%Secant_Sensor_Zenith 
        ENDDO

    ! Calc total sun angle secant
        Predictor%Calc_Sun_Angle_Secant = .FALSE.

	IF( GeometryInfo%Source_Zenith_Angle > ZERO .AND. &
	    GeometryInfo%Source_Zenith_Angle < 89.9 )  &
	   Predictor%Calc_Sun_Angle_Secant = .TRUE.
        
	IF( Predictor%Calc_Sun_Angle_Secant) THEN
	   Suncos = COS( DEGREES_TO_RADIANS*GeometryInfo%Source_Zenith_Angle )
	   CALL SACONV( GeometryInfo%Source_Zenith_Angle, &
	                Predictor%Altitude(1),Local_Zenith_Angle)
	   Predictor%Source_COS_Layer1 = COS( Local_Zenith_Angle  )
    ! Total secant
	   RJUNK2 = Predictor%Secant_Sensor_Zenith( Predictor%n_Layers ) + ONE / Suncos
	   
    !  Calc non-unity fudge factor if total secant > 9
           IF (RJUNK2 .GT. 9.0) THEN

    !  fudge factor = true_total_secant/calc_total_secant
             Predictor%Sun_Fudge = RJUNK2 / 9.0
    
    !  truncated solar angle to use to calc SECSUN
             RJUNK1 = ACOS( 1.0 / ( 9.0 - &
	            Predictor%Secant_Sensor_Zenith( Predictor%n_Layers ) ) ) &
		    / DEGREES_TO_RADIANS
   
           ELSE
	     Predictor%Sun_Fudge = ONE
	     RJUNK1 = GeometryInfo%Source_Zenith_Angle
	   
	   ENDIF
	   
	   DO L = 1, Predictor%n_Layers
	     CALL SACONV( RJUNK1, Predictor%Altitude( L ), Local_Zenith_Angle  )
	     Predictor%Secant_Source_Zenith( L ) = Predictor%Secant_Sensor_Zenith( L ) &
	                           + ONE / COS( Local_Zenith_Angle  )
	   ENDDO			      
	   
	ENDIF
  
  END SUBROUTINE Compute_Secant_Zenith
  
  ! #---------------------------------------------------------------#
  ! # This Subroutine compute the secant zenith angle for different #
  ! # layer height, Tangent-linear model                            #
  ! #---------------------------------------------------------------#

  SUBROUTINE Compute_Secant_Zenith_TL( GeometryInfo,  &  ! Input
                                       Predictor, &  ! Input
                                       Predictor_TL) ! In/Output
				       
    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    TYPE( CRTM_GeometryInfo_type ),  INTENT( IN )     :: GeometryInfo 
    TYPE( Predictor_type ), INTENT( IN ) :: Predictor

    ! -- In/Outputs
    TYPE( Predictor_type ), INTENT( IN OUT ) :: Predictor_TL

    INTEGER :: Error_Status
    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Secant_Zenith_TL'

    CHARACTER( 256 ) :: Message
    REAL(fp) :: SVA, Local_Zenith_Angle, Local_Zenith_Angle_TL 
    REAL(fp) :: EVA, EVA_TL
    REAL(fp) :: RJUNK1 
    REAL(fp) :: RJUNK2
    REAL(fp) :: Suncos       
    INTEGER :: L

    Error_Status = SUCCESS

    !#--------------------------------------------------------------------------#
    !#       -- GET THE BOTTOM LAYER NUMBER AND FRACTIONAL MULTIPLIER --        #
    !#--------------------------------------------------------------------------#
    ! Convert Sensor_Zenith_Angle or Sensor_Scan_Angle to viewing angle
       IF (GeometryInfo%Sensor_Zenith_Angle  > 0.0  .AND. &
           GeometryInfo%Sensor_Zenith_Angle  < 63.0 ) THEN
    !    Convert zenith angle at surface to view angle at satellite
           CALL SACONV( GeometryInfo%Sensor_Zenith_Angle, &
	         SATELLITE_HEIGHT*1000.0, Local_Zenith_Angle )
           SVA = Local_Zenith_Angle / DEGREES_TO_RADIANS
       ELSE
    !    Check if scan angle is valid
          IF (GeometryInfo%Sensor_Scan_Angle  > -49.6 .AND.  &
	      GeometryInfo%Sensor_Scan_Angle < 49.6 ) THEN
    !       View angle should be within a few degrees of scan angle
             SVA = ABS( GeometryInfo%Sensor_Scan_Angle )
          ELSE
             Error_Status = FAILURE
	     WRITE(Message,'( "Invalid angles for Sensor_Zenith_Angle ", 1PE11.4, &
	              &"and Sensor_Scan_Angle", E11.4 )' ) &
                    GeometryInfo%Sensor_Zenith_Angle,GeometryInfo%Sensor_Scan_Angle   
             CALL Display_Message( ROUTINE_NAME, &
                                   TRIM( Message ), &
                                   Error_Status )
            RETURN
          ENDIF
	ENDIF		              

    ! Truncate angle if too big
        IF ( SVA > MAX_SATELLITE_VIEW_ANGLE ) THEN
          SVA = MAX_SATELLITE_VIEW_ANGLE 
        ENDIF

    ! Convert from satellite to earth viewing angle (in radians)
        DO L = 1, Predictor%n_Layers
	  CALL VACONV_TL( SVA, SATELLITE_HEIGHT, &
	         Predictor%Altitude(L), Predictor_TL%Altitude(L), EVA, EVA_TL)
          Predictor_TL%Secant_Sensor_Zenith( L ) = ZERO
        ENDDO

   
	IF( Predictor%Calc_Sun_Angle_Secant) THEN
	   Suncos = COS( DEGREES_TO_RADIANS*GeometryInfo%Source_Zenith_Angle )
	   CALL SACONV_TL( GeometryInfo%Source_Zenith_Angle, Predictor%Altitude(1), &
	            Predictor_TL%Altitude(1),Local_Zenith_Angle,Local_Zenith_Angle_TL )
 	   Predictor_TL%Source_COS_Layer1 = - SIN( Local_Zenith_Angle ) * Local_Zenith_Angle_TL
	   
    ! Total secant
	   RJUNK2 = Predictor%Secant_Sensor_Zenith( Predictor%n_Layers ) + ONE / Suncos
	   
    !  Calc non-unity fudge factor if total secant > 9
           IF (RJUNK2 .GT. 9.0_fp) THEN

    !  truncated solar angle to use to calc SECSUN
             RJUNK1 = ACOS( ONE / ( 9.0_fp - &
	            Predictor%Secant_Sensor_Zenith( Predictor%n_Layers ) ) ) &
		    / DEGREES_TO_RADIANS
   
           ELSE
 
 	     RJUNK1 = GeometryInfo%Source_Zenith_Angle
	     Predictor_TL%Sun_Fudge = ZERO
	   ENDIF
	   
	   DO L = 1, Predictor%n_Layers
	     CALL SACONV_TL( RJUNK1, Predictor%Altitude( L ), Predictor_TL%Altitude( L ),&
	                     Local_Zenith_Angle, Local_Zenith_Angle_TL )
	     Predictor_TL%Secant_Source_Zenith( L ) = Predictor_TL%Secant_Sensor_Zenith( L ) &
	            + SIN(Local_Zenith_Angle) * Local_Zenith_Angle_TL / (COS( Local_Zenith_Angle )**TWO)
	   ENDDO			      
	   
	ENDIF
  
  END SUBROUTINE Compute_Secant_Zenith_TL
  

  ! #---------------------------------------------------------------#
  ! # This Subroutine compute the secant zenith angle for different #
  ! # layer height, Adjoint model                                   #
  ! #---------------------------------------------------------------#

  SUBROUTINE Compute_Secant_Zenith_AD( GeometryInfo,  &  ! Input
                                       Predictor, &  ! Input
                                       Predictor_AD) ! In/Output
				       
    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    TYPE( CRTM_GeometryInfo_type ),  INTENT( IN )     :: GeometryInfo 
    TYPE( Predictor_type ), INTENT( IN )     :: Predictor

    ! -- In/Outputs
    TYPE( Predictor_type ), INTENT( IN OUT ) :: Predictor_AD

    INTEGER :: Error_Status
    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Secant_Zenith_AD'

    CHARACTER( 256 ) :: Message
    REAL(fp) :: SVA, Local_Zenith_Angle0, Local_Zenith_Angle1,Local_Zenith_Angle1_AD  
    REAL(fp), DIMENSION(Predictor%n_Layers) :: EVA, EVA_AD
    REAL(fp) :: RJUNK1 
    REAL(fp) :: RJUNK2
    REAL(fp) :: Suncos       
    INTEGER :: L
    REAL(fp), DIMENSION(Predictor%n_Layers) :: Local_Zenith_Angle,Local_Zenith_Angle_AD
         

    Error_Status = SUCCESS

    !#--------------------------------------------------------------------------#
    !#       -- GET THE BOTTOM LAYER NUMBER AND FRACTIONAL MULTIPLIER --        #
    !#--------------------------------------------------------------------------#
    ! Convert Sensor_Zenith_Angle or Sensor_Scan_Angle to viewing angle
       IF (GeometryInfo%Sensor_Zenith_Angle  > 0.0  .AND. &
           GeometryInfo%Sensor_Zenith_Angle  < 63.0 ) THEN
    !    Convert zenith angle at surface to view angle at satellite
           CALL SACONV( GeometryInfo%Sensor_Zenith_Angle, &
	         SATELLITE_HEIGHT*1000.0, Local_Zenith_Angle0 )
           SVA = Local_Zenith_Angle0 / DEGREES_TO_RADIANS
       ELSE
    !    Check if scan angle is valid
          IF (GeometryInfo%Sensor_Scan_Angle  > -49.6 .AND.  &
	      GeometryInfo%Sensor_Scan_Angle < 49.6 ) THEN
    !       View angle should be within a few degrees of scan angle
             SVA = ABS( GeometryInfo%Sensor_Scan_Angle )
          ELSE
             Error_Status = FAILURE
	     WRITE(Message,'( "Invalid angles for Sensor_Zenith_Angle ", 1PE11.4, &
	              &"and Sensor_Scan_Angle", E11.4 )' ) &
                    GeometryInfo%Sensor_Zenith_Angle,GeometryInfo%Sensor_Scan_Angle   
             CALL Display_Message( ROUTINE_NAME, &
                                   TRIM( Message ), &
                                   Error_Status )
            RETURN
          ENDIF
	ENDIF		              

    ! Truncate angle if too big
        IF ( SVA > MAX_SATELLITE_VIEW_ANGLE ) THEN
          SVA = MAX_SATELLITE_VIEW_ANGLE 
        ENDIF

    ! Convert from satellite to earth viewing angle (in radians)
        DO L = 1, Predictor%n_Layers
	  CALL VACONV( SVA, SATELLITE_HEIGHT, &
	               Predictor%Altitude(L),  EVA(L) )
        ENDDO

    ! Calc total sun angle secant
	IF( Predictor%Calc_Sun_Angle_Secant) THEN
	   Suncos = COS( DEGREES_TO_RADIANS*GeometryInfo%Source_Zenith_Angle )
	   CALL SACONV( GeometryInfo%Source_Zenith_Angle, &
	                Predictor%Altitude(1), Local_Zenith_Angle1)
	   RJUNK2 = Predictor%Secant_Sensor_Zenith( Predictor%n_Layers ) + ONE / Suncos
			
    !  Calc non-unity fudge factor if total secant > 9
           IF (RJUNK2 .GT. 9.0) THEN

    !  truncated solar angle to use to calc SECSUN
             RJUNK1 = ACOS( 1.0 / ( 9.0 - &
	            Predictor%Secant_Sensor_Zenith( Predictor%n_Layers ) ) ) &
		    / DEGREES_TO_RADIANS
   
           ELSE

 	     RJUNK1 = GeometryInfo%Source_Zenith_Angle
	   
	   ENDIF

	   DO L = 1, Predictor%n_Layers

	     CALL SACONV( RJUNK1, Predictor%Altitude( L ), Local_Zenith_Angle(L) )

 	   ENDDO
	
	ENDIF  			      
			
    ! Adjoint Model
	IF( Predictor%Calc_Sun_Angle_Secant) THEN
	
	   DO L = 1, Predictor%n_Layers
	   
             Predictor_AD%Secant_Sensor_Zenith( L ) = Predictor_AD%Secant_Sensor_Zenith( L ) &
	                     + Predictor_AD%Secant_Source_Zenith( L )
             
	     Local_Zenith_Angle_AD( L ) = SIN(Local_Zenith_Angle(L)) &
	            * Predictor_AD%Secant_Source_Zenith( L ) / (COS( Local_Zenith_Angle(L) )**TWO)
	     
	     CALL SACONV_AD( RJUNK1, Predictor%Altitude( L ), Local_Zenith_Angle_AD(L), &
	                       Predictor_AD%Altitude( L ) )	    
	   ENDDO
	   
	   Local_Zenith_Angle1_AD = - SIN( Local_Zenith_Angle1) * Predictor_AD%Source_COS_Layer1 
	   CALL SACONV_AD( GeometryInfo%Source_Zenith_Angle, Predictor%Altitude(1), &
     	                    Local_Zenith_Angle1_AD,Predictor_AD%Altitude(1) )
	   
	ENDIF  			      

    ! Convert from satellite to earth viewing angle (in radians)
        DO L = 1, Predictor%n_Layers
          EVA_AD(L) = SIN( EVA(L)) * Predictor_AD%Secant_Sensor_Zenith( L ) / (COS( EVA(L) ))**TWO
	  CALL VACONV_AD( SVA, SATELLITE_HEIGHT, &
	         Predictor%Altitude(L), EVA_AD(L), Predictor_AD%Altitude(L))
        ENDDO
   
  END SUBROUTINE Compute_Secant_Zenith_AD
  

  
  ! #---------------------------------------------------------------#
  ! # This Subroutine compute the fix and trace gases multipliers   #
  ! #---------------------------------------------------------------#

  SUBROUTINE Fix_And_Trace_Gas_Multi( Sensor_Index,  &  ! Input
                                      GeometryInfo,  &  ! Input
                                      Predictor)    ! In/Output
 
    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    INTEGER,                         INTENT( IN )     :: Sensor_Index
    TYPE( CRTM_GeometryInfo_type ),  INTENT( IN )     :: GeometryInfo 

    ! -- In/Outputs
    TYPE( Predictor_type ), INTENT( IN OUT ) :: Predictor


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Fix_And_Trace_Gas_Multi'


    !  Variables for fixed gases adjustment
    REAL( fp ) :: PWATER
    REAl( fp ) ::  GSCAL
    REAL( fp ) ::    A_F
    
    INTEGER :: L
    INTEGER :: H2O_Index
    TYPE( ODCAPS_TauCoeff_type ), POINTER  ::  TC => NULL()

    TC => ODCAPS_TC(Sensor_Index)
 

    !#--------------------------------------------------------------------------#
    !#              -- GET THE Atmosphere INDEX FOR WATER VAPOR --              #
    !#--------------------------------------------------------------------------#
    H2O_Index = MINLOC( ABS( Predictor%Absorber_ID - H2O_ID ), DIM = 1 )

    !#--------------------------------------------------------------------------#
    !#       -- CALC THE FIXED GASES GRAVITY CORRECTION FACTOR   --             #
    !#--------------------------------------------------------------------------#
    GSCAL=( 9.78050518 + 0.0518017 &
            * ( COS( (GeometryInfo%Latitude - 90.0 ) &
	    * DEGREES_TO_RADIANS ) )**2 ) / 9.80683613


    Layer_Loop : DO L = 1, Predictor%n_Layers 

      !  Calc the fixed gases correction term for this layer	      
         PWATER = KMOLE*Predictor%Absorber(L,H2O_Index)*Predictor%Temperature(L)/ &
	          (STDDEN*STDTMP*100.0*TC%Ref_Profile_Data(2,L)) 
         A_F = ( ONE - PMULT*PWATER/TC%Ref_Profile_Data(3,L) )/ &
	       ( TC%Fix_Gases_Adjustment(L)*GSCAL ) 
         
	 Predictor%Fix_Amount_Multiplier( L ) = A_F
      
      !  CO2 mult=1 when prof amount = 1.03 * ref amount
         Predictor%CO2_Multiplier( L ) = 33.3333_fp*( Predictor%CO2_Amount(L) &
	     - A_F * real(TC%Ref_Profile_Data(5,L),fp) ) &
	     / real(TC%Ref_Profile_Data(5,L),fp)
	      
      !  Ignore changes in CO2 of less than ~0.03%
         IF ( ABS( Predictor%CO2_Multiplier( L ) ) < 1.0E-2  ) & 
	     Predictor%CO2_Multiplier( L ) = ZERO

      !   SO2 mult=1 when prof amount = 1000 * ref amount
          Predictor%SO2_Multiplier( L ) = 1.0010E-3*( Predictor%SO2_Amount(L) &
	     - A_F * TC%Ref_Profile_Data(10,L) ) / TC%Ref_Profile_Data(10,L)

      !   Ignore changes in SO2 of less than ~10%
          IF ( ABS( Predictor%SO2_Multiplier( L ) ) < 1.0E-4 ) &
	     Predictor%SO2_Multiplier( L ) = ZERO

      !   HNO3 mult=1 when prof amount = 2 * ref amount
          Predictor%HNO3_Multiplier( L ) = ( Predictor%HNO3_Amount(L) &
	     - A_F * TC%Ref_Profile_Data(11,L) ) / TC%Ref_Profile_Data(11,L)
      
      !   Ignore changes in HNO3 less than ~1%
          IF ( ABS( Predictor%HNO3_Multiplier( L ) ) < 1.0E-2 ) &
	     Predictor%HNO3_Multiplier( L ) = ZERO

      !   N2O mult=-1 when prof amount = 0.75 * ref amount
          Predictor%N2O_Multiplier( L ) = 4.0* ( Predictor%N2O_Amount(L) &
	     - A_F * TC%Ref_Profile_Data(12,L) ) / TC%Ref_Profile_Data(12,L)
      
      !   Ignore changes in N2O less than ~0.3%
          IF ( ABS( Predictor%N2O_Multiplier( L ) ) < 1.0E-2 ) &
	     Predictor%N2O_Multiplier( L ) = ZERO
 	 
    END DO Layer_Loop

    NULLIFY(TC)
  END SUBROUTINE Fix_And_Trace_Gas_Multi


  ! #---------------------------------------------------------------#
  ! # This Subroutine compute the fix and trace gases multipliers   #
  ! # Tangent-linear model                                          #
  ! #---------------------------------------------------------------#

  SUBROUTINE Fix_And_Trace_Gas_Multi_TL( Sensor_Index,  &  ! Input
                                         GeometryInfo,  &  ! Input
                                         Predictor, &  ! Input
					 Predictor_TL) ! In/Output
 
    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    INTEGER,                         INTENT( IN )     :: Sensor_Index
    TYPE( CRTM_GeometryInfo_type ),  INTENT( IN )     :: GeometryInfo 
    TYPE( Predictor_type ),INTENT( IN )     :: Predictor

    ! -- In/Outputs
    TYPE( Predictor_type ), INTENT( IN OUT ) :: Predictor_TL


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Fix_And_Trace_Gas_Multi_TL'


    !  Variables for fixed gases adjustment
    REAL( fp ) ::  PWATER_TL
    REAl( fp ) ::  GSCAL
    REAL( fp ) ::  A_F_TL
    
    INTEGER :: L
    INTEGER :: H2O_Index
    TYPE( ODCAPS_TauCoeff_type ), POINTER  ::  TC => NULL()

    TC => ODCAPS_TC(Sensor_Index)
 
    !#--------------------------------------------------------------------------#
    !#              -- GET THE Atmosphere INDEX FOR WATER VAPOR --              #
    !#--------------------------------------------------------------------------#
    H2O_Index = MINLOC( ABS( Predictor%Absorber_ID - H2O_ID ), DIM = 1 )

    !#--------------------------------------------------------------------------#
    !#       -- CALC THE FIXED GASES GRAVITY CORRECTION FACTOR   --             #
    !#--------------------------------------------------------------------------#
    GSCAL=( 9.78050518 + 0.0518017 &
            * ( COS( (GeometryInfo%Latitude - 90.0 ) &
	    * DEGREES_TO_RADIANS ) )**2 ) / 9.80683613


    Layer_Loop : DO L = 1, Predictor%n_Layers 

      !  Calc the fixed gases correction term for this layer	      
         PWATER_TL = KMOLE*Predictor_TL%Absorber(L,H2O_Index)*Predictor%Temperature(L)/ &
	             (STDDEN*STDTMP*100.0*TC%Ref_Profile_Data(2,L))  &
		   + KMOLE*Predictor%Absorber(L,H2O_Index)*Predictor_TL%Temperature(L)/ &
	             (STDDEN*STDTMP*100.0*TC%Ref_Profile_Data(2,L))
		   
         A_F_TL = - PMULT*PWATER_TL/TC%Ref_Profile_Data(3,L) / &
	            ( TC%Fix_Gases_Adjustment(L)*GSCAL )
	 
	 Predictor_TL%Fix_Amount_Multiplier( L ) = A_F_TL
      
      !  CO2 mult=1 when prof amount = 1.03 * ref amount
	 Predictor_TL%CO2_Multiplier( L ) = 33.3333_fp * ( Predictor_TL%CO2_Amount(L) &
	     - A_F_TL * real(TC%Ref_Profile_Data(5,L),fp) ) &
	     / real(TC%Ref_Profile_Data(5,L),fp)    
      
      !  Ignore changes in CO2 of less than ~0.03%
         IF ( ABS( Predictor%CO2_Multiplier( L ) ) < 1.0E-2  ) & 
	     Predictor_TL%CO2_Multiplier( L ) = ZERO

      !   SO2 mult=1 when prof amount = 1000 * ref amount
          Predictor_TL%SO2_Multiplier( L ) = 1.0010E-3*( Predictor_TL%SO2_Amount(L) &
	     - A_F_TL * TC%Ref_Profile_Data(10,L) ) / TC%Ref_Profile_Data(10,L)

      !   Ignore changes in SO2 of less than ~10%
          IF ( ABS( Predictor%SO2_Multiplier( L ) ) < 1.0E-4 ) &
	     Predictor_TL%SO2_Multiplier( L ) = ZERO

      !   HNO3 mult=1 when prof amount = 2 * ref amount
          Predictor_TL%HNO3_Multiplier( L ) = ( Predictor_TL%HNO3_Amount(L) &
	     - A_F_TL * TC%Ref_Profile_Data(11,L) ) / TC%Ref_Profile_Data(11,L)
      
      !   Ignore changes in HNO3 less than ~1%
          IF ( ABS( Predictor%HNO3_Multiplier( L ) ) < 1.0E-2 ) &
	     Predictor_TL%HNO3_Multiplier( L ) = ZERO

      !   N2O mult=-1 when prof amount = 0.75 * ref amount
          Predictor_TL%N2O_Multiplier( L ) = 4.0 * ( Predictor_TL%N2O_Amount(L) &
	     - A_F_TL * TC%Ref_Profile_Data(12,L) ) / TC%Ref_Profile_Data(12,L)
      
      !   Ignore changes in N2O less than ~0.3%
          IF ( ABS( Predictor%N2O_Multiplier( L ) ) < 1.0E-2 ) &
	     Predictor_TL%N2O_Multiplier( L ) = ZERO
 	 
    END DO Layer_Loop
    
    NULLIFY(TC)
  END SUBROUTINE Fix_And_Trace_Gas_Multi_TL


  ! #---------------------------------------------------------------#
  ! # This Subroutine compute the fix and trace gases multipliers   #
  ! # Adjoint model                                                 #
  ! #---------------------------------------------------------------#

  SUBROUTINE Fix_And_Trace_Gas_Multi_AD( Sensor_Index,  &  ! Input
                                         GeometryInfo,  &  ! Input
                                         Predictor, &  ! Input	    
					 Predictor_AD) ! In/Output      
 
    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    INTEGER,                         INTENT( IN )     :: Sensor_Index
    TYPE( CRTM_GeometryInfo_type ),  INTENT( IN )     :: GeometryInfo 
    TYPE( Predictor_type ),INTENT( IN )     :: Predictor

    ! -- In/Outputs
    TYPE( Predictor_type ), INTENT( IN OUT ) :: Predictor_AD


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Fix_And_Trace_Gas_Multi_AD'


    !  Variables for fixed gases adjustment
    REAL( fp ) ::  PWATER_AD
    REAl( fp ) ::  GSCAL
    REAL( fp ) ::  A_F_AD
    
    INTEGER :: L
    INTEGER :: H2O_Index
    TYPE( ODCAPS_TauCoeff_type ), POINTER  ::  TC => NULL()

    TC => ODCAPS_TC(Sensor_Index)
 
    !#--------------------------------------------------------------------------#
    !#              -- GET THE Atmosphere INDEX FOR WATER VAPOR --              #
    !#--------------------------------------------------------------------------#
    H2O_Index = MINLOC( ABS( Predictor%Absorber_ID - H2O_ID ), DIM = 1 )

    !#--------------------------------------------------------------------------#
    !#       -- CALC THE FIXED GASES GRAVITY CORRECTION FACTOR   --             #
    !#--------------------------------------------------------------------------#
    GSCAL=( 9.78050518 + 0.0518017 &
            * ( COS( (GeometryInfo%Latitude - 90.0 ) &
	    * DEGREES_TO_RADIANS ) )**2 ) / 9.80683613
    
    A_F_AD = ZERO
     
    ! Adjoint Model
    Adjoint_Layer_Loop : DO L = 1, Predictor%n_Layers 

      !  Ignore changes in N2O less than ~0.3%
         IF ( ABS( Predictor%N2O_Multiplier( L ) ) < 1.0E-2 ) &
	     Predictor_AD%N2O_Multiplier( L ) = ZERO
         
         Predictor_AD%N2O_Amount(L) = Predictor_AD%N2O_Amount(L) + &
	 	  4.0 * Predictor_AD%N2O_Multiplier( L ) / TC%Ref_Profile_Data(12,L)
         
	 A_F_AD = A_F_AD - 4.0 * Predictor_AD%N2O_Multiplier( L )  
 
      !   Ignore changes in HNO3 less than ~1%
         IF ( ABS( Predictor%HNO3_Multiplier( L ) ) < 1.0E-2 ) &
	     Predictor_AD%HNO3_Multiplier( L ) = ZERO

         Predictor_AD%HNO3_Amount(L) = Predictor_AD%HNO3_Amount(L) + &
	         Predictor_AD%HNO3_Multiplier( L ) / TC%Ref_Profile_Data(11,L)
		 
	 A_F_AD = A_F_AD - Predictor_AD%HNO3_Multiplier( L )  
		 
      !   Ignore changes in SO2 of less than ~10%
         IF ( ABS( Predictor%SO2_Multiplier( L ) ) < 1.0E-4 ) &
	     Predictor_AD%SO2_Multiplier( L ) = ZERO
		 
         Predictor_AD%SO2_Amount(L) = Predictor_AD%SO2_Amount(L) + &
	     1.0010E-3*Predictor_AD%SO2_Multiplier( L ) / TC%Ref_Profile_Data(10,L)
		 
	 A_F_AD = A_F_AD - 1.0010E-3* Predictor_AD%SO2_Multiplier( L )  

      !  Ignore changes in CO2 of less than ~0.03%
         IF ( ABS( Predictor%CO2_Multiplier( L ) ) < 1.0E-2  ) & 
	     Predictor_AD%CO2_Multiplier( L ) = ZERO

         Predictor_AD%CO2_Amount(L) = Predictor_AD%CO2_Amount(L) + &
	     33.3333_fp*Predictor_AD%CO2_Multiplier( L ) / TC%Ref_Profile_Data(5,L)
		 
	 A_F_AD = A_F_AD - 33.3333_fp * Predictor_AD%CO2_Multiplier( L )  
         
	 
         A_F_AD = A_F_AD + Predictor_AD%Fix_Amount_Multiplier( L )

         PWATER_AD = - PMULT * A_F_AD / TC%Ref_Profile_Data(3,L) / &
	                ( TC%Fix_Gases_Adjustment(L)*GSCAL )
         
	 Predictor_AD%Absorber(L,H2O_Index) = Predictor_AD%Absorber(L,H2O_Index) + &
	        KMOLE* PWATER_AD * Predictor%Temperature(L)/ &
	             (STDDEN*STDTMP*100.0*TC%Ref_Profile_Data(2,L))
         Predictor_AD%Temperature(L) = Predictor_AD%Temperature(L) + &
	        KMOLE*Predictor%Absorber(L,H2O_Index)* PWATER_AD / &
	             (STDDEN*STDTMP*100.0*TC%Ref_Profile_Data(2,L))
 	 A_F_AD = ZERO
	 
    END DO Adjoint_Layer_Loop

    NULLIFY(TC)
  END SUBROUTINE Fix_And_Trace_Gas_Multi_AD


  SUBROUTINE Compute_Predictors( Sensor_Index,  &  ! Input 	    		
                                 Atmosphere,    &  ! Input
                                 GeometryInfo,  &  ! Input 	     
                                 Predictor,     &  ! Output
                                 APV            )  ! Internal variable output     
                                 	     


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    TYPE( CRTM_Atmosphere_type ),    INTENT( IN )     :: Atmosphere
    TYPE( CRTM_GeometryInfo_type ),  INTENT( IN )     :: GeometryInfo
    INTEGER,                         INTENT( IN )     :: Sensor_Index

    ! -- Outputs
    TYPE( Predictor_type ), INTENT( IN OUT ) :: Predictor    
    TYPE(APVariables_type), INTENT(OUT)      :: APV

    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Predictors'


    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER, DIMENSION( MAX_N_SUBSETS ) :: Set_Index 
    INTEGER, DIMENSION( MAX_N_SUBSETS_SUN ) :: Set_Solar_Index 
    INTEGER :: Calc_Sun_Angle_Secant 
    INTEGER :: I, L 
 
    !------------------------------------------------------------------
    ! Interpolate user profiles on ODCAPS grids and get ODCAPS parameters
    !------------------------------------------------------------------
    Predictor%User_Level_Pressure = Atmosphere%Level_Pressure                 
    
    CALL ConvertToODCAPSProfile(Atmosphere, GeometryInfo, Sensor_Index, Predictor)

    !#--------------------------------------------------------------------------#
    !#      -- CALCULATE THE PREDICTORS FOR THE UPWELLING TRANSMITTANCE --      #
    !#--------------------------------------------------------------------------#


    ! -----------------------------------------------------
    ! Calculate the predictors for different subsets
    ! -----------------------------------------------------

    ! Subset predictors for IR
    DO I = 1, MAX_N_SUBSETS
      Set_Index( I ) = I
      CALL Compute_Predictors_Subset( Sensor_Index,       &  ! Input
                                      Set_Index( I ),     &  ! Input
                                      Predictor )       ! In/Output

    END DO
     
    ! Subset predictors for Solar
    IF ( Predictor%Calc_Sun_Angle_Secant ) THEN
      Calc_Sun_Angle_Secant = 1 
    
      DO I = 1, MAX_N_SUBSETS_SUN 
    
    ! Subset index from 4 to 7   
        Set_Solar_Index( I ) = I + MAX_N_SUBSETS -  MAX_N_SUBSETS_SUN    
        CALL Compute_Predictors_Subset( Sensor_Index,          &  ! Input
                                        Set_Solar_Index( I ),  & ! Input	  
                                        Predictor,         & ! In/Output	  
       			                Calc_Sun_Angle_Secant)   ! Optional Input 	  
      END DO
    
    ! Non_LTE effect predictors
      CALL Compute_Non_LTE_Predictors( GeometryInfo, Predictor )

    ! ----------------------------------------------------------------
    ! Calculate the predictors for trace gases for CO2, SO2, HNO3, N2O
    ! ----------------------------------------------------------------
      CALL Compute_TraceGas_Predictors( Sensor_Index,Predictor,Calc_Sun_Angle_Secant )
      
    ENDIF
 
    ! ----------------------------------------------------------------
    ! Calculate the predictors for trace gases for CO2, SO2, HNO3, N2O
    ! ----------------------------------------------------------------
    CALL Compute_TraceGas_Predictors( Sensor_Index, Predictor )
 
    ! -----------------------------------------------
    ! Calculate the predictors for OPTRAN water vapor  
    ! -----------------------------------------------
    CALL Compute_WOPTRAN_Predictors( Sensor_Index, Predictor )

  
  END SUBROUTINE Compute_Predictors
 
  SUBROUTINE Compute_Predictors_TL( Sensor_Index,        &  ! Input	      
                                    Atmosphere,	         &  ! Input	    		  
                                    Predictor,           &  ! Input	      
                                    Atmosphere_TL,       &  ! Input	      
                                    GeometryInfo,        &  ! Input	      
                                    Predictor_TL,        &  ! In/Output       
                                    APV                  )  ! Internal variable input     

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    TYPE( CRTM_Atmosphere_type ),    INTENT( IN )     :: Atmosphere
    TYPE( Predictor_type ),          INTENT( IN )     :: Predictor
    TYPE( CRTM_GeometryInfo_type ),  INTENT( IN )     :: GeometryInfo
    INTEGER,                         INTENT( IN )     :: Sensor_Index
    TYPE( CRTM_Atmosphere_type ),    INTENT( IN )     :: Atmosphere_TL 
    TYPE(APVariables_type),          INTENT( IN )     :: APV

    ! -- Outputs 
    TYPE( Predictor_type ), INTENT( IN OUT ) :: Predictor_TL

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Predictors_TL'

    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER, DIMENSION( MAX_N_SUBSETS ) :: Set_Index 
    INTEGER, DIMENSION( MAX_N_SUBSETS_SUN ) :: Set_Solar_Index 
    INTEGER :: Calc_Sun_Angle_Secant 
    INTEGER :: I 

    Predictor_TL%User_Level_Pressure = Atmosphere_TL%Level_Pressure                 
 
    !------------------------------------------------------------------
    ! Interpolate user profiles on ODCAPS grids and get ODCAPS parameters
    !------------------------------------------------------------------

    CALL ConvertToODCAPSProfile_TL(Atmosphere, Predictor,  Atmosphere_TL, & 
                                            GeometryInfo, Sensor_Index, Predictor_TL)

    !#--------------------------------------------------------------------------#
    !#      -- CALCULATE THE PREDICTORS FOR THE UPWELLING TRANSMITTANCE --      #
    !#--------------------------------------------------------------------------#


    ! -----------------------------------------------------
    ! Calculate the predictors for different subsets
    ! -----------------------------------------------------

    ! Subset predictors for IR
    DO I = 1, MAX_N_SUBSETS
      Set_Index( I ) = I
      CALL Compute_Predictors_Subset_TL( Sensor_Index,       &  ! Input
                                         Set_Index( I ),     & ! Input
                                         Predictor,      & ! Input
				         Predictor_TL )    ! In/Output
 
    END DO
    
    ! Subset predictors for Solar
    IF ( Predictor%Calc_Sun_Angle_Secant ) THEN
      Calc_Sun_Angle_Secant = 1 
    
      DO I = 1, MAX_N_SUBSETS_SUN 
    
    ! Subset index from 4 to 7   
        Set_Solar_Index( I ) = I + MAX_N_SUBSETS -  MAX_N_SUBSETS_SUN    
        CALL Compute_Predictors_Subset_TL( Sensor_Index,              &  ! Input
                                           Set_Solar_Index( I ),  & ! Input	  
                                           Predictor,         & ! Input	  
					   Predictor_TL,      & ! In/Output
       			                   Calc_Sun_Angle_Secant)   ! Optional Input 	  
      END DO
    
    ! Non_LTE effect predictors
      CALL Compute_Non_LTE_Predictors_TL( GeometryInfo, Predictor, Predictor_TL)

    ! ----------------------------------------------------------------
    ! Calculate the predictors for trace gases for CO2, SO2, HNO3, N2O
    ! ----------------------------------------------------------------
      CALL Compute_TraceGas_Predictors_TL( Sensor_Index,Predictor, Predictor_TL,Calc_Sun_Angle_Secant )
      
    ENDIF

    ! ----------------------------------------------------------------
    ! Calculate the predictors for trace gases for CO2, SO2, HNO3, N2O
    ! ----------------------------------------------------------------
    CALL Compute_TraceGas_Predictors_TL( Sensor_Index, Predictor, Predictor_TL )

    ! -----------------------------------------------
    ! Calculate the predictors for OPTRAN water vapor  
    ! -----------------------------------------------
    CALL Compute_WOPTRAN_Predictors_TL( Sensor_Index, Predictor, Predictor_TL)

  END SUBROUTINE Compute_Predictors_TL 

  SUBROUTINE Compute_Predictors_AD( Sensor_Index,        &  ! Input                          
                                    Atmosphere,          &  ! Input          
                                    Predictor,           &  ! Input                  
                                    Predictor_AD,        &  ! Input                    
                                    GeometryInfo,        &  ! Input                    
                                    Atmosphere_AD,       &  ! Output         
                                    APV                  )  ! Internal variable input     
    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    TYPE( CRTM_Atmosphere_type ),    INTENT( IN )     :: Atmosphere
    TYPE( Predictor_type ),          INTENT( IN )     :: Predictor
    TYPE( Predictor_type ),          INTENT( IN OUT ) :: Predictor_AD
    TYPE( CRTM_GeometryInfo_type ),  INTENT( IN )     :: GeometryInfo
    INTEGER,                         INTENT( IN )     :: Sensor_Index
    TYPE(APVariables_type)        ,  INTENT( IN )     :: APV
 
    ! -- Outputs
    TYPE( CRTM_Atmosphere_type ), INTENT( IN OUT )    :: Atmosphere_AD  

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Predictors_AD'


    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER, DIMENSION( MAX_N_SUBSETS ) :: Set_Index 
    INTEGER, DIMENSION( MAX_N_SUBSETS_SUN ) :: Set_Solar_Index 
    INTEGER :: Calc_Sun_Angle_Secant 
    INTEGER :: I 

 
    ! -----------------------------------------------
    ! Calculate the predictors for OPTRAN water vapor  
    ! -----------------------------------------------
    CALL Compute_WOPTRAN_Predictors_AD(  Sensor_Index, Predictor, Predictor_AD)


    ! ----------------------------------------------------------------
    ! Calculate the predictors for trace gases for CO2, SO2, HNO3, N2O
    ! ----------------------------------------------------------------
    CALL Compute_TraceGas_Predictors_AD(Sensor_Index , Predictor, Predictor_AD )
    

    ! Subset predictors for Solar
    IF ( Predictor%Calc_Sun_Angle_Secant ) THEN
      Calc_Sun_Angle_Secant = 1 
    
    ! ----------------------------------------------------------------
    ! Calculate the predictors for trace gases for CO2, SO2, HNO3, N2O
    ! ----------------------------------------------------------------
      CALL Compute_TraceGas_Predictors_AD(Sensor_Index, Predictor,Predictor_AD,Calc_Sun_Angle_Secant )

    ! Non_LTE effect predictors
      CALL Compute_Non_LTE_Predictors_AD( GeometryInfo, Predictor, Predictor_AD)

      DO I = 1, MAX_N_SUBSETS_SUN 
    
    ! Subset index from 4 to 7   
        Set_Solar_Index( I ) = I + MAX_N_SUBSETS -  MAX_N_SUBSETS_SUN    
        CALL Compute_Predictors_Subset_AD( Sensor_Index,          &  ! Input
                                           Set_Solar_Index( I ),  & ! Input	  
                                           Predictor,         & ! Input	  
					   Predictor_AD,      & ! In/Output
       			                   Calc_Sun_Angle_Secant)   ! Optional Input 	  
      END DO
   
    ENDIF

    ! Subset predictors for IR
    DO I = 1, MAX_N_SUBSETS
      Set_Index( I ) = I
      CALL Compute_Predictors_Subset_AD( Sensor_Index,       &  ! Input
                                         Set_Index( I ),     & ! Input
                                         Predictor,      & ! Input
				         Predictor_AD )    ! In/Output
 
    END DO
    
    !------------------------------------------------------------------
    ! Interpolate user profiles on ODCAPS grids and get ODCAPS parameters
    !------------------------------------------------------------------

    CALL ConvertToODCAPSProfile_AD( Atmosphere, Predictor, Predictor_AD, &
                                             GeometryInfo, Sensor_Index, Atmosphere_AD)
    CALL Zero_Predictor (Predictor_AD)  
     
  END SUBROUTINE Compute_Predictors_AD              



END MODULE ODCAPS_Predictor
