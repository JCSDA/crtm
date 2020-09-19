

!------------------------------------------------------------------------------
!
!   PROGRAM: Compute_Coeff
!
!   The program 'Compute_Coeff' selects atmospheric predictors used for
!   transmittance estimation and generates its transmittance coefficients.
!
! CREATION HISTORY:
!       Written by:     Yong Han, JCSDA NOAA/NESDIS, July 10, 2008
!                       Yong Chen, CIRA/CSU/JCSDA/STAR
!
!------------------------------------------------------------------------------
PROGRAM Compute_Coeff
  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  ! -----------------
  USE Type_Kinds         , ONLY : fp_kind, & 
                                  Single                           
  USE Message_Handler    , ONLY : SUCCESS, FAILURE, Display_Message     
  USE File_Utility
  USE SpcCoeff_Define
  USE SpcCoeff_netCDF_IO
  USE PredictorSubsetIndex
  USE ODPS_Predictor_Define, ONLY : &
                             Predictor_type, &
                             Allocate_Predictor, &
                             Destroy_Predictor
  USE ODPS_ZAtmAbsorption, ONLY : &
                             MAX_N_PREDICTORS_ZSSMIS, &
                             Compute_Predictors_zssmis, &
                             PREDICTOR_MASK
  USE ODPS_Define,    ONLY : ODPS_type, Allocate_ODPS, Destroy_ODPS
  USE ODPS_Binary_IO, ONLY : Write_ODPS_Binary
  USE Regression,     ONLY : Compute_AutoCross_Matrix, Compute_RegressionCoeff
  USE TauProfile,     ONLY : Get_Profile, TauProfile_Type
  USE ODPS_netCDF_IO, ONLY : Write_ODPS_netCDF

  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  TYPE :: stat_type
    REAL( fp_kind ) :: mean
    REAL( fp_kind ) :: rms
    REAL( fp_kind ) :: maxv
  END TYPE stat_type
  
  ! ----------
  ! Parameters
  ! ----------
  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Compute_Coeff'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'
  CHARACTER( * ), PARAMETER :: ParameterList = 'Namelist.txt'

  ! Group 1 - all trace gases; Group 2 - IR broadband; Group 3 - MW
  ! Group_4 - ZSSMIS
  INTEGER, PARAMETER :: This_Group = 4

  REAL( fp_kind ), PARAMETER :: ZERO = 0.0_fp_kind
  REAL( fp_kind ), PARAMETER :: ONE  = 1.0_fp_kind
  REAL( fp_kind ), PARAMETER :: INFINITE  = HUGE( ONE )
  REAL( fp_kind ), PARAMETER :: TOLERANCE = EPSILON( ONE )

  REAL( fp_kind ), PARAMETER :: OD_MAX = 20.0_fp_kind

  ! number of absorber and component id
  INTEGER, PARAMETER :: Nabs = 0  ! no absorber for Zeeman channels      
  INTEGER, PARAMETER :: Component_ID = 13  ! Dry

  ! ---------
  ! Variables
  ! ---------
 
  CHARACTER( 256 ) :: Message, Results
  TYPE( Predictor_type )  :: Predictor
  TYPE( TauProfile_Type ) :: TauProf

  CHARACTER( 256 ) ::    inFilename_spcCoef, &      ! Planck coeff. file
                         inFilename_TauProfile, &   ! tau profiles
                         inFilename_AtmProfile      ! atmospheric profiles
                         
  TYPE(SpcCoeff_type)     :: SpcCoeff  
  
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status

  ! Information to describe the dimensions of the TauProfile data 
  INTEGER :: Nlay
  INTEGER :: NBe
  INTEGER :: NC
  INTEGER :: Nang 
  INTEGER :: Natm 
  INTEGER :: Nfrq
  INTEGER :: Ncom

  INTEGER,      DIMENSION( MAX_N_PREDICTORS_ZSSMIS ) :: predictor_index  ! n_preds
  INTEGER,         DIMENSION( : ),       ALLOCATABLE :: predSubIndex     ! n_preds
  REAL( fp_kind ), DIMENSION( :,: ),     ALLOCATABLE :: Coeff            ! n_preds x Nlay
  REAL( fp_kind ), DIMENSION( :,:,: ),   ALLOCATABLE :: Coeff_all        ! n_preds x Nlay x NFreq
  REAL( fp_kind ), DIMENSION( : ),       ALLOCATABLE :: TauPred(:)       ! 0:Layer 
  REAL( fp_kind ), DIMENSION( :,:,:,:,:,: ), ALLOCATABLE :: Predictors   ! Nlay x n_preds x Be x CosBK x Ang x Prof
  REAL( fp_kind ), DIMENSION( :,:,:,:,: ),   ALLOCATABLE :: layer_od_path   !Nlay x NBe x NCosBK x Nangle x Natm
  REAL( fp_kind ), DIMENSION( : ),       ALLOCATABLE :: Aux_data       

  REAL( fp_kind ), DIMENSION( :,: ),     ALLOCATABLE :: X             ! (Nangle x Natm) x n_preds  for each layer
  REAL( fp_kind ), DIMENSION( : ),       ALLOCATABLE :: Y             ! (Nangle x Natm) for each layer
  REAL( fp_kind ), DIMENSION( : ),       ALLOCATABLE :: weight        ! (Nangle x Natm) for each layer
  REAL( fp_kind ), DIMENSION( : ),       ALLOCATABLE :: CC            ! n_preds 
  REAL( fp_kind ), DIMENSION( :, : ),    ALLOCATABLE :: XX            ! n_preds x n_preds 
  REAL( fp_kind ), DIMENSION( : ),       ALLOCATABLE :: XY            ! n_preds 

  REAL( fp_kind ), DIMENSION( :,: ),     ALLOCATABLE :: X1             ! (Nangle x Natm) x n_preds  for each layer
  REAL( fp_kind ), DIMENSION( : ),       ALLOCATABLE :: Y1             ! (Nangle x Natm) for each layer
  REAL( fp_kind ), DIMENSION( : ),       ALLOCATABLE :: weight1        ! (Nangle x Natm) for each layer

  REAL( fp_kind ), DIMENSION( :,:,:,: ),     ALLOCATABLE :: tb_lbl    ! Be x CosBK x Nangle x Natm
  REAL( fp_kind ), DIMENSION( :,:,:,: ),     ALLOCATABLE :: tb_pred   ! Be x CosBK x Nangle x Natm

  TYPE(Stat_type) :: stat   

  REAL( fp_kind ) :: wgt, residual, delta_path, path, frequency

  INTEGER :: j ! n_Absorbers
  INTEGER :: k ! n_Layers, n_Levels
  INTEGER :: m ! n_Profiles
  INTEGER :: n
  INTEGER :: i ! n_Angles 
  INTEGER :: jp ! n_preds
  INTEGER :: idx

  INTEGER :: Nsample, Nsample_max ! total sample 
  INTEGER :: kp, ichan, sensor_channel, n_predSub, n_predictors, endOfselection
  INTEGER :: ib, ic, iang, ifreq
  
  ! Reference profiles
  REAL( fp_kind ), DIMENSION( : ),   ALLOCATABLE :: Ref_Level_Pressure
  REAL( fp_kind ), DIMENSION( : ),   ALLOCATABLE :: Ref_Temperature
  REAL( fp_kind ), DIMENSION( :,: ), ALLOCATABLE :: Ref_absorber
  
  REAL( fp_kind ), DIMENSION( : ),   ALLOCATABLE :: Secant_Ang   
   
  WRITE(*, *)'Enter TauProfile filename:'
  READ(*, '(a)')inFilename_TauProfile
  WRITE(*, *)'Enter AtmProfile filename:'
  READ(*, '(a)')inFilename_AtmProfile  
  WRITE(*, *)'Enter SpcCoeff file filename:'
  READ(*, '(a)')inFilename_spcCoef

  !--- read spectral coefficients                               

  Error_Status = Read_SpcCoeff_netCDF( inFilename_spcCoef, SpcCoeff )
  IF( Error_Status /= SUCCESS ) THEN                                   
    PRINT *, '### ERROR ###'                                    
    PRINT *, 'Fail to read a spectral coefficient file'         
    PRINT *, 'File name: ', InFileName_SpcCoef                  
    STOP                                                    
  END IF                            

  ! obtaining atmospheric profiels
   
  CALL Get_Profile(inFilename_AtmProfile, inFilename_TauProfile, TauProf)

  Nlay = TauProf%n_Layer 
  NBe  = TauProf%n_Be 
  NC   = TauProf%n_CosBK 
  Nang = TauProf%n_Angle 
  Natm = TauProf%n_Profile
  Nfrq = TauProf%n_freq
  NCom = 1 

  ichan = TauProf%Channel_Index  ! ZSSMIS channel index
  sensor_channel = SpcCoeff%Sensor_Channel(ichan)  ! SSMIS channel index

  ! ------------------------------------------------------------
  ! Allocate the Predictor structure and the Predictors array  
  ! ------------------------------------------------------------
  Allocate_Status = Allocate_Predictor( NLay,   &
                                        Ncom,   &
                                        MAX_N_PREDICTORS_ZSSMIS, &
                                        Predictor )
  IF ( Allocate_Status /= SUCCESS ) THEN
    WRITE( Message, '( "Error allocating Predictor structure STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          FAILURE )
    STOP
  END IF

  ! Obtain the predictor indexes from the predictor mask for this channel
  n_Predictors = 0
  DO i = 1, MAX_N_PREDICTORS_ZSSMIS
    IF( PREDICTOR_MASK(i, ichan) == 1 )THEN
      n_Predictors = n_Predictors + 1
      predictor_index(n_Predictors) = i 
    END IF
  END DO

  ALLOCATE( Predictors(Nlay, n_Predictors, NBe, NC, Nang, Natm), &
            Secant_Ang(Nlay), &
            Coeff(MAX_N_PREDICTORS_ZSSMIS, Nlay), &
            Coeff_all(MAX_N_PREDICTORS_ZSSMIS, Nlay, Nfrq), &
            TauPred(0:Nlay), &
            layer_od_path(Nlay, NBe, NC, Nang, Natm), &
            tb_pred(NBe, NC, Nang, Natm), &  
            tb_lbl(NBe, NC, Nang, Natm), &
            Ref_Level_Pressure(0:Nlay), &
            Ref_Temperature(Nlay), &
            Ref_absorber(Nlay, Nabs), & 
            Aux_data(TauProf%n_freq+1), & 
            STAT = Allocate_Status )
  
  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error allocating arrays. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          FAILURE )
    STOP
  END IF

  ! set reference profile
  Ref_Level_Pressure = TauProf%p_level
  Ref_absorber    = ZERO
  
  DO k = 1, Nlay
    Ref_Temperature(k) = SUM(TauProf%tk_layer(k, :))/Natm
  END DO
  
  !=======================================================================
  ! Compute the atmos predictors (all predictors in the pool for each absorber) 
  !=======================================================================
  DO m = 1, Natm

    DO iang = 1, Nang
       Secant_Ang(:) = TauProf%Secant_Ang(iang)
    DO ib = 1, NBe
    DO ic = 1, NC
      
      CALL Compute_Predictors_zssmis(TauProf%tk_layer(:, m), &
                             TauProf%Be(ib), &
                             TauProf%CosBK(ic), &
                             ZERO, &           ! Doppler shift - not used in the training process
                             Secant_Ang, &
                             Predictor)       

      DO i = 1, n_predictors
        Predictors(:, i, ib, ic, iang, m) = Predictor%X(:, predictor_index(i),1)    
      END DO             
    END DO
    END DO
    END DO
                           
  END DO

Freq_loop: DO ifreq = 1, Nfrq
  
    !=======================================================================                  
    ! Compute layer optical path                                                              
    !=======================================================================                  
    DO iang = 1, Nang                                                  
    DO ib = 1,  NBe                                                  
    DO ic = 1,  NC                                                  
    DO m= 1, Natm                                               
      DO k = 1, Nlay                                                                                            
         layer_od_path(k, ib, ic, iang, m) = -LOG(TauProf%Tau(k, ib, ic, iang, m, ifreq) / &  
                                              TauProf%Tau(k-1, ib, ic, iang, m, ifreq) ) /&   
                                              TauProf%Secant_Ang(iang)                       
      END DO   
    END DO                                                                                  
    END DO                                                                                    
    END DO                                                                                    
    END DO                                                                                    

  
    ALLOCATE( predSubIndex(n_predictors), &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '( "Error allocating predSubIndex. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF

    ! set these variables to default values  
    predSubIndex = 0                         
    n_predSub = 0                            
    Coeff(:,:) = ZERO                             
      
    !----------------------------------------------------                     
    ! compute tau coefficients and                                            
    ! stepwise search a best set of predictors                                
    !----------------------------------------------------                     
    CALL reset_search_predSubset()                                            
   
    PredictorSet_search_loop: DO                                              

      endOfselection = Get_subsetIndex_rev_stepwise(predSubIndex, n_predSub) 

      if(endOfselection == 1) then                                            
                                                                              
        call retrieve_bestset_rev_stepwise(predSubIndex, n_predSub)           
                                                                              
        exit PredictorSet_search_loop                                         
                                                                              
      endif                                                                   

      !--------------------------------------------------------------         
      ! In the routine: do regression looping over each layer                 
      !--------------------------------------------------------------         
      CALL DoRegression()                                                     

      ! evaluate fitting errors and save the current set of indexes if the    
      ! current fitting yields a smaller error.                               
      CALL evaluate_fittingError(stat%rms, ZERO, predSubIndex)                    
   
    END DO PredictorSet_search_loop                                           


    ! get statistics for the best predictor set                               

    CALL DoRegression()                                                       

    frequency = SpcCoeff%Frequency(Ichan)
  
    ! write the statistics out  
    WRITE(Results, '(i4, f7.2, f12.4, 3E13.5, 50I3)')&
                         Sensor_Channel, TauProf%Freq(ifreq), frequency, & 
                         stat%mean, stat%rms, stat%maxv, &
                         n_predSub, predictor_index(predSubIndex(1:n_predSub))
    WRITE(*,  '(A)')TRIM(Results)
    WRITE(40, '(A)')TRIM(Results)

    Coeff_all(:,:,ifreq) = ZERO
    DO i = 1, n_predSub                      
      j = predictor_index( predSubIndex(i) )                       
      DO k = 1, Nlay                       
        Coeff_all(j,k,ifreq)  = Coeff(i, k)                                                                       
      END DO                                    
    END DO                                                                                          

    DEALLOCATE( predSubIndex, STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN                                                         
      WRITE( Message, '( "Error deallocateiong array predSubIndex. STAT = ", i5 )' ) &    
                      Allocate_Status                                                        
      CALL Display_Message( PROGRAM_NAME, &                                                  
                            TRIM( Message ), &                                               
                            FAILURE )                                                        
      STOP                                                                                   
    END IF                                                                                   
    
    
  END DO Freq_loop

  !---------------------------------                                                   
  ! Write the results to a file                                                        
  !---------------------------------
  Aux_data(1) = REAL(nfrq, fp_kind)
  Aux_data(2:nfrq+1) = TauProf%Freq                                                  
  Error_Status = Write_Out(This_Group,                &                                
                           Component_ID,              &                                        
                           Sensor_Channel,            &                                        
                           Coeff_all,                 & 
                           Aux_data,                  &                                       
                           Ref_Level_Pressure,        &                                        
                           Ref_Temperature)                                       

  IF ( Error_Status /= 0 ) THEN                                                                
    WRITE( Message, '( "Error writing our tau coefficient data to a file", i0)' ) &            
                    Error_Status                                                               
    CALL Display_Message( PROGRAM_NAME, &                                                      
                          TRIM( Message ), &                                                   
                          FAILURE )                                                            
    STOP                                                                                       
  END IF                                                                                       

  ! -----------------------------------------
  ! Deallocate the Predictors array
  ! -----------------------------------------

  DEALLOCATE( Predictors, &
              Coeff, &
              Coeff_all, &
              TauPred, &
              layer_od_path, &
              tb_pred, &  
              tb_lbl, &
              Ref_Level_Pressure, &
              Ref_Temperature, &
              Ref_absorber, &  
              STAT = Allocate_Status )
      
  IF ( Allocate_Status /= 0 ) THEN                                                           
    WRITE( Message, '( "Error deallocating Tau and other arrays. STAT = ", i5 )' ) &      
                    Allocate_Status                                                          
    CALL Display_Message( PROGRAM_NAME, &                                                    
                          TRIM( Message ), &                                                 
                          FAILURE )                                                          
    STOP                                                                                     
  END IF                                                                                     


  ! Deallocate the Predictor structure
  Allocate_Status = Destroy_Predictor(Predictor)
  IF ( Allocate_Status /= SUCCESS ) THEN
    WRITE( Message, '( "Error deallocating Predictor structure. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          FAILURE )
  END IF

  ! deallocate the SpcCoeff data structure
  Error_Status = Destroy_SpcCoeff( SpcCoeff )  
  
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error deallocating SpcCoeff structure.', & 
                           Error_Status )
    STOP
  END IF
  
  !-----------------------------------------------              
  !  output a signal file indicating completion                 
  !-----------------------------------------------              
                                                                
  OPEN(UNIT=11, FILE='Completion_signal.txt', STATUS='REPLACE')
                                                                
  WRITE(11, *) 'Normal End'

  CLOSE(11)

  write(*, *) '*** NORMAL END ***'

CONTAINS

  !---------------------------------------------------------------
  !  Compute regression coefficients for all layers
  !
  !     All variables are from the calling routine
  !---------------------------------------------------------------
   
  SUBROUTINE DoRegression()                                                         

    Nsample_max = NBe*NC*Nang*Natm

    ALLOCATE(  X(Nsample_max, n_predSub), &  				      
    	       Y(Nsample_max), &						      
    	       weight(Nsample_max), &					      
    	       CC(n_predSub), & 
    	       STAT = Allocate_Status ) 							      
  
    IF ( Allocate_Status /= 0 ) THEN								      
      WRITE( Message, '( "Error allocating X, Y, and CC array. STAT = ", i5 )') &		      
    		      Allocate_Status								      
      CALL Display_Message( PROGRAM_NAME, &							      
    			    TRIM( Message ), &  						      
    			    FAILURE )								      
      STOP											      
    END IF											      

    Layer_loop:  DO k = 1, NLay      ! Loop over layers
					      
      X(:,:)	= ZERO  									      
      Y(:)	= ZERO  									      
      weight(:) = ZERO  									      
      CC(:)	= ZERO

!!      Nsample = COUNT( ABS(layer_od_path(k, :, :, :, :)) > ZERO )
      Nsample = COUNT( layer_od_path(k, :, :, :, :) > 1.0e-6 )

      IF( Nsample <  Nsample_max/3 ) CYCLE				      
!      IF( Nsample <  20 ) CYCLE				      

      Nsample = 0    
      DO ib = 1, NBe    
      DO ic = 1, NC   
      DO m= 1, Natm    
      DO iang = 1, Nang						      
        wgt = SQRT(TauProf%Tau(k, ib, ic, iang, m, ifreq) * TauProf%Tau(k, ib, ic, iang, m, ifreq))                                 

!        IF(wgt <= TOLERANCE .OR. layer_od_path(k, ib, ic, iang, m)  == INFINITE)CYCLE    
 	
    	Nsample = Nsample + 1                                                                         
    	                                                                                              
    	weight(Nsample) = wgt                                                            
	
        Y(Nsample) = layer_od_path(k, ib, ic, iang, m)
        IF(sensor_channel == 20)THEN                                                                                
    	  Y(Nsample) = LOG(Y(Nsample))
        END IF
        
        DO jp = 1, n_predSub                                                                      
          X(Nsample, jp ) = Predictors(k, predSubIndex(jp), ib, ic, iang, m)
        END DO
										      
      END DO  							      
      END DO  
      END DO  							      
      END DO  

!      IF( Nsample <  20 ) CYCLE

      ALLOCATE(  X1(Nsample, n_predSub), &							      
    		 Y1(Nsample), &
                 XX(n_predSub,n_predSub), &
                 XY(n_predSub), &   								      
    		 weight1(Nsample), &								      
    		 STAT = Allocate_Status )							      
  
      IF ( Allocate_Status /= 0 ) THEN  							      
    	WRITE( Message, '( "Error allocating X1, Y1, and weight1 array. STAT = ", i5 )' ) &	      
    			Allocate_Status 							      
    	CALL Display_Message( PROGRAM_NAME, &							      
    			      TRIM( Message ), &						      
    			      FAILURE ) 							      
    	STOP											      
      END IF											      

!      weight1(1: Nsample) = ONE
      weight1(1: Nsample) = weight(1:Nsample)
   												      
      DO i=1, Nsample										      
       X1(i, :) = X(i, : ) !* weight1(i) 							      
       Y1(i)	= Y(i)  !* weight1(i)								      
      END DO

      CALL Compute_AutoCross_Matrix(X1, Y1, XX, XY)
      ! Add gamma (noise) at the top to stabilize
!      IF(k <= 3)THEN
!        DO jp = 1, n_predSub
!          XX(jp,jp) = XX(jp, jp)*(ONE + 1.e-6_fp_kind/REAL(k, fp_kind))
!        END DO
!      END IF

      Error_Status = Compute_RegressionCoeff(XX, XY, CC)
      IF(Error_Status /= 0)THEN
    	CALL Display_Message( PROGRAM_NAME, &							      
    			      "Error solve the linear equation", &						      
    			      FAILURE ) 							      
    	STOP											      
      END IF											      

      DO jp=1, n_predSub 									      
        Coeff(jp,k) = CC(jp)									      
      END DO											      

      DEALLOCATE(X1, Y1, XX, XY, weight1, &								      
    		 STAT = Allocate_Status )							      
  
      IF ( Allocate_Status /= 0 ) THEN  							      
    	WRITE( Message, '( "Error deallocating X1, Y1, and weight1 array. STAT = ", i5 )' ) &	      
    			Allocate_Status 							      
    	CALL Display_Message( PROGRAM_NAME, &							      
    			      TRIM( Message ), &						      
    			      FAILURE ) 							      
    	STOP											      
      END IF											      
 
    END DO Layer_loop

    !----------------------------------------------                                                   
    ! Compute predicted transmittances and Tb                                                         
    !----------------------------------------------                                                   
    DO iang = 1, Nang   ! Loop over scan angles                                           
    DO m= 1, Natm       ! Loop over profiles                                                          
    DO ib = 1, NBe      ! Loop over Be fields                                                          
    DO ic = 1, NC       ! Loop over CosBK fields                                                         
       TauPred(0) = ONE                                                  
       path = ZERO                                                                         
       DO k = 1, Nlay     ! Loop over layers 
         delta_path = ZERO                 
         DO jp = 1, n_predSub                                     
           delta_path = delta_path + Coeff(jp,k) * Predictors(k, predSubIndex(jp), ib, ic, iang, m)          
         END DO
         IF(sensor_channel == 20)THEN
           delta_path = exp(delta_path)
         END IF
         path = path +  MIN(MAX(delta_path, ZERO), OD_MAX)                                
         TauPred(k) = exp(-path*TauProf%Secant_Ang(iang))   
       END DO                                                                             
       CALL Compute_BrightTemp(TauPred, TauProf%tk_layer(:,m), tb_pred(ib, ic, iang, m))                
       ! LBL BT                                                                           
       CALL Compute_BrightTemp(TauProf%Tau(:,ib, ic, iang, m, ifreq), TauProf%tk_layer(:,m), tb_lbl(ib, ic, iang, m))  

    END DO
    END DO                                                                                          
    END DO                                                                                            
    END DO                                                                                            

    ! compute statistics
    n = COUNT(tb_lbl > ZERO)   ! 
    stat%mean = SUM(tb_pred - tb_lbl)/n
    stat%rms = SQRT(SUM((tb_pred - tb_lbl)**2)/n)
    stat%maxv = MAXVAL(ABS(tb_pred - tb_lbl))

    DEALLOCATE(  X, Y, weight, CC, &                                                                 
                STAT = Allocate_Status )                                                            
  
    IF ( Allocate_Status /= 0 ) THEN                                                                 
      WRITE( Message, '( "Error deallocating X, Y, and CC array. STAT = ", i5 )' ) &                
                      Allocate_Status                                                               
      CALL Display_Message( PROGRAM_NAME, &                                                         
                            TRIM( Message ), &                                                      
                            FAILURE )                                                               
      STOP                                                                                          
    END IF                                                                                           


  END SUBROUTINE DoRegression

  SUBROUTINE Compute_BrightTemp(Tau, t, tb)
    real(fp_kind), intent(in)  :: tau(0:)  
    real(fp_kind), intent(in)  :: t(:)    ! layer temperature      
    real(fp_kind), intent(out) :: tb
    
    integer :: k
    
    tb = ZERO
    DO k = 1, SIZE(t)
      tb = tb + (tau(k-1)-tau(k))*t(k)
    END DO
    
  END SUBROUTINE Compute_BrightTemp   
  

  FUNCTION Write_Out(This_Group,   &
                     Component_ID, &
                     Channel,      &
                     Coeff,        & 
                     Aux_data,     &
                     Ref_Level_Pressure, &
                     Ref_Temperature) &
                     RESULT(Error_Status)
    INTEGER,       INTENT( IN ) :: This_Group
    INTEGER,       INTENT( IN ) :: Component_ID
    INTEGER,       INTENT( IN ) :: Channel
    REAL(fp_kind), INTENT( IN)  :: Coeff(:,:,:)           ! J x K x Freq
    REAL(fp_kind), INTENT( IN)  :: Aux_data(:)
    REAL(fp_kind), INTENT( IN)  :: Ref_Level_Pressure(0:) ! K      
    REAL(fp_kind), INTENT( IN)  :: Ref_Temperature(:)     ! K      

    INTEGER :: Error_Status

    !Local
    CHARACTER(*), PARAMETER :: SUBROUTINE_NAME = 'Write_Out'
    TYPE( ODPS_type ) :: TC
    INTEGER           :: n_Layers
    INTEGER           :: n_Coeffs
    INTEGER           :: n_freqs
    INTEGER           :: n_pred
    INTEGER           :: n_Aux_data
    INTEGER           :: i, j, m, ifrq, max_n_pred
    CHARACTER(*), PARAMETER  :: file_out = "TauCoeff.nc"
    INTEGER, PARAMETER :: n_absorbers = 1
    INTEGER, PARAMETER :: H2O_ID = 1  ! HITRAN absorber ID for H2O
    CHARACTER( 1024 ) :: Title, History, Comment, Profile_Set_Id

    n_pred  = SIZE(Coeff, DIM=1)
    n_Layers = SIZE(Coeff,DIM=2)
    n_freqs = SIZE(Coeff, DIM=3)
    n_Aux_data = SIZE(Aux_data)
        
    ! find the size of the predictors
    max_n_pred = 0
    DO ifrq = 1, n_freqs
      DO j = n_pred, 1, -1
        IF(ANY(ABS(Coeff(j,:,ifrq)) > ZERO))THEN
          IF(j > max_n_pred)THEN
            max_n_pred = j
            EXIT
          END IF
        END IF
      END DO
    END DO 
    
    n_Coeffs = max_n_pred * n_Layers * n_freqs + n_Aux_data

    Error_Status = Allocate_ODPS( n_Layers,                   &                             
                                  1,                          &  ! 1 component              
                                  n_absorbers,                &  ! number of molecules      
                                  1,                          &  ! 1 Channel                
                                  n_Coeffs,                   &                             
                                  TC)                                                       

    IF( Error_Status /= SUCCESS)THEN                                                         
      WRITE( Message, '( "Error allocating the TC structure. Error_Status = ", i5 )' ) &                
                      Error_Status                                                                 
      CALL Display_Message( SUBROUTINE_NAME, &                                                           
                            TRIM( Message ), &                                                        
                            FAILURE )                                                                 
      STOP                                                                                            
    END IF                                                                                            

    TC%n_Predictors(1,1)  = max_n_pred
    TC%Pos_Index(1,1)     = 1
    TC%C(1:n_Aux_data) = REAL(Aux_data, Single)
    m = n_Aux_data
    DO ifrq = 1, n_freqs
      DO i = 1, max_n_pred
        DO k = 1, n_Layers
          TC%C(m+k)  = REAL(Coeff(i, k, ifrq), Single)                                                                     
        END DO
        m = m + n_Layers 
      END DO 
    END DO                                                                                       

    ! Scalar IDs
    TC%Group_Index      = This_Group     
    TC%Sensor_id        = SpcCoeff%Sensor_id
    TC%WMO_Satellite_ID = SpcCoeff%WMO_Satellite_ID
    TC%WMO_Sensor_ID    = SpcCoeff%WMO_Sensor_ID                              
    TC%Sensor_Type      = SpcCoeff%Sensor_Type

    TC%Ref_Level_Pressure = Ref_Level_Pressure                                                       
    TC%Ref_Pressure       = (Ref_Level_Pressure(1:n_Layers) - Ref_Level_Pressure(0:n_Layers-1))/&    
                             LOG(Ref_Level_Pressure(1:n_Layers) / Ref_Level_Pressure(0:n_Layers-1))
    TC%Ref_Absorber       = 1.0e-12_fp_kind  
    TC%Min_Absorber       = 1.0e-12_fp_kind  
    TC%Max_Absorber       = 1.0e-12_fp_kind  
    TC%Ref_Temperature    = Ref_Temperature                                                          
    TC%Sensor_Channel     = Channel                                                                  
    TC%Component_ID       = Component_ID
    TC%Absorber_ID        = H2O_ID
                                                             
    Title = 'TauCoeff data for absorption calculations with inclusion of Zeeman-effect.'
    History = ' '
    Comment = 'Include 9 sets of Tau coefficients, conditioned at 9 frequencies of Doppler shift, respectively'
    Profile_Set_Id = '48LVL UMBC profile set, extened to 0.000071 mb (about 110 km) using 6 standard model profiles '
!    Error_Status = Write_ODPS_Binary(file_out, TC)
    Error_Status = Write_ODPS_netCDF(file_out,      &
                                     TC,            &
                                     TRIM(Title),   &
                                     TRIM(History), &
                                     TRIM(Comment), &
                                     TRIM(Profile_Set_Id))
    IF(Error_Status /= SUCCESS)THEN                                                         
      CALL Display_Message( SUBROUTINE_NAME, &                                                           
                            "Error writing out the TC structure. Error_Status = ", &                                                        
                            FAILURE )                                                                 
      STOP                                                                                            
    END IF 

    Error_Status = Destroy_ODPS(TC)
    IF(Error_Status /= SUCCESS)THEN
      CALL Display_Message( SUBROUTINE_NAME,     &                                                           
                            "Error: destroy TC", &                                                        
                            FAILURE )                                                                 
      STOP                                                                                            
    END IF 

  END FUNCTION Write_Out
    
!================================================================
!  A wrapper for calling LAPACK DGELS, which calculates real linear
!  system.
!    
!    Yong Han, March 2003
!================================================================
  subroutine DGELS_LAPACK(Nsample, Nel, x, y, coef, weight, residual)
		    
    integer, intent(in) :: Nsample   ! number of samples
    integer, intent(in) :: Nel       ! number of predictors
    real(fp_kind), intent(in) :: x(Nsample,Nel)  ! Predictors matrix
    real(fp_kind), intent(in) :: y(Nsample)      ! Predictand vector
    real(fp_kind), intent(in) :: weight(Nsample)      ! Predictand weight
    real(fp_kind), intent(inout) :: coef(Nel)    ! regression coefficients
    real(fp_kind), intent(inout) :: residual  ! residual of LSM
        
    
    !--- local variables
    real(fp_kind)  :: xwork(Nsample, Nel)
    real(fp_kind)  :: ywork(Nsample, 1)
    real(fp_kind)  :: work(2*Nsample)
    real(fp_kind)  :: diff
    integer        :: i, Info
    
    if (Nsample < 1 .or. Nel < 1) then
      print *, 'DGELS_LAPACK either Nsample or Nel has a value less than 1'
      stop 99
    end if
    
    !--- copy data
    xwork(:, :) = x(:, :)
    ywork(:, 1) = y(:)
    
    call DGELS('N', Nsample, Nel, 1, xwork, Nsample, ywork, &
                Nsample, work, 2*Nsample, Info)

    if(Info /= 0)then
      print *, 'DGELS_LAPACK: error from DGELS'
      stop 99
    endif

    coef(:) = ywork(:, 1)

    residual = 0.
    do i = 1, Nsample
        diff =  (y(i) - SUM(coef(:) * x(i, :)) ) / weight(i)
        residual = residual + diff*diff
    end do
    residual = sqrt(residual / Nsample)
    
  end subroutine DGELS_LAPACK

END PROGRAM Compute_Coeff
