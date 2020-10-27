

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
  USE Message_Handler    , ONLY : SUCCESS, WARNING, FAILURE, Display_Message     
  USE File_Utility
  USE Parameters
  USE SensorInfo_Parameters
  USE AtmProfile_Define
  USE AtmProfile_netCDF_IO
  USE TauProfile_Define
  USE TauProfile_netCDF_IO
  USE SpcCoeff_Define
  USE SpcCoeff_netCDF_IO
  USE CalcStatTransTemp
  USE PlanckFunc
  USE PredictorSubsetIndex
  USE Interpolation
  USE ODPS_Predictor_Define, ONLY : &
                             Predictor_type,     &
                             Allocate_Predictor, &
                             Destroy_Predictor
  USE ODPS_Predictor, ONLY : Get_max_n_Predicotrs,  &  
                             Get_n_Components,      & 
                             Get_n_Absorbers,       & 
                             Get_Component_ID,      &  ! Component IDs
                             Get_Absorber_ID,       &  ! Absorber IDs
                             Compute_Predictor,     &
                             GROUP_1,GROUP_2,GROUP_3, &
                             TOT_ComID, CO2_ComID, & 
                             Get_Ozone_Component_ID
  USE ODPS_Define,    ONLY : ODPS_type, Allocate_ODPS, Destroy_ODPS
!  USE ODPS_Binary_IO, ONLY : Write_ODPS_Binary
  USE ODPS_netCDF_IO, ONLY : Write_ODPS_netCDF
  USE Regression,     ONLY : Compute_AutoCross_Matrix, Compute_RegressionCoeff
  
  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE
  ! ----------
  ! Parameters
  ! ----------
  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Compute_Coeff'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'
  CHARACTER( * ), PARAMETER :: ParameterList = 'Namelist.txt'

  ! Group 1 - all trace gases; Group 2 - IR broadband; Group 3 - MW
  INTEGER :: This_Group
  
  !----------------------------------------------------------------------------
  ! This parameter is used to check the significance of the tau component.
  ! If the BT difference between BT1 and BT2 are less the parameter, then
  ! this tau component will not be taken into account in the transmittance 
  ! calculations, where
  !   BT1 - brightness temperature computed from the total tau (all components)
  !   BT2 - brightness temperature computed from all components except the
  !         component to be fitted.
  !----------------------------------------------------------------------------
  REAL( fp_kind ), PARAMETER :: COMPONENT_SIGNIFICANCE = 0.001_fp_kind 

  REAL( fp_kind ), PARAMETER :: Re = 6370.949 ! Earth's radius (km)

  ! water vapor index for accessing absorber array
  INTEGER            ::   H2O_idx  

  ! ---------
  ! Variables
  ! ---------
 
  CHARACTER( 256 ) :: Message
  TYPE( AtmProfile_type ), ALLOCATABLE :: AtmProfile(:)
  TYPE( Predictor_type )  :: Predictor

  INTEGER, ALLOCATABLE    :: Absorber_id(:)        
  INTEGER, ALLOCATABLE    :: Absorber_index(:)     
  
  CHARACTER( 128 )        :: File_Prefix                ! name of satellite sensor
  INTEGER                 :: iChan_start,  &            ! starting ch seq #
                             iChan_end                  ! ending ch seq #
  INTEGER                 :: icom,         &            ! compoent index
                             Nangle_regression          ! # of incidence angles used in coefficient calculation
  CHARACTER( 256 )        :: inFilename_spcCoef, &      ! Planck coeff. file
                             inFilename_atmProfile,&    ! AtmosProfile file
                             inFilename_tauProfile,&    ! transmittance file
                             inFilename_tauCoeff        ! tau coeffs
                         
  TYPE(SpcCoeff_type)     :: SpcCoeff  
  INTEGER                 :: Nlay, Natm, Nabs, Ncom
  INTEGER                 :: NsensorChan
  
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status

  ! actual and max numbers of predictors set dynamically
  INTEGER :: n_predictors = 0
  INTEGER :: n_predictors_max = 0

  ! Component_ID
  INTEGER :: Component_ID
  
  ! Information to describe the dimensions of the TauProfile data 
  INTEGER :: n_Layers
  INTEGER :: n_Channels
  INTEGER :: n_Angles
  INTEGER :: n_Profiles
  INTEGER :: n_components 
 
  REAL( fp_kind ), DIMENSION( :,:,:,: ), ALLOCATABLE :: Predictors    ! Nlay x n_preds x Nangle x Natm
  INTEGER,         DIMENSION( : ),       ALLOCATABLE :: predSubIndex  ! n_preds
  REAL( fp_kind ), DIMENSION( :,: ),     ALLOCATABLE :: Coeff         ! n_preds x Nlay
  REAL( fp_kind ), DIMENSION( :,:,: ),   ALLOCATABLE :: Tau           !0:Nlay x Nangle x Natm

  REAL( fp_kind ), DIMENSION( :,:,: ),   ALLOCATABLE :: TauTotal       !0:Nlay x Nangle x Natm
  REAL( fp_kind ), DIMENSION( :,:,: ),   ALLOCATABLE :: TauFactor      !0:Nlay x Nangle x Natm
  REAL( fp_kind ), DIMENSION( :,:,: ),   ALLOCATABLE :: TauPred        !0:Nlay x Nangle x Natm
  REAL( fp_kind ), DIMENSION( :,:,: ),   ALLOCATABLE :: TauTotalPred   !0:Nlay x Nangle x Natm

  INTEGER,         DIMENSION( : ),       ALLOCATABLE :: TauProfile_Channel      ! NCHAN
  REAL(fp_kind),   DIMENSION( : ),       ALLOCATABLE :: TauProfile_Angle        ! Nangle
  INTEGER,         DIMENSION( : ),       ALLOCATABLE :: TauProfile_Profile      ! Natm
  INTEGER,         DIMENSION( : ),       ALLOCATABLE :: TauProfile_Component_Set! NABS
  REAL(fp_kind),   DIMENSION( :,:,: ),   ALLOCATABLE :: Geometric_Angle         ! Nlay x Nang x Natm

  REAL( fp_kind ), DIMENSION( :,: ),     ALLOCATABLE :: X             ! (Nangle x Natm) x n_preds  for each layer
  REAL( fp_kind ), DIMENSION( : ),       ALLOCATABLE :: Y             ! (Nangle x Natm) for each layer
  REAL( fp_kind ), DIMENSION( : ),       ALLOCATABLE :: weight        ! (Nangle x Natm) for each layer
  REAL( fp_kind ), DIMENSION( : ),       ALLOCATABLE :: CC            ! n_preds 
  REAL( fp_kind ), DIMENSION( :, : ),    ALLOCATABLE :: XX            ! n_preds x n_preds 
  REAL( fp_kind ), DIMENSION( : ),       ALLOCATABLE :: XY            ! n_preds 

  REAL( fp_kind ), DIMENSION( :,: ),     ALLOCATABLE :: X1             ! (Nangle x Natm) x n_preds  for each layer
  REAL( fp_kind ), DIMENSION( : ),       ALLOCATABLE :: Y1             ! (Nangle x Natm) for each layer
  REAL( fp_kind ), DIMENSION( : ),       ALLOCATABLE :: weight1        ! (Nangle x Natm) for each layer

  REAL( fp_kind ), DIMENSION( :,: ),     ALLOCATABLE :: tb_lbl    ! Nangle x Natm
  REAL( fp_kind ), DIMENSION( :,: ),     ALLOCATABLE :: tb_pred   ! Nangle x Natm

  REAL( fp_kind ), DIMENSION( :,:,: ),   ALLOCATABLE :: layer_od_path   !Nlay x Nangle x Natm
  TYPE(StatTbTau), TARGET :: stat0, stat   
  TYPE(StatTbTau), POINTER :: stat_p   

  ! Reference profiles
  REAL( fp_kind ), DIMENSION( : ),   ALLOCATABLE :: Ref_Level_Pressure
  REAL( fp_kind ), DIMENSION( : ),   ALLOCATABLE :: Ref_Temperature
  REAL( fp_kind ), DIMENSION( :,: ), ALLOCATABLE :: Ref_absorber
  REAL( fp_kind ), DIMENSION( :,: ), ALLOCATABLE :: Min_absorber
  REAL( fp_kind ), DIMENSION( :,: ), ALLOCATABLE :: Max_absorber

  ! Profile arrys
  REAL( fp_kind ), DIMENSION( : ),   ALLOCATABLE :: Temperature
  REAL( fp_kind ), DIMENSION( :,: ), ALLOCATABLE :: Absorber

  REAL( fp_kind ) :: wgt, OD, path, frequency, VirtEmiss, &
                     H_top, sin_ang

  INTEGER :: j ! n_Absorbers
  INTEGER :: k ! n_Layers, n_Levels
  INTEGER :: m ! n_Profiles
  INTEGER :: i ! n_Angles 
  INTEGER :: jp ! n_preds
  INTEGER :: ichan ! n_channels

  INTEGER :: Nsample, Nsample_max ! total sample 
  INTEGER :: kp, n_predSub, endOfselection

  CHARACTER(80)     :: ID_Tag
  CHARACTER(256)    :: Title
  CHARACTER(3000)   :: History
  CHARACTER(3000)   :: Comment
  CHARACTER(3000)   :: algorithm_comments
  LOGICAL :: isUseful 
  ! Read in Group ID
  WRITE(*, *)'Enter Group ID: 1,2 or 3'
  READ(*, '(i5)')This_Group

  Write(algorithm_comments, '("Training parameters: Surf_emi_ir=",&
                              & f6.4, 1x, "; Surf_emi_mw=", f6.4)')&
                              VirtEmiss_ir, VirtEmiss_mw

  ! get number of components
  Ncom = Get_n_Components(This_Group) 

  !--- read prameters from external file
 
  CALL Get_parameter_list()

  !--- read spectral coefficients

  Error_Status = SpcCoeff_netCDF_ReadFile( inFilename_spcCoef, SpcCoeff )  

  IF( Error_Status /= SUCCESS ) THEN
    print *, '### ERROR ###'        
    print *, 'Fail to read a spectral coefficient file'
    print *, 'File name: ', InFileName_SpcCoef 
    STOP 90
  END IF   

  ! Set surface emissivity
  IF(SpcCoeff%Sensor_Type == MICROWAVE_SENSOR)THEN
    VirtEmiss = VirtEmiss_mw
  ELSE
    VirtEmiss = VirtEmiss_ir
  END IF
  
  !=======================================================================
  !--- read atmospheric profiles, and obtain the reference profile
  !=======================================================================
 
  ! Inquire the AtmProfile file for the dimensions
  error_status = Inquire_AtmProfile_netCDF( &
                   inFileName_atmProfile, &
                   n_Layers    = Nlay, &
                   n_Absorbers = Nabs, &
                   n_Profiles  = Natm ) 
  IF ( error_status /= SUCCESS ) THEN
    message = 'Error inquiring '//TRIM(inFileName_atmProfile)//' for dimensions.'
    CALL Display_Message( PROGRAM_NAME, message, FAILURE ); STOP
  END IF 

  ! Allocate the AtmProfile array
  ALLOCATE( atmprofile(Natm), STAT=allocate_status )
  IF ( allocate_status /= 0 ) THEN
    message = 'Error allocating AtmProfile array.'
    CALL Display_Message( PROGRAM_NAME, message, FAILURE ); STOP
  END IF 

  ! Read the AtmProfile file
  error_status = Read_AtmProfile_netCDF( &
                   inFileName_atmProfile, &
                   AtmProfile, &
                   Reverse = .TRUE. ) 
  IF ( error_status /= SUCCESS ) THEN
    message = 'Error reading '//TRIM(inFileName_atmProfile)//' data.'
    CALL Display_Message( PROGRAM_NAME, message, FAILURE ); STOP
  END IF 

  ! Convert altitudes from metres to kilometres
  DO m = 1, Natm
    AtmProfile(m)%LEVEL_ALTITUDE = AtmProfile(m)%LEVEL_ALTITUDE * 0.001_fp_kind
  END DO
  
  Nabs = Get_n_Absorbers(This_Group)
  NsensorChan = SpcCoeff%n_Channels
  n_predictors_max = Get_max_n_Predicotrs(This_Group)
  
  ! get component IDs
  Component_ID = Get_Component_ID(icom, This_Group)

  ! Allocate memory for absorber id and index arrays
  ALLOCATE( absorber_id(Nabs),  &
            absorber_index(Nabs), &
            STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error allocating absorber ID and Index arrays. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          FAILURE )
    STOP
  END IF

  !#--------------------------------------------------------------------------#  
  !#              -- Set the absorbers and their IDs and Indexes  --          #           
  !#--------------------------------------------------------------------------#  
  DO i = 1, Nabs
   absorber_id(i) = Get_Absorber_ID(i, This_Group)
   absorber_index(i:i) = PACK( (/(j, j = 1, AtmProfile(1)%n_Absorbers)/), &
                               AtmProfile(1)%Absorber_ID == absorber_id(i) )
   ! save index for water vapor
   IF(absorber_id(i) == H2O_ID)THEN
     H2O_idx = absorber_index(i)
   END IF
  END DO
  IF( ANY(absorber_index .LT. 1) )THEN
    CALL Display_Message(  PROGRAM_NAME, &
                          'Error: at least one absorber ID can not be found.)', &
                           FAILURE ) 
    STOP
  ENDIF

  !#--------------------------------------------------------------------------#  
  !#              -- Allocate memory for profile arays  --                    #           
  !#--------------------------------------------------------------------------#  
  ALLOCATE( Ref_Level_Pressure(0:Nlay),  &
            Ref_Temperature(Nlay),       &
            Ref_absorber(Nlay, Nabs),    &
            Min_absorber(Nlay, Nabs),    &
            Max_absorber(Nlay, Nabs),    &
            Temperature(Nlay),           &
            absorber(Nlay, Nabs),        &
            STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error allocating Profile arrays. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          FAILURE )
    STOP
  END IF

  ! Calculating the reference profiles
  ! ----------------------------------
  ! Initialise summation and min/max variables
  ref_level_pressure = ZERO
  ref_temperature    = ZERO
  ref_absorber       = ZERO
  min_absorber       =  1.0e+10_fp_kind
  max_absorber       = -1.0e+10_fp_kind

  ! Begin summation loop
  Ref_Sum_Loop: DO m = 1, Natm
    Ref_Level_Pressure(0) = ref_level_pressure(0) + AtmProfile(m)%Level_Pressure(1)
    DO k = 1, Nlay
      Ref_Level_Pressure(k) = ref_level_pressure(k) + AtmProfile(m)%Level_Pressure(k+1)
      Ref_Temperature(k)    = ref_temperature(k)    + AtmProfile(m)%Layer_Temperature(k)
      DO j = 1, Nabs
        Ref_absorber(k,j) = ref_absorber(k,j) + AtmProfile(m)%Layer_Absorber(k,absorber_index(j))
        Min_absorber(k,j) = MIN(min_absorber(k,j),AtmProfile(m)%Layer_Absorber(k,absorber_index(j)))   
        Max_absorber(k,j) = MAX(max_absorber(k,j),AtmProfile(m)%Layer_Absorber(k,absorber_index(j)))   
      END DO
    END DO
  END DO Ref_Sum_Loop

  ! Compute average values for reference
  ref_level_pressure = ref_level_pressure / REAL(Natm,fp_kind)
  ref_temperature    = ref_temperature    / REAL(Natm,fp_kind)
  ref_absorber       = ref_absorber       / REAL(Natm,fp_kind)



  ! Get TauProfile array dimensions

  Error_Status = Inquire_TauProfile_netCDF( TRIM( inFilename_tauProfile ),             &
                                                  n_Layers     = n_Layers,             &
                                                  n_Channels   = n_Channels,           &
                                                  n_Angles     = n_Angles,             &
                                                  n_Profiles   = n_Profiles,           &
                                                  n_Molecule_Sets  =n_components)

  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error Inquiring TauProfile metadata.', & 
                           Error_Status )
    STOP
  END IF
 
  IF (Nlay /= n_Layers .or. Natm /= n_Profiles) THEN
  
    PRINT *, 'Error: the number of layers or number of profiles are not consistent between atmFile and transFile'
    STOP
    
  ENDIF


  ! Get TauProfile metadata

  ALLOCATE( TauProfile_Channel(n_Channels), &
            TauProfile_Angle(n_Angles), &
            TauProfile_Profile(n_Profiles), &
            TauProfile_Component_Set(n_components), &
            Geometric_Angle (n_layers, n_angles, n_Profiles), &
            STAT = Allocate_Status )
  
  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error allocating TauProfile metadata arrays. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          FAILURE )
    STOP
  END IF
            

  Error_Status = Inquire_TauProfile_netCDF(  TRIM( inFilename_tauProfile),              &
                                             Channel_List = TauProfile_Channel,         &      
                                             Angle_List   = TauProfile_Angle,           &      
                                             Profile_List = TauProfile_Profile,         &      
                                             Molecule_Set_List = TauProfile_Component_Set, &
                                             ID_Tag  = ID_Tag            , &  ! Optional output
                                             Title   = Title             , &  ! Optional output
                                             History = History           , &  ! Optional output
                                             Comment = Comment          )     ! Optional output
                                             
 
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error Inquiring TauProfile metadata.', & 
                           Error_Status )
    STOP
  END IF

  !--- check whether or not the transmittances exist for input component
  IF (all(TauProfile_Component_Set /= Component_ID ) )THEN
  
     CALL Display_Message( PROGRAM_NAME, &
                           'No transmittance exist for the input gas ID', &
                           FAILURE )
    STOP
  
  ENDIF

  ! --- read zenith angle profiles
  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error allocating Geometric_Angle array. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          FAILURE )
    STOP
  END IF
  
  IF(SpcCoeff%Sensor_Type == MICROWAVE_SENSOR)THEN
    DO i = 1, n_Angles
    DO m = 1, Natm
      Geometric_Angle(:, i, m)  = TauProfile_Angle(i)  ! secant zenith angle
    END DO
    END DO
  ELSE
    ! For non MW sensor, read in zenith angle profies (vertical) 
    Error_Status = Read_GeometricAngle_netCDF( inFilename_tauProfile, &  ! Input
                                               Geometric_Angle )         ! Output
    IF ( Error_Status /= SUCCESS ) THEN 
       CALL Display_Message( PROGRAM_NAME, &
                             'Error reading geometricAnlge data.', & 
                             Error_Status )
      STOP
    END IF
    Geometric_Angle = ONE/COS(Geometric_Angle*DEGREES_TO_RADIANS)  ! convert to secant zenith angle
  END IF
 
  ! ------------------------------------------------------------
  ! Allocate the Predictor structure and the Predictors array  
  ! ------------------------------------------------------------
  Allocate_Status = Allocate_Predictor( nLay,   &
                                        Ncom,   &
                                        n_Predictors_max, &
                                        Predictor )
  IF ( Allocate_Status /= SUCCESS ) THEN
    WRITE( Message, '( "Error allocating Predictor structure STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          FAILURE )
    STOP
  END IF

  ALLOCATE( Predictors(Nlay, n_Predictors_max, n_Angles, Natm), &
            layer_od_path(Nlay, n_Angles, Natm), &
            STAT = Allocate_Status )
  
  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error allocating Predictors array. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          FAILURE )
    STOP
  END IF

  !=======================================================================
  ! Compute the atmos predictors (all predictors in the pool for each absorber) 
  !=======================================================================
  Compute_Predictor_Loop: DO m = 1, Natm
    Temperature = AtmProfile(m)%Layer_Temperature
    DO j = 1, Nabs
      Absorber(:,j) = AtmProfile(m)%Layer_Absorber(:,absorber_index(j))  
    END DO
    H_top = AtmProfile(m)%LEVEL_ALTITUDE(1)
    DO i = 1, n_Angles
      CALL Compute_Predictor(This_Group, Temperature, Absorber,   &
                             Ref_Level_Pressure, Ref_Temperature, &
                             Ref_Absorber, Geometric_Angle(:,i,m), &
                             Predictor)
      n_predictors = Predictor%n_CP(icom)                                           
      Predictors(:, 1:n_predictors, i, m) = Predictor%X(:, 1:n_predictors,icom)    
    END DO
  END DO Compute_Predictor_Loop


  ALLOCATE( Coeff(n_predictors, Nlay), &
            predSubIndex(n_predictors), &
            STAT = Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error allocating coeff array. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          FAILURE )
    STOP
  END IF
  
  !-----------------------------------
  !     Channel loop
  !-----------------------------------

  Channel_loop: DO Ichan = iChan_start, iChan_end
   
    ALLOCATE(  Tau(0:Nlay, n_Angles, Natm),           &
               TauTotal(0:Nlay, n_Angles, Natm),      &
               TauFactor(0:Nlay, n_Angles, Natm),     &
               TauPred(0:Nlay, n_Angles, Natm),       &
               TauTotalPred(0:Nlay, n_Angles, Natm),  &
               tb_pred(n_Angles,Natm), &
               tb_lbl(n_Angles,Natm), &
               STAT = Allocate_Status )
  
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '( "Error allocating Tau (transmittance) array. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF

    ! Read transmittance for this channel and for icom
    DO m= 1, Natm   
      DO i = 1,  n_Angles  
       Tau(0, i, m ) = ONE
       TauTotal(0, i, m) = ONE
         
       Error_Status = Read_TauProfile_netCDF( TRIM(inFilename_tauProfile),       &
                                              TauProfile_Channel(Ichan),         &
                                              TauProfile_Angle(i),               &
                                              TauProfile_Profile(m),             &
                                              TOT_ComID,                         & ! Molecule_AL
                                              TauTotal(1:n_Layers,i,m),          &
                                              Quiet=1                )

       IF ( Error_Status /= SUCCESS ) THEN
          CALL Display_Message( PROGRAM_NAME, &
                               'Error reading total tau from TauProfile_File.', &
                                Error_Status )
          STOP
       END IF

       Error_Status = Read_TauProfile_netCDF( TRIM(inFilename_tauProfile),       &
                                              TauProfile_Channel(Ichan),         &
                                              TauProfile_Angle(i),               &
                                              TauProfile_Profile(m),             &
                                              Component_ID,                      &
                                              Tau(1:n_Layers,i,m),               &
                                              Quiet=1                )

       IF ( Error_Status /= SUCCESS ) THEN
          CALL Display_Message( PROGRAM_NAME, &
                               'Error reading TauProfile_File.', &
                                Error_Status )
         STOP
       END IF

        DO k = 1, Nlay      ! Loop over layers

          IF(ANY(SpcCoeff%WMO_Sensor_Id == INTERFEROMETER_LIST))THEN
            !--- Set Tau to 1 if it is larger than 1
            IF (Tau(k, i, m) > ONE ) THEN
              Tau(k, i, m) = ONE
            END IF
            !--- Set Tau(k) to Tau(k-1) if Tau(k) is larger than Tau(k-1)
            IF( TauTotal(k-1, i, m) < TauTotal(k, i, m) ) THEN
              TauTotal(k, i, m) = TauTotal(k-1, i, m)
            END IF
          
          ELSE
            !*** Conditions not allowed fo non-interferometer sensors ***
            !--- check total tau within 0 to 1 
            IF (TauTotal(k, i, m) < ZERO) THEN
              WRITE( Message, '( "Error: Total trans is less than 0. Tau =  ", f16.8 )' ) &
                                 TauTotal(k, i, m)
              CALL Display_Message( PROGRAM_NAME, &
                                    TRIM( Message ), &
                                    FAILURE )
              STOP 90
            ENDIF 

            IF (TauTotal(k, i, m) > ONE ) THEN
              WRITE( Message, '( "Error: Total trans is larger than 1. Tau =  ", f16.8 )' ) &
                                 TauTotal(k, i, m)
              CALL Display_Message( PROGRAM_NAME, &
                                    TRIM( Message ), &
                                    FAILURE )
              STOP 90
            ENDIF 

            !--- check if total tau decreasing from top to bottom
            IF( TauTotal(k-1, i, m) < TauTotal(k, i, m) ) THEN

              ! the decreasing check is applied for significant tau
              IF(TauTotal(k-1, i, m) < 1.0e-12_fp_kind)THEN
                TauTotal(k, i, m) = TauTotal(k-1, i, m)
              ELSE
                WRITE( Message, '( "Warning: Total trans increase as descending. k,  Tau(k-1), Tau(k) = ", I5, 2es20.12 )' ) &
                                   k, TauTotal(k-1, i, m), TauTotal(k, i, m)
                TauTotal(k, i, m) = TauTotal(k-1, i, m)
                CALL Display_Message( PROGRAM_NAME, &
                                      TRIM( Message ), &
                                      WARNING )
!                WRITE( Message, '( "Error: Total trans increase as descending. Tau(k-1), Tau(k) = ", 2es20.12 )' ) &
!                                   TauTotal(k-1, i, m), TauTotal(k, i, m)
!                CALL Display_Message( PROGRAM_NAME, &
!                                      TRIM( Message ), &
!                                      FAILURE )
!                STOP 90
              END IF
            END IF
          
          END IF       

          !--- revise missing value and check tolerance
          IF ( TauTotal(k, i, m) < TOLERANCE ) THEN
             TauTotal(k, i, m) = ZERO
          ENDIF
           
          IF (Tau(k, i, m) < ZERO ) THEN
             Tau(k, i, m) = RMISS
          ELSE IF (Tau(k, i, m) < TOLERANCE) THEN
             Tau(k, i, m) = ZERO
          ENDIF
            
        ENDDO  
      END DO
    END DO

    WHERE( Tau > TOLERANCE)
      TauFactor = TauTotal/Tau
    ELSEWHERE
      TauFactor = ZERO
    ENDWHERE

   
!**** PvD: Not sure the dimension changes here will work!!! ****
    !--- calculate Tb from LBL trans
    CALL Compute_BrightTemp(ichan, n_angles, Natm, &
                            TauTotal, AtmProfile, &
                            VirtEmiss, SpcCoeff, &
                            tb_lbl)

   !----------------------------------------------------
   ! Check the significance of this tau component
   ! If not significant, set all tau coefficient to 0
   ! and exit
   !----------------------------------------------------

    TauTotalPred = TauFactor
    !--- correct transmittance profile if it is not monotonously decreasing.
    DO i = 1,  n_Angles ! Loop over scan angles
      DO m= 1, Natm   ! Loop over profiles
       DO k = 1, Nlay         ! Loop over layers
         IF(TauTotalPred(k,i,m) > TauTotalPred(k-1,i,m) ) THEN
           TauTotalPred(k,i,m) = TauTotalPred(k-1,i,m) 
         ENDIF
       END DO 
      ENDDO
    ENDDO

!**** PvD: Not sure the dimension changes here will work!!! ****
    !--- calculate Tb from LBL trans
    CALL Compute_BrightTemp(ichan, n_angles, Natm, &
                            TauTotalPred, AtmProfile, &
                            VirtEmiss, SpcCoeff, &
                            tb_pred)

    !--- calculate statistics
    CALL Calc_StatTransTemp( &
          TauTotal(:,1:n_Angles-1,:),  tb_lbl(1:n_Angles-1,:),     &                     
          TauFactor(:,1:n_Angles-1,:), tb_pred(1:n_Angles-1,:),    &                     
          stat0)                                                                                 

    ! set these variables to default values
    predSubIndex = 0  
    n_predSub = 0     
    Coeff(:,:) = ZERO
    stat_p => stat0
    
    !-------------------------------------------------------------------
    ! If the RMS BT difference is larger than COMPONENT_SIGNIFICANCE, 
    ! then do regression
    !-------------------------------------------------------------------
    IF(stat0%tbmaxerr > COMPONENT_SIGNIFICANCE)THEN 
      
      !=======================================================================
      ! Compute layer optical path 
      !=======================================================================
      DO i = 1,  n_Angles       ! Loop over scan angles                                       
        DO m= 1, Natm   ! Loop over profiles                                         
          DO k = 1, Nlay                                             
             IF (Tau(k-1, i, m) < 1.0e-10_fp_kind .or. Tau(k, i, m) < 1.0e-10_fp_kind) THEN
               layer_od_path(k, i, m) = INFINITE
             ELSE
               layer_od_path(k, i, m) = -LOG(Tau(k, i, m) / Tau(k-1, i, m) )
             END IF 
          END DO
        END DO
      END DO  
   
      !----------------------------------------------------
      ! compute tau coefficients and
      ! stepwise search a best set of predictors
      !----------------------------------------------------
      ! initialize the serch routine
      CALL reset_search_predSubset()

      PredictorSet_search_loop: DO 

        endOfselection = Get_subsetIndex_rev_stepwise(predSubIndex, n_predSub)

        if(endOfselection == 1) then                                          
                                                                            
          call retrieve_bestset_rev_stepwise(predSubIndex, n_predSub)         
                                                                              
          exit PredictorSet_search_loop                                       
                                                                            
        endif                                                                 
        IsUseful = Check_Usefulness(predSubIndex, n_predSub, &
                     SpcCoeff%Sensor_Type, SpcCoeff%Wavenumber(ichan), Component_ID, This_Group)

        IF( .NOT. IsUseful )CYCLE

        !--------------------------------------------------------------
        ! In the routine: do regression looping over each layer
        !-------------------------------------------------------------- 
        CALL DoRegression()

        ! evaluate fitting errors and save the current set of indexes if the
        ! current fitting yields a smaller error (smaller the the previous one + 
        ! COMPONENT_SIGNIFICANCE).
        CALL evaluate_fittingError(stat%tbrmse, COMPONENT_SIGNIFICANCE, predSubIndex)
   
      END DO PredictorSet_search_loop

      ! get statistics for the best predictor set
      CALL DoRegression()

      ! compare to the result of no predictor case
      IF(stat%tbrmse + COMPONENT_SIGNIFICANCE > stat0%tbrmse )THEN ! the case of no predictors is better
        predSubIndex = 0 
        n_predSub = 0    
        Coeff(:,:) = ZERO
        stat_p => stat0
      ELSE
        stat_p => stat
      END IF

    END IF

    IF(SpcCoeff%Sensor_Type == INFRARED_SENSOR)THEN
      frequency = SpcCoeff%Wavenumber(Ichan)
    ELSE
      frequency = SpcCoeff%Frequency(Ichan)
    ENDIF
  
    ! write the statistics out  
    write(40, '(i4, f12.4, 4E13.5, 50I3)')TauProfile_Channel(Ichan), frequency, & 
                         stat_p%tbmean, stat_p%tbsderr, stat_p%tbrmse, stat_p%tbmaxerr, &
                         n_predSub, predSubIndex(1:n_predSub)

    !---------------------------------
    ! Write the results to a file
    !--------------------------------- 
    Error_Status = Write_Out(This_Group,                &
                             n_predSub,                 & 
                             predSubIndex,              &                                     
                             Nlay,                      &                                      
                             Absorber_ID,               &                                      
                             Component_ID,              &                                      
                             TauProfile_Channel(Ichan), &                                      
                             Coeff,                     &                                      
                             Ref_Level_Pressure,        &                                      
                             Ref_Temperature,           &                                      
                             Ref_Absorber,              &
                             Min_Absorber,              &
                             Max_Absorber)
                             
    IF ( Error_Status /= 0 ) THEN                                                              
      WRITE( Message, '( "Error writing our tau coefficient data to a file", i0)' ) &          
                      Error_Status                                                             
      CALL Display_Message( PROGRAM_NAME, &                                                    
                            TRIM( Message ), &                                                 
                            FAILURE )                                                          
      STOP 90                                                                                     
    END IF                                                                                     
    
    DEALLOCATE(  Tau, TauTotal, TauTotalPred, TauFactor, tb_pred, tb_lbl, &        
               STAT = Allocate_Status )                                                      
  
    IF ( Allocate_Status /= 0 ) THEN                                                         
      WRITE( Message, '( "Error allocating Tau (transmittance) array. STAT = ", i5 )' ) &    
                      Allocate_Status                                                        
      CALL Display_Message( PROGRAM_NAME, &                                                  
                            TRIM( Message ), &                                               
                            FAILURE )                                                        
      STOP 90                                                                                  
    END IF                                                                                   

      
  END DO Channel_loop
    

  ! Deallocate the Predictor structure
  Allocate_Status = Destroy_Predictor(Predictor)
  IF ( Allocate_Status /= SUCCESS ) THEN
    WRITE( Message, '( "Error deallocating Predictor structure. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          FAILURE )
  END IF

  ! deallocate the AtmProfile profile data structure
  Error_Status = Destroy_AtmProfile( AtmProfile)  
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error deallocating AtmProfile structure.', & 
                           Error_Status )
  END IF

  ! deallocate the SpcCoeff data structure
  CALL SpcCoeff_Destroy( SpcCoeff )  
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error deallocating SpcCoeff structure.', & 
                           Error_Status )
  END IF

  ! -----------------------------------------
  ! Deallocate arrays
  ! -----------------------------------------
  DEALLOCATE( Predictors,        &
            layer_od_path,       &
            Ref_Level_Pressure,  &
            Ref_Temperature,     &
            Ref_absorber,        &
            Min_absorber,        &
            Max_absorber,        &
            Temperature,         &
            absorber,            &
            Coeff,               &
            predSubIndex,        &
            atmprofile,          &
            STAT = Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error deallocating arrays. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          FAILURE )
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
   
  subroutine DoRegression()                                                         

    ALLOCATE(  X(Nangle_regression*Natm, n_predSub), & 
               Y(Nangle_regression*Natm),            &
               weight(Nangle_regression*Natm),       &
               CC(n_predSub),                        & 
               STAT = Allocate_Status )     
  
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '( "Error allocating X, Y, and CC array. STAT = ", i5 )') &
             Allocate_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), & 
                            FAILURE )
      STOP 90
    END IF

    Nsample_max = Nangle_regression*Natm
   
    Layer_loop:  DO k = 1, Nlay      ! Loop over layers

      X(:,:)   = ZERO 
      Y(:)     = ZERO 
      weight(:)= ZERO 
      CC(:)    = ZERO

      IF(ALL(TauTotal(k, 1:Nangle_regression, 1:Natm) < TOTAL_TAU_MIN ))CYCLE

      Nsample = 0    
      DO i = 1,  Nangle_regression ! Loop over scan angles
        DO m= 1, Natm   ! Loop over profiles

          wgt = SQRT(TauTotal(k-1, i, m) * TauTotal(k, i, m))      

          IF(wgt <= TOLERANCE .OR. layer_od_path(k, i, m) > OD_MAX) CYCLE

          Nsample = Nsample + 1         
            
          weight(Nsample) = wgt 
 
          Y(Nsample) = layer_od_path(k, i, m)          

          DO jp = 1, n_predSub     
            X(Nsample, jp ) = Predictors(k, predSubIndex(jp), i, m) 
          END DO   
 
        END DO ! end of profile loop 
      END DO  ! End of scan angle loop

      ALLOCATE(  X1(Nsample, n_predSub),  &    
                 Y1(Nsample),             &
                 XX(n_predSub,n_predSub), &
                 XY(n_predSub),           &       
     weight1(Nsample),        &       
      STAT = Allocate_Status )     
  
      IF ( Allocate_Status /= 0 ) THEN       
       WRITE( Message, '( "Error allocating X1, Y1, and weight1 array. STAT = ", i5 )' ) &       
             Allocate_Status 
       CALL Display_Message( PROGRAM_NAME,    &    
                             TRIM( Message ), &     
                             FAILURE )   
     STOP 90      
      END IF      

!      weight1(1: Nsample) = ONE
      weight1(1: Nsample) = weight(1:Nsample)
      
      DO i=1, Nsample     
       X1(i, :) = X(i, : ) * weight1(i)       
       Y1(i) = Y(i) * weight1(i)      
      END DO

      CALL Compute_AutoCross_Matrix(X1, Y1, XX, XY)
      ! Add gamma (noise) at the top to stabilize
      IF(k <= 3)THEN
        DO jp = 1, n_predSub
          XX(jp,jp) = XX(jp, jp)*(ONE + 1.e-6_fp_kind/REAL(k, fp_kind))
        END DO
      END IF

      Error_Status = Compute_RegressionCoeff(XX, XY, CC)
      IF(Error_Status /= 0)THEN
       
       CALL Display_Message( PROGRAM_NAME, &      
          "cannot obtain the solution for this set of predictors,"//&
          " so set the Tau coefficients to zero", &     
          WARNING )    
        CC = ZERO      
      END IF      


      DO i=1, n_predSub      
        Coeff(i,k) = CC(i)     
      END DO      

      DEALLOCATE(X1, Y1, XX, XY, weight1, & 
                 STAT = Allocate_Status ) 
  
      IF ( Allocate_Status /= 0 ) THEN      
        WRITE( Message, '( "Error deallocating X1, Y1, and weight1 array. STAT = ", i5 )' ) &      
        Allocate_Status   
        CALL Display_Message( PROGRAM_NAME, &      
                              TRIM( Message ), &      
                              FAILURE )       
      STOP 90      
      END IF      
 
    END DO Layer_loop    


    !----------------------------------------------                                                   
    ! Compute predicted transmittances and Tb                                                         
    !----------------------------------------------                                                   
    DO i = 1,  Nangle_regression    ! Loop over scan angles                                           
      DO m= 1, Natm   ! Loop over profiles                                                          
        TauPred(0, i, m) = ONE 
        path = ZERO                                                                        
        DO k = 1, Nlay     ! Loop over layers                                                       
          OD = ZERO                                                                          
          DO jp = 1, n_predSub
            OD = OD + REAL(Coeff(jp,k), Single) * Predictors(k, predSubIndex(jp), i, m)           
          END DO 

          IF(OD > OD_MAX)OD = OD_MAX
          ! IF it is a non-ozone component, force OD >= 0
          ! For ozone, the effective optical depth can be negtive
          IF(Component_ID /= Get_Ozone_Component_ID(This_Group) .AND. OD < ZERO)OD = ZERO
          
          path = path + OD

          TauPred(k, i, m) = exp(-path)                                  

        END DO
      END DO                                                                                          
    END DO                                                                                            

    TauTotalPred = TauFactor * TauPred 
     
    !--- correct transmittance profile if it is not monotonously decreasing.    
    DO i = 1,  Nangle_regression            ! Loop over scan angles             
      DO m= 1, Natm   ! Loop over profiles                                  
        DO k = 1, Nlay      ! Loop over layers                              
          IF(TauTotalPred(k,i,m) > TauTotalPred(k-1,i,m) ) THEN             
            TauTotalPred(k,i,m) = TauTotalPred(k-1,i,m)                         
          ENDIF                                                                 
        END DO                                                              
      ENDDO                                                                 
    ENDDO                                                                   

    !--- calculate Tb from LBL trans								    ! 
    CALL Compute_BrightTemp(ichan, Nangle_regression, Natm, &
                            TauTotalPred, AtmProfile, &
                            VirtEmiss, SpcCoeff, &
                            tb_pred)

    !--- calculate statistics      
    CALL Calc_StatTransTemp( &       
           TauTotal(:,1:Nangle_regression,:), tb_lbl(1:Nangle_regression,:),         &       
           TauTotalPred(:,1:Nangle_regression,:), tb_pred(1:Nangle_regression,:),    &       
           stat)      

    DEALLOCATE(  X, Y, weight, CC, &      
                 STAT = Allocate_Status )     
  
    IF ( Allocate_Status /= 0 ) THEN      
      WRITE( Message, '( "Error deallocating X, Y, and CC array. STAT = ", i5 )' ) &    
             Allocate_Status   
      CALL Display_Message( PROGRAM_NAME, &   
                            TRIM( Message ), &       
                            FAILURE )    
      STOP 90   
    END IF       

  END subroutine DoRegression
  
  SUBROUTINE Compute_BrightTemp(ichan, n_angles, n_profiles, tau, &
                                atmprofile, &
                                emi, SpcCoeff, tb)
    integer, intent(in)        :: ichan
    integer, intent(in)        :: n_angles  
    integer, intent(in)        :: n_profiles  
    real(fp_kind), intent(in)  :: tau(0:, :, :)  ! dimension n_layers x n_angles x n_profiles
    type(atmprofile_type), intent(in) :: atmprofile(:)    ! dimension n_profiles
    real(fp_kind), intent(in)  :: emi            ! surface emissivity
    TYPE(SpcCoeff_type)        :: SpcCoeff
    real(fp_kind), intent(out) :: tb(:,:)        ! dimension n_angles x n_profiles

    ! local
    integer :: i, m, n_levels

    DO i = 1,  n_Angles     ! Loop over scan angles
      DO m= 1, n_profiles   ! Loop over profiles   
        n_levels = atmprofile(m)%n_Levels
        tb(i,m) = Pred_BrightTemp(Ichan,         & 
                                  tau(0:, i,m),  & 
                                  atmprofile(m)%layer_temperature,      & 
                                  atmprofile(m)%level_temperature(n_levels), &         
                                  emi, SpcCoeff )
      ENDDO
    ENDDO 
    
  END SUBROUTINE Compute_BrightTemp 

    
  
  SUBROUTINE Get_parameter_list()
      
    !---local
    
    integer,parameter :: InFileId_parameterlist = 11

    !--- parameterlist

    namelist /SATSEN/       File_Prefix, &     ! name of satellite sensor
                            iChan_start, &     ! starting ch seq #
                            iChan_end          ! ending ch seq #
    namelist /GENCOEF/      icom,    &         ! component index
                            Nangle_regression         ! # of incidence angles used in coefficient calculation
    namelist /FILENAMES/ inFilename_spcCoef, &   ! Planck coeff. file
                         inFilename_atmProfile,& ! AtmosProfile file
                         inFilename_tauProfile,& ! transmittance file
                         inFilename_tauCoeff     ! tau coeffs of non-corrTerm

    open( InFileId_parameterlist,       &
          FILE   = ParameterList,       &
          STATUS = 'OLD',               &
          ACCESS = 'SEQUENTIAL',        &
          FORM   = 'FORMATTED',         &
          ACTION = 'READ'               )

    read( InFileId_parameterlist, SATSEN )
    read( InFileId_parameterlist, GENCOEF )
    read( InFileId_parameterlist, FILENAMES )

    print *, 'parameters from namelist.txt file:'
    print *, trim(inFilename_spcCoef)
    print *, trim(inFilename_atmProfile)    
    print *, trim(inFilename_tauProfile)
    print *, trim(inFilename_tauCoeff)

    close( InFileId_parameterlist )

   !--- check parameters

    if( iChan_start < 1 .or. iChan_end < 1 .or. &
        iChan_start > iChan_end )then
      print *, '### ERROR ###'
      print *, 'Specified channel numbers are not acceptable.'
      print *, 'Starting  ch seq # = ', iChan_start
      print *, 'Ending ch seq # = ', iChan_end
      stop 90
    endif

    if( icom < 1 .or. icom > Ncom)then                                    
      print *, '### ERROR ###'                                            
      print *, 'The specified component index is not acceptable: ', icom  
      print *, 'It must be great than 0 and less than ', Ncom+1           
      stop 90                                                             
    endif                                                                 


    if( Nangle_regression < 1 .or. Nangle_regression > N_ZENITH_ANGLES )then
      print *, '### ERROR ###'
      print *, 'Specified # of incidence angles is not acceptable.'
      print *, '# of inc ang = ', Nangle_regression
      stop 90
    endif
    
  END SUBROUTINE Get_parameter_list

  FUNCTION Write_Out(This_Group,   &
                     n_predictors, &
                     predSubIndex, &
                     n_Layers,     & 
                     Absorber_ID,  &
                     Component_ID, &
                     Channel,      &
                     Coeff,        & 
                     Ref_Level_Pressure, &
                     Ref_Temperature, &
                     Ref_Absorber, & 
                     Min_Absorber, &
                     Max_Absorber) &
                     RESULT(Error_Status)
    INTEGER,       INTENT( IN ) :: This_Group
    INTEGER,       INTENT( IN ) :: n_predictors
    INTEGER,       INTENT( IN ) :: predSubIndex(:)
    INTEGER,       INTENT( IN ) :: n_Layers
    INTEGER,       INTENT( IN ) :: Absorber_ID(:)
    INTEGER,       INTENT( IN ) :: Component_ID
    INTEGER,       INTENT( IN ) :: Channel
    REAL(fp_kind), INTENT( IN)  :: Coeff(:,:)
    REAL(fp_kind), INTENT( IN)  :: Ref_Level_Pressure(0:) ! K      
    REAL(fp_kind), INTENT( IN)  :: Ref_Temperature(:)     ! K      
    REAL(fp_kind), INTENT( IN)  :: Ref_Absorber(:,:)      ! K x Jm 
    REAL(fp_kind), INTENT( IN)  :: Min_Absorber(:,:)      ! K x Jm 
    REAL(fp_kind), INTENT( IN)  :: Max_Absorber(:,:)      ! K x Jm 

    INTEGER :: Error_Status

    !Local
    CHARACTER(*), PARAMETER :: SUBROUTINE_NAME = 'Write_Out'
    TYPE( ODPS_type ) :: TC
    INTEGER           :: n_Coeffs
    INTEGER           :: n_absorbers
    INTEGER           :: i, j, m, max_n_pred
!    CHARACTER(*), PARAMETER  :: file_out = "TauCoeff.bin"
    CHARACTER(*), PARAMETER  :: file_out = "TauCoeff.nc"

    n_Absorbers = SIZE(Absorber_ID)
    IF( n_predictors > 0 )THEN
      max_n_pred = MAXVAL(predSubIndex(1:n_predictors))
    ELSE
      max_n_pred = 0
    END IF
    n_Coeffs = max_n_pred * n_Layers

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

    ! Scalar IDs
    TC%Group_Index      = This_Group     
    TC%Sensor_id        = SpcCoeff%Sensor_id
    TC%WMO_Satellite_ID = SpcCoeff%WMO_Satellite_ID
    TC%WMO_Sensor_ID    = SpcCoeff%WMO_Sensor_ID                              
    TC%Sensor_Type      = SpcCoeff%Sensor_Type

    TC%Ref_Level_Pressure = Ref_Level_Pressure                                                       
    TC%Ref_Pressure       = (Ref_Level_Pressure(1:n_Layers) - Ref_Level_Pressure(0:n_Layers-1))/&    
                             LOG(Ref_Level_Pressure(1:n_Layers) / Ref_Level_Pressure(0:n_Layers-1))  
    TC%Ref_Temperature    = Ref_Temperature                                                          
    TC%Ref_Absorber       = Ref_Absorber                                                             
    TC%Min_Absorber       = Min_Absorber                                                             
    TC%Max_Absorber       = Max_Absorber                                                             
    TC%Absorber_ID        = Absorber_ID                                                              
    TC%Sensor_Channel     = Channel                                                                  
    TC%Component_ID(1)    = Component_ID
                                                             
    TC%n_Predictors(1,1)  = max_n_pred
    IF( n_Predictors > 0 )THEN                                                            
      TC%Pos_Index(1,1)     = 1
      TC%C = 0.0

      DO i = 1, n_Predictors
        j = predSubIndex(i)
        m = (j-1)*n_Layers
        DO k = 1, n_Layers
          TC%C(m+k)  = REAL(Coeff(i, k), Single)                                                                     
        END DO 
      END DO                                                                                        
    ELSE
      TC%Pos_Index(1,1)     = 0
    END IF
 
    Title = "ODPS upwelling transmittance coefficients"//' for '//trim(TC%Sensor_id) 
!    Error_Status = Write_ODPS_Binary(file_out, TC)
    Error_Status = Write_ODPS_netCDF( file_out                                                 , &
                                      TC                                                       , &
                                      Title         = TRIM(Title)                              , &
                                      History       = TRIM(PROGRAM_RCS_ID)//'  '//TRIM(History), &
                                      Comment       = TRIM(algorithm_comments)//'; '//TRIM(Comment), &
                                      Profile_Set_ID= TRIM(ID_Tag) )
 
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

  FUNCTION Check_Usefulness(predIndex, npred, Sensor_Type, freq, ComID, GroupID) &
           RESULT(IsUseful)
    INTEGER, INTENT(IN) :: predIndex(:)
    INTEGER, INTENT(IN) :: npred
    INTEGER, INTENT(IN) :: Sensor_Type
    REAL(fp_kind), INTENT(IN) :: freq
    INTEGER, INTENT(IN) :: ComID
    INTEGER, INTENT(IN) :: GroupID
    LOGICAL :: IsUseful
    
    IsUseful = .TRUE. 
    
    IF(Sensor_Type /= MICROWAVE_SENSOR)THEN ! Non MW sensor (IR sensors)
      SELECT CASE( ComID )
        CASE( CO2_ComID ) 
          IF( (freq < 1995.0_fp_kind   .OR.  &
               freq > 2660.0_fp_kind ) .AND. &
               ANY(predIndex(1:npred) == 10) ) THEN
            IsUseful = .FALSE.
            RETURN
          ENDIF 
          
          IF( (freq < 1995.0_fp_kind   .OR.  &
               freq > 2247.0_fp_kind)  .AND. &
              (GroupID == GROUP_1)     .AND. &
               ANY(predIndex(1:npred) == 11) )THEN
            IsUseful = .FALSE.
            RETURN
          END IF
          
      END SELECT
    END IF   

  END FUNCTION Check_Usefulness


END PROGRAM Compute_Coeff
