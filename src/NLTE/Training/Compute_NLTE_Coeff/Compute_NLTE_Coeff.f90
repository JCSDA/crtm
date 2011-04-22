!
! Compute_NLTE_Coeff
!
! Program to generate NLTE correction coefficients
!
!    User inputs:  
!          (1) number of training sets 
!          (2) netCDF filenames for the training sets (the number of files must match 
!              the first input)
!          (3) sensor id (example: crisB3_npp)
!          (4) path to the SpcCoeff file
! 
!    Output file:
!          (1) NLTE correction coefficient file, named as ${Sensor_Id}.NLTECoeff.bin,
!              where ${Sensor_Id} contains the sensor Id such as crisB3_npp.
!          (2) Fitting error file, named as ${Sensor_Id}.NLTECoeff.error.txt 
!          (3) sensor radiances, named as ${Sensor_Id}.radiance.txt
!
! CREATION HISTORY:
!       Written by:     Yong Han, 08-05-2010
!       Modified by:    Yong Chen, Mar-08-2010
!
PROGRAM Compute_NLTE_Coeff

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds                , ONLY: fp
  USE Message_Handler
  USE DeltaRad_TrainSet_Define  , ONLY: TrainSet_type
  USE DeltaRadTrainSet_netCDF_IO, ONLY: Read_TrainSet_netCDF
  USE CRTM_SpcCoeff             , ONLY: SC, &
                                        CRTM_SpcCoeff_Load, CRTM_SpcCoeff_Destroy
  USE Regression                , ONLY: Compute_AutoCross_Matrix, &
                                        Compute_RegressionCoeff
  USE NLTE_Parameters           , ONLY: N_NLTE_PREDICTORS, &
                                        N_PLay, &
                                        Pt 
  USE NLTECoeff_Define          , ONLY: NLTECoeff_type, &
                                        NLTECoeff_Associated, &
                                        NLTECoeff_Create
                                                                                
  USE NLTECoeff_netCDF_IO       , ONLY: NLTECoeff_netCDF_WriteFile
  USE CRTM_Planck_Functions     , ONLY: CRTM_Planck_Temperature

  ! Disable all implicit typing
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Compute_NLTE_Coeff'
  ! Start and end of the NLTE range
  REAL(fp), PARAMETER :: NLTE_Begin_Frequency = 2200.0_fp
  REAL(fp), PARAMETER :: NLTE_END_Frequency   = 2400.0_fp
  ! Constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp, ONE = 1.0_fp, TWO = 2.0_fp
  REAL(fp), PARAMETER :: D2R = 3.14159274_fp/180.0_fp
  
  REAL(fp), PARAMETER :: SCALE_FACTOR = 1.0e7_fp
  INTEGER,  PARAMETER :: NOT_NLTE = 0
  
  TYPE(TrainSet_type), ALLOCATABLE :: TrainSet(:)
  CHARACTER(256),      ALLOCATABLE :: TrainSet_fname(:)
  CHARACTER(256) :: Message, Coeff_fname, FitErr_fname
  INTEGER :: Allocate_Status, Error_Status
  INTEGER,  ALLOCATABLE :: SC_Channel_Index(:)
  INTEGER,  ALLOCATABLE :: NLTE_Channel(:)
  INTEGER,  ALLOCATABLE :: NLTE_Channel_Index(:)
  REAL(fp), ALLOCATABLE :: NLTE_Channel_Wavenumber(:)
  REAL(fp), ALLOCATABLE :: x(:,:), y(:), xx(:,:), xy(:), cc(:), Tm(:,:), Tm_lim(:,:)
  REAL(fp), ALLOCATABLE :: y_est(:,:,:,:),  y_true(:,:,:,:), Tb_est(:,:,:,:),  Tb_true(:,:,:,:), tb_lte(:,:,:)
  INTEGER :: i, k, k1, k2, iset, isen, isun, ich, iprof, idx, i_NLTE_Channel
  INTEGER :: n_Profiles, n_NLTE_Channels, n_sets, n_sensor_angles, n_sun_angles, n_points
  CHARACTER(128)  :: Sensor_id(1)
  CHARACTER(256)  :: path
  TYPE(NLTECoeff_type) :: nlte
  REAL(fp) :: dd, maxdiff, rmsdiff,  rmsdiff0, meandiff
  INTEGER,  PARAMETER :: fid_FitErr = 30, fid_radiance = 35

  ! Get user input
  WRITE(*, FMT='(/5x,"Enter the number of the training sets:")', ADVANCE='NO')
  READ(*,'(i5)') n_sets
  
  ALLOCATE( TrainSet_fname(n_Sets), &
            TrainSet(n_Sets),       &
            STAT = Allocate_Status )
  IF (Allocate_Status /= 0) THEN
    WRITE(Message,'("Error allocating the TrainSetarrays. STAT=",i0)') Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          FAILURE )
    STOP
  END IF
  DO i = 1, n_sets
  
    WRITE(*, FMT='(/5x,"Enter the netCDF filename for the ",i2, "training set (i.g. airsM1b_aqua: ")' ,ADVANCE='NO') i
    READ(*,'(a)') TrainSet_fname(i)
    TrainSet_fname(i) = ADJUSTL(TrainSet_fname(i))
  END DO
  WRITE(*, FMT='(/5x,"Enter the sensor id of the sensor that uses these training sets: ")', ADVANCE='NO')
  READ(*,'(a)') Sensor_id(1)
  Sensor_id(1) = ADJUSTL(Sensor_id(1))
  WRITE(*, FMT='(/5x,"Enter the path to the SpcCoeff data directory: ")', ADVANCE='NO')
  READ(*,'(a)') path
  path = ADJUSTL(path)

  ! load SpcCoeff data
  error_status = CRTM_SpcCoeff_Load( &                                                          
                                     Sensor_ID         = Sensor_ID , &                                         
                                     File_Path         = Path )                                        
  IF ( error_status /= SUCCESS ) THEN                                                               
    CALL Display_Message( PROGRAM_NAME,'Error loading SpcCoeff data',FAILURE )  
    Stop                                                                                      
  END IF                                                                                        

  ! Allocate channel index arrays
  !   NLTE_Channel_Index has a dimension of SC(1)%n_Channels
  !   NLTE_Channel has a dimension smaller than SC(1)%n_Channels, here
  !   it is allocated to a larger size. 
  ALLOCATE(NLTE_Channel(SC(1)%n_Channels), &
           NLTE_Channel_Index(SC(1)%n_Channels), &
           NLTE_Channel_Wavenumber(SC(1)%n_Channels), &
           SC_Channel_Index(SC(1)%n_Channels), &
           STAT = Allocate_Status )
  IF (Allocate_Status /= 0) THEN
    WRITE(Message,'("Error allocating the NLTE_Channel and NLTE_Channel_Index arrays. STAT=",i0)') Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          FAILURE )
    STOP
  END IF
  NLTE_Channel = -1
    
  !-----------------------------------------------
  ! Find the NLTE channelsRegression
  !-----------------------------------------------

  NLTE_Channel_Index = NOT_NLTE
  n_NLTE_Channels = 0
  Set_Loop:  DO iset = 1, n_sets
    Error_Status = Read_TrainSet_netCDF(TrainSet_fname(iset), &
                                        TrainSet(iset))
    ! Change radiance units to mW/(m^2.sr.cm^-1)
    TrainSet(iset)%Radiance_nlte = TrainSet(iset)%Radiance_nlte*SCALE_FACTOR
    TrainSet(iset)%Radiance_lte  = TrainSet(iset)%Radiance_lte*SCALE_FACTOR
                 
    Channel_Loop: DO ich = 1, TrainSet(iset)%n_channels 

      ! find an index match in the SC Sensor Channel array  
      idx = Find_Index( TrainSet(iset)%Channel(ich), SC(1)%Sensor_Channel )        
      IF( idx > 0 )THEN  ! match is found                            
        IF( SC(1)%Wavenumber(idx) < NLTE_Begin_Frequency .OR. &         
            SC(1)%Wavenumber(idx) > NLTE_END_Frequency )THEN            
           CYCLE Channel_Loop                                        
        END IF    
      ELSE
!        CALL Display_Message( PROGRAM_NAME, &
!                          "Error: the channel index in the training set is not found in the SC structure data", &
!                          FAILURE )
!        STOP
!        CALL Display_Message( PROGRAM_NAME, &
!                          "Warning: the channel index in the training set is not found in the SC structure data", &
!                          WARNING )
        CYCLE Channel_loop
      END IF                                                   
    
      n_NLTE_Channels = n_NLTE_Channels + 1
      NLTE_Channel(n_NLTE_Channels) = SC(1)%Sensor_Channel(idx)
      NLTE_Channel_Index(idx)  = n_NLTE_Channels
      NLTE_Channel_Wavenumber(n_NLTE_Channels) = SC(1)%Wavenumber(idx)
      SC_Channel_Index(n_NLTE_Channels) = idx
      
    END DO Channel_Loop
  END DO Set_Loop

  ! Check consistency of the sensor angle, solar angle and profile dimensions
  DO iset = 2, n_sets
    IF( TrainSet(iset)%n_sensor_angles /= TrainSet(iset-1)%n_sensor_angles .OR. &
        TrainSet(iset)%n_sun_angles    /= TrainSet(iset-1)%n_sun_angles    .OR. &
        TrainSet(iset)%n_profiles      /= TrainSet(iset-1)%n_profiles)THEN
        CALL Display_Message( PROGRAM_NAME, &                                                         
                              "Error: the sensor anlge, solar angle or profile dimensions are different,"//&    
                              " between different data sets", &                                                  
                              FAILURE )                                                               
        STOP
    END IF
  END DO                                                                                     

  n_sensor_angles = TrainSet(1)%n_sensor_angles
  n_sun_angles = TrainSet(1)%n_sun_angles
          
 ! Allocate nlte 
  CALL NLTECoeff_Create( nlte,  &
                    N_NLTE_PREDICTORS, &
                    n_sensor_angles, &
                    n_sun_angles, &
                    n_NLTE_Channels, &
                    SC(1)%n_Channels)
  IF( .NOT. NLTECoeff_Associated(nlte) )THEN
    CALL Display_Message( PROGRAM_NAME, &
                          "Error creating the nlte structure", &
                          FAILURE )
    STOP
  END IF
                    
  nlte%NLTE_Channel = NLTE_Channel(1:n_NLTE_Channels)
  nlte%C_Index = NLTE_Channel_Index
  
  WHERE( nlte%C_Index > 0 ) nlte%Is_NLTE_Channel = .TRUE.

  !-----------------------------------------------
  ! Regression to compute coefficients
  !-----------------------------------------------
  n_profiles = TrainSet(1)%n_profiles
  ALLOCATE( x(n_profiles, N_NLTE_PREDICTORS), &
            y(n_profiles),  &
            xx(N_NLTE_PREDICTORS, N_NLTE_PREDICTORS), &
            xy(N_NLTE_PREDICTORS), &
            Tm(N_PLay, n_profiles), &
            Tm_lim(3, N_PLay), & 
            cc(N_NLTE_PREDICTORS), &
            y_est(n_profiles, n_sensor_angles, n_sun_angles, n_NLTE_Channels), &
            y_true(n_profiles, n_sensor_angles, n_sun_angles, n_NLTE_Channels), &
            Tb_est(n_profiles, n_sensor_angles, n_sun_angles, n_NLTE_Channels), &
            Tb_true(n_profiles, n_sensor_angles, n_sun_angles, n_NLTE_Channels), &
            Tb_lte(n_profiles, n_sensor_angles, n_NLTE_Channels), &
            STAT = Allocate_Status )
  IF (Allocate_Status /= 0) THEN
    WRITE(Message,'("Error allocating the x and y arrays. STAT=",i0)') Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          FAILURE )
    STOP
  END IF

  ! Compute mean temperatures, used as predictors
  iset = 1
  DO iprof = 1, n_profiles
    IF( Pt(1, 1) < TrainSet(iset)%Level_Pressure(0, iprof) .OR. &
        Pt(2, N_Play) > TrainSet(iset)%Level_Pressure(TrainSet(iset)%n_Layers, iprof) )THEN
       CALL Display_Message( PROGRAM_NAME, &   
              "The pressure range to compute mean temperature is out of the user profile range", &  
              FAILURE )         
       STOP
    END IF
    DO i = 1, N_PLay
      ! find the level just above Pt(1, i)
      k_Loop1: DO k = 1, TrainSet(iset)%n_Layers
        IF( TrainSet(iset)%Level_Pressure(k, iprof) > Pt(1, i) )THEN
          k1 = k-1
          EXIT k_Loop1
        END IF
      END DO k_Loop1
      ! find the level just below Pt(2, i) 
      k_Loop2: DO k = k1, TrainSet(iset)%n_Layers
        IF( TrainSet(iset)%Level_Pressure(k, iprof) > Pt(2, i) )THEN
          k2 = k
          EXIT k_Loop2
        END IF
      END DO k_Loop2
      IF( k2 <= k1 )THEN
         CALL Display_Message( PROGRAM_NAME, &   
              "Error in computing mean temperature", &  
              FAILURE )         
         STOP
      END IF
      ! temepratuere averaging
      Tm(i, iprof) = ZERO
      DO k = k1+1, k2 
        Tm(i, iprof) = Tm(i, iprof) + (TrainSet(iset)%Level_Temperature(k-1, iprof) + &
                TrainSet(iset)%Level_Temperature(k, iprof))/TWO
      END DO
      Tm(i, iprof) = Tm(i, iprof) / REAL(k2-k1, fp)
    END DO
    
  END DO
  
  DO i = 1, N_PLay
    Tm_lim(1, i)  = MINVAL(Tm(i, :))
    Tm_lim(2, i)  = MAXVAL(Tm(i, :))
    Tm_lim(3, i) = SUM(Tm(i, :))/REAL(n_profiles, fp)
  END DO
  
  n_points = n_profiles*n_sensor_angles*n_sun_angles
    
  ! Do regression
  i_NLTE_Channel = 0          
  Set_Loop2:  DO iset = 1, n_sets
    
    Channel_Loop2: DO ich = 1, TrainSet(iset)%n_channels 
 
      idx = Find_Index( TrainSet(iset)%Channel(ich), NLTE_Channel )        
      IF(idx < 1)CYCLE Channel_Loop2
       
      i_NLTE_Channel = i_NLTE_Channel + 1
      
      Sun_Angle_Loop: DO isun = 1, n_sun_angles
        Sensor_Angle_Loop: DO isen = 1, n_sensor_angles
           
          DO iprof = 1, n_profiles
            y(iprof) = TrainSet(iset)%Radiance_nlte(ich, isen, isun, iprof) - &
                       TrainSet(iset)%Radiance_lte(ich, isen, iprof)
            x(iprof, 1) = ONE
            x(iprof, 2) = Tm(1, iprof)
            x(iprof, 3) = Tm(2, iprof)
            
          END DO

          ! If the NLTE and LTE difference (in %) is less than the threshold, the channel is assumed non-NLTE channel
          IF(MAXVAL(ABS(y/TrainSet(iset)%Radiance_lte(ich, isen, :))) > 0.001_fp)THEN
           
            CALL Compute_AutoCross_Matrix(x, y, xx, xy)

            Error_Status = Compute_RegressionCoeff(xx, xy, cc)
            IF(Error_Status /= 0)THEN
             
              CALL Display_Message( PROGRAM_NAME, &                                                   
                                    "Cannot obtain the solution for this set of predictors,"//&
                                    " so exit", &                                           
                                    FAILURE )                                                         
              STOP                                                                               
            END IF 
          ELSE
            cc = ZERO
          END IF                                                                                      
          nlte%C(:, isen, isun, i_NLTE_Channel) = cc

          ! estimated y
          DO iprof = 1, n_profiles

            y_est(iprof, isen, isun, i_NLTE_Channel) = SUM(cc*x(iprof, :))
            CALL CRTM_Planck_Temperature(1, SC_Channel_Index(i_NLTE_Channel), &
                     TrainSet(iset)%Radiance_lte(ich, isen, iprof)+y_est(iprof, isen, isun, i_NLTE_Channel),&
                     Tb_est(iprof, isen, isun, i_NLTE_Channel))
                     
            y_true(iprof, isen, isun, i_NLTE_Channel) = y(iprof)
            CALL CRTM_Planck_Temperature(1, SC_Channel_Index(i_NLTE_Channel), &
                     TrainSet(iset)%Radiance_nlte(ich, isen, isun, iprof), &
                     Tb_true(iprof, isen, isun, i_NLTE_Channel))

            CALL CRTM_Planck_Temperature(1, SC_Channel_Index(i_NLTE_Channel), &
                     TrainSet(iset)%Radiance_lte(ich, isen, iprof), &
                     Tb_lte(iprof, isen, i_NLTE_Channel))
          END DO
          
        END DO Sensor_Angle_Loop
      END DO Sun_Angle_Loop  
    END DO Channel_Loop2
  END DO Set_Loop2
  

  open(fid_radiance, file=TRIM(Sensor_id(1))//'.radiance.txt', status='replace')
  WRITE( fid_radiance, '(i5)') n_profiles
  do iprof = 1, n_profiles
  do isun =1, n_sun_angles
  do isen = 1, n_sensor_angles
    WRITE( fid_radiance, '(i5)') n_NLTE_Channels
  do i = 1, n_NLTE_Channels
     WRITE( fid_radiance, '(i5, 4(2x, es13.6))') SC(1)%Sensor_Channel(SC_Channel_Index(i)), y_est(iprof, isen, isun, i), &
       y_true(iprof, isen, isun, i), Tb_true(iprof, isen, isun, i), Tb_lte(iprof, isen, i)
  enddo
  enddo
  enddo
  enddo
  close(fid_radiance)
  nlte%Sensor_Channel   = SC(1)%Sensor_Channel
  nlte%Sensor_Id        = Sensor_id(1)
  nlte%WMO_Satellite_ID = SC(1)%WMO_Satellite_ID
  nlte%WMO_Sensor_ID    = SC(1)%WMO_Satellite_ID
  nlte%Secant_Sensor_Zenith    = ONE/COS(TrainSet(1)%Sensor_Angle*D2R)
  DO isun = 1, n_sun_angles
    IF( TrainSet(1)%Sun_Angle(isun) < 89.9_fp )THEN
      nlte%Secant_Solar_Zenith(isun)     = ONE/COS(TrainSet(1)%Sun_Angle(isun)*D2R)
    ELSE
      nlte%Secant_Solar_Zenith(isun)     = 573.0_fp
    END IF
  END DO

  nlte%Min_Tm   = Tm_lim(1,:)
  nlte%Max_Tm   = Tm_lim(2,:)
  nlte%Mean_Tm  = Tm_lim(3,:)
  
  nlte%Upper_Plevel = Pt(:, 1)
  nlte%Lower_Plevel = Pt(:, 2)
         
  Coeff_fname = TRIM(Sensor_id(1))//'.NLTECoeff.nc'
  Error_Status = NLTECoeff_netCDF_WriteFile(Coeff_fname, &
                                   nlte )
  IF (Error_Status /= SUCCESS) THEN
    WRITE(Message,'("Error writing the NLTE structure to the file:", a)') TRIM(Coeff_fname)
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          FAILURE )
    STOP
  END IF
                                 
  FitErr_fname =  TRIM(Sensor_id(1))//'.NLTE.error.txt'
  OPEN(fid_FitErr, FILE=FitErr_fname, STATUS='REPLACE')
  WRITE(fid_FitErr, '(a)')'Channel_Index, Channel_Wavenumber, Mean_Difference, RMS_Difference, '//&
                'RMS_Difference(Secant_Sensor_Zenith=0, Secant_Solar_Zenith=0), MAX_Difference'
  
  dd = REAL(n_points, fp)
  DO ich = 1, n_NLTE_Channels
    meandiff = SUM( Tb_est(:,:,:, ich) - Tb_true(:,:,:, ich) ) / dd
    rmsdiff  = SQRT(SUM( (Tb_est(:,:,:, ich) - Tb_true(:,:,:, ich))**2 ) / dd)
    rmsdiff0 = SQRT(SUM( (Tb_est(:,1,1, ich) - Tb_true(:,1,1, ich))**2 ) / REAL(n_Profiles, fp))
    maxdiff  = MAXVAL( ABS(Tb_est(:,:,:, ich) - Tb_true(:,:,:, ich)) )
    WRITE(fid_FitErr, '(i6, f9.3, 4f9.3)')NLTE_Channel(ich), NLTE_Channel_Wavenumber(ich), meandiff, rmsdiff, rmsdiff0, maxdiff
  END DO
  CLOSE(fid_FitErr)

CONTAINS

  FUNCTION Find_Index(m, Index_Array)RESULT(idx)
    INTEGER,             INTENT(IN)  :: m
    INTEGER,             INTENT(IN)  :: Index_Array(:) 
    INTEGER :: idx
    ! local
    INTEGER :: i
      ! find an index match in the SC Sensor Channel array           
      idx = -1                                                       
      Loop: DO i = 1, SIZE(Index_Array)           
        IF( m == Index_Array(i) ) THEN 
          idx = i                                                    
          EXIT Loop                                       
        END IF                                                       
      END DO Loop                                         
  
  END FUNCTION Find_Index    
END PROGRAM Compute_NLTE_Coeff
