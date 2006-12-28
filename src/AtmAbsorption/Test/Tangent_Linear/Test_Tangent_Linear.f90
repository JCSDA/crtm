!
!  Program to test the CRTM AtmAbsorption Tangent-Linear code.
!
!
! CREATION HISTORY
!       Written by:     Paul van Delst, CIMSS/SSEC 21-Jan-2005
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Test_Tangent_Linear


  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, &
                                   Display_Message, Program_Message
  USE Compare_Float_Numbers, ONLY: Compare_Float
  USE CRTM_Parameters
  USE CRTM_LifeCycle       , ONLY: CRTM_Init, CRTM_Destroy
  USE CRTM_GeometryInfo_Define
  USE CRTM_ChannelInfo_Define
  USE CRTM_Atmosphere_Define
  USE CRTM_Atmosphere_Binary_IO
  USE CRTM_Predictor
  USE CRTM_AtmAbsorption
  USE ComponentTest_netCDF_IO
  ! Disable all implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER( * ), PARAMETER :: PROGRAM_NAME   = 'Test_Tangent_Linear'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: Test_Tangent_Linear.f90,v 1.14 2006/11/22 15:55:40 dgroff Exp $'

  INTEGER, PARAMETER :: SL = 512

  ! Input data file and dimensions
  CHARACTER(*), PARAMETER :: ATMOSPHERE_FILENAME = 'ECMWF-Atmosphere.Cloud.Aerosol.bin'
  INTEGER,      PARAMETER :: N_PROFILES = 52
  INTEGER,      PARAMETER :: N_PERTURBATIONS = 31
  INTEGER,      PARAMETER :: ZERO_PERT = 16
  
  ! The iteration limits
  REAL(fp), PARAMETER :: MAX_DELTAX     = 0.100_fp
  INTEGER,  PARAMETER :: MAX_DELTA_ITER = 15

  ! The floating point precision comparison multiplier
  INTEGER, PARAMETER :: ULP = 900000000
  
  ! Input variables to perturb
  INTEGER, PARAMETER :: N_INPUT_VARIABLES = 4
  INTEGER, PARAMETER :: NIV_P = 1  ! Layer pressure   
  INTEGER, PARAMETER :: NIV_T = 2  ! Layer temperature
  INTEGER, PARAMETER :: NIV_W = 3  ! Layer water vapor
  INTEGER, PARAMETER :: NIV_O = 4  ! Layer ozone      
  CHARACTER( * ), PARAMETER, DIMENSION( N_INPUT_VARIABLES ) :: &
    INPUT_VARIABLE_NAME = (/ 'Layer pressure   ', &
                             'Layer temperature', &
                             'Layer water vapor', &
                             'Layer ozone      ' /)
  CHARACTER( * ), PARAMETER, DIMENSION( N_INPUT_VARIABLES ) :: &
    INPUT_VARIABLE_UNITS = (/ 'hectoPascal', &
                              'Kelvin     ', &
                              'g/kg       ', &
                              'ppmv       ' /)

  ! Output variables to inspect
  INTEGER, PARAMETER :: N_OUTPUT_VARIABLES = 1
  INTEGER, PARAMETER :: NOV_TAU = 1  ! Optical depth
  CHARACTER( * ), PARAMETER, DIMENSION( N_OUTPUT_VARIABLES ) :: &
    OUTPUT_VARIABLE_NAME = (/ 'Optical depth' /)
  CHARACTER( * ), PARAMETER, DIMENSION( N_OUTPUT_VARIABLES ) :: &
    OUTPUT_VARIABLE_UNITS = (/ 'Unitless' /)

       
  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message

  INTEGER :: Error_Status
  INTEGER :: Error_Status_TL
  INTEGER :: Error_Status_NL
  INTEGER :: Error_Status_Neg_NL
  INTEGER :: Error_Status_Neg_TL
  INTEGER :: Error_Status_Pos_TL
  INTEGER :: Error_Status_Pos_NL

  CHARACTER(SL) :: SensorId
  INTEGER       :: SensorIdx, ChannelIdx
  CHARACTER(SL) :: ComponentTest_File
  INTEGER :: New_File

  INTEGER, DIMENSION( 1 ) :: Idx
  INTEGER :: H2O_Idx
  INTEGER :: O3_Idx

  INTEGER :: j, k, l, m, n, nP, nIV, switch, pL, pS, &
             pLcounter, counter
  
  ! Local Variables
  REAL(fp) :: DeltaX  
  REAL(fp) :: perturbationUsed 
  REAL(fp) :: Positive_TL
  REAL(fp) :: Negative_TL
  REAL(fp) :: Positive_NL
  REAL(fp) :: Negative_NL
  REAL(fp) :: Linear_Test
  REAL(fp) :: switchMultiplier
  REAL(fp) :: Fill_multiplier
  
  REAL(fp) :: Perturbation_Factor
  REAL(fp), DIMENSION(N_PERTURBATIONS) :: perturbationFraction

  LOGICAL :: NLTL_PosGradientEqual
  LOGICAL :: NLTL_NegGradientEqual
  LOGICAL :: TL_GradientEqual
  LOGICAL :: FatalFailure
  
  TYPE(CRTM_ChannelInfo_type), DIMENSION(1) :: ChannelInfo
  TYPE(CRTM_Predictor_type)     :: Predictor, Predictor_NL, Predictor_TL
  TYPE(CRTM_GeometryInfo_type)  :: GeometryInfo
  TYPE(CRTM_AtmAbsorption_type) :: AtmAbsorption_Baseline, &
                                   AtmAbsorption_NL, AtmAbsorption_TL, &
                                   AtmAbsorption_Pos_NL, AtmAbsorption_Pos_TL, &
                                   AtmAbsorption_Neg_NL, AtmAbsorption_Neg_TL
  TYPE(CRTM_Atmosphere_type) :: Atmosphere(N_PROFILES), &
                                Atmosphere_NL, Atmosphere_TL
  TYPE(ComponentTest_type) :: ComponentTest
  TYPE(CRTM_APVariables_type) :: APV
  TYPE(CRTM_AAVariables_type) :: AAV_Baseline, AAV_NL
  

  ! Output program header
  CALL Program_Message(PROGRAM_NAME,&
                       'Program to test the CRTM AtmAbsorption Tangent-Linear '//&
                       'components with respect to the Forward components.', &
                       '$Revision: 1.14 $')


  ! Read the atmosphere data file
  WRITE( *, '( /5x, "Reading the Atmosphere structure file..." )' )
  Error_Status = CRTM_Read_Atmosphere_Binary( ATMOSPHERE_FILENAME, &
                                              Atmosphere )
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error reading Atmosphere structure file '//&
                           ATMOSPHERE_FILENAME, & 
                           Error_Status )
   STOP
  END IF


  ! Assign dummy values to the GeometryInfo structure
  GeometryInfo%Sensor_Scan_Angle   = ZERO   ! Nadir
  GeometryInfo%Sensor_Zenith_Angle = ZERO   ! Nadir
  Error_Status = CRTM_Compute_GeometryInfo( GeometryInfo )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error computing GeometryInfo values.', & 
                          Error_Status )
    STOP
  END IF


  ! Enter the sensor Id
  WRITE( *, FMT     = '( /5x, "Enter the sensorId [e.g.hirs3_n16] : " )', &
            ADVANCE = 'NO' )
  READ(*,'(a)') SensorID
  SensorId = ADJUSTL(SensorId)
  SensorIdx = 1  ! This is always 1 !


  ! Initialise the CRTM for that sensor
  WRITE( *, '( /5x, "Initializing the CRTM..." )' )
  Error_Status = CRTM_Init( ChannelInfo, &
                            SensorId=(/SensorId/) )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error initializing CRTM', & 
                           Error_Status)  
    STOP
  END IF


  ! Allocate the ComponentTest structure
  Error_Status = Allocate_ComponentTest( Atmosphere(1)%n_Layers, &
                                         ChannelInfo(SensorIdx)%n_Channels, &
                                         N_PERTURBATIONS, &
                                         N_INPUT_VARIABLES, &
                                         N_OUTPUT_VARIABLES, &
                                         ComponentTest )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error occurred allocating ComponentTest', &
                           Error_Status )                           
    STOP
  END IF


  ! Initialise the perturbation fraction array
  perturbationFraction = ZERO


  ! Assign data to ComponentTest
  ComponentTest%TestType = COMPONENTTEST_FWDTL_TESTTYPE
  ComponentTest%DataType = COMPONENTTEST_POLY_DATATYPE
  ComponentTest%Pressure = Atmosphere(1)%Pressure
  ComponentTest%Spectral = ChannelInfo(SensorIdx)%Sensor_Channel
  ComponentTest%Input_Variable_Name  = INPUT_VARIABLE_NAME
  ComponentTest%Input_Variable_Units = INPUT_VARIABLE_UNITS
  ComponentTest%Output_Variable_Name  = OUTPUT_VARIABLE_NAME
  ComponentTest%Output_Variable_Units = OUTPUT_VARIABLE_UNITS


  ! ------------------------------
  ! Loop over Atmospheric Profiles
  ! ------------------------------
  Profile_Loop: DO m = 1, N_PROFILES

    WRITE( *, '( 5x, "Processing profile # ", i3 )' ) m


    ! Determine the absorber indices
    H2O_Idx = Get_AbsorberIdx(Atmosphere(m), H2O_ID)
    O3_Idx  = Get_AbsorberIdx(Atmosphere(m), O3_ID )
    IF ( H2O_Idx < 0 .OR. O3_Idx < 0 ) THEN
      WRITE(Message,'("Water vapour and/or ozone data not present in profile #",i0)') m
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            FAILURE )
      STOP
    END IF


    ! Allocate Predictor Structures
    Error_Status    = CRTM_Allocate_Predictor( Atmosphere(m)%n_Layers, &  ! Input
                                               MAX_N_PREDICTORS      , &  ! Input
                                               MAX_N_ABSORBERS       , &  ! Input 
                                               Predictor               )  ! Output
    Error_Status_TL = CRTM_Allocate_Predictor( Atmosphere(m)%n_Layers, &  ! Input
                                               MAX_N_PREDICTORS      , &  ! Input
                                               MAX_N_ABSORBERS       , &  ! Input
                                               Predictor_TL            )  ! Output
    Error_Status_NL = CRTM_Allocate_Predictor( Atmosphere(m)%n_Layers, &  ! Input
                                               MAX_N_PREDICTORS      , &  ! Input
                                               MAX_N_ABSORBERS       , &  ! Input
                                               Predictor_NL            )  ! Output
    IF ( Error_Status    /= SUCCESS .OR. &
         Error_Status_TL /= SUCCESS .OR. &
         Error_Status_NL /= SUCCESS      ) THEN
      WRITE(Message,'("Error occurred allocating predictor structures in profile #",i0)') m
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            Error_Status )
      STOP
    END IF  

    ! Allocate AtmAbsorption structures
    ! The baseline structure
    Error_Status = CRTM_Allocate_AtmAbsorption( Atmosphere(m)%n_Layers, &  ! Input
                                                AtmAbsorption_Baseline  )  ! Input
    ! The tangent-linear structure
    Error_Status_TL = CRTM_Allocate_AtmAbsorption( Atmosphere(m)%n_Layers, &  ! Input
                                                   AtmAbsorption_TL        )  ! Output
    ! The non-linear structure
    Error_Status_NL = CRTM_Allocate_AtmAbsorption( Atmosphere(m)%n_Layers, &  ! Input
                                                   AtmAbsorption_NL        )  ! Output

    ! The positive non-linear structure used for storing AtmAbsorption_NL
    ! depending on the state of the switch variable
    Error_Status_Neg_TL = CRTM_Allocate_AtmAbsorption( Atmosphere(m)%n_Layers, &  ! Input
                                                       AtmAbsorption_Neg_TL    )  ! Output

    ! The negative non-linear structure used for storing AtmAbsorption_NL
    ! depending on the state of the switch variable
    Error_Status_Neg_NL = CRTM_Allocate_AtmAbsorption( Atmosphere(m)%n_Layers, &  ! Input
                                                       AtmAbsorption_Neg_NL    )  ! Output

    ! The positive tangent-linear structure used for storing AtmAbsorption_TL
    ! depending on the state of the switch variable
    Error_Status_Pos_TL = CRTM_Allocate_AtmAbsorption( Atmosphere(m)%n_Layers, &  ! Input
                                                       AtmAbsorption_Pos_TL    )  ! Output

    ! The negative tangent-linear structure used for storing AtmAbsorption
    ! depending on the state of the switch variable
    Error_Status_Pos_NL = CRTM_Allocate_AtmAbsorption( Atmosphere(m)%n_Layers, &  ! Input
                                                       AtmAbsorption_Pos_NL    )  ! Output
    ! Test results
    IF ( Error_Status    /= SUCCESS     .OR. &
         Error_Status_TL /= SUCCESS     .OR. &
         Error_Status_NL /= SUCCESS     .OR. &
         Error_Status_Pos_NL /= SUCCESS .OR. &
         Error_Status_Neg_NL /= SUCCESS .OR. &
         Error_Status_Pos_TL /= SUCCESS .OR. &
         Error_Status_Neg_TL /= SUCCESS      ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error occurred allocating AtmAbsorption structures', &
                             Error_Status )                           
      STOP
    END IF


    ! Copy data into the TL atmosphere structure
    ! (to get the required dimensions) and zero
    Error_Status = CRTM_Assign_Atmosphere( Atmosphere(m), &  ! Input
                                           Atmosphere_TL  )  ! Output
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error copying Atmosphere input data ", &
                        &"into TL structure for profile #", i2 )' ) m
      CALL DIsplay_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF
    CALL CRTM_Zero_Atmosphere( Atmosphere_TL )


    ! Initialise logical flags
    FatalFailure=.FALSE.


    ! Calculate gas absorption model predictors for baseline calculation
    CALL CRTM_Compute_Predictors( Atmosphere(m), &  ! Input
                                  GeometryInfo , &  ! Input
                                  Predictor    , &  ! Output
                                  APV            )  ! Output   


    ! -------------------------
    ! Loop over sensor channels
    ! -------------------------
    Channel_Loop: DO l = 1, ChannelInfo(SensorIdx)%n_Channels

      WRITE( *, '( 5x, "Channel: ", i5 )' ) ChannelInfo(SensorIdx)%Sensor_Channel(l)

      ! Short(er) name for channel index
      ChannelIdx = ChannelInfo(SensorIdx)%Channel_Index(l)

      ! Compute the baseline layer optical
      ! depths due to gaseous absorption
      CALL CRTM_Compute_AtmAbsorption( SensorIdx             , & ! Input
                                       ChannelIdx            , &  ! Input
                                       Predictor             , &  ! Input
                                       AtmAbsorption_Baseline, &  ! Output
                                       AAV_Baseline            )  ! Internal variable output


      ! -------------------------
      ! Loop Over Input Variables
      ! -------------------------
      Input_Variable_Loop: DO nIV = 1, N_INPUT_VARIABLES

        WRITE( *, '( 5x, "Perturbation variable: ", a )' ) INPUT_VARIABLE_NAME(nIV)


        ! -------------------------
        ! Restart continuation mark
        ! -------------------------
        100 CONTINUE
        
        
        ! ----------------
        ! Begin layer loop
        ! ----------------
        Layer_Loop: DO k = 1, Atmosphere(m)%n_Layers

          ! Initialise the perturbation delta    
          DeltaX = MAX_DELTAX
          
          ! Initialize the perturbation index counter
          nP = 0


          ! ----------------------------
          ! Begin open perturbation loop
          ! ----------------------------
          Perturbation_Loop: DO


            ! Increment perturbation index
            nP = nP + 1
          

            ! ---------------------------------------------------
            ! Begin the switch loop for +/- ve AtmAbsorption calc
            ! ---------------------------------------------------
            Switch_Loop: DO switch = -1, 1, 2


              ! Calculate the perturbationUsed by multiplying the switch and current DeltaX
              switchMultiplier=REAL(switch,fp)
              perturbationUsed = switchMultiplier*DeltaX

              ! Fill Perturbation fraction array which will be used to fill the perturbations for componenttest
              IF ( switch == -1 ) THEN
                perturbationFraction(nP) = perturbationUsed
              END IF
              IF ( switch == 1 ) THEN 
                perturbationFraction((N_PERTURBATIONS + 1) - nP) = perturbationUsed
              END IF


              ! Re-initialise all NL and TL data
              ! The forward model input data
              Error_Status = CRTM_Assign_Atmosphere( Atmosphere(m), &  ! Input
                                                     Atmosphere_NL  )  ! Output
              IF ( Error_Status /= SUCCESS ) THEN
                CALL Display_Message( PROGRAM_NAME, &
                                      'Error reinitialising forward model Atmosphere input data', &
                                      Error_Status )
                STOP
              END IF
              ! The tangent-linear model input data
              CALL CRTM_Zero_Atmosphere( Atmosphere_TL )


              ! ------------------
              ! Perturb the inputs
              ! ------------------

              Input_Variable_Select: SELECT CASE ( nIV )

                ! Layer pressure
                CASE ( NIV_P )
                  IF ( k == 1 ) THEN
                    Atmosphere_TL%Pressure(k) = perturbationUsed * &
                                                ( Atmosphere(m)%Pressure(k)-TOA_PRESSURE )
                  ELSE
                    Atmosphere_TL%Pressure(k) = perturbationUsed * &
                                                ( Atmosphere(m)%Pressure(k)-Atmosphere(m)%Pressure(k-1) )
                  END IF
                  Atmosphere_NL%Pressure(k) = Atmosphere_NL%Pressure(k) + & 
                                              Atmosphere_TL%Pressure(k)
                  Linear_Test=Atmosphere_TL%Pressure(k)

                ! Layer temperature
                CASE ( NIV_T )
                  Atmosphere_TL%Temperature(k) = perturbationUsed * Atmosphere(m)%Temperature(k)
                  Atmosphere_NL%Temperature(k) = Atmosphere_NL%Temperature(k) + &
                                                 Atmosphere_TL%Temperature(k)
                  Linear_Test=Atmosphere_TL%Temperature(k)

                ! Layer water vapour
                CASE ( NIV_W )
                  Atmosphere_TL%Absorber(k,H2O_Idx) = perturbationUsed * Atmosphere(m)%Absorber(k,H2O_Idx)
                  Atmosphere_NL%Absorber(k,H2O_Idx) = Atmosphere_NL%Absorber(k,H2O_Idx) + &
                                                      Atmosphere_TL%Absorber(k,H2O_Idx)
                  Linear_Test=Atmosphere_TL%Absorber(k,H2O_Idx)            

                ! Layer ozone
                CASE ( NIV_O )
                  Atmosphere_TL%Absorber(k,O3_Idx) = perturbationUsed * Atmosphere(m)%Absorber(k,O3_Idx)
                  Atmosphere_NL%Absorber(k,O3_Idx) = Atmosphere_NL%Absorber(k,O3_Idx) + &
                                                     Atmosphere_TL%Absorber(k,O3_Idx)
                  Linear_Test=Atmosphere_TL%Absorber(k,O3_Idx)

              END SELECT Input_Variable_Select


              ! Calculate TL/NL gas absorption model predictors
              CALL CRTM_Compute_Predictors( Atmosphere_NL,  &  ! Input
                                            GeometryInfo,   &  ! Input
                                            Predictor_NL,   &  ! Output
                                            APV             )  ! Output      
              CALL CRTM_Compute_Predictors_TL( Atmosphere(m),     &   ! FWD Input
                                               Predictor,         &   ! FWD Input
                                               Atmosphere_TL,     &   ! TL Input
                                               GeometryInfo,      &   ! Input
                                               Predictor_TL,      &   ! TL Output
                                               APV                )   ! Internal variable input

              

              

              ! Calculate TL/NL AtmAbsorption
              CALL CRTM_Compute_AtmAbsorption( SensorIdx       , &  ! Input
                                               ChannelIdx      , &  ! Input
                                               Predictor_NL    , &  ! Input
                                               AtmAbsorption_NL, &  ! Output
                                               AAV_NL            )  ! Internal variable output
              CALL CRTM_Compute_AtmAbsorption_TL( SensorIdx       , &  ! Input
                                                  ChannelIdx      , &  ! Input
                                                  Predictor       , &  ! Input
                                                  Predictor_TL    , &  ! Input
                                                  AtmAbsorption_TL, &  ! Output
                                                  AAV_Baseline      )  ! Internal variable input
              
              ! Save results
              IF (switch==-1) THEN
                ! Negative perturbation results
                Error_Status_NL = CRTM_Assign_AtmAbsorption( AtmAbsorption_NL,    &
                                                             AtmAbsorption_Neg_NL )
                Error_Status_TL = CRTM_Assign_AtmAbsorption( AtmAbsorption_TL,    &
                                                             AtmAbsorption_Neg_TL )
              ELSE 
                ! Positive perturbation results
                Error_Status_NL = CRTM_Assign_AtmAbsorption( AtmAbsorption_NL,    &
                                                             AtmAbsorption_Pos_NL )
                Error_Status_TL = CRTM_Assign_AtmAbsorption( AtmAbsorption_TL,    &
                                                             AtmAbsorption_Pos_TL )
              END IF

              IF ( Error_Status_NL /= SUCCESS .OR. &
                   Error_Status_TL /= SUCCESS) THEN
                 WRITE(Message,'("Error saving results for perturbation:",&
                                &f5.2,"; layer 3",i0)') perturbationUsed, k 
                 CALL Display_Message( PROGRAM_NAME, &
                                       TRIM(Message), &
                                       Error_Status )
                STOP
              END IF

            END DO Switch_Loop  

           
            ! Calculate the gradients and test by comparison
            ! if a fatal failure has not occured
            IF ( .NOT. FatalFailure ) THEN

              Positive_TL = ABS(AtmAbsorption_Pos_TL%Optical_Depth(k)/Linear_Test)
              Negative_TL = ABS(AtmAbsorption_Neg_TL%Optical_Depth(k)/Linear_Test)
              Positive_NL = ABS((AtmAbsorption_Pos_NL%Optical_Depth(k) - AtmAbsorption_Baseline%Optical_Depth(k))/(Linear_Test))
              Negative_NL = ABS((AtmAbsorption_Neg_NL%Optical_Depth(k) - AtmAbsorption_Baseline%Optical_Depth(k))/(Linear_Test))
              
              TL_GradientEqual = Compare_Float( Positive_TL, &  ! Input
                                                Negative_TL, &  ! Input
                                                ULP=ULP      )  ! Input

              NLTL_NegGradientEqual = Compare_Float( Negative_TL,  &  ! Input
                                                     Negative_NL,  &  ! Input
                                                     ULP=ULP       )  ! Input

              NLTL_PosGradientEqual = Compare_Float( Positive_TL,  &  ! Input
                                                     Positive_NL,  &  ! Input
                                                     ULP=ULP       )  ! Input
             
              
              ! Determine if ALL of the tests passed...
              IF ( TL_GradientEqual      .AND. &
                   NLTL_NegGradientEqual .AND. &
                   NLTL_PosGradientEqual       ) THEN
                ! All the tests passed. Go to next layer
                CYCLE Layer_Loop
              ELSE
                ! A test failed. Decrease the perturbation fraction and repeat
                CALL Get_Next_DeltaX(DeltaX)
                ! Has the maximum number of deltaX iterations been reached?
                IF (Max_Iterations(nP)) THEN
                  CALL Display_Message(PROGRAM_NAME,&
                                       'Max iterations reached. Switching to failure mode.', &
                                       INFORMATION)
                  FatalFailure = .TRUE.
                  GOTO 100
                END IF        
              END IF

            ELSE
            
              ! We're in fatal failure mode so output all data
              ComponentTest%d1(k,l,nP,nIV,NOV_TAU) = ((AtmAbsorption_Neg_NL%Optical_Depth(k) - &
                                                      AtmAbsorption_Baseline%Optical_Depth(k)))
              ComponentTest%d2(k,l,nP,nIV,NOV_TAU) = (AtmAbsorption_Neg_TL%Optical_Depth(k))

              ComponentTest%d1(k,l,((N_PERTURBATIONS + 1) - nP),nIV,NOV_TAU) = (AtmAbsorption_Pos_NL%Optical_Depth(k) - &
                                                                                 AtmAbsorption_Baseline%Optical_Depth(k))
              ComponentTest%d2(k,l,((N_PERTURBATIONS + 1) - nP),nIV,NOV_TAU) = AtmAbsorption_Pos_TL%Optical_Depth(k)

              ComponentTest%d1(k,l,ZERO_PERT,nIV,NOV_TAU) = ZERO
              ComponentTest%d2(k,l,ZERO_PERT,nIV,NOV_TAU) = ZERO
              
              ! Decrease the perturbation amount
              CALL Get_Next_DeltaX(DeltaX)

              ! If we've reached the maximum number of iterations, the
              ! we're done. Being the next layer calculations
              IF (Max_Iterations( nP )) CYCLE Layer_Loop

            END IF              

          END DO Perturbation_Loop
        END DO Layer_Loop
      END DO Input_Variable_Loop
    END DO Channel_Loop



    ! ------------------------------------------------------    
    ! Write the current profile's ComponentTest data to file
    ! ------------------------------------------------------    
    WRITE( *, '( 10x, "Writing FWD/TL data to output file...." )' )

    ! Set the current dataset number in the output structure
    ComponentTest%nM = m
    WRITE( ComponentTest%nM_Name, '( "Profile # ", i3 )' ) m

    ! Fill the perturbation field
    ComponentTest%Perturbation = perturbationFraction

    ! Write the data
    ComponentTest_File=TRIM(SensorId)//'.CRTM_AtmAbsorption.ComponentTest.nc'
    Error_Status = Write_ComponentTest_netCDF( TRIM( ComponentTest_File ), &
                                               ComponentTest, &
                                               New_File      = New_File, &
                                               Title         = 'FWD/TL CRTM AtmAbsorption test results for '//&
                                                               TRIM(SensorId), &
                                               History       = PROGRAM_RCS_ID, &
                                               Sensor_Name   = TRIM(SensorId), &
                                               Platform_Name = TRIM(SensorId), &
                                               Comment       = 'Compact-OPTRAN AtmAbsorption algorithm.', &
                                               ID_Tag        = 'ECMWF' )
    IF ( Error_Status /= SUCCESS ) THEN 
      WRITE( Message, '( "Error writing ComponentTest structure for profile #", i5, " to ", a )' ) &
                      m, TRIM( ComponentTest_File )
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )                           
      STOP
    END IF

    ! Reset the new file flag
    IF ( New_File == 1 ) New_File = 0


    ! -----------------------------------------
    ! Deallocate structures for current profile
    ! -----------------------------------------
    ! The AtmAbsorption structures
    Error_Status_NL = CRTM_Destroy_AtmAbsorption( AtmAbsorption_NL )
    Error_Status_TL = CRTM_Destroy_AtmAbsorption( AtmAbsorption_TL )
    Error_Status    = CRTM_Destroy_AtmAbsorption( AtmAbsorption_Baseline )
    Error_Status_Pos_NL = CRTM_Destroy_AtmAbsorption( AtmAbsorption_Pos_NL )
    Error_Status_Pos_TL = CRTM_Destroy_AtmAbsorption( AtmAbsorption_Pos_TL )
    Error_Status_Neg_NL = CRTM_Destroy_AtmAbsorption( AtmAbsorption_Neg_NL )
    Error_Status_Pos_TL = CRTM_Destroy_AtmAbsorption( AtmAbsorption_Neg_TL )
    IF ( Error_Status    /= SUCCESS .OR. &
         Error_Status_TL /= SUCCESS .OR. &
         Error_Status_NL /= SUCCESS .OR. & 
         Error_Status_Pos_NL /= SUCCESS .OR. &   
         Error_Status_Neg_NL /= SUCCESS .OR. &
         Error_Status_Pos_TL /= SUCCESS .OR. &
         Error_Status_Neg_TL /= SUCCESS  ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error deallocating AtmAbsorption structures', &
                            Error_Status )
      STOP
    END IF

    ! The Predictor Structures
    Error_Status = CRTM_Destroy_Predictor( Predictor )  
    Error_Status_TL = CRTM_Destroy_Predictor( Predictor_TL )
    Error_Status_NL = CRTM_Destroy_Predictor( Predictor_NL )
    IF ( Error_Status    /= SUCCESS .OR. &
         Error_Status_TL /= SUCCESS .OR. &
         Error_Status_NL /= SUCCESS     ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error deallocating Predictor structures', &
                            Error_Status  )
      STOP
    END IF

    ! The individual Atmosphere structures
    Error_Status_TL = CRTM_Destroy_Atmosphere( Atmosphere_TL )
    Error_Status_NL = CRTM_Destroy_Atmosphere( Atmosphere_NL )
    IF ( Error_Status_TL /= SUCCESS .OR. &
         Error_Status_NL /= SUCCESS     ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error deallocating Atmosphere TL/NL structures', &
                            Error_Status )
      STOP
    END IF

  END DO Profile_Loop


  ! Destroy the ComponentTest structure
  Error_Status = Destroy_ComponentTest( ComponentTest )
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error occurred destroying ComponentTest for '//&
                           TRIM(SensorId), &
                           Error_Status )                           
   STOP
  END IF

  ! Destroy the CRTM
  WRITE( *, '( /5x, "Destroying the CRTM..." )' )
  Error_Status = CRTM_Destroy( ChannelInfo )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying CRTM', & 
                           Error_Status )
  END IF

  ! Deallocate Atmosphere structure array
  Error_Status = CRTM_Destroy_Atmosphere( Atmosphere )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error deallocating Atmosphere structure array', &
                          Error_Status )
  END IF


CONTAINS

  ! Function to determine the index of the requested absorber
  ! in the atmosphere structure absorber component
  FUNCTION Get_AbsorberIdx(Atm, AbsorberId) RESULT(AbsorberIdx)
    TYPE(CRTM_Atmosphere_type), INTENT(IN) :: Atm
    INTEGER                   , INTENT(IN) :: AbsorberId
    INTEGER :: AbsorberIdx
    INTEGER :: j, Idx(1)
    ! Initialise result to "not found"
    AbsorberIdx = -1
    ! Return if absorber not present
    IF ( COUNT(Atm%Absorber_ID == AbsorberId) /= 1 ) RETURN
    ! Find the location
    Idx = PACK( (/(j,j=1,Atm%n_Absorbers)/), &
                Atm%Absorber_ID==AbsorberId  )
    AbsorberIdx=Idx(1)
  END FUNCTION Get_AbsorberIdx

  ! Purpose: 
  ! To update DeltaX in perturbation loop. 
  ! The perturbation is halved for each iteration
  SUBROUTINE Get_Next_DeltaX ( sDeltaX )
    REAL(fp), INTENT(INOUT) :: sDeltaX
    sDeltaX=POINT_5*sDeltaX
  END SUBROUTINE Get_Next_DeltaX

  ! Purpose:
  ! To determine if the maximum number of 
  ! perturbation iterations allowed has been reached
  FUNCTION Max_Iterations( fnP )
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: fnP
    LOGICAL :: Max_Iterations
    Max_Iterations = (fnP==MAX_DELTA_ITER)
  END FUNCTION Max_Iterations

END PROGRAM Test_Tangent_Linear
