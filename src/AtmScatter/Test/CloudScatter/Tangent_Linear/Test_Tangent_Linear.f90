!
!  Program to test the CRTM CloudScatter Tangent-Linear code.
!
!
!  CREATION HISTORY
!       Written by:     Paul van Delst, CIMSS/SSEC 21-Jan-2005
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Test_Tangent_Linear

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds,                ONLY: fp=>fp_kind
  USE Message_Handler,           ONLY: SUCCESS, WARNING, Display_Message, Program_Message
  USE CRTM_Parameters,           ONLY: ZERO, &
                                       MAX_N_LEGENDRE_TERMS, MAX_N_PHASE_ELEMENTS
  USE CRTM_LifeCycle,            ONLY: CRTM_Init, CRTM_Destroy
  USE CRTM_GeometryInfo_Define,  ONLY: CRTM_GeometryInfo_type, &
                                       CRTM_Compute_GeometryInfo 
  USE CRTM_ChannelInfo_Define,   ONLY: CRTM_ChannelInfo_type
  USE CRTM_Atmosphere_Define,    ONLY: RAIN_CLOUD, SNOW_CLOUD, CLOUD_TYPE_NAME, &
                                       CRTM_Atmosphere_type, &
                                       CRTM_Destroy_Atmosphere, &
                                       CRTM_Assign_Atmosphere, &
                                       CRTM_Zero_Atmosphere
  USE CRTM_Atmosphere_Binary_IO, ONLY: CRTM_Read_Atmosphere_Binary
  USE CRTM_CloudScatter,         ONLY: CRTM_AtmScatter_type, &
                                       CRTM_CSVariables_type, &
                                       CRTM_Allocate_AtmScatter, &
                                       CRTM_Destroy_AtmScatter, &
                                       CRTM_Compute_CloudScatter, &
                                       CRTM_Compute_CloudScatter_TL
  USE ComponentTest_netCDF_IO,   ONLY: COMPONENTTEST_FWDTL_TESTTYPE, &
                                       COMPONENTTEST_POLY_DATATYPE, &
                                       ComponentTest_type, &
                                       Allocate_ComponentTest, &
                                       Destroy_ComponentTest, &
                                       Write_ComponentTest_netCDF
  USE Compare_Float_Numbers
  USE CRTM_AtmScatter_Define
  ! Disable all implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER( * ), PARAMETER :: PROGRAM_NAME   = 'Test_Tangent_Linear'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: Test_Tangent_Linear.f90,v 1.6 2006/06/29 19:48:23 wd20pd Exp $'
  CHARACTER( * ),  PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  INTEGER, PARAMETER :: SL = 512

  CHARACTER(*), PARAMETER :: ATMOSPHERE_FILENAME = 'ECMWF-Atmosphere.Cloud.Aerosol.bin'
  INTEGER,      PARAMETER :: N_PROFILES = 52

  INTEGER, PARAMETER :: N_Perturbations = 31
  REAL(fp), DIMENSION(N_Perturbations) :: PERTURBATION_FRACTION 
  
  INTEGER, PARAMETER :: N_INPUT_VARIABLES = 4
  INTEGER, PARAMETER :: NIV_T  = 1  ! Temperature
  INTEGER, PARAMETER :: NIV_RE = 2  ! Effective radius
  INTEGER, PARAMETER :: NIV_RV = 3  ! Effective variance
  INTEGER, PARAMETER :: NIV_Q  = 4  ! Water content
  CHARACTER( * ), PARAMETER, DIMENSION( N_INPUT_VARIABLES ) :: &
    INPUT_VARIABLE_NAME = (/ 'Temperature       ', &
                             'Effective radius  ', &
                             'Effective variance', &
                             'Water content     ' /)
  CHARACTER( * ), PARAMETER, DIMENSION( N_INPUT_VARIABLES ) :: &
    INPUT_VARIABLE_UNITS = (/ 'Kelvin ', &
                              'microns', &
                              'microns', &
                              'g/cm^2 ' /)
  INTEGER, PARAMETER :: N_OUTPUT_VARIABLES = 11
  INTEGER, PARAMETER :: NOV_TAU         = 1  ! Optical depth
  INTEGER, PARAMETER :: NOV_OMEGA       = 2  ! Single scatter albedo
  INTEGER, PARAMETER :: NOV_G           = 3  ! Asymmetry factor
  INTEGER, PARAMETER :: NOV_D           = 4  ! Delta truncation factor
  INTEGER, PARAMETER :: NOV_PC_L0       = 5  ! Phase coefficient, Legendre term #0
  INTEGER, PARAMETER :: NOV_PC_L1       = 6  ! Phase coefficient, Legendre term #1
  INTEGER, PARAMETER :: NOV_PC_L2       = 7  ! Phase coefficient, Legendre term #2
  INTEGER, PARAMETER :: NOV_PC_L3       = 8  ! Phase coefficient, Legendre term #3
  INTEGER, PARAMETER :: NOV_PC_L4       = 9  ! Phase coefficient, Legendre term #4
  INTEGER, PARAMETER :: NOV_PC_L5       =10  ! Phase coefficient, Legendre term #5
  INTEGER, PARAMETER :: NOV_PC_L6       =11  ! Phase coefficient, Legendre term #6
  CHARACTER( * ), PARAMETER, DIMENSION( N_OUTPUT_VARIABLES ) :: &
    OUTPUT_VARIABLE_NAME = (/ 'Optical depth          ', &
                              'Single scatter albedo  ', &
                              'Asymmetry factor       ', &
                              'Delta truncation factor', &
                              'Phase coefficient L0   ', &
                              'Phase coefficient L1   ', &
                              'Phase coefficient L2   ', &
                              'Phase coefficient L3   ', &
                              'Phase coefficient L4   ', &
                              'Phase coefficient L5   ', &
                              'Phase coefficient L6   ' /)
  CHARACTER( * ), PARAMETER, DIMENSION( N_OUTPUT_VARIABLES ) :: &
    OUTPUT_VARIABLE_UNITS = (/ 'Unitless', &  ! Optical depth
                               'Unitless', &  ! Single scatter albedo
                               'Unitless', &  ! Asymmetry factor
                               'Unitless', &  ! Delta truncation factor
                               'Unitless', &  ! Phase coefficient, Legendre term #0
                               'Unitless', &  ! Phase coefficient, Legendre term #1
                               'Unitless', &  ! Phase coefficient, Legendre term #2
                               'Unitless', &  ! Phase coefficient, Legendre term #3
                               'Unitless', &  ! Phase coefficient, Legendre term #4
                               'Unitless', &  ! Phase coefficient, Legendre term #5
                               'Unitless' /)  ! Phase coefficient, Legendre term #6

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

  CHARACTER(SL) :: File_Prefix
  CHARACTER(SL) :: SpcCoeff_File
  CHARACTER(SL) :: TauCoeff_File
  CHARACTER(SL) :: AerosolCoeff_File
  CHARACTER(SL) :: CloudCoeff_File
  CHARACTER(SL) :: EmisCoeff_File
  CHARACTER(SL) :: ComponentTest_File
  INTEGER :: New_File
  INTEGER :: k, l, m, n, nP, nIV, nOV, nm, mc, mcType, nLT, switch
  

  ! ---------------
  ! Local Variables
  ! ---------------
  REAL(fp) :: DeltaX
  REAL(fp) :: Perturbation_Used
  REAL(fp) :: switchmultiplier
  REAL(fp) :: Fill_multiplier
  REAL(fp) :: Linear_Test
  
  REAL(fp) :: Positive_OD_TL
  REAL(fp) :: Negative_OD_TL
  REAL(fp) :: Positive_OD_NL
  REAL(fp) :: Negative_OD_NL
  
  REAL(fp) :: Positive_SSA_TL
  REAL(fp) :: Negative_SSA_TL
  REAL(fp) :: Positive_SSA_NL
  REAL(fp) :: Negative_SSA_NL
  
  REAL(fp) :: Positive_AF_TL
  REAL(fp) :: Negative_AF_TL
  REAL(fp) :: Positive_AF_NL
  REAL(fp) :: Negative_AF_NL
  
  REAL(fp) :: Positive_DT_TL
  REAL(fp) :: Negative_DT_TL
  REAL(fp) :: Positive_DT_NL
  REAL(fp) :: Negative_DT_NL
  
  REAL(fp), DIMENSION( MAX_N_LEGENDRE_TERMS ) :: Positive_PC_TL
  REAL(fp), DIMENSION( MAX_N_LEGENDRE_TERMS ) :: Negative_PC_TL
  REAL(fp), DIMENSION( MAX_N_LEGENDRE_TERMS ) :: Positive_PC_NL
  REAL(fp), DIMENSION( MAX_N_LEGENDRE_TERMS ) :: Negative_PC_NL
  
  INTEGER, PARAMETER :: ULP = 900000000
  INTEGER, PARAMETER :: Max_Delta_Iter = 15

  INTEGER, PARAMETER :: Zero_Pert = 16
  REAL(fp), PARAMETER :: Max_DeltaX=0.100_fp
  
  ! Variables used to hold results for individual tests
  LOGICAL :: NLTL_OD_PosGradientEqual
  LOGICAL :: NLTL_OD_NegGradientEqual
  LOGICAL :: TL_OD_GradientEqual
  
  LOGICAL :: NLTL_SSA_PosGradientEqual
  LOGICAL :: NLTL_SSA_NegGradientEqual
  LOGICAL :: TL_SSA_GradientEqual

  LOGICAL :: NLTL_AF_PosGradientEqual
  LOGICAL :: NLTL_AF_NegGradientEqual
  LOGICAL :: TL_AF_GradientEqual

  LOGICAL :: NLTL_DT_PosGradientEqual
  LOGICAL :: NLTL_DT_NegGradientEqual
  LOGICAL :: TL_DT_GradientEqual

  LOGICAL :: NLTL_PC_PosGradientEqual
  LOGICAL :: NLTL_PC_NegGradientEqual
  LOGICAL :: TL_PC_GradientEqual

  ! Variables used to determine definitive pass/fail
  LOGICAL :: AF_Test
  LOGICAL :: SSA_Test
  LOGICAL :: DT_Test
  LOGICAL :: OD_Test
  LOGICAL :: PC_Test

  LOGICAL :: FatalFailure
  
    
           
  TYPE(CRTM_ChannelInfo_type)  :: ChannelInfo
  TYPE(CRTM_GeometryInfo_type) :: GeometryInfo
  TYPE(CRTM_AtmScatter_type)   :: CloudScatter_Baseline, &
                                  CloudScatter_NL, CloudScatter_TL, &
                                  CloudScatter_Pos_NL, CloudScatter_Neg_NL, &
                                  CloudScatter_Pos_TL, CloudScatter_Neg_TL
  TYPE(CRTM_Atmosphere_type), DIMENSION(N_PROFILES) :: Atmosphere
  TYPE(CRTM_Atmosphere_type)                        :: Atmosphere_NL, Atmosphere_TL
  TYPE(CRTM_CSVariables_type) :: CSV, CSV_Dummy
  TYPE(ComponentTest_type) :: ComponentTest

  ! Output descriptive header
  CALL Program_Message(PROGRAM_NAME, &
                       'Program to test the CRTM CloudScatter Tangent-Linear '//&
                       'components with respect to the Forward components.', &
                       '$Revision: 1.6 $' )


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

  ! Fix the cloud water content units.
  DO m = 1, N_PROFILES
    IF ( Atmosphere(m)%n_Clouds > 0 ) THEN
      DO n = 1, Atmosphere(m)%n_Clouds
        Atmosphere(m)%Cloud(n)%Water_Content = Atmosphere(m)%Cloud(n)%Water_Content/1000.0_fp
      END DO
    END IF
  END DO

  ! Assign dummy values to the geometryinfo structure
  GeometryInfo%Sensor_Scan_Angle   = ZERO   ! Nadir
  GeometryInfo%Sensor_Zenith_Angle = ZERO   ! Nadir
  Error_Status = CRTM_Compute_GeometryInfo( GeometryInfo )
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error computing GeometryInfo values.', & 
                           Error_Status )
   STOP
  END IF

  ! Enter the instrument file prefix, e.g. hirs3_n16
  WRITE( *, FMT     = '( /5x, "Enter the instrument file prefix : " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) File_Prefix
  File_Prefix = ADJUSTL( File_Prefix )

  ! Create the filenames
  SpcCoeff_File      = TRIM( File_Prefix )//'.SpcCoeff.bin'
  TauCoeff_File      = TRIM( File_Prefix )//'.TauCoeff.bin'
  CloudCoeff_File    = 'CloudCoeff.bin'
  AerosolCoeff_File  = 'AerosolCoeff.bin'
  EmisCoeff_File     = 'EmisCoeff.bin'
  ComponentTest_File = TRIM( File_Prefix )//'.CRTM_CloudScatter.ComponentTest.nc'
  New_File = 1

  ! Initialise the CRTM
  WRITE( *, '( /5x, "Initializing the CRTM..." )' )
  Error_Status = CRTM_Init( ChannelInfo, &
                            SpcCoeff_File     = SpcCoeff_File, &
                            TauCoeff_File     = TauCoeff_File, &
                            CloudCoeff_File   = CloudCoeff_File, &
                            AerosolCoeff_File = AerosolCoeff_File, &
                            EmisCoeff_File    = EmisCoeff_File )
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error initializing CRTM', & 
                            Error_Status)  
   STOP
  END IF

  ! Allocate the ComponentTest structure
  Error_Status = Allocate_ComponentTest( Atmosphere(1)%n_Layers, &
                                         ChannelInfo%n_Channels, &
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

  ! Assign data to ComponentTest
  ComponentTest%TestType              = COMPONENTTEST_FWDTL_TESTTYPE
  ComponentTest%DataType              = COMPONENTTEST_POLY_DATATYPE
  ComponentTest%Pressure              = Atmosphere(1)%Pressure
  ComponentTest%Spectral              = ChannelInfo%Sensor_Channel
  ComponentTest%Perturbation          = PERTURBATION_FRACTION
  ComponentTest%Input_Variable_Name   = INPUT_VARIABLE_NAME
  ComponentTest%Input_Variable_Units  = INPUT_VARIABLE_UNITS
  ComponentTest%Output_Variable_Name  = OUTPUT_VARIABLE_NAME
  ComponentTest%Output_Variable_Units = OUTPUT_VARIABLE_UNITS

  ! Initialise the number of profiles actually written
  nm = 0

          ! Begin profile loop
          Profile_Loop: DO m = 1, N_PROFILES

            ! Cycle profile loop if no clouds in input data
            IF ( Atmosphere(m)%n_Clouds == 0 ) THEN
              WRITE( *, '( 5x, "No clouds in profile # ", i3, ". Cycling..." )' ) m
              CYCLE Profile_Loop
            END IF
            WRITE( *, '( 5x, "Processing profile # ", i3 )' ) m

            ! Determine the cloud index to use for this profile
            ! Rain and snow clouds stress the CloudScatter code
            ! a bit more than water clouds, so they're used if
            ! possible.
            mc = 1
            Cloud_Select: DO n = 1, Atmosphere(m)%n_Clouds
              IF ( Atmosphere(m)%Cloud(n)%Type == RAIN_CLOUD .OR. &
                   Atmosphere(m)%Cloud(n)%Type == SNOW_CLOUD      ) THEN
                mc = n
                EXIT Cloud_Select
              END IF
            END DO Cloud_Select
            mcType = Atmosphere(m)%Cloud(mc)%Type

            ! Allocate CloudScatter structures
            Error_Status    = CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers, &  ! Input
                                                        MAX_N_LEGENDRE_TERMS,   &  ! Input
                                                        MAX_N_PHASE_ELEMENTS,   &  ! Input
                                                        CloudScatter_Baseline   )  ! Output
            Error_Status_NL = CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers, &  ! Input
                                                        MAX_N_LEGENDRE_TERMS,   &  ! Input
                                                        MAX_N_PHASE_ELEMENTS,   &  ! Input
                                                        CloudScatter_NL         )  ! Output
            Error_Status_TL = CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers, &  ! Input
                                                        MAX_N_LEGENDRE_TERMS,   &  ! Input
                                                        MAX_N_PHASE_ELEMENTS,   &  ! Input
                                                        CloudScatter_TL         )  ! Output
            Error_Status_Neg_NL = CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers, &  ! Input
                                                            MAX_N_LEGENDRE_TERMS,   &  ! Input
                                                            MAX_N_PHASE_ELEMENTS,   &  ! Input
                                                            CloudScatter_Neg_NL     )  ! Output
            Error_Status_Pos_NL = CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers,  &  ! Input
                                                            MAX_N_LEGENDRE_TERMS,    &  ! Input
                                                            MAX_N_PHASE_ELEMENTS,    &  ! Input
                                                            CloudScatter_Pos_NL      )  ! Output
            Error_Status_Neg_TL = CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers, &  ! Input
                                                            MAX_N_LEGENDRE_TERMS,   &  ! Input
                                                            MAX_N_PHASE_ELEMENTS,   &  ! Input
                                                            CloudScatter_Neg_TL     )  ! Output
            Error_Status_Pos_TL = CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers, &  ! Input
                                                            MAX_N_LEGENDRE_TERMS,   &  ! Input
                                                            MAX_N_PHASE_ELEMENTS,   &  ! Input
                                                            CloudScatter_Pos_TL     )  ! Output
                                                           
            IF ( Error_Status    /= SUCCESS .OR. &
                 Error_Status_NL /= SUCCESS .OR. &
                 Error_Status_TL /= SUCCESS .OR. &
                 Error_Status_Pos_TL /= SUCCESS .OR. &
                 Error_Status_Pos_NL /= SUCCESS .OR. &
                 Error_Status_Neg_TL /= SUCCESS .OR. &
                 Error_Status_Pos_TL /= SUCCESS      ) THEN
              CALL Display_Message( PROGRAM_NAME, &
                                    'Error occurred allocating CloudScatter structures', &
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
              CALL Display_Message( PROGRAM_NAME, &
                                    TRIM( Message ), &
                                    Error_Status )
              STOP
            END IF
            CALL CRTM_Zero_Atmosphere( Atmosphere_TL )

            

                    ! Loop over sensor channels
                    Channel_Loop: DO l = 1, ChannelInfo%n_Channels
                      WRITE( *, '( 5x, "Channel: ", i5 )' ) ChannelInfo%Sensor_Channel(l)

                      
                     
                      ! Compute the baseline layer optical
                      ! depths due to gaseous absorption
                      Error_Status = CRTM_Compute_CloudScatter(  Atmosphere(m),                &  ! Input
                                                                 ChannelInfo%Channel_Index(l), &  ! Input, scalar
                                                                 CloudScatter_Baseline,        &  ! In/Output
                                                                 CSV                           )  ! Internal variable output
                      IF ( Error_Status /= SUCCESS ) THEN
                        CALL Display_Message( PROGRAM_NAME, &
                                              'Error computing AtmScatter_Baseline', &
                                              Error_Status )
                        STOP
                      END IF

                              ! Loop over input variables
                              Input_Variable_Loop: DO nIV = 1, N_INPUT_VARIABLES
                                WRITE( *, '( 5x, "Perturbation variable: ", a )' ) INPUT_VARIABLE_NAME(nIV)


                                ! Initialize logical flags
                                FatalFailure=.FALSE.
                                 
          
                                1 CONTINUE
                                PRINT *, FatalFailure
                                        ! Begin layer loop
                                        Layer_Loop: DO k = 1, Atmosphere(m)%n_Layers

                                          ! Initialize the perturbation delta
                                          DeltaX = Max_DeltaX

                                          ! Initialize the perturbation counter
                                          nP = 0

                                                  ! Begin the perturbation loop
                                                  Perturbation_Loop: DO 

                                                    ! Increment perturbation counter
                                                    nP=nP+1 

                                                            !#--------------------------------------------------------------------------------#
                                                            !# -- BEGIN THE SWITCH LOOP (Need to calc AtmAbsorption for Neg and Pos Pert). -- #
                                                            !#--------------------------------------------------------------------------------#


                                                            Switch_Loop: DO switch = -1, 1, 2



                                                               ! Calculate the Perturbation_Used by multiplying the switch and current DeltaX
                                                               switchmultiplier=REAL(switch,fp)

                                                               Perturbation_Used = switchmultiplier*DeltaX
                                                               

                                                               ! Fill Perturbation fraction array which will be used to fill the perturbations for componenttest
                                                               IF ( switch == -1 ) THEN
                                                                 PERTURBATION_FRACTION(nP) = Perturbation_Used
                                                               END IF
                                                               IF ( switch == 1 ) THEN 
                                                                 PERTURBATION_FRACTION((N_Perturbations + 1) - nP) = Perturbation_Used
                                                               END IF


                                                               ! Re-initialise all NL and TL data
                                                               Error_Status_NL = CRTM_Assign_Atmosphere( Atmosphere(m), &  ! Input
                                                                                                         Atmosphere_NL  )  ! Output
                                                               IF ( Error_Status_NL /= SUCCESS ) THEN
                                                                 CALL Display_Message( PROGRAM_NAME, &
                                                                                       'Error reinitialising forward model Atmosphere input data', &
                                                                                       Error_Status )
                                                                 STOP
                                                               END IF
                                                               CALL CRTM_Zero_Atmosphere( Atmosphere_TL )

                                                               ! Perturb the inputs. Only ONE cloud at a time
                                                               Input_Variable_Select: SELECT CASE ( nIV )

                                                                 ! -- Temperature
                                                                 CASE ( NIV_T )
                                                                   Atmosphere_TL%Temperature(k) = &
                                                                     Perturbation_Used * Atmosphere(m)%Temperature(k)

                                                                   Atmosphere_NL%Temperature(k) = &
                                                                     Atmosphere_NL%Temperature(k) + &
                                                                     Atmosphere_TL%Temperature(k)

                                                                   Linear_Test=Atmosphere_TL%Temperature(k)
                                                                   

                                                                 ! -- Effective radius
                                                                 CASE ( NIV_RE )
                                                                   Atmosphere_TL%Cloud(mc)%Effective_Radius(k) = &
                                                                     Perturbation_Used * Atmosphere(m)%Cloud(mc)%Effective_Radius(k)

                                                                   Atmosphere_NL%Cloud(mc)%Effective_Radius(k) = &
                                                                     Atmosphere_NL%Cloud(mc)%Effective_Radius(k) + &
                                                                     Atmosphere_TL%Cloud(mc)%Effective_Radius(k)

                                                                   Linear_Test=Atmosphere_TL%Cloud(mc)%Effective_Radius(k)

                                                                 ! -- Effective variance
                                                                 CASE ( NIV_RV )
                                                                   Atmosphere_TL%Cloud(mc)%Effective_Variance(k) = &
                                                                     Perturbation_Used * Atmosphere(m)%Cloud(mc)%Effective_Variance(k)

                                                                   Atmosphere_NL%Cloud(mc)%Effective_Variance(k) = &
                                                                     Atmosphere_NL%Cloud(mc)%Effective_Variance(k) + &
                                                                     Atmosphere_TL%Cloud(mc)%Effective_Variance(k)

                                                                   Linear_Test=Atmosphere_TL%Cloud(mc)%Effective_Variance(k)

                                                                 ! -- Cloud water content
                                                                 CASE ( NIV_Q )
                                                                   Atmosphere_TL%Cloud(mc)%Water_Content(k) = &
                                                                     Perturbation_Used * Atmosphere(m)%Cloud(mc)%Water_Content(k)

                                                                   Atmosphere_NL%Cloud(mc)%Water_Content(k) = &
                                                                     Atmosphere_NL%Cloud(mc)%Water_Content(k) + &
                                                                     Atmosphere_TL%Cloud(mc)%Water_Content(k)

                                                                   Linear_Test=Atmosphere_TL%Cloud(mc)%Water_Content(k)
                                                                   
                                                                     
                                                                   
                                                               END SELECT Input_Variable_Select

                                                               ! -----------------------------
                                                               ! Calculate TL/NL cloud scatter
                                                               ! -----------------------------


                                                               ! Compute the perturbed cloud scattering properties
                                                               Error_Status_NL = CRTM_Compute_CloudScatter( Atmosphere_NL,                &  ! Input
                                                                                                            ChannelInfo%Channel_Index(l), &  ! Input
                                                                                                            CloudScatter_NL,              &  ! In/Output
                                                                                                            CSV_Dummy                     )  ! Internal variable output
                                                               Error_Status_TL = CRTM_Compute_CloudScatter_TL( Atmosphere(m),                &  ! Input
                                                                                                               CloudScatter_Baseline,        &  ! Input
                                                                                                               Atmosphere_TL,                &  ! Input
                                                                                                               ChannelInfo%Channel_Index(l), &  ! Input
                                                                                                               CloudScatter_TL,              &  ! In/Output
                                                                                                               CSV                           )  ! Internal variable input
                                                               IF ( Error_Status_NL /= SUCCESS .OR. &
                                                                    Error_Status_TL /= SUCCESS      ) THEN
                                                                 CALL Display_Message( PROGRAM_NAME, &
                                                                                       'Error computing CloudScatter NL/TL', &
                                                                                        Error_Status )                           
                                                                 STOP
                                                               END IF
                                                               
                                                               ! Save results
                                                               IF (switch==-1) THEN
                                                                 Error_Status = CRTM_Assign_AtmScatter( CloudScatter_NL,     &
                                                                                                        CloudScatter_Neg_NL  )

                                                                 IF ( Error_Status /= SUCCESS ) THEN
                                                                    CALL Display_Message( PROGRAM_NAME,  &
                                                                                          'Error assigning CloudScatter TL/NL structures', &
                                                                                           Error_Status )
                                                                    STOP
                                                                 END IF                          
                                                                 
                                                                 Error_Status = CRTM_Assign_AtmScatter( CloudScatter_TL,     &
                                                                                                        CloudScatter_Neg_TL  )

                                                                 IF ( Error_Status /= SUCCESS ) THEN
                                                                    CALL Display_Message( PROGRAM_NAME,  &
                                                                                          'Error assigning CloudScatter TL/NL structures', &
                                                                                           Error_Status )
                                                                    STOP
                                                                 END IF  

                                                                  
                                                               ELSE  
                                                                 Error_Status = CRTM_Assign_AtmScatter( CloudScatter_NL,     &
                                                                                                        CloudScatter_Pos_NL  )
                                                                 
                                                                 IF ( Error_Status /= SUCCESS ) THEN
                                                                    CALL Display_Message( PROGRAM_NAME,  &
                                                                                          'Error assigning CloudScatter TL/NL structures', &
                                                                                           Error_Status )
                                                                    STOP
                                                                 END IF    

                                                                 Error_Status = CRTM_Assign_AtmScatter( CloudScatter_TL,     &
                                                                                                        CloudScatter_Pos_TL  )
                                                                 

                                                                 IF ( Error_Status /= SUCCESS ) THEN
                                                                    CALL Display_Message( PROGRAM_NAME,  &
                                                                                          'Error assigning CloudScatter TL/NL structures', &
                                                                                           Error_Status )
                                                                    STOP
                                                                 END IF

                                                               END IF

                                                            END DO Switch_Loop
                                                   
                                                  ! ----------------------------------------------
                                                  ! CALCULATE THE GRADIENTS AND TEST BY COMPARISON
                                                  ! IF A FATAL FAILURE HAS NOT OCCURED
                                                  ! ----------------------------------------------   

                                                  IF ( .NOT. FatalFailure ) THEN 
                                                    
                                                    IF (Linear_Test == ZERO) THEN
                                                       CYCLE Layer_Loop
                                                    END IF 
                                                    ! Optical depth gradients
                                                    Positive_OD_TL = ABS(CloudScatter_Pos_TL%Optical_Depth(k)/Linear_Test)
                                                    Negative_OD_TL = ABS(CloudScatter_Neg_TL%Optical_Depth(k)/Linear_Test)
                                                    Positive_OD_NL = ABS((CloudScatter_Pos_NL%Optical_Depth(k) - CloudScatter_Baseline%Optical_Depth(k))/Linear_Test)
                                                    Negative_OD_NL = ABS((CloudScatter_Neg_NL%Optical_Depth(k) - CloudScatter_Baseline%Optical_Depth(k))/Linear_Test)
                                                   
                                                      
                                                      
                                                    ! Single Scatter Albedo gradients
                                                    Positive_SSA_TL = ABS(CloudScatter_Pos_TL%Single_Scatter_Albedo(k)/Linear_Test)
                                                    Negative_SSA_TL = ABS(CloudScatter_Neg_TL%Single_Scatter_Albedo(k)/Linear_Test)
                                                    Positive_SSA_NL = ABS((CloudScatter_Pos_NL%Single_Scatter_Albedo(k) - CloudScatter_Baseline%Single_Scatter_Albedo(k))/Linear_Test)
                                                    Negative_SSA_NL = ABS((CloudScatter_Neg_NL%Single_Scatter_Albedo(k) - CloudScatter_Baseline%Single_Scatter_Albedo(k))/Linear_Test)

                                                    ! Assymetry Factor
                                                    Positive_AF_TL = ABS(CloudScatter_Pos_TL%Asymmetry_Factor(k)/Linear_Test)
                                                    Negative_AF_TL = ABS(CloudScatter_Neg_TL%Asymmetry_Factor(k)/Linear_Test)
                                                    Positive_AF_NL = ABS((CloudScatter_Pos_NL%Asymmetry_Factor(k) - CloudScatter_Baseline%Asymmetry_Factor(k))/Linear_Test)
                                                    Negative_AF_NL = ABS((CloudScatter_Neg_NL%Asymmetry_Factor(k) - CloudScatter_Baseline%Asymmetry_Factor(k))/Linear_Test)

                                                    ! Delta Truncation
                                                    Positive_DT_TL = ABS(CloudScatter_Pos_TL%Delta_Truncation(k)/Linear_Test)
                                                    Negative_DT_TL = ABS(CloudScatter_Neg_TL%Delta_Truncation(k)/Linear_Test)
                                                    Positive_DT_NL = ABS((CloudScatter_Pos_NL%Delta_Truncation(k) - CloudScatter_Baseline%Delta_Truncation(k))/Linear_Test)
                                                    Negative_DT_NL = ABS((CloudScatter_Neg_NL%Delta_Truncation(k) - CloudScatter_Baseline%Delta_Truncation(k))/Linear_Test)

                                                            ! Phase Coefficient
                                                            DO nLT = 0, MAX_N_LEGENDRE_TERMS
                                                              
                                                              Positive_PC_TL(nLT) = ABS(CloudScatter_Pos_TL%Phase_Coefficient(nLT,1,k)/Linear_Test)
                                                              Negative_PC_TL(nLT) = ABS(CloudScatter_Neg_TL%Phase_Coefficient(nLT,1,k)/Linear_Test)
                                                              Positive_PC_NL(nLT) = ABS((CloudScatter_Pos_NL%Phase_Coefficient(nLT,1,k) - &
                                                              CloudScatter_Pos_NL%Phase_Coefficient(nLT,1,k))/Linear_Test)
                                                              Negative_PC_NL(nLT) = ABS((CloudScatter_Neg_NL%Phase_Coefficient(nLT,1,k) - &
                                                              CloudScatter_Pos_NL%Phase_Coefficient(nLT,1,k))/Linear_Test)

                                                              TL_PC_GradientEqual = Compare_Float( Positive_PC_TL(nLT),    &  ! Input
                                                                                                   Negative_PC_TL(nLT),    &  ! Input
                                                                                                   ULP=ULP                 )  ! Input

                                                              NLTL_PC_NegGradientEqual = Compare_Float( Negative_PC_TL(nLT),  &   !  Input
                                                                                                        Negative_PC_NL(nLT),  &   !  Input
                                                                                                        ULP=ULP               )

                                                              NLTL_PC_PosGradientEqual = Compare_Float( Positive_PC_TL(nLT),  &
                                                                                                        Positive_PC_NL(nLT),  &
                                                                                                        ULP=ULP               )

                                                              IF (.NOT. TL_PC_GradientEqual .OR. .NOT. NLTL_PC_NegGradientEqual .OR. .NOT. NLTL_PC_PosGradientEqual) THEN
                                                                PC_Test = .FALSE.
                                                                EXIT
                                                              END IF

                                                            END DO

                                                    TL_OD_GradientEqual = Compare_Float( Positive_OD_TL,  &  !  Input
                                                                                         Negative_OD_TL,  &  !  Input
                                                                                         ULP=ULP          )  !  Input


                                                    NLTL_OD_NegGradientEqual = Compare_Float( Negative_OD_TL,    &    !  Input
                                                                                              Negative_OD_NL,    &    !  Input
                                                                                              ULP=ULP            )    !  Input 

                                                    NLTL_OD_PosGradientEqual = Compare_Float( Positive_OD_TL,    &    !  Input                                       
                                                                                              Negative_OD_NL,    &    !  Input
                                                                                              ULP=ULP            )    !  Input
                                                    
                                                    IF (.NOT. TL_OD_GradientEqual .OR. .NOT. NLTL_OD_NegGradientEqual .OR. .NOT. NLTL_OD_PosGradientEqual) THEN
                                                      OD_Test = .FALSE.
                                                    END IF


                                                    TL_SSA_GradientEqual = Compare_Float( Positive_SSA_TL,  &  !  Input
                                                                                          Negative_SSA_TL,  &  !  Input
                                                                                          ULP=ULP           )  !  Input


                                                    NLTL_SSA_NegGradientEqual = Compare_Float( Negative_SSA_TL,     &    !  Input
                                                                                               Negative_SSA_NL,     &    !  Input
                                                                                               ULP=ULP              )    !  Input 

                                                    NLTL_SSA_PosGradientEqual = Compare_Float( Positive_SSA_TL,    &    !  Input                                       
                                                                                               Negative_SSA_NL,    &    !  Input
                                                                                               ULP=ULP             )    !  Input

                                                    IF (.NOT. TL_SSA_GradientEqual .OR. .NOT. NLTL_SSA_NegGradientEqual .OR. .NOT. NLTL_SSA_PosGradientEqual) THEN
                                                      SSA_Test = .FALSE.
                                                    END IF

                                                    TL_AF_GradientEqual = Compare_Float( Positive_AF_TL,  &  !  Input
                                                                                         Negative_AF_TL,  &  !  Input
                                                                                         ULP=ULP          )  !  Input


                                                    NLTL_AF_NegGradientEqual = Compare_Float( Negative_AF_TL,    &    !  Input
                                                                                              Negative_AF_NL,    &    !  Input
                                                                                              ULP=ULP            )    !  Input 

                                                    NLTL_AF_PosGradientEqual = Compare_Float( Positive_AF_TL,    &    !  Input                                       
                                                                                              Negative_AF_NL,    &    !  Input
                                                                                              ULP=ULP            )    !  Input
                                                    
                                                    IF (.NOT. TL_AF_GradientEqual .OR. .NOT. NLTL_AF_NegGradientEqual .OR. .NOT. NLTL_AF_PosGradientEqual) THEN
                                                      AF_Test = .FALSE.
                                                    END IF


                                                    TL_DT_GradientEqual = Compare_Float( Positive_DT_TL,  &  !  Input
                                                                                         Negative_DT_TL,  &  !  Input
                                                                                         ULP=ULP          )  !  Input


                                                    NLTL_DT_NegGradientEqual = Compare_Float( Negative_DT_TL,    &    !  Input
                                                                                              Negative_DT_NL,    &    !  Input
                                                                                              ULP=ULP            )    !  Input 

                                                    NLTL_DT_PosGradientEqual = Compare_Float( Positive_DT_TL,    &    !  Input                                       
                                                                                              Negative_DT_NL,    &    !  Input
                                                                                              ULP=ULP            )    !  Input

                                                    
                                                    IF (.NOT. TL_DT_GradientEqual .OR. .NOT. NLTL_DT_NegGradientEqual .OR. .NOT. NLTL_DT_PosGradientEqual) THEN
                                                      DT_Test = .FALSE.
                                                    END IF


                                                    !  Determine if ALL of the tests passed ..
                                                    IF ( OD_Test .AND. AF_Test .AND. SSA_Test .AND. DT_Test .AND. PC_Test) THEN

                                                        CYCLE Layer_Loop
                                                    !  If the test has not passed multiply DeltaX by 0.5
                                                    ELSE
                                                        CALL Get_Next_DeltaX(DeltaX)
                                                        ! Test to see if np==Max_Iterations
                                                        IF (Max_Iterations( nP )) THEN
                                                          FatalFailure = .TRUE.
                                                          GO TO 1
                                                        END IF
                                                    END IF



                                                  ELSE


                                                      ! Save the data for this profile if necessary 
                                                      ComponentTest%d1(k,l,nP,nIV,NOV_TAU) = (CloudScatter_Neg_NL%Optical_Depth(k) - &
                                                                                             CloudScatter_Baseline%Optical_Depth(k))
                                                      ComponentTest%d2(k,l,nP,nIV,NOV_TAU) = CloudScatter_Neg_TL%Optical_Depth(k)

                                                      ComponentTest%d1(k,l,((N_Perturbations + 1) - nP),nIV,NOV_TAU) = (CloudScatter_Pos_NL%Optical_Depth(k) - &
                                                                                                                       CloudScatter_Baseline%Optical_Depth(k))
                                                      ComponentTest%d2(k,l,((N_Perturbations + 1) - nP),nIV,NOV_TAU) = CloudScatter_Pos_TL%Optical_Depth(k)
                                                       

                                                     ! PRINT *, ComponentTest%d2(k,l,((N_Perturbations + 1) - nP),nIV,NOV_TAU) 
                                                      ComponentTest%d1(k,l,Zero_Pert,nIV,NOV_TAU) = ZERO
                                                      ComponentTest%d2(k,l,Zero_Pert,nIV,NOV_TAU) = ZERO

                                                      ComponentTest%d1(k,l,nP,nIV,NOV_OMEGA) = (CloudScatter_Neg_NL%Single_Scatter_Albedo(k) - &
                                                                                               CloudScatter_Baseline%Single_Scatter_Albedo(k))
                                                      ComponentTest%d2(k,l,nP,nIV,NOV_OMEGA) = CloudScatter_Neg_TL%Single_Scatter_Albedo(k)

                                                      ComponentTest%d1(k,l,((N_Perturbations + 1) - nP),nIV,NOV_OMEGA) = (CloudScatter_Pos_NL%Single_Scatter_Albedo(k) - &
                                                                                                                         CloudScatter_Baseline%Single_Scatter_Albedo(k))
                                                      ComponentTest%d2(k,l,((N_Perturbations + 1) - nP),nIV,NOV_OMEGA) = CloudScatter_Pos_TL%Single_Scatter_Albedo(k)

                                                      ComponentTest%d1(k,l,Zero_Pert,nIV,NOV_OMEGA) = ZERO
                                                      ComponentTest%d2(k,l,Zero_Pert,nIV,NOV_OMEGA) = ZERO

                                                      ComponentTest%d1(k,l,nP,nIV,NOV_G) = (CloudScatter_Neg_NL%Asymmetry_Factor(k) - &
                                                                                           CloudScatter_Baseline%Asymmetry_Factor(k))
                                                      ComponentTest%d2(k,l,nP,nIV,NOV_G) = CloudScatter_Neg_TL%Asymmetry_Factor(k)

                                                      ComponentTest%d1(k,l,((N_Perturbations + 1) - nP),nIV,NOV_G) = (CloudScatter_Pos_NL%Asymmetry_Factor(k) - &
                                                                                                                     CloudScatter_Baseline%Asymmetry_Factor(k))
                                                      ComponentTest%d2(k,l,((N_Perturbations + 1) - nP),nIV,NOV_G) = CloudScatter_Pos_TL%Asymmetry_Factor(k)

                                                      ComponentTest%d1(k,l,Zero_Pert,nIV,NOV_G) = ZERO
                                                      ComponentTest%d2(k,l,Zero_Pert,nIV,NOV_G) = ZERO

                                                      ComponentTest%d1(k,l,nP,nIV,NOV_D) = (CloudScatter_Neg_NL%Delta_Truncation(k) - &
                                                                                           CloudScatter_Baseline%Delta_Truncation(k))
                                                      ComponentTest%d2(k,l,nP,nIV,NOV_D) = CloudScatter_Neg_TL%Delta_Truncation(k)

                                                      ComponentTest%d1(k,l,((N_Perturbations + 1) - nP),nIV,NOV_D) = (CloudScatter_Pos_NL%Delta_Truncation(k) - &
                                                                                                                     CloudScatter_Baseline%Delta_Truncation(k))
                                                      ComponentTest%d2(k,l,((N_Perturbations + 1) - nP),nIV,NOV_D) = CloudScatter_Pos_TL%Delta_Truncation(k)

                                                      ComponentTest%d1(k,l,Zero_Pert,nIV,NOV_D) = ZERO
                                                      ComponentTest%d2(k,l,Zero_Pert,nIV,NOV_D) = ZERO

                                                         DO nLT = 0, MAX_N_LEGENDRE_TERMS
                                                           nOV = NOV_PC_L0 + nLT  ! Offset index into output arrays
                                                           ComponentTest%d1(k,l,nP,nIV,nOV) = (CloudScatter_Neg_NL%Phase_Coefficient(nLT,1,k) - &
                                                                                              CloudScatter_Baseline%Phase_Coefficient(nLT,1,k))
                                                           ComponentTest%d2(k,l,nP,nIV,nOV) = CloudScatter_Neg_TL%Phase_Coefficient(nLT,1,k)

                                                           ComponentTest%d1(k,l,((N_Perturbations + 1) - nP),nIV,nOV) = (CloudScatter_Pos_NL%Phase_Coefficient(nLT,1,k) - &
                                                                                                                        CloudScatter_Baseline%Phase_Coefficient(nLT,1,k))
                                                           ComponentTest%d2(k,l,((N_Perturbations + 1) - nP),nIV,nOV) = CloudScatter_Pos_TL%Phase_Coefficient(nLT,1,k)

                                                           ComponentTest%d1(k,l,Zero_Pert,nIV,nOV) = ZERO
                                                           ComponentTest%d2(k,l,Zero_Pert,nIV,nOV) = ZERO
                                                         END DO
                                                    
                                                      CALL Get_Next_DeltaX(DeltaX)

                                                      IF (Max_Iterations( nP )) THEN
                                                        Cycle Layer_Loop
                                                      END IF
                                                   END IF 
                                                    
                                                  END DO Perturbation_Loop
                                        END DO Layer_Loop
                              END DO Input_Variable_Loop
                    END DO Channel_Loop

    ! Set the current dataset number in the output structure
    nm = nm + 1
    ComponentTest%nM = nm
    WRITE( ComponentTest%nM_Name, '( "Profile # ", i3, ". Selected cloud: ", a )' ) &
                                  m, CLOUD_TYPE_NAME( mcType)

    ! Write the data
    WRITE( *, '( 10x, "Writing FWD/TL data to output file...." )' )
    Error_Status = Write_ComponentTest_netCDF( TRIM( ComponentTest_File ), &
                                               ComponentTest, &
                                               New_File      = New_File, &
                                               Title         = 'FWD/TL CRTM CloudScatter '//&
                                                               'test results for '//&
                                                               TRIM( File_Prefix ), &
                                               History       = PROGRAM_RCS_ID, &
                                               Sensor_Name   = TRIM( File_Prefix ), &
                                               Platform_Name = TRIM( File_Prefix ), &
                                               Comment       = 'Only one phase element '//&
                                                               'of phase coefficient '//&
                                                               'data written.', &
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

    ! Deallocate structures for current profile
    Error_Status_NL = CRTM_Destroy_AtmScatter( CloudScatter_NL )
    Error_Status_TL = CRTM_Destroy_AtmScatter( CloudScatter_TL )
    Error_Status    = CRTM_Destroy_AtmScatter( CloudScatter_Baseline )
    Error_Status_Pos_NL = CRTM_Destroy_AtmScatter( CloudScatter_Pos_NL )
    Error_Status_Neg_NL = CRTM_Destroy_AtmScatter( CloudScatter_Neg_NL )
    Error_Status_Pos_TL = CRTM_Destroy_AtmScatter( CloudScatter_Pos_TL )
    Error_Status_Neg_TL = CRTM_Destroy_AtmScatter( CloudScatter_Neg_TL )
    IF ( Error_Status    /= SUCCESS .OR. &
         Error_Status_TL /= SUCCESS .OR. &
         Error_Status_NL /= SUCCESS .OR. &
         Error_Status_Pos_TL /= SUCCESS .OR. &
         Error_Status_Pos_NL /= SUCCESS .OR. &
         Error_Status_Neg_TL /= SUCCESS .OR. &
         Error_Status_Neg_NL /= SUCCESS      ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error deallocating CloudScatter structures', &
                            Error_Status )
      STOP
    END IF

    Error_Status = CRTM_Destroy_Atmosphere( Atmosphere_TL, Atmosphere_NL )
    IF ( Error_Status /= SUCCESS ) THEN
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
                           TRIM( File_Prefix ), &
                            Error_Status )                           
   STOP
  END IF

  ! Destroy the CRTM
  WRITE( *, '( /5x, "Destroying the CRTM..." )' )
  Error_Status = CRTM_Destroy( ChannelInfo )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying CRTM', & 
                           WARNING )
  END IF

  ! Deallocate Atmosphere structure array
  Error_Status = CRTM_Destroy_Atmosphere( Atmosphere )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error deallocating Atmosphere structure array', &
                          WARNING )
  END IF

  CONTAINS

  SUBROUTINE Get_Next_DeltaX ( sDeltaX )
    ! Purpose: 
    ! To update DeltaX in perturbation loop. 
    ! The perturbation is halved for each iteration
    IMPLICIT NONE
    REAL(fp), INTENT(INOUT) :: sDeltaX
    sDeltaX=(0.500_fp)*sDeltaX
  END SUBROUTINE Get_Next_DeltaX

  FUNCTION Max_Iterations( fnP )
    ! Purpose:
    ! To determine if np has reached the maximum number of 
    ! perturbation iterations allowed
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: fnP
    LOGICAL :: Max_Iterations

    Max_Iterations = (fnP==Max_Delta_Iter)
  END FUNCTION Max_Iterations

END PROGRAM Test_Tangent_Linear
