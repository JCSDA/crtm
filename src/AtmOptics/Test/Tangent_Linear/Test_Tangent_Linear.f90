
!  Program to test the CRTM AtmOptics Tangent-Linear code.
!
!  CREATION HISTORY:    David Groff, SAIC 18-May-2006
!                       david.groff@noaa.gov
!

PROGRAM Test_Tangent_Linear


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler
  USE CRTM_Parameters
  USE CRTM_LifeCycle
  USE CRTM_GeometryInfo_Define, ONLY: CRTM_GeometryInfo_type, &
                                      CRTM_Compute_GeometryInfo
  USE CRTM_ChannelInfo_Define
  USE CRTM_Atmosphere_Define
  USE CRTM_AtmAbsorption, ONLY: CRTM_AtmAbsorption_type, &
                                CRTM_Allocate_AtmAbsorption, &
                                CRTM_Destroy_AtmAbsorption, &
                                CRTM_Compute_AtmAbsorption, &
                                CRTM_Compute_AtmAbsorption_TL
  USE CRTM_CloudScatter, ONLY: CRTM_AtmScatter_type, &
                               CRTM_Allocate_AtmScatter, &
                               CRTM_Destroy_AtmScatter, &
                               CRTM_CSVariables_type, &
                               CRTM_Compute_CloudScatter, &
                               CRTM_Compute_CloudScatter_TL
  USE CRTM_AerosolScatter, ONLY: CRTM_Compute_AerosolScatter, &
                                 CRTM_Compute_AerosolScatter_TL
  USE CRTM_AtmOptics, ONLY: CRTM_AOVariables_type, &
                            CRTM_Combine_AtmOptics, &
                            CRTM_Combine_AtmOptics_TL
  USE CRTM_Atmosphere_Binary_IO, ONLY: CRTM_Read_Atmosphere_Binary
  USE ComponentTest_netCDF_IO
  USE CRTM_Predictor
  USE CRTM_Predictor_Define
  

  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME   = 'Test_Tangent_Linear'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  CHARACTER( * ),  PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  INTEGER, PARAMETER :: SL = 512

  CHARACTER(*), PARAMETER :: ATMOSPHERE_FILENAME = 'ECMWF-Atmosphere.Cloud.Aerosol.bin'
  INTEGER,      PARAMETER :: N_PROFILES = 52

  

  INTEGER,         PARAMETER :: N_PERTURBATIONS = 37
  REAL( fp_kind ), PARAMETER, DIMENSION(N_PERTURBATIONS) :: PERTURBATION_FRACTION = &
  (/-0.1000_fp_kind, -0.0900_fp_kind, -0.0800_fp_kind, -0.0700_fp_kind, -0.0600_fp_kind, -0.0500_fp_kind, &
    -0.0400_fp_kind, -0.0300_fp_kind, -0.0200_fp_kind, -0.0100_fp_kind, -0.0040_fp_kind, -0.0035_fp_kind, &
    -0.0030_fp_kind, -0.0025_fp_kind, -0.0020_fp_kind, -0.0015_fp_kind, -0.0010_fp_kind, -0.0005_fp_kind, &
     0.0000_fp_kind, &
     0.0005_fp_kind,  0.0010_fp_kind,  0.0015_fp_kind,  0.0020_fp_kind,  0.0025_fp_kind,  0.0030_fp_kind, &
     0.0035_fp_kind,  0.0040_fp_kind,  0.0100_fp_kind,  0.0200_fp_kind,  0.0300_fp_kind,  0.0400_fp_kind, &
     0.0500_fp_kind,  0.0600_fp_kind,  0.0700_fp_kind,  0.0800_fp_kind,  0.0900_fp_kind,  0.1000_fp_kind /)
!  (/-0.100_fp_kind, -0.090_fp_kind, -0.080_fp_kind, -0.070_fp_kind, -0.060_fp_kind, -0.050_fp_kind, &
!    -0.040_fp_kind, -0.030_fp_kind, -0.020_fp_kind, -0.018_fp_kind, -0.016_fp_kind, -0.014_fp_kind, &
!    -0.012_fp_kind, -0.010_fp_kind, -0.008_fp_kind, -0.006_fp_kind, -0.004_fp_kind, -0.002_fp_kind, &
!     0.000_fp_kind, &
!     0.002_fp_kind,  0.004_fp_kind,  0.006_fp_kind,  0.008_fp_kind,  0.010_fp_kind,  0.012_fp_kind, &
!     0.014_fp_kind,  0.016_fp_kind,  0.018_fp_kind,  0.020_fp_kind,  0.030_fp_kind,  0.040_fp_kind, &
!     0.050_fp_kind,  0.060_fp_kind,  0.070_fp_kind,  0.080_fp_kind,  0.090_fp_kind,  0.100_fp_kind /)
  INTEGER, PARAMETER :: N_INPUT_VARIABLES = 10
  INTEGER, PARAMETER :: NIV_P = 1  ! Layer pressure   
  INTEGER, PARAMETER :: NIV_T = 2  ! Layer temperature
  INTEGER, PARAMETER :: NIV_W = 3  ! Layer water vapor
  INTEGER, PARAMETER :: NIV_O = 4  ! Layer ozone 
  INTEGER, PARAMETER :: NIV_CRE = 5  ! Cloud effective radius 
  INTEGER, PARAMETER :: NIV_CRV = 6  ! Cloud effective variance
  INTEGER, PARAMETER :: NIV_CQ  = 7  ! Cloud concentration
  INTEGER, PARAMETER :: NIV_ARE = 8  ! Aerosol effective radius 
  INTEGER, PARAMETER :: NIV_ARV = 9  ! Aerosol effective variance
  INTEGER, PARAMETER :: NIV_AC  = 10 ! Aerosol concentration
  CHARACTER( * ), PARAMETER, DIMENSION( N_INPUT_VARIABLES ) :: &
    INPUT_VARIABLE_NAME = (/ 'Layer pressure            ', &
                             'Layer temperature         ', &
                             'Layer water vapor         ', &
                             'Layer ozone               ', & 
        'Effective radius          ', &
        'Effective variance        ', &
        'Water content             ', &
        'Aerosol effective radius  ', &
        'Aerosol effective variance', &
        'Aerosol concentration     ' /)
  CHARACTER( * ), PARAMETER, DIMENSION( N_INPUT_VARIABLES ) :: &
    INPUT_VARIABLE_UNITS = (/ 'hectoPascal', &
                              'Kelvin     ', &
                              'g/kg       ', &
                              'ppmv       ', &
         'microns    ', &
         'microns    ', &
         'g/cm^2     ', &
         'microns    ', &
         'microns    ', &
         'ppm        ' /)

  INTEGER, PARAMETER :: N_OUTPUT_VARIABLES = 5
  INTEGER, PARAMETER :: NOV_TAU         = 1  ! Optical depth
  INTEGER, PARAMETER :: NOV_OMEGA       = 2  ! Single scatter albedo
  INTEGER, PARAMETER :: NOV_G           = 3  ! Asymmetry factor
  INTEGER, PARAMETER :: NOV_D           = 4  ! Delta truncation
  INTEGER, PARAMETER :: NOV_P  = 5  ! Phase coefficient
  CHARACTER( * ), PARAMETER, DIMENSION( N_OUTPUT_VARIABLES ) :: &
    OUTPUT_VARIABLE_NAME = (/ 'Optical depth          ', &
                              'Single scatter albedo  ', &
                              'Asymmetry factor       ', &
                              'Delta truncation       ', &
                              'Phase coefficient      ' /)
        
  CHARACTER( * ), PARAMETER, DIMENSION( N_OUTPUT_VARIABLES ) :: &
    OUTPUT_VARIABLE_UNITS = (/ 'Unitless', &  ! Optical depth
                               'Unitless', &  ! Single scatter albedo
                               'Unitless', &  ! Asymmetry factor
                               'Unitless', &  ! Delta truncation 
                               'Unitless' /)  ! Phase coefficient


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message
  INTEGER :: Error_Status
  INTEGER :: Error_Status_TL
  INTEGER :: Error_Status_NL
  CHARACTER( SL ) :: File_Prefix
  CHARACTER( SL ) :: SpcCoeff_File
  CHARACTER( SL ) :: TauCoeff_File
  CHARACTER( SL ) :: AerosolCoeff_File
  CHARACTER( SL ) :: CloudCoeff_File
  CHARACTER( SL ) :: EmisCoeff_File
  CHARACTER( SL ) :: ComponentTest_File
  INTEGER, DIMENSION( 1 ) :: Idx
  INTEGER :: H2O_Idx
  INTEGER :: O3_Idx
  INTEGER :: New_File
  INTEGER :: j, k, l, m, n, nP, nIV, nOV, nm, nLT
  INTEGER :: mc, mcType
  LOGICAL :: Cloud_Present
  INTEGER :: ma, mam, maType
  LOGICAL :: Aerosol_Present
  TYPE( CRTM_ChannelInfo_type )   :: ChannelInfo
  TYPE( CRTM_GeometryInfo_type )  :: GeometryInfo
  TYPE( CRTM_AtmAbsorption_type ) :: AtmAbsorption_Baseline, &
                                     AtmAbsorption_NL, AtmAbsorption_TL
  TYPE( CRTM_AtmScatter_type ) :: CloudScatter_Baseline, CloudScatter_NL, &
      CloudScatter_TL
  TYPE( CRTM_AtmScatter_type ) :: AerosolScatter_Baseline, AerosolScatter_NL, &
      AerosolScatter_TL
  TYPE( CRTM_AtmScatter_type ) :: AtmOptics_Baseline, AtmOptics_NL, &
      AtmOptics_TL
  TYPE( CRTM_Atmosphere_type ), DIMENSION( N_PROFILES ) :: Atmosphere
  TYPE( CRTM_Atmosphere_type )                          :: Atmosphere_NL, Atmosphere_TL
  TYPE( CRTM_CSVariables_type ) :: CSV, CSV_Dummy
  TYPE( CRTM_AOVariables_type ) :: AOV, AOV_Dummy
  TYPE( ComponentTest_type ) :: ComponentTest
  TYPE( CRTM_Predictor_type ) :: Predictor, Predictor_TL, Predictor_NL
  TYPE( CRTM_APVariables_type ) :: APV


  ! ------------------
  ! Program descriptor
  ! ------------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test the CRTM AtmOptics Tangent-Linear '//&
                        'components with respect to the Forward components.', &
                        '$Revision: 1.8 $' )



  !#----------------------------------------------------------------------------#
  !#                -- READ THE Atmosphere STRUCTURE DATA FILE --               #
  !#----------------------------------------------------------------------------#

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

  DO m = 1, N_PROFILES
    IF ( Atmosphere(m)%n_Clouds > 0 ) THEN
      DO n = 1, Atmosphere(m)%n_Clouds
        Atmosphere(m)%Cloud(n)%Water_Content = Atmosphere(m)%Cloud(n)%Water_Content / 1000.0_fp_kind
      END DO
    END IF
  END DO

 

  !#----------------------------------------------------------------------------#
  !#           -- ASSIGN DUMMY VALUES TO THE GeometryInfo STRUCTURE --          #
  !#----------------------------------------------------------------------------#

  GeometryInfo%Sensor_Scan_Angle   = 0.0_fp_kind   ! Nadir
  GeometryInfo%Sensor_Zenith_Angle = 0.0_fp_kind   ! Nadir

  Error_Status = CRTM_Compute_GeometryInfo( GeometryInfo )

  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error computing GeometryInfo values.', & 
                           Error_Status )
   STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                     -- GET THE COEFFICIENT FILENAMES --                    #
  !#----------------------------------------------------------------------------#

  ! ------------------------------------------------
  ! Enter the instrument file prefix, e.g. hirs3_n16
  ! ------------------------------------------------

  WRITE( *, FMT     = '( /5x, "Enter the instrument file prefix : " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) File_Prefix
  File_Prefix = ADJUSTL( File_Prefix )


  ! --------------------
  ! Create the filenames
  ! --------------------

  SpcCoeff_File     = TRIM( File_Prefix )//'.SpcCoeff.bin'
  TauCoeff_File     = TRIM( File_Prefix )//'.TauCoeff.bin'
  CloudCoeff_File   = 'CloudCoeff.bin'
  AerosolCoeff_File = 'AerosolCoeff.bin'
  EmisCoeff_File    = 'EmisCoeff.bin'

  ComponentTest_File   = TRIM( File_Prefix )//'.CRTM_AtmOptics.ComponentTest.nc'

  New_File = 1



  !#----------------------------------------------------------------------------#
  !#                          -- INITIALIZE THE CRTM --                         #
  !#----------------------------------------------------------------------------#

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



  !#------------------------------------------------------------------------#
  !#                   -- ALLOCATE ComponentTest STRUCTURE --               #
  !#------------------------------------------------------------------------#

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


  ! ----------------------------
  ! Assign data to ComponentTest
  ! ----------------------------

  ComponentTest%TestType = COMPONENTTEST_FWDTL_TESTTYPE
  ComponentTest%DataType = COMPONENTTEST_POLY_DATATYPE

  ComponentTest%Pressure      = Atmosphere(1)%Pressure
  ComponentTest%Spectral      = ChannelInfo%Sensor_Channel
  ComponentTest%Perturbation  = PERTURBATION_FRACTION

  ComponentTest%Input_Variable_Name  = INPUT_VARIABLE_NAME
  ComponentTest%Input_Variable_Units = INPUT_VARIABLE_UNITS

  ComponentTest%Output_Variable_Name  = OUTPUT_VARIABLE_NAME
  ComponentTest%Output_Variable_Units = OUTPUT_VARIABLE_UNITS



          !#----------------------------------------------------------------------------#
          !#                   -- LOOP OVER ATMOSPHERIC PROFILES --                     #
          !#----------------------------------------------------------------------------#

          Profile_Loop: DO m = 1, N_PROFILES

            WRITE( *, '( 5x, "Processing profile # ", i3 )' ) m


            ! -------------------------------------------------
            ! Determine the cloud index to use for this profile
            ! Rain and snow clouds stress the CloudScatter code
            ! a bit more than water clouds, so they're used if
            ! possible.
            ! -------------------------------------------------

            Cloud_Present = .FALSE.
            IF ( Atmosphere(m)%n_Clouds > 0 ) THEN
              Cloud_Present = .TRUE.
              mc = 1
              Cloud_Select: DO n = 1, Atmosphere(m)%n_Clouds
                IF ( Atmosphere(m)%Cloud(n)%Type == RAIN_CLOUD .OR. &
                     Atmosphere(m)%Cloud(n)%Type == SNOW_CLOUD      ) THEN
                  mc = n
                  EXIT Cloud_Select
                END IF
              END DO Cloud_Select
              mcType = Atmosphere(m)%Cloud(mc)%Type
            END IF


            ! ---------------------------------------------------
            ! Determine the aerosol index to use for this profile
            ! Just pick the first one
            ! ---------------------------------------------------

            Aerosol_Present = .FALSE.
            IF ( Atmosphere(m)%n_Aerosols > 0 ) THEN
              Aerosol_Present = .TRUE.
              ma  = 1
              mam = 1  ! The mode
              maType = Atmosphere(m)%Aerosol(ma)%Type
            END IF


            ! ------------------------------
            ! Determine the absorber indices
            ! ------------------------------

            ! -- Water vapour
            IF ( COUNT( Atmosphere(m)%Absorber_ID == H2O_ID ) /= 1 ) THEN
              Error_Status = FAILURE
              CALL Display_Message( PROGRAM_NAME, &
                                    'No water Vapor data in Atmosphere structure.', &
                                    Error_Status )
              STOP
            END IF

            Idx = PACK( (/ (j, j = 1, Atmosphere(m)%n_Absorbers ) /), &
                        Atmosphere(m)%Absorber_ID == H2O_ID )

            H2O_Idx = Idx(1)


            ! -- Ozone
            IF ( COUNT( Atmosphere(m)%Absorber_ID == O3_ID ) /= 1 ) THEN
              Error_Status = FAILURE
              CALL Display_Message( PROGRAM_NAME, &
                                    'No ozone data in Atmosphere structure.', &
                                    Error_Status )
              STOP
            END IF

            Idx = PACK( (/ (j, j = 1, Atmosphere(m)%n_Absorbers ) /), &
                        Atmosphere(m)%Absorber_ID == O3_ID )

            O3_Idx = Idx(1)


            ! -----------------------------
            ! Allocate Predictor Structures
            ! -----------------------------

            Error_Status = CRTM_Allocate_Predictor( Atmosphere(m)%n_Layers,  &  ! Input
                                                    MAX_N_PREDICTORS,        &  ! Input
                                                    MAX_N_ABSORBERS,         &  ! Input
                                                    Predictor                )  ! Output

            Error_Status_TL = CRTM_Allocate_Predictor( Atmosphere(m)%n_Layers,  &  ! Input
                                                       MAX_N_PREDICTORS,        &  ! Input
                                                       MAX_N_ABSORBERS,         &  ! Input
                                                       Predictor_TL             )  ! Output

            Error_Status_NL = CRTM_Allocate_Predictor( Atmosphere(m)%n_Layers,  &  ! Input
                                                       MAX_N_PREDICTORS,        &  ! Input
                                                       MAX_N_ABSORBERS,         &  ! Input
                                                       Predictor_NL             )  ! Output

            ! -- Test results
                IF ( Error_Status     /= SUCCESS   .OR. &
                     Error_Status_TL  /= SUCCESS   .OR. &
                     Error_Status_NL  /= SUCCESS     ) THEN
                  CALL Display_Message( PROGRAM_NAME, &
                                        'Error occured while allocating predictor structures', &
                                         Error_Status )
                  STOP
                END IF  

            ! ----------------------------------------------
            ! Calculate gas absorption model predictors for 
            ! AtmAbsorption baseline calculation
            ! ----------------------------------------------

            CALL CRTM_Compute_Predictors(  Atmosphere(m),     &  ! Input
                                           GeometryInfo,      &  ! Input
                                           Predictor,         &  ! Output
                                           APV                )  ! Output




            ! -----------------------------------------------------------------------------
            ! Allocate AtmAbsorption, CloudScatter, AerosolScatter and AtmOptics structures
            ! -----------------------------------------------------------------------------

            ! -- The baseline structure
            Error_Status = CRTM_Allocate_AtmAbsorption( Atmosphere(m)%n_Layers, &  ! Input
                                                        AtmAbsorption_Baseline  )  ! Output

            ! -- The tangent-linear structure
            Error_Status_TL = CRTM_Allocate_AtmAbsorption( Atmosphere(m)%n_Layers, &  ! Input
                                                           AtmAbsorption_TL        )  ! Output

            ! -- The non-linear structure
            Error_Status_NL = CRTM_Allocate_AtmAbsorption( Atmosphere(m)%n_Layers, &  ! Input
                                                           AtmAbsorption_NL        )  ! Output


            ! -- Test results
            IF ( Error_Status    /= SUCCESS .OR. &
                 Error_Status_NL /= SUCCESS .OR. &
                 Error_Status_TL /= SUCCESS      ) THEN
              CALL Display_Message( PROGRAM_NAME, &
                                    'Error occurred allocating AtmAbsorption structures', &
                                     Error_Status )                           
              STOP
            END IF



            ! -- The baseline structure
            Error_Status = CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers, &  ! Input
                                                     MAX_N_LEGENDRE_TERMS,   &  ! Input
                                                     MAX_N_PHASE_ELEMENTS,   &  ! Input
                                                     CloudScatter_Baseline   )  ! Output

            ! -- The non-linear structure
            Error_Status_NL = CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers, &  ! Input
                                                        MAX_N_LEGENDRE_TERMS,   &  ! Input
                                                        MAX_N_PHASE_ELEMENTS,   &  ! Input
                                                        CloudScatter_NL        )  ! Output

            ! -- The tangent-linear structure
            Error_Status_TL = CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers, &  ! Input
                                                        MAX_N_LEGENDRE_TERMS,   &  ! Input
                                                        MAX_N_PHASE_ELEMENTS,   &  ! Input
                                                        CloudScatter_TL        )  ! Output

            ! -- Test results
            IF ( Error_Status    /= SUCCESS .OR. &
                 Error_Status_TL /= SUCCESS .OR. &
                 Error_Status_NL /= SUCCESS      ) THEN
              CALL Display_Message( PROGRAM_NAME, &
                                    'Error occurred allocating CloudScatter structure', &
                                     Error_Status )                           
              STOP
            END IF


            ! -- The baseline structure
            Error_Status = CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers, &  ! Input
                                                     MAX_N_LEGENDRE_TERMS,   &  ! Input
                                                     MAX_N_PHASE_ELEMENTS,   &  ! Input
                                                     AerosolScatter_Baseline   )  ! Output

            ! -- The non-linear structure
            Error_Status_NL = CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers, &  ! Input
                                                        MAX_N_LEGENDRE_TERMS,   &  ! Input
                                                        MAX_N_PHASE_ELEMENTS,   &  ! Input
                                                        AerosolScatter_NL        )  ! Output

            ! -- The tangent-linear structure
            Error_Status_TL = CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers, &  ! Input
                                                        MAX_N_LEGENDRE_TERMS,   &  ! Input
                                                        MAX_N_PHASE_ELEMENTS,   &  ! Input
                                                        AerosolScatter_TL        )  ! Output

            ! -- Test results
            IF ( Error_Status    /= SUCCESS .OR. &
                 Error_Status_TL /= SUCCESS .OR. &
                 Error_Status_NL /= SUCCESS      ) THEN
              CALL Display_Message( PROGRAM_NAME, &
                                    'Error occurred allocating AerosolScatter structure', &
                                     Error_Status )                           
              STOP
            END IF



            ! -- The baseline structure
            Error_Status = CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers, &  ! Input
                                                     MAX_N_LEGENDRE_TERMS,   &  ! Input
                                                     MAX_N_PHASE_ELEMENTS,   &  ! Input
                                                     AtmOptics_Baseline      )  ! Output

            ! -- The non-linear structure
            Error_Status_NL = CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers, &  ! Input
                                                        MAX_N_LEGENDRE_TERMS,   &  ! Input
                                                        MAX_N_PHASE_ELEMENTS,   &  ! Input
                                                        AtmOptics_NL            )  ! Output

            ! -- The tangent-linear structure
            Error_Status_TL = CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers, &  ! Input
                                                        MAX_N_LEGENDRE_TERMS,   &  ! Input
                                                        MAX_N_PHASE_ELEMENTS,   &  ! Input
                                                        AtmOptics_TL            )  ! Output

            ! -- Test results
            IF ( Error_Status    /= SUCCESS .OR. &
                 Error_Status_NL /= SUCCESS .OR. &
                 Error_Status_TL /= SUCCESS      ) THEN
              CALL Display_Message( PROGRAM_NAME, &
                                    'Error occurred allocating AtmOptics structures', &
                                     Error_Status )                           
              STOP
            END IF




            ! ------------------------------------------
            ! Copy data into the TL atmosphere structure
            ! (to get the required dimensions) and zero
            ! ------------------------------------------

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
  
                    !#--------------------------------------------------------------------------#
                    !#                    -- LOOP OVER SENSOR CHANNELS --                       #
                    !#--------------------------------------------------------------------------#

                    Channel_Loop: DO l = 1, ChannelInfo%n_Channels

                      WRITE( *, '( 5x, "Channel: ", i5 )' ) ChannelInfo%Sensor_Channel(l)


                      ! ----------------------------------
                      ! Compute the baseline layer optical
                      ! depths due to gaseous absorption
                      ! ----------------------------------

                      CALL CRTM_Compute_AtmAbsorption( ChannelInfo%Channel_Index(l), &  ! Input
                                                                          Predictor, &
                                                             AtmAbsorption_Baseline  )  ! In/Output



                      ! ----------------------------------
                      ! Compute the baseline layer optical
                      ! depths due to Cloud Scatter
                      ! ----------------------------------
                      IF ( Cloud_Present ) THEN
                        Error_Status = CRTM_Compute_CloudScatter( Atmosphere(m),                &  ! Input
                                                                  ChannelInfo%Channel_Index(l), &  ! Input, scalar
                                                                  CloudScatter_Baseline,        &  ! In/Output
                                                                  CSV                           )  ! Internal variable output
                        IF ( Error_Status /= SUCCESS ) THEN
                          CALL Display_Message( PROGRAM_NAME, &
                                                'Error computing CloudScatter_Baseline', &
                                                Error_Status )
                          STOP
                        END IF
                      END IF

                      ! ----------------------------------
                      ! Compute the baseline layer optical
                      ! depths due to Aerosol Scatter
                      ! ----------------------------------
                      IF ( Aerosol_Present ) THEN
                        Error_Status = CRTM_Compute_AerosolScatter( Atmosphere(m),                &  ! Input
                                                                    GeometryInfo,                 &  ! Input
                                                                    ChannelInfo%Channel_Index(l), &  ! Input, scalar
                                                                    AerosolScatter_Baseline ) !,      &  ! In/Output
                                                                    !ASV                          )  ! Internal variable output
                        IF ( Error_Status /= SUCCESS ) THEN
                          CALL Display_Message( PROGRAM_NAME, &
                                                'Error computing AerosolScatter_Baseline', &
                                                Error_Status )
                          STOP
                        END IF
                      END IF

                      ! ------------------------------
                      ! Compute the baseline AtmOptics
                      ! ------------------------------
                      CALL CRTM_Combine_AtmOptics( AtmAbsorption_Baseline,  &  ! Input
                                                   CloudScatter_Baseline,   &  ! Input
                                                   AerosolScatter_Baseline, &  ! Input   
                                                   AtmOptics_Baseline,      &  ! In/Output
                                                   AOV                      )  ! Internal variable output


                              !#------------------------------------------------------------------------#
                              !#                     -- LOOP OVER INPUT VARIABLES --                    #
                              !#------------------------------------------------------------------------#

                              Input_Variable_Loop: DO nIV = 1, N_INPUT_VARIABLES

                                WRITE( *, '( 5x, "Perturbation variable: ", a )' ) INPUT_VARIABLE_NAME(nIV)


                                ! -----------------------------
                                ! Check for clouds and aerosols
                                ! -----------------------------
                                ! If we're processing cloud variables, but there
                                ! are no clouds, go to next input variable
                                IF ( (nIV >= NIV_CRE .AND. nIV <= NIV_CQ) .AND. (.NOT. Cloud_Present) ) CYCLE Input_Variable_Loop

                                ! If we're processing aerosol variables, but there
                                ! are none, go to next input variable
                                IF ( (nIV >= NIV_ARE .AND. nIV <= NIV_AC) .AND. (.NOT. Aerosol_Present) ) CYCLE Input_Variable_Loop



                                        !#----------------------------------------------------------------------#
                                        !#                  -- BEGIN THE PERTURBATION LOOP --                   #
                                        !#----------------------------------------------------------------------#

                                        Perturbation_Loop: DO nP = 1, N_PERTURBATIONS


                                          ! ----------------
                                          ! Begin layer loop
                                          ! ----------------

                                          Layer_Loop: DO k = 1, Atmosphere(m)%n_Layers


                                            ! --------------------------------
                                            ! Re-initialise all NL and TL data
                                            ! --------------------------------

                                            ! -- The forward model input data
                                            Error_Status = CRTM_Assign_Atmosphere( Atmosphere(m), &  ! Input
                                                                                   Atmosphere_NL  )  ! Output

                                            IF ( Error_Status /= SUCCESS ) THEN
                                              CALL DIsplay_Message( PROGRAM_NAME, &
                                                                    'Error reinitialising forward model Atmosphere input data', &
                                                                    Error_Status )
                                              STOP
                                            END IF

                                            ! -- The tangent-linear model input data
                                            CALL CRTM_Zero_Atmosphere( Atmosphere_TL )


                                            ! ------------------
                                            ! Perturb the inputs
                                            ! ------------------

                                            Input_Variable_Select: SELECT CASE ( nIV )

                                              ! -- Layer pressure
                                              CASE ( NIV_P )
                                                IF ( k == 1 ) THEN
                                                  Atmosphere_TL%Pressure(k) = PERTURBATION_FRACTION(nP) * &
                                                                              ( Atmosphere(m)%Pressure(k) - TOA_PRESSURE )
                                                ELSE
                                                  Atmosphere_TL%Pressure(k) = PERTURBATION_FRACTION(nP) * &
                                                                              ( Atmosphere(m)%Pressure(k) - Atmosphere(m)%Pressure(k-1) )
                                                END IF
                                                Atmosphere_NL%Pressure(k) = Atmosphere_NL%Pressure(k) + Atmosphere_TL%Pressure(k)


                                              ! -- Layer temperature
                                              CASE ( NIV_T )
                                                Atmosphere_TL%Temperature(k) = PERTURBATION_FRACTION(nP) * Atmosphere(m)%Temperature(k)
                                                Atmosphere_NL%Temperature(k) = Atmosphere_NL%Temperature(k) + Atmosphere_TL%Temperature(k)


                                              ! -- Water vapor 
                                              CASE ( NIV_W )
                                                Atmosphere_TL%Absorber(k,H2O_Idx) = PERTURBATION_FRACTION(nP) * Atmosphere(m)%Absorber(k,H2O_Idx)
                                                Atmosphere_NL%Absorber(k,H2O_Idx) = Atmosphere_NL%Absorber(k,H2O_Idx) + Atmosphere_TL%Absorber(k,H2O_Idx)


                                              ! -- Ozone
                                              CASE ( NIV_O )
                                                Atmosphere_TL%Absorber(k,O3_Idx) = PERTURBATION_FRACTION(nP) * Atmosphere(m)%Absorber(k,O3_Idx)
                                                Atmosphere_NL%Absorber(k,O3_Idx) = Atmosphere_NL%Absorber(k,O3_Idx) + Atmosphere_TL%Absorber(k,O3_Idx)


                                              ! -- Cloud effective radius
                                              CASE ( NIV_CRE )
                                                Atmosphere_TL%Cloud(mc)%Effective_Radius(k) = &
                                                  PERTURBATION_FRACTION(nP) * Atmosphere(m)%Cloud(mc)%Effective_Radius(k)

                                                Atmosphere_NL%Cloud(mc)%Effective_Radius(k) = &
                                                  Atmosphere_NL%Cloud(mc)%Effective_Radius(k) + &
                                                  Atmosphere_TL%Cloud(mc)%Effective_Radius(k)

                                              ! -- Cloud effective variance
                                              CASE ( NIV_CRV )
                                                Atmosphere_TL%Cloud(mc)%Effective_Variance(k) = &
                                                  PERTURBATION_FRACTION(nP) * Atmosphere(m)%Cloud(mc)%Effective_Variance(k)

                                                Atmosphere_NL%Cloud(mc)%Effective_Variance(k) = &
                                                  Atmosphere_NL%Cloud(mc)%Effective_Variance(k) + &
                                                  Atmosphere_TL%Cloud(mc)%Effective_Variance(k)

                                              ! -- Cloud water content
                                              CASE ( NIV_CQ )
                                                Atmosphere_TL%Cloud(mc)%Water_Content(k) = &
                                                  PERTURBATION_FRACTION(nP) * Atmosphere(m)%Cloud(mc)%Water_Content(k)

                                                Atmosphere_NL%Cloud(mc)%Water_Content(k) = &
                                                  Atmosphere_NL%Cloud(mc)%Water_Content(k) + &
                                                  Atmosphere_TL%Cloud(mc)%Water_Content(k)

                                              ! -- Aerosol effective radius
                                              CASE ( NIV_ARE )
                                                Atmosphere_TL%Aerosol(ma)%Effective_Radius(k,mam) = &
                                                  PERTURBATION_FRACTION(nP)*Atmosphere(m)%Aerosol(ma)%Effective_Radius(k,mam)

                                                Atmosphere_NL%Aerosol(ma)%Effective_Radius(k,mam) = &
                                                  Atmosphere_NL%Aerosol(ma)%Effective_Radius(k,mam) + &
                                                  Atmosphere_TL%Aerosol(ma)%Effective_Radius(k,mam)

                                              ! -- Aerosol effective variance
                                              CASE ( NIV_ARV )
                                                Atmosphere_TL%Aerosol(ma)%Effective_Variance(k,mam) = &
                                                  PERTURBATION_FRACTION(nP) * Atmosphere(m)%Aerosol(ma)%Effective_Variance(k,mam)

                                                Atmosphere_NL%Aerosol(ma)%Effective_Variance(k,mam) = &
                                                  Atmosphere_NL%Aerosol(ma)%Effective_Variance(k,mam) + &
                                                  Atmosphere_TL%Aerosol(ma)%Effective_Variance(k,mam)

                                              ! -- Aerosol Concentration
                                              CASE ( NIV_AC )
                                                Atmosphere_TL%Aerosol(ma)%Concentration(k,mam) = &
                                                  PERTURBATION_FRACTION(nP) * Atmosphere(m)%Aerosol(ma)%Concentration(k,mam)

                                                Atmosphere_NL%Aerosol(ma)%Concentration(k,mam) = &
                                                  Atmosphere_NL%Aerosol(ma)%Concentration(k,mam) + &
                                                  Atmosphere_TL%Aerosol(ma)%Concentration(k,mam)

                                            END SELECT Input_Variable_Select


                                            ! -----------------------------------------------
                                            ! Calculate TL/NL gas absorption model predictors
                                            ! -----------------------------------------------

                                            CALL CRTM_Compute_Predictors( Atmosphere_NL,   &  ! Input
                                                                          GeometryInfo,    &  ! Input
                                                                          Predictor_NL,    &  ! Output
                                                                          APV              )  ! Output

                                            CALL CRTM_Compute_Predictors_TL( Atmosphere(m),    &  ! FWD Input
                                                                             Predictor,        &  ! FWD Input
                                                                             Atmosphere_TL,    &  ! TL Input
                                                                             GeometryInfo,     &  ! Input
                                                                             Predictor_TL,     &  ! TL Output
                                                                             APV               )  ! Internal Variable Input

                                            ! -----------------------------------
                                            ! Compute the perturbed layer optical
                                            ! depths due to gaseous absorption
                                            ! -----------------------------------

                                            ! -- The perturbated forward model
                                            CALL CRTM_Compute_AtmAbsorption( ChannelInfo%Channel_Index(l), &  ! Input
                                                                                             Predictor_NL, &  ! Input
                                                                                         AtmAbsorption_NL  )  ! In/Output

                                            ! -- The tangent-linear form
                                            CALL CRTM_Compute_AtmAbsorption_TL( ChannelInfo%Channel_Index(l), &  ! Input
                                                                                                   Predictor, &  ! Input
                                                                                                Predictor_TL, &  ! Input
                                                                                             AtmAbsorption_TL )  ! In/Output




                                            ! -------------------------------------------------
                                            ! Compute the perturbed cloud scattering properties
                                            ! -------------------------------------------------

                                            IF ( Cloud_Present ) THEN
                                              ! -- The perturbated forward model
                                              Error_Status_NL = CRTM_Compute_CloudScatter( Atmosphere_NL,                &  ! Input
                                                                                           ChannelInfo%Channel_Index(l), &  ! Input
                                                                                           CloudScatter_NL,              &  ! In/Output
                                                                                           CSV_Dummy                     )  ! Internal variable output
                                              ! -- The tangent-linear form
                                              Error_Status_TL = CRTM_Compute_CloudScatter_TL( Atmosphere(m),                &  ! Input
                                                                                              CloudScatter_Baseline,        &  ! Input
                                                                                              Atmosphere_TL,                &  ! Input
                                                                                              ChannelInfo%Channel_Index(l), &  ! Input
                                                                                              CloudScatter_TL,              &  ! In/Output
                                                                                              CSV                           )  ! Internal variable input
                                              ! -- Test results
                                              IF ( Error_Status_NL /= SUCCESS .OR. &
                                                   Error_Status_TL /= SUCCESS      ) THEN
                                                CALL Display_Message( PROGRAM_NAME, &
                                                                      'Error computing CloudScatter NL/TL', &
                                                                       Error_Status )                           
                                                STOP
                                              END IF
                                            END IF

                                            ! -------------------------------------------------
                                            ! Compute the perturbed aerosol scattering properties
                                            ! -------------------------------------------------

                                            IF ( Aerosol_Present ) THEN 
                                              ! -- The perturbated forward model
                                              Error_Status_NL = CRTM_Compute_AerosolScatter( Atmosphere_NL,                &  ! Input
                                                                                             GeometryInfo,                 &  ! Input
                                                                                             ChannelInfo%Channel_Index(l), &  ! Input
                                                                                             AerosolScatter_NL     )!,     &  ! In/Output
                                                                                             !ASV_Dummy                     )  ! Internal variable output
                                              ! -- The tangent-linear form
                                              Error_Status_TL = CRTM_Compute_AerosolScatter_TL( Atmosphere(m),                &  ! Input
                                                                                                AerosolScatter_Baseline,      &  ! Input
                                                                                                Atmosphere_TL,                &  ! Input
                                                                                                GeometryInfo,                 &  ! Input
                                                                                                ChannelInfo%Channel_Index(l), &  ! Input
                                                                                                AerosolScatter_TL  )!,        &  ! In/Output
                                                                                                !ASV                           )  ! Internal variable input
                                              ! -- Test results
                                              IF ( Error_Status_NL /= SUCCESS .OR. &
                                                   Error_Status_TL /= SUCCESS      ) THEN
                                                CALL Display_Message( PROGRAM_NAME, &
                                                                      'Error computing AerosolScatter NL/TL', &
                                                                       Error_Status )                           
                                                STOP
                                              END IF
                                            END IF


                                            ! ------------------------------------
                                            ! Compute the perturbed AtmOptics data
                                            ! ------------------------------------

                                            ! -- The perturbated forward model
                                            CALL CRTM_Combine_AtmOptics( AtmAbsorption_NL,  &  ! Input
                                                                         CloudScatter_NL,   &  ! Input                    
                                                                         AerosolScatter_NL, &  ! Input                    
                                                                         AtmOptics_NL,      &  ! Output                   
                                                                         AOV_Dummy          )  ! Internal variable output 

                                            ! -- The tangent-linear form
                                            CALL CRTM_Combine_AtmOptics_TL( AtmAbsorption_Baseline,  &  ! FWD Input
                                                                            CloudScatter_Baseline,   &  ! FWD Input
                                                                            AerosolScatter_Baseline, &  ! FWD Input
                                                                            AtmOptics_Baseline,      &  ! FWD Input
                                                                            AtmAbsorption_TL,        &  ! TL Input
                                                                            CloudScatter_TL,         &  ! TL Input
                                                                            AerosolScatter_TL,       &  ! TL Input
                                                                            AtmOptics_TL,            &  ! TL Output
                                                                            AOV                      )  ! Internal variable input


                                            ! ------------------------------
                                            ! Save the data for this profile
                                            ! ------------------------------

                                            ComponentTest%d1(k,l,nP,nIV,NOV_TAU) = AtmOptics_NL%Optical_Depth(k) - &
                                                                                   AtmOptics_Baseline%Optical_Depth(k)
                                            ComponentTest%d2(k,l,nP,nIV,NOV_TAU) = AtmOptics_TL%Optical_Depth(k)

                                            ComponentTest%d1(k,l,nP,nIV,NOV_OMEGA) = AtmOptics_NL%Single_Scatter_Albedo(k) - &
                                                                                     AtmOptics_Baseline%Single_Scatter_Albedo(k)
                                            ComponentTest%d2(k,l,nP,nIV,NOV_OMEGA) = AtmOptics_TL%Single_Scatter_Albedo(k)

                                            ComponentTest%d1(k,l,nP,nIV,NOV_G) = AtmOptics_NL%Asymmetry_Factor(k) - &
                                                                                 AtmOptics_Baseline%Asymmetry_Factor(k)
                                            ComponentTest%d2(k,l,nP,nIV,NOV_G) = AtmOptics_TL%Asymmetry_Factor(k)

                                            ComponentTest%d1(k,l,nP,nIV,NOV_D) = AtmOptics_NL%Delta_Truncation(k) - &
                                                                                 AtmOptics_Baseline%Delta_Truncation(k)
                                            ComponentTest%d2(k,l,nP,nIV,NOV_D) = AtmOptics_TL%Delta_Truncation(k)


                                           !---------------------------
                                           ! Loop through the LEGENDREE
                                           ! terms. This loop will be
                                           ! implemented next week.
                                           !---------------------------

                                !            DO nLT = 0, MAX_N_LEGENDRE_TERMS
                                !              nOV = NOV_PC_L0 + nLT  ! Offset index into output arrays
                                !              ComponentTest%d1(k,l,nP,nIV,NOV_P) = AtmOptics_NL%Phase_Coefficient(nLT,1,k) - &
                                !                                                   AtmOptics_Baseline%Phase_Coefficient(nLT,1,k)
                                !              ComponentTest%d2(k,l,nP,nIV,NOV_P) = AtmOpics_TL%Phase_Coefficient(nLT,1,k) 
                                !            END DO

                                            ComponentTest%d1(k,l,nP,nIV,NOV_P) = AtmOptics_NL%Phase_Coefficient(1,1,k) - &
                                                                                 AtmOptics_Baseline%Phase_Coefficient(1,1,k)
                                            ComponentTest%d2(k,l,nP,nIV,NOV_P) = AtmOptics_TL%Phase_Coefficient(1,1,k)

                                          END DO Layer_Loop
                                        END DO Perturbation_Loop
                                      END DO Input_Variable_Loop
                                    END DO Channel_Loop



            !#--------------------------------------------------------------------------#
            !#      -- WRITE THE CURRENT DATASET ComponentTest STRUCTURE TO FILE --     #
            !#--------------------------------------------------------------------------#

            WRITE( *, '( 10x, "Writing FWD/TL data to output file...." )' )


            ! ------------------------------------------------------
            ! Set the current dataset number in the output structure
            ! ------------------------------------------------------

            ComponentTest%nM = m
            WRITE( ComponentTest%nM_Name, '( "Profile # ", i3 )' ) m


            ! --------------
            ! Write the data
            ! --------------

            Error_Status = Write_ComponentTest_netCDF( TRIM( ComponentTest_File ), &
                                                       ComponentTest, &
                                                       New_File      = New_File, &
                                                       Title         = 'FWD/TL CRTM AtmOptics test results for '//&
                                                                       TRIM( File_Prefix ), &
                                                       History       = PROGRAM_RCS_ID, &
                                                       Sensor_Name   = TRIM( File_Prefix ), &
                                                       Platform_Name = TRIM( File_Prefix ), &
                                                       Comment       = 'Compact-OPTRAN AtmOptics algorithm.', &
                                                       ID_Tag        = 'ECMWF' )

            IF ( Error_Status /= SUCCESS ) THEN 
              WRITE( Message, '( "Error writing ComponentTest structure for profile #", i5, " to ", a )' ) &
                              m, TRIM( ComponentTest_File )
              CALL Display_Message( PROGRAM_NAME, &
                                    TRIM( Message ), &
                                    Error_Status )                           
              STOP
            END IF


            ! -----------------------
            ! Reset the new file flag
            ! -----------------------

            IF ( New_File == 1 ) New_File = 0



            !#--------------------------------------------------------------------------#
            !#              -- DEALLOCATE STRUCTURES FOR CURRENT PROFILE --             #
            !#--------------------------------------------------------------------------#

            ! ----------------------------
            ! The AtmAbsorption structures
            ! ----------------------------

            Error_Status_NL = CRTM_Destroy_AtmAbsorption( AtmAbsorption_NL )
            Error_Status_TL = CRTM_Destroy_AtmAbsorption( AtmAbsorption_TL )
            Error_Status    = CRTM_Destroy_AtmAbsorption( AtmAbsorption_Baseline )

            IF ( Error_Status    /= SUCCESS .OR. &
                 Error_Status_TL /= SUCCESS .OR. &
                 Error_Status_NL /= SUCCESS      ) THEN
              CALL Display_Message( PROGRAM_NAME, &
                                    'Error deallocating AtmAbsorption structures', &
                                    Error_Status )
              STOP
            END IF



            ! ---------------------------
            ! The CloudScatter structures
            ! ---------------------------

            Error_Status_NL = CRTM_Destroy_AtmScatter( CloudScatter_NL )
            Error_Status_TL = CRTM_Destroy_AtmScatter( CloudScatter_TL )
            Error_Status    = CRTM_Destroy_AtmScatter( CloudScatter_Baseline )

            IF ( Error_Status    /= SUCCESS .OR. &
                 Error_Status_TL /= SUCCESS .OR. &
                 Error_Status_NL /= SUCCESS      ) THEN
              CALL Display_Message( PROGRAM_NAME, &
                                    'Error deallocating CloudScatter structures', &
                                    Error_Status )
              STOP
            END IF


            ! -----------------------------
            ! The AerosolScatter structures
            ! -----------------------------

            Error_Status_NL = CRTM_Destroy_AtmScatter( AerosolScatter_NL )
            Error_Status_TL = CRTM_Destroy_AtmScatter( AerosolScatter_TL )
            Error_Status    = CRTM_Destroy_AtmScatter( AerosolScatter_Baseline )

            IF ( Error_Status    /= SUCCESS .OR. &
                 Error_Status_TL /= SUCCESS .OR. &
                 Error_Status_NL /= SUCCESS      ) THEN
              CALL Display_Message( PROGRAM_NAME, &
                                    'Error deallocating AerosolScatter structures', &
                                    Error_Status )
              STOP
            END IF


            ! ------------------------
            ! The AtmOptics structures
            ! ------------------------

            Error_Status_NL = CRTM_Destroy_AtmScatter( AtmOptics_NL )
            Error_Status_TL = CRTM_Destroy_AtmScatter( AtmOptics_TL )
            Error_Status    = CRTM_Destroy_AtmScatter( AtmOptics_Baseline )

            IF ( Error_Status    /= SUCCESS .OR. &
                 Error_Status_TL /= SUCCESS .OR. &
                 Error_Status_NL /= SUCCESS      ) THEN
              CALL Display_Message( PROGRAM_NAME, &
                                    'Error deallocating AtmOptics structures', &
                                    Error_Status )
              STOP
            END IF

            ! ------------------------
            ! The Predictor Structures
            ! ------------------------

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


            ! ------------------------------------
            ! The individual Atmosphere structures
            ! ------------------------------------

            Error_Status = CRTM_Destroy_Atmosphere( Atmosphere_TL, Atmosphere_NL )

            IF ( Error_Status /= SUCCESS ) THEN
              CALL Display_Message( PROGRAM_NAME, &
                                    'Error deallocating Atmosphere TL/NL structures', &
                                    Error_Status )
              STOP
            END IF

          END DO Profile_Loop



  !#----------------------------------------------------------------------------#
  !#                    -- DESTROY ComponentTest STRUCTURE --                   #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_ComponentTest( ComponentTest )

  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error occurred destroying ComponentTest for '//&
                           TRIM( File_Prefix ), &
                            Error_Status )                           
   STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                           -- DESTROY THE CRTM --                           #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Destroying the CRTM..." )' )

  Error_Status = CRTM_Destroy( ChannelInfo )

  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying CRTM', & 
                           WARNING )
  END IF



  !#----------------------------------------------------------------------------#
  !#                   -- DEALLOCATE Atmosphere STRUCTURES --                   #
  !#----------------------------------------------------------------------------#

  Error_Status = CRTM_Destroy_Atmosphere( Atmosphere )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error deallocating Atmosphere structure array', &
                          WARNING )
  END IF

END PROGRAM Test_Tangent_Linear
