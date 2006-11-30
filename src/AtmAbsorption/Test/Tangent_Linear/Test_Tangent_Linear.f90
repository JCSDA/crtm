
!  Program to test the CRTM AtmAbsorption Tangent-Linear code.
!
!  Written by:     Paul van Delst, CIMSS/SSEC 21-Jan-2005
!                  paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2005 Paul van Delst
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

PROGRAM Test_Tangent_Linear


  ! ------------
  ! Module usage
  ! ------------

  USE CRTM_Predictor
  USE Type_Kinds 
  USE Message_Handler
  USE CRTM_Parameters
  USE CRTM_LifeCycle
  USE CRTM_GeometryInfo_Define
  USE CRTM_ChannelInfo_Define
  USE CRTM_Atmosphere_Define
  USE CRTM_Atmosphere_Binary_IO
  USE CRTM_AtmAbsorption
  USE ComponentTest_netCDF_IO
  USE Compare_Float_Numbers
  USE CRTM_Predictor_Define
  
  


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


 ! LOGICAL, EXTERNAL :: Max_Iterations

  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME   = 'Test_Tangent_Linear'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: Test_Tangent_Linear.f90,v 1.14 2006/11/22 15:55:40 dgroff Exp $'
  CHARACTER( * ),  PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  INTEGER, PARAMETER :: SL = 512

  CHARACTER(*), PARAMETER :: ATMOSPHERE_FILENAME = 'ECMWF-Atmosphere.Cloud.Aerosol.bin'
  INTEGER,      PARAMETER :: N_PROFILES = 52
  INTEGER,      PARAMETER :: N_Perturbations = 31
  INTEGER,      PARAMETER :: Zero_Pert = 16
  

  REAL(fp_kind), PARAMETER :: Max_DeltaX=0.100_fp_kind
  
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

  INTEGER, PARAMETER :: N_OUTPUT_VARIABLES = 1
  INTEGER, PARAMETER :: NOV_TAU = 1  ! Optical depth
  CHARACTER( * ), PARAMETER, DIMENSION( N_OUTPUT_VARIABLES ) :: &
    OUTPUT_VARIABLE_NAME = (/ 'Optical depth' /)
  CHARACTER( * ), PARAMETER, DIMENSION( N_OUTPUT_VARIABLES ) :: &
    OUTPUT_VARIABLE_UNITS = (/ 'Unitless' /)


  ! ---------
  ! Variables
  ! ---------


  INTEGER :: pn_pos
  
  CHARACTER( 80 ) :: pn_fmt

  CHARACTER( 256 ) :: Message

  INTEGER :: Error_Status
  INTEGER :: Error_Status_TL
  INTEGER :: Error_Status_NL
  INTEGER :: Error_Status_Neg_NL
  INTEGER :: Error_Status_Neg_TL
  INTEGER :: Error_Status_Pos_TL
  INTEGER :: Error_Status_Pos_NL

  CHARACTER( SL ) :: File_Prefix
  CHARACTER( SL ) :: SpcCoeff_File
  CHARACTER( SL ) :: TauCoeff_File
  CHARACTER( SL ) :: AerosolCoeff_File
  CHARACTER( SL ) :: CloudCoeff_File
  CHARACTER( SL ) :: EmisCoeff_File
  CHARACTER( SL ) :: ComponentTest_File
  INTEGER :: New_File

  INTEGER, DIMENSION( 1 ) :: Idx
  INTEGER :: H2O_Idx
  INTEGER :: O3_Idx

  INTEGER :: j, k, l, m, n, nP, nIV, switch, pL, pS, &
             pLcounter, counter
  
  ! Local Variables
  REAL(fp_kind) :: DeltaX  
  REAL(fp_kind) :: Perturbation_Used 
  REAL(fp_kind) :: Positive_TL
  REAL(fp_kind) :: Negative_TL
  REAL(fp_kind) :: Positive_NL
  REAL(fp_kind) :: Negative_NL
  REAL(fp_kind) :: Linear_Test
  REAL(fp_kind) :: switchmultiplier
  REAL(fp_kind) :: Fill_multiplier
  
  REAL(fp_kind) :: Perturbation_Factor
  
  REAL(fp_kind), DIMENSION(N_Perturbations) :: PERTURBATION_FRACTION

  INTEGER, PARAMETER :: Max_Delta_Iter = 15
  INTEGER, PARAMETER :: kind_number = SELECTED_INT_KIND(12)

  INTEGER( Long ), PARAMETER :: ULP = 900000000

  LOGICAL :: NLTL_PosGradientEqual
  LOGICAL :: NLTL_NegGradientEqual
  LOGICAL :: TL_GradientEqual
  LOGICAL :: FatalFailure
  

  

  TYPE( CRTM_Predictor_type ) :: Predictor, Predictor_NL, Predictor_TL
  TYPE( CRTM_ChannelInfo_type )   :: ChannelInfo
  TYPE( CRTM_GeometryInfo_type )  :: GeometryInfo
  TYPE( CRTM_AtmAbsorption_type ) :: AtmAbsorption_Baseline, &
                                     AtmAbsorption_NL, AtmAbsorption_TL, &
                                     AtmAbsorption_Pos_NL, AtmAbsorption_Pos_TL, &
                                     AtmAbsorption_Neg_NL, AtmAbsorption_Neg_TL
  TYPE( CRTM_Atmosphere_type ), DIMENSION( N_PROFILES ) :: Atmosphere
  TYPE( CRTM_Atmosphere_type )                          :: Atmosphere_NL, Atmosphere_TL

  TYPE( ComponentTest_type ) :: ComponentTest
  TYPE( CRTM_APVariables_type ) :: APV
  


  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a)' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to test the CRTM AtmAbsorption Tangent-Linear")' )
  WRITE( *, '( 5x, "   components with respect to the Forward components.")' )
  WRITE( *, '(/5x, " $Revision: 1.14 $")' )
  WRITE( *, '( 5x, a, /)' ) PROGRAM_HEADER



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

  ComponentTest_File   = TRIM( File_Prefix )//'.CRTM_AtmAbsorption.ComponentTest.nc'

  New_File = 1



  !#----------------------------------------------------------------------------#
  !#                          -- INITIALISE THE CRTM --                         #
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
                                         N_Perturbations, &
                                         N_INPUT_VARIABLES, &
                                         N_OUTPUT_VARIABLES, &
                                         ComponentTest )

  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error occurred allocating ComponentTest', &
                           Error_Status )                           
    STOP
  END IF

  
  ! Fill the zero perturbation fraction position. 
  ! The other positions will be filled in the perturbation loop.
  PERTURBATION_FRACTION(Zero_Pert) = 0.00_fp_kind
    
  ! ----------------------------
  ! Assign data to ComponentTest
  ! ----------------------------

  ComponentTest%TestType = COMPONENTTEST_FWDTL_TESTTYPE
  ComponentTest%DataType = COMPONENTTEST_POLY_DATATYPE

  ComponentTest%Pressure      = Atmosphere(1)%Pressure
  ComponentTest%Spectral      = ChannelInfo%Sensor_Channel


  ComponentTest%Input_Variable_Name  = INPUT_VARIABLE_NAME
  ComponentTest%Input_Variable_Units = INPUT_VARIABLE_UNITS

  ComponentTest%Output_Variable_Name  = OUTPUT_VARIABLE_NAME
  ComponentTest%Output_Variable_Units = OUTPUT_VARIABLE_UNITS


      ! Loop over Atmospheric Profiles
      Profile_Loop: DO m = 1, N_PROFILES

        WRITE( *, '( 5x, "Processing profile # ", i3 )' ) m


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

        Error_Status = CRTM_Allocate_Predictor( Atmosphere(m)%n_Layers, &  ! Input
                                                MAX_N_PREDICTORS,       &  ! Input
                                                MAX_N_ABSORBERS,        &  ! Input 
                                                Predictor               )  ! Output

        Error_Status_TL = CRTM_Allocate_Predictor( Atmosphere(m)%n_Layers, &  ! Input
                                                   MAX_N_PREDICTORS,       &  ! Input
                                                   MAX_N_ABSORBERS,        &  ! Input
                                                   Predictor_TL            )  ! Output

        Error_Status_NL = CRTM_Allocate_Predictor( Atmosphere(m)%n_Layers, &  ! Input
                                                   MAX_N_PREDICTORS,       &  ! Input
                                                   MAX_N_ABSORBERS,        &  ! Input
                                                   Predictor_NL            )  ! Output

        ! -- Test results
        IF ( Error_Status     /= SUCCESS   .OR. &
             Error_Status_TL  /= SUCCESS   .OR. &
             Error_Status_NL  /= SUCCESS     ) THEN
          CALL Display_Message( PROGRAM_NAME, &
                                'Error occured while allocating predictor structures', &
                                 Error_Status )
          STOP
        END IF  

        ! ------------------------------------------------------------------
        ! Calculate gas absorption model predictors for baseline calculation
        ! ------------------------------------------------------------------
        
        CALL CRTM_Compute_Predictors(  Atmosphere(m),     &  ! Input
                                       GeometryInfo,      &  ! Input
                                       Predictor,         &  ! Output
                                       APV                )  ! Output   

        ! ---------------------------------
        ! Allocate AtmAbsorption structures
        ! ---------------------------------

        ! -- The baseline structure
        Error_Status = CRTM_Allocate_AtmAbsorption( Atmosphere(m)%n_Layers, &  ! Input
                                                    AtmAbsorption_Baseline  )  ! Input
                                                   
        ! -- The tangent-linear structure
        Error_Status_TL = CRTM_Allocate_AtmAbsorption( Atmosphere(m)%n_Layers, &  ! Input
                                                       AtmAbsorption_TL        )  ! Output

        ! -- The non-linear structure
        Error_Status_NL = CRTM_Allocate_AtmAbsorption( Atmosphere(m)%n_Layers, &  ! Input
                                                       AtmAbsorption_NL        )  ! Output

        ! -- The positive non-linear structure used for storing AtmAbsorption_NL
        ! -- depending on the state of the switch variable
        Error_Status_Neg_TL = CRTM_Allocate_AtmAbsorption( Atmosphere(m)%n_Layers, &  ! Input
                                                           AtmAbsorption_Neg_TL    )  ! Output

        ! -- The negative non-linear structure used for storing AtmAbsorption_NL
        ! -- depending on the state of the switch variable
        Error_Status_Neg_NL = CRTM_Allocate_AtmAbsorption( Atmosphere(m)%n_Layers, &  ! Input
                                                           AtmAbsorption_Neg_NL    )  ! Output

        ! -- The positive tangent-linear structure used for storing AtmAbsorption_TL
        ! -- depending on the state of the switch variable
        Error_Status_Pos_TL = CRTM_Allocate_AtmAbsorption( Atmosphere(m)%n_Layers, &  ! Input
                                                           AtmAbsorption_Pos_TL    )  ! Output

        ! -- The negative tangent-linear structure used for storing AtmAbsorption
        ! -- depending on the state of the switch variable
        Error_Status_Pos_NL = CRTM_Allocate_AtmAbsorption( Atmosphere(m)%n_Layers, &  ! Input
                                                           AtmAbsorption_Pos_NL    )  ! Output
        ! -- Test results
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


        ! Initialise logical flags
        FatalFailure=.FALSE.


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
                                                                Predictor, &  ! Input
                                                   AtmAbsorption_Baseline  )  ! In/Output

            IF ( Error_Status /= SUCCESS ) THEN
              CALL DIsplay_Message( PROGRAM_NAME, &
                                    'Error computing AtmAbsorption_Baseline', &
                                    Error_Status )
              STOP
            END IF


              
              ! Loop Over Input Variables
              Input_Variable_Loop: DO nIV = 1, N_INPUT_VARIABLES

                WRITE( *, '( 5x, "Perturbation variable: ", a )' ) INPUT_VARIABLE_NAME(nIV)

                


               ! ----------------
               ! Begin layer loop
               ! ----------------

               1 CONTINUE
                 Layer_Loop: DO k = 1, Atmosphere(m)%n_Layers

                   ! Imitialise the perturbation delta    
                   DeltaX = Max_DeltaX
                  

                   ! Initialize the perturbation counter
                   nP = 0



                  

                     ! Begin Perturbation Loop       
                     Perturbation_Loop: DO

                       ! Increment perturbation counter
                       nP=nP+1

                       !#--------------------------------------------------------------------------------#
                       !# -- BEGIN THE SWITCH LOOP (Need to calc AtmAbsorption for Neg and Pos Pert). -- #
                       !#--------------------------------------------------------------------------------#

                       
                         Switch_Loop: DO switch = -1, 1, 2



                           ! Calculate the Perturbation_Used by multiplying the switch and current DeltaX
                           switchmultiplier=REAL(switch,fp_kind)

                           Perturbation_Used = switchmultiplier*DeltaX

                           ! Fill Perturbation fraction array which will be used to fill the perturbations for componenttest
                           IF ( switch == -1 ) THEN
                             PERTURBATION_FRACTION(nP) = Perturbation_Used
                           END IF
                           IF ( switch == 1 ) THEN 
                             PERTURBATION_FRACTION((N_Perturbations + 1) - nP) = Perturbation_Used
                           END IF
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
                                  Atmosphere_TL%Pressure(k) = Perturbation_Used * &
                                                              ( Atmosphere(m)%Pressure(k) - TOA_PRESSURE )
                                ELSE
                                  Atmosphere_TL%Pressure(k) = Perturbation_Used * &
                                                              ( Atmosphere(m)%Pressure(k) - Atmosphere(m)%Pressure(k-1) )
                                END IF

                                Atmosphere_NL%Pressure(k) = Atmosphere_NL%Pressure(k) + Atmosphere_TL%Pressure(k)

                                Linear_Test=Atmosphere_TL%Pressure(k)

                              ! -- Layer temperature
                              CASE ( NIV_T )
                                Atmosphere_TL%Temperature(k) = Perturbation_Used * Atmosphere(m)%Temperature(k)
                                Atmosphere_NL%Temperature(k) = Atmosphere_NL%Temperature(k) + Atmosphere_TL%Temperature(k)

                                Linear_Test=Atmosphere_TL%Temperature(k)

                              CASE ( NIV_W )
                                Atmosphere_TL%Absorber(k,H2O_Idx) = Perturbation_Used * Atmosphere(m)%Absorber(k,H2O_Idx)
                                Atmosphere_NL%Absorber(k,H2O_Idx) = Atmosphere_NL%Absorber(k,H2O_Idx) + Atmosphere_TL%Absorber(k,H2O_Idx)

                                Linear_Test=Atmosphere_TL%Absorber(k,H2O_Idx)            

                              CASE ( NIV_O )
                                Atmosphere_TL%Absorber(k,O3_Idx) = Perturbation_Used * Atmosphere(m)%Absorber(k,O3_Idx)
                                Atmosphere_NL%Absorber(k,O3_Idx) = Atmosphere_NL%Absorber(k,O3_Idx) + Atmosphere_TL%Absorber(k,O3_Idx)

                                Linear_Test=Atmosphere_TL%Absorber(k,O3_Idx)
                                

                           END SELECT Input_Variable_Select


                           ! -----------------------------------------------
                           ! Calculate TL/NL gas absorption model predictors
                           ! -----------------------------------------------
        
         
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

                           

                           

                           ! AtmAbsorption calc
                           ! Need to fill Predictor and Predictor_TL using subroutines in CRTM_Predictor.f90
                           CALL CRTM_Compute_AtmAbsorption( ChannelInfo%Channel_Index(l),     &  ! Input
                                                                            Predictor_NL,     &  ! Input 
                                                                         AtmAbsorption_NL     )  ! In/Output
                           CALL CRTM_Compute_AtmAbsorption_TL( ChannelInfo%Channel_Index(l),          &  ! Input
                                                                                  Predictor,          &  ! Input
                                                                               Predictor_TL,          &  ! Input
                                                                            AtmAbsorption_TL          )  ! In/Output
                           
                       
                           
                           ! Save results
                           IF (switch==-1) THEN
                             Error_Status = CRTM_Assign_AtmAbsorption( AtmAbsorption_NL,      &
                                                                       AtmAbsorption_Neg_NL   )

                             IF ( Error_Status /= SUCCESS ) THEN
                                CALL Display_Message( PROGRAM_NAME, &
                                                      'Error assigning Atmosphere TL/NL structures', &
                                                       Error_Status )
                               STOP
                             END IF

                             Error_Status = CRTM_Assign_AtmAbsorption( AtmAbsorption_TL,      &
                                                                       AtmAbsorption_Neg_TL   )

                             IF ( Error_Status /= SUCCESS ) THEN
                                CALL Display_Message( PROGRAM_NAME, &
                                                      'Error assigning Atmosphere TL/NL structures', &
                                                       Error_Status )
                               STOP
                             END IF

                           ELSE 
                             Error_Status = CRTM_Assign_AtmAbsorption( AtmAbsorption_NL,      &
                                                                       AtmAbsorption_Pos_NL   )

                             IF ( Error_Status /= SUCCESS ) THEN
                                CALL Display_Message( PROGRAM_NAME, &
                                                      'Error assigning Atmosphere TL/NL structures', &
                                                       Error_Status )
                               STOP
                             END IF

                             Error_Status = CRTM_Assign_AtmAbsorption( AtmAbsorption_TL,      &
                                                                       AtmAbsorption_Pos_TL   )

                             IF ( Error_Status /= SUCCESS ) THEN
                                CALL Display_Message( PROGRAM_NAME, &
                                                      'Error assigning Atmosphere TL/NL structures', &
                                                       Error_Status )
                               STOP
                             END IF
                             

                           END IF
                         
                           
                         END DO Switch_Loop  

                      
                       ! ----------------------------------------------
                       ! CALCULATE THE GRADIENTS AND TEST BY COMPARISON
                       ! IF A FATAL FAILURE HAS NOT OCCURED
                       ! ----------------------------------------------

                       IF ( .NOT.FatalFailure ) THEN

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
                         IF ( TL_GradientEqual .AND. NLTL_NegGradientEqual .AND. NLTL_PosGradientEqual ) THEN
                        
                             CYCLE Layer_Loop
                           ! If the test has not passed multiply DeltaX by 0.5
                           ELSE
                             CALL Get_Next_DeltaX(DeltaX)
                           ! Test to see if np==Max_Iterations
                           IF (Max_Iterations( nP )) THEN
                             FatalFailure = .TRUE.
                              IF (nIV==4 .AND. l==10) THEN
                              END IF
                             GO TO 1       
                           END IF        
                             
                         END IF



                       ELSE 
                       ! -------------------------------------------
                       ! Save the data for this profile if necessary
                       ! The data saved 
                       ! -------------------------------------------

                         ComponentTest%d1(k,l,nP,nIV,NOV_TAU) = ((AtmAbsorption_Neg_NL%Optical_Depth(k) - &
                                                                 AtmAbsorption_Baseline%Optical_Depth(k)))
                         ComponentTest%d2(k,l,nP,nIV,NOV_TAU) = (AtmAbsorption_Neg_TL%Optical_Depth(k))


                         ComponentTest%d1(k,l,((N_Perturbations + 1) - nP),nIV,NOV_TAU) = (AtmAbsorption_Pos_NL%Optical_Depth(k) - &
                                                                                            AtmAbsorption_Baseline%Optical_Depth(k))
                         ComponentTest%d2(k,l,((N_Perturbations + 1) - nP),nIV,NOV_TAU) = AtmAbsorption_Pos_TL%Optical_Depth(k)

                         ComponentTest%d1(k,l,Zero_Pert,nIV,NOV_TAU) = ZERO
                         ComponentTest%d2(k,l,Zero_Pert,nIV,NOV_TAU) = ZERO
                         
                        
                         CALL Get_Next_DeltaX(DeltaX)

                         IF (Max_Iterations( nP )) THEN

                           Cycle Layer_Loop

                         END IF

                       END IF              

                     END DO Perturbation_Loop

                 END DO Layer_Loop

              END DO Input_Variable_Loop

          END DO Channel_Loop
        ! Fill the componenttest perturbation field. Needed for IDL plots to make sense
        ComponentTest%Perturbation  = PERTURBATION_FRACTION
        
        
        

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
                                                   Title         = 'FWD/TL CRTM AtmAbsorption test results for '//&
                                                                   TRIM( File_Prefix ), &
                                                   History       = PROGRAM_RCS_ID, &
                                                   Sensor_Name   = TRIM( File_Prefix ), &
                                                   Platform_Name = TRIM( File_Prefix ), &
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



  CONTAINS

  SUBROUTINE Get_Next_DeltaX ( sDeltaX )
    ! Purpose: 
    ! To update DeltaX in perturbation loop. 
    ! The perturbation is halved for each iteration
    IMPLICIT NONE
    REAL(fp_kind), INTENT(INOUT) :: sDeltaX
    sDeltaX=(0.500_fp_kind)*sDeltaX
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
