
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


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME   = 'Test_Tangent_Linear'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: Test_Tangent_Linear.f90,v 1.11 2006/06/26 20:45:42 dgroff Exp $'
  CHARACTER( * ),  PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  INTEGER, PARAMETER :: SL = 512

  CHARACTER(*), PARAMETER :: ATMOSPHERE_FILENAME = 'ECMWF-Atmosphere.Cloud.Aerosol.bin'
  INTEGER,      PARAMETER :: N_PROFILES = 52

  INTEGER,         PARAMETER :: N_PERTURBATIONS = 37
  REAL( fp_kind ), PARAMETER, DIMENSION(N_PERTURBATIONS) :: PERTURBATION_FRACTION = &
  (/-0.100_fp_kind, -0.090_fp_kind, -0.080_fp_kind, -0.070_fp_kind, -0.060_fp_kind, -0.050_fp_kind, &
    -0.040_fp_kind, -0.030_fp_kind, -0.020_fp_kind, -0.018_fp_kind, -0.016_fp_kind, -0.014_fp_kind, &
    -0.012_fp_kind, -0.010_fp_kind, -0.008_fp_kind, -0.006_fp_kind, -0.004_fp_kind, -0.002_fp_kind, &
     0.000_fp_kind, &
     0.002_fp_kind,  0.004_fp_kind,  0.006_fp_kind,  0.008_fp_kind,  0.010_fp_kind,  0.012_fp_kind, &
     0.014_fp_kind,  0.016_fp_kind,  0.018_fp_kind,  0.020_fp_kind,  0.030_fp_kind,  0.040_fp_kind, &
     0.050_fp_kind,  0.060_fp_kind,  0.070_fp_kind,  0.080_fp_kind,  0.090_fp_kind,  0.100_fp_kind /)

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

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

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
  INTEGER :: New_File

  INTEGER, DIMENSION( 1 ) :: Idx
  INTEGER :: H2O_Idx
  INTEGER :: O3_Idx

  INTEGER :: j, k, l, m, n, nP, nIV

  TYPE( CRTM_ChannelInfo_type )   :: ChannelInfo
  TYPE( CRTM_GeometryInfo_type )  :: GeometryInfo
  TYPE( CRTM_AtmAbsorption_type ) :: AtmAbsorption_Baseline, &
                                     AtmAbsorption_NL, AtmAbsorption_TL
  TYPE( CRTM_Atmosphere_type ), DIMENSION( N_PROFILES ) :: Atmosphere
  TYPE( CRTM_Atmosphere_type )                          :: Atmosphere_NL, Atmosphere_TL

  TYPE( ComponentTest_type ) :: ComponentTest



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
  WRITE( *, '(/5x, " $Revision: 1.11 $")' )
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


    ! ---------------------------------
    ! Allocate AtmAbsorption structures
    ! ---------------------------------

    ! -- The baseline structure
    Error_Status = CRTM_Allocate_AtmAbsorption( Atmosphere(m)%n_Layers, &  ! Input
                                                MAX_N_PREDICTORS,       &  ! Input
                                                MAX_N_ABSORBERS,        &  ! Input
                                                AtmAbsorption_Baseline  )  ! Output

    ! -- The tangent-linear structure
    Error_Status_TL = CRTM_Allocate_AtmAbsorption( Atmosphere(m)%n_Layers, &  ! Input
                                                   MAX_N_PREDICTORS,       &  ! Input
                                                   MAX_N_ABSORBERS,        &  ! Input
                                                   AtmAbsorption_TL        )  ! Output

    ! -- The non-linear structure
    Error_Status_NL = CRTM_Allocate_AtmAbsorption( Atmosphere(m)%n_Layers, &  ! Input
                                                   MAX_N_PREDICTORS,       &  ! Input
                                                   MAX_N_ABSORBERS,        &  ! Input
                                                   AtmAbsorption_NL        )  ! Output

    ! -- Test results
    IF ( Error_Status    /= SUCCESS .OR. &
         Error_Status_TL /= SUCCESS .OR. &
         Error_Status_NL /= SUCCESS      ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error occurred allocating AtmAbsorption structure', &
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


    ! --------------------------------------------
    ! Set up baseline for gaseous absorption calcs
    ! --------------------------------------------

    Error_Status = CRTM_SetUp_AtmAbsorption( Atmosphere(m),         &  ! Input
                                             GeometryInfo,          &  ! Input
                                             AtmAbsorption_Baseline )  ! Output

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error setting up AtmAbsorption_Baseline structure', &
                             Error_Status )                           
      STOP
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- LOOP OVER SENSOR CHANNELS --                       #
    !#--------------------------------------------------------------------------#

    Channel_Loop: DO l = 1, ChannelInfo%n_Channels

      WRITE( *, '( 5x, "Channel: ", i5 )' ) ChannelInfo%Sensor_Channel(l)


      ! ----------------------------------
      ! Compute the baseline layer optical
      ! depths due to gaseous absorption
      ! ----------------------------------

      Error_Status = CRTM_Compute_AtmAbsorption( ChannelInfo%Channel_Index(l), &  ! Input
                                                 AtmAbsorption_Baseline        )  ! In/Output

      IF ( Error_Status /= SUCCESS ) THEN
        CALL DIsplay_Message( PROGRAM_NAME, &
                              'Error computing AtmAbsorption_Baseline', &
                              Error_Status )
        STOP
      END IF



      !#------------------------------------------------------------------------#
      !#                     -- LOOP OVER INPUT VARIABLES --                    #
      !#------------------------------------------------------------------------#

      Input_Variable_Loop: DO nIV = 1, N_INPUT_VARIABLES

        WRITE( *, '( 5x, "Perturbation variable: ", a )' ) INPUT_VARIABLE_NAME(nIV)



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

              CASE ( NIV_W )
                Atmosphere_TL%Absorber(k,H2O_Idx) = PERTURBATION_FRACTION(nP) * Atmosphere(m)%Absorber(k,H2O_Idx)
                Atmosphere_NL%Absorber(k,H2O_Idx) = Atmosphere_NL%Absorber(k,H2O_Idx) + Atmosphere_TL%Absorber(k,H2O_Idx)

              CASE ( NIV_O )
                Atmosphere_TL%Absorber(k,O3_Idx) = PERTURBATION_FRACTION(nP) * Atmosphere(m)%Absorber(k,O3_Idx)
                Atmosphere_NL%Absorber(k,O3_Idx) = Atmosphere_NL%Absorber(k,O3_Idx) + Atmosphere_TL%Absorber(k,O3_Idx)

            END SELECT Input_Variable_Select


            ! ----------------------------------
            ! Setup the AtmAbsorption structures
            ! ----------------------------------

            ! -- The perturbated forward model
            Error_Status_NL = CRTM_SetUp_AtmAbsorption( Atmosphere_NL,   &  ! Input
                                                        GeometryInfo,    &  ! Input
                                                        AtmAbsorption_NL )  ! Output

            ! -- The tangent-linear form
            Error_Status_TL = CRTM_SetUp_AtmAbsorption_TL( Atmosphere(m),          &  ! Input
                                                           AtmAbsorption_Baseline, &  ! Input   
                                                           Atmosphere_TL,          &  ! Input   
                                                           GeometryInfo,           &  ! Input   
                                                           AtmAbsorption_TL        )  ! Output  

            ! -- Test results
            IF ( Error_Status_NL /= SUCCESS .OR. &
                 Error_Status_TL /= SUCCESS      ) THEN
              CALL Display_Message( PROGRAM_NAME, &
                                    'Error setting up AtmAbsorption TL/NL structures', &
                                     Error_Status )                           
              STOP
            END IF


            ! -----------------------------------
            ! Compute the perturbed layer optical
            ! depths due to gaseous absorption
            ! -----------------------------------

            ! -- The perturbated forward model
            Error_Status_NL = CRTM_Compute_AtmAbsorption( ChannelInfo%Channel_Index(l), &  ! Input
                                                          AtmAbsorption_NL              )  ! In/Output

            ! -- The tangent-linear form
            Error_Status_TL = CRTM_Compute_AtmAbsorption_TL( ChannelInfo%Channel_Index(l), &  ! Input
                                                             AtmAbsorption_Baseline,       &  ! Input
                                                             AtmAbsorption_TL              )  ! In/Output

            ! -- Test results
            IF ( Error_Status_NL /= SUCCESS .OR. &
                 Error_Status_TL /= SUCCESS      ) THEN
              CALL Display_Message( PROGRAM_NAME, &
                                    'Error computing AtmAbsorption TL/NL', &
                                     Error_Status )                           
              STOP
            END IF


            ! ------------------------------
            ! Save the data for this profile
            ! ------------------------------

            ComponentTest%d1(k,l,nP,nIV,NOV_TAU) = AtmAbsorption_NL%Optical_Depth(k) - &
                                                   AtmAbsorption_Baseline%Optical_Depth(k)
            ComponentTest%d2(k,l,nP,nIV,NOV_TAU) = AtmAbsorption_TL%Optical_Depth(k)

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

    IF ( Error_Status    /= SUCCESS .OR. &
         Error_Status_TL /= SUCCESS .OR. &
         Error_Status_NL /= SUCCESS      ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error deallocating AtmAbsorption structures', &
                            Error_Status )
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
