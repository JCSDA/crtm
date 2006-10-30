
!  Program to test the CRTM CloudScatter Tangent-Linear code.
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

  ! Module usage
  USE Type_Kinds
  USE Message_Handler
  USE CRTM_Parameters
  USE CRTM_LifeCycle
  USE CRTM_GeometryInfo_Define
  USE CRTM_ChannelInfo_Define
  USE CRTM_Atmosphere_Define
  USE CRTM_Atmosphere_Binary_IO
  USE CRTM_CloudScatter
  USE ComponentTest_netCDF_IO

  ! Disable all implicit typing
  IMPLICIT NONE

  ! Parameters
  CHARACTER( * ), PARAMETER :: PROGRAM_NAME   = 'Test_Tangent_Linear'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: Test_Tangent_Linear.f90,v 1.4 2006/05/02 14:58:34 dgroff Exp $'
  CHARACTER( * ),  PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  INTEGER, PARAMETER :: SL = 512

  CHARACTER(*), PARAMETER :: ATMOSPHERE_FILENAME = 'ECMWF-Atmosphere.Cloud.Aerosol.bin'
  INTEGER,      PARAMETER :: N_PROFILES = 52

  INTEGER, PARAMETER :: UNSET = 0
  INTEGER, PARAMETER ::   SET = 1

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

  ! Variables
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
  INTEGER :: j, k, l, m, n, nP, nIV, nOV, nm, mc, mcType, nLT
  TYPE( CRTM_ChannelInfo_type )  :: ChannelInfo
  TYPE( CRTM_GeometryInfo_type ) :: GeometryInfo
  TYPE( CRTM_AtmScatter_type )   :: CloudScatter_Baseline, &
                                    CloudScatter_NL, CloudScatter_TL
  TYPE( CRTM_Atmosphere_type ), DIMENSION( N_PROFILES ) :: Atmosphere
  TYPE( CRTM_Atmosphere_type )                          :: Atmosphere_NL, Atmosphere_TL
  TYPE( CRTM_CSVariables_type ) :: CSV, CSV_Dummy
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
  WRITE( *, '(/5x, " Program to test the CRTM CloudScatter Tangent-Linear")' )
  WRITE( *, '( 5x, "   components with respect to the Forward components.")' )
  WRITE( *, '(/5x, " $Revision: 1.4 $")' )
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

  ComponentTest_File   = TRIM( File_Prefix )//'.CRTM_CloudScatter.ComponentTest.nc'

  New_File = SET



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

  ! -- Initialise the number of profiles actually written
  nm = 0


  Profile_Loop: DO m = 1, N_PROFILES


    ! ---------------------------------------------
    ! Cycle profile loop if no clouds in input data
    ! ---------------------------------------------

    IF ( Atmosphere(m)%n_Clouds == 0 ) THEN
      WRITE( *, '( 5x, "No clouds in profile # ", i3, ". Cycling..." )' ) m
      CYCLE Profile_Loop
    END IF

    WRITE( *, '( 5x, "Processing profile # ", i3 )' ) m


    ! -------------------------------------------------
    ! Determine the cloud index to use for this profile
    ! Rain and snow clouds stress the CloudScatter code
    ! a bit more than water clouds, so they're used if
    ! possible.
    ! -------------------------------------------------

    mc = 1

    Cloud_Select: DO n = 1, Atmosphere(m)%n_Clouds
      IF ( Atmosphere(m)%Cloud(n)%Type == RAIN_CLOUD .OR. &
           Atmosphere(m)%Cloud(n)%Type == SNOW_CLOUD      ) THEN
        mc = n
        EXIT Cloud_Select
      END IF
    END DO Cloud_Select

    mcType = Atmosphere(m)%Cloud(mc)%Type


    ! --------------------------------
    ! Allocate CloudScatter structures
    ! --------------------------------

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
         Error_Status_NL /= SUCCESS .OR. &
         Error_Status_TL /= SUCCESS      ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error occurred allocating CloudScatter structures', &
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
      CALL Display_Message( PROGRAM_NAME, &
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

      Error_Status = CRTM_Compute_CloudScatter( Atmosphere(m),                &  ! Input
                                                ChannelInfo%Channel_Index(l), &  ! Input, scalar
                                                CloudScatter_Baseline,        &  ! In/Output
                                                CSV                           )  ! Internal variable output

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error computing AtmScatter_Baseline', &
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
          ! Beign layer loop
          ! ----------------

          Layer_Loop: DO k = 1, Atmosphere(m)%n_Layers


            ! --------------------------------
            ! Re-initialise all NL and TL data
            ! --------------------------------

            ! -- The forward model input data
            Error_Status_NL = CRTM_Assign_Atmosphere( Atmosphere(m), &  ! Input
                                                      Atmosphere_NL  )  ! Output

            IF ( Error_Status_NL /= SUCCESS ) THEN
              CALL Display_Message( PROGRAM_NAME, &
                                    'Error reinitialising forward model Atmosphere input data', &
                                    Error_Status )
              STOP
            END IF

            ! -- The tangent-linear model input data
            CALL CRTM_Zero_Atmosphere( Atmosphere_TL )


            ! --------------------------------------------
            ! Perturb the inputs. Only ONE cloud at a time
            ! --------------------------------------------

            Input_Variable_Select: SELECT CASE ( nIV )

              ! -- Temperature
              CASE ( NIV_T )
                Atmosphere_TL%Temperature(k) = &
                  PERTURBATION_FRACTION(nP) * Atmosphere(m)%Temperature(k)

                Atmosphere_NL%Temperature(k) = &
                  Atmosphere_NL%Temperature(k) + &
                  Atmosphere_TL%Temperature(k)

              ! -- Effective radius
              CASE ( NIV_RE )
                Atmosphere_TL%Cloud(mc)%Effective_Radius(k) = &
                  PERTURBATION_FRACTION(nP) * Atmosphere(m)%Cloud(mc)%Effective_Radius(k)

                Atmosphere_NL%Cloud(mc)%Effective_Radius(k) = &
                  Atmosphere_NL%Cloud(mc)%Effective_Radius(k) + &
                  Atmosphere_TL%Cloud(mc)%Effective_Radius(k)

              ! -- Effective variance
              CASE ( NIV_RV )
                Atmosphere_TL%Cloud(mc)%Effective_Variance(k) = &
                  PERTURBATION_FRACTION(nP) * Atmosphere(m)%Cloud(mc)%Effective_Variance(k)

                Atmosphere_NL%Cloud(mc)%Effective_Variance(k) = &
                  Atmosphere_NL%Cloud(mc)%Effective_Variance(k) + &
                  Atmosphere_TL%Cloud(mc)%Effective_Variance(k)

              ! -- Cloud water content
              CASE ( NIV_Q )
                Atmosphere_TL%Cloud(mc)%Water_Content(k) = &
                  PERTURBATION_FRACTION(nP) * Atmosphere(m)%Cloud(mc)%Water_Content(k)

                Atmosphere_NL%Cloud(mc)%Water_Content(k) = &
                  Atmosphere_NL%Cloud(mc)%Water_Content(k) + &
                  Atmosphere_TL%Cloud(mc)%Water_Content(k)

            END SELECT Input_Variable_Select


            ! -------------------------------------------------
            ! Compute the perturbed cloud scattering properties
            ! -------------------------------------------------

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


            ! ------------------------------
            ! Save the data for this profile
            ! ------------------------------

            ComponentTest%d1(k,l,nP,nIV,NOV_TAU) = CloudScatter_NL%Optical_Depth(k) - &
                                                   CloudScatter_Baseline%Optical_Depth(k)
            ComponentTest%d2(k,l,nP,nIV,NOV_TAU) = CloudScatter_TL%Optical_Depth(k)

            ComponentTest%d1(k,l,nP,nIV,NOV_OMEGA) = CloudScatter_NL%Single_Scatter_Albedo(k) - &
                                                     CloudScatter_Baseline%Single_Scatter_Albedo(k)
            ComponentTest%d2(k,l,nP,nIV,NOV_OMEGA) = CloudScatter_TL%Single_Scatter_Albedo(k)

            ComponentTest%d1(k,l,nP,nIV,NOV_G) = CloudScatter_NL%Asymmetry_Factor(k) - &
                                                 CloudScatter_Baseline%Asymmetry_Factor(k)
            ComponentTest%d2(k,l,nP,nIV,NOV_G) = CloudScatter_TL%Asymmetry_Factor(k)

            ComponentTest%d1(k,l,nP,nIV,NOV_D) = CloudScatter_NL%Delta_Truncation(k) - &
                                                 CloudScatter_Baseline%Delta_Truncation(k)
            ComponentTest%d2(k,l,nP,nIV,NOV_D) = CloudScatter_TL%Delta_Truncation(k)

            DO nLT = 0, MAX_N_LEGENDRE_TERMS
              nOV = NOV_PC_L0 + nLT  ! Offset index into output arrays
              ComponentTest%d1(k,l,nP,nIV,nOV) = CloudScatter_NL%Phase_Coefficient(nLT,1,k) - &
                                                 CloudScatter_Baseline%Phase_Coefficient(nLT,1,k)
              ComponentTest%d2(k,l,nP,nIV,nOV) = CloudScatter_TL%Phase_Coefficient(nLT,1,k)
            END DO
            
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

    nm = nm + 1
    ComponentTest%nM = nm
    WRITE( ComponentTest%nM_Name, '( "Profile # ", i3, ". Selected cloud: ", a )' ) &
                                  m, CLOUD_TYPE_NAME( mcType)


    ! --------------
    ! Write the data
    ! --------------

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


    ! -----------------------
    ! Reset the new file flag
    ! -----------------------

    IF ( New_File == SET ) New_File = UNSET



    !#--------------------------------------------------------------------------#
    !#              -- DEALLOCATE STRUCTURES FOR CURRENT PROFILE --             #
    !#--------------------------------------------------------------------------#

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
  !#                     -- DESTROY ComponentTest STRUCTURE --                  #
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
  !#                  -- DEALLOCATE Atmosphere STRUCTURE ARRAY --               #
  !#----------------------------------------------------------------------------#

  Error_Status = CRTM_Destroy_Atmosphere( Atmosphere )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error deallocating Atmosphere structure array', &
                          WARNING )
  END IF

END PROGRAM Test_Tangent_Linear
