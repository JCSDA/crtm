
!  Program to test the CRTM CloudScatter Adjoint code.
!
!  Written by:     Paul van Delst, CIMSS/SSEC 10-Mar-2006
!                  paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2006 Paul van Delst
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

PROGRAM Test_Adjoint


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
  USE CRTM_CloudScatter
  USE ComponentTest_netCDF_IO


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME   = 'Test_Adjoint'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: Test_Adjoint.f90,v 1.4 2006/06/29 15:19:49 dgroff Exp $'
  CHARACTER( * ),  PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  INTEGER, PARAMETER :: SL = 512

  CHARACTER(*), PARAMETER :: ATMOSPHERE_FILENAME = 'ECMWF-Atmosphere.Cloud.Aerosol.bin'
  INTEGER,      PARAMETER :: N_PROFILES = 52

  
  INTEGER, PARAMETER :: N_PERTURBATIONS = 1

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

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  CHARACTER( 256 ) :: Message

  INTEGER :: Error_Status,    Status_FWD   
  INTEGER :: Error_Status_TL, Status_TL
  INTEGER :: Error_Status_AD, Status_AD

  CHARACTER( SL ) :: File_Prefix
  CHARACTER( SL ) :: SpcCoeff_File
  CHARACTER( SL ) :: TauCoeff_File
  CHARACTER( SL ) :: AerosolCoeff_File
  CHARACTER( SL ) :: CloudCoeff_File
  CHARACTER( SL ) :: EmisCoeff_File
  CHARACTER( SL ) :: ComponentTest_File
  INTEGER :: New_File

  INTEGER :: j, k, l, m, n, nP, nIV, nOV, nm, mc, mcType

  TYPE( CRTM_ChannelInfo_type )  :: ChannelInfo
  TYPE( CRTM_GeometryInfo_type ) :: GeometryInfo
  TYPE( CRTM_AtmScatter_type )   :: CloudScatter, &
                                    CloudScatter_TL, CloudScatter_AD
  TYPE( CRTM_Atmosphere_type ), DIMENSION( N_PROFILES ) :: Atmosphere
  TYPE( CRTM_Atmosphere_type )                          :: Atmosphere_TL, Atmosphere_AD
  TYPE( CRTM_CSVariables_type ) :: CSV
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
  WRITE( *, '(/5x, " Program to test the CRTM CloudScatter Adjoint component")' )
  WRITE( *, '( 5x, "   with respect to the Tangent-linear component.")' )
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

  ComponentTest%TestType = COMPONENTTEST_TLAD_TESTTYPE
  ComponentTest%DataType = COMPONENTTEST_POLY_DATATYPE

  ComponentTest%Pressure = Atmosphere(1)%Pressure
  ComponentTest%Spectral = ChannelInfo%Sensor_Channel

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


    ! -----------------------------------------------
    ! Allocate single TL and AD Atmosphere structures
    ! -----------------------------------------------

    ! -- The TL structure
    Error_Status = CRTM_Assign_Atmosphere( Atmosphere(m), &  ! Input
                                           Atmosphere_TL  )  ! Output

    IF ( Error_Status /= SUCCESS ) THEN
      CALL DIsplay_Message( PROGRAM_NAME, &
                            'Error copying to Atmosphere_TL', &
                            Error_Status )
      STOP
    END IF

    ! -- The AD structure
    Error_Status = CRTM_Assign_Atmosphere( Atmosphere(m), &  ! Input
                                           Atmosphere_AD  )  ! Output

    IF ( Error_Status /= SUCCESS ) THEN
      CALL DIsplay_Message( PROGRAM_NAME, &
                            'Error copying to Atmosphere_AD', &
                            Error_Status )
      STOP
    END IF


    ! --------------------------------
    ! Allocate CloudScatter structures
    ! --------------------------------

    ! -- The forward structure
    Error_Status = CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers, &  ! Input
                                             MAX_N_LEGENDRE_TERMS,   &  ! Input
                                             MAX_N_PHASE_ELEMENTS,   &  ! Input
                                             CloudScatter            )  ! Output

    ! -- The tangent-linear structure
    Error_Status_TL = CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers, &  ! Input
                                                MAX_N_LEGENDRE_TERMS,   &  ! Input
                                                MAX_N_PHASE_ELEMENTS,   &  ! Input
                                                CloudScatter_TL         )  ! Output

    ! -- The adjoint structure
    Error_Status_AD = CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers, &  ! Input
                                                MAX_N_LEGENDRE_TERMS,   &  ! Input
                                                MAX_N_PHASE_ELEMENTS,   &  ! Input
                                                CloudScatter_AD         )  ! Output

    ! -- Test results
    IF ( Error_Status    /= SUCCESS .OR. &
         Error_Status_TL /= SUCCESS .OR. &
         Error_Status_AD /= SUCCESS      ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error occurred allocating CloudScatter structures', &
                             Error_Status )                           
      STOP
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- LOOP OVER SENSOR CHANNELS --                       #
    !#--------------------------------------------------------------------------#

    Channel_Loop: DO l = 1, ChannelInfo%n_Channels

      WRITE( *, '( 5x, "Channel: ", i5 )' ) ChannelInfo%Sensor_Channel(l)


      ! --------------------------------------
      ! Compute the forward model CloudScatter
      ! --------------------------------------

      Error_Status = CRTM_Compute_CloudScatter( Atmosphere(m),                &  ! Input
                                                ChannelInfo%Channel_Index(l), &  ! Input, scalar
                                                CloudScatter,                 &  ! In/Output
                                                CSV                           )  ! Internal variable output

      IF ( Error_Status /= SUCCESS ) THEN
        CALL DIsplay_Message( PROGRAM_NAME, &
                              'Error computing CloudScatter', &
                              Error_Status )
        STOP
      END IF



      !#------------------------------------------------------------------------#
      !#               -- BEGIN TANGENT-LINEAR VARIABLE LOOP --                 #
      !#------------------------------------------------------------------------#

      TL_Variable_Loop: DO nIV = 1, N_INPUT_VARIABLES

        WRITE( *, '( 10x, "Performing TL calculations for ", a, "...." )' ) &
                  TRIM( INPUT_VARIABLE_NAME( nIV ) )



        !#----------------------------------------------------------------------#
        !#                     -- BEGIN THE LAYER LOOP --                       #
        !#----------------------------------------------------------------------#

        TL_Layer_Loop: DO k = 1, Atmosphere(m)%n_Layers


          ! -------------------------
          ! Re-initialise all TL data
          ! -------------------------

          CALL CRTM_Zero_Atmosphere( Atmosphere_TL )


          ! --------------------------------------------
          ! Perturb the inputs. Only ONE cloud at a time
          ! --------------------------------------------

          SELECT CASE ( nIV )
            CASE ( NIV_T )
              Atmosphere_TL%Temperature(k) = ONE
            CASE ( NIV_RE )
              Atmosphere_TL%Cloud(mc)%Effective_Radius(k) = ONE
            CASE ( NIV_RV )
              Atmosphere_TL%Cloud(mc)%Effective_Variance(k) = ONE
            CASE ( NIV_Q )
              Atmosphere_TL%Cloud(mc)%Water_Content(k) = ONE
          END SELECT


          ! ------------------------------------------------------
          ! Compute the tangent-linear cloud scattering properties
          ! ------------------------------------------------------

          Error_Status_TL = CRTM_Compute_CloudScatter_TL( Atmosphere(m),                &  ! Input
                                                          CloudScatter,                 &  ! Input
                                                          Atmosphere_TL,                &  ! Input
                                                          ChannelInfo%Channel_Index(l), &  ! Input
                                                          CloudScatter_TL,              &  ! In/Output
                                                          CSV                           )  ! Internal variable input

          IF ( Error_Status_TL /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error computing CloudScatter TL', &
                                   Error_Status )                           
            STOP
          END IF


          ! ------------------------------
          ! Save the data for this profile
          ! ------------------------------

          ComponentTest%d1(k,l,1,nIV,NOV_TAU)   = CloudScatter_TL%Optical_Depth(k)
          ComponentTest%d1(k,l,1,nIV,NOV_OMEGA) = CloudScatter_TL%Single_Scatter_Albedo(k)
          ComponentTest%d1(k,l,1,nIV,NOV_G)     = CloudScatter_TL%Asymmetry_Factor(k)
          ComponentTest%d1(k,l,1,nIV,NOV_D)     = CloudScatter_TL%Delta_Truncation(k)
          DO n = 0, MAX_N_LEGENDRE_TERMS
            nOV = NOV_PC_L0 + n  ! Offset index into output arrays
            ComponentTest%d1(k,l,1,nIV,nOV) = CloudScatter_TL%Phase_Coefficient(n,1,k)
          END DO

        END DO TL_Layer_Loop

      END DO TL_Variable_Loop



      !#------------------------------------------------------------------------#
      !#                  -- BEGIN ADJOINT VARIABLE LOOP --                     #
      !#------------------------------------------------------------------------#

      AD_Variable_Loop: DO nOV = 1, N_OUTPUT_VARIABLES

        WRITE( *, '( 10x, "Performing AD calculations for ", a, "...." )' ) &
                  TRIM( OUTPUT_VARIABLE_NAME( nOV ) )



        !#----------------------------------------------------------------------#
        !#                    -- BEGIN THE LAYER LOOP --                        #
        !#----------------------------------------------------------------------#

        AD_Layer_Loop: DO k = 1, Atmosphere(m)%n_Layers


          ! --------------------------
          ! Reinitialise all AD arrays
          ! --------------------------

          CALL CRTM_Zero_Atmosphere( Atmosphere_AD )

          CloudScatter_AD%Optical_Depth         = ZERO
          CloudScatter_AD%Single_Scatter_Albedo = ZERO
          CloudScatter_AD%Asymmetry_Factor      = ZERO
          CloudScatter_AD%Delta_Truncation      = ZERO
          CloudScatter_AD%Phase_Coefficient     = ZERO


          ! ------------------
          ! Perturb the inputs
          ! ------------------

          SELECT CASE ( nOV )
            CASE ( NOV_TAU )
              CloudScatter_AD%Optical_Depth(k)         = ONE
            CASE ( NOV_OMEGA )
              CloudScatter_AD%Single_Scatter_Albedo(k) = ONE
            CASE ( NOV_G )
              CloudScatter_AD%Asymmetry_Factor(k)      = ONE
            CASE ( NOV_D )
              CloudScatter_AD%Delta_Truncation(k)      = ONE
            CASE ( NOV_PC_L0 )
              CloudScatter_AD%Phase_Coefficient(0,1,k) = ONE 
            CASE ( NOV_PC_L1 )
              CloudScatter_AD%Phase_Coefficient(1,1,k) = ONE 
            CASE ( NOV_PC_L2 )
              CloudScatter_AD%Phase_Coefficient(2,1,k) = ONE 
            CASE ( NOV_PC_L3 )
              CloudScatter_AD%Phase_Coefficient(3,1,k) = ONE 
            CASE ( NOV_PC_L4 )
              CloudScatter_AD%Phase_Coefficient(4,1,k) = ONE 
            CASE ( NOV_PC_L5 )
              CloudScatter_AD%Phase_Coefficient(5,1,k) = ONE 
            CASE ( NOV_PC_L6 )
              CloudScatter_AD%Phase_Coefficient(6,1,k) = ONE 
          END SELECT


          ! --------------------------------
          ! Compute the adjoint CloudScatter
          ! --------------------------------

          Error_Status = CRTM_Compute_CloudScatter_AD( Atmosphere(m),                &  ! Input 
                                                       CloudScatter,                 &  ! Input 
                                                       CloudScatter_AD,              &  ! Input 
                                                       ChannelInfo%Channel_Index(l), &  ! Input
                                                       Atmosphere_AD,                &  ! In/Output
                                                       CSV                           )  ! Internal variable input

          IF ( Error_Status_AD /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error computing CloudScatter AD', &
                                   Error_Status )                           
            STOP
          END IF


          ! ---------------------------------------------------------
          ! Save the data for this profile. Only ONE cloud at a time.
          ! ---------------------------------------------------------

          ComponentTest%d2(k,l,1,NIV_T, nOV) = Atmosphere_AD%Temperature(k)
          ComponentTest%d2(k,l,1,NIV_RE,nOV) = Atmosphere_AD%Cloud(mc)%Effective_Radius(k)
          ComponentTest%d2(k,l,1,NIV_RV,nOV) = Atmosphere_AD%Cloud(mc)%Effective_Variance(k)
          ComponentTest%d2(k,l,1,NIV_Q, nOV) = Atmosphere_AD%Cloud(mc)%Water_Content(k)

        END DO AD_Layer_Loop

      END DO AD_Variable_Loop

    END DO Channel_Loop



    !#--------------------------------------------------------------------------#
    !#      -- WRITE THE CURRENT DATASET ComponentTest STRUCTURE TO FILE --     #
    !#--------------------------------------------------------------------------#

    WRITE( *, '( 10x, "Writing TL/AD data to output file...." )' )


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
                                               Title         = 'TL/AD CRTM CloudScatter test results for '//&
                                                               TRIM( File_Prefix ), &
                                               History       = PROGRAM_RCS_ID, &
                                               Sensor_Name   = TRIM( File_Prefix ), &
                                               Platform_Name = TRIM( File_Prefix ), &
                                               Comment       = 'Phase Coefficient data not written.', &
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

    ! ---------------------------
    ! The CloudScatter structures
    ! ---------------------------

    Error_Status_AD = CRTM_Destroy_AtmScatter( CloudScatter_AD )
    Error_Status_TL = CRTM_Destroy_AtmScatter( CloudScatter_TL )
    Error_Status    = CRTM_Destroy_AtmScatter( CloudScatter    )

    IF ( Error_Status    /= SUCCESS .OR. &
         Error_Status_TL /= SUCCESS .OR. &
         Error_Status_AD /= SUCCESS      ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error deallocating CloudScatter structures', &
                            Error_Status )
      STOP
    END IF


    ! ------------------------------------
    ! The individual Atmosphere structures
    ! ------------------------------------

    Error_Status = CRTM_Destroy_Atmosphere( Atmosphere_TL, Atmosphere_AD )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error deallocating Atmosphere TL/AD structures', &
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

END PROGRAM Test_Adjoint
