!------------------------------------------------------------------------------
!P+
! NAME:
!       Test_Adjoint
!
! PURPOSE:
!       Program to test the CRTM AtmOptics Adjoint code.
!
! CATEGORY:
!       CRTM : AtmOptics : Test
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:                Module containing definitions for kinds
!                                  of variable types.
!
!       Message_Handler:           Module to define simple error codes and
!                                  handle error conditions
!                                  USEs: FILE_UTILITY module
!
!       CRTM_Parameters:           Module of parameter definitions for the CRTM.
!                                  USEs: TYPE_KINDS module
!
!       CRTM_LifeCycle:            Module containing CRTM life cycle functions
!                                  to initialize and destroy the CRTM space.
!                                  USEs: ERROR_HANDLER module
!                                        CRTM_SPCCOEFF module
!                                        CRTM_TAUCOEFF module
!                                        CRTM_AEROSOLCOEFF module
!                                        CRTM_SCATTERCOEFF module
!                                        CRTM_CHANNELINFO_DEFINE module
!                                        CRTM_CHANNELINFO module
!  
!       CRTM_GeometryInfo_Define:  Module defining the CRTM GeometryInfo data
!                                  structure.
!                                  USEs: TYPE_KINDS module
!                                        ERROR_HANDLER module
!                                        CRTM_PARAMETERS module
!
!       CRTM_ChannelInfo_Define:   Module defining the CRTM ChannelInfo data
!                                  structure and containing routines to
!                                  manipulate it.
!                                  USEs: TYPE_KINDS module
!                                        ERROR_HANDLER module
!                                        CRTM_PARAMETERS module
!
!       CRTM_Atmosphere_Define:    Module defining the CRTM Atmosphere
!                                  structure and containing routines to 
!                                  manipulate it.
!                                  USEs: TYPE_KINDS module
!                                        ERROR_HANDLER module
!                                        CRTM_CLOUD_DEFINE module
!
!       CRTM_Atmosphere_Binary_IO: Module to read data into a CRTM_Atmosphere
!                                  structure/array
!                                  USEs: TYPE_KINDS module
!                                        FILE_UTILITY module
!                                        ERROR_HANDLER module
!                                        BINARY_FILE_UTILITY module
!                                        CRTM_ATMOSPHERE_DEFINE module
!
!       CRTM_Atmosphere_Define:    Module defining the CRTM Atmosphere
!                                  structure and containing routines to 
!                                  manipulate it.
!                                  USEs: TYPE_KINDS module
!                                        ERROR_HANDLER module
!                                        CRTM_CLOUD_DEFINE module
!
!       CRTM_Atmosphere_Binary_IO: Module to read data into a CRTM_Atmosphere
!                                  structure/array
!                                  USEs: TYPE_KINDS module
!                                        FILE_UTILITY module
!                                        ERROR_HANDLER module
!                                        BINARY_FILE_UTILITY module
!                                        CRTM_ATMOSPHERE_DEFINE module
!
!       CRTM_AtmAbsorption:        Module containing routines to compute the
!                                  optical depth profile due to gaseous
!                                  absorption.
!                                  USEs: TYPE_KINDS module
!                                        ERROR_HANDLER module
!                                        CRTM_PARAMETERS module
!                                        CRTM_TAUCOEFF module
!                                        CRTM_ATMOSPHERE_DEFINE module
!                                        CRTM_GEOMETRYINFO_DEFINE module
!                                        CRTM_ATMABSORPTION_DEFINE module
!                                        CRTM_ATMABSORPTION_INTABSORBER module
!                                        CRTM_ATMABSORPTION_PREDICTOR module
!
!       ComponentTest_Define:          Module defining the structure to hold CRTM
!                                  Tangent-Linear/Adjoint model test results
!                                  and containing routines to manipulate it.
!                                  USEs: TYPE_KINDS module
!                                        ERROR_HANDLER module
!
!       ComponentTest_netCDF_IO:       Module containing routines to read and write
!                                  netCDF format ComponentTest files.
!                                  USEs: TYPE_KINDS module
!                                        FILE_UTILITY module
!                                        ERROR_HANDLER module
!                                        ComponentTEST_DEFINE module
!                                        NETCDF module
!                                        NETCDF_UTILITY module
! CONTAINS:
!       None.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Jan-2005
!                       paul.vandelst@ssec.wisc.edu
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
!P-
!------------------------------------------------------------------------------

PROGRAM Test_Adjoint


  ! ------------
  ! Module usage
  ! ------------

  ! -- Utility modules
  USE Type_Kinds
  USE Message_Handler

  ! -- CRTM modules
  USE CRTM_Parameters
  USE CRTM_LifeCycle
  USE CRTM_GeometryInfo_Define, ONLY: CRTM_GeometryInfo_type, &
                                      CRTM_Compute_GeometryInfo
  USE CRTM_ChannelInfo_Define
  USE CRTM_Atmosphere_Define
  USE CRTM_AtmAbsorption, ONLY: CRTM_AtmAbsorption_type, &
                                CRTM_Allocate_AtmAbsorption, &
                                CRTM_Destroy_AtmAbsorption, &
                                CRTM_SetUp_AtmAbsorption, &
                                CRTM_Compute_AtmAbsorption, &
                                CRTM_SetUp_AtmAbsorption_TL, &
                                CRTM_Compute_AtmAbsorption_TL, &
                                CRTM_SetUp_AtmAbsorption_AD, &
                                CRTM_Compute_AtmAbsorption_AD
  USE CRTM_CloudScatter, ONLY: CRTM_AtmScatter_type, &
                               CRTM_Allocate_AtmScatter, &
                               CRTM_Destroy_AtmScatter, &
                               CRTM_CSVariables_type, &
                               CRTM_Compute_CloudScatter, &
                               CRTM_Compute_CloudScatter_TL, &
                               CRTM_Compute_CloudScatter_AD
  USE CRTM_AerosolScatter, ONLY: CRTM_Compute_AerosolScatter, &
                                 CRTM_Compute_AerosolScatter_TL, &
                                 CRTM_Compute_AerosolScatter_AD
  USE CRTM_AtmOptics, ONLY: CRTM_AOVariables_type, &
                            CRTM_Combine_AtmOptics, &
                            CRTM_Combine_AtmOptics_TL, &
                            CRTM_Combine_AtmOptics_AD

  USE CRTM_Atmosphere_Binary_IO, ONLY: CRTM_Read_Atmosphere_Binary
  USE ComponentTest_Define
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
  CHARACTER( * ),  PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'
  INTEGER, PARAMETER :: SL = 512


  CHARACTER(*), PARAMETER :: ATMOSPHERE_FILENAME = 'ECMWF-Atmosphere.Cloud.Aerosol.bin'
  INTEGER,      PARAMETER :: N_PROFILES = 52

  INTEGER, PARAMETER :: N_PERTURBATIONS = 1

  INTEGER, PARAMETER :: N_INPUT_VARIABLES = 10
  INTEGER, PARAMETER :: NIV_P  = 1   ! Layer pressure
  INTEGER, PARAMETER :: NIV_T  = 2   ! Layer temperature
  INTEGER, PARAMETER :: NIV_W  = 3   ! Layer water vapor
  INTEGER, PARAMETER :: NIV_O  = 4   ! Layer ozone
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
   INTEGER, PARAMETER :: NOV_TAU     = 1  ! Optical depth
   INTEGER, PARAMETER :: NOV_OMEGA   = 2  ! Single scatter albedo
   INTEGER, PARAMETER :: NOV_G       = 3  ! Asymmetry factor
   INTEGER, PARAMETER :: NOV_D       = 4  ! Delta truncation
   INTEGER, PARAMETER :: NOV_P       = 5  ! Phase coefficient 
  
   CHARACTER( * ), PARAMETER, DIMENSION( N_OUTPUT_VARIABLES ) :: &
    OUTPUT_VARIABLE_NAME = (/ 'Optical depth            ', &
                              'Single scatter albedo    ', &
                              'Asymmetry factor         ', &
                              'Delta truncation         ', &
                              'Phase coefficient        ' /)
                              
   CHARACTER( * ), PARAMETER, DIMENSION( N_OUTPUT_VARIABLES ) :: &
    OUTPUT_VARIABLE_UNITS = (/ 'Unitless', &  ! Optical depth
                               'Unitless', &  ! Single scatter albedo
                               'Unitless', &  ! Asymmetry factor
                               'Unitless', &  ! Delta truncation factor
                               'Unitless' /)  ! Phase coefficient
                               

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
  INTEGER, DIMENSION( 1 ) :: Idx
  INTEGER :: H2O_Idx
  INTEGER :: O3_Idx
  INTEGER :: j, k, l, m, n, nP, nIV, nOV, nm, nLT
  INTEGER :: mc, mcType
  LOGICAL :: Cloud_Present
  INTEGER :: ma, mam, maType
  LOGICAL :: Aerosol_Present


  TYPE( CRTM_ChannelInfo_type )   :: ChannelInfo
  TYPE( CRTM_GeometryInfo_type )  :: GeometryInfo
  TYPE( CRTM_AtmAbsorption_type ) :: AtmAbsorption, &
                                     AtmAbsorption_TL, &
                                     AtmAbsorption_AD
  TYPE( CRTM_AtmScatter_type ) :: CloudScatter, CloudScatter_TL, CloudScatter_AD
  TYPE( CRTM_AtmScatter_type ) :: AerosolScatter, AerosolScatter_TL, AerosolScatter_AD
  TYPE( CRTM_AtmScatter_type ) :: AtmOptics, AtmOptics_TL, AtmOptics_AD
  TYPE( CRTM_Atmosphere_type ), DIMENSION( N_PROFILES ) :: Atmosphere
  TYPE( CRTM_Atmosphere_type )                          :: Atmosphere_TL, Atmosphere_AD
                                 
  TYPE( CRTM_CSVariables_type ) :: CSV, CSV_Dummy
  TYPE( CRTM_AOVariables_type ) :: AOV, AOV_Dummy
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
  WRITE( *, '(/5x, " Program to test the CRTM AtmOptics Adjoint")' )
  WRITE( *, '( 5x, "   components with respect to the Tangent-Linear")' )
  WRITE( *, '( 5x, "   components.")' )
  WRITE( *, '(/5x, " $Revision: 1.1 $")' )
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

  DO m=1, N_PROFILES
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
  AerosolCoeff_File = 'AerosolCoeff.bin'
  CloudCoeff_File   = 'CloudCoeff.bin'
  EmisCoeff_File    = 'EmisCoeff.bin'

  ComponentTest_File = TRIM( File_Prefix )//'.CRTM_AtmOptics.ComponentTest.nc'

  New_File = 1



  !#----------------------------------------------------------------------------#
  !#                          -- INITIALIZE THE CRTM --                         #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Initializing the CRTM..." )' )

  Error_Status = CRTM_Init( ChannelInfo, &
                            SpcCoeff_File     = SpcCoeff_File, &
                            TauCoeff_File     = TauCoeff_File, &
                            AerosolCoeff_File = AerosolCoeff_File, &
                            CloudCoeff_File   = CloudCoeff_File, &
                            EmisCoeff_File    = EmisCoeff_File )

  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error initializing CRTM', & 
                            Error_Status)  
   STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                      -- ALLOCATE ComponentTest STRUCTURE --                #
  !#----------------------------------------------------------------------------#

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

  ComponentTest%Input_Variable_Name = INPUT_VARIABLE_NAME
  ComponentTest%Input_Variable_Units = INPUT_VARIABLE_UNITS

  ComponentTest%Output_Variable_Name = OUTPUT_VARIABLE_NAME
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

    ! -- Water vapor
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


    ! -----------------------------------------------------------------------------
    ! Allocate AtmAbsorption, CloudScatter, AerosolScatter and AtmOptics structures
    ! -----------------------------------------------------------------------------

    ! -- Forward
    Status_FWD = CRTM_Allocate_AtmAbsorption( Atmosphere(m)%n_Layers, &  ! Input
                                              MAX_N_PREDICTORS,       &  ! Input
                                              MAX_N_ABSORBERS,        &  ! Input
                                              AtmAbsorption           )  ! Output

    ! -- Tangent-linear
    Status_TL = CRTM_Allocate_AtmAbsorption( Atmosphere(m)%n_Layers,   &  ! Input
                                             MAX_N_PREDICTORS,         &  ! Input
                                             MAX_N_ABSORBERS,          &  ! Input
                                             AtmAbsorption_TL          )  ! Output

    ! -- Adjoint
    Status_AD = CRTM_Allocate_AtmAbsorption( Atmosphere(m)%n_Layers,   &  ! Input
                                             MAX_N_PREDICTORS,         &  ! Input
                                             MAX_N_ABSORBERS,          &  ! Input
                                             AtmAbsorption_AD          )  ! Output
 
    ! -- Check for successful allocation
    IF ( Status_FWD /= SUCCESS .OR. Status_TL /= SUCCESS .OR. Status_AD /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL DIsplay_Message( PROGRAM_NAME, &
                            'Error allocating AtmAbsorption structures', &
                            Error_Status )
      STOP
    END IF

    ! -- Forward
    Status_FWD = CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers,         &  ! Input
                                           MAX_N_LEGENDRE_TERMS,           &  ! Input
                                           MAX_N_PHASE_ELEMENTS,           &  ! Input
                                           CloudScatter                    )  ! Output

    ! -- Tangent-linear
    Status_TL = CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers,   &  ! Input
                                          MAX_N_LEGENDRE_TERMS,     &  ! Input
                                          MAX_N_PHASE_ELEMENTS,     &  ! Input
                                          CloudScatter_TL           )  ! Output

    ! -- Adjoint
    Status_AD = CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers,   &  ! Input
                                          MAX_N_LEGENDRE_TERMS,     &  ! Input
                                          MAX_N_PHASE_ELEMENTS,     &  ! Input
                                          CloudScatter_AD           )  ! Output
 
    ! -- Check for successful allocation
    IF ( Status_FWD /= SUCCESS .OR. Status_TL /= SUCCESS .OR. Status_AD /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL DIsplay_Message( PROGRAM_NAME, &
                            'Error allocating CloudScatter structures', &
                            Error_Status )
      STOP
    END IF

    ! -- Forward
    Status_FWD = CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers,  &  ! Input
                                           MAX_N_LEGENDRE_TERMS,    &  ! Input
                                           MAX_N_PHASE_ELEMENTS,    &  ! Input
                                           AerosolScatter           )  ! Output

    ! -- Tangent-linear
    Status_TL = CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers,   &  ! Input
                                          MAX_N_LEGENDRE_TERMS,     &  ! Input
                                          MAX_N_PHASE_ELEMENTS,     &  ! Input
                                          AerosolScatter_TL         )  ! Output

    ! -- Adjoint
    Status_AD = CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers,   &  ! Input
                                          MAX_N_LEGENDRE_TERMS,     &  ! Input
                                          MAX_N_PHASE_ELEMENTS,     &  ! Input
                                          AerosolScatter_AD         )  ! Output
 
    ! -- Check for successful allocation
    IF ( Status_FWD /= SUCCESS .OR. Status_TL /= SUCCESS .OR. Status_AD /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL DIsplay_Message( PROGRAM_NAME, &
                            'Error allocating AerosolScatter structures', &
                            Error_Status )
      STOP
    END IF

    ! -- Forward
    Status_FWD = CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers,         &  ! Input
                                           MAX_N_LEGENDRE_TERMS,           &  ! Input
                                           MAX_N_PHASE_ELEMENTS,           &  ! Input
                                           AtmOptics                       )  ! Output

    ! -- Tangent-linear
    Status_TL = CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers,   &  ! Input
                                          MAX_N_LEGENDRE_TERMS,     &  ! Input
                                          MAX_N_PHASE_ELEMENTS,     &  ! Input
                                          AtmOptics_TL              )  ! Output

    ! -- Adjoint
    Status_AD = CRTM_Allocate_AtmScatter( Atmosphere(m)%n_Layers,   &  ! Input
                                          MAX_N_LEGENDRE_TERMS,     &  ! Input
                                          MAX_N_PHASE_ELEMENTS,     &  ! Input
                                          AtmOptics_AD              )  ! Output
 
    ! -- Check for successful allocation
    IF ( Status_FWD /= SUCCESS .OR. Status_TL /= SUCCESS .OR. Status_AD /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL DIsplay_Message( PROGRAM_NAME, &
                            'Error allocating AtmOptics structures', &
                            Error_Status )
      STOP
    END IF

    ! ---------------------------------
    ! Setup for the forward calculation
    ! ---------------------------------

    Error_Status = CRTM_SetUp_AtmAbsorption( Atmosphere(m), &  ! Input
                                             GeometryInfo,  &  ! Input
                                             AtmAbsorption  )  ! Output

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error setting up AtmAbsorption structure', &
                             Error_Status )                           
      STOP
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- LOOP OVER SENSOR CHANNELS --                       #
    !#--------------------------------------------------------------------------#

    Channel_Loop: DO l = 1, ChannelInfo%n_Channels

      WRITE( *, '( 5x, "Channel: ", i5 )' ) ChannelInfo%Sensor_Channel(l)



      !#------------------------------------------------------------------------#
      !#            -- COMPUTE THE FORWARD MODEL AtmAbsorption --               #
      !#------------------------------------------------------------------------#

      Error_Status = CRTM_Compute_AtmAbsorption( ChannelInfo%Channel_Index(l), &  ! Input
                                                 AtmAbsorption                 )  ! In/Output

      IF ( Error_Status /= SUCCESS ) THEN
        CALL DIsplay_Message( PROGRAM_NAME, &
                              'Error computing AtmAbsorption_', &
                              Error_Status )
        STOP
      END IF

      !#------------------------------------------------------------------------#
      !#            -- COMPUTE THE FORWARD MODEL CloudScatter --                #
      !#------------------------------------------------------------------------#
      IF ( Cloud_Present ) THEN
        Error_Status = CRTM_Compute_CloudScatter( Atmosphere(m),                 &  ! Input
                                                  ChannelInfo%Channel_Index(1),  &  ! Input, scalar
                                                  CloudScatter,                  &  ! In/Output
                                                  CSV                            )
        IF ( Error_Status /= SUCCESS ) THEN
          CALL Display_Message( PROGRAM_NAME, &
                                'Error computing CloudScatter', &
                                Error_Status )

           STOP
         END IF
       END IF


      !#------------------------------------------------------------------------#
      !#             -- COMPUTE THE FORWARD MODEL AerosolScatter --             #
      !#------------------------------------------------------------------------#
      IF ( Aerosol_Present ) THEN
         Error_Status = CRTM_COMPUTE_AerosolScatter( Atmosphere(m),                &  ! Input
                                                     GeometryInfo,                 &  ! Input
                                                     ChannelInfo%Channel_Index(1), &  ! Input, scalar
                                                     AerosolScatter                ) !,    & ! In/Output
                                                     !ASV                          ) ! Internal variable output
         
        IF ( Error_Status /= SUCCESS ) THEN
          CALL Display_Message( PROGRAM_NAME, &
                                'Error computing AerosolScatter', &
                                Error_Status )
      
           STOP
         END IF
       END IF

      ! ------------------------------
      ! Compute the AtmOptics
      ! ------------------------------
      CALL CRTM_Combine_AtmOptics( AtmAbsorption,       &  ! Input
                                   CloudScatter,        &  ! Input
                                   AerosolScatter,      &  ! Input   
                                   AtmOptics,           &  ! In/Output
                                   AOV                  )  ! Internal variable output



      !#------------------------------------------------------------------------#
      !#               -- BEGIN TANGENT-LINEAR VARIABLE LOOP --                 #
      !#------------------------------------------------------------------------#

      TL_Variable_Loop: DO nIV = 1, N_INPUT_VARIABLES

        WRITE( *, '( 10x, "Performing TL calculations for ", a, "...." )' ) &
	TRIM( INPUT_VARIABLE_NAME( nIV ) )


        ! -----------------------------
        ! Check for clouds and aerosols
        ! -----------------------------
        ! If we're processing cloud variables, but there
        ! are no clouds, go to next input variable
        IF ( (nIV >= NIV_CRE .AND. nIV <= NIV_CQ) .AND. (.NOT. Cloud_Present) ) CYCLE TL_Variable_Loop

        ! If we're processing aerosol variables, but there
        ! are none, go to next input variable
        IF ( (nIV >= NIV_ARE .AND. nIV <= NIV_AC) .AND. (.NOT. Aerosol_Present) ) CYCLE TL_Variable_Loop


        !#----------------------------------------------------------------------#
        !#                    -- BEGIN THE LAYER LOOP --                        #
        !#----------------------------------------------------------------------#

        TL_Layer_Loop: DO k = 1, Atmosphere(m)%n_Layers


          ! --------------------------
          ! Reinitialise all TL arrays
          ! --------------------------

          CALL CRTM_ZERO_Atmosphere( Atmosphere_TL )
          
          ! ------------------
          ! Perturb the inputs
          ! ------------------

          SELECT CASE ( nIV )
            CASE ( NIV_P )
              Atmosphere_TL%Pressure(k)         = ONE
            CASE ( NIV_T )
              Atmosphere_TL%Temperature(k)      = ONE
            CASE ( NIV_W )
              Atmosphere_TL%Absorber(k,H2O_Idx) = ONE
            CASE ( NIV_O )
              Atmosphere_TL%Absorber(k,O3_Idx)  = ONE
            CASE ( NIV_CRE )
              Atmosphere_TL%Cloud(mc)%Effective_Radius(k) = ONE
            CASE ( NIV_CRV )
              Atmosphere_TL%Cloud(mc)%Effective_Variance(k) = ONE
            CASE ( NIV_CQ )
              Atmosphere_TL%Cloud(mc)%Water_Content(k) = ONE
            CASE ( NIV_ARE )
              Atmosphere_TL%Aerosol(ma)%Effective_Radius(k,mam) = ONE
            CASE ( NIV_ARV )
              Atmosphere_TL%Aerosol(ma)%Effective_Variance(k,mam) = ONE
            CASE ( NIV_AC )
              Atmosphere_TL%Aerosol(ma)%Concentration(k,mam) = ONE
          END SELECT


          ! ------------------------------------------------
          ! Setup the tangent-linear AtmAbsorption structure
          ! ------------------------------------------------

          Error_Status = CRTM_SetUp_AtmAbsorption_TL( Atmosphere(m),   &  ! Input
                                                      AtmAbsorption,   &  ! Input   
                                                      Atmosphere_TL,   &  ! Input   
                                                      GeometryInfo,    &  ! Input   
                                                      AtmAbsorption_TL )  ! Output  

          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error setting up AtmAbsorption TL structures', &
                                   Error_Status )                           
            STOP
          END IF


          ! ----------------------------------------
          ! Compute the tangent-linear AtmAbsorption
          ! ----------------------------------------

          Error_Status = CRTM_Compute_AtmAbsorption_TL( ChannelInfo%Channel_Index(l), &  ! Input
                                                        AtmAbsorption,                &  ! Input
                                                        AtmAbsorption_TL              )  ! In/Output

          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error computing AtmAbsorption TL', &
                                   Error_Status )                           
            STOP
          END IF

          ! ------------------------------------------------------
          ! Compute the tangent-linear cloud scattering properties
          ! ------------------------------------------------------

          
          IF (Cloud_Present) THEN
             Error_Status_TL = CRTM_Compute_CloudScatter_TL( Atmosphere(m),                 &  ! Input
                                                             CloudScatter,                  &  ! Input
                                                             Atmosphere_TL,                 &  ! Input
                                                             ChannelInfo%Channel_Index(1),  &  ! Input
                                                             CloudScatter_TL,               &  ! In/Output
                                                             CSV                            )  ! Internal variable input
            ! -- Test results
            IF ( Error_Status_TL /= SUCCESS ) THEN
              CALL Display_Message( PROGRAM_NAME, &
                                    'Error computing CloudScatter TL', &
                                     Error_Status )

              STOP
            END IF
          END IF


          ! --------------------------------------------------------
          ! Compute the tangent-linear aerosol scattering properties
          ! --------------------------------------------------------

          IF ( Aerosol_Present ) THEN
             Error_Status_TL = CRTM_compute_AerosolScatter_TL( Atmosphere(m),                 &  ! Input
                                                               AerosolScatter,                &  ! Input
                                                               Atmosphere_TL,                 &  ! Input 
                                                               GeometryInfo,                  &  ! Input
                                                               ChannelInfo%Channel_Index(1),  &  ! Input
                                                               AerosolScatter_TL  )!,         &  ! In/Output
                                                               !ASV                            )  Internal variable input

          ! -- Test results
          IF ( Error_Status_TL /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error computing AerosolScatter TL', &
                                   Error_Status )
               STOP
             END IF
           END IF

           


          ! -----------------------------------------
          ! Compute the tangent linear AtmOptics data
          ! -----------------------------------------

          CALL CRTM_Combine_AtmOptics_TL( AtmAbsorption,     &  ! FWD Input
                                          CloudScatter,      &  ! FWD Input
                                          AerosolScatter,    &  ! FWD Input
                                          AtmOptics,         &  ! FWD Input
                                          AtmAbsorption_TL,  &  ! TL Input
                                          CloudScatter_TL,   &  ! TL Input
                                          AerosolScatter_TL, &  ! TL Input
                                          AtmOptics_TL,      &  ! TL Output
                                          AOV                )  ! Internal variable input 


          ! ------------------------------                   
          ! Save the data for this profile
          ! ------------------------------

          ComponentTest%d1(k,l,1, nIV, NOV_TAU) = AtmOptics_TL%Optical_Depth(k)
          ComponentTest%d1(k,l,1, nIV, NOV_OMEGA) = AtmOptics_TL%Single_Scatter_Albedo(k)
          ComponentTest%d1(k,l,1, nIV, NOV_G) = AtmOptics_TL%Asymmetry_Factor(k)
          ComponentTest%d1(k,l,1, nIV, NOV_D) = AtmOptics_TL%Delta_Truncation(k)
          ComponentTest%d1(k,l,1,nIV,nOV_P) = AtmOptics_TL%Phase_Coefficient(1,1,k)
          

        END DO TL_Layer_Loop

      END DO TL_Variable_Loop



      !#------------------------------------------------------------------------#
      !#                  -- BEGIN ADJOINT VARIABLE LOOP --                     #
      !#------------------------------------------------------------------------#

      WRITE( *, '( 10x, "Performing AD calculations...." )' )

      AD_Variable_Loop: DO nOV = 1, N_OUTPUT_VARIABLES


        ! -----------------------------
        ! Check for clouds and aerosols
        ! -----------------------------
        ! If we're processing cloud/aerosol variables, but there
        ! are no clouds/aerosols, go to next output variable
        IF ( (nOV >= NOV_OMEGA) .AND. (.NOT. Cloud_Present) .AND. (.NOT. Aerosol_Present) ) CYCLE AD_Variable_Loop



         WRITE( *, '(10x, "Performing AD calculations for ", a, "....")' ) &
                   TRIM( OUTPUT_VARIABLE_NAME( nOV ) )


        !#---------------------------------------------------------------------#
        !#                   -- BEGIN THE LAYER LOOP --                        #
        !#---------------------------------------------------------------------#
        AD_Layer_Loop: DO k = 1, Atmosphere(m)%n_Layers


        ! --------------------------
        ! Reinitialize all AD arrays
        ! -------------------------- 

        CALL CRTM_Zero_Atmosphere( Atmosphere_AD )

        AtmOptics_AD%Optical_Depth         = ZERO
        AtmOptics_AD%Single_Scatter_Albedo = ZERO
        AtmOptics_AD%Asymmetry_Factor      = ZERO
        AtmOptics_AD%Delta_Truncation      = ZERO
        AtmOptics_AD%Phase_Coefficient     = ZERO

        ! ---------------------------------
        ! Perturb the adjoint optical depth
        ! ---------------------------------


        SELECT CASE ( nOV )
          CASE ( NOV_Tau ) 
            AtmOptics_AD%Optical_Depth(k) = ONE
          CASE ( NOV_Omega )
            AtmOptics_AD%Single_Scatter_Albedo(k) = ONE
          CASE ( NOV_G )
            AtmOptics_AD%Asymmetry_Factor(k) = ONE
          CASE ( NOV_D )
            AtmOptics_AD%Delta_Truncation(k) = ONE
	  CASE ( NOV_P )
	    AtmOptics_AD%Phase_Coefficient(1,1,k) = ONE
         END SELECT 


       
        ! ---------------------------------------------------------------------------------------
        ! Compute the adjoint Optical Properties (AtmAbsorption, CloudScatter and AerosolScatter)
        ! ---------------------------------------------------------------------------------------

        CALL CRTM_Combine_AtmOptics_AD( AtmAbsorption,     &  ! Input
                                        CloudScatter,      &  ! Input
                                        AerosolScatter,    &  ! Input
                                        AtmOptics,         &  ! Input
                                        AtmOptics_AD,      &  ! AD Input
                                        AtmAbsorption_AD,  &  ! AD Output
                                        CloudScatter_AD,   &  ! AD Output
                                        AerosolScatter_AD, &  ! AD Output
                                        AOV                )  ! Internal variable input

        IF ( Error_Status /= SUCCESS ) THEN
           CALL Display_Message( PROGRAM_NAME, &
                                 'Error computing Adjoint form of Optical Properties', &
                                  Error_Status )
          STOP
        END IF

        
        

        ! ------------------------------
        ! Compute the adjoint Atmosphere
        ! ------------------------------

        Error_Status = CRTM_Compute_AtmAbsorption_AD( ChannelInfo%Channel_Index(l), &  ! Input
                                                      AtmAbsorption,                &  ! Input
                                                      AtmAbsorption_AD              )  ! In/Output

        IF ( Error_Status /= SUCCESS ) THEN
          CALL Display_Message( PROGRAM_NAME, &
                                'Error computing AtmAbsorption AD', &
                                 Error_Status )                           
          STOP
        END IF
 




        Error_Status = CRTM_SetUp_AtmAbsorption_AD( Atmosphere(m),    &  ! Input
                                                    AtmAbsorption,    &  ! Input
                                                    AtmAbsorption_AD, &  ! Input
                                                    GeometryInfo,     &  ! Input
                                                    Atmosphere_AD     )  ! Output

        IF ( Error_Status /= SUCCESS ) THEN
          CALL Display_Message( PROGRAM_NAME, &
                                'Error computing AtmAbsorption AD', &
                                 Error_Status )                           
          STOP
        END IF



        
        IF (Cloud_Present) THEN
        Error_Status = CRTM_Compute_CloudScatter_AD( Atmosphere(m),                & ! Input
                                                     CloudScatter,                 & ! Input
                                                     CloudScatter_AD,              & ! Input
                                                     ChannelInfo%Channel_Index(1), & ! Input
                                                     Atmosphere_AD,                & ! In/Output
                                                     CSV                           ) ! Internal variable input

        IF ( Error_Status /= SUCCESS ) THEN
          CALL Display_Message( PROGRAM_NAME, &
                                'Error computing CloudScatter AD', &
                                 Error_Status )
          STOP
        END IF
        END IF
        
        IF (Aerosol_Present) THEN
        Error_Status = CRTM_Compute_AerosolScatter_AD( Atmosphere(m),                  & ! Input
                                                       AerosolScatter,                 & ! Input
                                                       AerosolScatter_AD,              & ! Input
                                                       GeometryInfo,                   & ! Input
                                                       ChannelInfo%Channel_Index(1),   & ! Input
                                                       Atmosphere_AD)!,                & ! In/Output
                                                       !ASV                            ) ! Internal variable input


        IF ( Error_Status /= SUCCESS ) THEN
           CALL Display_Message( PROGRAM_NAME, &
                                 'Error computing AerosolScatter AD setup', &
                                  Error_Status )
           STOP
        END IF
        END IF

       
        ! -------------------------------
        ! Save the data for this profile.
        ! -------------------------------

        ComponentTest%d2(k,l,1,NIV_P, nOV)   = Atmosphere_AD%Pressure(k)
        ComponentTest%d2(k,l,1,NIV_T, nOV)   = Atmosphere_AD%Temperature(k)
        ComponentTest%d2(k,l,1,NIV_W, nOV)   = Atmosphere_AD%Absorber(k, H2O_Idx)
        ComponentTest%d2(k,l,1,NIV_O, nOV)   = Atmosphere_AD%Absorber(k, O3_Idx)
        IF (Cloud_Present) THEN
        ComponentTest%d2(k,l,1,NIV_CRE, nOV) = Atmosphere_AD%Cloud(mc)%Effective_Radius(k) 
        ComponentTest%d2(k,l,1,NIV_CRV, nOV) = Atmosphere_AD%Cloud(mc)%Effective_Variance(k)
        ComponentTest%d2(k,l,1,NIV_CQ, nOV)  = Atmosphere_AD%Cloud(mc)%Water_Content(k)
        END IF
        IF (Aerosol_Present) THEN
        ComponentTest%d2(k,l,1,NIV_ARE, nOV) = Atmosphere_AD%Aerosol(ma)%Effective_Radius(k,mam)
        ComponentTest%d2(k,l,1,NIV_ARV, nOV) = Atmosphere_AD%Aerosol(ma)%Effective_Variance(k,mam)
        ComponentTest%d2(k,l,1,NIV_AC, nOV)  = Atmosphere_AD%Aerosol(ma)%Concentration(k,mam)
        END IF
         
        END DO AD_Layer_Loop
       
      END DO AD_Variable_Loop

    END DO Channel_Loop

    

    !#--------------------------------------------------------------------------#
    !#        -- WRITE THE CURRENT DATASET TLADMtest STRUCTURE TO FILE --       #
    !#--------------------------------------------------------------------------#

    WRITE( *, '( 10x, "Writing TL/AD data to output file...." )' )


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
                                           Title         = 'TL/AD CRTM AtmOptics test results for '//&
                                                           TRIM( File_Prefix ), &
                                           History       = PROGRAM_RCS_ID, &
                                           Sensor_Name   = TRIM( File_Prefix ), &
                                           Platform_Name = TRIM( File_Prefix ), &
                                           Comment       = 'TL and AD data are optical depths.', &
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

    Error_Status    = CRTM_Destroy_AtmAbsorption( AtmAbsorption )
    Error_Status_TL = CRTM_Destroy_AtmAbsorption( AtmAbsorption_TL )
    Error_Status_AD = CRTM_Destroy_AtmAbsorption( AtmAbsorption_AD )

    IF ( Error_Status    /= SUCCESS .OR. &
         Error_Status_TL /= SUCCESS .OR. &
         Error_Status_AD /= SUCCESS      ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error deallocating AtmAbsorption structures', &
                            Error_Status )
      STOP
    END IF

    ! ---------------------------
    ! The CloudScatter structures
    ! ---------------------------

    Error_Status    = CRTM_Destroy_AtmScatter( CloudScatter )
    Error_Status_TL = CRTM_Destroy_AtmScatter( CloudScatter_TL )
    Error_Status_AD = CRTM_Destroy_AtmScatter( CloudScatter_AD )

    IF ( Error_Status    /= SUCCESS .OR. &
         Error_Status_TL /= SUCCESS .OR. &
         Error_Status_AD /= SUCCESS      ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error deallocating CloudScatter structures', &
                            Error_Status )
      STOP
    END IF

    ! -----------------------------
    ! The AerosolScatter structures
    ! -----------------------------

    Error_Status    = CRTM_Destroy_AtmScatter( AerosolScatter )
    Error_Status_TL = CRTM_Destroy_AtmScatter( AerosolScatter_TL )
    Error_Status_AD = CRTM_Destroy_AtmScatter( AerosolScatter_AD )

    IF ( Error_Status    /= SUCCESS .OR. &
         Error_Status_TL /= SUCCESS .OR. &
         Error_Status_AD /= SUCCESS      ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error deallocating AerosolScatter structures', &
                            Error_Status )
      STOP
    END IF

    ! ------------------------
    ! The AtmOptics structures
    ! ------------------------

    Error_Status    = CRTM_Destroy_AtmScatter( AtmOptics )
    Error_Status_TL = CRTM_Destroy_AtmScatter( AtmOptics_TL )
    Error_Status_AD = CRTM_Destroy_AtmScatter( AtmOptics_AD )

    IF ( Error_Status    /= SUCCESS .OR. &
         Error_Status_TL /= SUCCESS .OR. &
         Error_Status_AD /= SUCCESS      ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error deallocating AtmOptics structures', &
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
  !#                       -- DESTROY ComponentTest STRUCTURE --                #
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
  !#            -- DEALLOCATE THE INPUT Atmosphere STRUCTURE ARRAY --           #
  !#----------------------------------------------------------------------------#

  Error_Status = CRTM_Destroy_Atmosphere( Atmosphere )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error deallocating input Atmosphere structure array', &
                          WARNING )
  END IF

END PROGRAM Test_Adjoint


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2006/07/10 20:08:49 $
!
! $Revision: 1.1 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Test_Adjoint.f90,v $
! Revision 1.1  2006/07/10 20:08:49  dgroff
! This program test the AtmOptics adjoint code. The AtmOptics code combines AerosolScatter,
! CloudScatter and AtmAbsorption to calculate Optical Depth.
!
! Revision 1.7  2006/05/02 14:58:34  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.6  2005/08/12 20:56:07  paulv
! - Now using new ComponentTest definition and I/O modules.
! - Only layer variables are output.
! - Added the CRTM IRSSE emissivity coefficient file to the initialisation list.
!
! Revision 1.5  2005/03/28 15:59:49  paulv
! - Updated calls to CRTM_AtmAbsorption routines from subroutines to functions.
!
! Revision 1.4  2005/02/16 22:41:54  paulv
! - Updated to use new AerosolCoeff modules.
!
! Revision 1.3  2005/02/16 15:49:07  paulv
! - Updated to use new ComponentTest modules.
!
! Revision 1.2  2005/02/01 15:57:32  paulv
! - Replaced CRTM_SetUp_AtmAbsorption function calls with subroutine calls.
!
! Revision 1.1  2005/01/27 21:25:39  paulv
! Initial checkin.
!
!
!
!
