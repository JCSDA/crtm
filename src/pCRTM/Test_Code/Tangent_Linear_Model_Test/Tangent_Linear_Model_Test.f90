!------------------------------------------------------------------------------
!P+
! NAME:
!       Tangent_Linear_Model_Test
!
! PURPOSE:
!       Program to test the pCRTM Tangent-Linear component with respect
!       to the Forward component.
!
! CATEGORY:
!       pCRTM : Test 
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:             Module containing definitions for kinds
!                               of variable types.
!
!       Message_Handler:        Module to define simple error codes and
!                               handle error conditions
!                               USEs: FILE_UTILITY module
!
!       AtmProfile_Define:      Module defining the AtmProfile data structure
!                               and containing routines to manipulate it.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!
!       AtmProfile_netCDF_IO:   Module containing routines to read and write
!                               AtmProfile netCDF format files.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!                                     ATMPROFILE_DEFINE module
!                                     NETCDF module
!                                     NETCDF_UTILITY module
!
!       SensorInfo_Define:      Module defining the SensorInfo data structure and
!                               containing routines to manipulate it.
!                               USEs: TYPE_KINDS module
!                                     FILE_UTILITY module
!                                     ERROR_HANDLER module
!
!       SensorInfo_LinkedList:  Module defining the SensorInfo Linked List
!                               data structure and containing routines to
!                               manipulate it.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!                                     SENSORINFO_DEFINE module
!
!       SensorInfo_IO:          Module continaing routines to read and write ASCII
!                               SensorInfo format files.
!                               USEs: TYPE_KINDS module
!                                     FILE_UTILITY module
!                                     ERROR_HANDLER module
!                                     SENSORINFO_DEFINE module
!
!       Initialize:             Module for pCRTM initialisation.
!                               USEs: ERROR_HANDLER module
!                                     SPECTRAL_COEFFICIENTS module
!                                     TRANSMITTANCE_COEFFICIENTS module
!
!       Parameters:             Module to hold pCRTM parameter constants
!                               USEs: TYPE_KINDS module
!
!       Forward_Model:          Module containing the pCRTM forward component
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!                                     PARAMETERS module
!                                     SPECTRAL_COEFFICIENTS module
!                                     ABSORBER_PROFILE module
!                                     PREDICTORS module
!                                     TRANSMITTANCE module
!                                     RADIANCE module
!
!       Tangent_Linear_Model:   Module containing the pCRTM tangent-linear
!                               component
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!                                     PARAMETERS module
!                                     SPECTRAL_COEFFICIENTS module
!                                     ABSORBER_PROFILE module
!                                     PREDICTORS module
!                                     TRANSMITTANCE module
!                                     RADIANCE module
!                                     FORWARD_MODEL module
!
!       Spectral_Coefficients:  Module containing the pCRTM spectral
!                               coefficients.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!                                     PARAMETERS module
!                                     SPCCOEFF_DEFINE module
!                                     SPCCOEFF_BINARY_IO module
!
!       FWDTLMtest_Define:      Module defining the structure to hold the pCRTM
!                               tangent-linear test results containing
!                               routines to manipulate it.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!
!       FWDTLMtest_netCDF_IO:   Module containing routines to read and write
!                               netCDF format FWDTLMtest files.
!                               USEs: TYPE_KINDS
!                                     FILE_UTILITY
!                                     ERROR_HANDLER
!                                     FWDTLMTEST_DEFINE
!                                     NETCDF
!                                     NETCDF_UTILITY
!
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
!       Input:
!         - ASCII SensorInfo file
!         - netCDF AtmProfile file
!
!       Output:
!         - netCDF FWDTLMtest file
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 10-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2004 Paul van Delst
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

PROGRAM Tangent_Linear_Model_Test


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler

  USE AtmProfile_Define
  USE AtmProfile_netCDF_IO

  USE SensorInfo_Define
  USE SensorInfo_LinkedList
  USE SensorInfo_IO

  USE Initialize
  USE Parameters
  USE Forward_Model
  USE Tangent_Linear_Model

  USE Spectral_Coefficients  ! For access to SC structure

  USE FWDTLMtest_Define
  USE FWDTLMtest_netCDF_IO


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME   = 'Tangent_Linear_Model_Test'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: Tangent_Linear_Model_Test.f90,v 1.7 2006/05/02 14:58:35 dgroff Exp $'
  CHARACTER( * ),  PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  INTEGER, PARAMETER :: UNSET = 0
  INTEGER, PARAMETER ::   SET = 1

  INTEGER, PARAMETER :: H2O_ID = 1
  INTEGER, PARAMETER ::  O3_ID = 3

  REAL( fp_kind ), PARAMETER :: MIN_PERTURBATION = -0.1_fp_kind   ! +/- 10%
  REAL( fp_kind ), PARAMETER :: MAX_PERTURBATION =  0.1_fp_kind   ! +/- 10%
  INTEGER,         PARAMETER :: N_PERTURBATIONS  = 51

  INTEGER, PARAMETER :: N_LAYER_VARIABLES = 5
  INTEGER, PARAMETER :: N_SURFACE_VARIABLES = 4
  INTEGER, PARAMETER :: N_VARIABLES = N_LAYER_VARIABLES + N_SURFACE_VARIABLES
  INTEGER, PARAMETER :: NV_LEVEL_PRESSURE       = 1
  INTEGER, PARAMETER :: NV_LAYER_PRESSURE       = 2
  INTEGER, PARAMETER :: NV_LAYER_TEMPERATURE    = 3
  INTEGER, PARAMETER :: NV_LAYER_WATER_VAPOR    = 4
  INTEGER, PARAMETER :: NV_LAYER_OZONE          = 5
  INTEGER, PARAMETER :: NV_SURFACE_TEMPERATURE  = 6
  INTEGER, PARAMETER :: NV_SURFACE_EMISSIVITY   = 7
  INTEGER, PARAMETER :: NV_SURFACE_REFLECTIVITY = 8
  INTEGER, PARAMETER :: NV_SOLAR_REFLECTIVITY   = 9
  CHARACTER( * ), PARAMETER, DIMENSION( N_VARIABLES ) :: &
    VARIABLE_NAME = (/ 'Level pressure      ', &
                       'Layer pressure      ', &
                       'Layer temperature   ', &
                       'Layer water vapor   ', &
                       'Layer ozone         ', &
                       'Surface temperature ', &
                       'Surface emissivity  ', &
                       'Surface reflectivity', &
                       'Solar reflectivity  ' /)

  REAL( fp_kind ), PARAMETER ::  VIEW_ANGLE = 20.0_fp_kind
  REAL( fp_kind ), PARAMETER ::  FLUX_ANGLE = 53.13_fp_kind
  REAL( fp_kind ), PARAMETER :: SOLAR_ANGLE = 70.0_fp_kind


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  CHARACTER( 256 ) :: SensorInfo_Filename
  CHARACTER( 256 ) :: AtmProfile_Filename
  CHARACTER( 256 ) :: SpcCoeff_Filename
  CHARACTER( 256 ) :: TauCoeff_Filename
  CHARACTER( 256 ) :: FWDTLMtest_Filename

  INTEGER :: Error_Status
  INTEGER :: Allocate_Status

  INTEGER, DIMENSION( 1 ) :: Idx
  INTEGER :: H2O_Idx
  INTEGER :: O3_Idx

  INTEGER :: j, k, l, m, n, nP, nV, nLV, nSV

  INTEGER :: New_File

  CHARACTER( 256 ) :: Comment
  CHARACTER( 256 ) :: ID_Tag
  
  TYPE( AtmProfile_type ) :: AtmProfile

  INTEGER                      :: n_Sensors
  TYPE( SensorInfo_type )      :: SensorInfo
  TYPE( SensorInfo_List_type ) :: SensorInfo_List

  TYPE( FWDTLMtest_type ) :: FWDTLMtest

  ! -- Variable dimension determined during initialisation
  INTEGER :: n_Channels  ! L dimension

  ! -- Forward inputs
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Level_Pressure             ! K
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Layer_Pressure             ! K
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Layer_Temperature          ! K
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Layer_Water_Vapor          ! K
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Layer_Ozone                ! K
  REAL( fp_kind )                                 :: Surface_Temperature        ! Scalar
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Surface_Emissivity         ! L
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Surface_Reflectivity       ! L

  ! -- Tangent-linear inputs
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Level_Pressure_TL          ! K
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Layer_Pressure_TL          ! K
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Layer_Temperature_TL       ! K
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Layer_Water_Vapor_TL       ! K
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Layer_Ozone_TL             ! K
  REAL( fp_kind )                                 :: Surface_Temperature_TL     ! Scalar
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Surface_Emissivity_TL      ! L
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Surface_Reflectivity_TL    ! L

  ! -- Other inputs
  REAL( fp_kind )                                 :: Secant_View_Angle          ! Scalar
  REAL( fp_kind )                                 :: Secant_Solar_Angle         ! Scalar
  INTEGER                                         :: n_Channels_Per_Profile     ! Scalar
  INTEGER,         DIMENSION( : ),    ALLOCATABLE :: Channel_Index              ! L

  ! -- Forward outputs                                                                                
  REAL( fp_kind ), DIMENSION( :, : ), ALLOCATABLE :: Tau                        ! K x L
  REAL( fp_kind ), DIMENSION( :, : ), ALLOCATABLE :: Flux_Tau                   ! K x L
  REAL( fp_kind ), DIMENSION( :, : ), ALLOCATABLE :: Solar_Tau                  ! K x L
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Upwelling_Radiance         ! L
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Brightness_Temperature     ! L

  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Upwelling_Radiance_FWDBL     ! L
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Brightness_Temperature_FWDBL ! L

  ! -- Tangent-linear outputs                                                                                
  REAL( fp_kind ), DIMENSION( :, : ), ALLOCATABLE :: Tau_TL                     ! K x L
  REAL( fp_kind ), DIMENSION( :, : ), ALLOCATABLE :: Flux_Tau_TL                ! K x L
  REAL( fp_kind ), DIMENSION( :, : ), ALLOCATABLE :: Solar_Tau_TL               ! K x L
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Upwelling_Radiance_TL      ! L
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Brightness_Temperature_TL  ! L

  ! -- Optional inputs
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Solar_Reflectivity         ! L
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Solar_Reflectivity_TL      ! L
  REAL( fp_kind )                                 :: Secant_Flux_Angle          ! Scalar


  ! -- Perturbation fraction and amount
  REAL( fp_kind ) :: dPerturbation
  REAL( fp_kind ), DIMENSION( N_PERTURBATIONS ) :: Perturbation_Fraction



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a)' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to test the pCRTM Tangent-Linear component with")' )
  WRITE( *, '( 5x, "   respect to the Forward component.")' )
  WRITE( *, '(/5x, " $Revision: 1.7 $")' )
  WRITE( *, '( 5x, a, /)' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                       -- READ THE SensorInfo FILE --                       #
  !#----------------------------------------------------------------------------#

  ! ---------------------------
  ! Get the SensorInfo filename
  ! ---------------------------

  WRITE( *, FMT     = '( /5x, "Enter a SensorInfo filename: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) SensorInfo_fileNAME
  SensorInfo_fileNAME = ADJUSTL( SensorInfo_fileNAME )


  ! -------
  ! Read it
  ! -------

  Error_Status = Read_SensorInfo( SensorInfo_fileNAME, &
                                  SensorInfo_List, &
                                  Quiet = SET )
                               
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading SensorInfo file '//&
                          TRIM( SensorInfo_fileNAME ), &
                          FAILURE )
    STOP
  END IF


  ! ---------------------------
  ! Count the number of sensors
  ! ---------------------------

  n_Sensors = Count_SensorInfo_Nodes( SensorInfo_List )

  IF ( n_Sensors < 1 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'SensorInfo_List is empty.', &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                       -- READ THE AtmProfile FILE --                       #
  !#----------------------------------------------------------------------------#

  ! ---------------------------
  ! Get the AtmProfile filename
  ! ---------------------------

  WRITE( *, FMT     = '( /5x, "Enter an AtmProfile filename: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) AtmProfile_fileNAME
  AtmProfile_fileNAME = ADJUSTL( AtmProfile_fileNAME )


  ! -------
  ! Read it
  ! -------

  Error_Status = Read_AtmProfile_netCDF( TRIM( AtmProfile_fileNAME ), &
                                         AtmProfile, &
                                         ID_Tag = ID_Tag, &
                                         Reverse = 1 )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading AtmProfile file '//&
                          TRIM( AtmProfile_fileNAME ), &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#              -- ENTER A COMMENT STRING FOR THE OUTPUT FILE --              #
  !#----------------------------------------------------------------------------#

  WRITE( *, FMT     = '( /5x, "Enter a comment attribute string for the output file:" )' )
  READ( *, FMT = '( a )' ) Comment



  !#----------------------------------------------------------------------------#
  !#            -- ALLOCATE THE CHANNEL/SENSOR INDEPENDENT ARRAYS --            #
  !#----------------------------------------------------------------------------#

  ALLOCATE( Level_Pressure( AtmProfile%n_Layers ), &        ! K
            Layer_Pressure( AtmProfile%n_Layers ), &        ! K
            Layer_Temperature( AtmProfile%n_Layers ), &     ! K
            Layer_Water_Vapor( AtmProfile%n_Layers ), &     ! K
            Layer_Ozone( AtmProfile%n_Layers ), &           ! K

            Level_Pressure_TL( AtmProfile%n_Layers ), &     ! K
            Layer_Pressure_TL( AtmProfile%n_Layers ), &     ! K
            Layer_Temperature_TL( AtmProfile%n_Layers ), &  ! K
            Layer_Water_Vapor_TL( AtmProfile%n_Layers ), &  ! K
            Layer_Ozone_TL( AtmProfile%n_Layers ), &        ! K

            STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    Error_Status = FAILURE
    WRITE( Message, '( "Error allocating channel-independent arrays. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          Error_Status )
    STOP
  END IF  



  !#----------------------------------------------------------------------------#
  !#              -- ASSIGN THE PROFILE INDEPENDENT INPUT RTM DATA --           #
  !#----------------------------------------------------------------------------#

  ! --------------------
  ! Secant of the angles
  ! --------------------

  Secant_View_Angle  = ONE / COS( DEGREES_TO_RADIANS * VIEW_ANGLE )
  Secant_Flux_Angle  = ONE / COS( DEGREES_TO_RADIANS * FLUX_ANGLE )
  Secant_Solar_Angle = ONE / COS( DEGREES_TO_RADIANS * SOLAR_ANGLE )



  !#----------------------------------------------------------------------------#
  !#                    -- DETERMINE THE ABSORBER INDICES --                    #
  !#----------------------------------------------------------------------------#

  ! -----------
  ! Water vapor
  ! -----------

  IF ( COUNT( AtmProfile%Absorber_ID == H2O_ID ) /= 1 ) THEN
    Error_Status = FAILURE
    CALL Display_Message( PROGRAM_NAME, &
                          'No water Vapor data in AtmProfile structure.', &
                          Error_Status )
    STOP
  END IF

  Idx = PACK( (/ (j, j = 1, AtmProfile%n_Absorbers ) /), &
              AtmProfile%Absorber_ID == H2O_ID )

  H2O_Idx = Idx(1)


  ! -------------
  ! Ozone profile
  ! -------------

  IF ( COUNT( AtmProfile%Absorber_ID == O3_ID ) /= 1 ) THEN
    Error_Status = FAILURE
    CALL Display_Message( PROGRAM_NAME, &
                          'No ozone data in AtmProfile structure.', &
                          Error_Status )
    STOP
  END IF

  Idx = PACK( (/ (j, j = 1, AtmProfile%n_Absorbers ) /), &
              AtmProfile%Absorber_ID == O3_ID )

  O3_Idx = Idx(1)



  !#----------------------------------------------------------------------------#
  !#                    -- COMPUTE THE PERTURBATION ARRAY --                    #
  !#----------------------------------------------------------------------------#

  dPerturbation = ( MAX_PERTURBATION - MIN_PERTURBATION ) / REAL( N_PERTURBATIONS - 1, fp_kind )

  nP = 1
  DO n = -N_PERTURBATIONS/2, N_PERTURBATIONS/2
    Perturbation_Fraction(nP) = REAL( n, fp_kind ) * dPerturbation
    nP = nP + 1
  END DO



  !#----------------------------------------------------------------------------#
  !#                      -- BEGIN LOOP OVER SENSORS --                         #
  !#----------------------------------------------------------------------------#

  n_Sensor_Loop: DO n = 1, n_Sensors



    !#--------------------------------------------------------------------------#
    !#          -- GET THE CURRENT SensorInfo DATA FROM THE LIST --             #
    !#--------------------------------------------------------------------------#

    Error_Status = GetFrom_SensorInfo_List( SensorInfo_List, &
                                            n, &
                                            SensorInfo )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error retrieving SensorInfo data for sensor # ", i5 )' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF



    !#--------------------------------------------------------------------------#
    !#                  -- ONLY OPERATE ON FILES THAT EXIST --                  #
    !#--------------------------------------------------------------------------#

    ! -----------------------
    ! Construct the filenames
    ! -----------------------

    SpcCoeff_Filename = TRIM( SensorInfo%File_Prefix )//'.SpcCoeff.bin'
    TauCoeff_Filename = TRIM( SensorInfo%File_Prefix )//'.TauCoeff.bin'


    ! -----------------------
    ! Test for file existance
    ! -----------------------

    Available_Sensors: IF ( File_Exists( TRIM( SpcCoeff_Filename   ) ) .AND. &
                            File_Exists( TRIM( TauCoeff_Filename   ) )       ) THEN

      WRITE( *, '( //5x, "Testing Tangent-Linear pCRTM for ", a, 1x, a )' ) &
                TRIM( SensorInfo%Satellite_Name ), &
                TRIM( SensorInfo%Sensor_Name )


      ! -------------------------------
      ! Set the output filename and new
      ! flag for the current sensor
      ! -------------------------------

      FWDTLMtest_Filename = TRIM( SensorInfo%File_Prefix )//'.FWDTLMtest.nc'
      New_File = SET



      !#------------------------------------------------------------------------#
      !#             -- INITIALISE THE RTM FOR THE CURRENT SENSOR --            #
      !#------------------------------------------------------------------------#

      Error_Status = Initialize_RTM( Spectral_File = TRIM( SpcCoeff_FileNAME ), &
                                     Tau_File      = TRIM( TauCoeff_FileNAME )  )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error initialising pCRTM for '//&
                              TRIM( SensorInfo%Satellite_Name )//' '//&
                              TRIM( SensorInfo%Sensor_Name ), &
                              Error_Status )
        STOP
      END IF


      ! -------------------------------------------------
      ! Get the number of channels for the current sensor
      ! -------------------------------------------------

      CALL Get_Max_n_Channels( n_Channels )



      !#------------------------------------------------------------------------#
      !#   -- ALLOCATE THE CHANNEL-DEPENDENT ARRAYS FOR THE CURRENT SENSOR --   #
      !#------------------------------------------------------------------------#

      ALLOCATE( Surface_Emissivity( n_Channels ), &                ! L     
                Surface_Reflectivity( n_Channels ), &              ! L     
                Channel_Index( n_Channels ), &                     ! L     
                Tau( AtmProfile%n_Layers, n_Channels ), &          ! K x L 
                Flux_Tau( AtmProfile%n_Layers, n_Channels ), &     ! K x L 
                Solar_Tau( AtmProfile%n_Layers, n_Channels ), &    ! K x L 
                Upwelling_Radiance( n_Channels ), &                ! L     
                Brightness_Temperature( n_Channels ), &            ! L
                Solar_Reflectivity( n_Channels ), &                ! L     
     
                Surface_Emissivity_TL( n_Channels ), &             ! L     
                Surface_Reflectivity_TL( n_Channels ), &           ! L     
                Tau_TL( AtmProfile%n_Layers, n_Channels ), &       ! K x L 
                Flux_Tau_TL( AtmProfile%n_Layers, n_Channels ), &  ! K x L 
                Solar_Tau_TL( AtmProfile%n_Layers, n_Channels ), & ! K x L 
                Upwelling_Radiance_TL( n_Channels ), &             ! L     
                Brightness_Temperature_TL( n_Channels ), &         ! L     
                Solar_Reflectivity_TL( n_Channels ), &             ! L

                Upwelling_Radiance_FWDBL( n_Channels ), &          ! L     
                Brightness_Temperature_FWDBL( n_Channels ), &      ! L     

                STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error allocating channel-dependent arrays for ", a, 1x, a, ". STAT = ", i5 )' ) &
                        TRIM( SensorInfo%Satellite_Name ), &
                        TRIM( SensorInfo%Sensor_Name ), &
                        Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF



      !#------------------------------------------------------------------------#
      !#         -- ASSIGN THE CHANNEL/SENSOR DEPENDENT INPUT RTM DATA --       #
      !#------------------------------------------------------------------------#

      ! -------------------------------------------------
      ! Surface emissivity/reflectivity and channel index
      ! -------------------------------------------------

      DO l = 1, n_Channels

        Channel_Index( l ) = l

        IF ( SC%Is_Microwave_Channel( l ) == 1 ) THEN
          ! -- Specular microwave
          Surface_Emissivity( l )   = 0.6_fp_kind
          Surface_Reflectivity( l ) = ONE - Surface_Emissivity( l )
          Solar_Reflectivity( l )   = ZERO
        ELSE
          ! -- Isotropic infrared
          Surface_Emissivity( l )   = 0.90_fp_kind
          Surface_Reflectivity( l ) = ( ONE - Surface_Emissivity( l ) ) / PI
          Solar_Reflectivity( l )   = ( ONE - 0.85_fp_kind ) / PI
        END IF

      END DO


      ! ------------------------------------
      ! All sensor channels for each profile
      ! ------------------------------------

      n_Channels_Per_Profile = n_Channels



      !#------------------------------------------------------------------------#
      !#                    -- ALLOCATE FWDTLMtest STRUCTURE --                 #
      !#------------------------------------------------------------------------#

      Error_Status = Allocate_FWDTLMtest( AtmProfile%n_Layers, &
                                          n_Channels, &
                                          N_PERTURBATIONS, &
                                          N_LAYER_VARIABLES, &
                                          N_SURFACE_VARIABLES, &
                                          FWDTLMtest )

      IF ( Error_Status /= SUCCESS ) THEN 
         CALL Display_Message( PROGRAM_NAME, &
                               'Error occurred allocating FWDTLMtest for '//&
                               TRIM( SensorInfo%Satellite_Name )//' '//&
                               TRIM( SensorInfo%Sensor_Name ), &
                                Error_Status )                           
       STOP
      END IF


      ! -------------------------
      ! Assign data to FWDTLMtest
      ! -------------------------

      FWDTLMtest%Output_Variable_Name  = 'Tb'
      FWDTLMtest%Output_Variable_Units = '(K)'

      FWDTLMtest%Sensor_Channel = SensorInfo%Sensor_Channel
      FWDTLMtest%Variable_Name  = VARIABLE_NAME
      FWDTLMtest%Perturbation   = Perturbation_Fraction



      !#------------------------------------------------------------------------#
      !#                        -- BEGIN PROFILE LOOP --                        #
      !#------------------------------------------------------------------------#

      Profile_Loop: DO m = 1, AtmProfile%n_Profiles


        WRITE( *, '( /10x, "Profile #", i5 )' ) m



        !#----------------------------------------------------------------------#
        !#            -- ASSIGN PROFILE DEPENDENT INPUT RTM DATA --             #
        !#----------------------------------------------------------------------#

        ! -- The pressure profiles
        Level_Pressure = AtmProfile%Level_Pressure(2:AtmProfile%n_Levels,m)
        Layer_Pressure = AtmProfile%Layer_Pressure(:,m)

        ! -- The temperature profile
        Layer_Temperature = AtmProfile%Layer_Temperature(:,m)


        ! -- The water vapor profile
        Layer_Water_Vapor = AtmProfile%Layer_Absorber( :, H2O_Idx, m )


        ! -- Ozone profile
        Layer_Ozone = AtmProfile%Layer_Absorber( :, O3_Idx, m )


        ! -- Surface temperature
        Surface_Temperature = AtmProfile%Level_Temperature( AtmProfile%n_Levels, m )



        !#------------------------------------------------------------------------#
        !#               -- FORWARD MODEL BASELINE COMPUTATION --                 #
        !#------------------------------------------------------------------------#

        Error_Status = Compute_RTM( Level_Pressure,               &  ! Input, K
                                    Layer_Pressure,               &  ! Input, K 
                                    Layer_Temperature,            &  ! Input, K 
                                    Layer_Water_Vapor,            &  ! Input, K 
                                    Layer_Ozone,                  &  ! Input, K    
                                    Surface_Temperature,          &  ! Input, Scalar 
                                    Surface_Emissivity,           &  ! Input, L   
                                    Surface_Reflectivity,         &  ! Input, L     
                                    Secant_View_Angle,            &  ! Input, Scalar
                                    Secant_Solar_Angle,           &  ! Input, Scalar
                                    n_Channels_Per_Profile,       &  ! Input, Scalar
                                    Channel_Index,                &  ! Input, L 
                                    Tau,                          &  ! Output, K x L  
                                    Flux_Tau,                     &  ! Output, K x L  
                                    Solar_Tau,                    &  ! Output, K x L  
                                    Upwelling_Radiance_FWDBL,     &  ! Output, L
                                    Brightness_Temperature_FWDBL, &  ! Output, L
                                    Solar_Reflectivity = Solar_Reflectivity, &  ! Optional input, L
                                    Secant_Flux_Angle  = Secant_Flux_Angle   )  ! Optional input, Scalar

        IF ( Error_Status /= SUCCESS ) THEN 
           CALL Display_Message( PROGRAM_NAME, &
                                 'Error in BASELINE Compute_RTM call', & 
                                  Error_Status )                           
         STOP
        END IF



        !#----------------------------------------------------------------------#
        !#                       -- BEGIN THE VARIABLE LOOP --                  #
        !#----------------------------------------------------------------------#

        Variable_Loop: DO nV = 1, N_VARIABLES

          WRITE( *, '( 5x, "Perturbation variable: ", a )' ) VARIABLE_NAME(nV)



          !#--------------------------------------------------------------------#
          !#                  -- BEGIN THE PERTURBATION LOOP --                 #
          !#--------------------------------------------------------------------#

          Perturbation_Loop: DO nP = 1, N_PERTURBATIONS



            !#------------------------------------------------------------------#
            !#                  -- PERTURB THE VARIABLES --                     #
            !#------------------------------------------------------------------#

            Variable_Select: SELECT CASE ( nV )


              ! -------------------
              ! The layer variables
              ! -------------------

              CASE( NV_LEVEL_PRESSURE, &
                    NV_LAYER_PRESSURE, &
                    NV_LAYER_TEMPERATURE, &
                    NV_LAYER_WATER_VAPOR, &
                    NV_LAYER_OZONE  )


                ! -- Loop over the layers
                Layer_Loop: DO k = 1, AtmProfile%n_Layers

                  ! -- Reinitialise all input TL arrays
                  Level_Pressure_TL       = ZERO
                  Layer_Pressure_TL       = ZERO
                  Layer_Temperature_TL    = ZERO
                  Layer_Water_Vapor_TL    = ZERO
                  Layer_Ozone_TL          = ZERO
                  Surface_Temperature_TL  = ZERO
                  Surface_Emissivity_TL   = ZERO
                  Surface_Reflectivity_TL = ZERO
                  Solar_Reflectivity_TL   = ZERO

                  ! -- Perturb the TL inputs
                  SELECT CASE ( nV )
                    CASE ( NV_LEVEL_PRESSURE )
                      IF ( k == 1 ) THEN
                        Level_Pressure_TL( k ) = Perturbation_Fraction(nP) * ( Level_Pressure( k ) - TOA_PRESSURE )
                      ELSE
                        Level_Pressure_TL( k ) = Perturbation_Fraction(nP) * ( Level_Pressure( k ) - Level_Pressure( k-1 ) )
                      END IF
                    CASE ( NV_LAYER_PRESSURE )
                      IF ( k == 1 ) THEN
                        Layer_Pressure_TL( k ) = Perturbation_Fraction(nP) * ( Layer_Pressure( k ) - TOA_PRESSURE )
                      ELSE
                        Layer_Pressure_TL( k ) = Perturbation_Fraction(nP) * ( Layer_Pressure( k ) - Layer_Pressure( k-1 ) )
                      END IF
                    CASE ( NV_LAYER_TEMPERATURE )
                      Layer_Temperature_TL( k ) = Perturbation_Fraction(nP) * Layer_Temperature( k )
                    CASE ( NV_LAYER_WATER_VAPOR )
                      Layer_Water_Vapor_TL( k ) = Perturbation_Fraction(nP) * Layer_Water_Vapor( k )
                    CASE ( NV_LAYER_OZONE )
                      Layer_Ozone_TL( k )       = Perturbation_Fraction(nP) * Layer_Ozone( k )
                  END SELECT

                  nLV = nV

                  ! -- Call the tangent-linear model
                  Error_Status = Compute_RTM_TL( Level_Pressure,                    &  ! Input, K
                                                 Layer_Pressure,                    &  ! Input, K
                                                 Layer_Temperature,                 &  ! Input, K
                                                 Layer_Water_Vapor,                 &  ! Input, K
                                                 Layer_Ozone,                       &  ! Input, K   
                                                 Surface_Temperature,               &  ! Input, Scalar
                                                 Surface_Emissivity,                &  ! Input, L  
                                                 Surface_Reflectivity,              &  ! Input, L
                                                 Level_Pressure_TL,                 &  ! Input, K
                                                 Layer_Pressure_TL,                 &  ! Input, K 
                                                 Layer_Temperature_TL,              &  ! Input, K 
                                                 Layer_Water_vapor_TL,              &  ! Input, K 
                                                 Layer_Ozone_TL,                    &  ! Input, K   
                                                 Surface_Temperature_TL,            &  ! Input, Scalar 
                                                 Surface_Emissivity_TL,             &  ! Input, L   
                                                 Surface_Reflectivity_TL,           &  ! Input, L
                                                 Secant_View_Angle,                 &  ! Input, Scalar
                                                 Secant_Solar_Angle,                &  ! Input, Scalar
                                                 n_Channels_Per_Profile,            &  ! Input, Scalar
                                                 Channel_Index,                     &  ! Input, L 
                                                 Tau, Flux_Tau, Solar_Tau,          &  ! Output, K x L
                                                 Upwelling_Radiance,                &  ! Output, L
                                                 Brightness_Temperature,            &  ! Output, L
                                                 Tau_TL, Flux_Tau_TL, Solar_Tau_TL, &  ! Output, K x L
                                                 Upwelling_Radiance_TL,             &  ! Output, L
                                                 Brightness_Temperature_TL,         &  ! Output, L
                                                 Solar_Reflectivity    = Solar_Reflectivity,    &  ! Optional input, L
                                                 Solar_Reflectivity_TL = Solar_Reflectivity_TL, &  ! Optional input, L
                                                 Secant_Flux_Angle     = Secant_Flux_Angle      )  ! Optional input, Scalar

                  IF ( Error_Status /= SUCCESS ) THEN
                     WRITE( Message, '( "Error in ", a, " TL run at layer# ", i5 )' ) TRIM( VARIABLE_NAME( nV ) ), k
                     CALL Display_Message( PROGRAM_NAME, &
                                           TRIM( Message ), & 
                                            Error_Status )                           
                   STOP
                  END IF


                  ! -- Call the forward  model
                  Error_Status = Compute_RTM( Level_Pressure + Level_Pressure_TL,              &  ! Input, K
                                              Layer_Pressure + Layer_Pressure_TL,              &  ! Input, K 
                                              Layer_Temperature + Layer_Temperature_TL,        &  ! Input, K 
                                              Layer_Water_Vapor + Layer_Water_Vapor_TL,        &  ! Input, K 
                                              Layer_Ozone + Layer_Ozone_TL,                    &  ! Input, K 
                                              Surface_Temperature + Surface_Temperature_TL,    &  ! Input, Scalar 
                                              Surface_Emissivity + Surface_Emissivity_TL,      &  ! Input, L   
                                              Surface_Reflectivity + Surface_Reflectivity_TL,  &  ! Input, L     
                                              Secant_View_Angle,                               &  ! Input, Scalar
                                              Secant_Solar_Angle,                              &  ! Input, Scalar
                                              n_Channels_Per_Profile,                          &  ! Input, Scalar
                                              Channel_Index,                                   &  ! Input, L 
                                              Tau,                                             &  ! Output, K x L 
                                              Flux_Tau,                                        &  ! Output, K x L 
                                              Solar_Tau,                                       &  ! Output, K x L 
                                              Upwelling_Radiance,                              &  ! Output, L
                                              Brightness_Temperature,                          &  ! Output, L
                                              Solar_Reflectivity = Solar_Reflectivity,         &  ! Optional input, L
                                              Secant_Flux_Angle  = Secant_Flux_Angle           )  ! Optional input, Scalar

                  IF ( Error_Status /= SUCCESS ) THEN
                     WRITE( Message, '( "Error in ", a, " FWD run at layer# ", i5 )' ) TRIM( VARIABLE_NAME( nV ) ), k
                     CALL Display_Message( PROGRAM_NAME, &
                                           TRIM( Message ), & 
                                            Error_Status )                           
                   STOP
                  END IF


                  ! -- Save the data for this profile
                  FWDTLMtest%dLayer_TL( k, :, nP, nLV ) = Brightness_Temperature_TL
                  FWDTLMtest%dLayer_NL( k, :, nP, nLV ) = Brightness_Temperature - Brightness_Temperature_FWDBL

                END DO Layer_Loop



              ! ---------------------
              ! The surface variables
              ! ---------------------

              CASE DEFAULT

                ! -- Reinitialise all input TL arrays
                Level_Pressure_TL       = ZERO
                Layer_Pressure_TL       = ZERO
                Layer_Temperature_TL    = ZERO
                Layer_Water_Vapor_TL    = ZERO
                Layer_Ozone_TL          = ZERO
                Surface_Temperature_TL  = ZERO
                Surface_Emissivity_TL   = ZERO
                Surface_Reflectivity_TL = ZERO
                Solar_Reflectivity_TL   = ZERO


                ! -- Perturb the TL inputs
                SELECT CASE ( nV )
                  CASE ( NV_SURFACE_TEMPERATURE )
                    Surface_Temperature_TL  = Perturbation_Fraction(nP) * Surface_Temperature
                  CASE ( NV_SURFACE_EMISSIVITY )
                    Surface_Emissivity_TL   = Perturbation_Fraction(nP) * Surface_Emissivity
                  CASE ( NV_SURFACE_REFLECTIVITY )
                    Surface_Reflectivity_TL = Perturbation_Fraction(nP) * Surface_Reflectivity
                  CASE ( NV_SOLAR_REFLECTIVITY )
                    Solar_Reflectivity_TL   = Perturbation_Fraction(nP) * Solar_Reflectivity
                END SELECT

                nSV = nV - N_LAYER_VARIABLES


                ! -- Call the tangent-linear model
                Error_Status = Compute_RTM_TL( Level_Pressure,                    &  ! Input, K
                                               Layer_Pressure,                    &  ! Input, K
                                               Layer_Temperature,                 &  ! Input, K
                                               Layer_Water_Vapor,                 &  ! Input, K
                                               Layer_Ozone,                       &  ! Input, K   
                                               Surface_Temperature,               &  ! Input, Scalar
                                               Surface_Emissivity,                &  ! Input, L  
                                               Surface_Reflectivity,              &  ! Input, L
                                               Level_Pressure_TL,                 &  ! Input, K
                                               Layer_Pressure_TL,                 &  ! Input, K 
                                               Layer_Temperature_TL,              &  ! Input, K 
                                               Layer_Water_vapor_TL,              &  ! Input, K 
                                               Layer_Ozone_TL,                    &  ! Input, K   
                                               Surface_Temperature_TL,            &  ! Input, Scalar 
                                               Surface_Emissivity_TL,             &  ! Input, L   
                                               Surface_Reflectivity_TL,           &  ! Input, L
                                               Secant_View_Angle,                 &  ! Input, Scalar
                                               Secant_Solar_Angle,                &  ! Input, Scalar
                                               n_Channels_Per_Profile,            &  ! Input, Scalar
                                               Channel_Index,                     &  ! Input, L 
                                               Tau, Flux_Tau, Solar_Tau,          &  ! Output, K x L
                                               Upwelling_Radiance,                &  ! Output, L
                                               Brightness_Temperature,            &  ! Output, L
                                               Tau_TL, Flux_Tau_TL, Solar_Tau_TL, &  ! Output, K x L
                                               Upwelling_Radiance_TL,             &  ! Output, L
                                               Brightness_Temperature_TL,         &  ! Output, L
                                               Solar_Reflectivity    = Solar_Reflectivity,    &  ! Optional input, L
                                               Solar_Reflectivity_TL = Solar_Reflectivity_TL, &  ! Optional input, L
                                               Secant_Flux_Angle     = Secant_Flux_Angle      )  ! Optional input, Scalar

                IF ( Error_Status /= SUCCESS ) THEN
                   WRITE( Message, '( "Error in ", a, " TL run" )' ) TRIM( VARIABLE_NAME( nV ) )
                   CALL Display_Message( PROGRAM_NAME, &
                                         TRIM( Message ), & 
                                          Error_Status )                           
                 STOP
                END IF


                ! -- Call the forward  model
                Error_Status = Compute_RTM( Level_Pressure + Level_Pressure_TL,              &  ! Input, K
                                            Layer_Pressure + Layer_Pressure_TL,              &  ! Input, K 
                                            Layer_Temperature + Layer_Temperature_TL,        &  ! Input, K 
                                            Layer_Water_Vapor + Layer_Water_Vapor_TL,        &  ! Input, K 
                                            Layer_Ozone + Layer_Ozone_TL,                    &  ! Input, K 
                                            Surface_Temperature + Surface_Temperature_TL,    &  ! Input, Scalar 
                                            Surface_Emissivity + Surface_Emissivity_TL,      &  ! Input, L   
                                            Surface_Reflectivity + Surface_Reflectivity_TL,  &  ! Input, L     
                                            Secant_View_Angle,                               &  ! Input, Scalar
                                            Secant_Solar_Angle,                              &  ! Input, Scalar
                                            n_Channels_Per_Profile,                          &  ! Input, Scalar
                                            Channel_Index,                                   &  ! Input, L 
                                            Tau,                                             &  ! Output, K x L 
                                            Flux_Tau,                                        &  ! Output, K x L 
                                            Solar_Tau,                                       &  ! Output, K x L 
                                            Upwelling_Radiance,                              &  ! Output, L  
                                            Brightness_Temperature,                          &  ! Output, L
                                            Solar_Reflectivity = Solar_Reflectivity,         &  ! Optional input, L
                                            Secant_Flux_Angle  = Secant_Flux_Angle           )  ! Optional input, Scalar

                IF ( Error_Status /= SUCCESS ) THEN
                   WRITE( Message, '( "Error in ", a, " FWD run" )' ) TRIM( VARIABLE_NAME( nV ) )
                   CALL Display_Message( PROGRAM_NAME, &
                                         TRIM( Message ), & 
                                          Error_Status )                           
                 STOP
                END IF


                ! -- Save the data for this profile
                FWDTLMtest%dSurface_TL( :, nP, nSV ) = Brightness_Temperature_TL
                FWDTLMtest%dSurface_NL( :, nP, nSV ) = Brightness_Temperature - Brightness_Temperature_FWDBL

            END SELECT Variable_Select

          END DO Perturbation_Loop

        END DO Variable_Loop



        !#----------------------------------------------------------------------#
        !#     -- WRITE THE CURRENT PROFILE FWDTLMtest STRUCTURE TO FILE --     #
        !#----------------------------------------------------------------------#

        WRITE( *, '( 10x, "Writing FWD/TL data to output file...." )' )


        ! ------------------------------------------------------
        ! Set the current profile number in the output structure
        ! ------------------------------------------------------

        FWDTLMtest%Profile_Number = m


        ! --------------
        ! Write the data
        ! --------------

        Error_Status = Write_FWDTLMtest_netCDF( TRIM( FWDTLMtest_Filename ), &
                                                FWDTLMtest, &
                                                New_File = New_File, &
                                                Title         = 'FWD/TL pCRTM test results for '//&
                                                                TRIM( SensorInfo%Satellite_Name )//' '//&
                                                                TRIM( SensorInfo%Sensor_Name ), &
                                                History       = PROGRAM_RCS_ID, &
                                                Sensor_Name   = TRIM( SensorInfo%Sensor_Name ), &
                                                Platform_Name = TRIM( SensorInfo%Satellite_Name ), &
                                                Comment       = TRIM( Comment ), &
                                                ID_Tag        = TRIM( ID_Tag ) )

        IF ( Error_Status /= SUCCESS ) THEN 
          WRITE( Message, '( "Error writing FWDTLMtest structure for profile #", i5, " to ", a )' ) &
                          m, TRIM( FWDTLMtest_Filename )
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                 Error_Status )                           
          STOP
        END IF


        ! -----------------------
        ! Reset the new file flag
        ! -----------------------

        IF ( New_File == SET ) New_File = UNSET

      END DO Profile_Loop



      !#------------------------------------------------------------------------#
      !#                    -- DESTROY FWDTLMtest STRUCTURE --                  #
      !#------------------------------------------------------------------------#

      Error_Status = Destroy_FWDTLMtest( FWDTLMtest )

      IF ( Error_Status /= SUCCESS ) THEN 
         CALL Display_Message( PROGRAM_NAME, &
                               'Error occurred destroying FWDTLMtest for '//&
                               TRIM( SensorInfo%Satellite_Name )//' '//&
                               TRIM( SensorInfo%Sensor_Name ), &
                                Error_Status )                           
       STOP
      END IF


      !#------------------------------------------------------------------------#
      !#  -- DEALLOCATE THE CHANNEL-DEPENDENT ARRAYS FOR THE CURRENT SENSOR --  #
      !#------------------------------------------------------------------------#

      DEALLOCATE( Surface_Emissivity, &
                  Surface_Reflectivity, &
                  Channel_Index, &
                  Tau, &
                  Flux_Tau, &
                  Solar_Tau, &
                  Upwelling_Radiance, &
                  Brightness_Temperature, &
                  Solar_Reflectivity, &

                  Surface_Emissivity_TL, &
                  Surface_Reflectivity_TL, &
                  Tau_TL, &
                  Flux_Tau_TL, &
                  Solar_Tau_TL, &
                  Upwelling_Radiance_TL, &
                  Brightness_Temperature_TL, &
                  Solar_Reflectivity_TL, &

                  Upwelling_Radiance_FWDBL, &
                  Brightness_Temperature_FWDBL, &

                  STAT = Allocate_Status )
 
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating channel-dependent arrays for ", a, 1x, a, ". STAT = ", i5 )' ) &
                        TRIM( SensorInfo%Satellite_Name ), TRIM( SensorInfo%Sensor_Name ), Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF



      !#------------------------------------------------------------------------#
      !#               -- DESTROY THE RTM FOR THE CURRENT SENSOR --             #
      !#------------------------------------------------------------------------#

      Error_Status = Destroy_RTM( )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error destroying pCRTM for '//&
                              TRIM( SensorInfo%Satellite_Name )//' '//&
                              TRIM( SensorInfo%Sensor_Name ), &
                              Error_Status )
        STOP
      END IF

    END IF Available_Sensors



    !#------------------------------------------------------------------------#
    !#                   -- DESTROY SensorInfo STRUCTURE --                   #
    !#------------------------------------------------------------------------#

    Error_Status = Destroy_SensorInfo( SensorInfo )

    IF ( Error_Status /= SUCCESS ) THEN 
      Error_Status = FAILURE
      WRITE( Message, '( "Error destroying SensorInfo structure data for sensor # ", i5 )' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
     STOP
    END IF

  END DO n_Sensor_Loop



  !#----------------------------------------------------------------------------#
  !#           -- DEALLOCATE THE CHANNEL/SENSOR INDEPENDENT ARRAYS --           #
  !#----------------------------------------------------------------------------#

  DEALLOCATE( Level_Pressure, &
              Layer_Pressure, &
              Layer_Temperature, &
              Layer_Water_Vapor, &
              Layer_Ozone, &

              Level_Pressure_TL, &
              Layer_Pressure_TL, &
              Layer_Temperature_TL, &
              Layer_Water_Vapor_TL, &
              Layer_Ozone_TL, &

              STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    Error_Status = FAILURE
    WRITE( Message, '( "Error deallocating channel-independent arrays. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          Error_Status )
    STOP
  END IF  



  !#----------------------------------------------------------------------------#
  !#                   -- DESTROY THE AtmProfile STRUCTURE --                   #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_AtmProfile( AtmProfile )

  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying AtmProfile structure.', &
                          WARNING )
  END IF



  !#----------------------------------------------------------------------------#
  !#                  -- DESTROY THE SensorInfo LINKED LIST --                  #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_SensorInfo_List( SensorInfo_List )

  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying SensorInfo linked list.', &
                          WARNING )
  END IF

END PROGRAM Tangent_Linear_Model_Test


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Tangent_Linear_Model_Test.f90,v 1.7 2006/05/02 14:58:35 dgroff Exp $
!
! $Date: 2006/05/02 14:58:35 $
!
! $Revision: 1.7 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Tangent_Linear_Model_Test.f90,v $
! Revision 1.7  2006/05/02 14:58:35  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.6  2005/02/09 17:54:34  paulv
! - Now using updated test output structure with generic output variable
!   names.
!
! Revision 1.5  2004/12/28 20:22:05  paulv
! - Small corrections made.
!
! Revision 1.4  2004/12/28 19:54:57  paulv
! - Updated for category change.
!
! Revision 1.3  2004/10/05 22:32:38  paulv
! - Code modified to process and output a single profile at a time rather
!   than all profiles at once.
!
! Revision 1.2  2004/09/27 14:39:21  paulv
! - Output now uses FWDTLMtest_netCDF_IO module.
!
! Revision 1.1  2004/06/14 21:07:55  paulv
! Initial checkin.
!
!
!
