!------------------------------------------------------------------------------
!P+
! NAME:
!       Adjoint_Model_Test
!
! PURPOSE:
!       Program to test the pCRTM Adjoint component with respect to the
!       Tangent-Linear component.
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
!       Adjoint_Model:          Module containing the pCRTM adjoint
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
!       TLADMtest_Define:       Module defining the structure to hold the pCRTM
!                               tangent-linear test results containing
!                               routines to manipulate it.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!
!       TLADMtest_netCDF_IO:    Module containing routines to read and write
!                               netCDF format TLADMtest files.
!                               USEs: TYPE_KINDS
!                                     FILE_UTILITY
!                                     ERROR_HANDLER
!                                     TLADMTEST_DEFINE
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
!         - netCDF TLADMtest file
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

PROGRAM Adjoint_Model_Test


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
  USE Tangent_Linear_Model
  USE Adjoint_Model

  USE Spectral_Coefficients  ! For access to SC structure

  USE TLADMtest_Define
  USE TLADMtest_netCDF_IO


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME   = 'Adjoint_Model_Test'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: Adjoint_Model_Test.f90,v 1.7 2006/05/02 14:58:35 dgroff Exp $'
  CHARACTER( * ),  PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  INTEGER, PARAMETER :: UNSET = 0
  INTEGER, PARAMETER ::   SET = 1

  INTEGER, PARAMETER :: H2O_ID = 1
  INTEGER, PARAMETER ::  O3_ID = 3

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
  CHARACTER( 256 ) :: TLADMtest_Filename

  INTEGER :: Error_Status
  INTEGER :: Allocate_Status

  INTEGER, DIMENSION( 1 ) :: Idx
  INTEGER :: H2O_Idx
  INTEGER :: O3_Idx

  INTEGER :: j, k, l, m, n, nV

  INTEGER :: New_File

  CHARACTER( 256 ) :: Comment
  CHARACTER( 256 ) :: ID_Tag
  
  TYPE( AtmProfile_type ) :: AtmProfile

  INTEGER                      :: n_Sensors
  TYPE( SensorInfo_type )      :: SensorInfo
  TYPE( SensorInfo_List_type ) :: SensorInfo_List

  TYPE( TLADMtest_type ) :: TLADMtest

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

  ! -- Adjoint outputs
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Level_Pressure_AD          ! K
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Layer_Pressure_AD          ! K
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Layer_Temperature_AD       ! K
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Layer_Water_Vapor_AD       ! K
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Layer_Ozone_AD             ! K
  REAL( fp_kind )                                 :: Surface_Temperature_AD     ! Scalar
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Surface_Emissivity_AD      ! L
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Surface_Reflectivity_AD    ! L

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

  ! -- Tangent-linear outputs                                                                                
  REAL( fp_kind ), DIMENSION( :, : ), ALLOCATABLE :: Tau_TL                     ! K x L
  REAL( fp_kind ), DIMENSION( :, : ), ALLOCATABLE :: Flux_Tau_TL                ! K x L
  REAL( fp_kind ), DIMENSION( :, : ), ALLOCATABLE :: Solar_Tau_TL               ! K x L
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Upwelling_Radiance_TL      ! L  
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Brightness_Temperature_TL  ! L  

  ! -- Adjoint inputs                                                                              
  REAL( fp_kind ), DIMENSION( :, : ), ALLOCATABLE :: Tau_AD                     ! K x L
  REAL( fp_kind ), DIMENSION( :, : ), ALLOCATABLE :: Flux_Tau_AD                ! K x L
  REAL( fp_kind ), DIMENSION( :, : ), ALLOCATABLE :: Solar_Tau_AD               ! K x L
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Upwelling_Radiance_AD      ! L  
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Brightness_Temperature_AD  ! L  

  ! -- Optional forward inputs
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Solar_Reflectivity         ! L 
  REAL( fp_kind )                                 :: Secant_Flux_Angle          ! Scalar

  ! -- Optional tangent-linear inputs
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Solar_Reflectivity_TL      ! L 

  ! -- Optional adjoint outputs
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Solar_Reflectivity_AD      ! L 



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a)' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to test the pCRTM Adjoint component with")' )
  WRITE( *, '( 5x, "   respect to the Tangent-Linear component.")' )
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
  READ( *, FMT = '( a )' ) SensorInfo_Filename
  SensorInfo_Filename = ADJUSTL( SensorInfo_Filename )


  ! -------
  ! Read it
  ! -------

  Error_Status = Read_SensorInfo( SensorInfo_Filename, &
                                  SensorInfo_List, &
                                  Quiet = SET )
                               
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading SensorInfo file '//&
                          TRIM( SensorInfo_Filename ), &
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
  READ( *, FMT = '( a )' ) AtmProfile_Filename
  AtmProfile_Filename = ADJUSTL( AtmProfile_Filename )


  ! -------
  ! Read it
  ! -------

  Error_Status = Read_AtmProfile_netCDF( TRIM( AtmProfile_Filename ), &
                                         AtmProfile, &
                                         ID_Tag = ID_Tag, &
                                         Reverse = 1 )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading AtmProfile file '//&
                          TRIM( AtmProfile_Filename ), &
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

            Level_Pressure_AD( AtmProfile%n_Layers ), &     ! K
            Layer_Pressure_AD( AtmProfile%n_Layers ), &     ! K
            Layer_Temperature_AD( AtmProfile%n_Layers ), &  ! K
            Layer_Water_Vapor_AD( AtmProfile%n_Layers ), &  ! K
            Layer_Ozone_AD( AtmProfile%n_Layers ), &        ! K

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

      WRITE( *, '( //5x, "Testing Adjoint pCRTM for ", a, 1x, a )' ) &
                TRIM( SensorInfo%Satellite_Name ), &
                TRIM( SensorInfo%Sensor_Name )


      ! -------------------------------
      ! Set the output filename and new
      ! flag for the current sensor
      ! -------------------------------

      TLADMtest_Filename = TRIM( SensorInfo%File_Prefix )//'.TLADMtest.nc'
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

      ALLOCATE( Surface_Emissivity( n_Channels ), &                    
                Surface_Reflectivity( n_Channels ), &
                Channel_Index( n_Channels ), &
                Tau( AtmProfile%n_Layers, n_Channels ), &
                Flux_Tau( AtmProfile%n_Layers, n_Channels ), &
                Solar_Tau( AtmProfile%n_Layers, n_Channels ), &
                Upwelling_Radiance( n_Channels ), &
                Brightness_Temperature( n_Channels ), &
                Solar_Reflectivity( n_Channels ), &

                Surface_Emissivity_TL( n_Channels ), &
                Surface_Reflectivity_TL( n_Channels ), &
                Tau_TL( AtmProfile%n_Layers, n_Channels ), &
                Flux_Tau_TL( AtmProfile%n_Layers, n_Channels ), &
                Solar_Tau_TL( AtmProfile%n_Layers, n_Channels ), &
                Upwelling_Radiance_TL( n_Channels ), &
                Brightness_Temperature_TL( n_Channels ), &
                Solar_Reflectivity_TL( n_Channels ), &

                Surface_Emissivity_AD( n_Channels ), &
                Surface_Reflectivity_AD( n_Channels ), &
                Tau_AD( AtmProfile%n_Layers, n_Channels ), &
                Flux_Tau_AD( AtmProfile%n_Layers, n_Channels ), &
                Solar_Tau_AD( AtmProfile%n_Layers, n_Channels ), &
                Upwelling_Radiance_AD( n_Channels ), &
                Brightness_Temperature_AD( n_Channels ), &
                Solar_Reflectivity_AD( n_Channels ), &

                STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error allocating channel-dependent arrays for ", a, 1x, a, ". STAT = ", i5 )' ) &
                        TRIM( SensorInfo%Satellite_Name ), TRIM( SensorInfo%Sensor_Name ), Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF



      !#------------------------------------------------------------------------#
      !#        -- ASSIGN THE CHANNEL/SENSOR DEPENDENT INPUT RTM DATA --        #
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
          Surface_Emissivity( l )   = 0.96_fp_kind
          Surface_Reflectivity( l ) = ( ONE - Surface_Emissivity( l ) ) / PI
          Solar_Reflectivity( l )   = ( ONE - 0.85_fp_kind ) / PI
        END IF

      END DO


      ! ------------------------------------
      ! All sensor channels for each profile
      ! ------------------------------------

      n_Channels_Per_Profile = n_Channels




      !#------------------------------------------------------------------------#
      !#                   -- ALLOCATE TLADMtest STRUCTURE --                   #
      !#------------------------------------------------------------------------#

      Error_Status = Allocate_TLADMtest( AtmProfile%n_Layers, &
                                         n_Channels, &
                                         N_LAYER_VARIABLES, &
                                         N_SURFACE_VARIABLES, &
                                         TLADMtest )

      IF ( Error_Status /= SUCCESS ) THEN 
         CALL Display_Message( PROGRAM_NAME, &
                               'Error occurred allocating TLADMtest for '//&
                               TRIM( SensorInfo%Satellite_Name )//' '//&
                               TRIM( SensorInfo%Sensor_Name ), &
                                Error_Status )                           
       STOP
      END IF


      ! ------------------------
      ! Assign data to TLADMtest
      ! ------------------------

      TLADMtest%Output_Variable_Name  = 'Tb'
      TLADMtest%Output_Variable_Units = '(K)'

      TLADMtest%Sensor_Channel = SensorInfo%Sensor_Channel
      TLADMtest%Variable_Name  = VARIABLE_NAME



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
        !#               -- BEGIN TANGENT-LINEAR VARIABLE LOOP --                 #
        !#------------------------------------------------------------------------#

        TL_Variable_Loop: DO nV = 1, N_VARIABLES

          WRITE( *, '( 10x, "Performing TL calculations for ", a, "...." )' ) TRIM( VARIABLE_NAME( nV ) )



          !#----------------------------------------------------------------------#
          !#                       -- PERTURB THE VARIABLES --                    #
          !#----------------------------------------------------------------------#

          TL_Variable_Select: SELECT CASE ( nV )


            ! ------------------
            ! The level pressure
            ! ------------------

            CASE( NV_LEVEL_PRESSURE )

              ! -- Loop over the layers
              Level_Pressure_Loop: DO k = 1, AtmProfile%n_Layers

                ! -- Zero all TL variables
                Level_Pressure_TL       = ZERO
                Layer_Pressure_TL       = ZERO
                Layer_Temperature_TL    = ZERO
                Layer_Water_Vapor_TL    = ZERO
                Layer_Ozone_TL          = ZERO
                Surface_Temperature_TL  = ZERO
                Surface_Emissivity_TL   = ZERO
                Surface_Reflectivity_TL = ZERO
                Solar_Reflectivity_TL   = ZERO

                Tau_TL                    = ZERO
                Flux_Tau_TL               = ZERO
                Solar_Tau_TL              = ZERO
                Upwelling_Radiance_TL     = ZERO
                Brightness_Temperature_TL = ZERO

                ! -- Perturb the level pressure
                Level_Pressure_TL( k ) = ONE

                ! -- Run the TL model
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

                ! -- Save the data for this profile
                TLADMtest%dLayer_TL( k, :, NV_LEVEL_PRESSURE ) = Brightness_Temperature_TL(:)

              END DO Level_Pressure_Loop



            ! ------------------
            ! The layer pressure
            ! ------------------

            CASE( NV_LAYER_PRESSURE )

              ! -- Loop over the layers
              Layer_Pressure_Loop: DO k = 1, AtmProfile%n_Layers

                ! -- Zero all TL variables
                Level_Pressure_TL       = ZERO
                Layer_Pressure_TL       = ZERO
                Layer_Temperature_TL    = ZERO
                Layer_Water_Vapor_TL    = ZERO
                Layer_Ozone_TL          = ZERO
                Surface_Temperature_TL  = ZERO
                Surface_Emissivity_TL   = ZERO
                Surface_Reflectivity_TL = ZERO
                Solar_Reflectivity_TL   = ZERO

                Tau_TL                    = ZERO
                Flux_Tau_TL               = ZERO
                Solar_Tau_TL              = ZERO
                Upwelling_Radiance_TL     = ZERO
                Brightness_Temperature_TL = ZERO

                ! -- Perturb the layer pressure
                Layer_Pressure_TL( k ) = ONE

                ! -- Run the TL model
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

                ! -- Save the data for this profile
                TLADMtest%dLayer_TL( k, :, NV_LAYER_PRESSURE ) = Brightness_Temperature_TL(:)


              END DO Layer_Pressure_Loop



            ! ---------------------
            ! The layer temperature
            ! ---------------------

            CASE( NV_LAYER_TEMPERATURE )

              ! -- Loop over the layers
              Layer_Temperature_Loop: DO k = 1, AtmProfile%n_Layers

                ! -- Zero all TL variables
                Level_Pressure_TL       = ZERO
                Layer_Pressure_TL       = ZERO
                Layer_Temperature_TL    = ZERO
                Layer_Water_Vapor_TL    = ZERO
                Layer_Ozone_TL          = ZERO
                Surface_Temperature_TL  = ZERO
                Surface_Emissivity_TL   = ZERO
                Surface_Reflectivity_TL = ZERO
                Solar_Reflectivity_TL   = ZERO

                Tau_TL                    = ZERO
                Flux_Tau_TL               = ZERO
                Solar_Tau_TL              = ZERO
                Upwelling_Radiance_TL     = ZERO
                Brightness_Temperature_TL = ZERO

                ! -- Perturb the layer temperature
                Layer_Temperature_TL( k ) = ONE

                ! -- Run the TL model
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

                ! -- Save the data for this profile
                TLADMtest%dLayer_TL( k, :, NV_LAYER_TEMPERATURE ) = Brightness_Temperature_TL(:)

              END DO Layer_Temperature_Loop



            ! ---------------------
            ! The layer water vapor
            ! ---------------------

            CASE( NV_LAYER_WATER_VAPOR )

              ! -- Loop over the layers
              Layer_Water_Vapor_Loop: DO k = 1, AtmProfile%n_Layers

                ! -- Zero all TL variables
                Level_Pressure_TL       = ZERO
                Layer_Pressure_TL       = ZERO
                Layer_Temperature_TL    = ZERO
                Layer_Water_Vapor_TL    = ZERO
                Layer_Ozone_TL          = ZERO
                Surface_Temperature_TL  = ZERO
                Surface_Emissivity_TL   = ZERO
                Surface_Reflectivity_TL = ZERO
                Solar_Reflectivity_TL   = ZERO

                Tau_TL                    = ZERO
                Flux_Tau_TL               = ZERO
                Solar_Tau_TL              = ZERO
                Upwelling_Radiance_TL     = ZERO
                Brightness_Temperature_TL = ZERO

                ! -- Perturb the layer water vapor
                Layer_Water_Vapor_TL( k ) = ONE

                ! -- Run the TL model
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

                ! -- Save the data for this profile
                TLADMtest%dLayer_TL( k, :, NV_LAYER_WATER_VAPOR ) = Brightness_Temperature_TL(:)

              END DO Layer_Water_Vapor_Loop



            ! ---------------
            ! The layer ozone
            ! ---------------

            CASE( NV_LAYER_OZONE )

              ! -- Loop over the layers
              Layer_Ozone_Loop: DO k = 1, AtmProfile%n_Layers

                ! -- Zero all TL variables
                Level_Pressure_TL       = ZERO
                Layer_Pressure_TL       = ZERO
                Layer_Temperature_TL    = ZERO
                Layer_Water_Vapor_TL    = ZERO
                Layer_Ozone_TL          = ZERO
                Surface_Temperature_TL  = ZERO
                Surface_Emissivity_TL   = ZERO
                Surface_Reflectivity_TL = ZERO
                Solar_Reflectivity_TL   = ZERO

                Tau_TL                    = ZERO
                Flux_Tau_TL               = ZERO
                Solar_Tau_TL              = ZERO
                Upwelling_Radiance_TL     = ZERO
                Brightness_Temperature_TL = ZERO

                ! -- Perturb the layer ozone
                Layer_Ozone_TL( k ) = ONE

                ! -- Run the TL model
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

                ! -- Save the data for this profile
                TLADMtest%dLayer_TL( k, :, NV_LAYER_OZONE ) = Brightness_Temperature_TL(:)

              END DO Layer_Ozone_Loop



            ! -----------------------
            ! The surface temperature
            ! -----------------------

            CASE( NV_SURFACE_TEMPERATURE )

              ! -- Zero all TL variables
              Level_Pressure_TL       = ZERO
              Layer_Pressure_TL       = ZERO
              Layer_Temperature_TL    = ZERO
              Layer_Water_Vapor_TL    = ZERO
              Layer_Ozone_TL          = ZERO
              Surface_Temperature_TL  = ZERO
              Surface_Emissivity_TL   = ZERO
              Surface_Reflectivity_TL = ZERO
              Solar_Reflectivity_TL   = ZERO

              Tau_TL                    = ZERO
              Flux_Tau_TL               = ZERO
              Solar_Tau_TL              = ZERO
              Upwelling_Radiance_TL     = ZERO
              Brightness_Temperature_TL = ZERO

              ! -- Perturb the surface temperaturelayer ozone
              Surface_Temperature_TL = ONE

              ! -- Run the TL model
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

              ! -- Save the data for this profile
              TLADMtest%dSurface_TL( :, NV_SURFACE_TEMPERATURE - N_LAYER_VARIABLES ) = Brightness_Temperature_TL(:)


            ! ----------------------
            ! The surface emissivity
            ! ----------------------

            CASE( NV_SURFACE_EMISSIVITY )

              ! -- Zero all TL variables
              Level_Pressure_TL       = ZERO
              Layer_Pressure_TL       = ZERO
              Layer_Temperature_TL    = ZERO
              Layer_Water_Vapor_TL    = ZERO
              Layer_Ozone_TL          = ZERO
              Surface_Temperature_TL  = ZERO
              Surface_Emissivity_TL   = ZERO
              Surface_Reflectivity_TL = ZERO
              Solar_Reflectivity_TL   = ZERO

              Tau_TL                    = ZERO
              Flux_Tau_TL               = ZERO
              Solar_Tau_TL              = ZERO
              Upwelling_Radiance_TL     = ZERO
              Brightness_Temperature_TL = ZERO

              ! -- Perturb the surface emissivity (all channels)
              Surface_Emissivity_TL( : ) = ONE

              ! -- Run the TL model
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

              ! -- Save the data for this profile
              TLADMtest%dSurface_TL( :, NV_SURFACE_EMISSIVITY - N_LAYER_VARIABLES ) = Brightness_Temperature_TL(:)


            ! ------------------------
            ! The surface reflectivity
            ! ------------------------

            CASE( NV_SURFACE_REFLECTIVITY )

              ! -- Zero all TL variables
              Level_Pressure_TL       = ZERO
              Layer_Pressure_TL       = ZERO
              Layer_Temperature_TL    = ZERO
              Layer_Water_Vapor_TL    = ZERO
              Layer_Ozone_TL          = ZERO
              Surface_Temperature_TL  = ZERO
              Surface_Emissivity_TL   = ZERO
              Surface_Reflectivity_TL = ZERO
              Solar_Reflectivity_TL   = ZERO

              Tau_TL                    = ZERO
              Flux_Tau_TL               = ZERO
              Solar_Tau_TL              = ZERO
              Upwelling_Radiance_TL     = ZERO
              Brightness_Temperature_TL = ZERO

              ! -- Perturb the surface reflectivity (all channels)
              Surface_Reflectivity_TL( : ) = ONE

              ! -- Run the TL model
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

              ! -- Save the data for this profile
              TLADMtest%dSurface_TL( :, NV_SURFACE_REFLECTIVITY - N_LAYER_VARIABLES ) = Brightness_Temperature_TL(:)


            ! ----------------------
            ! The solar reflectivity
            ! ----------------------

            CASE( NV_SOLAR_REFLECTIVITY )

              ! -- Zero all TL variables
              Level_Pressure_TL       = ZERO
              Layer_Pressure_TL       = ZERO
              Layer_Temperature_TL    = ZERO
              Layer_Water_Vapor_TL    = ZERO
              Layer_Ozone_TL          = ZERO
              Surface_Temperature_TL  = ZERO
              Surface_Emissivity_TL   = ZERO
              Surface_Reflectivity_TL = ZERO
              Solar_Reflectivity_TL   = ZERO

              Tau_TL                    = ZERO
              Flux_Tau_TL               = ZERO
              Solar_Tau_TL              = ZERO
              Upwelling_Radiance_TL     = ZERO
              Brightness_Temperature_TL = ZERO

              ! -- Perturb the solar reflectivity (all channels)
              Solar_Reflectivity_TL( : ) = ONE

              ! -- Run the TL model
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

              ! -- Save the data for this profile
              TLADMtest%dSurface_TL( :, NV_SOLAR_REFLECTIVITY - N_LAYER_VARIABLES ) = Brightness_Temperature_TL(:)

          END SELECT TL_Variable_Select

        END DO TL_Variable_Loop



        !#------------------------------------------------------------------------#
        !#                   -- BEGIN ADJOINT VARIABLE LOOP --                    #
        !#------------------------------------------------------------------------#

        WRITE( *, '( 10x, "Performing AD calculations...." )' )


        ! --------------
        ! Begin the loop
        ! --------------

        AD_Variable_Loop: DO l = 1, n_Channels


          ! -------------------
          ! Zero out everything
          ! -------------------

          Level_Pressure_AD         = ZERO
          Layer_Pressure_AD         = ZERO
          Layer_Temperature_AD      = ZERO
          Layer_Water_Vapor_AD      = ZERO
          Layer_Ozone_AD            = ZERO
          Surface_Temperature_AD    = ZERO
          Surface_Emissivity_AD     = ZERO
          Surface_Reflectivity_AD   = ZERO
          Solar_Reflectivity_AD     = ZERO

          Tau_AD                    = ZERO
          Flux_Tau_AD               = ZERO
          Solar_Tau_AD              = ZERO
          Upwelling_Radiance_AD     = ZERO
          Brightness_Temperature_AD = ZERO


          ! ------------------------------------------
          ! Perturb the adjoint brightness temperature
          ! ------------------------------------------

          Brightness_Temperature_AD( l ) = ONE


          ! ----------------------
          ! Call the adjoint model
          ! ----------------------

          Error_Status = Compute_RTM_AD( Level_Pressure,                    &  ! Input, K
                                         Layer_Pressure,                    &  ! Input, K
                                         Layer_Temperature,                 &  ! Input, K
                                         Layer_Water_Vapor,                 &  ! Input, K
                                         Layer_Ozone,                       &  ! Input, K   
                                         Surface_Temperature,               &  ! Input, Scalar    
                                         Surface_Emissivity,                &  ! Input, L
                                         Surface_Reflectivity,              &  ! Input, L
                                         Tau_AD, Flux_Tau_AD, Solar_Tau_AD, &  ! Input, K x L
                                         Upwelling_Radiance_AD,             &  ! Input, L
                                         Brightness_Temperature_AD,         &  ! Input, L
                                         Secant_View_Angle,                 &  ! Input, Scalar
                                         Secant_Solar_Angle,                &  ! Input, Scalar
                                         n_Channels_Per_Profile,            &  ! Input, Scalar
                                         Channel_Index,                     &  ! Input, L
                                         Tau, Flux_Tau, Solar_Tau,          &  ! Output, K x L
                                         Upwelling_Radiance,                &  ! Output, L
                                         Brightness_Temperature,            &  ! Output, L
                                         Level_Pressure_AD,                 &  ! Output, K
                                         Layer_Pressure_AD,                 &  ! Output, K 
                                         Layer_Temperature_AD,              &  ! Output, K 
                                         Layer_Water_vapor_AD,              &  ! Output, K 
                                         Layer_Ozone_AD,                    &  ! Output, K   
                                         Surface_Temperature_AD,            &  ! Output, Scalar     
                                         Surface_Emissivity_AD,             &  ! Output, L
                                         Surface_Reflectivity_AD,           &  ! Output, L
                                         Solar_Reflectivity    = Solar_Reflectivity,   &  ! Optional input, L
                                         Secant_Flux_Angle     = Secant_Flux_Angle,    &  ! Optional input, Scalar
                                         Solar_Reflectivity_AD = Solar_Reflectivity_AD )  ! Optional output, L

          IF ( Error_Status /= SUCCESS ) THEN
             WRITE( Message, '( "Error in AD run at channel# ", i5 )' ) nV
             CALL Display_Message( PROGRAM_NAME, &
                                   TRIM( Message ), & 
                                    Error_Status )                           
           STOP
          END IF


          ! --------------------
          ! Fill TLADM structure
          ! --------------------

          TLADMtest%dLayer_AD( :, l, NV_LEVEL_PRESSURE )    = Level_Pressure_AD
          TLADMtest%dLayer_AD( :, l, NV_LAYER_PRESSURE )    = Layer_Pressure_AD
          TLADMtest%dLayer_AD( :, l, NV_LAYER_TEMPERATURE ) = Layer_Temperature_AD
          TLADMtest%dLayer_AD( :, l, NV_LAYER_WATER_VAPOR ) = Layer_Water_vapor_AD
          TLADMtest%dLayer_AD( :, l, NV_LAYER_OZONE )       = Layer_Ozone_AD

          TLADMtest%dSurface_AD( l, NV_SURFACE_TEMPERATURE  - N_LAYER_VARIABLES ) = Surface_Temperature_AD
          TLADMtest%dSurface_AD( l, NV_SURFACE_EMISSIVITY   - N_LAYER_VARIABLES ) = Surface_Emissivity_AD( l )
          TLADMtest%dSurface_AD( l, NV_SURFACE_REFLECTIVITY - N_LAYER_VARIABLES ) = Surface_Reflectivity_AD( l )
          TLADMtest%dSurface_AD( l, NV_SOLAR_REFLECTIVITY   - N_LAYER_VARIABLES ) = Solar_Reflectivity_AD( l )

        END DO AD_Variable_Loop



        !#------------------------------------------------------------------------#
        !#      -- WRITE THE CURRENT PROFILE TLADMtest STRUCTURE TO FILE --       #
        !#------------------------------------------------------------------------#

        WRITE( *, '( 10x, "Writing TL/AD data to output file...." )' )


        ! ------------------------------------------------------
        ! Set the current profile number in the output structure
        ! ------------------------------------------------------

        TLADMtest%Profile_Number = m


        ! --------------
        ! Write the data
        ! --------------

        Error_Status = Write_TLADMtest_netCDF( TRIM( TLADMtest_Filename ), &
                                               TLADMtest, &
                                               New_File = New_File, &
                                               Title         = 'TL/AD pCRTM test results for '//&
                                                               TRIM( SensorInfo%Satellite_Name )//' '//&
                                                               TRIM( SensorInfo%Sensor_Name ), &
                                               History       = PROGRAM_RCS_ID, &
                                               Sensor_Name   = TRIM( SensorInfo%Sensor_Name ), &
                                               Platform_Name = TRIM( SensorInfo%Satellite_Name ), &
                                               Comment       = TRIM( Comment ), &
                                               ID_Tag        = TRIM( ID_Tag ) )

        IF ( Error_Status /= SUCCESS ) THEN 
          WRITE( Message, '( "Error writing TLADMtest structure for profile #", i5, " to ", a )' ) &
                          m, TRIM( TLADMtest_Filename )
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
      !#                    -- DESTROY TLADMtest STRUCTURE --                   #
      !#------------------------------------------------------------------------#

      Error_Status = Destroy_TLADMtest( TLADMtest )

      IF ( Error_Status /= SUCCESS ) THEN 
         CALL Display_Message( PROGRAM_NAME, &
                               'Error occurred destroying TLADMtest for '//&
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

                  Surface_Emissivity_AD, &
                  Surface_Reflectivity_AD, &
                  Tau_AD, &
                  Flux_Tau_AD, &
                  Solar_Tau_AD, &
                  Upwelling_Radiance_AD, &
                  Brightness_Temperature_AD, &
                  Solar_Reflectivity_AD, &

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
              Level_Pressure_AD, &
              Layer_Pressure_AD, &
              Layer_Temperature_AD, &
              Layer_Water_Vapor_AD, &
              Layer_Ozone_AD, &
              STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
     CALL Display_Message( PROGRAM_NAME, &
                           'Error deallocating channel-independent arrays', & 
                            WARNING )  
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


END PROGRAM Adjoint_Model_Test


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Adjoint_Model_Test.f90,v 1.7 2006/05/02 14:58:35 dgroff Exp $
!
! $Date: 2006/05/02 14:58:35 $
!
! $Revision: 1.7 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Adjoint_Model_Test.f90,v $
! Revision 1.7  2006/05/02 14:58:35  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.6  2005/02/09 17:54:34  paulv
! - Now using updated test output structure with generic output variable
!   names.
!
! Revision 1.5  2004/12/28 19:54:57  paulv
! - Updated for category change.
!
! Revision 1.4  2004/12/28 17:08:27  paulv
! - Changes made due to AtmProfile structure update.
! - Some error output made a bit more informative.
!
! Revision 1.3  2004/09/30 14:44:28  paulv
! - Assigned Profile_Number value to the TLADMtest structure prior to Write()
! - Altered Write() function call interface to reflect changes in netCDF I/O
!   module.
!
! Revision 1.2  2004/09/29 20:04:15  paulv
! - First working version. Minimally tested.
!
! Revision 1.1  2004/09/29 14:04:26  paulv
! Initial checkin. Incomplete.
!
!
!
