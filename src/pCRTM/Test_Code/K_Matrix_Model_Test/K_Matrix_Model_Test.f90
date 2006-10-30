!------------------------------------------------------------------------------
!P+
! NAME:
!       K_Matrix_Model_Test
!
! PURPOSE:
!       Program to test the pCRTM K-Matrix component with respect
!       to the Adjoint component.
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
!       K_Matrix_Model:         Module containing the pCRTM K-matrix
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
!       ADKMtest_Define:        Module defining the structure to hold the CRTM
!                               K-matrix test results and containing routines
!                               to manipulate it.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!
!       ADKMtest_netCDF_IO:     Module containing routines to read and write
!                               netCDF format ADKMtest files.
!                               USEs: TYPE_KINDS
!                                     FILE_UTILITY
!                                     ERROR_HANDLER
!                                     ADKMTEST_DEFINE
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
!         - netCDF ADKMtest file
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 01-Oct-2004
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

PROGRAM K_Matrix_Model_Test


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
  USE Adjoint_Model
  USE K_Matrix_Model

  USE Spectral_Coefficients  ! For access to SC structure

  USE ADKMtest_Define
  USE ADKMtest_netCDF_IO


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME   = 'K_Matrix_Model_Test'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: K_Matrix_Model_Test.f90,v 1.4 2006/05/02 14:58:35 dgroff Exp $'
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
  CHARACTER( 256 ) :: ADKMtest_Filename

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

  TYPE( ADKMtest_type ) :: ADKMtest

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

  ! -- Adjoint outputs
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Level_Pressure_AD          ! K
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Layer_Pressure_AD          ! K
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Layer_Temperature_AD       ! K
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Layer_Water_Vapor_AD       ! K
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Layer_Ozone_AD             ! K
  REAL( fp_kind )                                 :: Surface_Temperature_AD     ! Scalar
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Surface_Emissivity_AD      ! L
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Surface_Reflectivity_AD    ! L

  ! -- K-matrix outputs
  REAL( fp_kind ), DIMENSION( :,: ),  ALLOCATABLE :: Level_Pressure_K           ! K x L
  REAL( fp_kind ), DIMENSION( :,: ),  ALLOCATABLE :: Layer_Pressure_K           ! K x L
  REAL( fp_kind ), DIMENSION( :,: ),  ALLOCATABLE :: Layer_Temperature_K        ! K x L
  REAL( fp_kind ), DIMENSION( :,: ),  ALLOCATABLE :: Layer_Water_Vapor_K        ! K x L
  REAL( fp_kind ), DIMENSION( :,: ),  ALLOCATABLE :: Layer_Ozone_K              ! K x L
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Surface_Temperature_K      ! L
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Surface_Emissivity_K       ! L
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Surface_Reflectivity_K     ! L

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

  ! -- Adjoint inputs
  REAL( fp_kind ), DIMENSION( :, : ), ALLOCATABLE :: Tau_AD                     ! K x L
  REAL( fp_kind ), DIMENSION( :, : ), ALLOCATABLE :: Flux_Tau_AD                ! K x L
  REAL( fp_kind ), DIMENSION( :, : ), ALLOCATABLE :: Solar_Tau_AD               ! K x L
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Upwelling_Radiance_AD      ! L  
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Brightness_Temperature_AD  ! L  

  ! -- K-Matrix inputs
  REAL( fp_kind ), DIMENSION( :, : ), ALLOCATABLE :: Tau_K                      ! K x L
  REAL( fp_kind ), DIMENSION( :, : ), ALLOCATABLE :: Flux_Tau_K                 ! K x L
  REAL( fp_kind ), DIMENSION( :, : ), ALLOCATABLE :: Solar_Tau_K                ! K x L
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Upwelling_Radiance_K       ! L  
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Brightness_Temperature_K   ! L  

  ! -- Optional forward inputs
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Solar_Reflectivity         ! L 
  REAL( fp_kind )                                 :: Secant_Flux_Angle          ! Scalar

  ! -- Optional adjoint outputs
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Solar_Reflectivity_AD      ! L 

  ! -- Optional K-matrix outputs
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Solar_Reflectivity_K       ! L 



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a)' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to test the pCRTM K-Matrix component with")' )
  WRITE( *, '( 5x, "   respect to the Adjoint component.")' )
  WRITE( *, '(/5x, " $Revision: 1.4 $")' )
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

      WRITE( *, '( //5x, "Testing K-matrix pCRTM for ", a, 1x, a )' ) &
                TRIM( SensorInfo%Satellite_Name ), &
                TRIM( SensorInfo%Sensor_Name )


      ! -------------------------------
      ! Set the output filename and new
      ! flag for the current sensor
      ! -------------------------------

      ADKMtest_Filename = TRIM( SensorInfo%File_Prefix )//'.ADKMtest.nc'
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

                Surface_Emissivity_AD( n_Channels ), &
                Surface_Reflectivity_AD( n_Channels ), &
                Tau_AD( AtmProfile%n_Layers, n_Channels ), &
                Flux_Tau_AD( AtmProfile%n_Layers, n_Channels ), &
                Solar_Tau_AD( AtmProfile%n_Layers, n_Channels ), &
                Upwelling_Radiance_AD( n_Channels ), &
                Brightness_Temperature_AD( n_Channels ), &
                Solar_Reflectivity_AD( n_Channels ), &

                Level_Pressure_K( AtmProfile%n_Layers, n_Channels ), &
                Layer_Pressure_K( AtmProfile%n_Layers, n_Channels ), &
                Layer_Temperature_K( AtmProfile%n_Layers, n_Channels ), &
                Layer_Water_Vapor_K( AtmProfile%n_Layers, n_Channels ), &
                Layer_Ozone_K( AtmProfile%n_Layers, n_Channels ), &

                Surface_Temperature_K( n_Channels ), &
                Surface_Emissivity_K( n_Channels ), &
                Surface_Reflectivity_K( n_Channels ), &
                Tau_K( AtmProfile%n_Layers, n_Channels ), &
                Flux_Tau_K( AtmProfile%n_Layers, n_Channels ), &
                Solar_Tau_K( AtmProfile%n_Layers, n_Channels ), &
                Upwelling_Radiance_K( n_Channels ), &
                Brightness_Temperature_K( n_Channels ), &
                Solar_Reflectivity_K( n_Channels ), &

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
      !#                   -- ALLOCATE ADKMtest STRUCTURE --                    #
      !#------------------------------------------------------------------------#

      Error_Status = Allocate_ADKMtest( AtmProfile%n_Layers, &
                                        n_Channels, &
                                        N_LAYER_VARIABLES, &
                                        N_SURFACE_VARIABLES, &
                                        ADKMtest )

      IF ( Error_Status /= SUCCESS ) THEN 
         CALL Display_Message( PROGRAM_NAME, &
                               'Error occurred allocating ADKMtest for '//&
                               TRIM( SensorInfo%Satellite_Name )//' '//&
                               TRIM( SensorInfo%Sensor_Name ), &
                                Error_Status )                           
       STOP
      END IF


      ! ------------------------
      ! Assign data to ADKMtest
      ! ------------------------

      ADKMtest%Output_Variable_Name  = 'Tb'
      ADKMtest%Output_Variable_Units = '(K)'

      ADKMtest%Sensor_Channel = SensorInfo%Sensor_Channel
      ADKMtest%Variable_Name  = VARIABLE_NAME



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
             WRITE( Message, '( "Error in AD run at channel index # ", i5 )' ) l
             CALL Display_Message( PROGRAM_NAME, &
                                   TRIM( Message ), & 
                                    Error_Status )                           
           STOP
          END IF


          ! -------------------
          ! Fill ADKM structure
          ! -------------------

          ADKMtest%dLayer_AD( :, l, NV_LEVEL_PRESSURE )    = Level_Pressure_AD
          ADKMtest%dLayer_AD( :, l, NV_LAYER_PRESSURE )    = Layer_Pressure_AD
          ADKMtest%dLayer_AD( :, l, NV_LAYER_TEMPERATURE ) = Layer_Temperature_AD
          ADKMtest%dLayer_AD( :, l, NV_LAYER_WATER_VAPOR ) = Layer_Water_vapor_AD
          ADKMtest%dLayer_AD( :, l, NV_LAYER_OZONE )       = Layer_Ozone_AD

          ADKMtest%dSurface_AD( l, NV_SURFACE_TEMPERATURE  - N_LAYER_VARIABLES ) = Surface_Temperature_AD
          ADKMtest%dSurface_AD( l, NV_SURFACE_EMISSIVITY   - N_LAYER_VARIABLES ) = Surface_Emissivity_AD( l )
          ADKMtest%dSurface_AD( l, NV_SURFACE_REFLECTIVITY - N_LAYER_VARIABLES ) = Surface_Reflectivity_AD( l )
          ADKMtest%dSurface_AD( l, NV_SOLAR_REFLECTIVITY   - N_LAYER_VARIABLES ) = Solar_Reflectivity_AD( l )

        END DO AD_Variable_Loop



        !#------------------------------------------------------------------------#
        !#                   -- BEGIN K-MATRIX CALCULATIONS --                    #
        !#------------------------------------------------------------------------#

        WRITE( *, '( 10x, "Performing K-Matrix calculations...." )' )


        ! -------------------
        ! Zero out everything
        ! -------------------

        Level_Pressure_K         = ZERO
        Layer_Pressure_K         = ZERO
        Layer_Temperature_K      = ZERO
        Layer_Water_Vapor_K      = ZERO
        Layer_Ozone_K            = ZERO
        Surface_Temperature_K    = ZERO
        Surface_Emissivity_K     = ZERO
        Surface_Reflectivity_K   = ZERO
        Solar_Reflectivity_K     = ZERO

        Tau_K                    = ZERO
        Flux_Tau_K               = ZERO
        Solar_Tau_K              = ZERO
        Upwelling_Radiance_K     = ZERO
        Brightness_Temperature_K = ZERO


        ! -------------------------------------------
        ! Perturb the K-matrix brightness temperature
        ! -------------------------------------------

        Brightness_Temperature_K(:) = ONE


        ! -----------------------
        ! Call the K-matrix model
        ! -----------------------

        Error_Status = Compute_RTM_K( Level_Pressure,                 &  ! Input, K
                                      Layer_Pressure,                 &  ! Input, K
                                      Layer_Temperature,              &  ! Input, K
                                      Layer_Water_Vapor,              &  ! Input, K
                                      Layer_Ozone,                    &  ! Input, K   
                                      Surface_Temperature,            &  ! Input, Scalar    
                                      Surface_Emissivity,             &  ! Input, L
                                      Surface_Reflectivity,           &  ! Input, L
                                      Tau_K, Flux_Tau_K, Solar_Tau_K, &  ! Input, K x L
                                      Upwelling_Radiance_K,           &  ! Input, L
                                      Brightness_Temperature_K,       &  ! Input, L
                                      Secant_View_Angle,              &  ! Input, Scalar
                                      Secant_Solar_Angle,             &  ! Input, Scalar
                                      n_Channels_Per_Profile,         &  ! Input, Scalar
                                      Channel_Index,                  &  ! Input, L
                                      Tau, Flux_Tau, Solar_Tau,       &  ! Output, K x L
                                      Upwelling_Radiance,             &  ! Output, L
                                      Brightness_Temperature,         &  ! Output, L
                                      Level_Pressure_K,               &  ! Output, K x L
                                      Layer_Pressure_K,               &  ! Output, K x L 
                                      Layer_Temperature_K,            &  ! Output, K x L 
                                      Layer_Water_vapor_K,            &  ! Output, K x L 
                                      Layer_Ozone_K,                  &  ! Output, K x L   
                                      Surface_Temperature_K,          &  ! Output, L
                                      Surface_Emissivity_K,           &  ! Output, L
                                      Surface_Reflectivity_K,         &  ! Output, L
                                      Solar_Reflectivity   = Solar_Reflectivity, &  ! Optional input, L
                                      Secant_Flux_Angle    = Secant_Flux_Angle,  &  ! Optional input, Scalar
                                      Solar_Reflectivity_K = Solar_Reflectivity_K )  ! Optional output, L

        IF ( Error_Status /= SUCCESS ) THEN
           CALL Display_Message( PROGRAM_NAME, &
                                 'Error in K-Matrix run.', & 
                                 Error_Status )                           
         STOP
        END IF


        ! --------------------
        ! Fill TLADM structure
        ! --------------------

        ADKMtest%dLayer_K( :, :, NV_LEVEL_PRESSURE )    = Level_Pressure_K
        ADKMtest%dLayer_K( :, :, NV_LAYER_PRESSURE )    = Layer_Pressure_K
        ADKMtest%dLayer_K( :, :, NV_LAYER_TEMPERATURE ) = Layer_Temperature_K
        ADKMtest%dLayer_K( :, :, NV_LAYER_WATER_VAPOR ) = Layer_Water_vapor_K
        ADKMtest%dLayer_K( :, :, NV_LAYER_OZONE )       = Layer_Ozone_K

        ADKMtest%dSurface_K( :, NV_SURFACE_TEMPERATURE  - N_LAYER_VARIABLES ) = Surface_Temperature_K
        ADKMtest%dSurface_K( :, NV_SURFACE_EMISSIVITY   - N_LAYER_VARIABLES ) = Surface_Emissivity_K
        ADKMtest%dSurface_K( :, NV_SURFACE_REFLECTIVITY - N_LAYER_VARIABLES ) = Surface_Reflectivity_K
        ADKMtest%dSurface_K( :, NV_SOLAR_REFLECTIVITY   - N_LAYER_VARIABLES ) = Solar_Reflectivity_K



        !#------------------------------------------------------------------------#
        !#       -- WRITE THE CURRENT PROFILE ADKMtest STRUCTURE TO FILE --       #
        !#------------------------------------------------------------------------#

        WRITE( *, '( 10x, "Writing AD/K data to output file...." )' )


        ! ------------------------------------------------------
        ! Set the current profile number in the output structure
        ! ------------------------------------------------------

        ADKMtest%Profile_Number = m


        ! --------------
        ! Write the data
        ! --------------

        Error_Status = Write_ADKMtest_netCDF( TRIM( ADKMtest_Filename ), &
                                              ADKMtest, &
                                              New_File = New_File, &
                                              Title         = 'AD/K pCRTM test results for '//&
                                                              TRIM( SensorInfo%Satellite_Name )//' '//&
                                                              TRIM( SensorInfo%Sensor_Name ), &
                                              History       = PROGRAM_RCS_ID, &
                                              Sensor_Name   = TRIM( SensorInfo%Sensor_Name ), &
                                              Platform_Name = TRIM( SensorInfo%Satellite_Name ), &
                                              Comment       = TRIM( Comment ), &
                                              ID_Tag        = TRIM( ID_Tag ) )

        IF ( Error_Status /= SUCCESS ) THEN 
          WRITE( Message, '( "Error writing ADKMtest structure for profile #", i5, " to ", a )' ) &
                          m, TRIM( ADKMtest_Filename )
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
      !#                    -- DESTROY ADKMtest STRUCTURE --                    #
      !#------------------------------------------------------------------------#

      Error_Status = Destroy_ADKMtest( ADKMtest )

      IF ( Error_Status /= SUCCESS ) THEN 
         CALL Display_Message( PROGRAM_NAME, &
                               'Error occurred destroying ADKMtest for '//&
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

                  Surface_Emissivity_AD, &
                  Surface_Reflectivity_AD, &
                  Tau_AD, &
                  Flux_Tau_AD, &
                  Solar_Tau_AD, &
                  Upwelling_Radiance_AD, &
                  Brightness_Temperature_AD, &
                  Solar_Reflectivity_AD, &

                  Level_Pressure_K, &
                  Layer_Pressure_K, &
                  Layer_Temperature_K, &
                  Layer_Water_Vapor_K, &
                  Layer_Ozone_K, &

                  Surface_Temperature_K, &
                  Surface_Emissivity_K, &
                  Surface_Reflectivity_K, &
                  Tau_K, &
                  Flux_Tau_K, &
                  Solar_Tau_K, &
                  Upwelling_Radiance_K, &
                  Brightness_Temperature_K, &
                  Solar_Reflectivity_K, &

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


END PROGRAM K_Matrix_Model_Test


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: K_Matrix_Model_Test.f90,v 1.4 2006/05/02 14:58:35 dgroff Exp $
!
! $Date: 2006/05/02 14:58:35 $
!
! $Revision: 1.4 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: K_Matrix_Model_Test.f90,v $
! Revision 1.4  2006/05/02 14:58:35  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.3  2005/02/09 17:54:34  paulv
! - Now using updated test output structure with generic output variable
!   names.
!
! Revision 1.2  2004/12/28 19:54:57  paulv
! - Updated for category change.
!
! Revision 1.1  2004/10/04 20:03:33  paulv
! Initial checkin.
!
!
!
