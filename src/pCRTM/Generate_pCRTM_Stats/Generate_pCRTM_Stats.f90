!------------------------------------------------------------------------------
!P+
! NAME:
!       Generate_pCRTM_Stats
!
! PURPOSE:
!       Program to generate profile set statistics for the prototype
!       Community Radiative Transfer Model (pCRTM)
!
! CATEGORY:
!       pCRTM
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:             Module containing definitions for kinds
!                               of variable types.
!
!       File_Utility:           Module containing generic file utility routines
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
!       TauProfile_Define:      Module defining the TauProfile data structure
!                               and containing routines to manipulate it.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!
!       TauProfile_netCDF_IO:   Module containing routines to read and write
!                               TauProfile netCDF format files.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!                                     TAUPROFILE_DEFINE module
!                                     NETCDF module
!                                     NETCDF_UTILITY module
!
!       pCRTMstats_Define:      Module defining the pCRTMstats data structure
!                               and containing routines to manipulate it.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!
!       pCRTMstats_netCDF_IO:   Module containing routines to read and write
!                               pCRTMstats netCDF format files.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!                                     RTMSTATS_DEFINE module
!                                     NETCDF module
!                                     NETCDF_UTILITY module
!
!       Initialize:             Module for pCRTM initialisation.
!                               USEs: ERROR_HANDLER module
!                                     SPECTRAL_COEFFICIENTS module
!                                     TRANSMITTANCE_COEFFICIENTS module
!
!       Parameters:             Module to hold pCRTM parameter constants.
!                               USEs: TYPE_KINDS module
!
!       Forward_Model:          Module containing the pCRTM forward model
!                               function.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!                                     PARAMETERS module
!                                     SPECTRAL_COEFFICIENTS module
!                                     ABSORBER_PROFILE module
!                                     PREDICTORS module
!                                     TRANSMITTANCE module
!                                     RADIANCE module
!
!       Spectral_Coefficients:  Module containing the pCRTM spectral
!                               coefficients.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!                                     PARAMETERS module
!                                     SPCCOEFF_DEFINE module
!                                     SPCCOEFF_BINARY_IO module
!
!       Radiance:              Module containing the pCRTM radiative
!                              transfer routines.
!                              USEs: TYPE_KINDS module
!                                    PARAMETERS module
!                                    SPECTRAL_COEFFICIENTS module
!                                    SENSOR_PLANCK_ROUTINES module
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
!       Input: - ASCII format SensorInfo file
!              - netCDF format AtmProfile file
!              - ASCII format list file of profiles to process
!              - Binary format SpcCoeff file } As part of
!              - Binary format TauCoeff file } the pCRTM
!
!       Output: - netCDF format pCRTMstats file.
!
! SIDE EFFECTS:
!       If the output file already exists, it is overwritten.
!
! RESTRICTIONS:
!       Processing ois performed only if all the mandatory input
!       files are present.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 20-Jun-2003
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2003, 2004 Paul van Delst
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

PROGRAM Generate_pCRTM_Stats


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE List_File_Utility

  USE AtmProfile_Define
  USE AtmProfile_netCDF_IO

  USE SensorInfo_Define
  USE SensorInfo_LinkedList
  USE SensorInfo_IO

  USE TauProfile_Define
  USE TauProfile_netCDF_IO

  USE pCRTMstats_Define
  USE pCRTMstats_netCDF_IO

  USE Initialize
  USE Parameters
  USE Forward_Model

  USE Spectral_Coefficients  ! For access to SC structure
  USE Radiance               ! For Compute_RTM


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ),  PARAMETER :: PROGRAM_NAME   = 'Generate_pCRTM_Stats'
  CHARACTER( * ),  PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: Generate_pCRTM_Stats.f90,v 2.4 2006/05/02 14:58:35 dgroff Exp $'
  CHARACTER( * ),  PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  ! -- Optional keyword set value
  INTEGER,         PARAMETER :: SET = 1

  ! -- The water vapor and ozone molecule IDs
  INTEGER, PARAMETER :: H2O_ID = 1
  INTEGER, PARAMETER ::  O3_ID = 3

  ! -- Total transmittance molecule set index
  INTEGER,         PARAMETER :: TAU_ALL_INDEX = 10

  ! -- Number of molecule sets to test
  INTEGER,         PARAMETER :: N_MOLECULE_SETS = 1

  ! -- ID tag for the profile set used to generate the coefficients
  CHARACTER( * ),  PARAMETER :: REG_PROFILE_ID_TAG = 'UMBC'

  ! -- Default emissivities and IR reflectivity "type"
  REAL( fp_kind ), PARAMETER :: MICROWAVE_EMISSIVITY = 0.6_fp_kind
  REAL( fp_kind ), PARAMETER :: INFRARED_EMISSIVITY  = 0.96_fp_kind
  INTEGER, PARAMETER :: SPECULAR  = 1
  INTEGER, PARAMETER :: ISOTROPIC = 2
  INTEGER, PARAMETER :: INFRARED_REFLECTIVITY_TYPE = SPECULAR

  ! -- Solar zenith angle and reflectivity
  REAL( fp_kind ), PARAMETER :: DEFAULT_SOLAR_ANGLE = 89.5_fp_kind
  REAL( fp_kind ), PARAMETER :: DEFAULT_SOLAR_REFLECTIVITY = 0.0_fp_kind


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message
  CHARACTER(  10 ) :: Message_Length

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  CHARACTER( 256 ) :: SensorInfo_Filename
  CHARACTER( 256 ) :: AtmProfile_Filename
  CHARACTER( 256 ) :: List_Filename
  CHARACTER( 256 ) :: TauProfile_Filename
  CHARACTER( 256 ) :: SpcCoeff_Filename
  CHARACTER( 256 ) :: TauCoeff_Filename
  CHARACTER( 256 ) :: pCRTMstats_Filename

  CHARACTER( 16 )   :: LBL_Profile_ID_Tag
  CHARACTER( 5000 ) :: History
  CHARACTER( 5000 ) :: Comment

  INTEGER :: Output_FileID

  INTEGER :: Error_Status
  INTEGER :: IO_Status
  INTEGER :: Allocate_Status

  INTEGER :: n_Layers
  INTEGER :: n_Channels
  INTEGER :: n_Angles
  INTEGER :: n_Profiles

  ! -- Loop counters
  INTEGER :: i, j, k, l, m, mIndex, lm, n

  ! -- SensorInfo linked list variables
  INTEGER                      :: n_Sensors
  TYPE( SensorInfo_type )      :: SensorInfo
  TYPE( SensorInfo_List_type ) :: SensorInfo_List

  ! -- AtmProfile and related variables
  TYPE( AtmProfile_type ) :: AtmProfile
  INTEGER, DIMENSION( 1 ) :: Idx
  INTEGER :: H2O_Idx
  INTEGER :: O3_Idx

  ! -- Profiles to process list variables
  INTEGER                              :: n_Profiles_to_Process
  TYPE( Integer_List_File_type )       :: Profile_List
  INTEGER, DIMENSION( : ), ALLOCATABLE :: Profiles_to_Process

  ! -- Transmittance profile variables
  INTEGER                                         :: n_TauProfile_Channels
  REAL( fp_kind ), DIMENSION( :, : ), ALLOCATABLE :: LBL_Tau
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Secant_View_Angle

  ! -- Forward model inputs
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Level_Pressure             ! K
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Layer_Pressure             ! K
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Layer_Temperature          ! K
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Layer_Water_Vapor          ! K
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Layer_Ozone                ! K
  REAL( fp_kind )                                 :: Surface_Temperature        ! Scalar
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Surface_Emissivity         ! L
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Surface_Reflectivity       ! L
  REAL( fp_kind )                                 :: Secant_Solar_Angle         ! Scalar
  INTEGER                                         :: n_Channels_Per_Profile     ! Scalar
  INTEGER,         DIMENSION( : ),    ALLOCATABLE :: Channel_Index              ! L

  ! -- Forward model outputs                                                                                
  REAL( fp_kind ), DIMENSION( :, : ), ALLOCATABLE :: Tau                        ! K x L
  REAL( fp_kind ), DIMENSION( :, : ), ALLOCATABLE :: Flux_Tau                   ! K x L
  REAL( fp_kind ), DIMENSION( :, : ), ALLOCATABLE :: Solar_Tau                  ! K x L
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Upwelling_Radiance         ! L  
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Brightness_Temperature     ! L  

  ! -- Compute radiance outputs
  REAL( fp_kind ), DIMENSION( : ),    ALLOCATABLE :: Layer_Radiance             ! L
  REAL( fp_kind )                                 :: Downwelling_Radiance       ! Scalar
  REAL( fp_kind )                                 :: LBL_BT                     ! Scalar

  ! -- Statistics result structure
  TYPE( pCRTMstats_type ) :: pCRTMstats


  ! -- Timing variables
  INTEGER         :: Hertz, Begin_Time, End_Time
  REAL( fp_kind ) :: Time_Taken



  !#----------------------------------------------------------------------------#
  !#                       -- GET THE TIMING COUNT RATE --                      #
  !#----------------------------------------------------------------------------#

  CALL SYSTEM_CLOCK( COUNT_RATE = Hertz )



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a)' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to generate profile set statistics for the pCRTM")' )
  WRITE( *, '(/5x, " $Revision: 2.4 $")' )
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


  ! ------------------------
  ! Create a SensorInfo list
  ! ------------------------

  SensorInfo_List = New_SensorInfo_List()


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
                                         Reverse = 1 )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading AtmProfile file '//&
                          TRIM( AtmProfile_fileNAME ), &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                -- READ THE PROFILES TO PROCESS LIST FILE --                #
  !#----------------------------------------------------------------------------#

  ! ------------------------------------
  ! Get the profiles-to-process listfile
  ! ------------------------------------

  WRITE( *, FMT     = '( /5x, "Enter a Profiles-to-Process list file [none == all]: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) List_Filename
  List_Filename = ADJUSTL( List_Filename )


  ! --------------------------
  ! Do we need to read a file?
  ! --------------------------

  IF ( TRIM( List_Filename ) == 'none' ) THEN


    ! --------------------
    ! Process ALL profiles
    ! --------------------

    ! -- Set the processing counter to the maximum
    n_Profiles_to_Process = AtmProfile%n_Profiles

    ! -- Allocate the processing array
    ALLOCATE( Profiles_to_Process( n_Profiles_to_Process ), STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '( "Error allocating Profiles_to_Process array for no list ", &
                        &"file. STAT = ", i5 )' ) Allocate_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF

    ! -- Fill the processing array
    Profiles_to_Process = (/ ( m, m = 1, n_Profiles_to_Process ) /)


  ELSE


    ! --------------------------
    ! Only process SOME profiles
    ! --------------------------

    ! -- Read the list file
    Error_Status = Read_List_File( TRIM( List_Filename ), &
                                   Profile_List )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error reading Profiles-to-Process list file '//&
                            TRIM( List_Filename ), &
                            Error_Status )
      STOP
    END IF


    ! -- Get the list size
    n_Profiles_to_Process = Get_List_Size( Profile_List )

    IF ( n_Profiles_to_Process < 1 ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Profile_to_Process list file contained no valid entries', &
                            FAILURE )
      STOP
    END IF


    ! -- Allocate the processing array
    ALLOCATE( Profiles_to_Process( n_Profiles_to_Process ), STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '( "Error allocating Profiles_to_Process array for list ", &
                        &"file ", a, ". STAT = ", i5 )' ) &
                      TRIM( List_Filename ), Allocate_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF


    ! -- Fill the processing array
    DO m = 1, n_Profiles_to_Process

      Error_Status = Get_List_Entry( Profile_List, &
                                     m, &
                                     Profiles_to_Process(m) )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error obtaining Profiles_to_Process list entry #", i5 )' ) m
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF

    END DO

  END IF


  ! -----------------------------------------------
  ! Output info message on profiles to be processed
  ! -----------------------------------------------

  WRITE( *, '( /5x, "The profiles numbers to be processed: " )' ) 
  WRITE( *, '( 10( 2x, i2 ) )' ) Profiles_to_Process



  !#----------------------------------------------------------------------------#
  !#            -- ALLOCATE THE CHANNEL/SENSOR INDEPENDENT ARRAYS --            #
  !#----------------------------------------------------------------------------#

  ALLOCATE( Level_Pressure( AtmProfile%n_Layers ), &     ! K
            Layer_Pressure( AtmProfile%n_Layers ), &     ! K
            Layer_Temperature( AtmProfile%n_Layers ), &  ! K
            Layer_Water_Vapor( AtmProfile%n_Layers ), &  ! K
            Layer_Ozone( AtmProfile%n_Layers ), &        ! K

            Layer_Radiance( AtmProfile%n_Layers ), &     ! K

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
  !#                -- ASSIGN THE PROFILE INDEPENDENT INPUT DATA --             #
  !#----------------------------------------------------------------------------#

  ! ----------------------
  ! The solar angle secant
  ! ----------------------

  Secant_Solar_Angle = ONE / COS( DEFAULT_SOLAR_ANGLE )



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

    ! -- The LBL transmittance data
    TauProfile_Filename = './TauProfile/upwelling.'//TRIM( SensorInfo%File_Prefix )//'.TauProfile.nc'

    ! -- The RTM coefficient files
    SpcCoeff_Filename = './SpcCoeff/'//TRIM( SensorInfo%File_Prefix )//'.SpcCoeff.bin'
    TauCoeff_Filename = './TauCoeff/'//TRIM( SensorInfo%File_Prefix )//'.TauCoeff.bin'


    ! -----------------------
    ! Test for file existance
    ! -----------------------

    Available_Sensors: IF ( File_Exists( TRIM( TauProfile_Filename ) ) .AND. &
                            File_Exists( TRIM( SpcCoeff_Filename   ) ) .AND. &
                            File_Exists( TRIM( TauCoeff_Filename   ) )       ) THEN

      WRITE( *, '( //5x, "Generating pCRTM profile set statistics for ", a, 1x, a )' ) &
                TRIM( SensorInfo%Satellite_Name ), &
                TRIM( SensorInfo%Sensor_Name )


      ! --------------------------
      ! Assign the output filename
      ! --------------------------

      pCRTMstats_Filename = TRIM( SensorInfo%File_Prefix )//'.pCRTMstats.nc'



      !#------------------------------------------------------------------------#
      !#            -- INITIALISE THE pCRTM FOR THE CURRENT SENSOR --           #
      !#------------------------------------------------------------------------#

      Error_Status = Initialize_RTM( Spectral_File = TRIM( SpcCoeff_Filename ), &
                                     Tau_File      = TRIM( TauCoeff_Filename )  )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error initialising the pCRTM for '//&
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
          Surface_Emissivity( l )   = MICROWAVE_EMISSIVITY
          Surface_Reflectivity( l ) = ONE - Surface_Emissivity( l )
        ELSE
          ! -- Infrared
          Surface_Emissivity( l )   = INFRARED_EMISSIVITY
          IF ( INFRARED_REFLECTIVITY_TYPE == SPECULAR ) THEN
            Surface_Reflectivity( l ) = ONE - Surface_Emissivity( l )
          ELSE
            Surface_Reflectivity( l ) = ( ONE - Surface_Emissivity( l ) ) / PI
          END IF
        END IF

      END DO


      ! ------------------------------------
      ! All sensor channels for each profile
      ! ------------------------------------

      n_Channels_Per_Profile = n_Channels



      !#------------------------------------------------------------------------#
      !#     -- ALLOCATE A LBL TRANSMITTANCE ARRAY FOR THE CURRENT SENSOR --    #
      !#------------------------------------------------------------------------#

      ! ----------------------------------------------
      ! Inquire the TauProfile file for its dimensions
      ! ----------------------------------------------

      Error_Status = Inquire_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                                n_Layers   = n_Layers, &
                                                n_Channels = n_TauProfile_Channels, &
                                                n_Angles   = n_Angles, &
                                                n_Profiles = n_Profiles, &
                                                ID_Tag  = LBL_Profile_ID_Tag, &
                                                History = History, &
                                                Comment = Comment )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error inquiring TauProfile file '//TRIM( TauProfile_Filename )//&
                              ' for dimensions.', &
                              Error_Status )
        STOP
      END IF


      ! -----------------------------------
      ! Check the dimensions are consistent
      ! -----------------------------------

      IF ( n_Layers   /= AtmProfile%n_Layers   .OR. &
           n_Channels /= n_TauProfile_Channels .OR. &
           n_Profiles /= AtmProfile%n_Profiles      ) THEN
        Error_Status = FAILURE
        CALL Display_Message( PROGRAM_NAME, &
                              'Inconsistent AtmProfile/TauProfile dimensions', &
                              Error_Status )
        STOP
      END IF


      ! ----------------------------------------------------------
      ! Allocate the TauProfile transmittance array and angle list
      ! ----------------------------------------------------------

      ALLOCATE( LBL_Tau( n_Layers, n_Channels ), &
                Secant_View_Angle( n_Angles ), &
                STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error allocating LBL_Tau and angle list arrays for ", a, 1x, a, &
                          &" run. STAT = ", i5 )' ) &
                        TRIM( SensorInfo%Satellite_Name ), &
                        TRIM( SensorInfo%Sensor_Name ), &
                        Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF

      
      ! -------------------
      ! Read the angle list
      ! -------------------

      Error_Status = Inquire_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                                Angle_List = Secant_View_Angle )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error inquiring TauProfile file '//TRIM( TauProfile_Filename )//&
                              ' for angle list data.', &
                              Error_Status )
        STOP
      END IF



      !#------------------------------------------------------------------------#
      !#              -- ALLOCATE THE OUTPUT pCRTMstats STRUCTURE --            #
      !#------------------------------------------------------------------------#

      Error_Status = Allocate_pCRTMstats( n_Channels, &
                                          n_Angles - 1, &  ! Do not include the maximum angle
                                          n_Profiles_to_Process, &
                                          N_MOLECULE_SETS, &
                                          pCRTMstats )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error allocating pCRTMstats structure for '//&
                              TRIM( SensorInfo%Satellite_Name )//' '//&
                              TRIM( SensorInfo%Sensor_Name ), &
                              Error_Status )
        STOP
      END IF


      ! --------------------------------
      ! Fill some of the pCRTMstats fields
      ! --------------------------------

      pCRTMstats%LBL_Profile_ID_Tag = TRIM( LBL_Profile_ID_Tag )
      pCRTMstats%REG_Profile_ID_Tag = REG_PROFILE_ID_TAG

      pCRTMstats%NCEP_Sensor_ID   = SensorInfo%NCEP_Sensor_ID
      pCRTMstats%WMO_Satellite_ID = SensorInfo%WMO_Satellite_ID
      pCRTMstats%WMO_Sensor_ID    = SensorInfo%WMO_Sensor_ID

      pCRTMstats%Sensor_Channel = SensorInfo%Sensor_Channel
      pCRTMstats%Frequency      = SC%Wavenumber
      pCRTMstats%Angle          = Secant_View_Angle(1:n_Angles - 1)
      pCRTMstats%Profile        = Profiles_to_Process
      pCRTMstats%Molecule_Set   = (/ TAU_ALL_INDEX /)

      

      !#------------------------------------------------------------------------#
      !#                     -- BEGIN LOOP OVER PROFILES --                     #
      !#------------------------------------------------------------------------#

      m_Profile_Loop: DO mIndex = 1, n_Profiles_to_Process



        !#----------------------------------------------------------------------#
        !#                   -- ASSIGN THE TRUE PROFILE NUMBER --               #
        !#----------------------------------------------------------------------#

        m = Profiles_to_Process( mIndex )

        ! -- Output an info message
        WRITE( *, '( 7x, "Processing profile ", i2, " for ", a, 1x, a, "..." )' ) &
                  m, &
                  TRIM( SensorInfo%Satellite_Name ), &
                  TRIM( SensorInfo%Sensor_Name )



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



        !#----------------------------------------------------------------------#
        !#                   -- BEGIN LOOP OVER VIEW ANGLES --                  #
        !#----------------------------------------------------------------------#

        i_View_Angle_Loop: DO i = 1, n_Angles - 1  ! Do not include the maximum angle.


          ! -----------------------------------------------
          ! Compute the transmittances/radiances/brightness
          ! temperatures using the pCRTM
          ! -----------------------------------------------

          Error_Status = Compute_RTM( Level_Pressure,         &  ! Input,  K
                                      Layer_Pressure,         &  ! Input,  K
                                      Layer_Temperature,      &  ! Input,  K
                                      Layer_Water_Vapor,      &  ! Input,  K
                                      Layer_Ozone,            &  ! Input,  K
                                      Surface_Temperature,    &  ! Input,  Scalar
                                      Surface_Emissivity,     &  ! Input,  L
                                      Surface_Reflectivity,   &  ! Input,  L
                                      Secant_View_Angle(i),   &  ! Input,  Scalar
                                      Secant_Solar_Angle,     &  ! Input,  Scalar
                                      n_Channels_Per_Profile, &  ! Input,  Scalar
                                      Channel_Index,          &  ! Input,  L
                                      Tau,                    &  ! Output, K x L
                                      Flux_Tau,               &  ! Output, K x L
                                      Solar_Tau,              &  ! Output, K x L
                                      Upwelling_Radiance,     &  ! Output, L
                                      Brightness_Temperature  )  ! Output, L

          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error in Compute_RTM call for '//&
                                  TRIM( SensorInfo%Satellite_Name )//' '//&
                                  TRIM( SensorInfo%Sensor_Name ), &
                                  Error_Status )
            STOP
          END IF


          ! -------------------------------
          ! Save the TOA regression results
          ! -------------------------------

          pCRTMstats%REG_Tau(:,i,mIndex,1) = Tau(n_Layers,:)
          pCRTMstats%REG_BT( :,i,mIndex,1) = Brightness_Temperature


          ! ---------------------------------------
          ! Read the TauProfile total transmittance
          ! data for the current angle and profile
          ! ---------------------------------------

          Error_Status = Read_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                                 Secant_View_Angle( i ), &
                                                 m, &
                                                 TAU_ALL_INDEX, &
                                                 LBL_Tau )

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error reading angle ", f4.2, ", profile ", i2, &
                              &" total transmittance profiles from TauProfile file ", a )' ) &
                            Secant_View_Angle( i ), &
                            m, &
                            TRIM( TauProfile_Filename )
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM( Message ), &
                                  Error_Status )
            STOP
          END IF


          ! -----------------------------------
          ! Compute the brightness temperatures
          ! using the LBL transmittances
          ! -----------------------------------

          l_Channel_Loop: DO l = 1, n_Channels

            CALL Compute_Radiance( Layer_Temperature,          &
                                   Surface_Temperature,        &
                                   Surface_Emissivity( l ),    &
                                   Surface_Reflectivity( l ),  &
                                   DEFAULT_SOLAR_REFLECTIVITY, &
                                   LBL_Tau( :, l ),            &
                                   Flux_Tau( :, l ),           &
                                   Solar_Tau( 1, l ),          &
                                   Secant_Solar_Angle,         &
                                   0,                          &  ! Valid_Solar
                                   Channel_Index( l ),         &
                                   Layer_Radiance,             &
                                   Downwelling_Radiance,       &
                                   Upwelling_Radiance( l ),    &
                                   LBL_BT                      )


            ! -----------------------------
            ! Save the line-by-line results
            ! -----------------------------

            pCRTMstats%LBL_Tau(l,i,mIndex,1) = LBL_Tau(n_Layers,l)
            pCRTMstats%LBL_BT( l,i,mIndex,1) = LBL_BT

          END DO l_Channel_Loop

        END DO i_View_Angle_Loop

      END DO m_Profile_Loop



      !#------------------------------------------------------------------------#
      !#              -- COMPUTE AND OUTPUT THE pCRTM STATISTICS --             #
      !#------------------------------------------------------------------------#

      ! ----------------------
      ! Compute the statistics
      ! ----------------------

      Error_Status = Compute_pCRTMstats( pCRTMstats )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error computing pCRTMstats for '//&
                              TRIM( SensorInfo%Satellite_Name )//' '//&
                              TRIM( SensorInfo%Sensor_Name ), &
                              Error_Status )
        STOP
      END IF


      ! --------------------
      ! Output the structure
      ! --------------------

      Error_Status = Write_pCRTMstats_netCDF( TRIM( pCRTMstats_Filename ), &
                                              pCRTMstats, &
                                              Title = 'pCRTM statistics for LBL and REG comparison.', &
                                              History = PROGRAM_RCS_ID//'; '//TRIM( History ), &
                                              Sensor_Name = TRIM( SensorInfo%Sensor_Name ), &
                                              Platform_Name = TRIM( SensorInfo%Satellite_Name ), &
                                              Comment = 'Stats are for LBL - REG; '//TRIM( Comment ) )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error writing pCRTMstats to '//&
                              TRIM( pCRTMstats_Filename ), &
                              Error_Status )
        STOP
      END IF


      ! ------------------------
      ! Deallocate the structure
      ! ------------------------

      Error_Status = Destroy_pCRTMstats( pCRTMstats )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error destroying pCRTMstats structure for '//&
                              TRIM( SensorInfo%Satellite_Name )//' '//&
                              TRIM( SensorInfo%Sensor_Name ), &
                              Error_Status )
        STOP
      END IF



      !#------------------------------------------------------------------------#
      !#               -- DEALLOCATE THE TauProfile DATA ARRAYS --              #
      !#------------------------------------------------------------------------#

      DEALLOCATE( LBL_Tau, Secant_View_Angle, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauProfile data arrays for ", a, 1x, a, &
                          &" run. STAT = ", i5 )' ) &
                        TRIM( SensorInfo%Satellite_Name ), &
                        TRIM( SensorInfo%Sensor_Name ), &
                        Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF



      !#------------------------------------------------------------------------#
      !#  -- DEALLOCATE THE CHANNEL-DEPENDENT ARRAYS FOR THE CURRENT SENSOR --  #
      !#------------------------------------------------------------------------#

      DEALLOCATE( Surface_Emissivity, &                    
                  Surface_Reflectivity, &
                  Channel_Index, &
                  Tau, Flux_Tau, Solar_Tau, &
                  Upwelling_Radiance, &
                  Brightness_Temperature, &
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

      Error_Status = Destroy_RTM()

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error destroying RTM for '//&
                              TRIM( SensorInfo%Satellite_Name )//' '//&
                              TRIM( SensorInfo%Sensor_Name ), &
                              Error_Status )
        STOP
      END IF

    END IF Available_Sensors



    !#--------------------------------------------------------------------------#
    !#              -- DESTROY THE CURRENT SensorInfo STRUCTURE --              #
    !#--------------------------------------------------------------------------#

    Error_Status = Destroy_SensorInfo( SensorInfo )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error destroying SensorInfo data structure for sensor #", i3 )' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_status )
      STOP
    END IF


  END DO n_Sensor_loop



  !#----------------------------------------------------------------------------#
  !#           -- DEALLOCATE THE CHANNEL/SENSOR INDEPENDENT ARRAYS --           #
  !#----------------------------------------------------------------------------#

  DEALLOCATE( Level_Pressure, &     ! K
              Layer_Pressure, &     ! K
              Layer_Temperature, &  ! K
              Layer_Water_Vapor, &  ! K
              Layer_Ozone, &        ! K
              Layer_Radiance, &     ! K
              STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    Error_Status = WARNING
    WRITE( Message, '( "Error deallocating channel-independent arrays. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          Error_Status )
  END IF  



  !#----------------------------------------------------------------------------#
  !#               -- DEALLOCATE THE PROFILES-TO-PROCESS ARRAY --               #
  !#----------------------------------------------------------------------------#

  DEALLOCATE( Profiles_to_Process, STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error deallocating Profiles-to-Process array.', &
                          WARNING )
  END IF



  !#----------------------------------------------------------------------------#
  !#                    -- DESTROY THE AtmProfile STRUCTURE --                  #
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



END PROGRAM Generate_pCRTM_Stats


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Generate_pCRTM_Stats.f90,v 2.4 2006/05/02 14:58:35 dgroff Exp $
!
! $Date: 2006/05/02 14:58:35 $
!
! $Revision: 2.4 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Generate_pCRTM_Stats.f90,v $
! Revision 2.4  2006/05/02 14:58:35  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 2.3  2005/02/07 17:40:35  paulv
! - Updated onscreen output from RTM to pCRTM.
!
! Revision 2.2  2005/01/07 18:43:40  paulv
! - Catagory change to pCRTM.
!
! Revision 2.1  2005/01/06 18:48:49  paulv
! - Modified filename to include "upwelling" prefix.
!
! Revision 2.0  2004/10/06 15:57:31  paulv
! - Removed RTMarg use. Replaced with individual allocated arrays for RTM inputs.
! - Now using new Fortran-95 utility modules.
!
! Revision 1.6  2004/03/29 18:05:17  paulv
! - Added default solar reflectivity.
! - Corrected bug in call to forward model due to solar transmittance argument.
!
! Revision 1.5  2004/02/13 17:15:21  paulv
! - Added RTMstats initialisation call(!)
! - Added TauProfile history and comment attributes to the output RTMstats file.
! - Corrected bugs in passing arguments to the Compute_Radiance subroutine.
! - No longer include the maximum angle in the computations.
!
! Revision 1.4  2004/02/12 17:28:35  paulv
! - Stats no longer include the largest angle in the calcs.
! - Corrected a bug in saving the total transmittance.
!
! Revision 1.3  2004/01/28 21:41:32  paulv
! - Using the RTMstats modules to collect and output the statistics.
!
! Revision 1.2  2003/12/05 22:38:02  paulv
! - Continuing updates. Not completed.
!
! Revision 1.1  2003/06/20 22:00:00  paulv
! Initial checkin. Incomplete.
!
! Revision 2.1  2003/05/16 18:46:57  paulv
! - New version.
!
!
!
