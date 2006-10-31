!--------------------------------------------------------------------------------
!M+
! NAME:
!       Compute_MW_Transmittance
!
! PURPOSE:
!       Program to compute channel transmittances for microwave instruments.
!
! CATEGORY:
!       LBL : Microwave
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:                  Module containing definitions for kinds
!                                    of variable types.
!
!       Message_Handler:               Module to define simple error codes and
!                                    handle error conditions
!                                    USEs: FILE_UTILITY module
!
!       Profile_Utility_Parameters:  Module containing parameters used in the
!                                    profile utility modules.
!                                    USEs: TYPE_KINDS module
!                                          FUNDAMENTAL_CONSTANTS module
!
!       Units_Conversion:            Module containing routines to convert
!                                    atmospheric profile concentration units.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!                                          PROFILE_UTILITY_PARAMETERS module
!                                          ATMOSPHERIC_PROPERTIES module
!
!       SensorInfo_Define:           Module defining the SensorInfo data structure
!                                    and containing routines to manipulate it.
!                                    USEs: TYPE_KINDS module
!                                          FILE_UTILITY module
!                                          ERROR_HANDLER module
!
!       SensorInfo_LinkedList:       Module defining the SensorInfo Linked List
!                                    data structure and containing routines to
!                                    manipulate it.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!                                          SENSORINFO_DEFINE module
!
!       SensorInfo_IO:               Module continaing routines to read and write
!                                    ASCII SensorInfo format files.
!                                    USEs: TYPE_KINDS module
!                                          FILE_UTILITY module
!                                          ERROR_HANDLER module
!                                          SENSORINFO_DEFINE module
!
!       AtmProfile_Define:           Module defining the AtmProfile data
!                                    structure and containing routines to
!                                    manipulate it.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!
!       AtmProfile_netCDF_IO:        Module containing routines to read and
!                                    write AtmProfile netCDF format files.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!                                          ATMPROFILE_DEFINE module
!                                          NETCDF module
!                                          NETCDF_UTILITY module
!
!       TauProfile_Define:           Module defining the TauProfile data
!                                    structure and containing routines to
!                                    manipulate it.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!
!       TauProfile_netCDF_IO:        Module containing routines to read and
!                                    write TauProfile netCDF format files.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!                                          TAUPROFILE_DEFINE module
!                                          NETCDF module
!                                          NETCDF_UTILITY module
!
!       MW_SensorData_Define:        Module defining the MW_SensorData data
!                                    structure and containing routines to
!                                    manipulate it.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!
!       MWLBL_Transmittance:         Module containing routines for microwave
!                                    transmittance profile calculations.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!                                          MWLBL_LIEBE89 module
!                                          MWLBL_LIEBE93 module
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
!       Input:  - ASCII format SensorInfo file
!               - netCDF format AtmProfile file
!
!       Output: - netCDF format TauProfile file.
!
! SIDE EFFECTS:
!       If the output file already exists, it is overwritten.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 20-Sep-2002
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2002 Paul van Delst
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
!M-
!--------------------------------------------------------------------------------

PROGRAM Compute_MW_Transmittance



  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler

  USE Profile_Utility_Parameters, ONLY : ID_H2O, &
                                         PPMV_UNITS, &
                                         ND_UNITS, &
                                         MR_UNITS, &
                                         MD_UNITS, &
                                         PP_UNITS
  USE Units_Conversion

  USE SensorInfo_Define
  USE SensorInfo_LinkedList
  USE SensorInfo_IO

  USE AtmProfile_Define
  USE AtmProfile_netCDF_IO

  USE TauProfile_Define
  USE TauProfile_netCDF_IO

  USE MW_SensorData_Define

  USE MWLBL_Transmittance


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Compute_MW_Transmittance'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: Compute_MW_Transmittance.f90,v 2.3 2006/06/30 16:47:16 dgroff Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  ! -- Keyword argument set value
  INTEGER, PARAMETER :: SET = 1

  ! -- Direction names
  INTEGER, PARAMETER :: N_DIRECTIONS = 2
  CHARACTER( * ), PARAMETER, DIMENSION( N_DIRECTIONS ) :: DIRECTION_NAME = (/ 'upwelling  ', &
                                                                              'downwelling' /)

  ! -- Model names
  INTEGER, PARAMETER :: N_MODELS = 2
  INTEGER, PARAMETER :: MODEL_LIEBE      = 1
  INTEGER, PARAMETER :: MODEL_ROSENKRANZ = 2
  INTEGER,        PARAMETER, DIMENSION( N_MODELS ) :: MODEL_INDEX = (/ MODEL_LIEBE, &
                                                                       MODEL_ROSENKRANZ /)
  CHARACTER( * ), PARAMETER, DIMENSION( N_MODELS ) :: MODEL_NAME = (/ 'Liebe89/93  ', &
                                                                      'Rosenkranz03' /)

  ! -- Define the secant of the zenith angles to be used
  INTEGER, PARAMETER :: N_ANGLES = 7
  REAL( fp_kind ), PARAMETER, DIMENSION( N_ANGLES ) :: ANGLE_SECANT = &
    (/ 1.00_fp_kind, 1.25_fp_kind, 1.50_fp_kind, &
       1.75_fp_kind, 2.00_fp_kind, 2.25_fp_kind, &
       3.00_fp_kind /)

  ! -- Total number of points per channel. Must be
  ! -- evenly divisible by 2 and 4.
  INTEGER, PARAMETER :: N_FREQUENCIES = 256
  INTEGER, PARAMETER :: N_HALFPOINTS  = N_FREQUENCIES/2

  ! -- The molecular set IDs.
  ! --   1 == WLO; Wet lines only
  ! --  10 == ALL; All absorbers with continua
  ! --  12 == WET; Wet (and the wet continua)
  ! --  13 == DRY; Dry (and the dry continua)
  ! --  15 == WCO; Wet continua only
  ! -- 101 == EffWLO; Effective wet lines (wet/wco)
  ! -- 113 == EffDRY; Effective dry (all/wet)
  INTEGER, PARAMETER :: N_MOLECULE_SETS = 7
  INTEGER, PARAMETER, DIMENSION( N_MOLECULE_SETS ) :: MOLECULE_SET_LIST = &
    (/ 1, 10, 12, 13, 15, 101, 113 /)
  INTEGER, PARAMETER :: WLO_IDX = 1
  INTEGER, PARAMETER :: ALL_IDX = 2
  INTEGER, PARAMETER :: WET_IDX = 3
  INTEGER, PARAMETER :: DRY_IDX = 4
  INTEGER, PARAMETER :: WCO_IDX = 5
  INTEGER, PARAMETER :: EFFECTIVE_WLO_IDX = 6
  INTEGER, PARAMETER :: EFFECTIVE_DRY_IDX = 7

  ! -- Numeric literal
  REAL( fp_kind ), PARAMETER :: ZERO = 0.0_fp_kind
  REAL( fp_kind ), PARAMETER :: ONE  = 1.0_fp_kind
  REAL( fp_kind ), PARAMETER :: TOLERANCE = EPSILON( ONE )




  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: IO_Status

  INTEGER :: Direction
  INTEGER :: Downwelling

  INTEGER :: Model
  INTEGER :: Rosenkranz

  CHARACTER( 256 )             :: SensorInfo_Filename
  TYPE( SensorInfo_type )      :: SensorInfo
  TYPE( SensorInfo_List_type ) :: SensorInfo_List

  CHARACTER( 256 )        :: AtmProfile_Filename
  TYPE( AtmProfile_type ) :: AtmProfile

  CHARACTER( 256 )        :: TauProfile_Filename
  TYPE( TauProfile_type ) :: TauProfile
  CHARACTER( 256 )        :: Comment

  TYPE( MW_SensorData_type ) :: MW_SensorData

  INTEGER :: j ! n_Absorbers
  INTEGER :: k ! n_Layers, n_Levels
  INTEGER :: l ! n_Frequencies
  INTEGER :: m ! n_Profiles
  INTEGER :: n,  n_Sensors

  INTEGER, DIMENSION( 2 ) :: n_Points
  INTEGER :: n_OffsetPoints
  INTEGER :: ln
  INTEGER :: i, i_Upper, i_Lower

  CHARACTER( 256 ) :: Profile_Set_ID_Tag

  INTEGER, DIMENSION( 1 ) :: j_idx
  INTEGER                 :: Index_H2O

  REAL( fp_kind ), DIMENSION( : ), ALLOCATABLE :: H2O_Pressure

  CHARACTER( 512 ) :: MW_SensorData_RCS_Id
  CHARACTER( 512 ) :: MWLBL_Transmittance_RCS_Id

  REAL( fp_kind ) :: df, f1, f
  REAL( fp_kind ) :: Delta_Frequency

  REAL( fp_kind ), DIMENSION( : ),       ALLOCATABLE         :: Frequency  ! L
  REAL( fp_kind ), DIMENSION( :, :, : ), ALLOCATABLE, TARGET :: TauALL     ! L x K x I
  REAL( fp_kind ), DIMENSION( :, :, : ), ALLOCATABLE, TARGET :: TauWLO     ! L x K x I
  REAL( fp_kind ), DIMENSION( :, :, : ), ALLOCATABLE, TARGET :: TauWCO     ! L x K x I
  REAL( fp_kind ), DIMENSION( :, :, : ), ALLOCATABLE, TARGET :: TauWET     ! L x K x I
  REAL( fp_kind ), DIMENSION( :, :, : ), ALLOCATABLE, TARGET :: TauDRY     ! L x K x I

  REAL( fp_kind ), DIMENSION( :, :, : ), POINTER :: Tau

  REAL( fp_kind ) :: SumTau, SumError, SumTemp, SumY



  !#----------------------------------------------------------------------------#
  !#                -- INITIALISE DATA STRUCTURES AND POINTERS --               #
  !#----------------------------------------------------------------------------#

  ! --------------------------
  ! New SensorInfo linked list
  ! --------------------------

  SensorInfo_List = New_SensorInfo_List()


  ! ---------------------------
  ! Transmittance array pointer
  ! ---------------------------

  NULLIFY( Tau )



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to compute transmittance profiles for user-defined ")' )
  WRITE( *, '( 5x, "   microwave instruments.")' )
  WRITE( *, '(/5x, " $Revision: 2.3 $")' )
  WRITE( *, '( 5x, a, / )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                        -- DETERMINE THE DIRECTION --                       #
  !#----------------------------------------------------------------------------#

  ! --------------
  ! Get user input
  ! --------------

  ! -- Output choices
  WRITE( *, FMT     = '( /5x, "Select atmospheric path" )' )
  DO i = 1, N_DIRECTIONS
    WRITE( *, FMT = '( 10x, i1, ") ", a )' ) i, DIRECTION_NAME( i )
  END DO
  WRITE( *, FMT = '( 5x, "Enter choice: " )', &
            ADVANCE = 'NO' )

  ! -- Read direction input
  READ( *, FMT    = '( i5 )', &
           IOSTAT = IO_Status ) Direction

  ! -- Check for errors or invalid values
  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid ATMOSPHERIC PATH identifier input.', &
                          FAILURE )
    STOP
  END IF

  IF ( Direction < 1 .OR. Direction > N_DIRECTIONS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid ATMOSPERIC PATH identifier value.', &
                          FAILURE )
    STOP
  ENDIF


  ! ---------------------------------------------------
  ! Assign optional argument for transmittance function
  ! ---------------------------------------------------

  ! -- Upwelling by default....
  Downwelling = 0

  ! -- ....unless downwelling is selected
  IF ( TRIM( DIRECTION_NAME( Direction ) ) == 'downwelling' ) THEN
    Downwelling = 1
  END IF



  !#----------------------------------------------------------------------------#
  !#                     -- SELECT THE ABSORPTION MODEL --                      #
  !#----------------------------------------------------------------------------#

  ! --------------
  ! Get user input
  ! --------------

  ! -- Output choices
  WRITE( *, FMT     = '( /5x, "Select absorption model" )' )
  DO i = 1, N_MODELS
    WRITE( *, FMT = '( 10x, i1, ") ", a )' ) i, MODEL_NAME( i )
  END DO
  WRITE( *, FMT = '( 5x, "Enter choice: " )', &
            ADVANCE = 'NO' )

  ! -- Read direction input
  READ( *, FMT    = '( i5 )', &
           IOSTAT = IO_Status ) Model

  ! -- Check for errors or invalid values
  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid ABSORPTION MODEL identifier input.', &
                          FAILURE )
    STOP
  END IF

  IF ( Model < 1 .OR. Model > N_MODELS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid ABSORPTION MODEL identifier value.', &
                          FAILURE )
    STOP
  ENDIF


  ! ---------------------------------------------------
  ! Assign optional argument for transmittance function
  ! ---------------------------------------------------

  ! -- Liebe by default....
  Rosenkranz = 0

  ! -- ....unless Rosenkranz is selected
  IF ( Model == MODEL_ROSENKRANZ ) THEN
    Rosenkranz = 1
  END IF



  !#----------------------------------------------------------------------------#
  !#                       -- READ THE SensorInfo FILE --                       #
  !#----------------------------------------------------------------------------#

  ! ---------------------------
  ! Get the SensorInfo filename
  ! ---------------------------

  WRITE( *, FMT     = '( /5x, "Enter the SensorInfo filename: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) SensorInfo_Filename

  SensorInfo_Filename = ADJUSTL( SensorInfo_Filename )


  ! ------------------------
  ! Read the SensorInfo data
  ! ------------------------

  Error_Status = Read_SensorInfo( SensorInfo_Filename, &
                                  SensorInfo_List, &
                                  Quiet = SET )
                               
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME,    &
                          'Error reading SensorInfo file '//TRIM( SensorInfo_Filename ), &
                          FAILURE          )
    STOP
  END IF

  ! -- Count the number of sensors
  n_Sensors = Count_SensorInfo_Nodes( SensorInfo_List )

  IF ( n_Sensors < 1 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'SensorInfo_List is empty.', &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                      -- READ THE INPUT AtmProfile DATA --                  #
  !#----------------------------------------------------------------------------#

  ! ---------------------------
  ! Get the AtmProfile filename
  ! ---------------------------

  WRITE( *, FMT = '( /5x, "Enter the netCDF AtmProfile filename: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a ) ' ) AtmProfile_Filename

  AtmProfile_Filename = ADJUSTL( AtmProfile_Filename )


  ! ---------------------------
  ! Read the AtmProfile dataset
  ! ---------------------------

  Error_Status = Read_AtmProfile_netCDF( TRIM( AtmProfile_Filename ), &
                                         AtmProfile )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading AtmProfile file '//TRIM( AtmProfile_Filename ), &
                          Error_Status )
    STOP
  END IF


  ! --------------------------
  ! Get the profile set ID tag
  ! --------------------------

  Profile_Set_ID_Tag = ' '

  Error_Status = Inquire_AtmProfile_netCDF( TRIM( AtmProfile_Filename ), &
                                            ID_Tag = Profile_Set_ID_Tag )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error inquiring AtmProfile input file '//&
                          TRIM( AtmProfile_Filename ), &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#         -- CONVERT THE WATER VAPOUR AMOUNTS TO PARTIAL PRESSURE --         #
  !#----------------------------------------------------------------------------#

  ! ---------------------------------------------
  ! Determine the H2O index in the absorber array
  ! ---------------------------------------------

  ! -- Make sure there is one water vapour entry present
  n = COUNT( AtmProfile%Absorber_ID == ID_H2O )

  IF ( n == 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'No H2O in absorber set.', &
                          FAILURE )
    STOP
  END IF

  IF ( n > 1 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'More than one H2O identifier in absorber set.', &
                          FAILURE )
    STOP
  END IF

  ! -- Determine the water vapour index
  j_idx = PACK( (/ ( j, j = 1, AtmProfile%n_Absorbers ) /), &
                AtmProfile%Absorber_ID == ID_H2O )

  Index_H2O = j_idx(1)


  ! ------------------------------------------------------------
  ! Allocate the H2O partial pressure array for units conversion
  ! ------------------------------------------------------------

  ALLOCATE( H2O_Pressure( AtmProfile%n_Layers ), STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error allocating H2O partial pressure array. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          FAILURE )
    STOP
  END IF


  ! ---------------------------------------------------------------------
  ! Convert the water vapor amounts from mixing ratio to partial pressure
  ! ---------------------------------------------------------------------

  m_H2O_Convert_Loop: DO m = 1, AtmProfile%n_Profiles

    SELECT CASE ( AtmProfile%Absorber_Units_ID( Index_H2O ) )

      ! -- Convert from ppmv
      CASE ( PPMV_UNITS )
        H2O_Pressure = PPMV_to_PP( AtmProfile%Layer_Pressure( :, m ), &
                                   AtmProfile%Layer_Absorber( :, Index_H2O, m ) )


      ! -- Convert from number density
      CASE ( ND_UNITS )
        H2O_Pressure = ND_to_PP( AtmProfile%Layer_Absorber( :, Index_H2O, m ), &
                                 AtmProfile%Layer_Temperature( :, m ) )


      ! -- Convert from mixing ratio
      CASE ( MR_UNITS )
        H2O_Pressure = MR_to_PP( AtmProfile%Layer_Pressure( :, m ), &
                                 AtmProfile%Layer_Absorber( :, Index_H2O, m ) )


      ! -- Convert from mass density
      CASE ( MD_UNITS )
        H2O_Pressure = MD_to_PP( AtmProfile%Layer_Absorber( :, Index_H2O, m ), &
                                 AtmProfile%Layer_Temperature( :, m ) )


      ! -- Partial pressure input
      CASE ( PP_UNITS )
        H2O_Pressure = AtmProfile%Layer_Absorber( :, Index_H2O, m )


      ! -- Any other input
      CASE DEFAULT
        CALL Display_Message( PROGRAM_NAME, &
                              'Unrecognised water vapour units.', &
                              FAILURE )
        STOP

    END SELECT

    ! -- Check the result
    IF ( ANY( H2O_Pressure < ZERO ) ) THEN
      WRITE( Message, '( "Error converting water vapor units to hPa for AtmProfile #", i3, &
                        &" from ", a, "." )' ) &
                      m, TRIM( AtmProfile_Filename )
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF
    
    ! -- Save the partial pressure in the structure array
    AtmProfile%Layer_Absorber( :, Index_H2O, m ) = H2O_Pressure

  END DO m_H2O_Convert_Loop

  ! -- Update the absorber units ID
  AtmProfile%Absorber_Units_ID( Index_H2O ) = PP_UNITS



  ! -----------------------------------------
  ! Deallocate the H2O partial pressure array
  ! -----------------------------------------

  DEALLOCATE( H2O_Pressure, STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error deallocating H2O partial pressure array. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          WARNING )
  END IF



  !#----------------------------------------------------------------------------#
  !#              -- ALLOCATE THE FREQUENCY DEPENDENT ARRAYS --                 #
  !#----------------------------------------------------------------------------#

  ALLOCATE( Frequency( N_FREQUENCIES ), &
            TauALL( N_FREQUENCIES, AtmProfile%n_Layers, N_ANGLES ), &
            TauWLO( N_FREQUENCIES, AtmProfile%n_Layers, N_ANGLES ), &
            TauWCO( N_FREQUENCIES, AtmProfile%n_Layers, N_ANGLES ), &
            TauWET( N_FREQUENCIES, AtmProfile%n_Layers, N_ANGLES ), &
            TauDRY( N_FREQUENCIES, AtmProfile%n_Layers, N_ANGLES ), &
            STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    Error_Status = FAILURE
    WRITE( Message, '( "Error allocating frequency and transmittance arrays. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                          -- BEGIN SENSOR LOOP --                           #
  !#----------------------------------------------------------------------------#


  n_Sensor_loop: DO n = 1, n_Sensors



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
    !#                   -- ONLY PROCESS MICROWAVE SENSORS --                   #
    !#--------------------------------------------------------------------------#

    Microwave_Sensor: IF ( SensorInfo%Microwave_Flag == MICROWAVE_SENSOR_TYPE ) THEN

      ! -- Output an info message
      WRITE( *, '( //5x, "Calculating ", a, " transmittances for ", a, 1x, a )' ) &
                TRIM( DIRECTION_NAME( Direction ) ), &
                TRIM( SensorInfo%Satellite_Name ), &
                TRIM( SensorInfo%Sensor_Name )



      !#------------------------------------------------------------------------#
      !#               -- LOAD THE CURRENT MICROWAVE SENSOR DATA --             #
      !#------------------------------------------------------------------------#

      Error_Status = Load_MW_SensorData( MW_SensorData, &
                                         NCEP_Sensor_ID = SensorInfo%NCEP_Sensor_ID, &
                                         RCS_Id = MW_SensorData_RCS_ID )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error loading MW sensor data for '//&
                              TRIM( SensorInfo%Satellite_Name )//' '//&
                              TRIM( SensorInfo%Sensor_Name ), &
                              Error_Status )
        STOP
      END IF



      !#------------------------------------------------------------------------#
      !#     -- ALLOCATE THE TauPROFILE STRUCTURE FOR THE CURRENT SENSOR --     #
      !#------------------------------------------------------------------------#

      Error_Status = Allocate_TauProfile( AtmProfile%n_Layers, &
                                          MW_SensorData%n_Channels, &
                                          N_ANGLES, &
                                          AtmProfile%n_Profiles, &
                                          N_MOLECULE_SETS, &

                                          TauProfile )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error allocating TauProfile structure for '//&
                              TRIM( SensorInfo%Satellite_Name )//' '//&
                              TRIM( SensorInfo%Sensor_Name ), &
                              Error_Status )
        STOP
      END IF



      !#------------------------------------------------------------------------#
      !#                     -- LOOP OVER INSTRUMENT CHANNELS --                #
      !#------------------------------------------------------------------------#

      l_Channel_loop: DO l = 1, MW_SensorData%n_Channels

        WRITE( *, '( 10x, "Channel ", i2, "...." )' ) MW_SensorData%Sensor_Channel( l )



        !#----------------------------------------------------------------------#
        !#                    -- COMPUTE THE FREQUENCY GRID --                  #
        !#----------------------------------------------------------------------#

        ! -----------------------------------------------
        ! Compute the number of points in the sideband(s)
        ! -----------------------------------------------

        ! -- First determine the total frequency range in the sidebands
        df = ZERO
        DO ln = 1, MW_SensorData%n_Sidebands( l )
          df = df + ( MW_SensorData%IF_Band(2,ln,l) - MW_SensorData%IF_Band(1,ln,l) )
        END DO


        ! -- Now determine the frequency interval for this frequency
        ! -- range to provide the required number of points. Note that
        ! -- for > 1 sideband channels, the divisor is n-2, not n-1.
        ! -- This is to exclude the "space" between the sidebands in
        ! -- the frequency interval calculation. E.g.:
        ! --
        ! --       Sideband 1             Sideband 2
        ! --     |-----------|      |-------------------|
        ! --     x   x   x   x      x   x   x   x   x   x
        ! --     1   2   3   4      5   6   7   8   9  10   N_HALFPOINTS (n)
        ! --
        ! --       1   2   3          4   5   6   7   8     INTERVALS    (n-2)

        IF ( MW_SensorData%n_Sidebands( l ) == 1 ) THEN
          Delta_Frequency = df / REAL( N_HALFPOINTS - 1, fp_kind )
        ELSE
          Delta_Frequency = df / REAL( N_HALFPOINTS - 2, fp_kind )
        END IF


        ! -- Now determine the number of points for each sideband.
        ! -- Note that for single sideband channels, this will
        ! -- *always* be N_HALFPOINTS. For double sideband channels
        ! -- the points will be split up according to how broad the
        ! -- the sidebands are. E.g.: If they are the same width, then
        ! -- we'll get N_HALFPOINTS/2 points per sideband
        n_Points(:) = 0
        DO ln = 1, MW_SensorData%n_Sidebands( l )
          df = MW_SensorData%IF_Band(2,ln,l) - MW_SensorData%IF_Band(1,ln,l)
          n_Points(ln) = NINT( df / Delta_Frequency ) + 1
        END DO


        ! -- Check the result
        IF ( SUM( n_Points ) /= n_HalfPoints ) THEN
          Error_Status = FAILURE
          WRITE( Message, '( "Error computing n_HalfPoints for channel ", i2, &
                            &" of ", a,1x,a, ". Computed value is ", i5 )' ) &
                          MW_SensorData%Sensor_Channel( l ), &
                          TRIM( SensorInfo%Satellite_Name ), &
                          TRIM( SensorInfo%Sensor_Name ), &
                          SUM( n_Points )
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                Error_Status )
          STOP
        END IF


        ! -----------------------------------------------------------
        ! Fill the frequency array. It's a bit convoluted as I
        ! want the frequencies to be in ascending order.
        !
        ! For the first sideband loop:
        !
        !   -Sideband2   -Sideband1    F0     +Sideband1   +Sideband2
        !                               ^
        !    |------|   |----------|    |    |----------|   |------|
        !                               |
        !               |<.........|         |.........>|
        !                  n_Lower             n_Upper
        !
        ! For the second sideband loop:
        !
        !                              F0      Sideband1    Sideband2
        !                               ^
        !    |------|   |----------|    |    |----------|   |------|
        !                               |
        !    |<.....|                                       |.....>|
        !     n_Lower                                        n_Upper
        !
        ! -----------------------------------------------------------

        ! -- Initialise the sideband point offset. This is the
        ! -- the number of points offset from the central frequency
        ! -- for a sideband.
        n_OffsetPoints = 0

        ! -- Loop over the number of sidebands
        DO ln = 1, MW_SensorData%n_Sidebands(l)

          ! -- Assign the start intermediate frequency for the sideband
          f1 = MW_SensorData%IF_Band( 1, ln, l )

          ! -- Loop over the number of points in the sideband
          DO i = 1, n_Points( ln )

            ! -- Determine the positions of the frequencies in the array
            i_Upper = N_HALFPOINTS + n_OffsetPoints + i
            i_Lower = N_HALFPOINTS - n_OffsetPoints - i + 1

            ! -- Compute the frequency offset
            f = f1 + ( REAL( i-1, fp_kind ) * Delta_Frequency )

            ! -- Apply the offset to the central frequency
            Frequency( i_Upper ) = MW_SensorData%Central_Frequency(l) + f
            Frequency( i_Lower ) = MW_SensorData%Central_Frequency(l) - f

          END DO

          ! -- Update the number of offset points
          n_OffsetPoints = n_OffsetPoints + n_Points( ln )

        END DO

        ! -- Output information on the comp[uted frequency interval
        WRITE( *, '( 10x, "Frequency interval for ", a, 1x, a, &
                     &" channel ", i2, ": ", es13.6, " GHz" )' ) &
                  TRIM( SensorInfo%Satellite_Name ), &
                  TRIM( SensorInfo%Sensor_Name ), &
                  MW_SensorData%Sensor_Channel( l ), &
                  Delta_Frequency



        !#----------------------------------------------------------------------#
        !#                       -- LOOP OVER PROFILES --                       #
        !#----------------------------------------------------------------------#

        m_profile_loop: DO m = 1, AtmProfile%n_Profiles

           WRITE( *, '( 10x, "Processing profile #", i3, "...." )' ) m



          !#--------------------------------------------------------------------#
          !#                 -- CALL THE TRANSMITTANCE FUNCTION --              #
          !#--------------------------------------------------------------------#

          Error_Status = MWLBL_Compute_Tau( AtmProfile%Layer_Pressure(:,m),           &  ! Input
                                            AtmProfile%Layer_Temperature(:,m),        &  ! Input
                                            AtmProfile%Layer_Absorber(:,Index_H2O,m), &  ! Input
                                            AtmProfile%Layer_Delta_Z(:,m),            &  ! Input
                                            ANGLE_SECANT,                             &  ! Input
                                            Frequency,                                &  ! Input

                                            TauALL,                                   &  ! Output
                                            TauWLO,                                   &  ! Output
                                            TauWCO,                                   &  ! Output
                                            TauWET,                                   &  ! Output
                                            TauDRY,                                   &  ! Output

                                            Downwelling = Downwelling,                &  ! Optional input
                                            Rosenkranz = Rosenkranz,                  &  ! Optional input
                                            Quiet = 1,                                &  ! Optional input
                                            RCS_ID = MWLBL_Transmittance_RCS_Id       )  ! Revision control

          IF ( Error_Status == FAILURE ) THEN
            WRITE( Message, '( "Error computing MW LBL transmittance for profile #", i3, &
                              &", channel ", i2, " of ", a,1x,a, "." )' ) &
                            m, &
                            MW_SensorData%Sensor_Channel( l ), &
                            TRIM( SensorInfo%Satellite_Name ), &
                            TRIM( SensorInfo%Sensor_Name )
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM( message ), &
                                  FAILURE )
            STOP
          END IF


          ! ----------------------------------------------
          ! Average the data over the frequencies, but not
          ! for the effective transmittances
          ! ----------------------------------------------

          DO j = 1, N_MOLECULE_SETS - 2  ! Last two are effective WLO and DRY

            ! -- Use a pointer to the particular transmittances
            SELECT CASE ( j )
              CASE ( WLO_IDX )
                Tau => TauWLO
              CASE ( ALL_IDX )
                Tau => TauALL
              CASE ( WET_IDX )
                Tau => TauWET
              CASE ( DRY_IDX )
                Tau => TauDRY
              CASE ( WCO_IDX )
                Tau => TauWCO
              CASE DEFAULT
                WRITE( Message, '( "Invalid molecule list index found at profile #", i3, &
                                  &", channel ", i2, " of ", a,1x,a, "." )' ) &
                                m, &
                                MW_SensorData%Sensor_Channel( l ), &
                                TRIM( SensorInfo%Satellite_Name ), &
                                TRIM( SensorInfo%Sensor_Name )
                CALL Display_Message( PROGRAM_NAME, &
                                      TRIM( message ), &
                                      FAILURE )
                STOP
            END SELECT

            ! -- Loop over the other transmittance dimensions. Note
            ! -- that the assumption here is that the instrument
            ! -- channel response is unity for all frequencies in
            ! -- the band pass.
            DO i = 1, N_ANGLES
              DO k = 1, AtmProfile%n_Layers

                ! -- Sum over all frequencies using Kahan's
                ! -- compensated summation algorithm
                SumTau   = ZERO
                SumError = ZERO
                DO ln = 1, N_FREQUENCIES
                  SumTemp  = SumTau
                  SumY     = Tau( ln, k, i ) + SumError
                  SumTau   = SumTemp + SumY
                  SumError = ( SumTemp - SumTau ) + SumY
                END DO  ! ln

                ! -- Normalise the result
                TauProfile%Tau( k, l, i, m, j ) =              SumTau                / &
                !                                 ----------------------------------
                                                    REAL( N_FREQUENCIES, fp_kind )

              END DO  ! k
            END DO  ! i
          END DO  ! j

          ! -- Nullify the transmittance array pointer
          NULLIFY( Tau )


          ! ---------------------------------------------------
          ! Compute the EFFECTIVE transmittances.
          !
          ! Note that using the WHERE construct causes compiler
          ! errors on older SGI compilers.
          ! ---------------------------------------------------

          DO i = 1, N_ANGLES
            DO k = 1, AtmProfile%n_Layers


              ! -- The EFFECTIVE WLO transmittances
              IF( TauProfile%Tau( k, l, i, m, WCO_IDX ) > TOLERANCE ) THEN
                TauProfile%Tau( k, l, i, m, EFFECTIVE_WLO_IDX ) = TauProfile%Tau( k, l, i, m, WET_IDX ) / &
                !                                                 -------------------------------------
                                                                  TauProfile%Tau( k, l, i, m, WCO_IDX )

              ELSE
                TauProfile%Tau( k, l, i, m, EFFECTIVE_WLO_IDX ) = -ONE
              END IF


              ! -- The EFFECTIVE DRY transmittances
              IF( TauProfile%Tau( k, l, i, m, WET_IDX ) > TOLERANCE ) THEN
                TauProfile%Tau( k, l, i, m, EFFECTIVE_DRY_IDX ) = TauProfile%Tau( k, l, i, m, ALL_IDX ) / &
                !                                                 -------------------------------------
                                                                  TauProfile%Tau( k, l, i, m, WET_IDX )

              ELSE
                TauProfile%Tau( k, l, i, m, EFFECTIVE_DRY_IDX ) = -ONE
              END IF

            END DO  ! k
          END DO  ! i

        END DO m_Profile_Loop

      END DO l_Channel_Loop



      !#------------------------------------------------------------------------#
      !#                     -- OUTPUT THE TauProfile DATA --                   #
      !#------------------------------------------------------------------------#

      ! -------------------------
      ! Create the comment string
      ! -------------------------

      WRITE( Comment, '( "Number of points used per channel in LBL calculation: ", i5, &
                        &". Absorption model used: ", a )' ) &
                      N_FREQUENCIES, MODEL_NAME( Model )


      ! ---------------------------
      ! Create the output data file
      ! ---------------------------

      TauProfile_Filename = TRIM( DIRECTION_NAME( Direction ) )//'.'//&
                            TRIM( SensorInfo%File_Prefix )//&
                            '.TauProfile.nc'

      Error_Status = Create_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                               AtmProfile%Level_Pressure(:,1), &
                                               MW_SensorData%Sensor_Channel, &
                                               ANGLE_SECANT, &
                                               (/ ( m, m = 1, AtmProfile%n_Profiles ) /), &
                                               MOLECULE_SET_LIST, &

                                               NCEP_Sensor_ID   = SensorInfo%NCEP_Sensor_ID, &
                                               WMO_Satellite_ID = SensorInfo%WMO_Satellite_ID, &
                                               WMO_Sensor_ID    = SensorInfo%WMO_Sensor_ID, &

                                               ID_Tag = TRIM( PROFILE_SET_ID_TAG ), &
                                               Title = TRIM( SensorInfo%Sensor_Name )//' '//&
                                                       TRIM( DIRECTION_NAME( Direction ) )//&
                                                       ' transmittances for '//&
                                                       TRIM( SensorInfo%Satellite_Name ), &
                                               History = PROGRAM_RCS_ID//'; '//&
                                                         TRIM( MWLBL_Transmittance_RCS_Id )//'; '//&
                                                         TRIM( MW_SensorData_RCS_Id ), &
                                               Sensor_Name   = TRIM( SensorInfo%Sensor_Name ), &
                                               Platform_Name = TRIM( SensorInfo%Satellite_Name ), &
                                               Comment = TRIM( Comment ) )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error creating netCDF TauProfile file '//&
                              TRIM( TauProfile_Filename ), &
                              Error_Status )
        STOP
      END IF


      ! ----------------------
      ! Write the data to file
      ! ----------------------

      Error_Status = Write_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                              TauProfile )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error writing TauProfile structure for '//&
                              TRIM( SensorInfo%Satellite_Name )//' '//&
                              TRIM( SensorInfo%Sensor_Name )//' to '//&
                              TRIM( TauProfile_Filename ), &
                              Error_Status )
        STOP
      END IF



      !#------------------------------------------------------------------------#
      !#             -- DESTROY THE CURRENT TauPROFILE STRUCTURE --             #
      !#------------------------------------------------------------------------#

      Error_Status = Destroy_TauProfile( TauProfile )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error destroying TauProfile structure for '//&
                              TRIM( SensorInfo%Satellite_Name )//' '//&
                              TRIM( SensorInfo%Sensor_Name ), &
                              Error_Status )
        STOP
      END IF



      !#------------------------------------------------------------------------#
      !#        -- DESTROY THE CURRENT MICROWAVE SENSOR DATA STRUCTURE --       #
      !#------------------------------------------------------------------------#

      Error_Status = Destroy_MW_SensorData( MW_SensorData )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error destroying MW sensor data structure for sensor #", i2 )' ) n
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF

    END IF Microwave_Sensor



    !#--------------------------------------------------------------------------#
    !#                   -- DESTROY THE SensorInfo STRUCTURE --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = Destroy_SensorInfo( SensorInfo )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error destroying SensorInfo structure for sensor #", i2 )' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF

  END DO n_Sensor_Loop



  !#----------------------------------------------------------------------------#
  !#          -- DEALLOCATE THE FREQUENCY AND TRANSMITTANCE ARRAYS --           #
  !#----------------------------------------------------------------------------#

  DEALLOCATE( Frequency, &
              TauALL, &
              TauWLO, &
              TauWCO, &
              TauWET, &
              TauDRY, &
              STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    Error_Status = FAILURE
    WRITE( Message, '( "Error deallocating frequency and transmittance arrays. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                    -- DESTROY THE AtmProfile STRUCTURE --                  #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_AtmProfile( AtmProfile )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying AtmProfile structure array.', &
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

END PROGRAM Compute_MW_Transmittance


!#------------------------------------------------------------------------------#
!#                          -- MODIFICATION HISTORY --                          # 
!#------------------------------------------------------------------------------#
!
! $Id: Compute_MW_Transmittance.f90,v 2.3 2006/06/30 16:47:16 dgroff Exp $
!
! $Date: 2006/06/30 16:47:16 $
!
! $Revision: 2.3 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Compute_MW_Transmittance.f90,v $
! Revision 2.3  2006/06/30 16:47:16  dgroff
! Changed "Error_Handler" references to "Message_Handler"
!
! Revision 2.2  2005/01/25 21:35:27  paulv
! - Removed type declarations of unused variables.
!
! Revision 2.1  2004/12/16 18:41:31  paulv
! - Upgraded to Fortran-95
! - Added ability for user to select between Liebe and Rosenkranz models.
!
! Revision 2.0  2004/05/27 22:26:48  paulv
! - New version. All component transmittances are now output:
!     TauWLO, TauALL, TauWET, TauDRY, TauWCO, TauEffWLO, and TauEffDRY.
!
! Revision 1.8  2004/02/04 19:47:05  paulv
! - Now can use any SensorInfo file and only the microwave sensors will have
!   their transmittances calculated.
! - Updated the interface to the TauProfile routines to reflect changes in
!   the TauProfile netCDF I/O module.
! - Updated the header documentation.
!
! Revision 1.7  2003/05/27 14:32:26  paulv
! - Corrected all references to n_Layers and n_Profiles to AtmProfile%n_Layers
!   and AtmProfile%n_Profiles.
! - Replaced water vapour array index variable j_idx(1) with a scalar variable
!   Index_H2O
! - Corrected bug in call to ND_to_PP where the layer temperature argument
!   was being specified incorrectly.
! - Added n_Sensors to local variable list.
!
! Revision 1.6  2003/05/23 20:52:51  paulv
! - Altered use of AtmProfile routines due to interface change.
! - Now using linked list form of SensorInfo reasder.
! - Experimenting with compensated summation algorithm in transmittance
!   summation across the bandwidth.
!
! Revision 1.5  2003/03/18 15:53:27  paulv
! - Added frequency interval information output.
! - Added comment global attribute to output TauProfile data files.
!
! Revision 1.4  2003/03/17 15:33:59  paulv
! - Added capability to select in which direction (up or downwelling) the
!   transmittance calculation is done. This is not the best way since all the
!   profile parameters are recalculated, but will do for now.
! - Changed the method of computing the number of points in an instrument
!   passband. Previously, a fixed frequency interval was used. This led to a
!   large number of points for wider bands that didn't require such a fine
!   frequency spacing - which led to long execution times. Now the number
!   of points is fixed at 256 (this can change though) - a number divisible
!   by 2 and 4 - and the frequency interval is determined dynamically.
!
! Revision 1.3  2003/03/03 19:52:15  paulv
! - Completed, running version. Still testing.
!
! Revision 1.2  2002/12/16 14:36:14  paulv
! - Added USE statement for transmittance module.
!
! Revision 1.1  2002/09/20 16:20:42  paulv
! Initial checkin.
!
!
!
