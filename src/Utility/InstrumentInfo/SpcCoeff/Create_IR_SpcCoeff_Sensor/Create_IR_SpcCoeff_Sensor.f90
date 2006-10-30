!------------------------------------------------------------------------------
!P+
! NAME:
!       Create_IR_SpcCoeff_Sensor
!
! PURPOSE:
!       Program to create the SENSOR infrared spectral coefficient (SpcCoeff)
!       data files from the sensor SRF data files.
!
! CATEGORY:
!       Instrument_Information
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:                 Module containing definitions for kinds
!                                   of variable types.
!
!       File_Utility:               Module containing generic file utility routines
!
!       Message_Handler:            Module to define simple error codes and
!                                   handle error conditions
!                                   USEs: FILE_UTILITY module
!
!       Compare_Float_Numbers:      Module containing routines to perform equality
!                                   check comparisons on input floating point
!                                   numbers.
!                                   USEs: TYPE_KINDS module
!
!       Fundamental_Constants:      Module containing various fundamental
!                                   physical constants.
!                                   USEs: TYPE_KINDS module
!
!       Planck_Functions:           Module containing Planck function radiance,
!                                   temperature, dB/dT, and dT/dB routines.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         FUNDAMENTAL_CONSTANTS module
!
!       Interpolate:                Module containing interpolation routines
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!       Integrate:                  Module containing integration routines.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         INTERPOLATE module
!
!       Spectral_Units_Conversion:  Module containing functions to convert
!                                   between various spectral units.
!                                   USEs: TYPE_KINDS module
!                                         FUNDAMENTAL_CONSTANTS module
!
!       SensorInfo_Define:          Module defining the SensorInfo data structure and
!                                   containing routines to manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         FILE_UTILITY module
!                                         ERROR_HANDLER module
!
!       SensorInfo_LinkedList:      Module defining the SensorInfo Linked List
!                                   data structure and containing routines to
!                                   manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         SENSORINFO_DEFINE module
!
!       SensorInfo_IO:              Module continaing routines to read and write ASCII
!                                   SensorInfo format files.
!                                   USEs: TYPE_KINDS module
!                                         FILE_UTILITY module
!                                         ERROR_HANDLER module
!                                         SensorInfo_DEFINE module
!
!       SRF_Define:                 Module defining the generic SRF data structure
!                                   and its manipulation routines.
!                                   USEs: TYPE_KINDS module
!                                         FILE_UTILITY module
!                                         ERROR_HANDLER module
!
!       SRF_netCDF_IO:              Module containing routines to read and write
!                                   netCDF format SRF files.
!                                   USEs: TYPE_KINDS module
!                                         FILE_UTILITY module
!                                         ERROR_HANDLER module
!                                         SRF_DEFINE module
!                                         NETCDF module
!                                         NETCDF_UTILITY module
!
!       Solar_Define:                Module defining the Solar data structure
!                                    and containing routines to manipulate it.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!
!       Solar_netCDF_IO:             Module containing routines to read and write
!                                    netCDF format Solar files.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!                                          SOLAR_DEFINE module
!                                          NETCDF module
!                                          NETCDF_UTILITY module
!
!       SpcCoeff_Define:             Module defining the SpcCoeff data structure
!                                    and containing routines to manipulate it.
!                                    USEs: TYPE_KINDS module
!                                          FILE_UTILITY module
!                                          ERROR_HANDLER module
!
!       SpcCoeff_netCDF_IO:          Module continaing routines to read and write
!                                    netCDF format SpcCoeff files.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!                                          SPCCOEFF_DEFINE module
!                                          NETCDF module
!                                          NETCDF_UTILITY module
!
! CONTAINS:
!       Linear_Least_Squares_Fit:  Function to fit a line to an input X,Y
!                                  dataset by linear least squares.
!
!       Compute_Frequency_Index:   Function to determine the frequency array
!                                  element index for an input frequency value.
!
! INCLUDE FILES:
!       None
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       Input:
!         - ASCII SensorInfo data file
!         - netCDF SRF data file
!         - netCDF Solar data file
!
!       Output:
!         - ASCII file containing the band correction fit equations and data.
!         - netCDF SpcCoeff data file containing all the required data.
!
! SIDE EFFECTS:
!       If any of the output files exist, they are overwritten.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Jan-2002
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
!P-
!------------------------------------------------------------------------------

PROGRAM Create_IR_SpcCoeff_Sensor


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE Compare_Float_Numbers

  USE Fundamental_Constants, ONLY: C_1, C_2
  USE Planck_Functions
  USE Interpolate
  USE Integrate

  USE Spectral_Units_Conversion

  USE SensorInfo_Define, SENSORINFO_INFRARED => INFRARED_SENSOR_TYPE
  USE SensorInfo_LinkedList
  USE SensorInfo_IO

  USE SRF_Define
  USE SRF_netCDF_IO

  USE Solar_Define
  USE Solar_netCDF_IO

  USE SpcCoeff_Define, SPCCOEFF_INFRARED => INFRARED_SENSOR
  USE SpcCoeff_netCDF_IO


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Create_IR_SpcCoeff_Sensor'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Create_IR_SpcCoeff_Sensor.f90,v 6.4 2006/05/02 16:58:03 dgroff Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  ! -- Keyword set flag
  INTEGER, PARAMETER :: SET = 1

  ! -- The following scaling factors are applied to produce radiances in units
  ! -- of mW/(m^2.sr.cm^-1) when they are used.

  ! -- First Planck function constant (C_1) scale factors. Units of C_1 are W.m^2.
  ! -- Length scaling: To convert to W/(m^2.cm^-4) requires a scaling of m->cm,
  ! --                 which is 100, to the fourth power, which is 1.0e+08.
  ! -- Power scaling:  To convert to mW.m^2 requires a scaling of 1000.
  REAL( fp_kind ), PARAMETER :: C_1_LENGTH_SCALE_FACTOR = 1.0e+08_fp_kind
  REAL( fp_kind ), PARAMETER :: C_1_POWER_SCALE_FACTOR  = 1.0e+03_fp_kind
  REAL( fp_kind ), PARAMETER :: C_1_SCALE_FACTOR = C_1_LENGTH_SCALE_FACTOR * C_1_POWER_SCALE_FACTOR

  ! -- Second Planck function constant (C_2) scale factor. Units of C_2 are K.m,
  ! -- So to convert to K.cm, a scaling of 100 is applied.
  REAL( fp_kind ), PARAMETER :: C_2_SCALE_FACTOR = 100.0_fp_kind

  ! -- The number and range of temperatures used in determining the 
  ! -- polychromatic correction coefficients
  INTEGER,         PARAMETER :: N_TEMPERATURES = 17
  REAL( fp_kind ), PARAMETER :: MIN_TEMPERATURE = 180.0_fp_kind
  REAL( fp_kind ), PARAMETER :: MAX_TEMPERATURE = 340.0_fp_kind

  ! -- Solar channel cut-off frequency
!  REAL( fp_kind ), PARAMETER :: SOLAR_CUTOFF_WAVENUMBER = 775.0_fp_kind
  REAL( fp_kind ), PARAMETER :: SOLAR_CUTOFF_WAVENUMBER = 1800.0_fp_kind

  ! -- Numerical constants
  REAL( fp_kind ), PARAMETER :: ZERO = 0.0_fp_kind

  ! -- Integration methods
  INTEGER, PARAMETER :: N_INTEGRATION_METHODS = 2
  INTEGER, PARAMETER :: SUMMATION_METHOD = 1
  INTEGER, PARAMETER :: INTEGRATE_METHOD = 2
  INTEGER, PARAMETER, DIMENSION( N_INTEGRATION_METHODS ) :: &
    INTEGRATION_METHOD = (/ SUMMATION_METHOD, &
                            INTEGRATE_METHOD /)
  CHARACTER( * ), PARAMETER, DIMENSION( N_INTEGRATION_METHODS ) :: &
    INTEGRATION_METHOD_NAME = (/ 'Summation  ', &
                                 'Integration' /)

  ! -- Interpolation order
  INTEGER, PARAMETER :: LINEAR_ORDER = 1
  INTEGER, PARAMETER ::  CUBIC_ORDER = 3


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  CHARACTER( 256 ) :: SensorInfo_Filename
  CHARACTER( 256 ) :: SRF_Filename
  CHARACTER( 256 ) :: Solar_Filename
  CHARACTER( 256 ) :: SpcCoeff_Filename
  CHARACTER( 256 ) :: ASCII_Filename
  INTEGER          :: ASCII_fileID

  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: IO_Status
 
  INTEGER :: l
  INTEGER :: ls1, ls2
  INTEGER :: i, n

  INTEGER :: SRF_Integration_Method
  INTEGER :: SpcCoeff_File_Version

  CHARACTER(  256 ) :: Title
  CHARACTER( 2000 ) :: SRF_History, Solar_History
  CHARACTER(  256 ) :: Sensor_Name
  CHARACTER(  256 ) :: Platform_Name
  CHARACTER( 2000 ) :: Comment

  REAL( fp_kind ) :: dFrequency
  REAL( fp_kind ) :: SRF_Integral
  REAL( fp_kind ) :: SRF_First_Moment

  REAL( fp_kind ), DIMENSION( N_TEMPERATURES ) :: x_Temperature
  REAL( fp_kind ), DIMENSION( N_TEMPERATURES ) :: y_Effective_Temperature
  REAL( fp_kind ), DIMENSION( N_TEMPERATURES ) :: yFit_Effective_Temperature
  REAL( fp_kind ) :: var_Effective_Temperature

  REAL( fp_kind ), DIMENSION( : ), ALLOCATABLE, TARGET :: Spectrum
  REAL( fp_kind ), DIMENSION( : ), POINTER             :: Radiance   => NULL()
  REAL( fp_kind ), DIMENSION( : ), POINTER             :: Irradiance => NULL()

  REAL( fp_kind ) :: Convolved_Radiance

  REAL( fp_kind ) :: v1, v2

  INTEGER :: n_Sensors
  TYPE( SensorInfo_type )      :: SensorInfo
  TYPE( SensorInfo_List_type ) :: SensorInfo_List
  TYPE( SRF_type )             :: SRF
  TYPE( Solar_type )           :: Solar
  TYPE( SpcCoeff_Sensor_type ) :: SpcCoeff

  REAL( fp_kind ), DIMENSION( : ), ALLOCATABLE :: Solar_Derivative



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a)' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to create the SENSOR infrared spectral")' )
  WRITE( *, '( 5x, "   coefficient (SpcCoeff) data files from the")' )
  WRITE( *, '( 5x, "   sensor SRF data files.")' )
  WRITE( *, '(/5x, " $Revision: 6.4 $")' )
  WRITE( *, '( 5x, a)' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                         -- GET USER INPUTS --                              #
  !#----------------------------------------------------------------------------#

  ! ----------------------------
  ! Read the SensorInfo filename
  ! ----------------------------

  ! -- Get the filename
  WRITE( *, FMT     = '( /5x, "Enter a SensorInfo filename: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) SensorInfo_Filename
  SensorInfo_Filename = ADJUSTL( SensorInfo_Filename )

  ! -- Read the SensorInfo file into the linked list
  Error_Status = Read_SensorInfo( TRIM( SensorInfo_Filename ), &
                                  SensorInfo_List, &
                                  Quiet = SET )
                               
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading SensorInfo file '//&
                          TRIM( SensorInfo_Filename ), &
                          FAILURE )
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


  ! ------------------
  ! Integration method
  ! ------------------

  WRITE( *, FMT = '( /5x, "Select SRF integration method" )' )
  DO i = 1, N_INTEGRATION_METHODS
    WRITE( *, FMT = '( 10x, i1, ") ", a )' ) i, INTEGRATION_METHOD_NAME(i)
  END DO
  WRITE( *, FMT     = '( 5x, "Enter choice : " )', &
            ADVANCE = 'NO' )
  READ( *, * ) SRF_Integration_Method

  IF ( SRF_Integration_Method < 1                     .OR. &
       SRF_Integration_Method > N_INTEGRATION_METHODS      ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid integration method. Using summation.', &
                          INFORMATION )
    SRF_Integration_Method = SUMMATION_METHOD
  END IF


  ! --------------------
  ! The SpcCoeff version
  ! --------------------

  WRITE( *, FMT     = '( /5x, "Default SpcCoeff file version is: ", i3, &
                             &".  Enter value: " )', &
            ADVANCE = 'NO' ) SpcCoeff%Version
  READ( *, * ) SpcCoeff_File_Version

  IF ( SpcCoeff_File_Version < SpcCoeff%Version ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid version number specified. Using default.', &
                          INFORMATION )
    SpcCoeff_File_Version = SpcCoeff%Version
  END IF

  

  !#----------------------------------------------------------------------------#
  !#                       -- READ THE netCDF Solar FILE --                     #
  !#----------------------------------------------------------------------------#

  Solar_Filename = 'solar.nc'

  WRITE( *, '( /5x, "Reading the solar irradiance data file ", a, "..." )' ) &
            TRIM( Solar_Filename )


  ! ---------------------------------------------
  ! Inquire the file to get the history attribute
  ! ---------------------------------------------

  Error_Status = Inquire_Solar_netCDF( Solar_Filename, &
                                       History = Solar_History )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error inquiring netCDF Solar file '//TRIM( Solar_Filename ), &
                          Error_Status )
    STOP
  END IF


  ! -------------
  ! Read the file
  ! -------------

  Error_Status = Read_Solar_netCDF( TRIM( Solar_Filename ), &
                                    Solar )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading netCDF Solar file '//TRIM( Solar_Filename ), &
                          Error_Status )
    STOP
  END IF


  ! ----------------------------
  ! Compute the derivatives for
  ! solar spectrum interpolation
  ! ----------------------------

  ! -- Allocate the solar derivative array
  ALLOCATE( Solar_Derivative( Solar%n_Frequencies ), &
            STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error allocating solar array for interpolation. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME,    &
                          TRIM( Message ), &
                          FAILURE          )
    STOP
  END IF


  ! -- Initialise the spline
  Error_Status = Spline_Initialize( Solar%Frequency, &
                                    Solar%Irradiance, &
                                    Solar_Derivative )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error computing solar irradiance derivatives.', &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#              -- GENERATE THE "MONOCHROMATIC" TEMPERATURES --               #
  !#----------------------------------------------------------------------------#

  x_Temperature = (/ ( REAL( i-1 ), i = 1, N_TEMPERATURES ) /) / &
  !               --------------------------------------------
                           REAL( N_TEMPERATURES - 1 )

  x_Temperature = ( x_Temperature * ( MAX_TEMPERATURE - MIN_TEMPERATURE ) ) + MIN_TEMPERATURE



  !#----------------------------------------------------------------------------#
  !#                      -- BEGIN LOOP OVER SENSORS --                         #
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


    ! ----------------------------------------------
    ! Create the SRF filename for the current sensor
    ! ----------------------------------------------

    SRF_Filename = TRIM( SensorInfo%File_Prefix )//'.srf.nc'



    !#--------------------------------------------------------------------------#
    !#        -- OPERATE ONLY ON INFRARED SENSORS WITH SRF FILE PRESENT --      #
    !#--------------------------------------------------------------------------#

    Infrared_Sensors: IF ( SensorInfo%Microwave_Flag == SENSORINFO_INFRARED       .AND. &
                           File_Exists( TRIM( SRF_Filename ) )                    .AND. &
                           TRIM( SensorInfo%File_Prefix ) /= 'airs281SUBSET_aqua' .AND. &
                           TRIM( SensorInfo%File_Prefix ) /= 'airs324SUBSET_aqua' .AND. &
                           TRIM( SensorInfo%File_Prefix ) /= 'airs_aqua'             ) THEN

   
      ! ----------------------
      ! Output an info message
      ! ----------------------

      WRITE( *, '( //5x, "Creating the SpcCoeff data file for ", a, 1x, a, " (", a, ")" )' ) &
                TRIM( SensorInfo%Satellite_Name ), &
                TRIM( SensorInfo%Sensor_Name ), &
                TRIM( SensorInfo%File_Prefix )



      !#------------------------------------------------------------------------#
      !#                        -- INQUIRE THE SRF FILE --                      #
      !#------------------------------------------------------------------------#

      Error_Status = Inquire_SRF_netCDF( TRIM( SRF_Filename ), &  ! Input
                                         Title         = Title,         & ! Optional output
                                         History       = SRF_History,   & ! Optional output
                                         Sensor_Name   = Sensor_Name,   & ! Optional output
                                         Platform_Name = Platform_Name, & ! Optional output
                                         Comment       = Comment        ) ! Optional output

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error inquiring the netCDF SRF file '//&
                              TRIM( SRF_Filename ), &
                              Error_Status )
        STOP
      END IF



      !#------------------------------------------------------------------------#
      !#                  -- ALLOCATE THE SpcCoeff STRUCTURE --                 #
      !#------------------------------------------------------------------------#

      ! ---------------------------------------------
      ! Central frequency will be calculated from the
      ! SRF directly, so allocate the structure
      ! ---------------------------------------------

      Error_Status = Allocate_SpcCoeff( SensorInfo%n_Channels, &
                                        SpcCoeff )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error allocating SpcCoeff data structure.', &
                              Error_Status )
        STOP
      END IF



      ! ------------------------------------
      ! Set the number of sensors, their IDs,
      ! turn off the microwave flag, and set
      ! the channel polarizations
      ! ------------------------------------

      SpcCoeff%Version = SpcCoeff_File_Version

      SpcCoeff%n_Sensors = 1

      SpcCoeff%Sensor_Descriptor    = TRIM( SensorInfo%File_Prefix )
      SpcCoeff%Sensor_Type          = SPCCOEFF_INFRARED
      SpcCoeff%NCEP_Sensor_ID       = SensorInfo%NCEP_Sensor_ID
      SpcCoeff%WMO_Satellite_ID     = SensorInfo%WMO_Satellite_ID
      SpcCoeff%WMO_Sensor_ID        = SensorInfo%WMO_Sensor_ID
      SpcCoeff%Sensor_Channel       = SensorInfo%Sensor_Channel
      SpcCoeff%Polarization         = UNPOLARIZED



      !#------------------------------------------------------------------------#
      !#           -- OPEN FILE FOR OUTPUT RE: BAND CORRECTION FITS --          #
      !#------------------------------------------------------------------------#

      ! -------------------
      ! Create the filename
      ! -------------------

      ASCII_Filename = TRIM( SensorInfo%File_Prefix )//'.SpcCoeff.asc'


      ! ---------------------------
      ! Get a free file unit number
      ! ---------------------------

      ASCII_fileID = Get_Lun()

      IF ( ASCII_fileID < 0 ) THEN
        Error_Status = FAILURE
        CALL Display_Message( PROGRAM_NAME, &
                              'Error obtaining file unit number for output to '//&
                              TRIM( ASCII_Filename ), &
                              Error_Status )
        STOP
      END IF


      ! -------------------------
      ! Open the output data file
      ! -------------------------

      OPEN( ASCII_fileID, FILE   = TRIM( ASCII_Filename ), &
                          FORM   = 'FORMATTED', &
                          STATUS = 'REPLACE', &
                          ACTION = 'WRITE',     &
                          IOSTAT = IO_Status    )

      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error opening statistics output file ", a, ". STAT = ", i5 )' ) &
                        TRIM( ASCII_Filename ), IO_Status
        CALL Display_Message( PROGRAM_NAME,    &
                              TRIM( Message ), &
                              FAILURE          )
        STOP
      END IF

      WRITE( ASCII_fileID, '( 5x, "SRF data from file: ", a )' ) TRIM( SRF_Filename )



      !#------------------------------------------------------------------------#
      !#                       -- LOOP OVER SRF CHANNELS --                     #
      !#------------------------------------------------------------------------#

      WRITE( *, '( /, "    CH        V0            ", &
                     &"FK1            FK2            BC1            BC2          ", &
                     &"F(Solar)" )' )

      Channel_Loop: DO l = 1, SpcCoeff%n_Channels

        WRITE( *, FMT     = '( 2x, i4 )', &
                  ADVANCE = 'NO'          ) SpcCoeff%Sensor_Channel( l )


        ! -------------------------
        ! Read the current SRF data
        ! -------------------------

        Error_Status = Read_SRF_netCDF( TRIM( SRF_Filename ),         &  ! Input
                                        SpcCoeff%Sensor_Channel( l ), &  ! Input
                                        SRF                           )  ! Output

        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error reading channel #", i5, " SRF from ", a )' ) &
                          SpcCoeff%Sensor_Channel( l ), TRIM( SRF_Filename )
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                Error_Status )
          STOP
        END IF



        !#----------------------------------------------------------------------#
        !#            -- DEFINE THE INTEGRATED SRF AND FIRST MOMENT --          #
        !#----------------------------------------------------------------------#

        IF ( SRF_Integration_Method == SUMMATION_METHOD ) THEN


          ! ---------
          ! Summation
          ! ---------

          ! -- Assign the SRF integral value
          SRF_Integral = SRF%Summation_SRF

          ! -- The un-normalised centroid
          dFrequency = SRF%Frequency( 2 ) - SRF%Frequency( 1 )
          SRF_First_Moment = SUM( SRF%Response * SRF%Frequency ) * dFrequency


        ELSE


          ! -----------
          ! Integration
          ! -----------

          ! -- Assign the SRF integral value
          SRF_Integral = SRF%Integrated_SRF

          ! -- The un-normalised centroid
          Error_Status = Simpsons_Integral( SRF%Frequency, &
                                            SRF%Response * SRF%Frequency, &
                                            SRF_First_Moment, &
                                            Order = CUBIC_ORDER )

          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error occurred calculating SRF first moment by integration.', &
                                  Error_Status )
            STOP
          END IF
        END IF



        !#----------------------------------------------------------------------#
        !#        -- DETERMINE THE SRF CENTROID AND PLANCK COEFFICIENTS --      #
        !#----------------------------------------------------------------------#

        ! ----------------
        ! The SRF centroid
        ! ----------------

        SpcCoeff%Wavenumber( l ) = SRF_First_Moment / SRF_Integral


        ! ------------------------------------
        ! Convert the centroid to units of GHz
        ! ------------------------------------

        SpcCoeff%Frequency( l ) = Inverse_cm_to_GHz( SpcCoeff%Wavenumber( l ) )


        ! -------------------------------
        ! Compute the Planck coefficients
        ! -------------------------------

        SpcCoeff%Planck_C1( l ) = C_1_SCALE_FACTOR * C_1 * ( SpcCoeff%Wavenumber( l )**3 )
        SpcCoeff%Planck_C2( l ) = C_2_SCALE_FACTOR * C_2 *   SpcCoeff%Wavenumber( l )

        WRITE( *, FMT     = '( 3( 2x, es13.6 ) )', &
                  ADVANCE = 'NO' ) SpcCoeff%Wavenumber( l ), &
                                   SpcCoeff%Planck_C1( l ), &
                                   SpcCoeff%Planck_C2( l )


 
        !#----------------------------------------------------------------------#
        !#         -- ALLOCATE THE RADIANCE/IRRADIANCE SPECTRUM ARRAY --        #
        !#----------------------------------------------------------------------#

        ALLOCATE( Spectrum( SRF%n_Points ), STAT = Allocate_Status )

        IF ( Allocate_Status /= 0 ) THEN
          WRITE( Message, '( "Error allocating spectrum array. STAT = ", i5 )' ) &
                          Allocate_Status
          CALL Display_Message( PROGRAM_NAME,    &
                                TRIM( Message ), &
                                FAILURE          )
          STOP
        END IF


        !#----------------------------------------------------------------------#
        !#        -- COMPUTE THE POLYCHROMATIC CORRECTION COEFFICIENTS --       #
        !#----------------------------------------------------------------------#

        ! ------------------------------------
        ! Use the spectrum array for radiances
        ! ------------------------------------

        Radiance => Spectrum


        ! -----------------------------------------
        ! Generate the "polychromatic" temperatures
        ! -----------------------------------------

        Temperature_Loop: DO i = 1, N_TEMPERATURES

          ! -- Calculate monochromatic radiances
          Error_Status = Planck_Radiance( SRF%Frequency, &
                                          x_Temperature( i ), &
                                          Radiance )

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error calculating radiance at T = ", f5.1, " K." )' ) &
                            x_Temperature( i )
            CALL Display_Message( PROGRAM_NAME,    &
                                  TRIM( Message ), &
                                  Error_Status     )
            STOP
          END IF

          ! -- Convolve the radiance spectrum with the SRF
          Error_Status = Simpsons_Integral( SRF%Frequency, &
                                            Radiance * SRF%Response, &
                                            Convolved_Radiance, &
                                            Order = CUBIC_ORDER )

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error convolving radiance at T = ", f5.1, " K." )' ) &
                            x_Temperature( i )
            CALL Display_Message( PROGRAM_NAME,    &
                                  TRIM( Message ), &
                                  Error_Status     )
            STOP
          END IF

          ! -- Normalise the convolved radiance
          Convolved_Radiance = Convolved_Radiance / SRF_Integral

          ! -- Convert the convolved radiance back into a temperature
          Error_Status = Planck_Temperature( SpcCoeff%Wavenumber( l ), &
                                             Convolved_Radiance, &
                                             y_Effective_Temperature( i ) )

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error calculating polychromatic temperature at T = ", f5.1, " K." )' ) &
                            x_Temperature( i )
            CALL Display_Message( PROGRAM_NAME,    &
                                  TRIM( Message ), &
                                  Error_Status     )
            STOP
          END IF

        END DO Temperature_Loop


        ! -------------------------------------
        ! Nullify the radiance spectrum pointer
        ! -------------------------------------

        NULLIFY( Radiance )


        ! --------------------------------------------
        ! Fit the mono- and polychromatic temperatures
        ! --------------------------------------------

        Error_Status = Least_Squares_Linear_Fit( x_Temperature, &
                                                 y_Effective_Temperature, &
                                                 SpcCoeff%Band_C1( l ), &
                                                 SpcCoeff%Band_C2( l ), &
                                                 yFit = yFit_Effective_Temperature, &
                                                 MSE  = var_Effective_Temperature )

        IF ( Error_Status /= SUCCESS ) THEN
          CALL Display_Message( PROGRAM_NAME, &
                                'Error calculating band correction coefficients', &
                                Error_Status  )
          STOP
        END IF

        ! -- Output fit coefficients to screen
        WRITE( *, FMT     = '( 2( 2x, es13.6 ) )', &
                  ADVANCE = 'NO' ) SpcCoeff%Band_C1( l ), &
                                   SpcCoeff%Band_C2( l )

        ! -- Output fit statistics to file
        WRITE( ASCII_fileID, FMT = '( /5x, "CHANNEL ", i4 )' ) SpcCoeff%Sensor_Channel( l )
        WRITE( ASCII_fileID, FMT = '( 2x, "Fit equation : Teff = ", es13.6, " + ", es13.6, " T" )' ) &
                                   SpcCoeff%Band_C1( l ), SpcCoeff%Band_C2( l )
        WRITE( ASCII_fileID, FMT = '( 2x, "MSE : ", es13.6, ";  Sigma : ", es13.6 )' )&
                                   var_Effective_Temperature, SQRT( var_Effective_Temperature )

        WRITE( ASCII_fileID, '( 7x, "T      Teff(true)   Teff(fit)  dTeff(true-fit)" )' )
        WRITE( ASCII_fileID, '( 2x, 49("-") )' )
        DO i = 1, N_TEMPERATURES
          WRITE( ASCII_fileID, '( 3( 2x, f10.6 ), 2x, es13.6 )' ) &
                               x_Temperature( i ), &
                               y_Effective_Temperature( i ),  yFit_Effective_Temperature( i ), &
                               y_Effective_Temperature( i ) - yFit_Effective_Temperature( i )
        END DO




        !#----------------------------------------------------------------------------#
        !#                   -- FILL THE COSMIC BACKGROUND FIELD --                   #
        !#----------------------------------------------------------------------------#

        SpcCoeff%Cosmic_Background_Radiance( l ) = ZERO



        !#----------------------------------------------------------------------------#
        !#              -- CONVOLVE THE SOLAR IRRADIANCE WITH THE SRF --              #
        !#----------------------------------------------------------------------------#

        ! -----------------------------
        ! All Infrared Solar turned off
        ! -----------------------------

        SpcCoeff%Is_Solar_Channel( l ) = 0
        SpcCoeff%Solar_Irradiance( l ) = ZERO


        ! ------------------------------------
        ! Use the spectrum array for radiances
        ! ------------------------------------

        Irradiance => Spectrum


        ! -------------------------------------------------
        ! Interpolate the solar spectrum to the SRF spacing
        ! -------------------------------------------------

        Error_Status = Spline_Interpolate( Solar%Frequency,      &  ! X
                                           Solar%Irradiance,     &  ! Y
                                           SRF%Frequency,        &  ! Xint
                                           Irradiance,           &  ! Yint
                                           y2 = Solar_Derivative )

        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error interpolating solar spectrum for SRF channel ", i4 )' ) &
                          SRF%Channel
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                Error_Status )
          STOP
        END IF


        ! -----------------------
        ! Perform the convolution
        ! -----------------------

        IF ( SRF_Integration_Method == SUMMATION_METHOD ) THEN

          ! -- By simple summation
          SpcCoeff%Solar_Irradiance( l )  = SUM( SRF%Response * Irradiance ) * &
                                            Solar%Frequency_Interval / SRF_Integral

        ELSE

          ! -- By integration
          Error_Status = Simpsons_Integral( SRF%Frequency, &
                                            SRF%Response * Irradiance, &
                                            SpcCoeff%Solar_Irradiance( l ), &
                                            Order = CUBIC_ORDER )

          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error occurred convoling solar irradiance with SRF.', &
                                  Error_Status )
            STOP
          END IF

          SpcCoeff%Solar_Irradiance( l ) = SpcCoeff%Solar_Irradiance( l ) / SRF_Integral


        END IF


        ! ---------------------------------------
        ! Nullify the irradiance spectrum pointer
        ! ---------------------------------------

        NULLIFY( Irradiance )


        ! --------------------------
        ! Set the solar channel flag
        ! --------------------------

        IF ( SpcCoeff%Wavenumber( l ) > SOLAR_CUTOFF_WAVENUMBER ) THEN
          SpcCoeff%Is_Solar_Channel( l ) = 1
        ELSE
          SpcCoeff%Is_Solar_Channel( l ) = 0
        END IF
 

        WRITE( *, FMT = '( 2x, es13.6 )' ) SpcCoeff%Solar_Irradiance( l )



        !#----------------------------------------------------------------------#
        !#        -- DEALLOCATE THE RADIANCE/IRRADIANCE SPECTRUM ARRAY --       #
        !#----------------------------------------------------------------------#

        DEALLOCATE( Spectrum, STAT = Allocate_Status )

        IF ( Allocate_Status /= 0 ) THEN
          WRITE( Message, '( "Error deallocating spectrum array. STAT = ", i5 )' ) &
                          Allocate_Status
          CALL Display_Message( PROGRAM_NAME,    &
                                TRIM( Message ), &
                                FAILURE          )
          STOP
        END IF



        !#----------------------------------------------------------------------#
        !#            -- DESTROY THE CURRENT CHANNEL SRF STRUCTURE --           #
        !#----------------------------------------------------------------------#
 
        Error_Status = Destroy_SRF( SRF )

        IF ( Error_Status/= SUCCESS ) THEN
          WRITE( Message, '( "Error destroying SRF structure for channel ", i5 )' ) &
                          SpcCoeff%Sensor_Channel( l )
          CALL Display_Message( PROGRAM_NAME,    &
                                TRIM( Message ), &
                                FAILURE          )
          STOP
        END IF

      END DO Channel_Loop



      !#--------------------------------------------------------------------------#
      !#                     -- CLOSE THE ASCII OUTPUT FILE --                    #
      !#--------------------------------------------------------------------------#

      CLOSE( ASCII_fileID, IOSTAT = IO_Status )

      IF ( IO_Status /= 0 ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error closing ASCII log file '//&
                              TRIM( ASCII_Filename ), &
                              WARNING )
      END IF



      !#------------------------------------------------------------------------#
      !#                    -- WRITE THE SpcCoeff DATA FILE --                  #
      !#------------------------------------------------------------------------#

      SpcCoeff_Filename = TRIM( SensorInfo%File_Prefix )//'.SpcCoeff.nc'

      Error_Status = Write_SpcCoeff_netCDF( TRIM( SpcCoeff_Filename ), &
                                            SpcCoeff, &
                                            Title = 'Spectral coefficients for '//&
                                                    TRIM( Platform_Name )//' '//&
                                                    TRIM( Sensor_Name )//&
                                                    ' derived from SRF data file '//&
                                                    TRIM( SRF_Filename )//&
                                                    ' and solar data file '//&
                                                    TRIM( Solar_Filename ), &
                                            History = PROGRAM_RCS_ID//&
                                                      '; '//TRIM( SRF_History )//&
                                                      '; '//TRIM( Solar_History ), &
                                            Sensor_Name   = TRIM( Sensor_Name ), &
                                            Platform_Name = TRIM( Platform_Name ), &
                                            Comment       = TRIM( Comment ) )


      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error writing netCDF SpcCoeff data file '//&
                              TRIM( SpcCoeff_Filename ), &
                              FAILURE )
        STOP
      END IF



      !#------------------------------------------------------------------------#
      !#        -- DESTROY THE CURRENT SENSOR SpcCoeff DATA STRUCTURE --        #
      !#------------------------------------------------------------------------#

      Error_Status = Destroy_SpcCoeff( SpcCoeff )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error destroying SpcCoeff data structure for '//&
                              TRIM( SRF_Filename )//' processing.', &
                              WARNING )
      END IF


    END IF Infrared_Sensors



    !#--------------------------------------------------------------------------#
    !#              -- DESTROY THE CURRENT SensorInfo STRUCTURE --              #
    !#--------------------------------------------------------------------------#

    Error_Status = Destroy_SensorInfo( SensorInfo )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error destroying SensorInfo data structure.', &
                            Error_status )
      STOP
    END IF


  END DO n_Sensor_loop



  !#----------------------------------------------------------------------------#
  !#                  -- DESTROY THE SOLAR DATA ARRAY/STRUCTURE --              #
  !#----------------------------------------------------------------------------#

  ! -- Destroy the interpoaltion derivative array
  DEALLOCATE( Solar_Derivative, &
              STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error deallocating solar array for interpolation. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME,    &
                          TRIM( Message ), &
                          WARNING          )
  END IF

  ! -- Destroy the solar data structure
  Error_Status = Destroy_Solar( Solar )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying Solar data structure.', &
                          WARNING )
  END IF



  !#--------------------------------------------------------------------------#
  !#                  -- DESTROY THE SensorInfo LINKED LIST --                #
  !#--------------------------------------------------------------------------#

  Error_Status = Destroy_SensorInfo_List( SensorInfo_List )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying SensorInfo linked list.', &
                          WARNING )
  END IF





CONTAINS





!--------------------------------------------------------------------------------
!
! NAME:
!       Least_Squares_Linear_Fit
!
! PURPOSE:
!       Function to perform a least squares linear fit on the input 
!       polychromatic and monochromatic temperature data.
!
! CATEGORY:
!       Instrument_Information : Create_IR_SpcCoeff_Sensor
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Least_Squares_Linear_Fit( x, y,        &  ! Input
!                                                a, b,        &  ! Output
!                                                yFit = yFit, &  ! Optional output
!                                                SSE  = SSE,  &  ! Optional output
!                                                MSE  = MSE,  &  ! Optional output
!                                                Message_Log = Message_Log ) !  Error messaging
!
! INPUT ARGUMENTS:
!       x:               Input ordinate data on which to perform the fit
!                          y = a + bx
!                        Corresponds to the true temperature.
!                        UNITS:      Kelvin (K)
!                        TYPE:       REAL( fp_kind )
!                        DIMENSION:  Rank-1
!                        ATTRIBUTES: INTENT( IN )
!
!       y:               Input coordinate data on which to perform the fit
!                          y = a + bx
!                        Corresponds to the effective temperature due to
!                        polychromaticity.
!                        UNITS:      Kelvin (K)
!                        TYPE:       REAL( fp_kind )
!                        DIMENSION:  Rank-1 (Same size as x)
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       a:               Offset coefficient that satisfies the fit criteria
!                        for the relationship
!                          y = a + bx
!                        UNITS:      Kelvin (K)
!                        TYPE:       REAL( fp_kind )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT )
!
!       b:               Slope coefficient that satisfies the fit criteria
!                        for the relationship
!                          y = a + bx
!                        UNITS:      Kelvin/Kelvin (K/K)
!                        TYPE:       REAL( fp_kind )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       yFit:            Predicted coordinate (effective temperature) data,
!                          yFit = a + bx
!                        UNITS:      Kelvin (K)
!                        TYPE:       REAL( fp_kind )
!                        DIMENSION:  Rank-1 (Same size as y)
!                        ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       SSE:             The residual sum of the squares of the fit to the
!                        input data,
!                                 __  N
!                                \                    2
!                          SSE =  > ( Y(i) - YFit(i) )
!                                /__
!                                    i=1
!
!                        UNITS:      Kelvin^2 (K^2)
!                        TYPE:       REAL( fp_kind )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       MSE:             The residual mean square of the fit to the
!                        input data,
!                                
!                                 SSE
!                          MSE = -----
!                                 N-2
!
!                        where N == number of input data points
!
!                        UNITS:      Kelvin^2 (K^2)
!                        TYPE:       REAL( fp_kind )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       The return value is an integer defining the error status. The
!       error status codes are defined in the ERROR_HANDLER module.
!
!       If result = SUCCESS the regression fit was successful.
!                 = FAILURE an error occurred.
!
! CALLS:
!       Display_Message:      Subroutine to output Messages
!                             SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 25-Nov-2002
!                       paul.vandelst@ssec.wisc.edu
!
!--------------------------------------------------------------------------------

  FUNCTION Least_Squares_Linear_Fit( x, y,         &  ! Input
                                     a, b,         &  ! Output
                                     yFit,         &  ! Optional output
                                     SSE,          &  ! Optional output
                                     MSE,          &  ! Optional output
                                     Message_Log ) &  ! Error messaging
                                   RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( fp_kind ),           DIMENSION( : ), INTENT( IN )  :: x
    REAL( fp_kind ),           DIMENSION( : ), INTENT( IN )  :: y

    ! -- Output
    REAL( fp_kind ),                           INTENT( OUT ) :: a
    REAL( fp_kind ),                           INTENT( OUT ) :: b

    ! -- Optional output
    REAL( fp_kind ), OPTIONAL, DIMENSION( : ), INTENT( OUT ) :: yFit
    REAL( fp_kind ), OPTIONAL,                 INTENT( OUT ) :: SSE
    REAL( fp_kind ), OPTIONAL,                 INTENT( OUT ) :: MSE

    ! -- Error messaging
    CHARACTER( * ),  OPTIONAL,                 INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Least_Squares_Linear_Fit'

    REAL( fp_kind ), PARAMETER :: TOLERANCE = EPSILON( 1.0_fp_kind )


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: n
    REAL( fp_kind ) :: xAverage
    REAL( fp_kind ) :: yAverage
    REAL( fp_kind ) :: sum_dx2

    REAL( fp_kind ), DIMENSION( SIZE( y ) ) :: yCalculated
    REAL( fp_kind ) :: Residual_Sum_of_Squares
    REAL( fp_kind ) :: Residual_Mean_Square   



    !#--------------------------------------------------------------------------#
    !#                   -- ASSIGN SUCCESSFUL RETURN STATUS --                  #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                             -- CHECK INPUT --                            #
    !#--------------------------------------------------------------------------#

    n = SIZE( x )

    IF ( n < 3 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input data must be at least 3 points.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( SIZE( y ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Sizes of input X,Y arguments are inconsistent', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( yFit ) ) THEN
      IF ( SIZE( yFit ) /= n ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Sizes of output YFIT argument is inconsistent', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- CALCULATE AVERAGES --                        #
    !#                                                                          #
    !#         __  N                 __  N                                      #
    !#    _   \                 _   \                                           #
    !#    X =  > Xi      and    Y =  > Yi                                       #
    !#        /__                   /__                                         #
    !#            i=1                   i=1                                     #
    !#       ---------             ---------                                    #
    !#           N                     N                                        #
    !#--------------------------------------------------------------------------#

    xAverage = SUM( x ) / REAL( n, fp_kind )
    yAverage = SUM( y ) / REAL( n, fp_kind )



    !#--------------------------------------------------------------------------#
    !#   -- CALCULATE THE SUMS OF THE SQUARE OF THE MEAN DIFFERENCE FOR X --    #
    !#                                                                          #
    !#               __  N                                                      #
    !#              \         _  2                                              #
    !#    sum_dX2 =  > ( Xi - X )                                               #
    !#              /__                                                         #
    !#                  i=1                                                     #
    !#--------------------------------------------------------------------------#

    sum_dx2 = SUM( ( x - xAverage )**2 )

    IF ( sum_dx2 < TOLERANCE ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Sum of the squares of mean difference for X is zero.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                       -- CALCULATE COEFFICIENTS --                       #
    !#                                                                          #
    !#           __  N                                                          #
    !#          \         _            _                                        #
    !#           > ( Xi - X ) * ( Yi - Y )                                      #
    !#          /__                                                             #
    !#              i=1                                                         #
    !#    b = ------------------------------                                    #
    !#                __  N                                                     #
    !#               \         _  2                                             #
    !#                > ( Xi - X )                                              #
    !#               /__                                                        #
    !#                   i=1                                                    #
    !#                                                                          #
    !#                                                                          #
    !#        _         _                                                       #
    !#    a = Y - ( b * X )                                                     #
    !#                                                                          #
    !#--------------------------------------------------------------------------#

    b = SUM( ( x - xAverage ) * ( y - yAverage ) ) / &
    !   --------------------------------------------
                         sum_dx2


    a = yAverage - ( b * xAverage )


    ! ---------------------------------
    ! Calculate the regression Y values
    ! ---------------------------------

    yCalculated = a + ( b * x )

    Residual_Sum_of_Squares = SUM( ( y - yCalculated )**2 )
    Residual_Mean_Square    = Residual_Sum_of_Squares / &
    !                         -----------------------
                                REAL( n-2, fp_kind )



    !#--------------------------------------------------------------------------#
    !#                      -- ASSIGN OPTIONAL ARGUMENTS --                     #
    !#--------------------------------------------------------------------------#

    ! ---------------
    ! Fitted Y values
    ! ---------------

    IF ( PRESENT( yFit ) ) THEN
      yFit = yCalculated
    END IF


    ! -----------------------
    ! Residual sum of squares
    ! -----------------------

    IF ( PRESENT( SSE ) ) THEN
      SSE = Residual_Sum_of_Squares
    END IF


    ! --------------------
    ! Residual mean square
    ! --------------------

    IF ( PRESENT( MSE ) ) THEN
      MSE = Residual_Mean_Square
    END IF

  END FUNCTION Least_Squares_Linear_Fit





!--------------------------------------------------------------------------------
!
! NAME:
!       Compute_Frequency_Index
!
! PURPOSE:
!       Function to determine the frequency array element index for
!       an input frequency value.
!
! CATEGORY:
!       Instrument_Information : Create_IR_SpcCoeff_Sensor
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Frequency_Index = Compute_Frequency_Index( Begin_Frequency,    &  ! Input
!                                                  Frequency_Interval, &  ! Input
!                                                  Frequency           )  ! Input
!
! INPUT ARGUMENTS:
!       Begin_Frequency:     Begin frequency corresponding to the first
!                            value in the array.
!                            UNITS:      Inverse centimetres (cm^-1)
!                            TYPE:       REAL( fp_kind )
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
!       Frequency_Interval:  Frequency interval between adjacent frequency
!                            array values.
!                            UNITS:      Inverse centimetres (cm^-1)
!                            TYPE:       REAL( fp_kind )
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
!       Frequency:           Frequency value for which the index is required.
!                            UNITS:      Inverse centimetres (cm^-1)
!                            TYPE:       REAL( fp_kind )
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Frequency_Index:     The return value is an integer corresponding to
!                            the location of the input frequency value in an
!                            array defined by the begin frequency and
!                            frequency interval.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 25-Nov-2002
!                       paul.vandelst@ssec.wisc.edu
!
!--------------------------------------------------------------------------------

  FUNCTION Compute_Frequency_Index( Begin_Frequency, &
                                    Frequency_Interval, &
                                    Frequency ) &
                                  RESULT ( Frequency_Index )


    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN ) :: Begin_Frequency
    REAL( fp_kind ), INTENT( IN ) :: Frequency_Interval
    REAL( fp_kind ), INTENT( IN ) :: Frequency


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Frequency_Index


    ! ----------------
    ! Local parameters
    ! ----------------

    REAL( fp_kind ), PARAMETER :: ONEpointFIVE = 1.5_fp_kind 



    !#--------------------------------------------------------------------------#
    !#                       -- COMPUTE THE INDEX VALUE --                      #
    !#--------------------------------------------------------------------------#

    Frequency_Index = INT( ( ( Frequency - Begin_Frequency ) / Frequency_Interval ) + ONEpointFIVE )

  END FUNCTION Compute_Frequency_Index

END PROGRAM Create_IR_SpcCoeff_Sensor


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Create_IR_SpcCoeff_Sensor.f90,v 6.4 2006/05/02 16:58:03 dgroff Exp $
!
! $Date: 2006/05/02 16:58:03 $
!
! $Revision: 6.4 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Create_IR_SpcCoeff_Sensor.f90,v $
! Revision 6.4  2006/05/02 16:58:03  dgroff
! *** empty log message ***
!
! Revision 6.3  2005/08/11 17:26:54  paulv
! - Replaced Radiance_Spectrum allocatable array with generic Spectrum array.
!   Pointers "Radiance" and "Irradiance" are now used to reference the
!   "Spectrum" array for the polychromatic correction and solar irradiance
!   convolution.
! - Solar spectrum is now interpolated to the SRF spacing. The interpolation
!   used is a spline whihc required the definition of an an allocatable array
!   to hold the derivatives computed during the spline initialisation.
! - Interpolate module is now USEd to gain access to the spline routines.
!
! Revision 6.2  2005/08/11 14:34:16  paulv
! - Solar calculation turned back on.
! - Added 281 and 324 AIRS subset lists to invalid qualifier to prevent
!   replication of module SpcCoeff calculations.
!
! Revision 6.1  2005/07/05 22:47:46  paulv
! - Fixed bug in SpcCoeff write function call.
!
! Revision 6.0  2005/07/05 22:46:45  paulv
! - Updated for Sensor SpcCoeff Release6 software.
! - Solar influence turned off for ALL channels.
!
! Revision 5.0  2005/04/01 18:23:58  paulv
! - Dummy checkin to set version to 5.0.
!
! Revision 1.22  2005/04/01 18:21:24  paulv
! - Updated for SpcCoeff Release5.
!
! Revision 1.21  2004/09/02 16:50:10  paulv
! - Upgraded to Fortran95.
! - Removed all structure initialisation calls.
! - Replaced SensorInfo linked list initialisation with call to
!   New_SensorInfo_List() function.
!
! Revision 1.20  2004/08/02 16:34:51  paulv
! - Changed SpcCoeff initialization call from Initialize_SpcCoeff() to
!   Init_SpcCoeff().
!
! Revision 1.19  2004/06/25 20:29:37  paulv
! - Added CMB reference to comment global attribute.
!
! Revision 1.18  2004/06/25 20:15:02  paulv
! - Updated cosmic background temperature.
! - Updated polarization assignment.
!
! Revision 1.17  2004/06/25 17:13:03  paulv
! - Removed unused variables from type declarations.
! - Cosmetic differences.
!
! Revision 1.16  2004/05/17 18:53:59  paulv
! - Added assignment of Sensor_Descriptor component of the SpcCoeff structure.
!
! Revision 1.15  2003/11/18 14:17:38  paulv
! - Updated documentation header delimiters.
!
! Revision 1.14  2003/11/17 17:56:26  paulv
! - Updated all header documentation.
! - Added parameters for defining the integration method. Previously explicit
!   values were used in the code which led to some confusion as to what
!   constituted a valid value (e.g. 0 and 1, or 1 and 2 type of thing)
! - Updated code to use the new Solar, SRF, SensorInfo, and Planck_Functions
!   modules.
! - Removed interpolation method specification.
! - Added capability for user to input an SpcCoeff file version numbers
!   different from the default.
! - Altered processing logic to operate only on IR sensors whose SRF data files
!   were present. Previosuly, all IR sensors were processed even if there was
!   no SRF data file...which caused an error to be thrown.
! - Removed the computation of the SRF frequency grid. This is now done in
!   the updated SRF modules and stored in the SRF structure.
! - Added optional argument ULP to Compare_Float() calls when comparing the
!   solar spectrum and SRF limit frequency values. For come channels the
!   differences were the same as the numerical precision and the ULP argument
!   allows for scaling of the tolerance comparison value.
! - Added documentation headers for internal subprograms.
!
! Revision 1.13  2003/06/18 19:54:56  paulv
! - Corrected bug in logic to skip the AIRS subset data set.
!
! Revision 1.12  2003/06/18 17:17:44  paulv
! - Altered code to reflect changes in the SensorInfo structure. The SensorInfo
!   structure now contains a microwave sensor flag. This is now used to process
!   the sensors so an "infrared-only" SensorInfo file is no longer required;
!   any MW sensors are simply skipped. The airsSUBSET_aqua case is also skipped
!   since those data are extracted from the individual AIRS module files.
!
! Revision 1.11  2003/02/12 15:33:21  paulv
! - Cosmetic change.
!
! Revision 1.10  2003/02/11 22:59:28  paulv
! - Using new Solar and SpcCoeff netCDF interface routines.
!
! Revision 1.9  2003/01/06 19:07:18  paulv
! - Moved from SRF to Instrument_Information category and renamed.
!
! Revision 1.8  2002/12/23 23:55:59  paulv
! - SpcCoeff data now output in netCDF format.
! - SensorInfo file now used to control processing. All in/output filenames
!   are constructed from SensorInfo file prefix.
!
! Revision 1.7  2002/11/25 19:46:02  paulv
! - Added capability to import central frequencies and Planck coefficients
!   from a separate SpcCoeff data file rather than computing them directly
!   from the SRF. For testing purposes only - central frequencies and
!   Planck coefficients should be determined from the SRF.
! - Added extra check in the overlay of the SRF onto the solar irradiance
!   spectrum. The begin and end frequencies are checked to ensure they are
!   the same.
! - Updated documentation.
! - Documented the linear least squares fit function.
!
! Revision 1.6  2002/11/22 18:10:58  paulv
! - Added solar spectral data calculations.
!
! Revision 1.5  2002/11/13 20:30:44  paulv
! - Added the SpcCoeff output file release and version number.
! - Added call to the Solar structure destruction function.
!
! Revision 1.4  2002/11/08 21:09:17  paulv
! - Modified to read netCDF solar data file. No convolution yet.
! - Spectral coefficient data now stored and written via SpcCoeff structure.
!
! Revision 1.3  2002/11/08 04:32:59  paulv
! - Completed code changes. Undocumented.
!
! Revision 1.2  2002/11/06 19:30:18  paulv
! - Updating code to use netCDF SRF files. Incomplete.
!
! Revision 1.1  2002/01/18 21:23:45  paulv
! Initial checkin.
!
!
!
