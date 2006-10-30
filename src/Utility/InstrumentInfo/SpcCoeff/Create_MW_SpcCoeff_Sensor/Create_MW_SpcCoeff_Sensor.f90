!------------------------------------------------------------------------------
!P+
! NAME:
!       Create_MW_SpcCoeff_Sensor
!
! PURPOSE:
!       Program to create the SENSOR microwave spectral coefficient (SpcCoeff)
!       data files.
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
!       Spectral_Units_Conversion:  Module containing functions to convert
!                                   between various spectral units.
!                                   USEs: TYPE_KINDS module
!                                         FUNDAMENTAL_CONSTANTS module
!
!       MW_SensorData_Define:       Module defining the MW_SensorData data structure
!                                   and containing routines to manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
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
!                                         SENSORINFO_DEFINE module
!
!       SpcCoeff_Define:            Module defining the SpcCoeff data structure
!                                   and containing routines to manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         FILE_UTILITY module
!                                         ERROR_HANDLER module
!
!       SpcCoeff_netCDF_IO:         Module continaing routines to read and write
!                                   netCDF format SpcCoeff files.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         SPCCOEFF_DEFINE module
!                                         NETCDF module
!                                         NETCDF_UTILITY module
!
! CONTAINS:
!       None.
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
!
!       Output:
!         - netCDF SpcCoeff data file containing all the required data.
!
! SIDE EFFECTS:
!       If any of the output files exist, they are overwritten.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 06-Jan-2003
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2003 Paul van Delst
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

PROGRAM Create_MW_SpcCoeff_Sensor


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler

  USE Fundamental_Constants, ONLY: C_1, C_2
  USE Planck_Functions

  USE Spectral_Units_Conversion

  USE MW_SensorData_Define

  USE SensorInfo_Define, SENSORINFO_MICROWAVE => MICROWAVE_SENSOR_TYPE
  USE SensorInfo_LinkedList
  USE SensorInfo_IO

  USE SpcCoeff_Define, SPCCOEFF_MICROWAVE => MICROWAVE_SENSOR
  USE SpcCoeff_netCDF_IO


  

  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Create_MW_SpcCoeff_Sensor'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Create_MW_SpcCoeff_Sensor.f90,v 6.1 2006/05/02 16:58:03 dgroff Exp $'
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

  ! -- The default cosmic background temperature
  REAL( fp_kind ), PARAMETER :: COSMIC_BACKGROUND_TEMPERATURE = 2.7253_fp_kind
  CHARACTER( * ),  PARAMETER :: CMB_REFERENCE = &
    'CMB value from J.C. Mather, et. al., 1999, "Calibrator Design for the '//&
    'COBE Far-Infrared Absolute Spectrophotometer (FIRAS)," Astrophysical '//&
    'Journal, vol 512, pp 511-520'

  ! -- Numerical constants
  REAL( fp_kind ), PARAMETER :: ZERO = 0.0_fp_kind


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: IO_Status
 
  CHARACTER( 256 ) :: SensorInfo_Filename
  CHARACTER( 256 ) :: SpcCoeff_Filename
  CHARACTER( 256 ) :: ASCII_Filename
  INTEGER          :: ASCII_fileID

  INTEGER :: i, l, n, n_Sensors
  INTEGER :: SpcCoeff_File_Version

  CHARACTER( 256 ) :: MW_SensorData_RCS_ID

  TYPE( SensorInfo_type )      :: SensorInfo
  TYPE( SensorInfo_List_type ) :: SensorInfo_List
  TYPE( MW_SensorData_type )   :: MW_SensorData
  TYPE( SpcCoeff_Sensor_type ) :: SpcCoeff

  REAL( fp_kind ) :: Integrated_MW_Response
  REAL( fp_kind ) :: Convolved_Radiance

  REAL( fp_kind ), DIMENSION( N_TEMPERATURES ) :: x_Temperature
  REAL( fp_kind ), DIMENSION( N_TEMPERATURES ) :: y_Effective_Temperature
  REAL( fp_kind ), DIMENSION( N_TEMPERATURES ) :: yFit_Effective_Temperature
  REAL( fp_kind ) :: var_Effective_Temperature
  REAL( fp_kind ), DIMENSION( : ), ALLOCATABLE :: Wavenumber
  REAL( fp_kind ), DIMENSION( : ), ALLOCATABLE :: Radiance




  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a)' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to create the SENSOR microwave spectral ")' )
  WRITE( *, '( 5x, "   coefficient (SpcCoeff) data files. ")' )
  WRITE( *, '(/5x, " $Revision: 6.1 $")' )
  WRITE( *, '( 5x, a)' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                           -- GET USER INPUTS --                            #
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



    !#--------------------------------------------------------------------------#
    !#                 -- OPERATE ONLY ON MICROWAVE SENSORS --                  #
    !#--------------------------------------------------------------------------#

    Microwave_Sensors: IF ( SensorInfo%Microwave_Flag == SENSORINFO_MICROWAVE ) THEN

   
      ! ----------------------
      ! Output an info message
      ! ----------------------

      WRITE( *, '( //5x, "Creating the SpcCoeff data file for ", a, 1x, a )' ) &
                TRIM( SensorInfo%Satellite_Name ), &
                TRIM( SensorInfo%Sensor_Name )



 
      !#------------------------------------------------------------------------#
      !#              -- LOAD THE CURRENT MICROWAVE SENSOR DATA --              #
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


      ! ------------------------
      ! Double check the WMO IDs
      ! ------------------------

      IF ( MW_SensorData%WMO_Satellite_ID /= SensorInfo%WMO_Satellite_ID .OR. &
           MW_SensorData%WMO_Sensor_ID    /= SensorInfo%WMO_Sensor_ID         ) THEN

        IF ( Error_Status /= SUCCESS ) THEN
          CALL Display_Message( PROGRAM_NAME, &
                                'MW_SensorData and SensorInfo WMO IDs are different for '//&
                                TRIM( SensorInfo%Satellite_Name )//' '//&
                                TRIM( SensorInfo%Sensor_Name ), &
                                Error_Status )
          STOP
        END IF

      END IF



      !#------------------------------------------------------------------------#
      !#                  -- ALLOCATE THE SpcCoeff STRUCTURE --                 #
      !#------------------------------------------------------------------------#

      Error_Status = Allocate_SpcCoeff( MW_SensorData%n_Channels, &
                                        SpcCoeff )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error allocating SpcCoeff data structure.', &
                              Error_Status )
        STOP
      END IF



      ! ------------------------------------------------
      ! Set the version value, and the number of sensors
      ! ------------------------------------------------

      SpcCoeff%Version   = SpcCoeff_File_Version
      SpcCoeff%n_Sensors = 1

 
      ! ---------------------------
      ! Assign the sensor ID values
      ! ---------------------------

      SpcCoeff%Sensor_Descriptor = TRIM( SensorInfo%File_Prefix )
      SpcCoeff%Sensor_Type       = SPCCOEFF_MICROWAVE
      SpcCoeff%NCEP_Sensor_ID    = MW_SensorData%NCEP_Sensor_ID
      SpcCoeff%WMO_Satellite_ID  = MW_SensorData%WMO_Satellite_ID
      SpcCoeff%WMO_Sensor_ID     = MW_SensorData%WMO_Sensor_ID
      SpcCoeff%Sensor_Channel    = MW_SensorData%Sensor_Channel


      ! ------------------------------
      ! Assign the central frequencies
      ! ------------------------------

      SpcCoeff%Frequency = MW_SensorData%Central_Frequency


      ! ----------------------------
      ! Assign the polarisation type
      ! ----------------------------

      SpcCoeff%Polarization = MW_SensorData%Polarization


      ! -----------------------------------------------------
      ! Set the solar channel flag and solar irradiance array
      ! -----------------------------------------------------

      SpcCoeff%Is_Solar_Channel = 0
      SpcCoeff%Solar_Irradiance = ZERO



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



      !#------------------------------------------------------------------------#
      !#                         -- LOOP OVER CHANNELS --                       #
      !#------------------------------------------------------------------------#

      WRITE( *, '( /, "    CH        F0             V0            ", &
                     &"FK1            FK2", 25x, "POLARIZATION", 26x, "BC1",12x,"BC2",12x,"CBR" )' )


      Channel_Loop: DO l = 1, SpcCoeff%n_Channels

        WRITE( *, FMT     = '( 2x, i4 )', &
                  ADVANCE = 'NO'          ) SpcCoeff%Sensor_Channel( l )



        !#----------------------------------------------------------------------#
        !#                 -- COMPUTE THE PLANCK COEFFICIENTS --                #
        !#----------------------------------------------------------------------#

        ! -------------------------------------------------------------
        ! Convert the central frequency to units of inverse centimetres
        ! -------------------------------------------------------------

        SpcCoeff%Wavenumber( l ) = GHz_to_inverse_cm( SpcCoeff%Frequency( l ) )


        ! -------------------------------
        ! Compute the Planck coefficients
        ! -------------------------------

        SpcCoeff%Planck_C1( l ) = C_1_SCALE_FACTOR * C_1 * ( SpcCoeff%Wavenumber( l )**3 )
        SpcCoeff%Planck_C2( l ) = C_2_SCALE_FACTOR * C_2 *   SpcCoeff%Wavenumber( l )

        WRITE( *, FMT     = '( 4( 2x, es13.6 ), 2x, a )', &
                  ADVANCE = 'NO' ) SpcCoeff%Frequency( l ), &
                                   SpcCoeff%Wavenumber( l ), &
                                   SpcCoeff%Planck_C1( l ), &
                                   SpcCoeff%Planck_C2( l ), &
                                   POLARIZATION_TYPE_NAME(MW_SensorData%Polarization(l) )



        !#----------------------------------------------------------------------#
        !#        -- COMPUTE THE POLYCHROMATIC CORRECTION COEFFICIENTS --       #
        !#----------------------------------------------------------------------#

        ! ---------------------------------------
        ! Allocate the wavenumber/radiance arrays
        ! ---------------------------------------

        ALLOCATE( Wavenumber( MW_SensorData%n_Frequencies ), &
                  Radiance( MW_SensorData%n_Frequencies ), &
                  STAT = Allocate_Status )

        IF ( Allocate_Status /= 0 ) THEN
          WRITE( Message, '( "Error allocating wavenumber and radiance arrays. STAT = ", i5 )' ) &
                          Allocate_Status
          CALL Display_Message( PROGRAM_NAME,    &
                                TRIM( Message ), &
                                FAILURE          )
          STOP
        END IF


        ! -----------------------------------------------------
        ! Convert the MW frequency grid from GHz to cm^-1 units
        ! -----------------------------------------------------

        Wavenumber = GHz_to_inverse_cm( MW_SensorData%Frequency(:,l) )


        ! -----------------------
        ! Compute the MW response
        ! -----------------------

        Integrated_MW_Response = SUM( MW_SensorData%Response(:,l) ) * MW_SensorData%Delta_Frequency(l)


        ! -----------------------------------------
        ! Generate the "polychromatic" temperatures
        ! -----------------------------------------

        Temperature_Loop: DO i = 1, N_TEMPERATURES

          ! -- Calculate monochromatic radiances
          Error_Status = Planck_Radiance( Wavenumber, &
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

          ! -- Convolve the radiance spectrum with the MW response (unity)
          Convolved_Radiance = SUM( Radiance ) * MW_SensorData%Delta_Frequency(l) / &
          !                    --------------------------------------------------
                                             Integrated_MW_Response

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

        ! -- Output fit information to file
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


        ! -----------------
        ! Deallocate arrays
        ! -----------------

        DEALLOCATE( Wavenumber, Radiance, STAT = Allocate_Status )

        IF ( Allocate_Status /= 0 ) THEN
          WRITE( Message, '( "Error deallocating wavenumber and radiance arrays. STAT = ", i5 )' ) &
                          Allocate_Status
          CALL Display_Message( PROGRAM_NAME,    &
                                TRIM( Message ), &
                                FAILURE          )
          STOP
        END IF



        !#----------------------------------------------------------------------#
        !#              -- COMPUTE THE COSMIC BACKGROUND RADIANCE --            #
        !#----------------------------------------------------------------------#

        Error_Status = Planck_Radiance( SpcCoeff%Wavenumber( l ), &
                                        COSMIC_BACKGROUND_TEMPERATURE, &
                                        SpcCoeff%Cosmic_Background_Radiance( l ) )

        IF ( Error_Status /= SUCCESS ) THEN
        WRITE( *, * )
          WRITE( Message, '( "Error computing cosmic background radiance for ", a, 1x, a, &
                            &" channel ", i2, "." )' ) &
                          TRIM( SensorInfo%Satellite_Name ), &
                          TRIM( SensorInfo%Sensor_Name ), &
                          SpcCoeff%Sensor_Channel( l )
          CALL Display_Message( PROGRAM_NAME,    &
                                TRIM( Message ), &
                                Error_Status     )
          STOP
        END IF

        WRITE( *, FMT = '( 2x, es13.6 )' ) SpcCoeff%Cosmic_Background_Radiance( l )

      END DO Channel_Loop



      !#------------------------------------------------------------------------#
      !#                    -- WRITE THE SpcCoeff DATA FILE --                  #
      !#------------------------------------------------------------------------#

      SpcCoeff_Filename = TRIM( SensorInfo%File_Prefix )//'.SpcCoeff.nc'

      Error_Status = Write_SpcCoeff_netCDF( TRIM( SpcCoeff_Filename ), &
                                            SpcCoeff, &
                                            Title = 'Spectral coefficients for '//&
                                                    TRIM( SensorInfo%Satellite_Name )//' '//&
                                                    TRIM( SensorInfo%Sensor_Name )//'.', &
                                            History = PROGRAM_RCS_ID//&
                                                      '; '//TRIM( MW_SensorData_RCS_ID ), &
                                            Sensor_Name = TRIM( SensorInfo%Sensor_Name ), &
                                            Platform_Name = TRIM( SensorInfo%Satellite_Name ), &
                                            Comment = 'Boxcar spectral response assumed; '//&
                                                      CMB_REFERENCE )


      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error writing SpcCoeff data structure to '//&
                              TRIM( SpcCoeff_Filename ), &
                              FAILURE )
        STOP
      END IF


      !#------------------------------------------------------------------------#
      !#            -- DESTROY THE CURRENT SENSOR DATA STRUCTURES --            #
      !#------------------------------------------------------------------------#

      ! ----------------------
      ! The SpcCoeff structure
      ! ----------------------

      Error_Status = Destroy_SpcCoeff( SpcCoeff )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error destroying SpcCoeff structure for '//&
                              TRIM( SensorInfo%Satellite_Name )//' '//&
                              TRIM( SensorInfo%Sensor_Name )//'.', &
                              Error_Status )
        STOP
      END IF


      ! ---------------------------
      ! The MW_SensorData structure
      ! ---------------------------

      Error_Status = Destroy_MW_SensorData( MW_SensorData )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error destroying MW_SensorData structure for '//&
                              TRIM( SensorInfo%Satellite_Name )//' '//&
                              TRIM( SensorInfo%Sensor_Name )//'.', &
                              Error_Status )
        STOP
      END IF

    END IF Microwave_Sensors



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
!       Instrument_Information : Create_MW_SpcCoeff_Sensor
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

END PROGRAM Create_MW_SpcCoeff_Sensor


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Create_MW_SpcCoeff_Sensor.f90,v 6.1 2006/05/02 16:58:03 dgroff Exp $
!
! $Date: 2006/05/02 16:58:03 $
!
! $Revision: 6.1 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Create_MW_SpcCoeff_Sensor.f90,v $
! Revision 6.1  2006/05/02 16:58:03  dgroff
! *** empty log message ***
!
! Revision 6.0  2005/07/05 22:28:31  paulv
! - Updated for Sensor SpcCoeff Release6 software.
!
! Revision 5.0  2005/04/01 17:51:50  paulv
! - Updated for SpcCoeff Release5.
!
! Revision 2.0  2004/09/17 16:40:46  paulv
! - New version.
! - Polychromatic correction due to channel bandwidth now computed.
!
! Revision 1.13  2004/09/02 16:55:42  paulv
! - Upgraded to Fortran95.
! - Removed all structure and list initialisation calls.
!
! Revision 1.12  2004/08/02 16:35:58  paulv
! - Changed SpcCoeff initialization call from Initialize_SpcCoeff() to
!   Init_SpcCoeff().
!
! Revision 1.11  2004/06/25 20:53:29  paulv
! - Updated CMB temperature to the value from the specified reference. D'oh.
!
! Revision 1.10  2004/06/25 20:51:12  paulv
! - Added CMB reference to comment global attribute.
! - Updated stdout output for Stokes vector polarization description.
!
! Revision 1.9  2004/05/17 18:56:21  paulv
! - Added assignment of Sensor_Descriptor component of the SpcCoeff structure.
!
! Revision 1.8  2003/11/18 14:18:26  paulv
! - Updated documentation header delimiters.
!
! Revision 1.7  2003/11/17 22:18:58  paulv
! - Updated code to use new Planck_Function module.
! - Added capability for user to input an output file version number different
!   from the default value.
!
! Revision 1.6  2003/06/16 19:14:07  paulv
! - Altered code to reflect changes in the SensorInfo structure. The SensorInfo
!   structure now contains a microwave sensor flag. This is now used to process
!   microwave sensors so a "microwave-only" SensorInfo file is no longer required;
!   any IR sensors are simply skipped.
!
! Revision 1.5  2003/06/09 15:06:27  paulv
! - Removed No_ChannelInfo optional arguments from calls to SensorInfo routines.
!   Ther is no longer a ChannelInfo read switch - all data is now read in.
!
! Revision 1.4  2003/05/23 19:38:10  paulv
! - Updated code to use new SensorInfo_LinkedList module for reading the
!   SensorInfo file.
!
! Revision 1.3  2003/02/12 17:07:40  paulv
! - Updated to use new SpcCoeff and SensorInfo I/O module routines.
!
! Revision 1.2  2003/01/07 16:45:52  paulv
! - Added to comment field in netCDF file creation.
!
! Revision 1.1  2003/01/06 18:23:35  paulv
! Initial checkin.
!
!
!
!
