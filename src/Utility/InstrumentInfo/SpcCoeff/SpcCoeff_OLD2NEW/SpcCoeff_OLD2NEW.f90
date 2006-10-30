!------------------------------------------------------------------------------
!P+
! NAME:
!       SpcCoeff_OLD2NEW
!
! PURPOSE:
!       Program to convert netCDF format SpcCoeff files from an OLD release 
!       format to a NEW one.
!
! CATEGORY:
!       Instrument Information : SpcCoeff
!
! LANGUAGE:
!       Fortran-90
!
! MODULES:
!       Type_Kinds:                 Module containing definitions for kinds
!                                   of variable types.
!
!       Message_Handler:            Module to define simple error codes and
!                                   handle error conditions
!                                   USEs: FILE_UTILITY module
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
!       SpcCoeff_Define:            Module defining the SpcCoeff data
!                                   structure and containing routines to
!                                   manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!       SpcCoeff_netCDF_IO:         Module containing routines to read and
!                                   write netCDF format SpcCoeff files.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         SPCCOEFF_DEFINE module
!                                         NETCDF module
!                                         NETCDF_UTILITY module
!
!       SpcCoeff_netCDF_IO_old:     Module containing routines to read and
!                                   write the old release format netCDF
!                                   SpcCoeff files.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         SPCCOEFF_DEFINE_OLD module
!                                         NETCDF module
!                                         NETCDF_UTILITY module
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
!         - ASCII SensorInfo data file
!         - Old format netCDF SpcCoeff data file
!
!       Output:
!         - New format netCDF SpcCoeff file.
!
! SIDE EFFECTS:
!       The output file is overwritten if it already exists.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 17-May-2004
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

PROGRAM SpcCoeff_OLD2NEW

  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler

  USE SensorInfo_Define
  USE SensorInfo_LinkedList
  USE SensorInfo_IO

  USE SpcCoeff_Define
  USE SpcCoeff_netCDF_IO

  USE SpcCoeff_Define_old,    ONLY : SpcCoeff_type_old => SpcCoeff_type, &
                                     Initialize_SpcCoeff_old => Initialize_SpcCoeff, &
                                     Destroy_SpcCoeff_old => Destroy_SpcCoeff
  USE SpcCoeff_netCDF_IO_old, ONLY : Read_SpcCoeff_netCDF_old => Read_SpcCoeff_netCDF


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'SpcCoeff_OLD2NEW'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: SpcCoeff_OLD2NEW.f90,v 1.4 2006/05/02 16:58:03 dgroff Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  ! -- Keyword set flag
  INTEGER, PARAMETER :: SET = 1

  ! -- Literal constants 
  REAL( fp_kind ), PARAMETER :: ZERO = 0.0_fp_kind
  REAL( fp_kind ), PARAMETER ::  ONE = 1.0_fp_kind

  ! ---------
  ! Variables
  ! ---------

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  CHARACTER( 256 ) :: Message

  INTEGER :: Error_Status

  CHARACTER( 256 ) :: SensorInfo_Filename
  CHARACTER( 256 ) :: Input_SpcCoeff_Filename
  CHARACTER( 256 ) :: Output_SpcCoeff_Filename

  CHARACTER( 5000 ) :: Title
  CHARACTER( 5000 ) :: History
  CHARACTER( 256 )  :: Sensor_Name
  CHARACTER( 256 )  :: Platform_Name
  CHARACTER( 5000 ) :: Comment


  INTEGER :: n_Sensors, n
  TYPE( SensorInfo_type )      :: SensorInfo
  TYPE( SensorInfo_List_type ) :: SensorInfo_List

  TYPE( SpcCoeff_type     ) :: SpcCoeff
  TYPE( SpcCoeff_type_old ) :: SpcCoeff_old



  !#----------------------------------------------------------------------------#
  !#                          -- INITIALIZE STRUCTURE --                        #
  !#----------------------------------------------------------------------------#

  ! --------------------
  ! SensorInfo structure
  ! --------------------

  CALL Initialize_SensorInfo( SensorInfo )


  ! ----------------------
  ! SensorInfo linked list
  ! ----------------------

  Error_Status = Initialize_SensorInfo_List( SensorInfo_List )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error initializing SensorInfo_List.', &
                          Error_Status )
    STOP
  END IF


  ! -------------------
  ! SpcCoeff structures
  ! -------------------

  CALL Init_SpcCoeff( SpcCoeff )
  CALL Initialize_SpcCoeff_old( SpcCoeff_old )



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to convert netCDF format SpcCoeff files from OLD ", &
             &/5x, "   to NEW format.                                         ")' )
  WRITE( *, '(/5x, " $Revision: 1.4 $")' )
  WRITE( *, '(/5x, a, / )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                        -- READ THE SensorInfo FILE --                      #
  !#----------------------------------------------------------------------------#

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
    CALL display_message( PROGRAM_NAME, &
                          'Error reading SensorInfo file '//&
                          TRIM( SensorInfo_Filename ), &
                          FAILURE )
    STOP
  END IF

  ! -- Count the number of sensors
  n_Sensors = Count_SensorInfo_Nodes( SensorInfo_List )

  IF ( n_Sensors < 1 ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'SensorInfo_List is empty.', &
                          FAILURE )
    STOP
  END IF



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
      CALL display_message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF


    ! ----------------------------------------------------
    ! Create the SpcCoeff filenames for the current sensor
    ! ----------------------------------------------------

    Input_SpcCoeff_Filename  = TRIM( SensorInfo%File_Prefix )//'.SpcCoeff.nc'
    Output_SpcCoeff_Filename = TRIM( SensorInfo%File_Prefix )//'.SpcCoeff.nc.NEW'



    !#--------------------------------------------------------------------------#
    !#          -- OPERATE ONLY ON IR SpcCoeff FILES THAT ARE PRESENT --        #
    !#--------------------------------------------------------------------------#

    File_Present: IF ( File_Exists( TRIM( Input_SpcCoeff_Filename ) ) .AND. &
                       SensorInfo%Microwave_Flag == 0 ) THEN

   
      ! ----------------------
      ! Output an info message
      ! ----------------------

      WRITE( *, '( //5x, "Converting the SpcCoeff data file for ", a, 1x, a, " (", a, ")" )' ) &
                TRIM( SensorInfo%Satellite_Name ), &
                TRIM( SensorInfo%Sensor_Name ), &
                TRIM( SensorInfo%File_Prefix )



      !#------------------------------------------------------------------------#
      !#              -- READ THE OLD FORMAT NETCDF SpcCoeff FILE --            #
      !#------------------------------------------------------------------------#

      WRITE( *, '( /5x, "Reading OLD FORMAT netCDF SpcCoeff data ..." )' )

      Error_Status = Read_SpcCoeff_netCDF_old( TRIM( Input_SpcCoeff_Filename ), &
                                               SpcCoeff_old, &
                                               Title         = Title,         &
                                               History       = History,       &
                                               Sensor_Name   = Sensor_Name,   &
                                               Platform_Name = Platform_Name, &
                                               Comment       = Comment        )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL display_message( PROGRAM_NAME, &
                              'Error reading OLD FORMAT netCDF SpcCoeff file '//&
                              TRIM( Input_SpcCoeff_Filename ), &
                              Error_Status )
        STOP
      END IF
   


      !#------------------------------------------------------------------------#
      !#                 -- ALLOCATE THE NEW SpcCoeff STRUCTURE --              #
      !#------------------------------------------------------------------------#

      Error_Status = Allocate_SpcCoeff( SpcCoeff_old%n_Channels, &
                                        SpcCoeff )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL display_message( PROGRAM_NAME, &
                              'Error allocating NEW SpcCoeff structure', &
                              Error_Status )
        STOP
      END IF



      !#------------------------------------------------------------------------#
      !#                  -- COPY OVER ALL THE UNCHANGED DATA --                #
      !#------------------------------------------------------------------------#

      SpcCoeff%n_Sensors = SpcCoeff_old%n_Sensors

      SpcCoeff%Sensor_Descriptor             = SpcCoeff_old%Sensor_Descriptor
      SpcCoeff%NCEP_Sensor_ID                = SpcCoeff_old%NCEP_Sensor_ID
      SpcCoeff%WMO_Satellite_ID              = SpcCoeff_old%WMO_Satellite_ID
      SpcCoeff%WMO_Sensor_ID                 = SpcCoeff_old%WMO_Sensor_ID
      SpcCoeff%Sensor_Channel                = SpcCoeff_old%Sensor_Channel
      SpcCoeff%Frequency                     = SpcCoeff_old%Frequency
      SpcCoeff%Wavenumber                    = SpcCoeff_old%Wavenumber
      SpcCoeff%Planck_C1                     = SpcCoeff_old%Planck_C1
      SpcCoeff%Planck_C2                     = SpcCoeff_old%Planck_C2
      SpcCoeff%Band_C1                       = SpcCoeff_old%Band_C1
      SpcCoeff%Band_C2                       = SpcCoeff_old%Band_C2
      SpcCoeff%Is_Microwave_Channel          = SpcCoeff_old%Is_Microwave_Channel
      SpcCoeff%Cosmic_Background_Temperature = SpcCoeff_old%Cosmic_Background_Temperature
      SpcCoeff%Cosmic_Background_Radiance    = SpcCoeff_old%Cosmic_Background_Radiance
      SpcCoeff%Is_Solar_Channel              = SpcCoeff_old%Is_Solar_Channel
      SpcCoeff%Solar_Irradiance              = SpcCoeff_old%Solar_Irradiance
      SpcCoeff%Blackbody_Irradiance          = SpcCoeff_old%Blackbody_Irradiance


      !#------------------------------------------------------------------------#
      !#                            -- THE NEW DATA --                          #
      !#------------------------------------------------------------------------#

      SpcCoeff%Cosmic_Background_Temperature = 2.7253_fp_kind

      SpcCoeff%Polarization = TRANSPOSE( SPREAD( (/ ONE, ZERO, ZERO, ZERO /), &
                                                 DIM = 1, &
                                                 NCOPIES = SpcCoeff%n_Channels ) )



      !#------------------------------------------------------------------------#
      !#             -- WRITE THE NEW FORMAT NETCDF SpcCoeff FILE --            #
      !#------------------------------------------------------------------------#

      WRITE( *, '( /5x, "Writing the NEW FORMAT netCDF SpcCoeff data ..." )' )

      Error_Status = Write_SpcCoeff_netCDF( TRIM( Output_SpcCoeff_Filename ), &
                                            SpcCoeff, &
                                            Title         = TRIM( Title ),         &
                                            History       = PROGRAM_RCS_ID//&
                                                            '; '//TRIM( History ), &
                                            Sensor_Name   = TRIM( Sensor_Name ),   &
                                            Platform_Name = TRIM( Platform_Name ), &
                                            Comment       = &
'CMB value from J.C. Mather, et. al., "Calibrator Design for the COBE Far-Infrared '//&
'Absolute Spectrophotometer (FIRAS)," Astrophysical Journal, vol 512, pp 511-520, Feb 1999; '//&
TRIM( Comment ) )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL display_message( PROGRAM_NAME, &
                              'Error writing NEW FORMAT netCDF SpcCoeff file '//&
                              TRIM( Output_SpcCoeff_Filename ), &
                              Error_Status )
        STOP
      END IF
   


      !#------------------------------------------------------------------------#
      !#        -- DESTROY THE CURRENT SENSOR SpcCoeff DATA STRUCTURES --       #
      !#------------------------------------------------------------------------#

      Error_Status = Destroy_SpcCoeff( SpcCoeff )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL display_message( PROGRAM_NAME, &
                              'Error destroying SpcCoeff data structure for '//&
                              TRIM( SensorInfo%Satellite_Name )//' '//&
                              TRIM( SensorInfo%Sensor_Name )//' processing.', &
                              Error_Status )
        STOP
      END IF


      Error_Status = Destroy_SpcCoeff_old( SpcCoeff_old )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL display_message( PROGRAM_NAME, &
                              'Error destroying SpcCoeff_old data structure for '//&
                              TRIM( SensorInfo%Satellite_Name )//' '//&
                              TRIM( SensorInfo%Sensor_Name )//' processing.', &
                              Error_Status )
        STOP
      END IF

    END IF File_Present



    !#--------------------------------------------------------------------------#
    !#              -- DESTROY THE CURRENT SensorInfo STRUCTURE --              #
    !#--------------------------------------------------------------------------#

    Error_Status = Destroy_SensorInfo( SensorInfo )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL display_message( PROGRAM_NAME, &
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
    CALL display_message( PROGRAM_NAME, &
                          'Error destroying SensorInfo linked list.', &
                          WARNING )
  END IF

END PROGRAM SpcCoeff_OLD2NEW


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: SpcCoeff_OLD2NEW.f90,v 1.4 2006/05/02 16:58:03 dgroff Exp $
!
! $Date: 2006/05/02 16:58:03 $
!
! $Revision: 1.4 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: SpcCoeff_OLD2NEW.f90,v $
! Revision 1.4  2006/05/02 16:58:03  dgroff
! *** empty log message ***
!
! Revision 1.3  2004/08/02 16:42:53  paulv
! - Changed SpcCoeff initialization call from Initialize_SpcCoeff() to
!   Init_SpcCoeff().
!
! Revision 1.2  2004/06/25 21:03:06  paulv
! - Updated for R3->R4 conversion.
!
! Revision 1.1  2004/05/17 17:43:05  paulv
! Initial checkin.
!
!
!
!
