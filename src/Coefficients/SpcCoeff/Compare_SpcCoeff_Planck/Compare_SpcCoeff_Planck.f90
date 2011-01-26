!------------------------------------------------------------------------------
!P+
! NAME:
!       Compare_SpcCoeff_Planck
!
! PURPOSE:
!       Program to compare the Planck function computations for sensors
!       with and without the polychromatic correction
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
!       Sensor_Planck_Functions:    Module containing Planck function radiance, 
!                                   temperature, dB/dT, and dT/dB routines for
!                                   use in computing sensor channel values.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         SPCCOEFF_DEFINE module
!                                         PLANCK_FUNCTIONS module
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
!         - netCDF SpcCoeff data files
!
!       Output:
!         - ASCII results file.
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

PROGRAM Compare_SpcCoeff_Planck


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

  USE Sensor_Planck_Functions


  

  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Compare_SpcCoeff_Planck'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Compare_SpcCoeff_Planck.f90,v 1.2 2006/05/02 16:58:03 dgroff Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  ! -- Keyword set flag
  INTEGER, PARAMETER :: SET = 1

  ! -- The number and range of temperatures used in determining the 
  ! -- polychromatic correction coefficients
  INTEGER,         PARAMETER :: N_TEMPERATURES = 20
  REAL( fp_kind ), PARAMETER :: MIN_TEMPERATURE = 150.0_fp_kind
  REAL( fp_kind ), PARAMETER :: MAX_TEMPERATURE = 340.0_fp_kind

  ! -- Literal constants
  REAL( fp_kind ), PARAMETER :: ZERO = 0.0_fp_kind
  REAL( fp_kind ), PARAMETER ::  ONE = 1.0_fp_kind


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  INTEGER :: Error_Status
  INTEGER :: IO_Status
 
  CHARACTER( 256 ) :: SensorInfo_Filename
  CHARACTER( 256 ) :: SpcCoeff_Filename
  CHARACTER( 256 ) :: ASCII_Filename
  INTEGER          :: ASCII_fileID

  INTEGER :: i, l, n, n_Sensors

  TYPE( SensorInfo_type )      :: SensorInfo
  TYPE( SensorInfo_List_type ) :: SensorInfo_List
  TYPE( SpcCoeff_type )        :: SpcCoeff
  TYPE( SpcCoeff_type )        :: NoPoly_SpcCoeff

  REAL( fp_kind ), DIMENSION( N_TEMPERATURES ) :: True_Temperature
  REAL( fp_kind ) :: Radiance
  REAL( fp_kind ) :: NoPoly_Temperature
  REAL( fp_kind ) :: Poly_Temperature




  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a)' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to compare the Planck function computations for")')
  WRITE( *, '( 5x, "   sensors with and without the polychromatic correction.")' )
  WRITE( *, '(/5x, " $Revision: 1.2 $")' )
  WRITE( *, '( 5x, a, /)' ) PROGRAM_HEADER



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
  !#                       -- GENERATE THE TEMPERATURES --                      #
  !#----------------------------------------------------------------------------#

  True_Temperature = (/ ( REAL( i-1 ), i = 1, N_TEMPERATURES ) /) / &
  !                  --------------------------------------------
                              REAL( N_TEMPERATURES - 1 )

  True_Temperature = MIN_TEMPERATURE + &
                     ( True_Temperature * ( MAX_TEMPERATURE - MIN_TEMPERATURE ) )



  !#----------------------------------------------------------------------------#
  !#                      -- BEGIN LOOP OVER SENSORS --                         #
  !#----------------------------------------------------------------------------#

  Sensor_Loop: DO n = 1, n_Sensors



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



    !#--------------------------------------------------------------------------#
    !#                 -- OPERATE ONLY ON EXISTING DATAFILES --                 #
    !#--------------------------------------------------------------------------#

    ! ------------------------
    ! Create the datafile name
    ! ------------------------

    SpcCoeff_Filename = TRIM( SensorInfo%File_Prefix )//'.SpcCoeff.nc'


    ! ------------------
    ! Test for existance
    ! ------------------

    Available_Data: IF ( File_Exists( TRIM(SpcCoeff_Filename) ) ) THEN

   
      ! ----------------------
      ! Output an info message
      ! ----------------------

      WRITE( *, '( //5x, "Comparing the polychromatic correction for ", a, 1x, a )' ) &
                TRIM( SensorInfo%Satellite_Name ), &
                TRIM( SensorInfo%Sensor_Name )



      !#------------------------------------------------------------------------#
      !#                      -- READ THE SpcCoeff DATA --                      #
      !#------------------------------------------------------------------------#

      Error_Status = Read_SpcCoeff_netCDF( TRIM(SpcCoeff_Filename), &
                                           SpcCoeff )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL display_message( PROGRAM_NAME, &
                              'Error reading netCDF SpcCoeff file '//TRIM(SpcCoeff_Filename), &
                              Error_Status )
        STOP
      END IF



      ! ---------------------------
      ! Copy the SpcCoeff structure
      ! ---------------------------

      Error_Status = Assign_SpcCoeff( SpcCoeff, NoPoly_SpcCoeff )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL display_message( PROGRAM_NAME, &
                              'Error copying SpcCoeff structure for '//&
                             TRIM( SensorInfo%Satellite_Name )//' '//&
                             TRIM( SensorInfo%Sensor_Name ), &
                              Error_Status )
        STOP
      END IF



      ! ------------------------------------------------
      ! Effectively disable any polychromatic correction
      ! ------------------------------------------------

      NoPoly_SpcCoeff%Band_C1 = ZERO
      NoPoly_SpcCoeff%Band_C2 = ONE



      !#------------------------------------------------------------------------#
      !#                      -- OPEN ASCII FILE FOR OUTPUT --                  #
      !#------------------------------------------------------------------------#

      ! -------------------
      ! Create the filename
      ! -------------------

      ASCII_Filename = TRIM( SensorInfo%File_Prefix )//'.SpcCoeff.Polychromatic_Test.asc'


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
        WRITE( Message, '( "Error opening output file ", a, ". STAT = ", i5 )' ) &
                        TRIM( ASCII_Filename ), IO_Status
        CALL Display_Message( PROGRAM_NAME,    &
                              TRIM( Message ), &
                              FAILURE          )
        STOP
      END IF



      !#------------------------------------------------------------------------#
      !#                         -- LOOP OVER CHANNELS --                       #
      !#------------------------------------------------------------------------#

      Channel_Loop: DO l = 1, SpcCoeff%n_Channels


        !#----------------------------------------------------------------------#
        !#               -- COMPUTE THE RADIANCES AND TEMPERATURES --           #
        !#----------------------------------------------------------------------#

        ! -------------
        ! Output header
        ! -------------

        WRITE( *, FMT = 100 ) TRIM( SensorInfo%Satellite_Name ), &
                              TRIM( SensorInfo%Sensor_Name ), &
                              SpcCoeff%Sensor_Channel( l )
        WRITE( *, FMT = 200 )
        WRITE( *, FMT = 300 )

        WRITE( ASCII_fileID, FMT = 100 ) TRIM( SensorInfo%Satellite_Name ), &
                                         TRIM( SensorInfo%Sensor_Name ), &
                                         SpcCoeff%Sensor_Channel( l )
        WRITE( ASCII_fileID, FMT = 200 )
        WRITE( ASCII_fileID, FMT = 300 )


        100 FORMAT( /5x, a, 1x, a, " CHANNEL ", i4 )
        200 FORMAT(  4x, "T_True     T_NoPoly   dT(True-NoPoly)    T_Poly     dT(True-Poly)" )
        300 FORMAT(  2x, 67("-") )


        ! -------------------------------------------
        ! Loop over temperature range used to compute
        ! polychromatic correction coefficients
        ! -------------------------------------------

        Temperature_Loop: DO i = 1, N_TEMPERATURES


          ! -----------------------------------------------------
          ! Calculate radiances WITH the polychromatic correction
          ! -----------------------------------------------------

          Error_Status = Sensor_Radiance( SpcCoeff, &
                                          SpcCoeff%Sensor_Channel( l ), &
                                          True_Temperature( i ), &
                                          Radiance )

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error calculating radiance at T = ", f5.1, " K for ", a, 1x, a )' ) &
                            True_Temperature( i ), &
                            TRIM( SensorInfo%Satellite_Name ), &
                            TRIM( SensorInfo%Sensor_Name )
            CALL Display_Message( PROGRAM_NAME,    &
                                  TRIM( Message ), &
                                  Error_Status     )
            STOP
          END IF


          ! --------------------------------------------
          ! Convert the radiance back into a temperature
          ! WITHOUT the polychromatic correction
          ! --------------------------------------------

          Error_Status = Sensor_Temperature( NoPoly_SpcCoeff, &
                                             NoPoly_SpcCoeff%Sensor_Channel( l ), &
                                             Radiance, &
                                             NoPoly_Temperature )

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error calculating no-poly temperature at T = ", f5.1, " K for ", a, 1x, a )' ) &
                            True_Temperature( i ), &
                            TRIM( SensorInfo%Satellite_Name ), &
                            TRIM( SensorInfo%Sensor_Name )
            CALL Display_Message( PROGRAM_NAME,    &
                                  TRIM( Message ), &
                                  Error_Status     )
            STOP
          END IF


          ! ---------------------------------------------
          ! Compute a polychromatic-corrected temperature
          ! for comparison
          ! ---------------------------------------------

          Error_Status = Sensor_Temperature( SpcCoeff, &
                                             SpcCoeff%Sensor_Channel( l ), &
                                             Radiance, &
                                             Poly_Temperature )

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error calculating temperature at T = ", f5.1, " K for ", a, 1x, a )' ) &
                            True_Temperature( i ), &
                            TRIM( SensorInfo%Satellite_Name ), &
                            TRIM( SensorInfo%Sensor_Name )
            CALL Display_Message( PROGRAM_NAME,    &
                                  TRIM( Message ), &
                                  Error_Status     )
            STOP
          END IF


          ! -------------
          ! Output result
          ! -------------

          WRITE( *, FMT = 400 ) True_Temperature( i ), &
                                NoPoly_Temperature, &
                                True_Temperature( i ) - NoPoly_Temperature, &
                                Poly_Temperature, &
                                True_Temperature( i ) - Poly_Temperature

          WRITE( ASCII_fileID, FMT = 400 ) True_Temperature( i ), &
                                           NoPoly_Temperature, &
                                           True_Temperature( i ) - NoPoly_Temperature, &
                                           Poly_Temperature, &
                                           True_Temperature( i ) - Poly_Temperature

          400 FORMAT( 2( 2x, f10.6 ), 2x, es13.6, 4x, f10.6, 2x, es13.6 )

        END DO Temperature_loop

      END DO Channel_Loop



      !#------------------------------------------------------------------------#
      !#           -- DESTROY THE CURRENT SpcCoeff DATA STRUCTURES --           #
      !#------------------------------------------------------------------------#

      Error_Status = Destroy_SpcCoeff( SpcCoeff )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL display_message( PROGRAM_NAME, &
                              'Error destroying SpcCoeff structure for '//&
                              TRIM( SensorInfo%Satellite_Name )//' '//&
                              TRIM( SensorInfo%Sensor_Name )//'.', &
                              Error_Status )
        STOP
      END IF

      Error_Status = Destroy_SpcCoeff( NoPoly_SpcCoeff )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL display_message( PROGRAM_NAME, &
                              'Error destroying NoPoly_SpcCoeff structure for '//&
                              TRIM( SensorInfo%Satellite_Name )//' '//&
                              TRIM( SensorInfo%Sensor_Name )//'.', &
                              Error_Status )
        STOP
      END IF

    END IF Available_Data



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

  END DO Sensor_Loop



  !#--------------------------------------------------------------------------#
  !#                  -- DESTROY THE SensorInfo LINKED LIST --                #
  !#--------------------------------------------------------------------------#

  Error_Status = Destroy_SensorInfo_List( SensorInfo_List )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error destroying SensorInfo linked list.', &
                          WARNING )
  END IF

END PROGRAM Compare_SpcCoeff_Planck


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Compare_SpcCoeff_Planck.f90,v 1.2 2006/05/02 16:58:03 dgroff Exp $
!
! $Date: 2006/05/02 16:58:03 $
!
! $Revision: 1.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Compare_SpcCoeff_Planck.f90,v $
! Revision 1.2  2006/05/02 16:58:03  dgroff
! *** empty log message ***
!
! Revision 1.1  2004/09/17 16:38:22  paulv
! Initial checkin.
!
!
!
!
