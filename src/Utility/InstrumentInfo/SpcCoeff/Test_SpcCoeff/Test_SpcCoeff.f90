!------------------------------------------------------------------------------
!P+
! NAME:
!       Test_SpcCoeff
!
! PURPOSE:
!       Program to test the SpcCoeff definition and I/O routines.
!
! CATEGORY:
!       Instrument_Information : SpcCoeff
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
!       SpcCoeff_Define:            Module defining the SpcCoeff data structure and
!                                   containing routines to manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         FILE_UTILITY module
!                                         ERROR_HANDLER module
!
!       SpcCoeff_Binary_IO:         Module containing routines to read and
!                                   write Binary format SpcCoeff files.
!                                   USEs: TYPE_KINDS module
!                                         FILE_UTILITY module
!                                         ERROR_HANDLER module
!                                         BINARY_FILE_UTILITY module
!                                         SPCCOEFF_DEFINE module
!
!       SpcCoeff_netCDF_IO:         Module containing routines to read and
!                                   write netCDF format SpcCoeff files.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         NETCDF module
!                                         NETCDF_UTILITY module
!                                         SPCCOEFF_DEFINE module
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
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 28-Feb-2003
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

PROGRAM Test_SpcCoeff


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler

  USE SpcCoeff_Define
  USE SpcCoeff_Binary_IO
  USE SpcCoeff_netCDF_IO


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------
 
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Test_SpcCoeff'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Test_SpcCoeff.f90,v 2.1 2006/05/02 16:58:03 dgroff Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  CHARACTER( * ), PARAMETER ::  SENSOR_NC_FILENAME = 'Test.Sensor.SpcCoeff.nc'
  CHARACTER( * ), PARAMETER :: SENSOR_BIN_FILENAME = 'Test.Sensor.SpcCoeff.bin'

  CHARACTER( * ), PARAMETER ::  SPECTRAL_NC_FILENAME = 'Test.Spectral.SpcCoeff.nc'
  CHARACTER( * ), PARAMETER :: SPECTRAL_BIN_FILENAME = 'Test.Spectral.SpcCoeff.bin'

  INTEGER, PARAMETER :: MAX_N_LOOPS  = 10000
  INTEGER, PARAMETER :: INFO_N_LOOPS = 1000


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  INTEGER :: Error_Status

  INTEGER :: n_Channels, l
  INTEGER :: n_Nodes
  INTEGER :: Max_Channels_per_Node
  INTEGER :: n

  TYPE( SpcCoeff_Sensor_type ) :: SpcCoeff_Sensor_1
  TYPE( SpcCoeff_Sensor_type ) :: SpcCoeff_Sensor_2
  TYPE( SpcCoeff_Sensor_type ) :: SpcCoeff_Sensor_3

  TYPE( SpcCoeff_Spectral_type ) :: SpcCoeff_Spectral_1
  TYPE( SpcCoeff_Spectral_type ) :: SpcCoeff_Spectral_2
  TYPE( SpcCoeff_Spectral_type ) :: SpcCoeff_Spectral_3



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to test SpcCoeff definition and I/O routines. ")' )
  WRITE( *, '(/5x, " $Revision: 2.1 $")' )
  WRITE( *, '(/5x, a, / )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                    -- FILL UP THE SpcCoeff STRUCTURES --                   #
  !#----------------------------------------------------------------------------#

  ! ------------------------------
  ! Define some typical dimensions
  ! ------------------------------

  n_Channels            = 500
  n_Nodes               = 2000
  Max_Channels_per_Node = 5


  ! --------------------------------
  ! Allocate the SpcCoeff structures
  ! --------------------------------

  Error_Status = Allocate_SpcCoeff( n_Channels, &
                                    SpcCoeff_Sensor_1 )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating SpcCoeff_Sensor_1 structure.', &
                          Error_Status )
    STOP
  END IF


  Error_Status = Allocate_SpcCoeff( n_Channels, &
                                    n_Nodes, &
                                    Max_Channels_per_Node, &
                                    SpcCoeff_Spectral_1 )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating SpcCoeff_Spectral_1 structure.', &
                          Error_Status )
    STOP
  END IF


  ! ---------------------------------
  ! Fill up the structures with stuff
  ! ---------------------------------

  ! -- The SENSOR type
  SpcCoeff_Sensor_1%n_Sensors = 1

  SpcCoeff_Sensor_1%Sensor_Descriptor             = 'test'
  SpcCoeff_Sensor_1%Sensor_Type                   = INFRARED_SENSOR
  SpcCoeff_Sensor_1%NCEP_Sensor_ID                = 1
  SpcCoeff_Sensor_1%WMO_Satellite_ID              = 2
  SpcCoeff_Sensor_1%WMO_Sensor_ID                 = 3
  SpcCoeff_Sensor_1%Sensor_Channel                = (/ ( l, l = 1, n_Channels ) /)
  SpcCoeff_Sensor_1%Frequency                     = 0.0_fp_kind
  SpcCoeff_Sensor_1%Wavenumber                    = 0.0_fp_kind
  SpcCoeff_Sensor_1%Planck_C1                     = 0.0_fp_kind
  SpcCoeff_Sensor_1%Planck_C2                     = 0.0_fp_kind
  SpcCoeff_Sensor_1%Band_C1                       = 0.0_fp_kind
  SpcCoeff_Sensor_1%Band_C2                       = 0.0_fp_kind
  SpcCoeff_Sensor_1%Polarization                  = UNPOLARIZED 
  SpcCoeff_Sensor_1%Cosmic_Background_Radiance    = 0.0_fp_kind
  SpcCoeff_Sensor_1%Is_Solar_Channel              = -1
  SpcCoeff_Sensor_1%Solar_Irradiance              = 0.0_fp_kind 

  ! -- The SPECTRAL type
  SpcCoeff_Spectral_1%n_Sensors = 1

  SpcCoeff_Spectral_1%Sensor_Descriptor             = 'test'
  SpcCoeff_Spectral_1%Sensor_Type                   = INFRARED_SENSOR
  SpcCoeff_Spectral_1%NCEP_Sensor_ID                = 1
  SpcCoeff_Spectral_1%WMO_Satellite_ID              = 2
  SpcCoeff_Spectral_1%WMO_Sensor_ID                 = 3
  SpcCoeff_Spectral_1%Sensor_Channel                = (/ ( l, l = 1, n_Channels ) /)
  SpcCoeff_Spectral_1%Frequency                     = 0.0_fp_kind
  SpcCoeff_Spectral_1%Wavenumber                    = 0.0_fp_kind
  SpcCoeff_Spectral_1%Planck_C1                     = 0.0_fp_kind
  SpcCoeff_Spectral_1%Planck_C2                     = 0.0_fp_kind
  SpcCoeff_Spectral_1%Band_C1                       = 0.0_fp_kind
  SpcCoeff_Spectral_1%Band_C2                       = 0.0_fp_kind
  SpcCoeff_Spectral_1%Polarization                  = UNPOLARIZED 
  SpcCoeff_Spectral_1%Is_Solar_Channel              = -1
  SpcCoeff_Spectral_1%Solar_Irradiance              = 0.0_fp_kind
  SpcCoeff_Spectral_1%MW_and_IR_Channel_Index       = 4
  SpcCoeff_Spectral_1%n_Channels_per_Node           = Max_Channels_per_Node
  SpcCoeff_Spectral_1%Channel_Node_Map              = 5
  SpcCoeff_Spectral_1%MW_and_IR_Node_Index          = 6
  SpcCoeff_Spectral_1%Node_Frequency                = 0.0_fp_kind
  SpcCoeff_Spectral_1%Node_Wavenumber               = 0.0_fp_kind
  SpcCoeff_Spectral_1%Node_Planck_C1                = 0.0_fp_kind
  SpcCoeff_Spectral_1%Node_Planck_C2                = 0.0_fp_kind
  SpcCoeff_Spectral_1%Cosmic_Background_Radiance    = 0.0_fp_kind



  !#----------------------------------------------------------------------------#
  !#               -- WRITE AND READ THE NETCDF SpcCoeff FILE --                #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Testing SpcCoeff netCDF I/O functions ..." )' )


  ! --------------------------
  ! Write the netCDF data file
  ! --------------------------

  ! -- The SENSOR file type
  Error_Status = Write_SpcCoeff_netCDF( SENSOR_NC_FILENAME, &
                                        SpcCoeff_Sensor_1, &
                                        Title = 'This is the SENSOR title attribute', &
                                        History = 'This is the SENSOR history attribute', &
                                        Sensor_Name = 'This is the SENSOR sensor_name attribute',   &
                                        Platform_Name = 'This is the SENSOR platform_name attribute', &
                                        Comment = 'This is the SENSOR comment attribute' )

  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Error writing the netCDF SENSOR SpcCoeff file '//TRIM( SENSOR_NC_FILENAME )
  ELSE
    Error_Status = INFORMATION
    Message = TRIM( SENSOR_NC_FILENAME )//' netCDF file write was successful'
  END IF

  CALL Display_Message( PROGRAM_NAME, &
                        TRIM( Message ), &
                        Error_Status )


  ! -- The SPECTRAL file type
  Error_Status = Write_SpcCoeff_netCDF( SPECTRAL_NC_FILENAME, &
                                        SpcCoeff_Spectral_1, &
                                        Title = 'This is the SPECTRAL title attribute', &
                                        History = 'This is the SPECTRAL history attribute', &
                                        Sensor_Name = 'This is the SPECTRAL sensor_name attribute',   &
                                        Platform_Name = 'This is the SPECTRAL platform_name attribute', &
                                        Comment = 'This is the SPECTRAL comment attribute' )

  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Error writing the netCDF SPECTRAL SpcCoeff file '//TRIM( SPECTRAL_NC_FILENAME )
  ELSE
    Error_Status = INFORMATION
    Message = TRIM( SPECTRAL_NC_FILENAME )//' netCDF file write was successful'
  END IF

  CALL Display_Message( PROGRAM_NAME, &
                        TRIM( Message ), &
                        Error_Status )


  ! -------------------------
  ! Read the netCDF data file
  ! -------------------------

  ! -- The SENSOR file type
  Error_Status = Read_SpcCoeff_netCDF( SENSOR_NC_FILENAME, &
                                       SpcCoeff_Sensor_2 )

  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Error reading the netCDF SENSOR SpcCoeff file '//TRIM( SENSOR_NC_FILENAME )
  ELSE
    Error_Status = INFORMATION
    Message = TRIM( SENSOR_NC_FILENAME )//' netCDF file read was successful'
  END IF

  CALL Display_Message( PROGRAM_NAME, &
                        TRIM( Message ), &
                        Error_Status )


  ! -- The SPECTRAL file type
  Error_Status = Read_SpcCoeff_netCDF( SPECTRAL_NC_FILENAME, &
                                       SpcCoeff_Spectral_2 )

  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Error reading the netCDF SPECTRAL SpcCoeff file '//TRIM( SPECTRAL_NC_FILENAME )
  ELSE
    Error_Status = INFORMATION
    Message = TRIM( SPECTRAL_NC_FILENAME )//' netCDF file read was successful'
  END IF

  CALL Display_Message( PROGRAM_NAME, &
                        TRIM( Message ), &
                        Error_Status )


  ! ----------------------
  ! Compare the structures
  ! ----------------------

  ! -- The SENSOR file type
  Error_Status = Equal_SpcCoeff( SpcCoeff_Sensor_2, SpcCoeff_Sensor_1 )

  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'NetCDF I/O SENSOR structures are different'
  ELSE
    Error_Status = INFORMATION
    Message = 'NetCDF I/O SENSOR structures are equal'
  END IF

  CALL Display_Message( PROGRAM_NAME, &
                        TRIM( Message ), &
                        Error_Status )


  ! -- The SPECTRAL file type
  Error_Status = Equal_SpcCoeff( SpcCoeff_Spectral_2, SpcCoeff_Spectral_1 )

  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'NetCDF I/O SPECTRAL structures are different'
  ELSE
    Error_Status = INFORMATION
    Message = 'NetCDF I/O SPECTRAL structures are equal'
  END IF

  CALL Display_Message( PROGRAM_NAME, &
                        TRIM( Message ), &
                        Error_Status )

  WRITE( *, '( //5x, "Press <ENTER> to test Binary I/O functions..." )' )
  READ( *, * )



  !#----------------------------------------------------------------------------#
  !#               -- WRITE AND READ THE BINARY SpcCoeff FILE --                #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Testing SpcCoeff Binary Write/Read functions ..." )' )


  ! --------------------------
  ! Write the Binary data file
  ! --------------------------

  ! -- The SENSOR file type
  Error_Status = Write_SpcCoeff_Binary( SENSOR_BIN_FILENAME, &
                                        SpcCoeff_Sensor_1 )

  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Error writing the Binary SENSOR SpcCoeff file '//TRIM( SENSOR_BIN_FILENAME )
  ELSE
    Error_Status = INFORMATION
    Message = TRIM( SENSOR_BIN_FILENAME )//' Binary file write was successful'
  END IF

  CALL Display_Message( PROGRAM_NAME, &
                        TRIM( Message ), &
                        Error_Status )


  ! -- The SPECTRAL file type
  Error_Status = Write_SpcCoeff_Binary( SPECTRAL_BIN_FILENAME, &
                                        SpcCoeff_Spectral_1 )

  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Error writing the Binary SPECTRAL SpcCoeff file '//TRIM( SPECTRAL_BIN_FILENAME )
  ELSE
    Error_Status = INFORMATION
    Message = TRIM( SPECTRAL_BIN_FILENAME )//' Binary file write was successful'
  END IF

  CALL Display_Message( PROGRAM_NAME, &
                        TRIM( Message ), &
                        Error_Status )


  ! -------------------------
  ! Read the Binary data file
  ! -------------------------

  ! -- The SENSOR file type
  Error_Status = Read_SpcCoeff_Binary( SENSOR_BIN_FILENAME, &
                                       SpcCoeff_Sensor_3 )

  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Error reading the Binary SENSOR SpcCoeff file '//TRIM( SENSOR_BIN_FILENAME )
  ELSE
    Error_Status = INFORMATION
    Message = TRIM( SENSOR_BIN_FILENAME )//' Binary file read was successful'
  END IF

  CALL Display_Message( PROGRAM_NAME, &
                        TRIM( Message ), &
                        Error_Status )


  ! -- The SPECTRAL file type
  Error_Status = Read_SpcCoeff_Binary( SPECTRAL_BIN_FILENAME, &
                                       SpcCoeff_Spectral_3 )

  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Error reading the Binary SPECTRAL SpcCoeff file '//TRIM( SPECTRAL_BIN_FILENAME )
  ELSE
    Error_Status = INFORMATION
    Message = TRIM( SPECTRAL_BIN_FILENAME )//' Binary file read was successful'
  END IF

  CALL Display_Message( PROGRAM_NAME, &
                        TRIM( Message ), &
                        Error_Status )


  ! ----------------------
  ! Compare the structures
  ! ----------------------

  ! -- The SENSOR file type
  Error_Status = Equal_SpcCoeff( SpcCoeff_Sensor_3, SpcCoeff_Sensor_1 )

  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Binary I/O SENSOR structures are different'
  ELSE
    Error_Status = INFORMATION
    Message = 'Binary I/O SENSOR structures are equal'
  END IF

  CALL Display_Message( PROGRAM_NAME, &
                        TRIM( Message ), &
                        Error_Status )

  ! -- The SPECTRAL file type
  Error_Status = Equal_SpcCoeff( SpcCoeff_Spectral_3, SpcCoeff_Spectral_1 )

  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Binary I/O SPECTRAL structures are different'
  ELSE
    Error_Status = INFORMATION
    Message = 'Binary I/O SPECTRAL structures are equal'
  END IF

  CALL Display_Message( PROGRAM_NAME, &
                        TRIM( Message ), &
                        Error_Status )

  WRITE( *, '( //5x, "Press <ENTER> to test for netCDF reader memory leaks..." )' )
  READ( *, * )



  !#----------------------------------------------------------------------------#
  !#                       -- LOOP FOR MEMORY LEAK TEST --                      #
  !#----------------------------------------------------------------------------#

  ! ---------------------------------------
  ! Test the netCDF reader for memory leaks
  ! ---------------------------------------

  ! -- The SENSOR file type
  WRITE( *, '( /5x, "Looping for netCDF SENSOR read memory leak test ..." )' )

  DO n = 1, MAX_N_LOOPS

    Error_Status = Read_SpcCoeff_netCDF( SENSOR_NC_FILENAME, &
                                         SpcCoeff_Sensor_1, &
                                         Quiet = 1 )

    IF ( MOD( n, INFO_N_LOOPS ) == 0 ) THEN
      WRITE( Message, '( "Completed loop #", i5, " of ", i5 )' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            INFORMATION )
    END IF

  END DO

  ! -- The SENSOR file type
  WRITE( *, '( /5x, "Looping for netCDF SPECTRAL read memory leak test ..." )' )

  DO n = 1, MAX_N_LOOPS

    Error_Status = Read_SpcCoeff_netCDF( SPECTRAL_NC_FILENAME, &
                                         SpcCoeff_Spectral_1, &
                                         Quiet = 1 )

    IF ( MOD( n, INFO_N_LOOPS ) == 0 ) THEN
      WRITE( Message, '( "Completed loop #", i5, " of ", i5 )' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            INFORMATION )
    END IF

  END DO

  WRITE( *, '( //5x, "Press <ENTER> to test for Binary reader memory leaks..." )' )
  READ( *, * )


  ! ---------------------------------------
  ! Test the Binary reader for memory leaks
  ! ---------------------------------------

  ! -- The SENSOR file type
  WRITE( *, '( /5x, "Looping for Binary SENSOR read memory leak test ..." )' )

  DO n = 1, MAX_N_LOOPS

    Error_Status = Read_SpcCoeff_Binary( SENSOR_BIN_FILENAME, &
                                         SpcCoeff_Sensor_2, &
                                         Quiet = 1 )

    IF ( MOD( n, INFO_N_LOOPS ) == 0 .AND. n /= MAX_N_LOOPS ) THEN
      WRITE( Message, '( "Completed loop #", i5, " of ", i5 )' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            INFORMATION )
    END IF

  END DO


  ! -- The SPECTRAL file type
  WRITE( *, '( /5x, "Looping for Binary SPECTRAL read memory leak test ..." )' )

  DO n = 1, MAX_N_LOOPS

    Error_Status = Read_SpcCoeff_Binary( SPECTRAL_BIN_FILENAME, &
                                         SpcCoeff_Spectral_2, &
                                         Quiet = 1 )

    IF ( MOD( n, INFO_N_LOOPS ) == 0 .AND. n /= MAX_N_LOOPS ) THEN
      WRITE( Message, '( "Completed loop #", i5, " of ", i5 )' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            INFORMATION )
    END IF

  END DO

  WRITE( *, '( //5x, "Press <ENTER> to test for structure assign function memory leaks..." )' )
  READ( *, * )


  ! -----------------------------------------
  ! Test the Assign function for memory leaks
  ! -----------------------------------------

  ! -- The SENSOR type
  WRITE( *, '( /5x, "Looping for SENSOR structure copy memory leak test ..." )' )

  DO n = 1, MAX_N_LOOPS

    Error_Status = Assign_SpcCoeff( SpcCoeff_Sensor_3, SpcCoeff_Sensor_2 )

    IF ( MOD( n, INFO_N_LOOPS ) == 0 ) THEN
      WRITE( Message, '( "Completed loop #", i5, " of ", i5 )' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            INFORMATION )
    END IF

  END DO


  ! -- The SPECTRAL type
  WRITE( *, '( /5x, "Looping for SPECTRAL structure copy memory leak test ..." )' )

  DO n = 1, MAX_N_LOOPS

    Error_Status = Assign_SpcCoeff( SpcCoeff_Spectral_3, SpcCoeff_Spectral_2 )

    IF ( MOD( n, INFO_N_LOOPS ) == 0 ) THEN
      WRITE( Message, '( "Completed loop #", i5, " of ", i5 )' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            INFORMATION )
    END IF

  END DO



  !#----------------------------------------------------------------------------#
  !#                          -- DESTROY THE STRUCTURES --                      #
  !#----------------------------------------------------------------------------#

  ! -- The SENSOR type
  Error_Status = Destroy_SpcCoeff( SpcCoeff_Sensor_1 )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying SpcCoeff_Sensor_1 structure.', &
                          WARNING )
  END IF


  Error_Status = Destroy_SpcCoeff( SpcCoeff_Sensor_2 )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying SpcCoeff_Sensor_2 structure.', &
                          WARNING )
  END IF

  Error_Status = Destroy_SpcCoeff( SpcCoeff_Sensor_3 )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying SpcCoeff_Sensor_3 structure.', &
                          WARNING )
  END IF


  ! -- The SPECTRAL type
  Error_Status = Destroy_SpcCoeff( SpcCoeff_Spectral_1 )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying SpcCoeff_Spectral_1 structure.', &
                          WARNING )
  END IF


  Error_Status = Destroy_SpcCoeff( SpcCoeff_Spectral_2 )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying SpcCoeff_Spectral_2 structure.', &
                          WARNING )
  END IF

  Error_Status = Destroy_SpcCoeff( SpcCoeff_Spectral_3 )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying SpcCoeff_Spectral_3 structure.', &
                          WARNING )
  END IF

END PROGRAM Test_SpcCoeff


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Test_SpcCoeff.f90,v 2.1 2006/05/02 16:58:03 dgroff Exp $
!
! $Date: 2006/05/02 16:58:03 $
!
! $Revision: 2.1 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Test_SpcCoeff.f90,v $
! Revision 2.1  2006/05/02 16:58:03  dgroff
! *** empty log message ***
!
! Revision 2.0  2005/07/05 23:53:33  paulv
! - Major update. Testing now includes that for the SENSOR and SPECTRAL
!   SpcCoeff data structure and I/O routines.
!
! Revision 1.2  2005/04/01 16:58:32  paulv
! - Updated for SpcCoeff Release5
!
! Revision 1.1  2004/08/23 15:08:30  paulv
! Initial checkin.
!
!
!
