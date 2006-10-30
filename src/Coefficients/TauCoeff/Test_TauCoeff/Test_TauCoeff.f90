!------------------------------------------------------------------------------
!M+
! NAME:
!       Test_TauCoeff
!
! PURPOSE:
!       Program to test the TauCoeff definition and I/O routines.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:                 Module containing definitions for kinds
!                                   of variable types.
!
!       File_Utility:               Module containing generic file utility
!                                   routines
!
!       Message_Handler:            Module to define simple error codes and
!                                   handle error conditions
!                                   USEs: FILE_UTILITY module
!
!       TauCoeff_Define:            Module defining the TauCoeff data
!                                   structure and containing routines to
!                                   manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         COMPARE_FLOAT_NUMBERS module
!
!       TauCoeff_Binary_IO:         Module containing routines to read and
!                                   write Binary format TauCoeff files.
!                                   USEs: TYPE_KINDS module
!                                         FILE_UTILITY module
!                                         ERROR_HANDLER module
!                                         BINARY_FILE_UTILITY module
!                                         TAUCOEFF_DEFINE module
!
!       TauCoeff_netCDF_IO:         Module containing routines to read and
!                                   write netCDF format TauCoeff files.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         NETCDF module
!                                         NETCDF_UTILITY module
!                                         TAUCOEFF_DEFINE module
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
!       - netCDF format TauCoeff data file, both input and output
!       - Binary format TauCoeff data file, both input and output
!
! SIDE EFFECTS:
!       The test output files are overwritten if they already exist.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 02-Jan-2003
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
!M-
!------------------------------------------------------------------------------

PROGRAM Test_TauCoeff

  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler

  USE TauCoeff_Define
  USE TauCoeff_Binary_IO
  USE TauCoeff_netCDF_IO


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Test_TauCoeff'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Test_TauCoeff.f90,v 1.6 2006/05/02 14:58:34 dgroff Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  CHARACTER( * ), PARAMETER ::  NC_FILENAME = 'Test.TauCoeff.nc'
  CHARACTER( * ), PARAMETER :: BIN_FILENAME = 'Test.TauCoeff.bin'

  INTEGER, PARAMETER ::  MAX_N_LOOPS = 5000
  INTEGER, PARAMETER :: INFO_N_LOOPS = 500

  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  INTEGER :: Error_Status

  INTEGER :: n_Orders
  INTEGER :: n_Predictors, i
  INTEGER :: n_Absorbers,  j
  INTEGER :: n_Channels,   l
  INTEGER :: n

  TYPE( TauCoeff_type ) :: TauCoeff1
  TYPE( TauCoeff_type ) :: TauCoeff2
  TYPE( TauCoeff_type ) :: TauCoeff3



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a)' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to test TauCoeff netCDF and binary IO.           ")' )
  WRITE( *, '(/5x, " $Revision: 1.6 $")' )
  WRITE( *, '( 5x, a, /)' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                     -- FILL UP A TauCoeff STRUCTURE --                     #
  !#----------------------------------------------------------------------------#

  ! ------------------------------
  ! Define some typical dimensions
  ! ------------------------------

  n_Orders     = 10
  n_Predictors = 5
  n_Absorbers  = 3
  n_Channels   = 100


  ! -----------------------------
  ! Allocate a TauCoeff structure
  ! -----------------------------

  Error_Status = Allocate_TauCoeff( n_Orders, &
                                    n_Predictors, &
                                    n_Absorbers, &
                                    n_Channels, &
                                    TauCoeff1 )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating TauCoeff1 structre.', &
                          Error_Status )
    STOP
  END IF


  ! --------------------------------
  ! Fill up the structure with stuff
  ! --------------------------------

  TauCoeff1%n_Sensors = 1

  TauCoeff1%NCEP_Sensor_ID   = 1 
  TauCoeff1%WMO_Satellite_ID = 2
  TauCoeff1%WMO_Sensor_ID    = 3
  TauCoeff1%Sensor_Channel   = (/ ( l, l = 1, n_Channels ) /)

  TauCoeff1%Absorber_ID = (/ ( j, j = 1, n_Absorbers ) /)    

  TauCoeff1%Alpha    = 4.0_Double
  TauCoeff1%Alpha_C1 = 5.0_Double
  TauCoeff1%Alpha_C2 = 6.0_Double

  TauCoeff1%Order_Index = 5

  DO l = 1, n_Channels
    DO j = 1, n_Absorbers
      TauCoeff1%Predictor_Index( :, j, l ) = (/ ( i, i = 0, n_Predictors ) /)
    END DO
  END DO

  TauCoeff1%C = -99.0_Double



  !#----------------------------------------------------------------------------#
  !#               -- WRITE AND READ THE NETCDF TauCoeff FILE --                #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Testing TauCoeff netCDF I/O functions ..." )' )


  ! --------------------------
  ! Write the netCDF data file
  ! --------------------------

  Error_Status = Write_TauCoeff_netCDF( NC_FILENAME, &
                                        TauCoeff1, &
                                        Title = 'This is the title attribute', &
                                        History = 'This is the history attribute', &
                                        Sensor_Name = 'This is the sensor_name attribute',   &
                                        Platform_Name = 'This is the platform_name attribute', &
                                        Comment = 'This is the comment attribute',&
                                        ID_Tag = 'This is the id_tag attribute' )

  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Error writing the netCDF TauCoeff file '//TRIM( NC_FILENAME )
  ELSE
    Error_Status = INFORMATION
    Message = TRIM( NC_FILENAME )//' netCDF file write was successful'
  END IF

  CALL Display_Message( PROGRAM_NAME, &
                        TRIM( Message ), &
                        Error_Status )


  ! -------------------------
  ! Read the netCDF data file
  ! -------------------------

  Error_Status = Read_TauCoeff_netCDF( NC_FILENAME, &
                                       TauCoeff2 )

  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Error reading the netCDF TauCoeff file '//TRIM( NC_FILENAME )
  ELSE
    Error_Status = INFORMATION
    Message = TRIM( NC_FILENAME )//' netCDF file read was successful'
  END IF

  CALL Display_Message( PROGRAM_NAME, &
                        TRIM( Message ), &
                        Error_Status )


  ! ----------------------
  ! Compare the structures
  ! ----------------------

  Error_Status = Equal_TauCoeff( TauCoeff2, TauCoeff1 )

  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'NetCDF I/O structures are different'
  ELSE
    Error_Status = INFORMATION
    Message = 'NetCDF I/O structures are equal'
  END IF

  CALL Display_Message( PROGRAM_NAME, &
                        TRIM( Message ), &
                        Error_Status )

  WRITE( *, '( //5x, "Press <ENTER> to test Binary I/O functions..." )' )
  READ( *, * )



  !#----------------------------------------------------------------------------#
  !#               -- WRITE AND READ THE BINARY TauCoeff FILE --                #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Testing TauCoeff Binary Write/Read functions ..." )' )


  ! --------------------------
  ! Write the Binary data file
  ! --------------------------

  Error_Status = Write_TauCoeff_Binary( BIN_FILENAME, &
                                        TauCoeff1 )

  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Error writing the Binary TauCoeff file '//TRIM( BIN_FILENAME )
  ELSE
    Error_Status = INFORMATION
    Message = TRIM( BIN_FILENAME )//' Binary file write was successful'
  END IF

  CALL Display_Message( PROGRAM_NAME, &
                        TRIM( Message ), &
                        Error_Status )


  ! -------------------------
  ! Read the Binary data file
  ! -------------------------

  Error_Status = Read_TauCoeff_Binary( BIN_FILENAME, &
                                       TauCoeff3 )

  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Error reading the Binary TauCoeff file '//TRIM( BIN_FILENAME )
  ELSE
    Error_Status = INFORMATION
    Message = TRIM( BIN_FILENAME )//' Binary file read was successful'
  END IF

  CALL Display_Message( PROGRAM_NAME, &
                        TRIM( Message ), &
                        Error_Status )


  ! ----------------------
  ! Compare the structures
  ! ----------------------

  Error_Status = Equal_TauCoeff( TauCoeff3, TauCoeff1 )

  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Binary I/O structures are different'
  ELSE
    Error_Status = INFORMATION
    Message = 'Binary I/O structures are equal'
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

  WRITE( *, '( /5x, "Looping for netCDF read memory leak test ..." )' )

  DO n = 1, MAX_N_LOOPS

    Error_Status = Read_TauCoeff_netCDF( NC_FILENAME, &
                                         TauCoeff1, &
                                         Quiet = 1 )

    IF ( MOD( n, INFO_N_LOOPS ) == 0 .AND. n /= MAX_N_LOOPS ) THEN
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

  WRITE( *, '( /5x, "Looping for Binary read memory leak test ..." )' )

  DO n = 1, MAX_N_LOOPS

    Error_Status = Read_TauCoeff_Binary( BIN_FILENAME, &
                                         TauCoeff2, &
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

  WRITE( *, '( /5x, "Looping for structure copy memory leak test ..." )' )

  DO n = 1, MAX_N_LOOPS

    Error_Status = Assign_TauCoeff( TauCoeff3, TauCoeff2 )

    IF ( MOD( n, INFO_N_LOOPS ) == 0 .AND. n /= MAX_N_LOOPS ) THEN
      WRITE( Message, '( "Completed loop #", i5, " of ", i5 )' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            INFORMATION )
    END IF

  END DO



  !#----------------------------------------------------------------------------#
  !#                          -- DESTROY THE STRUCTURES --                      #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_TauCoeff( TauCoeff1 )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying TauCoeff1 structure.', &
                          WARNING )
  END IF


  Error_Status = Destroy_TauCoeff( TauCoeff2 )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying TauCoeff2 structure.', &
                          WARNING )
  END IF

  Error_Status = Destroy_TauCoeff( TauCoeff3 )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying TauCoeff3 structure.', &
                          WARNING )
  END IF

END PROGRAM Test_TauCoeff


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Test_TauCoeff.f90,v 1.6 2006/05/02 14:58:34 dgroff Exp $
!
! $Date: 2006/05/02 14:58:34 $
!
! $Revision: 1.6 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Test_TauCoeff.f90,v $
! Revision 1.6  2006/05/02 14:58:34  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.5  2005/02/07 18:16:09  paulv
! - Category change only.
!
! Revision 1.4  2004/09/09 20:37:17  paulv
! - Renamed Test_TauCoeff_IO to Test_TauCoeff
!
! Revision 1.3  2004/08/20 18:06:24  paulv
! - Changed the number of loops for the memory leak test.
!
! Revision 1.2  2004/08/20 00:19:55  paulv
! - New versions for testing upgrades to Fortran95.
!
! Revision 1.1  2003/01/02 22:08:02  paulv
! Initial checkin.
!
!
!
!
