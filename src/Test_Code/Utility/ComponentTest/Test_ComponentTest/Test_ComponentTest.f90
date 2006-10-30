!------------------------------------------------------------------------------
!P+
! NAME:
!       Test_ComponentTest
!
! PURPOSE:
!       Program to test the ComponentTest definition and I/O routines.
!
! CATEGORY:
!       CRTM : Test : Utility : ComponentTest
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:                 Module containing definitions for kinds
!                                   of variable types.
!
!       Message_Handler:            Module to define simple error codes and
!                                   handle error conditions
!                                   USEs: FILE_UTILITY module
!
!       ComponentTest_Define:       Module defining the ComponentTest data structure and
!                                   containing routines to manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         FILE_UTILITY module
!                                         ERROR_HANDLER module
!
!       ComponentTest_netCDF_IO:    Module containing routines to read and
!                                   write netCDF format ComponentTest files.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         NETCDF module
!                                         NETCDF_UTILITY module
!                                         COMPONENTTEST_DEFINE module
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
!       Written by:     Paul van Delst, CIMSS/SSEC 06-Mar-2006
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2006 Paul van Delst
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

PROGRAM Test_ComponentTest


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler

  USE ComponentTest_netCDF_IO


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------
 
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Test_ComponentTest'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Test_ComponentTest.f90,v 1.2 2006/05/02 14:58:35 dgroff Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  CHARACTER( * ), PARAMETER ::  TEST_FILENAME = 'Test_ComponentTest.nc'

  INTEGER, PARAMETER :: N_DATASETS   = 52
  INTEGER, PARAMETER :: MAX_N_LOOPS  = 100
  INTEGER, PARAMETER :: INFO_N_LOOPS = 10


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  INTEGER :: Error_Status

  INTEGER :: nK, nL, nP, nIV, nOV
  INTEGER :: n

  TYPE( ComponentTest_type ) :: ComponentTest



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to test ComponentTest definition and I/O routines. ")' )
  WRITE( *, '(/5x, " $Revision: 1.2 $")' )
  WRITE( *, '(/5x, a, / )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                  -- FILL UP THE ComponentTest STRUCTURES --                #
  !#----------------------------------------------------------------------------#

  ! ------------------------------
  ! Define some typical dimensions
  ! ------------------------------

  nK  = 100
  nL  = 15
  nP  = 21
  nIV = 4
  nOV = 5


  ! -------------------------------------
  ! Allocate the ComponentTest structures
  ! -------------------------------------

  Error_Status = Allocate_ComponentTest( nK, nL, nP, nIV, nOV, &
                                         ComponentTest )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating ComponentTest structure.', &
                          Error_Status )
    STOP
  END IF


  ! ---------------------------------
  ! Fill up the structures with stuff
  ! ---------------------------------

  ComponentTest%TestType = COMPONENTTEST_TLAD_TESTTYPE
  ComponentTest%DataType = COMPONENTTEST_POLY_DATATYPE

  ComponentTest%Pressure     = 1.0_fp_kind
  ComponentTest%Spectral     = 2.0_fp_kind
  ComponentTest%Perturbation = 3.0_fp_kind

  DO n = 1, nIV
    WRITE( ComponentTest%Input_Variable_Name(n), '("Input Variable #", i2.2 )' ) n
    WRITE( ComponentTest%Input_Variable_Units(n), '("Units for input variable #", i2.2 )' ) n
  END DO

  DO n = 1, nOV
    WRITE( ComponentTest%Output_Variable_Name(n), '("Output Variable #", i2.2 )' ) n
    WRITE( ComponentTest%Output_Variable_Units(n), '("Units for output variable #", i2.2 )' ) n
  END DO

  ComponentTest%d1 = 4.0_fp_kind
  ComponentTest%d2 = 5.0_fp_kind



  !#----------------------------------------------------------------------------#
  !#              -- WRITE AND READ THE NETCDF ComponentTest FILE --            #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Testing ComponentTest netCDF I/O functions ..." )' )


  ! --------------------------
  ! Write the netCDF data file
  ! --------------------------

  DO n = 1, N_DATASETS

    ComponentTest%nM = n
    WRITE( ComponentTest%nM_Name, '("dataset #", i2.2 )' ) n

    Error_Status = Write_ComponentTest_netCDF( TEST_FILENAME, &
                                               ComponentTest, &
                                               Title = 'This is the title attribute', &
                                               History = 'This is the history attribute', &
                                               Sensor_Name = 'This is the sensor_name attribute',   &
                                               Platform_Name = 'This is the platform_name attribute', &
                                               Comment = 'This is the comment attribute' )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error writing '//TRIM( ComponentTest%nM_Name )//' to '//TEST_FILENAME, &
                            Error_Status )
      STOP
    END IF

  END DO


  ! -------------------------
  ! Read the netCDF data file
  ! -------------------------

  DO n = 1, N_DATASETS

    Error_Status = Read_ComponentTest_netCDF( TEST_FILENAME, &
                                              n, &
                                              ComponentTest )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '("dataset #", i2.2 )' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            'Error reading '//TRIM( ComponentTest%nM_Name )//' from '//TEST_FILENAME, &
                            Error_Status )
      STOP
    END IF

  END DO

  WRITE( *, '( //5x, "Press <ENTER> to test for netCDF reader memory leaks..." )' )
  READ( *, * )



  !#----------------------------------------------------------------------------#
  !#                       -- LOOP FOR MEMORY LEAK TEST --                      #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Looping for netCDF read memory leak test ..." )' )

  DO n = 1, MAX_N_LOOPS

    Error_Status = Read_ComponentTest_netCDF( TEST_FILENAME, &
                                              1, &
                                              ComponentTest  )

    IF ( MOD( n, INFO_N_LOOPS ) == 0 ) THEN
      WRITE( Message, '( "Completed loop #", i5, " of ", i5 )' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            INFORMATION )
    END IF

  END DO



  !#----------------------------------------------------------------------------#
  !#                          -- DESTROY THE STRUCTURE --                       #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_ComponentTest( ComponentTest )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying ComponentTest structure.', &
                          WARNING )
  END IF

END PROGRAM Test_ComponentTest


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Test_ComponentTest.f90,v 1.2 2006/05/02 14:58:35 dgroff Exp $
!
! $Date: 2006/05/02 14:58:35 $
!
! $Revision: 1.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Test_ComponentTest.f90,v $
! Revision 1.2  2006/05/02 14:58:35  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.1  2006/03/06 23:07:26  paulv
! Initial checkin. Tested.
!
!
!
