!------------------------------------------------------------------------------
!P+
! NAME:
!       Test_Equal_SpcCoeff
!
! PURPOSE:
!       Program to test if the data in two netCDF SpcCoeff files is equal.
!
! CATEGORY:
!       Instrument_Information : SpcCoeff
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:                 Module containing definitions for kinds
!                                   of variable types.
!
!       File_Utility:               Module containing generic file utility
!                                   routines.
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

PROGRAM Test_Equal_SpcCoeff


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler

  USE SpcCoeff_Define
  USE SpcCoeff_netCDF_IO


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------
 
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Test_Equal_SpcCoeff'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Test_Equal_SpcCoeff.f90,v 1.2 2006/05/02 16:58:03 dgroff Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  INTEGER, PARAMETER :: MAX_N_LOOPS  = 10000
  INTEGER, PARAMETER :: INFO_N_LOOPS = 500


  ! ---------
  ! Variables
  ! ---------

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt
  CHARACTER( 256 ) :: Message
  INTEGER :: Error_Status
  INTEGER :: IO_Status
  CHARACTER( 256 ) :: Filename1
  CHARACTER( 256 ) :: Filename2
  INTEGER :: ULP_Scale
  TYPE( SpcCoeff_type ) :: SpcCoeff1
  TYPE( SpcCoeff_type ) :: SpcCoeff2



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, "Program to test if the data in two netCDF SpcCoeff files ", &
             &/5x, "  is equal. ")' )
  WRITE( *, '(/5x, "$Revision: 1.2 $")' )
  WRITE( *, '(/5x, a, / )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                      -- ENTER THE INPUT FILENAMES --                       #
  !#----------------------------------------------------------------------------#

  WRITE( *, FMT     = '( /5x, "Enter the FIRST netCDF SpcCoeff file: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) Filename1
  Filename1 = ADJUSTL( Filename1 )
 
  IF ( .NOT. File_Exists( TRIM( Filename1 ) ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'File '//TRIM( Filename1 )//' not found.', &
                          FAILURE )
    STOP
  END IF
 
  WRITE( *, FMT     = '( /5x, "Enter the SECOND netCDF SpcCoeff file: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) Filename2
  Filename2 = ADJUSTL( Filename2 )
 
  IF ( .NOT. File_Exists( TRIM( Filename2 ) ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'File '//TRIM( Filename2 )//' not found.', &
                          FAILURE )
    STOP
  END IF
 


  !#----------------------------------------------------------------------------#
  !#                         -- ENTER AN ULP VALUE --                           # 
  !#----------------------------------------------------------------------------#

  WRITE( *, FMT     = '( /5x, "Enter an ULP scale value for fp comparisons: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT    = '( i10 )', &
           IOSTAT = IO_Status ) ULP_Scale

  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid input', &
                          FAILURE )
    STOP
  END IF

  ULP_Scale = ABS( ULP_Scale )



  !#----------------------------------------------------------------------------#
  !#                    -- READ THE NETCDF SpcCoeff FILES --                    #
  !#----------------------------------------------------------------------------#

  Error_Status = Read_SpcCoeff_netCDF( Filename1, &
                                       SpcCoeff1 )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading the netCDF SpcCoeff file '//TRIM( Filename1 ), &
                          Error_Status )
    STOP
  END IF


  Error_Status = Read_SpcCoeff_netCDF( Filename2, &
                                       SpcCoeff2 )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading the netCDF SpcCoeff file '//TRIM( Filename2 ), &
                          Error_Status )
    STOP
  END IF


  !#----------------------------------------------------------------------------#
  !#                   -- COMPARE THE SpcCoeff STRUCTURES --                    #
  !#----------------------------------------------------------------------------#

  Error_Status = Equal_SpcCoeff( SpcCoeff1, SpcCoeff2, &
                                 ULP_Scale = ULP_Scale, &
                                 Check_All = 1 )

  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'NetCDF I/O structures are different'
  ELSE
    Message = 'NetCDF I/O structures are equal'
  END IF

  CALL Display_Message( PROGRAM_NAME, &
                        TRIM( Message ), &
                        Error_Status )



  !#----------------------------------------------------------------------------#
  !#                          -- DESTROY THE STRUCTURES --                      #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_SpcCoeff( SpcCoeff1 )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying SpcCoeff1 structure.', &
                          WARNING )
  END IF


  Error_Status = Destroy_SpcCoeff( SpcCoeff2 )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying SpcCoeff2 structure.', &
                          WARNING )
  END IF

END PROGRAM Test_Equal_SpcCoeff


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Test_Equal_SpcCoeff.f90,v 1.2 2006/05/02 16:58:03 dgroff Exp $
!
! $Date: 2006/05/02 16:58:03 $
!
! $Revision: 1.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Test_Equal_SpcCoeff.f90,v $
! Revision 1.2  2006/05/02 16:58:03  dgroff
! *** empty log message ***
!
! Revision 1.1  2004/09/02 16:48:06  paulv
! Initial checkin.
!
!
!
!
