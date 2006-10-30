!------------------------------------------------------------------------------
!P+
! NAME:
!       AtmProfile_NC2BIN
!
! PURPOSE:
!       Program to convert netCDF format AtmProfile files to "Binary"
!       format.
!
! CATEGORY:
!       AtmProfile
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
!       Error_Handler:              Module to define simple error codes and
!                                   handle error conditions
!                                   USEs: FILE_UTILITY module
!
!       AtmProfile_Define:          Module defining the AtmProfile data
!                                   structure and containing routines to
!                                   manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!       AtmProfile_Binary_IO:       Module containing routines to read and
!                                   write AtmProfile Binary format files.
!                                   USEs: TYPE_KINDS module
!                                         FILE_UTILITY module
!                                         ERROR_HANDLER module
!                                         ATMPROFILE_DEFINE module
!
!       AtmProfile_netCDF_IO:       Module containing routines to read and
!                                   write AtmProfile netCDF format files.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         ATMPROFILE_DEFINE module
!                                         NETCDF module
!                                         NETCDF_UTILITY module
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
!       - Input netCDF AtmProfile data file(s)
!       - Output Binary format AtmProfile file.
!
! SIDE EFFECTS:
!       The output file is overwritten if it already exists.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Jul-2002
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

PROGRAM AtmProfile_NC2BIN


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Error_Handler

  USE AtmProfile_Define
  USE AtmProfile_Binary_IO
  USE AtmProfile_netCDF_IO


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'AtmProfile_NC2BIN'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: AtmProfile_NC2BIN.f90,v 2.2 2005/01/03 14:06:22 paulv Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'


  ! ---------
  ! Variables
  ! ---------

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  INTEGER :: Error_Status

  CHARACTER( 256 ) :: NC_Filename
  CHARACTER( 256 ) :: BIN_Filename

  TYPE( AtmProfile_type ) :: AtmProfile
  TYPE( AtmProfile_type ) :: AtmProfile_Test



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to convert netCDF format AtmProfile files to")' )
  WRITE( *, '( 5x, "   Binary format.                                    ")' )
  WRITE( *, '(/5x, " $Revision: 2.2 $")' )
  WRITE( *, '(/5x, a, / )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                 -- ENTER THE INPUT AND OUTPUT FILENAMES --                 #
  !#----------------------------------------------------------------------------#

  ! -----------
  ! NetCDF file
  ! -----------

  WRITE( *, FMT     = '( /5x, "Enter the INPUT netCDF AtmProfile file: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) NC_Filename
  NC_Filename = ADJUSTL( NC_Filename )

  IF ( .NOT. File_Exists( TRIM( NC_Filename ) ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'File '//TRIM( NC_Filename )//' not found.', &
                          FAILURE )
    STOP
  END IF
 

  ! -----------
  ! Binary file
  ! -----------

  WRITE( *, FMT     = '( /5x, "Enter the OUTPUT Binary AtmProfile file: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) BIN_Filename
  BIN_Filename = ADJUSTL( BIN_Filename )


  ! ---------------------------------------------------------
  ! Check that the netCDF file isn't accidentally overwritten
  ! ---------------------------------------------------------

  IF ( TRIM( NC_Filename ) == TRIM( BIN_Filename ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Output filename is the same as the input filename!', &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                   -- READ THE netCDF AtmProfile FILE --                    #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Reading the netCDF AtmProfile file ..." )' )

  Error_Status = Read_AtmProfile_netCDF( TRIM( NC_Filename ), &
                                         AtmProfile )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading the netCDF AtmProfile file '//&
                          TRIM( NC_Filename ), &
                          Error_Status )
    STOP
  END IF


  !#----------------------------------------------------------------------------#
  !#                   -- WRITE THE Binary AtmProfile FILE --                   #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Writing the Binary AtmProfile file ..." )' )

  Error_Status = Write_AtmProfile_Binary( TRIM( BIN_Filename ), &
                                          AtmProfile )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing the Binary AtmProfile file '//&
                          TRIM( BIN_Filename ), &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                   -- TEST READ THE BINARY DATA FILE --                     #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Test reading the Binary AtmProfile data file ..." )' )

  Error_Status = Read_AtmProfile_Binary( TRIM( BIN_fileNAME ), &
                                         AtmProfile_Test )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error reading Binary AtmProfile file '//&
                          TRIM( BIN_fileNAME ), &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#          -- COMPARE THE netCDF AND Binary AtmProfile STRUCTURES --         #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Comparing the netCDF and Binary AtmProfile structures ..." )' )

  Error_Status = Equal_AtmProfile( AtmProfile_Test, AtmProfile )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Differences found in netCDF and Binary '//&
                          'file AtmProfile structure comparison.', &
                          Error_Status )
  ELSE
    CALL display_message( PROGRAM_NAME, &
                          'netCDF and Binary file AtmProfile structures are equal.', &
                          INFORMATION )
  END IF



  !#----------------------------------------------------------------------------#
  !#                   -- DESTROY THE AtmProfile STRUCTURE --                   #
  !#----------------------------------------------------------------------------#

  Error_Status  = Destroy_AtmProfile(  AtmProfile )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying AtmProfile structure', &
                          WARNING )
  END IF

  Error_Status  = Destroy_AtmProfile(  AtmProfile_Test )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying AtmProfile_Test structure', &
                          WARNING )
  END IF

END PROGRAM AtmProfile_NC2BIN


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: AtmProfile_NC2BIN.f90,v 2.2 2005/01/03 14:06:22 paulv Exp $
!
! $Date: 2005/01/03 14:06:22 $
!
! $Revision: 2.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: AtmProfile_NC2BIN.f90,v $
! Revision 2.2  2005/01/03 14:06:22  paulv
! - Corrected document header delimiters
!
! Revision 2.1  2004/12/13 20:58:22  paulv
! - Updated documentation to reflect Fortran90->95 conversion.
!
! Revision 2.0  2004/08/27 14:28:58  paulv
! - New version using updated AtmProfile modules.
!
! Revision 1.3  2002/07/29 16:48:22  paulv
! - Expanded output file read check to include ALL profiles and output the
!   sum of the profile differences.
!
! Revision 1.2  2002/07/28 14:18:52  paulv
! - Added a test read of the output file and comparison of the read profile
!   with the netCDF read profile.
!
! Revision 1.1  2002/07/27 23:41:43  paulv
! Initial checkin.
!
!
!
!
