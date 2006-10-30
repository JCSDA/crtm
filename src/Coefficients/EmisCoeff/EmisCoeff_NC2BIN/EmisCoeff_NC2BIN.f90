!------------------------------------------------------------------------------
!P+
! NAME:
!       Spectral_EmisCoeff_NC2BIN
!
! PURPOSE:
!       Program to convert Spectral netCDF format EmisCoeff files to
!       Binary format.
!
! CATEGORY:
!       CRTM : Emissivity : Spectral Emissivity Model
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
!       EmisCoeff_Define:           Module defining the EmisCoeff data
!                                   structure and containing routines to
!                                   manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!       EmisCoeff_Binary_IO:        Module containing routines to read and
!                                   write Binary format EmisCoeff files.
!                                   USEs: TYPE_KINDS module
!                                         FILE_UTILITY module
!                                         ERROR_HANDLER module
!                                         EMISCOEFF_DEFINE module
!
!       EmisCoeff_netCDF_IO:        Module containing routines to read and
!                                   write netCDF format EmisCoeff files.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         EMISCOEFF_DEFINE module
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
!       - Input netCDF Spectral EmisCoeff data file.
!       - Output Binary format Spectral EmisCoeff file.
!
! SIDE EFFECTS:
!       The output file is overwritten if it already exists.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 19-Jul-2005
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2005 Paul van Delst
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

PROGRAM Spectral_EmisCoeff_NC2BIN


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler

  USE EmisCoeff_Define
  USE EmisCoeff_Binary_IO
  USE EmisCoeff_netCDF_IO


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Spectral_EmisCoeff_NC2BIN'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: EmisCoeff_NC2BIN.f90,v 1.2 2006/05/02 14:58:34 dgroff Exp $'
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

  TYPE( EmisCoeff_type ) :: EmisCoeff
  TYPE( EmisCoeff_type ) :: EmisCoeff_Test



  !#----------------------------------------------------------------------------#
  !#                      -- OUTPUT DESCRIPTIVE HEADER --                       #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to convert Spectral netCDF format EmisCoeff files", &
             &/5x, "   to Binary format.")' )
  WRITE( *, '(/5x, " $Revision: 1.2 $")' )
  WRITE( *, '( 5x, a, / )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                 -- ENTER THE INPUT AND OUTPUT FILENAMES --                 #
  !#----------------------------------------------------------------------------#

  ! -----------
  ! NetCDF file
  ! -----------

  WRITE( *, FMT     = '( /5x, "Enter the INPUT netCDF Spectral EmisCoeff file: " )', &
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

  WRITE( *, FMT     = '( /5x, "Enter the OUTPUT Binary Spectral EmisCoeff file: " )', &
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
  !#                     -- READ THE NETCDF EmisCoeff FILE --                   #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Reading netCDF EmisCoeff data ..." )' )

  Error_Status = Read_EmisCoeff_netCDF( TRIM( NC_Filename ), &
                                        EmisCoeff )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading netCDF EmisCoeff file '//&
                          TRIM( NC_Filename ), &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                      -- WRITE THE BINARY DATA FILE --                      #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Writing Binary EmisCoeff data ..." )' )

  Error_Status = Write_EmisCoeff_Binary( TRIM( BIN_Filename ), &
                                         EmisCoeff )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing Binary EmisCoeff file '//&
                          TRIM( BIN_Filename ), &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                   -- TEST READ THE BINARY DATA FILE --                     #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Test reading the Binary EmisCoeff data file ..." )' )


  Error_Status = Read_EmisCoeff_Binary( TRIM( BIN_Filename ), &
                                        EmisCoeff_Test )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading Binary EmisCoeff file '//&
                          TRIM( BIN_Filename ), &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#           -- COMPARE THE netCDF AND Binary EmisCoeff STRUCTURES--          #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Comparing the netCDF and Binary EmisCoeff structures ..." )' )

  Error_Status = Equal_EmisCoeff( EmisCoeff_Test, EmisCoeff )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Differences found in netCDF and Binary '//&
                          'file EmisCoeff structure comparison.', &
                          Error_Status )
  ELSE
    CALL Display_Message( PROGRAM_NAME, &
                          'netCDF and Binary file EmisCoeff structures are equal.', &
                          INFORMATION )
  END IF



  !#----------------------------------------------------------------------------#
  !#                          -- DESTROY THE STRUCTURES --                      #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_EmisCoeff( EmisCoeff )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying EmisCoeff structure.', &
                          WARNING )
  END IF


  Error_Status = Destroy_EmisCoeff( EmisCoeff_Test )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying EmisCoeff_Test structure.', &
                          WARNING )
  END IF

END PROGRAM Spectral_EmisCoeff_NC2BIN


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: EmisCoeff_NC2BIN.f90,v 1.2 2006/05/02 14:58:34 dgroff Exp $
!
! $Date: 2006/05/02 14:58:34 $
!
! $Revision: 1.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: EmisCoeff_NC2BIN.f90,v $
! Revision 1.2  2006/05/02 14:58:34  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.1  2005/07/19 15:18:31  paulv
! Initial checkin.
!
!
!
