!------------------------------------------------------------------------------
!P+
! NAME:
!       SpcCoeff_NC2BIN
!
! PURPOSE:
!       Program to convert netCDF format SpcCoeff files to the Binary
!       format.
!
! CATEGORY:
!       Instrument Information : SpcCoeff
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
!       SpcCoeff_Define:            Module defining the SpcCoeff data
!                                   structure and containing routines to
!                                   manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!       SpcCoeff_Binary_IO:         Module containing routines to read and
!                                   write Binary format SpcCoeff files.
!                                   USEs: TYPE_KINDS module
!                                         FILE_UTILITY module
!                                         ERROR_HANDLER module
!                                         SPCCOEFF_DEFINE module
!
!       SpcCoeff_netCDF_IO:         Module containing routines to read and
!                                   write netCDF format SpcCoeff files.
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
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       - Input netCDF SpcCoeff data file
!       - Output Binary format SpcCoeff file.
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

PROGRAM SpcCoeff_NC2BIN

  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE String_Utility

  USE SpcCoeff_Define
  USE SpcCoeff_Binary_IO
  USE SpcCoeff_netCDF_IO


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'SpcCoeff_NC2BIN'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: SpcCoeff_NC2BIN.f90,v 1.12 2006/05/02 16:58:03 dgroff Exp $'
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

  CHARACTER( 256 ) :: File_Type_Name

  TYPE( SpcCoeff_Sensor_type ) :: SpcCoeff_Sensor
  TYPE( SpcCoeff_Sensor_type ) :: SpcCoeff_Sensor_Test

  TYPE( SpcCoeff_Spectral_type ) :: SpcCoeff_Spectral
  TYPE( SpcCoeff_Spectral_type ) :: SpcCoeff_Spectral_Test



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to convert netCDF format SpcCoeff files to their ", &
             &/5x, "   binary format.                                         ")' )
  WRITE( *, '(/5x, " $Revision: 1.12 $")' )
  WRITE( *, '(/5x, a, / )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                 -- ENTER THE INPUT AND OUTPUT FILENAMES --                 #
  !#----------------------------------------------------------------------------#

  ! -----------
  ! NetCDF file
  ! -----------

  WRITE( *, FMT     = '( /5x, "Enter the INPUT netCDF SpcCoeff file: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) NC_Filename
  NC_Filename = ADJUSTL( NC_FileNAME )
 
  IF ( .NOT. File_Exists( TRIM( NC_Filename ) ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'File '//TRIM( NC_Filename )//' not found.', &
                          FAILURE )
    STOP
  END IF
 

  ! -----------
  ! Binary file
  ! -----------

  WRITE( *, FMT     = '( /5x, "Enter the OUTPUT Binary SpcCoeff file: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) BIN_Filename
  BIN_Filename = ADJUSTL( BIN_FileNAME )


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
  !#                         -- ENTER THE FILE TYPE --                          #
  !#----------------------------------------------------------------------------#

  WRITE( *, FMT     = '( /5x, "Enter the SpcCoeff file type [SENSOR or SPECTRAL]: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) File_Type_Name
  File_Type_Name = ADJUSTL( File_Type_Name )



  !#----------------------------------------------------------------------------#
  !#                          -- CONVERT THE FILE --                            #
  !#----------------------------------------------------------------------------#

  SELECT CASE( StrUpCase( TRIM( File_Type_Name ) ) )


    
    !#--------------------------------------------------------------------------#
    !#                      -- CONVERT A SENSOR FILE --                         #
    !#--------------------------------------------------------------------------#

    CASE ( 'SENSOR' )


      ! ---------------------------
      ! Read the SENSOR netCDF file
      ! ---------------------------

      WRITE( *, '( /5x, "Reading netCDF SENSOR SpcCoeff data ..." )' )

      Error_Status = Read_SpcCoeff_netCDF( TRIM( NC_Filename ), &
                                           SpcCoeff_Sensor )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error reading netCDF SENSOR SpcCoeff file '//&
                              TRIM( NC_Filename ), &
                              Error_Status )
        STOP
      END IF
       

      ! ----------------------------
      ! Write the SENSOR Binary file
      ! ----------------------------

      WRITE( *, '( /5x, "Writing Binary SENSOR SpcCoeff data ..." )' )

      Error_Status = Write_SpcCoeff_Binary( TRIM( BIN_Filename ), &
                                            SpcCoeff_Sensor )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error writing Binary SENSOR SpcCoeff file '//&
                              TRIM( BIN_Filename ), &
                              Error_Status )
        STOP
      END IF


      ! --------------------------------
      ! Test read the SENSOR Binary file
      ! --------------------------------

      WRITE( *, '( /5x, "Test reading the Binary SENSOR SpcCoeff data file ..." )' )


      Error_Status = Read_SpcCoeff_Binary( TRIM( BIN_Filename ), &
                                           SpcCoeff_Sensor_Test )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error reading Binary SENSOR SpcCoeff file '//&
                              TRIM( BIN_Filename ), &
                              Error_Status )
        STOP
      END IF


      ! -----------------------------------------------
      ! Compare the netCDF and Binary SENSOR structures
      ! -----------------------------------------------

      WRITE( *, '( /5x, "Comparing the netCDF and Binary SENSOR SpcCoeff structures ..." )' )

      Error_Status = Equal_SpcCoeff( SpcCoeff_Sensor_Test, SpcCoeff_Sensor )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Differences found in netCDF and Binary '//&
                              'file SENSOR SpcCoeff structure comparison.', &
                              Error_Status )
      ELSE
        CALL Display_Message( PROGRAM_NAME, &
                              'netCDF and Binary file SENSOR SpcCoeff structures are equal.', &
                              INFORMATION )
      END IF


      ! -----------------------------
      ! Destroy the SENSOR structures
      ! -----------------------------

      Error_Status = Destroy_SpcCoeff( SpcCoeff_Sensor )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error destroying SpcCoeff_Sensor structure.', &
                              WARNING )
      END IF


      Error_Status = Destroy_SpcCoeff( SpcCoeff_Sensor_Test )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error destroying SpcCoeff_Sensor_Test structure.', &
                              WARNING )
      END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CONVERT A SPECTRAL FILE --                       #
    !#--------------------------------------------------------------------------#

    CASE ( 'SPECTRAL' )


      ! -----------------------------
      ! Read the SPECTRAL netCDF file
      ! -----------------------------

      WRITE( *, '( /5x, "Reading netCDF SPECTRAL SpcCoeff data ..." )' )

      Error_Status = Read_SpcCoeff_netCDF( TRIM( NC_Filename ), &
                                           SpcCoeff_Spectral )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error reading netCDF SPECTRAL SpcCoeff file '//&
                              TRIM( NC_Filename ), &
                              Error_Status )
        STOP
      END IF
       

      ! ------------------------------
      ! Write the SPECTRAL Binary file
      ! ------------------------------

      WRITE( *, '( /5x, "Writing Binary SPECTRAL SpcCoeff data ..." )' )

      Error_Status = Write_SpcCoeff_Binary( TRIM( BIN_Filename ), &
                                            SpcCoeff_Spectral )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error writing Binary SPECTRAL SpcCoeff file '//&
                              TRIM( BIN_Filename ), &
                              Error_Status )
        STOP
      END IF


      ! ----------------------------------
      ! Test read the SPECTRAL Binary file
      ! ----------------------------------

      WRITE( *, '( /5x, "Test reading the Binary SPECTRAL SpcCoeff data file ..." )' )


      Error_Status = Read_SpcCoeff_Binary( TRIM( BIN_Filename ), &
                                           SpcCoeff_Spectral_Test )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error reading Binary SPECTRAL SpcCoeff file '//&
                              TRIM( BIN_Filename ), &
                              Error_Status )
        STOP
      END IF


      ! -------------------------------------------------
      ! Compare the netCDF and Binary SPECTRAL structures
      ! -------------------------------------------------

      WRITE( *, '( /5x, "Comparing the netCDF and Binary SPECTRAL SpcCoeff structures ..." )' )

      Error_Status = Equal_SpcCoeff( SpcCoeff_Spectral_Test, SpcCoeff_Spectral )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Differences found in netCDF and Binary '//&
                              'file SPECTRAL SpcCoeff structure comparison.', &
                              Error_Status )
      ELSE
        CALL Display_Message( PROGRAM_NAME, &
                              'netCDF and Binary file SPECTRAL SpcCoeff structures are equal.', &
                              INFORMATION )
      END IF


      ! -------------------------------
      ! Destroy the SPECTRAL structures
      ! -------------------------------

      Error_Status = Destroy_SpcCoeff( SpcCoeff_Spectral )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error destroying SpcCoeff_Spectral structure.', &
                              WARNING )
      END IF


      Error_Status = Destroy_SpcCoeff( SpcCoeff_Spectral_Test )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error destroying SpcCoeff_Spectral_Test structure.', &
                              WARNING )
      END IF



    !#--------------------------------------------------------------------------#
    !#                     -- INVALID FILE TYPE SPECIFIED --                    #
    !#--------------------------------------------------------------------------#

    CASE DEFAULT
      CALL Display_Message( PROGRAM_NAME, &
                            'Invalid file type. Must be SENSOR or SPECTRAL.', &
                            FAILURE )
      STOP

  END SELECT

END PROGRAM SpcCoeff_NC2BIN


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: SpcCoeff_NC2BIN.f90,v 1.12 2006/05/02 16:58:03 dgroff Exp $
!
! $Date: 2006/05/02 16:58:03 $
!
! $Revision: 1.12 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: SpcCoeff_NC2BIN.f90,v $
! Revision 1.12  2006/05/02 16:58:03  dgroff
! *** empty log message ***
!
! Revision 1.11  2005/07/06 00:23:40  paulv
! - Updated to use new SpcCoeff definition, netCDF I/O, and Binary I/O
!   module routines.
!
! Revision 1.10  2004/08/31 22:57:12  paulv
! - Upgraded to Fortran95.
! - Removed structure initialization calls.
!
! Revision 1.9  2004/08/02 16:33:21  paulv
! - Changed SpcCoeff initialization call from Initialize_SpcCoeff() to
!   Init_SpcCoeff().
!
! Revision 1.8  2004/06/25 21:36:44  paulv
! - Removed unused variables from type declarations.
!
! Revision 1.7  2003/11/17 22:31:04  paulv
! - Updated program header documentation delimiters.
!
! Revision 1.6  2003/10/24 18:18:15  paulv
! - Code category changed from
!     NCEP RTM : Coefficients : SpcCoeff
!   to
!     Instrument Information : SpcCoeff
!
! Revision 1.5  2003/07/09 15:24:07  paulv
! - Corrected a documnetation error.
!
! Revision 1.4  2003/06/19 21:47:01  paulv
! - Some minor cosmetic changes.
! - Corrected filename specifications on output statements.
!
! Revision 1.3  2003/02/12 19:25:02  paulv
! - Updated to use new SpcCoeff netCDF I/O module routines.
!
! Revision 1.2  2002/12/26 17:31:20  paulv
! - Added function call Equal_SpcCoeff() to compare netCDF and Binary
!   read SpcCoeff data structures.
!
! Revision 1.1  2002/12/26 13:54:55  paulv
! Initial checkin. Incomplete.
!
!
!
!
