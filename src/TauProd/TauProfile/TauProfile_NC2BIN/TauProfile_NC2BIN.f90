!------------------------------------------------------------------------------
!M+
! NAME:
!       TauProfile_NC2BIN
!
! PURPOSE:
!       Program to convert netCDF format TauProfile files to a simple
!       binary format.
!
! CATEGORY:
!       TauProfile
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:             Module containing definitions for kinds
!                               of variable types.
!
!       File_Utility:           Module containing generic file utility routines
!
!       Error_Handler:          Module to define simple error codes and
!                               handle error conditions
!                               USEs: FILE_UTILITY module
!
!       TauProfile_Define:      Module defining the TauProfile data
!                               structure and containing routines to
!                               manipulate it.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!
!       TauProfile_Binary_IO:   Module containing routines to read and
!                               write TauProfile Binary format files.
!                               USEs: TYPE_KINDS module
!                                     FILE_UTILITY module
!                                     ERROR_HANDLER module
!                                     ATMPROFILE_DEFINE module
!
!       TauProfile_netCDF_IO:   Module containing routines to read and
!                               write TauProfile netCDF format files.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!                                     ATMPROFILE_DEFINE module
!                                     NETCDF module
!                                     NETCDF_UTILITY module
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
!       - Input netCDF TauProfile data file
!       - Output Binary format TauProfile file.
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
!M-
!------------------------------------------------------------------------------

PROGRAM TauProfile_NC2BIN

  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Error_Handler

  USE TauProfile_Define
  USE TauProfile_Binary_IO
  USE TauProfile_netCDF_IO


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'TauProfile_NC2BIN'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: TauProfile_NC2BIN.f90,v 1.6 2004/12/16 18:21:47 paulv Exp $'

  REAL( fp_kind ), PARAMETER :: TOLERANCE = EPSILON( 1.0_fp_kind )


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  INTEGER :: Error_Status
  INTEGER :: IO_Status

  CHARACTER( 256 ) :: NC_Filename
  INTEGER          :: NC_FileID
  CHARACTER( 256 ) :: BIN_Filename
  INTEGER          :: BIN_FileID

  INTEGER :: i, j, k, l, m
  LOGICAL :: Data_OK

  TYPE( TauProfile_type ) :: NC_TauProfile, BIN_TauProfile



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( '**********************************************************' ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, "**********************************************************")' )
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, * )
  WRITE( *, '( 5x, " Program to convert netCDF format TauProfile files to a   ")' )
  WRITE( *, '( 5x, "   simple binary format.                                  ")' )
  WRITE( *, * )
  WRITE( *, '( 5x, " $Revision: 1.6 $")' )
  WRITE( *, '( 5x, "**********************************************************")' )
  WRITE( *, * )



  !#----------------------------------------------------------------------------#
  !#                 -- ENTER THE INPUT AND OUTPUT FilenameS --                 #
  !#----------------------------------------------------------------------------#

  ! -----------
  ! NetCDF file
  ! -----------

  WRITE( *, FMT     = '( /5x, "Enter the INPUT netCDF TauProfile file: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) NC_Filename

 
  ! -----------
  ! Binary file
  ! -----------

  WRITE( *, FMT     = '( /5x, "Enter the OUTPUT Binary TauProfile file: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) BIN_Filename



  !#----------------------------------------------------------------------------#
  !#                   -- READ THE NETCDF TauProfile FILE --                    #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Reading netCDF TauProfile data ..." )' )

  Error_Status = Read_TauProfile_netCDF( NC_Filename, &
                                         NC_TauProfile )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading netCDF TauProfile file '//&
                          TRIM( NC_Filename ), &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                      -- WRITE THE BINARY DATA FILE --                      #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Writing Binary TauProfile data ..." )' )


  ! -------------------------
  ! Open the Binary data file
  ! -------------------------

  Error_Status = Open_TauProfile_Binary( BIN_Filename, &
                                         NC_TauProfile%n_layers, &
                                         BIN_FileID, &
                                         new = 1 )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error opening Binary TauProfile file '//&
                          TRIM( BIN_Filename ), &
                          Error_Status )
    STOP
  END IF


  ! -------------------------
  ! Write the TauProfile data
  ! -------------------------

  Error_Status = Write_TauProfile_Binary( BIN_Filename, &
                                          BIN_FileID, &
                                          NC_TauProfile )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing Binary TauProfile file '//&
                          TRIM( NC_Filename ), &
                          Error_Status )
    STOP
  END IF


  ! ---------------------
  ! Close the output file
  ! ---------------------

  CLOSE( BIN_FileID, STATUS = 'KEEP', &
                     IOSTAT = IO_Status )

  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error closing output Binary file '//&
                          TRIM( BIN_Filename ), &
                          WARNING )
  END IF



  !#----------------------------------------------------------------------------#
  !#                   -- WRITE THE BINARY DATA README FILE --                  #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Writing Binary TauProfile data README file ..." )' )


  Error_Status = Write_TauProfile_Binary_README( BIN_Filename, &
                                                 NC_TauProfile )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing Binary TauProfile README file '//&
                          TRIM( BIN_Filename )//'.README', &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                   -- TEST READ THE BINARY DATA FILE --                     #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Test reading the Binary TauProfile data file ..." )' )


  ! ----------------------------------------
  ! Allocate the structure for the test read
  ! ----------------------------------------

  Error_Status = Allocate_TauProfile( NC_TauProfile%n_layers, &
                                      NC_TauProfile%n_channels, &
                                      NC_TauProfile%n_angles, &
                                      NC_TauProfile%n_profiles, &
                                      NC_TauProfile%n_molecule_sets, &
                                      BIN_TauProfile )
                                      
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating Binary TauProfile structure.', &
                          Error_Status )
    STOP
  END IF


  ! -------------------------
  ! Open the Binary data file
  ! -------------------------

  Error_Status = Open_TauProfile_Binary( BIN_Filename, &
                                         NC_TauProfile%n_layers, &
                                         BIN_FileID )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error opening Binary TauProfile file '//&
                          TRIM( BIN_Filename ), &
                          Error_Status )
    STOP
  END IF


  ! ------------------------
  ! Read the TauProfile data
  ! ------------------------

  Error_Status = Read_TauProfile_Binary( BIN_Filename, &
                                         BIN_FileID, &
                                         BIN_TauProfile )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading Binary TauProfile file '//&
                          TRIM( NC_Filename ), &
                          Error_Status )
    STOP
  END IF


  ! --------------------
  ! Close the input file
  ! --------------------

  CLOSE( BIN_FileID, STATUS = 'KEEP', &
                     IOSTAT = IO_Status )

  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error closing input Binary file '//&
                          TRIM( BIN_Filename ), &
                          WARNING )
  END IF



  !#----------------------------------------------------------------------------#
  !#          -- COMPARE THE netCDF AND Binary TAUPROFILE STRUCTURES--          #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Comparing the netCDF and Binary TauProfile structures ..." )' )


  ! -----------------------
  ! Loop through dimensions 
  ! -----------------------


  DO j = 1, NC_TauProfile%n_molecule_sets
    WRITE( *, '( 10x, "Molecule set #: ", i3 )' ) NC_TauProfile%molecule_set(j)

    Data_OK = .TRUE.

    DO m = 1, NC_TauProfile%n_profiles
      DO i = 1, NC_TauProfile%n_angles
        DO l = 1, NC_TauProfile%n_channels

          IF ( SUM( NC_TauProfile%tau( :, l, i, m, j ) - BIN_TauProfile%tau( :, l, i, m, j ) ) > TOLERANCE ) THEN
            WRITE( *, '( 15x, "Profile #", i2, &
                             &", Angle secant ", f4.2, &
                             &", Channel #", i4, &
                             &" TauProfile difference > TOLERANCE." )' ) &
                      m, NC_TauProfile%angle(i), NC_TauProfile%channel( l )
            Data_OK = .FALSE.
          END IF

        END DO
      END DO
    END DO

    IF ( Data_OK ) THEN
      WRITE( *, '( 15x, "All profiles/angles/channels O.K." )' )
    END IF

  END DO



  !#----------------------------------------------------------------------------#
  !#                          -- DESTROY THE STRUCTURES --                      #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_TauProfile( NC_TauProfile )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying NC_TauProfile structure.', &
                          WARNING )
  END IF


  Error_Status = Destroy_TauProfile( BIN_TauProfile )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying BIN_TauProfile structure.', &
                          WARNING )
  END IF

END PROGRAM TauProfile_NC2BIN


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: TauProfile_NC2BIN.f90,v 1.6 2004/12/16 18:21:47 paulv Exp $
!
! $Date: 2004/12/16 18:21:47 $
!
! $Revision: 1.6 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: TauProfile_NC2BIN.f90,v $
! Revision 1.6  2004/12/16 18:21:47  paulv
! - Converted to Fortran-95
! - Removed structure initialisation calls.
!
! Revision 1.5  2004/09/14 17:29:08  paulv
! No binary interface for TauProfile files anymore.
!
! Revision 1.4  2004/06/08 18:45:24  paulv
! - Removed netCDF OPEN and CLOSE functions. No longer required.
! - Removed the NC_FileID argument from the netCDF READ function. No
!   longer required.
!
! Revision 1.3  2002/09/20 16:03:09  paulv
! - Added in error check after call to the ALLOCATE_TAUPROFILE() function to
!   allocate space for the BIN_TauProfile structure.
!
! Revision 1.2  2002/08/06 20:00:10  paulv
! - Added test read of Binary data file and comparison of data with the
!   netCDF file data.
!
! Revision 1.1  2002/08/06 14:54:11  paulv
! Initial checkin.
!
!
!
!
