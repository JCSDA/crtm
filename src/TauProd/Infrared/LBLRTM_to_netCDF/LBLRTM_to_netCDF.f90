!------------------------------------------------------------------------------
!
! NAME:
!       LBLRTM_to_netCDF
!
! PURPOSE:
!       Program to read an LBLRTM format transmittance production data file
!       and write it out in netCDF format.
!
!       Note: This is *not* a generic LBLRTM -> netCDF format converter.
!             It is specifically for use with the transmittance production
!             code suite.
!
! CATEGORY:
!       Transmittance Production
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
!       Compare_Float_Numbers:      Module containing routines to perform
!                                   equality and relational comparisons on
!                                   floating point numbers.
!                                   USEs: TYPE_KINDS module
!
!       LBLRTM_Parameters:          Module containing shared parameters
!                                   required for LBLRTM format file IO
!                                   USEs: TYPE_KINDS module
!
!       LBLRTM_Utility:             Module containing some utility routines.
!                                   USEs: TYPE_KINDS module
!                                         FILE_UTILITY module
!                                         ERROR_HANDLER module
!                                         LBLRTM_PARAMETERS module
!
!       LBLRTM_Fhdr_IO:             Module containing routines to read, write,
!                                   and manipulate the LBLRTM file header data
!                                   structure.
!                                   USEs: TYPE_KINDS module
!                                         FILE_UTILITY module
!                                         ERROR_HANDLER module
!                                         LBLRTM_PARAMETERS module
!
!       LBLRTM_Layer_IO:            Module containing routines to read, write,
!                                   and manipulate the LBLRTM layer data
!                                   structure.
!                                   USEs: TYPE_KINDS module
!                                         FILE_UTILITY module
!                                         ERROR_HANDLER module
!                                         LBLRTM_PARAMETERS module
!                                         LBLRTM_UTILITY module
!                                         LBLRTM_FHDR_IO module
!                                         LBLRTM_PHDR_IO module
!                                         LBLRTM_PANEL_IO module
!
!       LBLRTM_netCDF_IO:           Module containing routine to read and write
!                                   netCDF format files of LBLRTM output data.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         NETCDF module
!                                         NETCDF_UTILITY module
!
!       Tau_Production_Parameters:  Module defining parameters used in the
!                                   LBL transmittance production runs
!                                   USEs: TYPE_KINDS module
!                                         LBLRTM_PARAMETERS module
!
!       Tau_Production_Utility:     Module continaing utility routines for the
!                                   LBL transmittance production run codes
!                                   USEs: TYPE_KINDS module
!                                         FILE_UTILITY module
!                                         ERROR_HANDLER module
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
!       Input:  LBLRTM format data file.
!
!       Output: - netCDF format data file.
!               - Completion signal file.
!
! SIDE EFFECTS:
!       Output files are overwritten if they already exist.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 10-Apr-2002
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
!
!------------------------------------------------------------------------------

PROGRAM LBLRTM_to_netCDF


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE Compare_Float_Numbers

  USE LBLRTM_Parameters
  USE LBLRTM_Utility
  USE LBLRTM_Fhdr_IO
  USE LBLRTM_Layer_IO
  USE LBLRTM_netCDF_IO

  USE Tau_Production_Parameters
  USE Tau_Production_Utility



  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'LBLRTM_to_netCDF'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: LBLRTM_to_netCDF.f90,v 2.3 2006/07/26 22:44:02 wd20pd Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  ! -- Direction parameters. Note that the direction flag values
  ! -- are also the index to the direction name array.
  INTEGER, PARAMETER :: DOWNWELLING = 0
  INTEGER, PARAMETER :: UPWELLING   = 1
  CHARACTER( * ), PARAMETER, DIMENSION(0:1) :: DIRECTION_NAME = (/ 'downwelling', &
                                                                   'upwelling  ' /)


  ! ---------
  ! Variables
  ! ---------


  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  CHARACTER( 256 ) :: LBLRTM_Filename
  INTEGER          :: LBLRTM_FileID

  CHARACTER( 256 ) :: NC_Filename

  CHARACTER( 256 ) :: Title
  CHARACTER( 256 ) :: Comment
  CHARACTER( 256 ) :: Id_Tag

  INTEGER :: Error_Status
  INTEGER :: LBLRTM_EOF
  INTEGER :: k, n

  TYPE( LBLRTM_Fhdr_type )  :: LBLRTM_Fhdr
  TYPE( LBLRTM_Layer_type ) :: LBLRTM_Layer

  INTEGER         :: Direction
  REAL( fp_kind ) :: f1
  REAL( fp_kind ) :: f2
  REAL( fp_kind ) :: df

  INTEGER :: fIdx



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to read LBLRTM format data files and write")' )
  WRITE( *, '( 5x, "   netCDF format output files.")' )
  WRITE( *, '(/5x, " $Revision: 2.3 $")' )
  WRITE( *, '( 5x, a, / )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                           -- GET USER INPUT --                             #
  !#----------------------------------------------------------------------------#

  ! -----------------------------
  ! Get the input binary filename
  ! -----------------------------

  WRITE( *, FMT     = '( /5x, "Enter input LBLRTM file:  " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) LBLRTM_Filename
  LBLRTM_Filename = ADJUSTL( LBLRTM_Filename )

  IF ( .NOT. File_Exists( TRIM( LBLRTM_Filename ) ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'File not found', &
                          FAILURE )
    STOP
  END IF

  NC_Filename = TRIM( LBLRTM_Filename )//'.nc'


  ! -----------------------------
  ! Read in the global attributes
  ! -----------------------------

  WRITE( *, '( /5x, "Enter a TITLE global attribute string:" )' )
  READ( *, '( a )' ) Title
  Title = ADJUSTL( Title )

  WRITE( *, '( /5x, "Enter a COMMENT global attribute string:" )' )
  READ( *, '( a )' ) Comment
  Comment = ADJUSTL( Comment )

  WRITE( *, '( /5x, "Enter a PROFILE SET ID_TAG global attribute string:" )' )
  READ( *, '( a )' ) Id_Tag
  ID_Tag = ADJUSTL( ID_Tag )



  !#----------------------------------------------------------------------------#
  !#                  -- OPEN THE LBLRTM FORMAT FILE --                         #
  !#----------------------------------------------------------------------------#

  Error_Status = Open_LBLRTM( TRIM( LBLRTM_Filename ), &
                              LBLRTM_FileID )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error opening '//TRIM( LBLRTM_Filename )//'.', &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#  -- READ THE FIRST LAYER FILE HEADER FOR DIRECTION AND FREQUENCY DATA --   #
  !#----------------------------------------------------------------------------#
 
  Error_Status = Read_LBLRTM_Fhdr( LBLRTM_FileID, &
                                   LBLRTM_Fhdr, &
                                   LBLRTM_EOF )

  IF ( Error_Status /= SUCCESS ) THEN
    WRITE( Message, '( "Error reading layer #1 file header from file ", a )' ) &
                    TRIM( LBLRTM_Filename )
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          FAILURE )
    STOP
  END IF


  ! ---------------------------
  ! Rewind the LBLRTM data file
  ! ---------------------------

  REWIND( LBLRTM_FileID )


  ! -----------------------------
  ! Determine the direction value
  ! -----------------------------

  IF ( LBLRTM_Fhdr%Run_Flags%layr1 == LBLRTM_Fhdr%Run_Flags%nlayr ) THEN
    Direction = UPWELLING
  ELSE
    Direction = DOWNWELLING
  END IF

  CALL Display_Message( PROGRAM_NAME, &
                        'Detected calculation direction for '//&
                        TRIM( LBLRTM_Filename )//' is '//&
                        TRIM( DIRECTION_NAME( Direction ) ), &
                        INFORMATION )


  ! ---------------------------
  ! Assign the frequency values
  ! ---------------------------

  f1 = REAL( LBLRTM_Fhdr%Begin_Frequency,    fp_kind )
  f2 = REAL( LBLRTM_Fhdr%End_Frequency,      fp_kind )
  df = REAL( LBLRTM_Fhdr%Frequency_Interval, fp_kind )


  ! --------------------------------------
  ! Determine the frequency interval index
  ! --------------------------------------

  fIdx = Compute_dF_Index( df )

  ! -- Check the result
  IF ( fIdx < 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Frequency interval mismatch', &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                  -- CREATE THE NETCDF FORMAT FILE --                       #
  !#----------------------------------------------------------------------------#

  Error_Status = Create_LBLRTM_netCDF( NC_Filename,              &
                                       N_FREQUENCIES( fIdx ),    &
                                       N_LAYERS,                 &
                                       Direction,                &
                                       f1,                       &
                                       f2,                       &
                                       df,                       &
                                       Id_Tag  = TRIM( Id_Tag ), &
                                       Title   = TRIM( Title ),  &
                                       History = PROGRAM_RCS_ID, &
                                       Comment = TRIM( Comment ) )

  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error creating '//TRIM( NC_Filename )//'.', &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                    -- READ AND WRITE LAYERS OF DATA --                     #
  !#----------------------------------------------------------------------------#

  WRITE( *, * )


  ! ----------------
  ! Loop over layers
  ! ----------------

  Layer_Loop: DO k = 1, N_LAYERS


    ! --------------
    ! Read the layer
    ! --------------

    Error_Status = Read_LBLRTM_Layer( LBLRTM_FileID,    &
                                      LBLRTM_FILE_TYPE, &
                                      LBLRTM_Layer,     &
                                      LBLRTM_EOF               )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error reading layer #", i3, " from file ", a )' ) &
                      k, TRIM( LBLRTM_Filename )
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF


    ! ---------------------------------------
    ! Write the layer data to the netCDF file
    ! ---------------------------------------

    n = LBLRTM_Layer%n_Points

    IF ( n /= N_FREQUENCIES( fIdx ) ) THEN
      WRITE( Message, '( "Actual number of LBLRTM points, ", i5, &
                        &" is different from expected, ", i5, &
                        &" for layer #", i3, "." )' ) &
                      n, N_FREQUENCIES( fIdx ), k
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF


    Error_Status = Write_LBLRTM_netCDF( NC_Filename, &
                                        k, &
                                        Transmittance = REAL( LBLRTM_Layer%Spectrum(1:n,1), fp_kind ) )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error writing layer #", i3, " to file ", a )' ) &
                      k, TRIM( NC_Filename )
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF


    WRITE( *, '( 5x, "Layer #", i3, " written..." )' ) k


    ! ----------------------------------------------------
    ! Destroy the LBLRTM_Layer structure for the next read
    ! ----------------------------------------------------

    Error_Status = Destroy_LBLRTM_Layer( LBLRTM_Layer )

    IF ( Error_Status /= SUCCESS ) THEN 
      WRITE( Message, '( "Error destroying LBLRTM_Layer structure for layer #", i3, &
                        &"; Input file ", a )' ) k, TRIM( LBLRTM_Filename )
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF

  END DO Layer_Loop



  !#----------------------------------------------------------------------------#
  !#                                -- DONE --                                  #
  !#----------------------------------------------------------------------------#

  CLOSE( LBLRTM_FileID )



  !#----------------------------------------------------------------------------#
  !#         -- CREATE A SIGNAL FILE INDICATING SUCCESSFUL COMPLETION --        #
  !#----------------------------------------------------------------------------#

  Error_Status = Create_Signal_File( TRIM( NC_Filename ) )

END PROGRAM LBLRTM_to_netCDF


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: LBLRTM_to_netCDF.f90,v 2.3 2006/07/26 22:44:02 wd20pd Exp $
!
! $Date: 2006/07/26 22:44:02 $
!
! $Revision: 2.3 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: LBLRTM_to_netCDF.f90,v $
! Revision 2.3  2006/07/26 22:44:02  wd20pd
! Changed EOF variable name to LBLRTM_EOF so as not to clash with the module
! variable EOF from Message_Handler.
!
! Revision 2.2  2006/06/30 16:47:16  dgroff
! Changed "Error_Handler" references to "Message_Handler"
!
! Revision 2.1  2005/05/08 15:37:40  paulv
! - Upgraded to Fortran-95
! - Added USE of Compare_Float_Numbers utility module.
! - Added parameters for up- or downwelling direction.
! - Removed all LBLRTM structure Initialize() subroutine calls.
! - Added call to Compute_dF_Index() to compute the frequency interval
!   index defined in the Tau_Production_Parameters module. In creating the
!   output netCDF file, the number of frequencies parameters is now referenced
!   by the frequency interval index.
!
! Revision 2.0  2003/07/16 16:41:22  paulv
! - New version using the new LBLRTM_netCDF_IO module routines.
!
! Revision 1.6  2002/07/18 16:04:44  paulv
! - Removed use of NC_dataID argument for netCDF write function due to interface
!   change in LBLRTM_netCDF_IO module.
! - Added Id_Tag argument to netCDF create function.
! - Added Id_Tag user input prompt after title and comment attribute request.
!
! Revision 1.5  2002/07/17 20:30:12  paulv
! - Altered RCS Id name to PROGRAM_RCS_ID.
!
! Revision 1.4  2002/06/07 17:42:03  paulv
! - Removed frequency data write to reflect changes in the LBLRTM netCDF IO
!   routines.
! - Updated documentation.
!
! Revision 1.3  2002/05/15 19:23:03  paulv
! - Added signal file output via Tau_Production_Utility module.
!
! Revision 1.2  2002/05/15 17:49:28  paulv
! - Added prompts for TITLE and COMMENT global attributes for writing to
!   the netCDF file.
! - Modified interface to netCDF creation and write functions to reflect
!   changes in LBLRTM_netCDF_IO module.
!
! Revision 1.1  2002/04/26 22:45:01  paulv
! Initial checkin.
!
!
!
!

