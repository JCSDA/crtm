!------------------------------------------------------------------------------
!
! NAME:
!       Layer_IO_Test
!
! PURPOSE:
!       Program to test the LBLRTM structure manipulation, read and write
!       functions
!
! CATEGORY:
!       LBLRTM : Test
!
! LANGUAGE:
!       Fortran-90
!
! MODULES:
!       type_kinds:         Module containing definitions for kinds
!                           of variable types.
!
!       file_utility:       Module containing generic file utility routines
!
!       error_handler:      Module to define simple error codes and
!                           handle error conditions
!                           USEs: FILE_UTILITY module
!
!       LBLRTM_Parameters:  Module containing shared parameters required
!                           for LBLRTM format file IO
!                           USEs: TYPE_KINDS module
!
!       LBLRTM_Utility:     Module containing some utility routines.
!                           USEs: TYPE_KINDS module
!                                 FILE_UTILITY module
!                                 ERROR_HANDLER module
!                                 LBLRTM_PARAMETERS module
!
!       LBLRTM_Layer_IO:    Module containing routines to read, write, and 
!                           manipulate the LBLRTM layer data structure.
!                           USEs: TYPE_KINDS module
!                                 FILE_UTILITY module
!                                 ERROR_HANDLER module
!                                 LBL_DEFINE module
!                                 LBLRTM_PARAMETERS module
!                                 LBLRTM_UTILITY module
!                                 LBLRTM_FHDR_IO module
!                                 LBLRTM_PHDR_IO module
!                                 LBLRTM_PANEL_IO module
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
!       INPUT:  LBLRTM data file.
!       OUTPUT: LBLRTM-format data file. Resultant file should be identical
!               to the input file.
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

PROGRAM Layer_IO_Test


  ! ------------
  ! Module usage
  ! ------------

  USE type_kinds
  USE file_utility
  USE error_handler

  USE LBLRTM_Parameters
  USE LBLRTM_Utility
  USE LBLRTM_Layer_IO


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Layer_IO_Test'


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: input_filename
  CHARACTER( 256 ) :: output_filename
  CHARACTER( 1 )   :: answer

  INTEGER :: error_status
  INTEGER :: file_type
  INTEGER :: input_fileID, output_fileID
  INTEGER :: eof
  INTEGER :: write_eol
  INTEGER :: l, n
  INTEGER :: layer_count

  REAL( Double ) :: frequency

  TYPE( LBLRTM_Layer_type ) :: LBLRTM_Layer


  ! ----------
  ! Intrinsics
  ! ----------

  INTRINSIC MIN, MAX, &
            TRIM



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  WRITE( *, '(/5x, "************************************************************")' )
  WRITE( *, '( 25x, a )' ) PROGRAM_NAME
  WRITE( *, * )
  WRITE( *, '( 5x, " Program to test the LBLRTM structure manipulation, read,")' )
  WRITE( *, '( 5x, " and write functions.")' )
  WRITE( *, * )
  WRITE( *, '( 5x, " A lot of screen output is generated, so get ready to use")' )
  WRITE( *, '( 5x, " the window scroll bar.")' )
  WRITE( *, * )
  WRITE( *, '( 5x, " $Revision: 1.3 $")' )
  WRITE( *, '( 5x, "************************************************************")' )



  !#----------------------------------------------------------------------------#
  !#                -- INITIALISE THE LBLRTM_LAYER STRUCTURE --                 #
  !#----------------------------------------------------------------------------#

  CALL initialize_LBLRTM_Layer( LBLRTM_Layer )



  !#----------------------------------------------------------------------------#
  !#                         -- GET THE FILE NAMES --                           #
  !#----------------------------------------------------------------------------#

  WRITE( *, FMT = '( /5x, "Enter input LBLRTM file:  " )', ADVANCE = 'NO' )
  READ( *, '( a )' ) input_filename
  IF ( .NOT. file_exists( TRIM( input_filename ) ) ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'File not found', &
                          FAILURE )
    STOP
  END IF

  WRITE( *, FMT = '( /5x, "Enter output LBLRTM file: " )', ADVANCE = 'NO' )
  READ( *, '( a )' ) output_filename



  !#----------------------------------------------------------------------------#
  !#                          -- GET THE FILE TYPE --                           #
  !#----------------------------------------------------------------------------#

  WRITE( *, FMT = '(  /5x, "LBLRTM file type: ", &
                    &/10x, "1) Single panel", &
                    &/10x, "2) Double panel", &
                     &/5x, "Enter selection: "  )', &
            ADVANCE = 'NO' )
  READ( *, * ) file_type
  SELECT CASE ( file_type )
    CASE ( 1 )
      file_type = LBLRTM_SINGLE_PANEL_TYPE
    CASE ( 2 )
      file_type = LBLRTM_DOUBLE_PANEL_TYPE
    CASE DEFAULT
      CALL display_message( PROGRAM_NAME, &
                            'Invalid selection', &
                            FAILURE )
      STOP
  END SELECT


  !#----------------------------------------------------------------------------#
  !#                            -- OPEN THE FILES --                            #
  !#----------------------------------------------------------------------------#

  error_status = open_LBLRTM( TRIM( input_filename ), &
                              input_fileID )
  IF ( error_status /= SUCCESS ) THEN 
    CALL display_message( PROGRAM_NAME, &
                          'Error opening input file', &
                          FAILURE )
    STOP
  END IF

  error_status = open_LBLRTM( TRIM( output_filename ), &
                              output_fileID, &
                              write = 1 )
  IF ( error_status /= SUCCESS ) THEN 
    CALL display_message( PROGRAM_NAME, &
                          'Error opening output file', &
                          FAILURE )
    STOP
  END IF


  !#----------------------------------------------------------------------------#
  !#                    -- READ AND WRITE LAYERS OF DATA --                     #
  !#----------------------------------------------------------------------------#

  ! ------------------------
  ! Initialise layer counter
  ! ------------------------

  layer_count = 0


  ! ---------------------
  ! Open loop over layers
  ! ---------------------

  layer_loop: DO


    ! ----------------------------------
    ! Destroy the LBLRTM_Layer structure
    ! ----------------------------------

    error_status = destroy_LBLRTM_Layer( LBLRTM_Layer )
    IF ( error_status /= SUCCESS ) THEN 
      CALL display_message( PROGRAM_NAME, &
                            'Error destroying LBLRTM_Layer structure', &
                            FAILURE )
      STOP
    END IF


    ! --------------
    ! Read the layer
    ! --------------

    error_status = read_LBLRTM_Layer( input_fileID,      &
                                      file_type,    &
                                      LBLRTM_Layer, &
                                      eof,          &
                                      diagnostic_output = 2 )
    IF ( error_status /= SUCCESS ) THEN 
      CALL display_message( PROGRAM_NAME, &
                            'Error reading layer', &
                            FAILURE )
      STOP
    END IF


    ! -----------------------
    ! Increment layer counter
    ! -----------------------

    layer_count = layer_count + 1


    ! ----------------------------------------------------------------------
    ! Write the layer IF:
    !   Have not reached end-of-file AND this is not the first layer
    !     OR
    !   Have reached end-of-file or end-of-layer AND this is the first layer
    ! ----------------------------------------------------------------------

    IF ( ( eof /= LBLRTM_FILE_PTR_EOF          .AND. layer_count >  1 ) .OR. &
         ( ( eof == LBLRTM_FILE_PTR_EOF .OR. &
             eof == LBLRTM_FILE_PTR_EOL      ) .AND. layer_count == 1 )      ) THEN

      ! -- If end-of-layer, output an EOL marker
      IF ( eof == LBLRTM_FILE_PTR_EOL ) THEN
        write_EOL = 1
      ELSE
        write_EOL = 0
      END IF

      ! -- Write the layer data
      error_status = write_LBLRTM_Layer( output_fileID,      &
                                         LBLRTM_Layer, &
                                         write_eol = write_eol )
      IF ( error_status /= SUCCESS ) THEN
        CALL display_message( PROGRAM_NAME, &
                              'Error writing layer', &
                              FAILURE )
        STOP
      END IF
    END IF


    ! ----------------
    ! Print some stuff
    ! ----------------

    WRITE( *, '( /5x, "Layer count = ", i5, &
                &/5x, "EOF flag    = ", i5, &
                &/5x, "n_points    = ", i10 )' ) &
              layer_count, eof, LBLRTM_Layer%lbl(1)%n_points


    ! ----------------------------
    ! Output some of the spectrum
    ! ----------------------------

    n = 5

    IF ( file_type == LBLRTM_SINGLE_PANEL_TYPE ) THEN
      WRITE( *, '( /5x, "    POINT#    FREQUENCY       PANEL 1" )' )
    ELSE
      WRITE( *, '( /5x, "    POINT#    FREQUENCY       PANEL 1        PANEL 2" )' )
    END IF

    ! -- First n points
    DO l = 1, MIN( LBLRTM_Layer%lbl(1)%n_points, n )

      frequency = LBLRTM_Layer%lbl(1)%begin_frequency + &
                  ( REAL( l-1, Double ) * LBLRTM_Layer%lbl(1)%frequency_interval )

      IF ( file_type == LBLRTM_SINGLE_PANEL_TYPE ) THEN
        WRITE( *, 10 ) l, frequency, LBLRTM_Layer%lbl(1)%spectrum(l)
      ELSE
        WRITE( *, 10 ) l, frequency, LBLRTM_Layer%lbl(1)%spectrum(l), &
                                     LBLRTM_Layer%lbl(2)%spectrum(l)
      END IF

    END DO


    ! -- Last n points
    DO l = MAX( LBLRTM_Layer%lbl(1)%n_points - n, 0 ), LBLRTM_Layer%lbl(1)%n_points

      frequency = LBLRTM_Layer%lbl(1)%begin_frequency + &
                  ( REAL( l-1, Double ) * LBLRTM_Layer%lbl(1)%frequency_interval )

      IF ( file_type == LBLRTM_SINGLE_PANEL_TYPE ) THEN
        WRITE( *, 10 ) l, frequency, LBLRTM_Layer%lbl(1)%spectrum(l)
      ELSE
        WRITE( *, 10 ) l, frequency, LBLRTM_Layer%lbl(1)%spectrum(l), &
                                     LBLRTM_Layer%lbl(2)%spectrum(l)
      END IF

    END DO

    10 FORMAT( 5x, i10, 2x, f12.6, 2( 2x, es13.6, : ) )


    ! -----------
    ! Exit if EOF
    ! -----------

    IF ( eof == LBLRTM_FILE_PTR_EOF ) EXIT layer_loop


    ! -----------------------------
    ! Prompt user for another layer
    ! -----------------------------

    WRITE( *, FMT = '( "Read another layer? [Y(default) or N] : " )', &
              ADVANCE = 'NO' )
    READ( *, '( a )' ) answer
    IF ( TRIM( answer ) == 'N' .OR. TRIM( answer ) == 'n' ) EXIT layer_loop

  END DO layer_loop

END PROGRAM Layer_IO_Test


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2002/04/16 19:51:12 $
!
! $Revision: 1.3 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Layer_IO_Test.f90,v $
! Revision 1.3  2002/04/16 19:51:12  paulv
! - Added multiple panel output for test output.
!
! Revision 1.2  2002/04/16 17:30:06  paulv
! - Added WRITE_LBLRTM_LAYER() function.
! - Added documentation.
!
!
!

