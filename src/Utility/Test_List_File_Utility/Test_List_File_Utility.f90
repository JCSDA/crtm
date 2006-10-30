!------------------------------------------------------------------------------
!P+
! NAME:
!       Test_List_File_Utility
!
! PURPOSE:
!       Program to test the routines in the List_File_Utility module
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Message_Handler:    Module to define simple error codes and
!                           handle error conditions
!                           USEs: FILE_UTILITY module
!
!       List_File_Utility:  Module containing routines for reading list files,
!                           i.e. ASCII files that contain lists of character
!                           or integer data, one item per line.
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
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 24-Aug-2004
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2004 Paul van Delst
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

PROGRAM Test_List_File_Utility
 

  ! ------------
  ! Module usage
  ! ------------

  USE Message_Handler
  USE List_File_Utility


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME   = 'Test_List_File_Utility'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: Test_List_File_Utility.f90,v 1.4 2006/05/02 16:58:03 dgroff Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  ! -- Test input filenames
  CHARACTER( * ), PARAMETER :: CHARACTER_LIST_FILENAME = 'Character.List_File'
  CHARACTER( * ), PARAMETER ::   INTEGER_LIST_FILENAME = 'Integer.List_File'


  ! ---------
  ! Variables
  ! ---------

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  CHARACTER( 256 ) :: Message

  INTEGER :: Error_Status
  INTEGER :: i, n

  CHARACTER( 80 ) :: Character_Entry
  INTEGER         :: Integer_Entry

  TYPE( Character_List_File_type ) :: Character_List
  TYPE(   Integer_List_File_type ) :: Integer_List



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x,a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to test the List_File_Utility module routines." )' )
  WRITE( *, '(/5x, " $Revision: 1.4 $")' )
  WRITE( *, '( 5x, a )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                 -- TEST THE CHARACTER LIST FILE ROUTINES --                #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Test of Character List File routines. Accessing ", a, "..." )' ) &
            CHARACTER_LIST_FILENAME


  ! ----------------------------
  ! Read the character list file
  ! ----------------------------

  Error_Status = Read_List_File( CHARACTER_LIST_FILENAME, Character_List )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading Character List File '//CHARACTER_LIST_FILENAME, &
                          Error_status )
    STOP
  END IF


  ! -------------------------------
  ! Determine the number of entries
  ! -------------------------------

  n = Get_List_Size( Character_List )


  ! ----------------
  ! Display the data
  ! ----------------

  DO i = 1, n

    Error_Status = Get_List_Entry( Character_List, i, Character_Entry )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error retrieving Character List File entry #", i5 )' ) i
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_status )
      STOP
    END IF

    WRITE( *, '( 10x, "Character entry #", i4, " : >", a, "<" )' ) i, TRIM( Character_Entry )

  END DO



  !#----------------------------------------------------------------------------#
  !#                  -- TEST THE INTEGER LIST FILE ROUTINES --                 #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Test of Integer List File routines. Accessing ", a, "..." )' ) &
            INTEGER_LIST_FILENAME


  ! --------------------------
  ! Read the integer list file
  ! --------------------------

  Error_Status = Read_List_File( INTEGER_LIST_FILENAME, Integer_List )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading Integer List File '//INTEGER_LIST_FILENAME, &
                          Error_status )
    STOP
  END IF


  ! -------------------------------
  ! Determine the number of entries
  ! -------------------------------

  n = Get_List_Size( Integer_List )


  ! ----------------
  ! Display the data
  ! ----------------

  DO i = 1, n

    Error_Status = Get_List_Entry( Integer_List, i, Integer_Entry )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error retrieving Integer List File entry #", i5 )' ) i
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_status )
      STOP
    END IF

    WRITE( *, '( 10x, "Integer entry #", i4, " : ", i5 )' ) i, Integer_Entry

  END DO

END PROGRAM Test_List_File_Utility


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Test_List_File_Utility.f90,v 1.4 2006/05/02 16:58:03 dgroff Exp $
!
! $Date: 2006/05/02 16:58:03 $
!
! $Revision: 1.4 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Test_List_File_Utility.f90,v $
! Revision 1.4  2006/05/02 16:58:03  dgroff
! *** empty log message ***
!
! Revision 1.3  2004/11/30 20:43:30  paulv
! - Cosmetic change only.
!
! Revision 1.2  2004/11/30 18:19:32  paulv
! - Added program header.
!
!
!


