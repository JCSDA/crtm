!------------------------------------------------------------------------------
!P+
! NAME:
!       Test_String_Utility
!
! PURPOSE:
!       Program to test the routines in the String_Utility module
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       String_Utility:  Module containing string utility routines.
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
!       Written by:     Paul van Delst, CIMSS/SSEC 08-Dec-2000
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2000, 2004 Paul van Delst
!
!P-
!------------------------------------------------------------------------------

PROGRAM Test_String_Utility


  ! ------------
  ! Module usage
  ! ------------

  USE String_Utility


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME   = 'Test_String_Utility'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  ! -- String length
  INTEGER, PARAMETER :: STRLEN = 57


  ! ---------
  ! Variables
  ! ---------

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  ! -- Note: The String initialisation below contains TAB characters
  CHARACTER( STRLEN ) :: String = '	The Quick	 Brown 123 Fox # @ Jumps Over The ! Lazy Dog'
  CHARACTER( STRLEN ) :: String_Save
  CHARACTER( STRLEN ) :: New_String

  INTEGER :: n



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x,a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to test the String_Utility module routines." )' )
  WRITE( *, '(/5x, " $Revision: 1.4 $")' )
  WRITE( *, '( 5x, a )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                        -- TEST THE CASE SWITCHING --                       #
  !#----------------------------------------------------------------------------#

  String_Save = String

  WRITE( *, '( /5x, "Original test string:" )' )
  WRITE( *, '( ">", a, "<" )' ) String

  ! -- Convert string to all uppercase
  String = StrUpCase( String )
  WRITE( *, '( /5x, "Uppercase test string:" )' )
  WRITE( *, '( ">", a, "<" )' ) String

  ! -- Convert string to all lowercase
  String = String_Save
  String = StrLowCase( String )
  WRITE( *, '( /5x, "Lowercase test string:" )' )
  WRITE( *, '( ">", a, "<" )' ) String



  !#----------------------------------------------------------------------------#
  !#                      -- TEST THE STRING COMPRESSION --                     #
  !#----------------------------------------------------------------------------#

  ! -- Compress the original string
  String = String_Save
  String =  StrCompress( String, n = n )
  WRITE( *, '( /5x, "Compressed test string:" )' )
  WRITE( *, '( ">", a, "<", 5x, "Compressed string length = ", i3 )' ) TRIM( String ), n
  WRITE( *, '( " 123456789012345678901234567890123456789012345678901234567890" )' )
  WRITE( *, '( "          1         2         3         4         5         6" )' )

  ! -- Combine the compression and case switching
  WRITE( *, '( /5x, "Compressed uppercase test string:" )' )
  WRITE( *, '( ">", a, "<" )' ) TRIM( StrUpCase( StrCompress( String_Save, n ) ) )
  WRITE( *, '( " 123456789012345678901234567890123456789012345678901234567890" )' )
  WRITE( *, '( "          1         2         3         4         5         6" )' )

  ! -- Compress a string comprised only of blanks
  New_String = StrCompress( '                    ', n = n )
  WRITE( *, '( /5x, "Compressed blank string:" )' )
  WRITE( *, '( ">", a, "<", 5x, "Compressed string length = ", i3 )' ) TRIM( New_String ), n
  WRITE( *, '( " 123456789012345678901234567890123456789012345678901234567890" )' )
  WRITE( *, '( "          1         2         3         4         5         6" )' )

END PROGRAM Test_String_Utility


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2004/11/30 18:57:19 $
!
! $Revision: 1.4 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Test_String_Utility.f90,v $
! Revision 1.4  2004/11/30 18:57:19  paulv
! - Added program header.
!
!
!

