!------------------------------------------------------------------------------
!M+
! NAME:
!       String_Utility
!
! PURPOSE:
!       Module containing string utility routines
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE String_Utility
!
! MODULES:
!       None.
!
! CONTAINS:
!       StrUpCase:    Function to convert an input string to upper case.
!
!       StrLowCase:   Function to convert an input string to lower case.
!
!       StrCompress:  Function to return a copy of an input string
!                     with all internal whitespace (spaces and tabs)
!                     removed.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None known.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Oct-1999
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 1999, 2004 Paul van Delst
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

MODULE String_Utility


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Visibility
  ! ----------

  PRIVATE
  PUBLIC :: StrUpCase
  PUBLIC :: StrLowCase
  PUBLIC :: StrCompress


  ! -----------------
  ! Module parameters
  ! -----------------

  CHARACTER( * ), PRIVATE, PARAMETER :: LOWER_CASE = 'abcdefghijklmnopqrstuvwxyz'
  CHARACTER( * ), PRIVATE, PARAMETER :: UPPER_CASE = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' 


CONTAINS


!------------------------------------------------------------------------------
!S+
! NAME:
!       StrUpCase
!
! PURPOSE:
!       Function to convert an input string to upper case.
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Result = StrUpCase( String )
!
! INPUT ARGUMENTS:
!       String:  Character string to be converted to upper case.
!                UNITS:      N/A
!                TYPE:       CHARACTER( * )
!                DIMENSION:  Scalar
!                ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Result:  The input character string converted to upper case.
!                UNITS:      N/A
!                TYPE:       CHARACTER( LEN(String) )
!                DIMENSION:  Scalar
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! EXAMPLE:
!       string = 'this is a string'
!       WRITE( *, '( a )' ) StrUpCase( string )
!   THIS IS A STRING
!
! PROCEDURE:
!       Figure 3.5B, pg 80, "Upgrading to Fortran 90", by Cooper Redwine,
!       1995 Springer-Verlag, New York.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Oct-1999
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION StrUpCase ( Input_String ) RESULT ( Output_String )

    ! -- Argument and result
    CHARACTER( * ), INTENT( IN )     :: Input_String
    CHARACTER( LEN( Input_String ) ) :: Output_String

    ! -- Local variables
    INTEGER :: i, n


    ! -- Copy input string
    Output_String = Input_String

    ! -- Loop over string elements
    DO i = 1, LEN( Output_String )

      ! -- Find location of letter in lower case constant string
      n = INDEX( LOWER_CASE, Output_String( i:i ) )

      ! -- If current substring is a lower case letter, make it upper case
      IF ( n /= 0 ) Output_String( i:i ) = UPPER_CASE( n:n )

    END DO

  END FUNCTION StrUpCase



!------------------------------------------------------------------------------
!S+
! NAME:
!       StrLowCase
!
! PURPOSE:
!       Function to convert an input string to lower case.
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Result = StrLowCase( String )
!
! INPUT ARGUMENTS:
!       String: Character string to be converted to lower case.
!               UNITS:      N/A
!               TYPE:       CHARACTER( * )
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Result:  The input character string converted to lower case.
!                UNITS:      N/A
!                TYPE:       CHARACTER( LEN(String) )
!                DIMENSION:  Scalar
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! EXAMPLE:
!       string = 'THIS IS A STRING'
!       WRITE( *, '( a )' ) StrLowCase( string )
!   this is a string
!
! PROCEDURE:
!       Figure 3.5B, pg 80, "Upgrading to Fortran 90", by Cooper Redwine,
!       1995 Springer-Verlag, New York.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Oct-1999
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION StrLowCase ( Input_String ) RESULT ( Output_String )

    ! -- Argument and result
    CHARACTER( * ), INTENT( IN )     :: Input_String
    CHARACTER( LEN( Input_String ) ) :: Output_String

    ! -- Local variables
    INTEGER :: i, n


    ! -- Copy input string
    Output_String = Input_String

    ! -- Loop over string elements
    DO i = 1, LEN( Output_String )

      ! -- Find location of letter in upper case constant string
      n = INDEX( UPPER_CASE, Output_String( i:i ) )

      ! -- If current substring is an upper case letter, make it lower case
      IF ( n /= 0 ) Output_String( i:i ) = LOWER_CASE( n:n )

    END DO

  END FUNCTION StrLowCase



!------------------------------------------------------------------------------
!S+
! NAME:
!       StrCompress
!
! PURPOSE:
!       Subroutine to return a copy of an input string with all whitespace
!       (spaces and tabs) removed.
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Result = StrCompress( String,  &  ! Input
!                             n = n    )  ! Optional Output
!
! INPUT ARGUMENTS:
!       String:         Character string to be compressed.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER( * )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n:              Number of useful characters in output string
!                       after compression. From character n+1 -> LEN( Input_String )
!                       the output is padded with blanks.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Result:         Input string with all whitespace removed before the
!                       first non-whitespace character, and from in-between 
!                       non-whitespace characters.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER( LEN(String) )
!                       DIMENSION:  Scalar
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! EXAMPLE:
!       Input_String = '  This is a string with spaces in it.'
!       Output_String = StrCompress( Input_String, n=n )
!       WRITE( *, '( a )' ) '>',Output_String( 1:n ),'<'
!   >Thisisastringwithspacesinit.<
!
!       or
!
!       WRITE( *, '( a )' ) '>',TRIM( Output_String ),'<'
!   >Thisisastringwithspacesinit.<
!
! PROCEDURE:
!       Definitions of a space and a tab character are made for the
!       ASCII collating sequence. Each single character of the input
!       string is checked against these definitions using the IACHAR()
!       intrinsic. If the input string character DOES NOT correspond 
!       to a space or tab, it is not copied to the output string.
!
!       Note that for input that ONLY has spaces or tabs BEFORE the first
!       useful character, the output of this function is the same as the
!       ADJUSTL() instrinsic.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Oct-1999
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION StrCompress( Input_String, n ) RESULT ( Output_String )

    ! -- Arguments
    CHARACTER( * ),    INTENT( IN )  :: Input_String
    INTEGER, OPTIONAL, INTENT( OUT ) :: n

    ! -- Function result
    CHARACTER( LEN( Input_String ) ) :: Output_String

    ! -- Local parameters
    INTEGER, PARAMETER :: IACHAR_SPACE = 32
    INTEGER, PARAMETER :: IACHAR_TAB   = 9

    ! -- Local variables
    INTEGER :: i, j
    INTEGER :: IACHAR_Character

    ! -- Initialise output string
    Output_String = ' '

    ! -- Initialise output string "useful" length counter
    j = 0

    ! -- Loop over string elements
    DO i = 1, LEN( Input_String )

      ! -- Convert the current character to its position
      ! -- in the ASCII collating sequence
      IACHAR_Character = IACHAR( Input_String( i:i ) )

      ! -- If the character is NOT a space ' ' or a tab '->|'
      ! -- copy it to the output string.
      IF ( IACHAR_Character /= IACHAR_SPACE .AND. &
           IACHAR_Character /= IACHAR_TAB         ) THEN
        j = j + 1
        Output_String( j:j ) = Input_String( i:i )
      END IF

    END DO

    ! -- Save the non-whitespace count
    IF ( PRESENT( n ) ) n = j

  END FUNCTION StrCompress

END MODULE String_Utility


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: String_Utility.f90,v 1.6 2004/08/11 23:21:23 paulv Exp $
!
! $Date: 2004/08/11 23:21:23 $
!
! $Revision: 1.6 $
!
! $State: Exp $
!
! $Log: String_Utility.f90,v $
! Revision 1.6  2004/08/11 23:21:23  paulv
! - Updated header documentation
! - Made "n" argument to StrCompress function optional.
!
! Revision 1.5  2002/09/06 21:58:01  paulv
! - Simplified output string initialisation in STRCOMPRESS().
!
! Revision 1.4  2001/12/19 22:23:56  paulv
! - Simplified the STRUPCASE and STRLOWCASE functions. Removed some extraneous
!   variables.
! - Converted STRCOMPRESS to a function. It was too klunky as a subroutine.
!   Also removed variables that were not really required.
!
! Revision 1.3  2000/12/08 21:48:14  paulv
! - Added header documentation.
! - Using ACHAR and IACHAR in STRCOMPRESS to enable tab characters to be
!   removed.
!
!
!
