!------------------------------------------------------------------------------
!M+
! NAME:
!       Coefficient_Utility
!
! PURPOSE:
!       Module to hold utility data and routines for reading and writing RT
!       model coefficient data
!
! CATEGORY:
!       NCEP RTM
!
! CALLING SEQUENCE:
!       USE Coefficient_Utility
!
! OUTPUTS:
!       None.
!
! MODULES:
!       Type_Kinds:             Module containing data type kind definitions.
!
!       File_Utility:           Module containing global file utility routines
!
!       Message_Handler:        Module to define error codes and handle error
!                               conditions.
!                               USEs: FILE_UTILITY module
!
! CONTAINS:
!       Open_Coefficient_File:  PUBLIC function to open the sequential access
!                               coefficient files.
!
!       Check_Coefficient_File: PRIVATE function to determine if the coefficient
!                               file is in the correct format, endian-wise.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       None known.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       All of the array documentation lists the dimensions by a single letter.
!       Throughout the RTM code these are:
!         I: Array dimension is of I predictors (Istd and Iint are variants).
!         J: Array dimension is of J absorbing species.
!         K: Array dimension is of K atmospheric layers.
!         L: Array dimension is of L spectral channels.
!         M: Array dimension is of M profiles.
!       Not all of these dimensions will appear in every module.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 12-Jun-2000
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2000 Paul van Delst
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

MODULE Coefficient_Utility


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Open_Coefficient_File


  ! -----------------
  ! Module parameters
  ! -----------------

  INTEGER( Long ), PARAMETER, PRIVATE :: MAGIC_NUMBER = 123456789_Long


  ! -----------------
  ! Module intrinsics
  ! -----------------

  INTRINSIC PRESENT, &
            TRIM



CONTAINS


!------------------------------------------------------------------------------
!S+
! NAME:
!       Open_Coefficient_File
!
! PURPOSE:
!       PUBLIC function to open the sequential access coefficient files
!
! CALLING SEQUENCE:
!       result = Open_Coefficient_File( Coefficient_File, &
!                                       File_ID, &
!                                       Message_Log = Message_Log )
!
! INPUT ARGUMENTS:
!       Coefficient_File: Name of the file containing the coefficient data.
!                         UNITS:      None
!                         TYPE:       Character
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:      Character string specifying a filename in which any
!                         Messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output Messages to the screen.
!                         UNITS:      None
!                         TYPE:       Character
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       File_ID:          File logical unit number.
!                         UNITS:      None
!                         TYPE:       Integer
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Result = SUCCESS => file open was successful
!              = FAILURE => error occurred during file open OR
!                           error occurred during file check
!
! CALLS:
!      file_exists:             Function to determine if a named file exists.
!                               SOURCE: File_Utility module
!
!      get_lun:                 Function to return a free logical unit number
!                               for file access.
!                               SOURCE: File_Utility module
!
!      display_message:         Subroutine to output Messages
!                               SOURCE: Message_Handler module
!
!      Check_Coefficient_File:  Function to check the file for the correct
!                               endian-ness.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       None known.
!
! RESTRICTIONS:
!       None.
!
!S-
!------------------------------------------------------------------------------

  FUNCTION Open_Coefficient_File( Coefficient_File, &
                                  File_ID,          &
                                  For_Output,       &
                                  No_Check,         &
                                  Message_Log )     &
                                RESULT( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    CHARACTER( * ),           INTENT( IN )  :: Coefficient_File
    INTEGER,                  INTENT( OUT ) :: File_ID

    INTEGER,        OPTIONAL, INTENT( IN )  :: For_Output
    INTEGER,        OPTIONAL, INTENT( IN )  :: No_Check

    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Open_Coefficient_File'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    LOGICAL :: File_Check
    LOGICAL :: File_Input

    INTEGER :: IO_Status

    CHARACTER( 7 ) :: File_Status
    CHARACTER( 5 ) :: File_Action



    !#--------------------------------------------------------------------------#
    !#                      -- CHECK OPTIONAL ARGUMENTS --                      #
    !#--------------------------------------------------------------------------#

    ! -----------------
    ! File endian-check
    ! -----------------

    ! -- Default action is to check the file...
    File_Check = .TRUE.
    ! -- Unless the No_Check argument is present
    IF ( PRESENT( No_Check ) ) THEN
      File_Check = .FALSE.
    END IF


    ! ------------------------------
    ! Is file to be read or written?
    ! ------------------------------

    ! -- Default action is to READ file
    File_Input = .TRUE.
    ! -- ...unless For_Output keyword is present
    IF ( PRESENT( For_Output ) ) THEN
      File_Input = .FALSE.
      File_Check = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CHECK DATA FILE EXISTENCE --                     #
    !#--------------------------------------------------------------------------#

    IF ( File_Input ) THEN

      ! -- If data file does not exist, return an error
      IF ( .NOT. file_exists( TRIM( Coefficient_File ) ) ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Coefficient file, ", a, " not found." )' ) &
                        TRIM( Coefficient_File )
        CALL display_message( ROUTINE_NAMe, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Set OPEN keywords for reading
      File_Status = 'OLD   '
      File_Action = 'READ '

    ELSE

      ! -- If data file does exist, output a warning Message
      IF ( file_exists( TRIM( Coefficient_File ) ) ) THEN
        WRITE( Message, '( "Coefficient file, ", a, " will be overwritten." )' ) &
                        TRIM( Coefficient_File )
        CALL display_message( ROUTINE_NAMe, &
                              TRIM( Message ), &
                              WARNING, &
                              Message_Log = Message_Log )
      END IF

      ! -- Set OPEN keywords for writing
      File_Status = 'REPLACE'
      File_Action = 'WRITE'

    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- OPEN THE DATA FILE --                          #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! Get a free unit number
    ! ----------------------

    File_ID = get_lun()

    IF ( File_ID < 0 ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error obtaining file unit number for coefficient file '//&
                            TRIM( Coefficient_File ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -------------
    ! Open the file
    ! -------------

    OPEN( File_ID, FILE   = TRIM( Coefficient_File ), &
                   STATUS = TRIM( File_Status ), &
                   ACTION = TRIM( File_Action ), &
                   ACCESS = 'SEQUENTIAL', &
                   FORM   = 'UNFORMATTED', &
                   IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error opening ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Coefficient_File ), IO_Status
      CALL display_message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#        -- IF FILE IS OPENED FOR OUTPUT, WRITE THE MAGIC NUMBER --        #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. File_Input ) THEN

      WRITE( File_ID, IOSTAT = IO_Status ) MAGIC_NUMBER

      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error writing magic number to ", a, ". IOSTAT = ", i5 )' ) &
                        TRIM( Coefficient_File ), IO_Status
        CALL display_message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( File_ID )
        RETURN
      END IF

    END IF


    
    !#--------------------------------------------------------------------------#
    !#             -- CHECK THE COEFFICIENT DATA FILE IF REQUIRED --            #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. File_Check ) THEN
      Error_Status = SUCCESS
      RETURN
    END IF

    Error_Status = Check_Coefficient_File( File_ID, &
                                           Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CLOSE( File_ID )
      WRITE( Message, '( "Error checking ", a, ". File closed." )' ) &
                      TRIM( Coefficient_File )
      CALL display_message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION Open_Coefficient_File



!------------------------------------------------------------------------------
!P+
! NAME:
!       Check_Coefficient_File
!
! PURPOSE:
!       PRIVATE function to determine if the coefficient file is in the correct
!       format, endian-wise.
!
! CALLING SEQUENCE:
!       result = Check_Coefficient_File( File_ID, &
!                                        Message_Log = Message_Log )
!
! INPUT ARGUMENTS:
!       File_ID:          File logical unit number for the open file that is
!                         to be checked.
!                         UNITS:      None
!                         TYPE:       Integer
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:      Character string specifying a filename in which any
!                         Messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output Messages to the screen.
!                         UNITS:      None
!                         TYPE:       Character
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Result = SUCCESS => file check was successful
!              = FAILURE => error occurred reading a file record OR
!                           8- and/or 32-bit integers not supported.
!
! CALLS:
!       display_message:  Subroutine to output Messages
!                         SOURCE: Message_Handler module
!
! CONTAINS:
!       swap_endian_long_integer:  Function to byte-swap a long integer to
!                                  determine if input data byte-swapping may
!                                  be required.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       None known.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       The file is inquired to determine if it exists. If so, it is opened
!       and the endian-ness is checked.
!P-
!------------------------------------------------------------------------------

  FUNCTION Check_Coefficient_File( File_ID,  &
                                   Message_Log ) &
                                 RESULT( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    INTEGER,        INTENT( IN )           :: File_ID
    CHARACTER( * ), INTENT( IN ), OPTIONAL :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Check_Coefficient_File'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: IO_Status

    INTEGER( Long ) :: Magic_Number_Read, Dummy_Long



    !#--------------------------------------------------------------------------#
    !#             -- CHECK THAT THE CURRENT COMPILATION SUPPORTS --            #
    !#             -- 1- AND 4-BYTE INTEGER TYPES                 --            #
    !#--------------------------------------------------------------------------#

    IF ( BIT_SIZE( 1_Long ) /= 32 .OR. &
         BIT_SIZE( 1_Byte ) /=  8      ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            '8- and/or 32-bit integers not supported. '//&
                            'Unable to determine endian-ness', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- READ THE MAGIC NUMBER --                       #
    !#--------------------------------------------------------------------------#

    READ( File_ID, IOSTAT = IO_Status ) Magic_Number_Read

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading file. IOSTAT = ", i5 )' ) &
                      IO_Status
      CALL display_message( ROUTINE_NAME, &
                            Message, &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- COMPARE THE MAGIC NUMBERS --                     #
    !#--------------------------------------------------------------------------#

    IF ( Magic_Number_Read /= MAGIC_NUMBER ) THEN

      ! -- Set the error status
      Error_Status = FAILURE

      ! -- Byte swap the file data.
      Dummy_Long = swap_endian_long_integer( Magic_Number_Read )

      ! -- Check the file data again
      IF ( Dummy_Long /= MAGIC_NUMBER ) THEN
        CALL display_message( ROUTINE_NAME, &
                              'Unrecognised file format. Invalid magic number.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- If we get here then the data does need to be byte-swapped.
      CALL display_message( ROUTINE_NAME, &
                            'Data file needs to be byte-swapped.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    Error_Status = SUCCESS


  CONTAINS


    !#--------------------------------------------------------------------------#
    !#          -- INTERNAL SUBPROGRAM TO BYTE-SWAP A LONG INTEGER --           #
    !#--------------------------------------------------------------------------#

    FUNCTION swap_endian_long_integer ( input ) &
                                      RESULT ( output )


      ! -----------------
      ! Type declarations
      ! -----------------

      ! -- Argument and result
      INTEGER( Long ), INTENT( IN ) :: input
      INTEGER( Long )               :: output


      ! -- Local variables
      INTEGER( Byte ), DIMENSION( n_bytes_for_Long_kind ) :: binput, boutput
      INTEGER( Long )                                     :: linput, loutput
      INTEGER :: i


      ! -------------------------------------------
      ! Equivalence the byte array and long integer
      ! -------------------------------------------

      EQUIVALENCE ( binput,  linput  ), &
                  ( boutput, loutput )


      ! ----------------------------------------------------------------
      ! Loop over the number of bytes for swapping.
      !
      ! Doing it this way is little bit faster (by about a factor of 4)
      ! than using the MVBITS intrinsic ( on the systems tested; Linux
      ! and AIX):
      !
      !  CALL MVBITS( input, 0,  8, output, 24 )  ! Bits  0-7  --> 24-31
      !  CALL MVBITS( input, 8,  8, output, 16 )  ! Bits  8-15 --> 16-23
      !  CALL MVBITS( input, 16, 8, output,  8 )  ! Bits 16-23 -->  8-15
      !  CALL MVBITS( input, 24, 8, output,  0 )  ! Bits 24-31 -->  0-8
      !
      ! but ONLY if the byte swap loop is inline (rather than by calling
      ! a generic byte swap routine with the number of bytes to swap is
      ! passed as an argument.)
      ! ----------------------------------------------------------------

      ! -- Reassign the input argument. Can't
      ! -- equivalence dummy arguments.
      linput = input

      ! -- Loop over the bytes and swap
      DO i = 1, n_bytes_for_Long_kind
        boutput( i ) = binput( n_bytes_for_Long_kind - ( i - 1 ) )
      END DO

      ! -- Assign the output argument. Can't
      ! -- equivalence dummy arguments.
      output = loutput

    END FUNCTION swap_endian_long_integer

  END FUNCTION Check_Coefficient_File

END MODULE Coefficient_Utility


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: coefficient_utility.f90,v 1.12 2006/05/02 14:58:35 dgroff Exp $
!
! $Date: 2006/05/02 14:58:35 $
!
! $Revision: 1.12 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: coefficient_utility.f90,v $
! Revision 1.12  2006/05/02 14:58:35  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.11  2003/05/16 18:20:15  paulv
! - Altered logic in Open() function. Using logical variables rather than
!   checking integer values.
! - Removed module wide message variable. Using local variables instead.
!
! Revision 1.10  2002/07/24 14:54:39  paulv
! - Added use of TRIM intrinsic when using coefficient_file filename variable.
!
! Revision 1.9  2001/10/01 20:28:46  paulv
! - Added "Name" to RCS keyword list.
!
! Revision 1.8  2001/08/16 16:39:54  paulv
! - Updated documentation
!
! Revision 1.7  2001/08/09 20:38:15  paulv
! - Changed magic number visibility attribute to PRIVATE. Ahhh....
! - Moved all the spectral and transmittance coefficient data type and name
!   definitions into their respective modules. Another ahhh.....
! - Added optional FOR_OUTPUT argument to OPEN_COEFFICIENT_FILE function
!   so that the same function can be used to open coefficient files for
!   writing. It also means the magic number write can be done in this module
!   totally encapsulating that functionality in this module only. Double ahhh....
!
! Revision 1.6  2001/08/01 16:43:05  paulv
! - Updated the definitions of data items and types in the transmittance
!   coefficient data file to reflect changes in code. The absorber space
!   levels are no longer calculated during model initialisation, but are
!   precalculated and stored in the transmittance coefficient data file.
!
! Revision 1.5  2001/07/12 16:58:18  paulv
! - Added USE of TYPE_KINDS module at top of this module. Previously it was
!   USEd only in the CHECK_COEFFICIENT_FILE() function.
! - Data file magic number definition now defined at top of module rather
!   than in the CHECK_COEFFICIENT_FILE() function.
! - Definitions for the number, type, and names of items in the transmittance
!   and spectral coefficient files moved from the TRANSMITTANCE_COEFFICIENTS
!   and SPECTRAL COEFFICIENTS module to this one. This was done to allow this
!   module to be used in both reading and writing/reformatting the coefficient
!   data files.
! - Module-wide error Message character string defined.
! - Added NO_CHECK optional argument to the OPEN_COEFFICIENT_FILE() function.
!   This was done to allow the function to be used to open the old format
!   coefficient files for reformatting by not performing a magic number check.
!
! Revision 1.4  2000/08/31 19:36:31  paulv
! - Added documentation delimiters.
! - Updated documentation headers.
!
! Revision 1.3  2000/08/24 15:22:10  paulv
! - File access changed from DIRECT to SEQUENTIAL. Record length argument
!   no longer required by OPEN_COEFFICIENT_FILE and CHECK_COEFFICIENT_FILE
!   subprograms.
! - INQUIRE statement in OPEN_COEFFICIENT_FILE that checks for existence
!   of the file replaced by function FILE_EXISTS in module FILE_UTILITY.
! - CHECK_COEFFICIENT_FILE used to return a WARNING status if either 8- or
!   32-bit integers were not supported. This condition now returns a
!   FAILURE status as the magic number would not be read so any subsequent
!   attempt to read data would either fail or return junk.
! - The name of the SWAP_ENDIAN_FOURBYTE_INTEGER subprogram was changed to
!   SWAP_ENDIAN_LONG_INTEGER to remove any indication of how many bytes are
!   expected for this data type *apart* from the definition of
!   N_BYTES_FOR_LONG_KIND in the TYPE_KINDS module.
! - Updated module and subprogram documentation.
!
! Revision 1.2  2000/08/08 17:05:45  paulv
! Cosmetic changes to highlight parameters in the source by making them
! uppercase.
!
! Revision 1.1  2000/07/12 16:08:10  paulv
! Initial checked in version
!
!
!

