!------------------------------------------------------------------------------
!M+
! NAME:
!       AtmProfile_Binary_IO
!
! PURPOSE:
!       Module containing routines to read and write AtmProfile Binary 
!       format files.
!       
! CATEGORY:
!       AtmProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE AtmProfile_Binary_IO
!
! MODULES:
!       Type_Kinds:            Module containing definitions for kinds
!                              of variable types.
!
!       Message_Handler:         Module to define simple error codes and
!                              handle error conditions
!                              USEs: FILE_UTILITY module
!
!       Binary_File_Utility:   Module containing utility routines for "Binary" 
!                              format datafiles.
!                              USEs: TYPE_KINDS module
!                                    FILE_UTILITY module
!                                    ERROR_HANDLER module
!
!       AtmProfile_Define:     Module defining the AtmProfile data structure and
!                              containing routines to manipulate it.
!                              USEs: TYPE_KINDS module
!                                    ERROR_HANDLER module
!
! CONTAINS:
!       Inquire_AtmProfile_Binary:  Function to inquire a Binary format 
!                                   AtmProfile file to obtain information
!                                   about the data dimensions.
!
!       Write_AtmProfile_Binary:    Function to write AtmProfile data to a
!                                   Binary format AtmProfile file.
!
!       Read_AtmProfile_Binary:     Function to read AtmProfile data from a
!                                   Binary format AtmProfile file.
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
!       User specified Binary format AtmProfile data files for both
!       input and output.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Jul-2002
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

MODULE AtmProfile_Binary_IO


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Message_Handler
  USE Binary_File_Utility

  USE AtmProfile_Define


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Inquire_AtmProfile_Binary
  PUBLIC :: Write_AtmProfile_Binary
  PUBLIC :: Read_AtmProfile_Binary


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: AtmProfile_Binary_IO.f90,v 4.1 2006/06/30 16:47:16 dgroff Exp $'

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1


CONTAINS


!------------------------------------------------------------------------------
!S+
! NAME:
!       Inquire_AtmProfile_Binary
!
! PURPOSE:
!       Function to inquire a Binary format AtmProfile file for its
!       dimensions.
!
! CATEGORY:
!       AtmProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_AtmProfile_Binary( Filename,                   &  ! Input
!                                                 n_Layers     = n_Layers,    &  ! Optional Output
!                                                 n_Absorbers  = n_Absorbers, &  ! Optional Output
!                                                 n_Profiles   = n_Profiles,  &  ! Optional Output
!                                                 RCS_Id       = RCS_Id,      &  ! Revision control
!                                                 Message_Log  = Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an AtmProfile
!                     format data file.
!                     UNITS:      None
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      None
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Layers:     The number of atmospheric layers dimension for the
!                     AtmProfile datafile.
!                     UNITS:      None
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       n_Absorbers:  The number of atmospheric absorbers dimension for the
!                     AtmProfile datafile.
!                     UNITS:      None
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       n_Profiles:   The number of atmospheric profiles dimension for the
!                     AtmProfile datafile.
!                     UNITS:      None
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      None
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the Binary file inquiry was successful
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Open_Binary_File:        Function to open Binary format Coeff
!                                data files.
!                                SOURCE: BINARY_FILE_UTILITY module
!
!       Display_Message:         Subroutine to output messages
!                                SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 13-Aug-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Inquire_AtmProfile_Binary( Filename,     &  ! Input
                                      n_Layers,     &  ! Optional Output
                                      n_Absorbers,  &  ! Optional Output
                                      n_Profiles,   &  ! Optional Output
                                      RCS_Id,       &  ! Revision control
                                      Message_Log ) &  ! Error messaging
                                    RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )  :: Filename

    ! -- Optional Output
    INTEGER,        OPTIONAL, INTENT( OUT ) :: n_Layers   
    INTEGER,        OPTIONAL, INTENT( OUT ) :: n_Absorbers
    INTEGER,        OPTIONAL, INTENT( OUT ) :: n_Profiles 

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error Message log file
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Inquire_AtmProfile_Binary'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    INTEGER :: IO_Status
    INTEGER :: FileID

    INTEGER( Long ) :: File_n_Levels
    INTEGER( Long ) :: File_n_Layers   
    INTEGER( Long ) :: File_n_Absorbers
    INTEGER( Long ) :: File_n_Profiles 
 



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS_ID ARGUMENT IF REQUIRED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#              -- OPEN THE BINARY FORMAT AtmProfile DATA FILE --           #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID, &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening AtmProfile file '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- READ THE "FILE HEADER" --                        #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) File_n_Levels, &
                                       File_n_Layers, &   
                                       File_n_Absorbers, &
                                       File_n_Profiles 

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading dimensions from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- ASSIGN THE RETURN ARGUMENTS --                     #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( n_Layers ) ) THEN
      n_Layers = File_n_Layers
    END IF

    IF ( PRESENT( n_Absorbers ) ) THEN
      n_Absorbers = File_n_Absorbers
    END IF

    IF ( PRESENT( n_Profiles ) ) THEN
      n_Profiles = File_n_Profiles
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- CLOSE THE FILE --                            #
    !#--------------------------------------------------------------------------#

    CLOSE( FileID, STATUS = 'KEEP', &
                   IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            WARNING, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION Inquire_AtmProfile_Binary





!------------------------------------------------------------------------------
!S+
! NAME:
!       Write_AtmProfile_Binary
!
! PURPOSE:
!       Function to write Binary format AtmProfile files.
!
! CATEGORY:
!       AtmProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Write_AtmProfile_Binary( Filename,                 &  ! Input
!                                               AtmProfile,               &  ! Input
!                                               Quiet       = Quiet,      &  ! Optional input
!                                               RCS_Id      = RCS_Id,     &  ! Revision control
!                                               Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of the output
!                     AtmProfile Binary format data file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
!       AtmProfile:   Structure containing the atmospheric profile data
!                     to be written to the Binary dataset.
!                     UNITS:      N/A
!                     TYPE:       AtmProfile_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:        Set this keyword to suppress information messages being
!                     printed to standard output (or the Message log file if 
!                     the Message_Log optional argument is used.) By default,
!                     information messages are printed.
!                     If QUIET = 0, information messages are OUTPUT.
!                        QUIET = 1, information messages are SUPPRESSED.
!                     UNITS:      None
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:  Character string specifying a Filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the Binary file write was successful
!                        == FAILURE - the input AtmProfile structure contains
!                                     unassociated pointer members, or
!                                   - a unrecoverable write error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Associated_AtmProfile:   Function to test the association status
!                                of the pointer members of an AtmProfile
!                                structure.
!                                SOURCE: ATMPROFILE_DEFINE module
!
!       Open_Binary_File:        Function to open Binary format Coeff
!                                data files.
!                                SOURCE: BINARY_FILE_UTILITY module
!
!       Display_Message:         Subroutine to output messages
!                                SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       - If the output file already exists, it is overwritten.
!       - If an error occurs *during* the write phase, the output file is deleted
!         before returning to the calling routine.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Jul-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Write_AtmProfile_Binary( Filename,     &  ! Input
                                    AtmProfile,   &  ! Input
                                    Quiet,        &  ! Optional input
                                    RCS_Id,       &  ! Revision control
                                    Message_Log ) &  ! Error messaging
                                  RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )  :: Filename
    TYPE( AtmProfile_type ),  INTENT( IN )  :: AtmProfile

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )  :: Quiet

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error handler message log
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_AtmProfile_Binary'
    CHARACTER( * ), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: FileID



    !#--------------------------------------------------------------------------#
    !#                    -- INITIALISE THE ERROR STATUS --                     #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS_ID ARGUMENT IF REQUIRED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF


    !#--------------------------------------------------------------------------#
    !#                   -- CHECK OPTIONAL KEYWORD ARGUMENTS --                 #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! Info Message output
    ! -------------------

    ! -- Output informational Messages....
    Noisy = .TRUE.
    ! -- ....unless the Quiet keyword is set
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- CHECK STRUCTURE POINTER ASSOCIATION STATUS --             #
    !#                                                                          #
    !#                ALL structure pointers must be associated                 #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. Associated_AtmProfile( AtmProfile ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT AtmProfile pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#



    ! -----------------------------------------
    ! Check the AtmProfile structure dimensions
    ! -----------------------------------------

    IF ( AtmProfile%n_Levels    < 1 .OR. &
         AtmProfile%n_Layers    < 1 .OR. &
         AtmProfile%n_Absorbers < 1 .OR. &
         AtmProfile%n_Profiles  < 1      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Dimensions of AtmProfile structure are < or = 0.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- OPEN THE AtmProfile DATA FILE --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID, &
                                     For_Output  = SET, &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- WRITE THE "FILE HEADER" --                        #
    !#--------------------------------------------------------------------------#

    ! --------------------
    ! Write the dimensions
    ! --------------------

    WRITE( FileID, IOSTAT = IO_Status ) AtmProfile%n_Levels, &   
                                        AtmProfile%n_Layers, &   
                                        AtmProfile%n_Absorbers, &
                                        AtmProfile%n_Profiles 

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing AtmProfile dimensions to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- WRITE THE ABSORBER INFORMATION --                 #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) AtmProfile%Absorber_ID, &
                                        AtmProfile%Absorber_Units_ID, &
                                        AtmProfile%Absorber_Units_Name, &
                                        AtmProfile%Absorber_Units_LBLRTM

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing AtmProfile absorber information to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- WRITE PROFILE DESCRIPTOR DATA --                  #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) AtmProfile%Description, &
                                        AtmProfile%Climatology_Model, &
                                        AtmProfile%DateTime%Year, &
                                        AtmProfile%DateTime%Month, &
                                        AtmProfile%DateTime%Day, &
                                        AtmProfile%DateTime%Hour, &
                                        AtmProfile%Location%Latitude, &       
                                        AtmProfile%Location%Longitude, &       
                                        AtmProfile%Location%Surface_Altitude

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing AtmProfile profile descriptor data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                            -- WRITE LEVEL DATA --                        #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) AtmProfile%Level_Pressure, &
                                        AtmProfile%Level_Temperature, &
                                        AtmProfile%Level_Absorber, &
                                        AtmProfile%Level_Altitude

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing AtmProfile level data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                            -- WRITE LAYER DATA --                        #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) AtmProfile%Layer_Pressure, &
                                        AtmProfile%Layer_Temperature, &
                                        AtmProfile%Layer_Absorber, &
                                        AtmProfile%Layer_Delta_Z

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing AtmProfile layer data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- CLOSE THE FILE --                            #
    !#--------------------------------------------------------------------------#

    CLOSE( FileID, STATUS = 'KEEP',   &
                   IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- OUTPUT AN INFO Message --                        #
    !#--------------------------------------------------------------------------#

    IF ( Noisy ) THEN
      CALL Info_AtmProfile( AtmProfile, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Write_AtmProfile_Binary





!------------------------------------------------------------------------------
!S+
! NAME:
!       Read_AtmProfile_Binary
!
! PURPOSE:
!       Function to read Binary format AtmProfile files.
!
! CATEGORY:
!       AtmProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Read_AtmProfile_Binary( Filename,                 &  ! Input
!                                              AtmProfile,               &  ! Output
!                                              Quiet       = Quiet,      &  ! Optional input
!                                              RCS_Id      = RCS_Id,     &  ! Revision control
!                                              Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of the output
!                     AtmProfile Binary format data file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:        Set this keyword to suppress information messages being
!                     printed to standard output (or the Message log file if 
!                     the Message_Log optional argument is used.) By default,
!                     information messages are printed.
!                     If QUIET = 0, information messages are OUTPUT.
!                        QUIET = 1, information messages are SUPPRESSED.
!                     UNITS:      None
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:  Character string specifying a Filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       AtmProfile:   Structure containing the atmospheric profile data
!                     read from the Binary dataset.
!                     UNITS:      N/A
!                     TYPE:       AtmProfile_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN OUT )
!
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the Binary file write was successful
!                        == FAILURE - the input AtmProfile structure contains
!                                     unassociated pointer members, or
!                                   - a unrecoverable write error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Associated_AtmProfile:   Function to test the association status
!                                of the pointer members of an AtmProfile
!                                structure.
!                                SOURCE: ATMPROFILE_DEFINE module
!
!       Open_Binary_File:        Function to open Binary format Coeff
!                                data files.
!                                SOURCE: BINARY_FILE_UTILITY module
!
!       Display_Message:         Subroutine to output messages
!                                SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output AtmProfile argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined on
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Jul-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Read_AtmProfile_Binary( Filename,     &  ! Input
                                   AtmProfile,   &  ! Output
                                   Quiet,        &  ! Optional input
                                   RCS_Id,       &  ! Revision control
                                   Message_Log ) &  ! Error messaging
                                 RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )     :: Filename

    ! -- Output
    TYPE( AtmProfile_type ),  INTENT( IN OUT ) :: AtmProfile

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )     :: Quiet

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! -- Error handler message log
    CHARACTER( * ), OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_AtmProfile_Binary'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: FileID

    INTEGER( Long ) :: n_Levels
    INTEGER( Long ) :: n_Layers
    INTEGER( Long ) :: n_Absorbers
    INTEGER( Long ) :: n_Profiles



    !#--------------------------------------------------------------------------#
    !#                    -- INITIALISE THE ERROR STATUS --                     #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS_ID ARGUMENT IF REQUIRED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- CHECK OPTIONAL KEYWORD ARGUMENTS --                 #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! Info Message output
    ! -------------------

    ! -- Output informational Messages....
    Noisy = .TRUE.
    ! -- ....unless the Quiet keyword is set
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- OPEN THE AtmProfile DATA FILE --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID, &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- READ THE "FILE HEADER" --                       #
    !#--------------------------------------------------------------------------#

    ! --------------------
    ! Read the dimensions
    ! --------------------

    READ( FileID, IOSTAT = IO_Status ) n_Levels, &   
                                       n_Layers, &   
                                       n_Absorbers, &
                                       n_Profiles 

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading AtmProfile dimensions from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                 -- ALLOCATE THE AtmProfile STRUCTURE --                  #
    !#--------------------------------------------------------------------------#

    Error_Status = Allocate_AtmProfile( n_Layers, &
                                        n_Absorbers, &
                                        n_Profiles, &
                                        AtmProfile, &
                                        Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output AtmProfile structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- READ THE ABSORBER INFORMATION --                  #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) AtmProfile%Absorber_ID, &
                                       AtmProfile%Absorber_Units_ID, &
                                       AtmProfile%Absorber_Units_Name, &
                                       AtmProfile%Absorber_Units_LBLRTM

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading AtmProfile absorber information from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- READ PROFILE DESCRIPTOR DATA --                   #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) AtmProfile%Description, &
                                       AtmProfile%Climatology_Model, &
                                       AtmProfile%DateTime%Year, &
                                       AtmProfile%DateTime%Month, &
                                       AtmProfile%DateTime%Day, &
                                       AtmProfile%DateTime%Hour, &
                                       AtmProfile%Location%Latitude, &       
                                       AtmProfile%Location%Longitude, &       
                                       AtmProfile%Location%Surface_Altitude

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading AtmProfile profile descriptor data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                            -- READ LEVEL DATA --                         #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) AtmProfile%Level_Pressure, &
                                       AtmProfile%Level_Temperature, &
                                       AtmProfile%Level_Absorber, &
                                       AtmProfile%Level_Altitude

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading AtmProfile level data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                            -- READ LAYER DATA --                         #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) AtmProfile%Layer_Pressure, &
                                       AtmProfile%Layer_Temperature, &
                                       AtmProfile%Layer_Absorber, &
                                       AtmProfile%Layer_Delta_Z

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading AtmProfile layer data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- CLOSE THE FILE --                            #
    !#--------------------------------------------------------------------------#

    CLOSE( FileID, STATUS = 'KEEP',   &
                   IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- OUTPUT AN INFO Message --                        #
    !#--------------------------------------------------------------------------#

    IF ( Noisy ) THEN
      CALL Info_AtmProfile( AtmProfile, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Read_AtmProfile_Binary

END MODULE AtmProfile_Binary_IO


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: AtmProfile_Binary_IO.f90,v 4.1 2006/06/30 16:47:16 dgroff Exp $
!
! $Date: 2006/06/30 16:47:16 $
!
! $Revision: 4.1 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: AtmProfile_Binary_IO.f90,v $
! Revision 4.1  2006/06/30 16:47:16  dgroff
! Changed "Error_Handler" references to "Message_Handler"
!
! Revision 4.0  2004/11/02 20:13:02  paulv
! - New versions for modified AtmProfile structure.
!
! Revision 3.1  2004/08/27 17:14:30  paulv
! - Changed the I/O of the DateTime and Location structures to read/write
!   each structure component separately rather than as a structure.
!
! Revision 3.0  2004/08/27 14:32:41  paulv
! - Updated to Fortran95
! - New versions to handle derived type initialisation.
!
! Revision 1.2  2002/07/29 15:26:20  paulv
! - Corrected use of layers vs. levels. AtmProfile allocation requires LAYER
!   input.
!
! Revision 1.1  2002/07/27 13:25:00  paulv
! Initial checkin.
!
!
!
!
