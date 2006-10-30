!------------------------------------------------------------------------------
!M+
! NAME:
!       TauProfile_Binary_IO
!
! PURPOSE:
!       Module containing routines to read and write TauProfile Binary 
!       format files.
!       
! CATEGORY:
!       TauProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE TauProfile_Binary_IO
!
! MODULES:
!       Type_Kinds:            Module containing definitions for kinds
!                              of variable types.
!
!       File_Utility:          Module containing generic file utility routines
!
!       Error_Handler:         Module to define simple error codes and
!                              handle error conditions
!                              USEs: FILE_UTILITY module
!
!       TauProfile_Define:     Module defining the TauProfile data structure and
!                              containing routines to manipulate it.
!                              USEs: TYPE_KINDS module
!                                    ERROR_HANDLER module
!
! CONTAINS:
!       Open_TauProfile_Binary:     Function to open an existing Binary
!                                   format TauProfile file for reading.
!
!       Write_TauProfile_Binary:    Function to write TauProfile data to a
!                                   Binary format TauProfile file.
!
!       Read_TauProfile_Binary:     Function to read TauProfile data from a
!                                   Binary format TauProfile file.
!
!       Write_TauProfile_Binary_README:   Function to write a README file for
!                                         a TauProfile data set.
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
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 05-Aug-2002
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

MODULE TauProfile_Binary_IO


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE File_Utility
  USE Error_Handler

  USE TauProfile_Define


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Open_TauProfile_Binary
  PUBLIC :: Write_TauProfile_Binary
  PUBLIC :: Read_TauProfile_Binary
  PUBLIC :: Write_TauProfile_Binary_README


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: TauProfile_Binary_IO.f90,v 1.6 2004/12/16 18:20:34 paulv Exp $'

  ! -- Keyword set value
  INTEGER,        PRIVATE, PARAMETER :: SET = 1



CONTAINS



!------------------------------------------------------------------------------
!S+
! NAME:
!       Open_TauProfile_Binary
!
! PURPOSE:
!       Function to open a Binary format TauProfile data file.
!
! CATEGORY:
!       TauProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Open_TauProfile_Binary( Filename,                 &  ! Input
!                                              n_Layers,                 &  ! Input
!                                              FileID,                   &  ! Output
!                                              New = New,                &  ! Optional input
!                                              RCS_Id  = RCS_Id,         &  ! Revision control
!                                              Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:       Character string specifying the name of the Binary
!                       TauProfile data file to open.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER( * )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       n_Layers:       The number of layers in the TauProfile data to
!                       be read/written. This value is used to set the
!                       record length for subsequent I/O
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       New:            Set this optional argument to create a new file
!                       for readwrite access.
!                       If NEW = 0, file must already exist and is opened
!                                   for READ access only. [DEFAULT]
!                          NEW = 1, new file is created and file is opened
!                                   for READWRITE access.
!                       If not specified, the file must already exist 
!                       and is opened for READ access only.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN ), OPTIONAL

!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER( * )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       FileID:         File unit ID for subsequent file access.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:         Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER( * )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the file open was successful
!                           == FAILURE an unrecoverable error occurred.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!       File_Exists:        Function to test the existence of a file.
!                           SOURCE: FILE_UTILITY module
!
!       Get_Lun:            Function to obtain an unused file unit number
!                           SOURCE: FILE_UTILITY module
!
!       Display_Message:    Subroutine to output messages
!                           SOURCE: ERROR_HANDLER module
!
! CONTAINS:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None
!
! SIDE EFFECTS:
!       If the NEW argument is set and the file already exists, it is
!       overwritten.
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 05-Aug-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Open_TauProfile_Binary( Filename,      &  ! Input
                                   n_Layers,      &  ! Input
                                   FileID,        &  ! Output
                                   New,           &  ! Optional input
                                   RCS_Id,        &  ! Revision control
                                   Message_Log )  &  ! Error messaging
                                 RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )  :: Filename
    INTEGER,                  INTENT( IN )  :: n_Layers

    ! -- Output
    INTEGER,                  INTENT( OUT ) :: FileID

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )  :: New

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Open_TauProfile_Binary'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    LOGICAL        :: Is_Old
    CHARACTER( 7 ) :: Status
    CHARACTER( 9 ) :: Action

    INTEGER :: IO_Status
    INTEGER :: Record_Length
    INTEGER :: Lun



    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE RETURN VALUES --                       #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS
    FileID       = -1



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                            -- PROCESS INPUT --                           #
    !#--------------------------------------------------------------------------#

    ! -------------------------------
    ! Check the NEW optional argument
    ! -------------------------------

    ! -- Default action is to open an
    ! -- existing file for READ access....
    Is_Old = .TRUE.
    Status = 'OLD    '
    Action = 'READ     '

    ! -- .... unless the NEW argument is set.
    IF ( PRESENT( New ) ) THEN
      IF ( New == SET ) THEN
        Is_Old = .FALSE.
        Status = 'REPLACE'
        Action = 'READWRITE'
      END IF
    END IF


    ! --------------------
    ! Does the file exist?
    ! --------------------

    IF ( Is_Old ) THEN
      IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'File '//TRIM( Filename )//' not found.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF


    ! -------------------------
    ! Is the file already open?
    ! -------------------------

    IF ( File_Open( Filename ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'File '//TRIM( Filename )//' is already open.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ----------------------------
    ! Are the number of layers > 0
    ! ----------------------------

    IF ( n_Layers < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Specified N_LAYERS dimension must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- OPEN THE Binary DATA FILE IN READ/WRITE MODE --           #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! Get a file unit number
    ! ----------------------

    Lun = Get_Lun()

    IF ( Lun < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining FileID number', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -------------
    ! Open the file
    ! -------------

    OPEN( Lun, FILE   = TRIM( Filename ), &
               FORM   = 'UNFORMATTED', &
               ACCESS = 'DIRECT', &
               STATUS = TRIM( Status ), &
               RECL   = n_Layers * n_Bytes_Double, &
               ACTION = TRIM( Action ), &
               IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error opening ", a, &
                        &" for ", a, " access. IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), TRIM( Action ), io_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                               -- DONE --                                 #
    !#--------------------------------------------------------------------------#

    FileID = Lun

  END FUNCTION Open_TauProfile_Binary



!------------------------------------------------------------------------------
!S+
! NAME:
!       Write_TauProfile_Binary
!
! PURPOSE:
!       Function to write TauProfile data to a direct-access, Binary
!       format TauProfile file.
!
! CATEGORY:
!       TauProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Write_TauProfile_Binary( Filename,                 &  ! Input
!                                               FileID,                   &  ! Input
!                                               TauProfile,               &  ! Input
!                                               Quiet       = Quiet,      &  ! Optional input
!                                               RCS_Id      = RCS_Id,     &  ! Revision control
!                                               Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:        Character string specifying the name of the Binary TauProfile
!                        format data file. Used only for message output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       FileID:          Binary file ID number returned from the Create_TauProfile_Binary()
!                        function.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       TauProfile:      Structure containing the transmittance profile data
!                        to be written to the Binary dataset.
!                        UNITS:      N/A
!                        TYPE:       TauProfile_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:           Set this keyword to suppress information messages being
!                        printed to standard output (or the message log file if
!                        the MESSAGE_LOG optional argument is used.) By default,
!                        information messages are printed.
!                        If QUIET = 0, information messages are OUTPUT.
!                           QUIET = 1, information messages are SUPPRESSED.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:          Character string containing the Revision Control
!                        System Id field for the module.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the Binary data write was successful
!                           == FAILURE - the input TauProfile structure is empty,
!                                      - the file is not open,
!                                      - the file is not open for UNFORMATTED,
!                                        DIRECT access,
!                                      - the file record length is inconsistent
!                                        with the TauProfile data, or
!                                      - an unrecoverable WRITE error occurred.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!       Associated_TauProfile:     Function to check the association status of
!                                  the TauProfile pointer components.
!                                  SOURCE: TAUPROFILE_DEFINE module
!
!       Information_TauProfile:    Subroutine to return a string containing
!                                  information about the TauProfile data
!                                  structure.
!                                  SOURCE: TAUPROFILE_DEFINE module
!
!       File_Open:                 Function to test if a file is open.
!                                  SOURCE: FILE_UTILITY module
!
!       Display_Message:           Subroutine to output messages
!                                  SOURCE: ERROR_HANDLER module
!
! CONTAINS:
!       None.
!
! SIDE EFFECTS:
!       If an error occurs, the output file is closed.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Jul-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Write_TauProfile_Binary( Filename,     &  ! Input
                                    FileID,       &  ! Input
                                    TauProfile,   &  ! Input
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
    INTEGER,                  INTENT( IN )  :: FileID
    TYPE( TauProfile_type ),  INTENT( IN )  :: TauProfile

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_TauProfile_Binary'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    LOGICAL :: Noisy

    CHARACTER( 256 ) :: Form_Type
    CHARACTER( 256 ) :: Access_Type
    INTEGER :: Record_Length

    INTEGER :: IO_Status
    INTEGER :: i, j, l, m
    INTEGER :: Record_Number



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                -- CHECK/SET OPTIONAL KEYWORD ARGUMENTS --                #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! Info message output
    ! -------------------

    ! -- Output informational messages....
    Noisy = .TRUE.

    ! -- ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- CHECK STRUCTURE POINTER ASSOCIATION STATUS --             #
    !#                                                                          #
    !#                ALL structure pointers must be associated                 #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. Associated_TauProfile( TauProfile ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT TauProfile pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- CHECK THE FILE --                             #
    !#--------------------------------------------------------------------------#

    ! -----------
    ! Is it open?
    ! -----------

    IF ( .NOT. File_Open( FileID ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'File '//TRIM( Filename )//' is not open.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ----------------------------------
    ! Are the file attributes consistent
    ! with the TauProfile argument?
    ! ----------------------------------

    INQUIRE( UNIT   = FileID, &
             FORM   = Form_Type, &
             ACCESS = Access_Type, &
             RECL   = Record_Length )

    ! -- Test the type of file
    IF ( TRIM( Form_Type )   /= 'UNFORMATTED' .OR. &
         TRIM( Access_Type ) /= 'DIRECT'           ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'File '//TRIM( Filename )//&
                            ' is not connected as a UNFORMATTED, DIRECT access file.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -- Test the record length IN BYTES
    IF ( Record_Length /= ( TauProfile%n_Layers * n_Bytes_Double ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'File '//TRIM( Filename )//&
                            ' record length inconsistent with TauProfile data.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- WRITE THE PROFILE DATA --                      #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Initialise the record counter
    ! -----------------------------

    Record_Number = 0


    ! ---------------------------------------------------
    ! Loop through dimensions IN OUTPUT ORDER for writing
    ! ---------------------------------------------------

    DO j = 1, TauProfile%n_Molecule_Sets
      DO i = 1, TauProfile%n_Angles
        DO m = 1, TauProfile%n_Profiles
          DO l = 1, TauProfile%n_Channels

            ! -- Increment the record counter
            Record_Number = Record_Number + 1

            ! -- Write the current profile to file
            WRITE( FileID, REC    = Record_Number, &
                           IOSTAT = IO_Status      ) TauProfile%Tau( :, l, i, m, j )

            IF ( IO_Status /= 0 ) THEN
              Error_Status = FAILURE
              WRITE( Message, '( "Error writing channel ", i4, ", profile ", i3, &
                                &", angle ", i2, ", absorber# ", i2, &
                                &" transmittance profile to ", a, ". IOSTAT = ", i5 )' ) &
                              l, m, i, j, TRIM( Filename ), IO_Status
              CALL Display_Message( ROUTINE_NAME, &
                                    TRIM( Message ), &
                                    Error_Status, &
                                    Message_Log = Message_Log )
              CLOSE( FileID )
              RETURN
            END IF

          END DO
        END DO
      END DO
    END DO



    !#--------------------------------------------------------------------------#
    !#                      -- OUTPUT AN INFO MESSAGE --                        #
    !#--------------------------------------------------------------------------#

    IF ( Noisy ) THEN
      CALL Information_TauProfile( TauProfile, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Write_TauProfile_Binary





!------------------------------------------------------------------------------
!S+
! NAME:
!       Read_TauProfile_Binary
!
! PURPOSE:
!       Function to read data from a Binary format TauProfile file.
!
! CATEGORY:
!       TauProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Read_TauProfile_Binary( Filename,                 &  ! Input
!                                              FileID,                   &  ! Input
!                                              TauProfile,               &  ! In/Output
!                                              Quiet       = Quiet,      &  ! Optional input
!                                              RCS_Id      = RCS_Id,     &  ! Revision control
!                                              Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:        Character string specifying the name of the Binary
!                        TauProfile format data file. Used only for message
!                        output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       FileID:          Binary file ID number returned from the
!                        Open_TauProfile_Binary() function.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       TauProfile:      On input, structure containing the transmittance
!                        profile dimension data and allocated arrays.
!                        UNITS:      N/A
!                        TYPE:       TauProfile_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:           Set this keyword to suppress information messages being
!                        printed to standard output (or the message log file if
!                        the MESSAGE_LOG optional argument is used.) By default,
!                        information messages are printed.
!                        If QUIET = 0, information messages are OUTPUT.
!                           QUIET = 1, information messages are SUPPRESSED.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       TauProfile:      On output, structure containing the transmittance
!                        profile data read from file.
!                        UNITS:      N/A
!                        TYPE:       TauProfile_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:          Character string containing the Revision Control
!                        System Id field for the module.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the Binary data read was successful
!                           == FAILURE an unrecoverable READ error occurred.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!       File_Open:                  Function to test if a file is open.
!                                   SOURCE: FILE_UTILITY module
!
!       Display_Message:            Subroutine to output messages
!                                   SOURCE: ERROR_HANDLER module
!
!
! SIDE EFFECTS:
!       If an I/O error occurs, the Binary TauProfile file is closed.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 05-Aug-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Read_TauProfile_Binary( Filename,     &  ! Input
                                   FileID,       &  ! Input
                                   TauProfile,   &  ! Output
                                   Quiet,        &  ! Optional input
                                   RCS_Id,       &  ! Optional output
                                   Message_Log ) &  ! Error messaging
                                 RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )     :: Filename
    INTEGER,                  INTENT( IN )     :: FileID

    ! -- Output
    TYPE( TauProfile_type ),  INTENT( IN OUT ) :: TauProfile

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )     :: Quiet

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! -- Error message log file
    CHARACTER( * ), OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_TauProfile_Binary'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message

    LOGICAL :: Noisy

    CHARACTER( 256 ) :: Form_Type
    CHARACTER( 256 ) :: Access_Type
    INTEGER :: Record_Length

    INTEGER :: IO_Status
    INTEGER :: i, j, l, m
    INTEGER :: Record_Number



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                -- CHECK/SET OPTIONAL KEYWORD ARGUMENTS --                #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! Info message output
    ! -------------------

    ! -- Output informational messages....
    Noisy = .TRUE.

    ! -- ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- CHECK STRUCTURE POINTER ASSOCIATION STATUS --             #
    !#                                                                          #
    !# ALL structure pointers must be associated. The structure must be         #
    !# to the required dimensions *before* this call.                           #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. Associated_TauProfile( TauProfile ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'The TauProfile pointer members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- CHECK THE FILE --                             #
    !#--------------------------------------------------------------------------#

    ! -----------
    ! Is it open?
    ! -----------

    IF ( .NOT. File_Open( FileID ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'File '//TRIM( Filename )//' is not open.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ----------------------------------
    ! Are the file attributes consistent
    ! with the TauProfile argument?
    ! ----------------------------------

    INQUIRE( UNIT   = FileID, &
             FORM   = Form_Type, &
             ACCESS = Access_Type, &
             RECL   = Record_Length )

    ! -- Test the type of file
    IF ( TRIM( Form_Type )   /= 'UNFORMATTED' .OR. &
         TRIM( Access_Type ) /= 'DIRECT'           ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'File '//TRIM( Filename )//&
                            ' is not connected as a UNFORMATTED, DIRECT access file.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -- Test the record length IN BYTES
    IF ( Record_Length /= ( TauProfile%n_Layers * n_Bytes_Double ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'File '//TRIM( Filename )//&
                            ' record length inconsistent with TauProfile data.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- READ THE PROFILE DATA --                      #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Initialise the record counter
    ! -----------------------------

    Record_Number = 0


    ! ---------------------------------------------------
    ! Loop through dimensions IN OUTPUT ORDER for reading
    ! ---------------------------------------------------

    DO j = 1, TauProfile%n_Molecule_Sets
      DO i = 1, TauProfile%n_Angles
        DO m = 1, TauProfile%n_Profiles
          DO l = 1, TauProfile%n_Channels

            ! -- Increment the record number
            Record_Number = Record_Number + 1

            ! -- Read a profile
            READ( FileID, REC    = Record_Number, &
                          IOSTAT = IO_Status      ) TauProfile%Tau( :, l, i, m, j )

            IF ( IO_Status /= 0 ) THEN
              Error_Status = FAILURE
              WRITE( Message, '( "Error reading channel ", i4, ", profile ", i3, &
                                &", angle ", i2, ", absorber# ", i2, &
                                &" transmittance profile from ", a, ". IOSTAT = ", i5 )' ) &
                              l, m, i, j, TRIM( Filename ), IO_Status
              CALL Display_Message( ROUTINE_NAME, &
                                    TRIM( Message ), &
                                    Error_Status, &
                                    Message_Log = Message_Log )
              CLOSE( FileID )
              RETURN
            END IF

          END DO
        END DO
      END DO
    END DO



    !#--------------------------------------------------------------------------#
    !#                      -- OUTPUT AN INFO MESSAGE --                        #
    !#--------------------------------------------------------------------------#

    IF ( Noisy ) THEN
      CALL Information_TauProfile( TauProfile, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Read_TauProfile_Binary



!------------------------------------------------------------------------------
!S+
! NAME:
!       Write_TauProfile_Binary_README
!
! PURPOSE:
!       Function to write a README file for the TauProfile data in the
!       Binary format file.
!
! CATEGORY:
!       TauProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Write_TauProfile_Binary_README( Filename,                 &  ! Input
!                                                      TauProfile,               &  ! Input
!                                                      RCS_Id      = RCS_Id,     &  ! Optional output
!                                                      Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:        Character string specifying the name of the Binary
!                        TauProfile format data file. The README file will
!                        be this filename suffixed with ".README"
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       TauProfile:      Structure containing the transmittance profile data
!                        written to the Binary dataset and for which a README
!                        file is needed.
!                        UNITS:      N/A
!                        TYPE:       TauProfile_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:          Character string containing the Revision Control
!                        System Id field for the module.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the Binary data README write was successful
!                           == FAILURE an unrecoverable error occurred.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!       Get_Lun:            Function to obtain an unused file unit number
!                           SOURCE: FILE_UTILITY module
!
!       Display_Message:    Subroutine to output messages
!                           SOURCE: ERROR_HANDLER module
!
! CONTAINS:
!       None.
!
! SIDE EFFECTS:
!       If the output README file exists, it is overwritten.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 05-Aug-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Write_TauProfile_Binary_README( Filename,     &  ! Input
                                           TauProfile,   &  ! Input
                                           RCS_Id,       &  ! Optional output
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
    TYPE( TauProfile_type ),  INTENT( IN )  :: TauProfile

    ! -- Optional output
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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_TauProfile_Binary_README'

    ! -- Taken from Tau_Production_Parameters
    INTEGER, PARAMETER :: N_MOLECULE_SETS = 19
    CHARACTER( * ), PARAMETER, DIMENSION( N_MOLECULE_SETS ) :: MOLECULE_SET_TAG = &
      (/ 'mol1            ', &  !   1
         'mol2            ', &  !   2
         'mol3            ', &  !   3
         'mol4            ', &  !   4
         'mol5            ', &  !   5
         'mol6            ', &  !   6
         'mol7            ', &  !   7
         'all_nocontinuum ', &  !   8
         'continua_only   ', &  !   9
         'all_withcontinua', &  !  10
         'wvo             ', &  !  11
         'wet             ', &  !  12
         'dry             ', &  !  13
         'ozo             ', &  !  14
         'wco             ', &  !  15
         'effective_mol1  ', &  ! 101
         'effective_wet   ', &  ! 112
         'effective_dry   ', &  ! 113
         'effective_ozo   ' /)  ! 114

    INTEGER, PARAMETER, DIMENSION( N_MOLECULE_SETS ) :: MOLECULE_SET_TAG_ID = &
      (/  1, &  !  mol1
          2, &  !  mol2
          3, &  !  mol3
          4, &  !  mol4
          5, &  !  mol5
          6, &  !  mol6
          7, &  !  mol7
          8, &  !  all_nocontinuum
          9, &  !  continua_only
         10, &  !  all_withcontinua
         11, &  !  wvo
         12, &  !  wet
         13, &  !  dry
         14, &  !  ozo
         15, &  !  wco
        101, &  !  effective_mol1
        112, &  !  effective_wet
        113, &  !  effective_dry
        114 /)  !  effective_ozo


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    CHARACTER( 16 )  :: Data_Type
    CHARACTER( 10 )  :: Prefix

    INTEGER :: Lun
    INTEGER :: IO_Status

    INTEGER :: j
    INTEGER, DIMENSION(1) :: jidx



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- OPEN THE README FILE --                        #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! Get a file unit number
    ! ----------------------

    Lun = Get_Lun()

    IF ( Lun < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining README file unit number', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -------------
    ! Open the file
    ! -------------

    OPEN( Lun, FILE   = TRIM( Filename )//'.README', &
               FORM   = 'FORMATTED', &
               STATUS = 'REPLACE', &
               IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error opening ", a, " README file. IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- WRITE THE README --                          #
    !#--------------------------------------------------------------------------#

    WRITE( Lun, FMT    = '( "Structure of ", a, " transmittance file", /, &
                         &/,"File type: UNIX FORTRAN unformatted direct access", &
                         &/,"Data type: DOUBLE PRECISION", &
                         &/,"Record length: ", i4, " bytes", &
                         &//,"Transmittances are given for layers 1 through ", i3, &
                         &/,"There are ", i3, " channels (L) , ", i2, &
                           &" atmospheres (K), ", i1, " angles (I), and ", i2, &
                           &" absorbers (J)" )', &
                IOSTAT = IO_Status ) TRIM( Filename ), &
                                     TauProfile%n_Layers * n_Bytes_Double, &
                                     TauProfile%n_Layers, &
                                     TauProfile%n_Channels, &
                                     TauProfile%n_Profiles, &
                                     TauProfile%n_Angles, &
                                     TauProfile%n_Molecule_Sets

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Part 1 to ", a, " README file. IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( Lun )
      RETURN
    END IF


    WRITE( Lun, FMT    = '( 5x,"The angles correspond to: sec(theta) = ", 10( f4.2, :, ", " ) )', &
                IOSTAT = IO_Status ) TauProfile%angle

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Sec(theta) list to ", a, " README file. IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( Lun )
      RETURN
    END IF


    WRITE( Lun, FMT     = '( 5x,"The absorbers are: " )', &
                ADVANCE = 'NO', &
                IOSTAT  = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Absorber list header to ", a, " README file. IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( Lun )
      RETURN
    END IF

    DO j = 1, TauProfile%n_Molecule_Sets

      jidx = MINLOC( ABS( TauProfile%molecule_set( j ) - MOLECULE_SET_TAG_ID ) )

      IF ( TauProfile%molecule_set( j ) - MOLECULE_SET_TAG_ID( jidx(1) ) /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "No match found for molecule set ", i3 )' ) TauProfile%molecule_set( j )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( Lun )
        RETURN
      END IF

      WRITE( Lun, FMT     = '( a, ", " )', &
                  ADVANCE = 'NO', &
                  IOSTAT  = IO_Status ) TRIM( MOLECULE_SET_TAG( jidx(1) ) )

      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error writing Absorber list to ", a, " README file. IOSTAT = ", i5 )' ) &
                        TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( Lun )
        RETURN
      END IF

    END DO


    WRITE( Lun, FMT = '(//,"Structure of the file:",//,"Record no.      Content", &
                        &/,"         1      absorber #1, angle #1, atmosphere #1, channel #1", &
                        &/,"         2      absorber #1, angle #1, atmosphere #1, channel #2", &
                        &/,"         3      absorber #1, angle #1, atmosphere #1, channel #3", &
                        &/,"         .", &
                        &/,"         .", &
                        &/,"         L      absorber #1, angle #1, atmosphere #1, channel #L", &
                        &/,"        L+1     absorber #1, angle #1, atmosphere #2, channel #1", &
                        &/,"         .", &
                        &/,"         .", &
                        &/,"        2*L     absorber #1, angle #1, atmosphere #2, channel #L", &
                        &/,"      (2*L)+1   absorber #1, angle #1, atmosphere #3, channel #1", &
                        &/,"         .", &
                        &/,"         .", &
                        &/,"       (K*L)    absorber #1, angle #1, atmosphere #K, channel #L", &
                        &/,"      (K*L)+1   absorber #1, angle #2, atmosphere #1, channel #1", &
                        &/,"         .", &
                        &/,"         .", &
                        &/,"      (I*K*L)   absorber #1, angle #I, atmosphere #K, channel #L", &
                        &/,"     (I*K*L)+1  absorber #2, angle #1, atmosphere #1, channel #1", &
                        &/,"         .", &
                        &/,"         .", &
                        &/,"     (J*I*K*L)  absorber #J, angle #I, atmosphere #K, channel #L", &
                        &//5x, "This yields a total of ", i10, " records in the file" )', &
                IOSTAT = IO_Status  ) TauProfile%n_Channels * &
                                      TauProfile%n_Profiles * &
                                      TauProfile%n_Angles * &
                                      TauProfile%n_Molecule_Sets

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Part 2 to ", a, " README file. IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( Lun )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                               -- DONE --                                 #
    !#--------------------------------------------------------------------------#

    CLOSE( Lun )

  END FUNCTION Write_TauProfile_Binary_README

END MODULE TauProfile_Binary_IO


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: TauProfile_Binary_IO.f90,v 1.6 2004/12/16 18:20:34 paulv Exp $
!
! $Date: 2004/12/16 18:20:34 $
!
! $Revision: 1.6 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: TauProfile_Binary_IO.f90,v $
! Revision 1.6  2004/12/16 18:20:34  paulv
! - Restored software from Attic.
! - Tidied up the code. Added some more consistency checks.
!
! Revision 1.5  2004/09/14 17:30:44  paulv
! No binary interface for TauProfile files.
!
! Revision 1.4  2004/06/08 18:36:36  paulv
! - Updated function that creates the README file to handle more molecule
!   sets.
!
! Revision 1.3  2002/09/20 16:05:24  paulv
! - Altered the molecule set lists and their outputs to make them consistent
!   with the new definitions.
!
! Revision 1.2  2002/08/06 20:01:15  paulv
! - Changed Open() function to handle both new and old files.
! - Added README file creation function.
!
! Revision 1.1  2002/08/06 14:53:46  paulv
! Initial checkin.
!
!
!
!
