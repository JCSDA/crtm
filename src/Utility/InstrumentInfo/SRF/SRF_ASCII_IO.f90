!------------------------------------------------------------------------------
!M+
! NAME:
!       SRF_ASCII_IO
!
! PURPOSE:
!       Module containing routines to read and write ASCII format
!       SRF data files.
!       
! CATEGORY:
!       Instrument Information : SRF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE SRF_Define
!
! MODULES:
!       Type_Kinds:            Module containing definitions for kinds
!                              of variable types.
!
!       File_Utility:          Module containing generic file utility routines
!
!       Message_Handler:       Module to define simple error codes and
!                              handle error conditions
!                              USEs: FILE_UTILITY module
!
!       String_Utility         Module containing string utility routines
!
!       SRF_Define:            Module defining the SRF data structure and
!                              containing routines to manipulate it.
!                              USEs: TYPE_KINDS module
!                                    Message_Handler module
!                                    INTEGRATE module
!
! CONTAINS:
!       Read_SRF_ASCII_Header:   Function to read the header from an ASCII format
!                                SRF data file.
!
!       Write_SRF_ASCII_Header:  Function to write the header to an ASCII format
!                                SRF data file.
!
!       Read_SRF_ASCII:          Function to read an SRF from an ASCII format
!                                SRF data file.
!
!       Write_SRF_ASCII:         Function to write an SRF to an ASCII format
!                                SRF data file.
!
! INCLUDE FILES:
!      None.
!
! EXTERNALS:
!      None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!      None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Aug-2003
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2001 Paul van Delst
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

MODULE SRF_ASCII_IO


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE String_Utility

  USE SRF_Define


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  ! -- Everything private by default
  PRIVATE

  ! -- Public procedures
  PUBLIC :: Read_SRF_ASCII_Header
  PUBLIC :: Write_SRF_ASCII_Header
  PUBLIC :: Read_SRF_ASCII
  PUBLIC :: Write_SRF_ASCII


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id field
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id$'

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: UNSET = 0
  INTEGER, PRIVATE, PARAMETER ::   SET = 1

  ! -- Header field delimiter
  CHARACTER( * ), PRIVATE, PARAMETER :: DELIMITER = ':'

  ! -- Maximum number of channels in an ASCII SRF file
  INTEGER, PRIVATE, PARAMETER :: MAX_N_CHANNELS = 99

  ! -- The data formats *for writing only*
  CHARACTER( * ), PRIVATE, PARAMETER :: SRF_HEADER_FORMAT = '( 2(1x,i5), 2(1x,f12.6) )'
  CHARACTER( * ), PRIVATE, PARAMETER :: RESPONSE_FORMAT   = '( 8(1x,f9.6) )'


  ! ------------------------
  ! PUBLIC Module parameters
  ! ------------------------

  ! -- Maximum number of channels in an ASCII SRF file
  INTEGER, PUBLIC, PARAMETER :: MAX_N_SRF_CHANNELS = MAX_N_CHANNELS


CONTAINS





!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PUBLIC MODULE ROUTINES ##                        ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!------------------------------------------------------------------------------
!S+
! NAME:
!       Read_SRF_ASCII_Header
!
! PURPOSE:
!       Function to read the header information from an ASCII SRF data file.
!
! CATEGORY:
!       Instrument Information: SRF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Read_SRF_ASCII_Header( Filename,                 &  ! Input
!                                             FileID,                   &  ! In/Output
!                                             n_Channels,               &  ! Output
!                                             Channel_List,             &  ! Output
!                                             Title,                    &  ! Output
!                                             History,                  &  ! Output
!                                             Sensor_Name,              &  ! Output
!                                             Platform_Name,            &  ! Output
!                                             Comment,                  &  ! Output
!                                             Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:         Character string specifying the name of the
!                         ASCII format SRF data file to read.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       FileID:           ASCII file ID number. If file is not already opened,
!                         it is opened and this argument contains the unit
!                         number for the opened file upon output.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:      Character string specifying a filename in which
!                         any messages will be logged. If not specified,
!                         or if an error occurs opening the log file, the
!                         default action is to output messages to standard
!                         output.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       n_Channels:       The number of channels of data in the ASCII SRF file
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT )
!
!       Channel_List:     The list of channel numbers for which there is data
!                         in the ASCII SRF file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Rank-1, n_Channels or greater
!                         ATTRIBUTES: INTENT( OUT )
!
!       Title:            Character string written into the TITLE 
!                         attribute field of the ASCII SRF file.
!                         Should contain a succinct one-line description
!                         of what is in the datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT )
!
!       History:          Character string written into the HISTORY 
!                         attribute field of the ASCII SRF file.
!                         Should contain a succinct one-line description
!                         of the datafile history.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT )
!
!       Sensor_Name:      Character string written into the SENSOR_NAME
!                         global attribute field of the ASCII SRF
!                         file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT )
!
!       Platform_Name:    Character string written into the PLATFORM_NAME
!                         global attribute field of the ASCII SRF
!                         file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT )
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the ASCII SRF file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error
!                         status. The error codes are defined in the
!                         Message_Handler module.
!                         If == SUCCESS the header read was successful
!                            == FAILURE an error occurred
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
! CALLS:
!       File_Open:          Function to check if a file is open.
!                           SOURCE: FILE_UTILITY module
!
!       StrUpCase:          Function to convert an input character string
!                           to upper case.
!                           SOURCE: STRING_PROCESSING module
!
!       Display_Message:    Subroutine to output messages
!                           SOURCE: Message_Handler module
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
!       If an error occurs, the ASCII file is closed before exit.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Aug-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Read_SRF_ASCII_Header( Filename,      &  ! Input
                                  FileID,        &  ! In/Output
                                  n_Channels,    &  ! Output
                                  Channel_List,  &  ! Output
                                  Title,         &  ! Output
                                  History,       &  ! Output
                                  Sensor_Name,   &  ! Output
                                  Platform_Name, &  ! Output
                                  Comment,       &  ! Output
                                  Message_Log )  &  ! Error messaging
                                RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )     :: Filename
    INTEGER,                  INTENT( IN OUT ) :: FileID

    ! -- Output
    INTEGER,                  INTENT( OUT )    :: n_Channels
    INTEGER,    DIMENSION(:), INTENT( OUT )    :: Channel_List
    CHARACTER( * ),           INTENT( OUT )    :: Title
    CHARACTER( * ),           INTENT( OUT )    :: History
    CHARACTER( * ),           INTENT( OUT )    :: Sensor_Name
    CHARACTER( * ),           INTENT( OUT )    :: Platform_Name
    CHARACTER( * ),           INTENT( OUT )    :: Comment

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_SRF_ASCII_Header'

    ! -- The number of attributes to read. Title, History,
    ! -- Sensor_Name, Platform_Name, and Comment
    INTEGER, PARAMETER :: N_ATTRIBUTES = 5


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: IO_Status
    CHARACTER( 2000 ) :: Buffer
    INTEGER :: i, l, m, n
    CHARACTER( 256 ) :: Attribute_Name
    CHARACTER( 2000 ) :: Attribute_Value




    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                     -- OPEN THE FILE IF REQUIRED --                      #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. File_Open( TRIM( Filename ) ) ) THEN


      ! ----------------------
      ! Get a file unit number
      ! ----------------------

      FileID = Get_Lun()

      IF ( FileID < 0 ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error obtianing file unit number', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF


      ! -------------
      ! Open the file
      ! -------------

      OPEN( FileID, FILE   = TRIM( Filename ), &
                    STATUS = 'OLD', &
                    FORM   = 'FORMATTED', &
                    ACCESS = 'SEQUENTIAL', &
                    ACTION = 'READ', &
                    IOSTAT = IO_Status )

      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error opening ASCII SRF file ", a, ". IOSTAT = ", i5 )' ) &
                        TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF


    ELSE


      ! ----------------------------------
      ! File is already open, so rewind it
      ! ----------------------------------

      REWIND( FileID )

    END IF



    !#--------------------------------------------------------------------------#
    !#          -- READ THE NUMBER OF CHANNELS AND THE CHANNEL LIST --          #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! The number of channels
    ! ----------------------

    READ( FileID, FMT    = '( i5 )', &
                  IOSTAT = IO_Status ) n_Channels

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading number of channels from ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ----------------------------------------
    ! Check the size of the channel list array
    ! ----------------------------------------

    IF ( SIZE( Channel_List ) < n_Channels ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "CHANNEL_LIST argument not large enough to hold ", i5, " channels." )' ) &
                      n_Channels
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ---------------------
    ! Read the channel list
    ! ---------------------

    DO l = 1, n_Channels

      READ( FileID, FMT    = '( i5 )', &
                    IOSTAT = IO_Status ) Channel_List(l)

      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error reading channel index ", i5, " from ", a, ". IOSTAT = ", i5 )' ) &
                        l, TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF

    END DO



    !#--------------------------------------------------------------------------#
    !#                        -- READ THE SRF ATTRIBUTES --                     #
    !#--------------------------------------------------------------------------#

    Attribute_Read_Loop: DO i = 1, N_ATTRIBUTES


      ! --------------------
      ! Read a line of input
      ! --------------------

      READ( FileID, FMT = '( a )', IOSTAT = IO_Status ) Buffer

      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error reading attribute #", i1, " from ", a, ". IOSTAT = ", i5 )' ) &
                        i, TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF


      ! ------------------------------------
      ! Extract the attribute name and value
      ! ------------------------------------

      ! -- Get the string length and ensure it's not empty
      n = LEN_TRIM( Buffer )

      IF ( n == 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Attribute #", i1, " from ", a, " is empty." )' ) &
                        i, TRIM( Filename )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF

      ! -- Search for the delimiter and check
      m = SCAN( Buffer, DELIMITER )

      IF ( m  == 0 .OR. &        ! No delimiter
           m  == 1 .OR. &        ! No attribute name
           m+1 > n      ) THEN   ! No attribute value
        Error_Status = FAILURE
        WRITE( Message, '( "Attribute #", i1, " from ", a, " delimiter scan failed." )' ) &
                        i, TRIM( Filename )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF

      ! -- Extract the attribute name and value
      Attribute_Name  = ADJUSTL( StrUpCase( Buffer(1:m-1) ) )
      Attribute_Value = Buffer(m+1:n)


      ! ------------------------------------------------
      ! Match the attribute by name and save
      ! ------------------------------------------------

      SELECT CASE ( TRIM( Attribute_Name ) )

        ! -- The TITLE
        CASE( 'TITLE' )
          Title = TRIM( Attribute_Value )

        ! -- The HISTORY
        CASE( 'HISTORY' )
          History = TRIM( Attribute_Value )

        ! -- The SENSOR_NAME
        CASE( 'SENSOR_NAME' )
          Sensor_Name = TRIM( Attribute_Value )

        ! -- The PLATFORM_NAME
        CASE( 'PLATFORM_NAME' )
          Platform_Name = TRIM( Attribute_Value )

        ! -- The COMMENT
        CASE( 'COMMENT' )
          Comment = TRIM( Attribute_Value )

        ! -- Unrecognised attribute
        CASE DEFAULT
          Error_Status = FAILURE
          CALL Display_Message( ROUTINE_NAME, &
                                'Unrecognised attribute name: '//TRIM( Attribute_Name ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          CLOSE( FileID )
          RETURN

      END SELECT

    END DO Attribute_Read_Loop

  END FUNCTION Read_SRF_ASCII_Header




!------------------------------------------------------------------------------
!S+
! NAME:
!       Write_SRF_ASCII_Header
!
! PURPOSE:
!       Function to write the header information to an ASCII SRF data file.
!
! CATEGORY:
!       Instrument Information: SRF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Write_SRF_ASCII_Header( Filename,                 &  ! Input
!                                              FileID,                   &  ! In/Output
!                                              n_Channels,               &  ! Input
!                                              Channel_List,             &  ! Input
!                                              Title,                    &  ! Input
!                                              History,                  &  ! Input
!                                              Sensor_Name,              &  ! Input
!                                              Platform_Name,            &  ! Input
!                                              Comment,                  &  ! Input
!                                              Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:         Character string specifying the name of the
!                         ASCII format SRF data file to read.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       FileID:           ASCII file ID number. If file is not already opened,
!                         it is opened and this argument contains the unit
!                         number for the opened file upon output.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN OUT )
!
!       n_Channels:       The number of channels of data to be written to
!                         the ASCII SRF file
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       Channel_List:     The list of channel numbers to be written to 
!                         the ASCII SRF file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Rank-1, n_Channels or greater
!                         ATTRIBUTES: INTENT( IN )
!
!       Title:            Character string written into the TITLE 
!                         attribute field of the ASCII SRF file.
!                         Should contain a succinct one-line description
!                         of what is in the datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       History:          Character string written into the HISTORY 
!                         attribute field of the ASCII SRF file.
!                         Should contain a succinct one-line description
!                         of the datafile history.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       Sensor_Name:      Character string written into the SENSOR_NAME
!                         global attribute field of the ASCII SRF
!                         file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       Platform_Name:    Character string written into the PLATFORM_NAME
!                         global attribute field of the ASCII SRF
!                         file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the ASCII SRF file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:      Character string specifying a filename in which
!                         any messages will be logged. If not specified,
!                         or if an error occurs opening the log file, the
!                         default action is to output messages to standard
!                         output.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
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
!       Error_Status:     The return value is an integer defining the error
!                         status. The error codes are defined in the
!                         Message_Handler module.
!                         If == SUCCESS the header write was successful
!                            == FAILURE an error occurred
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
! CALLS:
!       File_Open:          Function to check if a file is open.
!                           SOURCE: FILE_UTILITY module
!
!       Display_Message:    Subroutine to output messages
!                           SOURCE: Message_Handler module
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
!       If an error occurs, the ASCII file is closed.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Aug-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Write_SRF_ASCII_Header( Filename,      &  ! Input
                                   FileID,        &  ! In/Output
                                   n_Channels,    &  ! Input
                                   Channel_List,  &  ! Input
                                   Title,         &  ! Input
                                   History,       &  ! Input
                                   Sensor_Name,   &  ! Input
                                   Platform_Name, &  ! Input
                                   Comment,       &  ! Input
                                   Message_Log )  &  ! Error messaging
                                 RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )     :: Filename
    INTEGER,                  INTENT( IN OUT ) :: FileID

    INTEGER,                  INTENT( IN )     :: n_Channels
    INTEGER,    DIMENSION(:), INTENT( IN )     :: Channel_List
    CHARACTER( * ),           INTENT( IN )     :: Title
    CHARACTER( * ),           INTENT( IN )     :: History
    CHARACTER( * ),           INTENT( IN )     :: Sensor_Name
    CHARACTER( * ),           INTENT( IN )     :: Platform_Name
    CHARACTER( * ),           INTENT( IN )     :: Comment

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_SRF_ASCII_Header'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: IO_Status
    CHARACTER( 80 ) :: Action
    INTEGER :: l

 


    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                     -- OPEN THE FILE IF REQUIRED --                      #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. File_Open( TRIM( Filename ) ) ) THEN


      ! ----------------------
      ! Get a file unit number
      ! ----------------------

      FileID = Get_Lun()

      IF ( FileID < 0 ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error obtaining file unit number', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF


      ! -------------
      ! Open the file
      ! -------------

      OPEN( FileID, FILE   = TRIM( Filename ), &
                    STATUS = 'REPLACE', &
                    FORM   = 'FORMATTED', &
                    ACCESS = 'SEQUENTIAL', &
                    ACTION = 'WRITE', &
                    IOSTAT = IO_Status )

      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error opening ASCII SRF file ", a, ". IOSTAT = ", i5 )' ) &
                        TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF


    ELSE


      ! ----------------------------------
      ! File is already open, so rewind it
      ! ----------------------------------

      REWIND( FileID )


      ! -------------------------------------
      ! Check that the file can be written to
      ! -------------------------------------

      INQUIRE( UNIT = FileID, ACTION = Action )

      IF ( INDEX( Action, 'WRITE' ) == 0 ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'ASCII SRF file '//TRIM( Filename )//' is not opened for writing.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN

      END IF

    END IF


    !#--------------------------------------------------------------------------#
    !#                    -- CHECK CHANNEL DIMENSIONALITY --                    #
    !#--------------------------------------------------------------------------#

    IF ( SIZE( Channel_List ) < n_Channels ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "CHANNEL_LIST argument smaller than N_CHANNELS dimension: ", i5, "." )' ) &
                      n_Channels
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- WRITE THE CHANNEL INFORMATION --                    #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! The number of channels
    ! ----------------------

    WRITE( FileID, FMT    = '( i3 )', &
                   IOSTAT = IO_Status ) n_Channels

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing number of channels to ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ----------------
    ! The channel list
    ! ----------------

    DO l = 1, n_Channels

      WRITE( FileID, FMT    = '( i5 )', &
                     IOSTAT = IO_Status ) Channel_List(l)

      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error writing channel index ", i5, " to ", a, ". IOSTAT = ", i5 )' ) &
                        l, TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF

    END DO



    !#--------------------------------------------------------------------------#
    !#                       -- WRITE THE SRF ATTRIBUTES --                     #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! The TITLE
    ! ---------

    WRITE( FileID, FMT    = '( "Title:", a )', &
                   IOSTAT = IO_Status          ) TRIM( Title )

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing TITLE attribute to ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -----------
    ! The HISTORY
    ! -----------

    WRITE( FileID, FMT    = '( "History:", a )', &
                   IOSTAT = IO_Status            ) TRIM( History )

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing HISTORY attribute to ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ---------------
    ! The SENSOR_NAME
    ! ---------------

    WRITE( FileID, FMT    = '( "Sensor_Name:", a )', &
                   IOSTAT = IO_Status                ) TRIM( Sensor_Name )

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing SENSOR_NAME attribute to ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -----------------
    ! The PLATFORM_NAME
    ! -----------------

    WRITE( FileID, FMT    = '( "Platform_Name:", a )', &
                   IOSTAT = IO_Status                  ) TRIM( Platform_Name )

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing PLATFORM_NAME attribute to ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -----------
    ! The COMMENT
    ! -----------

    WRITE( FileID, FMT    = '( "Comment:", a )', &
                   IOSTAT = IO_Status            ) TRIM( Comment )

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing COMMENT attribute to ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

  END FUNCTION Write_SRF_ASCII_Header





!------------------------------------------------------------------------------
!S+
! NAME:
!       Read_SRF_ASCII
!
! PURPOSE:
!       Function to read a channel SRF from an ASCII format SRF file.
!
! CATEGORY:
!       Instrument Information: SRF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Read_SRF_ASCII( Filename,                   &  ! Input
!                                      FileID,                     &  ! Input
!                                      Channel,                    &  ! Input
!                                      SRF,                        &  ! Output
!                                      Quiet         = Quiet       &  !  Optional Input
!                                      RCS_Id        = RCS_Id,     &  ! Revision control
!                                      Message_Log   = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:         Character string specifying the name of the
!                         ASCII format SRF data file to inquire.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       FileID:           ASCII file ID number.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       Channel:          Channel number for which the SRF response and frequency
!                         are required.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:            Use this argument to suppress information messages being
!                         printed to standard output (or the message log file if
!                         the MESSAGE_LOG optional argument is used.) By default,
!                         information messages are printed.
!                         If QUIET = 0, information messages are OUTPUT (default)
!                            QUIET = 1, information messages are *SUPPRESSED*.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!       Message_Log:      Character string specifying a filename in which any
!                         messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output messages to standard output.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       SRF:              Data structure containing the requested channel SRF data.
!                         UNITS:      N/A
!                         TYPE:       SRF_type
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:           Character string containing the Revision Control
!                         System Id field for the module.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error
!                         status. The error codes are defined in the
!                         Message_Handler module.
!                         If == SUCCESS the file read was successful
!                            == FAILURE an error occurred reading the file
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
! CALLS:
!       File_Open:          Function to check if a file is open.
!                           SOURCE: FILE_UTILITY module
!
!       Get_Lun:            Function to return an unused file unit number.
!                           SOURCE: FILE_UTILITY module
!
!       Display_Message:    Subroutine to output messages
!                           SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       File is closed if an error occurs.
!
! RESTRICTIONS:
!       The SRF header must be read before this function can be called.
!
! COMMENTS:
!       Note the INTENT on the output SRF argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined on
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Aug-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Read_SRF_ASCII( Filename,     &  ! Input
                           FileID,       &  ! Input
                           Channel,      &  ! Input
                           SRF,          &  ! Output
                           Quiet,        &  ! Optional input
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
    CHARACTER( * ),           INTENT( IN )     :: Filename
    INTEGER,                  INTENT( IN )     :: FileID
    INTEGER,                  INTENT( IN )     :: Channel

    ! -- Output
    TYPE( SRF_type ),         INTENT( IN OUT ) :: SRF

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_SRF_ASCII'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message

    LOGICAL :: Noisy

    INTEGER :: IO_Status

    INTEGER :: ch
    INTEGER :: n_Points, i
    REAL( fp_kind ) :: f1, f2
    REAL( fp_kind ) :: dummy



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- CHECK INPUT --                               #
    !#--------------------------------------------------------------------------#

    ! -----------------
    ! Is the file open?
    ! -----------------

    IF ( .NOT. File_Open( FileID ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'ASCII SRF file '//TRIM( Filename )//' is not open.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ------------------
    ! Optional arguments
    ! ------------------

    ! -- Output information messages...
    Noisy = .TRUE.
    ! -- ...unles the QUIET keyword is set
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- BEGIN LOOP OVER CHANNEL READ --                    #
    !#--------------------------------------------------------------------------#

    Channel_Skip_Loop: DO


      ! -----------------------------------
      ! Read the current channel SRF header
      ! -----------------------------------

      READ( FileID, FMT = *, IOSTAT = IO_Status ) ch, n_Points, f1, f2

      ! -- End of file? EOF is most likely to occur
      ! -- here if there is a channel number mixup
      IF ( IO_Status < 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "End of file reached reading SRF header from ", a, ". IOSTAT = ", i5 )' ) &
                        TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF

      ! -- Error
      IF ( IO_Status > 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error reading SRF header from ", a, ". IOSTAT = ", i5 )' ) &
                        TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF


      ! --------------------------------------------
      ! Exit loop if this is the required channel...
      ! --------------------------------------------

      IF ( ch == Channel ) EXIT Channel_Skip_Loop


      ! -----------------------------------------------
      ! Otherwise, skip over the current channel's data
      ! -----------------------------------------------

      READ( FileID, FMT = *, IOSTAT = IO_Status ) ( dummy, i = 1, n_Points )

      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error skipping over channel ", i5, " SRF data in ", a, ". IOSTAT = ", i5 )' ) &
                        ch, TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF

    END DO Channel_Skip_Loop



    !#--------------------------------------------------------------------------#
    !#                    -- OUTPUT INFORMATION MESSAGE --                      #
    !#--------------------------------------------------------------------------#

    IF ( Noisy ) THEN
      WRITE( Message, '( a, " channel #", i2, " SRF spectral range; [", f10.4,",",f10.4,"]" )' ) &
                      TRIM( Filename ), Channel, f1, f2
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- FILL OUTPUT ARGUMENT --                         #
    !#--------------------------------------------------------------------------#

    ! --------------------------------------
    ! Allocate the SRF data structure arrays
    ! --------------------------------------

    Error_Status = Allocate_SRF( n_Points, &
                                 SRF, &
                                 Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error allocating SRF structure ", &
                        &"arrays for channel #", i4, "." )' ) Channel
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ------------------------------------------
    ! Read the required instrument response data
    ! ------------------------------------------

    READ( FileID, FMT = *, IOSTAT = IO_Status ) SRF%Response

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading channel ", i5, " SRF data. IOSTAT = ", i5 )' ) &
                      Channel, IO_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -----------------------------------------
    ! Assign the other scalar structure members
    ! -----------------------------------------

    SRF%Channel = Channel

    SRF%Begin_Frequency = f1
    SRF%End_Frequency   = f2


    ! ------------------------------
    ! Compute the SRF frequency grid
    ! ------------------------------

    Error_Status = Frequency_SRF( SRF, Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error computing frequency grid for channel ", i5, " SRF from ", a )' ) &
                      Channel, TRIM( Filename )
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------------------------
    ! Compute the integrated SRF values
    ! ---------------------------------

    Error_Status = Integrate_SRF( SRF, Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error computing integrated SRFs for channel ", i5, " SRF from ", a )' ) &
                      Channel, TRIM( Filename )
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION Read_SRF_ASCII






!------------------------------------------------------------------------------
!S+
! NAME:
!       Write_SRF_ASCII
!
! PURPOSE:
!       Function to write a channel SRF to an ASCII format SRF file.
!
! CATEGORY:
!       Instrument Information: SRF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Write_SRF_ASCII( Filename,                   &  ! Input
!                                       FileID,                     &  ! Input
!                                       SRF,                        &  ! Output
!                                       RCS_Id        = RCS_Id,     &  ! Revision control
!                                       Message_Log   = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:         Character string specifying the name of the
!                         ASCII format SRF data file to inquire.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       FileID:           ASCII file ID number.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:      Character string specifying a filename in which any
!                         messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output messages to standard output.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       SRF:              Data structure containing the requested channel SRF data.
!                         UNITS:      N/A
!                         TYPE:       SRF_type
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:           Character string containing the Revision Control
!                         System Id field for the module.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error
!                         status. The error codes are defined in the
!                         Message_Handler module.
!                         If == SUCCESS the file write was successful
!                            == FAILURE an error occurred writing to file
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
! CALLS:
!       File_Open:          Function to check if a file is open.
!                           SOURCE: FILE_UTILITY module
!
!       Get_Lun:            Function to return an unused file unit number.
!                           SOURCE: FILE_UTILITY module
!
!       Display_Message:    Subroutine to output messages
!                           SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       Output file is closed if an error occurs.
!
! RESTRICTIONS:
!       The SRF header must be written before this function can be called.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Aug-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Write_SRF_ASCII( Filename, &  ! Input
                            FileID,   &  ! Input
                            SRF,      &  ! Input

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
    INTEGER,                  INTENT( IN )  :: FileID
    TYPE( SRF_type ),         INTENT( IN )  :: SRF

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error message log file
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_SRF_ASCII'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message

    INTEGER :: IO_Status



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
    !#             -- CHECK STRUCTURE POINTER ASSOCIATION STATUS --             #
    !#                                                                          #
    !#                ALL structure pointers must be associated                 #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. Associated_SRF( SRF ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT SRF pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- GROSS SRF INPUT CHECK --                        #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! Check channel is valid
    ! ----------------------

    IF ( SRF%Channel < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'SRF CHANNEL member is < 1.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Check SRF array size is valid
    ! -----------------------------

    IF ( SRF%n_Points < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'SRF N_POINTS member is < 1.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- CHECK INPUT --                               #
    !#--------------------------------------------------------------------------#

    ! -----------------
    ! Is the file open?
    ! -----------------

    IF ( .NOT. File_Open( FileID ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'ASCII SRF file '//TRIM( Filename )//' is not open.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- WRITE THE SRF DATA --                           #
    !#--------------------------------------------------------------------------#

    ! ------------------------------------
    ! Write the current channel SRF header
    ! ------------------------------------

    WRITE( FileID, FMT    = SRF_HEADER_FORMAT, &
                   IOSTAT = IO_Status          ) SRF%Channel, &
                                                 SRF%n_Points, &
                                                 SRF%Begin_Frequency, &
                                                 SRF%End_Frequency

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing channel ", i5, " SRF header to ", a, ". IOSTAT = ", i5 )' ) &
                      SRF%Channel, TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ----------------------------------
    ! Write the instrument response data
    ! ----------------------------------

    WRITE( FileID, FMT    = RESPONSE_FORMAT, &
                   IOSTAT = IO_Status        ) SRF%Response

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing channel ", i5, " SRF data to ", a, ". IOSTAT = ", i5 )' ) &
                      SRF%Channel, TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

  END FUNCTION Write_SRF_ASCII

END MODULE SRF_ASCII_IO


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id$
!
! $Date: 2006/08/15 20:51:04 $
!
! $Revision$
!
! $Name:  $
!
! $State: Exp $
!
! $Log: SRF_ASCII_IO.f90,v $
! Revision 1.7  2006/08/15 20:51:04  wd20pd
! Additional replacement of Error_Handler with Message_Handler.
!
! Revision 1.6  2006/05/02 16:58:02  dgroff
! *** empty log message ***
!
! Revision 1.5  2004/08/31 20:43:53  paulv
! - Upgraded to Fortran95
! - Using new String_Utility module.
! - Intent of SRF dummy argument in Read_SRF_ASCII() routine changed from
!   OUT to IN OUT to prevent memory leaks.
! - Added structure association test to the Write_SRF_ASCII() function.
!
! Revision 1.4  2003/11/19 15:26:27  paulv
! - Updated header documentation.
!
! Revision 1.3  2003/08/29 18:30:00  paulv
! - Corrected bug in Noisy definition in Read_SRF() function.
!
! Revision 1.2  2003/08/29 18:05:51  paulv
! - Corrected some minor bugs.
!
! Revision 1.1  2003/08/27 21:45:13  paulv
! Initial checkin.
!
!
!
!
