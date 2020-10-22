!------------------------------------------------------------------------------
!M+
! NAME:
!       AIRS_ChannelProperties
!
! PURPOSE:
!       Module containing I/O functions for the AIRS L2 IR Channel
!       Properties file.
!       
! CATEGORY:
!       AIRS
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE AIRS_ChannelProperties
!
! MODULES:
!       Type_Kinds:                      Module containing definitions for
!                                        kinds of variable types.
!
!       File_Utility:                    Module containing generic file
!                                        utility routines
!
!       Message_Handler:                   Module to define simple error codes
!                                        and handle error conditions
!                                        USEs: FILE_UTILITY module
!
!       AIRS_Define:                     Module containing AIRS instrument
!                                        definitions.
!                                        USEs: TYPE_KINDS module
!
!       AIRS_ChannelProperties_Define:   Module defining the AIRS channel
!                                        properties data structure.
!                                        USEs: TYPE_KINDS module
!
! CONTAINS:
!       Read_AIRS_ChannelProperties: Function to read the AIRS L2 channel
!                                    properties file.
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
!       Written by:     Paul van Delst, CIMSS/SSEC 08-Jan-2003
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2003 Paul van Delst
!
!M-
!------------------------------------------------------------------------------

MODULE AIRS_ChannelProperties


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler

  USE AIRS_Define

  ! -- The AIRS_ChannelProperties structure definition module
  ! -- The PUBLIC entities in AIRS_ChannelProperties_Define
  ! -- are also explicitly defined as PUBLIC here so 
  ! -- a user need only USE AIRS_ChannelProperties.
  USE AIRS_ChannelProperties_Define


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  ! -- Everything private by default
  PRIVATE

  ! -- The AIRS_ChannelProperties data type defined
  ! -- in the AIRS_ChannelProperties_Define module
  PUBLIC :: AIRS_ChannelProperties_type

  ! -- Routines in this module
  PUBLIC :: Read_AIRS_ChannelProperties
  PUBLIC :: Write_AIRS_ChannelProperties



  ! ------------------
  ! Private parameters
  ! ------------------

  ! -- Module RCS Id string
  CHARACTER( * ),  PRIVATE, PARAMETER :: MODULE_RCS_ID = &

  ! -- The data reading format string
  CHARACTER( * ), PRIVATE, PARAMETER :: FORMAT_STRING = &
    '(i5,f9.3,1x,a5,i5,f7.4,f6.3,f8.4,2f8.1,f6.3,i3,i3,i3,1x,a8)'

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1


CONTAINS





!------------------------------------------------------------------------------
!S+
! NAME:
!       Read_AIRS_ChannelProperties
!
! PURPOSE:
!       Function to read the AIRS L2 channel properties file
!
! CATEGORY:
!       AIRS
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Read_AIRS_ChannelProperties( Filename,                 &  ! Input
!                                                   ChannelProperties,        &  ! Output
!                                                   Quiet       = Quiet,      &  ! Optional input
!                                                   RCS_Id      = RCS_Id,     &  ! Optional output
!                                                   Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:          Character string specifying the name of the
!                          AIRS L2 channel properties file.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER( * )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:             Set this keyword to suppress information messages
!                          being printed to standard output (or the message
!                          log file if the Message_Log optional argument is
!                          used.) By default, information messages are printed.
!                          If QUIET = 0, information Messages are OUTPUT.
!                             QUIET = 1, information Messages are SUPPRESSED.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:       Character string specifying a filename in which any
!                          Messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output Messages to standard output.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER( * )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       ChannelProperties: Structure array containing the AIRS channel properties
!                          data.
!                          UNITS:      N/A
!                          TYPE:       AIRS_ChannelProperties_type
!                          DIMENSION:  Rank-1
!                          ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:            Character string containing the Revision Control
!                          System Id field for the module.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER( * )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status:      The return value is an integer defining the error
!                          status. The error codes are defined in the
!                          Message_Handler module.
!                          If == SUCCESS the file read was successful
!                             == FAILURE an unrecoverable error occurred.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!
! CALLS:
!       Display_Message:   Subroutine to output messages
!                          SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 09-Jan-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Read_AIRS_ChannelProperties( Filename, &
                                        ChannelProperties, &
                                        Quiet, &
                                        RCS_Id, &
                                        Message_Log ) &
                                      RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),                                      INTENT( IN )  :: Filename

    ! -- Output
    TYPE( AIRS_ChannelProperties_type ), DIMENSION( : ), INTENT( OUT ) :: ChannelProperties

    ! -- Optional input
    INTEGER,                                   OPTIONAL, INTENT( IN )  :: Quiet

    ! -- Optional output
    CHARACTER( * ),                            OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error Message log file
    CHARACTER( * ),                            OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_AIRS_ChannelProperties'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message

    LOGICAL :: Noisy

    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: n_Lines
    INTEGER :: l, n_Channels

    CHARACTER( 256 ) :: Line_Buffer



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
    !#                             -- CHECK INPUT --                            #
    !#--------------------------------------------------------------------------#

    ! --------------------
    ! Does the file exist?
    ! --------------------

    IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'File '//TRIM( Filename )//' not found.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------------------------------
    ! Check the ChannelProperties dimensions
    ! --------------------------------------

    n_Channels = SIZE( ChannelProperties )

    IF ( n_Channels > N_AIRS_CHANNELS ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Number of channels, ", i4, &
                        &" exceeds the number of AIRS channels!" )' ) &
                      n_Channels
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF
  

    ! --------------
    ! Check keywords
    ! --------------

    ! -- Output informational Messages....
    Noisy = .TRUE.
    ! -- ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#            -- OPEN THE AIRS L2 CHANNEL PROPERTIES DATA FILE --           #
    !#--------------------------------------------------------------------------#

    ! ---------------------------
    ! Get a free file unit number
    ! ---------------------------

    FileID = Get_Lun()

    IF ( FileID < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining file unit number.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -------------
    ! Open the file
    ! -------------

    OPEN( FileID, FILE   = ADJUSTL( Filename ), &
                  STATUS = 'OLD', &
                  ACCESS = 'SEQUENTIAL', &
                  FORM   = 'FORMATTED', &
                  ACTION = 'READ', &
                  IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#            -- READ THE FILE LINE-BY-LINE UNTIL END-OF-FILE --            #
    !#--------------------------------------------------------------------------#

    n_Lines = 0
    l = 0

    Read_Loop: DO


      ! -------------------------------
      ! Increment the file line counter
      ! -------------------------------

      n_Lines = n_Lines + 1


      ! -----------------------
      ! Read a line of the file
      ! -----------------------

      READ( FileID, FMT    = '( a )',  &
                    IOSTAT = IO_Status ) Line_Buffer


      ! ---------------
      ! Check IO_Status
      ! ---------------

      IF ( IO_Status /= 0 ) THEN

        ! -- Close the input file
        CLOSE( FileID )

        ! -- Check for EOF
        IF ( IO_Status < 0 ) THEN
          EXIT Read_Loop

        ! -- Otherwise, we have an error
        ELSE
          Error_Status = FAILURE
          WRITE( Message, '( "Error reading ", a, " at line #", i4, &
                            &". IOSTAT = ", i5 )' ) &
                          TRIM( Filename ), n_Lines, IO_Status
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          RETURN
        END IF

      END IF


      ! ---------------------------------------------
      ! Cycle loop if this is a comment or blank line
      ! ---------------------------------------------

      IF ( Line_Buffer(1:1)        == '!' .OR. &
           LEN_TRIM( Line_Buffer ) == 0        ) CYCLE Read_Loop


      ! -----------------------------
      ! Increment the channel counter
      ! -----------------------------

      l = l + 1

      IF ( l > n_Channels ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Number of channels, ", i4, &
                          &" has exceeded the number of structure array elements!" )' ) &
                        l
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF


      ! ----------------------------------------
      ! Fill the structure from the line of data
      ! ----------------------------------------

      READ( Line_Buffer, FMT    = FORMAT_STRING, &
                         IOSTAT = IO_Status ) ChannelProperties( l )

      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error reading ", a, " line buffer for channel ", i4, &
                          &" after line #", i4, &
                          &". IOSTAT = ", i5 )' ) &
                        TRIM( Filename ), l, n_Lines, IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF

    END DO Read_Loop



    !#--------------------------------------------------------------------------#
    !#                      -- OUTPUT AN INFO Message --                        #
    !#--------------------------------------------------------------------------#

    IF ( Noisy ) THEN
      WRITE( Message, '( "FILE: ", a, ", N_CHANNELS=",i4 )' ) &
                      TRIM( Filename ), &
                      l
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Read_AIRS_ChannelProperties





!------------------------------------------------------------------------------
!S+
! NAME:
!       Write_AIRS_ChannelProperties
!
! PURPOSE:
!       Function to write the AIRS L2 channel properties file
!
! CATEGORY:
!       AIRS
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Write_AIRS_ChannelProperties( Filename,                 &  ! Input
!                                                    ChannelProperties,        &  ! Input
!                                                    Quiet       = Quiet,      &  ! Optional input
!                                                    RCS_Id      = RCS_Id,     &  ! Optional output
!                                                    Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:          Character string specifying the name of the
!                          AIRS L2 channel properties file.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER( * )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       ChannelProperties: Structure array containing the AIRS channel properties
!                          data.
!                          UNITS:      N/A
!                          TYPE:       AIRS_ChannelProperties_type
!                          DIMENSION:  Rank-1
!                          ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:             Set this keyword to suppress information messages
!                          being printed to standard output (or the message
!                          log file if the Message_Log optional argument is
!                          used.) By default, information messages are printed.
!                          If QUIET = 0, information Messages are OUTPUT.
!                             QUIET = 1, information Messages are SUPPRESSED.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:       Character string specifying a filename in which any
!                          Messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output Messages to standard output.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER( * )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:            Character string containing the Revision Control
!                          System Id field for the module.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER( * )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status:      The return value is an integer defining the error
!                          status. The error codes are defined in the
!                          Message_Handler module.
!                          If == SUCCESS the file write was successful
!                             == FAILURE an unrecoverable error occurred.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!
! CALLS:
!       Display_Message:   Subroutine to output messages
!                          SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       - If the output file already exists, a WARNING message is issued and
!         the file is overwritten.
!       - If an error occurs writing the data to file, the file is deleted.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 09-Jan-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Write_AIRS_ChannelProperties( Filename, &
                                         ChannelProperties, &
                                         Quiet, &
                                         RCS_Id, &
                                         Message_Log ) &
                                       RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),                                      INTENT( IN )  :: Filename
    TYPE( AIRS_ChannelProperties_type ), DIMENSION( : ), INTENT( IN )  :: ChannelProperties

    ! -- Optional input
    INTEGER,                                   OPTIONAL, INTENT( IN )  :: Quiet

    ! -- Optional output
    CHARACTER( * ),                            OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error Message log file
    CHARACTER( * ),                            OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_AIRS_ChannelProperties'
    CHARACTER( * ), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message

    LOGICAL :: Noisy

    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: l, n_Channels



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
    !#                             -- CHECK INPUT --                            #
    !#--------------------------------------------------------------------------#

    ! --------------------
    ! Does the file exist?
    ! --------------------

    IF ( File_Exists( TRIM( Filename ) ) ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'File '//TRIM( Filename )//' already exists. Overwriting.', &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! --------------------------------------
    ! Check the ChannelProperties dimensions
    ! --------------------------------------

    n_Channels = SIZE( ChannelProperties )

    IF ( n_Channels > N_AIRS_CHANNELS ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Number of channels, ", i4, &
                        &" exceeds the number of AIRS channels!" )' ) &
                      n_Channels
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF
  

    ! --------------
    ! Check keywords
    ! --------------

    ! -- Output informational Messages....
    Noisy = .TRUE.
    ! -- ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#            -- OPEN THE AIRS L2 CHANNEL PROPERTIES DATA FILE --           #
    !#--------------------------------------------------------------------------#

    ! ---------------------------
    ! Get a free file unit number
    ! ---------------------------

    FileID = Get_Lun()

    IF ( FileID < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining file unit number.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -------------
    ! Open the file
    ! -------------

    OPEN( FileID, FILE   = ADJUSTL( Filename ), &
                  STATUS = 'REPLACE', &
                  ACCESS = 'SEQUENTIAL', &
                  FORM   = 'FORMATTED', &
                  ACTION = 'WRITE', &
                  IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- WRITE THE ChannelPRoperties STRUCTURE ARRAY --            #
    !#--------------------------------------------------------------------------#

    Write_Loop: DO l = 1, n_Channels

      WRITE( FileID, FMT    = FORMAT_STRING, &
                     IOSTAT = IO_Status      ) ChannelProperties( l )

      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error writing channel ", i4, " data to ", a, ". IOSTAT = ", i5 )' ) &
                        l, TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
        RETURN
      END IF

    END DO Write_Loop


    ! --------------------
    ! Done. Close the file
    ! --------------------

    CLOSE( FileID )



    !#--------------------------------------------------------------------------#
    !#                      -- OUTPUT AN INFO Message --                        #
    !#--------------------------------------------------------------------------#

    IF ( Noisy ) THEN
      WRITE( Message, '( "FILE: ", a, ", N_CHANNELS=",i4 )' ) &
                      TRIM( Filename ), n_Channels
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Write_AIRS_ChannelProperties

END MODULE AIRS_ChannelProperties


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2006/06/15 17:14:58 $
!
! $Revision: 1.6 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: AIRS_ChannelProperties.f90,v $
! Revision 1.6  2006/06/15 17:14:58  wd20pd
! Replaced all references to Error_Handler with Message_Handler.
!
! Revision 1.5  2004/08/11 19:59:51  paulv
! - Tested.
!
! Revision 1.4  2004/08/11 12:05:45  paulv
! - Reorganising and f90->f95 conversion. Incomplete.
!
! Revision 1.3  2004/03/09 17:14:58  paulv
! - Cosmetic changes to documentation.
!
! Revision 1.2  2003/01/10 15:42:17  paulv
! - Altered the definition of the AIRS_ChannelProperties_type from a structure
!   of arrays (with a channel dimension) to a simple scalar structure. Now, the
!   AIRS channel properties are represented by an array of structures.
!
! Revision 1.1  2003/01/09 21:23:00  paulv
! Initial checkin.
!
!
!
!
