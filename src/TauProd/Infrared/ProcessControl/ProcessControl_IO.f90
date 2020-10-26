
MODULE ProcessControl_IO


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler

  USE ProcessControl_Define


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Write_ProcessControl
  PUBLIC :: Read_ProcessControl


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Keyword set value
  INTEGER,        PRIVATE, PARAMETER :: SET = 1

  ! -- Some line formats of the control file
  CHARACTER( * ), PRIVATE, PARAMETER :: MAIN_DIM_FORMAT  = '( i5, 5x, i5 )'
  CHARACTER( * ), PRIVATE, PARAMETER :: FILE_DIM_FORMAT  = '( i5, 2x, i1 )'
  CHARACTER( * ), PRIVATE, PARAMETER :: FILE_DATA_FORMAT = '( 2x, i4, 2x, i3, 2x, i3, 2x, i1 )'


CONTAINS




  FUNCTION Open_ProcessControl( Filename,     &  ! Input
                                FileID,       &  ! Output
                                Output,       &  ! Optional input
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

    ! -- Output
    INTEGER,                  INTENT( OUT ) :: FileID

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )  :: Output
    
    ! -- Error handler message log
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Open_ProcessControl'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    CHARACTER( 5 ) :: File_Action
    CHARACTER( 7 ) :: File_Status

    INTEGER :: IO_Status
    INTEGER :: Lun



    !#--------------------------------------------------------------------------#
    !#                      -- DEFAULT RETURN STATUS --                         #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                           -- CHECK INPUT --                              #
    !#--------------------------------------------------------------------------#

    ! -------------------------------------------------
    ! Default action is to open the file for READING...
    ! -------------------------------------------------

    File_Action = 'READ'
    File_Status = 'OLD'


    ! ------------------------------------
    ! ...unless the OUTPUT argument is set
    ! ------------------------------------

    IF ( PRESENT( Output ) ) THEN
      IF ( Output == SET ) THEN
        File_Action = 'WRITE'
        File_Status = 'REPLACE'
      END IF
    END IF



    !#--------------------------------------------------------------------------#
    !#                  -- OPEN THE ProcessControl LIST FILE --                 #
    !#--------------------------------------------------------------------------#

    ! ---------------------------
    ! Get a free file unit number
    ! ---------------------------

    Lun = Get_Lun()

    IF ( Lun < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining file unit number for '//TRIM( Filename )//'.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -------------
    ! Open the file
    ! -------------

    OPEN( Lun, FILE   = TRIM( Filename ), &
               ACCESS = 'SEQUENTIAL', &
               FORM   = 'FORMATTED', &
               STATUS = TRIM( File_Status ), &
               ACTION = TRIM( File_Action ), &
               IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error opening output ProcessControl file ", a, &
                        &" for ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), TRIM( File_Action ), IO_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- SAVE THE FILEID FOR OUTPUT --                     #
    !#--------------------------------------------------------------------------#

    FileID = Lun

  END FUNCTION Open_ProcessControl







  FUNCTION Write_ProcessControl( Filename,       &  ! Input
                                 ProcessControl, &  ! Input
                                 Message_Log )   &  ! Error messaging
                               RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),              INTENT( IN ) :: Filename
    TYPE( ProcessControl_type ), INTENT( IN ) :: ProcessControl

    ! -- Error handler message log
    CHARACTER( * ),    OPTIONAL, INTENT( IN ) :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_ProcessControl'

    CHARACTER( * ), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: FileID
    INTEGER :: IO_Status

    INTEGER :: i, l
    INTEGER :: n_File_Channels



    !#--------------------------------------------------------------------------#
    !#                       -- OPEN THE FILE FOR OUTPUT --                     #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_ProcessControl( TRIM( Filename ), &
                                        FileID,           &
                                        Output = SET      )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening Process Control file '//&
                            TRIM( Filename )//' for output.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                           -- WRITE THE DATA --                           #
    !#--------------------------------------------------------------------------#

    ! -------------------------
    ! Write the main dimensions
    ! -------------------------

    WRITE( FileID, FMT    = MAIN_DIM_FORMAT, &
                   IOSTAT = IO_Status        ) ProcessControl%n_Files, &
                                               ProcessControl%n_Channels

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing main dimensions to output ", &
                        &"ProcessControl file ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! ------------------------------
    ! Loop over individual file data
    ! ------------------------------

    DO i = 1, ProcessControl%n_Files

      ! -- Determine the number of channels in this file
      n_File_Channels = ProcessControl%Channel_Index(2,i) - ProcessControl%Channel_Index(1,i) + 1

      ! -- Write the file prefix
      WRITE( FileID, FMT    = '(a)', &
                     IOSTAT = IO_Status ) ProcessControl%File_Prefix(i)

      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error writing file prefix, ", a, ", to output ", &
                          &"ProcessControl file ", a, ". IOSTAT = ", i5 )' ) &
                        TRIM( ProcessControl%File_Prefix(i) ), &
                        TRIM( Filename ), &
                        IO_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
        RETURN
      END IF

      ! -- Write the file dimensions
      WRITE( FileID, FMT    = FILE_DIM_FORMAT, &
                     IOSTAT = IO_Status        ) n_File_Channels, &
                                                 ProcessControl%dF_Index(i)

      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error writing file dimensions for prefix ", a, " to output ", &
                          &"ProcessControl file ", a, ". IOSTAT = ", i5 )' ) &
                        TRIM( ProcessControl%File_Prefix(i) ), &
                        TRIM( Filename ), &
                        IO_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
        RETURN
      END IF

      ! -- Write the file data
      DO l = ProcessControl%Channel_Index(1,i), ProcessControl%Channel_Index(2,i)

        WRITE( FileID, FMT    = FILE_DATA_FORMAT, &
                       IOSTAT = IO_Status         ) ProcessControl%List( l )%Channel, &
                                                    ProcessControl%List( l )%Begin_LBLband, &
                                                    ProcessControl%List( l )%End_LBLband, &
                                                    ProcessControl%List( l )%Processed

        IF ( IO_Status /= 0 ) THEN
          Error_Status = FAILURE
          WRITE( Message, '( "Error writing ", a, " file channel index ", i4, &
                          &" data to output ", &
                          &"ProcessControl file ", a, ". IOSTAT = ", i5 )' ) &
                        TRIM( ProcessControl%File_Prefix(i) ), l, &
                        TRIM( Filename ), IO_Status
          CALL Display_Message( ROUTINE_NAME,    &
                                TRIM( Message ), &
                                Error_Status,    &
                                Message_Log = Message_Log )
          CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
          RETURN
        END IF

      END DO

    END DO



    !#--------------------------------------------------------------------------#
    !#                   -- CLOSE THE ProcessControl FILE --                    #
    !#--------------------------------------------------------------------------#

    CLOSE( FileID, STATUS = 'KEEP' )

  END FUNCTION Write_ProcessControl






  FUNCTION Read_ProcessControl( Filename,       &  ! Input
                                ProcessControl, &  ! Output
                                Message_Log )   &  ! Error messaging
                              RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),              INTENT( IN )     :: Filename

    ! -- Output
    TYPE( ProcessControl_type ), INTENT( IN OUT ) :: ProcessControl

    ! -- Error message log file
    CHARACTER( * ),    OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_ProcessControl'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message

    INTEGER :: FileID
    INTEGER :: IO_Status

    INTEGER :: i, l
    INTEGER :: n_Files
    INTEGER :: n_Channels
    INTEGER :: n_File_Channels
    INTEGER :: Begin_Index, End_Index



    !#--------------------------------------------------------------------------#
    !#                       -- OPEN THE FILE FOR INPUT --                      #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_ProcessControl( TRIM( Filename ), &
                            FileID )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening Process Control file '//&
                            TRIM( Filename )//' for input.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- ALLOCATE THE ProcessControl POINTER MEMBERS --            #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! Read the dimensions
    ! -------------------

    READ( FileID, FMT    = MAIN_DIM_FORMAT, &
                  IOSTAT = IO_Status        ) n_Files, n_Channels

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading main dimensions from input ", &
                        &"ProcessControl file ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -------------------------------------
    ! Allocate the ProcessControl structure
    ! -------------------------------------

    Error_Status = Allocate_ProcessControl( n_Files, &
                                            n_Channels, &
                                            ProcessControl, &
                                            Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error allocating ProcessControl pointer members.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    
    !#--------------------------------------------------------------------------#
    !#                  -- LOOP OVER INDIVIDUAL FILE DATA --                    #
    !#--------------------------------------------------------------------------#

    Begin_Index = 1

    File_Loop: DO i = 1, ProcessControl%n_Files


      ! -----------------------------------
      ! Read the file prefix and dimensions
      ! -----------------------------------

      READ( FileID, FMT    = '(a)', &
                    IOSTAT = IO_Status ) ProcessControl%File_Prefix(i)

      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error reading file ", i5, " prefix from input ", &
                          &"ProcessControl file ", a, ". IOSTAT = ", i5 )' ) &
                        i, TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF

      READ( FileID, FMT    = FILE_DIM_FORMAT, &
                    IOSTAT = IO_Status        ) n_File_Channels, &
                                                ProcessControl%dF_Index(i)

      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error reading ", a, " dimensions from input ", &
                          &"ProcessControl file ", a, ". IOSTAT = ", i5 )' ) &
                        TRIM( ProcessControl%File_Prefix(i) ), TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF



      ! -----------------------------------
      ! Calculate the begin and end channel
      ! indices in the entire list
      ! -----------------------------------

      ! -- Set the current end index
      End_Index = Begin_Index + n_File_Channels - 1

      IF ( End_Index > n_Channels ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "The number of channels in ", a, ", ", i4, &
                          &", exceeds that specified, ", i4, "." )' ) &
                        TRIM( Filename ), End_Index, n_Channels
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF

      ! -- Save the indices
      ProcessControl%Channel_Index(1,i) = Begin_Index
      ProcessControl%Channel_Index(2,i) =   End_Index

      ! -- Update the begin index for the next file
      Begin_Index = End_Index + 1


      ! ------------------
      ! Read the file data
      ! ------------------

      Channel_Loop: DO l = ProcessControl%Channel_Index(1,i), ProcessControl%Channel_Index(2,i)

        READ( FileID, FMT    = FILE_DATA_FORMAT, &
                      IOSTAT = IO_Status         ) ProcessControl%List( l )%Channel, &
                                                   ProcessControl%List( l )%Begin_LBLband, &
                                                   ProcessControl%List( l )%End_LBLband, &
                                                   ProcessControl%List( l )%Processed

        IF ( IO_Status /= 0 ) THEN
          Error_Status = FAILURE
          WRITE( Message, '( "Error reading ", a, " file channel index ", i4, &
                            &" data from input ", &
                            &"ProcessControl file ", a, ". IOSTAT = ", i5 )' ) &
                          TRIM( ProcessControl%File_Prefix(i) ), l, &
                          TRIM( Filename ), IO_Status
          CALL Display_Message( ROUTINE_NAME,    &
                                TRIM( Message ), &
                                Error_Status,    &
                                Message_Log = Message_Log )
          CLOSE( FileID )
          RETURN
        END IF


        ! --------------------------------
        ! Set the other structure memebers
        ! --------------------------------

        ProcessControl%List( l )%File_Index     = i
        ProcessControl%List( l )%Data_Available = .TRUE.

      END DO Channel_Loop

    END DO File_Loop



    !#--------------------------------------------------------------------------#
    !#                   -- CLOSE THE ProcessControl FILE --                    #
    !#--------------------------------------------------------------------------#

    CLOSE( FileID )

  END FUNCTION Read_ProcessControl

END MODULE ProcessControl_IO


