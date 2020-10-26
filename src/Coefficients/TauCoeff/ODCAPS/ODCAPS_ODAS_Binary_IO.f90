
MODULE ODCAPS_ODAS_Binary_IO


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE Binary_File_Utility

  USE ODCAPS_ODAS_Define


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Inquire_ODCAPS_ODAS_Binary
  PUBLIC :: Read_ODCAPS_ODAS_Binary
  PUBLIC :: Write_ODCAPS_ODAS_Binary


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: ODCAPS_ODAS_Binary_IO.f90,v 1.1 2006/05/02 18:15:01 ychen Exp $'

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1


CONTAINS





  FUNCTION Inquire_ODCAPS_ODAS_Binary( Filename,     &  ! Input
                                    n_Layers,     &  ! Optional output
                                    n_Predictors, &  ! Optional output
                                    n_Channels,   &  ! Optional output
                                    n_ProfAves,   &  ! Optional output
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

    ! -- Optional output
    INTEGER,        OPTIONAL, INTENT( OUT ) :: n_Layers
    INTEGER,        OPTIONAL, INTENT( OUT ) :: n_Predictors
    INTEGER,        OPTIONAL, INTENT( OUT ) :: n_Channels
    INTEGER,        OPTIONAL, INTENT( OUT ) :: n_ProfAves
 
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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Inquire_ODCAPS_ODAS_Binary'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER( Long ) :: File_n_Layers
    INTEGER( Long ) :: File_n_Predictors
    INTEGER( Long ) :: File_n_Channels
    INTEGER( Long ) :: File_n_ProfAves
 

    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS


    ! -----------------------------------
    ! Set the RCS Id argument if supplied
    ! -----------------------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF


    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! --------------------------
    ! Check that the file exists
    ! --------------------------

    IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'File '//TRIM( Filename )//' not found.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- OPEN THE Atmosphere DATA FILE --                  #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID, &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
 

    !#--------------------------------------------------------------------------#
    !#                      -- READ THE "FILE HEADER" --                        #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! Read the dimensions
    ! -------------------

    READ( FileID, IOSTAT = IO_Status ) File_n_Layers , &
                                       File_n_Predictors, &
                                       File_n_Channels, &
                                       File_n_ProfAves

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading data dimensions from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, IOSTAT = IO_Status  )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- CLOSE THE FILE --                            #
    !#--------------------------------------------------------------------------#

    CLOSE( FileID, IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- ASSIGN THE RETURN ARGUMENTS --                     #
    !#--------------------------------------------------------------------------#

    ! ----------
    ! Dimensions
    ! ----------

    IF ( PRESENT( n_Layers ) ) THEN
      n_Layers = File_n_Layers
    END IF

    IF ( PRESENT( n_Predictors ) ) THEN
      n_Predictors = File_n_Predictors
    END IF

    IF ( PRESENT( n_Channels ) ) THEN
      n_Channels = File_n_Channels
    END IF

    IF ( PRESENT( n_ProfAves ) ) THEN
      n_ProfAves = File_n_ProfAves
    END IF

  END FUNCTION Inquire_ODCAPS_ODAS_Binary





  FUNCTION Read_ODCAPS_ODAS_Binary( Filename,          &  ! Input
                                 ODCAPS_ODAS,          &  ! Output
                                 No_File_Close,     &  ! Optional input
                                 RCS_Id,            &  ! Revision control
                                 Message_Log )      &  ! Error messaging
                               RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )     :: Filename

    ! -- Output
    TYPE( ODCAPS_ODAS_type ),    INTENT( IN OUT ) :: ODCAPS_ODAS

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )     :: No_File_Close
 
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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_ODCAPS_ODAS_Binary'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    LOGICAL ::  Yes_File_Close
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER( Long ) :: n_Layers
    INTEGER( Long ) :: n_Predictors
    INTEGER( Long ) :: n_Channels
    INTEGER( Long ) :: n_ProfAves


    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS
 
    !#--------------------------------------------------------------------------#
    !#                 -- SET THE RCS ID ARGUMENT IF SUPPLIED --                #
    !#--------------------------------------------------------------------------#
 
    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF


    !#--------------------------------------------------------------------------#
    !#                -- CHECK/SET OPTIONAL KEYWORD ARGUMENTS --                #
    !#--------------------------------------------------------------------------#

  
    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------------------
    ! Check that the file is open. If not, open it.
    ! Otherwise get its file id
    ! ---------------------------------------------

    IF ( .NOT. File_Open( FileName ) ) THEN

      ! -- Check that the file exists
      IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
        Message = 'File '//TRIM( Filename )//' not found.'
        GOTO 2000  ! Clean up
      END IF 

      ! -- Open the file
      Error_Status = Open_Binary_File( TRIM( Filename ), &
                                       FileID, &
                                       Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error opening '//TRIM( Filename )
        GOTO 2000  ! Clean up
      END IF

    ELSE

      ! -- Inquire for the logical unit number
      INQUIRE( FILE = Filename, NUMBER = FileID )

      ! -- Ensure it's valid
      IF ( FileID == -1 ) THEN
        Message = 'Error inquiring '//TRIM( Filename )//' for its FileID'
        GOTO 2000  ! Clean up
      END IF

    END IF
 
    ! -------------------------------------------
    ! Process the optional No_File_Close argument
    ! -------------------------------------------

    ! -- Default action is to close the file on exit....
    Yes_File_Close = .TRUE.

    ! -- ...unless the No_File_Close optional argument is set.
    IF ( PRESENT( No_File_Close ) ) THEN
      IF ( No_File_Close == SET ) Yes_File_Close = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- READ THE ODCAPS_ODAS DIMENSION FILE --        #
    !#--------------------------------------------------------------------------#
 
    READ( FileID, IOSTAT = IO_Status ) n_Layers, &
                                       n_Predictors, &
                                       n_Channels, &
                                       n_ProfAves

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading data dimensions from ", a, &
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
    !#                  -- ALLOCATE THE ODCAPS_ODAS STRUCTURE --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = Allocate_ODCAPS_ODAS( n_Layers, &
                                      n_Predictors, &
                                      n_Channels, &
                                      n_ProfAves, &
                                      ODCAPS_ODAS, &
                                      Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error occurred allocating ODCAPS_ODAS structure.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- READ THE SENSOR ID DATA --                      #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status )  ODCAPS_ODAS%Channel_Index, & 
                                        ODCAPS_ODAS%Water_Amount, &  
                                        ODCAPS_ODAS%Water_ProfAve, & 
                                        ODCAPS_ODAS%Water_Coeff   
 
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading ODCAPS_ODAS data from ", a, &
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

    IF ( Yes_File_Close ) THEN

      CLOSE( FileID, IOSTAT = IO_Status )

      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                        TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              WARNING, &
                              Message_Log = Message_Log )
      END IF

    END IF
    
    RETURN

    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )
    
    
  END FUNCTION Read_ODCAPS_ODAS_Binary





  FUNCTION Write_ODCAPS_ODAS_Binary( Filename,     &  ! Input
                                  ODCAPS_ODAS,     &  ! Input
                                  No_File_Close,        &  ! Optional input
                                  RCS_Id,               &  ! Revision control
                                  Message_Log )         &  ! Error messaging
                                RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )  :: Filename
    TYPE( ODCAPS_ODAS_type ),    INTENT( IN )  :: ODCAPS_ODAS

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )  :: No_File_Close 

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_ODCAPS_ODAS_Binary'
    CHARACTER( * ), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Yes_File_Close
    INTEGER :: IO_Status
    INTEGER :: FileID



    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS


    !#--------------------------------------------------------------------------#
    !#                 -- SET THE RCS ID ARGUMENT IF SUPPLIED --                #
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

    IF ( .NOT. Associated_ODCAPS_ODAS( ODCAPS_ODAS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT ODCAPS_ODAS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------------------
    ! Check that the file is open. If not, open it.
    ! Otherwise get the file ID.
    ! ---------------------------------------------

    IF ( .NOT. File_Open( FileName ) ) THEN

      Error_Status = Open_Binary_File( TRIM( Filename ), &
                                       FileID, &
                                       For_Output  = SET, &
                                       Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error opening '//TRIM( Filename )
        GOTO 2000  ! Clean up
      END IF


    ELSE

      ! -- Inquire for the logical unit number
      INQUIRE( FILE = Filename, NUMBER = FileID )

      ! -- Ensure it's valid
      IF ( FileID == -1 ) THEN
        Message = 'Error inquiring '//TRIM( Filename )//' for its FileID'
        GOTO 1000  ! Clean up
      END IF

    END IF


    ! ---------------------------------------
    ! Check the ODCAPS_ODAS structure dimensions
    ! ---------------------------------------

    IF ( ODCAPS_ODAS%n_Layers     < 1 .OR. &
         ODCAPS_ODAS%n_Predictors < 1 .OR. &
         ODCAPS_ODAS%n_Channels   < 1 .OR. &
         ODCAPS_ODAS%n_ProfAves   < 1      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'One or more dimensions of ODCAPS_ODAS structure are < or = 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -------------------------------------------
    ! Process the optional No_File_Close argument
    ! -------------------------------------------

    ! -- Default action is to close the file on exit....
    Yes_File_Close = .TRUE.

    ! -- ...unless the No_File_Close optional argument is set.
    IF ( PRESENT( No_File_Close ) ) THEN
      IF ( No_File_Close == SET ) Yes_File_Close = .FALSE.
    END IF


    !#--------------------------------------------------------------------------#
    !#                     -- WRITE THE DIMENSION" --                           #
    !#--------------------------------------------------------------------------#
 
    WRITE( FileID, IOSTAT = IO_Status ) ODCAPS_ODAS%n_Layers, &
                                        ODCAPS_ODAS%n_Predictors, &
                                        ODCAPS_ODAS%n_Channels, &
                                        ODCAPS_ODAS%n_ProfAves

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing data dimensions to ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                      -- WRITE THE ODCAPS_ODAS DATA --                      #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) ODCAPS_ODAS%Channel_Index, & 
                                        ODCAPS_ODAS%Water_Amount, &  
                                        ODCAPS_ODAS%Water_ProfAve, & 
                                        ODCAPS_ODAS%Water_Coeff   
                                        				   
                                        
                                        
                                        

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing ODCAPS_ODAS data to ", a, &
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
    !#                 -- CLOSE THE OUTPUT FILE IF REQUIRED --                  #
    !#--------------------------------------------------------------------------#

    IF ( Yes_File_Close ) THEN

      CLOSE( FileID, IOSTAT = IO_Status )

      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                        TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              WARNING, &
                              Message_Log = Message_Log )
      END IF

    END IF

    RETURN

    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

    1000 CONTINUE
    CLOSE( FileID, IOSTAT = IO_Status )

    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )


 
  END FUNCTION Write_ODCAPS_ODAS_Binary

END MODULE ODCAPS_ODAS_Binary_IO
