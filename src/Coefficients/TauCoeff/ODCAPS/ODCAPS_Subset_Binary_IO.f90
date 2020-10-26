
MODULE ODCAPS_Subset_Binary_IO


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE Binary_File_Utility

  USE ODCAPS_Subset_Define



  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Inquire_ODCAPS_Subset_Binary
  PUBLIC :: Read_ODCAPS_Subset_Binary
  PUBLIC :: Write_ODCAPS_Subset_Binary

  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  INTERFACE Read_ODCAPS_Subset_Binary 
    MODULE PROCEDURE Read_ODCAPS_Subset_Scalar
    MODULE PROCEDURE Read_ODCAPS_Subset_Rank1
  END INTERFACE Read_ODCAPS_Subset_Binary 

  INTERFACE Write_ODCAPS_Subset_Binary 
    MODULE PROCEDURE Write_ODCAPS_Subset_Scalar
    MODULE PROCEDURE Write_ODCAPS_Subset_Rank1
  END INTERFACE Write_ODCAPS_Subset_Binary
  
  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: ODCAPS_Subset_Binary_IO.f90,v 5.6 2006/05/03 18:15:01 Ychen Exp $'

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: UNSET = 0
  INTEGER, PRIVATE, PARAMETER :: SET   = 1

  INTEGER, PRIVATE, PARAMETER :: NO    = UNSET
  INTEGER, PRIVATE, PARAMETER :: YES   = SET


CONTAINS



  FUNCTION Read_ODCAPS_Subset_Record( FileID,           &  ! Input
                                     	ODCAPS_Subset,  &  ! Output
                                     	No_Allocate,      &  ! Optional input
                                     	Message_Log )     &  ! Error messaging
                                      RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                       INTENT( IN )     :: FileID

    ! -- Outut
    TYPE( ODCAPS_Subset_type ),  INTENT( IN OUT ) :: ODCAPS_Subset

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )     :: No_Allocate

    ! -- Error handler Message log
    CHARACTER( * ), OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_ODCAPS_Subset_Binary(Record)'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Yes_Allocate
    INTEGER :: IO_Status
    INTEGER :: Destroy_Status
    INTEGER( Long ) :: n_Layers
    INTEGER( Long ) :: n_Total_Predictors  
    INTEGER( Long ) :: n_Absorbers        
    INTEGER( Long ) :: n_Channels         
  

    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! -------------------------------------------
    ! Process the optional No_Allocate argument
    ! -------------------------------------------

    ! -- Default action is to allocate the structure....
    Yes_Allocate = .TRUE.

    ! -- ...unless the No_Allocate optional argument is set.
    IF ( PRESENT( No_Allocate ) ) THEN
      IF ( No_Allocate == SET ) Yes_Allocate = .FALSE.
    END IF


    !#--------------------------------------------------------------------------#
    !#                      -- READ THE "FILE HEADER" --                        #
    !#--------------------------------------------------------------------------#

    ! ------------------------------------
    ! Read the Release/Version information
    ! ------------------------------------

    READ( FileID, IOSTAT = IO_Status ) ODCAPS_Subset%Release, &
                                       ODCAPS_Subset%Version
				   
    IF ( IO_Status /= 0 ) THEN
      WRITE(Message, '("Error reading ODCAPS_Subset file Release/Version. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! -----------------
    ! Check the release
    ! -----------------

    Error_Status = CheckRelease_ODCAPS_Subset( ODCAPS_Subset, &
                                           Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Message='ODCAPS_Subset Release check failed.' 
      GOTO 1000  ! Clean up
    END IF


    !#--------------------------------------------------------------------------#
    !#                     -- READ THE ODCAPS_Subset DIMENSIONS --                      #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) n_Layers, &                    
                                       n_Total_Predictors, &
                                       n_Absorbers, &
                                       n_Channels

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading ODCAPS_Subset data dimensions. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF



    !#--------------------------------------------------------------------------#
    !#              -- ALLOCATE THE ODCAPS_Subset STRUCTURE IF REQUIRED --              #
    !#--------------------------------------------------------------------------#

    IF ( Yes_Allocate ) THEN


      ! ----------------------
      ! Perform the allocation
      ! ----------------------

      Error_Status = Allocate_ODCAPS_Subset( n_Layers, &
                                               n_Total_Predictors, &
                                               n_Absorbers, &
                                               n_Channels, &
                                               ODCAPS_Subset, &
                                          Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error allocating ODCAPS_Subset structure.'
        GOTO 1000
      END IF


    ELSE


      ! ---------------------------------------------------------
      ! Structure already allocated. Check the association status
      ! ---------------------------------------------------------

      IF ( .NOT. Associated_ODCAPS_Subset( ODCAPS_Subset ) ) THEN
        Message = 'ODCAPS_Subset structure components are not associated.'
        GOTO 1000  ! Clean up
      END IF


      ! --------------------------
      ! Check the dimension values
      ! --------------------------

      IF ( n_Layers /= ODCAPS_Subset%n_Layers ) THEN
        WRITE( Message, '( "ODCAPS_Subset data dimensions, ", i5, &
                          &" are inconsistent with structure definition, ", i5, "." )' ) &
                        n_Layers, ODCAPS_Subset%n_Layers
        GOTO 1000  ! Clean up
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- READ THE ODCAPS_Subset DATA --             #
    !#--------------------------------------------------------------------------#


    !#--------------------------------------------------------------------------#
    !#                       -- READ THE CHANNEL INDEX DATA --                  #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) ODCAPS_Subset%Channel_Index

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading ODCAPS_Subset Channel_Index data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF

    !#--------------------------------------------------------------------------#
    !#                      -- READ THE ABSORBER ID DATA --                     #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) ODCAPS_Subset%Absorber_ID

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading ODCAPS_Subset absorber ID data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF

    !#--------------------------------------------------------------------------#
    !#                       -- READ THE ABSORBER PREDICTOR INDICES --          #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) ODCAPS_Subset%Absorber_Predictor

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading ODCAPS_Subset Absorber_Predictor data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF

    !#--------------------------------------------------------------------------#
    !#                -- READ THE GAS ABSORPTION COEFFICIENTS --                #
    !#--------------------------------------------------------------------------#
 
    READ( FileID, IOSTAT = IO_Status ) ODCAPS_Subset%C

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading ODCAPS_Subset absorption coefficients data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    RETURN
        
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

    1000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )

    IF ( Associated_ODCAPS_Subset( ODCAPS_Subset ) ) THEN
      Destroy_Status = Destroy_ODCAPS_Subset( ODCAPS_Subset, &
                                           Message_Log = Message_Log )
    END IF

    CLOSE( FileID, IOSTAT = IO_Status )

  END FUNCTION Read_ODCAPS_Subset_Record


  FUNCTION Write_ODCAPS_Subset_Record( FileID,           &  ! Input
                                     	 ODCAPS_Subset,  &  ! Input
                                     	 Message_Log )     &  ! Error messaging
                                       RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                       INTENT( IN )  :: FileID
    TYPE( ODCAPS_Subset_type ),  INTENT( IN )  :: ODCAPS_Subset

    ! -- Error handler Message log
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_ODCAPS_Subset_Binary(Record)'
    CHARACTER( * ), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message

    INTEGER :: IO_Status
 


    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#             -- CHECK STRUCTURE POINTER ASSOCIATION STATUS --             #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. Associated_ODCAPS_Subset( ODCAPS_Subset ) ) THEN
      Message = 'Some or all INPUT ODCAPS_Subset pointer members are NOT associated.'
      GOTO 1000
    END IF


    !#--------------------------------------------------------------------------#
    !#                      -- WRITE THE "FILE HEADER" --                        #
    !#--------------------------------------------------------------------------#

    ! ------------------------------------
    ! WRITE the Release/Version information
    ! ------------------------------------

    WRITE( FileID, IOSTAT = IO_Status ) ODCAPS_Subset%Release, &
                                        ODCAPS_Subset%Version

    IF ( IO_Status /= 0 ) THEN
      WRITE(Message, '("Error writing ODCAPS_Subset file Release/Version. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF

    !#--------------------------------------------------------------------------#
    !#                    -- WRITE THE ODCAPS_Subset DIMENSIONS --            #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) ODCAPS_Subset%n_Layers, &
                                        ODCAPS_Subset%n_Total_Predictors, &
                                        ODCAPS_Subset%n_Absorbers, &
                                        ODCAPS_Subset%n_Channels

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing ODCAPS_Subset data dimensions. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- WRITE THE ODCAPS_Subset DATA --             #
    !#--------------------------------------------------------------------------#

    !#--------------------------------------------------------------------------#
    !#                       -- WRITE THE CHANNEL INDEX DATA --                 #
    !#--------------------------------------------------------------------------#


    WRITE( FileID, IOSTAT = IO_Status )  ODCAPS_Subset%Channel_Index 
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing ODCAPS_Subset Channel_Index data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF

    !#--------------------------------------------------------------------------#
    !#                      -- WRITE THE ABSORBER ID DATA --                    #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) ODCAPS_Subset%Absorber_ID

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing ODCAPS_Subset absorber ID data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF

    !#--------------------------------------------------------------------------#
    !#                       -- WRITE THE ABSORBER PREDICTOR INDICES --         #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) ODCAPS_Subset%Absorber_Predictor

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing ODCAPS_Subset Absorber_Predictor data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF

    !#--------------------------------------------------------------------------#
    !#                -- WRITE THE GAS ABSORPTION COEFFICIENTS --               #
    !#--------------------------------------------------------------------------#
 
    WRITE( FileID, IOSTAT = IO_Status ) ODCAPS_Subset%C

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing ODCAPS_Subset absorption coefficients data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    RETURN


    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

    1000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )
    CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR, IOSTAT = IO_Status )

  END FUNCTION Write_ODCAPS_Subset_Record






  FUNCTION Inquire_ODCAPS_Subset_Binary( Filename,     &  ! Input
                                           n_Subsets,	 &  ! Optional output
                                           RCS_Id,	 &  ! Revision control
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
    INTEGER,        OPTIONAL, INTENT( OUT ) :: n_Subsets
 
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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Inquire_ODCAPS_Subset_Binary'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER( Long ) :: File_n_Subsets
 



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
    !#             -- OPEN THE BINARY FORMAT ODCAPS_Subset DATA FILE --              #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID,   &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening ODCAPS_Subset file '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- READ THE n_Subsets of ODCAPS --                #
    !#--------------------------------------------------------------------------#

    ! ------------------------------------
    ! Read the Release/Version information
    ! ------------------------------------

    READ( FileID, IOSTAT = IO_Status ) File_n_Subsets 

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading File_n_Subsets data dimension from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, IOSTAT = IO_Status )
      RETURN
    END IF

    !#--------------------------------------------------------------------------#
    !#                    -- SAVE THE NUMBER OF SUBSET  --                      #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( n_Subsets ) ) n_Subsets = File_n_Subsets

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


 
  END FUNCTION Inquire_ODCAPS_Subset_Binary





  FUNCTION Read_ODCAPS_Subset_Scalar( Filename,   &  ! Input
                                 ODCAPS_Subset,   &  ! Output
                                 No_File_Close,     &  ! Optional input
                                 No_Allocate,       &  ! Optional input
                                 n_Subsets,         &  ! Optional output
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
    TYPE( ODCAPS_Subset_type ),    INTENT( IN OUT ) :: ODCAPS_Subset

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )     :: No_File_Close 
    INTEGER,        OPTIONAL, INTENT( IN )     :: No_Allocate

    ! -- Optional output
    INTEGER,        OPTIONAL, INTENT( OUT )    :: n_Subsets

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_ODCAPS_Subset_Binary(Scalar)'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Yes_File_Close
    INTEGER :: IO_Status
    INTEGER :: Destroy_Status
    INTEGER :: FileID
    INTEGER( Long ):: n_Input_Subsets
 


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
                                     FileID,   &
                                     Message_Log = Message_Log )
 
      IF ( Error_Status /= SUCCESS ) THEN
       Message ='Error opening '//TRIM( Filename )  
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
    !#                       -- READ THE NUMBER OF SUBSETS --                   #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) n_Input_Subsets

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading n_Input_Subsets data dimension from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000  ! Clean up
    END IF

    ! ---------------------
    ! Check if n_Subsets > 1
    ! ---------------------

    IF ( n_Input_Subsets > 1 ) THEN
      WRITE( Message, '( "Number of subsets > 1 (",i5,") and output ODCAPS_Subset structure ", &
                        &"is scalar." )' ) n_Input_Subsets
      GOTO 1000  ! Clean up
    END IF

 
    !#--------------------------------------------------------------------------#
    !#                        -- READ THE STRUCTURE DATA --                     #
    !#--------------------------------------------------------------------------#
    Error_Status = Read_ODCAPS_Subset_Record( FileID, &
                                      ODCAPS_Subset, &
                                      No_Allocate = No_Allocate, &
                                      Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading Cloud record from '//TRIM( Filename )
      GOTO 1000  ! Clean up  
    END IF

    !#--------------------------------------------------------------------------#
    !#                   -- SAVE THE NUMBER OF SUBSETS READ --                  #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( n_Subsets ) ) n_Subsets = 1

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
    IF ( Associated_ODCAPS_Subset( ODCAPS_Subset ) ) THEN
      Destroy_Status = Destroy_ODCAPS_Subset( ODCAPS_Subset, &
                                           Message_Log = Message_Log )
    END IF
    
  END FUNCTION Read_ODCAPS_Subset_Scalar


  FUNCTION Read_ODCAPS_Subset_Rank1( Filename,    &  ! Input
                                 ODCAPS_Subset,   &  ! Output
                                 No_File_Close,     &  ! Optional input
                                 No_Allocate,       &  ! Optional input
                                 n_Subsets,         &  ! Optional output
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
    TYPE( ODCAPS_Subset_type ), DIMENSION( : ), INTENT( IN OUT ) :: ODCAPS_Subset

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )     :: No_File_Close
    INTEGER,        OPTIONAL, INTENT( IN )     :: No_Allocate

    ! -- Optional output
    INTEGER,        OPTIONAL, INTENT( OUT )    :: n_Subsets

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_ODCAPS_Subset_Binary(Rank-1)'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Yes_File_Close 
    INTEGER :: IO_Status
    INTEGER :: Destroy_Status
    INTEGER :: FileID
    INTEGER( Long )::m, n_Input_Subsets
 


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
                                     FileID,   &
                                     Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
       Message ='Error opening '//TRIM( Filename )  
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
    !#                       -- READ THE NUMBER OF SUBSETS --                   #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) n_Input_Subsets

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading n_Input_Subsets data dimension from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000  ! Clean up
    END IF

    ! -----------------------------------------
    ! Check if n_Subsets > size of output array
    ! -----------------------------------------

    IF ( n_Input_Subsets > size( ODCAPS_Subset ) ) THEN
      WRITE( Message, '( "Number of subsets ",i5," > size of the output ", &
                         &"structure array, ", i5, "." )' ) &
                      n_Input_Subsets, SIZE( ODCAPS_Subset ) 
      GOTO 1000  ! Clean up
    END IF

    !#--------------------------------------------------------------------------#
    !#                           -- LOOP OVER SUMSETS --                        #
    !#--------------------------------------------------------------------------#

    Subset_Loop: DO m = 1, n_Input_Subsets

      Error_Status = Read_ODCAPS_Subset_Record( FileID, &
                                        ODCAPS_Subset(m), &
                                        No_Allocate = No_Allocate, &
                                        Message_Log = Message_Log )!

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error reading subset element #", i5, " from ", a )' ) &
                        m, TRIM( Filename )
        GOTO 1000  ! Clean up
      END IF
 
    END DO Subset_Loop

 
    !#--------------------------------------------------------------------------#
    !#                   -- SAVE THE NUMBER OF SUBSETS READ --                  #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( n_Subsets ) ) n_Subsets = n_Input_Subsets

 
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
 
    Destroy_Status = Destroy_ODCAPS_Subset( ODCAPS_Subset, &
                                           Message_Log = Message_Log )
   
  END FUNCTION Read_ODCAPS_Subset_Rank1





  FUNCTION Write_ODCAPS_Subset_Scalar( Filename,     &  ! Input
                                  ODCAPS_Subset,     &  ! Input
                                  No_File_Close,       &  ! Optional input
                                  RCS_Id,              &  ! Revision control
                                  Message_Log )        &  ! Error messaging
                                RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )  :: Filename
    TYPE( ODCAPS_Subset_type ),    INTENT( IN )  :: ODCAPS_Subset

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_ODCAPS_Subset_Binary(Scalar)'
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


    !#--------------------------------------------------------------------------#
    !#             -- CHECK STRUCTURE POINTER ASSOCIATION STATUS --             #
    !#                                                                          #
    !#                ALL structure pointers must be associated                 #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. Associated_ODCAPS_Subset( ODCAPS_Subset ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT ODCAPS_Subset pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------------------------------
    ! Check the ODCAPS_Subset structure dimensions
    ! ---------------------------------------

    IF ( ODCAPS_Subset%n_Layers           < 1 .OR. &
         ODCAPS_Subset%n_Total_Predictors < 1 .OR. &
         ODCAPS_Subset%n_Absorbers        < 1 .OR. &
         ODCAPS_Subset%n_Channels         < 1      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'One or more dimensions of ODCAPS_Subset structure are < or = 0.', &
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
    !#                     -- WRITE THE NUMBER OF SUBSETS --                    #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) 1

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing n_Clouds data dimension to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      GOTO 1000
    END IF


    !#--------------------------------------------------------------------------#
    !#                        -- WRITE THE STRUCTURE DATA --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = Write_ODCAPS_Subset_Record( FileID, &
                                       ODCAPS_Subset, &
                                       Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing ODCAPS_Subset record to '//TRIM( Filename )
      GOTO 1000
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

  END FUNCTION Write_ODCAPS_Subset_Scalar
  

  FUNCTION Write_ODCAPS_Subset_Rank1( Filename,     &  ! Input
                                  ODCAPS_Subset,    &  ! Input
                                  No_File_Close,      &  ! Optional input
                                  RCS_Id,             &  ! Revision control
                                  Message_Log )       &  ! Error messaging
                                RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )  :: Filename
    TYPE( ODCAPS_Subset_type ), DIMENSION( : ), INTENT( IN )  :: ODCAPS_Subset

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_ODCAPS_Subset_Binary(Scalar)'
    CHARACTER( * ), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Yes_File_Close 
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: m



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
    ! Check the ODCAPS_Subset structure dimensions
    ! ---------------------------------------

    IF ( ANY( ODCAPS_Subset%n_Layers           < 1 ) .OR. &
         ANY( ODCAPS_Subset%n_Total_Predictors < 1 ) .OR. &
         ANY( ODCAPS_Subset%n_Absorbers        < 1 ) .OR. &
         ANY( ODCAPS_Subset%n_Channels         < 1 )     ) THEN
      Message = 'One or more dimensions of ODCAPS_Subset structure are < or = 0.'
      GOTO 1000
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
    !#                     -- WRITE THE NUMBER OF SUBSETS --                    #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) SIZE(ODCAPS_Subset)

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing n_Clouds data dimension to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      GOTO 2000
    END IF


    !#--------------------------------------------------------------------------#
    !#                        -- WRITE THE STRUCTURE DATA --                    #
    !#--------------------------------------------------------------------------#
    
    ODCAPS_Subset_Loop: DO m = 1, SIZE( ODCAPS_Subset )

      Error_Status = Write_ODCAPS_Subset_Record( FileID, &			    
        				 ODCAPS_Subset(m), &			    
        				 Message_Log = Message_Log )		    

      IF ( Error_Status /= SUCCESS ) THEN					    
        WRITE( Message, '( "Error writing Cloud element #", i5, " to ", a )' ) &
                      m, TRIM( Filename )
        GOTO 1000								    
      END IF									    
     
    END DO ODCAPS_Subset_Loop
  

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

  END FUNCTION Write_ODCAPS_Subset_Rank1

END MODULE ODCAPS_Subset_Binary_IO
