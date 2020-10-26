
MODULE ProcessControl_Define


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Message_Handler


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  ! -- Everything private by default
  PRIVATE

  ! -- Public procedures to manipulate the ProcessControl structure
  PUBLIC :: Associated_ProcessControl
  PUBLIC :: Destroy_ProcessControl
  PUBLIC :: Allocate_ProcessControl
  PUBLIC :: Assign_ProcessControl


  ! -----------------
  ! MOdule parameters
  ! -----------------

  ! -- RCS Id field
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: ProcessControl_Define.f90,v 1.7 2006/06/30 16:47:16 dgroff Exp $'

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1

  ! -- ProcessControl invalid values
  INTEGER, PRIVATE, PARAMETER :: INVALID = -1

  ! -- ProcessControl character strings length
  INTEGER, PRIVATE, PARAMETER :: SL = 80


  ! -------------
  ! Derived types
  ! -------------

  TYPE, PUBLIC :: ProcessControl_List_type
    INTEGER :: File_Index     = INVALID
    INTEGER :: Channel        = INVALID
    INTEGER :: Begin_LBLband  = INVALID
    INTEGER :: End_LBLband    = INVALID
    LOGICAL :: Data_Available = .FALSE.
    INTEGER :: Processed      = INVALID
  END TYPE ProcessControl_List_type


  TYPE, PUBLIC :: ProcessControl_type
    INTEGER :: n_Allocates = 0

    INTEGER :: StrLen  = SL
    INTEGER :: n_Files = 0
    CHARACTER( SL ), DIMENSION( : ),    POINTER :: File_Prefix         => NULL()
    CHARACTER( SL ), DIMENSION( : ),    POINTER :: SRF_Filename        => NULL()
    CHARACTER( SL ), DIMENSION( : ),    POINTER :: TauProfile_Filename => NULL()
    INTEGER,         DIMENSION( : ),    POINTER :: dF_Index            => NULL()
    INTEGER,         DIMENSION( :, : ), POINTER :: Channel_Index       => NULL()

    INTEGER :: n_Channels = 0
    TYPE( ProcessControl_List_type ), DIMENSION( : ), POINTER :: List => NULL()

  END TYPE ProcessControl_type


CONTAINS




  SUBROUTINE Clear_ProcessControl( ProcessControl )

    TYPE( ProcessControl_type ), INTENT( IN OUT ) :: ProcessControl

    ProcessControl%StrLen = SL

    ProcessControl%n_Files    = 0
    ProcessControl%n_Channels = 0

  END SUBROUTINE Clear_ProcessControl







  FUNCTION Associated_ProcessControl( ProcessControl, &  ! Input
                                      ANY_Test )      &  ! Optional input
                                    RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( ProcessControl_type ), INTENT( IN ) :: ProcessControl

    ! -- Optional input
    INTEGER,           OPTIONAL, INTENT( IN ) :: ANY_Test


    ! ---------------
    ! Function result
    ! ---------------

    LOGICAL :: Association_Status


    ! ---------------
    ! Local variables
    ! ---------------

    LOGICAL :: ALL_Test



    !#--------------------------------------------------------------------------#
    !#                           -- CHECK INPUT --                              #
    !#--------------------------------------------------------------------------#

    ! -- Default is to test ALL the pointer members
    ! -- for a true association status....
    ALL_Test = .TRUE.

    ! -- ...unless the ANY_Test argument is set.
    IF ( PRESENT( ANY_Test ) ) THEN
      IF ( ANY_Test == SET ) ALL_Test = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- TEST THE STRUCTURE POINTER MEMBER ASSOCIATION --            #
    !#--------------------------------------------------------------------------#

    Association_Status = .FALSE.

    IF ( ALL_Test ) THEN

      IF ( ASSOCIATED( ProcessControl%File_Prefix         ) .AND. &
           ASSOCIATED( ProcessControl%SRF_Filename        ) .AND. &
           ASSOCIATED( ProcessControl%TauProfile_Filename ) .AND. &
           ASSOCIATED( ProcessControl%dF_Index            ) .AND. &
           ASSOCIATED( ProcessControl%Channel_Index       ) .AND. &
           ASSOCIATED( ProcessControl%List                )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( ProcessControl%File_Prefix         ) .OR. &
           ASSOCIATED( ProcessControl%SRF_Filename        ) .OR. &
           ASSOCIATED( ProcessControl%TauProfile_Filename ) .OR. &
           ASSOCIATED( ProcessControl%dF_Index            ) .OR. &
           ASSOCIATED( ProcessControl%Channel_Index       ) .OR. &
           ASSOCIATED( ProcessControl%List                )      ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION Associated_ProcessControl



  FUNCTION Destroy_ProcessControl( ProcessControl, &  ! Output
                                   RCS_Id,         &  ! Revision control
                                   Message_Log )   &  ! Error messaging
                                 RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Output
    TYPE( ProcessControl_type ), INTENT( IN OUT ) :: ProcessControl

    ! -- Revision control
    CHARACTER( * ),    OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),    OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_ProcessControl'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    INTEGER :: Allocate_Status



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
    !#            -- TEST THE INPUT STRUCTURE POINTER ASSOCIATION --            #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. Associated_ProcessControl( ProcessControl ) ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Not all input ProcessControl pointer members are associated.', &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- PERFORM REINITIALISATION --                      #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Initialise the scalar members
    ! -----------------------------

    CALL Clear_ProcessControl( ProcessControl )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. Associated_ProcessControl( ProcessControl ) ) RETURN


    ! -----------------------------------------
    ! Deallocate the pointer members
    ! -----------------------------------------

    ! -- Deallocate the File_Prefix
    IF ( ASSOCIATED( ProcessControl%File_Prefix ) ) THEN

      DEALLOCATE( ProcessControl%File_Prefix, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ProcessControl File_Prefix ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the SRF_Filename
    IF ( ASSOCIATED( ProcessControl%SRF_Filename ) ) THEN

      DEALLOCATE( ProcessControl%SRF_Filename, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ProcessControl SRF_Filename ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the TauProfile_Filename
    IF ( ASSOCIATED( ProcessControl%TauProfile_Filename ) ) THEN

      DEALLOCATE( ProcessControl%TauProfile_Filename, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ProcessControl TauProfile_Filename ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the dF_Index
    IF ( ASSOCIATED( ProcessControl%dF_Index ) ) THEN

      DEALLOCATE( ProcessControl%dF_Index, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ProcessControl dF_Index ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the Channel_Index
    IF ( ASSOCIATED( ProcessControl%Channel_Index ) ) THEN

      DEALLOCATE( ProcessControl%Channel_Index, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ProcessControl Channel_Index ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the List
    IF ( ASSOCIATED( ProcessControl%List ) ) THEN

      DEALLOCATE( ProcessControl%List, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ProcessControl List ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF



    !#--------------------------------------------------------------------------#
    !#               -- DECREMENT AND TEST ALLOCATION COUNTER --                #
    !#--------------------------------------------------------------------------#

    ProcessControl%n_Allocates = ProcessControl%n_Allocates - 1

    IF ( ProcessControl%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      ProcessControl%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_ProcessControl






  FUNCTION Allocate_ProcessControl( n_Files,        &  ! Input
                                    n_Channels,     &  ! Input               
                                    ProcessControl, &  ! Output  
                                    RCS_Id,         &  ! Revision control    
                                    Message_Log )   &  ! Error messaging     
                                  RESULT ( Error_Status )                  



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                     INTENT( IN )     :: n_Files
    INTEGER,                     INTENT( IN )     :: n_Channels

    ! -- Output
    TYPE( ProcessControl_type ), INTENT( IN OUT ) :: ProcessControl

    ! -- Revision control
    CHARACTER( * ),    OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),    OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_ProcessControl'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: Allocate_Status



    !#--------------------------------------------------------------------------#
    !#                  -- SET SUCCESSFUL RETURN STATUS --                      #
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
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! ----------
    ! Dimensions
    ! ----------

    IF ( n_Files < 1 .OR. n_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Files and N_CHANNELS must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------------------------------------
    ! Check if ANY pointers are already associated
    ! If they are, destroy the structure.
    ! --------------------------------------------

    IF ( Associated_ProcessControl( ProcessControl, ANY_Test = SET ) ) THEN

      Error_Status = Destroy_ProcessControl( ProcessControl, &
                                             Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error destroying ProcessControl structure.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATIONS --                      #
    !#--------------------------------------------------------------------------#

    ALLOCATE( ProcessControl%File_Prefix( n_Files ), &
              ProcessControl%SRF_Filename( n_Files ), &
              ProcessControl%TauProfile_Filename( n_Files ), &
              ProcessControl%dF_Index( n_Files ), &
              ProcessControl%Channel_Index( 2, n_Files ), &
              ProcessControl%List( n_Channels ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allociating ProcessControl pointer members. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- ASSIGN THE DIMENSIONS --                         #
    !#--------------------------------------------------------------------------#

    ProcessControl%n_Files    = n_Files
    ProcessControl%n_Channels = n_Channels



    !#--------------------------------------------------------------------------#
    !#             -- FILL THE POINTER MEMBERS WITH INVALID VALUES --           #
    !#--------------------------------------------------------------------------#

    ProcessControl%File_Prefix         = ' '
    ProcessControl%SRF_Filename        = ' '
    ProcessControl%TauProfile_Filename = ' '
    ProcessControl%dF_Index            = INVALID
    ProcessControl%Channel_Index       = INVALID
    ProcessControl%List                = ProcessControl_List_type( &
                                           INVALID, &  ! File_Index
                                           INVALID, &  ! Channel
                                           INVALID, &  ! Begin_LBLband
                                           INVALID, &  ! End_LBLband
                                           .FALSE., &  ! Data_Available
                                           INVALID  )  ! Processed



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    ProcessControl%n_Allocates = ProcessControl%n_Allocates + 1

    IF ( ProcessControl%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      ProcessControl%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_ProcessControl






  FUNCTION Assign_ProcessControl( ProcessControl_in,  &  ! Input
                                  ProcessControl_out, &  ! Output
                                  RCS_Id,             &  ! Revision control
                                  Message_Log )       &  ! Error messaging
                                RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( ProcessControl_type ), INTENT( IN )     :: ProcessControl_in

    ! -- Output
    TYPE( ProcessControl_type ), INTENT( IN OUT ) :: ProcessControl_out

    ! -- Revision control
    CHARACTER( * ),    OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),    OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_ProcessControl'



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- TEST THE STRUCTURE ARGUMENT POINTER ASSOCIATION --          #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------------
    ! ALL *input* pointers must be associated
    ! ---------------------------------------

    IF ( .NOT. Associated_ProcessControl( ProcessControl_In ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT ProcessControl pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! Allocate the structure
    ! ----------------------

    Error_Status = Allocate_ProcessControl( ProcessControl_in%n_Files,    &
                                            ProcessControl_in%n_Channels, &
                                            ProcessControl_out,           &
                                            Message_Log = Message_Log     )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output ProcessControl arrays.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------
    ! Assign array data
    ! -----------------

    ProcessControl_out%File_Prefix         = ProcessControl_in%File_Prefix 
    ProcessControl_out%SRF_Filename        = ProcessControl_in%SRF_Filename       
    ProcessControl_out%TauProfile_Filename = ProcessControl_in%TauProfile_Filename
    ProcessControl_out%dF_Index            = ProcessControl_in%dF_Index      
    ProcessControl_out%Channel_Index       = ProcessControl_in%Channel_Index      
    ProcessControl_out%List                = ProcessControl_in%List               

  END FUNCTION Assign_ProcessControl

END MODULE ProcessControl_Define


