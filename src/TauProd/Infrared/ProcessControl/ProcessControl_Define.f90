!------------------------------------------------------------------------------
!M+
! NAME:
!       ProcessControl_Define
!
! PURPOSE:
!       Module defining the ProcessControl data structure and containing
!       routines to manipulate it.
!       
! CATEGORY:
!       Transmittance Production
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE ProcessControl_Define
!
! MODULES:
!       Type_Kinds:            Module containing definitions for kinds
!                              of variable types.
!
!       Message_Handler:         Module to define simple error codes and
!                              handle error conditions
!                              USEs: FILE_UTILITY module
!
! CONTAINS:
!       Associated_ProcessControl: Function to test the association status
!                                  of the pointer members of a ProcessControl
!                                  structure.
!
!       Destroy_ProcessControl:    Function to re-initialize an ProcessControl
!                                  structure.
!
!       Allocate_ProcessControl:   Function to allocate the pointer members
!                                  of an ProcessControl structure.
!
!       Assign_ProcessControl:     Function to copy an ProcessControl structure.
!
!
! DERIVED TYPES:
!       ProcessControl_List_type: Definition of the ProcessControl_List data
!       ------------------------  structure. Fields are,
!
!         File_Index:          Array index position of this list structure member
!                              in the main ProcesControl structure.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!
!         Channel:             Sensor channel number.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!
!         Begin_LBLband:       LBL band that contains the begin frequency
!                              of the channel SRF. Along with End_LBLband,
!                              defines the range of LBL data required to 
!                              process the channel.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!
!         End_LBLband:         LBL band that contains the end frequency
!                              of the channel SRF. Along with Begin_LBLband,
!                              defines the range of LBL data required to 
!                              process the channel.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!
!         Data_Available:      Logical flag used to determine if all the
!                              required data to process the channel is
!                              available.
!                              UNITS:      N/A
!                              TYPE:       LOGICAL
!                              DIMENSION:  Scalar
!
!         Processed:           Integer flag used to indicate if the channel
!                              has been processed.
!                              If == 0, channel has not been processed
!                                 == 1, channel has been processed.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!
!
!       ProcessControl_type: Definition of the ProcessControl data structure.
!       -------------------  Fields are,
!
!         StrLen:              Length of structure character string members.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!
!         n_Files:             The number of sensor SRF and TauProfile files
!                              represented in the structure.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!
!         File_Prefix:         Character string array identifying the file prefixes
!                              for the SRF and TauProfile dat files.
!                              UNITS:      N/A
!                              TYPE:       CHARACTER( StrLen )
!                              DIMENSION:  Rank-1 (n_Files)
!                              ATTRIBUTES: POINTER
!
!         SRF_Filename:        Character string array containing the sensor
!                              SRF filenames.
!                              UNITS:      N/A
!                              TYPE:       CHARACTER( StrLen )
!                              DIMENSION:  Rank-1 (n_Files)
!                              ATTRIBUTES: POINTER
!
!         TauProfile_Filename: Character string array containing the sensor
!                              transmittance profile filenames.
!                              UNITS:      N/A
!                              TYPE:       CHARACTER( StrLen )
!                              DIMENSION:  Rank-1 (n_Files)
!                              ATTRIBUTES: POINTER
!
!         dF_Index:            Integer array containing the frequency
!                              interval index for each sensor.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Rank-1 (n_Files)
!                              ATTRIBUTES: POINTER
!
!         Channel_Index:       Integer array containing the begin and end
!                              array index positions of the channels for each
!                              sensor in the ProcessControl List array.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Rank-2 (2,n_Files)
!                              ATTRIBUTES: POINTER
!
!         n_Channels:          Total number of sensor channels represented in
!                              the structure.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!
!         List:                Structure array defining the Transmittance
!                              production processing parameters for all the
!                              sensor channels.
!                              UNITS:      N/A
!                              TYPE:       ProcessControl_List_type
!                              DIMENSION:  Rank-1 (n_Channels)
!                              ATTRIBUTES: POINTER
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
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Apr-2002
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2001 Paul van Delst
!
!M-
!------------------------------------------------------------------------------

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


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!----------------------------------------------------------------------------------
!
! NAME:
!       Clear_ProcessControl
!
! PURPOSE:
!       Subroutine to clear the scalar members of a ProcessControl structure.
!
! CATEGORY:
!       Transmittance Production : Process Control
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Clear_ProcessControl( ProcessControl )
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       ProcessControl:  ProcessControl structure for which the scalar members have
!                        been cleared.
!                        UNITS:      N/A
!                        TYPE:       ProcessControl_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
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
! COMMENTS:
!       Note the INTENT on the output ProcessControl argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 20-May-2002
!                       paul.vandelst@ssec.wisc.edu
!
!----------------------------------------------------------------------------------

  SUBROUTINE Clear_ProcessControl( ProcessControl )

    TYPE( ProcessControl_type ), INTENT( IN OUT ) :: ProcessControl

    ProcessControl%StrLen = SL

    ProcessControl%n_Files    = 0
    ProcessControl%n_Channels = 0

  END SUBROUTINE Clear_ProcessControl





!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!S+
! NAME:
!       Associated_ProcessControl
!
! PURPOSE:
!       Function to test if ALL the pointer members of a ProcessControl structure
!       are associated.
!
! CATEGORY:
!       Transmittance Production : Process Control
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Association_Status = Associated_ProcessControl( ProcessControl )  ! Input
!
! INPUT ARGUMENTS:
!       ProcessControl:   ProcessControl structure which is to have its
!                         pointer member's association status tested.
!                         UNITS:      N/A
!                         TYPE:       ProcessControl_type
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
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
!       Association_Status:  The return value is a logical value indicating the
!                            association status of the ProcessControl pointer members.
!                            .TRUE.  - if ALL the ProcessControl pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the ProcessControl pointer
!                                      members are associated.
!                            .FALSE. - some or all of the ProcessControl pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
! CALLS:
!       Display_Message:      Subroutine to output Messages
!                             SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       This function tests the association status of the ProcessControl structure
!       pointer members, Therefore this function must only be called after
!       the input ProcessControl structure has had its pointer members initialized.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 05-Sep-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

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


!------------------------------------------------------------------------------
!S+
! NAME:
!       Destroy_ProcessControl
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of a
!       ProcessControl data structure.
!
! CATEGORY:
!       Transmittance Production : Process Control
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_ProcessControl( ProcessControl,           &  ! Output
!                                              RCS_Id = RCS_Id,          &  ! Revision control
!                                              Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      None
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       ProcessControl:  Re-initialised ProcessControl structure.
!                        UNITS:      N/A
!                        TYPE:       ProcessControl_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:          Character string containing the Revision Control
!                        System Id field for the module.
!                        UNITS:      None
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the structure re-initialisation was successful
!                        == FAILURE - an error occurred, or
!                                   - the structure internal allocation counter
!                                     is not equal to zero (0) upon exiting this
!                                     function. This value is incremented and
!                                     decremented for every structure allocation
!                                     and deallocation respectively.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Clear_ProcessControl:  Subroutine to initialize the scalar members
!                              of the Process Control structure.
!
!       Display_Message:       Subroutine to output messages
!                              SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output ProcessControl argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Apr-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

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





!------------------------------------------------------------------------------
!S+
! NAME:
!       Allocate_ProcessControl
! 
! PURPOSE:
!       Function to allocate the pointer members of the ProcessControl
!       data structure.
!
! CATEGORY:
!       Transmittance Production : Process Control
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_ProcessControl( n_Files,                  &  ! Input
!                                               n_Channels,               &  ! Input
!                                               ProcessControl,           &  ! Output
!                                               RCS_Id = RCS_Id,          &  ! Revision control
!                                               Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Files:         Number of data files to be represented in the
!                        ProcessControl data structure.
!                        Must be > 0.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       n_Channels:      Total number of spectral channels to be represented
!                        in the ProcessControl data structure.
!                        Must be > 0.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      None
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       ProcessControl:  Process Control structure with allocated
!                        pointer members
!                        UNITS:      N/A
!                        TYPE:       ProcessControl_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:          Character string containing the Revision Control
!                        System Id field for the module.
!                        UNITS:      None
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the structure pointer allocations were
!                                   successful
!                        == FAILURE - an error occurred, or
!                                   - the structure internal allocation counter
!                                     is not equal to one (1) upon exiting this
!                                     function. This value is incremented and
!                                     decremented for every structure allocation
!                                     and deallocation respectively.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Associated_ProcessControl:  Function to test the association status of
!                                   the pointer members of a ProcessControl
!                                   structure.
!
!       Destroy_ProcessControl:     Function to re-initialize the scalar and
!                                   pointer members of ProcessControl data
!                                   structures.
!
!       Display_Message:            Subroutine to output messages
!                                   SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output ProcessControl argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 08-Jan-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

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





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Assign_ProcessControl
!
! PURPOSE:
!       Function to copy valid ProcessControl structures.
!
! CATEGORY:
!       Transmitance Production : ProcessControl
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Assign_ProcessControl( ProcessControl_in,        &  ! Input
!                                             ProcessControl_out,       &  ! Output
!                                             RCS_Id = RCS_Id,          &  ! Revision control
!                                             Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       ProcessControl_in:   ProcessControl structure which is to be copied.
!                            UNITS:      N/A
!                            TYPE:       ProcessControl_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:         Character string specifying a filename in which any
!                            messages will be logged. If not specified, or if an
!                            error occurs opening the log file, the default action
!                            is to output messages to standard output.
!                            UNITS:      None
!                            TYPE:       CHARACTER(*)
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       ProcessControl_out:  Copy of the input structure, ProcessControl_in.
!                            UNITS:      N/A
!                            TYPE:       ProcessControl_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:              Character string containing the Revision Control
!                            System Id field for the module.
!                            UNITS:      None
!                            TYPE:       CHARACTER(*)
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the structure assignment was successful
!                        == FAILURE an error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Associated_ProcessControl:  Function to test the association status of
!                                   the pointer members of a ProcessControl
!                                   structure.
!
!       Allocate_ProcessControl:    Function to allocate the pointer members of
!                                   the ProcessControl data structure.
!
!       Display_Message:            Subroutine to output messages
!                                   SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output ProcessControl argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 09-May-2005
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

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


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2006/06/30 16:47:16 $
!
! $Revision: 1.7 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: ProcessControl_Define.f90,v $
! Revision 1.7  2006/06/30 16:47:16  dgroff
! Changed "Error_Handler" references to "Message_Handler"
!
! Revision 1.6  2005/05/15 23:28:01  paulv
! - Added dF_Index to PRocessControl structure definition.
! - Renamed Sensor_Platform_ID component of ProcessControl structure
!   to File_Index.
!
! Revision 1.5  2005/05/11 13:17:39  paulv
! - Upgraded to Fortran-95
! - Removed Initialization() subroutine.
! - Made Associated() function PUBLIC.
! - Added Assign() function.
! - Derived type name root changed from "PC" to "ProcessControl"
! - Separated out the structure component deallocations in the Destroy() function.
!
! Revision 1.4  2003/09/05 16:21:12  paulv
! - Removed definition of derived type PCfile_type. The processing software
!   no longer needs to keep track of the various file IDs so there was no need
!   to retain this derived type. The PC_type has been altered to reflect these
!   changes with the SRF and TauProfile PCfile_type members being replaced
!   by SRF_Filename and TauProfile_Filename.
! - Added an allocation counter to the PC_type definition. This counter is
!   incremented and checked against the value 1 in the Allocate() function
!   and decremented and checked against the value 0 in the Destroy() function.
!   A warning is issued if the allocation counter does not have the expected
!   value.
! - Added the string length as a parameter and defined a member for it in the
!   PC_type definition.
! - Added the Associated_PC() PRIVATE function.
! - Added the RCS_Id optional output argument to all the public routines.
!
! Revision 1.3  2002/06/19 17:02:58  paulv
! - Added Sensor_Platform_ID pointer component to the PC data type.
!
! Revision 1.2  2002/06/05 19:19:53  paulv
! - Removed MESSAGE as a module variable and placed definitions in each
!   module subprogram.
!
! Revision 1.1  2002/05/30 20:00:28  paulv
! Initial checkin.
!
!
!
!
!
