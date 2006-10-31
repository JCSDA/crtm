!------------------------------------------------------------------------------
!M+
! NAME:
!       SensorInfo_LinkedList
!
! PURPOSE:
!       Module containing type definitions for a SensorInfo linked list
!       and routines to manipulate it. 
!
! CATEGORY:
!       Instrument Information : SensorInfo
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE SensorInfo_LinkedList
!
! MODULES:
!       Type_Kinds:         Module containing definitions for kinds
!                           of variable types.
!
!       Message_Handler:    Module to define simple error codes and
!                           handle error conditions
!                           USEs: FILE_UTILITY module
!
!       SensorInfo_Define:  Module defining the SensorInfo data structure
!                           and containing routines to manipulate it.
!                           USEs: TYPE_KINDS module
!                                 ERROR_HANDLER module
!
! CONTAINS:
!       New_SensorInfo_List:      Function to return an initialised
!                                 SensorInfo linked list.
!
!       Destroy_SensorInfo_List:  Function to destroy a SensorInfo linked
!                                 list.
!
!       AddTo_SensorInfo_List:    Function to ADD a SensorInfo node TO a
!                                 SensorInfo linked list.
!
!       GetFrom_SensorInfo_List:  Function to GET a SensorInfo node FROM a
!                                 SensorInfo linked list.
!
!       Count_SensorInfo_Nodes:   Function to count the number of nodes in
!                                 a SensorInfo linked list.
!
! DERIVED TYPES:
!       SensorInfo_List_type:   Definition of the SensorInfo_List data
!                               structure. The type definition is PUBLIC
!                               but the structure contents are PRIVATE
!                               and can only be accessed via the PUBLIC
!                               routines of this module.
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
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Apr-2003
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2003 Paul van Delst
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

MODULE SensorInfo_LinkedList


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler

  USE SensorInfo_Define


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE

  ! -- Data type
  PUBLIC :: SensorInfo_List_type

  ! -- Methods
  PUBLIC :: New_SensorInfo_List
  PUBLIC :: Destroy_SensorInfo_List
  PUBLIC :: AddTo_SensorInfo_List
  PUBLIC :: GetFrom_SensorInfo_List
  PUBLIC :: Count_SensorInfo_Nodes


  ! ------------------------
  ! Derived type definitions
  ! ------------------------

  ! -- Node definition
  TYPE :: SensorInfo_Node_type
    TYPE( SensorInfo_type )               :: SensorInfo          ! Node data
    TYPE( SensorInfo_Node_type ), POINTER :: Previous => NULL()  ! Pointer to previous node
    TYPE( SensorInfo_Node_type ), POINTER :: Next     => NULL()  ! Pointer to next node
  END TYPE SensorInfo_Node_type

  ! -- Linked list definition
  TYPE :: SensorInfo_List_type
    PRIVATE
    INTEGER                               :: n_Nodes = 0        ! The number of SensorInfo nodes
    TYPE( SensorInfo_Node_type ), POINTER :: First   => NULL()  ! Pointer to the first node
  END TYPE SensorInfo_List_type



  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: SensorInfo_LinkedList.f90,v 1.11 2006/09/06 15:09:09 frpv Exp $'

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: UNSET = 0
  INTEGER, PRIVATE, PARAMETER ::   SET = 1


CONTAINS





!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!------------------------------------------------------------------------------
!
! NAME:
!       List_Is_Empty
!
! PURPOSE:
!       Function to determine if a SensorInfo linked list is empty.
!
! CATEGORY:
!       Instrument Information : SensorInfo
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Empty_Status = List_Is_Empty( SensorInfo_List )  ! Input
!
! INPUT ARGUMENTS:
!       SensorInfo_List:  The SensorInfo linked list.
!                         UNITS:      N/A
!                         TYPE:       SensorInfo_List_type
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
!       Empty_Status:  The return value is a logical value indicating the
!                      status of the SensorInfo linked list.
!                      .TRUE.  - the list is empty with no valid nodes.
!                      .FALSE. - the list is not empty and contains valid
!                                nodes.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       This function checks the association status of various components
!       of the linked list. Thus this function should only be called after
!       a list has at least been initialised.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Apr-2003
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION List_Is_Empty( SensorInfo_List ) RESULT( Boolean )


    ! --------------
    ! Input argument
    ! --------------

    TYPE( SensorInfo_List_type ), INTENT( IN ) :: SensorInfo_List


    ! ---------------
    ! Function result
    ! ---------------

    LOGICAL :: Boolean


    ! ----------------------------
    ! Is there a valid first node?
    ! ----------------------------

    Boolean = .NOT. ASSOCIATED( SensorInfo_List%First%Next )

  END FUNCTION List_Is_Empty





!------------------------------------------------------------------------------
!
! NAME:
!       Get_Node_Pointer
!
! PURPOSE:
!       Subroutine to traverse a SensorInfo linked list to a specified
!       node and return a pointer to that node.
!
! CATEGORY:
!       Instrument Information : SensorInfo
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Get_Node_Pointer( SensorInfo_List, &  ! Input
!                              Node_Numnber,    &  ! Input
!                              Node_Pointer     )  ! Output
!
! INPUT ARGUMENTS:
!       SensorInfo_List:  The SensorInfo linked list.
!                         UNITS:      N/A
!                         TYPE:       SensorInfo_List_type
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       Node_Number:      The SensorInfo_List node for which a pointer
!                         is required.
!                         UNITS:      None
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Node_Pointer:     The pointer to the requested SensorInfo node
!                         in the linked list. The dummy argument is not
!                         nullified so the actual argument should be
!                         nullified BEFORE calling this routine. 
!                         * Note that pointer dummy arguments cannot have
!                           an INTENT attribute. However, the programmer's
!                           intent of this argument is for OUTPUT.
!                         UNITS:      N/A
!                         TYPE:       SensorInfo_Node_type
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: POINTER
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! CALLS:
!       Count_SensorInfo_Nodes:  Function to count the number of nodes
!                                in the SensorInfo linked list.
!
!       List_Is_Empty:           Logical function to determine if the list
!                                is empty.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       This function checks the association status of various components
!       of the linked list. Thus this function should only be called after
!       a list has at least been initialised.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Apr-2003
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  SUBROUTINE Get_Node_Pointer( SensorInfo_List, &
                               Node_Number, &
                               Node_Pointer )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( SensorInfo_List_type ), INTENT( IN ) :: SensorInfo_List
    INTEGER,                      INTENT( IN ) :: Node_Number

    ! -- Output
    TYPE( SensorInfo_Node_type ), POINTER      :: Node_Pointer    ! INTENT( OUT )


    ! ---------------
    ! Local variables
    ! ---------------

    TYPE( SensorInfo_Node_type ), POINTER :: Current
    INTEGER :: n_Nodes



    !#--------------------------------------------------------------------------#
    !#                      -- NULLIFY LOCAL POINTERS --                        #
    !#--------------------------------------------------------------------------#

    NULLIFY( Current )



    !#--------------------------------------------------------------------------#
    !#                          -- CHECK INPUT --                               #
    !#--------------------------------------------------------------------------#

    ! -----------------------
    ! Initialise node counter
    ! -----------------------

    n_Nodes = 0


    ! -----------
    ! Check input
    ! -----------

    IF ( Node_Number < 1 ) RETURN
    IF ( Node_Number > SensorInfo_List%n_Nodes ) RETURN
    IF ( List_Is_Empty( SensorInfo_List ) ) RETURN



    !#--------------------------------------------------------------------------#
    !#                -- RETRIEVE THE REQUIRED NODE POINTER --                  #
    !#--------------------------------------------------------------------------#

    ! --------------------------------
    ! Initialise pointer to first node
    ! --------------------------------

    Current => SensorInfo_List%First%Next


    ! -------------
    ! Traverse list
    ! -------------

    DO


      ! -----------------------------------
      ! At end of list before required node
      ! -----------------------------------

      IF ( .NOT. ASSOCIATED( Current ) ) THEN
        RETURN
      END IF


      ! ----------------------
      ! Increment node counter
      ! ----------------------

      n_Nodes = n_Nodes + 1


      ! -------------------------------------
      ! Is the current node the one required?
      ! -------------------------------------

      IF ( n_Nodes == Node_Number ) THEN
        EXIT
      END IF


      ! ---------------
      ! Go to next node
      ! ---------------

      Current => Current%Next

    END DO


    ! ---------------------------------------
    ! Point return argument to requested node
    ! ---------------------------------------

    Node_Pointer => Current

  END SUBROUTINE Get_Node_Pointer





!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!S+
! NAME:
!       New_SensorInfo_List
!
! PURPOSE:
!       Function to return an initialised SensorInfo linked list.
!
! CATEGORY:
!       Instrument Information : SensorInfo
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       SensorInfo_List = New_SensorInfo_List( RCS_Id      = RCS_Id,     &  ! Revision control
!                                              Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:      Character string specifying a filename in which any
!                         messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output messages to standard output.
!                         UNITS:      None
!                         TYPE:       Character
!                         DIMENSION:  Scalar, LEN = *
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       SensorInfo_List:  The initialised (but empty) SensorInfo linked list.
!                         UNITS:      N/A
!                         TYPE:       SensorInfo_List_type
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:           Character string containing the Revision Control
!                         System Id field for the module.
!                         UNITS:      None
!                         TYPE:       CHARACTER
!                         DIMENSION:  Scalar, LEN = *
!                         ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!
! FUNCTION RESULT:
!       SensorInfo_List:  The initialised (but empty) SensorInfo linked list.
!                         UNITS:      N/A
!                         TYPE:       SensorInfo_List_type
!                         DIMENSION:  Scalar
!
! CALLS:
!       Display_Message:        Subroutine to output messages
!                               SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! EXAMPLE:
!       TYPE( SensorInfo_List_type ) :: SensorInfo_List
!
!       SensorInfo_List = New_SensorInfo_List()
!      
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Apr-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION New_SensorInfo_List( RCS_Id,       &  ! Revision control
                                Message_Log ) &  ! Error messaging
                              RESULT ( SensorInfo_List )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error handler message log
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    TYPE( SensorInfo_List_type ) :: SensorInfo_List


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'New_SensorInfo_List'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: Allocate_Status



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                -- INITIALISE THE SensorInfo LINKED LIST --               #
    !#--------------------------------------------------------------------------#

    ! ----------------------------------------
    ! Set the number of sensors(nodes) to zero
    ! ----------------------------------------

    SensorInfo_List%n_Nodes = 0

    
    ! ----------------------------------------
    ! Nullify the First pointer...just in case 
    ! ----------------------------------------

    NULLIFY( SensorInfo_List%First )


    ! ---------------------------------
    ! Allocate space for the first node
    ! ---------------------------------

    ALLOCATE( SensorInfo_List%First, STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '( "Error allocating SensorInfo_List First ", &
                        &"member. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            WARNING, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -------------------------
    ! Nullify the node pointers
    ! -------------------------

    NULLIFY( SensorInfo_List%First%Previous, &
             SensorInfo_List%First%Next      )

  END FUNCTION New_SensorInfo_List





!------------------------------------------------------------------------------
!S+
! NAME:
!       Destroy_SensorInfo_List
!
! PURPOSE:
!       Function to destroy a SensorInfo linked list.
!
! CATEGORY:
!       Instrument Information : SensorInfo
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_status = Destroy_SensorInfo_List( SensorInfo_List,          &  ! Output
!                                               Quiet       = Quiet,      &  ! Optional input
!                                               RCS_Id      = RCS_Id,     &  ! Revision control
!                                               Message_Log = Message_Log )  ! Error messaging
!
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:            Set this keyword to suppress information Messages being
!                         printed to standard output (or the Message log file if
!                         the Message_Log optional argument is used.) By default,
!                         information Messages are printed.
!                         If QUIET = 0, information Messages are OUTPUT.
!                            QUIET = 1, information Messages are SUPPRESSED.
!                         UNITS:      None
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:      Character string specifying a filename in which any
!                         messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output messages to standard output.
!                         UNITS:      None
!                         TYPE:       Character
!                         DIMENSION:  Scalar, LEN = *
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       SensorInfo_List:  The destroyed SensorInfo linked list.
!                         UNITS:      N/A
!                         TYPE:       SensorInfo_List_type
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:           Character string containing the Revision Control
!                         System Id field for the module.
!                         UNITS:      None
!                         TYPE:       CHARACTER
!                         DIMENSION:  Scalar, LEN = *
!                         ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error status.
!                         The error codes are defined in the ERROR_HANDLER module.
!                         If == SUCCESS the list destruction was successful,
!                            == FAILURE an unrecoverable error occurred.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
! CALLS:
!       Destroy_SensorInfo:   Function to destroy a SensorInfo structure.
!                             SOURCE: SENSORINFO_DEFINE module
!
!       Display_Message:      Subroutine to output messages
!                             SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! COMMENTS:
!       Note the INTENT on the output SensorInfo_List argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined
!       (at least its components may be) upon input. To prevent memory leaks,
!       the IN OUT INTENT is a must.
!
! EXAMPLE:
!       TYPE( SensorInfo_List_type ) :: SensorInfo_List
!       INTEGER                      :: Error_Status
!
!       ! -- Create a list
!       SensorInfo_List = New_SensorInfo_List( SensorInfo_List )
!
!       ....Work with linked list....
!
!       ! -- Destroy the list.
!       Error_Status = Destroy_SensorInfo_List( SensorInfo_List )
!       IF ( Error_Status /= SUCCESS ) THEN
!         PRINT *, '** SensorInfo_List DESTRUCTION ERROR**'
!         STOP
!       END IF
!         
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Apr-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Destroy_SensorInfo_List( SensorInfo_List, &  ! Output
                                    Quiet,           &  ! Optional input
                                    RCS_Id,          &  ! Revision control
                                    Message_Log )    &  ! Error messaging
                                  RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Output
    TYPE( SensorInfo_List_type ), INTENT( IN OUT ) :: SensorInfo_List

    ! -- Optional input
    INTEGER,            OPTIONAL, INTENT( IN )     :: Quiet

    ! -- Revision control
    CHARACTER( * ),     OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! -- Error handler message log
    CHARACTER( * ),     OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_SensorInfo_List'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message 
    LOGICAL :: Noisy
    INTEGER :: Allocate_Status

    INTEGER :: n_Nodes

    TYPE( SensorInfo_Node_type ), POINTER :: Current



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
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
    !#                      -- CHECK OPTIONAL ARGUMENTS --                      #
    !#--------------------------------------------------------------------------#

    ! -- Output informational messages....
    Noisy = .TRUE.
    ! -- ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- CHECK THE LINKED LIST HEADER --                    #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. ASSOCIATED( SensorInfo_List%First ) ) THEN
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                 -- DESTROY THE SensorInfo LINKED LIST --                 #
    !#--------------------------------------------------------------------------#

    ! ---------------------------
    ! Initialise the node counter
    ! ---------------------------

    n_Nodes = 0


    ! -----------------
    ! Traverse the list
    ! -----------------

    Traverse_List_Loop: DO


      ! -----------------------------------
      ! Get the pointer to the current node
      !
      !             ---------- 
      !  First =>  |X| Hdr  |N|
      !             ----------
      !            /|\       |
      !             |        |
      !             |       \|/
      !             ----------
      !            |P| Data |N| <= Current
      !             ----------
      !            /|\       |
      !             |        |
      !             |       \|/
      !             ----------
      !            |P| Data |N|
      !             ----------
      !            /|\       |
      !             |        |
      !             |       \|/
      !             ----------
      !            |P| Data |X|
      !             ---------- 
      !
      ! X == NULL pointer
      ! N == NEXT pointer
      ! P == PREVIOUS pointer
      ! -----------------------------------

      Current => SensorInfo_List%First%Next


      ! --------------------------------------
      ! If the pointer is not associated, then
      ! there are no more nodes in the list.
      ! --------------------------------------

      IF ( .NOT. ASSOCIATED( Current ) ) THEN
        EXIT Traverse_List_Loop
      END IF


      ! --------------------------
      ! Increment the node counter
      ! --------------------------

      n_Nodes = n_Nodes + 1


      ! ----------------------------------------------
      ! Make previous node's NEXT pointer (N) point to
      ! the node AFTER the current one, i.e. break the
      ! forward link.
      !
      !             ----------
      !  First =>  |X| Hdr  |N|
      !             ----------
      !            /|\       |
      !             |         --------------
      !             |                       |
      !             ----------              |
      !            |P| Data |N| <= Current  |
      !             ----------              |
      !            /|\       |              |
      !             |        |              |
      !             |       \|/             |
      !             ----------              |
      !            |P| Data |N| <-----------
      !             ----------
      !            /|\       |
      !             |        |
      !             |       \|/
      !             ----------
      !            |P| Data |X|
      !             ----------
      !
      ! X == NULL pointer
      ! N == NEXT pointer
      ! P == PREVIOUS pointer
      !
      ! ----------------------------------------------

      Current%Previous%Next => Current%Next


      ! -----------------------------------------------
      ! If we are not at the end of the list, make the
      ! next node's PREVIOUS pointer (P) point to the
      ! node BEFORE the current one, i.e. break the
      ! backward link.
      !
      !                  ----------
      !  First =>   --> |X| Hdr  |N|
      !            |     ----------
      !            |    /|\       |
      !            |     |         --------------
      !            |     |                       |
      !            |     ----------              |
      !            |    |P| Data |N| <= Current  |
      !            |     ----------              |
      !            |              |              |
      !             -----         |              |
      !                  |       \|/             |
      !                  ----------              |
      !                 |P| Data |N| <-----------
      !                  ----------
      !                 /|\       |
      !                  |        |
      !                  |       \|/
      !                  ----------
      !                 |P| Data |X|
      !                  ----------
      !
      ! X == NULL pointer
      ! N == NEXT pointer
      ! P == PREVIOUS pointer
      !
      ! -----------------------------------------------

      IF ( ASSOCIATED( Current%Next ) ) THEN
        Current%Next%Previous => Current%Previous
      END IF


      ! -----------------------------------------
      ! Nullify the pointers for the current node
      !
      !                  ----------
      !  First =>   --> |X| Hdr  |N|
      !            |     ----------
      !            |              |
      !            |               --------------
      !            |                             |
      !            |     ----------              |
      !            |    |X| Data |X| <= Current  |
      !            |     ----------              |
      !            |                             |
      !             -----                        |
      !                  |                       |
      !                  ----------              |
      !                 |P| Data |N| <-----------
      !                  ----------
      !                 /|\       |
      !                  |        |
      !                  |       \|/
      !                  ----------
      !                 |P| Data |X|
      !                  ----------
      !
      ! X == NULL pointer
      ! N == NEXT pointer
      ! P == PREVIOUS pointer
      ! -----------------------------------------

      NULLIFY( Current%Previous, &
               Current%Next )


      ! --------------------------------------------
      ! Destroy the current node's SensorInfo object
      !
      !                  ----------
      !  First =>   --> |X| Hdr  |N|
      !            |     ----------
      !            |              |
      !            |               --------------
      !            |                             |
      !            |     ----------              |
      !            |    |X|      |X| <= Current  |
      !            |     ----------              |
      !            |                             |
      !             -----                        |
      !                  |                       |
      !                  ----------              |
      !                 |P| Data |N| <-----------
      !                  ----------
      !                 /|\       |
      !                  |        |
      !                  |       \|/
      !                  ----------
      !                 |P| Data |X|
      !                  ----------
      !
      ! X == NULL pointer
      ! N == NEXT pointer
      ! P == PREVIOUS pointer
      ! --------------------------------------------

      Error_Status = Destroy_SensorInfo( Current%SensorInfo, &
                                         Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error destroying SensorInfo object at node # ", i5 )' ) &
                        n_Nodes
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF


      ! ---------------------------
      ! Deallocate the current node
      !
      !             ----------
      !  First =>  |X| Hdr  |N|
      !             ----------
      !            /|\       |
      !             |        |
      !             |        |
      !             |        |
      !             |        |   X <= Current
      !             |        |
      !             |        |
      !             |        |
      !             |       \|/
      !             ----------
      !            |P| Data |N|
      !             ----------
      !            /|\       |
      !             |        |
      !             |       \|/
      !             ----------
      !            |P| Data |X|
      !             ----------
      !
      ! X == NULL pointer
      ! N == NEXT pointer
      ! P == PREVIOUS pointer
      !
      ! ---------------------------

      DEALLOCATE( Current, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Current node # ", i5, ". STAT = ", i5 )' ) &
                        n_Nodes, Allocate_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO Traverse_List_Loop


    ! -----------------------------------------
    ! Deallocate the pointer to the list header
    !
    !  First => X
    !
    ! X == NULL pointer
    !
    ! -----------------------------------------

    DEALLOCATE( SensorInfo_List%First, STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error deallocating list header. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! --------------------------
    ! Set the node count to zero
    ! --------------------------

    SensorInfo_List%n_Nodes = 0



    !#--------------------------------------------------------------------------#
    !#                    -- OUTPUT AND INFORMATION MESSAGE --                  #
    !#--------------------------------------------------------------------------#

    IF ( Noisy ) THEN
      WRITE( Message, '( "Number of nodes deallocated: ", i5 )' ) n_Nodes
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_SensorInfo_List





!------------------------------------------------------------------------------
!S+
! NAME:
!       AddTo_SensorInfo_List
!
! PURPOSE:
!       Function to ADD a SensorInfo node TO a SensorInfo linked list.
!
! CATEGORY:
!       Instrument Information : SensorInfo
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = AddTo_SensorInfo_List( SensorInfo,               &  ! Input
!                                             SensorInfo_List,          &  ! In/Output
!                                             Node_Number = Node_Number,&  ! Optional input
!                                             RCS_Id      = RCS_Id,     &  ! Revision control
!                                             Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       SensorInfo:      SensorInfo structure to be added to the linked list.
!                        UNITS:      N/A
!                        TYPE:       SensorInfo_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Node_Number:     Set this argument to the position in the linked list
!                        that the new node will have. If not specified, the
!                        default action is to add the node at the END of the
!                        list.
!                        UNITS:      None
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      None
!                        TYPE:       Character
!                        DIMENSION:  Scalar, LEN = *
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       SensorInfo_List: SensorInfo linked list to which the new SensorInfo
!                        node was added.
!                        UNITS:      N/A
!                        TYPE:       SensorInfo_List_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:          Character string containing the Revision Control
!                        System Id field for the module.
!                        UNITS:      None
!                        TYPE:       CHARACTER
!                        DIMENSION:  Scalar, LEN = *
!                        ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the list addition was successful,
!                           == FAILURE an unrecoverable error occurred.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!       Assign_SensorInfo:      Function to copy a SensorInfo structure.
!                               SOURCE: SENSORINFO_DEFINE module
!
!       Display_Message:        Subroutine to output messages
!                               SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! COMMENTS:
!       Note the INTENT on the output SensorInfo_List argument is IN OUT rather
!       than just OUT. This is necessary because the argument is defined on
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! EXAMPLE:
!       TYPE( SensorInfo_type )      :: SensorInfo
!       TYPE( SensorInfo_List_type ) :: SensorInfo_List
!       INTEGER                      :: Error_Status
!
!       ! -- Start a new list
!       SensorInfo_List = New_SensorInfo_List()
!
!       ....Load data into SensorInfo structure....
!
!       ! -- Add node to the END of the list (default)
!       Error_Status = AddTo_SensorInfo_List( SensorInfo, &
!                                             SensorInfo_List )
!       IF ( Error_Status /= SUCCESS ) THEN
!         PRINT *, '**ERROR ADDING NODE TO END OF SensorInfo LIST**'
!         STOP
!       END IF
!         
!       ! -- Insert node within the list at position 6
!       Node = 6
!       Error_Status = AddTo_SensorInfo_List( SensorInfo, &
!                                             SensorInfo_List, &
!                                             Node_Number = Node )
!       IF ( Error_Status /= SUCCESS ) THEN
!         PRINT *, '**ERROR INSERTING NODE WITHIN SensorInfo LIST**'
!         STOP
!       END IF
!         
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Apr-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION AddTo_SensorInfo_List( SensorInfo,      &  ! Input
                                  SensorInfo_List, &  ! In/Output
                                  Node_Number,     &  ! Optional input
                                  RCS_Id,          &  ! Revision control
                                  Message_Log )    &  ! Error messaging
                                RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( SensorInfo_type ),      INTENT( IN )     :: SensorInfo

    ! -- In/Output
    TYPE( SensorInfo_List_type ), INTENT( IN OUT ) :: SensorInfo_List

    ! -- Optional input
    INTEGER,            OPTIONAL, INTENT( IN )     :: Node_Number

    ! -- Revision control
    CHARACTER( * ),     OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! -- Error handler message log
    CHARACTER( * ),     OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'AddTo_SensorInfo_List'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message 

    LOGICAL :: Insert_Node

    INTEGER :: Allocate_Status

    INTEGER :: n_Nodes

    TYPE( SensorInfo_Node_type ), POINTER :: Previous
    TYPE( SensorInfo_Node_type ), POINTER :: Current



    !#--------------------------------------------------------------------------#
    !#                     -- NULLIFY LOCAL POINTERS --                         #
    !#--------------------------------------------------------------------------#

    NULLIFY( Previous, Current )



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
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
    !#                           -- CHECK INPUT --                              #
    !#--------------------------------------------------------------------------#

    ! ---------------------
    ! Check the list header
    ! ---------------------

    IF ( .NOT. ASSOCIATED( SensorInfo_List%First ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input SensorInfo_List has not been initialised.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -- Default is to add the node at the end of the list...
    Insert_Node = .FALSE.
    ! -- ...unless a valid node number is specified.
    IF ( PRESENT( Node_Number ) ) THEN
      IF ( Node_Number > 0 ) THEN
        Insert_Node = .TRUE.
      ELSE
        CALL Display_Message( ROUTINE_NAME, &
                              'Invalid node number specified. Adding new node to end of list.', &
                              WARNING, &
                              Message_Log = Message_Log )
      END IF
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- TRAVERSE THE LIST --                         #
    !#--------------------------------------------------------------------------#

    ! -----------------------
    ! Initialise node counter
    ! -----------------------

    n_Nodes = 0


    ! -----------------------------------------
    ! Initialise the node pointers to the start
    ! of the list.
    !
    !               ----------
    !  Previous => |X| Hdr  |N|
    !               ----------
    !              /|\       |
    !               |        |
    !               |       \|/
    !               ----------
    !              |P| Data |N| <= Current
    !               ----------
    !              /|\       |
    !               |       \|/
    !              ...      ...
    !
    ! X == NULL pointer
    ! N == NEXT pointer
    ! P == PREVIOUS pointer
    ! -----------------------------------------
    
    Previous => SensorInfo_List%First
    Current  => Previous%Next


    ! ----------------------------
    ! Traverse the list to the end
    ! ----------------------------

    Traverse_List_Loop: DO


      ! -------------------------------------------
      ! If the current node pointer is unassociated
      ! we're at the end of the list...or we're at
      ! the beginning and the list is empty.
      !
      !               ....    ....
      !               /|\      | 
      !                |      \|/
      !               ----------
      !  Previous => |P| Hdr  |X|
      !               ----------
      !
      !                        X <= Current
      !
      ! X == NULL pointer
      ! N == NEXT pointer
      ! P == PREVIOUS pointer
      !
      ! -------------------------------------------

      IF ( .NOT. ASSOCIATED( Current ) ) THEN
        EXIT Traverse_List_Loop
      END IF


      ! ----------------------------------------------
      ! If a valid node number was passed, then exit
      ! the traversal loop if we're at the node before
      ! which the insertion is to be performed
      !
      !               ....    ....
      !               /|\      | 
      !                |      \|/
      !               ----------
      !  Previous => |P| Hdr  |N|
      !               ----------
      !              /|\       |         New node
      !               |        |   <---- will slot
      !               |       \|/        in here
      !               ----------
      !              |P| Data |N| <= Current
      !               ----------
      !              /|\       |
      !               |       \|/
      !              ...      ...
      !
      ! X == NULL pointer
      ! N == NEXT pointer
      ! P == PREVIOUS pointer
      !
      ! ----------------------------------------------

      IF ( Insert_Node ) THEN
        IF ( n_Nodes == ( Node_Number - 1 ) ) THEN
          EXIT Traverse_List_Loop
        END IF
      END IF


      ! ------------------------------------
      ! We're not at the end of the list, so
      ! move past the current node
      ! ------------------------------------

      Previous => Current
      Current  => Current%Next


      ! ----------------------
      ! Increment node counter
      ! ----------------------

      n_Nodes = n_Nodes + 1

    END DO Traverse_List_Loop


    ! ------------------------------
    ! Allocate and fill the new node
    ! ------------------------------

    ! -- First simply allocate the pointer. Note that
    ! -- now, Previous%Next points to the NEW NODE, 
    ! -- *not* the CURRENT NODE.
    ALLOCATE( Previous%Next, STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating new SensorInfo_List node member. ", &
                        &"STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Copy over the SensorInfo structure to the new node
    Error_Status = Assign_SensorInfo( SensorInfo, &
                                      Previous%Next%SensorInfo, &
                                      Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error copying SensorInfo structure into new list node.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ------------------------------------------
    ! Insert the new node pointers into the list
    ! ------------------------------------------

    ! -- Are we at the end of the list?
    IF ( .NOT. ASSOCIATED( Current ) ) THEN

      !!! YES. The new node is added to the end of the list !!!

      ! -- Mark the end of the list
      NULLIFY( Previous%Next%Next )

      ! -- Make the new node PREVIOUS node pointer 
      ! -- point to the previous node.
      Previous%Next%Previous => Previous

    ELSE

      !!! NO. The new node is slotted between the Previous and Current nodes !!!

      ! -- Make the new node NEXT pointer
      ! -- point to the Current node
      Previous%Next%Next => Current

      ! -- Make the new node PREVIOUS pointer
      ! -- point to the Previous node
      Previous%Next%Previous => Previous

      ! -- Make the Current node PREVIOUS pointer
      ! -- point to the new node
      Current%Previous => Previous%Next

    END IF


    ! -------------------------------------
    ! Increment the list total node counter
    ! -------------------------------------

    SensorInfo_List%n_Nodes = SensorInfo_List%n_Nodes + 1

  END FUNCTION AddTo_SensorInfo_List





!------------------------------------------------------------------------------
!S+
! NAME:
!       GetFrom_SensorInfo_List
!
! PURPOSE:
!       Function to GET a SensorInfo node FROM a SensorInfo linked list.
!
! CATEGORY:
!       Instrument Information : SensorInfo
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = GetFrom_SensorInfo_List( SensorInfo_List,          &  ! Input
!                                               Node_Number,              &  ! Input
!                                               SensorInfo,               &  ! Output
!                                               RCS_Id      = RCS_Id,     &  ! Revision control
!                                               Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       SensorInfo_List: SensorInfo linked list from which the SensorInfo
!                        node is to be retrieved.
!                        UNITS:      N/A
!                        TYPE:       SensorInfo_List_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       Node_Number:     The SensorInfo_List node number to retrieve.
!                        UNITS:      None
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
!                        TYPE:       Character
!                        DIMENSION:  Scalar, LEN = *
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       SensorInfo:      SensorInfo structure retrieved from the linked list.
!                        UNITS:      N/A
!                        TYPE:       SensorInfo_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:          Character string containing the Revision Control
!                        System Id field for the module.
!                        UNITS:      None
!                        TYPE:       CHARACTER
!                        DIMENSION:  Scalar, LEN = *
!                        ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the SensorInfo node retrieval was successful,
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Count_SensorInfo_Nodes:  Function to count the number of nodes
!                                in the SensorInfo linked list.
!
!       Get_Node_Pointer:        Subroutine to traverse the SensorInfo
!                                linked list to the required node and
!                                return a pointer to that node.
!
!       Assign_SensorInfo:       Function to copy a SensorInfo structure.
!                                SOURCE: SENSORINFO_DEFINE module
!
!       Display_Message:         Subroutine to output messages
!                                SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! COMMENTS:
!       Note the INTENT on the output SensorInfo argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined (at least
!       its components may be) upon input. To prevent memory leaks, the IN OUT
!       INTENT is a must.
!
! EXAMPLE:
!       TYPE( SensorInfo_type )      :: SensorInfo
!       TYPE( SensorInfo_List_type ) :: SensorInfo_List
!       INTEGER                      :: Error_Status
!
!       ! -- Create a new list
!       SensorInfo_List = New_SensorInfo_List( SensorInfo_List )
!
!       ....Load data into the SensorInfo linked list....
!
!       ! -- Retrieve required SensorInfo node from the linked list
!       Node = 6
!       Error_Status = GetFrom_SensorInfo_List( SensorInfo_List, & ! Input
!                                               Node,            & ! Input
!                                               SensorInfo       ) ! Output
!       IF ( Error_Status /= SUCCESS ) THEN
!         PRINT *, '**ERROR RETRIEVING NODE FROM SensorInfo LIST**'
!         STOP
!       END IF
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Apr-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION GetFrom_SensorInfo_List( SensorInfo_List, &  ! Input
                                    Node_Number,     &  ! Input
                                    SensorInfo,      &  ! Output
                                    RCS_Id,          &  ! Revision control
                                    Message_Log )    &  ! Error messaging
                                  RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( SensorInfo_List_type ), INTENT( IN )     :: SensorInfo_List
    INTEGER,                      INTENT( IN )     :: Node_Number

    ! -- Output
    TYPE( SensorInfo_type ),      INTENT( IN OUT ) :: SensorInfo

    ! -- Revision control
    CHARACTER( * ),     OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! -- Error handler message log
    CHARACTER( * ),     OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'GetFrom_SensorInfo_List'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message 

    TYPE( SensorInfo_Node_type ), POINTER :: Node_Pointer



    !#--------------------------------------------------------------------------#
    !#                     -- NULLIFY LOCAL POINTERS --                         #
    !#--------------------------------------------------------------------------#

    NULLIFY( Node_Pointer )



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
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
    !#                           -- CHECK INPUT --                              #
    !#--------------------------------------------------------------------------#

    IF ( Node_Number < 1 .OR. &
         Node_Number > SensorInfo_List%n_Nodes ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid node number specified.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#              -- TRAVERSE THE LIST TO THE REQUIRED NODE --                #
    !#--------------------------------------------------------------------------#

    CALL Get_Node_Pointer( SensorInfo_List, &
                           Node_Number, &
                           Node_Pointer )

    IF ( .NOT. ASSOCIATED( Node_Pointer ) ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Requested node #, ", i5, " does not exist in list." )' ) &
                      Node_Number
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#            -- COPY OUT THE SensorInfo DATA FROM THE NODE --              #
    !#--------------------------------------------------------------------------#

    Error_Status = Assign_SensorInfo( Node_Pointer%SensorInfo, &
                                      SensorInfo, &
                                      Message_Log = Message_Log )


    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error copying SensorInfo data from requested node #, ", i5, "." )' ) &
                      Node_Number
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      ! Don't want RETURN here so that the
      ! Node_Pointer can still be nullified
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- NULLIFY THE LOCAL POINTER --                       #
    !#--------------------------------------------------------------------------#

    NULLIFY( Node_Pointer )

  END FUNCTION GetFrom_SensorInfo_List





!------------------------------------------------------------------------------
!S+
! NAME:
!       Count_SensorInfo_Nodes
!
! PURPOSE:
!       Function to count the number of nodes in a SensorInfo linked list.
!
! CATEGORY:
!       Instrument Information : SensorInfo
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       n_Nodes = Count_SensorInfo_Nodes( SensorInfo_List )  ! Input
!
! INPUT ARGUMENTS:
!       SensorInfo_List: SensorInfo linked list in which the nodes are to
!                        be counted.
!                        UNITS:      N/A
!                        TYPE:       SensorInfo_List_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
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
!       n_Nodes:         The number of nodes in the SensorInfo linked list.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!       List_Is_Empty:  Logical function to determine if the list
!                       is empty.
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! EXAMPLE:
!       TYPE( SensorInfo_List_type ) :: SensorInfo_List
!
!       ! -- Create a new list
!       SensorInfo_List = New_SensorInfo_List( SensorInfo_List )
!
!       ....Load data into the SensorInfo linked list....
!
!       ! -- Count the number of nodes in the list
!       n_Nodes = Count_SensorInfo_Nodes( SensorInfo_List )
!
!       PRINT *, 'Number of SensorInfo_List nodes: ', n_Nodes
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Apr-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Count_SensorInfo_Nodes( SensorInfo_List ) RESULT ( n_Nodes )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( SensorInfo_List_type ), INTENT( IN ) :: SensorInfo_List


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: n_Nodes


    ! ---------------
    ! Local variables
    ! ---------------

    TYPE( SensorInfo_Node_type ), POINTER :: Current



    !#--------------------------------------------------------------------------#
    !#                      -- NULLIFY LOCAL POINTERS --                        #
    !#--------------------------------------------------------------------------#

    NULLIFY( Current )



    !#--------------------------------------------------------------------------#
    !#                      -- INITIALISE NODE COUNTER --                       #
    !#--------------------------------------------------------------------------#

    n_Nodes = 0



    !#--------------------------------------------------------------------------#
    !#                       -- CHECK THE INPUT LIST --                         #
    !#--------------------------------------------------------------------------#

    IF ( List_Is_Empty( SensorInfo_List ) ) RETURN



    !#--------------------------------------------------------------------------#
    !#                         -- COUNT THE NODES --                            #
    !#--------------------------------------------------------------------------#

    ! --------------------------------
    ! Initialise pointer to first node
    ! --------------------------------

    Current => SensorInfo_List%First%Next


    ! -------------
    ! Traverse list
    ! -------------

    DO

      ! ---------------------
      ! Check for end of list
      ! ---------------------

      IF ( .NOT. ASSOCIATED( Current ) ) THEN
        RETURN
      END IF


      ! ----------------------
      ! Increment node counter
      ! ----------------------

      n_Nodes = n_Nodes + 1


      ! ---------------
      ! Go to next node
      ! ---------------

      Current => Current%Next

    END DO

  END FUNCTION Count_SensorInfo_Nodes

END MODULE SensorInfo_LinkedList


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: SensorInfo_LinkedList.f90,v 1.11 2006/09/06 15:09:09 frpv Exp $
!
! $Date: 2006/09/06 15:09:09 $
!
! $Revision: 1.11 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: SensorInfo_LinkedList.f90,v $
! Revision 1.11  2006/09/06 15:09:09  frpv
! Removed unneeded PRIVATE attribute for SensorInfo_Node_type definition.
!
! Revision 1.10  2006/05/02 16:58:02  dgroff
! *** empty log message ***
!
! Revision 1.9  2004/09/03 16:06:25  paulv
! - Added header documentation for New() function.
!
! Revision 1.8  2004/08/18 15:44:21  paulv
! - Upgraded to Fortran95.
! - SensorInfo node and list structure definitions now contain initialisations
!   for their components.
! - Replaced the SensorInfo_List Init() routine with with a New() function
!   that returns an initialised SensorInfo linked list as a result.
! - Altered the INTENT of the SensorInfo_List dummy argument in the
!   Destroy_SensorInfo_List() function from OUT to IN OUT. Necessary to
!   prevent memory leaks.
! - Added optional Quiet argument to Destroy_SensorInfo_List() to suppress
!   information messages.
! - Altered the INTENT of the SensorInfo dummy argument in the
!   GetFrom_SensorInfo_List() function from OUT to IN OUT. Necessary to
!   prevent memory leaks.
!
! Revision 1.7  2004/07/19 19:32:11  paulv
! - Updated the document headers.
!
! Revision 1.6  2003/07/24 18:59:12  paulv
! - Updated some header documentation.
!
! Revision 1.5  2003/06/04 15:19:13  paulv
! - Removed all No_ChannelInfo optional arguments from calls to routines from
!   SensorInfo_Define() module. Channel information is now read in by default.
! - Added a node counter to the linked list header. This is now used internally
!   to check the number of nodes rather than a call to the Count_SensorInfo_Nodes()
!   function.
!
! Revision 1.4  2003/05/23 15:54:45  paulv
! - Removed multple specification of the PUBLIC attribute for the
!   SensorInfo_List data type.
!
! Revision 1.3  2003/05/23 15:44:17  paulv
! - Removed commented out code.
!
! Revision 1.2  2003/04/21 12:26:22  paulv
! - Updated header documentation.
!
! Revision 1.1  2003/04/16 21:10:07  paulv
! Initial checkin.
!
!
!
