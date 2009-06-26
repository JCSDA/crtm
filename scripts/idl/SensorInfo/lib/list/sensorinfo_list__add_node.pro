;+
; NAME:
;       SensorInfo_List::Add_Node
;
; PURPOSE:
;       The SensorInfo_List::Add_Node function method adds a SensorInfo
;       node to a SensorInfo linked list
;
; CALLING SEQUENCE:
;       Result = Obj->[SensorInfo_List::]Add_Node( SensorInfo, $               ; Input
;                                                  Node_Number=Node_Number, $  ; Input keyword
;                                                  Debug=Debug              )  ; Input keyword
;
; INPUT ARGUMENTS:
;       SensorInfo:  SensorInfo object to add to the list.
;                    UNITS:      N/A
;                    TYPE:       SensorInfo
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN)
;
; INPUT KEYWORD PARAMETERS:
;       Node_Number: The node number at which the SensorInfo object
;                    is to be inserted. If not specified, the object
;                    is added to the end of the list.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Debug:       Set this keyword for debugging.
;                    If NOT SET => Error handler is enabled. (DEFAULT)
;                       SET     => Error handler is disabled; Routine
;                                  traceback output is enabled.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
;
; FUNCTION RESULT:
;       Result:      The return value is an integer defining the error
;                    status. The error codes are defined in the error_codes
;                    include file.
;                    If == SUCCESS the node addition was successful
;                       == FAILURE an unrecoverable error occurred
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;
; INCLUDE FILES:
;       error_codes:           Include file containing error code definitions.
;
; EXAMPLE:
;       Given a valid SensorInfo List, list, a valid SensorInfo object, x,
;       can be added to the end of list like so,
;
;         IDL> Result = list->Add_Node(x)
;
;       The object can be added at any point in the list by specifying the
;       node number,
;
;         IDL> Result = list->Add_Node(x, Node_Number=3)
;
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 02-Oct-2008
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION SensorInfo_List::Add_Node, SensorInfo, $               ; Input
                                    Node_Number=Node_Number, $  ; Input keyword
                                    Debug=Debug                 ; Input keyword

  ; Set up
  ; ------
  ; error handler
  @error_codes
  IF ( KEYWORD_SET(Debug) ) THEN BEGIN
    MESSAGE, '--> Entered.', /INFORMATIONAL
    MsgSwitch = 0
  ENDIF ELSE BEGIN
    CATCH, Error_Status
    IF ( Error_Status NE 0 ) THEN BEGIN
      CATCH, /CANCEL
      MESSAGE, !ERROR_STATE.MSG, /CONTINUE
      RETURN, FAILURE
    ENDIF
    MsgSwitch = 1
  ENDELSE

  ; Check the list header
  IF ( NOT PTR_VALID(self.First) ) THEN $
    MESSAGE, 'Input SensorInfo_List has not been initialised', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  ; Check input
  IF ( N_ELEMENTS(Node_Number) EQ 0 ) THEN BEGIN
    ; Default is to add the node at the end of the list...
    n_Nodes = self.n_Nodes
  ENDIF ELSE BEGIN
    ; ...unless a valid node number is specfied
    IF ( Node_Number GT self.n_Nodes ) THEN $
      n_Nodes = self.n_Nodes $
    ELSE $
      n_Nodes = Node_Number - 1L
  ENDELSE


  ; Initialise the node pointers to the start of the list
  ; -----------------------------------------------------
  ;
  ;               ----------
  ;  Previous => |X| Hdr  |N|
  ;               ----------
  ;              /|\       |
  ;               |        |
  ;               |       \|/
  ;               ----------
  ;              |P| Data |N| <= Current
  ;               ----------
  ;              /|\       |
  ;               |       \|/
  ;              ...      ...
  ; X == NULL pointer
  ; N == NEXT pointer
  ; P == PREVIOUS pointer
  Previous = self.First
  Current  = (*Previous).Next
  
  ; Traverse the required number of nodes
  ; -------------------------------------
  FOR n = 1, n_Nodes DO BEGIN
  
    ; If the current node pointer is unassociated
    ; we're at the end of the list...or we're at
    ; the beginning and the list is empty.
    ;               ....    ....
    ;               /|\      | 
    ;                |      \|/
    ;               ----------
    ;  Previous => |P| Hdr  |X|
    ;               ----------
    ;                        X <= Current
    ; X == NULL pointer
    ; N == NEXT pointer
    ; P == PREVIOUS pointer
    IF ( NOT PTR_VALID(Current) ) THEN BREAK
  
    ; We're not at the end of the list, so
    ; move past the current node
    Previous = Current
    Current  = (*Current).Next
  
  ENDFOR ; List traversal loop


  ; Allocate and fill the new node
  ; ------------------------------
  ; First simply allocate the pointer to a new object.
  ; Note that now, (*Previous).Next points to the NEW NODE, 
  ; *not* the CURRENT NODE.
  (*Previous).Next = PTR_NEW({SensorInfo_Node})

  ; Copy over the SensorInfo structure to the new node
  (*(*Previous).Next).SensorInfo = PTR_NEW(OBJ_NEW('SensorInfo',Debug=Debug))
  Result = SensorInfo->Assign(*(*(*Previous).Next).SensorInfo,Debug=Debug)
  IF ( Result NE SUCCESS ) THEN $
    MESSAGE, 'Error copying SensorInfo structure into new list node.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  ; Insert the new node pointers into the list
  ; ------------------------------------------
  ; Are we at the end of the list?
  IF ( PTR_VALID(Current) ) THEN BEGIN
    ; NO. The new node is slotted between the Previous and Current nodes
    ; ------------------------------------------------------------------
    ; Make the new node NEXT pointer
    ; point to the Current node
    (*(*Previous).Next).Next = Current
  
    ; Make the new node PREVIOUS pointer
    ; point to the Previous node
    (*(*Previous).Next).Previous = Previous
  
    ; Make the Current node PREVIOUS pointer
    ; point to the new node
    (*Current).Previous = (*Previous).Next
  
  ENDIF ELSE BEGIN
    ; YES. The new node is added to the end of the list
    ; -------------------------------------------------
    ; Mark the end of the list
    PTR_FREE, (*(*Previous).Next).Next
    (*(*Previous).Next).Next = PTR_NEW()
    
    ; Make the new node PREVIOUS node pointer
    ; point to the previous node.
    (*(*Previous).Next).Previous = Previous
  
  ENDELSE

  ; Increment the list total node counter
  ; -------------------------------------
  ++self.n_Nodes


  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS
      
END ; FUNCTION SensorInfo_List::Add_Node
