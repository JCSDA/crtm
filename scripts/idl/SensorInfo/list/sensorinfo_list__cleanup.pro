;+
; Procedure to destroy a SensorInfo Linked List

PRO SensorInfo_List::Cleanup, Quiet=Quiet, $  ; Input keyword
                              Debug=Debug     ; Input keyword
;-
 
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
      RETURN
    ENDIF
    MsgSwitch = 1
  ENDELSE

  ; Check if list is empty
  IF ( self->Is_Empty() ) THEN RETURN
  
  ; Initialise the node counter
  n_Nodes = 0L


  ; Traverse the linked list
  ; ------------------------
  FOR n = 1, self.n_Nodes DO BEGIN
  
    ; Get the pointer to the current node
    ;
    ;             ---------- 
    ;  First =>  |X| Hdr  |N|
    ;             ----------
    ;            /|\       |
    ;             |        |
    ;             |       \|/
    ;             ----------
    ;            |P| Data |N| <= Current
    ;             ----------
    ;            /|\       |
    ;             |        |
    ;             |       \|/
    ;             ----------
    ;            |P| Data |N|
    ;             ----------
    ;            /|\       |
    ;             |        |
    ;             |       \|/
    ;             ----------
    ;            |P| Data |X|
    ;             ---------- 
    ;
    ; X == NULL pointer
    ; N == NEXT pointer
    ; P == PREVIOUS pointer
    Current = (*self.First).Next

    ; If the pointer is not associated, then
    ; there are no more nodes in the list.
    IF ( NOT PTR_VALID(Current) ) THEN BREAK

    ; Make previous node's NEXT pointer (N) point to
    ; the node AFTER the current one, i.e. break the
    ; forward link.
    ;
    ;             ----------
    ;  First =>  |X| Hdr  |N|
    ;             ----------
    ;            /|\       |
    ;             |         --------------
    ;             |                       |
    ;             ----------              |
    ;            |P| Data |N| <= Current  |
    ;             ----------              |
    ;            /|\       |              |
    ;             |        |              |
    ;             |       \|/             |
    ;             ----------              |
    ;            |P| Data |N| <-----------
    ;             ----------
    ;            /|\       |
    ;             |        |
    ;             |       \|/
    ;             ----------
    ;            |P| Data |X|
    ;             ----------
    ;
    ; X == NULL pointer
    ; N == NEXT pointer
    ; P == PREVIOUS pointer
    (*(*Current).Previous).Next = (*Current).Next

    ; If we are not at the end of the list, make the
    ; next node's PREVIOUS pointer (P) point to the
    ; node BEFORE the current one, i.e. break the
    ; backward link.
    ;
    ;                  ----------
    ;  First =>   --> |X| Hdr  |N|
    ;            |     ----------
    ;            |    /|\       |
    ;            |     |         --------------
    ;            |     |                       |
    ;            |     ----------              |
    ;            |    |P| Data |N| <= Current  |
    ;            |     ----------              |
    ;            |              |              |
    ;             -----         |              |
    ;                  |       \|/             |
    ;                  ----------              |
    ;                 |P| Data |N| <-----------
    ;                  ----------
    ;                 /|\       |
    ;                  |        |
    ;                  |       \|/
    ;                  ----------
    ;                 |P| Data |X|
    ;                  ----------
    ;
    ; X == NULL pointer
    ; N == NEXT pointer
    ; P == PREVIOUS pointer
    IF ( PTR_VALID((*Current).Next) ) THEN (*(*Current).Next).Previous = (*Current).Previous

    ; Nullify the pointers for the current node
    ;
    ;                  ----------
    ;  First =>   --> |X| Hdr  |N|
    ;            |     ----------
    ;            |              |
    ;            |               --------------
    ;            |                             |
    ;            |     ----------              |
    ;            |    |X| Data |X| <= Current  |
    ;            |     ----------              |
    ;            |                             |
    ;             -----                        |
    ;                  |                       |
    ;                  ----------              |
    ;                 |P| Data |N| <-----------
    ;                  ----------
    ;                 /|\       |
    ;                  |        |
    ;                  |       \|/
    ;                  ----------
    ;                 |P| Data |X|
    ;                  ----------
    ;
    ; X == NULL pointer
    ; N == NEXT pointer
    ; P == PREVIOUS pointer
    (*Current).Previous = PTR_NEW()
    (*Current).Next     = PTR_NEW()

    ; Destroy the current node's SensorInfo object
    ;
    ;                  ----------
    ;  First =>   --> |X| Hdr  |N|
    ;            |     ----------
    ;            |              |
    ;            |               --------------
    ;            |                             |
    ;            |     ----------              |
    ;            |    |X|      |X| <= Current  |
    ;            |     ----------              |
    ;            |                             |
    ;             -----                        |
    ;                  |                       |
    ;                  ----------              |
    ;                 |P| Data |N| <-----------
    ;                  ----------
    ;                 /|\       |
    ;                  |        |
    ;                  |       \|/
    ;                  ----------
    ;                 |P| Data |X|
    ;                  ----------
    ;
    ; X == NULL pointer
    ; N == NEXT pointer
    ; P == PREVIOUS pointer
    OBJ_DESTROY, *(*Current).SensorInfo
    PTR_FREE, (*Current).SensorInfo
    (*Current).SensorInfo = PTR_NEW()

    ; Deallocate the current node
    ;
    ;             ----------
    ;  First =>  |X| Hdr  |N|
    ;             ----------
    ;            /|\       |
    ;             |        |
    ;             |        |
    ;             |        |
    ;             |        |   X <= Current
    ;             |        |
    ;             |        |
    ;             |        |
    ;             |       \|/
    ;             ----------
    ;            |P| Data |N|
    ;             ----------
    ;            /|\       |
    ;             |        |
    ;             |       \|/
    ;             ----------
    ;            |P| Data |X|
    ;             ----------
    ;
    ; X == NULL pointer
    ; N == NEXT pointer
    ; P == PREVIOUS pointer
    PTR_FREE, Current
    Current = PTR_NEW()

    ; Update the counters
    ++n_Nodes
    --self.n_Nodes

  ENDFOR ; List traversal loop


  ; Deallocate the pointer to the list header
  ;
  ;  First => X
  ;
  ; X == NULL pointer
  ; -----------------------------------------
  PTR_FREE, self.First
  self.First = PTR_NEW()


  ; Check the list node count
  ; -------------------------
  IF ( self.n_Nodes NE 0 ) THEN $
    MESSAGE, 'Number of remaining nodes is not zero!', NONAME=MsgSwitch, NOPRINT=MsgSwitch

  
  ; Output an info message
  ; ----------------------
  IF ( NOT KEYWORD_SET(Quiet) ) THEN $
    MESSAGE, 'Number of nodes deallocated: '+STRTRIM(n_Nodes,2), /INFORMATIONAL

END ; PRO SensorInfo_List::Cleanup
