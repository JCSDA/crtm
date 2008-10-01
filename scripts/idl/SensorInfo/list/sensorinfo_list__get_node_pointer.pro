;+
; Procedure to traverse a SensorInfo linked list to a specified
; node and return a pointer to it.

PRO SensorInfo_List::Get_Node_Pointer, Node_Number, $   ; Input
                                       Node_Pointer, $  ; Output
                                       Debug=Debug      ; Input keyword
;- 

  ; Set up error handler
  @error_codes
  IF ( KEYWORD_SET(Debug) ) THEN BEGIN
    MESSAGE, '--> Entered.', /INFORMATIONAL
  ENDIF ELSE BEGIN
    CATCH, Error_Status
    IF ( Error_Status NE 0 ) THEN BEGIN
      CATCH, /CANCEL
      MESSAGE, !ERROR_STATE.MSG, /CONTINUE
      RETURN
    ENDIF
  ENDELSE
  
  ; Initialise return pointer and node counter
  Node_Pointer = PTR_NEW()
  n_Nodes = 0L
  
  ; Check input
  IF ( Node_Number LT 1 ) THEN RETURN
  IF ( Node_Number GT self.n_Nodes ) THEN RETURN
  IF ( self->Is_Empty() ) THEN RETURN

  ; Initialise pointer to first node
  Current = (*self.First).Next

  ; Traverse the list
  WHILE 1 DO BEGIN
  
    ; At end of list before required node
    IF ( NOT PTR_VALID(Current) ) THEN RETURN
    
    ; Increment node counter
    ++n_Nodes
    
    ; Is the current node the one required?
    IF ( n_Nodes EQ Node_Number ) THEN BREAK
    
    ; Go to next node
    Current = (*Current).Next
    
  ENDWHILE
  
  ; Point return argument to requested node
  Node_Pointer = Current
  
END ; PRO SensorInfo_List::Get_Node_Pointer
