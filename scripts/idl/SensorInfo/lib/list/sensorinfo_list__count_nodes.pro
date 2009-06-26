FUNCTION SensorInfo_List::Count_Nodes, Debug=Debug ; Input keyword                                    

  ; Set up
  ; ...Error handler
  @error_codes
  IF ( KEYWORD_SET(Debug) ) THEN BEGIN
    MESSAGE, '--> Entered.', /INFORMATIONAL
    MsgSwitch = 0
  ENDIF ELSE BEGIN
    CATCH, Error_Status
    IF ( Error_Status NE 0 ) THEN BEGIN
      CATCH, /CANCEL
      MESSAGE, !ERROR_STATE.MSG, /CONTINUE
      RETURN, -1
    ENDIF
    MsgSwitch = 1
  ENDELSE

  ; ...Initialise counter and check list
  n_Nodes = 0L
  IF ( self->Is_Empty() ) THEN RETURN, n_Nodes
  
  ; ...Initialise pointer to first node
  Current = (*self.First).Next


  ; Traverse the list
  WHILE 1 DO BEGIN
    IF ( NOT PTR_VALID(Current) ) THEN RETURN, n_Nodes
    ++n_Nodes
    Current = (*Current).Next
  ENDWHILE
  
END ; FUNCTION SensorInfo_List::Count_Nodes
