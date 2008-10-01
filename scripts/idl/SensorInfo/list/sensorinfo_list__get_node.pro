;+
; Procedure to GET a SensorInfo node from a SensorInfo linked list

FUNCTION SensorInfo_List::Get_Node, Node_Number, $  ; Input
                                    SensorInfo, $   ; Output
                                    Debug=Debug     ; Input keyword                                    
;- 

  ; Set up error handler
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
  
  ; Check the node number
  IF ( Node_Number LT 1 OR Node_Number GT self.n_Nodes ) THEN $
    MESSAGE, 'Invalid node number specified', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  ; Traverse list to the required node
  ; ----------------------------------
  self->Get_Node_Pointer,Node_Number,Node_Pointer,Debug=Debug
  IF ( NOT PTR_VALID(Node_Pointer) ) THEN $
    MESSAGE, 'Requested node number '+STRTRIM(Node_Number,2)+' does not exist in list', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  
  ; Copy out the SensorInfo data from the node
  ; ------------------------------------------
  ; Copy over the SensorInfo structure to the new node
  Result = *(*Node_Pointer).SensorInfo->Assign(SensorInfo,Debug=Debug)
  IF ( Result NE SUCCESS ) THEN $
    MESSAGE, 'Error copying SensorInfo node into output object.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  ; Free the local pointer
  ; ----------------------
  Node_Pointer = PTR_NEW()
    
END ; FUNCTION SensorInfo_List::Get_Node
