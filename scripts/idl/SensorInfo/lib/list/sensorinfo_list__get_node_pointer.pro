;+
; NAME:
;       SensorInfo_List::Get_Node_Pointer
;
; PURPOSE:
;       The SensorInfo_List::Get_Node_Pointer procedure method traverses
;       a SensorInfo linked list to a specified node and returns a
;       pointer to it.
;
; CALLING SEQUENCE:
;       Obj->[SensorInfo_List::]Get_Node_Pointer, Node_Pointer           , $  ; Output
;                                                 Node_Number=Node_Number, $  ; Input keyword
;                                                 Sensor_Id  =Sensor_Id  , $  ; Input keyword
;                                                 Debug=Debug                 ; Input keyword
;
; OUTPUT ARGUMENTS:
;       Node_Pointer:  Pointer to the requested node in the SensorInfo list.
;                      UNITS:      N/A
;                      TYPE:       POINTER
;                      DIMENSION:  Scalar
;                      ATTRIBUTES: INTENT(OUT)
;
; INPUT KEYWORD PARAMETERS:
;       Node_Number:   The number of the node for which a pointer is
;                      required.
;                      NOTE: Either this keyword or the Sensor_Id keyword
;                            must be specified. If BOTH are specified the
;                            Sensor_Id keyword takes precedence.
;                      UNITS:      N/A
;                      TYPE:       INTEGER
;                      DIMENSION:  Scalar
;                      ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Sensor_Id:     The sensor id of the node for which a pointer is
;                      required.
;                      NOTE: Either this keyword or the Node_Number keyword
;                            must be specified. If BOTH are specified the
;                            Sensor_Id keyword takes precedence.
;                      UNITS:      N/A
;                      TYPE:       CHARACTER
;                      DIMENSION:  Scalar
;                      ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Debug:         Set this keyword for debugging.
;                      If NOT SET => Error handler is enabled. (DEFAULT)
;                         SET     => Error handler is disabled; Routine
;                                    traceback output is enabled.
;                      UNITS:      N/A
;                      TYPE:       INTEGER
;                      DIMENSION:  Scalar
;                      ATTRIBUTES: INTENT(IN), OPTIONAL
;
; INCLUDE FILES:
;       error_codes:   Include file containing error code definitions.
;
; EXAMPLE:
;       Given a valid SensorInfo List, list, a pointer to a particular
;       SensorInfo node can be retrieved like so,
;
;         IDL> list->Get_Node_Pointer, ptr, Node_Number=3
;
;       Alternatively, the pointer to a SensorInfo node for a particular
;       sensor can be obtained via,
;
;         IDL> list->Get_Node_Pointer, ptr, Sensor_Id='amsua_metop-a'
;
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 02-Oct-2008
;                       paul.vandelst@noaa.gov
;
;-

PRO SensorInfo_List::Get_Node_Pointer, Node_Pointer           , $  ; Output
                                       Node_Number=Node_Number, $  ; Input keyword (integer)
                                       Sensor_Id=Sensor_Id    , $  ; Input keyword (string)
                                       Debug=Debug                 ; Input keyword

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
  
  ; Determine what's been defined
  Use_Node_Number = FALSE
  Use_Sensor_Id   = FALSE
  IF ( N_ELEMENTS(Node_Number) GT 0 ) THEN Use_Node_Number = TRUE
  IF ( Valid_String(Sensor_Id) ) THEN BEGIN
    Use_Sensor_Id   = TRUE
    Use_Node_Number = FALSE
  ENDIF

  ; Check that at least one of the keywords has been defined
  IF ( NOT (Use_Node_Number OR Use_Sensor_Id) ) THEN $
    MESSAGE, 'Must specify at least one of Node_Number or Sensor_Id keywords.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
             
  ; Check list
  IF ( self->Is_Empty() ) THEN RETURN

  ; Check node number input
  IF ( Use_Node_Number ) THEN BEGIN
    IF ( Node_Number LT 1 OR Node_Number GT self.n_Nodes ) THEN RETURN
  ENDIF
  
  ; Initialise pointer to first node
  Current = (*self.First).Next

  ; Traverse the list
  WHILE 1 DO BEGIN
  
    ; At end of list before required node
    IF ( NOT PTR_VALID(Current) ) THEN RETURN
    
    ; Increment node counter
    ++n_Nodes

    ; Is the current node the one required?
    IF ( Use_Node_Number ) THEN BEGIN
      ; Compare node number
      IF ( n_Nodes EQ Node_Number ) THEN BREAK
    ENDIF ELSE BEGIN
      ; Compare sensor id
      Result = *(*Current).SensorInfo->SensorInfo::Get(Sensor_Id=si)
      IF ( Result NE SUCCESS ) THEN $
        MESSAGE, 'Error obtaining Sensor_Id from node number '+STRTRIM(n_Nodes,2), $
                 NONAME=MsgSwitch, NOPRINT=MsgSwitch
      IF ( STRTRIM(Sensor_Id,2) EQ STRTRIM(si,2) ) THEN BREAK
    ENDELSE
    
    ; Go to next node
    Current = (*Current).Next
    
  ENDWHILE
  
  ; Point return argument to requested node
  Node_Pointer = Current
  
END ; PRO SensorInfo_List::Get_Node_Pointer
