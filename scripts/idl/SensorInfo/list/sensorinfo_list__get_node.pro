;+
; NAME:
;       SensorInfo_List::Get_Node
;
; PURPOSE:
;       The SensorInfo_List::Get_Node function method gets a SensorInfo
;       node from a SensorInfo linked list
;
; CALLING SEQUENCE:
;       Result = Obj->[SensorInfo_List::]Get_Node( SensorInfo, $               ; Output
;                                                  Node_Number=Node_Number, $  ; Input keyword
;                                                  Sensor_Id  =Sensor_Id  , $  ; Input keyword
;                                                  Debug=Debug              )  ; Input keyword
;
; OUTPUT ARGUMENTS:
;       SensorInfo:  SensorInfo object retrieved from the list
;                    UNITS:      N/A
;                    TYPE:       SensorInfo
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(OUT)
;
; INPUT KEYWORD PARAMETERS:
;       Node_Number: The number of the node to be retrieved from the
;                    list.
;                    NOTE: Either this keyword or the Sensor_Id keyword
;                          must be specified. If BOTH are specified the
;                          Sensor_Id keyword takes precedence.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Sensor_Id:   The sensor id of the node to be retrieved from the
;                    list.
;                    NOTE: Either this keyword or the Node_Number keyword
;                          must be specified. If BOTH are specified the
;                          Sensor_Id keyword takes precedence.
;                    UNITS:      N/A
;                    TYPE:       CHARACTER
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
;                    If == SUCCESS the node retrieval was successful
;                       == FAILURE an unrecoverable error occurred
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;
; INCLUDE FILES:
;       error_codes:    Include file containing error code definitions.
;
; EXAMPLE:
;       Given a valid SensorInfo List, list, a particular SensorInfo node
;       can be retrieved like so,
;
;         IDL> Result = list->Get_Node(x, Node_Number=3)
;
;       Alternatively, the SensorInfo node for a particular sensor can
;       be obtained via,
;
;         IDL> Result = list->Get_Node(x, Sensor_Id='amsua_metop-a')
;
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 02-Oct-2008
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION SensorInfo_List::Get_Node, SensorInfo             , $  ; Output
                                    Node_Number=Node_Number, $  ; Input keyword (integer)
                                    Sensor_Id=Sensor_Id    , $  ; Input keyword (string)
                                    Debug=Debug                 ; Input keyword                                    

  ; Set up
  ; ------
  ; Error handler
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
  

  ; Traverse list to the required node
  ; ----------------------------------
  self->Get_Node_Pointer,Node_Pointer,Node_Number=Node_Number,Sensor_Id=Sensor_Id,Debug=Debug
  IF ( NOT PTR_VALID(Node_Pointer) ) THEN $
    MESSAGE, 'Requested node does not exist in list', $
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

    
  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS

END ; FUNCTION SensorInfo_List::Get_Node
