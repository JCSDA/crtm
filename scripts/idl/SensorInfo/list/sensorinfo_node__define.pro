;+
; SensorInfo Link List Node structure definition procedure
;
PRO SensorInfo_Node__Define
;
;-
  void = { SensorInfo_Node, $
           SensorInfo : PTR_NEW(), $  ; Node data
           Previous   : PTR_NEW(), $  ; Pointer to previous node
           Next       : PTR_NEW()  }  ; Pointer to next node

END ; PRO SensorInfo_Node__Define
