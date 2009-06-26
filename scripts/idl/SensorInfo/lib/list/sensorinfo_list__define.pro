;+
; SensorInfo Linked List definition procedure
;
PRO SensorInfo_List__Define
;
;-
  void = { SensorInfo_List, $
           n_Nodes : 0L       , $  ; The number of SensorInfo Nodes
           First   : PTR_NEW()  }  ; Pointer to the first node

END ; PRO SensorInfo_List__Define
