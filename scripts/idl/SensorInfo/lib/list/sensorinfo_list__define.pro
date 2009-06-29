;+
; SensorInfo_List definition procedure
;
; SUPERCLASSES:
;       IDL_Container
;
; CREATION:
;       See SensorInfo_List::Init
;
; PROPERTIES:
;       See SensorInfo_List::Get_Property and SensorInfo_List::Set_Property
;       for this object's properties and which ones are Get/Set-able.
;
; METHODS:
;       This class has the following methods:
;
;       * SensorInfo_List::Add
;       * SensorInfo_List::Cleanup
;       * SensorInfo_List::Define
;       * SensorInfo_List::Get
;       * SensorInfo_List::Get_Property
;       * SensorInfo_List::Init
;       * SensorInfo_List::Inspect
;       * SensorInfo_List::Read
;       * SensorInfo_List::Set_Property
;       * SensorInfo_List::Write
;
;-

PRO SensorInfo_List__Define

  void = { SensorInfo_List, $
           ; File information
           Filename: '', $
           ; Dimension data
           n_Sensors : 0L, $  ; The number of SensorInfo Nodes
           ; Container for SensorInfo objects
           INHERITS IDL_Container }

END ; PRO SensorInfo_List__Define
