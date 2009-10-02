;+
; LBL_Input object definition procedure

PRO LBL_Input__Define

;-

  void = { LBL_Input, $
           ; File information
           filename: '', $
           ; Shared base object
           INHERITS LBL_Base, $
           ; Container for other objects
           INHERITS IDL_Container }

END ; PRO LBL_Input__Define
