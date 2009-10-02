;+
; LBL_UserId record definition procedure
;
; In LBLRTM  == Record 1.1
;    MonoRTM == Record 1.1

PRO LBL_UserId__Define

;-

  void = { LBL_UserId, $
           Identification: '', $  ; User identification string
           INHERITS LBL_Base   }  ; Shared base object

END ; PRO LBL_UserId__Define
