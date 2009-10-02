;+
; LBL_Solar record definition procedure
;
; In LBLRTM  :: Record 1.2.1
;    MonoRTM :: None

PRO LBL_Solar__Define

;-

  void = { LBL_Solar, $
           inflag  : 0L, $
           iotflag : 0L, $
           juldat  : 0L, $
           INHERITS LBL_Base }  ; Shared base object

END ; PRO LBL_Solar__Define
