;+
; LBL_Base definition procedure
;

PRO LBL_Base__Define

;-
  ; Set up
  COMPILE_OPT HIDDEN

  void = { LBL_Base, $
           lbl_type : 0L, $  ; Flag indicating LBL type
           lbl_name : '', $  ; Name of LBL type
           lbl_fmt  : ''  }  ; Output format

END ; PRO LBL_Base__Define
