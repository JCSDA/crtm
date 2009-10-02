;+
; NAME:
;       LBL_Base::Get_Property
;
; PURPOSE:
;       The LBL_Base::Get_Property procedure method gets the value
;       of a property or group of properties for this object.
;
; CALLING SEQUENCE:
;       Obj->[LBL_Base::]Get_Property, $
;         Debug    = Debug   , $  ; Input keyword
;         lbl_type = lbl_type, $  ; Output keyword
;         lbl_name = lbl_name, $  ; Output keyword
;         lbl_fmt  = lbl_fmt      ; Output keyword
;
; KEYWORDS:
;       Debug:     Set this keyword for debugging.
;                  If NOT SET => Error handler is enabled. (DEFAULT)
;                     SET     => Error handler is disabled; Routine
;                                traceback output is enabled.
;                  UNITS:      N/A
;                  TYPE:       INTEGER
;                  DIMENSION:  Scalar
;                  ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       lbl_type:  Integer flag for the LBL program type.
;                  UNITS:      N/A
;                  TYPE:       INTEGER
;                  DIMENSION:  Scalar
;                  ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       lbl_name:  String containing the LBL program type.
;                  UNITS:      N/A
;                  TYPE:       CHARACTER
;                  DIMENSION:  Scalar
;                  ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       lbl_fmt:   String containing output print format.
;                  UNITS:      N/A
;                  TYPE:       CHARACTER
;                  DIMENSION:  Scalar
;                  ATTRIBUTES: INTENT(OUT), OPTIONAL
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 16-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO LBL_Base::Get_Property, $
  Debug    = Debug   , $  ; Input keyword
  lbl_type = lbl_type, $  ; Output keyword
  lbl_name = lbl_name, $  ; Output keyword
  lbl_fmt  = lbl_fmt      ; Output keyword
  
  ; Set up
  COMPILE_OPT HIDDEN
  @lbl_pro_err_handler
 
 
  ; Set data
  IF ( ARG_PRESENT(lbl_type) ) THEN lbl_type = self.lbl_type
  IF ( ARG_PRESENT(lbl_name) ) THEN lbl_name = self.lbl_name
  IF ( ARG_PRESENT(lbl_fmt ) ) THEN lbl_fmt  = self.lbl_fmt
  
  
  ; Done
  CATCH, /CANCEL
 
END ; PRO LBL_Base::Get_Property
