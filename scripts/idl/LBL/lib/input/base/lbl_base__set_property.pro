;+
; NAME:
;       LBL_Base::Set_Property
;
; PURPOSE:
;       The LBL_Base::Set_Property procedure method sets the value
;       of a property or group of properties for this object.
;
; CALLING SEQUENCE:
;       Obj->[LBL_Base::]Set_Property, $
;         Debug    = Debug   , $  ; Input keyword
;         lbl_type = lbl_type, $  ; Input keyword
;         lbl_name = lbl_name, $  ; Input keyword
;         lbl_fmt  = lbl_fmt      ; Input keyword
;
; KEYWORDS:
;       Consult the LBLRTM and MonoRTM documentation for valid LBL_Base
;       keyword values. Other keywords provided are:
;
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
;                  ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       lbl_name:  String containing the LBL program type.
;                  UNITS:      N/A
;                  TYPE:       CHARACTER
;                  DIMENSION:  Scalar
;                  ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       lbl_fmt:   String containing output print format.
;                  UNITS:      N/A
;                  TYPE:       CHARACTER
;                  DIMENSION:  Scalar
;                  ATTRIBUTES: INTENT(IN), OPTIONAL
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 16-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO LBL_Base::Set_Property, $
  Debug    = Debug   , $  ; Input keyword
  lbl_type = lbl_type, $  ; Input keyword
  lbl_fmt  = lbl_fmt , $  ; Input keyword
  lbl_name = lbl_name     ; Input keyword
  
  ; Set up
  COMPILE_OPT HIDDEN
  @lbl_pro_err_handler
 
 
  ; Set data
  IF ( N_ELEMENTS(lbl_type) GT 0 ) THEN self.lbl_type = lbl_type
  IF ( Valid_String(lbl_name)    ) THEN self.lbl_name = lbl_name
  IF ( Valid_String(lbl_fmt)     ) THEN self.lbl_fmt  = lbl_fmt
  
  
  ; Done
  CATCH, /CANCEL
 
END ; PRO LBL_Base::Set_Property
