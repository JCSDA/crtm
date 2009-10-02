;+
; NAME:
;       LBL_Solar::Get_Property
;
; PURPOSE:
;       The LBL_Solar::Get_Property procedure method gets the value
;       of a property or group of properties for this object.
;
; CALLING SEQUENCE:
;       Obj->[LBL_Solar::]Get_Property, $
;         Debug   = Debug  , $  ; Input keyword
;         inflag  = inflag , $  ; Output keyword
;         iotflag = iotflag, $  ; Output keyword
;         juldat  = juldat      ; Output keyword
;
; KEYWORDS:
;       Consult the LBLRTM and MonoRTM documentation for valid LBL_Solar
;       keyword values. Other keywords provided are:
;
;       Along with any of the group of keywords accepted by the
;       LBL_Base::Get_Property procedure method, the following
;       keywords are also accepted:
;
;       Debug:                 Set this keyword for debugging.
;                              If NOT SET => Error handler is enabled. (DEFAULT)
;                                 SET     => Error handler is disabled; Routine
;                                            traceback output is enabled.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 16-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO LBL_Solar::Get_Property, $
  Debug      = Debug  , $  ; Input keyword
  inflag     = inflag , $  ; Output keyword
  iotflag    = iotflag, $  ; Output keyword
  juldat     = juldat , $  ; Output keyword
  _REF_EXTRA = Extra       ; Keywords passed onto LBL_Base::Get_Property
  
  ; Set up
  @lbl_solar_parameters
  @lbl_pro_err_handler
 
 
  ; Get subclass data
  IF ( ARG_PRESENT(inflag ) ) THEN inflag  = self.inflag 
  IF ( ARG_PRESENT(iotflag) ) THEN iotflag = self.iotflag
  IF ( ARG_PRESENT(juldat ) ) THEN juldat  = self.juldat 
  
  
  ; Get superclass data
  self->LBL_Base::Get_Property, _EXTRA = Extra, Debug = Debug
  
  
  ; Done
  CATCH, /CANCEL
 
END ; PRO LBL_Solar::Get_Property
