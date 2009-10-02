;+
; NAME:
;       LBL_Continuum::Get_Property
;
; PURPOSE:
;       The LBL_Continuum::Get_Property procedure method gets the value
;       of a property or group of properties for this object.
;
; CALLING SEQUENCE:
;       Obj->[LBL_Continuum::]Get_Property, $
;         Debug       = Debug      , $  ; Input keyword
;         h2o_self    = h2o_self   , $  ; Output keyword
;         h2o_foreign = h2o_foreign, $  ; Output keyword
;         co2         = co2        , $  ; Output keyword
;         o3          = o3         , $  ; Output keyword
;         o2          = o2         , $  ; Output keyword
;         n2          = n2         , $  ; Output keyword
;         rayleigh    = rayleigh        ; Output keyword
;
; KEYWORDS:
;       Consult the LBLRTM and MonoRTM documentation for valid LBL_Continuum
;       keyword values.
;
;       Along with any of the group of keywords accepted by the
;       LBL_Base::Get_Property procedure method, the following
;       keywords are also accepted:
;
;       Debug:                 Get this keyword for debugging.
;                              If NOT Get => Error handler is enabled. (DEFAULT)
;                                 Get     => Error handler is disabled; Routine
;                                            traceback output is enabled.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
; INCLUDE FILES:
;       lbl_parameters: Include file containing lbl specific
;                       parameter value definitions.
;
;       lbl_pro_err_handler: Error handler code for lbl procedures.
;
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 16-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO LBL_Continuum::Get_Property, $
  Debug       = Debug      , $  ; Input keyword
  h2o_self    = h2o_self   , $  ; Output keyword
  h2o_foreign = h2o_foreign, $  ; Output keyword
  co2         = co2        , $  ; Output keyword
  o3          = o3         , $  ; Output keyword
  o2          = o2         , $  ; Output keyword
  n2          = n2         , $  ; Output keyword
  rayleigh    = rayleigh   , $  ; Output keyword
  _REF_EXTRA  = Extra           ; Keywords passed onto LBL_Base::Get_Property
  
  ; Set up
  @lbl_continuum_parameters
  @lbl_pro_err_handler
 
 
  ; Get subclass data
  IF ( ARG_PRESENT(h2o_self   ) GT 0 ) THEN h2o_self    = self.h2o_self   
  IF ( ARG_PRESENT(h2o_foreign) GT 0 ) THEN h2o_foreign = self.h2o_foreign
  IF ( ARG_PRESENT(co2        ) GT 0 ) THEN co2         = self.co2        
  IF ( ARG_PRESENT(o3         ) GT 0 ) THEN o3          = self.o3         
  IF ( ARG_PRESENT(o2         ) GT 0 ) THEN o2          = self.o2         
  IF ( ARG_PRESENT(n2         ) GT 0 ) THEN n2          = self.n2         
  IF ( ARG_PRESENT(rayleigh   ) GT 0 ) THEN rayleigh    = self.rayleigh   
  
  
  ; Get superclass data
  self->LBL_Base::Get_Property, _EXTRA = Extra, Debug = Debug
  
  
  ; Done
  CATCH, /CANCEL
 
END ; PRO LBL_Continuum::Get_Property
