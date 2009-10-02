;+
; NAME:
;       LBL_Continuum::Set_Property
;
; PURPOSE:
;       The LBL_Continuum::Set_Property procedure method sets the value
;       of a property or group of properties for this object.
;
; CALLING SEQUENCE:
;       Obj->[LBL_Continuum::]Set_Property, $
;         Debug       = Debug      , $  ; Input keyword
;         h2o_self    = h2o_self   , $  ; Input keyword
;         h2o_foreign = h2o_foreign, $  ; Input keyword
;         co2         = co2        , $  ; Input keyword
;         o3          = o3         , $  ; Input keyword
;         o2          = o2         , $  ; Input keyword
;         n2          = n2         , $  ; Input keyword
;         rayleigh    = rayleigh   , $  ; Input keyword
;         lbl_type    = lbl_type        ; Input keyword
;
; KEYWORDS:
;       Consult the LBLRTM and MonoRTM documentation for valid LBL_Continuum
;       keyword values.
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
;       lbl_type:              Integer flag used to define the type of LBL input
;                              file. Valid values are defined in the lbl_parameters
;                              include file.
;                              If not specified, the default value is that for ASCII.
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

PRO LBL_Continuum::Set_Property, $
  Debug       = Debug      , $  ; Input keyword
  h2o_self    = h2o_self   , $  ; Input keyword
  h2o_foreign = h2o_foreign, $  ; Input keyword
  co2         = co2        , $  ; Input keyword
  o3          = o3         , $  ; Input keyword
  o2          = o2         , $  ; Input keyword
  n2          = n2         , $  ; Input keyword
  rayleigh    = rayleigh   , $  ; Input keyword
  lbl_type    = lbl_type   , $  ; Input keyword (used for LBL_Base)
  _EXTRA      = ignore
  
  ; Set up
  @lbl_continuum_parameters
  @lbl_pro_err_handler
 
 
  ; Set subclass data
  IF ( N_ELEMENTS(h2o_self   ) GT 0 ) THEN self.h2o_self    = DOUBLE(h2o_self[0])
  IF ( N_ELEMENTS(h2o_foreign) GT 0 ) THEN self.h2o_foreign = DOUBLE(h2o_foreign[0])
  IF ( N_ELEMENTS(co2        ) GT 0 ) THEN self.co2         = DOUBLE(co2[0])
  IF ( N_ELEMENTS(o3         ) GT 0 ) THEN self.o3          = DOUBLE(o3[0])
  IF ( N_ELEMENTS(o2         ) GT 0 ) THEN self.o2          = DOUBLE(o2[0])
  IF ( N_ELEMENTS(n2         ) GT 0 ) THEN self.n2          = DOUBLE(n2[0])
  IF ( N_ELEMENTS(rayleigh   ) GT 0 ) THEN self.rayleigh    = DOUBLE(rayleigh[0])


  ; Set superclass data
  IF ( N_ELEMENTS(lbl_type) GT 0 ) THEN type = LONG(lbl_type[0]) ELSE type = 0L
  name = LBL_TYPE_NAME[type]
  fmt  = LBL_CONTINUUM_FMT[type]
  self->LBL_Base::Set_Property, $
    Debug = Debug, $
    lbl_type = type, $
    lbl_name = name, $
    lbl_fmt  = fmt
  
  
  ; Done
  CATCH, /CANCEL
 
END ; PRO LBL_Continuum::Set_Property
