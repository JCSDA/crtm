;+
; NAME:
;       LBL_Specification::Get_Property
;
; PURPOSE:
;       The LBL_Specification::Get_Property procedure method gets the value
;       of a property or group of properties for this object.
;
; CALLING SEQUENCE:
;       Obj->[LBL_Specification::]Get_Property, $
;         Debug      = Debug     , $  ; Input keyword
;         v1         = v1        , $  ; Output keyword
;         v2         = v2        , $  ; Output keyword
;         sample     = sample    , $  ; Output keyword
;         dvset      = dvset     , $  ; Output keyword
;         alfal0     = alfal0    , $  ; Output keyword
;         avmass     = avmass    , $  ; Output keyword
;         dptmin     = dptmin    , $  ; Output keyword
;         dptfac     = dptfac    , $  ; Output keyword
;         ilnflg     = ilnflg    , $  ; Output keyword
;         dvout      = dvout     , $  ; Output keyword
;         nmol_scale = nmol_scale, $  ; Output keyword
;         hmol_scale = hmol_scale, $  ; Output keyword
;         xmol_scale = xmol_scale     ; Output keyword
;
; KEYWORDS:
;       Consult the LBLRTM and MonoRTM documentation for valid LBL_Specification
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
; CREATION HISTORY:
;       Written by:     Paul van Delst, 21-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO LBL_Specification::Get_Property, $
  Debug      = Debug     , $  ; Input keyword
  v1         = v1        , $  ; Output keyword
  v2         = v2        , $  ; Output keyword
  sample     = sample    , $  ; Output keyword
  dvset      = dvset     , $  ; Output keyword
  alfal0     = alfal0    , $  ; Output keyword
  avmass     = avmass    , $  ; Output keyword
  dptmin     = dptmin    , $  ; Output keyword
  dptfac     = dptfac    , $  ; Output keyword
  ilnflg     = ilnflg    , $  ; Output keyword
  dvout      = dvout     , $  ; Output keyword
  nmol_scale = nmol_scale, $  ; Output keyword
  hmol_scale = hmol_scale, $  ; Output keyword
  xmol_scale = xmol_scale, $  ; Output keyword
  _REF_EXTRA = Extra          ; Keywords passed onto LBL_Base::Get_Property
  
  ; Set up
  @lbl_specification_parameters
  @lbl_pro_err_handler
 
 
  ; Get subclass data
  IF ( ARG_PRESENT(v1        ) ) THEN v1         = self.v1     
  IF ( ARG_PRESENT(v2        ) ) THEN v2         = self.v2     
  IF ( ARG_PRESENT(sample    ) ) THEN sample     = self.sample 
  IF ( ARG_PRESENT(dvset     ) ) THEN dvset      = self.dvset  
  IF ( ARG_PRESENT(alfal0    ) ) THEN alfal0     = self.alfal0 
  IF ( ARG_PRESENT(avmass    ) ) THEN avmass     = self.avmass 
  IF ( ARG_PRESENT(dptmin    ) ) THEN dptmin     = self.dptmin 
  IF ( ARG_PRESENT(dptfac    ) ) THEN dptfac     = self.dptfac 
  IF ( ARG_PRESENT(ilnflg    ) ) THEN ilnflg     = self.ilnflg 
  IF ( ARG_PRESENT(dvout     ) ) THEN dvout      = self.dvout  
  IF ( ARG_PRESENT(nmol_scale) ) THEN nmol_scale = self.nmol_scale
  IF ( ARG_PRESENT(hmol_scale) ) THEN hmol_scale = self.hmol_scale
  IF ( ARG_PRESENT(xmol_scale) ) THEN xmol_scale = self.xmol_scale
  
  
  ; Get superclass data
  self->LBL_Base::Get_Property, _EXTRA = Extra, Debug = Debug
  
  
  ; Done
  CATCH, /CANCEL
 
END ; PRO LBL_Specification::Get_Property
