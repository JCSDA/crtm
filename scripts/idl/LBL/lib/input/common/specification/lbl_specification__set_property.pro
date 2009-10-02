;+
; NAME:
;       LBL_Specification::Set_Property
;
; PURPOSE:
;       The LBL_Specification::Set_Property procedure method sets the value
;       of a property or group of properties for this object.
;
; CALLING SEQUENCE:
;       Obj->[LBL_Specification::]Set_Property, $
;         Debug      = Debug     , $  ; Input keyword
;         lbl_type   = lbl_type  , $  ; Input keyword
;         v1         = v1        , $  ; Input keyword
;         v2         = v2        , $  ; Input keyword
;         sample     = sample    , $  ; Input keyword
;         dvset      = dvset     , $  ; Input keyword
;         alfal0     = alfal0    , $  ; Input keyword
;         avmass     = avmass    , $  ; Input keyword
;         dptmin     = dptmin    , $  ; Input keyword
;         dptfac     = dptfac    , $  ; Input keyword
;         ilnflg     = ilnflg    , $  ; Input keyword
;         dvout      = dvout     , $  ; Input keyword
;         nmol_scale = nmol_scale, $  ; Input keyword
;         hmol_scale = hmol_scale, $  ; Input keyword
;         xmol_scale = xmol_scale     ; Input keyword
;
; KEYWORDS:
;       Consult the LBLRTM and MonoRTM documentation for valid LBL_Specification
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
;       Written by:     Paul van Delst, 21-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO LBL_Specification::Set_Property, $
  Debug      = Debug     , $  ; Input keyword
  lbl_type   = lbl_type  , $  ; Input keyword (Used for LBL_Base)
  v1         = v1        , $  ; Input keyword
  v2         = v2        , $  ; Input keyword
  sample     = sample    , $  ; Input keyword
  dvset      = dvset     , $  ; Input keyword
  alfal0     = alfal0    , $  ; Input keyword
  avmass     = avmass    , $  ; Input keyword
  dptmin     = dptmin    , $  ; Input keyword
  dptfac     = dptfac    , $  ; Input keyword
  ilnflg     = ilnflg    , $  ; Input keyword
  dvout      = dvout     , $  ; Input keyword
  nmol_scale = nmol_scale, $  ; Input keyword
  hmol_scale = hmol_scale, $  ; Input keyword
  xmol_scale = xmol_scale, $  ; Input keyword
  _EXTRA     = ignore
  
  ; Set up
  @lbl_specification_parameters
  @lbl_pro_err_handler
 
 
  ; Set subclass data
  ; ...Frequency data and checks
  IF ( N_ELEMENTS(v1) GT 0 ) THEN self.v1 = DOUBLE(v1[0])
  IF ( N_ELEMENTS(v2) GT 0 ) THEN self.v2 = DOUBLE(v2[0])
  ;  IF ( LBLRTM and NEGATIVE ) THEN Error
  ;  IF ( POSITIVE ) THEN:
    IF ( self.v2 LT self.v1 ) THEN $
      MESSAGE, 'V2 must be >= V1', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    IF ( self.v2-self.v1 GE LBL_SPECIFICATION_BANDWIDTH_MAX) THEN $
      MESSAGE, 'V2-V1 must be < '+STRING(LBL_SPECIFICATION_BANDWIDTH_MAX, FORMAT='(f6.1)'), $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ; ...Number of sample points and checks
  IF ( N_ELEMENTS(sample) GT 0 ) THEN self.sample = ABS(DOUBLE(sample[0]))
    IF ( self.sample LT LBL_SPECIFICATION_SAMPLE_MIN ) THEN self.sample = LBL_SPECIFICATION_SAMPLE_MIN
    IF ( self.sample GT LBL_SPECIFICATION_SAMPLE_MAX ) THEN self.sample = LBL_SPECIFICATION_SAMPLE_MAX
  ; ...Fields where negative values trigger defaults
  IF ( N_ELEMENTS(dptmin) GT 0 ) THEN self.dptmin = DOUBLE(dptmin[0])
    IF ( self.dptmin LT ZERO ) THEN self.dptmin = LBL_SPECIFICATION_DPTMIN_DEFAULT
  IF ( N_ELEMENTS(dptfac) GT 0 ) THEN self.dptfac = DOUBLE(dptfac[0])
    IF ( self.dptfac LT ZERO ) THEN self.dptfac = LBL_SPECIFICATION_DPTFAC_DEFAULT
  ; ...Fields with no checks
  IF ( N_ELEMENTS(dvset     ) GT 0 ) THEN self.dvset      = DOUBLE(dvset[0])
  IF ( N_ELEMENTS(alfal0    ) GT 0 ) THEN self.alfal0     = DOUBLE(alfal0[0])
  IF ( N_ELEMENTS(avmass    ) GT 0 ) THEN self.avmass     = DOUBLE(avmass[0])
  IF ( N_ELEMENTS(ilnflg    ) GT 0 ) THEN self.ilnflg     = LONG(ilnflg[0])
  IF ( N_ELEMENTS(dvout     ) GT 0 ) THEN self.dvout      = DOUBLE(dvout [0])
  IF ( N_ELEMENTS(nmol_scale) GT 0 ) THEN self.nmol_scale = LONG(nmol_scale[0])
  IF ( N_ELEMENTS(hmol_scale) GT 0 ) THEN self.hmol_scale = hmol_scale
  IF ( N_ELEMENTS(xmol_scale) GT 0 ) THEN self.xmol_scale = xmol_scale

stop
  ; Set superclass data
  IF ( N_ELEMENTS(lbl_type) GT 0 ) THEN type = LONG(lbl_type[0]) ELSE type = 0L
  name = LBL_TYPE_NAME[type]
  fmt  = LBL_SPECIFICATION_FMT[*,type]
  self->LBL_Base::Set_Property, $
    Debug = Debug, $
    lbl_type = type, $
    lbl_name = name, $
    lbl_fmt  = fmt
  
  
  ; Done
  CATCH, /CANCEL
 
END ; PRO LBL_Specification::Set_Property
