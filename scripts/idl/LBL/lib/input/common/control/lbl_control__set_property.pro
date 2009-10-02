;+
; NAME:
;       LBL_Control::Set_Property
;
; PURPOSE:
;       The LBL_Control::Set_Property procedure method sets the value
;       of a property or group of properties for this object.
;
; CALLING SEQUENCE:
;       Obj->[LBL_Control::]Set_Property, $
;         Debug     = Debug    , $  ; Input keyword
;         hirac     = hirac    , $  ; Input keyword
;         lblf4     = lblf4    , $  ; Input keyword
;         continuum = continuum, $  ; Input keyword
;         aerosol   = aerosol  , $  ; Input keyword
;         emit      = emit     , $  ; Input keyword
;         scan      = scan     , $  ; Input keyword
;         filter    = filter   , $  ; Input keyword
;         plotlbl   = plotlbl  , $  ; Input keyword
;         test      = test     , $  ; Input keyword
;         atm       = atm      , $  ; Input keyword
;         merge     = merge    , $  ; Input keyword
;         laser     = laser    , $  ; Input keyword
;         od        = od       , $  ; Input keyword
;         xsection  = xsection , $  ; Input keyword
;         mpts      = mpts     , $  ; Input keyword
;         npts      = npts     , $  ; Input keyword
;         speed     = speed    , $  ; Input keyword
;         lbl_type  = lbl_type      ; Input keyword
;
; KEYWORDS:
;       Consult the LBLRTM and MonoRTM documentation for valid LBL_Control
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

PRO LBL_Control::Set_Property, $
  Debug     = Debug    , $  ; Input keyword
  hirac     = hirac    , $  ; Input keyword
  lblf4     = lblf4    , $  ; Input keyword
  continuum = continuum, $  ; Input keyword
  aerosol   = aerosol  , $  ; Input keyword
  emit      = emit     , $  ; Input keyword
  scan      = scan     , $  ; Input keyword
  filter    = filter   , $  ; Input keyword
  plotlbl   = plotlbl  , $  ; Input keyword
  test      = test     , $  ; Input keyword
  atm       = atm      , $  ; Input keyword
  merge     = merge    , $  ; Input keyword
  laser     = laser    , $  ; Input keyword
  od        = od       , $  ; Input keyword
  xsection  = xsection , $  ; Input keyword
  mpts      = mpts     , $  ; Input keyword
  npts      = npts     , $  ; Input keyword
  speed     = speed    , $  ; Input keyword
  lbl_type  = lbl_type , $  ; Input keyword (used for LBL_Base)
  _EXTRA    = ignore
  
  ; Set up
  @lbl_control_parameters
  @lbl_pro_err_handler
 
 
  ; Set subclass data
  IF ( N_ELEMENTS(hirac    ) GT 0 ) THEN self.hirac     = LONG(hirac[0])
  IF ( N_ELEMENTS(lblf4    ) GT 0 ) THEN self.lblf4     = LONG(lblf4[0])
  IF ( N_ELEMENTS(continuum) GT 0 ) THEN self.continuum = LONG(continuum[0])
  IF ( N_ELEMENTS(aerosol  ) GT 0 ) THEN self.aerosol   = LONG(aerosol[0])
  IF ( N_ELEMENTS(emit     ) GT 0 ) THEN self.emit      = LONG(emit[0])
  IF ( N_ELEMENTS(scan     ) GT 0 ) THEN self.scan      = LONG(scan[0])
  IF ( N_ELEMENTS(filter   ) GT 0 ) THEN self.filter    = LONG(filter[0])
  IF ( N_ELEMENTS(plotlbl  ) GT 0 ) THEN self.plotlbl   = LONG(plotlbl[0])
  IF ( N_ELEMENTS(test     ) GT 0 ) THEN self.test      = LONG(test[0])
  IF ( N_ELEMENTS(atm      ) GT 0 ) THEN self.atm       = LONG(atm[0])
  IF ( N_ELEMENTS(merge    ) GT 0 ) THEN self.merge     = LONG(merge[0])
  IF ( N_ELEMENTS(laser    ) GT 0 ) THEN self.laser     = LONG(laser[0])
  IF ( N_ELEMENTS(od       ) GT 0 ) THEN self.od        = LONG(od[0])
  IF ( N_ELEMENTS(xsection ) GT 0 ) THEN self.xsection  = LONG(xsection[0])
  IF ( N_ELEMENTS(mpts     ) GT 0 ) THEN self.mpts      = LONG(mpts[0])
  IF ( N_ELEMENTS(npts     ) GT 0 ) THEN self.npts      = LONG(npts[0])
  IF ( N_ELEMENTS(speed    ) GT 0 ) THEN self.speed     = LONG(speed[0])


  ; Set superclass data
  IF ( N_ELEMENTS(lbl_type) GT 0 ) THEN type = LONG(lbl_type[0]) ELSE type = 0L
  name = LBL_TYPE_NAME[type]
  fmt  = LBL_CONTROL_FMT[type]
  self->LBL_Base::Set_Property, $
    Debug = Debug, $
    lbl_type = type, $
    lbl_name = name, $
    lbl_fmt  = fmt
  
  
  ; Done
  CATCH, /CANCEL
 
END ; PRO LBL_Control::Set_Property
