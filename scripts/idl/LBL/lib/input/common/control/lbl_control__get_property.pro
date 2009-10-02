;+
; NAME:
;       LBL_Control::Get_Property
;
; PURPOSE:
;       The LBL_Control::Get_Property procedure method gets the value
;       of a property or group of properties for this object.
;
; CALLING SEQUENCE:
;       Obj->[LBL_Control::]Get_Property, $
;         Debug     = Debug    , $  ; Input keyword
;         hirac     = hirac    , $  ; Output keyword
;         lblf4     = lblf4    , $  ; Output keyword
;         continuum = continuum, $  ; Output keyword
;         aerosol   = aerosol  , $  ; Output keyword
;         emit      = emit     , $  ; Output keyword
;         scan      = scan     , $  ; Output keyword
;         filter    = filter   , $  ; Output keyword
;         plotlbl   = plotlbl  , $  ; Output keyword
;         test      = test     , $  ; Output keyword
;         atm       = atm      , $  ; Output keyword
;         merge     = merge    , $  ; Output keyword
;         laser     = laser    , $  ; Output keyword
;         od        = od       , $  ; Output keyword
;         xsection  = xsection , $  ; Output keyword
;         mpts      = mpts     , $  ; Output keyword
;         npts      = npts     , $  ; Output keyword
;         speed     = speed         ; Output keyword
;
; KEYWORDS:
;       Consult the LBLRTM and MonoRTM documentation for valid LBL_Control
;       keyword values.
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

PRO LBL_Control::Get_Property, $
  Debug      = Debug    , $  ; Input keyword
  hirac      = hirac    , $  ; Output keyword
  lblf4      = lblf4    , $  ; Output keyword
  continuum  = continuum, $  ; Output keyword
  aerosol    = aerosol  , $  ; Output keyword
  emit       = emit     , $  ; Output keyword
  scan       = scan     , $  ; Output keyword
  filter     = filter   , $  ; Output keyword
  plotlbl    = plotlbl  , $  ; Output keyword
  test       = test     , $  ; Output keyword
  atm        = atm      , $  ; Output keyword
  merge      = merge    , $  ; Output keyword
  laser      = laser    , $  ; Output keyword
  od         = od       , $  ; Output keyword
  xsection   = xsection , $  ; Output keyword
  mpts       = mpts     , $  ; Output keyword
  npts       = npts     , $  ; Output keyword
  speed      = speed    , $  ; Output keyword
  _REF_EXTRA = Extra         ; Keywords passed onto LBL_Base::Get_Property
  
  ; Set up
  @lbl_control_parameters
  @lbl_pro_err_handler
 
 
  ; Get subclass data
  IF ( ARG_PRESENT(hirac    ) ) THEN hirac     = self.hirac
  IF ( ARG_PRESENT(lblf4    ) ) THEN lblf4     = self.lblf4
  IF ( ARG_PRESENT(continuum) ) THEN continuum = self.continuum
  IF ( ARG_PRESENT(aerosol  ) ) THEN aerosol   = self.aerosol
  IF ( ARG_PRESENT(emit     ) ) THEN emit      = self.emit
  IF ( ARG_PRESENT(scan     ) ) THEN scan      = self.scan
  IF ( ARG_PRESENT(filter   ) ) THEN filter    = self.filter
  IF ( ARG_PRESENT(plotlbl  ) ) THEN plotlbl   = self.plotlbl
  IF ( ARG_PRESENT(test     ) ) THEN test      = self.test
  IF ( ARG_PRESENT(atm      ) ) THEN atm       = self.atm
  IF ( ARG_PRESENT(merge    ) ) THEN merge     = self.merge
  IF ( ARG_PRESENT(laser    ) ) THEN laser     = self.laser
  IF ( ARG_PRESENT(od       ) ) THEN od        = self.od
  IF ( ARG_PRESENT(xsection ) ) THEN xsection  = self.xsection
  IF ( ARG_PRESENT(mpts     ) ) THEN mpts      = self.mpts
  IF ( ARG_PRESENT(npts     ) ) THEN npts      = self.npts
  IF ( ARG_PRESENT(speed    ) ) THEN speed     = self.speed
  
  
  ; Get superclass data
  self->LBL_Base::Get_Property, _EXTRA = Extra, Debug = Debug
  
  
  ; Done
  CATCH, /CANCEL
 
END ; PRO LBL_Control::Get_Property
