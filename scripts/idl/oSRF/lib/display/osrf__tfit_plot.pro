;+
; NAME:
;       OSRF::Plot
;
; PURPOSE:
;       The OSRF::Plot procedure method displays a valid OSRF object.
;
; CALLING SEQUENCE:
;       Obj->[OSRF::]Plot, $
;         Debug=Debug  ; Input keyword
;
; INPUT KEYWORD PARAMETERS:
;       Debug:       Set this keyword for debugging.
;                    If NOT SET => Error handler is enabled. (DEFAULT)
;                       SET     => Error handler is disabled; Routine
;                                  traceback output is enabled.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
; INCLUDE FILES:
;       osrf_parameters: Include file containing OSRF specific
;                        parameter value definitions.
;
;       osrf_pro_err_handler: Error handler code for OSRF procedures.
;
; EXAMPLE:
;       Given an instance of a OSRF object,
;
;         IDL> HELP, x
;         X               OBJREF    = <ObjHeapVar8(OSRF)>
;
;       the data is plotted like so:
;
;         IDL> x->Plot
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 20-Apr-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO OSRF::Tfit_Plot, $
  Debug     = debug    , $  ; Input keyword
  Color     = color    , $  ; Input keyword
  Owin      = owin     , $  ; Input keyword
  _EXTRA    = extra

  ; Set up
  COMPILE_OPT HIDDEN
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
  ; ...ALL *input* pointers must be associated
  IF ( NOT self.Associated(Debug=debug) ) THEN $
    MESSAGE, 'Some or all input OSRF pointer members are NOT associated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ; ...Process keywords
  create_window = KEYWORD_SET(owin) ? ~ ISA(owin,'GraphicsWin') : 1


  ; Get the srf info and Tdata
  self.Get_Property, $
    Debug       = debug      , $
    n_Bands     = n_bands    , $
    Channel     = channel    , $
    Sensor_Id   = sensor_id  , $
    Sensor_Type = sensor_type, $
    poly_Tdata  = poly_tdata
  T    = poly_tdata["T"]
  Teff = poly_tdata["Teff"]
  Tfit = poly_tdata["Tfit"]


  ; Set the graphics window
  IF ( create_window ) THEN $
    owin = WINDOW( WINDOW_TITLE = sensor_id+' channel '+STRTRIM(channel,2)+' (Teff-Tfit) residuals' )
  owin.SetCurrent
  owin.Erase
  ; ...Save it
  self.twRef = owin
  ; ...Set some plotting parameters
  yticklen = 0.01
  font_size = 9
  title = "Polychromatic coefficient fit residual for " + $
          STRTRIM(sensor_id,2)+' channel '+STRTRIM(channel,2)


  ; Plot the residuals
  self.tpRef = PLOT( $
    T,Teff - tfit, $
    XTITLE = 'Temperature (K)', $
    YTITLE = 'T!Deff!N - T!Dfit!N (K)', $
    TITLE     = title,$
    FONT_SIZE = font_size, $                   
    XTICKFONT_SIZE = xtickfont_size, $
    POSITION=[0.2,0.1,0.95,0.9], $
    CURRENT = owin, $
    COLOR   = color, $
    THICK   = 2, $
    _EXTRA  = Extra)
  !NULL = PLOT(self.tpRef.Xrange,[0,0],LINESTYLE='dashed',/OVERPLOT)

END
