;+
; NAME:
;       OSRF::OPlot
;
; PURPOSE:
;       The OSRF::OPlot procedure method displays a valid OSRF object
;       on a previously drawn plot.
;
; CALLING SEQUENCE:
;       Obj->[OSRF::]OPlot, $
;         oSRF, $
;         Normalize = Normalize, $  ; Input keyword
;         Debug     = Debug         ; Input keyword
;
; INPUT ARGUMENTS:
;       oSRF:        The OSRF object to overplot.
;                    UNITS:      N/A
;                    TYPE:       OSRF object
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN)
;
; KEYWORDS:
;       Along with any keywords accepted by the IDL PLOT procedure, the 
;       following keywords can be used with this method:
;
;       Normalize:   Set this keyword to scale the OSRF data plots
;                    such that the maximum value plotted is 1.0.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
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
;       Given instances of OSRF objects,
;
;         IDL> HELP, x1, x2
;         X1              OBJREF    = <ObjHeapVar8(OSRF)>
;         X2              OBJREF    = <ObjHeapVar9(OSRF)>
;
;       the first object, x1, is plotted,
;
;         IDL> x1->Plot
;
;       and the second object, x2, is overplotted like so:
;
;         IDL> x1->OPlot, x2
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 20-Apr-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO OSRF::Tfit_OPlot, $
  osrf, $
  Debug  = debug, $  ; Input keyword
  Color  = color, $  ; Input keyword
  pRef   = pref , $  ; Output keyword
  _EXTRA = extra     ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
  ; ...ALL *input* pointers must be associated
  IF ( ~ self->Associated(Debug=debug) OR $
       ~ osrf->Associated(Debug=debug)) THEN $
    MESSAGE, 'Some or all input OSRF pointer members are NOT associated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Get the srf info and Tdata
  self.Get_Property, $
    Debug       = debug      , $
    n_Bands     = n_bands    , $
    Channel     = channel    , $
    Sensor_Id   = sensor_id  , $
    Sensor_Type = sensor_type
  osrf.Get_Property, $
    Debug       = debug      , $
    poly_Tdata  = poly_tdata
  T    = poly_tdata["T"]
  Teff = poly_tdata["Teff"]
  Tfit = poly_tdata["Tfit"]


  ; Plot it
  pref = PLOT( T,Teff - tfit, $
               COLOR    = color, $
               _EXTRA   = extra, $
               OVERPLOT = self.tpRef )

END
