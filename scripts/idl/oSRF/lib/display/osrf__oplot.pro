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

PRO OSRF::OPlot, $
  osrf, $
  Debug     = debug    , $  ; Input keyword
  Color     = color    , $  ; Input keyword
  Thick     = thick    , $  ; Input keyword
  Normalize = normalize, $  ; Input keyword
  _EXTRA    = extra    , $  ; Input keyword
  pRef      = pref          ; Output keyword

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
  ; ...Get some sensor info
  self->Get_Property, $
    Sensor_Type = sensor_type, $
    n_Bands     = n_bands, $
    Debug       = debug
  is_microwave = (sensor_type EQ MICROWAVE_SENSOR)


  ; Find maximum channel response if normalize keyword set
  IF ( KEYWORD_SET(normalize) ) THEN BEGIN
    Max_r = -999.0d0
    FOR i = 0L, n_bands-1L DO BEGIN
      osrf->Get_Property, i+1, Response = r, Debug = debug
      Max_r = Max_r > MAX(r)
    ENDFOR
  ENDIF ELSE BEGIN
    Max_r = ONE
  ENDELSE
  
  
  ; Check plotting variables
  IF ( N_ELEMENTS(self.pRef) NE n_bands ) THEN $
    MESSAGE, 'Plot reference object hash size different from the number of oSRF passbands.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  ; Create plotting hash
  pref = HASH()
  
  
  ; Loop over bands
  FOR band = 1L, n_bands DO BEGIN
    
    ; Get the band response
    osrf->Get_Property, $
      band, $
      Debug     = debug, $
      f0        = f0   , $
      Frequency = f    , $
      Response  = r
      
    ; Convert frequency units to GHz for microwave sensors
    IF ( is_microwave ) THEN BEGIN
      f0 = inverse_cm_to_GHz(f0)
      f  = inverse_cm_to_GHz(f)
    ENDIF

    ; Plot it
    pref[band] = PLOT( f, r/Max_r, $
                       COLOR    = color, $
                       THICK    = thick, $
                       _EXTRA   = extra, $
                       OVERPLOT = self.pRef[band] )
    ; ...Plot the central frequency position
    !NULL = PLOT([f0,f0],(self.pRef[band]).YRANGE, $
                 LINESTYLE = 'dash', $
                 COLOR     = color, $
                 THICK     = thick, $
                 OVERPLOT  = self.pRef[band])
    
  ENDFOR
  
END
