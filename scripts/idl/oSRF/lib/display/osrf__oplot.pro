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
  Debug     = Debug    , $ ; Input keyword
  Normalize = Normalize, $ ; Input keyword
  _EXTRA    = Extra        ; Input keyword

  ; Set up
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
  ; ...ALL *input* pointers must be associated
  IF ( NOT self->Associated(Debug=Debug) OR $
       NOT osrf->Associated(Debug=Debug)) THEN $
    MESSAGE, 'Some or all input OSRF pointer members are NOT associated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ; ...Get the number of SRF bands
  self->Get_Property, n_Bands=n_Bands


  ; Find maximum channel response if normalize keyword set
  IF ( KEYWORD_SET(Normalize) ) THEN BEGIN
    Max_r = -999.0d0
    FOR i = 0L, n_Bands-1L DO BEGIN
      osrf->Get_Property, i+1, Response = r
      Max_r = Max_r > MAX(r)
    ENDFOR
  ENDIF ELSE BEGIN
    Max_r = ONE
  ENDELSE
  
  
  ; Restore plotting variables
  self->Restore_PlotVars, wref, pref
  IF ( N_ELEMENTS(pref) NE n_Bands ) THEN $
    MESSAGE, 'Plot reference object array size different from the number of oSRF passbands.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  
  ; Loop over bands
  FOR i = 0L, n_Bands-1L DO BEGIN
    ; Get the band response
    osrf->Get_Property, $
      i+1, $
      Frequency = f, $
      Response  = r
    ; Plot it
    !NULL = OPLOT(f, r/Max_r, $
                  _EXTRA = Extra, $
                  CURRENT=wref, $
                  OVERPLOT=pref[i] )
  ENDFOR
  
END ; PRO OSRF::OPlot
