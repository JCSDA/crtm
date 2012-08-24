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

PRO OSRF::Plot, $
  Normalize = Normalize, $ ; Input keyword
  Debug     = Debug    , $ ; Input keyword
  _EXTRA    = Extra

  ; Set up
  @color_db
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
  ; ...ALL *input* pointers must be associated
  IF ( NOT self->Associated(Debug=Debug) ) THEN $
    MESSAGE, 'Some or all input OSRF pointer members are NOT associated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ; ...Get the number of SRF bands
  self->Get_Property, n_Bands=n_Bands, Channel=Channel, Sensor_Id=Sensor_Id

  ; Save current plotting sysvars
  psave = !P
  xsave = !X
  ysave = !Y


  ; Set plotting parameters
  !P.MULTI = [0,n_Bands,1]
  !X.OMARGIN = [10,0]
  charsize = (n_Bands GT 2) ? 2.0 : 1.0

  w = WINDOW(WINDOW_TITLE=Sensor_Id+', channel '+STRTRIM(Channel,2), $
             DIMENSIONS=[1000,500])
  p = OBJARR(n_Bands)

  ; Begin band response plots
  FOR i = 0L, n_Bands-1L DO BEGIN
    self->Get_Property, $
      i+1, $
      Frequency = f, $
      Response  = r
    ; Normalise data if required
    IF ( KEYWORD_SET(Normalize) ) THEN r = r/MAX(r)
    ; Only use a y-axis title and tickvalues for first plot.
    IF ( i EQ 0 ) THEN BEGIN
      ytitle = 'Relative response'
    ENDIF ELSE BEGIN
      ytitle = ''
      !Y.TICKNAME = REPLICATE(' ',30)
    ENDELSE
    ; Generate the xrange based on -/+ % of bandwidth
    fdelta = MAX(f)-MIN(f)
    df = 0.1*fdelta
    xrange = [MIN(f)-df,MAX(f)+df]
    ; Generate the title
    title = STRTRIM(Sensor_Id,2)+'   Ch.'+STRTRIM(Channel,2)
    IF ( n_Bands GT 1 ) THEN title = title +', band #'+STRTRIM(i+1,2)
    ; Plot the band response
    p[i] = PLOT(f, r, $
                TITLE=title, $
                XTITLE='Frequency', $
                YTITLE=ytitle, $
                XRANGE=xrange,/XSTYLE, $
                LAYOUT=[n_Bands,1,i+1], $
                CURRENT=w, $
                COLOR='red', $
                _EXTRA = Extra)
    self->Save_PlotVars, w, p
  ENDFOR

  
  ; Restore plotting sysvars
  !P = psave
  !X = xsave
  !Y = ysave

END ; PRO OSRF::Plot
