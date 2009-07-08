;+
; NAME:
;       AtmProfile::Plot
;
; PURPOSE:
;       The AtmProfile::Plot procedure method displays a valid AtmProfile object.
;
; CALLING SEQUENCE:
;       Obj->[AtmProfile::]Plot, $
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
;       atmprofile_parameters: Include file containing AtmProfile specific
;                              parameter value definitions.
;
;       atmprofile_pro_err_handler: Error handler code for AtmProfile procedures.
;
; EXAMPLE:
;       Given an instance of a AtmProfile object,
;
;         IDL> HELP, x
;         X               OBJREF    = <ObjHeapVar8(AtmProfile)>
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

PRO AtmProfile::Plot, $
  Debug     = Debug    , $ ; Input keyword
  Absorber_Id = Absorber_Id, $
  Max_Range = Max_Range, $ ; Input keyword
  XLOG      = xlog     , $ ; Input keyword
  YLOG      = ylog     , $ ; Input keyword
  _EXTRA    = Extra

  ; Set up
  @color_db
  ; ...AtmProfile parameters
  @atmprofile_parameters
  ; ...Set up error handler
  @atmprofile_pro_err_handler
  ; ...ALL *input* pointers must be associated
  IF ( NOT self->Associated(Debug=Debug) ) THEN $
    MESSAGE, 'Some or all input AtmProfile pointer members are NOT associated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Get the data
  self->Get_Property, $
    n_Absorbers         = n_Absorbers, $
    Absorber_Id         = aid, $
    Absorber_Units_Name = aun, $
    Level_Pressure      = p, $
    Level_Temperature   = t, $
    Level_Absorber      = a


  ; Check keywords
  IF ( N_ELEMENTS(Absorber_Id) EQ 0 ) THEN Absorber_Id = aid
  n_A2Plot = N_ELEMENTS(Absorber_Id)
  
  
  ; Save current plotting sysvars
  psave = !P
  xsave = !X
  ysave = !Y


  ; Set plotting parameters
  IF ( n_A2Plot LE 3 ) THEN $
    !P.MULTI = [0, n_A2Plot+1, 1] $
  ELSE $
    !P.MULTI = [0,4,2]
  !X.OMARGIN = [6,0]
  charsize = (!D.NAME EQ 'PS') ? 2.5 : 1.5
  font     = (!D.NAME EQ 'PS') ? 1 : -1
  yrange = [MAX(p),MIN(p)]
  xtickformat = KEYWORD_SET(xlog) ? 'logticks_exp' : ''
  ytickformat = KEYWORD_SET(ylog) ? 'logticks' : ''
  xmargin = [2,3]

  ; Plot the temperature
  IF ( N_ELEMENTS(Max_Range) GT 0 ) THEN $
    xrange = Max_Range.t $
  ELSE $
    xrange = [MIN(t),MAX(t)]
  PLOT, t, p, $
    TITLE = 'Temperature', $
    XTITLE = 'Temperature (K)', $
    XRANGE = xrange, xmargin=xmargin, $
    YTITLE = 'Pressure (hPa)', $
    YRANGE = yrange, /YSTYLE, $
    YLOG = ylog, $
    YTICKFORMAT = ytickformat, $
    CHARSIZE = charsize, $
    FONT = font, $
    _EXTRA = Extra
  self->Save_PlotVars, 0
  
  ; Plot the absorbers
  FOR j = 0, n_Absorbers-1 DO BEGIN
    loc = WHERE( Absorber_Id EQ aid[j], count )
    IF ( count GT 0 ) THEN BEGIN
      IF ( N_ELEMENTS(Max_Range) GT 0 ) THEN $
        xrange = Max_Range.a[*,j] $
      ELSE $
        xrange = [MIN(a[*,j]),MAX(a[*,j])]
      ; Only use a y-axis title for leftmost plots.
      IF ( (!P.MULTI[0] MOD !P.MULTI[1]) EQ 0 ) THEN BEGIN
        ytitle = 'Pressure (hPa)'
      ENDIF ELSE BEGIN
        ytitle = ''
      ENDELSE
      PLOT, a[*,j], p, $
        TITLE = STRTRIM(ATMPROFILE_ABSORBER_NAME[aid[j]],2), $
        XTITLE = 'Amount ('+STRTRIM(aun[j],2)+')', $
        XRANGE = xrange, xmargin=xmargin, $
        XLOG = xlog, $
        XTICKFORMAT = xtickformat, $
        YTITLE = ytitle, $
        YRANGE = yrange, /YSTYLE, $
        YLOG = ylog, $
        YTICKFORMAT = ytickformat, $
        CHARSIZE = charsize, $
        FONT = font, $
        _EXTRA = Extra
    ENDIF
    self->Save_PlotVars, j+1
  ENDFOR
    
 
  ; Restore plotting sysvars
  !P = psave
  !X = xsave
  !Y = ysave


  ; Done
  CATCH, /CANCEL
  
END ; PRO AtmProfile::Plot
