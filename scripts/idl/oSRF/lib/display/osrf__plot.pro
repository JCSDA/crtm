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
  Normalize = normalize, $  ; Input keyword
  Debug     = debug    , $  ; Input keyword
  Ylog      = ylog     , $  ; Input keyword
  Color     = color    , $  ; Input keyword
  Owin      = owin     , $  ; Input keyword
  gTitle    = gTitle   , $  ; Input keyword
  EPS       = eps      , $  ; Input keyword (increase font size for eps output)
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
  IF ( KEYWORD_SET(eps) ) THEN BEGIN
    create_window = TRUE
  ENDIF ELSE BEGIN
    IF ( KEYWORD_SET(owin) ) THEN BEGIN
      create_window = ~ ISA(owin,'GraphicsWin')
    ENDIF ELSE BEGIN
      create_window = TRUE
    ENDELSE
  ENDELSE


  ; Get the sensor info
  self.Get_Property, $
    Debug       = debug    , $
    n_Bands     = n_bands  , $
    Channel     = channel  , $
    Sensor_Id   = sensor_id, $
    Sensor_Type = sensor_type
  is_microwave = ( sensor_type EQ MICROWAVE_SENSOR )


  ; Set the graphics window
  IF ( create_window ) THEN $
    owin = WINDOW( WINDOW_TITLE = sensor_id+', channel '+STRTRIM(Channel,2)+' oSRF', $
                   BUFFER = KEYWORD_SET(eps) )
  owin.SetCurrent
  owin.Erase
  ; ...Save it
  self.wRef = owin
  
  
  ; Set some plotting parameters
  font_size = KEYWORD_SET(eps) ? EPS_FONT_SIZE : WIN_FONT_SIZE
  xticklen  = 0.02
  CASE n_bands OF
    2: BEGIN
       yticklen = 0.02
       n_xplots = 2
       n_yplots = 1
       margin = [0.155, 0.1, 0.05, 0.1] 
       END
    4: BEGIN
       yticklen = 0.02
       n_xplots = 2
       n_yplots = 2
       margin = [0.155, 0.165, 0.05, 0.1]
       END
    ELSE: BEGIN
          yticklen = 0.01
          n_xplots = 1
          n_yplots = 1
          margin = KEYWORD_SET(eps) ? [0.2, 0.15, 0.05, 0.1] : [0.1, 0.1, 0.05, 0.1]
          END
  ENDCASE
  ; ...Initialise cross-band min/max
  master_ymin =  1.0e+10
  master_ymax = -1.0e+10
  ; ...Default frequency title
  xtitle = 'Frequency (cm!U-1!N)'
  IF ( is_microwave ) THEN xtitle = 'Frequency (GHz)'

  
  ; Begin band response plots
  FOR band = 1L, n_bands DO BEGIN

  
    ; Get the band data
    self->Get_Property, $
      band, $
      Debug     = debug, $
      f0        = f0   , $
      Frequency = f    , $
      Response  = r

    ; Convert to units to GHz for microwave sensors
    IF ( is_microwave ) THEN BEGIN
      f0 = inverse_cm_to_GHz(f0)
      f  = inverse_cm_to_GHz(f)
    ENDIF

    ; Normalise data if required
    IF ( KEYWORD_SET(normalize) ) THEN r = r/MAX(r)


    ; Generate the xrange based on -/+ % of bandwidth
    IF ( n_bands GT 1 ) THEN BEGIN
      fdelta = f[-1] - f[0]
      df = 0.1*fdelta
      xrange = [f[0]-df,f[-1]+df]
    ENDIF
    xstyle = 1


    ; Generate the yrange from 0->ymax+1%
    IF ( KEYWORD_SET(ylog) ) THEN BEGIN
      ymax = 2.0*MAX(r)
      ymin = MIN(r)/2.0
      ytickformat = 'logticks_exp'
    ENDIF ELSE BEGIN
      ymin = -0.01*MAX(r) < MIN(r)
      ymax = MAX(r) + 0.01*MAX(r)
      ytickformat = ''
    ENDELSE
    yrange = [ymin, ymax]
    ystyle = 1


    ; Set the band-specific plotting parameters
    IF ( band EQ 1 ) THEN BEGIN
      master_ymin = ymin
      master_ymax = ymax
    ENDIF


    ; Generate the titles
    ; ...The plot title
    IF ( KEYWORD_SET(gTitle) ) THEN $
      title = "Ch." + STRTRIM(channel,2) $
    ELSE $
      title = STRTRIM(sensor_id,2) + " Ch." + STRTRIM(channel,2)
    IF ( n_bands GT 1 ) THEN title = title +", band #" + STRTRIM(band,2)
    ; ...The yaxis title
    ytitle = "Relative response"
    
    
    ; Modify stuff if it is a difference plot
    IF ( self.Flag_Is_Set(IS_DIFFERENCE_FLAG, Debug=debug) ) THEN BEGIN
      ytitle = "$\Delta$(" + ytitle + ")"
      yrange = !NULL
      ystyle = 0
    ENDIF
    

    ; Plot the band response
    self.pRef[band] = PLOT( $
      f, r, $
      TITLE          = title, $
      XTITLE         = xtitle, $
      YTITLE         = ytitle, $
      XRANGE         = xrange, $
      XSTYLE         = xstyle, $
      YRANGE         = yrange, $
      YSTYLE         = ystyle, $
      YLOG           = ylog, $
      YTICKFORMAT    = ytickformat, $
      XTICKLEN       = xticklen, $
      XTICKFONT_SIZE = font_size, $
      YTICKLEN       = yticklen, $
      LAYOUT         = [n_xplots, n_yplots, band], $
      MARGIN         = margin, $
      FONT_SIZE      = font_size, $  
      CURRENT        = owin, $
      COLOR          = color, $
      _EXTRA         = extra)

    ; Adjust the yrange if necessary
    IF ( band GT 1 ) THEN BEGIN
      IF ( ymin LT master_ymin ) THEN master_ymin = ymin
      IF ( ymax GT master_ymax ) THEN master_ymax = ymax
      FOR j = 1, band DO self.pRef[j].yrange = [master_ymin,master_ymax]
    ENDIF                  

  ENDFOR
  
  
  ; Additional overplots
  FOR band = 1L, n_bands DO BEGIN
    ; ...Plot the central frequency position
    !NULL = PLOT([f0,f0],(self.pRef[band]).YRANGE, $
                 LINESTYLE = 'dash', $
                 COLOR     = color, $
                 OVERPLOT  = self.pRef[band])
    ; ...Plot a zero line
    IF ( self.Flag_Is_Set(IS_DIFFERENCE_FLAG, Debug=debug) ) THEN $
      !NULL = PLOT((self.pRef[band]).XRANGE, [0,0], $
                   LINESTYLE = 'dash', $
                   OVERPLOT  = self.pRef[band])
  ENDFOR

END
