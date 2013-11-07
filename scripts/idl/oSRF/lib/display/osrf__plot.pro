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
    owin = WINDOW( WINDOW_TITLE = sensor_id+', channel '+STRTRIM(Channel,2)+' oSRF' )
  owin.SetCurrent
  owin.Erase
  ; ...Save it
  self.wRef = owin
  ; ...Set some band-based plotting parameters
  CASE n_bands OF
    2: BEGIN
         yticklen = 0.02
         xtickfont_size = 8
       END
    4: BEGIN
         yticklen = 0.035
         xtickfont_size = 6
       END
    ELSE: BEGIN
            yticklen = 0.01
            xtickfont_size = 9
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
    IF ( KEYWORD_SET(Normalize) ) THEN r = r/MAX(r)


    ; Generate the xrange based on -/+ % of bandwidth
    fdelta = f[-1] - f[0]
    df = 0.1*fdelta
    xrange = [f[0]-df,f[-1]+df]


    ; Generate the yrange from 0->ymax+1%
    IF ( KEYWORD_SET(ylog) ) THEN BEGIN
      ymax = 2.0*MAX(r)
      ymin = MIN(r)/2.0
      ytickformat = 'logticks_exp'
    ENDIF ELSE BEGIN
      ymin = 0 < MIN(r)
      ymax = MAX(r) + 0.01*MAX(r)
      ytickformat = ''
    ENDELSE
    yrange = [ymin, ymax]


    ; Set the band-specific plotting parameters
    IF ( band EQ 1 ) THEN BEGIN
      ytitle = 'Relative response'
      master_ymin = ymin
      master_ymax = ymax
    ENDIF ELSE BEGIN  
      ytitle = ''
    ENDELSE


    ; Generate the title
    title = STRTRIM(sensor_id,2)+'   Ch.'+STRTRIM(channel,2)
    IF ( n_bands GT 1 ) THEN title = title +', band #'+STRTRIM(band,2)


    ; Compute plot positions
    dx_left    = 0.1d0
    dx_right   = 0.0d0
    dx_middle  = 1.0d0 - dx_left - dx_right
    dx = dx_middle/DOUBLE(n_bands)
    x1 = dx_left + DOUBLE(band-1)*dx
    x2 = x1+dx-0.05d0
    
    
    ; Plot the band response
    self.pRef[band] = PLOT( $
      f, r, $
      TITLE=title, $
      XTITLE=xtitle, $
      YTITLE=ytitle, $
      XRANGE=xrange, /XSTYLE, $
      YRANGE=yrange, /YSTYLE, $
      YLOG=ylog, $
      YTICKFORMAT=ytickformat, $
      XTICKLEN=0.02, $
      XTICKFONT_SIZE=xtickfont_size, $
      YTICKLEN=yticklen, $
      LAYOUT=[n_bands,1,band], $
      POSITION=[x1,0.1,x2,0.9], $
      FONT_SIZE = 9, $                   
      CURRENT=owin, $
      THICK=2, $
      _EXTRA = Extra)


    ; Adjust the yrange if necessary
    IF ( band GT 1 ) THEN BEGIN
      IF ( ymin LT master_ymin ) THEN master_ymin = ymin
      IF ( ymax GT master_ymax ) THEN master_ymax = ymax
      FOR j = 1, band DO self.pRef[j].yrange = [master_ymin,master_ymax]
    ENDIF                  

  ENDFOR

  ; Plot the central frequency location for single bands
  IF ( n_bands EQ 1 ) THEN BEGIN
    !NULL = PLOT([f0,f0],self.pRef[1].yrange, $
                 LINESTYLE='dash', $
                 COLOR='blue', $
                 THICK=2, $
                 OVERPLOT=self.pRef[1])
  ENDIF

END
