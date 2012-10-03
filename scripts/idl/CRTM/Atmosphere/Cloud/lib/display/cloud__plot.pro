PRO Cloud::Plot, $
  Pressure = pressure, $  ; Input keyword
  Title    = title   , $  ; Input keyword
  YNoLog   = ynolog  , $  ; Input keyword
  Owin     = owin    , $  ; Input keyword
  Png      = png     , $  ; Input keyword
  Debug    = debug        ; Input keyword
  
  ; Set up
  @cloud_pro_err_handler
  ; ...Check the structure has been allocated
  IF ( ~ self->Associated(Debug=debug) ) THEN $
    MESSAGE, 'Object has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ; ...Retrieve the cloud data
  self->Get_Property, $
    Debug = debug, $
    n_Layers = n_layers, $
    Type = type, $
    Effective_Radius = x_re, $
    Water_Content = x_wc
  cloud_name = self->TypeName() + ' cloud'
  ; ...Process boolean keywords
  buffer = KEYWORD_SET(png)
  ; ...Process other keywords
  IF ( N_ELEMENTS(pressure) NE 0 ) THEN BEGIN
    y           = pressure[SORT(pressure)]
    ytitle      = 'Pressure (hPa)'
    ylog        = KEYWORD_SET(ynolog) ? 0 : 1
    ytickformat = 'logticks'
  ENDIF ELSE BEGIN
    y           = LINDGEN(n_layers) + 1L
    ytitle      = 'Layer number'
    ylog        = 0
    ytickformat = ''
  ENDELSE
  yrange = [MAX(y), MIN(y)]
  create_window = KEYWORD_SET(owin) ? ~ ISA(owin,'GraphicsWin') : 1
  ; ...The graphic layout, col x row.
  n_col = 2
  n_row = 1
  

  ; Create a window
  IF ( create_window ) THEN BEGIN
    owin = WINDOW( $
      WINDOW_TITLE = title, $
      BUFFER = buffer )
  ENDIF
  owin.SetCurrent
  ; ...Initialise layout index
  index = 0
  
  
  ; Plot the effective radius
  index++
  pr = PLOT( $
    x_re, y, $
    TITLE  = cloud_name + '!CEffective radius', $
    XTITLE = 'r!Deff!N ($\mu$m)', $
    YTITLE = ytitle, $
    YRANGE = yrange, $
    YLOG = ylog, $
    YTICKFORMAT = ytickformat, $
    /NODATA, $
    LAYOUT = [ n_col,n_row, index ], $
    CURRENT = owin )
  !NULL = PLOT( $
    x_re, y, $
    OVERPLOT = pr, $
    COLOR = 'red' )
    
  
  ; Plot the water content
  index++
  pw = PLOT( $
    x_wc, y, $
    TITLE  = cloud_name + '!CWater content', $
    XTITLE = 'q (kg.m!U-2!N)', $
    YTITLE = ytitle, $
    YRANGE = yrange, $
    YLOG = ylog, $
    YTICKFORMAT = ytickformat, $
    /NODATA, $
    LAYOUT = [ n_col,n_row, index ], $
    CURRENT = owin )
  !NULL = PLOT( $
    x_wc, y, $
    OVERPLOT = pw, $
    COLOR = 'red' )
    
END
