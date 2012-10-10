PRO RTSolution::Plot, $
  Pressure   = pressure  , $  ; Input keyword
  Title      = title     , $  ; Input keyword
  NoYLog     = noylog    , $  ; Input keyword
  Owin       = owin      , $  ; Input keyword
  Png        = png       , $  ; Input keyword
  Debug      = debug          ; Input keyword
  
  ; Set up
  @rtsolution_pro_err_handler
  @rtsolution_parameters
  ; ...Do nothing if the structure is not allocated
  IF ( ~ self->Associated(Debug=debug) ) THEN RETURN
  ; ...Retrieve the RTSolution data
  self->Get_Property, $
    Debug = debug, $
    n_Layers            = n_layers           , $
    Algorithm           = algorithm          , $
    Sensor_Id           = sensor_id          , $
    Sensor_Channel      = sensor_channel     , $
    Upwelling_Radiance  = upwelling_radiance , $
    Layer_Optical_Depth = layer_optical_depth   
  ; ...Process boolean keywords
  buffer = KEYWORD_SET(png)
  ; ...Process other keywords
  IF ( N_ELEMENTS(pressure) NE 0 ) THEN BEGIN
    y           = pressure[SORT(pressure)]
    ytitle      = 'Pressure (hPa)'
    ylog        = KEYWORD_SET(noylog) ? 0 : 1
    ytickformat = 'logticks'
  ENDIF ELSE BEGIN
    y           = LINDGEN(n_layers) + 1L
    ytitle      = 'Layer number'
    ylog        = 0
    ytickformat = ''
  ENDELSE
  yrange = [MAX(y), MIN(y)]
  _title = Valid_String(title) ? title : 'RTSolution'
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
  
  
  ; Plot the upwelling radiance
  index++
  pur = PLOT( $
    upwelling_radiance, y, $
    TITLE  = STRTRIM(sensor_id,2) + ' channel ' + $
             STRTRIM(sensor_channel,2) + $
             '!CUpwelling Radiance Profile', $
    XTITLE = 'R (mW/(m!U2!N.sr.cm!U-1!N))', $
    YTITLE = ytitle, $
    YRANGE = yrange, $
    YLOG = ylog, $
    YTICKFORMAT = ytickformat, $
    /NODATA, $
    LAYOUT = [ n_col, n_row, index ], $
    CURRENT = owin )
  !NULL = PLOT( $
    upwelling_radiance, y, $
    OVERPLOT = pur, $
    COLOR = 'red' )
    
  
  ; Plot the layer optical depth
  index++
  plod = PLOT( $
    layer_optical_depth, y, $
    TITLE  = STRTRIM(sensor_id,2) + ' channel ' + $
             STRTRIM(sensor_channel,2) + $
             '!COptical Depth Profile', $
    XTITLE = '$\sigma$', $
    YTITLE = ytitle, $
    YRANGE = yrange, $
    YLOG = ylog, $
    YTICKFORMAT = ytickformat, $
    /NODATA, $
    LAYOUT = [ n_col, n_row, index ], $
    CURRENT = owin )
  !NULL = PLOT( $
    layer_optical_depth, y, $
    OVERPLOT = plod, $
    COLOR = 'red' )
    
END
