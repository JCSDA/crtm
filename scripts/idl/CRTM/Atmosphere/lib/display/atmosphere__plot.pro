PRO Atmosphere::Plot, $
  Pressure   = pressure  , $  ; Input keyword
  Title      = title     , $  ; Input keyword
  NoAerosols = noaerosols, $  ; Input keyword
  NoClouds   = noclouds  , $  ; Input keyword
  NoYLog     = noylog    , $  ; Input keyword
  Owin       = owin      , $  ; Input keyword
  Png        = png       , $  ; Input keyword
  Debug      = debug          ; Input keyword
  
  ; Set up
  @atmosphere_pro_err_handler
  @atmosphere_parameters
  ; ...Check the structure has been allocated
  IF ( ~ self->Associated(Debug=debug) ) THEN $
    MESSAGE, 'Object has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ; ...Retrieve the atmosphere data
  self->Get_Property, $
    Debug = debug, $
    n_Layers        = n_layers       , $
    n_Absorbers     = n_absorbers    , $
    n_Clouds        = n_clouds       , $
    n_Aerosols      = n_aerosols     , $
    Climatology     = climatology    , $
    Absorber_ID     = absorber_id    , $
    Absorber_Units  = absorber_units , $
    Pressure        = _pressure      , $
    Temperature     = temperature    , $
    Absorber_Amount = absorber_amount, $
    Cloud           = cloud          , $
    Aerosol         = aerosol        
  ; ...Process boolean keywords
  plot_clouds   = ~ KEYWORD_SET(noclouds)
  plot_aerosols = ~ KEYWORD_SET(noaerosols)
  ylog          = ~ KEYWORD_SET(noylog)
  buffer        = KEYWORD_SET(png)
  ; ...Process other keywords
  y = (N_ELEMENTS(pressure) GT 0) ? pressure[SORT(pressure)] : _pressure
  yrange = [MAX(y), MIN(y)]
  _title = Valid_String(title) ? title : 'Atmosphere'
  create_window = KEYWORD_SET(owin) ? ~ ISA(owin,'GraphicsWin') : 1
  ; ...The graphic layout, col x row.
  n_col = 2
  n_row = CEIL(DOUBLE(n_absorbers+1)/n_col)
  

  ; Set the graphics window
  IF ( create_window ) THEN BEGIN
    xsize = 640
    ysize = (n_row LE 4) ? 256*n_row : 256*4
    owin = WINDOW( $
      WINDOW_TITLE = _title, $
      DIMENSIONS = [xsize, ysize], $
      BUFFER = buffer )
  ENDIF
  owin.SetCurrent
  ; ...Initialise layout index
  index = 0
  
  
  ; Plot the temperature
  index++
  pt = PLOT( $
    temperature, y, $
    TITLE  = 'Temperature profile', $
    XTITLE = 'T (K)', $
    YTITLE = 'Pressure (hPa)', $
    YRANGE = yrange, $
    YLOG = ylog, $
    YTICKFORMAT = 'logticks', $
    /NODATA, $
    LAYOUT = [ n_col, n_row, index ], $
    CURRENT = owin )
  !NULL = PLOT( $
    temperature, y, $
    OVERPLOT = pt, $
    COLOR = 'red' )
    
  
  ; Plot the absorber data
  FOR j = 0, n_absorbers-1 DO BEGIN
    index++
    absorber_name = STRTRIM(ABSORBER_ID_NAME[absorber_id[j]],2)
    units_name    = STRTRIM(ABSORBER_UNITS_NAME[absorber_units[j]],2)
    pa = PLOT( $
      absorber_amount[*,j], y, $
      TITLE  = absorber_name + ' profile', $
      XTITLE = 'Amount (' + units_name + ')', $
      YTITLE = 'Pressure (hPa)', $
      YRANGE = yrange, $
      YLOG = ylog, $
      YTICKFORMAT = 'logticks', $
      /NODATA, $
      LAYOUT = [ n_col, n_row, index ], $
      CURRENT = owin )
    !NULL = PLOT( $
      absorber_amount[*,j], y, $
      OVERPLOT = pa, $
      COLOR = 'red' )
  ENDFOR


  ; Plot the cloud data
  IF ( plot_clouds ) THEN BEGIN
    FOR n = 0, n_clouds-1 DO BEGIN
      cloud[n].Plot, $
        Pressure = y, $
        Title    = _title + '; Cloud #' + STRTRIM(n+1,2), $
        YNoLog   = ynolog  , $
        Png      = png     , $
        Debug    = debug      
    ENDFOR
  ENDIF

  ; Plot the aerosol data
  IF ( plot_aerosols ) THEN BEGIN
    FOR n = 0, n_aerosols-1 DO BEGIN
      aerosol[n].Plot, $
        Pressure = y, $
        Title    = _title + '; Aerosol #' + STRTRIM(n+1,2), $
        YNoLog   = ynolog  , $
        Png      = png     , $
        Debug    = debug      
    ENDFOR
  ENDIF
  
END
