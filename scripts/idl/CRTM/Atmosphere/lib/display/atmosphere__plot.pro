PRO Atmosphere::Plot, $
  Pressure   = pressure  , $  ; Input keyword
  Diff_Input = diff_input, $  ; Input keyword
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
    n_Layers        = n_layers      , $
    n_Absorbers     = n_absorbers   , $
    n_Clouds        = n_clouds      , $
    n_Aerosols      = n_aerosols    , $
    Absorber_ID     = absorber_id   , $
    Absorber_Units  = absorber_units, $
    Pressure        = _pressure      
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
  n_row = CEIL(DOUBLE(n_absorbers+2)/n_col)
  

  ; Check difference input keyword separately (coz it's unwieldy)
  plot_difference = FALSE
  IF ( N_ELEMENTS(diff_input) GT 0 ) THEN BEGIN
    ; Must be an Atmosphere to start with
    IF ( ISA(diff_input,'Atmosphere') ) THEN BEGIN
      ; Must have the same dimensions as self
      diff_input->Get_Property, $
        Debug = debug, $
        n_Layers    = diff_n_layers   , $
        n_Absorbers = diff_n_absorbers, $
        n_Clouds    = diff_n_clouds   , $
        n_Aerosols  = diff_n_aerosols
      IF ( (n_layers EQ diff_n_layers) && (n_absorbers EQ diff_n_absorbers) && $
           (n_clouds EQ diff_n_clouds) && (n_aerosols EQ diff_n_aerosols)) THEN BEGIN
        ; We can use this!
        plot_difference = TRUE
      ENDIF
    ENDIF
  ENDIF
  ; ...Set title modifier
  delta = plot_difference ? '$\Delta$' : ''
  
  
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
  
  
  ; Display the temperature data
  ; ...Extract the data
  self->Get_Property, Temperature = x, Debug = debug
  ; ...Process the difference data
  IF ( plot_difference ) THEN BEGIN
    ; Extract and difference the data
    diff_input->Get_Property, Temperature = x2, Debug = debug
    x  = x - x2
  ENDIF
  ; ...Create scaled x-data for pretty plotting
  axis_scale, [MIN(x),MAX(x)], 'T (K)', xscale, xtitle
  x = x * xscale
  ; ...Plot the temperature
  index++
  pt = PLOT( $
    x, y, $
    TITLE  = 'Temperature profile', $
    XTITLE = xtitle, $
    YTITLE = 'Pressure (hPa)', $
    YRANGE = yrange, $
    YLOG = ylog, $
    YTICKFORMAT = 'logticks', $
    COLOR = 'red', $
    LAYOUT = [ n_col, n_row, index ], $
    CURRENT = owin )
    
  
  ; Display the cloud fraction data
  ; ...Extract the data
  self->Get_Property, CFraction = x, Debug = debug
  ; ...Process the difference data
  IF ( plot_difference ) THEN BEGIN
    ; Extract and difference the data
    diff_input->Get_Property, CFraction = x2, Debug = debug
    x  = x - x2
  ENDIF
  ; ...Create scaled x-data for pretty plotting
  axis_scale, [MIN(x),MAX(x)], 'Cloud fraction', xscale, xtitle
  x = x * xscale
  ; ...Plot the cloud fraction
  index++
  pcf = PLOT( $
    x, y, $
    TITLE  = 'Cloud fraction profile', $
    XTITLE = xtitle, $
    YTITLE = 'Pressure (hPa)', $
    YRANGE = yrange, $
    YLOG = ylog, $
    YTICKFORMAT = 'logticks', $
    COLOR = 'red', $
    LAYOUT = [ n_col, n_row, index ], $
    CURRENT = owin )

    
  ; Display the absorber data
  ; ...Extract the data
  self->Get_Property, Absorber_Amount = x, Debug = debug
  ; ...Process the difference data
  IF ( plot_difference ) THEN BEGIN
    ; Extract and difference the data
    diff_input->Get_Property, Absorber_Amount = x2, Debug = debug
    x  = x - x2
  ENDIF
  ; ...Plot the data
  FOR j = 0, n_absorbers-1 DO BEGIN
    ; ...Create scaled x-data for pretty plotting
    xa = x[*,j]
    axis_scale, [MIN(xa),MAX(xa)], STRTRIM(ABSORBER_UNITS_NAME[absorber_units[j]],2), xscale, xtitle
    xa = xa * xscale
    ; ...Plot it
    index++
    absorber_name = STRTRIM(ABSORBER_ID_NAME[absorber_id[j]],2)
    pa = PLOT( $
      xa, y, $
      TITLE  = absorber_name + ' profile', $
      XTITLE = xtitle, $
      YTITLE = 'Pressure (hPa)', $
      YRANGE = yrange, $
      YLOG = ylog, $
      YTICKFORMAT = 'logticks', $
      COLOR = 'red', $
      LAYOUT = [ n_col, n_row, index ], $
      CURRENT = owin )
  ENDFOR


  ; Plot the cloud data
  IF ( plot_clouds && (n_clouds GT 0) ) THEN BEGIN
    ; ...Extract the data
    self->Get_Property, Cloud = cloud, Debug = debug
    IF ( plot_difference ) THEN diff_input->Get_Property, Cloud = diff_cloud, Debug = debug
    ; ...Loop over clouds
    FOR n = 0, n_clouds-1 DO BEGIN
      IF ( plot_difference ) THEN diff_input = diff_cloud[n]
      cloud[n].Plot, $
        Pressure   = y, $
        Diff_Input = diff_input, $
        Title      = _title + '; Cloud #' + STRTRIM(n+1,2), $
        YNoLog     = ynolog  , $
        Png        = png     , $
        Debug      = debug      
    ENDFOR
  ENDIF

  ; Plot the aerosol data
  IF ( plot_aerosols && (n_aerosols GT 0) ) THEN BEGIN
    ; ...Extract the data
    self->Get_Property, aerosol = aerosol, Debug = debug
    IF ( plot_difference ) THEN diff_input->Get_Property, aerosol = diff_aerosol, Debug = debug
    ; ...Loop over aerosols
    FOR n = 0, n_aerosols-1 DO BEGIN
      IF ( plot_difference ) THEN diff_input = diff_aerosol[n]
      aerosol[n].Plot, $
        Pressure   = y, $
        Diff_Input = diff_input, $
        Title      = _title + '; Aerosol #' + STRTRIM(n+1,2), $
        YNoLog     = ynolog  , $
        Png        = png     , $
        Debug      = debug      
    ENDFOR
  ENDIF
  
END
