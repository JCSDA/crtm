PRO Aerosol::Plot, $
  Pressure   = pressure  , $  ; Input keyword
  Diff_Input = diff_input, $  ; Input keyword
  Title      = title     , $  ; Input keyword
  YNoLog     = ynolog    , $  ; Input keyword
  Owin       = owin      , $  ; Input keyword
  Png        = png       , $  ; Input keyword
  Debug      = debug          ; Input keyword
  
  ; Set up
  @aerosol_pro_err_handler
  ; ...Check the structure has been allocated
  IF ( ~ self->Associated(Debug=debug) ) THEN $
    MESSAGE, 'Object has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ; ...Retrieve the aerosol data
  self->Get_Property, $
    Debug = debug, $
    n_Layers = n_layers, $
    Type = type
  aerosol_name = self->TypeName() + ' aerosol'
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
  

  ; Check difference input keyword separately (coz it's unwieldy)
  plot_difference = FALSE
  IF ( N_ELEMENTS(diff_input) GT 0 ) THEN BEGIN
    ; Must be an Aerosol to start with
    IF ( ISA(diff_input,'Aerosol') ) THEN BEGIN
      ; Must have the same dimensions as self
      diff_input->Get_Property, $
        Debug = debug, $
        n_Layers = diff_n_layers, $
        Type     = diff_type
      IF ( (n_layers EQ diff_n_layers) && (type EQ diff_type) ) THEN BEGIN
        ; We can use this!
        plot_difference = TRUE
      ENDIF
    ENDIF
  ENDIF
  ; ...Set title modifier
  delta = plot_difference ? '$\Delta$' : ''
  
  
  ; Create a window
  IF ( create_window ) THEN BEGIN
    owin = WINDOW( $
      WINDOW_TITLE = title, $
      BUFFER = buffer )
  ENDIF
  owin.SetCurrent
  ; ...Initialise layout index
  index = 0
  
  
  ; Display the effective radius
  ; ...Extract the data
  self->Get_Property, Effective_Radius = x, Debug = debug
  ; ...Process the difference data
  IF ( plot_difference ) THEN BEGIN
    ; Extract and difference the data
    diff_input->Get_Property, Effective_Radius = x2, Debug = debug
    x  = x - x2
  ENDIF
  ; ...Create scaled x-data for pretty plotting
  axis_scale, [MIN(x),MAX(x)], 'r!Deff!N ($\mu$m)', xscale, xtitle
  x = x * xscale
  ; ...Plot the profile
  index++
  pr = PLOT( $
    x, y, $
    TITLE  = aerosol_name + '!C' + delta + ' Effective radius', $
    XTITLE = delta+xtitle, $
    YTITLE = ytitle, $
    YRANGE = yrange, $
    YLOG = ylog, $
    YTICKFORMAT = ytickformat, $
    COLOR = 'red', $
    LAYOUT = [ n_col, n_row, index ], $
    CURRENT = owin )
    
  
  ; Display the concentration
  ; ...Extract the data
  self->Get_Property, Concentration = x, Debug = debug
  ; ...Process the difference data
  IF ( plot_difference ) THEN BEGIN
    ; Extract and difference the data
    diff_input->Get_Property, Concentration = x2, Debug = debug
    x  = x - x2
  ENDIF
  ; ...Create scaled x-data for pretty plotting
  axis_scale, [MIN(x),MAX(x)], 'q (kg.m!U-2!N)', xscale, xtitle
  x = x * xscale
  ; ...Plot the profile
  index++
  pc = PLOT( $
    x, y, $
    TITLE  = aerosol_name + '!C' + delta + ' Concentration', $
    XTITLE = delta+xtitle, $
    YTITLE = ytitle, $
    YRANGE = yrange, $
    YLOG = ylog, $
    YTICKFORMAT = ytickformat, $
    COLOR = 'red', $
    LAYOUT = [ n_col, n_row, index ], $
    CURRENT = owin )
    
END
