PRO Atmosphere_List::Channel_Plot, $
  layer, $  ; Input
  Diff_Input = diff_input, $  ; Input keyword
  Channel    = channel   , $  ; Input keyword
  Title      = title     , $  ; Input keyword
  Owin       = owin      , $  ; Input keyword
  Png        = png       , $  ; Input keyword
  Debug      = debug          ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  @atmosphere_pro_err_handler
  @atmosphere_parameters
  ; ...Check that all list members are Atmosphere objects
  IF ( ~ self->HasOnly_Atmospheres(Debug = debug) ) THEN $
    MESSAGE, 'Non-Atmosphere object found in channel list.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ; ...Process boolean keywords
  buffer = KEYWORD_SET(png)
  ; ...Process other keywords
  _title = Valid_String(title) ? title : 'Atmosphere Channel'
  create_window = KEYWORD_SET(owin) ? ~ ISA(owin,'GraphicsWin') : 1
  display_current_channel = N_ELEMENTS(channel) GT 0
  IF ( display_current_channel ) THEN _channel = [LONG(channel[0]), LONG(channel[0])]
  

  ; Get some dimension and absorber information from the first list entry
  self[0]->Get_Property, $
    Debug = debug, $
    n_Layers       = n_layers, $
    n_Absorbers    = n_absorbers, $
    Absorber_Id    = absorber_id, $
    Absorber_Units = absorber_units
  ; ...Check layer
  IF ( layer LT 1 || layer GT n_layers ) THEN $
    MESSAGE, 'Invalid layer, ' + STRTRIM(layer,2) + $
             ', specified. Valid range is 1-' + STRTRIM(n_layers,2), $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Define the graphic layout, col x row.
  n_items = n_absorbers + 1 ; temperature
  n_col = 2
  n_row = CEIL(DOUBLE(n_items)/n_col)


  ; Check difference input keyword separately (coz it's unwieldy)
  plot_difference = FALSE
  IF ( N_ELEMENTS(diff_input) GT 0 ) THEN BEGIN
    ; Must be an Atmosphere list to start with
    IF ( ISA(diff_input,'Atmosphere_List') ) THEN BEGIN
      ; Must be an Atmosphere-object-only list
      IF ( diff_input->HasOnly_Atmospheres(Debug = debug) ) THEN BEGIN
        ; Must have the same number of channels as self
        IF ( self.Count() EQ diff_input.Count() ) THEN BEGIN
          ; Must have same number of layers and absorbers as self
          diff_input[0]->Get_Property, $
            Debug = debug, $
            n_Layers    = diff_n_layers, $
            n_Absorbers = diff_n_absorbers
          IF ( (n_layers EQ diff_n_layers) && (n_absorbers EQ diff_n_absorbers) ) THEN BEGIN
            ; We can use this!
            plot_difference = TRUE
          ENDIF
        ENDIF
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


  ; Set channel-number-dependent plotting parameters
  n_channels = self.Count()
  IF ( n_channels LT 50 ) THEN BEGIN
    symbol = 'diamond'
  ENDIF ELSE BEGIN
    symbol = 'none'
  ENDELSE
  channel_number = LINDGEN(n_channels)+1L
  xtitle = 'Channel number'


  ; Display the temperature data
  ; ...Extract the data
  y = self->Get_Channel_Data('temperature', Layer = layer, Debug = debug)
  ; ...Process the difference list
  IF ( plot_difference ) THEN BEGIN
    ; Extract and difference the data
    y2 = diff_input->Get_Channel_Data('temperature', Layer = layer, Debug = debug)
    y  = y - y2
  ENDIF
  ; ...Create scaled y-data for pretty plotting
  axis_scale, [MIN(y),MAX(y)], 'T (K)', yscale, ytitle
  y = y * yscale
  ; ...Plot the data
  index++
  pt = PLOT( $
    channel_number, y, $
    TITLE  = delta + 'Temperature spectrum at layer ' + STRTRIM(layer,2), $
    XTITLE = xtitle, $
    YTITLE = ytitle, $
    LAYOUT = [ n_col, n_row, index ], $
    COLOR = 'red', $
    SYMBOL = symbol, $
    CURRENT = owin )
  IF ( plot_difference ) THEN $
    !NULL = PLOT( $
      pt.Xrange, [0,0], $
      LINESTYLE = 'dash', $
      OVERPLOT = pt )
  ; ...Overplot the current channel indicator if supplied
  IF ( display_current_channel ) THEN $
    !NULL = PLOT( $
      _channel, pt.Yrange, $
      COLOR = 'green', $
      OVERPLOT = pt )


  ; Display the cloud fraction data
  ; ...Extract the data
  y = self->Get_Channel_Data('cfraction', Layer = layer, Debug = debug)
  ; ...Process the difference list
  IF ( plot_difference ) THEN BEGIN
    ; Extract and difference the data
    y2 = diff_input->Get_Channel_Data('cfraction', Layer = layer, Debug = debug)
    y  = y - y2
  ENDIF
  ; ...Create scaled y-data for pretty plotting
  axis_scale, [MIN(y),MAX(y)], 'Cloud fraction', yscale, ytitle
  y = y * yscale
  ; ...Plot the data
  index++
  pcf = PLOT( $
    channel_number, y, $
    TITLE  = delta + 'Cloud fraction spectrum at layer ' + STRTRIM(layer,2), $
    XTITLE = xtitle, $
    YTITLE = ytitle, $
    LAYOUT = [ n_col, n_row, index ], $
    COLOR = 'red', $
    SYMBOL = symbol, $
    CURRENT = owin )
  IF ( plot_difference ) THEN $
    !NULL = PLOT( $
      pcf.Xrange, [0,0], $
      LINESTYLE = 'dash', $
      OVERPLOT = pcf )
  ; ...Overplot the current channel indicator if supplied
  IF ( display_current_channel ) THEN $
    !NULL = PLOT( $
      _channel, pcf.Yrange, $
      COLOR = 'green', $
      OVERPLOT = pcf )


  ; Display the absorber data
  FOR j = 0, n_absorbers-1 DO BEGIN
    ; ...Extract the data
    y = self->Get_Channel_Data('absorber_amount', Layer = layer, Absorber = j+1, Debug = debug)
    ; ...Process the difference list
    IF ( plot_difference ) THEN BEGIN
      ; Extract and difference the data
      y2 = diff_input->Get_Channel_Data('absorber_amount', Layer = layer, Absorber = j+1, Debug = debug)
      y  = y - y2
    ENDIF
    ; ...Create scaled y-data for pretty plotting
    axis_scale, [MIN(y),MAX(y)], STRTRIM(ABSORBER_UNITS_NAME[absorber_units[j]],2), yscale, ytitle
    y = y * yscale
    ; ...Plot the data
    index++
    absorber_name = STRTRIM(ABSORBER_ID_NAME[absorber_id[j]],2)
    pa = PLOT( $
      channel_number, y, $
      TITLE  = delta + absorber_name + ' spectrum at layer ' + STRTRIM(layer,2), $
      XTITLE = xtitle, $
      YTITLE = ytitle, $
      LAYOUT = [ n_col, n_row, index ], $
      COLOR = 'red', $
      SYMBOL = symbol, $
      CURRENT = owin )
    IF ( plot_difference ) THEN $
      !NULL = PLOT( $
        pa.Xrange, [0,0], $
        LINESTYLE = 'dash', $
        OVERPLOT = pa )
    IF ( display_current_channel ) THEN $
      !NULL = PLOT( $
        _channel, pa.Yrange, $
        COLOR = 'green', $
        OVERPLOT = pa )
  ENDFOR

END
