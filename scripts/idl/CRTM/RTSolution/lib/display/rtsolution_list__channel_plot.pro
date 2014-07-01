PRO RTSolution_List::Channel_Plot, $
  Diff_Input = diff_input, $  ; Input keyword
  Title      = title     , $  ; Input keyword
  Owin       = owin      , $  ; Input keyword
  Png        = png       , $  ; Input keyword
  Debug      = debug          ; Input keyword
  
  ; Set up
  COMPILE_OPT HIDDEN
  @rtsolution_pro_err_handler
  @rtsolution_parameters
  ; ...Check that all list members are RTSolution objects
  IF ( ~ self->HasOnly_RTSolutions(Debug = debug) ) THEN $
    MESSAGE, 'Non-RTSolution object found in channel list.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ; ...Process boolean keywords
  buffer = KEYWORD_SET(png)
  ; ...Process other keywords
  _title = Valid_String(title) ? title : 'RTSolution Channel'
  create_window = KEYWORD_SET(owin) ? ~ ISA(owin,'GraphicsWin') : 1
  ; ...The graphic layout, col x row.
  n_col = 2
  n_row = CEIL(DOUBLE(N_CHANNEL_DATA_ITEMS)/n_col)


  ; Check difference input keyword separately (coz it's unwieldy)
  plot_difference = FALSE
  IF ( N_ELEMENTS(diff_input) GT 0 ) THEN BEGIN
    ; Must be an RTSolution list to start with
    IF ( ISA(diff_input,'RTSolution_List') ) THEN BEGIN
      ; Must be an RTSolution-object-only list
      IF ( diff_input->HasOnly_RTSolutions(Debug = debug) ) THEN BEGIN
        ; Must have the same number of channels as self
        IF ( self.Count() EQ diff_input.Count() ) THEN BEGIN
          ; We can use this!
          plot_difference = TRUE
        ENDIF
      ENDIF
    ENDIF
  ENDIF


  ; Create a window
  IF ( create_window ) THEN BEGIN
    owin = WINDOW( $
      WINDOW_TITLE = title, $
      BUFFER = buffer )
  ENDIF
  owin.SetCurrent
  ; ...Initialise layout index
  index = 0


  ; Get some sensor and channel information
  ; ...The sensor identifier
  self[0]->RTSolution::Get_Property, $
    Debug = debug, $
    Sensor_Id = sensor_id
  ; ...The channel numbers
  sensor_channel = self->Get_Channel_Data('Sensor_Channel', Debug = debug)


  ; Set channel-number-dependent plotting parameters
  IF ( N_ELEMENTS(sensor_channel) LT 50 ) THEN BEGIN
    symbol = 'diamond'
  ENDIF ELSE BEGIN
    symbol = 'none'
  ENDELSE
  
  
  ; Loop over the data items
  FOR i = 0, N_CHANNEL_DATA_ITEMS-1 DO BEGIN

    ; Extract the data
    y = self->Get_Channel_Data(CHANNEL_DATA_NAME[i], Debug = debug)
    ; ...and set the plot titles
    title  = STRTRIM(sensor_id,2) + ' ' + CHANNEL_DATA_NAME[i]
    
    ; Process the difference list
    IF ( plot_difference ) THEN BEGIN
      ; Extract and difference the data
      y2 = diff_input->Get_Channel_Data(CHANNEL_DATA_NAME[i], Debug = debug)
      y  = y - y2
      ; ...and modify the plot titles
      title  = STRTRIM(sensor_id,2) + ' ' + '$\Delta$' + CHANNEL_DATA_NAME[i]
    ENDIF
    
    
    ; Plot the data
    index++
    pdi = PLOT( $
      sensor_channel, y, $
      TITLE  = title, $
      XTITLE = 'Channel', $
      LAYOUT = [ n_col, n_row, index ], $
      COLOR = 'red', $
      SYMBOL = symbol, $
      MARGIN = [0.125,0.16,0.05,0.1], $
      FONT_SIZE = 9, $
      CURRENT = owin )
    IF ( plot_difference ) THEN $
      !NULL = PLOT( $
        pdi.Xrange, [0,0], $
        LINESTYLE = 'dash', $
        OVERPLOT = pdi )

  ENDFOR

END
