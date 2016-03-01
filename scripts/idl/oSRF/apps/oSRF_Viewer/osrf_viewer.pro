;-------------------------------------------------
PRO ov_freestate, id
  COMPILE_OPT HIDDEN
  WIDGET_CONTROL, id, GET_UVALUE = state
  OBJ_DESTROY, state
END


;-------------------------------------------------
PRO ov_getstate, id, state
  COMPILE_OPT HIDDEN
  WIDGET_CONTROL, id, GET_UVALUE = state
END


;-------------------------------------------------
PRO ov_setstate, id, state
  COMPILE_OPT HIDDEN
  WIDGET_CONTROL, id, SET_UVALUE = state
END


;-------------------------------------------------
PRO ov_cleanup, id
  COMPILE_OPT HIDDEN
  ov_getstate, id, state
  debug = state['debug']
  IF ( debug ) THEN MESSAGE, 'Entered...', /INFORMATIONAL
  ov_freestate, id
  IF ( debug ) THEN MESSAGE, '...Exiting', /INFORMATIONAL
END


;-------------------------------------------------
PRO ov_menu_exit_event, event
  COMPILE_OPT HIDDEN
  ov_getstate, event.Top, state
  debug = state['debug']
  IF ( debug ) THEN MESSAGE, 'Entered...', /INFORMATIONAL
  WIDGET_CONTROL, event.Top, /DESTROY
  IF ( debug ) THEN MESSAGE, '...Exiting', /INFORMATIONAL
END


;-------------------------------------------------
PRO ov_menu_open_event, event
  COMPILE_OPT HIDDEN
  ov_getstate, event.Top, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  ; Get the filename
  filename = DIALOG_PICKFILE( $
    TITLE = 'Select an oSRF file', $
    FILTER = '*.osrf.nc', $
    /MUST_EXIST )
  IF ( ~ Valid_String(filename) ) THEN RETURN

  ; Load and display the data
  ov_load_file, filename, event.Top

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL

END


;-------------------------------------------------
PRO ov_menu_saveas_png_event, event
  COMPILE_OPT HIDDEN
  ov_getstate, event.Top, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL
  
  ; Make sure there's a filename to work with
  IF ( state.HasKey('filename') ) THEN BEGIN
  
    ; Get the channel number
    WIDGET_CONTROL, $
      state['ov_slider_id'], $
      GET_VALUE = channel_number

    ; Create the filename
    filename = FILE_BASENAME(state['filename'],'.osrf.nc') + $
               '.channel'+STRTRIM(channel_number,2) + '.png'
  
  
    ; Save the plot to file
    MESSAGE, 'Creating output file ' + filename, /INFORMATIONAL
    state['ov_window_id'].Save, filename, HEIGHT=600, BORDER=10
    
  ENDIF
  
  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL
END


;-------------------------------------------------
PRO ov_menu_saveas_eps_event, event
  COMPILE_OPT HIDDEN
  ov_getstate, event.Top, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL
  
  ; Make sure there's a filename to work with
  IF ( state.HasKey('filename') ) THEN BEGIN
  
    ; Get the channel number
    WIDGET_CONTROL, $
      state['ov_slider_id'], $
      GET_VALUE = channel_number

    ; Create the filename
    filename = FILE_BASENAME(state['filename'],'.osrf.nc') + $
               '.channel'+STRTRIM(channel_number,2) + '.eps'

    ; Get the current oSRF
    osrf = state['osrf_file'].Get( Debug    = state['debug'], $
                                   Position = state['channel_position'] )

    ; Increase the plot font sizes for EPS output
    osrf.Get_Property, $
      Debug   = state['debug'], $
      n_Bands = n_bands
    font_size = HASH()
    FOR band = 1, n_bands DO BEGIN
      ; Get the band plot reference
      osrf.Get_Property, $
        band, $
        Debug   = state['debug'], $
        pRef    = pref
      ; Save current font size
      font_size[band] = pref.font_size
      ; Increase it
      pref.font_size = pref.font_size * 2.0
    ENDFOR
  
    ; Save the plot to file
    MESSAGE, 'Creating output file ' + filename, /INFORMATIONAL
    state['ov_window_id'].Save, filename
   
    ; Restore the original plot font sizes
    FOR band = 1, n_bands DO BEGIN
      ; Get the band plot reference
      osrf.Get_Property, $
        band, $
        Debug   = state['debug'], $
        pRef    = pref
      ; Restore font size
      pref.font_size = font_size[band]
    ENDFOR
    
  ENDIF
  
  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL
END


;-------------------------------------------------
PRO ov_menu_ylogtoggle_event, event
  COMPILE_OPT HIDDEN
  ov_getstate, event.Top, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  ; Toggle the y-axis log view  
  state['ylog'] = ~state['ylog']

  ; Save the state variable
  ov_setstate, event.Top, state

  ; Display the new data
  ov_display, event.Top

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL

END


;-------------------------------------------------
PRO ov_slider_event, event
  COMPILE_OPT HIDDEN
  ov_getstate, event.Top, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL
  
  ov_display, event.Top

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL

END


;-------------------------------------------------
PRO ov_display_info, id
  COMPILE_OPT HIDDEN
  @osrf_parameters
  ov_getstate, id, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  ; Get the current oSRF
  osrf = state['osrf_file'].Get( Debug    = state['debug'], $
                                 Position = state['channel_position'] )
  ; Get the current oSRF info
  osrf.Get_Property, $
    Debug                = state['debug'], $
    Sensor_Id            = sensor_id, $
    Sensor_Type          = sensor_type, $
    Flags                = flags, $
    Integral             = integral, $
    f0                   = f0, $
    Planck_Coeffs        = planck_coeffs, $
    Polychromatic_Coeffs = polychromatic_coeffs
  ; ...Set the units string
  IF ( Sensor_Type EQ MICROWAVE_SENSOR ) THEN BEGIN
    f0 = inverse_cm_to_GHz(f0)
    frequency_units = 'GHz'
  ENDIF ELSE $
    frequency_units = 'cm^-1'

  ; Create the info string
  info = [ $
    'Sensor id                  : ' + sensor_id, $
    'Sensor type                : ' + SENSOR_TYPE_NAME[sensor_type], $
    'Channel                    : ' + STRTRIM(state['channel'],2), $
    'Bit flags                  : ' + STRING(flags,FORMAT='(b32.32)'), $
    'Integrated SRF             : ' + STRING(integral,FORMAT='(e13.6)'), $
    'Central frequency          : ' + STRTRIM(STRING(f0,FORMAT='(1x,f12.6)'),2) + frequency_units, $
    'Planck coefficients        :' + STRING(planck_coeffs,FORMAT='(2(1x,e16.10))'), $
    'Polychromatic coefficients :' + STRING(polychromatic_coeffs,FORMAT='(2(1x,f16.10))') ]

  
  ; Set the text
  WIDGET_CONTROL, $
    state['ov_text_info_id'], $
    SET_VALUE = info

  
  ; Display it
  ibid = WIDGET_INFO(state['ov_text_info_id'], /PARENT)
  WIDGET_CONTROL, ibid, /MAP

 
  ; Save state variable
  ov_setstate, id, state


  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL

END


;-------------------------------------------------
PRO ov_display, id
  COMPILE_OPT HIDDEN
  ov_getstate, id, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  ; Setup window for (re)display
  state['ov_window_id'].Refresh, /DISABLE
  state['ov_window_id'].Erase

  ; Get the channel index
  WIDGET_CONTROL, $
    state['ov_slider_id'], $
    GET_VALUE = channel_index
  channel_position = channel_index - 1

  ; Get the current oSRF
  osrf = state['osrf_file'].Get( Debug    = state['debug'], $
                                 Position = channel_position )
  ; ...Get some current oSRF info
  osrf.Get_Property, $
    Debug   = state['debug'], $
    Channel = channel
  state['channel'] = channel
  state['channel_position'] = channel_position

  ; Plot the data
  osrf.Plot, $
    Debug = state['debug'], $
    Owin  = state['ov_window_id'], $
    Ylog  = state['ylog']

  ; and display it
  state['ov_window_id'].Refresh

  ; Save state variable
  ov_setstate, id, state

  ; Update text info
  ov_display_info, id

END


;--------------------------------------------
PRO ov_load_file, file, id
  COMPILE_OPT HIDDEN
  ov_getstate, id, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  WIDGET_CONTROL, /HOURGLASS


  ; Read the file
  osrf_file = OBJ_NEW('osrf_file', file, Debug = state['debug'])
  osrf_file.Read, Debug = state['debug']
  osrf_file.Get_Property, $
    Debug = state['debug'], $
    n_Channels = n_channels
  
  ; Save the data
  state['osrf_file']  = osrf_file
  state['filename']   = file
  state['n_channels'] = n_channels
  
  ; Sensitise the Save-As and View menus
  WIDGET_CONTROL, $
    state['ov_file_saveas_menu_id'], $
    /SENSITIVE
  WIDGET_CONTROL, $
    state['ov_view_menu_id'], $
    /SENSITIVE

  ; Set and sensitise the channel slider widget
  state['channel'] = 1
  WIDGET_CONTROL, $
    state['ov_slider_id'], $
    SET_VALUE      = state['channel'], $
    SET_SLIDER_MIN = 1, $
    SET_SLIDER_MAX = state['n_channels'], $
    SENSITIVE = (state['n_channels'] GT 1)

  ; Save the state variable
  ov_setstate, id, state

  ; Display the new data
  ov_display, id

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL

END



;--------------------------------------------
PRO OSRF_Viewer, $
  File         = file, $
  State        = state, $
  Regular_Size = regular_size, $
  Debug        = debug

  ; Setup
  ; ...Check arguments
  IF ( KEYWORD_SET(debug) ) THEN MESSAGE, 'Entered...', /INFORMATIONAL
  ; ...Parameters
  MAXFILES = 11L
  ; ...Widget-y defaults
  nomap = 0
  map   = 1

  ; Create the top level base
  top_level_base_id = WIDGET_BASE( $
    COLUMN =1, $
    MAP    = map, $
    MBAR   = menu_bar_id, $
    TITLE  = 'oSRF_Viewer' )
  WIDGET_CONTROL, $
    top_level_base_id, $
    UPDATE = 0


  ; Create the menu bar contents
  ; ...The file menu
  file_menu_id = WIDGET_BUTTON( $
    menu_bar_id, $
    VALUE = 'File', $
    /MENU )
    ; ...File->Open
    file_open_id = WIDGET_BUTTON( $
      file_menu_id, $
      EVENT_PRO = 'ov_menu_open_event', $
      VALUE     = 'Open', $
      UVALUE    = 'FileOpen' )
    ; ...File->Save-As
    ov_file_saveas_menu_id = WIDGET_BUTTON( $
      file_menu_id, $
      VALUE     = 'Save As', $
      SENSITIVE = 0, $
      /MENU )
      ; ...File->Save-As->PNG
      file_saveas_png_id = WIDGET_BUTTON( ov_file_saveas_menu_id, $
        EVENT_PRO = 'ov_menu_saveas_png_event', $
        VALUE     = 'PNG', $
        UVALUE    = 'SaveAsPNG' )
      ; ...File->Save-As->EPS
      file_saveas_eps_id = WIDGET_BUTTON( ov_file_saveas_menu_id, $
        EVENT_PRO = 'ov_menu_saveas_eps_event', $
        VALUE     = 'EPS', $
        UVALUE    = 'SaveAsEPS' )
    ; ...File->Exit
    file_exit_id = WIDGET_BUTTON( file_menu_id, $
      EVENT_PRO = 'ov_menu_exit_event', $
      VALUE     = 'Exit', $
      UVALUE    = 'FileExit', $
      /SEPARATOR )
  ; ...The view menu
  ov_view_menu_id = WIDGET_BUTTON( $
    menu_bar_id, $
    VALUE     = 'View', $
    SENSITIVE = 0, $
    /MENU )
    ; ...View->YLogToggleOpen
    view_ylogtoggle_id = WIDGET_BUTTON( $
      ov_view_menu_id, $
      EVENT_PRO = 'ov_menu_ylogtoggle_event', $
      VALUE     = 'Yaxis Log toggle', $
      UVALUE    = 'ViewYlogToggle' )


  ; Create the right base for drawing
  draw_base_id = WIDGET_BASE( $
    top_level_base_id, $
    GROUP_LEADER =top_level_base_id, $
    ROW = 1, $
    MAP = Map )
    ; ...Create the widget window
    IF ( KEYWORD_SET(regular_size) ) THEN BEGIN
      xsize = 640
      ysize = 512
    ENDIF ELSE BEGIN
      xsize = 800
      ysize = 600
    ENDELSE
    draw_id = WIDGET_WINDOW( $
      draw_base_id, $
      GROUP_LEADER = top_level_base_id, $
      XSIZE        = xsize, $
      YSIZE        = ysize, $
      FRAME        = 2    , $
      SENSITIVE    = 0      )


  ; Create the base for controls
  control_base_id = WIDGET_BASE( $
    top_level_base_id, $
    GROUP_LEADER = top_level_base_id, $
    FRAME        = 2  , $
    MAP          = Map, $
    COLUMN       = 1    )
    ; ...Create the channel slider
    slider_id = WIDGET_SLIDER( control_base_id, $
      GROUP_LEADER = top_level_base_id, $
      DRAG         = 0, $
      EVENT_PRO    = 'ov_slider_event', $
      MINIMUM      = 0, $
      MAXIMUM      = 1, $
      SENSITIVE    = 0, $
      TITLE        = 'oSRF count', $
      UVALUE       = 'Position', $
      XSIZE        = xsize-5 )

  ; Create the base for info output
  info_base_id = WIDGET_BASE( $
    top_level_base_id, $
    GROUP_LEADER = top_level_base_id, $
    FRAME        = 2, $
    MAP          = Map, $
    COLUMN       = 1 )
    ; ...Create the text widget for info
    ov_text_info_id = WIDGET_TEXT( $
      info_base_id, $
      XSIZE = 130, $
      YSIZE = 8, $ ; Need to adjust this for more info output
      GROUP_LEADER = top_level_base_id )



  ; Map, update and realise the widget heirarchy
  WIDGET_CONTROL, $
    top_level_base_id, $
    MAP     = map, $
    /REALIZE, $
    /UPDATE


  ; Obtain the widget window ID
  WIDGET_CONTROL, $
    draw_id, $
    GET_VALUE = window_id


  ; Create a state variable
  state = HASH()
  ; ...Boolean states
  state['debug'] = KEYWORD_SET(debug)
  state['ylog']  = 0
  ; ...Widget ids
  state['ov_file_saveas_menu_id'] = ov_file_saveas_menu_id
  state['ov_view_menu_id'] = ov_view_menu_id
  state['ov_view_menu_id'] = ov_view_menu_id
  state['ov_slider_id'] = slider_id
  state['ov_window_id'] = window_id
  state['ov_text_info_id'] = ov_text_info_id
  ; ...and save it
  ov_setstate, top_level_base_id, state


  ; Load file or display instructions
  IF ( Valid_String(file) ) THEN BEGIN
    ov_load_file, file, top_level_base_id
  ENDIF ELSE BEGIN
    text = TEXT( $
      0.5, 0.5, $
      'Use File$\rightarrow$Open to select a file', $
      /NORMAL, $
      TARGET = av_window_id, $
      ALIGNMENT = 0.5, $
      VERTICAL_ALIGNMENT = 0.5 )
  ENDELSE


    ; Start the XManager
  XMANAGER, $
    'oSRF_Viewer', $
    top_level_base_id, $
    CLEANUP = 'ov_cleanup', $
    GROUP_LEADER = top_level_base_id, $
    /NO_BLOCK

  IF ( KEYWORD_SET(debug) ) THEN MESSAGE, '...Exiting', /INFORMATIONAL

END ; OSRF_Viewer
