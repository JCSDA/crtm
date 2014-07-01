;
; $Id$
;

;--------------------------------------------
PRO rv_get_dimensions, $
  data, $
  profile_index, $
  n_Layers = n_layers
  
  COMPILE_OPT HIDDEN

  ; Extract an object
  rts = ((data['rtsolution'])[profile_index])[0]
  
  ; Extract the dimensions
  rts -> RTSolution::Get_Property, $
    n_Layers = n_layers
END


;-------------------------------------------------
PRO rv_freestate, id
  COMPILE_OPT HIDDEN
  WIDGET_CONTROL, id, GET_UVALUE = state
  OBJ_DESTROY, state
END


;-------------------------------------------------
PRO rv_getstate, id, state
  COMPILE_OPT HIDDEN
  WIDGET_CONTROL, id, GET_UVALUE = state
END


;-------------------------------------------------
PRO rv_setstate, id, state
  COMPILE_OPT HIDDEN
  WIDGET_CONTROL, id, SET_UVALUE = state
END


;-------------------------------------------------
PRO rv_cleanup, id
  COMPILE_OPT HIDDEN
  rv_getstate, id, state
  debug = state['debug']
  IF ( debug ) THEN MESSAGE, 'Entered...', /INFORMATIONAL
  rv_freestate, ID
  IF ( debug ) THEN MESSAGE, '...Exiting', /INFORMATIONAL
END


;-------------------------------------------------
PRO rv_menu_exit_event, event
  COMPILE_OPT HIDDEN
  rv_getstate, event.Top, state
  debug = state['debug']
  IF ( debug ) THEN MESSAGE, 'Entered...', /INFORMATIONAL
  WIDGET_CONTROL, event.Top, /DESTROY
  IF ( debug ) THEN MESSAGE, '...Exiting', /INFORMATIONAL
END


;-------------------------------------------------
PRO rv_menu_open_event, event
  COMPILE_OPT HIDDEN
  rv_getstate, event.Top, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  ; Get the filename
  filename = DIALOG_PICKFILE( $
    TITLE = 'Select an RTSolution file', $
    FILTER = '*RTSolution*', $
    /MUST_EXIST )
  IF ( ~ Valid_String(filename) ) THEN RETURN

  ; Get open "type" from event uvalue
  WIDGET_CONTROL, event.Id, GET_UVALUE = uvalue
  state['plot_type'] = uvalue

  ; Save state variable
  rv_setstate, event.Top, state

  ; Load and display the data
  rv_load_file, filename, event.Top

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL

END


;-------------------------------------------------
PRO rv_menu_print_event, event
  COMPILE_OPT HIDDEN
  rv_getstate, event.Top, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  filename = FILE_BASENAME(((state['data'])[0])['filename']+'.png')
  state['rv_window_id'].Save, filename, HEIGHT=1000
  MESSAGE, 'Created output file '+filename, /INFORMATIONAL
  
  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL
END


;-------------------------------------------------
PRO rv_slider_profile_event, event
  COMPILE_OPT HIDDEN
  rv_getstate, event.Top, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  rv_display, event.Top

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL
END


;-------------------------------------------------
PRO rv_display_info, id
  COMPILE_OPT HIDDEN
  @rtsolution_viewer_parameters
  rv_getstate, id, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  ; Get the data
  data = state['data']
  
  ; Get the various indices
  WIDGET_CONTROL, $
    state['rv_slider_profile_id'], $
    GET_VALUE = profile_number
  profile_index = profile_number - 1
  rv_get_dimensions, data[0], profile_index, n_layers = n_layers
  
  ; Construct text info string
  c_profile_number = STRTRIM(profile_number,2)
  finfo = 'Filename : ' + STRTRIM(FILE_BASENAME((data[0])['filename']),2)
  IF ( state['plot_type'] EQ DIFFERENCE ) THEN BEGIN
    finfo = $
      [ finfo, $
      'File #2  : ' + STRTRIM(FILE_BASENAME((data[1])['filename']),2) ]
  ENDIF
  info = [ $
    finfo, $
    'No. of profiles : ' + STRTRIM((data[0])['n_profiles'],2), $
    'Current profile : ' + c_profile_number, $
    'No. of layers for profile #' + c_profile_number + ' : ' + STRTRIM(n_layers,2), $
    'No. of channels : ' + STRTRIM((data[0])['n_channels'],2) ]

  
  ; Set the text
  WIDGET_CONTROL, $
    state['rv_text_info_id'], $
    SET_VALUE = info
  
  ; Display it
  pbid = WIDGET_INFO(state['rv_text_info_id'], /PARENT)
  WIDGET_CONTROL, pbid, /MAP
  
  ; Save state variable
  rv_setstate, id, state

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL

END


;-------------------------------------------------
PRO rv_display, id
  COMPILE_OPT HIDDEN
  @rtsolution_viewer_parameters
  rv_getstate, id, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  ; Setup window for (re)display
  state['rv_window_id'].Refresh, /DISABLE
  state['rv_window_id'].Erase
  
  ; Get the profile index
  WIDGET_CONTROL, $
    state['rv_slider_profile_id'], $
    GET_VALUE = profile_number
  profile_index = profile_number - 1
  
  ; Select the required rtsolution
  IF ( state['plot_type'] EQ DISPLAY ) THEN BEGIN
    data = (state['data'])[0]
    rts = (data['rtsolution'])[profile_index]
  ENDIF ELSE BEGIN
    data0 = (state['data'])[0]
    data1 = (state['data'])[1]
    rts = (data0['rtsolution'])[profile_index]
    diff_input = (data1['rtsolution'])[profile_index]
  ENDELSE

  ; Plot the data
  rts.Channel_Plot, $
    Diff_Input = diff_input, $
    Owin = state['rv_window_id'], $
    Debug = state['debug']

  ; and display it
  state['rv_window_id'].Refresh

  ; Update text info
  rv_display_info, id


  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL

END


;--------------------------------------------
PRO rv_load_file, file, id
  COMPILE_OPT HIDDEN
  @error_codes
  @rtsolution_viewer_parameters
  rv_getstate, id, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  WIDGET_CONTROL, /HOURGLASS
  
  ; Read the file
  MESSAGE, 'Loading RTSolution data file '+file+' for '+state['plot_type'], /INFORMATIONAL
  rtsolution = RTSolution_List()
  rtsolution->RTSolution_List::ReadFile, file, Quiet = ~ state['debug']


  ; Save the data
  index = state['plot_type'] EQ DISPLAY ? 0 : 1
  data = (state['data'])[index]
  data['filename'] = file
  data['rtsolution'] = rtsolution
  data['n_profiles'] = rtsolution.Count()
  data['n_channels'] = (rtsolution[0]).Count()
  (rtsolution[0])[0] -> Get_Property, $
    Debug = debug, $
    Sensor_Id = sensor_id
  data['sensor_id'] = sensor_id


  ; Check the data if differencing
  IF ( state['plot_type'] EQ DIFFERENCE ) THEN BEGIN
    initial_data = (state['data'])[0]
    IF ( (initial_data['n_profiles'] NE data['n_profiles']) OR $
         (initial_data['n_channels'] NE data['n_channels']) OR $
         (initial_data['sensor_id']  NE data['sensor_id'] ) ) THEN BEGIN
      MESSAGE, 'Comparison data sensor id and/or dimensions do not match initial input! Ignoring...', $
               /INFORMATIONAL   
      state['plot_type'] = DISPLAY
    ENDIF  
  ENDIF

  ; Set/sensitise the main dimension slider widget
  WIDGET_CONTROL, $
    state['rv_slider_profile_id'], $
    SET_VALUE      = 1, $
    SET_SLIDER_MIN = 1, $
    SET_SLIDER_MAX = data['n_profiles'], $
    SENSITIVE = (data['n_profiles'] GT 1)
  
  
  ; Sensitise the comparison menu
  WIDGET_CONTROL, $
    state['rv_compare_menu_id'], $
    /SENSITIVE
  
  
  ; Save the state variable
  rv_setstate, id, state

  ; Display the new data
  rv_display, id

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL

END


;--------------------------------------------
PRO RTSolution_Viewer, $
  Debug = debug

  ; Setup
  @rtsolution_viewer_parameters
  ; ...Check arguments
  IF ( KEYWORD_SET(debug) ) THEN MESSAGE, 'Entered...', /INFORMATIONAL
  ; ...Parameters
  MAXFILES = 11L
  ; ...Widget-y defaults
  nomap = 0
  map   = 1


  ; Create the top level base
  top_level_base_id = WIDGET_BASE( $
    MAP    = map, $
    MBAR   = menu_bar_id, $
    COLUMN = 1, $
    TITLE  = 'RTSolution_Viewer' )
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
      EVENT_PRO = 'rv_menu_open_event', $
      UVALUE    = DISPLAY, $
      VALUE     = 'Open' )
    ; ...File->Print
    file_print_id = WIDGET_BUTTON( $
      file_menu_id, $
      EVENT_PRO = 'rv_menu_print_event', $
      VALUE     = 'Print', $
      UVALUE    = 'FilePrint' )
    ; ...File->Exit
    file_exit_id = WIDGET_BUTTON( $
      file_menu_id, $
      EVENT_PRO = 'rv_menu_exit_event', $
      VALUE     = 'Exit', $
      UVALUE    = 'FileExit', $
      /SEPARATOR )
  ; ...The compare menu
  rv_compare_menu_id = WIDGET_BUTTON( $
    menu_bar_id, $
    VALUE     = 'Compare', $
    SENSITIVE = 0, $
    /MENU )
    ; ...Compare->Open
    compare_open_id = WIDGET_BUTTON( $
      rv_compare_menu_id, $
      EVENT_PRO = 'rv_menu_open_event', $
      UVALUE    = DIFFERENCE, $
      VALUE     = 'Open' )


  ; Create the UPPER base for controls and plots
  upper_base_id = WIDGET_BASE( $
    top_level_base_id, $
    GROUP_LEADER = top_level_base_id, $
    FRAME        = 2, $
    MAP          = map, $
    ROW          = 1 )
    ; ...Create the LEFT UPPER base for controls
    left_upper_base_id = WIDGET_BASE( $
      upper_base_id, $
      GROUP_LEADER = top_level_base_id, $
      MAP          = map, $
      COLUMN       = 1 )
      ; ...Create the slider base for data selection
      slider_base_id = WIDGET_BASE( $
        left_upper_base_id, $
        GROUP_LEADER = top_level_base_id, $
        MAP          = map, $
        COLUMN       = 1 )
        ; ...Create the profile slider
        rv_slider_profile_id = WIDGET_SLIDER( $
          slider_base_id, $
          GROUP_LEADER = top_level_base_id, $
          DRAG         = 0, $
          EVENT_PRO    = 'rv_slider_profile_event', $
          MINIMUM      = 0, $
          MAXIMUM      = 1, $
          SENSITIVE    = 0, $
          TITLE        = 'Profile #', $
          UVALUE       = 'profile' )
    ; ...Create the RIGHT UPPER widget window for display
    draw_id = WIDGET_WINDOW( $
      upper_base_id, $
      XSIZE = 700, $
      YSIZE = 800, $
      GROUP_LEADER = top_level_base_id, $
      FRAME        = 2, $
      SENSITIVE    = 0 )

      
  ; Create the LOWER base for info output
  lower_base_id = WIDGET_BASE( $
    top_level_base_id, $
    GROUP_LEADER = top_level_base_id, $
    FRAME        = 2, $
    MAP          = 0, $
    ROW          = 1 )
    ; ...Create the text widget for info
    rv_text_info_id = WIDGET_TEXT( $
      lower_base_id, $
      XSIZE = 133, $
      YSIZE = 6, $ ; Need to adjust this for more info output
      GROUP_LEADER = top_level_base_id )


  ; map, update and realise the widget heirarchy
  WIDGET_CONTROL, $
    top_level_base_id, $
    MAP     = map, $
    /REALIZE, $
    /UPDATE


  ; Obtain the widget window ID
  WIDGET_CONTROL, $
    draw_id, $
    GET_VALUE = rv_window_id


  ; Create a state variable
  state = HASH()
  ; ...Boolean states
  state['debug'] = KEYWORD_SET(debug)
  ; ...Widget ids
  state['rv_compare_menu_id'] = rv_compare_menu_id
  state['rv_slider_profile_id'] = rv_slider_profile_id
  state['rv_text_info_id'] = rv_text_info_id
  state['rv_window_id'] = rv_window_id
  ; ...Object array to hold data
  state['index'] = 0
  data  = OBJARR(2)
  FOR i = 0, 1 DO data[i] = HASH()
  state['data'] = data

  
  ; ...and save it
  WIDGET_CONTROL, $
    top_level_base_id, $
    SET_UVALUE = state


  ; Display instructions
  text = TEXT( $
    0.5, 0.5, $
    'Use File$\rightarrow$Open to select a file', $
    /NORMAL, $
    TARGET = rv_window_id, $
    ALIGNMENT = 0.5, $
    VERTICAL_ALIGNMENT = 0.5 )


  ; Start the XManager
  XMANAGER, $
    'RTSolution_Viewer', $
    top_level_base_id, $
    CLEANUP = 'rv_cleanup', $
    GROUP_LEADER = top_level_base_id, $
    /NO_BLOCK

  IF ( KEYWORD_SET(debug) ) THEN MESSAGE, '...Exiting', /INFORMATIONAL

END
