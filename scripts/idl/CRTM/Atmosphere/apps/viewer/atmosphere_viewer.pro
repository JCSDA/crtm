;
; $Id$
;

;--------------------------------------------
PRO av_get_dimensions, $
  state, $
  profile_index, $
  n_Layers   = n_layers, $
  n_Clouds   = n_clouds, $
  n_Aerosols = n_aerosols
  
  COMPILE_OPT HIDDEN

  ; Extract the atmosphere object
  CASE state['data_type'] OF
    'forward' : atm = (state['atmosphere'])[profile_index]
    'k_matrix': BEGIN
      WIDGET_CONTROL, $
        state['av_slider_channel_id'], $
        GET_VALUE = channel_number
      channel_index = channel_number - 1
      atm = ((state['atmosphere'])[profile_index])[channel_index]
    END
    ELSE: MESSAGE, 'Invalid data type!'
  ENDCASE
  
  ; Extract the dimensions
  atm -> Atmosphere::Get_Property, $
    n_Layers   = n_layers, $
    n_Clouds   = n_clouds, $
    n_Aerosols = n_aerosols
END


;-----syntactic-sugar-function---------------
FUNCTION av_n_layers, state, profile_index
  COMPILE_OPT HIDDEN
  av_get_dimensions, $
    state, $
    profile_index, $
    n_Layers = n_layers
  RETURN, n_layers
END


;-----syntactic-sugar-function---------------
FUNCTION av_n_clouds, state, profile_index
  COMPILE_OPT HIDDEN
  av_get_dimensions, $
    state, $
    profile_index, $
    n_Clouds = n_clouds
  RETURN, n_clouds
END


;-----syntactic-sugar-function---------------
FUNCTION av_n_aerosols, state, profile_index
  COMPILE_OPT HIDDEN
  av_get_dimensions, $
    state, $
    profile_index, $
    n_Aerosols = n_aerosols
  RETURN, n_aerosols
END


;-------------------------------------------------
PRO av_freestate, id
  COMPILE_OPT HIDDEN
  WIDGET_CONTROL, id, GET_UVALUE = state
  OBJ_DESTROY, state
END


;-------------------------------------------------
PRO av_getstate, id, state
  COMPILE_OPT HIDDEN
  WIDGET_CONTROL, id, GET_UVALUE = state
END


;-------------------------------------------------
PRO av_setstate, id, state
  COMPILE_OPT HIDDEN
  WIDGET_CONTROL, id, SET_UVALUE = state
END


;-------------------------------------------------
PRO av_cleanup, id
  COMPILE_OPT HIDDEN
  av_getstate, id, state
  debug = state['debug']
  IF ( debug ) THEN MESSAGE, 'Entered...', /INFORMATIONAL
  av_freestate, ID
  IF ( debug ) THEN MESSAGE, '...Exiting', /INFORMATIONAL
END


;-------------------------------------------------
PRO av_menu_exit_event, event
  COMPILE_OPT HIDDEN
  av_getstate, event.Top, state
  debug = state['debug']
  IF ( debug ) THEN MESSAGE, 'Entered...', /INFORMATIONAL
  WIDGET_CONTROL, event.Top, /DESTROY
  IF ( debug ) THEN MESSAGE, '...Exiting', /INFORMATIONAL
END


;-------------------------------------------------
PRO av_menu_open_event, event
  COMPILE_OPT HIDDEN
  av_getstate, event.Top, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  ; Get the filename
  filename = DIALOG_PICKFILE( $
    TITLE = 'Select an Atmosphere file', $
    FILTER = '*Atmosphere*', $
    /MUST_EXIST )
  IF ( ~ Valid_String(filename) ) THEN RETURN

  ; Load and display the data
  av_load_file, filename, event.Top

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL

END


;-------------------------------------------------
PRO av_menu_print_event, event
  COMPILE_OPT HIDDEN
  av_getstate, event.Top, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

print, '*** File->Print not implemented ***'

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL
END


;-------------------------------------------------
PRO av_button_atmosphere_event, event
  COMPILE_OPT HIDDEN
  av_getstate, event.Top, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  state['plot_type'] = 'atmosphere'
  
  ; Always desensitise the cloud and aerosol sliders
  WIDGET_CONTROL, $
    state['av_slider_cloud_id'], $
    SENSITIVE = 0
  WIDGET_CONTROL, $
    state['av_slider_aerosol_id'], $
    SENSITIVE = 0

  av_display, event.Top

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL
END


;-------------------------------------------------
PRO av_button_cloud_event, event
  COMPILE_OPT HIDDEN
  av_getstate, event.Top, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  state['plot_type'] = 'cloud'

  ; Get the profile index
  WIDGET_CONTROL, $
    state['av_slider_profile_id'], $
    GET_VALUE = profile_index

  ; Only sensitise the cloud slider if more than a single cloud
  WIDGET_CONTROL, $
    state['av_slider_cloud_id'], $
    SENSITIVE = (av_n_clouds(state,profile_index) GT 1)
  
  ; Always desensitise the aerosol slider
  WIDGET_CONTROL, $
    state['av_slider_aerosol_id'], $
    SENSITIVE = 0

  av_display, event.Top

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL
END


;-------------------------------------------------
PRO av_button_aerosol_event, event
  COMPILE_OPT HIDDEN
  av_getstate, event.Top, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  state['plot_type'] = 'aerosol'

  ; Get the profile index
  WIDGET_CONTROL, $
    state['av_slider_profile_id'], $
    GET_VALUE = profile_index

  ; Only sensitise the aerosol slider if more than a single aerosol
  WIDGET_CONTROL, $
    state['av_slider_aerosol_id'], $
    SENSITIVE = (av_n_aerosols(state,profile_index) GT 1)
      
  ; Always desensitise the cloud slider
  WIDGET_CONTROL, $
    state['av_slider_cloud_id'], $
    SENSITIVE = 0

  av_display, event.Top

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL
END


;-------------------------------------------------
PRO av_slider_profile_event, event
  COMPILE_OPT HIDDEN
  av_getstate, event.Top, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  av_display, event.Top

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL
END


;-------------------------------------------------
PRO av_slider_cloud_event, event
  COMPILE_OPT HIDDEN
  av_getstate, event.Top, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  av_display, event.Top

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL
END


;-------------------------------------------------
PRO av_slider_aerosol_event, event
  COMPILE_OPT HIDDEN
  av_getstate, event.Top, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  av_display, event.Top

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL
END


;-------------------------------------------------
PRO av_slider_channel_event, event
  COMPILE_OPT HIDDEN
  av_getstate, event.Top, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  av_display, event.Top

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL
END


;-------------------------------------------------
PRO av_display_info, id
  COMPILE_OPT HIDDEN
  av_getstate, id, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  ; Get the various indices
  WIDGET_CONTROL, $
    state['av_slider_profile_id'], $
    GET_VALUE = profile_number
  profile_index = profile_number - 1
  n_layers   = av_n_layers(state,profile_index)
  n_clouds   = av_n_clouds(state,profile_index)
  n_aerosols = av_n_aerosols(state,profile_index)
  
  ; Construct text info string
  c_profile_number = STRTRIM(profile_number,2)
  info = [ $
  'Filename : ' + STRTRIM(FILE_BASENAME(state['filename']),2), $
  'No. of profiles : ' + STRTRIM(state['n_profiles'],2), $
  'Current profile : ' + c_profile_number, $
  'No. of layers for profile #' + c_profile_number + '   : ' + STRTRIM(n_layers,2), $
  'No. of clouds for profile #' + c_profile_number + '   : ' + STRTRIM(n_clouds,2), $
  'No. of aerosols for profile #' + c_profile_number + ' : ' + STRTRIM(n_aerosols,2) ]
  ;...Add in channel ifo for K-matrix output
  IF ( state['data_type'] EQ 'k_matrix' ) THEN BEGIN
    WIDGET_CONTROL, $
      state['av_slider_channel_id'], $
      GET_VALUE = channel_number
    info = [ info, $
      [ 'No. of channels : ' + STRTRIM(state['n_channels'],2), $
        'Current channel : ' + STRTRIM(channel_number,2) ] ]
  ENDIF
  
  ; Set the text
  WIDGET_CONTROL, $
    state['av_text_info_id'], $
    SET_VALUE = info
  
  ; Display it
  pbid = WIDGET_INFO(state['av_text_info_id'], /PARENT)
  WIDGET_CONTROL, pbid, /MAP
  
  ; Save state variable
  av_setstate, id, state

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL

END


;-------------------------------------------------
PRO av_display, id
  COMPILE_OPT HIDDEN
  av_getstate, id, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  ; Setup window for (re)display
  state['av_window_id'].Refresh, /DISABLE
  state['av_window_id'].Erase
  
  ; Get the profile index
  WIDGET_CONTROL, $
    state['av_slider_profile_id'], $
    GET_VALUE = profile_number
  profile_index = profile_number - 1

  
  ; Select the required atmosphere
  CASE state['data_type'] OF
    'forward' : BEGIN
      atm = (state['atmosphere'])[profile_index]
    END
    'k_matrix': BEGIN
      ; Get the channel index
      WIDGET_CONTROL, $
        state['av_slider_channel_id'], $
        GET_VALUE = channel_number
      channel_index = channel_number - 1
      atm = ((state['atmosphere'])[profile_index])[channel_index]
    END
    ELSE: MESSAGE, 'Invalid data type!'
  ENDCASE


  ; Check the pressure
  ; ...Define a default pressure array
  default_pressure = CRTM_Default_Pressure()
  n_layers = av_n_layers(state,profile_index)
  default_pressure = default_pressure[N_ELEMENTS(default_pressure)-n_layers:*]
  ; ...Extract the pressure profile
  atm -> Atmosphere::Get_Property, $
    Debug = state['debug'], $
    Pressure = pressure
  ; ...Check to see if they're valid
  loc = WHERE(pressure LT 1.0d-06, count)
  IF ( count GT 0 ) THEN pressure = default_pressure
  

  ; Plot the data
  CASE state['plot_type'] OF

    'atmosphere': BEGIN
      atm.Plot, $
        Pressure = pressure, $
        Owin = state['av_window_id'], $
        Debug = state['debug'], $
        /noclouds, /noaerosols
    END

    'cloud': BEGIN
      atm.Get_Property, $
        Debug = state['debug'], $
        Cloud = cloud
      WIDGET_CONTROL, $
        state['av_slider_cloud_id'], $
        GET_VALUE = cloud_number
      cloud_index = cloud_number - 1
      cloud[cloud_index].Plot, $
        Pressure = pressure, $
        Owin = state['av_window_id'], $
        Debug = state['debug']
    END

    'aerosol': BEGIN
      atm.Get_Property, $
        Debug = state['debug'], $
        Aerosol = aerosol
      WIDGET_CONTROL, $
        state['av_slider_aerosol_id'], $
        GET_VALUE = aerosol_number
      aerosol_index = aerosol_number - 1
      aerosol[aerosol_index].Plot, $
        Pressure = pressure, $
        Owin = state['av_window_id'], $
        Debug = state['debug']
    END
    ELSE: MESSAGE, 'Invalid plot_type: ' + state['plot_type']
  ENDCASE

  ; and display it
  state['av_window_id'].Refresh

  ; Save state variable
  av_setstate, id, state

  ; Update text info
  av_display_info, id


  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL

END


;--------------------------------------------
PRO av_load_file, file, id
  COMPILE_OPT HIDDEN
  @error_codes
  av_getstate, id, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  WIDGET_CONTROL, /HOURGLASS
  
  ; Read the file
  atmosphere_readfile, file, atmosphere, Quiet = ~ state['debug']
  state['filename'] = file


  ; Save the data in the state structure
  state['atmosphere'] = atmosphere
  

  ; Set some data info
  ; ...Default/initial plot type
  state['plot_type'] = 'atmosphere'
  ; ...Number of profiles
  state['n_profiles'] = atmosphere.Count()
  ; ...data type: forward or k-matrix model?
  IF ( ISA(atmosphere[0],'Atmosphere') ) THEN BEGIN
    state['data_type' ] = 'forward'
    state['n_channels'] = 0
  ENDIF ELSE BEGIN
    state['data_type' ] = 'k_matrix'
    state['n_channels'] = (atmosphere[0]).Count()
  ENDELSE


  ; Get some data info for the INITIAL (0'th) profile
  n_clouds   = av_n_clouds(state,0)
  n_aerosols = av_n_aerosols(state,0)
  
  
  ; Sensitise the button widgets
  WIDGET_CONTROL, $
    state['av_button_atmosphere_id'], $
    /SENSITIVE, $
    /SET_BUTTON
  ; ...For clouds, only if there are some
  WIDGET_CONTROL, $
    state['av_button_cloud_id'], $
    SENSITIVE = (n_clouds GT 0)
  ; ...For aerosols, only if there are some
  WIDGET_CONTROL, $
    state['av_button_aerosol_id'], $
    SENSITIVE = (n_aerosols GT 0)


  ; Sensitise the slider widgets
  WIDGET_CONTROL, $
    state['av_slider_profile_id'], $
    SET_VALUE      = 1, $
    SET_SLIDER_MIN = 1, $
    SET_SLIDER_MAX = state['n_profiles'], $
    SENSITIVE = (state['n_profiles'] GT 1)
  ; ...For clouds, only if there are > 1 for INITIAL profile
  WIDGET_CONTROL, $
    state['av_slider_cloud_id'], $
    SET_VALUE      = 1, $
    SET_SLIDER_MIN = 1, $
    SET_SLIDER_MAX = n_clouds, $
    SENSITIVE = (n_clouds GT 1)
  ; ...For aerosols, only if there are > 1 for INITIAL profile
  WIDGET_CONTROL, $
    state['av_slider_aerosol_id'], $
    SET_VALUE      = 1, $
    SET_SLIDER_MIN = 1, $
    SET_SLIDER_MAX = n_aerosols, $
    SENSITIVE = (n_aerosols GT 1)
  ; ...(De)Sensitise the channel slider appropriately
  WIDGET_CONTROL, $
    state['av_slider_channel_id'], $
    SET_VALUE      = 1, $
    SET_SLIDER_MIN = 1, $
    SET_SLIDER_MAX = state['n_channels'], $
    SENSITIVE = (state['n_channels'] GT 1)
  
  
  ; Save the state variable
  av_setstate, id, state

  ; Display the new data
  av_display, id

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL

END



;--------------------------------------------
PRO Atmosphere_Viewer, $
  Debug = debug

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
    MAP    = map, $
    MBAR   = menu_bar_id, $
    COLUMN = 1, $
    TITLE  = 'Atmosphere_Viewer' )
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
      EVENT_PRO = 'av_menu_open_event', $
      VALUE     = 'Open', $
      UVALUE    = 'FileOpen' )
    ; ...File->Print
    file_print_id = WIDGET_BUTTON( $
      file_menu_id, $
      EVENT_PRO = 'av_menu_print_event', $
      VALUE     = 'Print', $
      UVALUE    = 'FilePrint' )
    ; ...File->Exit
    file_exit_id = WIDGET_BUTTON( $
      file_menu_id, $
      EVENT_PRO = 'av_menu_exit_event', $
      VALUE     = 'Exit', $
      UVALUE    = 'FileExit', $
      /SEPARATOR )
  ; ...The conmpare menu
  Compare_Menu_ID = WIDGET_BUTTON( $
    menu_bar_id, $
    VALUE     = 'Compare', $
    SENSITIVE = 0, $
    /MENU )
    ; ...Compare->Open
    Compare_Open_ID = WIDGET_BUTTON( $
      Compare_Menu_ID, $
      EVENT_PRO = 'av_menu_open_event', $
      VALUE     = 'Open', $
      UVALUE    = 'CompareOpen' )


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
;      FRAME        = 2, $
      MAP          = map, $
      COLUMN       = 1 )
      ; ...Create the button base for exclusive button selection controls
      button_base_id = WIDGET_BASE( $
        left_upper_base_id, $
        GROUP_LEADER = top_level_base_id, $
        EXCLUSIVE    = 1, $
        MAP          = map, $
        COLUMN       = 1 )
        ; ...Create the atmosphere radio button
        av_button_atmosphere_id = WIDGET_BUTTON( $
          button_base_id, $
          GROUP_LEADER = top_level_base_id, $
          EVENT_PRO    = 'av_button_atmosphere_event', $
          NO_RELEASE   = 1, $
          SENSITIVE    = 0, $
          VALUE        = 'Atmosphere' )
        ; ...Create the cloud radio button
        av_button_cloud_id = WIDGET_BUTTON( $
          button_base_id, $
          GROUP_LEADER = top_level_base_id, $
          EVENT_PRO    = 'av_button_cloud_event', $
          NO_RELEASE   = 1, $
          SENSITIVE    = 0, $
          VALUE        = 'Cloud' )
        ; ...Create the cloud radio button
        av_button_aerosol_id = WIDGET_BUTTON( $
          button_base_id, $
          GROUP_LEADER = top_level_base_id, $
          EVENT_PRO    = 'av_button_aerosol_event', $
          NO_RELEASE   = 1, $
          SENSITIVE    = 0, $
          VALUE        = 'Aerosol' )
      ; ...Create the slider base for data selection
      slider_base_id = WIDGET_BASE( $
        left_upper_base_id, $
        GROUP_LEADER = top_level_base_id, $
        MAP          = map, $
        COLUMN       = 1 )
        ; ...Create the profile slider
        av_slider_profile_id = WIDGET_SLIDER( $
          slider_base_id, $
          GROUP_LEADER = top_level_base_id, $
          DRAG         = 0, $
          EVENT_PRO    = 'av_slider_profile_event', $
          MINIMUM      = 0, $
          MAXIMUM      = 1, $
          SENSITIVE    = 0, $
          TITLE        = 'Profile #', $
          UVALUE       = 'Profile' )
        ; ...Create the cloud slider
        av_slider_cloud_id = WIDGET_SLIDER( $
          slider_base_id, $
          GROUP_LEADER = top_level_base_id, $
          DRAG         = 0, $
          EVENT_PRO    = 'av_slider_cloud_event', $
          MINIMUM      = 0, $
          MAXIMUM      = 1, $
          SENSITIVE    = 0, $
          TITLE        = 'Cloud #', $
          UVALUE       = 'Cloud' )
        ; ...Create the aerosol slider
        av_slider_aerosol_id = WIDGET_SLIDER( $
          slider_base_id, $
          GROUP_LEADER = top_level_base_id, $
          DRAG         = 0, $
          EVENT_PRO    = 'av_slider_aerosol_event', $
          MINIMUM      = 0, $
          MAXIMUM      = 1, $
          SENSITIVE    = 0, $
          TITLE        = 'Aerosol #', $
          UVALUE       = 'Aerosol' )
        ; ...Create the channel slider
        av_slider_channel_id = WIDGET_SLIDER( $
          slider_base_id, $
          GROUP_LEADER = top_level_base_id, $
          DRAG         = 0, $
          EVENT_PRO    = 'av_slider_channel_event', $
          MINIMUM      = 0, $
          MAXIMUM      = 1, $
          SENSITIVE    = 0, $
          TITLE        = 'Channel index', $
          UVALUE       = 'Channel' )
    ; ...Create the RIGHT UPPER widget window for display
    draw_id = WIDGET_WINDOW( $
      upper_base_id, $
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
    av_text_info_id = WIDGET_TEXT( $
      lower_base_id, $
      XSIZE = 123, $
      YSIZE = 8, $ ; Need to adjust this for more info output
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
    GET_VALUE = av_window_id


  ; Create a state variable
  state = HASH()
  ; ...Boolean states
  state['debug'] = debug
  ; ...Widget ids
  state['av_button_atmosphere_id'] = av_button_atmosphere_id
  state['av_button_cloud_id'     ] = av_button_cloud_id
  state['av_button_aerosol_id'   ] = av_button_aerosol_id
  state['av_slider_profile_id'] = av_slider_profile_id
  state['av_slider_cloud_id'  ] = av_slider_cloud_id
  state['av_slider_aerosol_id'] = av_slider_aerosol_id
  state['av_slider_channel_id'] = av_slider_channel_id
  state['av_text_info_id'] = av_text_info_id
  state['av_window_id'] = av_window_id
  ; ...and save it
  WIDGET_CONTROL, $
    top_level_base_id, $
    SET_UVALUE = state


  ; Display instructions
  text = TEXT( $
    0.5, 0.5, $
    'Use File$\rightarrow$Open to select a file', $
    /NORMAL, $
    TARGET = av_window_id, $
    ALIGNMENT = 0.5, $
    VERTICAL_ALIGNMENT = 0.5 )


  ; Start the XManager
  XMANAGER, $
    'Atmosphere_Viewer', $
    top_level_base_id, $
    CLEANUP = 'av_cleanup', $
    GROUP_LEADER = top_level_base_id

  IF ( KEYWORD_SET(debug) ) THEN MESSAGE, '...Exiting', /INFORMATIONAL
  
END
