;
; $Id$
;

;--------------------------------------------
FUNCTION av_extract_atmosphere, $
  data, $
  profile_index, $
  channel_index = channel_index

  COMPILE_OPT HIDDEN
  
  IF ( N_ELEMENTS(channel_index) GT 0 ) THEN BEGIN
    atm = ((data['atmosphere'])[profile_index])[channel_index]
  ENDIF ELSE BEGIN
    atm = (data['atmosphere'])[profile_index]
  ENDELSE
  
  RETURN, atm
END


;--------------------------------------------
FUNCTION av_get_atmosphere, $
  state, $
  From_Diff_Input = from_diff_input

  COMPILE_OPT HIDDEN
  @atmosphere_viewer_parameters
  
  ; Set the data index to use
  index = KEYWORD_SET(from_diff_input) ? 1 : 0
  
  ; Get the current profile/channel indices 
  WIDGET_CONTROL, $
    state['av_slider_profile_id'], $
    GET_VALUE = profile_number
  profile_index = profile_number - 1
  WIDGET_CONTROL, $
    state['av_slider_channel_id'], $
    GET_VALUE = channel_number
  channel_index = channel_number - 1

  ; Extract the particular data item
  data = (state['data'])[index]

  ; Extract the atmosphere object
  CASE data['data_type'] OF
    DATA_TYPE_FORWARD : atm = av_extract_atmosphere(data, profile_index)
    DATA_TYPE_KMATRIX : atm = av_extract_atmosphere(data, profile_index, channel_index=channel_index)
    ELSE: MESSAGE, 'Invalid data type!'
  ENDCASE
  
  RETURN, atm
END


;--------------------------------------------
FUNCTION av_get_atmosphere_list, $
  state, $
  From_Diff_Input = from_diff_input

  COMPILE_OPT HIDDEN
  @atmosphere_viewer_parameters
  
  ; Set the data index to use
  index = KEYWORD_SET(from_diff_input) ? 1 : 0
  
  ; Get the current profile index 
  WIDGET_CONTROL, $
    state['av_slider_profile_id'], $
    GET_VALUE = profile_number
  profile_index = profile_number - 1

  ; Extract the particular data item
  data = (state['data'])[index]

  ; Extract the atmosphere list
  atm = av_extract_atmosphere(data, profile_index)
  
  RETURN, atm
END


;--------------------------------------------
PRO av_get_dimensions, $
  state, $
  From_Diff_Input = from_diff_input, $
  n_Layers   = n_layers, $
  n_Clouds   = n_clouds, $
  n_Aerosols = n_aerosols
  
  COMPILE_OPT HIDDEN

  ; Extract the atmosphere object for the current selection
  atm = av_get_atmosphere(state, From_Diff_Input = from_diff_input)
  
  ; Extract the dimensions
  atm -> Atmosphere::Get_Property, $
    n_Layers   = n_layers, $
    n_Clouds   = n_clouds, $
    n_Aerosols = n_aerosols
END


;-----syntactic-sugar-function---------------
FUNCTION av_n_layers, state, From_Diff_Input = from_diff_input
  COMPILE_OPT HIDDEN
  av_get_dimensions, $
    state, $
    From_Diff_Input = from_diff_input, $
    n_Layers = n_layers
  RETURN, n_layers
END


;-----syntactic-sugar-function---------------
FUNCTION av_n_clouds, state, From_Diff_Input = from_diff_input
  COMPILE_OPT HIDDEN
  av_get_dimensions, $
    state, $
    From_Diff_Input = from_diff_input, $
    n_Clouds = n_clouds
  RETURN, n_clouds
END


;-----syntactic-sugar-function---------------
FUNCTION av_n_aerosols, state, From_Diff_Input = from_diff_input
  COMPILE_OPT HIDDEN
  av_get_dimensions, $
    state, $
    From_Diff_Input = from_diff_input, $
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

  ; Get the plot type from event uvalue
  WIDGET_CONTROL, event.Id, GET_UVALUE = uvalue
  state['plot_type'] = uvalue

  ; Save state variable
  av_setstate, event.Top, state

  ; Load and display the data
  av_load_file, filename, event.Top

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL

END


;-------------------------------------------------
PRO av_menu_print_event, event
  COMPILE_OPT HIDDEN
  @atmosphere_viewer_parameters
  av_getstate, event.Top, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL
  
  ; Get the various index values
  WIDGET_CONTROL, $
    state['av_slider_profile_id'], $
    GET_VALUE = profile_number
  WIDGET_CONTROL, $
    state['av_slider_channel_id'], $
    GET_VALUE = channel_number
  WIDGET_CONTROL, $
    state['av_slider_layer_id'], $
    GET_VALUE = layer_number

  ; Determine the filename root
  data = (state['data'])[0]
  fileroot = FILE_BASENAME(data['filename'],'.bin') + $
             '.profile'+STRTRIM(profile_number,2)

  ; Set the plot type descriptor  
  plot_type = (state['plot_type'] EQ PLOT_TYPE_DIFFERENCE) ? '.difference' : ''
  
  ; Create the filename
  IF ( state['display_type'] EQ DISPLAY_TYPE_PROFILE ) THEN BEGIN
    filename = fileroot + $
               '.channel'+STRTRIM(channel_number,2) + plot_type+'.png'
  ENDIF ELSE BEGIN
    filename = fileroot + $
               '.layer'+STRTRIM(layer_number,2)+plot_type+'.png'
  ENDELSE
  
  ; Save the plot to file
  MESSAGE, 'Creating output file ' + filename, /INFORMATIONAL
  state['av_window_id'].Save, filename, HEIGHT=600
   
  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL
END


;-------------------------------------------------
PRO av_button_atmosphere_event, event
  COMPILE_OPT HIDDEN
  @atmosphere_viewer_parameters
  av_getstate, event.Top, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  state['component'] = COMPONENT_ATMOSPHERE
  
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
  @atmosphere_viewer_parameters
  av_getstate, event.Top, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  state['component'] = COMPONENT_CLOUD

  ; Only sensitise the cloud slider if more than a single cloud
  WIDGET_CONTROL, $
    state['av_slider_cloud_id'], $
    SENSITIVE = (av_n_clouds(state) GT 1)
  
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
  @atmosphere_viewer_parameters
  av_getstate, event.Top, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  state['component'] = COMPONENT_AEROSOL

  ; Only sensitise the aerosol slider if more than a single aerosol
  WIDGET_CONTROL, $
    state['av_slider_aerosol_id'], $
    SENSITIVE = (av_n_aerosols(state) GT 1)
      
  ; Always desensitise the cloud slider
  WIDGET_CONTROL, $
    state['av_slider_cloud_id'], $
    SENSITIVE = 0

  av_display, event.Top

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL
END


;-------------------------------------------------
PRO av_button_layer_event, event
  COMPILE_OPT HIDDEN
  @atmosphere_viewer_parameters
  av_getstate, event.Top, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  ; Sensitise the channel index slider
  WIDGET_CONTROL, $
    state['av_slider_channel_id'], $
    SENSITIVE = 1

  ; DEsensitise the layer index slider
  WIDGET_CONTROL, $
    state['av_slider_layer_id'], $
    SENSITIVE = 0

  ; Update the display type indicator
  state['display_type'] = DISPLAY_TYPE_PROFILE
 
  ; Save the updated state
  av_setstate, event.Top, state
  
  ; Display the layer (profile) data
  av_profile_display, event.Top
      
  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL
END


;-------------------------------------------------
PRO av_button_channel_event, event
  COMPILE_OPT HIDDEN
  @atmosphere_viewer_parameters
  av_getstate, event.Top, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  ; DEsensitise the channel index slider
  WIDGET_CONTROL, $
    state['av_slider_channel_id'], $
    SENSITIVE = 0

  ; Ssensitise the layer index slider
  WIDGET_CONTROL, $
    state['av_slider_layer_id'], $
    SENSITIVE = 1

  ; Update the display type indicator
  state['display_type'] = DISPLAY_TYPE_SPECTRUM
 
  ; Save the updated state
  av_setstate, event.Top, state
  
  ; Display the channel (spectrum) data
  av_spectrum_display, event.Top
      
  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL
END


;-------------------------------------------------
PRO av_button_tartan_event, event
  COMPILE_OPT HIDDEN
  @atmosphere_viewer_parameters
  av_getstate, event.Top, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  ; DEsensitise both the layer and channel index slider
  WIDGET_CONTROL, $
    state['av_slider_layer_id'], $
    SENSITIVE = 0
  WIDGET_CONTROL, $
    state['av_slider_channel_id'], $
    SENSITIVE = 0

  ; Update the display type indicator
  state['display_type'] = DISPLAY_TYPE_TARTAN
 
  ; Save the updated state
  av_setstate, event.Top, state
  
  ; Display the tartan
  av_tartan_display, event.Top
      
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

  av_profile_display, event.Top

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL
END


;-------------------------------------------------
PRO av_slider_layer_event, event
  COMPILE_OPT HIDDEN
  av_getstate, event.Top, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  av_spectrum_display, event.Top

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL
END


;-------------------------------------------------
PRO av_button_redraw_event, event
  COMPILE_OPT HIDDEN
  av_getstate, event.Top, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  av_display, event.Top

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL
END


;-------------------------------------------------
PRO av_display_info, id
  COMPILE_OPT HIDDEN
  @atmosphere_viewer_parameters
  av_getstate, id, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  ; Get the various indices
  WIDGET_CONTROL, $
    state['av_slider_profile_id'], $
    GET_VALUE = profile_number
  c_profile_number = STRTRIM(profile_number,2)

  n_layers   = av_n_layers(state)
  n_clouds   = av_n_clouds(state)
  n_aerosols = av_n_aerosols(state)
  
  ; Construct text info string
  data = (state['data'])[0]
  finfo = 'Filename : ' + STRTRIM(FILE_BASENAME(data['filename']),2)
  IF ( state['plot_type'] EQ PLOT_TYPE_DIFFERENCE ) THEN BEGIN
    diff_input = (state['data'])[1]
    finfo = $
      [ finfo, $
      'File #2  : ' + STRTRIM(FILE_BASENAME(diff_input['filename']),2) ]
  ENDIF
  info = [ $
    finfo, $
    'No. of profiles : ' + STRTRIM(data['n_profiles'],2), $
    'Current profile : ' + c_profile_number, $
    'No. of clouds for profile #' + c_profile_number + '   : ' + STRTRIM(n_clouds,2), $
    'No. of aerosols for profile #' + c_profile_number + ' : ' + STRTRIM(n_aerosols,2), $
    'No. of layers for profile #' + c_profile_number + '   : ' + STRTRIM(n_layers,2) ]
  ;...Add in channel info for K-matrix output
  IF ( data['data_type'] EQ DATA_TYPE_KMATRIX) THEN BEGIN
    WIDGET_CONTROL, $
      state['av_slider_channel_id'], $
      GET_VALUE = channel_number
    info = [ info, $
      [ 'No. of channels : ' + STRTRIM(data['n_channels'],2), $
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
PRO av_profile_display, id
  COMPILE_OPT HIDDEN
  @atmosphere_viewer_parameters
  av_getstate, id, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  ; Setup window for (re)display
  state['av_window_id'].Refresh, /DISABLE
  state['av_window_id'].Erase

  
  ; Get the atmosphere objects to display
  atm = av_get_atmosphere(state)
  IF ( state['plot_type'] EQ PLOT_TYPE_DIFFERENCE ) THEN $
    diff_input = av_get_atmosphere(state, /from_diff_input)
  

  ; Check the pressure
  ; ...Define a default pressure array
  default_pressure = CRTM_Default_Pressure()
  n_layers = av_n_layers(state)
  default_pressure = default_pressure[N_ELEMENTS(default_pressure)-n_layers:*]
  ; ...Extract the pressure profile
  atm -> Atmosphere::Get_Property, $
    Debug = state['debug'], $
    Pressure = pressure
  ; ...Check to see if they're valid
  loc = WHERE(pressure LT 1.0d-06, count)
  IF ( count GT 0 ) THEN pressure = default_pressure
  

  ; Plot the data
  CASE state['component'] OF

    COMPONENT_ATMOSPHERE: BEGIN
      atm.Plot, $
        Pressure = pressure, $
        Diff_Input = diff_input, $
        Owin = state['av_window_id'], $
        Debug = state['debug'], $
        /noclouds, /noaerosols
    END

    COMPONENT_CLOUD: BEGIN
      WIDGET_CONTROL, $
        state['av_slider_cloud_id'], $
        GET_VALUE = cloud_number
      cloud_index = cloud_number - 1
      atm.Get_Property, $
        Debug = state['debug'], $
        Cloud = cloud
      IF ( state['plot_type'] EQ PLOT_TYPE_DIFFERENCE ) THEN BEGIN
        diff_input.Get_Property, $
          Debug = state['debug'], $
          Cloud = diff_cloud
        diff_cloud = diff_cloud[cloud_index]
      ENDIF
      cloud[cloud_index].Plot, $
        Pressure = pressure, $
        Diff_Input = diff_cloud, $
        Owin = state['av_window_id'], $
        Debug = state['debug']
    END

    COMPONENT_AEROSOL: BEGIN
      WIDGET_CONTROL, $
        state['av_slider_aerosol_id'], $
        GET_VALUE = aerosol_number
      aerosol_index = aerosol_number - 1
      atm.Get_Property, $
        Debug = state['debug'], $
        Aerosol = aerosol
      IF ( state['plot_type'] EQ PLOT_TYPE_DIFFERENCE ) THEN BEGIN
        diff_input.Get_Property, $
          Debug = state['debug'], $
          Aerosol = diff_aerosol
        diff_aerosol = diff_aerosol[aerosol_index]
      ENDIF
      aerosol[aerosol_index].Plot, $
        Pressure = pressure, $
        Diff_Input = diff_aerosol, $
        Owin = state['av_window_id'], $
        Debug = state['debug']
    END
    ELSE: MESSAGE, 'Invalid component: ' + state['component']
  ENDCASE

  ; and display it
  state['av_window_id'].Refresh

  ; Save state variable
  av_setstate, id, state

  ; Update text info
  av_display_info, id


  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL

END


;-------------------------------------------------
PRO av_spectrum_display, id
  COMPILE_OPT HIDDEN
  @atmosphere_viewer_parameters
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

  ; Get the channel number
  WIDGET_CONTROL, $
    state['av_slider_channel_id'], $
    GET_VALUE = channel_number

  ; Get the layer number
  WIDGET_CONTROL, $
    state['av_slider_layer_id'], $
    GET_VALUE = layer_number


  ; Get the atmosphere lists to display
  atm = av_get_atmosphere_list(state)
  IF ( state['plot_type'] EQ PLOT_TYPE_DIFFERENCE ) THEN $
    diff_input = av_get_atmosphere_list(state, /from_diff_input)
  

  ; Plot the data
  CASE state['component'] OF

    COMPONENT_ATMOSPHERE: BEGIN
      atm.Channel_Plot, $
        layer_number, $
        Diff_Input = diff_input, $
        Channel = channel_number, $
        Owin = state['av_window_id'], $
        Debug = state['debug']
    END

    COMPONENT_CLOUD: BEGIN
      MESSAGE, '**** Not implemented yet ****', /INFORMATIONAL
    END

    COMPONENT_AEROSOL: BEGIN
      MESSAGE, '**** Not implemented yet ****', /INFORMATIONAL
    END

    ELSE: MESSAGE, 'Invalid component: ' + state['component']
  ENDCASE

  ; and display it
  state['av_window_id'].Refresh

  ; Save state variable
  av_setstate, id, state

  ; Update text info
  av_display_info, id

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL

END


;-------------------------------------------------
PRO av_tartan_display, id
  COMPILE_OPT HIDDEN
  @atmosphere_viewer_parameters
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

  ; Get the channel number
  WIDGET_CONTROL, $
    state['av_slider_channel_id'], $
    GET_VALUE = channel_number

  ; Get the layer number
  WIDGET_CONTROL, $
    state['av_slider_layer_id'], $
    GET_VALUE = layer_number


  ; Get the atmosphere lists to display
  atm = av_get_atmosphere_list(state)
  IF ( state['plot_type'] EQ PLOT_TYPE_DIFFERENCE ) THEN $
    diff_input = av_get_atmosphere_list(state, /from_diff_input)
  

  ; Plot the data
  CASE state['component'] OF

    COMPONENT_ATMOSPHERE: BEGIN
      atm.Tartan_Plot, $
        Diff_Input = diff_input, $
        Channel = channel_number, $
        Owin = state['av_window_id'], $
        Debug = state['debug']
    END

    COMPONENT_CLOUD: BEGIN
      MESSAGE, '**** Not implemented yet ****', /INFORMATIONAL
    END

    COMPONENT_AEROSOL: BEGIN
      MESSAGE, '**** Not implemented yet ****', /INFORMATIONAL
    END

    ELSE: MESSAGE, 'Invalid component: ' + state['component']
  ENDCASE

  ; and display it
  state['av_window_id'].Refresh

  ; Save state variable
  av_setstate, id, state

  ; Update text info
  av_display_info, id

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL

END


;-------------------------------------------------
PRO av_display, id
  COMPILE_OPT HIDDEN
  @atmosphere_viewer_parameters
  av_getstate, id, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  CASE state['display_type'] OF
    DISPLAY_TYPE_PROFILE : av_profile_display, id
    DISPLAY_TYPE_SPECTRUM: av_spectrum_display, id
    DISPLAY_TYPE_TARTAN  : av_tartan_display, id
  ENDCASE
  
  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL

END


;--------------------------------------------
PRO av_load_file, file, id
  COMPILE_OPT HIDDEN
  @atmosphere_viewer_parameters
  av_getstate, id, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  WIDGET_CONTROL, /HOURGLASS
  
  ; Read the file
  atmosphere = Atmosphere_List()
  atmosphere->Atmosphere_List::ReadFile, file, Quiet = ~ state['debug']


  ; Set the data index based on the plot type
  index = state['plot_type'] EQ PLOT_TYPE_DISPLAY ? 0 : 1


  ; Save the data
  data = (state['data'])[index]
  data['filename'] = file
  data['atmosphere'] = atmosphere
  ; ...Number of profiles
  data['n_profiles'] = atmosphere.Count()
  ; ...data type: forward or k-matrix model?
  IF ( ISA(atmosphere[0],'Atmosphere') ) THEN BEGIN
    data['data_type' ] = DATA_TYPE_FORWARD
    data['n_channels'] = 0
    ; ...ensure we can't plot spectra for forward data
    state['display_type'] = DISPLAY_TYPE_PROFILE
  ENDIF ELSE BEGIN
    data['data_type' ] = DATA_TYPE_KMATRIX
    data['n_channels'] = (atmosphere[0]).Count()
  ENDELSE


  ; Check the data if differencing
  IF ( state['plot_type'] EQ PLOT_TYPE_DIFFERENCE ) THEN BEGIN
    initial_data = (state['data'])[0]
    IF ( (initial_data['n_profiles'] NE data['n_profiles']) OR $
         (initial_data['n_channels'] NE data['n_channels']) OR $
         (initial_data['data_type']  NE data['data_type'] ) ) THEN BEGIN
      MESSAGE, 'Comparison data type and/or dimensions do not match initial input! Ignoring...', $
               /INFORMATIONAL   
      state['plot_type'] = PLOT_TYPE_DISPLAY
      (state['data'])[1].Remove, /ALL
      IF ( state['debug'] ) THEN MESSAGE, '...Exiting in warning state', /INFORMATIONAL
      RETURN
    ENDIF  
  ENDIF


  ; Set the default/initial plot type
  state['component'] = COMPONENT_ATMOSPHERE



  ; Sensitise the comparison menu
  WIDGET_CONTROL, $
    state['av_compare_menu_id'], $
    /SENSITIVE
  
  
  ; Set/sensitise the main dimension slider widgets
  ; ...Profile index
  WIDGET_CONTROL, $
    state['av_slider_profile_id'], $
    SET_VALUE      = 1, $
    SET_SLIDER_MIN = 1, $
    SET_SLIDER_MAX = data['n_profiles'], $
    SENSITIVE = (data['n_profiles'] GT 1)
  ; ...Channel index
  WIDGET_CONTROL, $
    state['av_slider_channel_id'], $
    SET_VALUE      = 1, $
    SET_SLIDER_MIN = 1, $
    SET_SLIDER_MAX = data['n_channels'], $
    SENSITIVE = ((data['n_channels'] GT 1) && $
                 (data['data_type'] EQ DATA_TYPE_KMATRIX) && $
                 (state['display_type'] EQ DISPLAY_TYPE_PROFILE))
    
    
  ; Set the main layer slider widget
  n_layers = av_n_layers(state)
  WIDGET_CONTROL, $
    state['av_slider_layer_id'], $
    SET_VALUE      = 1, $
    SET_SLIDER_MIN = 1, $
    SET_SLIDER_MAX = n_layers, $
    SENSITIVE = (state['display_type'] EQ DISPLAY_TYPE_SPECTRUM)
  

  ; Get some data info for the INITIAL (0'th) profile
  n_clouds   = av_n_clouds(state)
  n_aerosols = av_n_aerosols(state)
  
  
  ; Sensitise the plot type button widgets
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


  ; Sensitise the display type button widgets
  ; ...The base
  WIDGET_CONTROL, $
    state['av_displaytype_button_base_id'], $
    /SENSITIVE
  ; ...For forward data, set if a new file
  WIDGET_CONTROL, $
    state['av_button_layer_id'], $
    SET_BUTTON = ~index
  ; ...For K-matrix channel data, only for k-matrix
  WIDGET_CONTROL, $
    state['av_button_channel_id'], $
    SENSITIVE = (data['n_channels'] GT 1)
  WIDGET_CONTROL, $
    state['av_button_tartan_id'], $
    SENSITIVE = (data['n_channels'] GT 1)


  ; Set/Sensitise the remaining slider widgets
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
  
  ; Sensitise the redraw button
  WIDGET_CONTROL, $
    state['av_button_redraw_id'], $
    /SENSITIVE
  
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
  @atmosphere_viewer_parameters
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
      UVALUE    = PLOT_TYPE_DISPLAY )
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
  ; ...The compare menu
  av_compare_menu_id = WIDGET_BUTTON( $
    menu_bar_id, $
    VALUE     = 'Compare', $
    SENSITIVE = 0, $
    /MENU )
    ; ...Compare->Open
    compare_open_id = WIDGET_BUTTON( $
      av_compare_menu_id, $
      EVENT_PRO = 'av_menu_open_event', $
      VALUE     = 'Open', $
      UVALUE    = PLOT_TYPE_DIFFERENCE )


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
      ; ...Create the button base for plot type selection controls
      plottype_button_base_id = WIDGET_BASE( $
        left_upper_base_id, $
        GROUP_LEADER = top_level_base_id, $
        EXCLUSIVE    = 1, $
        FRAME        = 2, $
        MAP          = map, $
        COLUMN       = 1 )
        ; ...Create the atmosphere radio button
        av_button_atmosphere_id = WIDGET_BUTTON( $
          plottype_button_base_id, $
          GROUP_LEADER = top_level_base_id, $
          EVENT_PRO    = 'av_button_atmosphere_event', $
          NO_RELEASE   = 1, $
          SENSITIVE    = 0, $
          VALUE        = 'Atmosphere' )
        ; ...Create the cloud radio button
        av_button_cloud_id = WIDGET_BUTTON( $
          plottype_button_base_id, $
          GROUP_LEADER = top_level_base_id, $
          EVENT_PRO    = 'av_button_cloud_event', $
          NO_RELEASE   = 1, $
          SENSITIVE    = 0, $
          VALUE        = 'Cloud' )
        ; ...Create the cloud radio button
        av_button_aerosol_id = WIDGET_BUTTON( $
          plottype_button_base_id, $
          GROUP_LEADER = top_level_base_id, $
          EVENT_PRO    = 'av_button_aerosol_event', $
          NO_RELEASE   = 1, $
          SENSITIVE    = 0, $
          VALUE        = 'Aerosol' )
      ; ...Create the button base for display type selection controls
      av_displaytype_button_base_id = WIDGET_BASE( $
        left_upper_base_id, $
        GROUP_LEADER = top_level_base_id, $
        EXCLUSIVE    = 1, $
        FRAME        = 2, $
        MAP          = map, $
        SENSITIVE    = 0, $
        COLUMN       = 1 )
        ; ...Create the layer display radio button
        av_button_layer_id = WIDGET_BUTTON( $
          av_displaytype_button_base_id, $
          GROUP_LEADER = top_level_base_id, $
          EVENT_PRO    = 'av_button_layer_event', $
          NO_RELEASE   = 1, $
          VALUE        = 'Profile' )
        ; ...Create the channel radio button
        av_button_channel_id = WIDGET_BUTTON( $
          av_displaytype_button_base_id, $
          GROUP_LEADER = top_level_base_id, $
          EVENT_PRO    = 'av_button_channel_event', $
          NO_RELEASE   = 1, $
          VALUE        = 'Spectrum' )
        ; ...Create the tartan radio button
        av_button_tartan_id = WIDGET_BUTTON( $
          av_displaytype_button_base_id, $
          GROUP_LEADER = top_level_base_id, $
          EVENT_PRO    = 'av_button_tartan_event', $
          NO_RELEASE   = 1, $
          VALUE        = 'Tartan' )
      ; ...Create the slider base for data selection
      slider_base_id = WIDGET_BASE( $
        left_upper_base_id, $
        GROUP_LEADER = top_level_base_id, $
        FRAME        = 2, $
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
          TITLE        = 'Channel number', $
          UVALUE       = 'Channel' )
        ; ...Create the layer slider
        av_slider_layer_id = WIDGET_SLIDER( $
          slider_base_id, $
          GROUP_LEADER = top_level_base_id, $
          DRAG         = 0, $
          EVENT_PRO    = 'av_slider_layer_event', $
          MINIMUM      = 0, $
          MAXIMUM      = 1, $
          SENSITIVE    = 0, $
          TITLE        = 'Layer number', $
          UVALUE       = 'Layer' )
      ; ...Create the redraw button
      av_button_redraw_id = WIDGET_BUTTON( $
        left_upper_base_id, $
        GROUP_LEADER = top_level_base_id, $
        EVENT_PRO    = 'av_button_redraw_event', $
        SENSITIVE    = 0, $
        VALUE        = 'Redraw' )
    ; ...Create the RIGHT UPPER widget window for display
    draw_id = WIDGET_WINDOW( $
      upper_base_id, $
      GROUP_LEADER = top_level_base_id, $
      XSIZE        = 750, $
      YSIZE        = 600, $
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
      XSIZE = 142, $
      YSIZE = 9, $ ; Need to adjust this for more info output
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

  ; Set initial button behaviour
  WIDGET_CONTROL, $
    av_button_atmosphere_id, $
    /SET_BUTTON
  WIDGET_CONTROL, $
    av_button_layer_id, $
    /SET_BUTTON

  ; Create a state variable
  state = HASH()
  ; ...Boolean states
  state['debug'] = KEYWORD_SET(debug)
  ; ...Widget ids
  state['av_compare_menu_id'] = av_compare_menu_id
  state['av_button_atmosphere_id'] = av_button_atmosphere_id
  state['av_button_cloud_id'     ] = av_button_cloud_id
  state['av_button_aerosol_id'   ] = av_button_aerosol_id
  state['av_displaytype_button_base_id'] = av_displaytype_button_base_id
  state['av_button_layer_id'           ] = av_button_layer_id
  state['av_button_channel_id'         ] = av_button_channel_id
  state['av_button_tartan_id'          ] = av_button_tartan_id
  state['av_slider_profile_id'] = av_slider_profile_id
  state['av_slider_cloud_id'  ] = av_slider_cloud_id
  state['av_slider_aerosol_id'] = av_slider_aerosol_id
  state['av_slider_channel_id'] = av_slider_channel_id
  state['av_slider_layer_id'  ] = av_slider_layer_id
  state['av_button_redraw_id'] = av_button_redraw_id
  state['av_text_info_id'] = av_text_info_id
  state['av_window_id'] = av_window_id
  ; ...Default/initial display type
  state['display_type'] = DISPLAY_TYPE_PROFILE
  ; ...Default/initial plot type
  state['plot_type'] = PLOT_TYPE_DISPLAY
  ; ...Two element object array to hold data (for differencing)
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
    TARGET = av_window_id, $
    ALIGNMENT = 0.5, $
    VERTICAL_ALIGNMENT = 0.5 )


  ; Start the XManager
  XMANAGER, $
    'Atmosphere_Viewer', $
    top_level_base_id, $
    CLEANUP = 'av_cleanup', $
    GROUP_LEADER = top_level_base_id, $
    /NO_BLOCK

  IF ( KEYWORD_SET(debug) ) THEN MESSAGE, '...Exiting', /INFORMATIONAL
  
END
