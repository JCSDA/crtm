;-----
PRO MWwaterCoeff_Viewer_Cleanup, ID
  COMPILE_OPT HIDDEN
  GetState, ID, Info
  IF ( Info.Debug ) THEN PRINT, 'MWwaterCoeff_Viewer_Cleanup'
  FreeState, ID
END

;-----
PRO MWwaterCoeff_Viewer_Exit_Event, Event
  COMPILE_OPT HIDDEN
  GetState, Event.Top, Info
  IF ( Info.Debug ) THEN PRINT, 'MWwaterCoeff_Viewer_Exit_Event'
  WIDGET_CONTROL, Event.Top, /DESTROY
END

;-----
PRO MWwaterCoeff_Viewer_Open_Event, Event
  COMPILE_OPT HIDDEN
  GetState, Event.Top, Info
  IF ( Info.Debug ) THEN PRINT, 'MWwaterCoeff_Viewer_Open_Event'
  Filename = DIALOG_PICKFILE( TITLE = 'Select a MWwaterCoeff file', $
                              FILTER = '*.bin', $
                              /MUST_EXIST )
  IF ( NOT Valid_String( Filename ) ) THEN RETURN
  MWwaterCoeff_Viewer_Load_File, Filename, Event.Top
END

;-----
PRO MWwaterCoeff_Viewer_Print_Event, Event
  COMPILE_OPT HIDDEN
  GetState, Event.Top, Info
  IF ( Info.Debug ) THEN PRINT, 'MWwaterCoeff_Viewer_Print_Event'
  pson, FILENAME='mwwatercoeff_display.ps'
  DEVICE, SCALE_FACTOR=1.5
  MWwaterCoeff_Viewer_Display, Event.Top, $
    FONT=1, $
    CHARSIZE=3.5
  psoff
END

;-----
PRO MWwaterCoeff_Viewer_Display, ID, $
                                 FONT=Font, $
                                 CHARSIZE=Charsize
  COMPILE_OPT HIDDEN
  GetState, ID, Info
  IF ( Info.Debug ) THEN PRINT, 'MWwaterCoeff_Viewer_Display'


  ; Assign the data selectors
  Angle       = (Info.Active)['Angle']
  Frequency   = (Info.Active)['Frequency']
  Temperature = (Info.Active)['Temperature']
  Wind_Speed  = (Info.Active)['Wind_Speed']
  
  ; Assign the data indices
  WIDGET_CONTROL, (Info.Slider_ID)['Angle']      , GET_VALUE = iAngle
  WIDGET_CONTROL, (Info.Slider_ID)['Frequency']  , GET_VALUE = iFrequency  
  WIDGET_CONTROL, (Info.Slider_ID)['Temperature'], GET_VALUE = iTemperature
  WIDGET_CONTROL, (Info.Slider_ID)['Wind_Speed'] , GET_VALUE = iWind_Speed 

  SetState, ID, Info
  
  Info.MWwaterCoeff->Plot, $
    Debug=Info.Debug, $
    Angle       = Angle      , $
    Frequency   = Frequency  , $
    Temperature = Temperature, $
    Wind_Speed  = Wind_Speed , $
    iAngle       = iAngle       - 1, $
    iFrequency   = iFrequency   - 1, $
    iTemperature = iTemperature - 1, $
    iWind_Speed  = iWind_Speed  - 1, $
    FONT=Font, $
    CHARSIZE=Charsize
END

;-----
PRO MWwaterCoeff_Viewer_Load_File, File, ID
  COMPILE_OPT HIDDEN
  GetState, ID, Info
  IF ( Info.Debug ) THEN PRINT, 'MWwaterCoeff_Viewer_Load_File'

  WIDGET_CONTROL, /HOURGLASS

  Info.Full_Filename = File
  Info.Filename = FILE_BASENAME(File)
  Info.MWwaterCoeff = MWwaterCoeff(Debug=Info.Debug)
  Info.MWwaterCoeff->ReadFile, File, Debug=Info.Debug

  ; Get data information
  Info.MWwaterCoeff->Get_Property, $
    n_Angles       = n_angles, $
    n_Frequencies  = n_frequencies, $
    n_temperatures = n_temperatures, $
    n_Wind_Speeds  = n_wind_speeds
  dim_value = HASH( 'Angle', n_angles, $
                    'Frequency', n_frequencies, $
                    'Temperature', n_temperatures, $
                    'Wind_Speed', n_wind_speeds )
  
  ; Sensitise the plot type
  WIDGET_CONTROL, $
    Info.Plottype_Base_Id, $
    SENSITIVE = 1
  
  ; Set slider data
  FOR i = 0, N_ELEMENTS(Info.Control_Value)-1 DO BEGIN
    WIDGET_CONTROL, $
      (Info.Slider_ID)[Info.Control_Value[i]], $
      SET_SLIDER_MIN = 1, $
      SET_SLIDER_MAX = dim_value[Info.Control_Value[i]], $
      SET_VALUE = 1, $
      SENSITIVE = 0
    WIDGET_CONTROL, $
      (Info.Button_ID)[Info.Control_Value[i]], $
      SENSITIVE = 1
  ENDFOR
  
  ; Make redisplay sensitive
  WIDGET_CONTROL, $
    Info.Redisplay_ID, $
    SENSITIVE = 1
  
  ; Display some more info
  ERASE
  XYOUTS, 0.5, 0.5, $
          'Select plot type and dimensions to display and press "Redisplay"', $
          /NORM, $
          ALIGNMENT = 0.5, $
          CHARSIZE = 2.0
                  
  ; Save top level base info state
  SetState, ID, Info
END

;-----
PRO MWwaterCoeff_Viewer_Plottype_Event, event
  COMPILE_OPT HIDDEN
  GetState, event.top, Info
  IF ( Info.Debug ) THEN PRINT, 'MWwaterCoeff_Viewer_Plottype_Event'

  WIDGET_CONTROL, event.id, GET_UVALUE = uvalue
  CASE uvalue OF
    '1D': Info.Plottype = 1
    '2D': Info.Plottype = 2
  ENDCASE    

  SetState, Event.Top, Info
END

;-----
PRO MWwaterCoeff_Viewer_Button_Event, event
  COMPILE_OPT HIDDEN
  GetState, event.top, Info
  IF ( Info.Debug ) THEN PRINT, 'MWwaterCoeff_Viewer_Button_Event'

  ; Desensitise slider base
  WIDGET_CONTROL, Info.Slider_Base_Id, SENSITIVE=0
  
  ; Get button widget information
  WIDGET_CONTROL, event.id, GET_UVALUE = uvalue
  is_set = WIDGET_INFO( event.id, /BUTTON_SET )
  
  ; Update active count
  IF (is_set) THEN BEGIN
    Info.n_Active++
    (Info.Active)[uvalue] = 1
  ENDIF ELSE BEGIN
    Info.n_Active--
    (Info.Active)[uvalue] = 0
  ENDELSE
  
  ; Sensitise corresponding slider for selected button accordingly
  WIDGET_CONTROL, (Info.Slider_Id)[uvalue], SENSITIVE = ~(Info.Active)[uvalue]

  SetState, Event.Top, Info
END

;-----
PRO MWwaterCoeff_Viewer_Slider_Event, event
  COMPILE_OPT HIDDEN
  GetState, event.top, Info
  IF ( Info.Debug ) THEN PRINT, 'MWwaterCoeff_Viewer_Slider_Event'

  MWwaterCoeff_Viewer_Display, Event.Top
END

;-----
PRO MWwaterCoeff_Viewer_Redisplay_Event, event
  COMPILE_OPT HIDDEN
  GetState, event.top, Info
  IF ( Info.Debug ) THEN PRINT, 'MWwaterCoeff_Viewer_Redisplay_Event'

  ; Ensure the required number of dimensions are active
  IF ( Info.n_Active NE Info.Plottype ) THEN BEGIN
    MESSAGE, 'Number of dimensions does not agree with plot type!', /INFORMATIONAL
    RETURN
  ENDIF
  
  ; Ensure all the sliders are correctly sensitised
  FOREACH control, Info.Control_Value DO $
    WIDGET_CONTROL, (Info.Slider_Id)[control], SENSITIVE = ~(Info.Active)[control]
  
  ; Sensitise the slider base
  WIDGET_CONTROL, Info.Slider_Base_Id, SENSITIVE=1

  SetState, Event.Top, Info
  MWwaterCoeff_Viewer_Display, Event.Top
END

;-----
PRO MWwaterCoeff_Viewer, Debug = debug

  ; Check arguments
  IF ( NOT KEYWORD_SET(debug) ) THEN BEGIN
    debug = 0 
  ENDIF ELSE BEGIN
    debug = 1
    PRINT, 'MWwaterCoeff_Viewer'
  ENDELSE


  ; Create the widget display
  nomap = 0
  map   = 1
  ; ...Top level base
  top_level_base_id = WIDGET_BASE( ROW=1, $
                                   MAP = map, $
                                   MBAR = menu_bar_id, $
                                   TITLE = 'MWwaterCoeff_Viewer' )
  WIDGET_CONTROL, top_level_base_id, UPDATE = 0
  ; ....Menu bar
  file_menu_id  = WIDGET_BUTTON( menu_bar_id, $
                                 VALUE = 'File', $
                                 /MENU )
  file_open_id  = WIDGET_BUTTON( file_menu_id , $
                                 VALUE = 'Open', $
                                 EVENT_PRO = 'MWwaterCoeff_Viewer_Open_Event', $
                                 UVALUE = 'Open' )
  file_print_id = WIDGET_BUTTON( file_menu_id , $
                                 VALUE = 'Print', $
                                 EVENT_PRO = 'MWwaterCoeff_Viewer_Print_Event', $
                                 UVALUE = 'Print' )
  file_exit_id  = WIDGET_BUTTON( file_menu_id , $
                                 VALUE = 'Exit', $
                                 EVENT_PRO = 'MWwaterCoeff_Viewer_Exit_Event', $
                                 /SEPARATOR, $
                                 UVALUE = 'Exit' )
  ; ...The left base for controls
  control_base_id = WIDGET_BASE( top_level_base_id, $
                                 GROUP_LEADER = top_level_base_id, $
                                 FRAME = 2, $
                                 MAP = map, $
                                 COLUMN = 1 )
  control_value = ['Angle','Frequency','Temperature','Wind_Speed']
  n_controls = N_ELEMENTS(control_value)
  plottype_value = ['2D','1D']
  n_plottypes = N_ELEMENTS(plottype_value)
  ; ...Create the plot type buttons
  plottype_base_id = WIDGET_BASE( control_base_id, $
                                  GROUP_LEADER = top_level_base_id, $
                                  MAP = map, $
                                  COLUMN = 1, $
                                  SENSITIVE = 0, $
                                  /EXCLUSIVE )
  plottype_button_id = HASH()
  FOR i = 0, n_plottypes-1 DO BEGIN
    plottype_button_id[plottype_value[i]] = $
      WIDGET_BUTTON( plottype_base_id, $
                     GROUP_LEADER = top_level_base_id, $
                     EVENT_PRO = 'MWwaterCoeff_Viewer_Plottype_Event', $
                     VALUE = plottype_value[i], $
                     UVALUE = plottype_value[i] )
  ENDFOR
  ; ...Create the dimension buttons
  button_base_id = WIDGET_BASE( control_base_id, $
                                GROUP_LEADER = top_level_base_id, $
                                MAP = map, $
                                COLUMN = 1, $
                                /NONEXCLUSIVE )
  button_id = HASH()
  active = HASH()
  FOR i = 0, n_controls-1 DO BEGIN
    active[control_value[i]] = 0
    button_id[control_value[i]] = WIDGET_BUTTON( button_base_id, $
                                                 GROUP_LEADER = top_level_base_id, $
                                                 EVENT_PRO = 'MWwaterCoeff_Viewer_Button_Event', $
                                                 SENSITIVE = 0, $
                                                 VALUE = control_value[i], $
                                                 UVALUE = control_value[i] )
  ENDFOR
  ; ...The redisplay button
  redisplay_id = WIDGET_BUTTON( control_base_id, $
                                GROUP_LEADER = top_level_base_id, $
                                EVENT_PRO = 'MWwaterCoeff_Viewer_Redisplay_Event', $
                                SENSITIVE = 0, $
                                VALUE = 'Redisplay' )
  ; ...Create the dimension sliders
  slider_base_id = WIDGET_BASE( control_base_id, $
                                GROUP_LEADER = top_level_base_id, $
                                MAP = map, $
                                COLUMN = 1, $
                                SENSITIVE = 0 )
  slider_id = HASH()
  FOR i = 0, n_controls-1 DO BEGIN
    slider_id[control_value[i]] = WIDGET_SLIDER( slider_base_id, $
                                                 GROUP_LEADER = top_level_base_id, $
                                                 DRAG = 1, $
                                                 EVENT_PRO = 'MWwaterCoeff_Viewer_Slider_Event', $
                                                 MINIMUM = 0, $
                                                 MAXIMUM = 1, $
                                                 SENSITIVE = 1, $
                                                 TITLE = control_value[i], $
                                                 UVALUE = control_value[i], $
                                                 XSIZE = 200 )
  ENDFOR
  ; ...The right base for drawing
  draw_base_id = WIDGET_BASE( top_level_base_id, $
                              GROUP_LEADER = top_level_base_id, $
                              ROW = 1, $
                              MAP = map )
  ; ...The draw widget
  xsize = 800
  ysize = 600
  draw_widget_id = WIDGET_DRAW( draw_base_id, $
                                GROUP_LEADER = top_level_base_id, $
                                XSIZE = xsize, $
                                YSIZE = ysize )
  ; ...Map, update and realise the widget heirarchy
  WIDGET_CONTROL, top_level_base_id, MAP = map, $
                                     REALIZE = 1, $
                                     UPDATE = 1
  ; ...Obtain the draw widget window ID
  WIDGET_CONTROL, draw_widget_id, GET_VALUE = draw_window_id


  ; Load the information structure
  info = { Debug          : debug, $
           Draw_Widget_ID : draw_widget_id, $
           Draw_Window_ID : draw_window_id, $
           Plottype_Base_Id   : plottype_base_id, $
           Plottype_Button_Id : plottype_button_id, $
           Plottype_Value     : plottype_value, $
           Plottype           : ' ', $
           Button_Id      : button_id, $
           Redisplay_Id   : redisplay_id, $
           Slider_Base_Id : slider_base_id, $
           Slider_Id      : slider_id, $
           Active         : active, $
           n_Active       : 0L, $
           Control_Value  : control_value, $
           Full_Filename  : ' ', $
           Filename       : ' ', $
           MWwaterCoeff   : OBJ_NEW() }
  infoptr = PTR_NEW(Info)
  WIDGET_CONTROL, top_level_base_id, SET_UVALUE = infoptr
  
  
  ; Display some user info
  XYOUTS, 0.5, 0.5, $
          'Use File->Open to select a file', $
          /NORM, $
          ALIGNMENT = 0.5, $
          CHARSIZE = 2.0


  ; Start the XManager
  XMANAGER, 'MWwaterCoeff_Viewer', top_level_base_id, $
            CLEANUP = 'MWwaterCoeff_Viewer_Cleanup', $
            GROUP_LEADER = top_level_base_id

END
