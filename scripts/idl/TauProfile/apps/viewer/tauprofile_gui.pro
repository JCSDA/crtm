;
; $Id$
;

;-------------------------------------------------
PRO tv_freestate, id
  COMPILE_OPT HIDDEN
  WIDGET_CONTROL, id, GET_UVALUE = state
  OBJ_DESTROY, state
END


;-------------------------------------------------
PRO tv_getstate, id, state
  COMPILE_OPT HIDDEN
  WIDGET_CONTROL, id, GET_UVALUE = state
END


;-------------------------------------------------
PRO tv_setstate, id, state
  COMPILE_OPT HIDDEN
  WIDGET_CONTROL, id, SET_UVALUE = state
END


;-------------------------------------------------
PRO tv_cleanup, id
  COMPILE_OPT HIDDEN
  tv_getstate, id, state
  debug = state['debug']
  IF ( debug ) THEN MESSAGE, 'Entered...', /INFORMATIONAL
  tv_freestate, id
  IF ( debug ) THEN MESSAGE, '...Exiting', /INFORMATIONAL
END



;-------------------------------------------------
PRO tv_menu_exit_event, event
  COMPILE_OPT HIDDEN
  tv_getstate, event.Top, state
  debug = state['debug']
  IF ( debug ) THEN MESSAGE, 'Entered...', /INFORMATIONAL
  WIDGET_CONTROL, event.Top, /DESTROY
  IF ( debug ) THEN MESSAGE, '...Exiting', /INFORMATIONAL
END


;-------------------------------------------------
PRO tv_menu_open_event, event
  COMPILE_OPT HIDDEN
  tv_getstate, event.Top, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  ; Get the filename
  filename = DIALOG_PICKFILE( $
    TITLE = 'Select a TauProfile file', $
    FILTER = '*TauProfile*', $
    /MUST_EXIST )
  IF ( ~ Valid_String(filename) ) THEN RETURN

  ; Get the plot type from event uvalue
  WIDGET_CONTROL, event.Id, GET_UVALUE = uvalue
  state['plot_type'] = uvalue

  ; Save state variable
  tv_setstate, event.Top, state

  ; Load and display the data
  tv_load_file, filename, event.Top

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL

END


;-------------------------------------------------
PRO tv_menu_print_event, event
  COMPILE_OPT HIDDEN
  tv_getstate, event.Top, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL
  
  ; Get the current data position values
  WIDGET_CONTROL, state['tv_slider_channel_id'], GET_VALUE=l
  WIDGET_CONTROL, state['tv_slider_angle_id']  , GET_VALUE=i
  WIDGET_CONTROL, state['tv_slider_profile_id'], GET_VALUE=m

  ; Create the output filename
  filename = FILE_BASENAME(state['filename'],'.nc') + $
               '_channel'+STRTRIM((state['channel'])[l-1],2)  + $
               '_angle'+STRING((state['angle'])[i-1],FORMAT='(f4.2)') + $
               '_profile'+STRTRIM((state['profile'])[m-1],2) + $
               '.png'

  ; Save the plot to file
  state['tv_window_id'].SetCurrent
  MESSAGE, 'Creating output file ' + filename, /INFORMATIONAL
  state['tv_window_id'].Save, filename, HEIGHT=850

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL
END



;-------------------------------------------------
PRO tv_slider_event, event
  COMPILE_OPT HIDDEN
  tv_getstate, event.Top, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  tv_display, event.Top

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL
END



;-------------------------------------------------
PRO tv_slider_profile_event, event
  COMPILE_OPT HIDDEN
  tv_getstate, event.Top, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  ; Get the profile number
  WIDGET_CONTROL, $
    state['tv_slider_profile_id'], $
    GET_VALUE = profile_number

  ; Alert user
  WIDGET_CONTROL, /HOURGLASS

  ; Read the requested profile
  (state['data']).ReadFile, $
    state['filename'], $
    Profile_List = profile_number, $
    Debug = state['debug']

  ; Save the state variable
  tv_setstate, event.Top, state

  ; Display the new profile
  tv_display, event.Top

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL

END


;--------------------------------------------
PRO tv_button_updown_event, event
  COMPILE_OPT HIDDEN
  tv_getstate, event.Top, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  ; Reverse the pressure array
  state['pressure'] = REVERSE(state['pressure'])

  ; Save the state variable
  tv_setstate, event.Top, state

  ; Display the new data
  tv_display, event.Top

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL

END


;--------------------------------------------
PRO tv_button_loglin_event, event
  COMPILE_OPT HIDDEN
  tv_getstate, event.Top, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  ; Reset the yLog keyword
  IF ( state['ylog'] EQ 1 ) THEN BEGIN
    state['ylog']        = 0 
    state['ytickformat'] = '' 
  ENDIF ELSE BEGIN
    state['ylog']        = 1
    state['ytickformat'] = 'logticks' 
  ENDELSE

  ; Save the state variable
  tv_setstate, event.Top, state

  ; Display the new data
  tv_display, event.Top

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL

END


;--------------------------------------------
PRO tv_button_symbol_event, event
  COMPILE_OPT HIDDEN
  tv_getstate, event.Top, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  ; Toggle the weighting function keyword
  state['symbol'] = (state['symbol'] EQ 0) ? 1 : 0

  ; Save the state variable
  tv_setstate, event.Top, state

  ; Display the new data
  tv_display, event.Top

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL

END


;--------------------------------------------
PRO tv_button_wgtfn_event, event
  COMPILE_OPT HIDDEN
  tv_getstate, event.Top, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  ; Toggle the weighting function keyword
  state['wgtfn'] = (state['wgtfn'] EQ 0) ? 1 : 0

  ; Save the state variable
  tv_setstate, event.Top, state

  ; Display the new data
  tv_display, event.Top

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL

END


;--------------------------------------------
FUNCTION tv_molecule_name, molecule_id
  COMPILE_OPT HIDDEN
  @tauprofile_parameters
  loc = WHERE( MOLECULE_SET_ID EQ molecule_id, count )
  IF ( count EQ 0 ) THEN MESSAGE, 'Invalid molecule set id: '+STRTRIM(molecule_id,2)
  RETURN, MOLECULE_SET_NAME[loc[0]]
END 


;--------------------------------------------
PRO tv_display, id
  COMPILE_OPT HIDDEN
  @tauprofile_parameters
  tv_getstate, id, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  ; Get the current slider values
  WIDGET_CONTROL, state['tv_slider_channel_id'], GET_VALUE=l
  WIDGET_CONTROL, state['tv_slider_angle_id']  , GET_VALUE=i
  WIDGET_CONTROL, state['tv_slider_profile_id'], GET_VALUE=m


  ; Set up some plotting stuff
  index  = 0
  lnp    = ALOG(state['pressure'])
  yrange = [ MAX(state['pressure']), MIN(state['pressure'])<0.01 ]
  ; ...Plot weighting function?
  IF ( state['wgtfn'] EQ 1 ) THEN $
    xtitle = 'd$\tau$/dlnp' $
  ELSE $
    xtitle = 'Transmittance'
  ; ...Plot symbols?
  IF ( state['symbol'] EQ 1 ) THEN $
    symbol = 'diamond' $
  ELSE $
    symbol = 'none'


  ; Setup window for (re)display
  state['tv_window_id'].Refresh, /DISABLE
  state['tv_window_id'].Erase

    
  ; Loop over the molecule sets
  FOR j = 0L, state['n_molecule_sets']-1L DO BEGIN

    ; Construct the plot titles
    jname = tv_molecule_name((state['molecule_set'])[j])
    Title = 'Molecule Set: ' + jname + '!C' + $
            'Profile: ' + STRTRIM((state['profile'])[m-1],2) + $
            '; Angle: ' + STRING((state['angle'])[i-1],FORMAT='(f4.2)') + $ 
            '; Channel: ' + STRTRIM((state['channel'])[l-1],2) 
            
    ; Extract the requested profile
    (state['data']).Extract_Tau, $
      (state['channel'])[l-1]   , $
      (state['angle'])[i-1]     , $
      m                         , $
      (state['molecule_set'])[j], $
      x                         , $
      Debug = state['debug']
    IF ( state['wgtfn'] EQ 1 ) THEN x = DERIV(lnp,x)
    
    ; Plot the data
    index++
    layout = [ state['n_xplots'], state['n_yplots'], index ]
    
    p = PLOT( x, state['pressure'], $
          XTITLE      = xtitle, $
          YTITLE      = 'Pressure (hPa)', $
          YRANGE      = yrange, $
          YLOG        = state['ylog'], $
          YTICKFORMAT = state['ytickformat'], $
          SYMBOL      = symbol, $
          TITLE       = title, $
          LAYOUT      = layout, $
          FONT_SIZE   = 8, $
          MARGIN      = [0.15, 0.15, 0.05, 0.15], $
          CURRENT     = state['tv_window_id'] )
    !NULL = PLOT([1,1], p.yrange, $
              XRANGE    = p.xrange, $
              XSTYLE    = 1, $
              LINESTYLE = 'dash', $
              COLOR     = 'red', $
              OVERPLOT  = p )
  ENDFOR
  
  
  ; Update the display
  state['tv_window_id'].Refresh


  ; Save state variable
  tv_setstate, id, state

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL

END


;--------------------------------------------
PRO tv_load_file, file, id
  COMPILE_OPT HIDDEN
  @tauprofile_parameters
  tv_getstate, id, state
  IF ( state['debug'] ) THEN MESSAGE, 'Entered...', /INFORMATIONAL

  WIDGET_CONTROL, /HOURGLASS

  ; Create a new TauProfile object
  IF ( state.HasKey('data') ) THEN state.Remove, 'data'
  tp = TauProfile()


  ; Get the list of profiles in the file
  tp.InquireFile, file, Profile = profile, Debug = state['debug']
  state['profile']    = profile
  state['n_profiles'] = N_ELEMENTS(profile)

 
  ; Read the first profile
  tp.ReadFile, file, Profile_List = profile[0], Debug = state['debug']


  ; Copy out the pressure and other dimensional lists and non-profile dimensions
  tp.Get_Property, $
    n_Channels      = n_channels     , $
    n_Angles        = n_angles       , $
    n_Molecule_Sets = n_molecule_sets, $
    Level_Pressure  = pressure       , $
    Channel         = channel        , $
    Angle           = angle          , $
    Molecule_Set    = molecule_set   , $
    Debug = state['debug']
  state['n_channels']      = n_channels     
  state['n_angles']        = n_angles       
  state['n_molecule_sets'] = n_molecule_sets
  state['pressure']     = REVERSE(pressure[1:-1])  ; Default for upwelling
  state['channel']      = channel     
  state['angle']        = angle       
  state['molecule_set'] = molecule_set


  ; Get the non-profile dimensions and save 'em
  tp.Get_Property, $
    n_Channels      = n_channels     , $
    n_Angles        = n_angles       , $
    n_Molecule_Sets = n_molecule_sets, $
    Debug           = state['debug']
  state['n_channels']      = n_channels     
  state['n_angles']        = n_angles       
  state['n_molecule_sets'] = n_molecule_sets
    
    
  ; Determine the plotting layout
  n_xplots = LONG(SQRT(DOUBLE(n_molecule_sets)))
  n_yplots = n_xplots
  IF ( n_xplots*n_yplots LT n_molecule_sets ) THEN n_xplots = n_xplots + 1L
  IF ( n_xplots*n_yplots LT n_molecule_sets ) THEN n_yplots = n_yplots + 1L
  state['n_xplots'] = n_xplots
  state['n_yplots'] = n_yplots
  
  
  ; Save the filename and data
  state['filename'] = file
  state['data']     = tp


  ; Set/sensitise the main dimension slider widgets
  ; ...Channel index
  WIDGET_CONTROL, $
    state['tv_slider_channel_id'], $
    SET_VALUE      = 1, $
    SET_SLIDER_MIN = 1, $
    SET_SLIDER_MAX = state['n_channels'], $
    SENSITIVE = (state['n_channels'] GT 1)
  ; ...Angle index
  WIDGET_CONTROL, $
    state['tv_slider_angle_id'], $
    SET_VALUE      = 1, $
    SET_SLIDER_MIN = 1, $
    SET_SLIDER_MAX = state['n_angles'], $
    SENSITIVE = (state['n_angles'] GT 1)
  ; ...Molecule set index
  WIDGET_CONTROL, $
    state['tv_slider_profile_id'], $
    SET_VALUE      = 1, $
    SET_SLIDER_MIN = 1, $
    SET_SLIDER_MAX = state['n_profiles'], $
    SENSITIVE = (state['n_profiles'] GT 1)


  ; Sensitise the buttons
  WIDGET_CONTROL, state['tv_button_updown_id'], /SENSITIVE
  WIDGET_CONTROL, state['tv_button_loglin_id'], /SENSITIVE
  WIDGET_CONTROL, state['tv_button_symbol_id'], /SENSITIVE
  WIDGET_CONTROL, state['tv_button_wgtfn_id'] , /SENSITIVE


  ; Save the state variable
  tv_setstate, id, state


  ; Display the new data
  tv_display, id

  IF ( state['debug'] ) THEN MESSAGE, '...Exiting', /INFORMATIONAL

END



;--------------------------------------------
PRO TauProfile_GUI, $
  File, $
  Debug = debug

  ; Set up
  COMPILE_OPT HIDDEN
  ; ...Check arguments
  IF ( KEYWORD_SET(debug) ) THEN MESSAGE, 'Entered...', /INFORMATIONAL
  ; ...Widget-y defaults
  frame = 2
  nomap = 0
  map   = 1
  nodrag = 0
  drag   = 1
  notsensitive = 0
  sensitive    = 1
  

  ; Create the top level base
  top_level_base_id = WIDGET_BASE( $
    MAP    = map, $
    MBAR   = menu_bar_id, $
    COLUMN = 1, $
    TITLE  = 'TauProfile_Viewer' )
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
      EVENT_PRO = 'tv_menu_open_event', $
      VALUE     = 'Open', $
      UVALUE    = 'FileOpen' )
    ; ...File->Print
    file_print_id = WIDGET_BUTTON( $
      file_menu_id, $
      EVENT_PRO = 'tv_menu_print_event', $
      VALUE     = 'Print', $
      UVALUE    = 'FilePrint' )
    ; ...File->Exit
    file_exit_id = WIDGET_BUTTON( $
      file_menu_id, $
      EVENT_PRO = 'tv_menu_exit_event', $
      VALUE     = 'Exit', $
      UVALUE    = 'FileExit', $
      /SEPARATOR )


  ; Create the UPPER base for the display
  display_base_id = WIDGET_BASE( $
    top_level_base_id, $
    GROUP_LEADER = top_level_base_id, $
    FRAME        = frame, $
    MAP          = map, $
    ROW          = 1 )
    ; ...Create the widget window for display
    xsize = 1400
    ysize = 850
    draw_id = WIDGET_WINDOW( $
      display_base_id, $
      GROUP_LEADER = top_level_base_id, $
      XSIZE        = xsize, $
      YSIZE        = ysize, $
      FRAME        = frame, $
      SENSITIVE    = 0 )
    

  ; Create the LOWER base for the controls
  control_base_id = WIDGET_BASE( $
    top_level_base_id, $
    GROUP_LEADER = top_level_base_id, $
    FRAME        = frame, $
    MAP          = map, $
    ROW          = 1 )
    ; ...Create the slider base for data selection
    slider_base_id = WIDGET_BASE( $
      control_base_id, $
      GROUP_LEADER = top_level_base_id, $
      FRAME        = frame, $
      MAP          = map, $
      COLUMN       = 1 )
      ; ...Create the channel slider
      tv_slider_channel_id = WIDGET_SLIDER( $
        slider_base_id, $
        GROUP_LEADER = top_level_base_id, $
        DRAG         = nodrag, $
        EVENT_PRO    = 'tv_slider_event', $
        MINIMUM      = 0, $
        MAXIMUM      = 1, $
        SENSITIVE    = notsensitive, $
        TITLE        = 'Channel index', $
        UVALUE       = 'channel', $
        XSIZE        = xsize/2 )
      ; ...Create the angle slider
      tv_slider_angle_id = WIDGET_SLIDER( $
        slider_base_id, $
        GROUP_LEADER = top_level_base_id, $
        DRAG         = nodrag, $
        EVENT_PRO    = 'tv_slider_event', $
        MINIMUM      = 0, $
        MAXIMUM      = 1, $
        SENSITIVE    = notsensitive, $
        TITLE        = 'Angle index', $
        UVALUE       = 'angle', $
        XSIZE        = xsize/2 )
      ; ...Create the profile slider
      tv_slider_profile_id = WIDGET_SLIDER( $
        slider_base_id, $
        GROUP_LEADER = top_level_base_id, $
        DRAG         = nodrag, $
        EVENT_PRO    = 'tv_slider_profile_event', $
        MINIMUM      = 0, $
        MAXIMUM      = 1, $
        SENSITIVE    = notsensitive, $
        TITLE        = 'Profile index', $
        UVALUE       = 'profile', $
        XSIZE        = xsize/2 )
    ; ...Create the base for display button controls
    button_base_id = WIDGET_BASE( $
      control_base_id, $
      GROUP_LEADER = top_level_base_id, $
      FRAME        = frame, $
      MAP          = map, $
      COLUMN       = 1 )
      ; ...Create the button to toggle vertical direction
      tv_button_updown_id = WIDGET_BUTTON( $
        button_base_id, $
        GROUP_LEADER = top_level_base_id, $
        EVENT_PRO    = 'tv_button_updown_event', $
        SENSITIVE    = notsensitive, $
        VALUE        = 'Up/Down', $
        UVALUE       = 'updown' )
      ; ...Create the button to toggle vertical scaling
      tv_button_loglin_id = WIDGET_BUTTON( $
        button_base_id, $
        GROUP_LEADER = top_level_base_id, $
        EVENT_PRO    = 'tv_button_loglin_event', $
        SENSITIVE    = notsensitive, $
        VALUE        = 'Log/Linear', $
        UVALUE       = 'loglin' )
      ; ...Create the button to toggle use of plot symbols
      tv_button_symbol_id = WIDGET_BUTTON( $
        button_base_id, $
        GROUP_LEADER = top_level_base_id, $
        EVENT_PRO    = 'tv_button_symbol_event', $
        SENSITIVE    = notsensitive, $
        VALUE        = 'Symbol', $
        UVALUE       = 'symbol' )
      ; ...Create the button to toggle between transmittance and weighting function plots
      tv_button_wgtfn_id = WIDGET_BUTTON( $
        button_base_id, $
        GROUP_LEADER = top_level_base_id, $
        EVENT_PRO    = 'tv_button_wgtfn_event', $
        SENSITIVE    = notsensitive, $
        VALUE        = 'Tau/WgtFn', $
        UVALUE       = 'wgtfn' )


  ; Map, update and realise the widget heirarchy
  WIDGET_CONTROL, $
    top_level_base_id, $
    MAP = map, $
    /REALIZE, $
    /UPDATE


  ; Obtain the widget window ID
  WIDGET_CONTROL, $
    draw_id, $
    GET_VALUE = tv_window_id


  ; Create a state variable
  state = HASH()
  ; ...Boolean states
  state['debug'] = KEYWORD_SET(debug)
  ; ...Widget ids
  state['tv_slider_channel_id'] = tv_slider_channel_id
  state['tv_slider_angle_id'  ] = tv_slider_angle_id
  state['tv_slider_profile_id'] = tv_slider_profile_id
  state['tv_button_updown_id'] = tv_button_updown_id
  state['tv_button_loglin_id'] = tv_button_loglin_id
  state['tv_button_symbol_id'] = tv_button_symbol_id
  state['tv_button_wgtfn_id']  = tv_button_wgtfn_id
  state['tv_window_id'] = tv_window_id
  ; ...Initial default states
  state['ylog']        = 1
  state['ytickformat'] = 'logticks'
  state['updown'] = 0
  state['symbol'] = 0
  state['wgtfn']  = 0

  
  ; ...and save it
  WIDGET_CONTROL, $
    top_level_base_id, $
    SET_UVALUE = state


  ; Display a file or instructions
  IF ( Valid_String(file) ) THEN BEGIN
    tv_load_file, File, top_level_base_id
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
    'TauProfile_Viewer', $
    top_level_base_id, $
    CLEANUP = 'tv_cleanup', $
    GROUP_LEADER = top_level_base_id, $
    /NO_BLOCK

  IF ( KEYWORD_SET(debug) ) THEN MESSAGE, '...Exiting', /INFORMATIONAL
  
END
