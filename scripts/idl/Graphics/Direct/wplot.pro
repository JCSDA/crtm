;
;+
; NAME:
;       WPLOT
;
; PURPOSE:
;       Plots X,Y data in a resizeable, zoomable graphics window. To zoom in
;       press the LEFT mouse button and drag the rubber-band box over the region
;       of interest. To return to the original plotting limits, press the RIGHT
;       mouse button.
;
; CATEGORY:
;       Direct Graphics
;
; CALLING SEQUENCE:
;       WPLOT, X, Y
;
; INPUTS:
;       X: abscissa data
;       Y: ordinate data
;
; KEYWORD PARAMETERS:
;       All the keywords allowed in the PLOT command
;
;       NEW: Set this keyword to create a new WPLOT window rather than reusing
;            an existing one.
;
; OUTPUTS:
;       None (yet)
;
; COMMON BLOCKS:
;       WPLOT_comm: This common block contains the state and data pointers.
;
; SIDE EFFECTS:
;       This routine will no longer work if the command
;         PTR_FREE, PTR_VALID()
;       is used, either on the command line or in any application programs.
;     
;       There does seem to be some crosstalk between windows when zooming in and
;       out in different windows that exist simultaneously. It's a bug and is 
;       being looked at.
;
; RESTRICTIONS:
;       Cannot use the command
;         PTR_FREE, PTR_VALID()
;       anywhere when WPLOT windows are active.
;
; PROCEDURE:
;       The data displayed in the WPLOT window, and the associated PLOT keywords used, are
;       stored in a linked list for each WPLOT window. Each linked list is accessed
;       through a data pointer array. The state information for each window is stored
;       in a structure array. Both of these arrays are accessed via pointers in a
;       common block.
;
; EXAMPLE:
;       x = findgen(100)
;       y = x
;       wplot, x, sqrt(y)
;
; MODIFICATION HISTORY:
; 	Written by:     Paul van Delst, CIMSS/SSEC, 25-Nov-1997
;
;-

;
; Function to determine which button was used
;
FUNCTION WPLOT_Which_Button, Value
  CASE Value OF
     1:   e='LEFT'
     2:   e='MIDDLE'
     4:   e='RIGHT'
     8:   e='UP'
    16:   e='DOWN'
    ELSE: e='NONE'
  ENDCASE
  RETURN, e
END



;##############################################################################
;##############################################################################
;
;                        ## WPLOT_REPLOT PROCEDURE ##
;
;
pro wplot_replot, state_ptr, data_ptr, loc
;
;
; Procedure to replot the data in the current window - which is defined
;   by the state array index, loc.
;
;##############################################################################
;##############################################################################


; ---------------------------------------------------
; Dereference the data pointer array and get the data
; ---------------------------------------------------

  data_ptr_array = temporary( *data_ptr )
  data = *data_ptr_array[ loc ]


; -----------------------
; Plot the top level data
; -----------------------

  plot, *data.x, *data.y, $
;       color = data.color, $
        linestyle = data.linestyle, $
        thick = data.thick, $
        psym = data.psym, $
        _extra = *data.extra, $
        xrange = ( *state_ptr )[ loc ].xrange, $
        yrange = ( *state_ptr )[ loc ].yrange


; -----------------------------------------
; Overplot the "child" data if there is any
; -----------------------------------------

  while ( ptr_valid( data.next ) ne 0 ) do begin
    data = *data.next
    oplot, *data.x, *data.y, $
           color = data.color, $
           linestyle = data.linestyle, $
           thick = data.thick, $
           psym = data.psym, $
           _extra = *data.extra
  endwhile


; ------------------------
; Restore the data pointer
; ------------------------

  *data_ptr = temporary( data_ptr_array )

end



;##############################################################################
;##############################################################################
;
;                    ## WPLOT_PS_OUTPUT PROCEDURE ##
;
;
pro wplot_ps_output, event
;
;
; Procedure to output wplot contents to PostScript.
;
;##############################################################################
;##############################################################################

  @wplot_common

  loc = ( where( ( *state_ptr ).tlb_id eq event.top ) )[ 0 ]

  set_plot, 'PS'
  psave = !p
  !p.font = 1
  !p.charsize=1.25
  !p.thick=3
  device, /color, /portrait, bits_per_pixel=8
  wplot_replot, state_ptr, data_ptr, loc
  device, /close
  spawn, 'mv idl.ps wplot.ps'
  !p = psave
  set_plot, 'X'


end




;##############################################################################
;##############################################################################
;
;                    ## WPLOT_ERASE_ZOOMBOX PROCEDURE ##
;
;
pro wplot_erase_zoombox, xs, xd, ys, yd, pixmap_id
;
;
; Procedure to erase the zoom box in a wplot window
;
;##############################################################################
;##############################################################################


; ---------------------------------
; Sort the vertices of the zoom box
; ---------------------------------

  s = [ xd, xs ]
  s = temporary( s( sort( s ) ) )
  x0 = s( 0 )
  x1 = s( 1 )

  s = [ yd, ys ]
  s = temporary( s( sort( s ) ) )
  y0 = s( 0 )
  y1 = s( 1 )


; -------------
; Erase the box
; -------------

  device, copy = [ x0, y0, x1 - x0 + 1, 1, x0, y0, pixmap_id ]
  device, copy = [ x0, y1, x1 - x0 + 1, 1, x0, y1, pixmap_id ]
  device, copy = [ x0, y0, 1, y1 - y0 + 1, x0, y0, pixmap_id ]
  device, copy = [ x1, y0, 1, y1 - y0 + 1, x1, y0, pixmap_id ]

end

 
 
;##############################################################################
;##############################################################################
;
;                    ## WPLOT_CREATE_GUI PROCEDURE ##
;
;
pro wplot_create_gui, tlb_id, menu_id, dw_id, win_id
;
;
; Procedure to create the wplot Graphical User Interface.
;
;##############################################################################
;##############################################################################



; --------------------------------
; Create the base and draw widgets
; --------------------------------

  dw_xsize = 600
  dw_ysize = 500

  tlb_id = widget_base( map = 1, $
                        column = 1, $
                        mbar = menu_id, $
                        /tlb_size_events )


  dw_id = widget_draw( tlb_id, $
                       /button_events, $
                       event_pro = 'wplot_draw_event', $
                       xsize = dw_xsize, $
                       ysize = dw_ysize )
                         

; ---------------------
; Create the menu items
; ---------------------

  opmenu_id = widget_button( menu_id, $
                             /menu, $
                             value = 'Output', $
                             xsize = 100 )
  psop_id   = widget_button( opmenu_id, $
                             event_pro = 'wplot_ps_output', $
                             value = 'PostScript' )


; ----------------------------
; Realize the widget heirarchy
; ----------------------------
 
  widget_control, tlb_id, /realize


; ----------------------------
; Create the plot window title
; ----------------------------

  widget_control, dw_id, get_value = win_id
  wset, win_id
  tlb_title = 'WPLOT window ' + strcompress( string( win_id ), /remove_all )
  widget_control, tlb_id, tlb_set_title = tlb_title
 

; -------------------------------------------
; Start the XMANAGER with command line access
; -------------------------------------------

  xmanager, 'wplot', tlb_id, /no_block, cleanup = 'wplot_cleanup', $
            event_handler = 'wplot_resize_event'

end



;##############################################################################
;##############################################################################
;
;                      ## WPLOT_CLEANUP PROCEDURE ##
;
;
pro wplot_cleanup, id, win = win
;
;
; Procedure to cleanup if a window is killed or re-used.
;
;##############################################################################
;##############################################################################

  @wplot_common


; -------------------------
; Check if pointer is valid
; -------------------------

  if ( ptr_valid( state_ptr ) eq 0 ) then return
  n_win = n_elements( *state_ptr )


; ----------------------
; Define location arrays
; ----------------------

  if ( not keyword_set( win ) ) then begin
    loc_delete = where( ( *state_ptr ).tlb_id eq id, count_delete )
    loc_keep   = where( ( *state_ptr ).tlb_id ne id, count_keep )
  endif else begin
    loc_delete = where( ( *state_ptr ).win_id eq id, count_delete )
    loc_keep   = where( ( *state_ptr ).win_id ne id, count_keep )
  endelse

  if ( count_delete le 0 ) then return     ; No pointers to delete!


; -------------------------------------------
; Dereference the pointers (easier to follow)
; -------------------------------------------

  state_array    = temporary( *state_ptr )
  data_ptr_array = temporary( *data_ptr )


; ---------------------------
; Clear all the data pointers
; ---------------------------

  data = temporary( *data_ptr_array[ loc_delete[ 0 ] ] )
  ptr_free, data_ptr_array[ loc_delete[ 0 ] ], data.x, data.y, data.extra

  while ( ptr_valid( data.next ) ne 0 ) do begin
    data_next = temporary( *data.next ) 
    ptr_free, data.next

    data = temporary( data_next )
    ptr_free, data.x, data.y, data.extra
  endwhile


; ----------------------------------------------------------
; Check if the number of window "pointer sets" to be cleared 
; is the same as the total number of remaining windows. If
; it is, just can undefine the state and data pointers also.
; ----------------------------------------------------------

  if ( count_keep eq 0 ) then begin

    ptr_free, state_ptr, data_ptr

  endif else begin

    state_array    = state_array[ loc_keep ]
    *state_ptr     = temporary( state_array )
    data_ptr_array = data_ptr_array[ loc_keep ]
    *data_ptr      = temporary( data_ptr_array )
  
  endelse

    
end



;##############################################################################
;##############################################################################
;
;                       ## WPLOT_RESIZE_EVENT PROCEDURE ##
;
;
pro wplot_resize_event, event
;
;
; Procedure to handle events when a wplot window is resized
;
;##############################################################################
;##############################################################################

  @wplot_common


; -------------------------------------
; Get event description from user value
; -------------------------------------

  widget_control, event.id, get_uvalue = uvalue



; ----------------------------------------------------------
; Find the state structure for the wplot window in which the 
; event occurred
; ----------------------------------------------------------

  loc = ( where( ( *state_ptr ).tlb_id eq event.top ) )[ 0 ]


; -------------------
; Resize the menu bar
; -------------------

; widget_control, ( *state_ptr )[ loc ].menu_id, xsize = event.x


; --------------------------------------------
; Resize the draw window, making it the active
; window in the process
; --------------------------------------------

  widget_control, ( *state_ptr )[ loc ].dw_id, $
                  xsize = event.x, ysize = event.y
  wset, ( *state_ptr )[ loc ].win_id
    

; -------------------------------
; Replot the data for this window
; -------------------------------

  wplot_replot, state_ptr, data_ptr, loc


; ------------------------
; Save the new window size
; ------------------------

  ( *state_ptr )[ loc ].win_xsize = event.x
  ( *state_ptr )[ loc ].win_ysize = event.y


; -------------------------------
; Save the viewing transformation
; -------------------------------

  ( *state_ptr )[ loc ].xstate = !x
  ( *state_ptr )[ loc ].ystate = !y


; --------------------------------------------------
; Determine the pixel ranges allowed for the zoombox
; --------------------------------------------------

  pixel_coords = convert_coord( !x.crange[ 0 ], !y.crange[ 0 ], /data, /to_device )
  x_pixel_min = pixel_coords[ 0 ]
  y_pixel_min = pixel_coords[ 1 ]
  
  pixel_coords = convert_coord( !x.crange[ 1 ], !y.crange[ 1 ], /data, /to_device )
  x_pixel_max = pixel_coords[ 0 ]
  y_pixel_max = pixel_coords[ 1 ]

  ( *state_ptr )[ loc ].pixel_xrange = [ x_pixel_min, x_pixel_max ]
  ( *state_ptr )[ loc ].pixel_yrange = [ y_pixel_min, y_pixel_max ]
  
end



;##############################################################################
;##############################################################################
;
;                        ## WPLOT_DRAW_EVENT PROCEDURE ##
;
;
pro wplot_draw_event, event
;
;
; Procedure to handle zoom-in button events in the wplot draw window(s).
;
; If a LEFT button DOWN event occurs, two things happen:
;   (1) the static and dynamic corners of the zoom box are set, and
;   (2) the event handler for the current draw window/widget is set to
;       WPLOT_DRAW_ZOOMBOX
;
; If a RIGHT button UP event occurs, two things happen:
;   (1) the original x and y data limits are restored, and
;   (2) all the window data is replotted.
;
; Adapted from David Fanning's ZIMAGE.PRO.
;
;##############################################################################
;##############################################################################


;------------------------------------------------------------------------------
;                      -- Declare the common block --
;------------------------------------------------------------------------------

  @wplot_common



;------------------------------------------------------------------------------
;                      -- Determine the event type --
;------------------------------------------------------------------------------

  possible_events = [ 'DOWN', 'UP', 'MOTION', 'SCROLL' ]
  this_event      = possible_events[ event.type ]

  if ( ( this_event ne 'DOWN' ) and $
       ( this_event ne 'UP' ) ) then return



;------------------------------------------------------------------------------
;     -- Determine the location of the current window state structure --
;------------------------------------------------------------------------------

  loc = ( where( ( *state_ptr ).tlb_id eq event.top ) )[ 0 ]
  state = temporary( ( *state_ptr )[ loc ] )



;------------------------------------------------------------------------------
;                      -- Branch on event type --
;------------------------------------------------------------------------------

  case this_event of


;   ------------------
;   Button DOWN events
;   ------------------

    'DOWN': begin

      button_pressed = WPLOT_Which_Button(event.PRESS)

      if ( button_pressed eq 'LEFT' ) then begin


;       -----------------------------------
;       Copy the current window to a pixmap
;       -----------------------------------

        win_id = state.win_id
        window, /free, /pixmap, xsize = state.win_xsize, ysize = state.win_ysize
        state.pixmap_id = !d.window
        wset, state.pixmap_id
        device, copy = [ 0, 0, state.win_xsize, state.win_ysize, 0, 0, $
                         win_id ]
        wset, win_id


;       -------------------------------------
;       Set the static corners of the zoombox
;       -------------------------------------

        state.x_static_pixel = ( event.x > state.pixel_xrange[ 0 ] ) < state.pixel_xrange[ 1 ]
        state.y_static_pixel = ( event.y > state.pixel_yrange[ 0 ] ) < state.pixel_yrange[ 1 ]


;       ----------------------------------------------------
;       Change the event handler for the current draw widget
;       and turn MOTION events ON
;       ----------------------------------------------------

        widget_control, event.id, event_pro = 'wplot_draw_zoombox', $
                        draw_motion_events = 1


      endif

    end


;   ----------------
;   Button UP events
;   ----------------

    'UP': begin

      button_released = WPLOT_Which_Button(event.RELEASE)

      if ( button_released eq 'RIGHT' ) then begin


;       ---------------------------------------------------
;       Make the window in which the event occurred current
;       ---------------------------------------------------

        wset, state.win_id


;       ----------------------------
;       Restore original data limits
;       ----------------------------

        state.xrange = state.default_xrange
        state.yrange = state.default_yrange


;       ------------------------------------
;       Replot data with default data limits
;       ------------------------------------

        ( *state_ptr )[ loc ] = temporary( state )
        wplot_replot, state_ptr, data_ptr, loc
        state = temporary( ( *state_ptr )[ loc ] )


;       -------------------------------
;       Save the viewing transformation
;       -------------------------------

        state.xstate = !x
        state.ystate = !y
        
      endif

    end

    else: print, this_event

  endcase


; -------------------------
; Restore the state pointer
; -------------------------

  ( *state_ptr )[ loc ] = temporary( state )

end



; =======================================================================
; Procedure to continuously draw and erase the zoom box until it receives
;   a LEFT button UP event. The MOTION events are then disabled and the
;   handler is reset back to WPLOT_DRAW_EVENT.
;
; Adapted from David Fanning's ZIMAGE.PRO.
; =======================================================================
PRO WPLOT_Draw_Zoombox, event

  ; Declare the common block
  ; ------------------------
  @wplot_common


  ; Determine the event type
  ; ------------------------
  CASE event.TYPE OF
    0:    this_event='DOWN'
    1:    this_event='UP'
    2:    this_event='MOTION'
    3:    this_event='SCROLL'
    ELSE: this_event='UNKNOWN'
  ENDCASE


  ; Determine the location of the
  ; current window state structure
  ; ------------------------------
  loc   = (WHERE((*state_ptr).tlb_id EQ event.top))[0]
  state = TEMPORARY((*state_ptr)[loc])


  ; Branch on event type
  ; --------------------
  CASE this_event OF


    ; Button UP events
    ; ----------------
    'UP': BEGIN

      ; ...but only for the LEFT mouse button
      IF ( WPLOT_Which_Button(event.RELEASE) EQ 'LEFT' ) THEN BEGIN

        ; Erase the zoombox one final time and delete the pixmap
        WSET, state.win_id
        WPLOT_Erase_Zoombox, state.x_static_pixel, state.x_dynamic_pixel, $
                             state.y_static_pixel, state.y_dynamic_pixel, $
                             state.pixmap_id
        WDELETE, state.pixmap_id

        ; Redirect events to WPLOT_DRAW_EVENT
        WIDGET_CONTROL, event.ID, $
                        EVENT_PRO='WPLOT_Draw_Event', $
                        DRAW_MOTION_EVENTS=0

        ; Define the zoombox outline in device coordinates
        pixel_xrange = [state.x_static_pixel < event.X, $
                        state.x_static_pixel > event.X]
        pixel_yrange = [state.y_static_pixel < event.Y, $
                        state.y_static_pixel > event.Y]

        ; Restore the state information and return if the
        ; the user simply clicked in the window
        IF ( state.x_static_pixel EQ event.X ) OR $
           ( state.y_static_pixel EQ event.Y ) THEN BEGIN
          (*state_ptr)[loc] = TEMPORARY(state)
          RETURN
        ENDIF

        ; Reset the viewing transformation
        !X = state.xstate
        !Y = state.ystate

        ; Convert coordinates to data coordinates
        data_min_coords = CONVERT_COORD(pixel_xrange[0], pixel_yrange[0], /DEVICE, /TO_DATA)
        data_max_coords = CONVERT_COORD(pixel_xrange[1], pixel_yrange[1], /DEVICE, /TO_DATA)

        state.xrange = [data_min_coords[0], data_max_coords[0]]
        state.yrange = [data_min_coords[1], data_max_coords[1]]


        ; Plot the data with new limits
        (*state_ptr)[loc] = TEMPORARY(state)
        WPLOT_Replot, state_ptr, data_ptr, loc
        state = TEMPORARY((*state_ptr)[loc])

        ; Save the viewing transformation
        state.xstate = !X
        state.ystate = !Y
        
      ENDIF  ; LEFT-button release

    END ; UP-button event


    ; MOTION events
    ; -------------
    'MOTION': BEGIN

      ; Erase the zoom box
      WSET, state.win_id
      WPLOT_Erase_Zoombox, state.x_static_pixel, state.x_dynamic_pixel, $
                           state.y_static_pixel, state.y_dynamic_pixel, $
                           state.pixmap_id

      ; Update the dynamic corner of the zoom
      ; box to the current cursor location
      state.x_dynamic_pixel = (event.X > state.pixel_xrange[0]) < state.pixel_xrange[1]
      state.y_dynamic_pixel = (event.Y > state.pixel_yrange[0]) < state.pixel_yrange[1]

      ; Draw the zoom box
      PLOTS, [ state.x_static_pixel , $
               state.x_static_pixel , $
               state.x_dynamic_pixel, $
               state.x_dynamic_pixel, $
               state.x_static_pixel   ], $
             [ state.y_static_pixel , $
               state.y_dynamic_pixel, $
               state.y_dynamic_pixel, $
               state.y_static_pixel , $
               state.y_static_pixel   ], $
             /DEVICE, COLOR=!P.COLOR, LINESTYLE=1
             
    END ; MOTION event
    
  ENDCASE ; Window event type


  ; Restore the state strucutre
  ; ---------------------------
  (*state_ptr)[loc] = TEMPORARY(state)

END



; ========================
; The main WPLOT procedure
; ========================
PRO WPLOT, xp, yp, $
           XRANGE   = xrange   , $
           YRANGE   = yrange   , $
           COLOR    = color    , $
           LINESTYLE= linestyle, $
           THICK    = thick    , $
           PSYM     = psym     , $
           _EXTRA   = extra    , $
           NEW      = new

  ; Declare the common block
  ; ------------------------
  @wplot_common


  ; Check the input arguments
  ; -------------------------
  IF ( N_PARAMS() LT 1 ) THEN BEGIN
    MESSAGE, 'Incorrect number of arguments', /INFORMATIONAL
    RETURN
  ENDIF

  IF ( N_PARAMS() EQ 1 ) THEN BEGIN
    y = xp
    x = FINDGEN(N_ELEMENTS(y))
  ENDIF ELSE BEGIN
    x = xp
    y = yp
  ENDELSE


  ; Check the keywords
  ; ------------------
  IF ( N_ELEMENTS(xrange   ) EQ 0 ) THEN xrange    = [ MIN(x), MAX(x) ]
  IF ( N_ELEMENTS(yrange   ) EQ 0 ) THEN yrange    = [ MIN(y), MAX(y) ]
  IF ( N_ELEMENTS(color    ) EQ 0 ) THEN color     = !P.COLOR
  IF ( N_ELEMENTS(linestyle) EQ 0 ) THEN linestyle = !P.LINESTYLE
  IF ( N_ELEMENTS(thick    ) EQ 0 ) THEN thick     = !P.THICK
  IF ( N_ELEMENTS(psym     ) EQ 0 ) THEN psym      = !P.PSYM
  IF ( N_ELEMENTS(new      ) EQ 0 ) THEN new       = 0 $
                                    ELSE new       = 1


  ; Create and realise the plotting GUI
  ; -----------------------------------
  IF ( ( new EQ 1 ) OR ( PTR_VALID(state_ptr) EQ 0 ) ) THEN BEGIN

    ; Create a new GUI everytime
    WPLOT_Create_GUI, tlb_id, menu_id, dw_id, win_id

  ENDIF ELSE BEGIN

    ; Get current window ID
    win_id = !D.WINDOW

    ; Is this a valid WPLOT window ID?
    loc = (WHERE((*state_ptr).win_id EQ win_id, count))[0]
    IF ( count LE 0 ) THEN BEGIN

      ; Invalid WPLOT ID
      ; ----------------
      ; How many wplot windows are there?
      n_win = N_ELEMENTS(*state_ptr)
      IF ( n_win EQ 0 ) THEN BEGIN
      
        ; No wplot windows, so create one
        WPLOT_Create_GUI, tlb_id, menu_id, dw_id, win_id
        
      ENDIF ELSE BEGIN
      
        ; Make the last created WPLOT window current
        tlb_id  = (*state_ptr)[n_win-1].tlb_id
        menu_id = (*state_ptr)[n_win-1].menu_id
        dw_id   = (*state_ptr)[n_win-1].dw_id
        win_id  = (*state_ptr)[n_win-1].win_id

      ENDELSE

      ; Set the plotting window
      WSET, win_id

    ENDIF ELSE BEGIN

      ; Valid WPLOT ID
      ; --------------
      tlb_id  = (*state_ptr)[loc].tlb_id
      menu_id = (*state_ptr)[loc].menu_id
      dw_id   = (*state_ptr)[loc].dw_id
      
    ENDELSE
  ENDELSE


  ; Plot the data
  ; -------------
  PLOT, x, y, $
        XRANGE   = xrange   , $
        YRANGE   = yrange   , $
        COLOR    = color    , $
        LINESTYLE= linestyle, $
        THICK    = thick    , $
        PSYM     = psym     , $
        _EXTRA   = extra


  ; Determine the pixel ranges allowed for the zoombox
  ; --------------------------------------------------
  pixel_coords = CONVERT_COORD(!X.CRANGE[0], !Y.CRANGE[0], /DATA, /TO_DEVICE)
  x_pixel_min = pixel_coords[0]
  y_pixel_min = pixel_coords[1]
  
  pixel_coords = CONVERT_COORD(!X.CRANGE[1], !Y.CRANGE[1], /DATA, /TO_DEVICE)
  x_pixel_max = pixel_coords[0]
  y_pixel_max = pixel_coords[1]

  pixel_xrange = [x_pixel_min, x_pixel_max]
  pixel_yrange = [y_pixel_min, y_pixel_max]
  

  ; Set up the pointer structures
  ; -----------------------------
  ; Set the state structure
  geometry = WIDGET_INFO(dw_id, /GEOMETRY)
  state = { tlb_id          : tlb_id        , $
            menu_id         : menu_id       , $
            dw_id           : dw_id         , $
            win_id          : win_id        , $
            pixmap_id       : -1L           , $
            win_xsize       : geometry.XSIZE, $
            win_ysize       : geometry.YSIZE, $
            xstate          : !X            , $
            ystate          : !Y            , $
            xrange          : xrange        , $
            yrange          : yrange        , $
            default_xrange  : xrange        , $
            default_yrange  : yrange        , $
            pixel_xrange    : pixel_xrange  , $
            pixel_yrange    : pixel_yrange  , $
            x_static_pixel  : 0L            , $
            y_static_pixel  : 0L            , $
            x_dynamic_pixel : 0L            , $
            y_dynamic_pixel : 0L              }


  ; Set the data structure
  data = { x         : PTR_NEW(x)    , $
           y         : PTR_NEW(y)    , $
           color     : color         , $
           linestyle : linestyle     , $
           thick     : thick         , $
           psym      : psym          , $
           extra     : PTR_NEW(extra), $
           next      : PTR_NEW()       }


  ; Check if state pointer is valid
  ; -------------------------------
  IF ( PTR_VALID(state_ptr) EQ 0 ) THEN BEGIN

    ; Create the state array
    state_ptr = PTR_NEW(state)

    ; Create the data pointer array
    data_ptr_array    = PTRARR(1)
    data_ptr_array[0] = PTR_NEW(data)
    data_ptr          = PTR_NEW(data_ptr_array)

  ENDIF ELSE BEGIN

    ; Clean up old pointers associated with current (old) window id
    IF ( new EQ 0 ) THEN WPLOT_Cleanup, win_id, /WIN

    ; Append current window state and data pointer structures.
    ; Have to check again for pointer validity as the above
    ; garbage collection could have free'd up the last
    ; remaining pointer.
    ; --------------------------------------------------------
    IF ( PTR_VALID(state_ptr) EQ 0 ) THEN BEGIN

      ; Create the state array
      state_ptr = PTR_NEW(state)

      ; Create the data pointer array
      data_ptr_array    = PTRARR(1)
      data_ptr_array[0] = PTR_NEW(data)
      data_ptr          = PTR_NEW(data_ptr_array)

    ENDIF ELSE BEGIN

      ; Append an element to the state array
      state_array = TEMPORARY(*state_ptr)
      state_array = [state_array, state_array[0]]

      ; Update the last element of the state array
      n_win = N_ELEMENTS(state_array)
      state_array[n_win-1].tlb_id          = state.tlb_id
      state_array[n_win-1].menu_id         = state.menu_id
      state_array[n_win-1].dw_id           = state.dw_id
      state_array[n_win-1].win_id          = state.win_id
      state_array[n_win-1].pixmap_id       = state.pixmap_id
      state_array[n_win-1].win_xsize       = state.win_xsize
      state_array[n_win-1].win_ysize       = state.win_ysize
      state_array[n_win-1].xstate          = state.xstate
      state_array[n_win-1].ystate          = state.ystate
      state_array[n_win-1].xrange          = state.xrange
      state_array[n_win-1].yrange          = state.yrange
      state_array[n_win-1].default_xrange  = state.default_xrange
      state_array[n_win-1].default_yrange  = state.default_yrange
      state_array[n_win-1].pixel_xrange    = state.pixel_xrange
      state_array[n_win-1].pixel_yrange    = state.pixel_yrange
      state_array[n_win-1].x_static_pixel  = state.x_static_pixel
      state_array[n_win-1].y_static_pixel  = state.y_static_pixel
      state_array[n_win-1].x_dynamic_pixel = state.x_dynamic_pixel
      state_array[n_win-1].y_dynamic_pixel = state.y_dynamic_pixel

      ; Put the array back into the pointer
      *state_ptr = TEMPORARY(state_array)

      ; Append the current data to the data pointer array
;      data_ptr_array = TEMPORARY(*data_ptr)
;      data_ptr_array = [data_ptr_array, PTR_NEW(data)]
;      *data_ptr      = TEMPORARY(data_ptr_array)
      *data_ptr = [TEMPORARY(*data_ptr), PTR_NEW(data)]

    ENDELSE
  ENDELSE

END

