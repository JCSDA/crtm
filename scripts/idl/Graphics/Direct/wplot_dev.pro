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
;       WPLOT_COMM: This common block contains the state and data pointers.
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
; $Author: paulv $
; $Date: 1997/12/16 03:10:21 $
; $Id: wplot.pro,v 1.10 1997/12/16 03:10:21 paulv Exp $
; $Log: wplot.pro,v $
; Revision 1.10  1997/12/16 03:10:21  paulv
; Fixed bug so that menu_id was defined upon call to WPLOT
; with a window already established.
;
; Revision 1.9  1997/12/16 03:07:00  paulv
; Fixed bug that didn't allow dw_id to be defined on subsequent
; calls to WPLOT when a window was already established.
;
; Revision 1.8  1997/11/28 18:14:10  paulv
; Added !x,!y structures to state structure.
;
; Revision 1.7  1997/11/28 17:27:07  paulv
; Renamed X,Y parameters to allow plotting of only Y data.
;
; Revision 1.6  1997/11/28 17:17:26  paulv
; Fixed bug in creation of new elements of the state array.
;
; Revision 1.5  1997/11/26 17:03:46  paulv
; Added X,Y parameter checks.
;
; Revision 1.4  1997/11/26 16:43:44  paulv
; Changed calls to WHAT_BUTTON_PRESSED/RELEASED to
; WPLOT_BUTTON_PRESSED/RELEASED.
;
; Revision 1.3  1997/11/26 16:01:44  paulv
; Added window zoom box capability.
;
; Revision 1.2  1997/11/25 23:01:50  paulv
; Changed pointer in common block to a state pointer, state_ptr, which
; contains "state" information such as window and widget ids, and a
; data pointer, data_ptr, which points to a pointer array containing
; all the data valid wplot windows.
;
; Renamed wplot_garbage_collection function to wplot_cleanup and use
; it as the procedure called when a widget heirarchy is killed. Defined
; via XMANAGER.
;
; Removed the XSIZE and YSIZE keywords from the WIDGET_BASE call as this
; screws up the window resizing, i.e. won't let you do it.
;
; Revision 1.1  1997/11/25 17:36:30  paulv
; Initial revision
;
;-

;
;##############################################################################
;##############################################################################
;
;                    ## WPLOT_BUTTON_RELEASED FUNCTION ##
;
;
function wplot_button_released, event
; 
; Function to determine what kind of button was released in a wplot
;   draw window. Taken from David Fanning's ZIMAGE.PRO example.
;
;##############################################################################
;##############################################################################
 
  button = ['NONE', 'LEFT', 'MIDDLE', 'NONE', 'RIGHT']
  return, button( event.release )
 
end
 


;##############################################################################
;##############################################################################
;
;                    ## WPLOT_BUTTON_PRESSED FUNCTION ##
;
;
function wplot_button_pressed, event
;
; Function to determine what kind of button was pressed in a wplot
;   draw window. Taken from David Fanning's ZIMAGE.PRO example.
;
;##############################################################################
;##############################################################################
 
  button = ['NONE', 'LEFT', 'MIDDLE', 'NONE', 'RIGHT']
  return, button( event.press )
 
end



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

  common wplot_comm, state_ptr, data_ptr

  loc = ( where( ( *state_ptr ).tlb_id eq event.top ) )[ 0 ]

  set_plot, 'PS'

  fontsave = !p.font
  charsizesave = !p.charsize

  !p.font = 1
  !p.charsize = 2.0

  device, /color, /portrait, bits_per_pixel=8
  wplot_replot, state_ptr, data_ptr, loc
  device, /close
  spawn, 'mv idl.ps wplot.ps'

  !p.font = fontsave
  !p.charsize = charsizesave

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

  common wplot_comm, state_ptr, data_ptr


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

  common wplot_comm, state_ptr, data_ptr


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

  common wplot_comm, state_ptr, data_ptr



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

      button_pressed = wplot_button_pressed( event )

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

      button_released = wplot_button_released( event )

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



;##############################################################################
;##############################################################################
;
;                        ## WPLOT_DRAW_ZOOMBOX PROCEDURE ##
;
;
pro wplot_draw_zoombox, event
;
;
; Procedure to continuously draw and erase the zoom box until it receives
;   a LEFT button UP event. The MOTION events are then disabled and the
;   handler is reset back to WPLOT_DRAW_EVENT.
;
; Adapted from David Fanning's ZIMAGE.PRO.
;
;##############################################################################
;##############################################################################


;------------------------------------------------------------------------------
;                      -- Declare the common block --
;------------------------------------------------------------------------------

  common wplot_comm, state_ptr, data_ptr



;------------------------------------------------------------------------------
;                      -- Determine the event type --
;------------------------------------------------------------------------------

  possible_events = [ 'DOWN', 'UP', 'MOTION', 'SCROLL' ]
  this_event      = possible_events[ event.type ]




;------------------------------------------------------------------------------
;     -- Determine the location of the current window state structure --
;------------------------------------------------------------------------------

  loc = ( where( ( *state_ptr ).tlb_id eq event.top ) )[ 0 ]
  state = temporary( ( *state_ptr )[ loc ] )



;------------------------------------------------------------------------------
;                      -- Branch on event type --
;------------------------------------------------------------------------------

  case this_event of


;   ----------------
;   Button UP events
;   ----------------

    'UP': begin

      button_released = wplot_button_released( event )

      if ( button_released eq 'LEFT' ) then begin


;       ------------------------------------------------------
;       Erase the zoombox one final time and delete the pixmap
;       ------------------------------------------------------

        wset, state.win_id
        wplot_erase_zoombox, state.x_static_pixel, state.x_dynamic_pixel, $
                             state.y_static_pixel, state.y_dynamic_pixel, $
                             state.pixmap_id
        wdelete, state.pixmap_id


;       -----------------------------------
;       Redirect events to WPLOT_DRAW_EVENT
;       -----------------------------------

        widget_control, event.id, event_pro = 'wplot_draw_event', $
                        draw_motion_events = 0


;       ------------------------------------------------
;       Define the zoombox outline in device coordinates
;       ------------------------------------------------

        pixel_xrange = [ state.x_static_pixel < event.x , $
                         state.x_static_pixel > event.x ]

        pixel_yrange = [ state.y_static_pixel < event.y , $
                         state.y_static_pixel > event.y ]


;       --------------------------------------------------
;       Make sure the user didn't just click in the window
;       --------------------------------------------------

        if ( state.x_static_pixel eq event.x ) or $
           ( state.y_static_pixel eq event.y ) then begin

          ( *state_ptr )[ loc ] = temporary( state )
          return

        endif


;       --------------------------------
;       Reset the viewing transformation
;       --------------------------------

        !x = state.xstate
        !y = state.ystate


;       ---------------------------------------
;       Convert coordinates to data coordinates
;       ---------------------------------------

        data_min_coords = convert_coord( pixel_xrange[ 0 ], pixel_yrange[ 0 ], $
                                         /device, /to_data )

        data_max_coords = convert_coord( pixel_xrange[ 1 ], pixel_yrange[ 1 ], $
                                         /device, /to_data )

        state.xrange = [ data_min_coords[ 0 ], data_max_coords[ 0 ] ]
        state.yrange = [ data_min_coords[ 1 ], data_max_coords[ 1 ] ]



;       -----------------------------
;       Plot the data with new limits
;       -----------------------------

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


;   -------------
;   MOTION events
;   -------------

    'MOTION': begin


;     ------------------
;     Erase the zoom box
;     ------------------

      wset, state.win_id
      wplot_erase_zoombox, state.x_static_pixel, state.x_dynamic_pixel, $
                           state.y_static_pixel, state.y_dynamic_pixel, $
                           state.pixmap_id


;     ------------------------------------------------------------------------
;     Update the dynamic corner of the zoom box to the current cursor location
;     ------------------------------------------------------------------------

      state.x_dynamic_pixel = ( event.x > state.pixel_xrange[ 0 ] ) < state.pixel_xrange[ 1 ]
      state.y_dynamic_pixel = ( event.y > state.pixel_yrange[ 0 ] ) < state.pixel_yrange[ 1 ]


;     -----------------
;     Draw the zoom box
;     -----------------

      plots, [ state.x_static_pixel, state.x_static_pixel, state.x_dynamic_pixel, $
               state.x_dynamic_pixel, state.x_static_pixel ], $
             [ state.y_static_pixel, state.y_dynamic_pixel, state.y_dynamic_pixel, $
               state.y_static_pixel, state.y_static_pixel ], $
             /device, color = !p.color, linestyle = 1


    end

  endcase


; ---------------------------
; Restore the state strucutre
; ---------------------------

  ( *state_ptr )[ loc ] = temporary( state )

end



;##############################################################################
;##############################################################################
;
;                          ## WPLOT PROCEDURE ##
;
;
pro wplot, xp, yp, $
           xrange = xrange, yrange = yrange, $
           color = color, linestyle = linestyle, thick = thick, psym = psym, $
           _extra = extra, $
           new = new
;
;##############################################################################
;##############################################################################


;------------------------------------------------------------------------------
;                      -- Declare the common block --
;------------------------------------------------------------------------------

  common wplot_comm, state_ptr, data_ptr



;------------------------------------------------------------------------------
;                         -- Check the parameters --
;------------------------------------------------------------------------------

  if ( n_params() lt 1 ) then begin
    message, 'Incorrect number of arguments', /info
    return
  endif

  if ( n_params() eq 1 ) then begin
    y = xp
    x = findgen( n_elements( y ) )
  endif else begin
    x = xp
    y = yp
  endelse



;------------------------------------------------------------------------------
;                         -- Check the keywords --
;------------------------------------------------------------------------------

  if ( n_elements( xrange ) eq 0 ) then xrange = [ min( x ), max( x ) ]
  if ( n_elements( yrange ) eq 0 ) then yrange = [ min( y ), max( y ) ]
  if ( n_elements( color ) eq 0 ) then color = !p.color
  if ( n_elements( linestyle ) eq 0 ) then linestyle = !p.linestyle
  if ( n_elements( thick ) eq 0 ) then thick = !p.thick
  if ( n_elements( psym ) eq 0 ) then psym = !p.psym
  if ( n_elements( new ) eq 0 ) then new = 0 else new = 1



;------------------------------------------------------------------------------
;                 -- Create and realise the plotting GUI --
;------------------------------------------------------------------------------


  if ( ( new eq 1 ) or ( ptr_valid( state_ptr ) eq 0 ) ) then begin


;   --------------------------
;   Create a new GUI everytime
;   --------------------------

    wplot_create_gui, tlb_id, menu_id, dw_id, win_id

  endif else begin


;   ---------------------
;   Get current window ID
;   ---------------------

    win_id = !d.window


;   --------------------------------
;   Is this a valid wplot window ID?
;   --------------------------------

    loc = ( where( ( *state_ptr ).win_id eq win_id, count ) )[ 0 ]

    if ( count le 0 ) then begin                    ; No wplot windows with win_id found

      n_win = n_elements( *state_ptr )                ; How many wplot windows are there?

      if ( n_win eq 0 ) then begin                    ; No wplot windows -> create one
        wplot_create_gui, tlb_id, menu_id, dw_id, win_id
      endif else begin                                ; Make the last created wplot window current
        tlb_id  = ( *state_ptr )[ n_win - 1 ].tlb_id
        menu_id = ( *state_ptr )[ n_win - 1 ].menu_id
        dw_id   = ( *state_ptr )[ n_win - 1 ].dw_id
        win_id  = ( *state_ptr )[ n_win - 1 ].win_id
      endelse

      wset, win_id

    endif else $

      tlb_id  = ( *state_ptr )[ loc ].tlb_id
      menu_id = ( *state_ptr )[ loc ].menu_id
      dw_id   = ( *state_ptr )[ loc ].dw_id

  endelse






;------------------------------------------------------------------------------
;                           -- Plot the data --
;------------------------------------------------------------------------------

  plot, x, y, $
        xrange = xrange, yrange = yrange, $
        color = color, linestyle = linestyle, thick = thick, psym = psym, $
        _extra = extra



;------------------------------------------------------------------------------
;        -- Determine the pixel ranges allowed for the zoombox --
;------------------------------------------------------------------------------

  pixel_coords = convert_coord( !x.crange[ 0 ], !y.crange[ 0 ], /data, /to_device )
  x_pixel_min = pixel_coords[ 0 ]
  y_pixel_min = pixel_coords[ 1 ]
  
  pixel_coords = convert_coord( !x.crange[ 1 ], !y.crange[ 1 ], /data, /to_device )
  x_pixel_max = pixel_coords[ 0 ]
  y_pixel_max = pixel_coords[ 1 ]

  pixel_xrange = [ x_pixel_min, x_pixel_max ]
  pixel_yrange = [ y_pixel_min, y_pixel_max ]
  


;------------------------------------------------------------------------------
;                      -- Setup the pointer structures --
;------------------------------------------------------------------------------

; -----------------------
; Set the state structure
; -----------------------

  geometry = widget_info( dw_id, /geometry )

  state = { tlb_id          : tlb_id, $
            menu_id         : menu_id, $
            dw_id           : dw_id, $
            win_id          : win_id, $
            pixmap_id       : -1L, $
            win_xsize       : geometry.xsize, $
            win_ysize       : geometry.ysize, $
            xstate          : !x, $
            ystate          : !y, $ 
            xrange          : xrange, $
            yrange          : yrange, $
            default_xrange  : xrange, $
            default_yrange  : yrange, $
            pixel_xrange    : pixel_xrange, $
            pixel_yrange    : pixel_yrange, $
            x_static_pixel  : 0L, $
            y_static_pixel  : 0L, $
            x_dynamic_pixel : 0L, $
            y_dynamic_pixel : 0L }


; ----------------------
; Set the data structure
; ----------------------

  data = { x         : ptr_new( x ), $
           y         : ptr_new( y ), $
           color     : color, $
           linestyle : linestyle, $
           thick     : thick, $
           psym      : psym, $
           extra     : ptr_new( extra ), $
           next      : ptr_new() }


; -------------------------------
; Check if state pointer is valid
; -------------------------------

  if ( ptr_valid( state_ptr ) eq 0 ) then begin


;   ----------------------
;   Create the state array
;   ----------------------

    state_ptr   = ptr_new( state )


;   -----------------------------
;   Create the data pointer array
;   -----------------------------

    data_ptr_array      = ptrarr( 1 )
    data_ptr_array[ 0 ] = ptr_new( data )
    data_ptr            = ptr_new( data_ptr_array )

  endif else begin


;   -------------------------------------------------------------
;   Clean up old pointers associated with current (old) window id
;   -------------------------------------------------------------

    if ( new eq 0 ) then wplot_cleanup, win_id, /win


;   --------------------------------------------------------
;   Append current window state and data pointer structures.
;   Have to check again for pointer validity as the above
;   garbage collection could have free'd up the last
;   remaining pointer.
;   --------------------------------------------------------

    if ( ptr_valid( state_ptr ) eq 0 ) then begin


;     -- Create the state array --

      state_ptr   = ptr_new( state )


;     -- Create the data pointer array --

      data_ptr_array      = ptrarr( 1 )
      data_ptr_array[ 0 ] = ptr_new( data )
      data_ptr            = ptr_new( data_ptr_array )

    endif else begin


;     -- Add to state array --

      state_array = temporary( *state_ptr )
      state_array = [ state_array, state_array[0] ]

      n_win = n_elements( state_array )

      state_array[ n_win - 1].tlb_id          = state.tlb_id
      state_array[ n_win - 1].menu_id         = state.menu_id
      state_array[ n_win - 1].dw_id           = state.dw_id
      state_array[ n_win - 1].win_id          = state.win_id
      state_array[ n_win - 1].pixmap_id       = state.pixmap_id
      state_array[ n_win - 1].win_xsize       = state.win_xsize
      state_array[ n_win - 1].win_ysize       = state.win_ysize
      state_array[ n_win - 1].xstate          = state.xstate
      state_array[ n_win - 1].ystate          = state.ystate
      state_array[ n_win - 1].xrange          = state.xrange
      state_array[ n_win - 1].yrange          = state.yrange
      state_array[ n_win - 1].default_xrange  = state.default_xrange
      state_array[ n_win - 1].default_yrange  = state.default_yrange
      state_array[ n_win - 1].pixel_xrange    = state.pixel_xrange
      state_array[ n_win - 1].pixel_yrange    = state.pixel_yrange
      state_array[ n_win - 1].x_static_pixel  = state.x_static_pixel
      state_array[ n_win - 1].y_static_pixel  = state.y_static_pixel
      state_array[ n_win - 1].x_dynamic_pixel = state.x_dynamic_pixel
      state_array[ n_win - 1].y_dynamic_pixel = state.y_dynamic_pixel

      *state_ptr  = temporary( state_array )


;     -- Add to data pointer array --

      data_ptr_array = temporary( *data_ptr )
      data_ptr_array = [ data_ptr_array, ptr_new( data ) ]
      *data_ptr      = temporary( data_ptr_array )

    endelse

  endelse

end

