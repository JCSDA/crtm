pro fset, frames = frames, xsize = xsize, ysize = ysize, colors = colors, $
  x_scroll_size = x_scroll_size, y_scroll_size = y_scroll_size, $
  portrait = portrait, landscape = landscape, quiet = quiet, help = help, $
  group = group

;+
;  Create a visible window and a corresponding buffer of frames in memory.
;  Color tables and plotting system variables (!x,!y,!z,!p)
;  are saved for each frame. Optional scroll bars may be used.
;  
;  USAGE: FSET
;  
;  OPTIONAL KEYWORDS:
;    FRAMES         Number of frames to create (default=4)
;    XSIZE          Horizontal frame size in pixels (default=640)
;    YSIZE          Vertical frame size in pixels (default=480)
;    COLORS         Number of colors to use
;                   (Only has an effect when set for first window
;                   created in an IDL session)
;    X_SCROLL_SIZE  Viewable window horizontal size in pixels (default=no scroll)
;    Y_SCROLL_SIZE  Viewable window vertical size in pixels (default=no scroll)
;    /PORTRAIT      Creates a window sized like an 8.5x11" portrait page
;    /LANDSCAPE     Creates a window sized like an 11x8.5" landscape page
;    /QUIET         Do not print frame setup information (default=print)
;    /HELP          Print help information only (default=do not print) 
;
;  USAGE NOTES:
;  (1) Only one set of frame buffers created by FSET may be active at one time.
;      If a set of frame buffers exists, executing FSET will destroy the old
;      frame buffers and create a new set.
;  (2) Other graphics windows may be created while FSET frame buffers are in use.
;      Just use WINDOW,/FREE, and then examine !D.WINDOW to find the id of the
;      newly created window. Draw widgets may also be created while FSET
;      frame buffers are in use.
;  (3) To find the window ID for the FSET visible window, just type SF.
;  (4) Odd behavior may occur when COLORS=256 is specified.
;
;  AUTHOR: Liam Gumley, CIMSS/SSEC, 15-APR-1996 (liam.gumley@ssec.wisc.edu)
;          15-MAY-1996 Added /PORTRAIT and /LANDSCAPE keywords
;          27-AUG-1996 Report number of colors allocated
;          15-OCT-1996 It seems to work in 24 bit mode as well
;
;  RELATED COMMANDS:
;    SF    Show a frame and print frame information
;    AF    Advance one frame
;    BF    Backup one frame
;    LF    Loop frames
;
;  EXAMPLE:
;
;; Create 2 frames, display a different kind of graphic in each (with
;; different color tables), overplot an array of values in each frame
;; using the data coordinates defined for each frame, and then loop
;; through the frames.
;
; FSET,FRAMES=2
; LOADCT,20 & PLOT,INDGEN(20),PSYM=2 & WAIT,1.0 & AF
; LOADCT,30 & MAP_SET,/CONT & WAIT,1.0 & AF
; FOR F=0,1 DO BEGIN & SF,F & OPLOT,INDGEN(20),PSYM=4 & WAIT,1.0 & ENDFOR
; LF,DELAY=1.0
;-

;- return to caller if an error occurs

on_error, 2

;- print help message

if keyword_set( help ) then begin
  print, '  Create a visible window and a corresponding buffer of frames in memory.
  print, '  Color tables and plotting system variables (!x,!y,!z,!p)
  print, '  are saved for each frame. Optional scroll bars may be used.
  print, '  
  print, '  USAGE: FSET
  print, '  
  print, '  OPTIONAL KEYWORDS:
  print, '    FRAMES         Number of frames to create (default=4)
  print, '    XSIZE          Horizontal frame size in pixels (default=640)
  print, '    YSIZE          Vertical frame size in pixels (default=480)
  print, '    COLORS         Number of colors to use
  print, '                   (Only has an effect when set for first window
  print, '                   created in an IDL session)
  print, '    X_SCROLL_SIZE  Viewable window horizontal size in pixels (default=no scroll)
  print, '    Y_SCROLL_SIZE  Viewable window vertical size in pixels (default=no scroll)
  print, '    /PORTRAIT      Creates a window sized like an 8.5x11" portrait page
  print, '    /LANDSCAPE     Creates a window sized like an 11x8.5" landscape page
  print, '    /QUIET         Do not print frame setup information (default=print)
  print, '    /HELP          Print help information only (default=do not print) 
  print, '
  print, '  USAGE NOTES:
  print, '  (1) Only one set of frame buffers created by FSET may be active at one time.
  print, '      If a set of frame buffers exists, executing FSET will destroy the old
  print, '      frame buffers and create a new set.
  print, '  (2) Other graphics windows may be created while FSET frame buffers are in use.
  print, '      Just use WINDOW,/FREE, and then examine !D.WINDOW to find the id of the
  print, '      newly created window. Draw widgets may also be created while FSET
  print, '      frame buffers are in use.
  print, '  (3) To find the window ID for the FSET visible window, just type SF.
  print, '  (4) Odd behavior may occur when COLORS=256 is specified.
  print, '
  print, '  AUTHOR: Liam Gumley, CIMSS/SSEC, 15-APR-1996 (liam.gumley@ssec.wisc.edu)
  print, '          15-MAY-1996 Added /PORTRAIT and /LANDSCAPE keywords
  print, '          27-AUG-1996 Report number of colors allocated
  print, '          15-OCT-1996 It seems to work in 24 bit mode as well
  print, '
  print, '  RELATED COMMANDS:
  print, '    SF    Show a frame and print frame information
  print, '    AF    Advance one frame
  print, '    BF    Backup one frame
  print, '    LF    Loop frames
  print, '
  print, '  EXAMPLE:
  print, '
  print, ' Create 2 frames, display a different kind of graphic in each (with
  print, ' different color tables), overplot an array of values in each frame
  print, ' using the data coordinates defined for each frame, and then loop
  print, ' through the frames.
  print, '
  print, ' FSET,FRAMES=2
  print, ' LOADCT,20 & PLOT,INDGEN(20),PSYM=2 & WAIT,1.0 & AF
  print, ' LOADCT,30 & MAP_SET,/CONT & WAIT,1.0 & AF
  print, ' FOR F=0,1 DO BEGIN & SF,F & OPLOT,INDGEN(20),PSYM=4 & WAIT,1.0 & ENDFOR
  print, ' LF,DELAY=1.0
  goto, finish
endif

;- common block to hold handle id

common frame_info, handle_id
  
;- set keyword defaults

if n_elements( frames ) eq 0 then frames = 4
if n_elements( xsize ) eq 0 then xsize = 640
if n_elements( ysize ) eq 0 then ysize = 480
if n_elements( colors ) eq 0 then colors = -1

;- check input parameters

if frames lt 1 then message, 'FRAMES must be GE 1'
if xsize lt 32 then message, 'XSIZE must be GE 32'
if ysize lt 32 then message, 'YSIZE must be GE 32'

;- if FSET has been run previously, delete old windows

if n_elements( handle_id ) ne 0 then begin

  handle_value, handle_id, state

  if state.base lt 0 then begin

    ;- delete normal graphics windows
    
    wdelete, state.visible
    for i = 0, state.frames - 1 do begin
      wdelete, state.index( i )
    endfor

  endif else begin

    ;- destroy draw widget if user didn't already close it
    
    if widget_info( state.base, /valid_id ) then $
      widget_control, state.base, /destroy

  endelse

  ;- reset state variable
  
  state = 0
  handle_value, handle_id, state, /set

endif

;- check if portrait or landscape keywords were set
;- and if so, compute window size based on display size

if keyword_set( portrait ) or keyword_set( landscape ) then begin
  device, get_screen_size = sz
  ysize = sz( 1 ) - 50
  xsize = fix( 8.5 * ysize / 11.0 )
  if keyword_set( landscape ) then begin
    tmp = ysize
    ysize = xsize
    xsize = tmp
  endif
endif

;- create visible window with scrollbars (draw widget) or
;- without scrollbars (normal graphics window)

title = 'FSET Visible Window'

if keyword_set( x_scroll_size ) or keyword_set( y_scroll_size ) then begin

  if x_scroll_size le 32 then x_scroll_size = 512
  if y_scroll_size le 32 then y_scroll_size = 512
  if not keyword_set( group ) then group = 0
  base = widget_base( group = group, title = title )
  draw = widget_draw( base, xsize = xsize, ysize = ysize, colors = colors, $
    x_scroll_size = x_scroll_size, y_scroll_size = y_scroll_size )
  widget_control, base, /realize
  widget_control, draw, get_value = visible

endif else begin

  window, xsize = xsize, ysize = ysize, colors = colors, /free, title = title
  visible = !d.window
  base = -1

endelse

;- create state variables for each frame

index = intarr( frames )
xstate = make_array( frames, value = !x )
ystate = make_array( frames, value = !y )
zstate = make_array( frames, value = !z )
pstate = make_array( frames, value = !p )
loadct, 0
tvlct, r, g, b, /get
n = n_elements( r )
red = rebin( r, n, frames )
green = rebin( g, n, frames )
blue = rebin( b, n, frames )

;- create frames in memory

for i = 0, frames - 1 do begin
  window, xsize = xsize, ysize = ysize, colors = colors, /free, /pixmap
  index( i ) = !d.window
endfor

;- point to visible frame and set current frame index

wset, visible
current = 0

;- print current frame number at bottom left of visible window

xyouts, 0, 0, strcompress( current, /remove_all ), /normal

;- create structure of state variables and store using handle

state = { frames:frames, xsize:xsize, ysize:ysize, visible:visible, $
  index:index, current:current, xstate:xstate, ystate:ystate, zstate:zstate, $
  pstate:pstate, red:red, green:green, blue:blue, base:base }
handle_id = handle_create()
handle_value, handle_id, state, /set

;- let user know what happened

if not keyword_set( quiet ) then begin
  print, strcompress( 'Created ' + string( frames ) + ' frames' )
  print, strcompress( 'Frame size is ' + string( xsize ) + ' by ' + $
    string( ysize ) )
  if keyword_set( x_scroll_size ) or keyword_set( y_scroll_size ) then $
  print, strcompress( 'Scroll size is ' + $
    string( x_scroll_size ) + ' by ' + string( y_scroll_size ) )
  print, strcompress( 'Current Frame is ' + string( current ) )
  print, strcompress( 'Number of colors is ' + string( !d.n_colors ) )
endif

finish:

end
