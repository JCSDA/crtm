pro sf, frame, quiet = quiet, help = help

;+
;  Print the current frame number and visible window ID, and optionally
;  switch to a specified frame number.
;  
;  USAGE: SF
;
;  OPTIONAL ARGUMENTS:
;    FRAME   Frame number to switch to
;  
;  OPTIONAL KEYWORDS:
;    /QUIET  Do not print any information (default=print)
;    /HELP   Print help information only (default=do not print) 
;
;  USAGE NOTES:
;  (1) FSET must have been executed previously to create frame buffers.
;  (2) Use the LF (loop frames) command for looping.
;
;  AUTHOR: Liam Gumley, CIMSS/SSEC, 15-APR-1996 (liam.gumley@ssec.wisc.edu)
;          10-MAY-1996 Fixed problem when size of non-scrolling window
;                      was changed.
;          27-AUG-1996 Color table for visible frame is now loaded before image.
;
;  RELATED COMMANDS:
;    FSET  Set up frames in memory
;    AF    Advance one frame
;    BF    Backup one frame
;    LF    Loop frames
;
;  EXAMPLE:
;
;; Create two frames with a graphic in each, and switch between them.
;; (Note that the LF command is designed for looping frames.)
;
; FSET,FRAMES=2,XSIZE=512,YSIZE=512
; A=DIST(64)
; SF,0
; TVSCL,REBIN(A,512,512) & LOADCT,39
; SF,1
; SHOW3,A & LOADCT,39
; FRAME=0
; PRINT,"HIT Q TO QUIT, ANY OTHER KEY TO SWITCH FRAMES"
; WHILE GET_KBRD(1) NE "Q" DO BEGIN & SF,FRAME & FRAME=FRAME XOR 1 & ENDWHILE
;-

;- return to caller if an error occurs

on_error, 2

;- print help message

if keyword_set( help ) then begin
  print, '  Print the current frame number and visible window ID, and optionally
  print, '  switch to a specified frame number.
  print, '  
  print, '  USAGE: SF
  print, '
  print, '  OPTIONAL ARGUMENTS:
  print, '    FRAME   Frame number to switch to
  print, '  
  print, '  OPTIONAL KEYWORDS:
  print, '    /QUIET  Do not print any information (default=print)
  print, '    /HELP   Print help information only (default=do not print) 
  print, '
  print, '  USAGE NOTES:
  print, '  (1) FSET must have been executed previously to create frame buffers.
  print, '  (2) Use the LF (loop frames) command for looping.
  print, '
  print, '  AUTHOR: Liam Gumley, CIMSS/SSEC, 15-APR-1996 (liam.gumley@ssec.wisc.edu)
  print, '          10-MAY-1996 - Fixed problem when size of non-scrolling window
  print, '                        was changed.
  print, '          27-AUG-1996 Color table for visible frame is now loaded before image.
  print, '
  print, '  RELATED COMMANDS:
  print, '    FSET  Set up frames in memory
  print, '    AF    Advance one frame
  print, '    BF    Backup one frame
  print, '    LF    Loop frames
  print, '
  print, '  EXAMPLE:
  print, '
  print, ' Create two frames with a graphic in each, and switch between them.
  print, ' (Note that the LF command is designed for looping frames.)
  print, '
  print, ' FSET,FRAMES=2,XSIZE=512,YSIZE=512
  print, ' A=DIST(64)
  print, ' SF,0
  print, ' TVSCL,REBIN(A,512,512) & LOADCT,39
  print, ' SF,1
  print, ' SHOW3,A & LOADCT,39
  print, ' FRAME=0
  print, ' PRINT,"HIT Q TO QUIT, ANY OTHER KEY TO SWITCH FRAMES"
  print, ' WHILE GET_KBRD(1) NE "Q" DO BEGIN & SF,FRAME & FRAME=FRAME XOR 1 & ENDWHILE
  goto, finish
endif

;- common block to hold handle id

common frame_info, handle_id

;- check that handle_id is defined

if n_elements( handle_id ) eq 0 then $
  message, 'FSET must be executed before SF can be used'

;- get state information

handle_value, handle_id, state

;- check if non-scrolling window size has been changed

if state.base lt 0 then begin

  if ( !d.x_size ne state.xsize ) or ( !d.y_size ne state.ysize ) then begin
  
    ;- tell user that visible window will be re-created
    
    if not keyword_set( quiet ) then begin
      print, 'Visible window size was changed - recreating with correct size'
    endif
    
    ;- delete visible window, and re-create it with the correct size
    
    wdelete, state.visible
    title = 'FSET Visible Window'
    window, xsize = state.xsize, ysize = state.ysize, title = title, /free
    state.visible = !d.window

    ;- copy the frame buffer contents into the re-created visible window
    
    device, copy = [ 0, 0, state.xsize, state.ysize, 0, 0, state.index( state.current ) ]    

  endif

endif

;- if no arguments, just print info

if n_params() eq 0 then goto, print_info

;- save state variables for current frame

state.xstate( state.current ) = !x
state.ystate( state.current ) = !y
state.zstate( state.current ) = !z
state.pstate( state.current ) = !p
tvlct, r, g, b, /get
state.red( *, state.current ) = r
state.green( *, state.current ) = g
state.blue( *, state.current ) = b

;- copy contents of visible window into current frame

wset, state.index( state.current )
device, copy = [ 0, 0, state.xsize, state.ysize, 0, 0, state.visible ]

;- set current frame number to new frame

state.current = ( frame > 0 ) < ( state.frames - 1 )

;- load state variables for current frame

!x = state.xstate( state.current )
!y = state.ystate( state.current )
!z = state.zstate( state.current )
!p = state.pstate( state.current )
r = state.red( *, state.current )
g = state.green( *, state.current )
b = state.blue( *, state.current )
tvlct, r, g, b

;- copy contents of new frame into visible window

wset, state.visible
device, copy = [ 0, 0, state.xsize, state.ysize, 0, 0, $
  state.index( state.current ) ]

;- update state information

handle_value, handle_id, state, /set

print_info:

;- print current frame number at bottom left of visible window

xyouts, 0, 0, strcompress( state.current, /remove_all ), /normal

;- let user know what happened

if not keyword_set( quiet ) then begin
  print, strcompress( 'Frame ' + string( state.current ) + $
    ', Visible window ID ' + string( state.visible ) )
endif

finish:

end
