pro lf, first = first, last = last, delay = delay, backward = backward, $
  pause = pause, quiet = quiet, help = help

;+
;  Loop over frames in memory created by FSET.
;  The user may optionally specify the first and last frames (default is
;  to use all frames), the time delay between each frame (default is 0.5 sec),
;  the direction of looping (default is increasing frame number), and whether
;  to pause at the last frame (default is no pause). The user hits any key
;  to end looping.
;  
;  USAGE: LF
;  
;  OPTIONAL KEYWORDS:
;    FIRST      First frame (default=current frame)
;    LAST       Last frame (default=frame preceding current frame)
;    DELAY      Time delay between frame updates in seconds (default=0.5)
;    /BACKWARD  Loop over decreasing frame numbers (defualt=increasing)
;    /PAUSE     Pause at last frame for 2.0*DELAY seconds
;    /QUIET     Do not print any information (default=print)
;    /HELP      Print help information only (default=do not print) 
;
;  USAGE NOTES:
;  (1) FSET must have been executed previously to create frame buffers.
;
;  AUTHOR: Liam Gumley, CIMSS/SSEC, 15-APR-1996 (liam.gumley@ssec.wisc.edu)
;
;  RELATED COMMANDS:
;    FSET  Set up frames in memory
;    SF    Show a frame and print frame information
;    AF    Advance one frame
;    BF    Backup one frame
;
;  EXAMPLE:
;
;; Create 4 frames, display a graphic in each, and then loop.
;
; FSET,FRAMES=4,XSIZE=512,YSIZE=512
; FOR I=4,7 DO BEGIN & TVSCL,REBIN(DIST(2^I),512,512,/SAMPLE) & LOADCT,39 & AF & ENDFOR
; LF
;-

;- return to caller if an error occurs

on_error, 2

;- print help message

if keyword_set( help ) then begin
  print, '  Loop over frames in memory created by FSET.
  print, '  The user may optionally specify the first and last frames (default is
  print, '  to use all frames), the time delay between each frame (default is 0.5 sec),
  print, '  the direction of looping (default is increasing frame number), and whether
  print, '  to pause at the last frame (default is no pause). The user hits any key
  print, '  to end looping.
  print, '  
  print, '  USAGE: LF
  print, '  
  print, '  OPTIONAL KEYWORDS:
  print, '    FIRST      First frame (default=current frame)
  print, '    LAST       Last frame (default=frame preceding current frame)
  print, '    DELAY      Time delay between frame updates in seconds (default=0.5)
  print, '    /BACKWARD  Loop over decreasing frame numbers (defualt=increasing)
  print, '    /PAUSE     Pause at last frame for 2.0*DELAY seconds
  print, '    /QUIET     Do not print any information (default=print)
  print, '    /HELP      Print help information only (default=do not print) 
  print, '
  print, '  USAGE NOTES:
  print, '  (1) FSET must have been executed previously to create frame buffers.
  print, '
  print, '  AUTHOR: Liam Gumley, CIMSS/SSEC, 15-APR-1996 (liam.gumley@ssec.wisc.edu)
  print, '
  print, '  RELATED COMMANDS:
  print, '    FSET  Set up frames in memory
  print, '    SF    Show a frame and print frame information
  print, '    AF    Advance one frame
  print, '    BF    Backup one frame
  print, '
  print, '  EXAMPLE:
  print, '
  print, ' Create 4 frames, display a graphic in each, and then loop.
  print, '
  print, ' FSET,FRAMES=4,XSIZE=512,YSIZE=512
  print, ' FOR I=4,7 DO BEGIN & TVSCL,REBIN(DIST(2^I),512,512,/SAMPLE) & LOADCT,39 & AF & ENDFOR
  print, ' LF
  goto, finish
endif

;- common block to hold handle id

common frame_info, handle_id

;- check that handle_id is defined

if n_elements( handle_id ) eq 0 then $
  message, 'FSET must be executed before LF can be used'

;- get current state information

handle_value, handle_id, state

;- set defaults

if not keyword_set( delay ) then delay = 0.5
direction = 1
if keyword_set( backward ) then direction = -1
if not keyword_set( quiet ) then quiet = 0
if not keyword_set( pause ) then pause = 0
if n_elements( first ) eq 0 then first = state.current
if n_elements( last ) eq 0 then begin
  if direction eq 1 then begin
    last = state.current - 1
    if last lt 0 then last = state.frames - 1
  endif else begin
    last = state.current + 1
    if last gt ( state.frames - 1 ) then last = 0
  endelse
endif

;- tell the user what to do

if not quiet then begin
  print, 'Hit any key to exit...'
endif

;- switch to first frame if necessary

if first ne state.current then begin
  sf, first, /quiet
endif

;- enter frame loop

while get_kbrd( 0 ) eq '' do begin

  if get_kbrd( 0 ) ne '' then goto, finish
  wait, delay
  if get_kbrd( 0 ) ne '' then goto, finish
  
  if direction eq 1 then begin
    af, /quiet
  endif else begin
    bf, /quiet
  endelse
  
  handle_value, handle_id, state
  if state.current eq last then begin
    if get_kbrd( 0 ) ne '' then goto, finish
    wait, delay
    if get_kbrd( 0 ) ne '' then goto, finish
    if pause ne 0 then wait, delay
    if get_kbrd( 0 ) ne '' then goto, finish
    sf, first, /quiet
  endif
  
endwhile

finish:

end
