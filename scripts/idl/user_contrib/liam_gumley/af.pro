pro af, quiet = quiet, help = help

;+
;  Advance to the next frame.
;  
;  USAGE: AF
;
;  OPTIONAL KEYWORDS:
;    /QUIET  Do not print any information (default=print)
;    /HELP   Print help information only (default=do not print) 
;
;  USAGE NOTES:
;  (1) FSET must have been executed previously to create frame buffers.
;
;  AUTHOR: Liam Gumley, CIMSS/SSEC, 15-APR-1996 (liam.gumley@ssec.wisc.edu)
;
;  RELATED COMMANDS:
;    FSET  Set up frames in memory
;    SF    Show a frame and print frame information
;    BF    Backup one frame
;    LF    Loop frames
;
;  EXAMPLE:
;
;; Create two graphics frames, and display an image in each
;
; FSET,FRAMES=2
; PLOT,INDGEN(20) & WAIT,2.0
; AF
; PLOT,-1*INDGEN(20) & WAIT,2.0
; AF
;-

;- return to caller if an error occurs

on_error, 2

;- print help message

if keyword_set( help ) then begin
  print, '  Advance to the next frame.
  print, '  
  print, '  USAGE: AF
  print, '
  print, '  OPTIONAL KEYWORDS:
  print, '    /QUIET  Do not print any information (default=print)
  print, '    /HELP   Print help information only (default=do not print) 
  print, '
  print, '  USAGE NOTES:
  print, '  (1) FSET must have been executed previously to create frame buffers.
  print, '
  print, '  AUTHOR: Liam Gumley, CIMSS/SSEC, 15-APR-1996 (liam.gumley@ssec.wisc.edu)
  print, '
  print, '  RELATED COMMANDS:
  print, '    FSET  Set up frames in memory
  print, '    SF    Show a frame and print frame information
  print, '    BF    Backup one frame
  print, '    LF    Loop frames
  print, '
  print, '  EXAMPLE:
  print, '
  print, ' Create two graphics frames, and display an image in each
  print, '
  print, ' FSET,FRAMES=2
  print, ' PLOT,INDGEN(20) & WAIT,2.0
  print, ' AF
  print, ' PLOT,-1*INDGEN(20) & WAIT,2.0
  print, ' AF
  goto, finish
endif

;- common block to hold handle id

common frame_info, handle_id

;- set defaults

if not keyword_set( quiet ) then quiet = 0

;- check that handle_id is defined

if n_elements( handle_id ) eq 0 then $
  message, 'FSET must be called before AF can be used'

;- get current state information

handle_value, handle_id, state

;- advance one frame

newframe = state.current + 1
if newframe gt state.frames - 1 then newframe = 0
sf, newframe, /quiet

;- let user know what happened

if not quiet then begin
  print, strcompress( 'Frame ' + string( newframe ) )
endif

finish:

end
