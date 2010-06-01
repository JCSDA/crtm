pro bf, quiet = quiet, help = help

;+
;  Backup to the previous frame.
;  
;  USAGE: BF
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
;    AF    Advance one frame
;    LF    Loop frames
;
;  EXAMPLE:
;
;; Create two graphics frames, and display an image in each
;
; FSET,FRAMES=2
; SF,1
; MAP_SET,/MERC,/CONT & WAIT,2.0
; BF
; MAP_SET,/ORTH,/CONT & WAIT,2.0
; BF
;-

;- return to caller if an error occurs

on_error, 2

;- print help message

if keyword_set( help ) then begin
  print, '  Backup to the previous frame.
  print, '  
  print, '  USAGE: BF
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
  print, '    AF    Advance one frame
  print, '    LF    Loop frames
  print, '
  print, '  EXAMPLE:
  print, '
  print, ' Create two graphics frames, and display an image in each
  print, '
  print, ' FSET,FRAMES=2
  print, ' SF,1
  print, ' MAP_SET,/MERC,/CONT & WAIT,2.0
  print, ' BF
  print, ' MAP_SET,/ORTH,/CONT & WAIT,2.0
  print, ' BF
  goto, finish
endif

;- common block to hold handle id

common frame_info, handle_id

;- set defaults

if not keyword_set( quiet ) then quiet = 0

;- check that handle_id is defined

if n_elements( handle_id ) eq 0 then $
  message, 'FSET must be called before BF can be used'

;- get current state information

handle_value, handle_id, state

;- backup one frame

newframe = state.current - 1
if newframe lt 0 then newframe = state.frames - 1
sf, newframe, /quiet

;- let user know what happened

if not quiet then begin
  print, strcompress( 'Frame ' + string( newframe ) )
endif

finish:

end
