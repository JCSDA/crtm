;+
; Name:
;	nlines
; Purpose:
;	Return the number of lines in a file
; Usage:
;	nl = nlines(file)
; Inputs:
;	file = file to scan
; Optional Inputs or Keywords:
;	help = flag to print header
; Outputs:
;	nl = number of lines in the file.
; Common blocks:
;	none
; Procedure:
;	Assume ASCII data and read through file.
; Modification history:
;	write, 24 Feb 92, F.K.Knight
;-
function nlines,file,help=help
;
;	=====>> HELP
;
on_error,2
if keyword_set(help) then begin & doc_library,'nlines' & return,0 & endif
;
;	=====>> LOOP THROUGH FILE COUNTING LINES
;
tmp = ' '
nl = 0L
on_ioerror,NOASCII
if n_elements(file) eq 0 then file = pickfile()
openr,lun,file,/get_lun
while not eof(lun) do begin
  readf,lun,tmp
  nl = nl + 1L
  endwhile
close,lun
free_lun,lun
NOASCII:
return,nl
end
