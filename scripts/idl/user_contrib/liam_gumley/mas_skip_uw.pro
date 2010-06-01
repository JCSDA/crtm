pro mas_skip_uw, in_file, out_file

;+
;Purpose:
;   Pick out only the even numbered scans from a
;   MAS UW intermediate format file.
;
;Usage:
;   MAS_SKIP_UW, IN_FILE, OUT_FILE
;-

;- open files

openr, in_lun, in_file, /get_lun
openw, out_lun, out_file, /get_lun

;- read every line, and write those lines where scan number is even

data = intarr( 742, 50 )
while not eof( in_lun ) do begin
  readu, in_lun, data
  scan = long( data( 2 : 3, 0 ), 0 )
  if scan mod 2 eq 0 then writeu, out_lun, data
endwhile

;- close files

free_lun, in_lun
free_lun, out_lun

end
