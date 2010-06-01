pro readhal, file, band, wave, resp, title = title

;+
; Purpose:
;     Read a Hal Woolf format spectral response data file.
;
; Usage:
;     READHAL, FILE, BAND, WAVE, RESP, TITLE = TITLE
;
; Input:
;     FILE    filename
;     BAND    band number
;
; Output:
;     WAVE    wavenumber (inverse centimeters)
;     RESP    response (no units)
;
; Optional Keywords:
;     TITLE   on output, the header from the spectral response file
;
; Revised:
;     23-SEP-1996 Liam Gumley, CIMSS/SSEC
;-

openr, lun, file, /get_lun
title = ' '
readf, lun, title, format = '(a)'

while not eof( lun ) do begin

  readf, lun, b, n, wmin, wmax
  resp = fltarr( n )
  readf, lun, resp
  wave = ( findgen( n ) / float( n - 1 ) ) * ( wmax - wmin ) + wmin
  if b eq band then begin
    free_lun, lun
    goto, finish
  endif
  
endwhile

finish:

end
