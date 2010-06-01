pro getchan, file, band, data

;+
;Purpose:
;   Read a single band of data (including header) from
;   a MAS 50 channel UW intermediate format file. GETCHAN
;   automagically handles the format created directly by
;   readtape5.f (slow), or the band-interleaved format
;   created by mas_interleave_uw.pro (fast).
;
;Usage:
;   GETCHAN, FILE, BAND, DATA
;
;Input:
;   file    Name of file to read
;   band    Band number to read (1-50)
;
;Output:
;   data    Array containing header and image data
;-

;- check band number

if band lt 1 or band gt 50 then $
  message, 'MAS band number must be in the range 1 to 50'

;- open file and get number of complete scans

openr, lun, file, /get_lun
result = fstat( lun )
nscans = result.size / 74200L

;- check whether file is band-interleaved by checking the band number

data = intarr( 742, 2 )
readu, lun, data
point_lun, lun, 0L

if data( 15, 0 ) eq data( 15, 1 ) then begin

  ;- read band-interleaved data (fast!)

  data = intarr( 742, nscans )
  point_lun, lun, ( band - 1 ) * 2L * 742L * nscans
  readu, lun, data

endif else begin

  ;- read data as created by readtape5.f (slow!)

  print, '############################################################################'
  print, 'Hint: Transform this dataset with MAS_INTERLEAVE_UW to speed things up. Type'
  print, "  DOC_LIBRARY,'MAS_INTERLEAVE_UW'"
  print, 'at the IDL prompt for help.'
  print, '############################################################################'
  npixels = 25 + 716 + 1
  scan = intarr( npixels, 50 )
  data = intarr( npixels, nscans )
  for i = 0, nscans - 1 do begin
    readu, lun, scan
    data( *, i ) = scan( *, band - 1 )
  endfor

endelse

free_lun, lun

end
