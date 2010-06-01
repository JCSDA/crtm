function findtag, file, string

;+
; Purpose:
;   Return the start byte of the first occurence of
;   a string within a file.
;
; Input:
;   file    name of file
;   string  string to locate within file
;
; Output:
;   Returns the byte location (zero based) within the file of the first
;   character of the specified string. A value of -1 is returned if the
;   string is not found.
;-

;- read the entire data file as a byte array

openr, lun, 'test.dat', /get_lun
info = fstat( lun )
data = bytarr( info.size )
readu, lun, data
free_lun, lun

;- convert string to a byte tag

tag = byte( string )
tagsize = n_elements( tag )
if tagsize lt 1 then goto, finish

;- find first occurence of tag within data

loc = where( data eq tag( 0 ), count )
start = -1
if count lt 1 then goto, finish

for i = 0, count - 1 do begin
  byte1 = loc( i )
  byte2 = byte1 + tagsize - 1
  compare = data( byte1 : byte2 ) - tag
  tmploc = where( compare eq 0B, null )
  if null eq tagsize then begin
    start = byte1
    goto, finish
  endif
endfor

finish:
return, start

end
