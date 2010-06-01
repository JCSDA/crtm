function mas_file_type, file

;+
; Purpose:
;    Identify the format of a MAS file. Recognized formats are
;    (1) Exabyte format as recorded in-flight (57344 bytes per block),
;    (2) UW-Intermediate format (74200 bytes per block),
;    (3) HDF Level 1B format,
;    (4) UW-Intermediate quicklook format.
;    An error message will be printed and execution will halt if the file
;    cannot be opened, or the format is not recognized.
;
; Usage:
;    TYPE = MAS_FILE_TYPE( FILE )
;
; Input:
;    FILE             Name of file
;
; Output:
;    MAS_FILE_TYPE    Integer variable identifying file format
;                     1 => Exabyte
;                     2 => UW-Intermediate
;                     3 => HDF
;                     4 => UW Intermediate quicklook format
;
; Revised:
;    30-SEP-1996 Liam Gumley, CIMSS/SSEC
;                Created
;    27-NOV-1996 Liam Gumley, CIMSS/SSEC
;                Fixed problem with decoding scan number from UW format
;    07-APR-1997 Liam Gumley, CIMSS/SSEC
;                Added identification of UW Intermediate quicklook format.
;                Added test for byte-swapped UW Intermediate data types.
;-

;- can the file be opened?

openr, lun, file, /get_lun, error = status
if status ne 0 then message, 'File was not found'
free_lun, lun

;- set return value

result = -1

;- check if file is HDF

if hdf_ishdf( file ) then begin
  result = 3
  return, result
endif

;- check if file is raw Exabyte format (57344 bytes per block)

openr, lun, file, /get_lun
data = bytarr( 1126 )
readu, lun, data
free_lun, lun
if data( 0 ) eq 'A6'XB and data( 1 ) eq 'AF'XB and $
  data( 0 + 1124 ) eq 'A6'XB and data( 1 + 1124 ) eq 'AF'XB then begin
  result = 1
  return, result
endif

;- check if file is UW intermediate format (74200 bytes per block)

openr, lun, file, /get_lun
data = intarr( 746 )
readu, lun, data
free_lun, lun
scan1 = ishft( long( data( 2 ) ), 16 ) + ( data( 3 ) and 65535L )
if scan1 lt 0L or scan1 gt 270000L then begin
  data = swap_endian( temporary( data ) )
  scan1 = ishft( long( data( 2 ) ), 16 ) + ( data( 3 ) and 65535L )
endif
scan2 = ishft( long( data( 2 + 742 ) ), 16 ) + ( data( 3 + 742 ) and 65535L )
if ( scan2 eq scan1 or scan2 eq ( scan1 + 1 ) or scan2 eq ( scan1 + 2 ) ) $
  and scan1 ge 1000 then begin
  result = 2
  return, result
endif

;- check if file is UW Intermediate quicklook format

scanq = ishft( long( data( 2 + 204 ) ), 16 ) + ( data( 3 + 204 ) and 65535L )
if ( scanq eq ( scan1 + 4 ) or scanq eq ( scan1 + 8 ) ) $
  and scan1 ge 1000 then begin
  result = 4
  return, result
endif

;- if file was not identifed, print a message and stop

if result eq -1 then $
  message, 'File is not MAS Exabyte, UW-Intermediate, or HDF format'

return, result

end
