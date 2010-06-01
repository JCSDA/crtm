pro mas_read_hdf, file, band, $
  image, hour, minute, second, scan, year, month, day, $
  record = record, nscans = nscans

;+
; PURPOSE:
;    To read MAS image and engineering data for a selected band
;    from a HDF Level 1B file.
;
; USAGE:
;      $
;      IMAGE, HOUR, MINUTE, SECOND, SCAN, YEAR, MONTH, DAY
;
; INPUT:
;    FILE	     Name of HDF file
;    BAND	     Band number to read (1-50)
;
; OPTIONAL KEYWORDS:
;    RECORD      A two element vector containing the first and last record numbers
;                to be read, e.g. [2000,3000] (default is to read all records).
;    NSCANS      On output, contains the total number of scans.
;
; OUTPUT:
;    IMAGE       MAS radiance (Watts/meter2/steradian/micron)
;    HOUR        GPS hours (note GPS time is corrected for the 2 second offset)
;    MINUTE      GPS minutes
;    SECOND      GPS seconds
;    SCAN        Scan line number
;    YEAR        Last two digits of year
;    MONTH       Month of year
;    DAY         Day of month
;
; REVISED:
;    16-APR-1997 Liam Gumley, CIMSS/SSEC
;                Added error checking for RECORD keyword.
;                Added NSCANS keyword.
;-

;- get scanline counter (and hence number of scanlines)

scan = 0B
sds_read, file, scan, sds = 'ScanLineCounter', /read_all
ny = n_elements( scan )
nscans = ny

;- check record keyword

rec1 = 0
rec2 = ny - 1
if keyword_set( record ) then begin
  if n_elements( record ) ne 2 then $
    message, 'Keyword RECORD must be an array with 2 elements, e.g. [0,1000]'
  rec1 = record( 0 ) > 0 < ( ny - 100 )
  rec2 = ( rec1 + 99 ) > record( 1 ) < ( ny - 1 )
endif
start = [ rec1 ]
count = [ rec2 - rec1 + 1 ]
ny = count( 0 )
scan = scan( rec1 : rec2 )

;- get time and date

time = 0B
sds_read, file, time, sds = 'GpsTime', start = start, count = count
hour = time / (10L^6)
minute = time /(10L^4) mod 100L
second = time /(10L^2) mod 100L

date = 0B
sds_read, file, date, sds = 'YearMonthDay', start = start, count = count
year = date( ny / 2 ) /(10L^4) mod 100L
month = date( ny / 2 ) /(10L^2) mod 100L
day = date( ny / 2 ) mod 100L
if year gt 90 then begin
  year = year + 1900L
endif else begin
  year = year + 2000L
endelse

;- read single band from HDF file

image = 0B
sds_read, file, image, sds = 'CalibratedData', scale = scale, $
  start = [ 0, band - 1, start( 0 ) ], count = [ 716, 1, count( 0 ) ]
image = reform( temporary( image ), 716, ny ) * scale( band - 1 )

end
