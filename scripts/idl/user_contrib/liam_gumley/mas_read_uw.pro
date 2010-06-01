pro mas_read_uw, file, band, image, bb1c, bb2c, bb1t, bb2t, headt, $
  hour, minute, second, scan, year, month, day, $
  heading, altitude, lat, lon, record = record, nscans = nscans

;+
; Purpose:
;    To read MAS image and engineering data for a selected band
;    from a UW intermediate format file created by readtape5.f.
;    NOTE: Both full resolution and quicklook formats are supported.
;
; Usage:
;    MAS_READ_UW, FILE, BAND, IMAGE, BB1C, BB2C, BB1T, BB2T, HEADT, $
;      HOUR, MINUTE, SECOND, SCAN, YEAR, MONTH, DAY, $
;      HEADING, ALTITUDE, LAT, LON
;
; Input:
;    FILE      Name of UW intermediate format file
;    BAND      Band number to read (1-50)
;              (If FILE is in quicklook format, BAND is ignored on input,
;              and on output contains the actual band number read from
;              the first scanline of the quicklook data).
;
; Output:
;    IMAGE     MAS image counts, corrected for digital gain and offset
;    BB1C      Black body 1 (ambient) counts
;    BB2C      Black body 2 (warm) counts
;    BB1T      Black body 1 temperatures (Kelvin)
;    BB2T      Black body 2 temperatures (Kelvin)
;    HEADT     Brightness temperatures from head 2 count in band 45 (Kelvin)
;              (If FILE is in quicklook format, HEADT is simply the mean of
;              BB1T and BB2T - use it at your own risk!)
;    HOUR      GPS hours (note GPS time is corrected for the 2 second offset)
;    MINUTE    GPS minutes
;    SECOND    GPS seconds
;    SCAN      Scan line number
;    YEAR      Last two digits of year
;    MONTH     Month of year
;    DAY       Day of month
;    HEADING   Aircraft true heading (degrees, clockwise from North)
;    ALTITUDE  Aircraft altitude (meters)
;    LAT       Aircraft latitude (degrees, South negative, North positive)
;    LON       Aircraft longitude (degrees, West negative, East positive,
;              Greenwich zero)
;
; Optional Keywords:
;    RECORD    A two element vector containing the first and last
;              relative record numbers to be read, e.g. [2000,3000]
;              (default is to read all records)
;    NSCANS    On output, contains the total number of scans.
;
; Revised:
;    23-OCT-1996 Liam Gumley, CIMSS/SSEC
;                Created
;    27-NOV-1996 Liam Gumley, CIMSS/SSEC
;                Fixed problem with decoding scan and date.
;    07-APR-1997 Liam Gumley, CIMSS/SSEC
;                GETCHAN is no longer needed.
;                Data is byte-swapped if required.
;                Full resolution or Quicklook Intermediate format may be read.
;                Added RECORD keyword.
;    16-APR-1997 Liam Gumley, CIMSS/SSEC
;                When RECORD is specified only read as many scans as needed.
;                Added NSCANS keyword
;-

;- check the file type

type = mas_file_type( file )
if type ne 2 and type ne 4 then $
  message, 'File must be UW Intermediate or Quicklook format'
  
;- open file and get number of complete scans

openr, lun, file, /get_lun
result = fstat( lun )
case type of
  2 : begin
    nscans = long( result.size ) / 74200L
    npixels = 716
    nwords = 742
  end
  4 : begin
    nscans = long( result.size ) / 408L
    npixels = 179
    nwords = 204
  end
endcase

;- check scan number to see if data is byte swapped

swap = 0
test = lonarr( 2 )
readu, lun, test
point_lun, lun, 0L
if test( 1 ) lt 0L or test( 1 ) gt 270000L then swap = 1

;- check whether file is band-interleaved by checking the band number

interleave = 0
data = intarr( nwords, 2 )
readu, lun, data
point_lun, lun, 0L
if swap then data = swap_endian( temporary( data ) )
if data( 15, 0 ) eq data( 15, 1 ) then interleave = 1

;- check record keyword

rec1 = 0
rec2 = nscans - 1
if keyword_set( record ) then begin
  if n_elements( record ) ne 2 then $
    message, 'Keyword RECORD must be an array with 2 elements, e.g. [0,1000]'
  rec1 = record( 0 ) > 0
  rec2 = ( rec1 + 99 ) > record( 1 ) < ( nscans - 1 )
endif

;- create MAS record structure

case type of

  2 : mas_record = { status:0, offset:0, scanline:0L, date:0L, $
        bb1temp:0, bb2temp:0, scanrate:0, hours:0, minutes:0, $
        seconds:0, heading:0, altitude:0, gain:0, band:0, time:0L, $
        bb1count:0, bb2count:0, roll:0, nadirlat:0L, nadirlon:0L, $
        video:intarr( npixels ), headtemp:0 }

        
  4 : mas_record = { status:0, offset:0, scanline:0L, date:0L, $
        bb1temp:0, bb2temp:0, scanrate:0, hours:0, minutes:0, $
        seconds:0, heading:0, altitude:0, gain:0, band:0, time:0L, $
        bb1count:0, bb2count:0, roll:0, nadirlat:0L, nadirlon:0L, $
        video:intarr( npixels ) }

endcase

;- create data array

data = replicate( mas_record, rec2 - rec1 + 1 )

;- read data from disk

case type of

  2 : begin
  
    case interleave of
    
      0 : begin
        scan = replicate( mas_record, 50 )
        point_lun, lun, rec1 * 74200L
        for i = 0, rec2 - rec1 do begin
          readu, lun, scan
          data( i ) = scan( band - 1 )
        endfor
      end
      
      1 : begin
        point_lun, lun, ( band - 1 ) * 2L * nwords * nscans + ( rec1 * 2L * nwords )
        readu, lun, data
      end
      
    endcase
    
  end
  
  4 : begin
    point_lun, lun, rec1 * 2L * nwords
    readu, lun, data
  end
    
endcase    

;- swap data if necessary

if swap then data = swap_endian( temporary( data ) )

;- extract band, bbc1, bbc2, bbt1, bbt2, headt, scan

band = data( 0 ).band
bb1c = data.bb1count
bb2c = data.bb2count
bb1t = data.bb1temp * 0.01 + 273.15
bb2t = data.bb2temp * 0.01 + 273.15
if type eq 2 then begin
  headt = data.headtemp * 0.1
endif else begin
  headt = ( bb1t + bb2t ) * 0.5
endelse
scan = data.scanline

;- get time and date

hour = data.hours
minute = data.minutes
second = data.seconds / 10
date = data( 0 ).date
if date eq 96044587 then date = 96110123
year = date / 1000000L
if year gt 90 then begin
  year = year + 1900L
endif else begin
  year = year + 2000L
endelse
doy = date mod 1000L 
jul2day, doy, year, month, day

;- get navigation data

heading = data.heading / 90.0
altitude = float( data.altitude )
lat = data.nadirlat * 1.0e-5
lon = data.nadirlon * 1.0e-5

;- extract image

image = data.video
data = 0

end
