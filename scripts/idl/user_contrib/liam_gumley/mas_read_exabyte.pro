pro mas_read_exabyte, file, band, image, bb1c, bb2c, bb1t, bb2t, headt, $
  hour, minute, second, scan, headband = headband, $
  record = record, nscans = nscans

;+
; Purpose:
;    To read MAS image and engineering data for a selected band
;    from an Exabyte (raw) format file which has been dumped directly
;    from a MAS flight tape.
;
; Usage:
;    MAS_READ_EXABYTE, FILE, BAND, IMAGE, BB1C, BB2C, BB1T, BB2T, HEADT, $
;      HOUR, MINUTE, SECOND, SCAN
;
; Input:
;    FILE      Name of Exabyte format file
;    BAND      Band number to read (1-50)
;
; Optional Keywords:
;    HEADBAND  If set, band number to use for head temperature (default=45)
;    RECORD    A two element vector containing the first and last
;              relative record numbers to be read, e.g. [2000,3000]
;              (default is to read all records)
;    NSCANS    On output, contains the total number of scans.
;
; Output:
;    IMAGE     MAS image counts, corrected for digital gain and offset
;    BB1C      Black body 1 (ambient) counts
;    BB2C      Black body 2 (warm) counts
;    BB1T      Black body 1 temperatures (Kelvin)
;    BB2T      Black body 2 temperatures (Kelvin)
;    HEADT     Brightness temperatures from head 2 count in band 45 (Kelvin)
;    HOUR      GPS hours (note GPS time is corrected for the 2 second offset)
;    MINUTE    GPS minutes
;    SECOND    GPS seconds
;    SCAN      Scan line number
;
; Revised:
;    27-NOV-1996 Liam Gumley, CIMSS/SSEC
;                Fixed problem with head temperature computation
;                Added HEADBAND keyword
;    16-APR-1997 Liam Gumley, CIMSS/SSEC
;                Added RECORD and NSCANS keywords
;-

;- open file and get number of records

openr, lun, file, /get_lun
result = fstat( lun )
nrec = result.size / 57344L
nscans = nrec

;- check record keyword

rec1 = 0
rec2 = nscans - 1
if keyword_set( record ) then begin
  if n_elements( record ) ne 2 then $
    message, 'Keyword RECORD must be an array with 2 elements, e.g. [0,1000]'
  rec1 = record( 0 ) > 0
  rec2 = ( rec1 + 99 ) > record( 1 ) < ( nscans - 1 )
endif
nrec = rec2 - rec1 + 1

;- make storage arrays

record = { record, head1 : intarr( 2 ), head2 : lonarr( 2 ), $
  head3 : intarr( 14 ), video : bytarr( 1074 ), status : intarr( 5 ) }
record = replicate( record, 50 )
trailer = bytarr( 1144 )
image = lonarr( 716, nrec )
scan = lonarr( nrec )
bb1c = lonarr( nrec )
bb2c = lonarr( nrec )
bb1t = fltarr( nrec )
bb2t = fltarr( nrec )
headt = fltarr( nrec )
hour = lonarr( nrec )
minute = lonarr( nrec )
second = lonarr( nrec )

;- read requested data

point_lun, lun, rec1 * 57344L

for n = 0, nrec - 1 do begin

  ;- read data
  
  readu, lun, record, trailer

  ;- unpack scanline, bb1c, bb2c

  scan( n ) = record( band - 1 ).head2( 0 )
  bb1c( n ) = record( band - 1 ).head3( 0 ) and 'FFFF'XL
  bb2c( n ) = record( band - 1 ).head3( 1 ) and 'FFFF'XL
  doffset = record( band - 1 ).head3( 9 ) and 'FFFF'XL
  temp = record( band - 1 ).head3( 8 )
  case temp of
     '40'XL : dgain = 16L
     '80'XL : dgain = 8L
    '100'XL : dgain = 4L
    '200'XL : dgain = 2L
    '400'XL : dgain = 1L
    else : dgain = 1L
  endcase
  
  ;- unpack video and apply digital gain and offset correction
  
  input = record( band - 1 ).video
  image( *, n ) = long( mas_unpack( input ) ) * dgain + doffset

  ;- unpack blackbody temperatures to degrees Kelvin
  
  sign = ishft( trailer( 24 ), -7 )
  if not sign then sign = -1.0 
  d1 = ishft( trailer( 24 ), -4 ) and 7B
  d2 = trailer( 24 ) and 15B
  d3 = ishft( trailer( 25 ), -4 ) and 15B
  d4 = trailer( 25 ) and 15B
  bb1t( n ) = sign * ( 10.0 * d1 + 1.0 * d2 + 0.1 * d3 + 0.01 * d4 ) + 273.15

  sign = ishft( trailer( 26 ), -7 )
  if not sign then sign = -1.0 
  d1 = ishft( trailer( 26 ), -4 ) and 7B
  d2 = trailer( 26 ) and 15B
  d3 = ishft( trailer( 27 ), -4 ) and 15B
  d4 = trailer( 27 ) and 15B
  bb2t( n ) = sign * ( 10.0 * d1 + 1.0 * d2 + 0.1 * d3 + 0.01 * d4 ) + 273.15

  ;- compute brightness temperature for head 2 count (default is band 45)
  
  if not keyword_set( headband ) then headband = 45
  r1 = masplanck( bb1t( n ), headband )
  r2 = masplanck( bb2t( n ), headband )
  c1 = record( headband - 1 ).head3( 0 ) and 'FFFF'XL
  c2 = record( headband - 1 ).head3( 1 ) and 'FFFF'XL
  slope = ( r2 - r1 ) / float( c2 - c1 )
  incpt = r1 - slope * c1
  headc = record( headband - 1 ).status( 1 ) and 'FFFF'XL
  rad = slope * headc + incpt
  headt( n ) = masbright( rad, headband )
    
  ;- get GPS time code and apply 2 second offset correction
  
  h = float( string( trailer( 91 : 92 ) ) )
  m = float( string( trailer( 94 : 95 ) ) )
  s = float( string( trailer( 97 : 98 ) ) )
  time = h + m / 60.0 + s / 3600.0
  time = time + 2.0 / 3600.0
  if time gt 24.0 then time = time - 24.0
  int_time, time, h, m, s, fraction
  hour( n ) = h
  minute( n ) = m
  second( n ) = s
  
endfor

finish:
free_lun, lun

end
