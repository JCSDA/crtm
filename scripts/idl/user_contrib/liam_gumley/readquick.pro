pro readquick, file, equalize = equalize, reverse = reverse, hisfile = hisfile

;+
; Purpose:
;    Display MAS 50 channel intermediate format quicklook images created by 
;    readtape5.f in a scrolling window. The GPS time code is displayed
;    at 2 minute intervals to the right of the image data. A narrow
;    bar is displayed to the right of the image whenever the aircraft
;    roll exceeds 3 degrees.
;
; Usage:
;    READQUICK, FILE
;
; Input:
;    FILE         Name of quicklook image to read (e.g. '970202.ch02')
;
; Optional Keywords:
;    /EQUALIZE    If set, equalize the image histogram
;                 (image is scaled into 0-255 range).
;    /REVERSE     If set, will reverse color table (useful for IR bands).
;    HISFILE      Name of HIS spectrum file. Tickmarks and timecodes will
;                 be displayed for each HIS spectrum.
;
; Revised:
;    06-MAR-1997 Liam Gumley, CIMSS/SSEC
;    (liam.gumley@ssec.wisc.edu)
;
; Examples:
;
;; Display band 2 with histogram equalization
; readquick, '970202.ch02', /equalize
;-

;- create MAS record structure

mas_record = { mas_record, $
  status   : 0, $
  offset   : 0, $
  scanline : 0L, $
  date     : 0L, $
  bb1temp  : 0, $
  bb2temp  : 0, $
  scanrate : 0, $
  hours    : 0, $
  minutes  : 0, $
  seconds  : 0, $
  heading  : 0, $
  altitude : 0, $
  gain     : 0, $
  band     : 0, $
  time     : 0L, $
  bb1count : 0, $
  bb2count : 0, $
  roll     : 0, $
  nadirlat : 0L, $
  nadirlon : 0L, $
  video    : intarr( 179 ) }

;- get input MAS file size and read in MAS data

openr, lun, file, /get_lun
info = fstat( lun )
nx = 25L + 179L
ny = long( info.size ) / (  nx * 2L )
mas_data = replicate( mas_record, ny )
readu, lun, mas_data
free_lun, lun

;- check if MAS data needs to be swapped

if ( ( mas_data( 0 ).band lt 1 ) or $
     ( mas_data( 0 ).band gt 50 ) ) then begin
  mas_data = swap_endian( temporary( mas_data ) )
  message, 'Byte swapping MAS data', /info
endif

;- fill any gaps in the MAS data with zero records

scan = 4 * ( mas_data.scanline / 4 )
index = ( scan - scan( 0 ) ) / 4
ny = index( ny - 1 ) + 1
new_data = replicate( mas_record, ny )
new_data( * ).video( * ) = -32767
new_data( index ) = temporary( mas_data )
mas_data = temporary( new_data )

;- extract info and video data in correct orientation

hr = mas_data.hours
mn = mas_data.minutes
sc = mas_data.seconds / 10
roll = mas_data.roll * 0.01
band = mas_data( 0 ).band
bb1c = mas_data.bb1count
image = mas_data.video
mas_data = 0B
if ( band gt 26 ) then $
  image = temporary( image ) - rebin( rotate( bb1c, 1 ), 179, ny, /sample )
image = rotate( rotate( temporary( image ), 7 ), 2 )

;- enhance image

loc = where( image ne -32767 )
minv = min( image( loc ), max = maxv )
if not keyword_set( equalize ) then begin
  image = bytscl( temporary( image ), top = ( !d.n_colors - 1 ) < 255, $
    min = minv, max = maxv )
endif else begin
  image = hist_equal( temporary( image ), minv = minv, maxv = maxv )
endelse
if keyword_set( reverse ) then image = 255B - temporary( image )

;- display scaled image in scrolling window

device, get_screen_size = sz
base = widget_base( title = strcompress( 'MAS Quicklook ' + file ) )
draw = widget_draw( base, xs = nx + 125, ys = ny + 200, $
  x_sc = nx + 125, y_sc = ( ny + 200 ) < ( sz( 1 ) - 100 ), $
  /scroll, retain = 2 )
widget_control, base, /realize
tv, temporary( image ), 0, 100

;- regenerate MAS times based on linear fit of unique non-zero MAS time entries

masline = findgen( ny )
mastime = hr + mn / 60.0 + sc / 3600.0
loc = uniq( mastime )
mastime_uniq = mastime( loc )
masline_uniq = masline( loc )
loc = where( mastime gt 0.0 )
mastime_uniq = mastime( loc )
masline_uniq = masline( loc )
result = poly_fit( masline_uniq, mastime_uniq, 1 )
mastime = masline * result( 1 ) + result( 0 )
hr = fix( mastime )
temp = 60.0 * ( mastime mod 1.0 )
mn = fix( temp )
temp = 60.0 * ( temp mod 1.0 )
sc = fix( temp )

;- plot MAS time tick marks at 2 minute intervals

loc = where( ( mn mod 2 eq 0 ) and ( sc eq 0 or sc eq 1 ), count )
if count ge 1 then begin
  loc = loc( uniq( mn( loc ) ) )
  usersym, [ 0, 5.0 ], [ 0, 0 ]
  plots, 179, loc + 100, /device, psym = 8
endif

;- plot MAS time labels at 2 minute intervals

if count ge 1 then begin
  charsize = !p.charsize
  if charsize lt 0.01 then charsize = 1.0
  label = string( hr( loc ), format = '( i2.2 )' ) + ':' + $
    string( mn( loc ), format = '( i2.2 )' )
  xyouts, 179 + 20, loc + 100 - 5, label, /device, charsize = 1.5 * charsize
endif

;- plot small ticks wherever roll exceeds 3.0 degrees absolute

loc = where( abs( roll ) gt 3.0, count )
if count ge 1 then begin
  usersym, [ 0, 0.5 ], [ 0, 0 ]
  plots, 179 + 5, loc + 100, /device, psym = 8
endif
undefine, roll

print, 'Times are UTC from GPS'
print, 'White bar to right indicates roll > 3 degrees'

if not keyword_set( hisfile ) then goto, finish

;- get HIS time array

get_his_time, hisfile, hour, minute, second
histime = hour + minute / 60.0 + second / 3600.0

;- compute MAS line numbers corresponding to HIS times

hisline = ( histime - result( 0 ) ) / result( 1 )
loc = where( hisline ge 0.0 and hisline le ny, count )
if count lt 1 then message, 'No HIS times found within this MAS image'
hisline = hisline( loc )
hour = hour( loc )
minute = minute( loc )
second = second( loc )

;- plot HIS FOV markers on MAS image

plots, 179 / 2, [ hisline + 100 ], psym = 1, /device

;- plot HIS time labels

label = string( hour( 0 : ( count - 1 ) < 1023 ), format = '( i2.2 )' ) + ':' + $
  string( minute( 0 : ( count - 1 ) < 1023 ), format = '( i2.2 )' ) + ':' + $
  string( second( 0 : ( count - 1 ) < 1023 ), format = '( i2.2 )' )
xyouts, 179 + 90, hisline( 0 : ( count - 1 ) < 1023 ) + 100 - 4, $
  label, /device, charsize = 1.0 * charsize

if count ge 1024 then begin
  label = string( hour( 1024 : * ), format = '( i2.2 )' ) + ':' + $
    string( minute( 1024 : * ), format = '( i2.2 )' ) + ':' + $
    string( second( 1024 : * ), format = '( i2.2 )' )
  xyouts, 179 + 90, hisline( 1024 : * ) + 100 - 4, $
    label, /device, charsize = 1.0 * charsize
endif

finish:

end
