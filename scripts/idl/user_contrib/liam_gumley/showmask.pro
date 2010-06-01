pro showmask, imagefile, maskfile, range = range

;+
;PURPOSE:
;    Display results from the UW MAS cloud mask algorithm.
;
;CALLING SEQUENCE:
;    SHOWMASK, IMAGEFILE, MASKFILE, RANGE = RANGE
;
;INPUT:
;    IMAGEFILE    Name of MAS image flat file
;                 (LONG integers scaled by 100, 716 pixels per record)
;    MASKFILE     Name of cloud mask flat file
;                 (BYTES, 6 bytes per pixel, 716 pixels per record)
;
;OPTIONAL KEYWORDS:
;    RANGE        Two element vector indicating physical range over which
;                 to map the color scale.
;
;NOTES:
;    (1) This procedure requires the IDL Frame Tools package, available from
;        http://cimss.ssec.wisc.edu/~gumley/frame.html
;    (2) This procedure must be run in 8 bit mode. To make sure, type
;        DEVICE,PSEUDO=8
;        as soon as you start IDL.
;
;EXAMPLE:
;
;; The following data files are part of the MAS cloud mask delivered to GSFC.
;; mas95013.m45 is MAS band 45 brightness temperature (716x1520 pixels).
;; CM95013.TST  is the corresponding output from the cloud mask.
;; The brightness temperatures will be scaled over the range 285 to 295 K.
;
;showmask, 'mas95013.m45', 'CM95013.TST', range = [ 285, 295 ]
;
;REVISED:
;    24-JULY-1996 Liam Gumley, CIMSS/SSEC
;-

;- read MAS image file
;- (716 pixels per record, each pixel is a long integer scaled up by 100)

openr, lun, imagefile, /get_lun
result = fstat( lun )
nbytes = result.size
nx = 716L
ny = nbytes / 4L / nx
image = lonarr( nx, ny )
readu, lun, image
free_lun, lun

;- remove image scaling

image = image * 0.01

;- read cloud mask file
;- (716 pixels per record, each pixel has 6 bytes of cloud mask data)
;- (first record is a header, followed by ny-2 records of cloud mask)
;- (only ny-2 records are output because of the 3x3 pixel tests)

openr, lun, maskfile, /get_lun
mask = bytarr( 6, nx, ny - 1 )
readu, lun, mask
free_lun, lun

;- set up graphics window and color table

fset, frames = 5, xs = nx, ys = ny, x_sc = 716, y_sc = 716
hicolor = byte( !d.n_colors - 1 - 16 )
colors, start = hicolor
magenta = hicolor + 1B
red = hicolor + 5B
green = hicolor + 4B
blue = hicolor + 6B
tvlct, r, g, b, /get

;- convert image to scaled bytes and display in first frame

if n_elements( range ) eq 0 then begin
  imin = min( image, max = imax )
  range = [ imin, imax ]
endif
image = bytscl( image, min = range( 0 ), max = range( 1 ), top = hicolor - 1B )
tv, image

;- select first byte for each mask pixel, and map it into a 2D array the same
;- size as the image array

mask1 = bytarr( nx, ny )
mask1( *, 1 : ny - 2 ) = mask( 0, *, 1 : * )

;- shift right by 1 bit and mask out the 6 high order bits

mask1 = ishft( mask1, -1 ) and 3B

;- display cloud mask results in separate frames

colors = [ magenta, red, green, blue ]
for index = 0B, 3B, 1B do begin
  loc = where( mask1 eq index, count )
  newimage = image
  if count ge 1 then newimage( loc ) = colors( index )
  af & tvlct, r, g, b & tv, newimage
endfor

;- let user know what the colors mean and then loop through the frames

print, '-------------------------------------------------------'
print, 'Image is MAS band 45 (11 micron) brightness temperature'
print, 'magenta => cloud'
print, 'red     => 66% prob. clear'
print, 'green   => 95% prob. clear'
print, 'blue    => 99% prob. clear'
print, '-------------------------------------------------------'
lf, delay = 1.0

end

