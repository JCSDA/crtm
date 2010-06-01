pro imagemap2, image, lat, lon, newimage = newimage, range = range, $
  limit = limit, position = position, isotropic = isotropic, title = title, $
  xoffset = xoffset, yoffset = yoffset, xsize = xsize, ysize = ysize, $
  missing = missing, noborder = noborder, noerase = noerase, lowres = lowres

;+
;PURPOSE:
;   Display an image which has latitude and longitude defined for
;   each pixel on a map projection. If a map projection is not currently
;   defined, then a Mercator map projection is created which corresponds to
;   the lat/lon limits of the image.
;
;CALLING SEQUENCE:
;   IMAGEMAP2, IMAGE, LAT, LON
; 
;INPUT:
;   IMAGE      Array (2D) or vector (1D) of image values
;   LAT        Array or vector of latitudes corresponding to image values
;              (degrees, -90.0 = S, +90.0 = N)
;   LON        Array or vector of longitudes corresponding to image values
;              (degrees, -180.0 = W, +180.0 = E)
;
;OPTIONAL KEYWORDS:
;   NEWIMAGE   Named variable in which resampled image array is returned.
;              Note that this image is always scaled to a BYTE array.
;   RANGE      Range of image values used for brightness scaling, [MIN,MAX]
;              (default is [MIN(IMAGE),MAX(IMAGE)])
;   LIMIT      Limits of map display, [LATMIN,LONMIN,LATMAX,LONMAX]
;              (default is [MIN(LAT),MIN(LON),MAX(LAT),MAX(LON)])
;   POSITION   Normalized coordinates for map display window [X1,Y1,X2,Y2]
;              (default is to let MAP_SET determine the window size)
;              This is useful when used in conjunction with the
;              ESRG BOXPOS procedure. For example,
;              IMAGEMAP, IMAGE, LAT, LON, POS = BOXPOS( /RMARG )
;              will leave room at the right for a COLOR_KEY colorbar.
;   ISOTROPIC  If set, creates an isotropic map projection (default=non-isotropic).
;   TITLE      String variable containing image title (default=no title).
;   XOFFSET    Named variable in which the lower left device X coordinate
;              of the displayed image is returned.
;   YOFFSET    Named variable in which the lower left device Y coordinate
;              of the displayed image is returned.
;   XSIZE      Named variable in which the width of the displayed image
;              is returned (used by devices which have scalable pixels
;              such as Postscript).
;   YSIZE      Named variable in which the height of the displayed image
;              is returned (used by devices which have scalable pixels
;              such as Postscript).
;   MISSING    Byte value to use for missing (unfilled) portions of image
;              (default is zero).
;   NOBORDER   If set, do not draw border around image (default=draw border).
;   NOERASE    If set, do not erase window before creating image (default=erase).
;   LOWRES     If set, draw image in low resolution mode (default=high resolution).
;
;OUTPUT:
;   The resampled image is displayed in the current graphics window
;   in map coordinates. Continental outlines and lat/lon grids may be
;   overlaid with MAP_CONTINENTS AND MAP_GRID.
;
;CREATED:
;   Liam Gumley, CIMSS/SSEC, 26-JUL-1996
;   liam.gumley@ssec.wisc.edu
;   http://cimss.ssec.wisc.edu/~gumley/index.html
;
;REVISED:
;   Liam Gumley, CIMSS/SSEC, 17-SEP-1996
;   Modified to work with Postscript output.
;   Added XOFFSET, YOFFSET, XSIZE, YSIZE keywords.
;   Mercator map projection is now created only if no map projection exists.
;
;   Liam Gumley, CIMSS/SSEC, 15-OCT-1996
;   Added MISSING keyword to set missing values in image.
;   Added NOBORDER keyword.
;
;   Liam Gumley, CIMSS/SSEC, 25-NOV-1996
;   Now uses MISSING keyword properly.
;   Added NOERASE keyword.
;   Added LOWRES keyword (useful for low resolution images, e.g. HIRS, AMSU).
;
;NOTES:
;   (1) Hermann Mannstein (h.mannstein@dlr.de) suggested this IDL method.
;   (2) This has been tested on MAS, AVHRR, GOES, and simulated MODIS data.
;       It will not work well on low resolution data like HIRS or MSU.
;   (3) You might run into problems with data over the poles - I've only
;       tried mid-latitude imagery.
;   (4) This procedure was designed for display purposes *only*.
;       If you use the resampled data for any other purpose, you do so at
;       your own risk.
;   (5) The example takes about 15.6 seconds to execute
;       on a SGI Power Indigo2 (R8000/75MHz) with 256 MB RAM.
;
;EXAMPLE:
;
;; This example is adopted from the ESRG library 'REGRID' procedure.
;; First, create latitude, longitude, and image arrays at 250x200 size.
;;
;c = complex(2,2) + cmplxgen(250,200,/center)/100
;c = c + c^2
;lon = float(c) - 100.0
;lat = 20 + imaginary(c)
;image = sqrt(abs(sin(lon*!pi)*sin(lat*!pi)))^0.3 
;;
;; Resize arrays to simulate 1km resolution imagery.
;;
;lat = congrid(lat,1000,800,interp=1)
;lon = congrid(lon,1000,800,interp=1)
;image = congrid(image,1000,800,interp=1)
;;
;; Resample data to Mercator projection, and overlay coastline and grid
;;
;t0 = systime(1.0)
;imagemap,image,lat,lon
;print,'Elapsed time (sec) = ',systime(1.0)-t0
;map_continents
;map_grid
;-

;- check limit keyword

if keyword_set( limit ) then begin
  if n_elements( limit ) ne 4 then $
    message, 'LIMIT must be a 4 element vector of the form [LATMIN,LONMIN,LATMAX,LONMAX]'
  latmin = limit( 0 )
  lonmin = limit( 1 )
  latmax = limit( 2 )
  lonmax = limit( 3 )
endif else begin
  latmin = min( lat, max = latmax )
  lonmin = min( lon, max = lonmax )
endelse

;- check keywords

if not keyword_set( title ) then title = ' '
if not keyword_set( isotropic ) then isotropic = 0
if not keyword_set( missing ) then missing = 0B
missing = byte( ( missing > 0 ) < ( !d.n_colors - 1 ) )

;- create Mercator map projection if necessary after checking position keyword

if not keyword_set( noerase) then begin
  if keyword_set( position ) then begin
    if n_elements( position ) ne 4 then $
	message, 'POSITION must be a 4 element vector of the form [X1,Y1,X2,Y2]'
    map_set, /mercator, limit = [ latmin, lonmin, latmax, lonmax ], $
	title = title, isotropic = isotropic, position = position, /noborder
  endif else begin
    map_set, /mercator, limit = [ latmin, lonmin, latmax, lonmax ], $
	title = title, isotropic = isotropic, /noborder
  endelse
endif

;- compute scaling range for byte image after checking range keyword

if keyword_set( range ) then begin
  if n_elements( range ) ne 2 then $
    message, 'RANGE must be a 2 element vector of the form [MIN,MAX]'
  imin = range( 0 )
  imax = range( 1 )
endif else begin
  imin = min( image, max = imax )
endelse

;- set number of samples and lines for warped image

ns = !d.x_size
nl = !d.y_size
if ( !d.flags and 1 ) then begin
  ns = 640L
  nl = long( float( ns ) * float( !d.y_size ) / float( !d.x_size ) )
endif

;- create resampled byte image

p = convert_coord( lon, lat, /data, /to_normal )

IF ( NOT KEYWORD_SET( newimage ) ) THEN newimage = replicate( 0B, ns, nl )
newimage( p( 0, * ) * ( ns - 1 ), p( 1, * ) * ( nl - 1 ) ) = $
  bytscl( image, min = imin, max = imax, top = !d.n_colors - 2 ) + 1B

;- extract portion of image which fits within map boundaries

x = !x.window * ns
y = !y.window * nl
newimage = temporary( newimage( x(0):x(1), y(0):y(1) ) )

;- compute image offset and size (device coordinates)

p = convert_coord( [ x(0), x(1) ] / float( ns ), [ y(0), y(1) ] / float( nl ), $
  /normal, /to_device )
xoffset = p(0,0)
yoffset = p(1,0)
xsize = p(0,1) - p(0,0)
ysize = p(1,1) - p(1,0)

;- fill holes in resampled image

nxr = 2
nyr = 2
if keyword_set( lowres ) then begin
  nxr = 5
  nyr = 5
  if ( !d.flags and 1 ) then begin
    nxr = 7
    nyr = 7
  endif
endif
fill = dilate( newimage, replicate( 1, nxr, nyr ), /gray )
loc = where( ( fill ge 1b ) and ( newimage eq 0 ), count )
if count ge 1 then newimage( loc ) = fill( loc )
fill = 0

;- fill remaining undefined areas of image with the missing value

loc = where( newimage eq 0B, count )
if ( count ge 1 ) and ( missing gt 0B) then newimage( loc ) = missing

;- display resampled image

tv, newimage, xoffset, yoffset, xsize = xsize, ysize = ysize

;- draw map borders

if not keyword_set( noborder ) then begin
  plots, [ p(0,0), p(0,1) ], [ p(1,0), p(1,0) ], /device
  plots, [ p(0,1), p(0,1) ], [ p(1,0), p(1,1) ], /device
  plots, [ p(0,1), p(0,0) ], [ p(1,1), p(1,1) ], /device
  plots, [ p(0,0), p(0,0) ], [ p(1,1), p(1,0) ], /device
endif

end
