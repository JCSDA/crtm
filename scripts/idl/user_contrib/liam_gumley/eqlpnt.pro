pro eqlpnt, eth, eph, dist, az, th, phi

;+
; Purpose:
;    Compute the lat/lon at a set of arbitrary points defined by 
;    distance and azimuth from an initial lat/lon on the Earth's surface.
;    Latitude is assumed to range from -90.0 degrees at the South pole
;    to +90.0 degrees at the North pole.
;    longitude is assumed to range from -180.0 degrees West to
;    +180.0 degrees East, with Greenwich at 0.0 degrees.
;
; Usage:
;    EQLPNT, ETH, EPH, DIST, AZ, TH, PHI
;
; Input:
;    ETH     initial latitude (degrees)
;    EPH     initial longitude (degrees)
;    DIST    array of distances from initial point to a set of
;            arbitrary points (kilometers)
;    AZ      array of azimuths from initial point to a set of
;            arbitrary points (degrees, clockwise from north)
;
; Output:
;    TH 	 array of latitudes at arbitrary points (degrees)
;    PHI	 array of longitudes at arbitrary points (degrees)
;
; Revised:
;    23-OCT-1996 Liam Gumley, CIMSS/SSEC
;-

;- check input parameters

if n_params() ne 6 then $
  message, 'Usage is EQLPNT, ETH, EPH, DIST, AZ, TH, PHI'
  
if n_elements( eth ) ne 1 or n_elements( eph ) ne 1 then begin
  print, 'ETH and EPH must each have one element only'
  message, 'Usage is EQLPNT, ETH, EPH, DIST, AZ, TH, PHI'
endif

if n_elements( dist ) ne n_elements( az ) then begin
  print, 'DIST and AZ must have the same number of elements'
  message, 'Usage is EQLPNT, ETH, EPH, DIST, AZ, TH, PHI'
endif

;- define constants

radco = 1.745329251994329d-02
degco = 57.29577951308232d
flattn = 1.d0/298.257d0
onflat = 1.d0 - flattn
ellip0 = 1.001119d0
ellip1 = 0.001687d0
degkm = 111.19504d0
degkmi = 1.d0/degkm
pio2=radco*90.d0

;- calculate equidistant latitude factor
 
fltfac = onflat*sqrt(onflat)

;- convert to colatitude

thk = eth*radco
phk = eph*radco

; convert azimuth to radians

azim = radco * az

;- convert source geographic latitude to equidistant latitude

thg = thk
loc = where(abs(eth) ne 90.d0, count)
if count ge 1 then thg(loc) = atan(fltfac*tan(thk(loc)))

;- convert to colatitude
 
thg = pio2 - thg

;- calculate quantities for great-ellipse correction
 
bh = sin(acos(cos(phk)*sin(azim)))

;- convert distance to radians including great-ellipse correction
 
delta = (radco * dist*degkmi)/(ellip0 - ellip1*bh*bh)

;- calculate trig quantities

sd = sin(delta)
sz = sin(azim)
sa = sin(thg)
cz = cos(azim)
ca = cos(thg)
cd = cos(delta)

;- calculate colatitude of new point

th = acos(sa*sd*cz+cd*ca)

;- calculate latitude difference
 
phi = atan((sd*sz)/(sa*cd-sd*cz*ca))

;- convert to longitude of new point in decimal degrees
 
phi = (phi * degco) + eph
loc = where(phi ge 360.d0, count)
if count ge 1 then phi(loc) = phi(loc) - 360.d0
loc = where(phi le -360.d0, count)
if count ge 1 then phi(loc) = phi(loc) + 360.d0

;- convert to latitude in decimal degrees
 
th = pio2 - th
sd = th * degco

;- convert from equidistant latitude to geographic latitude

loc = where(abs(sd) ne 90.d0, count)
if count ge 1 then th(loc) = atan(tan(th(loc))/fltfac)
th = th * degco

end
