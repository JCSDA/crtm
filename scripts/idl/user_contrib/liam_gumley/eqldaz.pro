pro eqldaz, eth, eph, th, phi, xdeg, dist, az

;+
; Purpose:
;    Compute the distance and azimuth from an initial lat/lon to
;    a set of arbitrary lat/lons on the Earth's surface.
;    Latitude is assumed to range from -90.0 degrees at the South Pole
;    to +90.0 degrees at the North Pole.
;    Longitude is assumed to range from -180.0 degrees West to
;    +180.0 degrees East, with Greenwich at 0.0 degrees.
;
; Usage:
;    EQLDAZ, ETH, EPH, TH, PHI, XDEG, DIST, AZ
;
; Input:
;    ETH     Initial latitude (degrees)
;    EPH     Initial longitude (degrees)
;    TH      Array of arbitrary latitudes (degrees)
;    PHI     Array of arbitrary longitudes (degrees)
;
; Output:
;    XDEG    Distance from initial point to each arbitrary point (degrees)
;    DIST    Distance from initial point to each arbitrary point (kilometers)
;    AZ      Azimuth from initial point to each arbitrary point (degrees,
;            clockwise from North)
;
; Revised:
;    23-OCT-1996 Liam Gumley, CIMSS/SSEC
;-

;- check input parameters

if n_params() ne 7 then $
  message, 'Usage is EQLDAZ, ETH, EPH, TH, PHI, XDEG, DIST, AZ'
  
if n_elements( eth ) ne 1 or n_elements( eph ) ne 1 then begin
  print, 'ETH and EPH must each have one element only'
  message, 'Usage is EQLDAZ, ETH, EPH, TH, PHI, XDEG, DIST, AZ'
endif

if n_elements( th ) ne n_elements( phi ) then begin
  print, 'TH and PHI must have the same number of elements'
  message, 'Usage is EQLDAZ, ETH, EPH, TH, PHI, XDEG, DIST, AZ'
endif

;- define constants

radco = 1.745329251994329d-02
degco = 57.29577951308232d
flattn = 1.d0/298.257d0
onflat = 1.d0 - flattn
ellip0 = 1.001119d0
ellip1 = 0.001687d0
degkm = 111.19504d0
pio2 = radco*90.d0

;- calculate equidistant latitude factor

fltfac = onflat*sqrt(onflat)
thk = eth*radco
phk = eph*radco

;- convert source geographic latitude to equidistant latitude

thg = thk
loc = where(abs(eth) ne 90.d0, count)
if count ge 1 then thg(loc) = atan(fltfac*tan(thk(loc)))

;- convert to colatitude

thg = pio2 - thg

;- calculate spherical trig quantities

d = sin(phk)
e = cos(phk) 
f = sin(thg) 
a = e*f
b = d*f
c = cos(thg)
g = c*e
h = c*d
e = -e
f = -f

;- calculate distance and azimuth from each position to source

thc = th*radco
phc = phi*radco
thg = thc

loc = where(abs(th) ne 90.0d0, count)
if count ge 1 then thg(loc) = atan(fltfac*tan(thc(loc)))

thg = pio2 - thg
d1 = sin(phc)
e1 = cos(phc)
f1 = sin(thg)
a1 = f1*e1
b1 = d1*f1
c1 = cos(thg)

;- calculate distance in radians

xdeg = acos(a*a1+b*b1+c*c1)
ad = a1 - d
be = b1 - e
ag = a1 - g
bh = b1 - h
ck = c1 - f

;- calculate azimuth in radians

az = atan(ad*ad+be*be+c1*c1-2.d0,ag*ag+bh*bh+ck*ck-2.d0)

;- calculate quantities for great-ellipse correction (brown (36))

bh = sin(acos(-e*sin(az)))

;- make great-ellipse correction and convert to degrees

xdeg = xdeg * degco * (ellip0 - ellip1*bh*bh)
az = degco * az

;- calculate arc distance in km

dist = degkm * xdeg

end
