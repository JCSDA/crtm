pro sunzen, lat, lon, day, hour, mins, sec, zen, azm

;+
;PURPOSE:
;   To compute the zenith and azimuth angles of the center of the sun, 
;   as seen by an observer on the surface of the Earth.  The effects of 
;   atmospheric refraction are not accounted for in this routine.
;
;
;USAGE:
;   SUNZEN, LAT, LON, DAY, HOUR, MINS, SEC, ZEN, AZM
;
;INPUT:
;   LAT     latitude of observer (degrees, -90=S, +90=N)
;   LON     longitude of observer (degrees, -180=W, +180=E)
;   DAY     day of the year (1-366)
;   HOUR    GMT hours (0-24)
;   MINS    GMT minutes (0-1440)
;   SEC     GMT seconds (0-86400)
;
;OUTPUT:
;   ZEN	solar zenith angle (degrees), defined as the
;      	the angle between the local zenith and the
;      	line joining the observer and the sun
;   AZM	solar azimuth angle (degrees), defined as the
;      	rotation angle (clockwise from North) of a
;           vector from the observer to the sun
;
;REVISED :
;   03-DEC-1996 Liam Gumley, CIMSS/SSEC
;
;REFERENCES:
;   Iqbal, M., 1983:  An Introduction to Solar Radiation.  Academic
;   Press, New York, pp 1-19.
;
;NOTES:
;   The solar declination is estimated with a maximum error of 0.034 deg.
;   The equation of time  is estimated with a maximum error of 0.143 deg.
;   The error in the solar zenith angle due to atmospheric refraction
;   ranges from zero when the sun is directly overhead to about 0.567
;   degrees when the sun is at the horizon.
;   It should be noted that the sun subtends a finite angle of 32 minutes
;   (about 0.533 degrees) when viewed from the surface of the Earth.
;
;EXAMPLE:
;; NASA Goddard 18 Dec 95
; LAT = 39.0
; LON = -76.8
; DAY = 352
; HOUR = 14
; MINS = 30
; SEC = 15
; SUNZEN, LAT, LON, DAY, HOUR, MINS, SEC, ZEN, AZM
; PRINT, ZEN, AZM
;-

rtd = 180.0 / !pi
dtr = !pi / 180.0

;- day angle (radians), eq 1.2.2 in Iqbal
      
fr = ( hour * 3600.0 + mins * 60.0 + sec ) / 86400.0
da = 2.0 * !pi * ( day + fr - 1.0 ) / 365.0

;- solar declination (radians), eq 1.3.1 in Iqbal

dc = ( 0.006918 - 0.399912 * cos( da ) + $
       0.070257 * sin( da ) - 0.006758 * cos( 2.0 * da ) + $
       0.000907 * sin( 2.0 * da ) - 0.002697 * cos( 3.0 * da ) + $
       0.00148 * sin( 3.0 * da ) )

;- equation of time (hours), eq 1.4.1 in Iqbal

eqnt = ( 0.000075 + 0.001868 * cos( da ) - $
         0.032077 * sin( da ) - 0.014615 * cos( 2.0 * da ) - $
         0.04089 * sin( 2.0 * da ) ) * 229.18 / 60.0

;- local apparent solar time (hours), eq 1.4.2 in Iqbal

st = hour + mins / 60.0 + sec / 3600.0 + ( 4.0 / 60.0 ) * lon + eqnt
loc = where( st lt  0.0, count )
if( count ge 1 ) then st( loc ) = st( loc ) + 24.0
loc = where( st gt 24.0, count )
if( count ge 1 ) then st( loc ) = st( loc ) - 24.0
      
;- hour angle (radians), p. 15 in Iqbal
      
ha = ( 12.0 - st ) * 15.0 * dtr

;- solar zenith angle (degrees), eq 1.5.1 in Iqbal

x = sin( dc ) * sin( dtr * lat ) + cos( dc ) * cos( dtr * lat ) * cos( ha )
x = temporary( x ) < 1.0
x = -1.0 > temporary( x )
zen = rtd * acos( x )

;- solar elevation angle (radians)

el = dtr * ( 90.0 - zen )
           
;- solar azimuth angle (degrees), eq 1.5.2a in Iqbal

x = ( sin( el ) * sin( dtr * lat ) - sin( dc ) ) / ( cos( el ) * cos( dtr * lat ) )
x = temporary( x ) < 1.0
x = -1.0 > temporary( x )
azm = rtd * acos( x )
loc = where( ha lt 0.0, count )
if( count ge 1 ) then azm( loc ) = -1.0 * azm( loc )
azm = 180.0 - temporary( azm )

end
