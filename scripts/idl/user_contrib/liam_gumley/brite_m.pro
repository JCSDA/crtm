function brite_m, v, r

;+
; Purpose:
;    Compute brightness temperature given monochromatic Planck radiance.
;
; Usage:
;    T = BRITE_M( V, R )
;
; Input:
;    V         Wavenumber (inverse centimeters)
;    R         Planck radiance (milliWatts per square meter per steradian
;              per wavenumber)
; Output:
;    BRITE_M   Brightness temperature (Kelvin)
;-

;- Constants are from
;- Cohen, E. R. and B. N. Taylor, 1993: "The Fundamental Physical Constants",
;- Physics Today, August 1993.

h = 6.6260755d-34   ; Planck constant (Joule second)
c = 2.9979246d+8    ; Speed of light in vacuum (meters per second)
k = 1.380658d-23    ; Boltzmann constant (Joules per Kelvin)
c1 = 2.0d0 * h * c^2
c2 = h * c / k

vs = 1.0d2 * v
return, float( c2 * vs / alog( c1 * vs^3 / ( 1.0d-5 * r ) + 1.0d0 ) )

end
