function bright_m, w, r

;+
; Purpose:
;    Compute brightness temperature given monochromatic Planck radiance.
;
; Usage:
;    T = BRIGHT_M( W, R )
;
; Input:
;    W         Wavelength (microns)
;    R         Planck radiance (Watts per square meter per steradian per micron)
;
; Output:
;    BRIGHT_M  Brightness temperature (Kelvin)
;-

;- Constants are from
;- Cohen, E. R. and B. N. Taylor, 1993: "The Fundamental Physical Constants",
;- Physics Today, August 1993.

h = 6.6260755d-34   ; Planck constant (Joule second)
c = 2.9979246d+8    ; Speed of light in vacuum (meters per second)
k = 1.380658d-23    ; Boltzmann constant (Joules per Kelvin)
c1 = 2.0d0 * h * c^2
c2 = h * c / k

ws = 1.0d-6 * w
return, float( c2 / ( ws * alog( c1 / ( 1.0d6 * r * ws^5 ) + 1.0d ) ) )

end
