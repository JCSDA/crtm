function planck_m, w, t

;+
; Purpose:
;     Compute monochromatic Planck radiance given wavelength and temperature
;     (radiance units of Watts per square meter per steradian per micron).
;
; Usage:
;     RESULT = PLANCK_M( W, T )
;
; Input:
;     W         wavelength (microns)
;     T         temperature (Kelvin)
;
; Output:
;     PLANCK_M  Planck radiance (Watts per square meter per steradian per micron)
;-

;- Constants are from
;- Cohen, E. R. and B. N. Taylor, 1993: "The Fundamental Physical Constants",
;- Physics Today, August 1993.

h = 6.6260755d-34   ; Planck constant (Joule second)
c = 2.9979246d+8    ; Speed of light in vacuum (meters per second)
k = 1.380658d-23    ; Boltzmann constant (Joules per Kelvin)
c1 = 2.0d0 * h * c * c
c2 = h * c / k

ws = 1.0d-6 * w
return, float( 1.0d-6 * ( c1 / ws^5 ) * 1.0d+0 / ( exp( c2 / ( ws * t ) ) - 1.0d+0 ) )

end
