function planc_m, v, t

;+
; Purpose:
;     Compute monochromatic Planck radiance given wavelength and temperature
;
; Usage:
;     RESULT = PLANC_M( V, T )
;
; Input:
;     V        wavenumber (inverse centimeters)
;     T        temperature (Kelvin)
;
; Output:
;    PLANC_M   Planck radiance (milliWatts per square meter per steradian
;              per wavenumber)
;-

;- Constants are from
;- Cohen, E. R. and B. N. Taylor, 1993: "The Fundamental Physical Constants",
;- Physics Today, August 1993.

h = 6.6260755d-34    ; Planck constant (Joule second)
c = 2.9979246d+8    ; Speed of light in vacuum (meters per second)
k = 1.380658d-23     ; Boltzmann constant (Joules per Kelvin)
c1 = 2.0d0 * h * c^2
c2 = h * c / k

vs = 1.0d2 * v
return, float( 1.0d5 * c1 * vs^3 / ( exp( c2 * vs / t ) - 1.0d0 ) )

end
