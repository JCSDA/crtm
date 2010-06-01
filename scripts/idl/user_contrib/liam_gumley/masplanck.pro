function masplanck, t, band, micron = micron

;+
; Purpose:
;    Compute Planc radiance for a MAS IR band.
;
; Usage:
;    RESULT = MASPLANCK( T, BAND )
;
; Input:
;    T       Brightness temperature (Kelvin)
;    BAND    MAS IR band number (26-50)
;
; Optional Keywords:
;    MICRON  If set, radiance units are
;            Watts per square meter per steradian per micron
;
; Output:
;    R       Radiance (milliWatts per square meter per steradian per wavenumber)
;-

;- check input

loc = where( t le 0.0, count )
if count ge 1 then begin
  message, 'Negative temperature value found - setting to 10.0 K', /continue
  t( loc ) = 10.0
endif

if band lt 26 or band gt 50 then $
  message, 'MODIS IR band number must be in the range 26-50'

;- coefficients updated from INITCF.f on 08-NOV-1996

cwn = [ 3382.7280, 3219.6619, 3055.3538, 2924.8838, 2789.7378, $
        2673.8267, 2565.4993, 2468.4175, 2377.9524, 2293.3669, $
        2215.5767, 2143.6938, 2074.3494, 2011.4028, 1952.4149, $
        1894.2410, 1164.3201, 1023.3481,  949.2105,  908.1420, $
        835.6713,  778.1019,  755.5133,  729.2321,  704.9153 ]
        
ts = [ 0.999058, 0.998995, 0.999125, 0.999143, 0.999254, 0.999336, $
       0.999351, 0.999435, 0.999447, 0.999552, 0.999556, 0.999569, $
       0.999618, 0.999651, 0.999660, 0.999717, 0.999150, 0.999148, $
       0.999413, 0.999445, 0.999602, 0.999711, 0.999712, 0.999684, $
       0.999793 ]
       
ti = [ 0.863662, 0.897961, 0.740580, 0.685433, 0.569402, 0.486726, $
       0.458723, 0.383232, 0.360886, 0.282989, 0.273081, 0.256477, $
       0.221397, 0.196835, 0.186935, 0.151540, 0.300056, 0.269216, $
       0.173490, 0.157717, 0.104556, 0.070867, 0.068426, 0.072357, $
       0.045461 ]

if keyword_set( micron ) then begin

  return, planck_m( 1.0e4 / cwn( band - 26 ), $
    t * ts( band - 26 ) + ti( band - 26 ) )

endif else begin

  return, planc_m( cwn( band - 26 ), $
    t * ts( band - 26 ) + ti( band - 26 ) )

endelse

end
