; docformat = 'rst'

;+
; Inverse hyperbolic tangent.
;
; :Returns:
;    float, double, complex, or double complex depending on the input 
;
; :Params:
;    z : in, required, type=numeric
;       input
;-
function mg_atanh, z
  compile_opt strictarr

  return, alog((1 + z) / (1 - z)) / 2.0
end
