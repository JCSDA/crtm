; docformat = 'rst'

;+
; Inverse hyperbolic sine.
;
; :Returns:
;    float, double, complex, or double complex depending on the input 
;
; :Params:
;    z : in, required, type=numeric
;       input
;-
function mg_asinh, z
  compile_opt strictarr
  
  return, alog(z + sqrt(1 + z*z))
end