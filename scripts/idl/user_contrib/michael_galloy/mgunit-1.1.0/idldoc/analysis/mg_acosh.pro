; docformat = 'rst'

;+
; Inverse hyperbolic cosine.
;
; :Returns:
;    float, double, complex, or double complex depending on the input 
;
; :Params:
;    z : in, required, type=numeric
;       input
;-
function mg_acosh, z
  compile_opt strictarr
  
  return, alog(z + sqrt(z + 1) * sqrt(z - 1))
end