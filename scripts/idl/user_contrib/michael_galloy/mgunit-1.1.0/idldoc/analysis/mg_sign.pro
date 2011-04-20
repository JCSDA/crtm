; docformat = 'rst'

;+
; Return the sign of the values of an array: -1 for negative values, 0 for
; 0 values, 1 for positive values. The dimensionality of the result should be
; exactly the same as the input array.
;
; :Returns:
;    intarr
;
; :Params:
;    x : in, required, type=numeric array
;       input array
;-
function mg_sign, x
  compile_opt strictarr
  
  return, fix(x gt 0) - fix(x lt 0)
end