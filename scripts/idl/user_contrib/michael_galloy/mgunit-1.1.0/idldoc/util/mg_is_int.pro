; docformat = 'rst'

;+
; Checks a variable to determine if it is an integer type: byte or one of the 
; signed/unsigned ints, longs, or 64-bit integers. 
; 
; :Returns: 0 or 1
; 
; :Params:
;    n : in, required, type=any
;       variable to check
;
; :Examples:
;    For example::
; 
;       IDL> print, mg_is_int(1)
;                  1
;       IDL> print, mg_is_int('1')
;                  0 
;-
function mg_is_int, n
  compile_opt strictarr
  
  ind = where([1, 2, 3, 12, 13, 14, 15] eq size(n, /type), isInt)
  return, isInt
end
