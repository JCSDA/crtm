; docformat = 'rst'

;+
; Convert an integer into a string with no trailing/leading spaces and
; delimited with commas every three digits from the right.
;   
; :Returns: string
;
; :Params:
;    n : in, required, type=integer
;       integer to format
;
; :Examples:
;    For example::
;
;       IDL> print, mg_int_format(12345)
;       12,345
;-
function mg_int_format, n
  compile_opt strictarr, hidden
  
  ; handle negative values
  if (n lt 0) then return, '-' + mg_int_format(abs(n))
  
  v = strtrim(n, 2)
  
  ; add some leading spaces to make length divisible by 3
  npad = 3 - strlen(v) mod 3
  if (npad lt 3) then v = strjoin(replicate(' ', npad)) + v
  
  ; convert into a bytarr(3, nGroups)
  v = reform(byte(v), 3, strlen(v) / 3)
  
  ; conver to a strarr(nGroups)
  v = string(v)
  
  ; add commas and return the result
  return, strtrim(strjoin(v, ','), 2)
end
