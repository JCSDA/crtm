; docformat = 'rst'

;+
; Raises an error if the given condition is not met. Uses logical_predicate
; to determine truth of condition: so zero or null values are false, anything
; else is true. Be careful of conditions like::
;     
;    assert, not file_test(filename)
;
; :Params: 
;    condition : in, required, type=boolean
;       condition to assert
;    msg : in, optional, type=string, default="Assertion failed"
;       message to throw if condition is not met
;-
pro assert, condition, msg
  compile_opt strictarr, logical_predicate, hidden
  on_error, 2

  if (~condition) then message, n_elements(msg) eq 0 ? 'Assertion failed' : msg
end
