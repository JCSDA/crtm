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
;    arg1 : in, optional, type=string
;       argument for any C format codes in msg
;    arg2 : in, optional, type=string
;       argument for any C format codes in msg
;    arg3 : in, optional, type=string
;       argument for any C format codes in msg
;
; :Keywords:
;    skip : in, optional, type=boolean
;       set to skip test instead of fail
;-
pro assert, condition, msg, arg1, arg2, arg3, skip=skip
  compile_opt strictarr, logical_predicate, hidden
  on_error, 2

  if (~condition) then begin
    if (keyword_set(skip)) then (scope_varfetch('self', level=-1))->skip

    case n_params() of
      0: 
      1: message, 'Assertion failed'
      2: message, msg
      3: message, string(arg1, format='(%"' + msg + '")')
      4: message, string(arg1, arg2, format='(%"' + msg + '")')
      5: message, string(arg1, arg2, arg3, format='(%"' + msg + '")')
    endcase
  endif
end
