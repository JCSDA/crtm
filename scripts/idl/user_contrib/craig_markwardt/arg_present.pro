;+
; NAME:
;   ARG_PRESENT
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Determine whether output parameter has been passed (IDL4 compatibility)
;
; CALLING SEQUENCE:
;   PRESENT = ARG_PRESENT(ARG)
;
; DESCRIPTION: 
;
;   ARG_PRESENT tests whether an argument to a function or procedure
;   can be used as an output parameter.  The behavior of this function
;   is identical to that of the built-in ARG_PRESENT function in IDL
;   version 5 or greater, and is meant to give the same functionality
;   to programs in IDL 4.
;
;   An IDL procedure or function can use ARG_PRESENT to decide whether
;   the value of a positional or keyword parameter will be returned to
;   the calling procedure.  Generally, if the caller did not pass the
;   parameter then there is no need to compute the value to be
;   returned.
;
;   To be a valid output parameter, the caller must have passed a
;   named variable into which the result is stored.  If the caller
;   passed the parameter by value (e.g., an expression or a
;   subscripted array) the value cannot be returned and ARG_PRESENT
;   returns 0.
;
; INPUTS:
;
;   ARG - the parameter to be tested.  It can be either a positional
;         or a keyword parameter.  Passing a normal local variable
;         (i.e., not a passed parameter) will cause ARG_PRESENT to
;         return zero.
;
; RETURNS:
;
;   Returns a value of 1 if ARG is a valid output parameter, and a
;   value of 0 otherwise.
;
;
; EXAMPLE:
;
;   Consider the following procedure:
;      PRO TESTARG, ARG1
;        print, ARG_PRESENT(ARG1)
;      END
;
;   This procedure will print 1 when an ARG1 can be used as an output
;   parameter.  Here are some examples of the results of TESTARG.
;
;      IDL> testarg
;             0
;      IDL> testarg, x      
;             1
;      IDL> testarg, findgen(10)
;             0
;   
;   In the first case, no argument is passed, so ARG1 cannot be a
;   return variable.  In the second case, X is undefined, but it is
;   still a legal named variable capable of receiving an output
;   parameter.  In the third case, FINDGEN(10) is an expression which
;   cannot receive an output parameter.
;
; SEE ALSO:
;
;   ARG_PRESENT in IDL version 5
;
; MODIFICATION HISTORY:
;   Written, CM, 13 May 2000
;   Small documentation and bug fixes, CM, 04 Jul 2000
;
;-
; Copyright (C) 2000, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-
forward_function arg_present, arg_present1
function arg_present, arg, nocatch=nocatch

  ;; Exception handling - default is to return 0
  catcherr = 0
  if NOT keyword_set(nocatch) then catch, catcherr
  if catcherr NE 0 then begin
      catch, /cancel
      return, 0
  endif

  ;; Get current call level
  forward_function routine_names
  lev = routine_names(/level)
  if lev LT 3 then return, 0

  ;; Extract name of the argument
  a1 = routine_names(arg, arg_name=lev-2)

  ;; If the value is anything but a non-empty string, return 0
  if n_elements(a1) EQ 0 then return, 0
  sz = size(a1)
  if sz(sz(0)+1) NE 7 then return, 0
  if strtrim(a1(0),2) EQ '' then return, 0

  return, 1
end
