;+
; NAME:
;   DXPTRACE
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Parse the current call stack (INTERNAL)
;
; CALLING SEQUENCE:
;   RESULT = DXPTRACE(STRING)
;
; DESCRIPTION: 
;
;   DXPTRACE is a function which parses the call stack, as returned by
;   HELP, /CALL.  It is internal to the debugging routines.
;
; SEE ALSO:
;
;   DXUP, DXDOWN, DXGET, DXSET
;
; MODIFICATION HISTORY:
;   Written, 15 Apr 2000
;
;  $Id$
;
;-
; Copyright (C) 2000, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-
function dxptrace, str, status=status
  status = 0
  if n_elements(str) LE 0 then return, 0
  sz = size(str)
  if sz(sz(0)+1) NE 7 then return, 0

  result = replicate({name:'', path:'', line:0L}, n_elements(str))
  result.name = str
  p1 = strpos(str(*), '<')
  p2 = strpos(str(*), '(')
  for i = 0L, n_elements(str)-1 do begin
      if p1(i) GE 0 then $
        result(i).name = strtrim(strmid(str(i),0,p1(i)),2)
      if p1(i) GE 0 AND p2(i)-p1(i) GT 1 then $
        result(i).path = strmid(str(i),p1(i)+1,p2(i)-p1(i)-1)
      if p2(i) GT 0 then $
        result(i).line = long(strmid(str(i),p2(i)+1,10))
  endfor

  return, result
end
