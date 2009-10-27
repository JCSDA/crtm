;+
; NAME:
;   DXSET
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Sets IDL variable in a different IDL call level
;
; CALLING SEQUENCE:
;   DXSET, 'NAME', VALUE    ; quoted variable name (OR)
;   DXSET,  NAME,  VALUE    ; unquoted variable name
;
; DESCRIPTION: 
;
;   DXSET sets a variable value at any point in the IDL call stack.
;   The DXGET and DXSET routines allow any variable at any level to be
;   examined and changed.
;
;   The call level to be examined is determined by the current
;   debugging "focus."  By default this is the deepest level in the
;   call stack -- where the breakpoint occurred.  However, this level
;   can be changed by using the DXUP and DXDOWN procedures.
;
;   If the variable doesn't exist, then an error message is reported,
;   and the variable is not set.
;
; INPUTS:
;
;   NAME - the name of the variable, either quoted or unquoted.
;
;   VALUE - the new value of the variable.
;
; KEYWORDS:
;
;   LEVEL - the call level to be examined, if not the current
;           debugging focus.
;
; EXAMPLE:
;
;   dxset, 'a', 5
;
;   Set the value of the variable A to 5 in the currently debugged
;   call level.
;
; SEE ALSO:
;
;   DXGET, DXSET, DXUP, DXDOWN
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
pro dxset, vname, value, level=level0
@dxcommon.pro

  if n_params() LT 2 then begin
      USAGE_MESSAGE:
      print, "USAGE:"
      print, "  dxset, 'NAME', VALUE   ; named variable"
      print, "  dxset,  NAME,  VALUE   ; without quotes"
      return
  endif

  if n_elements(level0) EQ 0 then level0=dblevel
  level = floor(level0(0))

  pass = 1
  ;; Retrieve variable name
  sz = size(vname)
  if sz(sz(0)+1) EQ 7 then begin
      name = vname(0)
  endif else begin
      RETRY_NAME:
      thislev = routine_names(/level)
      name = routine_names(vname, arg_name=thislev-1)
      if n_elements(name) LT 1 then goto, USAGE_MESSAGE
      name = name(0)
  endelse
  if name EQ '' then begin
      if pass GE 2 then goto, NO_EXIST_ERROR
      goto, USAGE_MESSAGE
  endif
  name = strupcase(name)

  vars = routine_names(variables=level)
  wh = where(name EQ vars, ct)
  if ct EQ 0 then begin
      NO_EXIST_ERROR:
      if pass EQ 2 then begin
          if n_elements(name0) EQ 0 then name0 = name
          print, 'ERROR: Variable '+name0+' does not exist at level '+ $
            strtrim(level, 2)
          return
      endif
      name0 = name
      pass = pass + 1
      goto, RETRY_NAME
  endif

  catch, catcherr
  if catcherr NE 0 then begin
      catch, /cancel
      print, 'ERROR: '+name+' could not be set'
      return
  endif

  dummy = routine_names(name, value, store=level)
  return
end
