;+
; NAME:
;   DXGET
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Gets IDL variable from a different IDL call level
;
; CALLING SEQUENCE:
;   RESULT = DXGET('NAME')  ; quoted variable name (OR)
;   RESULT = DXGET(NAME)    ; unquoted variable name
;
; DESCRIPTION: 
;
;   DXGET retrieves a variable value from any point in the IDL call
;   stack.  The DXGET and DXSET routines allow any variable at any
;   level to be examined and changed.
;
;   The call level to be examined is determined by the current
;   debugging "focus."  By default this is the deepest level in the
;   call stack -- where the breakpoint occurred.  However, this level
;   can be changed by using the DXUP and DXDOWN procedures.
;
;   If the variable doesn't exist, then an error message is reported.
;
; INPUTS:
;
;   NAME - the name of the variable, either quoted or unquoted.
;
; KEYWORDS:
;
;   LEVEL - the call level to be examined, if not the current
;           debugging focus.
;
; EXAMPLE:
;
;   value = dxget('a')
;
;   Retrieve the value of the variable A from the debugged call level.
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
function dxget, vname, level=level0, status=status
@dxcommon.pro

  status = 0
  if n_params() LT 1 then begin
      USAGE_MESSAGE:
      print, "USAGE:"
      print, "  VALUE = dxget('NAME')   ; named variable"
      print, "  VALUE = dxget( NAME )   ; without quotes"
      return, 0
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
  if name EQ '' then goto, USAGE_MESSAGE
  name = strupcase(name)

  vars = routine_names(variables=level)
  wh = where(name EQ vars, ct)
  if ct EQ 0 then begin
      if pass EQ 2 then begin
          print, 'ERROR: Variable '+name+' does not exist at level '+ $
            strtrim(level, 2)
          return, 0
      endif
      pass = pass + 1
      goto, RETRY_NAME
  endif

  catch, catcherr
  if catcherr NE 0 then begin
      catch, /cancel
      print, 'ERROR: '+name+' could not be set'
      return, 0
  endif

  sz = size(routine_names(name, fetch=level))
  if sz(sz(0)+1) EQ 0 then begin
      print, 'ERROR: '+name+' is undefined'
      return, 0
  endif
  value = routine_names(name, fetch=level)
  status = 1
  return, value
end
