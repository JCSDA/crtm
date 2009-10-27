;+
; NAME:
;   DXPRINT
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Perform HELP equivalent at any point in IDL call stack
;
; CALLING SEQUENCE:
;   DXPRINT, X0, X1, ... [, LEVEL=LEVEL, FORMAT=FORMAT ]
;
; DESCRIPTION: 
;
;   DXPRINT prints the values of variables from any level in the IDL
;   call stack.
;
;   The call level to be examined is determined by the current
;   debugging "focus."  By default this is the deepest level in the
;   call stack -- where the breakpoint occurred.  However, this level
;   can be changed by using the DXUP and DXDOWN procedures.
;
; INPUTS:
;
;   Xi - variables to be printed, unquoted.  Non-string expressions
;        are printed, but of course refer to the deepest call level.
;
; KEYWORDS:
;
;   LEVEL - the call level to be examined, if not the current
;           debugging focus.
;
;   FORMAT - format string to be applied to data values.
;
;
; EXAMPLE:
;
;   dxprint, a, b
;
;   Print A and B from the current debugging focus level
;
; SEE ALSO:
;
;   DXUP, DXDOWN, DXHELP, DXPRINT
;
; MODIFICATION HISTORY:
;   Written, 15 Apr 2000
;   Corrected FORMAT statement, 30 Jun 2001
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
pro dxprint, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, format=format, $
             level=level0
@dxcommon.pro

  catch, catcherr
  if catcherr NE 0 then begin
      catch, /cancel
      return
  endif

  dxlreset

  if n_elements(level0) EQ 0 then level0=dblevel
  level = floor(level0(0))

  ;; Temporary values are stored here
  s0 = 0 & s1 = 0 & s2 = 0 & s3 = 0 & s4 = 0 & s5 = 0 & s6 = 0
  s7 = 0 & s8 = 0 & s9 = 0

  cmd = 'print'  ;; This will be the ultimate print command
  lev = routine_names(/level)
  levnames = routine_names(variables=level)

  dxplevel, /current
  if n_params() EQ 0 then return
  for i = 0, n_params()-1 do begin

      ;; Retrieve the value of the variable
      ii = strtrim(i,2)
      name = ''
      if execute('name = routine_names(x'+ii+',arg_name=lev-1)') EQ 1 then $
        begin
          name = name(0)
          ;; Check to be sure there was an actual name
          if name EQ '' then begin
              print, 'WARNING: Argument X'+ii+' is not a variable name'
              goto, FAILED
          endif

          ;; Find the name in the level's variable list
          name = strupcase(name)
          wh = (where(name EQ levnames))(0)
          if wh LT 0 then begin
              print, 'WARNING: Variable '+name+' does not exist at level '+ $
                strtrim(level,2)
              goto, FAILED
          endif

          ;; Check for undefined
          sz = size(routine_names(name, fetch=level))
          if sz(sz(0)+1) EQ 0 then begin
              print, 'WARNING: Variable '+name+' is undefined'
              goto, FAILED
          endif

          ;; Retrieve the value, and place it in an "S" variable
          ss = routine_names(name, fetch=level)
          if execute('s'+ii+' = temporary(ss)') NE 1 then begin
              print, 'WARNING: Could not retrieve value of '+name
              goto, FAILED
          endif
          cmd = cmd + ', s'+ii
      endif else begin
          print, 'WARNING: Argument X'+ii+' could not be found'
          FAILED:
          cmd = cmd + ", ''"
      endelse
  endfor

  ;; Add formatting if needed, and then print
  if n_elements(format) GT 0 then cmd = cmd + ', format=format(0)'
  result = execute(cmd)

  return
end

