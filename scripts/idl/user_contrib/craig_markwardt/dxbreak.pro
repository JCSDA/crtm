;+
; NAME:
;   DXBREAK
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Sets an IDL breakpoint
;
; CALLING SEQUENCE:
;   DXBREAK, LINE                        ; current procedure (OR)"
;   DXBREAK, 'PROCNAME', LINE            ; named procedure (OR)"
;   DXBREAK, 'path/procname.pro', LINE   ; procedure path (OR)"
;   DXBREAK, PROCNAME, LINE              ; without quotes"
;
; DESCRIPTION: 
;
;   DXBREAK is a convenience routine for setting IDL breakpoints.
;
;   The benefits over the built-in IDL procedure BREAKPOINT are:
;     * fewer characters to type;
;     * full pathname not required, just a procedure name; and
;     * for breakpoints within the current procedure, the file name 
;       is not needed.
;
;   To clear breakpoints, use either DXCLEAR or BREAKPOINT, /CLEAR.
;
; INPUTS:
;
;   LINE - the line number where the breakpoint is to be set.  This
;          value is required.
;
;   PROCNAME - the procedure name in which the breakpoint is to be
;              set.  Note that IDL requires that a procedure be
;              compiled on disk -- console-compiled or .RUN files
;              cannot have breakpoints set.
;
;              The name is one of:
;                * the procedure or function name;
;                * the full path to the procedure or function; OR
;                * an unquoted procedure or function name.
;
;              DXBREAK will search your path to find the correct
;              procedure.  The first file found will be used.
;
;
; KEYWORDS:
;
;   ONCE - if set, then the breakpoint will only occur once.  The same
;          as the ONCE keyword to BREAKPOINT.
;
;   IS_FUNCTION - if set, and there is an ambiguity between whether
;                 PROCNAME is a procedure or a function, then DXBREAK
;                 will assume that it is a function.
;
; EXAMPLE:
;
;   dxbreak, 'myfunc', 50
;
;   Set breakpoint in MYFUNC at line 50.
;
; SEE ALSO:
;
;   BREAKPOINT, DXCLEAR
;
; MODIFICATION HISTORY:
;   Written, 15 Apr 2000
;   Addition of NOCATCH keyword for internal testing, 21 Sep 2000, CM
;   Made mostly compatible with IDL v4, 21 Sep 2000, CM
;   Added AFTER keyword, 13 Dec 2000, CM
;   Removed AFTER, use _EXTRA instead, 08 Apr 2001, CM
;
;  $Id$
;
;-
; Copyright (C) 2000-2001, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-
pro dxbreak, arg0, arg1, is_function=is_func0, once=once, nocatch=nocatch, $
             _EXTRA=extra

  forward_function routine_info, routine_names
  if NOT keyword_set(nocatch) then begin
      catch, catcherr
      if catcherr NE 0 then begin
          CATCH_HANDLER:
          catch, /cancel
          print, 'ERROR: could not set breakpoint; is it compiled yet?'
          return
      endif
  endif

  if n_params() EQ 0 then begin
      USAGE_MESSAGE:
      print, 'USAGE:'
      print, "  dxbreak, LINE                        ; current procedure (OR)"
      print, "  dxbreak, 'PROCNAME', LINE            ; named procedure (OR)"
      print, "  dxbreak, 'path/procname.pro', LINE   ; procedure path (OR)"
      print, "  dxbreak, PROCNAME, LINE              ; without quotes"
      return
  endif

  line = -1
  proc = '' 
  file = ''
  resolved = 0
  ;; Parse parameters... either LINE alone or PROC+LINE
  if n_params() EQ 1 then begin
      ;; Line number only specified

      if n_elements(arg0) EQ 0 then goto, USAGE_MESSAGE
      arg = arg0
      
      ;; Type checking only
      LINE_CHECK:
      sz = size(arg)
      tp = sz(sz(0)+1)
      if tp EQ 0 OR (tp GE 6 AND tp LE 11) then goto, USAGE_MESSAGE
      line = floor(arg(0))
  endif else begin
      ;; File and line number specified
      if n_elements(arg1) EQ 0 then goto, USAGE_MESSAGE
      
      sz = size(arg0)
      tp = sz(sz(0)+1)
      if tp EQ 7 then begin
          ;; String file name specified
          file = arg0(0)
          proc = strupcase(arg0(0))
          if strpos(strlowcase(file), '.pro') GE 0 then resolved = 1 $
          else file = strlowcase(file) + '.pro'
              
          arg = arg1
          goto, LINE_CHECK
      endif else begin
          ;; Variable name is procedure or function name
          level = routine_names(/level)
          proc = routine_names(arg0, arg_name=level-1)
          if n_elements(proc) LT 1 then goto, USAGE_MESSAGE
          file = proc(0)
          proc = proc(0)
          if proc EQ '' then goto, USAGE_MESSAGE
          
          arg = arg1
          goto, LINE_CHECK
      endelse
  endelse

  if line LT 0 then begin
      print, 'ERROR: line number was not specified'
      return
  endif

  ;; Make sure the procedure is compiled
  if file NE '' AND NOT resolved then begin
      is_func = 0
      pass = 0
      RETRY_FIND:
      pass = pass + 1

      ;; Compare to known proc/funcs
      funcs = routine_names(/functions)
      procs = routine_names()
      whf = where(proc EQ funcs, ctf)
      whp = where(proc EQ procs, ctp)
      if ctp GT 0 AND ctf GT 0 then begin
          ;; Both procs and funcs are listed; decide in favor of procs
          ;; unless is_funct0 is set
          if keyword_set(is_funct0) then is_func = 1
      endif else begin
          ;; It's a function
          if ctf GT 0 then is_func = 1
      endelse
      if ctf EQ 0 AND ctp EQ 0 then begin
          ;; If it is found in neither, then try compiling, and then
          ;; go through the process again
          if pass EQ 1 then begin
              p1 = 0 & p2 = 0
              catch, catcherr
              ;; Errors will fall through!!
              if p2 EQ 0 then begin ;; Try as a function
                  p2 = 1
                  resolve_routine, proc, /is_function
              endif
              if p1 EQ 0 then begin ;; Try as a procedure
                  p1 = 1
                  resolve_routine, proc
              endif

              ;; Revert to original error handler
              catcherr = 0
              catch, catcherr
              if catcherr NE 0 then goto, CATCH_HANDLER
              goto, RETRY_FIND
          endif
          print, 'ERROR: '+proc+' is not a valid procedure'
          return
      endif

      ;; Get source code path name
      if double(!version.release) GE 5 then begin
          ;; IDL v5. We can get the filename directly
          path = routine_info(proc, /source, functions=is_func)
          file = path.path
      endif else begin
          ;; IDL v4. We guess that it's the lowercase procname with .pro
          file = strlowcase(proc)+'.pro'
      endelse
      resolved = 1
  endif

  ;; The file is not known yet, so this must be the current procedure
  if file EQ '' then begin
@dxcommon.pro
      dxlreset
      tb = dxptrace(dbtraceback)
      n = n_elements(tb)
      file = (tb(0).path)(0)
      fline = (tb(0).line)(0)
      if fline EQ 0 OR file EQ '' OR file EQ filepath(/terminal) $
        OR file EQ '/dev/tty' then begin
          print, 'ERROR: '+tb(0).name+' is not on disk'
          return
      endif
      proc = (tb(0).name)(0)
      resolved = 1
  endif

  ;; Scan each path element until the file is found; the '' path is
  ;; for the case where we know the path of the file already.
  paths = expand_path(!path, /array, count=pathcount)
  if pathcount EQ 0 then paths = [''] else paths = ['', paths]
  pathcount = pathcount + 1

  ;; Check for existence by opening them.
  get_lun, unit
  for i = 0L, pathcount-1 do begin
      if paths(i) EQ '' then path = file $
      else path = filepath(root=paths(i), file)
      openr, unit, path, error=openerr
      if openerr EQ 0 then goto, READ_SUCCESS
  endfor

  free_lun, unit
  print, 'ERROR: the procedure '+proc+' was not found in your path'
  return

  READ_SUCCESS:

  free_lun, unit
  breakpoint, path, line, /set, once=keyword_set(once), _EXTRA=extra
  print, 'Breakpoint set at: '+strupcase(proc)+'    '+strtrim(line,2)+$
    '  ('+path+')'
end

