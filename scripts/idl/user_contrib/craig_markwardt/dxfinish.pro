;+
; NAME:
;   DXFINISH
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Sets an IDL breakpoint to stop upon return of the current procedure
;
; CALLING SEQUENCE:
;   DXFINISH [ , NLEVELS ]
;
; DESCRIPTION: 
;
;   DXFINISH is a convenience routine for setting IDL breakpoints.
;
;   DXFINISH sets a breakpoint so that when the current procedure
;   finishes, execution will stop.  Often when debugging one wants to
;   let the current procedure complete but stop at the next level.
;   DXFINISH does exactly that.
;
;   DXFINISH examines the state of the current IDL call stack,
;   determines at what point the current procedure will return, and
;   sets a breakpoint there.  Note that the procedure in which the
;   breakpoint is set must be compiled and on disk.
;
;   By default the breakpoint is set with the ONCE keyword.
;
; INPUTS:
;
;   NLEVELS - Number of call levels up to set breakpoint.  Default is
;             1.
;
; KEYWORDS:
;
;   ONCE - if set, then the breakpoint will only occur once.  Default
;          value is SET, so ONCE=0 must be passed explicitly to
;          disable this function.
;
; EXAMPLE:
;
;   dxfinish
;
;   Set breakpoint in calling procedure.
;
; SEE ALSO:
;
;   BREAKPOINT, DXBREAK, DXCLEAR
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
pro dxfinish, nlevels0, once=once

@dxcommon.pro
  ;; Make sure we know what level we are in the call stack 
  dxlreset
  tb = dxptrace(dbtraceback)
  if n_elements(tb) LT 2 then begin
      print, 'INFO: this procedure will return to the main level'
      return
  endif

  ;; Pull the calling procedure
  if n_elements(nlevels0) EQ 0 then nlevels0 = 1L
  n = n_elements(tb)
  nlevels = floor(nlevels0(0)) > 0 < (n-1)
  path = (tb(nlevels).path)(0)
  name = (tb(nlevels).name)(0)
  fline = (tb(nlevels).line)(0)
  if fline EQ 0 OR name EQ '' OR path EQ '' OR path EQ '/dev/tty' $
    OR path EQ filepath(/terminal) then begin
      print, 'ERROR: calling procedure '+tb(nlevels).name+' is not on disk'
      return
  endif
  line = tb(nlevels).line
  line = line(0)
  if line EQ 0 then begin
      print, 'ERROR: calling procedure is not available'
      return
  endif

  ;; Add one to the line!
  line = line + 1
  if n_elements(once) EQ 0 then once = 1
  breakpoint, path, line, /set, once=keyword_set(once)
  print, 'Breakpoint set at: '+strupcase(name)+'    '+strtrim(line,2)
end
