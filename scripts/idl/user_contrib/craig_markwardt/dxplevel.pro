;+
; NAME:
;   DXPLEVEL
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Print the current call stack (INTERNAL)
;
; CALLING SEQUENCE:
;   DXPLEVEL
;
; DESCRIPTION: 
;
;   DXLRESET prints the current call stack, and highlights the
;   debugging focus level.This routine is internal to the debugging
;   procedures.
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
pro dxplevel, level=level0, current=current
@dxcommon.pro

  if n_elements(level0) EQ 0 then level0=dblevel
  level = floor(level0(0))

  if n_elements(dbtraceback) EQ 0 then return
  sz = size(dbtraceback)
  if sz(sz(0)+1) NE 7 then return
  tb = dxptrace(dbtraceback)
  blanks = string(replicate(32b,80))
  n = n_elements(dbtraceback)
  for i = n-1,0,-1 do begin
      line = string(n-i,format='(I2)') + ' ' + strmid(tb(i).name+blanks,0,15)
      line = line + ' ' + string(tb(i).line, format='(I4)')
      path = tb(i).path
      flen = 47
      if strlen(path) GT flen then path = strmid(path,strlen(path)-flen,flen)
      line = line + ' ' + strmid(path+blanks,0,flen)
      if n-i EQ level then $
        line = ' >> ' + line + ' << ' $
      else $
        line = '    ' + line + '    '
      if (keyword_set(current) AND n-i NE level) EQ 0 then $
        print, line
  endfor
end
