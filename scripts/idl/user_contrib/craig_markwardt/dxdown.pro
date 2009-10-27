;+
; NAME:
;   DXDOWN
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Move the debugging focus deeper down the IDL call stack
;
; CALLING SEQUENCE:
;   DXDOWN [ , NLEVELS ]
;
; DESCRIPTION: 
;
;   DXDOWN moves the debugging "focus" deeper down the IDL call stack.
;   By using this procedure and DXUP, one can navigate up and down an
;   existing call stack, and examine and set variables at various
;   levels.
;
;   While IDL always keeps the command line at the deepest call level
;   (i.e., where the breakpoint occurred), DXDOWN and its related
;   debugging procedures maintain a separate notion of which part of
;   the call stack they are examining -- the debugging "focus."
;
;   DXDOWN moves the debugging focus deeper by at least one level, but
;   never beyond the deepest level.
;
; INPUTS:
;
;   NLEVELS - option number of levels to move.  Default (and minimum)
;             value is 1.
;
; EXAMPLE:
;
;   dxdown
;
;   Move the debugging focus down one level.
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
pro dxdown, nlevels0
@dxcommon.pro

  ;; Be sure we are on the same level as last time... otherwise reset
  dxlreset
  
  if n_elements(nlevels0) EQ 0 then nlevels0 = 1L
  nlevels = floor(nlevels0(0)) > 1

  maxlev = routine_names(/level)-1
  if (dblevel + nlevels) GT maxlev then $
    print, 'WARNING: lowermost level is '+strtrim(maxlev, 2)
  dblevel = (dblevel + nlevels) < maxlev
  dxplevel, /current

end
