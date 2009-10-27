;+
; NAME:
;   DXUP
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Move the debugging focus higher up the IDL call stack
;
; CALLING SEQUENCE:
;   DXUP [ , NLEVELS ]
;
; DESCRIPTION: 
;
;   DXUP moves the debugging "focus" higher up the IDL call stack.  By
;   using this procedure and DXDOWN, one can navigate up and down an
;   existing call stack, and examine and set variables at various
;   levels.
;
;   While IDL always keeps the command line at the deepest call level
;   (i.e., where the breakpoint occurred), DXUP and its related
;   debugging procedures maintain a separate notion of which part of
;   the call stack they are examining -- the debugging "focus."
;
;   DXUP moves the debugging focus higher by at least one level, but
;   never beyond the "root" $MAIN$ level.
;
; INPUTS:
;
;   NLEVELS - option number of levels to move.  Default (and minimum)
;             value is 1.
;
; EXAMPLE:
;
;   dxup
;
;   Move the debugging focus up one level.
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
pro dxup, nlevels0
@dxcommon.pro

  ;; Be sure we are on the same level as last time... otherwise reset
  dxlreset
  
  if n_elements(nlevels0) EQ 0 then nlevels0 = 1L
  nlevels = floor(nlevels0(0)) > 1
  if (dblevel - nlevels) LT 1 then $
    print, 'WARNING: uppermost level is 1'
  dblevel = (dblevel - nlevels) > 1
  dxplevel, /current

end
