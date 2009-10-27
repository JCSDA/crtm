;+
; NAME:
;   DXLRESET
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Reset the current debugging focus level (INTERNAL)
;
; CALLING SEQUENCE:
;   DXLRESET
;
; DESCRIPTION: 
;
;   DXLRESET resets the current debugging focus level, if it has
;   changed.  This routine is internal to the debugging procedures.
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
pro dxlreset, remove=remove0
@dxcommon.pro

  if n_elements(remove0) EQ 0 then remove0 = 1L
  remove = floor(remove0(0)) + 1

  ;; Be sure we are on the same level as last time... otherwise reset
  reset = 1
  n = n_elements(dbtraceback)
  if routine_names(/level)-remove EQ n then begin
      help, calls=newtraceback
      if n_elements(newtraceback)-remove EQ n then $
        if total(newtraceback(remove:*) EQ dbtraceback) EQ n then $
        reset = 0
  endif
  if reset then begin
      dblevel = (routine_names(/level)-remove) > 1
      remove = (routine_names(/level)-dblevel)
      help, calls=newtraceback
      dbtraceback = newtraceback(remove:*)
  endif
  

end
