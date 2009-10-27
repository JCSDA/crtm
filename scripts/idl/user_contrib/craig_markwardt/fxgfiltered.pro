;+
; NAME:
;   FXGFILTERED
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Determine if a unit has been opened by FXGOPEN.
;
; MAJOR TOPICS:
;   File I/O, Pipes, URLs, FITS
;
; CALLING SEQUENCE:
;   Q = FXGFILTERED(UNIT)
;
; DESCRIPTION:
;
;   FXGFILTERED is a function which determines whether a UNIT has been
;   opened by FXGOPEN and requires special handling.
;
;   In principle, only 'FXG' procedures should be used to access file
;   units returned by FXGOPEN.  However, if the unit turns out to be a
;   normal file then special treatment is not required.  User programs
;   can use the FXGFILTERED function to find out this information.
;
;   If FXGFILTERED returns 0, then normal file-access procedures (such
;   as READU, WRITEU, CLOSE, and POINT_LUN) can be used.  Otherwise,
;   the 'FXG' routines must be used.
;
; INPUTS:
;
;   UNIT - Any file LUN.
;
; RETURNS:
;   0 - resource is a normal file.
;   1 - resource is not a normal file.
;
; EXAMPLE:
;
;  if fxgfiltered(unit) EQ 0 then begin  
;    ;; If zero then can use standard IDL routines
;    point_lun, unit, position
;    readu, unit, buffer
;    close, unit
;  endif
;
; MODIFICATION HISTORY:
;   Written, 1999, CM
;   Documented, 02 Oct 1999, CM
;   Changed copyright notice, 21 Sep 2000, CM
;
; TODO:
;   * Add more protocols
;   * Make more windows friendly
;
;  $Id$
;
;-
; Copyright (C) 1999-2000, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-
function fxgfiltered, unit

  on_error, 2
  if n_elements(unit) EQ 0 then $
    message, 'ERROR: UNIT is not defined'

@fxfilter.pro
  if unit LT 0 OR unit GE FXFILTER_MAX_LUN then $
    message, 'ERROR: UNIT is not a valid file unit'

  return, filterflag(unit) AND 1
end
