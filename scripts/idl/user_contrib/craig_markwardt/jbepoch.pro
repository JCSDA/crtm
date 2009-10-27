;+
; NAME:
;   JBEPOCH
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Compute Julian Day to/from Julian or Besselian Epoch
;
; CALLING SEQUENCE:
;   EPOCH = JBEPOCH(/B, JDAY)   ;; Julian Day to Besselian Epoch
;   EPOCH = JBEPOCH(/J, JDAY)   ;; Julian Day to Julian Epoch
;
;   JDAY  = JBEPOCH(/B, EPOCH, /TO_DAY)  ;; Besselian Epoch to Julian Day
;   JDAY  = JBEPOCH(/J, EPOCH, /TO_DAY)  ;; Julian Epoch to Julian Day
;
; DESCRIPTION: 
;
;   The function JBEPOCH computes the Julian or Besselian Epoch year
;   number from a given Julian day number.  Epochs of this form are
;   often given in the astronomical literature as B1950.0 or J2000.0,
;   but they can be different.
;
;   Besselian year numbers are measured in tropical years of about
;   365.2422 days.  Julian year numbers are measured in years whose
;   lengths are exactly 365.25 days of 86400 second lengths.  The "/J"
;   or "/B" keywords identify which year numbering system is being
;   used.
;
;   JBEPOCH also computes the inverse transformation, from Julian or
;   Besselian epoch to Julian Day, by specifying the /TO_DAY keyword.
;
;   The computational logic is inspired by STARLINK (P.T. Wallace).
;
;
; INPUTS:
;
;   EPOCH or JDAY - If TO_DAY is set, Besselian or Julian year number.
;                   If TO_DAY is not set, the Julian day number.
;
; KEYWORDS:
;
;   B - if set, then year numbers (input/output) are expressed in
;       Besselian years.
;
;   J - if set, then year numbers (input/output) are expressed in
;       Julian years.
;
;   TO_DAY - if set, then convert EJ (assumed to be year number) into
;            Julian day number.
;
;   MJD - if set, then Julian days are expressed as "modified" Julian
;         Days, or Julian days minus 2400000.5d.
;
;
; RETURNS:
;
;   If TO_DAY is set, then returns Julian Days.
;
;   If TO_DAY is not set, then returns year number.
;   
;
; REFERENCES:
;
;   Lieske, J. H. 1979, Astron & Astrophysics, 73, 282
;
;   Wallace, P. T. 1999, SLALIB Software Library (STARLINK)
;     http://star-www.st-and.ac.uk/starlink/
;     (Routines sla_EPB2D, sla_EPJ2D, sla_EPB and sla_EPJ)
;
; SEE ALSO:
;
;   JULDAY, CALDAT
;
; MODIFICATION HISTORY:
;   Written, CM, 04 Mar 2002
;   Documented, CM, 22 Mar 2002
;
;  $Id$
;
;-
; Copyright (C) 2002, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-


function jbepoch, ep, b=b, j=j, to_day=today, mjd=mjd

  if NOT keyword_set(mjd) then offset = 2400000.5d else offset = 0d

  if keyword_set(b) then begin
      if keyword_set(today) then begin
          return, offset + 15019.81352D0 + (ep-1900D0)*365.242198781D0
      endif else begin
          return, 1900D0 + (ep-15019.81352D0-offset)/365.242198781D0
      endelse
  endif else if keyword_set(j) then begin
      if keyword_set(today) then begin
          return, offset + 51544.5D0 + (ep-2000D0)*365.25D0
      endif else begin
          return, 2000D0 + (ep-51544.5D0-offset)/365.25D0
      endelse
  endif

  message, 'ERROR: you must specify the /B or /J keyword'

end
