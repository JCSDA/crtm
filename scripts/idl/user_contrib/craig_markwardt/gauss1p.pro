;+
; NAME:
;   GAUSS1P
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Compute Gaussian curve given the mean, sigma and area (procedure).
;
; MAJOR TOPICS:
;   Curve and Surface Fitting
;
; CALLING SEQUENCE:
;   GAUSS1, XVALS, [MEAN, SIGMA, AREA], YVALS, SKEW=skew
;
; DESCRIPTION:
;
;  This routine computes the values of a Gaussian function whose
;  X-values, mean, sigma, and total area are given.  It is meant to be
;  a demonstration for curve-fitting.
;
;  XVALS can be an array of X-values, in which case the returned
;  Y-values are an array as well.  The second parameter to GAUSS1
;  should be an array containing the MEAN, SIGMA, and total AREA, in
;  that order.
;
; INPUTS:
;   X - Array of X-values.
;
;   [MEAN, SIGMA, AREA] - the mean, sigma and total area of the 
;                         desired Gaussian curve.
;
;   YVALS - returns the array of Y-values.
;
;
; KEYWORD PARAMETERS:
;
;   SKEW - You may specify a skew value.  Default is no skew.
;
; EXAMPLE:
;
;   p = [2.2D, 1.4D, 3000.D]
;   x = dindgen(200)*0.1 - 10.
;   gauss1p, x, p, y
;
;   Computes the values of the Gaussian at equispaced intervals
;   (spacing is 0.1).  The gaussian has a mean of 2.2, standard
;   deviation of 1.4, and total area of 3000.
;
; REFERENCES:
;
; MODIFICATION HISTORY:
;   Transcribed from GAUSS1, 13 Dec 1999, CM
;   Added copyright notice, 25 Mar 2001, CM
;
;  $Id$
;
;-
; Copyright (C) 1999,2001, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

pro gauss1p, x, p, f, skew=skew, _EXTRA=extra

  sz = size(x)
  if sz(sz(0)+1) EQ 5 then smax = 26D else smax = 13.

  if n_elements(p) GE 3 then norm = p(2) else norm = x(0)*0 + 1

  u = ((x-p(0))/(abs(p(1)) > 1e-20))^2
  mask = u LT (smax^2)  ;; Prevent floating underflow
  f = norm * mask * exp(-0.5*temporary(u) * mask) / (sqrt(2.D * !dpi)*p(1))
  mask = 0
  
  if n_elements(skew) GT 0 then $
    f = (1.D + skew * (x-p(0))/p(1))*f

  return
end
