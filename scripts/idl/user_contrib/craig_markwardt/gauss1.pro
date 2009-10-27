;+
; NAME:
;   GAUSS1
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Compute Gaussian curve given the mean, sigma and area.
;
; MAJOR TOPICS:
;   Curve and Surface Fitting
;
; CALLING SEQUENCE:
;   YVALS = GAUSS1(XVALS, [MEAN, SIGMA, AREA], SKEW=skew)
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
; INPUT KEYWORD PARAMETERS:
;
;   SKEW - You may specify a skew value.  Default is no skew.
;
;   PEAK - if set then AREA is interpreted as the peak value rather
;          than the area under the peak.
;
; RETURNS:
;
;   Returns the array of Y-values.
;
; EXAMPLE:
;
;   p = [2.2D, 1.4D, 3000.D]
;   x = dindgen(200)*0.1 - 10.
;   y = gauss1(x, p)
;
;   Computes the values of the Gaussian at equispaced intervals
;   (spacing is 0.1).  The gaussian has a mean of 2.2, standard
;   deviation of 1.4, and total area of 3000.
;
; REFERENCES:
;
; MODIFICATION HISTORY:
;   Written, Jul 1998, CM
;   Correct bug in normalization, CM, 01 Nov 1999
;   Optimized for speed, CM, 02 Nov 1999
;   Added copyright notice, 25 Mar 2001, CM
;   Added PEAK keyword, 30 Sep 2001, CM
;
;  $Id$
;
;-
; Copyright (C) 1998,1999,2001, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

function gauss1, x, p, skew=skew, peak=peak, _EXTRA=extra

  sz = size(x)
  if sz(sz(0)+1) EQ 5 then smax = 26D else smax = 13.

  if n_elements(p) GE 3 then norm = p(2) else norm = x(0)*0 + 1

  u = ((x-p(0))/(abs(p(1)) > 1e-20))^2
  mask = u LT (smax^2)
  if NOT keyword_set(peak) then norm = norm / (sqrt(2.D * !dpi)*p(1))
  f = norm * mask * exp(-0.5*temporary(u) * mask)
  mask = 0

  if n_elements(skew) GT 0 then $
    f = (1.D + skew * (x-p(0))/p(1))*f

  return, f
end
