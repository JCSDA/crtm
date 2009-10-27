;+
; NAME:
;   GAUSS2
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
;   YVALS = GAUSS2(X, Y, [XCENT, YCENT, SIGMA, PEAK])
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
;   X - 2-dimensional array of "X"-values.
;   Y - 2-dimensional array of "Y"-values.
;
;   XCENT - X-position of gaussian centroid.
;   YCENT - Y-position of gaussian centroid.
;
;   SIGMA - sigma of the curve (X and Y widths are the same).
;
;   PEAK - the peak value of the gaussian function.
;
; RETURNS:
;
;   Returns the array of Y-values.
;
; EXAMPLE:
;
;   p = [2.2D, -0.7D, 1.4D, 3000.D]
;   x = (dindgen(200)*0.1 - 10.) # (dblarr(200) + 1)
;   y = (dblarr(200) + 1) # (dindgen(200)*0.1 - 10.)
;   z = gauss2(x, y, p)
;
;   Computes the values of the Gaussian at equispaced intervals in X
;   and Y (spacing is 0.1).  The gaussian has a centroid position of
;   (2.2, -0.7), standard deviation of 1.4, and peak value of 3000.
;
; REFERENCES:
;
; MODIFICATION HISTORY:
;   Written, 02 Oct 1999, CM
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

function gauss2, x, y, p, _EXTRA=extra

  u = ((x-p(0))/p(2))^2 + ((y-p(1))/p(2))^2
  mask = u LT 100
  f = p(3) * mask * exp(-0.5D * temporary(u) * mask)
  mask = 0

  return, f
end
