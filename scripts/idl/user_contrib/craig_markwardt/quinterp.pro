;+
; NAME:
;   QUINTERP
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Quintic spline interpolation from tabulated first and second derivatives
;
; MAJOR TOPICS:
;   Interpolation, Hermite Interpolation
;
; CALLING SEQUENCE:
;   QINTERP, XTAB, YTAB, YPTAB, YPPTAB, $
;      XINT, YINT, YPINT=, YPPINT=, MISSING=
;
; DESCRIPTION:
;
;   QUINTERP performs quintic spline interpolation of a function.
;   This routine is a natural extension of CUBETERP, in that it meant
;   for interpolation where the tabulated function has known values,
;   first derivatives *and* second derivatives at each point.  Given
;   that there are six known values for each interpolation interval,
;   the resulting interpolation function is a quintic polynomial (one
;   of a class of Hermite interpolating splines).
;
;   The user provides a tabulated set of data, whose (X,Y) positions
;   are (XTAB, YTAB), and whose first and second derivatives are YPTAB
;   and YPPTAB.  The user also provides a set of desired "X" abcissae
;   for which interpolants are requested.  The interpolated spline
;   values are returned in YINT.  The interpolated curve will smoothly
;   pass through the control points, and have the requested
;   derivatives at those points.
;
;   Note that the user must provide both derivatives (they are not
;   optional).  If you don't have one or more derivatives, then you
;   should use the IDL spline functions SPL_INIT/SPL_INTERP, or the
;   functions CUBETERP, QUADTERP or LINTERP instead.  Unlike CUBETERP,
;   if the requested point is outside of the tabulated range, the
;   function is not extrapolated.  Instead the value provided by the
;   MISSING keyword is returned for those points.
;
;   The user may also optionally request the first and second
;   derivatives of the function with the YPINT and YPPINT keywords.
;
; INPUTS:
;
;   XTAB - tabulated X values.  Must be sorted in increasing order.
;
;   YTAB - tabulated Y values.
;
;   YPTAB - tabulated first derivatives ( = dY/dX ). Not optional
;   YPPTAB - tabulated second derivatives ( = d(YPTAB)/dX ).  Not optional.
;
;   XINT - X values of desired interpolants.
;
; OUTPUTS:
;
;   YINT - Y values of desired interpolants.
;
; OPTIONAL KEYWORDS:
;
;   YPINT - upon return, the slope (first derivative) at the
;           interpolated positions.
;
;   YPPINT - upon return, the second derivative at the interpolated
;            positions.
;
;   MISSING - a value to report for "missing" data.  This function
;             does not perform extrapolation; any requested point
;             outside the range [MIN(XTAB),MAX(XTAB)] is considered
;             missing.
;             Default: 0
;
; EXAMPLE:
;
;  ;; Set up some fake data, a sinusoid
;  xtab = dindgen(101)/100d * 2d*!dpi    ; 100 points from 0 -> 2*!dpi
;  ytab = sin(xtab)    ;; values
;  yptab = cos(xtab)   ;; 1st deriv
;  ypptab = -sin(xtab) ;; 2nd deriv
;
;  ;; Interpolate to a finer grid
;  xint = dindgen(1001)/1000 * 2d*!dpi   ;; 1000 points from 0->2*!dpi
;  quinterp, xtab, ytab, yptab, ypptab, xint, yint, ypint=ypint, yppint=yppint
;
;  ;; Plot it
;  plot, xint, yint
;  oplot, xtab, ytab, psym=1, symsize=2
;  for i = 0, n_elements(xtab)-1 do $   ;; Also plot slopes
;    oplot, xtab(i)+[-0.5,0.5], ytab(i)+[-0.5,0.5]*yptab(i)
; 
;
; MODIFICATION HISTORY:
;   Written and documented, CM, 08 Oct 2008
;
;  $Id$
;
;-
; Copyright (C) 2008, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-


pro quinterp, xtab, ytab, yptab, ypptab, xint, yint, $
              ypint=ypint, yppint=yppint, $
              missing=missing0

  ntab = n_elements(xtab)

  if n_elements(xtab) EQ 0 OR n_elements(ytab) EQ 0 then begin
      message, 'ERROR: XTAB and YTAB must be passed'
  endif
  if (n_elements(xtab) NE n_elements(ytab) OR $
      n_elements(xtab) NE n_elements(yptab) OR $
      n_elements(xtab) NE n_elements(ypptab)) then begin
      message, $
        'ERROR: Number of elements of XTAB, YTAB, YPTAB and YPPTAB must agree'
  endif
  if n_elements(xint) EQ 0 then begin
      message, 'ERROR: XINT must be passed'
  endif
  if n_elements(missing0) EQ 0 then begin
      miss = 0d
  endif else begin
      miss = missing0(0)
  endelse

  ;; Locate previous tabulated value
  ii = value_locate(xtab, xint)

  ;; Here we make a safety check, in case the desired point(s) is
  ;; above or below the interior of the interpolation range.  In that
  ;; case, we will need to extrapolate, based on the next nearest
  ;; interval.
  iis = ii > 0 < (ntab-2)
  whbad = where(xint LT xtab(0) OR xint GT xtab(ntab-1), ctbad)

  ;; Distance from interpolated abcissae to previous tabulated abcissa
  dx = (xint - xtab(iis))
  
  ;; Distance between adjoining tabulated abcissae and ordinates
  xs = xtab(iis+1) - xtab(iis)
  ys = ytab(iis+1) - ytab(iis) 

  ;; Rescale or pull out quantities of interest
  dx   = dx/xs              ;; Rescale DX
  y0   = ytab(iis)          ;; No rescaling of Y - start of interval
  y1   = ytab(iis+1)        ;; No rescaling of Y - end of interval
  yp0  = yptab(iis)*xs      ;; Rescale tabulated derivatives - start of interval
  yp1  = yptab(iis+1)*xs    ;; Rescale tabulated derivatives - end of interval
  ypp0 = ypptab(iis)*xs*xs  ;; Rescale tabulated 2nd der. - start of interval
  ypp1 = ypptab(iis+1)*xs*xs;; Rescale tabulated 2nd der. - end of interval


  ;; Compute values of t^n for quintic (n = 0 .. 5)
  t0 = 1d
  t1 = dx
  t2 = dx*dx
  t3 = dx*t2
  t4 = dx*t3
  t5 = dx*t4

  ;; Quintic Hermite polynomial
  yint = ((-6*t5 + 15*t4 - 10*t3 + 1 )*y0 + $
          ( 6*t5 - 15*t4 + 10*t3     )*y1 + $
          (-3*t5 +  8*t4 -  6*t3 + t1)*yp0 + $
          (-3*t5 +  7*t4 -  4*t3     )*yp1 + $
          (  -t5 +  3*t4 -  3*t3 + t2)*ypp0/2d + $
          (   t5 -  2*t4 +    t3     )*ypp1/2d)
  if ctbad GT 0 then yint(whbad) = miss
  
  if arg_present(ypint) then begin
      ypint = ((-30*t4 + 60*t3 - 30*t2       )*y0 + $
               ( 30*t4 - 60*t3 + 30*t2       )*y1 + $
               (-15*t4 + 32*t3 - 18*t2 + 1   )*yp0 + $
               (-15*t4 + 28*t3 - 12*t2       )*yp1 + $
               ( -5*t4 + 12*t3 -  9*t2 + 2*t1)*ypp0/2d + $
               (  5*t4 -  8*t3 +  3*t2       )*ypp1/2d) / xs
      if ctbad GT 0 then ypint(whbad) = miss
  endif
  
  if arg_present(yppint) then begin
      yppint = ((-120*t3 + 180*t2 - 60*t1     )*y0 + $
                ( 120*t3 - 180*t2 + 60*t1     )*y1 + $
                ( -60*t3 +  96*t2 - 36*t1     )*yp0 + $
                ( -60*t3 +  84*t2 - 24*t1     )*yp1 + $
                ( -20*t3 +  36*t2 - 18*t1 + 2d)*ypp0/2d + $
                (  20*t3 -  24*t2 +  6*t1     )*ypp1/2d) / (xs*xs)
      if ctbad GT 0 then yppint(whbad) = miss
  endif

  return
end
