;+
; NAME:
;   CUBETERP
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Cubic spline interpolation with known derivatives
;
; MAJOR TOPICS:
;   Interpolation
;
; CALLING SEQUENCE:
;   CUBETERP, XTAB, YTAB, YPTAB, XINT, YINT, YPINT=, YPPINT=, EXTRAP_ORDER=
;
; DESCRIPTION:
;
;   CUBETERP performs cubic spline interpolation of a function.  This
;   routine is different from the many other spline interpolation
;   functions for IDL in that it allows you to choose the slope of the
;   spline at each control point.  I.e. it is not forced to be a
;   "natural" spline.
;
;   The user provides a tabulated set of data, whose (X,Y) positions
;   are (XTAB, YTAB), and whose derivatives are YPTAB.  The user also
;   provides a set of desired "X" abcissae for which interpolants are
;   requested.  The interpolated spline values are returned in YINT.
;   The interpolated curve will smoothly pass through the control
;   points, and have the requested slopes at those points.
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
;   YPTAB - tabulated derivatives ( = dY/dX, evaluated at XTAB).
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
;   EXTRAP_ORDER - technique used to extrapolate beyond the tabulated
;                  values.  Allowed values:
;                    -1 - extrapolated points are set to NaN (not a number)
;                     0 - constant extrapolation, equal to the value
;                         at the nearest tabulated point
;                     1 - linear extrapolation, based on slope at
;                         nearest tabulated value
;                     2 - quadratic extrapolation, based on slope and
;                         second derivative at nearest tabulated value
;                     3 - cubic extrapolation.
;                  DEFAULT: 2 (quadratic extrapolation)
;
;
; EXAMPLE:
;
;  ;; Set up some fake data
;  xtab = [0D,2,5,10]
;  ytab = [2D,4,-3,-5]
;  yptab = [-1D,0.5,2.3,-4]
;
;  ;; Interpolate to a finer grid
;  xint = dindgen(1001)/100
;  cubeterp, xtab, ytab, yptab, xint, yint
;
;  ;; Plot it
;  plot, xint, yint
;  oplot, xtab, ytab, psym=1, symsize=2
;  for i = 0, n_elements(xtab)-1 do $   ;; Also plot slopes
;    oplot, xtab(i)+[-0.5,0.5], ytab(i)+[-0.5,0.5]*yptab(i)
; 
;
; MODIFICATION HISTORY:
;   Written and documented, CM, July 2003
;   Added EXTRAP_ORDER = -1 option, CM, 15 May 2005
;   Syntax error fix, CM, 07 Mar 2007
;   Clarified documentation a bit, CM, 12 Nov 2007
;   Small documentation changes, CM, 16 Apr 2009
;
;  $Id$
;
;-
; Copyright (C) 2003, 2005, 2007, 2009, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

; Cubic interpolation with known slopes


pro cubeterp, xtab, ytab, yptab, xint, yint, $
              extrap_order=extr0, $
              ypint=ypint, yppint=yppint

  ntab = n_elements(xtab)
  if n_elements(extr0) EQ 0 then extr = 2 else extr = round(extr0(0))

  if n_elements(xtab) EQ 0 OR n_elements(ytab) EQ 0 then begin
      message, 'ERROR: XTAB and YTAB must be passed'
  endif
  if (n_elements(xtab) NE n_elements(ytab) OR $
      n_elements(xtab) NE n_elements(yptab)) then begin
      message, 'ERROR: Number of elements of XTAB, YTAB and YPTAB must agree'
  endif
  if n_elements(xint) EQ 0 then begin
      message, 'ERROR: XINT must be passed'
  endif

  ;; Locate previous tabulated value
  ii = value_locate(xtab, xint)

  ;; Here we make a safety check, in case the desired point(s) is
  ;; above or below the interior of the interpolation range.  In that
  ;; case, we will need to extrapolate, based on the next nearest
  ;; interval.
  iis = ii
  whll = where(ii LT 0, ctll)
  if ctll GT 0 then iis(whll) = iis(whll) + 1
  whgg = where(ii GE (ntab-1), ctgg)
  if ctgg GT 0 then iis(whgg) = iis(whgg) - 1

  ;; Distance from interpolated abcissae to previous tabulated abcissa
  dx = (xint - xtab(iis))
  
  ;; Distance between adjoining tabulated abcissae and ordinates
  xs = xtab(iis+1) - xtab(iis)
  ys = ytab(iis+1) - ytab(iis) 

  ;; Rescale or pull out quantities of interest
  dx = dx/xs               ;; Rescale DX
  y0 = ytab(iis)           ;; No rescaling of Y - start of interval
  y1 = ytab(iis+1)         ;; No rescaling of Y - end of interval
  yp0 = yptab(iis)*xs      ;; Rescale tabulated derivatives - start of interval
  yp1 = yptab(iis+1)*xs    ;; Rescale tabulated derivatives - end of interval

  ;; Compute polynomial coefficients
  a = y0
  b = yp0
  c = 3*ys - 2*yp0 - yp1
  d = yp0 + yp1 - 2*ys

  ;; Extrapolate only quadratically
  if extr EQ 2 then begin
      if ctll GT 0 then begin
          ;; Lower end of extrapolation
          d(whll) = 0
      endif
      if ctgg GT 0 then begin
          ;; Upper end of extrapolation
          dgg = d(whgg)
          a(whgg) = a(whgg) + dgg
          b(whgg) = b(whgg) - 3*dgg
          c(whgg) = c(whgg) + 3*dgg
          d(whgg) = 0
      endif
  endif

  ;; Extrapolate only linearly
  if extr EQ 1 then begin

      if ctll GT 0 then begin
          ;; Lower end of extrapolation
          c(whll) = 0
          d(whll) = 0
      endif
      if ctgg GT 0 then begin
          ;; Upper end of extrapolation
          dgg = d(whgg)
          cgg = c(whgg)
          a(whgg) = a(whgg) - cgg - 2*dgg
          b(whgg) = b(whgg) + 2*cgg + 3*dgg
          c(whgg) = 0
          d(whgg) = 0
      endif
  endif

  if extr EQ 0 then begin
      if ctll GT 0 then begin
          ;; Lower end of extrapolation
          b(whll) = 0
          c(whll) = 0
          d(whll) = 0
      endif
      if ctgg GT 0 then begin
          ;; Upper end of extrapolation
          a(whgg) = a(whgg) + b(whgg) + c(whgg) + d(whgg)
          b(whgg) = 0
          c(whgg) = 0
          d(whgg) = 0
      endif
  endif      

  if extr EQ -1 then begin
      sz = size(y0) & tp = sz(sz(0)+1)
      if sz EQ 4 OR sz EQ 6 then nanv = !values.f_nan $
      else nanv = !values.d_nan

      if ctll GT 0 then begin
          ;; Lower end of extrapolation
          a(whll) = nanv
      endif
      if ctgg GT 0 then begin
          ;; Upper end of extrapolation
          a(whgg) = nanv
      endif
  endif

  yint = a + dx*(b + dx*(c + dx*d))

  ;; Compute derivatives if requested
  if arg_present(ypint) then ypint = (b + dx*(2*c + dx*3*d))/xs
  if arg_present(yppint) then yppint = (2*c + 6*d*dx)/xs^2


  return
end
