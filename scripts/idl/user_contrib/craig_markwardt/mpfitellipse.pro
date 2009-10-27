;+
; NAME:
;   MPFITELLIPSE
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Approximate fit to points forming an ellipse
;
; MAJOR TOPICS:
;   Curve and Surface Fitting
;
; CALLING SEQUENCE:
;   parms = MPFITELLIPSE(X, Y, start_parms, [/TILT, WEIGHTS=wts, ...])
;
; DESCRIPTION:
;
;   MPFITELLIPSE fits a closed elliptical or circular curve to a two
;   dimensional set of data points.  The user specifies the X and Y
;   positions of the points, and an optional set of weights.  The
;   ellipse may also be tilted at an arbitrary angle.
;
;   IMPORTANT NOTE: this fitting program performs simple ellipse
;   fitting.  It will not work well for ellipse data with high
;   eccentricity.  More robust answers can usually be obtained with
;   "orthogonal distance regression."  (See FORTRAN package ODRPACK on
;   netlib.org for more information).
;
;   The best fitting ellipse parameters are returned from by
;   MPFITELLIPSE as a vector, whose values are:
;
;      P[0]   Ellipse semi axis 1
;      P[1]   Ellipse semi axis 2   ( = P[0] if CIRCLE keyword set)
;      P[2]   Ellipse center - x value
;      P[3]   Ellipse center - y value
;      P[4]   Ellipse rotation angle (radians) if TILT keyword set
;
;   As of the 17 Sep 2007 version of this function, P[0] is meant to
;   be the semi-major axis, and P[1] the semi-minor axis.  The
;   returned semi-axis lengths should always be positive.
;
;   The user may specify an initial set of trial parameters, but by
;   default MPFITELLIPSE will estimate the parameters automatically.
;
;   Users should be aware that in the presence of large amounts of
;   noise, namely when the measurement error becomes significant
;   compared to the ellipse axis length, then the estimated parameters
;   become unreliable.  Generally speaking the computed axes will
;   overestimate the true axes.  For example when (SIGMA_R/R) becomes
;   0.5, the radius of the ellipse is overestimated by about 40%.
;
;   This unreliability is also pronounced if the ellipse has high
;   eccentricity, as noted above.
;
;   Users can weight their data as they see appropriate.  However, the
;   following prescription for the weighting may serve as a good
;   starting point, and appeared to produce results comparable to the
;   typical chi-squared value.
;
;     WEIGHTS = 0.75/(SIGMA_X^2 + SIGMA_Y^2)
;
;   where SIGMA_X and SIGMA_Y are the measurement error vectors in the
;   X and Y directions respectively.  However, this has not been
;   robustly tested, and it should be pointed out that this weighting
;   may only be appropriate for a set of points whose measurement
;   errors are comparable.  If a more robust estimation of the
;   parameter values is needed, the so-called orthogonal distance
;   regression package should be used (ODRPACK, available in FORTRAN
;   at www.netlib.org).
;
; INPUTS:
;
;   X - measured X positions of the points in the ellipse.
;   Y - measured Y positions of the points in the ellipse.
;
;   START_PARAMS - an array of starting values for the ellipse
;                  parameters, as described above.  This parameter is
;                  optional; if not specified by the user, then the
;                  ellipse parameters are estimated automatically from
;                  the properties of the data.
;
; RETURNS:
;
;   Returns the best fitting model ellipse parameters.
;
; KEYWORDS:
;
;   ** NOTE ** Additional keywords such as PARINFO, BESTNORM, and
;              STATUS are accepted by MPFITELLIPSE but not documented
;              here.  Please see the documentation for MPFIT for the
;              description of these advanced options.
;
;   PERROR - upon return, the 1-sigma uncertainties of the returned
;            ellipse parameter values.  These values are only
;            meaningful if the WEIGHTS keyword is specified properly.
;
;            If the fit is unweighted (i.e. no errors were given, or
;            the weights were uniformly set to unity), then PERROR
;            will probably not represent the true parameter
;            uncertainties.  
;
;   QUIET - if set then diagnostic fitting messages are suppressed.
;           Default: QUIET=1 (i.e., no diagnostics]
;
;   CIRCULAR - if set, then the curve is assumed to be a circle
;              instead of ellipse.  When set, the parameters P[0] and
;              P[1] will be identical and the TILT keyword will have
;              no effect.
;
;   TILT - if set, then the major and minor axes of the ellipse
;          are allowed to rotate with respect to the data axes.
;          Parameter P[4] will be set to the clockwise rotation angle
;          of the P[0] axis in radians, as measured from the +X axis.
;          P[4] should be in the range 0 to !dpi.
;
;   WEIGHTS - Array of weights to be used in calculating the
;             chi-squared value.  The chi-squared value is computed
;             as follows:
;
;                CHISQ = TOTAL( (Z-MYFUNCT(X,Y,P))^2 * ABS(WEIGHTS)^2 )
;
;             Users may wish to follow the guidelines for WEIGHTS
;             described above.
;
;
; EXAMPLE:
;
; ; Construct a set of points on an ellipse, with some noise
;   ph0 = 2*!pi*randomu(seed,50)
;   x =  50. + 32.*cos(ph0) + 4.0*randomn(seed, 50)
;   y = -75. + 65.*sin(ph0) + 0.1*randomn(seed, 50)
;
; ; Compute weights function
;   weights = 0.75/(4.0^2 + 0.1^2)
;
; ; Fit ellipse and plot result
;   p = mpfitellipse(x, y)
;   plot, x, y, psym=1
;   phi = dindgen(101)*2D*!dpi/100
;   oplot, p[2]+p[0]*cos(phi), p[3]+p[1]*sin(phi)
;
; REFERENCES:
;
;   MINPACK-1, Jorge More', available from netlib (www.netlib.org).
;   "Optimization Software Guide," Jorge More' and Stephen Wright, 
;     SIAM, *Frontiers in Applied Mathematics*, Number 14.
;
; MODIFICATION HISTORY:
;
;   Ported from MPFIT2DPEAK, 17 Dec 2000, CM
;   More documentation, 11 Jan 2001, CM
;   Example corrected, 18 Nov 2001, CM
;   Change CIRCLE keyword to the correct CIRCULAR keyword, 13 Sep
;      2002, CM
;   Add error messages for SYMMETRIC and CIRCLE, 08 Nov 2002, CM
;   Found small error in computation of _EVAL (when CIRCULAR) was set;
;      sanity check when CIRCULAR is set, 21 Jan 2003, CM
;   Convert to IDL 5 array syntax (!), 16 Jul 2006, CM
;   Move STRICTARR compile option inside each function/procedure, 9
;     Oct 2006
;   Add disclaimer about the suitability of this program for fitting
;     ellipses, 17 Sep 2007, CM
;   Clarify documentation of TILT angle; make sure output contains
;    semi-major axis first, followed by semi-minor; make sure that
;    semi-axes are always positive (and can handle negative inputs)
;      17 Sep 2007, CM
;   Output tilt angle is now in range 0 to !DPI, 20 Sep 2007, CM
;   Some documentation clarifications, including to remove reference
;     to the "ERR" keyword, which does not exist, 17 Jan 2008, CM
;
;  $Id$
;-
; Copyright (C) 1997-2000,2002,2003,2007,2008 Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-


FORWARD_FUNCTION mpfitellipse_u, mpfitellipse_eval, mpfitellipse, mpfit

; Compute the "u" value = (x/a)^2 + (y/b)^2 with optional rotation
function mpfitellipse_u, x, y, p, tilt=tilt, circle=circle
  COMPILE_OPT strictarr
  widx  = abs(p[0]) > 1e-20 & widy  = abs(p[1]) > 1e-20 
  if keyword_set(circle) then widy  = widx
  xp    = x-p[2]            & yp    = y-p[3]
  theta = p[4]

  if keyword_set(tilt) AND theta NE 0 then begin
      c  = cos(theta) & s  = sin(theta)
      return, ( (xp * (c/widx) - yp * (s/widx))^2 + $
                (xp * (s/widy) + yp * (c/widy))^2 )
  endif else begin
      return, (xp/widx)^2 + (yp/widy)^2
  endelse

end

; This is the call-back function for MPFIT.  It evaluates the
; function, subtracts the data, and returns the residuals.
function mpfitellipse_eval, p, tilt=tilt, circle=circle, _EXTRA=extra

  COMPILE_OPT strictarr
  common mpfitellipse_common, xy, wc

  tilt = keyword_set(tilt) 
  circle = keyword_set(circle)
  u2 = mpfitellipse_u(xy[*,0], xy[*,1], p, tilt=tilt, circle=circle) - 1.

  if n_elements(wc) GT 0 then begin
      if circle then u2 = sqrt(abs(p[0]*p[0]*wc))*u2 $
      else           u2 = sqrt(abs(p[0]*p[1]*wc))*u2 
  endif

  return, u2
end

function mpfitellipse, x, y, p0, WEIGHTS=wts, $
                       BESTNORM=bestnorm, nfev=nfev, STATUS=status, $
                       tilt=tilt, circular=circle, $
                       circle=badcircle1, symmetric=badcircle2, $
                       parinfo=parinfo, query=query, $
                       covar=covar, perror=perror, niter=iter, $
                       quiet=quiet, ERRMSG=errmsg, _EXTRA=extra

  COMPILE_OPT strictarr
  status = 0L
  errmsg = ''

  ;; Detect MPFIT and crash if it was not found
  catch, catcherror
  if catcherror NE 0 then begin
      MPFIT_NOTFOUND:
      catch, /cancel
      message, 'ERROR: the required function MPFIT must be in your IDL path', /info
      return, !values.d_nan
  endif
  if mpfit(/query) NE 1 then goto, MPFIT_NOTFOUND
  catch, /cancel
  if keyword_set(query) then return, 1

  if n_params() EQ 0 then begin
      message, "USAGE: PARMS = MPFITELLIPSE(X, Y, START_PARAMS, ... )", $
        /info
      return, !values.d_nan
  endif
  nx = n_elements(x) & ny = n_elements(y)
  if (nx EQ 0) OR (ny EQ 0) OR (nx NE ny) then begin
      message, 'ERROR: X and Y must have the same number of elements', /info
      return, !values.d_nan
  endif

  if keyword_set(badcircle1) OR keyword_set(badcircle2) then $
    message, 'ERROR: do not use the CIRCLE or SYMMETRIC keywords.  ' +$
    'Use CIRCULAR instead.'

  p = make_array(5, value=x[0]*0)

  if n_elements(p0) GT 0 then begin
      p[0] = p0
      if keyword_set(circle) then p[1] = p[0]
  endif else begin
      mx = moment(x)
      my = moment(y)
      p[0] = [sqrt(mx[1]), sqrt(my[1]), mx[0], my[0], 0]
      if keyword_set(circle) then $
        p[0:1] = sqrt(mx[1]+my[1])
  endelse

  common mpfitellipse_common, xy, wc
  if n_elements(wts) GT 0 then begin
      wc = abs(wts)
  endif else begin
      wc = 0 & dummy = temporary(wc)
  endelse

  xy = [[x],[y]]

  result = mpfit('mpfitellipse_eval', p, $
                 parinfo=parinfo, STATUS=status, nfev=nfev, BESTNORM=bestnorm,$
                 covar=covar, perror=perror, niter=iter, $
                 functargs={circle:keyword_set(circle), tilt:keyword_set(tilt)},$
                 ERRMSG=errmsg, quiet=quiet, _EXTRA=extra)

  ;; Print error message if there is one.
  if NOT keyword_set(quiet) AND errmsg NE '' then $
    message, errmsg, /info

  ;; Sanity check on resulting parameters
  if keyword_set(circle) then begin
      result[1] = result[0]
      perror[1] = perror[0]
  endif
  if NOT keyword_set(tilt) then begin
      result[4] = 0
      perror[4] = 0
  endif 

  ;; Make sure the axis lengths are positive, and the semi-major axis
  ;; is listed first
  result[0:1] = abs(result[0:1])
  if abs(result[0]) LT abs(result[1]) then begin
      tmp = result[0] & result[0] = result[1] & result[1] = tmp
      tmp = perror[0] & perror[0] = perror[1] & perror[1] = tmp
      if keyword_set(tilt) then result[4] = result[4] - !dpi/2d
  endif

  if keyword_set(tilt) then begin
      ;; Put tilt in the range 0 to +Pi
      result[4] = result[4] - !dpi * floor(result[4]/!dpi)
  endif

  return, result
end

