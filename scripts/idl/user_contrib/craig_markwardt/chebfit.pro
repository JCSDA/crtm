;+
; NAME:
;   CHEBFIT
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Fit Chebyshev polynomial coefficients to a tabulated function
;
; MAJOR TOPICS:
;   Curve and Surface Fitting
;
; CALLING SEQUENCE:
;   p = CHEBFIT(X, Y, ERR, INTERVAL=interval, NMAX=nmax,
;               PRECISION=prec, /EVEN, /ODD, REDUCE_ALGORITHM=)
;
; DESCRIPTION:
;
;   CHEBFIT fits a series of Chebyshev polynomials to a set of
;   tabulated and possibly noisy data points.  The functions MPFIT and
;   CHEBEVAL, available from the above web page, must also be in your
;   IDL path for this function to work properly.  The user can choose
;   the desired precision and maximum number of chebyshev
;   coefficients.
;
;   This function is intended for use on already-tabulated data which
;   are potentially noisy.  The user should never expect more than
;   NPOINTS terms, where NPOINTS is the number of (x,y) pairs.  For
;   functions which can be evaluated to full machine precision at
;   arbitrary abcissae, the routine CHEBCOEF should be used instead.
;   For exact data tabulated on a regular grid, the routine CHEBGRID
;   should be tried.
;
;   The user can also specify that the function is even or odd, using
;   the keywords EVEN or ODD.  This saves computation time because
;   certain terms in the expansion can be ignored.  For the purposes
;   of this function even and odd refer to the symmetry about the
;   center of the interval.
;
;   The algorithm is employed in three steps.  In the first step, the
;   coefficients are estimated at a crude level.  In the second step,
;   it is determined whether certain coefficients are deemed
;   "ignoreable", i.e., they do not contribute significantly to the
;   function and are discarded.  The operation of this step is
;   determined by the REDUCE_ALGORITHM keyword.  Finally, the
;   remaining "good" coefficients are re-fitted to achieve the best
;   fit.
;
; INPUTS:
;
;   X, Y - the x- and y- tabulated values to be fitted.
;
;   ERR - (optional) the y-error bar associated with each (x,y) pair.
;         Default: 1
;
; RETURNS:
;
;   An array of Chebyshev coefficients which can be passed to
;   CHEBEVAL.  NOTE: the convention employed here is such that the
;   constant term in the expansion is P(0)*T0(x) (i.e., the convention
;   of Luke), and not P(0)/2 * T0(x).
;
; KEYWORD PARAMETERS:
;
;   EVEN, ODD - if set, then the fitting routine assumes the function
;               is even or odd, about the center of the interval.
;
;   INTERVAL - a 2-element vector describing the interval over which
;              the polynomial is to be evaluated.
;              Default: [-1, 1]
;
;   NMAX - a scalar, the maximum number of polynomial terms to be
;          fitted at one time.
;          Default: 16
;
;   PRECISION - a scalar, the requested precision in the fit.  Any
;               terms which do not contribute significantly, as
;               defined by this threshold, are discarded.  If the
;               function to be fitted is not well-behaved, then the
;               precision is not guaranteed to reach the desired
;               level.
;               Default: 1E-7
;
;   REDUCE_ALGORITHM - a scalar integer, describes how insignificant
;               terms are removed from the fit.  If 0, then all terms
;               are kept, and none are dicarded.  If 1, then only
;               trailing terms less than PRECISION are discarded.  If
;               2, then both trailing and intermediate terms less than
;               PRECISION are discarded.
;               Default: 2
;
; EXAMPLE:
;
;   x = dindgen(1000)/100     ; Range of 0 to 10
;   y = cos(x) + randomn(seed,1000)*0.01  ; Function with some noise
;   p = chebfit(x, y, interval=[0d,10d])
;   plot, x, y - chebeval(x,p, interval=[0d,10d])
;
; REFERENCES:
;
;   Abramowitz, M. & Stegun, I., 1965, *Handbook of Mathematical
;     Functions*, 1965, U.S. Government Printing Office, Washington,
;     D.C. (Applied Mathematical Series 55)
;   CERN, 1995, CERN Program Library, Function E407
;   Luke, Y. L., *The Special Functions and Their Approximations*,
;     1969, Academic Press, New York
;
; MODIFICATION HISTORY:
;   Written and documented, CM, June 2001
;   Copyright license terms changed, CM, 30 Dec 2001
;   Added usage message, CM, 20 Mar 2002
;   Slight docs change, CM, 25 Mar 2002
;
;  $Id$
;
;-
; Copyright (C) 2001, 2002, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

;; Compute residuals for MPFIT
function chebfit_eval, p, interval=interval, nterms=nterms, igood=igood, $
                       _EXTRA=extra

  common chebfit_common, x, y, err

  if n_elements(igood) EQ 0 then begin
      p1 = p
  endif else begin
      p1 = replicate(p(0)*0, nterms)
      p1(igood) = p
  endelse

  ;; Compute the Chebyshev polynomial
  f = chebeval(x, p1, interval=interval)

  ;; Compute the deviates, applying either errors or weights
  if n_elements(err) GT 0 then begin
      result = (y-f)/err
  endif else if n_elements(wts) GT 0 then begin
      result = (y-f)*wts
  endif else begin
      result = (y-f)
  endelse
  
  ;; Make sure the returned result is one-dimensional.
  result = reform(result, n_elements(result), /overwrite)
  return, result
end

function chebfit, x, y, err, nmax=nterms0, interval=interval, $
                  precision=prec, even=even, odd=odd, quiet=quiet, $
                  initialize=init, reduce_algorithm=redalg0, $
                  indices=igood, nocatch=nocatch, $
                  yfit=yfit, perror=perror, bestnorm=bestnorm, dof=dof

  if n_params() EQ 0 then begin
      message, 'USAGE:', /info
      message, 'P = CHEBFIT(X, Y, ERR, INTERVAL=[a,b], NMAX=, ...)', /info
      return, !values.d_nan
  endif

  if n_elements(nterms0) EQ 0 then nterms = 16L $
  else                             nterms = floor(nterms0(0)) > 2L
  nterms = nterms < n_elements(x)
  if n_elements(interval) LT 2 then interval = [-1., 1.]
  if n_elements(prec) EQ 0 then prec = 1.e-7
  if n_elements(redalg0) EQ 0 then redalg = 2 else redalg = floor(redalg0(0))
  if n_elements(quiet) EQ 0 then quiet = 1

  ;; Handle error conditions gracefully
  if NOT keyword_set(nocatch) then begin
      catch, catcherror
      if catcherror NE 0 then begin
          catch, /cancel
          message, 'Error detected while fitting', /info
          message, !err_string, /info
          ier = -1L
          return, 0L
      endif
  endif

  if n_elements(p) LT nterms OR keyword_set(init) then begin
      p = replicate(x(0)*0 + 1, nterms) / (findgen(nterms)+1)^2
      p(0) = total(y)/n_elements(y)
      ;; If mean is *exactly* zero, then shift it off slightly
      if p(0) EQ 0 then p(0) = sqrt(total(y^2))/n_elements(y)/10
  endif
  p0 = p
  igood = lindgen(nterms)

  if keyword_set(even) OR keyword_set(odd) then $
    igood = lindgen(n_elements(p)/2)*2 + keyword_set(odd)
  nt = min([nterms, max(igood)+1])

  ;; Cancel out old common entries
  common chebfit_common, xc, yc, errc
  xc   = 0 & dummy = temporary(xc)
  yc   = 0 & dummy = temporary(yc)
  errc = 0 & dummy = temporary(errc)
  
  xc = x
  yc = y
  if n_elements(err) GT 0 then begin
      errc = err
  endif

  fa = {interval: interval, igood: igood, nterms: nt}
  p1 = mpfit('CHEBFIT_EVAL', p0(igood), functargs=fa, maxiter=5, quiet=quiet)
  p0(igood) = p1

  ;; Look for and remove the insignificant terms from the fit
  if redalg GT 0 then begin
      wh = where(abs(p1) GT prec(0), ct)
      if ct EQ 0 then begin
          ALL_ZERO:
          message, 'WARNING: no significant Chebyshev terms were detected', $
            /info
          p = p0*0
          return, 0L
      endif
      if max(wh) LT n_elements(igood)-1 then begin
          imax = max(wh)
          igood = igood(0:imax)
          p1 = p1(0:imax)
      endif

      if redalg EQ 2 then begin
          wh = where(abs(p1) GT 0.1*prec, ct)
          if ct EQ 0 then goto, ALL_ZERO
          igood = igood(wh)
          p1 = p1(wh)
      endif
  endif

  nt = min([nterms, max(igood)+1])
  fa = {interval: interval, igood: igood, nterms: nt}
  p2 = mpfit('CHEBFIT_EVAL', p1, functargs=fa, maxiter=10, quiet=quiet, $
             perror=dp2, bestnorm=bestnorm, dof=dof)

  xc = 0 & yc = 0 & errc = 0
  p = p0*0 
  perror = p
  p(igood) = p2
  perror(igood) = dp2

  if arg_present(yfit) then $
    yfit = chebeval(x, p, interval=interval)

  return, p
end
