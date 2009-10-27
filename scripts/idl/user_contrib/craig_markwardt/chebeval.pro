;+
; NAME:
;   CHEBEVAL
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Evaluate a Chebyshev polynomial on an interval, given the coefficients
;
; MAJOR TOPICS:
;   Curve and Surface Fitting
;
; CALLING SEQUENCE:
;   y = CHEBEVAL(X, P, INTERVAL=interval, DERIVATIVE=deriv)
;
; DESCRIPTION:
;
;   CHEBEVAL computes the values of a Chebyshev polynomial function at
;   specified abcissae, over the interval [a,b].  The user must supply
;   the abcissae and the polynomial coefficients.  The function is of
;   the form:
;
;               N
;       y(x) = Sum p_n T_n(x*)     x in [a,b]
;              i=0
;
;   Where T_n(x*) are the orthogonal Chebyshev polynomials of the
;   first kind, defined on the interval [-1,1] and p_n are the
;   coefficients.  The scaled variable x* is defined on the [-1,1]
;   interval such that (x*) = (2*x - a - b)/(b - a), and x is defined
;   on the [a,b] interval.
;
;   The derivative of the function may be computed simultaneously
;   using the DERIVATIVE keyword.  
;
;   The is some ambiguity about the definition of the first
;   coefficient, p_0, namely, the use of p_0 vs. the use of p_0/2.
;   The p_0 definition of Luke is used in this function.
;
; INPUTS:
;
;   X - a numerical scalar or vector, the abcissae at which to
;       evaluate the polynomial.  If INTERVAL is specified, then all
;       values of X must lie within the interval.
;
;   P - a vector, the Chebyshev polynomial coefficients, as returned
;       by CHEBFIT or CHEBCOEF.
;
; RETURNS:
;
;   An array of function values, evaluated at the abcissae.  The
;   numeric precision is the greater of X or P.
;
; KEYWORD PARAMETERS:
;
;   DERIVATIVE - upon return, a vector containing the derivative of
;                the function at each abcissa is returned in this
;                keyword.
;
;   INTERVAL - a 2-element vector describing the interval over which
;              the polynomial is to be evaluated.
;              Default: [-1, 1]
;
; EXAMPLE:
;
;   x = dindgen(1000)/100     ; Range of 0 to 10
;   p = chebcoef('COS(x)', /expr, interval=[0d, 10d])  ;; Compute coefs
;   y = chebeval(x, p, interval=[0d,10d])              ;; Eval Cheby poly
;   plot, x, y - cos(x)       ; Plot residuals
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
;   Return a vector even when P has one element, CM, 22 Nov 2004
;   Fix bug in evaluation of derivatives, CM, 22 Nov 2004
;
;  $Id$
;
;-
; Copyright (C) 2001, 2002, 2004, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-
function chebeval, x0, p, derivative=v, interval=interval0

  if n_params() EQ 0 then begin
      message, 'USAGE:', /info
      message, 'F = CHEBEVAL(X, P, INTERVAL=[a,b], DERIVATIVE=DFDX)', /info
      return, !values.d_nan
  endif

  zero = x0(0)*0 + 0. & one = zero + 1
  y = zero
  v = zero

  if n_elements(interval0) LT 2 then begin
      t = x0
      a = -one & b = +one
  endif else begin
      a = interval0(0)+0. & b = interval0(1)+0.
      if a EQ b then return, t*0
      t = (2*x0 - a - b)/(b-a)
  endelse

  p0 = one + t*0
  p1 = t

  v0 = zero + t*0
  v1 = one + t*0
  v2 = 4.*t

  t = 2.*t

  n = n_elements(p)
  if arg_present(v) then begin
      for i = 0L, n-1, 2 do begin
          if i EQ n-1 then begin
              p1 = zero
              v1 = zero
          endif
          j = (i+1)<(n-1)

          y = temporary(y) + p(i)*p0 + p(j)*p1
          v = temporary(v) + p(i)*v0 + p(j)*v1

          ;; Advance to the next set of Chebyshev polynomials. For
          ;; velocity we need to keep the next orders around
          ;; momentarily.
          p2 = t*p1 - p0
          p3 = t*p2 - p1
          v2 = t*v1 - v0 + 2*p1
          v3 = t*v2 - v1 + 2*p2
          
          p0 = temporary(p2) & p1 = temporary(p3)
          v0 = temporary(v2) & v1 = temporary(v3)
      endfor
  endif else begin
      for i = 0L, n-1, 2 do begin
          if i EQ n-1 then p1 = zero
          j = (i+1)<(n-1)

          y = temporary(y) + p(i)*p0 + p(j)*p1

          ;; Advance to the next set of Chebyshev polynomials.  For no
          ;; derivative, we can re-use old variables.
          p0 = t*p1 - temporary(p0)
          p1 = t*p0 - temporary(p1)
      endfor
  endelse

  v = temporary(v)*2/(b-a)
  return, y
end
