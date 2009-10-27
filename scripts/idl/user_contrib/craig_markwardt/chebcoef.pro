;+
; NAME:
;   CHEBCOEF
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Estimate Chebyshev polynomial coefficients of a function on an interval
;
; MAJOR TOPICS:
;   Curve and Surface Fitting
;
; CALLING SEQUENCE:
;   p = CHEBCOEF(FUNC, PRIVATE, FUNCTARGS=functargs, /DOUBLE, /EXPRESSION, $
;                PRECISION=prec, ERROR=err, NMAX=nmax, INTERVAL=interval, $
;                REDUCE_ALGORITHM=, STATUS=)
;
; DESCRIPTION:
;
;   CHEBCOEF estimates the coefficients for a finite sum of Chebyshev
;   polynomials approximating the function FUNC(x) over an interval.
;   The user can choose the desired precision and maximum number of
;   chebyshev coefficients.
;
;   This routine is intended for functions which can be evaluated to
;   full machine precision at arbitrary abcissae, and which are smooth
;   enough to ensure that the coefficients are a decreasing sequence.
;   For already-tabulated or potentially noisy data, the routines
;   CHEBGRID or CHEBFIT should be used instead.
;
;   The function to be approximated may either be the name of an IDL
;   function (the default behavior), or an IDL expression (using the
;   /EXPRESSION keyword).
;
;   The procedure uses a modified form of the classic algorithm for
;   determining the coefficients, which relies the orthogonality
;   relation for Chebyshev polynomials.  The interval [a,b] is
;   subdivided successively into sets of subintervals of length
;   2^(-k)*(b-a),(k = 0,1,2...). After each subdivision the
;   orthogonality properties of the Chebyshev polynomials with respect
;   to summation over equally-spaced points are used to compute two
;   sets of approximate values of the coefficients cj, one set
;   computed using the end-points of the subintervals, and one set
;   using the mid-points.  Certain convergence requirements must be
;   met before terminating.  If the routine fails to converge with 64
;   coefficents, then the current best-fitting coefficients are
;   returned, along with an error estimate in the ERROR keyword.
;   CHEBCOEF never returns more than 64 coefficients.
;
;   The coefficients may be further refined.  If the keyword
;   REDUCE_ALGORITHM is set to a value of 1, then any high order
;   coefficients below a certain threshold are discarded.  If
;   REDUCE_ALGORITHM is set to 2 (the default), then all coefficients
;   below the threshold are discarded rather than just the high order
;   ones.  The threshold is determined by the PRECISION keyword.
;
; INPUTS:
;
;   FUNC - a scalar string, the name of the function to be
;          approximated, or an IDL string containing an expression to
;          be approximated (if /EXPRESSION is set).
;
;  PRIVATE - any optional variable to be passed on to the function to
;            be integrated.  For functions, PRIVATE is passed as the
;            second positional parameter; for expressions, PRIVATE can
;            be referenced by the variable 'P'.  CHEBCOEF does not
;            examine or alter PRIVATE.
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
;   DOUBLE - if set, then computations are done in double precision
;            rather than single precision.
;
;   ERROR - upon return, this keyword contains an estimate of the
;           maximum absolute error in the approximation.
;
;   EXPRESSION - if set, then FUNC is an IDL expression to be
;                approximated, rather than the name of a function.
;
;   FUNCTARGS - A structure which contains the parameters to be passed
;               to the user-supplied function specified by FUNCT via
;               the _EXTRA mechanism.  This is the way you can pass
;               additional data to your user-supplied function without
;               using common blocks.  By default, no extra parameters
;               are passed to the user-supplied function.
;
;   INTERVAL - a 2-element vector describing the interval over which
;              the polynomial is to be evaluated.
;              Default: [-1, 1]
;
;   NMAX - a scalar, the maximum number of coefficients to be
;          estimated.   This number may not exceed 64. 
;          Default: 64
;
;   PRECISION - a scalar, the requested precision in the
;               approximation.  Any terms which do not contribute
;               significantly, as defined by this threshold, are
;               discarded.  If the function to be estimated is not
;               well-behaved, then the precision is not guaranteed to
;               reach the desired level.  Default: 1E-7
;
;   REDUCE_ALGORITHM - a scalar integer, describes how insignificant
;               terms are removed from the fit.  If 0, then all terms
;               are kept, and none are dicarded.  If 1, then only
;               trailing terms less than PRECISION are discarded.  If
;               2, then both trailing and intermediate terms less than
;               PRECISION are discarded.
;               Default: 2
;
;   STATUS - upon return, this keyword contains information about the
;            status of the approximation.  A value of -1 indicates bad
;            input values; a value of 0 indicates the required
;            accuracy was not obtained; a value of 1 indicates
;            success.
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
;   CERN, 1995, CERN Program Library, Function E406
;   Luke, Y. L., *The Special Functions and Their Approximations*,
;     1969, Academic Press, New York
;
; MODIFICATION HISTORY:
;   Written and documented, CM, June 2001
;   Copyright license terms changed, CM, 30 Dec 2001
;   Added usage message, CM, 20 Mar 2002
;   Changed docs slightly, CM, 25 Mar 2002
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

;; Evaluate a user-supplied expression
function chebcoef_eval, x, p, expression=expr, _EXTRA=extra
  y = 0
  cmd = 'Y = '+expr
  dummy = execute(cmd)
  return, y
end

function chebcoef, f0, priv, functargs=fa, double=double, error=err, $
                   nmax=nmax, interval=interval, precision=prec0, $
                   expression=expr, reduce_algorithm=redalg0, $
                   status=status, indices=igood

  if n_params() EQ 0 then begin
      message, 'USAGE:', /info
      message, 'P = CHEBCOEF(FUNCT, [PRIV,] INTERVAL=[a,b], NMAX=...)', /info
      return, !values.d_nan
  endif

  sz = size(f0)
  err = -1
  if sz(sz(0)+1) NE 7 OR n_elements(f0) NE 1 then begin
      NO_FUNCT:
      message, 'ERROR: FUNCT must be a scalar string', /info
      return, 0
  endif

  ;; Check for empty string
  f = strtrim(f0(0),2)
  if f EQ '' then goto, NO_FUNCT

  ;; Prepare for EXPRESSION if requested
  if keyword_set(expr) then begin
      f = 'CHEBCOEF_EVAL'
      fa = {expression: strtrim(f0(0),2)}
  endif else begin
      f = strtrim(f0(0),2)
  endelse

  ;; Handle error conditions gracefully
  if NOT keyword_set(nocatch) then begin
      catch, catcherror
      if catcherror NE 0 then begin
          catch, /cancel
          message, 'Error detected while approximating '+f, /info
          message, !err_string, /info

          errmsg = 0
          if NOT keyword_set(expr) then begin
              f1 = byte(strupcase(strtrim(f0(0),2)))
              ca = (byte('A'))(0) 
              cz = (byte('Z'))(0) 
              c0 = (byte('0'))(0) 
              c9 = (byte('9'))(0) 
              c_ = (byte('_'))(0) 
              wh = where((f1 GE ca AND f1 LE cz) EQ 0 AND f1 NE c_ $
                         AND (f1 GE c0 AND f1 LE c9) EQ 0, ct)
              if ct GT 0 OR (f1(0) GE c0 AND f1(0) LE c9) then begin
                  message, ('FUNCT appears to be an expression.  Did you '+$
                            'intend to pass the /EXPRESSION keyword?'), /info
                  errmsg = 1
              endif
          endif
          if errmsg EQ 0 then $
            message, ('Please verify that function works and conforms to '+$
                      'the documentation'), /info
          ier = -1L
          return, 0L
      endif
  endif

  if n_elements(prec0) EQ 0 then prec = 1e-7 else prec = prec0(0)
  zero = prec*0.
  if keyword_set(double) then zero = 0D
  if n_elements(interval) LT 2 then interval = zero + [-1., 1.]
  if n_elements(redalg0) EQ 0 then redalg = 2 else redalg = floor(redalg0(0))
  status = -1
  
  a = interval(0)
  b = interval(1)
  hf = zero + 0.5
  eps = prec
  z1 = zero + 1
  z2 = zero + 2
  sz = size(zero)
  if sz(sz(0)+1) EQ 5 then pi = !dpi else pi = !pi

  x0 = [a, b]
  if n_elements(priv) GT 0 then begin
      if n_elements(fa) GT 0 then fv = call_function(f, x0, priv, _EXTRA=fa) $
      else                        fv = call_function(f, x0, priv)
  endif else begin
      if n_elements(fa) GT 0 then fv = call_function(f, x0, _EXTRA=fa) $
      else                        fv = call_function(f, x0)
  endelse
  

  ALFA=HF*(B-A)
  BETA=HF*(B+A)
  C1=fv(0)
  C2=fv(1)
  AC = [C2+C1, C2-C1]
  BC = AC*0

  for i = 1, 7 do begin
      I1=2^(I-1)
      I2=I1-1
      I3=2*I1
      C1=Z2/I1
      C2=PI/I1

      jj = dindgen(i2+1)
      x = alfa*cos((jj+hf)*c2)+beta
      if n_elements(priv) GT 0 then begin
          if n_elements(fa) GT 0 then fv = call_function(f, x, priv, $
                                                         _EXTRA=fa) $
          else                        fv = call_function(f, x, priv)
      endif else begin
          if n_elements(fa) GT 0 then fv = call_function(f, x, _EXTRA=fa) $
          else                        fv = call_function(f, x)
      endelse
      c = fv

      ;; Compute B-coefficients
      for j = 0L, i2 do begin
          F1=J*C2
          F2=-HF*F1
      
          C3=2*COS(F1)
          A2=zero
          A1=zero
          A0=C(I2)

          for K = I2-1,0L,-1 do begin
              A2=A1
              A1=A0
              A0=C(K)+C3*A1-A2
          endfor
          BC(J)=C1*(A0*COS(F1+F2)-A1*COS(F2))
          BC(I1)=zero
      endfor

      c = hf*[ac(0:i1-1)+bc(0:i1-1), rotate(ac(0:i1)-bc(0:i1),2)]
      cc = abs(c)
      cmx = max(cc)
      
      if (CMX GT 0) THEN begin
          CMX=1/CMX
          CC(I3)=HF*CC(I3)
          A0=CC(I2)*CMX
          A1=CC(I1)*CMX
          for J = I1+2,I3 do begin
              A2=CC(J)*CMX
              IF(A0 LE EPS AND A1 LE EPS AND A2 LE EPS) THEN $
                goto, CHEB9
              A0=A1
              A1=A2
          endfor
      ENDIF

      ;; DOUBLE THE NUMBER OF COEFFICIENTS.      
      if i LT 7 then begin
          ac = c(0:i3)
          bc = ac*0
      endif
  endfor

  ;; REQUIRED ACCURACY NOT OBTAINED

  NC=64
  DELTA=total(abs(c(60:nc)))
  message, 'WARNING: Required accuracy not obtained', /info
  status = 0
  goto, CLEANUP

  CHEB9:
  ;; REQUIRED ACCURACY OBTAINED
  ;; SUM NEGLECTED TERMS IN EXPANSION

  status = 1
  DELTA=total(cc(j:i3))

  ;;  CHECK IF FURTHER REDUCTION OF COEFFICIENTS IS POSSIBLE.

  NC=J-1
  REST=EPS-DELTA
  IF (REST GT 0) AND redalg GT 0 THEN begin
      while (CC(NC) LT REST) do begin
          DELTA=DELTA+CC(NC)
          REST=REST-CC(NC)
          NC=NC-1
      endwhile
  ENDIF

  CLEANUP:
  C(0)=HF*C(0)
  p = c(0:nc)
  rest = eps - delta

  if redalg EQ 2 then begin
      wh = where(cc(0:nc) LT prec, ct)
      i = ct-1
      while (i GE 0) AND ((rest GT 0) OR (status EQ 1)) do begin
          delta = delta + cc(wh(i))
          rest  = rest - cc(wh(i))
          p(wh(i)) = 0
          i = i - 1
      endwhile
  endif

  DONE:
  igood = where(p NE 0)
  err = delta
  RETURN, p
end

