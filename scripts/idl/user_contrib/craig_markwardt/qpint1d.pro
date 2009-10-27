;+
; NAME:
;   QPINT1D
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   One dimensional numerical integration of IDL function or expression
;
; MAJOR TOPICS:
;   Numerical Analysis.
;
; CALLING SEQUENCE:
;   value = QPINT1D(FUNCT, A, B, [ PRIVATE, /EXPRESSION, FUNCTARGS=, 
;                   ERROR=error, NFEV=nfev, STATUS=status, NSUBINTERVALS=nsub,
;                   EPSABS=, EPSREL=, LIMIT=, BREAKPOINTS=, NPOINTS=
;                   /SYMMETRIC, SYM_AXIS= ] )
;
; DESCRIPTION:
;
;  QPINT1D adaptively calculates an approximation result to a given
;  definite integral
;
;       result = Integral[ f(x) dx ] over [a,b]
;
;  hopefully satisfying a constraint on the accuracy of the solution.
;  QPINT1D is based on the QUADPACK fortran package originally by
;  Piessens, de Doncker, Ueberhuber and Kahaner (and implements
;  equivalents to the QAGSE, QAGPE, QAGIE, and DQKxx fortran routines).
;
;  The returned result is intended to satisfy the following claim for
;  accuracy: ABS(result-value) LE MAX([epsabs, epsrel*ABS(value)]),
;  where VALUE is the true value of the integral, and EPSABS and
;  EPSREL are the absolute and relative error tolerances defined
;  below.  An estimate of the error is returned in the ERROR keyword.
;  Either A or B may be finite or infinite (i.e., an improper
;  integral).
;
;  QPINT1D is "adaptive" in the sense that it locates regions of the
;  integration interval which contain the highest error, and focusses
;  its efforts on those regions.  The algorithm locates these regions
;  by successively bisecting the starting interval.  Each subinterval
;  is assigned an error estimate, and the region with the largest
;  error estimate is subdivided further, until each subinterval
;  carries approximately the same amount of error.  Convergence of the
;  procedure may be accelerated by the Epsilon algorithm due to Wynn.
;
;  The estimate of the integral and the estimate of the error in each
;  subinterval are computed using Gauss Kronrod quadrature.
;  Integrators based on the 15-, 21-, 31-, 41-, 51- and 61-point
;  Gauss-Kronrod rule are available, and selected using the NPOINTS
;  keyword.  Generally, the more points the greater the precision,
;  especially for rapidly varying functions.  However the default
;  value of 21 is often sufficient, especially because of the adaptive
;  nature of QPINT1D.
;
;  In the following sections the requirements for the form of the
;  integrand are established.  Also, a description of how QPINT1D
;  handles singularities and discontinuities is presented.
;
; INTEGRAND is a FUNCTION
;
;  The integrand can be specified in two forms, either as a standard
;  IDL function, or as an IDL expression.  If integrating a function,
;  then the FUNCT should be a string naming the function.  The
;  function must be declared as following:
;
;    FUNCTION MYFUNCT, X, P, KEYWORDS=...
;      RETURN, (compute function of X and P)
;    END
;
;  The function must accept at least one, but optionally two,
;  parameters.  The first, 'X', is a vector of abcissae where the
;  function is to be computed.  The function must return the same
;  number of function values as abcissae passed.  The second
;  positional parameter, 'P', is a purely optional PRIVATE parameter
;  as described below.  MYFUNCT may accept more positional parameters,
;  but QPINT1D will not use them.  The difference between X and P is
;  that X is the variable of integration, while P contains any other
;  information expected to remain essentially constant over the
;  integration.
;
; INTEGRAND is an EXPRESSION
;
;  The integrand can also be specfied as an IDL expression and setting
;  the EXPRESSION keyword.  Any expression that can accept a vector of
;  abcissae named 'X' and produce a corresponding vector of output is
;  a valid expression.  Here is an example:
;
;    RESULT = QPINT1D('X^2 * EXP(-X)', /EXPRESSION, 0D, 10D)
;
;  It is important to note that the variable of integration must
;  always be named 'X', and the expression must be vectorizable.  The
;  expression may also use the PRIVATE data, and as above, it would be
;  referred to according to the variable 'P'.  For example, if the
;  exponential decay constant is parameterized by PRIVATE(0), then the
;  expression would be:
;
;    RESULT = QPINT1D('X^2 * EXP(-X/P(0))', /EXPRESSION, 0D, 10D, PRIVATE)
;
;  The user is solely responsible for defining and using the PRIVATE
;  data.  QPINT1D does not access or modify PRIVATE / P; it only
;  passes it on to the user routine for convenience.
;
; IMPROPER INTEGRALS and DISCONTINUITIES
;
;  QPINT1D computes improper integrals, as well as integrands with
;  discontinuities or singularities.  
;
;  Improper integrals are integrals where one or both of the limits of
;  integration are "infinity."  (Formally, these integrals are defined
;  by taking the limit as the integration limit tends to infinity).
;  QPINT1D handles a small class of such integrals, generally for
;  integrands that are convergent and monotonic (i.e.,
;  non-oscillatory, and falling off as 1/ABS(X)^2 or steeper).  Such
;  integrals are handled by a transformation of the original interval
;  into the interval [0,1].
;
;  Integrals from negative infinity to positive infinity are done in
;  two subintervals.  By default the interval is split at X EQ 0,
;  however this can be controlled by using the SYM_AXIS keyword.
;  Users should note that if the first subinterval fails the second is
;  not attempted, and thus the return value VALUE should not be
;  trusted in those cases.
;
;  Infinite integration limits are specified by using the standard
;  values !VALUES.F_INFINITY or !VALUES.D_INFINITY.  No other special
;  invocation syntax is required.
;
;  The integration routine is able to handle integrands which have
;  integrable singularities at the endpoints.  For example, the
;  integral:
;
;    RESULT = QPINT1D('2*sqrt((1-x)/(1+x))/(1-x^2)', 0.0d, 1d, /expr)
;
;  has a singularity at a value of X EQ 1.  Still, the singularity is
;  integrable, and the value returned is a correct value of 2.
;
;  If known singularities are present within the interval of
;  integration, then users should pass the BREAKPOINTS keyword to list
;  the locations of these points.  QPINT1D will then integrate each
;  subinterval separately, while still maintaining an overall error
;  budget.
;
;  If known discontinuities exist in the integrand, then the user may
;  additionally list those points using the BREAKPOINTS keyword.
;
;  It should be noted that the algorithm used is different, depending
;  on whether the BREAKPOINTS keyword has been specified or not (this
;  is the difference between the QAGSE vs. QAGPE routines in the
;  original FORTRAN).  The algorithm *without* BREAKPOINTS is
;  generally thought to be more precise than *with*.  Thus, it may be
;  worth splitting the original integration interval manually and
;  invoking QPINT1D without BREAKPOINTS.
;
;
; INPUTS:
;
;  FUNCT - by default, a scalar string containing the name of an IDL
;          function to be integrated.  See above for the formal
;          definition of MYFUNCT.  (No default).
;
;          If the EXPRESSION keyword is set, then FUNCT is a scalar
;          string containing an IDL expression to be evaluated, as
;          described above.
;
;  A, B - a scalar number indicating the lower and upper limits of the
;         interval of integration (i.e., [A, B] is the interval of
;         integration).
;
;  PRIVATE - any optional variable to be passed on to the function to
;            be integrated.  For functions, PRIVATE is passed as the
;            second positional parameter; for expressions, PRIVATE can
;            be referenced by the variable 'P'.   QPINT1D does not
;            examine or alter PRIVATE.
;
; RETURNS:
;
;   The value of the integral.  If either A or B are double precision,
;   then the integral is computed in double precision; otherwise the
;   result is returned in single precision floating point.
;
; KEYWORD PARAMETERS:
;
;   BREAKPOINTS - an array of numbers specifying points within the
;                 integration interval where the integrand is
;                 discontinuous or singular.  Out of bounds points are
;                 ignored.
;                 Default: undefined, i.e., no such points
;
;   EPSABS - a scalar number, the absolute error tolerance requested
;            in the integral computation.  If less than or equal to
;            zero, then the value is ignored.
;            Default: 0
;
;   EPSREL - a scalar number, the relative (i.e., fractional) error
;            tolerance in the integral computation.  If less than or
;            equal to zero, then the value is ignored.
;            Default: 1e-4 for float, 1d-6 for double
;
;   EXPRESSION - if set, then FUNCT is an IDL expression.  Otherwise,
;                FUNCT is an IDL function.
;
;   ERROR - upon return, this keyword contains an estimate of the
;           error in the computation.
;
;   FUNCTARGS - A structure which contains the parameters to be passed
;               to the user-supplied function specified by FUNCT via
;               the _EXTRA mechanism.  This is the way you can pass
;               additional data to your user-supplied function without
;               using common blocks.  By default, no extra parameters
;               are passed to the user-supplied function.
;
;   LIMIT - a scalar, the maximum number of subintervals to create
;           before terminating execution.  Upon return, a STATUS value
;           of 1 indicates such an overflow occurred.
;           Default: 100
;
;   NFEV - upon return, this keyword contains the number of function
;          calls executed (i.e., the number of abcissae).
;
;   NPOINTS - a scalar, the number of Gauss Kronrod points to use in
;             computing the integral over a subinterval.  A larger
;             number of points can in principle increase the precision
;             of the integral, but also makes the computation take
;             longer.  Possible values are 15, 21, 31, 41, 51, and 61.
;             NPOINTS is rounded up to the next nearest available set,
;             with a maximum of 61.
;             Default: 21
;
;   NSUBINTERVALS - upon return, this keyword contains the number of
;                   subintervals the integration interval was divided
;                   into.
;
;   STATUS - upon return, the status of the integration operation is
;            returned in this keyword as an integer value.  A value of
;            zero indicates success; otherwise an abnormal condition
;            has occurred and the returned value should be considered
;            erroneous or less reliable according to STATUS:
;
;                 any negative number - outright failure (reserved for
;                                       future use).
;
;                 -1 - the input parameters are invalid, because
;                      epsabs LE 0 and epsrel LT max([50*EPS,0.5d-28]),
;                      where EPS is the machine precision, or if LIMIT
;                      is smaller than the number of BREAKPOINTS.
;
;                  0 - success.
;
;                  1 - maximum number of subdivisions allowed has been
;                      achieved. One can allow more subdivisions by
;                      increasing the value of limit (and taking the
;                      according dimension adjustments into
;                      account). However, if this yields no
;                      improvement it is advised to analyze the
;                      integrand in order to determine the integration
;                      difficulties.  If the position of a local
;                      difficulty can be determined (i.e. singularity,
;                      discontinuity within the interval), it should
;                      be supplied to the routine as an element of the
;                      vector BREAKPOINTS.
;
;                  2 - The occurrence of roundoff error is detected,
;                      which prevents the requested tolerance from
;                      being achieved.  The error may be
;                      under-estimated.
;
;                  3 - Extremely "bad" integrand behaviour occurs at
;                      some points of the integration interval.
;
;                  4 - The algorithm does not converge.  Roundoff
;                      error is detected in the extrapolation table.
;                      It is presumed that the requested tolerance
;                      cannot be achieved, and that the returned
;                      result is the best which can be obtained.
;
;                  5 - The integral is probably divergent, or only
;                      slowly convergent.  It must be noted that
;                      divergence can occur with any other value of
;                      ier GT 0.
;
;   SYM_AXIS - a scalar number, the bisection point of the real line
;              for improper integrals from negative infinity to
;              positive infinity.  Otherwise ignored.
;              Default: 0.
;
;   
; EXAMPLES:
;
;  Shows how function and expression can be used for exponential
;  integrand:
;
;    IDL> print, qpint1d('EXP(X)', 0D, 10D, /expr)
;           22025.466
;    IDL> print, qpint1d('EXP', 0D, 10D)
;           22025.466
;
;  Normal definite integral, and then parameterized using a PRIVATE
;  value of 2.
;    IDL> print, qpint1d('X^2*EXP(-X)', 0D, 10D, /expr)
;           1.9944612
;    IDL> print, qpint1d('X^2*EXP(-X/P(0))', 0D, 10D, 2D, /expr)
;           14.005568
;
;  Improper integrals of the gaussian function
;    IDL> inf = !values.d_infinity
;    IDL> print, qpint1d('EXP(-X^2)', 0D, +inf, 2D, /expr)
;          0.88622693
;    IDL> print, qpint1d('EXP(-X^2)', -inf, +inf, 2D, /expr), sqrt(!dpi)
;           1.7724539       1.7724539
;  The second integral shows the comparison to the analytic value of
;  SQRT(!DPI).
;
; COMMON BLOCKS:
;
;   COMMON QPINT1D_MACHAR
;   COMMON QPINT1D_PROFILE_COMMON
;   COMMON QPINT1D_QKEVAL_COMMON
;
;   These common blocks are used internally only and should not be
;   accessed or modified.
;
; REFERENCES:
;
;    R. Piessens, E. deDoncker-Kapenga, C. Uberhuber, D. Kahaner
;    Quadpack: a Subroutine Package for Automatic Integration
;    Springer Verlag, 1983.    Series in Computational Mathematics v.1
;    515.43/Q1S  100394Z
;
;    Netlib repository: http://www.netlib.org/quadpack/
;
;    SLATEC Common Mathematical Library, Version 4.1, July 1993
;    a comprehensive software library containing over
;    1400 general purpose mathematical and statistical routines
;    written in Fortran 77.  (http://www.netlib.org/slatec/)
;
; MODIFICATION HISTORY:
;   Written, Feb-Jun, 2001, CM
;   Documented, 04 Jun, 2001, CM
;   Add usage message, error checking, 15 Mar 2002, CM
;   Correct usage message, 28 Apr 2002, CM
;
;  $Id$
;-
; Copyright (C) 2001, 2002, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

FORWARD_FUNCTION qpint1d_eval, qpint1d

; --------------------------------------------------------------
; Some functions used in testing the algorithm
;
; Test functions
; .COMP
; FUNCTION BESTEST, X, N
;  RETURN, BESELJ(ABS(X-10)+0.001, N)
; END
; print, qpint1d('BESTEST', .1d, 35d, 1L, nev=nev, nsub=nsub, status=ier)

; .comp
; function invsq, x
;   return, 1/(abs(x-10)+0.0001)^2
; end
; print, qpint1d('INVSQ', 0d, 50d, nev=nev, nsub=nsub, status=ier, error=err)

; .comp 
; function broken, x
;   f = x*0
;   wh = where(x LT 0, ct)
;   if ct GT 0 then f(wh) = exp(x(wh))
;   wh = where(x GE 0 AND x LT 10, ct)
;   if ct GT 0 then f(wh) = x(wh)^2
;   wh = where(x GE 10, ct)
;   if ct GT 0 then f(wh) = exp(-x(wh))
;   return, f
; end
; print, qpint1d('BROKEN', -10d, 50d, nev=nev, nsub=nsub, status=ier, error=err) & print, err, ier, nsub


;; Function used in testing speed
pro qpint1d_profile, clear=clear
  common qpint1d_profile_common, profvals
;  if n_elements(profvals) EQ 0 OR keyword_set(clear) then $
;    profvals = {functime: 0D, tottime: 0D, srttime: 0D, acctime: 0D, $
;                qktime: 0D}
end

;; Following are machine constants that can be loaded once.  I have
;; found that bizarre underflow messages can be produced in each call
;; to MACHAR(), so this structure minimizes the number of calls to
;; one.

pro qpint1d_setmachar, double=isdouble
  common qpint1d_machar, qpint1d_machar_vals

  ;; In earlier versions of IDL, MACHAR itself could produce a load of
  ;; error messages.  We try to mask some of that out here.
  if (!version.release) LT 5 then dummy = check_math(1, 1)

  mch = 0.
  mch = machar(double=keyword_set(isdouble))
  dmachep = mch.eps
  dmaxnum = mch.xmax
  dminnum = mch.xmin
  dmaxlog = alog(mch.xmax)
  dminlog = alog(mch.xmin)
  if keyword_set(isdouble) then $
    dmaxgam = 171.624376956302725D $
  else $
    dmaxgam = 171.624376956302725
  drdwarf = sqrt(dminnum*1.5) * 10
  drgiant = sqrt(dmaxnum) * 0.1

  qpint1d_machar_vals = {machep: dmachep, maxnum: dmaxnum, minnum: dminnum, $
                       maxlog: dmaxlog, minlog: dminlog, maxgam: dmaxgam, $
                       rdwarf: drdwarf, rgiant: drgiant}

  if (!version.release) LT 5 then dummy = check_math(0, 0)

  return
end


;; --------------------------------------------------------
;; Main workhorse routine
pro qpint1d_qagse, f, a0, b0, result, abserr, private, functargs=fa, $
                   epsabs=epsabs, epsrel=epsrel, npoints=npts0, $
                   status=ier, limit=limit, neval=neval, nsubintervals=last, $
                   breakpoints=bpoints0, isdouble=isdouble, $
                   alist=alist, blist=blist, rlist=rlist, $
                   elist=elist, iord=iord
                   

  ;; Derived from QUADPACK QAGSE
  ;;***PURPOSE  The routine calculates an approximation result to a given
  ;;            definite integral I = Integral of F over (A,B),
  ;;            hopefully satisfying following claim for accuracy
  ;;            ABS(I-RESULT).LE.MAX(EPSABS,EPSREL*ABS(I)).
  ;;***LIBRARY   SLATEC (QUADPACK)

  common qpint1d_machar, machvals
  r1mach1 = machvals.minnum
  r1mach2 = machvals.maxnum
  r1mach4 = machvals.machep

  ;; Establish the precision we will be computing in, base on the
  ;; integration limits.
  a = a0
  b = b0
  zero = a*0.
  if n_elements(isdouble) EQ 0 then begin
      sz = size(zero)
      if sz(sz(0)+1) EQ 5 then isdouble = 1 else isdouble = 0
  endif
  isdouble = isdouble(0)

  ;; Establish default values
  if n_elements(neval) EQ 0 then neval = 0L
  if n_elements(limit) EQ 0 then limit = 100L
  if n_elements(epsabs) EQ 0 then epsabs = zero
  if n_elements(epsrel) EQ 0 then epsrel = zero + 1e-6
  if n_elements(npts0) EQ 0 then npoints = 21L $
  else                           npoints = floor(npts0(0))
  rlist2 = make_array(52, value=zero)
  res3la = rlist2(0:2)

  ;; These are the work arrays which manage the subintervals
  alist = make_array(limit, value=zero)
  blist = alist
  rlist = alist
  elist = alist
  iord = lonarr(limit)
  level = iord

  epmach = r1mach4

;            TEST ON VALIDITY OF PARAMETERS
  ier = 0L
  neval = 0
  last = 0
  result = zero
  abserr = zero

  ;; Extract the valid breakpoints
  nbp0 = n_elements(bpoints0)
  nbp = nbp0
  if nbp GT 0 then begin
      abmin = min([a,b], max=abmax)
      wh = where(bpoints0 GT abmin(0) AND bpoints0 LT abmax(0), nbp)
      if nbp GT 0 then begin
          ;; Valid breakpoints were found.  Sort them into place.
          bpoints = bpoints0(wh)
          if a LE b then bpoints = [a, bpoints(sort(bpoints)),  b] $
          else           bpoints = [a, bpoints(sort(-bpoints)), b]
          nbp = nbp + 2
          if nbp GE limit then goto, INPUT_ERROR
      endif
  endif
  if nbp EQ 0 then begin
      ;; By default, we have two breakpoints, namely the start and
      ;; stop of the interval.
      bpoints = [a, b]
      nbp = 2L
  endif

  ;; Search for infinite bounds
  dirsign = 1
  if finite(b) EQ 0 then begin
      bound = a
      dirsign = 2*(b GT 0) - 1
      a = zero
      b = zero + 1

      ;; Transform the breakpoints into the [0,1] interval
      bpoints = rotate(1/((bpoints-bound(0))*dirsign + 1),2)
  endif

  alist(0) = a
  blist(0) = b
  rlist(0) = zero
  elist(0) = zero
  xnum = (a(0)*b(0)*0.)
  sz = size(xnum)
  if sz(sz(0)+1) EQ 5 then xlim = 0.5d-28 else xlim = 0.5d-14

  if (epsabs LE 0 AND epsrel LT max([0.5e2*epmach,xlim])) then begin
      INPUT_ERROR:
      ier = 6L
      goto, LAB999
  endif

  nint = nbp-1
  npts = nbp-2
  npts2 = nbp

;           FIRST APPROXIMATION TO THE INTEGRAL
  uflow = r1mach1
  oflow = r1mach2
  resabs = zero
  a1 = bpoints(0)
  ndin = iord * 0
  for i = 0L,nint-1 do begin
      b1 = bpoints(i+1)

      qpint1d_qkeval, f,a1,b1,area1,error1,defabs,resa, private, $
        neval=neval, functargs=fa, inflow=bound, dirsign=dirsign, $
        isdouble=isdouble, npoints=npoints

      abserr = abserr+error1
      result = result+area1
      ndin(i) = (error1 EQ resa AND error1 NE zero)
      resabs = resabs+defabs
      level(i) = 0
      elist(i) = error1
      alist(i) = a1
      blist(i) = b1
      rlist(i) = area1
      iord(i) = i+1
      a1 = b1
  endfor

  if nbp0 GT 0 then begin
      wh = where(ndin EQ 1, ct)
      if ct GT 0 then elist(wh) = abserr
  endif
  errsum = total(elist(0:nint-1))

;           TEST ON ACCURACY.
  last = nint
  dres = abs(result)
  errbnd = max([epsabs,epsrel*dres])
  if (abserr LE 1.0e+02*epmach*resabs) AND (abserr GT errbnd) then $
    ier = 2L

  ;; Sort in descending order
  iord(0:nint-1) = sort(-elist(0:nint-1)) + 1

  if (limit LT npts2) then ier = 1L
  if (ier NE 0 OR abserr LE errbnd) then goto, LAB999

;           INITIALIZATION
  rlist2(1-1) = result
  maxerr = iord(0)
  errmax = elist(maxerr-1)
  area = result
  nrmax = 1L
  nres = 0L
  if nbp0 EQ 0 then begin
      numrl2 = 2L
      errsum = abserr
  endif else begin
      numrl2 = 1L
      erlarg = errsum
  endelse
  ktmin = 0
  extrap = 0
  noext = 0
  ertest = errbnd
  levmax = 1L
  iroff1 = 0
  iroff2 = 0
  iroff3 = 0
  ierro = 0
  abserr = oflow
  ksgn = -1
  if (dres GE (0.1E+01-0.5E+02*epmach)*resabs) then ksgn = 1
  
;           MAIN DO-LOOP
  for last = npts2, limit do begin

;           BISECT THE SUBINTERVAL WITH THE NRMAX-TH LARGEST
;           ERROR ESTIMATE.
      levcur = level(maxerr)+1
      a1 = alist(maxerr-1)
      b1 = 0.5e+00*(alist(maxerr-1)+blist(maxerr-1))
      a2 = b1
      b2 = blist(maxerr-1)
      erlast = errmax
      qpint1d_qkeval, f,a1,b1,area1,error1,resa,defab1, private, $
        neval=neval, functargs=fa, inflow=bound, dirsign=dirsign, $
        isdouble=isdouble, npoints=npoints
      qpint1d_qkeval, f,a2,b2,area2,error2,resa,defab2, private, $
        neval=neval, functargs=fa, inflow=bound, dirsign=dirsign, $
        isdouble=isdouble, npoints=npoints

;           IMPROVE PREVIOUS APPROXIMATIONS TO INTEGRAL
;           AND ERROR AND TEST FOR ACCURACY.
      area12 = area1+area2
      erro12 = error1+error2
      errsum = errsum+erro12-errmax
      area = area+(area12-rlist(maxerr-1))
      if NOT (defab1 EQ error1 OR defab2 EQ error2) then begin
          
          if NOT (abs(rlist(maxerr-1)-area12) GT 0.1e-04*abs(area12) $
                  OR erro12 LT 0.99e+00*errmax) then begin
              if (extrap) then iroff2 = iroff2+1
              if (NOT extrap) then iroff1 = iroff1+1
          endif
          if(last GT 10 AND erro12 GT errmax) then iroff3 = iroff3+1
      endif
      level(maxerr-1) = levcur
      level(last-1) = levcur
      rlist(maxerr-1) = area1
      rlist(last-1) = area2
      errbnd = max([epsabs,epsrel*abs(area)])

;           TEST FOR ROUNDOFF ERROR AND EVENTUALLY
;           SET ERROR FLAG.
      if (iroff1+iroff2 GE 10 OR iroff3 GE 20) then ier = 2L
      if (iroff2 GE 5) then ierro = 3

;           SET ERROR FLAG IN THE CASE THAT THE NUMBER OF
;           SUBINTERVALS EQUALS LIMIT.
      if last EQ limit then ier = 1L
      
;           SET ERROR FLAG IN THE CASE OF BAD INTEGRAND BEHAVIOUR
;           AT A POINT OF THE INTEGRATION RANGE.
      if (max([abs(a1),abs(b2)]) LE (0.1e+01+0.1e+03*epmach)* $
          (abs(a2)+0.1e+04*uflow)) then ier = 4L

;           APPEND THE NEWLY-CREATED INTERVALS TO THE LIST.
      if (error2 LE error1) then begin ;; IDL OK
          alist(last-1) = a2
          blist(maxerr-1) = b1
          blist(last-1) = b2
          elist(maxerr-1) = error1
          elist(last-1) = error2
      endif else begin
          alist(maxerr-1) = a2
          alist(last-1) = a1
          blist(last-1) = b1
          rlist(maxerr-1) = area2
          rlist(last-1) = area1
          elist(maxerr-1) = error2
          elist(last-1) = error1
      endelse

;           CALL SUBROUTINE QPSRT TO MAINTAIN THE DESCENDING ORDERING
;           IN THE LIST OF ERROR ESTIMATES AND SELECT THE
;           SUBINTERVAL WITH NRMAX-TH LARGEST ERROR ESTIMATE (TO BE
;           BISECTED NEXT).

      qpint1d_qpsrt, limit,last,maxerr,errmax,elist,iord,nrmax

; ***JUMP OUT OF DO-LOOP
      if (errsum LE errbnd) then goto, LAB115
; ***JUMP OUT OF DO-LOOP
      if (ier NE 0) then goto, LAB100
      if (nbp0 EQ 0 AND last EQ 2) then goto, LAB80
      if (noext) then goto, LAB90
      erlarg = erlarg-erlast
      if (nbp0 EQ 0) then begin
          if (abs(b1-a1) GT small) then erlarg = erlarg+erro12 
      endif else begin
          if (levcur+1 LE levmax) then erlarg = erlarg+erro12
      endelse
      if NOT (extrap) then begin
          bool = 0
          if nbp0 EQ 0 then begin
              if (abs(blist(maxerr-1)-alist(maxerr-1)) GT small) then $
                goto, LAB90
          endif else begin
              if (level(maxerr-1)+1 LE levmax) then goto, LAB90
          endelse
          extrap = 1
          nrmax = 2L
      endif
      
      if NOT (ierro EQ 3 OR erlarg LE ertest) then begin
;
;           THE SMALLEST INTERVAL HAS THE LARGEST ERROR.
;           BEFORE BISECTING DECREASE THE SUM OF THE ERRORS
;           OVER THE LARGER INTERVALS (ERLARG) AND PERFORM
;           EXTRAPOLATION.

          id = nrmax
          jupbnd = last
          if (last GT (2+limit/2)) then jupbnd = limit+3-last
          for k = id, jupbnd do begin
              maxerr = iord(nrmax-1)
              errmax = elist(maxerr-1)
; ***JUMP OUT OF DO-LOOP
              bool = 0
              if nbp0 EQ 0 then begin
                  if (abs(blist(maxerr-1)-alist(maxerr-1)) GT small) then $
                    goto, LAB90
              endif else begin
                  if (level(maxerr-1)+1 LE levmax) then goto, LAB90
              endelse
              nrmax = nrmax+1
          endfor
      endif

;           PERFORM EXTRAPOLATION.
      numrl2 = numrl2+1
      rlist2(numrl2-1) = area
      if (nbp0 GT 0 AND numrl2 LE 2) then goto, LAB72
      qpint1d_qelg, numrl2,rlist2,reseps,abseps,res3la,nres
      ktmin = ktmin+1
      if (ktmin GT 5 AND abserr LT 0.1e-02*errsum) then ier = 5L
      if NOT (abseps GE abserr) then begin
          ktmin = 0
          abserr = abseps
          result = reseps
          correc = erlarg
          ertest = max([epsabs,epsrel*abs(reseps)])
; ***JUMP OUT OF DO-LOOP
          if (abserr LE ertest) then goto, LAB100
      endif

;           PREPARE BISECTION OF THE SMALLEST INTERVAL.
      if (numrl2 EQ 1) then noext = 1
      if (ier EQ 5) then goto, LAB100

      LAB72:
      maxerr = iord(0)
      errmax = elist(maxerr-1)
      nrmax = 1L
      extrap = 0
      if nbp0 EQ 0 then small = small*0.5e+00
      levmax = levmax + 1
      erlarg = errsum
      goto, LAB90

      LAB80:
      small = abs(b-a)*0.375e+00
      erlarg = errsum
      ertest = errbnd
      rlist2(2-1) = area
      
      LAB90:
  endfor
      
  LAB100:
;           SET FINAL RESULT AND ERROR ESTIMATE.
  if abserr EQ oflow then goto, LAB115
  if ier+ierro EQ 0 then goto, LAB110
  if ierro EQ 3 then abserr = abserr+correc
  if ier EQ 0 then ier = 3L
  if NOT (result NE zero AND area NE zero) then begin
      if abserr GT errsum then goto, LAB115
      if area EQ 0 then goto, LAB130
      goto, LAB110
  endif
  if(abserr/abs(result) GT errsum/abs(area)) then goto, LAB115

;           TEST ON DIVERGENCE.
  LAB110:
  if (ksgn EQ (-1) AND max([abs(result),abs(area)]) LE $
      resabs*0.1e-01) then goto, LAB130
  if (0.1e-01 GT (result/area) OR (result/area) GT 0.1e+03 $
      OR errsum GT abs(area)) then ier = 6L
  goto, LAB130

  LAB115:
  result = total(rlist(0:last-1))
  abserr = errsum

  LAB130:
  if (ier GT 2) then ier = ier-1

  LAB999:
  result = result * dirsign
  return
end

;; Originally DQK21.F, this routine performs Gauss-Kronrod quadrature
;; using 15, 21, 31, 41, 51 or 61 points.
pro qpint1d_qkeval, f, a, b, result, abserr, resabs, resasc, priv, $  
                  functargs=fa, neval=neval, reset=reset, isdouble=isdouble, $
                  inflow=bound, dirsign=dirsign, npoints=npts0

  ;; Derived from QUADPACK QK21-QK61, and QK15I
  ;;***PURPOSE  To compute I = Integral of F over (A,B), with error
  ;;                           estimate
  ;;                       J = Integral of ABS(F) over (A,B)
  ;;***LIBRARY   SLATEC (QUADPACK)

  common qpint1d_qkeval_common, wg, wgk, xgk, ig, nptsreq, nptsact, prec

  if n_elements(npts0) EQ 0 then npts0 = 21L
  if n_elements(isdouble) EQ 0 then isdouble = 1

  ;; Determine the number of points "requested", versus the number
  ;; "actually" granted.
  if n_elements(nptsreq) EQ 0 OR keyword_set(reset) then begin
      nptsreq = 0L
      prec = -1L
  endif
  if npts0(0) NE nptsreq OR isdouble(0) NE prec then begin
      ;; If this is the first time around, or if we need to use a
      ;; different set of GK points, then request the new set of
      ;; points from GKWEIGHTS.
      nptsreq = npts0(0)
      prec = isdouble(0) EQ 1
      qpint1d_gkweights, wg, wgk, xgk, ig, nptsreq, nptsact, prec
      if keyword_set(reset) then return
  endif

  common qpint1d_machar, machvals
  r1mach1 = machvals.minnum
  r1mach2 = machvals.maxnum
  r1mach4 = machvals.machep
  common qpint1d_profile_common, profvals
;  prof_start1 = systime(1)

  if n_elements(neval) EQ 0 then neval = 0L
  epmach = r1mach4
  uflow = r1mach1

  zero = a*b*0.
  centr = 0.5e+00*(a+b)
  hlgth = 0.5e+00*(b-a)
  dhlgth = abs(hlgth)

  x = centr+hlgth*xgk
  if n_elements(bound) GT 0 then begin
      ;; Transformation for infinite integrals
      u = temporary(x)
      x = bound(0) + dirsign*(1/u - 1)
  endif

;  prof_start2 = systime(1)
  ;; Call with or without PRIVATE and _EXTRA keywords
  if n_elements(priv) GT 0 then begin
      if n_elements(fa) GT 0 then fv = call_function(f, x, priv, _EXTRA=fa) $
      else                        fv = call_function(f, x, priv)
  endif else begin
      if n_elements(fa) GT 0 then fv = call_function(f, x, _EXTRA=fa) $
      else                        fv = call_function(f, x)
  endelse
;  profvals.functime = profvals.functime + (systime(1) - prof_start2)
  neval = neval + n_elements(x)
  if n_elements(fv) NE n_elements(x) then $
    message, 'ERROR: Integrand function '+strupcase(f)+$
    ' must return a vector of values'
  
  if n_elements(bound) GT 0 then begin
      ;; Complete the transformation for infinite integrals
      fv = fv / u^2
  endif

  resk = total(wgk*fv)
  resabs = total(wgk*abs(fv))
  resg = total(wg*fv(ig))

  resasc = total(wgk*abs(fv-resk*0.5e+00))

  result = resk*hlgth
  resabs = resabs*dhlgth
  resasc = resasc*dhlgth
  abserr = abs((resk-resg)*hlgth)
  if (resasc NE 0.0e+00 AND abserr NE 0.0e+00) then $
    abserr = resasc*min([0.1e+01,(0.2e+03*abserr/resasc)^1.5])
  if (resabs GT uflow/(0.5e+02*epmach)) then $
    abserr = max([(epmach*0.5e+02)*resabs,abserr])
;  profvals.qktime = profvals.qktime + (systime(1) - prof_start1)
  return
end



pro qpint1d_qelg, n, epstab, result, abserr, res3la, nres

  ;; Derived from QUADPACK QELG
  ;;***PURPOSE  The routine determines the limit of a given sequence of
  ;;            approximations, by means of the Epsilon algorithm of
  ;;            P. Wynn. An estimate of the absolute error is also given.
  ;;            The condensed Epsilon table is computed. Only those
  ;;            elements needed for the computation of the next diagonal
  ;;            are preserved.
  ;;***LIBRARY   SLATEC

  common qpint1d_machar, machvals
  r1mach1 = machvals.minnum
  r1mach2 = machvals.maxnum
  r1mach4 = machvals.machep
  common qpint1d_profile_common, profvals
;  prof_start = systime(1)

  epmach = r1mach4
  oflow = r1mach2
  nres = nres+1
  abserr = oflow
  result = epstab(n-1)
  if (n LT 3) then goto, LAB100
  limexp = 50
  epstab(n+2-1) = epstab(n-1)
  newelm = (n-1)/2
  epstab(n-1) = oflow
  num = n
  k1 = n-1  ;; OK IDL 
  for i = 1, newelm do begin
      res = epstab(k1+2) ;; OK IDL
      e0 = epstab(k1-2)  ;; OK IDL
      e1 = epstab(k1-1)  ;; OK IDL
      e2 = res
      e1abs = abs(e1)
      delta2 = e2-e1
      err2 = abs(delta2)
      tol2 = max([abs(e2),e1abs])*epmach
      delta3 = e1-e0
      err3 = abs(delta3)
      tol3 = max([e1abs,abs(e0)])*epmach

      if NOT (err2 GT tol2 OR err3 GT tol3) then begin
;           IF E0, E1 AND E2 ARE EQUAL TO WITHIN MACHINE
;           ACCURACY, CONVERGENCE IS ASSUMED.
          result = res
          abserr = err2+err3
; ***JUMP OUT OF DO-LOOP
          goto, LAB100
      endif
      e3 = epstab(k1)
      epstab(k1) = e1
      delta1 = e1-e3
      err1 = abs(delta1)
      tol1 = max([e1abs,abs(e3)])*epmach

;           IF TWO ELEMENTS ARE VERY CLOSE TO EACH OTHER, OMIT
;           A PART OF THE TABLE BY ADJUSTING THE VALUE OF N
      if NOT (err1 LE tol1 OR err2 LE tol2 OR err3 LE tol3) then begin

          ;; Avoid underflow errors
          if abs(delta1) GE 0.5*abs(oflow) then odelta1 = delta1*0 $
          else                                  odelta1 = 1/delta1
          ss = odelta1+0.1e+01/delta2-0.1e+01/delta3
          epsinf = abs(ss*e1)
          
;           TEST TO DETECT IRREGULAR BEHAVIOUR IN THE TABLE, AND
;           EVENTUALLY OMIT A PART OF THE TABLE ADJUSTING THE VALUE
;           OF N.
          if (epsinf GT 0.1e-03) then goto, LAB30
      endif

      
      n = i+i-1
; ***JUMP OUT OF DO-LOOP
      goto, LAB50
      
;           COMPUTE A NEW ELEMENT AND EVENTUALLY ADJUST
;           THE VALUE OF RESULT.
      LAB30:
      res = e1+0.1e+01/ss
      epstab(k1) = res
      k1 = k1-2
      error = err2+abs(res-e2)+err3
      if NOT (error GT abserr) then begin
          abserr = error
          result = res
      endif
  endfor
  
;           SHIFT THE TABLE.
  LAB50:
  if (n EQ limexp) then n = 2*(limexp/2)-1
  ib = 1
  if ((num/2)*2 EQ num) then ib = 2
  ie = newelm+1
  ii = ib-1+lindgen(ie)*2
  epstab(ii) = epstab(ii+2) ;; OK IDL
  if NOT (num EQ n) then begin
      indx = num-n+1
      ii = lindgen(n)
      epstab(ii) = epstab(indx-1+ii) ;; OK IDL
  endif
  
  if NOT (nres GE 4) then begin
      res3la(nres-1) = result
      abserr = oflow
  endif else begin
      
;           COMPUTE ERROR ESTIMATE - OK IDL
      abserr = (abs(result-res3la(2))+abs(result-res3la(1)) $
                +abs(result-res3la(0)))
      res3la(0) = res3la(1)
      res3la(1) = res3la(2)
      res3la(2) = result
  endelse
  
  LAB100:
  abserr = max([abserr,0.5e+01*epmach*abs(result)])
;  profvals.acctime = profvals.acctime + (systime(1) - prof_start)
  return
end
  
pro qpint1d_qpsrt, limit, last, maxerr, ermax, elist, iord, nrmax
;;                              OOOOOO  OOOOO         OOOO  OOOOO

  ;; Derived from QUADPACK QPSRT
  ;;***PURPOSE  Subsidiary to QAGE, QAGIE, QAGPE, QAGSE, QAWCE, QAWOE and
  ;;            QAWSE
  ;;***LIBRARY   SLATEC

  ;; ELIST - list of unsorted errors
  ;; IORD - (O) list of indices which sort ELIST in descending order
  ;; LAST - index of last element (new elt) in ELIST
  ;; LIMIT - maximum size of ELIST

  ;; NRMAX - (O) position in IORD of the maximum error
  ;; MAXERR - (O) position in ELIST of the maximum error
  ;; ERMAX - (O) amount of maximum error
  common qpint1d_profile_common, profvals
;  prof_start = systime(1)

  if (last LE 2) then begin
      iord(1-1) = 1
      iord(2-1) = 2
      goto, LAB90
  endif

;           THIS PART OF THE ROUTINE IS ONLY EXECUTED
;           IF, DUE TO A DIFFICULT INTEGRAND, SUBDIVISION
;           INCREASED THE ERROR ESTIMATE. IN THE NORMAL CASE
;           THE INSERT PROCEDURE SHOULD START AFTER THE
;           NRMAX-TH LARGEST ERROR ESTIMATE.

  errmax = elist(maxerr-1)
  if (nrmax NE 1) then begin
      ido = nrmax-1
      
      for i = 1, ido do begin
          isucc = iord(nrmax-1-1)
; ***JUMP OUT OF DO-LOOP
          if (errmax LE elist(isucc-1)) then goto, LAB30
          iord(nrmax-1) = isucc
          nrmax = nrmax-1
      endfor
  endif

  LAB30:
  jupbn = last
  if (last GT (limit/2+2)) then jupbn = limit+3-last
  errmin = elist(last-1)

;           INSERT ERRMAX BY TRAVERSING THE LIST TOP-DOWN,
;           STARTING COMPARISON FROM THE ELEMENT ELIST(IORD(NRMAX+1)).
  jbnd = jupbn-1
  ibeg = nrmax+1
  if (ibeg LE jbnd) then begin
      for i = ibeg, jbnd do begin
          isucc = iord(i-1)
; ***JUMP OUT OF DO-LOOP
          if (errmax GE elist(isucc-1)) then goto, LAB60
          iord(i-1-1) = isucc
      endfor
  endif
  iord(jbnd-1) = maxerr
  iord(jupbn-1) = last
  goto, LAB90
  
;           INSERT ERRMIN BY TRAVERSING THE LIST BOTTOM-UP.
  LAB60:
  iord(i-1-1) = maxerr
  k = jbnd
  for j=i, jbnd do begin
      isucc = iord(k-1)
; ***JUMP OUT OF DO-LOOP
      if (errmin LT elist(isucc-1)) then goto, LAB80
      iord(k+1-1) = isucc
      k = k-1
  endfor
  iord(i-1) = last
  goto, LAB90
  LAB80:
  iord(k+1-1) = last

;           SET MAXERR AND ERMAX.
  LAB90:
  maxerr = iord(nrmax-1)
  ermax = elist(maxerr-1)
;  profvals.srttime = profvals.srttime + (systime(1) - prof_start)
  return
end

pro qpint1d_gkweights, wg, wgk, xgk, ig, nptsreq, nptsact, prec


  ;; The user has requested NPTSREQ points, however that number may
  ;; not be available.  Filter the number and record the *actual*
  ;; number of points used, NPTSACT


;           THE ABSCISSAE AND WEIGHTS ARE GIVEN FOR THE INTERVAL (-1,1).
;           BECAUSE OF SYMMETRY ONLY THE POSITIVE ABSCISSAE AND THEIR
;           CORRESPONDING WEIGHTS ARE GIVEN.
;
;           XGK    - ABSCISSAE OF THE 21-POINT KRONROD RULE
;                    XGK(2), XGK(4), ...  ABSCISSAE OF THE 10-POINT
;                    GAUSS RULE
;                    XGK(1), XGK(3), ...  ABSCISSAE WHICH ARE OPTIMALLY
;                    ADDED TO THE 10-POINT GAUSS RULE
;
;           WGK    - WEIGHTS OF THE 21-POINT KRONROD RULE
;
;           WG     - WEIGHTS OF THE 10-POINT GAUSS RULE

      ;; gauss quadrature weights and kronron quadrature abscissae and
      ;; weights as evaluated with 80 decimal digit arithmetic by
      ;; l. w. fullerton, bell labs, nov. 1981.
    
  if nptsreq LE 15 then begin
      nptsact = 15
      wg = [ $
             0.129484966168869693270611432679082d0, $
             0.279705391489276667901467771423780d0, $
             0.381830050505118944950369775488975d0, $
             0.417959183673469387755102040816327d0  ]

      xgk = [ $
              0.991455371120812639206854697526329d0, $
              0.949107912342758524526189684047851d0, $
              0.864864423359769072789712788640926d0, $
              0.741531185599394439863864773280788d0, $
              0.586087235467691130294144838258730d0, $
              0.405845151377397166906606412076961d0, $
              0.207784955007898467600689403773245d0, $
              0.000000000000000000000000000000000d0  ]

      wgk = [ $
              0.022935322010529224963732008058970d0, $
              0.063092092629978553290700663189204d0, $
              0.104790010322250183839876322541518d0, $
              0.140653259715525918745189590510238d0, $
              0.169004726639267902826583426598550d0, $
              0.190350578064785409913256402421014d0, $
              0.204432940075298892414161999234649d0, $
              0.209482141084727828012999174891714d0  ]
              
  endif else if nptsreq LE 21 then begin
      nptsact = 21
      wg = [  0.066671344308688137593568809893332d0, $
              0.149451349150580593145776339657697d0, $
              0.219086362515982043995534934228163d0, $
              0.269266719309996355091226921569469d0, $
              0.295524224714752870173892994651338d0  ]

      xgk = [ 0.995657163025808080735527280689003d0, $
              0.973906528517171720077964012084452d0, $
              0.930157491355708226001207180059508d0, $
              0.865063366688984510732096688423493d0, $
              0.780817726586416897063717578345042d0, $
              0.679409568299024406234327365114874d0, $
              0.562757134668604683339000099272694d0, $
              0.433395394129247190799265943165784d0, $
              0.294392862701460198131126603103866d0, $
              0.148874338981631210884826001129720d0, $
              0.000000000000000000000000000000000d0  ]

      wgk = [ 0.011694638867371874278064396062192d0, $
              0.032558162307964727478818972459390d0, $
              0.054755896574351996031381300244580d0, $
              0.075039674810919952767043140916190d0, $
              0.093125454583697605535065465083366d0, $
              0.109387158802297641899210590325805d0, $
              0.123491976262065851077958109831074d0, $
              0.134709217311473325928054001771707d0, $
              0.142775938577060080797094273138717d0, $
              0.147739104901338491374841515972068d0, $
              0.149445554002916905664936468389821d0  ]

  endif else if nptsreq LE 31 then begin
      nptsact = 31
      wg = [ $
             0.03075324199611726834628393577204d0, $
             0.07036604748810812479267416450667d0, $
             0.10715922046717193501869546685869d0, $
             0.13957067792615431447804794511028d0, $
             0.16626920581699393353200860481209d0, $
             0.18616100001556221106800561866423d0, $
             0.19843148532711157646118326443839d0, $
             0.20257824192556127280620199967519d0  ]

      xgk = [ $
              0.99800229869339706025172840152271d0, $
              0.98799251802048542849565718586613d0, $
              0.96773907567913913427347978784337d0, $
              0.93727339240070590437758947710209d0, $
              0.89726453234408190082509656454496d0, $
              0.84820658341042721620648320774217d0, $
              0.79041850144246593297649294817947d0, $
              0.72441773136017004746186054613938d0, $
              0.65099674129741697053735895313275d0, $
              0.57097217260853884757226737253911d0, $
              0.48508186364023968063655740232351d0, $
              0.39415134707756336987207370981045d0, $
              0.29918000715316881216780024266389d0, $
              0.20119409399743452230628303394596d0, $
              0.10114206691871749907074231447392d0, $
              0.00000000000000000000000000000000d0  ]

      wgk = [ $
              0.00537747987292334897792051430128d0, $
              0.01500794732931612258374763075807d0, $
              0.02546084732671532016874001019653d0, $
              0.03534636079137584622037948478360d0, $
              0.04458975132476487668227299373280d0, $
              0.05348152469092808725343147239430d0, $
              0.06200956780067064025139230960803d0, $
              0.06985412131872825879520077099147d0, $
              0.07684968075772037884432777482659d0, $
              0.08308050282313302108289247286104d0, $
              0.08856444305621177067275443693774d0, $
              0.09312659817082532125486872747346d0, $
              0.09664272698362367855179907627589d0, $
              0.09917359872179195932393173484603d0, $
              0.10076984552387559504946662617570d0, $
              0.10133000701479154907374792767493d0  ]
  endif else if nptsreq LE 41 then begin
      nptsact = 41
      wg = [ $
             0.017614007139152118311861962351853d0, $
             0.040601429800386941331039952274932d0, $
             0.062672048334109063569506535187042d0, $
             0.083276741576704748724758143222046d0, $
             0.101930119817240435036750135480350d0, $
             0.118194531961518417312377377711382d0, $
             0.131688638449176626898494499748163d0, $
             0.142096109318382051329298325067165d0, $
             0.149172986472603746787828737001969d0, $
             0.152753387130725850698084331955098d0  ]
      xgk = [ $
              0.998859031588277663838315576545863d0, $
              0.993128599185094924786122388471320d0, $
              0.981507877450250259193342994720217d0, $
              0.963971927277913791267666131197277d0, $
              0.940822633831754753519982722212443d0, $
              0.912234428251325905867752441203298d0, $
              0.878276811252281976077442995113078d0, $
              0.839116971822218823394529061701521d0, $
              0.795041428837551198350638833272788d0, $
              0.746331906460150792614305070355642d0, $
              0.693237656334751384805490711845932d0, $
              0.636053680726515025452836696226286d0, $
              0.575140446819710315342946036586425d0, $
              0.510867001950827098004364050955251d0, $
              0.443593175238725103199992213492640d0, $
              0.373706088715419560672548177024927d0, $
              0.301627868114913004320555356858592d0, $
              0.227785851141645078080496195368575d0, $
              0.152605465240922675505220241022678d0, $
              0.076526521133497333754640409398838d0, $
              0.000000000000000000000000000000000d0  ]
      wgk = [ $
              0.003073583718520531501218293246031d0, $
              0.008600269855642942198661787950102d0, $
              0.014626169256971252983787960308868d0, $
              0.020388373461266523598010231432755d0, $
              0.025882133604951158834505067096153d0, $
              0.031287306777032798958543119323801d0, $
              0.036600169758200798030557240707211d0, $
              0.041668873327973686263788305936895d0, $
              0.046434821867497674720231880926108d0, $
              0.050944573923728691932707670050345d0, $
              0.055195105348285994744832372419777d0, $
              0.059111400880639572374967220648594d0, $
              0.062653237554781168025870122174255d0, $
              0.065834597133618422111563556969398d0, $
              0.068648672928521619345623411885368d0, $
              0.071054423553444068305790361723210d0, $
              0.073030690332786667495189417658913d0, $
              0.074582875400499188986581418362488d0, $
              0.075704497684556674659542775376617d0, $
              0.076377867672080736705502835038061d0, $
              0.076600711917999656445049901530102d0  ]
  endif else if nptsreq LE 51 then begin
      nptsact = 51

      wg = [ $
             0.011393798501026287947902964113235d0, $
             0.026354986615032137261901815295299d0, $
             0.040939156701306312655623487711646d0, $
             0.054904695975835191925936891540473d0, $
             0.068038333812356917207187185656708d0, $
             0.080140700335001018013234959669111d0, $
             0.091028261982963649811497220702892d0, $
             0.100535949067050644202206890392686d0, $
             0.108519624474263653116093957050117d0, $
             0.114858259145711648339325545869556d0, $
             0.119455763535784772228178126512901d0, $
             0.122242442990310041688959518945852d0, $
             0.123176053726715451203902873079050d0  ]

      xgk = [ $
              0.999262104992609834193457486540341d0, $
              0.995556969790498097908784946893902d0, $
              0.988035794534077247637331014577406d0, $
              0.976663921459517511498315386479594d0, $
              0.961614986425842512418130033660167d0, $
              0.942974571228974339414011169658471d0, $
              0.920747115281701561746346084546331d0, $
              0.894991997878275368851042006782805d0, $
              0.865847065293275595448996969588340d0, $
              0.833442628760834001421021108693570d0, $
              0.797873797998500059410410904994307d0, $
              0.759259263037357630577282865204361d0, $
              0.717766406813084388186654079773298d0, $
              0.673566368473468364485120633247622d0, $
              0.626810099010317412788122681624518d0, $
              0.577662930241222967723689841612654d0, $
              0.526325284334719182599623778158010d0, $
              0.473002731445714960522182115009192d0, $
              0.417885382193037748851814394594572d0, $
              0.361172305809387837735821730127641d0, $
              0.303089538931107830167478909980339d0, $
              0.243866883720988432045190362797452d0, $
              0.183718939421048892015969888759528d0, $
              0.122864692610710396387359818808037d0, $
              0.061544483005685078886546392366797d0, $
              0.000000000000000000000000000000000d0  ]

      wgk = [ $
              0.001987383892330315926507851882843d0, $
              0.005561932135356713758040236901066d0, $
              0.009473973386174151607207710523655d0, $
              0.013236229195571674813656405846976d0, $
              0.016847817709128298231516667536336d0, $
              0.020435371145882835456568292235939d0, $
              0.024009945606953216220092489164881d0, $
              0.027475317587851737802948455517811d0, $
              0.030792300167387488891109020215229d0, $
              0.034002130274329337836748795229551d0, $
              0.037116271483415543560330625367620d0, $
              0.040083825504032382074839284467076d0, $
              0.042872845020170049476895792439495d0, $
              0.045502913049921788909870584752660d0, $
              0.047982537138836713906392255756915d0, $
              0.050277679080715671963325259433440d0, $
              0.052362885806407475864366712137873d0, $
              0.054251129888545490144543370459876d0, $
              0.055950811220412317308240686382747d0, $
              0.057437116361567832853582693939506d0, $
              0.058689680022394207961974175856788d0, $
              0.059720340324174059979099291932562d0, $
              0.060539455376045862945360267517565d0, $
              0.061128509717053048305859030416293d0, $
              0.061471189871425316661544131965264d0, $
              0.061580818067832935078759824240066d0  ]
;       note: wgk (26) was calculated from the values of wgk(1..25)

  endif else begin
      nptsact = 61

      wg = [ $
             0.007968192496166605615465883474674d0, $
             0.018466468311090959142302131912047d0, $
             0.028784707883323369349719179611292d0, $
             0.038799192569627049596801936446348d0, $
             0.048402672830594052902938140422808d0, $
             0.057493156217619066481721689402056d0, $
             0.065974229882180495128128515115962d0, $
             0.073755974737705206268243850022191d0, $
             0.080755895229420215354694938460530d0, $
             0.086899787201082979802387530715126d0, $
             0.092122522237786128717632707087619d0, $
             0.096368737174644259639468626351810d0, $
             0.099593420586795267062780282103569d0, $
             0.101762389748405504596428952168554d0, $
             0.102852652893558840341285636705415d0  ]

      xgk = [ $
              0.999484410050490637571325895705811d0, $
              0.996893484074649540271630050918695d0, $
              0.991630996870404594858628366109486d0, $
              0.983668123279747209970032581605663d0, $
              0.973116322501126268374693868423707d0, $
              0.960021864968307512216871025581798d0, $
              0.944374444748559979415831324037439d0, $
              0.926200047429274325879324277080474d0, $
              0.905573307699907798546522558925958d0, $
              0.882560535792052681543116462530226d0, $
              0.857205233546061098958658510658944d0, $
              0.829565762382768397442898119732502d0, $
              0.799727835821839083013668942322683d0, $
              0.767777432104826194917977340974503d0, $
              0.733790062453226804726171131369528d0, $
              0.697850494793315796932292388026640d0, $
              0.660061064126626961370053668149271d0, $
              0.620526182989242861140477556431189d0, $
              0.579345235826361691756024932172540d0, $
              0.536624148142019899264169793311073d0, $
              0.492480467861778574993693061207709d0, $
              0.447033769538089176780609900322854d0, $
              0.400401254830394392535476211542661d0, $
              0.352704725530878113471037207089374d0, $
              0.304073202273625077372677107199257d0, $
              0.254636926167889846439805129817805d0, $
              0.204525116682309891438957671002025d0, $
              0.153869913608583546963794672743256d0, $
              0.102806937966737030147096751318001d0, $
              0.051471842555317695833025213166723d0, $
              0.000000000000000000000000000000000d0  ]

      wgk = [ $
              0.001389013698677007624551591226760d0, $
              0.003890461127099884051267201844516d0, $
              0.006630703915931292173319826369750d0, $
              0.009273279659517763428441146892024d0, $
              0.011823015253496341742232898853251d0, $
              0.014369729507045804812451432443580d0, $
              0.016920889189053272627572289420322d0, $
              0.019414141193942381173408951050128d0, $
              0.021828035821609192297167485738339d0, $
              0.024191162078080601365686370725232d0, $
              0.026509954882333101610601709335075d0, $
              0.028754048765041292843978785354334d0, $
              0.030907257562387762472884252943092d0, $
              0.032981447057483726031814191016854d0, $
              0.034979338028060024137499670731468d0, $
              0.036882364651821229223911065617136d0, $
              0.038678945624727592950348651532281d0, $
              0.040374538951535959111995279752468d0, $
              0.041969810215164246147147541285970d0, $
              0.043452539701356069316831728117073d0, $
              0.044814800133162663192355551616723d0, $
              0.046059238271006988116271735559374d0, $
              0.047185546569299153945261478181099d0, $
              0.048185861757087129140779492298305d0, $
              0.049055434555029778887528165367238d0, $
              0.049795683427074206357811569379942d0, $
              0.050405921402782346840893085653585d0, $
              0.050881795898749606492297473049805d0, $
              0.051221547849258772170656282604944d0, $
              0.051426128537459025933862879215781d0, $
              0.051494729429451567558340433647099d0  ]
  endelse

  nhalf  = (nptsact-1)/2
  nquart = (nptsact-1)/4
  wgk = [  wgk(0:nhalf-1), rotate(wgk,2) ]
  xgk = [ -xgk(0:nhalf-1), rotate(xgk,2) ]
  wg  = [wg(0:nquart-1), rotate(wg,2)]
  ig  = lindgen(nhalf)*2 + 1

  ;; Convert to float from double if requested
  if prec EQ 0 then begin
      wgk = float(wgk)
      xgk = float(xgk)
      wg  = float(wg)
  endif
end

;; Evaluate a user-supplied expression
function qpint1d_eval, x, p, expression=expr
  y = 0
  cmd = 'Y = '+expr
  dummy = execute(cmd)
  return, y
end

;; The outer routine which does most of the preparation and special
;; cases.
function qpint1d, f0, a0, b0, private, npoints=npoints, expression=expr, $
                  epsabs=epsabs, epsrel=epsrel, error=abserr, nfev=neval, $
                  status=ier, functargs=fa, limit=limit, nsubintervals=nsub, $
                  sym_axis=symaxis, symmetric=symmetric, breakpoints=bpoints0,$
                  alist=alist, blist=blist, rlist=rlist, $
                  elist=elist, iord=iord, nocatch=nocatch

  ;; Derived from QUADPACK QAGS
  ;;***PURPOSE  The routine calculates an approximation result to a given
  ;;            Definite integral  I = Integral of F over (A,B),
  ;;            Hopefully satisfying following claim for accuracy
  ;;           ABS(I-RESULT).LE.MAX(EPSABS,EPSREL*ABS(I)).
  ;;***LIBRARY   SLATEC (QUADPACK)

  if keyword_set(query) then return, 1
  common qpint1d_profile_common, profvals
  qpint1d_profile

  if n_params() EQ 0 then begin
      USAGE:
      message, 'USAGE:', /info
      message, ' G = QPINT1D(FUNCNAME, A, B, $', /info
      message, '             [EPSABS=, EPSREL=, ERROR=, STATUS=])', /info
      message, '      (or)', /info
      message, ' G = QPINT1D(EXPR, A, B, /EXPRESSION, $', /info
      message, '             [EPSABS=, EPSREL=, ERROR=, STATUS=])', /info
      return, !values.d_nan
  endif
  if n_elements(f0) EQ 0 OR n_elements(a0) EQ 0 OR n_elements(b0) EQ 0 then $
    goto, USAGE

  sz = size(f0)
  if sz(sz(0)+1) NE 7 OR n_elements(f0) NE 1 then begin
      message, 'ERROR: FUNCT must be a scalar string', /info
      return, !values.d_nan
  endif

  ;; Handle error conditions gracefully
  if NOT keyword_set(nocatch) then begin
      catch, catcherror
      if catcherror NE 0 then begin
          catch, /cancel
          message, 'Error detected while integrating "'+f0(0)+'"', /info
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
          return, !values.d_nan
      endif
  endif
  
;  prof_start = systime(1)
  neval = 0L
  sza = size(a0)
  szb = size(b0)
  ;; Determine the data precision, i.e. single or double precision
  if sza(sza(0)+1) EQ 5 OR szb(szb(0)+1) EQ 5 then begin
      isdouble = 1
      zero = 0D
      reldef = 1e-6
      a = double(a0(0))
      b = double(b0(0))
  endif else begin
      isdouble = 0
      zero = 0.
      reldef = 1e-4
      a = float(a0(0))
      b = float(b0(0))
  endelse
  qpint1d_setmachar, double=isdouble

  ;; Default values
  if n_elements(limit) EQ 0 then limit = 100L
  if n_elements(epsrel) EQ 0 then epsrel = zero + reldef
  if n_elements(epsabs) EQ 0 then epsabs = zero

  ;; Establish the initial return values return
  abserr = zero
  result = zero
  if a EQ b then begin
      ier = 0L
      return, result
  endif

  ;; Now prepare for potentially one or two integrals, depending on
  ;; whether the integral is fully infinite, or only partially
  infa = finite(a) EQ 0
  infb = finite(b) EQ 0

  if infa AND infb then begin
      ;; Fully infinite: break it into two partial integrals
      if n_elements(symaxis0) EQ 0 then symaxis = zero $
      else                              symaxis = zero + symaxis0(0)

      a1 = symaxis & b1 = b
      a2 = symaxis & b2 = a
      ef = 2.  ;; Each integral contributes half of error budget
  endif else if infa then begin
      ;; Partially infinite, [a,inf]: do only second partial integral
      a1 = zero    & b1 = zero
      a2 = b       & b2 = a
      ef = 1.
  endif else begin
      ;; Partially infinite, [inf,b]: do only first partial integral
      a1 = a       & b1 = b
      a2 = zero    & b2 = zero
      ef = 1.
  endelse

  ;; Prepare for EXPRESSION if requested
  if keyword_set(expr) then begin
      f = 'QPINT1D_EVAL'
      fa = {expression: strtrim(f0(0),2)}
  endif else begin
      f = strtrim(f0(0),2)
  endelse

  ;; Call first partial integral if requested
  ier = 0L
  neval = 0L
  nsub = 0L
  if a1 NE b1 then begin
      qpint1d_qagse, f, a1, b1, result, abserr, private, functargs=fa, $
        epsabs=epsabs/ef, epsrel=epsrel/ef, breakpoints=bpoints0, $
        status=ier, limit=limit, neval=neval, nsubintervals=nsub, $
        alist=alist, blist=blist, rlist=rlist, elist=elist, iord=iord, $
        npoints=npoints
  endif

  ;; Return if an error condition was detected
  if ier EQ 6 then ier = -1L
  if ier EQ -1 OR ier EQ 3 then return, result

  ;; Now call the second partial integral if requested
  if a2 NE b2 then begin
      qpint1d_qagse, f, a2, b2, result2, abserr2, private, functargs=fa, $
        epsabs=epsabs/ef, epsrel=epsrel/ef, breakpoints=bpoints0, $
        status=ier, limit=limit, neval=neval2, nsubintervals=nsub2, $
        alist=alist, blist=blist, rlist=rlist, elist=elist, iord=iord, $
        npoints=npoints

      ;; Merge the two results together
      result = result - result2
      abserr = abserr + abserr2
      neval  = neval  + neval2
      nsub   = nsub   + nsub2
  endif

;  profvals.tottime = profvals.tottime + (systime(1) - prof_start)

  ;; Convert from QUADPACK to QPINT1D errors
  if ier EQ 6 then ier = -1L
  return, result
end
