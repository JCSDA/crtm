;+
; NAME:
;   TNMIN
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE:
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Performs function minimization (Truncated-Newton Method)
;
; MAJOR TOPICS:
;   Optimization and Minimization
;
; CALLING SEQUENCE:
;   parms = TNMIN(MYFUNCT, X, FUNCTARGS=fcnargs, NFEV=nfev,
;                 MAXITER=maxiter, ERRMSG=errmsg, NPRINT=nprint,
;                 QUIET=quiet, XTOL=xtol, STATUS=status,
;                 FGUESS=fguess, PARINFO=parinfo, BESTMIN=bestmin,
;                 ITERPROC=iterproc, ITERARGS=iterargs, niter=niter)
;
; DESCRIPTION:
;
;  TNMIN uses the Truncated-Newton method to minimize an arbitrary IDL
;  function with respect to a given set of free parameters.  The
;  user-supplied function must compute the gradient with respect to
;  each parameter.  TNMIN is based on TN.F (TNBC) by Stephen Nash.
;
;  If you want to solve a least-squares problem, to perform *curve*
;  *fitting*, then you will probably want to use the routines MPFIT,
;  MPFITFUN and MPFITEXPR.  Those routines are specifically optimized
;  for the least-squares problem.  TNMIN is suitable for constrained
;  and unconstrained optimization problems with a medium number of
;  variables.  Function *maximization* can be performed using the
;  MAXIMIZE keyword.
;
;  TNMIN is similar to MPFIT in that it allows parameters to be fixed,
;  simple bounding limits to be placed on parameter values, and
;  parameters to be tied to other parameters.  One major difference
;  between MPFIT and TNMIN is that TNMIN does not compute derivatives
;  automatically by default.  See PARINFO and AUTODERIVATIVE below for
;  more discussion and examples.
;
; USER FUNCTION
;
;  The user must define an IDL function which returns the desired
;  value as a single scalar.  The IDL function must accept a list of
;  numerical parameters, P.  Additionally, keyword parameters may be
;  used to pass more data or information to the user function, via the
;  FUNCTARGS keyword.
;
;  The user function should be declared in the following way:
;
;     FUNCTION MYFUNCT, p, dp [, keywords permitted ]
;       ; Parameter values are passed in "p"
;       f  = ....   ; Compute function value
;       dp = ....   ; Compute partial derivatives (optional)
;       return, f
;     END
;
;  The function *must* accept at least one argument, the parameter
;  list P.  The vector P is implicitly assumed to be a one-dimensional
;  array.  Users may pass additional information to the function by
;  composing and _EXTRA structure and passing it in the FUNCTARGS
;  keyword.
;
;  User functions may also indicate a fatal error condition using the
;  ERROR_CODE common block variable, as described below under the
;  TNMIN_ERROR common block definition (by setting ERROR_CODE to a
;  number between -15 and -1).
;
;  EXPLICIT vs. NUMERICAL DERIVATIVES
;
;  By default, the user must compute the function gradient components
;  explicitly using AUTODERIVATIVE=0.  As explained below, numerical
;  derivatives can also be calculated using AUTODERIVATIVE=1.
;
;  For explicit derivatives, the user should call TNMIN using the
;  default keyword value AUTODERIVATIVE=0.  [ This is different
;  behavior from MPFIT, where AUTODERIVATIVE=1 is the default. ] The
;  IDL user routine should compute the gradient of the function as a
;  one-dimensional array of values, one for each of the parameters.
;  They are passed back to TNMIN via "dp" as shown above.
;
;  The derivatives with respect to fixed parameters are ignored; zero
;  is an appropriate value to insert for those derivatives.  Upon
;  input to the user function, DP is set to a vector with the same
;  length as P, with a value of 1 for a parameter which is free, and a
;  value of zero for a parameter which is fixed (and hence no
;  derivative needs to be calculated).  This input vector may be
;  overwritten as needed.
;
;  For numerical derivatives, a finite differencing approximation is
;  used to estimate the gradient values.  Users can activate this
;  feature by passing the keyword AUTODERIVATIVE=1.  Fine control over
;  this behavior can be achieved using the STEP, RELSTEP and TNSIDE
;  fields of the PARINFO structure.
;
;  When finite differencing is used for computing derivatives (ie,
;  when AUTODERIVATIVE=1), the parameter DP is not passed.  Therefore
;  functions can use N_PARAMS() to indicate whether they must compute
;  the derivatives or not.  However there is no penalty (other than
;  computation time) for computing the gradient values and users may
;  switch between AUTODERIVATIVE=0 or =1 in order to test both
;  scenarios.
;
; CONSTRAINING PARAMETER VALUES WITH THE PARINFO KEYWORD
;
;  The behavior of TNMIN can be modified with respect to each
;  parameter to be optimized.  A parameter value can be fixed; simple
;  boundary constraints can be imposed; limitations on the parameter
;  changes can be imposed; properties of the automatic derivative can
;  be modified; and parameters can be tied to one another.
;
;  These properties are governed by the PARINFO structure, which is
;  passed as a keyword parameter to TNMIN.
;
;  PARINFO should be an array of structures, one for each parameter.
;  Each parameter is associated with one element of the array, in
;  numerical order.  The structure can have the following entries
;  (none are required):
;
;     .VALUE - the starting parameter value (but see the START_PARAMS
;              parameter for more information).
;
;     .FIXED - a boolean value, whether the parameter is to be held
;              fixed or not.  Fixed parameters are not varied by
;              TNMIN, but are passed on to MYFUNCT for evaluation.
;
;     .LIMITED - a two-element boolean array.  If the first/second
;                element is set, then the parameter is bounded on the
;                lower/upper side.  A parameter can be bounded on both
;                sides.  Both LIMITED and LIMITS must be given
;                together.
;
;     .LIMITS - a two-element float or double array.  Gives the
;               parameter limits on the lower and upper sides,
;               respectively.  Zero, one or two of these values can be
;               set, depending on the values of LIMITED.  Both LIMITED
;               and LIMITS must be given together.
;
;     .PARNAME - a string, giving the name of the parameter.  The
;                fitting code of TNMIN does not use this tag in any
;                way.
;
;     .STEP - the step size to be used in calculating the numerical
;             derivatives.  If set to zero, then the step size is
;             computed automatically.  Ignored when AUTODERIVATIVE=0.
;
;     .TNSIDE - the sidedness of the finite difference when computing
;               numerical derivatives.  This field can take four
;               values:
;
;                  0 - one-sided derivative computed automatically
;                  1 - one-sided derivative (f(x+h) - f(x)  )/h
;                 -1 - one-sided derivative (f(x)   - f(x-h))/h
;                  2 - two-sided derivative (f(x+h) - f(x-h))/(2*h)
;
;              Where H is the STEP parameter described above.  The
;              "automatic" one-sided derivative method will chose a
;              direction for the finite difference which does not
;              violate any constraints.  The other methods do not
;              perform this check.  The two-sided method is in
;              principle more precise, but requires twice as many
;              function evaluations.  Default: 0.
;
;     .TIED - a string expression which "ties" the parameter to other
;             free or fixed parameters.  Any expression involving
;             constants and the parameter array P are permitted.
;             Example: if parameter 2 is always to be twice parameter
;             1 then use the following: parinfo(2).tied = '2 * P(1)'.
;             Since they are totally constrained, tied parameters are
;             considered to be fixed; no errors are computed for them.
;             [ NOTE: the PARNAME can't be used in expressions. ]
;
;  Future modifications to the PARINFO structure, if any, will involve
;  adding structure tags beginning with the two letters "MP" or "TN".
;  Therefore programmers are urged to avoid using tags starting with
;  these two combinations of letters; otherwise they are free to
;  include their own fields within the PARINFO structure, and they
;  will be ignored.
;
;  PARINFO Example:
;  parinfo = replicate({value:0.D, fixed:0, limited:[0,0], $
;                       limits:[0.D,0]}, 5)
;  parinfo(0).fixed = 1
;  parinfo(4).limited(0) = 1
;  parinfo(4).limits(0)  = 50.D
;  parinfo(*).value = [5.7D, 2.2, 500., 1.5, 2000.]
;
;  A total of 5 parameters, with starting values of 5.7,
;  2.2, 500, 1.5, and 2000 are given.  The first parameter
;  is fixed at a value of 5.7, and the last parameter is
;  constrained to be above 50.
;
;
; INPUTS:
;
;   MYFUNCT - a string variable containing the name of the function to
;             be minimized (see USER FUNCTION above).  The IDL routine
;             should return the value of the function and optionally
;             its gradients.
;
;   X - An array of starting values for each of the parameters of the
;       model.
;
;       This parameter is optional if the PARINFO keyword is used.
;       See above.  The PARINFO keyword provides a mechanism to fix or
;       constrain individual parameters.  If both X and PARINFO are
;       passed, then the starting *value* is taken from X, but the
;       *constraints* are taken from PARINFO.
;
;
; RETURNS:
;
;   Returns the array of best-fit parameters.
;
;
; KEYWORD PARAMETERS:
;
;   AUTODERIVATIVE - If this is set, derivatives of the function will
;                    be computed automatically via a finite
;                    differencing procedure.  If not set, then MYFUNCT
;                    must provide the (explicit) derivatives.
;                    Default: 0 (explicit derivatives required)
;
;   BESTMIN - upon return, the best minimum function value that TNMIN
;             could find.
;
;   EPSABS - a nonnegative real variable.  Termination occurs when the
;            absolute error between consecutive iterates is at most
;            EPSABS.  Note that using EPSREL is normally preferable
;            over EPSABS, except in cases where ABS(F) is much larger
;            than 1 at the optimal point.  A value of zero indicates
;            the absolute error test is not applied.  If EPSABS is
;            specified, then both EPSREL and EPSABS tests are applied;
;            if either succeeds then termination occurs.
;            Default: 0 (i.e., only EPSREL is applied).
;
;   EPSREL - a nonnegative input variable. Termination occurs when the
;            relative error between two consecutive iterates is at
;            most EPSREL.  Therefore, EPSREL measures the relative
;            error desired in the function.  An additional, more
;            lenient, stopping condition can be applied by specifying
;            the EPSABS keyword.
;            Default: 100 * Machine Precision
;
;   ERRMSG - a string error or warning message is returned.
;
;   FGUESS - provides the routine a guess to the minimum value.
;            Default: 0
;
;   FUNCTARGS - A structure which contains the parameters to be passed
;               to the user-supplied function specified by MYFUNCT via
;               the _EXTRA mechanism.  This is the way you can pass
;               additional data to your user-supplied function without
;               using common blocks.
;
;               Consider the following example:
;                if FUNCTARGS = { XVAL:[1.D,2,3], YVAL:[1.D,4,9]}
;                then the user supplied function should be declared
;                like this:
;                FUNCTION MYFUNCT, P, XVAL=x, YVAL=y
;
;               By default, no extra parameters are passed to the
;               user-supplied function.
;
;   ITERARGS - The keyword arguments to be passed to ITERPROC via the
;              _EXTRA mechanism.  This should be a structure, and is
;              similar in operation to FUNCTARGS.
;              Default: no arguments are passed.
;
;   ITERDERIV - Intended to print function gradient information.  If
;               set, then the ITERDERIV keyword is set in each call to
;               ITERPROC.  In the default ITERPROC, parameter values
;               and gradient information are both printed when this
;               keyword is set.
;
;   ITERPROC - The name of a procedure to be called upon each NPRINT
;              iteration of the TNMIN routine.  It should be declared
;              in the following way:
;
;              PRO ITERPROC, MYFUNCT, p, iter, fnorm, FUNCTARGS=fcnargs, $
;                PARINFO=parinfo, QUIET=quiet, _EXTRA=extra
;                ; perform custom iteration update
;              END
;
;              ITERPROC must accept the _EXTRA keyword, in case of
;              future changes to the calling procedure.
;
;              MYFUNCT is the user-supplied function to be minimized,
;              P is the current set of model parameters, ITER is the
;              iteration number, and FUNCTARGS are the arguments to be
;              passed to MYFUNCT.  FNORM is should be the function
;              value.  QUIET is set when no textual output should be
;              printed.  See below for documentation of PARINFO.
;
;              In implementation, ITERPROC can perform updates to the
;              terminal or graphical user interface, to provide
;              feedback while the fit proceeds.  If the fit is to be
;              stopped for any reason, then ITERPROC should set the
;              common block variable ERROR_CODE to negative value
;              between -15 and -1 (see TNMIN_ERROR common block
;              below).  In principle, ITERPROC should probably not
;              modify the parameter values, because it may interfere
;              with the algorithm's stability.  In practice it is
;              allowed.
;
;              Default: an internal routine is used to print the
;                       parameter values.
;
;   MAXITER - The maximum number of iterations to perform.  If the
;             number is exceeded, then the STATUS value is set to 5
;             and TNMIN returns.
;             Default: 200 iterations
;
;   MAXIMIZE - If set, the function is maximized instead of
;              minimized.
;
;   MAXNFEV - The maximum number of function evaluations to perform.
;             If the number is exceeded, then the STATUS value is set
;             to -17 and TNMIN returns.  A value of zero indicates no
;             maximum.
;             Default: 0 (no maximum)
;
;   NFEV - upon return, the number of MYFUNCT function evaluations
;          performed.
;
;   NITER - upon return, number of iterations completed.
;
;   NPRINT - The frequency with which ITERPROC is called.  A value of
;            1 indicates that ITERPROC is called with every iteration,
;            while 2 indicates every other iteration, etc.
;            Default value: 1
;
;   PARINFO - Provides a mechanism for more sophisticated constraints
;             to be placed on parameter values.  When PARINFO is not
;             passed, then it is assumed that all parameters are free
;             and unconstrained.  Values in PARINFO are never modified
;             during a call to TNMIN.
;
;             See description above for the structure of PARINFO.
;
;             Default value:  all parameters are free and unconstrained.
;
;   QUIET - set this keyword when no textual output should be printed
;           by TNMIN
;
;   STATUS - an integer status code is returned.  All values greater
;            than zero can represent success (however STATUS EQ 5 may
;            indicate failure to converge).  Gaps in the numbering
;            system below are to maintain compatibility with MPFIT.
;            Upon return, STATUS can have one of the following values:
;
;        -18  a fatal internal error occurred during optimization.
;
;        -17  the maximum number of function evaluations has been
;             reached without convergence.
;
;        -16  a parameter or function value has become infinite or an
;             undefined number.  This is usually a consequence of
;             numerical overflow in the user's function, which must be
;             avoided.
;
;        -15 to -1
;             these are error codes that either MYFUNCT or ITERPROC
;             may return to terminate the fitting process (see
;             description of MPFIT_ERROR common below).  If either
;             MYFUNCT or ITERPROC set ERROR_CODE to a negative number,
;             then that number is returned in STATUS.  Values from -15
;             to -1 are reserved for the user functions and will not
;             clash with MPFIT.
;
;	   0  improper input parameters.
;
;	   1  convergence was reached.
;
;          2-4 (RESERVED)
;
;	   5  the maximum number of iterations has been reached
;
;          6-8 (RESERVED)
;
;
; EXAMPLE:
;
;   FUNCTION F, X, DF, _EXTRA=extra  ;; *** MUST ACCEPT KEYWORDS
;     F  = (X(0)-1)^2 + (X(1)+7)^2
;     DF = [ 2D * (X(0)-1), 2D * (X(1)+7) ] ; Gradient
;     RETURN, F
;   END
;
;   P = TNMIN('F', [0D, 0D], BESTMIN=F0)
;   Minimizes the function F(x0,x1) = (x0-1)^2 + (x1+7)^2.
;
;
; COMMON BLOCKS:
;
;   COMMON TNMIN_ERROR, ERROR_CODE
;
;     User routines may stop the fitting process at any time by
;     setting an error condition.  This condition may be set in either
;     the user's model computation routine (MYFUNCT), or in the
;     iteration procedure (ITERPROC).
;
;     To stop the fitting, the above common block must be declared,
;     and ERROR_CODE must be set to a negative number.  After the user
;     procedure or function returns, TNMIN checks the value of this
;     common block variable and exits immediately if the error
;     condition has been set.  By default the value of ERROR_CODE is
;     zero, indicating a successful function/procedure call.
;
;
; REFERENCES:
;
;   TRUNCATED-NEWTON METHOD, TN.F
;      Stephen G. Nash, Operations Research and Applied Statistics
;      Department
;      http://www.netlib.org/opt/tn
;
;   Nash, S. G. 1984, "Newton-Type Minimization via the Lanczos
;      Method," SIAM J. Numerical Analysis, 21, p. 770-778
;
;
; MODIFICATION HISTORY:
;   Derived from TN.F by Stephen Nash with many changes and additions,
;      to conform to MPFIT paradigm, 19 Jan 1999, CM
;   Changed web address to COW, 25 Sep 1999, CM
;   Alphabetized documented keyword parameters, 02 Oct 1999, CM
;   Changed to ERROR_CODE for error condition, 28 Jan 2000, CM
;   Continued and fairly major improvements (CM, 08 Jan 2001):
;      - calling of user procedure is now concentrated in TNMIN_CALL,
;        and arguments are reduced by storing a large number of them
;        in common blocks;
;      - finite differencing is done within TNMIN_CALL; added
;        AUTODERIVATIVE=1 so that finite differencing can be enabled,
;        both one and two sided;
;      - a new procedure to parse PARINFO fields, borrowed from MPFIT;
;        brought PARINFO keywords up to date with MPFIT;
;      - go through and check for float vs. double discrepancies;
;      - add explicit MAXIMIZE keyword, and support in TNMIN_CALL and
;        TNMIN_DEFITER to print the correct values in that case;
;        TNMIN_DEFITER now prints function gradient values if
;        requested;
;      - convert to common-based system of MPFIT for storing machine
;        constants; revert TNMIN_ENORM to simple sum of squares, at
;        least for now;
;      - remove limit on number of function evaluations, at least for
;        now, and until I can understand what happens when we do
;        numerical derivatives.
;   Further changes: more float vs double; disable TNMINSTEP for now;
;     experimented with convergence test in case of function
;     maximization, 11 Jan 2001, CM
;   TNMINSTEP is parsed but not enabled, 13 Mar 2001
;   Major code cleanups; internal docs; reduced commons, CM, 08 Apr
;     2001
;   Continued code cleanups; documentation; the STATUS keyword
;     actually means something, CM, 10 Apr 2001
;   Added reference to Nash paper, CM, 08 Feb 2002
;   Fixed 16-bit loop indices, D. Schelgel, 22 Apr 2003
;   Changed parens to square brackets because of conflicts with
;     limits function.  K. Tolbert, 23 Feb 2005
;   Some documentation clarifications, CM, 09 Nov 2007
;   Ensure that MY_FUNCT returns a scalar; make it more likely that
;     error messages get back out to the user; fatal CATCH'd error 
;     now returns STATUS = -18, CM, 17 Sep 2008
;
; TODO
;  - scale derivatives semi-automatically;
;  - ability to scale and offset parameters;
;
;  $Id$
;-
; Copyright (C) 1998-2001,2002,2003,2007,2008 Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy and distribute unmodified copies for
; non-commercial purposes, and to modify and use for personal or
; internal use, is granted.  All other rights are reserved.
;-

;%% TRUNCATED-NEWTON METHOD:  SUBROUTINES
;   FOR OTHER MACHINES, MODIFY ROUTINE MCHPR1 (MACHINE EPSILON)
;   WRITTEN BY:  STEPHEN G. NASH
;                OPERATIONS RESEARCH AND APPLIED STATISTICS DEPT.
;                GEORGE MASON UNIVERSITY
;                FAIRFAX, VA 22030
;******************************************************************

;; Routine which declares functions and common blocks
pro tnmin_dummy
  forward_function tnmin_enorm, tnmin_step1, tnmin
  forward_function tnmin_call, tnmin_autoder
  common tnmin_error, error_code
  common tnmin_machar, tnmin_machar_vals
  common tnmin_config, tnmin_tnconfig
  common tnmin_fcnargs, tnmin_tnfcnargs
  common tnmin_work, lsk, lyk, ldiagb, lsr, lyr
  a = 1
  return
end

;; Following are machine constants that can be loaded once.  I have
;; found that bizarre underflow messages can be produced in each call
;; to MACHAR(), so this structure minimizes the number of calls to
;; one.
pro tnmin_setmachar, double=isdouble

  common tnmin_machar, tnmin_machar_vals

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

  tnmin_machar_vals = {machep: dmachep, maxnum: dmaxnum, minnum: dminnum, $
                       maxlog: dmaxlog, minlog: dminlog, maxgam: dmaxgam, $
                       rdwarf: drdwarf, rgiant: drgiant}

  if (!version.release) LT 5 then dummy = check_math(0, 0)

  return
end

;; Procedure to parse the parameter values in PARINFO
pro tnmin_parinfo, parinfo, tnames, tag, values, default=def, status=status, $
                   n_param=n

  status = 0
  if n_elements(n) EQ 0 then n = n_elements(parinfo)

  if n EQ 0 then begin
      if n_elements(def) EQ 0 then return
      values = def
      status = 1
      return
  endif

  if n_elements(parinfo) EQ 0 then goto, DO_DEFAULT
  if n_elements(tnames) EQ 0 then tnames = tag_names(parinfo)
  wh = where(tnames EQ tag, ct)

  if ct EQ 0 then begin
      DO_DEFAULT:
      if n_elements(def) EQ 0 then return
      values = make_array(n, value=def(0))
      values(0) = def
  endif else begin
      values = parinfo.(wh(0))
  endelse

  status = 1
  return
end

;; Procedure to tie one parameter to another.
pro tnmin_tie, p, _ptied
  _wh = where(_ptied NE '', _ct)
 if _ct EQ 0 then return
  for _i = 0L, _ct-1 do begin
      _cmd = 'p('+strtrim(_wh(_i),2)+') = '+_ptied(_wh(_i))
      _err = execute(_cmd)
      if _err EQ 0 then begin
          message, 'ERROR: Tied expression "'+_cmd+'" failed.'
          return
      endif
  endfor
end

function tnmin_autoder, fcn, x, dx, dside=dside

  common tnmin_machar, machvals
  common tnmin_config, tnconfig

  MACHEP0 = machvals.machep
  DWARF   = machvals.minnum
  if n_elements(dside) NE n_elements(x) then dside = tnconfig.dside

  eps = sqrt(MACHEP0)
  h = eps * (1. + abs(x))

  ;; if STEP is given, use that
  wh = where(tnconfig.step GT 0, ct)
  if ct GT 0 then h(wh) = tnconfig.step(wh)

  ;; if relative step is given, use that
  wh = where(tnconfig.dstep GT 0, ct)
  if ct GT 0 then h(wh) = abs(tnconfig.dstep(wh)*x(wh))

  ;; In case any of the step values are zero
  wh = where(h EQ 0, ct)
  if ct GT 0 then h(wh) = eps

  ;; Reverse the sign of the step if we are up against the parameter
  ;; limit, or if the user requested it.
  mask = (dside EQ -1 OR (tnconfig.ulimited AND (x GT tnconfig.ulimit-h)))
  wh = where(mask, ct)
  if ct GT 0 then h(wh) = -h(wh)

  dx = x * 0.
  f = tnmin_call(fcn, x)
  for i = 0L, n_elements(x)-1 do begin
      if tnconfig.pfixed(i) EQ 1 then goto, NEXT_PAR
      hh = h(i)

      RESTART_PAR:
      xp = x
      xp(i) = xp(i) + hh

      fp = tnmin_call(fcn, xp)

      if abs(dside(i)) LE 1 then begin
          ;; COMPUTE THE ONE-SIDED DERIVATIVE
          dx(i) = (fp-f)/hh
      endif else begin
          ;; COMPUTE THE TWO-SIDED DERIVATIVE
          xp(i) = x(i) - hh

          fm = tnmin_call(fcn, xp)
          dx(i) = (fp-fm)/(2*hh)
      endelse
      NEXT_PAR:
  endfor

  return, f
end

;; Call user function or procedure, with _EXTRA or not, with
;; derivatives or not.
function tnmin_call, fcn, x1, dx, fullparam_=xall

;  on_error, 2
  common tnmin_config, tnconfig
  common tnmin_fcnargs, fcnargs

  if keyword_set(tnconfig.qanytied) then tnmin_tie, x, tnconfig.ptied
  ifree = tnconfig.ifree
  ;; Following promotes the byte array to a floating point array so
  ;; that users who simply re-fill the array aren't surprised when
  ;; their gradient comes out as bytes. :-)
  dx = tnconfig.pfixed + x1(0)*0.

  if n_elements(xall) GT 0 then begin
      x = xall
      x(ifree) = x1
  endif else begin
      x = x1
  endelse

  ;; Decide whether we are calling a procedure or function
  if tnconfig.proc then proc = 1 else proc = 0
  tnconfig.nfev = tnconfig.nfev + 1

  if n_params() EQ 3 then begin
      if tnconfig.autoderiv then $
        f = tnmin_autoder(fcn, x, dx) $
      else if n_elements(fcnargs) GT 0 then $
        f = call_function(fcn, x, dx, _EXTRA=fcnargs) $
      else $
        f = call_function(fcn, x, dx)

      dx = dx(ifree)
      if tnconfig.max then begin
          dx = -dx
          f = -f
      endif
  endif else begin
      if n_elements(fcnargs) GT 0 then $
        f = call_function(fcn, x, _EXTRA=fcnargs) $
      else $
        f = call_function(fcn, x)

      if n_elements(f) NE 1 then begin
          message, 'ERROR: function "'+fcn+'" returned a vector when '+$
            'a scalar was expected.'
      endif
  endelse

  if n_elements(f) GT 1 then return, f $
  else                       return, f(0)
end

function tnmin_enorm, vec

  common tnmin_config, tnconfig
  ;; Very simple-minded sum-of-squares
  if n_elements(tnconfig) GT 0 then if tnconfig.fastnorm then begin
      ans = sqrt(total(vec^2,1))
      goto, TERMINATE
  endif

  common tnmin_machar, machvals

  agiant = machvals.rgiant / n_elements(vec)
  adwarf = machvals.rdwarf * n_elements(vec)

  ;; This is hopefully a compromise between speed and robustness.
  ;; Need to do this because of the possibility of over- or underflow.
  mx = max(vec, min=mn)
  mx = max(abs([mx,mn]))
  if mx EQ 0 then return, vec(0)* 0.

  if mx GT agiant OR mx LT adwarf then ans = mx * sqrt(total((vec/mx)^2)) $
  else                                 ans = sqrt( total(vec^2) )

  TERMINATE:
  return, ans
end

;
; ROUTINES TO INITIALIZE PRECONDITIONER
;
pro tnmin_initpc, diagb, emat, n, upd1, yksk, gsk, yrsr, lreset
  ;; Rename common variables as they appear in INITP3.  Those
  ;; indicated in all caps are not used or renamed here.
; common tnmin_work, lsk, lyk, ldiagb, lsr, lyr
  common tnmin_work,  sk,  yk, LDIAGB,  sr,  yr
;                    I    I            I    I

  ;; From INITP3
  if keyword_set(upd1) then begin
      EMAT = DIAGB
  endif else if keyword_set(lreset) then begin
      BSK  = DIAGB*SK
      SDS  = TOTAL(SK*BSK)
      EMAT = DIAGB - DIAGB*DIAGB*SK*SK/SDS + YK*YK/YKSK
  endif else begin
      BSK  = DIAGB * SR
      SDS  = TOTAL(SR*BSK)
      SRDS = TOTAL(SK*BSK)
      YRSK = TOTAL(YR*SK)
      BSK  = DIAGB*SK - BSK*SRDS/SDS+YR*YRSK/YRSR
      EMAT = DIAGB-DIAGB*DIAGB*SR*SR/SDS+YR*YR/YRSR
      SDS  = TOTAL(SK*BSK)
      EMAT = EMAT - BSK*BSK/SDS+YK*YK/YKSK
  endelse

  return
end

pro tnmin_ssbfgs, n, gamma, sj, yj, hjv, hjyj, yjsj, yjhyj, $
         vsj, vhyj, hjp1v
;
; SELF-SCALED BFGS
;
  DELTA = (1. + GAMMA*YJHYJ/YJSJ)*VSJ/YJSJ - GAMMA*VHYJ/YJSJ
  BETA = -GAMMA*VSJ/YJSJ
  HJP1V = GAMMA*HJV + DELTA*SJ + BETA*HJYJ
  RETURN
end


;
; THIS ROUTINE ACTS AS A PRECONDITIONING STEP FOR THE
; LINEAR CONJUGATE-GRADIENT ROUTINE.  IT IS ALSO THE
; METHOD OF COMPUTING THE SEARCH DIRECTION FROM THE
; GRADIENT FOR THE NON-LINEAR CONJUGATE-GRADIENT CODE.
; IT REPRESENTS A TWO-STEP SELF-SCALED BFGS FORMULA.
;
pro tnmin_msolve, g, y, n, upd1, yksk, gsk, yrsr, lreset, first, $
                  hyr, hyk, ykhyk, yrhyr
  ;; Rename common variables as they appear in MSLV
; common tnmin_work, lsk, lyk, ldiagb, lsr, lyr
  common tnmin_work,  sk,  yk,  diagb,  sr,  yr
;                    I    I      I     I    I

  ;; From MSLV
  if keyword_set(UPD1) then begin
      Y = G / DIAGB
      RETURN
  endif

  ONE = G(0)*0 + 1.
  GSK = TOTAL(G*SK)

  if keyword_set(lreset) then begin
;
; COMPUTE GH AND HY WHERE H IS THE INVERSE OF THE DIAGONALS
;
      HG = G/DIAGB
      if keyword_set(FIRST) then begin
          HYK = YK/DIAGB
          YKHYK = TOTAL(YK*HYK)
      endif
      GHYK = TOTAL(G*HYK)
      TNMIN_SSBFGS,N,ONE,SK,YK,HG,HYK,YKSK, YKHYK,GSK,GHYK,Y
      RETURN
  endif

;
; COMPUTE HG AND HY WHERE H IS THE INVERSE OF THE DIAGONALS
;
  HG = G/DIAGB
  if keyword_set(FIRST) then begin
      HYK = YK/DIAGB
      HYR = YR/DIAGB
      YKSR = TOTAL(YK*SR)
      YKHYR = TOTAL(YK*HYR)
  endif
  GSR = TOTAL(G*SR)
  GHYR = TOTAL(G*HYR)
  if keyword_set(FIRST) then begin
      YRHYR = TOTAL(YR*HYR)
  endif

  TNMIN_SSBFGS,N,ONE,SR,YR,HG,HYR,YRSR, YRHYR,GSR,GHYR,HG
  if keyword_set(FIRST) then begin
      TNMIN_SSBFGS,N,ONE,SR,YR,HYK,HYR,YRSR, YRHYR,YKSR,YKHYR,HYK
  endif
  YKHYK = TOTAL(HYK*YK)
  GHYK  = TOTAL(HYK*G)
  TNMIN_SSBFGS,N,ONE,SK,YK,HG,HYK,YKSK, YKHYK,GSK,GHYK,Y

  RETURN
end

;
; THIS ROUTINE COMPUTES THE PRODUCT OF THE MATRIX G TIMES THE VECTOR
; V AND STORES THE RESULT IN THE VECTOR GV (FINITE-DIFFERENCE VERSION)
;
pro tnmin_gtims, v, gv, n, x, g, fcn, first, delta, accrcy, xnorm, $
        xnew

  IF keyword_set(FIRST) THEN BEGIN
      ;; Extra factor of ten is to avoid clashing with the finite
      ;; difference scheme which computes the derivatives
      DELTA = SQRT(100*ACCRCY)*(1.+XNORM)  ;; XXX diff than TN.F
;      DELTA = SQRT(ACCRCY)*(1.+XNORM)
      FIRST = 0
  ENDIF
  DINV = 1.  /DELTA

  F = tnmin_call(FCN, X + DELTA*V, GV, fullparam_=xnew)
  GV = (GV-G)*DINV
  return
end

;
; UPDATE THE PRECONDITIOING MATRIX BASED ON A DIAGONAL VERSION
; OF THE BFGS QUASI-NEWTON UPDATE.
;
pro tnmin_ndia3, n, e, v, gv, r, vgv
  VR = TOTAL(V*R)
  E = E - R*R/VR + GV*GV/VGV
  wh = where(e LE 1D-6, ct)
  if ct GT 0 then e(wh) = 1
  return
end

pro tnmin_fix, whlpeg, whupeg, z
  if whlpeg(0) NE -1 then z(whlpeg) = 0
  if whupeg(0) NE -1 then z(whupeg) = 0
end

;
; THIS ROUTINE PERFORMS A PRECONDITIONED CONJUGATE-GRADIENT
; ITERATION IN ORDER TO SOLVE THE NEWTON EQUATIONS FOR A SEARCH
; DIRECTION FOR A TRUNCATED-NEWTON ALGORITHM.  WHEN THE VALUE OF THE
; QUADRATIC MODEL IS SUFFICIENTLY REDUCED,
; THE ITERATION IS TERMINATED.
;
; PARAMETERS
;
; ZSOL        - COMPUTED SEARCH DIRECTION
; G           - CURRENT GRADIENT
; GV,GZ1,V    - SCRATCH VECTORS
; R           - RESIDUAL
; DIAGB,EMAT  - DIAGONAL PRECONDITONING MATRIX
; NITER       - NONLINEAR ITERATION #
; FEVAL       - VALUE OF QUADRATIC FUNCTION
pro tnmin_modlnp, zsol, gv, r, v, diagb, emat, $
         x, g, zk, n, niter, maxit, nmodif, nlincg, $
         upd1, yksk, gsk, yrsr, lreset, fcn, whlpeg, whupeg, $
         accrcy, gtp, gnorm, xnorm, xnew

;
; GENERAL INITIALIZATION
;
  zero = x(0)* 0.
  one = zero + 1
  IF (MAXIT EQ 0) THEN RETURN
  FIRST = 1
  RHSNRM = GNORM
  TOL = zero + 1.E-12
  QOLD = zero

;
; INITIALIZATION FOR PRECONDITIONED CONJUGATE-GRADIENT ALGORITHM
;
  tnmin_initpc, diagb, emat, n, upd1, yksk, gsk, yrsr, lreset

  R = -G
  V = G*0.
  ZSOL = V

;
; ************************************************************
; MAIN ITERATION
; ************************************************************
;
  FOR K = 1L, MAXIT DO BEGIN
      NLINCG = NLINCG + 1

;
; CG ITERATION TO SOLVE SYSTEM OF EQUATIONS
;
      tnmin_fix, whlpeg, whupeg, r
      TNMIN_MSOLVE, R, ZK, N, UPD1, YKSK, GSK, YRSR, LRESET, FIRST, $
        HYR, HYK, YKHYK, YRHYR
      tnmin_fix, whlpeg, whupeg, zk
      RZ = TOTAL(R*ZK)
      IF (RZ/RHSNRM LT TOL) THEN GOTO, MODLNP_80
      IF (K EQ 1) THEN BETA = ZERO $
      ELSE             BETA = RZ/RZOLD
      V = ZK + BETA*V
      tnmin_fix, whlpeg, whupeg, v
      TNMIN_GTIMS, V, GV, N, X, G, FCN, FIRST, DELTA, ACCRCY, XNORM, XNEW
      tnmin_fix, whlpeg, whupeg, gv
      VGV = TOTAL(V*GV)
      IF (VGV/RHSNRM LT TOL) THEN GOTO, MODLNP_50
      TNMIN_NDIA3, N,EMAT,V,GV,R,VGV
;
; COMPUTE LINEAR STEP LENGTH
;
      ALPHA = RZ / VGV
;
; COMPUTE CURRENT SOLUTION AND RELATED VECTORS
;
      ZSOL = ZSOL + ALPHA*V
      R = R - ALPHA*GV
;
; TEST FOR CONVERGENCE
;
      GTP = TOTAL(ZSOL*G)
      PR = TOTAL(R*ZSOL)
      QNEW = 5.E-1 * (GTP + PR)
      QTEST = K * (1.E0 - QOLD/QNEW)
      IF (QTEST LT 0.D0) THEN GOTO, MODLNP_70
      QOLD = QNEW
      IF (QTEST LE 5.D-1) THEN GOTO, MODLNP_70
;
; PERFORM CAUTIONARY TEST
;
      IF (GTP GT 0) THEN GOTO, MODLNP_40
      RZOLD = RZ
  ENDFOR

;
; TERMINATE ALGORITHM
;
  K = K-1
  goto, MODLNP_70

MODLNP_40:
  ZSOL = ZSOL - ALPHA*V
  GTP = TOTAL(ZSOL*G)
  goto, MODLNP_90

MODLNP_50:
  ;; printed output
MODLNP_60:
  IF (K GT 1) THEN GOTO, MODLNP_70
  TNMIN_MSOLVE,G,ZSOL,N,UPD1,YKSK,GSK,YRSR,LRESET,FIRST, $
    HYR, HYK, YKHYK, YRHYR
  ZSOL = -ZSOL
  tnmin_fix, whlpeg, whupeg, zsol
  GTP = TOTAL(ZSOL*G)
MODLNP_70:
  goto, MODLNP_90
MODLNP_80:
  IF (K  GT  1) THEN GOTO, MODLNP_70
  ZSOL = -G
  tnmin_fix, whlpeg, whupeg, zsol
  GTP = TOTAL(ZSOL*G)
  goto, MODLNP_70

;
; STORE (OR RESTORE) DIAGONAL PRECONDITIONING
;
MODLNP_90:
  diagb = emat
  return
end

function tnmin_step1, fnew, fm, gtp, smax, epsmch

; ********************************************************
; STEP1 RETURNS THE LENGTH OF THE INITIAL STEP TO BE TAKEN ALONG THE
; VECTOR P IN THE NEXT LINEAR SEARCH.
; ********************************************************

  D = ABS(FNEW-FM)
  ALPHA = FNEW(0)*0  + 1.
  IF (2.D0*D LE (-GTP) AND D GE EPSMCH) THEN $
    ALPHA = -2.*D/GTP
  IF (ALPHA GE SMAX) THEN ALPHA = SMAX
  return, alpha
end

;
; ************************************************************
; GETPTC, AN ALGORITHM FOR FINDING A STEPLENGTH, CALLED REPEATEDLY BY
; ROUTINES WHICH REQUIRE A STEP LENGTH TO BE COMPUTED USING CUBIC
; INTERPOLATION. THE PARAMETERS CONTAIN INFORMATION ABOUT THE INTERVAL
; IN WHICH A LOWER POINT IS TO BE FOUND AND FROM THIS GETPTC COMPUTES A
; POINT AT WHICH THE FUNCTION CAN BE EVALUATED BY THE CALLING PROGRAM.
; THE VALUE OF THE INTEGER PARAMETERS IENTRY DETERMINES THE PATH TAKEN
; THROUGH THE CODE.
; ************************************************************
pro tnmin_getptc, big, small, rtsmll, reltol, abstol, tnytol, $
         fpresn, eta, rmu, xbnd, u, fu, gu, xmin, fmin, gmin, $
         xw, fw, gw, a, b, oldf, b1, scxbnd, e, step, factor, $
         braktd, gtest1, gtest2, tol, ientry, itest

  ;; This chicanery is so that we get the data types right
  ZERO = fu(0)* 0.
; a1 = zero & scale = zero & chordm = zero
; chordu = zero & d1 = zero & d2 = zero
; denom = zero
  POINT1 = ZERO + 0.1
  HALF = ZERO + 0.5
  ONE = ZERO + 1
  THREE = ZERO + 3
  FIVE = ZERO + 5
  ELEVEN = ZERO + 11

  if ientry EQ 1 then begin ;; else clause = 20 (OK)
;
;      IENTRY=1
;      CHECK INPUT PARAMETERS
;
      ;; GETPTC_10:
      ITEST = 2
      IF (U LE ZERO OR XBND LE TNYTOL OR GU GT ZERO) THEN RETURN
      ITEST = 1
      IF (XBND LT ABSTOL) THEN ABSTOL = XBND
      TOL = ABSTOL
      TWOTOL = TOL + TOL
;
; A AND B DEFINE THE INTERVAL OF UNCERTAINTY, X AND XW ARE POINTS
; WITH LOWEST AND SECOND LOWEST FUNCTION VALUES SO FAR OBTAINED.
; INITIALIZE A,SMIN,XW AT ORIGIN AND CORRESPONDING VALUES OF
; FUNCTION AND PROJECTION OF THE GRADIENT ALONG DIRECTION OF SEARCH
; AT VALUES FOR LATEST ESTIMATE AT MINIMUM.
;
      A = ZERO
      XW = ZERO
      XMIN = ZERO
      OLDF = FU
      FMIN = FU
      FW = FU
      GW = GU
      GMIN = GU
      STEP = U
      FACTOR = FIVE
;
;      THE MINIMUM HAS NOT YET BEEN BRACKETED.
;
      BRAKTD = 0
;
; SET UP XBND AS A BOUND ON THE STEP TO BE TAKEN. (XBND IS NOT COMPUTED
; EXPLICITLY BUT SCXBND IS ITS SCALED VALUE.)  SET THE UPPER BOUND
; ON THE INTERVAL OF UNCERTAINTY INITIALLY TO XBND + TOL(XBND).
;
      SCXBND = XBND
      B = SCXBND + RELTOL*ABS(SCXBND) + ABSTOL
      E = B + B
      B1 = B
;
; COMPUTE THE CONSTANTS REQUIRED FOR THE TWO CONVERGENCE CRITERIA.
;
      GTEST1 = -RMU*GU
      GTEST2 = -ETA*GU
;
; SET IENTRY TO INDICATE THAT THIS IS THE FIRST ITERATION
;
      IENTRY = 2
      goto, GETPTC_210
  endif

;
; IENTRY = 2
;
; UPDATE A,B,XW, AND XMIN
;
  ;; GETPTC_20:
  IF (FU GT FMIN) THEN GOTO, GETPTC_60
;
; IF FUNCTION VALUE NOT INCREASED, NEW POINT BECOMES NEXT
; ORIGIN AND OTHER POINTS ARE SCALED ACCORDINGLY.
;
  CHORDU = OLDF - (XMIN + U)*GTEST1
  if NOT (FU LE CHORDU) then begin
;
; THE NEW FUNCTION VALUE DOES NOT SATISFY THE SUFFICIENT DECREASE
; CRITERION. PREPARE TO MOVE THE UPPER BOUND TO THIS POINT AND
; FORCE THE INTERPOLATION SCHEME TO EITHER BISECT THE INTERVAL OF
; UNCERTAINTY OR TAKE THE LINEAR INTERPOLATION STEP WHICH ESTIMATES
; THE ROOT OF F(ALPHA)=CHORD(ALPHA).
;
      CHORDM = OLDF - XMIN*GTEST1
      GU = -GMIN
      DENOM = CHORDM-FMIN
      IF (ABS(DENOM) LT 1.D-15) THEN BEGIN
          DENOM = ZERO + 1.E-15
          IF (CHORDM-FMIN LT 0.D0) THEN DENOM = -DENOM
      ENDIF
      IF (XMIN NE ZERO) THEN GU = GMIN*(CHORDU-FU)/DENOM
      FU = (HALF*U*(GMIN+GU) + FMIN) > FMIN
;
; IF FUNCTION VALUE INCREASED, ORIGIN REMAINS UNCHANGED
; BUT NEW POINT MAY NOW QUALIFY AS W.
;
      GETPTC_60:
      IF (U GE ZERO) THEN BEGIN
          B = U
          BRAKTD = 1
      ENDIF ELSE BEGIN
          A = U
      ENDELSE
      XW = U
      FW = FU
      GW = GU
  endif else begin
      ;; GETPTC_30:
      FW = FMIN
      FMIN = FU
      GW = GMIN
      GMIN = GU
      XMIN = XMIN + U
      A = A-U
      B = B-U
      XW = -U
      SCXBND = SCXBND - U
      IF (GU GT ZERO) THEN BEGIN
          B = ZERO
          BRAKTD = 1
      ENDIF ELSE BEGIN
          A = ZERO
      ENDELSE
      TOL = ABS(XMIN)*RELTOL + ABSTOL
  endelse

  TWOTOL = TOL + TOL
  XMIDPT = HALF*(A + B)

;
; CHECK TERMINATION CRITERIA
;
  CONVRG = ABS(XMIDPT) LE TWOTOL - HALF*(B-A) OR $
    ABS(GMIN) LE GTEST2 AND FMIN LT OLDF AND $
    (ABS(XMIN - XBND) GT TOL OR NOT BRAKTD)
  IF CONVRG THEN BEGIN
      ITEST = 0
      IF (XMIN NE ZERO) THEN RETURN
;
; IF THE FUNCTION HAS NOT BEEN REDUCED, CHECK TO SEE THAT THE RELATIVE
; CHANGE IN F(X) IS CONSISTENT WITH THE ESTIMATE OF THE DELTA-
; UNIMODALITY CONSTANT, TOL.  IF THE CHANGE IN F(X) IS LARGER THAN
; EXPECTED, REDUCE THE VALUE OF TOL.
;
      ITEST = 3
      IF (ABS(OLDF-FW) LE FPRESN*(ONE + ABS(OLDF))) THEN RETURN
      TOL = POINT1*TOL
      IF (TOL LT TNYTOL) THEN RETURN
      RELTOL = POINT1*RELTOL
      ABSTOL = POINT1*ABSTOL
      TWOTOL = POINT1*TWOTOL
  endif

;
; CONTINUE WITH THE COMPUTATION OF A TRIAL STEP LENGTH
;
  ;; GETPTC_100:
  R = ZERO
  Q = ZERO
  S = ZERO
  IF (ABS(E) GT TOL) THEN BEGIN
;
; FIT CUBIC THROUGH XMIN AND XW
;
      R = THREE*(FMIN-FW)/XW + GMIN + GW
      ABSR = ABS(R)
      Q = ABSR
      IF (GW EQ ZERO OR GMIN EQ ZERO) EQ 0 THEN BEGIN ;; else clause = 140 (OK)
;
; COMPUTE THE SQUARE ROOT OF (R*R - GMIN*GW) IN A WAY
; WHICH AVOIDS UNDERFLOW AND OVERFLOW.
;
          ABGW = ABS(GW)
          ABGMIN = ABS(GMIN)
          S = SQRT(ABGMIN)*SQRT(ABGW)
          IF ((GW/ABGW)*GMIN LE ZERO) THEN BEGIN
;
; COMPUTE THE SQUARE ROOT OF R*R + S*S.
;
              SUMSQ = ONE
              P = ZERO
              IF (ABSR LT S) THEN BEGIN ;; else clause = 110 (OK)
;
; THERE IS A POSSIBILITY OF OVERFLOW.
;
                  IF (S GT RTSMLL) THEN P = S*RTSMLL
                  IF (ABSR GE P) THEN SUMSQ = ONE +(ABSR/S)^2
                  SCALE = S
              endif else begin ;; flow to 120 (OK)
;
; THERE IS A POSSIBILITY OF UNDERFLOW.
;
                  ;; GETPTC_110:
                  IF (ABSR GT RTSMLL) THEN P = ABSR*RTSMLL
                  IF (S GE P) THEN SUMSQ = ONE + (S/ABSR)^2
                  SCALE = ABSR
              ENDELSE ;; flow to 120 (OK)
              ;; GETPTC_120:
              SUMSQ = SQRT(SUMSQ)
              Q = BIG
              IF (SCALE LT BIG/SUMSQ) THEN Q = SCALE*SUMSQ
          endif else begin ;; flow to 140
;
; COMPUTE THE SQUARE ROOT OF R*R - S*S
;
              ;; GETPTC_130:
              Q = SQRT(ABS(R+S))*SQRT(ABS(R-S))
              IF (R GE S OR R LE (-S)) EQ 0 THEN BEGIN
                  R = ZERO
                  Q = ZERO
                  goto, GETPTC_150
              endif
          endelse
      endif
;
; COMPUTE THE MINIMUM OF FITTED CUBIC
;
      ;; GETPTC_140:
      IF (XW LT ZERO) THEN Q = -Q
      S = XW*(GMIN - R - Q)
      Q = GW - GMIN + Q + Q
      IF (Q GT ZERO) THEN S = -S
      IF (Q LE ZERO) THEN Q = -Q
      R = E
      IF (B1 NE STEP OR BRAKTD) THEN E = STEP
  endif

;
; CONSTRUCT AN ARTIFICIAL BOUND ON THE ESTIMATED STEPLENGTH
;
GETPTC_150:
  A1 = A
  B1 = B
  STEP = XMIDPT
  IF (BRAKTD) EQ 0 THEN BEGIN ;; else flow to 160 (OK)
      STEP = -FACTOR*XW
      IF (STEP GT SCXBND) THEN STEP = SCXBND
      IF (STEP NE SCXBND) THEN FACTOR = FIVE*FACTOR
      ;; flow to 170 (OK)
  endif else begin
;
; IF THE MINIMUM IS BRACKETED BY 0 AND XW THE STEP MUST LIE
; WITHIN (A,B).
;
      ;; GETPTC_160:
      if (a NE zero OR xw GE zero) AND (b NE zero OR xw LE zero) then $
        goto, GETPTC_180
;
; IF THE MINIMUM IS NOT BRACKETED BY 0 AND XW THE STEP MUST LIE
; WITHIN (A1,B1).
;
      D1 = XW
      D2 = A
      IF (A EQ ZERO) THEN D2 = B
; THIS LINE MIGHT BE
;     IF (A EQ ZERO) THEN D2 = E
      U = - D1/D2
      STEP = FIVE*D2*(POINT1 + ONE/U)/ELEVEN
      IF (U LT ONE) THEN STEP = HALF*D2*SQRT(U)
  endelse
  ;; GETPTC_170:
  IF (STEP LE ZERO) THEN A1 = STEP
  IF (STEP GT ZERO) THEN B1 = STEP
;
; REJECT THE STEP OBTAINED BY INTERPOLATION IF IT LIES OUTSIDE THE
; REQUIRED INTERVAL OR IT IS GREATER THAN HALF THE STEP OBTAINED
; DURING THE LAST-BUT-ONE ITERATION.
;
GETPTC_180:
  if NOT (abs(s) LE abs(half*q*r) OR s LE q*a1 OR s GE q*b1) then begin
      ;; else clause = 200 (OK)
;
; A CUBIC INTERPOLATION STEP
;
      STEP = S/Q
;
; THE FUNCTION MUST NOT BE EVALUTATED TOO CLOSE TO A OR B.
;
      if NOT (step - a GE twotol AND b - step GE twotol) then begin
          ;; else clause = 210 (OK)
          IF (XMIDPT LE ZERO) THEN STEP = -TOL ELSE STEP = TOL
      endif ;; flow to 210 (OK)
  endif else begin
      ;; GETPTC_200:
      E = B-A
  endelse

;
; IF THE STEP IS TOO LARGE, REPLACE BY THE SCALED BOUND (SO AS TO
; COMPUTE THE NEW POINT ON THE BOUNDARY).
;
GETPTC_210:
  if (step GE scxbnd) then begin ;; else clause = 220 (OK)
      STEP = SCXBND
;
; MOVE SXBD TO THE LEFT SO THAT SBND + TOL(XBND) = XBND.
;
      SCXBND = SCXBND - (RELTOL*ABS(XBND)+ABSTOL)/(ONE + RELTOL)
  endif
  ;; GETPTC_220:
  U = STEP
  IF (ABS(STEP) LT TOL AND STEP LT ZERO) THEN U = -TOL
  IF (ABS(STEP) LT TOL AND STEP GE ZERO) THEN U = TOL
  ITEST = 1
  RETURN
end

;
;      LINE SEARCH ALGORITHMS OF GILL AND MURRAY
;
pro tnmin_linder, n, fcn, small, epsmch, reltol, abstol, $
         tnytol, eta, sftbnd, xbnd, p, gtp, x, f, alpha, g, $
         iflag, xnew

  zero = f(0) * 0.
  one = zero + 1.

  LSPRNT = 0L
  NPRNT  = 10000L
  RTSMLL = SQRT(SMALL)
  BIG = 1./SMALL
  ITCNT = 0L

;
;     SET THE ESTIMATED RELATIVE PRECISION IN F(X).
;
  FPRESN = 10.*EPSMCH
  U = ALPHA
  FU = F
  FMIN = F
  GU = GTP
  RMU = zero + 1E-4

;
;      FIRST ENTRY SETS UP THE INITIAL INTERVAL OF UNCERTAINTY.
;
  IENTRY = 1L

LINDER_10:
;
; TEST FOR TOO MANY ITERATIONS
;
  ITCNT = ITCNT + 1
  IF (ITCNT GT 30) THEN BEGIN
      ;; deviation from Nash: allow optimization to continue in outer
      ;; loop even if we fail to converge, if IFLAG EQ 0.  A value of
      ;; 1 indicates failure.  I believe that I tried IFLAG=0 once and
      ;; there was some problem, but I forget what it was.
      IFLAG = 1
      F = FMIN
      ALPHA = XMIN
      X = X + ALPHA*P
      RETURN
  ENDIF

  IFLAG = 0
  TNMIN_GETPTC,BIG,SMALL,RTSMLL,RELTOL,ABSTOL,TNYTOL, $
    FPRESN,ETA,RMU,XBND,U,FU,GU,XMIN,FMIN,GMIN, $
    XW,FW,GW,A,B,OLDF,B1,SCXBND,E,STEP,FACTOR, $
    BRAKTD,GTEST1,GTEST2,TOL,IENTRY,ITEST

;
;      IF ITEST=1, THE ALGORITHM REQUIRES THE FUNCTION VALUE TO BE
;      CALCULATED.
;
  IF (ITEST EQ 1) THEN BEGIN
      UALPHA = XMIN + U
      FU = TNMIN_CALL(FCN, X + UALPHA*P, LG, fullparam_=xnew)
      GU = TOTAL(LG*P)
;
;      THE GRADIENT VECTOR CORRESPONDING TO THE BEST POINT IS
;      OVERWRITTEN IF FU IS LESS THAN FMIN AND FU IS SUFFICIENTLY
;      LOWER THAN F AT THE ORIGIN.
;
      IF (FU LE FMIN AND FU LE OLDF-UALPHA*GTEST1) THEN $
        G = LG
;      print, 'fu = ', fu
      GOTO, LINDER_10
  ENDIF
;
;      IF ITEST=2 OR 3 A LOWER POINT COULD NOT BE FOUND
;
  IFLAG = 1
  IF (ITEST NE 0) THEN RETURN

;
;      IF ITEST=0 A SUCCESSFUL SEARCH HAS BEEN MADE
;
;  print, 'itcnt = ', itcnt
  IFLAG = 0
  F = FMIN
  ALPHA = XMIN
  X = X + ALPHA*P
  RETURN
END

pro tnmin_defiter, fcn, x, iter, fnorm, fmt=fmt, FUNCTARGS=fcnargs, $
                   quiet=quiet, deriv=df, dprint=dprint, pfixed=pfixed, $
                   maximize=maximize, _EXTRA=iterargs

  if keyword_set(quiet) then return
  if n_params() EQ 3 then begin
      fnorm = tnmin_call(fcn, x, df)
  endif

  if keyword_set(maximize) then f = -fnorm else f = fnorm
  print, iter, f, format='("Iter ",I6,"   FUNCTION = ",G20.8)'
  if n_elements(fmt) GT 0 then begin
      print, x, format=fmt
  endif else begin
      n = n_elements(x)
      ii = lindgen(n)
      p = '     P('+strtrim(ii,2)+') = '+string(x,format='(G)')
      if keyword_set(dprint) then begin
          p1 = strarr(n)
          wh = where(pfixed EQ 0, ct)
          if ct GT 0 AND n_elements(df) GE ct then begin
              if keyword_set(maximize) then df1 = -df else df1 = df
              p1(wh) = string(df1, format='(G)')
          endif
          wh = where(pfixed EQ 1, ct)
          if ct GT 0 then $
            p1(wh) = '          (FIXED)'
          p = p + '  :  DF/DP('+strtrim(ii,2)+') = '+p1
      endif
      print, p, format='(A)'
  endelse

  return
end

;      SUBROUTINE TNBC (IERROR, N, X, F, G, W, LW, SFUN, LOW, UP, IPIVOT)
;      IMPLICIT          DOUBLE PRECISION (A-H,O-Z)
;      INTEGER           IERROR, N, LW, IPIVOT(N)
;      DOUBLE PRECISION  X(N), G(N), F, W(LW), LOW(N), UP(N)
;
; THIS ROUTINE SOLVES THE OPTIMIZATION PROBLEM
;
;   MINIMIZE     F(X)
;      X
;   SUBJECT TO   LOW <= X <= UP
;
; WHERE X IS A VECTOR OF N REAL VARIABLES.  THE METHOD USED IS
; A TRUNCATED-NEWTON ALGORITHM (SEE "NEWTON-TYPE MINIMIZATION VIA
; THE LANCZOS ALGORITHM" BY S.G. NASH (TECHNICAL REPORT 378, MATH.
; THE LANCZOS METHOD" BY S.G. NASH (SIAM J. NUMER. ANAL. 21 (1984),
; PP. 770-778).  THIS ALGORITHM FINDS A LOCAL MINIMUM OF F(X).  IT DOES
; NOT ASSUME THAT THE FUNCTION F IS CONVEX (AND SO CANNOT GUARANTEE A
; GLOBAL SOLUTION), BUT DOES ASSUME THAT THE FUNCTION IS BOUNDED BELOW.
; IT CAN SOLVE PROBLEMS HAVING ANY NUMBER OF VARIABLES, BUT IT IS
; ESPECIALLY USEFUL WHEN THE NUMBER OF VARIABLES (N) IS LARGE.
;
; SUBROUTINE PARAMETERS:
;
; IERROR  - (INTEGER) ERROR CODE
;           ( 0 => NORMAL RETURN
;           ( 2 => MORE THAN MAXFUN EVALUATIONS
;           ( 3 => LINE SEARCH FAILED TO FIND LOWER
;           (          POINT (MAY NOT BE SERIOUS)
;           (-1 => ERROR IN INPUT PARAMETERS
; N       - (INTEGER) NUMBER OF VARIABLES
; X       - (REAL*8) VECTOR OF LENGTH AT LEAST N; ON INPUT, AN INITIAL
;           ESTIMATE OF THE SOLUTION; ON OUTPUT, THE COMPUTED SOLUTION.
; G       - (REAL*8) VECTOR OF LENGTH AT LEAST N; ON OUTPUT, THE FINAL
;           VALUE OF THE GRADIENT
; F       - (REAL*8) ON INPUT, A ROUGH ESTIMATE OF THE VALUE OF THE
;           OBJECTIVE FUNCTION AT THE SOLUTION; ON OUTPUT, THE VALUE
;           OF THE OBJECTIVE FUNCTION AT THE SOLUTION
; W       - (REAL*8) WORK VECTOR OF LENGTH AT LEAST 14*N
; LW      - (INTEGER) THE DECLARED DIMENSION OF W
; SFUN    - A USER-SPECIFIED SUBROUTINE THAT COMPUTES THE FUNCTION
;           AND GRADIENT OF THE OBJECTIVE FUNCTION.  IT MUST HAVE
;           THE CALLING SEQUENCE
;             SUBROUTINE SFUN (N, X, F, G)
;             INTEGER           N
;             DOUBLE PRECISION  X(N), G(N), F
; LOW, UP - (REAL*8) VECTORS OF LENGTH AT LEAST N CONTAINING
;           THE LOWER AND UPPER BOUNDS ON THE VARIABLES.  IF
;           THERE ARE NO BOUNDS ON A PARTICULAR VARIABLE, SET
;           THE BOUNDS TO -1.D38 AND 1.D38, RESPECTIVELY.
; IPIVOT  - (INTEGER) WORK VECTOR OF LENGTH AT LEAST N, USED
;           TO RECORD WHICH VARIABLES ARE AT THEIR BOUNDS.
;
; THIS IS AN EASY-TO-USE DRIVER FOR THE MAIN OPTIMIZATION ROUTINE
; LMQNBC.  MORE EXPERIENCED USERS WHO WISH TO CUSTOMIZE PERFORMANCE
; OF THIS ALGORITHM SHOULD CALL LMQBC DIRECTLY.
;
;----------------------------------------------------------------------
; THIS ROUTINE SETS UP ALL THE PARAMETERS FOR THE TRUNCATED-NEWTON
; ALGORITHM.  THE PARAMETERS ARE:
;
; ETA    - SEVERITY OF THE LINESEARCH
; MAXFUN - MAXIMUM ALLOWABLE NUMBER OF FUNCTION EVALUATIONS
; XTOL   - DESIRED ACCURACY FOR THE SOLUTION X*
; STEPMX - MAXIMUM ALLOWABLE STEP IN THE LINESEARCH
; ACCRCY - ACCURACY OF COMPUTED FUNCTION VALUES
; MSGLVL - CONTROLS QUANTITY OF PRINTED OUTPUT
;          0 = NONE, 1 = ONE LINE PER MAJOR ITERATION.
; MAXIT  - MAXIMUM NUMBER OF INNER ITERATIONS PER STEP
;
function tnmin, fcn, xall, fguess=fguess, functargs=fcnargs, parinfo=parinfo, $
                epsrel=epsrel0, epsabs=epsabs0, fastnorm=fastnorm, $
                nfev=nfev, maxiter=maxiter0, maxnfev=maxfun0, maximize=fmax, $
                errmsg=errmsg, nprint=nprint, status=status, nocatch=nocatch, $
                iterproc=iterproc, iterargs=iterargs, niter=niter,quiet=quiet,$
                autoderivative=autoderiv, iterderiv=iterderiv, bestmin=f

  if n_elements(nprint) EQ 0 then nprint = 1
  if n_elements(iterproc) EQ 0 then iterproc = 'TNMIN_DEFITER'
  if n_elements(autoderiv) EQ 0 then autoderiv = 0
  if n_elements(msglvl) EQ 0 then msglvl = 0
  if n_params() EQ 0 then begin
      message, "USAGE: PARMS = TNMIN('MYFUNCT', START_PARAMS, ... )", /info
      return, !values.d_nan
  endif
  iterd = keyword_set(iterderiv)
  maximize = keyword_set(fmax)
  status = 0L
  nfev = 0L
  errmsg = ''
  catch_msg = 'in TNMIN'

  common tnmin_config, tnconfig
  tnconfig = {fastnorm: keyword_set(fastnorm), proc: 0, nfev: 0L, $
              autoderiv: keyword_set(autoderiv), max: maximize}

  ;; Handle error conditions gracefully
  if NOT keyword_set(nocatch) then begin
      catch, catcherror
      if catcherror NE 0 then begin
          catch, /cancel
          err_string = ''+!error_state.msg
          message, /cont, 'Error detected while '+catch_msg+':'
          message, /cont,    err_string
          message, /cont, 'Error condition detected. Returning to MAIN level.'
          if errmsg EQ '' then $
            errmsg = 'Error detected while '+catch_msg+': '+err_string
          if status EQ 0 then status = -18
          return, !values.d_nan
      endif
  endif

  ;; Parinfo:
  ;; --------------- STARTING/CONFIG INFO (passed in to routine, not changed)
  ;; .value   - starting value for parameter
  ;; .fixed   - parameter is fixed
  ;; .limited - a two-element array, if parameter is bounded on
  ;;            lower/upper side
  ;; .limits - a two-element array, lower/upper parameter bounds, if
  ;;           limited vale is set
  ;; .step   - step size in Jacobian calc, if greater than zero

  catch_msg = 'parsing input parameters'
  ;; Parameters can either be stored in parinfo, or x.  Parinfo takes
  ;; precedence if it exists.
  if n_elements(xall) EQ 0 AND n_elements(parinfo) EQ 0 then begin
      errmsg = 'ERROR: must pass parameters in X or PARINFO'
      goto, TERMINATE
  endif

  ;; Be sure that PARINFO is of the right type
  if n_elements(parinfo) GT 0 then begin
      parinfo_size = size(parinfo)
      if parinfo_size(parinfo_size(0)+1) NE 8 then begin
          errmsg = 'ERROR: PARINFO must be a structure.'
          goto, TERMINATE
      endif
      if n_elements(xall) GT 0 AND n_elements(xall) NE n_elements(parinfo) $
        then begin
          errmsg = 'ERROR: number of elements in PARINFO and X must agree'
          goto, TERMINATE
      endif
  endif

  ;; If the parameters were not specified at the command line, then
  ;; extract them from PARINFO
  if n_elements(xall) EQ 0 then begin
      tnmin_parinfo, parinfo, tagnames, 'VALUE', xall, status=stx
      if stx EQ 0 then begin
          errmsg = 'ERROR: either X or PARINFO(*).VALUE must be supplied.'
          goto, TERMINATE
      endif

      sz = size(xall)
      ;; Convert to double if parameters are not float or double
      if sz(sz(0)+1) NE 4 AND sz(sz(0)+1) NE 5 then $
        xall = double(xall)
  endif
  npar = n_elements(xall)
  zero = xall(0) * 0.
  one  = zero + 1
  ten  = zero + 10
  twothird = (zero+2)/(zero+3)
  quarter = zero + 0.25
  half = zero + 0.5

  ;; Extract machine parameters
  sz = size(xall)
  tp = sz(sz(0)+1)
  if tp NE 4 AND tp NE 5 then begin
      if NOT keyword_set(quiet) then begin
          message, 'WARNING: input parameters must be at least FLOAT', /info
          message, '         (converting parameters to FLOAT)', /info
      endif
      xall = float(xall)
      sz = size(xall)
  endif
  isdouble = (sz(sz(0)+1) EQ 5)

  common tnmin_machar, machvals
  tnmin_setmachar, double=isdouble
  MCHPR1 = machvals.machep

  ;; TIED parameters?
  tnmin_parinfo, parinfo, tagnames, 'TIED', ptied, default='', n=npar
  ptied = strtrim(ptied, 2)
  wh = where(ptied NE '', qanytied)
  qanytied = qanytied GT 0
  tnconfig = create_struct(tnconfig, 'QANYTIED', qanytied, 'PTIED', ptied)

  ;; FIXED parameters ?
  tnmin_parinfo, parinfo, tagnames, 'FIXED', pfixed, default=0, n=npar
  pfixed = pfixed EQ 1
  pfixed = pfixed OR (ptied NE '')   ;; Tied parameters are also effectively fixed

  ;; Finite differencing step, absolute and relative, and sidedness of derivative
  tnmin_parinfo, parinfo, tagnames, 'STEP',     step, default=zero, n=npar
  tnmin_parinfo, parinfo, tagnames, 'RELSTEP', dstep, default=zero, n=npar
  tnmin_parinfo, parinfo, tagnames, 'TNSIDE',  dside, default=2,    n=npar

  ;; Maximum and minimum steps allowed to be taken in one iteration
  tnmin_parinfo, parinfo, tagnames, 'TNMAXSTEP', maxstep, default=zero, n=npar
  tnmin_parinfo, parinfo, tagnames, 'TNMINSTEP', minstep, default=zero, n=npar
  qmin = minstep *  0 ;; Disable minstep for now
  qmax = maxstep NE 0
  wh = where(qmin AND qmax AND maxstep LT minstep, ct)
  if ct GT 0 then begin
      errmsg = 'ERROR: TNMINSTEP is greater than TNMAXSTEP'
      goto, TERMINATE
  endif
  wh = where(qmin AND qmax, ct)
  qminmax = ct GT 0

  ;; Finish up the free parameters
  ifree = where(pfixed NE 1, ct)
  if ct EQ 0 then begin
      errmsg = 'ERROR: no free parameters'
      goto, TERMINATE
  endif

  ;; Compose only VARYING parameters
  xnew = xall      ;; xnew is the set of parameters to be returned
  x = xnew(ifree)  ;; x is the set of free parameters

  ;; LIMITED parameters ?
  tnmin_parinfo, parinfo, tagnames, 'LIMITED', limited, status=st1
  tnmin_parinfo, parinfo, tagnames, 'LIMITS',  limits,  status=st2
  if st1 EQ 1 AND st2 EQ 1 then begin

      ;; Error checking on limits in parinfo
      wh = where((limited[0,*] AND xall LT limits[0,*]) OR $
                 (limited[1,*] AND xall GT limits[1,*]), ct)
      if ct GT 0 then begin
          errmsg = 'ERROR: parameters are not within PARINFO limits'
          goto, TERMINATE
      endif
      wh = where(limited[0,*] AND limited[1,*] AND $
                 limits[0,*] GE limits[1,*] AND pfixed EQ 0, ct)
      if ct GT 0 then begin
          errmsg = 'ERROR: PARINFO parameter limits are not consistent'
          goto, TERMINATE
      endif


      ;; Transfer structure values to local variables
      qulim = limited[1, ifree]
      ulim  = limits [1, ifree]
      qllim = limited[0, ifree]
      llim  = limits [0, ifree]

      wh = where(qulim OR qllim, ct)
      if ct GT 0 then qanylim = 1 else qanylim = 0

  endif else begin

      ;; Fill in local variables with dummy values
      qulim = lonarr(n_elements(ifree))
      ulim  = x * 0.
      qllim = qulim
      llim  = x * 0.
      qanylim = 0

  endelse

  tnconfig = create_struct(tnconfig, $
                           'PFIXED', pfixed, 'IFREE', ifree, $
                           'STEP', step, 'DSTEP', dstep, 'DSIDE', dside, $
                           'ULIMITED', qulim, 'ULIMIT', ulim)


  common tnmin_fcnargs, tnfcnargs
  tnfcnargs = 0 & dummy = temporary(tnfcnargs)
  if n_elements(fcnargs) GT 0 then tnfcnargs = fcnargs

  ;; SET UP CUSTOMIZING PARAMETERS
  ;; ETA    - SEVERITY OF THE LINESEARCH
  ;; MAXFUN - MAXIMUM ALLOWABLE NUMBER OF FUNCTION EVALUATIONS
  ;; XTOL   - DESIRED ACCURACY FOR THE SOLUTION X*
  ;; STEPMX - MAXIMUM ALLOWABLE STEP IN THE LINESEARCH
  ;; ACCRCY - ACCURACY OF COMPUTED FUNCTION VALUES
  ;; MSGLVL - DETERMINES QUANTITY OF PRINTED OUTPUT
  ;;          0 = NONE, 1 = ONE LINE PER MAJOR ITERATION.
  ;; MAXIT  - MAXIMUM NUMBER OF INNER ITERATIONS PER STEP

  n = n_elements(x)
  if n_elements(maxit) EQ 0 then begin
      maxit = (n/2) < 50 > 2    ;; XXX diff than TN.F
  endif

  if n_elements(maxfun0) EQ 0 then $
    maxfun = 0L $
  else $
    maxfun = floor(maxfun0(0)) > 1
;  maxfun = 150L*n
;  if keyword_set(autoderiv) then $
;    maxfun = maxfun*(1L + round(total(abs(dside)> 1 < 2)))
  eta = zero + 0.25
  stepmx = zero + 10

  if n_elements(maxiter0) EQ 0 then $
    maxiter = 200L $
  else $
    maxiter = floor(maxiter0(0)) > 1

  g = replicate(x(0)* 0., n)

;; call minimizer
;
; THIS ROUTINE IS A BOUNDS-CONSTRAINED TRUNCATED-NEWTON METHOD.
; THE TRUNCATED-NEWTON METHOD IS PRECONDITIONED BY A LIMITED-MEMORY
; QUASI-NEWTON METHOD (THIS PRECONDITIONING STRATEGY IS DEVELOPED
; IN THIS ROUTINE) WITH A FURTHER DIAGONAL SCALING (SEE ROUTINE NDIA3).
; FOR FURTHER DETAILS ON THE PARAMETERS, SEE ROUTINE TNBC.
;

;
; initialize variables
;
  common tnmin_work, lsk, lyk, ldiagb, lsr, lyr
;                    I/O  I/O     I/O  I/O  I/O
  lsk = 0 & lyk = 0 & ldiagb = 0 & lsr = 0 & lyr = 0

  zero   = x(0)* 0.
  one    = zero + 1

  if n_elements(fguess) EQ 0 then fguess = one
  if maximize then f = -fguess else f = fguess
  conv = 0 & lreset = 0 & upd1 = 0 & newcon = 0
  gsk = zero & yksk = zero & gtp = zero & gtpnew = zero & yrsr = zero

  upd1 = 1
  ireset = 0L
  nmodif = 0L
  nlincg = 0L
  fstop  = f
  conv   = 0
  nm1    = n - 1

;; From CHKUCP
;
; CHECKS PARAMETERS AND SETS CONSTANTS WHICH ARE COMMON TO BOTH
; DERIVATIVE AND NON-DERIVATIVE ALGORITHMS
;
  EPSMCH = MCHPR1
  SMALL = EPSMCH*EPSMCH
  TINY = SMALL
  NWHY = -1L
;
; SET CONSTANTS FOR LATER
;
  ;; Some of these constants have been moved around for clarity (!)
  if n_elements(epsrel0) EQ 0 then epsrel = 100*MCHPR1 $
  else                             epsrel = epsrel0(0)+0.
  if n_elements(epsabs0) EQ 0 then epsabs = zero $
  else                             epsabs = abs(epsabs0(0))+0.

  ACCRCY = epsrel
  XTOL   = sqrt(ACCRCY)

  RTEPS = SQRT(EPSMCH)
  RTOL = XTOL
  IF (ABS(RTOL) LT ACCRCY) THEN RTOL = 10. *RTEPS

  FTOL2 = 0
  FTOL1 = RTOL^2 + EPSMCH    ;; For func chg convergence test (U1a)
  if epsabs NE 0 then $
    FTOL2 = EPSABS + EPSMCH ;; For absolute func convergence test (U1b)
  PTOL = RTOL + RTEPS       ;; For parm chg convergence test (U2)
  GTOL1 = ACCRCY^TWOTHIRD   ;; For gradient convergence test (U3, squared)
  GTOL2 = (1D-2*XTOL)^2     ;; For gradient convergence test (U4, squared)

;
; CHECK FOR ERRORS IN THE INPUT PARAMETERS
;
  IF (ETA LT 0.D0 OR STEPMX LT RTOL) THEN BEGIN
      errmsg = 'ERROR: input keywords are inconsistent'
      goto, TERMINATE
  endif
  ;; Check input parameters for errors
  if (n LE 0) OR (xtol LE 0) OR (maxit LE 0) then begin
      errmsg = 'ERROR: input keywords are inconsistent'
      goto, TERMINATE
  endif
  NWHY = 0L

  XNORM = TNMIN_ENORM(X)
  ALPHA = zero
  TEST = zero

; From SETUCR
;
; CHECK INPUT PARAMETERS, COMPUTE THE INITIAL FUNCTION VALUE, SET
; CONSTANTS FOR THE SUBSEQUENT MINIMIZATION
;
  fm = f
;
; COMPUTE THE INITIAL FUNCTION VALUE
;
  catch_msg = 'calling TNMIN_CALL'
  fnew = tnmin_call(fcn, x, g, fullparam_=xnew)

;
; SET CONSTANTS FOR LATER
;
  NITER = 0L
  OLDF = FNEW
  GTG = TOTAL(G*G)

  common tnmin_error, tnerr

  if nprint GT 0 AND iterproc NE '' then begin
      iflag = 0L
      if (niter-1) MOD nprint EQ 0 then begin
          catch_msg = 'calling '+iterproc
          tnerr = 0
          call_procedure, iterproc, fcn, xnew, niter, fnew, $
            FUNCTARGS=fcnargs, parinfo=parinfo, quiet=quiet, $
            dprint=iterd, deriv=g, pfixed=pfixed, maximize=maximize, $
            _EXTRA=iterargs
          iflag = tnerr
          if iflag LT 0 then begin
              errmsg = 'WARNING: premature termination by "'+iterproc+'"'
              nwhy = 4L
              goto, CLEANUP
          endif
      endif
  endif


  fold = fnew
  flast = fnew

; From LMQNBC
;
; TEST THE LAGRANGE MULTIPLIERS TO SEE IF THEY ARE NON-NEGATIVE.
; BECAUSE THE CONSTRAINTS ARE ONLY LOWER BOUNDS, THE COMPONENTS
; OF THE GRADIENT CORRESPONDING TO THE ACTIVE CONSTRAINTS ARE THE
; LAGRANGE MULTIPLIERS.  AFTERWORDS, THE PROJECTED GRADIENT IS FORMED.
;
  catch_msg = 'zeroing derivatives of pegged parameters'
  lmask = qllim AND (x EQ llim) AND (g GE 0)
  umask = qulim AND (x EQ ulim) AND (g LE 0)
  whlpeg = where(lmask, nlpeg)
  whupeg = where(umask, nupeg)
  tnmin_fix, whlpeg, whupeg, g
  GTG = TOTAL(G*G)

;
; CHECK IF THE INITIAL POINT IS A LOCAL MINIMUM.
;
  FTEST = ONE + ABS(FNEW)
  IF (GTG LT GTOL2*FTEST*FTEST) THEN GOTO, CLEANUP

;
; SET INITIAL VALUES TO OTHER PARAMETERS
;
  ICYCLE = NM1
  GNORM  = SQRT(GTG)
  DIFNEW = ZERO
  EPSRED = HALF/TEN
  FKEEP  = FNEW

;
; SET THE DIAGONAL OF THE APPROXIMATE HESSIAN TO UNITY.
;
  LDIAGB = replicate(one, n)


;
; ..................START OF MAIN ITERATIVE LOOP..........
;
; COMPUTE THE NEW SEARCH DIRECTION
;
  catch_msg = 'calling TNMIN_MODLNP'
  tnmin_modlnp, lpk, lgv, lz1, lv, ldiagb, lemat, $
    x, g, lzk, n, niter, maxit, nmodif, nlincg, upd1, yksk, $
    gsk, yrsr, lreset, fcn, whlpeg, whupeg, accrcy, gtpnew, gnorm, xnorm, $
    xnew

ITER_LOOP:
  catch_msg = 'computing step length'
  LOLDG = G
  PNORM = tnmin_enorm(LPK)
  OLDF = FNEW
  OLDGTP = GTPNEW

;
; PREPARE TO COMPUTE THE STEP LENGTH
;
  PE = PNORM + EPSMCH

;
; COMPUTE THE ABSOLUTE AND RELATIVE TOLERANCES FOR THE LINEAR SEARCH
;
  RELTOL = RTEPS*(XNORM + ONE)/PE
  ABSTOL = - EPSMCH*FTEST/(OLDGTP - EPSMCH)

;
; COMPUTE THE SMALLEST ALLOWABLE SPACING BETWEEN POINTS IN
; THE LINEAR SEARCH
;
  TNYTOL = EPSMCH*(XNORM + ONE)/PE

  ;; From STPMAX
  SPE = STEPMX/PE
  mmask = (NOT lmask AND NOT umask)
  wh = where(mmask AND (lpk GT 0) AND qulim AND (ulim - x LT spe*lpk), ct)
  if ct GT 0 then begin
      spe = min( (ulim(wh)-x(wh)) / lpk(wh))
  endif
  wh = where(mmask AND (lpk LT 0) AND qllim AND (llim - x GT spe*lpk), ct)
  if ct GT 0 then begin
      spe = min( (llim(wh)-x(wh)) / lpk(wh))
  endif

  ;; From LMQNBC
;
; SET THE INITIAL STEP LENGTH.
;
  ALPHA = TNMIN_STEP1(FNEW,FM,OLDGTP,SPE, epsmch)

;
; PERFORM THE LINEAR SEARCH
;
  catch_msg = 'performing linear search'
  tnmin_linder, n, fcn, small, epsmch, reltol, abstol, tnytol, $
    eta, zero, spe, lpk, oldgtp, x, fnew, alpha, g, nwhy, xnew

  NEWCON = 0
  IF (ABS(ALPHA-SPE) GT 1.D1*EPSMCH) EQ 0 THEN BEGIN
      NEWCON = 1
      NWHY   = 0L

      ;; From MODZ
      mmask = (NOT lmask AND NOT umask)
      wh = where(mmask AND (lpk LT 0) AND qllim $
                 AND (x-llim LE 10*epsmch*(abs(llim)+one)),ct)
      if ct GT 0 then begin
          flast = fnew
          lmask(wh) = 1
          x(wh) = llim(wh)
          whlpeg = where(lmask, nlpeg)
      endif
      wh = where(mmask AND (lpk GT 0) AND qulim $
                 AND (ulim-x LE 10*epsmch*(abs(ulim)+one)),ct)
      if ct GT 0 then begin
          flast = fnew
          umask(wh) = 1
          x(wh) = ulim(wh)
          whupeg = where(umask, nupeg)
      endif
      xnew(ifree) = x

      ;; From LMQNBC
      FLAST = FNEW
  endif

  FOLD = FNEW
  NITER = NITER + 1

;
; IF REQUIRED, PRINT THE DETAILS OF THIS ITERATION
;
  if nprint GT 0 AND iterproc NE '' then begin
      iflag = 0L
      xx = xnew
      xx(ifree) = x
      if (niter-1) MOD nprint EQ 0 then begin
          catch_msg = 'calling '+iterproc
          tnerr = 0
          call_procedure, iterproc, fcn, xx, niter, fnew, $
            FUNCTARGS=fcnargs, parinfo=parinfo, quiet=quiet, $
            dprint=iterd, deriv=g, pfixed=pfixed, maximize=maximize, $
            _EXTRA=iterargs
          iflag = tnerr
          if iflag LT 0 then begin
              errmsg = 'WARNING: premature termination by "'+iterproc+'"'
              nwhy = 4L
              goto, CLEANUP
          endif
      endif
  endif

  catch_msg = 'testing for convergence'
  IF (NWHY LT 0) THEN BEGIN
      NWHY = -2L
      goto, CLEANUP
  ENDIF
  IF (NWHY NE 0 AND NWHY NE 2) THEN BEGIN
      ;; THE LINEAR SEARCH HAS FAILED TO FIND A LOWER POINT
      NWHY = 3L
      goto, CLEANUP
  endif
  if nwhy GT 1 then begin
      fnew = tnmin_call(fcn, x, g, fullparam_=xnew)
  endif
  wh = where(finite(x) EQ 0, ct)
  if ct GT 0 OR finite(fnew) EQ 0 then begin
      nwhy = -3L
      goto, CLEANUP
  endif

;
; TERMINATE IF MORE THAN MAXFUN EVALUATIONS HAVE BEEN MADE
;
  NWHY = 2L
  if maxfun GT 0 AND tnconfig.nfev GT maxfun then goto, CLEANUP
  if niter GT maxiter then goto, CLEANUP
  NWHY = 0L

;
; SET UP PARAMETERS USED IN CONVERGENCE AND RESETTING TESTS
;
  DIFOLD = DIFNEW
  DIFNEW = OLDF - FNEW

;
; IF THIS IS THE FIRST ITERATION OF A NEW CYCLE, COMPUTE THE
; PERCENTAGE REDUCTION FACTOR FOR THE RESETTING TEST.
;
  IF (ICYCLE EQ 1) THEN BEGIN
      IF (DIFNEW GT 2.D0*DIFOLD) THEN EPSRED = EPSRED + EPSRED
      IF (DIFNEW LT 5.0D-1*DIFOLD) THEN EPSRED = HALF*EPSRED
  ENDIF
  LGV = G
  tnmin_fix, whlpeg, whupeg, lgv
  GTG = TOTAL(LGV*LGV)
  GNORM = SQRT(GTG)
  FTEST = ONE + ABS(FNEW)
  XNORM = tnmin_enorm(X)

  ;; From CNVTST
  LTEST = (FLAST - FNEW) LE (-5.D-1*GTPNEW)
  wh = where((lmask AND g LT 0) OR (umask AND g GT 0), ct)
  if ct GT 0 then begin
      conv = 0
      if NOT ltest then begin
          mx = max(abs(g(wh)), wh2)
          lmask(wh(wh2)) = 0 & umask(wh(wh2)) = 0
          FLAST = FNEW
          goto, CNVTST_DONE
      endif
  endif
  ;; Gill Murray and Wright tests are listed to the right.
  ;; Modifications due to absolute function value test are done here.

  fconv = abs(DIFNEW) LT FTOL1*FTEST              ;; U1a
  if ftol2 EQ 0 then begin
      pconv = ALPHA*PNORM LT PTOL*(ONE + XNORM)   ;; U2
      gconv = GTG LT GTOL1*FTEST*FTEST            ;; U3
  endif else begin
      ;; Absolute tolerance implies a loser constraint on parameters
      fconv = fconv OR (abs(difnew) LT ftol2)     ;; U1b
      acc2  = (FTOL2/FTEST + EPSMCH)
      pconv = ALPHA*PNORM LT SQRT(acc2)*(ONE + XNORM) ;; U2
      gconv = GTG LT (acc2^twothird)*FTEST*FTEST  ;; U3
  endelse
  IF ((PCONV AND FCONV AND GCONV) $               ;; U1 + U2 + U3
      OR (GTG LT GTOL2*FTEST*FTEST)) THEN BEGIN   ;; U4
      CONV = 1
  ENDIF ELSE BEGIN
      ;; Convergence failed
      CONV = 0
  ENDELSE

;
; FOR DETAILS, SEE GILL, MURRAY, AND WRIGHT (1981, P. 308) AND
; FLETCHER (1981, P. 116).  THE MULTIPLIER TESTS (HERE, TESTING
; THE SIGN OF THE COMPONENTS OF THE GRADIENT) MAY STILL NEED TO
; MODIFIED TO INCORPORATE TOLERANCES FOR ZERO.
;

CNVTST_DONE:
  ;; From LMQNBC

  IF (CONV) THEN GOTO, CLEANUP
  tnmin_fix, whlpeg, whupeg, g

;
; COMPUTE THE CHANGE IN THE ITERATES AND THE CORRESPONDING CHANGE
; IN THE GRADIENTS
;
  IF NEWCON EQ 0 THEN BEGIN
      LYK = G - LOLDG
      LSK = ALPHA*LPK
;
; SET UP PARAMETERS USED IN UPDATING THE PRECONDITIONING STRATEGY.
;
      YKSK = TOTAL(LYK*LSK)
      LRESET = 0

      IF (ICYCLE EQ NM1 OR DIFNEW LT EPSRED*(FKEEP-FNEW)) THEN LRESET = 1
      IF (LRESET EQ 0) THEN BEGIN
          YRSR = TOTAL(LYR*LSR)
          IF (YRSR LE ZERO) THEN LRESET = 1
      ENDIF
      UPD1 = 0
  ENDIF

;
;      COMPUTE THE NEW SEARCH DIRECTION
;
  ;; TNMIN_90:
  catch_msg = 'calling TNMIN_MODLNP'

  tnmin_modlnp, lpk, lgv, lz1, lv, ldiagb, lemat, $
    x, g, lzk, n, niter, maxit, nmodif, nlincg, upd1, yksk, $
    gsk, yrsr, lreset, fcn, whlpeg, whupeg, accrcy, gtpnew, gnorm, xnorm, $
    xnew

  IF (NEWCON) THEN GOTO, ITER_LOOP
;  IF (NOT LRESET) OR ICYCLE EQ 1 AND n_elements(LSR) GT 0 THEN BEGIN   ;; For testing
  IF (LRESET EQ 0) THEN BEGIN
;
; COMPUTE THE ACCUMULATED STEP AND ITS CORRESPONDING
; GRADIENT DIFFERENCE.
;
      LSR = LSR + LSK
      LYR = LYR + LYK
      ICYCLE = ICYCLE + 1
      goto, ITER_LOOP
  ENDIF

;
; RESET
;
  ;; TNMIN_110:
  IRESET = IRESET + 1
;
; INITIALIZE THE SUM OF ALL THE CHANGES IN X.
;
  LSR = LSK
  LYR = LYK
  FKEEP = FNEW
  ICYCLE = 1L
  goto, ITER_LOOP

;
; ...............END OF MAIN ITERATION.......................
;
  CLEANUP:

  nfev = tnconfig.nfev
  tnfcnargs = 0
  catch, /cancel
  case NWHY of
      -3: begin
          ;; INDEFINITE VALUE
          status = -16L
          if errmsg EQ '' then $
            errmsg = ('ERROR: parameter or function value(s) have become '+$
                      'infinite; check model function for over- '+$
                      'and underflow')
          return, !values.d_nan
      end
      -2: begin
          ;; INTERNAL ERROR IN LINE SEARCH
          status = -18L
          if errmsg EQ '' then $
            errmsg = 'ERROR: Fatal error during line search'
          return, !values.d_nan
      end
      -1: begin
          TERMINATE:
          ;; FATAL TERMINATION
          status = 0L
          if errmsg EQ '' then errmsg = 'ERROR: Invalid inputs'
          return, !values.d_nan
      end
      0: begin
          CONVERGED:
          status = 1L
      end
      2: begin
          ;; MAXIMUM NUMBER of FUNC EVALS or ITERATIONS REACHED
          if maxfun GT 0 AND nfev GT maxfun then begin
              status = -17L
              if errmsg EQ '' then $
                errmsg = ('WARNING: no convergence within maximum '+$
                          'number of function calls')
          endif else begin
              status = 5L
              if errmsg EQ '' then $
                errmsg = ('WARNING: no convergence within maximum '+$
                          'number of iterations')
          endelse
          FNEW = OLDF
      end
      3: begin
          status = -18L
          if errmsg EQ '' then errmsg = 'ERROR: Line search failed to converge'
      end
      4: begin
          ;; ABNORMAL TERMINATION BY USER ROUTINE
          status = iflag
      end
  endcase


  ;; Successful return
  F = FNEW
  xnew(ifree) = x
  return, xnew
end

