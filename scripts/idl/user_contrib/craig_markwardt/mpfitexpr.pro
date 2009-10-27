;+
; NAME:
;   MPFITEXPR
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Perform Levenberg-Marquardt least-squares fit to arbitrary expression
;
; MAJOR TOPICS:
;   Curve and Surface Fitting
;
; CALLING SEQUENCE:
;   MYFUNCT = 'X*(1-X)+3'
;   parms = MPFITEXPR(MYFUNCT, XVAL, YVAL, ERR, start_parms, ...)
;
; DESCRIPTION:
;
;  MPFITEXPR fits a user-supplied model -- in the form of an arbitrary IDL
;  expression -- to a set of user-supplied data.  MPFITEXPR calls
;  MPFIT, the MINPACK-1 least-squares minimizer, to do the main
;  work.
;
;  Given the data and their uncertainties, MPFITEXPR finds the best set
;  of model parameters which match the data (in a least-squares
;  sense) and returns them in an array.
;  
;  The user must supply the following items:
;   - An array of independent variable values ("X").
;   - An array of "measured" *dependent* variable values ("Y").
;   - An array of "measured" 1-sigma uncertainty values ("ERR").
;   - A text IDL expression which computes Y given X.
;   - Starting guesses for all of the parameters ("START_PARAMS").
;
;  There are very few restrictions placed on X, Y or the expression of
;  the model.  Simply put, the expression must map the "X" values into
;  "Y" values given the model parameters.  The "X" values may
;  represent any independent variable (not just Cartesian X), and
;  indeed may be multidimensional themselves.  For example, in the
;  application of image fitting, X may be a 2xN array of image
;  positions.
;
;  Some rules must be obeyed in constructing the expression.  First,
;  the independent variable name *MUST* be "X" in the expression,
;  regardless of the name of the variable being passed to MPFITEXPR.
;  This is demonstrated in the above calling sequence, where the X
;  variable passed in is called "XVAL" but the expression still refers
;  to "X".  Second, parameter values must be referred to as an array
;  named "P".
;
;  If you do not pass in starting values for the model parameters,
;  MPFITEXPR will attempt to determine the number of parameters you
;  intend to have (it does this by looking for references to the array
;  variable named "P").  When no starting values are passed in, the
;  values are assumed to start at zero.
;
;  MPFITEXPR carefully avoids passing large arrays where possible to
;  improve performance.
;
;  See below for an example of usage.
;
; EVALUATING EXPRESSIONS
;
;  This source module also provides a function called MPEVALEXPR.  You
;  can use this function to evaluate your expression, given a list of
;  parameters.  This is one of the easier ways to compute the model
;  once the best-fit parameters have been found.  Here is an example:
;
;       YMOD = MPEVALEXPR(MYFUNCT, XVAL, PARMS)
;
;  where MYFUNCT is the expression (see MYFUNCT below), XVAL is the
;  list of "X" values, and PARMS is an array of parameters.  The
;  returned array YMOD contains the expression MYFUNCT evaluated at
;  each point in XVAL.
;
; PASSING PRIVATE DATA TO AN EXPRESSION
;
;  The most complicated optimization problems typically involve other
;  external parameters, in addition to the fitted parameters.  While
;  it is extremely easy to rewrite an expression dynamically, in case
;  one of the external parameters changes, the other possibility is to
;  use the PRIVATE data structure.
;
;  The user merely passes a structure to the FUNCTARGS keyword.  The
;  user expression receives this value as the variable PRIVATE.
;  MPFITEXPR nevers accesses this variable so it can contain any
;  desired values.  Usually it would be an IDL structure so that any
;  named external parameters can be passed to the expression.
;
;
; CONSTRAINING PARAMETER VALUES WITH THE PARINFO KEYWORD
;
;  The behavior of MPFIT can be modified with respect to each
;  parameter to be fitted.  A parameter value can be fixed; simple
;  boundary constraints can be imposed; limitations on the parameter
;  changes can be imposed; properties of the automatic derivative can
;  be modified; and parameters can be tied to one another.
;
;  These properties are governed by the PARINFO structure, which is
;  passed as a keyword parameter to MPFIT.
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
;              MPFIT, but are passed on to MYFUNCT for evaluation.
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
;                fitting code of MPFIT does not use this tag in any
;                way.  However, the default ITERPROC will print the
;                parameter name if available.
;  
;     .STEP - the step size to be used in calculating the numerical
;             derivatives.  If set to zero, then the step size is
;             computed automatically.  Ignored when AUTODERIVATIVE=0.
;             This value is superceded by the RELSTEP value.
;
;     .RELSTEP - the *relative* step size to be used in calculating
;                the numerical derivatives.  This number is the
;                fractional size of the step, compared to the
;                parameter value.  This value supercedes the STEP
;                setting.  If the parameter is zero, then a default
;                step size is chosen.
;
;     .MPSIDE - the sidedness of the finite difference when computing
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
;     .MPMAXSTEP - the maximum change to be made in the parameter
;                  value.  During the fitting process, the parameter
;                  will never be changed by more than this value in
;                  one iteration.
;
;                  A value of 0 indicates no maximum.  Default: 0.
;  
;     .TIED - a string expression which "ties" the parameter to other
;             free or fixed parameters as an equality constraint.  Any
;             expression involving constants and the parameter array P
;             are permitted.
;             Example: if parameter 2 is always to be twice parameter
;             1 then use the following: parinfo[2].tied = '2 * P[1]'.
;             Since they are totally constrained, tied parameters are
;             considered to be fixed; no errors are computed for them.
;             [ NOTE: the PARNAME can't be used in a TIED expression. ]
;
;     .MPPRINT - if set to 1, then the default ITERPROC will print the
;                parameter value.  If set to 0, the parameter value
;                will not be printed.  This tag can be used to
;                selectively print only a few parameter values out of
;                many.  Default: 1 (all parameters printed)
;
;     .MPFORMAT - IDL format string to print the parameter within
;                 ITERPROC.  Default: '(G20.6)'  (An empty string will
;                 also use the default.)
;
;  Future modifications to the PARINFO structure, if any, will involve
;  adding structure tags beginning with the two letters "MP".
;  Therefore programmers are urged to avoid using tags starting with
;  "MP", but otherwise they are free to include their own fields
;  within the PARINFO structure, which will be ignored by MPFIT.
;  
;  PARINFO Example:
;  parinfo = replicate({value:0.D, fixed:0, limited:[0,0], $
;                       limits:[0.D,0]}, 5)
;  parinfo[0].fixed = 1
;  parinfo[4].limited[0] = 1
;  parinfo[4].limits[0]  = 50.D
;  parinfo[*].value = [5.7D, 2.2, 500., 1.5, 2000.]
;  
;  A total of 5 parameters, with starting values of 5.7,
;  2.2, 500, 1.5, and 2000 are given.  The first parameter
;  is fixed at a value of 5.7, and the last parameter is
;  constrained to be above 50.
;
;
; COMPATIBILITY
;
;  This function is designed to work with IDL 5.0 or greater.  Because
;  this function uses the IDL EXECUTE() function, it will not work
;  with the free version of the IDL Virtual machine.
;  
;
; INPUTS:
;   MYFUNCT - a string variable containing an IDL expression.  The
;             only restriction is that the independent variable *must*
;             be referred to as "X" and model parameters *must* be
;             referred to as an array called "P".  Do not use symbol
;             names beginning with the underscore, "_".
;
;             The expression should calculate "model" Y values given
;             the X values and model parameters.  Using the vector
;             notation of IDL, this can be quite easy to do.  If your
;             expression gets complicated, you may wish to make an IDL
;             function which will improve performance and readibility.
;
;             The resulting array should be of the same size and
;             dimensions as the "measured" Y values.
;
;   X - Array of independent variable values.
;
;   Y - Array of "measured" dependent variable values.  Y should have
;       the same data type as X.  The function MYFUNCT should map
;       X->Y.
;
;   ERR - Array of "measured" 1-sigma uncertainties.  ERR should have
;         the same data type as Y.  ERR is ignored if the WEIGHTS
;         keyword is specified.
;
;   START_PARAMS - An array of starting values for each of the
;                  parameters of the model.  The number of parameters
;                  should be fewer than the number of measurements.
;                  Also, the parameters should have the same data type
;                  as the measurements (double is preferred).
;
;                  This parameter is optional if the PARINFO keyword
;                  is used (see MPFIT).  The PARINFO keyword provides
;                  a mechanism to fix or constrain individual
;                  parameters.  If both START_PARAMS and PARINFO are
;                  passed, then the starting *value* is taken from
;                  START_PARAMS, but the *constraints* are taken from
;                  PARINFO.
;
;                  If no parameters are given, then MPFITEXPR attempts
;                  to determine the number of parameters by scanning
;                  the expression.  Parameters determined this way are
;                  initialized to zero.  This technique is not fully
;                  reliable, so users are advised to pass explicit
;                  parameter starting values.
; 
;
; RETURNS:
;
;   Returns the array of best-fit parameters.
;
;
; KEYWORD PARAMETERS:
;
;   BESTNORM - the value of the summed, squared, weighted residuals
;              for the returned parameter values, i.e. the chi-square value.
;
;   COVAR - the covariance matrix for the set of parameters returned
;           by MPFIT.  The matrix is NxN where N is the number of
;           parameters.  The square root of the diagonal elements
;           gives the formal 1-sigma statistical errors on the
;           parameters IF errors were treated "properly" in MYFUNC.
;           Parameter errors are also returned in PERROR.
;
;           To compute the correlation matrix, PCOR, use this:
;           IDL> PCOR = COV * 0
;           IDL> FOR i = 0, n-1 DO FOR j = 0, n-1 DO $
;                PCOR[i,j] = COV[i,j]/sqrt(COV[i,i]*COV[j,j])
;
;           If NOCOVAR is set or MPFIT terminated abnormally, then
;           COVAR is set to a scalar with value !VALUES.D_NAN.
;
;   DOF - number of degrees of freedom, computed as
;             DOF = N_ELEMENTS(DEVIATES) - NFREE
;         Note that this doesn't account for pegged parameters (see
;         NPEGGED).
;
;   ERRMSG - a string error or warning message is returned.
;
;   FTOL - a nonnegative input variable. Termination occurs when both
;          the actual and predicted relative reductions in the sum of
;          squares are at most FTOL (and STATUS is accordingly set to
;          1 or 3).  Therefore, FTOL measures the relative error
;          desired in the sum of squares.  Default: 1D-10
;
;   FUNCTARGS - passed directly to the expression as the variable
;               PRIVATE.  Any user-private data can be communicated to
;               the user expression using this keyword.
;               Default: PRIVATE is undefined in user expression
;
;   GTOL - a nonnegative input variable. Termination occurs when the
;          cosine of the angle between fvec and any column of the
;          jacobian is at most GTOL in absolute value (and STATUS is
;          accordingly set to 4). Therefore, GTOL measures the
;          orthogonality desired between the function vector and the
;          columns of the jacobian.  Default: 1D-10
;
;   ITERARGS - The keyword arguments to be passed to ITERPROC via the
;              _EXTRA mechanism.  This should be a structure, and is
;              similar in operation to FUNCTARGS.
;              Default: no arguments are passed.
;
;   ITERPROC - The name of a procedure to be called upon each NPRINT
;              iteration of the MPFIT routine.  It should be declared
;              in the following way:
;
;              PRO ITERPROC, MYFUNCT, p, iter, fnorm, FUNCTARGS=fcnargs, $
;                PARINFO=parinfo, QUIET=quiet, ...
;                ; perform custom iteration update
;              END
;         
;              ITERPROC must either accept all three keyword
;              parameters (FUNCTARGS, PARINFO and QUIET), or at least
;              accept them via the _EXTRA keyword.
;          
;              MYFUNCT is the user-supplied function to be minimized,
;              P is the current set of model parameters, ITER is the
;              iteration number, and FUNCTARGS are the arguments to be
;              passed to MYFUNCT.  FNORM should be the
;              chi-squared value.  QUIET is set when no textual output
;              should be printed.  See below for documentation of
;              PARINFO.
;
;              In implementation, ITERPROC can perform updates to the
;              terminal or graphical user interface, to provide
;              feedback while the fit proceeds.  If the fit is to be
;              stopped for any reason, then ITERPROC should set the
;              common block variable ERROR_CODE to negative value (see
;              MPFIT_ERROR common block below).  In principle,
;              ITERPROC should probably not modify the parameter
;              values, because it may interfere with the algorithm's
;              stability.  In practice it is allowed.
;
;              Default: an internal routine is used to print the
;                       parameter values.
;
;   MAXITER - The maximum number of iterations to perform.  If the
;             number is exceeded, then the STATUS value is set to 5
;             and MPFIT returns.
;             Default: 200 iterations
;
;   NFEV - the number of MYFUNCT function evaluations performed.
;
;   NFREE - the number of free parameters in the fit.  This includes
;           parameters which are not FIXED and not TIED, but it does
;           include parameters which are pegged at LIMITS.
;
;   NITER - the number of iterations completed.
;
;   NOCOVAR - set this keyword to prevent the calculation of the
;             covariance matrix before returning (see COVAR)
;
;   NPEGGED - the number of free parameters which are pegged at a
;             LIMIT.
;
;   NPRINT - The frequency with which ITERPROC is called.  A value of
;            1 indicates that ITERPROC is called with every iteration,
;            while 2 indicates every other iteration, etc.  Note that
;            several Levenberg-Marquardt attempts can be made in a
;            single iteration.
;            Default value: 1
;
;   PARINFO - Provides a mechanism for more sophisticated constraints
;             to be placed on parameter values.  When PARINFO is not
;             passed, then it is assumed that all parameters are free
;             and unconstrained.  Values in PARINFO are never 
;             modified during a call to MPFIT.
;
;             See description above for the structure of PARINFO.
;
;             Default value:  all parameters are free and unconstrained.
;
;   PERROR - The formal 1-sigma errors in each parameter, computed
;            from the covariance matrix.  If a parameter is held
;            fixed, or if it touches a boundary, then the error is
;            reported as zero.
;
;            If the fit is unweighted (i.e. no errors were given, or
;            the weights were uniformly set to unity), then PERROR
;            will probably not represent the true parameter
;            uncertainties.  
;
;            *If* you can assume that the true reduced chi-squared
;            value is unity -- meaning that the fit is implicitly
;            assumed to be of good quality -- then the estimated
;            parameter uncertainties can be computed by scaling PERROR
;            by the measured chi-squared value.
;
;              DOF     = N_ELEMENTS(X) - N_ELEMENTS(PARMS) ; deg of freedom
;              PCERROR = PERROR * SQRT(BESTNORM / DOF)   ; scaled uncertainties
;
;   QUIET - set this keyword when no textual output should be printed
;           by MPFIT
;
;   STATUS - an integer status code is returned.  All values other
;            than zero can represent success.  It can have one of the
;            following values:
;
;	   0  improper input parameters.
;         
;	   1  both actual and predicted relative reductions
;	      in the sum of squares are at most FTOL.
;         
;	   2  relative error between two consecutive iterates
;	      is at most XTOL
;         
;	   3  conditions for STATUS = 1 and STATUS = 2 both hold.
;         
;	   4  the cosine of the angle between fvec and any
;	      column of the jacobian is at most GTOL in
;	      absolute value.
;         
;	   5  the maximum number of iterations has been reached
;         
;	   6  FTOL is too small. no further reduction in
;	      the sum of squares is possible.
;         
;	   7  XTOL is too small. no further improvement in
;	      the approximate solution x is possible.
;         
;	   8  GTOL is too small. fvec is orthogonal to the
;	      columns of the jacobian to machine precision.
;
;   WEIGHTS - Array of weights to be used in calculating the
;             chi-squared value.  If WEIGHTS is specified then the ERR
;             parameter is ignored.  The chi-squared value is computed
;             as follows:
;
;                CHISQ = TOTAL( (Y-MYFUNCT(X,P))^2 * ABS(WEIGHTS) )
;
;             Here are common values of WEIGHTS:
;
;                1D/ERR^2 - Normal weighting (ERR is the measurement error)
;                1D/Y     - Poisson weighting (counting statistics)
;                1D       - Unweighted
;
;   XTOL - a nonnegative input variable. Termination occurs when the
;          relative error between two consecutive iterates is at most
;          XTOL (and STATUS is accordingly set to 2 or 3).  Therefore,
;          XTOL measures the relative error desired in the approximate
;          solution.  Default: 1D-10
;
;   YFIT - the best-fit model function, as returned by MYFUNCT.
;
;
; EXAMPLE:
;
;   ; First, generate some synthetic data
;   x  = dindgen(200) * 0.1 - 10.                   ; Independent variable 
;   yi = gauss1(x, [2.2D, 1.4, 3000.]) + 1000       ; "Ideal" Y variable
;   y  = yi + randomn(seed, 200) * sqrt(yi)         ; Measured, w/ noise
;   sy = sqrt(y)                                    ; Poisson errors
;
;   ; Now fit a Gaussian to see how well we can recover
;   expr = 'P[0] + GAUSS1(X, P[1:3])'               ; fitting function
;   p0 = [800.D, 1.D, 1., 500.]                     ; Initial guess
;   p = mpfitexpr(expr, x, y, sy, p0)               ; Fit the expression
;   print, p
;
;   plot, x, y                                      ; Plot data
;   oplot, x, mpevalexpr(expr, x, p)                ; Plot model
;
;   Generates a synthetic data set with a Gaussian peak, and Poisson
;   statistical uncertainty.  Then a model consisting of a constant
;   plus Gaussian is fit to the data.
;
;
; COMMON BLOCKS:
;
;   COMMON MPFIT_ERROR, ERROR_CODE
;
;     User routines may stop the fitting process at any time by
;     setting an error condition.  This condition may be set in either
;     the user's model computation routine (MYFUNCT), or in the
;     iteration procedure (ITERPROC).
;
;     To stop the fitting, the above common block must be declared,
;     and ERROR_CODE must be set to a negative number.  After the user
;     procedure or function returns, MPFIT checks the value of this
;     common block variable and exits immediately if the error
;     condition has been set.  By default the value of ERROR_CODE is
;     zero, indicating a successful function/procedure call.
;
;
; REFERENCES:
;
;   MINPACK-1, Jorge More', available from netlib (www.netlib.org).
;   "Optimization Software Guide," Jorge More' and Stephen Wright, 
;     SIAM, *Frontiers in Applied Mathematics*, Number 14.
;
; MODIFICATION HISTORY:
;   Written, Apr-Jul 1998, CM
;   Added PERROR keyword, 04 Aug 1998, CM
;   Added COVAR keyword, 20 Aug 1998, CM
;   Added NITER output keyword, 05 Oct 1998
;      D.L Windt, Bell Labs, windt@bell-labs.com;
;   Added ability to return model function in YFIT, 09 Nov 1998
;   Parameter values can be tied to others, 09 Nov 1998
;   Added MPEVALEXPR utility function, 09 Dec 1998
;   Cosmetic documentation updates, 16 Apr 1999, CM
;   More cosmetic documentation updates, 14 May 1999, CM
;   Made sure to update STATUS value, 25 Sep 1999, CM
;   Added WEIGHTS keyword, 25 Sep 1999, CM
;   Changed from handles to common blocks, 25 Sep 1999, CM
;     - commons seem much cleaner and more logical in this case.
;   Alphabetized documented keywords, 02 Oct 1999, CM
;   Added QUERY keyword and query checking of MPFIT, 29 Oct 1999, CM
;   Check to be sure that X and Y are present, 02 Nov 1999, CM
;   Documented PERROR for unweighted fits, 03 Nov 1999, CM
;   Removed ITERPROC='' when quiet EQ 1, 10 Jan 2000, CM
;   Changed to ERROR_CODE for error condition, 28 Jan 2000, CM
;   Updated the EXAMPLE, 26 Mar 2000, CM
;   Copying permission terms have been liberalized, 26 Mar 2000, CM
;   Propagated improvements from MPFIT, 17 Dec 2000, CM
;   Correct reference to _WTS in MPFITEXPR_EVAL, 25 May 2001, CM
;      (thanks to Mark Fardal)
;   Added useful FUNCTARGS behavior (as yet undocumented), 04 Jul
;      2002, CM
;   Documented FUNCTARGS/PRIVATE behavior, 22 Jul 2002, CM
;   Added NFREE and NPEGGED keywords, 13 Sep 2002, CM
;   Documented RELSTEP field of PARINFO (!!), CM, 25 Oct 2002
;   Add DOF keyword, CM, 31 Jul 2003
;   Add FUNCTARGS keyword to MPEVALEXPR, CM 08 Aug 2003
;   Minor documentation adjustment, 03 Feb 2004, CM
;   Move STRICTARR compile option inside each function/procedure, 9 Oct 2006
;   Clarify documentation on user-function, derivatives, and PARINFO,
;     27 May 2007
;   Add COMPATIBILITY section, CM, 13 Dec 2007
;
;  $Id$
;-
; Copyright (C) 1997-2001, 2002, 2003, 2004, 2007, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

FORWARD_FUNCTION mpevalexpr, mpfitexpr_eval, mpfitexpr, mpfit

; Utility function which simply returns the value of the expression,
; evaluated at each point in x, using the parameters p.
function mpevalexpr, _expr, x, p, functargs=private

  COMPILE_OPT strictarr
  _cmd = '_f = '+_expr
  _err = execute(_cmd)
  return, _f
end

; This is the call-back function for MPFIT.  It evaluates the
; expression, subtracts the data, and returns the residuals.
function mpfitexpr_eval, p, _EXTRA=private

  COMPILE_OPT strictarr
  common mpfitexpr_common, _expr, x, y, err, _wts, _f

  ;; Compute the model value by executing the expression
  _f = 0.D
  _cmd = '_f = '+_expr
  _xxx = execute(_cmd)
  if _xxx EQ 0 then message, 'ERROR: command execution failed.'

  ;; Compute the deviates, applying either errors or weights
  if n_elements(err) GT 0 then begin
      result = (y-_f)/err
  endif else if n_elements(_wts) GT 0 then begin
      result = (y-_f)*_wts
  endif else begin
      result = (y-_f)
  endelse

  ;; The returned result should be one-dimensional
  result = reform(result, n_elements(result), /overwrite)
  return, result
end

;; This is the main entry point for this module
function mpfitexpr, expr, x, y, err, p, WEIGHTS=wts, $
                    BESTNORM=bestnorm, STATUS=status, nfev=nfev, $
                    parinfo=parinfo, query=query, functargs=fcnargs, $
                    covar=covar, perror=perror, yfit=yfit, $
                    niter=niter, nfree=nfree, npegged=npegged, dof=dof, $
                    quiet=quiet, _EXTRA=extra, errmsg=errmsg

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
      message, "USAGE: PARMS = MPFITEXPR('EXPR', X, Y, ERR, "+ $
        "START_PARAMS, ... )", /info
      return, !values.d_nan
  endif
  if n_elements(x) EQ 0 OR n_elements(y) EQ 0 then begin
      message, 'ERROR: X and Y must be defined', /info
      return, !values.d_nan
  endif

  ;; If no parameters are given, then parse the input expression,
  ;; and determine the number of parameters automatically.
  if (n_elements(parinfo) GT 0) AND (n_elements(p) EQ 0) then $
    p = parinfo[*].value
  if (n_elements(p) EQ 0) then begin
      pos = 0L
      nparams = 0L
      ee = strupcase(expr)
      ;; These are character constants representing the boundaries of
      ;; variable names.
      ca = (byte('A'))[0]
      cz = (byte('Z'))[0]
      c0 = (byte('0'))[0]
      c9 = (byte('9'))[0]
      c_ = (byte('_'))[0]  ;; Underscore can be in a variable name
      ll = strlen(ee)
      pnames = ['']

      ;; Now step through, looking for variables looking like p[0], etc.
      repeat begin
          i = [strpos(ee, 'P(', pos), strpos(ee, 'P[', pos)]
          wh = where(i GE 0, ct)
          if ct LE 0 then goto, DONE_PARAMS
          i = min(i[wh])

          ;; None found, finished
          if i LT 0 then goto, DONE_PARAMS
          ;; Too close to the end of the string
          if i GT ll-4 then goto, DONE_PARAMS

          ;; Have to be careful here, to be sure that this isn't just
          ;; a variable name ending in "p"
          maybe = 0
          ;; If this is the first character
          if i EQ 0 then maybe = 1 $
          else begin
              ;; Or if the preceding character is a non-variable character
              c = (byte(strmid(ee, i-1, 1)))[0]
              if NOT ( (c GE ca AND c LE cz) OR (c GE c0 AND c LE c9) $
                       OR c EQ c_ ) then maybe = 1
          endelse
          if maybe then begin
              ;; If we found one, then strip out the value inside the
              ;; parentheses.
              rest = strmid(ee, i+2, ll-i-2)
              next = str_sep(rest, ')', /trim)
              next = next[0]
              pnames = [pnames, next]
          endif
          pos = i+1
      endrep until pos GE ll

      DONE_PARAMS:
      if n_elements(pnames) EQ 1 then begin
          message, 'ERROR: no parameters to fit', /info
          return, !values.d_nan
      endif

      ;; Finally, we take the maximum parameter number
      pnames = pnames[1:*]
      nparams = max(long(pnames)) + 1
      if NOT keyword_set(quiet) then $
        message, '  Number of parameters: '+strtrim(nparams,2) $
        + ' (initialized to zero)', /info

      ;; Create a parameter vector, starting at zero
      p = dblarr(nparams)
  endif

  ;; Use common block to pass data back and forth
  common mpfitexpr_common, fc, xc, yc, ec, wc, mc
  fc = expr & xc = x & yc = y & mc = 0L
  ;; These optional parameters must be undefined first
  ec = 0 & dummy = size(temporary(ec))
  wc = 0 & dummy = size(temporary(wc))

  if n_elements(wts) GT 0 then begin
      wc = sqrt(abs(wts))
  endif else if n_elements(err) GT 0 then begin
      wh = where(err EQ 0, ct)
      if ct GT 0 then begin
          message, 'ERROR: ERROR value must not be zero.  Use WEIGHTS.', $
            /info
          return, !values.d_nan
      endif
      ec = err
  endif

  ;; Test out the function, as mpfit would call it, to see if it works
  ;; okay.  There is no sense in calling the fitter if the function
  ;; itself doesn't work.
  catch, catcherror
  if catcherror NE 0 then begin
      CATCH_ERROR:
      catch, /cancel
      message, 'ERROR: execution of "'+expr+'" failed.', /info
      message, '       check syntax and parameter usage', /info
      xc = 0 & yc = 0 & ec = 0 & wc = 0 & ac = 0
      return, !values.d_nan
  endif

  ;; Initialize.  Function that is actually called is mpfitexpr_eval,
  ;; which is a wrapper that sets up the expression evaluation.
  fcn = 'mpfitexpr_eval'

  ;; FCNARGS are passed by MPFIT directly to MPFITEXPR_EVAL.  These
  ;; actually contain the data, the expression, and a slot to return
  ;; the model function.
  fvec = call_function(fcn, p, _EXTRA=fcnargs)
  if n_elements(fvec) EQ 1 then $
    if NOT finite(fvec[0]) then goto, CATCH_ERROR
  ;; No errors caught if reached this stage
  catch, /cancel

  ;; Call MPFIT
  result = mpfit(fcn, p, $
                 parinfo=parinfo, STATUS=status, nfev=nfev, BESTNORM=bestnorm,$
                 covar=covar, perror=perror, functargs=fcnargs, $
                 niter=niter, nfree=nfree, npegged=npegged, dof=dof, $
                 ERRMSG=errmsg, quiet=quiet, _EXTRA=extra)

  ;; Retrieve the fit value
  yfit = temporary(mc)
  ;; Some cleanup
  xc = 0 & yc = 0 & wc = 0 & ec = 0 & mc = 0 & ac = 0

  ;; Print error message if there is one.
  if NOT keyword_set(quiet) AND errmsg NE '' then $
    message, errmsg, /info

  return, result
end
