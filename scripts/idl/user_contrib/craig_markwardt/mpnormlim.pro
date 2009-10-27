;+
; NAME:
;   MPNORMLIM
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Compute confidence limits for normally distributed variable
;
; MAJOR TOPICS:
;   Curve and Surface Fitting, Statistics
;
; CALLING SEQUENCE:
;   Z = MPNORMLIM(PROB, [/CLEVEL, /SLEVEL ])
;
; DESCRIPTION:
;
;  The function MPNORMLIM() computes confidence limits of a normally
;  distributed variable (with zero mean and unit variance), for a
;  desired probability level.  The returned values, Z, are the
;  limiting values: a the magnitude of a normally distributed value
;  is greater than Z by chance with a probability PROB:
;
;    P_NORM(ABS(X) > Z) = PROB
;
;  In specifying the probability level the user has two choices:
;
;    * give the confidence level (default);
;
;    * give the significance level (i.e., 1 - confidence level) and
;      pass the /SLEVEL keyword; OR
;
;  Note that /SLEVEL and /CLEVEL are mutually exclusive.
;
; INPUTS:
;
;   PROB - scalar or vector number, giving the desired probability
;          level as described above.
;
; RETURNS:
;
;  Returns a scalar or vector of normal confidence limits.
;
; KEYWORD PARAMETERS:
;
;   SLEVEL - if set, then PROB describes the significance level.
;
;   CLEVEL - if set, then PROB describes the confidence level
;            (default).
;
; EXAMPLE:
;
;   print, mpnormlim(0.99d, /clevel)
;
;   Print the 99% confidence limit for a normally distributed
;   variable.  In this case it is about 2.58 sigma.
;
; REFERENCES:
;
;   Algorithms taken from CEPHES special function library, by Stephen
;   Moshier. (http://www.netlib.org/cephes/)
;
; MODIFICATION HISTORY:
;   Completed, 1999, CM
;   Documented, 16 Nov 2001, CM
;   Reduced obtrusiveness of common block and math error handling, 18
;     Nov 2001, CM
;   Convert to IDL 5 array syntax (!), 16 Jul 2006, CM
;   Move STRICTARR compile option inside each function/procedure, 9 Oct 2006
;   Add usage message, 24 Nov 2006, CM
;
;  $Id$
;-
; Copyright (C) 1997-2001, 2006, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

forward_function cephes_polevl, cephes_ndtri, mpnormlim

;; Set machine constants, once for this session.  Double precision
;; only.
pro cephes_setmachar
  COMPILE_OPT strictarr
  common cephes_machar, cephes_machar_vals
  if n_elements(cephes_machar_vals) GT 0 then return

  if (!version.release) LT 5 then dummy = check_math(1, 1)

  mch = machar(/double)
  machep = mch.eps
  maxnum = mch.xmax
  minnum = mch.xmin
  maxlog = alog(mch.xmax)
  minlog = alog(mch.xmin)
  maxgam = 171.624376956302725D

  cephes_machar_vals = {machep: machep, maxnum: maxnum, minnum: minnum, $
                        maxlog: maxlog, minlog: minlog, maxgam: maxgam}

  if (!version.release) LT 5 then dummy = check_math(0, 0)
  return
end

function cephes_polevl, x, coef
  COMPILE_OPT strictarr
  ans = coef[0]
  nc  = n_elements(coef)
  for i = 1L, nc-1 do ans = ans * x + coef[i]
  return, ans
end

function cephes_ndtri, y0
;   
;   	Inverse of Normal distribution function
;   
;   
;   
;    SYNOPSIS:
;   
;    double x, y, ndtri();
;   
;    x = ndtri( y );
;   
;   
;   
;    DESCRIPTION:
;   
;    Returns the argument, x, for which the area under the
;    Gaussian probability density function (integrated from
;    minus infinity to x) is equal to y.
;   
;   
;    For small arguments 0 < y < exp(-2), the program computes
;    z = sqrt( -2.0 * log(y) );  then the approximation is
;    x = z - log(z)/z  - (1/z) P(1/z) / Q(1/z).
;    There are two rational functions P/Q, one for 0 < y < exp(-32)
;    and the other for y up to exp(-2).  For larger arguments,
;    w = y - 0.5, and  x/sqrt(2pi) = w + w**3 R(w**2)/S(w**2)).
;   
;   
;    ACCURACY:
;   
;                         Relative error:
;    arithmetic   domain        # trials      peak         rms
;       DEC      0.125, 1         5500       9.5e-17     2.1e-17
;       DEC      6e-39, 0.135     3500       5.7e-17     1.3e-17
;       IEEE     0.125, 1        20000       7.2e-16     1.3e-16
;       IEEE     3e-308, 0.135   50000       4.6e-16     9.8e-17
;   
;   
;    ERROR MESSAGES:
;   
;      message         condition    value returned
;    ndtri domain       x <= 0        -MAXNUM
;    ndtri domain       x >= 1         MAXNUM
  COMPILE_OPT strictarr
  common cephes_ndtri_data, s2pi, p0, q0, p1, q1, p2, q2

  if n_elements(s2pi) EQ 0 then begin
      s2pi = sqrt(2.D*!dpi)
      p0 = [ -5.99633501014107895267D1,  9.80010754185999661536D1, $
             -5.66762857469070293439D1,  1.39312609387279679503D1, $
             -1.23916583867381258016D0 ]
      q0 = [ 1.D, $
             1.95448858338141759834D0,   4.67627912898881538453D0, $
             8.63602421390890590575D1,  -2.25462687854119370527D2, $
             2.00260212380060660359D2,  -8.20372256168333339912D1, $
             1.59056225126211695515D1,  -1.18331621121330003142D0  ]
      p1 = [ 4.05544892305962419923D0,   3.15251094599893866154D1, $
             5.71628192246421288162D1,   4.40805073893200834700D1, $
             1.46849561928858024014D1,   2.18663306850790267539D0, $
             -1.40256079171354495875D-1,-3.50424626827848203418D-2,$
             -8.57456785154685413611D-4  ]
      q1 = [ 1.D, $
             1.57799883256466749731D1,   4.53907635128879210584D1, $
             4.13172038254672030440D1,   1.50425385692907503408D1, $
             2.50464946208309415979D0,  -1.42182922854787788574D-1,$
             -3.80806407691578277194D-2,-9.33259480895457427372D-4 ]
      p2 = [  3.23774891776946035970D0,  6.91522889068984211695D0, $
              3.93881025292474443415D0,  1.33303460815807542389D0, $
              2.01485389549179081538D-1, 1.23716634817820021358D-2,$
              3.01581553508235416007D-4, 2.65806974686737550832D-6,$
              6.23974539184983293730D-9 ]
      q2 = [  1.D, $
              6.02427039364742014255D0,  3.67983563856160859403D0, $
              1.37702099489081330271D0,  2.16236993594496635890D-1,$
              1.34204006088543189037D-2, 3.28014464682127739104D-4,$
              2.89247864745380683936D-6, 6.79019408009981274425D-9]
  endif

  common cephes_machar, machvals
  MAXNUM = machvals.maxnum

  if y0 LE 0 then begin
      message, 'ERROR: domain', /info
      return, -MAXNUM
  endif
  if y0 GE 1 then begin
      message, 'ERROR: domain', /info
      return, MAXNUM
  endif

  code = 1
  y = y0
  exp2 = exp(-2.D)
  if y GT (1.D - exp2) then begin
      y = 1.D - y
      code = 0
  endif
  if y GT exp2 then begin
      y = y - 0.5
      y2 = y * y
      x = y + y * y2 * cephes_polevl(y2, p0) / cephes_polevl(y2, q0)
      x = x * s2pi
      return, x
  endif
  
  x = sqrt( -2.D * alog(y))
  x0 = x - alog(x)/x
  z = 1.D/x
  if x LT 8. then $
    x1 = z * cephes_polevl(z, p1) / cephes_polevl(z, q1) $
  else $
    x1 = z * cephes_polevl(z, p2) / cephes_polevl(z, q2)

  x = x0 - x1
  if code NE 0 then x = -x
  return, x
end

; MPNORMLIM - given a probability level, return the corresponding
;             "sigma" level.
;
;  p - Either the significance level (if SLEVEL is set) or the
;      confidence level (if CLEVEL is set).  This should be the
;      two-tailed level, ie:
;
;         * SLEVEL:   p = Prob(|z| > z0)
;         * CLEVEL:   p = Prob(|z| < z0)
;

function mpnormlim, p, clevel=clevel, slevel=slevel

  COMPILE_OPT strictarr

  if n_params() EQ 0 then begin
      message, 'USAGE: Z = MPNORMLIM(PROB, [/CLEVEL, /SLEVEL ])', /info
      return, !values.d_nan
  endif

  cephes_setmachar   ;; Set machine constants

  ;; Default is to assume the confidence level
  if n_elements(clevel) EQ 0 then clevel = 1
  y = 0 * p

  ;; cephes_ndtri accepts the integrated probability from negative
  ;; infinity to z, so we have to compute.
  if keyword_set(slevel) then begin
      p1 = 0.5D * p    ;; Take only one of the two tails
      for i = 0L, n_elements(y)-1 do begin
          y[i] = - cephes_ndtri(p1[i])
      endfor
  endif else if keyword_set(clevel) then begin
      p1 = 0.5D + 0.5D * p  ;; On binary computers this computation is 
      ;; exact (to the machine precision), so don't worry about it.
      ;; This computation shaves off the top half of the confidence
      ;; region, and then adds the "negative infinity to zero part.
      for i = 0L, n_elements(y)-1 do begin
          y[i] = cephes_ndtri(p1[i])
      endfor
  endif else begin
      message, 'ERROR: must specify one of CLEVEL or SLEVEL'
  endelse


  return, y
end
