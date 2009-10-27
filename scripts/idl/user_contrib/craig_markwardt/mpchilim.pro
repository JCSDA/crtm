;+
; NAME:
;   MPCHILIM
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Compute confidence limits for chi-square statistic
;
; MAJOR TOPICS:
;   Curve and Surface Fitting, Statistics
;
; CALLING SEQUENCE:
;   DELCHI = MPCHILIM(PROB, DOF, [/SIGMA, /CLEVEL, /SLEVEL ])
;
; DESCRIPTION:
;
;  The function MPCHILIM() computes confidence limits of the
;  chi-square statistic for a desired probability level.  The returned
;  values, DELCHI, are the limiting chi-squared values: a chi-squared
;  value of greater than DELCHI will occur by chance with probability
;  PROB:
;
;    P_CHI(CHI > DELCHI; DOF) = PROB
;
;  In specifying the probability level the user has three choices:
;
;    * give the confidence level (default);
;
;    * give the significance level (i.e., 1 - confidence level) and
;      pass the /SLEVEL keyword; OR
;
;    * give the "sigma" of the probability (i.e., compute the
;      probability based on the normal distribution) and pass the
;      /SIGMA keyword.
;
;  Note that /SLEVEL, /CLEVEL and /SIGMA are mutually exclusive.
;
; INPUTS:
;
;   PROB - scalar or vector number, giving the desired probability
;          level as described above.
;
;   DOF - scalar or vector number, giving the number of degrees of
;         freedom in the chi-square distribution.
;
; RETURNS:
;
;  Returns a scalar or vector of chi-square confidence limits.
;
; KEYWORD PARAMETERS:
;
;   SLEVEL - if set, then PROB describes the significance level.
;
;   CLEVEL - if set, then PROB describes the confidence level
;            (default).
;
;   SIGMA - if set, then PROB is the number of "sigma" away from the
;           mean in the normal distribution.
;
; EXAMPLES:
;
;   print, mpchilim(0.99d, 2d, /clevel)
;
;   Print the 99% confidence limit for a chi-squared of 2 degrees of
;   freedom.
;
;   print, mpchilim(5d, 2d, /sigma)
;
;   Print the "5 sigma" confidence limit for a chi-squared of 2
;   degrees of freedom.  Here "5 sigma" indicates the gaussian
;   probability of a 5 sigma event or greater. 
;       P_GAUSS(5D) = 1D - 5.7330314e-07
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
;   Move STRICTARR compile option inside each function/procedure, 9
;     Oct 2006
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

forward_function cephes_ndtri, cephes_igam, cephes_igamc, cephes_igami

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

function cephes_igam, a, x
;   
;   	Incomplete gamma integral
;   
;   
;   
;    SYNOPSIS:
;   
;    double a, x, y, igam();
;   
;    y = igam( a, x );
;   
;    DESCRIPTION:
;   
;    The function is defined by
;   
;                              x
;                               -
;                      1       | |  -t  a-1
;     igam(a,x)  =   -----     |   e   t   dt.
;                     -      | |
;                    | (a)    -
;                              0
;   
;   
;    In this implementation both arguments must be positive.
;    The integral is evaluated by either a power series or
;    continued fraction expansion, depending on the relative
;    values of a and x.
;   
;    ACCURACY:
;   
;                         Relative error:
;    arithmetic   domain     # trials      peak         rms
;       IEEE      0,30       200000       3.6e-14     2.9e-15
;       IEEE      0,100      300000       9.9e-14     1.5e-14
  COMPILE_OPT strictarr
  common cephes_machar, machvals
  MAXLOG = machvals.maxlog
  MACHEP = machvals.machep

  if x LE 0 OR a LE 0 then return, 0.D
  if x GT 1. AND x GT a then return, 1.D - cephes_igamc(a, x)
  
  ax = a * alog(x) - x - lngamma(a)
  if ax LT -MAXLOG then begin
;      message, 'WARNING: underflow', /info
      return, 0.D
  endif
  ax = exp(ax)
  r = a
  c = 1.D
  ans = 1.D
  
  repeat begin
      r = r + 1
      c = c * x/r
      ans = ans + c
  endrep until (c/ans LE MACHEP)

  return, ans*ax/a
end

function cephes_igamc, a, x
;   
;   	Complemented incomplete gamma integral
;   
;   
;   
;    SYNOPSIS:
;   
;    double a, x, y, igamc();
;   
;    y = igamc( a, x );
;   
;    DESCRIPTION:
;   
;    The function is defined by
;   
;   
;     igamc(a,x)   =   1 - igam(a,x)
;   
;                               inf.
;                                 -
;                        1       | |  -t  a-1
;                  =   -----     |   e   t   dt.
;                       -      | |
;                      | (a)    -
;                                x
;   
;   
;    In this implementation both arguments must be positive.
;    The integral is evaluated by either a power series or
;    continued fraction expansion, depending on the relative
;    values of a and x.
;   
;    ACCURACY:
;   
;    Tested at random a, x.
;                   a         x                      Relative error:
;    arithmetic   domain   domain     # trials      peak         rms
;       IEEE     0.5,100   0,100      200000       1.9e-14     1.7e-15
;       IEEE     0.01,0.5  0,100      200000       1.4e-13     1.6e-15

  COMPILE_OPT strictarr
  common cephes_machar, machvals
  MAXLOG = machvals.maxlog
  MACHEP = machvals.machep

  big = 4.503599627370496D15
  biginv = 2.22044604925031308085D-16

  if x LE 0 OR a LE 0 then return, 1.D
  if x LT 1. OR x LT a then return, 1.D - cephes_igam(a, x)
  ax = a * alog(x) - x - lngamma(a)

  if ax LT -MAXLOG then begin
;      message, 'ERROR: underflow', /info
      return, 0.D
  endif
  
  ax = exp(ax)
  y = 1.D - a
  z = x + y + 1.D
  c = 0.D
  pkm2 = 1.D
  qkm2 = x
  pkm1 = x + 1.D
  qkm1 = z * x
  ans = pkm1 / qkm1

  repeat begin
      c = c + 1.D
      y = y + 1.D
      z = z + 2.D
      yc = y * c
      pk = pkm1 * z - pkm2 * yc
      qk = qkm1 * z - qkm2 * yc
      if qk NE 0 then begin
          r = pk/qk
          t = abs( (ans-r)/r )
          ans = r
      endif else begin
          t = 1.D
      endelse
      pkm2 = pkm1
      pkm1 = pk
      qkm2 = qkm1
      qkm1 = qk
      if abs(pk) GT big then begin
          pkm2 = pkm2 * biginv
          pkm1 = pkm1 * biginv
          qkm2 = qkm2 * biginv
          qkm1 = qkm1 * biginv
      endif
  endrep until t LE MACHEP

  return, ans * ax
end
  
function cephes_igami, a, y0
;  
;        Inverse of complemented imcomplete gamma integral
;  
;  
;  
;   SYNOPSIS:
;  
;   double a, x, p, igami();
;  
;   x = igami( a, p );
;  
;   DESCRIPTION:
;  
;   Given p, the function finds x such that
;  
;    igamc( a, x ) = p.
;  
;   Starting with the approximate value
;  
;           3
;    x = a t
;  
;    where
;  
;    t = 1 - d - ndtri(p) sqrt(d)
;   
;   and
;  
;    d = 1/9a,
;  
;   the routine performs up to 10 Newton iterations to find the
;   root of igamc(a,x) - p = 0.
;  
;   ACCURACY:
;  
;   Tested at random a, p in the intervals indicated.
;  
;                  a        p                      Relative error:
;   arithmetic   domain   domain     # trials      peak         rms
;      IEEE     0.5,100   0,0.5       100000       1.0e-14     1.7e-15
;      IEEE     0.01,0.5  0,0.5       100000       9.0e-14     3.4e-15
;      IEEE    0.5,10000  0,0.5        20000       2.3e-13     3.8e-14

  COMPILE_OPT strictarr
  common cephes_machar, machvals
  MAXNUM = machvals.maxnum
  MAXLOG = machvals.maxlog
  MACHEP = machvals.machep

  x0 = MAXNUM
  yl = 0.D
  x1 = 0.D
  yh = 1.D
  dithresh = 5.D * MACHEP

  d = 1.D/(9.D*a)
  y = (1.D - d - cephes_ndtri(y0) * sqrt(d))
  x = a * y * y * y
 
  lgm = lngamma(a)

  for i=0, 9 do begin
      if x GT x0 OR x LT x1 then goto, ihalve
      y = cephes_igamc(a, x)
      if y LT yl OR y GT yh then goto, ihalve
      if y LT y0 then begin
          x0 = x
          yl = y
      endif else begin
          x1 = x
          yh = y
      endelse
      
      d = (a-1.D) * alog(x) - x - lgm
      if d LT -MAXLOG then goto, ihalve
      d = -exp(d)
      d = (y - y0)/d
      if abs(d/x) LT MACHEP then goto, done
      x = x - d
  endfor

; Resort to interval halving if Newton iteration did not converge 
IHALVE:
  d = 0.0625D
  if x0 EQ MAXNUM then begin
      if x LE 0 then x = 1.D
      while x0 EQ MAXNUM do begin
          x = (1.D + d) * x
          y = cephes_igamc(a, x)
          if y LT y0 then begin
              x0 = x
              yl = y
              goto, DONELOOP1
          endif
          d = d + d
      endwhile
      DONELOOP1:
  endif

  d = 0.5
  dir = 0L
  for i=0, 399 do begin
      x = x1 + d * (x0-x1)
      y = cephes_igamc(a, x)
      lgm = (x0 - x1)/(x1 + x0)
      if abs(lgm) LT dithresh then goto, DONELOOP2
      lgm = (y - y0)/y0
      if abs(lgm) LT dithresh then goto, DONELOOP2
      if x LT 0 then goto, DONELOOP2
      if y GE y0 then begin
          x1 = x
          yh = y
          if dir LT 0 then begin
              dir = 0
              d = 0.5D
          endif else if dir GT 1 then begin
              d = 0.5 * d + 0.5
          endif else begin
              d = (y0 - yl)/(yh - yl)
          endelse
          dir = dir + 1
      endif else begin
          x0 = x
          yl = y
          if dir GT 0 then begin
              dir = 0
              d = 0.5
          endif else if dir LT -1 then begin
              d = 0.5 * d
          endif else begin
              d = (y0 - yl)/(yh - yl)
          endelse
          dir = dir - 1
      endelse
  endfor
  DONELOOP2:
  
  if x EQ 0 then begin
;      message, 'WARNING: underflow', /info
  endif

  DONE:
  return, x
end


function mpchilim, p, dof, sigma=sigma, clevel=clevel, slevel=slevel

  COMPILE_OPT strictarr

  if n_params() EQ 0 then begin
      message, 'USAGE: DELCHI = MPCHILIM(PROB, DOF, [/SIGMA, /CLEVEL, /SLEVEL ])', /info
      return, !values.d_nan
  endif

  cephes_setmachar   ;; Set machine constants
  if n_elements(dof) EQ 0 then dof = 1.

  ;; Confidence level is the default
  if n_elements(clevel) EQ 0 then clevel = 1
  if keyword_set(sigma) then begin  ;; Significance in terms of SIGMA
      slev = 1D - errorf(p/sqrt(2.))
  endif else if keyword_set(slevel) then begin ;; in terms of SIGNIFICANCE LEVEL
      slev = p
  endif else if keyword_set(clevel) then begin ;; in terms of CONFIDENCE LEVEL
      slev = 1.D - double(p)        
  endif else begin
      message, 'ERROR: must specify one of SIGMA, CLEVEL, SLEVEL'
  endelse

  ;; Output will have same type as input
  y = p*0

  ;; Loop through, computing the inverse, incomplete gamma function
  ;; slev is the significance level
  for i = 0L, n_elements(p)-1 do begin
      y[i] = 2.D * cephes_igami(0.5D*double(dof), slev[i])
  end

  return, y
end
