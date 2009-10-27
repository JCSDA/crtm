;+
; NAME:
;   CHEBGRID
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Estimate Chebyshev polynomial coefficients of a function on a grid
;
; MAJOR TOPICS:
;   Curve and Surface Fitting
;
; CALLING SEQUENCE:
;   p = CHEBGRID(T, X, [ DXDT, NPOINTS=, NPOLY=, NGRANULE= , $
;                RMS=, DRMS=, RESIDUALS=, DRESIDUALS= , $
;                XMATRIX=, DXMATRIX=, RESET=, 
;                DERIV_WEIGHT= ] )
;
; DESCRIPTION:
;
;   CHEBGRID estimates the coefficients for a finite sum of Chebyshev
;   polynomials approximating a continuous tabulated function over an
;   interval.  The function (and optionally its derivative) must be
;   tabulated on a regularly sampled grid.  The implementation of this
;   function is taken from a method described by X. X. Newhall, used
;   in estimating coefficients for ephemerides in the solar system.
;
;   The tabulated function is assumed to be continuous over the entire
;   interval.  A Chebyshev series is fitted to the function over small
;   segments, called granules.  The size of each granule, the number
;   of points in each granule, and the number of Chebyshev polynomials
;   are all configurable.
;
;   Users may specify either the function alone, or the function and
;   its first derivative.  By also giving the tabulated derivative, a
;   more accurate Chebyshev polynomial can be developed.  Aside from
;   the constraints mentioned in the next paragraph, the polynomial
;   that is returned is the best-fit polynomial in a least-squares
;   sense.
;
;   Here is a definition of terms:
;
;      GRANULE - a single continuous fitted segment.  The length of the
;         granule, NGRANULE, is specified in units of the tabulated
;         grid size.  Because of the continuity requirements developed
;         below, granules will always overlap at their endpoints.
;         Thus, then length of a granule should be a factor of
;         N_ELEMENTS(X)-1.  For simple functions over short intervals,
;         the granule size can be equal to N_ELEMENTS(X)-1
;
;      NUMBER OF POINTS the number of points, NPOINTS, within a
;         granule to be fitted to the polynomial, not necessarily
;         equal to the granule size.  The greater the number of
;         points, the more computation time and storage is required.
;         This number *must* be a factor of NGRANULE.  Typically
;         NPOINTS is a number between 8 and 12.  Because of the
;         single-point overlap between granules (see below), the
;         actual number of points per fit is NPOINTS+1.
;
;      NUMBER OF POLYNOMIALS the number of Chebyshev polynomial terms,
;         NPOLYNOMIAL, to be fitted per granule.  The greater the
;         number of polynomial terms, the more computation time and
;         storage is required, but also the greater the approximating
;         precision of the fit.
;
;   The particular set of Chebyshev polynomial coefficients developed
;   by this function have some special properties.  If both the
;   function and its derivative are specified, then the value and
;   derivative of the interpolating polynomial at the granule
;   endpoints will be exactly equal to the tabulated endpoint values.
;   This feature allows many approximations to be strung together
;   piecewise, and the function value and first derivative will be
;   continuous across granule boundaries.
;
;   If only the function value is specified, then only the function
;   value will be continuous at the granule endpoints, and not the
;   derivative.
;
;   An extensive set of statistics are computed to assess the quality
;   of the Chebyshev polynomial fit.  The keywords RESIDUALS and
;   DRESIDUALS return the residuals of the fit after subtracting the
;   interpolation.  The RMS and DRMS keywords return the root mean
;   squared deviations between data and model.
;
;   If the user does not know how many granules, points, or polynomial
;   coefficients to use, then he or she should try several
;   combinations and see which minimizes the r.m.s. value with the
;   fewest number of coefficients.
;
;   If the XMATRIX and DXMATRIX keywords are passed, then CHEBGRID
;   attempts to avoid recomputing several of the matrices it uses in
;   estimating the coefficients.  If multiple calls to CHEBGRID are to
;   be made, some compution time savings can be made.  In the first
;   call CHEBGRID the required matrices are computed and returned.  In
;   subsequent calls, CHEBGRID detects the XMATRIX and DXMATRIX
;   keyword values and uses those values if it can.
;
;   The user can also estimate their own coefficients.  The matrices
;   returned are (NPOINTS+1)x(NPOLYNOMIAL).  The coefficients from a
;   NPOINTS+1 tabulation, X, are found by:
;
;      PCHEB = XMATRIX ## X  +  DXMATRIX ## DXDT
;
;   if derivative information is known, or
;
;      PCHEB = XMATRIX ## X
;
;   if no derivative information is known.  [ Note: the matrices are
;   different, depending on whether derivative information is known or
;   not. ]
;
;   
; INPUTS:
;
;   T - array of regularly sampled *independent* variables.  The number
;       of elements in T should be a multiple of NGRANULE, plus one.
;
;   X - array of regularly sampled *dependent* variables.  The number
;       of elements in X should be equal to the number of elements in
;       T.
;
;   DXDT - optionally, a tabulated array of first derivatives of X
;          with respect to T, at the same grid points.
;
; KEYWORD PARAMETERS:
;
;   NGRANULE - size of a "granule", in grid intervals.  NGRANULE must
;              be at least 2, and a factor of N_ELEMENTS(T)-1.
;              Default: 8
;
;   NPOINTS - number of points per granule that are fitted.  NPOINTS
;             must be at least 2, and a factor of NGRANULE. 
;             Default: NGRANULE
;
;   NPOLYNOMIAL - number of Chebyshev polynomial terms per fit.
;                 NPOLYNOMIAL must be at least 2 and less than
;                 2*(NPOINTS+1), when derivative information is
;                 specified; or less than NPOINTS+1, when no
;                 derivative information is specified.
;                 Default: 7
;
;  RESIDUALS - upon return, an array of size N_ELEMENTS(T), with
;              residuals of the tabulated function minus the
;              interpolated function.
;
;  DRESIDUALS - same as RESIDUALS, but for the function's first
;               derivative.
;
;  RMS - upon return, the root mean square of the function value
;        residuals.
;
;  DRMS - same as RMS, but for the function's first derivative.
;
;  XMATRIX - upon return, the matrix used to compute Chebyshev
;            polynomial coefficients from the function value.
;
;            Upon input, CHEBGRID determines if XMATRIX will apply to
;            the data, and if so, XMATRIX is reused rather than
;            computed.  If XMATRIX cannot be reused, then it is
;            computed afresh, and the new value is returned in the
;            XMATRIX keyword.
;
;            The user should not modify the contents of this array.
;
;  DXMATRIX - same as XMATRIX, but for the function's first
;             derivative.
;
;  RESET - if set, force a recomputation of XMATRIX and/or DXMATRIX.
;
;  DERIV_WEIGHT - amount of weight to give to function derivative,
;                 relative to the function value.
;                 Default: 0.16d
;
;
; RETURNS:
;
;   An array of coefficient values.  The dimensions of the array are
;   NPOLYNOMIALxNSEGS, where NSEGS is the number of granules in the
;   entire interval.
;
;
; EXAMPLE:
;
;   ;; Estimate Chebyshev coefficients for the function SIN(X), on the
;   ;; interval [-1,+1].  
;   xx = dindgen(9)/4d - 1d   ;; Regular grid from -1 to 1 (9 points)
;   yy = sin(xx)              ;; Function values, sin(x), ...
;   dy = cos(xx)              ;; ... and derivatives
;
;   ;; Estimate coefficients using CHEBGRID (single granule of 8 intervals)
;   p = chebgrid(xx, yy, dy, npoints=8, ngranule=8, npoly=10)
;
;   xxx = dindgen(1001)/500 - 1d   ;; New grid for testing
;   res = sin(xxx) - chebeval(xxx, p)
;   plot, xxx, res
;
;   ;; Same as example above, except extended range to [-1, +15],
;   using eight granules.
;   xx2 = dindgen(65)/4d - 1
;   yy2 = sin(xx2)
;   dy2 = cos(xx2)
;   p = chebgrid(xx2, yy2, dy2, ngranule=8, npoint=8, npoly=10)
;   help, p
;       P               DOUBLE    = Array[10, 8]
;   ;; (i.e., 10 polynomial coefficients over 8 granules)
;
;
; REFERENCES:
;
;   Abramowitz, M. & Stegun, I., 1965, *Handbook of Mathematical
;     Functions*, 1965, U.S. Government Printing Office, Washington,
;     D.C. (Applied Mathematical Series 55)
;   Newhall, X. X. 1989, Celestial Mechanics, 45, p. 305-310
;
; MODIFICATION HISTORY:
;   Written, CM, Feb 2002
;   Documented, CM, 24 Mar 2002
;   Corrected documentation, CM, 28 Apr 2002
;   Typo correction, CM, 10 Oct 2002
;
;  $Id$
;
;-
; Copyright (C) 2002, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

;; Utility function: compute XMATRIX and DXMATRIX using Newhall approach
pro chebpcmat, npts, npoly, xmat, vmat, dweight=weight0

  ;; n0 is the number of intervals in Cheb approx.
  n0 = npts - 1
  if n_elements(weight0) EQ 0 then $
    weight = 0.16d $
  else $
    weight = weight0(0)

  tmat = dblarr(npoly, npts)
  tdot = tmat

  cj = dblarr(npoly)
  xj = 1d - 2d*dindgen(npts)/n0
  for i = 0, npoly-1 do begin
      cj(*) = 0 & cj(i) = 1
      tmat(i,*) = chebeval(xj, cj, deriv=v)
      tdot(i,*) = v
  endfor

  ;; Form matrix T*W
  tw = dblarr(2,npts,npoly)
  tw(0,*,*) = transpose(tmat)
  tw(1,*,*) = transpose(tdot) * weight

  ;; Form matrix T*WT
  twt = reform(tw(0,*,*),npts,npoly) ## tmat + $
    reform(tw(1,*,*),npts,npoly) ## tdot

  tw  = reform(tw, 2*npts, npoly, /overwrite)
  twt = reform(twt, npoly, npoly, /overwrite)

  ;; Augment matrix T*W to get matrix C2
  c2 = dblarr(2*npts,npoly+4)
  c2(*,0:npoly-1) = tw
  c2(0,npoly)          = 1 & c2(1,npoly+1)        = 1
  c2(2*npts-2,npoly+2) = 1 & c2(2*npts-1,npoly+3) = 1

  ;; Augment matrix T*WT to get the matrix C1
  c1 = dblarr(npoly+4,npoly+4)
  c1(0:npoly-1,0:npoly-1) = twt

  c1(0:npoly-1,npoly+0) = tmat(*,0)
  c1(0:npoly-1,npoly+1) = tdot(*,0)
  c1(0:npoly-1,npoly+2) = tmat(*,npts-1)
  c1(0:npoly-1,npoly+3) = tdot(*,npts-1)

  c1(npoly:*,0:npoly-1) = transpose(c1(0:npoly-1,npoly:*))

  ;; Compute matrix C1^(-1)
  c1inv = invert(c1)
  ;; Compute matrix C1^(-1) C2
  c1c2 = c1inv ## c2

  c1c2 = reform(c1c2, 2,npts,npoly+4)
  c1c2 = reverse(c1c2,2)
  c1c2 = reform(c1c2, 2*npts,npoly+4)

  ii = lindgen(npts)*2
  xmat = c1c2(ii,0:npoly-1)   ;; Split into terms multiplying Y and VY
  vmat = c1c2(ii+1,0:npoly-1)

  return
end

;; Utility function: compute XMATRIX only, using only the constraint
;; on the function values at the endpoints.
pro chebpcmat_xonly, npts, npoly, xmat

  ;; n0 is the number of points in Cheb approx.
  n0 = npts - 1
  
  tmat = dblarr(npoly, npts)

  cj = dblarr(npoly)
  xj = 1d - 2d*dindgen(npts)/n0
  for i = 0, npoly-1 do begin
      cj(*) = 0 & cj(i) = 1
      tmat(i,*) = chebeval(xj, cj, deriv=v)
  endfor

  ;; Augment matrix T to get matrix C2
  c2 = dblarr(npts,npoly+2)
  c2(*,0:npoly-1) = transpose(tmat)
  c2(0,npoly)     = 1
  c2(npts-1,npoly+1) = 1

  ;; Augment matrix T*WT to get the matrix C1
  c1 = dblarr(npoly+2,npoly+2)
  c1(0:npoly-1,0:npoly-1) = transpose(tmat) ## tmat

  c1(0:npoly-1,npoly+0) = tmat(*,0)
  c1(0:npoly-1,npoly+1) = tmat(*,npts-1)

  c1(npoly:*,0:npoly-1) = transpose(c1(0:npoly-1,npoly:*))

  ;; Compute matrix C1^(-1)
  c1inv = invert(c1)
  ;; Compute matrix C1^(-1) C2
  c1c2 = c1inv ## c2

  c1c2 = reform(c1c2, npts,npoly+2)
  c1c2 = reverse(c1c2,1)
  xmat = c1c2(*,0:npoly-1)

  return
end

function chebgrid, t, x, dxdt, ngranule=ngran0, npoints=npts0, $
                   npolynomial=npoly0, deriv_weight=dweight0, $
                   rms=rms, drms=drms, residuals=resid, dresiduals=dresid, $
                   xmatrix=xmatrix, dxmatrix=dxmatrix, reset=reset

  ;; Default processing
  if n_elements(ngran0) EQ 0 then ngran = 8 $
  else ngran = round(ngran0(0)) > 2

  if n_elements(npts0) EQ 0 then npts = ngran $
  else npts = round(npts0(0)) > 2

  if n_elements(npoly0) EQ 0 then npoly = 7 $
  else npoly = round(npoly0(0)) > 2

  ;; Error checking
  if ngran LT npts then begin
      message, 'ERROR: Granule size ('+strtrim(ngran,2)+') is too '+ $
        'small for number of samples ('+strtrim(npts,2)+')'
      return, !values.d_nan
  endif
  
  ;; Be sure NGRAN is a multiple of NPTS - or not.  Instead, a warning
  ;; message is printed in the loop.
;  if abs(double(ngran)/npts - round(ngran/npts)) GT 1d-5 then begin
;      message, 'ERROR: NPOINTS must be a multiple of NGRANULE'
;      return, !values.d_nan
;  endif
  
  ;; Be sure we are solving a least-squares problem.  If the number of
  ;; polynomials is too great then it becomes underconstrained, not
  ;; overconstrained.
  if n_elements(dxdt) GT 0 then begin
      if npoly GE 2*(npts+1) then $
        message, 'ERROR: NPOLYNOMIAL must be less than 2*(NPOINTS+1)'
  endif else begin
      if npoly GE npts+1 then $
        message, 'ERROR: NPOLYNOMIAL must be less than NPOINTS+1'
  endelse
  
  ;; Begin size checking of input matrices - we may be able to use the
  ;; previously computed version.
  szx = size(xmatrix)
  szv = size(dxmatrix)

  ;; Cases: recompute because existing X matrix is wrong size;
  ;;        recompute because existing V matrix is wrong size;
  ;;        recompute because a V matrix was passed, but no DXDT was
  redo_x = (szx(0) NE 2 OR szx(1) NE npts+1 OR szx(2) NE npoly)
  redo_v = (n_elements(dxdt) GT 0 AND $
            (szv(0) NE 2 OR szv(1) NE npts+1 OR szv(2) NE npoly))
  no_v = (n_elements(dxdt) EQ 0 AND n_elements(dxmatrix) GT 0)

  ;; Actual recomputation of matrices
  if redo_x OR redo_v OR no_v OR keyword_set(reset) then begin
      COMPUTE_CHEBMAT:
      xmatrix = 0 & dummy = temporary(xmatrix)
      dxmatrix = 0 & dummy = temporary(dxmatrix)

      if n_elements(dxdt) GT 0 then $
        chebpcmat, npts+1, npoly, xmatrix, dxmatrix, dweight=dweight0 $
      else $
        chebpcmat_xonly, npts+1, npoly, xmatrix
  endif

  rms = 0.*x(0)
  drms = rms
  chebm = dblarr(npoly, (n_elements(x)-1)/ngran)
  resid = x*0.
  dresid = resid

  ispan = lindgen(npts+1)*(ngran/npts)
  imax = max(ispan)
  ng = 0L
  for ibase = 0, n_elements(x)-1, ngran do begin
      if ibase EQ n_elements(x)-1 then goto, DONE
      if n_elements(x)-ibase LT ngran+1 then begin
          nlost = n_elements(x)-ibase
          message, 'WARNING: last '+strtrim(nlost,2)+' elements of X '+$
            'were discarded because they formed only a fractional granule.', $
            /info
          goto, DONE
      endif

      tspan  = [t(ibase), t(ibase+imax)]
      tgran = t(ibase:ibase+imax-1)-t(ibase)
      dt     = tspan(1) - tspan(0)
      tspan  = tspan - tspan(0)

      ;; Compute the X portion of the coefficients
      xgran  = x(ibase+ispan)
      chebi  = xmatrix ## xgran

      ;; Compute the DXDT portion if it is available
      if n_elements(dxdt) GT 0 then begin
          dxgran = dxdt(ibase+ispan) * dt/2.
          chebi = chebi + dxmatrix ## dxgran

          ;; Statistics - V first, then X comes later
          xmod = chebeval(tgran, chebi, interval=tspan, derivative=dxmod)

          ;; DXDT portion of statistics
          dresid(ibase:ibase+imax-1) = dxdt(ibase:ibase+imax-1) -  dxmod
          diff_dx = (dxdt(ibase:ibase+imax-1) - dxmod)^2
          drms = drms + total(diff_dx)

      endif else begin
          ;; Statistics - X only

          xmod = chebeval(tgran, chebi, interval=tspan)
      endelse

      ;; Finish statistics with X portion
      resid(ibase:ibase+imax-1) = x(ibase:ibase+imax-1) -  xmod
      diff_x  = (   x(ibase:ibase+imax-1) -  xmod)^2
      rms  =  rms + total(diff_x)

      ;; Append to existing coefficient list
      chebm(*,ng) = chebi(*)
      ng = ng + 1L
  endfor
  
  DONE:
  ;; Final adjustments to statistics
  rms  = sqrt( rms / ngran)
  if n_elements(dxdt) GT 0 then drms = sqrt(drms / ngran)

  return, chebm
end
