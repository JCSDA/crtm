;+
; NAME:
;   LEGCHEB
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Compute Legendre polynomial coefficents from Chebyshev coefficients
;
; MAJOR TOPICS:
;   Curve and Surface Fitting, Special Functions
;
; CALLING SEQUENCE:
;   b = LEGCHEB(a)
;
; DESCRIPTION:
;
;   This routine computes the coefficients of a Legendre polynomial
;   expansion when the Chebyshev expansion is known.
;
;   Users can determine the Chebyshev expansion coefficients using a
;   routine like CHEBFIT, CHEBCOEF or CHEBGRID.  Then, if the Legendre
;   expansion is needed instead, this conversion routine should be
;   used.  Evaluation of the Legendre series can be performed using
;   the POLYLEG function in the IDL Astronomy Library.
;
;   Internally, the computational precision is double precision.
;   This routine relies upon the algorithm of Piessens (1974).
;
; INPUTS:
;
;   A - a vector, the coefficients of the Chebyshev series of the
;       desired function.
;
; RETURNS:
;
;   The vector B, which contains the coefficients of the Legendre
;   polynomial expansion.  Both A and B will have the same number of
;   elements and data type.
;
; KEYWORD PARAMETERS:
;
;   NONE
;
; EXAMPLE:
;
;   ;; Compute the Chebyshev series coefficients of 1/(2-X) on [-1,1]
;   A = CHEBCOEF('1d/(2d - X)', /expr)
;
;   ;; Convert to Legendre series coefficients
;   B = LEGCHEB(A)
;
; REFERENCES:
;
;   Abramowitz, M. & Stegun, I., 1965, *Handbook of Mathematical
;     Functions*, 1965, U.S. Government Printing Office, Washington,
;     D.C. (Applied Mathematical Series 55)
;   Piessens, R. 1974, Comm. ACM, v. 17, p. 25 (TOMS 473)
;
; MODIFICATION HISTORY:
;   Written and documented, CM, 25 Sep 2002
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

function legcheb, a, reset=reset

  common legcheb_common, ink

  n1 = n_elements(a)
  n  = n1-1

  ;; Internal routine: reset the common block
  if keyword_set(reset) then begin
      ink = 0 & dummy = temporary(ink)
      return, -1d
  endif

  ;; A common block is used to store the matrix which converts from
  ;; one representation to another.  This matrix needs to be expanded
  ;; if the input vector (size N1) is longer than the size of the
  ;; matrix (NINK x NINK).

  nink = sqrt(n_elements(ink))
  if nink LT n1 then begin

      ;; If we've never been called before, initialize the 2x2 array
      ;; with hard coded numbers
      if n_elements(ink) EQ 0 then begin
          ink = reform([[2d,0d],[0d, 2d/3d]], 2,2)
          nink = 2
      endif

      ;; Insert the new array into the old array
      inknew = dblarr(n1,n1)
      inknew(0:nink-1,0:nink-1) = ink
      ink = reform(inknew,n1,n1)
      
      ;; Compute the diagonal components, based on recurrence relation
      ;; in Piessens (1974)
      for nn = nink, n1-1 do begin
          ;; Recurrence relation for diagonal components
          ;; ORIG: ink(nn,nn) = ink(nn-1,nn-1)*4d*nn^2/(2d*nn+1)/(2d*nn)
          ink(nn,nn) = ink(nn-1,nn-1)*2d*nn/(2d*nn+1)
      endfor

      ;; Special case: first row, because of a 0/0 condition
      kk = dindgen(n1/2)*2
      ink(kk,0) = -2d/(kk*kk - 1)
      
      ;; Fill remaining columns of the INK array, using the recurrence
      ;; of eqn 6 in Piessens
      for nn = 1, n1-1 do begin
          ;; KSTART is the starting index, which could be larger than
          ;; NINK because the elements left of the diagonal are always
          ;; zero.  The NINK-1 vs NINK-2 logic is because the cells
          ;; alternate nonzero quantities.
          kstart = nink-1
          if ink(kstart,nn) EQ 0 then kstart = nink-2
          kstart = kstart>nn

          ;; Recurrence used here.
          for kk = kstart, n1-3, 2 do if ink(kk,nn) NE 0 then begin
              ink(kk+2,nn) = (ink(kk,nn) $
                              * (double((kk-1)*kk - nn*(nn+1)) $
                                 / ((kk+3)*(kk+2) - nn*(nn+1))) $
                              * (double(kk+2)/kk))
          end
      endfor
  endif

  ;; Extract relevant portion of INK matrix
  nn = dindgen(n1)
  mat = ink(0:n1-1,0:n1-1)

  ;; Keep same data type for A and B
  b = reform(a)*0.

  ;; Compute B by matrix multiplication
  b(*) = (mat ## a(*))

  ;; Apply final multiplicative factor, the normalization of the
  ;; Legendre polynomials.
  return, b*(0.5d + nn)
end
