;+
; NAME:
;   QRSOLV
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Solve a linear equation after performing QR factorization
;
; MAJOR TOPICS:
;   Linear Systems
;
; CALLING SEQUENCE:
;   X = QRSOLV(A, R, B, PIVOTS=IPVT)
;
; DESCRIPTION:
;
;  The procedure QRSOLV completes the solution of a linear equation,
;
;        A ## x = B
;
;  after the MxN matrix has been factorized by QR decomposition.
;  After being factorized once using QRFAC, the matrices can be used
;  for multiple righthand sides (i.e., different B's).
;
;  The solution technique is to first compute the factorization using
;  QRFAC, which yields the orthogonal matrix Q and the upper
;  triangular matrix R.  [ Actually, Q is represented by its
;  Householder reflectors. ]  Then the solution vector, X, is computed
;  using QRSOLV.
;
;  If pivoting was performed in the factorization, the permutation
;  vector IPVT returned by QRFAC must also be passed to QRSOLV.
;  
;
; PARAMETERS:
;
;   A - upon input, the factorized matrix A, returned by QRFAC.
;
;   R - upon input, the upper diagonal matrix R, returned by QRFAC.
;
;   B - upon input, the righthand vector B, which fits into the
;       equation,  A ## x = B
;
;   X - upon ouptut, the solution vector X, to the above linear
;       equation.  For an overdetermined system, X is the least
;       squares solution which minimizes TOTAL( (A ## X - B)^2 ).
;
;
; KEYWORD PARAMETERS:
;
;   PIVOTS - upon input, the permutation matrix IPVT returned by
;            QRFAC, if pivoting is to be performed.
;
;
; EXAMPLE:
;
;  Solve the equation A ## X = B, in the least squares sense, where:
;
;    A = [[1.0,1.0,1.0,1.0,1.0,1.0],$
;         [0.6,0.8,0.5,0.8,0.7,0.9],$
;         [0.2,0.3,0.1,0.4,0.3,0.4]]
;
;  and B = [0.57E,0.69,0.5,0.7,0.6,0.8]
;
;  qrfac, a, r, ipvt, /PIVOT
;  x = qrsolv(a, r, b, PIVOTS=ipvt)
;
;  print, x
;       0.0834092     0.852273    -0.179545
;
; REFERENCES:
;
;   More', Jorge J., "The Levenberg-Marquardt Algorithm:
;     Implementation and Theory," in *Numerical Analysis*, ed. Watson,
;     G. A., Lecture Notes in Mathematics 630, Springer-Verlag, 1977.
;
; MODIFICATION HISTORY:
;   Written (taken from MPFIT), CM, Feb 2002
;   Usage message, error checking, CM, 15 Mar 2002
;   Error checking is fixed, CM, 10 May 2002
;   Found error in return of permuted results, CM, 21 May 2004
;
;  $Id$
;
;-
; Copyright (C) 2002, 2004, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

function qrsolv, q, r, b, pivots=ipvt0

  if n_params() EQ 0 then begin
      USAGE:
      message, 'USAGE:', /info
      message, 'X = QRSOLV(Q, R, B, [PIVOTS=IPVT])', /info
      message, '  Q and R are factorization from QRFAC', /info
      message, '  B is right hand side of equation Q # X = B', /info
      return, 0
  endif

  sz = size(q)
  m = sz(1)
  n = sz(2)
  if sz(0) NE 2 OR m LT n then $
    goto, USAGE

  szr = size(r)
  if szr(0) NE 2 OR szr(1) NE szr(2) OR szr(1) NE n then $
    goto, USAGE

  if n_elements(b) NE m then $
    goto, USAGE

  delm = lindgen(n) * (n+1) ;; Diagonal elements of r

  ;; Default pivoting if not specified
  if n_elements(ipvt0) EQ 0 then ipvt = lindgen(n) $
  else ipvt = ipvt0

  ;; Multiply QT * B to get QTB.  Because Q is stored as reflectors,
  ;; they must be expanded explicitly.
  wa4 = b
  for j=0L, n-1 do begin
      lj = ipvt(j)
      temp3 = q(j,lj)
      if temp3 NE 0 then begin
          fj = q(j:*,lj)
          wj = wa4(j:*)
          ;; *** optimization wa4(j:*)
          wa4(j) = wj - fj * total(fj*wj) / temp3  
      endif
  endfor
  qtb = wa4(0:n-1)

  ;; Get some initial parameters
  diag = r(delm)
  x = qtb

  ;; Solve the triangular system for x.  If the system is singular
  ;; then obtain a least squares solution
  nsing = n
  wh = where(diag EQ 0, ct)
  if ct GT 0 then begin
      nsing = wh(0)
      x(nsing:*) = 0
  endif

  if nsing GE 1 then begin
      ;; Could use LUSOL here, but it turns out that the largest
      ;; expense for most reasonable problems is in computing QT*B,
      ;; rather than the solution here.  For huge problems where that
      ;; is not true, you are going to run into other problems.
      x(nsing-1) = x(nsing-1)/diag(nsing-1) ;; Degenerate case
      ;; *** Reverse loop ***
      for j=nsing-2,0,-1 do begin  
          sum = total(r(j+1:nsing-1,j)*x(j+1:nsing-1))
          x(j) = (x(j)-sum)/diag(j)
      endfor
  endif

  ;; Permute the components of x back to original
  xp = x & xp(ipvt) = x

  return, xp
end
