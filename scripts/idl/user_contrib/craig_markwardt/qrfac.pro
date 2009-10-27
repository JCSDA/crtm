;+
; NAME:
;   QRFAC
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Perform QR decomposition of a rectangular matrix
;
; MAJOR TOPICS:
;   Linear Systems
;
; CALLING SEQUENCE:
;   QRFAC, A, R, [ IPVT, /PIVOT, QMATRIX=qmatrix ]
;
; DESCRIPTION:
;
;  Given an MxN matrix A (M>N), the procedure QRFAC computes the QR
;  decomposition (factorization) of A.  This factorization is useful
;  in least squares applications solving the equation, A # x = B.
;  Together with the procedure QRSOLV, this equation can be solved in
;  a least squares sense.
;
;  The QR factorization produces two matrices, Q and R, such that
;
;     A = Q ## R
;
;  where Q is orthogonal such that TRANSPOSE(Q)##Q equals the identity
;  matrix, and R is upper triangular.  This procedure does not compute
;  Q directly, but returns the more-compact Householder reflectors,
;  which QRSOLV applies in constructing the solution.
;
;  Pivoting can be performed by setting the PIVOT keyword.  Rows with
;  the largest L2-norm are pivoted into the top positions of the
;  matrix.  The permutation matrix is returned in the IPVT parameter.
;
;
; PARAMETERS:
;
;   A - upon input, an MxN matrix ( =XARRAY(M,N) ) to be factored,
;       where M is greater than N.
;
;       Upon output, the upper triangular MxN matrix of Householder
;       reflectors used in reconstructing Q.  Obviously the original
;       matrix A is destroyed upon output.
;
;       Note that the dimensions of A in this routine are the
;       *TRANSPOSE* of the conventional appearance in the least
;       squares matrix equation.
;
;   R - upon ouptut, an upper triangular NxN matrix
;
;   IPVT - upon output, the permutation indices used in partial
;          pivoting.  If pivoting is used, this array should be passed
;          to the PIVOTS keyword of QRSOLV.  If the PIVOT keyword is
;          not set, then IPVT returns an unpermuted array of indices.
;
; KEYWORD PARAMETERS:
;
;   PIVOT - if set, then partial pivoting is performed, to bring the
;           rows with the largest norm to the top of the matrix.
;
;   QMATRIX - upon return, the fully explicit "Q" matrix is returned.
;             This matrix is optional since the Householder vectors
;             needed to solve QR problems, and to compute QMAT, are
;             also stored in A.  This square matrix can be used to
;             perform explicit matrix multiplication (although not
;             super efficiently).
;
;
; IMPLEMENTATION NOTE:
;
;   Upon return, A is in standard parameter order; A(*,IPVT) is in
;   permuted order.  RDIAG and QMATRIX are in permuted order upon
;   return.  QRSOLV accounts for these facts at the solution stage.
;
; EXAMPLE:
;
;  Decompose the 3x2 matrix [[9.,2.,6.],[4.,8.,7.]]
;    aa = [[9.,2.,6.],[4.,8.,7.]]
;    qrfac, aa, r, ipvt
;
;     IDL> print, aa
;          1.81818      0.181818      0.545455 
;         XXXXXXXXX      1.90160      0.432573 
;    (position marked with Xs is undefined)
;
;  Construct the matrix Q by expanding the Householder reflectors
;  returned in AA.  ( M = 3, N = 2 )  This same procedure is
;  accomplished by using the QMATRIX keyword.
;
;    ident = fltarr(m,m)  ;; Construct an identity matrix
;    ident(lindgen(m),lindgen(m)) = 1
;
;    q = ident
;    for i = 0, n-1 do begin
;     v = aa(*,i) & if i GT 0 then v(0:i-1) = 0  ;; extract reflector
;     q = q ## (ident - 2*(v # v)/total(v * v))  ;; generate matrix
;    endfor
;
;  Verify that Q ## R returns to the original AA
;
;     print, q(0:1,*) ## r
;         9.00000      4.00000
;         2.00000      8.00000
;         6.00000      7.00000
;     (transposed)
;
;  See example in QRSOLV to solve a least squares problem.
;   
;
; REFERENCES:
;
;   More', Jorge J., "The Levenberg-Marquardt Algorithm:
;     Implementation and Theory," in *Numerical Analysis*, ed. Watson,
;     G. A., Lecture Notes in Mathematics 630, Springer-Verlag, 1977.
;
; MODIFICATION HISTORY:
;   Written (taken from MPFIT), CM, Feb 2002
;   Added usage message, error checking, CM 15 Mar 2002
;   Corrected error in EXAMPLE, CM, 10 May 2002
;   Now returns Q matrix explicitly if requested, CM, 14 Jul 2002
;   Documented QMATRIX keyword, CM, 22 Jul 2002
;   Corrected errors in computations of R and Q matrices when
;     pivoting, CM, 21 May 2004
;   Small correction to documentation, CM, 05 Oct 2007
;   Documentation, CM, 17 Dec 2007
;
;  $Id$
;
;-
; Copyright (C) 2002, 2004, 2007, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

pro qrfac, a, r, ipvt, acnorm, pivot=pivot, qmatrix=qmat

  if n_params() EQ 0 OR n_elements(a) EQ 0 then begin
      USAGE:
      message, 'USAGE:', /info
      message, 'QRFAC, A, R, [IPVT, /PIVOT]', /info
      message, '    A is M x N matrix where (M>N)', /info
      return
  endif
  
  sz = size(a)
  tp = sz(sz(0)+1)
  m = sz(1)
  n = sz(2)

;  if m LT n then begin
;      message, 'ERROR: A must be an MxN matrix where M > N', /info
;      goto, USAGE
;  endif

  machvals = machar(double=(tp EQ 5))
  
  MACHEP0 = machvals.eps
  
  ;; Compute the initial column norms and initialize arrays
  acnorm = make_array(n, value=a(0)*0.)
  for j = 0L, n-1 do $
    acnorm(j) = sqrt(total(a(*,j)^2))
  rdiag = acnorm
  wa = rdiag
  ipvt = lindgen(n)

  ;; Reduce a to r with householder transformations
  minmn = min([m,n])
  for j = 0L, minmn-1 do begin
      if NOT keyword_set(pivot) then goto, HOUSE1
      
      ;; Bring the column of largest norm into the pivot position
      rmax = max(rdiag(j:*))
      kmax = where(rdiag(j:*) EQ rmax, ct) + j
      if ct LE 0 then goto, HOUSE1
      kmax = kmax(0)
      
      ;; Exchange rows via the pivot only.  Avoid actually exchanging
      ;; the rows, in case there is lots of memory transfer.  The
      ;; exchange occurs later, within the body of MPFIT, after the
      ;; extraneous columns of the matrix have been shed.
      if kmax NE j then begin
          temp     = ipvt(j)   & ipvt(j)    = ipvt(kmax) & ipvt(kmax)  = temp
          rdiag(kmax) = rdiag(j)
          wa(kmax)    = wa(j)
      endif
      
      HOUSE1:

      ;; Compute the householder transformation to reduce the jth
      ;; column of A to a multiple of the jth unit vector
      lj     = ipvt(j)
      ajj    = a(j:*,lj)
      ajnorm = sqrt(total(ajj^2))
      if ajnorm EQ 0 then goto, NEXT_ROW
      if a(j,lj) LT 0 then ajnorm = -ajnorm
      
      ajj     = ajj / ajnorm
      ajj(0)  = ajj(0) + 1
      ;; *** Note optimization a(j:*,j)
      a(j,lj) = ajj
      
      ;; Apply the transformation to the remaining columns
      ;; and update the norms

      ;; NOTE to SELF: tried to optimize this by removing the loop,
      ;; but it actually got slower.  Reverted to "for" loop to keep
      ;; it simple.
      if j+1 LT n then begin
          for k=j+1, n-1 do begin
              lk = ipvt(k)
              ajk = a(j:*,lk)
              ;; *** Note optimization a(j:*,lk) 
              ;; (corrected 20 Jul 2000)
              if a(j,lj) NE 0 then $
                a(j,lk) = ajk - ajj * total(ajk*ajj)/a(j,lj)

              if keyword_set(pivot) AND rdiag(k) NE 0 then begin
                  temp = a(j,lk)/rdiag(k)
                  rdiag(k) = rdiag(k) * sqrt((1.-temp^2) > 0)
                  temp = rdiag(k)/wa(k)
                  if 0.05D*temp*temp LE MACHEP0 then begin
                      rdiag(k) = sqrt(total(a(j+1:*,lk)^2))
                      wa(k) = rdiag(k)
                  endif
              endif
          endfor
      endif

      NEXT_ROW:
      rdiag(j) = -ajnorm
  endfor

  r = fltarr(minmn,minmn)+a(0)*0
  for j = 1, minmn-1 do r(j,0:j-1) = a(0:j-1,ipvt(j))
  idiag = lindgen(minmn)
  r(idiag, idiag) = rdiag
  
  ;; Construct matrix Q explicitly, if requested
  forward_function arg_present
  if arg_present(qmat) then begin

      ident = fltarr(m,m)  ;; Construct an identity matrix
      ident(lindgen(m),lindgen(m)) = 1
      qmat = ident

      for i = 0L, n-1 do begin
          v = a(*,ipvt(i)) ;; extract reflector
          if i GT 0 then v(0:i-1) = 0  
          qmat = qmat ## (ident - 2*(v # v)/total(v * v))  ;; generate matrix
      endfor
      
  endif

  return
end
