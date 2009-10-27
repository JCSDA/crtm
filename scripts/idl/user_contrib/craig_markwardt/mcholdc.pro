;+
; NAME:
;   MCHOLDC
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Modified Cholesky Factorization of a Symmetric Matrix
;
; MAJOR TOPICS:
;   Linear Systems
;
; CALLING SEQUENCE:
;   MCHOLDC, A, D, E [, /OUTFULL, /SPARSE, /PIVOT, TAU=TAU, $
;                      PERMUTE=PERMUTE, INVPERMUTE=INVPERMUTE ]
;
; DESCRIPTION:
;
;  Given a symmetric matrix A, the MCHOLDC procedure computes the
;  factorization:
;
;     A + E   =   TRANSPOSE(U) ## D ## U
;
;  where A is the original matrix (optionally permuted if the PIVOT
;  keyword is set), U is an upper triangular matrix, D is a diagonal
;  matrix, and E is a diagonal error matrix.
;
;  The standard Cholesky factorization is only defined for a positive
;  definite symmetric matrix.  If the input matrix is positive
;  definite then the error term E will be zero upon output.  The user
;  may in fact test the positive-definiteness of their matrix by
;  factoring it and testing that all terms in E are zero.
;
;  If A is *not* positive definite, then the standard Cholesky
;  factorization is undefined.  In that case we adopt the "modified"
;  factorization strategy of Gill, Murray and Wright (p. 108), which
;  involves adding a diagonal error term to A in order to enforce
;  positive-definiteness.  The approach is optimal in the sense that
;  it attempts to minimize E, and thus disturbing A as little as
;  possible.  For optimization problems, this approximate
;  factorization can be used to find a direction of descent even when
;  the curvature is not positive definite.
;
;  The upper triangle of A is modified in place.  By default, the
;  lower triangle is left unchanged, and the matrices D and E are
;  actually returned as vectors containing only the diagonal terms.
;  However, if the keyword OUTFULL is set then full matrices are
;  returned.  This is useful when matrix multiplication will be
;  performed at the next step.
;
;  The modified Cholesky factorization is most stable when pivoting is
;  performed.  If the keyword PIVOT is set, then pivoting is performed
;  to place the diagonal terms with the largest amplitude in the next
;  row.  The permutation vectors returned in PERMUTE and INVPERMUTE
;  can be used to apply and reverse the pivoting.
;    [ i.e.,  (U(PP,*))(*,PP) applies the permutation and
;             (U(IPP,*))(*,IPP) reverses it, where PP and IPP are the
;             permutation and inverse permutation vectors. ]
;
;  If the matrix to be factored is very sparse, then setting the
;  SPARSE keyword may improve the speed of the computations.  SPARSE
;  is more costly on a dense matrix, but only grows as N^2, where as
;  the standard computation grows as N^3, where N is the rank of the
;  matrix.
;
;  If the CHOLSOL keyword is set, then the output is slightly
;  modified.  The returned matrix A that is returned is structured so
;  that it is compatible with the CHOLSOL built-in IDL routine.  This
;  involves converting A to being upper to lower triangular, and
;  multiplying by SQRT(D).  Users must be sure to check that all
;  elements of E are zero before using CHOLSOL.
;
; PARAMETERS:
;
;   A - upon input, a symmetric NxN matrix to be factored.
;       Upon output, the upper triangle of the matrix is modified to
;       contain the factorization.
;
;   D - upon output, the diagonal matrix D.
;
;   E - upon output, the diagonal error matrix E.
;
; KEYWORD PARAMETERS:
;
;   OUTFULL - if set, then A, D and E will be modified to be full IDL
;             matrices than can be matrix-multiplied.  By default,
;             only the upper triangle of A is modified, and D and E
;             are returned as vectors.
;
;   PIVOT - if set, then diagonal elements of A are pivoted into place
;           and operated on, in decrease order of their amplitude.
;           The permutation vectors are returned in the PERMUTE and
;           INVPERMUTE keywords.
;
;   PERMUTE - upon return, the permutation vector which converts a
;             vector into permuted form.
;
;   INVPERMUTE - upon return, the inverse permutation vector which
;                converts a vector from permuted form back into
;                standard form.
;
;   SPARSE - if set, then operations optimized for sparse matrices are
;            employed.  For large but very sparse matrices, this can
;            save a significant amount of computation time.
;
;   CHOLSOL - if set, then A and D are returned, suitable for input to
;             the built-in IDL routine CHOLSOL.  CHOLSOL is mutually
;             exclusive with the FULL keyword.
;
;   TAU - if set, then use the Tau factor as described in the
;         "unconventional" modified Cholesky factorization, as
;         described by Xie & Schlick.
;         Default: the unconventional technique is not used.
;
; EXAMPLE:
;
;   Example 1
;   ---------
;   a = randomn(seed, 5,5)    ;; Generate a random matrix
;   a = 0.5*(transpose(a)+a)  ;; Symmetrize it
;
;   a1 = a                    ;; Make a copy
;   mcholdc, a1, d, e, /full  ;; Factorize it
;   print, max(abs(e))        ;; This matrix is not positive definite
;
;   diff = transpose(a1) ## d ## a1 - e - a
;                             ;; Test the factorization by inverting
;                             ;; it and subtracting A
;   print, max(abs(diff))     ;; Differences are small
;
;   Example 2
;   ---------
;   Solving a problem with MCHOLDC and CHOLSOL
;
;   a = [[6E,15,55],[15E,55,225],[55E,225,979]]
;   b = [9.5E,50,237]
;
;   mcholdc, a, d, e, /cholsol  ;; Factorize matrix, compatible w/ CHOLSOL
;   if total(abs(e)) NE 0 then $
;      message, 'ERROR: Matrix A is not positive definite'
;
;   x = cholsol(a, d, b)        ;; Solve with CHOLSOL
;   print, x
;        -0.500001    -0.999999     0.500000
;   which is within 1e-6 of the true solution.
;
;
; REFERENCES:
;
;   Gill, P. E., Murray, W., & Wright, M. H. 1981
;     *Practical Optimization*, Academic Press
;   Schlick, T. & Fogelson, A., "TNPACK - A Truncated Newton
;     Minimization Package for Large- Scale Problems: I. Algorithm and
;     Usage," 1992, ACM TOMS, v. 18, p. 46-70.  (Alg. 702)
;   Xie, D. & Schlick, T., "Remark on Algorithm 702 - The Updated
;     Truncated Newton Minimization Package," 1999, ACM TOMS, v. 25,
;     p. 108-122.
;
; MODIFICATION HISTORY:
;   Written, CM, Apr 2001
;   Added CHOLSOL keyword, CM, 15 Feb 2002
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
pro mcholdc, a, d, e, outfull=full, sparse=sparse, pivot=pivot, $
             permute=pp, invpermute=ipp, tau=tau0, cholsol=cholsol

  if n_params() EQ 0 then begin
      message, 'USAGE: MCHOLDC, A, D, E [, /OUTFULL, /SPARSE, /PIVOT, '+$
        '/CHOLSOL, TAU=, PERMUTE=, INVPERMUTE= ]', /info
      return
  endif

  ;; Test for proper dimensions
  sz = size(a)
  if sz(0) EQ 0 then begin
      d = a
      a = a(0)*0 + 1
      e = a(0)*0
      return
  endif
  if sz(0) NE 2 then $
    message, 'ERROR: Matrix A must be two dimensional'
  if sz(1) NE sz(2) then $
    message, 'ERROR: Matrix A must be square'
  if n_elements(tau0) GT 0 then tau = tau0(0)

  n = sz(1)

  zero = a(0)*0
  one  = zero + 1
  eps = zero + 1e-6

  ;; Gamma and Xi are the max diagonal and off-diagonal components
  diag = lindgen(n)*n + lindgen(n)
  gamma = max(abs(a(diag)))

  xi = zero
  for i = 0L, n-2 do begin
      xi = max( [xi, max(abs(a(i+1:*,i)))] )
  endfor

  eps1 = max([gamma,xi]) * eps
  del = max([eps, eps1])
  
  ;; Compute the bound on the diagonal elements
  bound = max([gamma, xi/sqrt(n^2-1), eps])

  ;; Compute output arrays
  d = replicate(zero, n)
  e = d
  pp = lindgen(n)

  for j = 0, n-1 do begin
      
      ;; Pivoting - search for max element of abs(diag(A))
      if keyword_set(pivot) then begin
          maxa = max(abs(a(diag(j:*))), jmax)
          if jmax GT 0 then begin
              jmax = jmax + j
              nn = lindgen(n)
              j1 = (nn*n+j   ) < (nn+j*n)
              j2 = (nn*n+jmax) < (nn+jmax*n)
              temp = j1(j)  & j1(j) = j1(jmax) & j1(jmax) = temp

              ;; Exchange the row/columns
              temp  = a(j1) & a(j1) = a(j2)    & a(j2)    = temp

              ;; Change the permutation vector
              temp  = pp(j) & pp(j) = pp(jmax) & pp(jmax) = temp
          endif
      endif

      ;; Compute update to the L matrix, in place
      if j GT 0 AND j LT n-1 then begin
          
          ;; The sparse path computes the same thing, but is faster if
          ;; very few components of the matrix are non-zero.
          if keyword_set(sparse) then begin
              ajk = reform(a(j,0:j-1)*d(0:j-1), /overwrite)
              wh = where(ajk NE 0, nk)
              if nk GT 0 then $
                a(j+1:*,j) = a(j+1:*,j) - a(j+1:*,wh) # ajk(wh)
          endif else begin
              a(j+1:*,j) = a(j+1:*,j) - $
                a(j+1:*,0:j-1) # reform(a(j,0:j-1)*d(0:j-1), /overwrite)
          endelse
      endif

      ;; Compute correction to diagonal element
      if j LT n-1 then begin
          thj2 = max(a(j:*,j)^2)
      endif

      ;; Compute the unusual modified factorization of Xie and
      ;; Schlick, or else default to the standard Gill, Murray & Wright
      if n_elements(tau) GT 0 then begin
          ww = a(j,j) + tau(0)
          if      ww GT del      then d(j) = max([ww, thj2/bound]) $
          else if abs(ww) LE del then d(j) = del $
          else                        d(j) = min([ww,-thj2/bound])
      endif else begin
          d(j) = max([del, abs(a(j,j)), thj2/bound])
      endelse
      e(j) = d(j) - a(j,j)

      ;; Apply corrections
      if j LT n-1 then begin
          i = lindgen(n-1-j)+j+1
          a(i,i) = a(i,i) - a(i,j)^2/d(j)
          a(j+1:*,j) = a(j+1:*,j) / d(j)
      endif

  endfor

  ;; Invert the permutation vector
  ipp = sort(pp)

  ;; Expand to a full matrix if requested
  if keyword_set(cholsol) then begin
      d = sqrt(d)
      for j = 0, n-2 do a(j+1:*,j) = a(j+1:*,j)*d(j)
      a = transpose(temporary(a))
  endif else if keyword_set(full) then begin
      for j = 0, n-1 do a(0:j,j) = 0
      a(diag) = 1
      dd = a*0
      ee = dd
      dd(diag) = d
      ee(diag) = e
      
      d = dd
      e = ee
  endif

end

