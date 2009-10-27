;+
; NAME:
;   CROSSPN
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   A version of CROSSP for efficient vector cross products
;
; CALLING SEQUENCE:
;   C = CROSSPN(A, B)
;
; DESCRIPTION: 
;
;   The function CROSSPN computes the vector cross product (outer
;   product).  The difference between CROSSPN and the IDL library
;   function CROSSP, is that CROSSPN allows more than one cross
;   product to be computed at one time (i.e., it is vectorized).
;
;   Thus, in the expression "C = CROSSPN(A, B)" the vector cross
;   product is computed as C = A x B.  Because CROSSPN is vectorized,
;   any of the following combinations are valid:
;
;       * A is a 3-vector, B is a 3-vector
;            ==> C is the vector cross product C = A x B
;
;       * A is a 3xN array, B is a 3-vector
;            ==> C(*,I) = A(*,I) x B    (each A is crossed with B)
;
;       * A is a 3-vector, B is a 3xN array
;            ==> C(*,I) = A x B(*,I)  (A is crossed with each B)
;
;       * A is a 3xN array, B is a 3xN array
;            ==> C(*,I) = A(*,I) x B(*,I)   (component-by-component)
;
;   If both A and B are arrays then they must have the same
;   dimensions.
;
; INPUTS:
;
;   A - a 3-vector or 3xN array.
;
;   B - a 3-vector or 3xN array.
;
;
; RETURNS:
;
;   The vector cross product A x B, either a 3-vector or a 3xN array
;   depending on A and B.
;
; SEE ALSO:
;
;   CROSSP
;
; MODIFICATION HISTORY:
;   Written, CM, 10 Mar 2002
;   Documented, CM, 22 Mar 2002
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

function crosspn, x1, x2

  n1 = n_elements(x1)/3
  n2 = n_elements(x2)/3

  if n1 EQ 1 AND n2 EQ 1 then begin
      return, crossp(x1,x2)
  endif else if n1 GT 1 AND n2 EQ 1 then begin
      xr = make_array(3,n1,value=0*x1(0)*x2(0))
      xr(0,*) = x1(1,*)*x2(2) - x2(1)*x1(2,*)
      xr(1,*) = x1(2,*)*x2(0) - x2(2)*x1(0,*)
      xr(2,*) = x1(0,*)*x2(1) - x2(0)*x1(1,*)
      return, xr
  endif else if n1 EQ 1 AND n2 GT 1 then begin
      xr = make_array(3,n2,value=0*x1(0)*x2(0))
      xr(0,*) = x1(1)*x2(2,*) - x2(1,*)*x1(2)
      xr(1,*) = x1(2)*x2(0,*) - x2(2,*)*x1(0)
      xr(2,*) = x1(0)*x2(1,*) - x2(0,*)*x1(1)
      return, xr
  endif else if n1 EQ n2 then begin
      xr = make_array(3,n1,value=0*x1(0)*x2(0))
      xr(0,*) = x1(1,*)*x2(2,*) - x2(1,*)*x1(2,*)
      xr(1,*) = x1(2,*)*x2(0,*) - x2(2,*)*x1(0,*)
      xr(2,*) = x1(0,*)*x2(1,*) - x2(0,*)*x1(1,*)
      return, xr
  endif else begin
      message, 'ERROR: number of vectors must match'
  endelse

  return, 0
end

      
