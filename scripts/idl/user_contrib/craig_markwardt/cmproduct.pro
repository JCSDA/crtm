;+
; NAME:
;   CMPRODUCT
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   CMPRODUCT() is the multiplicative equivalent of TOTAL().
;
; CALLING SEQUENCE:
;   Result = CMPRODUCT(ARRAY)
;
; DESCRIPTION: 
;
;   Calculates the product of all the elements of an array.  Vector
;   multiplication in groups of powers of two make this operation
;   faster than a simple FOR loop.  The number of actual
;   multiplications is still N_ELEMENTS(ARRAY).  Double precision
;   should be used for the highest accuracy when multiplying many
;   numbers.
;
; INPUTS:
;
;   ARRAY - Array of elements to multiply together.  For instance,
;           ARRAY could contain the dimensions of another array--then
;           CMPRODUCT(ARRAY) would be the total number of elements of
;           that other array.
;
; RETURNS:
;  The result of the function is the total product of all the elements
;  of ARRAY.
;
; EXAMPLE:
;
; SEE ALSO:
;
;   TOTAL, PRODUCT (from Astronomy User's Library)
;
; MODIFICATION HISTORY:
;   Written, CM, 28 Mar 2000
;     (based on outline of PRODUCT by William Thompson)
;
;  $Id$
;
;-
; Copyright (C) 2000, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

FUNCTION CMPRODUCT, ARRAY
  ON_ERROR,2
;
;  Check the number of parameters.
;
  IF N_PARAMS() NE 1 THEN MESSAGE,'Syntax:  Result = PRODUCT(ARRAY)'
;
;  Check the type of ARRAY.
;
  SZ = SIZE(ARRAY)
  TYPE = SZ(SZ(0)+1)
  IF TYPE EQ 0 THEN MESSAGE,'ARRAY not defined'
  IF TYPE EQ 7 THEN MESSAGE,'Operation illegal with string arrays'
  IF TYPE EQ 8 THEN MESSAGE,'Operation illegal with structures'
;
;  Calculate the product.
;
  X = ARRAY
  N = N_ELEMENTS(X)
  WHILE N GT 1 DO BEGIN
      IF (N MOD 2) EQ 1 THEN X(0) = X(0) * X(N-1)
      N2 = FLOOR(N/2)
      X = X(0:N2-1) * X(N2:*)
      N = N2
  ENDWHILE
;
  RETURN,X(0)
END
