; $Id$
;
; Copyright (c) 1995-2007, ITT Visual Information Solutions. All
;       rights reserved. Unauthorized reproduction is prohibited.
;+
; NAME:
;       INT_TABULATED
;
; PURPOSE:
;       This function integrates a tabulated set of data { x(i) , f(i) },
;       on the closed interval [min(X) , max(X)].
;
; CATEGORY:
;       Numerical Analysis.
;
; CALLING SEQUENCE:
;       Result = INT_TABULATED(X, F)
;
; INPUTS:
;       X:  The tabulated X-value data. This data may be irregularly
;           gridded and in random order. If the data is randomly ordered
;	    you must set the SORT keyword to a nonzero value.
;           Duplicate x values will result in a warning message.
;       F:  The tabulated F-value data. Upon input to the function
;           X(i) and F(i) must have corresponding indices for all
;	    values of i. If X is reordered, F is also reordered.
;
;       X and F must be of floating point or double precision type.
;
; KEYWORD PARAMETERS:
;       SORT:   A zero or non-zero scalar value.
;               SORT = 0 (the default) The tabulated x-value data is
;                        already in ascending order.
;               SORT = 1 The tabulated x-value data is in random order
;                        and requires sorting into ascending order. Both
;			 input parameters X and F are returned sorted.
;       DOUBLE: If set to a non-zero value, computations are done in
;               double precision arithmetic.
;
; OUTPUTS:
;       This fuction returns the integral of F computed from the tabulated
;	data in the closed interval [min(X) , max(X)].
;
; RESTRICTIONS:
;       Data that is highly oscillatory requires a sufficient number
;       of samples for an accurate integral approximation.
;
; PROCEDURE:
;       INT_TABULATED.PRO constructs a regularly gridded x-axis with a
;	number of segments as an integer multiple of four. Segments
;	are processed in groups of four using a 5-point Newton-Cotes
;	integration formula.
;       For 'sufficiently sampled' data, this algorithm is highly accurate.
;
; EXAMPLES:
;       Example 1:
;       Define 11 x-values on the closed interval [0.0 , 0.8].
;         x = [0.0, .12, .22, .32, .36, .40, .44, .54, .64, .70, .80]
;
;       Define 11 f-values corresponding to x(i).
;         f = [0.200000, 1.30973, 1.30524, 1.74339, 2.07490, 2.45600, $
;              2.84299,  3.50730, 3.18194, 2.36302, 0.231964]
;
;       Compute the integral.
;         result = INT_TABULATED(x, f)
;
;       In this example, the f-values are generated from a known function,
;       (f = .2 + 25*x - 200*x^2 + 675*x^3 - 900*x^4 + 400*x^5)
;
;       The Multiple Application Trapazoid Method yields;  result = 1.5648
;       The Multiple Application Simpson's Method yields;  result = 1.6036
;				INT_TABULATED.PRO yields;  result = 1.6232
;         The Exact Solution (4 decimal accuracy) yields;  result = 1.6405
;
;	Example 2: 
;       Create 30 random points in the closed interval [-2 , 1].
;         x = randomu(seed, 30) * 3.0 - 2.0
;
;       Explicitly define the interval's endpoints.
;         x(0) = -2.0  &  x(29) = 1.0
;
;       Generate f(i) corresponding to x(i) from a given function.
;         f = sin(2*x) * exp(cos(2*x))
;
;       Call INT_TABULATED with the SORT keyword.
;         result = INT_TABULATED(x, f, /sort)
;
;       In this example, the f-values are generated from the function,
;       f = sin(2*x) * exp(cos(2*x))
;
;       The result of this example will vary because the x(i) are random.
;       Executing this example three times gave the following results:
;		               INT_TABULATED.PRO yields;  result = -0.0702
;		               INT_TABULATED.PRO yields;  result = -0.0731
;		               INT_TABULATED.PRO yields;  result = -0.0698
;        The Exact Solution (4 decimal accuracy) yields;  result = -0.0697
;
; MODIFICATION HISTORY:
;           Written by:  GGS, RSI, September 1993
;           Modified:    GGS, RSI, November  1993
;                        Use Numerical Recipes cubic spline interpolation 
;                        function NR_SPLINE/NR_SPLINT. Execution time is 
;                        greatly reduced. Added DOUBLE keyword. The 'sigma' 
;                        keyword is no longer supported.
;           Modified:    GGS, RSI, April  1995
;                        Changed cubic spline calls from NR_SPLINE/NR_SPLINT
;                        to SPL_INIT/SPL_INTERP. Improved double-precision
;                        accuracy.
;           Modified:    GGS, RSI, April 1996
;                        Replaced WHILE loop with vector operations. 
;                        Check for duplicate points in x vector.  
;                        Modified keyword checking and use of double precision.
;-

FUNCTION My_Int_Tabulated, X, F, Double = Double, Sort = Sort, $
  X_INT    = x_Int, $    ; Output
  Y_INT    = y_Int       ; Output


  ;Return to caller if an error occurs.
  ON_ERROR, 2 

  TypeX = SIZE(X)
  TypeF = SIZE(F)

  ;Check F data type.
  if TypeF[TypeF[0]+1] ne 4 and TypeF[TypeF[0]+1] ne 5 then $
    MESSAGE, "F values must be float or double."

  ;Check length.
  if TypeX[TypeX[0]+2] ne TypeF[TypeF[0]+2] then $
    MESSAGE, "X and F arrays must have the same number of elements."

  ;Check duplicate values.
  if TypeX[TypeX[0]+2] ne N_ELEMENTS(UNIQ(X[SORT(X)])) then $
    MESSAGE, "X array contains duplicate points."

  ;If the DOUBLE keyword is not set then the internal precision and
  ;result are identical to the type of input.
  if N_ELEMENTS(Double) eq 0 then $
    Double = (TypeX[TypeX[0]+1] eq 5 or TypeF[TypeF[0]+1] eq 5) 

  Xsegments = TypeX[TypeX[0]+2] - 1L

  ;Sort vectors into ascending order.
  if KEYWORD_SET(Sort) ne 0 then begin
    ii = SORT(x)
    X = X[ii]
    F = F[ii]
  endif

  while (Xsegments MOD 4L) ne 0L do $
    Xsegments = Xsegments + 1L

  Xmin = MIN(X)
  Xmax = MAX(X)

  ;Uniform step size.
    h = (Xmax+0.0 - Xmin) / Xsegments
  ;Compute the interpolates at Xgrid.
    ;x values of interpolates >> Xgrid = h * FINDGEN(Xsegments + 1L) + Xmin
    z = SPL_INTERP(X, F, SPL_INIT(X, F, Double = Double), $
                   h * FINDGEN(Xsegments + 1L) + Xmin, Double = Double)
    x_int = h * FINDGEN(Xsegments + 1L) + Xmin
    y_int = z
  ;Compute the integral using the 5-point Newton-Cotes formula.
    ii = (LINDGEN((N_ELEMENTS(z) - 1L)/4L)+1) * 4
    if Double eq 0 then $
      RETURN, FLOAT(TOTAL(2.0 * h * (7.0 * (z[ii-4] + z[ii]) + $
                    32.0 * (z[ii-3] + z[ii-1]) + 12.0 * z[ii-2]) / 45.0)) $
    else $
      RETURN, TOTAL(2D * h * (7D * (z[ii-4] + z[ii]) + $
                    32D * (z[ii-3] + z[ii-1]) + 12D * z[ii-2]) / 45D, /DOUBLE)

END

