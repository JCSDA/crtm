;+
; NAME:
;       Integral
;
; PURPOSE:
;       Function to integrate tabulated data using Simpson's rule with 3-point
;       Lagrangian interpolation.
;
; CALLING SEQUENCE:
;       result = Integral( x, y [,/DOUBLE] )
;
; INPUTS:
;       x:  Vector of abscissa points. Elements must be unique. 
;
;       y:  Vector of corresponding ordinate points.
;
; INPUT KEYWORDS:
;       DOUBLE: Set this keyword to force the computation to be done
;               in double-precision.
;
; OUTPUTS:
;       None.
;
; OUTPUT KEYWORD PARAMETERS:
;       None.
;
; FUNCTION RESULT:
;       Function returns the integrated value.
;       If an error occurs, -1 is returned.
;
; INCLUDE FILES:
;       error_codes:   Include file containing error code definitions.
;
; RESTRICTIONS:
;       At least 6 data pooints must be passed. This simplifies the Lagrangian
;       interpolation. For less than 6 points, use INT_TABULATED.
;
; PROCEDURE:
;       Simpsons rule is used:
;                                      _
;         integral = Sum{ 1/6 * (y1 + 4y + y2 ) * (x2 - x1 ) }
;
;           _                          _    x1 + x2
;       and y is the ordinate value of x = --------- 
;                                              2
;
;       and x1 and x2 are successive abscissa points in the input array.
;
; EXAMPLE:
;       Define 11 x-values on the closed interval [0.0 , 0.8].
;
;         IDL> x = [ 0.0, .12, .22, .32, .36, .40, .44, .54, .64, .70, .80 ]
;
;       Define 11 f-values corresponding to x(i).
;
;         IDL> f = [ 0.200000, 1.30973, 1.30524, 1.74339, 2.07490, 2.45600, $
;         IDL>       2.84299,  3.50730, 3.18194, 2.36302, 0.231964 ]
;
;       Compute the integral:
;
;         IDL> result = integral( x, f )
;         IDL> HELP, result
;         RESULT          DOUBLE    =        1.6274544
;
;       In this example, the f-values are generated from a known function,
;       (f = .2 + 25*x - 200*x^2 + 675*x^3 - 900*x^4 + 400*x^5)
;
;       The Multiple Application Trapezoid Method yields;  result = 1.5648
;       The Multiple Application Simpson's Method yields;  result = 1.6036
;              IDL User Library INT_TABULATED.PRO yields;  result = 1.6232
;                                    INTEGRAL.PRO yields;  result = 1.6274
;         The Exact Solution (4 decimal accuracy) yields;  result = 1.6405
;
; CREATION HISTORY:
;       Written by:     Liam Gumley, CIMSS/SSEC, 22-Dec-1995
;                       Based on a FORTRAN-77 version by Paul van Delst, CIMSS/SSEC
;
;                       Paul van Delst, CIMSS/SSEC, 04-Jun-1999
;                       - Placed under CVS/RCS control.
;                       - Updated header documentation
;                       - Prettified source.
;
;  Copyright (C) 1995, 1999, 2001 Paul van Delst, Liam Gumley CIMSS/SSEC/UW-Madison
;
;  This program is free software; you can redistribute it and/or
;  modify it under the terms of the GNU General Public License
;  as published by the Free Software Foundation; either version 2
;  of the License, or (at your option) any later version.
;
;  This program is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with this program; if not, write to the Free Software
;  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;-

PRO Get_Integration_Coefficients, $
  n,  $  ; Input
  c0, $  ; Output
  c      ; Output
  
  COMPILE_OPT HIDDEN
  
  CASE n OF
    ; Trapezoidal rule
    2: BEGIN
      c0 = 1.0d0/2.0d0
      c = [1.0d0, 1.0d0]
    END
    ; Simpson's 3/8 rule
    4: BEGIN
      c0 = 3.0d0/8.0d0
      c = [1.0d0, 3.0d0, 3.0d0, 1.0d0]
    END
    ; Boole's rule
    5: BEGIN
      c0 = 2.0d0/45.0d0
      c = [7.0d0, 32.0d0, 12.0d0, 32.0d0, 7.0d0]
    END
    ; And higher orders...
    6: BEGIN
      c0 = 5.0d0/288.0d0
      c = [19.0d0, 75.0d0, 50.0d0, 50.0d0, 75.0d0, 19.0d0]
    END
    7: BEGIN
      c0 = 1.0d0/140.0d0
      c = [41.0d0, 216.0d0, 27.0d0, 272.0d0, 27.0d0, 216.0d0, 41.0d0]
    END
    ; Default is good ol' Simpson's rule
    ELSE: BEGIN
      c0 = 1.0d0/3.0d0
      c = [1.0d0, 4.0d0, 1.0d0]
    END
  ENDCASE
  
END
      

FUNCTION New2_Integral, $
  x, $  ; Input
  y, $  ; Input
  INT_TYPE = Int_Type, $ ; Input
  N_POINTS = n_Points, $ ; Input
  DOUBLE   = Double, $   ; Input
  X_INT    = x_Int, $    ; Output
  Y_INT    = y_Int       ; Output


  ; Setup
  ; ...Error handler
  @error_codes
;  CATCH, error_status
;  IF ( error_status NE 0 ) THEN BEGIN
;    CATCH, /CANCEL
;    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
;    RETURN, FAILURE
;  ENDIF
  ; ...Check n_Points keywords
  _n_Points = N_ELEMENTS(n_Points) GT 0 ? LONG(n_Points[0]) : 0L
  ; ...Set local double keyword based on input data
  _Double = KEYWORD_SET(Double)
  x_Info = SIZE(x,/STRUCTURE)
  y_Info = SIZE(y,/STRUCTURE)
  IF ( x_Info.TYPE_NAME EQ 'DOUBLE' AND y_Info.TYPE_NAME EQ 'DOUBLE' ) THEN _Double = 1
  ; ...Parameters
  TOLERANCE = (MACHAR(DOUBLE=_Double)).EPS
  ZERO      = KEYWORD_SET(_Double) ? 0.0d0 : 0.0
  POINT5    = KEYWORD_SET(_Double) ? 0.5d0 : 0.5
  ONE       = KEYWORD_SET(_Double) ? 1.0d0 : 1.0
  THREE     = KEYWORD_SET(_Double) ? 3.0d0 : 3.0
  FOUR      = KEYWORD_SET(_Double) ? 4.0d0 : 4.0
  SIX       = KEYWORD_SET(_Double) ? 6.0d0 : 6.0


  ; Check input
  IF ( N_PARAMS() LT 2 ) THEN  $
    MESSAGE, 'Invalid number of arguments.', /NONAME, /NOPRINT
  ; ...Check that arguments are defined
  n = N_ELEMENTS(x)
  IF ( n EQ 0 ) THEN $
    MESSAGE, 'Input X argument not defined!', /NONAME, /NOPRINT
  IF ( N_ELEMENTS(y) EQ 0 ) THEN $
    MESSAGE, 'Input Y argument not defined!', /NONAME, /NOPRINT
  ; ...Check the number of array elements
  IF ( N_ELEMENTS(y) NE n ) THEN $
    MESSAGE, 'X and Y input vector must be same size.', /NONAME, /NOPRINT
  IF ( n LE 6 ) THEN $
    MESSAGE, 'Not enough data points! Routine uses 3-point Lagrangian ' + $
             'interpolation so N > 6.', /NONAME, /NOPRINT


  ; Copy input at required precision
  _x = KEYWORD_SET(_Double) ? DOUBLE(x) : x
  _y = KEYWORD_SET(_Double) ? DOUBLE(y) : y
  
  
  ; Prepare arrays for integration
  idx = UNIQ(_x, SORT(_x))
  _x = _x[idx]
  _y = _y[idx]
  ; ...Let user know if non-unique values found
  IF ( N_ELEMENTS(_x) NE n ) THEN $
    MESSAGE, 'Non-unique X values found and removed.', /INFORMATIONAL
  ; ...Double-check
  n = N_ELEMENTS(_x)
  x0 = _x(0:n-2)
  x1 = _x(1:n-1)
  idx = WHERE(ABS(x1-x0) LT TOLERANCE, Count)
  IF ( Count GE 1 ) THEN $
    MESSAGE, 'Not all non-unique X values removed!', /NONAME, /NOPRINT

  ; Get the integration coefficients and set the number of points
  Get_Integration_Coefficients, _n_Points, c0, c
  _n_Points = N_ELEMENTS(c)


  ; Perform the interpolation
  ; ...Determine the number of points to interpolate
  n_Even = n
  WHILE (((n_Even-1L) MOD (_n_Points-1L)) NE 0) DO n_Even++
  ; ...Construct an evenly spaced abscissa grid
  x_Int = DINDGEN(n_Even)/DOUBLE(n_Even-1)
  x_Int = x_Int*(_x[n-1] - _x[0]) + _x[0]
  ; ...Interpolate data to regular grid
  yd2 = SPL_INIT(_x, _y, DOUBLE=_Double)

  CASE Int_Type OF
    1: y_Int = INTERPOL(_y, _x, x_Int, /QUADRATIC)
    2: y_Int = INTERPOL(_y, _x, x_Int)
    3: y_Int = SPLINE(_x, _y, x_Int, 10.0d0, DOUBLE=_Double)
    4: y_Int = AvgQuad_Interpolate(_x, _y, x_Int, DOUBLE=_Double)
    ELSE: y_Int = SPL_INTERP(_x, _y, yd2, x_Int)
  ENDCASE

  
  ; Perform the integration
  ; ...Set the uniform interval
  h = (x_Int[n_Even-1] - x_Int[0])/DOUBLE(n_Even-1)
  ; ...Construct the index array to use for integration
  iidx = (LINDGEN((n_Even - 1L)/(_n_Points-1L)) + 1L) * (_n_Points-1L)
  ; ...Integrate
  Sum = ZERO
  FOR i = 0L, N_ELEMENTS(iidx)-1L DO BEGIN
    idx = LINDGEN(_n_Points) + iidx[i] - (_n_Points-1L)
    Sum = Sum + (c0 * h * (TRANSPOSE(c)#y_Int[idx])) 
  ENDFOR

  ; Done
  CATCH, /CANCEL
  RETURN, Sum

END

