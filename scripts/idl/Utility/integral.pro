;+
; NAME:
;       integral
;
; PURPOSE:
;       Function to integrate tabulated data using Simpson's rule with 3-point
;       Lagrangian interpolation.
;
; CATEGORY:
;       General
;
; LANGUAGE:
;       IDL v5.4
;
; CALLING SEQUENCE:
;       result = integral( x, y )
;
; INPUTS:
;       x:  Vector of abscissa points. Elements must be unique and monotonically
;           increasing.
;
;       y:  Vector of corresponding ordinate points.
;
; INPUT KEYWORD PARAMETERS:
;       None.
;
; OUTPUTS:
;       None.
;
; OUTPUT KEYWORD PARAMETERS:
;       None.
;
; FUNCTION RESULT:
;       Function returns the integrated value in double precision. If an
;       error occurs, -1 is returned.
;
; CALLS:
;       None.
;
; COMMON BLOCKS:
;       None.
;
; INCLUDE FILES:
;       error_codes:   Include file containing error code definitions.
;
; SIDE EFFECTS:
;       None.
;
; RESTRICTIONS:
;       At least 6 data pooints must be passed. This simplifies the Lagrangian
;       interpolation. Fro less than 6 points, use INT_TABULATED.
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

FUNCTION integral, x, y, $
                   sum_array = sum_array



  ;------------------------------------------------------------------------------
  ;                         -- Set up error handler --
  ;------------------------------------------------------------------------------

  @error_codes

  CATCH, error_status

  IF ( error_status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FAILURE
  ENDIF    



  ;------------------------------------------------------------------------------
  ;                   -- Set floating point precision --
  ;------------------------------------------------------------------------------

  tolerance = ( MACHAR( /DOUBLE ) ).EPS



  ;------------------------------------------------------------------------------
  ;                            -- Check input --
  ;------------------------------------------------------------------------------

  n_arguments = 2
  IF ( N_PARAMS() LT n_arguments ) THEN  $
    MESSAGE, 'Invalid number of arguments.', $
             /NONAME, /NOPRINT


  ; -----------------------------------------
  ; Check that required arguments are defined
  ; -----------------------------------------

  n = N_ELEMENTS( x )
  IF ( n EQ 0 ) THEN $
    MESSAGE, 'Input X argument not defined!', $
             /NONAME, /NOPRINT


  IF ( N_ELEMENTS( y ) EQ 0 ) THEN $
    MESSAGE, 'Input Y argument not defined!', $
             /NONAME, /NOPRINT


  ; ----------------------------------
  ; Check the number of array elements
  ; ----------------------------------

  IF ( N_ELEMENTS( y ) NE n ) THEN $
    MESSAGE, 'X and Y input vector must be same size.', $
             /NONAME, /NOPRINT

  IF ( n LE 6 ) THEN $
    MESSAGE, 'Not enough data points! Routine uses 3-point Largrangian ' + $
             'interpolation so N > 6. Use INT_TABULATED instead.', $
             /NONAME, /NOPRINT


  ; -----------------------------------------------------------------
  ; check that all points in X are unique, so we don't divide by zero
  ; -----------------------------------------------------------------

  x0 = DOUBLE( x( 0 : n - 2 ) )
  x1 = DOUBLE( x( 1 : n - 1 ) )

  index = WHERE( ABS( x1 - x0 ) LT tolerance, count )

  IF ( count GE 1 ) THEN $
    MESSAGE, 'Non-unique X values found.', $
             /NONAME, /NOPRINT



  ;------------------------------------------------------------------------------
  ;                         -- Perform integration --
  ;------------------------------------------------------------------------------

  ; -----------------------------------------
  ; Create interpolation point arrays to find 
  ; "panel" midpoints.
  ; -----------------------------------------

  x0 = DOUBLE( x( 0 : n - 3 ) )
  x1 = DOUBLE( x( 1 : n - 2 ) )
  x2 = DOUBLE( x( 2 : n - 1 ) )

  y0 = DOUBLE( y( 0 : n - 3 ) )
  y1 = DOUBLE( y( 1 : n - 2 ) )
  y2 = DOUBLE( y( 2 : n - 1 ) )


  ; -----------------------------------------------------
  ; Compute interpolation delta and panel midpoint arrays
  ; -----------------------------------------------------

  dx = x1 - x0
  xmid = 0.5d * ( x1 + x0 )


  ; ----------------------------
  ; Compute Lagrange polynomials
  ; ----------------------------

  l0 = ( ( xmid - x1 ) / ( x0 - x1 ) ) * ( ( xmid - x2 ) / ( x0 - x2 ) )
  l1 = ( ( xmid - x0 ) / ( x1 - x0 ) ) * ( ( xmid - x2 ) / ( x1 - x2 ) )
  l2 = ( ( xmid - x0 ) / ( x2 - x0 ) ) * ( ( xmid - x1 ) / ( x2 - x1 ) )


  ; -------------------------------------------------
  ; Compute interpolated points corresponding to xmid
  ; -------------------------------------------------

  ymid = ( y0 * l0 ) + ( y1 * l1 ) + ( y2 * l2 )


  ; --------------------
  ; Compute integral sum
  ; --------------------

  sum_array = ( 1.0d/6.0d ) * dx * ( y0 + ( 4.0d * ymid ) + y1 )

  sum = TOTAL( sum_array )



  ;------------------------------------------------------------------------------
  ;                     -- Handle last 3 points similarly --
  ;------------------------------------------------------------------------------

  ; ---------------------------
  ; Create interpolation points
  ; ---------------------------

  x0 = DOUBLE( x( n - 3 ) )
  x1 = DOUBLE( x( n - 2 ) )
  x2 = DOUBLE( x( n - 1 ) )

  y0 = DOUBLE( y( n - 3 ) )
  y1 = DOUBLE( y( n - 2 ) )
  y2 = DOUBLE( y( n - 1 ) )


  ; -----------------------------------------------
  ; Compute interpolation delta and midpoint values
  ; -----------------------------------------------

  dx = x2 - x1
  xmid = 0.5d * ( x2 + x1 )


  ; ----------------------------
  ; Compute Lagrange polynomials
  ; ----------------------------

  l0 = ( ( xmid - x1 ) / ( x0 - x1 ) ) * ( ( xmid - x2 ) / ( x0 - x2 ) )
  l1 = ( ( xmid - x0 ) / ( x1 - x0 ) ) * ( ( xmid - x2 ) / ( x1 - x2 ) )
  l2 = ( ( xmid - x0 ) / ( x2 - x0 ) ) * ( ( xmid - x1 ) / ( x2 - x1 ) )


; --------------------------
; Compute interpolated point
; --------------------------

  ymid = ( y0 * l0 ) + ( y1 * l1 ) + ( y2 * l2 )


; -------------------
; Add to integral sum
; -------------------

  sum = sum + ( ( 1.0d/6.0d ) * dx * ( y1 + ( 4.0d * ymid ) + y2 ) )

  sum_array = [ sum_array, sum ]



  ;------------------------------------------------------------------------------
  ;                            -- Done --
  ;------------------------------------------------------------------------------

  CATCH, /CANCEL
  RETURN, sum

END

