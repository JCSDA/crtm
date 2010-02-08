;+
; NAME:
;       Integral
;
;
; PURPOSE:
;       Function to integrate tabulated data.
;
;
; CALLING SEQUENCE:
;       result = Integral( x, y 
;                          [,N_POINTS=value]
;                          [,/DOUBLE]
;                          [,/FORCE_SPLINE]
;                          [,X_INT=vector]
;                          [,y_INT=vector] )
;
;
; INPUTS:
;       x:  Vector of abscissa points.
;
;       y:  Vector of corresponding ordinate points.
;
;
; INPUT KEYWORDS:
;       N_POINTS:      Set this keyword to an integer value that selects the
;                      closed Newton-Cotes quadrature formula to be used.
;                      Valid values are:
;                        2: Trapezoidal rule
;                        3: Simpson's rule [DEFAULT]
;                        4: Simpson's 3/8 rule
;                        5: Boole's rule
;                        6: 6-point rule
;                        7: 7-point rule
;
;       DOUBLE:        Set this keyword to force all computations to be done
;                      in double-precision.
;
;       FORCE_SPLINE:  Set this keyword to force spline interpolation to be used.
;                      Spline interpolation is the default UNLESS the number of
;                      input x,y points is < 50; then linear interpolation is used.
;
;
; OUTPUT KEYWORDS:
;       X_INT:         Use this keyword to specify a vector that contains the 
;                      interpolated x-values used in the integration.
;
;       Y_INT:         Use this keyword to specify a vector that contains the 
;                      interpolated y-values used in the integration.
;
;
; FUNCTION RESULT:
;       result:        The function return the integrated value.
;                      If an error occurs a FAILURE code, defined in the error_codes
;                      include file, is returned.
;
;
; INCLUDE FILES:
;       error_codes:   Include file containing error code definitions.
;
;-


; Local procedure to return the coefficients
; for various Newton-Cotes quadrature formulae.

PRO Get_Integration_Coefficients, $
  n_Points, $  ; Input
  c0      , $  ; Output
  c            ; Output
  
  COMPILE_OPT HIDDEN
  
  CASE n_Points OF
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
    ; Default is regular Simpson's rule
    ELSE: BEGIN
      c0 = 1.0d0/3.0d0
      c = [1.0d0, 4.0d0, 1.0d0]
    END
  ENDCASE
  
END


; Main function      

FUNCTION Integral, $
  x, $  ; Input
  y, $  ; Input
  N_POINTS     = n_Points    , $  ; Input
  DOUBLE       = Double      , $  ; Input
  FORCE_SPLINE = Force_Spline, $  ; Input
  X_INT        = x_Int       , $  ; Output
  Y_INT        = y_Int            ; Output


  ; Setup
  ; ...Error handler
  @error_codes
  CATCH, error_status
  IF ( error_status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FAILURE
  ENDIF
  ; ...Check input
  IF ( N_PARAMS() LT 2 ) THEN  $
    MESSAGE, 'Invalid number of arguments.', /NONAME, /NOPRINT
  n = N_ELEMENTS(x)
  IF ( n EQ 0 ) THEN $
    MESSAGE, 'Input X argument not defined!', /NONAME, /NOPRINT
  IF ( N_ELEMENTS(y) EQ 0 ) THEN $
    MESSAGE, 'Input Y argument not defined!', /NONAME, /NOPRINT
  IF ( N_ELEMENTS(y) NE n ) THEN $
    MESSAGE, 'X and Y input vector must be same size.', /NONAME, /NOPRINT
  ; ...Check keywords
  _n_Points = N_ELEMENTS(n_Points) GT 0 ? LONG(n_Points[0]) : 0L
  _Double = KEYWORD_SET(Double)
  x_Info = SIZE(x,/STRUCTURE)
  y_Info = SIZE(y,/STRUCTURE)
  IF ( x_Info.TYPE_NAME EQ 'DOUBLE' AND y_Info.TYPE_NAME EQ 'DOUBLE' ) THEN _Double = 1
  ; ...Set Parameters
  TOLERANCE = (MACHAR(DOUBLE=_Double)).EPS
  ZERO      = KEYWORD_SET(_Double) ? 0.0d0 : 0.0


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


  ; Set interpolation type based on the number of unique input points
  IF ( n GT 50 OR KEYWORD_SET(Force_Spline) ) THEN BEGIN
    Spline_Interpolation = TRUE
  ENDIF ELSE BEGIN
    Spline_Interpolation = FALSE
  ENDELSE
  
  
  ; Get the integration coefficients and set the number of integration points
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
  IF ( Spline_Interpolation ) THEN BEGIN
    yd2   = SPL_INIT(_x, _y, DOUBLE=_Double)
    y_Int = SPL_INTERP(_x, _y, yd2, x_Int, DOUBLE=_Double)
  ENDIF ELSE BEGIN
    y_Int = INTERPOL(_y, _x, x_Int)
  ENDELSE

  
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

