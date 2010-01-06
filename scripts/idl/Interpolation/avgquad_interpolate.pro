FUNCTION AvgQuad_Interpolate_dx, x
  COMPILE_OPT HIDDEN
  dx = [x[0]-x[1], x[0]-x[2], x[1]-x[2]]
  RETURN, dx
END

FUNCTION AvgQuad_Interpolate_dxi, x, xi
  COMPILE_OPT HIDDEN
  dxi = [xi-x[0], xi-x[1], xi-x[2]]
  RETURN, dxi
END

FUNCTION AvgQuad_Interpolate_QPoly, dxi, dx
  COMPILE_OPT HIDDEN
  lp = [dxi[1]*dxi[2] / ( dx[0]*dx[1]), $
        dxi[0]*dxi[2] / (-dx[0]*dx[2]), $
        dxi[0]*dxi[1] / ( dx[1]*dx[2])  ]
  RETURN, lp
END

FUNCTION AvgQuad_Interpolate_LPoly, x, xi, DOUBLE=Double
  COMPILE_OPT HIDDEN
  ; Parameters
  ZERO = KEYWORD_SET(Double) ? 0.0d0 : 0.0
  ONE  = KEYWORD_SET(Double) ? 1.0d0 : 1.0
  ; Compute the numerator differences
  dxi_left  = AvgQuad_Interpolate_dxi(x[0:2],xi)
  dxi_right = AvgQuad_Interpolate_dxi(x[1:3],xi)
  ; Compute the denominator differences
  dx_left  = AvgQuad_Interpolate_dx(x[0:2])
  dx_right = AvgQuad_Interpolate_dx(x[1:3])
  ; Compute the quadratic polynomials
  lp_left  = AvgQuad_Interpolate_QPoly(dxi_left , dx_left )
  lp_right = AvgQuad_Interpolate_QPoly(dxi_right, dx_right)
  ; Polynomial weights
  CASE 1 OF
    ( xi LT x[1] ): BEGIN
      w_right = ZERO
      w_left  = ONE
    END
    ( xi GT x[2] ): BEGIN
      w_right = ONE
      w_left  = ZERO
    END
    ELSE: BEGIN
      w_right = dxi_left[1] / (-dx_left[2])
      w_left  = ONE - w_right
    END
  ENDCASE
  RETURN, { LPoly, $
            lp_left  : lp_left, $
            lp_right : lp_right, $
            w_left   : w_left, $
            w_right  : w_right }
END


FUNCTION AvgQuad_Interpolate, $
  x, $  ; Input
  y, $  ; Input
  xi, $  ; Input
  DOUBLE = Double

  ; Setup
  ; ...Error handler
  @error_codes
  CATCH, error_status
  IF ( error_status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FAILURE
  ENDIF


  ; Check input
  IF ( N_PARAMS() LT 3 ) THEN  $
    MESSAGE, 'Invalid number of arguments.', /NONAME, /NOPRINT
  ; ...Check that arguments are defined
  x_Info  = SIZE(x,/STRUCTURE)
  y_Info  = SIZE(y,/STRUCTURE)
  xi_Info = SIZE(xi,/STRUCTURE)
  IF ( x_Info.TYPE_NAME EQ 'UNDEFINED' ) THEN $
    MESSAGE, 'Input X argument not defined!', /NONAME, /NOPRINT
  IF ( y_Info.TYPE_NAME EQ 'UNDEFINED' ) THEN $
    MESSAGE, 'Input Y argument not defined!', /NONAME, /NOPRINT
  IF ( xi_Info.TYPE_NAME EQ 'UNDEFINED' ) THEN $
    MESSAGE, 'Input XI argument not defined!', /NONAME, /NOPRINT
  ; ...Check the number of dimensions
  IF ( x_Info.N_DIMENSIONS  NE 1 OR $
       y_Info.N_DIMENSIONS  NE 1 OR $
       xi_Info.N_DIMENSIONS NE 1    ) THEN $
    MESSAGE, 'Only vector input allowed', /NONAME, /NOPRINT
  ; ...Check the number of points
  IF ( x_Info.N_ELEMENTS NE y_Info.N_ELEMENTS ) THEN $
    MESSAGE, 'X and Y input vector must be same size.', /NONAME, /NOPRINT
  IF ( x_Info.N_ELEMENTS LE 4 ) THEN $
    MESSAGE, 'Not enough data points! Routine requires N >= 4.', /NONAME, /NOPRINT
  ; ...Ensure input x and xi data are sorted
  idx = WHERE(SORT(x) - LINDGEN(x_Info.N_ELEMENTS) NE 0, count)
  IF ( count NE 0 ) THEN $
  ; ...Set local double keyword based on input data
  _Double = KEYWORD_SET(Double)
  IF ( x_Info.TYPE_NAME  EQ 'DOUBLE' OR $
       y_Info.TYPE_NAME  EQ 'DOUBLE' OR $
       xi_Info.TYPE_NAME EQ 'DOUBLE'    ) THEN _Double = 1


  ; Set parameters
  ZERO  = KEYWORD_SET(_Double) ? 0.0d0 : 0.0


  ; Copy input at required precision
  _x  = KEYWORD_SET(_Double) ? DOUBLE(x)  : x
  _y  = KEYWORD_SET(_Double) ? DOUBLE(y)  : y
  _xi = KEYWORD_SET(_Double) ? DOUBLE(xi) : xi


  ; ...Ensure input x data is monotonic
  dx = _x[1] - _x[0]
  IF ( dx GT ZERO ) THEN BEGIN
    idx = SORT(_x)           ; Ascending
  ENDIF ELSE BEGIN
    idx = SORT(REVERSE(_x))  ; Descending
  ENDELSE
  idx = idx - LINDGEN(x_Info.N_ELEMENTS)
  loc = WHERE(idx NE 0, count)
  IF ( count NE 0 ) THEN $
    MESSAGE, 'Input x data must be monotonic (ascending or descending)', /NONAME, /NOPRINT


  ; Create array for interpolates
  yi = KEYWORD_SET(_Double) ? DBLARR(xi_Info.N_ELEMENTS) : FLTARR(xi_Info.N_ELEMENTS)
  
  
  ; Get the indices of the x intervals for interpolation
  idx = (VALUE_LOCATE(_x, _xi) > 1L) < (x_Info.N_ELEMENTS - 3L)


  ; Perform the interpolations
  FOR i = 0L, xi_Info.N_ELEMENTS-1L DO BEGIN
    j = idx[i]
    ; Compute the interpolating polynomials
    p = AvgQuad_Interpolate_LPoly(_x[j-1L:j+2L], _xi[i], DOUBLE=_Double)
    ; Interpolate and average
    yi[i] = (p.w_left  * (p.lp_left[0]  * _y[j-1L] + $
                          p.lp_left[1]  * _y[j]    + $
                          p.lp_left[2]  * _y[j+1L] ) ) + $
            (p.w_right * (p.lp_right[0] * _y[j]    + $
                          p.lp_right[1] * _y[j+1L] + $
                          p.lp_right[2] * _y[j+2L] ) )
  ENDFOR
  
  ; Done
  CATCH, /CANCEL
  RETURN, yi
  
END

