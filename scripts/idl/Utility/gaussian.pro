;+
FUNCTION Gaussian, x0  , $ ; Input 
                   fwhm, $ ; Input
                   n   , $ ; Input. Gauusian power
                   x       ; Input. x-axis grid
;-

  ; Set up error handler
  ; --------------------
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    HELP, /LAST_MESSAGE
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FAILURE
  ENDIF

  ; Set some parameters
  ; -------------------
  ; Floating point precision tolerance value
  tolerance = (MACHAR(/DOUBLE)).EPS
  ; Literal constants
  ZERO          = 0.0d0
  ZEROpointFIVE = 0.5d0
  TWO           = 2.0d0
  
  ; Compute Gaussian
  ; ----------------
  ; Width values
  hwhm = fwhm/TWO
  emult = ALOG(ZEROpointFIVE)/(hwhm^n)
  ; The distribution
  dx = ABS(x-x0)
  g = EXP(emult*(dx^n))

  ; Clean up
  ; --------
  idx = WHERE( g LT tolerance, count )
  IF ( count GT 0 ) THEN g[idx] = ZERO
  junk = CHECK_MATH()
  
  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, g
  
END
