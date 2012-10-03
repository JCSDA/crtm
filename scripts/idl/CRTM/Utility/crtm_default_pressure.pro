;+
;
; NAME:
;       CRTM_Default_Pressure
;
; PURPOSE:
;       Function to create the default pressure profile used in CRTM testing,
;       the 101-level AIRS pressure grid developed by the UMBC ASL.
;
; CALLING SEQUENCE:
;       pressure = CRTM_Default_Pressure( Level_Pressure = level_pressure )
;
; OUTPUT KEYWORDS:
;       Level_Pressure:  Set this keyword to a named variable to return
;                        the pressures at 101 LEVELS.
;                        UNITS:      hPa
;                        TYPE:       REAL(Double)
;                        DIMENSION:  Rank-1
;                        ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;
; FUNCTION RESULT:
;       Pressure:        The average LAYER pressures for 100 layers.
;                        UNITS:      hPa
;                        TYPE:       REAL(Double)
;                        DIMENSION:  Rank-1
;
; PROCEDURE:
;       The LEVEL pressure grid is defined by the formula,
;
;         p(i) = ( a*i^2 + b*i + c )^3.5     ...................(1)
;
;       where p(1)   = 1100hPa
;             p(38)  = 300hPa
;             p(101) = 0.005hPa
;
;       Using these three conditions to solve for a, b, and c, we get,
;
;                                   n - 1
;                z - x - ( y - x ) -------
;                                   m - 1 
;         a = -------------------------------
;                                     n - 1
;              n^2 - 1 - ( m^2 - 1 ) -------
;                                     m - 1
;
;              y - x - a( m^2 - 1 )
;         b = ----------------------
;                     m - 1
;
;         c = x - a - b
;
;       where 
;
;
;                (  ln( 1100 )  )
;         x = exp( ------------ )
;                (     3.5      )
;
;                (  ln( 300 )  )
;         y = exp( ----------- )
;                (     3.5     )
;
;                (  ln( 0.005 )  )
;         z = exp( ------------- )
;                (      3.5      )
;
;         m = 38
;
;         n = 101
;
; The created pressure array is inverted so that
;   p(1) == TOA 
; and
;   p(101) == SFC
;
;
; $Id$
;-

FUNCTION CRTM_Default_Pressure, $
  Level_Pressure = level_pressure

  ; Setup
  @error_codes
  CATCH, error_status
  IF ( error_status NE 0 ) THEN BEGIN
    MESSAGE, !ERR_STRING, /CONTINUE
    CATCH, /CANCEL
    RETURN, FAILURE
  ENDIF
  ; ...Parameters
  ONE = 1.0d


  ; Compute the coefficients
  ; ...Initialise the power
  d = 3.5d
  ; ...Calculate the function results for the boundary conditions
  x = EXP(ALOG(1100.0d) / d)                ; P(1)   = 1100hPa
  y = EXP(ALOG( 300.0d) / d) &  m = 38.0d   ; P(38)  = 300hPa
  z = EXP(ALOG( 0.005d) / d) &  n = 101.0d  ; P(101) = 0.005hPa
  ; ...Solve for the "a" coefficient
  scale = ( n - ONE ) / ( m - ONE )
  a_numerator   = z - x - ( scale * ( y - x ) )
  a_denominator = ( n^2 - ONE ) - ( scale * ( m^2 - ONE ) )
  a = a_numerator / a_denominator
  ; ...Backsubstitute for the "b" coefficient
  b_numerator   = y - x - ( a * ( m^2 - ONE ) )
  b_denominator = m - ONE
  b = b_numerator / b_denominator
  ; Backsubstitute for the "c" coefficient
  c = x - a - b

 
  ; Compute the LEVEL pressures
  ; ...Specify the level numbers
  levels = DINDGEN(LONG(n)) + ONE
  ; ...Calculate the pressures
  level_pressure = ( ( a * levels^2 ) + ( b * levels ) + c )^d
  ; ...Reverse 'em
  level_pressure = REVERSE(level_pressure)


  ; Compute the LAYER pressures
  n = LONG( n )
  pressure =     ( level_pressure[ 0:n-2 ] - level_pressure[ 1:n-1 ] ) / $
  ;          ---------------------------------------------------------
             ALOG( level_pressure[ 0:n-2 ] / level_pressure[ 1:n-1 ] )

  RETURN, pressure

END
