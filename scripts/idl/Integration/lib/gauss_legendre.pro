;+
; Procedure to compute Gaussian abscissa and weights for integration

PRO Gauss_Legendre, $
  n, $ ; Input
  x, $ ; Output
  weight, $ ; Output
  xinterval=xinterval ; Optional input
;-

  ; Parameters
  @fundamental_constants
  POINT5 = 0.5d0
  MAX_ITERATIONS = 1000L
  TOLERANCE = (MACHAR(/DOUBLE)).EPS
  
  ; Set the integration limits
  IF ( N_ELEMENTS(xinterval) EQ 2 ) THEN BEGIN
    xlower = MIN(xinterval)
    xupper = MAX(xinterval)
  ENDIF ELSE BEGIN
    xlower = -ONE
    xupper =  ONE
  ENDELSE
  
  ; Create the output arrays
  x      = DBLARR(n)
  weight = DBLARR(n)
  
  ; The roots are symmetric so only half need to be determined
  m = (n+1L)/2L
  
  ; The scaling values to convert the computed abscissae from
  ; the interval (-1,1) to the input interval (xlower, xupper)
  xaverage   = POINT5 * (xupper + xlower) ; Average X of interval
  xhalfwidth = POINT5 * (xupper - xlower) ; Halfwidth of interval

  ; Loop over roots to find
  FOR i = 1, m DO BEGIN ; Root loop
  
    ; Approximate the i'th root with the analytic
    ; value for Chebyshev polynomials,
    z = COS(PI * (DOUBLE(i)-POINT5) / DOUBLE(n))
  
    ; Initialise the iteration counter used
    ; to refine the polynomial root value
    n_Iterations = 0

    ; Refine the i'th root by Newton's method
    REPEAT BEGIN

      ; Increment and test the iteration counter
      n_Iterations++
      IF ( n_Iterations GT MAX_ITERATIONS ) THEN $
        MESSAGE, 'Maximum number of iterations exceeded for finding root #'+STRTRIM(i,2)
                 
      ; Initialise the lower order polynomials
      Pj   = ONE    ; j'th   polynomial
      Pj_1 = ZERO   ; j-1'th polynomial
    
      ; Loop up the recurrence relation to get
      ; the Legendre polynomial evaluated at x
      FOR j = 1, n DO BEGIN ; Recurrance loop
      
        ; Demote the previous loop's polynomials
        Pj_2 = Pj_1   ; j-2'th polynomial
        Pj_1 = Pj     ; j-1'th polynomial

        ; Compute the new polynomial using a recurrence relation
        Pj = ((((TWO * DOUBLE(j)) - ONE) * z * Pj_1) - ((DOUBLE(j) - ONE) * Pj_2)) / DOUBLE(j)
      
      ENDFOR ; Recurrance loop
      
      ; Pj is now the desired Legendre polynomial. Now compute its
      ; derivate, Pj_Prime, using a standard relation involving also
      ; Pj_1, the polynomial of one lower order
      Pj_Prime = DOUBLE(n) * ((z * Pj) - Pj_1) / ((z*z) - ONE)
      
      ; Refine the root value using Newton's method
      ;...Save the previous root value
      z1 = z
      ;...Update the root approximation
      z  = z1 - ( Pj / Pj_Prime )

    ENDREP UNTIL (ABS(z-z1) LT TOLERANCE)
    
    ; Scale the root value, z, and it's symmetric counterpart
    ; from the (-1,1) interval to the (xLower,xUpper) interval
    x[i-1] = xaverage - (xhalfwidth * z)
    x[n-i] = xaverage + (xhalfwidth * z)
      
    ; Compute the weight and its symmetric counterpart using 
    ; the special form for the Gauss-Legendre case
    weight[i-1] = TWO * xhalfwidth / ((ONE - (z^2)) * Pj_Prime^2)
    weight[n-i] = weight[i-1]

  ENDFOR ; Root loop
        
END ; PRO Gauss_Legendre
