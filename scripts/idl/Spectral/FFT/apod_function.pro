FUNCTION Apod_Function, x, MAXX=MaxX, APOD_TYPE=Apod_Type

  @fft_parameters
  
  ; Set type
  aType = -1L ; Doesn't match any defined type, so force default
  IF ( N_ELEMENTS(Apod_Type) NE 0 ) THEN aType = LONG(Apod_Type[0])

  ; Get sizes
  n_Ifg = N_ELEMENTS(x)
  IF ( (n_Ifg MOD 2) NE ZERO ) THEN $
    MESSAGE, 'The size of the input OPD array, x, must be divisible by 2'
  n_Spc = Compute_nSpc(n_Ifg)
  
  ; Create the grid array
  IF ( N_ELEMENTS(MaxX) GT 0 ) THEN xMax=DOUBLE(MaxX[0]) ELSE xMax = x[n_Ifg-1L]
  a = x[n_Spc-2L:n_Ifg-1L]/xMax
  
  ; Get indices of grid to use
  idx = WHERE(a LT ONE)

  ; Create output array
  y = DBLARR(n_Spc)
  
  ; Compute apodisation function for +ve delays
  ;
  ; The formulae taken from:
  ;   Weisstein, Eric W. "Apodization Function."
  ;   From MathWorld--A Wolfram Web Resource.
  ;   http://mathworld.wolfram.com/ApodizationFunction.html
  ;
  ; NOTE: Default apodisation function is CONNES_APOD
  CASE aType OF
    BARTLETT_APOD: y[idx] = ONE - a[idx]
    WELCH_APOD:    y[idx] = ONE - a[idx]^2
    COSINE_APOD:   y[idx] = COS(POINT5*!DPI*a[idx])
    HAMMING_APOD:  y[idx] = POINT54 + (POINT46*COS(!DPI*a[idx]))
    HANNING_APOD:  y[idx] = POINT5*(ONE + COS(!DPI*a[idx]))
    ELSE:          y[idx] = (ONE - a[idx]^2)^2
  ENDCASE

  ; Reflect for -ve delays
  y = [REVERSE(y[1:n_Spc-2L]), y]
  RETURN, y
END
