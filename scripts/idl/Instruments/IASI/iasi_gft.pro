FUNCTION IASI_GFT, x, MAXX=MaxX

  @iasi_parameters
  
  IF ( N_ELEMENTS(MaxX) EQ 0 ) THEN Xmax=NOMINAL_MAXX_IN_CM ELSE Xmax=DOUBLE(MaxX[0])
  n   = N_ELEMENTS(x)
  gft = DBLARR(n)
  
  sigma = LN2/(!DPI*GFT_HWHM)
  
  valid = WHERE( ABS(x) LE Xmax, n_valid )
  IF ( n_valid GT 0 ) THEN $
    gft[valid] = EXP(-LN2*(x[valid]/sigma)^2)

  RETURN, gft
END
