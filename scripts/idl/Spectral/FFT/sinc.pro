FUNCTION Sinc, x
  @fft_parameters
  y = DBLARR(N_ELEMENTS(x))
  i = WHERE(ABS(x) GT ZERO, m, COMPLEMENT=j, NCOMPLEMENT=n)
  IF ( m GT 0 ) THEN $
    y[i] = SIN(x[i])/x[i]
  IF ( n GT 0 ) THEN $
    y[j] = ONE
  RETURN, y
END
