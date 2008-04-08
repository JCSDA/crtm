PRO lpoly, x, x_int, lp
  @interpolation_parameters
  lp = DBLARR(NPTS)
  FOR j = 0, NPTS-1 DO BEGIN
    lp[j] = ONE
    FOR i = 0, NPTS-1 DO BEGIN
      IF ( i EQ j ) THEN CONTINUE
      lp[j] = lp[j]*(x_int-x[i])/(x[j]-x[i])
    ENDFOR
  ENDFOR
END
