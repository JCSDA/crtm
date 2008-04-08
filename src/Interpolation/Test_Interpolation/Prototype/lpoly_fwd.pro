PRO lpoly_FWD, x, x_int, lp
  @interpolation_parameters
  lp = DBLARR(NPTS+1,NPTS)
  FOR j = 0, NPTS-1 DO BEGIN
    lp[0,j] = ONE
    FOR ix = 1, NPTS DO BEGIN
      i = ix-1
      IF ( i EQ j ) THEN BEGIN
        lp[ix,j]=lp[i,j]
        CONTINUE
      ENDIF
      lp[ix,j] = lp[i,j]*(x_int-x[i])/(x[j]-x[i])
    ENDFOR
  ENDFOR
END
