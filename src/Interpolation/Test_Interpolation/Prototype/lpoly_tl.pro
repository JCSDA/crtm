PRO lpoly_TL, x, x_int, lp, $
              x_TL, x_int_TL, $
              lp_TL
  @interpolation_parameters
  lp_TL = DBLARR(NPTS+1,NPTS)
  FOR j = 0, NPTS-1 DO BEGIN
    lp_TL[0,j] = ZERO
    FOR ix = 1, NPTS DO BEGIN
      i=ix-1
      IF ( i EQ j ) THEN BEGIN
        lp_TL[ix,j]=lp_TL[i,j]
        CONTINUE
      ENDIF
      i_xj_m_xi = ONE/(x[j]-x[i])
      lp_TL[ix,j] = i_xj_m_xi*( (x_int-x[i])*lp_TL[i,j] + $
                                lp[i,j]*x_int_TL + $
                                lp[i,j]*(x_int-x[j])*i_xj_m_xi*x_TL[i] - $
                                lp[i,j]*(x_int-x[i])*i_xj_m_xi*x_TL[j]   )
    ENDFOR
  ENDFOR
END
