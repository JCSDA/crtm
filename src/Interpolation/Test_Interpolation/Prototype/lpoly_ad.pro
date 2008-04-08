PRO lpoly_AD, x, x_int, lp, $
              lp_AD, $
              x_AD, x_int_AD
  @interpolation_parameters
  d = DBLARR(NPTS)
  
  ; The denominator terms
  d[0] = ONE/(x[0]-x[1])
  d[1] = -d[0]
  
  ; Adjoint of lp[1]
  x_AD[1]  = x_AD[1]  - d[1]*lp[1]*lp_AD[1]
  x_AD[0]  = x_AD[0]  + d[1]*(lp[1]-ONE)*lp_AD[1]
  x_int_AD = x_int_AD + d[1]*lp_AD[1]
  lp_AD[1] = ZERO
  
  ; Adjoint of lp[0]
  x_AD[1]  = x_AD[1]  + d[0]*(lp[0]-ONE)*lp_AD[0]
  x_AD[0]  = x_AD[0]  - d[0]*lp[0]*lp_AD[0]
  x_int_AD = x_int_AD + d[0]*lp_AD[0]
  lp_AD[0] = ZERO
END
