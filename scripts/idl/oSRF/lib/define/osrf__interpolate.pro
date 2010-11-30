PRO OSRF::Interpolate, $
  int_OSRF, $
  Sigma = Sigma, $
  Debug = Debug

  ; Set up
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
 
  ; Perform the interpolation
  IF ( NOT KEYWORD_SET(Sigma) ) THEN Sigma = 5.0d0
  FOR i = 0L, self.n_Bands-1L DO BEGIN
    f     = *(*self.Frequency)[i]
    r     = *(*self.Response)[i]
    f_int = *(*int_OSRF.Frequency)[i]

    IF ( self->Flag_Is_Set(INTERPOLATION_METHOD_FLAG) ) THEN BEGIN
      *(*int_OSRF.Response)[i] = INTERPOL( r, f, f_int )
    ENDIF ELSE BEGIN
      *(*int_OSRF.Response)[i] = SPLINE( f, r, f_int, Sigma, /DOUBLE )
    ENDELSE
  ENDFOR
  
  
  ; Recompute the various SRF parameters
  int_OSRF->Integrate, Debug=Debug
  int_OSRF->Compute_Central_Frequency, Debug=Debug
  int_OSRF->Compute_Planck_Coefficients, Debug=Debug
  int_OSRF->Compute_Polychromatic_Coefficients, Debug=Debug
  
END ; PRO OSRF::Interpolate
