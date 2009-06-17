PRO OSRF::Interpolate, $
  int_OSRF, $
  Debug = Debug

  ; Set up
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
 
  Sigma = 5.0d0
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


  ; Done
  CATCH, /CANCEL
 
END ; PRO OSRF::Interpolate
