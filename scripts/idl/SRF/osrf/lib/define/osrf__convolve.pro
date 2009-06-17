FUNCTION OSRF::Convolve, $
  ptr, $  ; Input
  Debug=Debug

  ; Set up
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_func_err_handler


  ; Sum up band integrals
  y = 0.0d0 
  FOR i = 0L, self.n_Bands-1L DO BEGIN
    IF ( NOT self->Flag_Is_Set(INTEGRATED_FLAG) ) THEN self->Integrate, Debug=Debug
    y = y + Integral( *(*self.Frequency)[i], (*ptr[i]) * (*(*self.Response)[i]))
  ENDFOR
  y = y / self.Integral


  ; Done
  CATCH, /CANCEL
  RETURN, y
 
END ; FUNCTION OSRF::Convolve
