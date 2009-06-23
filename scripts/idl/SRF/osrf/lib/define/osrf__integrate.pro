PRO OSRF::Integrate, $
  Debug=Debug

  ; Set up
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler

  ; Sum up band integrals
  self.Integral = 0.0d0
  FOR i = 0L, self.n_Bands-1L DO $
    self.Integral = self.Integral + Integral(*(*self.Frequency)[i], *(*self.Response)[i])
  self->Set_Flag, /Integrated


  ; Done
  CATCH, /CANCEL

END ; PRO OSRF::Integrate
