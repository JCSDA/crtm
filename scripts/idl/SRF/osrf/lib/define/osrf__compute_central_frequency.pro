PRO OSRF::Compute_Central_Frequency, $
  Debug=Debug

  ; Set up
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler


  ; Compute the SRF first moment
  self.f0 = self->Convolve(*self.Frequency)
  self->Set_Flags, /f0_Computed


  ; Done
  CATCH, /CANCEL

END ; PRO OSRF::Compute_Central_Frequency
