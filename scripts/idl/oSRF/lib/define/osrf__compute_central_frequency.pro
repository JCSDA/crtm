PRO OSRF::Compute_Central_Frequency, $
  Debug=Debug

  ; Set up
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler


  ; Compute the SRF first moment
  f0 = self->Convolve(*self.Frequency, Debug=Debug)
  self->Set_Property, $
    f0=f0, $
    Debug=Debug
  self->Set_Flag, /f0_Computed

END ; PRO OSRF::Compute_Central_Frequency
