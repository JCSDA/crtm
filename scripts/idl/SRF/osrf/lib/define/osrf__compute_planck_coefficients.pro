PRO OSRF::Compute_Planck_Coefficients, $
  Debug=Debug

  ; Set up
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
  ; ...Definition of C1 and C2
  @fundamental_constants
  ; ...Parameters for C1, C2 scaling
  ; The following scaling factors are applied to produce radiances in units
  ; of mW/(m^2.sr.cm^-1) when they are used.
  ;
  ; First Planck function constant (C1) scale factors. Units of C1 are W.m^2.
  ; Length scaling: To convert to W/(m^2.cm^-4) requires a scaling of m->cm,
  ;                 which is 100, to the fourth power, which is 1.0e+08.
  ; Power scaling:  To convert to mW.m^2 requires a scaling of 1000.
  C1_LENGTH_SCALE_FACTOR = 1.0d+08
  C1_POWER_SCALE_FACTOR  = 1.0d+03
  C1_SCALE_FACTOR = C1_LENGTH_SCALE_FACTOR * C1_POWER_SCALE_FACTOR
  ; Second Planck function constant (C2) scale factor. Units of C2 are K.m,
  ; So to convert to K.cm, a scaling of 100 is applied.
  C2_SCALE_FACTOR = 100.0d0
  
  
  ; Compute the central frequency if necessary
  IF ( NOT self->Flag_Is_Set(F0_COMPUTED_FLAG) ) THEN self->Compute_Central_Frequency, Debug=Debug
  

  ; Check frequency units
  f0 = self.f0
  IF ( self->Flag_Is_Set(FREQUENCY_UNITS_FLAG) ) THEN f0 = GHz_to_inverse_cm(f0)


  ; Comput ethe Planck coefficients
  self.Planck_C1 = C1_SCALE_FACTOR * C1 * f0^3
  self.Planck_C2 = C2_SCALE_FACTOR * C2 * f0
  
    
  ; Done
  CATCH, /CANCEL

END ; PRO OSRF::Compute_Polychromatic_Coefficients
