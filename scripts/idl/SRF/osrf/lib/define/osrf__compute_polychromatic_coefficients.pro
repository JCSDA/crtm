PRO OSRF::Compute_Polychromatic_Coefficients, $
  Debug=Debug

  ; Set up
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler

  ; Create temperature arrays
  min_T = 150.0d0
  max_T = 340.0d0
  d_T   = 10.0d0
  n_T   = LONG((max_T-min_T)/d_T) + 1L
  T = DINDGEN(n_T)/DOUBLE(n_T-1L)
  T = T*(max_T-min_T) + min_T
  Teff = DBLARR(n_T)

  ; Compute the central frequency if necessary
  IF ( NOT self->Flag_Is_Set(F0_COMPUTED_FLAG) ) THEN self->Compute_Central_Frequency, Debug=Debug
  
  ; Copy the SRF
  self->Assign, new, Debug=Debug
  
  ; Perform conversions to units of inverse centimetres if necessary
  IF ( new->Flag_Is_Set(FREQUENCY_UNITS_FLAG) ) THEN BEGIN
    ; ...Convert frequency arrays
    FOR i = 0L, new.n_Bands-1L DO *(*new.Frequency)[i] = GHz_to_inverse_cm(*(*new.Frequency)[i])
    ; ...Recompute integral
    new->Integrate, Debug=Debug
    ; ...Recompute central frequency
    new->Compute_Central_Frequency, Debug=Debug
    ; ...Clear the frequency units flag to indicate cm-1
    new->Clear_Flag, /Frequency_Units, Debug=Debug
  ENDIF
  

  ; Generate the polychromatic temperatures
  FOR i = 0L, n_T-1L DO BEGIN
    ; ...Compute the monochromatic Planck radiances
    new->Compute_Planck_Radiance, T[i], Debug=Debug
    ; ...Convolve Planck radiance with SRF
    Reff = new->Convolve(*new.B, Debug=Debug) 
    ; ...Convert convolved radiance back to temperature
    result = Planck_Temperature(new.f0, Reff, x)
    IF ( result NE SUCCESS ) THEN $
      MESSAGE, 'Error computing effective temperature at T='+STRING(T[i],FORMAT='(f5.1)'), $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    Teff[i] = x
  ENDFOR
  
  
  ; Perform the polynomial fit
  Degree_of_Fit = N_POLYCHROMATIC_COEFFS-1L
  x = POLY_FIT( T, Teff, Degree_of_Fit, /DOUBLE, STATUS=Status, YFIT=Tfit )
  IF ( Status NE 0 ) THEN $
    MESSAGE, 'Error performing polynomial fit', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ; ...Save results in original object
  self.Polychromatic_Coeffs = x
  

  ; Cleanup
  new->Destroy, Debug=Debug
  
    
  ; Done
  CATCH, /CANCEL

END ; PRO OSRF::Compute_Polychromatic_Coefficients
