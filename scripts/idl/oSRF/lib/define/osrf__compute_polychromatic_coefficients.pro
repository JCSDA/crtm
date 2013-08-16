PRO OSRF::Compute_Polychromatic_Coefficients, $
  Debug=Debug

  ; Set up
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler


  ; Check if object has been allocated
  IF ( ~self.Associated(Debug=Debug) ) THEN $
    MESSAGE, 'OSRF object has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Get the sensor type
  self.Get_Property, Sensor_Type=sensor_type, Debug=Debug


  ; Compute the central frequency if necessary
  IF ( ~self.Flag_Is_Set(F0_COMPUTED_FLAG) ) THEN self.Compute_Central_Frequency, Debug=Debug

  
  ; Copy the SRF into a work object
  self.Assign, work, Debug=Debug

  
  ; Perform conversions to units of inverse centimetres if necessary
  IF ( sensor_type EQ MICROWAVE_SENSOR ) THEN BEGIN
    ; ...Convert frequency arrays
    FOR i = 0L, work.n_Bands-1L DO BEGIN
      work.Get_Property, i+1, Frequency=f, Debug=Debug
      f = GHz_to_inverse_cm(f)
      work.Set_Property, i+1, Frequency=f, Debug=Debug
    ENDFOR
    ; ...Clear the frequency units flag to indicate cm-1
    work.Clear_Flag, /Frequency_GHz, Debug=Debug
    ; ...Recompute integral
    work.Integrate, Debug=Debug
    ; ...Recompute central frequency
    work.Compute_Central_Frequency, Debug=Debug
  ENDIF
  

  ; Generate the "monochromatic" temperatures
  ; ...Set min and max temps based on Sensor_Type
  IF ( sensor_type EQ VISIBLE_SENSOR ) THEN BEGIN
    min_T = 3000.0d0
    max_T = 5700.0d0
  ENDIF ELSE BEGIN 
    min_T = 150.0d0
    max_T = 340.0d0
  ENDELSE
  ; ...Compute the set of temperatures
  d_T  = 10.0d0
  n_T  = LONG((max_T-min_T)/d_T) + 1L
  T    = DINDGEN(n_T)/DOUBLE(n_T-1L)
  T    = T*(max_T-min_T) + min_T
  Teff = DBLARR(n_T)


  ; Generate the polychromatic temperatures
  FOR i = 0L, n_T-1L DO BEGIN
    ; ...Compute the monochromatic Planck radiances
    work.Compute_Planck_Radiance, T[i], Debug=Debug
    ; ...Convolve Planck radiance with SRF
    work.Convolved_R = work.Convolve(work.Radiance, Debug=Debug) 
    ; ...Convert convolved radiance back to temperature
    result = Planck_Temperature(work.f0, work.Convolved_R, x)
    IF ( result NE SUCCESS ) THEN $
      MESSAGE, 'Error computing effective temperature at T='+STRING(T[i],FORMAT='(f5.1)'), $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    Teff[i] = x
  ENDFOR
  
  
  ; Perform the polynomial fit
  Degree_of_Fit = N_POLYCHROMATIC_COEFFS-1L
  x = POLY_FIT( T, Teff, Degree_of_Fit, /DOUBLE, STATUS=status, YFIT=tfit )
  IF ( status NE 0 ) THEN $
    MESSAGE, 'Error performing polynomial fit', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ; ...Save results in original object
  self.Polychromatic_Coeffs = x
  

  ; Cleanup
  OBJ_DESTROY, work, Debug=Debug
  
END
