;+
;
; Constants used in Planck function routines
;
  ; Unit types
  X_UNITS = ['Frequency (cm^-1)','Wavelength (um)']
  FREQUENCY_INDEX  = 0
  WAVELENGTH_INDEX = 1

  ; Scale factors. One for each unit type
  ; -------------------------------------
  ; Radiance scale factor
  ;   Frequency:  Scaling factor to convert mW/(m2.sr.cm-1) -> W/(m2.sr.cm-1)
  ;   Wavelength: Scaling factor set to 1.0 since for wavelength we want W/(m2.sr.um)
  R_SCALE  = [1000.0d0, 1.0d0]
  ; C1 derived constant scale factor
  ;   Frequency:   W.m2 to W/(m2.cm-4) => multiplier of 1.0e+08 is required.
  ;   Wavelength:  W.m2 to W/(m2.um-4) => multiplier of 1.0e+24 is required.
  C1_SCALE = [ 1.0d+08, 1.0d+24]
  ; C2 derived constant scale factor
  ;   Frequency:   K.m to K.cm => multiplier of 100 is required
  ;   Wavelength:  K.m to K.um => multiplier of 1.0e+06 is required.
  C2_SCALE = [ 100.0d0, 1.0d+06]

;- 
