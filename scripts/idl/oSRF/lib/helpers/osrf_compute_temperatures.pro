;+
; Helper function to compute and return the array of temperatures
; used in the polychromatic correction coefficient calculation.
;
;  T = oSRF_Compute_Temperatures(min_T, max_T, d_T)
;

FUNCTION oSRF_Compute_Temperatures, $
  min_T, $  ; Input  - minimum temperature
  max_T, $  ; Input  - maximum temperature
  d_T       ; Input  - temperature increment
;-

  n    = LONG((max_T-min_T)/d_T) + 1L
  T    = DINDGEN(n)/DOUBLE(n-1L)
  T    = T*(max_T-min_T) + min_T

  RETURN, T
END
