; Function to compute the resampled
; frequency grid
; f = 645 + 0.25*(n-1), n=1,8461
FUNCTION IASI_f, band
  @iasi_parameters
  f1 = BAND_F1[band-1]
  f2 = BAND_F2[band-1]
  df = D_FREQUENCY
  n = LONG((f2-f1)/df + 1.5d0)
  f = DINDGEN(n)/DOUBLE(n-1L)
  f = f*(f2-f1) + f1
  RETURN, f
END
