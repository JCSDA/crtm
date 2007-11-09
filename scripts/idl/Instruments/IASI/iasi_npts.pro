; Function to compute the number of
; spectral points in an IASI band
FUNCTION IASI_nPts, band
  @iasi_parameters
  ib = (band < N_BANDS) > 1
  f1 = BAND_F1[band-1]
  f2 = BAND_F2[band-1]
  df = D_FREQUENCY
  n = LONG((f2-f1)/df + 1.5d0)
  RETURN, n
END
