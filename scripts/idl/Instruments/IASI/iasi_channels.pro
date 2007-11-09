; Function to compute the channel
; numbers for an IASI band.
FUNCTION IASI_Channels, band
  @iasi_parameters
  ib = (band < N_BANDS) > 1
  n = 0L
  FOR i = 1, ib-1 DO BEGIN
    n = n + IASI_nPts(i)
  ENDFOR
  RETURN, LINDGEN(IASI_nPts(ib)) + n + 1L
END
