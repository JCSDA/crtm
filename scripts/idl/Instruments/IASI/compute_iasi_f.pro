;FUNCTION Compute_IASI_F, PO2=po2
;  @iasi_parameters
;  
;  exponent=0L
;  IF ( N_ELEMENTS(po2) GT 0 ) THEN exponent=LONG(po2[0])
;  
;  n_Ifg = N_FFT * (2L^exponent)
;  n_Spc = Compute_nSpc(n_Ifg)
;  x  = Compute_IASI_X()
;  dx = Compute_MeanDelta(x)
;  Bandwidth = ONE/(TWO*dx)
;  f = DINDGEN(n_Spc)/DOUBLE(n_Spc-1L)
;  RETURN, f*Bandwidth
;END
FUNCTION Compute_IASI_F, PO2=po2
  @iasi_parameters
  
  exponent=0L
  IF ( N_ELEMENTS(po2) GT 0 ) THEN exponent=LONG(po2[0])
  
  n_Ifg = N_FFT * (2L^exponent)
  n_Spc = Compute_nSpc(n_Ifg)
  x  = Compute_IASI_X()
  dx = Compute_MeanDelta(x)
  Bandwidth = ONE/(TWO*dx)
  f = DINDGEN(n_Spc)/DOUBLE(n_Spc-1L)
  RETURN, f*Bandwidth
END
