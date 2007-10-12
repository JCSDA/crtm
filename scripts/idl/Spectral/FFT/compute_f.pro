FUNCTION Compute_F, x
  nX = N_ELEMENTS(x)
  nF = Compute_nSpc(nX)
  NyquistF = Compute_NyquistF(x)
  f = NyquistF * DINDGEN(nF)/DOUBLE(nF-1L)
  RETURN, f
END

