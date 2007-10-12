FUNCTION Compute_NyquistF, x
  @fft_parameters
  dX = Compute_MeanDelta(x)
  RETURN, ONE/(TWO*dX)
END

