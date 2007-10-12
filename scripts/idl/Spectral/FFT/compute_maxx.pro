FUNCTION Compute_MaxX, f
  @fft_parameters
  dF = Compute_MeanDelta(f)
  RETURN, ONE/(TWO*dF)
END

