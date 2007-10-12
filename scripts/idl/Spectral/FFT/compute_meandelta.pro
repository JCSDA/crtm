FUNCTION Compute_MeanDelta, a
  n = N_ELEMENTS(a)
  dA = TOTAL(a[1L:n-1L] - a[0L:n-2L])/DOUBLE(n-1L)
  RETURN, dA
END

