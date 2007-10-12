FUNCTION Compute_MaxF, f1, f2
  delta_f = f2-f1
  n = CEIL(f2/delta_f)
  RETURN, DOUBLE(n)*delta_f
END

