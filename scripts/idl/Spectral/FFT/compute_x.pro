FUNCTION Compute_X, f
  nF = N_ELEMENTS(f)
  nX = Compute_nIfg(nF)
  MaxX = Compute_MaxX(f)
  x = DBLARR(nX)
  x[nF-2L:nX-1L] = MaxX * DINDGEN(nF)/DOUBLE(nF-1L)  ; +ve delays
  x[0:nF-3L]     = REVERSE(-x[nF-1L:nX-2L])          ; Relfect for -ve delays
  RETURN, x
END

