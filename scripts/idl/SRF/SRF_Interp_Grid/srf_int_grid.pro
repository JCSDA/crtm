; function to obtain a 
; frequency grid at .1cm^-1 from
; the original

Function SRF_int_grid,  Frequency  ; Input
                   
  
  ; obtain frequency grid at .1 cm^-1 intervals
  ; Find the minimum and maximum frequency at a 0.1cm-1 value.
  minF = MIN(Frequency, MAX=maxF)
  iminF = (DOUBLE(LONG(minF)))
  minF = iminF+DOUBLE(LONG(10.0d0*(minF-iminF)))/10.0d0 + 0.1d0
  imaxF = (DOUBLE(LONG(maxF)))
  maxF = imaxF+DOUBLE(LONG(10.0d0*(maxF-imaxF)))/10.0d0

  ; The new frequency grid
  n=ROUND(10.0d0*(maxF-minF))+1L
  f=DINDGEN(n)/DOUBLE(n-1)
  f=f*(maxF-minF) + minF
  return, f
END

