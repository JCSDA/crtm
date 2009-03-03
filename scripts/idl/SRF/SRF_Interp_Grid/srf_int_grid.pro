; function to obtain a 
; frequency grid at .1cm^-1 from
; the original

Function SRF_int_grid,  Frequency,     $ ; Input
                        Min_F=Min_F,   $ ; Min frequency
                        Max_F=Max_F      ; Max frequency
                   
  
  ; obtain frequency grid at .1 cm^-1 intervals
  ; Find the minimum and maximum frequency at a 0.1cm-1 value.
  IF (N_ELEMENTS(Min_F) EQ 1) THEN BEGIN
    minF=Min_F
  ENDIF ELSE BEGIN
    minF=MIN(Frequency)
  ENDELSE
  IF (N_ELEMENTS(Max_F) EQ 1) THEN BEGIN
    maxF=Max_F
  ENDIF ELSE BEGIN
    maxF=MAX(Frequency)
  ENDELSE
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

