PRO SPCtoIFG, f, spc, $  ; Input
              x, ifg     ; Output

  n_Spc = N_ELEMENTS(f)
  
  ; Reflect the spectrum
  ; --------------------
  ; The "x" represent the input spectrum. The "o" represent how
  ; the data is reflected about the Nyquist frequency prior to
  ; calling the FFT routine.
  ;
  ; nSpc = 5
  ; nIfg = 2*(nSpc-1) = 8
  ;
  ;     Zero            nSpc
  ;  frequency     (Nyquist pt)     nIfg
  ;      |               |           | 
  ;      v               v           v
  ;
  ;      x   x   x   x   x   o   o   o  
  ;                          
  ;          |   |   |       ^   ^   ^
  ;          |   |   `------'    |   |
  ;          |   `--------------'    |
  ;          `----------------------'
  ;
  Spectrum = [ spc, REVERSE(spc[1L:n_Spc-2L]) ]
  
  
  ; FFT the spectrum and normalise the result
  ; -----------------------------------------
  ifg = FFT(Spectrum, /DOUBLE, /INVERSE) * Compute_MeanDelta(f)
  
  ; Shift the IFG array to give an measurement-
  ; like interferogram; from
  ;
  ;                     nSpc
  ;     ZPD         (Nyquist pt)    nIfg
  ;      |               |           | 
  ;      v               v           v
  ;
  ;      x   x   x   x   x   o   o   o  
  ;
  ; to
  ;
  ;                 ZPD
  ;                  |
  ;                  v
  ;      o   o   o   x   x   x   x   x
  ;
  ; -------------------------------------------
  ifg = SHIFT(ifg,-n_Spc)
  
  
  ; Compute the optical delay grid
  ; ------------------------------
  x = Compute_X(f)
  
END
