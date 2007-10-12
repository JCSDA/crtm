PRO IFGtoSPC, x, ifg, $  ; Output
              f, spc     ; Input
              
  n_Ifg = N_ELEMENTS(x)
  n_Spc = Compute_nSpc(n_Ifg)
  
  
  ; Compute the frequency grid
  ; --------------------------
  f = Compute_F(x)
  
  
  ; Shift the IFG array to give an "FFT-ready"
  ; double-sided interferogram; from
  ;
  ;                 ZPD
  ;                  |
  ;                  v
  ;      o   o   o   x   x   x   x   x
  ;
  ; to
  ;                    n_Spc
  ;     ZPD         (Nyquist pt)   n_Ifg
  ;      |               |           | 
  ;      v               v           v
  ;
  ;      x   x   x   x   x   o   o   o  
  ;
  ; ------------------------------------------
  Interferogram = SHIFT(ifg,n_Spc)


  ; FFT the interferogram and normalise
  ; -----------------------------------
  spc = FFT(Interferogram, /DOUBLE) / Compute_MeanDelta(f)
  
  ; Only keep the positive frequencies
  ; ----------------------------------
  spc = spc[0L:n_Spc-1L]

  
END
