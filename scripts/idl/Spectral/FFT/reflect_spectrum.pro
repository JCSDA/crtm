PRO Reflect_Spectrum, rSpc     , $  ; Input
                      rIfg     , $  ; Output
                      iIfg     , $  ; Output
                      iSpc=iSpc     ; Optional input

  n_Spc = N_ELEMENTS(rSpc)
  n_Ifg = N_ELEMENTS(rIfg)
  
  ; Load the return IFG arrays. The ASCII art below describes
  ; how the positive frequencies are reflected.
  ;
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
  ; The real part
  rIfg[0L:nSpc-1L]   = rSpc
  rIfg[nSpc:nIfg-1L] = REVERSE(rIfg[1L:n_Spc-2L])
  
  ; The imaginary part if provided.
  ; Note that the imaginary component of the spectrum is multiplied
  ; by -1. This is to make the input Hermitian so that the result
  ; is a real, asymmetric interferogram.
  IF ( N_ELEMENTS(iSpc) GT 0 ) THEN BEGIN
    iIfg[0L:nSpc-1L]   = iSpc
    iIfg[nSpc:nIfg-1L] = -1.0d0 * REVERSE(iIfg[1L:nSpc-2L])
  ENDIF ELSE BEGIN
    iIfg = 0.0d0
  ENDELSE
  
END
