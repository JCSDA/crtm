;
;                 ZPD                Max X
;                  |                   |
;                  v                   v 
;
;   -3   -2   -1   0   +1   +2   +3   +4
;
;                  `------------------'
;                            |
;                          n_Spc
;
;   `---------------------------------'
;                    |
;                 n_Ifg
;
FUNCTION Compute_IASI_X
  @iasi_parameters
  n_Ifg = N_FFT
  n_Spc = Compute_nSpc(n_Ifg)
  x = DINDGEN(n_Spc)/DOUBLE(n_Spc-1L)
  x = x * NOMINAL_MAXX_IN_CM
  RETURN, [ REVERSE(-x[1L:n_Spc-2L]),x ]
END
