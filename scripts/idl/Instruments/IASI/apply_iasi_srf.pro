PRO Apply_IASI_SRF, f, spc, PO2=po2

  @iasi_parameters
  @fft_parameters
  @color_db

  n_spc = N_ELEMENTS(f)
  


  ; Interpolate the input spectrum to IASI frequencies
  ; --------------------------------------------------
  ; Standard IASI frequency grid
  f_iasi   = Compute_IASI_F()
  spc_iasi = INTERPOL(spc, f, f_iasi)
  Idx      = WHERE(f_iasi GT MIN(f) AND f_iasi LT MAX(f), n_spc_iasi)
  f_iasi   = f_iasi[Idx]
  spc_iasi = spc_iasi[Idx]
  
  ; PO2 multiplied frequency grid
  f_po2   = Compute_IASI_F(PO2=po2)
  spc_po2 = INTERPOL(spc, f, f_po2)
  Idx     = WHERE(f_po2 GE MIN(f_iasi) AND f_po2 LE MAX(f_iasi), n_spc_po2)
  f_po2   = f_po2[Idx]
  spc_po2 = spc_po2[Idx]
  
  PRINT, 'No. of input SPC points:           ', n_spc
  PRINT, 'No. of IASI points:                ', n_spc_iasi
  PRINT, 'No. of IASI PO2 multiplied points: ', n_spc_po2, '(',po2,')'


  ; Compute the optical delay grids
  ; -------------------------------
  x_iasi = Compute_X(f_iasi)
  x_po2  = Compute_X(f_po2 )


  ; Compute the IASI IRF
  ; --------------------
  irf_iasi = IASI_GFT(x_iasi)
  irf_po2  = IASI_GFT(x_po2 )
  
  wplot, x_po2, irf_po2, /NEW, $
         XTITLE='Optical delay (cm)', $
         YTITLE='Relative response', $
         TITLE='IASI GFT', $
         /NODATA
  woplot, x_po2 , irf_po2 , PSYM=-5, COLOR=RED
  woplot, x_iasi, irf_iasi, PSYM=-6, COLOR=CYAN
  

  ; Convert the IRF to an SRF
  ; -------------------------
  IFGtoSPC, x_iasi , irf_iasi, $
            fs_iasi, srf_iasi
  IFGtoSPC, x_po2 , irf_po2, $
            fs_po2, srf_po2
  
  
  ; Reflect the SRF (real part only) so it is symmetrical
  ; -----------------------------------------------------
  srf_iasi = DOUBLE(srf_iasi)
  fs_iasi  = [ REVERSE(-fs_iasi[1L:n_spc_iasi-1L]),  fs_iasi ]
  srf_iasi = [ REVERSE(srf_iasi[1L:n_spc_iasi-1L]), srf_iasi ]
  srf_po2  = DOUBLE(srf_po2 )
  fs_po2   = [ REVERSE(-fs_po2[ 1L:n_spc_po2 -1L]),  fs_po2  ]
  srf_po2  = [ REVERSE(srf_po2[ 1L:n_spc_po2 -1L]), srf_po2  ]
  
  wplot, fs_po2, srf_po2, /NEW, $
         XTITLE='Frequency (cm!U-1!N)', $
         YTITLE='Relative response', $
         TITLE='IASI SRF', $
         /NODATA
  woplot, fs_po2 , srf_po2 , COLOR=RED
  woplot, fs_iasi, srf_iasi, COLOR=CYAN
  woplot, !X.CRANGE, [0,0], LINESTYLE=2, COLOR=GRAY
  
  
  ; Integrate the SRFs
  ; ------------------
  isrf_iasi = INTEGRAL(fs_iasi, srf_iasi)
  isrf_po2  = INTEGRAL(fs_po2 , srf_po2 )
  PRINT, 'IASI SRF integral:                ', isrf_iasi
  PRINT, 'IASI PO2 multiplied SRF integral: ', isrf_po2


  ; Integrate for a number of FWHM from f0
  ; --------------------------------------
  fwhm = srf_fwhm(fs_po2, srf_po2)

  FOR i = 5, 100, 5 DO BEGIN
    extent = DOUBLE(i)*fwhm
    idx = WHERE(ABS(fs_po2) LT extent, n)
    isrf = INTEGRAL(fs_po2[idx], srf_po2[idx])
    diff = 100.0d0*(isrf_po2-isrf)
    PRINT, 'Integral to ',i,' FWHMs (',n,') = ', INTEGRAL(fs_po2[idx], srf_po2[idx]), '; %d = ', diff
    IF ( diff LT 0.01 ) THEN BEGIN
      n_fwhm = i
      BREAK
    ENDIF
  ENDFOR
  
  ; Convolve the PO2 spectrum with the SRF
  ; --------------------------------------
  fs  = fs_po2[idx]
  srf = srf_po2[idx]
  
  scale2 = INTEGRAL(DINDGEN(n),srf)
  spc2   = CONVOL(spc_po2, srf, scale2, /EDGE_TRUNCATE, /CENTER)

  wplot, f, spc, /NEW, $
         XTITLE='Frequency (cm!U-1!N)', $
         YTITLE='Transmittance', $
         TITLE='IASI Convolved spectrum'
  woplot, f_po2, spc_po2, COLOR=RED
  woplot, f_po2, spc2   , COLOR=CYAN, THICK=3
  
END
