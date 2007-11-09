PRO Reflect_SRF, f, srf
  srf   = DOUBLE(srf)
  n_srf = N_ELEMENTS(srf)
  f     = [ REVERSE(-f[1L:n_srf-1L]) , f[0L:n_srf-2L]   ]
  srf   = [ REVERSE(srf[1L:n_srf-1L]), srf[0L:n_srf-2L] ]
END

PRO Apply_IASI_GFT, f_in, spc_in, $  ; Input
                    FILTER_WIDTH=Filter_Width, $
                    PO2=po2, $  ; SPC interpolate PO2
                    N_FWHM=n_FWHM, $
                    MAXX=MaxX

  @iasi_parameters
  @fft_parameters
  @color_db


  ; Process keyword input
  ; ---------------------
  ; No of FWHM to use for SRF convolution
  Min_n_FWHM = 20L
  IF ( N_ELEMENTS(n_FWHM) EQ 0 ) THEN $
    n_FWHM = Min_n_FWHM $
  ELSE $
    n_FWHM = n_FWHM > Min_n_FWHM


  ; Save input spectrum
  ; -------------------
  f_orig   = f_in
  spc_orig = spc_in
  n_spc    = N_ELEMENTS(f_in)
  f1 = f_in[0]
  f2 = f_in[n_spc-1L]
  deltaf = f2-f1
  PRINT, 'Input spectrum f1, f2, npts: ', f1, f2, n_spc

  f0 =f_orig[n_spc/2L]

  ; The input spectral spacing
  ; --------------------------
  df = Compute_MeanDelta(f_in)


  ; Compute the cosine filter parameters
  ; ------------------------------------
  Filter   = Cos_Filter( df, FILTER_WIDTH=Filter_Width, /FLIP )
  n_Filter = N_ELEMENTS(Filter)
  d_Filter = DOUBLE(n_Filter-1L)*df


  ; Copy input spectrum into work array
  ; -----------------------------------
  f   = f_in
  spc = spc_in
  
;  minf = 800.0d0
;  maxf = 900.0d0
;  idx = WHERE(f GE minf AND f LE maxf, n_spc)
;  f   = f[idx]
;  spc = spc[idx]
  
  f1 = f[0]
  f2 = f[n_spc-1L]
  deltaf = f2-f1
  
  ; Apply the cosine filter
  if1=0L             & if2=n_Filter-1L
  ib1=n_spc-n_Filter & ib2=n_spc-1L
  
  spc[if1:if2] = spc[if1:if2]*Filter
  spc[ib1:ib2] = spc[ib1:ib2]*REVERSE(Filter)
  
  spc_in[if1:if2] = spc_in[if1:if2]*Filter
  spc_in[ib1:ib2] = spc_in[ib1:ib2]*REVERSE(Filter)

  ; The number of points to add to pad the spectrum
  ; -----------------------------------------------
;  spc = [ TEMPORARY(spc), DBLARR(n_spc-1L), DBLARR(n_spc-1L) ]
;;  f2  = f1 + TWO*deltaf
;  f2  = f1 + THREE*deltaf
;  n_spc = N_ELEMENTS(spc)
;  f = DINDGEN(n_spc)/DOUBLE(n_spc-1L)
;  f = f*(f2-f1) + f1
;  PRINT, 'Work spectrum f1, f2, npts: ', f1, f2, n_spc

  wplot, f, spc, /NEW, $
         XTITLE='Frequency (cm!U-1!N)', $
         YTITLE='Transmittance', $
         TITLE='Input spectra'
                
  
  ; Interpolate the input spectrum to a power-of-two
  ; ------------------------------------------------
  IF ( KEYWORD_SET(po2) ) THEN BEGIN
    n_po2 = Compute_NextPO2(n_spc)-1L
    n_spc = 2L^LONG(n_po2) + 1L
    PRINT, FORMAT='("Interpolating input spectrum to 2^",i2,"+1 (",i10,") points...")', n_po2, n_spc 
    f_po2 = DINDGEN(n_spc)/DOUBLE(n_spc-1L)
    f_po2 = (f2-f1)*f_po2 + f1
    spc   = INTERPOL(TEMPORARY(spc), f, f_po2)
    f     = TEMPORARY(f_po2)
  ENDIF


  ; Print out some information
  ; --------------------------  
  PRINT, 'No. of SPC points: ', n_spc
  PRINT, 'SPC dF           : ', df
  PRINT, 'SPC MaxX         : ', Compute_MaxX(f)


  ; FFT spectra to an interferogram
  ; -------------------------------
  PRINT, 'Input SPC->IFG; ', Compute_nIfg(n_spc), ' points.'
  t = SYSTIME(1)
  SPCtoIFG, f, spc, $
            x, ifg
  PRINT, '...took ', SYSTIME(1)-t, 'seconds'
  

  ; Compute the IASI IRF
  ; --------------------
  irf = IASI_GFT(x,MAXX=MaxX)
;  irf = Apod_Function(x,MAXX=MaxX,APOD_TYPE=COSINE_APOD)
  wplot, x, irf, /NEW, $
         XTITLE='Optical delay (cm)', $
         YTITLE='Relative response', $
         TITLE='IASI GFT'


  ; Apply the IASI IRF to the IFG
  ; -----------------------------
  wplot, x, DOUBLE(ifg), /NEW, $
         XTITLE='Optical delay (cm)', $
         YTITLE='IFG magnitude', $
         TITLE='Original IFG', $
         /NODATA
  woplot, x, DOUBLE(ifg)   , COLOR=RED
  woplot, x, IMAGINARY(ifg), COLOR=GREEN

  ifg = ifg*irf

  wplot, x, ifg, /NEW, $
         XTITLE='Optical delay (cm)', $
         YTITLE='IFG magnitude', $
         TITLE='Apodised IFG', $
         /NODATA
  woplot, x, ifg, COLOR=CYAN

  
  ; Only keep up to 2cm of the original spectrum IFG
  ; ------------------------------------------------
  idx = WHERE(ABS(x) LE TWO, n)
  idx = idx[1L:n-1L]
  x   = x[idx]
  ifg = ifg[idx]
  irf = irf[idx]


  ; FFT apodised IFGs to a spectrum
  ; -------------------------------
  PRINT, 'Input IFG->SPC; ', N_ELEMENTS(x), ' points.'
  t = SYSTIME(1)
  IFGtoSPC, x, ifg, $
            f, spc
  PRINT, '...took ', SYSTIME(1)-t, 'seconds'
  f = f1 + f


  ; Convert the IRF to an SRF
  ; -------------------------
  IFGtoSPC, x , irf, $
            fs, srf
  

  ; Reflect the SRF (real part only) so it is symmetrical
  ; -----------------------------------------------------
  Reflect_SRF, fs, srf
  
  wplot, fs, DOUBLE(srf), /NEW, $
         XTITLE='Frequency (cm!U-1!N)', $
         YTITLE='Relative response', $
         TITLE='IASI SRF'
  woplot, fs, IMAGINARY(srf), COLOR=MAGENTA
  woplot, !X.CRANGE, [0,0], LINESTYLE=2, COLOR=GRAY
  
  
  ; Integrate for a number of FWHM from f0
  ; --------------------------------------
  isrf = INTEGRAL(fs, srf)
  PRINT, 'SRF integral: ', isrf
  fwhm = srf_fwhm(fs, srf)
  PRINT, 'SRF FWHM: ', fwhm
  extent = DOUBLE(n_FWHM)*fwhm
  idx = WHERE(ABS(fs) LT extent, n)
  isrft = INTEGRAL(fs[idx], srf[idx])
  diff = 100.0d0*(ABS(isrf-isrft))
  PRINT, 'SRF Integral to ',n_FWHM,' FWHMs (',extent,') = ', isrft, '; %d = ', diff


  ; Convolve the spectrum with the SRF
  ; --------------------------------------
  PRINT, 'SRF convolve...'
  t = SYSTIME(1)
  Convolve_SPC, f_in, spc_in, $
               fs, srf, $
               cf, cspc
  PRINT, '...took ', SYSTIME(1)-t, 'seconds'
  
  ; Plot all the spectra up to this point
  ; -------------------------------------
;  yrange = [MIN(spc)<MIN(spc_po2), MAX(spc)>MAX(spc_po2) ]
  yrange = [MIN(spc), MAX(spc)]
  wplot, f_orig, spc_orig, /NEW, $
         XTITLE='Frequency (cm!U-1!N)', $
         YTITLE='Transmittance', $
         TITLE='Apodised IASI layer transmittance data', $
         XRANGE=[min(f),max(f)], $
         YRANGE=yrange
  woplot, f , DOUBLE(spc)   , COLOR=RED, thick=3
  woplot, f , IMAGINARY(spc), COLOR=MAGENTA
;  woplot, cf , cspc         , COLOR=CYAN, thick=3

;  woplot, fs+f0, srf*df,psym=4, color=magenta

;  wplot, f, cspc-DOUBLE(spc), /new, $
;         XTITLE='Frequency (cm!U-1!N)', $
;         YTITLE='SPC', $
;         TITLE='SPC(Convolved)-SPC(FFT)'
;         
;  ; Plot the SRF difference
;  fs = fs[idx]+f0
;  srf = srf[idx]/100.0d0
;  loc = where(f ge fs[0] and f le fs[n-1])
;  wplot, f[loc] , srf-DOUBLE(spc[loc]), /new, $
;         XTITLE='Frequency (cm!U-1!N)', $
;         YTITLE='dSRF', $
;         TITLE ='SRF(FFT(irf)) - SRF(FFT(irf*FFT(delta)))'
;
;  ; Plot the sinc terms         
;  vi=f0
;  v =f[loc]
;  L =1.96750d0;NOMINAL_MAXX_IN_CM
;  xm=TWO*!dpi*L*(v-vi)
;  xp=TWO*!dpi*L*(v+vi)
;  sincxm = sinc(xm)
;  sincxp = sinc(xp)
;  wplot, v, sincxm, /new, $
;         XTITLE='Frequency (cm!U-1!N)', $
;         YTITLE='SINC(2.PI.L.(v-v!Di!N))', $
;         TITLE ='(v-v!Di!N) Sinc term'
;  wplot, v, sincxp, /new, $
;         XTITLE='Frequency (cm!U-1!N)', $
;         YTITLE='SINC(2.PI.L.(v+v!Di!N))', $
;         TITLE ='(v+v!Di!N) Sinc term'
;
;  ; Compare the sinc(v+vi) and dSRF terms
;  dsrf= srf - DOUBLE(spc[loc])
;  dsrf=dsrf/max(dsrf)
;  sxp = sincxp/max(sincxp)
;  wplot, v, sxp, /new, $
;         XTITLE='Frequency (cm!U-1!N)', $
;         YTITLE='Normalised magnitude', $
;         TITLE ='Comparison of (v+v!Di!N) Sinc and d(SRF) terms'
;  woplot, v, dsrf, color=5

END
