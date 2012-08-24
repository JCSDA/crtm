PRO Plot_Spectrum_and_oSRF, spectrum, $  ; Input
                            osrf    , $  ; Input
                            radiance=radiance, $
                            transmittance=transmittance, $
                            title=title, $
                            xrange=xrange, $
                            color=color, $
                            linestyle=linestyle, $
                            sym=sym, $
                            overplot=overplot, $
                            boxcar=boxcar, $
                            ps=ps, $
                            _extra=extra

  @error_codes
  @color_db
  
  ; Ordinate data to plot
  CASE 1 OF
    KEYWORD_SET(transmittance): BEGIN
      sy = spectrum.transmittance
      sytitle = 'Transmittance'
    END
    KEYWORD_SET(radiance): BEGIN
      sy = spectrum.radiance * 1.0d-07
      sytitle = 'Radiance (mW/(m!U2!N.ster.cm!U-1!N))'
    END
    ELSE: BEGIN
      v = GHz_to_Inverse_cm(spectrum.frequency)
      result = Planck_Temperature(v,spectrum.radiance,sy)
      sytitle = 'Brightness Temperaure (K)'
    END
  ENDCASE
  
  
  ; Set plotting parameters
  ; ...General
  osrf_color = KEYWORD_SET(color) ? color : !P.COLOR
  osrf_psym  = KEYWORD_SET(sym)  ? sym  : 0
  osrf_linestyle = KEYWORD_SET(linestyle)  ? linestyle  : 0
  initplot = KEYWORD_SET(overplot) ? FALSE : TRUE
  ; ...PS dependent
  Thick    = (KEYWORD_SET(PS)) ? 2    :  1
  Font     = (KEYWORD_SET(PS)) ? 1    : -1
  Charsize = (KEYWORD_SET(PS)) ? 1.35 : 1.0


  ; Initialise the plot
  IF ( initplot ) THEN BEGIN
    ; Plot spectrum
    PLOT, spectrum.frequency, sy, $
          TITLE=title, $
          XTITLE='Frequency (GHz)', $
          XRANGE=xrange, $
          YSTYLE=4, $
          XMARGIN=[10,10], $
          FONT=font, $
          THICK=thick, $
          CHARSIZE=charsize, $
          /YNOZERO, $
          _EXTRA=extra
    AXIS, YAXIS=0, $
          YTITLE=sytitle, $
          FONT=font, $
          CHARSIZE=charsize, $
          /SAVE        

    ; Create axis for oSRF data
    AXIS, YAXIS=1, $
          YRANGE=[0,1], $
          YTITLE='Relative Response', $
          FONT=font, $
          CHARSIZE=charsize, $
          /SAVE
  ENDIF
  
  ; Plot oSRF data
  FOR i = 0, N_ELEMENTS(osrf)-1 DO BEGIN
    osrf[i]->OSRF::Get_Property, n_Bands=n_Bands
    FOR Band = 1, n_Bands DO BEGIN
      osrf[i]->OSRF::Get_Property, Band, Frequency=f, Response=r, n_Points=n
      IF ( KEYWORD_SET(boxcar) ) THEN BEGIN
        f = [f[0],f,f[n-1]]
        r = [0.0d0,r,0.0d0]
      ENDIF
      OPLOT, f, r/MAX(r), $
             COLOR=osrf_color, $
             PSYM=osrf_psym, $
             LINESTYLE=osrf_linestyle, $
             THICK=thick

    ENDFOR  
  ENDFOR
  
END
