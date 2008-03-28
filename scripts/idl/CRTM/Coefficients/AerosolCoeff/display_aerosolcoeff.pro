PRO Display_AerosolCoeff

  @color_db
  
  charsize=(!D.NAME eq 'PS') ? 2.5 : 3.0
  font    =(!D.NAME eq 'PS') ? 1   : -1
  xt = 0.5
  yt = 0.96
  pixels = 5000
  
  result = read_netCDF('AerosolCoeff.nc',ac)
  
  FOR n = 0, ac.n_Types - 1 DO BEGIN
  
    SHADE_SURF, ac.ke[*,*,n], $
             ac.wavelength, $
             ac.reff[*,n], $
             XTITLE='Wavelength (micron)', $
             YTITLE='Effective radius (micron)', $
             ZTITLE='k!De!N (m!U2!N/kg)', $
             PIXELS=1000, $
             FONT=font, $
             CHARSIZE=charsize
    XYOUTS, xt, yt, 'Extinction coefficient for '+STRTRIM(ac.aerosol_type_name[n],2), $
            ALIGNMENT=0.5, /NORMAL, $
            FONT=font, $
            CHARSIZE=charsize*0.75
    q = GET_KBRD(1)
    IF ( STRUPCASE(q) EQ 'Q' ) THEN BREAK
    IF ( STRUPCASE(q) EQ 'C' ) THEN CONTINUE

    SHADE_SURF, ac.w[*,*,n], $
             ac.wavelength, $
             ac.reff[*,n], $
             XTITLE='Wavelength (micron)', $
             YTITLE='Effective radius (micron)', $
             ZTITLE='w', $
             PIXELS=1000, $
             FONT=font, $
             CHARSIZE=charsize
    XYOUTS, xt, yt, 'Single scatter albedo for '+STRTRIM(ac.aerosol_type_name[n],2), $
            ALIGNMENT=0.5, /NORMAL, $
            FONT=font, $
            CHARSIZE=charsize*0.75
    q = GET_KBRD(1)
    IF ( STRUPCASE(q) EQ 'Q' ) THEN BREAK
    IF ( STRUPCASE(q) EQ 'C' ) THEN CONTINUE

    SHADE_SURF, ac.g[*,*,n], $
             ac.wavelength, $
             ac.reff[*,n], $
             XTITLE='Wavelength (micron)', $
             YTITLE='Effective radius (micron)', $
             ZTITLE='g', $
             PIXELS=1000, $
             FONT=font, $
             CHARSIZE=charsize
    XYOUTS, xt, yt, 'Asymmetry parameter for '+STRTRIM(ac.aerosol_type_name[n],2), $
            ALIGNMENT=0.5, /NORMAL, $
            FONT=font, $
            CHARSIZE=charsize*0.75
    q = GET_KBRD(1)
    IF ( STRUPCASE(q) EQ 'Q' ) THEN BREAK
    IF ( STRUPCASE(q) EQ 'C' ) THEN CONTINUE

    FOR i = 0, ac.n_Legendre_Terms - 1 DO BEGIN
      SHADE_SURF, ac.pcoeff[*,*,n,i], $
               ac.wavelength, $
               ac.reff[*,n], $
               XTITLE='Wavelength (micron)', $
               YTITLE='Effective radius (micron)', $
               ZTITLE='P('+STRTRIM(i,2)+')', $
               PIXELS=1000, $
               FONT=font, $
               CHARSIZE=charsize
      XYOUTS, xt, yt, 'P('+STRTRIM(i,2)+') Legendre coefficient for '+$
                      STRTRIM(ac.aerosol_type_name[n],2), $
              ALIGNMENT=0.5, /NORMAL, $
              FONT=font, $
              CHARSIZE=charsize*0.75
      q = GET_KBRD(1)
      IF ( STRUPCASE(q) EQ 'Q' ) THEN GOTO, Done
      IF ( STRUPCASE(q) EQ 'C' ) THEN BREAK
    ENDFOR

  ENDFOR

  Done:
  
END
