PRO Plot_Line, z, x, y, $
               ZTITLE=ztitle, $
               XTITLE=xtitle, $
               YTITLE=ytitle, $
               TITLE =title, $
               PIXELS=pixels

  @color_db
  colors
  charsize = (!D.NAME EQ 'PS') ? 1.5 : 1.0
  font     = (!D.NAME EQ 'PS') ? 1 : -1
  thick    = (!D.NAME EQ 'PS') ? 2 :  1
  psym = -4

  n_x = N_ELEMENTS(x)
  n_y = N_ELEMENTS(y)
  
  ; Plot X vs Z for every Y
  FOR i = 0L, n_y-1L DO BEGIN
    PLOT, x, z[*,i], $
          XTITLE=xtitle, $
          YTITLE=ztitle, $
          TITLE=title + ' for '+ytitle+' = '+STRING(y[i],FORMAT='(e13.6)'), $
          CHARSIZE=charsize, $
          FONT=font, $
          THICK=thick, $
          PSYM=psym
    q=GET_KBRD(1)
    IF ( STRUPCASE(q) EQ 'Q') THEN GOTO, Done
    IF ( STRUPCASE(q) EQ 'N') THEN BREAK
    IF ( STRUPCASE(q) EQ 'S') THEN STOP 
  ENDFOR

  ; Plot Y vs Z for every X
  FOR i = 0L, n_x-1L DO BEGIN
    PLOT, y, z[i,*], $
          XTITLE=ytitle, $
          YTITLE=ztitle, $
          TITLE=title + ' for '+xtitle+' = '+STRING(x[i],FORMAT='(e13.6)'), $
          CHARSIZE=charsize, $
          FONT=font, $
          THICK=thick, $
          PSYM=psym
    q=GET_KBRD(1)
    IF ( STRUPCASE(q) EQ 'Q') THEN GOTO, Done
    IF ( STRUPCASE(q) EQ 'N') THEN BREAK
    IF ( STRUPCASE(q) EQ 'S') THEN STOP 
  ENDFOR
  
  Done:
  
END


PRO Plot_Surface, z, x, y, $
                  ZTITLE=ztitle, $
                  XTITLE=xtitle, $
                  YTITLE=ytitle, $
                  TITLE =title, $
                  PIXELS=pixels

  @color_db
  colors
  charsize = 3.0
  font     = (!D.NAME EQ 'PS') ? 1 : -1
  thick    = (!D.NAME EQ 'PS') ? 2 :  1

  n_x = N_ELEMENTS(x)
  n_y = N_ELEMENTS(y)
  
  SHADE_SURF, z, x, y, $
              XTITLE=xtitle, $
              YTITLE=ytitle, $
              ZTITLE=ztitle, $
              /SAVE, $
              PIXELS=pixels, $
              CHARSIZE=charsize, $
              FONT=font, $
              THICK=thick
  PLOTS, [x[0],x[0]],[y[0],y[0]],[!Z.CRANGE[0],z[0,0]], /T3D, $
         color=red, thick=thick
  PLOTS, [x[n_x-1],x[n_x-1]],[y[0],y[0]],[!Z.CRANGE[0],z[n_x-1,0]], /T3D, $
         color=red, thick=thick
  PLOTS, [x[0],x[0]],[y[n_y-1],y[n_y-1]],[!Z.CRANGE[0],z[0,n_y-1]], /T3D, $
         color=red, thick=thick
  PLOTS, [x[0],x[0]],[y[n_y-1],y[0]],[!Z.CRANGE[0],!Z.CRANGE[0]], /T3D, $
         color=red, thick=thick
  PLOTS, [x[0],x[n_x-1]],[y[0],y[0]],[!Z.CRANGE[0],!Z.CRANGE[0]], /T3D, $
         color=red, thick=thick
  PLOTS, replicate(x[0],n_y),y,z[0,*], /T3D, $
         color=red, thick=thick
  PLOTS, x,replicate(y[0],n_x),z[*,0], /T3D, $
         color=red, thick=thick
  xlp = 0.5
  ylp = 0.96
  XYOUTS, xlp, ylp, title, $
          /NORMAL, ALIGNMENT=0.5, $
          CHARSIZE=charsize/1.5, FONT=font
END


PRO Display_AerosolCoeff, NCfile, ps=ps, pixels=pixels

  ; Plotting setup
  TVLCT, r, g, b, /GET
  @color_db
  micron = (KEYWORD_SET(ps)) ? '!9m!Xm' : '!4l!Xm'
  rho    = (KEYWORD_SET(ps)) ? '!9r!X'  : '!4q!X'
  omega  = (KEYWORD_SET(ps)) ? '!9v!X'  : '!4x!X'
  delta  = (KEYWORD_SET(ps)) ? '!9d!X'  : '!4d!X'
  

  ; Read the data
  result = Read_netCDF(NCfile,a)

  ; Set x and y plot titles
  xtitle='Frequency (cm!U-1!N)'
  ytitle='R!Deff!N ('+micron+')'

  ; Output plotting information
  PRINT, FORMAT='("DISPLAY_AEROSOLCOEFF: <Enter>-continue. Q-quit. N-next. S-stop. L-plot line.")'
  
  ; Loop over aerosol types
  FOR i = 0, a.N_TYPES-1 DO BEGIN
  
    aerosol = STRTRIM(a.AEROSOL_TYPE_NAME[i],2)
    
    ; Loop over variables
    FOR iVar = 1, 41 DO BEGIN
    
      ; Set up variables for plotting
      CASE iVar OF
        ; Extinction coefficient      
        1: BEGIN
             LOADCT, 1, /SILENT
             psfile = 'ke.a'+STRTRIM(i+1,2)+'.ps'
             z = a.KE[*,*,i]
             ztitle='k!De!N (m!U2!N.kg!U-1!N)'
             title=aerosol+' extinction coefficient'
           END
        ; Single scatter albedo
        2: BEGIN
             LOADCT, 3, /SILENT
             psfile = 'w.a'+STRTRIM(i+1,2)+'.ps'
             z = a.W[*,*,i]
             ztitle=omega
             title=aerosol+' single scatter albedo'
           END
        ; Asymmetry factor
        3: BEGIN
             LOADCT, 8, /SILENT
             psfile = 'g.a'+STRTRIM(i+1,2)+'.ps'
             z = a.G[*,*,i]
             ztitle='g'
             title=aerosol+' asymmetry factor'
           END
        ; Phase coefficients
        ELSE: BEGIN
              LOADCT, 0, /SILENT
             l = iVar-4
             IF ( l LT 5 ) THEN BEGIN
               lsub=l
               n_streams=4
             ENDIF
             IF ( l GE 5 AND l LT 12 ) THEN BEGIN
               lsub=l-5
               n_streams=6
             ENDIF
             IF ( l GE 12 AND l LT 21 ) THEN BEGIN
               lsub=l-12
               n_streams=8
             ENDIF
             IF ( l GE 21 ) THEN BEGIN
               lsub=l-21
               n_streams=16
             ENDIF
             lsubscript=STRTRIM(lsub,2)
             psfile = 'pcoeff.a'+STRTRIM(i+1,2)+'.l'+STRTRIM(lsub,2)+'.n'+STRTRIM(n_streams,2)+'.ps'
             z = a.PCOEFF[*,*,i,l]
             ztitle='P!Dcoeff!N'
             title=aerosol+' P!D'+STRTRIM(lsub,2)+'!N coefficient for n!Dstreams!N='+STRTRIM(n_streams,2)
           END
      ENDCASE
      
      ; Plot the data
      IF ( KEYWORD_SET(ps) ) THEN pson, file=psfile, /quiet
      Plot_Surface, z, a.FREQUENCY, a.REFF[*,i], $
                    ztitle=ztitle, $
                    xtitle=xtitle, $
                    ytitle=ytitle, $
                    title =title , $
                    pixels=pixels
      IF ( KEYWORD_SET(ps) ) THEN BEGIN
        psoff, /quiet
      ENDIF ELSE BEGIN
        q=GET_KBRD(1)
        IF ( STRUPCASE(q) EQ 'Q') THEN GOTO, Done
        IF ( STRUPCASE(q) EQ 'N') THEN BREAK
        IF ( STRUPCASE(q) EQ 'S') THEN STOP 
        IF ( STRUPCASE(q) EQ 'L') THEN Plot_Line, z, a.FREQUENCY, a.REFF[*,i], $
                                                  ztitle=ztitle, $
                                                  xtitle=xtitle, $
                                                  ytitle=ytitle, $
                                                  title =title
      ENDELSE
    
    ENDFOR ; Variables
    
  ENDFOR ; Types

  Done:
  TVLCT, r, g, b
  
END
