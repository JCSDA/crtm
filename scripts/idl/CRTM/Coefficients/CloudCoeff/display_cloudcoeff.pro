PRO Plot_Surface, z, x, y, $
                  ZTITLE=ztitle, $
                  XTITLE=xtitle, $
                  YTITLE=ytitle, $
                  TITLE =title, $
                  PIXELS=pixels

  @color_db
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
;  PLOTS, [x[0],x[0]],[y[0],y[0]],[!Z.CRANGE[0],z[0,0]], /T3D, $
;         color=red, thick=thick
;  PLOTS, [x[n_x-1],x[n_x-1]],[y[0],y[0]],[!Z.CRANGE[0],z[n_x-1,0]], /T3D, $
;         color=red, thick=thick
;  PLOTS, [x[0],x[0]],[y[n_y-1],y[n_y-1]],[!Z.CRANGE[0],z[0,n_y-1]], /T3D, $
;         color=red, thick=thick
;  PLOTS, [x[0],x[0]],[y[n_y-1],y[0]],[!Z.CRANGE[0],!Z.CRANGE[0]], /T3D, $
;         color=red, thick=thick
;  PLOTS, [x[0],x[n_x-1]],[y[0],y[0]],[!Z.CRANGE[0],!Z.CRANGE[0]], /T3D, $
;         color=red, thick=thick
;  PLOTS, replicate(x[0],n_y),y,z[0,*], /T3D, $
;         color=red, thick=thick
;  PLOTS, x,replicate(y[0],n_x),z[*,0], /T3D, $
;         color=red, thick=thick

  xlp = 0.5
  ylp = 0.96
  XYOUTS, xlp, ylp, title, $
          /NORMAL, ALIGNMENT=0.5, $
          CHARSIZE=charsize/1.5, FONT=font
END


PRO Display_CloudCoeff, NCfile, ps=ps, pixels=pixels

  ; Plotting setup
  TVLCT, r, g, b, /GET
  @color_db
  micron = (KEYWORD_SET(ps)) ? '!9m!Xm' : '!4l!Xm'
  rho    = (KEYWORD_SET(ps)) ? '!9r!X'  : '!4q!X'
  omega  = (KEYWORD_SET(ps)) ? '!9v!X'  : '!4x!X'
  delta  = (KEYWORD_SET(ps)) ? '!9d!X'  : '!4d!X'
  

  ; Read the data
  result = Read_netCDF(NCfile,c)
help, c, /struct

  ; ========
  ; IR plots
  ; ========
  ; Set x and y plot titles
  xtitle='Frequency (cm!U-1!N)'
  ytitle='R!Deff!N ('+micron+')'
  
  ; Loop over densities
  FOR i = 0, c.N_IR_DENSITIES-1 DO BEGIN
  
    IF ( i EQ 0 ) THEN $
      density = 'liquid water' $
    ELSE $
      density = rho+'='+STRING(c.DENSITY[i-1]/1000.0d0,FORMAT='(f3.1,"g.cm!U-3!N")')

    ; Loop over variables
    FOR iVar = 1, 41 DO BEGIN
    
      ; Set up variables for plotting
      CASE iVar OF
        ; Extinction coefficient      
        1: BEGIN
             LOADCT, 1, /SILENT
             psfile = 'ke_IR.d'+STRTRIM(i,2)+'.ps'
             z = c.KE_IR[*,*,i]
             ztitle='k!De!N (m!U2!N.kg!U-1!N)'
             title='Infrared extinction coefficient for '+density
           END
        ; Single scatter albedo
        2: BEGIN
             LOADCT, 3, /SILENT
             psfile = 'w_IR.d'+STRTRIM(i,2)+'.ps'
             z = c.W_IR[*,*,i]
             ztitle=omega
             title='Infrared single scatter albedo for '+density
           END
        ; Asymmetry factor
        3: BEGIN
             LOADCT, 8, /SILENT
             psfile = 'g_IR.d'+STRTRIM(i,2)+'.ps'
             z = c.G_IR[*,*,i]
             ztitle='g'
             title='Infrared asymmetry factor for '+density
           END
        ; Phase coefficients
        ELSE: BEGIN
              LOADCT, 7, /SILENT
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
             psfile = 'pcoeff_IR.d'+STRTRIM(i,2)+'.l'+STRTRIM(lsub,2)+'.n'+STRTRIM(n_streams,2)+'.ps'
             z = c.PCOEFF_IR[*,*,i,l]
             ztitle='P!Dcoeff!N'
             title='P!D'+STRTRIM(lsub,2)+'!N coefficient for n!Dstreams!N='+STRTRIM(n_streams,2)+' and '+density
           END
      ENDCASE
      
      ; Plot the data
      IF ( KEYWORD_SET(ps) ) THEN pson, file=psfile, /quiet
      Plot_Surface, z, c.FREQUENCY_IR, c.REFF_IR, $
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
        IF ( STRUPCASE(q) EQ 'M') THEN GOTO, Microwave
        IF ( STRUPCASE(q) EQ 'N') THEN BREAK
        IF ( STRUPCASE(q) EQ 'S') THEN STOP 
      ENDELSE
    
    ENDFOR ; Variables
    
  ENDFOR ; Densities


  ; ========
  ; MW plots
  ; ========
  Microwave:
  ; Set x and y plot titles
  xtitle='Frequency (GHz)'
  ytitle='R!Deff!N ('+micron+')'
  
  ; SOLID PHASE PLOTS, NO Pcoeff
  ; ----------------------------
  ; Loop over densities
  FOR i = 0, c.N_DENSITIES-1 DO BEGIN
  
    density = rho+'='+STRING(c.DENSITY[i]/1000.0d0,FORMAT='(f3.1,"g.cm!U-3!N")')

    ; Loop over variables
    FOR iVar = 1, 3 DO BEGIN
    
      ; Set up variables for plotting
      CASE iVar OF
        ; Extinction coefficient      
        1: BEGIN
             LOADCT, 1, /SILENT
             psfile = 'ke_S_MW.d'+STRTRIM(i,2)+'.ps'
             z = c.KE_S_MW[*,*,i]
             ztitle='k!De!N (m!U2!N.kg!U-1!N)'
             title='Microwave extinction coefficient for solid phase, '+density
           END
        ; Single scatter albedo
        2: BEGIN
             LOADCT, 3, /SILENT
             psfile = 'w_S_MW.d'+STRTRIM(i,2)+'.ps'
             z = c.W_S_MW[*,*,i]
             ztitle=omega
             title='Microwave single scatter albedo for solid phase, '+density
           END
        ; Asymmetry factor
        3: BEGIN
             LOADCT, 8, /SILENT
             psfile = 'g_S_MW.d'+STRTRIM(i,2)+'.ps'
             z = c.G_S_MW[*,*,i]
             ztitle='g'
             title='Microwave asymmetry factor for solid phase, '+density
           END
      ENDCASE
      
      ; Plot the data
      IF ( KEYWORD_SET(ps) ) THEN pson, file=psfile, /quiet
      Plot_Surface, z, c.FREQUENCY_MW, c.REFF_MW, $
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
        IF ( STRUPCASE(q) EQ 'M') THEN GOTO, Solid_Pcoeff
        IF ( STRUPCASE(q) EQ 'N') THEN BREAK
        IF ( STRUPCASE(q) EQ 'S') THEN STOP 
      ENDELSE
    
    ENDFOR ; Variables
    
  ENDFOR ; Densities


  ; SOLID PHASE PLOTS, Pcoeff ONLY
  ; ------------------------------
  Solid_Pcoeff:
  LOADCT, 7, /SILENT
  ; Loop over densities
  FOR i = 0, c.N_DENSITIES-1 DO BEGIN
  
    density = rho+'='+STRING(c.DENSITY[i]/1000.0d0,FORMAT='(f3.1,"g.cm!U-3!N")')

    ; Loop over variables
    FOR l = 0, c.N_LEGENDRE_TERMS-2 DO BEGIN
    
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
      nsubscript=STRTRIM(n_streams,2)
      
      psfile = 'pcoeff_S_MW.d'+STRTRIM(i,2)+'.l'+lsubscript+'.n'+nsubscript+'.ps'
      IF ( KEYWORD_SET(ps) ) THEN pson, file=psfile, /quiet
      title='P!D'+lsubscript+'!N coefficients for solid phase, n!Dstreams!N='+nsubscript+' and '+density

      ; Loop over phase elements
;      !P.MULTI=[0,3,2]
;      !Y.OMARGIN=[0,4]
;      FOR m = 0, c.N_PHASE_ELEMENTS-1 DO BEGIN
      m=0
        msubscript = STRTRIM(m+1,2)
      
        z = c.PCOEFF_S_MW[*,*,i,l,m]
        ztitle='P!D'+lsubscript+','+msubscript+'!N'
      
        ; Plot the data
        Plot_Surface, z, c.FREQUENCY_MW, c.REFF_MW, $
                      ztitle=ztitle, $
                      xtitle=xtitle, $
                      ytitle=ytitle, $
                      title =title , $
                      pixels=pixels
;      ENDFOR
;      !Y.OMARGIN=0
;      !P.MULTI=0
      
      IF ( KEYWORD_SET(ps) ) THEN BEGIN
        psoff, /quiet
      ENDIF ELSE BEGIN
        q=GET_KBRD(1)
        IF ( STRUPCASE(q) EQ 'Q') THEN GOTO, Done
        IF ( STRUPCASE(q) EQ 'M') THEN GOTO, Liquid
        IF ( STRUPCASE(q) EQ 'N') THEN BREAK
        IF ( STRUPCASE(q) EQ 'S') THEN STOP 
      ENDELSE
    
    ENDFOR ; Variables
    
  ENDFOR ; Densities


  ; LIQUID PHASE PLOTS, NO Pcoeff
  ; -----------------------------
  Liquid:
  ; Loop over temperatures
  FOR i = 0, c.N_TEMPERATURES-1 DO BEGIN
  
    temperature = 'T='+STRING(c.TEMPERATURE[i],FORMAT='(f5.1,"K")')

    ; Loop over variables
    FOR iVar = 1, 3 DO BEGIN
    
      ; Set up variables for plotting
      CASE iVar OF
        ; Extinction coefficient      
        1: BEGIN
             LOADCT, 1, /SILENT
             psfile = 'ke_L_MW.t'+STRTRIM(i,2)+'.ps'
             z = c.KE_L_MW[*,*,i]
             ztitle='k!De!N (m!U2!N.kg!U-1!N)'
             title='Microwave extinction coefficient for liquid phase, '+temperature
           END
        ; Single scatter albedo
        2: BEGIN
             LOADCT, 3, /SILENT
             psfile = 'w_L_MW.d'+STRTRIM(i,2)+'.ps'
             z = c.W_L_MW[*,*,i]
             ztitle=omega
             title='Microwave single scatter albedo for liquid phase, '+temperature
           END
        ; Asymmetry factor
        3: BEGIN
             LOADCT, 8, /SILENT
             psfile = 'g_L_MW.d'+STRTRIM(i,2)+'.ps'
             z = c.G_L_MW[*,*,i]
             ztitle='g'
             title='Microwave asymmetry factor for liquid phase, '+temperature
           END
      ENDCASE
      
      ; Plot the data
      IF ( KEYWORD_SET(ps) ) THEN pson, file=psfile, /quiet
      Plot_Surface, z, c.FREQUENCY_MW, c.REFF_MW, $
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
        IF ( STRUPCASE(q) EQ 'M') THEN GOTO, Liquid_Pcoeff
        IF ( STRUPCASE(q) EQ 'N') THEN BREAK
        IF ( STRUPCASE(q) EQ 'S') THEN STOP 
      ENDELSE
    
    ENDFOR ; Variables
    
  ENDFOR ; Temperatures


  ; LIQUID PHASE PLOTS, Pcoeff ONLY
  ; -------------------------------
  Liquid_Pcoeff:
  LOADCT, 7, /SILENT
  ; Loop over temperatures
  FOR i = 0, c.N_TEMPERATURES-1 DO BEGIN
  
    temperature = 'T='+STRING(c.TEMPERATURE[i],FORMAT='(f5.1,"K")')

    ; Loop over variables
    FOR l = 0, c.N_LEGENDRE_TERMS-2 DO BEGIN
    
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
      nsubscript=STRTRIM(n_streams,2)
      
      psfile = 'pcoeff_L_MW.t'+STRTRIM(i,2)+'.l'+lsubscript+'.n'+nsubscript+'.ps'
      IF ( KEYWORD_SET(ps) ) THEN pson, file=psfile, /quiet
      title='P!D'+lsubscript+'!N coefficients for liquid phase, n!Dstreams!N='+nsubscript+' and '+temperature

      ; Loop over phase elements
;      !P.MULTI=[0,3,2]
;      !Y.OMARGIN=[0,4]
;      FOR m = 0, c.N_PHASE_ELEMENTS-1 DO BEGIN
      m=0
        msubscript = STRTRIM(m+1,2)
      
        z = c.PCOEFF_L_MW[*,*,i,l,m]
        ztitle='P!D'+lsubscript+','+msubscript+'!N'
      
        ; Plot the data
        Plot_Surface, z, c.FREQUENCY_MW, c.REFF_MW, $
                      ztitle=ztitle, $
                      xtitle=xtitle, $
                      ytitle=ytitle, $
                      title =title , $
                      pixels=pixels
;      ENDFOR
;      !Y.OMARGIN=0
;      !P.MULTI=0
      
      IF ( KEYWORD_SET(ps) ) THEN BEGIN
        psoff, /quiet
      ENDIF ELSE BEGIN
        q=GET_KBRD(1)
        IF ( STRUPCASE(q) EQ 'Q') THEN GOTO, Done
        IF ( STRUPCASE(q) EQ 'M') THEN GOTO, Liquid2
        IF ( STRUPCASE(q) EQ 'N') THEN BREAK
        IF ( STRUPCASE(q) EQ 'S') THEN STOP 
      ENDELSE
    
    ENDFOR ; Variables
    
  ENDFOR ; Temperatures


  ; LIQUID PHASE PLOTS, but for each Reff
  ; -------------------------------------
  Liquid2:
  ytitle='Temperature (K)'
  ; Loop over radii
  FOR i = 0, c.N_MW_RADII-1 DO BEGIN
  
    radius = 'R!Deff!N='+STRTRIM(STRING(c.REFF_MW[i],FORMAT='(f6.1)'),2) + micron

    ; Loop over variables
    FOR iVar = 1, 3 DO BEGIN
    
      ; Set up variables for plotting
      CASE iVar OF
        ; Extinction coefficient      
        1: BEGIN
             LOADCT, 1, /SILENT
             psfile = 'ke_L_MW.r'+STRTRIM(i,2)+'.ps'
             z = REFORM(c.KE_L_MW[*,i,*])
             ztitle='k!De!N (m!U2!N.kg!U-1!N)'
             title='Microwave extinction coefficient for liquid phase, '+radius
           END
        ; Single scatter albedo
        2: BEGIN
             LOADCT, 3, /SILENT
             psfile = 'w_L_MW.r'+STRTRIM(i,2)+'.ps'
             z = REFORM(c.W_L_MW[*,i,*])
             ztitle=omega
             title='Microwave single scatter albedo for liquid phase, '+radius
           END
        ; Asymmetry factor
        3: BEGIN
             LOADCT, 8, /SILENT
             psfile = 'g_L_MW.r'+STRTRIM(i,2)+'.ps'
             z = REFORM(c.G_L_MW[*,i,*])
             ztitle='g'
             title='Microwave asymmetry factor for liquid phase, '+radius
           END
      ENDCASE
      
      ; Plot the data
      IF ( KEYWORD_SET(ps) ) THEN pson, file=psfile, /quiet
      Plot_Surface, z, c.FREQUENCY_MW, c.TEMPERATURE, $
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
        IF ( STRUPCASE(q) EQ 'M') THEN GOTO, Liquid2_Pcoeff
        IF ( STRUPCASE(q) EQ 'N') THEN BREAK
        IF ( STRUPCASE(q) EQ 'S') THEN STOP 
      ENDELSE
    
    ENDFOR ; Variables
    
  ENDFOR ; Radii


  ; LIQUID PHASE PLOTS, Pcoeff ONLY for each Reff
  ; ---------------------------------------------
  Liquid2_Pcoeff:
  LOADCT, 7, /SILENT
  ; Loop over temperatures
  FOR i = 0, c.N_MW_RADII-1 DO BEGIN
  
    radius = 'R!Deff!N='+STRTRIM(STRING(c.REFF_MW[i],FORMAT='(f6.1)'),2) + micron

    ; Loop over variables
    FOR l = 0, c.N_LEGENDRE_TERMS-2 DO BEGIN
    
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
      nsubscript=STRTRIM(n_streams,2)
      
      psfile = 'pcoeff_L_MW.r'+STRTRIM(i,2)+'.l'+lsubscript+'.n'+nsubscript+'.ps'
      IF ( KEYWORD_SET(ps) ) THEN pson, file=psfile, /quiet
      title='P!D'+lsubscript+'!N coefficients for liquid phase, n!Dstreams!N='+nsubscript+' and '+radius

      ; Loop over phase elements
;      !P.MULTI=[0,3,2]
;      !Y.OMARGIN=[0,4]
;      FOR m = 0, c.N_PHASE_ELEMENTS-1 DO BEGIN
      m = 0
        msubscript = STRTRIM(m+1,2)
      
        z = REFORM(c.PCOEFF_L_MW[*,i,*,l,m])
        ztitle='P!D'+lsubscript+','+msubscript+'!N'
      
        ; Plot the data
        Plot_Surface, z, c.FREQUENCY_MW, c.TEMPERATURE, $
                      ztitle=ztitle, $
                      xtitle=xtitle, $
                      ytitle=ytitle, $
                      title =title , $
                      pixels=pixels
;      ENDFOR
;      !Y.OMARGIN=0
;      !P.MULTI=0
      
      IF ( KEYWORD_SET(ps) ) THEN BEGIN
        psoff, /quiet
      ENDIF ELSE BEGIN
        q=GET_KBRD(1)
        IF ( STRUPCASE(q) EQ 'Q') THEN GOTO, Done
        IF ( STRUPCASE(q) EQ 'N') THEN BREAK
        IF ( STRUPCASE(q) EQ 'S') THEN STOP 
      ENDELSE
    
    ENDFOR ; Variables
    
  ENDFOR ; Radii


  Done:
  TVLCT, r, g, b
  
END
