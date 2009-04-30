; Procedure to plot SRF data within idl
; procedure that is used to create SRF
; netCDF files

PRO Plot_SRF,    plot_holder,                           $  ; structure holding plot information
                 Negative_Loc,                          $  ; location of negative responses
                 SRF=SRF,                               $  ; Optionally pass in SRF structure if passband boundaries are needed
                 multiple_microwave=multiple_microwave, $  ; option to overplot microwave frequencies
                 PS=PS
                

  @color_db
  @srf_parameters
  
  IF(plot_holder.Sensor_Type EQ MICROWAVE_SENSOR) THEN BEGIN
    XTITLE='Frequency (GHz)'
  ENDIF ELSE BEGIN 
    XTITLE='Frequency (cm!U-1!N)'
  ENDELSE
    
  ; Set default display window
  IF ( !D.NAME EQ 'X') THEN BEGIN
  xSize=800
  ySize=1000
  winID = !D.WINDOW
  IF ( winID EQ -1 ) THEN winID=0
  IF ( !D.X_SIZE NE xSize OR !D.Y_SIZE NE ySize ) THEN $
    WINDOW, winID, XSIZE=xSize, YSIZE=ySize
  ENDIF
 
  ; Check for microwave plotting    
  IF(plot_holder.Sensor_Type EQ MICROWAVE_SENSOR) THEN BEGIN
  
    ; scale to make the x axis plots large enough
    xscale=.0006d0
  
    ; Set plotting info for microwave
    thick    = (KEYWORD_SET(PS)) ?  4.5     :  4.5
    font     = (KEYWORD_SET(PS)) ?  1       :  1
    charsize = (KEYWORD_SET(PS)) ?  2.0     :  2.0
    ypos1 = (KEYWORD_SET(PS)) ? 1.42  :  1.34
    ypos2 = (KEYWORD_SET(PS)) ? 1.28  :  1.20
    ; Specify regions for plotting
    TopRegion=[0.0,0.42,0.975,0.90]
    BottomRegion=[0.0,0.0,0.975,0.40]
    !P.REGION=TopRegion
    
    IF(KEYWORD_SET(multiple_microwave)) THEN BEGIN
                   
      yRange = [-(MAX(*plot_holder.r)/20),MAX(*plot_holder.r)]
      
      ; plot the first moment derived data
      PLOT, *plot_holder.f_fm, *plot_holder.r,                                 $
             XTITLE=xtitle,                                                    $
             YTITLE='Relative Response',                                       $
             TITLE=plot_holder.Infile+': Ch.'+plot_holder.channel_name,        $
             YRANGE=yRange, /YSTYLE,                                           $
             XRANGE=[((-xscale)*(*plot_holder.f0_doc)[0]) +                    $
             MIN([*plot_holder.f_fm,*plot_holder.f_doc]),                      $
             ((xscale)*(*plot_holder.f0_doc)[0]) +                             $
              MAX([*plot_holder.f_fm,*plot_holder.f_doc])],                    $
             /xstyle,                                                          $
             FONT=font, THICK=thick, CHARSIZE=charsize, PSYM=-4
      xRange = !X.CRANGE
      
      ; overplot the document derived data
      OPLOT, *plot_holder.f_doc, *plot_holder.r, color=GREEN, psym=-1, thick=thick
      
      yLimit = MAX(*plot_holder.r)
      
      IF(plot_holder.n_bands EQ 4) THEN BEGIN
 
        ; plot the central frequencies
        FOR n = 0, plot_holder.n_bands - 1 DO BEGIN
          OPLOT, [(*plot_holder.f0_doc)[n], (*plot_holder.f0_doc)[n]],         $
          [ylimit,yRange[0]], LINESTYLE=2, THICK=thick, COLOR=GREEN
        ENDFOR
      ENDIF
                
      IF(plot_holder.n_bands EQ 2) THEN BEGIN
 
        ; plot the central frequencies
        FOR n = 0, plot_holder.n_bands - 1 DO BEGIN
          OPLOT, [(*plot_holder.f0_doc)[n], (*plot_holder.f0_doc)[n]],         $
          [ylimit,yRange[0]], LINESTYLE=2, THICK=thick, COLOR=GREEN
        ENDFOR
        
      ENDIF
      
      IF(plot_holder.n_bands EQ 1) THEN BEGIN

        ; plot the central frequencies
        FOR n = 0, plot_holder.n_bands - 1 DO BEGIN
          OPLOT, [(*plot_holder.f0_doc)[n], (*plot_holder.f0_doc)[n]],         $
          [ylimit,yRange[0]], LINESTYLE=2, THICK=thick, COLOR=GREEN
        ENDFOR
      
      ENDIF
      
      mylegend, -0.02, ypos1,                                                             $
      ['Translation to Measurement Frequencies by Using the First Moment'],               $
      color=[!P.Color], LINESTYLE=[2], THICK=[thick], CHARSIZE=CHARSIZE, FONT=font         
                     
      mylegend, -0.02, ypos2,                                                             $
      ['Translation to Measurement Frequencies by Using Documented Central Frequencies'], $
      color=[GREEN], LINESTYLE=[2], THICK=[thick], CHARSIZE=CHARSIZE, FONT=font 
      
      !P.REGION=BottomRegion
    
      yLimit = 0.01*MAX(*plot_holder.r)                                                
      yRange = [(-0.250d)*yLimit,yLimit]                                                        
                        
      TITLE='Close Up of Relative Responses < 1% of The Maximum'                           
                                                                                 
      ; The first moment derived data                                                                 
      PLOT, *plot_holder.f_fm, *plot_holder.r, /NOERASE,             $                 
            XTITLE=xtitle,                                           $                 
            YTITLE='Relative Response',                              $                 
            YRANGE=yRange,      /ystyle,                             $                 
            XRANGE=[((-xscale)*(*plot_holder.f0_doc)[0]) +           $
            MIN([*plot_holder.f_fm,*plot_holder.f_doc]),             $
            ((xscale)*(*plot_holder.f0_doc)[0]) +                    $
            MAX([*plot_holder.f_fm,*plot_holder.f_doc])],            $
            /xstyle,                                                 $                 
            TITLE=TITLE,                                             $                 
            FONT=font, THICK=thick, CHARSIZE=charsize, PSYM=-4            
      xRange = !X.CRANGE       
                                                              
      ; overplot the document derived data
      OPLOT, *plot_holder.f_doc, *plot_holder.r, THICK=thick, COLOR=GREEN, psym=-1
      OPLOT, !X.CRANGE, [0,0], LINESTYLE=2, THICK=thick                                
      !P.REGION=0     
    ENDIF ELSE BEGIN
           
      yRange = [-(MAX(*plot_holder.r)/20),MAX(*plot_holder.r)]
      
      ; The first moment derived data                                                                 
      PLOT, *plot_holder.f_fm, *plot_holder.r,                                 $               
            XTITLE=xtitle,                                                     $               
            YTITLE='Relative Response',                                        $               
            YRANGE=yRange, /ystyle,                                            $               
            XRANGE=[((-xscale)*(*plot_holder.f0_doc)[0]) +                     $
            MIN(*plot_holder.f_fm), ((xscale)*(*plot_holder.f0_doc)[0]) +      $
            MAX(*plot_holder.f_fm)],                                           $               
            /xstyle,                                                           $
            TITLE=plot_holder.Infile+': Ch.'+plot_holder.channel_name,         $               
            FONT=font, THICK=thick, CHARSIZE=charsize, PSYM=-4          
      xRange = !X.CRANGE
      
      yLimit = MAX(*plot_holder.r)
      
      IF(plot_holder.n_bands EQ 4) THEN BEGIN
        
        ; plot the central frequencies
        FOR n = 0, plot_holder.n_bands - 1 DO BEGIN
          OPLOT, [(*plot_holder.f0_doc)[n], (*plot_holder.f0_doc)[n]],       $
          [ylimit,yRange[0]], LINESTYLE=2, THICK=thick, COLOR=GREEN
        ENDFOR  
        
      ENDIF

      IF(plot_holder.n_bands EQ 2) THEN BEGIN

        ; plot the central frequencies
        FOR n = 0, plot_holder.n_bands - 1 DO BEGIN
          OPLOT, [(*plot_holder.f0_doc)[n], (*plot_holder.f0_doc)[n]],       $
          [ylimit,yRange[0]], LINESTYLE=2, THICK=thick, COLOR=GREEN
        ENDFOR

      ENDIF

      IF(plot_holder.n_bands EQ 1) THEN BEGIN
      
        ; plot the central frequencies
        FOR n = 0, plot_holder.n_bands - 1 DO BEGIN
          OPLOT, [(*plot_holder.f0_doc)[n], (*plot_holder.f0_doc)[n]],       $
          [ylimit,yRange[0]], LINESTYLE=2, THICK=thick, COLOR=GREEN
        ENDFOR

      ENDIF
      
      ; legend for overplotted central frequencies                          
      mylegend, 0.20, 1.32, ['Documented Band Central Frequency'],            $
      color=[GREEN], LINESTYLE=[2], THICK=[thick], CHARSIZE=CHARSIZE, FONT=font 

      !P.REGION=BottomRegion

      yLimit = 0.01*MAX(*plot_holder.r)                                               
      yRange = [(-0.250d)*yLimit,yLimit]                                                       
                     
      TITLE='Close Up of Relative Responses < 1% of The Maximum'                    
                                                                                        
      ; The first moment derived data                                                                  
      PLOT, *plot_holder.f_fm, *plot_holder.r, /NOERASE,                      $                  
            XTITLE=xtitle,                                                    $                  
            YTITLE='Relative Response',                                       $                  
            YRANGE=yRange,     /ystyle,                                       $                  
            XRANGE=[((-xscale)*(*plot_holder.f0_doc)[0]) +                    $
            MIN(*plot_holder.f_fm), ((xscale)*(*plot_holder.f0_doc)[0])       $
            + MAX(*plot_holder.f_fm)],                                        $
            /xstyle,                                                          $                  
            TITLE=TITLE,                                                      $                  
            FONT=font, THICK=thick, CHARSIZE=charsize, PSYM=-4            
      xRange = !X.CRANGE                                                                

      OPLOT, !X.CRANGE, [0,0], LINESTYLE=2, THICK=thick                                 
      
      !P.REGION=0
    ENDELSE 
  
  ; Start of infrared plotting
  ENDIF ELSE BEGIN
  
    ; Set plotting parameters for infrared
    thick    = (KEYWORD_SET(PS)) ? 2       :   1
    font     = (KEYWORD_SET(PS)) ? 1       :   1
    charsize = (KEYWORD_SET(PS)) ? 4.25    :   1.5
    f0color  = (KEYWORD_SET(PS)) ? BLUE    :   CYAN  
    
    yLimit = MAX(*plot_holder.orig_r)/2
    TopRegion=[0.0,0.5,0.975,1.0]
    MiddleRegion=[0.0,0.25,0.975,0.5]
    BottomRegion=[0.0,0.0,0.975,0.25]
    
    ; assign string for reporting f0 on plot
    cf0 = 'f!D0!N = '+STRTRIM(STRING(plot_holder.f0,FORMAT='(f9.3)'),2)+'cm!U-1!N'
    !P.REGION=TopRegion
    yRange = [-(MAX(*plot_holder.orig_r)/20),MAX(*plot_holder.orig_r)]
    
    PLOT, *plot_holder.orig_f, *plot_holder.orig_r, $
          XTITLE=xtitle, $
          YTITLE='Relative Response', $
          TITLE=plot_holder.Infile+': Ch.'+plot_holder.channel_name, $
          YRANGE=yRange, /YSTYLE, $
          FONT=font, THICK=thick, CHARSIZE=charsize, PSYM=-4, $
          XRANGE=[plot_holder.v1-(0.1*(plot_holder.v2-plot_holder.v1)), $
          plot_holder.v2+(0.1*(plot_holder.v2-plot_holder.v1))]
    xRange = !X.CRANGE
    
    ; The interpolated data
    OPLOT, *plot_holder.f, *plot_holder.r, THICK=thick, COLOR=RED
    
    ; The original negative data
    IF ( plot_holder.Negative_Count GT 0 ) THEN $
    OPLOT, (*plot_holder.orig_f)[Negative_Loc], (*plot_holder.orig_r)[Negative_Loc], PSYM=4, COLOR=GREEN
   
    OPLOT, [plot_holder.v1,plot_holder.v1], [yLimit,yRange[0]], LINESTYLE=2, THICK=thick, COLOR=MAGENTA    
    OPLOT, [plot_holder.v2,plot_holder.v2], [yLimit,yRange[0]], LINESTYLE=2, THICK=thick, COLOR=MAGENTA
    
    ; Other way of plotting cutoffs
    ;XYOUTS, (plot_holder.v1), yLimit, 'f1 cutoff', FONT=font, COLOR=RED, CHARSIZE=charsize, /DATA, ALIGNMENT=1.0
    ;XYOUTS, (plot_holder.v2), yLimit, 'f2 cutoff', FONT=font, COLOR=RED, CHARSIZE=charsize, /DATA, ALIGNMENT=0.0

    ; The central frequency
    OPLOT, [plot_holder.f0,plot_holder.f0], !Y.CRANGE, LINESTYLE=2, THICK=thick, COLOR=f0color
          
    mylegend, 0.7, 0.9,            $
              ['Original SRF',     $
               'Interpolated SRF', $
               cf0,                $
               'Negative input' ], $
              COLOR=[!P.COLOR,RED,f0color,GREEN], $
              LINESTYLE=[0,0,2,0], $
              PSYM=[-4,0,0,4], $
              THICK=[thick,thick,thick,thick], $
              CHARSIZE=charsize, $
              FONT=font
          
    ; Plot the SRF baseline zoom
    ; --------------------------
    !P.REGION=MiddleRegion
    yLimit = 0.01*MAX(*plot_holder.orig_r)
    yRange = [-yLimit,yLimit]
   
    ; The input data
    PLOT, *plot_holder.orig_f, *plot_holder.orig_r, /NOERASE, $
          XTITLE=xtitle, $
          YTITLE='Relative Response', $
          TITLE='SRF baseline. Area removed = '+STRING(plot_holder.Percent_Removed,FORMAT='(f9.6,"%")'), $
          YRANGE=yRange, $        
          XRANGE=[plot_holder.v1-(0.1*(plot_holder.v2-plot_holder.v1)), $
          plot_holder.v2+(0.1*(plot_holder.v2-plot_holder.v1))], $
          FONT=font, THICK=thick, CHARSIZE=charsize, PSYM=-4
    xRange = !X.CRANGE
    
    ; The interpolated data
    OPLOT, *plot_holder.f, *plot_holder.r, THICK=thick, COLOR=RED
    
    ; The original negative data
    IF ( plot_holder.Negative_Count GT 0 ) THEN $
    OPLOT, (*plot_holder.orig_f)[Negative_Loc], (*plot_holder.orig_r)[Negative_Loc], PSYM=4, COLOR=GREEN
    
    ; The frequency cutoffs
    OPLOT, [plot_holder.v1,plot_holder.v1], [0.2,yRange[0]], LINESTYLE=2, THICK=thick, COLOR=MAGENTA
    OPLOT, [plot_holder.v2,plot_holder.v2], [0.2,yRange[0]], LINESTYLE=2, THICK=thick, COLOR=MAGENTA
    OPLOT, !X.CRANGE, [0,0], LINESTYLE=2, THICK=thick

    ; Plot the frequency derivative
    ; -----------------------------
    !P.REGION=BottomRegion
    df=DERIV(*plot_holder.Orig_f)
    dfmean = MEAN(df)
    df = df - dfmean
    PLOT, *plot_holder.Orig_r, df, /YNOZERO, /NOERASE, /NODATA, $
          FONT=font, THICK=thick, CHARSIZE=charsize, $
          XRANGE=xRange,            $
          YRANGE=[MIN(df),MAX(df)], $
          XTITLE=xtitle, $
          YTITLE='dF-MEAN(dF)', $
          TITLE='Frequency derivative. MEAN(df) = '+STRING(dfmean,format='(e8.2,"cm!U-1!N")')
    OPLOT, *plot_holder.orig_f, df, COLOR=RED
    OPLOT, !X.CRANGE, [0,0], LINESTYLE=2, THICK=thick
    !P.REGION=0  
    
  ENDELSE        

END 
