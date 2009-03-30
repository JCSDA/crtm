; Procedure to plot SRF data within idl
; procedure that is used to create SRF
; netCDF files

PRO Plot_SRF,    plot_holder,  $  ; structure holding plot information
                 Negative_Loc     ; location of negative responses

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

  ; output to ps if keyword is set
  IF ( KEYWORD_SET(PS) ) THEN pson, file=plot_holder.Sensor_Id+'/'+plot_holder.Sensor_Id+'.srf.ps'

  ; Set plotting parameters
  thick    = (KEYWORD_SET(PS)) ? 2      :  1
  font     = (KEYWORD_SET(PS)) ? 1      :  1
  charsize = (KEYWORD_SET(PS)) ? 4.25   :  1.5
  f0color  = (KEYWORD_SET(PS)) ? BLUE   :  CYAN
      
  IF(plot_holder.Sensor_Type EQ MICROWAVE_SENSOR) THEN BEGIN
    
    ; Specify regions for plotting
    TopRegion=[0.0,0.20,0.975,0.85]
    BottomRegion=[0.0,0.0,0.975,0.20]
        
    !P.REGION=TopRegion
    yRange = [-(MAX(*plot_holder.r)/20),MAX(*plot_holder.r)]
    
    PLOT, *plot_holder.f_fm, *plot_holder.r,                                 $
           XTITLE=xtitle,                                                    $
           YTITLE='Relative Response',                                       $
           TITLE=plot_holder.Infile+': Ch.'+plot_holder.channel_name,        $
           YRANGE=yRange, /YSTYLE,                                           $
           XRANGE=[MIN(*plot_holder.f_fm),MAX(*plot_holder.f_fm)], /XSTYLE,  $
           FONT=font, THICK=thick, CHARSIZE=charsize, PSYM=-4
    xRange = !X.CRANGE
    
    OPLOT, *plot_holder.f_hm, *plot_holder.r, color=RED, psym=-2, linestyle=6
    OPLOT, *plot_holder.f_doc, *plot_holder.r, color=GREEN, psym=-1, linestyle=6
    
    yLimit = MAX(*plot_holder.r)/2
;    
    IF(plot_holder.n_bands EQ 4) THEN BEGIN
;    
;      ; plot the half max values 
;      FOR n = 0, (plot_holder.n_bands*2) - 1 DO BEGIN
;        OPLOT, [(*plot_holder.hmv)[n],(*plot_holder.hmv)[n]], [ylimit,yRange[0]], LINESTYLE=2, THICK=thick, COLOR=MAGENTA
;      ENDFOR
;      
;      ; plot the central frequencies
      FOR n = 0, plot_holder.n_bands - 1 DO BEGIN
        OPLOT, [(*plot_holder.f0_doc)[n], (*plot_holder.f0_doc)[n]], [ylimit*2,yRange[0]], LINESTYLE=6, THICK=thick, COLOR=GREEN
        OPLOT, [(*plot_holder.f0_hm)[n] , (*plot_holder.f0_hm)[n]],   [ylimit*2,yRange[0]], LINESTYLE=4, THICK=thick, COLOR=RED
        OPLOT, [(*plot_holder.f0_fm)[n] , (*plot_holder.f0_fm)[n]],   [ylimit*2,yRange[0]], LINESTYLE=2, THICK=thick, COLOR=!P.COLOR
      ENDFOR
    ENDIF
;              
    IF(plot_holder.n_bands EQ 2) THEN BEGIN
;          
;      ; plot the half max values
;      FOR n = 0, (plot_holder.n_bands*2) - 1 DO BEGIN
;        OPLOT, [(*plot_holder.hmv)[n],(*plot_holder.hmv)[n]], [ylimit,yRange[0]], LINESTYLE=2, THICK=thick, COLOR=MAGENTA
;      ENDFOR
;      
;      ; plot the central frequencies
      FOR n = 0, plot_holder.n_bands - 1 DO BEGIN
        OPLOT, [(*plot_holder.f0_doc)[n], (*plot_holder.f0_doc)[n]], [ylimit*2,yRange[0]], LINESTYLE=6, THICK=thick, COLOR=GREEN
        OPLOT, [(*plot_holder.f0_hm)[n], (*plot_holder.f0_hm)[n]],   [ylimit*2,yRange[0]], LINESTYLE=4, THICK=thick, COLOR=RED
        OPLOT, [(*plot_holder.f0_fm)[n], (*plot_holder.f0_fm)[n]], [ylimit*2,yRange[0]], LINESTYLE=2, THICK=thick, COLOR=!P.COLOR
      ENDFOR
;      
    ENDIF
    
    IF(plot_holder.n_bands EQ 1) THEN BEGIN
;      ; plot the half max values
;      FOR n = 0, (plot_holder.n_bands*2) - 1 DO BEGIN
;        OPLOT, [(*plot_holder.hmv)[n],(*plot_holder.hmv)[n]], [ylimit,yRange[0]], LINESTYLE=2, THICK=thick, COLOR=MAGENTA
;      ENDFOR
;      
;      ; plot the central frequencies
      FOR n = 0, plot_holder.n_bands - 1 DO BEGIN
        OPLOT, [(*plot_holder.f0_doc)[n], (*plot_holder.f0_doc)[n]], [ylimit*2,yRange[0]], LINESTYLE=6, THICK=thick, COLOR=GREEN
        OPLOT, [(*plot_holder.f0_hm)[n], (*plot_holder.f0_hm)[n]],   [ylimit*2,yRange[0]], LINESTYLE=4, THICK=thick, COLOR=RED
        OPLOT, [(*plot_holder.f0_fm)[n], (*plot_holder.f0_fm)[n]], [ylimit*2,yRange[0]], LINESTYLE=2, THICK=thick, COLOR=!P.COLOR
      ENDFOR
;      
    ENDIF
     
    ;mylegend, 0.45, 1.30, ['Relative Response Data'], color=[!P.COLOR], LINESTYLE=[0], PSYM=[-4], THICK=[thick], CHARSIZE=CHARSIZE, FONT=font
;    mylegend, 0.45, 1.25, ['f!D0!N Calculated As First Moment of Passband Response Data'], color=[!P.COLOR], LINESTYLE=[2], PSYM=[0], THICK=[thick], CHARSIZE=CHARSIZE, FONT=font 
;    mylegend, 0.45, 1.20, ['f!D0!N Calculated From Average of Half Max Points'], color=[GREEN], LINESTYLE=[2], PSYM=[0], THICK=[thick], CHARSIZE=CHARSIZE, FONT=font 
;    mylegend, 0.45, 1.15, ['f!D0!N As Documented'], color=[Red], LINESTYLE=[2], PSYM=[0], THICK=[thick], CHARSIZE=CHARSIZE, FONT=font                         
;    mylegend, 0.45, 1.10, ['Half Max Points'], color=[Magenta], LINESTYLE=[2], PSYM=[0], THICK=[thick], CHARSIZE=CHARSIZE, FONT=font 
    mylegend, 0.21, 1.20, ['Measured Frequencies Obtained Using the First Moment'], color=[!P.Color], PSYM=[-4], THICK=[thick], CHARSIZE=CHARSIZE, FONT=font
    mylegend, 0.21, 1.15, ['Measured Frequencies Obtained Using Passband Averages of Half Max Points'], color=[Red], PSYM=[-2], THICK=[thick], CHARSIZE=CHARSIZE, FONT=font                         
    mylegend, 0.21, 1.10, ['Measured Frequencies Obtained Using Documented Central Frequencies'], color=[GREEN], PSYM=[-1], THICK=[thick], CHARSIZE=CHARSIZE, FONT=font 
        
    !P.REGION=BottomRegion
    
    yLimit = 0.01*MAX(*plot_holder.r)
    yRange = [-yLimit,yLimit]
    IF(plot_holder.Sensor_Type EQ MICROWAVE_SENSOR) THEN BEGIN
      TITLE='Close Up of Relative Responses < 1% of The Maximum'
    ENDIF
    ; The input data
    PLOT, *plot_holder.f_fm, *plot_holder.r, /NOERASE,             $
          XTITLE=xtitle,                                           $
          YTITLE='Relative Response',                              $
          YRANGE=yRange,                                           $        
          XRANGE=[MIN(*plot_holder.f_fm),MAX(*plot_holder.f_fm)],  $
          TITLE=TITLE,                                             $
          FONT=font, THICK=thick, CHARSIZE=charsize, PSYM=-4, linestyle=2
    xRange = !X.CRANGE
    ; The interpolated data
    OPLOT, *plot_holder.f_hm, *plot_holder.r, THICK=thick, COLOR=RED, linestyle=4
    OPLOT, *plot_holder.f_doc, *plot_holder.r, THICK=thick, COLOR=GREEN, linestyle=6
    OPLOT, !X.CRANGE, [0,0], LINESTYLE=2, THICK=thick
    ; The original negative data
    
    ; The frequency cutoffs
    
    
    ; Plot the frequency derivative
    ; -----------------------------
    
    
    
  ;           'f0 Using half width bounds',             $
;             'f0 Using first moment of total SRF',     $
;             'f0 As Documented',                       $
;             'half width bounds'                   ], $
;            COLOR=[!P.COLOR,Cyan,Green,Blue,Magenta],   $
;            LINESTYLE=[0,2,2,2,2],                        $
;            PSYM=[-4,0,0,0,0],                           $
;            THICK=[thick,thick,thick,thick,thick],    $
;            CHARSIZE=charsize,                              $
;            FONT=font
  
  
  ENDIF ELSE BEGIN
  
    yLimit = MAX(*plot_holder.orig_r)/2
    TopRegion=[0.0,0.5,0.975,1.0]
    MiddleRegion=[0.0,0.25,0.975,0.5]
    BottomRegion=[0.0,0.0,0.975,0.25]
    
    ; assign string for reporting f0 on plot
    cf0 = 'f!D0!N = '+STRTRIM(STRING(plot_holder.f0,FORMAT='(f9.3)'),2)+'cm!U-1!N'
    !P.REGION=TopRegion
    yRange = [-(MAX(*plot_holder.orig_r)/20),MAX(*plot_holder.orig_r)]
    ; The input data

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
    ; The frequency cutoffs
    ;STOP
    OPLOT, [plot_holder.v1,plot_holder.v1], [yLimit,yRange[0]], LINESTYLE=2, THICK=thick, COLOR=GREEN
    ;XYOUTS, (plot_holder.v1), yLimit, 'f1 cutoff', FONT=font, COLOR=RED, CHARSIZE=charsize, /DATA, ALIGNMENT=1.0
    OPLOT, [plot_holder.v2,plot_holder.v2], [yLimit,yRange[0]], LINESTYLE=2, THICK=thick, COLOR=GREEN
    ;XYOUTS, (plot_holder.v2), yLimit, 'f2 cutoff', FONT=font, COLOR=RED, CHARSIZE=charsize, /DATA, ALIGNMENT=0.0

    ; The central frequency
    OPLOT, [plot_holder.f0,plot_holder.f0], !Y.CRANGE,       LINESTYLE=2, THICK=thick, COLOR=f0color
    ; plot half maximum 
    ; yhm=MAX(*plot_holder.r)/2
    ; PLOTS, plot_holder.half_maximum[0], yhm, psym=-4, COLOR=magenta
    ; PLOTS, plot_holder.half_maximum[1], yhm, psym=-4, COLOR=magenta
        
  
  
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
    OPLOT, [plot_holder.v1,plot_holder.v1], [0.2,yRange[0]], LINESTYLE=2, THICK=thick, COLOR=GREEN
    OPLOT, [plot_holder.v2,plot_holder.v2], [0.2,yRange[0]], LINESTYLE=2, THICK=thick, COLOR=GREEN
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
