; Procedure to plot SRF data within idl
; procedure that is used to create SRF
; netCDF files

PRO Plot_SRF,    plot_holder,  $  ; structure holding plot information
                 Negative_Loc     ; location of negative responses

  @color_db

  ; Set default display window
  IF ( !D.NAME EQ 'X') THEN BEGIN
  xSize=800
  ySize=900
  winID = !D.WINDOW
  IF ( winID EQ -1 ) THEN winID=0
  IF ( !D.X_SIZE NE xSize OR !D.Y_SIZE NE ySize ) THEN $
    WINDOW, winID, XSIZE=xSize, YSIZE=ySize
  ENDIF

  IF ( KEYWORD_SET(PS) ) THEN pson, file=plot_holder.Sensor_Id+'/'+plot_holder.Sensor_Id+'.srf.ps'

  thick    = (KEYWORD_SET(PS)) ? 2    :  1
  font     = (KEYWORD_SET(PS)) ? 1    : -1
  charsize = (KEYWORD_SET(PS)) ? 1.25 :  1
  f0color  = (KEYWORD_SET(PS)) ? BLUE : CYAN

  TopRegion=[0.0,0.5,0.975,1.0]
  MiddleRegion=[0.0,0.25,0.975,0.5]
  BottomRegion=[0.0,0.0,0.975,0.25]

  ; assign string for reporting f0 on plot
  cf0 = 'f!D0!N = '+STRTRIM(STRING(plot_holder.f0,FORMAT='(f9.3)'),2)+'cm!U-1!N'
  !P.REGION=TopRegion
  yRange = [-(MAX(*plot_holder.orig_r)/20),MAX(*plot_holder.orig_r)]
  ; The input data
  PLOT, *plot_holder.orig_f, *plot_holder.orig_r, $
        XTITLE='Frequency (cm!U-1!N)', $
        YTITLE='Relative Response', $
        TITLE=plot_holder.Infile+': Ch.'+plot_holder.channel_name, $
        YRANGE=yRange, /YSTYLE, $
        FONT=font, THICK=thick, CHARSIZE=charsize, PSYM=-4
  xRange = !X.CRANGE
  ; The interpolated data
  OPLOT, *plot_holder.f, *plot_holder.r, THICK=thick, COLOR=RED
  ; The original negative data
  IF ( plot_holder.Negative_Count GT 0 ) THEN $
  OPLOT, (*plot_holder.orig_f)[Negative_Loc], (*plot_holder.orig_r)[Negative_Loc], PSYM=4, COLOR=GREEN
  ; The frequency cutoffs
  yLimit = MAX(*plot_holder.orig_r)/2
  OPLOT, [plot_holder.v1,plot_holder.v1], [yLimit,yRange[0]], LINESTYLE=2, THICK=thick, COLOR=RED
  XYOUTS, (plot_holder.v1+5.0d0), yLimit, 'f1 cutoff', FONT=font, COLOR=RED, CHARSIZE=charsize, /DATA, ALIGNMENT=1.0
  OPLOT, [plot_holder.v2,plot_holder.v2], [yLimit,yRange[0]], LINESTYLE=2, THICK=thick, COLOR=RED
  XYOUTS, (plot_holder.v2-5.0d0), yLimit, 'f2 cutoff', FONT=font, COLOR=RED, CHARSIZE=charsize, /DATA, ALIGNMENT=0.0

  ; The central frequency
  OPLOT, [plot_holder.f0,plot_holder.f0], !Y.CRANGE,       LINESTYLE=2, THICK=thick, COLOR=f0color

  mylegend, 0.7, 0.9, $
            ['Original SRF',$
             'Interpolated SRF',$
             cf0, $
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
  yLimit = 0.01
  yRange = [-yLimit,yLimit]
  ; The input data
  PLOT, *plot_holder.orig_f, *plot_holder.orig_r, /NOERASE, $
        XTITLE='Frequency (cm!U-1!N)', $
        YTITLE='Relative Response', $
        TITLE='SRF baseline. Area removed = '+STRING(plot_holder.Percent_Removed,FORMAT='(f9.6,"%")'), $
        YRANGE=yRange, $
        FONT=font, THICK=thick, CHARSIZE=charsize, PSYM=-4
  xRange = !X.CRANGE
  ; The interpolated data
  OPLOT, *plot_holder.f, *plot_holder.r, THICK=thick, COLOR=RED
  ; The original negative data
  IF ( plot_holder.Negative_Count GT 0 ) THEN $
  OPLOT, (*plot_holder.orig_f)[Negative_Loc], (*plot_holder.orig_r)[Negative_Loc], PSYM=4, COLOR=GREEN
  ; The frequency cutoffs
  OPLOT, [plot_holder.v1,plot_holder.v1], [0.2,yRange[0]], LINESTYLE=2, THICK=thick, COLOR=RED
  OPLOT, [plot_holder.v2,plot_holder.v2], [0.2,yRange[0]], LINESTYLE=2, THICK=thick, COLOR=RED
  OPLOT, !X.CRANGE, [0,0], LINESTYLE=2, THICK=thick


  ; Plot the frequency derivative
  ; -----------------------------
  !P.REGION=BottomRegion
  df=DERIV(*plot_holder.Orig_f)
  dfmean = MEAN(df)
  df = df - dfmean
  PLOT, *plot_holder.Orig_r, df, /YNOZERO, /NOERASE, /NODATA, $
        FONT=font, THICK=thick, CHARSIZE=charsize, $
        XRANGE=xRange, $
        YRANGE=[MIN(df),MAX(df)], $
        XTITLE='Frequency (cm!U-1!N)', $
        YTITLE='dF-MEAN(dF)', $
        TITLE='Frequency derivative. MEAN(df) = '+STRING(dfmean,format='(e8.2,"cm!U-1!N")')
  OPLOT, *plot_holder.orig_f, df, COLOR=RED
  OPLOT, !X.CRANGE, [0,0], LINESTYLE=2, THICK=thick
  !P.REGION=0          


END 
