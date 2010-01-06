PRO Display_FitStats, Sensor_Id, XRANGE=xRange, CHANNEL=Channel, Microwave=Microwave

  ; Set up for display
  ; ------------------
  @color_db
  pSave = !P
  !P.MULTI = [0,2,4]


  ; Set up error handler
  ; --------------------
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    !P = pSave
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN
  ENDIF

  
  ; Include parameters
  ; ------------------
  @fitstats_parameters

  
  ; Plot parameters
  ; ---------------
  Charsize  = (!D.NAME EQ 'PS') ? 2.5 : 1.75
  lCharsize = (!D.NAME EQ 'PS') ? 2.0 : 1.25
  Font      = (!D.NAME EQ 'PS') ? 1 : -1
  Thick     = (!D.NAME EQ 'PS') ? 2 :  1
  xStyle    = 1
  pSym      = KEYWORD_SET(Channel) ? 10 : 0
  zeroLine  = 2
  tickLen   = 1.0
  gridStyle = 1
  ; Special characters
  tau   = (!D.NAME EQ 'PS') ? '!9t!X' : '!4t!X'
  Delta = (!D.NAME EQ 'PS') ? '!9D!X' : '!4D!X'
  chi   = (!D.NAME EQ 'PS') ? '!9x!X' : '!4x!X'
  ; Common plot attributes
  yTitle = Delta+'T!DB!N (K)'
  ; Set default display window
  IF ( !D.NAME EQ 'X') THEN BEGIN
    xSize=800
    ySize=900
    winID = !D.WINDOW
    IF ( winID EQ -1 ) THEN winID=0
    IF ( !D.X_SIZE NE xSize OR !D.Y_SIZE NE ySize ) THEN $
      WINDOW, winID, XSIZE=xSize, YSIZE=ySize
  ENDIF
  ; Set number of absorber files
  n_Mol_Sets = KEYWORD_SET(Microwave) ? MAX_N_MOL_SETS-1L : MAX_N_MOL_SETS
  

  ; Initialise total stats
  ; ----------------------
  Total_BIAS = ZERO
  Total_RMS  = ZERO


  ; Begin molecule/absorber set loop
  ; --------------------------------
  FOR j = 0, n_Mol_Sets-1 DO BEGIN

    ; Read the current dataset
    ; ------------------------
    NC_Filename = MOL_SET_NAME[j]+'.'+STRTRIM(Sensor_Id,2)+'.FitStats.nc'
    result = Read_netCDF(NC_Filename, fs, /Quiet)
    IF ( result NE SUCCESS ) THEN $
      MESSAGE, 'Error reading FitStats file '+NC_Filename, $
               /NONAME, /NOPRINT

    ; Plot the data
    ; -------------
    id_tag = STRTRIM(Sensor_Id,2)+' '+MOL_SET_NAME[j]
    
    ; Select x-coord
    IF ( KEYWORD_SET(Channel) ) THEN BEGIN
      x = fs.Sensor_Channel
      xTitle = 'Sensor Channel'
    ENDIF ELSE BEGIN
      x = fs.Frequency
      xTitle = 'Frequency (cm!U-1!N)'
    ENDELSE
    
    ; Set xRange
    IF ( N_ELEMENTS(xRange) NE 2 ) THEN BEGIN
      xr     = [MIN(x),MAX(x)]
    ENDIF ELSE BEGIN
      xr     = xRange[SORT(xRange)]
    ENDELSE
    xloc = WHERE(x GE xr[0] AND x LE xr[1])
    
    ; The Tb residual bias
    yr = [MIN(fs.Tb_Bias[xloc]),MAX(fs.Tb_Bias[xloc])]
    Title = 'T!DB!N residual bias for '+id_tag+' fit'
    PLOT, x, fs.Tb_Bias, $
          XTITLE=xTitle, $
          YTITLE=yTitle, $
          TITLE=Title, $
          XRANGE=xr, XSTYLE=xStyle, $
          YRANGE=yr, $
          XTICKLEN=tickLen, XGRIDSTYLE=gridStyle, $
          YTICKLEN=tickLen, YGRIDSTYLE=gridStyle, $
          CHARSIZE=Charsize, $
          FONT=Font, $
          THICK=Thick, $
          /NODATA
    OPLOT, x, fs.Tb_Bias, $
           THICK=Thick, $
           PSYM=pSym, $
           COLOR=MOL_SET_COLOR[j]
    OPLOT, !X.CRANGE, [0,0], $
           THICK=Thick, $
           LINESTYLE=zeroLine, $
           COLOR=MOL_SET_COLOR[j]
    myxyouts, XLABEL, YLABEL, PLOT_LABEL[j*2], $
              CHARSIZE=lCharsize, $
              FONT=Font
          
    ; The Tb residual RMS
    yr = [MIN(fs.Tb_RMS[xloc]),MAX(fs.Tb_RMS[xloc])]
    Title = 'T!DB!N residual RMS for '+id_tag+' fit'
    PLOT, x, fs.Tb_RMS, $
          XTITLE=xTitle, $
          YTITLE=yTitle, $
          TITLE=Title, $
          XRANGE=xr, XSTYLE=xStyle, $
          YRANGE=yr, $
          XTICKLEN=tickLen, XGRIDSTYLE=gridStyle, $
          YTICKLEN=tickLen, YGRIDSTYLE=gridStyle, $
          CHARSIZE=Charsize, $
          FONT=Font, $
          THICK=Thick, $
          /NODATA
    OPLOT, x, fs.Tb_RMS, $
           THICK=Thick, $
           PSYM=pSym, $
           COLOR=MOL_SET_COLOR[j]
    myxyouts, XLABEL, YLABEL, PLOT_LABEL[j*2 + 1], $
              CHARSIZE=lCharsize, $
              FONT=Font
              
    ; Accumulate the stats
    Total_BIAS = Total_BIAS + fs.Tb_Bias
    Total_RMS  = Total_RMS  + fs.Tb_RMS^2   
    
  ENDFOR

  ; The total Tb residual bias
  yr = [MIN(Total_BIAS[xloc]),MAX(Total_BIAS[xloc])]
  Title = 'Total T!DB!N residual bias for '+STRTRIM(Sensor_Id,2)
  PLOT, x, Total_BIAS, $
        XTITLE=xTitle, $
        YTITLE=yTitle, $
        TITLE=Title, $
        XRANGE=xr, XSTYLE=xStyle, $
        YRANGE=yr, $
        XTICKLEN=tickLen, XGRIDSTYLE=gridStyle, $
        YTICKLEN=tickLen, YGRIDSTYLE=gridStyle, $
        CHARSIZE=Charsize, $
        FONT=Font, $
        THICK=Thick, $
        PSYM=pSym
  OPLOT, !X.CRANGE, [0,0], $
         THICK=Thick, $
         LINESTYLE=zeroLine
  myxyouts, XLABEL, YLABEL, PLOT_LABEL[n_Mol_Sets*2], $
            CHARSIZE=lCharsize, $
            FONT=Font

  ; The total Tb residual RMS
  Total_RMS = SQRT(Total_RMS)
  yr = [MIN(Total_RMS[xloc]),MAX(Total_RMS[xloc])]
  Title = 'Total T!DB!N residual RMS for '+STRTRIM(Sensor_Id,2)
  PLOT, x, Total_RMS, $
        XTITLE=xTitle, $
        YTITLE=yTitle, $
        TITLE=Title, $
        XRANGE=xr, XSTYLE=xStyle, $
        YRANGE=yr, $
        XTICKLEN=tickLen, XGRIDSTYLE=gridStyle, $
        YTICKLEN=tickLen, YGRIDSTYLE=gridStyle, $
        CHARSIZE=Charsize, $
        FONT=Font, $
        THICK=Thick, $
        PSYM=pSym
  myxyouts, XLABEL, YLABEL, PLOT_LABEL[n_Mol_Sets*2 + 1], $
            CHARSIZE=lCharsize, $
            FONT=Font

  ; Done
  ; ----
  CATCH, /CANCEL
  !P = pSave

END
