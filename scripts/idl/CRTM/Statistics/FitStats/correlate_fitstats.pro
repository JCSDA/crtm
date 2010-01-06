PRO Correlate_FitStats, Sensor_Id, XRANGE=xRange, CHANNEL=Channel

  ; Set up for display
  ; ------------------
  @color_db
  pSave = !P
  !P.MULTI = [0,1,4]
  ; Plot parameters
  Charsize = (!D.NAME EQ 'PS') ? 2.5 : 1.75
  Font     = (!D.NAME EQ 'PS') ? 1 : -1
  Thick    = (!D.NAME EQ 'PS') ? 2 :  1
  xStyle   = 1
  ; Special characters
  tau   = (!D.NAME EQ 'PS') ? '!9t!Xf' : '!4t!Xf'
  Delta = (!D.NAME EQ 'PS') ? '!9D!Xf' : '!4D!Xf'
  chi   = (!D.NAME EQ 'PS') ? '!9x!Xf' : '!4x!Xf'
  ; Set default display window
  IF ( !D.NAME EQ 'X') THEN WINDOW, XSIZE=800, YSIZE=900
  

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


  ; Begin molecule/absorber set loop
  ; --------------------------------
  FOR j = 0, N_MOL_SETS-1 DO BEGIN

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
      xr = [MIN(x),MAX(x)]
    ENDIF ELSE BEGIN
      xr = xRange[SORT(xRange)]
    ENDELSE
    xloc = WHERE(x GE xr[0] AND x LE xr[1])
    
    ; The polynomial order
    Title = 'Polynomial order for '+id_tag+' fit'
    PLOT, x, fs.Order, $
          XTITLE=xTitle, $
          YTITLE='Order', $
          TITLE=Title, $
          XRANGE=xr, XSTYLE=xStyle, $
          CHARSIZE=Charsize, $
          FONT=Font, $
          THICK=Thick
                   
    ; The number of predictors
    Title = 'Number of predictors for '+id_tag+' fit'
    PLOT, x, fs.n_Predictors, $
          XTITLE=xTitle, $
          YTITLE='# of predictors', $
          TITLE=Title, $
          XRANGE=xr, XSTYLE=xStyle, $
          CHARSIZE=Charsize, $
          FONT=Font, $
          THICK=Thick
                   
    ; The fit residula
    Title = 'Fit residual for '+id_tag+' fit'
    PLOT, x, fs.Fit_Residual, $
          XTITLE=xTitle, $
          YTITLE=Delta+'log('+chi+')', $
          TITLE=Title, $
          XRANGE=xr, XSTYLE=xStyle, $
          CHARSIZE=Charsize, $
          FONT=Font, $
          THICK=Thick
                   
    ; The Tb residual bias and RMS
    yr = [MIN(fs.Tb_Bias[xloc])<MIN(fs.Tb_RMS[xloc]), $
          MAX(fs.Tb_Bias[xloc])>MAX(fs.Tb_RMS[xloc])]
    Title = 'T!DB!N residual bias and RMS for '+id_tag+' fit'
    PLOT, x, fs.Tb_Bias, $
          XTITLE=xTitle, $
          YTITLE=Delta+'T!DB!N (K)', $
          TITLE=Title, $
          XRANGE=xr, XSTYLE=xStyle, $
          YRANGE=yr, $
          CHARSIZE=Charsize, $
          FONT=Font, $
          THICK=Thick
    OPLOT, x, fs.Tb_RMS, $
           THICK=Thick, $
           COLOR=RED
    
    
    IF ( j LT N_MOL_SETS-1 ) THEN BEGIN
      IF (!D.NAME EQ 'X') THEN BEGIN
        q = GET_KBRD(1)
        IF ( STRUPCASE(q) EQ 'Q' ) THEN BREAK
      ENDIF
    ENDIF
    
  ENDFOR


  ; Done
  ; ----
  CATCH, /CANCEL
  !P = pSave

END
