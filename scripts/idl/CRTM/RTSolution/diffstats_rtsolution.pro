;+
PRO DiffStats_RTSolution, r1, r2, $  ; Inputs
                          Title=Title, $
                          Radiance=Radiance, $
                          xRange=xRange, $
                          Hist=Hist
;-

  @color_db
  
  ; Set up error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN
  ENDIF
  
  IF ( NOT KEYWORD_SET(Title) ) THEN Title=''

  Info1 = SIZE(r1, /STRUCTURE)
  Info2 = SIZE(r2, /STRUCTURE)
  n_Channels = Info1.DIMENSIONS[0]
  IF ( Info2.DIMENSIONS[0] NE n_Channels ) THEN $
    MESSAGE, 'Channel dimensions inconsistent', /NONAME, /NOPRINT
  n_Profiles = Info1.DIMENSIONS[1]
  IF ( Info2.DIMENSIONS[1] NE n_Profiles ) THEN $
    MESSAGE, 'Profile dimensions inconsistent', /NONAME, /NOPRINT
  
  
  ynames  = [ 'Brightness Temperature','Radiance']
  ytitles = [ 'dT!DB!N (K)', 'dR (mW/m!U2!N.sr.cm!U-1!N)' ]
  n = N_ELEMENTS(ynames)
  x  = LINDGEN(n_Channels)+1
  dy_avg = DBLARR(n_Channels,n)
  dy_rms = dy_avg

  FOR m = 0L, n_Profiles-1 DO BEGIN
    FOR l = 0L, n_Channels-1 DO BEGIN
      dt = (*(r1[l,m])).Brightness_Temperature - (*(r2[l,m])).Brightness_Temperature
      dr = (*(r1[l,m])).Radiance - (*(r2[l,m])).Radiance
      
      dy_avg[l,0] = dy_avg[l,0] + dt
      dy_avg[l,1] = dy_avg[l,1] + dr
      
      dy_rms[l,0] = dy_rms[l,0] + dt^2
      dy_rms[l,1] = dy_rms[l,1] + dr^2
    ENDFOR
  ENDFOR
  dy_avg = dy_avg / DOUBLE(n_Profiles)
  dy_rms = SQRT(dy_rms / DOUBLE(n_Profiles))
    
  pSym1 = ( n_Channels GT 100 ) ? 0 : -4
  pSym2 = ( n_Channels GT 100 ) ? 0 : -6
  pSym3 = KEYWORD_SET(Hist) ? 10 : pSym1
  xticks = KEYWORD_SET(Hist) ? n_Channels-1 : 0
  xminor = KEYWORD_SET(Hist) ? 1 : 0
  xstyle = ( n_Channels GT 100 ) ? 0 : 1
  mcharsize = KEYWORD_SET(NoMulti) ? 1.5 : 1.0
  charsize  = ((!D.NAME eq 'PS') ? 1.75 : 1.25) * mcharsize
  lcharsize = (!D.NAME eq 'PS') ? charsize : charsize
  font      = (!D.NAME eq 'PS') ? 1 : -1
  thick     = (!D.NAME eq 'PS') ? 2 :  1

  i = KEYWORD_SET(Radiance)
  IF ( N_ELEMENTS(xRange) NE 2 ) THEN $
    xRange = [1,n_Channels]
  loc = WHERE( x GE xRange[0] AND x LE xRange[1] )
  
  !P.MULTI = [0,1,2]

  yRange = [MIN(dy_avg[loc,i]),MAX(dy_avg[loc,i])]
  PLOT, x, dy_avg[*,i], $
        TITLE = Title + '!C Average ' + ynames[i] + ' difference', $
        XTITLE = 'Channel index', $
        YTITLE = ytitles[i], $
        XRANGE=xRange, XSTYLE=xstyle, XTICKS=xticks, XMINOR=xminor, $
        YRANGE=yRange, $
        YMARGIN = [4,4], $
        PSYM = pSym1, $
        CHARSIZE=charsize, THICK=thick, FONT=font, $
        /YNOZERO
  OPLOT, !X.CRANGE, [0,0], $
         LINESTYLE=2, THICK=thick

  yRange = [0.0,MAX(dy_rms[loc,i])]
  PLOT, x, dy_rms[*,i], $
        TITLE = Title + '!C RMS ' + ynames[i] + ' difference', $
        XTITLE = 'Channel index', $
        YTITLE = ytitles[i], $
        XRANGE=xRange, XSTYLE=xstyle, XTICKS=xticks, XMINOR=xminor, $
        YMARGIN = [4,4], $
        PSYM = pSym3, $
        CHARSIZE=charsize, THICK=thick, FONT=font, $
        /YNOZERO
 

  ; Done
  CATCH, /CANCEL
  
END
