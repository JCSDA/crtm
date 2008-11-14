;+
PRO Difference_RTSolution, r1, r2, Selected_Profile, $  ; Inputs
                           Title=Title, $
                           Radiance=Radiance, $
                           xRange=xRange, $
                           Legend=Legend
;-

  @color_db
  
  ; Set up error handler
  ; --------------------
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN
  ENDIF
  
  IF ( N_ELEMENTS(Selected_Profile) EQ 0 ) THEN m = 0 ELSE m = Selected_Profile-1
  IF ( NOT KEYWORD_SET(Title) ) THEN Title=''

  Info1 = SIZE(r1, /STRUCTURE)
  Info2 = SIZE(r2, /STRUCTURE)
  n_Channels = Info1.DIMENSIONS[0]
  IF ( Info2.DIMENSIONS[0] NE n_Channels ) THEN $
    MESSAGE, 'Channel dimensions inconsistent', /NONAME, /NOPRINT
  n_Profiles = Info1.DIMENSIONS[1]
  IF ( Info2.DIMENSIONS[1] NE n_Profiles ) THEN $
    MESSAGE, 'Profile dimensions inconsistent', /NONAME, /NOPRINT
  
  m = (m<(n_Profiles-1))>0
  
  ynames  = [ 'Brightness Temperature','Radiance']
  ytitles = [ 'T!DB!N (K)', 'R (mW/m!U2!N.sr.cm!U-1!N)' ]
  n = N_ELEMENTS(ynames)
  x  = LINDGEN(n_Channels)+1
  y1 = DBLARR(n_Channels,n)
  y2 = y1
             
  FOR l = 0, n_Channels-1 DO BEGIN
    y1[l,0] = (*(r1[l,m])).Brightness_Temperature 
    y1[l,1] = (*(r1[l,m])).Radiance               
    y2[l,0] = (*(r2[l,m])).Brightness_Temperature 
    y2[l,1] = (*(r2[l,m])).Radiance               
  ENDFOR
  
  pSym1 = -4
  pSym2 = -6
  xstyle = 1
  charsize  = (!D.NAME eq 'PS') ? 1.75 : 1.25
  lcharsize = (!D.NAME eq 'PS') ? charsize : charsize
  font      = (!D.NAME eq 'PS') ? 1 : -1
  thick     = (!D.NAME eq 'PS') ? 2 :  1
  
  IF ( n_Channels GT 100 ) THEN BEGIN
    pSym1  = 0
    pSym2  = 0
    xstyle = 0
  ENDIF
  i = KEYWORD_SET(Radiance)
  IF ( N_ELEMENTS(xRange) NE 2 ) THEN $
    xRange = [1,n_Channels]
  
  loc = WHERE( x GE xRange[0] AND x LE xRange[1] )
  yRange = [MIN(y1[loc,i])<MIN(y2[loc,i]),MAX(y1[loc,i])>MAX(y2[loc,i])]
  
  !P.MULTI = [0,1,2]
  PLOT, x, y1[*,i], $
        TITLE = Title + '!C' + ynames[i], $
        XTITLE = 'Channel index', $
        YTITLE = ytitles[i], $
        XRANGE=xRange, XSTYLE=xstyle, $
        YRANGE=yRange, $
        YMARGIN = [4,4], $
        PSYM = pSym1, $
        CHARSIZE=charsize, THICK=thick, FONT=font, $
        /YNOZERO
  OPLOT, x, y2[*,i], $
         COLOR = RED, $
         THICK=thick, $
         PSYM = pSym2
  IF ( N_ELEMENTS(Legend) EQ 1 ) THEN BEGIN
    mylegend, Legend.xlp, Legend.ylp, $
              Legend.text, $
              COLOR=[!P.COLOR,RED], $
              THICK=[thick,thick], $
              PSYM=[pSym1,psym2], $
              CHARSIZE=lcharsize, FONT=font
  ENDIF
  PLOT, x, y1[*,i]-y2[*,i], $
        TITLE = Title + '!C' + ynames[i] + ' difference', $
        XTITLE = 'Channel index', $
        YTITLE = 'd'+ytitles[i], $
        XRANGE=xRange, XSTYLE=xstyle, $
        YMARGIN = [4,4], $
        PSYM = pSym1, $
        CHARSIZE=charsize, THICK=thick, FONT=font, $
        /YNOZERO
  OPLOT, !X.CRANGE, [0,0], $
         LINESTYLE=2, THICK=thick
  !P.MULTI = 0
  

  ; Done
  ; ----
  CATCH, /CANCEL
  
END
