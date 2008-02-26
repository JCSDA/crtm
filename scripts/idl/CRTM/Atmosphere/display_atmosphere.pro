;+
PRO Display_Atmosphere, Atm              , $  ; Input. Output from CRTM_Read_Atmosphere_Binary()
                        Selected_Profile , $  ; Optional input. 1 if not specified
                        Selected_Channel , $  ; Optional input. 1 if not specified
                        Title=Title      , $  ; Keyword input
                        Pressure=Pressure, $  ; Keyword input
                        xLog=xLog        , $  ; Keyword input
                        yLog=yLog             ; Keyword input
;-

  ; Set up error handler
  ; --------------------
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN
  ENDIF

  ; Save default plot info
  ; ----------------------
  psave = !P
    
  ; Process inputs
  ; --------------
  IF ( N_ELEMENTS(Selected_Profile) EQ 0 ) THEN m = 0 ELSE m = Selected_Profile-1
  IF ( N_ELEMENTS(Selected_Channel) EQ 0 ) THEN l = 0 ELSE l = Selected_Channel-1
  IF ( NOT KEYWORD_SET(Title) ) THEN Title=''
  xTickformat = KEYWORD_SET(xLog) ? 'logticks' : ''
  yTickformat = KEYWORD_SET(yLog) ? 'logticks' : ''

  ; Extract out the requested (or default) profile
  ; ----------------------------------------------
  Info = SIZE(Atm, /STRUCTURE)
  IF ( Info.N_DIMENSIONS EQ 1 ) THEN BEGIN
    n_Channels = 0
    n_Profiles = Info.DIMENSIONS[0]
    m = (m<(n_Profiles-1))>0
    a = *Atm[m]
  ENDIF ELSE BEGIN
    n_Channels = Info.DIMENSIONS[0]
    n_Profiles = Info.DIMENSIONS[1]
    m = (m<(n_Profiles-1))>0
    l = (l<(n_Channels-1))>0
    a = *Atm[l,m]
  ENDELSE
  
  ; Set the profile pressures
  ; -------------------------
  IF ( N_ELEMENTS(Pressure) GT 0 ) THEN BEGIN
    p = Pressure
  ENDIF ELSE BEGIN
    p = *a.pressure
  ENDELSE
  pRange = [1100,0.01] ;[MAX(p),MIN(p)]

  ; Define some default graphics parameters
  ; ---------------------------------------
  charSize = 1.5
  pSym = -4
  
  ; Plot the "regular" profile data
  ; -------------------------------
  ; Plot the temperature profile
  PLOT, *a.temperature, p, $
        TITLE = Title + '!CTemperature', $
        XTITLE = 'Temperature (K)', $
        YTITLE = 'Pressure (hPa)', $
        YLOG = yLog, /YSTYLE, YRANGE = pRange, $
        YTICKFORMAT = yTickformat, $
        YMARGIN = [4,4], $
        CHARSIZE = charSize, $
        PSYM = pSym
  ; Determine the number of absorber plot loops
  n = a.n_Absorbers
  nx = 2
  nPlots = (n MOD nx) EQ 0 ? n/nx : n/nx+1
  !P.MULTI = [0, nx, 1]
  ; Plot the absorber profiles
  FOR i = 0, a.n_Absorbers-1, nx DO BEGIN
    FOR i2 = 0, 1 DO BEGIN
      j = i+i2
      PLOT, (*a.absorber)[*,j], p, $
            TITLE = Title + '!CAbsorber #'+STRTRIM(j+1,2), $
            XTITLE = 'Absorber Units', $
            YTITLE = 'Pressure (hPa)', $
            XLOG = xLog, $
            XTICKFORMAT = xTickformat, $
            YLOG = yLog, /YSTYLE, YRANGE = pRange, $
            YTICKFORMAT = yTickformat, $
            YMARGIN = [4,4], $
            CHARSIZE = charSize, $
            PSYM = pSym
    ENDFOR
    
    q = GET_KBRD(1)
    CASE STRUPCASE(q) OF
      'Q': GOTO, Done
      'C': BREAK
      ELSE:
    ENDCASE
  ENDFOR


   
  ; Plot the cloud profile data
  ; ---------------------------
  !P.MULTI = [0,2,1]
  FOR n = 0, a.n_Clouds-1 DO BEGIN
    Cld = (*a.Cloud)[n]
    ; Plot the effective radius profile
    PLOT, *Cld.Effective_Radius, p, $
          TITLE = Title + '!CCloud '+STRTRIM(n+1,2)+' Effective Radius', $
          XTITLE = 'R!Deff!N (um)', $
          YTITLE = 'Pressure (hPa)', $
          YLOG = yLog, /YSTYLE, YRANGE = pRange, $
          YTICKFORMAT = yTickformat, $
          YMARGIN = [4,4], $
          CHARSIZE = charSize, $
          PSYM = pSym
    ; Plot the water content profile
    PLOT, *Cld.Water_Content, p, $
          TITLE = Title + '!CCloud '+STRTRIM(n+1,2)+' Water Content', $
          XTITLE = 'w (kg/m!U2!N)', $
          YTITLE = 'Pressure (hPa)', $
          YLOG = yLog, /YSTYLE, YRANGE = pRange, $
          YTICKFORMAT = yTickformat, $
          YMARGIN = [4,4], $
          CHARSIZE = charSize, $
          PSYM = pSym

    q = GET_KBRD(1)
    CASE STRUPCASE(q) OF
      'Q': GOTO, Done
      'C': BREAK
      ELSE:
    ENDCASE
  ENDFOR

  
  ; Plot the aerosol profile data
  ; -----------------------------
  !P.MULTI = [0,2,1]
  FOR n = 0, a.n_Aerosols-1 DO BEGIN
    Aero = (*a.Aerosol)[n]
    ; Plot the effective radius profile
    PLOT, *Aero.Effective_Radius, p, $
          TITLE = Title + '!CAerosol '+STRTRIM(n+1,2)+' Effective Radius', $
          XTITLE = 'R!Deff!N (um)', $
          YTITLE = 'Pressure (hPa)', $
          YLOG = yLog, /YSTYLE, YRANGE = pRange, $
          YTICKFORMAT = yTickformat, $
          YMARGIN = [4,4], $
          CHARSIZE = charSize, $
          PSYM = pSym
    ; Plot the concentration profile
    PLOT, *Aero.Concentration, p, $
          TITLE = Title + '!CAerosol '+STRTRIM(n+1,2)+' Concentration', $
          XTITLE = 'conc. (kg/m!U2!N)', $
          YTITLE = 'Pressure (hPa)', $
          YLOG = yLog, /YSTYLE, YRANGE = pRange, $
          YTICKFORMAT = yTickformat, $
          YMARGIN = [4,4], $
          CHARSIZE = charSize, $
          PSYM = pSym

    q = GET_KBRD(1)
    CASE STRUPCASE(q) OF
      'Q': GOTO, Done
      'C': BREAK
      ELSE:
    ENDCASE
  ENDFOR
  
  
  ; Done
  ; ----
  Done:
  !P = psave
  CATCH, /CANCEL
  
END
