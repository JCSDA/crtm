PRO Display_RTSolution, Rts, Selected_Profile

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

  Info = SIZE(Rts, /STRUCTURE)
  n_Channels = Info.DIMENSIONS[0]
  n_Profiles = Info.DIMENSIONS[1]
  
  m = (m<(n_Profiles-1))>0
  
  n = 7
  r = DBLARR(n_Channels,n)
  rnames = [ 'Surface_Emissivity',$
             'Up_Radiance',$
             'Down_Radiance',$
             'Down_Solar_Radiance',$
             'Surface_Planck_Radiance',$
             'Radiance',$
             'Brightness_Temperature' ]
             
  FOR l = 0, n_Channels-1 DO BEGIN
    r[l,0] = (*(Rts[l,m])).Surface_Emissivity
    r[l,1] = (*(Rts[l,m])).Up_Radiance            
    r[l,2] = (*(Rts[l,m])).Down_Radiance          
    r[l,3] = (*(Rts[l,m])).Down_Solar_Radiance    
    r[l,4] = (*(Rts[l,m])).Surface_Planck_Radiance
    r[l,5] = (*(Rts[l,m])).Radiance               
    r[l,6] = (*(Rts[l,m])).Brightness_Temperature 
  ENDFOR
  
  !P.MULTI = [0,2,4]
  charSize = 2.0
  pSym = -4
  IF ( n_Channels GT 100 ) THEN pSym = 0
  FOR i = 0, n-1 DO BEGIN
    PLOT, r[*,i], $
          TITLE = rnames[i], $
          XTITLE = 'Channel index', $
          CHARSIZE = charSize, $
          PSYM = pSym, $
          /YNOZERO
  ENDFOR
  !P.MULTI = 0

  ; Done
  ; ----
  CATCH, /CANCEL
  
END
