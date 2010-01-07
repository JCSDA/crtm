;+
; Extract brightness temperatures from an RTSolution structure
; unless the Radiance keyword is set

FUNCTION Extract_RTSolution, rts, $              ; Input
                             Radiance=Radiance   ; Input keyword
;-

  ; Set up error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FAILURE
  ENDIF
  

  ; Determine the required structure tag
  tags = TAG_NAMES((*(rts[0,0])))
  IF ( KEYWORD_SET(Radiance) ) THEN $
    tag_select = 'RADIANCE' $
  ELSE $
    tag_select = 'BRIGHTNESS_TEMPERATURE'
  i = (WHERE(tags EQ tag_select))[0]

    
  ; Get structure dimensions
  Info = SIZE(rts, /STRUCTURE)
  n_Channels = Info.DIMENSIONS[0]
  n_Profiles = Info.DIMENSIONS[1]


  ; Extract out the required data
  y = DBLARR(n_Channels,n_Profiles)
  FOR m = 0, n_Profiles-1 DO BEGIN
    FOR l = 0, n_Channels-1 DO BEGIN
      y[l,m] = (*(rts[l,m])).(i)
    ENDFOR
  ENDFOR


  ; Done
  CATCH, /CANCEL
  RETURN, y
  
END
