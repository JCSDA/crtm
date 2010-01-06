; ---------------------------------------
; Generic procedure to free an info state
; ---------------------------------------
PRO FreeState, id

  ; Get pointer
  WIDGET_CONTROL, id, GET_UVALUE=infoptr

  ; Test it
  IF ( PTR_VALID(infoptr)   EQ 0 ) THEN RETURN
  IF ( N_ELEMENTS(*infoptr) EQ 0 ) THEN MESSAGE, 'State information structure is undefined'

  ; Debug output
  loc = WHERE( TAG_NAMES((*infoptr)) EQ 'DEBUG', count )
  IF ( count GT 0 ) THEN $
    IF ( (*infoptr).debug EQ 1 ) THEN PRINT, 'DEBUG: FreeState'

  ; Free any pointer elements
  FOR i = 0, N_TAGS(*infoptr)-1 DO BEGIN
    IF ( SIZE((*infoptr).(i),/TNAME) EQ 'POINTER' ) THEN $
      IF ( PTR_VALID((*infoptr).(i)) ) THEN PTR_FREE, (*infoptr).(i)
  ENDFOR

  ; Free the state information pointer itself
  PTR_FREE, infoptr

END ; PRO FreeState
