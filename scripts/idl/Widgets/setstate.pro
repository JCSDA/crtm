; --------------------------------------
; Generic procedure to set an info state
; --------------------------------------
PRO SetState, id, $            ; Input
              info, $          ; Input
              NO_COPY=no_copy  ; Optional input

  ; Get pointer
  WIDGET_CONTROL, id, GET_UVALUE = infoptr

  ; Test it
  IF ( PTR_VALID(infoptr) EQ 0 ) THEN MESSAGE, 'State information pointer is invalid'
  IF ( N_ELEMENTS(info)   EQ 0 ) THEN MESSAGE, 'State information structure is undefined'

  ; Set state information structure
  IF ( KEYWORD_SET(no_copy) ) THEN $
    *infoptr = TEMPORARY(info) $
  ELSE $
    *infoptr = info

  ; Debug output
  loc = WHERE( TAG_NAMES((*infoptr)) EQ 'DEBUG', count )
  IF ( count GT 0 ) THEN $
    IF ( (*infoptr).debug EQ 1 ) THEN PRINT, 'DEBUG: SetState'

END ; PRO SetState

