; --------------------------------------
; Generic procedure to get an info state
; --------------------------------------
PRO GetState, id, $             ; Input
              info, $           ; Output
              NO_COPY=no_copy   ; Optional input

  ; Get pointer
  WIDGET_CONTROL, id, GET_UVALUE=infoptr

  ; Test it
  IF ( PTR_VALID(infoptr)   EQ 0 ) THEN MESSAGE, 'State information pointer is invalid'
  IF ( N_ELEMENTS(*infoptr) EQ 0 ) THEN MESSAGE, 'State information structure is undefined'

  ; Get state information structure
  IF ( KEYWORD_SET(no_copy) ) THEN $
    info = TEMPORARY(*infoptr) $
  ELSE $
    info = *infoptr

  ; Debug output
  loc = WHERE( TAG_NAMES(info) EQ 'DEBUG', count )
  IF ( count GT 0 ) THEN $
    IF ( info.debug EQ 1 ) THEN PRINT, 'DEBUG: GetState'

END ; PRO GetState
