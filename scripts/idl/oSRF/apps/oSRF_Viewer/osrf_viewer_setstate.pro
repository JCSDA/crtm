;+
; Procedure to set the OSRF_Viewer Info state

PRO OSRF_Viewer_SetState, ID  , $          ; Input
                          Info, $          ; Input
                          No_Copy=No_Copy  ; Optional input
;-

  ; Get and test pointer
  WIDGET_CONTROL, ID, GET_UVALUE = InfoPtr
  IF ( PTR_VALID( InfoPtr ) EQ 0 ) THEN $
  MESSAGE, 'State information pointer is invalid'

  ; Set state information structure
  IF ( N_ELEMENTS( Info ) EQ 0 ) THEN $
    MESSAGE, 'State information structure is undefined'
  IF ( KEYWORD_SET( No_Copy ) ) THEN BEGIN
    *InfoPtr = TEMPORARY( Info )
  ENDIF ELSE BEGIN
    *InfoPtr = Info
  ENDELSE

  ; Debug output
  IF ( (*InfoPtr).Debug EQ 1 ) THEN PRINT, 'OSRF_Viewer_SetState'

END ; PRO OSRF_Viewer_SetState
