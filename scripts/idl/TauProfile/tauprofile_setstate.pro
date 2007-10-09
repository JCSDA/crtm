; -----------------------------
; Routine to set the Info state
; -----------------------------
PRO TauProfile_SetState, ID, Info, No_Copy = No_Copy
  WIDGET_CONTROL, ID, GET_UVALUE = InfoPtr
  IF ( PTR_VALID( InfoPtr ) EQ 0 ) THEN MESSAGE, 'State information pointer is invalid'
  IF ( N_ELEMENTS( Info )   EQ 0 ) THEN MESSAGE, 'State information structure is undefined'

  IF ( KEYWORD_SET( No_Copy ) ) THEN BEGIN
    *InfoPtr = TEMPORARY( Info )
  ENDIF ELSE BEGIN
    *InfoPtr = Info
  ENDELSE

  IF ( (*InfoPtr).Debug EQ 1 ) THEN PRINT, 'TauProfile_SetState'
END

