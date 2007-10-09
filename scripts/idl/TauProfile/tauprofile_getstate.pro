; -----------------------------
; Routine to get the Info state
; -----------------------------
PRO TauProfile_GetState, ID, Info, No_Copy = No_Copy
  WIDGET_CONTROL, ID, GET_UVALUE = InfoPtr
  IF ( PTR_VALID( InfoPtr )   EQ 0 ) THEN MESSAGE, 'State Information pointer is invalid'
  IF ( N_ELEMENTS( *InfoPtr ) EQ 0 ) THEN MESSAGE, 'State information structure is undefined'

  IF ( KEYWORD_SET( No_Copy ) ) THEN BEGIN
    Info = TEMPORARY( *InfoPtr )
  ENDIF ELSE BEGIN
    Info = *InfoPtr
  ENDELSE

  IF ( Info.Debug EQ 1 ) THEN PRINT, 'TauProfile_GetState'
END

