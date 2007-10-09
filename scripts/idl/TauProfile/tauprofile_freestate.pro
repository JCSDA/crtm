; ------------------------------
; Routine to free the Info state
; ------------------------------
PRO TauProfile_FreeState, ID
  WIDGET_CONTROL, ID, GET_UVALUE = InfoPtr
  IF ( PTR_VALID( InfoPtr ) EQ 0 ) THEN RETURN
  IF ( (*InfoPtr).Debug EQ 1 ) THEN PRINT, 'TauProfile_FreeState'

  ; Free the TauProfile data pointer
  PTR_FREE, (*InfoPtr).TauProfile

  ; Free state information pointer
  PTR_FREE, InfoPtr
END

