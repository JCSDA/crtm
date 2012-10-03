FUNCTION File_Entry::Init, $
  Debug  = Debug, $    ; Input keyword
  _EXTRA = Properties  ; Keywords passed onto File_Entry::Set_Property
  
  ; Set up
  COMPILE_OPT HIDDEN
  ; ...Set up error handler
  @error_codes
  IF ( KEYWORD_SET(Debug) ) THEN BEGIN
    MESSAGE, '--> Entered.', /INFORMATIONAL
    MsgSwitch = 0
  ENDIF ELSE BEGIN
    CATCH, Error_Status
    IF ( Error_Status NE 0 ) THEN BEGIN
      CATCH, /CANCEL
      MESSAGE, !ERROR_STATE.MSG, /CONTINUE
      RETURN, FALSE
    ENDIF
    MsgSwitch = 1
  ENDELSE

  ; Set default properties
  self.Name = ''
  
  ; Set supplied properties.
  self->Set_Property, _EXTRA = Properties
  
  ; Done
  CATCH, /CANCEL
  RETURN, TRUE

END
