FUNCTION RTS_File::Init, $
  Filename, $
  Debug=Debug  ; Input keyword

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
  ; ...Check input
  IF ( Valid_String(Filename) ) THEN $
    _Filename = Filename $
  ELSE $
    _Filename = 'RTSolution.bin'
 

  ; Set default values
  self.filename   = _Filename
  self.n_Profiles = 0L


  ; Done
  CATCH, /CANCEL
  RETURN, TRUE
 
END ; FUNCTION RTS_File::Init
