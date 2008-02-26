  ; Set up error handler for CRTM Binary I/O procedures
  ; ---------------------------------------------------
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    HELP, /LAST_MESSAGE
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    FREE_LUN, FileID
    RETURN, FAILURE
  ENDIF
