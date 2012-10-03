;+
PRO RTS_File::Inspect, $
  Verbose=Verbose, $  ; Input keyword
  Debug=Debug         ; Input keyword
;-
  ; Set up
  @rts_pro_err_handler


  ; Output info
  IF ( KEYWORD_SET(Debug) ) THEN HELP, /ROUTINES
  HELP, self, /OBJECTS

  IF ( KEYWORD_SET(Verbose) ) THEN BEGIN
    rts_profile = self->Get(/ALL, ISA='RTS_Profile', COUNT=n_profiles)
    IF ( n_profiles EQ 0 ) THEN RETURN
    FOR n = 0L, n_profiles-1L DO BEGIN
      MESSAGE, 'Inspecting RTS_Profile element #'+STRTRIM(n,2)+' ******', /INFORMATIONAL
      rts_profile[n]->RTS_Profile::Inspect, $
        Verbose=Verbose, $  ; Input keyword
        Debug=Debug         ; Input keyword
      IF ( n LT n_profiles-1L ) THEN BEGIN
        MESSAGE, 'Press <ENTER> to continue....', /INFORMATIONAL
        q = GET_KBRD(1)
        IF ( STRUPCASE(q) EQ 'Q' ) THEN BREAK
      ENDIF
    ENDFOR

  ENDIF

END
