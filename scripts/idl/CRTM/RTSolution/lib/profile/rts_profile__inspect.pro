;+
PRO RTS_Profile::Inspect, $
  Verbose=Verbose, $  ; Input keyword
  Debug=Debug         ; Input keyword
;-
  ; Set up
  @rts_pro_err_handler


  ; Output info
  IF ( KEYWORD_SET(Debug) ) THEN HELP, /ROUTINES
  HELP, self, /OBJECTS

  IF ( KEYWORD_SET(Verbose) ) THEN BEGIN
    rts_channel = self->Get(/ALL, ISA='RTS_Channel', COUNT=n_channels)
    IF ( n_channels EQ 0 ) THEN RETURN
    ; Loop over contained channel objects
    FOR n = 0L, n_channels-1L DO BEGIN
      MESSAGE, 'Inspecting RTS_Channel element #'+STRTRIM(n,2)+' ******', /INFORMATIONAL
      rts_channel[n]->RTS_Channel::Inspect, $
        Verbose=Verbose, $  ; Input keyword
        Debug=Debug         ; Input keyword
    ENDFOR
  ENDIF

END
