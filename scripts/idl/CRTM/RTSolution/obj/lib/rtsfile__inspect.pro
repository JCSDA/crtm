PRO RTSfile::Inspect, $
  Verbose=Verbose, $  ; Input keyword
  Debug=Debug         ; Input keyword

  ; Set up
  @rts_pro_err_handler


  ; Output info
  IF ( KEYWORD_SET(Debug) ) THEN HELP, /ROUTINES
  HELP, self, /OBJECTS

END ; FUNCTION RTSfile::Inspect
