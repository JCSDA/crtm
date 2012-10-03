PRO File_Entry::Inspect, $
  Verbose=Verbose, $  ; Input keyword
  Debug=Debug         ; Input keyword

  IF ( KEYWORD_SET(Debug) ) THEN HELP, /ROUTINES
  HELP, self, /OBJECTS

  IF ( KEYWORD_SET(Verbose) ) THEN BEGIN
    ; ...For future use...
  ENDIF 
  
END
