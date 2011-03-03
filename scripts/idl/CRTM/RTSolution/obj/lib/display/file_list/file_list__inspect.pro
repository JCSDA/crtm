PRO File_List::Inspect, $
  Verbose=Verbose, $  ; Input keyword
  Debug=Debug         ; Input keyword

  IF ( KEYWORD_SET(Debug) ) THEN HELP, /ROUTINES
  HELP, self, /OBJECTS

  ; Output info on each contained File_Entry object
  IF ( KEYWORD_SET(Verbose) ) THEN BEGIN

    file_entry = self->Get(/ALL, ISA='File_Entry', COUNT=n_Files)
    IF ( n_Files EQ 0 ) THEN RETURN
    
    ; Loop over contained File_Entry objects
    FOR n = 0L, n_Files-1L DO BEGIN
      MESSAGE, 'Inspecting File_Entry element #'+STRTRIM(n,2)+' ******', /INFORMATIONAL
      file_entry[n]->File_Entry::Inspect, $
        Verbose=Verbose, $  ; Input keyword
        Debug=Debug         ; Input keyword
      IF ( n LT n_Files-1L ) THEN BEGIN
        q = GET_KBRD(1)
        IF ( STRUPCASE(q) EQ 'Q' ) THEN BREAK
      ENDIF
    ENDFOR
    
  ENDIF 
  
END
