FUNCTION File_List::Get_Names, $
  Debug = Debug  ; Input keyword
 
  ; Set up error handler
  @file_list_err_handler

  ; Default return value
  Filenames = ['']
  
  ; Create an array for filenames
  file_entry = self->Get(/ALL, ISA='File_Entry', COUNT=n_Files)
  IF ( n_Files GT 0 ) THEN BEGIN
    Filenames = STRARR(n_Files)
    FOR n = 0L, n_Files-1L DO BEGIN
      file_entry[n]->File_Entry::Get_Property, $
        Name  = Name, $  ; Input keyword
        Debug = Debug    ; Input keyword
      Filenames[n] = Name
    ENDFOR
  ENDIF
  
  ; Done
  CATCH, /CANCEL
  RETURN, Filenames

END
