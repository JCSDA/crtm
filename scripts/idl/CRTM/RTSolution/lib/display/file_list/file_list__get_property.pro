PRO File_List::Get_Property, $
  Debug   = Debug, $  ; Input keyword
  n_Files = n_Files   ; Output keyword

  ; Set up
  @file_list_err_handler

  ; Get data
  IF ( ARG_PRESENT(n_Files) ) THEN n_Files = self.n_Files

  ; Done
  CATCH, /CANCEL

END
