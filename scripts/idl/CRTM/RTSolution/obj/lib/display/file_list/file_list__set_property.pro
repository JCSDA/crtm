PRO File_List::Set_Property, $
  Debug   = Debug, $  ; Input keyword
  n_Files = n_Files   ; Input keyword

  ; Set up
  @file_list_err_handler

  ; Get data
  IF ( N_ELEMENTS(n_Files) GT 0 ) THEN self.n_Files = LONG(n_Files[0])

  ; Done
  CATCH, /CANCEL

END
