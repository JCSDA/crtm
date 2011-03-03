PRO File_Entry::Set_Property, $
  Debug = Debug, $
  Name  = Name
  
  ; Set up
  @file_list_err_handler

  ; Get data
  IF ( N_ELEMENTS(Name) GT 0 ) THEN self.Name = Name

END
