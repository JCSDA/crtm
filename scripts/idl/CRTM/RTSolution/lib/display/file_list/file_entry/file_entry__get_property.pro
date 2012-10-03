PRO File_Entry::Get_Property, $
  Debug = Debug, $
  Name  = Name
  
  ; Set up
  @file_list_err_handler

  ; Get data
  IF ( ARG_PRESENT(Name) ) THEN Name = self.Name

END
