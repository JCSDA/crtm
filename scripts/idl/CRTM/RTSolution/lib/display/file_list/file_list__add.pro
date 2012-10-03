PRO File_List::Add, $
  obj, $
  Debug      = Debug, $  ; Input keyword
  _REF_EXTRA = Extra     ; Keywords passed onto IDL_Container::Add
 
  ; Set up error handler
  @file_list_err_handler

  ; Add object to the container
  self->IDL_Container::Add, obj, _EXTRA = Extra
  
  ; If the object added is a File_Entry object, increment the file counter
  IF ( OBJ_ISA(obj, 'File_Entry') ) THEN BEGIN
    self->Get_Property, n_Files = n_Files
    n_Files++
    self->Set_Property, n_Files = n_Files
  ENDIF

  ; Done
  CATCH, /CANCEL

END
