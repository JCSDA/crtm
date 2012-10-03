FUNCTION File_List::Get, $
  Debug      = Debug  , $  ; Input keyword
  Count      = Count  , $  ; Output keyword
  _REF_EXTRA = Extra       ; Keywords passed onto IDL_Container::Get
 
  ; Set up
  @file_list_err_handler

  ; Get the requested object reference(s)
  objref = self->IDL_Container::Get(COUNT = Count, _EXTRA = Extra)

  ; Done
  CATCH, /CANCEL
  RETURN, objref

END
