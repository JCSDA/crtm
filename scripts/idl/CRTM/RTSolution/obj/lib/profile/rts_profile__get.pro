FUNCTION RTS_Profile::Get, $
  Debug      = Debug  , $  ; Input keyword
  Count      = Count  , $  ; Output keyword
  _REF_EXTRA = Extra       ; Keywords passed onto IDL_Container::Get
 
  ; Set up
  ; ...Set up error handler
  @rts_func_err_handler


  ; Get the requested object reference
  objref = self->IDL_Container::Get(COUNT = Count, _EXTRA = Extra)
  

  ; Done
  CATCH, /CANCEL
  RETURN, objref

END ; FUNCTION RTS_Profile::Get
