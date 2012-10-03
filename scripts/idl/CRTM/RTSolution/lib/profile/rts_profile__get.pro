;+
FUNCTION RTS_Profile::Get, $
  Debug      = Debug  , $  ; Input keyword
  Count      = Count  , $  ; Output keyword
  _REF_EXTRA = Extra       ; Keywords passed onto IDL_Container::Get
;-
  ; Set up
  @rts_func_err_handler


  ; Get the requested object reference
  RETURN, self->IDL_Container::Get(COUNT = Count, _EXTRA = Extra)

END
