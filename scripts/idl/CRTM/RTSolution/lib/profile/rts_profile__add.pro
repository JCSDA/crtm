;+
PRO RTS_Profile::Add, $
  obj               , $  ; Object to add to the RTS_Profile container
  Debug      = Debug, $  ; Input keyword
  _REF_EXTRA = Extra     ; Keywords passed onto IDL_Container::Add
;-
  ; Set up
  @rts_pro_err_handler


  ; Add object to the container
  self->IDL_Container::Add, obj, _EXTRA = Extra


  ; If the object added is an RTS_Channel object, increment the channel counter
  IF ( OBJ_ISA(obj, 'RTS_Channel') ) THEN self.n_Channels++

END
