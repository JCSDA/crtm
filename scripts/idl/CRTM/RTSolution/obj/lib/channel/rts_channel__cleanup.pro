PRO RTS_Channel::Cleanup, $
  Debug=Debug  ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  @rts_pro_err_handler


  ; Reinitialise
  self->Destroy, Debug=Debug

END
