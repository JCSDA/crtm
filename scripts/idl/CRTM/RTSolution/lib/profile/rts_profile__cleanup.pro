PRO RTS_Profile::Cleanup, $
  Debug=Debug  ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  @rts_pro_err_handler

  ; Empty container
  self->IDL_Container::Cleanup

END
