FUNCTION RTS_File::Init, $
  Debug = Debug  ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  @rts_func_err_handler

  ; Set default values
  self.Filename   = ''
  self.n_Profiles = 0L

  RETURN, TRUE

END
