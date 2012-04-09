;+
PRO RTS_Profile::Get_Property, $
  Debug      = Debug     , $  ; Input keyword
  n_Channels = n_Channels     ; Output keyword
;-

  ; Set up
  @rts_pro_err_handler


  ; Get data
  IF ( ARG_PRESENT(n_Channels) ) THEN n_Channels = self.n_Channels

END
