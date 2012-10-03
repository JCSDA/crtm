;+
PRO RTS_File::Get_Property, $
  Debug      = Debug     , $  ; Input keyword
  Filename   = Filename  , $  ; Output keyword
  n_Profiles = n_Profiles     ; Output keyword
;-

  ; Set up
  @rts_pro_err_handler


  ; Get data
  IF ( ARG_PRESENT(Filename  ) ) THEN Filename   = self.Filename
  IF ( ARG_PRESENT(n_Profiles) ) THEN n_Profiles = self.n_Profiles

END
