PRO RTS_Profile::Get_Property, $
  Debug      = Debug     , $  ; Input keyword
  n_Channels = n_Channels     ; Output keyword


  ; Set up
  ; ...Set up error handler
  @rts_pro_err_handler
 
  
  ; Get data
  n_Channels = self.n_Channels

  
  ; Done
  CATCH, /CANCEL
 
END ; PRO RTS_Profile::Get_Property
