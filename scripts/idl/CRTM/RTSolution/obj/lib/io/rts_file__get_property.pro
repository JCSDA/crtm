PRO RTS_File::Get_Property, $
  Debug      = Debug     , $  ; Input keyword
  Filename   = Filename  , $  ; Output keyword
  n_Profiles = n_Profiles     ; Output keyword


  ; Set up
  ; ...Set up error handler
  @rts_pro_err_handler
 
  
  ; Get scalar data
  Filename   = self.Filename  
  n_Profiles = self.n_Profiles

  
  ; Done
  CATCH, /CANCEL
 
END ; PRO RTS_File::Get_Property
