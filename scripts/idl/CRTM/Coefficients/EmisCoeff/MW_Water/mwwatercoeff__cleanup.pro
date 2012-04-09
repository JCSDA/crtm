PRO MWwaterCoeff::Cleanup, $
  Debug = Debug  ; Input keyword
 
  ; Set up
  @emiscoeff_pro_err_handler
 
 
  ; Reinitialise
  self->Destroy, Debug = Debug
 
END
