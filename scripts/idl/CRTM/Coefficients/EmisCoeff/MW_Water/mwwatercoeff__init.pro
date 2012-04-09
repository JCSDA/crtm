FUNCTION MWwaterCoeff::Init, Debug=Debug  ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  @emiscoeff_func_err_handler 


  ; Set default values
  self.Is_Allocated = FALSE
  self.Release        = 1L
  self.Version        = 1L
  self.n_Angles       = 0L
  self.n_Frequencies  = 0L
  self.n_Temperatures = 0L
  self.n_Wind_Speeds  = 0L

  RETURN, TRUE
 
END
