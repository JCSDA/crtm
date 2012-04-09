;+
PRO MWwaterCoeff::Destroy, $
  Debug = Debug  ; Input keyword
;-
  ; Set up
  @emiscoeff_pro_err_handler
 
 
  ; Reinitialise
  self.Is_Allocated = FALSE
  self.n_Angles       = 0L
  self.n_Frequencies  = 0L
  self.n_Temperatures = 0L
  self.n_Wind_Speeds  = 0L
  PTR_FREE, self.Angle      
  PTR_FREE, self.Frequency  
  PTR_FREE, self.Temperature
  PTR_FREE, self.Wind_Speed 
  PTR_FREE, self.ev         
  PTR_FREE, self.eh         
 
END
