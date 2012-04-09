;+
PRO MWwaterCoeff::Create, $
  n_Angles      , $ ; Input
  n_Frequencies , $ ; Input
  n_Temperatures, $ ; Input
  n_Wind_Speeds , $ ; Input
  Debug = Debug     ; Input keyword
;-
  ; Set up
  @emiscoeff_pro_err_handler
 
 
  ; Check input
  IF ( n_Angles       LT 1 OR $
       n_Frequencies  LT 1 OR $
       n_Temperatures LT 1 OR $
       n_Wind_Speeds  LT 1 ) THEN $
    MESSAGE, 'Input dimensions must be > 0', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
   

  ; Destroy the input
  self->Destroy, Debug=Debug


  ; Allocate the pointer components
  self.Angle       = PTR_NEW(DBLARR(n_Angles))
  self.Frequency   = PTR_NEW(DBLARR(n_Frequencies))
  self.Temperature = PTR_NEW(DBLARR(n_Temperatures))
  self.Wind_Speed  = PTR_NEW(DBLARR(n_Wind_Speeds))
  self.ev          = PTR_NEW(DBLARR(n_Angles, n_Frequencies, n_Temperatures, n_Wind_Speeds))
  self.eh          = PTR_NEW(DBLARR(n_Angles, n_Frequencies, n_Temperatures, n_Wind_Speeds))
  
    
  ; Initialise dimensions
  self.n_Angles       = n_Angles      
  self.n_Frequencies  = n_Frequencies 
  self.n_Temperatures = n_Temperatures
  self.n_Wind_Speeds  = n_Wind_Speeds 
  
  
  ; Set allocation indicator
  self.Is_Allocated = TRUE

END
