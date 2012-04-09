;+
PRO MWwaterCoeff::Get_Property, $
  Debug          = Debug         , $  ; Input keyword
  Release        = Release       , $  ; Output keyword
  Version        = Version       , $  ; Output keyword
  n_Angles       = n_Angles      , $  ; Output keyword
  n_Frequencies  = n_Frequencies , $  ; Output keyword
  n_Temperatures = n_Temperatures, $  ; Output keyword
  n_Wind_Speeds  = n_Wind_Speeds , $  ; Output keyword
  Angle          = Angle         , $  ; Output keyword
  Frequency      = Frequency     , $  ; Output keyword
  Temperature    = Temperature   , $  ; Output keyword
  Wind_Speed     = Wind_Speed    , $  ; Output keyword
  ev             = ev            , $  ; Output keyword
  eh             = eh                 ; Output keyword
;-

  ; Set up
  @emiscoeff_pro_err_handler
  ; ...Check the structure has been allocated
  IF ( NOT self->Associated(Debug=Debug) ) THEN $
    MESSAGE, 'Object has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
 

  ; Get data
  IF ( ARG_PRESENT(Release       ) ) THEN Release        = self.Release
  IF ( ARG_PRESENT(Version       ) ) THEN Version        = self.Version
  IF ( ARG_PRESENT(n_Angles      ) ) THEN n_Angles       = self.n_Angles      
  IF ( ARG_PRESENT(n_Frequencies ) ) THEN n_Frequencies  = self.n_Frequencies 
  IF ( ARG_PRESENT(n_Temperatures) ) THEN n_Temperatures = self.n_Temperatures
  IF ( ARG_PRESENT(n_Wind_Speeds ) ) THEN n_Wind_Speeds  = self.n_Wind_Speeds 
  IF ( ARG_PRESENT(Angle         ) ) THEN Angle          = *self.Angle      
  IF ( ARG_PRESENT(Frequency     ) ) THEN Frequency      = *self.Frequency  
  IF ( ARG_PRESENT(Temperature   ) ) THEN Temperature    = *self.Temperature
  IF ( ARG_PRESENT(Wind_Speed    ) ) THEN Wind_Speed     = *self.Wind_Speed 
  IF ( ARG_PRESENT(ev            ) ) THEN ev             = *self.ev         
  IF ( ARG_PRESENT(eh            ) ) THEN eh             = *self.eh         

END
