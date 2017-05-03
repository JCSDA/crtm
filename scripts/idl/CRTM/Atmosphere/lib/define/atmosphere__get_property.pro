;+
; NAME:
;       Atmosphere::Get_Property
;
; PURPOSE:
;       The Atmosphere::Get_Property procedure method gets the value of a property
;       or group of properties for this object.
;
; CALLING SEQUENCE:
;       Obj->[Atmosphere::]Get_Property, $
;         Debug           = Debug          , $
;         n_Layers        = n_Layers       , $
;         n_Absorbers     = n_Absorbers    , $
;         n_Clouds        = n_Clouds       , $
;         n_Aerosols      = n_Aerosols     , $
;         Climatology     = Climatology    , $
;         Absorber_ID     = Absorber_ID    , $
;         Absorber_Units  = Absorber_Units , $
;         Level_Pressure  = Level_Pressure , $
;         Pressure        = Pressure       , $
;         Temperature     = Temperature    , $
;         Absorber_Amount = Absorber_Amount, $
;         CFraction       = CFraction      , $
;         Cloud           = Cloud          , $
;         Aerosol         = Aerosol           
;
; INPUT KEYWORDS:
;       Debug:                 Set this keyword for debugging.
;                              If NOT SET => Error handler is enabled. (DEFAULT)
;                                 SET     => Error handler is disabled; Routine
;                                            traceback output is enabled.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
; OUTPUT KEYWORDS:
;       n_Layers:              The number of layers dimension of the
;                              Atmosphere data.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       n_Absorbers:           The number of absorbers dimension of the
;                              Atmosphere data.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       n_Clouds:              The number of clouds in the Atmosphere data.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       n_Aerosols:            The number of aerosols in the Atmosphere data.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Climatology:           Integer flag specifying the profile climatology.
;                              Valid climatologies are:
;                                1: Tropical              
;                                2: Midlatitude Summer    
;                                3: Midlatitude Winter    
;                                4: Subarctic Summer      
;                                5: Subarctic Winter      
;                                6: US Standard Atmosphere
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Absorber_ID:           Integer flag identifying the gaseous absorber.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Rank-1 (n_Absorbers)
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Absorber_Units         Integer flag identifying the gaseous absorber
;                              concentration units.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Rank-1 (n_Absorbers)
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Level_Pressure:        The level pressure profile for the atmosphere.
;                              UNITS:      hectoPascals (hPa)
;                              TYPE:       REAL
;                              DIMENSION:  Rank-1 (n_Layers+1)
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Pressure:              The layer pressure profile for the atmosphere.
;                              UNITS:      hectoPascals (hPa)
;                              TYPE:       REAL
;                              DIMENSION:  Rank-1 (n_Layers)
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Temperature:           The layer temperature profile for the atmosphere.
;                              UNITS:      Kelvin (K)
;                              TYPE:       REAL
;                              DIMENSION:  Rank-1 (n_Layers)
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Absorber_Amount:       The gaseous absorber concentration profiles for
;                              the atmosphere.
;                              UNITS:      Variable
;                              TYPE:       REAL
;                              DIMENSION:  Rank-2 (n_Layers x n_Absorbers)
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       CFraction:             The cloud fraction profile for the atmosphere.
;                              UNITS:      Variable
;                              TYPE:       REAL
;                              DIMENSION:  Rank-1 (n_Layers)
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Cloud:                 The list of cloud objects.
;                              UNITS:      Variable
;                              TYPE:       Cloud_List
;                              DIMENSION:  N/A
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Aerosol:               The list of aerosol objects.
;                              UNITS:      Variable
;                              TYPE:       Aerosol_List
;                              DIMENSION:  N/A
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;-

PRO Atmosphere::Get_Property, $
   Debug           = Debug          , $  ; Input keyword
   n_Layers        = n_Layers       , $  ; Output keyword
   n_Absorbers     = n_Absorbers    , $  ; Output keyword
   n_Clouds        = n_Clouds       , $  ; Output keyword
   n_Aerosols      = n_Aerosols     , $  ; Output keyword
   Climatology     = Climatology    , $  ; Output keyword
   Absorber_ID     = Absorber_ID    , $  ; Output keyword
   Absorber_Units  = Absorber_Units , $  ; Output keyword
   Level_Pressure  = Level_Pressure , $  ; Output keyword
   Pressure        = Pressure       , $  ; Output keyword
   Temperature     = Temperature    , $  ; Output keyword
   Absorber_Amount = Absorber_Amount, $  ; Output keyword
   CFraction       = CFraction      , $  ; Output keyword
   Cloud           = Cloud          , $  ; Output keyword
   Aerosol         = Aerosol             ; Output keyword
  
  ; Set up
  COMPILE_OPT HIDDEN
  @atmosphere_parameters
  @atmosphere_pro_err_handler
  ; ...Check the structure has been allocated
  IF ( ~ self->Associated(Debug=Debug) ) THEN $
    MESSAGE, 'Object has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
 

  ; Get data
  ; ...Scalars
  IF ( ARG_PRESENT(n_Layers   ) ) THEN n_Layers    = self.n_Layers   
  IF ( ARG_PRESENT(n_Absorbers) ) THEN n_Absorbers = self.n_Absorbers          
  IF ( ARG_PRESENT(n_Clouds   ) ) THEN n_Clouds    = self.n_Clouds   
  IF ( ARG_PRESENT(n_Aerosols ) ) THEN n_Aerosols  = self.n_Aerosols           
  IF ( ARG_PRESENT(Climatology) ) THEN Climatology = self.Climatology          
  ; ...Arrays
  IF ( ARG_PRESENT(Absorber_ID    ) ) THEN Absorber_ID     = (self.Absorber_ID   )[0]  
  IF ( ARG_PRESENT(Absorber_Units ) ) THEN Absorber_Units  = (self.Absorber_Units)[0]  
  IF ( ARG_PRESENT(Level_Pressure ) ) THEN Level_Pressure  = (self.Level_Pressure)[0]  
  IF ( ARG_PRESENT(Pressure       ) ) THEN Pressure        = (self.Pressure      )[0]  
  IF ( ARG_PRESENT(Temperature    ) ) THEN Temperature     = (self.Temperature   )[0]  
  IF ( ARG_PRESENT(Absorber_Amount) ) THEN Absorber_Amount = (self.Absorber      )[0]
  IF ( ARG_PRESENT(CFraction      ) ) THEN CFraction       = (self.CFraction     )[0]
  ; ...Objects  
  IF ( ARG_PRESENT(Cloud) ) THEN BEGIN
    IF ( self.n_Clouds GT 0 ) THEN Cloud = (self.Cloud)[*]
  ENDIF
  IF ( ARG_PRESENT(Aerosol) ) THEN BEGIN
    IF ( self.n_Aerosols GT 0 ) THEN Aerosol = (self.Aerosol)[*]
  ENDIF

END
