;+
; NAME:
;       Atmosphere::Set_Property
;
; PURPOSE:
;       The Atmosphere::Set_Property procedure method sets the value of a property
;       or group of properties for this object.
;
; CALLING SEQUENCE:
;       Obj->[Atmosphere::]Set_Property, $
;         Climatology     = Climatology    , $
;         Absorber_ID     = Absorber_ID    , $
;         Absorber_Units  = Absorber_Units , $
;         Level_Pressure  = Level_Pressure , $
;         Pressure        = Pressure       , $
;         Temperature     = Temperature    , $
;         Absorber_Amount = Absorber_Amount, $
;         CFraction       = CFraction      , $
;         Cloud           = Cloud          , $
;         Aerosol         = Aerosol        , $
;         Debug           = Debug             
;
; INPUT KEYWORDS:
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
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Absorber_ID:           Integer flag identifying the gaseous absorber.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Rank-1 (n_Absorbers)
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Absorber_Units         Integer flag identifying the gaseous absorber
;                              concentration units.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Rank-1 (n_Absorbers)
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Level_Pressure:        The level pressure profile for the atmosphere.
;                              UNITS:      hectoPascals (hPa)
;                              TYPE:       REAL
;                              DIMENSION:  Rank-1 (n_Layers+1)
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Pressure:              The layer pressure profile for the atmosphere.
;                              UNITS:      hectoPascals (hPa)
;                              TYPE:       REAL
;                              DIMENSION:  Rank-1 (n_Layers)
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Temperature:           The layer temperature profile for the atmosphere.
;                              UNITS:      Kelvin (K)
;                              TYPE:       REAL
;                              DIMENSION:  Rank-1 (n_Layers)
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Absorber_Amount:       The gaseous absorber concentration profiles for
;                              the atmosphere.
;                              UNITS:      Variable
;                              TYPE:       REAL
;                              DIMENSION:  Rank-2 (n_Layers x n_Absorbers)
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       CFraction:             The cloud fraction profile for the atmosphere.
;                              UNITS:      Variable
;                              TYPE:       REAL
;                              DIMENSION:  Rank-1 (n_Layers)
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Cloud:                 A list of cloud objects.
;                              UNITS:      Variable
;                              TYPE:       LIST
;                              DIMENSION:  N/A
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Aerosol:               An list of aerosol objects.
;                              UNITS:      Variable
;                              TYPE:       LIST
;                              DIMENSION:  N/A
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Debug:                 Set this keyword for debugging.
;                              If NOT SET => Error handler is enabled. (DEFAULT)
;                                 SET     => Error handler is disabled; Routine
;                                            traceback output is enabled.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
; INCLUDE FILES:
;       atmosphere_parameters: Include file for atmosphere specific parameters.
;
;       atmosphere_pro_err_handler: Include file for error handling.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 13-Dec-2010
;                       paul.vandelst@noaa.gov
;
;-

PRO Atmosphere::Set_Property, $
  Climatology     = Climatology    , $  ; Input keyword
  Absorber_ID     = Absorber_ID    , $  ; Input keyword
  Absorber_Units  = Absorber_Units , $  ; Input keyword
  Level_Pressure  = Level_Pressure , $  ; Input keyword
  Pressure        = Pressure       , $  ; Input keyword
  Temperature     = Temperature    , $  ; Input keyword
  Absorber_Amount = Absorber_Amount, $  ; Input keyword
  CFraction       = CFraction      , $  ; Input keyword
  Cloud           = Cloud          , $  ; Input keyword
  Aerosol         = Aerosol        , $  ; Input keyword
  Debug           = Debug               ; Input keyword
  
  
  ; Set up
  COMPILE_OPT HIDDEN
  @atmosphere_parameters
  @atmosphere_pro_err_handler
  ; ...Check the structure has been allocated
  IF ( ~ self->Associated(Debug=Debug) ) THEN $
    MESSAGE, 'Object has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
 

  ; Get the object dimension for checking
  self->Get_Property, $
    n_Layers    = n_layers   , $
    n_Absorbers = n_absorbers, $
    Debug = Debug


  ; Check the input data arrays first
  n = N_ELEMENTS(Absorber_Id)
  IF ( n GT 0 ) THEN BEGIN
    IF ( n NE n_absorbers ) THEN $
      MESSAGE, 'Size of input Absorber_Id different from Atmosphere allocation.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    (self.Absorber_Id).Add, Absorber_Id
  ENDIF
  n = N_ELEMENTS(Absorber_Units)
  IF ( n GT 0 ) THEN BEGIN
    IF ( n NE n_absorbers ) THEN $
      MESSAGE, 'Size of input Absorber_Units different from Atmosphere allocation.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    (self.Absorber_Units).Add, Absorber_Units
  ENDIF
  n = N_ELEMENTS(Level_Pressure)
  IF ( n GT 0 ) THEN BEGIN
    IF ( n NE n_layers+1L ) THEN $
      MESSAGE, 'Size of input Level_Pressure different from Atmosphere allocation.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    (self.Level_Pressure).Add, Level_Pressure
  ENDIF
  n = N_ELEMENTS(Pressure)
  IF ( n GT 0 ) THEN BEGIN
    IF ( n NE n_layers ) THEN $
      MESSAGE, 'Size of input Pressure different from Atmosphere allocation.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    (self.Pressure).Add, Pressure
  ENDIF
  n = N_ELEMENTS(Temperature)
  IF ( n GT 0 ) THEN BEGIN
    IF ( n NE n_layers ) THEN $
      MESSAGE, 'Size of input Temperature different from Atmosphere allocation.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    (self.Temperature).Add, Temperature
  ENDIF
  absorber_info = SIZE(Absorber_Amount,/STRUCTURE)
  IF ( absorber_info.N_ELEMENTS GT 0 ) THEN BEGIN
    IF ( absorber_info.N_DIMENSIONS NE 2 ) THEN $
      MESSAGE, 'Must specify at least two absorber species.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    IF ( absorber_info.DIMENSIONS[0] NE n_layers ) THEN $
      MESSAGE, 'Size of input Absorber different from Atmosphere allocation.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    (self.Absorber).Add, Absorber_Amount
  ENDIF
  n = N_ELEMENTS(CFraction)
  IF ( n GT 0 ) THEN BEGIN
    IF ( n NE n_layers ) THEN $
      MESSAGE, 'Size of input CFraction different from Atmosphere allocation.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    (self.CFraction).Add, CFraction
  ENDIF
  
  
  ; And now the scalars
  IF ( N_ELEMENTS(Climatology) GT 0 ) THEN BEGIN
    _climatology = LONG(Climatology[0])
    IF ( _climatology LT 1 OR _climatology GT N_VALID_CLIMATOLOGY_MODELS ) THEN BEGIN
      MESSAGE, 'Valid climatologies:', /INFORMATIONAL
      FOR n = 1, N_VALID_CLIMATOLOGY_MODELS DO $
        MESSAGE, STRING(n,CLIMATOLOGY_MODEL_NAME[n], FORMAT='(2x,i2,") ",a)'), /INFORMATIONAL
      MESSAGE, 'Invalid climatology specified, '+STRTRIM(_climatology,2), $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    ENDIF
    self.Climatology = _climatology
  ENDIF


  ; And now the objects
  ; ...Clouds
  IF ( N_ELEMENTS(Cloud) GT 0 ) THEN BEGIN
    ; Only accept cloud list input
    IF ( ~ ISA(Cloud,'Cloud_List') ) THEN $
      MESSAGE, 'Input Cloud must be a list containing individual cloud objects.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    ; Iterate over clouds
    FOREACH cld, Cloud DO BEGIN
      ; Ignore non-clouds
      IF ( ~ OBJ_ISA(cld,'Cloud') ) THEN CONTINUE
      ; Check dimensions
      cld->Cloud::Get_Property, n_Layers=n_cloud_layers, Debug=Debug
      IF ( n_cloud_layers NE n_layers ) THEN $
        MESSAGE, 'Size of input Cloud different from Atmosphere allocation.', $
                  NONAME=MsgSwitch, NOPRINT=MsgSwitch
      ; Accumulate clouds
      (self.Cloud).Add, cld
      self.n_Clouds++
    ENDFOREACH
  ENDIF
  ; ...Aerosols
  IF ( N_ELEMENTS(Aerosol) GT 0 ) THEN BEGIN
    ; Only accept list input
    IF ( ~ ISA(Aerosol,'Aerosol_List') ) THEN $
      MESSAGE, 'Input Aerosol must be a list containing individual aerosol objects.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    ; Iterate over aerosols
    FOREACH aero, Aerosol DO BEGIN
      ; Ignore non-Aerosols
      IF ( ~ OBJ_ISA(aero,'Aerosol') ) THEN CONTINUE
      ; Check dimensions
      aero->Aerosol::Get_Property, n_Layers=n_aerosol_layers, Debug=Debug
      IF ( n_aerosol_layers NE n_layers ) THEN $
        MESSAGE, 'Size of input Aerosol different from Atmosphere allocation.', $
                  NONAME=MsgSwitch, NOPRINT=MsgSwitch
      ; Accumulate aerosols
      (self.Aerosol).Add, aero
      self.n_Aerosols++
    ENDFOREACH
  ENDIF
  
END
