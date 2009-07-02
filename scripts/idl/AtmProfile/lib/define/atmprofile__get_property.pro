;+
; NAME:
;       AtmProfile::Get_Property
;
; PURPOSE:
;       The AtmProfile::Get_Property procedure method retrieves the value
;       of a property or group of properties for this object.
;
; CALLING SEQUENCE:
;       Obj->[AtmProfile::]Get_Property, $
;         Debug                 = Debug                , $  ; Input keyword
;         n_Levels              = n_Levels             , $  ; Output keyword
;         n_Layers              = n_Layers             , $  ; Output keyword
;         n_Absorbers           = n_Absorbers          , $  ; Output keyword
;         Version               = Version              , $  ; Output keyword
;         Description           = Description          , $  ; Output keyword
;         Climatology_Model     = Climatology_Model    , $  ; Output keyword
;         Year                  = Year                 , $  ; Output keyword
;         Month                 = Month                , $  ; Output keyword
;         Day                   = Day                  , $  ; Output keyword
;         Hour                  = Hour                 , $  ; Output keyword
;         Latitude              = Latitude             , $  ; Output keyword
;         Longitude             = Longitude            , $  ; Output keyword
;         Surface_Altitude      = Surface_Altitude     , $  ; Output keyword
;         Absorber_ID           = Absorber_ID          , $  ; Output keyword
;         Absorber_Units_ID     = Absorber_Units_ID    , $  ; Output keyword
;         Absorber_Units_Name   = Absorber_Units_Name  , $  ; Output keyword
;         Absorber_Units_LBLRTM = Absorber_Units_LBLRTM, $  ; Output keyword
;         Level_Pressure        = Level_Pressure       , $  ; Output keyword
;         Level_Temperature     = Level_Temperature    , $  ; Output keyword
;         Level_Absorber        = Level_Absorber       , $  ; Output keyword
;         Level_Altitude        = Level_Altitude       , $  ; Output keyword
;         Layer_Pressure        = Layer_Pressure       , $  ; Output keyword
;         Layer_Temperature     = Layer_Temperature    , $  ; Output keyword
;         Layer_Absorber        = Layer_Absorber       , $  ; Output keyword
;         Layer_Delta_Z         = Layer_Delta_Z             ; Output keyword
;
; INPUT KEYWORD PARAMETERS:
;       Debug:                 Set this keyword for debugging.
;                              If NOT SET => Error handler is enabled. (DEFAULT)
;                                 SET     => Error handler is disabled; Routine
;                                            traceback output is enabled.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
; OUTPUT KEYWORD PARAMETERS:
;       n_Levels:              The number of atmospheric levels dimension of the
;                              atmospheric profile data.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       n_Layers:              The number of atmospheric layers dimension of the
;                              atmospheric profile data.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       n_Absorbers:           The number of molecular absorbers dimension of the
;                              atmospheric profile data.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Version:               The version number of the AtmProfile data.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Description:           Profile description
;                              UNITS:      N/A
;                              TYPE:       CHARACTER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Climatology_Model:     Climatology model id. 
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Year:                  Year associated with profile data.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Month:                 Month associated with profile data.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Day:                   Day associated with profile data.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Hour:                  Hour associated with profile data.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Latitude:              Latitude associated with profile data.
;                              UNITS:      degrees North (-90->+90)
;                              TYPE:       DOUBLE
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Longitude:             Longitude associated with profile data.
;                              UNITS:      degrees East (0->360)
;                              TYPE:       DOUBLE
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Surface_Altitude:      Surface altitude associated with profile data.
;                              UNITS:      Metres
;                              TYPE:       DOUBLE
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Absorber_ID:           Atmospheric absorber id. Same as for HITRAN.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  n_Absorbers
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Absorber_Units_ID:     Atmospheric absorber units id.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  n_Absorbers
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Absorber_Units_Name:   Atmospheric absorber units name.
;                              UNITS:      N/A
;                              TYPE:       CHARACTER
;                              DIMENSION:  n_Absorbers
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Absorber_Units_LBLRTM: Atmospheric absorber units flag used in LBLRTM.
;                              UNITS:      N/A
;                              TYPE:       CHARACTER
;                              DIMENSION:  n_Absorbers
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Level_Pressure:        Profile pressure levels.
;                              UNITS:      hPa
;                              TYPE:       DOUBLE
;                              DIMENSION:  n_Levels
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Level_Temperature:     Profile level temperatures.
;                              UNITS:      Kelvin
;                              TYPE:       DOUBLE
;                              DIMENSION:  n_Levels
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Level_Absorber:        Profile level absorber amounts.
;                              UNITS:      Variable
;                              TYPE:       DOUBLE
;                              DIMENSION:  n_Levels x n_Absorbers
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Level_Altitude:        Profile level altitudes.
;                              UNITS:      Metres
;                              TYPE:       DOUBLE
;                              DIMENSION:  n_Levels
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Layer_Pressure:        Profile pressure layers.
;                              UNITS:      hPa
;                              TYPE:       DOUBLE
;                              DIMENSION:  n_Layers
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Layer_Temperature:     Profile layer temperatures.
;                              UNITS:      Kelvin
;                              TYPE:       DOUBLE
;                              DIMENSION:  n_Layers
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Layer_Absorber:        Profile layer absorber amounts.
;                              UNITS:      Variable
;                              TYPE:       DOUBLE
;                              DIMENSION:  n_Layers x n_Absorbers
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Layer_Delta_Z:         Profile layer thickness.
;                              UNITS:      Metres
;                              TYPE:       DOUBLE
;                              DIMENSION:  n_Layers
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
; INCLUDE FILES:
;       atmprofile_parameters: Include file containing AtmProfile specific
;                              parameter value definitions.
;
;       atmprofile_pro_err_handler: Error handler code for AtmProfile procedures.
;
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 02-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO AtmProfile::Get_Property, $
  Debug                 = Debug                , $  ; Input keyword
  n_Levels              = n_Levels             , $  ; Output keyword
  n_Layers              = n_Layers             , $  ; Output keyword
  n_Absorbers           = n_Absorbers          , $  ; Output keyword
  Version               = Version              , $  ; Output keyword
  Description           = Description          , $  ; Output keyword
  Climatology_Model     = Climatology_Model    , $  ; Output keyword
  Year                  = Year                 , $  ; Output keyword
  Month                 = Month                , $  ; Output keyword
  Day                   = Day                  , $  ; Output keyword
  Hour                  = Hour                 , $  ; Output keyword
  Latitude              = Latitude             , $  ; Output keyword
  Longitude             = Longitude            , $  ; Output keyword
  Surface_Altitude      = Surface_Altitude     , $  ; Output keyword
  Absorber_ID           = Absorber_ID          , $  ; Output keyword
  Absorber_Units_ID     = Absorber_Units_ID    , $  ; Output keyword
  Absorber_Units_Name   = Absorber_Units_Name  , $  ; Output keyword
  Absorber_Units_LBLRTM = Absorber_Units_LBLRTM, $  ; Output keyword
  Level_Pressure        = Level_Pressure       , $  ; Output keyword
  Level_Temperature     = Level_Temperature    , $  ; Output keyword
  Level_Absorber        = Level_Absorber       , $  ; Output keyword
  Level_Altitude        = Level_Altitude       , $  ; Output keyword
  Layer_Pressure        = Layer_Pressure       , $  ; Output keyword
  Layer_Temperature     = Layer_Temperature    , $  ; Output keyword
  Layer_Absorber        = Layer_Absorber       , $  ; Output keyword
  Layer_Delta_Z         = Layer_Delta_Z             ; Output keyword

  ; Set up
  ; ...AtmProfile parameters
  @atmprofile_parameters
  ; ...Set up error handler
  @atmprofile_pro_err_handler
 
  
  ; Check if structure has been allocated
  IF ( self->Associated(Debug=Debug) EQ FALSE ) THEN $
    MESSAGE, 'AtmProfile structure has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Get data
  IF ( ARG_PRESENT(n_Levels             ) ) THEN n_Levels              = self.n_Levels             
  IF ( ARG_PRESENT(n_Layers             ) ) THEN n_Layers              = self.n_Layers             
  IF ( ARG_PRESENT(n_Absorbers          ) ) THEN n_Absorbers           = self.n_Absorbers          
  IF ( ARG_PRESENT(Version              ) ) THEN Version               = self.Version              
  IF ( ARG_PRESENT(Description          ) ) THEN Description           = self.Description          
  IF ( ARG_PRESENT(Climatology_Model    ) ) THEN Climatology_Model     = self.Climatology_Model    
  IF ( ARG_PRESENT(Year                 ) ) THEN Year                  = self.Year                 
  IF ( ARG_PRESENT(Month                ) ) THEN Month                 = self.Month                
  IF ( ARG_PRESENT(Day                  ) ) THEN Day                   = self.Day                  
  IF ( ARG_PRESENT(Hour                 ) ) THEN Hour                  = self.Hour                 
  IF ( ARG_PRESENT(Latitude             ) ) THEN Latitude              = self.Latitude             
  IF ( ARG_PRESENT(Longitude            ) ) THEN Longitude             = self.Longitude            
  IF ( ARG_PRESENT(Surface_Altitude     ) ) THEN Surface_Altitude      = self.Surface_Altitude     
  IF ( ARG_PRESENT(Absorber_ID          ) ) THEN Absorber_ID           = *self.Absorber_ID          
  IF ( ARG_PRESENT(Absorber_Units_ID    ) ) THEN Absorber_Units_ID     = *self.Absorber_Units_ID    
  IF ( ARG_PRESENT(Absorber_Units_Name  ) ) THEN Absorber_Units_Name   = *self.Absorber_Units_Name  
  IF ( ARG_PRESENT(Absorber_Units_LBLRTM) ) THEN Absorber_Units_LBLRTM = *self.Absorber_Units_LBLRTM
  IF ( ARG_PRESENT(Level_Pressure       ) ) THEN Level_Pressure        = *self.Level_Pressure       
  IF ( ARG_PRESENT(Level_Temperature    ) ) THEN Level_Temperature     = *self.Level_Temperature    
  IF ( ARG_PRESENT(Level_Absorber       ) ) THEN Level_Absorber        = *self.Level_Absorber       
  IF ( ARG_PRESENT(Level_Altitude       ) ) THEN Level_Altitude        = *self.Level_Altitude       
  IF ( ARG_PRESENT(Layer_Pressure       ) ) THEN Layer_Pressure        = *self.Layer_Pressure       
  IF ( ARG_PRESENT(Layer_Temperature    ) ) THEN Layer_Temperature     = *self.Layer_Temperature    
  IF ( ARG_PRESENT(Layer_Absorber       ) ) THEN Layer_Absorber        = *self.Layer_Absorber       
  IF ( ARG_PRESENT(Layer_Delta_Z        ) ) THEN Layer_Delta_Z         = *self.Layer_Delta_Z        

  
  ; Done
  CATCH, /CANCEL
 
END ; PRO AtmProfile::Get_Property
