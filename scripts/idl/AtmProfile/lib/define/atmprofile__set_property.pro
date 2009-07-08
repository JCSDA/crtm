;+
; NAME:
;       AtmProfile::Set_Property
;
; PURPOSE:
;       The AtmProfile::Set_Property procedure method sets the value
;       of a property or group of properties for this object.
;
; CALLING SEQUENCE:
;       Obj->[AtmProfile::]Set_Property, $
;         Debug                 = Debug                , $  ; Input keyword
;         Version               = Version              , $  ; Input keyword
;         Profile               = Profile              , $  ; Input keyword
;         Description           = Description          , $  ; Input keyword
;         Climatology_Model     = Climatology_Model    , $  ; Input keyword
;         Year                  = Year                 , $  ; Input keyword
;         Month                 = Month                , $  ; Input keyword
;         Day                   = Day                  , $  ; Input keyword
;         Hour                  = Hour                 , $  ; Input keyword
;         Latitude              = Latitude             , $  ; Input keyword
;         Longitude             = Longitude            , $  ; Input keyword
;         Surface_Altitude      = Surface_Altitude     , $  ; Input keyword
;         Absorber_ID           = Absorber_ID          , $  ; Input keyword
;         Absorber_Units_ID     = Absorber_Units_ID    , $  ; Input keyword
;         Level_Pressure        = Level_Pressure       , $  ; Input keyword
;         Level_Temperature     = Level_Temperature    , $  ; Input keyword
;         Level_Absorber        = Level_Absorber       , $  ; Input keyword
;         Level_Altitude        = Level_Altitude       , $  ; Input keyword
;         Layer_Pressure        = Layer_Pressure       , $  ; Input keyword
;         Layer_Temperature     = Layer_Temperature    , $  ; Input keyword
;         Layer_Absorber        = Layer_Absorber       , $  ; Input keyword
;         Layer_Delta_Z         = Layer_Delta_Z             ; Input keyword
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
;       Version:               The version number of the AtmProfile data.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Profile:               The profile number.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Description:           Profile description
;                              UNITS:      N/A
;                              TYPE:       CHARACTER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Climatology_Model:     Climatology model id. 
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Year:                  Year associated with profile data.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Month:                 Month associated with profile data.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Day:                   Day associated with profile data.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Hour:                  Hour associated with profile data.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Latitude:              Latitude associated with profile data.
;                              UNITS:      degrees North (-90->+90)
;                              TYPE:       DOUBLE
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Longitude:             Longitude associated with profile data.
;                              UNITS:      degrees East (0->360)
;                              TYPE:       DOUBLE
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Surface_Altitude:      Surface altitude associated with profile data.
;                              UNITS:      Metres
;                              TYPE:       DOUBLE
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Absorber_ID:           Atmospheric absorber id. Same as for HITRAN.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  n_Absorbers
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Absorber_Units_ID:     Atmospheric absorber units id.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  n_Absorbers
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Level_Pressure:        Profile pressure levels.
;                              UNITS:      hPa
;                              TYPE:       DOUBLE
;                              DIMENSION:  n_Levels
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Level_Temperature:     Profile level temperatures.
;                              UNITS:      Kelvin
;                              TYPE:       DOUBLE
;                              DIMENSION:  n_Levels
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Level_Absorber:        Profile level absorber amounts.
;                              UNITS:      Variable
;                              TYPE:       DOUBLE
;                              DIMENSION:  n_Levels x n_Absorbers
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Level_Altitude:        Profile level altitudes.
;                              UNITS:      Metres
;                              TYPE:       DOUBLE
;                              DIMENSION:  n_Levels
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Layer_Pressure:        Profile pressure layers.
;                              UNITS:      hPa
;                              TYPE:       DOUBLE
;                              DIMENSION:  n_Layers
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Layer_Temperature:     Profile layer temperatures.
;                              UNITS:      Kelvin
;                              TYPE:       DOUBLE
;                              DIMENSION:  n_Layers
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Layer_Absorber:        Profile layer absorber amounts.
;                              UNITS:      Variable
;                              TYPE:       DOUBLE
;                              DIMENSION:  n_Layers x n_Absorbers
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Layer_Delta_Z:         Profile layer thickness.
;                              UNITS:      Metres
;                              TYPE:       DOUBLE
;                              DIMENSION:  n_Layers
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
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

PRO AtmProfile::Set_Property, $
  Debug             = Debug            , $  ; Input keyword
  Version           = Version          , $  ; Input keyword
  Profile           = Profile          , $  ; Input keyword
  Description       = Description      , $  ; Input keyword
  Climatology_Model = Climatology_Model, $  ; Input keyword
  Year              = Year             , $  ; Input keyword
  Month             = Month            , $  ; Input keyword
  Day               = Day              , $  ; Input keyword
  Hour              = Hour             , $  ; Input keyword
  Latitude          = Latitude         , $  ; Input keyword
  Longitude         = Longitude        , $  ; Input keyword
  Surface_Altitude  = Surface_Altitude , $  ; Input keyword
  Absorber_ID       = Absorber_ID      , $  ; Input keyword
  Absorber_Units_ID = Absorber_Units_ID, $  ; Input keyword
  Level_Pressure    = Level_Pressure   , $  ; Input keyword
  Level_Temperature = Level_Temperature, $  ; Input keyword
  Level_Absorber    = Level_Absorber   , $  ; Input keyword
  Level_Altitude    = Level_Altitude   , $  ; Input keyword
  Layer_Pressure    = Layer_Pressure   , $  ; Input keyword
  Layer_Temperature = Layer_Temperature, $  ; Input keyword
  Layer_Absorber    = Layer_Absorber   , $  ; Input keyword
  Layer_Delta_Z     = Layer_Delta_Z         ; Input keyword

  ; Set up
  ; ...AtmProfile parameters
  @atmprofile_parameters
  ; ...Set up error handler
  @atmprofile_pro_err_handler
 
  
  ; Check if structure has been allocated
  IF ( self->Associated(Debug=Debug) EQ FALSE ) THEN $
    MESSAGE, 'AtmProfile structure has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Set data
  IF ( N_ELEMENTS(Version          ) GT 0 ) THEN self.Version           = Version
  IF ( N_ELEMENTS(Profile          ) GT 0 ) THEN self.Profile           = Profile    
  IF ( N_ELEMENTS(Description      ) GT 0 ) THEN self.Description       = Description
  IF ( N_ELEMENTS(Climatology_Model) GT 0 ) THEN self.Climatology_Model = Climatology_Model
  IF ( N_ELEMENTS(Year             ) GT 0 ) THEN self.Year              = Year
  IF ( N_ELEMENTS(Month            ) GT 0 ) THEN self.Month             = Month
  IF ( N_ELEMENTS(Day              ) GT 0 ) THEN self.Day               = Day
  IF ( N_ELEMENTS(Hour             ) GT 0 ) THEN self.Hour              = Hour
  IF ( N_ELEMENTS(Latitude         ) GT 0 ) THEN self.Latitude          = Latitude
  IF ( N_ELEMENTS(Longitude        ) GT 0 ) THEN self.Longitude         = Longitude
  IF ( N_ELEMENTS(Surface_Altitude ) GT 0 ) THEN self.Surface_Altitude  = Surface_Altitude
  
  IF ( N_ELEMENTS(Absorber_ID      ) EQ self.n_Absorbers ) THEN *self.Absorber_ID = Absorber_ID
  IF ( N_ELEMENTS(Absorber_Units_ID) EQ self.n_Absorbers ) THEN BEGIN
    *self.Absorber_Units_ID = Absorber_Units_ID
    *self.Absorber_Units_Name = ATMPROFILE_ABSORBER_UNITS_NAME[Absorber_Units_ID]
    *self.Absorber_Units_LBL  = ATMPROFILE_ABSORBER_UNITS_CHAR[Absorber_Units_ID]
  ENDIF

  IF ( N_ELEMENTS(Level_Pressure   ) EQ self.n_Levels ) THEN *self.Level_Pressure    = Level_Pressure
  IF ( N_ELEMENTS(Level_Temperature) EQ self.n_Levels ) THEN *self.Level_Temperature = Level_Temperature
  IF ( N_ELEMENTS(Level_Absorber   ) GT 0 ) THEN BEGIN
    info = SIZE(Level_Absorber,/STRUCTURE)
    IF ( info.N_DIMENSIONS EQ 2 ) THEN $
      IF ( info.DIMENSIONS(0) EQ self.n_Levels AND $
           info.DIMENSIONS(1) EQ self.n_Absorbers ) THEN *self.Level_Absorber = Level_Absorber
  ENDIF
  IF ( N_ELEMENTS(Level_Altitude) EQ self.n_Levels ) THEN *self.Level_Altitude = Level_Altitude

  IF ( N_ELEMENTS(Layer_Pressure   ) EQ self.n_Layers ) THEN *self.Layer_Pressure    = Layer_Pressure
  IF ( N_ELEMENTS(Layer_Temperature) EQ self.n_Layers ) THEN *self.Layer_Temperature = Layer_Temperature
  IF ( N_ELEMENTS(Layer_Absorber   ) GT 0 ) THEN BEGIN
    info = SIZE(Layer_Absorber,/STRUCTURE)
    IF ( info.N_DIMENSIONS EQ 2 ) THEN $
      IF ( info.DIMENSIONS(0) EQ self.n_Layers AND $
           info.DIMENSIONS(1) EQ self.n_Absorbers ) THEN *self.Layer_Absorber = Layer_Absorber
  ENDIF
  IF ( N_ELEMENTS(Layer_Delta_Z) EQ self.n_Layers ) THEN *self.Layer_Delta_Z = Layer_Delta_Z

  
  ; Done
  CATCH, /CANCEL
 
END ; PRO AtmProfile::Set_Property
