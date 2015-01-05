;+
; NAME:
;       IRwaterCoeff::Get_Property
;
; PURPOSE:
;       The IRwaterCoeff::Get_Property procedure method gets the value of a property
;       or group of properties for this object.
;
; CALLING SEQUENCE:
;       Obj->[IRwaterCoeff::]Get_Property, $
;         Debug         = Debug        , $
;         Release       = Release      , $
;         Version       = Version      , $
;         n_Angles      = n_Angles     , $
;         n_Frequencies = n_Frequencies, $
;         n_Wind_Speeds = n_Wind_Speeds, $
;         Angle         = Angle        , $
;         Frequency     = Frequency    , $
;         Wind_Speed    = Wind_Speed   , $
;         Emissivity    = Emissivity   
;
; INPUT KEYWORDS:
;       Debug:               Set this keyword for debugging.
;                            If NOT SET => Error handler is enabled. (DEFAULT)
;                               SET     => Error handler is disabled; Routine
;                                          traceback output is enabled.
;                            UNITS:      N/A
;                            TYPE:       INTEGER
;                            DIMENSION:  Scalar
;                            ATTRIBUTES: INTENT(IN), OPTIONAL
;
; OUTPUT KEYWORDS:
;       Release:             The data release.
;                            UNITS:      N/A
;                            TYPE:       INTEGER
;                            DIMENSION:  Scalar
;                            ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Version:             The data version.
;                            UNITS:      N/A
;                            TYPE:       INTEGER
;                            DIMENSION:  Scalar
;                            ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       n_Angles:            Number of angles for which there are
;                            IRwaterCoeff data.
;                            UNITS:      N/A
;                            TYPE:       INTEGER
;                            DIMENSION:  Scalar
;                            ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       n_Frequencies:       Number of spectral frequencies for which there are
;                            IRwaterCoeff data.
;                            UNITS:      N/A
;                            TYPE:       INTEGER
;                            DIMENSION:  Scalar
;                            ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       n_Wind_Speeds:       Number of wind speeds for which there are
;                            IRwaterCoeff data.
;                            UNITS:      N/A
;                            TYPE:       INTEGER
;                            DIMENSION:  Scalar
;                            ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Angle:               The zenith angle dimension vector
;                            UNITS:      Degrees
;                            TYPE:       REAL
;                            DIMENSION:  Rank-1
;                            ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Frequency:           The frequency dimension vector.
;                            UNITS:      inverse centimetres (cm^-1)
;                            TYPE:       REAL
;                            DIMENSION:  Rank-1
;                            ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Wind_Speed:          The wind speed dimension vector
;                            UNITS:      m.s^-1
;                            TYPE:       REAL
;                            DIMENSION:  Rank-1
;                            ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Emissivity:          The sea surface emissivity data
;                            UNITS:      N/A
;                            TYPE:       REAL
;                            DIMENSION:  Rank-3 (angle x frequency x wind-speed)
;                            ATTRIBUTES: INTENT(OUT), OPTIONAL
;-

PRO IRwaterCoeff::Get_Property, $
  Debug         = Debug        , $  ; Input keyword
  Release       = Release      , $  ; Output keyword
  Version       = Version      , $  ; Output keyword
  n_Angles      = n_Angles     , $  ; Output keyword
  n_Frequencies = n_Frequencies, $  ; Output keyword
  n_Wind_Speeds = n_Wind_Speeds, $  ; Output keyword
  Angle         = Angle        , $  ; Output keyword
  Frequency     = Frequency    , $  ; Output keyword
  Wind_Speed    = Wind_Speed   , $  ; Output keyword
  Emissivity    = Emissivity   , $  ; Output keyword
  Remove        = Remove


  ; Set up
  @irwatercoeff_pro_err_handler
  ; ...Check the structure has been allocated
  IF ( ~ self->Associated(Debug=Debug) ) THEN $
    MESSAGE, 'Object has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
 
  ; Get scalar data
  IF ( ARG_PRESENT(Release) ) THEN Release = self.Release
  IF ( ARG_PRESENT(Version) ) THEN Version = self.Version
  IF ( ARG_PRESENT(n_Angles     ) ) THEN n_Angles      = self.n_Angles     
  IF ( ARG_PRESENT(n_Frequencies) ) THEN n_Frequencies = self.n_Frequencies
  IF ( ARG_PRESENT(n_Wind_Speeds) ) THEN n_Wind_Speeds = self.n_Wind_Speeds
  
  
  ; Get array data
  IF ( ARG_PRESENT(angle) ) THEN BEGIN
    IF ( KEYWORD_SET(Remove) ) THEN $
      angle = (self.Angle).Remove(0) $
    ELSE $
      angle = (self.Angle)[0]
  ENDIF
  
  IF ( ARG_PRESENT(frequency) )  THEN BEGIN
    IF ( KEYWORD_SET(Remove) ) THEN $
      frequency = (self.Frequency).Remove(0) $
    ELSE $
      frequency = (self.Frequency)[0]  
    ENDIF
    
  IF ( ARG_PRESENT(wind_speed) ) THEN BEGIN
    IF ( KEYWORD_SET(Remove) ) THEN $
      wind_speed = (self.Wind_Speed).Remove(0) $
    ELSE $
      wind_speed = (self.Wind_Speed)[0]
  ENDIF
  
  IF ( ARG_PRESENT(emissivity) ) THEN BEGIN
    IF ( KEYWORD_SET(Remove) ) THEN $
      emissivity = (self.Emissivity).Remove(0) $
    ELSE $
      emissivity = (self.Emissivity)[0]
  ENDIF
  
END
