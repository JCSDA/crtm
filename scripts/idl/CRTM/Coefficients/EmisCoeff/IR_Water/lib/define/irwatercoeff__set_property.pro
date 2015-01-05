;+
; NAME:
;       IRwaterCoeff::Set_Property
;
; PURPOSE:
;       The IRwaterCoeff::Set_Property procedure method sets the value of a property
;       or group of properties for this object.
;
; CALLING SEQUENCE:
;       Obj->[IRwaterCoeff::]Set_Property, $
;         Version    = Version   , $
;         Angle      = Angle     , $
;         Frequency  = Frequency , $
;         Wind_Speed = Wind_Speed, $
;         Emissivity = Emissivity, $
;         Debug      = Debug                
;
; INPUT KEYWORDS:
;       Version:             The data version.
;                            UNITS:      N/A
;                            TYPE:       INTEGER
;                            DIMENSION:  Scalar
;                            ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Angle:               The zenith angle dimension vector
;                            UNITS:      Degrees
;                            TYPE:       REAL
;                            DIMENSION:  Rank-1
;                            ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Frequency:           The frequency dimension vector.
;                            UNITS:      inverse centimetres (cm^-1)
;                            TYPE:       REAL
;                            DIMENSION:  Rank-1
;                            ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Wind_Speed:          The wind speed dimension vector
;                            UNITS:      m.s^-1
;                            TYPE:       REAL
;                            DIMENSION:  Rank-1
;                            ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Emissivity:          The sea surface emissivity data
;                            UNITS:      N/A
;                            TYPE:       REAL
;                            DIMENSION:  Rank-3 (angle x frequency x wind-speed)
;                            ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Debug:               Set this keyword for debugging.
;                            If NOT SET => Error handler is enabled. (DEFAULT)
;                               SET     => Error handler is disabled; Routine
;                                          traceback output is enabled.
;                            UNITS:      N/A
;                            TYPE:       INTEGER
;                            DIMENSION:  Scalar
;                            ATTRIBUTES: INTENT(IN), OPTIONAL
;-

PRO IRwaterCoeff::Set_Property, $
  Version    = Version   , $  ; Output keyword
  Angle      = Angle     , $  ; Input keyword
  Frequency  = Frequency , $  ; Input keyword
  Wind_Speed = Wind_Speed, $  ; Input keyword
  Emissivity = Emissivity, $  ; Input keyword
  Debug      = Debug          ; Input keyword


  ; Set up
  @irwatercoeff_parameters
  @irwatercoeff_pro_err_handler
  ; ...Check the structure has been allocated
  IF ( NOT self->Associated(Debug=Debug) ) THEN $
    MESSAGE, 'Object has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
 

  ; Get the object dimensions for checking
  self->Get_Property, $
    n_Angles      = n_angles     , $
    n_Frequencies = n_frequencies, $
    n_Wind_Speeds = n_wind_speeds, $
    Debug=Debug


  ; Check the input data arrays first
  n = N_ELEMENTS(Angle)
  IF ( n GT 0 ) THEN BEGIN
    IF ( n NE n_angles ) THEN $
      MESSAGE, 'Size of input Angle different from IRwaterCoeff allocation.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    (self.Angle).Add, Angle
  ENDIF
  n = N_ELEMENTS(Frequency)
  IF ( n GT 0 ) THEN BEGIN
    IF ( n NE n_frequencies ) THEN $
      MESSAGE, 'Size of input Frequency different from IRwaterCoeff allocation.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    (self.Frequency).Add, Frequency
  ENDIF
  n = N_ELEMENTS(Wind_Speed)
  IF ( n GT 0 ) THEN BEGIN
    IF ( n NE n_wind_speeds ) THEN $
      MESSAGE, 'Size of input Wind_Speed different from IRwaterCoeff allocation.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    (self.Wind_Speed).Add, Wind_Speed
  ENDIF
  n = N_ELEMENTS(Emissivity)
  IF ( n GT 0 ) THEN BEGIN
    einfo = SIZE(Emissivity, /STRUCTURE)
    IF ( einfo.N_DIMENSIONS NE 3 ) THEN $
      MESSAGE, 'Dimensions of input Emissivity different from IRwaterCoeff allocation.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    IF ( ~ ARRAY_EQUAL(einfo.DIMENSIONS[0:2], [n_angles, n_frequencies, n_wind_speeds]) ) THEN $
      MESSAGE, 'Size of input Emissivity different from IRwaterCoeff allocation.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    (self.Emissivity).Add, Emissivity
  ENDIF
  
  ; And now the scalars
  IF ( N_ELEMENTS(Version) GT 0 ) THEN BEGIN
    _Version = LONG(Version[0])
    IF ( _Version LT 1  ) THEN $
        MESSAGE, 'Data version must be > 0', $
                 NONAME=MsgSwitch, NOPRINT=MsgSwitch
    self.Version = _Version
  ENDIF
  
END
