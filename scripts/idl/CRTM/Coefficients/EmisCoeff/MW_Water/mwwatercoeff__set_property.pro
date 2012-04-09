;+
PRO MWwaterCoeff::Set_Property, $
  Debug          = Debug         , $  ; Input keyword
  Angle          = Angle         , $  ; Input keyword
  Frequency      = Frequency     , $  ; Input keyword
  Temperature    = Temperature   , $  ; Input keyword
  Wind_Speed     = Wind_Speed    , $  ; Input keyword
  ev             = ev            , $  ; Input keyword
  eh             = eh                 ; Input keyword
;-

  ; Set up
  @emiscoeff_pro_err_handler
  ; ...Check the structure has been allocated
  IF ( NOT self->Associated(Debug=Debug) ) THEN $
    MESSAGE, 'Object has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
 
 
  ; Get the object dimensions for checking
  self->Get_Property, $
    n_Angles       = n_Angles      , $
    n_Frequencies  = n_Frequencies , $
    n_Temperatures = n_Temperatures, $
    n_Wind_Speeds  = n_Wind_Speeds , $
    Debug = Debug
 
    
  ; Set dimension vectors
  ; ...Angle vector
  n = N_ELEMENTS(Angle)
  IF ( n GT 0 ) THEN BEGIN
    IF ( n NE n_Angles ) THEN $
      MESSAGE, 'Size of input Angle array is different from that allocated.', $
        NONAME=MsgSwitch, NOPRINT=MsgSwitch
    *self.Angle = Angle
  ENDIF
  ; ...Frequency vector
  n = N_ELEMENTS(Frequency)
  IF ( n GT 0 ) THEN BEGIN
    IF ( n NE n_Frequencies ) THEN $
      MESSAGE, 'Size of input Frequency array is different from that allocated.', $
        NONAME=MsgSwitch, NOPRINT=MsgSwitch
    *self.Frequency = Frequency
  ENDIF
  ; ...Temperature vector
  n = N_ELEMENTS(Temperature)
  IF ( n GT 0 ) THEN BEGIN
    IF ( n NE n_Temperatures ) THEN $
      MESSAGE, 'Size of input Temperature array is different from that allocated.', $
        NONAME=MsgSwitch, NOPRINT=MsgSwitch
    *self.Temperature = Temperature
  ENDIF
  ; ...Wind_Speed vector
  n = N_ELEMENTS(Wind_Speed)
  IF ( n GT 0 ) THEN BEGIN
    IF ( n NE n_Wind_Speeds ) THEN $
      MESSAGE, 'Size of input Wind_Speed array is different from that allocated.', $
        NONAME=MsgSwitch, NOPRINT=MsgSwitch
    *self.Wind_Speed = Wind_Speed
  ENDIF
  
  
  ; Set data arrays
  ; ...Vertical polarisation
  input_info = SIZE(ev,/STRUCTURE)
  array_info = SIZE(*self.ev,/STRUCTURE)
  IF ( input_info.N_ELEMENTS GT 0 ) THEN BEGIN
    IF ( input_info.N_DIMENSIONS NE array_info.N_DIMENSIONS ) THEN $
      MESSAGE, 'Number of ev input array dimensions is different from structure definition.', $
          NONAME=MsgSwitch, NOPRINT=MsgSwitch
    FOR n = 0, input_info.N_DIMENSIONS - 1 DO BEGIN
      IF ( input_info.DIMENSIONS[n] NE array_info.DIMENSIONS[n] ) THEN $
        MESSAGE, 'Size of input ev array is different from that allocated.', $
            NONAME=MsgSwitch, NOPRINT=MsgSwitch
    ENDFOR
    *self.ev = ev
  ENDIF
  ; ...Horizontal polarisation
  input_info = SIZE(eh,/STRUCTURE)
  array_info = SIZE(*self.eh,/STRUCTURE)
  IF ( input_info.N_ELEMENTS GT 0 ) THEN BEGIN
    IF ( input_info.N_DIMENSIONS NE array_info.N_DIMENSIONS ) THEN $
      MESSAGE, 'Number of eh input array dimensions is different from structure definition.', $
          NONAME=MsgSwitch, NOPRINT=MsgSwitch
    FOR n = 0, input_info.N_DIMENSIONS - 1 DO BEGIN
      IF ( input_info.DIMENSIONS[n] NE array_info.DIMENSIONS[n] ) THEN $
        MESSAGE, 'Size of input eh array is different from that allocated.', $
            NONAME=MsgSwitch, NOPRINT=MsgSwitch
    ENDFOR
    *self.eh = eh
  ENDIF

END
