;+
; NAME:
;       Aerosol::Set_Property
;
; PURPOSE:
;       The Aerosol::Set_Property procedure method sets the value of a property
;       or group of properties for this object.
;
; CALLING SEQUENCE:
;       Obj->[Aerosol::]Set_Property, $
;         Type             = Type            , $  ; Input keyword
;         Effective_Radius = Effective_Radius, $  ; Input keyword
;         Concentration    = Concentration   , $  ; Input keyword
;         Debug            = Debug                ; Input keyword
;
; INPUT KEYWORDS:
;       Type:                  Integer flag specifying the aerosol type.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Effective_Radius:      The profile of effective radii for the aerosol.
;                              UNITS:      micrometres (um)
;                              TYPE:       REAL
;                              DIMENSION:  Rank-1
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Concentration:         The water content profile of the aerosol.
;                              UNITS:      kg/m^2
;                              TYPE:       REAL
;                              DIMENSION:  Rank-1
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
;       aerosol_parameters: Include file for aerosol specific parameters.
;
;       aerosol_pro_err_handler: Include file for error handling.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 09-Dec-2010
;                       paul.vandelst@noaa.gov
;
;-

PRO Aerosol::Set_Property, $
  Type             = Type            , $  ; Input keyword
  Effective_Radius = Effective_Radius, $  ; Input keyword
  Concentration    = Concentration   , $  ; Input keyword
  Debug            = Debug                ; Input keyword


  ; Set up
  @aerosol_parameters
  @aerosol_pro_err_handler
  ; ...Check the structure has been allocated
  IF ( NOT self->Associated(Debug=Debug) ) THEN $
    MESSAGE, 'Object has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
 

  ; Get the object dimension for checking
  self->Get_Property, n_Layers = n_layers, Debug=Debug


  ; Check the input data arrays first
  n = N_ELEMENTS(Effective_Radius)
  IF ( n GT 0 ) THEN BEGIN
    IF ( n NE n_layers ) THEN $
      MESSAGE, 'Size of input Effective_Radius different from Aerosol allocation.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    (self.Effective_Radius).Add, Effective_Radius
  ENDIF
  n = N_ELEMENTS(Concentration)
  IF ( n GT 0 ) THEN BEGIN
    IF ( n NE n_layers ) THEN $
      MESSAGE, 'Size of input Concentration different from Aerosol allocation.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    (self.Concentration).Add, Concentration
  ENDIF
  
  
  ; And now the scalars
  IF ( N_ELEMENTS(Type) GT 0 ) THEN BEGIN
    _Type = LONG(Type[0])
    IF ( _Type LT 1 OR _Type GT N_VALID_AEROSOL_TYPES ) THEN BEGIN
      MESSAGE, 'Valid aerosol types:', /INFORMATIONAL
      FOR na = 1, N_VALID_AEROSOL_TYPES DO $
        MESSAGE, STRING(na,AEROSOL_TYPE_NAME[na], FORMAT='(2x,i2,") ",a)'), /INFORMATIONAL
      MESSAGE, 'Invalid aerosol type specified, '+STRTRIM(_Type,2), $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    ENDIF
    self.Type = _Type
  ENDIF

END ; PRO Aerosol::Set_Property
