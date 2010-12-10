;+
; NAME:
;       Cloud::Set_Property
;
; PURPOSE:
;       The Cloud::Set_Property procedure method sets the value of a property
;       or group of properties for this object.
;
; CALLING SEQUENCE:
;       Obj->[Cloud::]Set_Property, $
;         Type               = Type              , $  ; Input keyword
;         Effective_Radius   = Effective_Radius  , $  ; Input keyword
;         Effective_Variance = Effective_Variance, $  ; Input keyword
;         Water_Content      = Water_Content     , $  ; Input keyword
;         Debug              = Debug                  ; Input keyword
;
; INPUT KEYWORDS:
;       Type:                  Integer flag specifying the cloud type.
;                              Valid cloud types are:
;                                1: Water cloud  
;                                2: Ice cloud    
;                                3: Rain cloud   
;                                4: Snow cloud   
;                                5: Graupel cloud
;                                6: Hail cloud   
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Effective_Radius:      The profile of effective radii for the cloud.
;                              UNITS:      micrometres (um)
;                              TYPE:       REAL
;                              DIMENSION:  Rank-1
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Effective_Variance:    The profile of the variance of the effective
;                              radii for the cloud.
;                              UNITS:      micrometres^2 (um^2)
;                              TYPE:       REAL
;                              DIMENSION:  Rank-1
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Water_Content:         The water content profile of the cloud.
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
;       cloud_parameters: Include file for cloud specific parameters.
;
;       cloud_pro_err_handler: Include file for error handling.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 09-Dec-2010
;                       paul.vandelst@noaa.gov
;
;-

PRO Cloud::Set_Property, $
  Type               = Type              , $  ; Input keyword
  Effective_Radius   = Effective_Radius  , $  ; Input keyword
  Effective_Variance = Effective_Variance, $  ; Input keyword
  Water_Content      = Water_Content     , $  ; Input keyword
  Debug              = Debug                  ; Input keyword


  ; Set up
  @cloud_parameters
  @cloud_pro_err_handler
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
      MESSAGE, 'Size of input Effective_Radius different from Cloud allocation.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    (self.Effective_Radius).Add, Effective_Radius
  ENDIF
  n = N_ELEMENTS(Effective_Variance)
  IF ( n GT 0 ) THEN BEGIN
    IF ( n NE n_layers ) THEN $
      MESSAGE, 'Size of input Effective_Variance different from Cloud allocation.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    (self.Effective_Variance).Add, Effective_Variance
  ENDIF
  n = N_ELEMENTS(Water_Content)
  IF ( n GT 0 ) THEN BEGIN
    IF ( n NE n_layers ) THEN $
      MESSAGE, 'Size of input Water_Content different from Cloud allocation.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    (self.Water_Content).Add, Water_Content
  ENDIF
  
  
  ; And now the scalars
  IF ( N_ELEMENTS(Type) GT 0 ) THEN BEGIN
    _Type = LONG(Type[0])
    IF ( _Type LT 1 OR _Type GT N_VALID_CLOUD_TYPES ) THEN BEGIN
      MESSAGE, 'Valid cloud types:', /INFORMATIONAL
      FOR nc = 1, N_VALID_CLOUD_TYPES DO $
        MESSAGE, STRING(nc,CLOUD_TYPE_NAME[nc], FORMAT='(2x,i2,") ",a)'), /INFORMATIONAL
      MESSAGE, 'Invalid cloud type specified, '+STRTRIM(_Type,2), $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    ENDIF
    self.Type = _Type
  ENDIF

END ; PRO Cloud::Set_Property
