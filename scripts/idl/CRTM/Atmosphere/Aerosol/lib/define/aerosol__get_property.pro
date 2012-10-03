;+
; NAME:
;       Aerosol::Get_Property
;
; PURPOSE:
;       The Aerosol::Get_Property procedure method gets the value of a property
;       or group of properties for this object.
;
; CALLING SEQUENCE:
;       Obj->[Aerosol::]Get_Property, $
;         Debug            = Debug           , $
;         n_Layers         = n_Layers        , $
;         Type             = Type            , $
;         Effective_Radius = Effective_Radius, $
;         Concentration    = Concentration      
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
;                              Aerosol data.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Type:                  Integer flag identifying the aerosol type.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Effective_Radius:      The profile of effective radii for the aerosol.
;                              UNITS:      micrometres (um)
;                              TYPE:       REAL
;                              DIMENSION:  Rank-1
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Concentration:         The concentration profile of the aerosol.
;                              UNITS:      kg/m^2
;                              TYPE:       REAL
;                              DIMENSION:  Rank-1
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;-

PRO Aerosol::Get_Property, $
  Debug            = Debug             , $  ; Input keyword
  n_Layers         = n_Layers          , $  ; Output keyword
  Type             = Type              , $  ; Output keyword
  Effective_Radius = Effective_Radius  , $  ; Output keyword
  Concentration    = Concentration          ; Output keyword


  ; Set up
  @aerosol_pro_err_handler
  ; ...Check the structure has been allocated
  IF ( ~ self->Associated(Debug=Debug) ) THEN $
    MESSAGE, 'Object has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
 

  ; Get data
  IF ( ARG_PRESENT(n_Layers        ) ) THEN n_Layers         = self.n_Layers
  IF ( ARG_PRESENT(Type            ) ) THEN Type             = self.Type              
  IF ( ARG_PRESENT(Effective_Radius) ) THEN Effective_Radius = (self.Effective_Radius)[0]  
  IF ( ARG_PRESENT(Concentration   ) ) THEN Concentration    = (self.Concentration)[0]     

END
