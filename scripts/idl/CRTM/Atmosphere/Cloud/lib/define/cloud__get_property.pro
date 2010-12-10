;+
; NAME:
;       Cloud::Get_Property
;
; PURPOSE:
;       The Cloud::Get_Property procedure method gets the value of a property
;       or group of properties for this object.
;
; CALLING SEQUENCE:
;       Obj->[Cloud::]Get_Property, $
;         Debug              = Debug             , $  ; Input keyword
;         n_Layers           = n_Layers          , $  ; Output keyword
;         Type               = Type              , $  ; Output keyword
;         Effective_Radius   = Effective_Radius  , $  ; Output keyword
;         Effective_Variance = Effective_Variance, $  ; Output keyword
;         Water_Content      = Water_Content          ; Output keyword
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
;                              Cloud data.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Type:                  Integer flag identifying the cloud type.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Effective_Radius:      The profile of effective radii for the cloud.
;                              UNITS:      micrometres (um)
;                              TYPE:       REAL
;                              DIMENSION:  Rank-1
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Effective_Variance:    The profile of the variance of the effective
;                              radii for the cloud.
;                              UNITS:      micrometres^2 (um^2)
;                              TYPE:       REAL
;                              DIMENSION:  Rank-1
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Water_Content:         The water content profile of the cloud.
;                              UNITS:      kg/m^2
;                              TYPE:       REAL
;                              DIMENSION:  Rank-1
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
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

PRO Cloud::Get_Property, $
  Debug              = Debug             , $  ; Input keyword
  n_Layers           = n_Layers          , $  ; Output keyword
  Type               = Type              , $  ; Output keyword
  Effective_Radius   = Effective_Radius  , $  ; Output keyword
  Effective_Variance = Effective_Variance, $  ; Output keyword
  Water_Content      = Water_Content          ; Output keyword


  ; Set up
  @cloud_pro_err_handler
  ; ...Check the structure has been allocated
  IF ( NOT self->Associated(Debug=Debug) ) THEN $
    MESSAGE, 'Object has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
 

  ; Get data
  IF ( ARG_PRESENT(n_Layers          ) ) THEN n_Layers           = self.n_Layers
  IF ( ARG_PRESENT(Type              ) ) THEN Type               = self.Type              
  IF ( ARG_PRESENT(Effective_Radius  ) ) THEN Effective_Radius   = (self.Effective_Radius)[0]  
  IF ( ARG_PRESENT(Effective_Variance) ) THEN Effective_Variance = (self.Effective_Variance)[0]
  IF ( ARG_PRESENT(Water_Content     ) ) THEN Water_Content      = (self.Water_Content)[0]     

END ; PRO Cloud::Get_Property
