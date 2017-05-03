;+
; NAME:
;       Atmosphere::Destroy
;
; PURPOSE:
;       The Atmosphere::Destroy procedure method reinitialises a Atmosphere object.
;
; CALLING SEQUENCE:
;       Obj->[Atmosphere::]Destroy, $
;         Debug=Debug
;
; INPUT KEYWORD PARAMETERS:
;       Debug:       Set this keyword for debugging.
;                    If NOT SET => Error handler is enabled. (DEFAULT)
;                       SET     => Error handler is disabled; Routine
;                                  traceback output is enabled.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;-

PRO Atmosphere::Destroy, $
  Debug = Debug  ; Input keyword
 
  ; Set up
  COMPILE_OPT HIDDEN
  @atmosphere_parameters
  @atmosphere_pro_err_handler
 
 
  ; Reinitialise
  self.Is_Allocated = FALSE
  self.n_Layers     = 0L
  self.n_Absorbers  = 0L
  self.n_Clouds     = 0L
  self.n_Aerosols   = 0L
  self.Climatology  = US_STANDARD_ATMOSPHERE

  IF ( OBJ_VALID(self.Absorber_ID   ) ) THEN self.Absorber_ID    -> REMOVE, /ALL
  IF ( OBJ_VALID(self.Absorber_Units) ) THEN self.Absorber_Units -> REMOVE, /ALL
  IF ( OBJ_VALID(self.Level_Pressure) ) THEN self.Level_Pressure -> REMOVE, /ALL
  IF ( OBJ_VALID(self.Pressure      ) ) THEN self.Pressure       -> REMOVE, /ALL
  IF ( OBJ_VALID(self.Temperature   ) ) THEN self.Temperature    -> REMOVE, /ALL
  IF ( OBJ_VALID(self.Absorber      ) ) THEN self.Absorber       -> REMOVE, /ALL
  IF ( OBJ_VALID(self.CFraction     ) ) THEN self.CFraction      -> REMOVE, /ALL
  IF ( OBJ_VALID(self.Cloud         ) ) THEN self.Cloud          -> REMOVE, /ALL
  IF ( OBJ_VALID(self.Aerosol       ) ) THEN self.Aerosol        -> REMOVE, /ALL

END
