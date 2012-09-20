
; NAME:
;       Aerosol::Destroy
;
; PURPOSE:
;       The Aerosol::Destroy procedure method reinitialises a Aerosol object.
;
; CALLING SEQUENCE:
;       Obj->[Aerosol::]Destroy, $
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

PRO Aerosol::Destroy, $
  Debug = Debug  ; Input keyword
 
  ; Set up
  @aerosol_parameters
  @aerosol_pro_err_handler
 
 
  ; Reinitialise
  self.Is_Allocated = FALSE
  self.n_Layers     = 0L
  self.Type         = INVALID_AEROSOL
  IF ( OBJ_VALID(self.Effective_Radius) ) THEN self.Effective_Radius -> REMOVE, /ALL
  IF ( OBJ_VALID(self.Concentration   ) ) THEN self.Concentration    -> REMOVE, /ALL
 
END
