;+
; NAME:
;       Cloud::Destroy
;
; PURPOSE:
;       The Cloud::Destroy procedure method reinitialises a Cloud object.
;
; CALLING SEQUENCE:
;       Obj->[Cloud::]Destroy, $
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

PRO Cloud::Destroy, $
  Debug = Debug  ; Input keyword
 
  ; Set up
  @cloud_parameters
  @cloud_pro_err_handler
 

  ; Reinitialise
  self.Is_Allocated = FALSE
  self.n_Layers     = 0L
  self.Type         = INVALID_CLOUD
  IF ( OBJ_VALID(self.Effective_Radius  ) ) THEN self.Effective_Radius   -> REMOVE, /ALL
  IF ( OBJ_VALID(self.Effective_Variance) ) THEN self.Effective_Variance -> REMOVE, /ALL
  IF ( OBJ_VALID(self.Water_Content     ) ) THEN self.Water_Content      -> REMOVE, /ALL

END
