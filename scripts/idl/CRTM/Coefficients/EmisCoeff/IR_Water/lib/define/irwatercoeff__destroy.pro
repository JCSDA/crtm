;+
; NAME:
;       IRwaterCoeff::Destroy
;
; PURPOSE:
;       The IRwaterCoeff::Destroy procedure method reinitialises a IRwaterCoeff object.
;
; CALLING SEQUENCE:
;       Obj->[IRwaterCoeff::]Destroy, $
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

PRO IRwaterCoeff::Destroy, $
  Debug = Debug  ; Input keyword
 
  ; Set up
  @irwatercoeff_parameters
  @irwatercoeff_pro_err_handler
 

  ; Reinitialise
  self.Is_Allocated = FALSE
  self.n_Angles      = 0L
  self.n_Frequencies = 0L
  self.n_Wind_Speeds = 0L
  IF ( OBJ_VALID(self.Angle     ) ) THEN self.Angle      -> REMOVE, /ALL
  IF ( OBJ_VALID(self.Frequency ) ) THEN self.Frequency  -> REMOVE, /ALL
  IF ( OBJ_VALID(self.Wind_Speed) ) THEN self.Wind_Speed -> REMOVE, /ALL
  IF ( OBJ_VALID(self.Emissivity) ) THEN self.Emissivity -> REMOVE, /ALL

END
