;+
; NAME:
;       Atmosphere::Associated
;
; PURPOSE:
;       The Atmosphere::Associated function determies if the list
;       components of a Atmosphere object are associated with some data.
;
; CALLING SEQUENCE:
;       Result = Obj->[Atmosphere::]Associated( Debug=Debug )
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
;
; FUNCTION RESULT:
;       Result:      The return value is an integer defining the error
;                    status. The error codes are defined in the error_codes
;                    include file.
;                    If == TRUE the object list components are associated.
;                       == FALSE the object list components are NOT associated.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;-

FUNCTION Atmosphere::Associated, $
  Debug=Debug           ; Input keyword
 
  ; Set up
  COMPILE_OPT HIDDEN
  @atmosphere_func_err_handler

  
  ; Test association status
  status = self.Is_Allocated
  ; ...Clouds
  IF ( self.n_Clouds GT 0 ) THEN $
    FOR i = 0L, self.n_Clouds-1L DO $
      status = status AND (self.Cloud)[i]->Cloud::Associated( Debug=Debug )
  ; ...Aerosols
  IF ( self.n_Aerosols GT 0 ) THEN $
    FOR i = 0L, self.n_Aerosols-1L DO $
      status = status AND (self.Aerosol)[i]->Aerosol::Associated( Debug=Debug )
 
  RETURN, status
 
END
