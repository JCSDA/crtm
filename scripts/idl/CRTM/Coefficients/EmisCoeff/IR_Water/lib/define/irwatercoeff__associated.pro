;+
; NAME:
;       IRwaterCoeff::Associated
;
; PURPOSE:
;       The IRwaterCoeff::Associated function determies if the list
;       components of a IRwaterCoeff object are associated with some data.
;
; CALLING SEQUENCE:
;       Result = Obj->[IRwaterCoeff::]Associated( Debug=Debug )
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

FUNCTION IRwaterCoeff::Associated, $
  Debug=Debug           ; Input keyword
 
  ; Set up
  @irwatercoeff_func_err_handler


  ; Test association status
  RETURN, self.Is_Allocated
 
END
