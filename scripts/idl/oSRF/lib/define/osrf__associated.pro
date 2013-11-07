;+
; NAME:
;       OSRF::Associated
;
; PURPOSE:
;       The OSRF::Associated function determies if an OSRF object has
;       data associated with it and is valid for use.
;
; CALLING SEQUENCE:
;       Result = Obj->[OSRF::]Associated( $
;                  Debug=Debug        )  ; Input keyword
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
;                    If == TRUE the object pointer components are associated.
;                       == FALSE the object pointer components are NOT associated.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;
; INCLUDE FILES:
;       error_codes: Include file containing error code definitions.
;
; EXAMPLE:
;       After creating a OSRF object,
;
;         IDL> x = OBJ_NEW('OSRF')
;
;       the association status can be tested,
;
;         IDL> IF ( x->Associated() ) THEN PRINT,'Associated' ELSE PRINT,'Not Associated'
;         Not Associated
;
;       Once the object pointer components have been allocated, the associated
;       status should be "true",
;
;         IDL> x->Allocate,10
;         IDL> IF ( x->Associated() ) THEN PRINT,'Associated' ELSE PRINT,'Not Associated'
;         Associated
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 20-Apr-2009
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION OSRF::Associated, $
  Debug=Debug           ; Input keyword
 
  ; Set up
  COMPILE_OPT HIDDEN
  @osrf_func_err_handler

  RETURN, self.Is_Allocated
 
END
