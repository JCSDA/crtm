;+
; NAME:
;       Aerosol::Associated
;
; PURPOSE:
;       The Aerosol::Associated function determies if the list
;       components of a Aerosol object are associated with some data.
;
; CALLING SEQUENCE:
;       Result = Obj->[Aerosol::]Associated( Debug=Debug )  ; Input keyword
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
;
; INCLUDE FILES:
;       aerosol_func_err_handler: Include file for error handling.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 09-Dec-2010
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION Aerosol::Associated, $
  Debug=Debug           ; Input keyword
 
  ; Set up
  @aerosol_func_err_handler


  ; Test association status
  RETURN, self.Is_Allocated
 
END ; FUNCTION Aerosol::Associated
