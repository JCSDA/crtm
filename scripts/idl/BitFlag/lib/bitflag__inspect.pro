;+
; NAME:
;       BitFlag::Inspect
;
; PURPOSE:
;       The BitFlag::Inspect procedure method outputs information
;       about the current BitFlag object.
;
; CALLING SEQUENCE:
;       Obj->[BitFlag::]Inspect, $
;         Verbose=Verbose, &  ; Input keyword
;         Debug  =Debug       ; Input keyword
;
; INPUT KEYWORD PARAMETERS:
;       Verbose:     Set this keyword for more verbose output.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Debug:       Set this keyword for debugging.
;                    If NOT SET => Regular output. (DEFAULT)
;                       SET     => Information about all currently compiled
;                                  routines and their arguments are output.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
; EXAMPLE:
;       Inspect the contents of a BitFlag object, x:
;
;         IDL> x->Inspect, /Verbose
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 23-Sep-2010
;                       paul.vandelst@noaa.gov
;
;-

PRO BitFlag::Inspect, $
  Verbose=Verbose, $  ; Input keyword
  Debug=Debug         ; Input keyword

  ; Set up
  ; ...BitFlag parameters
  @bitflag_parameters
  ; ...Set up error handler
  @bitflag_pro_err_handler
 
  ; Basic stuff
  IF ( KEYWORD_SET(Debug) ) THEN HELP, /ROUTINES

  PRINT, FORMAT='("Value:", b32.32," (",a,")")', self.value, STRTRIM(self.value,2)

  ; Verbose stuff
  IF ( KEYWORD_SET(Verbose) ) THEN BEGIN
    HELP, self, /OBJECTS
    ; Hook for more elaborate output
  ENDIF 
  
END ; PRO BitFlag::Inspect
