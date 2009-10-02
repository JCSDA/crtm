;+
; NAME:
;       LBL_Specification::Inspect
;
; PURPOSE:
;       The LBL_Specification::Inspect procedure method outputs information
;       about the current LBL_Specification object.
;
; CALLING SEQUENCE:
;       Obj->[LBL_Specification::]Inspect, $
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
;       Inspect the contents of a LBL_Specification object, x:
;
;         IDL> x->Inspect, /Verbose
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 21-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO LBL_Specification::Inspect, $
  Verbose=Verbose, $  ; Input keyword
  Debug=Debug         ; Input keyword

  IF ( KEYWORD_SET(Debug) ) THEN HELP, /ROUTINES
  HELP, self, /OBJECTS
  IF ( KEYWORD_SET(Verbose) ) THEN BEGIN
    ; Nothing to be verbose about yet
  ENDIF 
  
END ; PRO LBL_Specification::Inspect
