;+
; NAME:
;       LBL_Control::Inspect
;
; PURPOSE:
;       The LBL_Control::Inspect procedure method outputs information
;       about the current LBL_Control object.
;
; CALLING SEQUENCE:
;       Obj->[LBL_Control::]Inspect, $
;         Verbose=Verbose, &  ; Input keyword
;         Debug  =Debug       ; Input keyword
;
; KEYWORDS:
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
;       Inspect the contents of a LBL_Control object, x:
;
;         IDL> x->Inspect, /Verbose
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 17-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO LBL_Control::Inspect, $
  Verbose=Verbose, $  ; Input keyword
  Debug=Debug         ; Input keyword

  IF ( KEYWORD_SET(Debug) ) THEN HELP, /ROUTINES
  HELP, self, /OBJECTS

  IF ( KEYWORD_SET(Verbose) ) THEN BEGIN
    ; Nothing to be verbose about yet
  ENDIF 
  
END ; PRO LBL_Control::Inspect
