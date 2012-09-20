;+
; NAME:
;       Aerosol::Inspect
;
; PURPOSE:
;       The Aerosol::Inspect procedure method output object information to stdout.
;
; CALLING SEQUENCE:
;       Obj->[Aerosol::]Inspect, $
;         Verbose=Verbose, $
;         Debug = Debug     
;
; INPUT KEYWORDS:
;       Verbose:   Set this to keyword to output more infoprmation about
;                  the list element contents.
;                  UNITS:      N/A
;                  TYPE:       INTEGER
;                  DIMENSION:  Scalar
;                  ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Debug:     Set this keyword for debugging.
;                  If NOT SET => Error handler is enabled. (DEFAULT)
;                     SET     => Error handler is disabled; Routine
;                                traceback output is enabled.
;                  UNITS:      N/A
;                  TYPE:       INTEGER
;                  DIMENSION:  Scalar
;                  ATTRIBUTES: INTENT(IN), OPTIONAL
;-

PRO Aerosol::Inspect, $
  Verbose=Verbose, $  ; Input keyword
  Debug=Debug         ; Input keyword

  ; Set up
  @cloud_pro_err_handler
 
 
  ; Output info
  IF ( KEYWORD_SET(Debug) ) THEN HELP, /ROUTINES
  HELP, self, /OBJECTS

  IF ( KEYWORD_SET(Verbose) AND self->Associated(Debug=Debug) ) THEN $
    HELP, (self.Effective_Radius)[0], $
          (self.Concentration)[0]     

END
