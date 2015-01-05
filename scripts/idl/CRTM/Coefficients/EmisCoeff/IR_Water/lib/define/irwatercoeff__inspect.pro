;+
; NAME:
;       IRwaterCoeff::Inspect
;
; PURPOSE:
;       The IRwaterCoeff::Inspect procedure method output object information to stdout.
;
; CALLING SEQUENCE:
;       Obj->[IRwaterCoeff::]Inspect, $
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

PRO IRwaterCoeff::Inspect, $
  Verbose=Verbose, $  ; Input keyword
  Debug=Debug         ; Input keyword

  ; Set up
  @irwatercoeff_pro_err_handler
 
 
  ; Output info
  IF ( KEYWORD_SET(Debug) ) THEN HELP, /ROUTINES
  HELP, self, /OBJECTS

  IF ( KEYWORD_SET(Verbose) AND self->Associated(Debug=Debug) ) THEN $
    HELP, (self.Angle     )[0]  , $
          (self.Frequency )[0], $
          (self.Wind_Speed)[0], $
          (self.Emissivity)[0]     

END
