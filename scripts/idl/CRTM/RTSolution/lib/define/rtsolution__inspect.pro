;+
; NAME:
;       RTSolution::Inspect
;
; PURPOSE:
;       The RTSolution::Inspect procedure method output object information to stdout.
;
; CALLING SEQUENCE:
;       Obj->[RTSolution::]Inspect, $
;         Verbose = Verbose, $
;         Debug   = Debug     
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

PRO RTSolution::Inspect, $
  Verbose = verbose, $  ; Input keyword
  Debug   = debug       ; Input keyword

  ; Set up
  @rtsolution_pro_err_handler


  ; Output info
  IF ( KEYWORD_SET(debug) ) THEN HELP, /ROUTINES
  HELP, self, /OBJECTS

  IF ( KEYWORD_SET(verbose) AND self->Associated(Debug = debug) ) THEN $
    HELP, (self.Upwelling_Radiance )[0], $
          (self.Layer_Optical_Depth)[0]

END
