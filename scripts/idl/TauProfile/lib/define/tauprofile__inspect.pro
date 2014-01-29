;+
; NAME:
;       TauProfile::Inspect
;
; PURPOSE:
;       The TauProfile::Inspect procedure method output object information to stdout.
;
; CALLING SEQUENCE:
;       Obj->[TauProfile::]Inspect, $
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

PRO TauProfile::Inspect, $
  Verbose=Verbose, $  ; Input keyword
  Debug=Debug         ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  @tauprofile_pro_err_handler
 
 
  ; Output info
  IF ( KEYWORD_SET(Debug) ) THEN HELP, /ROUTINES
  HELP, self, /OBJECTS

  IF ( KEYWORD_SET(Verbose) AND self->Associated(Debug=Debug) ) THEN BEGIN
    HELP, (self.Level_Pressure)[0], $
          (self.Channel       )[0], $
          (self.Angle         )[0], $
          (self.Profile       )[0], $
          (self.Molecule_Set  )[0], $
          (self.Tau           )[0]
  ENDIF

END
