;+
; NAME:
;       Atmosphere::Inspect
;
; PURPOSE:
;       The Atmosphere::Inspect procedure method output object information to stdout.
;
; CALLING SEQUENCE:
;       Obj->[Atmosphere::]Inspect, $
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

PRO Atmosphere::Inspect, $
  Verbose=Verbose, $  ; Input keyword
  Debug=Debug         ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  @atmosphere_pro_err_handler
 
 
  ; Output info
  IF ( KEYWORD_SET(Debug) ) THEN HELP, /ROUTINES
  HELP, self, /OBJECTS

  IF ( KEYWORD_SET(Verbose) AND self->Associated(Debug=Debug) ) THEN BEGIN
    HELP, (self.Absorber_ID   )[0], $
          (self.Absorber_Units)[0], $
          (self.Level_Pressure)[0], $
          (self.Pressure      )[0], $
          (self.Temperature   )[0], $
          (self.Absorber      )[0], $
          (self.CFraction     )[0]
    HELP, self.Cloud  , $
          self.Aerosol
    FOR n = 0, self.n_Clouds-1   DO HELP, (self.Cloud)[n]
    FOR n = 0, self.n_Aerosols-1 DO HELP, (self.Aerosol)[n]
  ENDIF

END
