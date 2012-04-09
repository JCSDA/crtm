;+
PRO MWwaterCoeff::Inspect, $
  Verbose=Verbose, $  ; Input keyword
  Debug=Debug         ; Input keyword
;-
  ; Set up
  @emiscoeff_pro_err_handler


  ; Output info
  IF ( KEYWORD_SET(Debug) ) THEN HELP, /ROUTINES
  HELP, self, /OBJECTS

  IF ( KEYWORD_SET(Verbose) AND self->Associated(Debug=Debug) ) THEN $
    HELP, *self.Angle      , $
          *self.Frequency  , $
          *self.Temperature, $
          *self.Wind_Speed , $
          *self.ev         , $
          *self.eh           
 
END
