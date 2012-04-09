;+
PRO RTS_Channel::Inspect, $
  Verbose=Verbose, $  ; Input keyword
  Debug=Debug         ; Input keyword
;-
  ; Set up
  @rts_pro_err_handler


  ; Output info
  IF ( KEYWORD_SET(Debug) ) THEN HELP, /ROUTINES
  HELP, self, /OBJECTS

  IF ( KEYWORD_SET(Verbose) AND self->Associated(Debug=Debug) ) THEN $
    HELP, *self.Upwelling_Radiance , $
          *self.Layer_Optical_Depth

END
