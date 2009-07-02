;+
; NAME:
;       AtmProfile::Inspect
;
; PURPOSE:
;       The AtmProfile::Inspect procedure method outputs information
;       about the current AtmProfile object.
;
; CALLING SEQUENCE:
;       Obj->[AtmProfile::]Inspect, $
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
;       Inspect the contents of a AtmProfile object, x:
;
;         IDL> x->Inspect, /Verbose
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 01-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO AtmProfile::Inspect, $
  Verbose=Verbose, $  ; Input keyword
  Debug=Debug         ; Input keyword

  IF ( KEYWORD_SET(Debug) ) THEN HELP, /ROUTINES
  HELP, self, /OBJECTS

  IF ( KEYWORD_SET(Verbose) ) THEN BEGIN

    ; Absorber info
    IF ( PTR_VALID(self.Absorber_ID          ) AND $
         PTR_VALID(self.Absorber_Units_ID    ) AND $
         PTR_VALID(self.Absorber_Units_Name  ) AND $
         PTR_VALID(self.Absorber_Units_LBLRTM)     ) THEN BEGIN
      PRINT, FORMAT='(/,"Absorber information:")'
      HELP, *self.Absorber_ID          , $
            *self.Absorber_Units_ID    , $
            *self.Absorber_Units_Name  , $
            *self.Absorber_Units_LBLRTM
    ENDIF

    ; Level data
    IF ( PTR_VALID(self.Level_Pressure   ) AND $
         PTR_VALID(self.Level_Temperature) AND $
         PTR_VALID(self.Level_Absorber   ) AND $
         PTR_VALID(self.Level_Altitude   )     ) THEN BEGIN
      PRINT, FORMAT='(/,"Level profile data:")'
      HELP, *self.Level_Pressure   , $
            *self.Level_Temperature, $
            *self.Level_Absorber   , $
            *self.Level_Altitude   
    ENDIF

    ; Layer data
    IF ( PTR_VALID(self.Layer_Pressure   ) AND $
         PTR_VALID(self.Layer_Temperature) AND $
         PTR_VALID(self.Layer_Absorber   ) AND $
         PTR_VALID(self.Layer_Delta_Z    )     ) THEN BEGIN
      PRINT, FORMAT='(/,"Layer profile data:")'
      HELP, *self.Layer_Pressure   , $
            *self.Layer_Temperature, $
            *self.Layer_Absorber   , $
            *self.Layer_Delta_Z    
    ENDIF

  ENDIF 
  
END ; PRO AtmProfile::Inspect
