;+
; NAME:
;       OSRF::Inspect
;
; PURPOSE:
;       The OSRF::Inspect procedure method outputs information
;       about the current OSRF object.
;
; CALLING SEQUENCE:
;       Obj->[OSRF::]Inspect, $
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
;       Inspect the contents of a OSRF object, x:
;
;         IDL> x->Inspect, /Verbose
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 20-Apr-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO OSRF::Inspect, $
  Verbose=Verbose, $  ; Input keyword
  Debug=Debug         ; Input keyword

  IF ( KEYWORD_SET(Debug) ) THEN HELP, /ROUTINES
  HELP, self, /OBJECTS

  IF ( KEYWORD_SET(Verbose) ) THEN BEGIN

    ; The band definition data
    IF ( PTR_VALID(self.f1       ) AND $
         PTR_VALID(self.f2       ) AND $
         PTR_VALID(self.n_Points )     ) THEN BEGIN
      PRINT, FORMAT='(/,"Band definition data:")'
      HELP, *self.f1       , $
            *self.f2       , $
            *self.n_Points
      PRINT, FORMAT='("F1:       ",99(1x,e13.6))', *self.f1
      PRINT, FORMAT='("F2:       ",99(1x,e13.6))', *self.f2
      PRINT, FORMAT='("N_POINTS: ",99(1x,i13))', *self.n_Points
    ENDIF

    ; The band response data
    IF ( PTR_VALID(self.Frequency) AND $
         PTR_VALID(self.Response )     ) THEN BEGIN
      PRINT, FORMAT='(/,"Band response data:")'
      HELP, *self.Frequency, $
            *self.Response
      FOR i = 0, self.n_Bands-1 DO BEGIN
        PRINT, i+1, FORMAT='(2x,"Band ",i1," frequency and response:")'
        HELP, *(*self.Frequency)[i], *(*self.Response)[i]
      ENDFOR
    ENDIF

  ENDIF 
  
END ; FUNCTION OSRF::Inspect
