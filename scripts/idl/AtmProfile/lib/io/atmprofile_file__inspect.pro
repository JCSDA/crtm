;+
; NAME:
;       AtmProfile_File::Inspect
;
; PURPOSE:
;       The AtmProfile_File::Inspect procedure method outputs information
;       about the current AtmProfile_File object.
;
; CALLING SEQUENCE:
;       Obj->[AtmProfile_File::]Inspect, $
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
;       Inspect the contents of a AtmProfile_File object, x:
;
;         IDL> x->Inspect, /Verbose
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 06-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO AtmProfile_File::Inspect, $
  Verbose=Verbose, $  ; Input keyword
  Debug=Debug         ; Input keyword

  IF ( KEYWORD_SET(Debug) ) THEN HELP, /ROUTINES
  HELP, self, /OBJECTS

  IF ( KEYWORD_SET(Verbose) ) THEN BEGIN

    atmprofile = self->Get(/ALL, ISA='AtmProfile', COUNT=n_Profiles)
    IF ( n_Profiles EQ 0 ) THEN RETURN
    
    ; Loop over contained AtmProfile objects
    FOR m = 0L, n_Profiles-1L DO BEGIN
      MESSAGE, 'Inspecting AtmProfile element #'+STRTRIM(m,2)+' ******', /INFORMATIONAL
      atmprofile[m]->AtmProfile::Inspect, $
        Verbose=Verbose, $  ; Input keyword
        Debug=Debug         ; Input keyword
      q = GET_KBRD(1)
      IF ( STRUPCASE(q) EQ 'Q' ) THEN BREAK
    ENDFOR
    
  ENDIF 
  
END ; PRO AtmProfile_File::Inspect
