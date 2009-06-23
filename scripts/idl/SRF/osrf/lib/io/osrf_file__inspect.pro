;+
; NAME:
;       OSRF_File::Inspect
;
; PURPOSE:
;       The OSRF_File::Inspect procedure method outputs information
;       about the current OSRF_File object.
;
; CALLING SEQUENCE:
;       Obj->[OSRF_File::]Inspect, $
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
;       Inspect the contents of a OSRF_File object, x:
;
;         IDL> x->Inspect, /Verbose
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 22-Jun-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO OSRF_File::Inspect, $
  Verbose=Verbose, $  ; Input keyword
  Debug=Debug         ; Input keyword

  IF ( KEYWORD_SET(Debug) ) THEN HELP, /ROUTINES
  HELP, self, /OBJECTS

  IF ( KEYWORD_SET(Verbose) ) THEN BEGIN

    osrf = self->Get(/ALL, ISA='OSRF', COUNT=n_OSRFs)
    IF ( n_OSRFs EQ 0 ) THEN RETURN
    
    ; Loop over contained OSRF objects
    FOR n = 0L, n_OSRFs-1L DO BEGIN
      MESSAGE, 'Inspecting OSRF element #'+STRTRIM(n,2)+' ******', /INFORMATIONAL
      osrf[n]->OSRF::Inspect, $
        Verbose=Verbose, $  ; Input keyword
        Debug=Debug         ; Input keyword
      q = GET_KBRD(1)
      IF ( STRUPCASE(q) EQ 'Q' ) THEN BREAK
    ENDFOR
    
  ENDIF 
  
END ; PRO OSRF_File::Inspect
