;+
; NAME:
;       LBL_Input::Inspect
;
; PURPOSE:
;       The LBL_Input::Inspect procedure method outputs information
;       about the current LBL_Input object.
;
; CALLING SEQUENCE:
;       Obj->[LBL_Input::]Inspect, $
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
;       Inspect the contents of a LBL_Input object, x:
;
;         IDL> x->Inspect, /Verbose
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 17-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO LBL_Input::Inspect, $
  Verbose=Verbose, $  ; Input keyword
  Debug=Debug         ; Input keyword

  IF ( KEYWORD_SET(Debug) ) THEN HELP, /ROUTINES
  HELP, self, /OBJECTS

  IF ( KEYWORD_SET(Verbose) ) THEN BEGIN

    obj = self->Get(/ALL, COUNT=n_Objs)
    IF ( n_Objs EQ 0 ) THEN RETURN
    
    ; Loop over contained objects
    FOR n = 0L, n_Objs-1L DO BEGIN
      IF ( OBJ_HASMETHOD(obj[n], 'INSPECT') ) THEN $
        obj[n]->Inspect, Verbose=Verbose, Debug=Debug
      q = GET_KBRD(1)
      IF ( STRUPCASE(q) EQ 'Q' ) THEN BREAK
    ENDFOR
    
  ENDIF 
  
END ; PRO LBL_Input::Inspect
