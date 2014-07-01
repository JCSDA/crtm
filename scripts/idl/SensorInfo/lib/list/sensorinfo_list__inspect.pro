;+
; NAME:
;       SensorInfo_List::Inspect
;
; PURPOSE:
;       The SensorInfo_List::Inspect procedure method outputs information
;       about the current SensorInfo_List object.
;
; CALLING SEQUENCE:
;       Obj->[SensorInfo_List::]Inspect, $
;         Verbose=Verbose, &  ; Input keyword
;         Debug  =Debug       ; Input keyword
;
; KEYWORDS:
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
;       Inspect the contents of a SensorInfo_List object, list:
;
;         IDL> list->Inspect, /Verbose
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 02-Oct-2008
;                       paul.vandelst@noaa.gov
;
;-

PRO SensorInfo_List::Inspect, $
  Verbose=Verbose, $  ; Input keyword
  Debug=Debug         ; Input keyword

  ; Setup
  ; ...Parameters
  @sensorinfo_parameters
  ; ...Set up error handler
  @sensorinfo_pro_err_handler
  
  ; Display contents
  IF ( KEYWORD_SET(Debug) ) THEN HELP, /ROUTINES
  HELP, self, /OBJECTS

  IF ( KEYWORD_SET(Verbose) ) THEN BEGIN

    si = self->Get(/ALL, ISA='SensorInfo', COUNT=n_Sensors)
    IF ( n_Sensors EQ 0 ) THEN RETURN
    
    ; Loop over contained SensorInfo objects
    FOR n = 0L, n_Sensors-1L DO BEGIN
      MESSAGE, 'Inspecting SensorInfo element #'+STRTRIM(n,2)+' ******', /INFORMATIONAL
      si[n]->SensorInfo::Inspect, $
        Verbose=Verbose, $  ; Input keyword
        Debug=Debug         ; Input keyword
      q = GET_KBRD(1)
      IF ( STRUPCASE(q) EQ 'Q' ) THEN BREAK
    ENDFOR
    
  ENDIF 

END
