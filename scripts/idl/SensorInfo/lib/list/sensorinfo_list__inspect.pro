;+
; NAME:
;       SensorInfo_List::Inspect
;
; PURPOSE:
;       The SensorInfo_List::Inspect procedure method outputs information
;       about the current SensorInfo_List object and its nodes.
;
; CALLING SEQUENCE:
;       Obj->[SensorInfo_List::]Inspect, Verbose=Verbose, &  ; Input keyword
;                                        Debug  =Debug       ; Input keyword
;
; INPUT KEYWORD PARAMETERS:
;       Verbose:     Set this keyword for more verbose output.Information
;                    about each node in the list is output if this keyword
;                    is set.
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

PRO SensorInfo_List::Inspect, Verbose=Verbose, $  ; Input keyword
                              Debug=Debug         ; Input keyword

  IF ( KEYWORD_SET(Debug) ) THEN HELP, /ROUTINES
  ; Dump the object info
  HELP, self, /OBJECTS
  ; Dump the First component info
  IF ( PTR_VALID(self.First) ) THEN BEGIN
    HELP, *self.First, /STRUCTURE, OUTPUT=help_data
    PRINT, FORMAT='(9x,a)', help_data[0]
    FOR i=1L,N_ELEMENTS(help_data)-1L DO PRINT, FORMAT='(6x,a)', help_data[i]
  ENDIF

  ; Dump the help output for each node if required
  ; ----------------------------------------------
  IF ( KEYWORD_SET(Verbose) ) THEN BEGIN
    ; Initialise a node counter
    n = 0L
    ; Initialise pointer to first node
    Current = (*self.First).Next
    ; Traverse the list
    WHILE ( PTR_VALID(Current) ) DO BEGIN
      ; Output info
      ++n
      PRINT, FORMAT='(/6x,"Node: ",i5,/6x,"-----------")', n
      (*(*Current).SensorInfo)->Inspect
      ; Go to next node
      Current = (*Current).Next
    ENDWHILE
  ENDIF
  
END ; FUNCTION SensorInfo_List::Inspect
