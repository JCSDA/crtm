;+
; NAME:
;       OSRF_File::Get
;
; PURPOSE:
;       The OSRF_File::Get function method returns object references to
;       objects in a OSRF_File container.
;
;       This method overrides the IDL_Container::Get function method.
;
; CALLING SEQUENCE:
;       Result = Obj->[OSRF_File::]Get( $
;         Debug   = Debug  , $  ; Input keyword
;         Channel = Channel, $  ; Input keyword
;         Count   = Count    )  ; Output keyword
;
; KEYWORDS:
;       Any keywords to the IDL_Container::Get method can be used.
;       In addition, the following keywords are available:
;
;       Debug:       Set this keyword for debugging.
;                    If NOT SET => Error handler is enabled. (DEFAULT)
;                       SET     => Error handler is disabled; Routine
;                                  traceback output is enabled.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Channel:     Set this keyword to a channel number or array of channel
;                    numbers for which the OSRF object reference(s) are to be
;                    retrieved.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar or Rank-1
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       
;       Count:       Set this keyword equal to a named variable that will
;                    contain the number of objects selected by the function.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(OUT), OPTIONAL
;
; FUNCTION RESULT:
;       Result:      The return value is a scalar or array of object references
;                    to OSRF objects in the OSRF_File container. If no objects
;                    are found in the container, the Get function returns -1.
;                    UNITS:      N/A
;                    TYPE:       Object
;                    DIMENSION:  Scalar or Rank-1
;
; INCLUDE FILES:
;       osrf_file parameters: Include file containing OSRF_File specific
;                             parameter value definitions.
;
;       osrf_func_err_handler: Error handler code for OSRF procedures.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 01-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION OSRF_File::Get, $
  Debug      = Debug  , $  ; Input keyword
  Channel    = Channel, $  ; Input keyword
  Count      = Count  , $  ; Output keyword
  _REF_EXTRA = Extra       ; Keywords passed onto IDL_Container::Get
 
  ; Set up
  ; ...File parameters
  @osrf_file_parameters
  ; ...Set up error handler
  @osrf_func_err_handler


  ; Get object reference from container
  IF ( N_ELEMENTS(Channel) GT 0 ) THEN BEGIN
    ; Initialise return object reference
    objref = -1L
    ; Get references for all OSRF objects in list
    osrf = self->IDL_Container::Get(/ALL, ISA='OSRF', COUNT=n_Channels)
    ; Loop over all OSRF objects to build the sensor channel list
    Sensor_Channel = LONARR(n_Channels)
    FOR i = 0L, n_Channels-1L DO BEGIN
      osrf[i]->Get_Property, Channel=ch
      Sensor_Channel[i] = ch
    ENDFOR
    ; Pick out the matching channels
    ; ...Initialise return count
    Count = 0L
    ; ...Loop over requested channels
    FOR i = 0L, N_ELEMENTS(Channel)-1 DO BEGIN
      loc = WHERE(Sensor_Channel EQ Channel[i], n )
      IF ( n GT 0 ) THEN BEGIN
        IF ( Count EQ 0 ) THEN objref = osrf[loc] ELSE objref = [objref, osrf[loc]]
        Count++
      ENDIF
    ENDFOR
  
  ENDIF ELSE BEGIN
  
    ; Just get the requested object reference
    objref = self->IDL_Container::Get(COUNT = Count, _EXTRA = Extra)
  
  ENDELSE
  

  ; Done
  CATCH, /CANCEL
  RETURN, objref

END ; FUNCTION OSRF_File::Get
