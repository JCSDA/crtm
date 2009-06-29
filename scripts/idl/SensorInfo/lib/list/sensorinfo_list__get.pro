;+
; NAME:
;       SensorInfo_List::Get
;
; PURPOSE:
;       The SensorInfo_List::Get function method returns object references to
;       objects in a SensorInfo_List container.
;
;       This method overrides the IDL_Container::Get function method.
;
; CALLING SEQUENCE:
;       Result = Obj->[SensorInfo)List::]Get( $
;         Debug     = Debug    , $  ; Input keyword
;         Sensor_Id = Sensor_Id, $  ; Input keyword
;         Count     = Count      )  ; Output keyword
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
;       Sensor_Id:   Set this keyword to a Sensor Id string to retrieve
;                    the object reference in the SensorInfo list of the 
;                    matching SensorInfo object. Only one object reference
;                    can be retrieved at a time if thise keyword is used.
;                    UNITS:      N/A
;                    TYPE:       CHARACTER
;                    DIMENSION:  Scalar
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
;                    to SensorInfo objects in the SensorInfo_List container. If
;                    no objects are found in the container, the Get function
;                    returns -1.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;
; INCLUDE FILES:
;       sensorinfo_parameters: Include file containing SensorInfo specific
;                              parameter value definitions.
;
;       sensorinfo_func_err_handler: Error handler code for SensorInfo functions.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 26-Jun-2009
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION SensorInfo_List::Get, $
  Debug     = Debug    , $  ; Input keyword
  Sensor_Id = Sensor_Id, $  ; Input keyword
  Count     = Count    , $  ; Output keyword
  _REF_EXTRA = Extra        ; Keywords passed onto IDL_Container::Get
 
  ; Set up
  ; ...SensorInfo parameters
  @sensorinfo_parameters
  ; ...Set up error handler
  @sensorinfo_func_err_handler


  ; Get object reference from container
  IF ( Valid_String(Sensor_Id) ) THEN BEGIN
    ; Initialise return count
    Count = 0L
    ; Get requested SensorInfo object
    ; ...Get references for all SensorInfo object in list
    si = self->IDL_Container::Get(/ALL, ISA='SensorInfo', COUNT=n_Sensors)
    ; ...Loop over all SensorInfo objects searching for Sensor_Id
    FOR i = 0L, n_Sensors-1L DO BEGIN
      si[i]->Get_Property, Sensor_Id=sid
      IF ( STRCMP(STRTRIM(Sensor_Id,2), STRTRIM(sid,2), /FOLD_CASE) ) THEN BEGIN
        objref = si[i]
        Count = 1L
        GOTO, Done
      ENDIF
    ENDFOR
    MESSAGE, 'SensorInfo_List contains no SensorInfo node for '+STRTRIM(Sensor_Id,2), $
             /CONTINUE
    objref = -1L
  
  ENDIF ELSE BEGIN
  
    ; Just get the requested object reference
    objref = self->IDL_Container::Get(COUNT = Count, _EXTRA = Extra)
  
  ENDELSE
  

  ; Done
  Done:
  CATCH, /CANCEL
  RETURN, objref

END ; FUNCTION SensorInfo_File::Get
