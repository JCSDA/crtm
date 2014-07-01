;+
; NAME:
;       SensorInfo_List::Get_Property
;
; PURPOSE:
;       The SensorInfo_List::Get_Property procedure method gets the value
;       of a property or group of properties for this object.
;
; CALLING SEQUENCE:
;       Obj->[SensorInfo_List::]Get_Property, $
;         Debug    = Debug   , $  ; Input keyword
;         Filename = Filename, $  ; Output keyword
;         n_Sensor = n_Sensors    ; Output keyword
;
; KEYWORDS:
;       Debug:            Set this keyword for debugging.
;                         If NOT SET => Error handler is enabled. (DEFAULT)
;                            SET     => Error handler is disabled; Routine
;                                       traceback output is enabled.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Filename:         Set this keyword to a named variable to return the
;                         filename associated with the SensorInfo_List object.
;                         UNITS:      N/A
;                         TYPE:       CHARACTER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       n_Sensors:        Set this keyword to a named variable to return the
;                         number of SensorInfo nodes contained in the
;                         SensorInfo_List object.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(OUT), OPTIONAL
;
; INCLUDE FILES:
;       sensorinfo_parameters: Include file containing SensorInfo specific
;                              parameter value definitions.
;
;       sensorinfo_pro_err_handler: Error handler code for SensorInfo procedures.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 29-Jun-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO SensorInfo_List::Get_Property, $
  Debug     = Debug    , $  ; Input keyword
  Filename  = Filename , $  ; Input keyword 
  n_Sensors = n_Sensors     ; Input keyword 

  ; Set up
  ; ...Parameters
  @sensorinfo_parameters
  ; ...Set up error handler
  @sensorinfo_pro_err_handler

  
  ; Set data
  IF ( ARG_PRESENT(Filename ) GT 0 ) THEN Filename  = self.Filename
  IF ( ARG_PRESENT(n_Sensors) GT 0 ) THEN n_Sensors = self.n_Sensors
  
END
