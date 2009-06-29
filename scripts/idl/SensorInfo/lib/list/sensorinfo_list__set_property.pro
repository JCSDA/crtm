;+
; NAME:
;       SensorInfo_List::Set_Property
;
; PURPOSE:
;       The SensorInfo_List::Set_Property procedure method sets the value
;       of a property or group of properties for this object.
;
; CALLING SEQUENCE:
;       Obj->[SensorInfo_List::]Set_Property, $
;         Debug    = Debug   , $  ; Input keyword
;         Filename = Filename     ; Input keyword
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
;       Filename:         Specify this argument to set the filename
;                         associated with the SensorInfo_List object.
;                         UNITS:      N/A
;                         TYPE:       CHARACTER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(IN), OPTIONAL
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

PRO SensorInfo_List::Set_Property, $
  Debug    = Debug   , $  ; Input keyword
  Filename = Filename     ; Input keyword 

  ; Set up
  ; ...Parameters
  @sensorinfo_parameters
  ; ...Set up error handler
  @sensorinfo_pro_err_handler

  
  ; Set data
  IF ( Valid_String(Filename) ) THEN self.Filename = Filename
  
  
  ; Done
  CATCH, /CANCEL
 
END ; PRO SensorInfo_List::Set_Property
