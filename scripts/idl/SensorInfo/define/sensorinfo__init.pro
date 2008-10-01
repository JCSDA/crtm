;+
; Init function for SensorInfo object

FUNCTION SensorInfo::Init, Debug=Debug  ; Input keyword
;- 

  ; Set up
  ; ------
  ; Include SensorInfo parameters
  @sensorinfo_parameters
  
  ; error handler
  @error_codes
  IF ( KEYWORD_SET(Debug) ) THEN BEGIN
    MESSAGE, '--> Entered.', /INFORMATIONAL
  ENDIF ELSE BEGIN
    CATCH, Error_Status
    IF ( Error_Status NE 0 ) THEN BEGIN
      CATCH, /CANCEL
      MESSAGE, !ERROR_STATE.MSG, /CONTINUE
      RETURN, FAILURE
    ENDIF
  ENDELSE

  ; Set default values
  ; ------------------
  self.Sensor_Name      = ' '
  self.Satellite_Name   = ' '
  self.Sensor_Id        = ' '
  self.WMO_Satellite_Id = INVALID_WMO_SATELLITE_ID
  self.WMO_Sensor_Id    = INVALID_WMO_SENSOR_ID
  self.Microwave_Flag   = INVALID
  self.Sensor_Type      = INVALID_SENSOR

  CATCH, /CANCEL
  RETURN, SUCCESS
 
END ; FUNCTION SensorInfo::Init
