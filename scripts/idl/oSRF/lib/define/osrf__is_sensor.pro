;+
FUNCTION OSRF::Is_Sensor, $
  Sensor_Type, $
  Debug=Debug
;-  
  ; Set up
  COMPILE_OPT HIDDEN
  @osrf_func_err_handler

  ; Determine if sensor type
  status = self.Sensor_Type EQ Sensor_Type
  
  ; Output debug info
  IF ( KEYWORD_SET(Debug) ) THEN BEGIN
    MESSAGE, 'oSRF object sensor type is ' + SENSOR_TYPE_NAME[self.Sensor_Type] + $
             ', not ' + SENSOR_TYPE_NAME[Sensor_Type], /INFORMATIONAL
  ENDIF
  
  ; Return status value
  RETURN, status
  
END

