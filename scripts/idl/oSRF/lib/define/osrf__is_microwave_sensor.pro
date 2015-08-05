;+
; Syntactic sugar function

FUNCTION OSRF::Is_Microwave_Sensor, $
  Debug=Debug
;-
  
  COMPILE_OPT HIDDEN
  @osrf_func_err_handler

  RETURN, self.Is_Sensor(MICROWAVE_SENSOR, Debug=Debug)
END

