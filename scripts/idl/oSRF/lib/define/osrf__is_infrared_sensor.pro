;+
; Syntactic sugar function

FUNCTION OSRF::Is_Infrared_Sensor, $
  Debug=Debug
;-

  COMPILE_OPT HIDDEN
  @osrf_func_err_handler

  RETURN, self.Is_Sensor(INFRARED_SENSOR, Debug=Debug)
END

