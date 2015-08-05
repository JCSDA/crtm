;+
; Syntactic sugar function

FUNCTION OSRF::Is_Ultraviolet_Sensor, $
  Debug=Debug
;-
  
  COMPILE_OPT HIDDEN
  @osrf_func_err_handler

  RETURN, self.Is_Sensor(ULTRAVIOLET_SENSOR, Debug=Debug)
END

