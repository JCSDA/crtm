;+
; Syntactic sugar function

FUNCTION OSRF::Is_Visible_Sensor, $
  Debug=Debug
;-
  
  COMPILE_OPT HIDDEN
  @osrf_func_err_handler

  RETURN, self.Is_Sensor(VISIBLE_SENSOR, Debug=Debug)
END

