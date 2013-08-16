FUNCTION OSRF::Flag_Is_Set, $
  Flag, $
  Debug=Debug
  
  ; Set up
  COMPILE_OPT HIDDEN
  @osrf_func_err_handler

  ; Get flag status
  Value = self.Flags AND Flag.position
  i = Value GT 0 ? ALOG(Value)/ALOG(2) : 0
  RETURN, ISHFT(Value, -i)
  
END

