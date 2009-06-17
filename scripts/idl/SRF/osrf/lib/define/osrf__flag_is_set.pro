FUNCTION OSRF::Flag_Is_Set, $
  Flag, $
  Debug=Debug
  
  ; Set up
  @osrf_func_err_handler

  ; Get flag status
  Value = self.Flags AND Flag
  i = Value GT 0 ? ALOG(Value)/ALOG(2) : 0
  RETURN, ISHFT(Value, -i)
  
END ; FUNCTION OSRF::Flag_Is_Set

