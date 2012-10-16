FUNCTION Atmosphere_List::HasOnly_Atmospheres, $
  Debug = debug ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  @atmosphere_func_err_handler

  ; Check that all list members are Atmosphere objects
  result = TRUE
  FOREACH element, self DO result = result && ISA(element,'Atmosphere')
  RETURN, result

END

