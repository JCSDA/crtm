FUNCTION Cloud_List::HasOnly_Clouds, $
  Debug = debug ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  @cloud_func_err_handler

  ; Check that all list members are Cloud objects
  result = TRUE
  FOREACH element, self DO result = result && ISA(element,'Cloud')
  RETURN, result

END

