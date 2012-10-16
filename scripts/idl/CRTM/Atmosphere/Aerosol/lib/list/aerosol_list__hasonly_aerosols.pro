FUNCTION Aerosol_List::HasOnly_Aerosols, $
  Debug = debug ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  @aerosol_func_err_handler

  ; Check that all list members are Aerosol objects
  result = TRUE
  FOREACH element, self DO result = result && ISA(element,'Aerosol')
  RETURN, result

END

