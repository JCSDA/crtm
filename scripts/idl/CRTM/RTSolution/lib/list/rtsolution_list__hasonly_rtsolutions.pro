FUNCTION RTSolution_List::HasOnly_RTSolutions, $
  Debug = debug ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  @rtsolution_func_err_handler

  ; Check that all list members are RTSolution objects
  result = TRUE
  FOREACH element, self DO result = result && ISA(element,'RTSolution')
  RETURN, result

END


