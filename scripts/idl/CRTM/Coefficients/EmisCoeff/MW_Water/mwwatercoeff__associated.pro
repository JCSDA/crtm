;+
FUNCTION MWwaterCoeff::Associated, $
  Debug=Debug           ; Input keyword
;-
  ; Set up
  @emiscoeff_func_err_handler

  ; Test association status
  RETURN, self.Is_Allocated
 
END
