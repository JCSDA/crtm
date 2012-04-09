;+
FUNCTION RTS_Channel::Associated, $
  Debug=Debug           ; Input keyword
;-
  ; Set up
  @rts_func_err_handler


  ; Test association status
  RETURN, self.Is_Allocated

END
