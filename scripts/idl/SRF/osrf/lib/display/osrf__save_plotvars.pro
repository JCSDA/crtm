PRO OSRF::Save_PlotVars, $
  Band_Index, $
  Debug=Debug ; Input keyword

  ; Set up
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler

  ; ...ALL *input* pointers must be associated
  IF ( NOT self->Associated(Debug=Debug) ) THEN $
    MESSAGE, 'Some or all input OSRF pointer members are NOT associated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  (*self.psysvar)[Band_Index] = !P
  (*self.xsysvar)[Band_Index] = !X
  (*self.ysysvar)[Band_Index] = !Y

END ; PRO OSRF::Save_PlotVars
