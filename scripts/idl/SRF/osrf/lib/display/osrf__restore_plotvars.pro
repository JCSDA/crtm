PRO OSRF::Restore_PlotVars, $
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

  !P = (*self.psysvar)[Band_Index]
  !X = (*self.xsysvar)[Band_Index]
  !Y = (*self.ysysvar)[Band_Index]

END ; PRO OSRF::Restore_PlotVars
