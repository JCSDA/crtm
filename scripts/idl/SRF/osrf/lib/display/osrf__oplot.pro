PRO OSRF::OPlot, $
  osrf, $
  Debug=Debug ; Input keyword

  ; Set up
  @color_db
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler

  ; ...ALL *input* pointers must be associated
  IF ( NOT self->Associated(Debug=Debug) OR $
       NOT osrf->Associated(Debug=Debug)) THEN $
    MESSAGE, 'Some or all input OSRF pointer members are NOT associated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  psave = !P
  xsave = !X
  ysave = !Y
  FOR i = 0L, self.n_Bands-1L DO BEGIN
    self->Restore_PlotVars, i
    OPLOT, *(*osrf.Frequency)[i], *(*osrf.Response)[i], $
           COLOR=RED, PSYM=-4
  ENDFOR
  !P = psave
  !X = xsave
  !Y = ysave

END ; PRO OSRF::OPlot
