PRO AtmProfile::OPlot, $
  atmprofile, $
  Debug     = Debug    , $ ; Input keyword
  Absorber_Id = Absorber_Id, $
  _EXTRA    = Extra        ; Input keyword

  ; Set up
  ; ...AtmProfile parameters
  @atmprofile_parameters
  ; ...Set up error handler
  @atmprofile_pro_err_handler
  ; ...ALL *input* pointers must be associated
  IF ( NOT self->Associated(Debug=Debug) OR $
       NOT atmprofile->Associated(Debug=Debug)) THEN $
    MESSAGE, 'Some or all input AtmProfile pointer members are NOT associated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Get the data
  atmprofile->Get_Property, $
    n_Absorbers         = n_Absorbers, $
    Absorber_Id         = aid, $
    Absorber_Units_Name = aun, $
    Level_Pressure      = p, $
    Level_Temperature   = t, $
    Level_Absorber      = a


  ; Check keywords
  IF ( N_ELEMENTS(Absorber_Id) EQ 0 ) THEN Absorber_Id = aid
  
  
  ; Save current plotting sysvars
  psave = !P
  xsave = !X
  ysave = !Y


  ; Plot the temperature
  self->Restore_PlotVars, 0
  OPLOT, t, p, $
    _EXTRA = Extra
  
  
  ; Plot the absorbers
  FOR j = 0, n_Absorbers-1 DO BEGIN
    loc = WHERE( Absorber_Id EQ aid[j], count )
    IF ( count GT 0 ) THEN BEGIN
      self->Restore_PlotVars, j+1
      OPLOT, a[*,j], p,$
        _EXTRA = Extra
    ENDIF
  ENDFOR
  
  
  ; Restore plotting sysvars
  !P = psave
  !X = xsave
  !Y = ysave


  ; Done
  CATCH, /CANCEL
  
END ; PRO AtmProfile::OPlot
