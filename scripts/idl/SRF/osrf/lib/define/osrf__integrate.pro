PRO OSRF::Integrate, $
  Debug=Debug

  ; Set up
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler

  ; Sum up band integrals
  IntSum = ZERO
  FOR i = 0L, self.n_Bands-1L DO BEGIN
    ; Get band data
    Band = i+1
    self->Get_Property, $
      Band, $
      Frequency=f, $
      Response =r, $
      Debug=Debug
    ; Integrate
    Sum = INT_TABULATED(f, r, /DOUBLE)
    IF ( Sum LE ZERO ) THEN $
      MESSAGE, "SRF integration for band #"+STRTRIM(Band,2)+" is < zero", $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    ; Accumulate
    IntSum = IntSum + Sum
  ENDFOR
  self->Set_Property, $
    Integral=IntSum, $
    Debug=Debug
  self->Set_Flag, /Integrated


  ; Done
  CATCH, /CANCEL

END ; PRO OSRF::Integrate
