PRO OSRF::Integrate, $
  Debug=Debug

  ; Set up
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler

  ; Get the number of bands
  self->Get_Property, $
    n_Bands = n_Bands, $
    Debug   = Debug

  ; Sum up band integrals
  IntSum = ZERO
  FOR i = 0L, n_Bands-1L DO BEGIN
    ; Get band data
    Band = i+1
    self->Get_Property, $
      Band, $
      Frequency = f, $
      Response  = r, $
      Debug     = Debug
    ; Integrate
    Sum = Integral(f, r)
    IF ( Sum LE ZERO ) THEN $
      MESSAGE, "SRF integration for band #"+STRTRIM(Band,2)+" is < zero", $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    ; Accumulate
    IntSum = IntSum + Sum
  ENDFOR
  self->Set_Property, $
    Integral = IntSum, $
    Debug    = Debug
  self->Set_Flag, /Integrated

END ; PRO OSRF::Integrate
