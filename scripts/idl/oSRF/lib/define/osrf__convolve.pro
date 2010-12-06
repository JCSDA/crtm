FUNCTION OSRF::Convolve, $
  ptr, $  ; Input
  Debug=Debug

  ; Set up
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_func_err_handler

  ; Get the number of bands
  self->Get_Property, $
    n_Bands = n_Bands, $
    Debug   = Debug

  ; Sum up band integrals
  y = ZERO
  FOR i = 0L, n_Bands-1L DO BEGIN
    IF ( NOT self->Flag_Is_Set(INTEGRATED_FLAG) ) THEN self->Integrate, Debug=Debug
    ; Get band data
    Band = i+1
    self->Get_Property, $
      Band, $
      Frequency = f, $
      Response  = r, $
      Debug=Debug
    ; Integrate
    Sum = Integral(f, (*ptr[i])*r)
    IF ( Sum LE ZERO ) THEN $
      MESSAGE, "SRF integration for band #"+STRTRIM(Band,2)+" is < zero", $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    ; Accumulate
    y = y + Sum
  ENDFOR
  self->Get_Property, $
    Integral = IntSum, $
    Debug    = Debug
  y = y / IntSum


  ; Done
  RETURN, y
 
END ; FUNCTION OSRF::Convolve
