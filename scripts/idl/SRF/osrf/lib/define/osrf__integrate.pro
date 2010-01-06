PRO OSRF::Integrate, $
  Debug=Debug, method=method

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
;    Sum = Integral(f, r)

if ( n_elements(method) gt 0 ) then begin
  case method of
    1: Sum = new_Integral(f, r)
    2: Sum = new2_Integral(f, r)
    3: Sum = new3_Integral(f, r)
    4: Sum = my_int_tabulated(f, r)
    else: Sum = Integral(f, r)
  endcase
endif else Sum = Integral(f, r)

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


  ; Done
  CATCH, /CANCEL

END ; PRO OSRF::Integrate
