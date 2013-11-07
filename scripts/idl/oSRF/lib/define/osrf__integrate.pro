;+
; NAME:
;       OSRF::Integrate
;
; PURPOSE:
;       The OSRF::Integrate procedure method integrates the response data
;       and saves the result in the Integral property.
;
; CALLING SEQUENCE:
;       Obj->[OSRF::]Integrate, Debug = Debug
;
; INPUT KEYWORD PARAMETERS:
;       Debug:                 Set this keyword for debugging.
;                              If NOT SET => Error handler is enabled. (DEFAULT)
;                                 SET     => Error handler is disabled; Routine
;                                            traceback output is enabled.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
; INCLUDE FILES:
;       osrf_parameters: Include file containing OSRF specific
;                        parameter value definitions.
;
;       osrf_pro_err_handler: Error handler code for OSRF procedures.
;
; EXAMPLE:
;       Given a valid OSRF object, x, it is integrated like so,
;
;         IDL> x->Integrate
;
;       The integrated value can be obtained via the Get_Property method:
;
;         IDL> x->Get_Property, Integral = intvalue
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 17-Jun-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO OSRF::Integrate, $
  Debug=Debug

  ; Set up
  COMPILE_OPT HIDDEN
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler


  ; Check if object has been allocated
  IF ( ~ self.Associated(Debug=Debug) ) THEN $
    MESSAGE, 'OSRF object has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Get the number of bands
  self.Get_Property, n_Bands=n_bands, Debug=Debug


  ; Sum up band integrals
  intsum = ZERO
  FOR band = 1L, n_Bands DO BEGIN
    ; Get band data
    self->Get_Property, $
      band, $
      Frequency = f, $
      Response  = r, $
      Debug=Debug
    ; Integrate
    sum = Integral(f, r)
    IF ( sum LE ZERO ) THEN $
      MESSAGE, "SRF integration for band #"+STRTRIM(band,2)+" is < zero", $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    ; Accumulate
    intsum = intsum + sum
  ENDFOR
  
  
  ; Assign to object
  self.Integral = intsum
  self.Set_Flag, /Is_Integrated, Debug=Debug

END
